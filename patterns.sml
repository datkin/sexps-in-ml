(* Scheme macros may be defined using a simple yet powerful tool
 * called "syntax-rules" which gives a language for matching and
 * transforming patterns of s-expressions. Patterns is a similar tool
 * for parsing syntax-rules style definitions from S-expressions to
 * generate rewrite rules, and then applying those rules. *)
structure Pattern = struct

  (* Pattern elements that should be matched literally. *)
  datatype literal = Id of Id.id
                   | Num of Ast.num
                   | String of string

  (**
   * A pattern is one of:
   * - An identifier, which may either be a literal or a binding
   *   variable.
   * - Some other literal (number, string, ...).
   * - A list of patterns. At most one pattern inside a list may be a
   *   "sequence", to be matched repeatedly (zero or more times,
   *   greedily). A sequence is indicated by a pattern followed by an
   *   ellipsis, eg:
   *     (1 ... 2)
   *   will match every list of zero or more ones, ending in a 2. If a
   *   list doesn't contain a sequence, it may end with a dot followed
   *   by a variable, to which the remainder of the input list will be
   *   bound. Eg:
   *     (1 2 3 . rest)
   *   will match any list beginning with elements 1 2 3, with the
   *   sublist after the 3rd element bound to 'rest'.
   *)
  datatype pattern = PList of pattern list * ptail
                   | PVar of Id.id
                   | PLiteral of literal
       and ptail = PSeq of pattern * pattern list
                 | PRest of Id.id
                 | PEnd

  val ellipsis = Id.id "..."
  val dot = Id.id "."

  (* A predicate for ellipses. *)
  fun isEllipsis (Ast.Id id) = id = ellipsis
    | isEllipsis _ = false

  (**
   * Convert a pattern to it's s-expression representation. Does not
   * preserve distinction between identifiers and literals.
   *)
  fun toSexp (PList (patterns, tail)) =
      let
        fun tailToSexps (PSeq (seq, tailPatterns)) =
            (toSexp seq) :: Ast.Id ellipsis :: (map toSexp tailPatterns)
          | tailToSexps (PRest id) = [Ast.Id dot, Ast.Id id]
          | tailToSexps PEnd = []
      in
        Ast.Sexp ((map toSexp patterns) @ (tailToSexps tail))
      end
    | toSexp (PVar id) = Ast.Id id
    | toSexp (PLiteral (Id id)) = Ast.Id id
    | toSexp (PLiteral (Num num)) = Ast.Num num
    | toSexp (PLiteral (String str)) = Ast.String str

  (**
   * Creates a pattern value given it's sexp representation. All
   * indentifiers in the pattern definition are treated as binders
   * unless they're given in the list of literals.
   *)
  fun fromSexp (form, literals) = let
    fun isLiteral id = List.exists (fn id' => id = id') literals
    fun isDot (Ast.Id id) = id = Id.id "."
      | isDot _ = false

    fun toPattern form = fromSexp (form, literals)

    (**
     * Consumes a list of sexp pattern represenations, and converts
     * them into patterns to create a PList. The list of sexps is
     * inspected two at a time, looking for a sequence ("x ...") or a
     * dotted tail (". x"). Until a sequence is found, all patterns
     * are accumulated in the second argument, and the third argument
     * is NONE. Once a sequence has been found the third argument
     * stores a tuple of the sequenced pattern, and all patterns in
     * the list after the sequence.
     *)
    fun plistFromSexps (exp1 :: exp2 :: rest, patterns, NONE) =
        if isDot exp1 then
          case (toPattern exp2, rest) of
            (PVar id, []) => PList (patterns, PRest id)
          | (_, []) => raise Fail "A '.' must be followed by a bind variable."
          | (_, _) => raise Fail "Only one pattern allowed after '.'."
        else if isEllipsis exp2 then
          plistFromSexps (rest, patterns, SOME (toPattern exp1, []))
        else
          plistFromSexps (exp2 :: rest, patterns @ [toPattern exp1], NONE)
      | plistFromSexps (exp1 :: exp2 :: rest, patterns, SOME (seq, tailPatterns)) =
        if isDot exp1 then
          raise Fail "A '.' is not allowed after '...'."
        else if isEllipsis exp2 then
          raise Fail "Only one sequence allowed per list."
        else
          plistFromSexps (exp2 :: rest, patterns, SOME (seq, tailPatterns @ [toPattern exp1]))
      (* The next two cases are only used lists of length < 2 *)
      | plistFromSexps (exps, patterns, NONE) =
        PList (patterns @ (map toPattern exps), PEnd)
      | plistFromSexps (exps, patterns, SOME (seq, tailPatterns)) =
        PList (patterns, PSeq (seq, tailPatterns @ (map toPattern exps)))
  in
    case form of
      Ast.Id id => if isLiteral id then
                     PLiteral (Id id)
                   else
                     PVar id
    | Ast.Num num => PLiteral (Num num)
    | Ast.String string => PLiteral (String string)
    | Ast.Sexp exps => (plistFromSexps (exps, [], NONE))
  end

  (**
   * Collect all the binding variables in a pattern and create a map
   * from the binder to it's binding depth. The binding depth is how
   * many sequences a binder is nested inside of. For example, this
   * pattern:
   *   (a (b) c ... (((d ...) ...) ...))
   * has the following map:
   *   a: 0, b: 0, c: 1, d: 3
   *
   * If a binder occurs more than once in the pattern, an exception is
   * raised.
   *)
  fun getBinderDepths (PVar id, depths, depth) =
      (case Id.IdMap.find (depths, id) of
         NONE => Id.IdMap.insert (depths, id, depth)
       | SOME _ => raise Fail (Id.name id ^ " already defined in pattern."))
    | getBinderDepths (PList (patterns, tail), depths, depth) = let
        val collectDepths = foldr (fn (pattern, depths') =>
                                      getBinderDepths (pattern, depths', depth))
        val depths' = collectDepths depths patterns
        val depths'' = case tail of
                         PSeq (seq, tailPatterns) => collectDepths (getBinderDepths (seq, depths', depth + 1))
                                                                   tailPatterns
                       | PRest id => getBinderDepths (PVar id, depths', depth)
                       | PEnd => depths'
      in
        depths''
      end
    | getBinderDepths (PLiteral _, depths, depth) = depths

  (**
   * A match instantiates a pattern, associating each binder with a
   * bound AST, and each sequence with a list of matches.
   *)
  datatype match = MList of match list * mtail
                 | MVar of Id.id * Ast.exp
                 | MLiteral of literal
       and mtail = MSeq of match list * match list
                 | MRest of Id.id * Ast.exp
                 | MEnd

  fun diffString (pattern, ast) =
      let
        val patternStr = Ast.toString (toSexp pattern)
        val astStr = Ast.toString ast
      in
        "Expected " ^ patternStr ^ ", got " ^ astStr
      end

  (**
   * Attempt to match the given pattern against the given
   * S-expression. Returns a match instantiating the pattern if
   * successful, raises an exception otherwise.
   *)
  fun match (PVar id) ast = MVar (id, ast)
    | match (pattern as PLiteral literal) ast =
      if (toSexp pattern) = ast then
        MLiteral literal
      else
        raise Fail (diffString (pattern, ast))
    | match (PList (patterns, tail)) (Ast.Sexp exps) =
      let
        fun matchList (patterns, exps) =
            ListPair.foldlEq (fn (pattern, exp, matches) =>
                                 match pattern exp :: matches)
                             []
                             (patterns, exps)

        fun matchTail (PSeq (seq, tailPatterns), exps) =
            let
              val (headExps, tailExps) = Util.splitTail (length tailPatterns) exps
              val headMatches = (map (match seq) headExps)
              val tailMatches = matchList (tailPatterns, tailExps)
            in
              MSeq (headMatches, tailMatches)
            end
          | matchTail (PRest id, exps) = MRest (id, Ast.Sexp exps)
          | matchTail (PEnd, []) = MEnd
          | matchTail (_, _) = raise Fail "Couldn't match the tail of the list."

        (* Split the expression list based on where the sequence or
         * dot (if any) appears in the pattern. *)
        val (headExps, tailExps) = Util.splitHead (length patterns) exps
      in
        MList (matchList (patterns, headExps), matchTail (tail, tailExps))
      end
    | match pattern ast = raise Fail (diffString (pattern, ast))
end

structure Template = struct
  open Pattern

  datatype template = TList of titem list
                    | TVar of Id.id
                    | TLiteral of literal
       and titem = TMany of titem * Id.IdSet.set
                 | TOne of template

  (* Generate an error for a template variable not nested within
   * enough sequences. *)
  fun shallowNestingError (id, required_depth, actual_depth) =
      Fail (Id.name id ^ " must be nested in at least " ^ Int.toString
            required_depth ^ " sequences, but it's nested in " ^
            Int.toString actual_depth ^ " sequences.")

  (**
   * Create a template for a pattern, given the binding depths for
   * variables in the pattern, and an s-expression representation of
   * the template.
   *)
  fun fromSexp binderDepths ast = let
    fun fromSexp' (Ast.Num num, _) = TLiteral (Num num)
      | fromSexp' (Ast.String str, _) = TLiteral (String str)
      | fromSexp' (Ast.Id id, current_depth) =
        (* If an identifier appears in the binder depths map, it's a
         * variable, otherwise it's a literal.  If a variable is
         * nested within n sequences in a pattern, it must appear
         * nested within at least n sequences in the corresponding
         * template. *)
        (case Id.IdMap.find (binderDepths, id) of
           SOME required_depth =>
           if required_depth <= current_depth then
             TVar id
           else
             raise shallowNestingError (id, required_depth, current_depth)
         | NONE => TLiteral (Id id))
      | fromSexp' (Ast.Sexp exps, current_depth) = TList []
  in
    fromSexp' (ast, 0)
  end

end
