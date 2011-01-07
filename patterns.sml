(**
 * Scheme macros may be defined using a simple yet powerful tool
 * called "syntax-rules" which gives a language for matching and
 * transforming patterns of s-expressions. Here are some tools for
 * creating "syntax-rules"-style defintions, and using them to
 * transform S-expressions.
 *
 * There are two component structures:
 * - 'Pattern' for defining patterns, and using them to match
 *    S-expressions.
 * - 'Template' for defining templates for a pattern, and
 *   transforming corresponding pattern matches to S-expressions.
 *)
structure Pattern = struct

  (* Syntax elements that should be matched literally. *)
  datatype literal = Id of Id.id
                   | Num of Ast.num
                   | String of string

  (**
   * A pattern is one of:
   * - A literal string or number.
   * - An identifier, which may either be a literal or a binding
   *   variable.
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
  datatype pattern = PLiteral of literal
                   | PVar of Id.id
                   | PList of pattern list * ptail
       and ptail = PSeq of pattern * pattern list
                 | PRest of Id.id
                 | PEnd

  val ellipsis = Id.id "..."
  val dot = Id.id "."

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
   * Create a pattern value given it's sexp representation. All
   * indentifiers in the pattern definition are treated as binders
   * unless they're given in the list of literals.
   *)
  fun fromSexp (form, literals) = let
    fun isLiteral id = List.exists (fn id' => id = id') literals
    fun isDot (Ast.Id id) = id = Id.id "."
      | isDot _ = false

    fun toPattern form = fromSexp (form, literals)

    (**
     * Consume a list of sexp pattern represenations, and convert them
     * into patterns to create a PList. The list of sexps is inspected
     * two at a time, looking for a sequence ("x ...") or a dotted
     * tail (". x"). Until a sequence is found, all patterns are
     * accumulated in the second argument, and the third argument is
     * NONE. Once a sequence has been found the third argument stores
     * a tuple of the sequenced pattern, and all patterns in the list
     * after the sequence.
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
   * Parse, validate, and analyze a pattern for a given S-expression.
   *)
  fun makePattern (sexp, literals) = let
    val pattern = fromSexp (sexp, literals);
    val binderDepths = getBinderDepths (pattern, Id.IdMap.empty, 0)
  in
    {pattern = pattern, binderDepths = binderDepths}
  end

  (**
   * A match instantiates a pattern, associating each binder with a
   * bound AST, and each sequence with a list of matches.
   *)
  datatype match = MLiteral of literal
                 | MVar of Id.id * Ast.exp
                 | MList of match list * mtail
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
   * successful, otherwise raises an exception.
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

(*
 Validate:
  (1) every variable must be nested deeply enough.
      Accomplish this by counting the depth as we nest.
  (2) sequenced templates may not be nested more deeply than nest more deeply than their deepest used variable
      Accomplish this by comparing the nest of the deepest variable with the depth

 ((a ...) (b ...))
 legal: ((a b) ...)
 illegal: (a b ...), ((a b) ... ...)

 (a b ...)
 legal: ((a b) ...)

 *)

structure Template = struct
  open Pattern

  datatype template = TList of titem list
                    | TVar of Id.id
                    | TLiteral of literal
                     (* Each template in a list may be nested in arbitrarily deep sequences... *)
       and titem = TSequence of titem (* I did these as nested TMany's before... hrmm *)
                 | TSingleton of template

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
  fun fromSexp binderDepths ast =
      case ast of
        Ast.Num num => TLiteral (Num num)
      | Ast.String str => TLiteral (String str)
      | Ast.Id id =>
        (* If an identifier appears in the binder depths map, it's a
         * variable, otherwise it's a literal. *)
        (case Id.IdMap.find (binderDepths, id) of
           SOME _ => TVar id
         | NONE => TLiteral (Id id))
      | Ast.Sexp exps => let
          (* Pull expressions from the list one at a time.  Unless the
           * expression is an ellipsis, parse it and wrap it in a
           * "TSingleton".  Continue down the list consuming as many
           * ellipses as possible. For each ellipsis, wrap the item
           * with a "TSequence". Once a non-ellipsis is encountered,
           * repeat the process with the new expression. *)
          fun toItem exp = TSingleton (fromSexp binderDepths exp)

          fun itemsFromSexps (SOME prevItem, nextExp :: rest) =
              if isEllipsis nextExp then
                itemsFromSexps (SOME (TSequence prevItem), rest)
              else
                prevItem :: itemsFromSexps (SOME (toItem nextExp), rest)
            | itemsFromSexps (NONE, nextExp :: rest) =
              if isEllipsis nextExp then
                raise Fail "'...' must be preceeded by a template."
              else
                itemsFromSexps (SOME (toItem nextExp), rest)
            | itemsFromSexps (SOME prevItem, []) = [prevItem]
            | itemsFromSexps (NONE, []) = []
        in
          TList (itemsFromSexps (NONE, exps))
        end

end
