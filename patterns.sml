(* Scheme macros may be defined using a simple yet powerful tool
 * called "syntax-rules" which gives a language for matching and
 * transforming patterns of s-expressions. Patterns is a similar tool
 * for parsing syntax-rules style definitions from S-expressions to
 * generate rewrite rules, and then applying those rules. *)
structure Patterns = struct

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
   * from the binder to it's binding level. The binding level is how
   * many sequences a binder is nested inside of. For example, this
   * pattern:
   *   (a (b) c ... (((d ...) ...) ...))
   * has the following map:
   *   a: 0, b: 0, c: 1, d: 3
   *
   * If a binder occurs more than once in the pattern, an exception is
   * raised.
   *)
  fun getBinderLevels (PVar id, levels, level) =
      (case Id.IdMap.find (levels, id) of
         NONE => Id.IdMap.insert (levels, id, level)
       | SOME _ => raise Fail (Id.name id ^ " already defined in pattern."))
    | getBinderLevels (PList (patterns, tail), levels, level) = let
        val collectLevels = foldr (fn (pattern, levels') =>
                                      getBinderLevels (pattern, levels', level))
        val levels' = collectLevels levels patterns
        val levels'' = case tail of
                         PSeq (seq, tailPatterns) => collectLevels (getBinderLevels (seq, levels', level + 1))
                                                                   tailPatterns
                       | PRest id => getBinderLevels (PVar id, levels', level)
                       | PEnd => levels'
      in
        levels''
      end
    | getBinderLevels (PLiteral _, levels, level) = levels

  (**
   * A match instantiates a pattern, associating each binder with a
   * bound AST, and each sequence with a list of matches.
   *)
  datatype match = MList of match list * mtail
                 | MVar of Id.id * Ast.exp
                 | MLiteral of literal
       and mtail = MSeq of match list * match list
                 | MRest of match
                 | MEnd

  fun diffString (pattern, ast) =
      let
        val patternStr = Ast.toString (toSexp pattern)
        val astStr = Ast.toString ast
      in
        "Expected " ^ patternStr ^ ", got " ^ astStr
      end

  fun match (pattern as PLiteral literal, ast) =
      if (toSexp pattern) = ast then
        MLiteral literal
      else
        raise Fail (diffString (pattern, ast))
    | match (PVar id, ast) = MVar (id, ast)
    | match _ = MList ([], MEnd)

end
