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

  (* A predicate for ellipses. *)
  fun isEllipsis (Ast.Id id) = id = Id.id "..."
    | isEllipsis _ = false

                     (*
  type pattern_info = {pattern: pattern, binders: Id.IdSet.set}
                      *)

  (**
   * Creates a pattern value given it's sexp representation. All
   * indentifiers in the pattern definition are treated as binders
   * unless they're given in the list of literals.
   *
   *)
  fun makePattern (form, literals) = let
    fun isLiteral id = List.exists (fn id' => id = id') literals
    fun isDot (Ast.Id id) = id = Id.id "."
      | isDot _ = false

    fun toPattern form = makePattern (form, literals)

    (**
     * Consumes a list of sexp pattern represenations, and converts
     * them into patterns to create a PList. The list of sexps is
     * inspected two at a time, looking for a sequence ("x ...") or a
     * dotted tail (". x"). Until a sequence is found, all patterns
     * are accumulated in the second argument, and the third argument
     * is NONE. Once a sequence has been found, the third argument
     * stores a tuple of the sequenced pattern, and all patterns in
     * the list after the sequence.
     *)
    fun makePatternList (exp1 :: exp2 :: rest, patterns, NONE) =
        if isDot exp1 then
          case (toPattern exp2, rest) of
            (PVar id, []) => PList (patterns, PRest id)
          | (_, []) => raise Fail "A '.' must be followed by a bind variable."
          | (_, _) => raise Fail "Only one pattern allowed after '.'."
        else if isEllipsis exp2 then
          makePatternList (rest, patterns, SOME (toPattern exp1, []))
        else
          makePatternList (exp2 :: rest, patterns @ [toPattern exp1], NONE)
      | makePatternList (exp1 :: exp2 :: rest, patterns, SOME (seq, tailPatterns)) =
        if isDot exp1 then
          raise Fail "A '.' is not allowed after '...'."
        else if isEllipsis exp2 then
          raise Fail "Only one sequence allowed per list."
        else
          makePatternList (exp2 :: rest, patterns, SOME (seq, tailPatterns @ [toPattern exp1]))
      (* The next two cases are only used lists of length < 2 *)
      | makePatternList (exps, patterns, NONE) =
        PList (patterns @ (map toPattern exps), PEnd)
      | makePatternList (exps, patterns, SOME (seq, tailPatterns)) =
        PList (patterns, PSeq (seq, tailPatterns @ (map toPattern exps)))
  in
    case form of
      Ast.Id id => if isLiteral id then
                     PLiteral (Id id)
                   else
                     PVar id
    | Ast.Num num => PLiteral (Num num)
    | Ast.String string => PLiteral (String string)
    | Ast.Sexp exps => (makePatternList (exps, [], NONE))
  end
end
  (*   At most one pattern inside a list may be
       followed by an ellipsis indicating a "sequence" that should be
       greedily matched zero or more times. If a list has no
       sequences, it may end with a . and a binder indicating the
       remainder of the input form should be bound to the binder. *)
