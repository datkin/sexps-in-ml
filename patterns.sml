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
        val collectDepths = foldl (fn (pattern, depths') =>
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

  (**
   * A binding represents all the S-expressions that a bind variable
   * matched for some pattern.  An un-nested variable yields a lone
   * Binding. A variable nested in n times yields a full tree of depth
   * n, with Bindings at the leaves.
   *)
  datatype binding = Binding of Ast.exp
                   | Nested of binding list

  (**
   * Convert a match structure to a map of bindings for each variable
   * in the match.
   *)
  fun matchToBindings match =
      case match of
        MLiteral _ => Id.IdMap.empty
      | MVar (id, exp) => Id.IdMap.singleton (id, Binding exp)
      | MList (matches, tail) => let
          (* Append two nested bindings. *)
          fun appendBindings (Nested b1, Nested b2) = Nested (b1 @ b2)
            | appendBindings (_, _) = raise Fail "Can only append two nested bindings."

          (* Merge a list of bindings into one. *)
          val merge = foldl (Id.IdMap.unionWith appendBindings) Id.IdMap.empty

          (* Wrap all the bindings in a map in "Nested". *)
          val wrap = Id.IdMap.map (fn b => Nested [b])

          val bindings = merge (map matchToBindings matches)
          val bindings' =
              case tail of
                MEnd => Id.IdMap.empty
              | MRest (id, exp) => Id.IdMap.singleton (id, Binding exp)
              | MSeq (seqMatches, tailMatches) => let
                  val seqBindings = merge (map (wrap o matchToBindings) seqMatches)
                  val tailBindings = merge (map matchToBindings tailMatches)
                in
                  merge [seqBindings, tailBindings]
                end
        in
          merge [bindings, bindings']
        end

end

structure Template = struct
  open Pattern

  datatype template = TList of titem list
                    | TVar of Id.id
                    | TLiteral of literal
 (**
  * In a pattern, there may be only one sequence per list, so nested
  * sequences are always lexically nested as well. In a templates
  * however, nested sequences may be lexically flattened. For example,
  * this pattern:
  *   (a (b ...) (c ...) ...)
  * could have this template:
  *   (a b ... c ... ...)
  * which would expand, e.g., like this:
  *   (1 () (2 3) (4 5 6)) -> (1 2 3 4 5 6)
  * Thus, every template in a list is wrapped in a "TSingleton" item,
  * and for every sequence immediately following the template, the
  * singleton is wrapped in a "TSequence". So the example above would
  * be:
  *   TList [TSingleton (Id.id "a"),
  *          TSequence (TSingleton (Id.id "b")),
  *          TSequence (TSequence (TSingleton (Id.id "c")))]
  *)
       and titem = TSequence of titem
                 | TSingleton of template


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

  fun shallowNesting (id, requiredDepth, actualDepth) =
      Fail (Id.name id ^ " must be nested in at least " ^ Int.toString
            requiredDepth ^ " sequences, but it's nested in " ^
            Int.toString actualDepth ^ " sequences.")

  fun invalidVariable id =
      Fail (Id.name id ^ " is not a valid bind variable.")

  fun depthMismatch (deepestBinder, depth) =
      Fail ("Deepest binders are " ^ (Int.toString deepestBinder) ^
            ", but they are nested " ^ (Int.toString depth) ^ " deep.")

  (**
   * There are two properties of templates that must be validated.
   * (1) Every variable must be nested at least as deeply as it
   *     appears in the corresponding pattern. Consider this pattern:
   *       ((a ...) (b ...))
   *     This template would be invalid because 'a' isn't nested:
   *       (a b ...)
   * (2) Sequenced templates cannot be nested more deeply than the
   *     deepest variable they contain. For the above pattern, this
   *     template is invalid, because 'a' and 'b' are sequenced twice
   *     but they are both only depth one:
   *       ((a b) ... ...)
   *
   * Note that it's valid for more shallow variables to appear as
   * deeply nested variables as the deepest variables. For instance,
   * this pattern:
   *   (a b ...)
   * may have the following template:
   *   ((a b) ...)
   *
   * Property (1) is checked by comparing every variable's binding
   * depth with the depth of its appearance in the template.
   *
   * Property (2) is checked by comparing the deepest binding depth of
   * any variable in the sequence, with the depth of the sequence.
   *
   * The nesting depth is propogated down the call stack, and the
   * deepest binder depth for each sequence is returned.
   *)
  fun validate (template, binderDepths, currentDepth) =
      case template of
        TLiteral _ => 0
      | TVar id => let
          val requiredDepth = case Id.IdMap.find (binderDepths, id) of
                                SOME depth => depth
                              | NONE => raise invalidVariable id
        in
          if currentDepth < requiredDepth
          then raise shallowNesting (id, requiredDepth, currentDepth)
          else requiredDepth
        end
      | TList items => let
          fun validateItem depth (TSequence item) = (validateItem (depth + 1) item) - 1
            | validateItem depth (TSingleton template) = let
                val deepest = validate (template, binderDepths, depth)
              in
                if deepest <> depth
                then raise depthMismatch (deepest, depth)
                else deepest
              end
        in
          foldl Int.max 0 (map (validateItem currentDepth) items)
        end

  fun makeTemplate (sexp, binderDepths) = let
    val template = fromSexp binderDepths sexp
    val _ = validate (template, binderDepths, 0)
  in
    {template = template}
  end

end
