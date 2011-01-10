structure PatternTest = TestRunner(struct
  open Test
  open Parser
  open Pattern

  val a = Id.id "a"
  val b = Id.id "b"
  val c = Id.id "c"
  val d = Id.id "d"

  fun doMatch (patternStr, inputStr) = let
    val pattern = fromSexp (readExp patternStr, [])
    val input = readExp inputStr
  in
    match pattern input
  end

  fun toMap (items: (Id.IdMap.Key.ord_key * 'a) list) =
      foldr Id.IdMap.insert' Id.IdMap.empty items

  fun mapsEqual (map1, map2) =
      (Id.IdMap.listItemsi map1) = (Id.IdMap.listItemsi map2)

  val tests = [
      Test ("literal",
         fn _ => fromSexp (readExp "a", [Id.id "a"]) = PLiteral (Id a)),
      Test ("variable",
         fn _ => fromSexp (readExp "a", []) = PVar a),
      Test ("string literal",
         fn _ => fromSexp (readExp "\"a\"", []) = PLiteral (String "a")),
      Test ("numeric literal",
         fn _ => fromSexp (readExp "1", []) = PLiteral (Num (Ast.Int 1))),
      Test ("empty list",
         fn _ => fromSexp (readExp "()", []) = PList ([], PEnd)),
      Test ("simple list",
         fn _ => fromSexp (readExp "(a b c)", []) = PList ([(PVar a), (PVar b), (PVar c)], PEnd)),
      Test ("sequence",
         fn _ => fromSexp (readExp "(a ...)", []) = PList ([], PSeq (PVar a, []))),
      Test ("complex sequence",
         fn _ => fromSexp (readExp "(a b ... c)", []) = PList ([PVar a], PSeq (PVar b, [PVar c]))),
      Test ("dotted tail",
         fn _ => fromSexp (readExp "(. b)", []) = PList ([], PRest b)),

      Test ("binder depths",
         fn _ => let
              val pattern = fromSexp (readExp "(a (b) c ... (((d ...) ...) ...))", [])
              val actual = getBinderDepths (pattern, Id.IdMap.empty, 0)
              val expected = toMap [(a, 0), (b, 0), (c, 1), (d, 3)]
            in
              mapsEqual (actual, expected)
            end),

      (* Test that the matches succeed, don't actually check the results... *)
      Test ("empty sequence match",
         fn _ => (doMatch ("(a ...)", "()"); true)),

      Test ("sequence match",
         fn _ => (doMatch ("(a ...)", "(1 2 3)"); true)),

      Test ("empty tail match",
         fn _ => (doMatch ("(. b)", "()"); true)),

      Test ("tail match",
         fn _ => (doMatch ("(. b)", "(1 2 3)"); true)),

      Test ("no bindings",
         fn _ => mapsEqual (matchToBindings (doMatch ("(1)", "(1)")),
                            Id.IdMap.empty)),

      Test ("simple binding",
         fn _ => mapsEqual (matchToBindings (doMatch ("(a)", "(a)")),
                            toMap [(a, Binding (Ast.Id a))])),

      Test ("sequence binding",
         fn _ => mapsEqual (matchToBindings (doMatch ("(a ...)", "(a b c)")),
                            toMap [(a, Nested [Binding (Ast.Id a),
                                               Binding (Ast.Id b),
                                               Binding (Ast.Id c)])])),

      Test ("tail binding",
         fn _ => mapsEqual (matchToBindings (doMatch ("(. b)", "(a b c)")),
                            toMap [(b, Binding (readExp "(a b c)"))]))
  ]

  val continueOnFailure = false
end)
