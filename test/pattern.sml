structure PatternTest = TestRunner(struct
  open Test
  open Parser
  open Pattern

  val a = Id.id "a"
  val b = Id.id "b"
  val c = Id.id "c"

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
         fn _ => fromSexp (readExp "(. b)", []) = PList ([], PRest b))
  ]

  val continueOnFailure = false
end)
