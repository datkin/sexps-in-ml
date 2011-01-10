structure TemplateTest = TestRunner(struct
  open Test
  open Parser
  open Template

  (* Succeeds if the template is valid for the pattern. *)
  fun newTemplate patternStr templateStr = let
    val pattern = Pattern.makePattern (readExp patternStr, [])
    val template = makeTemplate (readExp templateStr, #binderDepths pattern)
  in
    true
  end

  (* Use SyntaxRule.makeRule to test Template.expand. *)
  fun expansion patternStr templateStr inputStr expectedStr = let
      val f = SyntaxRule.makeRule (readExp patternStr, readExp templateStr, [])
      val actual = f (readExp inputStr)
      val expected = readExp expectedStr
  in
    if actual = expected then true
    else (print ("Expected " ^ Ast.toString expected ^ ", got " ^ Ast.toString actual); false)
  end

  val tests = [
      Test ("simple template",
         fn _ => newTemplate "(a ...)" "(a ...)"),
      Test ("legal over nesting 1",
         fn _ => newTemplate "(a b ...)" "((a b) ...)"),
      Test ("legal over nesting 2",
         fn _ => newTemplate "((a ...) (b ...))" "(((a) b) ...)"),
      Test ("illegal under nesting",
         fn _ => (newTemplate "((a ...) (b ...))" "(a b ...)"; false)
            handle _ => true),
      Test ("illegal over nesting 1",
         fn _ => (newTemplate "(a ...)" "(b ...)"; false)
            handle _ => true),
      Test ("illegal over nesting 2",
         fn _ => (newTemplate "((a ...) (b ...))" "((a b) ... ...)"; false)
            handle _ => true),
      Test ("flat nesting",
         fn _ => newTemplate "((a ...) ...)" "(a ... ...)"),

      Test ("simple expansion",
         fn _ => expansion "(a b c)" "(c b a)" "(1 2 3)" "(3 2 1)"),

      Test ("sequence expansion",
         fn _ => expansion "(a ...)" "((a) ...)" "(1 2 3)" "((1) (2) (3))"),

      Test ("over nested expansion",
         fn _ => expansion "(a b ...)" "((a b) ...)" "(1 2 3)" "((1 2) (1 3))"),

      Test ("paired expansion",
         fn _ => expansion "((a ...) (b ...))" "((a b) ...)" "((1 2 3) (x y z))" "((1 x) (2 y) (3 z))"),

      Test ("fail on unequal paired expansion",
         fn _ => (expansion "((a ...) (b ...))" "((a b) ...)" "((1 2) (x y z))" "-"; false)
            handle _ => true),

      Test ("flat expansion",
         fn _ => expansion "((a ...) ...)" "(a ... ...)" "((1) () (2 3))" "(1 2 3)"),

      Test ("literal expansion",
         fn _ => expansion "(1 \"foo\" (x))" "(2 \"bar\" x)" "(1 \"foo\" (a))" "(2 \"bar\" a)"),

      Test ("dot expansion",
         fn _ => expansion "(. b)" "(x b)" "(1 2 3)" "(x (1 2 3))"),

      Test ("empty sequence expansion",
         fn _ => expansion "((a ...) (b ...) ...)"
                           "((a b) ... ...)"
                           "((1 2 3) () (x) (y z))"
                           "((1 x) (2 y) (2 z))"
                           (* "((2 x) (3 y) (3 z))" *))

  ]

  val continueOnFailure = false
end)
