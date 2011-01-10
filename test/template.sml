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
         fn _ => newTemplate "((a ...) ...)" "(a ... ...)")
  ]

  val continueOnFailure = false
end)
