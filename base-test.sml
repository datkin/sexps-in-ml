structure BaseTest = TestRunner(struct
  open Test

  val tests = [
      Test ("ids are canonical",
         fn _ => let
              val id1 = Id.id "x"
              val id2 = Id.id "x"
            in
              if id1 = id2 then true
              else raise Fail "Ids with the same name should be equal."
            end),
      Test ("ids roundtrip",
           fn _ =>
              if Id.name (Id.id "x") = "x" then true
              else raise Fail "Ids should roundtrip.")]

  val continueOnFailure = false
end)
