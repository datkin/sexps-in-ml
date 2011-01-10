structure ParseTest = TestRunner(struct
  open Test

  fun assertEqual (expected: string, actual: string) =
      if expected = actual then true
      else raise Fail ("Expected: " ^ expected ^ ", got " ^ actual ^ "\n")

  fun roundtrip str = let
    val exp = Parser.readExp str
  in
    assertEqual (str, Ast.toString exp)
  end

  val all = List.all;

  val tests = [
      Test ("floats",
         fn _ => all roundtrip ["1.0", "100.0",  "100.00", "0.1", "0.010", "3.14159"]),
      Test ("rationals",
         fn _ => all roundtrip ["1/2", "123/425" (*, "-1/3" *)]),
      Test ("symbols",
         fn _ => all roundtrip ["a", "a'", "abc", "abc-123"])
  ];

  val continueOnFailure = false;
end)
