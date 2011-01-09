structure ParseTest = TestRunner(
struct
  open Test

  fun assertEqual (expected: string, actual: string) =
      if expected = actual then ()
      else raise Fail ("Expected: " ^ expected ^ ", got " ^ actual ^ "\n")

  fun roundtrip str = let
    val exp = Parser.readExp str
  in
    assertEqual (str, Ast.toString exp)
  end

  val tests = [
      Test ("floats",
         fn () => (roundtrip "1.0";
                   roundtrip "100.0";
                   roundtrip "100.00";
                   roundtrip "0.1";
                   roundtrip "0.010";
                   roundtrip "3.14159")),
      Test ("rationals",
         fn () => (roundtrip "1/2";
                   roundtrip "123/425" (*;
                   roundtrip "-1/3" *))),
      Test ("symbols",
         fn () => (roundtrip "a";
                   roundtrip "a'";
                   roundtrip "abc";
                   roundtrip "abc-123"))
  ];

  val continueOnFailure = false;
end)
