structure Test = struct
  datatype test = Test of string * (unit -> bool)
end

functor TestRunner (val tests: Test.test list val continueOnFailure: bool) = struct
  open Test

  fun run (Test (name, test)) = let
    val _ = print ("Testing '" ^ name ^ "'... ")
    val (successful, error) =
        ((test (), false)
         handle error => (print "failed.\n";
                          if continueOnFailure then (false, true)
                          else raise error))
    val _ = if successful then print "passed.\n"
            else if not error then print "failed.\n"
            else ()
  in
    successful
  end

  fun runTests () = let
    val total = length tests
    val passed = length (List.filter run tests)
    val failed = total - passed
  in
    if failed = 0 then
      print "All tests passed.\n"
    else
    print (Int.toString failed ^ "/" ^ Int.toString total ^ " tests failed.\n")
  end

  val _ = runTests ();
end
