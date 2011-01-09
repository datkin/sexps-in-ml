structure Test = struct
  datatype test = Test of string * (unit -> unit)
end

functor TestRunner (val tests: Test.test list val continueOnFailure: bool) = struct
  open Test

  fun handleError error =
      if continueOnFailure then false
      else raise error

  fun statusToString success =
      if success then "passed"
      else "failed"

  fun run (Test (name, test)) = let
    val _ = print ("Testing '" ^ name ^ "'... ")
    val successful = ((test ();
                       true)
                      handle error => (print "failed.\n";
                                       if continueOnFailure then false
                                       else raise error))
    val _ = if successful then print "passed.\n"
            else ()
  in
    successful
  end

  fun runTests () = let
    val total = length tests
    val passed = length (List.filter run tests)
  in
    print (Int.toString passed ^ "/" ^ Int.toString total ^ " tests passed.\n")
  end

  val _ = runTests ();
end
