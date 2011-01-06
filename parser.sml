structure Parser = struct
  structure SexpParser = SexpParseFn(Lexer)

  datatype result = Success of Ast.exp list
                  | Failure

  fun parse input = let
    val sourceMap = AntlrStreamPos.mkSourcemap ()
    val lex = Lexer.lex sourceMap
    val stream = Lexer.streamifyInstream input
    val (maybeAst, stream', errors) = SexpParser.parse lex stream
    val _ = TextIO.closeIn input
  in
    case (maybeAst, errors) of
      (SOME ast, []) => Success ast
    | _ => Failure
  end

  fun parseFile filename =
      parse (TextIO.openIn filename)

  fun parseString str =
      parse (TextIO.openString str)

  (* A super naive reader: read input until a successful parse occurs. *)
  fun read () = let
    fun loop str =
        let
          val str' = str ^ (valOf (TextIO.inputLine TextIO.stdIn))
          val result = parseString str'
        in
          case result of
            Success _ => result
          | Failure => loop str'
        end
  in
    loop ""
  end

  fun repl () : unit =
      (read (); repl ())

  fun getAst (Success ast) = ast
    | getAst _ = raise Fail "Parse failed"

end
