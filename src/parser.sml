structure Parser = struct
  structure SexpParser = SexpParseFn(Lexer)

  exception ParseFailure of (SexpTokens.token AntlrRepair.repair) list

  fun parse input = let
    val sourceMap = AntlrStreamPos.mkSourcemap ()
    val lex = Lexer.lex sourceMap
    val stream = Lexer.streamifyInstream input
    val (maybeAst, stream', errors) = SexpParser.parse lex stream
    val _ = TextIO.closeIn input
  in
    case (maybeAst, errors) of
      (SOME ast, []) => ast
    | _ => raise ParseFailure errors
  end

  fun parseFile filename =
      parse (TextIO.openIn filename)

  fun parseString str =
      parse (TextIO.openString str)

  (* A super naive reader: read input until a successful parse occurs. *)
  fun parseStdIn () = let
    fun loop str =
        let
          val str' = str ^ (valOf (TextIO.inputLine TextIO.stdIn))
        in
          parseString str'
          handle ParseFailure _ => loop str'
        end
  in
    loop ""
  end

  fun repl () : unit =
      (parseStdIn (); repl ())
        (* A convenience method for getting a single expression. *)

  fun readExp string =
      case parseString string of
        [exp] => exp
      | [] => raise Fail "Required 1 expression, got none."
      | exps => raise Fail ("Required 1 expression, got " ^ Int.toString (length exps))
end
