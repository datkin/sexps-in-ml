structure 
SexpTokens = struct

    datatype token = EOF
      | UNUSED
      | COMMENT
      | UNQUOTE
      | QUASIQUOTE
      | QUOTE
      | RBRACK
      | LBRACK
      | RPAREN
      | LPAREN
      | RATIONAL of (LargeInt.int * LargeInt.int)
      | FLOAT of (LargeInt.int * LargeInt.int)
      | STRING of string
      | INT of LargeInt.int
      | ID of string

    val allToks = [EOF, UNUSED, COMMENT, UNQUOTE, QUASIQUOTE, QUOTE, RBRACK, LBRACK, RPAREN, LPAREN]

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (UNUSED) => "UNUSED"
  | (COMMENT) => "COMMENT"
  | (UNQUOTE) => "UNQUOTE"
  | (QUASIQUOTE) => "QUASIQUOTE"
  | (QUOTE) => "QUOTE"
  | (RBRACK) => "RBRACK"
  | (LBRACK) => "LBRACK"
  | (RPAREN) => "RPAREN"
  | (LPAREN) => "LPAREN"
  | (RATIONAL(_)) => "RATIONAL"
  | (FLOAT(_)) => "FLOAT"
  | (STRING(_)) => "STRING"
  | (INT(_)) => "INT"
  | (ID(_)) => "ID"
(* end case *))
    fun isKW tok =
(case (tok)
 of (EOF) => false
  | (UNUSED) => false
  | (COMMENT) => false
  | (UNQUOTE) => false
  | (QUASIQUOTE) => false
  | (QUOTE) => false
  | (RBRACK) => false
  | (LBRACK) => false
  | (RPAREN) => false
  | (LPAREN) => false
  | (RATIONAL(_)) => false
  | (FLOAT(_)) => false
  | (STRING(_)) => false
  | (INT(_)) => false
  | (ID(_)) => false
(* end case *))


  fun toksToString toks = String.concatWith " " (map toString toks)

  fun isEOF EOF = true
    | isEOF _ = false

end

functor SexpParseFn(Lex : ANTLR_LEXER) = struct

  local
    structure Tok = 
SexpTokens
    structure UserCode = struct

 
  open Ast


fun explist_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [])
fun explist_PROD_2_ACT (exp, COMMENT, explist, exp_SPAN : (Lex.pos * Lex.pos), COMMENT_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( explist)
fun explist_PROD_3_ACT (exp, explist, exp_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( exp :: explist)
fun exp_PROD_1_ACT (RPAREN, explist, LPAREN, RPAREN_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), LPAREN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Sexp (explist))
fun exp_PROD_2_ACT (RBRACK, explist, LBRACK, RBRACK_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), LBRACK_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Sexp (explist))
fun exp_PROD_3_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Id (Id.id ID))
fun exp_PROD_4_ACT (num, num_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Num (num))
fun exp_PROD_5_ACT (STRING, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( String (STRING))
fun exp_PROD_6_ACT (exp, QUOTE, exp_SPAN : (Lex.pos * Lex.pos), QUOTE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Sexp ([Id (Id.id "quote"), exp]))
fun exp_PROD_7_ACT (exp, QUASIQUOTE, exp_SPAN : (Lex.pos * Lex.pos), QUASIQUOTE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Sexp ([Id (Id.id "quasiquote"), exp]))
fun exp_PROD_8_ACT (exp, UNQUOTE, exp_SPAN : (Lex.pos * Lex.pos), UNQUOTE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Sexp ([Id (Id.id "unquote"), exp]))
fun num_PROD_1_ACT (INT, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Int INT)
fun num_PROD_2_ACT (FLOAT, FLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Float FLOAT)
fun num_PROD_3_ACT (RATIONAL, RATIONAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Rational RATIONAL)

    end

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)
    structure EBNF = AntlrEBNF(struct
			         type strm = Err.wstream
			         val getSpan = Err.getSpan
			       end)

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, strm, repairs)
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) = 
	        (Err.whileDisabled eh (fn() => prod strm)) 
		handle Err.ParseError => try (prods)
          in try prods end
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchUNUSED strm = (case (lex(strm))
 of (Tok.UNUSED, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOMMENT strm = (case (lex(strm))
 of (Tok.COMMENT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchUNQUOTE strm = (case (lex(strm))
 of (Tok.UNQUOTE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchQUASIQUOTE strm = (case (lex(strm))
 of (Tok.QUASIQUOTE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchQUOTE strm = (case (lex(strm))
 of (Tok.QUOTE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRBRACK strm = (case (lex(strm))
 of (Tok.RBRACK, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLBRACK strm = (case (lex(strm))
 of (Tok.LBRACK, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRPAREN strm = (case (lex(strm))
 of (Tok.RPAREN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLPAREN strm = (case (lex(strm))
 of (Tok.LPAREN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRATIONAL strm = (case (lex(strm))
 of (Tok.RATIONAL(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchFLOAT strm = (case (lex(strm))
 of (Tok.FLOAT(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchSTRING strm = (case (lex(strm))
 of (Tok.STRING(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchINT strm = (case (lex(strm))
 of (Tok.INT(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchID strm = (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))

val (prog_NT) = 
let
fun num_NT (strm) = let
      fun num_PROD_1 (strm) = let
            val (INT_RES, INT_SPAN, strm') = matchINT(strm)
            val FULL_SPAN = (#1(INT_SPAN), #2(INT_SPAN))
            in
              (UserCode.num_PROD_1_ACT (INT_RES, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun num_PROD_2 (strm) = let
            val (FLOAT_RES, FLOAT_SPAN, strm') = matchFLOAT(strm)
            val FULL_SPAN = (#1(FLOAT_SPAN), #2(FLOAT_SPAN))
            in
              (UserCode.num_PROD_2_ACT (FLOAT_RES, FLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun num_PROD_3 (strm) = let
            val (RATIONAL_RES, RATIONAL_SPAN, strm') = matchRATIONAL(strm)
            val FULL_SPAN = (#1(RATIONAL_SPAN), #2(RATIONAL_SPAN))
            in
              (UserCode.num_PROD_3_ACT (RATIONAL_RES, RATIONAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.RATIONAL(_), _, strm') => num_PROD_3(strm)
          | (Tok.INT(_), _, strm') => num_PROD_1(strm)
          | (Tok.FLOAT(_), _, strm') => num_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun explist_NT (strm) = let
      fun explist_PROD_1 (strm) = let
            val FULL_SPAN = (Err.getPos(strm), Err.getPos(strm))
            in
              (UserCode.explist_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm)
            end
      fun explist_PROD_2 (strm) = let
            val (COMMENT_RES, COMMENT_SPAN, strm') = matchCOMMENT(strm)
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val (explist_RES, explist_SPAN, strm') = explist_NT(strm')
            val FULL_SPAN = (#1(COMMENT_SPAN), #2(explist_SPAN))
            in
              (UserCode.explist_PROD_2_ACT (exp_RES, COMMENT_RES, explist_RES, exp_SPAN : (Lex.pos * Lex.pos), COMMENT_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun explist_PROD_3 (strm) = let
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm)
            val (explist_RES, explist_SPAN, strm') = explist_NT(strm')
            val FULL_SPAN = (#1(exp_SPAN), #2(explist_SPAN))
            in
              (UserCode.explist_PROD_3_ACT (exp_RES, explist_RES, exp_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => explist_PROD_3(strm)
          | (Tok.INT(_), _, strm') => explist_PROD_3(strm)
          | (Tok.STRING(_), _, strm') => explist_PROD_3(strm)
          | (Tok.FLOAT(_), _, strm') => explist_PROD_3(strm)
          | (Tok.RATIONAL(_), _, strm') => explist_PROD_3(strm)
          | (Tok.LPAREN, _, strm') => explist_PROD_3(strm)
          | (Tok.LBRACK, _, strm') => explist_PROD_3(strm)
          | (Tok.QUOTE, _, strm') => explist_PROD_3(strm)
          | (Tok.QUASIQUOTE, _, strm') => explist_PROD_3(strm)
          | (Tok.UNQUOTE, _, strm') => explist_PROD_3(strm)
          | (Tok.RPAREN, _, strm') => explist_PROD_1(strm)
          | (Tok.RBRACK, _, strm') => explist_PROD_1(strm)
          | (Tok.EOF, _, strm') => explist_PROD_1(strm)
          | (Tok.COMMENT, _, strm') => explist_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and exp_NT (strm) = let
      fun exp_PROD_1 (strm) = let
            val (LPAREN_RES, LPAREN_SPAN, strm') = matchLPAREN(strm)
            val (explist_RES, explist_SPAN, strm') = explist_NT(strm')
            val (RPAREN_RES, RPAREN_SPAN, strm') = matchRPAREN(strm')
            val FULL_SPAN = (#1(LPAREN_SPAN), #2(RPAREN_SPAN))
            in
              (UserCode.exp_PROD_1_ACT (RPAREN_RES, explist_RES, LPAREN_RES, RPAREN_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), LPAREN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_2 (strm) = let
            val (LBRACK_RES, LBRACK_SPAN, strm') = matchLBRACK(strm)
            val (explist_RES, explist_SPAN, strm') = explist_NT(strm')
            val (RBRACK_RES, RBRACK_SPAN, strm') = matchRBRACK(strm')
            val FULL_SPAN = (#1(LBRACK_SPAN), #2(RBRACK_SPAN))
            in
              (UserCode.exp_PROD_2_ACT (RBRACK_RES, explist_RES, LBRACK_RES, RBRACK_SPAN : (Lex.pos * Lex.pos), explist_SPAN : (Lex.pos * Lex.pos), LBRACK_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_3 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.exp_PROD_3_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_4 (strm) = let
            val (num_RES, num_SPAN, strm') = num_NT(strm)
            val FULL_SPAN = (#1(num_SPAN), #2(num_SPAN))
            in
              (UserCode.exp_PROD_4_ACT (num_RES, num_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_5 (strm) = let
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm)
            val FULL_SPAN = (#1(STRING_SPAN), #2(STRING_SPAN))
            in
              (UserCode.exp_PROD_5_ACT (STRING_RES, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_6 (strm) = let
            val (QUOTE_RES, QUOTE_SPAN, strm') = matchQUOTE(strm)
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val FULL_SPAN = (#1(QUOTE_SPAN), #2(exp_SPAN))
            in
              (UserCode.exp_PROD_6_ACT (exp_RES, QUOTE_RES, exp_SPAN : (Lex.pos * Lex.pos), QUOTE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_7 (strm) = let
            val (QUASIQUOTE_RES, QUASIQUOTE_SPAN, strm') = matchQUASIQUOTE(strm)
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val FULL_SPAN = (#1(QUASIQUOTE_SPAN), #2(exp_SPAN))
            in
              (UserCode.exp_PROD_7_ACT (exp_RES, QUASIQUOTE_RES, exp_SPAN : (Lex.pos * Lex.pos), QUASIQUOTE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_8 (strm) = let
            val (UNQUOTE_RES, UNQUOTE_SPAN, strm') = matchUNQUOTE(strm)
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val FULL_SPAN = (#1(UNQUOTE_SPAN), #2(exp_SPAN))
            in
              (UserCode.exp_PROD_8_ACT (exp_RES, UNQUOTE_RES, exp_SPAN : (Lex.pos * Lex.pos), UNQUOTE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.UNQUOTE, _, strm') => exp_PROD_8(strm)
          | (Tok.QUOTE, _, strm') => exp_PROD_6(strm)
          | (Tok.INT(_), _, strm') => exp_PROD_4(strm)
          | (Tok.FLOAT(_), _, strm') => exp_PROD_4(strm)
          | (Tok.RATIONAL(_), _, strm') => exp_PROD_4(strm)
          | (Tok.LBRACK, _, strm') => exp_PROD_2(strm)
          | (Tok.LPAREN, _, strm') => exp_PROD_1(strm)
          | (Tok.ID(_), _, strm') => exp_PROD_3(strm)
          | (Tok.STRING(_), _, strm') => exp_PROD_5(strm)
          | (Tok.QUASIQUOTE, _, strm') => exp_PROD_7(strm)
          | _ => fail()
        (* end case *))
      end
fun prog_NT (strm) = let
      val (explist_RES, explist_SPAN, strm') = explist_NT(strm)
      val FULL_SPAN = (#1(explist_SPAN), #2(explist_SPAN))
      in
        ((explist_RES), FULL_SPAN, strm')
      end
in
  (prog_NT)
end
val prog_NT =  fn s => unwrap (Err.launch (eh, lexFn, prog_NT , true) s)

in (prog_NT) end
  in
fun parse lexFn  s = let val (prog_NT) = mk lexFn in prog_NT s end

  end

end
