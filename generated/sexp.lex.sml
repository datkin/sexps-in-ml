structure Lexer  = struct

    datatype yystart_state = 
COMMENT_S | STRING_S | INITIAL
    structure UserDeclarations = 
      struct

 
  open SexpTokens;
  type lex_result = token;
  fun eof () = EOF;

  val currentString = ref "";

  fun toInteger (str) = valOf (LargeInt.fromString (str))

  fun toFloat (str) =
      case String.tokens (fn (chr) => chr = #".") str of
        [integer, fraction] =>
        let
          val digitStr = integer ^ fraction
          val exponent = LargeInt.fromInt (String.size fraction)
        in
          (toInteger (digitStr), exponent)
        end
      | _ => raise Fail ("Couldn't create float from " ^ str)

  fun toRational (str) =
      case String.tokens (fn (chr) => chr = #"/") str of
        [numerStr, denomStr] =>
        let
          val numerator = toInteger (numerStr)
          val denominator = toInteger (denomStr)
        in
          (numerator, denominator)
        end
      | _ => raise Fail ("Couldn't create rational from " ^ str)



      end

    local
    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    val yytable : ((UTF8.wchar * UTF8.wchar * int) list * int list) Vector.vector = 
Vector.fromList []
    fun yystreamify' p input = ULexBuffer.mkStream (p, input)

    fun yystreamifyReader' p readFn strm = let
          val s = ref strm
	  fun iter(strm, n, accum) = 
	        if n > 1024 then (String.implode (rev accum), strm)
		else (case readFn strm
		       of NONE => (String.implode (rev accum), strm)
			| SOME(c, strm') => iter (strm', n+1, c::accum))
          fun input() = let
	        val (data, strm) = iter(!s, 0, [])
	        in
	          s := strm;
		  data
	        end
          in
            yystreamify' p input
          end

    fun yystreamifyInstream' p strm = yystreamify' p (fn ()=>TextIO.input strm)

    fun innerLex 
(yystrm_, yyss_, yysm) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	  fun yysetStrm strm = yystrm := strm
	  fun yygetPos() = ULexBuffer.getpos (!yystrm)
	  fun yystreamify input = yystreamify' (yygetPos()) input
	  fun yystreamifyReader readFn strm = yystreamifyReader' (yygetPos()) readFn strm
	  fun yystreamifyInstream strm = yystreamifyInstream' (yygetPos()) strm
        (* start position of token -- can be updated via skip() *)
	  val yystartPos = ref (yygetPos())
	(* get one char of input *)
	  fun yygetc strm = (case UTF8.getu ULexBuffer.getc strm
                of (SOME (0w10, s')) => 
		     (AntlrStreamPos.markNewLine yysm (ULexBuffer.getpos strm);
		      SOME (0w10, s'))
		 | x => x)
          fun yygetList getc strm = let
            val get1 = UTF8.getu getc
            fun iter (strm, accum) = 
	        (case get1 strm
	          of NONE => rev accum
	           | SOME (w, strm') => iter (strm', w::accum)
	         (* end case *))
          in
            iter (strm, [])
          end
	(* create yytext *)
	  fun yymksubstr(strm) = ULexBuffer.subtract (strm, !yystrm)
	  fun yymktext(strm) = Substring.string (yymksubstr strm)
	  fun yymkunicode(strm) = yygetList Substring.getc (yymksubstr strm)
          open UserDeclarations
          fun lex () = let
            fun yystuck (yyNO_MATCH) = raise Fail "lexer reached a stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yygetPos()
	    fun yygetlineNo strm = AntlrStreamPos.lineNo yysm (ULexBuffer.getpos strm)
	    fun yygetcolNo  strm = AntlrStreamPos.colNo  yysm (ULexBuffer.getpos strm)
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    val yylastwasnref = ref (ULexBuffer.lastWasNL (!yystrm))
	    fun continue() = let val yylastwasn = !yylastwasnref in
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;  LPAREN)
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;  RPAREN)
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;  LBRACK)
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;  RBRACK)
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;  QUOTE)
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;  QUASIQUOTE)
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;  UNQUOTE)
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
       COMMENT (* This style comment is handled in the parser *))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN COMMENT_S; continue ())
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;  continue ())
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; continue ())
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN STRING_S; continue ())
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  currentString := !currentString ^ yytext; continue ()
      end
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL;
                    let val str = STRING (!currentString) in
                      currentString := ""; str
                    end)
fun yyAction14 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  INT (toInteger yytext)
      end
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  FLOAT (toFloat yytext)
      end
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  RATIONAL (toRational yytext)
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  ID yytext
      end
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;  skip ())
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
       continue () (* Error *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyAction17(strm, yyNO_MATCH)
            else if inp < 0wx3B
              then if inp = 0wx24
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx24
                  then if inp = 0wx21
                      then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                    else if inp < 0wx21
                      then yyAction17(strm, yyNO_MATCH)
                    else if inp = 0wx23
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx28
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < 0wx28
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp <= 0wx29
                  then yyAction17(strm, yyNO_MATCH)
                  else yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx5D
              then yyAction17(strm, yyNO_MATCH)
            else if inp < 0wx5D
              then if inp = 0wx5B
                  then yyAction17(strm, yyNO_MATCH)
                  else yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp <= 0wx7E
              then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyQ22(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp < 0wx3A
              then if inp = 0wx24
                  then yyQ22(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                else if inp < 0wx24
                  then if inp = 0wx21
                      then yyQ22(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                    else if inp < 0wx21
                      then yyAction16(strm, yyNO_MATCH)
                    else if inp = 0wx23
                      then yyAction16(strm, yyNO_MATCH)
                      else yyQ22(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                else if inp = 0wx2A
                  then yyQ22(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                else if inp < 0wx2A
                  then if inp <= 0wx27
                      then yyQ22(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                      else yyAction16(strm, yyNO_MATCH)
                else if inp <= 0wx2F
                  then yyQ22(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyQ26(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp = 0wx5C
              then yyQ22(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp < 0wx5C
              then if inp = 0wx3C
                  then yyQ22(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                else if inp < 0wx3C
                  then yyAction16(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction16(strm, yyNO_MATCH)
                  else yyQ22(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp = 0wx5E
              then yyQ22(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp < 0wx5E
              then yyAction16(strm, yyNO_MATCH)
            else if inp <= 0wx7E
              then yyQ22(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx3A
              then if inp = 0wx24
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx24
                  then if inp = 0wx21
                      then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                    else if inp < 0wx21
                      then yyAction17(strm, yyNO_MATCH)
                    else if inp = 0wx23
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx2A
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx2A
                  then if inp <= 0wx27
                      then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                else if inp <= 0wx2F
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ26(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx5C
              then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx5C
              then if inp = 0wx3C
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx3C
                  then yyAction17(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction17(strm, yyNO_MATCH)
                  else yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx5E
              then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx5E
              then yyAction17(strm, yyNO_MATCH)
            else if inp <= 0wx7E
              then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < 0wx3A
              then if inp = 0wx24
                  then yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp < 0wx24
                  then if inp = 0wx21
                      then yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                    else if inp < 0wx21
                      then yyAction15(strm, yyNO_MATCH)
                    else if inp = 0wx23
                      then yyAction15(strm, yyNO_MATCH)
                      else yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp = 0wx2A
                  then yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp < 0wx2A
                  then if inp <= 0wx27
                      then yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                      else yyAction15(strm, yyNO_MATCH)
                else if inp <= 0wx2F
                  then yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyQ27(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp = 0wx5C
              then yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < 0wx5C
              then if inp = 0wx3C
                  then yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp < 0wx3C
                  then yyAction15(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction15(strm, yyNO_MATCH)
                  else yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp = 0wx5E
              then yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < 0wx5E
              then yyAction15(strm, yyNO_MATCH)
            else if inp <= 0wx7E
              then yyQ22(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx3A
              then if inp = 0wx24
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx24
                  then if inp = 0wx21
                      then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                    else if inp < 0wx21
                      then yyAction17(strm, yyNO_MATCH)
                    else if inp = 0wx23
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx2A
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx2A
                  then if inp <= 0wx27
                      then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                else if inp <= 0wx2F
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ27(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx5C
              then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx5C
              then if inp = 0wx3C
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx3C
                  then yyAction17(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction17(strm, yyNO_MATCH)
                  else yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx5E
              then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx5E
              then yyAction17(strm, yyNO_MATCH)
            else if inp <= 0wx7E
              then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ25(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp < 0wx30
              then if inp = 0wx28
                  then yyAction14(strm, yyNO_MATCH)
                else if inp < 0wx28
                  then if inp = 0wx23
                      then yyAction14(strm, yyNO_MATCH)
                    else if inp < 0wx23
                      then if inp <= 0wx20
                          then yyAction14(strm, yyNO_MATCH)
                          else yyQ22(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                      else yyQ22(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                else if inp = 0wx2E
                  then yyQ23(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp <= 0wx29
                      then yyAction14(strm, yyNO_MATCH)
                      else yyQ22(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyQ24(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp = 0wx5B
              then yyAction14(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3B
                  then yyAction14(strm, yyNO_MATCH)
                else if inp < 0wx3B
                  then if inp = 0wx3A
                      then yyQ22(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                      else yyQ25(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyQ22(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp = 0wx5E
              then yyQ22(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp < 0wx5E
              then if inp = 0wx5C
                  then yyQ22(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyAction14(strm, yyNO_MATCH)
            else if inp <= 0wx7E
              then yyQ22(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ25(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp < 0wx30
              then if inp = 0wx28
                  then yyAction14(strm, yyNO_MATCH)
                else if inp < 0wx28
                  then if inp = 0wx23
                      then yyAction14(strm, yyNO_MATCH)
                    else if inp < 0wx23
                      then if inp <= 0wx20
                          then yyAction14(strm, yyNO_MATCH)
                          else yyQ22(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                      else yyQ22(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                else if inp = 0wx2E
                  then yyQ23(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                else if inp < 0wx2E
                  then if inp <= 0wx29
                      then yyAction14(strm, yyNO_MATCH)
                      else yyQ22(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyQ24(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp = 0wx5B
              then yyAction14(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3B
                  then yyAction14(strm, yyNO_MATCH)
                else if inp < 0wx3B
                  then if inp = 0wx3A
                      then yyQ22(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                      else yyQ25(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyQ22(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp = 0wx5E
              then yyQ22(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp < 0wx5E
              then if inp = 0wx5C
                  then yyQ22(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyAction14(strm, yyNO_MATCH)
            else if inp <= 0wx7E
              then yyQ22(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx3A
              then if inp = 0wx24
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx24
                  then if inp = 0wx21
                      then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                    else if inp < 0wx21
                      then yyAction17(strm, yyNO_MATCH)
                    else if inp = 0wx23
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx2A
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx2A
                  then if inp <= 0wx27
                      then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                else if inp <= 0wx2F
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyQ25(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx5C
              then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx5C
              then if inp = 0wx3C
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx3C
                  then yyAction17(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction17(strm, yyNO_MATCH)
                  else yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx5E
              then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx5E
              then yyAction17(strm, yyNO_MATCH)
            else if inp <= 0wx7E
              then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyQ28(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyAction17(strm, yyNO_MATCH)
            else if inp < 0wx3B
              then if inp = 0wx24
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < 0wx24
                  then if inp = 0wx21
                      then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                    else if inp < 0wx21
                      then yyAction17(strm, yyNO_MATCH)
                    else if inp = 0wx23
                      then yyAction17(strm, yyNO_MATCH)
                      else yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = 0wx28
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < 0wx28
                  then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp <= 0wx29
                  then yyAction17(strm, yyNO_MATCH)
                  else yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp = 0wx5D
              then yyAction17(strm, yyNO_MATCH)
            else if inp < 0wx5D
              then if inp = 0wx5B
                  then yyAction17(strm, yyNO_MATCH)
                  else yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp <= 0wx7E
              then yyQ22(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxB
              then yyAction18(strm, yyNO_MATCH)
            else if inp < 0wxB
              then if inp <= 0wx8
                  then yyAction18(strm, yyNO_MATCH)
                  else yyQ29(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ29(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxB
              then yyAction18(strm, yyNO_MATCH)
            else if inp < 0wxB
              then if inp <= 0wx8
                  then yyAction18(strm, yyNO_MATCH)
                  else yyQ29(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ29(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2D
              then yyQ16(strm', lastMatch)
            else if inp < 0wx2D
              then if inp = 0wx23
                  then yyQ11(strm', lastMatch)
                else if inp < 0wx23
                  then if inp = 0wx20
                      then yyQ8(strm', lastMatch)
                    else if inp < 0wx20
                      then if inp = 0wx9
                          then yyQ8(strm', lastMatch)
                        else if inp < 0wx9
                          then yyQ7(strm', lastMatch)
                        else if inp <= 0wxA
                          then yyQ8(strm', lastMatch)
                          else yyQ7(strm', lastMatch)
                    else if inp = 0wx21
                      then yyQ9(strm', lastMatch)
                      else yyQ10(strm', lastMatch)
                else if inp = 0wx29
                  then yyQ14(strm', lastMatch)
                else if inp < 0wx29
                  then if inp = 0wx27
                      then yyQ12(strm', lastMatch)
                    else if inp = 0wx28
                      then yyQ13(strm', lastMatch)
                      else yyQ9(strm', lastMatch)
                else if inp = 0wx2C
                  then yyQ15(strm', lastMatch)
                  else yyQ9(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ9(strm', lastMatch)
            else if inp < 0wx5C
              then if inp = 0wx3B
                  then yyQ18(strm', lastMatch)
                else if inp < 0wx3B
                  then if inp = 0wx30
                      then yyQ17(strm', lastMatch)
                    else if inp < 0wx30
                      then yyQ9(strm', lastMatch)
                    else if inp = 0wx3A
                      then yyQ9(strm', lastMatch)
                      else yyQ17(strm', lastMatch)
                else if inp = 0wx5B
                  then yyQ19(strm', lastMatch)
                  else yyQ9(strm', lastMatch)
            else if inp = 0wx60
              then yyQ21(strm', lastMatch)
            else if inp < 0wx60
              then if inp = 0wx5D
                  then yyQ20(strm', lastMatch)
                  else yyQ9(strm', lastMatch)
            else if inp <= 0wx7E
              then yyQ9(strm', lastMatch)
              else yyQ7(strm', lastMatch)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx22
              then yyAction12(strm, yyNO_MATCH)
            else if inp < 0wx22
              then if inp <= 0wx1F
                  then yyAction12(strm, yyNO_MATCH)
                  else yyQ5(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp <= 0wx7E
              then yyQ5(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx22
              then yyQ6(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < 0wx22
              then if inp <= 0wx1F
                  then if ULexBuffer.eof(!(yystrm))
                      then let
                        val yycolno = ref(yygetcolNo(!(yystrm)))
                        val yylineno = ref(yygetlineNo(!(yystrm)))
                        in
                          (case (!(yyss))
                           of _ => (UserDeclarations.eof())
                          (* end case *))
                        end
                      else yyAction12(strm, yyNO_MATCH)
                  else yyQ5(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp <= 0wx7E
              then yyQ5(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ4(strm', lastMatch)
              else yyQ3(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of COMMENT_S => yyQ0(!(yystrm), yyNO_MATCH)
    | STRING_S => yyQ1(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ2(!(yystrm), yyNO_MATCH)
  (* end case *))
end
end
            and skip() = (yystartPos := yygetPos(); 
			  yylastwasnref := ULexBuffer.lastWasNL (!yystrm);
			  continue())
	    in (continue(), (!yystartPos, yygetPos()), !yystrm, !yyss) end
          in 
            lex()
          end
  in
    type pos = AntlrStreamPos.pos
    type span = AntlrStreamPos.span
    type tok = UserDeclarations.lex_result

    datatype prestrm = STRM of ULexBuffer.stream * 
		(yystart_state * tok * span * prestrm * yystart_state) option ref
    type strm = (prestrm * yystart_state)

    fun lex sm 
(STRM (yystrm, memo), ss) = (case !memo
	  of NONE => let
	     val (tok, span, yystrm', ss') = innerLex 
(yystrm, ss, sm)
	     val strm' = STRM (yystrm', ref NONE);
	     in 
	       memo := SOME (ss, tok, span, strm', ss');
	       (tok, span, (strm', ss'))
	     end
	   | SOME (ss', tok, span, strm', ss'') => 
	       if ss = ss' then
		 (tok, span, (strm', ss''))
	       else (
		 memo := NONE;
		 lex sm 
(STRM (yystrm, memo), ss))
         (* end case *))

    fun streamify input = (STRM (yystreamify' 0 input, ref NONE), INITIAL)
    fun streamifyReader readFn strm = (STRM (yystreamifyReader' 0 readFn strm, ref NONE), 
				       INITIAL)
    fun streamifyInstream strm = (STRM (yystreamifyInstream' 0 strm, ref NONE), 
				  INITIAL)

    fun getPos (STRM (strm, _), _) = ULexBuffer.getpos strm

  end
end
