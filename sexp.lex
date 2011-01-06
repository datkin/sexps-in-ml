%name Lexer;

%let digit = [0-9];

(* ASCII code points are given as three digits base ten, prefixed with a backslash. *)

(* Unreserved, printable ascii characters. Identifiers must begin with one of these characters. *)
%let ascii = [\033\036-\038\042-\043\045-\058\060-\090\092\094\095\097-\126];

(* Reserved characters that may be used in identifiers, following the first character:
 * double quote: \034
 * single quote: \039
 * comma: \044
 * backtick: \096 *)
%let reserved = [\034\039\044\096]

(* Legal string characters: all printable ascii characters, except double quote and newline. *)
%let string = [\032\033\035-\126]

%defs (
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

);

%states COMMENT_S STRING_S;

<INITIAL> "(" => (LPAREN);
<INITIAL> ")" => (RPAREN);
<INITIAL> "[" => (LBRACK);
<INITIAL> "]" => (RBRACK);
<INITIAL> "'" => (QUOTE);
<INITIAL> "`" => (QUASIQUOTE);
<INITIAL> "," => (UNQUOTE);

<INITIAL> "#;" => (COMMENT (* This style comment is handled in the parser *));

<INITIAL> ";" => (YYBEGIN COMMENT_S; continue ());
<COMMENT_S> . => (continue ());
<COMMENT_S> "\n" => (YYBEGIN INITIAL; continue ());

<INITIAL> "\"" => (YYBEGIN STRING_S; continue ());
<STRING_S> {string}* => (currentString := !currentString ^ yytext; continue ());
<STRING_S> "\"" => (YYBEGIN INITIAL;
                    let val str = STRING (!currentString) in
                      currentString := ""; str
                    end);

<INITIAL> "-"? {digit}+ => (INT (toInteger yytext));
<INITIAL> "-"? {digit}+ "." {digit}+ => (FLOAT (toFloat yytext));
<INITIAL> "-"? {digit}+ "/" {digit}+ => (RATIONAL (toRational yytext));
<INITIAL> {ascii}({ascii}|{reserved})* => (ID yytext);
<INITIAL> [\n\t ]+ => (skip ());
<INITIAL>.   => (continue () (* Error *));
