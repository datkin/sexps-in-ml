structure Ast = struct
  datatype num = Int of LargeInt.int (* Arbitrary precision integer *)
               (* Floating point number represented as x * 10^-y *)
               | Float of (LargeInt.int * LargeInt.int)
               (* Rational number represented as x/y *)
               | Rational of (LargeInt.int * LargeInt.int)

  datatype exp = Id of Id.id
               | Num of num
               | String of string
               | Sexp of exp list

  fun numToString (Int n) = LargeInt.toString n
    | numToString (Float (value, scale)) =
      let (* 1000, 1 *)
        fun zeroes n = String.concat (List.tabulate (n, fn _ => "0"))
        val digits = LargeInt.toString value
        val size = String.size digits
        val scale = LargeInt.toInt scale (* practically, this ought to be safe *)
        val beforeDecimal = if scale <= 0 then (* pad the end *)
                              digits ^ (zeroes (~scale))
                            else if scale < size then
                              String.substring (digits, 0, size - scale)
                            else (* everything is after *)
                              "0"
        val afterDecimal = if scale <= 0 then (* everything is before *)
                             "0"
                           else if scale < size then
                             String.substring (digits, size - scale, scale)
                           else (* pad in front *)
                             (zeroes (scale - size)) ^ digits
      in
        beforeDecimal ^ "." ^ afterDecimal
      end
    | numToString (Rational (numerator, denominator)) =
      (LargeInt.toString numerator) ^ "/" ^ (LargeInt.toString denominator)

  fun toString (Id id) = Id.name id
    | toString (Num num) = numToString num
    | toString (String str) = "\"" ^ str ^ "\""
    | toString (Sexp exps) = "(" ^ (String.concatWith " " (map toString exps)) ^ ")"
end
