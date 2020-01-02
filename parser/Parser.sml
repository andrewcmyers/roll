structure Parser = struct
  structure DiceLrVals = DiceLrValsFun (structure Token = LrParser.Token)
  structure DiceLex = DiceLexFun (structure Tokens = DiceLrVals.Tokens)
  structure DiceParser = Join (structure ParserData = DiceLrVals.ParserData
                               structure Lex = DiceLex
                               structure LrParser = LrParser)

  exception ParseError

  fun error (s) =
    (print ("PARSE ERROR: " ^ s ^ "\n"); raise ParseError)

  fun invoke (lexstream) = let
    val print_error = fn (s,i:int,_) =>
      error (concat [s," [line ",(Int.toString i),"]"])
  in
    DiceParser.parse(0,lexstream,print_error,())
  end

  fun parse (instream) = let
    val lexer = DiceParser.makeLexer (fn _ => case TextIO.inputLine instream of SOME s => s | NONE => "")
    val dummyEOF = DiceLrVals.Tokens.EOF(0,0)
    fun loop lexer = let
      val (result,lexer) = invoke lexer
      val (nextToken,lexer) = DiceParser.Stream.get lexer
    in if DiceParser.sameToken(nextToken,dummyEOF) then result
       else loop lexer
    end
  in
    DiceLex.UserDeclarations.pos := 1;
    (SOME (loop lexer) handle _ => NONE)
  end

  fun parseString (string) = let
    val s = TextIO.openString (string)
    val r = parse (s)
  in
    TextIO.closeIn (s);
    case r of
      SOME e => e
    | NONE => raise ParseError
  end

end
