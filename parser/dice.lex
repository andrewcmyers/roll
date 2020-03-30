(* Required for parser *)
structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

structure Keywords =
    struct
        type pos = int
        type token = (svalue, pos) Tokens.token

        fun ident (a,p1,p2) =
          Tokens.ID (Atom.toString (a),p1,p2)

        fun mkKW (kw,tk) = (kw, fn (p1:pos,p2:pos) => tk (p1,p2))

        val keywords = map mkKW
          [("d", Tokens.KW_d),
           ("sum", Tokens.KW_Sum),
           ("count", Tokens.KW_Count),
           ("min", Tokens.KW_Least),
           ("max", Tokens.KW_Greatest),
           ("let", Tokens.KW_let),
           ("in", Tokens.KW_in),
           ("end", Tokens.KW_end),
           ("compare", Tokens.KW_compare),
           ("if", Tokens.KW_if),
           ("then", Tokens.KW_then),
           ("else", Tokens.KW_else),
           ("filter", Tokens.KW_Filter)]
    end

structure K = KeywordFn (Keywords)

val pos = ref 0
val eof = fn () => Tokens.EOF (!pos, !pos)
val error = fn (e, l: int, _) =>
              print ("line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")

%%
%header (functor DiceLexFun (structure Tokens : Dice_TOKENS));

NL = (\n | \013\n | \013);
WS = (" " | \t | \f);
ID = [a-zA-Z][a-zA-Z_']*;
INT = [0-9][0-9]*;
%%

{NL} => (pos := !pos + 1; lex ());
{WS} => (lex ());
"(" => (Tokens.LPAREN (!pos, !pos));
")" => (Tokens.RPAREN (!pos, !pos));
"{" => (Tokens.LBRACE (!pos, !pos));
"}" => (Tokens.RBRACE (!pos, !pos));
"," => (Tokens.COMMA (!pos, !pos));
"++" => (Tokens.AT (!pos, !pos));
"+" => (Tokens.PLUS (!pos, !pos));
"-" => (Tokens.MINUS (!pos, !pos));
"*" => (Tokens.TIMES (!pos, !pos));
"/" => (Tokens.DIV (!pos, !pos));
"%" => (Tokens.MOD (!pos, !pos));
"#" => (Tokens.HASH (!pos, !pos));
"!=" => (Tokens.NOTEQUAL (!pos, !pos));
"<>" => (Tokens.NOTEQUAL (!pos, !pos));
"<=" => (Tokens.LESSEQUAL (!pos, !pos));
">=" => (Tokens.GREATEREQUAL (!pos, !pos));
"=" => (Tokens.EQUAL (!pos, !pos));
"<" => (Tokens.LESS (!pos, !pos));
">" => (Tokens.GREATER (!pos, !pos));
{ID} => (K.keyword (yytext, !pos, !pos));
{INT} => (Tokens.INT (valOf (Int.fromString yytext), !pos, !pos));
.  => (error ("ignoring bad character "^yytext, !pos, !pos);
       lex());
