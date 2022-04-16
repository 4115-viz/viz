open Parser

(* this code will be called with dune -ts flag, for ensuring we scan correct program *)
let string_of_token (t : token) : string  =
  match t with
    | ID_VAR(x)  -> String.concat "" ["ID_VAR("; x ; ")"]
    | ID_FUNC(x)  -> String.concat "" ["ID_FUNC("; x ; ")"]
    | LIT_BOOL(x) -> String.concat "" ["LIT_BOOL("; Bool.to_string x ; ")"]
    | LIT_STR(x) -> String.concat "" ["LIT_STR("; x ; ")"]
    | LIT_INT(x) -> String.concat "" ["LIT_INT("; Int.to_string x ; ")"]
    | LIT_FLOAT(x) -> String.concat "" ["LIT_FLOAT("; Float.to_string x ; ")"]
    | FUNC   -> "FUNC"
    | COLON  -> "COLON"
    | T_NONE -> "T_NONE"
    | T_BOOL -> "T_BOOL"
    | T_STR  -> "T_STR"
    | T_INT  -> "T_INT"
    | T_FLOAT  -> "T_FLOAT"
    | T_ARRAY -> "T_ARRAY"
    | LBRACE -> "LBRACE"
    | RBRACE -> "RBRACE"
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | LBRACKET -> "LBRACKET"
    | RBRACKET -> "RBRACKET"
    | DOT  -> "DOT"
    | SEMI   -> "SEMI"
    | PLUS   -> "PLUS"
    | MINUS  -> "MINUS"
    | DIVIDE -> "DIVIDE"
    | MOD -> "MOD"
    | TIMES  -> "TIMES"
    | ASSIGN -> "ASSIGN"
    | COMMA  -> "COMMA"
    | IF -> "IF"
    | ELSE -> "ELSE"
    | ELIF -> "ELIF"
    | FOR -> "FOR"
    | WHILE -> "WHILE"
    | INFINITE_LOOP -> "INFINITE_LOOP"
    | RETURN -> "RETURN"
    | BREAK -> "BREAK"
    | CONTINUE -> "CONTINUE"
    | TRY -> "TRY"
    | CATCH -> "CATCH"
    | RAISE -> "RAISE" (* i guess we will also need exception right? *)
    | LINK -> "LINK"
    | USE -> "USE"
    | IN -> "IN"
    | STEP -> "STEP"
    | AS -> "AS"
    | EQ -> "EQ"
    | NEQ -> "NEQ"
    | GTEQ -> "GTEQ"
    | LTEQ -> "LTEQ"
    | GT -> "GT"
    | LT -> "LT"
    | AND -> "AND"
    | OR -> "OR"
    | NOT -> "NOT"
    | PLUSEQ -> "PLUSEQ"
    | MINUSEQ -> "MINUSEQ"
    | TIMESEQ -> "TIMESEQ"
    | DIVEQ -> "DIVEQ"
    | MODEQ -> "MODEQ"
    | QUESTION -> "QUESTION"
    | RANGE -> "RANGE"    
    | BAR -> "BAR"  
    | _ -> "UNABLE TO FORMAT THIS TOKEN"

(* lexm.sh via CTeX group project 4115*)
let string_of_lexbuf lexbuf =
  let rec next l =
    match Scanner.token lexbuf with
      | EOF -> l
      | x -> next (x :: l)
  in
  let token_list = List.rev (next []) in
  String.concat "\n" (List.map string_of_token token_list)