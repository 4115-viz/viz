(* Scanner for Viz language *)

{
open Parser
exception Viz_scan_error of string
}

let digit  = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf} (* whitespace *)
| "/*" { comment lexbuf } (* multi line comments *)
| "//" { comment lexbuf } (* single line comment *)
| eof  { EOF } (* end of file *)

(* reserved keywords *)
| "func" { FUNC }
| "if"   { IF }
| "else" { ELSE }
| "elif" { ELIF }
| "for" { FOR }
| "while" { WHILE }
| "infinite_loop" { INFINITE_LOOP }
| "return" { RETURN }
| "break" { BREAK }
| "continue" { CONTINUE }
| "try" { TRY }
| "catch" { CATCH }
| "raise" { RAISE }
| "link" { LINK }
| "use" { USE }
| "in" { IN }
| "step" { STEP }
| "as" { AS }

(* our data types *)
| "int" { INT }
| "-" ? digit+ as num { INT_LITERAL(int_of_string num) } (* int literal *)
| "@" letter (digit | letter | '_')* as varname { ID(varname) } (* variable name *)
| "string" { STRING }
(* | ("\"" | "\'") (digit | letter)* ("\"" | "\'") as str {STRINGLIT(str)} (* string literal *) *)
| "float" { FLOAT }
| "boolean" { BOOLEAN }
| "true" { BOOL_LITERAL(true) } (* boolean literal *)
| "false" { BOOL_LITERAL(false) } (* boolean literal *)
| "none" { NONE }

(* arithmetic operators *)
| "+" { PLUS }
| "-" { MINUS }
| "*" { MULT }
| "/" { DIV }
| "%" { MOD }

(* assignment operators *)
| "=" { ASSIGN }
| "+=" { PLUS_EQ }
| "-=" { MINUS_EQ }
| "*=" { TIMES_EQ }
| "/=" { DIV_EQ }
| "%=" { MOD_EQ }

(* relational operators *)
| "==" { EQ }
| "!=" { N_EQ }
| ">=" { GT_EQ }
| "<=" { LT_EQ }
| ">" { GT }
| "<" { LT }
| "and" { AND }
| "or" { OR }
| "not" { NOT }
| "?" { QUESTION }

(* delimiters *)
| "("  { LPAREN }
| ")"  { RPAREN }
| "[" { LBRACKET }
| "]" { RBRACKET }
| "{" {LBRACE }
| "}" { RBRACE }
| "," { COMMA }
| ":" { COLON }
| "." { DOT }
| ";"  { SEMI }

(* ADT declarations *)
| "array"      { ARRAY }
| "queue"      { QUEUE }
| "stack"      { STACK }
| "linkedNode" { LINKED_NODE }
| "treeNode"   { TREE_NODE }

| _ as char { raise (Viz_scan_error ("unexpected character: " ^ Char.escaped char)) }

and comment = parse
| "*/" { token lexbuf } (* end of multi line comment, head back to token *)
| "\n" { token lexbuf } (* this is how we will end a single line comment *)
| _    { comment lexbuf } (* want to ignore the rest of the noise *)