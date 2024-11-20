//
// Parser for SimpleC programs.  This component checks 
// the input program to see if it meets the syntax rules
// of SimpleC.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid SimpleC program.
//
// Name: Ethan Wong
// CS 341, Fall 2024
// System: Arch GNU/Linux (dotnet 8.0.108) using nvim
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // matchToken
  //
  let private matchToken expected_token tokens =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens

    if expected_token = next_token then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)

  //
  // check identifier
  //
  let private checkIdentifier itype token =
    let beginsWith (pattern: string) (literal: string) =
      literal.StartsWith (pattern)
    beginsWith (itype + ":") token

  let private (|CheckIdentifier|_|) itype token =
    if checkIdentifier itype token then Some() else None

  //
  // match names/identifiers
  //
  let private matchIdentifier itype tokens =
    let next_token = List.head tokens

    if (checkIdentifier itype next_token) then  
      List.tail tokens
    else
      failwith ("expecting " + itype + ", but found " + next_token)

  //
  // empty statement/semicolon
  //
  let private empty tokens =
    matchToken ";" tokens

  //
  // vardecl statement
  //
  let private vardecl tokens =
    tokens
    |> matchToken "int"
    |> matchIdentifier "identifier"
    |> empty

  //
  // cin input statement
  //
  let private input tokens =
    tokens
    |> matchToken "cin"
    |> matchToken ">>"
    |> matchIdentifier "identifier"
    |> empty

  //
  // expression value
  //
  let private expr_value tokens =
    let next_token = List.head tokens
    match next_token with
    | CheckIdentifier "identifier" -> matchIdentifier "identifier" tokens
    | CheckIdentifier "int_literal" -> matchIdentifier "int_literal" tokens
    | CheckIdentifier "str_literal" -> matchIdentifier "str_literal" tokens
    | "true"
    | "false" -> matchToken next_token tokens
    | _ -> failwith("expecting identifier or literal, but found " + next_token)

  //
  // expression op
  //
  let private expr_op tokens =
    let next_token = List.head tokens
    match next_token with
    | "+"
    | "-"
    | "*"
    | "/"
    | "^"
    | "<"
    | "<="
    | ">"
    | ">="
    | "=="
    | "!=" -> matchToken next_token tokens
    | _ -> failwith("expecting expr-op but found " + next_token)

  //
  // combination expression
  //
  let private expr tokens =
    let tail = expr_value tokens
    let next_token = List.head tail
    match next_token with
    | "+"
    | "-"
    | "*"
    | "/"
    | "^"
    | "<"
    | "<="
    | ">"
    | ">="
    | "=="
    | "!=" -> expr_op tail |> expr_value
    | _ -> tail

  //
  // output value
  //
  let private output_value tokens =
    let next_token = List.head tokens
    match next_token with
    | CheckIdentifier "identifier"
    | CheckIdentifier "int_literal"
    | CheckIdentifier "str_literal"
    | "true"
    | "false" -> expr_value tokens
    | "endl" -> matchToken next_token tokens
    | _ -> failwith("expecting identifier or literal, but found " + next_token)

  //
  // cout output statement
  //
  let private output tokens =
    tokens
    |> matchToken "cout"
    |> matchToken "<<"
    |> output_value
    |> empty

  //
  // assignment statement
  //
  let private assignment tokens =
    tokens
    |> matchIdentifier "identifier"
    |> matchToken "="
    |> expr
    |> empty

  //
  // condition
  //
  let private condition tokens =
    expr tokens

  //
  // single statement
  //
  let rec private stmt tokens =
    let next_token = List.head tokens
    match next_token with
    | ";" -> empty tokens
    | "int" -> vardecl tokens
    | "cin" -> input tokens
    | "cout" -> output tokens
    | "if" -> ifstmt tokens
    | CheckIdentifier "identifier" -> assignment tokens
    | _ -> failwith("expecting statement, but found " + next_token)

  //
  // if statement
  //
  and private ifstmt tokens =
    tokens
    |> matchToken "if"
    |> matchToken "("
    |> condition
    |> matchToken ")"
    |> then_part
    |> else_part

  //
  // then part of an if statement
  //
  and private then_part tokens =
    stmt tokens

  //
  // else part of an if statement
  //
  and private else_part tokens =
    let next_token = List.head tokens
    match next_token with
    | "else" -> matchToken next_token tokens |> stmt
    | _ -> tokens

  //
  // multiple statements
  //
  let rec private stmts tokens =
    tokens
    |> stmt
    |> morestmts

  and private morestmts tokens =
    let next_token = List.head tokens
    match next_token with
    | ";"
    | "int"
    | "cin"
    | "cout"
    | "if"
    | CheckIdentifier "identifier" -> stmt tokens |> morestmts
    | _ -> tokens
    
  //
  // simpleC
  //
  let private simpleC tokens = 
    tokens
    |> matchToken "void"
    |> matchToken "main"
    |> matchToken "("
    |> matchToken ")"
    |> matchToken "{"
    |> stmts
    |> matchToken "}"
    |> matchToken "$"

  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid SimpleC program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "Success!"
    with 
      | ex -> "syntax_error: " + ex.Message
