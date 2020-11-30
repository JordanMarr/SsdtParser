module Parser
open FParsec

/// SSDT AST
type AST = 
    | Table of Model.Table
    | Column of Model.Column
    | Constraint of Model.Constraint

let str s = pstring s
let strCI s = pstringCI s
let ws = spaces

/// Parses a name segment like [dbo] or [Orders].
let segment : Parser<string, unit> = 
    opt (skipChar '[') 
    >>. many1Chars letter 
    .>> opt (skipChar ']') 
    .>> opt (pchar '.')
    |>> string


let createTable segments =
    match segments with
    | [owner; table] ->
        { Model.Table.Name = table
          Model.Table.Owner = owner }
    | [table] ->
        { Model.Table.Name = table
          Model.Table.Owner = "dbo" }
    | _ -> 
        failwith "Expected either '[dbo].[Table]' or '[Table]'."

/// Parses table
let table = 
    skipString "CREATE TABLE " 
    >>. many segment
    .>> spaces
    .>> pchar '('
    |>> createTable