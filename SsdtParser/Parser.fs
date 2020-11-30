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

let tableOwner : Parser<string, unit> = 
    opt (skipChar '[') 
    >>. many1Chars letter 
    .>> opt (skipChar ']') 
    .>> pchar '.'
    |>> string

/// Parses a name segment delimited by a dot - ex: [dbo].[Orders]
let tableName : Parser<string, unit> = 
    opt (skipChar '[') 
    >>. many1Chars letter 
    .>> opt (skipChar ']') 
    |>> string

let createTable (owner, table) = 
    { Model.Table.Name = table
      Model.Table.Owner = owner |> Option.defaultValue "dbo" }

/// Parses table
let table = 
    // Ex: CREATE TABLE [dbo].[DrawingLog] (
    skipString "CREATE TABLE " 
    >>. opt tableOwner
    .>>. tableName
    .>> spaces
    .>> pchar '('
    |>> createTable