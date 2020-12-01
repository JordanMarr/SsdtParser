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


module DataTypes =
    let uniqueIdentifier : Parser<Model.DataType, unit> = stringReturn "UNIQUEIDENTIFIER" Model.UniqueIdentifier
    let bit : Parser<Model.DataType, unit> = stringReturn "BIT" Model.Bit
    let date : Parser<Model.DataType, unit> = stringReturn "DATE" Model.Date
    let varChar : Parser<Model.DataType, unit> = 
        strCI "VARCHAR" 
        >>. spaces
        >>. skipChar '(' 
        >>. many (letter <|> digit)
        .>> skipChar ')'
        |>> fun _ -> Model.VarChar

let createColumn ((name, dataType), allowNulls) =
    { Model.Column.Name = name
      Model.Column.DataType = dataType
      Model.Column.AllowNulls = allowNulls
      Model.Column.Default = None }

/// Parses a column definition
let column = 
    spaces 
    >>. segment
    .>> spaces
    .>>. choice 
        [ DataTypes.uniqueIdentifier
          DataTypes.bit
          DataTypes.varChar
          DataTypes.date ]
    .>> spaces
    .>>. ((stringReturn "NULL" true) <|> (stringReturn "NOT NULL" false))
    .>> spaces
    .>> opt (pchar ',')
    |>> createColumn