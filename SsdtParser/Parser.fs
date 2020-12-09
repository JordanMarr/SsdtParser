﻿module Parser
open FParsec

let str s = pstring s
let strCI s = pstringCI s
let ws = spaces

/// Parses a name segment like [dbo] or [Orders].
let segment : Parser<string, unit> = 
    opt (skipChar '[') 
    >>. many1Chars (letter <|> pchar '_')
    .>> opt (skipChar ']') 
    .>> opt (skipChar '.')
    |>> string

let createTableHeader segments =
    match segments with
    | [owner; table] ->
        { Model.TableHeader.Name = table
          Model.TableHeader.Owner = owner }
    | [table] ->
        { Model.TableHeader.Name = table
          Model.TableHeader.Owner = "dbo" }
    | _ -> 
        failwith "Expected either '[dbo].[Table]' or '[Table]'."

let tableHeader = 
    skipString "CREATE TABLE " 
    >>. many1 segment
    .>> spaces
    .>> skipChar '('
    |>> createTableHeader

/// Implements a parser for each case in Model.DataType.
let getDataTypeParser = function 
    | Model.UniqueIdentifier as t -> stringReturn "UNIQUEIDENTIFIER" t
    | Model.Bit as t -> stringReturn "BIT" t    
    | Model.Int as t -> stringReturn "INT" t
    | Model.VarChar as t -> 
        strCI "VARCHAR" 
        >>. spaces
        >>. skipChar '(' 
        >>. skipMany (letter <|> digit)
        .>> skipChar ')'
        >>% t
    | Model.DateTimeOffset as t -> // must be before date
        strCI "DATETIMEOFFSET" 
        >>. spaces
        >>. skipChar '(' 
        >>. skipMany digit
        .>> skipChar ')'
        >>% t
    | Model.Date as t -> stringReturn "DATE" t

open Microsoft.FSharp.Reflection
// Will crash if DU contains members which aren't only tags
let getNameAndInstance (caseInfo: UnionCaseInfo) = caseInfo.Name, FSharpValue.MakeUnion(caseInfo, [||]) :?> Model.DataType

/// All supported col data types listed by descending length
let dataTypes = 
    FSharpType.GetUnionCases typeof<Model.DataType>
    |> Array.map getNameAndInstance
    |> Array.sortByDescending (fun (name, _) -> name.Length, name) // Longest names first to avoid partial consumption bugs
    |> Array.map snd
    |> Array.map getDataTypeParser
    
let colDefault = 
    skipString "DEFAULT"
    >>. spaces
    >>. manyTill anyChar (pchar ',')

let colConstraintType =
    choice [ 
        skipString "PRIMARY KEY CLUSTERED"
        skipString "FOREIGN KEY" 
        skipString "UNIQUE" 
    ]

let colConstraintReference = 
    skipString "REFERENCES" 
    >>. spaces 
    >>. many1 segment 
    >>. spaces 
    >>. between (opt (pchar '(')) (opt (pchar ')')) segment

let colConstraint =
    skipString "CONSTRAINT"
    >>. spaces
    >>. segment
    .>> spaces
    .>> colConstraintType
    .>> spaces
    .>> between (opt (pchar '(')) (opt (pchar ')')) segment
    .>> opt colConstraintReference
    |>> (fun s -> { Model.Constraint.Name = s })
    
let createColumn ((name, dataType), allowNulls) =
    { Model.Column.Name = name
      Model.Column.DataType = dataType
      Model.Column.AllowNulls = allowNulls
      Model.Column.Default = None }

let column = 
    spaces 
    >>. segment
    .>> spaces
    .>>. choice dataTypes
    .>> spaces
    .>>. ((stringReturn "NULL" true) <|> (stringReturn "NOT NULL" false))
    .>> spaces
    .>> opt (spaces >>. colDefault >>. spaces)
    |>> createColumn

let createTable ((tableHeader: Model.TableHeader, columns), constraints) =
    { Model.Table.Name = tableHeader.Name
      Model.Table.Owner = tableHeader.Owner
      Model.Table.Columns = columns
      Model.Table.Constraints = constraints }

/// Parses a create table script with columns
let table = 
    let colWs = (spaces >>. column .>> spaces)
    let constraintWs = (spaces >>. colConstraint .>> spaces)
    let commaWs = (spaces >>. skipChar ',' >>. spaces)

    spaces
    >>. tableHeader 
    .>>. sepBy1 colWs commaWs
    .>>. sepBy constraintWs commaWs
    .>> opt (skipString ");")
    |>> createTable
