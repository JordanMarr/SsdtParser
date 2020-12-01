module Parser
open FParsec

/// SSDT AST
type AST = 
    | Table of Model.TableHeader
    | Column of Model.Column
    | Constraint of Model.Constraint

let str s = pstring s
let strCI s = pstringCI s
let ws = spaces

//let segmentWithBrackets : Parser<string, unit> =
//    skipChar '['
//    >>. many1CharsTill (letter <|> digit <|> pchar ' ') (pchar ']')
//    .>> opt (skipChar '.')
//    |>> string

//let segmentNoBrackets : Parser<string, unit> =
//    manyChars (letter <|> digit)
//    .>> opt (skipChar '.')

//let segment : Parser<string, unit> = 
//    segmentWithBrackets <|> segmentNoBrackets

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

/// Parses table
let tableHeader = 
    skipString "CREATE TABLE " 
    >>. many1 segment
    .>> spaces
    .>> skipChar '('
    |>> createTableHeader

module DataTypes =
    let uniqueIdentifier = stringReturn "UNIQUEIDENTIFIER" Model.UniqueIdentifier
    let bit = stringReturn "BIT" Model.Bit
    let date = stringReturn "DATE" Model.Date
    let int = stringReturn "INT" Model.Int
    let varChar = 
        strCI "VARCHAR" 
        >>. spaces
        >>. skipChar '(' 
        >>. skipMany (letter <|> digit)
        .>> skipChar ')'
        >>% Model.VarChar

let colDefault = 
    skipString "DEFAULT"
    >>. spaces
    >>. manyTill anyChar (pchar ',')

let colConstraintType =
    choice [ skipString "PRIMARY KEY CLUSTERED"; skipString "FOREIGN KEY" ]

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

/// Parses a column definition
let column = 
    spaces 
    >>. segment
    .>> spaces
    .>>. choice 
        [ DataTypes.uniqueIdentifier
          DataTypes.bit
          DataTypes.varChar
          DataTypes.int
          DataTypes.date ]
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
