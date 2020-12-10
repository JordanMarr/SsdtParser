module Parser
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

module DataTypeParsers = 
    open Model
    open Microsoft.FSharp.Reflection
    
    let sizeSingleDigit = skipChar '(' >>. digit .>> skipChar ')' // "(1)", "(7)"
    let sizeMultiDigit = skipChar '(' >>. skipMany digit .>> skipChar ')'
    let sizeAlphaNumeric = skipChar '(' >>. skipMany (letter <|> digit) .>> skipChar ')' // "(100)", "(MAX)"
    let sizeScale = skipChar ',' >>. spaces >>. skipMany digit >>. spaces
    let sizePrecisionScale = skipChar '(' >>. skipMany digit >>. opt sizeScale >>. skipChar ')' 

    /// Ex: "VARCHAR (100)", "VARCHAR(MAX)", etc
    let sizedOrMaxColumn identifier = strCI identifier >>. spaces >>. sizeAlphaNumeric
    let sizedTimeColumn identifier = strCI identifier >>. spaces >>. opt sizeSingleDigit
    let sizedColumn identifier = strCI identifier >>. spaces >>. opt sizeMultiDigit
    let sizedDecimalNumeric identifier = strCI identifier >>. spaces >>. opt sizePrecisionScale

    /// Implements a parser for each case in Model.DataType.
    let getDataTypeParser dt = 
        match dt with
        | UniqueIdentifier -> stringReturn "UNIQUEIDENTIFIER" dt
        | Bit -> stringReturn "BIT" dt    
        | Int -> stringReturn "INT" dt
        | BigInt -> stringReturn "BIGINT" dt
        | SmallInt -> stringReturn "SMALLINT" dt
        | TinyInt -> stringReturn "TINYINT" dt
        | Float -> stringReturn "FLOAT" dt
        | Real -> stringReturn "REAL" dt
        | Decimal -> sizedDecimalNumeric "DECIMAL" >>% dt
        | Numeric -> sizedDecimalNumeric "NUMERIC" >>% dt
        | Money -> stringReturn "MONEY" dt
        | SmallMoney -> stringReturn "SMALLMONEY" dt
        | VarChar -> sizedOrMaxColumn "VARCHAR" >>% dt
        | NVarChar -> sizedOrMaxColumn "NVARCHAR" >>% dt
        | Char -> sizedOrMaxColumn "CHAR" >>% dt
        | NChar -> sizedOrMaxColumn "NCHAR" >>% dt
        | DateTimeOffset -> sizedTimeColumn "DATETIMEOFFSET" >>% dt
        | Date -> stringReturn "DATE" dt
        | DateTime -> stringReturn "DATETIME" dt
        | DateTime2 -> sizedTimeColumn "DATETIME2" >>% dt
        | SmallDateTime -> stringReturn "SMALLDATETIME" dt
        | Time -> sizedTimeColumn "TIME" >>% dt
        | VarBinary -> sizedOrMaxColumn "VARBINARY" >>% dt
        | Binary -> sizedColumn "BINARY" >>% dt
        | RowVersion -> stringReturn "ROWVERSION" dt
        | Xml -> stringReturn "XML" dt
        | Geography -> stringReturn "GEOGRAPHY" dt
        | Geometry -> stringReturn "GEOMETRY" dt
        | HierarchyId -> stringReturn "HIERARCHYID" dt
    
    /// All supported column data types listed by descending length
    let all = 
        FSharpType.GetUnionCases typeof<Model.DataType>
        |> Array.map (fun c -> c.Name, FSharpValue.MakeUnion(c, [||]) :?> Model.DataType) // Will crash if DU contains members which aren't only tags
        |> Array.sortByDescending (fun (name, _) -> name.Length, name) // Longest names first to avoid partial consumption bugs
        |> Array.map snd
        |> Array.map getDataTypeParser
    
let colDefault = 
    skipString "DEFAULT"
    >>. spaces
    >>. manyTill anyChar (pchar ',')

let colConstraintType =
    let clusteredNonClustered = (skipString "CLUSTERED" <|> skipString "NONCLUSTERED")
    choice [ 
        skipString "PRIMARY KEY " .>> opt clusteredNonClustered
        skipString "FOREIGN KEY" 
        skipString "UNIQUE" .>> opt clusteredNonClustered
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
      Model.Column.AllowNulls = allowNulls |> Option.defaultValue true
      Model.Column.Default = None }

let column = 
    spaces 
    >>. segment
    .>> spaces
    .>>. choice DataTypeParsers.all
    .>> spaces
    .>>. opt ((stringReturn "NULL" true) <|> (stringReturn "NOT NULL" false))
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
