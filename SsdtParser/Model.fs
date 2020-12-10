module Model

type TableHeader = {
    Name: string
    Owner: string
}

and Table = {
    Name: string
    Owner: string
    Columns: Column list
    Constraints: Constraint list
}

and Column = {
    Name: string
    DataType: DataType
    AllowNulls: bool
    Default: string option
}

/// All supported SQL Server data types.
and DataType = 
    // Exact numerics
    | BigInt 
    | Numeric
    | Bit 
    | SmallInt
    | Decimal 
    | SmallMoney
    | Int 
    | TinyInt
    | Money

    // Approximate numerics
    | Float
    | Real

    // Date and time
    | Date
    | DateTimeOffset
    | DateTime2
    | SmallDateTime
    | DateTime
    | Time

    // Character strings
    | Char
    | VarChar
    //| Text - deprecated!

    // Unicode character strings
    | NChar
    | NVarChar
    //| NText - deprecated!

    // Binary strings
    | Binary
    | VarBinary
    //| Image - deprecated!

    // Other data types
    //| Cursor
    | RowVersion
    | HierarchyId
    | UniqueIdentifier
    //| SqlVariant
    | Xml
    | Geometry
    | Geography
    //| Table

and Constraint = {
    Name: string
    //Type: ConstraintType
}

and ConstraintType = 
    | PrimaryKey
    | ForeignKey of table: string * column: string

