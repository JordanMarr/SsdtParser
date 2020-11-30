module Model

type Table = {
    Name: string
    Owner: string
    //Columns: Column list
    //PKs: Column list
}

and Column = {
    Name: string
    DataType: string
    AllowNulls: bool
    Default: string option
}

and DataType = 
    | UniqueIdentifier
    | Int
    | Bit
    | VarChar

and Constraint = {
    Name: string
    Type: ConstraintType
}

and ConstraintType = 
    | PrimaryKey
    | ForeignKey of table: string * column: string

let dataTypes = 
    [ "UNIQUEIDENTIFIER"
      "INT"
      "BIT"
      "VARCHAR" ]