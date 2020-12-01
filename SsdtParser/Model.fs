module Model

type TableHeader = {
    Name: string
    Owner: string
    //Columns: Column list
    //PKs: Column list
}

and Table = {
    Name: string
    Owner: string
    Columns: Column list
}

and Column = {
    Name: string
    DataType: DataType
    AllowNulls: bool
    Default: string option
}

and DataType = 
    | UniqueIdentifier
    | Int
    | Bit
    | VarChar
    | Date

and Constraint = {
    Name: string
    Type: ConstraintType
}

and ConstraintType = 
    | PrimaryKey
    | ForeignKey of table: string * column: string
