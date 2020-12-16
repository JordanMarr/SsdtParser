module MicrosoftParser

open System.IO
open System.Xml

type Table = {
    Schema: string
    Name: string
    Columns: Column list
    PrimaryKeys: PrimaryKeyConstraint list
    ForeignKeys: ForeignKeyConstraint list
}
and Column = {
    Name: string
    DataType: string
    AllowNulls: bool
    Identity: IdentitySpec option
}
and IdentitySpec = {
    Seed: int
    Increment: int
}
and ForeignKeyConstraint = {
    Name: string
    Columns: string list
    References: RefTable
}
and RefTable = {
    Schema: string
    Table: string
    Columns: string list
}
and PrimaryKeyConstraint = {
    Name: string
    Columns: string list
}

/// Analyzes Microsoft SQL Parser XML results and returns a Table model.
let parseMsResult (parseResult: Microsoft.SqlServer.Management.SqlParser.Parser.ParseResult) = 
    let doc = new XmlDocument()
    use rdr = new StringReader(parseResult.Script.Xml)
    doc.Load(rdr)
    
    let att (nm: string) (node: XmlNode) = 
        node.Attributes 
        |> Seq.cast<XmlAttribute> 
        |> Seq.tryFind (fun a -> a.Name = nm) 
        |> Option.map (fun a -> a.Value) 
        |> Option.defaultValue ""

    let tblStatement = doc.SelectSingleNode("/SqlScript/SqlBatch/SqlCreateTableStatement")
    let tblSchemaName, tblObjectName = 
        let objId = tblStatement.SelectSingleNode("SqlObjectIdentifier")
        objId |> att "SchemaName", objId |> att "ObjectName"

    let cols = 
        tblStatement.SelectSingleNode("SqlTableDefinition").SelectNodes("SqlColumnDefinition")
        |> Seq.cast<XmlNode>
        |> Seq.map (fun cd -> 
            let colName = cd |> att "Name"
            let dataType = cd.SelectSingleNode("SqlDataTypeSpecification/SqlDataType") |> att "ObjectIdentifier"
            let constraints = cd.SelectNodes("SqlConstraint") |> Seq.cast<XmlNode> |> Seq.map (att "Type")
            let allowNulls = not (constraints |> Seq.contains "NotNull") // default is allow nulls
            let identity = 
                cd.SelectSingleNode("SqlColumnIdentity") 
                |> Option.ofObj 
                |> Option.map (fun n -> { Increment = n |> att "Increment" |> int; Seed = n |> att "Seed" |> int })

            { Column.Name= colName
              Column.DataType = dataType
              Column.AllowNulls = allowNulls
              Column.Identity = identity }
        )
        |> Seq.toList

    let primaryKeyConstraints = 
        tblStatement.SelectSingleNode("SqlTableDefinition").SelectNodes("SqlPrimaryKeyConstraint")
        |> Seq.cast<XmlNode>
        |> Seq.map (fun pkc -> 
            let name = pkc |> att "Name"
            let cols = 
                pkc.SelectNodes("SqlIndexedColumn")
                |> Seq.cast<XmlNode>
                |> Seq.map (att "Name")
                |> Seq.toList
            { PrimaryKeyConstraint.Name = name
              PrimaryKeyConstraint.Columns = cols }
        )
        |> Seq.toList

    let foreignKeyConstraints = 
        tblStatement.SelectSingleNode("SqlTableDefinition").SelectNodes("SqlForeignKeyConstraint")
        |> Seq.cast<XmlNode> 
        |> Seq.map (fun fkc -> 
            let name = fkc |> att "Name"

            let children = fkc.ChildNodes |> Seq.cast<XmlNode> |> Seq.toList |> List.filter (fun c -> c.Name = "SqlIdentifier" || c.Name = "SqlObjectIdentifier")
            let idx = children |> List.findIndex (fun c -> c.Name = "SqlObjectIdentifier")

            let localColumnNodes = children |> List.take idx
            let refTableNode = children.[idx]
            let refColumnNodes = children |> List.skip (idx + 1)

            let fkCols = 
                match localColumnNodes with
                | _ :: fkCols -> fkCols |> List.map (att "Value")
                | _ -> []

            let refCols = 
                refColumnNodes
                |> List.map (att "Value")

            
            let refTable =
                { RefTable.Schema = refTableNode |> att "SchemaName"
                  RefTable.Table = refTableNode |> att "ObjectName"
                  RefTable.Columns = refCols }

            { ForeignKeyConstraint.Name = name
              ForeignKeyConstraint.Columns = fkCols
              ForeignKeyConstraint.References = refTable }
        )
        |> Seq.toList
    
    { Table.Schema = tblSchemaName 
      Table.Name = tblObjectName
      Table.Columns = cols
      Table.PrimaryKeys = primaryKeyConstraints
      Table.ForeignKeys = foreignKeyConstraints }
