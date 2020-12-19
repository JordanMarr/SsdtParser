module MicrosoftParser

open System.IO
open System.Xml

type SsdtTable = {
    Schema: string
    Name: string
    Columns: SsdtColumn list
    PrimaryKey: PrimaryKeyConstraint option
    ForeignKeys: ForeignKeyConstraint list
}
and SsdtColumn = {
    Name: string
    DataType: string
    AllowNulls: bool
    Identity: IdentitySpec option
    HasDefault: bool
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
and SsdtView = {
    Schema: string
    Name: string
    Columns: SsdtViewColumn list
}
and SsdtViewColumn = {
    Name: string
    RefCol: SsdtColumn
}

/// Analyzes Microsoft SQL Parser XML results and returns an SsdtTable model.
let parseTableResult (parseResult: Microsoft.SqlServer.Management.SqlParser.Parser.ParseResult) = 
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
            let hasDefaultConstraint = cd.SelectSingleNode("SqlDefaultConstraint") <> null

            { SsdtColumn.Name= colName
              SsdtColumn.DataType = dataType
              SsdtColumn.AllowNulls = allowNulls
              SsdtColumn.Identity = identity
              SsdtColumn.HasDefault = hasDefaultConstraint }
        )
        |> Seq.toList

    let primaryKeyConstraint = 
        tblStatement.SelectSingleNode("SqlTableDefinition").SelectSingleNode("SqlPrimaryKeyConstraint")
        |> Option.ofObj
        |> Option.map (fun pkc -> 
            let name = pkc |> att "Name"
            let cols = 
                pkc.SelectNodes("SqlIndexedColumn")
                |> Seq.cast<XmlNode>
                |> Seq.map (att "Name")
                |> Seq.toList
            { PrimaryKeyConstraint.Name = name
              PrimaryKeyConstraint.Columns = cols }
        )

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
    
    { SsdtTable.Schema = tblSchemaName 
      SsdtTable.Name = tblObjectName
      SsdtTable.Columns = cols
      SsdtTable.PrimaryKey = primaryKeyConstraint
      SsdtTable.ForeignKeys = foreignKeyConstraints }

/// Analyzes Microsoft SQL Parser XML results and returns an SsdtView model.
let parseViewResult (tablesByFullName: Map<string * string, SsdtTable>) (parseResult: Microsoft.SqlServer.Management.SqlParser.Parser.ParseResult) = 
    let doc = new XmlDocument()
    use rdr = new StringReader(parseResult.Script.Xml)
    doc.Load(rdr)

    let attMaybe (nm: string) (node: XmlNode) = 
        node.Attributes 
        |> Seq.cast<XmlAttribute> 
        |> Seq.tryFind (fun a -> a.Name = nm) 
        |> Option.map (fun a -> a.Value) 

    let att (nm: string) (node: XmlNode) = 
        attMaybe nm node |> Option.defaultValue ""

    let viewDef = doc.SelectSingleNode("/SqlScript/SqlBatch/SqlCreateViewStatement/SqlViewDefinition")
    let viewSchemaName, viewObjectName = 
        let objId = viewDef.SelectSingleNode("SqlObjectIdentifier")
        objId |> att "SchemaName", objId |> att "ObjectName"

    // Represents all the types of view columns that we have implemented for parsing
    let (|SqlScalarRefExpression|SqlNullScalarExpression|Other|) (parentNode: XmlNode) =
        let ssre = parentNode.SelectSingleNode("SqlScalarRefExpression")
        if ssre <> null then SqlScalarRefExpression ssre
        else
            let snse = parentNode.SelectSingleNode("SqlNullScalarExpression")
            if snse <> null then SqlNullScalarExpression snse
            else Other

    let cols = 
        viewDef.SelectSingleNode("SqlQuerySpecification").SelectSingleNode("SqlSelectClause").SelectNodes("SqlSelectScalarExpression")
        |> Seq.cast<XmlNode>
        |> Seq.choose (fun ssce ->
            match ssce with
            | SqlScalarRefExpression exp -> 
                let objId = exp.SelectSingleNode("SqlObjectIdentifier")
                let schema = objId |> att "DatabaseName" // Not sure why these are the way they are...
                let table = objId |> att "SchemaName"
                let tableColumn = objId |> att "ObjectName"
                let colName = ssce |> attMaybe "Alias" |> Option.defaultValue tableColumn

                // Try to find related table/column, else ignore
                tablesByFullName.TryFind(schema, table)
                |> Option.bind (fun table -> table.Columns |> List.tryFind (fun c -> c.Name = tableColumn))
                |> Option.map (fun refCol ->
                    { SsdtViewColumn.Name = colName
                      SsdtViewColumn.RefCol = refCol }
                )
            | SqlNullScalarExpression exp -> 
                let sqlId = ssce.SelectSingleNode("SqlIdentifier")
                let colName = sqlId |> att "Value"
                Some
                    { SsdtViewColumn.Name = colName
                      SsdtViewColumn.RefCol = 
                        { SsdtColumn.Name= colName
                          SsdtColumn.DataType = "SQL_VARIANT" // TODO: Could possibly manually parse XML comment to determine ref table.column
                          SsdtColumn.AllowNulls = false
                          SsdtColumn.Identity = None
                          SsdtColumn.HasDefault = false } }
            | Other -> 
                None // Some view column type that is not yet handled...
        )
        |> Seq.toList
            
    { SsdtView.Schema = viewSchemaName 
      SsdtView.Name = viewObjectName
      SsdtView.Columns = cols }

let viewToTable (view: SsdtView) = 
    { SsdtTable.Schema = view.Schema
      SsdtTable.Name = view.Name
      SsdtTable.Columns = view.Columns |> List.map (fun c -> c.RefCol)
      SsdtTable.PrimaryKey = None
      SsdtTable.ForeignKeys = [] }