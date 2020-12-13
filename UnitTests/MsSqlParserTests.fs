module MsSqlParserTests

open Expecto
open System.IO
open System.Xml

let readParseResultXml() =
    let file = FileInfo("SqlParseResult_TimeEntries.xml") // Expecto
    //let file = FileInfo("../../../SqlParseResult_TimeEntries.xml") // VS Test Explorer
    File.ReadAllText(file.FullName)

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

[<Tests>]
let tests =
    testList "Microsoft SQL Parser" [

        testCase "MS Parse Script to XML" <| fun _ ->
            let createTableScript = 
                "CREATE TABLE [dbo].[TimeEntries] (
                [Id]        UNIQUEIDENTIFIER   NOT NULL,
                [ProjectId] UNIQUEIDENTIFIER   NOT NULL,
                [TaskId]    UNIQUEIDENTIFIER   NOT NULL,
                [SheetId]   UNIQUEIDENTIFIER   NULL,
                [Username]  VARCHAR (100)      NOT NULL,
                [Email]     VARCHAR (100)      NULL,
                [Date]      DATE               NOT NULL,
                [Hours]     FLOAT (53)         NOT NULL,
                [Created]   DATETIMEOFFSET (0) CONSTRAINT [DF_TimeEntries_Created] DEFAULT (getutcdate()) NOT NULL,
                [Updated]   DATETIMEOFFSET (0) CONSTRAINT [DF_TimeEntries_Updated] DEFAULT (getutcdate()) NOT NULL,
                CONSTRAINT [PK_TimeEntries] PRIMARY KEY CLUSTERED ([Id] ASC),
                CONSTRAINT [FK_TimeEntries_Projects] FOREIGN KEY ([ProjectId]) REFERENCES [dbo].[Projects] ([Id]),
                CONSTRAINT [FK_TimeEntries_ProjectTasks] FOREIGN KEY ([TaskId]) REFERENCES [dbo].[ProjectTasks] ([Id]),
                CONSTRAINT [FK_TimeEntries_DrawingLogSheets] FOREIGN KEY ([SheetId]) REFERENCES [dbo].[DrawingLogSheets] ([Id]),
                CONSTRAINT [UQ_TimeEntries_TaskId_Email_Date_Sheet] UNIQUE NONCLUSTERED ([TaskId] ASC, [Email] ASC, [Date] ASC, [SheetId] ASC)
                );"

            let parseResult = Microsoft.SqlServer.Management.SqlParser.Parser.Parser.Parse(createTableScript)
            let xml = parseResult.Script.Xml
            printfn "Parse Result: %s" xml
            
            
        ftestCase "Read XML" <| fun _ ->
            let xml = readParseResultXml()
            let att (nm: string) (node: XmlNode) = node.Attributes.[nm].Value
            let doc = new XmlDocument()
            use stream = new StringReader(xml)
            doc.Load(stream)
            let tblStatement = doc.SelectSingleNode("/SqlScript/SqlBatch/SqlCreateTableStatement")
            let tblSchemaName, tblObjectName = 
                let objId = tblStatement.SelectSingleNode("SqlObjectIdentifier")
                objId |> att "SchemaName", objId |> att "ObjectName"

            let cols = 
                tblStatement.SelectSingleNode("SqlTableDefinition").SelectNodes("SqlColumnDefinition")
                |> Seq.cast<XmlNode>
                |> Seq.map (fun c -> 
                    let colName = c |> att "Name"
                    let dataType = c.SelectSingleNode("SqlDataTypeSpecification/SqlDataType") |> att "ObjectIdentifier"
                    let constraints = c.SelectNodes("SqlDataTypeSpecification/SqlConstraint") |> Seq.cast<XmlNode> |> Seq.map (att "Type")
                    let allowNulls = not (constraints |> Seq.contains "NotNull") // default is allow nulls
                    { Column.Name= colName
                      Column.DataType = dataType
                      Column.AllowNulls = allowNulls }
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
            
            let table = 
                { Table.Schema = tblSchemaName 
                  Table.Name = tblObjectName
                  Table.Columns = cols
                  Table.PrimaryKeys = primaryKeyConstraints
                  Table.ForeignKeys = foreignKeyConstraints }

            printfn "Table: %A" table
            ()

    ]