module ParserTests
open Expecto
open FParsec
open Parser
open Model

let testParser desc parser (text: string) =
    testCase desc <| fun _ ->
        match (run parser text) with
        | Success(result,_,_) -> printfn $"Success: %A{result}"
        | Failure(_,error,_) -> failwith $"Error: %A{error}"

let testDataTypeParser desc (dataType: DataType) (text: string) =
    testParser desc (DataTypeParsers.getDataTypeParser dataType) text

[<Tests>]
let tests =
    testList "Parse" [
        //testParser segmentWithBrackets "[Total Price]"
        //testParser segmentNoBrackets "dbo."
        testParser "segment1" segment "[dbo]."
        testParser "segment2" segment "dbo."
        testParser "segment3" segment "[Orders]"
        testParser "segment4" segment "Orders"

        testParser "tableHeader1" tableHeader "CREATE TABLE [dbo].[DrawingLog] ("
        testParser "tableHeader2" tableHeader "CREATE TABLE [DrawingLog]    ("

        testParser "testParser1" column "    [Id]                              UNIQUEIDENTIFIER NOT NULL,"
        testParser "testParser2" column "    [DatePrelimForReviewSheetIssued]  DATE         NULL,"
        testParser "testParser3" column "    [SheetName]                       VARCHAR (200)    NOT NULL,"
        testParser "testParser4" column "    [Holidays] VARCHAR(MAX) NOT NULL DEFAULT '[]', "
        
        testDataTypeParser "uniqueIdentifier" UniqueIdentifier "UNIQUEIDENTIFIER"
        testDataTypeParser "bit" Bit "BIT"
        testDataTypeParser "varChar100" VarChar "VARCHAR(100)"
        testDataTypeParser "varCharMax" VarChar "VARCHAR (MAX)"
        testDataTypeParser "nvarChar100" NVarChar "NVARCHAR(100)"
        testDataTypeParser "nvarCharMax" NVarChar "NVARCHAR (MAX)"
        testDataTypeParser "char" Char "CHAR (MAX)"
        testDataTypeParser "nchar" NChar "NCHAR (255)"
        testDataTypeParser "time 7" Time "TIME (7)"
        testDataTypeParser "datetimeoffset 7" DateTimeOffset "DATETIMEOFFSET (7)"
        testDataTypeParser "datetimeoffset" DateTimeOffset "DATETIMEOFFSET"
        testDataTypeParser "smalldatetime" SmallDateTime "SMALLDATETIME"
        testDataTypeParser "datetime" DateTime "DATETIME"
        testDataTypeParser "datetime2 7" DateTime2 "DATETIME2 (7)"
        testDataTypeParser "datetime2" DateTime2 "DATETIME2"
        testDataTypeParser "int" Int "INT"
        testDataTypeParser "bigint" BigInt "BIGINT"
        testDataTypeParser "tinyint" TinyInt "TINYINT"
        testDataTypeParser "smallint" SmallInt "SMALLINT"
        testDataTypeParser "float" Float "FLOAT"
        testDataTypeParser "real" Real "REAL"
        testDataTypeParser "decimal" Decimal "DECIMAL"
        testDataTypeParser "decimal (9)" Decimal "DECIMAL(9)"
        testDataTypeParser "decimal (9, 7)" Decimal "DECIMAL(9, 7)"
        testDataTypeParser "numeric" Numeric "NUMERIC"
        testDataTypeParser "numeric (9)" Numeric "NUMERIC(9)"
        testDataTypeParser "numeric (9, 7)" Numeric "NUMERIC(9, 7)"
        testDataTypeParser "money" Money "MONEY"
        testDataTypeParser "smallmoney" SmallMoney "SMALLMONEY"
        testDataTypeParser "varbinary" VarBinary "VARBINARY(5000)"
        testDataTypeParser "varbinary max" VarBinary "VARBINARY(MAX)"
        testDataTypeParser "binary" Binary "BINARY"
        testDataTypeParser "binary 500" Binary "BINARY (500)"
        testDataTypeParser "rowversion" RowVersion "ROWVERSION"
        testDataTypeParser "xml" Xml "XML"

        testParser "colConstraint1" colConstraint "CONSTRAINT [PK_DrawingLog] PRIMARY KEY CLUSTERED ([ProjectId]),"
        testParser "colConstraint2" colConstraint "CONSTRAINT [FK_DrawingLog_Projects] FOREIGN KEY ([ProjectId]) REFERENCES [dbo].[Projects] ([Id])"
        testParser "colConstraint3" colConstraint "CONSTRAINT [PK_DrawingLogSheets] PRIMARY KEY CLUSTERED ([Id] ASC),"
        testParser "colConstraint4" colConstraint "CONSTRAINT [UQ_DrawingLogSheets_SheetNumber] UNIQUE ([ProjectId], SheetNumber)"

        testParser "table1" table 
            "CREATE TABLE [dbo].[DrawingLog] ( 
                [ProjectId]                       UNIQUEIDENTIFIER NOT NULL,
                [DurationForQAQC] INT NOT NULL , 
                [DurationToReviseQAQCComments] INT NOT NULL , 
                [DurationForGCToReview] INT NOT NULL , 
                [DurationToReviseCommentsFromGC] INT NOT NULL , 
                [WorkOnSaturdays] BIT NOT NULL , 
                [Holidays] VARCHAR(MAX) NOT NULL DEFAULT '[]', 
                CONSTRAINT [PK_DrawingLog] PRIMARY KEY CLUSTERED ([ProjectId]),
                CONSTRAINT [FK_DrawingLog_Projects] FOREIGN KEY ([ProjectId]) REFERENCES [dbo].[Projects] ([Id])
            );"
            
        testParser "table2" table 
            "CREATE TABLE [dbo].[DrawingLogHistory] (
                [SheetId]           UNIQUEIDENTIFIER   NOT NULL,
                [SheetNumber]       VARCHAR (40)       NOT NULL,
                [ChangeDescription] VARCHAR (200)      NOT NULL,
                [Changed]           DATETIMEOFFSET (7) NOT NULL,
                [Username]          VARCHAR (100)      NOT NULL
            );"
    ]
