module ParserTests
open Expecto
open FParsec
open Parser

let testParser desc parser (text: string) =
    testCase desc <| fun _ ->
        match (run parser text) with
        | Success(result,_,_) -> printfn $"Success: %A{result}"
        | Failure(_,error,_) -> failwith $"Error: %A{error}"

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
        
        testParser "uniqueIdentifier" DataTypes.uniqueIdentifier "UNIQUEIDENTIFIER"
        testParser "bit" DataTypes.bit "BIT"
        testParser "varChar1" DataTypes.varChar "VARCHAR(100)"
        testParser "varChar2" DataTypes.varChar "VARCHAR (MAX)"

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
