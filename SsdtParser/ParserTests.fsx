#r "nuget: FParsec"
open FParsec

#load "Model.fs"
#load "Parser.fs"
open Model
open Parser

let test parser text =
    match (run parser text) with
    | Success(result,_,_) -> printfn "Success: %A" result
    | Failure(_,error,_) -> printfn "Error: %A" error

//test segmentWithBrackets "[Total Price]"
//test segmentNoBrackets "dbo."
test segment "[dbo]."
test segment "dbo."
test segment "[Orders]"
test segment "Orders"
test tableHeader "CREATE TABLE [dbo].[DrawingLog] ("
test tableHeader "CREATE TABLE [DrawingLog]    ("
test column "    [Id]                              UNIQUEIDENTIFIER NOT NULL,"
test column "    [DatePrelimForReviewSheetIssued]  DATE         NULL,"
test column "    [SheetName]                       VARCHAR (200)    NOT NULL,"
test column "    [Holidays] VARCHAR(MAX) NOT NULL DEFAULT '[]', "
test DataTypes.uniqueIdentifier "UNIQUEIDENTIFIER"
test DataTypes.bit "BIT"
test DataTypes.varChar "VARCHAR(100)"
test DataTypes.varChar "VARCHAR (MAX)"
test colConstraint "CONSTRAINT [PK_DrawingLog] PRIMARY KEY CLUSTERED ([ProjectId]),"
test colConstraint "CONSTRAINT [FK_DrawingLog_Projects] FOREIGN KEY ([ProjectId]) REFERENCES [dbo].[Projects] ([Id])"
test colConstraint "CONSTRAINT [PK_DrawingLogSheets] PRIMARY KEY CLUSTERED ([Id] ASC),"
test colConstraint "CONSTRAINT [UQ_DrawingLogSheets_SheetNumber] UNIQUE ([ProjectId], SheetNumber)"

test table """
CREATE TABLE [dbo].[DrawingLog] ( 
    [ProjectId]                       UNIQUEIDENTIFIER NOT NULL,
    [DurationForQAQC] INT NOT NULL , 
    [DurationToReviseQAQCComments] INT NOT NULL , 
    [DurationForGCToReview] INT NOT NULL , 
    [DurationToReviseCommentsFromGC] INT NOT NULL , 
    [WorkOnSaturdays] BIT NOT NULL , 
    [Holidays] VARCHAR(MAX) NOT NULL DEFAULT '[]', 
    CONSTRAINT [PK_DrawingLog] PRIMARY KEY CLUSTERED ([ProjectId]),
    CONSTRAINT [FK_DrawingLog_Projects] FOREIGN KEY ([ProjectId]) REFERENCES [dbo].[Projects] ([Id])
);"""

test table """
CREATE TABLE [dbo].[DrawingLogHistory] (
    [SheetId]           UNIQUEIDENTIFIER   NOT NULL,
    [SheetNumber]       VARCHAR (40)       NOT NULL,
    [ChangeDescription] VARCHAR (200)      NOT NULL,
    [Changed]           DATETIMEOFFSET (7) NOT NULL,
    [Username]          VARCHAR (100)      NOT NULL
);"""
