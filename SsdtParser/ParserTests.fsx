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

test table """
CREATE TABLE [dbo].[DrawingLog] (
    [ProjectId]                       UNIQUEIDENTIFIER NOT NULL,
    [DurationForQAQC] INT NOT NULL , 
    [DurationToReviseQAQCComments] INT NOT NULL , 
    [DurationForGCToReview] INT NOT NULL , 
    [DurationToReviseCommentsFromGC] INT NOT NULL , 
    [WorkOnSaturdays] BIT NOT NULL , 
    [Holidays] VARCHAR(MAX) NOT NULL DEFAULT '[]', 
);"""