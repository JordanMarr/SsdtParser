module MsParserWorkspace

open Expecto
open System.IO
open MicrosoftParser

let readParseResultXml() =
    let file = FileInfo("SqlParseResult_TimeEntries.xml") // Expecto
    //let file = FileInfo("../../../SqlParseResult_TimeEntries.xml") // VS Test Explorer
    File.ReadAllText(file.FullName)

[<Tests>]
let tests =
    let tableScript1 = 
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

    let enrollmentTableScript = 
        "CREATE TABLE [dbo].[Enrollment] (
        [EnrollmentID] INT IDENTITY (1, 1) NOT NULL,
        [Grade]        DECIMAL(3, 2) NULL,
        [CourseID]     INT NOT NULL,
        [StudentID]    INT NOT NULL,
        PRIMARY KEY CLUSTERED ([EnrollmentID] ASC),
        CONSTRAINT [FK_dbo.Enrollment_dbo.Course_CourseID] FOREIGN KEY ([CourseID]) 
            REFERENCES [dbo].[Course] ([CourseID]) ON DELETE CASCADE,
        CONSTRAINT [FK_dbo.Enrollment_dbo.Student_StudentID] FOREIGN KEY ([StudentID]) 
            REFERENCES [dbo].[Student] ([StudentID]) ON DELETE CASCADE
        )"

    testList "Microsoft SQL Parser" [

        testCase "Parse Script to XML" <| fun _ ->
            let result = Microsoft.SqlServer.Management.SqlParser.Parser.Parser.Parse(tableScript1)
            printfn "Parse Result: %s" result.Script.Xml
            
        testCase "Read XML" <| fun _ ->
            let xml = readParseResultXml()
            printfn "%s" xml
            ()

        ftestCase "Parse schema from table script" <| fun _ ->
            Microsoft.SqlServer.Management.SqlParser.Parser.Parser.Parse tableScript1
            |> MicrosoftParser.parseMsResult
            |> printfn "%A"

        testCase "Parse Enrollment Table Script" <| fun _ ->
            Microsoft.SqlServer.Management.SqlParser.Parser.Parser.Parse enrollmentTableScript
            |> MicrosoftParser.parseMsResult
            |> printfn "%A"

    ]