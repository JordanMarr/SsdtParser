module MsParserWorkspace

open Expecto
open System.IO
open System
open Microsoft.SqlServer.Management.SqlParser.Parser

let readParseResultXml() =
    let file = FileInfo("SqlParseResult_TimeEntries.xml") // Expecto
    //let file = FileInfo("../../../SqlParseResult_TimeEntries.xml") // VS Test Explorer
    File.ReadAllText(file.FullName)

[<Tests>]
let tests =
    let timeEntriesScript = 
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

    let computedColumnTableScript = 
        "CREATE TABLE dbo.Products
        (
           ProductID int IDENTITY (1,1) NOT NULL
           , QtyAvailable smallint
           , UnitPrice money
           , InventoryValue AS QtyAvailable * UnitPrice
         );"

    let view1 = 
        "CREATE VIEW dbo.v_AllTasksLog
        AS
        SELECT        dbo.Projects.Name AS ProjectName, dbo.Projects.ProjectNumber, dbo.Projects.ProjectType, dbo.Projects.LOD, dbo.Projects.Division, dbo.Projects.IsActive, dbo.ProjectTaskCategories.Name AS Category, 
                                 dbo.ProjectTasks.Name AS TaskName, dbo.ProjectTasks.CostCode, dbo.ProjectTasks.Sort, dbo.TimeEntries.Username, dbo.TimeEntries.Email, dbo.TimeEntries.Date, COALESCE (dbo.TimeEntries.Hours, 0) AS Hours, 
                                 dbo.TimeEntries.Created, dbo.TimeEntries.Updated, dbo.Users.EmployeeId
        FROM            dbo.Projects INNER JOIN
                                 dbo.ProjectTasks ON dbo.Projects.Id = dbo.ProjectTasks.ProjectId INNER JOIN
                                 dbo.ProjectTaskCategories ON dbo.ProjectTasks.ProjectTaskCategoryId = dbo.ProjectTaskCategories.Id LEFT OUTER JOIN
                                 dbo.TimeEntries ON dbo.Projects.Id = dbo.TimeEntries.ProjectId AND dbo.ProjectTasks.Id = dbo.TimeEntries.TaskId LEFT OUTER JOIN
                                 dbo.Users ON dbo.Users.Email = dbo.TimeEntries.Email"

    testList "Microsoft SQL Parser" [

        testCase "Parse Script to XML" <| fun _ ->
            let result = Parser.Parse view1
            printfn "XML: %s" result.Script.Xml

        testCase "Dynamically Parse Table Script" <| fun _ ->
            let xml = Reflection.parseSqlScript(timeEntriesScript)
            printfn "XML: %s" xml
            
        testCase "Read XML" <| fun _ ->
            let xml = readParseResultXml()
            printfn "XML %s" xml
            ()

        testCase "Computed Column XML" <| fun _ -> 
            let result = Parser.Parse(computedColumnTableScript)
            printfn "XML: %s" result.Script.Xml

        testCase "Parse schema from table script" <| fun _ ->
            Parser.Parse timeEntriesScript
            |> MicrosoftParser.parseTableResult
            |> printfn "%A"

        testCase "Parse Enrollment Table Script" <| fun _ ->
            Parser.Parse enrollmentTableScript
            |> MicrosoftParser.parseTableResult
            |> printfn "%A"

        ftestCase "Parse View" <| fun _ ->
            let tbl = Parser.Parse timeEntriesScript |> MicrosoftParser.parseTableResult
            let tables = [(tbl.Schema,tbl.Name), tbl] |> Map.ofList
            Parser.Parse view1
            |> MicrosoftParser.parseViewResult tables
            |> printfn "%A"
    ]