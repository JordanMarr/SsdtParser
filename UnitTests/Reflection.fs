module Reflection
    
open System.Reflection
open System.IO
open System

let assembly = System.Reflection.Assembly.LoadFrom(@"C:\_github\SsdtParser\UnitTests\bin\Debug\netcoreapp3.1\Microsoft.SqlServer.Management.SqlParser.dll")

let findType f = assembly.GetTypes() |> Array.find f

// Dynamic load for Microsoft.SqlServer.Management.SqlParser.Parser.Parser
let parserType =  lazy(findType (fun t -> t.Name = "Parser"))
let parseResultType =  lazy(findType (fun t -> t.Name = "ParseResult"))
let sqlScriptType =  lazy(findType (fun t -> t.Name = "SqlScript"))
let parseMethod = lazy(parserType.Value.GetMethod("Parse", [| typeof<string> |])) //Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Static))
let scriptProperty = lazy(parseResultType.Value.GetProperty("Script"))
let xmlProperty = lazy(sqlScriptType.Value.GetProperty("Xml"))

/// Parses a script - returns an xml string with a table schema.
let parseSqlScript (tableScript: string) =
    let oResult = parseMethod.Value.Invoke(null, [| box tableScript |])
    let oSqlScript = scriptProperty.Value.GetValue(oResult)
    xmlProperty.Value.GetValue(oSqlScript) :?> string
