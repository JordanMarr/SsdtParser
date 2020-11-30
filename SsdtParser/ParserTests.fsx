﻿#r "nuget: FParsec"
open FParsec

#load "Model.fs"
#load "Parser.fs"
open Model
open Parser

let test parser text =
    match (run parser text) with
    | Success(result,_,_) -> printfn "Success: %A" result
    | Failure(_,error,_) -> printfn "Error: %A" error

test segment "[dbo]."
test segment "dbo."
test segment "[Orders]"
test segment "Orders"
test table "CREATE TABLE [dbo].[DrawingLog] ("
test table "CREATE TABLE [DrawingLog]    ("
