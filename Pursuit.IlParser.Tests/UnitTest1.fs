module Pursuit.IlParser.Tests

open NUnit.Framework
open IlParser


[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    let str = """
        ldstr "wtf"
        
        call void [mscorlib] System. Console :: WriteLine(string)
        
        ret
    """
    
    let expectedCode = [
        Ldstr "wtf"
        Call ("mscorlib", "System.Console", "WriteLine", "void", ["string"])
        Ret
    ]
    
    Assert.AreEqual((parseCilCode str) |> List.toArray, expectedCode |> List.toArray)