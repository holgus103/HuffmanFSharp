namespace Huffman.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Huffman
open System.Diagnostics


[<TestClass>]
type EncodingTests () =


    [<TestMethod>]
    member this.EncodingShortTest () =      
            
        let value = Main.encode "abcca"

        Trace.WriteLine(value)
        Assert.AreEqual(value.currentBuffer, 0b11001011)
        // Assert.Fail()




    