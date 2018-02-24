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

    [<TestMethod>]
    member this.EncodingLongTest () =

        let value = Main.encode "abccaabcabcabcabcabcabcabc"
        01011    
        Trace.WriteLine(value)
        Assert.AreEqual(List.head value.output, 0b10110101101011010110101111001011)
        Assert.AreEqual(value.currentBuffer, 0b01011010110)





    