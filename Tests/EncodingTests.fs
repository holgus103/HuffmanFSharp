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
    member this.EncodingBufferFlushTest () =
        // a -> 0
        // b -> 10
        // c -> 11 

        let value = Main.encode "abccaabcabcabcabcababc"
        Trace.WriteLine(value)
                         
        Assert.AreEqual(0b01001110011100111001110001111100, List.head value.output)
        Assert.AreEqual(value.currentBuffer, 0b1110)
