namespace Huffman.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Huffman


[<TestClass>]
type TestClass () =

    let validator c v = 
        match v with 
        | Some x -> Assert.AreEqual(c, x)
        | None -> Assert.Fail ()
        
    [<TestMethod>]
    member this.TestMethodPassing () =
        Assert.IsTrue(true);

    [<TestMethod>]
    member this.CountsBasicTest () = 

        Huffman.getCounts ['a'] Map.empty
        |> Map.tryFind 'a'
        |> validator 1



  

        
