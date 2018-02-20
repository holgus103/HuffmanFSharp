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
    member this.CountsBasicOneTest () = 

        FrequencyTable.getCounts ['a'] Map.empty
        |> Map.tryFind 'a'
        |> validator 1

    [<TestMethod>]
    member this.CountsBasicFiveTest () = 

        FrequencyTable.getCounts ['a'; 'a'; 'a'; 'a'; 'a'] Map.empty
        |> Map.tryFind 'a'
        |> validator 5

    [<TestMethod>]
    member this.CountsVariousTest () = 

        let res = FrequencyTable.getCounts ['a'; 'b'; 'c'; 'c'; 'a'] Map.empty
        
        let folder state v =
            let (k, c) = v
            res
            |> Map.tryFind k
            |> validator c

        [('a', 2); ('b', 1); ('c', 2)]
        |> List.fold folder ()






  

        
