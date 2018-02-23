namespace Huffman.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Huffman


[<TestClass>]
type BasicTests () =

    let validator c v = 
        match v with 
        | Some x -> Assert.AreEqual(c, x)
        | None -> Assert.Fail ()
        
    [<TestMethod>]
    member this.TestMethodPassing () =
        Assert.IsTrue(true);

    [<TestMethod>]
    member this.CountsBasicOneTest () = 

        FrequencyTable.getCounts Map.empty ['a'] 
        |> Map.tryFind 'a'
        |> validator 1

    [<TestMethod>]
    member this.CountsBasicFiveTest () = 

        FrequencyTable.getCounts Map.empty ['a'; 'a'; 'a'; 'a'; 'a'] 
        |> Map.tryFind 'a'
        |> validator 5

    [<TestMethod>]
    member this.CountsVariousTest () = 

        let res = FrequencyTable.getCounts Map.empty ['a'; 'b'; 'c'; 'c'; 'a'] 
        
        let folder state v =
            let (k, c) = v
            res
            |> Map.tryFind k
            |> validator c

        [('a', 2); ('b', 1); ('c', 2)]
        |> List.fold folder ()






  

        
