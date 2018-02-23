namespace Huffman.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Huffman


[<TestClass>]
type TreeTests () =

    [<TestMethod>]
    member this.TreeCountTest () =
        
        FrequencyTable.getCounts Map.empty ['a'; 'b'; 'c'; 'c'; 'a'] 
        |> FrequencyTable.buildTree
        |> Map.count
        |> (fun x -> Assert.AreEqual(1, x))
        
    [<TestMethod>]
    member this.TreeContentsTest () =      
            
        FrequencyTable.getCounts Map.empty ['a'; 'b'; 'c'; 'c'; 'a'] 
        |> FrequencyTable.buildTree
        |> Map.toList
        |> List.head
        |> (fun (key, value) -> 
            Assert.AreEqual(5, value.value)
        )

    