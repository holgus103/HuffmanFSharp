namespace Huffman.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Huffman


[<TestClass>]
type TreeTests () =

    [<TestMethod>]
    member this.TreeCountTest () =
        
        FrequencyTable.getCounts ['a'; 'b'; 'c'; 'c'; 'a'] Map.empty
        |> FrequencyTable.buildTree
        |> Map.count
        |> (fun x -> Assert.AreEqual(1, x))
        
    [<TestMethod>]
    member this.TreeContentsTest () =      
            
        FrequencyTable.getCounts ['a'; 'b'; 'c'; 'c'; 'a'] Map.empty
        |> FrequencyTable.buildTree
        |> Map.toList
        |> List.head
        |> (fun (key, value) -> 
            Assert.AreEqual(5, value.value)
        )