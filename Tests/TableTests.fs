namespace Huffman.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Huffman
open System.Diagnostics


[<TestClass>]
type TableTests () =

    let assertValue (key : char) (expected : int) (map : Map<char, FrequencyTable.Coding>) = 
        match Map.tryFind key map with
        | None -> Assert.Fail()
        | Some x-> Assert.AreEqual(expected, x.value)
        map

    [<TestMethod>]
    member this.TableContentsTest () =      
            
        
        FrequencyTable.getCounts ['a'; 'b'; 'c'; 'c'; 'a'] Map.empty
        |> FrequencyTable.buildTree
        |> Map.toList
        |> List.head
        |> snd
        |> FrequencyTable.generateTable 
        |> Map.ofList 
        |> assertValue 'b' 1
        |> assertValue 'a' 3
        |> assertValue 'c' 0
        |> ignore




    