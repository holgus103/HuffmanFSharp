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
            
        let v = FrequencyTable.getCounts ['a'; 'b'; 'c'; 'c'; 'a'] Map.empty
        Trace.WriteLine(v);
        let t = 
            v
            |> FrequencyTable.buildTree
            |> Map.toList
            |> List.head
            |> snd

        Trace.WriteLine(t)
        let tab = 
            t
            |> FrequencyTable.generateTable 
        Trace.WriteLine(tab)
        tab
        |> Map.ofList 
        |> assertValue 'b' 1
        |> assertValue 'a' 0
        |> assertValue 'c' 1
        |> ignore




    