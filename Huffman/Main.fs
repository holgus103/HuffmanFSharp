namespace Huffman 
open System

module Main = 

    type EncodingState = {output : int list; currentBuffer : int; bufferCount : int}

    let encode (txt : string) =     

        let input = 
            txt.ToCharArray()
            |> List.ofArray

        let dict =
            input
            |> FrequencyTable.getCounts Map.empty
            |> FrequencyTable.buildTree
            |> Map.toList
            |> List.head
            |> snd
            |> FrequencyTable.generateTable 

        let processChar (state : EncodingState) (c : char) = 

            let currentCode = 
                match Map.tryFind c dict with
                | None -> raise (Exception("Missing dictionary entry"))
                | Some x -> x
            
            if currentCode.meaningfulBits >= (32 - state.bufferCount) then
            // needs to push buffer to the list 
                {
                output = 
                    state.bufferCount
                    |> (-) 32 
                    |> (<<<) 1
                    |> (+) -1
                    |> (&&&) currentCode.value 
                    |> (|||) state.currentBuffer
                    |> (fun x -> x::state.output)
                bufferCount = 
                    state.bufferCount
                    |> (-) 32
                    |> (-) currentCode.meaningfulBits
                currentBuffer = 
                    state.bufferCount
                    |> (-) 32
                    |> (>>>) currentCode.value            
                }
            else   
            // no need to push the buffer to the list
                {
                    currentBuffer = state.bufferCount
                        |> (<<<) currentCode.value  
                        |> (|||) state.currentBuffer;
                    bufferCount = state.bufferCount + currentCode.meaningfulBits;
                    output = state.output
                }
           


        input
        |> List.fold processChar {output = List.empty; currentBuffer = 0; bufferCount = 0}
       

    let decode = 
        ()

            
    

