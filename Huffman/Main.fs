namespace Huffman 


module Main = 

    type EncodingState = {output : int list; currentBuffer : int; bufferCount : int}

    let encode (txt : string) =     


        let dict =
            txt.ToCharArray()
            |> List.ofArray
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
            
            if currentCode.meaningfulBits > (32 - state.bufferCount) then
                // needs to write only a part of the code 
                {output = state.output; currentBuffer = state.currentBuffer; bufferCount = state.bufferCount}
            else   
                // write whole code to the buffer
                {currentBuffer = 
                    state.bufferCount
                    |> (<<<) currentCode.value  
                    |> (|||) currentBuffer;
                bufferCount += currentCode.meaningfulBits;
                output = if state.bufferCount < 32 then state.currentBuffer else state.currentBuffer}
               



        txt
        |> List.fold processChar       
        
        ()

    let decode = 
        ()

            
    

