namespace Huffman 
open System
open FrequencyTable

module Main = 

    type EncodingState = 
        {
            output : int list;
            currentBuffer : int;
            bufferCount : int
        }

    type EncodedResult = 
        {
            output : int list;
            appendedZeros : int;
            dictionary : Map<char, Coding>
        }

    type DecodingState = 
        {
            input : int list;
            output : char list;
            buffer : int;
            bufferCount : int;
            foundCode : Coding
        }    

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
        |> (fun x -> 
                if x.bufferCount > 0 then
                    {
                    output = x.currentBuffer :: x.output;
                    appendedZeros = 32 - x.bufferCount;
                    dictionary = dict
                    }
                else 
                    {
                       output = x.output;
                       appendedZeros = 0;
                       dictionary = dict
                    }
            )
       

    let decode (input : EncodedResult) = 

        let dict = 
            input.dictionary
            |> FrequencyTable.invertDictionary
            |> (fun x-> x.dict)
        
            
        let rec processCodes (state : DecodingState) =



            if state.bufferCount = 0 then
            // no bits left to read, refill buffer and start another iteration
                match state.input with
                // end of input - exit condition
                | [] -> 
                    match Map.tryFind state.foundCode dict with 
                    // fimished reading, just append the last character
                    | Some x -> x::state.output
                    // none found, input error?
                    | None -> state.output
                // refill buffer
                | b::tail -> 
                    match Map.tryFind state.foundCode dict with
                    // append new characted and refill buffer
                    | Some x ->
                        {
                            input = tail;
                            output = x::state.output;  
                            buffer = b;
                            bufferCount = 32;
                            foundCode = {value = 0; meaningfulBits = 0}                            
                        }
                    | None -> 
                        {
                            input = tail;
                            output = state.output;
                            buffer = b;
                            bufferCount = 32;
                            foundCode = state.foundCode;
                        }
                    // process further
                    |> processCodes
            else
                {
                    input = state.input;
                    output = state.output;
                    buffer = state.buffer <<< 1;
                    bufferCount = state.bufferCount - 1;
                    foundCode = 
                        match Map.tryFind state.foundCode dict with
                        // no code exists, extend current code
                        | None -> 
                            {
                                meaningfulBits = state.foundCode.meaningfulBits + 1;
                                value =
                                    (
                                        1
                                        |> (-) state.bufferCount
                                        |> (<<<) 1
                                        |> (&&&) state.buffer,
                                        1
                                        |> (+) state.foundCode.meaningfulBits
                                        |> (-) state.bufferCount
                                    )                            
                                    ||> (>>>)
                                    |> (|||) state.buffer 
                            }
                        // code found, reset current code
                        | Some x -> 
                            {
                                meaningfulBits = 0;
                                value = 
                                    1
                                    |> (-) state.bufferCount
                                    |> (<<<) 1
                                    |> (&&&) state.buffer
                                    |> (>>>) (state.bufferCount - 1)
                            }
                
                }
                |> processCodes
                



        ()

            
    

