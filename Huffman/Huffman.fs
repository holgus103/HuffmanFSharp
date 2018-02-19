namespace Huffman 

module Huffman = 

    type MinInfo = {key : char; value : int}
    let incrementCounter (dict : Map<char, int>) (key : char) =
        match Map.tryFind key dict with
            | None -> Map.add key 1 dict
            | Some x -> 
                Map.remove key dict
                |> Map.add key (x + 1)

    let rec getCounts text res = 
        match text with 
        | [] -> res
        | head :: tail -> 
            incrementCounter res head
            |> getCounts tail
            
    let findMin counts = 
        counts
        |> Map.toList
        |> List.fold
            (fun (state : MinInfo) x -> 
                let (key, value) = x;
                if state.value < 0 || value < state.value then {key = key; value = value}
                else state
            )
            {value = -1 ; key = '0'}

            
    

