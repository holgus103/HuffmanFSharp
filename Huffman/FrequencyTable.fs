namespace Huffman

module FrequencyTable = 


    type TreeNode = {left : TreeNode option; right : TreeNode option; value : int; key : char } 

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
        |> Map.fold
            (fun ((key, state) : (char * TreeNode)) currentKey x -> 
                if state.value < 0 || x.value < state.value then (currentKey, x)
                else (key, state)
            )
            ('0', {value = -1 ; key = '0'; left = None; right = None})

    let buildTree counts = 

        let rec addNodes nodes =
            if Map.count nodes = 1 then
                nodes
            else
                let (min1key, min1val) =
                    nodes
                    |> findMin
                
                let (min2key, min2val) = 
                    nodes
                    |> Map.remove min1key
                    |> findMin

                nodes
                |> Map.remove min1key
                |> Map.remove min2key
                |> Map.add min1key 
                    {
                        left = Some(min1val);
                        right = Some(min2val);
                        key = min1val.key;
                        value = min1val.value + min2val.value
                    }
                |> addNodes
            

        let nodes = 
            counts
            |> Map.map (fun k x -> 
                {left = None; right = None; value = x; key = k})

        nodes 
        |> addNodes