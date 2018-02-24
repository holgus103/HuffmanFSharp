namespace Huffman

open System.Diagnostics

module FrequencyTable = 

    type Coding = {meaningfulBits : int; value : int}

    type TreeNode = {left : TreeNode option; right : TreeNode option; value : int; key : char } 

    let incrementCounter (dict : Map<char, int>) (key : char) =
        match Map.tryFind key dict with
            | None -> Map.add key 1 dict
            | Some x -> 
                Map.remove key dict
                |> Map.add key (x + 1)

    let rec getCounts res text = 
        match text with 
        | [] -> res
        | head :: tail -> 
            (incrementCounter res head |> getCounts) tail
            
            
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

                // Trace.WriteLine(min1val)
                
                let (min2key, min2val) = 
                    nodes
                    |> Map.remove min1key
                    |> findMin

                // Trace.WriteLine(min2val)
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

    let generateTable root =

        // Trace.WriteLine("Table generation")
        let rec processNode (node : TreeNode) (prefix : Coding) = 
            let right x = 

                let newValue = 
                    prefix.value
                    |> (<<<) 1
                    |> (|||) 1

                {meaningfulBits = prefix.meaningfulBits + 1; value = newValue  }
                |> processNode x 

            let left x = 
                {meaningfulBits = prefix.meaningfulBits + 1; value = prefix.value <<< 1}
                |> processNode x 

            // Trace.WriteLine(node)
            match (node.left, node.right) with
            | (None, None) -> [(node.key, prefix)] 
            | (None, Some x) -> right x                
            | (Some x, None) -> left x               
            | (Some x, Some y) ->
                (left x, right y)
                ||> List.append

        processNode root {meaningfulBits = 0; value = 0}
        |> Map.ofList

