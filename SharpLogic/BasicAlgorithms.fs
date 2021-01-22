namespace SharpLogic

module BasicAlgorithms =

    let rec cartList nll = 
        let f0 n nll =  
            match nll with
            | [] -> [[n]]
            | _ -> List.map (fun nl->n::nl) nll
        match nll with
        | [] -> []
        | h::t -> List.collect (fun n->f0 n (cartList t)) h
