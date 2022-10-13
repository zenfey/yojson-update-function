let update key f (json : Yojson.Basic.json) =
    let rec update_json_obj = function
        | [] ->
            begin match f None with
                | None -> []
                | Some v -> [(key, v)]
            end
        | ((k, v) as m) :: tl ->
            if k = key then
                match f (Some v) with
                | None -> update_json_obj tl
                | Some v' ->
                    if v' == v then m :: tl
                    else (k, v') :: tl
            else m :: (update_json_obj tl)
    in

    match json with
    | `Assoc obj -> `Assoc (update_json_obj obj)
    | _ -> json
