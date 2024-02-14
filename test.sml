fun find_completion (num, num_list) = 
    let 
        fun find_completion_recursive(num, num_list, acc) = 
            case num_list of 
                [] => acc
                | x::[] => acc
                | head::neck::tail => 
                    if head + neck = num 
                    then find_completion_recursive(num, tail,  [head, neck] @ acc)
                    else find_completion_recursive(num, neck::tail, acc) @ find_completion_recursive(num, head::tail, acc) 
    in find_completion_recursive (num, num_list, [])
    end

