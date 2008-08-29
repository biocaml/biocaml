type t = int * int * int
    
let to_rgb_string (r,g,b) = String.concat "," (List.map string_of_int [r;g;b])
  
let assert_well_formed ((r,g,b) as c) =
  let pred v = v >= 0 && v <= 255 in
    if not (pred r && pred g && pred b) then
      failwith ("invalid RGB color " ^ (to_rgb_string c))
        
let of_rgb r g b = let c = (r,g,b) in assert_well_formed c; c
  
let black = 0,0,0
let yale_blue = 14,76,146
