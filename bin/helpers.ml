open Base;;

let map_tailrec f lst =
  let rec aux lst k =
    match lst with 
    | [] -> k []
    | x::xs -> aux xs (fun i -> k (f x :: i))
  in
  aux lst Fn.id;;

let rec sqrt n =
  if n < 2 then
    1
  else
    let half = n / 2 in
    let lower_sqrt = sqrt (half) in
    let upper_sqrt = sqrt (n - half * half) in
    if lower_sqrt * lower_sqrt > n then
      upper_sqrt + 1
    else
      lower_sqrt;;

let cartesian_product list_1 list_2 =
  list_1 |> List.fold ~init:([]) ~f:(fun x a -> 
    list_2 |> List.fold ~init:(x) ~f:(fun y b ->(a,b)::y))
  |> List.rev;;
  