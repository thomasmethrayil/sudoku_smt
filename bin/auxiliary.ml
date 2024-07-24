open Base
open Stdio

let chunk_to_array size list =
  let rec work remaining current_chunk_size current_chunk acc =
    match remaining with
    | [] ->
        List.rev
          (if List.is_empty current_chunk |> not then
             List.rev current_chunk :: acc
           else acc)
    | x :: xs ->
        if current_chunk_size = size - 1 then
          work xs 0 [] (List.rev (x :: current_chunk) :: acc)
        else work xs (current_chunk_size + 1) (x :: current_chunk) acc
  in
  work list 0 [] [] |> List.map ~f:(fun l -> Array.of_list l) |> Array.of_list

let print_grid grid dim =
  let print_row row =
    let print_number_or_blank s =
      match s with Some num -> printf "%u " num | None -> printf "  "
    in
    row
    |> Array.iteri ~f:(fun i s ->
           if i > 0 && Int.rem i dim = 0 then printf "|| ";
           if i > 0 && Int.rem i dim <> 0 then printf "| ";
           print_number_or_blank s);
    printf "\n"
  in
  let grid_length = Array.length grid in
  (*Length computation of separator is not of much use as the '-' character occupies much less space in the terminal unless we use Format's boxes for pretty printing*)
  let separator_length = (dim * grid_length) + (dim - 1) - 1 in
  let print_separator_line =
    for _ = 0 to separator_length - 1 do
      printf "-"
    done;
    printf "\n"
  in
  print_separator_line;
  grid
  |> Array.iteri ~f:(fun i r ->
         if i > 0 && Int.rem (i + 1) dim = 0 && grid_length <> i + 1 then
           print_separator_line;
         print_row r)
