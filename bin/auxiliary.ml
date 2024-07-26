open Base
open Stdio

let chunk_to_array size list =
  let rec work remaining current_chunk_size current_chunk acc =
    match remaining with
    | [] ->
      List.rev
        (if List.is_empty current_chunk |> not then List.rev current_chunk :: acc else acc)
    | x :: xs ->
      if current_chunk_size = size - 1
      then work xs 0 [] (List.rev (x :: current_chunk) :: acc)
      else work xs (current_chunk_size + 1) (x :: current_chunk) acc
  in
  work list 0 [] [] |> List.map ~f:(fun l -> Array.of_list l) |> Array.of_list
;;

type row_to_print =
  { row : int option array
  ; print : bool
  }

let print_grid grid cell_dim =
  let grid_length = Array.length grid in
  let separator_length = cell_dim * grid_length in
  let separator_line = Array.init separator_length ~f:(fun _ -> '-') |> String.of_array in
  let print_row row_info =
    if row_info.print then printf "%s\n" separator_line;
    let print_number_or_blank s =
      match s with
      | Some num -> printf "%u " num
      | None -> printf "  "
    in
    row_info.row
    |> Array.iteri ~f:(fun i s ->
      if i > 0 && Int.rem i cell_dim = 0 then printf "| ";
      if i > 0 && Int.rem i cell_dim <> 0 then printf " ";
      print_number_or_blank s);
    printf "\n"
  in
  printf "%s\n" separator_line;
  grid
  |> Array.iteri ~f:(fun i r ->
    if i > 0 && Int.rem i cell_dim = 0 && grid_length <> i + 1
    then print_row { row = r; print = true }
    else print_row { row = r; print = false })
;;
