open Base
open Z3
open Z3.Arithmetic
open Z3.Solver

let parse_string sudoku_string =
  sudoku_string
  |> String.split ~on:(',')
  |> List.to_array
  |> Array.map ~f:(fun s ->
      Array.init (String.length s) ~f:(String.get s)
        |> Array.map ~f: (fun c -> if Char.is_digit c then Some (Char.to_int c) else None));;

let initialize_expr_grid grid_dimension ctx =
  let position_name = Printf.sprintf "x_%d_%d" in
  let initialize_single_expr_row (row_index : int) =
    Array.init grid_dimension ~f:(fun col_index -> Integer.mk_const_s ctx (position_name row_index col_index)) in
  Array.init grid_dimension ~f:(fun ri -> initialize_single_expr_row ri);;

let make_cell_constraints grid expr_grid row_index col_index ctx =
  let current_position = expr_grid.(row_index).(col_index) in
  match grid.(row_index).(col_index) with
  | Some num -> Boolean.mk_eq ctx current_position (Integer.mk_numeral_i ctx num)
  | None ->
    let gt_0, le_9 = 
    mk_ge ctx current_position (Integer.mk_numeral_i ctx 0), mk_le ctx current_position (Integer.mk_numeral_i ctx 9) in
    Boolean.mk_and ctx [gt_0; le_9];;

(* let make_distinct_constraint coordinates ctx expr_grid =
  let should_be_different =
    coordinates
    |> List.map ~f:(fun (x,y) -> expr_grid.(x).(y)) in
  Boolean.mk_distinct ctx should_be_different;; *)

let get_row_constraints expr_row ctx =
  expr_row
  |> Array.to_list
  |> Boolean.mk_distinct ctx;;

let get_column_constraints expr_grid col_index ctx =
  expr_grid
  |> Array.map ~f:(fun ea -> ea.(col_index))
  |> Array.to_list
  |> Boolean.mk_distinct ctx;;

let subgrid_constraints subgrid ctx =
  subgrid
  |> Array.to_list
  |> Array.concat
  |> Array.to_list
  |> Boolean.mk_distinct ctx;;


let get_subgrid_coordinates grid_dim =
  let subgrid_length = Helpers.sqrt grid_dim in
  let rec range n =
    if n < 0 then []
    else n::range (n-1) in
  let range_numbers = range subgrid_length in
  let subgrid_borders =
    range_numbers
    |> List.map ~f:(fun num -> subgrid_length * num) in
  let subgrid_corners = Helpers.cartesian_product subgrid_borders subgrid_borders in
  ()
;;

let solve_sudoku sudoku_string =
  let z3_context = mk_context [] in
  let sudoku_grid = parse_string sudoku_string in
  let grid_dimension = Array.length sudoku_grid in
  let expr_grid = initialize_expr_grid grid_dimension z3_context in
  let solver = mk_simple_solver z3_context in
  for index = 0 to (grid_dimension - 1) do
    let expr_row = expr_grid.(index) in
    for col_index = 0 to (grid_dimension - 1) do
      Solver.add solver [make_cell_constraints sudoku_grid expr_grid index col_index z3_context]
    done;
    Solver.add solver [get_row_constraints expr_row z3_context];
    Solver.add solver [get_column_constraints expr_grid index z3_context];
  done;;


let testHard = "8--------,--36-----,-7--9-2--,-5---7---,----457--,---1---3-,--1----68,--85---1-,-9----4--";;

let _ =solve_sudoku testHard;;
