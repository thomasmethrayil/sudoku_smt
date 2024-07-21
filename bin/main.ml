open Base
open Stdio.Out_channel
open Z3
open Z3.Arithmetic
open Z3.Solver

let split_sudoku_rows sudoku_string = sudoku_string |> String.split ~on:(',') |> List.to_array;;

let split_row_to_int_cells row_string =
  Array.init (String.length row_string) ~f:(String.get row_string)
  |> Array.map ~f: (fun c -> if Char.is_digit c then Char.get_digit c else None);;

let split_rows_to_int_grid string_array = string_array |> Array.map ~f:(fun s -> split_row_to_int_cells s);;

let parse_sudoku_string sudoku_string = split_sudoku_rows sudoku_string |> split_rows_to_int_grid;;

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
    mk_gt ctx current_position (Integer.mk_numeral_i ctx 0), mk_le ctx current_position (Integer.mk_numeral_i ctx 9) in
    Boolean.mk_and ctx [gt_0; le_9];;

let get_row_constraints expr_row ctx =
  expr_row
  |> Array.to_list
  |> Boolean.mk_distinct ctx;;

let get_column_constraints expr_grid col_index ctx =
  expr_grid
  |> Array.map ~f:(fun ea -> ea.(col_index))
  |> Array.to_list
  |> Boolean.mk_distinct ctx;;

let get_subgrid_coordinates grid_dim =
  (* we're assuming perfect input from user *)
  let subgrid_length = Float.sqrt (Int.to_float grid_dim) |> Int.of_float in
  let subgrid_borders = List.init (subgrid_length + 1) ~f:(fun i -> i * subgrid_length) in
  let subgrid_corners = List.cartesian_product subgrid_borders subgrid_borders in
  let subgrid_squares =
    subgrid_corners
    |> List.map ~f:(fun (x,y) ->
      subgrid_corners
      |> List.fold ~init:([]) ~f:(fun acc (x1,y1) ->
        if (x1 - x = subgrid_length) && (y1 - y = subgrid_length) then ((x,y), (x1,y1))::acc else acc))
    |> List.filter ~f:(fun l -> (List.is_empty l) |> not)
    |> List.concat in
  let rows ((row_1st,_),(row_last, _)) = List.init (row_last - row_1st) ~f:(fun i -> row_1st + i) in
  let columns ((_,column_1st),(_, column_last)) = List.init (column_last - column_1st) ~f:(fun i -> column_1st + i) in
  subgrid_squares
  |> List.map ~f:(fun coords ->
    rows coords
    |> List.fold ~init:([]) ~f: (fun acc x ->
      columns coords
      |> List.fold ~init:(acc) ~f:(fun acc y -> (x,y)::acc)));;

let get_subgrid_constraints expr_grid grid_length ctx =
  get_subgrid_coordinates grid_length
  |> List.map ~f:(fun subgrid ->
    subgrid
    |> List.map ~f:(fun (x,y) -> expr_grid.(x).(y)))
  |> List.map ~f:(fun expr_subgrid -> Boolean.mk_distinct ctx expr_subgrid);;

let solve_sudoku unsolved_grid =
  let z3_context = mk_context [] in
  let grid_dimension = Array.length unsolved_grid in
  let expr_grid = initialize_expr_grid grid_dimension z3_context in
  let solver = mk_simple_solver z3_context in
  let subgrid_constraints = get_subgrid_constraints expr_grid grid_dimension z3_context in
  begin
    for index = 0 to (grid_dimension - 1) do
      let expr_row = expr_grid.(index) in
      for colIndex = 0 to (grid_dimension - 1) do
        Solver.add solver [make_cell_constraints unsolved_grid expr_grid index colIndex z3_context]
      done;
      Solver.add solver [get_row_constraints expr_row z3_context];
      Solver.add solver [get_column_constraints expr_grid index z3_context];
    done;
    Solver.add solver subgrid_constraints;
  end;
  begin
    let status = Solver.check solver [] in
    (status, Solver.get_model solver, expr_grid)
  end;;

let get_solved_grid model expr_grid =
  expr_grid
  |> Array.map ~f:(fun expr_array ->
    expr_array
    |> Array.map ~f:(fun ex ->
      match Model.get_const_interp_e model ex with
      | Some interpretation ->
        let interpreted_string = Expr.to_string interpretation |> String.to_array in
        if Char.is_digit interpreted_string.(0) then Char.get_digit interpreted_string.(0) else None
      | None -> None));;

let print_grid (grid : int option array array) (dim : int) =
  let print_row (row : int option array) =
    let print_number_or_blank s =
      match s with
      | Some num -> printf "%u " num;
      | None -> printf "  " in
    row |>
    Array.iteri ~f:(fun i s ->
      if i > 0 && (Int.rem (i+1) dim = 0) then  printf "|| ";
      if i > 0 && (Int.rem (i+1) dim <> 0) then printf "| ";
      print_number_or_blank s;
    );
    printf "\n" in
  let grid_length = Array.length grid in
  let separator_length = (dim * grid_length) + (dim - 1) - 1 in
  let print_separator_line =
    for _ = 0 to (separator_length - 1) do
      printf "-" done;
    printf "\n" in
  print_separator_line;
  grid
    |> Array.iteri ~f:(fun i r ->
      if i > 0 && (Int.rem (i + 1) dim = 0) && (grid_length <> (i+1)) then print_separator_line;
      print_row r;
    );;

let () =
  let test_hard = "8--------,--36-----,-7--9-2--,-5---7---,----457--,---1---3-,--1----68,--85---1-,-9----4--" in
  begin
    let parsed_sudoku_string = parse_sudoku_string test_hard in
    let subgrid_dimension =
      let grid_dim = Array.length parsed_sudoku_string in
      Float.sqrt (Int.to_float grid_dim) |> Int.of_float in
    let (status, model_opt, expr_grid) = solve_sudoku parsed_sudoku_string in
    begin
      print_grid parsed_sudoku_string subgrid_dimension
    end;
    match (status, model_opt) with
      | Solver.UNSATISFIABLE, _ -> Stdio.print_string "Model doesn't have a solution"
      | Solver.UNKNOWN, _ -> Stdio.print_string "Nothing";
      | Solver.SATISFIABLE, None -> Stdio.print_string "Nothing";
      | Solver.SATISFIABLE, Some model ->
        Stdio.print_string "Model was satisfied";
        let solved_grid = get_solved_grid model expr_grid in
        print_grid solved_grid subgrid_dimension;
  end
