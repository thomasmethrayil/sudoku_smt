open Base
open Z3
open Z3.Arithmetic
open Z3.Solver

let parse_sudoku_string sudoku_string puzzle_number =
  let sanitized_string =
    sudoku_string |> String.to_list
    |> List.filter ~f:(fun c -> Char.is_digit c || Char.( = ) '-' c)
  in
  let row_length =
    Float.sqrt (Int.to_float (List.length sanitized_string)) |> Int.of_float
  in
  if not (List.length sanitized_string = row_length * row_length) then
    let err_str =
      Printf.sprintf
        "The given sudoku string in puzzle no : %u does not form a square grid"
        puzzle_number
    in
    failwith err_str
  else
    match
      ( List.find sanitized_string ~f:(fun c -> Char.( = ) '0' c),
        List.find sanitized_string ~f:(fun c -> Char.( = ) 'c' c) )
    with
    | Some _, Some _ ->
        let err_str =
          Printf.sprintf
            "Sudoku input contains both 0 and '-' to denote missing numbers in \
             puzzle no : %u. Please fix."
            puzzle_number
        in
        failwith err_str
    | _ ->
        sanitized_string
        |> List.map ~f:(fun c ->
               if Char.( = ) '0' c then None else Char.get_digit c)
        |> Auxiliary.chunk_to_array row_length

let initialize_expr_grid grid_dimension ctx =
  let position_name = Printf.sprintf "x_%d_%d" in
  let initialize_single_expr_row row_index =
    Array.init grid_dimension ~f:(fun col_index ->
        Integer.mk_const_s ctx (position_name row_index col_index))
  in
  Array.init grid_dimension ~f:(fun ri -> initialize_single_expr_row ri)

let make_cell_constraints grid expr_grid row_index col_index ctx =
  let current_position = expr_grid.(row_index).(col_index) in
  match grid.(row_index).(col_index) with
  | Some num ->
      Boolean.mk_eq ctx current_position (Integer.mk_numeral_i ctx num)
  | None ->
      let gt_0, le_9 =
        ( mk_gt ctx current_position (Integer.mk_numeral_i ctx 0),
          mk_le ctx current_position (Integer.mk_numeral_i ctx 9) )
      in
      Boolean.mk_and ctx [ gt_0; le_9 ]

let get_row_constraints expr_row ctx =
  expr_row |> Array.to_list |> Boolean.mk_distinct ctx

let get_column_constraints expr_grid col_index ctx =
  expr_grid
  |> Array.map ~f:(fun ea -> ea.(col_index))
  |> Array.to_list |> Boolean.mk_distinct ctx

let get_subgrid_coordinates grid_dim =
  let subgrid_length = Float.sqrt (Int.to_float grid_dim) |> Int.of_float in
  let subgrid_borders =
    List.init (subgrid_length + 1) ~f:(fun i -> i * subgrid_length)
  in
  let subgrid_corners =
    List.cartesian_product subgrid_borders subgrid_borders
  in
  let subgrid_squares =
    subgrid_corners
    |> List.map ~f:(fun (x, y) ->
           subgrid_corners
           |> List.fold ~init:[] ~f:(fun acc (x1, y1) ->
                  if x1 - x = subgrid_length && y1 - y = subgrid_length then
                    ((x, y), (x1, y1)) :: acc
                  else acc))
    |> List.filter ~f:(fun l -> List.is_empty l |> not)
    |> List.concat
  in
  let rows ((row_1st, _), (row_last, _)) =
    List.init (row_last - row_1st) ~f:(fun i -> row_1st + i)
  in
  let columns ((_, column_1st), (_, column_last)) =
    List.init (column_last - column_1st) ~f:(fun i -> column_1st + i)
  in
  subgrid_squares
  |> List.map ~f:(fun coords ->
         rows coords
         |> List.fold ~init:[] ~f:(fun acc x ->
                columns coords
                |> List.fold ~init:acc ~f:(fun acc y -> (x, y) :: acc)))

let get_subgrid_constraints expr_grid grid_length ctx =
  get_subgrid_coordinates grid_length
  |> List.map ~f:(fun subgrid ->
         subgrid |> List.map ~f:(fun (x, y) -> expr_grid.(x).(y)))
  |> List.map ~f:(fun expr_subgrid -> Boolean.mk_distinct ctx expr_subgrid)

let solve_sudoku unsolved_grid =
  let z3_context = mk_context [] in
  let grid_dimension = Array.length unsolved_grid in
  let expr_grid = initialize_expr_grid grid_dimension z3_context in
  let solver = mk_simple_solver z3_context in
  let subgrid_constraints =
    get_subgrid_constraints expr_grid grid_dimension z3_context
  in
  for index = 0 to grid_dimension - 1 do
    let expr_row = expr_grid.(index) in
    for colIndex = 0 to grid_dimension - 1 do
      add solver
        [
          make_cell_constraints unsolved_grid expr_grid index colIndex
            z3_context;
        ]
    done;
    add solver [ get_row_constraints expr_row z3_context ];
    add solver [ get_column_constraints expr_grid index z3_context ]
  done;
  add solver subgrid_constraints;
  let status = check solver [] in
  (status, get_model solver, expr_grid)

let get_solved_grid model expr_grid =
  expr_grid
  |> Array.map ~f:(fun expr_array ->
         expr_array
         |> Array.map ~f:(fun ex ->
                match Model.get_const_interp_e model ex with
                | Some interpretation ->
                    let interpreted_string =
                      Expr.to_string interpretation |> String.to_array
                    in
                    if Char.is_digit interpreted_string.(0) then
                      Char.get_digit interpreted_string.(0)
                    else None
                | None -> None))
