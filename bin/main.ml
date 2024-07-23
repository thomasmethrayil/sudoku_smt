open Base
open Stdio
open Eio.Std
open Z3
open Z3.Arithmetic
open Z3.Solver

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
        |> chunk_to_array row_length

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
      Solver.add solver
        [
          make_cell_constraints unsolved_grid expr_grid index colIndex
            z3_context;
        ]
    done;
    Solver.add solver [ get_row_constraints expr_row z3_context ];
    Solver.add solver [ get_column_constraints expr_grid index z3_context ]
  done;
  Solver.add solver subgrid_constraints;
  let status = Solver.check solver [] in
  (status, Solver.get_model solver, expr_grid)

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

let load_puzzle_and_solve sudoku_string puzzle_number_opt =
  let puzzle_number =
    match puzzle_number_opt with Some num -> num | None -> 1
  in
  let parsed_sudoku_string = parse_sudoku_string sudoku_string puzzle_number in
  let subgrid_dimension =
    let grid_dim = Array.length parsed_sudoku_string in
    Float.sqrt (Int.to_float grid_dim) |> Int.of_float
  in
  let status, model_opt, expr_grid = solve_sudoku parsed_sudoku_string in
  printf "Initial puzzle for puzzle no %u: \n" puzzle_number;
  print_grid parsed_sudoku_string subgrid_dimension;
  match (status, model_opt) with
  | Solver.UNSATISFIABLE, _ ->
      Stdio.print_string "Model doesn't have a solution\n"
  | Solver.UNKNOWN, _ -> Stdio.print_string "Solver status unknown\n"
  | Solver.SATISFIABLE, None ->
      Stdio.print_string
        "Solver did not return an assignment despite the existence of \
         satisfying assignments\n"
  | Solver.SATISFIABLE, Some model ->
      Stdio.print_string "\n Model was satisfied \n\n";
      let solved_grid = get_solved_grid model expr_grid in
      print_grid solved_grid subgrid_dimension;;

let () =
  let filepath = "/Users/thomas/code/sudoku_smt/input/test_6" in
  let lines = In_channel.read_lines filepath |> List.mapi ~f:(fun i p -> (p, Some i)) in
  let main ~pool = 
  (Eio.Executor_pool.submit_exn pool ~weight:1.0
        (fun () -> Eio.Fiber.List.iter (fun (p, i_opt) -> load_puzzle_and_solve p i_opt) lines)) in

  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
    let pool =
      Eio.Executor_pool.create
      ~sw (Eio.Stdenv.domain_mgr env)
      ~domain_count:5 in
  main ~pool