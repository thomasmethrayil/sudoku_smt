open Base
open Solver
open Stdio
open Eio.Std
open Z3.Solver

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
  Auxiliary.print_grid parsed_sudoku_string subgrid_dimension;
  match (status, model_opt) with
  | UNSATISFIABLE, _ -> Stdio.print_string "Model doesn't have a solution\n"
  | UNKNOWN, _ -> Stdio.print_string "Solver status unknown\n"
  | SATISFIABLE, None ->
      Stdio.print_string
        "Solver did not return an assignment despite the existence of \
         satisfying assignments\n"
  | SATISFIABLE, Some model ->
      Stdio.print_string "\n Model was satisfied \n\n";
      let solved_grid = get_solved_grid model expr_grid in
      Auxiliary.print_grid solved_grid subgrid_dimension

let () =
  let filepath = "/Users/thomas/code/sudoku_smt/input/test_6" in
  let lines =
    In_channel.read_lines filepath |> List.mapi ~f:(fun i p -> (p, Some i))
  in
  let main ~pool =
    Eio.Executor_pool.submit_exn pool ~weight:1.0 (fun () ->
        Eio.Fiber.List.iter
          (fun (p, i_opt) -> load_puzzle_and_solve p i_opt)
          lines)
  in
  let num_of_threads =
    match Sys.getenv "num_threads" with
    | Some num -> Int.of_string num
    | None -> Domain.recommended_domain_count ()
  in
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let pool =
    Eio.Executor_pool.create ~sw
      (Eio.Stdenv.domain_mgr env)
      ~domain_count:num_of_threads
  in
  main ~pool
