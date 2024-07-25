open Base
open Stdio
open Eio

let load_puzzle_and_solve sudoku_string puzzle_number_opt =
  let module A = Auxiliary in
  let module S = Solver in
  let module Z = Z3.Solver in
  let puzzle_number =
    match puzzle_number_opt with Some num -> num | None -> 1
  in
  let parsed_sudoku_string =
    S.parse_sudoku_string sudoku_string puzzle_number
  in
  let subgrid_dimension =
    let grid_dim = Array.length parsed_sudoku_string in
    Float.sqrt (Int.to_float grid_dim) |> Int.of_float
  in
  let status, model_opt, expr_grid = S.solve_sudoku parsed_sudoku_string in
  printf "Initial puzzle for puzzle no %u: \n" puzzle_number;
  A.print_grid parsed_sudoku_string subgrid_dimension;
  match (status, model_opt) with
  | Z.UNSATISFIABLE, _ -> Stdio.print_string "Model doesn't have a solution\n"
  | Z.UNKNOWN, _ -> Stdio.print_string "Solver status unknown\n"
  | Z.SATISFIABLE, None ->
      Stdio.print_string
        "Solver did not return an assignment despite the existence of \
         satisfying assignments\n"
  | Z.SATISFIABLE, Some model ->
      Stdio.print_string "\n Model was satisfied \n\n";
      let solved_grid = S.get_solved_grid model expr_grid in
      A.print_grid solved_grid subgrid_dimension

let () =
  let run_event_pool filename =
    let parse_and_solve filename ~pool =
      let lines =
        In_channel.read_lines filename |> List.mapi ~f:(fun i p -> (p, Some i))
      in
      Executor_pool.submit_exn pool ~weight:1.0 (fun () ->
          Fiber.List.iter
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
      Executor_pool.create ~sw (Stdenv.domain_mgr env)
        ~domain_count:num_of_threads
    in
    parse_and_solve filename ~pool
  in
  let command =
    let module C = Command in
    C.basic ~summary:"Solve sudoku puzzles from file"
      ~readme:(fun () ->
        "This program takes in a compulsory filename argument eg. dune exec -- \
         sudoku_smt ./input/test_6 for a file containing 1 or more sudoku \
         puzzles. The puzzles can be encoded using '-' or '0' for empty cells. \
         Please take care to encode each puzzle in one line only. Do not \
         create any empty lines between puzzles.")
      C.Param.(
        map
          (anon ("filename" %: string))
          ~f:(fun filename () -> run_event_pool filename))
  in
  Command_unix.run ~version:"1.0" ~build_info:"RWO" command
