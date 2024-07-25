open Base
open Stdio
open Eio

let () =
  let command =
    let module C = Command in
    C.basic
      ~summary:"Solve sudoku puzzles from file"
      ~readme:(fun () ->
        "This program takes in a compulsory filename argument eg. dune exec -- \
         sudoku_smt ./input/test_6 for a file containing 1 or more sudoku puzzles. The \
         puzzles can be encoded using '-' or '0' for empty cells. Please take care to \
         encode each puzzle in one line only. Do not create any empty lines between \
         puzzles.")
      C.Param.(
        map
          (anon ("filename" %: string))
          ~f:(fun filename () ->
            let parse_and_solve ~pool =
              let lines =
                In_channel.read_lines filename |> List.mapi ~f:(fun i p -> p, Some i)
              in
              Executor_pool.submit_exn pool ~weight:1.0 (fun () ->
                Fiber.List.iter
                  (fun (p, i_opt) ->
                    let open Solver in
                    load_puzzle_and_solve p i_opt)
                  lines)
            in
            let num_of_threads =
              match Sys.getenv "num_threads" with
              | Some num -> Int.of_string num
              | None -> Domain.recommended_domain_count ()
            in
            Eio_main.run
            @@ fun env ->
            Switch.run
            @@ fun sw ->
            let pool =
              Executor_pool.create
                ~sw
                (Stdenv.domain_mgr env)
                ~domain_count:num_of_threads
            in
            parse_and_solve ~pool))
  in
  Command_unix.run ~version:"1.0" ~build_info:"RWO" command
;;
