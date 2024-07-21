open Base
open Z3
open Z3.Arithmetic
open Z3.Solver

type sudokuGrid =
  | Unsolved of int option array array
  | Solved of string array array;;

let splitSudokuRows sudokuString = sudokuString |> String.split ~on:(',') |> List.to_array;;

let splitRowIntCells rowString =
  Array.init (String.length rowString) ~f:(String.get rowString)
  |> Array.map ~f: (fun c -> if Char.is_digit c then Some (Char.to_int c) else None);;

(* let splitRowsToStringGrid rowString = String.to_array rowString |> Array.map ~f:(fun c -> String.of_char c);;*)

let splitRowsToIntGrid stringArray = stringArray |> Array.map ~f:(fun s -> splitRowIntCells s);;

let parseSudokuString sudokuString = splitSudokuRows sudokuString |> splitRowsToIntGrid;;

let initializeExprGrid gridDimension ctx =
  let positionName = Printf.sprintf "x_%d_%d" in
  let initializeSingleExprRow (rowIndex : int) =
    Array.init gridDimension ~f:(fun colIndex -> Integer.mk_const_s ctx (positionName rowIndex colIndex)) in
  Array.init gridDimension ~f:(fun ri -> initializeSingleExprRow ri);;

let makeCellConstraints grid exprGrid rowIndex colIndex ctx =
  let currentPosition = exprGrid.(rowIndex).(colIndex) in
  match grid.(rowIndex).(colIndex) with
  | Some num -> Boolean.mk_eq ctx currentPosition (Integer.mk_numeral_i ctx num)
  | None ->
    let gt0, le9 =
    mk_ge ctx currentPosition (Integer.mk_numeral_i ctx 0), mk_le ctx currentPosition (Integer.mk_numeral_i ctx 9) in
    Boolean.mk_and ctx [gt0; le9];;

let getRowConstraints exprRow ctx =
  exprRow
  |> Array.to_list
  |> Boolean.mk_distinct ctx;;

let getColumnConstraints exprGrid colIndex ctx =
  exprGrid
  |> Array.map ~f:(fun ea -> ea.(colIndex))
  |> Array.to_list
  |> Boolean.mk_distinct ctx;;

let getSubgridCoordinates gridDim =
  (* we're assuming perfect input from user *)
  let subgridLength = Float.sqrt (Int.to_float gridDim) |> Int.of_float in
  let subgridBorders = List.init (subgridLength + 1) ~f:(fun i -> i * subgridLength) in
  let subgridCorners = List.cartesian_product subgridBorders subgridBorders in
  let subgridSquares =
    subgridCorners
    |> List.map ~f:(fun (x,y) ->
      subgridCorners
      |> List.fold ~init:([]) ~f:(fun acc (x1,y1) ->
        if (x1 - x = subgridLength) && (y1 - y = subgridLength) then ((x,y), (x1,y1))::acc else acc))
    |> List.filter ~f:(fun l -> (List.is_empty l) |> not)
    |> List.concat in
  let rows ((row1st,_),(rowLast, _)) = List.init (rowLast - row1st) ~f:(fun i -> row1st + i) in
  let columns ((_,column1st),(_, columnLast)) = List.init (columnLast - column1st) ~f:(fun i -> column1st + i) in
  subgridSquares
  |> List.map ~f:(fun coords ->
    rows coords
    |> List.fold ~init:([]) ~f: (fun acc x ->
      columns coords
      |> List.fold ~init:(acc) ~f:(fun acc y -> (x,y)::acc)));;

let getSubgridConstraints exprGrid gridLength ctx =
  getSubgridCoordinates gridLength
  |> List.map ~f:(fun subgrid ->
    subgrid
    |> List.map ~f:(fun (x,y) -> exprGrid.(x).(y)))
  |> List.map ~f:(fun subExprGrid -> Boolean.mk_distinct ctx subExprGrid);;

let solveSudoku unsolvedGrid =
  let z3Context = mk_context [] in
  let gridDimension = Array.length unsolvedGrid in
  let exprGrid = initializeExprGrid gridDimension z3Context in
  let solver = mk_simple_solver z3Context in
  let subgridConstraints = getSubgridConstraints exprGrid gridDimension z3Context in
  begin
    for index = 0 to (gridDimension - 1) do
      let exprRow = exprGrid.(index) in
      for colIndex = 0 to (gridDimension - 1) do
        Solver.add solver [makeCellConstraints unsolvedGrid exprGrid index colIndex z3Context]
      done;
      Solver.add solver [getRowConstraints exprRow z3Context];
      Solver.add solver [getColumnConstraints exprGrid index z3Context];
    done;
    Solver.add solver subgridConstraints;
  end;
  begin
    let status = Solver.check solver [] in
    (status, Solver.get_model solver, exprGrid)
  end;;

let getSolvedGrid model exprGrid =
  exprGrid
  |> Array.map ~f:(fun exprArray ->
    exprArray
    |> Array.map ~f:(fun ex ->
      match Model.evaluate model ex false with
      | Some expr -> Expr.to_string expr
      | None -> ""
    ));;

let printGrid (grid : sudokuGrid) =
  match grid with
  | Unsolved _ ->
    ()
  | Solved _ -> ();;

let () =
  let testHard = "8--------,--36-----,-7--9-2--,-5---7---,----457--,---1---3-,--1----68,--85---1-,-9----4--" in
  begin
    let parsedSudokuString = parseSudokuString testHard in
    let unsolvedGrid = Unsolved (parsedSudokuString) in
    begin
      printGrid unsolvedGrid
    end;
    let (status, modelOpt, exprGrid) = solveSudoku parsedSudokuString in
    match (status, modelOpt) with
      | Solver.UNSATISFIABLE, _ -> Stdio.print_string "Model doesn't have a solution"
      | Solver.UNKNOWN, _ -> Stdio.print_string "Nothing";
      | Solver.SATISFIABLE, None -> Stdio.print_string "Nothing";
      | Solver.SATISFIABLE, Some model ->
        Stdio.print_string "Model was satisfied";
        let solvedGrid = Solved (getSolvedGrid model exprGrid) in
        printGrid solvedGrid;
  end
