open Base
open Z3
open Z3.Arithmetic
open Z3.Solver

let parseString sudokuString =
  sudokuString
  |> String.split ~on:(',')
  |> List.to_array
  |> Array.map ~f:(fun s ->
      Array.init (String.length s) ~f:(String.get s)
        |> Array.map ~f: (fun c -> if Char.is_digit c then Some (Char.to_int c) else None));;

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

(* let makeDistinctConstraint coordinates ctx exprGrid =
  let shouldBeDifferent =
    coordinates
    |> List.map ~f:(fun (x,y) -> exprGrid.(x).(y)) in
  Boolean.mk_distinct ctx shouldBeDifferent;; *)

let getRowConstraints exprRow ctx =
  exprRow
  |> Array.to_list
  |> Boolean.mk_distinct ctx;;

let getColumnConstraints exprGrid colIndex ctx =
  exprGrid
  |> Array.map ~f:(fun ea -> ea.(colIndex))
  |> Array.to_list
  |> Boolean.mk_distinct ctx;;

let subgridConstraints subgrid ctx =
  subgrid
  |> Array.to_list
  |> Array.concat
  |> Array.to_list
  |> Boolean.mk_distinct ctx;;


let getSubgridCoordinates gridDim =
  let subgridLength = Helpers.sqrt gridDim in
  let rec range n =
    if n < 0 then []
    else n::range (n-1) in
  let rangeNumbers = range subgridLength in
  let subgridBorders =
    rangeNumbers
    |> List.map ~f:(fun num -> subgridLength * num) in
  let subgridCorners = Helpers.cartesian_product subgridBorders subgridBorders in
  ()
;;

let solveSudoku sudokuString =
  let z3Context = mk_context [] in
  let sudokuGrid = parseString sudokuString in
  let gridDimension = Array.length sudokuGrid in
  let exprGrid = initializeExprGrid gridDimension z3Context in
  let solver = mk_simple_solver z3Context in
  for index = 0 to (gridDimension - 1) do
    let exprRow = exprGrid.(index) in
    for colIndex = 0 to (gridDimension - 1) do
      Solver.add solver [makeCellConstraints sudokuGrid exprGrid index colIndex z3Context]
    done;
    Solver.add solver [getRowConstraints exprRow z3Context];
    Solver.add solver [getColumnConstraints exprGrid index z3Context];
  done;;


let testHard = "8--------,--36-----,-7--9-2--,-5---7---,----457--,---1---3-,--1----68,--85---1-,-9----4--";;

let _ = solveSudoku testHard;;
