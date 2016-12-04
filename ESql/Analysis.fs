module Analysis

type SqlType =
    | Int
    | Varchar
    | NVarchar
    | Unknown

type Columns = (string * SqlType) list

type Id =
    | Named of string
    | Unnamed
    | Star

type Projected = (string option * SqlType) list

type SqlExpr =
    | IdExpr of Id
    | AliasExpr of SqlExpr * string
    | CastExpr of SqlExpr * SqlType
    | CountExpr of SqlExpr

type SelectStmt = { Projection: SqlExpr list; Source: Columns }

// TODO: qualified names

let mapFst f (x, y) = (f x, y)

let single xs =
    match xs with
    | [x] -> x
    | _ -> failwith "Must be single item"

let analyze (stmt: SelectStmt) : Projected =
    let rec analyzeExpr expr =
        match expr with
        | IdExpr(Named id) -> [List.find (fst >> (=) id) stmt.Source |> mapFst Some]
        | IdExpr(Star) -> List.map (mapFst Some) stmt.Source
        | IdExpr(Unnamed) -> failwith "Shouldn't have Unnamed here"
        | AliasExpr(body, id) -> [Some id, analyzeExpr body |> single |> snd]
        | CastExpr(body, typ) -> [analyzeExpr body |> single |> fst, typ]
        | CountExpr _ -> [None, Int]
    List.map analyzeExpr stmt.Projection |> List.concat
