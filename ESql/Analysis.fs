module Analysis

type SqlType =
    | Int
    | Varchar
    | NVarchar
    | Unknown

type Columns = (string * SqlType) list

type Id =
    | Qualified of string * Id
    | Named of string
    | Unnamed
    | Star

type Projected = (string option * SqlType) list

type SqlExpr =
    | ConstExpr of SqlType
    | IdExpr of Id
    | AliasExpr of SqlExpr * string
    | CastExpr of SqlExpr * SqlType
    | CountExpr of SqlExpr

type Sources = (string * Columns) list

type SelectStmt = { Projection: SqlExpr list; Sources: Sources }

let mapFst f (x, y) = (f x, y)

let single xs =
    match xs with
    | [x] -> x
    | _ -> failwith "Must be single item"

let analyze (stmt: SelectStmt) : Projected =
    let rec analyzeIdForTable id (cols: Columns) =
        match id with
        | Qualified _ -> failwith "Shouldn't have Qualified here"
        | Named name -> [cols |> List.find (fst >> (=) name) |> mapFst Some]
        | Star -> List.map (mapFst Some) cols
        | Unnamed -> failwith "Shouldn't have Unnamed here"
    let rec analyzeId (id: Id) (sources: Sources) =
        match id with
        | Qualified(qualifier, id) -> sources |> List.find (fst >> (=) qualifier) |> snd |> analyzeIdForTable id
        | Named name -> [sources |> List.map snd |> List.concat |> List.filter (fst >> (=) name) |> single |> mapFst Some]
        | Star -> sources |> List.map snd |> List.concat |> List.map (mapFst Some)
        | Unnamed -> failwith "Shouldn't have Unnamed here"
    let rec analyzeExpr expr =
        match expr with
        | ConstExpr typ -> [None, typ]
        | IdExpr id -> analyzeId id stmt.Sources
        | AliasExpr(body, id) -> [Some id, analyzeExpr body |> single |> snd]
        | CastExpr(body, typ) -> [analyzeExpr body |> single |> fst, typ]
        | CountExpr _ -> [None, Int]
    List.map analyzeExpr stmt.Projection |> List.concat
