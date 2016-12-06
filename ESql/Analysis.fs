module Analysis

type SqlType =
    | Bit
    | Int
    | Varchar
    | NVarchar
    | Unknown

type Op =
    | Eq
    | Gt
    | Lt
    | And
    | Or

type Columns = (string * SqlType) list

type Id =
    | Qualified of string * Id
    | Named of string
    | Unnamed
    | Star

type Projection = (string option * SqlType) list

type SqlExpr =
    | ConstExpr of SqlType
    | IdExpr of Id
    | AliasExpr of SqlExpr * string
    | CastExpr of SqlExpr * SqlType
    | CountExpr of SqlExpr
    | BinaryExpr of Op * SqlExpr * SqlExpr

type Sources = (string * Columns) list

type SelectStmt = { Selections: SqlExpr list; Sources: Sources }

let mapFst f (x, y) = (f x, y)

let single xs =
    match xs with
    | [x] -> x
    | _ -> failwith "Must be single item"

let inferProjection (stmt: SelectStmt) : Projection =
    let analyzeIdForTable id (cols: Columns) =
        match id with
        | Qualified _ -> failwith "Shouldn't have Qualified here"
        | Named name -> [cols |> List.find (fst >> (=) name) |> mapFst Some]
        | Star -> List.map (mapFst Some) cols
        | Unnamed -> failwith "Shouldn't have Unnamed here"
    let analyzeId (id: Id) (sources: Sources) =
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
        | BinaryExpr(_, left, right) -> [None, Bit]
    stmt.Selections |> List.map analyzeExpr |> List.concat

type WhereClause = { Condition: SqlExpr; Sources: Sources }

type Parameters = (string * SqlType) list

let inferQualifiedIdType name (cols: Columns) : SqlType =
    cols |> List.find (fst >> (=) name) |> snd

let inferIdType id (sources: Sources) : SqlType =
    match id with
    | Unnamed -> failwith "Shouldn't have Unnamed here"
    | Star -> failwith "Shouldn't have Star here"
    | Qualified(qualifier, Named id) -> sources |> List.find (fst >> (=) qualifier) |> snd |> inferQualifiedIdType id
    | Qualified _ -> failwith "Qualified should have single name here"
    | Named name -> sources |> List.map snd |> List.concat |> List.filter (fst >> (=) name) |> single |> snd

let rec inferType expr (sources: Sources) : Parameters * SqlType =
    match expr with
    | IdExpr id -> [], inferIdType id sources
    | ConstExpr typ -> [], typ
    | AliasExpr(body, _) -> inferType body sources
    | CastExpr(_, typ) -> [], typ
    | CountExpr _ -> [], Int
    | BinaryExpr(_, _, _) -> [], Bit

let inferParameters (clause: WhereClause) : Parameters =
    let rec analyzeExpr expr =
        match expr with
        | IdExpr id -> [], inferIdType id clause.Sources
        | ConstExpr typ -> [], typ
        | AliasExpr(body, _) -> inferType body clause.Sources
        | CastExpr(_, typ) -> [], typ
        | CountExpr _ -> [], Int
        | BinaryExpr(_, left, right) ->
            let l = analyzeExpr left
            let r = analyzeExpr right
            [], Bit
    analyzeExpr clause.Condition |> fst
