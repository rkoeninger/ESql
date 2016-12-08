module Analysis

type SqlType =
    | Bit
    | Int
    | Varchar
    | NVarchar
    | Multi of Set<SqlType>
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
    | Param of string
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

type WhereClause = { Condition: SqlExpr; Sources: Sources }

type Parameters = Map<string, SqlType>

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
        | Param _ -> failwith "Shouldn't have Param here"
    let analyzeId (id: Id) (sources: Sources) =
        match id with
        | Qualified(qualifier, id) -> sources |> List.find (fst >> (=) qualifier) |> snd |> analyzeIdForTable id
        | Named name -> [sources |> List.map snd |> List.concat |> List.filter (fst >> (=) name) |> single |> mapFst Some]
        | Star -> sources |> List.map snd |> List.concat |> List.map (mapFst Some)
        | Unnamed -> failwith "Shouldn't have Unnamed here"
        | Param _ -> failwith "Shouldn't have Param here"
    let rec analyzeExpr expr =
        match expr with
        | ConstExpr typ -> [None, typ]
        | IdExpr id -> analyzeId id stmt.Sources
        | AliasExpr(body, id) -> [Some id, analyzeExpr body |> single |> snd]
        | CastExpr(body, typ) -> [analyzeExpr body |> single |> fst, typ]
        | CountExpr _ -> [None, Int]
        | BinaryExpr(_, left, right) -> [None, Bit]
    stmt.Selections |> List.map analyzeExpr |> List.concat

type Mode = Union | Intersection

let unifyScalarMulti m x ys =
    match m with
    | Union -> Multi(Set.add x ys)
    | Intersection -> if Set.contains x ys then x else Unknown

// TODO: distinguish between Unknown as in 'Top' and 'Bottom'

let unifyTypes m t0 t1 =
    let f =
        match m with
        | Union -> Set.union
        | Intersection -> Set.intersect
    match t0, t1 with
    | Unknown, Unknown -> Unknown
    | Multi xs, Multi ys -> Multi(f xs ys)
    | x, Unknown -> x
    | Unknown, y -> y
    //| Unknown, _ -> Unknown // TODO: this is what it looks like with Unknown := 'Top'
    //| _, Unknown -> Unknown
    | x, Multi ys -> unifyScalarMulti m x ys
    | Multi xs, y -> unifyScalarMulti m y xs
    | x, y when x = y -> x
    | x, y ->
        match m with
        | Union -> Multi(Set.ofList [x; y])
        | Intersection -> Unknown

let merge m =
    let combine acc key value0 =
        match Map.tryFind key acc with
        | Some value1 -> Map.add key (unifyTypes m value0 value1) acc
        | None -> Map.add key value0 acc
    Map.fold combine

let rec inferType expr (sources: Sources) =
    match expr with
    | ConstExpr typ -> typ
    | CastExpr(_, typ) -> typ
    | CountExpr _ -> Int
    | AliasExpr(body, _) -> inferType body sources
    | IdExpr(Named name) -> sources |> List.map snd |> List.concat |> List.filter (fst >> (=) name) |> single |> snd
    | _ -> failwith "Can't infer type"

let inferParameters (clause: WhereClause) : Parameters =
    let rec analyzeExpr expr =
        match expr with
        | BinaryExpr(_, IdExpr(Param name), expr) -> Map.ofList [name, inferType expr clause.Sources]
        | BinaryExpr(_, expr, IdExpr(Param name)) -> Map.ofList [name, inferType expr clause.Sources]
        | BinaryExpr(Or, expr0, expr1) -> merge Union (analyzeExpr expr0) (analyzeExpr expr1)
        | BinaryExpr(_, expr0, expr1) -> merge Intersection (analyzeExpr expr0) (analyzeExpr expr1)
        | _ -> Map.empty
    analyzeExpr clause.Condition
