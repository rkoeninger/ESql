module Trilogy.Analysis

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
        | Unnamed -> failwith "Shouldn't have Unnamed here"
        | Param _ -> failwith "Shouldn't have Param here"
    let analyzeId (id: Id) (sources: Sources) =
        match id with
        | Qualified(qualifier, id) -> sources |> List.find (fst >> (=) qualifier) |> snd |> analyzeIdForTable id
        | Named name -> [sources |> List.map snd |> List.concat |> List.filter (fst >> (=) name) |> single |> mapFst Some]
        | Unnamed -> failwith "Shouldn't have Unnamed here"
        | Param _ -> failwith "Shouldn't have Param here"
    let rec analyzeExpr expr =
        match expr with
        | ConstExpr typ -> [None, typ]
        | IdExpr id -> analyzeId id stmt.Sources
        | CastExpr(body, typ) -> [None, typ]
        | CountExpr _ -> [None, Int]
        | BinaryExpr(left, _, right) -> [None, Bit]
    let analyzeSelection = function
        | Unaliased expr -> analyzeExpr expr
        | Aliased(expr, name) -> [Some name, analyzeExpr expr |> single |> snd]
        | Star(Some table) -> stmt.Sources |> List.find (fst >> (=) table) |> snd |> List.map (mapFst Some)
        | Star _ -> stmt.Sources |> List.map snd |> List.concat |> List.map (mapFst Some)
    stmt.Selections |> List.map analyzeSelection |> List.concat

let singleType = Set.singleton >> Limits

let unionTypeBounds t0 t1 =
    match t0, t1 with
    | (_, Any | Any, _) -> Any
    | Limits xs, Limits ys -> Limits(Set.union xs ys)

let intersectTypeBounds t0 t1 =
    match t0, t1 with
    | (xs, Any | Any, xs) -> xs
    | Limits xs, Limits ys -> Limits(Set.intersect xs ys)

let merge f =
    let combine acc key value0 =
        match Map.tryFind key acc with
        | Some value1 -> Map.add key (f value0 value1) acc
        | None -> Map.add key value0 acc
    Map.fold combine

let rec inferType expr (sources: Sources) =
    match expr with
    | ConstExpr typ -> singleType typ
    | CastExpr(_, typ) -> singleType typ
    | CountExpr _ -> singleType Int
    | IdExpr(Named name) -> sources |> List.map snd |> List.concat |> List.filter (fst >> (=) name) |> single |> snd |> singleType
    | _ -> Any

let inferParameters (clause: WhereClause) : Parameters =
    let rec analyzeExpr expr =
        match expr with
        | BinaryExpr(IdExpr(Param name0), _, IdExpr(Param name1)) -> Map.ofList [name0, Any; name1, Any]
        | BinaryExpr(IdExpr(Param name), _, expr) -> Map.ofList [name, inferType expr clause.Sources]
        | BinaryExpr(expr, _, IdExpr(Param name)) -> Map.ofList [name, inferType expr clause.Sources]
        | BinaryExpr(expr0, Or, expr1) -> merge unionTypeBounds (analyzeExpr expr0) (analyzeExpr expr1)
        | BinaryExpr(expr0, _, expr1) -> merge intersectTypeBounds (analyzeExpr expr0) (analyzeExpr expr1)
        | _ -> Map.empty
    analyzeExpr clause.Condition

let sortStatements =
    let ord = function
        | CreateStatement _ -> 0
        | InsertStatement _ -> 1
        | UpdateStatement _ -> 2
        | DeleteStatement _ -> 3
        | SelectStatement _ -> 4
    List.sortBy (fun (x, y) -> compare (ord x) (ord y))

let infer tables parameters = function
    | CreateStatement create -> tables, parameters
    | InsertStatement insert -> tables, parameters
    | UpdateStatement update -> tables, parameters
    | DeleteStatement delete -> tables, parameters
    | SelectStatement select -> tables, parameters
