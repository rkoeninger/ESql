module SqlPen.Analysis

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
        | CastExpr(body, typ) -> [None, typ]
        | CountExpr _ -> [None, Int]
        | BinaryExpr(left, _, right) -> [None, Bit]
    stmt.Selections |> List.map analyzeExpr |> List.concat

let singleType = Set.singleton >> Limits

let unifyTypeBounds m t0 t1 =
    match t0, t1 with
    | Any, Any -> Any
    | (xs, Any | Any, xs) ->
        match m with
        | Union -> Any
        | Intersection -> xs
    | Limits xs, Limits ys ->
        match m with
        | Union -> Limits(Set.union xs ys)
        | Intersection -> Limits(Set.intersect xs ys)

let merge m =
    let combine acc key value0 =
        match Map.tryFind key acc with
        | Some value1 -> Map.add key (unifyTypeBounds m value0 value1) acc
        | None -> Map.add key value0 acc
    Map.fold combine

let rec inferType expr (sources: Sources) =
    match expr with
    | ConstExpr typ -> singleType typ
    | CastExpr(_, typ) -> singleType typ
    | CountExpr _ -> singleType Int
    | AliasExpr(body, _) -> inferType body sources
    | IdExpr(Named name) -> sources |> List.map snd |> List.concat |> List.filter (fst >> (=) name) |> single |> snd |> singleType
    | _ -> Any

let inferParameters (clause: WhereClause) : Parameters =
    let rec analyzeExpr expr =
        match expr with
        | BinaryExpr(IdExpr(Param name0), _, IdExpr(Param name1)) -> Map.ofList [name0, Any; name1, Any]
        | BinaryExpr(IdExpr(Param name), _, expr) -> Map.ofList [name, inferType expr clause.Sources]
        | BinaryExpr(expr, _, IdExpr(Param name)) -> Map.ofList [name, inferType expr clause.Sources]
        | BinaryExpr(expr0, Or, expr1) -> merge Union (analyzeExpr expr0) (analyzeExpr expr1)
        | BinaryExpr(expr0, _, expr1) -> merge Intersection (analyzeExpr expr0) (analyzeExpr expr1)
        | _ -> Map.empty
    analyzeExpr clause.Condition
