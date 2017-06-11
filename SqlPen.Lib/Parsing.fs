module SqlPen.Parsing

open FParsec

let private pExpr, pExprRef = createParserForwardedToRef<SqlExpr, unit>()

let private pBinOp =
    (pExpr .>> spaces) // .>>.
    //choice [pstring "+"; pstring "-"] .>>.
    //(spaces >>. pExpr)

do pExprRef := choice [pBinOp]

let private pWhere =
    pstring "where" >>.
    spaces >>.
    pExpr

let private pJoin =
    pstring "join" >>.
    spaces >>.
    regex "\\w+" .>>
    spaces .>>
    pstring "on" .>>
    pExpr

let private pFrom =
    pstring "from" >>.
    spaces >>.
    many (pJoin .>> spaces)

let private pSelect =
    pstring "select" >>.
    spaces >>.
    many (pExpr .>> spaces) .>>
    pFrom .>>
    spaces .>>
    pWhere

let parse s =
    match run pSelect s with
    | Success(result, _, _) -> result
    | Failure(error, _, _) -> failwith error
