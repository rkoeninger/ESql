module SqlPen.Parsing

open FParsec

let private pExpr, pExprRef = createParserForwardedToRef<SqlExpr, unit>()

let private binex ((l, op), r) = BinaryExpr(op, l, r)

let private ops = [
    "=",   Eq
    ">",   Gt
    "<",   Lt
    "and", And
    "or",  Or
]

let private pBinOp =
    (pExpr .>> spaces1) .>>.
    choice (List.map (fun (s, op) -> pstring s >>. preturn op) ops) .>>.
    (spaces1 >>. pExpr) |>>
    binex

do pExprRef := choice [pBinOp]

let private pWhere =
    pstring "where" >>. spaces1 >>.
    pExpr

let private pJoin =
    pstring "join" >>. spaces1 >>.
    regex "\\w+" .>> spaces1 .>>
    pstring "on" .>>
    pExpr

let private pFrom =
    pstring "from" >>. spaces1 >>.
    many (pJoin .>> spaces1)

let private pSelect =
    pstring "select" >>. spaces1 >>.
    many (pExpr .>> spaces1) .>>
    pFrom .>> spaces1 .>>
    pWhere

let parse s =
    match run pSelect s with
    | Success(result, _, _) -> result
    | Failure(error, _, _) -> failwith error
