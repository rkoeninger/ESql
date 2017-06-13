[<AutoOpen>]
module SqlPen.Parser

open FParsec

let private pExpr, pExprRef = createParserForwardedToRef<SqlExpr, unit>()

let private pOperator =
    choice [
        pstring "=" >>. preturn Eq
        pstring ">" >>. preturn Gt
        pstring "<" >>. preturn Lt
        pstring "and" >>. preturn And
        pstring "or" >>. preturn Or
    ]

let private pType =
    choice [
        pstring "bit" >>. preturn Bit
        pstring "int" >>. preturn Int
        pstring "varchar" >>. preturn Varchar
        pstring "nvarchar" >>. preturn NVarchar
    ]

let private binary p0 pfill0 p1 f = tuple2 (p0 .>> pfill0) p1 |>> f

let private ternary p0 pfill0 p1 pfill1 p2 f = tuple3 (p0 .>> pfill0) (p1 .>> pfill1) p2 |>> f

let private pConst =
    choice [
        pfloat >>. preturn (ConstExpr Int)
        between (pstring "\'") (pstring "\'") (regex "\\w*") >>. preturn (ConstExpr Varchar)
    ]

let private pBinOp =
    ternary
        pExpr
        spaces1
        pOperator
        spaces1
        pExpr
        BinaryExpr

let private pCast =
    binary
        pExpr
        (spaces1 .>> pstring "as" .>> spaces1)
        pType
        CastExpr

do pExprRef := choice [pBinOp; pCast; pConst]

let private pWhere =
    pstring "where" >>. spaces1 >>.
    pExpr

let private pJoin =
    pstring "join" >>. spaces1 >>.
    regex "\\w+" .>> spaces1 .>>
    pstring "on" .>> spaces1 .>>
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
