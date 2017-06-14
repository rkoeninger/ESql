module SqlPen.Parser

open FParsec

let private pExpr, pExprRef = createParserForwardedToRef<SqlExpr, unit>()

let private pOperator =
    choice [
        stringReturn "="   Eq
        stringReturn ">"   Gt
        stringReturn "<"   Lt
        stringReturn "and" And
        stringReturn "or"  Or
    ]

let private pType =
    choice [
        stringReturn "bit"      Bit
        stringReturn "int"      Int
        stringReturn "varchar"  Varchar
        stringReturn "nvarchar" NVarchar
    ]

let rec private ident (x: string) =
    if x.StartsWith "@" then Param(x.Substring 1)
    elif x.Contains "." then
        let i = x.IndexOf "."
        Qualified(x.Substring(0, i), ident (x.Substring(i + 1, x.Length - i - 1)))
    else Named x

let private pIdentifier =
    choice [
        stringReturn "*" Star
        manySatisfy (fun ch -> ch = ' ' || ch = '\n' || ch = '\r') |>> ident
    ]

let private binary p0 pfill0 p1 f = tuple2 (p0 .>> pfill0) p1 |>> f

let private ternary p0 pfill0 p1 pfill1 p2 f = tuple3 (p0 .>> pfill0) (p1 .>> pfill1) p2 |>> f

let private pConst =
    choice [
        pint32 >>. preturn (ConstExpr Int)
        between (pchar '\'') (pchar '\'') (manySatisfy ((<>) '\'')) >>. preturn (ConstExpr Varchar)
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

let private pParens = between (pchar '(') (pchar ')')

do pExprRef := choice [
    pParens pExpr
    pBinOp
    pCast
    pConst
    pIdentifier |>> IdExpr
]

let private pWhere =
    pstring "where" >>. spaces1 >>.
    pExpr

let private pJoin =
    pstring "join" >>. spaces1 >>.
    pIdentifier .>> spaces1 .>>
    pstring "on" .>> spaces1 .>>
    pExpr

let private pFrom =
    (pstring "from" >>. spaces1 >>. pIdentifier) .>>.
    many (pJoin .>> spaces1)

let private pSelect =
    stringReturn "select" {Selections=[]; Sources=[]}
    //pstring "select" >>. spaces1 >>. sepBy pExpr (spaces .>> pchar ',' .>> spaces)
    ////many1 (pExpr .>> spaces .>> pchar ',' .>> spaces)// .>>
    ////opt pFrom .>> spaces1 .>>
    ////opt pWhere

let parse s =
    match run pSelect s with
    | Success(result, _, _) -> result
    | Failure(error, _, _) -> failwith error
