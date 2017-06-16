module SqlPen.Parser

open System
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

let private isIdentifierChar ch = Char.IsLetter ch || Char.IsDigit ch || ch = '_' || ch = '.' || ch = '@'

let private pIdentifier =
    choice [
        stringReturn "*" Star
        manySatisfy isIdentifierChar |>> ident
    ]

let private binary p0 pfill0 p1 f = tuple2 (p0 .>> pfill0) p1 |>> f

let private ternary p0 pfill0 p1 pfill1 p2 f = tuple3 (p0 .>> pfill0) (p1 .>> pfill1) p2 |>> f

let private pConst =
    choice [
        pint32 >>. preturn (ConstExpr Int)
        between (pchar '\'') (pchar '\'') (manySatisfy ((<>) '\'')) >>. preturn (ConstExpr Varchar)
    ]

let private pBinOp = // TODO: including in pExpr causes infinite loop?
    ternary
        pExpr
        spaces1
        pOperator
        spaces1
        pExpr
        BinaryExpr

let private pCast = // TODO: including in pExpr causes infinite loop?
    binary
        pExpr
        (spaces1 .>> pstring "as" .>> spaces1)
        pType
        CastExpr

let private pParens = between (pchar '(') (pchar ')')

do pExprRef := choice [
    pParens pExpr
    //pBinOp
    //pCast
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
    many (spaces1 >>. pJoin)

// TODO: need to be able to have optional spaces around comma
let private pcomma = //spaces >>. pchar ',' >>. spaces
    pchar ','

let private pSelect =
    pstring "select" >>. spaces1 >>. sepBy pExpr pcomma

let private sel (exprs, ids) =
    let (first, rest) = ids
    {
        Expressions = exprs;
        Tables = List.map (fun x -> x.ToString()) (first :: rest);
        Filter = ConstExpr Int
    }

let private pSelectStatement =
    binary
        pSelect
        spaces1
        pFrom
        sel

let parse s =
    match run pSelectStatement s with
    | Success(result, _, _) -> result
    | Failure(error, _, _) -> failwith error
