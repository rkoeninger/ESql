module Trilogy.Parser

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

let private isShortIdentifierChar ch = Char.IsLetter ch || Char.IsDigit ch || ch = '_'

let private pShortIdentifier = manySatisfy isShortIdentifierChar

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

let private pParens p = between (pchar '(' .>> spaces) (spaces >>. pchar ')') p

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
    many (attempt (spaces1 >>. pJoin))

let private pComma = spaces >>. pchar ',' >>. spaces

let private pSelect = pstring "select" >>. spaces1 >>. sepBy1 pExpr (attempt pComma)

let private sel (exprs, ids, wh) =
    let (first, rest) = ids
    {
        Expressions = exprs;
        Tables = List.map (fun x -> x.ToString()) (first :: rest);
        Filter = wh
    }

let private sel2 (exprs, ids) =
    let (first, rest) = ids
    {
        Expressions = exprs;
        Tables = List.map (fun x -> x.ToString()) (first :: rest);
        Filter = ConstExpr Int
    }

let private pSelectStatement =
    choice [
        attempt
            (ternary
                pSelect
                spaces1
                pFrom
                spaces1
                pWhere
                sel)
        binary
            pSelect
            spaces1
            pFrom
            sel2
    ]

let private pInsertTable =
    pstring "insert" >>.
    spaces1 >>.
    pstring "into" >>.
    spaces1 >>.
    pShortIdentifier

let private pInsertColumns = pParens (sepBy1 pShortIdentifier (attempt pComma))

let private pInsertValues =
    pstring "values" >>.
    spaces >>.
    pParens (sepBy1 pExpr (attempt pComma))

let private ins (tbl, cols, vals) = {
    Table = tbl
    Columns = cols
    Values = vals
}

let private pInsertStatement =
    ternary
        pInsertTable
        spaces
        pInsertColumns
        spaces
        pInsertValues
        ins

let private pColumnDecl =
    binary
        pShortIdentifier
        spaces1
        pType
        id

let private ctable (name, cols) = { Name = name; Columns = cols }

let private pCreateTableStatement =
    pstring "create" >>.
    spaces1 >>.
    pstring "table" >>.
    spaces1 >>.
    (binary
        pShortIdentifier
        spaces
        (pParens (sepBy1 pColumnDecl (attempt pComma)))
        ctable)

let private pStatement =
    choice [
        pCreateTableStatement |>> CreateStatement
        pSelectStatement      |>> SelectStatement
        pInsertStatement      |>> InsertStatement
    ]

let parse s =
    match run pStatement s with
    | Success(result, _, _) -> result
    | Failure(error, _, _) -> failwith error
