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
        stringCIReturn "bit"      Bit
        stringCIReturn "int"      Int
        stringCIReturn "varchar"  Varchar
        stringCIReturn "nvarchar" NVarchar
    ]

let private pComma = spaces >>. pchar ',' >>. spaces

let private pAs = spaces1 >>. pstring "as" >>. spaces1

let rec private ident (x: string) =
    if x.StartsWith "@" then Param(x.Substring 1)
    elif x.Contains "." then
        let i = x.IndexOf "."
        Qualified(x.Substring(0, i), ident (x.Substring(i + 1, x.Length - i - 1)))
    else Named x

let private isIdentifierChar ch = Char.IsLetter ch || Char.IsDigit ch || ch = '_' || ch = '.' || ch = '@'

let private pIdentifier = manySatisfy isIdentifierChar |>> ident

let private isShortIdentifierChar ch = Char.IsLetter ch || Char.IsDigit ch || ch = '_'

let private pShortIdentifier = manySatisfy isShortIdentifierChar

let private binary p0 pfill0 p1 f = tuple2 (p0 .>> pfill0) p1 |>> f

let private ternary p0 pfill0 p1 pfill1 p2 f = tuple3 (p0 .>> pfill0) (p1 .>> pfill1) p2 |>> f

let private pParens p = between (pchar '(' .>> spaces) (spaces >>. pchar ')') p

let private pConstExpr =
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

let private pCountExpr =
    pstringCI "count"
    >>. spaces
    >>. pParens pExpr
    |>> CountExpr

let private pCastExpr =
    pstringCI "cast"
    >>. spaces
    >>. pParens (binary pExpr pComma pType CastExpr)

do pExprRef := choice [
    pParens pExpr
    pCountExpr
    pCastExpr
    //pBinOp
    pConstExpr
    pIdentifier |>> IdExpr
]

let private pWhere =
    pstringCI "where" >>. spaces1 >>.
    pExpr

let private pJoin =
    pstringCI "join" >>. spaces1 >>.
    pIdentifier .>> spaces1 .>>
    pstringCI "on" .>> spaces1 .>>
    pExpr

let private pFrom =
    (pstringCI "from" >>. spaces1 >>. pIdentifier) .>>.
    many (attempt (spaces1 >>. pJoin))

let private pSelection =
    choice [
        attempt (binary pExpr pAs pShortIdentifier Aliased)
        pExpr |>> Unaliased
    ]

let private pSelect = pstringCI "select" >>. spaces1 >>. sepBy1 pSelection (attempt pComma)

let private pSelectStatement =
    choice [
        attempt
            (ternary
                pSelect
                spaces1
                pFrom
                spaces1
                pWhere
                (fun (exprs, ids, wh) ->
                  let (first, rest) = ids
                  SelectStatement {
                    Selections = exprs;
                    Tables = List.map (fun x -> x.ToString()) (first :: rest);
                    Filter = wh
                  }))
        binary
            pSelect
            spaces1
            pFrom
            (fun (exprs, ids) ->
              let (first, rest) = ids
              SelectStatement {
                Selections = exprs;
                Tables = List.map (fun x -> x.ToString()) (first :: rest);
                Filter = ConstExpr Int
              })
    ]

let private pInsertTable =
    pstringCI "insert" >>.
    spaces1 >>.
    pstringCI "into" >>.
    spaces1 >>.
    pShortIdentifier

let private pInsertColumns = pParens (sepBy1 pShortIdentifier (attempt pComma))

let private pInsertValues =
    pstringCI "values" >>.
    spaces >>.
    pParens (sepBy1 pExpr (attempt pComma))

let private pInsertStatement =
    ternary
        pInsertTable
        spaces
        pInsertColumns
        spaces
        pInsertValues
        (fun (tbl, cols, vals) ->
          InsertStatement {
            Table = tbl
            Columns = cols
            Values = vals
          })

let private pUpdateTable =
    pstringCI "update" >>.
    spaces1 >>.
    pShortIdentifier

let private pUpdateAssign =
    binary
        pShortIdentifier
        (attempt (spaces >>. pstring "=" >>. spaces))
        pExpr
        id

let private pSet =
    pstringCI "set" >>.
    spaces1 >>.
    sepBy pUpdateAssign (attempt pComma)

let private pUpdateStatement = 
    choice [
        attempt
            (ternary
                pUpdateTable
                spaces1
                pSet
                spaces1
                pWhere
                (fun (tbl, set, filter) ->
                  UpdateStatement {
                    Table = tbl
                    Assignments = set
                    Filter = filter
                  }))
        binary
            pUpdateTable
            spaces1
            pSet
            (fun (tbl, set) ->
              UpdateStatement {
                Table = tbl
                Assignments = set
                Filter = ConstExpr Int
              })
    ]

let private pDeleteTable =
    pstringCI "delete" >>.
    spaces1 >>.
    pstringCI "from" >>.
    spaces1 >>.
    pShortIdentifier

let private pDeleteStatement =
    binary
        pDeleteTable
        spaces1
        pWhere
        (fun (tbl, filter) ->
          DeleteStatement {
            Table = tbl
            Filter = filter
          })

let private pColumnDecl =
    binary
        pShortIdentifier
        spaces1
        pType
        id

let private pCreateStatement =
    pstringCI "create" >>.
    spaces1 >>.
    pstringCI "table" >>.
    spaces1 >>.
    (binary
        pShortIdentifier
        spaces
        (pParens (sepBy1 pColumnDecl (attempt pComma)))
        (fun (name, cols) ->
          CreateStatement {
            Name = name
            Columns = cols
          }))

let private pStatement =
    choice [
        pSelectStatement
        pInsertStatement
        pUpdateStatement
        pDeleteStatement
        pCreateStatement
    ]

let private pStatements = sepBy pStatement spaces1

let private runParser p s =
    match run p s with
    | Success(result, _, _) -> result
    | Failure(error, _, _) -> failwith error

let parse = runParser pStatement

let parseAll = runParser pStatements
