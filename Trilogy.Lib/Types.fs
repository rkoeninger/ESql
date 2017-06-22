namespace Trilogy

type SqlType =
    | Bit
    | Int
    | Varchar
    | NVarchar

type SqlTypeBounds =
    | Any
    | Limits of Set<SqlType> // If set is empty, can't be anything

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
    override this.ToString() =
        match this with
        | Star -> "*"
        | Unnamed -> "_"
        | Param x -> sprintf "@%s" x
        | Named x -> x
        | Qualified(x, y) -> sprintf "%s.%O" x y

type Projection = (string option * SqlType) list

type SqlExpr =
    | ConstExpr of SqlType
    | IdExpr of Id
    //| AliasExpr of SqlExpr * string
    | CastExpr of SqlExpr * SqlType
    | CountExpr of SqlExpr
    | BinaryExpr of SqlExpr * Op * SqlExpr

type Sources = (string * Columns) list

type SelectStmt = {
    Selections: SqlExpr list
    Sources: Sources
}

type WhereClause = {
    Condition: SqlExpr
    Sources: Sources
}

type Parameters = Map<string, SqlTypeBounds>

type Mode = Union | Intersection

type Select = {
    Expressions: SqlExpr list
    Tables: string list
    Filter: SqlExpr
}

type Insert = {
    Table: string
    Columns: string list
    Values: SqlExpr list
}

type Update = {
    Table: string
    Assignments: (string * SqlExpr) list
    Filter: SqlExpr
}

type Delete = {
    Table: string
    Filter: SqlExpr
}

type CreateTable = {
    Name: string
    Columns: (string * SqlType) list
}

type Statement =
    | SelectStatement of Select
    | InsertStatement of Insert
    | UpdateStatement of Update
    | DeleteStatement of Delete
    | CreateStatement of CreateTable

type Batch = {
    Name: string
    Statements: Statement list
}
