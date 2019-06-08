structure AST = struct

  datatype lam
    = Lambda (aexp aexp aexp ...) exp

  datatype aexp
    = Int n
    | Var x
    | True
    | False
    | (?prim? aexp ...)

  datatype cexp
    = Set aexp aexp
    | Call aexp
    | CC aexp
    | If aexp exp exp
    | Letrec ([aexp aexp] ...) exp
    | (aexp aexp ...)

  datatype exp
    = cexp
    | aexp
    | Let ([aexp exp]) exp
