open System
//TYPE DECLARATIONS
type Round =
  | OPENR
  | CLOSER
  
type Curly =
  | OPENC
  | CLOSEC

type Keyword =
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO

type RelationalOP =
  | GREATER
  | SMALLER

type BooleanOP =
  | AND
  | OR

type ArithmeticOP =
  | PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE

type Token =
  | INTEGER of int
  | VARIABLE of string
  | TRUE
  | FALSE
  | DECLARE
  | SEMICOLON
  | ROUND of Round
  | CURLY of Curly
  | KEYWORD of Keyword
  | RELATIONAL of RelationalOP
  | BOOLEAN of BooleanOP
  | ARITHMETIC of ArithmeticOP
//COMPOUND TYPES
type ArithmeticExpr =
  | VAR of string
  | INT of int
  | ARITHEXPR of ArithmeticExpr * ArithmeticOP * ArithmeticExpr
  | PARENSARITH of ArithmeticExpr
type BooleanExpr =
  | TRUECONST
  | FALSECONST
  | BOOLEXPR of BooleanExpr * BooleanOP * BooleanExpr
  | COMPEXPR of ArithmeticExpr * RelationalOP * ArithmeticExpr
  | PARENSBOOL of BooleanExpr
type Statement =
  | DECLARATION of string * ArithmeticExpr
  | IFTHENELSE of BooleanExpr * Statement * Statement
  | WHILEDO of BooleanExpr * Statement
  | SEQUENCE of Statement * Statement
//MONADIC STUFF
type Parser<'a, 's> = 's -> Result<'a, 's>
and Result<'a, 's> =
  | Done of 'a*'s
  | Fail

let ret x = fun s -> Done(x, s)
let bind p k =
  fun s ->
    match p s with
    | Done(a, s') -> k a s'
    | Fail -> Fail

type ParserBuilder () =
  member this.Return(x) = ret x
  member this.ReturnFrom(x) = x
  member this.Bind(p,k) = bind p k
let prs = ParserBuilder()

let doFail =
  fun s -> Fail


let (.||) x y =
  fun s ->
    match x s with
    | Done(a, s') -> Done(a, s')
    | Fail ->
      match y s with
      | Done(a, s') -> Done(a, s')
      | Fail -> Fail

let getHead =
  fun s ->
    match s with
    | h::t -> Done(h, t)
    | _ -> Fail

let getEOF =
  fun s ->
    match s with
    | [] -> Done((), [])
    | _ -> Fail

let rec repeat p =
  prs{
    let! h = p
    let! t = repeat p
    return h::t
  } .||
  prs{
    return []
  }

let repeatAtLeastOnce p =
  prs{
    let! h = p
    let! t = repeat p
    return h::t
  }

let rec checkMultiple cs =
  match cs with
  | [] -> prs{return ()}
  | h::t ->
    prs{
      let! next = getHead
      if next = h then
        return! checkMultiple t
      else
        return! doFail
    }

let checkAlphanum =
  prs{
    let! next = getHead
    if next >= 'a' && next <= 'z' || next >= 'A' && next <= 'Z' then
      return next
    else
      return! doFail
  }

let checkNumeric =
  prs{
    let! next = getHead
    if next >= '0' && next <= '9' then
      return next
    else
      return! doFail 
  }

let readVariable =
  prs{
    let! result = repeatAtLeastOnce checkAlphanum
    return VARIABLE(List.fold (fun s x -> s + x.ToString()) "" result)
  }

let readInteger =
  prs{
    let! result = repeatAtLeastOnce checkNumeric
    return INTEGER(System.Int32.Parse(List.fold (fun acc elem -> acc + (elem.ToString())) "" result))
  }

let readSemi =
  prs{
    let! result = getHead
    if result = ';' then
      return SEMICOLON
    else
      return! doFail
  }

let readRound =
  prs{
    let! result = getHead
    if result = '(' then
      return ROUND(OPENR)
    else if result = ')' then
      return ROUND(CLOSER)
    else
      return! doFail
  }

let readCurly =
  prs{
    let! result = getHead
    if result = '{' then
      return CURLY(OPENC)
    else if result = '}' then
      return CURLY(CLOSEC)
    else
      return! doFail
  }

let readRelational =
  prs{
    let! result = getHead
    if result = '>' then
      return RELATIONAL(GREATER)
    else if result = '<' then
      return RELATIONAL(SMALLER)
    else
      return! doFail
  }

let readArithmetic =
  prs{
    let! result = getHead
    if result = '+' then
      return ARITHMETIC(PLUS)
    else if result = '-' then
      return ARITHMETIC(MINUS)
    else if result = '*' then
      return ARITHMETIC(MULTIPLY)
    else if result = '/' then
      return ARITHMETIC(DIVIDE)
    else
      return! doFail
  }

let readCompound charlist compound =
  prs{
    do! checkMultiple charlist
    return compound
  }

let whiteSpace =
  prs{
    let! result = getHead
    if result = ' ' || result = '\n' || result = '\r' || result = '\t' then
      return ()
    else
      return! doFail
  }

let removeWhiteSpace =
  prs{
    let! _ = repeat whiteSpace
    return ()
  }

let rec Lexer () : Parser<Token list, char list> =
  prs{
    do! removeWhiteSpace
    let! head =
      readInteger .||
      readVariable .||
      readArithmetic .||
      readRelational .||
      readSemi .||
      readCurly .||
      readRound .||
      readCompound ['t';'r';'u';'e'] TRUE .||
      readCompound ['f';'a';'l';'s';'e'] FALSE .||
      readCompound [':';'='] DECLARE .||
      readCompound ['i';'f'] (KEYWORD(IF)) .||
      readCompound ['t';'h';'e';'n'] (KEYWORD(THEN)) .||
      readCompound ['e';'l';'s';'e'] (KEYWORD(ELSE)) .||
      readCompound ['w';'h';'i';'l';'e'] (KEYWORD(WHILE)) .||
      readCompound ['d';'o'] (KEYWORD(DO)) .||
      readCompound ['a';'n';'d'] (BOOLEAN(AND)) .||
      readCompound ['o';'r'] (BOOLEAN(OR))
    let! tail = Lexer ()
    return head::tail
  } .||
  prs{
    do! getEOF
    return []
  }

//PARSER
let rec ParseArithmetic tokens =
  match tokens with
  | VARIABLE(value)::t ->
    match t with
    | ARITHMETIC(op)::t ->
      let A2, t' = ParseArithmetic t
      ARITHEXPR(VAR(value), op, A2), t'
    | _ -> VAR(value), t
  | INTEGER(value)::t ->
    match t with
    | ARITHMETIC(op)::t ->
      let A2, t' = ParseArithmetic t
      ARITHEXPR(INT(value), op, A2), t'
    | _ -> INT(value), t
  | ROUND(OPENR)::t ->
    let A, t' = ParseArithmetic t
    match t' with
    | ROUND(CLOSER)::t ->
      PARENSARITH(A), t
    | _ -> failwith ""
  | _ -> failwith ""

let rec ParseBoolean tokens =
  match tokens with
  | TRUE::t -> TRUECONST, t
  | FALSE::t -> FALSECONST, t
  | ROUND(OPENR)::t ->
    let B, t' = ParseBoolean t
    match t' with
    | ROUND(CLOSER)::t ->
      PARENSBOOL(B), t
    | _ -> failwith ""
  | VARIABLE(value)::t ->
    match t with
    | RELATIONAL(op)::t ->
      let B2, t' = ParseArithmetic t
      let comp = COMPEXPR(VAR(value), op, B2)
      match t' with
      | BOOLEAN(op)::t ->
        let B2, t'' = ParseBoolean t
        BOOLEXPR(comp, op, B2), t'
      | _ -> comp, t'
    | _ -> failwith ""
  | INTEGER(value)::t ->
    match t with
    | RELATIONAL(op)::t ->
      let B2, t' = ParseArithmetic t
      let comp = COMPEXPR(INT(value), op, B2)
      match t' with
      | BOOLEAN(op)::t ->
        let B2, t' = ParseBoolean t
        BOOLEXPR(comp, op, B2), t'
      | _ -> comp, t'
    | _ -> failwith ""
  | _ -> failwith ""

let rec ParseStatement tokens =
  match tokens with
  | VARIABLE(var)::t ->
    match t with
    | DECLARE::t ->
      let arith, t' = ParseArithmetic t
      match t' with
      | SEMICOLON::t ->
        let NEXTSTMT, t' = ParseStatement t
        SEQUENCE(DECLARATION(var, arith), NEXTSTMT), t'
      | _ -> DECLARATION(var, arith), []
    | _ -> failwith ""
  | KEYWORD(IF)::t ->
    let bool, t' = ParseBoolean t
    match t' with
    | KEYWORD(THEN)::t ->
      match t with
      | CURLY(OPENC)::t ->
        let S1, t' = ParseStatement t
        match t' with
        | CURLY(CLOSEC)::t ->
          match t with
          | KEYWORD(ELSE)::t ->
            match t with
            | CURLY(OPENC)::t ->
              let S2, t' = ParseStatement t
              match t' with
              | CURLY(CLOSEC)::t ->
                match t with
                | SEMICOLON::t -> let NEXTSTMT, t' = ParseStatement t
                                  SEQUENCE(IFTHENELSE(bool, S1, S2), NEXTSTMT), t'
                | [] -> IFTHENELSE(bool, S1, S2), []
                | _ -> failwith ""
              | _ -> failwith ""
            | _ -> failwith ""
          | _ -> failwith ""
        | _ -> failwith ""
      | _ -> failwith ""
    | _ -> failwith ""
  | KEYWORD(WHILE)::t ->
    let bool, t' = ParseBoolean t
    match t' with
    | KEYWORD(DO)::t ->
      match t with
      | CURLY(OPENC)::t ->
        let body, t' = ParseStatement t
        match t' with
        | CURLY(CLOSEC)::t ->
          match t with
          | SEMICOLON::t ->
            let NEXTSTMT, t' = ParseStatement t
            SEQUENCE(WHILEDO(bool, body), NEXTSTMT), t'
          | [] -> WHILEDO(bool, body), []
          | _ -> failwith ""
        | _ -> failwith ""
      | _ -> failwith ""
    | _ -> failwith ""
  | _ -> failwith ""

//lexing the statements
let IN = Console.In
let tokenList = match Lexer() (IN.ReadToEnd() |> List.ofSeq) with
                | Fail -> failwith "you royally fucked up"
                | Done(tokens, charlist) -> tokens
//parsing the statements
let statement, tokens = ParseStatement tokenList 
//evaluating the statements

printfn "%A" statement