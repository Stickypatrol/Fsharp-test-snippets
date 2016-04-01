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

type ArithmeticOP1 =
  | MULTIPLY
  | DIVIDE

type ArithmeticOP2 =
  | PLUS
  | MINUS

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
  | ARITHMETIC1 of ArithmeticOP1
  | ARITHMETIC2 of ArithmeticOP2
  | END
//COMPOUND TYPES
type ArithmeticExpr =
  | VAR of string
  | INT of int
  | ARITHEXPR1 of ArithmeticExpr * ArithmeticOP1 * ArithmeticExpr
  | ARITHEXPR2 of ArithmeticExpr * ArithmeticOP2 * ArithmeticExpr
  | PARENSARITH of ArithmeticExpr
type BooleanExpr =
  | TRUECONST//
  | FALSECONST//
  | BOOLEXPR of BooleanExpr * BooleanOP * BooleanExpr///
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
  member this.Zero() = ret ()
let prs = ParserBuilder()

let doFail =
  fun s -> Fail

let willSucceed i =
  fun s ->
    match i s with
    | Done(_, _) -> Done(true, s)
    | Fail -> Done(false, s)

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

let returnHead x =
  fun s ->
    Done((), x::s)

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

let readArithmetic1 =
  prs{
    let! result = getHead
    if result = '*' then
      return ARITHMETIC1(MULTIPLY)
    else if result = '/' then
      return ARITHMETIC1(DIVIDE)
    else
      return! doFail
  }
  
let readArithmetic2 =
  prs{
    let! result = getHead
    if result = '+' then
      return ARITHMETIC2(PLUS)
    else if result = '-' then
      return ARITHMETIC2(MINUS)
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
      readArithmetic1 .||
      readArithmetic2 .||
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
      readCompound ['o';'r'] (BOOLEAN(OR)) .||
      readVariable
    let! tail = Lexer ()
    return head::tail
  } .||
  prs{
    do! getEOF
    return []
  }

//PARSER
let checkToken token =
  prs{
    let! x = getHead
    if x = token then
      return ()
    else
      return! doFail
  }

let getVarValue =
  prs{
    let! x = getHead
    match x with
    | VARIABLE(x) -> return x
    | _ -> return! doFail
  }

let getIntValue =
  prs{
    let! x = getHead
    match x with
    | INTEGER(x) -> return x
    | _ -> return! doFail
  }

let matchRoundValue round =
  prs{
    let! x = getHead
    match x with
    | ROUND(x) -> if x = round then return () else return! doFail
    | _ -> return! doFail
  }

let matchCurlyvalue curly =
  prs{
    let! x = getHead
    match x with
    | CURLY(x) -> if x = curly then return () else return! doFail
    | _ -> return! doFail
  }

let matchKWValue keyw =
  prs{
    let! x = getHead
    match x with
    | KEYWORD(x) -> if x = keyw then return () else return! doFail
    | _ -> return! doFail
  }

let getRelValue =
  prs{
    let! x = getHead
    match x with
    | RELATIONAL(x) -> return x
    | _ -> return! doFail
  }

let getBoolValue =
  prs{
    let! x = getHead
    match x with
    | BOOLEAN(x) -> return x
    | _ -> return! doFail
  }
  
let getArith1Value =
  prs{
    let! x = getHead
    match x with
    | ARITHMETIC1(x) -> return x
    | _ -> return! doFail
  }

let getArith2Value =
  prs{
    let! x = getHead
    match x with
    | ARITHMETIC2(x) -> return x
    | _ -> return! doFail
  }
let printstate =
      fun s ->
        printfn "STATE IS NOW(((%A)))" s
        Done((), s)  
let rec ParseArithmetic() : Parser<ArithmeticExpr, Token list> =
  prs{
    let attachPlus =
      fun s ->
        let x = ARITHMETIC2(PLUS)
        Done((), x::s)
    let rec checkMultipleOpens() = //make this a function that calls itself when open else order1
      prs{
        let! n = getHead
        match n with
        | ROUND(OPENR)    ->  let! a = checkMultipleOpens()
                              let! a' = order2(a)
                              return a'
        | VARIABLE(value) ->  let! a = order2(VAR(value))
                              return a
        | INTEGER(value)  ->  let! a = order2(INT(value))
                              return a
        | _ ->  failwith ""
                return INT(0)//just to let it compile
      }
    and order2 (a:ArithmeticExpr) = //PLUS should be correct
      prs{
        let! n = getHead //OPERATOR or CLOSE
        match n with
        | ARITHMETIC2(op) ->
          let! m = getHead
          match m with
          | VARIABLE(value) ->  let! right = order2 (VAR(value))
                                return ARITHEXPR2(a, op, right)
          | INTEGER(value)  ->  let! right = order2 (INT(value))
                                return ARITHEXPR2(a, op, right)
          | ROUND(OPENR)    ->  let! a' = checkMultipleOpens()
                                let! right = order2 a'
                                return ARITHEXPR2(a, op, right)
          | _ ->  failwith ""
                  return INT(0)
        | ARITHMETIC1(op) -> //MULTI or DIVIDE
          do! returnHead n
          let! left = order1(a)
          let! a' = order2(left)
          return a'
        | ROUND(CLOSER) ->
          return a //here we return because we can then put it in A and continue
        | _ ->
          do! returnHead n
          return a 
      } .||
      prs{
        return a
      }
    and order1 (a:ArithmeticExpr) = //MULTI DIVIDE - CHANGE TO MULTIPLE DIVIDE SO 1
      prs{
        let! n = getHead //OPERATOR or CLOSE
        match n with
        | ARITHMETIC1(op) ->
          let! m = getHead
          match m with
          | VARIABLE(value) ->  let! right = order1 (VAR(value))
                                return ARITHEXPR1(a, op, right)
          | INTEGER(value)  ->  let! right = order1 (INT(value))
                                return ARITHEXPR1(a, op, right)
          | ROUND(OPENR)    ->  let! a' = checkMultipleOpens()
                                let! right = order2 a'
                                return ARITHEXPR1(a, op, right)
          | _ ->  failwith ""
                  return INT(0)
        | ARITHMETIC2(op) -> //MULTI or DIVIDE
          do! returnHead n
          return a
        | ROUND(CLOSER) ->
          return a //here we return because we can then put it in A and continue
        | _ ->
          do! returnHead n
          return a
      } .||
      prs{
        return a
      }
    do! attachPlus
    let! start = order2(INT(0))
    return start
  }
let rec ParseBoolean() =
  prs{//true
    do! checkToken TRUE
    let! boolvalue = willSucceed (getBoolValue)
    if boolvalue then
      let! b = getBoolValue
      let! B2 = ParseBoolean()
      return BOOLEXPR(TRUECONST, b, B2)
    else
      return TRUECONST
  }.||
  prs{//false
    do! checkToken FALSE
    let! boolvalue = willSucceed (getBoolValue)
    if boolvalue then
      let! b = getBoolValue
      let! B2 = ParseBoolean()
      return BOOLEXPR(FALSECONST, b, B2)
    else
      return FALSECONST
  }.||
  prs{//parensbool
    do! matchRoundValue OPENR
    let! B1 = ParseBoolean()
    do! matchRoundValue CLOSER
    let! boolvalue = willSucceed (getBoolValue)
    if boolvalue then
      let! b = getBoolValue
      let! B2 = ParseBoolean()
      return BOOLEXPR(PARENSBOOL(B1), b, B2)
    else
      return PARENSBOOL(B1)
  }.||
  prs{//compexpr
    let! A1 = ParseArithmetic()
    let! rel = getRelValue
    let! A2 = ParseArithmetic()
    let x =
      prs{
        let! b = getBoolValue
        let! B2 = ParseBoolean()
        return BOOLEXPR(COMPEXPR(A1, rel, A2), b, B2)
      } .||
      prs{
        return COMPEXPR(A1, rel, A2)
      }
    let! next = x
    return next
  }


let rec ParseStatement() =
  let main() = 
    prs{//declaration
      let! var = getVarValue
      do! checkToken DECLARE
      let! expr = ParseArithmetic()
      return DECLARATION(var, expr)
    }.||
    prs{//if then else
      do! matchKWValue IF
      do! matchRoundValue OPENR
      let! b = ParseBoolean()
      do! matchKWValue THEN
      do! matchCurlyvalue OPENC
      let! S1 = ParseStatement()
      do! matchCurlyvalue CLOSEC
      do! matchKWValue ELSE
      do! matchCurlyvalue OPENC
      let! S2 = ParseStatement()
      do! matchCurlyvalue CLOSEC
      return IFTHENELSE(b, S1, S2)
    }.||
    prs{//while do
      do! matchKWValue WHILE
      do! matchRoundValue OPENR
      let! b = ParseBoolean()
      do! matchKWValue DO
      do! matchCurlyvalue OPENC
      let! S1 = ParseStatement()
      do! matchCurlyvalue CLOSEC
      return WHILEDO(b, S1)
    }
  prs{
    let! S1 = main()
    let x = prs{
              do! checkToken SEMICOLON
              let! S2 = ParseStatement()
              return SEQUENCE(S1, S2)
            }.||
            prs{
              return S1
            }
    let! cont = x
    return cont
  }
//EVALUATION
let rec WhileOP bool stmt decl =
  if EvalBool bool decl then
    let decl' = EvalStatement stmt decl
    WhileOP bool stmt decl'
  else
    decl

and EvalArithmetic arit decl =
  match arit with
  | VAR(value) -> Map.find value decl
  | INT(value) -> value
  | ARITHEXPR1(expr1, op, expr2) ->
    match op with
    | MULTIPLY  ->  let l = (EvalArithmetic expr1 decl)
                    let r = (EvalArithmetic expr2 decl)
                    l*r
    | DIVIDE    ->  let l = (EvalArithmetic expr1 decl)
                    let r = (EvalArithmetic expr2 decl)
                    l/r
  | ARITHEXPR2(expr1, op, expr2) ->
    match op with
    | PLUS      -> (EvalArithmetic expr1 decl) + (EvalArithmetic expr2 decl)
    | MINUS     -> (EvalArithmetic expr1 decl) - (EvalArithmetic expr2 decl)
  | PARENSARITH(expr) -> (EvalArithmetic expr decl)

and EvalBool bool (decl:Map<string, int>) =
  match bool with
  | TRUECONST -> true
  | FALSECONST -> false
  | BOOLEXPR(b1, op, b2) ->
    match op with
    | AND -> (EvalBool b1 decl) && (EvalBool b2 decl)
    | OR  -> (EvalBool b1 decl) || (EvalBool b2 decl)
  | PARENSBOOL(b) -> EvalBool b decl
  | COMPEXPR(a1, op, a2) ->
    match op with
    | GREATER ->  let a1' = (EvalArithmetic a1 decl)
                  let a2' = (EvalArithmetic a2 decl)
                  a1' > a2'
    | SMALLER -> (EvalArithmetic a1 decl) < (EvalArithmetic a2 decl)

and EvalStatement stmt decl : Map<string, int> =
  match stmt with
  | DECLARATION(variable, expr) ->
    let exprResult = EvalArithmetic expr decl
    match Map.tryFind variable decl with
    | Some(var) ->  Map.add variable exprResult decl
    | None      ->  Map.add variable exprResult decl
  | SEQUENCE(S1, S2) -> let decl' = EvalStatement S1 decl
                        EvalStatement S2 decl'
  | IFTHENELSE(boolexpr, S1, S2) -> if EvalBool boolexpr decl then
                                      EvalStatement S1 decl
                                    else
                                      EvalStatement S2 decl
  | WHILEDO(boolexpr, S) -> WhileOP boolexpr S decl

//lexing the statements
let IN = Console.In
let tokenList = match Lexer() (IN.ReadToEnd() |> List.ofSeq) with
                | Fail -> failwith "you royally fucked up"
                | Done(tokens, charlist) -> tokens
//parsing the statements
let statement, tokens = match ParseStatement() tokenList with
                        | Done(stmt, tokens) -> stmt, tokens
                        | Fail -> failwith "failed at the parsing"
printfn "%A" statement
//evaluating the statements
let finalDeclarations = EvalStatement statement Map.empty
let unsorted = Map.fold (fun list key value -> key.ToString()::list) [] finalDeclarations
let sorted = List.sort unsorted
List.iter (fun x -> match Map.tryFind x finalDeclarations with
                    | Some(value) -> printfn "%s %i" x value
                    | None -> failwith "sorted/unsorted list is messed up") sorted