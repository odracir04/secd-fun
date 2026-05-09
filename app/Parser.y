{
module Parser where

import Data.Char
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    var     { TokenVar $$ }
    int     { TokenInt $$ }
    if      { TokenIf }
    then    { TokenThen }
    else    { TokenElse }
    let     { TokenLet }
    in      { TokenIn }
    fix     { TokenFix }
    '\\'    { TokenLambda }
    '->'    { TokenArrow }
    '+'     { TokenPlus }
    '-'     { TokenMinus }
    '*'     { TokenTimes }
    '='     { TokenEqual }
    '!'     { TokenBang }
    '>'     { TokenGT }
    '=='    { TokenEQ }
    '>='    { TokenGTE }
    '&&'    { TokenAnd }
    '('     { TokenLB }
    ')'     { TokenRB }

%%

Term
    : '\\' var '->' Term                  { Lambda $2 $4 }
    | let var '=' Term in Term            { Let $2 $4 $6 }
    | if Term then Term else Term         { IfZero $2 $4 $6 }
    | fix Term                            { Fix $2 }
    | OrExpr                              { $1 }

OrExpr
    : OrExpr '&&' CmpExpr                 { And $1 $3 }
    | CmpExpr                             { $1 }

CmpExpr
    : AddExpr '>' AddExpr                 { Greater $1 $3 }
    | AddExpr '>=' AddExpr                { GreaterEq $1 $3 }
    | AddExpr '==' AddExpr                { Equal $1 $3 }
    | AddExpr                             { $1 }

AddExpr  
    : AddExpr '+' MulExpr                 { Add $1 $3 }
    | AddExpr '-' MulExpr                 { Sub $1 $3 }
    | MulExpr                             { $1 }

MulExpr
    : MulExpr '*' AppExpr                 { Mul $1 $3 }
    | AppExpr                             { $1 }

AppExpr       
    : AppExpr Atom                        { App $1 $2 }
    | Atom                                { $1 }

Atom          
    : '(' Term ')'                        { $2 }
    | int                                 { Const $1 }
    | var                                 { Var $1 }
    | '!' Atom                            { Not $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

type Ident = String

data Term
    = Var Ident
    | Lambda Ident Term
    | App Term Term
    | Const Int
    | IfZero Term Term Term
    | Let Ident Term Term
    | Fix Term
    | Add Term Term
    | Sub Term Term
    | Mul Term Term
    | Greater Term Term
    | GreaterEq Term Term
    | Equal Term Term
    | And Term Term
    | Not Term
    deriving Show

data Token = TokenVar Ident
            | TokenInt Int
            | TokenIf 
            | TokenThen
            | TokenElse
            | TokenLet  
            | TokenIn 
            | TokenFix 
            | TokenLambda
            | TokenArrow    
            | TokenPlus   
            | TokenMinus    
            | TokenTimes    
            | TokenEqual    
            | TokenBang   
            | TokenGT 
            | TokenEQ 
            | TokenGTE  
            | TokenAnd  
            | TokenLB 
            | TokenRB 
            deriving Show


lexer :: String -> [Token]
lexer [] = []
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('>':'=':cs) = TokenGTE : lexer cs
lexer ('&':'&':cs) = TokenAnd : lexer cs
lexer ('=':'=':cs) = TokenEQ : lexer cs
lexer ('\\':cs) = TokenLambda : lexer cs
lexer ('=':cs) = TokenEqual : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('!':cs) = TokenBang : lexer cs
lexer ('>':cs) = TokenGT : lexer cs
lexer ('(':cs) = TokenLB : lexer cs
lexer (')':cs) = TokenRB : lexer cs
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
      ("let",rest) -> TokenLet : lexer rest
      ("in",rest)  -> TokenIn : lexer rest
      ("fix",rest) -> TokenFix : lexer rest
      ("if", rest) -> TokenIf : lexer rest
      ("then", rest) -> TokenThen : lexer rest
      ("else", rest) -> TokenElse : lexer rest
      (var,rest)   -> TokenVar var : lexer rest

parser = parse . lexer

}
