module SECD where

import Data.List (elemIndex)
import Data.Map (Map, size, insert, empty, lookup)
import Parser(Term(..))

data Instr = LD Int
            | LDC Int
            | LDF Code
            | LDRF Code
            | AP
            | RTN
            | SEL Code Code
            | JOIN
            | ADD
            | SUB
            | MUL
            | AND
            | EQL
            | GTH
            | GTE
            | NOT
            | HALT
            deriving Show

type Ident = String
            
instance Show Value where
    show (Int x) = show x
    show (Addr a) = "SECD Address: " ++ show a

type Closure = (Code, Env)
type Addr = Int

data Value = Int Int
            | Addr Addr

type Stack = [Value]
type Env = [Value]
type Code = [Instr]
type Dump = [(Stack, Env, Code)]
type Store = Map Addr Closure

type SECD = (Stack, Env, Code, Dump, Store)

type Symtable = [Ident]

extend :: Symtable -> Ident -> Symtable
extend sym x = x : sym

next :: Store -> Addr
next sto = size sto + 1

compile :: Term -> Symtable -> Code
compile (Var x) sym = case elemIndex x sym of Just var -> [LD var]
                                              Nothing -> error ("Undeclared variable: " ++ show x)
compile (Lambda x c) sym = [LDF (compile c sym' ++ [RTN])]
                           where sym' = extend sym x
compile (App e1 e2) sym = compile e1 sym ++ compile e2 sym ++ [AP]
compile (Const x) _ = [LDC x]
compile (IfZero b e1 e2) sym =  let c1 = compile e1 sym ++ [JOIN]
                                    c2 = compile e2 sym ++ [JOIN]
                                    in compile b sym ++ [SEL c1 c2]
compile (Let x e1 e2) sym = compile (App (Lambda x e2) e1) sym
compile (Fix (Lambda f (Lambda x e))) sym = [LDRF (compile e sym' ++ [RTN])]
                                            where sym' = extend (extend sym f) x
compile (Add e1 e2) sym = compile e1 sym ++ compile e2 sym ++ [ADD]
compile (Sub e1 e2) sym = compile e1 sym ++ compile e2 sym ++ [SUB]
compile (Mul e1 e2) sym = compile e1 sym ++ compile e2 sym ++ [MUL]
compile (Greater e1 e2) sym = compile e1 sym ++ compile e2 sym ++ [GTH]
compile (GreaterEq e1 e2) sym = compile e1 sym ++ compile e2 sym ++ [GTE]
compile (Equal e1 e2) sym = compile e1 sym ++ compile e2 sym ++ [EQL]
compile (And e1 e2) sym = compile e1 sym ++ compile e2 sym ++ [AND]
compile (Not e) sym = compile e sym ++ [NOT]
compile t _ = error ("Invalid term: " ++ show t)

step :: SECD -> SECD
step (s, e, LD x:c, d, m) = (var:s, e, c, d, m)
                                where var = e !! x
step (s, e, LDC x:c, d, m) = (Int x:s, e, c, d, m)
step (s, e, LDF c':c, d, m) = (Addr a:s, e, c, d, m')
                                where a = next m
                                      m' = insert a (c', e) m
step (s, e, LDRF c':c, d, m) = (Addr a:s, e, c, d, m')
                                where a = next m
                                      m' = insert a (c', Addr a:e) m
step (v:(Addr a):s, e, AP:c, d, m) 
    = case Data.Map.lookup a m of Just (c', e') -> ([], v:e', c', (s,e,c):d, m)
                                  Nothing -> error ("Function does not exist at address " ++ show a)
step (v:_, _, RTN:_, (s', e', c'):d, m) = (v:s', e', c', d, m)
step ((Int b):s, e, SEL c1 c2:c, d, m) = if b == 0
                                         then (s, e, c1, ([], [], c):d, m)
                                         else (s, e, c2, ([], [], c):d, m)
step (s, e, JOIN:_, (_, _, c'):d, m) = (s, e, c', d, m)
step ((Int v1):(Int v2):s, e, ADD:c, d, m) = (Int(v1+v2):s, e, c, d, m)
step ((Int v1):(Int v2):s, e, SUB:c, d, m) = (Int(v2-v1):s, e, c, d, m)
step ((Int v1):(Int v2):s, e, MUL:c, d, m) = (Int(v1*v2):s, e, c, d, m)
step ((Int v1):(Int v2):s, e, AND:c, d, m) = (v:s, e, c, d, m)
                                              where v = if v1 == 0 && v2 == 0 then Int 0 else Int 1
step ((Int v1):(Int v2):s, e, EQL:c, d, m) = (v:s, e, c, d, m)
                                              where v = if v1 == v2 then Int 0 else Int 1
step ((Int v1):(Int v2):s, e, GTH:c, d, m) = (v:s, e, c, d, m)
                                              where v = if v1 > v2 then Int 0 else Int 1
step ((Int v1):(Int v2):s, e, GTE:c, d, m) = (v:s, e, c, d, m)
                                              where v = if v1 >= v2 then Int 0 else Int 1
step ((Int v1):s, e, NOT:c, d, m) = (v:s, e, c, d, m)
                                     where v = if v1 == 0 then Int 1 else Int 0
step (s, e, HALT:c, d, m) = (s, e, c, d, m)
step (s, _, c, _, _) = error ("Invalid execution: \nStack: " ++ show s ++ "\nCode: " ++ show c)

execute :: SECD -> Value
execute (v:_, _, [], _, _) = v
execute secd = execute (step secd)

run :: Term -> Value
run t = execute ([], [], compile t [] ++ [HALT], [], empty)
