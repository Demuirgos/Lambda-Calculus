type cat = 
    | TUnit  : cat 
    | TArrow : cat -> cat -> cat
type var  = int 
type expr =
    | EUnit : expr
    | ELam  : cat -> expr -> expr
    | EVar  : var -> expr
    | EApp  : expr -> expr -> expr

type step : expr -> expr -> cat = 
    | Beta : 
        t : cat -> e0: expr-> e1: expr-> 
        step (EApp (ELam t e0) e1) (subt (sub_beta e2) e1)
    | AppL :
        e0 : expr -> e1 : expr -> e0': expr ->
        hst: step e0 e0' -> tep (EApp e0 e1) (EApp e0' e1)
    | AppR :
        e0 : expr -> e1 : expr -> e1': expr ->
        hst: step e1 e1' -> step (EApp e0 e1) (EApp e0 e1')

type steps : expr -> expr -> cat = 
    | Single : 
        #e0 : expr  ->
        #e1 : expr  ->
        step e0 e1  ->
        steps e0 e1
    | Mupltiple :
        #e0 : expr  ->
        #e1 : expr  ->
        #e2 : expr  ->
        step e0 e1  ->
        steps e0 e1 ->
        steps e0 e2

let sub0 = var -> expr  