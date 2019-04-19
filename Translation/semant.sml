signature SEMANT =
sig
  val semant : Ast.ProgItem list -> unit
end 

structure Semant :> SEMANT =
struct

  structure A = Ast
  structure E = Env

  exception Undefined_Variable of string 
  exception Undefined_Function of string 
  exception ExistingFunction 
  exception ExistingVariable
  exception Error of string

  val enter = Symbol.enter 
  val look  = Symbol.look  

  fun prec Types.VOID = 0
    | prec Types.CHAR = 1
    | prec Types.INT = 2
    | prec Types.LONG = 3
    | prec Types.FLOAT = 4
    | prec Types.DOUBLE = 5
    | prec _ = 0

  fun deconsParam (A.ParamsCons (prim, dec) :: xs) = (prim, dec) :: (deconsParam xs) 
    | deconsParam _ = []    

  fun getPrimType (A.INT) = Types.INT 
    | getPrimType (A.VOID) = Types.VOID 
    | getPrimType (A.FLOAT) = Types.FLOAT
    | getPrimType (A.CHAR) = Types.CHAR
    | getPrimType (A.DOUBLE) = Types.DOUBLE
    | getPrimType (A.LONG) = Types.LONG 

  fun checkVar env x = 
    case (look (env, x)) of 
      SOME (E.VarEntry {ty = ty}) => ty  
    | _ => raise Undefined_Variable "undefined variable name"

  fun checkFun env x = 
    case (look (env, x)) of 
      SOME (E.FuncEntry {formals= formals, result = res}) => (formals, res) 
    | _ => raise Undefined_Function "undefined function name"

  fun isDeclaredVar env x = 
    case (look (env, x)) of 
      SOME (E.VarEntry _) => true  
    | _ => false

  fun isDeclaredFun env x = 
    case (look (env, x)) of 
      SOME (E.FuncEntry _) => true 
    | _ => false 


  fun getPrecType t1 t2 = 
    if (prec t1) < (prec t2)
    then t2 
    else t1  

  fun semanticExp env = 
    let 
      fun checkArguments id xs args = 
        if (length xs = length args orelse Symbol.name id = "printf") 
        then ()
        else raise Error ("wrong number of arguments in function " ^ (Symbol.name id))

      fun trIdValues (A.IdValuesVar id) = checkVar env id
        | trIdValues (A.IntVal x) = Types.INT
        | trIdValues (A.FloatVal x) = Types.FLOAT
        | trIdValues (A.CharVal x) = Types.CHAR
        (* TODO : to be implemented later *)
        | trIdValues (A.StringVal x) = Types.CHAR

      fun trExpOp (A.BinaryOp (e1, oper, e2)) = 
                let 
                  val t1 = trExp e1 
                  val t2 = trExp e2 
                in 
                  getPrecType t1 t2
                end 
        | trExpOp (A.IdIncDec (id, x)) = checkVar env id
        | trExpOp (A.IncDecId (id, x)) = checkVar env id
        | trExpOp (A.UnaryOp (unop, e)) = 
                let 
                  val ty = trExp e
                in
                  ty
                end 

      and trExp (A.IdValuesCons x) = trIdValues x 
        | trExp (A.ExpOpCons x) = trExpOp x 
        | trExp (A.AssignCons (id, asn, e)) = 
                let 
                  val tyId = checkVar env id 
                  val ty = trExp e 
                in 
                  tyId
                end 
        | trExp (A.TernaryOp (e1, e2, e3)) = 
                let  
                  val tye1 = trExp e1 
                  val tye2 = trExp e2 
                  val tye3 = trExp e3 
                in 
                  getPrecType tye2 tye3
                end 
        | trExp (A.Paren e) = 
                let  
                  val ty = trExp e 
                in 
                  ty
                end 
        | trExp (A.ExpFuncCall (Ast.FuncCallCons (id, xs))) = 
                let 
                  val (args, tyres) = checkFun env id 
                  val _ = checkArguments id xs args 
                in 
                  tyres
                end 

    in 
      trExp 
    end 

  fun trDecs ty env [] = env 
    | trDecs ty env ((A.Variable id) :: xs) = trDecs ty (enter (env, id, E.VarEntry {ty = ty})) xs 
    | trDecs ty env (A.DecVar (id, e) :: xs) = (semanticExp env e;
                                                trDecs ty (enter (env, id, E.VarEntry {ty = ty})) xs)

  fun semanticDecStmt env (A.DecStmtCons (prim, decs)) = 
    let 
      val myty = getPrimType prim 
      val env' = trDecs myty env decs 
    in 
      env'
    end

  fun semanticReturnStmt env A.RETURN = env  
    | semanticReturnStmt env (A.ReturnCons e) = (semanticExp env e; env)

  fun semanticStmt env (A.IF (e, x)) = 
                let 
                  val ty = semanticExp env e 
                in 
                  semanticStmt env x; env 
                end 

    | semanticStmt env (A.IFELSE (e, x1, x2)) = 
                  let 
                    val ty = semanticExp env e 
                  in 
                    semanticStmt env x1;
                    semanticStmt env x2;
                    env 
                  end 
    | semanticStmt env (A.StmtDecStmts x) = semanticDecStmt env x 
    | semanticStmt env (A.ReturnStmtCons x) = semanticReturnStmt env x 
    | semanticStmt env (A.BREAK) = env 
    | semanticStmt env (A.CONTINUE) = env 
    | semanticStmt env (A.Block sList) = (semanticStmts env sList; env)
    | semanticStmt env (A.While (e, x)) = 
                  let 
                    val ty = semanticExp env e 
                  in  
                    semanticStmt env x 
                  end 
    | semanticStmt env (A.StmtCommentCons x) = env 
    | semanticStmt env (A.ExpCons e) = (semanticExp env e; env)

  and semanticStmts env [] = env 
    | semanticStmts env (x::xs) = semanticStmts (semanticStmt env x) xs 

  fun semanticDec env p (A.Variable id) = 
          if not (isDeclaredVar env id) 
          then enter (env, id, E.VarEntry {ty = getPrimType p})
          else raise ExistingVariable 
    | semanticDec env p (A.DecVar (id, e)) = 
          if not (isDeclaredVar env id)
          then enter (env, id, E.VarEntry {ty = getPrimType p})
          else raise ExistingVariable 

  fun semanticDecs env [] = env 
    | semanticDecs env ((p,x)::xs) = semanticDecs (semanticDec env p x) xs 

  fun semanticProgItem env (A.ProgItemDirectives _) = env 
    | semanticProgItem env (A.DecStmtsCons x) = semanticDecStmt env x 
    | semanticProgItem env (A.FuncCons (prim, id, pList, sList)) =
          if isDeclaredFun env id 
          then raise ExistingFunction 
          else (
            let 
              val plist' = deconsParam pList
              val tylist = map getPrimType (map #1 plist')
              val env' = enter (env, id, E.FuncEntry {formals = tylist,result = getPrimType prim})
              val env'' = semanticDecs env' plist'
            in 
              (semanticStmt env'' sList; env')
            end 
          )
    | semanticProgItem env (A.ProgItemComments x) = env 

  fun semanticProg env [] = []
    | semanticProg env (x :: xs) = semanticProg (semanticProgItem env x) xs

  fun semant ast = (semanticProg E.base_env ast; ())

end