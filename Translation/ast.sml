structure Ast = struct 

    type symbol = Symbol.symbol

    datatype IdValues =  IdValuesVar of symbol
                       | IntVal of int 
                       | FloatVal of real
                       | CharVal of char 
                       | StringVal of string
    

    datatype Compare =  NE 
                      | EQ 
                      | GT 
                      | GTE 
                      | LT 
                      | LTE 

    datatype Bitwise =  BITAND    
                      | BITOR     
                      | XOR    
                      | LEFTSHIFT 
                      | RIGHTSHIFT

    datatype Logical =  AND 
                      | OR

    datatype Binop =  PLUS 
                    | MINUS
                    | MUL 
                    | DIV 
                    | MODULUS  
                    | BinopCompare of Compare
                    | BinopBitwise of Bitwise 
                    | BinopLogical of Logical 

    datatype Assignment =  ASSIGN    
                         | PLUSA     
                         | MINUSA    
                         | TIMESA    
                         | DIVA  
                         | MODULUSA 
                         | LEFTSHIFTA    
                         | RIGHTSHIFTA   
                         | BITORA    
                         | BITANDA   
                         | XORA

    datatype IncDec =  INC 
                     | DEC 

    datatype Unop =  NOT 
                   | COMPLEMENT

    datatype Prim =  INT 
                   | VOID 
                   | CHAR 
                   | FLOAT 
                   | DOUBLE
                   | LONG 

    datatype CommentStmt =  SingleComment of string 
                          | MultiComment of string 

    datatype Directives =  Include of string 
                         | Includeh of string 
                         | Define of symbol * IdValues

    datatype ExpOp =  BinaryOp of Exp * Binop * Exp 
                    | IdIncDec of symbol * IncDec
                    | IncDecId of symbol * IncDec
                    | UnaryOp of Unop * Exp 

    and      Exp =  IdValuesCons of IdValues 
                  | ExpOpCons of ExpOp 
                  | AssignCons of symbol * Assignment * Exp 
                  | TernaryOp of Exp * Exp * Exp 
                  | Paren of Exp 
                  | ExpFuncCall of FuncCall 
                  
    and FuncCall = FuncCallCons of symbol * Exp list

    datatype Dec =  Variable of symbol
                  | DecVar of symbol * Exp

    datatype DecStmt = DecStmtCons of Prim * Dec list

    datatype Params = ParamsCons of Prim * Dec 

    datatype ReturnStmt =  RETURN 
                         | ReturnCons of Exp 
    
    datatype Statement =  IF of Exp * Statement
                        | IFELSE of Exp * Statement * Statement
                        | StmtDecStmts of DecStmt
                        | ReturnStmtCons of ReturnStmt
                        | BREAK
                        | CONTINUE
                        | Block of Statement list 
                        | While of Exp * Statement 
                        | StmtCommentCons of CommentStmt
                        | ExpCons of Exp 

    
    
    datatype ProgItem =  ProgItemDirectives of Directives 
                       | DecStmtsCons of DecStmt
                       | FuncCons of Prim * symbol * Params list * Statement 
                       | ProgItemComments of CommentStmt


end