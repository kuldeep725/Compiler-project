structure JavaAst = struct 

  open Symbol 

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
                  | BinopBitwise of Bitwise 

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

  datatype CondAsn =  CASSIGN 
                    | CBITORA 
                    | CBITANDA
                    | CXORA 

  datatype IncDec = INC 
                  | DEC 

  datatype Prim = INT 
                | VOID 
                | CHAR 
                | FLOAT 
                | DOUBLE
                | LONG 
                | BOOL 

  (* Access Modifiers *)
  datatype AM = PUBLIC 
              | PRIVATE 
              | PROTECTED
              | PACKAGE 

  datatype CondExp = BinComp of Exp * Compare * Exp 
                   | BinLog of CondExp * Logical * CondExp
                   | NotCons of CondExp  
                   | TRUE 
                   | FALSE
                   | BoolId of symbol 
                   | CondParen of CondExp 
  
  and Exp = IdValuesCons of IdValues
          | ExpOpCons of ExpOp
          | AssignCons of symbol * Assignment * Exp 
          | AssignCond of symbol * CondAsn * CondExp 
          | TernaryOp of CondExp * Exp * Exp 
          | Paren of Exp 
          | ExpFuncCall of FuncCall 

  and ExpOp = BinaryOp of Exp * Binop * Exp 
            | IdIncDec of symbol * IncDec 
            | IncDecId of symbol * IncDec
            | ComplementCons of Exp 

  and FuncCall = FuncCallCons of symbol * Exp list 
            
  
  datatype Dec =  Variable of symbol
                | DecVar of symbol * Exp

  datatype Params = ParamsCons of Prim * Dec 
  
  datatype ReturnStmt = RETURN 
                      | ReturnCons of Exp 

  datatype CommentStmt =  SingleComment of string 
                        | MultiComment of string 

  datatype Statement = IF of CondExp * Statement 
                     | IFELSE of CondExp * Statement * Statement
                     | StmtDecStmts of Prim * Dec list 
                     | ReturnStmtCons of ReturnStmt 
                     | BREAK 
                     | CONTINUE 
                     | Block of Statement list 
                     | While of CondExp * Statement
                     | StmtCommentCons of CommentStmt
                     | ExpCons of Exp        
               

  datatype ProgItem = ProgStaticDecStmts of AM * Prim * Dec list 
                    | ProgDecStmts of AM * Prim * Dec list 
                    | FuncAMCons of AM * Prim * symbol * Params list * Statement
                    | FuncStaticAMCons of AM * Prim * symbol * Params list * Statement
                    | FuncStaticCons of Prim * symbol * Params list * Statement
                    | FuncCons of Prim * symbol * Params list * Statement
                   

end 