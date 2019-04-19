signature ENV =
sig
    datatype env_entry =  VarEntry of {ty : Types.ty }
                        | FuncEntry of { formals : Types.ty list, result : Types.ty }   
    val base_env : env_entry Symbol.table 
end

structure Env :> ENV =
struct
    datatype env_entry =  VarEntry of {ty : Types.ty }
                        | FuncEntry of { formals : Types.ty list, result : Types.ty }

    val base_env = Symbol.empty
end