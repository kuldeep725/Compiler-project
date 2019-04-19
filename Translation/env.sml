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

    fun enter ((symbol, entry), env) = Symbol.enter(env, symbol, entry)

    val base_env = foldr enter Symbol.empty [
        (Symbol.symbol("printf"), FuncEntry {formals=[Types.STRING], result = Types.VOID}),
        (Symbol.symbol("toupper"), FuncEntry {formals=[Types.CHAR], result = Types.CHAR})
    ]
end