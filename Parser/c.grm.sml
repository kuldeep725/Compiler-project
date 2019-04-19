functor ExprLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Expr_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "c.grm"*)(* This is the preamble where you can have arbitrary sml code. For us
it is empty *)


(*#line 15.1 "c.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\004\000\118\000\006\000\018\000\008\000\117\000\011\000\017\000\
\\013\000\016\000\015\000\116\000\016\000\015\000\017\000\014\000\
\\019\000\115\000\028\000\013\000\029\000\114\000\035\000\035\000\
\\036\000\034\000\037\000\033\000\038\000\049\000\039\000\048\000\
\\043\000\101\000\047\000\074\000\050\000\073\000\051\000\072\000\
\\052\000\071\000\053\000\070\000\054\000\069\000\055\000\047\000\
\\056\000\046\000\057\000\068\000\058\000\067\000\059\000\066\000\
\\060\000\065\000\061\000\064\000\062\000\063\000\063\000\062\000\
\\064\000\061\000\065\000\045\000\066\000\060\000\067\000\059\000\
\\068\000\058\000\069\000\044\000\070\000\057\000\071\000\056\000\
\\083\000\031\000\000\000\
\\001\000\004\000\118\000\006\000\018\000\008\000\117\000\011\000\017\000\
\\013\000\016\000\015\000\116\000\016\000\015\000\017\000\014\000\
\\019\000\115\000\028\000\013\000\029\000\114\000\035\000\035\000\
\\036\000\034\000\037\000\033\000\038\000\049\000\039\000\048\000\
\\043\000\101\000\055\000\047\000\056\000\046\000\065\000\045\000\
\\069\000\044\000\083\000\031\000\000\000\
\\001\000\035\000\035\000\036\000\034\000\037\000\033\000\038\000\032\000\
\\083\000\031\000\000\000\
\\001\000\035\000\035\000\036\000\034\000\037\000\033\000\038\000\049\000\
\\039\000\048\000\041\000\128\000\055\000\047\000\056\000\046\000\
\\065\000\045\000\069\000\044\000\083\000\031\000\000\000\
\\001\000\035\000\035\000\036\000\034\000\037\000\033\000\038\000\049\000\
\\039\000\048\000\055\000\047\000\056\000\046\000\065\000\045\000\
\\069\000\044\000\083\000\031\000\000\000\
\\001\000\038\000\023\000\000\000\
\\001\000\038\000\025\000\000\000\
\\001\000\038\000\037\000\000\000\
\\001\000\038\000\076\000\000\000\
\\001\000\040\000\092\000\000\000\
\\001\000\040\000\096\000\047\000\074\000\050\000\073\000\051\000\072\000\
\\052\000\071\000\053\000\070\000\054\000\069\000\057\000\068\000\
\\058\000\067\000\059\000\066\000\060\000\065\000\061\000\064\000\
\\062\000\063\000\063\000\062\000\064\000\061\000\066\000\060\000\
\\067\000\059\000\068\000\058\000\070\000\057\000\071\000\056\000\000\000\
\\001\000\040\000\104\000\000\000\
\\001\000\041\000\027\000\000\000\
\\001\000\041\000\125\000\047\000\074\000\050\000\073\000\051\000\072\000\
\\052\000\071\000\053\000\070\000\054\000\069\000\057\000\068\000\
\\058\000\067\000\059\000\066\000\060\000\065\000\061\000\064\000\
\\062\000\063\000\063\000\062\000\064\000\061\000\066\000\060\000\
\\067\000\059\000\068\000\058\000\070\000\057\000\071\000\056\000\000\000\
\\001\000\041\000\130\000\000\000\
\\001\000\041\000\131\000\000\000\
\\001\000\041\000\133\000\047\000\074\000\050\000\073\000\051\000\072\000\
\\052\000\071\000\053\000\070\000\054\000\069\000\057\000\068\000\
\\058\000\067\000\059\000\066\000\060\000\065\000\061\000\064\000\
\\062\000\063\000\063\000\062\000\064\000\061\000\066\000\060\000\
\\067\000\059\000\068\000\058\000\070\000\057\000\071\000\056\000\000\000\
\\001\000\043\000\101\000\000\000\
\\001\000\044\000\124\000\000\000\
\\001\000\047\000\074\000\048\000\103\000\050\000\073\000\051\000\072\000\
\\052\000\071\000\053\000\070\000\054\000\069\000\057\000\068\000\
\\058\000\067\000\059\000\066\000\060\000\065\000\061\000\064\000\
\\062\000\063\000\063\000\062\000\064\000\061\000\066\000\060\000\
\\067\000\059\000\068\000\058\000\070\000\057\000\071\000\056\000\000\000\
\\138\000\000\000\
\\139\000\002\000\020\000\003\000\019\000\006\000\018\000\011\000\017\000\
\\013\000\016\000\016\000\015\000\017\000\014\000\028\000\013\000\
\\032\000\012\000\033\000\011\000\034\000\010\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\000\000\
\\151\000\000\000\
\\152\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\042\000\026\000\000\000\
\\156\000\000\000\
\\157\000\039\000\029\000\072\000\028\000\000\000\
\\157\000\072\000\028\000\000\000\
\\158\000\047\000\074\000\050\000\073\000\051\000\072\000\052\000\071\000\
\\053\000\070\000\054\000\069\000\057\000\068\000\058\000\067\000\
\\059\000\066\000\060\000\065\000\061\000\064\000\062\000\063\000\
\\063\000\062\000\064\000\061\000\066\000\060\000\067\000\059\000\
\\068\000\058\000\070\000\057\000\071\000\056\000\000\000\
\\159\000\000\000\
\\159\000\039\000\091\000\055\000\047\000\056\000\046\000\072\000\090\000\
\\073\000\089\000\074\000\088\000\075\000\087\000\076\000\086\000\
\\077\000\085\000\078\000\084\000\079\000\083\000\080\000\082\000\
\\081\000\081\000\082\000\080\000\000\000\
\\160\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\163\000\000\000\
\\164\000\047\000\074\000\050\000\073\000\051\000\072\000\052\000\071\000\
\\053\000\070\000\054\000\069\000\057\000\068\000\058\000\067\000\
\\059\000\066\000\060\000\065\000\061\000\064\000\062\000\063\000\
\\063\000\062\000\064\000\061\000\066\000\060\000\067\000\059\000\
\\068\000\058\000\070\000\057\000\071\000\056\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\000\000\
\\194\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\\197\000\000\000\
\\198\000\000\000\
\\199\000\000\000\
\\200\000\000\000\
\\201\000\000\000\
\\202\000\000\000\
\\203\000\000\000\
\\204\000\000\000\
\\205\000\000\000\
\\206\000\047\000\074\000\050\000\073\000\051\000\072\000\052\000\071\000\
\\053\000\070\000\054\000\069\000\057\000\068\000\058\000\067\000\
\\059\000\066\000\060\000\065\000\061\000\064\000\062\000\063\000\
\\063\000\062\000\064\000\061\000\066\000\060\000\067\000\059\000\
\\068\000\058\000\070\000\057\000\071\000\056\000\000\000\
\\207\000\047\000\074\000\050\000\073\000\051\000\072\000\052\000\071\000\
\\053\000\070\000\054\000\069\000\057\000\068\000\058\000\067\000\
\\059\000\066\000\060\000\065\000\061\000\064\000\062\000\063\000\
\\063\000\062\000\064\000\061\000\066\000\060\000\067\000\059\000\
\\068\000\058\000\070\000\057\000\071\000\056\000\000\000\
\\208\000\000\000\
\\209\000\000\000\
\\210\000\006\000\018\000\011\000\017\000\013\000\016\000\016\000\015\000\
\\017\000\014\000\028\000\013\000\000\000\
\\211\000\042\000\102\000\000\000\
\\212\000\000\000\
\\213\000\000\000\
\\214\000\035\000\035\000\036\000\034\000\037\000\033\000\038\000\049\000\
\\039\000\048\000\055\000\047\000\056\000\046\000\065\000\045\000\
\\069\000\044\000\083\000\031\000\000\000\
\\215\000\042\000\105\000\047\000\074\000\050\000\073\000\051\000\072\000\
\\052\000\071\000\053\000\070\000\054\000\069\000\057\000\068\000\
\\058\000\067\000\059\000\066\000\060\000\065\000\061\000\064\000\
\\062\000\063\000\063\000\062\000\064\000\061\000\066\000\060\000\
\\067\000\059\000\068\000\058\000\070\000\057\000\071\000\056\000\000\000\
\\216\000\000\000\
\\217\000\000\000\
\\218\000\000\000\
\\219\000\002\000\020\000\003\000\019\000\004\000\118\000\006\000\018\000\
\\008\000\117\000\011\000\017\000\013\000\016\000\015\000\116\000\
\\016\000\015\000\017\000\014\000\019\000\115\000\028\000\013\000\
\\029\000\114\000\035\000\035\000\036\000\034\000\037\000\033\000\
\\038\000\049\000\039\000\048\000\043\000\101\000\055\000\047\000\
\\056\000\046\000\065\000\045\000\069\000\044\000\083\000\031\000\000\000\
\\220\000\000\000\
\\221\000\000\000\
\\222\000\012\000\135\000\000\000\
\\223\000\000\000\
\\224\000\000\000\
\\225\000\000\000\
\\226\000\000\000\
\\227\000\000\000\
\\228\000\000\000\
\\229\000\000\000\
\\230\000\000\000\
\\231\000\000\000\
\\232\000\000\000\
\\233\000\000\000\
\\234\000\000\000\
\"
val actionRowNumbers =
"\022\000\027\000\026\000\025\000\
\\006\000\022\000\021\000\024\000\
\\007\000\029\000\028\000\032\000\
\\036\000\031\000\034\000\035\000\
\\033\000\118\000\119\000\038\000\
\\013\000\040\000\023\000\003\000\
\\008\000\037\000\005\000\095\000\
\\030\000\048\000\043\000\047\000\
\\046\000\045\000\039\000\041\000\
\\094\000\042\000\005\000\009\000\
\\090\000\089\000\088\000\087\000\
\\086\000\085\000\005\000\044\000\
\\010\000\008\000\060\000\059\000\
\\058\000\005\000\071\000\070\000\
\\069\000\068\000\067\000\073\000\
\\072\000\066\000\065\000\064\000\
\\063\000\062\000\061\000\057\000\
\\056\000\055\000\054\000\053\000\
\\005\000\052\000\051\000\011\000\
\\050\000\005\000\084\000\083\000\
\\082\000\081\000\080\000\079\000\
\\078\000\077\000\076\000\075\000\
\\074\000\099\000\018\000\096\000\
\\049\000\020\000\093\000\091\000\
\\012\000\100\000\098\000\104\000\
\\095\000\005\000\102\000\099\000\
\\104\000\110\000\104\000\019\000\
\\113\000\014\000\109\000\008\000\
\\005\000\004\000\005\000\015\000\
\\016\000\097\000\092\000\101\000\
\\106\000\105\000\103\000\115\000\
\\001\000\017\000\116\000\001\000\
\\112\000\111\000\114\000\117\000\
\\107\000\002\000\108\000\000\000"
val gotoT =
"\
\\001\000\135\000\002\000\007\000\003\000\006\000\004\000\005\000\
\\005\000\004\000\006\000\003\000\020\000\002\000\027\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\020\000\008\000\019\000\000\000\
\\002\000\007\000\003\000\022\000\004\000\005\000\005\000\004\000\
\\006\000\003\000\020\000\002\000\027\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\028\000\000\000\
\\007\000\034\000\008\000\019\000\000\000\
\\000\000\
\\009\000\041\000\010\000\040\000\016\000\039\000\017\000\038\000\
\\018\000\037\000\022\000\036\000\000\000\
\\005\000\049\000\019\000\048\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\053\000\012\000\052\000\013\000\051\000\014\000\050\000\000\000\
\\009\000\041\000\010\000\040\000\016\000\039\000\017\000\038\000\
\\018\000\073\000\022\000\036\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\041\000\010\000\040\000\016\000\039\000\017\000\038\000\
\\018\000\075\000\022\000\036\000\000\000\
\\015\000\077\000\016\000\076\000\000\000\
\\000\000\
\\008\000\091\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\041\000\010\000\040\000\016\000\039\000\017\000\038\000\
\\018\000\092\000\022\000\036\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\041\000\010\000\040\000\016\000\039\000\017\000\038\000\
\\018\000\093\000\022\000\036\000\000\000\
\\011\000\053\000\012\000\052\000\013\000\051\000\014\000\050\000\000\000\
\\000\000\
\\011\000\053\000\012\000\052\000\013\000\051\000\014\000\050\000\000\000\
\\000\000\
\\009\000\041\000\010\000\040\000\016\000\039\000\017\000\038\000\
\\018\000\095\000\022\000\036\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\041\000\010\000\040\000\016\000\039\000\017\000\038\000\
\\018\000\097\000\021\000\096\000\022\000\036\000\000\000\
\\023\000\098\000\000\000\
\\000\000\
\\011\000\053\000\012\000\052\000\013\000\051\000\014\000\050\000\000\000\
\\011\000\053\000\012\000\052\000\013\000\051\000\014\000\050\000\000\000\
\\000\000\
\\011\000\053\000\012\000\052\000\013\000\051\000\014\000\050\000\000\000\
\\000\000\
\\011\000\053\000\012\000\052\000\013\000\051\000\014\000\050\000\000\000\
\\000\000\
\\005\000\111\000\006\000\110\000\009\000\041\000\010\000\040\000\
\\016\000\039\000\017\000\038\000\018\000\109\000\022\000\036\000\
\\023\000\108\000\024\000\107\000\025\000\106\000\026\000\105\000\
\\027\000\104\000\000\000\
\\005\000\049\000\019\000\117\000\000\000\
\\009\000\041\000\010\000\040\000\016\000\039\000\017\000\038\000\
\\018\000\118\000\022\000\036\000\000\000\
\\000\000\
\\009\000\041\000\010\000\040\000\016\000\039\000\017\000\038\000\
\\018\000\097\000\021\000\119\000\022\000\036\000\000\000\
\\005\000\111\000\006\000\110\000\009\000\041\000\010\000\040\000\
\\016\000\039\000\017\000\038\000\018\000\109\000\022\000\036\000\
\\023\000\108\000\024\000\120\000\025\000\106\000\026\000\105\000\
\\027\000\104\000\000\000\
\\000\000\
\\005\000\111\000\006\000\110\000\009\000\041\000\010\000\040\000\
\\016\000\039\000\017\000\038\000\018\000\109\000\022\000\036\000\
\\023\000\108\000\024\000\121\000\025\000\106\000\026\000\105\000\
\\027\000\104\000\000\000\
\\000\000\
\\000\000\
\\011\000\053\000\012\000\052\000\013\000\051\000\014\000\050\000\000\000\
\\000\000\
\\007\000\020\000\008\000\019\000\000\000\
\\009\000\041\000\010\000\040\000\016\000\039\000\017\000\038\000\
\\018\000\124\000\022\000\036\000\000\000\
\\009\000\041\000\010\000\040\000\016\000\039\000\017\000\038\000\
\\018\000\125\000\022\000\036\000\000\000\
\\009\000\041\000\010\000\040\000\016\000\039\000\017\000\038\000\
\\018\000\127\000\022\000\036\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\053\000\012\000\052\000\013\000\051\000\014\000\050\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\111\000\006\000\110\000\009\000\041\000\010\000\040\000\
\\011\000\053\000\012\000\052\000\013\000\051\000\014\000\050\000\
\\016\000\039\000\017\000\038\000\018\000\109\000\022\000\036\000\
\\023\000\108\000\025\000\130\000\026\000\105\000\000\000\
\\011\000\053\000\012\000\052\000\013\000\051\000\014\000\050\000\000\000\
\\000\000\
\\005\000\111\000\006\000\110\000\009\000\041\000\010\000\040\000\
\\011\000\053\000\012\000\052\000\013\000\051\000\014\000\050\000\
\\016\000\039\000\017\000\038\000\018\000\109\000\022\000\036\000\
\\023\000\108\000\025\000\132\000\026\000\105\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\111\000\006\000\110\000\009\000\041\000\010\000\040\000\
\\016\000\039\000\017\000\038\000\018\000\109\000\022\000\036\000\
\\023\000\108\000\025\000\134\000\026\000\105\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 136
val numrules = 97
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID' | ntVOID of unit | STRING of  (string) | ID of  (string) | CHARVAL of  (char) | FLOATVAL of  (real) | INTVAL of  (int) | INCLUDEH of  (string) | INCLUDE of  (string) | SINGLECOMMENT of  (string) | MULTICOMMENT of  (string) | COMMENTSTMT of  (Ast.CommentStmt) | RETURNSTMT of  (Ast.ReturnStmt) | STATEMENT of  (Ast.Statement) | STATEMENTS of  (Ast.Statement list) | BLOCK of  (Ast.Statement list) | FUNC_CALL of  (Ast.FuncCall) | ARGUMENTS of  (Ast.Exp list) | FUNC of  (Ast.ProgItem) | PARAMS of  (Ast.Params list) | EXP of  (Ast.Exp) | UNOP of  (Ast.Unop) | INCDEC of  (Ast.IncDec) | ASSIGNMENT of  (Ast.Assignment) | LOGICAL of  (Ast.Logical) | BITWISE of  (Ast.Bitwise) | COMPARE of  (Ast.Compare) | BINOP of  (Ast.Binop) | EXP_OP of  (Ast.ExpOp) | ID_VALUES of  (Ast.IdValues) | DECS of  (Ast.Dec) | DECSTMT of  (Ast.Dec list) | DECSTMTS of  (Ast.DecStmt) | PRIM of  (Ast.Prim) | PROG_ITEM of  (Ast.ProgItem) | PROG_ITEMS of  (Ast.ProgItem list) | DIRECTIVES of  (Ast.Directives) | PROGRAM of  (Ast.ProgItem list)
end
type svalue = MlyValue.svalue
type result = Ast.ProgItem list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "MULTICOMMENT"
  | (T 2) => "SINGLECOMMENT"
  | (T 3) => "BREAK"
  | (T 4) => "CASE"
  | (T 5) => "CHAR"
  | (T 6) => "CONST"
  | (T 7) => "CONTINUE"
  | (T 8) => "DEFAULT"
  | (T 9) => "DO"
  | (T 10) => "DOUBLE"
  | (T 11) => "ELSE"
  | (T 12) => "FLOAT"
  | (T 13) => "FOR"
  | (T 14) => "IF"
  | (T 15) => "INT"
  | (T 16) => "LONG"
  | (T 17) => "REGISTER"
  | (T 18) => "RETURN"
  | (T 19) => "SHORT"
  | (T 20) => "SIGNED"
  | (T 21) => "SIZEOF"
  | (T 22) => "STATIC"
  | (T 23) => "STRUCT"
  | (T 24) => "SWITCH"
  | (T 25) => "TYPEDEF"
  | (T 26) => "UNSIGNED"
  | (T 27) => "VOID"
  | (T 28) => "WHILE"
  | (T 29) => "NULL"
  | (T 30) => "PRINTF"
  | (T 31) => "INCLUDE"
  | (T 32) => "INCLUDEH"
  | (T 33) => "DEFINE"
  | (T 34) => "INTVAL"
  | (T 35) => "FLOATVAL"
  | (T 36) => "CHARVAL"
  | (T 37) => "ID"
  | (T 38) => "LPAREN"
  | (T 39) => "RPAREN"
  | (T 40) => "SEMICOLON"
  | (T 41) => "COMMA"
  | (T 42) => "LBRACE"
  | (T 43) => "RBRACE"
  | (T 44) => "LBRACKET"
  | (T 45) => "RBRACKET"
  | (T 46) => "QMARK"
  | (T 47) => "COLON"
  | (T 48) => "DOT"
  | (T 49) => "PLUS"
  | (T 50) => "MINUS"
  | (T 51) => "MUL"
  | (T 52) => "DIV"
  | (T 53) => "MODULUS"
  | (T 54) => "INC"
  | (T 55) => "DEC"
  | (T 56) => "NE"
  | (T 57) => "EQ"
  | (T 58) => "GT"
  | (T 59) => "GTE"
  | (T 60) => "LT"
  | (T 61) => "LTE"
  | (T 62) => "AND"
  | (T 63) => "OR"
  | (T 64) => "NOT"
  | (T 65) => "BITAND"
  | (T 66) => "BITOR"
  | (T 67) => "XOR"
  | (T 68) => "COMPLEMENT"
  | (T 69) => "LEFTSHIFT"
  | (T 70) => "RIGHTSHIFT"
  | (T 71) => "ASSIGN"
  | (T 72) => "PLUSA"
  | (T 73) => "MINUSA"
  | (T 74) => "TIMESA"
  | (T 75) => "DIVA"
  | (T 76) => "MODULUSA"
  | (T 77) => "LEFTSHIFTA"
  | (T 78) => "RIGHTSHIFTA"
  | (T 79) => "BITORA"
  | (T 80) => "BITANDA"
  | (T 81) => "XORA"
  | (T 82) => "STRING"
  | (T 83) => "HIGHER"
  | (T 84) => "LOWER"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID'
end
val terms : term list = nil
 $$ (T 84) $$ (T 83) $$ (T 81) $$ (T 80) $$ (T 79) $$ (T 78) $$ (T 77) $$ (T 76) $$ (T 75) $$ (T 74) $$ (T 73) $$ (T 72) $$ (T 71) $$ (T 70) $$ (T 69) $$ (T 68) $$ (T 67) $$ (T 66) $$ (T 65) $$ (T 64) $$ (T 63) $$ (T 62) $$ (T 61) $$ (T 60) $$ (T 59) $$ (T 58) $$ (T 57) $$ (T 56) $$ (T 55) $$ (T 54) $$ (T 53) $$ (T 52) $$ (T 51) $$ (T 50) $$ (T 49) $$ (T 48) $$ (T 47) $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 33) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.PROG_ITEMS PROG_ITEMS, PROG_ITEMS1left, PROG_ITEMS1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 104.42 "c.grm"*)PROG_ITEMS(*#line 597.1 "c.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PROG_ITEMS1left, PROG_ITEMS1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.PROG_ITEMS ((*#line 106.42 "c.grm"*)[](*#line 601.1 "c.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.PROG_ITEMS PROG_ITEMS, _, PROG_ITEMS1right)) :: ( _, ( MlyValue.PROG_ITEM PROG_ITEM, PROG_ITEM1left, _)) :: rest671)) => let val  result = MlyValue.PROG_ITEMS ((*#line 107.42 "c.grm"*)PROG_ITEM :: PROG_ITEMS(*#line 605.1 "c.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, PROG_ITEM1left, PROG_ITEMS1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.DIRECTIVES DIRECTIVES, DIRECTIVES1left, DIRECTIVES1right)) :: rest671)) => let val  result = MlyValue.PROG_ITEM ((*#line 109.38 "c.grm"*)Ast.ProgItemDirectives DIRECTIVES(*#line 609.1 "c.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, DIRECTIVES1left, DIRECTIVES1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.DECSTMTS DECSTMTS, DECSTMTS1left, DECSTMTS1right)) :: rest671)) => let val  result = MlyValue.PROG_ITEM ((*#line 110.38 "c.grm"*)Ast.DecStmtsCons DECSTMTS(*#line 613.1 "c.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, DECSTMTS1left, DECSTMTS1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.FUNC FUNC, FUNC1left, FUNC1right)) :: rest671)) => let val  result = MlyValue.PROG_ITEM ((*#line 111.38 "c.grm"*)FUNC(*#line 617.1 "c.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, FUNC1left, FUNC1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.COMMENTSTMT COMMENTSTMT, COMMENTSTMT1left, COMMENTSTMT1right)) :: rest671)) => let val  result = MlyValue.PROG_ITEM ((*#line 112.38 "c.grm"*)Ast.ProgItemComments COMMENTSTMT(*#line 621.1 "c.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, COMMENTSTMT1left, COMMENTSTMT1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.INCLUDE INCLUDE, INCLUDE1left, INCLUDE1right)) :: rest671)) => let val  result = MlyValue.DIRECTIVES ((*#line 115.38 "c.grm"*)Ast.Include INCLUDE(*#line 625.1 "c.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, INCLUDE1left, INCLUDE1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.INCLUDEH INCLUDEH, INCLUDEH1left, INCLUDEH1right)) :: rest671)) => let val  result = MlyValue.DIRECTIVES ((*#line 116.38 "c.grm"*)Ast.Includeh INCLUDEH(*#line 629.1 "c.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, INCLUDEH1left, INCLUDEH1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ID_VALUES ID_VALUES, _, ID_VALUES1right)) :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, DEFINE1left, _)) :: rest671)) => let val  result = MlyValue.DIRECTIVES ((*#line 117.38 "c.grm"*)Ast.Define (Ast.Id ID, ID_VALUES)(*#line 633.1 "c.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, DEFINE1left, ID_VALUES1right), rest671)
end
|  ( 10, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.PRIM ((*#line 120.38 "c.grm"*)Ast.INT(*#line 637.1 "c.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, INT1left, INT1right), rest671)
end
|  ( 11, ( ( _, ( _, VOID1left, VOID1right)) :: rest671)) => let val  result = MlyValue.PRIM ((*#line 121.38 "c.grm"*)Ast.VOID(*#line 641.1 "c.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, VOID1left, VOID1right), rest671)
end
|  ( 12, ( ( _, ( _, CHAR1left, CHAR1right)) :: rest671)) => let val  result = MlyValue.PRIM ((*#line 122.38 "c.grm"*)Ast.CHAR(*#line 645.1 "c.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, CHAR1left, CHAR1right), rest671)
end
|  ( 13, ( ( _, ( _, FLOAT1left, FLOAT1right)) :: rest671)) => let val  result = MlyValue.PRIM ((*#line 123.38 "c.grm"*)Ast.FLOAT(*#line 649.1 "c.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, FLOAT1left, FLOAT1right), rest671)
end
|  ( 14, ( ( _, ( _, DOUBLE1left, DOUBLE1right)) :: rest671)) => let val  result = MlyValue.PRIM ((*#line 124.38 "c.grm"*)Ast.DOUBLE(*#line 653.1 "c.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, DOUBLE1left, DOUBLE1right), rest671)
end
|  ( 15, ( ( _, ( _, LONG1left, LONG1right)) :: rest671)) => let val  result = MlyValue.PRIM ((*#line 125.38 "c.grm"*)Ast.LONG(*#line 657.1 "c.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, LONG1left, LONG1right), rest671)
end
|  ( 16, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.DECSTMT DECSTMT, _, _)) :: ( _, ( MlyValue.PRIM PRIM, PRIM1left, _)) :: rest671)) => let val  result = MlyValue.DECSTMTS ((*#line 127.38 "c.grm"*)Ast.DecStmtCons (PRIM, DECSTMT)(*#line 661.1 "c.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, PRIM1left, SEMICOLON1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.DECS DECS, DECS1left, DECS1right)) :: rest671)) => let val  result = MlyValue.DECSTMT ((*#line 129.39 "c.grm"*)DECS :: [](*#line 665.1 "c.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, DECS1left, DECS1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.DECSTMT DECSTMT, _, DECSTMT1right)) :: _ :: ( _, ( MlyValue.DECS DECS, DECS1left, _)) :: rest671)) => let val  result = MlyValue.DECSTMT ((*#line 130.39 "c.grm"*)DECS :: DECSTMT(*#line 669.1 "c.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, DECS1left, DECSTMT1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.DECS ((*#line 132.32 "c.grm"*)Ast.Variable (Ast.Id ID)(*#line 673.1 "c.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, ID1left, ID1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.DECS ((*#line 133.32 "c.grm"*)Ast.DecVar (Ast.Id ID, EXP)(*#line 677.1 "c.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, ID1left, EXP1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.ID_VALUES ((*#line 136.32 "c.grm"*)Ast.IdValuesVar (Ast.Id ID)(*#line 681.1 "c.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, ID1left, ID1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.INTVAL INTVAL, INTVAL1left, INTVAL1right)) :: rest671)) => let val  result = MlyValue.ID_VALUES ((*#line 137.32 "c.grm"*)Ast.IntVal INTVAL(*#line 685.1 "c.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, INTVAL1left, INTVAL1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.FLOATVAL FLOATVAL, FLOATVAL1left, FLOATVAL1right)) :: rest671)) => let val  result = MlyValue.ID_VALUES ((*#line 138.32 "c.grm"*)Ast.FloatVal FLOATVAL(*#line 689.1 "c.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, FLOATVAL1left, FLOATVAL1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.CHARVAL CHARVAL, CHARVAL1left, CHARVAL1right)) :: rest671)) => let val  result = MlyValue.ID_VALUES ((*#line 139.32 "c.grm"*)Ast.CharVal CHARVAL(*#line 693.1 "c.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, CHARVAL1left, CHARVAL1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.STRING STRING, STRING1left, STRING1right)) :: rest671)) => let val  result = MlyValue.ID_VALUES ((*#line 140.32 "c.grm"*)Ast.StringVal STRING(*#line 697.1 "c.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, STRING1left, STRING1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: ( _, ( MlyValue.BINOP BINOP, _, _)) :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP_OP ((*#line 142.44 "c.grm"*)Ast.BinaryOp (EXP1, BINOP, EXP2)(*#line 701.1 "c.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, EXP1left, EXP2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.INCDEC INCDEC, _, INCDEC1right)) :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.EXP_OP ((*#line 143.32 "c.grm"*)Ast.IdIncDec (Ast.Id ID, INCDEC)(*#line 705.1 "c.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, ID1left, INCDEC1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.ID ID, _, ID1right)) :: ( _, ( MlyValue.INCDEC INCDEC, INCDEC1left, _)) :: rest671)) => let val  result = MlyValue.EXP_OP ((*#line 144.32 "c.grm"*)Ast.IncDecId (Ast.Id ID, INCDEC)(*#line 709.1 "c.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, INCDEC1left, ID1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: ( _, ( MlyValue.UNOP UNOP, UNOP1left, _)) :: rest671)) => let val  result = MlyValue.EXP_OP ((*#line 145.45 "c.grm"*)Ast.UnaryOp (UNOP, EXP)(*#line 713.1 "c.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, UNOP1left, EXP1right), rest671)
end
|  ( 30, ( ( _, ( _, PLUS1left, PLUS1right)) :: rest671)) => let val  result = MlyValue.BINOP ((*#line 147.32 "c.grm"*)Ast.PLUS(*#line 717.1 "c.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, PLUS1left, PLUS1right), rest671)
end
|  ( 31, ( ( _, ( _, MINUS1left, MINUS1right)) :: rest671)) => let val  result = MlyValue.BINOP ((*#line 148.32 "c.grm"*)Ast.MINUS(*#line 721.1 "c.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, MINUS1left, MINUS1right), rest671)
end
|  ( 32, ( ( _, ( _, MUL1left, MUL1right)) :: rest671)) => let val  result = MlyValue.BINOP ((*#line 149.32 "c.grm"*)Ast.MUL(*#line 725.1 "c.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, MUL1left, MUL1right), rest671)
end
|  ( 33, ( ( _, ( _, DIV1left, DIV1right)) :: rest671)) => let val  result = MlyValue.BINOP ((*#line 150.32 "c.grm"*)Ast.DIV(*#line 729.1 "c.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, DIV1left, DIV1right), rest671)
end
|  ( 34, ( ( _, ( _, MODULUS1left, MODULUS1right)) :: rest671)) => let val  result = MlyValue.BINOP ((*#line 151.32 "c.grm"*)Ast.MODULUS(*#line 733.1 "c.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, MODULUS1left, MODULUS1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.COMPARE COMPARE, COMPARE1left, COMPARE1right)) :: rest671)) => let val  result = MlyValue.BINOP ((*#line 152.32 "c.grm"*)Ast.BinopCompare COMPARE(*#line 737.1 "c.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, COMPARE1left, COMPARE1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.BITWISE BITWISE, BITWISE1left, BITWISE1right)) :: rest671)) => let val  result = MlyValue.BINOP ((*#line 153.32 "c.grm"*)Ast.BinopBitwise BITWISE(*#line 741.1 "c.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, BITWISE1left, BITWISE1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.LOGICAL LOGICAL, LOGICAL1left, LOGICAL1right)) :: rest671)) => let val  result = MlyValue.BINOP ((*#line 154.32 "c.grm"*)Ast.BinopLogical LOGICAL(*#line 745.1 "c.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, LOGICAL1left, LOGICAL1right), rest671)
end
|  ( 38, ( ( _, ( _, NE1left, NE1right)) :: rest671)) => let val  result = MlyValue.COMPARE ((*#line 156.32 "c.grm"*)Ast.NE(*#line 749.1 "c.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, NE1left, NE1right), rest671)
end
|  ( 39, ( ( _, ( _, EQ1left, EQ1right)) :: rest671)) => let val  result = MlyValue.COMPARE ((*#line 157.32 "c.grm"*)Ast.EQ(*#line 753.1 "c.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, EQ1left, EQ1right), rest671)
end
|  ( 40, ( ( _, ( _, GT1left, GT1right)) :: rest671)) => let val  result = MlyValue.COMPARE ((*#line 158.32 "c.grm"*)Ast.GT(*#line 757.1 "c.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, GT1left, GT1right), rest671)
end
|  ( 41, ( ( _, ( _, GTE1left, GTE1right)) :: rest671)) => let val  result = MlyValue.COMPARE ((*#line 159.32 "c.grm"*)Ast.GTE(*#line 761.1 "c.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, GTE1left, GTE1right), rest671)
end
|  ( 42, ( ( _, ( _, LT1left, LT1right)) :: rest671)) => let val  result = MlyValue.COMPARE ((*#line 160.32 "c.grm"*)Ast.LT(*#line 765.1 "c.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, LT1left, LT1right), rest671)
end
|  ( 43, ( ( _, ( _, LTE1left, LTE1right)) :: rest671)) => let val  result = MlyValue.COMPARE ((*#line 161.32 "c.grm"*)Ast.LTE(*#line 769.1 "c.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, LTE1left, LTE1right), rest671)
end
|  ( 44, ( ( _, ( _, BITAND1left, BITAND1right)) :: rest671)) => let val  result = MlyValue.BITWISE ((*#line 163.32 "c.grm"*)Ast.BITAND(*#line 773.1 "c.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, BITAND1left, BITAND1right), rest671)
end
|  ( 45, ( ( _, ( _, BITOR1left, BITOR1right)) :: rest671)) => let val  result = MlyValue.BITWISE ((*#line 164.32 "c.grm"*)Ast.BITOR(*#line 777.1 "c.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, BITOR1left, BITOR1right), rest671)
end
|  ( 46, ( ( _, ( _, XOR1left, XOR1right)) :: rest671)) => let val  result = MlyValue.BITWISE ((*#line 165.32 "c.grm"*)Ast.XOR(*#line 781.1 "c.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, XOR1left, XOR1right), rest671)
end
|  ( 47, ( ( _, ( _, LEFTSHIFT1left, LEFTSHIFT1right)) :: rest671)) => let val  result = MlyValue.BITWISE ((*#line 166.32 "c.grm"*)Ast.LEFTSHIFT(*#line 785.1 "c.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, LEFTSHIFT1left, LEFTSHIFT1right), rest671)
end
|  ( 48, ( ( _, ( _, RIGHTSHIFT1left, RIGHTSHIFT1right)) :: rest671)) => let val  result = MlyValue.BITWISE ((*#line 167.32 "c.grm"*)Ast.RIGHTSHIFT(*#line 789.1 "c.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, RIGHTSHIFT1left, RIGHTSHIFT1right), rest671)
end
|  ( 49, ( ( _, ( _, AND1left, AND1right)) :: rest671)) => let val  result = MlyValue.LOGICAL ((*#line 169.32 "c.grm"*)Ast.AND(*#line 793.1 "c.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, AND1left, AND1right), rest671)
end
|  ( 50, ( ( _, ( _, OR1left, OR1right)) :: rest671)) => let val  result = MlyValue.LOGICAL ((*#line 170.32 "c.grm"*)Ast.OR(*#line 797.1 "c.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, OR1left, OR1right), rest671)
end
|  ( 51, ( ( _, ( _, ASSIGN1left, ASSIGN1right)) :: rest671)) => let val  result = MlyValue.ASSIGNMENT ((*#line 172.32 "c.grm"*)Ast.ASSIGN(*#line 801.1 "c.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, ASSIGN1left, ASSIGN1right), rest671)
end
|  ( 52, ( ( _, ( _, PLUSA1left, PLUSA1right)) :: rest671)) => let val  result = MlyValue.ASSIGNMENT ((*#line 173.32 "c.grm"*)Ast.PLUSA(*#line 805.1 "c.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, PLUSA1left, PLUSA1right), rest671)
end
|  ( 53, ( ( _, ( _, MINUSA1left, MINUSA1right)) :: rest671)) => let val  result = MlyValue.ASSIGNMENT ((*#line 174.32 "c.grm"*)Ast.MINUSA(*#line 809.1 "c.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, MINUSA1left, MINUSA1right), rest671)
end
|  ( 54, ( ( _, ( _, TIMESA1left, TIMESA1right)) :: rest671)) => let val  result = MlyValue.ASSIGNMENT ((*#line 175.32 "c.grm"*)Ast.TIMESA(*#line 813.1 "c.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, TIMESA1left, TIMESA1right), rest671)
end
|  ( 55, ( ( _, ( _, DIVA1left, DIVA1right)) :: rest671)) => let val  result = MlyValue.ASSIGNMENT ((*#line 176.32 "c.grm"*)Ast.DIVA(*#line 817.1 "c.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, DIVA1left, DIVA1right), rest671)
end
|  ( 56, ( ( _, ( _, MODULUSA1left, MODULUSA1right)) :: rest671)) => let val  result = MlyValue.ASSIGNMENT ((*#line 177.32 "c.grm"*)Ast.MODULUSA(*#line 821.1 "c.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, MODULUSA1left, MODULUSA1right), rest671)
end
|  ( 57, ( ( _, ( _, LEFTSHIFTA1left, LEFTSHIFTA1right)) :: rest671)) => let val  result = MlyValue.ASSIGNMENT ((*#line 178.32 "c.grm"*)Ast.LEFTSHIFTA(*#line 825.1 "c.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, LEFTSHIFTA1left, LEFTSHIFTA1right), rest671)
end
|  ( 58, ( ( _, ( _, RIGHTSHIFTA1left, RIGHTSHIFTA1right)) :: rest671)) => let val  result = MlyValue.ASSIGNMENT ((*#line 179.32 "c.grm"*)Ast.RIGHTSHIFTA(*#line 829.1 "c.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, RIGHTSHIFTA1left, RIGHTSHIFTA1right), rest671)
end
|  ( 59, ( ( _, ( _, BITORA1left, BITORA1right)) :: rest671)) => let val  result = MlyValue.ASSIGNMENT ((*#line 180.32 "c.grm"*)Ast.BITORA(*#line 833.1 "c.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, BITORA1left, BITORA1right), rest671)
end
|  ( 60, ( ( _, ( _, BITANDA1left, BITANDA1right)) :: rest671)) => let val  result = MlyValue.ASSIGNMENT ((*#line 181.32 "c.grm"*)Ast.BITANDA(*#line 837.1 "c.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, BITANDA1left, BITANDA1right), rest671)
end
|  ( 61, ( ( _, ( _, XORA1left, XORA1right)) :: rest671)) => let val  result = MlyValue.ASSIGNMENT ((*#line 182.32 "c.grm"*)Ast.XORA(*#line 841.1 "c.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, XORA1left, XORA1right), rest671)
end
|  ( 62, ( ( _, ( _, INC1left, INC1right)) :: rest671)) => let val  result = MlyValue.INCDEC ((*#line 185.32 "c.grm"*)Ast.INC(*#line 845.1 "c.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, INC1left, INC1right), rest671)
end
|  ( 63, ( ( _, ( _, DEC1left, DEC1right)) :: rest671)) => let val  result = MlyValue.INCDEC ((*#line 186.32 "c.grm"*)Ast.DEC(*#line 849.1 "c.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, DEC1left, DEC1right), rest671)
end
|  ( 64, ( ( _, ( _, NOT1left, NOT1right)) :: rest671)) => let val  result = MlyValue.UNOP ((*#line 188.32 "c.grm"*)Ast.NOT(*#line 853.1 "c.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, NOT1left, NOT1right), rest671)
end
|  ( 65, ( ( _, ( _, COMPLEMENT1left, COMPLEMENT1right)) :: rest671)) => let val  result = MlyValue.UNOP ((*#line 189.32 "c.grm"*)Ast.COMPLEMENT(*#line 857.1 "c.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, COMPLEMENT1left, COMPLEMENT1right), rest671)
end
|  ( 66, ( ( _, ( MlyValue.ID_VALUES ID_VALUES, ID_VALUES1left, ID_VALUES1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 191.38 "c.grm"*)Ast.IdValuesCons ID_VALUES(*#line 861.1 "c.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, ID_VALUES1left, ID_VALUES1right), rest671)
end
|  ( 67, ( ( _, ( MlyValue.EXP_OP EXP_OP, EXP_OP1left, EXP_OP1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 192.38 "c.grm"*)Ast.ExpOpCons EXP_OP(*#line 865.1 "c.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, EXP_OP1left, EXP_OP1right), rest671)
end
|  ( 68, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: ( _, ( MlyValue.ASSIGNMENT ASSIGNMENT, _, _)) :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 193.38 "c.grm"*)Ast.AssignCons (Ast.Id ID, ASSIGNMENT, EXP)(*#line 869.1 "c.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, ID1left, EXP1right), rest671)
end
|  ( 69, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 194.50 "c.grm"*)Ast.TernaryOp (EXP1, EXP2, EXP3)(*#line 873.1 "c.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, EXP1left, EXP3right), rest671)
end
|  ( 70, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 195.38 "c.grm"*)Ast.Paren (EXP)(*#line 877.1 "c.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 71, ( ( _, ( MlyValue.FUNC_CALL FUNC_CALL, FUNC_CALL1left, FUNC_CALL1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 196.38 "c.grm"*)Ast.ExpFuncCall FUNC_CALL(*#line 881.1 "c.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, FUNC_CALL1left, FUNC_CALL1right), rest671)
end
|  ( 72, ( rest671)) => let val  result = MlyValue.PARAMS ((*#line 198.45 "c.grm"*) [] (*#line 885.1 "c.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, defaultPos, defaultPos), rest671)
end
|  ( 73, ( ( _, ( MlyValue.DECS DECS, _, DECS1right)) :: ( _, ( MlyValue.PRIM PRIM, PRIM1left, _)) :: rest671)) => let val  result = MlyValue.PARAMS ((*#line 199.45 "c.grm"*)Ast.ParamsCons (PRIM, DECS) :: [](*#line 889.1 "c.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, PRIM1left, DECS1right), rest671)
end
|  ( 74, ( ( _, ( MlyValue.PARAMS PARAMS, _, PARAMS1right)) :: _ :: ( _, ( MlyValue.DECS DECS, _, _)) :: ( _, ( MlyValue.PRIM PRIM, PRIM1left, _)) :: rest671)) => let val  result = MlyValue.PARAMS ((*#line 200.45 "c.grm"*)Ast.ParamsCons (PRIM, DECS) :: PARAMS(*#line 893.1 "c.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, PRIM1left, PARAMS1right), rest671)
end
|  ( 75, ( ( _, ( MlyValue.BLOCK BLOCK, _, BLOCK1right)) :: _ :: ( _, ( MlyValue.PARAMS PARAMS, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( MlyValue.PRIM PRIM, PRIM1left, _)) :: rest671)) => let val  result = MlyValue.FUNC ((*#line 203.62 "c.grm"*)Ast.FuncCons (PRIM, Ast.Id ID, PARAMS, Ast.Block BLOCK)(*#line 897.1 "c.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, PRIM1left, BLOCK1right), rest671)
end
|  ( 76, ( rest671)) => let val  result = MlyValue.ARGUMENTS ((*#line 205.44 "c.grm"*)[](*#line 901.1 "c.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, defaultPos, defaultPos), rest671)
end
|  ( 77, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.ARGUMENTS ((*#line 206.44 "c.grm"*)EXP :: [](*#line 905.1 "c.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, EXP1left, EXP1right), rest671)
end
|  ( 78, ( ( _, ( MlyValue.ARGUMENTS ARGUMENTS, _, ARGUMENTS1right)) :: _ :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.ARGUMENTS ((*#line 207.44 "c.grm"*)EXP :: ARGUMENTS(*#line 909.1 "c.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, EXP1left, ARGUMENTS1right), rest671)
end
|  ( 79, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ARGUMENTS ARGUMENTS, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.FUNC_CALL ((*#line 209.45 "c.grm"*)Ast.FuncCallCons (Ast.Id ID, ARGUMENTS)(*#line 913.1 "c.grm.sml"*)
)
 in ( LrTable.NT 21, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 80, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.STATEMENTS STATEMENTS, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.BLOCK ((*#line 211.44 "c.grm"*)STATEMENTS(*#line 917.1 "c.grm.sml"*)
)
 in ( LrTable.NT 22, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 81, ( rest671)) => let val  result = MlyValue.STATEMENTS ((*#line 213.44 "c.grm"*)[](*#line 921.1 "c.grm.sml"*)
)
 in ( LrTable.NT 23, ( result, defaultPos, defaultPos), rest671)
end
|  ( 82, ( ( _, ( MlyValue.STATEMENTS STATEMENTS, _, STATEMENTS1right)) :: ( _, ( MlyValue.STATEMENT STATEMENT, STATEMENT1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENTS ((*#line 214.44 "c.grm"*)STATEMENT :: STATEMENTS(*#line 925.1 "c.grm.sml"*)
)
 in ( LrTable.NT 23, ( result, STATEMENT1left, STATEMENTS1right), rest671)
end
|  ( 83, ( ( _, ( MlyValue.STATEMENTS STATEMENTS, _, STATEMENTS1right)) :: ( _, ( MlyValue.COMMENTSTMT COMMENTSTMT, COMMENTSTMT1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENTS ((*#line 215.44 "c.grm"*)Ast.StmtCommentCons COMMENTSTMT :: STATEMENTS(*#line 929.1 "c.grm.sml"*)
)
 in ( LrTable.NT 23, ( result, COMMENTSTMT1left, STATEMENTS1right), rest671)
end
|  ( 84, ( ( _, ( MlyValue.STATEMENT STATEMENT, _, STATEMENT1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 218.61 "c.grm"*)Ast.IF (EXP, STATEMENT)(*#line 933.1 "c.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, IF1left, STATEMENT1right), rest671)
end
|  ( 85, ( ( _, ( MlyValue.STATEMENT STATEMENT2, _, STATEMENT2right)) :: _ :: ( _, ( MlyValue.STATEMENT STATEMENT1, _, _)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 219.50 "c.grm"*)Ast.IFELSE (EXP, STATEMENT1, STATEMENT2)(*#line 937.1 "c.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, IF1left, STATEMENT2right), rest671)
end
|  ( 86, ( ( _, ( MlyValue.DECSTMTS DECSTMTS, DECSTMTS1left, DECSTMTS1right)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 220.50 "c.grm"*)Ast.StmtDecStmts DECSTMTS(*#line 941.1 "c.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, DECSTMTS1left, DECSTMTS1right), rest671)
end
|  ( 87, ( ( _, ( MlyValue.RETURNSTMT RETURNSTMT, RETURNSTMT1left, RETURNSTMT1right)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 221.50 "c.grm"*)Ast.ReturnStmtCons RETURNSTMT(*#line 945.1 "c.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, RETURNSTMT1left, RETURNSTMT1right), rest671)
end
|  ( 88, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( _, BREAK1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 222.50 "c.grm"*)Ast.BREAK(*#line 949.1 "c.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, BREAK1left, SEMICOLON1right), rest671)
end
|  ( 89, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( _, CONTINUE1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 223.50 "c.grm"*)Ast.CONTINUE(*#line 953.1 "c.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, CONTINUE1left, SEMICOLON1right), rest671)
end
|  ( 90, ( ( _, ( MlyValue.BLOCK BLOCK, BLOCK1left, BLOCK1right)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 224.50 "c.grm"*)Ast.Block BLOCK(*#line 957.1 "c.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, BLOCK1left, BLOCK1right), rest671)
end
|  ( 91, ( ( _, ( MlyValue.STATEMENT STATEMENT, _, STATEMENT1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 225.50 "c.grm"*)Ast.While (EXP, STATEMENT)(*#line 961.1 "c.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, WHILE1left, STATEMENT1right), rest671)
end
|  ( 92, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.STATEMENT ((*#line 226.50 "c.grm"*)Ast.ExpCons EXP(*#line 965.1 "c.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, EXP1left, SEMICOLON1right), rest671)
end
|  ( 93, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( _, RETURN1left, _)) :: rest671)) => let val  result = MlyValue.RETURNSTMT ((*#line 228.44 "c.grm"*)Ast.RETURN(*#line 969.1 "c.grm.sml"*)
)
 in ( LrTable.NT 25, ( result, RETURN1left, SEMICOLON1right), rest671)
end
|  ( 94, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, RETURN1left, _)) :: rest671)) => let val  result = MlyValue.RETURNSTMT ((*#line 229.44 "c.grm"*)Ast.ReturnCons EXP(*#line 973.1 "c.grm.sml"*)
)
 in ( LrTable.NT 25, ( result, RETURN1left, SEMICOLON1right), rest671)
end
|  ( 95, ( ( _, ( MlyValue.SINGLECOMMENT SINGLECOMMENT, SINGLECOMMENT1left, SINGLECOMMENT1right)) :: rest671)) => let val  result = MlyValue.COMMENTSTMT ((*#line 231.44 "c.grm"*)Ast.SingleComment SINGLECOMMENT(*#line 977.1 "c.grm.sml"*)
)
 in ( LrTable.NT 26, ( result, SINGLECOMMENT1left, SINGLECOMMENT1right), rest671)
end
|  ( 96, ( ( _, ( MlyValue.MULTICOMMENT MULTICOMMENT, MULTICOMMENT1left, MULTICOMMENT1right)) :: rest671)) => let val  result = MlyValue.COMMENTSTMT ((*#line 232.44 "c.grm"*)Ast.MultiComment MULTICOMMENT(*#line 981.1 "c.grm.sml"*)
)
 in ( LrTable.NT 26, ( result, MULTICOMMENT1left, MULTICOMMENT1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID'
val extract = fn a => (fn MlyValue.PROGRAM x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Expr_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.VOID',p1,p2))
fun MULTICOMMENT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.MULTICOMMENT i,p1,p2))
fun SINGLECOMMENT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.SINGLECOMMENT i,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID',p1,p2))
fun CASE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID',p1,p2))
fun CHAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID',p1,p2))
fun CONST (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID',p1,p2))
fun CONTINUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID',p1,p2))
fun DEFAULT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID',p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID',p1,p2))
fun DOUBLE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID',p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID',p1,p2))
fun FLOAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID',p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID',p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID',p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID',p1,p2))
fun LONG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID',p1,p2))
fun REGISTER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID',p1,p2))
fun RETURN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID',p1,p2))
fun SHORT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID',p1,p2))
fun SIGNED (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID',p1,p2))
fun SIZEOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID',p1,p2))
fun STATIC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID',p1,p2))
fun STRUCT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID',p1,p2))
fun SWITCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID',p1,p2))
fun TYPEDEF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID',p1,p2))
fun UNSIGNED (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.VOID',p1,p2))
fun VOID (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID',p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID',p1,p2))
fun NULL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID',p1,p2))
fun PRINTF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID',p1,p2))
fun INCLUDE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.INCLUDE i,p1,p2))
fun INCLUDEH (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.INCLUDEH i,p1,p2))
fun DEFINE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(ParserData.MlyValue.VOID',p1,p2))
fun INTVAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(ParserData.MlyValue.INTVAL i,p1,p2))
fun FLOATVAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(ParserData.MlyValue.FLOATVAL i,p1,p2))
fun CHARVAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(ParserData.MlyValue.CHARVAL i,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(ParserData.MlyValue.ID i,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(ParserData.MlyValue.VOID',p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(ParserData.MlyValue.VOID',p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(ParserData.MlyValue.VOID',p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(ParserData.MlyValue.VOID',p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(ParserData.MlyValue.VOID',p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(ParserData.MlyValue.VOID',p1,p2))
fun LBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(ParserData.MlyValue.VOID',p1,p2))
fun RBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(ParserData.MlyValue.VOID',p1,p2))
fun QMARK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(ParserData.MlyValue.VOID',p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(ParserData.MlyValue.VOID',p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(ParserData.MlyValue.VOID',p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(ParserData.MlyValue.VOID',p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(ParserData.MlyValue.VOID',p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 51,(ParserData.MlyValue.VOID',p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 52,(ParserData.MlyValue.VOID',p1,p2))
fun MODULUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 53,(ParserData.MlyValue.VOID',p1,p2))
fun INC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 54,(ParserData.MlyValue.VOID',p1,p2))
fun DEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 55,(ParserData.MlyValue.VOID',p1,p2))
fun NE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 56,(ParserData.MlyValue.VOID',p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 57,(ParserData.MlyValue.VOID',p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 58,(ParserData.MlyValue.VOID',p1,p2))
fun GTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 59,(ParserData.MlyValue.VOID',p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 60,(ParserData.MlyValue.VOID',p1,p2))
fun LTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 61,(ParserData.MlyValue.VOID',p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 62,(ParserData.MlyValue.VOID',p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 63,(ParserData.MlyValue.VOID',p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 64,(ParserData.MlyValue.VOID',p1,p2))
fun BITAND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 65,(ParserData.MlyValue.VOID',p1,p2))
fun BITOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 66,(ParserData.MlyValue.VOID',p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 67,(ParserData.MlyValue.VOID',p1,p2))
fun COMPLEMENT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 68,(ParserData.MlyValue.VOID',p1,p2))
fun LEFTSHIFT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 69,(ParserData.MlyValue.VOID',p1,p2))
fun RIGHTSHIFT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 70,(ParserData.MlyValue.VOID',p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 71,(ParserData.MlyValue.VOID',p1,p2))
fun PLUSA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 72,(ParserData.MlyValue.VOID',p1,p2))
fun MINUSA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 73,(ParserData.MlyValue.VOID',p1,p2))
fun TIMESA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 74,(ParserData.MlyValue.VOID',p1,p2))
fun DIVA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 75,(ParserData.MlyValue.VOID',p1,p2))
fun MODULUSA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 76,(ParserData.MlyValue.VOID',p1,p2))
fun LEFTSHIFTA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 77,(ParserData.MlyValue.VOID',p1,p2))
fun RIGHTSHIFTA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 78,(ParserData.MlyValue.VOID',p1,p2))
fun BITORA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 79,(ParserData.MlyValue.VOID',p1,p2))
fun BITANDA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 80,(ParserData.MlyValue.VOID',p1,p2))
fun XORA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 81,(ParserData.MlyValue.VOID',p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 82,(ParserData.MlyValue.STRING i,p1,p2))
fun HIGHER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 83,(ParserData.MlyValue.VOID',p1,p2))
fun LOWER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 84,(ParserData.MlyValue.VOID',p1,p2))
end
end
