############################################ EDIT MEEE #########################################

module ExtInt

push!(LOAD_PATH, pwd())

using Error
using Lexer
export parse, calc, interp, interpf

#
# ============ Functions ==================
#
function collatz( n::Real )
  return collatz_helper( n, 0 )
end

function collatz_helper( n::Real, num_iters::Int )
  if n == 1
    return num_iters
  end
  if mod(n,2)==0
    return collatz_helper( n/2, num_iters+1 )
  else
    return collatz_helper( 3*n+1, num_iters+1 )
  end
end

# =========== Dictionary ===============
symbols = Dict(:+ => +, :- => -, :* => *, :/ => /, :mod => mod, :collatz => collatz)



# ============ Classes ====================
abstract type AE
end

# <AE> ::= <number>
struct NumNode <: AE
    n::Real
end

# # <AE> ::= (+ <AE> <AE>)
# struct PlusNode <: AE
#     lhs::AE
#     rhs::AE
# end
#
# # <AE> ::= (- <AE> <AE>)
# struct MinusNode <: AE
#     lhs::AE
#     rhs::AE
# end

struct BinopNode <: AE
	op::Function
	lhs::AE
	rhs::AE
end

struct UnopNode <: AE
	op::Function
	side::AE
end

# <AE> ::= (if0 <AE> <AE> <AE>)
struct If0Node <: AE
    cond::AE
    zerobranch::AE
    nzerobranch::AE
end

# <AE> ::= (with <id> <AE> <AE>)
# <AE> ::= (with ( (id <AE>)* ) <AE>)
struct WithNode <: AE
    sym::Symbol
    binding_expr::AE
    body::AE
end

# <AE> ::= <id>
struct VarRefNode <: AE
    sym::Symbol
end

# <AE> ::= (lambda <id> <AE>)
# <AE> ::= (lambda (id*) <AE>)
struct FuncDefNode <: AE
    formal::Symbol
    body::AE
end

# <AE> ::= (<AE> <AE>)
# <AE> ::= (<AE> <AE>*)
struct FuncAppNode <: AE
    fun_expr::AE
    arg_expr::AE
end


# ============== Return values ===========================
abstract type RetVal
end

abstract type Environment
end

struct NumVal <: RetVal
    n::Real
end

struct ClosureVal <: RetVal
    formal::Symbol
    body::AE
    env::Environment
end


# =============== Environments ==========================
struct EmptyEnv <: Environment
end

struct ExtendedEnv <: Environment
    sym::Symbol
    val::RetVal
    parent::Environment
end

#
# ==================================================
#

function parse( expr::Number )
    return NumNode( expr )
end

function parse( expr::Symbol )
    return VarRefNode( expr )
end

function parse( expr::Array{Any} )

    if expr[1] == :+
        return PlusNode( parse( expr[2] ), parse( expr[3] ) )

    elseif expr[1] == :-
        return MinusNode( parse( expr[2] ), parse( expr[3] ) )

    elseif expr[1] == :if0
        return If0Node( parse(expr[2]), parse(expr[3]) , parse(expr[4]) )

    elseif expr[1] == :with
        return WithNode( expr[2], parse(expr[3]), parse(expr[4]) )

    elseif expr[1] == :lambda
        return FuncDefNode( expr[2], parse(expr[3]) )

    else
        return FuncAppNode( parse(expr[1]), parse(expr[2]) )

    end

    throw(LispError("Unknown operator!"))
end

function parse( expr::Any )
  throw( LispError("Invalid type $expr") )
end

#
# ==================================================
#

function calc( ast::NumNode, env::Environment )
    return NumVal( ast.n )
end

function calc( ast::PlusNode, env::Environment )
    lhs = calc( ast.lhs, env )
    rhs = calc( ast.rhs, env )
    return  NumVal( lhs.n + rhs.n )
end

function calc( ast::MinusNode, env::Environment )
    lhs = calc( ast.lhs, env )
    rhs = calc( ast.rhs, env )
    return  NumVal( lhs.n - rhs.n )
end

function calc( ast::If0Node, env::Environment )
    cond = calc( ast.cond, env )
    if cond.n == 0
        return calc( ast.zerobranch, env )
    else
        return calc( ast.nzerobranch, env )
    end
end

function calc( ast::WithNode, env::Environment )
    binding_val = calc( ast.binding_expr, env )
    ext_env = ExtendedEnv( ast.sym, binding_val, env )
    return calc( ast.body, ext_env )
end

function calc( ast::VarRefNode, env::EmptyEnv )
    throw( Error.LispError("Undefined variable " * string( ast.sym )) )
end

function calc( ast::VarRefNode, env::ExtendedEnv )
    if ast.sym == env.sym
        return env.val
    else
        return calc( ast, env.parent )
    end
end

function calc( ast::FuncDefNode, env::Environment )
    return ClosureVal( ast.formal, ast.body , env )
end

function calc( ast::FuncAppNode, env::Environment )
    closure_val = calc( ast.fun_expr, env )
    actual_parameter = calc( ast.arg_expr, env )
    ext_env = ExtendedEnv( closure_val.formal,
                           actual_parameter,
                           closure_val.env )
    return calc( closure_val.body, ext_env )
end

function calc( ast::AE )
    return calc( ast, EmptyEnv() )
end

#
# ==================================================
#

function interp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    ast = parse( lxd )
    return calc( ast, EmptyEnv() )
end

# evaluate a series of tests in a file
function interpf( fn::AbstractString )
  f = open( fn )

  cur_prog = ""
  for ln in eachline(f)
      ln = chomp( ln )
      if length(ln) == 0 && length(cur_prog) > 0
          println( "" )
          println( "--------- Evaluating ----------" )
          println( cur_prog )
          println( "---------- Returned -----------" )
          try
              println( interp( cur_prog ) )
          catch errobj
              println( ">> ERROR: lxd" )
              lxd = Lexer.lex( cur_prog )
              println( lxd )
              println( ">> ERROR: ast" )
              ast = parse( lxd )
              println( ast )
              println( ">> ERROR: rethrowing error" )
              throw( errobj )
          end
          println( "------------ done -------------" )
          println( "" )
          cur_prog = ""
      else
          cur_prog *= ln
      end
  end

  close( f )
end

end #module
