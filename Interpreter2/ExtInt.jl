module ExtInt

push!(LOAD_PATH, pwd())

using Error
using Lexer
export parse, calc, interp, interpf

#
# ============ Functions =================================================================
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

function parse_helper(x)
	parse(x)
end


# =========== Symbols dictionaries =====================================================
binops = Dict(:+ => +, :- => -, :* => *, :/ => /, :mod => mod)

unops = Dict(:collatz => collatz, :- => -)

other = Dict(:if0 => "taken", :with => "taken", :lambda => "taken")

# ============ Nodes =====================================================================
abstract type AE
end

# <AE> ::= <number>
struct NumNode <: AE
    n::Real
end

# <AE> ::= + through mod
struct BinopNode <: AE
	op::Function
	lhs::AE
	rhs::AE
end

#<AE> ::= collatz and -
struct UnopNode <: AE
	op::Function
	side::AE
end

# <AE> ::= <id>
struct VarRefNode <: AE
    sym::Symbol
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
		sym2AE::Dict{Symbol,AE}()
    #sym::Symbol           #Can have multiple of these connected to
    #binding_expr::AE      #these
    body::AE
end

# <AE> ::= (lambda <id> <AE>)
# <AE> ::= (lambda (id*) <AE>)
struct FuncDefNode <: AE
    formal::Symbol[]  #Can have multiple of these
    body::AE
end

# <AE> ::= (<AE> <AE>)
# <AE> ::= (<AE> <AE>*)
struct FuncAppNode <: AE
    fun_expr::AE
    arg_expr::AE[] #Can have multiple of these
end


# ============== Return values (Numbers and Closures) ===============================================
abstract type RetVal
end

abstract type Environment
end

struct NumVal <: RetVal
    n::Real
end

struct ClosureVal <: RetVal
    formal::Symbol[]  #Should also be a list of symbols?
    body::AE
    env::Environment
end


# =============== Environments ======================================================================
struct EmptyEnv <: Environment
end

struct ExtendedEnv <: Environment
		sym2Val::Dict{Symbol,RetVal}()
		#sym::Symbol  #Can have multiple of these connect to
    #val::RetVal  #these
    parent::Environment
end


# ================== Parsing ========================================================================
function parse( expr::Number )
    return NumNode( expr )
end

function parse( expr::Symbol )
		if haskey(binops, expr) || haskey(unops, expr) || haskey(other, expr)
			throw(LispError("That's a keyword bruh"))
		end
    return VarRefNode( expr )
end

function parse( expr::Array{Any} )
	#lambda, With, and Binop check
	if length(expr) == 3
		if expr[1] == :lambda
				#TODO handle case where expr[2] is an array of symbols
				fomals = map(parse_helper, expr[2])
			return FuncDefNode( expr[2], parse(expr[3]) )
		elseif expr[1] == :with
				#TODO handle case where expr[2] is arrays of bindings
			return
		elseif haskey(binops, expr[1])
			return BinopNode(symbols[expr[1]], parse( expr[2] ), parse( expr[3] ) )
		end
	end

	#Unop check
	if length(expr) == 2
		if haskey(unops, expr[1])
 			return UnopNode(symbols[expr[1]], parse(expr[2]))
		end
	end

	#If0 check
	if length(expr) == 4
		if expr[1] == :if0
			return If0Node( parse(expr[1]), parse(expr[3]) , parse(expr[4]) )
		end
	end

	#If it got here and it's a keyword, bad arity
	if haskey(binops, expr) || haskey(unops, expr) || haskey(other, expr)
		throw(LispError("Bad arity for known expression"))
	end

	args = map(parse_helper, expr[2:end])
	return FuncAppNode( parse(expr[1]), args )


		# if expr[1] == :+
    #     return PlusNode( parse( expr[2] ), parse( expr[3] ) )
	#
    # elseif expr[1] == :-
    #     return MinusNode( parse( expr[2] ), parse( expr[3] ) )
	#
    # elseif expr[1] == :if0
    #     return If0Node( parse(expr[2]), parse(expr[3]) , parse(expr[4]) )
	#
    # elseif expr[1] == :with
    #     return WithNode( expr[2], parse(expr[3]), parse(expr[4]) )
	#
    # elseif expr[1] == :lambda
    #     return FuncDefNode( expr[2], parse(expr[3]) )
	#
    # else
    #     return FuncAppNode( parse(expr[1]), parse(expr[2]) )
	#
    # end

    throw(LispError("Unknown operator!"))
end

function parse( expr::Any )
  throw( LispError("Invalid type $expr") )
end


# ============= Calculating ======================================================================
function calc( ast::NumNode, env::Environment )
    return NumVal( ast.n )
end

function calc( ast::BinopNode, env::Environment )
	if ast.op == symbols[:/] && calc(ast.rhs) == 0
		throw(LispError("Divide by zero error"))
	end
		#TODO: error checking, see if calc of left is NumVal etc.
    return ast.op(calc( ast.lhs ), calc( ast.rhs ))
end

function calc(ast::UnopNode, env::Environment)
	if ast.op == symbols[:collatz] && calc(ast.side) < 0
		throw(LispError("Collatz negative error"))
	end
	return ast.op(calc(ast.side))
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
		#instead, look to see if ast.sym is in list of env.sym
		# and make sure it only appears once
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


# =============== Interpreting =========================================================================
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
