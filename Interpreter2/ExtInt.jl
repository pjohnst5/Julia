module ExtInt

push!(LOAD_PATH, pwd())

using Error
using Lexer
export parse, calc, interp, interpf

# ============ lang Functions =================================================================
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


# =========== Symbol dictionaries =====================================================
binops = Dict(:+ => +, :- => -, :* => *, :/ => /, :mod => mod)

unops = Dict(:collatz => collatz, :- => -)

other = Dict(:if0 => "taken", :with => "taken", :lambda => "taken")


# =============== helper functions ======================================================
function symbol_check(x)
	if typeof(x) != Symbol
		throw(LispError("Must be type Symbol"))
	end
	if haskey(binops, x) || haskey(unops, x) || haskey(other, x)
		throw(LispError("That's a keyword bruh"))
	end
	return x
end

function with_helper(expr2)
	dict = Dict{Symbol,AE}()
	for i = 1:length(expr2)
		symbol = symbol_check(expr2[i][1])
		if (length(expr2[i]) == 1)
			throw(LispError("No AE following id in with"))
		end
		if haskey(dict, symbol)
			throw(LispError("Can't have duplicate symbols in with"))
		end
		ae = parse(expr2[i][2])
		dict[symbol] = ae
	end
	return dict
end

function sym2AE_to_sym2Val(sym2AE, env)
	dict = Dict{Symbol,RetVal}()
	for k in keys(sym2AE)
		val = calc(sym2AE[k], env)
		dict[k] = val
    end
	return dict
end

function FAN_helper(formals, argExprs, env)
	if length(formals) != length(argExprs)
		throw(LispError("Bad arity in FAN"))
	end
	dict = Dict{Symbol, RetVal}()
	for i = 1:length(formals)
		dict[formals[i]] = calc(argExprs[i], env)
	end
	return dict
end


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

# <AE> ::= (with ( (id <AE>)* ) <AE>)
struct WithNode <: AE
	sym2AE::Dict{Symbol,AE}
  body::AE
end

# <AE> ::= (lambda (id*) <AE>)
struct FuncDefNode <: AE
  formal::Array{Symbol}  #Can have multiple of these, inside each VarRefNode is a symbol
	body::AE
end

# <AE> ::= (<AE> <AE>*)
struct FuncAppNode <: AE
  fun_expr::AE
  arg_expr::Array{AE} #Can have multiple of these
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
  formal::Array{Symbol} #Can have multiple formals to function definition
  body::AE
  env::Environment
end


# =============== Environments ======================================================================
struct EmptyEnv <: Environment
end

struct ExtendedEnv <: Environment
  sym2Val::Dict{Symbol,RetVal} #multiple
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
			formals = map(symbol_check, expr[2])
			return FuncDefNode( formals, parse(expr[3]) )

		elseif expr[1] == :with
			dict = with_helper(expr[2])
			return WithNode(dict, parse(expr[3]))

		elseif haskey(binops, expr[1])
			return BinopNode(binops[expr[1]], parse( expr[2] ), parse( expr[3] ) )
		end
	end

	#Unop check
	if length(expr) == 2
		if haskey(unops, expr[1])
 			return UnopNode(unops[expr[1]], parse(expr[2]))
		end
	end

	#If0 check
	if length(expr) == 4
		if expr[1] == :if0
			return If0Node( parse(expr[2]), parse(expr[3]) , parse(expr[4]) )
		end
	end

	#If it got here and it's a keyword, bad arity
	if haskey(binops, expr) || haskey(unops, expr) || haskey(other, expr)
		throw(LispError("Bad arity for known expression"))
	end

	aes = map(parse, expr[2:end])
	return FuncAppNode( parse(expr[1]), aes )
end

function parse( expr::Any )
  throw( LispError("Invalid type $expr") )
end


# ============= Calculating ======================================================================
function calc( ast::NumNode, env::Environment )
    return NumVal( ast.n )
end

function calc( ast::BinopNode, env::Environment )
	left = calc(ast.lhs, env)
	right = calc(ast.rhs, env)
	if typeof(left) != NumVal || typeof(right) != NumVal
		throw(LispError("Binop sides must be NumVals"))
	end
	if ast.op == binops[:/] && right.n == 0
		throw(LispError("Divide by zero error"))
	end
  return NumVal(ast.op(left.n, right.n))
end

function calc(ast::UnopNode, env::Environment)
	side = calc(ast.side, env)
	if typeof(side) != NumVal
		throw(LispError("Unop must have numVal"))
	end
	if ast.op == unops[:collatz] && side.n < 0
		throw(LispError("Collatz negative error"))
	end
	return NumVal(ast.op(side.n))
end

function calc( ast::If0Node, env::Environment )
    cond = calc( ast.cond, env )
		if typeof(cond) != NumVal
			throw(LispError("If0 condition must be NumVal"))
		end
    if cond.n == 0
        return calc( ast.zerobranch, env )
    else
        return calc( ast.nzerobranch, env )
    end
end

function calc( ast::WithNode, env::Environment )
		#Given Symbol to AE dictionary, returns
		#Symbol to RetVal dictionary
		sym2Val = sym2AE_to_sym2Val(ast.sym2AE, env)
    ext_env = ExtendedEnv( sym2Val, env )
    return calc( ast.body, ext_env )
end

function calc( ast::VarRefNode, env::EmptyEnv )
    throw( Error.LispError("Undefined variable " * string( ast.sym )) )
end

function calc( ast::VarRefNode, env::ExtendedEnv )
		# instead, look to see if ast.sym is in list of env.sym
		# and make sure it only appears once
    if haskey(env.sym2Val, ast.sym)
        return env.sym2Val[ast.sym]
    else
        return calc( ast, env.parent )
    end
end

function calc( ast::FuncDefNode, env::Environment )
    return ClosureVal( ast.formal, ast.body , env )
end

function calc( ast::FuncAppNode, env::Environment )
    closure_val = calc( ast.fun_expr, env )
		if typeof(closure_val) != ClosureVal
			throw(LispError("First AE of FAN must be closure"))
		end
		#make dictionary of closures formal symbols
		#to what you get after calcing each ast.arg_expr
		dict = FAN_helper(closure_val.formal, ast.arg_expr, env)
    ext_env = ExtendedEnv(dict, closure_val.env )
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
