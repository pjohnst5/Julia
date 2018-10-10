module RudInt

push!(LOAD_PATH, pwd())

using Error
using Lexer
export parse, calc, interp

#
# ==================================================
#

# ============ Functions ==============
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




# =================Classes========================
abstract type AE
end

struct NumNode <: AE
	n::Real
end

struct BinopNode <: AE
	op::Function
	lhs::AE
	rhs::AE
end

struct UnopNode <: AE
	op::Function
	side::AE
end



#
# ==================================================
#

function parse( expr::Number )
    return NumNode( expr )
end

function parse( expr::Array{Any} )
	#Binop check
	if length(expr) == 3
		if expr[1] == :+
	        return BinopNode(symbols[:+], parse( expr[2] ), parse( expr[3] ) )
	    elseif expr[1] == :-
	        return BinopNode(symbols[:-], parse( expr[2] ), parse( expr[3] ) )
		elseif expr[1] == :*
			return BinopNode(symbols[:*], parse(expr[2]), parse(expr[3]))
		elseif expr[1] == :/
			return BinopNode(symbols[:/], parse(expr[2]), parse(expr[3]))
		elseif expr[1] == :mod
			return BinopNode(symbols[:mod], parse(expr[2]), parse(expr[3]))
		end
	end

	#Unop check
	if length(expr) == 2
		if expr[1] == :collatz
			return UnopNode(symbols[:collatz], parse(expr[2]))
		elseif expr[1] == :-
			return UnopNode(symbols[:-], parse(expr[2]))
		end
	end

    throw(LispError("Unknown operator! Or Bad arity!"))
end

function parse( expr::Any )
  throw( LispError("Invalid type $expr") )
end

#
# ==================================================
#

function calc( ast::NumNode )
    return ast.n
end

function calc( ast::BinopNode )
	if ast.op == symbols[:/] && calc(ast.rhs) == 0
		throw(LispError("Divide by zero error"))
	end
    return ast.op(calc( ast.lhs ), calc( ast.rhs ))
end

function calc(ast::UnopNode)
	if ast.op == symbols[:collatz] && calc(ast.side) < 0
		throw(LispError("Collatz negative error"))
	end
	return ast.op(calc(ast.side))
end


#
# ==================================================
#

function interp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    ast = parse( lxd )
    return calc( ast )
end

end #module
