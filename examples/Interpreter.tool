program Interpreter {
    println(new Fibonacci().init().toString());
}

class Fibonacci {
	def init() : Fibonacci = {
		var variables : StringIntMap;
		
		/*
		 * The interpreter will run the equivalent of the fibonacci.while program.
		 * 
		 * The inheritance graph is similar to the one we had for the whilelang project.
		 * 
		 */
		 
		var init_x  : Statement;
		var init_y  : Statement;
		var init_c  : Statement;
		
		var var_x   : Expression;
		var var_y   : Expression;
		var var_c   : Expression;
		var var_i   : Expression;
		
		var x_plus_y : Expression;
		var then     : Statement;
		var elze     : Statement;
		var ifThenElse : Statement;
		
		var body    : Statement;
		
		var forLoop : Statement;
		
		var program : Statement;
		
		variables = new StringIntMap().init();
		
		init_x = new Assign().init("x", new IntLiteral().init(1));
		init_y = new Assign().init("y", new IntLiteral().init(1));
		init_c = new Assign().init("c", new IntLiteral().init(0));
		
		var_x = new Var().init("x");
		var_y = new Var().init("y");
		var_c = new Var().init("c");
		var_i = new Var().init("i");
		
		x_plus_y = new Plus().init(var_x, var_y);
		then = new Block().init().add(new Print().init("", "x")).add(new Assign().init("x", x_plus_y));
		elze = new Block().init().add(new Print().init("", "y")).add(new Assign().init("y", x_plus_y));
		
		ifThenElse = new IfThenElse().init(var_c, then, elze);
		
		body = new Block().init().add(ifThenElse).add(new Assign().init("c", new Not().init(var_c)));
        
		forLoop = new For().init(new Assign().init("i", new IntLiteral().init(10)),
		                         new GreaterThan().init(var_i, new IntLiteral().init(0)),
		                         new Assign().init("i", new Minus().init(var_i, new IntLiteral().init(1))),
		                         body);
		
		program = new Block().init().add(init_x).add(init_y).add(init_c).add(forLoop);
		

		println("Executing program : " + program.toString());
		println("");
		println("Exit code (not working) = " + program.eval(variables));
		
		return this;
	}
	
	def toString() : String = {
		return "";
	}
}

/* =====================================================================
 *
 * Single "super" class for Expressions and Statements.
 * 
 * =====================================================================
 */

class Tree {
	def eval(variables : StringIntMap) : Int = {
		return 1;
	}
	
	def toString() : String = {
		return "";
	}
}

class Statement extends Tree {
	
}

/* =====================================================================
 *
 * Different types of "single" statements :
 * A function to print things
 * An assignment operator.
 * 
 * =====================================================================
 */

class SingleStatement extends Statement {
	
}

class Print extends SingleStatement {
	var msg : String;
	var varID : String;
	
	def init(msg_ : String, varID_ : String) : Print = {
		msg = msg_;
		varID = varID_;

		return this;
	}
	
	def eval(variables : StringIntMap) : Int = {
		println(msg + variables.get(varID));
		
		return 1;
	}
	
	def toString() : String = {
		return "println('" + msg + "', " + varID + ")";
	}
}

class Assign extends SingleStatement {
	var varID : String;
	var expr  : Expression;
	
	def init(varID_ : String, expr_ : Expression) : Assign = {
		varID = varID_;
		expr  = expr_;
		
		return this;
	}
	
	def eval(variables : StringIntMap) : Int = {
		var tmp : StringIntMap;
		
		tmp = variables.set(varID, expr.eval(variables));
		
		return 1;
	}
	
	def toString() : String = {
		return varID + " = " + expr.toString();
	}
}



/* =====================================================================
 *
 * Different types of statements :
 * A branch (if then else)
 * Two loops, while and for
 * And a block of statements.
 * 
 * =====================================================================
 */


/*
 * A block holds several statements, that are executed one after the other.
 * 
 */
class Block extends Statement {
	var first : StatementNode;
	var empty : Bool;
	
	def init() : Block = {
		empty = true;
		return this;
	}
	
	def add(statement_ : Statement) : Block = {
		var tmp : StatementNode;
		
		if (empty) {
			first = new StatementNode().init(statement_);
			empty = false;
		} else {
			tmp = first.add(new StatementNode().init(statement_));
		}
		
		return this;
	}
	
	def eval(variables : StringIntMap) : Int = {
		var n : StatementNode;
		var done : Bool;
		var last : Int;
		
		done = false;
		last = 1;
		
		if (! empty) {
			n = first;
			
			while(! done) {
				last = n.getStatement().eval(variables);
				
				if (n.hasNext()) {
					n = n.getNext();
				} else {
					done = true;
				}
			}
		}
		
		return last;
	}
	
	def toString() : String = {
		var s : String;
		var done : Bool;
		var n : StatementNode;
		
		s = "{";
		done = false;
		
		if (! empty) {
			n = first;
			
			while(! done) {
				s = s + n.getStatement().toString() + "; ";
				
				if (n.hasNext()) {
					n = n.getNext();
				} else {
					done = true;
				}
			}
		}
		
		return s + "} ";
	}
}

class IfThenElse extends Statement {
	var expr: Expression;
	var then: Statement;
	var elze: Statement;
	
	def init(expr_ : Expression, then_ : Statement, elze_ : Statement) : IfThenElse = {
		expr = expr_;
		then = then_;
		elze = elze_;
		
		return this;
	}
	
	def eval(variables : StringIntMap) : Int = {
		var r : Int;
		
		r = 1;
		
		if (expr.eval(variables) == 0) {
			r = elze.eval(variables);
		} else {
			r = then.eval(variables);
		}
		
		return r;
	}
	
	def toString() : String = {
		return "if (" + expr.toString() + ") " + then.toString() + " else " + elze.toString();
	}
}

class While extends Statement {
	var expr : Expression;
	var body : Statement;
	
	def init(expr_ : Expression, body_ : Statement) : While = {
		expr = expr_;
		body = body_;
		
		return this;
	}
	
	def eval(variables : StringIntMap) : Int = {
		var r : Int;
		
		while (! (expr.eval(variables) == 0)) {
			r = body.eval(variables);
		}
		
		return r;
	}
	
	def toString() : String = {
		return "while (" + expr.toString() + ") " + body.toString();
	}
}

class For extends Statement {
	var init: SingleStatement;
	var expr: Expression;
	var step: SingleStatement;
	var body: Statement;
	
	def init(init_: SingleStatement, expr_: Expression, step_: SingleStatement, body_: Statement) : For = {
		init = init_;
		expr = expr_;
		step = step_;
		body = body_;
		
		return this;
	}
	
	def eval(variables : StringIntMap) : Int = {
		var r : Int;
		
		r = init.eval(variables);
		
		while (! (expr.eval(variables) == 0)) {
			r = body.eval(variables);
			r = step.eval(variables);
		}
		
		return r;
	}
	
	def toString() : String = {
		return "for(" + init.toString() + "; " + expr.toString() + "; " + step.toString() + ") " + body.toString();
	}
}


/* =====================================================================
 *
 * Several mathematical operators.
 * 
 * =====================================================================
 */

class Expression extends Tree {
}

class IntLiteral extends Expression {
	var i : Int;
	
	def init(i_ : Int) : IntLiteral = {
		i = i_;
		
		return this;
	}
	
	def eval(variables : StringIntMap) : Int = {
		return i;
	}
	
	def toString() : String = {
		return i + "";
	}
}

class Plus extends Expression {
	var lhs : Expression;
	var rhs : Expression;
	
	def init(lhs_ : Expression, rhs_ : Expression) : Plus = {
		lhs = lhs_;
		rhs = rhs_;
		
		return this;
	}
	
	def eval(variables : StringIntMap) : Int = {
		return lhs.eval(variables) + rhs.eval(variables);
	}
	
	def toString() : String = {
		return lhs.toString() + " + " + rhs.toString();
	}
}

class Minus extends Expression {
	var lhs : Expression;
	var rhs : Expression;
	
	def init(lhs_ : Expression, rhs_ : Expression) : Minus = {
		lhs = lhs_;
		rhs = rhs_;
		
		return this;
	}
	
	def eval(variables : StringIntMap) : Int = {
		return lhs.eval(variables) - rhs.eval(variables);
	}
	
	def toString() : String = {
		return lhs.toString() + " - " + rhs.toString();
	}
}

class GreaterThan extends Expression {
	var lhs : Expression;
	var rhs : Expression;
	
	def init(lhs_ : Expression, rhs_ : Expression) : GreaterThan = {
		lhs = lhs_;
		rhs = rhs_;
		
		return this;
	}
	
	def eval(variables : StringIntMap) : Int = {
		var r : Int;
		
		r = 0;
		
		if (rhs.eval(variables) < lhs.eval(variables))
			r = 1;
		
		return r;
	}
	
	def toString() : String = {
		return lhs.toString() + " > " + rhs.toString();
	}
}

class Not extends Expression {
	var expr : Expression;
	
	def init(expr_ : Expression) : Not = {
		expr = expr_;
		
		return this;
	}
	
	def eval(variables : StringIntMap) : Int = {
		var r : Int;
		
		r = 0;
		
		if (expr.eval(variables) == 0)
			r = 1;
		
		return r;
	}
	
	def toString() : String = {
		return "! (" + expr.toString() + ")";
	}	
}

class Var extends Expression {
	var varID : String;
	
	def init(varID_ : String) : Var = {
		varID = varID_;
		
		return this;
	}
	
	def eval(variables : StringIntMap) : Int = {
		return variables.get(varID);
	}
	
	def toString() : String = {
		return varID;
	}
}


/* =====================================================================
 *
 * Linked lists and map implementation.
 * 
 * =====================================================================
 */

/*
 * One node of a linked list. Contains a Statement.
 * 
 * Used in the Block class, to chain statements.
 */
class StatementNode {
	var next  : StatementNode;
	var hNext : Bool;
	var value : Statement;

	def init(statement_ : Statement) : StatementNode = {
		hNext = false;
		value = statement_;
		
		return this;
	}

	def last() : StatementNode = {
		var n : StatementNode;
		
		if (hNext) {
			n = next.last();
		} else {
			n = this;
		}
		
		return n;
	}

	def add(n : StatementNode) : StatementNode = {
		var tmp : StatementNode;
		
		tmp = this.last().setNext(n);
		
		return this;
	}
	
	def getStatement() : Statement = {
		return value;
	}
	
	def hasNext() : Bool = {
		return hNext;
	}
	
	def getNext() : StatementNode = {
		return next;
	}
	
	def setNext(n : StatementNode) : StatementNode = {
		next  = n;
		hNext = true;
		
		return this;
	}
}

/*
 * A basic String to Int map implementation. Uses a linked list of TupleNode, containing the pair of values.
 *
 * Used to remember all the variables.
 */
class StringIntMap {
	var empty : Bool;
	var first : TupleNode;
	
	def init() : StringIntMap = {
		empty = true;
		
		return this;
	}
	
	def get(varID_ : String) : Int = {
		return this.get0(varID_).getSecond();
	}
	
	def get0(varID_ : String) : StringIntTuple = {
		var n : TupleNode;
		
		n = first;
		
		while (! n.getTuple().firstIs(varID_)) {
			n = n.getNext();
		}
		
		return n.getTuple();
	}
		
	
	def set(varID_ : String, value_ : Int) : StringIntMap = {
		
		if (this.has(varID_)) {
			do(this.get0(varID_).setSecond(value_));
		} else {
			do(this.add(new StringIntTuple().init(varID_, value_)));
		}
		
		return this;
	}
	
	def has(varID_ : String) : Bool = {
		var b : Bool;
		var n : TupleNode;
		var done : Bool;
		
		b = false;
		n = first;
		done = false;
		
		if (! empty) {
			while(! done) {
				if (n.getTuple().firstIs(varID_)) {
					b = true;
					done = true;
				} else if (n.hasNext()) {
					n = n.getNext();
				} else {
					done = true;
				}
			}
		}
		
		return b;
	}
	
	def add(t : StringIntTuple) : StringIntMap = {
		if (empty) {
			first = new TupleNode().init(t);
			empty = false;
		} else {
			do(first.add(new TupleNode().init(t)));
		}
		
		return this;
	}
}

/*
 * One node of a linked list. Contains a StringIntTuple.
 * 
 * Used in the StringIntMap class.
 */
class TupleNode {
	var next  : TupleNode;
	var hNext : Bool;
	var value : StringIntTuple;
	
	def init(StringIntTuple_ : StringIntTuple) : TupleNode = {
		hNext = false;
		value = StringIntTuple_;
		
		return this;
	}
	
	def last() : TupleNode = {
		var n : TupleNode;
		
		if (hNext) {
			n = next.last();
		} else {
			n = this;
		}
		
		return n;
	}
	
	def add(n : TupleNode) : TupleNode = {
		do(this.last().setNext(n));
		
		return this;
	}
	
	def getTuple() : StringIntTuple = {
		return value;
	}
	
	def hasNext() : Bool = {
		return hNext;
	}
	
	def getNext() : TupleNode = {
		return next;
	}
	
	def setNext(n : TupleNode) : TupleNode = {
		next  = n;
		hNext = true;
		
		return this;
	}
}

/*
 * A tuple, containing a String and an Int.
 * 
 * Used to associate a value to a variable.
 */
class StringIntTuple {
	var varID : String;
	var value : Int;
	
	def init(varID_ : String, value_ : Int) : StringIntTuple = {
		varID = varID_;
		value = value_;
		
		return this;
	}
	
	def firstIs(varID_ : String) : Bool = {
		return varID == varID_;
	}
	
	def secondIs(value_ : Int) : Bool = {
		return value == value_;
	}
	
	def setFirst(varID_ : String) : StringIntTuple = {
		varID = varID_;
		
		return this;
	}
	
	def setSecond(value_ : Int) : StringIntTuple = {
		value = value_;
		
		return this;
	}
	
	def getFirst() : String = {
		return varID;
	}
	
	def getSecond() : Int = {
		return value;
	}
	
	def toString() : String = {
		return "(" + varID + ", " + value + ")";
	}
}
