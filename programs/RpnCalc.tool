object RpnCalc {
	def main(): Unit = {println(new Test().execute());}
}

class Test {
	def execute(): String = {
		var result: Number;
		
		println("-------- RPN Calculator - Test cases --------");
		println("");
		
		// Test 1
		result = new Expr().get(
			new Expr().get(new Number().set(4), new Number().set(5), new Multiply()),
			new Number().set(8),
			new Add()
		);
		println(this.printTest("4 5 * 8 + = 28", result));
		
		// Test 2
		result = new Expr().get(
			new Number().set(8),
			new Expr().get(
				new Number().set(5),
				new Number().set(4),
				new Multiply()
			),
			new Add()
		);
		println(this.printTest("8 5 4 * + = 28", result));
		
		// Test 3
		result = new Expr().get(
			new Expr().get(
				new Number().set(8),
				new Number().set(25),
				new Add()
			),
			new Number().set(0),
			new Divide()
		);
		println(this.printTest("8 25 + 0 / = NaN", result));
		
		// Test 4
		result = new Expr().get(
			new Number().set(4),
			new Expr().get(
				new Number().set(3),
				new Expr().get(
					new Number().set(2),
					new Expr().get(
						new Number().set(6),
						new Number().set(2),
						new Divide()
					),
					new Multiply()
				),
				new Add()
			),
			new Substract()
		);
		println(this.printTest("4 3 2 6 2 / * + - = -5", result));
		
		// Test 5
		result = new Expr().get(
			new Number().set(5),
			new Number().set(2),
			new Divide()
		);
		println(this.printTest("5 2 / = Int(2.5) = 2", result));
		
		return "--------------- END of tests ----------------";
	}
	
	def printTest(descr: String, result: Number): String = {
		var out: String;
		out = descr + " | Result : " + result.toString();
		return out;
	}
}


class Expr {
	def get(lhs: Number, rhs: Number, op : Op): Number = {
		var result: Number;
		result = op.apply(lhs, rhs);
		return result;
	}
}

class Number {
	var NaN: Bool;
	var value: Int;
	
	def NaN(): Number = {
		return new Number().setNaN(true);
	}
	
	def setNaN(isNaN: Bool): Number = {
		NaN = isNaN;
		return this;
	}
	
	def isNaN(): Bool = {
		return NaN;
	}
	
	def set(v: Int) : Number = {
		value = v;
		return this;
	}
	
	def value(): Int = {
		return value;
	}
	
	def toString(): String = {
		var out: String;
		
		if (this.isNaN()) {
			out = "NaN";
		}
		else {
			out = value + "";
		}
		
		return out;
	}
}

class Op {
	def apply(lhs: Number, rhs: Number): Number = {
		return new Number().NaN();
	}
	
	def toString(): String = {
		return "?";
	}
}

class Add extends Op {
	def apply(lhs: Number, rhs: Number): Number = {
		var result : Number;
		if (lhs.isNaN() || rhs.isNaN()) {
			result = new Number().NaN();
		}
		else {
			result = new Number().set(lhs.value() + rhs.value());
		}
		
		return result;
	}
	
	def toString(): String = {
		return "+";
	}
}

class Substract extends Op {
	def apply(lhs: Number, rhs: Number): Number = {
		var result : Number;
		if (lhs.isNaN() || rhs.isNaN()) {
			result = new Number().NaN();
		}
		else {
			result = new Number().set(lhs.value() - rhs.value());
		}
		
		return result;
	}
	
	def toString(): String = {
		return "-";
	}
}

class Multiply extends Op {
	def apply(lhs: Number, rhs: Number): Number = {
		var result : Number;
		if (lhs.isNaN() || rhs.isNaN()) {
			result = new Number().NaN();
		}
		else {
			result = new Number().set(lhs.value() * rhs.value());
		}
		
		return result;
	}
	
	def toString(): String = {
		return "*";
	}
}

class Divide extends Op {
	def apply(lhs: Number, rhs: Number): Number = {
		var result : Number;
		if (lhs.isNaN() || rhs.isNaN() || (rhs.value() == 0)) {
			result = new Number().NaN();
		}
		else {
			result = new Number().set(lhs.value() / rhs.value());
		}
		
		return result;
	}
	
	def toString(): String = {
		return "/";
	}
}

/*

expr = expr expr op | number
op = + | - | * | /
number = <Int> | <NaN>


new Expr().init(new Expr().init(new Number(2), new Number(3), "*"),new Number().set(4),"+")
*/