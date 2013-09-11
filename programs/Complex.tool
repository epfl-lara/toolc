object Complex {
	def main() : Unit = {
		{
			{
			println(new ComplexOperat().init(new ComplexRepr().init(1,1), new ComplexRepr().init(1,1),0).Print());
			println(new ComplexOperat().init(new ComplexRepr().init(1,1), new ComplexRepr().init(1,1),1).Print());
			println(new ComplexOperat().init(new ComplexRepr().init(1,1), new ComplexRepr().init(1,1),2).Print());
			}
		}
	}
}


class ComplexRepr {
	var x: Int;
	var y: Int;
	
	def init(otherx : Int, othery : Int): ComplexRepr = {
		x = otherx;
		y = othery;
		return this;
	}
	
	def conj(comp : ComplexRepr): ComplexRepr = {
		y = 0 - y;
		return this;
	}
	
	def Re(): Int = {
		return x;
	}
	
	def Im(): Int = {
		return y;
	}
	
	def squaredLength(): Int = {
		return x*x + y*y;
	}
	
	def Print(): String ={
		return x+"+i*"+y;
	}
}

class ComplexOperat {
	var left : ComplexRepr;
	var right : ComplexRepr;
	var opID : Int;
	
	def init(leftOp : ComplexRepr, rightOp : ComplexRepr, op: Int): ComplexOperat = {
		left = leftOp;
		right = rightOp;
		opID = op;
		return this;
	}
	// Op ID = 0
	def sum(): ComplexRepr = {
		var toret : ComplexRepr;
		toret = new ComplexRepr().init( left.Re()+right.Re(), left.Im()+right.Im());
		return toret;
	}
	
	// op ID = 1
	def sub(): ComplexRepr = {
		var toret : ComplexRepr;
		toret = new ComplexRepr().init( left.Re()-right.Re(), left.Im()-right.Im());
		return toret;
	}
	
	// op ID = 2
	def mul(): ComplexRepr = {
		var toret : ComplexRepr;
		var leftInt : Int;
		var rightInt : Int;
		leftInt = left.Re()*right.Re() - left.Im()*right.Im();
		rightInt = left.Re()*right.Im() + left.Im()*right.Re();
		toret = new ComplexRepr().init(leftInt, rightInt);
		return toret;
	}
	
	def Print(): String = {
		var opString : String;
		var result : ComplexRepr;
		opString = "";
		result = new ComplexRepr();
		if( opID == 0){
			result = this.sum();
			opString = "+";
		}
		if( opID == 1){
			result = this.sub();
			opString = "-";
		}
		if( opID == 2){
			result = this.mul();
			opString = "*";
		}
		
		return "( "+left.Print()+" ) "+opString+" ( "+right.Print()+" ) = ( "+result.Print()+" )" ;
	}
}