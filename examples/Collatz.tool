object CollMain {
    def main() : Unit = {
       {
       println(new Collatz().init().check(10));
       println(new Collatz().init().check(17)); 
       println(new Collatz().init().check(123)); 
       println(new Collatz().init().check(14)); 
       println(new Collatz().init().check(243)); 
       }
    }
}
 
class Collatz {
	var r : String;
	
	def init() : Collatz = {
		r = "Series : ";
		return this;
	}
	
    def check(n : Int) : String = {
    	var void : String;
    	r = r + n + ", ";
    	if (n == 1)
    		println("");
    	else {
        	if (this.isEven(n) && !(n == 1)) {
         	void = this.check(n / 2);
        	}
        	else {
         	void = this.check(3 * n + 1);
        	}
        }
        return r;
    }
    
    def isEven(n : Int) : Bool = {
    	return !(n/2 == (n-1)/2);
    }
}