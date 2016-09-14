object FizzBuzz {
	def main() : Unit = {
		println(new FizzBuzzCounter().doYourThing(0,100));
	}
}

class FizzBuzzCounter {
	
	def doYourThing(from: Int, to: Int): Int = {
		var i : Int;
		i = from;
		println("Running FizzBuzz from 0 to 100");
		while(i < to) {
			if (this.divides(15,i) == true) {
				println("FizzBuzz");
			}
			else {
				if (this.divides(5,i)) {
					println("Fizz");
				}
				else {
					if (this.divides(3,i)) {
						println("Buzz");
					}
					else {
						println(i);
					}
				}
			}
			i = i + 1;
		}
		return i;
	}

	def divides(divisor: Int, number: Int) : Bool = {
		var n : Int;
		var r : Bool;
		
		n = this.mod(number, divisor);
		if (n == 0) { r = true; }
		else { r = false; }

		return r;
	}

	// From Pi.tool
    def mod(m : Int, n : Int) : Int = {
        return m - (n * (m / n));
    }
}
		
		
