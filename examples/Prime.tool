//give the prime numbers less than 100

program Prime {
    println(new Algo().start(100));
}

//classes

class Algo {
	
	def start ( top : Int ) : String = {
		var i : Int;
		
		i = 2;
		while ( i < top ) {
			if ( this.isPrime( i )) {
				println(i + " is prime !");
			}
			i = i + 1;
		}
		return "done!";
	}

	def isPrime ( integer : Int ) : Bool = {
		var prime : Bool;
		var count : Int;

		prime = true;
		count = 2;
		while ( count < integer ) {
			if ( 1 < this.euclide( count, integer )) {
				prime = false;
				count = integer;
			}
			count = count + 1;
		}
		return prime;
	}

	def euclide ( first : Int, sec : Int ) : Int = {
		var temp : Int;
		var a : Int;
		var b : Int;
		var left : Int;
		var res : Int;
		
		a = first;
		b = sec;

		if ( a < b ) {
			temp = a;
			a = b;
			b = temp;
		} 
		
		left = this.mod ( a, b );
		
		if ( left == 0) {
			res = b;
		} else {
			res = this.euclide( b, left );
		}
		return res;
	}

	def mod ( x : Int, y : Int ) : Int = {
		return (x - ( ( x / y ) * y ) );
	}
}
