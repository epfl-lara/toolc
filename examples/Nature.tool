/*small test programm in tool with basic class inheritance*/

program Nature {
    println ( new Setup().start(8, "X") );
}

//class
class Setup {

	def start ( h : Int, t : String ) : String = {
		println( new Fir().start(h, t));
		println( new AppleTree().start(t));
		return "done!";
	}
}

class Tree {
	var height : Int;
	var type : String;
}

class Fir extends Tree{
	var row : String;
	var i : Int;
	var j : Int;

	def start(h : Int, t : String ) : String = {
		height = h;
		type = t;
		println(this.constructTree());
		return "";
	}

	def constructTree() : String = {
		i = 0;
		while ( i < height ) {
			row = "";
			j = 0;
			while ( j < ( height - 1 - i ) ) {
				row = row + " ";
				j = j + 1;
			} 
			j = 0;
			while ( j < ( 1 + ( 2 * i) ) ) {
				row = row + type;
				j = j + 1;
			}
			i = i + 1;
			println(row);
		}

		i = 0;
		while ( i < 2 ) {
			row = "";
			j = 0;
			while ( j < ( height - 2 ) ) {
				row = row +	" ";
				j = j + 1;
			}
			j = 0;
			while ( j < 3 ) {
				row = row + "|";
				j = j + 1;
			}
			i = i + 1;
			println ( row );
		}
		return "A fir!";
	}
}

class AppleTree extends Tree {
	var i : Int;
	var j : Int;
	var row : String;

	def start(t : String ) : String = {
		height = 4;
		type = t;
		println(this.constructAppleTree());
		return "";
	}

	def constructAppleTree() : String = {
		i = 0;
		while ( i < ( height / 2 ) ) {
			row = "";
			j = 0;
			while ( j < ( height - 3 - i ) ) {
				row = row + " ";
				j = j + 1;
			}
			j = 0;
			while ( j < ( 6 + 2 * i ) ) {
				row = row + type;
				j = j + 1;
			}
			i = i + 1;
			println ( row );
		}
		
		
		while ( i < height ) {
			row = "";
			j = 0;
			while ( j < ( i - ( height / 2 ) ) ) {
				row = row + " ";
				j = j + 1;
			}
			j = 0;
			while ( j < ( height + 4 - ( 2 * ( i - ( height / 2 ) ) ) ) ) {
				row = row + type;
				j = j + 1;
			} 
			i = i + 1;
			println ( row );
		}

		i = 0;
		while ( i < 2 ) {
			row = "";
			j = 0;
			while ( j < ( height - 1 ) ) {
				row = row +	" ";
				j = j + 1;
			}
			j = 0;
			while ( j < 2 ) {
				row = row + "|";
				j = j + 1;
			}
			i = i + 1;
			println ( row );
		}

		return "An apple tree!";
	}
}

	
