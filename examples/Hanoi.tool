program Hanoi {
    println(new Tests().launch());
}

class Tests {
	def launch(): String = {
		do(this.test(3));
		do(this.test(5));
		
		return "----- END of tests -----";
	}
	
	def test(size: Int): Board = {
		var b: Board;
		
		println("***** SIZE : " + size + " *****");
		
		b = new Board().init1(size);
		b = new RecursiveSolver().solve(b);
		return b;
	}
}

class RecursiveSolver {
	var board: Board;
	
	def solve(b: Board): Board = {
		board = b;
		println("---- Solving with recursion ----");
		b = b.print();
		return this.move(b.nRings(), 0, 2, 1);
	}
	
	def move(n: Int, from: Int, to: Int, through: Int): Board = {
		if (!(n < 1)) {
			board = this.move(n - 1, from, through, to);
			board = board.moveRing(from, to);
			board = board.print();
			board = this.move(n - 1, through, to, from);
		}
		return board;
	}
}

/* ********************************************************************** */

class MultilinePrintable {
	/**
	 * line: the n-th line to print, 0 is the bottom
	 */
	def printLine(line: Int): String = {
		return "";	// Override this
	}
}

class Board extends MultilinePrintable {
	var d_b: Bool;		// Dummy
	var d_i: Int;		// Dummy
	var d_s: String;	// Dummy
	var d_B: Board;		// Dummy
	var d_t: Tower;		// Dummy
	
	var tower0: Tower;
	var tower1: Tower;
	var tower2: Tower;
	var height: Int;	// Towers height
	var rings: Int;		// # Rings
	
	def init1(nRings: Int): Board = {
		return this.init(nRings, nRings);
	}
	
	def init(tHeight: Int, nRings: Int): Board = {
		var i: Int;
		
		height = tHeight;
		rings = nRings;
		tower0 = new Tower().init(tHeight, nRings);
		tower1 = new Tower().init(tHeight, nRings);
		tower2 = new Tower().init(tHeight, nRings);
		
		i = 0;
		while (i < nRings) {
			d_b = tower0.push(nRings - i);	// Put a ring of size (max - i) on the tower [nRings to 1]
			i = i+1;
		}
		
		return this;
	}
	
	def moveRing(from: Int, to: Int): Board = {
		d_b = this.tower(to).push(this.tower(from).pop());
		return this;
	}
	
	def printLine(line: Int): String = {
		return tower0.printLine(line) + tower1.printLine(line) + tower2.printLine(line);
	}
	
	def tower(n: Int): Tower = {
		var out: Tower;
		
		if (n == 0)
			out = tower0;
		else if (n == 1)
			out = tower1;
		else if (n == 2)
			out = tower2;
		else
			out = new Tower();
		
		return out; 
	}
	
	def nRings(): Int = {
		return rings;
	}
	
	def print(): Board = {
		var line: Int;
		
		println("");
		
		line = height - 1;
		while ((0-2) < line) {
			println(this.printLine(line));
			line = line - 1;
		}
		
		return this;
	}
}

class Tower extends MultilinePrintable {
	var height: Int;
	var width: Int;
	var rings: Int[];
	var topIndex: Int;
	var EMPTY: Int;		// "No Ring" value 
	
	def init(h: Int, w: Int): Tower = {
		var i: Int;
		height = h;
		width = w;
		EMPTY = 0-1;	// Ring of size -1
		
		rings = new Int[h];
		topIndex = 0;
		
		i = 0;
		while (i < h) {
			rings[i] = EMPTY;
			i = i+1;
		}
		
		return this;
	}
	
	def push(size: Int): Bool = {
		var out: Bool;
		
		if (!this.isFull() && (this.peek() < size)) {
			rings[topIndex] = size;
			topIndex = topIndex + 1;
			out = true;
		}
		else {
			out = false;
		}
		
		return out;
	}
	
	def pop(): Int = {
		var out: Int;
		
		if (this.isEmpty()) {
			out = EMPTY;
		}
		else {
			topIndex = topIndex - 1;
			out = rings[topIndex];
			rings[topIndex] = EMPTY;
		}
		
		return out;
	}
	
	def peek(): Int = {
		var out: Int;
		
		if (this.isEmpty()) {
			out = EMPTY;
		}
		else {
			out = rings[topIndex];
		}
		
		return out;
	}
	
	def isFull(): Bool = {
		var isFull: Bool;
		
		if (topIndex < height) {
			isFull = false;
		}
		else {
			isFull = true;
		}
		
		return isFull;
	}
	
	def isEmpty(): Bool = {
		var isEmpty: Bool;
		
		if (topIndex < 1) {
			isEmpty = true;
		}
		else {
			isEmpty = false;
		}
		
		return isEmpty;
	}
	
	def printLine(line: Int): String = {
		var padWidth: Int;
		var ring: Int;
		var out: String;
		
		if (line < 0) {
			padWidth = width;
			out = this.pad("#", padWidth, "#");
		}
		else {
			ring = rings[line];
			
			if (ring == EMPTY) {
				padWidth = width;
				out = this.pad("|", padWidth, " ");
			}
			else {
				padWidth = rings[line];		// Size of ring
				out = this.pad("+", padWidth, "-");
				
				padWidth = width - padWidth;
				out = this.pad(out, padWidth, " ");
			}
		}
		
		return out;
	}
	
	def pad(s: String, n: Int, c: String): String = {
		var out: String;
		var i: Int;
		
		out = s;
		i = 0;
		while(i < n) {
			out = c + out + c;
			i = i+1;
		}
		
		return out;
	}
}
