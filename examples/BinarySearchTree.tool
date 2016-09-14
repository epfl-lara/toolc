object BinarySearchTree {
    def main() : Unit = {
        println(new Test().run());        
    }
}

class Tree {
	var data: Int;
	var right: Tree;
	var left: Tree;
	def isLeaf(): Bool = { return false; }
	def setData(i: Int) : Tree = { data = i; return this; }
	def getData() : Int = { return data; }
	def setRight(r: Tree) : Tree = { right = r; return this; }
	def getRight() : Tree = { return right; }
	def setLeft(l: Tree) : Tree = { left = l; return this; }
	def getLeft() : Tree = { return left; }
	def init(i: Int) : Tree = {
		data = i;
		right = new Leaf();
		left = new Leaf();
		return this;
	}
	def debugString(): String = {
		return " (" + data + left.debugString() + right.debugString() + ")";
	}
	def inOrderString(): String = {
		return left.inOrderString() + data + " " + right.inOrderString();
	}
	def size(): Int = {
		return 1 + right.size() + left.size();	
	}
	def depth(): Int = {
		var ret: Int;
		if(right.depth() < left.depth())
			ret = 1 + left.depth();
		else
			ret = 1 + right.depth();
		return ret;
	}
	def max(): Int = {
		var ret: Int;
		if(right.max() == 1337)
			ret = data;
		else if(data < right.max())
			ret = right.max();
		else
			ret = data;
		return ret;
	} 
	def contains(d: Int): Bool = {
		var ret: Bool;
		if (data < d)
			ret = right.contains(d);
		else if(d < data)
			ret = left.contains(d);
		else
			ret = true;
		return ret;
			
	}
	def add(d: Int): Tree = {
		if (data < d)
			right = right.add(d);
		else if(d < data)
			left = left.add(d);
		// else data == d, so there is nothing to add.
		return this;
	}
	def remove(d: Int): Tree = {
		var self: Tree;
		var maxValue: Int;
		self = this;
		if (data < d)
			right = right.remove(d);
		else if(d < data)
			left = left.remove(d);
		else if(right.isLeaf()) {
			self = left;
		} else if(left.isLeaf()) {
			self = right;
		} else {
			maxValue = left.max();
			left = left.remove(maxValue);
			data = maxValue;
		}
		return self;
	}
}

class Leaf extends Tree {
	def isLeaf(): Bool = { return true; }
	def debugString(): String = { return " E"; }
	def inOrderString(): String = { return ""; }
	def size(): Int = { return 0; }
	def depth(): Int = { return 0; }
	def max(): Int = { return 1337; }
	def contains(data: Int): Bool = { return false; }
	def add(data: Int): Tree = { return new Tree().init(data); }
	def remove(data: Int): Tree = { return this; }
}

class Test {
	def run(): String = {
		var n : Tree;
		n = new Tree().init(3);
		n = n.add(1);
		n = n.add(9);
		println("Creat a Tree and add 3, 1 and 9:" + n.debugString()); // (3 (1 L L) (9 L L));
		println("Test if it contains 0: " + this.bts(n.contains(0))); // false
		println("Test if it contains 3: " + this.bts(n.contains(3))); // true
		n = n.add(4);
		println("Add 4:" + n.debugString()); // (3 (1 L L) (9 (4 L L) L))
		n = n.add(0);
		println("Add 0:" + n.debugString()); // (3 (1 (0 L L) L) (9 (4 L L) L))
		println("Elements in order: " + n.inOrderString());
		println("Size: " + n.size()); // 5
		println("Depth: " + n.depth()); // 3

		println("");
		n = n.remove(3);
		println("Remove 3:" + n.debugString()); // (1 (0 L L) (9 (4 L L) L))
		n = n.remove(8);
		println("Remove 8:" + n.debugString()); // (1 (0 L L) (9 (4 L L) L))
		n = n.remove(4);
		println("Remove 4:" + n.debugString()); // (1 (0 L L) (9 L L))
		println("Size: " + n.size()); // 3
		println("Depth: " + n.depth()); // 2
		return "";
	}
	def bts(b: Bool): String = {
		var s: String;
		if(b)
			s = "true";
		else
			s = "false";
		return s;
	}
}