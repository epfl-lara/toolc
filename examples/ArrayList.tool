//Implementation of an array list

object ArrayList {
	def main() : Unit = {
		println(new Tester().exec());
	}
}

class Tester {
	def exec() : String = {
		var arr : Collection;
		var i : Int;
		var pos : Int;
		
		arr = (new ArrayListImpl()).init();
		
		i = 0;
		while(i < 20){
			do(arr.add(5*i*i-4*i));
			i = i+1;
		}
		
		println(arr.toString());
		pos = arr.search(64);
		println("Element 64 is at position (-1 = not found): "+pos);
		
		do(arr.remove(64));
		
		println("Element 64 removed");
		println("Element 64 is at position (-1 = not found): "+arr.search(64));
		
		println(arr.toString());
		
		do(arr.insert(pos, 64));
		println("Element 64 inserted back");
		println("Element 64 is at position (-1 = not found): "+arr.search(64));
		
		println(arr.toString());
		
		return "Testing finished";
	}
}

class Collection {
	//Initialize the collection
	def init() : Collection = { println("Error, this function wasn't implemented"); return new Collection(); }
	//Get an element at a certain index
	def get(index: Int) : Int = { println("Error, this function wasn't implemented"); return 0-1; }
	//Set an element at a certain index
	def set(index: Int, value: Int) : Int = { println("Error, this function wasn't implemented"); return 0-1; }
	//Get the size of the collection
	def size() : Int = { println("Error, this function wasn't implemented"); return 0-1; }
	//Add an object to the end of the collection, returns the index
	def add(obj: Int) : Int = { println("Error, this function wasn't implemented"); return 0-1; }
	//Remove an object from the collection
	def remove(obj: Int) : Bool = { println("Error, this function wasn't implemented"); return false; }
	//Remove an index from the collection
	def removeIndex(index: Int) : Bool = { println("Error, this function wasn't implemented"); return false; }
	//Search for an object in the collection
	def search(obj: Int) : Int = { println("Error, this function wasn't implemented"); return 0; }
	//Insert an element at a certain position
	def insert(index: Int, obj: Int) : Bool = { println("Error, this function wasn't implemented"); return false; }
	//Get a human-readable representation of the collection
	def toString() : String = { println("Error, this function wasn't implemented"); return ""; }
}

class ArrayListImpl extends Collection {
	var table : Int[];
	var size : Int;
	var max : Int;
	
	def init() : Collection = {
		table = new Int[1];
		size = 0;
		max = 1;
		
		return this;
	}
	
	def grow(toAdd: Int) : Collection = {
		var i : Int;
		var temp : Int[];
		temp = new Int[max+toAdd];
		
		i = 0;
		while(i < size){
			temp[i] = table[i];
			i = i + 1;
		}
		
		table = temp;
		max = max + toAdd;
		
		return this;
	}
	
	def resizeIfNecessary(toAdd: Int) : Collection = {
		var toGrow : Int;
		toGrow = 50;
		if(max-size < toAdd){
			do(this.grow(toGrow));
		}
		
		return this;
	}
	
	def get(index: Int) : Int = {
		var val : Int;
		
		val = 0;
		if(!(index < 0) && index < size)
			val = table[index];
		
		return val;
	}
	
	
	def set(index: Int, value: Int) : Int = {
		var val : Int;
		
		val = 0;
		if(!(index < 0) && index < size)
			table[index] = value;
		
		return value;
	}
	
	def size() : Int = {
		return size;
	}
	
	def add(obj: Int) : Int = {
		do(this.resizeIfNecessary(1));
		
		table[size] = obj;
		size = size + 1;
		
		return size-1;
	}
	def remove(obj: Int) : Bool = {
		 var index : Int;
		 var found : Bool;
		 
		 index = this.search(obj);
		 
		 found = !(index < 0);
		 
		 if(found)
		 	found = found && this.removeIndex(index);
		 
		 return found;
	}
	def removeIndex(index: Int) : Bool = {
		 var valid : Bool;
		 var i : Int;
		 
		 valid = !(index < 0) && index < size;
		 
		 if(valid){
		 	i = index;
		 	
		 	while(i < size-1){
		 		table[i] = table[i+1];
				i = i+1;
		 	}
		 	size = size - 1;
		 }
		 
		 return valid;
	}
	def search(obj: Int) : Int = {
		var i : Int;
		var result : Int;
		
		result = 0-1;
		i = 0;
		
		while(i < size && result == 0-1){
			if(table[i] == obj){
				result = i;
			}
			i = i + 1;
		}
		
		return result;
	}
	def insert(index: Int, obj: Int) : Bool = {
		 var valid : Bool;
		 var i : Int;
		 
		 valid = !(index < 0) && index < size;
		 
		 if(index == size){
		 	do(this.add(obj));
		 	valid = true;
		 }else if(valid){
		 	do(this.resizeIfNecessary(1));
		 	i = size-1;
		 	
		 	while(index-1 < i){
		 		table[i+1] = table[i];
				i = i-1;
		 	}
		 	table[index] = obj;
		 	size = size + 1;
		 }
		 
		 return valid; 
	}
	
	def toString() : String = {
		var i : Int;
		var str : String;
		
		i = 0;
		str = "[";
		
		while(i < size){
			if(0 < i)
				str = str + ",";
				
			str = str + table[i];
			
			i = i + 1;
		}
		
		return str + "]";
	}
}
