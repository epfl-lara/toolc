program FSM {
    println(new FSMTester().run());
}

class FSMTester {
	var fsm : FiniteStateMachine;
	def run() : String = {
		println("Starting test");
		println("This FSM checks if there are an even number of 1's in the string");
		println("-------------");
		
		println("Initializing FSM");
		fsm = new FiniteStateMachine().init();
		
		println("");
		println("Adding nodes (the first added node is the starting node)");
		do(this.addFinalNode("A"));
		do(this.addNode("B"));
		
		println("");
		println("Adding edges");
		do(this.addEdge("A", "A", 0));
		do(this.addEdge("A", "B", 1));
		do(this.addEdge("B", "B", 0));
		do(this.addEdge("B", "A", 1));
		
		println("");
		println("Running program 1");
		do(this.step(0));
		do(this.step(1));
		do(this.step(1));
		do(this.step(1));
		do(this.step(1));
		do(this.step(0));
		do(this.step(1));
		
		do(this.isFinal());
		
		do(fsm.reset());
		
		println("");
		println("Running program 2");
		do(this.step(0));
		do(this.step(1));
		do(this.step(1));
		do(this.step(0));
		do(this.step(1));
		do(this.step(0));
		do(this.step(1));
		
		do(this.isFinal());
		
		return "";
	}
	
	def addNode(name: String) : Int = {
		var index : Int;
		index = fsm.addNode(name);
		println("Added node "+name+" (index: "+index+")");
		
		return index;
	}
	
	def addFinalNode(name: String) : Int = {
		var index : Int;
		index = fsm.addFinalNode(name);
		println("Added final node "+name+" (index: "+index+")");
		
		return index;
	}
	
	def addEdge(from: String, to: String, condition: Int) : Int = {
		var index : Int;
		index = fsm.addEdge(from, to, condition);
		println("Added edge "+from+":"+condition+" => "+to+" (index: "+index+")");
		
		return index;
	}
	
	def step(input: Int) : Bool = {
		var success : Bool;
		success = fsm.step(input);
		if(success)
			println("FSM received input '"+input+"'");
		else
			println("No edge corresponds to input '"+input+"'. Ignored");
		 
		return success;
	}
	
	def isFinal() : Bool = {
		var success : Bool;
		success = fsm.isFinal();
		
		if(success)
			println("FSM is in a final state");
		else
			println("FSM isn't in a final state");
		 
		return success;
	}
}

class FiniteStateMachine {
	var nodes : NodeList;
	var edges : EdgeList;
	var initDone : Bool;
	
	var pos : Int;
	
	def init() : FiniteStateMachine = {
		initDone = true;
		nodes = (new NodeList()).init();
		edges = (new EdgeList()).init();
		
		pos = 0;
		
		return this;
	}
	
	def addNode(name: String): Int = {
		return nodes.add((new Node()).init(name));
	}
	
	def addFinalNode(name: String): Int = {
		return nodes.add((new FinalNode()).init(name));
	}
	
	def addEdge(from: String, to: String, condition: Int): Int = {
		var fromIndex: Int;
		var toIndex: Int;
		var result: Int;
		
		fromIndex = nodes.search(from);
		toIndex = nodes.search(to);
		
		if(fromIndex < 0 || toIndex < 0){
			result = 0-1;
		}else{
			result = edges.add(new Edge().init(nodes.get(fromIndex), nodes.get(toIndex), condition));
		}
		
		return result;
	}
	
	def step(input: Int): Bool = {
		var result : Bool;
		var edgeToTake : Int;
		edgeToTake = edges.search(nodes.get(pos).getName(), input);
		
		if(edgeToTake < 0){
			//No edge...
			result = false;
		}else{
			//Found edge to take!
			result = true;
			pos = nodes.search(edges.get(edgeToTake).getNext().getName());
		}
		return result;
	}
	
	def reset(): Bool = {
		pos = 0;
		return true;
	}
	
	def isFinal(): Bool = {
		return nodes.get(pos).isFinal();
	}
}

class Node {
	var name: String;
	
	def init(nodeName: String): Node = {
		name = nodeName;
		return this;
	}
	def getName(): String = {
		return name;
	}
	def isSame(nodeName: String): Bool = {
		return name == nodeName;
	}
	def isFinal() : Bool = { return false; }
}

class FinalNode extends Node {
	def isFinal() : Bool = { return true; }
}

class Edge {
	var from: Node;
	var to: Node;
	var c: Int;
	var id: String;
	def init(fromNode: Node, toNode: Node, char: Int): Edge = {
		from = fromNode;
		to = toNode;
		c = char;
		id = from.getName()+":"+c;
		
		return this;
	}
	
	def getNext(): Node = {
		return to;
	}
	
	def isSame(fromNode: String, condition: Int): Bool = {
		return (fromNode == from.getName()) && (condition == c);
	}
}


class NodeList {
	var hasElement : Bool;
	var first : NodeListElement;
	
	
	def init() : NodeList = {
		hasElement = false;
		first = new NodeListElement();
		
		return this;
	}
	
	def get(index: Int) : Node = {
		var result : Node;
		
		result = new Node();
		if(hasElement)
			result = first.get(index);
		
		return result;
	}
	
	
	def set(index: Int, value: Node) : NodeList = {
		if(hasElement)
			first = first.set(index, value);
		
		return this;
	}
	
	def search(name: String) : Int = {
		var result : Int;
		
		result = 0-1;
		if(hasElement)
			result = first.search(name);
		
		return result;
	}
	
	def size() : Int = {
		var result : Int;
		
		result = 0;
		if(hasElement)
			result = first.size();
		
		return result;
	}
	
	def add(obj: Node) : Int = {
		var result : Int;
		
		result = 0-1;
		if(hasElement)
			result = first.add(obj);
		else{
			first = new NodeListElement().init(obj);
			hasElement = true;
			result = 0;
		}
		
		return result;
	}
}

class NodeListElement {
	var n : Node;
	
	var hasNext : Bool;
	var next : NodeListElement;
	
	
	def init(node : Node) : NodeListElement = {
		n = node;
		hasNext = false;
		
		return this;
	}
	
	def get(index: Int) : Node = {
		var result : Node;
		
		result = new Node();
		if(index == 0)
			result = n;
		else{
			if(hasNext)
				result = next.get(index-1);
		}
		
		return result;
	}
	
	
	def set(index: Int, value: Node) : NodeListElement = {
		if(index == 0)
			n = value;
		else{
			if(hasNext)
				next = next.set(index-1, value);
		}
		
		return this;
	}
	
	def search(name: String) : Int = {
		var result : Int;
		
		result = 0-1;
		if(n.isSame(name))
			result = 0;
		else{
			if(hasNext){
				result = next.search(name);
				
				if(!(result < 0)){
					result = result + 1;
				}
			}
		}
		
		return result;
	}
	
	def size() : Int = {
		var result : Int;
		
		if(hasNext)
			result = 1+next.size();
		else{
			result = 1;
		}
		
		return result;
	}
	
	def add(obj: Node) : Int = {
		var result : Int;
		
		if(hasNext)
			result = 1+next.add(obj);
		else{
			next = (new NodeListElement()).init(obj);
			hasNext = true;
			result = 1;
		}
		
		return result;
	}
}

//Generics through find & replace :P

class EdgeList {
	var hasElement : Bool;
	var first : EdgeListElement;
	
	
	def init() : EdgeList = {
		hasElement = false;
		
		return this;
	}
	
	def get(index: Int) : Edge = {
		var result : Edge;
		
		result = new Edge();
		if(hasElement)
			result = first.get(index);
		
		return result;
	}
	
	
	def set(index: Int, value: Edge) : EdgeList = {
		if(hasElement)
			first = first.set(index, value);
		
		return this;
	}
	
	def search(from: String, condition: Int) : Int = {
		var result : Int;
		
		result = 0-1;
		if(hasElement)
			result = first.search(from, condition);
		
		return result;
	}
	
	def size() : Int = {
		var result : Int;
		
		result = 0;
		if(hasElement)
			result = first.size();
		
		return result;
	}
	
	def add(obj: Edge) : Int = {
		var result : Int;
		
		result = 0-1;
		if(hasElement)
			result = first.add(obj);
		else{
			first = new EdgeListElement().init(obj);
			hasElement = true;
			result = 0;
		}
		
		return result;
	}
}

class EdgeListElement {
	var n : Edge;
	
	var hasNext : Bool;
	var next : EdgeListElement;
	
	
	def init(Edge : Edge) : EdgeListElement = {
		n = Edge;
		hasNext = false;
		
		return this;
	}
	
	def get(index: Int) : Edge = {
		var result : Edge;
		
		result = new Edge();
		if(index == 0)
			result = n;
		else{
			if(hasNext)
				result = next.get(index-1);
		}
		
		return result;
	}
	
	
	def set(index: Int, value: Edge) : EdgeListElement = {
		if(index == 0)
			n = value;
		else{
			if(hasNext)
				next = next.set(index-1, value);
		}
		
		return this;
	}
	
	
	def search(from: String, condition: Int) : Int = {
		var result : Int;
		
		result = 0-1;
		if(n.isSame(from, condition))
			result = 0;
		else{
			if(hasNext){
				result = next.search(from, condition);
				
				if(!(result < 0)){
					result = result + 1;
				}
			}
		}
		
		return result;
	}
	
	def size() : Int = {
		var result : Int;
		
		if(hasNext)
			result = 1+next.size();
		else{
			result = 1;
		}
		
		return result;
	}
	
	def add(obj: Edge) : Int = {
		var result : Int;
		
		if(hasNext)
			result = 1+next.add(obj);
		else{
			next = (new EdgeListElement()).init(obj);
			hasNext = true;
			result = 1;
		}
		
		return result;
	}
}
