object LinkedMain {
	def main() : Unit = {
		println(new LinkedList().test());
	}
}

class LinkedList {
	var first : Element;
	var null : NullElement;
	var tmp : Element;
	
	def test(): String = {
		var lk : LinkedList;
		println("construction by adding : ");
		lk = new LinkedList().init().add(3).add(12).add(2).add(4).add(10).add(7).add(9).add(22).add(17);
		println(lk.toString());
		println("removing first : ");
		lk = lk.remove(lk.getFirst());
		println(lk.toString());
		println("removing last");
		lk = lk.remove(lk.getHead());
		println(lk.toString());
		println("removing from the middle : ");
		lk = lk.remove(lk.getFirst().next().next().next());
		println(lk.toString());
		return "SUCCESS";
	}
	
	def init(): LinkedList = {
		null = new NullElement();
		first = null;
		return this;
	}
	
	def getFirst(): Element = {
		return first;
	}
	
	def getHead(): Element = {
		tmp = first;
		while(!(tmp.next() == null)){
			tmp = tmp.next();
		}
		return tmp;
	}
	
	def add(val: Int): LinkedList = {
		var elem : Element;
		elem = new Element().init(val);
		tmp = elem.setNext(null);
		
		if(first == null) first = elem;
		else tmp = this.getHead().setNext(elem);
		
		return this;
	}
	
	def remove(elem: Element): LinkedList = {
		var tmp2 : Element;
		var tmp3 : Element;
		tmp = first;
		if(first == elem){
			tmp = first.next();
			tmp2 = first.setNext(null);
			first = tmp;
		} else { 
			if (this.getHead() == elem) {
			tmp = first;
				while(!(tmp.next().next() == null)) {
					tmp = tmp.next();
				}
				tmp2 = tmp.setNext(null);
			} else {
				tmp = first;
				while(!((tmp.next() == elem) || (tmp.next() == null))) {
					tmp = tmp.next();
				}
				if(tmp.next() == null) println("ELEMENT CANNOT BE FOUND");
				else {
					tmp2 = tmp.next().next();
					tmp3 = tmp.next().setNext(null);
					tmp3 = tmp.setNext(tmp2);
				}
			}
		}
		return this;
	}
	
	def toString(): String = {
		var str : String;
		str = "";
		tmp = first;
		while(!(tmp == null)) {
			str = str + tmp.getVal() +  ", ";
			tmp = tmp.next();
		}
		return str;
	}
}


class Element {
	var value : Int;
	var next : Element;
	
	def init(val: Int): Element = {
		value = val;
		return this;
	}
	
	def getVal(): Int = {
		return value;
	}
	
	def next(): Element = {
		return next;
	}
	
	def setNext(el : Element): Element = {
		next = el;
		return el;
	}
}

class NullElement extends Element {
	def getVal(): Int = {
		println("ACCESS GETVALUE ON NULLELEMENT !!!");
		return 0;
	}
}