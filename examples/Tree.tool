program treeMain {
    println(new Top().foo() + "expected : Top");
    println(new UnderTop().foo() + "expected : UnderTop");
    println(new Bot().foo() + "expected : UnderTop"); 
    println(new Bot().bar() + "expected : Top") ;
}

class Top { 
	def foo () : Int = { 
		println("foo in Top");
		return 0 ; 
	}
	
	def bar () : Int = {
		println("bar in Top");
		return 0;
	}
}


class UnderTop extends Top  { 
	def foo () : Int = { 
		println("foo in UnderTop");
		return 1 ; 
	}
}

class Bot extends UnderTop {
	
}
 
