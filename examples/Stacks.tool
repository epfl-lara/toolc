program Stacks {
    println(new Stack().init(100).push(5).push(3).pop());
}

class Stack {
  var mem : Int[];
  var cur : Int; /* Cursor, points to the next free space */
  
  def init (s: Int) : Stack = {
    
    mem = new Int[s];
    cur = 0;
    
    return this;
  }
  
  def push (n: Int) : Stack = {
    
    var tmp : Int[];
    var i : Int;
    
    if (mem.length < cur + 1) {
      tmp = new Int[mem.length * 2];
      
      i = 0;
      while (i < mem.length) {
        tmp[i] = mem[i];
        i = i + 1;
      }
      
      mem = tmp;
    }
    
    mem[cur] = n;
    cur = cur + 1;
    
    return this;
  }
  
  def pop () : Int = {
    
    var n : Int;
    n = 0;
    
    if (0 < cur) {
      n = mem[cur - 1];
      cur = cur - 1;
    }
    
    return n;
  }
}
