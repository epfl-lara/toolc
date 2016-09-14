object Dijkstra {
  def main() : Unit = {
    println("distance from source (0) to target (9): " + new DijkstraExample().run());
  }
}

class DijkstraExample {
  
  def run(): Int = {
    var graph: Graph;
    var current: Node;
    var neighbors: Int[];
    var stop: Bool;
    var i: Int;
    var test: Int;
    var neighbor: Node;
    var unit: Int;
    graph = new Graph().init();
    stop = false;
    unit = graph.get(0).setDistance(0); // Distance from source to source
    while (this.hasUnvisited(graph) && !stop) {
      current = this.findMinDistance(graph);
      println("visited: " + current.visit());
      if (!(current.distance() == 100)) { // if source has no neighbors, abort
        neighbors = current.neighbors();
        i = 0;
        while (i < neighbors.length) {
          if (!(neighbors[i] < 0)) {
            test = current.distance() + neighbors[i];
            neighbor = graph.get(i);
            if (test < neighbor.distance()) {
              unit = neighbor.setDistance(test);
              unit = neighbor.setPrevious(current.index());
            }
          }
          i = i + 1;
        }
      } else {
          stop = true;
      }
    }
    i = 0;
    while (i < 10) {
      println("distance of node " + i + ": " + graph.get(i).distance());
      i = i + 1;
    }
    
    return graph.get(9).distance();
  }
  
  def hasUnvisited(graph: Graph) : Bool = {
    return (graph.get(0).unvisited() == true) || (graph.get(1).unvisited() == true)
        || (graph.get(2).unvisited() == true) || (graph.get(3).unvisited() == true)
        || (graph.get(4).unvisited() == true) || (graph.get(5).unvisited() == true)
        || (graph.get(6).unvisited() == true) || (graph.get(7).unvisited() == true) 
        || (graph.get(8).unvisited() == true) || (graph.get(9).unvisited() == true); 
  }

  def findMinDistance(graph: Graph) : Node = {
    var minVal: Int;
    var minNode: Node;
    var currentNode: Node;
    var i: Int;
    minVal = 100; //infinity
    minNode = graph.get(0); //arbitrary choice
    i = 0;
    while (i < 10) {
      currentNode = graph.get(i);
      if ((currentNode.unvisited() == true) && (currentNode.distance() < minVal)) {
        minVal = currentNode.distance();
        minNode = currentNode;
      }
      i = i + 1;
    }
    return minNode;
  }
}
 
class Node {
  var weights: Int[]; // -1 if not connected, -2 if self (diagonal)
  var distance: Int;  // 100 if not reached yet (infinity)
  var previous: Int;  // only valid if distance != 100, else -1
  var unvisited: Bool;  // false once this node is treated, should not be tested again (BFS)
  var index: Int; // for convenience
  
  def init(w: Int[], i: Int) : Node = {
    weights = w;
    distance = 100; //infinity
    previous = 0-1;
    unvisited = true;
    index = i;
    return this;
  }
  
  def distance(): Int = {
    return distance;
  }
  def setDistance(d: Int): Int = {
    distance = d;
    return d;
  }

  def unvisited(): Bool = {
    return unvisited;
  }
  def visit(): Int = {
    unvisited = false;
    return index;
  }
  
  def setPrevious(p: Int): Int = {
    previous = p;
    return index;
  }
  
  def index(): Int = { return index; }

  def neighbors(): Int[] = { return weights; }

}

class Graph {
  var n0: Node; //source
  var n1: Node;
  var n2: Node;
  var n3: Node;
  var n4: Node;
  var n5: Node;
  var n6: Node;
  var n7: Node;
  var n8: Node;
  var n9: Node; //target
  
  def init(): Graph = {
                                      //   0    1    2    3    4    5    6    7    8    9
    n0 = new Node().init(this.newWeights(0-2,   3, 0-1, 0-1,  10, 0-1,   6, 0-1, 0-1, 0-1), 0);
    n1 = new Node().init(this.newWeights(  3, 0-2,   8, 0-1, 0-1, 0-1, 0-1, 0-1, 0-1, 0-1), 1);
    n2 = new Node().init(this.newWeights(0-1,   8, 0-2,   1,   2, 0-1, 0-1, 0-1, 0-1, 0-1), 2);
    n3 = new Node().init(this.newWeights(0-1, 0-1,   1, 0-2,   1, 0-1, 0-1, 0-1, 0-1,   0), 3);
    n4 = new Node().init(this.newWeights( 10, 0-1,   2,   1, 0-2,   4, 0-1, 0-1, 0-1, 0-1), 4);
    n5 = new Node().init(this.newWeights(0-1, 0-1, 0-1, 0-1,   4, 0-2,   3, 0-1,   1,   2), 5);
    n6 = new Node().init(this.newWeights(  6, 0-1, 0-1, 0-1, 0-1,   3, 0-2,   4, 0-1, 0-1), 6);
    n7 = new Node().init(this.newWeights(0-1, 0-1, 0-1, 0-1, 0-1, 0-1,   4, 0-2,   1, 0-1), 7);
    n8 = new Node().init(this.newWeights(0-1, 0-1, 0-1, 0-1, 0-1,   1, 0-1,   1, 0-2,   2), 8);
    n9 = new Node().init(this.newWeights(0-1, 0-1, 0-1,   0, 0-1,   2, 0-1, 0-1,   2, 0-2), 9);
    return this;
  }
  
  def newWeights(w0: Int, w1: Int, w2: Int, w3: Int, w4: Int, w5: Int, w6: Int, w7: Int, w8: Int, w9: Int): Int[] = {
    var weights: Int[];
    weights = new Int[10];
    weights[0] = w0;
    weights[1] = w1;
    weights[2] = w2;
    weights[3] = w3;
    weights[4] = w4;
    weights[5] = w5;
    weights[6] = w6;
    weights[7] = w7;
    weights[8] = w8;
    weights[9] = w9;
    return weights;
  }
  
  def get(index: Int): Node = {
    var ret: Node;
    if (index == 0) { ret = n0; }
    else { if (index == 1) { ret = n1; }
    else { if (index == 2) { ret = n2; }
    else { if (index == 3) { ret = n3; }
    else { if (index == 4) { ret = n4; }
    else { if (index == 5) { ret = n5; }
    else { if (index == 6) { ret = n6; }
    else { if (index == 7) { ret = n7; }
    else { if (index == 8) { ret = n8; } 
    else { ret = n9; }}}}}}}}}
    return ret;
  }
}
