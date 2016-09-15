
// This is a quick and dirty pseudo-random number generator
// It's not so impressive in itself, but is useful for other programs!

// The demo displays bar graphs of randomly generated numbers

object PRNGTest {
    def main() : Unit = {
        if(!new PRNGTestRunner().run()) { println("FAIL"); }
    }
}

class PRNGTestRunner {
    def run() : Bool = {
        var graph : BarGraph;

        println("Generating graph... this might take a few seconds");

        // you can change the width, height, and seed to get
        // a different graph
        graph = new BarGraph().init(80, 25, 43498234);
        return graph.print();
    }
}

class Random {

    var seed : Int;

    def init(pSeed: Int) : Random = {
        seed = pSeed;
        return this;
    }

    def nextInt() : Int = {
        var newseed : Int;
        newseed = seed * 69069 + 362437;
        seed = newseed;
        return newseed;
    }

    def mod(pA: Int, b: Int) : Int = {
        var a : Int; 
        a = pA;
        if (a < 0) {
            while (b < 0 - a) {
                a = a + b;
            }
            a = a + b;
        } else {
            while (b < a) {
                a = a - b; 
            }
        }
        return a;
    }

}

class BarGraph {

    var data : Int[];

    def init(width: Int, height: Int, seed: Int) : BarGraph = {
        var rand : Random;
        var num : Int;
        var i : Int;

        i = 0; 
        data = new Int[width];
        rand = new Random().init(seed);
      
        // generate width numbers between 0 and height
        while (i < width) {
            num = rand.nextInt();
            data[i] = rand.mod(num, height);
            i = i + 1;
        }
 
        return this; 
    }

    def print() : Bool = {
        var max : Int;        
        var i : Int;
        max = 0;
        i = 0;

        while (i < data.length) {
            if(max < data[i]) {
                max = data[i];
            }
            i = i + 1;
        }

        i = max;
        while (0 - 1 < i) {
            do(this.printLine(i));
            i = i - 1;
        }

        return true; 
    }

    def printLine(line: Int) : Bool = {
        var result : String;
        var width : Int;
        var column : Int;

        width = data.length;
        result = "";
        column = 0;
        while (column < width) {
            result = result + this.cell(line, column);
            column = column + 1;
        }
        println(result);
        return true;
    }

    def cell(line: Int, column: Int) : String = {
        var result : String;
        if(data[column] < line) {
            result = " ";
        } else {
            result = "#";
        }
        return result;
    }

}


