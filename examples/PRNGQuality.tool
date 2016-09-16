program PRNGQualityTest {
    if(!new PRNGQualityTestRunner().run()) { println("fail"); }
}

class PRNGQualityTestRunner {
    def run() : Bool = {
        var graph : BarGraph;
        var seed : Int;
        var result : Bool;
        var width : Int;
        var height : Int;

        width = 80;
        height = 30;

        seed = 234098520;

        println("FastPRNG graph");
        graph = new BarGraph().init(width, height, new FastPRNG().init(seed));
        result = graph.print();

        println("FastPRNG graph (sorted)");
        result = graph.sort();
        result = graph.print();

        println("StupidPRNG graph");
        graph = new BarGraph().init(width, height, new StupidPRNG().init(seed));
        result = graph.print();

        println("StupidPRNG graph (sorted)");
        result =graph.sort();
        result = graph.print();

        return result;
    }
}

class Random {

    var seed : Int;

    def init(pSeed: Int) : Random = {
        seed = pSeed;
        return this;
    }

    def nextInt() : Int = {
        return 0 - 1;
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

class StupidPRNG extends Random {

    def nextInt() : Int = {
        var newseed : Int;
        newseed = (seed + 1298000) * 3;
        seed = newseed;
        return newseed;
    }

}

class FastPRNG extends Random {

    def nextInt() : Int = {
        var newseed : Int;
        newseed = seed * 69069 + 362437;
        seed = newseed;
        return newseed;
    }

}

class BarGraph {

    var data : Int[];

    def init(width: Int, height: Int, rand: Random) : BarGraph = {
        var num : Int;
        var i : Int;

        i = 0; 
        data = new Int[width];
      
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
        var success : Bool;
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
            success = this.printLine(i);
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

    def sort() : Bool = {
        var sorted : Bool;
        var i : Int;
        var j : Int;

        j = 0;
        sorted = false;

        while (!sorted) {
            sorted = true;
            i = 0;
            while (i < data.length - 1) {
                if(data[i + 1] < data[i]) {
                    sorted = false;
                    j = data[i + 1];
                    data[i + 1] = data[i];
                    data[i] = j;
                }
                i = i + 1;
            }
        }

        return true;
    }

}



