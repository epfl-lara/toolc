object Factorial {
    def main() : Unit = {
        println("f" + "oo" == "fo" + "o");
        println(new Fact() == new Fact());
    }
}

class Fact {
    def computeFactorial(num : Int) : Int = {
        return num;
    }
}
