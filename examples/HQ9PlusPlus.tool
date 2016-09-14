
// This is a simple HQ9++ interpreter in Tool.
// Sadly, since Tool doesn't have either standard input
// nor string manipulation, it's a bit awkward. Still thought
// it was interesting for the parsing part of it!

// It reuses code from 99bottles.tool

object HQ9PlusPlus {
    def main() : Unit = {
        if(new ExampleRun().run()) { println("Done!"); }
    } 
}

class ExampleRun {
    def run() : Bool = {
        // for the purpose of this example run, we're going to run this program: QHQ+++9+
        // it's tricky because of '+++'. The specification of HQ9++, which can be found at
        // http://esoteric.voxelperfect.net/wiki/HQ9_Plus_Plus
        // doesn't specify whether '+' is left or right-associative, so we'll just
        // assume it's left-associative, as it's the easiest for us to do (no back-tracking)

        var program : Program;
        var success : Bool;

        program = new Program().init().add("Q").add("H").add("Q").add("+")
                                      .add("+").add("+").add("9").add("+");
        success = new Interpreter().eval(program);

        return true;
    }
}

class Program {
    // we handle programs up to 16KB in size
    // .. which is probably more than enough
    var data: Int[];
    var size: Int;
    var converter: Converter;

    def init() : Program = {
        size = 0;
        data = new Int[16 * 1024];
        converter = new Converter();
        return this;
    }

    def add(ch: String) : Program = {
        data[size] = converter.toAscii(ch);
        size = size + 1;
        return this;
    }

    def getSize() : Int = {
        return size;
    }

    def get(index: Int) : Int = {
        return data[index];
    }

    def toString() : String = {
        var result : String;
        result = this.toStringRecursively(0);
        return result;
    }

    def toStringRecursively(index: Int) : String = {
        var result : String;
        if(index < this.getSize()) {
            result = converter.fromAscii(this.get(index)) + this.toStringRecursively(index + 1);
        } else {
            result = "";
        }
        return result;
    }
}

class ObjectOfANewSubClassOfTheGenericSuperClass {
    // haha ;)
}

class Interpreter {
    def eval(program: Program) : Bool = {
        var accumulator : Int;
        var curr : Int;
        var sym : Int;
        var success : Bool;
        var theObject : ObjectOfANewSubClassOfTheGenericSuperClass;
        
        accumulator = 0;
        curr = 0;

        while (curr < program.getSize()) {
            sym = program.get(curr);
            if(sym == 72) { // H = Print Hello, world!
                println("Hello, world!");
            } else if(sym == 81) { // Q = Print the program's source code
                println(program.toString());
            } else if(sym == 57) { // 9 = Print 99 bottles of beer
                success = new DrunkPirate().sing();
            } else if(sym == 43) { // + = well, it depends
                if(program.get(curr + 1) == 43) { 
                    // ++ = increment the accumulator twice and [...] read the spec!
                    curr = curr + 1;
                    accumulator = accumulator + 2; 
                    theObject = new ObjectOfANewSubClassOfTheGenericSuperClass();
                } else {
                    // + = increment the accumulator
                    accumulator = accumulator + 1;
                }
            }
            curr = curr + 1;
        }
        return true;
    }
}

class DrunkPirate {
    def sing() : Bool = {
        var value : Int;
        value = this.verse(99);
        return true;
    }

    def verse(number: Int) : Int  = {
        var value : Int;
        value = 0;

        println(this.bottles(number) + " of beer on the wall, take one down, pass it around, " + this.bottles(number - 1) + " of beer on the wall.");
        if (1 < number) { value = this.verse(number - 1); }
        return value;
    }

    def bottles(number: Int) : String = {
        var value : String;
        if (number == 0) { value = "no more bottles"; } else {
            if (number == 1) { value = "1 bottle"; } else {
                value = number + " bottles";
            }
        }
        return value;
    }
}
class Converter {
   def toAscii(ch: String) : Int = {
        var value : Int;
        value = 0 - 1; // in case
        if(ch == " ") { value = 32; }
        else if(ch == "!") { value = 33; }
        else if(ch == "#") { value = 35; }
        else if(ch == "$") { value = 36; }
        else if(ch == "%") { value = 37; }
        else if(ch == "&") { value = 38; }
        else if(ch == "'") { value = 39; }
        else if(ch == "(") { value = 40; }
        else if(ch == ")") { value = 41; }
        else if(ch == "*") { value = 42; }
        else if(ch == "+") { value = 43; }
        else if(ch == ",") { value = 44; }
        else if(ch == "-") { value = 45; }
        else if(ch == ".") { value = 46; }
        else if(ch == "/") { value = 47; }
        else if(ch == "0") { value = 48; }
        else if(ch == "1") { value = 49; }
        else if(ch == "2") { value = 50; }
        else if(ch == "3") { value = 51; }
        else if(ch == "4") { value = 52; }
        else if(ch == "5") { value = 53; }
        else if(ch == "6") { value = 54; }
        else if(ch == "7") { value = 55; }
        else if(ch == "8") { value = 56; }
        else if(ch == "9") { value = 57; }
        else if(ch == ":") { value = 58; }
        else if(ch == ";") { value = 59; }
        else if(ch == "<") { value = 60; }
        else if(ch == "=") { value = 61; }
        else if(ch == ">") { value = 62; }
        else if(ch == "?") { value = 63; }
        else if(ch == "@") { value = 64; }
        else if(ch == "A") { value = 65; }
        else if(ch == "B") { value = 66; }
        else if(ch == "C") { value = 67; }
        else if(ch == "D") { value = 68; }
        else if(ch == "E") { value = 69; }
        else if(ch == "F") { value = 70; }
        else if(ch == "G") { value = 71; }
        else if(ch == "H") { value = 72; }
        else if(ch == "I") { value = 73; }
        else if(ch == "J") { value = 74; }
        else if(ch == "K") { value = 75; }
        else if(ch == "L") { value = 76; }
        else if(ch == "M") { value = 77; }
        else if(ch == "N") { value = 78; }
        else if(ch == "O") { value = 79; }
        else if(ch == "P") { value = 80; }
        else if(ch == "Q") { value = 81; }
        else if(ch == "R") { value = 82; }
        else if(ch == "S") { value = 83; }
        else if(ch == "T") { value = 84; }
        else if(ch == "U") { value = 85; }
        else if(ch == "V") { value = 86; }
        else if(ch == "W") { value = 87; }
        else if(ch == "X") { value = 88; }
        else if(ch == "Y") { value = 89; }
        else if(ch == "Z") { value = 90; }
        else if(ch == "[") { value = 91; }
        else if(ch == "]") { value = 93; }
        else if(ch == "^") { value = 94; }
        else if(ch == "_") { value = 95; }
        else if(ch == "`") { value = 96; }
        else if(ch == "a") { value = 97; }
        else if(ch == "b") { value = 98; }
        else if(ch == "c") { value = 99; }
        else if(ch == "d") { value = 100; }
        else if(ch == "e") { value = 101; }
        else if(ch == "f") { value = 102; }
        else if(ch == "g") { value = 103; }
        else if(ch == "h") { value = 104; }
        else if(ch == "i") { value = 105; }
        else if(ch == "j") { value = 106; }
        else if(ch == "k") { value = 107; }
        else if(ch == "l") { value = 108; }
        else if(ch == "m") { value = 109; }
        else if(ch == "n") { value = 110; }
        else if(ch == "o") { value = 111; }
        else if(ch == "p") { value = 112; }
        else if(ch == "q") { value = 113; }
        else if(ch == "r") { value = 114; }
        else if(ch == "s") { value = 115; }
        else if(ch == "t") { value = 116; }
        else if(ch == "u") { value = 117; }
        else if(ch == "v") { value = 118; }
        else if(ch == "w") { value = 119; }
        else if(ch == "x") { value = 120; }
        else if(ch == "y") { value = 121; }
        else if(ch == "z") { value = 122; }
        else if(ch == "{") { value = 123; }
        else if(ch == "|") { value = 124; }
        else if(ch == "}") { value = 125; }
        else if(ch == "~") { value = 126; }
        return value;
    }

    def fromAscii(ch: Int) : String = {
        var value : String;
        value = ""; // just in case
        if(ch == 32) { value = " "; }
        else if(ch == 33) { value = "!"; }
        else if(ch == 35) { value = "#"; }
        else if(ch == 36) { value = "$"; }
        else if(ch == 37) { value = "%"; }
        else if(ch == 38) { value = "&"; }
        else if(ch == 39) { value = "'"; }
        else if(ch == 40) { value = "("; }
        else if(ch == 41) { value = ")"; }
        else if(ch == 42) { value = "*"; }
        else if(ch == 43) { value = "+"; }
        else if(ch == 44) { value = ","; }
        else if(ch == 45) { value = "-"; }
        else if(ch == 46) { value = "."; }
        else if(ch == 47) { value = "/"; }
        else if(ch == 48) { value = "0"; }
        else if(ch == 49) { value = "1"; }
        else if(ch == 50) { value = "2"; }
        else if(ch == 51) { value = "3"; }
        else if(ch == 52) { value = "4"; }
        else if(ch == 53) { value = "5"; }
        else if(ch == 54) { value = "6"; }
        else if(ch == 55) { value = "7"; }
        else if(ch == 56) { value = "8"; }
        else if(ch == 57) { value = "9"; }
        else if(ch == 58) { value = ":"; }
        else if(ch == 59) { value = ";"; }
        else if(ch == 60) { value = "<"; }
        else if(ch == 61) { value = "="; }
        else if(ch == 62) { value = ">"; }
        else if(ch == 63) { value = "?"; }
        else if(ch == 64) { value = "@"; }
        else if(ch == 65) { value = "A"; }
        else if(ch == 66) { value = "B"; }
        else if(ch == 67) { value = "C"; }
        else if(ch == 68) { value = "D"; }
        else if(ch == 69) { value = "E"; }
        else if(ch == 70) { value = "F"; }
        else if(ch == 71) { value = "G"; }
        else if(ch == 72) { value = "H"; }
        else if(ch == 73) { value = "I"; }
        else if(ch == 74) { value = "J"; }
        else if(ch == 75) { value = "K"; }
        else if(ch == 76) { value = "L"; }
        else if(ch == 77) { value = "M"; }
        else if(ch == 78) { value = "N"; }
        else if(ch == 79) { value = "O"; }
        else if(ch == 80) { value = "P"; }
        else if(ch == 81) { value = "Q"; }
        else if(ch == 82) { value = "R"; }
        else if(ch == 83) { value = "S"; }
        else if(ch == 84) { value = "T"; }
        else if(ch == 85) { value = "U"; }
        else if(ch == 86) { value = "V"; }
        else if(ch == 87) { value = "W"; }
        else if(ch == 88) { value = "X"; }
        else if(ch == 89) { value = "Y"; }
        else if(ch == 90) { value = "Z"; }
        else if(ch == 91) { value = "["; }
        else if(ch == 93) { value = "]"; }
        else if(ch == 94) { value = "^"; }
        else if(ch == 95) { value = "_"; }
        else if(ch == 96) { value = "`"; }
        else if(ch == 97) { value = "a"; }
        else if(ch == 98) { value = "b"; }
        else if(ch == 99) { value = "c"; }
        else if(ch == 100) { value = "d"; }
        else if(ch == 101) { value = "e"; }
        else if(ch == 102) { value = "f"; }
        else if(ch == 103) { value = "g"; }
        else if(ch == 104) { value = "h"; }
        else if(ch == 105) { value = "i"; }
        else if(ch == 106) { value = "j"; }
        else if(ch == 107) { value = "k"; }
        else if(ch == 108) { value = "l"; }
        else if(ch == 109) { value = "m"; }
        else if(ch == 110) { value = "n"; }
        else if(ch == 111) { value = "o"; }
        else if(ch == 112) { value = "p"; }
        else if(ch == 113) { value = "q"; }
        else if(ch == 114) { value = "r"; }
        else if(ch == 115) { value = "s"; }
        else if(ch == 116) { value = "t"; }
        else if(ch == 117) { value = "u"; }
        else if(ch == 118) { value = "v"; }
        else if(ch == 119) { value = "w"; }
        else if(ch == 120) { value = "x"; }
        else if(ch == 121) { value = "y"; }
        else if(ch == 122) { value = "z"; }
        else if(ch == 123) { value = "{"; }
        else if(ch == 124) { value = "|"; }
        else if(ch == 125) { value = "}"; }
        else if(ch == 126) { value = "~"; }
        return value;
    }
}

