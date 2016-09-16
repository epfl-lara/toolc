program Rot13 {
    // shows the translation table for rot13
    println(new Sample().run());
}

class Decoder {

  def modulo(left : Int, right : Int) : Int = {
    var m : Int; m = left - right*(left/right);
    if (left < 0) m = right + m; return m;
  }

  def int2char(int : Int) : String = {
    // we use our own character code
    var char : String;
    if (int == 0) char = "A";
    else if (int == 1) char = "B";
    else if (int == 2) char = "C";
    else if (int == 3) char = "D";
    else if (int == 4) char = "E";
    else if (int == 5) char = "F";
    else if (int == 6) char = "G";
    else if (int == 7) char = "H";
    else if (int == 8) char = "I";
    else if (int == 9) char = "J";
    else if (int == 10) char = "K";
    else if (int == 11) char = "L";
    else if (int == 12) char = "M";
    else if (int == 13) char = "N";
    else if (int == 14) char = "O";
    else if (int == 15) char = "P";
    else if (int == 16) char = "Q";
    else if (int == 17) char = "R";
    else if (int == 18) char = "S";
    else if (int == 19) char = "T";
    else if (int == 20) char = "U";
    else if (int == 21) char = "V";
    else if (int == 22) char = "W";
    else if (int == 23) char = "X";
    else if (int == 24) char = "Y";
    else if (int == 25) char = "Z";
    else {
      char = "*";
       println("ERROR: character out of range");
    }
    return char;
  }

  def decode(chars : Int[]) : String = {
    var i : Int;
    var m : String;
    i = 0; m = "";
    while (i < chars.length) {
      m = m + this.int2char(this.modulo(chars[i]-13, 26));
      i = i + 1;
    }
    return m;
  }
}

class Sample {
  def run() : String = {
    var chars : Int[];
    chars = new Int[10];
    println("URYYBJBEYQ");
    chars[0] = 20;
    chars[1] = 17;
    chars[2] = 24;
    chars[3] = 24;
    chars[4] = 1;
    chars[5] = 9;
    chars[6] = 1;
    chars[7] = 4;
    chars[8] = 24;
    chars[9] = 16;
    return new Decoder().decode(chars);
  }
}

// Local Variables:
// mode: scala
// coding: utf-8-unix
// sentence-end-double-space: nil
// End:
