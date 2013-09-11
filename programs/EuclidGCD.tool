object EuclidGcd {
    def main() : Unit = {
      println(new EGCD().gcd(25,100));
    }
}
//Uses the euclid algorithm to compute
//the greatest common divisor between numbers a and b
class EGCD {

var num1 : Int;
var num2 : Int;
var res : Int;


  def gcd(a : Int, b : Int) : Int = {
     
     num1 = a;
     num2 = b;
     res = 0;
     
     if(num1 == 0) res = num2;
       else {
     
     
     while (!(num2 == 0)) {
        
        if (!((num1 < num2) || (num1 == num2) )) num1 = num1 - num2;
         else num2 = num2 - num1;
        
        
     }
     res = num1;
     }
     return res;}

}


