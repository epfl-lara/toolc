object HanoiTowers{
    def main(): Unit = {
    println(new Hanoi().minStep(4));
    }
    }
//Computes the minimal number of steps to resolve .
//Hanoi towers problem for n disks.
class Hanoi{

var ret : Int;

def minStep(n : Int) : Int = {
   if ( n == 1) ret = 1; else 
   ret = 2*(this.minStep(n-1)) + 1;
   return ret;
}


}
