// Implementation of the two sorting algorithms BubbleSort and SelectionSort
// with a simple pseudorandom array generator (another algorithm than in
// the Maze programm)

object Sorting {
   def main() : Unit = {      
      {
         println(new ArrayPrinter().print(new PseudoRandom().getNumberArray(29,13),"Unsorted Numbers : "));
         println(new ArrayPrinter().print(new BubbleSort().sort(new PseudoRandom().getNumberArray(29, 13)),"After Bubble Sort : "));   
         println(new ArrayPrinter().print(new PseudoRandom().getNumberArray(17,4),"Unsorted Numbers : "));
         println(new ArrayPrinter().print(new SelectionSort().sort(new PseudoRandom().getNumberArray(17,4)),"After Selection Sort : "));   
      }
   }
}

class BubbleSort {   
   def sort(array : Int[]) : Int[] = {
      var tmp : Int;
      var i : Int;
      var j : Int;
      var swapped : Bool;
      
      j = array.length;
      swapped = true;
      
      while(swapped){
         swapped = false;
         i = 1;
         
         while(i < array.length) {
            if(array[i] < array[i - 1]){
               tmp = array[i - 1];
               array[i - 1] = array[i];
               array[i] = tmp;
               swapped = true;
            }
            i = i + 1;
         }
         
         j = j - 1;
      }
      return array;      
   }

}

class SelectionSort {
   def sort(array : Int[]) : Int[] = {
      var tmp : Int;
      var last : Int;
      var left : Int;
      var min : Int;
      var i : Int;
      
      last = array.length;
      left = 0;
      
      while(left < last) {
         
         i = left + 1;
         min = left;
         
         while(i < last){
            if(array[i] < array[min]){
               min = i;
            }   
            i = i + 1;
         }
         
         tmp = array[left];
         array[left] = array[min];
         array[min] = tmp;         
         left = left + 1;
      }
      return array;
   }
}

class ArrayPrinter {
   def print(array : Int[], message : String) : Int = {
      var i : Int;
      i = 0;
      println(message);
      while(i < array.length) {
         println("array[" + i + "] = " + array[i]);
         i = i + 1;
      }
      return 999999;
   }
}

class PseudoRandom {
   def getNumberArray(size : Int, start : Int) : Int[] = {
      var baseNumbers : Int[];
      var numbers : Int[];
      var i : Int;
      var count : Int;
      
      baseNumbers = this.getBaseNumbers();
      numbers = new Int[size];
      i = 0;
      count = 0;
      
      while(i < size) {
         if(count == baseNumbers.length) {
            count = 0;
         }
         numbers[i] = (i + 1) * (start + baseNumbers[count]) / 2 + 3 * count * baseNumbers[count] - i * start;
         i = i + 1;
         count = count + 1;     
      }
      
      return numbers;  
   }
   
   def getBaseNumbers() : Int[] = {
      var baseNumbers : Int[];
      baseNumbers = new Int[10];
      
      baseNumbers[0] = 12;
      baseNumbers[1] = 10;
      baseNumbers[2] = 23;
      baseNumbers[3] = 16;
      baseNumbers[4] = 35;
      baseNumbers[5] = 2;
      baseNumbers[6] = 21;
      baseNumbers[7] = 5;
      baseNumbers[8] = 57;
      baseNumbers[9] = 1;
      
      return baseNumbers;
   }
}
