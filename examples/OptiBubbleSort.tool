program OptiBubbleMain {
    println(new BubbleAlgorithm().init().funct());
}

class BubbleAlgorithm {
	// The array of the numbers to sort
	var numbersToSort : Int[];
	var sizeLeftToSort : Int;
	var indexToCheck : Int;
	// counter is used for the printBubble method
	var counter : Int;
	var i : Int;
	// temporary storage for the swapBumbers method
	var tempForSwap : Int;
	
	/* Variables initialization */
    def init () : BubbleAlgorithm = {
        numbersToSort = new Int[15];
        numbersToSort[0] = 1;
        numbersToSort[1] = 5;
        numbersToSort[2] = 9;
        numbersToSort[3] = 9;
        numbersToSort[4] = 3;
        numbersToSort[5] = 5;
        numbersToSort[6] = 1;
        numbersToSort[7] = 2;
        numbersToSort[8] = 6;
        numbersToSort[10] = 4;
        numbersToSort[11] = 3;
        numbersToSort[12] = 8;
        numbersToSort[13] = 0;
        numbersToSort[14] = 9;
        return this;
    }
	
    def funct(): Int = {
    	sizeLeftToSort = numbersToSort.length;
		counter = 1;
		
		while (!(sizeLeftToSort == 0)) {
			/* indexToCheck set to 0 to have a minimum number for the algorithm */
			indexToCheck = this.printBubble();
			counter = counter + 1;
			i = 1;
			while (i < sizeLeftToSort) {
				if (numbersToSort[i] < numbersToSort[i-1]) {
					println("SWAP : " + this.swapNumbers(i-1, i));
					indexToCheck = i;
				}
				i = i + 1;
			}
			sizeLeftToSort = indexToCheck;
		}
		return 0;
	}
	
	def printBubble() : Int = {
		println ("STEP NUMBER " + counter + " : " + this.toString(numbersToSort));
		return 0;
	}
	
	def toString(tab: Int[]) : String = {
		var str : String;
		var k : Int;
		str = "";
		k = 0;
		while(k < numbersToSort.length){
			str = str + numbersToSort[k] + ", ";
			k = k + 1;
		}
		return str;
	}
	
	def swapNumbers(a : Int, b : Int) : String = {
		tempForSwap = numbersToSort[a];
		numbersToSort[a] = numbersToSort[b];
		numbersToSort[b] = tempForSwap;
		return numbersToSort[b] + " with " + numbersToSort[a];
	}
}
