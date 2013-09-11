object PolynomialDiv {
  def main() : Unit = {
    if(new PolynomialOperations().init()){
    }

  }
  
}

/*Class for showing polynomial division.
 *Due to unsupported use of - for assignment of negative 
 *integers (0-theValue) has been used for initialization of
 *such values.
 */
class PolynomialOperations{
  var numerator : Int[];
  var denominator : Int[];
  var remainder : Int[];
  
  /*Initialization of polynomials. 
   *The arrays contain the constants in front of
   *the x-values. The first index being the highest order.
   */
  def init() : Bool = {
    numerator = new Int[4];
    denominator = new Int[2];
    numerator[0] = 1;
    numerator[1] = (0-12);
    numerator[2] = 38;
    numerator[3] = 12;
    denominator[0] = 1;
    denominator[1] = (0-2); 
    
    return this.show();
  }
  //Methods that prints the output to the terminal.
  def show() : Bool = {
    var theNum : String;
    var theDen : String;
    var result : Int[];
    
    theNum = this.getPolynomial(numerator,true);
    theDen = this.getPolynomial(denominator,true);
    println("");
    println("Polynomial Division");
    println("************************");
    println(theNum + " / (" + theDen + ")" );
    result=this.divide(numerator,denominator);
    println("********Result**********");
    println(this.getPolynomial(result,false));
    println("");
    println("********Remainder*******");
    println(this.getPolynomial(remainder,false)+ " / (" + theDen + ")" );
    
    return true;
  }
  //Recursive method for computing and printing the different steps
  //of the polynomial division.
  def divide(theNumerator: Int[], theDenominator: Int[]) : Int[] = {
    
    var theLength : Int;
    var partResult : Int;
    var newNum : Int[];
    var i : Int;
    var aux : Int;
    var resultArray : Int[];
    var newResultArray : Int[];
    
    newNum = new Int[theNumerator.length];
    i = 0;
    
    partResult = theNumerator[0]/theDenominator[0];
    
    while (i<newNum.length){
      if (i<theDenominator.length) newNum[i] = partResult*theDenominator[i];
      else newNum[i] = 0;  
      i = i + 1;
    }
    aux = this.printPoly(newNum,false);
    aux = this.printLine(22);
    newNum = this.subtract(theNumerator,newNum);
    aux = this.printPoly(newNum,true);
    
    if ((0-1) < (newNum.length-theDenominator.length)){
      resultArray = this.divide(newNum,theDenominator);
      theLength = resultArray.length;
    }
    else{
      remainder = newNum;
      theLength = 0;
      resultArray = new Int[1];
    }
    
    i = 1;
    newResultArray = new Int[theLength+1];
    newResultArray[0] = partResult;
    while(i<newResultArray.length){
      newResultArray[i] = resultArray[i-1];
      i = i + 1;
    }
    
    return newResultArray;
  }
  
  //Prints a polynomial
  def printPoly(polynomial: Int[], showZeros: Bool) : Int = {
    var polyString : String;
    
    polyString = this.printSpaces(polynomial);
    
    polyString = polyString +  this.getPolynomial(polynomial,showZeros);
    
    println(polyString);
    
    return 0;
  }
  
  //Prints a line of length theLenght
  def printLine(theLength: Int) : Int = {
    var i : Int;
    var lineToPrint : String;
    i = 0;
    lineToPrint = "-";
    
    while (i < theLength){
      lineToPrint = lineToPrint + "-";
      i = i + 1;
    }
    
    println(lineToPrint);
    return 0;
  }
  //Returns spaces to indent.
  def printSpaces(poly: Int[]) : String = {
    var i : Int;
    var spaceString : String;
    i = 1;
    spaceString = "";
    
    if ( poly.length < 4 ) spaceString = spaceString + "      ";
    if ( poly.length < 3 ) spaceString = spaceString + "        ";
    if ( poly.length < 2 ) spaceString = spaceString + "      ";
    
    return spaceString;
  }
    
  //Subtracts a polynomial from another and returns the result.
  def subtract(current: Int[], number: Int[]) : Int[] = {
    var i : Int;
    var newNum : Int[];
    i = 0;
    newNum = new Int[current.length-1];
    
    while (i<newNum.length){
      newNum[i] = current[i+1]-number[i+1];
      i = i + 1;
    }
    return newNum;
  }
  
  //Method that converts the array of constants into a String of the polynomial.
  def getPolynomial(polynomial: Int[], showZeros : Bool) : String = {
    var polyString : String;
    var oldString : String;
    var i : Int;
    var theLength : Int;
    var temp : Int;
    polyString = "";
    oldString = "";
    i = 0;
    theLength = polynomial.length;
    
    while (i<theLength){
      temp = polynomial[i];
      
      //If starting constant is negative.
      if (temp<0 && i==0) polyString = "-";
      
      //Omits the ones in front of x.
      if ( 1 < temp || (theLength-2)< i ||  temp == 0 || temp < (0-1)){
        if (temp<0) temp = temp*(0-1);
        polyString = polyString + temp; 
      }
      
      //Sets the correct constant, sign and power.
      if (i < (theLength-1)) {
        polyString = polyString + "x"; 
        if (i < (theLength-2)){
          polyString = polyString + "^" + (theLength-1-i);
        }
        if (0 < polynomial[i+1] || (showZeros && (polynomial[i+1]==0))){
          polyString = polyString + " + ";
        }
        if ( polynomial[i+1] < 0){
          polyString = polyString + " - ";
        }
      }
      
      //Omits zero values if they are not wanted.
      if (!showZeros && 0 == polynomial[i]) polyString = oldString;
      
      oldString = polyString;
      i = i + 1;
    }
    
    return polyString; 
  }
}

// Local Variables:
// mode: scala
// coding: utf-8-unix
// sentence-end-double-space: nil
// End:
