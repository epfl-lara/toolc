object NewString {
	def main(): Unit = {
		{
		println(new NewStringOperators().concat(new StringBuilder().banana(),new StringBuilder().banana()).print());
		println(new NewStringOperators().subString(new StringBuilder().banana(), 0, 0).print());
		println(new NewStringOperators().toCamelCase(new StringBuilder().banana()).print());
		}
	}
}


class NewStringRep{
	var array: Int[];
	var size: Int;
	var inter: NewStringInterpreter;
	
	def init(arr : Int[], siz : Int): NewStringRep = {
		array = arr;
		size = siz;
		inter = new NewStringInterpreter();
		return this;
	}
	
	def print(): String = {
		var count : Int;
		var toret : String;
		toret ="";
		count = 0;
		while(count < size){
			toret=toret+inter.interpretInt(array[count]);
			count = count + 1;
		}
		return toret;
	}
	
	def getSize(): Int = {
		return size;
	}
	
	def getCharAt(at : Int): Int = {
		return array[at];
	}

}

class NewStringOperators{
	def concat(s1: NewStringRep, s2: NewStringRep): NewStringRep = {
		var arr : Int[];
		var size: Int;
		var count : Int;
		count = 0;
		size = s1.getSize() +s2.getSize();
		arr = new Int[size];
		
		while(count < s1.getSize()){
			arr[count] = s1.getCharAt(count);
			count = count + 1 ;
		}
		count = 0;
		while(count < s2.getSize()){
			arr[count+s1.getSize()] = s2.getCharAt(count);
			count = count + 1;
		}
		return new NewStringRep().init(arr, size);
	}
	
	def subString(str : NewStringRep ,from: Int, to: Int): NewStringRep = {
		var toretArr: Int[];
		var toretSize: Int;
		var count: Int;
		count = 0;
		if(to < from){
			toretSize = to;
			to = from;
			from = toretSize;
		}
		if(from < 0){
			from = 0;
		}
		if( str.getSize()-1 < to){
			to = str.getSize()-1;
		}
		
		toretSize = to - from + 1;
		toretArr = new Int[toretSize];
		count = 0;
		while(count < toretSize){
			toretArr[count] = str.getCharAt(from+count);
			count = count + 1;
		}
		
		return new NewStringRep().init(toretArr, toretSize);
	}
	
	def toCamelCase(str: NewStringRep): NewStringRep = {
		var mod: Utils;
		var toretArr: Int[];
		var toretSize: Int;
		var count: Int;
		mod = new Utils();
		toretSize = str.getSize();
		toretArr = new Int[str.getSize()];
		count = 0;
		while(count < toretSize){
			if(mod.mod(count,2) == 0){
				if(str.getCharAt(count)<0){
					toretArr[count] = str.getCharAt(count);
				}else{
					toretArr[count] = 0 - str.getCharAt(count);
				}
			}else{
				if(str.getCharAt(count)<0){
					toretArr[count] = 0 - str.getCharAt(count);
				}else{
					toretArr[count] = str.getCharAt(count);
				}	
			}
			count = count + 1;
		}
		return new NewStringRep().init(toretArr, toretSize);
	}
}

class NewStringInterpreter{
	def interpretInt (toInter : Int): String = {
		var toret: String;
		toret="";
		if(toInter < 0){
			toret = this.interpretUpperCase(0-toInter);
		}else{
			toret = this.interpretLowerCase(toInter);
		}
		return toret;
	}
	
	def interpretUpperCase(toInter: Int): String = {
		var toret : String;
		toret = "";
		if(toInter == 1){
			toret = "A";
		}
		if(toInter == 2){
			toret = "B";
		}
		if(toInter == 3){
			toret = "C";
		}
		if(toInter == 4){
			toret = "D";
		}
		if(toInter == 5){
			toret = "E";
		}
		if(toInter == 6){
			toret = "F";
		}
		if(toInter == 7){
			toret = "G";
		}
		if(toInter == 8){
			toret = "H";
		}
		if(toInter == 9){
			toret = "I";
		}
		if(toInter == 10){
			toret = "J";
		}
		if(toInter == 11){
			toret = "K";
		}
		if(toInter == 12){
			toret = "L";
		}
		if(toInter == 13){
			toret = "M";
		}
		if(toInter == 14){
			toret = "N";
		}
		if(toInter == 15){
			toret = "O";
		}
		if(toInter == 16){
			toret = "P";
		}
		if(toInter == 17){
			toret = "Q";
		}
		if(toInter == 18){
			toret = "R";
		}
		if(toInter == 19){
			toret = "S";
		}
		if(toInter == 20){
			toret = "T";
		}
		if(toInter == 21){
			toret = "U";
		}
		if(toInter == 22){
			toret = "V";
		}
		if(toInter == 23){
			toret = "W";
		}
		if(toInter == 24){
			toret = "X";
		}
		if(toInter == 25){
			toret = "Y";
		}
		if(toInter == 26){
			toret = "Z";
		}
		return toret;
	}
	
	def interpretLowerCase(toInter: Int): String = {
		var toret : String;
		toret = "";
		if(toInter == 1){
			toret = "a";
		}
		if(toInter == 2){
			toret = "b";
		}
		if(toInter == 3){
			toret = "c";
		}
		if(toInter == 4){
			toret = "d";
		}
		if(toInter == 5){
			toret = "e";
		}
		if(toInter == 6){
			toret = "f";
		}
		if(toInter == 7){
			toret = "g";
		}
		if(toInter == 8){
			toret = "h";
		}
		if(toInter == 9){
			toret = "i";
		}
		if(toInter == 10){
			toret = "j";
		}
		if(toInter == 11){
			toret = "k";
		}
		if(toInter == 12){
			toret = "l";
		}
		if(toInter == 13){
			toret = "m";
		}
		if(toInter == 14){
			toret = "n";
		}
		if(toInter == 15){
			toret = "o";
		}
		if(toInter == 16){
			toret = "p";
		}
		if(toInter == 17){
			toret = "q";
		}
		if(toInter == 18){
			toret = "r";
		}
		if(toInter == 19){
			toret = "s";
		}
		if(toInter == 20){
			toret = "t";
		}
		if(toInter == 21){
			toret = "u";
		}
		if(toInter == 22){
			toret = "v";
		}
		if(toInter == 23){
			toret = "w";
		}
		if(toInter == 24){
			toret = "x";
		}
		if(toInter == 25){
			toret = "y";
		}
		if(toInter == 26){
			toret = "z";
		}
		return toret;
	}
}

class StringBuilder{
	def banana(): NewStringRep = {
		var toret: Int[];
		var size: Int;
		size = 6;
		toret = new Int[size];
		toret[0] = 0-2;
		toret[1] = 1;
		toret[2] = 14;
		toret[3] = 1;
		toret[4] = 14;
		toret[5] = 1;
		return new NewStringRep().init(toret, size);
	}
}

class Utils{
	def mod(n: Int, p: Int): Int = {
		var toret : Int;
		toret =n-p*(n/p);
		return toret;
	}
}