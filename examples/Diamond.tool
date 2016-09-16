program Diamond {
    if(new DiamondPrinter().run(6)) 
        println("Success!");
    else 
        println("Error");
}

class DiamondPrinter {
	def run(num: Int) : Bool = {
		var line : String;
		var i : Int;
		var w : Int;
		var j : Int;
		
		i = 0;
		
		while(i < 2*num+1){
			line = "";
			w = 2*i+1;
			if(num < i)
				w = 2*(2*num-i)+1;
			
			j = 0;
			while(j < (2*num+1-w)/2){
				line = line + " ";
				j = j+1;
			}
			j = 0;
			while(j < w){
				line = line + "*";
				j = j+1;
			}
			j = 0;
			while(j < (2*num+1-w)/2){
				line = line + " ";
				j = j+1;
			}
			
			println(line);
			
			i = i+1;
		}
		
		return true;
	}
}
