program Sort {
    println(new bubbleSort().init());        
}
 

class bubbleSort {
	
	var n : Int;
	var tab : Int[];
	
	def sort() : Int =
	{
		var na: Int;
		var t : Int;
		var i : Int;
		var aux : Int;
		na = n;
		while(!(na==0))
		{
			i = 1;
			t = 0;
			while(i<na)
			{
				if(tab[i]<tab[i-1])
				{
					aux=tab[i-1];
					tab[i-1]=tab[i];
					tab[i]=aux;
					t=i;
				}
				i = i + 1;
			}
			na=t;
		}
	
	return 0;
	}
	
	def print() : Int = {
        var j : Int;
        j = 0 ;
        while (j < n) {
            println(tab[j]);
            j = j + 1 ;
        }
		return 0;
    }
	
	def init() : Int = {
	n=10;
	tab = new Int[10];
	tab[0]=2;
	tab[1]=23;
	tab[2]=225;
	tab[3]=32;
	tab[4]=5;
	tab[5]=89;
	tab[6]=30;
	tab[7]=2;
	tab[8]=10;
	tab[9]=1733;
	do(this.print());
	do(this.sort());
	println("Ok");
	do(this.print());
	return 0;
	}


}
