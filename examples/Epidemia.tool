/* 
 * Implements epidemy simulation.
*/

program Epidemia {
    println(new City().init(925000, 75000, new Malady().init(2, 3, 9)).run(200).printResults());
}

class Malady {
	var lethalityPercent : Int; // % that a sick guy die
	var recoveryPercent : Int; // % that a sick guy recover
	var propagationPercent : Int; // % that a sick guy contaminate an healthy guy
	var immunisation : Int; // Increase with each recovery and make people harder to contaminate
	
	def init (lP : Int, rP : Int, pP : Int) : Malady = {
		lethalityPercent = lP;
		recoveryPercent = rP;
		propagationPercent = pP;
		immunisation = 0;
		return this;
	}
	
	def immunisation (nbRecover : Int) : Malady = {
		immunisation = immunisation + nbRecover;
		if(1000000 < immunisation)
			immunisation = 1000000;
		return this;
	}
	
	def getImmunisation () : Int = {
		return immunisation;
	}
	
	def getDead (sick : Int) : Int = {
		return sick * lethalityPercent / 100;
	}
	
	def getSick (sick : Int) : Int = {
		var newSick : Int;
		// The following is here to prevent integer overflow. It will work assuming that %prop < 100%
		if(sick < 2000) {
			newSick = sick * propagationPercent / 100 * (1000000 - immunisation) / 1000000;
		}else if(sick < 20000){
			newSick = sick * propagationPercent / 1000 * (1000000 - immunisation) / 100000;
		}else if(sick < 200000){
			newSick = sick * propagationPercent / 10000 * (1000000 - immunisation) / 10000;
		}else if(sick < 2000000){
			newSick = sick * propagationPercent / 100000 * (1000000 - immunisation) / 1000;
		}else if(sick < 20000000){
			newSick = sick * propagationPercent / 1000000 * (1000000 - immunisation) / 100;
		}else if(sick < 200000000){
			newSick = sick * propagationPercent / 10000000 * (1000000 - immunisation) / 10;
		}else{
			newSick = sick * propagationPercent / 100000000 * (1000000 - immunisation);
		}
		return newSick;
	}
	
	def getRecoveries (sick : Int) : Int = {
		return sick * recoveryPercent / 100;
	}
}

class City {
	var population : Int;
	var healthy : Int;
	var sick : Int;
	var dead : Int;
	var malady : Malady;
	
	def init (hea : Int, sic : Int, mal : Malady) : City = {
		healthy = hea;
		sick = sic;
		malady = mal;
		dead = 0;
		population = hea + sic;
		return this;
	}
	
	def run (nbOfRun : Int) : City = {
		var k : Int;
		var nwDead : Int;
		var nwSick : Int;
		var nwHeal : Int;
		
		k = 0;
		
		while(k < nbOfRun) {
			
			if(this.mod(k, 1) == 0) { 
				println("RUN NUMBER " + k);
				println("Population");
				println(population);
				println("Healthy");
				println(healthy);
				println("Sick");
				println(sick);
				println("Dead");
				println(dead);
				println("Immunisation");
				println(malady.getImmunisation());
				println("");
				println("");
			}

			k = k + 1;
			nwDead = malady.getDead(sick);
			nwSick = malady.getSick(sick);
			nwHeal = malady.getRecoveries(sick);
			
			if(sick < nwDead)
				nwDead = sick;
			if(healthy < nwSick)
				nwSick = healthy;
			if(sick - nwDead < nwHeal)
				nwHeal = sick - nwDead;
			
			malady = malady.immunisation(nwHeal);
			dead = dead + nwDead;
			population = population - nwDead;
			sick = sick - nwDead - nwHeal + nwSick;
			healthy = healthy - nwSick + nwHeal;
		}
		
	return this;
	}
	
	def printResults() : Int = {
		println("FINAL RUN");
		println("Final population");
		println(population);
		println("Final healthy");
		println(healthy);
		println("Final sick");
		println(sick);
		println("Final dead");
		println(dead);
		println("Immunisation");
		return malady.getImmunisation();
	}
	def mod(i : Int, j : Int) : Int = { return i - (i / j * j); }
}
