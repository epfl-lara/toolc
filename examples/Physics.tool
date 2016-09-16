/*
 * Physics simulation of a moving object.
 */

program Physics {
    // object starts in (1;25) with an initial speed of (5;1)
    // simulation will run for 25 iterations and gravity is -1
    println(new Simulation().Start(1,25, 5,1, 25, 0-1));
}

class Simulation {
	// nb of iteration of the simulation 
	var nbTotalIter: Int;
	
	// simulation variables
	var positionX: Int;
	var positionY: Int;
	var speedX: Int;
	var speedY: Int;
	var gravityY: Int;

	// stored data to be displayed
	// this could be implemented more efficiently
	// with a hashmap but this is enough for this application
	var dataPointsX: Int[];
	var dataPointsY: Int[];
	var dataPointsD: Int[];

	// display size
	var resolutionX: Int;
	var resolutionY: Int;
	
	// Simulate then display object movement.
	def Start(pX: Int, pY: Int, sX: Int, sY: Int, nIter: Int, grav: Int) : String = {
		var nbIter: Int;	// iteration counter
		var d: Int;			// direction of the object

		// setup
		positionX = pX;
		positionY = pY;
		speedX = sX;
		speedY = sY;
		nbTotalIter = nIter;
		gravityY = grav;
		
		resolutionX = 65;
		resolutionY = 30;
		
		dataPointsX = new Int[nbTotalIter];
		dataPointsY = new Int[nbTotalIter];
		dataPointsD = new Int[nbTotalIter];
		
		// simulation
		nbIter = 0;
		while(nbIter < nbTotalIter) {
			positionX = positionX + speedX;
			positionY = positionY + speedY;
			
			// object bounces against the "walls"
			if(positionX < 0) {positionX = (0-1)*positionX; speedX = (0-1)*speedX;}
			if(positionY < 0) {positionY = (0-1)*positionY; speedY = (0-1)*speedY;}
			if(!(positionX < resolutionX)) {positionX = resolutionX -(resolutionX-positionX ); speedX = (0-1)*speedX;}
			if(!(positionY < resolutionY)) {positionY = resolutionY -(resolutionY-positionY ); speedY = (0-1)*speedY;}
			
			// gravity action
			speedY = speedY + gravityY;
			
			// current location is saved
			dataPointsX[nbIter] = positionX;
			dataPointsY[nbIter] = positionY;
			
			// compute the movement direction
			d = 0;
			if(speedX==0 && speedY==0) {d=5;}
			else if(speedX==0) {d=1;}
			else if(speedY==0) {d=2;}
			else if(!(speedX < 0) && !(speedY < 0)) {d=4;}
			else if(!(speedX < 0) && (speedY < 0)) {d=3;}
			else if((speedX < 0) && !(speedY < 0)) {d=3;}
			else if((speedX < 0) && (speedY < 0)) {d=4;}
			dataPointsD[nbIter] = d;

			nbIter = nbIter + 1;
		}
		
		return this.Display();
	}
	
	// Render the movement as ASCII characters.
	def Display() : String = {
		var screen: String;
		var posScreenX: Int;
		var posScreenY: Int;
		var touch: Int;
		var symb: String;
	
		posScreenY = resolutionY;

		while(!(posScreenY < 0)) {
			screen = "";
			posScreenX = 0;

			while(posScreenX < resolutionX) {
				// check if there has been an object at this position
				touch = this.Touch(posScreenX, posScreenY);

				if(!(touch == 0)) {
					// transform the direction to relevent symbol
					symb="?";
					if(touch == 1) {symb = "|";}
					if(touch == 2) {symb = "-";}
					if(touch == 3) {symb = "\";}
					if(touch == 4) {symb = "/";}
					if(touch == 5) {symb = ".";}

					screen = screen + symb;
				} else {
					screen = screen + " ";
				}

				posScreenX = posScreenX + 1;
			}
			
			println(screen);
			posScreenY = posScreenY - 1;
		}
		
		return "";
	}

	// Check if there has been a movement at a particular position.
	def Touch(x: Int, y: Int) : Int = {
		var iter: Int;
		var dir: Int;
		dir = 0;
		iter = 0;

		while(iter < nbTotalIter) {
			if(dataPointsX[iter] == x) {
				if(dataPointsY[iter] == y) {
					dir = dataPointsD[iter];
				}
			}
			
			iter = iter + 1;
		}

		return dir;
	}
}







