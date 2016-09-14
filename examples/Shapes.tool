object Shapes {
	def main() : Unit = {
		if (new GenerateShapes().start()) {println("Ok");} else {println("Error");}		        
	}
}

class GenerateShapes {
	var sq1: Square;
	var sq2: Square;
	var ci1: Circle;
	
	def start(): Bool = {
		sq1 = new Square();
		sq2 = new Square();
		ci1 = new Circle();

		//Initializes the shapes
		println(sq1.initialize("square1", "red", 4)); 
		println(sq2.initialize("square2", "green", 7)); 
		println(ci1.initialize("circle1", "blue", 3)); 
		
		println(sq1.attributes());
		println(sq2.attributes());
		println(ci1.attributes());

		return true;
	}
}

class Shape {
	var varName: String;
	var color: String;

	def initialize(name: String, clr: String, n: Int): String = {
		varName = name;
		color = clr;
		return "A new " + color + " shape has been created: " + varName + ".";
	}

	def attributes(): String = {
		return varName + ": " + color + " shape.";
	}
}

class Square extends Shape {
	var side: Int;

	def initialize(name: String, clr: String, sd: Int): String = {
		varName = name;
		color = clr;
		side = sd;
		return "A new " + color + " square of side " + side + " has been created: " + varName + ".";
	}

	def attributes(): String = {
		return varName + ": " + color + " square of side " + side + ".";
	}
}

class Circle extends Shape {
	var radius: Int;

	def initialize(name: String, clr: String, rd: Int): String = {
		varName = name;
		color = clr;
		radius = rd;
		return "A new " + radius + "-radius " + color + " circle has been created: " + varName + ".";
	}

	def attributes(): String = {
		return varName + ": " + radius + "-radius " + color + " circle.";
	}
}