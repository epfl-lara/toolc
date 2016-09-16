program Vector {
    println("--- Vector : addition test ---");
    println(new Vect3().test1());
    println("--- Vector : dot product test ---");
    println(new Vect3().test2());
    println("--- Vector : cross product test ---");
    println(new Vect3().test3());
    println("--- Vector : negative / multiplication test ---");
    println(new Vect3().test4());
    println("--- Vector : substraction test ---");
    println(new Vect3().test5());
}

class Vect3 {

    var x_ : Int;
    var y_ : Int;
    var z_ : Int;

    def test1() : String = {
        var vect1 : Vect3;
        var vect2 : Vect3;
        var result : Vect3;

        vect1 = new Vect3().init(2, 3, 4);
        vect2 = new Vect3().init(5, 6, 7);
        result = vect1.add(vect2);

        return vect1.toString() + " + " + vect2.toString() + " = " + result.toString();
    }

    def test2() : String = {
        var vect1 : Vect3;
        var vect2 : Vect3;
        var result : Int;

        vect1 = new Vect3().init(2, 3, 4);
        vect2 = new Vect3().init(5, 6, 7);
        result = vect1.dotp(vect2);

        return vect1.toString() + " * " + vect2.toString() + " = " + result;
    }

    def test3() : String = {
        var vect1 : Vect3;
        var vect2 : Vect3;
        var result : Vect3;

        vect1 = new Vect3().init(2, 3, 4);
        vect2 = new Vect3().init(5, 6, 7);
        result = vect1.crossp(vect2);

        return vect1.toString() + " ^ " + vect2.toString() + " = " + result.toString();
    }

    def test4() : String = {
        var vect1 : Vect3;
        var result : Vect3;

        vect1 = new Vect3().init(2, 3, 4);
        result = vect1.neg().mult(3);

        return "-" + vect1.toString() + " * 3 = " + result.toString();
    }

    def test5() : String = {
        var vect1 : Vect3;
        var vect2 : Vect3;
        var result : Vect3;

        vect1 = new Vect3().init(2, 3, 4);
        vect2 = new Vect3().init(5, 2, 8);
        result = vect1.sub(vect2);

        return vect1.toString() + " - " + vect2.toString() + " = " + result.toString();
    }

    def init(x : Int, y : Int, z : Int) : Vect3 = {
        x_ = x;
        y_ = y;
        z_ = z;
        return this;
    }

    def getCoord() : Int[] = {
        var coord : Int[];

        coord = new Int[3];
        coord[0] = x_;
        coord[1] = y_;
        coord[2] = z_;

        return coord;
    }

    def add(that : Vect3) : Vect3 = {
        var coord : Int[];

        coord = that.getCoord();
        return new Vect3().init(x_ + coord[0], y_ + coord[1], z_ + coord[2]);
    }

    def dotp(that : Vect3) : Int = {
        var coord : Int[];

        coord = that.getCoord();
        return x_ * coord[0] + y_ * coord[1] + z_ * coord[2];
    }

    def crossp(that : Vect3) : Vect3 = {
        var coord : Int[];

        coord = that.getCoord();
        return new Vect3().init(y_ * coord[2] - z_ * coord[1], 
                                z_ * coord[0] - x_ * coord[2], 
                                x_ * coord[1] - y_ * coord[0]);
    }

    def neg() : Vect3 = {
        return new Vect3().init(0 - x_, 0 - y_, 0 - z_);
    }

    def mult(num : Int) : Vect3 = {
        return new Vect3().init(num * x_, num * y_, num * z_);
    }
    
    def sub(that : Vect3) : Vect3 = {
        return this.add(that.neg());
    }

    def toString() : String = {
        return "(" + x_ + "," + y_ + "," + z_ + ")";
    }
}

