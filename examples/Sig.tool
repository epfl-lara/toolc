/*
-------------------- Source -------------------- 
class sig {public static void main(String[] g) {
        String c,s=         c="";char b
        =32,a=8226;         for(int r,i
        ,j,k=0;++k<         6;s+=(char)
        10)for(i=r=         18;i-->0;r=
        0){for(j=-1         ;j<4;r+=((k
        %2.<1)?32-8         *k:((4-k)*k
        *57+1845)/8         )<<++j*3&(3
        <<j*5));s+=         ((r+j*33825
        >>i&1)>0?a+         c+a:b+c+b);
        }System.out         .print(s);}
                       }

------------------- Indented ------------------- 
public class M {
    public static void main(String[] g) {
        String c, s = c = "";
        char b = 32, a = 8226;
        for (
                int r, i, j, k = 0;
                ++k < 6;
                s += (char) 10
            )
            for (
                    i = r = 18;
                    i-- > 0;
                    r = 0
                )
                {
                for (
                        j = -1;
                        j < 4;
                        r +=    ((k % 2. < 1) ?
                                    32 - 8 * k :
                                    ((4 - k) * k * 57 + 1845) / 8
                                ) << ++j * 3 & (3 << j * 5)
                    );
                s += ((r + j * 33825 >> i & 1) > 0 ?
                        a + c + a :
                        b + c + b
                     );
                }
        System.out.print(s);
    }
}

----------------- Implementation ------------------
*/
program Sig {
    println(new App().run());
}

class App {
    def run(): String = {
        var s: String;
        var b: Int;
        var r: Int;
        var i: Int;
        var j: Int;
        var k: Int;
        println("");
        s = "";
        b = 32;
        r = 0;
        i = 0;
        j = 0;
        k = 1;
        while(k < 6) {
            i = 18;
            r = 18;
            while(0 < i) {
                i = i - 1;
                j = 0 - 1;
                while(j < 4) {
                    j = j + 1;
                    r = r + this.and(
						this.shiftLeft(
							this.ternary(
								this.mod(k, 2) < 1,
								(32 - 8 * k),
								((4 - k) * k * 57 + 1845) / 8 // 8 tabs ftw, ho yeah!
							),
							j * 3
						), 
						this.shiftLeft(3, j * 5)
					);
                }
                s = s + this.intToChar(this.ternary(
					0 < (this.and(this.shiftRight((r + j * 33825), i), 1)),
					0,
					b
				));
                r = 0;
            }
            k = k + 1;
            println(s);
            s = "";
        }
        return "";
    }

    def mod(i : Int, j : Int) : Int = { return i - (i / j * j); }
    
    def shiftLeft(a: Int, b: Int): Int = {
        var i: Int;
        var j: Int;
        if (31 < b)
             a = 0;
        else {
            i = 0;
            j = 1;
            while(i < b) {
                j = 2 * j;
                i = i + 1;
            }
            a = a * j;
        }
        return a;
    }
    
    def shiftRight(a: Int, b: Int): Int = {
        var i: Int;
        var j: Int;
        if (30 < b) {
            if (a < 0) {
                a = 0 - 1;
            } else {
                a = 0;
            }
        } else if (0 < b) {
            i = 0;
            j = 1;
            while(i < b) {
                j = 2 * j;
                i = i + 1;
            }
            a = a / j;
        }
        return a;
    }
    
    def and(a: Int, b: Int): Int = {
        var c: Int;
        var x: Int;
        c = 0;        
        x = 0;
        while(x < 32) {
            x = x + 1;
            c = 2 * c;
            if (a < 0) {
                if (b < 0) {
                    c = c + 1;
                }
            }
            a = 2 * a;
            b = 2 * b;
        }
        return c;
    }
    
    def intToChar(i: Int): String = {
        var s: String;
        if(i == 32)
            s = "  ";
        else
            s = "##";
        return s;        
    }

	def ternary(b: Bool, x: Int, y: Int): Int = {
		var res: Int;
		if(b)
			res = x;
		else
			res = y;
		return res;
	}
}
