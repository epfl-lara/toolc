package toolc
package analyzer

import ast.Trees._

import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def tcClass(klass: ClassDecl): Unit = klass.methods.foreach(tcMethod)

    def tcMethod(meth: MethodDecl): Unit = {
      val methSym = meth.getSymbol
      val retType = methSym.getType
      meth.stats.foreach(tcStat)
      tcExpr(meth.retExpr, retType)
    }

    /** Checks that the expression is a subtype of the ones in expectedTps.
      * If it's not, prints an error message and returns the error type.
      * Also adds missing symbols to methods in MethodCalls and imposes extra restrictions as needed.
      */
    def tcExpr(expr: ExprTree, expectedTps: Type*): Unit = {
      expr match {
        case And(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
        case Or(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
        case Plus(lhs, rhs) =>
          tcExpr(lhs, TInt, TString)
          tcExpr(rhs, TInt, TString)
        case Minus(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        case Times(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        case Div(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        case LessThan(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        case Equals(lhs, rhs) =>
          tcExpr(lhs, TInt, TBoolean, TString, TIntArray, TObject)
          tcExpr(rhs, TInt, TBoolean, TString, TIntArray, TObject)
          (lhs.getType, rhs.getType) match {
            case (TClass(_), TClass(_)) =>
            case (t1, t2) if t1 == t2 =>
            case (t1, t2) =>
              error(s"Incompatible types in equality: $t1, $t2", expr)
          }
        case ArrayRead(arr, index) =>
          tcExpr(arr, TIntArray)
          tcExpr(index, TInt)
        case ArrayLength(arr) =>
          tcExpr(arr, TIntArray)
        case MethodCall(obj, meth, args) =>
          // Finally we check these method calls!
          tcExpr(obj, TObject)
          
          obj.getType match {
            case TClass(os) =>
              os.lookupMethod(meth.value) match {
                case Some(mSym) =>
                  // never too late :)
                  meth.setSymbol(mSym)

                  if(mSym.argList.length != args.length) {
                    error("Wrong number of arguments for method " + mSym.name + ".", meth)
                  } else {
                    args.zip(mSym.argList.map(_.getType)) foreach {
                      case (expr, tpe) => tcExpr(expr, tpe)
                    }
                  }
                case None =>
                  error("Method " + meth.value + " does not exist in class " + os.name, meth)
              }
            case other =>
              error(s"Method call should be applied to an object type, found: $other", expr)
          }

        case NewIntArray(size) =>
          tcExpr(size,TInt)

        case Not(expr) =>
          tcExpr(expr, TBoolean)

        case _ =>
          // do nothing
      }

      if (!expectedTps.toList.exists(expr.getType.isSubTypeOf)) {
        error("Type error: Expected: " + expectedTps.mkString(" or ") + s", found: ${expr.getType}", expr)
      }

    }
    
    def tcStat(stat: StatTree): Unit = stat match {
      case Block(stats) =>
        stats.foreach(tcStat)
      case If(expr, thn, elz) =>
        tcExpr(expr, TBoolean)
        tcStat(thn)
        elz.foreach(tcStat)
      case While(expr, stat) =>
        tcExpr(expr, TBoolean)
        tcStat(stat)
      case Println(expr) =>
        tcExpr(expr, TString, TInt, TBoolean)
      case Assign(id, expr) =>
        tcExpr(expr, id.getType)
      case ArrayAssign(id, index, expr) =>
        tcExpr(Variable(id), TIntArray)
        tcExpr(index, TInt)
        tcExpr(expr, TInt)
      case DoExpr(expr) =>
        tcExpr(expr, TInt, TBoolean, TString, TIntArray, TObject)
    }

    prog.main.stats.foreach(tcStat)
    prog.classes.foreach(tcClass)

    prog
  }
}
