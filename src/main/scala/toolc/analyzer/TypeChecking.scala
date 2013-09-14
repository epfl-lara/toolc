package toolc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def tcClass(klass: ClassDecl): Unit = klass.methods.foreach(tcMethod(_))

    def tcMethod(meth: MethodDecl): Unit = {
      val methSym = meth.getSymbol
      val retType = methSym.getType
      meth.stats.foreach(tcStat(_))
      tcExpr(meth.retExpr, retType)
    }

    /** Computes the type of an expression. If exp is not empty, checks that
     * the expression is a subtype of the ones in exp. If it's not, prints an
     * error message and returns the error type. */
    def tcExpr(expr: ExprTree, exp: Type*): Type = {
      val actualType: Type = expr match {
        case And(lhs, rhs) => tcExpr(lhs, TBoolean); tcExpr(rhs, TBoolean); TBoolean
        case Or(lhs, rhs) => tcExpr(lhs, TBoolean); tcExpr(rhs, TBoolean); TBoolean
        case Plus(lhs, rhs) =>
          val tLeft = tcExpr(lhs, TInt, TString)
          val tRight = tcExpr(rhs, TInt, TString)
          
          (tLeft,tRight) match {
            case (TInt,TInt) => TInt
            case (TString,TInt) => TString
            case (TInt,TString) => TString
            case (TString,TString) => TString
            case _ => TError
          }
       
        case Minus(lhs, rhs) => tcExpr(lhs, TInt); tcExpr(rhs, TInt); TInt
        case Times(lhs, rhs) => tcExpr(lhs, TInt); tcExpr(rhs, TInt); TInt
        case Div(lhs, rhs) => tcExpr(lhs, TInt); tcExpr(rhs, TInt); TInt
        case LessThan(lhs, rhs) => tcExpr(lhs, TInt); tcExpr(rhs, TInt); TBoolean
        case Equals(lhs, rhs) =>
          val tLeft = tcExpr(lhs, TInt, TBoolean, TString, TIntArray, anyObject)
          val tRight = tcExpr(rhs, TInt, TBoolean, TString, TIntArray, anyObject)
          
          (tLeft,tRight) match {
            case (TInt,TInt) => TBoolean
            case (TBoolean,TBoolean) => TBoolean
            case (TString,TString) => TBoolean
            case (TIntArray,TIntArray) => TBoolean
            case (t1 @ TObject(_), t2 @ TObject(_)) /* if(t1.isSubTypeOf(t2) || t2.isSubTypeOf(t1)) */ => TBoolean
            case (t1 @ _, t2 @ _) => error("Incompatible types in equality: " + t1 + ", " + t2, expr); TError
          }
        case ArrayRead(arr, index) => tcExpr(arr, TIntArray); tcExpr(index, TInt); TInt
        case ArrayLength(arr) => tcExpr(arr, TIntArray); TInt
        case MethodCall(obj, meth, args) =>
          // Finally we check these method calls!
          val objType = tcExpr(obj, anyObject)
          
          objType match {
            case TObject(os) => {
              os.lookupMethod(meth.value) match {
                case Some(mSym) => {
                  // never too late :)
                  meth.setSymbol(mSym)

                  if(mSym.argList.length != args.length) {
                    error("Wrong number of arguments for method " + mSym.name + ".", meth)
                  } else {
                    args.zip(mSym.argList.map(_.getType)).foreach((exTpePair) => tcExpr(exTpePair._1,exTpePair._2))
                  }
                  mSym.getType // better than error type.
                }
                case None => error("Method " + meth.value + " does not exist in class " + os.name, meth); TError
              }
            }
            case TError => TError // so as to not multiply useless method calls
            case other @ _ => error("Method call should be applied to an object type, found: " + other, expr); TError
          }
          
        case NumLit(value) => TInt
        case StringLit(value) => TString
        case True() => TBoolean
        case False() => TBoolean
        case id @ Identifier(_) => id.getType
        case t @ This() => TObject(t.getSymbol)
        case NewIntArray(size) =>
          tcExpr(size,TInt)
          TIntArray

        case New(tpe) =>
          tpe.getSymbol match {
            case cs: ClassSymbol => TObject(cs)
            case _ => error("Expected: class name, found: " + tpe, tpe); TUntyped
          }
        
        case Not(expr) =>
          tcExpr(expr, TBoolean);
          TBoolean
      }

      val tpe = if(exp.isEmpty) {
          actualType
        } else {
          if(!exp.exists(e => actualType.isSubTypeOf(e))) {
            if(exp.length == 1)
              error("Type error: Expected: " + exp.toList.head + ", found: " + actualType, expr)
            else          
              error("Type error: Expected one of the following types: " + exp.toList.mkString(", ") + ", found: " + actualType, expr)
            exp.head
          } else {
            actualType
          }
        }

      expr.setType(tpe)
      tpe
    }
    
    def tcStat(stat: StatTree): Unit = stat match {
      case Block(stats) => stats.foreach(tcStat(_))
      case If(expr, thn, elz) => tcExpr(expr, TBoolean); tcStat(thn); elz.foreach(tcStat(_))
      case While(expr, stat) => tcExpr(expr, TBoolean); tcStat(stat)
      case Println(expr) => tcExpr(expr, TString, TInt, TBoolean)
      case Assign(id, expr) => tcExpr(expr, tcExpr(id))
      case ArrayAssign(id, index, expr) => tcExpr(id, TIntArray); tcExpr(index, TInt); tcExpr(expr, TInt)
      case Assert(expr) => tcExpr(expr, TBoolean)
    }

    prog.main.stats.foreach(tcStat(_))
    prog.classes.foreach(tcClass(_))

    prog
  }
}