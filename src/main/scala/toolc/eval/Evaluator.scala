package toolc
package eval

import ast.Trees._
import utils._

class Evaluator(ctx: Context, prog: Program) {
  import ctx.reporter._

  def eval() {
    val ectx = new MainMethodContext
    prog.main.stats.foreach(evalStatement(ectx, _))
  }

  def evalStatement(ectx: EvaluationContext, stmt: StatTree): Unit = stmt match {
    case Block(stats) => stats.foreach(evalStatement(ectx, _))
    case If(expr, thn, els) =>
      if (evalExpr(ectx, expr).asBool) {
        evalStatement(ectx, thn)
      } else if (els.isDefined) {
        evalStatement(ectx, els.get)
      }

    case While(expr, stat) =>
      while(evalExpr(ectx, expr).asBool) {
        evalStatement(ectx, stat)
      }

    case Println(expr: ExprTree) =>
      evalExpr(ectx, expr) match {
        case BoolValue(v)   => println(v)
        case IntValue(v)    => println(v)
        case StringValue(v) => println(v)
        case _ => fatal("Unnexpected type", stmt)
      }

    case Assign(id: Identifier, expr: ExprTree) =>
      ectx.setVariable(id.value, evalExpr(ectx, expr))

    case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
      val av = ectx.getVariable(id.value).asArray
      av.setIndex(evalExpr(ectx, index).asInt, evalExpr(ectx, expr).asInt)

    case _ =>
      fatal("unnexpected statement", stmt)
  }

  def evalExpr(ectx: EvaluationContext, e: ExprTree): Value = e match {
    case And(lhs, rhs) =>
      BoolValue(evalExpr(ectx, lhs).asBool && evalExpr(ectx, rhs).asBool)

    case Or(lhs, rhs)  =>
      BoolValue(evalExpr(ectx, lhs).asBool || evalExpr(ectx, rhs).asBool)

    case Plus(lhs, rhs) =>
      (evalExpr(ectx, lhs), evalExpr(ectx, rhs)) match {
        case (IntValue(lv), IntValue(rv)) =>       IntValue(lv+rv)
        case (StringValue(lv), IntValue(rv)) =>    StringValue(lv+rv)
        case (IntValue(lv), StringValue(rv)) =>    StringValue(lv+rv)
        case (StringValue(lv), StringValue(rv)) => StringValue(lv+rv)
        case (l,r) => fatal("Unnexpected types: "+l+"+"+r, e)
      }

    case Minus(lhs, rhs) =>
      IntValue(evalExpr(ectx, lhs).asInt - evalExpr(ectx, rhs).asInt)

    case Times(lhs, rhs) =>
      IntValue(evalExpr(ectx, lhs).asInt * evalExpr(ectx, rhs).asInt)

    case Div(lhs, rhs) =>
      IntValue(evalExpr(ectx, lhs).asInt / evalExpr(ectx, rhs).asInt)

    case LessThan(lhs, rhs) =>
      BoolValue(evalExpr(ectx, lhs).asInt < evalExpr(ectx, rhs).asInt)

    case Not(expr) =>
      BoolValue(!evalExpr(ectx, expr).asBool)

    case IntLit(value) => IntValue(value)
    case StringLit(value) => StringValue(value)
    case True() => BoolValue(true)
    case False() => BoolValue(false)
    case Equals(lhs, rhs) =>
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      val res = (lv, rv) match {
        case (IntValue(l), IntValue(r)) => l == r
        case (BoolValue(l), BoolValue(r)) => l == r
        case (lr, rr) => lr eq rr
      }
      BoolValue(res)

    case ArrayRead(arr, index) =>
      val av = evalExpr(ectx, arr).asArray
      val i  = evalExpr(ectx, index).asInt

      IntValue(av.getIndex(i))

    case ArrayLength(arr) =>
      val av = evalExpr(ectx, arr).asArray
      IntValue(av.size)

    case MethodCall(obj, meth, args) =>
      val o  = evalExpr(ectx, obj).asObject
      val as = args.map(evalExpr(ectx, _))

      val nmctx = new MethodContext(o)

      val mdecl = findMethod(o.cd, meth.value)

      for ((f, v) <- mdecl.args zip as) {
        nmctx.declareVariable(f.id.value)
        nmctx.setVariable(f.id.value, v)
      }

      for (v <- mdecl.vars) {
        nmctx.declareVariable(v.id.value)
      }

      mdecl.stats.foreach(evalStatement(nmctx, _))

      evalExpr(nmctx, mdecl.retExpr)

    case Identifier(name) =>
      ectx.getVariable(name)

    case New(tpe) =>
      val cd = findClass(tpe.value)
      val ov = ObjectValue(cd)

      for (f <- fieldsOfClass(cd)) {
        ov.declareField(f)
      }

      ov

    case This() =>
      ectx match {
        case mctx: MethodContext =>
          mctx.obj
        case _ =>
          fatal("Accessing 'this' when no object context is available", e)
      }

    case NewIntArray(size) =>
      val s = evalExpr(ectx, size).asInt

      new ArrayValue(new Array(s), s)
  }

  abstract class EvaluationContext {
    def getVariable(name: String): Value
    def setVariable(name: String, v: Value): Unit
    def declareVariable(name: String): Unit
  }

  class MethodContext(val obj: ObjectValue) extends EvaluationContext {
    var vars = Map[String, Option[Value]]()

    def getVariable(name: String): Value = {
      vars.get(name) match {
        case Some(ov) =>
          ov.getOrElse(fatal("Uninitialized variable '"+name+"'"))
        case _ =>
          obj.getField(name)
      }
    }

    def setVariable(name: String, v: Value) {
      if (vars contains name) {
        vars += name -> Some(v)
      } else {
        obj.setField(name, v)
      }
    }

    def declareVariable(name: String) {
      vars += name -> None
    }
  }

  class MainMethodContext extends EvaluationContext {
    def getVariable(name: String): Value          = fatal("The main method contains no variable and/or field")
    def setVariable(name: String, v: Value): Unit = fatal("The main method contains no variable and/or field")
    def declareVariable(name: String): Unit       = fatal("The main method contains no variable and/or field")
  }

  def findMethod(cd: ClassDecl, name: String): MethodDecl = {
    cd.methods.find(_.id.value == name).orElse(
      cd.parent.map(p => findMethod(findClass(p.value), name))
    ).getOrElse(fatal("Unknown method "+cd.id+"."+name))
  }

  def findClass(name: String): ClassDecl = {
    prog.classes.find(_.id.value == name).getOrElse(fatal("Unknown class '"+name+"'"))
  }

  def fieldsOfClass(cl: ClassDecl): Set[String] = {
    cl.vars.map(_.id.value).toSet ++
      cl.parent.map(p => fieldsOfClass(findClass(p.value))).getOrElse(Set())
  }

  sealed abstract class Value {
    def asInt: Int            = fatal("Unexpected value, found "+this+" expected Int")
    def asString: String      = fatal("Unexpected value, found "+this+" expected String")
    def asBool: Boolean       = fatal("Unexpected value, found "+this+" expected Boolean")
    def asObject: ObjectValue = fatal("Unexpected value, found "+this+" expected Object")
    def asArray: ArrayValue   = fatal("Unexpected value, found "+this+" expected Array")
  }

  case class ObjectValue(cd: ClassDecl) extends Value {
    var fields = Map[String, Option[Value]]()

    def setField(name: String, v: Value) {
      if (fields contains name) {
        fields += name -> Some(v)
      } else {
        fatal("Unknown field '"+name+"'")
      }
    }

    def getField(name: String) = {
      fields.get(name).flatten.getOrElse(fatal("Unknown field '"+name+"'"))
    }

    def declareField(name: String) {
      fields += name -> None
    }

    override def asObject = this
  }

  case class ArrayValue(var entries: Array[Int], val size: Int) extends Value {
    def setIndex(i: Int, v: Int) {
      if (i >= size || i < 0) {
        fatal("Index '"+i+"' out of bounds (0 .. "+size+")")
      }
      entries(i) = v
    }

    def getIndex(i: Int) = {
      if (i >= size || i < 0) {
        fatal("Index '"+i+"' out of bounds (0 .. "+size+")")
      }
      entries(i)
    }

    override def asArray = this
  }

  case class StringValue(var v: String) extends Value {
    override def asString = v
  }

  case class IntValue(var v: Int) extends Value {
    override def asInt = v
  }

  case class BoolValue(var v: Boolean) extends Value {
    override def asBool = v
  }
}

