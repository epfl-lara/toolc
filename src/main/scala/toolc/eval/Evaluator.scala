package toolc
package eval

import ast.Trees._
import utils._

class Evaluator(ctx: Context, prog: Program) {
  import ctx.reporter._

  def eval() {
    val ectx = new MainMethodContext
    prog.main.stats.foreach(evalStatement(_)(ectx))
  }

  def evalStatement(stmt: StatTree)(implicit ectx: EvaluationContext): Unit = stmt match {
    case Block(stats) => stats.foreach(evalStatement)
    case If(expr, thn, els) =>
      if (evalExpr(expr).asBool) {
        evalStatement(thn)
      } else {
        els foreach evalStatement
      }

    case While(expr, stat) =>
      while(evalExpr(expr).asBool) {
        evalStatement(stat)
      }

    case Println(expr) =>
      evalExpr(expr) match {
        case BoolValue(v)   => println(v)
        case IntValue(v)    => println(v)
        case StringValue(v) => println(v)
        case _ => fatal("Unexpected type", stmt)
      }

    case Assign(id, expr) =>
      ectx.setVariable(id.value, evalExpr(expr))

    case ArrayAssign(id, index, expr) =>
      val av = ectx.getVariable(id.value).asArray
      av.setIndex(evalExpr(index).asInt, evalExpr(expr).asInt)

    case DoExpr(expr) =>
      evalExpr(expr)
  }

  def evalExpr(e: ExprTree)(implicit ectx: EvaluationContext): Value = e match {
    case And(lhs, rhs) =>
      BoolValue(evalExpr(lhs).asBool && evalExpr(rhs).asBool)

    case Or(lhs, rhs)  =>
      BoolValue(evalExpr(lhs).asBool || evalExpr(rhs).asBool)

    case Plus(lhs, rhs) =>
      (evalExpr(lhs), evalExpr(rhs)) match {
        case (IntValue(lv), IntValue(rv))       => IntValue(lv + rv)
        case (StringValue(lv), IntValue(rv))    => StringValue(lv + rv)
        case (IntValue(lv), StringValue(rv))    => StringValue(lv + rv)
        case (StringValue(lv), StringValue(rv)) => StringValue(lv + rv)
        case (l,r) => fatal(s"Unexpected types: $l + $r", e)
      }

    case Minus(lhs, rhs) =>
      IntValue(evalExpr(lhs).asInt - evalExpr(rhs).asInt)

    case Times(lhs, rhs) =>
      IntValue(evalExpr(lhs).asInt * evalExpr(rhs).asInt)

    case Div(lhs, rhs) =>
      IntValue(evalExpr(lhs).asInt / evalExpr(rhs).asInt)

    case LessThan(lhs, rhs) =>
      BoolValue(evalExpr(lhs).asInt < evalExpr(rhs).asInt)

    case Not(expr) =>
      BoolValue(!evalExpr(expr).asBool)

    case IntLit(value) => IntValue(value)
    case StringLit(value) => StringValue(value)
    case True() => BoolValue(true)
    case False() => BoolValue(false)
    case Equals(lhs, rhs) =>
      val lv = evalExpr(lhs)
      val rv = evalExpr(rhs)
      val res = (lv, rv) match {
        case (IntValue(l), IntValue(r)) => l == r
        case (BoolValue(l), BoolValue(r)) => l == r
        case (lr, rr) => lr eq rr
      }
      BoolValue(res)

    case ArrayRead(arr, index) =>
      val av = evalExpr(arr).asArray
      val i  = evalExpr(index).asInt
      IntValue(av.getIndex(i))

    case ArrayLength(arr) =>
      val av = evalExpr(arr).asArray
      IntValue(av.length)

    case MethodCall(obj, meth, args) =>
      val o  = evalExpr(obj).asObject
      val as = args.map(evalExpr)

      val nmctx = new MethodContext(o)

      val mdecl = findMethod(o.cd, meth.value)

      for ((f, v) <- mdecl.args zip as) {
        nmctx.declareVariable(f.id.value)
        nmctx.setVariable(f.id.value, v)
      }

      for (v <- mdecl.vars) {
        nmctx.declareVariable(v.id.value)
      }

      mdecl.stats.foreach(evalStatement(_)(nmctx))

      evalExpr(mdecl.retExpr)(nmctx)

    case Variable(Identifier(name)) =>
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
      val s = evalExpr(size).asInt
      new ArrayValue(new Array(s))
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
    def asInt: Int            = fatal(s"Unexpected value: found $this, expected Int")
    def asString: String      = fatal(s"Unexpected value: found $this, expected String")
    def asBool: Boolean       = fatal(s"Unexpected value: found $this, expected Boolean")
    def asObject: ObjectValue = fatal(s"Unexpected value: found $this, expected Object")
    def asArray: ArrayValue   = fatal(s"Unexpected value: found $this, expected Array")
  }

  case class ObjectValue(cd: ClassDecl) extends Value {
    var fields = Map[String, Option[Value]]()

    def setField(name: String, v: Value) {
      if (fields contains name) {
        fields += name -> Some(v)
      } else {
        fatal(s"Unknown field '$name'")
      }
    }

    def getField(name: String) = {
      fields.get(name) match {
        case Some(Some(v)) => v
        case Some(None) => fatal(s"Field '$name' has not been initialized")
        case None => fatal(s"Unknown field '$name'")
      }
    }

    def declareField(name: String) {
      fields += name -> None
    }

    override def asObject = this
  }

  case class ArrayValue(entries: Array[Int]) extends Value {
    val length = entries.length
    def setIndex(i: Int, v: Int) {
      if (i >= length || i < 0) {
        fatal(s"Index '$i' out of bounds (0 .. ${length-1})")
      }
      entries(i) = v
    }

    def getIndex(i: Int) = {
      if (i >= length || i < 0) {
        fatal(s"Index '$i' out of bounds (0 .. ${length-1})")
      }
      entries(i)
    }

    override def asArray = this
  }

  case class StringValue(v: String) extends Value {
    override def asString = v
  }

  case class IntValue(v: Int) extends Value {
    override def asInt = v
  }

  case class BoolValue(v: Boolean) extends Value {
    override def asBool = v
  }
}

