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
        case _ => fatal("Unnexpected type")
      }

    case Assign(id: Identifier, expr: ExprTree) =>
      ectx.setValue(id.value, evalExpr(ectx, expr))

    case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
      ectx.getValue(id.value) match {
        case av: ArrayValue =>
          av.setIndex(evalExpr(ectx, index).asInt, evalExpr(ectx, expr).asInt)
        case _ =>
          fatal("unnexpected type")
      }

    case _ => fatal("unnexpected statement")
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
        case (l,r) => fatal("Unnexpected types: "+l+"+"+r)
      }

    case Minus(lhs, rhs) =>
      IntValue(evalExpr(ectx, lhs).asInt - evalExpr(ectx, rhs).asInt)

    case Times(lhs, rhs) =>
      IntValue(evalExpr(ectx, lhs).asInt * evalExpr(ectx, rhs).asInt)

    case Div(lhs, rhs) =>
      IntValue(evalExpr(ectx, lhs).asInt / evalExpr(ectx, rhs).asInt)

    case LessThan(lhs, rhs) =>
      BoolValue(evalExpr(ectx, lhs).asInt < evalExpr(ectx, rhs).asInt)

    case NumLit(value) => IntValue(value)
    case StringLit(value) => StringValue(value)
    case True() => BoolValue(true)
    case False() => BoolValue(false)
    case Equals(lhs, rhs) =>
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)

      BoolValue(lv == rv)

    case ArrayRead(arr, index) =>
      val a = evalExpr(ectx, arr)
      val i = evalExpr(ectx, index).asInt
      a match {
        case av: ArrayValue =>
          IntValue(av.getIndex(i))

        case _ =>
          fatal("Unnexpected value: "+a+" expected array")
      }

    case ArrayLength(arr) =>
      val a = evalExpr(ectx, arr)
      a match {
        case av: ArrayValue =>
          IntValue(av.size)

        case _ =>
          fatal("Unnexpected value: "+a+" expected array")
      }

    case MethodCall(obj, meth, args) =>
      val o = evalExpr(ectx, obj).asObject
      val as = args.map(evalExpr(ectx, _))

      val nmctx = new MethodContext(o)

      val klass = findClass(o.className)
      val mdecl = findMethod(klass, meth.value)

      for ((f, v) <- mdecl.args zip as) {
        nmctx.declareValue(f.id.value)
        nmctx.setValue(f.id.value, v)
      }

      for (v <- mdecl.vars) {
        nmctx.declareValue(v.id.value)
      }

      mdecl.stats.foreach(evalStatement(nmctx, _))

      evalExpr(nmctx, mdecl.retExpr)

    case Identifier(name) =>
      ectx.getValue(name)

    case New(tpe) =>
      ObjectValue.newObject(findClass(tpe.value))

    case This() =>
      ectx match {
        case mctx: MethodContext =>
          mctx.obj
        case _ =>
          fatal("Accessing 'this' when no object context is available")
      }

    case NewIntArray(size) =>
      val s = evalExpr(ectx, size).asInt

      new ArrayValue(Array(s), s)

    case Not(expr) =>
      BoolValue(!evalExpr(ectx, expr).asBool)
  }

  abstract class EvaluationContext {
    def getValue(name: String): Value
    def setValue(name: String, v: Value): Unit
    def declareValue(name: String): Unit
  }

  class MethodContext(val obj: ObjectValue) extends EvaluationContext {
    var vars = Map[String, Option[Value]]()

    def getValue(name: String): Value = {
      vars.getOrElse(name, fatal("Unknown variable '"+name+"'"))
        .getOrElse(fatal("Uninitialized variable '"+name+"'"))
    }

    def setValue(name: String, v: Value) {
      if (vars.contains(name)) {
        vars += name -> Some(v)
      } else {
        fatal("Unknown variable '"+name+"'")
      }
    }

    def declareValue(name: String) {
      vars += name -> None
    }
  }

  class MainMethodContext extends EvaluationContext {
    def getValue(name: String): Value = ???
    def setValue(name: String, v: Value): Unit = ???
    def declareValue(name: String): Unit = ???
  }

  def findMethod(klass: ClassDecl, name: String): MethodDecl = {
    klass.methods.find(_.id.value == name).getOrElse(fatal("Unknown method "+klass.id+"."+name))
  }

  def findClass(name: String): ClassDecl = {
    prog.classes.find(_.id.value == name).getOrElse(fatal("Unknown class '"+name+"'"))
  }

  def fieldsOfClass(cl: ClassDecl): Set[String] = {
    cl.vars.map(_.id.value).toSet ++
      cl.parent.map(p => fieldsOfClass(findClass(p.value))).getOrElse(Set())
  }

  sealed abstract class Value {
    def asInt: Int            = fatal("Unnexpected value, found "+this+" expected Int")
    def asBool: Boolean       = fatal("Unnexpected value, found "+this+" expected Boolean")
    def asObject: ObjectValue = fatal("Unnexpected value, found "+this+" expected Object")
  }

  case class ObjectValue(id: Int, className: String, var fields: Map[String, Option[Value]]) extends Value {

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

  object ObjectValue {
    private var _cnt = 0;

    def newObject(klass: ClassDecl): ObjectValue = {
      _cnt += 1

      val ov = ObjectValue(_cnt, klass.id.value, Map())

      for (f <- fieldsOfClass(klass)) {
        ov.declareField(f)
      }

      ov
    }
  }

  case class StringValue(var v: String) extends Value

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
  }

  case class IntValue(var v: Int) extends Value {
    override def asInt = v
  }

  case class BoolValue(var v: Boolean) extends Value {
    override def asBool = v
  }
}

