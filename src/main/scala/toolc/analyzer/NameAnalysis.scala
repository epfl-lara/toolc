package toolc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    var varUsage = Map[VariableSymbol,Boolean]()

    def collectSymbols(prog: Program): GlobalScope = {

      val global = new GlobalScope

      if(prog.main.id.value.equals("Object")) {
        error("The main object cannot be named Object.", prog.main.id)
      }

      val mcSym = new MainSymbol(prog.main.id.value)
      global.mainClass = mcSym
      prog.main.setSymbol(mcSym)
      prog.main.id.setSymbol(mcSym)

      // We first create empty symbols for all classes, checking also that they
      // are defined only once.
      for (c <- prog.classes) {
        val name = c.id.value

        if(name.equals(mcSym.name)) {
          error(s"Class $name has the same name as the main class.", c.id)
        }

        if(name.equals("Object")) {
          error("No class can be named Object", c.id)
        }

        global.lookupClass(name) foreach { s =>
          error(s"Class $name is defined more than once. First definition here: ${s.position}", c.id)
        }

        val classSym = new ClassSymbol(name).setPos(c)
        c.setSymbol(classSym)
        c.id.setSymbol(classSym)
        global.classes += (name -> classSym)
      }

      // Set parent Symbols
      for {
        cls <- prog.classes
        clSym = global.classes(cls.id.value)
        par <- cls.parent
      } yield {
        global.lookupClass(par.value) match {
          case None =>
            error(s"Class ${clSym.name} extends class ${par.value} which is not defined.", par)
          case Some(parSym) =>
            clSym.parent = Some(parSym)
            par.setSymbol(parSym)
        }
      }

      // Check there are no cycles in the inheritance graph
      prog.classes foreach { cls =>
        val clsSym = cls.getSymbol

        def mkChain(curr: ClassSymbol): List[ClassSymbol] = {
          curr.parent match {
            case None => List(curr)
            case Some(`clsSym`) => List(curr, clsSym)
            case Some(p) => curr :: mkChain(p)
          }
        }

        val chain = mkChain(clsSym)

        if (chain.size > 1 && chain.head == chain.last) {
          fatal("Cyclic inheritance: " + chain.map(_.name).mkString(" -> "))
        }

      }

      // We now know that every class is unique and the inheritance graph is
      // correct. We proceed to check the contents of these classes.
      var done = Set[String]()
      prog.classes.foreach(collectInClass)

      def collectInClass(c: ClassDecl): Unit = {
        val classSym = c.getSymbol

        if(done(classSym.name)) return

        // it is important that we analyze parent classes first
        classSym.parent match {
          case Some(parSym) if !done(parSym.name) =>
            collectInClass(prog.classes.find(_.id.value == parSym.name).get)

          case _ =>
        }

        // class members
        c.vars.foreach { varDecl =>
          val varName = varDecl.id.value

          var foundInParent = false

          // are we overriding?
          classSym.parent.flatMap(_.lookupVar(varName)) match {
            case Some(s) =>
              error(varName + " member declaration overrides previous declaration at " + s.position + ".", varDecl)
              foundInParent = true

            case None =>
          }

          // have we defined twice?
          if(!foundInParent) {
            classSym.lookupVar(varName) match {
              case Some(prev) =>
                error(varName + " is declared more than once. First definition here: " + prev.position + ".", varDecl)

              case None =>
                val varSym = new VariableSymbol(varName).setPos(varDecl)
                varUsage += (varSym -> false)
                varDecl.setSymbol(varSym)
                varDecl.id.setSymbol(varSym)
                classSym.members += (varName -> varSym)
            }
          }
        }

        c.methods.foreach(collectInMethod)

        def collectInMethod(m: MethodDecl): Unit = {
          val methName = m.id.value
          val methSym = new MethodSymbol(methName, classSym).setPos(m)
          m.setSymbol(methSym)
          m.id.setSymbol(methSym)

          classSym.methods.get(methName) match {
            case Some(prev) =>
              error(methName + " is defined more than once. First definition here: " + prev.position + ".", m)

            case None =>
              classSym.lookupMethod(methName) match {
                case Some(overridden) =>
                  if(overridden.params.size != m.args.length) {
                    error(methName + " overrides previous definition from " + overridden.position + " with a different number of parameters.", m)
                  }
                  methSym.overridden = Some(overridden)

                case None =>
              }
              classSym.methods += (methName -> methSym)
          }

          for (formal <- m.args) {
            val parName = formal.id.value
            methSym.params.get(parName) match {
              case Some(varS) =>
                error("Parameter name " + parName + " is used twice in " + methName + ".", formal.id)

              case None =>
                val parSym = new VariableSymbol(parName).setPos(formal.id)
                formal.setSymbol(parSym)
                formal.id.setSymbol(parSym)
                methSym.params += (parName -> parSym)
                methSym.argList = methSym.argList ::: (parSym :: Nil)
            }
          }

          for (varDecl <- m.vars) {
            val varName = varDecl.id.value
            methSym.params.get(varName) match {
              case Some(parS) =>
                error("Declaration of " + varName + " as local shadows method parameter of the same name.", varDecl)

              case None =>
                methSym.members.get(varName) match {
                  case Some(first) =>
                    error(varName + " is declared more than once. First declaration here: " + first.position + ".", varDecl)

                  case None =>
                    val varSym = new VariableSymbol(varName).setPos(varDecl.id)
                    varUsage += (varSym -> false)
                    varDecl.id.setSymbol(varSym)
                    varDecl.setSymbol(varSym)
                    methSym.members += (varName -> varSym)
                }
            }
          }
        }

        done += c.id.value
      }

      global
    }


    def setPSymbols(prog: Program, gs: GlobalScope): Unit = {
      // we still need to do them in order because of method types.
      var checked = Set[ClassSymbol]()

      while(checked.size != prog.classes.size) {
        for (cd <- prog.classes) {
          val classSym = gs.lookupClass(cd.id.value).get

          if(!checked.contains(classSym)) {
            classSym.parent match {
              case Some(ps) if checked.contains(ps) =>
                setCSymbols(cd, gs)
                checked += classSym

              case None =>
                setCSymbols(cd, gs)
                checked += classSym

              case _ =>
            }
          }
        }
      }

      prog.main.stats.foreach(setSSymbols(_)(gs, None))
    }

    def setCSymbols(klass: ClassDecl, gs: GlobalScope): Unit = {
      val classSym = gs.lookupClass(klass.id.value).get

      for (varDecl <- klass.vars) {
        setTypeSymbol(varDecl.tpe, gs)

        varDecl.id.getSymbol match {
          case vs: VariableSymbol => vs.setType(varDecl.tpe.getType)
          case _ => fatal("Class member has a non-variable symbol attached.")
        }
      }

      klass.methods.foreach(setMSymbols(_, gs, classSym))
    }

    def setMSymbols(meth: MethodDecl, gs: GlobalScope, cs: ClassSymbol): Unit = {
      val methSym = cs.lookupMethod(meth.id.value).get

      setTypeSymbol(meth.retType, gs)
      methSym.setType(meth.retType.getType)

      for (formal <- meth.args) {
        setTypeSymbol(formal.tpe, gs)

        formal.id.getSymbol match {
          case vs: VariableSymbol => vs.setType(formal.tpe.getType)
          case _ => fatal("Method parameter has a non-variable symbol attached.")
        }
      }

      for (varDecl <- meth.vars) {
        setTypeSymbol(varDecl.tpe, gs)
        varDecl.id.getSymbol match {
          case vs: VariableSymbol => vs.setType(varDecl.tpe.getType)
          case _ => fatal("Method local has a non-variable symbol attached.")
        }
      }

      // We check whether the method is overriding another one...
      for(pcs <- cs.parent; oms <- pcs.lookupMethod(methSym.name)) {
        // we're supposed to have checked that before.
        assert(oms.argList.length == meth.args.length)

        for ((sarg, oarg) <- meth.args.zip(oms.argList)) {
          if(sarg.id.getType != oarg.getType) {
            error("Formal type in overriding method " + methSym.name + " does not match type in overridden method.", sarg.id)
          }
        }

        if(methSym.getType != oms.getType) {
          error("Method " + methSym.name + " overrides parent method with a different return type (" + methSym.getType + " and " + oms.getType + ")", meth.retExpr)
        }
      }

      meth.stats.foreach(setSSymbols(_)(gs,Some(methSym)))
      setESymbols(meth.retExpr)(gs, Some(methSym))
    }

    def setSSymbols(stat: StatTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]): Unit = stat match {
      case Block(stats) => stats.foreach(setSSymbols)
      case If(expr, thn, elz) =>
        setESymbols(expr)
        setSSymbols(thn)
        elz.foreach(setSSymbols)

      case While(expr, stat) =>
        setESymbols(expr)
        setSSymbols(stat)

      case Println(expr) =>
        setESymbols(expr)
      case Assign(id, expr) =>
        setISymbol(id)
        setESymbols(expr)

      case ArrayAssign(id, index, expr) =>
        setISymbol(id)
        setESymbols(index)
        setESymbols(expr)

      case DoExpr(expr) =>
        setESymbols(expr)
    }

    def setISymbol(id: Identifier)(implicit ms: Option[MethodSymbol]) = {
      // in this context, it will always be an expression (variable)
      ms.flatMap(_.lookupVar(id.value)) match {
        case None =>
          error("Undeclared identifier: " + id.value + ".", id)
        case Some(sym) =>
          id.setSymbol(sym)
          varUsage += sym -> true
      }
    }

    def setESymbols(expr: ExprTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]): Unit = expr match {
      case And(lhs, rhs) =>
        setESymbols(lhs)
        setESymbols(rhs)
      case Or(lhs, rhs) =>
        setESymbols(lhs)
        setESymbols(rhs)
      case Plus(lhs, rhs) =>
        setESymbols(lhs)
        setESymbols(rhs)
      case Minus(lhs, rhs) =>
        setESymbols(lhs)
        setESymbols(rhs)
      case Times(lhs, rhs) =>
        setESymbols(lhs)
        setESymbols(rhs)
      case Div(lhs, rhs) =>
        setESymbols(lhs)
        setESymbols(rhs)
      case LessThan(lhs, rhs) =>
        setESymbols(lhs)
        setESymbols(rhs)
      case Equals(lhs, rhs) =>
        setESymbols(lhs)
        setESymbols(rhs)
      case Not(ex) =>
        setESymbols(ex)
      case ArrayRead(arr, index) =>
        setESymbols(arr)
        setESymbols(index)
      case ArrayLength(arr) =>
        setESymbols(arr)
      case NewIntArray(size) =>
        setESymbols(size)
      case Variable(id) =>
        setISymbol(id)
      case t @ This() =>
        ms.map(_.classSymbol) match {
          case Some(cs) =>
            t.setSymbol(cs)
          case None =>
            error("Cannot use 'this' in the main object", t)
        }

      case New(id @ Identifier(typeName)) =>
        // tpe should always be a class.
        gs.lookupClass(typeName) match {
          case Some(s) =>
            id.setSymbol(s)
          case None =>
            error("Undeclared type: " + typeName + ".", id)
        }

      case MethodCall(obj, meth, args) =>
        setESymbols(obj)
        args.foreach(setESymbols)

      case _ =>
    }

    def setTypeSymbol(tpe: TypeTree, gs: GlobalScope): Unit = tpe match {
      case ClassType(id @ Identifier(value)) => gs.lookupClass(value) match {
        case Some(s) =>
          id.setSymbol(s)

        case None =>
          fatal("Undeclared type: " + value + ".", id)
      }
      case _ =>
    }

    val gs = collectSymbols(prog)

    terminateIfErrors()

    setPSymbols(prog, gs)

    for ((v, used) <- varUsage if !used) {
      warning("Variable " + v.name + " is declared but never used.", v)
    }

    prog
  }
}
