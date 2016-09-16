package toolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {
  // Bytecodes
  type LocalsPosMapping = Map[String,Int]

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(shortFileName: String, ct: ClassDecl, dir: String): Unit = {
      val cs = ct.getSymbol
      val cf = new ClassFile(cs.name, cs.parent.map(_.name))
      cf.setSourceFile(shortFileName)
      cf.addDefaultConstructor

      ct.vars.map(_.getSymbol).foreach(varSym => {
        cf.addField(typeToDescr(varSym.getType), varSym.name)
      })

      ct.methods.foreach(methDecl => {
        val methSym = methDecl.getSymbol
        val ch = cf.addMethod(
          typeToDescr(methSym.getType),
          methSym.name,
          methSym.argList.map(parSym => typeToDescr(parSym.getType))
        ).codeHandler
        cGenMethod(ch, methDecl)
      })

      try {
        cf.writeToFile(dir + cs.name + ".class")
      } catch {
        case e: Exception => fatal(e.getMessage)
      }
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame


    def cGenMethod(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      // Maps each argument to one local variable index position
      val argMappings = mt.args.zipWithIndex.map { case (arg, index) =>
        arg.id.getSymbol.name -> (index + 1)
      }.toMap

      // Maps each variable to one local variable index position
      val variableMappings = mt.vars.map( v => v.getSymbol.name -> ch.getFreshVar).toMap

      val mapping = argMappings ++ variableMappings

      // generate code for statements
      mt.stats foreach {
        cGenStat(_)(ch, mapping, methSym.classSymbol.name)
      }

      // Generate code for the return expression
      cGenExpr(mt.retExpr)(ch, mapping, methSym.classSymbol.name)

      // Based on the type of the return expression, return with a different
      // value
      ch << (mt.retExpr.getType match {
        case TInt | TBoolean => IRETURN
        case TString | TIntArray | TClass(_) => ARETURN
        case other => sys.error(s"Expected good type but got $other")
      })

      ch.freeze
    }

    def cGenMain(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
      stmts foreach {
        cGenStat(_)(ch, Map(), cname)
      }
      ch << RETURN

      ch.freeze
    }


    // Generates code for a statement
    def cGenStat(statement: StatTree)
              (implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = {
      statement match {
        case Block(stats) =>
          stats foreach cGenStat
        case If(expr,thn,elz) =>
          cGenExpr(expr)
          val endLabel = ch.getFreshLabel("end")
          val elseLabel = ch.getFreshLabel("else")
          ch << IfEq(elseLabel)
          cGenStat(thn)
          elz match {
            case None =>
              ch << Label(elseLabel)
            case Some(ins) =>
              ch << Goto(endLabel)
              ch << Label(elseLabel)
              cGenStat(ins)
              ch << Label(endLabel)
          }

        case While(expr, stat) =>
          val beginLabel = ch.getFreshLabel("begin")
          val endLabel = ch.getFreshLabel("end")

          ch << Label(beginLabel)
          cGenExpr(expr)
          ch << IfEq(endLabel)
          // While body:
          cGenStat(stat)
          ch << Goto(beginLabel)
          ch << Label(endLabel)

        case p @ Println(expr) => // Invoking Java library code
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          cGenExpr(expr)

          // Now: The expression might either be an integer, a boolean, or a
          // string.  we have to invoke the right method accordingly
          ch << LineNumber(p.line)
          val typeStr = expr.getType match {
            case TBoolean => "(Z)V"
            case TInt     => "(I)V"
            case TString  => "(Ljava/lang/String;)V"
            case _        => "(Ljava/lang/Object;)V"
          }
          ch << InvokeVirtual("java/io/PrintStream", "println", typeStr)

        case Assign(id,expr) => id.getSymbol match {
          case vsym: VariableSymbol =>
            mapping get vsym.name match {
              case Some(pos) => // Local variable
                cGenExpr(expr) // get the new value
                vsym.getType match {
                  case TInt | TBoolean => ch << IStore(pos)
                  case TString | TIntArray | TClass(_) => ch << AStore(pos)
                  case other => sys.error(s"Expected good type for store but got $other")
                }

              case None => // Field
                ch << ALoad(0) // this
                cGenExpr(expr) // get the new value
                ch << PutField(cname,vsym.name,typeToDescr(vsym.getType))
              // ch << Comment("Putting field " + vsym.name + " of class " + cname)
            }

          case _ =>
            sys.error("Internal error: symbol of assigned variable should have been of kind VariableSymbol.")
        }
        case ArrayAssign(id,index,expr) => id.getSymbol match { // Almost the same thing
          case vsym: VariableSymbol =>
            mapping get vsym.name match {
              case Some(pos) => ch << ALoad(pos)
              case None =>
                ch << ALoad(0)
                ch << GetField(cname,vsym.name,"[I") // We only deal with integer arrays
            }

            cGenExpr(index)
            cGenExpr(expr)

            ch << IASTORE
          case _ =>
            sys.error("Internal error: symbol of assigned variable should have been of kind VariableSymbol.")
        }

        case DoExpr(expr) =>
          cGenExpr(expr)
          ch << POP // discard the result
      }
    }


    /**
     * Generates code for an expression
     */
    def cGenExpr(expr: ExprTree)
              (implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = {
      expr match {
        case And(lhs,rhs) =>
          ch << ICONST_0
          cGenExpr(lhs)

          val theLabel = ch.getFreshLabel("alreadyFalse")
          ch << IfEq(theLabel)

          // Only care about the right hand side value
          ch << POP
          cGenExpr(rhs)

          ch << Label(theLabel)

        case Or(lhs,rhs) =>
          ch << ICONST_1
          cGenExpr(lhs)

          val theLabel = ch.getFreshLabel("alreadyTrue")
          ch << IfNe(theLabel)

          // Have to take a look at the right hand side
          ch << POP
          cGenExpr(rhs)

          ch << Label(theLabel)

        case p @ Plus(lhs,rhs) =>
          p.getType match {
            case TInt => genBinop(lhs,rhs,IADD,p)
            case _ =>
              ch << DefaultNew("java/lang/StringBuilder")
              cGenExpr(lhs)

              // append
              val lhsCallingType = lhs.getType match {
                case TInt => "(I)Ljava/lang/StringBuilder;"
                case _ => "(Ljava/lang/String;)Ljava/lang/StringBuilder;"
              }

              ch << LineNumber(p.line)
              ch << InvokeVirtual("java/lang/StringBuilder","append",lhsCallingType)

              cGenExpr(rhs)
              val rhsCallingType = rhs.getType match {
                case TInt => "(I)Ljava/lang/StringBuilder;"
                case _ => "(Ljava/lang/String;)Ljava/lang/StringBuilder;"
              }

              ch << LineNumber(p.line)
              ch << InvokeVirtual("java/lang/StringBuilder","append",rhsCallingType)

              // Finally, output string
              ch << InvokeVirtual("java/lang/StringBuilder","toString","()Ljava/lang/String;")
          }
        case m @ Minus(lhs,rhs) => genBinop(lhs,rhs,ISUB,m)
        case t @ Times(lhs,rhs) => genBinop(lhs,rhs,IMUL,t)
        case d @ Div(lhs,rhs) => genBinop(lhs,rhs,IDIV,d)
        case lt @ LessThan(lhs,rhs) =>
          val trueLabel = ch.getFreshLabel("ltrue")
          genComp(lhs,rhs,If_ICmpLt(trueLabel),trueLabel)

        case e @ Equals(lhs,rhs) =>
          val trueLabel = ch.getFreshLabel("ltrue")
          val theBytecode = (lhs.getType,rhs.getType) match {
            case (TInt,TInt) | (TBoolean,TBoolean) => If_ICmpEq(trueLabel)
            case _ =>
              If_ACmpEq(trueLabel) // Everything else compares by referenc.
          }

          genComp(lhs,rhs,theBytecode,trueLabel)

        case ar @ ArrayRead(arr,index) => // Luckily enough, always integers
          cGenExpr(arr)
          cGenExpr(index)
          ch << LineNumber(ar.line)
          ch << IALOAD

        case al @ ArrayLength(arr) =>
          cGenExpr(arr)
          ch << LineNumber(al.line)
          ch << ARRAYLENGTH

        case mc @ MethodCall(obj,meth,args) =>
          cGenExpr(obj)
          args foreach cGenExpr

          val msym = meth.getSymbol.asInstanceOf[MethodSymbol]
          val className = msym.classSymbol.name

          ch << LineNumber(mc.line)
          ch << InvokeVirtual(className,meth.value,
                  // Now the type of the invoked method
                  msym.argList.map(arg => typeToDescr(arg.getType)).mkString("(","",")") +
                  typeToDescr(meth.getSymbol.asInstanceOf[MethodSymbol].getType))

        case nl @ IntLit(value) => ch << LineNumber(nl.line) << Ldc(value)
        case sl @ StringLit(value) => ch << LineNumber(sl.line) << Ldc(value)
        case t @ True() => ch << LineNumber(t.line) << Ldc(1)
        case f @ False() => ch << LineNumber(f.line) << Ldc(0)
        case Variable(id) =>
          id.getSymbol match {
            case vsym: VariableSymbol =>
              mapping get vsym.name match {
                case Some(pos) => vsym.getType match {
                  case TClass(_) | TIntArray | TString => ch << LineNumber(id.line) << ALoad(pos)
                  case _ => ch << LineNumber(id.line) << ILoad(pos)
                }
                case None =>
                  // Not a local variable but a field
                      ch << LineNumber(id.line)
                  ch << ALoad(0) // this
                  ch << GetField(cname,vsym.name,
                         typeToDescr(vsym.getType))
            }
            case other => sys.error("Expected variable symbol but got " + other)
          }

        case t @ This() => ch << LineNumber(t.line) << ALoad(0) // Where this is stored
        case nia @ NewIntArray(size) =>
          cGenExpr(size)
          ch << LineNumber(nia.line)
          ch << NewArray.primitive("T_INT")

        case n @ ast.Trees.New(tpe) =>
          ch << LineNumber(n.line)
          ch << DefaultNew(tpe.value)

        case n @ Not(exp) =>
          cGenExpr(exp)

          val trueLabel = ch.getFreshLabel("ltrue")
          ch << LineNumber(n.line)
          ch << ICONST_1
          ch << SWAP
          ch << IfEq(trueLabel)
          ch << POP
          ch << ICONST_0
          ch << Label(trueLabel)
      }

      def genBinop(lhs: ExprTree, rhs: ExprTree, op: ByteCode, pos: Positioned) = {
        cGenExpr(lhs)
        cGenExpr(rhs)
        ch << LineNumber(pos.line)
        ch << op
      }

      def genComp(lhs: ExprTree, rhs: ExprTree,
            branchInst: AbstractByteCode, trueLabel: String) = {

        ch << ICONST_1 // The true value, for use later
        cGenExpr(lhs)
        cGenExpr(rhs)

        ch << branchInst
        ch << POP
        ch << ICONST_0
        ch << Label(trueLabel)
      }
    }

    def typeToDescr(t: Type): String = t match {
      case TInt => "I"
      case TBoolean => "Z"
      case TIntArray => "[I"
      case TString => "Ljava/lang/String;"
      case TClass(cs) => "L"+cs.name+";"
      case _ => sys.error("Trying to get a descriptor for type: " + t)
    }

    val shortName = ctx.files.head.getName

    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    // output code
    prog.classes foreach {
      generateClassFile(shortName, _, outDir)
    }

    // Main class has a special handling
    val cs = prog.main.getSymbol
    val mainClassFile = new cafebabe.ClassFile(cs.name, None)
    mainClassFile.setSourceFile(shortName)
    mainClassFile.addDefaultConstructor

    // Now do the main object
    cGenMain(
      mainClassFile.addMainMethod.codeHandler,
      prog.main.stats,cs.name
    )

    try {
      mainClassFile.writeToFile(outDir + cs.name + ".class")
    } catch {
      case e: Exception => fatal(e.getMessage)
    }

  }

}
