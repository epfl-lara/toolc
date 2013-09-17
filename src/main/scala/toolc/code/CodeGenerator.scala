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
    def generateClassFile(shortFileName: String, gs: GlobalScope, ct: ClassDecl, dir: String): Unit = {
      val cs = ct.getSymbol
      val cf = new ClassFile(cs.name, cs.parent.map(_.name))
      cf.setSourceFile(shortFileName)
      cf.addDefaultConstructor

      ct.vars.map(_.getSymbol).foreach(varSym => {
        cf.addField(typeToDescr(varSym.getType), varSym.name)
      })

      ct.methods.foreach(methDecl => {
        val methSym = methDecl.getSymbol
        val ch = cf.addMethod(typeToDescr(methSym.getType), methSym.name, methSym.argList.map(parSym => typeToDescr(parSym.getType))).codeHandler
        generateMethodCode(ch, methDecl)
      })

      try {
        cf.writeToFile(dir + cs.name + ".class")
      } catch {
        case e: Exception => fatal(e.getMessage)
      }
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame


    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      // Maps each argument to one local variable index position
      val (argMappings,_) = (mt.args foldLeft (Map[String,Int](),1)) {
        (acc, arg) =>
          val (currMap,currIndex) = acc
          (currMap + (arg.id.getSymbol.name -> currIndex),currIndex + 1)
      }

      // Maps each variable to one local variable index position
      val variableMappings = (mt.vars foldLeft Map[String,Int]()) {
        (acc, v) =>
          acc + (v.getSymbol.name -> ch.getFreshVar)
      }

      val mapping = argMappings ++ variableMappings

      // generate code for statements
      mt.stats foreach {
        stat => generateStatementCode(ch,stat, mapping, methSym.classSymbol.name)
      }

      // Generate code for the return expression
      generateExpressionCode(ch,mt.retExpr,mapping,
               methSym.classSymbol.name)

      // Based on the type of the return expression, return with a different
      // value
      ch << (mt.retExpr.getType match {
        case TInt | TBoolean => IRETURN
        case TString | TIntArray | TObject(_) => ARETURN
        case other => sys.error("Expected good type but got " + other)
      })

      ch.freeze
    }

    def generateMainMethodCode(ch: CodeHandler, mainStatement: StatTree, cname: String): Unit = {
      generateStatementCode(ch,mainStatement,Map(),cname)
      ch << RETURN

      ch.freeze
    }


    // Generates code for a statement
    def generateStatementCode(ch: CodeHandler,
                  statement: StatTree,
                  mapping: LocalsPosMapping,
                  cname: String): Unit = {

      def genStat(stat: StatTree): Unit = stat match {
        case Block(stats) => stats foreach genStat
        case If(expr,thn,elz) =>
          generateExpressionCode(ch,expr,mapping,cname)
          val endLabel = ch.getFreshLabel("end")
          val elseLabel = ch.getFreshLabel("else")
          ch << IfEq(elseLabel)
          genStat(thn)
          elz match {
            case None => ch << Label(elseLabel)
            case Some(ins) => {
              ch << Goto(endLabel)
              ch << Label(elseLabel)
              genStat(ins)
              ch << Label(endLabel)
            }
          }

        case While(expr,stat) =>
          val beginLabel = ch.getFreshLabel("begin")
          val endLabel = ch.getFreshLabel("end")

          ch << Label(beginLabel)
          generateExpressionCode(ch,expr,mapping,cname)
          ch << IfEq(endLabel)
          // While body:
          genStat(stat)
          ch << Goto(beginLabel)
          ch << Label(endLabel)

        case p @ Println(exp) => // Invoking Java library code
          ch << GetStatic("java/lang/System","out","Ljava/io/PrintStream;")
          generateExpressionCode(ch,exp,mapping,cname)

          // Now: The expression might either be an integer, a boolean, or a
          // string.  we have to invoke the right method accordingly
          ch << LineNumber(p.line)
          exp.getType match {
            case TBoolean => ch << InvokeVirtual("java/io/PrintStream","println","(Z)V")
            case TInt => ch << InvokeVirtual("java/io/PrintStream","println","(I)V")
            case TString => ch << InvokeVirtual("java/io/PrintStream","println","(Ljava/lang/String;)V")
            case _ => {
              ch << InvokeVirtual("java/io/PrintStream","println","(Ljava/lang/Object;)V")
            }
          }

        case a @ Assign(id,expr) => id.getSymbol match {
          case vsym: VariableSymbol =>
            mapping get vsym.name match {
              case Some(pos) => { // Local variable
                generateExpressionCode(ch,expr,mapping,cname) // get the new value
                vsym.getType match {
                  case TInt | TBoolean => ch << IStore(pos)
                  case TString | TIntArray | TObject(_) => ch << AStore(pos)
                  case other => sys.error("Expected good type for store but got " + other)
                }
              }
              case None => { // Field
                ch << ALoad(0) // this
                generateExpressionCode(ch,expr,mapping,cname) // get the new value
                ch << PutField(cname,vsym.name,typeToDescr(vsym.getType))
                // ch << Comment("Putting field " + vsym.name + " of class " + cname)
              }
            }

          case _ => sys.error("Internal error: symbol of assigned variable should have been of kind VariableSymbol.")
        }
        case aa @ ArrayAssign(id,index,expr) => id.getSymbol match { // Almost the same thing
          case vsym: VariableSymbol => {
            mapping get vsym.name match {
              case Some(pos) => ch << ALoad(pos)
              case None => {
                ch << ALoad(0)
                ch << GetField(cname,vsym.name,"[I") // We only deal with integer arrays
              }
            }

            generateExpressionCode(ch,index,mapping,cname) // Index
            generateExpressionCode(ch,expr,mapping,cname) // Expression

            ch << IASTORE
          }
          case _ => sys.error("Internal error: symbol of assigned variable should have been of kind VariableSymbol.")
        }
      }

      genStat(statement)
    }


    /**
     * Generates code for an expression
     */
    def generateExpressionCode(ch: CodeHandler,
                         expr: ExprTree,
                         mapping: LocalsPosMapping,
                         cname: String): Unit = {
      def genExp(exp: ExprTree): Unit = exp match {
        case And(lhs,rhs) =>
          ch << ICONST_0
          genExp(lhs)

          val theLabel = ch.getFreshLabel("alreadyFalse")
          ch << IfEq(theLabel)

          // Only care about the right hand side value
          ch << POP
          genExp(rhs)

          ch << Label(theLabel)

        case o @ Or(lhs,rhs) =>
          ch << ICONST_1
          genExp(lhs)

          val theLabel = ch.getFreshLabel("alreadyTrue")
          ch << IfNe(theLabel)

          // Have to take a look at the right hand side
          ch << POP
          genExp(rhs)

          ch << Label(theLabel)

        case p @ Plus(lhs,rhs) =>
          p.getType match {
            case TInt => genBinop(lhs,rhs,IADD,p)
            case _ =>
              ch << DefaultNew("java/lang/StringBuilder")
              genExp(lhs)

              // append
              val lhsCallingType = lhs.getType match {
                case TInt => "(I)Ljava/lang/StringBuilder;"
                case _ => "(Ljava/lang/String;)Ljava/lang/StringBuilder;"
              }

              ch << LineNumber(p.line)
              ch << InvokeVirtual("java/lang/StringBuilder","append",lhsCallingType)

              genExp(rhs)
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
          genExp(arr)
          genExp(index)
          ch << LineNumber(ar.line)
          ch << IALOAD

        case al @ ArrayLength(arr) =>
          genExp(arr)
          ch << LineNumber(al.line)
          ch << ARRAYLENGTH

        case mc @ MethodCall(obj,meth,args) =>
          genExp(obj)
          args foreach genExp

          val msym = meth.getSymbol.asInstanceOf[MethodSymbol]
          val className = msym.classSymbol.name

          ch << LineNumber(mc.line)
          ch << InvokeVirtual(className,meth.value,
                  // Now the type of the invoked method
                  msym.argList.map(arg => typeToDescr(arg.getType)).mkString("(","",")") +
                  typeToDescr(meth.getSymbol.asInstanceOf[MethodSymbol].getType))

        case nl @ NumLit(value) => ch << LineNumber(nl.line) << Ldc(value)
        case sl @ StringLit(value) => ch << LineNumber(sl.line) << Ldc(value)
        case t @ True() => ch << LineNumber(t.line) << Ldc(1)
        case f @ False() => ch << LineNumber(f.line) << Ldc(0)
        case id @ Identifier(_) =>
          id.getSymbol match {
            case vsym: VariableSymbol =>
              mapping get vsym.name match {
                case Some(pos) => vsym.getType match {
                  case TObject(_) | TIntArray | TString => ch << LineNumber(id.line) << ALoad(pos)
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
          genExp(size)
          ch << LineNumber(nia.line)
          ch << NewArray.primitive("T_INT")

        case n @ ast.Trees.New(tpe) =>
          ch << LineNumber(n.line)
          ch << DefaultNew(tpe.value)

        case n @ Not(exp) =>
          genExp(exp)

          val trueLabel = ch.getFreshLabel("ltrue")
          ch << LineNumber(n.line)
          ch << ICONST_1
          ch << SWAP
          ch << IfEq(trueLabel)
          ch << POP
          ch << ICONST_0
          ch << Label(trueLabel)
      }

      def genBinop(lhs : ExprTree, rhs : ExprTree, op : ByteCode, pos : Positioned) = {
        genExp(lhs)
        genExp(rhs)
        ch << LineNumber(pos.line)
        ch << op
      }

      def genComp(lhs: ExprTree, rhs: ExprTree,
            branchInst: AbstractByteCode, trueLabel: String) = {

        ch << ICONST_1 // The true value, for use later
        genExp(lhs)
        genExp(rhs)

        ch << branchInst
        ch << POP
        ch << ICONST_0
        ch << Label(trueLabel)
      }

      genExp(expr)
    }

    def typeToDescr(t: Type): String = t match {
      case TInt => "I"
      case TBoolean => "Z"
      case TIntArray => "[I"
      case TString => "Ljava/lang/String;"
      case TObject(cs) => "L"+cs.name+";"
      case _ => sys.error("Trying to get a descriptor for type: " + t)
    }

  }

}
