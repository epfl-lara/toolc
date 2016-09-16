package toolc
package analyzer

import Symbols._

object Types {
  trait Typed {
    self =>
    
    private var _tpe: Type = TUntyped
    
    def setType(tpe: Type): self.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }
  
  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }
  
  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }
  
  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }
  
  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe == this
    override def toString = "Int"
  }
  
  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe == this
    override def toString = "Bool"
  }
  
  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe == this
    override def toString = "String"
  }
  
  case object TIntArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TIntArray => true
      case _ => false
    }
    override def toString = "Int[]"
  }
  
  case class TClass(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = {
      def findInSymbol(cs1: ClassSymbol, cs2: ClassSymbol): Boolean = {
        if (cs1 == cs2) {
          true
        } else {
          cs1.parent.exists(findInSymbol(_, cs2))
        }
      }

      tpe match {
        case `TObject` => true
        case TClass(cs2) => findInSymbol(classSymbol, cs2)
        case _ => false
      }
    }
    override def toString = classSymbol.name
  }

  // The top of the class hierarchy. Does not correspond to anything in a Tool program,
  // we just use if for convenience during type checking.
  val TObject = TClass(new ClassSymbol("Object"))
}
