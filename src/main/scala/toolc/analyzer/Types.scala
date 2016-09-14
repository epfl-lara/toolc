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
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _ => false
    }
    override def toString = "int"
  }
  
  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _ => false
    }
    override def toString = "boolean"
  }
  
  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _ => false
    }
    override def toString = "String"
  }
  
  case object TIntArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TIntArray => true
      case _ => false
    }
    override def toString = "int[]"
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
        case TClass(cs2) => cs2.name.equals("Object") || findInSymbol(classSymbol, cs2)
        case _ => false
      }
    }
    override def toString = classSymbol.name
  }

  // The top of the class hierarchy
  val TObject = TClass(new ClassSymbol("Object"))
}
