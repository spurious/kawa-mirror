package kawa.lang;
import gnu.bytecode.*;
import gnu.math.IntNum;
import gnu.math.DFloNum;
import gnu.expr.*;
import gnu.kawa.util.Char;

/** Use to implement some special types that convert differently. */

public class SpecialType extends gnu.bytecode.PrimType
{
  public SpecialType (String nam, String sig, int siz, Class reflectClass)
  {
    super (nam, sig, siz, reflectClass);
  }

  public Object coerceFromObject (Object obj)
  {
    if (obj.getClass() == reflectClass)
      return obj;
    char sig1 = getSignature().charAt(0);
    switch (sig1)
      {
      case 'Z':
	return obj == Boolean.FALSE ? obj : Boolean.TRUE;
      case 'C':
	return new Character(((Char) obj).charValue());
      }
    return super.coerceFromObject(obj);
  }

  public char charValue (Object value)
  {
    if (value instanceof Character)
      return ((Character) value).charValue();
    return  ((Char) value).charValue();
  }

  public void emitCoerceFromObject (CodeAttr code)
  {
    char sig1 = getSignature().charAt(0);
    switch (sig1)
      {
      case 'Z':
	code.emitGetStatic(Compilation.falseConstant);
	code.emitIfNEq();
	code.emitPushInt(1);
	code.emitElse();
	code.emitPushInt(0);
	code.emitFi();
	break;
      case 'C':
	// We handle char specially, because Kawa does not use standard
	// java.lang.Character type.
	Char.initMakeMethods();
	code.emitCheckcast(Char.scmCharType);
	code.emitInvokeVirtual(Char.charValueMethod);
	break;
      default:
	super.emitCoerceFromObject(code);
      }
  }

  public Object coerceToObject (Object obj)
  {
    char sig1 = getSignature().charAt(0);
    switch (sig1)
      {
      case 'Z':
	return ((Boolean) obj).booleanValue() ? Boolean.TRUE : Boolean.FALSE;
      case 'C':
	if (obj instanceof Char)
	  return obj;
	return Char.make(((Character) obj).charValue());
      case 'B':  case 'S':  case 'I':
	return IntNum.make(((Number) obj).intValue());
      case 'J':
	return IntNum.make(((Number) obj).longValue());
      case 'D':  case 'F':
	return DFloNum.make(((Number) obj).doubleValue());
      case 'V':
	return gnu.mapping.Values.empty;
      }
    return super.coerceToObject(obj);
  }

  public void emitCoerceToObject (CodeAttr code)
  {
    char sig1 = getSignature().charAt(0);
    switch (sig1)
      {
      case 'Z':
	code.emitIfIntNotZero();
	code.emitGetStatic(Compilation.trueConstant);
	code.emitElse();
	code.emitGetStatic(Compilation.falseConstant);
	code.emitFi();
	break;
      case 'C':
	Char.initMakeMethods();
	code.emitInvokeStatic(Char.makeCharMethod);
	break;
      case 'B':  case 'S':  case 'I':
	IntNum.initMakeMethods();
	code.emitInvokeStatic(IntNum.makeIntMethod);
	break;
      case 'J':
	IntNum.initMakeMethods();
	code.emitInvokeStatic(IntNum.makeLongMethod);
	break;
      case 'F':
	code.emitConvert(Type.float_type, Type.double_type);
	// ... fall through ...
      case 'D':
	DFloNum.initMakeMethods();
	code.emitInvokeStatic(DFloNum.makeMethod);
	break;
      default:
	super.emitCoerceToObject(code);
      }
  }

}
