package gnu.kawa.lispexpr;
import gnu.bytecode.*;
import gnu.math.IntNum;
import gnu.math.DFloNum;
import gnu.expr.*;
import gnu.text.Char;

/** Use to implement some special types that convert differently. */

public class LangPrimType extends gnu.bytecode.PrimType
{
  Interpreter interpreter;

  public static LangPrimType byteType = new LangPrimType(Type.byte_type);
  public static LangPrimType shortType = new LangPrimType(Type.short_type);
  public static LangPrimType intType = new LangPrimType(Type.int_type);
  public static LangPrimType longType = new LangPrimType(Type.long_type);
  public static LangPrimType floatType = new LangPrimType(Type.float_type);
  public static LangPrimType doubleType = new LangPrimType(Type.double_type);
  public static LangPrimType charType = new LangPrimType(Type.char_type);
  public static LangPrimType voidType = new LangPrimType(Type.void_type);

  public LangPrimType (PrimType type)
  {
    super(type);
  }

  public LangPrimType (PrimType type, Interpreter interpreter)
  {
    super(type);
    this.interpreter = interpreter;
  }

  public LangPrimType (String nam, String sig, int siz, Class reflectClass)
  {
    super (nam, sig, siz, reflectClass);
  }

  public LangPrimType (String nam, String sig, int siz, Class reflectClass,
		      Interpreter interpreter)
  {
    super (nam, sig, siz, reflectClass);
    this.interpreter = interpreter;
  }

  public Object coerceFromObject (Object obj)
  {
    if (obj.getClass() == reflectClass)
      return obj;
    char sig1 = getSignature().charAt(0);
    switch (sig1)
      {
      case 'Z':
	return interpreter.isTrue(obj) ? Boolean.TRUE : Boolean.FALSE;
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
	interpreter.emitCoerceToBoolean(code);
	break;
      case 'C':
	// We handle char specially, because Kawa does not use standard
	// java.lang.Character type.
	ClassType scmCharType = ClassType.make("gnu.text.Char");
	Method charValueMethod = scmCharType.getDeclaredMethod("charValue", 0);
	code.emitCheckcast(scmCharType);
	code.emitInvokeVirtual(charValueMethod);
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
	return interpreter.booleanObject(((Boolean) obj).booleanValue());
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
    ClassType clas;
    Method method;
    Type[] args;
    switch (sig1)
      {
      case 'Z':
	code.emitIfIntNotZero();
	interpreter.emitPushBoolean(true, code);
	code.emitElse();
	interpreter.emitPushBoolean(false, code);
	code.emitFi();
	break;
      case 'C':
	ClassType scmCharType = ClassType.make("gnu.text.Char");
	Method makeCharMethod = scmCharType.getDeclaredMethod("make", 1);
	code.emitInvokeStatic(makeCharMethod);
	break;
      case 'B':  case 'S':  case 'I':
	clas = ClassType.make("gnu.math.IntNum");
	args = new Type[1];
	args[0] = Type.int_type;
	method = clas.getDeclaredMethod("make", args);
	code.emitInvokeStatic(method);
	break;
      case 'J':
	clas = ClassType.make("gnu.math.IntNum");
	args = new Type[1];
	args[0] = Type.long_type;
	method = clas.getDeclaredMethod("make", args);
	code.emitInvokeStatic(method);
	break;
      case 'F':
	code.emitConvert(Type.float_type, Type.double_type);
	// ... fall through ...
      case 'D':
	clas = ClassType.make("gnu.math.DFloNum");
	args = new Type[1];
	args[0] = Type.double_type;
	method = clas.getDeclaredMethod("make", args);
	code.emitInvokeStatic(method);
	break;
      default:
	super.emitCoerceToObject(code);
      }
  }

}
