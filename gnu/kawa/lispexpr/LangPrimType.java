package gnu.kawa.lispexpr;
import gnu.bytecode.*;
import gnu.math.IntNum;
import gnu.math.DFloNum;
import gnu.expr.*;
import gnu.text.Char;
import gnu.mapping.Procedure;
import gnu.mapping.Values;

/** Use to implement some special types that convert differently.
 * May not be needed/appropriate any more now that we support
 * arithmetic on a mix of Java and Kawa types.
 */

public class LangPrimType extends PrimType implements TypeValue
{
  Language language;
  PrimType implementationType;

  public static final PrimType byteType = Type.byteType;
  public static final PrimType shortType = Type.shortType;
  public static final PrimType intType = Type.intType;
  public static final PrimType longType = Type.longType;
  public static final PrimType floatType = Type.floatType;
  public static final PrimType doubleType = Type.doubleType;
  public static final LangPrimType charType
    = new LangPrimType(Type.charType);
  public static final LangPrimType voidType
    = new LangPrimType(Type.voidType);

  public LangPrimType (PrimType type)
  {
    super(type);
    implementationType = type;
  }

  public LangPrimType (PrimType type, Language language)
  {
    super(type);
    this.language = language;
    implementationType = type;
  }

  public LangPrimType (String nam, String sig, int siz, Class reflectClass)
  {
    super (nam, sig, siz, reflectClass);
  }

  public LangPrimType (String nam, String sig, int siz, Class reflectClass,
		      Language language)
  {
    this(nam, sig, siz, reflectClass);
    implementationType = Type.signatureToPrimitive(sig.charAt(0));
    this.language = language;
  }

  public Type getImplementationType()
  {
    return implementationType;
  }

  public Object coerceFromObject (Object obj)
  {
    if (obj.getClass() == reflectClass)
      return obj;
    char sig1 = getSignature().charAt(0);
    switch (sig1)
      {
      case 'Z':
	return language.isTrue(obj) ? Boolean.TRUE : Boolean.FALSE;
      case 'C':
	return new Character(((Char) obj).charValue());
      case 'V':
        return Values.empty;
      }
    return super.coerceFromObject(obj);
  }

  public char charValue (Object value)
  {
    if (value instanceof Character)
      return ((Character) value).charValue();
    return  ((Char) value).charValue();
  }

  public void emitIsInstance (CodeAttr code)
  {
    char sig1 = getSignature().charAt(0);
    switch (sig1)
      {
      case 'Z':
	code.emitPop(1);
	code.emitPushInt(1);
	break;
      case 'C':
	ClassType scmCharType = ClassType.make("gnu.text.Char");
	code.emitInstanceof(scmCharType);
	break;
      default:
	super.emitIsInstance(code);
      }
  }

  public void emitCoerceFromObject (CodeAttr code)
  {
    char sig1 = getSignature().charAt(0);
    switch (sig1)
      {
      case 'Z':
	language.emitCoerceToBoolean(code);
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
	return language.booleanObject(((Boolean) obj).booleanValue());
      case 'C':
	if (obj instanceof Char)
	  return obj;
	return Char.make(((Character) obj).charValue());
      case 'V':
        // Perhaps we should return Language.noValue() instead?
	return gnu.mapping.Values.empty;
      }
    return super.coerceToObject(obj);
  }

  public void emitCoerceToObject (CodeAttr code)
  {
    char sig1 = getSignature().charAt(0);
    Type argType = null;
    String cname = null;
    switch (sig1)
      {
      case 'Z':
	code.emitIfIntNotZero();
	language.emitPushBoolean(true, code);
	code.emitElse();
	language.emitPushBoolean(false, code);
	code.emitFi();
	break;
      case 'C':
	ClassType scmCharType = ClassType.make("gnu.text.Char");
	Method makeCharMethod = scmCharType.getDeclaredMethod("make", 1);
	code.emitInvokeStatic(makeCharMethod);
	break;
      default:
	super.emitCoerceToObject(code);
      }
    if (cname != null)
      {
	ClassType clas = ClassType.make(cname);
	Type[] args = { argType };
	code.emitInvokeStatic(clas.getDeclaredMethod("make", args));
      }
  }

  public int compare(Type other)
  {
    // Anything (except void) can be converted to boolean.
    char sig1 = getSignature().charAt(0);
    if (other instanceof PrimType)
      {
	char sig2 = other.getSignature().charAt(0);
	if (sig1 == sig2)
	  return 0;
	if (sig1 == 'V')
	  return 1;
	if (sig2 == 'V' || sig2 == 'Z')
	  return -1;
      }
    if (sig1 == 'V' || sig1 == 'Z')
      return 1;
    if (sig1 == 'C' && other.getName().equals("gnu.text.Char"))
      return -1;
    if (other instanceof LangObjType)
      return swappedCompareResult(other.compare(this));
    return super.compare(other);
  }

  public void emitTestIf(Variable incoming, Declaration decl, Compilation comp)
  {
    char sig1 = getSignature().charAt(0);
    /*
    switch (sig1)
      {
      case 'Z':
      }
    */
    CodeAttr code = comp.getCode();
    if (incoming != null)
      code.emitLoad(incoming);
    if (decl != null)
      {
	code.emitDup();
	decl.compileStore(comp);
      }
    emitIsInstance(code);
    code.emitIfIntNotZero();
  }

  public Expression convertValue (Expression value)
  {
    return null;
  }

  public void emitIsInstance(Variable incoming,
			     Compilation comp, Target target)
  {
    gnu.kawa.reflect.InstanceOf.emitIsInstance(this, incoming, comp, target);
  }

  public Procedure getConstructor ()
  {
    return null;
  }
}
