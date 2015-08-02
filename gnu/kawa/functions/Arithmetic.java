/** Various utility methods and conversions for handling mixed-mode arithmetic.
 * This should possibly be moved to gnu.math. */

package gnu.kawa.functions;
import gnu.math.*;
import java.math.*;
import gnu.bytecode.*;
import gnu.mapping.Lazy;
import gnu.mapping.Promise;
import gnu.kawa.lispexpr.LangPrimType;
import gnu.kawa.lispexpr.LangObjType;
import gnu.kawa.reflect.LazyType;

public class Arithmetic
{
  /** Promotion code for byte/Byte, short/Short, int/Integer. */
  public static final int INT_CODE = 1;
  public static final int UINT_CODE = 2;
  /** Promotion code for long/Long. */
  public static final int LONG_CODE = 3;
  public static final int ULONG_CODE = 4;
  /** Promotion code for java.math.BigInteger. */
  public static final int BIGINTEGER_CODE = 5;
  /** Promotion code for gnu.math.IntNum. */
  public static final int INTNUM_CODE = 6;
  /** Promotion code for java.math.BigDecimal. */
  public static final int BIGDECIMAL_CODE = 7;
  /** Promotion code for gnu.math.RatNum. */
  public static final int RATNUM_CODE = 8;
  /** Promotion code float/Float. */
  public static final int FLOAT_CODE = 9;
  /** Promotion code double/Double. */
  public static final int DOUBLE_CODE = 10;
  /** Promotion code for gnu.math.FloNum. */
    public static final int FLONUM_CODE = 11;
  /** Promotion code for gnu.math.RealNum. */
  public static final int REALNUM_CODE = 12;
  /** Promotion code for other gnu.math.Numeric. */
  public static final int NUMERIC_CODE = 13;
  public static final int UNKNOWN_CODE = 0;

    public static int leastSpecificCode(int code1, int code2) {
        return code1 <= UNKNOWN_CODE || code2 <= UNKNOWN_CODE ? UNKNOWN_CODE
            : code1 < code2 ? code2 : code1;
    }

    public static int classifyValue(Object value) {
	for (;;) {
	    if (value instanceof Numeric) {
		if (value instanceof IntNum)
		    return INTNUM_CODE;
		else if (value instanceof RatNum)
		    return RATNUM_CODE;
		else if (value instanceof DFloNum)
		    return FLONUM_CODE;
		else if (value instanceof RealNum)
		    return REALNUM_CODE;
		else
		    return NUMERIC_CODE;
	    }
	    else if (value instanceof Number) {
		if (value instanceof Integer || value instanceof Short
		    || value instanceof Byte)
		    return INT_CODE;
		else if (value instanceof Long)
		    return LONG_CODE;
                else if (value instanceof UnsignedPrim)
                    return value instanceof ULong ? ULONG_CODE
                        : value instanceof UInt ? UINT_CODE : INT_CODE;
		else if (value instanceof Float)
		    return FLOAT_CODE;
		else if (value instanceof Double)
		    return DOUBLE_CODE;
		else if (value instanceof BigInteger)
		    return BIGINTEGER_CODE;
		else if (value instanceof BigDecimal)
		    return BIGDECIMAL_CODE;
		else
		    break;
	    }
	    else if (value instanceof Lazy<?>) {
		Object v = ((Lazy) value).getValue();
                if (v == value)
                    break;
                value = v;
	    }
	    else
		break;
	}
        return UNKNOWN_CODE;
    }

  public static Type kindType (int kind)
  {
    switch (kind)
      {
      case INT_CODE:
        return LangPrimType.intType;
      case UINT_CODE:
        return LangPrimType.unsignedIntType;
      case LONG_CODE:
        return LangPrimType.longType;
      case ULONG_CODE:
        return LangPrimType.unsignedLongType;
      case BIGINTEGER_CODE:
        return ClassType.make("java.math.BigInteger");
      case INTNUM_CODE:
        return typeIntNum;
      case BIGDECIMAL_CODE:
        return ClassType.make("java.math.BigDecimal");
      case RATNUM_CODE:
        return typeRatNum;
      case FLOAT_CODE:
        return LangPrimType.floatType;
      case DOUBLE_CODE:
        return LangPrimType.doubleType;
      case FLONUM_CODE:
        return typeDFloNum;
      case REALNUM_CODE:
        return typeRealNum;
      case NUMERIC_CODE:
        return LangObjType.numericType;
      default:
        return Type.pointer_type;
      }
  }

  /*
  static ClassType typeInteger = ClassType.make("java.lang.Integer");
  static ClassType typeLong = ClassType.make("java.lang.Long");
  */
  static LangObjType typeDFloNum = LangObjType.dflonumType;
  static LangObjType typeRatNum = LangObjType.rationalType;
  static LangObjType typeRealNum = LangObjType.realType;
  static ClassType typeNumber = ClassType.make("java.lang.Number");
  static LangObjType typeIntNum = LangObjType.integerType;

  public static int classifyType (Type type)
  {
    int kind = 0;
    if (type instanceof PrimType)
      {
	char sig = type.getSignature().charAt(0);
	if (sig == 'V' || sig == 'Z' || sig == 'C')
	  return UNKNOWN_CODE;
	else if (sig == 'D')
	  return DOUBLE_CODE;
	else if (sig == 'F')
	  return FLOAT_CODE;
        else if (sig == 'J')
          return type == LangPrimType.unsignedLongType ? ULONG_CODE : LONG_CODE;
	else
	  return type == LangPrimType.unsignedIntType ? UINT_CODE :INT_CODE;
      }
    String tname = type.getName();
    if (type.isSubtype(typeIntNum))
      return INTNUM_CODE;
    else if (type.isSubtype(typeRatNum))
      return RATNUM_CODE;
    else if (type.isSubtype(typeDFloNum))
      return FLONUM_CODE;
    else if ("java.lang.Double".equals(tname))
      return DOUBLE_CODE;
    else if ("java.lang.Float".equals(tname))
      return FLOAT_CODE;
    else if ("java.lang.Long".equals(tname))
      return LONG_CODE;
    else if ("gnu.math.ULong".equals(tname))
      return ULONG_CODE;
    else if ("gnu.math.UInt".equals(tname))
      return UINT_CODE;
    else if ("java.lang.Integer".equals(tname)
             || "java.lang.Short".equals(tname)
             || "java.lang.Byte".equals(tname)
             || "gnu.math.UShort".equals(tname)
             || "gnu.math.UByte".equals(tname))
      return INT_CODE;
    else if ("java.math.BigInteger".equals(tname))
      return BIGINTEGER_CODE;
    else if ("java.math.BigDecimal".equals(tname))
      return BIGDECIMAL_CODE;
    else if (type.isSubtype(typeRealNum))
      return REALNUM_CODE;
    else if (type.isSubtype(LangObjType.numericType))
      return NUMERIC_CODE;
    else if (type instanceof LazyType)
      return classifyType(((LazyType) type).getValueType());
    else
      return UNKNOWN_CODE;
  }

  public static int asInt (Object value)
  {
    return ((Number) Promise.force(value)).intValue();
  }

  public static long asLong (Object value)
  {
    return ((Number) Promise.force(value)).longValue();
  }

  public static float asFloat (Object value)
  {
    return ((Number) Promise.force(value)).floatValue();
  }

  public static double asDouble (Object value)
  {
    return ((Number) Promise.force(value)).doubleValue();
  }

  public static BigInteger asBigInteger (Object value)
  {
    value = Promise.force(value);
    if (value instanceof BigInteger)
      return (BigInteger) value;
    if (value instanceof IntNum || value instanceof ULong)
      return new BigInteger(value.toString());
    return BigInteger.valueOf(((Number) value).longValue());
  }

  public static IntNum asIntNum (BigDecimal value)
  {
    return IntNum.valueOf(((BigDecimal) value).toBigInteger().toString(), 10);
  }

  public static IntNum asIntNum (BigInteger value)
  {
    return IntNum.valueOf(value.toString(), 10);
  }

  public static IntNum asIntNum (Object value)
  {
    value = Promise.force(value);
    if (value instanceof IntNum)
      return (IntNum) value;
    if (value instanceof UnsignedPrim)
      return ((UnsignedPrim) value).toIntNum();
    if (value instanceof BigInteger)
      return IntNum.valueOf(value.toString(), 10);
    if (value instanceof BigDecimal)
      return asIntNum((BigDecimal) value);
    return IntNum.valueOf(((Number) value).longValue());
  }

  public static BigDecimal asBigDecimal (Object value)
  {
    value = Promise.force(value);
    if (value instanceof BigDecimal)
      return (BigDecimal) value;
    if (value instanceof BigInteger)
      return new BigDecimal((BigInteger) value);
    if (value instanceof Long || value instanceof Integer
	|| value instanceof Short || value instanceof Byte
        || value instanceof UInt
	|| value instanceof UShort || value instanceof UByte)
      return BigDecimal.valueOf(((Number) value).longValue());
    return new BigDecimal(value.toString());
  }

  public static RatNum asRatNum (Object value)
  {
    value = Promise.force(value);
    if (value instanceof RatNum)
      return (RatNum) value;
    if (value instanceof BigInteger)
      return IntNum.valueOf(value.toString(), 10);
    if (value instanceof ULong)
        return IntNum.valueOfUnsigned(((ULong) value).longValue());
    if (value instanceof BigDecimal)
      return RatNum.valueOf((BigDecimal) value);
    else
      return IntNum.valueOf(((Number) value).longValue());
  }

  public static Numeric asNumeric (Object value)
  {
    value = Promise.force(value);
    Numeric n = Numeric.asNumericOrNull(value);
    return n != null ? n : (Numeric) value;
  }

  /** Convert a number to a String.
   * Handles classes subclasses of gnu.math.Numeric
   * as well as standard Java classes.
   */
  public static String toString (Object number, int radix)
  {
    int code = Arithmetic.classifyValue(number);
    switch (code)
      {
      case Arithmetic.INT_CODE:
        return Integer.toString(Arithmetic.asInt(number), radix);
      case Arithmetic.UINT_CODE:
      case Arithmetic.LONG_CODE:
        return Long.toString(Arithmetic.asLong(number), radix);
      case Arithmetic.ULONG_CODE:
          long lval = Arithmetic.asLong(number);
          /* #ifdef JAVA8 */
          // return Long.toUnsignedString(lval, radix);
          /* #else */
          if (lval >= 0)
              return Long.toString(lval, radix);
          return IntNum.valueOfUnsigned(lval).toString(radix);
          /* #endif */
      case Arithmetic.BIGINTEGER_CODE:
        return Arithmetic.asBigInteger(number).toString(radix);
      case Arithmetic.INTNUM_CODE:
        return Arithmetic.asIntNum(number).toString(radix);
      case Arithmetic.BIGDECIMAL_CODE:
        if (radix == 10)
          return Arithmetic.asBigDecimal(number).toString();
        // else fall through:
      case Arithmetic.FLOAT_CODE:
        if (radix == 10)
          return Float.toString(Arithmetic.asFloat(number));
        // else fall through:
      case Arithmetic.DOUBLE_CODE:
      case Arithmetic.FLONUM_CODE:
        if (radix == 10)
          return Double.toString(Arithmetic.asDouble(number));
        // else fall through:
      default:
        return Arithmetic.asNumeric(number).toString(radix);
      }
  }

  /** Coerce a number to one of the Arithmetic.XXX_CODE types.
   * Assumes {@code > Arithmetic.classifyValue(value)}, though
   * the converse might also work.
   */
  public static Object convert (Object value, int code)
  {
    value = Promise.force(value);
    switch (code)
      {
      case Arithmetic.INT_CODE:
        if (value instanceof Integer)
          return value;
        return Integer.valueOf(((Number) value).intValue());
      case Arithmetic.UINT_CODE:
        if (value instanceof UInt)
          return value;
        return UInt.valueOf(((Number) value).intValue());
      case Arithmetic.LONG_CODE:
        if (value instanceof Long)
          return value;
        return Long.valueOf(((Number) value).longValue());
      case Arithmetic.ULONG_CODE:
        if (value instanceof ULong)
          return value;
        return ULong.valueOf(((Number) value).longValue());
      case Arithmetic.BIGINTEGER_CODE:
        return Arithmetic.asBigInteger(value);
      case Arithmetic.INTNUM_CODE:
        return Arithmetic.asIntNum(value);
      case Arithmetic.BIGDECIMAL_CODE:
        return Arithmetic.asBigDecimal(value);
      case Arithmetic.RATNUM_CODE:
        return Arithmetic.asRatNum(value);
      case Arithmetic.FLOAT_CODE:
        if (value instanceof Float)
          return value;
        float f = Arithmetic.asFloat(value);
        /* #ifdef JAVA5 */
        return Float.valueOf(f);
        /* #else */
        // return new Float(f);
        /* #endif */
      case Arithmetic.DOUBLE_CODE:
        if (value instanceof Double)
          return value;
        double d = Arithmetic.asDouble(value);
        /* #ifdef JAVA5 */
        return Double.valueOf(d);
        /* #else */
        // return new Double(d);
        /* #endif */
      case Arithmetic.FLONUM_CODE:
        if (value instanceof DFloNum)
          return value;
        return DFloNum.valueOf(Arithmetic.asDouble(value));
      case Arithmetic.NUMERIC_CODE:
        return Arithmetic.asNumeric(value);
      case Arithmetic.REALNUM_CODE:
        return (RealNum) Arithmetic.asNumeric(value);
      default:
        return (Number) value;
      }
  }

  public static boolean isExact (Number num)
  {
    if (num instanceof Numeric)
      return ((Numeric) num).isExact();
    return !(num instanceof Double || num instanceof Float
             || num instanceof BigDecimal);
  }

  public static Number toExact (Number num)
  {
    if (num instanceof Numeric)
      return ((Numeric) num).toExact();
    if (num instanceof Double || num instanceof Float
        || num instanceof BigDecimal)
      return DFloNum.toExact(num.doubleValue());
    else
      return num;
  }

  public static Number toInexact (Number num)
  {
    if (num instanceof Numeric)
      return ((Numeric) num).toInexact();
    if (num instanceof Double || num instanceof Float
        || num instanceof BigDecimal)
      return num;
    else
      return Double.valueOf(num.doubleValue());
  }
}
