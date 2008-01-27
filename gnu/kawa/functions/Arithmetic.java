/** Various utility methods and conversions for handling mixed-mode arithmetic.
 * This should possibly be moved to gnu.math. */

package gnu.kawa.functions;
import gnu.math.*;
import java.math.*;
import gnu.bytecode.*;
import gnu.kawa.lispexpr.LangPrimType;

public class Arithmetic
{
  /** Promotion code for byte/Byte, short/Short, int/Integer. */
  public static final int INT_CODE = 1;
  /** Promotion code for long/Long. */
  public static final int LONG_CODE = 2;
  /** Promotion code for java.math.BigInteger. */
  public static final int BIGINTEGER_CODE = 3;
  /** Promotion code for gnu.math.IntNum. */
  public static final int INTNUM_CODE = 4;
  /** Promotion code for java.math.BigDecimal. */
  public static final int BIGDECIMAL_CODE = 5;
  /** Promotion code for gnu.math.RatNum. */
  public static final int RATNUM_CODE = 6;
  /** Promotion code float/Float. */
  public static final int FLOAT_CODE = 7;
  /** Promotion code double/Double. */
  public static final int DOUBLE_CODE = 8;
  /** Promotion code for gnu.math.FloNum. */
  public static final int FLONUM_CODE = 9;
  /** Promotion code for gnu.math.RealNum. */
  public static final int REALNUM_CODE = 10;
  /** Promotion code for other gnu.math.Numeric. */
  public static final int NUMERIC_CODE = 11;

  public static int classifyValue (Object value)
  {
    if (value instanceof Numeric)
      {
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
    else if (value instanceof Number)
      {
	if (value instanceof Integer || value instanceof Short
	    || value instanceof Byte)
	  return INT_CODE;
	else if (value instanceof Long)
	  return LONG_CODE;
	else if (value instanceof Float)
	  return FLOAT_CODE;
	else if (value instanceof Double)
	  return DOUBLE_CODE;
	else if (value instanceof BigInteger)
	  return BIGINTEGER_CODE;
	else if (value instanceof BigDecimal)
	  return BIGDECIMAL_CODE;
	else
	  return -1;
      }
    else
      return -1;
  }

  public static Type kindType (int kind)
  {
    switch (kind)
      {
      case INT_CODE:
        return LangPrimType.intType;
      case LONG_CODE:
        return LangPrimType.longType;
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
        return typeNumeric;
      default:
        return Type.pointer_type;
      }
  }

  /*
  static ClassType typeInteger = ClassType.make("java.lang.Integer");
  static ClassType typeLong = ClassType.make("java.lang.Long");
  */
  static ClassType typeDFloNum = ClassType.make("gnu.math.DFloNum");
  static ClassType typeRatNum = ClassType.make("gnu.math.RatNum");
  static ClassType typeRealNum = ClassType.make("gnu.math.RealNum");
  static ClassType typeNumber = ClassType.make("java.lang.Number");
  static ClassType typeNumeric = ClassType.make("gnu.math.Numeric");
  static ClassType typeIntNum = ClassType.make("gnu.math.IntNum");

  public static int classifyType (Type type)
  {
    int kind = 0;
    if (type instanceof PrimType)
      {
	char sig = type.getSignature().charAt(0);
	if (sig == 'V' || sig == 'Z' || sig == 'C')
	  return 0;
	else if (sig == 'D')
	  return DOUBLE_CODE;
	else if (sig == 'F')
	  return FLOAT_CODE;
        else if (sig == 'J')
          return LONG_CODE;
	else
	  return INT_CODE;
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
    else if ("java.lang.Integer".equals(tname)
             || "java.lang.Short".equals(tname)
             || "java.lang.Byte".equals(tname))
      return INT_CODE;
    else if ("java.math.BigInteger".equals(tname))
      return BIGINTEGER_CODE;
    else if ("java.math.BigDecimal".equals(tname))
      return BIGDECIMAL_CODE;
    else if (type.isSubtype(typeRealNum))
      return REALNUM_CODE;
    else if (type.isSubtype(typeNumeric))
      return NUMERIC_CODE;
    else
      return 0;
  }

  public static int asInt (Object value)
  {
    return ((Number) value).intValue();
  }

  public static long asLong (Object value)
  {
    return ((Number) value).longValue();
  }

  public static float asFloat (Object value)
  {
    return ((Number) value).floatValue();
  }

  public static double asDouble (Object value)
  {
    return ((Number) value).doubleValue();
  }

  public static BigInteger asBigInteger (Object value)
  {
    if (value instanceof BigInteger)
      return (BigInteger) value;
    if (value instanceof IntNum)
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
    if (value instanceof IntNum)
      return (IntNum) value;
    if (value instanceof BigInteger)
      return IntNum.valueOf(value.toString(), 10);
    if (value instanceof BigDecimal)
      return asIntNum((BigDecimal) value);
    return IntNum.make(((Number) value).longValue());
  }

  public static BigDecimal asBigDecimal (Object value)
  {
    if (value instanceof BigDecimal)
      return (BigDecimal) value;
    if (value instanceof BigInteger)
      return new BigDecimal((BigInteger) value);
    if (value instanceof Long || value instanceof Integer
	|| value instanceof Short || value instanceof Byte)
      return BigDecimal.valueOf(((Number) value).longValue());
    return new BigDecimal(value.toString());
  }

  public static final IntNum ten_exp_9 = IntNum.make(1000000000);

  public static RatNum asRatNum (Object value)
  {
    if (value instanceof RatNum)
      return (RatNum) value;
    if (value instanceof BigInteger)
      return IntNum.valueOf(value.toString(), 10);
    if (value instanceof BigDecimal)
      {
	BigDecimal d = (BigDecimal) value;
	RatNum v = IntNum.valueOf(d.unscaledValue().toString(), 10);
	int scale = d.scale();
	for (; scale >= 9; scale -= 9)
	  v = RatNum.divide(v, ten_exp_9);
	for (; scale <= -9; scale += 9)
	  v = RatNum.times(v, ten_exp_9);
	IntNum scaleVal;
	switch (scale > 0 ? scale : -scale)
	  {
	  case 1: scaleVal = IntNum.make(10);  break;
	  case 2: scaleVal = IntNum.make(100);  break;
	  case 3: scaleVal = IntNum.make(1000);  break;
	  case 4: scaleVal = IntNum.make(10000);  break;
	  case 5: scaleVal = IntNum.make(100000);  break;
	  case 6: scaleVal = IntNum.make(1000000);  break;
	  case 7: scaleVal = IntNum.make(10000000);  break;
	  case 8: scaleVal = IntNum.make(100000000);  break;
	  default:
	    return v;
	  }
	if (scale > 0)
	  return RatNum.divide(v, scaleVal);
	else
	  return RatNum.times(v, scaleVal);
      }
    else
      return IntNum.make(((Number) value).longValue());
  }

  public static Numeric asNumeric (Object value)
  {
    if (! (value instanceof Numeric))
      {
	if (value instanceof BigInteger || value instanceof Long
	    || value instanceof Short || value instanceof Byte
	    || value instanceof Integer)
	  return asIntNum(value);
	if (value instanceof BigDecimal)
	  return asRatNum(value);
	if (value instanceof Float || value instanceof Double)
	  return new DFloNum(asDouble(value));
      }
    return (Numeric) value;
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
      case Arithmetic.LONG_CODE:
        return Long.toString(Arithmetic.asLong(number), radix);
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
    switch (code)
      {
      case Arithmetic.INT_CODE:
        if (value instanceof Integer)
          return value;
        int i = ((Number) value).intValue();
        /* #ifdef JAVA5 */
        return Integer.valueOf(i);
        /* #else */
        // return new Integer(i);
        /* #endif */
      case Arithmetic.LONG_CODE:
        if (value instanceof Long)
          return value;
        long l = ((Number) value).longValue();
        /* #ifdef JAVA5 */
        return Long.valueOf(l);
        /* #else */
        // return new Long(l);
        /* #endif */
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
        return DFloNum.make(Arithmetic.asDouble(value));
      case Arithmetic.NUMERIC_CODE:
        return Arithmetic.asNumeric(value);
      case Arithmetic.REALNUM_CODE:
        return (RealNum) Arithmetic.asNumeric(value);
      default:
        return (Number) value;
      }
  }
}
