/** Various utility methods and conversions for handling mixed-mode arithmetic.
 * This should possibly be moved to gnu.math. */

package gnu.kawa.functions;
import gnu.math.*;
import java.math.*;

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
  /** Promoition code float/Float. */
  public static final int FLOAT_CODE = 7;
  /** Promoition code double/Double. */
  public static final int DOUBLE_CODE = 8;
  /** Promotion code for gnu.math.FloNum. */
  public static final int FLONUM_CODE = 9;
  /** Promotion code for other gnu.math.Numeric. */
  public static final int NUMERIC_CODE = 10;

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

  public static IntNum asIntNum (Object value)
  {
    if (value instanceof IntNum)
      return (IntNum) value;
    if (value instanceof BigInteger)
      return IntNum.valueOf(value.toString(), 10);
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
}
