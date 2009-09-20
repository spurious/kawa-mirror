package gnu.kawa.functions;
import gnu.math.*;
import gnu.mapping.*;
import java.math.*;

/**
 * Implement the Scheme standard function "/".
 * @author Per Bothner
 */

public class DivideOp extends ProcedureN
{
  /** Implement's Scheme {@code /} operation. */
  public static final int GENERIC = 0;

  /** Implements a division operation.
   * Like Scheme's {@code (exact->inexact (/ x y))}.
   */
  public static final int DIVIDE_INEXACT = 1;

  /** Implements a quotient operation.
   * Depends on the value of {@code getRoundingMode()}.
   * Operands are real; result is an integer.
   * Inexact operands yield inexact integer result.
   */
  public static final int QUOTIENT = 2;
  /** Implements a quotient operation.
   * Depends on the value of {@code getRoundingMode()}.
   * Operands and result are real.
   * Operands are real; result is an integer.
   * Inexact operands yield exact integer result.
   */
  public static final int QUOTIENT_EXACT = 3;

  /** Implements a modulo/remainder operation.
   * Depends on the value of {@code getRoundingMode()}.
   * Operands and result are real.
   */
  public static final int MODULO = 4;

  // GENERIC, MODULO, QUOTIENT, QUOTIENT_EXACT
  int op;

  /** Return one of FLOOR, CEILING, TRUNCATE, ROUND, or 0 if not applicable.
   * These are defined in gnu.math.Numeric.
   */
  public int getRoundingMode() { return rounding_mode; }
  int rounding_mode;

  public static final DivideOp $Sl = new DivideOp("/");
  public static final DivideOp idiv = new DivideOp("idiv");
  public static final DivideOp quotient = new DivideOp("quotient");
  public static final DivideOp remainder = new DivideOp("remainder");
  public static final DivideOp modulo = new DivideOp("modulo");
  public static final DivideOp div = new DivideOp("div");
  public static final DivideOp mod = new DivideOp("mod");
  public static final DivideOp div0 = new DivideOp("div0");
  public static final DivideOp mod0 = new DivideOp("mod0");
  static {
    idiv.op = QUOTIENT_EXACT;
    idiv.rounding_mode = Numeric.TRUNCATE;
    quotient.op = QUOTIENT;
    quotient.rounding_mode = Numeric.TRUNCATE;
    remainder.op = MODULO;
    remainder.rounding_mode = Numeric.TRUNCATE;
    modulo.op = MODULO;
    modulo.rounding_mode = Numeric.FLOOR;
    div.op = QUOTIENT;
    div.rounding_mode = Numeric.NONNEG_MOD;
    mod.op = MODULO;
    mod.rounding_mode = Numeric.NONNEG_MOD;
    div0.op = QUOTIENT;
    div0.rounding_mode = Numeric.ROUND;
    mod0.op = MODULO;
    mod0.rounding_mode = Numeric.ROUND;
  }

  public DivideOp(String name)
  {
    super(name);
    Procedure.inlineCallsKey.set(this, "*gnu.kawa.functions.CompileArith:forDiv");
    Procedure.compilerKey.set(this, "*gnu.kawa.functions.CompileArith:forDiv");
  }

  public Object applyN (Object[] args)
    throws Throwable
  {
    int len = args.length;
    if (len == 0)
      return IntNum.one ();
    Number result = (Number) args[0];
    if (len == 1)
      return apply2(IntNum.one(), result);
    int code = Arithmetic.classifyValue(result);
    for (int i = 1; i < len; i++)
      {
	Object arg2 = args[i];
	int code2 = Arithmetic.classifyValue(arg2);
	code = code < code2 ? code2 : code;
        int scode = code;
        if (code < Arithmetic.INTNUM_CODE)
          {
            switch (op)
              {
              case GENERIC:
              case DIVIDE_INEXACT:
                scode = code = Arithmetic.INTNUM_CODE;
                break;
              default:
                if (rounding_mode == Numeric.TRUNCATE
                    && (code == Arithmetic.INT_CODE || code == Arithmetic.LONG_CODE))
                  ; // handled specifically
                else
                  scode = Arithmetic.INTNUM_CODE;
                break;
              }
          }
        if (op == DIVIDE_INEXACT && code <= Arithmetic.REALNUM_CODE)
          {
            scode = Arithmetic.REALNUM_CODE;
            if (code != Arithmetic.DOUBLE_CODE && code != Arithmetic.FLOAT_CODE)
              code = Arithmetic.FLONUM_CODE;
          }
        else if (scode == Arithmetic.DOUBLE_CODE || scode ==  Arithmetic.FLOAT_CODE)
          {
            scode = Arithmetic.FLONUM_CODE;
            if (op == QUOTIENT_EXACT)
              code = scode;
          }
	switch (scode)
	  {
	  case Arithmetic.INT_CODE:
	    int i1 = Arithmetic.asInt(result);
	    int i2 = Arithmetic.asInt(arg2);
            switch (op)
              {
              case MODULO:
                i1 = i1 % i2;
                break;
              default:
                i1 = i1 / i2;
                break;
              }
	    result = Integer.valueOf(i1);
	    break;
	  case Arithmetic.LONG_CODE:
	    long l1 = Arithmetic.asLong(result);
	    long l2 = Arithmetic.asLong(arg2);
            switch (op)
              {
              case MODULO:
                l1 = l1 % l2;
                break;
              default:
                l1 = l1 / l2;
                break;
              }
	    result = Long.valueOf(l1);
	    break;
	  case Arithmetic.INTNUM_CODE:
            switch (op)
              {
              case QUOTIENT:
              case QUOTIENT_EXACT:
                result = IntNum.quotient(Arithmetic.asIntNum(result),
                                         Arithmetic.asIntNum(arg2),
                                         getRoundingMode());
                break;
              case MODULO:
                result = IntNum.remainder(Arithmetic.asIntNum(result),
                                          Arithmetic.asIntNum(arg2),
                                          getRoundingMode());
                break;
              case GENERIC:
                result = RatNum.make(Arithmetic.asIntNum(result),
                                     Arithmetic.asIntNum(arg2));
                code = result instanceof IntNum ? Arithmetic.INTNUM_CODE
                  : Arithmetic.RATNUM_CODE;
                scode = code;
                break;
              }
	    break;
          /* #ifdef JAVA5 */
	  case Arithmetic.BIGDECIMAL_CODE:
	    BigDecimal bd1 = Arithmetic.asBigDecimal(result);
	    BigDecimal bd2 = Arithmetic.asBigDecimal(arg2);
            int mprec = 0; // ???
            RoundingMode mround;
            switch (getRoundingMode())
              {
              case Numeric.FLOOR:
                mround = RoundingMode.FLOOR;
                break;
              case Numeric.CEILING:
                mround = RoundingMode.CEILING;
                break;
              case Numeric.TRUNCATE:
                mround = RoundingMode.DOWN;
                break;
              case Numeric.NONNEG_MOD:
                mround = bd2.signum() < 0 ? RoundingMode.CEILING : RoundingMode.FLOOR;
              default:  /* case ROUND: */
                mround = RoundingMode.HALF_EVEN;
                break;
              }
            MathContext mcontext = new MathContext(mprec, mround);
            switch (op)
              {
              case GENERIC:
                result = bd1.divide(bd2);
                break;
              case QUOTIENT:
                result = bd1.divideToIntegralValue(bd2, mcontext);
                break;
              case QUOTIENT_EXACT:
                result = bd1.divideToIntegralValue(bd2, mcontext)
                  .toBigInteger();
                code = scode = Arithmetic.BIGINTEGER_CODE;
                break;
              case MODULO:
                result = bd1.remainder(bd2, mcontext);
                break;
              }
	    break;
          /* #endif */
	  case Arithmetic.FLONUM_CODE:
	    double d1 = Arithmetic.asDouble(result);
	    double d2 = Arithmetic.asDouble(arg2);
            switch (op)
              {
              case GENERIC:
              case DIVIDE_INEXACT:
                result = DFloNum.make(d1 / d2);
                break;
              case QUOTIENT:
                result = RealNum.toInt(d1 / d2, getRoundingMode());
                break;
              case QUOTIENT_EXACT:
                result = RealNum.toExactInt(d1 / d2, getRoundingMode());
                code = scode = Arithmetic.INTNUM_CODE;
                break;
              case MODULO:
                if (d2 != 0)
                  d1 = d1 - RealNum.toInt(d1 / d2, getRoundingMode()) * d2;
                result = DFloNum.make(d1);
                break;
              }
	    break;
	  case Arithmetic.REALNUM_CODE:
	  default:
            Numeric num1 = Arithmetic.asNumeric(result);
            Numeric num2 = Arithmetic.asNumeric(arg2);
            if (op == MODULO && num2.isZero())
              return num2.isExact() ? num1 : num1.toInexact();
            Numeric numr = num1.div(num2);
            if (op == MODULO)
              numr = num1.sub(((RealNum) numr).toInt(getRoundingMode()).mul(num2));
            switch (op)
              {
              case QUOTIENT_EXACT:
                result = ((RealNum) numr).toExactInt(rounding_mode);
                scode = code = Arithmetic.INTNUM_CODE;
                break;
              case QUOTIENT:
                result = ((RealNum) numr).toInt(rounding_mode);
                break;
              case DIVIDE_INEXACT:
                result = numr.toInexact();
                break;
              default:
                result = numr;
              }
	  }
        if (code != scode)
          {
            switch (code)
              {
              case Arithmetic.INT_CODE:
                result = Integer.valueOf(result.intValue());
                break;
              case Arithmetic.LONG_CODE:
                result = Long.valueOf(result.longValue());
                break;
              case Arithmetic.FLOAT_CODE:
                result = Float.valueOf(result.floatValue());
                break;
              case Arithmetic.DOUBLE_CODE:
                result = Double.valueOf(result.doubleValue());
                break;
              case Arithmetic.BIGINTEGER_CODE:
                result = Arithmetic.asBigInteger(result);
              }
          }
      }
    return result;
   }

  public int numArgs()
  {
    return op == GENERIC ? 0xfffff001 : 0x2002;
  }
}
