package kawa.standard;
import kawa.lang.*;
import kawa.math.*;

/** Implement the standard Scheme procedure "expt". */

public class expt extends Procedure2
{
  public Object apply2 (Object arg1, Object arg2)
  {
    if (arg1 instanceof IntNum && arg2 instanceof IntNum)
      {
	IntNum i1 = (IntNum) arg1;
	IntNum i2 = (IntNum) arg2;
	if (i2.isZero ())
	  return IntNum.one ();
	if (i1.isZero ())
	  return IntNum.zero ();
	if (i2.words != null)
	  {
	    if (IntNum.compare (i1, IntNum.one ()) == 0)
	      return IntNum.one ();
	    if (IntNum.compare (i1, IntNum.make (-1)) == 0)
	      return  i2.isOdd () ? i1 : IntNum.one ();
	    throw new ArithmeticException ("exponent too big in expt");
	  }
	// temporary hack for R4RS required exactness - FIXME
	if ((i1.words == null || i1.ival <= 2)
	    && i2.ival >= 0)
	  {
	    double d1 = i1.doubleValue ();
	    double d2 = i2.doubleValue ();
	    boolean neg = false;
	    if (d1 < 0)
	      {
		d1 = -d1;
		if (i2.isOdd ())
		  neg = true;
	      }
	    double result = Math.pow (d1, d2);
	    if (neg)
	      result = -result;
	    if (result < (double) 0x1000000000000L) // exact, supposedly
	      return IntNum.make ((long)result);
	    else
	      return new DFloNum (result);	    
	  }
      }
    return new DFloNum (Math.pow (((RealNum)arg1).doubleValue (),
				  ((RealNum)arg2).doubleValue ()));
  }
}
