package kawa.math;
import kawa.lang.*;
import codegen.Method;
import codegen.ClassType;
import codegen.Access;
import codegen.Type;

/** A class for infinite-precision integers.
 * @author Per Bothner
 */

public class IntNum extends RatNum implements Compilable
{
  /** All integers are stored in 2's-complement form.
   * If words == null, the ival is the value of this IntNum.
   * Otherwise, the first ival elements of words make the value
   * of this IntNum, stored in little-endian order, 2's-complement form. */
  public int ival;
  public int[] words;


  /** We pre-allocate integers in the range minFixNum..maxFixNum. */
  static final int minFixNum = -100;
  static final int maxFixNum = 1024;
  static final int numFixNum = maxFixNum-minFixNum+1;
  static final IntNum[] smallFixNums = new IntNum[numFixNum];

  static {
    for (int i = numFixNum;  --i >= 0; )
      smallFixNums[i] = new IntNum(i + minFixNum);
  }

  public IntNum ()
  {
  }

  /** Create a new (non-shared) IntNum, and initialize to an int.
   * @param value the initial value */
  public IntNum (int value)
  {
    ival = value;
  }

  /** Return a (possibly-shared) IntNum with a given int value. */
  public static IntNum make (int value)
  {
    if (value >= minFixNum && value <= maxFixNum)
      return smallFixNums[(int) value - minFixNum];
    else
      return new IntNum (value);
  }

  public static final IntNum zero ()
  {
    return smallFixNums[- minFixNum];
  }

  public static final IntNum one ()
  {
    return smallFixNums[1 - minFixNum];
  }

  /** Return the IntNum for -1. */
  public static IntNum minusOne ()
  {
    return smallFixNums[-1 - minFixNum];
  }

  /** Return a (possibly-shared) IntNum with a given long value. */
  public static IntNum make (long value)
  {
    if (value >= minFixNum && value <= maxFixNum)
      return smallFixNums[(int)value - minFixNum];
    int i = (int) value;
    if ((long)i == value)
      return new IntNum (i);
    IntNum result = alloc (2);
    result.ival = 2;
    result.words[0] = i;
    result.words[1] = (int) (value >> 32);
    return result;
  }

  /** Make an IntNum from an array of words.
   * The array may be reused (i.e. not copied). */
  public static IntNum make (int[] words, int len)
  {
    if (words == null)
      return make (len);
    len = IntNum.wordsNeeded (words, len);
    if (len <= 0)
      return len == 0 ? zero () : make (words[0]);
    IntNum num = new IntNum ();
    num.words = words;
    num.ival = len;
    return num;
  }

  /** Allocate a new non-shared IntNum.
   * @param nwords number of words to allocate
   */
  public static IntNum alloc (int nwords)
  {
    if (nwords <= 1)
      return new IntNum ();
    IntNum result = new IntNum ();
    result.words = new int[nwords];
    return result;
  }

  /** Change words.length to nwords.
  * We allow words.length to be upto nwords+2 without reallocating. */
  public void realloc (int nwords)
  {
    if (nwords == 0)
      {
	if (words != null)
	  {
	    if (ival > 0)
	      ival = words[0];
	    words = null;
	  }
      }
    else if (words == null
	     || words.length < nwords
	     || words.length > nwords + 2)
      {
	int[] new_words = new int [nwords];
	if (words == null)
	  {
	    new_words[0] = ival;
	    ival = 1;
	  }
	else
	  {
	    if (nwords < ival)
	      ival = nwords;
	    System.arraycopy (words, 0, new_words, 0, ival);
	  }
	words = new_words;
      }
  }

  public final IntNum numerator ()
  {
    return this;
  }

  public final IntNum denominator ()
  {
    return one ();
  }

  public final boolean isNegative ()
  {
    return (words == null ? ival : words[ival-1]) < 0;
  }

  public int sign ()
  {
    int top = words == null ? ival : words[ival-1];
    return top > 0 ? 1 : top < 0 ? -1 : 0;
  }

  public static int compare (IntNum x, IntNum y)
  {
    if (x.words == null && y.words == null)
      return x.ival < y.ival ? -1 : x.ival > y.ival ? 1 : 0;
    boolean x_negative = x.isNegative ();
    boolean y_negative = y.isNegative ();
    if (x_negative != y_negative)
      return x_negative ? -1 : 1;
    int x_len = x.words == null ? 1 : x.ival;
    int y_len = y.words == null ? 1 : y.ival;
    if (x_len != y_len)
      return (x_len > y_len)!=x_negative ? 1 : -1;
    return MPN.cmp (x.words, y.words, x_len);
  }

  public int compare (Object obj)
  {
    if (obj instanceof IntNum)
      return compare (this, (IntNum) obj);
    if (! (obj instanceof RealNum))
      throw new IllegalArgumentException ();
    return ((RealNum)obj).compare_reversed (this);
  }

  public final boolean isOdd ()
  {
    int low = words == null ? ival : words[0];
    return (low & 1) != 0;
  }

  public final boolean isZero ()
  {
    return words == null && ival == 0;
  }

  public final boolean isOne ()
  {
    return words == null && ival == 1;
  }

  public final boolean isMinusOne ()
  {
    return words == null && ival == -1;
  }

  /** Calculate how many words are significant in words[0:len-1].
   * Returns the least value x such that x>0 && words[0:x-1]==words[0:len-1],
   * when words is views as a 2's complement integer.
   */
  public static int wordsNeeded (int[] words, int len)
  {
    int i = len;
    if (i > 0)
      {
	int word = words[--i];
	if (word == -1)
	  {
	    while (i > 0 && (word = words[i-1]) < 0)
	      {
		i--;
		if (word != -1) break;
	      }
	  }
	else
	  {
	    while (word == 0 && i > 0 && (word = words[i-1]) >= 0)  i--;
	  }
      }
    return i+1;
  }

  public IntNum canonicalize ()
  {
    if (words != null
	&& (ival = IntNum.wordsNeeded (words, ival)) <= 1)
      {
	if (ival == 1)
	  ival = words[0];
	words = null;
      }
    if (words == null && ival >= minFixNum && ival <= maxFixNum)
      return smallFixNums[(int) ival - minFixNum];
    return this;
  }

  /** Add two ints, yielding an IntNum. */
  public static final IntNum plus (int x, int y)
  {
    return IntNum.make ((long) x + (long) y);
  }

  /** Add an IntNum and an int, yielding a new IntNum. */
  public static IntNum plus (IntNum x, int y)
  {
    if (x.words == null)
      return IntNum.plus (x.ival, y);
    IntNum result = new IntNum (0);
    result.setPlus (x, y);
    return result.canonicalize ();
  }

  /** Set this to the sum of x and y.
   * OK if x==this. */
  public void setPlus (IntNum x, int y)
  {
    if (x.words == null)
      {
	set ((long) x.ival + (long) y);
	return;
      }
    int len = x.ival;
    realloc (len + 1);
    if (x.words[len - 1] >= 0)
      words[len] = MPN.add_1 (words, x.words, len, y);
    else
      {
	long carry = y;
	for (int i = 0;  i < len;  i++)
	  {
	    carry += ((long) x.words[i] & 0xffffffffL);
	    words[i] = (int) carry;
	    carry >>= 32;
	  }
	carry--;
	words[len] = (int) carry;
      }
    ival = wordsNeeded (words, len+1);
  }

  /** Destructively add an int to this. */
  public final void setPlus (int y)
  {
    setPlus (this, y);
  }

  /** Destructively set the value of this to an int. */
  public final void set (int y)
  {
    words = null;
    ival = y;
  }

  /** Destructively set the value of this to a long. */
  public final void set (long y)
  {
    int i = (int) y;
    if ((long) i == y)
      {
	ival = i;
	words = null;
      }
    else
      {
	realloc (2);
	words[0] = i;
	words[1] = (int) (y >> 32);
	ival = 2;
      }
  }

  /** Destructively set the value of this to the given words.
  * The words array is reused, not copied. */
  public final void set (int[] words, int length)
  {
    this.ival = length;
    this.words = words;
  }

  /** Destructively set the value of this to that of y. */
  public final void set (IntNum y)
  {
    if (y.words == null)
      set (y.ival);
    else if (this != y)
      set (y.words, y.ival); //COPY
  }

  // Assumes that words != 0.
  final void appendWord (int word)
  {
    if (ival == words.length)
      realloc (ival + 1);
    words[ival++] = word;
  }

  /** Destructively add y&0xffffffffL to this. */
  /*
  public void setPlusUnsigned (int y)
  {
    setPlusUnsigned (this, y);
  }
  */

  /** Destructively set this to the sum of x and y&0xffffffffL.
   * It is OK for x==this.*/
  /*
  public void setPlusUnsigned (IntNum x, int y)
  {
    long carry = (long) y & 0xffffffffL;
    if (x.words == null)
      {
	set ((long) x.ival + carry);
	return;
      }
    int i;
    boolean neg = x.isNegative ();
    realloc (x.ival + 1);
    for (i = 0;  i < x.ival;  i++)
      {
	if (carry == 0)
	  {
	    for (; i < x.ival; i++)
	      words[i] = x.words[i];
	    ival = i;
	    return;
	  }
	carry = ((long) x.words[i] & 0xffffffffL) + carry;
	words[i] = (int) carry;
	carry >>= 32;
      }
    if (neg)
      carry--;
    words[i] = (int) carry;
    ival = IntNum.wordsNeeded (words, i);
  }
  */

  /** Destructively set this to the difference of x and y&0xffffffffL.
   * It is OK for x==this.*/
  public void setMinusUnsigned (IntNum x, int y)
  {
    long carry = (long) y & 0xffffffffL;
    if (x.words == null)
      {
	set ((long) x.ival - carry);
	return;
      }
    throw new Error ("not implemented - setMinusUnsigned for bignum");
    /*
    ???;
    int i;
    boolean neg = x.isNegative ();
    realloc (x.ival + 1);
    for (i = 0;  i < x.ival;  i++)
      {
	carry = ((long) x.words[i] & 0xffffffffL) + carry;
	words[i] = (int) carry;
	carry >>= 32;
      }
    if (neg)
      carry--;
    words[i] = (int) carry;
    ival = wordsNeeded (words, i);
    */
  }

  /** Subtract two IntNums, yielding their difference as another IntNum. */
  public static IntNum minus (IntNum x, IntNum y)
  {
    if (x.words == null && y.words == null)
      return IntNum.make ((long) x.ival - (long) y.ival);
    // Inefficient.  FIXME.
    return plus (x, IntNum.neg (y));
  }

  /** Add two IntNums, yielding their sum as another IntNum. */
  public static IntNum plus (IntNum x, IntNum y)
  {
    if (x.words == null)
      {
	if (y.words == null)
	  return IntNum.plus (x.ival, y.ival);
	else
	  return IntNum.plus (y, x.ival);
      }
    if (y.words == null)
      return IntNum.plus (x, y.ival);
    // Both are big
    int len;
    if (y.ival > x.ival)
      { // Swap so x is longer then y.
	IntNum tmp = x;  x = y;  y = tmp;
      }
    IntNum result = alloc (x.ival + 1);
    int i = y.ival;
    long carry = MPN.add_n (result.words, x.words, y.words, i);
    long y_ext = y.words[i-1] < 0 ? 0xffffffffL : 0;
    for (; i < x.ival;  i++)
      {
	carry += ((long) x.words[i] & 0xffffffffL) + y_ext;;
	result.words[i] = (int) carry;
	carry >>>= 32;
      }
    if (x.words[i - 1] < 0)
      y_ext--;
    result.words[i] = (int) (carry + y_ext);
    result.ival = i+1;
    return result.canonicalize ();
  }

  /** Multiply two ints, yielding an IntNum. */
  public static final IntNum times (int x, int y)
  {
    return IntNum.make ((long) x * (long) y);
  }

  public static final IntNum times (IntNum x, IntNum y)
  {
    if (x.words == null && y.words == null)
      return times (x.ival, y.ival);
    boolean negative = false;
    if (x.isNegative ())
      {
	negative = true;
	x = IntNum.neg (x);
      }
    if (y.isNegative ())
      {
	negative = !negative;
	y = IntNum.neg (y);
      }
    int[] xwords = x.words;
    int[] ywords = y.words;
    int xlen = x.ival;
    int ylen = y.ival;
    // Swap if x is shorter then y.
    if (xwords == null || (ywords != null && xlen < ylen))
      {
	xwords = ywords;  ywords = x.words;
	xlen = ylen;  ylen = x.ival;
      }
    IntNum result;
    if (ywords == null)
      {
	result = IntNum.alloc (xlen+1);
	result.words[xlen] = MPN.mul_1 (result.words, xwords, xlen, ylen);
	ylen = 1;
      }
    else
      {
	result = IntNum.alloc (xlen+ylen);
	MPN.mul (result.words, xwords, xlen, ywords, ylen);
      }
    result.ival = xlen+ylen;
    if (negative)
      result.setNegative ();
    return result.canonicalize ();
  }

  public static void divide (long x, long y,
			     IntNum quotient, IntNum remainder,
			     int rounding_mode)
  {
    boolean xNegative, yNegative;
    if (x < 0)
      {
	xNegative = true;
	if (x == Long.MIN_VALUE)
	  {
	    divide (IntNum.make (x), IntNum.make (y),
		    quotient, remainder, rounding_mode);
	    return;
	  }
	x = -x;
      }
    else
      xNegative = false;

    if (y < 0)
      {
	yNegative = true;
	if (y == Long.MIN_VALUE)
	  {
	    if (rounding_mode == TRUNCATE)
	      { // x != Long.Min_VALUE implies abs(x) < abs(y)
		if (quotient != null)
		  quotient.set (0);
		if (remainder != null)
		  remainder.set (x);
	      }
	    else
	      divide (IntNum.make (x), IntNum.make (y),
		      quotient, remainder, rounding_mode);
	    return;
	  }
	y = -y;
      }
    else
      yNegative = false;

    long q = x / y;
    long r = x % y;
    boolean qNegative = xNegative ^ yNegative;

    boolean add_one = false;
    if (r != 0)
      {
	switch (rounding_mode)
	  {
	  case TRUNCATE:
	    break;
	  case CEILING:
	  case FLOOR:
	    if (qNegative == (rounding_mode == FLOOR))
	      add_one = true;
	    break;
	  case ROUND:
	    add_one = r > ((y - (q & 1)) >> 1);
	    break;
	  }
      }
    if (quotient != null)
      {
	if (add_one)
	  q++;
	if (qNegative)
	  q = -q;
	quotient.set (q);
      }
    if (remainder != null)
      {
	// The remainder is by definition: X-Q*Y
	if (add_one)
	  {
	    // Subtract the remainder from Y.
	    r = y - r;
	    // In this case, abs(Q*Y) > abs(X).
	    // So sign(remainder) = -sign(X).
	    xNegative = ! xNegative;
	  }
	else
	  {
	    // If !add_one, then: abs(Q*Y) <= abs(X).
	    // So sign(remainder) = sign(X).
	  }
	if (xNegative)
	  r = -r;
	remainder.set (r);
      }
  }

  /** Divide two integers, yielding quotient and remainder.
   * @param x the numerator in the division
   * @param t the denominator in the division
   * @param quotient is set to the quotient of the result (iff quotient!=null)
   * @param remainder is set to the remainder of the result
   *  (iff remainder!=null)
   * @param rounding_mode one of FLOOR, CEILING, TRUNCATE, or ROUND.
   */
  public static void divide (IntNum x, IntNum y,
			     IntNum quotient, IntNum remainder,
			     int rounding_mode)
  {
    if ((x.words == null || x.ival <= 2)
	&& (y.words == null || y.ival <= 2))
      {
	long x_l = x.longValue ();
	long y_l = y.longValue ();
	if (x_l != Long.MIN_VALUE && y_l != Long.MIN_VALUE)
	  {
	    divide (x_l, y_l, quotient, remainder, rounding_mode);
	    return;
	  }
      }

    boolean xNegative = x.isNegative ();
    boolean yNegative = y.isNegative ();
    boolean qNegative = xNegative ^ yNegative;

    int ylen = y.words == null ? 1 : y.ival;
    int[] ywords = new int[ylen];
    y.getAbsolute (ywords);
    while (ylen > 1 && ywords[ylen-1] == 0)  ylen--;

    int xlen = x.words == null ? 1 : x.ival;
    int[] xwords = new int[xlen+2];
    x.getAbsolute (xwords);
    while (xlen > 1 && xwords[xlen-1] == 0)  xlen--;

    int qlen, rlen;

    int cmpval = MPN.cmp (xwords, xlen, ywords, ylen);
    if (cmpval < 0)  // abs(x) < abs(y)
      { // quotient = 0;  remainder = num.
	int[] rwords = xwords;  xwords = ywords;  ywords = rwords;
	rlen = xlen;  qlen = 1;  xwords[0] = 0;
      }
    else if (cmpval == 0)  // abs(x) == abs(y)
      {
	xwords[0] = 1;  qlen = 1;  // quotient = 1
	ywords[0] = 0;  rlen = 0;  // remainder = 0;
      }
    else if (ylen == 1)
      {
	qlen = xlen;
	rlen = 1;
	ywords[0] = MPN.divmod_1 (xwords, xwords, xlen, ywords[0]);
      }
    else  // abs(x) > abs(y)
      {
	// Normalize the denominator, i.e. make its most significant bit set by
	// shifting it normalization_steps bits to the left.  Also shift the
	// numerator the same number of steps (to keep the quotient the same!).

	int nshift = MPN.count_leading_zeros (ywords[ylen-1]);
	if (nshift != 0)
	  {
	    // Shift up the denominator setting the most significant bit of
	    // the most significant word.
	    MPN.lshift (ywords, 0, ywords, ylen, nshift);

	    // Shift up the numerator, possibly introducing a new most
	    // significant word.
	    int x_high = MPN.lshift (xwords, 0, xwords, xlen, nshift);
	    xwords[xlen++] = x_high;
	}

	if (xlen == ylen)
	  xwords[xlen++] = 0;
	MPN.divide (xwords, xlen, ywords, ylen);
	rlen = ylen;
	if (remainder != null || rounding_mode != TRUNCATE)
	  {
	    if (nshift == 0)
	      System.arraycopy (xwords, 0, ywords, 0, rlen);
	    else
	      MPN.rshift (ywords, xwords, 0, rlen, nshift);
	  }

	qlen = xlen+1-ylen;
	if (quotient != null)
	  {
	    for (int i = 0;  i < qlen;  i++)
	      xwords[i] = xwords[i+ylen];
	  }
      }

    // Now the quotient is in xwords, and the remainder is in ywords.

    boolean add_one = false;
    if (rlen > 0 || ywords[0] != 0)
      { // Non-zero remainder i.e. in-exact quotient.
	switch (rounding_mode)
	  {
	  case TRUNCATE:
	    break;
	  case CEILING:
	  case FLOOR:
	    if (qNegative == (rounding_mode == FLOOR))
	      add_one = true;
	    break;
	  case ROUND:
	    throw new Error ("ROUND mode not implemented for divide");
	  }
      }
    if (quotient != null)
      {
	quotient.set (xwords, qlen);
	if (qNegative)
	  {
	    if (add_one)  // -(quotient + 1) == ~(quotient)
	      quotient.setInvert ();
	    else
	      quotient.setNegative ();
	  }
	else if (add_one)
	  quotient.setPlus (1);
      }
    if (remainder != null)
      {
	// The remainder is by definition: X-Q*Y
	if (add_one)
	  {
	    // Subtract the remainder from Y.

	    // In this case, abs(Q*Y) > abs(X).
	    // So sign(remainder) = -sign(X).
	    xNegative = ! xNegative;	
	    throw new Error ("r_l = y_l - r_l"); // FIXME
	  }
	else
	  {
	    // If !add_one, then: abs(Q*Y) <= abs(X).
	    // So sign(remainder) = sign(X).
	  }
	remainder.set (ywords, rlen);
	if (xNegative)
	  remainder.setNegative ();
      }
  }

  public static IntNum quotient (IntNum x, IntNum y)
  {
    IntNum quotient = new IntNum ();
    divide (x, y, quotient, null, TRUNCATE);
    return quotient.canonicalize ();
  }

  public static IntNum remainder (IntNum x, IntNum y)
  {
    IntNum rem = new IntNum ();
    divide (x, y, null, rem, TRUNCATE);
    return rem.canonicalize ();
  }

  public static IntNum modulo (IntNum x, IntNum y)
  {
    IntNum rem = new IntNum ();
    divide (x, y, null, rem, FLOOR);
    return rem.canonicalize ();
  }

  /** Calculate the integral power of an IntNum.
   * @param x the value (base) to exponentiate
   * @param y the exponent (must be non-negative)
   */
  public static IntNum power (IntNum x, int y)
  {
    if (y <= 0)
      {
	if (y == 0)
	  return one ();
	else
	  throw new Error ("negative exponent");
      }
    if (x.isZero ())
      return x;
    int plen = x.words == null ? 1 : x.ival;  // Length of pow2.
    int blen = ((x.intLength () * y) >> 5) + 2 * plen;
    boolean negative = x.isNegative () && (y & 1) != 0;
    int[] pow2 = new int [blen];
    int[] rwords = new int [blen];
    int[] work = new int [blen];
    x.getAbsolute (pow2);	// pow2 = abs(x);
    int rlen = 1;
    rwords[0] = 1; // rwords = 1;
    for (;;)  // for (i = 0;  ; i++)
      {
	// pow2 == x**(2**i)
	// prod = x**(sum(j=0..i-1, (y>>j)&1))
	if ((y & 1) != 0)
	  { // r *= pow2
	    MPN.mul (work, pow2, plen, rwords, rlen);
	    int[] temp = work;  work = rwords;  rwords = temp;
	    rlen += plen;
	    while (rwords[rlen-1] == 0)  rlen--;
	  }
	y >>= 1;
	if (y == 0)
	  break;
	// pow2 *= pow2;
	MPN.mul (work, pow2, plen, pow2, plen);
	int[] temp = work;  work = pow2;  pow2 = temp;  // swap to avoid a copy
	plen *= 2;
	while (pow2[plen-1] == 0)  plen--;
      }
    if (negative)
      negate (rwords, rwords, rlen);
    return IntNum.make (rwords, rlen);
  }

  private static final long __umulsidi (int x, int y)
  {
    return ((long)x & 0xffffffffL) * ((long)y & 0xffffffffL);
  }

  /** Add an unsigned int to this.
   * @param i add (long)i & 0xFFFFFFF to this.
   */
  /*
  void increment_unsigned (int i)
  {
    if (words != null)
      {
	int top = words[ival - 1];
	long carry = (long) i & 0xffffffffL;
	for (int i = 0;  i < ival;  i++)
	  {
	    long result = ((long)words[i] & 0xffffffffL) + carry;
	    words[i] = (int) result;
	    carry = carry >> 32;
	  }
	...;
      }
    else
      ...;
  }
  */
  /*
  void increment ()
  {
    if (words == null && ival != Integer.MAX_VAL)
      ival++;
    else
      increment_unsigned (1);
  }
  */

  /** Calculate Greatest Common Divisor for non-negative ints. */
  public static final int gcd (int a, int b)
  {
    // Euclid's algorithm, copied from libg++.
    if (b > a)
      {
	int tmp = a; a = b; b = tmp;
      }
    for(;;)
      {
	if (b == 0)
	  return a;
	else if (b == 1)
	  return b;
	else
	  {
	    int tmp = b;
	    b = a % b;
	    a = tmp;
	  }
      }
  }

  public static IntNum gcd (IntNum x, IntNum y)
  {
    int xval = x.ival;
    int yval = y.ival;
    if (x.words == null && y.words == null
	&& xval != Integer.MIN_VALUE && yval != Integer.MIN_VALUE)
      {
	if (xval < 0)
	  xval = -xval;
	if (yval < 0)
	  yval = -yval;
	return IntNum.make (IntNum.gcd (xval, yval));
      }

    int xlen = x.words == null ? 1 : xval;
    int ylen = y.words == null ? 1 : yval;
    int len = (xlen > ylen ? xlen : ylen) + 1;
    int[] xwords = new int[len];
    int[] ywords = new int[len];
    x.getAbsolute (xwords);
    y.getAbsolute (ywords);
    len = MPN.gcd (xwords, ywords, len);
    IntNum result = new IntNum (0);
    result.ival = len;
    result.words = xwords;
    return result.canonicalize ();
  }

  public static IntNum lcm (IntNum x, IntNum y)
  {
    if (x.isZero () || y.isZero ())
      return IntNum.zero ();
    x = IntNum.abs (x); 
    y = IntNum.abs (y);
    IntNum quotient = new IntNum ();
    divide (times (x, y), gcd (x, y), quotient, null, TRUNCATE);
    return quotient.canonicalize ();
  }

  void setInvert ()
  {
    if (words == null)
      ival = ~ival;
    else
      {
	for (int i = ival;  --i >= 0; )
	  words[i] = ~words[i];
      }
  }

  void setShiftLeft (IntNum x, int count)
  {
    int[] xwords;
    int xlen;
    if (x.words == null)
      {
	if (count < 32)
	  {
	    set ((long) x.ival << count);
	    return;
	  }
	xwords = new int[1];
	xwords[0] = x.ival;
	xlen = 1;
      }
    else
      {
	xwords = x.words;
	xlen = x.ival;
      }
    int word_count = count >> 5;
    count &= 31;
    int new_len = xlen + word_count;
    if (count == 0)
      {
	realloc (new_len);
	for (int i = xlen;  --i >= 0; )
	  words[i+word_count] = xwords[i];
      }
    else
      {
	new_len++;
	realloc (new_len);
	int shift_out = MPN.lshift (words, word_count, xwords, xlen, count);
	count = 32 - count;
	words[new_len-1] = (shift_out << count) >> count;  // sign-extend.
      }
    ival = new_len;
    for (int i = word_count;  --i >= 0; )
      words[i] = 0;
  }

  void setShiftRight (IntNum x, int count)
  {
    if (x.words == null)
      set (count < 32 ? x.ival >> count : x.ival < 0 ? -1 : 0);
    else if (count == 0)
      set (x);
    else
      {
	boolean neg = x.isNegative ();
	int word_count = count >> 5;
	count &= 31;
	int d_len = x.ival - word_count;
	if (d_len <= 0)
	  set (neg ? -1 : 0);
	else
	  {
	    if (words == null || words.length < d_len)
	      realloc (d_len);
	    MPN.rshift (words, x.words, word_count, d_len, count);
	    ival = d_len;
	    if (neg)
	      words[ival-1] |= -1 << (32 - count);
	  }
      }
  }

  void setShift (IntNum x, int count)
  {
    if (count > 0)
      setShiftLeft (x, count);
    else
      setShiftRight (x, -count);
  }

  public static IntNum shift (IntNum x, int count)
  {
    if (x.words == null)
      {
	if (count <= 0)
	  return make (count > -32 ? x.ival >> (-count) : x.ival < 0 ? -1 : 0);
	if (count < 32)
	  return make ((long) x.ival << count);
      }
    if (count == 0)
      return x;
    IntNum result = new IntNum (0);
    result.setShift (x, count);
    return result.canonicalize ();
  }

  public String toString (int radix)
  {
    if (words == null)
      return Integer.toString (ival, radix);
    else if (ival <= 2)
      return Long.toString (longValue (), radix);
    boolean neg = isNegative ();
    int[] work;
    if (neg || radix != 16)
      {
	work = new int[ival];
	getAbsolute (work);
      }
    else
      work = words;
    int len = ival;

    int buf_size = len * (MPN.chars_per_word (radix) + 1);
    StringBuffer buffer = new StringBuffer (buf_size);
    if (radix == 16)
      {
	if (neg)
	  buffer.append ('-');
	int buf_start = buffer.length ();
	for (int i = len;  --i >= 0; )
	  {
	    int word = work[i];
	    for (int j = 8;  --j >= 0; )
	      {
		int hex_digit = (word >> (4 * j)) & 0xF;
		// Suppress leading zeros:
		if (hex_digit > 0 || buffer.length () > buf_start)
		  buffer.append (Character.forDigit (hex_digit, 16));
	      }
	  }
      }
    else
      {
	for (;;)
	  {
	    int digit = MPN.divmod_1 (work, work, len, radix);
	    buffer.append (Character.forDigit (digit, radix));
	    while (len > 0 && work[len-1] == 0) len--;
	    if (len == 0)
	      break;
	  }
	if (neg)
	  buffer.append ('-');
	/* Reverse buffer. */
	int i = 0;
	int j = buffer.length () - 1;
	while (i < j)
	  {
	    char tmp = buffer.charAt (i);
	    buffer.setCharAt (i, buffer.charAt (j));
	    buffer.setCharAt (j, tmp);
	    i++;  j--;
	  }
      }
    return buffer.toString ();
  }

  public int intValue ()
  {
    if (words == null)
      return ival;
    // Should we throw an exception?  java.lang.Long doesn't.
    return words[0];
  }

  /** Cast an Object to an int.  The Object must (currently) be an IntNum. */
  public static int intValue (Object obj)
  {
    IntNum inum = (IntNum) obj;
    if (inum.words != null)
      // This is not quite appropriate, but will do.
      throw new ClassCastException ("integer too large");
    return inum.ival;
  }

  public long longValue ()
  {
    if (words == null)
      return ival;
    // Should we throw an exception if too large?
    return ((long)words[1] << 32) + ((long)words[0] & 0xffffffffL);
  }

  public int hashCode ()
  {
    return words == null ? ival : (words[0] + words[ival-1]);
  }

  /* Assumes x and y are both canonicalized. */
  public static boolean equals (IntNum x, IntNum y)
  {
    if (x.words == null && y.words == null)
      return x.ival == y.ival;
    if (x.words == null || y.words == null || x.ival != y.ival)
      return false;
    for (int i = x.ival; --i >= 0; )
      {
	if (x.words[i] != y.words[i])
	  return false;
      }
    return true;
  }

  /* Assumes this and obj are both canonicalized. */
  public boolean equals (Object obj)
  {
    if (obj == null || ! (obj instanceof IntNum))
      return false;
    return IntNum.equals (this, (IntNum) obj);
  }

  public static IntNum valueOf (String s, int radix)
       throws NumberFormatException
  {
    int chars_per_word = MPN.chars_per_word (radix);
    if (s.length () <= chars_per_word)
      return IntNum.make (Long.parseLong (s, radix));
    int len = s.length ();
    IntNum result = IntNum.alloc ((len + 1) / chars_per_word + 1);
    
    int byte_len = 0;
    byte[] bytes = new byte[len];
    boolean negative = false;
    for (int i = 0;  i < len;  i++)
      {
	char ch = s.charAt (i);
	if (ch == '-')
	  negative = true;
	else if (ch == '_' || (byte_len == 0 && (ch == ' ' || ch == '\t')))
	  continue;
	else
	  {
	    int digit = Character.digit (ch, radix);
	    if (digit < 0)
	      break;
	    bytes[byte_len++] = (byte) digit;
	  }
      }
    result.ival = MPN.set_str (result.words, bytes, byte_len, radix);
    if (result.ival == 0 || result.words[result.ival-1] < 0)
      result.words[result.ival++] = 0;
    if (negative)
      result.setNegative ();
    return result.canonicalize ();
  }

  public static IntNum valueOf (String s)
       throws NumberFormatException
  {
    return IntNum.valueOf (s, 10);
  }

  static ClassType thisType;
  static Method makeIntMethod;
  static Method makeLongMethod;

  public Literal makeLiteral (Compilation comp)
  {
    if (thisType == null)
      {
	thisType = new ClassType ("kawa.math.IntNum");
	Type[] args = new Type[1];
	args[0] = Type.int_type;
	makeIntMethod = thisType.new_method ("make", args, thisType,
					     Access.PUBLIC|Access.STATIC);
	args = new Type[1];
	args[0] = Type.int_type;
	makeLongMethod = thisType.new_method ("make", args, thisType,
					     Access.PUBLIC|Access.STATIC);
      }
    return new Literal (this, thisType, comp);
  }

  public void emit (Literal literal, Compilation comp)
  {
    if (words == null)
      {
	comp.method.compile_push_int (ival);
	comp.method.compile_invoke_static (makeIntMethod);
      }
    else if (ival <= 2)
      {
	comp.method.compile_push_long (longValue ());
	comp.method.compile_invoke_static (makeLongMethod);
      }
    else
      throw new Error ("IntNum.emit for bignum - not implemented");
  }

  public double doubleValue ()
  {
    if (words == null)
      return (double) ival;
    if (ival < 2)
      return (double) longValue ();

    int il = MPN.intLength (words, ival) + 1;
    long l = MPN.rshift_long (words, ival, il - 64);
    boolean neg = l < 0;
    if (neg)
      l = -l;
    // l is now properly viewed as unsigned, since the -l might have
    // overflowed.  For correct rounding, check the bit that we will shift out.
    if ((l & (1l << 10)) != 0)
      l += (1l << 11);
    // Check for overflow into the sign bit.
    // This could be because either the negation or addition overflowed.
    // (However, it is not possible both overflowed.)
    if (l < 0)
      {
	l >>>= 1;
	il += 1;
      }
    // At this point the sign bit is zero, but the next-highest bit is one.
    // That bit becomes the "hidden" bit.
    l = (l >> 10) & 0xfffffffffffffL;
    double d;
    if (il >= 1025)
      d = Double.POSITIVE_INFINITY;
    else
      {
	l += (long)(il + 1021) << 52;
	d = Double.longBitsToDouble (l);
      }
    if (neg)
      d = -d;
    return d;
  }

  public Numeric add (Object y)
  {
    if (y instanceof IntNum)
      return IntNum.plus (this, (IntNum) y);
    if (!(y instanceof Numeric))
      throw new IllegalArgumentException ();
    return ((Numeric)y).add_reversed (this);
  }

  public Numeric sub (Object y)
  {
    if (y instanceof IntNum)
      return IntNum.minus (this, (IntNum) y);
    if (!(y instanceof Numeric))
      throw new IllegalArgumentException ();
    return ((Numeric)y).sub_reversed (this);
  }

  public Numeric mul (Object y)
  {
    if (y instanceof IntNum)
      return IntNum.times (this, (IntNum) y);
    if (!(y instanceof Numeric))
      throw new IllegalArgumentException ();
    return ((Numeric)y).mul_reversed (this);
  }

  public Numeric div (Object y)
  {
    if (y instanceof RatNum)
      {
	RatNum r = (RatNum) y;
	return RatNum.make (IntNum.times (this, r.denominator()),
			    r.numerator());
      }
    if (! (y instanceof Numeric))
      throw new IllegalArgumentException ();
    return ((Numeric)y).div_reversed (this);
  }

  /** Copy the abolute value of this into an array of words.
   * Assumes words.length >= (this.words == null ? 1 : this.ival).
   * Result is zero-extended, but need not be a valid 2's complement number.
   */
    
  public void getAbsolute (int[] words)
  {
    int len;
    if (this.words == null)
      {
	len = 1;
	words[0] = this.ival;
      }
    else
      {
	len = this.ival;
	for (int i = len;  --i >= 0; )
	  words[i] = this.words[i];
      }
    for (int i = words.length;  --i > len; )
      words[i] = 0;
    if (words[len-1] < 0)
      negate (words, words, words.length);
  }

  /** Set dest[0:len-1] to the negation of src[0:len-1]. */
  public static int negate (int[] dest, int[] src, int len)
  {
    long carry = 1;
    for (int i = 0;  i < len;  i++)
      {
        carry += ((long) (~src[i]) & 0xffffffffL);
        dest[i] = (int) carry;
        carry >>= 32;
      }
    return (int) carry;
  }

  /** Destructively set this to the negative of x.
   * It is OK if x==this.*/
  public void setNegative (IntNum x)
  {
    int len = x.ival;
    if (x.words == null)
      {
	if (len == Integer.MIN_VALUE)
	  set (- (long) len);
	else
	  set (-len);
	return;
      }
    realloc (len + 1);
    boolean negative = x.isNegative ();
    int carry = IntNum.negate (words, x.words, len);
    if (carry > 0 && negative)
      words[len++] = 1;
    ival = len;
  }

  /** Destructively negate this. */
  public final void setNegative ()
  {
    setNegative (this);
  }

  public static IntNum abs (IntNum x)
  {
    return x.isNegative () ? neg (x) : x;
  }

  public static IntNum neg (IntNum x)
  {
    if (x.words == null && x.ival != Integer.MIN_VALUE)
      return make (- x.ival);
    IntNum result = new IntNum (0);
    result.setNegative (x);
    return result.canonicalize ();
  }

  public Numeric neg ()
  {
    return IntNum.neg (this);
  }

  /** Calculates ceiling(log2(this < 0 ? -this : this+1))
   * See Common Lisp: the Language, 2nd ed, p. 361.
   */
  public int intLength ()
  {
    if (words == null)
      return MPN.intLength (ival);
    else
      return MPN.intLength (words, ival);
  }
}
