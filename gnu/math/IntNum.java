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

  /** Change words.length to nwords. */
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
    else if (words == null || words.length != nwords)
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
    else if (obj instanceof RealNum)
      return DFloNum.compare (doubleValue (), ((RealNum)obj).doubleValue ());
    throw new IllegalArgumentException ();
  }

  public boolean isExact ()
  {
    return true;
  }

  public boolean isOdd ()
  {
    int high = words == null ? ival : words[ival-1];
    return (high & 1) != 0;
  }

  public boolean isZero ()
  {
    return words == null && ival == 0;
  }

  /** Calculate how many words are significant in words[0:len-1].
   * Returns the least value x such that x>0 && words[0:x-1]==words[0:len-1],
   * when words is views as a 2's complement integer.
   */
  public static int wordsNeeded (int[] words, int len)
  {
    int i = len;
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
	while (word == 0 && i > 0 && (word = words[i-1]) >= 0)
	  {
	    i--;
	    System.err.println("word:"+word+",i:"+i);
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

  /** Destructively set the value of this to an int. */
  public final void set (int y)
  {
    words = null;
    ival = y;
  }

  /** Destructively set the value of this to a long. */
  public final void set (long y)
  {
    ival = (int) y;
    if ((long) ival == y)
      words = null;
    else
      {
	realloc (2);
	words[0] = ival;
	words[1] = (int) (y >> 32);
	ival = 2;
      }
  }

  /** Destructively add an int to this. */
  /*
  public void setPlus (int y)
  {
    if (y >= 0)
      setPlusUnsigned (this, y);
    else
      setMinusUnsigned (this, -y);
  }
  */

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
    /*
    long carry = 0;
    for (i = 0; i < y.ival;  i++)
      {
	carry = ((long) x.words[i] & (long) 0xffffffffL) +
	  ((long) y.words[i] & (long) 0xffffffffL) + carry;
	result.words[i] = (int) carry;
	carry >>= 32;
      }
      */
    for (; i < x.ival;  i++)
      {
	carry += (long) x.words[i] & (long) 0xffffffffL;
	result.words[i] = (int) carry;
	carry >>= 32;
      }
    if (x.words[i - 1] < 0)
      carry--;
    result.words[i] = (int) carry;
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
      result.setNeg (result);
    return result.canonicalize ();
  }

  /*
  public static IntNum power (IntNum x, int y)
  {
  }
  */

  /*
  public void setNegative ()
  {
    ...;
  }
  */

  /*
  public void setTimes (IntNum x, int y)
  {
    if (x.words == null)
      {
	set ((long) x.val * (long) y);
	return;
      }
    ...;
  }
  */

  /** Destructively multiply this by an int. */
  /*
  public void setTimes (int y)
  {
    if (words == null)
      {
	set ((long) ival * (long) y);
      }
    else
      {
	boolean neg = isNegative ();
	...;
      }
  }
  */

  private static final long __umulsidi (int x, int y)
  {
    return ((long)x & 0xffffffffL) * ((long)y & 0xffffffffL);
  }

  /*
  public static mul_1u (IntNum result, IntNum x, int y)
  {
  }
  */

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
	  xval = -xval;
	return IntNum.make (IntNum.gcd (xval, yval));
      }
    throw new Error ("unimplemented bignum gcd");
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

  public String toString (int radix)
  {
    if (words == null)
      return Integer.toString (ival, radix);
    else if (ival <= 2)
      return Long.toString (longValue (), radix);
    else if (isNegative ())
      {
	return "-" + (IntNum.neg (this).toString (radix));
      }
    else
      {
	int buf_size = ival * (MPN.chars_per_word (radix) + 1);
	StringBuffer buffer = new StringBuffer (buf_size);
	if (radix == 16)
	  {
	    for (int i = ival;  --i >= 0; )
	      {
		int word = words[i];
		for (int j = 8;  --j >= 0; )
		  {
		    int hex_digit = (word >> (4 * j)) & 0xF;
		    // Suppress leading zeros:
		    if (hex_digit > 0 || buffer.length () > 0)
		      buffer.append (Character.forDigit (hex_digit, 16));
		      
		  }
	      }
	  }
	else
	  {
	    // temporary - until we can implement other radixes.
	    return "#x"+toString(16);
	  }
	return buffer.toString ();
      }
  }

  public String toString ()
  {
    return toString (10);
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
    if (negative)
      result.setNeg (result);
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
      System.err.println ("IntNum.emit for bignum - not implemented");
  }

  public double doubleValue ()
  {
    if (words == null)
      return (double) ival;
    if (ival < 2)
      return (double) longValue ();
    throw new Error ("not implemented - IntNum.doubleValue for bignum");
  }

  public Numeric add (Object y)
  {
    if (y instanceof IntNum)
      return IntNum.plus (this, (IntNum) y);
    else if (y instanceof RealNum)
      return ((RealNum)y).add (this);
    else
      throw new IllegalArgumentException ();
  }

  public Numeric sub (Object y)
  {
    if (y instanceof IntNum)
      return IntNum.minus (this, (IntNum) y);
    else if (y instanceof RealNum)
      return new DFloNum (doubleValue () - ((RealNum)y).doubleValue ());
    else
      throw new IllegalArgumentException ();
  }

  public Numeric mul (Object y)
  {
    if (y instanceof IntNum)
      return IntNum.times (this, (IntNum) y);
    else if (y instanceof RealNum)
      return ((RealNum)y).mul (this);
    else
      throw new IllegalArgumentException ();
  }

  public Numeric div (Object y)
  {
    if (y instanceof RealNum)
      return new DFloNum (doubleValue () / ((RealNum)y).doubleValue ());
    else
      throw new IllegalArgumentException ();
  }

  /** Destructively set this to the negative of x.
   * It is OK if x==this.*/
  public void setNeg (IntNum x)
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
    long carry = 1;
    boolean negative = x.isNegative ();
    for (int i = 0;  i < len;  i++)
      {
	carry += ((long) (~x.words[i]) & 0xffffffffL);
	words[i] = (int) carry;
	carry >>= 32;
      }
    if (carry > 0 && negative)
      words[len++] = 1;
    ival = len;
  }

  public static IntNum neg (IntNum x)
  {
    if (x.words == null && x.ival != Integer.MIN_VALUE)
      return make (- x.ival);
    IntNum result = new IntNum (0);
    result.setNeg (x);
    return result.canonicalize ();
  }

  public Numeric neg ()
  {
    return IntNum.neg (this);
  }
}
