package kawa.standard;
import gnu.mapping.Procedure1;
import gnu.mapping.HasSetter;
import gnu.mapping.WrongType;
import gnu.lists.*;

/** Implement the standard Scheme procedures "c[ad]*r". */

public class cxr extends Procedure1 implements HasSetter
{
  public static final cxr caar = new cxr("caar");
  public static final cxr cadr = new cxr("cadr");
  public static final cxr cdar = new cxr("cdar");
  public static final cxr cddr = new cxr("cddr");
  public static final cxr caaar = new cxr("caaar");
  public static final cxr caadr = new cxr("caadr");
  public static final cxr cadar = new cxr("cadar");
  public static final cxr caddr = new cxr("caddr");
  public static final cxr cdaar = new cxr("cdaar");
  public static final cxr cdadr = new cxr("cdadr");
  public static final cxr cddar = new cxr("cddar");
  public static final cxr cdddr = new cxr("cdddr");
  public static final cxr caaaar = new cxr("caaaar");
  public static final cxr caaadr = new cxr("caaadr");
  public static final cxr caadar = new cxr("caadar");
  public static final cxr caaddr = new cxr("caaddr");
  public static final cxr cadaar = new cxr("cadaar");
  public static final cxr cadadr = new cxr("cadadr");
  public static final cxr caddar = new cxr("caddar");
  public static final cxr cadddr = new cxr("cadddr");
  public static final cxr cdaaar = new cxr("cdaaar");
  public static final cxr cdaadr = new cxr("cdaadr");
  public static final cxr cdadar = new cxr("cdadar");
  public static final cxr cdaddr = new cxr("cdaddr");
  public static final cxr cddaar = new cxr("cddaar");
  public static final cxr cddadr = new cxr("cddadr");
  public static final cxr cdddar = new cxr("cdddar");
  public static final cxr cddddr = new cxr("cddddr");

  /** An encoding of the car/cdr operations to be done.
      The count of car or cdr operations to apply is the high-order short.
      Whether an operation should be a car or a cdr:
      The i'th operation is a cdr iff (mask >> i) & 1 is true. */
  int mask;

  public cxr (String name)
  {
    super(name);
    program(name);
  }

  /** Set count and mask from a function name of the form "c[ad]*r". */
  private void program (String name)
  {
    int c = 0;
    int m = 0;
    int len = name.length ();
    for (int i = 0;  i < len;  i++)
      {
	char ch = name.charAt (i);
	if (ch == 'a' || ch == 'A')
	  {
	    m <<= 1;
	    c++;
	  }
	else if (ch == 'd' || ch == 'D')
	  {
	    m <<= 1;
	    m |= 1;
	    c++;
	  }
      }
    // Set the mask as an atomic operation.
    mask = (c << 16) | m;
  }

  public Object apply1 (Object arg1)
  {
    int m = mask;
    for (int i = (m >> 16);  --i >= 0;  m >>= 1)
      {
	if (! (arg1 instanceof Pair) )
	  throw new WrongType(this, 1, arg1, "list");
	Pair pair = (Pair) arg1;
	arg1 = (m & 1) != 0 ? pair.cdr : pair.car;
      }
    return arg1;
  }

  public void set1 (Object list, Object value)
  {
    int m = mask;
    Pair pair;
    for (int i = (m >> 16);  --i > 0;  m >>= 1)
      {
	if (! (list instanceof Pair) )
	  throw new WrongType(this, 1, list, "list");
	pair = (Pair) list;
	list = (m & 1) != 0 ? pair.cdr : pair.car;
      }
    pair = (Pair) list;
    if ((m & 1) != 0)
      pair.cdr = value;
    else
      pair.car = value;
  }
}
