package kawa.lang;

import java.io.PrintStream;

public class Pair extends List implements Printable, Compilable
{
   public Object car;
   public Object cdr;

  public Pair (Object carval, Object cdrval)
  {
    super ();
    car = carval;
    cdr = cdrval;
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print("(");
    printNoParen(this, ps);
    ps.print(")");
  }

  static public boolean equals (Pair pair1, Pair pair2)
  {
    while (pair1.car.equals (pair2.car))
      {
	if (! (pair1.cdr instanceof Pair) || !(pair2.cdr instanceof Pair))
	  return pair1.cdr.equals (pair2.cdr);
	pair1 = (Pair) pair1.cdr;
	pair2 = (Pair) pair2.cdr;
      
      }
    return false;
  }

  public Object elementAt (int index)
  {
    Pair pair = this;
    int i = index;
    while (i > 0)
      {
	i--;
	if (pair.cdr instanceof Pair)
	  pair = (Pair)pair.cdr;
	else if (pair.cdr instanceof Sequence)
	  return ((Sequence)pair.cdr).elementAt (i);
	else
	  break;
      }
    if (i == 0)
      return pair.car;
    else
      throw new IndexOutOfBoundsException ();
  }

  // A generalization of List.list_length
  public int length ()
  {
    // Based on list-length implementation in
    // Guy L Steele jr: "Common Lisp:  The Language", 2nd edition, page 414
    int n = 0;
    Object slow = this;
    Object fast = this;
    for (;;)
      {
	if (fast == Empty)
	  return n;
	if (! (fast instanceof Pair))
	  {
	    if (fast instanceof Sequence)
	      {
		int j = ((Sequence) fast).length ();
		return j >= 0 ? n + j : j;
	      }
	    return -2;
	  }
	Pair fast_pair = (Pair) fast;
	if (fast_pair.cdr == Empty)
	  return n+1;
	if (fast == slow && n > 0)
	  return -1;
	if (! (fast_pair.cdr instanceof Pair))
	  {
	    n++;
	    fast = fast_pair.cdr;
	    continue;
	  }
	if (!(slow instanceof Pair))
	  return -2;
	slow = ((Pair)slow).cdr;
	fast = ((Pair)fast_pair.cdr).cdr;
	n += 2;
      }
  }

  public boolean equals (Object obj)
  {
    if ((obj != null) && (obj instanceof Pair))
      return equals (this, (Pair) obj);
    else
      return false;
  }

  static public final void printNoParen (Pair p, java.io.PrintStream ps)
  {
    for (;;)
      {
	kawa.lang.print.print (p.car, ps);
	Object cdr = p.cdr;
	if (cdr == null || cdr == List.Empty)
	  break;
	if (cdr instanceof Pair)
	  {
	    ps.print(" ");
	    p = (Pair)cdr;
	  }
	else
	  {
	    ps.print(" . ");
	    kawa.lang.print.print (cdr, ps);
	    break;
	  }
      }
  }

  public Literal makeLiteral (Compilation comp)
  {
    Literal literal = new Literal (this, comp.scmPairType, comp);
    comp.findLiteral (car);
    comp.findLiteral (cdr);
    return literal;
  }

  public void emit (Literal literal, Compilation comp)
  {
    if ((literal.flags & Literal.ALLOCATING) != 0)
      {
	// We have detected a circularity.
	// Resolve it by only allocating the Pair, leaving the car and cdr
	// as null.  They will be set later by one of our callers.
	// Emit:  push makePair()  (same as new Pair (null, null)
	comp.method.compile_invoke_static (Compilation.makeNullPairMethod);
	literal.flags |= Literal.ALLOCATED;
      }
    else
      {
	literal.flags |= Literal.ALLOCATING;
	comp.emitLiteral (car);
	comp.emitLiteral (cdr);
	if ((literal.flags & Literal.ALLOCATED) != 0)
	  {
	    // It's already been allocated, because either the car or cdr
	    // depended on the value of the Literal (i.e a circularity).
	    // Just initialize car and cdr.
	    // Emit:  this.cdr = pop();  this.car = pop();  push this;
	    comp.method.compile_getstatic (literal.field);
	    comp.method.compile_dup (1, 1);  // emit dup_x1
	    comp.method.compile_swap ();
	    comp.method.compile_putfield (Compilation.cdrField);
	    comp.method.compile_dup (1, 1);  // emit dup_x1
	    comp.method.compile_swap ();
	    comp.method.compile_putfield (Compilation.carField);
	  }
	else
	  {
	    // The normal case - no circularities detected.
	    // emit:  push new Pair (pop(), pop())
	    comp.method.compile_invoke_static  (Compilation.makePairMethod);
	  }
	literal.flags |= Literal.ALLOCATED|Literal.INITIALIZED;
      }
  }

  // Convenience function used by emit.
  public static Pair makePair ()
  {
    return new Pair (null, null);
  }

  // Convenience function used by emit.
  public static Pair makePair (Object car, Object cdr)
  {
    return new Pair (car, cdr);
  }
};
