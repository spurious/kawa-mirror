package kawa.lang;

import java.io.PrintStream;

public class Pair extends List implements Printable
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
};
