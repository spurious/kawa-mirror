package kawa.lang;
import gnu.bytecode.Method;
import gnu.bytecode.ClassType;
import gnu.bytecode.Access;
import gnu.mapping.*;
import gnu.expr.*;
import java.io.*;

/**
 * A pattern that matches anything.
 */

public class AnyPat extends Pattern implements Printable, Compilable, Externalizable
{
  public AnyPat () { }

  public static AnyPat make () { return new AnyPat (); }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<match any>");
  }

  public boolean match (Object obj, Object[] vars, int start_vars)
  {
    vars[start_vars] = obj;
    return true;
  }

  public int varCount () { return 1; }

  static public ClassType thisType;
  static Method makeAnyPatMethod;

  /**
   * @serialData Write nothing.
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
  }

  public Literal makeLiteral (Compilation comp)
  {
    if (thisType == null)
      {
	thisType = ClassType.make("kawa.lang.AnyPat");
	makeAnyPatMethod =
	  thisType.addMethod ("make", comp.apply0args,
			       thisType, Access.PUBLIC|Access.STATIC);
      }
    return new Literal (this, thisType, comp);
  }

  public void emit (Literal literal, Compilation comp)
  {
    comp.getCode().emitInvokeStatic(makeAnyPatMethod);
  }
}
