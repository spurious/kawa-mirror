package kawa.lang;
import gnu.bytecode.Method;
import gnu.bytecode.ClassType;
import gnu.bytecode.Access;
import gnu.bytecode.Type;
import gnu.mapping.*;
import gnu.expr.*;
import java.io.*;

/**
 * A pattern that requires an exact match (using equal?).
 */

public class EqualPat extends Pattern implements Printable, Compilable, Externalizable
{

  Object value;

  public EqualPat () { }

  public EqualPat (Object obj) { value = obj; }

  static public EqualPat make (Object obj) { return new EqualPat (obj); }

  public boolean match (Object obj, Object[] vars, int start_vars) {
    return value.equals (obj);
  }

  public int varCount () { return 0; }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<equals: ");
    SFormat.print (value, ps);
    ps.print ('>');
  }

  /**
   * @serialData Write the value (using writeObject).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(value);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    value = in.readObject();
  }

  static public ClassType EqualPatType;
  static Method makeEqualPatMethod;

  public Literal makeLiteral (Compilation comp)
  {
    if (EqualPatType == null)
      {
	EqualPatType = ClassType.make("kawa.lang.EqualPat",
                                      Pattern.typePattern);
	makeEqualPatMethod =
	  EqualPatType.addMethod ("make", comp.apply1args,
				   EqualPatType, Access.PUBLIC|Access.STATIC);
      }
    Literal literal = new Literal (this, EqualPatType, comp);
    comp.findLiteral (value);
    return literal;
  }

  public void emit (Literal literal, Compilation comp)
  {
    literal.check_cycle ();
    comp.emitLiteral (value);
    comp.getCode().emitInvokeStatic(makeEqualPatMethod);
  }
}
