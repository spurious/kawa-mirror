package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.Printable;
import java.io.PrintWriter;

/** A class of special one-of-a-kind builtin values. */

public class Special extends Object implements Printable, Compilable
{
  private String name;

  public static Special optional = new Special("#!optional");
  public static Special rest = new Special("#!rest");
  public static Special key = new Special("#!key");
  public static Special eof = new Special("#!eof");
  public static Special dfault = new Special("#!default");
  // Also:
  // #!void is the same as Values.Empty.
  // #!null is Java null.

  // Note:  No public constructor!
  private Special (String n)
  {
    name = new String(n);
  }

  public int hashCode () { return name.hashCode (); }

  public final String toString()
  {
    return name;
  }

  public void print(java.io.PrintWriter ps)
  {
    Symbol.print(name, ps);
  }

  static public ClassType thisType;
  static Method makeMethod;

  public Literal makeLiteral (Compilation comp)
  {
    if (thisType == null)
      {
	thisType = ClassType.make("gnu.expr.Special");
	Type[] apply1args = new Type[1];
	apply1args[0] = comp.javaStringType;
	makeMethod =
	  thisType.addMethod ("make", apply1args,
			       thisType, Access.PUBLIC|Access.STATIC);
      }
    if (this == optional)
      return new Literal(this, thisType.addField("optional", thisType,
						  Access.PUBLIC|Access.STATIC),
			 comp);
    if (this == rest)
      return new Literal(this, thisType.addField("rest", thisType,
						  Access.PUBLIC|Access.STATIC),
			 comp);
    if (this == key)
      return new Literal(this, thisType.addField("key", thisType,
						  Access.PUBLIC|Access.STATIC),
			 comp);
    if (this == eof)
      return new Literal(this, thisType.addField("eof", thisType,
						  Access.PUBLIC|Access.STATIC),
			 comp);
    if (this == dfault)
      return new Literal(this, thisType.addField("dfault", thisType,
						  Access.PUBLIC|Access.STATIC),
			 comp);
    return new Literal (this, thisType, comp);
  }

  public void emit (Literal literal, Compilation comp)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    code.emitPushString(((Special) literal.value).name);
    code.emitInvokeStatic(makeMethod);
  }
}
