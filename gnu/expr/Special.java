package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.Printable;
import java.io.*;

/** A class of special one-of-a-kind builtin values. */

public class Special extends Object implements Printable, Compilable, Externalizable
{
  private String name;

  public static Special optional = new Special("optional");
  public static Special rest = new Special("rest");
  public static Special key = new Special("key");
  public static Special eof = new Special("eof");
  public static Special dfault = new Special("default");
  // Also:
  // #!void is the same as Values.Empty.
  // #!null is Java null.

  public Special ()
  {
  }

  private Special (String n)
  {
    name = new String(n);
  }

  public static Special make (String name)
  {
    if (name == "optional") return optional;
    if (name == "rest") return rest;
    if (name == "key") return key;
    if (name == "eof") return eof;
    if (name == "default") return dfault;
    return new Special(name);
  }

  public int hashCode () { return name.hashCode (); }

  public final String toString()
  {
    return "#!" + name;
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print(name);
  }

  /**
   * @serialData Write the keword name (without colons) using writeUTF.
   */

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeUTF(name);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    name = in.readUTF();
  }

  public Special readResolve() throws ObjectStreamException
  {
    return make(name);
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
