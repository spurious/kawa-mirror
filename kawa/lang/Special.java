package kawa.lang;
import codegen.*;
import java.io.PrintStream;

/** A class of special one-of-a-kind builtin values. */

public class Special extends Object implements Printable, Compilable
{
  private String name;

  public static Special optional = new Special("#!optional");
  public static Special rest = new Special("#!rest");
  public static Special key = new Special("#!key");
  public static Special eof = new Special("#!eof");
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

  public void print(java.io.PrintStream ps)
  {
    Symbol.print(name, ps);
  }

  static public ClassType thisType;
  static Method makeMethod;

  public Literal makeLiteral (Compilation comp)
  {
    if (thisType == null)
      {
	thisType = new ClassType ("kawa.lang.Special");
	Type[] apply1args = new Type[1];
	apply1args[0] = comp.javaStringType;
	makeMethod =
	  thisType.new_method ("make", apply1args,
			       thisType, Access.PUBLIC|Access.STATIC);
      }
    if (this == optional)
      return new Literal(this, thisType.new_field("optional", thisType,
						  Access.PUBLIC|Access.STATIC),
			 comp);
    if (this == rest)
      return new Literal(this, thisType.new_field("rest", thisType,
						  Access.PUBLIC|Access.STATIC),
			 comp);
    if (this == key)
      return new Literal(this, thisType.new_field("key", thisType,
						  Access.PUBLIC|Access.STATIC),
			 comp);
    if (this == eof)
      return new Literal(this, thisType.new_field("eof", thisType,
						  Access.PUBLIC|Access.STATIC),
			 comp);
    return new Literal (this, thisType, comp);
  }

  public void emit (Literal literal, Compilation comp)
  {
    comp.method.compile_push_string (((Special)literal.value).name);
    comp.method.compile_invoke_static (makeMethod);
  }
}
