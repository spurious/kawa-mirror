package kawa.standard;
import kawa.lang.*;
import codegen.Field;
import codegen.Access;
import codegen.ClassType;

public class cons extends Procedure2 implements Compilable
{
  public static cons consProcedure = new cons ();

  public cons()
  {
    super("cons");
  }

  public Object apply2 (Object arg1, Object arg2) 
  {
    return new Pair (arg1, arg2);
  }

  static Field consConstant;

  public Literal makeLiteral (Compilation comp)
  {
    if (consConstant == null)
      {
	ClassType thisType = new ClassType ("kawa.standard.cons");
	consConstant = thisType.new_field ("consProcedure", thisType,
					   Access.PUBLIC|Access.STATIC);
      }
    return new Literal (this, consConstant, comp);
  }

  public void emit (Literal literal, Compilation comp)
  {
    throw new Error ("internal error - cons.emit called");
  }
}
