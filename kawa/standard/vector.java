package kawa.standard;
import kawa.lang.*;
import codegen.Field;
import codegen.Access;
import codegen.ClassType;

public class vector extends ProcedureN implements Compilable
{
  public static vector vectorProcedure = new vector ();

  public vector()
  {
    super("vector");
  }

  public Object applyN (Object[] args)
  {
    return new Vector (args);
  }

  private static Field vectorConstant;

  public Literal makeLiteral (Compilation comp)
  {
    if (vectorConstant == null)
      {
	ClassType thisType = new ClassType ("kawa.standard.vector");
	vectorConstant = thisType.new_field ("vectorProcedure", thisType,
					     Access.PUBLIC|Access.STATIC);
      }
    return new Literal (this, vectorConstant, comp);
  }

  public void emit (Literal literal, Compilation comp)
  {
    throw new Error ("internal error - vector.emit called");
  }
}
