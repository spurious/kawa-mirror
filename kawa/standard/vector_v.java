package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.Field;
import gnu.bytecode.Access;
import gnu.bytecode.ClassType;
import java.lang.Error;  // To work around case-fold bug in some compilers.

public class vector_v extends ProcedureN implements Compilable
{
  public static vector_v vectorProcedure = new vector_v ();

  public vector_v()
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
	ClassType thisType = new ClassType ("kawa.standard.vector_v");
	vectorConstant = thisType.addField("vectorProcedure", thisType,
					   Access.PUBLIC|Access.STATIC);
      }
    return new Literal (this, vectorConstant, comp);
  }

  public void emit (Literal literal, Compilation comp)
  {
    throw new Error ("internal error - vector.emit called");
  }
}
