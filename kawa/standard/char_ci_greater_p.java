package kawa.standard;
import kawa.lang.*;

public class char_ci_greater_p extends Procedure2
{
  public char_ci_greater_p()
  {
    super("char-ci>?");
  }

  public Object apply2(Object arg1, Object arg2)
       throws WrongType
  {
    if (! (arg1 instanceof Char))
      throw new WrongType(this.name(),1,"character");
    if (! (arg2 instanceof Char))
      throw new WrongType(this.name(),2,"character");
    char c1 = ((Char)arg1).charValue ();
    char c2 = ((Char)arg2).charValue ();
    if (Character.toUpperCase (c1) > Character.toUpperCase (c2))
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
