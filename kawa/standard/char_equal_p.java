package kawa.standard;
import kawa.lang.*;

public class char_equal_p extends Procedure2
{
  public char_equal_p()
  {
    super("char=?");
  }

  public Object apply2(Object arg1, Object arg2)
       throws WrongType
  {
    if (! (arg1 instanceof Char))
      throw new WrongType(this.name,1,"character");
    if (! (arg2 instanceof Char))
      throw new WrongType(this.name,2,"character");
    if (((Char)arg1).intValue () == ((Char)arg2).intValue())
      return Interpreter.trueObject;
    else
      return Interpreter.falseObject;
  }
}
