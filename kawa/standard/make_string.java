package kawa.standard;
import kawa.lang.*;

public class make_string extends Procedure1or2 {
  public make_string()
  {
    super("make-string");
  }

  public final Object apply1 (Object arg1)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return apply2 (arg1, Char.make (0));
  }

  public final Object apply2 (Object arg1,Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (! (arg1 instanceof java.lang.Integer))
      throw new WrongType(this.name,1,"integer");
    int count = ((java.lang.Integer)arg1).intValue();
    char ch;
    if (arg2 instanceof Char)
      ch = ((Char)arg2).charValue();
    else
      throw new WrongArguments(this.name,2,"character");
    StringBuffer str = new StringBuffer();
    for (int i = count;  --i >= 0; )
      str.append (ch);
    return str;
  }
}
