package kawa.standard;
import kawa.lang.*;

public class string extends ProcedureN
{
  public kawa.standard.string()
  {
    super("string");
  }

  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    int count = args.length;
    StringBuffer str = new java.lang.StringBuffer(count);
    for (int i = 0; i < count; i++)
      {
	Object arg = args[i];
	if (arg instanceof Char)
	  str.append (((Char)arg).charValue());
	else
	  throw new WrongType (this.name,count,"character");
      }
    return str;
  }
}
