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
    //-- TODO: Hack... allocated two times.
    char value[] = new char[count];
    for (int i = 0; i < count; i++)
      {
	Object arg = args[i];
	if (arg instanceof java.lang.Character)
	  value[i] = ((java.lang.Character)arg).charValue();
	else
	  throw new WrongType (this.name,count,"character");
      }
    StringBuffer foo = new java.lang.StringBuffer();
    foo.append(value);
    return foo;
  }
}
