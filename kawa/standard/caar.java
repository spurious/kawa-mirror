package kawa.standard;
import kawa.lang.*;

public class caar extends Procedure1
{
  public caar()
  {
    super("caar");
  }

  public Object apply1 (Object arg1)
       throws WrongType, GenericError
  {
    if (arg1 instanceof Pair)
      {
	arg1 = ((Pair)arg1).car;          
	if (arg1 instanceof Pair)
	  return ((Pair)arg1).car;
      }
    throw new kawa.lang.WrongType(this.name,1,"list");
  }
}
