package kawa.standard;
import kawa.lang.*;
import gnu.mapping.WrongType;
import gnu.mapping.Procedure2;

public class mem extends Procedure2
{
  protected Procedure2 compare;
  protected java.lang.String usage;
  public mem (String name, Procedure2 comp)
  {
    super(name);
    compare = comp;
    usage = new java.lang.String("("+name+" obj list)");
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    while (arg2 instanceof Pair)
      {
	Pair list = (Pair)arg2;

	Boolean check = (Boolean) compare.apply2 (list.car, arg1);

	if (check.booleanValue())
	  return list;

	arg2 = list.cdr;
      }

    if (arg2 == List.Empty)
      return Boolean.FALSE;
    else
      throw new WrongType(this.name (), 2, "list");
  }
}
