package kawa.standard;
import gnu.mapping.Procedure2;
import gnu.mapping.WrongType;
import gnu.kawa.util.*;

public class ass extends Procedure2
{
  protected Procedure2 compare;
  protected java.lang.String usage;
  public ass (String name, Procedure2 comp)
  {
    super(name);
    compare = comp;
    usage = new String("("+name+" obj list)");
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    while (arg2 instanceof Pair)
      {
	Pair list = (Pair)arg2;

	if (! (list.car instanceof Pair))
            throw new RuntimeException
	      ("The association list contains non-pair elements.");
	Pair pair = (Pair) list.car;

	Boolean check = (Boolean) compare.apply2 (pair.car, arg1);

	if (check.booleanValue())
	  return pair;

	arg2 = list.cdr;
      }

    if (arg2 == LList.Empty)
      return Boolean.FALSE;
    else
      throw new WrongType(this.name (), 2, "list");
  }
}
