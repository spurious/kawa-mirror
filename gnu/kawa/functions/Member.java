package gnu.kawa.functions;
import gnu.mapping.WrongType;
import gnu.mapping.Procedure2;
import gnu.lists.*;

public class Member extends Procedure2
{
  protected Procedure2 compare;
  public Member (String name, Procedure2 comp)
  {
    super(name);
    compare = comp;
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

    if (arg2 == LList.Empty)
      return Boolean.FALSE;
    else
      throw new WrongType(this.name (), 2, "list");
  }
}
