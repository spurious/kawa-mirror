package kawa.standard;
import kawa.lang.*;

public class ass extends Procedure2
{
  protected kawa.lang.Procedure2 compare;
  protected java.lang.String usage;
  public ass (String name, Procedure2 comp)
  {
    super(name);
    compare = comp;
    usage = new String("("+name+" obj list)");
  }

  public Object apply2 (Object arg1, Object arg2)
       throws WrongType, WrongArguments, GenericError, UnboundSymbol
  {
    while (arg2 instanceof Pair)
      {
	Pair list = (Pair)arg2;

	if (! (list.car instanceof Pair))
            throw new GenericError
	      ("The association list contains non-pair elements.");
	Pair pair = (Pair) list.car;

	Boolean check = (Boolean) compare.apply2 (pair.car, arg1);

	if (check.booleanValue())
	  return pair;

	arg2 = list.cdr;
      }

    if (arg2 == List.Empty)
      return Interpreter.falseObject;
    else
      throw new WrongType(this.name (), 2, "list");
  }
}
