package kawa.standard;
import kawa.lang.*;

public class make_vector extends Procedure1or2 {
  public make_vector()
  {
    super("make-vector");
  }

  public final Object apply1 (Object arg1)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return apply2 (arg1, Interpreter.undefinedObject);
  }

  public final Object apply2 (Object arg1,Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (! (arg1 instanceof java.lang.Integer))
      throw new WrongType(this.name(),1,"integer");
    int count = ((java.lang.Integer)arg1).intValue();
    return new Vector (count, arg2);
  }
}
