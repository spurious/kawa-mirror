package kawa.standard;
import kawa.lang.*;

public class call_with_values extends Procedure2
{
  public call_with_values ()
  {
    super ("call-with-values");
  }

  public Object apply2 (Object producer, Object consumer)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    Object values = ((Procedure) producer).apply0 ();
    Procedure cons_proc = (Procedure) consumer;
    if (values instanceof Values)
      return ((Values) values).call_with (cons_proc);
    else
      return cons_proc.apply1 (values);
  }
}

