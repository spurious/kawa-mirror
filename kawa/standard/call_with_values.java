package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;

public class call_with_values extends Procedure2
{
  public Object apply2 (Object producer, Object consumer)
  {
    Object values = ((Procedure) producer).apply0 ();
    Procedure cons_proc = (Procedure) consumer;
    if (values instanceof Values)
      return ((Values) values).call_with (cons_proc);
    else
      return cons_proc.apply1 (values);
  }
}

