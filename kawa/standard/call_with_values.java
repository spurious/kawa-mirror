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

  public void apply (CallStack stack)
  {
    Procedure.checkArgCount(this, 2);
    Object[] args = stack.args;
    Object values = ((Procedure) args[0]).apply0 ();
    Procedure consumer = (Procedure) args[1];
    if (values instanceof Values)
      args = ((Values) values).values();
    else
      {
	args = new Object[1];
	args[0] = values;
      }
    stack.args = args;
    consumer.apply(stack);
  }
}


