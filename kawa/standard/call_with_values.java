package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;

public class call_with_values extends Procedure2
{
  public static Object callWithValues (Procedure producer, Procedure consumer)
  {
    Object values = producer.apply0();
    if (values instanceof Values)
      return ((Values) values).call_with(consumer);
    else
      return consumer.apply1(values);
  }

  public Object apply2 (Object producer, Object consumer)
  {
    return callWithValues((Procedure) producer, (Procedure) consumer);
  }

  public void apply (CallContext stack)
  {
    Procedure.checkArgCount(this, 2);
    Object[] args = stack.args;
    Object values = ((Procedure) args[0]).apply0 ();
    Procedure consumer = (Procedure) args[1];
    if (values instanceof Values)
      args = ((Values) values).getValues();
    else
      {
	args = new Object[1];
	args[0] = values;
      }
    stack.args = args;
    consumer.apply(stack);
  }
}


