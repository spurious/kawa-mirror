package kawa.standard;
import gnu.mapping.*;

public class call_with_values extends Procedure2
{
  public static final call_with_values callWithValues = new call_with_values();
  static { callWithValues.setName("call-with-values"); }

  public static Object callWithValues (Procedure producer, Procedure consumer)
     throws Throwable
  {
    Object values = producer.apply0();
    if (values instanceof Values)
      return ((Values) values).call_with(consumer);
    else
      return consumer.apply1(values);
  }

  public Object apply2 (Object producer, Object consumer) throws Throwable
  {
    return callWithValues((Procedure) producer, (Procedure) consumer);
  }

  public void apply (CallContext ctx) throws Throwable
  {
    Procedure.checkArgCount(this, 2);
    Object[] args = ctx.getArgs();
    Object values = ((Procedure) args[0]).apply0 ();
    Procedure consumer = (Procedure) args[1];
    if (values instanceof Values)
      {
	args = ((Values) values).getValues();
	consumer.checkN(args, ctx);
      }
    else
      {
	consumer.check1(values, ctx);
      }
  }
}


