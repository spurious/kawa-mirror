package gnu.kawa.functions;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.kawa.reflect.*;
import gnu.expr.*;
import java.io.*;

/** The value of the Kawa Scehem expression '*:PART-NAME'.
 * This function invokes a method or accesses a field,
 * if the PART-NAME starts with a '.'.
 *
 * This syntax is semi-depecated, since instead of
 * (*:method-name instance args ...) you can now write
 * (instance:method-name args ...), and
 * instead of (*:.field-name instance) you can write
 * instance:field-name (without the parentheses).
 */

public class GetNamedInstancePart extends ProcedureN
  implements Externalizable, CanInline, HasSetter
{
  String pname;
  boolean isField;

  public static Expression makeExp (Expression member)
  {
    String name;
    if (member instanceof QuoteExp)
      {
        Object val = ((QuoteExp) member).getValue();
        if (val instanceof SimpleSymbol)
          return QuoteExp.getInstance(new GetNamedInstancePart(val.toString()));
      }
    Expression[] args = new Expression[2];
    args[0] = new QuoteExp(ClassType.make("gnu.kawa.functions.GetNamedInstancePart"));
    args[1] = member;
    return new ApplyExp(Invoke.make, args);
  }

  public GetNamedInstancePart ()
  {
  }

  public GetNamedInstancePart (String name)
  {
    setPartName(name);
  }

  public void setPartName (String name)
  {
    setName("get-instance-part:"+name);
    if (name.length() > 1 && name.charAt(0) == '.')
      {
        isField = true;
        pname = name.substring(1);
      }
    else
      {
        isField = false;
        pname = name;
      }
  }

  public int numArgs() { return isField ? 0x1001 : 0xfffff001; }

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
    Expression[] args = exp.getArgs();
    Expression[] xargs;
    Procedure proc;
    if (isField)
      {
        xargs = new Expression[] { args[0], new QuoteExp(pname) };
        proc = SlotGet.field;
      }
    else
      {
        int nargs = args.length;
        xargs = new Expression[nargs+1];
        xargs[0] = args[0];
        xargs[1] = new QuoteExp(pname);
        System.arraycopy(args, 1, xargs, 2, nargs-1);
        proc = Invoke.invoke;
      }
    return walker.walkApplyOnly(new ApplyExp(proc, xargs));
  }

  public Object applyN (Object[] args)
    throws Throwable
  {
    checkArgCount(this, args.length);
    if (isField)
      return SlotGet.field(args[0], pname);
    else
      {
        Object[] xargs = new Object[args.length+1];
        xargs[0] = args[0];
        xargs[1] = pname;
        System.arraycopy(args, 1, xargs, 2, args.length-1);
        return Invoke.invoke.applyN(xargs);
      }
  }

  public Procedure getSetter()
  {
    if (! isField)
      throw new RuntimeException("no setter for instance method call");
    return new SetNamedInstancePart(pname);
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(isField ? ("."+pname) : pname);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    setPartName((String) in.readObject());
  }
}

class SetNamedInstancePart extends Procedure2
  implements Externalizable, CanInline
{
  String pname;

  public SetNamedInstancePart ()
  {
  }

  public SetNamedInstancePart (String name)
  {
    setPartName(name);
  }

  public void setPartName (String name)
  {
    setName("set-instance-part:."+name);
    pname = name;
  }

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
    Expression[] args = exp.getArgs();
    Expression[] xargs = new Expression[]
      { args[0], new QuoteExp(pname), args[1] };
    Procedure proc = SlotSet.set$Mnfield$Ex;
    return walker.walkApplyOnly(new ApplyExp(proc, xargs));
  }

  public Object apply2 (Object instance, Object value)
    throws Throwable
  {
    SlotSet.setField(instance, pname, value);
    return Values.empty;
  }


  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(pname);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    setPartName((String) in.readObject());
  }
}
