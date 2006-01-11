package gnu.kawa.functions;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.kawa.reflect.*;
import gnu.expr.*;

/** Procedure to get the value of a named component of an object. */

public class GetNamedPart extends Procedure2 implements HasSetter
{
  public static final GetNamedPart getNamedPart = new GetNamedPart();

  public Object apply2 (Object container, Object part)
    throws Throwable
  {
    if (container instanceof Values)
      {
        Object[] values = ((Values) container).getValues();
        Values result = new Values();
        for (int i = 0;  i < values.length;  i++)
          {
            Values.writeValues(apply2(values[i], part), result);
          }
        return result.canonicalize();
      }
    Symbol sym;
    if (part instanceof Symbol)
      sym = (Symbol) part;
    else
      sym = Namespace.EmptyNamespace.getSymbol(part.toString().intern());
    return getNamedPart(container, sym);
  }

  public static Object getNamedPart (Object container, Symbol part)
    throws Throwable
  {
    /*
    if (container implements HasNamedParts)
      return ((HasNamedParts) container).getNamedPart(part);
    */
    if (container instanceof Class)
      container = (ClassType) Type.make((Class) container);
    if (container instanceof ClassType)
      {
        try
          {
            return gnu.kawa.reflect.SlotGet.staticField(container, part.toString());
          }
        catch (Throwable ex)
          {
            // FIXME!
          }
        //return ClassMethodProc.make((ClassType)container, part.toString());
        return ClassMethods.apply(ClassMethods.classMethods, container, part, null, null, 0, 0);
      }
    try
      {
        return gnu.kawa.reflect.SlotGet.field(container, part.toString());
      }
    catch (Throwable ex)
      {
        // FIXME!
      }
    Language language = Language.getDefaultLanguage();
    MethodProc methods = ClassMethods.apply((ClassType) ClassType.make(container.getClass()),
                                            part.toString(),
                                            (Type) null, (Type[]) null,
                                            0, Access.STATIC);
    if (methods != null)
      return new CurriedInvoke(container, methods);
    throw new RuntimeException("no part '"+part+"' in "+container+" m:"+methods);
  }

  public void setN (Object[] args) throws Throwable
  {
    Object container = args[0];
    Object part = args[1];
    Object value = args[2];
    /*
    if (container implements HasNamedParts)
      return ((HasNamedParts) container).getNamedPart(part);
    */
    if (container instanceof Class)
      container = (ClassType) Type.make((Class) container);
    if (container instanceof ClassType)
      {
        try
          {
            gnu.kawa.reflect.SlotSet.setStaticField(container, part.toString(), value);
            return;
          }
        catch (Throwable ex)
          {
            // FIXME!
          }
      }

    gnu.kawa.reflect.SlotSet.setField(container, part.toString(), value);
  }
}

class CurriedInvoke extends ProcedureN
{
  Object thisArg;
  MethodProc methods;
  public CurriedInvoke(Object thisArg, MethodProc methods)
  {
    this.thisArg = thisArg;
    this.methods = methods;
  }
  public Object applyN(Object[] args)
    throws Throwable
  {
    Object[] xargs = new Object[args.length+1];
    System.arraycopy(args, 0, xargs, 1, args.length);
    xargs[0] = thisArg;
    return methods.applyN(xargs);
  }
}
