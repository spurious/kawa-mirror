package gnu.kawa.reflect;
import gnu.mapping.*;

public class SlotGet extends Procedure2 implements HasSetter
{
  static Class[] noClasses = { };

  public static Object apply (Object obj, String fname)
  {
    kawa.lang.Interpreter interpreter = kawa.standard.Scheme.getInstance();
    fname = gnu.expr.Compilation.mangleName(fname);
    Class clas = obj.getClass();
    boolean illegalAccess = false;
    try
      {
        java.lang.reflect.Field field = clas.getField(fname);
	Object result = field.get(obj);
        result = interpreter.coerceToObject(field.getType(), result);
        return result;
      }
    catch (IllegalAccessException ex)
      {
        illegalAccess = true;
      }
    catch (Exception ex)
      {
      }

    // Try looking for a method "getFname" instead:
    StringBuffer getname = new StringBuffer(fname.length()+3);
    getname.append("get");
    getname.append(Character.toTitleCase(fname.charAt(0)));
    getname.append(fname.substring(1));
    try
      {
        java.lang.reflect.Method getmethod
          = clas.getMethod(getname.toString(), noClasses);
        Object result = getmethod.invoke(obj, Procedure.noArgs);
        result = interpreter.coerceToObject(getmethod.getReturnType(), result);
        return result;
      }
    catch (java.lang.reflect.InvocationTargetException ex2)
      {
        Throwable th = ex2.getTargetException();
        if (th instanceof RuntimeException)
          throw (RuntimeException) th;
        if (th instanceof Error)
          throw (Error) th;
        throw new RuntimeException(th.toString());
      }
    catch (IllegalAccessException ex)
      {
        illegalAccess = true;
      }
    catch (java.lang.NoSuchMethodException ex3)
      {
      }
    if (illegalAccess)
      throw new RuntimeException("illegal access for field "+fname);
    else
      throw new RuntimeException ("no such field "+fname
                                  +" in "+clas.getName());
  }

  public Object apply2 (Object obj, Object fname)
  {
    return apply (obj, (String) fname);
  }

  public void setN (Object[] args)
  {
    int nargs = args.length;
    if (nargs != 3)
      throw new WrongArguments(getSetter(), nargs);
    set2(args[0], args[1], args[2]);
  }

  public void set2 (Object value, Object obj, Object name)
  {
    SlotSet.apply(obj, (String) name, value);
  }
}
