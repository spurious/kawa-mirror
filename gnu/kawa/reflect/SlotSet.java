package gnu.kawa.reflect;
import gnu.mapping.*;

public class SlotSet extends Procedure3
{
  public static void apply (Object obj, String name, Object value)
  {
    kawa.lang.Interpreter interpreter = kawa.standard.Scheme.getInstance();
    boolean illegalAccess = false;
    name = gnu.expr.Compilation.mangleName(name);
    Class clas = obj.getClass();
    try
      {
        java.lang.reflect.Field field = clas.getField(name);
        field.set(obj, interpreter.coerceFromObject(field.getType(), value));
        return;
      }
    catch (java.lang.NoSuchFieldException ex)
      {
      }
    catch (IllegalAccessException ex)
      {
	illegalAccess = true;
      }

    // Try looking for a method "setFname" instead.
    // First look for "getName", to get the "field type".
    StringBuffer namebuf = new StringBuffer(name.length()+3);
    namebuf.append("get");
    namebuf.append(Character.toTitleCase(name.charAt(0)));
    namebuf.append(name.substring(1));
    try
      {
        java.lang.reflect.Method getmethod
          = clas.getMethod(namebuf.toString(), SlotGet.noClasses);
        namebuf.setCharAt(0, 's');
        Class[] setArgTypes = new Class[1];
        setArgTypes[0] = getmethod.getReturnType();
        java.lang.reflect.Method setmethod
          = clas.getMethod(namebuf.toString(), setArgTypes);
        Object[] args = new Object[1];
        args[0] = interpreter.coerceFromObject(setArgTypes[0], value);
        setmethod.invoke(obj, args);
        return;
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
      throw new RuntimeException("illegal access for field "+name);
    else
      throw new RuntimeException ("no such field "+name
                                  +" in "+clas.getName());
  }

  public Object apply3 (Object obj, Object fname, Object value)
  {
    apply(obj, (String) fname, value);
    return Values.empty;
  }
}
