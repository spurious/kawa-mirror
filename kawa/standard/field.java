package kawa.standard;
import gnu.mapping.*;

public class field extends Procedure2 implements HasSetter
{
  public static Object apply (Object obj, String fname)
  {
    java.lang.reflect.Field field;
    Class clas = obj.getClass();
    try
      {
	field = clas.getField(fname);
      }
    catch (java.lang.NoSuchFieldException ex)
      {
	throw new RuntimeException ("no such field "+fname
				    +" in "+clas.getName());
      }
    try
      {
	return field.get(obj);
      }
    catch (IllegalAccessException ex)
      {
	throw new RuntimeException("illegal access for field "+fname);
      }
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
    String fname = (String) name;
    java.lang.reflect.Field field;
    Class clas = obj.getClass();
    try
      {
	field = clas.getField(fname);
      }
    catch (java.lang.NoSuchFieldException ex)
      {
	throw new RuntimeException ("no such field "+fname
				    +" in "+clas.getName());
      }
    try
      {
	field.set(obj, value);
      }

    catch (IllegalAccessException ex)
      {
	throw new RuntimeException("illegal access for field "+fname);
      }
  }
}
