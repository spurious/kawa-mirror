package kawa.standard;
import gnu.mapping.*;

public class static_field extends Procedure2 implements HasSetter
{
  static Class coerceToClass(Object obj)
  {
    if (obj instanceof Class)
      return (Class) obj;
    if (obj instanceof gnu.bytecode.Type)
      return ((gnu.bytecode.Type) obj).getReflectClass();
    throw new RuntimeException("argument is neither Class nor Type");
  }

  public static Object apply (Object cl, String fname)
  {
    java.lang.reflect.Field field;
    Class clas = coerceToClass(cl);
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
	return field.get(null);
      }
    catch (IllegalAccessException ex)
      {
	throw new RuntimeException("illegal access for field "+fname);
      }
  }

  public Object apply2 (Object cl, Object fname)
  {
    return apply (cl, (String) fname);
  }

  public void setN (Object[] args)
  {
    int nargs = args.length;
    if (nargs != 3)
      throw new WrongArguments(getSetter(), nargs);
    set2(args[0], args[1], args[2]);
  }

  public void set2 (Object value, Object cl, Object name)
  {
    String fname = (String) name;
    java.lang.reflect.Field field;
    Class clas = coerceToClass(cl);
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
	field.set(null, value);
      }

    catch (IllegalAccessException ex)
      {
	throw new RuntimeException("illegal access for field "+fname);
      }
  }
}
