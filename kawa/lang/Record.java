package kawa.lang;
import java.lang.reflect.Field;
import gnu.bytecode.*;
import gnu.mapping.*;

public class Record extends Procedure1 implements HasSetter
{
  public String getTypeName()
  {
    return getClass().getName();
  }

  public static boolean isRecord (Object obj) { return obj instanceof Record; }

  public int hashCode()
  {
    Field[] fields = getClass().getFields();
    int hash = 12345;
    for (int i = 0;  i < fields.length;  i++)
      {
	Field field = fields[i];
	Object value;
	try
	  {
	    value = field.get(this);
	  }
	catch (IllegalAccessException ex)
	  {
	    continue;
	  }
	if (value != null)
	  hash ^= value.hashCode();
      }
    return hash;
  }

  public Object apply1 (Object arg)
  {
    String fname = (String) arg;
    Class clas = getClass();
    java.lang.reflect.Field fld;
    try
      {
	fld = clas.getField (fname);
	return fld.get(this);
      }
    catch (NoSuchFieldException ex)
      {
	//throw new UnboundSymbol(fname);
	throw new GenericError("no such field "+fname+" in "+clas.getName());
      }
    catch (IllegalAccessException ex)
      {
	throw new GenericError("illegal access for field "+fname);
      }
  }

  public void set1 (Object value, Object arg)
  { set1(this, value, (String) arg); }

  public static void set1 (Object record, Object value, String fname)
  {
    Class clas = record.getClass();
    java.lang.reflect.Field fld;
    try
      {
	fld = clas.getField (fname);
	fld.set(record, value);
      }
    catch (NoSuchFieldException ex)
      {
	//throw new UnboundSymbol(fname);
	throw new GenericError("no such field "+fname+" in "+clas.getName());
      }
    catch (IllegalAccessException ex)
      {
	throw new GenericError("illegal access for field "+fname);
      }
  }

  public boolean equals (Object obj)
  {
    if (this == obj)
      return true;
    Class thisClass = getClass();
    if (obj == null || obj.getClass() != thisClass)
      return false;
    Field[] fields = thisClass.getFields();
    for (int i = 0;  i < fields.length;  i++)
      {
	Field field = fields[i];
	Object value1, value2;
	try
	  {
	    value1 = field.get(this);
	    value2 = field.get(obj);
	  }
	catch (IllegalAccessException ex)
	  {
	    continue;
	  }
	if (! (value1.equals(value2)))
	  return false;
      }
    return true;
  }

  public String toString()
  {
    StringBuffer buf = new StringBuffer(200);
    buf.append("#<");
    buf.append(getTypeName());
    Field[] fields = getClass().getFields();
    for (int i = 0;  i < fields.length;  i++)
      {
	Field field = fields[i];
	Object value;
	try
	  {
	    value = field.get(this);
	  }
	catch (IllegalAccessException ex)
	  {
	    continue;
	  }
	buf.append(' ');
	buf.append(field.getName());
	buf.append(": ");
	buf.append(value);
      }
    buf.append(">");
    return buf.toString();
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print(toString());
  }

  public static ClassType makeRecordType (String name, List fnames)
  {
    ClassType superClass = new ClassType("kawa.lang.Record");
    ClassType clas = new ClassType(name);
    clas.setSuper(superClass);
    clas.access_flags = Access.PUBLIC;

    // Generate the (default) constructor.
    Method constructor = clas.addMethod ("<init>", Type.typeArray0,
					  Type.void_type, Access.PUBLIC);
    Method superConstructor
      = superClass.addMethod ("<init>", Type.typeArray0,
			       Type.void_type, Access.PUBLIC);
    constructor.init_param_slots ();
    gnu.bytecode.CodeAttr code = constructor.getCode();
    code.emitPushThis();
    code.emitInvokeSpecial(superConstructor);
    code.emitReturn();

    while (fnames != List.Empty)
      {
	Pair pair = (Pair) fnames;
	clas.addField(pair.car.toString(), Type.pointer_type, Access.PUBLIC);
	fnames = (List) pair.cdr;
      }
    byte[][] arrays = new byte[1][];
    String[] names = new String[1];
    names[0] = name;
    try
      {
	arrays[0] = clas.writeToArray();
      }
    catch (java.io.IOException ex)
      {
	throw new InternalError (ex.toString());
      }
    ArrayClassLoader loader = new ArrayClassLoader(names, arrays);
    try
      {
	Class reflectClass = loader.loadClass (name, true);
	Type.registerTypeForClass(reflectClass, clas);
	return clas;
      }
    catch (ClassNotFoundException ex)
      {
	throw new InternalError (ex.toString());
      }
  }

  public static List typeFieldNames (Class clas)
  {
    List list = List.Empty;
    Field[] fields = clas.getFields();
    for (int i = fields.length;  --i >= 0; )
      list = new Pair (fields[i].getName().intern(), list);
    return list;
  }

  public static List typeFieldNames (ClassType ctype)
  {
    return typeFieldNames(ctype.getReflectClass());
  }
}
