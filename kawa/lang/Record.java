package kawa.lang;
import java.lang.reflect.Field;
import codegen.ClassType;
import codegen.Type;
import codegen.Access;
import codegen.Method;

public class Record
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

  public static Class makeRecordType (String name, List fnames)
  {
    ClassType superClass = new ClassType("kawa.lang.Record");
    ClassType clas = new ClassType(name);
    clas.setSuper(superClass);
    clas.access_flags = Access.PUBLIC;

    // Generate the (default) constructor.
    Method constructor = clas.new_method ("<init>", Type.typeArray0,
					  Type.void_type, Access.PUBLIC);
    Method superConstructor
      = superClass.new_method ("<init>", Type.typeArray0,
			       Type.void_type, Access.PUBLIC);
    constructor.init_param_slots ();
    constructor.compile_push_this ();
    constructor.compile_invoke_special (superConstructor);
    constructor.compile_return ();

    while (fnames != List.Empty)
      {
	Pair pair = (Pair) fnames;
	clas.new_field(pair.car.toString(), Type.pointer_type, Access.PUBLIC);
	fnames = (List) pair.cdr;
      }
    byte[][] arrays = new byte[1][];
    String[] names = new String[1];
    names[0] = name;
    try
      {
	arrays[0] = clas.emit_to_array();
	clas.emit_to_file("tmp.class");
      }
    catch (java.io.IOException ex)
      {
	throw new InternalError (ex.toString());
      }
    SchemeLoader loader = new SchemeLoader(names, arrays);
    try
      {
	return loader.loadClass (name, true);
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
      list = new Pair (Symbol.make(fields[i].getName()), list);
    return list;
  }
}
