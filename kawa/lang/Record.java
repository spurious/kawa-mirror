package kawa.lang;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Vector;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.kawa.util.*;
import gnu.expr.Compilation;

public class Record extends NameMap
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

  public Object getChecked (String fname)
  {
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

  public Object put (String fname, Object value)
  {
    return set1 (this, value, fname);
  }

  public static Object set1 (Object record, Object value, String fname)
  {
    Class clas = record.getClass();
    java.lang.reflect.Field fld;
    try
      {
	fld = clas.getField (fname);
	Object old = fld.get(record);
	fld.set(record, value);
	return old;
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
    Field[] fields = getFieldFields(thisClass);
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

  public static Field[] getFieldFields(Class clas)
  {
    String names;
    try
    {
      Field fld = clas.getDeclaredField("$FieldNames$");
      names = (String) fld.get(null);
    }
    catch (Exception ex)
    {
      return clas.getDeclaredFields();
    }
    int nfields = 0;
    int nlen = names.length();
    for (int i = nlen;  --i >= 0; )
    {
      if (names.charAt(i) == '\n')
	nfields++;
    }
    Field[] fields = new Field[nfields];
    int start = 0;
    int ifield;
    for (int i = 0;  i < nfields;  i++)
    {
      int end = names.indexOf('\n', start);
      String fname = names.substring(start, end);
      fname = gnu.expr.Compilation.mangleName(fname);
      try
	{
	  fields[i] = clas.getDeclaredField(fname);
	}
      catch (Exception ex)
	{
	  throw new WrappedException("record missing field "+fname, ex);
	}
      start = end + 1;
    }
    return fields;
  }

  public Field[] getFieldFields()
  {
    return getFieldFields(getClass());
  }

  public String toString()
  {
    StringBuffer buf = new StringBuffer(200);
    buf.append("#<");
    buf.append(getTypeName());
    /*
    try
      {
	Field fld = clas.getDeclaredField("$FieldNames$");
	String names = (String) fld.get(null);
	int nfields = 0;
	int nlen = names.length();
	int start = 0;
	int ifield;
	for (int i = 0;  ;  i++)
	  {
	    int end = names.indexOf('\n', start);
	    if (end < 0)
	      break;
	    String fname = names.substring(start, end);
	    String mname = gnu.expr.Compilation.mangleName(fname);
	    try
	      {
		this.fields[i] = clas.getDeclaredField(mname);
		Object value = field.get(this);
		start = end + 1;
		buf.append(' ');
		buf.append(fname);
		buf.append(": ");
		buf.append(value);
	      }
	    catch (Exception ex)
	      {
		throw new WrappedException("record missing field "+fname, ex);
	      }
	  }
      }
    catch (ClassCastException ex)
      {
        Field[] fields = clas.getFields();
      }
    */
    ClassType ctype = (ClassType) Type.make(getClass());
    for (gnu.bytecode.Field fld = ctype.getFields();
	 fld != null;  fld = fld.getNext())
      {
	if ((fld.getModifiers() & (Modifier.STATIC|Modifier.PUBLIC))
	    != Modifier.PUBLIC)
	  continue;
	Object value;
	try
	  {
	    Field field = fld.getReflectField();
	    value = field.get(this);
	  }
	catch (Exception ex)
	  {
	    value = "#<illegal-access>";
	  }
	buf.append(' ');
	buf.append(fld.getSourceName());
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

  public static ClassType makeRecordType (String name, LList fnames)
  {
    ClassType superClass = ClassType.make("kawa.lang.Record");
    String mangledName = Compilation.mangleName(name);
    ClassType clas = new ClassType(mangledName);
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
    if (! name.equals(mangledName))
      {
	Method meth = clas.addMethod ("getTypeName", Type.typeArray0,
				      Compilation.typeString, Access.PUBLIC);
	meth.init_param_slots ();
	code = meth.getCode();
	code.emitPushString(name);
	code.emitReturn();
      }

    //StringBuffer fnamesBuf = new StringBuffer(100);
    gnu.bytecode.Field fld;
    while (fnames != LList.Empty)
      {
	Pair pair = (Pair) fnames;
	String fname = pair.car.toString();
	//fnamesBuf.append(fname);  fnamesBuf.append('\n');
	fld = clas.addField(Compilation.mangleName(fname),
			    Type.pointer_type, Access.PUBLIC);
	fld.setSourceName(fname.intern());
	fnames = (LList) pair.cdr;
      }
    /*
    fld = clas.addField("$FieldNames$", Compilation.typeString,
		      Access.PUBLIC|Access.STATIC|Access.FINAL);
    ConstantValueAttr attr = new ConstantValueAttr(fnamesBuf.toString());
    attr.addToFrontOf(fld);
    */

    byte[][] arrays = new byte[1][];
    String[] names = new String[1];
    names[0] = mangledName;
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
	Class reflectClass = loader.loadClass (mangledName, true);
	Type.registerTypeForClass(reflectClass, clas);
	return clas;
      }
    catch (ClassNotFoundException ex)
      {
	throw new InternalError (ex.toString());
      }
  }

  public static LList typeFieldNames (Class clas)
  {
    LList list = LList.Empty;
    /*
    try
      {
	Field fld = clas.getDeclaredField("$FieldNames$");
	String names = (String) fld.get(null);
	int nfields = 0;
	int limit = names.length() - 1;
	
	int ifield;
	while (limit > 0)
	  {
	    int start = names.lastIndexOf('\n', limit - 1);
	    String fname = names.substring(start + 1, limit);
	    list = new Pair(fname.intern(), list);
	    limit = start;
	  }
	return list;
      }
    catch (Exception ex)
      {
      }
    */
    ClassType ctype = (ClassType) Type.make(clas);
    gnu.bytecode.Field field = ctype.getFields();
    Vector vec = new Vector(100);
    for (;  field != null;  field = field.getNext())
      {
	if ((field.getModifiers() & (Modifier.STATIC|Modifier.PUBLIC))
	    == Modifier.PUBLIC)
	  vec.addElement(field.getSourceName());
      }
    for (int i = vec.size();  --i >= 0; )
      {
	list = new Pair(vec.elementAt(i), list);
      }
    return list;
  }

  public static LList typeFieldNames (ClassType ctype)
  {
    return typeFieldNames(ctype.getReflectClass());
  }
}
