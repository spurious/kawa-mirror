package kawa.lang;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;

public class PrimGetStatic extends Procedure0 implements Inlineable
{
  ClassType ctype;
  String fname;
  gnu.bytecode.Field field;
  java.lang.reflect.Field reflectField;

  PrimGetStatic (Class clas, String fname) throws GenericError
  {
    ctype = (ClassType) gnu.bytecode.Type.make(clas);
    this.fname = fname;
  }

  public PrimGetStatic (ClassType ctype, String name, Type ftype, int flags)
  {
    this.ctype = ctype;
    this.fname = name;
    field = ctype.getField(name);
    if (field == null)
      field = ctype.addField(name, ftype, flags);
  }

  public Object apply0 ()
  {
    if (reflectField == null)
      {
	Class clas = ctype.getReflectClass();
	try
	  {
	    reflectField = clas.getField (fname);
	  }
	catch (NoSuchFieldException ex)
	  {
	    throw new GenericError ("no such field "+fname+" in "+clas.getName());
	  }
      }
    try
      {
	return reflectField.get(null);
      }
    catch (IllegalAccessException ex)
      {
	throw new GenericError("illegal access for field "+fname);
      }
  }

  public void compile (ApplyExp exp, Compilation comp, int flags)
  {
    if (field == null)
      {
	field = ctype.getField(fname);
	if (field == null)
	  field = ctype.addField(fname, Type.make(reflectField.getType()),
				 reflectField.getModifiers());
      }
    gnu.bytecode.CodeAttr code = comp.getCode();
    code.emitGetStatic(field);
    if ((flags & Expression.IGNORED) != 0)
      code.emitPop(1);
  }
}
