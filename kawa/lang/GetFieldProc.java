package kawa.lang;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;

// Should be called PrimGetField for consistency.

public class GetFieldProc extends Procedure1 implements Inlineable
{
  ClassType ctype;
  String fname;
  gnu.bytecode.Field field;
  java.lang.reflect.Field reflectField;

  GetFieldProc (Class clas, String fname) throws GenericError
  {
    ctype = (ClassType) gnu.bytecode.Type.make(clas);
    this.fname = fname;
  }

  public GetFieldProc (ClassType ctype, String name, Type ftype, int flags)
  {
    this.ctype = ctype;
    this.fname = name;
    field = ctype.getField(name);
    if (field == null)
      field = ctype.addField(name, ftype, flags);
  }

  public Object apply1 (Object arg1)
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
	return reflectField.get(arg1);
      }
    catch (IllegalAccessException ex)
      {
	throw new GenericError("illegal access for field "+fname);
      }
  }

  public void compile (ApplyExp exp, Compilation comp, int flags)
  {
    exp.args[0].compile(comp, 0, ctype);
    if (field == null)
      {
	field = ctype.getField(fname);
	if (field == null)
	  field = ctype.addField(fname, Type.make(reflectField.getType()),
				 reflectField.getModifiers());
      }
    gnu.bytecode.CodeAttr code = comp.getCode();
    code.emitGetField(field);
    if ((flags & Expression.IGNORED) != 0)
      code.emitPop(1);
  }
}
