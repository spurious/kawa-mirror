package kawa.lang;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;

// Should be called PrimSetField for consistency.

public class SetFieldProc extends Procedure2 implements Inlineable
{
  ClassType ctype;
  String fname;
  gnu.bytecode.Field field;
  java.lang.reflect.Field reflectField;

  SetFieldProc (Class clas, String fname)
  {
    ctype = (ClassType) gnu.bytecode.Type.make(clas);
    this.fname = fname;
  }

  public SetFieldProc (ClassType ctype, String fname)
  {
    this.ctype = ctype;
    this.fname = fname;
  }

  public SetFieldProc (ClassType ctype, String name, Type ftype, int flags)
  {
    this.ctype = ctype;
    this.fname = name;
    field = ctype.getField(name);
    if (field == null)
      field = ctype.addField(name, ftype, flags);
  }

  public Object apply2 (Object arg1, Object arg2)
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
	if (field != null)
	  arg2 = field.getType().coerceFromObject(arg2);
	reflectField.set(arg1, arg2);
	return Interpreter.voidObject;
      }
    catch (IllegalAccessException ex)
      {
	throw new GenericError("illegal access for field "+fname);
      }
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    if (field == null)
      {
	field = ctype.getField(fname);
	if (field == null)
	  field = ctype.addField(fname, Type.make(reflectField.getType()),
				 reflectField.getModifiers());
      }
    exp.args[0].compile(comp, ctype);
    exp.args[1].compile(comp, field.getType());
    gnu.bytecode.CodeAttr code = comp.getCode();
    code.emitPutField(field);
    comp.compileConstant(Interpreter.voidObject, target);
  }
}
