package kawa.lang;
import gnu.bytecode.*;
import kawa.math.IntNum;
import kawa.math.DFloNum;

/** A call to a plain Java method. */

public class PrimApplyExp extends ApplyExp
{
  PrimProcedure proc;

  PrimApplyExp (PrimProcedure proc, Expression[] args)
  {
    super (new QuoteExp (proc), args);
    this.proc = proc;
  }

  PrimApplyExp (QuoteExp proc, Expression[] args)
  {
    super (proc, args);
    this.proc = (PrimProcedure)proc.value;
  }

  public void compile (Compilation comp, int flags)
  {
    Type[] arg_types = proc.argTypes;
    int arg_count = arg_types.length;
    boolean is_static = proc.getStaticFlag();
    if (args.length != arg_count + (is_static ? 0 : 1))
      throw new Error ("internal error - wrong number of arguments to primitive");
    if (proc.opcode() == 183) // invokespecial == primitive-constructor
      {
	ClassType type = proc.method.getDeclaringClass();
	comp.method.compile_new (type);
	comp.method.compile_dup (type);
      }
    for (int i = 0; i < args.length; ++i)
      {
	Type arg_type = is_static ? arg_types[i]
	  : i==0 ? proc.method.getDeclaringClass()
	  : arg_types[i-1];
	args[i].compile (comp, 0, arg_type);
      }
    
    Type retType = proc.retType;
    if (proc.method == null)
      comp.method.compile_primop (proc.opcode(), args.length, retType);
    else
      comp.method.compile_invoke_method (proc.method, proc.opcode());

    if (retType == Type.void_type)
      {
	if ((flags & IGNORED) == 0)
	  comp.compileConstant (Interpreter.voidObject);
      }
    else if ((flags & IGNORED) != 0)
      comp.method.compile_pop (1);
    else if (retType instanceof ClassType)
      return;
    else if (retType == Type.int_type || retType == Type.short_type
	     || retType == Type.byte_type)
      {
	IntNum.initMakeMethods();
	comp.method.compile_invoke_static (IntNum.makeIntMethod);
      }
    else if (retType == Type.long_type)
      {
	IntNum.initMakeMethods();
	comp.method.compile_invoke_static (IntNum.makeLongMethod);
      }
    else if (retType == Type.double_type || retType == Type.float_type)
      {
	comp.method.compile_convert (Type.double_type);
	DFloNum.initMakeMethods();
	comp.method.compile_invoke_static (DFloNum.makeMethod);
      }
    else if (retType == Type.char_type)
      {
	Char.initMakeMethods();
	comp.method.compile_invoke_static (Char.makeCharMethod);
      }
    else if (retType == Type.boolean_type)
      {
	comp.method.compile_if_neq_0 ();
	comp.compileConstant (Interpreter.trueObject);
	comp.method.compile_else ();
	comp.compileConstant (Interpreter.falseObject);
	comp.method.compile_fi ();
      }
    else
      throw new Error ("unimplemented return type");
  }
}
