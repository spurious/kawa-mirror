package kawa.lang;

/** An Expression to set (bind) or define a new value to a named variable.
 * @author	Per Bothner
 */

public class SetExp extends Expression
{
  private int flags;

  /** The name of the variable to set. */
  String name;
  /** If non-null, the local Declaration that matches name. */
  public Declaration binding;
  /** The new value to assign to the variable. */
  Expression new_value;

  public SetExp (String sym, Expression val)
  { name = sym;  new_value = val; }

  static private int DEFINING_FLAG = 1;
  static private int GLOBAL_FLAG = 2;

  public final boolean isDefining ()
  {
    return (flags & DEFINING_FLAG) != 0;
  }

  public final void setDefining (boolean value)
  {
    if (value)
      flags |= DEFINING_FLAG;
    else
      flags &= ~DEFINING_FLAG;
  }

  public SetExp (Declaration decl, Expression val)
  {
    this.binding = decl;
    name = decl.sym;
    new_value = val;
  }

  public Object eval (Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    Object new_val = new_value.eval (env);

    if (binding != null)
      throw new Error ("internal error - SetExp.eval with lexical binding");
    if (isDefining ())
      env.define (name, new_val);
    else
      {
	Binding bind = env.lookup (name);
	if (bind == null)
	  throw new UnboundSymbol (name);
	env.put (name, new_val);
      }
    return Interpreter.voidObject;
  }

  /* Compile code to store a value (which must already be on the
     stack) into the variable decl. */
  static public void compile_store (Declaration decl, Compilation comp)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    if (decl.isSimple ())
      code.emitStore(decl);
    else
      {
	ReferenceExp.compile_load (decl.baseVariable, comp);
	comp.method.maybe_compile_checkcast (Compilation.objArrayType);
	code.emitSwap();
	code.emitPushInt(decl.offset);
	code.emitSwap();
	comp.method.compile_array_store (Compilation.scmObjectType);
      }
  }

  public void compile (Compilation comp, int flags)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    if (binding != null)
      {
	if (binding.isSimple ())
	  {
	    new_value.compile (comp, 0);
	    code.emitStore(binding);
	  }
	else
	  {
	    ReferenceExp.compile_load (binding.baseVariable, comp);
	    comp.method.maybe_compile_checkcast (Compilation.objArrayType);
	    code.emitPushInt(binding.offset);
	    new_value.compile (comp, 0);
	    comp.method.compile_array_store (Compilation.scmObjectType);
	  }
      }
    else
      {
	comp.compileConstant (name);
	comp.method.maybe_compile_checkcast (comp.scmSymbolType);
	new_value.compile (comp, 0);
	comp.method.compile_invoke_static (comp.defineGlobalMethod);
      }

    if ((flags & IGNORED) == 0)
      comp.compileConstant (Interpreter.voidObject);
  }

  public void print (java.io.PrintWriter ps)
  {
    ps.print(isDefining () ? "(#%define " : "(#%set! ");
    SFormat.print (name, ps);
    ps.print(" ");
    new_value.print (ps);
    ps.print(")");
  }
}
