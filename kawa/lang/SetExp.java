package kawa.lang;

/** An Expression to set (bind) or define a new value to a named variable.
 * @author	Per Bothner
 */

public class SetExp extends Expression
{
  private int flags;

  /** The name of the variable to set. */
  Symbol name;
  /** If non-null, the local Declaration that matches name. */
  public Declaration binding;
  /** The new value to assign to the variable. */
  Expression new_value;

  public SetExp (Symbol sym, Expression val)
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
      {
	ScopeExp scope = binding.context;
	while (scope.shared)
	  scope = scope.outer;
	while (env.scope != scope)
	  env = env.outer;
	env.values[binding.offset] = new_val;
      }
    else
      env.interp.define (name, new_val);
    return Interpreter.voidObject;
  }

  /* Compile code to store a value (which must already be on the
     stack) into the variable decl. */
  static public void compile_store (Declaration decl, Compilation comp)
  {
    if (decl.isSimple ())
      comp.method.compile_store_value (decl);
    else
      {
	ReferenceExp.compile_load (decl.baseVariable, comp);
	comp.method.compile_swap ();
	comp.method.compile_push_int (decl.offset);
	comp.method.compile_swap ();
	comp.method.compile_array_store (Compilation.scmObjectType);
      }
  }

  public void compile (Compilation comp, int flags)
  {
    if (binding != null)
      {
	if (binding.isSimple ())
	  {
	    new_value.compile (comp, 0);
	    comp.method.compile_store_value (binding);
	  }
	else
	  {
	    ReferenceExp.compile_load (binding.baseVariable, comp);
	    comp.method.compile_push_int (binding.offset);
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

  public void print (java.io.PrintStream ps)
  {
    ps.print(isDefining () ? "(#%define " : "(#%set! ");
    kawa.lang.print.print (name, ps);
    ps.print(" ");
    new_value.print (ps);
    ps.print(")");
  }
}
