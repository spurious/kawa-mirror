package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.*;

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

    if ("%do%loop".equals(decl.symbol())
	&& val instanceof LambdaExp)
      ((LambdaExp) val).setInlineOnly(true);
  }

  public Object eval (Environment env)
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
    return Values.empty;
  }

  static ClassType ctypeBinding = null;
  static Method setMethod = null;

  public void compile (Compilation comp, Target target)
  {
    if (new_value instanceof LambdaExp
	&& target instanceof IgnoreTarget
	&& ((LambdaExp) new_value).getInlineOnly())
      return;
    gnu.bytecode.CodeAttr code = comp.getCode();
    if (binding != null)
      {
	if (binding.ignorable())
	  new_value.compile (comp, Target.Ignore);
	else if (binding.isIndirectBinding())
	  {
	    binding.load(comp);
	    new_value.compile (comp, Target.pushObject);
	    if (ctypeBinding == null)
	      {
		ctypeBinding = ClassType.make("gnu.mapping.Binding");
		setMethod = ctypeBinding.addMethod("set",
						   Compilation.apply1args,
						   Type.void_type,
						   Access.PUBLIC|Access.FINAL);
	      }
	    code.emitInvokeVirtual (setMethod);
	  }
	else if (binding.isSimple ())
	  {
	    new_value.compile (comp, Target.pushObject);
	    code.emitStore(binding);
	  }
	else
	  {
	    binding.loadOwningObject(comp);
	    new_value.compile (comp, Target.pushObject);
	    code.emitPutField(binding.field);
	  }
      }
    else
      {
	comp.compileConstant (name);
	comp.method.maybe_compile_checkcast (comp.scmSymbolType);
	new_value.compile (comp, Target.pushObject);
	code.emitInvokeStatic(isDefining () ? comp.defineGlobalMethod
			      : comp.putGlobalMethod);
      }

    comp.compileConstant(Values.empty, target);
  }

  Object walk (ExpWalker walker) { return walker.walkSetExp(this); }

  public void print (java.io.PrintWriter ps)
  {
    ps.print(isDefining () ? "(#%define " : "(#%set! ");
    SFormat.print (name, ps);
    ps.print(" ");
    new_value.print (ps);
    ps.print(")");
  }
}
