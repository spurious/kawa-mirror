package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

/**
 * This class represents a variable reference (an identifier).
 * @author	Per Bothner
 */

public class ReferenceExp extends Expression
{
  String symbol;
  Declaration binding;
  public String string_name () { return symbol; }

  public final String getName() { return symbol; }
  /** If non-null, the local Declaration this refers to. */
  public final Declaration getBinding() { return binding; }

  static int counter;
  /** Unique id number, to ease print-outs and debugging. */
  int id = ++counter;

  boolean dontDereference;
  /* If true, must have binding.isBinding().  Don't dereference Binding. */
  public final boolean getDontDereference() { return  dontDereference; }
  public final void setDontDereference(boolean dontDereference)
  { this.dontDereference = dontDereference; }

  public ReferenceExp (String symbol)
  {
    this.symbol = symbol;
  }

  public ReferenceExp (String symbol, Declaration binding)
  {
    this.symbol = symbol;
    this.binding = binding;
  }

  public Object eval (Environment env)
  {
    if (binding != null)
      throw new Error("internal error: ReferenceExp.eval on lexical binding");
    return env.getChecked(symbol);
  }

  private static ClassType thisType;
  static ClassType ctypeBinding = null;
  static Method getMethod = null;

  public void compile (Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    CodeAttr code = comp.getCode();
    if (binding != null)
      {
	binding.load(comp);
	if (binding.isIndirectBinding() && ! getDontDereference())
	  {
	    if (ctypeBinding == null)
	      {
		ctypeBinding = ClassType.make("gnu.mapping.Binding");
		getMethod = ctypeBinding.addMethod("get",
						   Type.typeArray0,
						   Compilation.scmObjectType,
						   Access.PUBLIC|Access.FINAL);
	      }
	    code.emitInvokeVirtual (getMethod);
	  }
      }
    else
      {
	code.emitGetStatic(comp.getBindingField(symbol));
	code.emitInvokeVirtual(Compilation.getBindingMethod);
      }
    target.compileFromStack(comp, getType());
  }

  Object walk (ExpWalker walker) { return walker.walkReferenceExp(this); }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%ref/");
    ps.print(id);
    ps.print("/ ");
    SFormat.print (symbol, ps);
    ps.print(")");
  }

  public final gnu.bytecode.Type getType()
  {
    return binding == null ? Type.pointer_type : binding.getType();
  }

  public String toString()
  {
    return "RefExp/"+symbol+'/'+id+'/';
  }
}
