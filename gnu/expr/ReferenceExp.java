// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

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

  public ReferenceExp (Declaration binding)
  {
    this.binding = binding;
    this.symbol = binding.getName();
  }

  public Object eval (Environment env)
  {
    if (binding != null
        && ! (binding.context instanceof ModuleExp && ! binding.isPrivate()))
      throw new Error("internal error: ReferenceExp.eval on lexical binding");
    return env.getChecked(symbol);
  }

  private static ClassType thisType;
  static ClassType typeLocation = null;
  static Method getMethod = null;

  public void compile (Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    CodeAttr code = comp.getCode();
    if (binding != null
        && ! (binding.context instanceof ModuleExp && ! binding.isPrivate()))
      {
	binding.load(comp);
	if (binding.isIndirectBinding() && ! getDontDereference())
	  {
	    if (typeLocation == null)
	      {
		typeLocation = ClassType.make("gnu.mapping.Location");
		getMethod = typeLocation.addMethod("get",
						   Type.typeArray0,
						   Compilation.typeObject,
						   Access.PUBLIC|Access.FINAL);
	      }
	    code.emitInvokeVirtual (getMethod);
	  }
	else if (binding.isFluid())
	  code.emitGetField(FluidLetExp.valueField);
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
    return (binding == null || binding.isFluid()) ? Type.pointer_type
      : binding.getType();
  }

  public String toString()
  {
    return "RefExp/"+symbol+'/'+id+'/';
  }
}
