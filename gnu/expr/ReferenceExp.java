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

  private static int DONT_DEREFERENCE = NEXT_AVAIL_FLAG;
  private static int PROCEDURE_NAME = NEXT_AVAIL_FLAG << 1;
  public static int PREFER_BINDING2 = NEXT_AVAIL_FLAG << 2;

  /* If true, must have binding.isBinding().  Don't dereference Binding. */
  public final boolean getDontDereference()
  {
    return (flags & DONT_DEREFERENCE) != 0;
  }

  public final void setDontDereference(boolean setting)
  { setFlag(setting, DONT_DEREFERENCE); }

  /** True if this identifier appears in "function call position".
   * If so, it should be interpreted as a function name, which makes a
   * difference for languages (like Common Lisp) that have two name spaces. */
  public final boolean isProcedureName()
  {
    return (flags & PROCEDURE_NAME) != 0;
  }

  /** Note if this identifier appears in "function call position". */
  public final void setProcedureName(boolean setting)
  {
    setFlag(setting, PROCEDURE_NAME);
  }

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
    if (getDontDereference())
      return env.getBinding(symbol);
    else if (getFlag(PREFER_BINDING2))
      {
	Binding bind = Binding2.getBinding2(env, symbol);
	return isProcedureName() ? bind.getProcedure() : bind.get();
      }
    else
      return env.getChecked(symbol);
  }

  public void compile (Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    CodeAttr code = comp.getCode();
    Declaration decl = Declaration.followAliases(binding);
    if (decl != null)
      {
	decl.load(comp);
	if (decl.isIndirectBinding() && ! getDontDereference())
	  {
	    code.emitInvokeVirtual (Compilation.getLocationMethod);
	  }
	else if (decl.isFluid())
	  code.emitGetField(FluidLetExp.valueField);
      }
    else
      {
	Field field = comp.getBindingField(symbol);
	if (field.getStaticFlag())
	  code.emitGetStatic(field);
	else
	  {
	    LambdaExp lexp = comp.curLambda;
	    while (! (lexp instanceof ModuleExp))
	      lexp = lexp.outerLambda();
	    lexp.loadHeapFrame(comp);
	    code.emitGetField(field);
	  }
	if (getDontDereference())
	  { }
	else if (! isProcedureName())
	  code.emitInvokeVirtual(Compilation.getLocationMethod);
	else //if (! comp.getInterpreter().hasSeparateFunctionNamespace())
	  code.emitInvokeVirtual(Compilation.getProcedureBindingMethod);
	/*
	else
	  code.emitGetField(Compilation.functionValueBinding2Field);
	*/
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
      : getDontDereference() ? Compilation.typeLocation
      : binding.getType();
  }

  public String toString()
  {
    return "RefExp/"+symbol+'/'+id+'/';
  }
}
