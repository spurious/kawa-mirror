// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

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
  /** If non-null, the local Declaration this refers to. */
  public Declaration binding;
  /** The new value to assign to the variable. */
  Expression new_value;

  public SetExp (String sym, Expression val)
  { name = sym;  new_value = val; }

  static private int DEFINING_FLAG = NEXT_AVAIL_FLAG;
  static private int GLOBAL_FLAG = NEXT_AVAIL_FLAG << 1;
  public static  int PREFER_BINDING2 = NEXT_AVAIL_FLAG << 2;
  static private int PROCEDURE = NEXT_AVAIL_FLAG << 3;
  static private int SET_IF_UNBOUND = NEXT_AVAIL_FLAG << 4;
  static private int HAS_VALUE = NEXT_AVAIL_FLAG << 5;

  public final boolean isDefining ()
  {
    return (flags & DEFINING_FLAG) != 0;
  }

  public final void setDefining (boolean value)
  {
    if (value) flags |= DEFINING_FLAG; else flags &= ~DEFINING_FLAG;
  }

  /** True if evaluating the SetExp yields the value of the RHS. */
  public final boolean getHasValue()
  { return (flags & HAS_VALUE) != 0; }

  public final void setHasValue (boolean value)
  { if (value) flags |= HAS_VALUE; else flags &= ~HAS_VALUE; }

  /** True if this is a functon definition ("defun"). */
  public final boolean isFuncDef()
  { return (flags & PROCEDURE) != 0; }

  public final void setFuncDef (boolean value)
  { if (value) flags |= PROCEDURE; else flags &= ~PROCEDURE; }

  public final boolean isSetIfUnbound()
  { return (flags & SET_IF_UNBOUND) != 0; }

  public final void setSetIfUnbound (boolean value)
  { if (value) flags |= SET_IF_UNBOUND; else flags &= ~SET_IF_UNBOUND; }

  public SetExp (Declaration decl, Expression val)
  {
    this.binding = decl;
    name = decl.getName();
    new_value = val;

    if ("%do%loop".equals(decl.getName())
	&& val instanceof LambdaExp
	&& ! Compilation.usingCPStyle())
      ((LambdaExp) val).setInlineOnly(true);
  }

  public Object eval (Environment env)
  {
    if (isSetIfUnbound())
      {
	Binding binding = env.getBinding(name);
	if (! binding.isBound())
	  binding.set(new_value.eval (env));
	if (getHasValue())
	  return name;
	else
	  return Interpreter.getInterpreter().noValue();
      }

    Object new_val = new_value.eval (env);

    if (binding != null
        && ! (binding.isStatic() && ! binding.isPrivate()))
      throw new Error ("internal error - SetExp.eval with lexical binding");
    if (isDefining ())
      {
	if (binding != null && binding.isAlias())
	  AliasConstraint.define(env.getBinding(name),
				 (gnu.mapping.Location) new_val);
	else
	  env.define (name, new_val);
      }
    else
      {
	Binding bind = (getFlag(PREFER_BINDING2)
			? Binding2.getBinding2(env, name)
			: env.lookup (name));
	if (bind != null)
	  env.put (name, new_val);
	else
	  env.define (name, new_val);
	//	  throw new UnboundSymbol (name);
      }
    return getHasValue() ? new_val : Interpreter.getInterpreter().noValue();
  }

  static Method setMethod = null;

  public void compile (Compilation comp, Target target)
  {
    if (new_value instanceof LambdaExp
	&& target instanceof IgnoreTarget
	&& ((LambdaExp) new_value).getInlineOnly())
      return;
    gnu.bytecode.CodeAttr code = comp.getCode();
    // FIXME - handle isSetIfUnbound
    boolean needValue = getHasValue() && ! (target instanceof IgnoreTarget);
    if (needValue && binding != null)
      throw new Error("SetExp.compile: not implemented - return value");

    // This code is kind of kludgy, because it handles a number of
    // different cases:  assignments and definitions to both local and
    // globals variables.  Some of the complication is because we want
    // to generate fields for module-level definitions;  this is how
    // bindings are exported from modules.

    if (binding != null && ! binding.isPrivate()
	&& binding.context instanceof ModuleExp
	&& binding.getValue() instanceof LambdaExp
	&& ((LambdaExp) binding.getValue()).getName() != null // FIXME
	&& binding.getValue() == new_value)
      {
	((LambdaExp) new_value).compileSetField(comp);
      }
    else if (binding != null
	     && binding.context instanceof ModuleExp
	     && (new_value instanceof QuoteExp)
	     && ! binding.isPrivate() && ! comp.immediate
	     && binding.getValue() != null)
      { // This handles macros a la syntax-rules - and other constants.
	Object value = ((QuoteExp) new_value).getValue();
	String fname = Compilation.mangleName(name);
	Literal literal = comp.findLiteral(value);
	if (literal.field == null)
	  literal.assign(fname, comp);
      }
    else if (binding instanceof kawa.lang.Macro
	     && binding.context instanceof ModuleExp
	     && ((kawa.lang.Macro) binding).expander instanceof LambdaExp
	     && ! binding.isPrivate())
      {
	LambdaExp expander = (LambdaExp) ((kawa.lang.Macro) binding).expander;
	if (! expander.isHandlingTailCalls())
	  {
	    expander.flags |= LambdaExp.NO_FIELD;
	    expander.compileAsMethod(comp);
	    comp.applyMethods.addElement(expander);
	  }
	new BindingInitializer(binding, comp, new_value);
      }
    else if (binding != null)
      {
	if (binding.ignorable())
	  new_value.compile (comp, Target.Ignore);
	// FIXME combine next two case?  In that case,
	// where should isDefining() && ! binding.isPublic() be handled?
	else if (binding.isAlias() && isDefining() && binding.isPublic())
	  {
	    binding.load(comp);
	    new_value.compile (comp, Target.pushObject);
	    Method meth = ClassType.make("gnu.mapping.AliasConstraint")
	      .getDeclaredMethod("define", 2);
	    code.emitInvokeStatic(meth);
	  }
	else if (binding.isIndirectBinding()
		 && (! isDefining() || binding.isPublic()))
	  {
	    binding.load(comp);
	    new_value.compile (comp, Target.pushObject);
	    if (setMethod == null)
	      setMethod = comp.typeLocation.addMethod
		("set", Compilation.apply1args,
		 Type.void_type, Access.PUBLIC|Access.FINAL);
	    code.emitInvokeVirtual (setMethod);
	  }
	else if (binding.isFluid())
	  {
	    binding.load(comp);
	    new_value.compile(comp, Type.pointer_type);
	    code.emitPutField(FluidLetExp.valueField);
	  }
	else if (binding.isSimple ())
	  {
	    new_value.compile (comp, binding.getType());
	    if (needValue)
	      code.emitDup(1, 0);  // dup
	    code.emitStore(binding.getVariable());
	  }
	else
	  {
	    Field field = binding.field;
            if (! field.getStaticFlag())
              binding.loadOwningObject(comp);
	    new_value.compile (comp, field.getType());
            if (field.getStaticFlag())
              code.emitPutStatic(field);
            else
              code.emitPutField(field);
	  }
      }
    else
      {
	comp.compileConstant (name);
	new_value.compile (comp, Target.pushObject);
	if (needValue)
	  code.emitDup(1, 1);  // dup_x1
	Method method;
	if (isDefining())
	  {
	    if (isFuncDef()
		&& comp.getInterpreter().hasSeparateFunctionNamespace())
	      method = comp.defineFunctionMethod;
	    else
	      method = comp.defineGlobalMethod;
	  }
	else
	  method = comp.putGlobalMethod;
	code.emitInvokeStatic(method);
      }

    if (! needValue)
      comp.compileConstant(Values.empty, target);
  }

  public final gnu.bytecode.Type getType()
  {
    return ! getHasValue() ? Type.void_type
      : binding == null ? Type.pointer_type : binding.getType();
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
