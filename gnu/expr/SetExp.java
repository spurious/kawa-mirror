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
    name = decl.getName();
    new_value = val;

    if ("%do%loop".equals(decl.getName())
	&& val instanceof LambdaExp
	&& ! Compilation.usingCPStyle())
      ((LambdaExp) val).setInlineOnly(true);
  }

  public Object eval (Environment env)
  {
    Object new_val = new_value.eval (env);

    if (binding != null
        && ! (binding.isStatic() && ! binding.isPrivate()))
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

  static Method setMethod = null;

  public void compile (Compilation comp, Target target)
  {
    if (new_value instanceof LambdaExp
	&& target instanceof IgnoreTarget
	&& ((LambdaExp) new_value).getInlineOnly())
      return;
    gnu.bytecode.CodeAttr code = comp.getCode();

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
	code.emitInvokeStatic(isDefining () ? comp.defineGlobalMethod
			      : comp.putGlobalMethod);
      }

    comp.compileConstant(Values.empty, target);
  }

  public final gnu.bytecode.Type getType()
  {
    return Type.void_type;
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
