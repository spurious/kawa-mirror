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
    name = decl.sym;
    new_value = val;

    if ("%do%loop".equals(decl.symbol())
	&& val instanceof LambdaExp
	&& ! Compilation.usingCPStyle())
      ((LambdaExp) val).setInlineOnly(true);
  }

  public Object eval (Environment env)
  {
    Object new_val = new_value.eval (env);

    if (binding != null
        && ! (binding.context instanceof ModuleExp && ! binding.isPrivate()))
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

  static ClassType ctypeLocation = null;
  static Method setMethod = null;

  public void compile (Compilation comp, Target target)
  {
    if (new_value instanceof LambdaExp
	&& target instanceof IgnoreTarget
	&& ((LambdaExp) new_value).getInlineOnly())
      return;
    gnu.bytecode.CodeAttr code = comp.getCode();
    if (binding != null
        && ! (binding.context instanceof ModuleExp && ! binding.isPrivate()))
      {
	if (binding.ignorable())
	  new_value.compile (comp, Target.Ignore);
	else if (binding.isIndirectBinding() && ! isDefining())
	  {
	    binding.load(comp);
	    new_value.compile (comp, Target.pushObject);
	    if (ctypeLocation == null)
	      {
		ctypeLocation = ClassType.make("gnu.mapping.Location");
		setMethod = ctypeLocation.addMethod
                  ("set", Compilation.apply1args,
                   Type.void_type, Access.PUBLIC|Access.FINAL);
	      }
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
	    code.emitStore(binding);
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
            /*
            if (binding.context instanceof ModuleExp && ! binding.isPrivate())
              {
                comp.compileConstant (name);
                //comp.method.maybe_compile_checkcast (comp.scmSymbolType);
                new_value.compile (comp, Target.pushObject);
                code.emitInvokeStatic(isDefining () ? comp.defineGlobalMethod
                                      : comp.putGlobalMethod);
              }
            */
	  }
      }
    else
      {
	comp.compileConstant (name);
	//comp.method.maybe_compile_checkcast (comp.scmSymbolType);
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
