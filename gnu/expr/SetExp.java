// Copyright (c) 1999, 2001  Per M.A. Bothner.
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

  public final String getName() { return name; }

  /** Get the Expression for calculating the new ("right-hand") value. */
  public final Expression getNewValue() { return new_value; }

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
  }

  public Object eval (Environment env) throws Throwable
  {
    if (isSetIfUnbound())
      {
	Symbol binding = env.getSymbol(name);
	if (! binding.isBound())
	  binding.set(new_value.eval (env));
	if (getHasValue())
	  return name;
	else
	  return Interpreter.getInterpreter().noValue();
      }

    Object new_val = new_value.eval (env);

    if (binding != null
        && ! (binding.context instanceof ModuleExp))
      throw new Error ("internal error - SetExp.eval with lexical binding");
    if (isDefining ())
      {
	if (binding != null && binding.isAlias())
	  AliasConstraint.define(env.getSymbol(name),
				 (gnu.mapping.Location) new_val);
	else
	  env.define (name, new_val);
      }
    else
      {
	Symbol bind = env.lookup (name);
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
    Type type;
    gnu.bytecode.CodeAttr code = comp.getCode();
    // FIXME - handle isSetIfUnbound
    boolean needValue = getHasValue() && ! (target instanceof IgnoreTarget);

    // set the following to true if the value has been pushed.
    // this is used to detect not implemented cases.
    // when all cases are implemented, remove this.
    boolean valuePushed = false;

    // This code is kind of kludgy, because it handles a number of
    // different cases:  assignments and definitions to both local and
    // global variables.  Some of the complication is because we want
    // to generate fields for module-level definitions;  this is how
    // bindings are exported from modules.

    Object value;
    Declaration decl = binding;
    if (! decl.isPrivate()
	&& decl.context instanceof ModuleExp
	&& decl.getValue() instanceof LambdaExp
	&& ((LambdaExp) decl.getValue()).getName() != null // FIXME
	&& decl.getValue() == new_value)
      {
	((LambdaExp) new_value).compileSetField(comp);
      }
    else if (decl.context instanceof ModuleExp
	     && (new_value instanceof QuoteExp
		 || decl.getFlag(Declaration.IS_CONSTANT))
	     && isDefining()
	     && decl.getValue() != null)
      { // This is handled in ModuleExp's allocFields method.
	if (decl.getFlag(Declaration.IS_SYNTAX))
	  {
	    value = ((kawa.lang.Macro)  decl.getConstantValue()).expander;
	    if (value instanceof LambdaExp)
	      {
		LambdaExp expander = (LambdaExp) value;
		expander.flags |= LambdaExp.NO_FIELD;
		expander.compileAsMethod(comp);
		comp.mainLambda.addApplyMethod(expander);
		decl.makeField(comp, new_value);
	      }
	  }
	// Otherwise this is handled in ModuleExp's allocFields method.  But:
	if (needValue)
	  {
	    decl.load(comp);
	    valuePushed = true;
	  }
      }
    else
      {
	if (! isDefining())
	  decl = Declaration.followAliases(decl);
	if (decl.ignorable())
	  new_value.compile (comp, Target.Ignore);
	else if (decl.isAlias() && isDefining())
	  {
	    if (!( decl.getValue() instanceof ReferenceExp))
	      {
		decl.load(comp);
		new_value.compile (comp, Target.pushObject);
		Method meth = ClassType.make("gnu.mapping.AliasConstraint")
		  .getDeclaredMethod("define", 2);
		code.emitInvokeStatic(meth);
	      }
	  }
	else if (decl.isIndirectBinding()
		 && (! isDefining() || decl.isPublic()))
	  {
	    decl.load(comp);
	    new_value.compile (comp, Target.pushObject);
	    if (needValue)
	      {
		code.emitDupX();
		valuePushed = true;
	      }
	    if (setMethod == null)
	      setMethod = comp.typeLocation.addMethod
		("set", Compilation.apply1args,
		 Type.void_type, Access.PUBLIC|Access.FINAL);
	    code.emitInvokeVirtual (setMethod);
	  }
	else if (decl.isFluid())
	  {
	    decl.load(comp);
	    new_value.compile(comp, Type.pointer_type);
	    code.emitPutField(FluidLetExp.valueField);
	  }
	else if (decl.isSimple ())
	  {
            type = decl.getType();
	    new_value.compile (comp, type);
	    if (needValue)
              {
                code.emitDup(type);  // dup or dup2
                valuePushed = true;
              }
	    Variable var = decl.getVariable();
	    if (var == null)
	      var = decl.allocateVariable(code);
	    code.emitStore(var);
	  }
	else if (decl.context instanceof ClassExp && decl.field == null
		 && ! getFlag(PROCEDURE)
		 && ((ClassExp) decl.context).isMakingClassPair())
	  {
	    String setName = ClassExp.slotToMethodName("set", decl.getName());
	    ClassExp cl = (ClassExp) decl.context;
	    Method setter = cl.type.getDeclaredMethod(setName, 1);
	    cl.loadHeapFrame(comp);
	    new_value.compile(comp, decl.getType());
	    if (needValue)
	      {
		code.emitDupX();
		valuePushed = true;
	      }
	    code.emitInvoke(setter);
	  }
	else
	  {
	    Field field = decl.field;
            if (! field.getStaticFlag())
              decl.loadOwningObject(comp);
            type = field.getType();
	    new_value.compile (comp, type);
            if (field.getStaticFlag())
              {
                if (needValue)
                  {
                    code.emitDup(type);
                    valuePushed = true;
                  }
                code.emitPutStatic(field);
              }
            else
              { 
                if (needValue)
                  {
                    code.emitDupX();
                    valuePushed = true;
                  }
                code.emitPutField(field);
              }
	  }
      }

    if (needValue && ! valuePushed)
      throw new Error("SetExp.compile: not implemented - return value");

    if (needValue)
      target.compileFromStack(comp, getType());
    else
      comp.compileConstant(Values.empty, target);
  }

  public final gnu.bytecode.Type getType()
  {
    return ! getHasValue() ? Type.void_type
      : binding == null ? Type.pointer_type : binding.getType();
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkSetExp(this);
  }

  protected void walkChildren (ExpWalker walker)
  {
    new_value = (Expression) walker.walk(new_value);
  }

  public void print (OutPort out)
  {
    out.startLogicalBlock(isDefining () ? "(Define" : "(Set", ")", 2);
    out.writeSpaceFill();
    printLineColumn(out);
    if (binding == null || name.toString() != binding.getName())
      {
	out.print('/');
	SFormat.print (name, out);
      }
    if (binding != null)
      {
	out.print('/');
	out.print(binding);
      }
    out.writeSpaceLinear();
    new_value.print(out);
    out.endLogicalBlock(")");
  }
}
