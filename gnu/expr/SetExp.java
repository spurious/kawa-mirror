// Copyright (c) 1999, 2001, 2004, 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;
import gnu.mapping.Location; // As opposed to gnu.bytecode.Location
import gnu.bytecode.*;

/** An Expression to set (bind) or define a new value to a named variable.
 * @author	Per Bothner
 */

public class SetExp extends AccessExp
{
  /** The new value to assign to the variable. */
  Expression new_value;

  public SetExp (Object symbol, Expression val)
  { this.symbol = symbol;  new_value = val; }

  /** Get the Expression for calculating the new ("right-hand") value. */
  public final Expression getNewValue() { return new_value; }

  public static final int DEFINING_FLAG = NEXT_AVAIL_FLAG;
  public static final int GLOBAL_FLAG = NEXT_AVAIL_FLAG << 1;
  public static final int PREFER_BINDING2 = NEXT_AVAIL_FLAG << 2;
  public static final int PROCEDURE = NEXT_AVAIL_FLAG << 3;
  public static final int SET_IF_UNBOUND = NEXT_AVAIL_FLAG << 4;
  public static final int HAS_VALUE = NEXT_AVAIL_FLAG << 5;

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
    symbol = decl.getSymbol();
    new_value = val;
  }

  public Object eval (Environment env) throws Throwable
  {
    Symbol sym = symbol instanceof Symbol ? (Symbol) symbol
      : env.getSymbol(symbol.toString());
    Object property = null;
    Language language = Language.getDefaultLanguage();
    if (isFuncDef() && language.hasSeparateFunctionNamespace())
      property = EnvironmentKey.FUNCTION;
    if (binding != null
        && ! (binding.context instanceof ModuleExp))
      throw new Error ("internal error - SetExp.eval with lexical binding");

    if (isSetIfUnbound())
      {
	Location loc = env.getLocation(sym, property);
	if (! loc.isBound())
	  loc.set(new_value.eval (env));
	if (getHasValue())
	  return loc;
	else
	  return language.noValue();
      }

    Object new_val = new_value.eval (env);
    if (isDefining ())
      {
	/*
	if (binding != null && binding.isAlias())
	  env.addLocation(sym, null, (gnu.mapping.Location) new_val);
	else
	*/
	env.define(sym, property, new_val);
      }
    else
      {
	env.put(sym, property, new_val);
      }
    return getHasValue() ? new_val : language.noValue();
  }

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
    Expression declValue = decl.getValue();
    if (decl.getFlag(Declaration.EARLY_INIT)
	&& isDefining() && ! decl.ignorable())
      {
	BindingInitializer.create(decl, new_value, comp);
      }
    else if (declValue instanceof LambdaExp
	&& decl.context instanceof ModuleExp
	&& (! decl.isPrivate() || declValue instanceof ClassExp)
	&& ((LambdaExp) declValue).getName() != null // FIXME
	&& declValue == new_value)
      {
	((LambdaExp) new_value).compileSetField(comp);
      }
    else if (decl.context instanceof ModuleExp
	     && (new_value instanceof QuoteExp
                 || (decl.getFlag(Declaration.IS_CONSTANT) || decl.isAlias()))
             && isDefining()
             && declValue != null)
      { // This is handled in ModuleExp's allocFields method.  But:
        if (needValue)
          {
            decl.load(null, 0, comp, Target.pushObject);
	    valuePushed = true;
	  }
      }
    else
      {
        Declaration owner = null;
	if (! isDefining())
          {
            while (decl != null && decl.isAlias())
              {
                declValue = decl.getValue();
                if (! (declValue instanceof ReferenceExp))
                  break;
                ReferenceExp rexp = (ReferenceExp) declValue;
                Declaration orig = rexp.binding;
                if (orig == null)
                  break;
                if (owner != null && orig.needsContext())
                  break;
                owner = rexp.contextDecl();
                decl = orig;
              }
          }
	if (decl.ignorable())
	  new_value.compile (comp, Target.Ignore);
	else if (decl.isAlias() && isDefining())
	  {
            decl.load(null, ReferenceExp.DONT_DEREFERENCE,
                      comp, Target.pushObject);
	    ClassType locType
	      = ClassType.make("gnu.mapping.IndirectableLocation");
	    code.emitCheckcast(locType);
	    new_value.compile(comp, Target.pushObject);
	    Method meth = locType.getDeclaredMethod("setAlias", 1);
	    code.emitInvokeVirtual(meth);
	  }
	else if (decl.isIndirectBinding()
		 && (isSetIfUnbound() || ! isDefining() || decl.isPublic()))
	  {

            decl.load(owner, ReferenceExp.DONT_DEREFERENCE,
                      comp, Target.pushObject);
	    if (isSetIfUnbound())
	      {
		if (needValue)
		  {
		    code.emitDup();
		    valuePushed = true;
		  }
		code.pushScope();
		code.emitDup();
		Variable symLoc = code.addLocal(Compilation.typeLocation);
		code.emitStore(symLoc);
		code.emitInvokeVirtual(Compilation.typeLocation
				       .getDeclaredMethod("isBound", 0));
		code.emitIfIntEqZero();
		code.emitLoad(symLoc);
	      }
	    new_value.compile (comp, Target.pushObject);
	    if (needValue && ! isSetIfUnbound())
	      {
		code.emitDupX();
		valuePushed = true;
	      }
	    String setterName = "set";
	    code.emitInvokeVirtual(Compilation.typeLocation
				   .getDeclaredMethod(setterName, 1));
	    if (isSetIfUnbound())
	      {
		code.emitFi();
		code.popScope();
	      }
	  }
	else if (decl.isFluid())
	  {
            decl.load(null, ReferenceExp.DONT_DEREFERENCE,
                      comp, Target.pushObject);
	    new_value.compile(comp, Type.pointer_type);
	    code.emitInvokeVirtual(Compilation.typeLocation
				   .getDeclaredMethod("set", 1));
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
              decl.loadOwningObject(owner, comp);
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
    if (binding == null || symbol.toString() != binding.getName())
      {
	out.print('/');
	out.print(symbol);
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
