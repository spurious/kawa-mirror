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
  Object symbol;
  Declaration binding;
  public String string_name () { return symbol.toString(); }

  public final String getName()
  {
    return symbol instanceof Symbol ? ((Symbol) symbol).getName()
      : symbol.toString();
  }
  public final Object getSymbol() { return symbol; }
  /** If non-null, the local Declaration this refers to. */
  public final Declaration getBinding() { return binding; }

  public final void setBinding(Declaration decl) { binding = decl; }

  static int counter;
  /** Unique id number, to ease print-outs and debugging. */
  int id = ++counter;

  private static int DONT_DEREFERENCE = NEXT_AVAIL_FLAG;
  private static int PROCEDURE_NAME = NEXT_AVAIL_FLAG << 1;
  public static int PREFER_BINDING2 = NEXT_AVAIL_FLAG << 2;

  /* If true, must have binding.isIndirectBinding().  Don't dereference it. */
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

  public ReferenceExp (Object symbol)
  {
    this.symbol = symbol;
  }

  public ReferenceExp (Object symbol, Declaration binding)
  {
    this.symbol = symbol;
    this.binding = binding;
  }

  public ReferenceExp (Declaration binding)
  {
    this.binding = binding;
    this.symbol = binding.getSymbol();
  }

  public Object eval (Environment env)
  {
    if (binding != null)
      {
        if (binding.field != null && binding.field.getStaticFlag())
          {
            try
              {
                Object value = binding.field.getReflectField().get(null);
                if (! (value instanceof Symbol))
                  return value;
                // otherwise not implemented!
              }
            catch (Exception ex)
              {
              }
          }
        if ( ! (binding.context instanceof ModuleExp && ! binding.isPrivate()))
          throw new Error("internal error: ReferenceExp.eval on lexical binding");
      }
    Symbol sym = symbol instanceof Symbol ? (Symbol) symbol
      : env.getSymbol(symbol.toString());
    Object property = getFlag(PREFER_BINDING2) && isProcedureName()
      ? EnvironmentKey.FUNCTION
      : null;
    if (getDontDereference())
      return env.getLocation(sym, property);
    Object unb = gnu.mapping.Location.UNBOUND;
    Object val = env.get(sym, property, unb);
    if (val == unb)
      throw new UnboundLocationException(sym);
    return val;
  }

  public void compile (Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    Type rtype = getType();
    CodeAttr code = comp.getCode();
    Declaration decl = Declaration.followAliases(binding);
    if (! decl.isIndirectBinding() && getDontDereference())
      {
	if (decl.field == null)
	  throw new Error("internal error: cannot take location of "+decl);
	Method meth;
	if (decl.field.getStaticFlag())
	  {
	    ClassType typeStaticFieldLocation
	      = ClassType.make("gnu.kawa.reflect.StaticFieldLocation");
	    meth = typeStaticFieldLocation.getDeclaredMethod("make", 2);
	  }
	else
	  {
	    ClassType typeClassMemberLocation
	      = ClassType.make("gnu.kawa.reflect.ClassMemberLocation");
	    meth = typeClassMemberLocation.getDeclaredMethod("make", 3);
	    decl.base.load(comp);
	  }
	comp.compileConstant(decl.field.getDeclaringClass().getName());
	comp.compileConstant(decl.field.getName());
	code.emitInvokeStatic(meth);
      }
    else
      {
	decl.load(comp);
	if (decl.isIndirectBinding() && ! getDontDereference())
	  {
	    code.emitInvokeVirtual(Compilation.getLocationMethod);
	    rtype = Type.pointer_type;
	  }
	/*
	else if (decl.isFluid() && decl.field == null)
	  code.emitGetField(FluidLetExp.valueField);
	*/
      }
    if (target instanceof SeriesTarget
	&& decl.getFlag(Declaration.IS_SINGLE_VALUE))
      // A kludge until we get a better type system.
      ((SeriesTarget) target).compileFromStackSimple(comp, rtype);
    else
      target.compileFromStack(comp, rtype);
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkReferenceExp(this);
  }

  public void print (OutPort ps)
  {
    ps.print("(Ref/");
    ps.print(id);
    if (symbol != null
	&& (binding == null || symbol.toString() != binding.getName()))
      {
	ps.print('/');
	ps.print(symbol);
      }
    if (binding != null)
      {
	ps.print('/');
	ps.print(binding);
      }
    ps.print(")");
  }

  public gnu.bytecode.Type getType()
  {
    return (binding == null || binding.isFluid()) ? Type.pointer_type
      : getDontDereference() ? Compilation.typeLocation
      : Declaration.followAliases(binding).getType();
  }

  public String toString()
  {
    return "RefExp/"+symbol+'/'+id+'/';
  }
}
