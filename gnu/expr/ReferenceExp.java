// Copyright (c) 1999, 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

/**
 * This class represents a variable reference (an identifier).
 * @author	Per Bothner
 */

public class ReferenceExp extends AccessExp
{
  static int counter;
  /** Unique id number, to ease print-outs and debugging. */
  int id = ++counter;

  public static final int DONT_DEREFERENCE = NEXT_AVAIL_FLAG;
  public static final int PROCEDURE_NAME = NEXT_AVAIL_FLAG << 1;
  public static final int PREFER_BINDING2 = NEXT_AVAIL_FLAG << 2;
  /** Flag indicates a reference to a type name. */
  public static final int TYPE_NAME = NEXT_AVAIL_FLAG << 3;

  /* If true, must have binding.isIndirectBinding().  Don't dereference it. */
  public final boolean getDontDereference()
  {
    return (flags & DONT_DEREFERENCE) != 0;
  }

  public final void setDontDereference(boolean setting)
  { setFlag(setting, DONT_DEREFERENCE); }

  public final boolean isUnknown ()
  {
    return Declaration.isUnknown(binding);
  }

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
    this(binding.getSymbol(), binding);
  }

  protected boolean mustCompile () { return false; }

  public final Object valueIfConstant()
  {
    if (binding != null)
      {
        Expression dvalue = binding.getValue();
        if (dvalue != null)
          return dvalue.valueIfConstant();
      }
    return null;
  }

  public void apply (CallContext ctx)
    throws Throwable
  {
    Object value;
    if (binding != null && binding.isAlias() && ! getDontDereference()
        && binding.value instanceof ReferenceExp)
      {
        ReferenceExp rexp = (ReferenceExp) binding.value;
        if (rexp.getDontDereference() && rexp.binding != null)
          {
            Expression v = rexp.binding.getValue();
            if (v instanceof QuoteExp || v instanceof ReferenceExp
                || v instanceof LambdaExp)
              {
                v.apply(ctx);
                return;
              }
          }
        value = binding.value.eval(ctx);
      }
    else if (binding != null && binding.field != null
             && binding.field.getDeclaringClass().isExisting()
             && (! getDontDereference() || binding.isIndirectBinding()))
      {
        try
          {
            Object instance = binding.field.getStaticFlag() ? null
              : contextDecl().getValue().eval(ctx);
            value = binding.field.getReflectField().get(instance);
          }
        catch (Exception ex)
          {
            String msg = "exception evaluating "+symbol
              +" from "+binding.field+" - "+ex;
            // We abuse msg as a UnboundLocationException name.
            throw new UnboundLocationException(msg, this);
          }
      }
    // This isn't just an optimization - it's needed for evaluating procedural
    // macros (e.g. syntax-case) defined in a not-yet-compiled module.
    else if (binding != null
        && (binding.value instanceof QuoteExp
            || binding.value instanceof LambdaExp)
        && binding.value != QuoteExp.undefined_exp
        && (! getDontDereference() || binding.isIndirectBinding()))
      {
        value = binding.value.eval(ctx);
      }
    else if (binding == null
             || (binding.context instanceof ModuleExp
                && ! binding.isPrivate()))
      {
        Environment env = Environment.getCurrent();
        Symbol sym = symbol instanceof Symbol ? (Symbol) symbol
          : env.getSymbol(symbol.toString());
        Object property = getFlag(PREFER_BINDING2) && isProcedureName()
          ? EnvironmentKey.FUNCTION
          : null;
        if (getDontDereference())
          value = env.getLocation(sym, property);
        else
          {
            Object unb = gnu.mapping.Location.UNBOUND;
            value = env.get(sym, property, unb);
            if (value == unb)
              throw new UnboundLocationException(sym, this);
          }
        ctx.writeValue(value);
        return;
      }
    else
      value = ctx.evalFrames[ScopeExp.nesting(binding.context)][binding.evalIndex];
    if (! getDontDereference() && binding.isIndirectBinding())
      value = ((gnu.mapping.Location) value).get();
    ctx.writeValue(value);
  }

  public void compile (Compilation comp, Target target)
  {
    if (! (target instanceof ConsumerTarget)
        || ! ((ConsumerTarget) target).compileWrite(this, comp))
      binding.load(this, flags, comp, target);
  }

  protected Expression deepCopy (gnu.kawa.util.IdentityHashTable mapper)
  {
    Declaration d = (Declaration) mapper.get(binding, binding);
    Object s = mapper.get(symbol, symbol);
    ReferenceExp copy = new ReferenceExp(s, d);
    copy.flags = getFlags();
    return copy;
  }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitReferenceExp(this, d);
  }

  public Expression validateApply (ApplyExp exp, InlineCalls visitor,
                                   Type required, Declaration decl)
  {
    decl = this.binding; // We don't use the passed-in Declaration.
    if (decl != null && ! decl.getFlag(Declaration.IS_UNKNOWN))
      {
        decl = Declaration.followAliases(decl);
        if (! (decl.isIndirectBinding()))
          {
            Expression dval = decl.getValue();
            if (dval != null)
              return dval.validateApply(exp, visitor, required, decl);
          }
      }
    else if (getSymbol() instanceof Symbol)
      {
        Symbol symbol = (Symbol) getSymbol();
        Object fval = Environment.getCurrent().getFunction(symbol, null);
        if (fval instanceof Procedure)
          return new QuoteExp(fval).validateApply(exp, visitor, required, null);
      }
    exp.visitArgs(visitor);
    return exp;
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
    Declaration decl = binding;
    if (decl == null || decl.isFluid())
      return Type.pointer_type;
    if (getDontDereference())
      return Compilation.typeLocation;
    decl = Declaration.followAliases(decl);
    Type type = decl.getType();
    if (type == null || type == Type.pointer_type)
      {
        Expression value = decl.getValue();
        if (value != null && value != QuoteExp.undefined_exp)
          {
            // Kludge to guard against cycles.
            // Not verified if it is really needed, but just in case ...
            Expression save = decl.value;
            decl.value = null;
            type = value.getType();
            decl.value = save;
          }
      }
    if (type == Type.toStringType)
      type = Type.javalangStringType;
    return type;
  }

  public boolean isSingleValue()
  {
    if (binding != null && binding.getFlag(Declaration.IS_SINGLE_VALUE))
      return true;
    return super.isSingleValue();
  }

  public boolean side_effects ()
  {
    return binding == null || ! binding.isLexical();
  }

  public String toString()
  {
    return "RefExp/"+symbol+'/'+id+'/';
  }
}
