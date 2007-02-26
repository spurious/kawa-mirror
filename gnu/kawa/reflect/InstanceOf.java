package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

public class InstanceOf extends Procedure2 implements CanInline, Inlineable
{
  protected Language language;

  public InstanceOf(Language language)
  {
    this.language = language;
  }

  public InstanceOf(Language language, String name)
  {
    this.language = language;
    setName(name);
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    Type type = language.asType(arg2);
    return language.booleanObject(type.isInstance(arg1));
  }

  static gnu.bytecode.ClassType typeType;
  static gnu.bytecode.Method instanceMethod;

  public Expression inline (ApplyExp exp, InlineCalls walker)
  {
    exp.walkArgs(walker);
    exp = Invoke.inlineClassName(exp, 1, walker);
    Expression[] args = exp.getArgs();
    if (args.length == 2)
      {
        Expression value = args[0];
        Expression texp = args[1];
        if (texp instanceof QuoteExp)
          {
            Object t = ((QuoteExp) texp).getValue();
            if (t instanceof Type)
              {
                Type type = (Type) t;
                if (value instanceof QuoteExp)
                  return type.isInstance(((QuoteExp) value).getValue())
                    ? QuoteExp.trueExp : QuoteExp.falseExp;
                if (! value.side_effects())
                  {
                    int comp = type.compare(value.getType());
                    if (comp == 1 || comp == 0)
                      return QuoteExp.trueExp;
                    if (comp == -3)
                      return QuoteExp.falseExp;
                  }
              }
          }
      }
    return exp;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    CodeAttr code = comp.getCode();
    Type type = null;
    Expression typeArg = args[1];
    if (typeArg instanceof QuoteExp)
      {
        try
          {
            type = language.asType(((QuoteExp) typeArg).getValue());
          }
        catch (Exception ex)
          {
            comp.error('w', "unknown type spec: "+type);
          }
      }
    else 
      type = language.getTypeFor(typeArg);
    if (type != null)
      {
	args[0].compile(comp, Target.pushObject);
	if (type instanceof TypeValue)
	  {
	    ((TypeValue) type).emitIsInstance(null, comp, target);
	    return;
	  }
	else
	  type.emitIsInstance(code);
	comp.usedClass(type);
      }
    else
      {
	if (typeType == null)
	  {
	    typeType = ClassType.make("gnu.bytecode.Type");
	    instanceMethod = typeType.addMethod("isInstance",
						Compilation.apply1args,
						Type.boolean_type,
						gnu.bytecode.Access.PUBLIC);
	  }
	args[1].compile(comp, typeType);
	args[0].compile(comp, Target.pushObject);
	code.emitInvokeVirtual(instanceMethod);
      }
    target.compileFromStack(comp, language.getTypeFor(Boolean.TYPE));
  }

  public Type getReturnType (Expression[] args)
  {
    return language.getTypeFor(Boolean.TYPE);
  }

  public static void emitIsInstance(TypeValue type, Variable incoming,
				    Compilation comp, Target target)
  {
    CodeAttr code = comp.getCode();
    type.emitTestIf(null, null, comp);
    ConditionalTarget cond = null;
    if (target instanceof ConditionalTarget)
      {
	cond = (ConditionalTarget) target;
	code.emitGoto(cond.ifTrue);
      }
    else
      code.emitPushInt(1);
    code.emitElse();
    if (cond != null)
      code.emitGoto(cond.ifFalse);
    else
      code.emitPushInt(0);
    code.emitFi();
    if (cond == null)
      target.compileFromStack(comp,
			      comp.getLanguage().getTypeFor(Boolean.TYPE));
  }
}
