package gnu.kawa.functions;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.mapping.Procedure;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.reflect.ArraySet;

/** Implements Kawa extension function "setter", as in SRFI-17. */

public class Setter extends Procedure1 implements CanInline, HasSetter
{
  public static final Setter setter = new Setter();
  static { setter.setName("setter"); }

  public static Object setter (Procedure arg)
  {
    return arg.getSetter();
  }

  public Object apply1 (Object arg)
  {
    if (! (arg instanceof Procedure))
      {
        Class cl = arg.getClass();
        if (cl.isArray())
          return new SetArray(arg);
      }
    return ((Procedure)arg).getSetter();
  }

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression[] args = exp.getArgs();
    if (args.length == 1)
      {
        Expression arg = args[0];
        Type argType = arg.getType();
        if (argType instanceof ArrayType)
          {
            return new SetArrayExp(arg, (ArrayType) argType);
          }
        if (arg instanceof ReferenceExp)
          {
            Declaration decl = ((ReferenceExp) arg).getBinding();
            if (decl != null)
              arg = decl.getValue();
          }
        if (arg instanceof QuoteExp)
          {
            Object value = ((QuoteExp) arg).getValue();
            if (value instanceof Procedure)
              {
                Object setter = ((Procedure) value).getSetter();
                if (setter instanceof Procedure)
                  {
                    if (setter instanceof java.io.Externalizable)
                      return new QuoteExp(setter);
                    Declaration decl
                      = Declaration.getDeclaration((Procedure) setter);
                    if (decl != null)
                      return new ReferenceExp(decl);
                  }
              }
          }
      }
    return exp;
  }

  public void set1(Object arg1, Object value) throws Throwable
  {
    ((Procedure) arg1).setSetter((Procedure) value);
  }

  static final ClassType setterType = ClassType.make("gnu.kawa.functions.Setter");
  static final Field setterField = setterType.getDeclaredField("setter");
  public static final Declaration setterDecl = new Declaration("setter", setterField);
  static { setterDecl.noteValue(new QuoteExp(Setter.setter)); }

}

class SetArray extends Procedure2
{
  Object array;
  public SetArray(Object array)
  {
    this.array = array;
  }

  public Object apply2 (Object index, Object value)
  {
    // FIXME: value = element_type.coerceFromObject(value);
    java.lang.reflect.Array.set(array,
                                ((Number) index).intValue(),
                                value);
    return Values.empty;
  }
}

class SetArrayExp extends ApplyExp
{
  public static final ClassType typeSetArray
    = ClassType.make("gnu.kawa.functions.SetArray");

  Type elementType;

  public SetArrayExp (Expression array, ArrayType arrayType)
  {
    super(Invoke.make, new Expression[] { new QuoteExp(typeSetArray), array });
    elementType = arrayType.getComponentType();
  }

  public Expression inline (ApplyExp exp, InlineCalls walker, Declaration decl)
  {
    Expression[] args = exp.getArgs();
    if (args.length == 2)
      {
        Expression array = this.getArgs()[1];
        Expression[] xargs = new Expression[3];
        xargs[0] = array;
        xargs[1] = args[0];
        xargs[2] = args[1];
        ArraySet arrSetter = new ArraySet(elementType);
        return walker.walkApplyOnly(new ApplyExp(arrSetter, xargs));
      }
    return exp;
  }
}
