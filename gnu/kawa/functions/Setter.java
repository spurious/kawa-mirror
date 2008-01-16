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
        /* #ifdef JAVA2 */
        if (arg instanceof java.util.List)
          return new SetList((java.util.List) arg);
        /* #else */
        // if (arg instanceof gnu.lists.Sequence)
        //   return new SetList((gnu.lists.Sequence) arg);
        /* #endif */
        Class cl = arg.getClass();
        if (cl.isArray())
          return new SetArray(arg, Language.getDefaultLanguage()/*FIXME*/);
      }
    return ((Procedure)arg).getSetter();
  }

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
    Expression[] args = exp.getArgs();
    if (args.length == 1)
      {
        Expression arg = args[0];
        Type argType = arg.getType();
        ClassType ctype;
        if (argType instanceof ArrayType)
          {
            return new SetArrayExp(arg, (ArrayType) argType);
          }
        if (argType instanceof ClassType
            && (ctype = (ClassType) argType).isSubclass(ApplyToArgs.typeList))
          {
            if (exp instanceof SetListExp)
              return exp;
            else
              return new SetListExp(exp.getFunction(), args);
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
  Type elementType;
  public SetArray (Object array, Language language)
  {
    Class elementClass = array.getClass().getComponentType();
    elementType = language.getTypeFor(elementClass);
    this.array = array;
  }

  public Object apply2 (Object index, Object value)
  {
    value = elementType.coerceFromObject(value);
    java.lang.reflect.Array.set(array,
                                ((Number) index).intValue(),
                                value);
    return Values.empty;
  }
}

class SetList extends Procedure2
{
  /* #ifdef JAVA2 */
  java.util.List list;
  public SetList (java.util.List list)
  {
    this.list = list;
  }
  /* #else */
  // gnu.lists.Sequence list;
  // public SetList (gnu.lists.Sequence list)
  // {
  //   this.list = list;
  // }
  /* #endif */
  Type elementType;

  public Object apply2 (Object index, Object value)
  {
    list.set(((Number) index).intValue(), value);
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

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            Declaration decl, boolean argsInlined)
  {
    if (! argsInlined)
      exp.walkArgs(walker);
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

class SetListExp extends ApplyExp
{
  public SetListExp (Expression func, Expression[] args)
  {
    super(func, args);
  }

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            Declaration decl, boolean argsInlined)
  {
    if (! argsInlined)
      exp.walkArgs(walker);
    Expression[] args = exp.getArgs();
    if (args.length == 2)
      {
        Expression[] xargs = new Expression[4];
        xargs[0] = this.getArgs()[0];
        xargs[1] = QuoteExp.getInstance("set");
        xargs[2] = Convert.makeCoercion(args[0], Type.int_type);
        xargs[3] = args[1];
        Expression set
          = walker.walkApplyOnly(new ApplyExp(Invoke.invoke, xargs));
        return Convert.makeCoercion(set, Type.void_type);
      }
    return exp;
  }
}

