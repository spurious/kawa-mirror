package gnu.kawa.functions;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.mapping.*;
import gnu.kawa.reflect.CompileReflect;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.reflect.ArrayGet;
import gnu.kawa.reflect.ArraySet;
import gnu.math.*;
import gnu.text.Char;

/** Various static methods used to inline and compile specific procedures.
 * They are separate from the Procedure classes they apply to in order
 * to reduce the size of kawart.jar.
 */

public class CompilationHelpers
{
  private static boolean nonNumeric(Expression exp)
  {
    if (exp instanceof QuoteExp)
      {
        Object value = ((QuoteExp) exp).getValue();
        return ! (value instanceof Numeric || value instanceof Char
                  || value instanceof Symbol);
      }
    return false;
  }

  static final ClassType typeList
  /* #ifdef JAVA2 */
  = ClassType.make("java.util.List");
  /* #else */
  // = ClassType.make("gnu.lists.Sequence");
  /* #endif */

  public static Expression validateApplyToArgs
  (ApplyExp exp, InlineCalls visitor,
   Type required, Procedure applyToArgs)
  {
    Expression[] args = exp.getArgs();
    int nargs = args.length - 1;
    if (nargs >= 0)
      {
        Expression proc = args[0];
        if (! proc.getFlag(Expression.VALIDATED))
          {
            if (proc instanceof LambdaExp)
              {
                Expression[] rargs = new Expression[nargs];
                System.arraycopy(args, 1, rargs, 0, nargs);
                return visitor.visit(new ApplyExp(proc, rargs).setLine(exp), required);
              }
            proc = visitor.visit(proc, null);
            args[0] = proc;
          }
        Type ptype = proc.getType().getRealType();
        Compilation comp = visitor.getCompilation();
        Language language = comp.getLanguage();
        if (ptype.isSubtype(Compilation.typeProcedure))
          {
            Expression[] rargs = new Expression[nargs];
            System.arraycopy(args, 1, rargs, 0, nargs);
            ApplyExp nexp = new ApplyExp(proc, rargs);
            nexp.setLine(exp);
            return proc.validateApply(nexp, visitor, required, null);
          }

        ClassType ctype;
        ApplyExp result = null;
        if (CompileReflect.checkKnownClass(ptype, comp) < 0)
          ; // This might be more cleanly handled at the type specifier. FIXME
        else if (ptype.isSubtype(Compilation.typeType)
                 || language.getTypeFor(proc,false) != null)
          {
            result = new ApplyExp(Invoke.make, args);
          }
        else if (ptype instanceof ArrayType)
          {
            Type elementType = ((ArrayType) ptype).getComponentType();
            result = new ApplyExp(new ArrayGet(elementType), args);
          }
        else if (ptype instanceof ClassType
                 && (ctype = (ClassType) ptype).isSubclass(typeList)
                 && nargs == 1)
          {
            // We search for a "get(int)" method, rather than just using
            // typeList.getDeclaredMethod("get", 1) to see if we make a
            // a virtual call rather than an interface call.
            Method get = ctype.getMethod("get", new Type[] { Type.intType  });
            result = new ApplyExp(get, args);
          }
        if (result != null)
          {
            result.setLine(exp);
            return ((InlineCalls) visitor).visitApplyOnly(result, required);
          }
      }
    exp.visitArgs(visitor);
    return exp;
  }

  static final ClassType setterType = ClassType.make("gnu.kawa.functions.Setter");
  static final Field setterField = setterType.getDeclaredField("setter");
  public static final Declaration setterDecl = new Declaration("setter", setterField);
  static { setterDecl.noteValue(new QuoteExp(Setter.setter)); }

  public static Expression validateSetter
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
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
            && (ctype = (ClassType) argType).isSubclass(typeList))
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

  public static Expression validateIsEqv
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    Expression[] args = exp.getArgs();
    if (nonNumeric(args[0]) || nonNumeric(args[1]))
      return new ApplyExp(((IsEqv) proc).isEq, args);
    return exp;
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

  public Expression validateApply (ApplyExp exp, InlineCalls visitor,
                                   Type required, Declaration decl)
  {
    exp.visitArgs(visitor);
    Expression[] args = exp.getArgs();
    if (args.length == 2)
      {
        Expression array = this.getArgs()[1];
        Expression[] xargs = new Expression[3];
        xargs[0] = array;
        xargs[1] = args[0];
        xargs[2] = args[1];
        ArraySet arrSetter = new ArraySet(elementType);
        return visitor.visitApplyOnly(new ApplyExp(arrSetter, xargs), required);
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

  public Expression validateApply (ApplyExp exp, InlineCalls visitor,
                                   Type required, Declaration decl)
  {
    exp.visitArgs(visitor);
    Expression[] args = exp.getArgs();
    if (args.length == 2)
      {
        Expression[] xargs = new Expression[4];
        xargs[0] = this.getArgs()[0];
        xargs[1] = QuoteExp.getInstance("set");
        xargs[2] = Compilation.makeCoercion(args[0], Type.intType);
        xargs[3] = args[1];
        Expression set
          = visitor.visitApplyOnly(new ApplyExp(Invoke.invoke, xargs), required);
        return Compilation.makeCoercion(set, Type.voidType);
      }
    return exp;
  }
}
