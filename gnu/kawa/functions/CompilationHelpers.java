package gnu.kawa.functions;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.mapping.*;
import gnu.kawa.lispexpr.LangPrimType;
import gnu.kawa.reflect.CompileReflect;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.reflect.ArrayGet;
import gnu.kawa.reflect.ArraySet;
import gnu.kawa.reflect.LazyType;
import gnu.math.*;
import gnu.text.Char;

/** Various static methods used to inline and compile specific procedures.
 * They are separate from the Procedure classes they apply to in order
 * to reduce the size of kawart.jar.
 */

public class CompilationHelpers
{
    public static boolean maybeLazy(Expression exp) {
        if (exp instanceof QuoteExp)
            return false;
        return LazyType.maybeLazy(exp.getType());
    }

  private static boolean nonNumeric(Expression exp)
  {
    if (exp instanceof QuoteExp)
      {
        Object value = ((QuoteExp) exp).getValue();
        return ! (value instanceof Number
                  || value instanceof Boolean
                  || value instanceof Char
                  || value instanceof Symbol);
      }
    return false;
  }

    public static Expression validateApplyToArgs
        (ApplyExp exp, InlineCalls visitor,
         Type required, Procedure applyToArgs) {
        Expression[] args = exp.getArgs();
        int nargs = args.length - 1;
        if (nargs >= 0) {
        Expression proc = args[0];
        if (! proc.getFlag(Expression.VALIDATED))
          {
            Expression pval = proc;
            if (proc instanceof ReferenceExp)
              {
                Declaration decl = ((ReferenceExp) proc).getBinding();
                if (decl != null)
                  pval = decl.getValue();
              }
            if (pval != null && pval.getClass() == LambdaExp.class
                && exp.firstSpliceArg < 0 /* for now */)
              {
                Expression[] rargs = new Expression[nargs];
                System.arraycopy(args, 1, rargs, 0, nargs);
                exp.setFuncArgs(proc, rargs);
                return visitor.visit(exp, required);
              }
            proc = visitor.visit(proc, InlineCalls.typeForCalledFunction(proc));
            args[0] = proc;
          }
        Type ptype = proc.getType().getRealType();
        Compilation comp = visitor.getCompilation();
        Language language = comp.getLanguage();
        if (ptype.isSubtype(Compilation.typeProcedure))
          {
            Expression[] rargs = new Expression[nargs];
            System.arraycopy(args, 1, rargs, 0, nargs);
            exp.setFuncArgs(proc, rargs);
            exp.adjustSplice(exp, -1);
            return proc.validateApply(exp, visitor, required, null);
          }

        ClassType ctype = ptype instanceof ClassType ? (ClassType) ptype
	    : ptype instanceof ParameterizedType
	    ? ((ParameterizedType) ptype).getRawType()
	    : null;
        ApplyExp result = null;
        if (CompileReflect.checkKnownClass(ptype, comp) < 0)
          ; // This might be more cleanly handled at the type specifier. FIXME
        else if (ptype.isSubtype(Compilation.typeType)
                 || language.getTypeFor(proc,false) != null)
          {
            result = exp.setFuncArgs(Invoke.make, args);
          }
        else if (ptype instanceof ArrayType)
          {
            Type elementType = ((ArrayType) ptype).getComponentType();
            exp.setFuncArgs(new ArrayGet(elementType), args);
            result = exp;
          }
        else if (ctype != null
		 && ctype.isSubclass(Compilation.typeCharSequence)
                 && nargs == 1)
          {
              Method method = ClassType.make("gnu.lists.Strings")
                  .getDeclaredMethod("characterAt", 2);
              PrimProcedure prproc =
                  new PrimProcedure(method, LangPrimType.characterType, null);
              result = exp.setFuncArgs(prproc, args);
          }
        else if (ctype != null
		 && ctype.isSubclass(Compilation.typeList)
                 && nargs == 1)
          {
            String mname = "get";
            Type retType = null;
            String cname = ctype.getName();
            if (cname.startsWith("gnu.lists.")) {
                switch (cname.charAt(10)) {
                case 'U':
                    if ("gnu.lists.U64Vector".equals(cname)) {
                        mname = "longAt";
                        retType = LangPrimType.unsignedLongType;
                    }
                    else if ("gnu.lists.U32Vector".equals(cname)) {
                        mname = "intAt";
                        retType = LangPrimType.unsignedIntType;
                    }
                    else if ("gnu.lists.U16Vector".equals(cname)) {
                        mname = "shortAt";
                        retType = LangPrimType.unsignedShortType;
                    }
                    else if ("gnu.lists.U8Vector".equals(cname)) {
                        mname = "byteAt";
                        retType = LangPrimType.unsignedByteType;
                    }
                    break;
                case 'S':
                    if ("gnu.lists.S64Vector".equals(cname)) {
                        mname = "longAt";
                        retType = LangPrimType.longType;
                    }
                    else if ("gnu.lists.S32Vector".equals(cname)) {
                        mname = "intAt";
                        retType = LangPrimType.intType;
                    }
                    else if ("gnu.lists.S16Vector".equals(cname)) {
                        mname = "shortAt";
                        retType = LangPrimType.shortType;
                    }
                    else if ("gnu.lists.S8Vector".equals(cname)) {
                        mname = "byteAt";
                        retType = LangPrimType.byteType;
                    }
                    break;
                case 'F':
                    if ("gnu.lists.F64Vector".equals(cname)) {
                        mname = "doubleAt";
                        retType = LangPrimType.doubleType;
                    }
                    else if ("gnu.lists.F32Vector".equals(cname)) {
                        mname = "floatAt";
                        retType = LangPrimType.floatType;
                    }
                    break;
                }
            }
            
            // We search for a "get(int)" method, rather than just using
            // typeList.getDeclaredMethod("get", 1) to see if we make a
            // a virtual call rather than an interface call.
            Method get = ctype.getMethod(mname, new Type[] { Type.intType  });
	    ParameterizedType prtype = ptype instanceof ParameterizedType ?
		((ParameterizedType) ptype) : null;
            PrimProcedure prproc =
                new PrimProcedure(get, 'V', language, prtype);
            if (retType != null)
                prproc.setReturnType(retType);
            result = exp.setFuncArgs(prproc, args);
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
            && (ctype = (ClassType) argType).isSubclass(Compilation.typeList))
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
                Procedure setter;
                Procedure pvalue = (Procedure) value;
                try
                  {
                    setter = pvalue.getSetter();
                  }
                catch (RuntimeException ex)
                  {
                    setter = null;
                    visitor.getCompilation().error('w', "procedure '"+pvalue.getName()+"' has no setter");
                  }
                if (setter != null)
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
        if (argType instanceof ClassType
            && ((ClassType) argType).isSubclass(Compilation.typeProcedure)) {
            return new ApplyExp(getSetterProc, args);
        }
      }
    return exp;
  }

    public static final PrimProcedure getSetterProc = new PrimProcedure
      (Compilation.typeProcedure.getDeclaredMethod("getSetter", 0));

  public static Expression validateIsEqv
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    Expression[] args = exp.getArgs();
    if ((nonNumeric(args[0]) || nonNumeric(args[1]))
        && ! maybeLazy(args[0]) && ! maybeLazy(args[1]))
      return new ApplyExp(((IsEqv) proc).isEq, args);
    Method meth =
        ClassType.make("gnu.kawa.functions.IsEqv")
        .getDeclaredMethod("apply", 2);
    return new ApplyExp(new PrimProcedure(meth, visitor.getLanguage()),
                        args).setLine(exp);
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
