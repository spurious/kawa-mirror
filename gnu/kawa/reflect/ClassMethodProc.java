package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.io.*;
import gnu.kawa.functions.Convert;

/** A pairing of (class, method-name) treated as a procedure.
 * Usually equivalent to
 * <code>(lambda args (apply invoke-static ctype mname args))</code>.
 * However, if the class is null, corresponds to <code>invoke</code>,
 * and if <code>method-name</code> starts with a period,
 * corresponds to <code>static-field</code> or <code>field</code>.
 */

public class ClassMethodProc extends ProcedureN
  implements Externalizable, HasSetter
{
  ClassType ctype;
  String methodName;

  /*
   * 'N' - new (make) - if methodName is "new".
   * 'I' - instance of - if methodName is INSTANCEOF_METHOD_NAME.
   * 'C' - cast - if methodName is CAST_METHOD_NAME.
   * 'F' - get field
   */
  char kind;

  void fixup ()
  {
    if (methodName.equals("new"))
      kind = 'N';
    else if (methodName.equals(INSTANCEOF_METHOD_NAME))
      kind = 'I';
    else if (methodName.equals(CAST_METHOD_NAME))
      kind = 'C';
    else if (methodName.length() > 1 && methodName.charAt(0) == '.')
      kind = 'F';
  }

  /** PREFIX:<> is equivalent to the ClassType bound to PREFIX. */
  public static final String CLASSTYPE_FOR = "<>";

  /** Pseudo-method-name for the cast operation. */
  public static final String CAST_METHOD_NAME = "@";

  /** Pseudo-method-name for class-membership-test (instanceof) operation. */
  public static final String INSTANCEOF_METHOD_NAME = "instance?";

  private static final Method makeMethod
    = (ClassType.make("gnu.kawa.reflect.ClassMethodProc")
       .getDeclaredMethod("make", 2));
  private static PrimProcedure makeProc = new PrimProcedure(makeMethod);
  public static final QuoteExp makeMethodExp
    = new QuoteExp(makeProc);

  static final Declaration fieldDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.reflect.SlotGet", "field");

  static final Declaration staticFieldDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.reflect.SlotGet", "staticField");

  static final Declaration makeDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.reflect.Invoke", "make");

  static final Declaration invokeDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.reflect.Invoke", "invoke");

  static final Declaration invokeStaticDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.reflect.Invoke", "invokeStatic");

  static final Declaration instanceOfDecl
  = Declaration.getDeclarationFromStatic("kawa.standard.Scheme", "instanceOf");

  static final Declaration castDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.functions.Convert", "as");

  public static ClassMethodProc make (ClassType ctype, String methodName)
  {
    ClassMethodProc p = new ClassMethodProc();
    if (ctype == null)
      new Error("ClassMethodProc.make ctype:"+ctype+" mname:"+methodName);
    p.ctype = ctype;
    p.methodName = methodName;
    p.fixup();
    return p;
  }

  public static ApplyExp makeExp (Expression clas, Expression member)
  {
    //return gnu.kawa.functions.GetNamedPart.makeExp(clas, member);
    Expression[] args = { clas, member };
    //return new ApplyExp(gnu.kawa.functions.GetNamedPart.getNamedPart, args);
    ApplyExp aexp = new ClassMethodExp(args);
    //    aexp.setFlag(ApplyExp.INLINE_IF_CONSTANT);
    return aexp;
  }

  public static Expression makeExp (Expression clas, String member)
  {
    if (CLASSTYPE_FOR.equals(member))
      return clas;
    //return gnu.kawa.functions.GetNamedPart.makeExp(clas, new QuoteExp(member));
    return makeExp(clas, new QuoteExp(member));
    /*
    Expression[] args = { clas, new QuoteExp(member) };
    ApplyExp aexp = new ClassMethodExp(args);
    //aexp.setFlag(ApplyExp.INLINE_IF_CONSTANT);
    return aexp;
    */
  }

  public void apply (CallContext ctx) throws Throwable
  {
    Object[] args = ctx.getArgs();
    if (kind != 0)
      {
        Object result = applyN(args);
        ctx.writeValue(result);
      }
    else
      {
        int thisCount = ctype==null ? 1 : 0;
        Object[] xargs = new Object[args.length+2-thisCount];
        Invoke proc;
        String name = methodName;
        if (ctype == null)
          {
            proc = Invoke.invoke;
            xargs[0] = args[0];
          }
        else
          {
            proc = Invoke.invokeStatic;
            xargs[0] = ctype;
          }
        xargs[1] = name;
        System.arraycopy(args, thisCount, xargs, 2, args.length - thisCount);
        proc.apply(xargs, ctx);
      }
  }

  public Object applyN (Object[] args)  throws Throwable
  {
    boolean isInstance = ctype == null;
    boolean isField = kind == 'F';
    boolean isNew = kind == 'N';
    boolean isInstanceOf = kind == 'I';
    boolean isCast = kind == 'C';
    Object[] xargs
      = new Object[args.length+(isInstance||isNew||isInstanceOf||isCast?1:2)];
    Procedure proc;
    String name = isField ? methodName.substring(1) : methodName;
    if (isField && ! isInstance && args.length == 1)
      return SlotGet.field.apply2(ctype.coerceFromObject(args[0]), name);
    if (isInstance)
      {
        if (isField)
          proc = SlotGet.field;
        else
          proc = Invoke.invoke;
        System.arraycopy(args, 1, xargs, 2, args.length-1);
        xargs[0] = args[0];
        xargs[1] = name;
      }
    else if (isNew)
      {
        proc = Invoke.make;
        System.arraycopy(args, 0, xargs, 1, args.length);
        xargs[0] = ctype;
      }
    else if (isInstanceOf)
      {
        proc = kawa.standard.Scheme.instanceOf;
        System.arraycopy(args, 1, xargs, 2, args.length-1);
        xargs[0] = args[0];
        xargs[1] = ctype;
      }
    else if (isCast)
      {
        proc = gnu.kawa.functions.Convert.as;
        System.arraycopy(args, 1, xargs, 2, args.length-1);
        xargs[0] = ctype;
        xargs[1] = args[0];
      }
    else
      {
        if (isField)
          proc = SlotGet.staticField;
        else
          proc = Invoke.invokeStatic;
        System.arraycopy(args, 0, xargs, 2, args.length);
        xargs[0] = ctype;
        xargs[1] = name;
      }
    return proc.applyN(xargs);
  }

  public void setN (Object[] args) throws Throwable
  {
    boolean isInstance = ctype == null;
    boolean isField = methodName.length() > 1 && methodName.charAt(0) == '.';
    if (! isField)
      throw new Error("invalid setter for method invokcation "+this);
    String mname = methodName.substring(1);
    if (isInstance)
      {
        if (args.length != 2)
          throw new WrongArguments(this, args.length);
        SlotSet.apply(false, args[0], mname, args[1]);
      }
    else
      {
        if (args.length == 1)
          SlotSet.apply(true, ctype, mname, args[0]);
        else if (args.length == 2)
          SlotSet.apply(false, ctype.coerceFromObject(args[0]), mname, args[1]);
        else
          throw new WrongArguments(this, args.length);
      }
  }

  /** Rewrite/optimize ((ClassMethodProc:make CLASS "METHOD") args). */
  public static ApplyExp rewrite (ApplyExp exp)
  {
    Expression func = exp.getFunction();
    if (func instanceof ReferenceExp)
      {
        Declaration fdecl = ((ReferenceExp) func).getBinding();
        if (fdecl != null)
          func = fdecl.getValue();
      }
    Object fvalue;
    Expression clExp;
    String mname;
    if (func instanceof ApplyExp)
      {
        ApplyExp fapp = (ApplyExp) func;
        Expression ffunc = fapp.getFunction();
        Expression[] fargs;
        if (ffunc != makeMethodExp
            || (fargs = fapp.getArgs()).length != 2
            || ! (fargs[1] instanceof QuoteExp))
          return exp;
        clExp = fargs[0];
        mname = ((QuoteExp) fargs[1]).getValue().toString();
      }
    else if (func instanceof QuoteExp
             && (fvalue = ((QuoteExp) func).getValue()) instanceof ClassMethodProc)
      {
        ClassMethodProc cmProc = (ClassMethodProc) fvalue;
        clExp = QuoteExp.getInstance(cmProc.ctype);
        mname = cmProc.methodName;
      }
    else
      return exp;
    boolean isInstance = clExp == QuoteExp.nullExp;
    Expression[] args = exp.getArgs();
    boolean isInstanceOf = mname.equals(INSTANCEOF_METHOD_NAME);
    boolean isCast = mname.equals(CAST_METHOD_NAME);
    if (args.length == 0 && (isInstance || isInstanceOf || isCast))
      return exp;
    boolean isField = mname.length() > 1 && mname.charAt(0) == '.';
    boolean isNew = mname.equals("new");
    if (isField && ! isInstance && args.length == 1)
      {
        args = new Expression[] { Convert.makeCoercion(args[0], clExp) };
        isInstance = true;
      }
    Expression[] xargs
      = new Expression[args.length+(isInstance||isNew||isInstanceOf||isCast?1:2)];
    Declaration decl;
    if (isInstance)
      {
        decl = isField ? fieldDecl : invokeDecl;
        System.arraycopy(args, 1, xargs, 2, args.length-1);
        xargs[0] = args[0];
      }
    else if (isNew)
      {
        decl = makeDecl;
        System.arraycopy(args, 0, xargs, 1, args.length);
        xargs[0] = clExp;
      }
    else if (isInstanceOf)
      {
        decl = instanceOfDecl;
        System.arraycopy(args, 1, xargs, 2, args.length-1);
        xargs[0] = args[0];
        xargs[1] = clExp;
      }
    else if (isCast)
      {
        decl = castDecl;
        System.arraycopy(args, 1, xargs, 2, args.length-1);
        xargs[0] = clExp;
        xargs[1] = args[0];
      }
    else
      {
        decl = isField ? staticFieldDecl : invokeStaticDecl;
        System.arraycopy(args, 0, xargs, 2, args.length);
        xargs[0] = clExp;
      }
    if (! isNew && ! isInstanceOf && ! isCast)
      xargs[1] = new QuoteExp(isField ? mname.substring(1) : mname);
    return new ApplyExp(new ReferenceExp(decl), xargs);
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(ctype);
    out.writeUTF(methodName);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    ctype = (ClassType) in.readObject();
    methodName = in.readUTF();
    fixup();
  }

  public String toString()
  {
    StringBuffer sbuf = new StringBuffer("#<class-method ");
    if (ctype != null)
      sbuf.append(ctype.getName());
    sbuf.append(' ');
    sbuf.append(methodName);
    sbuf.append('>');
    return sbuf.toString();
  }
}

class ClassMethodExp extends ApplyExp
{
  public ClassMethodExp (Expression[] args)
  {
    super(ClassMethodProc.makeMethodExp, args);
  }

  public Expression inline (ApplyExp exp, ExpWalker walker, Declaration decl)
  {
    System.err.println("inline ClassMethodExp");
    return exp;
  }
}
