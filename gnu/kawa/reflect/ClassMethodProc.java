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

  public static final Method makeMethod
    = (ClassType.make("gnu.kawa.reflect.ClassMethodProc")
       .getDeclaredMethod("make", 2));
  public static final QuoteExp makeMethodExp
    = new QuoteExp(new PrimProcedure(makeMethod));

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

  public static ClassMethodProc make (ClassType ctype, String methodName)
  {
    ClassMethodProc p = new ClassMethodProc();
    p.ctype = ctype;
    p.methodName = methodName;
    return p;
  }

  public static ApplyExp makeExp (Expression clas, Expression member)
  {
    Expression[] args = { clas, member };
    ApplyExp aexp = new ApplyExp(ClassMethodProc.makeMethodExp, args);
    aexp.setFlag(ApplyExp.INLINE_IF_CONSTANT);
    return aexp;
  }

  public Object applyN (Object[] args)  throws Throwable
  {
    boolean isInstance = ctype == null;
    boolean isField = methodName.length() > 1 && methodName.charAt(0) == '.';
    boolean isNew = methodName.equals("new");
    boolean isInstanceOf = methodName.equals("nstance?");
    Object[] xargs
      = new Object[args.length+(isInstance||isNew||isInstanceOf?1:2)];
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
    if (! (func instanceof ApplyExp))
      return exp;
    ApplyExp fapp = (ApplyExp) func;
    Expression ffunc = fapp.getFunction();
    Expression[] fargs;
    if (ffunc != makeMethodExp
        || (fargs = fapp.getArgs()).length != 2
        || ! (fargs[1] instanceof QuoteExp))
      return exp;
    Expression clExp = fargs[0];
    boolean isInstance = clExp == QuoteExp.nullExp;
    Expression[] args = exp.getArgs();
    String mname = ((QuoteExp) fargs[1]).getValue().toString();
    boolean isInstanceOf = mname.equals("instance?");
    if (args.length == 0 && (isInstance || isInstanceOf))
      return exp;
    boolean isField = mname.length() > 1 && mname.charAt(0) == '.';
    boolean isNew = mname.equals("new");
    if (isField && ! isInstance && args.length == 1)
      {
        args = new Expression[] { Convert.makeCoercion(args[0], clExp) };
        isInstance = true;
      }
    Expression[] xargs
      = new Expression[args.length+(isInstance||isNew||isInstanceOf?1:2)];
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
        xargs[0] = fargs[0];
      }
    else if (isInstanceOf)
      {
        decl = instanceOfDecl;
        System.arraycopy(args, 1, xargs, 2, args.length-1);
        xargs[0] = args[0];
        xargs[1] = fargs[0];
      }
    else
      {
        decl = isField ? staticFieldDecl : invokeStaticDecl;
        System.arraycopy(args, 0, xargs, 2, args.length);
        xargs[0] = fargs[0];
      }
    if (! isNew && ! isInstanceOf)
      xargs[1] = isField ? new QuoteExp(mname.substring(1)) : fargs[1];
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
  }
}
