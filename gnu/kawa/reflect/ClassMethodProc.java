package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.io.*;

/** A pairing of (class, method-name) treated as a procedure.
 * Equivalent to
 * <code>(lambda args (apply invoke-static ctype mname args))</code>.
 */

public class ClassMethodProc extends GenericProc
  implements Externalizable, CanInline
{
  ClassType ctype;
  String methodName;

  public static ClassMethodProc make (ClassType ctype, String methodName)
  {
    ClassMethodProc p = new ClassMethodProc();
    p.ctype = ctype;
    p.methodName = methodName;
    return p;
  }

  public MethodProc[] getMethods()
  {
    if (methods != null)
      return methods;

    String mname = methodName.equals("new") ? "<init>"
      : Compilation.mangleName(methodName);

    add(ClassMethods.getMethods(ctype, mname, 0, 0, null, Language.getDefaultLanguage()));
    return methods;
  }

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    String mname = methodName.equals("new") ? "<init>"
      : Compilation.mangleName(methodName);
    return Invoke.invokeStatic.inline(exp, mname, exp.getArgs().length, 0, -1,
				      ctype, walker);
  }

  public int numArgs()
  {
    if (methods == null)
      getMethods();
    return super.numArgs();
  }

  public int isApplicable(Type[] args)
  {
    if (methods == null)
      getMethods();
    return super.isApplicable(args);
  }

  public int match0 (CallContext ctx)
  {
    if (methods == null)
      getMethods();
    return super.match0(ctx);
  }

  public int match1 (Object arg1, CallContext ctx)
  {
    if (methods == null)
      getMethods();
    return super.match1(arg1, ctx);
  }

  public int match2 (Object arg1, Object arg2, CallContext ctx)
  {
    if (methods == null)
      getMethods();
    return super.match2(arg1, arg2, ctx);
  }

  public int match3 (Object arg1, Object arg2, Object arg3, CallContext ctx)
  {
    if (methods == null)
      getMethods();
    return super.match3(arg1, arg2, arg3, ctx);
  }

  public int match4 (Object arg1, Object arg2, Object arg3, Object arg4,
		     CallContext ctx)
  {
    if (methods == null)
      getMethods();
    return super.match4(arg1, arg2, arg3, arg4, ctx);
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
