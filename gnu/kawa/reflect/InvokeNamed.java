// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see COPYING.

package gnu.kawa.reflect;
import java.io.*;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

/** Procedure that invokes a specific named method.
 * new InvokeNamed("CLASS", "METHOD") is equivalent to the Scheme
 * expression (lambda ARGS (apply invoke-static "CLASS" "METHOD" ARGS)). */

public class InvokeNamed extends ProcedureN implements Externalizable
{
  ClassType methodClass;
  String methodName;

  public InvokeNamed (String className, String methodName)
  {
    super(className + ':' + methodName);
    methodClass = ClassType.make(className);
    this.methodName = methodName;
  }

  public Object applyN (Object[] args)  throws Throwable
  {
    String mname = Compilation.mangleName(methodName);
    MethodProc proc
      = ClassMethods.apply(methodClass, mname, null, null,0, 0);
    if (proc == null)
      throw new RuntimeException(getName() + ": no method named `" + mname
				 + "' in class " + methodClass.getName());
    return proc.applyN(args);

  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(methodClass);
    out.writeUTF(methodName);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    methodClass = (ClassType) in.readObject();
    methodName = in.readUTF();
  }
}
