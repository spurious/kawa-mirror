package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

public class ClassExp extends LambdaExp
{
  /*
  public Object eval (Environment env)
  {
    System.err.println("eval ClassExp");
    Class clas = evalToClass();
    return Type.make(clas);
    }
  */

  /*
  public void compile (Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    ClassType new_class = compile (comp);
    String className = new_class.getName();
    // Type.make(Class.forname)

    ClassType typeClass = ClassType.make("java.lang.Class");
    Method forNameClassMethod
      = typeClass.addMethod("forName", comp.string1Arg,
                            typeClass, Access.STATIC|Access.PUBLIC);
    gnu.bytecode.CodeAttr code = comp.getCode();
    code.emitPushString(className);
    code.emitInvokeStatic(forNameClassMethod);

    ClassType typeType = ClassType.make("gnu.bytecode.Type");
    Type[]  argsClass = { typeClass };
    Method makeTypeMethod
      = typeType.addMethod("make", argsClass,
                           typeType, Access.STATIC|Access.PUBLIC);
    code.emitInvokeStatic(makeTypeMethod);

    target.compileFromStack(comp, typeType);
  }
  */
}
