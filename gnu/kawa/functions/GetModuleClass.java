package gnu.kawa.functions;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

/** Special procedure to get the Class of the current module.
 * Since "current module" is defined by lexical scope,
 * this isn't a first-class procedure - it has to be inlined.
 */

public class GetModuleClass extends Procedure0
  implements Inlineable
{
  public static final GetModuleClass getModuleClass
    = new GetModuleClass();

  public Object apply0 ()
  {
    throw new Error("get-module-class must be inlined");
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    comp.loadClassRef(comp.mainClass);
    target.compileFromStack(comp, ClassType.make("java.lang.Class"));
  }

  public gnu.bytecode.Type getReturnType (Expression[] args)
  {
    return ClassType.make("java.lang.Class");
  }

  private static String CLASS_RESOURCE_NAME = "$class_resource_URI$";

  /** Return an expression that evaluates to a module-relative URI.
   * This has the Kawa-specific URI scheme "class-resource:" and an
   * assocatied ClassLoader (using a WekaHashMap).  It used to reference
   * resources located using the compiled class's ClassLoader. */
  public static Expression getModuleClassURI (Compilation comp)
  {
    Declaration decl = comp.mainLambda.lookup(CLASS_RESOURCE_NAME);
    if (decl == null)
      {
        comp.mustCompileHere();
        decl = new Declaration(CLASS_RESOURCE_NAME);
        decl.setFlag(Declaration.IS_CONSTANT|Declaration.STATIC_SPECIFIED);
        Method maker = ClassType.make("gnu.text.URI_utils")
          .getDeclaredMethod("makeClassResourceURI", 1);
        Expression clas
          = new ApplyExp(gnu.kawa.functions.GetModuleClass.getModuleClass,
                         Expression.noExpressions);
        decl.setValue(new ApplyExp(maker, new Expression[] { clas }));
        comp.mainLambda.add(null, decl);
      }
    return new ReferenceExp(decl);
  }
}
