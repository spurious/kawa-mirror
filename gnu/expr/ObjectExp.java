package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

public class ObjectExp extends LambdaExp
{
  /** List of base classes and implemented interfaces. */
  public Expression[] supers;

  public ObjectExp ()
  {
    type = Type.pointer_type;
  }

  public ClassType compile (Compilation comp)
  {
    ClassType saveClass = comp.curClass;
    Method saveMethod = comp.method;
    try
      {
	ClassType new_class = type;
	String name = getName();
	if (new_class == comp.scmProcedureType)
	  new_class = new ClassType(name == null? "object" : name);
	this.type = new_class;
	if (supers == null || supers.length == 0)
	  new_class.setSuper(Type.pointer_type);
	else
	  {
	    int i = 0;
	    ClassType type = (ClassType) ((QuoteExp) supers[0]).getValue();
	    if ((type.getModifiers() & Access.INTERFACE) == 0)
	      {
		new_class.setSuper(type);
		i++;
	      }
	    int skip = i;
	    int len = supers.length;
	    if (i < len)
	      {
		ClassType[] interfaces = new ClassType[len - i];
		for (;  i < len;  i++)
		  interfaces[i-skip]
		    = (ClassType) ((QuoteExp) supers[i]).getValue();
		new_class.setInterfaces(interfaces);
	      }
	  }
	//comp.addClass(new_class);
	comp.curClass = new_class;
	//allocChildClasses(comp);

	String filename = getFile();
	type = new_class;
	if (filename != null)
	  new_class.setSourceFile (filename);

	LambdaExp saveLambda = comp.curLambda;
	comp.curLambda = this;

	comp.generateConstructor (comp.curClass, this);

	CodeAttr code;

	for (Variable var = firstVar ();  var != null;  var = var.nextVar ())
	  {
	    Declaration decl = (Declaration) var;
	    decl.field = new_class.addField(decl.getName(), decl.getType(),
					    Access.PUBLIC);
	  }

	for (LambdaExp child = firstChild;  child != null; )
	  {
	    ClassType method_class;
	    boolean method_static;
	    int method_flags;
	    //method_class = heapFrameLambda.getCompiledClassType();
	    //method_class = (ClassType) heapFrame.getType();
	    method_class = type;
	    method_flags = Access.PUBLIC;
	    child.declareThis(method_class);

	    // generate_unique_name (method_class, child.getName());
	    String child_name = child.getName();
	    String method_name = comp.mangleName(child_name);
	    child.primMethod
	      = child.addMethodFor (method_class, method_name, method_flags);
	    child = child.nextSibling;
	  }

	for (LambdaExp child = firstChild;  child != null; )
	  {
	    Method save_method = comp.method;
	    LambdaExp save_lambda = comp.curLambda;
	    comp.method = child.primMethod;
	    comp.curClass = comp.method.getDeclaringClass();
	    comp.curLambda = child;
	    comp.method.initCode();
            child.allocChildClasses(comp);
	    child.allocParameters(comp, null);
	    child.enterFunction(comp, null);
	    Type rtype = child.primMethod.getReturnType();
	    Target target = rtype == Type.pointer_type ? Target.returnObject
	      : rtype == Type.void_type ? Target.Ignore
	      : new TailTarget(rtype);
	    child.body.compileWithPosition(comp, target);
	    if (comp.method.reachableHere ())
	      comp.getCode().emitReturn();
	    comp.method.popScope();
	    comp.method = save_method;
	    comp.curClass = new_class;
	    comp.curLambda = save_lambda;
	    child = child.nextSibling;
	  }

	comp.curLambda = saveLambda;

	return new_class;
      }
    finally
      {
	comp.curClass = saveClass;
	comp.method = saveMethod;
      }
  }

  /*
  public void compile (Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    Type rtype = compileAlloc (comp);
    target.compileFromStack(comp, rtype);
  }
  */
}
