package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

public class ObjectExp extends LambdaExp
{
  /** List of base classes and implemented interfaces. */
  public Expression[] supers;

  public ObjectExp ()
  {
    type = null;
  }

  public String getJavaName ()
  {
    return name == null ? "object" : Compilation.mangleName (name);
  }

  public ClassType getCompiledClassType()
  {
    if (type == null)
      {
	String name = getName();
	type = new ClassType(name == null? "object" : name);
	if (supers == null || supers.length == 0)
	  type.setSuper(Type.pointer_type);
	else
	  {
	    int i = 0;
	    ClassType type = (ClassType) ((QuoteExp) supers[0]).getValue();
	    if ((type.getModifiers() & Access.INTERFACE) == 0)
	      {
		type.setSuper(type);
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
		type.setInterfaces(interfaces);
	      }
	  }
      }
    return type;
  }

  public ClassType compile (Compilation comp)
  {
    ClassType saveClass = comp.curClass;
    Method saveMethod = comp.method;
    try
      {
	ClassType new_class = getCompiledClassType();
	comp.curClass = new_class;

	String filename = getFile();
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
	    decl.setSimple(false);
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

  Object walk (ExpWalker walker) { return walker.walkObjectExp(this); }

  public String toString()
  {
    String str = "ObjectExp/"+name+'/'+id+'/';

	int l = getLine();
	if (l <= 0 && body != null)
	  l = body.getLine();
	if (l > 0)
	  str = str + "l:" + l;

    return str;
  }
}
