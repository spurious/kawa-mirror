package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

public class ObjectExp extends LambdaExp
{
  /** List of base classes and implemented interfaces. */
  public Expression[] supers;

  public LambdaExp initMethod;

  public ObjectExp ()
  {
    type = null;
    // Make sure we actually generate a class.
    setCanRead(true);
  }

  public String getJavaName ()
  {
    return name == null ? "object" : Compilation.mangleName (name);
  }

  public ClassType getCompiledClassType(Compilation comp)
  {
    if (type == null)
      {
	String name = getName();
	if (name == null)
	  name = "object";
	type = new ClassType(comp.generateClassName(name));
	if (supers == null || supers.length == 0)
	  type.setSuper(Type.pointer_type);
	else
	  {
	    int len = supers.length;
	    ClassType[] superTypes = new ClassType[len];
	    int j = 0;
	    for (int i = 0;  i < len;  i++)
	      {
		ClassType t = (ClassType) ((QuoteExp) supers[i]).getValue(); 
		if ((t.getModifiers() & Access.INTERFACE) == 0)
		  {
		    if (j < i)
		      throw new Error("duplicate superclass");
		    type.setSuper(t);
		  }
		else
		  superTypes[j++] = t;
	      }
	    if (j > 0)
	      {
		ClassType[] interfaces;
		if (j == len)
		  interfaces = superTypes;
		else
		  {
		    interfaces = new ClassType[j];
		    System.arraycopy(superTypes, 0, interfaces, 0, j);
		  }
		type.setInterfaces(interfaces);
	      }
            if (j == len)
              type.setSuper(Type.pointer_type);
	  }
      }
    return type;
  }

  public void compile (Compilation comp, Target target)
  {
    super.compile(comp, Target.pushObject);
    if (initMethod != null)
      {
	CodeAttr code = comp.getCode();
	code.emitDup(1);
	code.emitInvokeVirtual(initMethod.primMethod);
      }
    target.compileFromStack(comp, getCompiledClassType(comp));
  }

  public ClassType compile (Compilation comp)
  {
    ClassType saveClass = comp.curClass;
    Method saveMethod = comp.method;
    try
      {
	ClassType new_class = getCompiledClassType(comp);
	comp.curClass = new_class;

	String filename = getFile();
	if (filename != null)
	  new_class.setSourceFile (filename);

	LambdaExp saveLambda = comp.curLambda;
	comp.curLambda = this;

	allocFrame(comp);
	if (getNeedsStaticLink() && saveLambda.closureEnv != null)
	  {
	    staticLinkField
	      = new_class.addField("staticLink", 
				   saveLambda.closureEnv.getType());
	  }
	comp.generateConstructor (comp.curClass, this);

	CodeAttr code;

	for (Variable var = firstVar ();  var != null;  var = var.nextVar ())
	  {
	    Declaration decl = (Declaration) var;
	    // If the declaration derives from a method, don't create field.
	    if (decl.getCanRead())
	      {
		decl.field = new_class.addField(decl.getName(), decl.getType(),
						Access.PUBLIC);
		decl.setSimple(false);
	      }
	  }

	for (LambdaExp child = firstChild;  child != null; )
	  {
	    ClassType method_class;
	    boolean method_static;
	    int method_flags;
	    //method_class = heapFrameLambda.getCompiledClassType(comp);
	    //method_class = (ClassType) heapFrame.getType();
	    method_class = type;
	    child.declareThis(method_class);

	    // generate_unique_name (method_class, child.getName());
	    String child_name = child.getName();
	    String method_name = comp.mangleName(child_name);
	    child.primMethod
	      = child.addMethodFor (method_class, method_name, null);
	    child.primMethod.setModifiers(Access.PUBLIC);
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
	    child.body.compileWithPosition(comp, Target.returnValue(rtype));
	    child.compileEnd(comp);
	    child.compileChildMethods(comp);
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

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%object/");
    if (name != null)
      {
	ps.print(name);
	ps.print('/');
      }
    ps.print(id);
    ps.print("/ (");
    Special prevMode = null;
    int i = 0;
    int opt_i = 0;
    int key_args = keywords == null ? 0 : keywords.length;
    int opt_args = defaultArgs == null ? 0 : defaultArgs.length - key_args;
    for (Variable var = firstVar ();  var != null; var = var.nextVar ())
      {
	if (! var.isParameter () || var.isArtificial ())
	  continue;
	Special mode;
	if (i < min_args)
	  mode = null;
	else if (i < min_args + opt_args)
	  mode = Special.optional;
	else if (max_args < 0 && i == min_args + opt_args)
	  mode = Special.rest;
	else
	  mode = Special.key;
	if (i > 0)
	  ps.print(' ');
	if (mode != prevMode)
	  {
	    ps.print(mode);
	    ps.print(' ');
	  }
	Expression defaultArg = null;
	if (mode == Special.optional || mode == Special.key)
	  defaultArg = defaultArgs[opt_i++];
	if (defaultArg != null)
	  ps.print('(');
	ps.print(((Declaration)var).string_name());
	if (defaultArg != null && defaultArg != QuoteExp.falseExp)
	  {
	    ps.print(' ');
	    defaultArg.print(ps);
	    ps.print(')');
	  }
	i++;
	prevMode = mode;
      }
    ps.print(") ");
    for (LambdaExp child = firstChild;  child != null;
	 child = child.nextSibling)
      {
        ps.println();
        ps.print("  method: ");
        child.print(ps);
      }
    if (body == null)
      ps.print("<null body>");
    else
      body.print (ps);
    ps.print(")");
  }

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
