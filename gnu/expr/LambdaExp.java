package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

/**
 * Class used to implement Scheme lambda expressions.
 * @author	Per Bothner
 */

public class LambdaExp extends ScopeExp
{
  public String name;
  public Expression body;
  public int min_args;
  // Maximum number of actual arguments;  -1 if variable.
  public int max_args;
  public Keyword[] keywords;
  public Expression[] defaultArgs;

  /** The name to give to a dummy implicit function that surrounds a file. */
  public static String fileFunctionName = "atFileLevel";

  /** True iff this is the dummy top-level function of a module body. */
  public final boolean isModuleBody () { return this instanceof ModuleExp; }

  public final boolean variable_args () { return max_args < 0; }


  /** Number of argument variable actually passed by the caller.
   * For functions that accept more than 4 argument, or take a variable number,
   * this is 1, since in that all arguments are passed in a single array. */
  public int incomingArgs ()
  {
    // The max_args > 0 is a hack to handle LambdaProecdure, which
    // currently always uses a single array argument.
    return min_args == max_args && max_args <= 4 && max_args > 0 ? max_args : 1;
  }

  public void setName (String name)
  {
    this.name = name;
  }

  public LambdaExp outerLambda ()
  {
    return outer == null ? null : outer.currentLambda ();
  }

  public void declareThis()
  {
    Variable var;
    var = addDeclaration ("this");
    var.setParameter (true);  var.setArtificial (true);
  }

  public LambdaExp ()
  {
  }

  public LambdaExp (Expression body)
  {
    declareThis();
    this.body = body;
  }

  /** If non-null, this is the Field that contains the static link. */
  public Field staticLinkField;

  /** If non-null, this contains the static link.
   * It is copied from the staticLinkField.
   * The static link variable points to the same array as the
   * enclosing function's heapFrame. */
  public Declaration staticLink;

  /** Declaration used if varargs or too many args. */
  public Declaration argsArray;

  /** Get the i'the formal parameter. */
  Declaration getArg (int i)
  {
    for (Variable var = firstVar ();  ; var = var.nextVar ())
      {
	if (var == null)
	  throw new Error ("internal error - getArg");
	if (var.isParameter () && !var.isArtificial ())
	  {
	    if (i == 0)
	      return (Declaration) var;
	    --i;
	  }
      }
  }

  public void compile (Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    gnu.bytecode.CodeAttr code = comp.getCode();
    ClassType saveClass = comp.curClass;
    Method saveMethod = comp.method;
    ClassType new_class;
    try
      {
	String new_name
	  = comp.generateClassName (name == null ? "lambda" : name.toString());
	new_class = comp.addClass (this, new_name);
      }
    finally
      {
	comp.curClass = saveClass;
	comp.method = saveMethod;
      }
    code.emitNew(new_class);
    code.emitDup(new_class);
    if (staticLink != null)
      code.emitLoad(outerLambda().heapFrame);
    code.emitInvokeSpecial(new_class.constructor);
    target.compileFromStack(comp, new_class);
  }

  void compile_setLiterals (Compilation comp)
  {
    ClassType[] interfaces = { new ClassType ("gnu.expr.CompiledProc") };
    comp.mainClass.setInterfaces (interfaces);

    Method setLiterals_method
      = comp.mainClass.addMethod ("setLiterals", comp.applyNargs,
				   Type.void_type, Access.PUBLIC);
    setLiterals_method.init_param_slots ();
    CodeAttr code = setLiterals_method.getCode();
    code.emitLoad(code.getArg(1));
    code.emitPutStatic(comp.literalsField);
    code.emitReturn();
  }

  public Object eval (Environment env)
  {
    try
      {
	String class_name = name == null ? "lambda"
	  : Compilation.mangleName (name.toString ());

	Compilation comp = new Compilation (this, class_name, true);
	compile_setLiterals (comp);

	byte[][] classes = new byte[comp.numClasses][];
	String[] classNames = new String[comp.numClasses];
	for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	  {
	    ClassType clas = comp.classes[iClass];
	    classNames[iClass] = clas.getName ();
	    classes[iClass] = clas.writeToArray ();
	  }

	/* DEBUGGING:
	java.io.FileOutputStream zfout = new java.io.FileOutputStream("Foo.zip");
	java.util.zip.ZipOutputStream zout = new java.util.zip.ZipOutputStream(zfout);
	for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	  {
	    String clname = classNames[iClass].replace ('.', '/') + ".class";
	    java.util.zip.ZipEntry zent = new java.util.zip.ZipEntry (clname);
	    zent.setSize(classes[iClass].length);
	    java.util.zip.CRC32 crc = new java.util.zip.CRC32();
	    crc.update(classes[iClass]);
	    zent.setCrc(crc.getValue());
	    zent.setMethod(java.util.zip.ZipEntry.STORED);
	    zout.putNextEntry(zent);
	    zout.write(classes[iClass]);
	  }
	zout.close ();
	*/
	/* DEBUGGING:
	for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	  ClassTypeWriter.print(comp.classes[iClass], System.out, 0);
	*/

	ArrayClassLoader loader = new ArrayClassLoader (classNames, classes);
	Class clas = loader.loadClass (class_name, true);
	Object inst = clas.newInstance ();

	/* Pass literal values to the compiled code. */
	CompiledProc cproc = (CompiledProc) inst;
	Object[] literals = new Object[comp.literalsCount];
	for (Literal literal = comp.literalsChain;  literal != null;
	     literal = literal.next)
	  {
	    /* DEBUGGING:
	    System.err.print ("literal["+literal.index+"]=");
	    SFormat.print (literal.value, System.err);
	    System.err.println();
	    */
	    literals[literal.index] = literal.value;
	  }
	cproc.setLiterals (literals);
	Named named = (Named) inst;
	if (named.name () == null)
	  named.setName (this.name);

	return inst;
      }
    catch (java.io.IOException ex)
      {
    ex.printStackTrace(OutPort.errDefault());
	throw new RuntimeException ("I/O error in lambda eval: "+ex);
      }
    catch (ClassNotFoundException ex)
      {
	throw new RuntimeException("class not found in lambda eval");
      }
    catch (InstantiationException ex)
      {
	throw new RuntimeException("class not instantiable: in lambda eval");
      }
    catch (IllegalAccessException ex)
      {
	throw new RuntimeException("class illegal access: in lambda eval");
      }
  }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%lambda (");
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
    body.print (ps);
    ps.print(")");
  }
}
