package kawa.lang;
import codegen.*;
import java.util.Hashtable;

/**
 * Class used to implement Scheme lambda expressions.
 * @author	Per Bothner
 */

public class LambdaExp extends ScopeExp
{
  String name;
  Expression body;
  int min_args;
  // Maximum number of actual arguments;  -1 if variable.
  int max_args;
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

  /**
   * Higher-level constructor, that does the re-writing.
   * @param formals the formal parameter list (or symbol)
   * @param body the body of the procedure
   * @param interp the (Scheme) interpreter
   */
  public LambdaExp (Object formals, Object body, Interpreter interp)
    throws WrongArguments
  {
    /* Count formals, while checking that the syntax is OK. */
    Object bindings = formals;
    for (; bindings instanceof Pair; min_args++)
      bindings = ((Pair)bindings).cdr;
    if (bindings == List.Empty)
      max_args = min_args;
    else if (bindings instanceof Symbol)
      max_args = -1;
    else
      throw new WrongArguments ("lambda", 2,
				"(lambda formals body) [invalid formals]");

    Declaration array_args = null;
    Variable var;
    var = add_decl (Symbol.make ("this"));
    var.setParameter (true);  var.setArtificial (true);
    
    if (max_args < 0 || max_args > 4)
      {
	// compilefunc.compile depends on the "argsArray" variable
	// being the second one created for this scope.
	array_args = add_decl (Symbol.make ("argsArray"),
			       Compilation.objArrayType);
	array_args.setParameter (true);
	array_args.setArtificial (true);
      }
    bindings = formals;
    int i = 0;
    while (bindings instanceof Pair)
      {
	Pair bind_pair = (Pair) bindings;
	Declaration decl = add_decl ((Symbol) bind_pair.car);
	decl.setParameter (true);
	bindings = bind_pair.cdr;
      }
    if (bindings instanceof Symbol)
      {
	add_decl ((Symbol) bindings).setParameter (true);
      }
    push (interp);
    this.body = interp.rewrite_body (body);
    pop (interp);
  }

  /** A variable that points to the heap-allocated part of the frame.
   * This is an Object array that contains all the variables
   * captured by an inner Lambda. */
  Declaration heapFrame;

  /** The size needed for the heapFrame array. */
  int heapSize;

  /** If non-null, this contains the static link.
   * The static link variable points to the same array as the
   * enclosing function's heapFrame. */
  Declaration staticLink;

  public static boolean doCompile = true;

  public void compile (Compilation comp, boolean ignore_result)
  {
    if (ignore_result)
      return;
    ClassType saveClass = comp.curClass;
    Method saveMethod = comp.method;
    ClassType new_class;
    try
      {
	String new_name = "lambda" + comp.numClasses;
	new_class = new ClassType (new_name);
	comp.curClass = new_class;
	comp.addClass (new_class);
	compilefunc.compile (comp, this);
      }
    finally
      {
	comp.curClass = saveClass;
	comp.method = saveMethod;
      }
    comp.method.compile_new (new_class);
    comp.method.compile_dup (new_class);
    if (staticLink != null)
      {
	Declaration frame = outerLambda().heapFrame;
	comp.method.compile_push_value (frame);
      }
    comp.method.compile_invoke_nonvirtual (new_class.constructor);
  }

  public Object eval (Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    if (!doCompile)
      return new LambdaProcedure (this, env);
    try
      {
    Compilation comp = compilefunc.compile (this, "lambda0", true);

    byte[][] classes = new byte[comp.numClasses][];
    for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
      {
	classes[iClass] = comp.classes[iClass].emit_to_array ();

	// DEBUGGING - FIXME
	try {comp.classes[iClass].emit_to_file ("lambda" + iClass + ".class");}
	catch (java.io.IOException ex)
	  {   System.err.print("Caught I/O exception: ");  }

      }

    SchemeLoader loader = new SchemeLoader (classes);
    Class clas = loader.loadClass ("lambda0", true);
    //Class clas = Class.forName ("lambda0");
    Object inst = clas.newInstance ();
    /* Pass literal values to the compiled code. */
    CompiledProc cproc = (CompiledProc) inst;
    Object[] literals = new Object[comp.literalsCount];
    for (Literal literal = comp.literalsChain;  literal != null;
	 literal = literal.next)
      {
	literals[literal.index] = literal.value;
      }
    cproc.setLiterals (literals);

    return inst;
      }
    catch (java.io.IOException ex)
      {
	throw new GenericError ("class I/O error in lambda eval");
      }
    catch (ClassNotFoundException ex)
      {
	throw new GenericError ("class not found in lambda eval");
      }
    catch (InstantiationException ex)
      {
	throw new GenericError ("class not instantiable: in lambda eval");
      }
    catch (IllegalAccessException ex)
      {
	throw new GenericError ("class illegal access: in lambda eval");
      }
  }

  public void print (java.io.PrintStream ps)
  {
    ps.print("(#%lambda ... ");
    body.print (ps);
    ps.print(")");
  }
}
