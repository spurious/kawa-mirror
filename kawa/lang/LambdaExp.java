package kawa.lang;
import codegen.*;
import java.util.Hashtable;

/**
 * Class used to implement Scheme lambda expressions.
 * @author	Per Bothner
 */

public class LambdaExp extends ScopeExp
{
  Symbol name;
  Expression body;
  int min_args;
  // Maximum number of actual arguments;  -1 if variable.
  int max_args;
  Keyword[] keywords;
  Expression[] defaultArgs;

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

  public void setName (Symbol name)
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
   * @param tr the (Scheme) Translator
   */
  public LambdaExp (Object formals, Object body, Translator tr)
  {
    /* Count formals, while checking that the syntax is OK. */
    Object bindings = formals;
    int opt_args = -1;
    int rest_args = -1;
    int key_args = -1;
    Pair pair;
    for (; bindings instanceof Pair;  bindings = pair.cdr)
      {
	pair = (Pair) bindings;
	if (pair.car == Special.optional)
	  {
	    if (opt_args >= 0)
	      {
		tr.syntaxError ("multiple #!optional in parameter list");
		return;
	      }
	    else if (rest_args >= 0 || key_args >= 0)
	      {
		tr.syntaxError ("#!optional after #!rest or #!key");
		return;
	      }
	    opt_args = 0;
	  }
	else if (pair.car == Special.rest)
	  {
	    if (rest_args >= 0)
	      {
		tr.syntaxError ("multiple #!rest in parameter list");
		return;
	      }
	    else if (key_args >= 0)
	      {
		tr.syntaxError ("#!rest after #!key");
		return;
	      }
	    rest_args = 0;
	  }
	else if (pair.car == Special.key)
	  {
	    if (key_args >= 0)
	      {
		tr.syntaxError ("multiple #!key in parameter list");
		return;
	      }
	    key_args = 0;
	  }
	else if (key_args >= 0)
	  key_args++;
	else if (rest_args >= 0)
	  rest_args++;
	else if (opt_args >= 0)
	  opt_args++;
	else
	  min_args++;
	bindings = pair.cdr;
      }
    if (bindings instanceof Symbol)
      {
	if (opt_args >= 0 || key_args >= 0 || rest_args >= 0)
	  {
	    tr.syntaxError ("dotted rest-arg after #!optional, #!rest, or #!key");
	    return;
	  }
	rest_args = 1;
      }
    else if (bindings != List.Empty)
      {
	tr.syntaxError ("misformed formals in lambda");
	return;
      }
    if (rest_args > 1)
      {
	tr.syntaxError ("multiple #!rest parameters");
        return;
      }
    if (opt_args < 0)
      opt_args = 0;
    if (rest_args < 0)
      rest_args = 0;
    if (key_args < 0)
      key_args = 0;
    if (rest_args > 0)
      max_args = -1;
    else
      max_args = min_args + opt_args + 2 * key_args;  // Is this useful?
    if (opt_args + key_args > 0)
      defaultArgs = new Expression[opt_args + key_args];
    if (key_args > 0)
      keywords = new Keyword[key_args];

    Variable var;
    var = add_decl (Symbol.make ("this"));
    var.setParameter (true);  var.setArtificial (true);
    
    if (min_args != max_args || min_args > 4)
      {
	// Compilation.compile depends on the "argsArray" variable
	// being the second one created for this scope.
	argsArray = add_decl (Symbol.make ("argsArray"),
			       Compilation.objArrayType);
	argsArray.setParameter (true);
	argsArray.setArtificial (true);
      }
    push (tr);
    bindings = formals;
    int i = 0;
    opt_args = 0;
    key_args = 0;
    Object mode = null;
    for (; bindings instanceof Pair;  bindings = pair.cdr)
      {
	pair = (Pair) bindings;
	if (pair.car == Special.optional
	    || pair.car == Special.rest || pair.car == Special.key)
	  {
	    mode = pair.car;
	    continue;
	  }
	Symbol name;
	Object defaultValue;
	if (pair.car instanceof Symbol)
	  {
	    name = (Symbol) pair.car;
	    defaultValue = null;
	  }
	else if (pair.car instanceof Pair
		 && ((Pair) pair.car).car instanceof Symbol
		 && ((Pair) pair.car).cdr instanceof Pair)
	  {
	    Pair pair_car = (Pair) pair.car;
	    name = (Symbol) pair_car.car;
	    defaultValue = ((Pair) pair_car.cdr).car;
	    if (mode == null || mode == Special.rest)
	      {
		tr.syntaxError ("default value for required or #!rest parameter");
		
		return;
	      }
	  }
	else
	  {
	    tr.syntaxError ("parameter is neither name nor (name default)");
	    return;
	  }
	if (mode == Special.optional || mode == Special.key)
	  {
	    defaultArgs[opt_args++] = defaultValue == null ? QuoteExp.falseExp
	      : tr.rewrite(defaultValue);
	  }
	if (mode == Special.key)
	  keywords[key_args++] = Keyword.make(name.toString());
	Declaration decl = add_decl (name);
	decl.setParameter(true);
	decl.noteValue(null);  // Does not have a known value.
	decl.push(tr);
      }
    if (bindings instanceof Symbol)
      {
	Declaration decl = add_decl ((Symbol) bindings);
	decl.setParameter (true);
	decl.noteValue (null);  // Does not have a known value.
	decl.push(tr);
      }
    this.body = tr.rewrite_body (body);
    pop (tr);
  }

  /** If non-null, this is the Field that contains the static link. */
  Field staticLinkField;

  /** If non-null, this contains the static link.
   * It is copied from the staticLinkField.
   * The static link variable points to the same array as the
   * enclosing function's heapFrame. */
  Declaration staticLink;

  /** Declaration used if varargs or too many args. */
  Declaration argsArray;

  /** Start of actual body (after copying args etc into home locations). */
  Label start_label;

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

  public void compile (Compilation comp, int flags)
  {
    if ((flags & IGNORED) != 0)
      return;
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
    comp.method.compile_new (new_class);
    comp.method.compile_dup (new_class);
    if (staticLink != null)
      {
	Declaration frame = outerLambda().heapFrame;
	comp.method.compile_push_value (frame);
      }
    comp.method.compile_invoke_special (new_class.constructor);
  }

  void compile_setLiterals (Compilation comp)
  {
    ClassType[] interfaces = { new ClassType ("kawa.lang.CompiledProc") };
    comp.mainClass.setInterfaces (interfaces);

    Method setLiterals_method
      = comp.mainClass.new_method ("setLiterals", comp.applyNargs,
				   Type.void_type, Access.PUBLIC);
    setLiterals_method.init_param_slots ();
    setLiterals_method.compile_push_value (setLiterals_method.find_arg (1));
    setLiterals_method.compile_putstatic (comp.literalsField);
    setLiterals_method.compile_return ();
  }

  public Object eval (Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    try
      {
	String class_name = name == null ? "lambda"
	  : Compilation.mangleClassName (name.toString ());

	Compilation comp = new Compilation (this, class_name, true);
	compile_setLiterals (comp);

	byte[][] classes = new byte[comp.numClasses][];
	String[] classNames = new String[comp.numClasses];
	for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	  {
	    ClassType clas = comp.classes[iClass];
	    classNames[iClass] = clas.getName ();
	    classes[iClass] = clas.emit_to_array ();
	  }

	/* DEBUGGING:
	ZipArchive zar = new ZipArchive ("Foo.zip", "rw");
	for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	  zar.append (classNames[iClass].replace ('.', '/') + ".class",
	              classes[iClass]);
	zar.close ();
	*/

	SchemeLoader loader = new SchemeLoader (classNames, classes);
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
	throw new GenericError ("I/O error in lambda eval");
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
