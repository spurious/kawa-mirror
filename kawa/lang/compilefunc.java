package kawa.lang;
import codegen.*;

public class compilefunc extends Procedure2
{
  public compilefunc ()
  {
    super ("compile-func");
  }

  static public final Compilation compile (LambdaExp lexp,
					   String className,
					   boolean immediate)
  {
    Compilation comp = new Compilation (className);
    comp.immediate = immediate;
    compile (comp, lexp);
    return comp;
  }

  static public final void compile (Compilation comp,  LambdaExp lexp)
  {
    int arg_count;
    char arg_letter;
    LambdaExp saveLambda = comp.curLambda;
    comp.curLambda = lexp;
    Type[] arg_types;
    Variable argsArray;
    if (lexp.min_args != lexp.max_args || lexp.min_args > 4)
      {
	arg_count = 1;
	arg_letter = 'N';
	arg_types = new Type[1];
	arg_types[0] = new ArrayType (comp.scmObjectType);

	// The "argsArray" is the second variable allocated (after "this").
	argsArray = lexp.firstVar().nextVar();
      }
    else
      {
	arg_count = lexp.min_args;
	arg_letter = Character.forDigit (arg_count, 10);
	arg_types = new Type[arg_count];
	for (int i = arg_count;  --i >= 0; )
	  arg_types[i] = comp.scmObjectType;
	argsArray = null;
      }
    ClassType superType = new ClassType ("kawa.lang.Procedure" + arg_letter);
    comp.curClass.set_super (superType);

    if (comp.curClass == comp.mainClass)
      {
	ClassType[] interfaces = { new ClassType ("kawa.lang.CompiledProc") };
	comp.curClass.setInterfaces (interfaces);

	Method setLiterals_method = comp.curClass.new_method ("setLiterals",
							      comp.applyNargs,
							      Type.void_type,
							      Access.PUBLIC);
	setLiterals_method.init_param_slots ();
	setLiterals_method.compile_push_value
	  (setLiterals_method.find_arg (1));
	setLiterals_method.compile_putstatic (comp.literalsField);
	setLiterals_method.compile_return ();
      }

    Type[] constructor_args = comp.apply0args;
    Field staticLink = null;
    if (lexp.staticLink != null)
      {
	staticLink = comp.curClass.new_field ("staticLink", comp.objArrayType);
	constructor_args = comp.applyNargs;
      }
    Method constructor_method = comp.curClass.new_method ("<init>",
							  constructor_args,
							  Type.void_type,
							  Access.PUBLIC);
    comp.curClass.constructor = constructor_method;
    Method superConstructor = superType.new_method ("<init>",
						    comp.apply0args,
						    Type.void_type,
						    Access.PUBLIC);

    constructor_method.init_param_slots ();
    constructor_method.compile_push_this ();
    constructor_method.compile_invoke_nonvirtual (superConstructor);
    if (lexp.staticLink != null)
      {
	constructor_method.compile_push_this ();
	Variable staticLinkArg = constructor_method.find_arg (1);
	constructor_method.compile_push_value (staticLinkArg);
	constructor_method.compile_putfield (staticLink);
      }
    constructor_method.compile_return ();

    Method apply_method = comp.curClass.new_method ("apply"+arg_letter,
					 arg_types,
					 comp.scmObjectType,
					 Access.PUBLIC|Access.FINAL);
    comp.method = apply_method;


    // If imcomingMap[i] is non-null, it means that the user's i'th
    // formal parameter (numbering the left-most one as 0) is captured
    // by an inferior lambda, so it needs to be saved in the heapFram.
    // The incoming variable is incomingMap[i], which is in register (i+1)
    // (since the unnamed "this" parameter is in register 0).
    Declaration incomingMap[] = new Declaration[lexp.min_args];

    // For each parameter, assign it to its proper slot.
    // If a parameter !isSimple(), we cannot assign it to a local slot,
    // so instead create an artificial Variable for the incoming argument.
    // Below, we assign the value to the slot.
    int i = 0;
    for (Variable var = lexp.firstVar ();  var != null;  var = var.nextVar ())
      {
	if (! (var instanceof Declaration) || ! var.isParameter ())
	  continue;
	// i is the register to use for the current parameter
	Declaration decl = (Declaration) var;
	if (var.isSimple ())
	  {
	    // For a simple parameter not captured by an inferior lambda,
	    // just allocate it in the incoming register.  This case also
	    // handles the artificial "this" and "argsArray" variables.
	    if (! var.isAssigned ()
		&& ! comp.method.assign_local (var, i))
	      throw new Error ("internal error assigning parameters");
	  }
	else if (argsArray != null)
	  {
	    // The incoming value is an element in the argsArray variable
	    // (or many elements in the case of a "rest" parameter).
	    // We do not need to do anything here (but see below).
	  }
	else
	  {
	    // This variable was captured by an inner lambda.
	    // It's home location is in the heapFrame.
	    // Later, we copy it from it's incoming register
	    // to its home location heapFrame.  Here we just create and
	    // assign a Variable for the incoming (register) value.
	    Symbol incoming_name = Symbol.make (var.strName ()+"Incoming");
	    Declaration incoming = lexp.add_decl (incoming_name);
	    incoming.setArtificial (true);
	    incoming.setParameter (true);
	    if (! comp.method.assign_local (incoming, i))
	      throw new Error ("internal error assigning parameters");
	    incoming.baseVariable = decl;
	    // Subtract 1, so we don't count the "this" variable.
	    incomingMap[i-1] = incoming;
	  }
	i++;
      }

    comp.method.enterScope (lexp.scope);

    if (lexp.staticLink != null)
      {
	comp.method.compile_push_this ();
	comp.method.compile_getfield (staticLink);
	comp.method.compile_store_value (lexp.staticLink);
      }

    if (lexp.heapFrame != null)
      {
	comp.method.compile_push_int (lexp.heapSize);
	comp.method.compile_new_array (comp.scmObjectType);
	comp.method.compile_store_value (lexp.heapFrame);
      }

    // For each non-artificial parameter, copy it from its incoming
    // location (a local variable register, or the argsArray) into
    // its home location, if they are different.
    i = 0;
    for (Variable var = lexp.firstVar ();  var != null; var = var.nextVar ())
      {
	if (var.isParameter () && ! var.isArtificial ())
	  {
	    if (argsArray != null || incomingMap[i] != null)
	      {
		// If the parameter is captured by an inferior lambda,
		// then the incoming parameter needs to be copied into its
		// slot in the heapFrame.  Thus we emit an aaload instruction.
		// Unfortunately, it expects the new value *last*,
		// so first push the heapFrame array and the array index.
		Declaration param = (Declaration) var;
		if (!param.isSimple ())
		  {
		    ReferenceExp.compile_load (param.baseVariable, comp);
		    comp.method.compile_push_int (param.offset);
		  }
		// This part of the code pushes the incoming argument.
		if (argsArray == null)
		  {
		    // Simple case:  Incoming register is in incomingMap[i]:
		    comp.method.compile_push_value (incomingMap[i]);
		  }
		else
		  {
		    // Incoming parameters are in argsArray.
		    comp.method.compile_push_value (argsArray);
		    comp.method.compile_push_int (i);
		    if (i >= lexp.min_args)
		      {
			// This is the "rest" parameter (i.e. following a "."):
			// Convert argsArray[i .. ] to a list.
			comp.method.compile_invoke_static
			  (Compilation.makeListMethod);
		      }
		    else
		      {
			// This is a required parameter, in argsArray[i].
			comp.method.compile_array_load
			  (Compilation.scmObjectType);
		      }
		  }
		// Now finish copying the incoming argument into its
		// home location.
		if (param.isSimple ())
		  comp.method.compile_store_value (param);
		else
		  comp.method.compile_array_store (Compilation.scmObjectType);
	      }
	    i++;
	  }
      }

    lexp.body.compile (comp, false);
    comp.method.compile_return ();

    comp.method.pop_scope ();
    comp.curLambda = saveLambda;
  }

  public final Object apply2 (Object arg1, Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (! (arg1 instanceof LambdaProcedure))
      throw new WrongType (this.name, 1, "lambda procedure");
    LambdaProcedure proc = (LambdaProcedure) arg1;
    try
      {
	compile (proc.lexpr, arg2.toString (), false).curClass.emit_to_file ();
      }
    catch (java.io.IOException ex)
      {
        System.err.print("Caught I/O exception: ");
      }
    return Interpreter.voidObject;
  }

}
