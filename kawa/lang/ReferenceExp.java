package kawa.lang;

/**
 * This class represents a variable reference (an identifier).
 * @author	Per Bothner
 */

public class ReferenceExp extends Expression
{
  Symbol symbol;
  Declaration binding;
  public String string_name () { return symbol.toString (); }

  public ReferenceExp (Symbol symbol)
  {
    this.symbol = symbol;
  }

  public ReferenceExp (Symbol symbol, Declaration binding)
  {
    this.symbol = symbol;
    this.binding = binding;
  }

  public Object eval (Environment env)
       throws kawa.lang.UnboundSymbol
  {
    Object val;
    if (binding != null)
      return binding.getValue (env);
    else
      {
	val = env.interp.lookup (symbol);
	if (val == null)
	  throw 
	    new UnboundSymbol(symbol.toString ());
      }
    return val;
  }

  static public void compile_load (Declaration decl, Compilation comp)
  {
    if (decl.baseVariable != null)
      {
	compile_load (decl.baseVariable, comp);  // recursive!
	comp.method.maybe_compile_checkcast (Compilation.objArrayType);
	comp.method.compile_push_int (decl.offset);
	comp.method.compile_array_load (Compilation.scmObjectType);
      }
    else
      {
	LambdaExp curLambda = comp.curLambda;
	LambdaExp declLambda = decl.context.currentLambda ();
	if (curLambda != declLambda)
	  {
	    comp.method.compile_push_value (curLambda.staticLink);
	    LambdaExp lambda = curLambda.outerLambda ();
	    for ( ; lambda != declLambda;  lambda = lambda.outerLambda ())
	      {
		comp.method.maybe_compile_checkcast (Compilation.objArrayType);
		comp.method.compile_push_int (lambda.staticLink.offset);
		comp.method.compile_array_load (Compilation.scmObjectType);
		// Invariant:  The stack top contains lambda.staticLink.
	      }
	    // Now the stack top is the declLambda heapFrame.
	  }
	else
	  {
	    comp.method.compile_push_value (decl);
	  }
      }
  }

  public void compile (Compilation comp, boolean ignore_result)
  {
    if (ignore_result)
      return;
    if (binding != null)
      compile_load (binding, comp);
    else
      {
	comp.compileConstant (symbol);
	if (comp.immediate)
	  comp.method.compile_checkcast (comp.scmSymbolType);
	comp.method.compile_invoke_static (comp.lookupGlobalMethod);
      }
  }

  public void print (java.io.PrintStream ps)
  {
    ps.print("(#%ref ");
    kawa.lang.print.print (symbol, ps);
    ps.print(")");
  }
}
