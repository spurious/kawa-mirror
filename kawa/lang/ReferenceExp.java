package kawa.lang;

/**
 * This class represents a variable reference (an identifier).
 * @author	Per Bothner
 */

public class ReferenceExp extends Expression
{
  String symbol;
  Declaration binding;
  public String string_name () { return symbol; }

  public ReferenceExp (String symbol)
  {
    this.symbol = symbol;
  }

  public ReferenceExp (String symbol, Declaration binding)
  {
    this.symbol = symbol;
    this.binding = binding;
  }

  public Object eval (Environment env)
       throws kawa.lang.UnboundSymbol
  {
    Object val;
    if (binding != null)
      throw new Error("internal error: ReferenceExp.eval on lexical binding");
    else
      {
	val = env.get (symbol);
	if (val == null)
	  throw new UnboundSymbol (symbol);
      }
    return val;
  }

  static public void compile_load (Declaration decl, Compilation comp)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    if (decl.baseVariable != null)
      {
	compile_load (decl.baseVariable, comp);  // recursive!
	comp.method.maybe_compile_checkcast (Compilation.objArrayType);
	code.emitPushInt(decl.offset);
	comp.method.compile_array_load (Compilation.scmObjectType);
      }
    else
      {
	LambdaExp curLambda = comp.curLambda;
	LambdaExp declLambda = decl.context.currentLambda ();
	if (curLambda != declLambda)
	  {
	    compile_load (curLambda.staticLink, comp);
	    LambdaExp lambda = curLambda.outerLambda ();
	    for ( ; lambda != declLambda;  lambda = lambda.outerLambda ())
	      {
		comp.method.maybe_compile_checkcast (Compilation.objArrayType);
		code.emitPushInt(lambda.staticLink.offset);
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

  public void compile (Compilation comp, int flags)
  {
    if ((flags & IGNORED) != 0)
      return;
    if (binding != null)
      compile_load (binding, comp);
    else
      {
	comp.compileConstant (symbol);
	if (comp.immediate)
	  comp.method.compile_checkcast (comp.scmSymbolType);
	comp.getCode().emitInvokeStatic(comp.lookupGlobalMethod);
      }
  }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%ref ");
    SFormat.print (symbol, ps);
    ps.print(")");
  }
}
