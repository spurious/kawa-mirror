package kawa.lang;
import gnu.bytecode.ClassType;
import gnu.bytecode.Method;
import gnu.bytecode.Access;

/**
 * This class represents a variable reference (an identifier).
 * @author	Per Bothner
 */

public class ReferenceExp extends Expression
{
  String symbol;
  Declaration binding;
  public String string_name () { return symbol; }

  public final String getName() { return symbol; }
  public final Declaration getBinding() { return binding; }

  public ReferenceExp (String symbol)
  {
    this.symbol = symbol;
  }

  public ReferenceExp (String symbol, Declaration binding)
  {
    this.symbol = symbol;
    this.binding = binding;
  }

  public static Object lookup(String name)
  { return lookup(Environment.user(), name); }

  /** Lookup on a symbol in a given environment.
    * As a Kawa/Scheme extension, handles <TYPENAME> if otherwise undefined.
    */
  public static Object lookup(Environment env, String name)
  {
    Object val = env.get(name);
    if (val == null)
      {
	int len = name.length();
	if (len > 2 && name.charAt(0) == '<' && name.charAt(len-1) == '>')
	  {
	    String tname = name.substring(1, len-1);
	    gnu.bytecode.Type type = PrimProcedure.string2Type(tname);
	    if (type != null && type.getReflectClass() != null)
	      return type;
	  }
	throw new UnboundSymbol(name);
      }
    return val;
  }

  public Object eval (Environment env)
       throws kawa.lang.UnboundSymbol
  {
    if (binding != null)
      throw new Error("internal error: ReferenceExp.eval on lexical binding");
    return lookup(env, symbol);
  }

  static public void compile_load (Declaration decl, Compilation comp)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    if (decl.baseVariable != null)
      {
	compile_load (decl.baseVariable, comp);  // recursive!
	comp.method.maybe_compile_checkcast (Compilation.objArrayType);
	code.emitPushInt(decl.offset);
	code.emitArrayLoad(Compilation.scmObjectType);
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
		code.emitArrayLoad(Compilation.scmObjectType);
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

  private static ClassType thisType;
  private static Method lookupMethod;

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
	int len = symbol.length();
	if (len > 2 && symbol.charAt(0) == '<' && symbol.charAt(len-1) == '>')
	  {
	    if (thisType == null)
	      {
		thisType = new ClassType("kawa.lang.ReferenceExp");
		lookupMethod
		  = thisType.addMethod ("lookup",
					Compilation.sym1Arg,
					Compilation.scmObjectType,
					Access.PUBLIC|Access.STATIC);
	      }
	    comp.getCode().emitInvokeStatic(lookupMethod);
	  }
	else
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
