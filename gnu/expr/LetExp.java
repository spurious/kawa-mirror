package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

/**
 * Class used to implement "let" syntax (and variants) for Scheme.
 * @author	Per Bothner
 */

public class LetExp extends ScopeExp
{
  public Expression[] inits;
  public Expression body;

  public LetExp (Expression[] i) { inits = i; }

  public Expression getBody() { return body; }
  public void setBody(Expression body) { this.body = body; }

  /* Recursive helper routine, to store the values on the stack
   * into the variables in vars, in reverse order. */
  void store_rest (Compilation comp, int i, Declaration decl)
  {
    if (decl != null)
      {
	store_rest (comp, i+1, decl.nextDecl());
	if (decl.needsInit())
	  {
	    if (decl.isIndirectBinding())
	      {
		CodeAttr code = comp.getCode();
		if (inits[i] == QuoteExp.undefined_exp)
		  {
		    Object name = decl.getSymbol();
		    comp.compileConstant(name, Target.pushObject);
		    code.emitInvokeStatic(BindingInitializer.makeLocationMethod(name));
		  }
		else
		  {
		    decl.pushIndirectBinding(comp);
		  }
	      }
            decl.compileStore(comp);
	  }
      }
  }

  /* CPS:
   * Need to ensure that ctx.pc is 0 before the this is called
   * the first time. This is currently done by match0.
   * Need to define concention so ReferenceExp can find appropriate binding.
   * Need to define convention for what gets copied, if anything,
   * when a continuation is created.  (Note problem below if a half-initialized
   * frame gets captuerd.  Then multiple cals to the same continuation
   * could clobber the frame, unless it has been copied.  But copying the
   * frame is tricky if we want to avoid copying the whole stack, plus we
   * have to correctly handle set! to a local/
  public void apply (CallContext ctx) throws Throwable
  {
    CallFrame fr;
    if (ctx.pc == 0)
      {
	fr = new gnu.mapping.CallFrame();
	fr.previous = ctx.frame;
	fr.saveVstackLen = ctx.startFromContext();
	ctx.frame = fr;
      }
    else
      fr = ctx.frame;
    int i = ctx.pc;
    if (i == inits.length + 1)
      {
	// pop
	ctx.frame = fr.previous;
	return;
      }
    if (i > 0)
      fr.values[i-1] = ctx.getFromContext(fr.saveVstackLen);
    ctx.pc++;
    if (i == inits.length)
      {
        body.match0(ctx);
	return;
      }
    fr.saveVstackLen = ctx.startFromContext();
    inits[i].match0(ctx);
  }
  */

  public void compile (Compilation comp, Target target)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();

    /*
    if (comp.usingCPStyle())
      { 
	for (Declartion decl = firstDecl(); decl != null; decl = decl.nextVar ())
	  {
	    decl.assignField(comp);
	  }
     }
    */

    /* Compile all the initializations, leaving the results
       on the stack (in reverse order).  */
    Declaration decl = firstDecl();
    for (int i = 0; i < inits.length; i++, decl = decl.nextDecl())
      {
	Target varTarget;
	Expression init = inits[i];
        decl.allocateVariable(code);
	if (! decl.needsInit()
	    || (decl.isIndirectBinding() && inits[i] == QuoteExp.undefined_exp))
	  varTarget = Target.Ignore;
	else
	  {
	    Type varType = decl.getType();
	    varTarget = CheckedTarget.getInstance(varType);
	    if (init == QuoteExp.undefined_exp)
	      {
		// Typically created by letrec.
		if (varType instanceof PrimType)
		  init = new QuoteExp(new Byte((byte) 0));
		else if (varType != null && varType != Type.pointer_type)
		  init = QuoteExp.nullExp;
	      }
	  }
	init.compile (comp, varTarget);
      }

    code.enterScope(getVarScope());

    /* Assign the initial values to the proper variables, in reverse order. */
    store_rest (comp, 0, firstDecl());

    body.compileWithPosition(comp, target);
    popScope(code);
  }

  public final gnu.bytecode.Type getType()
  {
    return body.getType();
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkLetExp(this);
  }

  protected void walkChildren(ExpWalker walker)
  {
    if (inits != null)
      inits = walker.walkExps(inits);
    if (walker.exitValue == null)
      body = (Expression) walker.walk(body);
  }

  public void print (OutPort out)
  {
    print(out, "(Let", ")");
  }

  public void print (OutPort out, String startTag, String endTag)
  {
    out.startLogicalBlock(startTag+"#"+id, endTag, 2);
    out.writeSpaceFill();
    printLineColumn(out);
    out.startLogicalBlock("(", false, ")");
    Declaration decl = firstDecl();
    int i = 0;
    
    for (; decl != null;  decl = decl.nextDecl())
      {
	if (i > 0)
	  out.writeSpaceFill();
	out.startLogicalBlock("(", false, ")");
	decl.printInfo(out);
	if (inits != null)
	  {
	    out.writeSpaceFill();
	    out.print('=');
	    out.writeSpaceFill();
	    //if (decl.isArtificial ())
	    //out.print ("<artificial>");
	    //else
	    {
	      if (i >= inits.length)
		out.print("<missing init>");
	      else if (inits[i] == null)
		out.print("<null>");
	      else
		inits[i].print(out);
	      i++;
	    }
	  }
	out.endLogicalBlock(")");
      }
    out.endLogicalBlock(")");
    out.writeSpaceLinear();
    body.print (out);
    out.endLogicalBlock(endTag);
  }
}
