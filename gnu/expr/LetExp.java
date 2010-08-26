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

  protected boolean mustCompile () { return false; }

  protected Object evalVariable (int i, CallContext ctx) throws Throwable
  {
    Expression init = inits[i];
    return init.eval(ctx);
  }

  public void apply (CallContext ctx) throws Throwable
  {
    setIndexes();
    int level = ScopeExp.nesting(this);
    int i = frameSize;
    
    Object[] evalFrame = new Object[i];
    Object[][] evalFrames = ctx.evalFrames;
    if (evalFrames == null)
      {
        evalFrames = new Object[level+10][];
        ctx.evalFrames = evalFrames;
      }
    else if (level >= evalFrames.length)
      {
        Object[][] newFrames = new Object[level+10][];
        System.arraycopy(evalFrames, 0, newFrames, 0, evalFrames.length);
        ctx.evalFrames = evalFrames = newFrames;
      }
    Object[] oldFrame = evalFrames[level]; // usually null
    evalFrames[level] = evalFrame;

    try
      {
        i = 0;
        for (Declaration decl = firstDecl(); decl != null;
             decl = decl.nextDecl(), i++)
          {
            if (inits[i] == QuoteExp.undefined_exp)
              continue;
            Object value = evalVariable(i, ctx);
            Type type = decl.type;
            if (type != null && type != Type.pointer_type)
              value = type.coerceFromObject(value);
            if (decl.isIndirectBinding())
              {
                gnu.mapping.Location loc = decl.makeIndirectLocationFor();
                loc.set(value);
                value = loc;
              }
            evalFrame[i] = value;
          }
        body.apply(ctx);
      }
    finally
      {
        evalFrames[level] = oldFrame;
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

  public void compile (Compilation comp, Target target)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();

    /*
    if (comp.usingCPStyle())
      { 
	for (Declartion decl = firstDecl(); decl != null; decl = decl.nextDecl())
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
        boolean needsInit = decl.needsInit();
	if (needsInit && decl.isSimple())
          decl.allocateVariable(code);
	if (! needsInit
	    || (decl.isIndirectBinding() && init == QuoteExp.undefined_exp))
	  varTarget = Target.Ignore;
	else
	  {
	    Type varType = decl.getType();
            varTarget = CheckedTarget.getInstance(decl);
	    if (init == QuoteExp.undefined_exp)
	      {
		// Typically created by letrec.
		if (varType instanceof PrimType)
		  init = new QuoteExp(new Byte((byte) 0));
		else if (varType != null && varType != Type.pointer_type)
		  init = QuoteExp.nullExp;
	      }
	  }
	init.compileWithPosition (comp, varTarget);
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

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitLetExp(this, d);
  }

  public <R,D> void visitInitializers (ExpVisitor<R,D> visitor, D d)
  {
    Declaration decl = firstDecl();
    for (int i = 0; i < inits.length; i++, decl = decl.nextDecl())
      {
        Expression init0 = inits[i];
        if (init0 == null)
          throw new Error("null1 init for "+this+" i:"+i+" d:"+decl);
        Expression init = visitor.visitAndUpdate(init0, d);
        if (init == null)
          throw new Error("null2 init for "+this+" was:"+init0);
        inits[i] = init;
        if (decl.value == init0)
          decl.value = init;
      }
  }

  protected <R,D> void visitChildren (ExpVisitor<R,D> visitor, D d)
  {
    visitInitializers(visitor, d);
    if (visitor.exitValue == null)
      body = visitor.visitAndUpdate(body, d);
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
    if (body == null)
      out.print("<null body>");
    else
      body.print (out);
    out.endLogicalBlock(endTag);
  }
}
