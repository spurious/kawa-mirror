package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.OutPort;

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
  static void store_rest (Compilation comp, Declaration decl)
  {
    if (decl != null)
      {
	store_rest (comp, decl.nextDecl());
	if (decl.needsInit())
	  decl.initBinding(comp);
      }
  }

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
	if (! decl.needsInit())
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
    store_rest (comp, firstDecl());

    body.compileWithPosition(comp, target);
    code.popScope ();
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
    out.startLogicalBlock(startTag+"#"+id+" outer:"+outer.id, endTag, 2);
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
