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

  Method makeBindingMethod = null;

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
        decl.allocateVariable(code);
	if (! decl.needsInit())
	  varTarget = Target.Ignore;
	else
	  {
	    Type varType = decl.getType();
	    if (varType == Type.pointer_type)
	      varTarget = Target.pushObject;
	    else
	      varTarget = new StackTarget(varType);
	  }
	inits[i].compile (comp, varTarget);
      }

    code.enterScope (scope);

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
    inits = walker.walkExps(inits);
    if (walker.exitValue == null)
      body = (Expression) walker.walk(body);
  }

  public void print (OutPort out)
  {
    out.startLogicalBlock("(Let", ")", 2);
    out.writeSpaceFill();
    printLineColumn(out);
    out.startLogicalBlock("(", false, ")");
    Declaration decl = firstDecl();
    int i = 0;
    
    for (; decl != null;  decl = decl.nextDecl())
      {
	if (i > 0)
	  out.writeSpaceLinear();
	out.print('(');
	out.print(decl);
	out.writeSpaceFill();
	//if (decl.isArtificial ())
        //out.print ("<artificial>");
	//else
	  {
	    if (inits[i] == null)
	      out.print("<null>");
	    else
	      inits[i].print(out);
	    i++;
	  }
	out.print(')');
      }
    out.endLogicalBlock(")");
    out.writeSpaceLinear();
    body.print (out);
    out.endLogicalBlock(")");
  }
}
