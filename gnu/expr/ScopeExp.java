package gnu.expr;
import gnu.bytecode.*;

/**
 * Abstract class for expressions that add local variable bindings.
 * @author	Per Bothner
 */

public abstract class ScopeExp extends Expression
{
  Scope scope;

  public final Variable firstVar () { return scope.firstVar (); }

  public ScopeExp () { scope = new Scope (); }

  /** The statically enclosing binding contour. */
  public ScopeExp outer;

  /** True iff this scope should be embedded in the outer scope's frame. */
  public boolean shared;

  /** A variable that points to the heap-allocated part of the frame.
   * This is an Object array that contains all the variables
   * captured by an inner Lambda.
   * The size of the array is frameSize.  */
  Declaration heapFrame;

  /** The number of slots used by the current frame.
   * This is only used if shared is set.
   * In a LambdaExp, it is the size needed for the heapFrame array. */
  int frameSize;

  public void assign_space ()
  {
    ScopeExp alloc_scope = this;
    LambdaExp cur_lambda = null;
    for (;;)
      {
	if (alloc_scope instanceof LambdaExp)
	  {
	    cur_lambda = (LambdaExp) alloc_scope;
	    break;
	  }
	ScopeExp alloc_outer = alloc_scope.outer;
	if (alloc_outer == null)
	  break;
	alloc_scope = alloc_outer;
      }
    for (Variable var = firstVar ();  var != null;  var = var.nextVar ()) 
      {
	Declaration decl = (Declaration) var;
	if (decl.offset < 0)
	  {
	    if (! decl.isSimple () || cur_lambda == null)
	      {
		/* A variable captured by an inner Lambda is allocated
		   in the heap frame. */
		if (alloc_scope.heapFrame == null)
		  {
		    alloc_scope.heapFrame
		      = alloc_scope.addDeclaration ("heapFrame",
						    Compilation.objArrayType);
		    alloc_scope.heapFrame.setArtificial (true);
		  }
		decl.baseVariable = alloc_scope.heapFrame;
		decl.offset = alloc_scope.frameSize++;
	      }
	  }
      }
  }

  public LambdaExp currentLambda ()
  {
    ScopeExp exp = this;
    for (;; exp = exp.outer)
      {
	if (exp == null)
	  return null;
	if (exp instanceof LambdaExp)
	  return (LambdaExp) exp;
      }
  }

  /**
   * Find a Declaration by name.
   * @param sym the (interned) name of the Declaration sought
   * @return the matching Declaration, if found;  otherwise null
   */
  public Declaration lookup (String sym)
  {
    for (Variable var = firstVar ();  var != null;  var = var.nextVar ())
      {
	Declaration decl = (Declaration) var;
	if (decl.sym == sym)
	  return decl;
      }
    return null;
  }

  /**
   * Create a new declaration in the current Scope.
   * @param name name (interned) to give to the new Declaration.
   */
  public final Declaration addDeclaration (String name)
  {
    Declaration decl = new Declaration (name);
    addDeclaration(decl);
    return decl;
  }

  /**
   * Create a new declaration in the current Scope.
   * @param name name (interned) to give to the new Declaration.
   * @param type type of the new Declaration.
   */
  public final Declaration addDeclaration (String name, Type type)
  {
    Declaration decl = new Declaration (name);
    addDeclaration(decl);
    decl.setType(type);
    return decl;
  }

  /**
   * Add a Declaration to the current Scope.
   */
  public final void addDeclaration (Declaration decl)
  {
    scope.addVariable (decl);
    decl.context = this;
  }

  public int countDecls ()
  {
    int n = 0;
    for (Variable var = firstVar ();  var != null;  var = var.nextVar ())
      n++;
    return n;
  }
}
