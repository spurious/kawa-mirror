package kawa.lang;
import codegen.*;

/**
 * The static information associated with a local variable binding.
 * @author	Per Bothner
 *
 * These are the kinds of Declaration we use:
 *
 * A local variable that is not captured by an inner lambda is stored
 * in a Java local variables slot (register).  The predicate isSimple ()
 * is true, and offset is the number of the local variable slot.
 *
 * If a local variable is captured by an inner lambda, the
 * variable is stored in the heapFrame array variable.
 * The offset field indicates the element in the heapFrame array.
 * The baseVariable field points to the declaration of the headFrame array.
 *
 * If a function needs a heapFrame array, then LambdaExp.heapFrame
 * points to the declaration variable that points to the heapFrame array.
 * This declaration has isSimple and isArtificial true.
 *
 * The heapFrame array is passed to the constructors of inferior
 * procedures that need a static link.  It is pointed to by the
 * staticLink field of the generated procedure object.  When
 * the procedure is applied, the procedure prologue copies the
 * staticLink field into the local staticLink variable.
 * That staticLink variable has isArtificial set.
 * In the case of multi-level capture, then the staticLink variable
 * may in turn also be captured.
 *
 * If a function takes a fixed number of parameters, at most four,
 * then the arguments are passed in Java registers 1..4.
 * If a parameter is not captured by an inner lambda, the parameter
 * has the flags isSimple and isParameter true.
 *
 * A parameter named "foo" that is captured by an inner lambda is represented
 * using two Declarations, named "foo" and "fooIncoming".
 * The "fooIncoming" declaration is the actual parameter as passed
 * by the caller using a Java local variable slot.  It has isParameter(),
 * isSimple(), and isArtificial set.  Its baseVariable field points to
 * the "foo" Declaration.  The "foo" Declaration has isParameter() set.
 * Its baseVariable points to the heapFrame Declaration.
 * The procedure prologue copies "fooIncoming" to "foo", which acts
 * just like a normal captured local variable.
 *
 * If a function takes more than 4 or a variable number of parameters,
 * the arguments are passed in an array (using the applyN virtual method).
 * This array is referenced by the argsArray declaration, which has
 * isSimple(), isParameter(), and isArtificial() true, and its offset is 1.
 * The parameters are copied into the program-named variables by the
 * procedure prologue, so the parameters henceforth act like local variables.
 */

public class Declaration extends Variable
{
  /** The name of the new variable. */
  Symbol sym;

  public final Symbol symbol () { return sym; }

  ScopeExp context;

  /* SEMI-OBOSLETE (misleading):
   * A frame is a set of local variables.  In interpreted code, the frame
   * is mapped into a single Object array, indexed by Variable.offset.
   * In compiled code, the frame is mapped into the local variable slots
   * of the current method, where Variable.offset is the variable number.
   *
   * The value of a simple variable is stored directly in the frame
   * (array element or local variable slot).  This only works for variables
   * that are not captured by inner functions.  The index field specified
   * the array index or local variable slot;  the baseVariable is null.
   *
   * A 'frame-indirect' variable is stored in a heap array pointed to
   * by another variable.  The baseVariable is that other variable,
   * the index field specifies the array element.
   */
  public Declaration baseVariable;

  /** If non-null, the Declaration that we "shadow" (hide). */
  Declaration shadowed;

  public Declaration (Symbol s)
  {
    sym = s;
    name = ClassType.to_utf8 (s.toString ());
    type = Type.pointer_type;
  }

  public String string_name () { return sym.toString (); }

  public Object getValue (Object[] frame)
  {
    //System.err.println ("getvalue: " + sym + " offset:"+offset + " context:"+context + " this:"+this);
    if (context.heapFrame == this)
      return frame;
    if (baseVariable != null)
      frame = (Object[]) baseVariable.getValue (frame);
    return frame[offset];
  }

  public Object[] getFrame (Environment env)
  {
    //System.err.println ("getframe: " + sym + " offset:"+offset);
    Object frame[] = env.values;
    ScopeExp curScope = env.scope;
    ScopeExp declScope = context;
    while (declScope.shared) declScope = declScope.outer;
    for (; curScope != declScope; curScope = curScope.outer)
      {
	if (curScope instanceof LambdaExp)
	  {
	    LambdaExp lambda = (LambdaExp) curScope;
	    if (lambda.staticLink == null)
	      throw new Error ("interal error - canot find static link");
	    frame = (Object[]) lambda.staticLink.getValue (frame);
	  }
      }
    return frame;
  }

  Object getValue (Environment env)
  {
    if (context.heapFrame == this)
      return env.values;
    //System.err.println ("getvalue: " + sym);
    return getValue (getFrame (env)); 
  }

  void setValue (Environment env, Object value)
  {
    Object[] frame = getFrame (env);
    if (baseVariable != null)
      frame = (Object[]) baseVariable.getValue (frame);
    frame[offset] = value;
  }

  /**
   * Insert this into Interpreter.current_decls.
   * (Used at rewrite time, not eval time.)
   */
  void push (Interpreter interp)
  {
    Declaration old_decl = (Declaration) interp.current_decls.get (sym);
    if (old_decl != null)
      shadowed = old_decl;
    interp.current_decls.put (sym, this);
  }

  /** Remove this from Interpreter.current_decls.
   * (Used at rewrite time, not eval time.)
   */
  void pop (Interpreter interp)
  {
    if (shadowed == null)
      interp.current_decls.remove (sym);
    else
      interp.current_decls.put (sym, shadowed);
  }
}
