package kawa.lang;
import gnu.bytecode.*;

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
 * points to the declaration that points to the heapFrame array.
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
  /** The (interned) name of the new variable. */
  // Redundant with Variable.name, except sym is interned.  FIXME */
  String sym;

  public final String symbol () { return sym; }

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
  Object shadowed;  /* Either a Declaration or a String. */

  /** If non-null, the single expression used to set this variable.
   * If the variable can be set more than once, then value is null. */
  Expression value = QuoteExp.undefined_exp;

  public void noteValue (Expression value)
  {
    // We allow assigning a real value after undefined ...
    if (this.value == QuoteExp.undefined_exp)
      this.value = value;
    else
      this.value = null;
  }

  public Declaration (String s)
  {
    sym = s;
    name = s;
    setType(Type.pointer_type);
  }

  public String string_name () { return sym; }

  /**
   * Insert this into Translator.current_decls.
   * (Used at rewrite time, not eval time.)
   */
  void push (Translator tr)
  {
    Object old_decl = tr.current_decls.get (sym);
    if (old_decl != null)
      shadowed = old_decl;
    tr.current_decls.put (sym, this);
  }

  /** Remove this from Translator.current_decls.
   * (Used at rewrite time, not eval time.)
   */
  void pop (Translator tr)
  {
    if (shadowed == null)
      tr.current_decls.remove (sym);
    else
      tr.current_decls.put (sym, shadowed);
  }
}
