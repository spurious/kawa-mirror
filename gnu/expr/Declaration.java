package gnu.expr;
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
 * variable is stored in a field of the LambdaExp's heapFrame variable.
 * (The latter declaration has isSimple and isArtificial true.)
 * The heapFrame reference is an instance of the LambdaExp's heapFrameLambda;
 * the Declaration's field specifies the Field used.
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
 * isSimple(), and isArtificial set.  The "foo" Declaration has isParameter()
 * set.  The procedure prologue copies "fooIncoming" to "foo", which acts
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
  static int counter;
  /** Unique id number, to ease print-outs and debugging. */
  protected int id = ++counter;

  /** The (interned) name of the new variable.
   * This is the source-level (non-mangled) name. */
  protected String sym;

  public final String symbol () { return sym; }

  ScopeExp context;

  /** Return the ScopeExp that contains (declares) this Declaration. */
  public final ScopeExp getContext() { return context; }

  /** Used to link Declarations in a LambdaExp's capturedVars list. */
  Declaration nextCapturedVar;

  Field field;

  /** If this is a field in some object, load a reference to that object. */
  public void loadOwningObject (Compilation comp)
  {
    getContext().currentLambda().loadHeapFrame(comp);
  }

  static int fieldNum;
  public void assignField (Compilation comp)
  {
    setSimple(false);
    String name = comp.mangleName(getName())+'_'+(++fieldNum);
    field = comp.curClass.addField(name, getType(), 0);
  }

  public void load (Compilation comp)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    if (field != null)
      {
        if (! field.getStaticFlag())
          loadOwningObject(comp);
	code.emitGetField(field);
      }
    else
      code.emitLoad(this);
  }

  /* Compile code to store a value (which must already be on the
     stack) into this variable. */
  public void compileStore (Compilation comp)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    if (isSimple ())
      code.emitStore(this);
    else
      {
        if (! field.getStaticFlag())
          {
            loadOwningObject(comp);
            code.emitSwap();
          }
	code.emitPutField(field);
      }
  }

  /** If non-null, the Declaration that we "shadow" (hide). */
  public Object shadowed;  /* Either a Declaration or a String. */

  /** If non-null, the single expression used to set this variable.
   * If the variable can be set more than once, then value is null. */
  protected Expression value = QuoteExp.undefined_exp;

  public Expression getValue() { return value; }

  static final int INDIRECT_BINDING = 1;
  static final int CAN_READ = 2;
  static final int CAN_CALL = 4;
  static final int CAN_WRITE = 8;
  static final int IS_FLUID = 16;
  static final int PRIVATE = 32;
  int flags;

  public final void setFlag (boolean setting, int flag)
  {
    if (setting) flags |= flag;
    else flags &= ~flag;
  }

  public final boolean isPrivate() { return (flags & PRIVATE) != 0; }

  public final void setPrivate(boolean isPrivate)
  {
    setFlag(isPrivate, PRIVATE);
  }

  /** True if this is a fluid binding (in a FluidLetExp). */
  public final boolean isFluid () { return (flags & IS_FLUID) != 0; }

  public final void setFluid (boolean fluid) { setFlag(fluid, IS_FLUID); }

  /** True if the value of the variable is the contents of a Binding. */
  public final boolean isIndirectBinding()
  { return (flags & INDIRECT_BINDING) != 0; }
  public final void setIndirectBinding(boolean indirectBinding)
  {
    if (indirectBinding) flags |= INDIRECT_BINDING;
    else flags &= ~INDIRECT_BINDING;
  }

  /* Note:  You probably want to use !ignorable(). */
  public final boolean getCanRead() { return (flags & CAN_READ) != 0; }
  public final void setCanRead(boolean read) { setFlag(read, CAN_READ); }

  public final boolean getCanCall() { return (flags & CAN_CALL) != 0; }
  public final void setCanCall(boolean called) { setFlag(called, CAN_CALL); }

  public final boolean getCanWrite()
  { return (flags & CAN_WRITE) != 0; }
  public final void setCanWrite(boolean written)
  {
    if (written) flags |= CAN_WRITE;
    else flags &= ~CAN_WRITE;
  }

  /** True if we never need to access this declaration. */
  // rename to isAccessed?
  public boolean ignorable()
  {
    if (getCanRead())
      return false;
    if (! getCanCall())
      return true;
    if (value == null || ! (value instanceof LambdaExp))
      return false;
    LambdaExp lexp = (LambdaExp) value;
    return ! lexp.isHandlingTailCalls() || lexp.getInlineOnly();
  }

  public boolean isStatic()
  {
    return context instanceof ModuleExp;
  }

  public final boolean isLexical()
  {
    return ! isFluid() && ! isStatic();
  }

  /** List of ApplyExp where this declaration is the function called.
   * The applications are chained using their nextcall fields. */
  public ApplyExp firstCall;

  public void noteValue (Expression value)
  {
    // We allow assigning a real value after undefined ...
    if (this.value == QuoteExp.undefined_exp)
      {
	if (value instanceof LambdaExp)
	  ((LambdaExp) value).nameDecl = this;
	this.value = value;
      }
    else
      {
	if (this.value instanceof LambdaExp) 
          ((LambdaExp) this.value).nameDecl = null; 
	this.value = null;
      }
  }

  protected Declaration()
  {
  }

  public Declaration (String name)
  {
    this(name, Type.pointer_type);
  }

  public Declaration (String s, Type type)
  {
    sym = s;
    if (s != null)
      {
        name = Compilation.mangleName(s);
        if (s.equals(name))
          name = s;
      }
    setType(type);
  }

  public String string_name () { return sym; }

  Method makeBindingMethod = null;

  /** Create a Binding object, given that isIndirectBinding().
      Assume the initial value is already pushed on the stack;
      leaves initialized Binding object on stack.  */
  public void pushIndirectBinding (Compilation comp)
  {
    CodeAttr code = comp.getCode();
    code.emitPushString(symbol());
    if (makeBindingMethod == null)
      {
	ClassType typeBinding = ClassType.make("gnu.mapping.Binding");
	Type[] args = new Type[2];
	args[0] = Type.pointer_type;
	args[1] = Type.string_type;
	makeBindingMethod
	  = typeBinding.addMethod("make", args, typeBinding,
				  Access.PUBLIC|Access.STATIC);
      }
    code.emitInvokeStatic(makeBindingMethod);
  }

  /** Generate code to initialize the location for this.
      Assume the initial value is already pushed on the stack. */
  public void initBinding (Compilation comp)
  {
    if (isIndirectBinding())
      {
	pushIndirectBinding(comp);
	CodeAttr code = comp.getCode();
	code.emitStore(this);
      }
    else
      compileStore(comp);
  }

  public String toString()
  {
    return "Declaration["+getName()+'/'+id+']';
  }

}
