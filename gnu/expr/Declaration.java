// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

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
 * The Declaration's field specifies the Field used.
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

public class Declaration
{
  static int counter;
  /** Unique id number, to ease print-outs and debugging.
   * If negative, a code to specify a builtin function. */
  protected int id = ++counter;

  /** The name of the new variable, either an interned String or a Symbol.
   * This is the source-level (non-mangled) name. */
  Object symbol;

  public void setCode (int code)
  {
    if (code >= 0) throw new Error("code must be negative");
    this.id = code;
  }

  public int getCode () { return id; }

  public ScopeExp context;

  protected Type type;
  public final Type getType() { return type; }
  public final void setType(Type type)
  { this.type = type;  if (var != null) var.setType(type); }
  public final String getName()
  {
    return symbol == null ? null : symbol instanceof Symbol ? ((Symbol) symbol).getName()
      : symbol.toString();
  }
  public final void setName(Object symbol)
  {
    this.symbol = symbol;
  }

  public final Object getSymbol() { return symbol; }
  public final void setSymbol(Object symbol) { this.symbol = symbol; }

  /* Declarations in a ScopeExp are linked together in a linked list. */
  Declaration next;

  public final Declaration nextDecl() { return next; }
  public final void setNext(Declaration next) {  this.next = next; }

  Variable var;
  public Variable getVariable() { return var; }

  public final boolean isSimple()
  { return (flags & IS_SIMPLE) != 0; }

  public final void setSimple(boolean b)
  {
    setFlag(b, IS_SIMPLE);
    if (var != null) var.setSimple(b);
  }

  public final void setSyntax ()
  {
    setSimple(false);
    setFlag(Declaration.IS_CONSTANT | Declaration.IS_SYNTAX);
  }

  /** Return the ScopeExp that contains (declares) this Declaration. */
  public final ScopeExp getContext() { return context; }

  /** Used to link Declarations in a LambdaExp's capturedVars list. */
  Declaration nextCapturedVar;

  /** If non-null, field is relative to base.
   * If IS_FLUID, base points to IS_UNKNOWN Symbol. */
  public Declaration base;

  public Field field;

  /** If this is a field in some object, load a reference to that object. */
  public void loadOwningObject (Compilation comp)
  {
    if (base != null)
      base.load(null, 0, comp, Target.pushObject);
    else
      getContext().currentLambda().loadHeapFrame(comp);
  }

  public void load (Declaration owner, int flags,
                    Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    if (isAlias() && (flags & ReferenceExp.DONT_DEREFERENCE) == 0
	&& value instanceof ReferenceExp)
      {
        ReferenceExp rexp = (ReferenceExp) value;
        Declaration orig = rexp.binding;
        if (orig != null
            && (owner == null || ! orig.needsContext()))
          {
            orig.load(rexp.context, flags, comp, target);
            return;
          }
      }
    CodeAttr code = comp.getCode();
    Type rtype = getType();
    if (! isIndirectBinding() && (flags & ReferenceExp.DONT_DEREFERENCE) != 0)
      {
        if (field == null)
          throw new Error("internal error: cannot take location of "+this);
        Method meth;
        ClassType ltype;
        if (field.getStaticFlag())
          {
            ltype = ClassType.make("gnu.kawa.reflect.StaticFieldLocation");
            meth = ltype.getDeclaredMethod("make", 2);
          }
        else
          {
            ltype = ClassType.make("gnu.kawa.reflect.FieldLocation");
            meth = ltype.getDeclaredMethod("make", 3);

            if (owner != null)
              owner.load(null, 0, comp, Target.pushObject);
            else
              loadOwningObject(comp);
          }
        comp.compileConstant(field.getDeclaringClass().getName());
        comp.compileConstant(field.getName());
        code.emitInvokeStatic(meth);
        rtype = ltype;
      }
    else
      {
        Object val;
        if (field != null)
          {
            if (! field.getStaticFlag())
              {
                if (owner != null)
                  owner.load(null, 0, comp, Target.pushObject);
                else
                  loadOwningObject(comp);
                code.emitGetField(field);
              }
            else
              code.emitGetStatic(field);
          }
        else if (isIndirectBinding() && comp.immediate && getVariable() == null)
          {
            // This is a bit of a kludge.  See comment in ModuleExp.evalModule.
            Environment env = comp.getEnvironment();
            Symbol sym = symbol instanceof Symbol ? (Symbol) symbol
              : env.getSymbol(symbol.toString());
            Object property = null;
            if (isProcedureDecl()
                && comp.getLanguage().hasSeparateFunctionNamespace())
              property = EnvironmentKey.FUNCTION;
            gnu.mapping.Location loc = env.getLocation(sym, property);
            comp.compileConstant(loc, Target.pushValue(Compilation.typeLocation));
          }
        else if (comp.immediate && (val = getConstantValue()) != null)
          {
            comp.compileConstant(val);
          }
        else
          {
            Variable var = getVariable();
            ClassExp cl;
            if (context instanceof ClassExp && var == null
                && ! getFlag(PROCEDURE)
                && (cl = (ClassExp) context).isMakingClassPair())
              {
                String getName = ClassExp.slotToMethodName("get", getName());
                Method getter = cl.type.getDeclaredMethod(getName, 0);
                cl.loadHeapFrame(comp);
                code.emitInvoke(getter);
              }
            else
              {
                if (var == null)
                  var = allocateVariable(code);
                code.emitLoad(var);
              }
          }
        if (isIndirectBinding()
            && (flags & ReferenceExp.DONT_DEREFERENCE) == 0)
          {
            code.emitInvokeVirtual(Compilation.getLocationMethod);
            rtype = Type.pointer_type;
          }
      }
    if (target instanceof SeriesTarget
	&& getFlag(Declaration.IS_SINGLE_VALUE))
      // A kludge until we get a better type system.
      ((SeriesTarget) target).compileFromStackSimple(comp, rtype);
    else
      target.compileFromStack(comp, rtype);
  }

  /* Compile code to store a value (which must already be on the
     stack) into this variable. */
  public void compileStore (Compilation comp)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    if (isSimple ())
      code.emitStore(getVariable());
    else
      {
        if (! field.getStaticFlag())
          {
            loadOwningObject(comp);
            code.emitSwap();
	    code.emitPutField(field);
          }
	else
	  code.emitPutStatic(field);
      }
  }

  /** If non-null, the single expression used to set this variable.
   * If the variable can be set more than once, then value is null. */
  protected Expression value = QuoteExp.undefined_exp;

  /** The value of this <code>Declaration</code>, if known.
   * Usually the expression used to initialize the <code>Declaration</code>,
   * or null if the <code>Declaration</code> can be assigned a different
   * value after initialization.  Note that this is the semantic value: If the
   * <code>INDIRECT_LOCATION</code> is set, then <code>getValue</code> is the
   * value <em>after</em> de-referencing the resulting <code>Location</code>.
   * An exception is if <code>isAlias()</code>; in that case
   * <code>getValue()</code> is an expression yielding a <code>Location</code>
   * which needs to be de-referenced to get this <code>Declaration</code>'s
   * actual value.
   */
  public final Expression getValue() { return value; }

  /** If getValue() is a constant, return the constant value, otherwise null. */
  public final Object getConstantValue()
  {
    if (! (value instanceof QuoteExp) || value == QuoteExp.undefined_exp)
      return null;
    return ((QuoteExp) value).getValue();
  }

  /** This prefix is prepended to field names for unknown names. */
  static final String UNKNOWN_PREFIX = "loc$";

  /** This prefix is used in field names for a declaration that has
   * both EXTERNAL_ACCESS and IS_PRIVATE set. */
  public static final String PRIVATE_PREFIX = "$Prvt$";

  /** If this flag is set then to get the actual value you have to dereference
   * a <code>gnu.mapping.Location</code>.  I.e. this <code>Declaration</code>'s
   * <code>var</code> or <code>field</code> does not contain the
   * <code>Declaration</code>'s value directly, but rather yields a
   * <code>Location</code> that contains the <code>Declaration</code>'s value.
   * Note that this flag indicates the <em>representation</em>:
   * The result of <code>getValue()</code> is not the location, but the 
   * semantic value. after dereferencing.  Likewise <code>getType</code> is
   * the value after de-referencing, not a <code>Location</code> sub-class. */
  static final int INDIRECT_BINDING = 1;

  static final int CAN_READ = 2;
  static final int CAN_CALL = 4;
  static final int CAN_WRITE = 8;
  static final int IS_FLUID = 0x10;
  static final int PRIVATE = 0x20;
  static final int IS_SIMPLE = 0x40;

  /** True if in the function namespace, for languages that distinguishes them.
   * I.e. a function definition or macro definition. */
  static final int PROCEDURE = 0x80;

  public static final int IS_ALIAS = 0x100;

  /** Set if this is just a declaration, not a definition. */
  public static final int NOT_DEFINING = 0x200;

  public static final int EXPORT_SPECIFIED = 0x400;
  public static final int STATIC_SPECIFIED = 0x800;
  public static final int NONSTATIC_SPECIFIED = 0x1000;
  public static final int TYPE_SPECIFIED = 0x2000;
  public static final int IS_CONSTANT = 0x4000;
  public static final int IS_SYNTAX = 0x8000;
  public static final int IS_UNKNOWN = 0x10000;
  public static final int IS_IMPORTED = 0x20000;

  // This should be a type property, not a variable property, at some point!
  public static final int IS_SINGLE_VALUE = 0x40000;

  /** This flag bit is set if this can be be acceessed from other modules.
   * Ignored unless PRIVATE.
   * Used when an exported macro references a non-exported name. */
  public static final int EXTERNAL_ACCESS = 0x80000;

  public final boolean needsExternalAccess ()
  {
    return (flags & EXTERNAL_ACCESS+PRIVATE) == EXTERNAL_ACCESS+PRIVATE;
  }

  /** If we need a 'context' supplied from a ReferenceExp or 'this. */
  public final boolean needsContext ()
  {
    return base == null && field != null && ! field.getStaticFlag();
  }

  /** True if this is a field or method in a class definition. */
  public static final int FIELD_OR_METHOD = 0x100000;

  /** Set if this declares a namespace prefix (as in XML namespaces). */
  public static final int IS_NAMESPACE_PREFIX = 0x200000;

  public static final int PRIVATE_ACCESS = 0x1000000;
  public static final int PRIVATE_SPECIFIED = PRIVATE_ACCESS; /* deprecated*/
  public static final int PROTECTED_ACCESS = 0x2000000;
  public static final int PUBLIC_ACCESS = 0x4000000;
  public static final int PACKAGE_ACCESS = 0x8000000;

  public static final int IS_DYNAMIC = 0x10000000;

  /** Initialize in <code>&lt;init&gt;</code>/<code>&lt;clinit&gt;</code>
   * rather than in <code>run</code>/<code>$run$</code>. */
  public static final int EARLY_INIT = 0x20000000;

  protected int flags = IS_SIMPLE;

  public final boolean getFlag (int flag)
  {
    return (flags & flag) != 0;
  }

  public final void setFlag (boolean setting, int flag)
  {
    if (setting) flags |= flag;
    else flags &= ~flag;
  }

  public final void setFlag (int flag)
  {
    flags |= flag;
  }

  public final boolean isPublic()
  { return context instanceof ModuleExp && (flags & PRIVATE) == 0; }

  public final boolean isPrivate() { return (flags & PRIVATE) != 0; }

  public final void setPrivate(boolean isPrivate)
  {
    setFlag(isPrivate, PRIVATE);
  }

  public short getAccessFlags (short defaultFlags)
  {
    if (getFlag(Declaration.PRIVATE_ACCESS))
      return Access.PRIVATE;
    if (getFlag(Declaration.PROTECTED_ACCESS))
      return Access.PROTECTED;
    if (getFlag(Declaration.PACKAGE_ACCESS))
      return 0;
    if (getFlag(Declaration.PUBLIC_ACCESS))
      return Access.PUBLIC;
    return defaultFlags;
  }

  public final boolean isAlias() { return (flags & IS_ALIAS) != 0; }
  public final void setAlias(boolean flag) { setFlag(flag, IS_ALIAS); }

  /** True if this is a fluid binding (in a FluidLetExp). */
  public final boolean isFluid () { return (flags & IS_FLUID) != 0; }

  public final void setFluid (boolean fluid) { setFlag(fluid, IS_FLUID); }

  public final boolean isProcedureDecl () { return (flags & PROCEDURE) != 0; }

  public final void setProcedureDecl (boolean val) { setFlag(val, PROCEDURE); }

  public final boolean isNamespaceDecl ()
  {
    return (flags & IS_NAMESPACE_PREFIX) != 0;
  }   

  /** True if the value of the variable is the contents of a Location.
   * @see #INDIRECT_BINDING */
  public final boolean isIndirectBinding()
  { return (flags & INDIRECT_BINDING) != 0; }

  /** Note that the value of the variable is the contents of a Location.
   * @see #INDIRECT_BINDING */
  public final void setIndirectBinding(boolean indirectBinding)
  {
    setFlag(indirectBinding, INDIRECT_BINDING);
  }

  /* Note:  You probably want to use !ignorable(). */
  public final boolean getCanRead() { return (flags & CAN_READ) != 0; }
  public final void setCanRead(boolean read)
  {
    setFlag(read, CAN_READ);
  }
  public final void setCanRead()
  {
    setFlag(true, CAN_READ);
    if (base != null)
      base.setCanRead();
  }

  public final boolean getCanCall() { return (flags & CAN_CALL) != 0; }
  public final void setCanCall(boolean called) { setFlag(called, CAN_CALL); }
  public final void setCanCall()
  {
    setFlag(true, CAN_CALL);
    if (base != null)
      base.setCanRead();
  }

  public final boolean getCanWrite()
  { return (flags & CAN_WRITE) != 0; }

  public final void setCanWrite(boolean written)
  {
    if (written) flags |= CAN_WRITE;
    else flags &= ~CAN_WRITE;
  }

  public final void setCanWrite()
  {
    flags |= CAN_WRITE;
    if (base != null)
      base.setCanRead();
  }

  /** True if we never need to access this declaration. */
  // rename to isAccessed?
  public boolean ignorable()
  {
    if (getCanRead() || isPublic())
      return false;
    if (getCanWrite() && getFlag(IS_UNKNOWN))
      return false;
    if (! getCanCall())
      return true;
    Expression value = getValue();
    if (value == null || ! (value instanceof LambdaExp))
      return false;
    LambdaExp lexp = (LambdaExp) value;
    return ! lexp.isHandlingTailCalls() || lexp.getInlineOnly();
  }

  /** Does this variable need to be initialized or is default ok
   */
  public boolean needsInit()
  {
    // This is a kludge.  Ideally, we should do some data-flow analysis.
    // But at least it makes sure require'd variables are not initialized.
    return ! ignorable()
      && ! (value == QuoteExp.nullExp && base != null);
  }

  public boolean isStatic()
  {
    if (getFlag(STATIC_SPECIFIED))
      return true;
    if (getFlag(NONSTATIC_SPECIFIED))
      return false;
    LambdaExp lambda = context.currentLambda();
    return lambda instanceof ModuleExp
      && ((ModuleExp) lambda).isStatic();
  }

  public final boolean isLexical()
  {
    return ! isFluid() && ! getFlag(IS_UNKNOWN);
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
    else if (this.value != value)
      {
	if (this.value instanceof LambdaExp) 
	  ((LambdaExp) this.value).nameDecl = null;
	this.value = null;
      }
  }

  protected Declaration()
  {
  }

  public Declaration (Variable var)
  {
    this(var.getName(), var.getType());
    this.var = var;
  }

  public Declaration (Object name)
  {
    this(name, Type.pointer_type);
  }

  public Declaration (Object s, Type type)
  {
    setName(s);
    setType(type);
  }

  public Declaration (Object name, Field field)
  {
    this(name, field.getType());
    this.field = field;
    setSimple(false);
  }

  Method makeLocationMethod = null;

  /** Create a Location object, given that isIndirectBinding().
      Assume the initial value is already pushed on the stack;
      leaves initialized Location object on stack.  */
  public void pushIndirectBinding (Compilation comp)
  {
    CodeAttr code = comp.getCode();
    code.emitPushString(getName());
    if (makeLocationMethod == null)
      {
	Type[] args = new Type[2];
	args[0] = Type.pointer_type;
	args[1] = Type.string_type;
	makeLocationMethod
	  = Compilation.typeLocation.addMethod("make", args,
					      Compilation.typeLocation,
					      Access.PUBLIC|Access.STATIC);
      }
    code.emitInvokeStatic(makeLocationMethod);
  }

  public final Variable allocateVariable(CodeAttr code)
  {
    if (! isSimple())
      return null;
    if (var == null)
      {
        String vname = null;
        if (symbol != null)
          vname = Compilation.mangleNameIfNeeded(getName());
	if (isAlias() && getValue() instanceof ReferenceExp)
	  {
	    Declaration base = followAliases(this);
	    var = base == null ? null : base.var;
	  }
	else
	  {
	    Type type = isIndirectBinding() ? Compilation.typeLocation
	      : getType().getImplementationType();
	    var = context.getVarScope().addVariable(code, type, vname);
	  }
      }
    return var;
  }

  String filename;
  int position;

  public final void setFile (String filename)
  {
    this.filename = filename;
  }

  public final void setLine (int lineno, int colno)
  {
    position = (lineno << 12) + colno;
  }

  public final void setLine (int lineno)
  {
    setLine (lineno, 0);
  }

  public final String getFile ()
  {
    return filename;
  }

  /** Get the line number of (the start of) this Expression.
    * The "first" line is line 1. */
  public final int getLine ()
  {
    return position >> 12;
  }

  public final int getColumn ()
  {
    return position & ((1 << 12) - 1);
  }

  public void printInfo(OutPort out)
  {
    StringBuffer sbuf = new StringBuffer();
    printInfo(sbuf);
    out.print(sbuf.toString());
  }

  public void printInfo(StringBuffer sbuf)
  {
    sbuf.append(symbol);
    sbuf.append('/');
    sbuf.append(id);
    /*
    int line = getLine();
    if (line != 0)
      {
	sbuf.append("/line:");
	sbuf.append(line);
	int column = getColumn();
	if (column != 0)
	  {
	    sbuf.append(':');
	    sbuf.append(column);
	  }
      }
    */
    sbuf.append("/fl:");
    sbuf.append(Integer.toHexString(flags));
    Type t = getType();
    if (t != null && t != Type.pointer_type)
      {
	sbuf.append("::");
	sbuf.append(t.getName());
      }
  }


  public String toString()
  {
    return "Declaration["+symbol+'/'+id+']';
    /*
    StringBuffer sbuf = new StringBuffer();
    sbuf.append("Declaration[");
    printInfo(sbuf);
    sbuf.append(']');
    return sbuf.toString();
    */
  }

  public static Declaration followAliases (Declaration decl)
  {
    while (decl != null && decl.isAlias())
      {
	Expression declValue = decl.getValue();
	if (! (declValue instanceof ReferenceExp))
	  break;
	ReferenceExp rexp = (ReferenceExp) declValue;
	Declaration orig = rexp.binding;
	if (orig == null)
	  break;
	decl = orig;
      }
    return decl;
  }

  public void makeField(Compilation comp, Expression value)
  {
    setSimple(false);
    boolean external_access = needsExternalAccess();
    int fflags = 0;
    boolean isConstant = getFlag(IS_CONSTANT);
    boolean typeSpecified = getFlag(TYPE_SPECIFIED);
    if (isPublic() && ! isConstant && ! typeSpecified)
      setIndirectBinding(true);
    if (! isPrivate() || external_access)
      fflags |= Access.PUBLIC;
    if (isStatic()
	|| (isConstant && value instanceof QuoteExp)
	|| (value instanceof ClassExp
	    && ! ((LambdaExp) value).getNeedsClosureEnv()))
      fflags |= Access.STATIC;
    if (isIndirectBinding() || isConstant)
      fflags |= Access.FINAL;
    Type ftype = getType().getImplementationType();
    if (isIndirectBinding() && ! ftype.isSubtype(Compilation.typeLocation))
      if (getFlag(EARLY_INIT) && isAlias())
        ftype = ClassType.make("gnu.kawa.reflect.FieldLocation");
      else
        ftype = Compilation.typeLocation;
    String fname = getName();
    fname = Compilation.mangleNameIfNeeded(fname);
    if (getFlag(IS_UNKNOWN))
      fname = UNKNOWN_PREFIX + fname;
    if (external_access)
      fname = PRIVATE_PREFIX + fname;
    int nlength = fname.length();
    int counter = 0;
    while (comp.mainClass.getDeclaredField(fname) != null)
      fname = fname.substring(0, nlength) + '$' + (++ counter);

    field = comp.mainClass.addField (fname, ftype, fflags);
    if (value instanceof QuoteExp)
      {
	Object val = ((QuoteExp) value).getValue();
	if (val.getClass().getName().equals(ftype.getName()))
	  {
	    Literal literal = comp.litTable.findLiteral(val);
	    if (literal.field == null)
	      literal.assign(field, comp.litTable);
	  }
	else if (ftype instanceof PrimType
		 || "java.lang.String".equals(ftype.getName()))
	  {
	    if (val instanceof gnu.text.Char)
	      val = gnu.math.IntNum.make(((gnu.text.Char) val).intValue());
	    field.setConstantValue(val, comp.mainClass);
	    return;
	  }
      }
    // The EARLY_INIT case is handled in SetExp.compile.
    if (! getFlag(EARLY_INIT)
	&& (isIndirectBinding()
	    || (value != null && ! (value instanceof ClassExp))))
      {
	BindingInitializer.create(this, value, comp);
      }
  }

  public static Declaration getDeclaration(Named proc)
  {
    return getDeclaration(proc, proc.getName());
  }

  public static Declaration getDeclaration(Object proc, String name)
  {
    if (name != null)
      {
        Class procClass = PrimProcedure.getProcedureClass(proc);
        if (procClass != null)
          {
            ClassType procType = (ClassType) Type.make(procClass);
            String fname = Compilation.mangleNameIfNeeded(name);
            gnu.bytecode.Field procField = procType.getDeclaredField(fname);
            if (procField != null)
              {
                int fflags = procField.getModifiers();
                if ((fflags & Access.STATIC) != 0)
                  {
                    Declaration decl = new Declaration(name, procField);
                    decl.noteValue(new QuoteExp(proc));
                    if ((fflags & Access.FINAL) != 0)
                      decl.setFlag(Declaration.IS_CONSTANT);
                    return decl;
                  }
              }
          }
      }
    return null;
  }
}
