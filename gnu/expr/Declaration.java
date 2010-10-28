// Copyright (c) 2003, 2009, 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.text.SourceLocator;

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
 * If a function takes more than 4 or a variable number of parameters,
 * the arguments are passed in an array (using the applyN virtual method).
 * This array is referenced by the argsArray declaration, which has
 * isSimple(), isParameter(), and isArtificial() true, and its offset is 1.
 * The parameters are copied into the program-named variables by the
 * procedure prologue, so the parameters henceforth act like local variables.
 */

public class Declaration 
  implements SourceLocator
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

  /** The type of the value of this Declaration.
   * It is null if the type is un-specified and not yet inferred.
   * Will get set implicitly by getType, to avoid inconsistencies.
   */
  protected Type type;
  protected Expression typeExp;
  public final Expression getTypeExp()
  {
    if (typeExp == null)
      setType(Type.objectType);
    return typeExp;
  }
  public final Type getType()
  {
    if (type == null)
      setType(Type.objectType);
    return type;
  }
  public final void setType(Type type)
  {
    this.type = type;
    if (var != null) var.setType(type);
    typeExp = QuoteExp.getInstance(type);
  }

  public final void setTypeExp (Expression typeExp)
  {
    this.typeExp = typeExp;
    Type t = null;
    if (typeExp instanceof TypeValue)
      t = ((TypeValue) typeExp).getImplementationType();
    else
      t = Language.getDefaultLanguage().getTypeFor(typeExp, false);
    if (t == null)
      t = Type.pointer_type;
    this.type = t;
    if (var != null) var.setType(t);
  }

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

  /** Index in evalFrame for this scope, if interpreting. */
  int evalIndex;

  Variable var;
  public Variable getVariable() { return var; }

  public final boolean isSimple()
  { return (flags & IS_SIMPLE) != 0; }

  public final void setSimple(boolean b)
  {
    setFlag(b, IS_SIMPLE);
    if (var != null && ! var.isParameter()) var.setSimple(b);
  }

  public final void setSyntax ()
  {
    setSimple(false);
    setFlag(IS_CONSTANT|IS_SYNTAX|EARLY_INIT);
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
  void loadOwningObject (Declaration owner, Compilation comp)
  {
    if (owner == null)
      owner = base;
    if (owner != null)
      owner.load(null, 0, comp, Target.pushObject);
    else
      getContext().currentLambda().loadHeapFrame(comp);
  }

  public void load (AccessExp access, int flags,
                    Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    Declaration owner = access == null ? null : access.contextDecl();
    if (isAlias() && value instanceof ReferenceExp)
      {
        ReferenceExp rexp = (ReferenceExp) value;
        Declaration orig = rexp.binding;
        if (orig != null
            && ((flags & ReferenceExp.DONT_DEREFERENCE) == 0
                || orig.isIndirectBinding())
            && (owner == null || ! orig.needsContext()))
          {
            orig.load(rexp, flags, comp, target);
            return;
          }
      }
    if (isFluid())
      {
        if (context instanceof FluidLetExp)
          {
            base.load(access, flags, comp, target);
            return;
          }
      }
    CodeAttr code = comp.getCode();
    Type rtype = getType();
    if (! isIndirectBinding()
        && (flags & ReferenceExp.DONT_DEREFERENCE) != 0)
      {
        if (field == null)
          throw new Error("internal error: cannot take location of "+this);
        Method meth;
        ClassType ltype;
        boolean immediate = comp.immediate;
        if (field.getStaticFlag())
          {
            ltype = ClassType.make("gnu.kawa.reflect.StaticFieldLocation");
            meth = ltype.getDeclaredMethod("make", immediate ? 1 : 2);
          }
        else
          {
            ltype = ClassType.make("gnu.kawa.reflect.FieldLocation");
            meth = ltype.getDeclaredMethod("make", immediate ? 2 : 3);

            loadOwningObject(owner, comp);
          }
        if (immediate)
          comp.compileConstant(this);
        else
          {
            comp.compileConstant(field.getDeclaringClass().getName());
            comp.compileConstant(field.getName());
          }
        code.emitInvokeStatic(meth);
        rtype = ltype;
      }
    else
      {
        Object val;
        if (field != null)
          {
            comp.usedClass(field.getDeclaringClass());
            comp.usedClass(field.getType());
            if (! field.getStaticFlag())
              {
                loadOwningObject(owner, comp);
                code.emitGetField(field);
              }
            else
              code.emitGetStatic(field);
          }
        else if (isIndirectBinding() && comp.immediate && getVariable() == null)
          {
            // This is a bit of a kludge.  See comment in ModuleExp.evalModule.
            Environment env = Environment.getCurrent();
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
            comp.compileConstant(val, target);
            return;
          }
        else if (value != QuoteExp.undefined_exp && ignorable()
                 && ! (value instanceof LambdaExp
                       && ((LambdaExp) value).outer instanceof ModuleExp))
          {
            value.compile(comp, target);
            return;
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

            String filename;
            int line;
            if (access != null
                && (filename = access.getFileName()) != null
                && (line = access.getLineNumber()) > 0)
              {
                // Wrap call to Location.get by a catch handler that
                // calls setLine on the UnboundLocationException.
                ClassType typeUnboundLocationException
                  = ClassType.make("gnu.mapping.UnboundLocationException");
                // See comment in CheckedTarget.emitCheckedCoerce.
                boolean isInTry = code.isInTry();
                int column = access.getColumnNumber();
                Label startTry = new Label(code);
                startTry.define(code);
                code.emitInvokeVirtual(Compilation.getLocationMethod);
                Label endTry = new Label(code);
                endTry.define(code);
                Label endLabel = new Label(code);
                endLabel.setTypes(code);
                if (isInTry)
                  code.emitGoto(endLabel);
                else
                  code.setUnreachable();
                int fragment_cookie = 0;
                if (! isInTry)
                  fragment_cookie = code.beginFragment(endLabel);
                code.addHandler(startTry, endTry, typeUnboundLocationException);

                code.emitDup(typeUnboundLocationException);
                code.emitPushString(filename);
                code.emitPushInt(line);
                code.emitPushInt(column);
                code.emitInvokeVirtual(typeUnboundLocationException
                                       .getDeclaredMethod("setLine", 3));
                code.emitThrow();
                if (isInTry)
                  endLabel.define(code);
                else
                  code.endFragment(fragment_cookie);
              }
            else
              code.emitInvokeVirtual(Compilation.getLocationMethod);

            rtype = Type.pointer_type;
          }
      }
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
            loadOwningObject(null, comp);
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
  public final Expression getValue()
  {
    if (value == QuoteExp.undefined_exp)
      {
        if (field != null
            && ((field.getModifiers() & Access.STATIC+Access.FINAL)
                == Access.STATIC+Access.FINAL)
            && ! isIndirectBinding())
          {
            try
              {
                value = new QuoteExp(field.getReflectField().get(null));
              }
            catch (Throwable ex)
              {
              }
          }
      }
    else if (value instanceof QuoteExp && getFlag(TYPE_SPECIFIED)
             && value.getType() != type)
      {
        try
          {
            Object val = ((QuoteExp) value).getValue();
            Type t = getType();
            value = new QuoteExp(t.coerceFromObject(val), t);
          }
        catch (Throwable ex)
          {
          }
      }
    return value;
  }

  /** Set the value associated with this Declaration.
   * Most code should use noteValue instead. */
  public final void setValue(Expression value) { this.value = value; }

  /** If getValue() is a constant, return the constant value, otherwise null. */
  public final Object getConstantValue()
  {
    Object v = getValue();
    if (! (v instanceof QuoteExp) || v == QuoteExp.undefined_exp)
      return null;
    return ((QuoteExp) v).getValue();
  }

  public final boolean hasConstantValue ()
  {
    Object v = getValue();
    return (v instanceof QuoteExp) && v != QuoteExp.undefined_exp;
  }

  boolean shouldEarlyInit ()
  {
    return getFlag(EARLY_INIT) || isCompiletimeConstant ();
  }

  public boolean isCompiletimeConstant ()
  {
    return getFlag(IS_CONSTANT) && hasConstantValue();
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
    return (flags & EXTERNAL_ACCESS+PRIVATE) == EXTERNAL_ACCESS+PRIVATE
      // Kludge - needed for macros - see Savannah bug #13601.
      || (flags & IS_NAMESPACE_PREFIX+PRIVATE) == IS_NAMESPACE_PREFIX+PRIVATE;
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

  /** Initialize in {@code <init>}/{@code <clinit>}
   * rather than in {@code run}/{@code $run$}. */
  public static final int EARLY_INIT = 0x20000000;
  /** A reference to a module instance. */
  public static final int MODULE_REFERENCE = 0x40000000;

  public static final long VOLATILE_ACCESS = 0x80000000l;
  public static final long TRANSIENT_ACCESS = 0x100000000l;
  public static final long ENUM_ACCESS = 0x200000000l;
  public static final long FINAL_ACCESS = 0x400000000l;
  public static final long CLASS_ACCESS_FLAGS =
    PRIVATE_ACCESS|PROTECTED_ACCESS|ENUM_ACCESS|FINAL_ACCESS;
  public static final long FIELD_ACCESS_FLAGS = PRIVATE_ACCESS|PROTECTED_ACCESS|
    PUBLIC_ACCESS|PACKAGE_ACCESS|VOLATILE_ACCESS|TRANSIENT_ACCESS|
    ENUM_ACCESS|FINAL_ACCESS;
  public static final long METHOD_ACCESS_FLAGS = PRIVATE_ACCESS
    |PROTECTED_ACCESS|PUBLIC_ACCESS|PACKAGE_ACCESS|FINAL_ACCESS;

  protected long flags = IS_SIMPLE;

  public final boolean getFlag (long flag)
  {
    return (flags & flag) != 0;
  }

  public final void setFlag (boolean setting, long flag)
  {
    if (setting) flags |= flag;
    else flags &= ~flag;
  }

  public final void setFlag (long flag)
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
    short flags;
    if (getFlag(PRIVATE_ACCESS|PROTECTED_ACCESS|PACKAGE_ACCESS|PUBLIC_ACCESS))
      {
        flags = 0;
        if (getFlag(PRIVATE_ACCESS))
          flags |= Access.PRIVATE;
        if (getFlag(PROTECTED_ACCESS))
          flags |= Access.PROTECTED;
        if (getFlag(PUBLIC_ACCESS))
          flags |= Access.PUBLIC;
      }
    else
      flags = defaultFlags;
    if (getFlag(VOLATILE_ACCESS))
      flags |= Access.VOLATILE;
    if (getFlag(TRANSIENT_ACCESS))
      flags |= Access.TRANSIENT;
    if (getFlag(ENUM_ACCESS))
      flags |= Access.ENUM;
    if (getFlag(FINAL_ACCESS))
      flags |= Access.FINAL;
    return flags;
  }

  public final boolean isAlias() { return (flags & IS_ALIAS) != 0; }
  public final void setAlias(boolean flag) { setFlag(flag, IS_ALIAS); }

  /** True if this is a fluid binding (in a FluidLetExp).
   * Also true if this binding is the one re-bound by a FluidLetExp. */
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

  public void maybeIndirectBinding (Compilation comp)
  {
    if (isLexical() 
        && ! (context instanceof ModuleExp) || context == comp.mainLambda)
      setIndirectBinding(true);
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

  /** Is this an implicit 'this' parameter? */
  public final boolean isThisParameter ()
  {
    return symbol == ThisExp.THIS_NAME;
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
    if (field != null)
      return field.getStaticFlag();
    if (getFlag(STATIC_SPECIFIED)
        || isCompiletimeConstant())
      return true;
    if (getFlag(NONSTATIC_SPECIFIED))
      return false;
    LambdaExp lambda = context.currentLambda();
    return lambda instanceof ModuleExp
      && ((ModuleExp) lambda).isStatic();
  }

  public final boolean isLexical()
  {
    return (flags & (IS_FLUID|IS_DYNAMIC|IS_UNKNOWN)) == 0;
  }

  public static final boolean isUnknown (Declaration decl)
  {
    return decl == null || decl.getFlag(IS_UNKNOWN);
  }

  /** List of ApplyExp where this declaration is the function called.
   * The applications are chained using their nextCall fields.
   * The chain is not built if STATIC_SPECIFIED. */
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
    setName(name);
  }

  public Declaration (Object name, Type type)
  {
    setName(name);
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
    if (! isSimple() || var == null)
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

  public final void setLocation (SourceLocator location)
  {
    this.filename = location.getFileName();
    setLine(location.getLineNumber(), location.getColumnNumber());
  }

  public final void setFile (String filename)
  {
    this.filename = filename;
  }

  public final void setLine (int lineno, int colno)
  {
    if (lineno < 0)
      lineno = 0;
    if (colno < 0)
      colno = 0;
    position = (lineno << 12) + colno;
  }

  public final void setLine (int lineno)
  {
    setLine (lineno, 0);
  }

  public final String getFileName ()
  {
    return filename;
  }

  public String getPublicId ()
  {
    return null;
  }

  public String getSystemId ()
  {
    return filename;
  }

  /** Get the line number of (the start of) this Expression.
    * The "first" line is line 1; unknown is -1. */
  public final int getLineNumber()
  {
    int line = position >> 12;
    return line == 0 ? -1 : line;
  }

  public final int getColumnNumber()
  {
    int column = position & ((1 << 12) - 1);
    return column == 0 ? -1 : column;
  }

  public boolean isStableSourceLocation() { return true; }

  public void printInfo(OutPort out)
  {
    StringBuffer sbuf = new StringBuffer();
    printInfo(sbuf);
    out.print(sbuf.toString());
  }

  public void printInfo(StringBuffer sbuf)
  {
    sbuf.append(symbol);
    if (true || // DEBUGGING
        symbol == null)
      ;
    else if (symbol instanceof SimpleSymbol)
      sbuf.append("[simple-symbol]");
    else if (symbol instanceof Symbol)
      sbuf.append("[symbol]");
    else if (symbol.toString().intern() == symbol)
      sbuf.append("[interned-string]");
    else if (symbol instanceof String)
      sbuf.append("[noninterned-string]");
    sbuf.append('/');
    sbuf.append(id);
    /*
    int line = getLineNumber();
    if (line != 0)
      {
	sbuf.append("/line:");
	sbuf.append(line);
	int column = getColumnNumber();
	if (column != 0)
	  {
	    sbuf.append(':');
	    sbuf.append(column);
	  }
      }
    */
    sbuf.append("/fl:");
    sbuf.append(Long.toHexString(flags));
    if (ignorable())
      sbuf.append("(ignorable)");
    Expression tx = typeExp;
    Type t = getType();
    if (tx != null && ! (tx instanceof QuoteExp))
      {
	sbuf.append("::");
        sbuf.append(tx);
      }
    else if (type != null && t != Type.pointer_type)
      {
	sbuf.append("::");
	sbuf.append(t.getName());
      }
    if (base != null)
      {
        sbuf.append("(base:#");
        sbuf.append(base.id);
        sbuf.append(')');
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
    makeField(comp.mainClass, comp, value);
  }

  public void makeField(ClassType frameType, Compilation comp, Expression value)
  {
    boolean external_access = needsExternalAccess();
    int fflags = 0;
    boolean isConstant = getFlag(IS_CONSTANT);
    boolean typeSpecified = getFlag(TYPE_SPECIFIED);
    if (comp.immediate && context instanceof ModuleExp
        && ! isConstant && ! typeSpecified)
      setIndirectBinding(true);
    // In immediate mode we may need to access the field from a future
    // command in a different "runtime package" (see JVM spec) because it
    // gets loaded by a different class loader.  So make the field public.
    if (isPublic() || external_access || comp.immediate)
      fflags |= Access.PUBLIC;
    if (isStatic()
        // "Dynamic" variables use ThreadLocation, based on the current
        // Environment, so we don't need more than one static field.
        || (getFlag(Declaration.IS_UNKNOWN
                    |Declaration.IS_DYNAMIC|Declaration.IS_FLUID)
            && isIndirectBinding() && ! isAlias())
	|| (value instanceof ClassExp
	    && ! ((LambdaExp) value).getNeedsClosureEnv()))
      fflags |= Access.STATIC;
    if ((isIndirectBinding()
         || (isConstant
             && (shouldEarlyInit()
                 || (context instanceof ModuleExp && ((ModuleExp) context).staticInitRun()))))
        && (context instanceof ClassExp || context instanceof ModuleExp))
      fflags |= Access.FINAL;
    Type ftype = getType().getImplementationType();
    if (isIndirectBinding() && ! ftype.isSubtype(Compilation.typeLocation))
      ftype = Compilation.typeLocation;
    if (! ignorable())
      {
        String fname = getName();
        int nlength;
        if (fname==null)
          {
            fname = "$unnamed$0";
            nlength = fname.length() - 2; // Without the "$0".
          }
        else
          {
            fname = Compilation.mangleNameIfNeeded(fname);
            if (getFlag(IS_UNKNOWN))
              fname = UNKNOWN_PREFIX + fname;
            if (external_access && ! getFlag(Declaration.MODULE_REFERENCE))
              fname = PRIVATE_PREFIX + fname;
            nlength = fname.length();
          }
        int counter = 0;
        while (frameType.getDeclaredField(fname) != null)
          fname = fname.substring(0, nlength) + '$' + (++ counter);
        field = frameType.addField (fname, ftype, fflags);
        if (value instanceof QuoteExp)
          {
            Object val = ((QuoteExp) value).getValue();
            if (field.getStaticFlag()
                  && val.getClass().getName().equals(ftype.getName()))
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
                field.setConstantValue(val, frameType);
                return;
              }
          }
      }
    // The EARLY_INIT case is handled in SetExp.compile.
    if (! shouldEarlyInit()
	&& (isIndirectBinding()
	    || (value != null && ! (value instanceof ClassExp))))
      {
	BindingInitializer.create(this, value, comp);
      }
  }

  /* Used when evaluating for an indirect binding. */
  gnu.mapping.Location makeIndirectLocationFor ()
  {
    Symbol sym = symbol instanceof Symbol ? (Symbol) symbol
      : Namespace.EmptyNamespace.getSymbol(symbol.toString().intern());
    return gnu.mapping.Location.make(sym);
  }

  /** Create a declaration corresponding to a static field.
   * @param cname name of class containing field
   * @param fname name of static field
   */
  public static Declaration
  getDeclarationFromStatic (String cname, String fname)
  {
    ClassType clas = ClassType.make(cname);
    Field fld = clas.getDeclaredField(fname);
    Declaration decl = new Declaration(fname, fld);
    decl.setFlag(Declaration.IS_CONSTANT|Declaration.STATIC_SPECIFIED);
    return decl;
  }

  /** Similar to {@code getDeclarationFromStatic},
   * but also do {@code noteValue} with the field's value.
   */
  public static Declaration
  getDeclarationValueFromStatic (String className,
                                 String fieldName, String name)
  {
    try
      {
	Class cls = Class.forName(className);
	java.lang.reflect.Field fld = cls.getDeclaredField(fieldName);
	Object value = fld.get(null);

	Declaration decl
          = new Declaration(name,
                            ClassType.make(className)
                            .getDeclaredField(fieldName));
	decl.noteValue(new QuoteExp(value));
	decl.setFlag(Declaration.IS_CONSTANT|Declaration.STATIC_SPECIFIED);
        return decl;
      }
    catch (Exception ex)
      {
	throw new WrappedException(ex);
      }
  }

  public static Declaration getDeclaration(Named proc)
  {
    return getDeclaration(proc, proc.getName());
  }

  public static Declaration getDeclaration(Object proc, String name)
  {
    gnu.bytecode.Field procField = null;
    if (name != null)
      {
        /*
        // This is a way to map from the Procedure's name to a Field,
        // by assuming the name as the form "classname:fieldname".
        // It may be better to use names of the form "{classname}fieldname".
        // For now we don't need this feature.
        int colon = name.indexOf(':');
        if (colon > 0)
          {
            try
              {
                ClassType procType
                  = (ClassType) ClassType.make(name.substring(0, colon));
                name = name.substring(colon+1);
                String fname = Compilation.mangleNameIfNeeded(name);
                procField = procType.getDeclaredField(fname);
              }
            catch (Throwable ex)
              {
                System.err.println("CAUGHT "+ex+" in getDeclaration for "+proc);
                return null;
              }
          }
        else
        */
          {
            Class procClass = PrimProcedure.getProcedureClass(proc);
            if (procClass != null)
              {
                ClassType procType = (ClassType) Type.make(procClass);
                String fname = Compilation.mangleNameIfNeeded(name);
                procField = procType.getDeclaredField(fname);
              }
          }
      }
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
    return null;
  }
}
