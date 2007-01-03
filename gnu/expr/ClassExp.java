package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;
import java.util.Vector;

public class ClassExp extends LambdaExp
{
  boolean simple;
  public boolean isSimple() { return simple; }
  public void setSimple(boolean value) { simple = value; }

  /** True if there is at least one explicit "<init>" ("*init*"} method. */
  boolean explicitInit;

  /** The class of instances of this class.
   * Same as super.type unless isMakingClassPair(), in which case super.type
   * is an interface, and instanceType is a class implementing the interface.
   * Using an interface plus a class gives us true multiple inheritance. */
  ClassType instanceType;

  /** True if we should make a pair of an interface and a class. */
  public boolean isMakingClassPair()
  {
    return ! simple;
  }

  /** List of base classes and implemented interfaces. */
  public Expression[] supers;

  /** An artificial method named {@code "$finit$"} for evaluating
   * non-static initializations.
   * All constructors need to call this. */
  public LambdaExp initMethod;

  /** An artificial method named {@code "$clinit$"} for evaluating 
   * static initializations. */
  public LambdaExp clinitMethod;

  public ClassExp ()
  {
    type = null;
    // Make sure we actually generate a class.
    setCanRead(true);
  }

  public ClassExp (boolean simple)
  {
    this.simple = simple;
    if (simple)
      instanceType = type = new ClassType();
    else
      {
        PairClassType ptype = new PairClassType();
        type = ptype;
        instanceType = new ClassType();
        ptype.setInterface(true);
        ptype.instanceType = instanceType;
      }
    setCanRead(true);
  }

  protected boolean mustCompile () { return true; }

  public void compile (Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    compile (comp);
    compilePushClass(comp, target);
  }

  public void compilePushClass (Compilation comp, Target target)
  {
    ClassType new_class = type;
    // Type.make(Class.forname)

    gnu.bytecode.CodeAttr code = comp.getCode();
    comp.loadClassRef(new_class);
    ClassType typeType;
    int nargs;
    boolean needsLink = getNeedsClosureEnv();
    if (isMakingClassPair() || needsLink)
      {
        if (new_class == instanceType)
          code.emitDup(instanceType);
        else
          comp.loadClassRef(instanceType);
	typeType = ClassType.make("gnu.expr.PairClassType");
	nargs = needsLink ? 3 : 2;
      }
    else
      {
	typeType = ClassType.make("gnu.bytecode.Type");
	nargs = 1;
      }
    Type[] argsClass = new Type[nargs];
    if (needsLink)
      {
	getOwningLambda().loadHeapFrame(comp);
	argsClass[--nargs] = Type.pointer_type;
      }
    ClassType typeClass = ClassType.make("java.lang.Class");
    while (--nargs >= 0) argsClass[nargs] = typeClass;
    Method makeMethod
      = typeType.addMethod("make", argsClass,
			   typeType, Access.STATIC|Access.PUBLIC);
    code.emitInvokeStatic(makeMethod);

    target.compileFromStack(comp, typeType);
  }

  public String getJavaName ()
  {
    String name = getName();
    return name == null ? "object" : Compilation.mangleNameIfNeeded (name);
  }

  protected ClassType getCompiledClassType(Compilation comp)
  {
    return type;
  }

  public void setClassName (Compilation comp)
  {
    if (type.getName() == null)
      {
	String name = getName();
	if (name != null)
	  {
	    int nlen = name.length();
	    if (nlen > 2
		&& name.charAt(0) == '<' && name.charAt(nlen-1) == '>')
	      name = name.substring(1, nlen-1);
	  }
        if (name == null)
          {
	    StringBuffer nbuf = new StringBuffer(100);
            comp.getModule().classFor(comp);
            nbuf.append(comp.mainClass.getName());
            nbuf.append('$');
            int len = nbuf.length();
            for (int i = 0;  ; i++)
              {
                nbuf.append(i);
                name = nbuf.toString();
                if (comp.findNamedClass(name) == null)
                  break;
                nbuf.setLength(len);
              }
          }
	else if (! isSimple() || this instanceof ObjectExp)
	  name = comp.generateClassName(name);
	else
	  {
	    int start = 0;
	    StringBuffer nbuf = new StringBuffer(100);
	    for (;;)
	      {
		int dot = name.indexOf('.', start);
		if (dot < 0)
		  break;
		nbuf.append(Compilation
			    .mangleNameIfNeeded(name.substring(start, dot)));
		nbuf.append('.');
		start = dot + 1;
	      }
	    if (start == 0)
	      {
		String mainName = comp.mainClass == null ? null
		  : comp.mainClass.getName();
		int dot = mainName == null ? -1 : mainName.lastIndexOf('.');
		if (dot > 0)
		  nbuf.append(mainName.substring(0, dot + 1));
		else if (comp.classPrefix != null)
		  nbuf.append(comp.classPrefix);
	      }
	    if (start < name.length())
	      nbuf.append(Compilation
			  .mangleNameIfNeeded(name.substring(start)));
	    name = nbuf.toString();
	  }
	type.setName(name);
        comp.addClass(type);
        if (isMakingClassPair())
          {
            instanceType.setName(type.getName()+"$class");
            comp.addClass(instanceType);
          }
      }
  }

  public void setTypes(Compilation comp)
  {
    int len = supers == null ? 0 : supers.length;
    ClassType[] superTypes = new ClassType[len];
    ClassType superType = null;
    int j = 0;
    for (int i = 0;  i < len;  i++)
      {
        // setTypes may be called at name-resolution time (so we can can
        // resolve against inherited field and method names).  Therefore do
        // inlining now.  Needed (for example) for deprecated PREXIX:<> syntax.
        supers[i] = new InlineCalls(comp).walk(supers[i]);

	Type st = Language.getDefaultLanguage().getTypeFor(supers[i]);
	if (! (st instanceof ClassType))
          {
            comp.setLine(supers[i]);
            comp.error('e', "invalid super type");
            continue;
          }
	ClassType t = (ClassType) st;
	int modifiers;
	try
	  {
	    modifiers = t.getModifiers();
	  }
	catch (RuntimeException ex)
	  {
	    modifiers = 0;
	    if (comp != null)
	      comp.error('e', "unknown super-type "+t.getName());
	  }
	if ((modifiers & Access.INTERFACE) == 0)
	  {
	    if (j < i)
              comp.error('e', "duplicate superclass for "+this);
	    superType = t;
	  }
	else
	  superTypes[j++] = t;
      }
    if (! isSimple())
      {
        if (superType != null)
          comp.error('e', "non-simple class inherts from non-interface "+superType.getName());
          
        ClassType[] interfaces = { type };
        // Can we better.  FIXME.
        instanceType.setSuper(Type.pointer_type);
        instanceType.setInterfaces(interfaces);
      }
    type.setSuper(superType == null ? Type.pointer_type : superType);

    ClassType[] interfaces;
    if (j == len)
      interfaces = superTypes;
    else
      {
	interfaces = new ClassType[j];
	System.arraycopy(superTypes, 0, interfaces, 0, j);
      }
    type.setInterfaces(interfaces);
  }

  boolean partsDeclared;

  public void declareParts(Compilation comp)
  {
    if (partsDeclared)
      return;
    partsDeclared = true;
    for (Declaration decl = firstDecl();
	 decl != null;  decl = decl.nextDecl())
      {
	// If the declaration derives from a method, don't create field.
	if (decl.getCanRead())
	  {
	    int flags = decl.getAccessFlags(Access.PUBLIC);
	    if (decl.getFlag(Declaration.STATIC_SPECIFIED))
	      flags |= Access.STATIC;
	    if (isMakingClassPair())
	      {
		flags |= Access.ABSTRACT;
		Type ftype = decl.getType().getImplementationType();
		type.addMethod(slotToMethodName("get", decl.getName()),
			       flags, Type.typeArray0, ftype);
		Type[] stypes = { ftype };
		type.addMethod(slotToMethodName("set",decl.getName()),
			       flags, stypes, Type.void_type);
	      }
	    else
	      {
		String fname
		  = Compilation.mangleNameIfNeeded(decl.getName());
		decl.field
		  = instanceType.addField(fname, decl.getType(), flags);
		decl.setSimple(false);
	      }
	  }
      }

    for (LambdaExp child = firstChild;  child != null;
	 child = child.nextSibling)
      {
        if ("*init*".equals(child.getName()))
          explicitInit = true;
	if ((child != initMethod && child != clinitMethod)
	    || ! isMakingClassPair())
	  child.addMethodFor(type, comp, null);
	if (isMakingClassPair())
          // FIXME this is wrong if the method is static
	  child.addMethodFor(instanceType, comp, type);
      }
    if (! explicitInit)
      Compilation.getConstructor(instanceType, this);
  }

  /** Return implementation method matching name and param types.
   * Used when compiling a pair class and generating a concrete method
   * implementing an interface method, to find static implementation method
   * in this or super implementation class we need to call.
   * @param interfaceType search the implementation classes corresponding
   *   to this interface type and its super-interfaces.
   * @param mname method name to look for.
   * @param paramTypes method types to look for.
   * @param vec where to place found methods
   * If a method is found, don't search super-interfaces, as the found method
   * is more specific and overrides any that might in super-interfaces.
   */
  static void getImplMethods(ClassType interfaceType,
			     String mname, Type[] paramTypes, Vector vec)
  {
    ClassType implType;
    if (interfaceType instanceof PairClassType)
      implType = ((PairClassType) interfaceType).instanceType;
    else if (! interfaceType.isInterface())
      return;
    else
      {
	String implTypeName = interfaceType.getName() + "$class";
	implType = ClassType.make(implTypeName);
      }
    Type[] itypes = new Type[paramTypes.length + 1];
    itypes[0] = interfaceType;
    System.arraycopy (paramTypes, 0, itypes, 1, paramTypes.length);
    Method implMethod = implType.getDeclaredMethod(mname, itypes);
    if (implMethod != null)
      {
	int count = vec.size();
	if (count == 0 || ! vec.elementAt(count-1).equals(implMethod))
	  vec.addElement(implMethod);
      }
    else
      {
	ClassType[] superInterfaces = interfaceType.getInterfaces();
	for (int i = 0;  i < superInterfaces.length;  i++)
	  getImplMethods(superInterfaces[i], mname, paramTypes, vec);
      }
  }

  /** Call comp.usedClass on the first arguments's supertypes. */
  private static void usedSuperClasses(ClassType clas, Compilation comp)
  {
    comp.usedClass(clas.getSuperclass());
    ClassType[] interfaces = clas.getInterfaces();
    if (interfaces != null)
      {
	for (int i = interfaces.length;  --i >= 0; )
	  comp.usedClass(interfaces[i]);
      }
  }

  public ClassType compile (Compilation comp)
  {
    ClassType saveClass = comp.curClass;
    Method saveMethod = comp.method;
    try
      {
	ClassType new_class = getCompiledClassType(comp);
	comp.curClass = new_class;

	usedSuperClasses(type, comp);
	if (type != instanceType)
	  usedSuperClasses(instanceType, comp);

	String filename = getFileName();
	if (filename != null)
	  new_class.setSourceFile (filename);

	LambdaExp saveLambda = comp.curLambda;
	comp.curLambda = this;

	allocFrame(comp);
	CodeAttr code;

	for (LambdaExp child = firstChild;  child != null; )
	  {
	    Method save_method = comp.method;
	    LambdaExp save_lambda = comp.curLambda;
            String saveFilename = comp.getFileName();
            int saveLine = comp.getLineNumber();
            int saveColumn = comp.getColumnNumber();
            comp.setLine(child);
	    comp.method = child.getMainMethod();
	    //comp.curClass = comp.method.getDeclaringClass();
            Declaration childDecl = child.nameDecl;
            if (childDecl == null
                || ! childDecl.getFlag(Declaration.STATIC_SPECIFIED))
              child.declareThis(comp.curClass);
	    comp.curClass = instanceType;
	    comp.curLambda = child;
	    comp.method.initCode();
            child.allocChildClasses(comp);
	    child.allocParameters(comp);
	    child.enterFunction(comp);
            if ("*init*".equals(child.getName()))
              {
                code = comp.getCode();

                // Extract "first" expression to see if it is special.
                Expression bodyFirst = child.body;
                while (bodyFirst instanceof BeginExp)
                  {
                    BeginExp bbody = (BeginExp) bodyFirst;
                    if (bbody.length == 0)
                      bodyFirst = null;
                    else
                      bodyFirst = bbody.exps[0];
                  }

                // See if bodyFirst is a this(...) or super(...) call.
                ClassType calledInit = null;
                Object value;  Expression exp;
                if (bodyFirst instanceof ApplyExp
                    && (exp = ((ApplyExp) bodyFirst).func) instanceof QuoteExp
                    && (value = ((QuoteExp) exp).getValue()) instanceof PrimProcedure)
                  {
                    PrimProcedure pproc = (PrimProcedure) value;
                    if (pproc.isSpecial()
                        && ("<init>".equals(pproc.method.getName())))
                      calledInit = pproc.method.getDeclaringClass();
                  }
                ClassType superClass = instanceType.getSuperclass();
                if (calledInit != null)
                  {
                    bodyFirst.compileWithPosition(comp, Target.Ignore);
                    if (calledInit != instanceType && calledInit != superClass)
                      comp.error('e', "call to <init> for not this or super class");
                  }
                else if (calledInit != superClass)
                  {
                    // Call default super constructor if there isn't an explicit
                    // call to a super constructor.
                    Method superConstructor
                      = superClass.getDeclaredMethod("<init>", 0);
                    if (superConstructor == null)
                      comp.error('e', "super class does not have a default constructor");
                    else
                      {
                        code.emitPushThis();
                        code.emitInvokeSpecial(superConstructor);
                      }
                  }
                if (calledInit != instanceType)
                  comp.callInitMethods(getCompiledClassType(comp),
                                       new Vector(10));
                if (calledInit != null)
                  // Skip bodyFirst since we already compiled it.
                  Expression.compileButFirst(child.body, comp);
                else
                  child.compileBody(comp);
              }
            else
              child.compileBody(comp);
	    child.compileEnd(comp);
	    child.compileChildMethods(comp);
	    comp.method = save_method;
	    comp.curClass = new_class;
	    comp.curLambda = save_lambda;
            comp.setLine(saveFilename, saveLine, saveColumn);
	    child = child.nextSibling;
	  }
        if (! explicitInit)
          comp.generateConstructor(instanceType, this);
        else if (initChain != null)
          initChain.reportError("unimplemented: explicit constructor cannot initialize ", comp);

	Method[] methods = type.getMethods(AbstractMethodFilter.instance, 2);
	for (int i = 0;  i < methods.length;  i++)
	  {
	    Method meth = methods[i];
	    String mname = meth.getName();
	    Type[] ptypes = meth.getParameterTypes();
	    Type rtype = meth.getReturnType();

	    Method mimpl = instanceType.getMethod(mname, ptypes);
	    if (mimpl != null && ! mimpl.isAbstract())
	      continue;

	    char ch;
	    if (mname.length() > 3
		&& mname.charAt(2) == 't'
		&& mname.charAt(1) == 'e'
		&& ((ch = mname.charAt(0)) == 'g' || ch == 's'))
	      { // a "set" or "get" method is treated as a slot accessor.
		Type ftype;
		if (ch == 's' && rtype.isVoid() && ptypes.length == 1)
		  ftype = ptypes[0];
		else if (ch == 'g' && ptypes.length == 0)
		  ftype = rtype;
		else
		  continue;
		String fname = Character.toLowerCase(mname.charAt(3))
		  + mname.substring(4);
		Field fld = instanceType.getField(fname);
		if (fld == null)
		  fld = instanceType.addField(fname, ftype, Access.PUBLIC);
		Method impl = instanceType.addMethod(mname, Access.PUBLIC,
						     ptypes, rtype);
		code = impl.startCode();
		code.emitPushThis();
		if (ch == 'g')
		  {
		    code.emitGetField(fld);
		  }
		else
		  {
		    code.emitLoad(code.getArg(1));
		    code.emitPutField(fld);
		  }
		code.emitReturn();
	      }
	    else
	      {
		Vector vec = new Vector();
		getImplMethods(type, mname, ptypes, vec);
		if (vec.size() != 1)
		  {
		    // FIXME - need better error message!
		    String msg = vec.size() == 0
		      ? "missing implementation for "
		      : "ambiguous implementation for ";
		    comp.error('e', msg+meth+" mname:"+mname);
		  }
		else
		  {
		    Method impl = instanceType.addMethod(mname, Access.PUBLIC,
							 ptypes, rtype);
		    code = impl.startCode();
		    for (Variable var = code.getCurrentScope().firstVar();
			 var != null;  var = var.nextVar())
		      code.emitLoad(var);
		    Method imethod = (Method) vec.elementAt(0);
		    code.emitInvokeStatic(imethod);
		    code.emitReturn();
		  }
	      }
	  }

        generateApplyMethods(comp);
	comp.curLambda = saveLambda;

	return new_class;
      }
    finally
      {
	comp.curClass = saveClass;
	comp.method = saveMethod;
      }
  }

  protected Expression walk (ExpWalker walker)
  {
    Compilation comp = walker.getCompilation();
    if (comp == null)
      return walker.walkClassExp(this);
    ClassType saveClass = comp.curClass;
    try
      {
	comp.curClass = type;
	return walker.walkClassExp(this);
      }
    finally
      {
	comp.curClass = saveClass;
      }
  }

  protected void walkChildren(ExpWalker walker)
  {
    LambdaExp save = walker.currentLambda;
    walker.currentLambda = this;
    try
      {
	for (LambdaExp child = firstChild;
	     child != null && walker.exitValue == null;
	     child = child.nextSibling)
          {
            if (instanceType != null)
              {
                Declaration firstParam = child.firstDecl();
                if (firstParam != null && firstParam.isThisParameter())
                  firstParam.setType(type);
              }
            walker.walkLambdaExp(child);
          }
      }
    finally
      {
	walker.currentLambda = save;
      }
  }

  public void print (OutPort out)
  {
    out.startLogicalBlock("("+getExpClassName()+"/", ")", 2);
    Object name = getSymbol();
    if (name != null)
      {
	out.print(name);
	out.print('/');
      }
    out.print(id);
    out.print("/fl:");  out.print(Integer.toHexString(flags));
    out.print(" (");
    Special prevMode = null;
    int i = 0;
    int key_args = keywords == null ? 0 : keywords.length;
    //int opt_args = defaultArgs == null ? 0 : defaultArgs.length - key_args;
    for (Declaration decl = firstDecl();  decl != null; decl = decl.nextDecl())
      {
	if (i > 0)
	  out.print(' ');
        decl.printInfo(out);
	i++;
      }
    out.print(") ");
    for (LambdaExp child = firstChild;  child != null;
	 child = child.nextSibling)
      {
	out.writeBreakLinear();
        child.print(out);
      }
    if (body != null)
      {
        out.writeBreakLinear();
        body.print (out);
      }
    out.endLogicalBlock(")");
  }

  public Field compileSetField (Compilation comp)
  {
    return (new ClassInitializer(this, comp)).field;
  }

  /** Mangle a "slot" name to a get- or set- method name.
   * @param prefix either "get" or "set"
   * @param sname a "slot" (property) name.  This is mangled if needed.
   */
  public static String slotToMethodName(String prefix, String sname)
  {
    if (! Compilation.isValidJavaName(sname))
      sname = Compilation.mangleName(sname, false);
    StringBuffer sbuf = new StringBuffer(sname.length()+3);
    sbuf.append(prefix);
    sbuf.append(Character.toTitleCase(sname.charAt(0)));
    sbuf.append(sname.substring(1));
    return sbuf.toString();
  }

  /** Helper class uses by ClassExp.compile.
   */
  private static class AbstractMethodFilter implements gnu.bytecode.Filter
  {
    public static final AbstractMethodFilter instance
      = new AbstractMethodFilter();

    public boolean select(Object value)
    {
      gnu.bytecode.Method method = (gnu.bytecode.Method) value;
      return method.isAbstract();
    }
  }
}
