// Copyright (c) 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;
import java.util.*;

public class ClassExp extends LambdaExp
{
  boolean simple;
  public boolean isSimple() { return simple; }
  public void setSimple(boolean value) { simple = value; }

  public final boolean isAbstract () { return getFlag(IS_ABSTRACT); }
  public static final int IS_ABSTRACT = LambdaExp.NEXT_AVAIL_FLAG;
  public static final int INTERFACE_SPECIFIED = 2 * LambdaExp.NEXT_AVAIL_FLAG;
  public static final int CLASS_SPECIFIED = 4 * LambdaExp.NEXT_AVAIL_FLAG;
  public static final int HAS_SUBCLASS = 8 * LambdaExp.NEXT_AVAIL_FLAG;

  /** True if there is at least one explicit "<init>" ("*init*"} method. */
  boolean explicitInit;

  /** The class of instances of this class.
   * Same as super.type unless isMakingClassPair(), in which case super.type
   * is an interface, and instanceType is a class implementing the interface.
   * Using an interface plus a class gives us true multiple inheritance. */
  ClassType instanceType;

  public String classNameSpecifier;

  /** True if we should make a pair of an interface and a class. */
  public boolean isMakingClassPair()
  {
    return type != instanceType;
  }

  /** The ClassType generated for this class.
   * Note difference from {@code getClassType}: The value of a {@code ClassExp}
   * (viewed as an expression) is a class/type object, so getType returns
   * the type of a type.
   */
  public Type getType()
  { return simple ? Compilation.typeClass : Compilation.typeClassType; }

  /** The ClassType generated for this class.
   */
  public ClassType getClassType() { return type; }

  /** List of base classes and implemented interfaces. */
  public Expression[] supers;
  /** Index in supers array of class we extend, or -1. */
  public int superClassIndex = -1;

  /** An artificial method named {@code "$finit$"} for evaluating
   * non-static initializations.
   * All constructors need to call this. */
  public LambdaExp initMethod;

  /** An artificial method named {@code "$clinit$"} for evaluating 
   * static initializations. */
  public LambdaExp clinitMethod;

  public ClassExp ()
  {
  }

  public ClassExp (boolean simple)
  {
    this.simple = simple;
    instanceType = type = new ClassType();
  }

  protected boolean mustCompile () { return true; }

  public void compile (Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    compileMembers(comp);
    compilePushClass(comp, target);
  }

  public void compilePushClass (Compilation comp, Target target)
  {
    ClassType new_class = type;

    gnu.bytecode.CodeAttr code = comp.getCode();
    comp.loadClassRef(new_class);
    ClassType typeType;
    int nargs;
    boolean needsLink = getNeedsClosureEnv();
    if (isSimple() && ! needsLink)
      return;
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

  protected ClassType getCompiledClassType(Compilation comp)
  {
    return type;
  }

  public void setTypes(Compilation comp)
  {
    int nsupers = supers == null ? 0 : supers.length;
    ClassType[] superTypes = new ClassType[nsupers];
    ClassType superType = null;
    int j = 0;
    for (int i = 0;  i < nsupers;  i++)
      {
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
            superClassIndex = i;
	  }
	else
	  superTypes[j++] = t;
      }
    if (superType != null && (flags & INTERFACE_SPECIFIED) != 0)
      comp.error('e', "cannot be interface since has superclass");
    if (! simple && superType == null && (flags & CLASS_SPECIFIED) == 0
        && (getFlag(HAS_SUBCLASS)
            || (nameDecl != null && nameDecl.isPublic())))
      {
        PairClassType ptype = new PairClassType();//(PairClassType) type;
        type = ptype;
        ptype.setInterface(true);
        ptype.instanceType = instanceType;
        ClassType[] interfaces = { type };
        // Can do better.  FIXME.
        instanceType.setSuper(Type.pointer_type);
        instanceType.setInterfaces(interfaces);
      }
    else if (getFlag(INTERFACE_SPECIFIED))
      instanceType.setInterface(true);
    type.setSuper(superType == null ? Type.pointer_type : superType);

    ClassType[] interfaces;
    if (j == nsupers)
      interfaces = superTypes;
    else
      {
	interfaces = new ClassType[j];
	System.arraycopy(superTypes, 0, interfaces, 0, j);
      }
    type.setInterfaces(interfaces);

    if (type.getName() == null)
      {
	String name;
        if (classNameSpecifier != null)
          name = classNameSpecifier;
        else
          {
            name = getName();
            if (name != null)
              {
                int nlen = name.length();
                if (nlen > 2
                    && name.charAt(0) == '<' && name.charAt(nlen-1) == '>')
                  name = name.substring(1, nlen-1);
              }
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
		start = dot + 1;
                if (start < name.length())
                  nbuf.append('.');
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
            else if (start == 1 && start < name.length())
              {
                nbuf.setLength(0);
                nbuf.append(comp.mainClass.getName());
                nbuf.append('$');
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

  boolean partsDeclared;

  public void declareParts(Compilation comp)
  {
    if (partsDeclared)
      return;
    partsDeclared = true;
    Hashtable<String,Declaration> seenFields
      = new Hashtable<String,Declaration>();
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
			       flags, stypes, Type.voidType);
	      }
	    else
	      {
		String fname
		  = Compilation.mangleNameIfNeeded(decl.getName());
		decl.field
		  = instanceType.addField(fname, decl.getType(), flags);
		decl.setSimple(false);
                Declaration old = seenFields.get(fname);
                if (old != null)
                  duplicateDeclarationError(old, decl, comp);
                seenFields.put(fname, decl);
	      }
	  }
      }

    for (LambdaExp child = firstChild;  child != null;
	 child = child.nextSibling)
      {
        if (child.isAbstract())
          setFlag(IS_ABSTRACT);
        if ("*init*".equals(child.getName()))
          {
            explicitInit = true;
            if (child.isAbstract())
              comp.error('e', "*init* method cannot be abstract", child);
            if (type instanceof PairClassType)
              comp.error('e', "'*init*' methods only supported for simple classes");
          }
        // Setting child.outer isn't normally needed.  The exception is
        // if we're called from object.rewriteClassDef and there is some
        // funny macro expansion going on, in which case outer
        // might be a TemplateScope.
        child.outer = this;
	if ((child != initMethod && child != clinitMethod
             && child.nameDecl != null // only if error
             && ! child.nameDecl.getFlag(Declaration.STATIC_SPECIFIED))
	    || ! isMakingClassPair())
	  child.addMethodFor(type, comp, null);
	if (isMakingClassPair())
	  child.addMethodFor(instanceType, comp, type);
      }
    if (! explicitInit && ! instanceType.isInterface())
      Compilation.getConstructor(instanceType, this);
    if (isAbstract())
      instanceType.setModifiers(instanceType.getModifiers() | Access.ABSTRACT);
    if (nameDecl != null)
      instanceType.setModifiers((instanceType.getModifiers()
                                 & ~Access.PUBLIC)
                                | nameDecl.getAccessFlags(Access.PUBLIC));
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
        try
          {
            Class reflectClass = interfaceType.getReflectClass();
            if (reflectClass == null)
              return;
            String implTypeName = interfaceType.getName() + "$class";
            ClassLoader loader = reflectClass.getClassLoader();
            /* #ifdef JAVA2 */
            Class implClass = Class.forName(implTypeName, false, loader);
            /* #else */
            // Class implClass = Class.forName(implTypeName);
            /* #endif */
            implType = (ClassType) Type.make(implClass);
          }
        catch (Throwable ex)
          {
            return;
          }
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

  public ClassType compileMembers (Compilation comp)
  {
    ClassType saveClass = comp.curClass;
    Method saveMethod = comp.method;
    try
      {
	ClassType new_class = getCompiledClassType(comp);
	comp.curClass = new_class;

        LambdaExp outer = outerLambda();
        Member enclosing = null;
        if (outer instanceof ClassExp)
          enclosing = outer.type;
        else if (outer != null && ! (outer instanceof ModuleExp))
          enclosing = saveMethod;
        else if (outer instanceof ModuleExp && type.getName().indexOf('$') > 0)
         enclosing = outer.type;
        if (enclosing != null)
          {
            new_class.setEnclosingMember(enclosing);
            if (enclosing instanceof ClassType)
              ((ClassType) enclosing).addMemberClass(new_class);
          }
        if (instanceType != new_class)
          {
            instanceType.setEnclosingMember(type);
            type.addMemberClass(instanceType);
          }
              
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

	for (LambdaExp child = firstChild;  child != null;
             child = child.nextSibling)
	  {
            if (child.isAbstract())
              continue;
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
            if ("*init*".equals(child.getName()))
              {
                code = comp.getCode();

                if (staticLinkField != null)
                  {
                    code.emitPushThis();
                    code.emitLoad(code.getCurrentScope().getVariable(1));
                    code.emitPutField(staticLinkField);
                  }

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
                else if (superClass != null)
                  {
                    // Call default super constructor if there isn't an explicit
                    // call to a super constructor.
                    invokeDefaultSuperConstructor(superClass, comp, this);
                  }
                child.enterFunction(comp);
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
              {
                child.enterFunction(comp);
                child.compileBody(comp);
              }
	    child.compileEnd(comp);
	    child.generateApplyMethods(comp);
	    comp.method = save_method;
	    comp.curClass = new_class;
	    comp.curLambda = save_lambda;
            comp.setLine(saveFilename, saveLine, saveColumn);
	  }
        if (! explicitInit && ! instanceType.isInterface())
          comp.generateConstructor(instanceType, this);
        else if (initChain != null)
          initChain.reportError("unimplemented: explicit constructor cannot initialize ", comp);

	Method[] methods;
        int nmethods;
        if (isAbstract())
          {
            methods = null;
            nmethods = 0;
          }
        else
          {
            methods = type.getAbstractMethods();
            nmethods = methods.length;
          }
	for (int i = 0;  i < nmethods;  i++)
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
		    comp.error('e', msg+meth);
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

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    Compilation comp = visitor.getCompilation();
    if (comp == null)
      return visitor.visitClassExp(this, d);
    ClassType saveClass = comp.curClass;
    try

      {
	comp.curClass = type;
	return visitor.visitClassExp(this, d);
      }
    finally
      {
	comp.curClass = saveClass;
      }
  }

  protected <R,D> void visitChildren (ExpVisitor<R,D> visitor, D d)
  {
    LambdaExp save = visitor.currentLambda;
    visitor.currentLambda = this;
    supers = visitor.visitExps(supers, supers.length, d);
    try
      {
	for (LambdaExp child = firstChild;
	     child != null && visitor.exitValue == null;
	     child = child.nextSibling)
          {
            if (instanceType != null)
              {
                Declaration firstParam = child.firstDecl();
                if (firstParam != null && firstParam.isThisParameter())
                  firstParam.setType(type);
              }
            visitor.visitLambdaExp(child, d);
          }
      }
    finally
      {
	visitor.currentLambda = save;
      }
  }

  static void loadSuperStaticLink (Expression superExp, ClassType superClass,
                                   Compilation comp)
  {
    CodeAttr code = comp.getCode();
    // This can be optimized in most cases. FIXME.
    superExp.compile(comp, Target.pushValue(Compilation.typeClassType));
    code.emitInvokeStatic(ClassType.make("gnu.expr.PairClassType").getDeclaredMethod("extractStaticLink", 1));
    code.emitCheckcast(superClass.getOuterLinkType());
  }

  static void invokeDefaultSuperConstructor (ClassType superClass,
                                             Compilation comp,
                                             LambdaExp lexp)
  {
    CodeAttr code = comp.getCode();
    Method superConstructor
      = superClass.getDeclaredMethod("<init>", 0);
    if (superConstructor == null)
      comp.error('e', "super class does not have a default constructor");
    else
      {
        code.emitPushThis();
        if (superClass.hasOuterLink() && lexp instanceof ClassExp)
          {
            ClassExp clExp = (ClassExp) lexp;
            Expression superExp = clExp.supers[clExp.superClassIndex];
            loadSuperStaticLink(superExp, superClass, comp);
          }
        code.emitInvokeSpecial(superConstructor);
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
    if (supers.length > 0)
      {

        out.writeSpaceFill();
        out.startLogicalBlock("supers:", "", 2);
        for (int i = 0;  i < supers.length;  i++)
          {
            supers[i].print(out);
            out.writeSpaceFill();
          }
        out.endLogicalBlock("");
      }
    out.print('(');
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
   * @param prefix either "get" or "set" or "add"
   * @param sname a "slot" (property) name.  This is mangled if needed.
   */
  public static String slotToMethodName(String prefix, String sname)
  {
    if (! Compilation.isValidJavaName(sname))
      sname = Compilation.mangleName(sname, false);
    int slen = sname.length();
    StringBuffer sbuf = new StringBuffer(slen+3);
    sbuf.append(prefix);
    if (slen > 0)
      {
        sbuf.append(Character.toTitleCase(sname.charAt(0)));
        sbuf.append(sname.substring(1));
      }
    return sbuf.toString();
  }

  public Declaration addMethod (LambdaExp lexp, Object mname)
  {
    Declaration mdecl = addDeclaration(mname, Compilation.typeProcedure);
    lexp.outer = this;
    lexp.setClassMethod(true);
    mdecl.noteValue(lexp);
    mdecl.setFlag(Declaration.FIELD_OR_METHOD);
    mdecl.setProcedureDecl(true);
    lexp.setSymbol(mname);
    return mdecl;
  }
}
