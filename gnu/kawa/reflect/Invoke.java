package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;
import gnu.lists.FString;

public class Invoke extends ProcedureN implements CanInline
{
  /** 'N' - make (new);  'S' - invoke-static (static or non-static);
      's' - like 'S' but only allow static method; 'V'  - non-static invoke. */
  char kind;

  Language language;

  public static final Invoke invoke = new Invoke("invoke", 'V');
  public static final Invoke invokeStatic = new Invoke("invoke-static", 'S');
  public static final Invoke invokeSpecial = new Invoke("invoke-special", 'P');
  public static final Invoke make = new Invoke("make", 'N');

  public Invoke(String name, char kind)
  {
    super(name);
    this.kind = kind;
    this.language = Language.getDefaultLanguage();
  }

  public Invoke(String name, char kind, Language language)
  {
    super(name);
    this.kind = kind;
    this.language = language;
  }

  public static Object invoke$V(Object[] args) throws Throwable
  {
    return applyN(invoke, args);
  }

  public static Object invokeStatic$V(Object[] args) throws Throwable
  {
    return applyN(invokeStatic, args);
  }

  public static Object make$V(Object[] args) throws Throwable
  {
    return applyN(make, args);
  }

  private static ClassType typeFrom (Object arg, Invoke thisProc)
  {
    if (arg instanceof Class)
      arg = Type.make((Class) arg);
    if (arg instanceof ClassType)
      return (ClassType) arg;
    if (arg instanceof String || arg instanceof FString)
      return ClassType.make(arg.toString());
    if (arg instanceof Symbol)
      return ClassType.make(((Symbol) arg).getName());
    throw new WrongType(thisProc, 0, null);
  }

  public void apply (CallContext ctx) throws Throwable
  {
    apply(ctx.getArgs(), ctx);
  }

  public void apply (Object[] args, CallContext ctx) throws Throwable
  {
    if (kind=='S' || kind=='V')
      {
        // The following is an optimization, so that output from the
        // method is sent directly to ctx.consumer, rather than reified.
        int nargs = args.length;
        Procedure.checkArgCount(this, nargs);
        Object arg0 = args[0];
        String mname;
        ClassType dtype = (kind != 'V' ? typeFrom(arg0, this)
                           : (ClassType) Type.make(arg0.getClass()));
        Object arg1 = args[1];
        if (arg1 instanceof String || arg1 instanceof FString)
          mname = arg1.toString();
	else if (arg1 instanceof Symbol)
	  mname = ((Symbol) arg1).getName();
        else
          throw new WrongType(this, 1, null);
        mname = Compilation.mangleName(mname);
        Procedure proc = ClassMethods.apply(dtype, mname, null, null,
                                            0, kind=='S' ? 0 : Access.STATIC);
        if (proc == null)
          throw new RuntimeException(getName() + ": no method named `"
                                     + mname + "' in class " + dtype.getName());
        Object[] margs = new Object[nargs-(kind == 'S' ? 2 : 1)];
        int i = 0;
        if (kind == 'V')
          margs[i++] = args[0];
        System.arraycopy(args, 2, margs, i, nargs - 2);
        proc.checkN(margs, ctx);
      }
    else
      ctx.writeValue(this.applyN(args));
  }

  public Object applyN (Object[] args) throws Throwable
  {
    return applyN(this, args);
  }

  protected static Object applyN (Invoke thisProc, Object[] args)
    throws Throwable
  {
    int kind = thisProc.kind;
    if (kind == 'P')
      throw new RuntimeException(thisProc.getName() 
                                 + ": invoke-special not allowed at run time");
    
    int nargs = args.length;
    Procedure.checkArgCount(thisProc, nargs);
    Object arg0 = args[0];
    ClassType dtype = (kind != 'V' ? typeFrom(arg0, thisProc)
                       : (ClassType) Type.make(arg0.getClass()));
    String mname;
    if (kind == 'N')
      {
	mname = "<init>";
	if (dtype instanceof PairClassType)
	  {
	    PairClassType ptype = (PairClassType) dtype;
	    dtype = ptype.instanceType;
	  }
      }
    else
      {
        Object arg1 = args[1];
        if (arg1 instanceof String || arg1 instanceof FString)
          mname = arg1.toString();
	else if (arg1 instanceof Symbol)
	  mname = ((Symbol) arg1).getName();
        else
          throw new WrongType(thisProc, 1, null);
        mname = Compilation.mangleName(mname);
      }
    MethodProc proc
      = ClassMethods.apply(dtype, mname, null, null,
                           thisProc.kind=='s' ? Access.STATIC : 0,
                           thisProc.kind=='S' ? 0 : Access.STATIC);
    if (proc == null)
      throw new RuntimeException(thisProc.getName() + ": no method named `"
                                 + mname + "' in class " + dtype.getName());
    if (kind != 'N')
      {
        Object[] margs = new Object[nargs-(kind == 'S' || kind == 's' ? 2 : 1)];
        int i = 0;
        if (kind == 'V')
          margs[i++] = args[0];
        System.arraycopy(args, 2, margs, i, nargs - 2);
        return proc.applyN(margs);
      }
    else
      {
        CallContext vars = CallContext.getInstance();
        int err = proc.matchN(args, vars);
        if (err == 0)
          return vars.runUntilValue();
        else if ((nargs & 1) == 1)
          {
            // Check if args is a set of (keyword,value)-pairs.
            for (int i = 1;  i < nargs;  i += 2)
              {
                if (! (args[i] instanceof Keyword))
                  throw MethodProc.matchFailAsException(err, thisProc, args);
              }

            Object result;
            result = proc.apply1(args[0]);
            for (int i = 1;  i < nargs;  i += 2)
              {
                Keyword key = (Keyword) args[i];
                Object arg = args[i+1];
                SlotSet.apply(false, result, key.getName(), arg);
              }
            return result;
          }
        throw MethodProc.matchFailAsException(err, thisProc, args);
      }
  }

  public int numArgs()
  {
    return (-1 << 12) | (kind == 'N' ? 1 : 2);
  }

  private PrimProcedure[] cacheMethods;
  private Expression[] cacheArgs;
  private int cacheDefinitelyApplicableMethodCount;
  private int cachePossiblyApplicableMethodCount;

  protected PrimProcedure[] getMethods(ClassType ctype, String mname,
                                       Expression[] args, int margsLength, 
                                       int argsStartIndex, int objIndex,
				       ClassType caller)
  {
    if (args == cacheArgs)
      return cacheMethods;

    Type[] atypes = new Type[margsLength];

    int dst = 0;
    if (objIndex >= 0)
      atypes[dst++] = ctype;
    for (int src = argsStartIndex; 
         src < args.length && dst < atypes.length; 
         src++, dst++)
      atypes[dst] = args[src].getType();

    PrimProcedure[] methods
      = ClassMethods.getMethods(ctype, mname,
                                kind == 's' ? Access.STATIC : 0,
                                kind == 'S' ? 0 : Access.STATIC,
                                kind == 'P',
                                caller, language);
    
    long num = ClassMethods.selectApplicable(methods, atypes);
    cacheArgs = args;
    cacheDefinitelyApplicableMethodCount = (int) (num >> 32);
    cachePossiblyApplicableMethodCount = (int) num;
    cacheMethods = methods;
    return cacheMethods;
  }

  /** Return an array if args (starting with start) is a set of
   * (keyword, value)-value pairs. */
  static Object[] checkKeywords(Type type, Expression[] args,
                                int start, ClassType caller)
  {
    int len = args.length;
    if (((len - start) & 1) != 0)
      return null;
    Object[] fields = new Object[(len-start) >> 1];
    for (int i = fields.length;  -- i>= 0; )
      {
        Expression arg = args[start + 2 * i];
        if (! (arg instanceof QuoteExp))
          return null;
        Object value = ((QuoteExp) arg).getValue();
        if (! (value instanceof Keyword))
          return null;
        String name = ((Keyword) value).getName();
        Object slot = SlotSet.getField((ClassType) type, name, caller);
        fields[i] = slot != null ? slot : name;
      }
    return fields;
  }

  /** Resolve class specifier to ClassType at inline time.
   * This is an optimization to avoid having a module-level binding
   * created for the class name. */

  public static Expression inlineClassName (ApplyExp exp, int carg,
					    InlineCalls walker)
  {
    Compilation comp = walker.getCompilation();
    Language language = comp.getLanguage();
    Expression[] args = exp.getArgs();
    if (args.length > carg)
      {
	Type type = language.getTypeFor(args[carg]);
	if (type instanceof PairClassType)
	  type = ((PairClassType) type).instanceType;
	else if (! (type instanceof Type))
	  return exp;
	if (type instanceof ClassType && ((ClassType) type).isExisting())
	  {
	    try
	      {
		type.getReflectClass();
	      }
	    catch (Exception ex)
	      {
		comp.error('e', "unknown class: " + type.getName());
	      }
	  }
	Expression[] nargs = new Expression[args.length];
	System.arraycopy(args, 0, nargs, 0, args.length);
	nargs[carg] = new QuoteExp(type);
	return new ApplyExp(exp.getFunction(), nargs).setLine(exp);
      }
    return exp;
  }

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    return inline(exp, walker.getCompilation());
  }

  private Expression inline (ApplyExp exp, Compilation comp)
  {
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    if (nargs == 0 || (kind == 'V' && nargs == 1))
      // This should never happen, as InlineCalls.walkApplyExp
      // checks the number of arguments before inline is called.
      return exp;
    ClassType type;
    Expression arg0 = args[0];
    Type type0 = (kind == 'V' ? arg0.getType() : language.getTypeFor(arg0));
    if (type0 instanceof PairClassType)
      type = ((PairClassType) type0).instanceType;
    else if (type0 instanceof ClassType)
      type = (ClassType) type0;
    else
      type = null;
    String name = getMethodName(args);

    int margsLength, argsStartIndex, objIndex;
    if (kind == 'V')                     // Invoke virtual
      {
        margsLength = nargs - 1;
        argsStartIndex = 2;
        objIndex = 0;
      }
    else if (kind == 'N')                // make new
      {
        margsLength = nargs;
        argsStartIndex = 0;
        objIndex = -1;
      }
    else if (kind == 'S' || kind == 's') // Invoke static
      {
        margsLength = nargs - 2;
        argsStartIndex = 2;
        objIndex = -1;
      }
    else if (kind == 'P')                // Invoke special
      {
        margsLength = nargs - 2;
        argsStartIndex = 3;
        objIndex = 1;
      }
    else
      return exp;

    Declaration decl;
    if (type != null && name != null)
      {
        PrimProcedure[] methods;
        int okCount, maybeCount;
        ClassType caller = comp == null ? null
          : comp.curClass != null ? comp.curClass
          : comp.mainClass;
        synchronized (this)
          {
            try
              {
                methods = getMethods(type, name, args, 
                                     margsLength, argsStartIndex, objIndex,
				     caller);
              }
            catch (Exception ex)
              {
                comp.error('w', "unknown class: " + type.getName());
                methods = null;
              }
            okCount = cacheDefinitelyApplicableMethodCount;
            maybeCount = cachePossiblyApplicableMethodCount;
          }
        if (methods != null)
          {
            int index = -1;
            if (methods.length == 0)
	      {
		if (comp.getBooleanOption("warn-invoke-unknown-method", true))
		  comp.error('w', "no accessible method '"+name+"' in "+type.getName());
	      }
            else if (okCount + maybeCount == 0)
              {
                Object[] slots;
                if (kind == 'N'
                    && (ClassMethods.selectApplicable(methods,
                                                      new Type[] { Compilation.typeClassType })
                        >> 32) == 1
                    && (slots = checkKeywords(type, args, 1, caller)) != null)
                  {
                    StringBuffer errbuf = null;
                    for (int i = 0;  i < slots.length;  i++)
                      {
                        if (slots[i] instanceof String)
                          {
                            if (errbuf == null)
                              {
                                errbuf = new StringBuffer();
                                errbuf.append("no field or setter ");
                              }
                            else
                              errbuf.append(", ");
                            errbuf.append('`');
                            errbuf.append(slots[i]);
                            errbuf.append('\'');
                          }
                      }
                    if (errbuf != null)
                      {
                        errbuf.append(" in class ");
                        errbuf.append(type.getName());
                        comp.error('w', errbuf.toString());
                      }
                    else
                      {
			ApplyExp e = new ApplyExp(methods[0],
                                                  new Expression[] { arg0 });
                        for (int i = 0;  i < slots.length;  i++)
                          {
			    Expression[] sargs
			      = { e, new QuoteExp(slots[i]), args[2 * i + 2] };
			    e = new ApplyExp(SlotSet.setFieldReturnObject,
					     sargs);
                         }
			return e.setLine(exp);
                      }
                  }
                else
                  comp.error('w', "no possibly applicable method '"
                             +name+"' in "+type.getName());
              }
            else if (okCount == 1 || (okCount == 0 && maybeCount == 1))
              index = 0;
            else if (okCount > 0)
              {
                index = MethodProc.mostSpecific(methods, okCount);
                if (index < 0)
		  {
		    if (kind == 'S')
		      {
			// If we didn't find a most specific method,
			// check if there is one that is static.  If so,
			// prefer that - after all, we're using invoke-static.
			for (int i = 0;  i < okCount;  i++)
			  {
			    if (methods[i].getStaticFlag())
			      {
				if (index >= 0)
				  {
				    index = -1;
				    break;
				  }
				else
				  index = i;
			      }
			  }
		      }
		  }
                if (index < 0
		    && comp.getBooleanOption("warn-invoke-unknown-method",
					     true))
		  {
                    StringBuffer sbuf = new StringBuffer();
                    sbuf.append("more than one definitely applicable method `");
                    sbuf.append(name);
                    sbuf.append("' in ");
                    sbuf.append(type.getName());
                    append(methods, okCount, sbuf);
		    comp.error('w', sbuf.toString());
		  }
              }
	    else if (comp.getBooleanOption("warn-invoke-unknown-method", true))
              {
                StringBuffer sbuf = new StringBuffer();
                sbuf.append("more than one possibly applicable method '");
                sbuf.append(name);
                sbuf.append("' in ");
                sbuf.append(type.getName());
                append(methods, maybeCount, sbuf);
                comp.error('w', sbuf.toString());
	      }
            if (index >= 0)
              {
                Expression[] margs = new Expression[margsLength];
                int dst = 0;
                if (objIndex >= 0)
                  margs[dst++] = args[objIndex];
                for (int src = argsStartIndex; 
                     src < args.length && dst < margs.length; 
                     src++, dst++)
                  margs[dst] = args[src];
                return new ApplyExp(methods[index], margs).setLine(exp);
              }
          }
      }
    return exp;
  }

  private void append (PrimProcedure[] methods, int mcount, StringBuffer sbuf)
  {
    for (int i = 0;  i < mcount;  i++)
      {
        sbuf.append("\n  candidate: ");
        sbuf.append(methods[i]);
      }
  }

  private String getMethodName(Expression[] args)
  {
    if (kind == 'N')
      return "<init>";
    int nameIndex = (kind == 'P' ? 2 : 1);
    if (args.length >= nameIndex + 1)
      return ClassMethods.checkName(args[nameIndex], false);
    return null;
  }

  /** Return an ApplyExp that will call a method with given arguments.
   * @param type the class containing the method we want to call.
   * @param name the name of the method we want to call
   * @param args the arguments to the call
   * @return an ApplyExp representing the call
   */
  public static synchronized
  ApplyExp makeInvokeStatic(ClassType type, String name, Expression[] args)
  {
    PrimProcedure method = getStaticMethod(type, name, args);
    if (method == null)
      throw new RuntimeException("missing or ambiguous method `" + name
                                 + "' in " + type.getName());
    return new ApplyExp(method, args);
  }

  public static synchronized PrimProcedure
  getStaticMethod(ClassType type, String name, Expression[] args)
  {
    PrimProcedure[] methods = invokeStatic.getMethods(type, name, args, 
                                                      args.length, 0, -1, null);
    int okCount = invokeStatic.cacheDefinitelyApplicableMethodCount;
    int maybeCount = invokeStatic.cachePossiblyApplicableMethodCount;
    int index;
    if (methods == null)
      index = -1;
    else if (okCount > 0)
      index = MethodProc.mostSpecific(methods, okCount);
    else if (maybeCount == 1)
      index = 0;
    else
      index = -1;
    return index < 0 ? null : methods[index];
  }
}
