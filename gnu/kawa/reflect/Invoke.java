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

  Interpreter interpreter;

  public static final Invoke invoke = new Invoke("invoke", 'V');
  public static final Invoke invokeStatic = new Invoke("invoke-static", 'S');
  public static final Invoke make = new Invoke("make", 'N');

  public Invoke(String name, char kind)
  {
    super(name);
    this.kind = kind;
    this.interpreter = Interpreter.getInterpreter();
  }

  public Invoke(String name, char kind, Interpreter interpreter)
  {
    super(name);
    this.kind = kind;
    this.interpreter = interpreter;
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

  public Object applyN (Object[] args) throws Throwable
  {
    return applyN(this, args);
  }

  protected static Object applyN (Invoke thisProc, Object[] args)
    throws Throwable
  {
    int nargs = args.length;
    Procedure.checkArgCount(thisProc, nargs);
    Object arg0 = args[0];
    int kind = thisProc.kind;
    ClassType dtype;
    String mname;
    if (kind == 'V')
      dtype = (ClassType) Type.make(arg0.getClass());
    else
      {
        if (arg0 instanceof Class)
          arg0 = Type.make((Class) arg0);
        if (arg0 instanceof ClassType)
          dtype = (ClassType) arg0;
        else if (arg0 instanceof String || arg0 instanceof FString)
          dtype = ClassType.make(arg0.toString());
	else if (arg0 instanceof Symbol)
          dtype = ClassType.make(((Symbol) arg0).getName());
        else
          throw new WrongType(thisProc, 0, null);
      }
    Object staticLink = null;
    if (kind == 'N')
      {
	mname = "<init>";
	if (dtype instanceof PairClassType)
	  {
	    PairClassType ptype = (PairClassType) dtype;
	    dtype = ptype.instanceType;
	    staticLink = ptype.getStaticLink();
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
    Object[] margs
      = new Object[nargs-(kind == 'S' || kind == 's' ? 2 : staticLink != null ? 0 : 1)];
    int i = 0;
    if (kind == 'V')
      margs[i++] = args[0];
    else if (staticLink != null)
      margs[i++] = staticLink;
    System.arraycopy(args, kind == 'N' ? 1 : 2, margs, i,
                     nargs - (kind == 'N' ? 1 : 2));
    if (kind == 'N')
      {
        CallContext vars = CallContext.getInstance();
        int err = proc.match(vars, margs);
        int len = nargs - 1;
        if (err == 0)
          return proc.applyV(vars);
        else if ((len & 1) == 0)
          {
            // Check if margs is a set of (keyword,value)-pairs.
            for (i = 0;  i < len;  i += 2)
              {
                if (! (margs[i] instanceof Keyword))
                  throw MethodProc.matchFailAsException(err, thisProc, args);
              }

            Object result;
	    if (staticLink == null)
	      {
		result = proc.apply0();
		i = 0;
	      }
	    else
	      {
		result = proc.apply1(staticLink);
		i = 1;
	      }
            for (;  i < len;  i += 2)
              {
                Keyword key = (Keyword) margs[i];
                Object arg = margs[i+1];
                SlotSet.apply(false, result, key.getName(), arg);
              }
            return result;
          }
        throw MethodProc.matchFailAsException(err, thisProc, args);
      }
    return proc.applyN(margs);
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
                                       Expression[] args, int argsToSkip)
  {
    if (args == cacheArgs)
      return cacheMethods;
    int nargs = args.length;
    Type[] atypes = new Type[nargs - argsToSkip];
    int i = 0;
    if (kind == 'V')
      atypes[i++] = ctype;
    for ( ; i < atypes.length;  i++)
      atypes[i] = args[i+argsToSkip].getType();
    PrimProcedure[] methods
    = ClassMethods.getMethods(ctype, mname,
                              kind == 's' ? Access.STATIC : 0,
			      kind=='S' ? 0 : Access.STATIC,
                              interpreter);

    long num = ClassMethods.selectApplicable(methods, atypes);
    cacheArgs = args;
    cacheDefinitelyApplicableMethodCount = (int) (num >> 32);
    cachePossiblyApplicableMethodCount = (int) num;
    cacheMethods = methods;
    return cacheMethods;
  }

  /** Return an array if args (starting with start) is a set of
   * (keyword, value)-value pairs. */
  static Object[] checkKeywords(Type type, Expression[] args, int start)
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
        Object slot = SlotSet.getField(type, name);
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
    Interpreter interpreter = comp.getInterpreter();
    Expression[] args = exp.getArgs();
    if (args.length > carg)
      {
	Type type = interpreter.getTypeFor(args[carg]);
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
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    ClassType type = getClassType(args);
    String name = getMethodName(args);
    if (type != null && name != null
	// We can't generate <init> until we know whether it needs
	// a lexical link, which we don't know until FindCapturedVars is run.
	// Unfortunately, this means make doesn't get property inlined.  FIXME
	&& (kind != 'N' || type.isExisting()))
      {
        PrimProcedure[] methods;
        int okCount, maybeCount;
        synchronized (this)
          {
            try
              {
                methods = getMethods(type, name, args,
				     kind == 'S' || kind == 's' ? 2 : 1);
              }
            catch (Exception ex)
              {
                walker.error('w', "unknown class: " + type.getName());
                methods = null;
              }
            okCount = cacheDefinitelyApplicableMethodCount;
            maybeCount = cachePossiblyApplicableMethodCount;
          }
        if (methods != null)
          {
            int index = -1;
            if (methods.length == 0)
              walker.error('w', "no method `"+name+"' in "+type.getName());
            else if (okCount + maybeCount == 0)
              {
                Object[] slots;
                if (kind == 'N'
                    && (ClassMethods.selectApplicable(methods,
                                                      Type.typeArray0)
                        >> 32) == 1
                    && (slots = checkKeywords(type, args, 1)) != null)
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
                        walker.error('w', errbuf.toString());
                      }
                    else
                      {
                        PrimProcedure method = methods[0];
			ApplyExp e = new ApplyExp(method, new Expression[0]);
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
                  walker.error('w', "no possibly applicable method `"
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
                if (index < 0)
		  {
		    walker.error('w',
			       "more than one definitely applicable method `"
			       +name+"' in "+type.getName());
		    for (int i = 0;  i < okCount;  i++)
		      walker.error('w', "candidate: " + methods[i]);
		  }
              }
	    else if (okCount == 0)
	      {
		walker.error('w',
			   "no definitely applicable method `"
			   +name+"' in "+type.getName());
	      }
            else
	      {
		walker.error('w',
			   "more than one possibly applicable method `"
			   +name+"' in "+type.getName());
		for (int i = 0;  i < okCount; )
		  walker.error('w', "candidate: " + methods[i]);
	      }
            if (index >= 0)
              {
                Expression[] margs
                  = new Expression[nargs-(kind == 'S' || kind == 's' ? 2 : 1)];
                int i = 0;
                if (kind == 'V')
		  margs[i++] = args[0];
                System.arraycopy(args, kind == 'N' ? 1 : 2,
                                 margs, i,
                                 nargs - (kind == 'N' ? 1 : 2));
                PrimProcedure method = methods[index];
                return new ApplyExp(method, margs).setLine(exp);
              }
          }
      }
    return exp;
  }

  private ClassType getClassType(Expression[] args)
  {
    if (args.length > 0)
      {
        Expression arg0 = args[0];
        Type type = (kind == 'V' ? arg0.getType()
                     : interpreter.getTypeFor(arg0));
	if (type instanceof PairClassType)
	  return ((PairClassType) type).instanceType;
        if (type instanceof ClassType)
	  return (ClassType) type;
      }
    return null;
  }

  private String getMethodName(Expression[] args)
  {
    if (kind == 'N')
      return "<init>";
    if (args.length >= 2)
      return ClassMethods.checkName(args[1], false);
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
    PrimProcedure[] methods = invokeStatic.getMethods(type, name, args, 0);
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

  public static synchronized PrimProcedure
  getMethod(ClassType type, String name, boolean isStatic,
	    Type[] argTypes, Interpreter interpreter)
  {
    PrimProcedure[] methods
      = ClassMethods.getMethods(type, name,
				isStatic ? Access.STATIC : 0, Access.STATIC,
				interpreter);
    long num = ClassMethods.selectApplicable(methods, argTypes);
    int okCount = (int) (num >> 32);
    int maybeCount = (int) num;
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
