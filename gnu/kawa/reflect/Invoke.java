package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;
import kawa.lang.FString;

public class Invoke extends ProcedureN implements Inlineable
{
  /** 'N' - make (new);  'S' - invoke-static;  'V'  - non-static invoke. */
  char kind;

  public static Invoke invoke = new Invoke("invoke", 'V');
  public static Invoke invokeStatic = new Invoke("invoke-static", 'S');
  public static Invoke make = new Invoke("make", 'N');

  public Invoke(String name, char kind)
  {
    super(name);
    this.kind = kind;
  }

  public static Object invoke$V(Object[] args)
  {
    return applyN(invoke, args);
  }

  public static Object invokeStatic$V(Object[] args)
  {
    return applyN(invokeStatic, args);
  }

  public static Object make$V(Object[] args)
  {
    return applyN(make, args);
  }

  public Object applyN (Object[] args)
  {
    return applyN(this, args);
  }

  protected static Object applyN (Invoke thisProc, Object[] args)
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
        else
          throw new WrongType(thisProc, 0, null);
      }
    if (kind == 'N')
      mname = "<init>";
    else
      {
        Object arg1 = args[1];
        if (arg1 instanceof String || arg1 instanceof FString)
          mname = arg1.toString();
        else
          throw new WrongType(thisProc, 1, null);
        mname = Compilation.mangleName(mname);
      }
    MethodProc proc
      = ClassMethods.apply(dtype, mname, null, null,
                           thisProc.kind=='S' ? Access.STATIC : 0,
                           Access.STATIC);
    Object[] margs = new Object[nargs-(kind == 'S' ? 2 : 1)];
    int i = 0;
    if (kind == 'V')
      margs[i++] = args[0];
    System.arraycopy(args, kind == 'N' ? 1 : 2, margs, i,
                     nargs - (kind == 'N' ? 1 : 2));
    if (kind == 'N')
      {
        Object vars = proc.getVarBuffer();
        RuntimeException ex = proc.match(vars, margs);
        int len = nargs - 1;
        if (ex == null)
          return proc.applyV(vars);
        else if ((len & 1) == 0)
          {
            // Check if margs is a set of (keyword,value)-pairs.
            for (i = 0;  i < len;  i += 2)
              {
                if (! (margs[i] instanceof Keyword))
                  throw ex;
              }

            Object result = proc.apply0();
            for (i = 0;  i < len;  i += 2)
              {
                Keyword key = (Keyword) margs[i];
                Object arg = margs[i+1];
                SlotSet.apply(result, key.getName(), arg);
              }
            return result;
          }
        throw ex;
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
                                       Expression[] args)
  {
    if (args == cacheArgs)
      return cacheMethods;
    int nargs = args.length;
    cacheArgs = args;
    cacheMethods = null;
    int argsToSkip = kind == 'S' ? 2 : 1;
    Type[] atypes = new Type[nargs - argsToSkip];
    int i = 0;
    if (kind == 'V')
      atypes[i++] = ctype;
    for ( ; i < atypes.length;  i++)
      atypes[i] = args[i+argsToSkip].getType();
    PrimProcedure[] methods
    = ClassMethods.getMethods(ctype, mname,
                              kind == 'S' ? Access.STATIC : 0, Access.STATIC,
                              kawa.standard.Scheme.getInstance());
    long num = ClassMethods.selectApplicable(methods, atypes);
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

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    ClassType type = getClassType(args);
    String name = getMethodName(args);
    if (type != null && name != null)
      {
        PrimProcedure[] methods;
        int okCount, maybeCount;
        synchronized (this)
          {
            methods = getMethods(type, name, args);
            okCount = cacheDefinitelyApplicableMethodCount;
            maybeCount = cachePossiblyApplicableMethodCount;
          }
        if (methods != null)
          {
            int index = -1;
            if (methods.length == 0)
              comp.error('w', "no method `"+name+"' in "+type.getName());
            else if (okCount + maybeCount == 0)
              {
                Object[] slots;
                if (kind == 'N'
                    && (ClassMethods.selectApplicable(cacheMethods,
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
                        comp.error('w', errbuf.toString());
                      }
                    else
                      {
                        PrimProcedure method = cacheMethods[0];
                        CodeAttr code = comp.getCode();
                        method.compile(new ApplyExp(method, new Expression[0]),
                                       comp, Target.pushObject);
                        for (int i = 0;  i < slots.length;  i++)
                          {
                            System.err.println("i:"+i+" alen:"+args.length
                                               +" slen:"+slots.length);
                            code.emitDup(type);
                            SlotSet.compileSet(this, type,
                                               args[2 * i + 2],
                                               slots[i], comp);
                         }
                        target.compileFromStack(comp, type);
                        return;
                      }
                  }
                else
                  comp.error('w', "no possibly applicable method `"
                             +name+"' in "+type.getName());
              }
            else if (okCount == 1 || (okCount == 0 && maybeCount == 1))
              index = 0;
            else if (okCount > 0)
              {
                index = MethodProc.mostSpecific(methods, okCount);
                if (index < 0)
                  comp.error('w',
                             "more than one definitelty applicable method `"
                             +name+"' in "+type.getName());
              }
            else
              comp.error('w',
                         "more than one possibly applicable method `"
                         +name+"' in "+type.getName());
            if (index >= 0)
              {
                Expression[] margs
                  = new Expression[nargs-(kind == 'S' ? 2 : 1)];
                int i = 0;
                if (kind == 'V')
                  margs[i++] = args[0];
                System.arraycopy(args, kind == 'N' ? 1 : 2,
                                 margs, i,
                                 nargs - (kind == 'N' ? 1 : 2));
                PrimProcedure method = methods[index];
                method.compile(new ApplyExp(method, margs), comp, target);
                return;
              }
          }
      }
    ApplyExp.compile(exp, comp, target);
  }

  private ClassType getClassType(Expression[] args)
  {
    if (args.length > 0)
      {
        Expression arg0 = args[0];
        Type type = (kind == 'V' ? arg0.getType()
                     : kawa.standard.Scheme.exp2Type(arg0));
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
      return ClassMethods.checkName(args[1]);
    return null;
  }

  public synchronized Type getReturnType (Expression[] args)
  {
    int nargs = args.length;
    if (nargs > 0)
      {
        Expression arg0 = args[0];
        Type type = (kind == 'V' ? arg0.getType()
                     : kawa.standard.Scheme.exp2Type(arg0));
        if (kind == 'N')
          return type == null ? Type.pointer_type : type;
        Object name = null;
        if (nargs >= 2
                 && args[1] instanceof QuoteExp)
          name = ((QuoteExp) args[1]).getValue();
        if (type instanceof ClassType
            && (name instanceof FString || name instanceof String))
          {
            PrimProcedure[] methods = getMethods((ClassType) type,
                                                 name.toString(), args);
            if (methods != null
                && (cacheDefinitelyApplicableMethodCount == 1
                    || (cacheDefinitelyApplicableMethodCount == 0
                        && cachePossiblyApplicableMethodCount == 1)))
              return methods[0].getReturnType(args);
          }
      }
    return Type.pointer_type;
  }
}
