package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.functions.CompileArith;
import gnu.math.IntNum;

public class CompileInvoke
{
  public static Expression validateApplyInvoke
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    Invoke iproc = (Invoke) proc;
    char kind = iproc.kind;
    Compilation comp = visitor.getCompilation();
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    if (! comp.mustCompile
        // This should never happen, as InlineCalls.visitApplyExp
        // checks the number of arguments before inline is called.
        || nargs == 0 || ((kind == 'V' || kind == '*') && nargs == 1))
      {
        exp.visitArgs(visitor);
        return exp;
      }
    ObjectType type;
    Expression arg0 = visitor.visit(args[0], null);
    args[0] = arg0;
    Type type0 = (kind == 'V' || kind == '*' ? arg0.getType() : iproc.language.getTypeFor(arg0));
    if (type0 instanceof PairClassType && kind == 'N')
      type = ((PairClassType) type0).instanceType;
    else if (type0 instanceof ObjectType)
      type = (ObjectType) type0;
    else
      type = null;
    String name = getMethodName(args, kind);

    int margsLength, argsStartIndex, objIndex;
    if (kind == 'V' || kind == '*')      // Invoke virtual
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
      {
        exp.visitArgs(visitor);
        return exp;
      }

    if (kind == 'N' && type instanceof ArrayType)
      {
        ArrayType atype = (ArrayType) type;
        Type elementType = atype.getComponentType();
        Expression sizeArg = null;
        boolean lengthSpecified = false;
        if (args.length >= 3 && args[1] instanceof QuoteExp)
          {
            Object arg1 = ((QuoteExp) args[1]).getValue();
            if (arg1 instanceof Keyword
                 && ("length".equals(name = ((Keyword) arg1).getName())
                     || "size".equals(name)))
              {
                sizeArg = args[2];
                lengthSpecified = true;
              }
          }
        if (sizeArg == null)
          sizeArg = QuoteExp.getInstance(new Integer(args.length-1));
        sizeArg = visitor.visit(sizeArg, Type.intType);
        Expression alloc = new ApplyExp(new ArrayNew(elementType),
                                        new Expression[] { sizeArg } );
        if (lengthSpecified && args.length == 3)
          return alloc;
        LetExp let = new LetExp(new Expression[] { alloc });
        Declaration adecl = let.addDeclaration((String) null, atype);
        adecl.noteValue(alloc);
        BeginExp begin = new BeginExp();
        int index = 0;
        for (int i = lengthSpecified ? 3 : 1; i < args.length;  i++)
          {
            Expression arg = args[i];
            if (lengthSpecified && i+1 < args.length && arg instanceof QuoteExp)
              {
                Object key = ((QuoteExp) arg).getValue();
                if (key instanceof Keyword)
                  {
                    String kname = ((Keyword) key).getName();
                    try
                      {
                        index = Integer.parseInt(kname);
                        arg = args[++i];
                      }
                    catch (Throwable ex)
                      {
                        comp.error('e', "non-integer keyword '"+kname+"' in array constructor");
                        return exp;
                      }
                  }
              }
            arg = visitor.visit(arg, elementType);
            begin.add(new ApplyExp(new ArraySet(elementType),
                                   new Expression[] {
                                     new ReferenceExp(adecl),
                                     QuoteExp.getInstance(new Integer(index)),
                                     arg}));
            index++;
          }
        begin.add(new ReferenceExp(adecl));
        let.body = begin;
        return let;
      }
    else if (type != null && name != null)
      {
        if (type instanceof TypeValue && kind == 'N')
          {
            Procedure constructor = ((TypeValue) type).getConstructor();
            if (constructor != null)
              {
                Expression[] xargs = new Expression[nargs-1];
                System.arraycopy(args, 1, xargs, 0, nargs-1);
                return visitor.visit(new ApplyExp(constructor, xargs), required);
              }
          }
        PrimProcedure[] methods;
        int okCount, maybeCount;
        ClassType caller = comp == null ? null
          : comp.curClass != null ? comp.curClass
          : comp.mainClass;
        ObjectType ctype = (ObjectType) type;
        exp.visitArgs(visitor);
        try
          {
            methods = getMethods(ctype, name, caller, iproc);
            long num = selectApplicable(methods, ctype, args, 
                                        margsLength, argsStartIndex, objIndex);
            okCount = (int) (num >> 32);
            maybeCount = (int) num;
          }
        catch (Exception ex)
          {
            comp.error('w', "unknown class: " + type.getName());
            return exp;
          }
        int index = -1;
        Object[] slots;
        if (okCount + maybeCount == 0
            && kind == 'N'
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
                return exp;
              }
            else
              {
                ApplyExp e = new ApplyExp(methods[0],
                                          new Expression[] { arg0 });
                for (int i = 0;  i < slots.length;  i++)
                  {
                    Expression[] sargs
                      = { e, new QuoteExp(slots[i]), args[2 * i + 2] };
                    e = new ApplyExp(SlotSet.setFieldReturnObject, sargs);
                  }
                return e.setLine(exp);
              }
          }
        int nmethods = methods.length;
        if (okCount + maybeCount == 0 && kind == 'N')
          {
            methods = getMethods(ctype, "valueOf", caller, Invoke.invokeStatic);
            argsStartIndex = 1;
            margsLength = nargs - 1;
            long num = selectApplicable(methods, ctype, args,
                                        margsLength, argsStartIndex, -1);
            okCount = (int) (num >> 32);
            maybeCount = (int) num;
          }
        if (okCount + maybeCount == 0)
          {
            if (kind == 'P'
                || comp.getBooleanOption("warn-invoke-unknown-method", true))
              {
                if (kind=='N')
                  name = name+"/valueOf";
                String message = 
                  (nmethods + methods.length == 0
                   ? "no accessible method '"+name+"' in "+type.getName()
                   : ("no possibly applicable method '"
                      +name+"' in "+type.getName()));
                comp.error(kind == 'P' ? 'e' : 'w', message);
              }
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
                && (kind == 'P'
                    || comp.getBooleanOption("warn-invoke-unknown-method", true)))
              {
                StringBuffer sbuf = new StringBuffer();
                sbuf.append("more than one definitely applicable method `");
                sbuf.append(name);
                sbuf.append("' in ");
                sbuf.append(type.getName());
                append(methods, okCount, sbuf);
                comp.error(kind == 'P' ? 'e' : 'w', sbuf.toString());
              }
          }
        else if (kind == 'P'
                 || comp.getBooleanOption("warn-invoke-unknown-method", true))
          {
            StringBuffer sbuf = new StringBuffer();
            sbuf.append("more than one possibly applicable method '");
            sbuf.append(name);
            sbuf.append("' in ");
            sbuf.append(type.getName());
            append(methods, maybeCount, sbuf);
            comp.error(kind == 'P' ? 'e' : 'w', sbuf.toString());
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
              margs[dst] = visitor.visit(args[src], methods[index].getParameterType(dst));
            return new ApplyExp(methods[index], margs).setLine(exp);
          }
      }
    exp.visitArgs(visitor);
    return exp;
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
        Member slot = SlotSet.lookupMember((ObjectType) type, name, caller);
        fields[i] = slot != null ? (Object) slot : (Object) name;
      }
    return fields;
  }

  private static String getMethodName(Expression[] args, char kind)
  {
    if (kind == 'N')
      return "<init>";
    int nameIndex = (kind == 'P' ? 2 : 1);
    if (args.length >= nameIndex + 1)
      return ClassMethods.checkName(args[nameIndex], false);
    return null;
  }

  private static void append (PrimProcedure[] methods, int mcount, StringBuffer sbuf)
  {
    for (int i = 0;  i < mcount;  i++)
      {
        sbuf.append("\n  candidate: ");
        sbuf.append(methods[i]);
      }
  }

  protected static PrimProcedure[] getMethods(ObjectType ctype, String mname,
                                              ClassType caller, Invoke iproc)
  {
    int kind = iproc.kind;
    return ClassMethods.getMethods(ctype, mname,
                                   kind == 'P' ? 'P'
                                   : kind == '*' || kind == 'V' ? 'V'
                                   : '\0',
                                   caller, iproc.language);
  }

  private static long selectApplicable(PrimProcedure[] methods,
                                       ObjectType ctype,
                                       Expression[] args, int margsLength, 
                                       int argsStartIndex, int objIndex)
  {
    Type[] atypes = new Type[margsLength];

    int dst = 0;
    if (objIndex >= 0)
      atypes[dst++] = ctype;
    for (int src = argsStartIndex; 
         src < args.length && dst < atypes.length; 
         src++, dst++)
      {
        Expression arg = args[src];
        Type atype = null;
        // Treat IntNum constant argument in int/long range as int/long.
        if (arg instanceof QuoteExp)
          {
            QuoteExp qexp = (QuoteExp) arg;
            Object val = qexp.getValue();
            if (val instanceof IntNum && qexp.getRawType() == null)
              {
                IntNum inum = (IntNum) val;
                if (CompileArith.inRange(inum, Integer.MIN_VALUE, Integer.MAX_VALUE))
                  atype = Type.intType;
                else if (CompileArith.inRange(inum, Long.MIN_VALUE, Long.MAX_VALUE))
                  atype = Type.longType;
              }
          }
        if (atype == null)
          atype = arg.getType();
        atypes[dst] = atype;
      }
    return ClassMethods.selectApplicable(methods, atypes);
  }

  public static synchronized PrimProcedure
  getStaticMethod(ClassType type, String name, Expression[] args)
  {
    PrimProcedure[] methods = getMethods(type, name, null, Invoke.invokeStatic);
    long num = selectApplicable(methods, type, args, args.length, 0, -1);
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
