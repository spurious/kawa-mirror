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
        ApplyExp alloc = new ApplyExp(new ArrayNew(elementType),
                                      new Expression[] { sizeArg } );
        alloc.setType(atype);
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
        ClassType caller = comp == null ? null
          : comp.curClass != null ? comp.curClass
          : comp.mainClass;
        ObjectType ctype = (ObjectType) type;
        int numCode;
        try
          {
            methods = getMethods(ctype, name, caller, iproc);
            numCode = ClassMethods.selectApplicable(methods, margsLength);
          }
        catch (Exception ex)
          {
            comp.error('w', "unknown class: " + type.getName());
            return exp;
          }
        int index = -1;
        Object[] slots;
        int keywordStart;
        if (kind == 'N'
            && ((keywordStart = hasKeywordArgument(1, args)) < args.length
                || (numCode <= 0
                    // There is a default constructor.
                    && (ClassMethods.selectApplicable(methods,
                                                      new Type[] { Compilation.typeClassType })
                        >> 32) == 1))
            && ((slots = checkKeywords(ctype, args, keywordStart, caller))
                .length * 2 == (args.length - keywordStart)
                || ClassMethods.selectApplicable(ClassMethods.getMethods(ctype, "add", 'V', null, iproc.language), 2) > 0))
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
                ApplyExp ae;
                if (keywordStart < args.length)
                  {
                    Expression[] xargs = new Expression[keywordStart];
                    System.arraycopy(args, 0, xargs, 0, keywordStart);
                    ae = (ApplyExp) visitor.visit(new ApplyExp(exp.getFunction(), xargs), ctype);
                  }
                else
                  ae = new ApplyExp(methods[0], new Expression[] { arg0 });
                ae.setType(ctype);
                Expression e = ae;
                if (args.length > 0)
                  {
                    for (int i = 0;  i < slots.length;  i++)
                      {
                        Object slot = slots[i];
                        Type stype;
                        if (slot instanceof Method)
                          stype = ((Method) slot).getParameterTypes()[0];
                        else if (slot instanceof Field)
                          stype = ((Field) slot).getType();
                        else
                          stype = null;
                        if (stype != null)
                          stype = iproc.language.getLangTypeFor(stype);
                        Expression arg = visitor.visit(args[keywordStart + 2 * i + 1], stype);
                        Expression[] sargs
                          = { ae, new QuoteExp(slot), arg};
                        ae = new ApplyExp(SlotSet.setFieldReturnObject, sargs);
                        ae.setType(ctype);
                      }
                    int sargs = keywordStart == args.length ? 1
                      : 2 * slots.length + keywordStart;
                    e = ae;
                    if (sargs < args.length)
                      {
                        LetExp let = new LetExp(new Expression[] { e });
                        Declaration adecl = let.addDeclaration((String) null, ctype);
                        adecl.noteValue(e);
                        BeginExp begin = new BeginExp();
                        for (int i = sargs;  i < args.length;  i++)
                          {
                            Expression[]  iargs = {
                              new ReferenceExp(adecl),
                              QuoteExp.getInstance("add"),
                              args[i]
                            };
                            begin.add(visitor.visit(new ApplyExp(Invoke.invoke,
                                                                 iargs),
                                                    null));
                          }
                        begin.add(new ReferenceExp(adecl));
                        let.body = begin;
                        e = let;
                      }
                  }
                return visitor.checkType(e.setLine(exp), required);
              }
          }
        int okCount, maybeCount;
        if (numCode >= 0)
          {
            for (int i = 1;  i < nargs; i++)
              {
                Type atype = null;
                boolean last = i == nargs-1;
                if ((kind == 'P' && i == 2) || (kind != 'N' && i == 1))
                  atype = null; // actually string or symbol
                else if (kind == 'P' && i == 1)
                  atype = ctype;
                else if (numCode > 0)
                  {
                    int pi = i - (kind == 'N' ? 1 : argsStartIndex);
                    for (int j = 0;  j < numCode;  j++)
                      {
                        PrimProcedure pproc = methods[j];
                        int pii = pi+(kind!='S'&&pproc.takesTarget()?1:0);
                        // KLUDGE If varargs, then the last parameter is
                        // special: If can be an array, or a single argument.
                        if (last && pproc.takesVarArgs()
                            && pii == pproc.minArgs())
                          atype = null;
                        else
                          {
                            Type ptype = pproc.getParameterType(pii);
                            if (j==0)
                              atype = ptype;
                            else if ((ptype instanceof PrimType) != (atype instanceof PrimType))
                              atype = null;
                            else
                              {
                                atype = Type.lowestCommonSuperType(atype, ptype);
                              }
                          }
                        if (atype == null)
                          break;
                      }
                  }
                args[i] = visitor.visit(args[i], atype);
              }
            long num = selectApplicable(methods, ctype, args, 
                                        margsLength, argsStartIndex, objIndex);
            okCount = (int) (num >> 32);
            maybeCount = (int) num;
          }
        else
          {
            okCount = 0;
            maybeCount = 0;
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
            if (kind == 'P' || comp.warnInvokeUnknownMethod())
              {
                if (kind=='N')
                  name = name+"/valueOf";
                StringBuilder sbuf = new StringBuilder();;
                if (nmethods + methods.length == 0)
                  sbuf.append("no accessible method '");
                else if (numCode == MethodProc.NO_MATCH_TOO_FEW_ARGS)
                  sbuf.append("too few arguments for method '");
                else if (numCode == MethodProc.NO_MATCH_TOO_MANY_ARGS)
                  sbuf.append("too many arguments for method '");
                else
                  sbuf.append("no possibly applicable method '");
                sbuf.append(name);
                sbuf.append("' in ");
                sbuf.append(type.getName());
                comp.error(kind == 'P' ? 'e' : 'w', sbuf.toString());
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
                && (kind == 'P' || comp.warnInvokeUnknownMethod()))
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
        else if (kind == 'P' || comp.warnInvokeUnknownMethod())
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
            PrimProcedure method = methods[index];
            boolean variable = method.takesVarArgs();
            int dst = 0;
            if (objIndex >= 0)
              margs[dst++] = args[objIndex];
            for (int src = argsStartIndex; 
                 src < args.length && dst < margs.length; 
                 src++, dst++)
              {
                margs[dst] = args[src];
              }
            ApplyExp e = new ApplyExp(method, margs);
            e.setLine(exp);
            return visitor.visitApplyOnly(e, required);
          }
      }
    exp.visitArgs(visitor);
    return exp;
  }

  /** Return an array if args (starting with start) is a set of
   * (keyword, value)-value pairs. */
  static Object[] checkKeywords(ObjectType type, Expression[] args,
                                int start, ClassType caller)
  {
    int len = args.length;
    int npairs = 0;
    while (start + 2 * npairs + 1 < len
           && args[start + 2 * npairs].valueIfConstant() instanceof Keyword)
      npairs++;
    Object[] fields = new Object[npairs];
    for (int i = 0;  i < npairs;  i++)
      {
        Object value = args[start + 2 * i].valueIfConstant();
        String name = ((Keyword) value).getName();
        // Look for field name for a "set" method.
        Member slot = SlotSet.lookupMember(type, name, caller);
        if (slot == null)
          {
            // Look for for an "add" method.
            // For example: (define b (JButton action-listener: ...))
            // maps to: (define b ...) (b:addActionListener ...)
            slot = type.getMethod(ClassExp.slotToMethodName("add", name), SlotSet.type1Array);
          }
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

  static int hasKeywordArgument (int argsStartIndex, Expression[] args)
  {
    for (int i = argsStartIndex; i < args.length; i++)
      {
        if (args[i].valueIfConstant() instanceof Keyword)
          return i;
      }
    return args.length;
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
        if (InlineCalls.checkIntValue(arg) != null)
          atype = Type.intType;
        else if (InlineCalls.checkLongValue(arg) != null)
          atype = Type.longType;
        else if (atype == null)
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
