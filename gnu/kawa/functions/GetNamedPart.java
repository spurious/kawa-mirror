package gnu.kawa.functions;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.kawa.reflect.*;
import gnu.expr.*;
import java.io.*;
import kawa.lang.Translator;

/** Procedure to get the value of a named component of an object. */

public class GetNamedPart extends Procedure2 implements HasSetter, CanInline
{
  public static final GetNamedPart getNamedPart = new GetNamedPart();

  /** {@code PREFIX:<>} is equivalent to the {@code ClassType} bound to {@code PREFIX}. */
  public static final String CLASSTYPE_FOR = "<>";

  /** Pseudo-method-name for the cast operation. */
  public static final String CAST_METHOD_NAME = "@";

  /** Pseudo-method-name for class-membership-test (instanceof) operation. */
  public static final String INSTANCEOF_METHOD_NAME = "instance?";

  public static String combineName (Expression part1, Expression part2)
  {
    String name1;
    Object name2;
    if ((name2 = part2.valueIfConstant()) instanceof SimpleSymbol
        && ((part1 instanceof ReferenceExp
             && (name1 = ((ReferenceExp) part1).getSimpleName()) != null)
            || (part1 instanceof GetNamedExp
                && (name1 = ((GetNamedExp) part1).combinedName) != null)))
      return (name1+':'+name2).intern();
    return null;
  }

  public static Expression makeExp (Expression clas, Expression member)
  {
    ReferenceExp rexp;
    String combinedName = combineName(clas, member);
    Environment env = Environment.getCurrent();
    if (combinedName != null)
      {
        Translator tr = (Translator) Compilation.getCurrent();
        Symbol symbol = Namespace.EmptyNamespace.getSymbol(combinedName);
        Declaration decl = tr.lexical.lookup(symbol, false/*FIXME*/);
        if (! Declaration.isUnknown(decl))
          return new ReferenceExp(decl);

        Object property = null; // FIXME?
        if (symbol != null && env.isBound(symbol, property))
          return new ReferenceExp(combinedName);
      }
    if (clas instanceof ReferenceExp
        && (rexp = (ReferenceExp) clas).isUnknown())
      {
        Object rsym = rexp.getSymbol();
        Symbol sym = rsym instanceof Symbol ? (Symbol) rsym
          : env.getSymbol(rsym.toString());
        if (env.get(sym, null) == null)
          {
            String name = rexp.getName();
            try
              {
                Class cl = ClassType.getContextClass(name);
                clas = QuoteExp.getInstance(Type.make(cl));
              }
            catch (Throwable ex)
              {
              }
          }
      }
    Expression[] args = { clas, member };
    GetNamedExp exp = new GetNamedExp(args);
    exp.combinedName = combinedName;
    return exp;
  }

  public static Expression makeExp (Expression clas, String member)
  {
    return makeExp(clas, new QuoteExp(member));
  }

  public static Expression makeExp (Type type, String member)
  {
    return makeExp(new QuoteExp(type), new QuoteExp(member));
  }

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
    Expression[] args = exp.getArgs();
    if (args.length != 2 || ! (args[1] instanceof QuoteExp)
        || ! (exp instanceof GetNamedExp))
      return exp;
    Expression context = args[0];
    Declaration decl = null;
    if (context instanceof ReferenceExp)
      {
        ReferenceExp rexp = (ReferenceExp) context;
        if ("*".equals(rexp.getName()))
          return GetNamedInstancePart.makeExp(args[1]);
        decl = rexp.getBinding();
      }

    String mname = ((QuoteExp) args[1]).getValue().toString();
    Type type = context.getType();
    boolean isInstanceOperator = context == QuoteExp.nullExp;
    Compilation comp = walker.getCompilation();
    Language language = comp.getLanguage();
    Type typeval = language.getTypeFor(context, false);
    ClassType caller = comp == null ? null
      : comp.curClass != null ? comp.curClass
      : comp.mainClass;
    GetNamedExp nexp = (GetNamedExp) exp;

    if (typeval != null)
      {
        if (mname.equals(CLASSTYPE_FOR))
          return new QuoteExp(typeval);

        if (typeval instanceof ObjectType)
          {
            if (mname.equals("new"))
              return nexp.setProcedureKind('N');
            if (mname.equals(INSTANCEOF_METHOD_NAME))
              return nexp.setProcedureKind('I');
            if (mname.equals(CAST_METHOD_NAME))
              return nexp.setProcedureKind('C');
          }
      }
    if (typeval instanceof ObjectType)
      {
        if (mname.length() > 1 && mname.charAt(0) == '.')
          {
            // The following would also work:
            // return nexp.setProcedureKind('D');
            // However, it makes optimzing the 'setter' case harder.
            return new QuoteExp(new NamedPart(typeval, mname, 'D'));
          }
        if (Invoke.checkKnownClass(typeval, comp) < 0)
          return exp;
        PrimProcedure[] methods
          = ClassMethods.getMethods((ObjectType) typeval,
                                    Compilation.mangleName(mname),
                                    '\0', caller, language);
        if (methods != null && methods.length > 0)
          {
            nexp.methods = methods;
            return nexp.setProcedureKind('S');
          }
        ApplyExp aexp = new ApplyExp(SlotGet.staticField, args);
        aexp.setLine(exp);
        return walker.walkApplyOnly(aexp);
                            
      }
    if (typeval != null)
      {
        
      }

    /*
    if (type.isSubtype(Compilation.typeValues))
      {
        // FIXME
      }
    */

    if (type.isSubtype(Compilation.typeClassType)
        || type.isSubtype(Type.javalangClassType))
      // The container evaluates to a class (so we should look for a static
      // field or method), but we don't know which class at compile-time.
      // However, we should still optimize it somewhat, above.  FIXME.
      return exp;

    if (type instanceof ObjectType)
      {
        ObjectType otype = (ObjectType) type;
        PrimProcedure[] methods
          = ClassMethods.getMethods(otype, Compilation.mangleName(mname),
                                    'V', caller, language);
        if (methods != null && methods.length > 0)
          {
            nexp.methods = methods;
            return nexp.setProcedureKind('M');
          }

        if (type.isSubtype(typeHasNamedParts))
          {
            Object val;
            if (decl != null
                && (val = Declaration.followAliases(decl).getConstantValue()) != null)
              {
                HasNamedParts value = (HasNamedParts) val;
                if (value.isConstant(mname))
                  {
                    val = value.get(mname);
                    return QuoteExp.getInstance(val);
                  }
              }
            args = new Expression[] { args[0], QuoteExp.getInstance(mname) };
            return new ApplyExp(typeHasNamedParts.getDeclaredMethod("get", 1),
                                args).setLine(exp);
          }

        Member part = SlotGet.lookupMember(otype, mname, caller);
        if (part != null
            || (mname.equals("length") && type instanceof ArrayType))
          {
            // FIXME: future kludge to avoid re-doing SlotGet.getField.
            // args = new Expression[] { context, new QuoteExp(part) });
            ApplyExp aexp = new ApplyExp(SlotGet.field, args);
            aexp.setLine(exp);
            return walker.walkApplyOnly(aexp);
          }
      }

    if (comp.getBooleanOption("warn-invoke-unknown-method", ! comp.immediate))
      comp.error('w', "no known slot '"+mname+"' in "+type.getName());
    return exp;
  }

  static final ClassType typeHasNamedParts
  = ClassType.make("gnu.mapping.HasNamedParts");

  public Object apply2 (Object container, Object part)
    throws Throwable
  {
    if (container instanceof Values)
      {
        Object[] values = ((Values) container).getValues();
        Values result = new Values();
        for (int i = 0;  i < values.length;  i++)
          {
            Values.writeValues(apply2(values[i], part), result);
          }
        return result.canonicalize();
      }
    Symbol sym;
    if (part instanceof Symbol)
      sym = (Symbol) part;
    else
      sym = Namespace.EmptyNamespace.getSymbol(part.toString().intern());
    return getNamedPart(container, sym);
  }

  public static Object getTypePart (Type type, String name)
    throws Throwable
  {
    if (name.equals(CLASSTYPE_FOR))
      return type;

    if (type instanceof ObjectType)
      {
        if (name.equals(INSTANCEOF_METHOD_NAME))
          return new NamedPart(type, name, 'I');
        if (name.equals(CAST_METHOD_NAME))
          return new NamedPart(type, name, 'C');
        if (name.equals("new"))
          return new NamedPart(type, name, 'N');
        if (name.equals(".length")
            || (name.length() > 1 && name.charAt(0) == '.'
                && type instanceof ClassType))
          return new NamedPart(type, name, 'D');
      }

    if (type instanceof ClassType)
      {
        try
          {
            return gnu.kawa.reflect.SlotGet.staticField(type, name);
          }
        catch (Throwable ex)
          {
            // FIXME!
          }
        return ClassMethods.apply(ClassMethods.classMethods, type, name);
      }
    return getMemberPart(type, name);
  }

  public static Object getNamedPart (Object container, Symbol part)
    throws Throwable
  {
    String name = part.getName();
    if (container instanceof HasNamedParts)
      return ((HasNamedParts) container).get(name);
    if (container instanceof Class)
      container = (ClassType) Type.make((Class) container);
    if (container instanceof Package)
      {
        try
          {
            String pname = ((Package) container).getName();
            return ClassType.getContextClass(pname + '.' + name);
          }
        catch (Throwable ex)
          {
          }
      }
    if (container instanceof Type)
      return getTypePart((Type) container, name);
    return getMemberPart(container, part.toString());
  }

  public static Object getMemberPart(Object container, String name)
    throws Throwable
  {
    try
      {
        return gnu.kawa.reflect.SlotGet.field(container, name);
      }
    catch (Throwable ex)
      {
        // FIXME!
      }
    MethodProc methods = ClassMethods.apply((ClassType) ClassType.make(container.getClass()),
                                            Compilation.mangleName(name), '\0',
                                            Language.getDefaultLanguage());
    if (methods != null)
      return new NamedPart(container, name, 'M', methods);
    throw new RuntimeException("no part '"+name+"' in "+container);
  }

  public Procedure getSetter()
  {
    return SetNamedPart.setNamedPart;
  }
}

class GetNamedExp extends ApplyExp
{
  /*
   * 'N' - new (make) - if methodName is "new".
   * 'I' - instance of - if methodName is INSTANCEOF_METHOD_NAME.
   * 'C' - cast - if methodName is CAST_METHOD_NAME.
   * 'T' - type - if methodName is CLASSTYPE_FOR
   * 'M' - non-static method
   * 'S' - static method
   * 'D' - if methodname starts with '.'
   */
  char kind;
  PrimProcedure[] methods;

  public String combinedName;

  public void apply (CallContext ctx) throws Throwable
  {
    if (combinedName != null)
      {
        Environment env = ctx.getEnvironment();
        Symbol sym = env.getSymbol(combinedName);
        Object unb = gnu.mapping.Location.UNBOUND;
        Object property = null;  // FIXME?
        Object value = env.get(sym, property, unb);
        if (value != unb)
          {
            ctx.writeValue(value);
            return;
          }
      }
    super.apply(ctx);
  }

  public GetNamedExp(Expression[] args)
  {
    super(GetNamedPart.getNamedPart, args);
  }

  protected GetNamedExp setProcedureKind (char kind)
  {
    // Called from GetNamedPart.inline when the expression evaluates to a
    // procedure that takes (at least) a 'this' parameter.  If the
    // expression is in turn used in function call position it is normally
    // the first argment to ApplyToArgs, so setting the type to typeProcedure
    // allows ApplyToArgs.inline to be optimized away, and then later
    // the inline method in the GetNamedExp class can get called.
    this.type = Compilation.typeProcedure;
    this.kind = kind;
    return this;
  }

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            Declaration decl, boolean argsInlined)
  {
    if (! argsInlined)
      exp.walkArgs(walker);
    Expression[] pargs = getArgs();
    Expression context = pargs[0];
    Expression[] args = exp.getArgs();
    Expression[] xargs;
    switch (kind)
      {
      case 'M':
        decl = invokeDecl;
        xargs = new Expression[args.length+2];
        xargs[0] = pargs[0];
        xargs[1] = pargs[1];
        System.arraycopy(args, 0, xargs, 2, args.length);
        break;
      case 'N': // new
        decl = makeDecl;
        xargs = new Expression[args.length+1];
        System.arraycopy(args, 0, xargs, 1, args.length);
        xargs[0] = context;
        break;
      case 'I': // instance-of
        decl = instanceOfDecl;
        xargs = new Expression[args.length+1];
        System.arraycopy(args, 1, xargs, 2, args.length-1);
        xargs[0] = args[0];
        xargs[1] = context;
        break;
      case 'C': // cast
        decl = castDecl;
        xargs = new Expression[args.length+1];
        System.arraycopy(args, 1, xargs, 2, args.length-1);
        xargs[0] = context;
        xargs[1] = args[0];
        break;
      case 'S': // invoke-static
        decl = invokeStaticDecl;
        xargs = new Expression[args.length+2];
        xargs[0] = context;
        xargs[1] = pargs[1];
        System.arraycopy(args, 0, xargs, 2, args.length);
        break;
      default:
        return exp;
      }
    ApplyExp result = new ApplyExp(new ReferenceExp(decl), xargs);
    result.setLine(exp);
    return walker.walkApplyOnly(result);
  }

  public boolean side_effects ()
  {
    // The actual GetNamedExp that returns a method reference doesn't
    // have side-effects - though applying tha result does.
    if (kind == 'S' || kind == 'N' || kind == 'C' || kind == 'I')
      return false;
    if (kind == 'M')
      return getArgs()[0].side_effects();
    return true;
  }

  static final Declaration fieldDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.reflect.SlotGet", "field");

  static final Declaration staticFieldDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.reflect.SlotGet", "staticField");

  static final Declaration makeDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.reflect.Invoke", "make");

  static final Declaration invokeDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.reflect.Invoke", "invoke");

  static final Declaration invokeStaticDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.reflect.Invoke", "invokeStatic");

  static final Declaration instanceOfDecl
  = Declaration.getDeclarationFromStatic("kawa.standard.Scheme", "instanceOf");

  static final Declaration castDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.functions.Convert", "as");
}

class NamedPart extends ProcedureN
  implements HasSetter, Externalizable, CanInline
{
  Object container;
  Object member;
  char kind;
  MethodProc methods;

  public NamedPart(Object container, Object member, char kind)
  {
    this.container = container;
    this.member = member;
    this.kind = kind;
  }

  public NamedPart (Object container, String mname, char kind,
                    MethodProc methods)
  {
    this.container = container;
    this.methods = methods;
    this.member = mname;
    this.kind = kind;
  }

  public int numArgs()
  {
    if (kind == 'I' || kind == 'C')
      return 0x1001;
    if (kind == 'D')
      return 0x1000;
    return 0xfffff000;
  }

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
    Expression[] args = exp.getArgs();
    switch (kind)
      {
      case 'D':
        String fname = member.toString().substring(1);
        Expression[] xargs = new Expression[2];
        xargs[1] = QuoteExp.getInstance(fname);
        SlotGet proc;
        if (args.length > 0)
          {
            xargs[0] = Convert.makeCoercion(args[0], new QuoteExp(container));
            proc = SlotGet.field;
          }
        else
          {
            xargs[0] = QuoteExp.getInstance(container);
            proc = SlotGet.staticField;
          }
        ApplyExp aexp = new ApplyExp(proc, xargs);
        aexp.setLine(exp);
        return walker.walkApplyOnly(aexp);
      }
    return exp;
  }

  public void apply (CallContext ctx) throws Throwable
  {
    apply(ctx.getArgs(), ctx);
  }

  public void apply (Object[] args, CallContext ctx) throws Throwable
  {
    // Optimization, so that output from the
    // method is sent directly to ctx.consumer, rather than reified.
    if (kind == 'S')
      methods.checkN(args, ctx);
    else if (kind=='M')
      {
        int nargs = args.length;
        Object[] xargs = new Object[nargs+1];
        xargs[0] = container;
        System.arraycopy(args, 0, xargs, 1, nargs);
        methods.checkN(xargs, ctx);
      }
    else
      ctx.writeValue(this.applyN(args));
  }

  public Object applyN (Object[] args)
    throws Throwable
  {
    Object[] xargs;

    switch (kind)
      {
      case 'I':
        return kawa.standard.Scheme.instanceOf.apply2(args[0], container);
      case 'C':
        return gnu.kawa.functions.Convert.as.apply2(container, args[0]);
      case 'N':
        xargs = new Object[args.length+1];
        xargs[0] = container;
        System.arraycopy(args, 0, xargs, 1, args.length);
        return Invoke.make.applyN(xargs);
      case 'S':
        return methods.applyN(args);
      case 'M':
        xargs = new Object[args.length+1];
        xargs[0] = container;
        System.arraycopy(args, 0, xargs, 1, args.length);
        return methods.applyN(xargs);
      case 'D':
        String fname = member.toString().substring(1);
        if (args.length == 0)
          return SlotGet.staticField((ClassType) container, fname);
        else
          return SlotGet.field(((Type) container).coerceFromObject(args[0]), fname);
      }
    throw new Error("unknown part "+member+" in "+container);
  }

  public Procedure getSetter()
  {
    if (kind == 'D')
      return new NamedPartSetter(this);
    else
      throw new RuntimeException("procedure '"+getName()+ "' has no setter");
  }

  public void set0 (Object value) throws Throwable
  {
    switch (kind)
      {
      case 'D':
        String fname = member.toString().substring(1);
        SlotSet.setStaticField((ClassType) container, fname, value);
        return;
      default:
        throw new Error("invalid setter for "+this);
      }
  }

  public void set1 (Object object, Object value) throws Throwable
  {
    switch (kind)
      {
      case 'D':
        String fname = member.toString().substring(1);
        object = ((Type) container).coerceFromObject(object);
        SlotSet.setField(object, fname, value);
        return;
      default:
        throw new Error("invalid setter for "+this);
      }
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(container);
    out.writeObject(member);
    out.writeChar(kind);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    kind = in.readChar();
    container = (Procedure) in.readObject();
    member = (Procedure) in.readObject();
  }
}

class NamedPartSetter extends gnu.mapping.Setter
  implements Externalizable, CanInline
{
  public NamedPartSetter (NamedPart getter)
  {
    super(getter);
  }

  public int numArgs()
  {
    if (((NamedPart) getter).kind == 'D')
      return 0x2001;
    return 0xfffff000;
  }

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
    NamedPart get = (NamedPart) this.getter;
    if (get.kind == 'D')
      {
        Expression[] xargs = new Expression[3];
        xargs[1] = QuoteExp.getInstance(get.member.toString().substring(1));
        xargs[2] = exp.getArgs()[0];
        SlotSet proc;
        if (exp.getArgCount() == 1)
          {
            xargs[0] = QuoteExp.getInstance(get.container);
            proc = SlotSet.set$Mnstatic$Mnfield$Ex;
          }
        else if (exp.getArgCount() == 2)
          {
            xargs[0]
              = Convert.makeCoercion(exp.getArgs()[0], new QuoteExp(get.container));
            proc = SlotSet.set$Mnfield$Ex;
          }
        else
          return exp;
        ApplyExp aexp = new ApplyExp(proc, xargs);
        aexp.setLine(exp);
        return walker.walkApplyOnly(aexp);
      }
    return exp;
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(getter);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    getter = (Procedure) in.readObject();
  }
}
