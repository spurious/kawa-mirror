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

  /** PREFIX:<> is equivalent to the ClassType bound to PREFIX. */
  public static final String CLASSTYPE_FOR = "<>";

  /** Pseudo-method-name for the cast operation. */
  public static final String CAST_METHOD_NAME = "@";

  /** Pseudo-method-name for class-membership-test (instanceof) operation. */
  public static final String INSTANCEOF_METHOD_NAME = "instance?";

  public static String combineName (Expression part1, Expression part2)
  {
    String name1;
    Object name2;
    if (part2 instanceof QuoteExp
        && (name2 = ((QuoteExp) part2).getValue()) instanceof String
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
    if (combinedName != null)
      {
        Translator tr = (Translator) Compilation.getCurrent();
        Declaration decl = tr.lexical.lookup(combinedName, false/*FIXME*/);
        if (! Declaration.isUnknown(decl))
          return new ReferenceExp(decl);

        Environment env = Environment.getCurrent();
        Symbol symbol = env.defaultNamespace().lookup(combinedName);
        Object property = null; // FIXME?
        if (symbol != null && env.isBound(symbol, property))
          return new ReferenceExp(combinedName);
      }
    if (clas instanceof ReferenceExp
        && (rexp = (ReferenceExp) clas).isUnknown())
      {
        String name = rexp.getName();
        try
          {
            /* #ifdef JAVA2 */
            Class cl = Class.forName(name, false,
                                     clas.getClass().getClassLoader());
            /* #else */
            // Class cl = Class.forName(name);
            /* #endif */
            clas = QuoteExp.getInstance(Type.make(cl));
          }
        catch (Throwable ex)
          {
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

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression[] args = exp.getArgs();
    if (args.length != 2 || ! (args[1] instanceof QuoteExp)
        || ! (exp instanceof GetNamedExp))
      return exp;
    Expression context = args[0];
    if (context instanceof ReferenceExp)
      {
        ReferenceExp rexp = (ReferenceExp) context;
        if ("*".equals(rexp.getName()))
          return GetNamedInstancePart.makeExp(args[1]);
      }

    String mname = ((QuoteExp) args[1]).getValue().toString();
    Type type = context.getType();
    boolean isInstanceOperator = context == QuoteExp.nullExp;
    Compilation comp = walker.getCompilation();
    Language language = comp.getLanguage();
    Type typeval = language.getTypeFor(context);
    ClassType caller = comp == null ? null
      : comp.curClass != null ? comp.curClass
      : comp.mainClass;
    GetNamedExp nexp = (GetNamedExp) exp;

    if (typeval instanceof Type)
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
    if (typeval instanceof ClassType)
      {
        if (mname.length() > 1 && mname.charAt(0) == '.')
          {
            // The following would also work:
            // return nexp.setProcedureKind('D');
            // However, it makes optimzing the 'setter' case harder.
            return new QuoteExp(new NamedPart(typeval, mname, 'D'));
          }
        PrimProcedure[] methods
          = ClassMethods.getMethods((ClassType) typeval,
                                    Compilation.mangleName(mname),
                                    0, 0, false, caller, language);
        if (methods != null && methods.length > 0)
          {
            nexp.methods = methods;
            return nexp.setProcedureKind('S');
          }
        return new ApplyExp(SlotGet.staticField, args);
                            
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
        || type.isSubtype(Type.java_lang_Class_type))
      // The container evaluates to a class (so we should look for a static
      // field or method), but we don't know which class at compile-time.
      // However, we should still optimize it somewhat, above.  FIXME.
      return exp;

    if (type instanceof ObjectType)
      {
        ClassType ctype
          = type instanceof ClassType ? (ClassType) type : Type.pointer_type;
        PrimProcedure[] methods
          = ClassMethods.getMethods(ctype, Compilation.mangleName(mname),
                                    Access.STATIC, 0, false, caller, language);
        if (methods != null && methods.length > 0)
          {
            nexp.methods = methods;
            return nexp.setProcedureKind('M');
          }
        Object part = SlotGet.getField(ctype, mname, caller);
        if (part != null
            || (mname.equals("length") && type instanceof ArrayType))
          {
            // FIXME: future kludge to avoid re-doing SlotGet.getField.
            // args = new Expression[] { context, new QuoteExp(part) });
            return new ApplyExp(SlotGet.field, args);
          }
      }

    return exp;
  }

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

  public static Object getNamedPart (Object container, Symbol part)
    throws Throwable
  {
    /*
    if (container implements HasNamedParts)
      return ((HasNamedParts) container).getNamedPart(part);
    */
    String name = part.getName();
    if (container instanceof Namespace)
      {
        Namespace ns = (Namespace) container;
        String uri = ns.getName();
        if (uri.startsWith("class:"))
          container = ClassType.make(uri.substring(6));
        else
          return Environment.getCurrent().get(ns.getSymbol(name));
      }
    if (container instanceof Class)
      container = (ClassType) Type.make((Class) container);
    if (container instanceof Type)
      {
        if (name.equals(CLASSTYPE_FOR))
          return container;

        if (container instanceof ObjectType)
          {
            if (name.equals(INSTANCEOF_METHOD_NAME))
              return new NamedPart(container, part, 'I');
            if (name.equals(CAST_METHOD_NAME))
              return new NamedPart(container, part, 'C');
            if (name.equals("new"))
              return new NamedPart(container, part, 'N');
            if (name.equals(".length")
                || (name.length() > 1 && name.charAt(0) == '.'
                    && container instanceof ClassType))
              return new NamedPart(container, part, 'D');
          }

        if (container instanceof ClassType)
          {
            try
              {
                return gnu.kawa.reflect.SlotGet.staticField(container, part.toString());
              }
            catch (Throwable ex)
              {
                // FIXME!
              }
            return ClassMethods.apply(ClassMethods.classMethods, container, part, null, null, 0, 0);
          }
      }

    try
      {
        return gnu.kawa.reflect.SlotGet.field(container, part.toString());
      }
    catch (Throwable ex)
      {
        // FIXME!
      }
    MethodProc methods = ClassMethods.apply((ClassType) ClassType.make(container.getClass()),
                                            name,
                                            (Type) null, (Type[]) null,
                                            0, Access.STATIC,
                                            Language.getDefaultLanguage());
    if (methods != null)
      return new NamedPart(container, name, 'M', methods);
    throw new RuntimeException("no part '"+part+"' in "+container+" m:"+methods);
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

  public Expression inline (ApplyExp exp, InlineCalls walker, Declaration decl)
  {
    Expression[] pargs = getArgs();
    Expression context = pargs[0];
    Object name;
    if (! (pargs[1] instanceof QuoteExp)
        || ! ((name = ((QuoteExp) pargs[1]).getValue()) instanceof String))
      return exp;
    Expression[] args = exp.getArgs();
    String mname = name.toString();
    // It would be more efficient to check the kind field,
    // but there may be case where kind hasn't been set.
    boolean isInstanceOf = mname.equals(GetNamedPart.INSTANCEOF_METHOD_NAME);
    boolean isCast = mname.equals(GetNamedPart.CAST_METHOD_NAME);
    boolean isField = mname.length() > 1 && mname.charAt(0) == '.';
    boolean isInstance = context == QuoteExp.nullExp;
    boolean isNew = mname.equals("new");
    if (isField && ! isInstance && args.length == 1)
      {
        args = new Expression[] { Convert.makeCoercion(args[0], context) };
        isInstance = true;
      }
    Expression[] xargs
      = new Expression[args.length+(isInstance||isNew||isInstanceOf||isCast?1:2)];
    if (isInstance)
      {
        decl = isField ? fieldDecl : invokeDecl;
        System.arraycopy(args, 1, xargs, 2, args.length-1);
        xargs[0] = args[0];
      }
    else if (isNew)
      {
        decl = makeDecl;
        System.arraycopy(args, 0, xargs, 1, args.length);
        xargs[0] = context;
      }
    else if (isInstanceOf)
      {
        decl = instanceOfDecl;
        System.arraycopy(args, 1, xargs, 2, args.length-1);
        xargs[0] = args[0];
        xargs[1] = context;
      }
    else if (isCast)
      {
        decl = castDecl;
        System.arraycopy(args, 1, xargs, 2, args.length-1);
        xargs[0] = context;
        xargs[1] = args[0];
      }
    else
      {
        decl = isField ? staticFieldDecl : invokeStaticDecl;
        System.arraycopy(args, 0, xargs, 2, args.length);
        xargs[0] = context;
      }
    if (! isNew && ! isInstanceOf && ! isCast)
      xargs[1] = new QuoteExp(isField ? mname.substring(1) : mname);
    return walker.walkApplyOnly(new ApplyExp(new ReferenceExp(decl), xargs));
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

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression[] args = exp.getArgs();
    switch (kind)
      {
      case 'D':
        String fname = member.toString().substring(1);
        Expression[] xargs = new Expression[2];
        xargs[1] = QuoteExp.getInstance(fname);
        if (args.length > 0)
          {
            xargs[0] = Convert.makeCoercion(args[0], new QuoteExp(container));
            return new ApplyExp(SlotGet.field, xargs);
          }
        else
          {
            xargs[0] = QuoteExp.getInstance(container);
            return new ApplyExp(SlotGet.staticField, xargs);
          }
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

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    NamedPart get = (NamedPart) this.getter;
    if (get.kind == 'D')
      {
        Expression[] xargs = new Expression[3];
        xargs[1] = QuoteExp.getInstance(get.member.toString().substring(1));
        xargs[2] = exp.getArgs()[0];
        if (exp.getArgCount() == 1)
          {
            xargs[0] = QuoteExp.getInstance(get.container);
            return new ApplyExp(SlotSet.set$Mnstatic$Mnfield$Ex, xargs);
          }
        else if (exp.getArgCount() == 2)
          {
            xargs[0]
              = Convert.makeCoercion(exp.getArgs()[0], new QuoteExp(get.container));
            return new ApplyExp(SlotSet.set$Mnfield$Ex, xargs);
          }
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
