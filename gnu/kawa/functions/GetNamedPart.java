package gnu.kawa.functions;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.kawa.reflect.*;
import gnu.expr.Compilation;
import gnu.expr.Language;
import java.io.*;

/** Procedure to get the value of a named component of an object. */

public class GetNamedPart extends Procedure2 implements HasSetter
{
  public static final GetNamedPart getNamedPart = new GetNamedPart();
  static {
      getNamedPart.setProperty(Procedure.validateApplyKey,
                       "gnu.kawa.functions.CompileNamedPart:validateGetNamedPart");
  }


  /** {@code PREFIX:<>} is equivalent to the {@code ClassType} bound to {@code PREFIX}. */
  public static final String CLASSTYPE_FOR = "<>";

  /** Pseudo-method-name for the cast operation. */
  public static final String CAST_METHOD_NAME = "@";

  /** Pseudo-method-name for class-membership-test (instanceof) operation. */
  public static final String INSTANCEOF_METHOD_NAME = "instance?";

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
      container = Type.make((Class) container);
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

class NamedPart extends ProcedureN
  implements HasSetter, Externalizable
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
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.functions.CompileNamedPart:validateNamedPart");
  }

  public NamedPart (Object container, String mname, char kind,
                    MethodProc methods)
  {
    this(container, mname, kind);
    this.methods = methods;
  }

  public int numArgs()
  {
    if (kind == 'I' || kind == 'C')
      return 0x1001;
    if (kind == 'D')
      return 0x1000;
    return 0xfffff000;
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
  implements Externalizable
{
  public NamedPartSetter (NamedPart getter)
  {
    super(getter);
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.functions.CompileNamedPart:validateNamedPartSetter");
  }

  public int numArgs()
  {
    if (((NamedPart) getter).kind == 'D')
      return 0x2001;
    return 0xfffff000;
  }

  Procedure getGetter() { return getter; }

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
