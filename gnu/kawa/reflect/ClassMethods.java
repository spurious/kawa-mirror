package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.bytecode.ClassType;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.FString;
import java.util.Vector;

public class ClassMethods extends ProcedureN
{
  public int numArgs() { return 2 | (2 << 12); } // For now.

  public Object applyN (Object[] args)
  {
    return apply(this, args[0], args[1], null, null, 0, 0);
  }

  public static MethodProc apply(Procedure thisProc,
                                 Object arg0, Object arg1,
                                 Type rtype, Type[] atypes,
                                 int modifiers, int modmask)
  {
    ClassType dtype;
    String mname;
    if (arg0 instanceof Class)
      arg0 = Type.make((Class) arg0);
    if (arg0 instanceof ClassType)
      dtype = (ClassType) arg0;
    else if (arg0 instanceof String || arg0 instanceof FString
	     || arg0 instanceof Binding)
      dtype = ClassType.make(arg0.toString());
    else
      throw new WrongType(thisProc, 0, null);
    if (arg1 instanceof String || arg1 instanceof FString
	|| arg1 instanceof Binding)
      mname = arg1.toString();
    else
      throw new WrongType(thisProc, 1, null);
    if (! ("<init>".equals(mname)))
      mname = Compilation.mangleName(mname);
    MethodProc result = apply(dtype, mname, rtype, atypes, modifiers, modmask);
    if (result == null)
      throw new RuntimeException("no applicable method named `"+mname+"' in "
                                 +dtype.getName());
    return result;
  }

  /** Return the methods of a class with the specified name and flag.
   * @return an array containing the methods.
   */
  public static PrimProcedure[] getMethods(ClassType dtype, String mname,
                                           int modifiers, int modmask,
                                           Interpreter interpreter)
  {
    MethodFilter filter = new MethodFilter(mname, modifiers, modmask);
    Vector methods = new Vector();
    dtype.getMethods(filter, "<init>".equals(mname) ? 0 : 2,
		     methods,
		     "-"); // Should be more specific - FIXME

    // Remove over-ridden methods.
    int mlength = methods.size();
  loopi:
    for (int i = 1;  i < mlength; )
    {
      Method method1 = (Method) methods.elementAt(i);
      ClassType class1 = method1.getDeclaringClass();
      Type[] types1 = method1.getParameterTypes();
      int tlen = types1.length;
      for (int j = 0;  j < i;  j++)
	{
	  Method method2 = (Method) methods.elementAt(j);
	  Type[] types2 = method2.getParameterTypes();
	  if (tlen != types2.length)
	    continue;
	  int k;
	  for (k = tlen;  --k >= 0;  )
	    {
	      if (types1[k] != types2[k])
		break;
	    }
	  if (k >= 0)
	    continue;
	  if (class1.isSubtype(method2.getDeclaringClass()))
	    methods.setElementAt(method1, j);
	  methods.setElementAt(methods.elementAt(mlength - 1), i);
	  mlength--;
	  // Re-do current i, since methods[i] replaced.
	  continue loopi;
	}
      i++;
    }

    PrimProcedure[] result = new PrimProcedure[mlength];
    int count = 0;
    for (int i = mlength;  --i >= 0; )
    {
      Method method = (Method) methods.elementAt(i);
      if ((method.getModifiers() & modmask) != modifiers)
	continue;
      PrimProcedure pproc = new PrimProcedure(method, interpreter);
      result[count++] = pproc;
    }
    return result;
  }

  /** Re-order the methods such that the ones that are definite
   * applicable (all argtypes is subset of parameter type) are first;
   * those possibly applicable next (argtype overlaps parameter types);
   * and ending with those definitely not applicable (some argtype does
   * overlap its parameter type).
   * @return ((number of definitely applicable methods) << 32
   *          + (number of possibly applicable methods.
   */
  public static long selectApplicable(PrimProcedure[] methods,
                                      Type[] atypes)
  {
    int limit = methods.length;
    int numDefApplicable = 0;
    int numPosApplicable = 0;
    for (int i = 0;  i < limit;  )
      {
        int code = methods[i].isApplicable(atypes);
        if (code < 0)
          { // Definitely not applicable.
            // swap(methods[limit-1], methods[i]):
            PrimProcedure tmp = methods[limit-1];
            methods[limit-1] = methods[i];
            methods[i] = tmp;
            limit--;
          }
        else if (code > 0)
          { // Definitely applicable.
            // swap(methods[numDefApplicable], methods[i]):
            PrimProcedure tmp = methods[numDefApplicable];
            methods[numDefApplicable] = methods[i];
            methods[i] = tmp;
            numDefApplicable++;
            i++;
          }
        else
          { // Possibly applicable.
            numPosApplicable++;
            i++;
          }
      }
    return (((long) numDefApplicable) << 32) + (long) numPosApplicable;
  }

  public static MethodProc apply(ClassType dtype, String mname,
                                 Type rtype, Type[] atypes,
                                 int modifiers, int modmask)
  {
    Interpreter interpreter = Interpreter.defaultInterpreter; // FIXME
    PrimProcedure[] methods = getMethods(dtype, mname,
                                         modifiers, modmask, interpreter);
    GenericProc gproc = null;
    PrimProcedure pproc = null;
    for (int i = 0;  i < methods.length;  i++)
      {
        PrimProcedure cur = methods[i];
        if (atypes != null)
          {
            int applicable = cur.isApplicable(atypes);
            if (applicable == -1)
              continue;
            if (pproc != null)
              {
                MethodProc best = MethodProc.mostSpecific(pproc, cur);
                if (best != null)
                  {
                    if (cur == best)
                      pproc = cur;
                    continue;
                  }
              }
          }
        if (pproc != null && gproc == null)
          {
            gproc = new GenericProc();
            gproc.add(pproc);
          }
        pproc = cur;
        if (gproc != null)
          gproc.add(pproc);
      }
    if (gproc != null)
      {
        gproc.setName(dtype.getName()+"."+mname);
        return gproc;
      }
    return pproc;
  }

  static String checkName(Expression exp)
  {
    if (exp instanceof QuoteExp)
      {
        Object name = ((QuoteExp) exp).getValue();
        if (name instanceof FString || name instanceof String
	    || name instanceof Binding)
          return Compilation.mangleNameIfNeeded(name.toString());
      }
    return null;
  }
}

class MethodFilter implements gnu.bytecode.Filter
{
  String name;
  String nameV;
  int modifiers;
  int modmask;

  public MethodFilter(String name, int modifiers, int modmask)
  {
    this.name = name;
    this.nameV = name+"$V";
    this.modifiers = modifiers;
    this.modmask = modmask;
  }

  public boolean select(Object value)
  {
    gnu.bytecode.Method method = (gnu.bytecode.Method) value;
    String mname = method.getName();
    return ((method.getModifiers() & modmask) == modifiers)
      && (mname.equals(name) || mname.equals(nameV));
  }
}
