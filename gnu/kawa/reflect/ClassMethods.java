package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.bytecode.ClassType;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.FString;
import java.util.Vector;

public class ClassMethods extends Procedure2
{
  public static final ClassMethods classMethods = new ClassMethods();
  static { classMethods.setName("class-methods"); }

  /** Create a method or generic of the matching methods.
   * @param arg0 a Class, ClassType, or a String, FString or Symbol
   *  that names a class.
   * @param arg1 a method name (a String, FString, or Symbol)
   * Loosely the same as ClassMethodProc.make(arg0, arg1),
   * but with some extar conversions and checks.
   */
  public Object apply2 (Object arg0, Object arg1)
  {
    return apply(this, arg0, arg1, null, null, 0, 0);
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
	     || arg0 instanceof Symbol)
      dtype = ClassType.make(arg0.toString());
    else
      throw new WrongType(thisProc, 0, null);
    if (arg1 instanceof String || arg1 instanceof FString
	|| arg1 instanceof Symbol)
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
   * @param caller if non-null, check that methods are accessible in it.
   * @return an array containing the methods.
   */
  public static PrimProcedure[] getMethods(ClassType dtype, String mname,
                                           int modifiers, int modmask,
                                           ClassType caller, Language language)
  {
    return getMethods(dtype,mname,modifiers,modmask,false, caller, language);
  }

  private static int removeRedundantMethods(Vector methods)
  {
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
    return mlength;
  }
    
  /** Return the methods of a class with the specified name and flag.
   * @return an array containing the methods.
   */
  public static PrimProcedure[] getMethods(ClassType dtype, String mname,
                                           int modifiers, int modmask,
                                           boolean is_special,
                                           ClassType caller,
                                           Language language)
  {
    MethodFilter filter = new MethodFilter(mname, modifiers, modmask, caller);
    boolean named_class_only = is_special || "<init>".equals(mname);
    Vector methods = new Vector();
    dtype.getMethods(filter, named_class_only ? 0 : 2,
		     methods,
		     "-"); // Should be more specific - FIXME

    int mlength = (named_class_only ? methods.size()
		   : removeRedundantMethods(methods));

    PrimProcedure[] result = new PrimProcedure[mlength];
    int count = 0;
    for (int i = mlength;  --i >= 0; )
    {
      Method method = (Method) methods.elementAt(i);
      PrimProcedure pproc = new PrimProcedure(method, is_special, language);
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
    Language language = Language.getDefaultLanguage();
    PrimProcedure[] methods = getMethods(dtype, mname, modifiers, modmask,
					 null, language);
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

  static String checkName(Expression exp, boolean reversible)
  {
    if (exp instanceof QuoteExp)
      {
        Object name = ((QuoteExp) exp).getValue();
	String nam;
        if (name instanceof FString || name instanceof String)
	  nam = name.toString();
	else if (name instanceof Symbol)
	  nam = ((Symbol) name).getName();
	else
	  return null;
	if (Compilation.isValidJavaName(nam))
	  return nam;
	return Compilation.mangleName(nam, reversible);
      }
    return null;
  }
}

class MethodFilter implements gnu.bytecode.Filter
{
  String name;
  int nlen;
  int modifiers;
  int modmask;
  ClassType caller;

  public MethodFilter(String name, int modifiers, int modmask,
		      ClassType caller)
  {
    this.name = name;
    this.nlen = name.length();
    this.modifiers = modifiers;
    this.modmask = modmask;
    this.caller = caller;
  }

  public boolean select(Object value)
  {
    gnu.bytecode.Method method = (gnu.bytecode.Method) value;
    String mname = method.getName();
    int mmods = method.getModifiers();
    if ((mmods & modmask) != modifiers
	|| ! mname.startsWith(name))
      return false;
    int mlen = mname.length();
    char c;
    if (mlen != nlen
	&& (mlen != nlen + 2
	    || mname.charAt(nlen) != '$'
	    || ((c = mname.charAt(nlen+1)) != 'V' && c != 'X'))
	&& (mlen != nlen + 4
	    || ! mname.endsWith("$V$X")))
      return false;
    return caller == null
      || caller.isAccessible(method.getDeclaringClass(), mmods);
  }
}
