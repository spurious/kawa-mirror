package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.ClassType;

public class ModuleInfo
{
  static final String INFO_KEY = new String("(module-info)");

  String className;

  Object instance;

  ModuleExp exp;

  public synchronized ModuleExp getModuleExp ()
  {
    ModuleExp m = exp;
    if (m == null)
      {
        ClassType ctype
          = (instance == null ? ClassType.make(className)
             : (ClassType) ClassType.make(instance.getClass()));
        m = ModuleExp.make(ctype);
        exp = m;
      }
    return m;
  }

  public static ModuleInfo find (String className)
  {
    Environment env = Environment.getCurrent();
    Symbol sym = Namespace.EmptyNamespace.getSymbol(className);
    synchronized (env)
      {
	Location loc = env.getLocation(sym, INFO_KEY, true);
	ModuleInfo info = (ModuleInfo) loc.get(null);
	if (info == null)
	  {
	    info = new ModuleInfo();
	    info.className = className;
	    loc.set(info);
	  }
	return info;
      }
  }

  public static void register (Object instance)
  {
    String cname = instance.getClass().getName();
    ModuleInfo info = find(cname);
    info.instance = instance;
  }

  public synchronized Object getInstance ()
  {
    Object inst = instance;
    if (inst == null)
      {
	Class clas;
	String cname = className;
	try
	  {
	    clas = Class.forName(cname);
	  }
	catch (java.lang.ClassNotFoundException ex)
	  {
	    throw new WrappedException("cannot find module " + cname, ex);
	  }

	try
	  {
	    try
	      {
		inst = clas.getDeclaredField("$instance").get(null);
	      }
	    catch (NoSuchFieldException ex)
	      {
		// Not a static module - create a new instance.
		inst = clas.newInstance();
	      }
	  }
	catch (Throwable ex)
	  {
	    throw new WrappedException
	      ("exception while initializing module " + cname, ex);
	  }
	instance = inst;
      }
    return inst;
  }
}
