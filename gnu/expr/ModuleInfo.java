package gnu.expr;
import gnu.mapping.*;

public class ModuleInfo
{
  static final String INFO_KEY = new String("(module-info)");

  String className;

  Object instance;

  boolean needsRun;

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
	    inst = clas.newInstance();
	    if (inst instanceof Runnable)
	      needsRun = true;
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

  public Object getRunInstance ()
  {
    Object inst = getInstance();
    synchronized (this)
      {
	if (needsRun)
	  {
	    needsRun = false;
	    ((Runnable) inst).run();
	  }
      }
    return inst;
  }
}
