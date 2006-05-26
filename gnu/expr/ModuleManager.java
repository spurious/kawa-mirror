package gnu.expr;
import java.net.*;
import gnu.bytecode.ClassType;

public class ModuleManager
{
  static ModuleManager instance = new ModuleManager();

  /** For now assumes a single global ModuleManager.
   * Later, might have multiple managers. */
  public static ModuleManager getInstance() { return instance; }

  /** Chain of all modules managed by this ModuleManager.
   * Linked together by ModuleInfo's next field. */
  ModuleInfo modules;

  public ModuleInfo find (Compilation comp)
  {
    ModuleExp mexp = comp.getModule();
    ClassType ctype = mexp.classFor(comp);
    ModuleInfo info = findWithClassName(ctype.getName());
    info.exp = mexp;
    info.comp = comp;
    comp.minfo = info;
    return info;
  }

  public void add (ModuleInfo info)
  {
    info.next = modules;
    modules = info;
  }

  public ModuleInfo searchWithClassName (String className)
  {
    for (ModuleInfo info = modules;  info != null;  info = info.next)
      if (className.equals(info.className))
        return info;
    return null;
  }

  public synchronized ModuleInfo findWithClassName (String className)
  {
    ModuleInfo info = searchWithClassName(className);
    if (info == null)
      {
        info = new ModuleInfo();
        info.className = className;
        add(info);
      }
    return info;
  }

  public ModuleInfo searchWithSourcePath (String sourcePath)
  {
    for (ModuleInfo info = modules;  info != null;  info = info.next)
      if (sourcePath.equals(info.sourceURL))
        return info;
    return null;
  }

  public synchronized ModuleInfo findWithSourcePath (String sourcePath)
  {
    ModuleInfo info = searchWithSourcePath(sourcePath);
    if (info == null)
      {
        info = new ModuleInfo();
        info.sourceURL = sourcePath;
        add(info);
      }
    return info;
  }

  public ModuleInfo findWithURL (URL url)
  {
    return findWithSourcePath(url.toExternalForm());
  }
}
