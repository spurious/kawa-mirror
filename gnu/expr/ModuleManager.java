package gnu.expr;
import java.net.*;

public class ModuleManager
{
  static ModuleManager instance = new ModuleManager();

  /** For now assumes a single global ModuleManager.
   * Later, might have multiple managers.
   * We may also want to have managers that share classes but not instances. */
  public static ModuleManager getInstance() { return instance; }

  ModuleInfo modules;

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
        info.next = modules;
        modules = info;
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
        info.next = modules;
        modules = info;
      }
    return info;
  }

  public ModuleInfo findWithURL (URL url)
  {
    return findWithSourcePath(url.toExternalForm());
  }
}
