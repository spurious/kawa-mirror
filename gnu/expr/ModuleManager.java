package gnu.expr;
import java.net.*;
import gnu.bytecode.ClassType;

/** A database of known modules as represented by {@link ModuleInfo}..
 * Current there is only a single global instanceof {@code ModuleManager};
 * in the future each different "applications" may have their own.
 */

public class ModuleManager
{
  public ClassLoader defaultClassLoader = ClassLoader.getSystemClassLoader();

  static ModuleManager instance = new ModuleManager();

  /** For now assumes a single global ModuleManager.
   * Later, might have multiple managers. */
  public static ModuleManager getInstance() { return instance; }

  public static final long LAST_MODIFIED_CACHE_TIME = 1000;
  /** Number millseconds before we re-check file's modified time. */
  public long lastModifiedCacheTime = LAST_MODIFIED_CACHE_TIME;

  /** Chain of all modules managed by this ModuleManager.
   * Linked together by ModuleInfo's next field. */
  ModuleInfo modules;
  public ModuleInfo firstModule () { return modules; }

  public ModuleInfo find (Compilation comp)
  {
    ModuleExp mexp = comp.getModule();
    ClassType ctype = mexp.classFor(comp);
    ModuleInfo info = findWithClassName(ctype.getName());
    info.setCompilation(comp);
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

  /** Called by compiler-generated code.
   * The compiler generates in each package a class that extends
   * {@link ModuleSet}, and that contains a
   * {@link ModuleSet#register(ModuleManager)} method that calls
   * back to this method.  This method then registers the specified module.
   */
  public void register (String moduleClass, String moduleSource, String moduleUri)
  {
    if (searchWithClassName(moduleClass) != null)
      return;
    if (searchWithSourcePath(moduleSource) != null)
      return;
    ModuleInfo info = new ModuleInfo();
    info.className = moduleClass;
    info.sourceURL = moduleSource;
    info.uri = moduleUri;
    add(info);
  }

  /** List of {@link ModuleSet}s registered with this {@code ModuleManager}. */
  ModuleSet packageInfoChain;

  /** Search for and if needed load the {@link ModuleSet} for a package.
   */
  public synchronized void loadPackageInfo (String packageName)
    throws ClassNotFoundException, InstantiationException, IllegalAccessException
  {
    String moduleSetClassName = packageName + "." + ModuleSet.MODULES_MAP;

    for (ModuleSet set = packageInfoChain;  set != null;  set = set.next)
      {
        String setName = set.getClass().getName();
        if (setName.equals(moduleSetClassName))
          continue;
      }
    Class setClass = Class.forName(moduleSetClassName);
    ModuleSet instance = (ModuleSet) setClass.newInstance();

    instance.register(this);
    instance.next = this.packageInfoChain;
    this.packageInfoChain = instance;
  }

  /** Reset the set of known modules. */
  public void clear ()
  {
    // Clear modules and packageIndoChain lists.
    // We also clean the 'next' fields, to avoid leaks if
    // somethings happens to be pointing at a ModuleInto or ModuleSet.
    // This may be overkill.
    ModuleSet set = packageInfoChain;
    while (set != null)
      {
        ModuleSet next = set.next;
        set.next = null;
        set = next;
      }
    packageInfoChain = null;

    ModuleInfo module = modules;
    while (module != null)
      {
        ModuleInfo next = module.next;
        module.next = null;
        module = next;
      }
    modules = null;
  }
}
