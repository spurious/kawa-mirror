package gnu.expr;
import java.net.*;
import gnu.bytecode.ClassType;
import gnu.text.*;

/** A database of known modules as represented by {@link ModuleInfo}..
 * Current there is only a single global instanceof {@code ModuleManager};
 * in the future each different "applications" may have their own.
 */

public class ModuleManager
{
  public ClassLoader defaultClassLoader = ClassLoader.getSystemClassLoader();

  private String compilationDirectory = "";
  public void setCompilationDirectory (String path)
  {
    if (path == null)
      path = "";
    int plen = path.length();
    if (plen > 0)
      {
        char sep = java.io.File.separatorChar; // Or '/' if path is a URL??
        if (path.charAt(plen - 1) != sep)
          path = path + sep;
      }
    compilationDirectory = path;
  }
  public String getCompilationDirectory () { return compilationDirectory; }

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

  private ModuleInfo searchWithAbsSourcePath (String sourcePath)
  {
    for (ModuleInfo info = modules;  info != null;  info = info.next)
      if (sourcePath.equals(info.getSourceAbsPathname()))
        return info;
    return null;
  }

  public synchronized ModuleInfo findWithSourcePath (String sourcePath)
  {
    Path sourceAbsPath = ModuleInfo.absPath(sourcePath);
    String sourceAbsPathname = sourceAbsPath.toString();
    ModuleInfo info = searchWithAbsSourcePath(sourceAbsPathname);
    if (info == null)
      {
        info = new ModuleInfo();
        info.sourcePath = sourcePath;
        info.sourceAbsPath = sourceAbsPath;
        info.sourceAbsPathname = sourceAbsPathname;
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
    // Unclear what is the right thing to do if we have an existing module
    // with the same source or class name.  One case is when we're explicitly
    // compiling a source file and (in XQuery) importing (other) modules in the
    // same namespace.  In that case the file we're compiling should take
    // precedence over old data in the packages's existing $ModulesMap$ class.
    if (searchWithClassName(moduleClass) != null)
      return;
    Path sourcePath = Path.valueOf(moduleSource);
    Path sourceAbsPath = sourcePath.getCanonical();
    String sourceAbsPathname = sourceAbsPath.toString();
    if (searchWithAbsSourcePath(sourceAbsPathname) != null)
      return;
    ModuleInfo info = new ModuleInfo();
    if (sourcePath.isAbsolute())
      {
        info.sourceAbsPath = sourcePath;
        info.sourceAbsPathname = sourceAbsPathname;
      }
    else
      {
        // Resolve moduleSource against moduleClass path.
        try
          {
            // See comment in loadPackageInfo.
            Class setClass = this.packageInfoChain.getClass();
            String setClassName = setClass.getName().replace('.', '/')+".class";
            java.net.URL setClassURL
              = setClass.getClassLoader().getResource(setClassName);
            sourceAbsPath = URLPath.valueOf(setClassURL).resolve(moduleSource);
            info.sourceAbsPath = sourceAbsPath;
            info.sourceAbsPathname = sourceAbsPath.toString();
          }
        catch (Throwable ex)
          {
            return;
          }
      }
    info.className = moduleClass;
    info.sourcePath = moduleSource;
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

    instance.next = this.packageInfoChain;
    // The ModuleInfo.register method depends on packageInfoChain being
    // the current ModuleSet.  Bit of a kludge.
    this.packageInfoChain = instance;
    instance.register(this);
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
