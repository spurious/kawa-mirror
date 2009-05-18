package gnu.expr;
import javax.script.*;
import java.util.*;
import gnu.mapping.*;
import gnu.kawa.util.WeakIdentityHashMap;

/** Abstract implementation of ScriptEngineFactory for any Kawa Language. */

public abstract class AbstractScriptEngineFactory implements ScriptEngineFactory
{
  Language language;
  private List<String> extensions;
  protected List<String> names;
  private static List<String> noMimeTypes;

  WeakIdentityHashMap<Bindings, Environment> contextMap =
    new WeakIdentityHashMap<Bindings, Environment>();

  void setEnvironment(Bindings bindings, Environment env)
  {
    contextMap.put(bindings, env);
  }

  Environment getEnvironment(ScriptContext ctx)
  {
    return getEnvironment(ctx.getBindings(ScriptContext.ENGINE_SCOPE));
  }

  Environment getEnvironment(Bindings bindings)
  {
    Environment env = contextMap.get(bindings);
    if (env == null)
      {
        throw new UnsupportedOperationException("Bindings must be created using createBindings");
      }
    return env;
  }

  protected AbstractScriptEngineFactory(Language language)
  {
    this.language = language;
  }

  public String getEngineName ()
  {
    return "Kawa-"+language.getName();
  }

  public String getEngineVersion ()
  {
    return kawa.Version.getVersion();
  }

  public String getLanguageVersion ()
  {
    return kawa.Version.getVersion();
  }

  public String getLanguageName ()
  {
    return language.getName();
  }

  public List<String> getExtensions ()
  {
    if (extensions == null)
      {
        ArrayList<String> exts = new ArrayList<String>(1);
        String[][] langs = Language.getLanguages();
        for (int i = 0; i < langs.length;  i++)
          {
            String[] lang = langs[i];
            if (lang == null)
              continue;
            int n = lang.length - 1;
            String langClass = lang[n];
            if (! language.getName().equals(langClass))
              continue;
            for (int j = 1; j < n;  j++)
              {
                String ext = lang[j];
                if (ext != null && ext.charAt(0) == '.')
                  exts.add(ext.substring(1));
              }
          }
        extensions = Collections.unmodifiableList(exts);
      }
    return extensions;
  }

  public List<String> getMimeTypes ()
  {
    if (noMimeTypes == null)
      noMimeTypes = Collections.unmodifiableList(new ArrayList<String>(0));
    return noMimeTypes;
  }

  public List<String> getNames()
  {
    if (names == null)
      {
        ArrayList<String> n = new ArrayList<String>(3);
        getNames(n);
        names = Collections.unmodifiableList(n);
      }
    return names;
  }

  protected void getNames (List<String> names)
  {
    names.add(language.getName());
  }

  public String getMethodCallSyntax(String obj, String m, String... args)
  {
    throw new UnsupportedOperationException(getClass().getName()+".getMethodCalSyntax not supported");
  }

  public String getOutputStatement(String toDisplay)
  {
    throw new UnsupportedOperationException(getClass().getName()+".getOutputStatement not supported");
  }

  public String getProgram(String... statements)
  {
    throw new UnsupportedOperationException(getClass().getName()+".getProgram not supported");
  }

  public javax.script.ScriptEngine getScriptEngine()
  {
    return new KawaScriptEngine(this);
  }

  public String getParameter(String key)
  {
    if (key.equals(ScriptEngine.ENGINE))
      return getEngineName();
    if (key.equals(ScriptEngine.ENGINE_VERSION))
      return getEngineVersion();
    if (key.equals(ScriptEngine.NAME))
      return getEngineName();
    if (key.equals(ScriptEngine.LANGUAGE))
      return getLanguageName();
    if (key.equals(ScriptEngine.LANGUAGE_VERSION))
      return getLanguageVersion();
    if (key.equals("THREADING"))
      return "MULTITHREADED";
    return null;
  } 

}
