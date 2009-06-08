package gnu.expr;
import javax.script.*;
import java.io.*;
import gnu.mapping.*;
import gnu.text.*;

public class KawaScriptEngine extends AbstractScriptEngine
  implements Compilable //, Invocable
{
  AbstractScriptEngineFactory factory;

  public KawaScriptEngine(AbstractScriptEngineFactory factory)
  {
    this.factory = factory;
    context.setBindings(createBindings(), ScriptContext.ENGINE_SCOPE);
    context.setReader(InPort.inDefault());
    context.setWriter(OutPort.outDefault());
    context.setErrorWriter(OutPort.errDefault());
 }

  public AbstractScriptEngineFactory getFactory()
  {
    return factory;
  }

  public Bindings createBindings()
  {
    SimpleEnvironment env = (SimpleEnvironment) factory.language.getNewEnvironment();
    Bindings bindings = new KawaScriptBindings(env);
    factory.setEnvironment(bindings, env);
    return bindings;
  }

  public Object eval(Reader in, ScriptContext context)
    throws ScriptException
  {
    return eval(in instanceof InPort ? (InPort) in : new InPort(in), context);
  }

  public Object eval(String string, ScriptContext context)
    throws ScriptException
  {
    return eval(new CharArrayInPort(string), context);
  }

  public Object eval(InPort in, ScriptContext context)
    throws ScriptException
  {
    KawaCompiledScript compiled = compile(in, context);
    return compiled.eval(context);
  }

  public KawaCompiledScript compile(String string)
    throws ScriptException
  {
    return compile(new CharArrayInPort(string), getContext());
  }

  public KawaCompiledScript compile(Reader in)
    throws ScriptException
  {
    return compile(in instanceof InPort ? (InPort) in : new InPort(in), getContext());
  }

  public KawaCompiledScript compile(InPort port, ScriptContext context)
    throws ScriptException
  {
    SourceMessages messages = new SourceMessages();
    try
      {
        return compile(port, context, messages);
      }
    catch (SyntaxException ex)
      {
        messages = ex.getMessages();  // Presumably a no-op.
        SourceError err = messages.getErrors();
        if (messages.seenErrors())
          {
            while (err.severity == 'w' && err.next != null)
              err = err.next;
          }
        throw new ScriptException(err.message, err.filename,
                                  err.line, err.column);
      }
    catch (Exception ex)
      {
        throw new ScriptException(ex);
      }
  }

  public KawaCompiledScript compile(InPort port, ScriptContext context, SourceMessages messages)
    throws SyntaxException, IOException
  {
    CallContext ctx = CallContext.getInstance();
    Language saveLang = Language.getDefaultLanguage();
    Environment saveEnv = ctx.getEnvironmentRaw();
    Environment env = factory.getEnvironment(context);
    ctx.setEnvironmentRaw(env);
    Language.setDefaultLanguage(factory.language);
    try
      {
        Compilation comp = factory.language.parse(port, messages, Language.PARSE_IMMEDIATE);
        if (messages.seenErrors())
          throw new SyntaxException(messages);
        ModuleExp mexp = comp.getModule();
        mexp.setName("atInteractiveLevel$"
                     + (++ModuleExp.interactiveCounter));
        String filename = (String) get(ScriptEngine.FILENAME);
        java.net.URL url = port.getPath().toURL();
        Writer errorWriter = context.getErrorWriter();
        OutPort errorPort
          = (errorWriter instanceof OutPort ? (OutPort) errorWriter
             : new OutPort(errorWriter));
        Object inst = mexp.evalModule1(env, comp, url, errorPort);
        return new KawaCompiledScript(this, mexp, inst);
      }
    finally
      {
	Language.setDefaultLanguage(saveLang);
        ctx.setEnvironmentRaw(saveEnv);
      }
  }
}

class KawaCompiledScript extends CompiledScript
{
  KawaScriptEngine engine;
  ModuleExp mexp;
  Object cookie;

  public KawaCompiledScript(KawaScriptEngine engine,
                            ModuleExp mexp, Object cookie)
  {
    this.engine = engine;
    this.mexp = mexp;
    this.cookie = cookie;
  }

  public Object eval(ScriptContext context) throws ScriptException
  {
    CallContext ctx = CallContext.getInstance();
    int oldIndex = ctx.startFromContext();
    Language saveLang = Language.getDefaultLanguage();
    Language language = engine.factory.language;
    Language.setDefaultLanguage(language);
    Environment saveEnv = ctx.getEnvironmentRaw();
    Environment env = engine.factory.getEnvironment(context);
    ctx.setEnvironmentRaw(env);
    Writer errWriter = context.getErrorWriter();
    OutPort errPort = errWriter instanceof OutPort ? (OutPort) errWriter
      : new OutPort(errWriter);
    try
      {
        mexp.evalModule2(env, ctx, language, mexp, cookie, errPort);
	return ctx.getFromContext(oldIndex);
      }
    catch (Throwable ex)
      { 
	ctx.cleanupFromContext(oldIndex);
        if (ex instanceof Exception)
          throw new ScriptException((Exception) ex);
        else if (ex instanceof Error)
          throw (Error) ex;
        else
          throw new RuntimeException(ex);
      }
    finally
      {
	Language.setDefaultLanguage(saveLang);
        ctx.setEnvironmentRaw(saveEnv);
      }
  }

  public KawaScriptEngine getEngine()
  {
    return engine;
  }
}
