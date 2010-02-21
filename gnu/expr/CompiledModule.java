// Copyright (c) 2010  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.expr;
import gnu.mapping.*;

/** Representation of an interactive module after parsing and compiling.
 * Also useful for loading a compiled class file.
 */

public class CompiledModule
{
  Language language;
  ModuleExp mexp;
  Object cookie;

  public CompiledModule (ModuleExp mexp, Object cookie, Language language)
  {
    this.mexp = mexp;
    this.cookie = cookie;
    this.language = language;
  }

  public static CompiledModule make (Class clas, Language language)
  {
    return new CompiledModule(null, clas, language);
  }

  /** Evaluate this compile, sending output to {@code ctx.consumer}.
   */
  public void evalModule (Environment env, CallContext ctx)
    throws Throwable
  {
    Language saveLang = Language.getDefaultLanguage();
    Language.setDefaultLanguage(language);
    Environment saveEnv = ctx.getEnvironmentRaw();
    ctx.setEnvironmentRaw(env);
    try
      {
        ModuleExp.evalModule2(env, ctx, language, mexp, cookie);
      }
    finally
      {
	Language.setDefaultLanguage(saveLang);
        ctx.setEnvironmentRaw(saveEnv);
      }
  }

  /** Evaluate this compile, yielding a result value.
   */ 
 public Object evalToResultValue (Environment env, CallContext ctx)
    throws Throwable
  {
    int oldIndex = ctx.startFromContext();
    try
      {
        evalModule(env, ctx);
        return ctx.getFromContext(oldIndex);
      }
    catch (Throwable ex)
      { 
        ctx.cleanupFromContext(oldIndex);
        throw ex;
      }
  }
}
