// Copyright (c) 2002, 2003, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;

/**
 * Contains various language-dependent methods.
 * Also contains "global" state about the executation environment,
 * such as the global Environment.  There can be multiple Interpreters
 * associated with different threads, representing mutiple top-levels.
 * (However, this functionality is incomplete.)
 */

public abstract class Language extends Interpreter
{
  public static void setDefaults (Language lang)
  {
    Language.setDefaultLanguage(lang);
    Environment env = lang.getEnvironment();
    Environment.setCurrent(env);
    Environment.setGlobal(env);
  }

  public Procedure getPrompter()
  {
    Object property = null;
    if (hasSeparateFunctionNamespace())
      property = EnvironmentKey.FUNCTION;
    Procedure prompter = (Procedure) Environment.getCurrent()
      .get(getSymbol("default-prompter"), property, null);
    if (prompter != null)
      return prompter;
    else
      return new SimplePrompter();
  }

  static protected int env_counter = 0;

  public void runAsApplication (String[] args)
  {
    defaultInterpreter = this;
    if (environ == null)
      environ = Environment.make("interaction-environment."+(++env_counter));
    Environment.setCurrent(environ);
    kawa.repl.main(args);
  }
}

class SimplePrompter extends Procedure1
{
  public String prefix = "[";
  public String suffix = "] ";

  public Object apply1 (Object arg)
  {
    if (arg instanceof InPort)
      {
	InPort port = (InPort) arg;
	int line = port.getLineNumber() + 1;
	if (line >= 0)
	  return prefix + line + suffix;
      }
    return suffix;
  }
}
