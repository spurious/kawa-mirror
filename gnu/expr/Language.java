// Copyright (c) 2002, 2003, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.text.SourceMessages;
import java.io.*;

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

  /** Return the result of evaluating a string as a source expression. */
  public final Object eval (String string) throws Throwable
  {
    return eval(new CharArrayInPort(string));
  }

  /** Evaluate expression(s) read from a Reader.
   * This just calls eval(InPort).
   */
  public final Object eval (Reader in) throws Throwable
  {
    return eval(in instanceof InPort ? (InPort) in : new InPort(in));
  }

  /** Evaluate expression(s) read from an InPort. */
  public final Object eval (InPort port) throws Throwable
  {
    CallContext ctx = CallContext.getInstance();
    int oldIndex = ctx.startFromContext();
    try
      {
	eval(port, ctx);
	return ctx.getFromContext(oldIndex);
      }
    catch (Throwable ex)
      { 
	ctx.cleanupFromContext(oldIndex);
	throw ex;
      }
  }

  /** Evaluate a string and write the result value(s) on a Writer. */
  public final void eval (String string, Writer out) throws Throwable
  {
    eval(new CharArrayInPort(string), out);
  }

  /** Evaluate a string and write the result value(s) to a PrintConsumer.
   * This is to disambiguate calls using OutPort or XMLPrinter,
   * which are both Writer and Consumer. */
  public final void eval (String string, PrintConsumer out) throws Throwable
  {
    eval(string, getOutputConsumer(out));
  }

  /** Evaluate a string and write the result value(s) to a Consumer. */
  public final void eval (String string, Consumer out) throws Throwable
  {
    eval(new CharArrayInPort(string), out);
  }

  /** Read expressions from a Reader and write the result to a Writer. */
  public final void eval (Reader in, Writer out) throws Throwable
  {
    eval(in, getOutputConsumer(out));
  }

  /** Read expressions from a Reader and write the result to a Consumer. */
  public void eval (Reader in, Consumer out) throws Throwable
  {
    InPort port = in instanceof InPort ? (InPort) in : new InPort(in);
    CallContext ctx = CallContext.getInstance();
    Consumer save = ctx.consumer;
    try
      {
	ctx.consumer = out;
	eval(port, ctx);
      }
    finally
      {
	ctx.consumer = save;
      }
  }

  public void eval (InPort port, CallContext ctx) throws Throwable
  {
    SourceMessages messages = new SourceMessages();
    Environment saveEnviron = Environment.getCurrent();
    if (saveEnviron != environ)
      Environment.setCurrent(environ);
    Language saveLang = getDefaultLanguage();
    setDefaultLanguage(this);
    try
      {
	Compilation comp = parse(port, messages, PARSE_IMMEDIATE);
	ModuleExp.evalModule(environ, ctx, comp);
      }
    finally
      {
	if (saveEnviron != environ && saveEnviron != null)
	  Environment.setCurrent(saveEnviron);
	setDefaultLanguage(saveLang);
      }
    if (messages.seenErrors())
      throw new RuntimeException("invalid syntax in eval form:\n"
				 + messages.toString(20));
  }

  static protected int env_counter = 0;

  public void runAsApplication (String[] args)
  {
    defaultLanguage = this;
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
