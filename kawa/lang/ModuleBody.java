package kawa.lang;

/**
 * Abstract class for the dummy top-level function of a module. */

public abstract class ModuleBody extends Procedure0
{
  public Object apply0 ()
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return run (Environment.current ());
  }

  public abstract Object run (Environment env)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol;

  /** This is invoked by main when ModuleBody is compiled with --main. */
  public final void runAsMain (String[] args)
  {
    Environment.setCurrent(new kawa.standard.Scheme().getEnvironment());
    kawa.repl.setArgs(args, 0);
    apply0();
  }

}
