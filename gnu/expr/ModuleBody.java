package gnu.expr;
import gnu.mapping.*;

/**
 * Abstract class for the dummy top-level function of a module. */

public abstract class ModuleBody extends Procedure0
{
  public Object apply0 ()
  {
    return run();
  }

  public abstract Object run ();

  /** This is invoked by main when ModuleBody is compiled with --main. */
  /*
  public final void runAsMain (String[] args)
  {
    Environment.setCurrent(new Scheme().getEnvironment());
    repl.setArgs(args, 0);
    apply0();
  }
  */

}
