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
  public final void runAsMain (String[] args)
  {
    // FIXME gnu.exp should not explicitly reference kawa.standard!
    Environment.setCurrent(new kawa.standard.Scheme().getEnvironment());
    kawa.repl.setArgs(args, 0);
    apply0();
  }

  /**
   * A subclass will typically override this like:
   * switch (method.selector) {
   *   case 3:  return function3();
   *   case 5:  return function5();
   *   default:  super.apply0(method);
   * }
   */

  public Object apply0(ModuleMethod method)
  {
    return applyN(method, Procedure.noArgs);
  }

  public Object apply1(ModuleMethod method, Object arg1)
  {
    Object[] args = new Object[1];
    args[0] = arg1;
    return applyN(method, args);
  }

  public Object apply2(ModuleMethod method, Object arg1, Object arg2)
  {
    Object[] args = new Object[2];
    args[0] = arg1;
    args[1] = arg2;
    return applyN(method, args);
  }

  public Object apply3(ModuleMethod method,
                       Object arg1, Object arg2, Object arg3)
  {
    Object[] args = new Object[3];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    return applyN(method, args);
  }

  public Object applyN(ModuleMethod method, Object[] args)
  {
    throw new WrongArguments(method, args.length);
  }

}
