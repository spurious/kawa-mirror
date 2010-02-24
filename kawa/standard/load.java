package kawa.standard;
import java.io.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.SourceMessages;
import gnu.text.SyntaxException;
import gnu.lists.*;
import gnu.text.*;
import kawa.Shell;

public class load extends Procedure1 {
  boolean relative;

  public load (String name, boolean relative)
  {
    super(name);
    this.relative = relative;
  }

  public static final load load = new load("load", false);
  public static final load loadRelative = new load("load-relative", true);

  public final Object apply1 (Object arg1)
    throws Throwable
  {
    return apply2 (arg1, Environment.getCurrent ());
  }

  public final Object apply2 (Object name, Object arg2)
    throws Throwable
  {
    try
      {
	Environment env = (Environment) arg2;
        Path path = Path.valueOf(name);
        if (relative)
          {
            Path curPath = (Path) Shell.currentLoadPath.get();
            if (curPath != null)
              path = curPath.resolve(path);
          }
        Shell.runFile(path.openInputStream(), path, env, true, 0);
	return Values.empty;
      }
    catch (java.io.FileNotFoundException e)
      {
	throw new RuntimeException ("cannot load "+e.getMessage());
      }
    catch (SyntaxException ex)
      {
	throw new RuntimeException("load: errors while compiling '"+
				   name+"':\n"+ex.getMessages().toString(20));
      }
  }
}
