// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.buffer;
import gnu.mapping.WrappedException;

public abstract class EToolkit
{
  public static String defaultToolkit = "gnu.jemacs.swing.SwingToolkit";
  static EToolkit instance;
  static Class toolkitClass;

  public static EToolkit getInstance ()
  {
    EToolkit inst = instance;
    if (inst != null)
      return inst;
    return getInstance ("gnu.jemacs.swing.SwingToolkit");
  }

  public static synchronized EToolkit getInstance (String toolkitClassname)
  {
    if (instance == null)
      {
	try
	  {
	    if (toolkitClass == null)
	      toolkitClass = Class.forName(toolkitClassname);
	    instance = (EToolkit) toolkitClass.newInstance();
	  }
	catch (Exception ex)
	  {
	    throw new WrappedException(ex);
	  }
      }
    return instance;
  }

  public abstract Buffer newBuffer (String name);

  public abstract EFrame newFrame(Buffer buffer);
}
