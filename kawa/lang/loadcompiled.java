package kawa.lang;
import gnu.mapping.*;

public class loadcompiled extends Procedure1
{
  public loadcompiled ()
  {
    super ("load-compiled");
  }

  public final Object apply1 (Object arg1)
  {
    try
      {
	Class clas = Class.forName (arg1.toString());
	return clas.newInstance ();
      }
    catch (ClassNotFoundException ex)
      {
	throw new GenericError ("class not found: " + arg1.toString());
      }
    catch (NoSuchMethodError ex)
      {
	throw new GenericError ("NoSuchMethodError in " + arg1.toString()
				 + ": " + ex.getMessage());
      }
    catch (InstantiationException ex)
      {
	throw new GenericError ("class not instantiable: " + arg1.toString());
      }
    catch (IllegalAccessException ex)
      {
	throw new GenericError ("class illegal access: " + arg1.toString());
      }
  }

}
