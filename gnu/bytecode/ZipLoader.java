package gnu.bytecode;

/** Load classes from a Zip archive.
 * @author	Per Bothner
 */

public class ZipLoader extends ClassLoader
{
  /** The zip archive from which we will load the classes.
   * The format of the archive is the same as classes.zip. */
  ZipArchive zar;

  /** Number of classes managed by this loader. */
  int size;

  /** Number of classes loaded so far. */
  int loaded;

  /** Classes that we have already loaded. */
  Class[] loadedClasses;

  //* Names of classes we have already loaded. */
  String[] loadedNames;

  public ZipLoader (ZipArchive zar)
  {
    this.zar = zar;
    size = zar.size ();
    loadedClasses = new Class [size];
    loadedNames = new String [size];
  }

  public Class loadClass (String name, boolean resolve)
       throws ClassNotFoundException
  {
    Class clas;
    for (int index = 0; ; index++)
      {
	if (index >= loaded)
	  {
	    String member_name = name.replace ('.', '/') + ".class";
	    ZipMember member = zar.find (member_name);
	    if (member == null)
	      clas = findSystemClass (name);
	    else
	      {
		try
		  {
		    byte[] bytes = zar.contents (member);
		    clas = defineClass (bytes, 0, bytes.length);
		    loadedClasses[loaded] = clas;
		    loadedNames[loaded] = name;
		    loaded++;
		    if (size == loaded)
		      zar.close ();
		  }
		catch (java.io.IOException ex)
		  {
		    throw new
		      Error ("IOException while loading from ziparchive: " +
			     ex.toString ());
		  }
	      }
	    break;
	  }
	if (name.equals (loadedNames[index]))
	  {
	    clas = loadedClasses[index];
	    break;
	  }
      }

    if (resolve)
      resolveClass (clas);
    return clas;
  }
}
