package kawa.lang;
import codegen.ZipArchive;

/** Load classes from either a set of byte arrays or a Zip archive.
 * By convention, the classes we manage are named "lambda"+<INTEGER>.
 * @author	Per Bothner
 */

public class SchemeLoader extends ClassLoader
{
  /** The raw byte arrays from which we will load the classes.
   * Each element is suitable for defineClass.
   * This is null (and unused) if zar != null. */
  byte[][] classBytes;

  /** The zip archive from which we will load the classes.
   * The format of the archive is the same as classes.zip.
   * This is null (and unused) if classBytes != null. */
  ZipArchive zar;

  /** Number of classes managed by this loader. */
  int size;

  /** Number of classes loaded so far. */
  int loaded;

  /** Classes that we have already loaded. */
  Class[] loadedClasses;

  public SchemeLoader (byte[][] classBytes)
  {
    this.classBytes = classBytes;
    size = classBytes.length;
    loadedClasses = new Class [size];
  }

  public SchemeLoader (ZipArchive zar)
  {
    this.zar = zar;
    size = zar.size ();
    loadedClasses = new Class [size];
  }

  public Class loadClass (String name, boolean resolve)
       throws ClassNotFoundException
  {
    Class clas;
    if (name.startsWith ("lambda"))
      {
	int index;
	try
	  {
	    index = Integer.parseInt (name.substring (6));
	  }
	catch (NumberFormatException ex)
	  {
	    index = size;  // Something out of range
	  }
	if (index >= size)
	  clas = findSystemClass (name);
	else if (loadedClasses[index] == null)
	  {
	    if (classBytes != null)
	      {
		byte[] bytes = classBytes[index];
		clas = defineClass (bytes, 0, bytes.length);
		loadedClasses[index] = clas;
		classBytes[index] = null;  // To help garbage collector.
		loaded++;
	      }
	    else
	      {
		try
		  {
		    String member_name = name.replace ('.', '/') + ".class";
		    byte[] bytes = zar.contents (zar.find (member_name));
		    clas = defineClass (bytes, 0, bytes.length);
		    loadedClasses[index] = clas;
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
	  }
	else
	  clas = loadedClasses[index];
      }
    else
      clas = findSystemClass (name);
    if (resolve)
      resolveClass (clas);
    return clas;
  }
}
