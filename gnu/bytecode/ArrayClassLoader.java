package gnu.bytecode;

/** Load classes from a set of byte arrays.
 * @author	Per Bothner
 */

public class ArrayClassLoader extends ClassLoader
{
  /** The raw byte arrays from which we will load the classes.
   * Each element is suitable for defineClass. */
  byte[][] classBytes;

  /** Number of classes managed by this loader. */
  int size;

  /** Classes that we have already loaded. */
  Class[] loadedClasses;

  String[] classNames;

  /** Load classes from the given byte arrays.
    By convention, the classes we manage are named "lambda"+<INTEGER>. */
  public ArrayClassLoader (byte[][] classBytes)
  {
    this.classBytes = classBytes;
    size = classBytes.length;
    loadedClasses = new Class [size];
    classNames = new String [size];
    for (int i = 0;  i < size;  i++)
      classNames[i] = "lambda" + i;
  }

  public ArrayClassLoader (String[] classNames, byte[][] classBytes)
  {
    this.classBytes = classBytes;
    size = classBytes.length;
    loadedClasses = new Class [size];
    this.classNames = classNames;
  }

  public Class loadClass (String name, boolean resolve)
       throws ClassNotFoundException
  {
    Class clas;
    for (int index = 0; ; index++)
      {
	if (index >= size)
	  {
	    clas = findSystemClass (name);
	    break;
	  }
	else if (name.equals (classNames[index]))
	  { 
	    clas = loadedClasses[index];
	    if (clas == null)
	      {
		byte[] bytes = classBytes[index];
		clas = defineClass (name, bytes, 0, bytes.length);
		loadedClasses[index] = clas;
		classBytes[index] = null;  // To help garbage collector.
	      }
            break;
	  }
      }

    if (resolve)
      resolveClass (clas);
    return clas;
  }
}
