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

  public ArrayClassLoader ()
  {
  }

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

  private void reserve(int count)
  {
    if (count <= 0)
      return;
    int newLength = size < count ? size + count : 2 * size;
    if (loadedClasses == null)
      loadedClasses = new Class[newLength];
    else if (size + count > loadedClasses.length)
      {
        Class[] loadedClassesNew = new Class[newLength];
        System.arraycopy(loadedClasses, 0, loadedClassesNew, 0, size);
        loadedClasses = loadedClassesNew;
      }
    if (classBytes == null)
      classBytes = new byte[newLength][];
    else if (size + count > classBytes.length)
      {
        byte[][] classBytesNew = new byte[newLength][];
        System.arraycopy(classBytes, 0, classBytesNew, 0, size);
        classBytes = classBytesNew;
      }
    if (classNames == null)
      classNames = new String[newLength];
    else if (size + count > classNames.length)
      {
        String[] classNamesNew = new String[newLength];
        System.arraycopy(classNames, 0, classNamesNew, 0, size);
        classNames = classNamesNew;
      }
  }

  public void addClass(Class clas)
  {
    reserve(1);
    classNames[size] = clas.getName();
    loadedClasses[size] = clas;
    size++;
  }

  public void addClass(String name, byte[] bytes)
  {
    reserve(1);
    classNames[size] = name == null ? ("lambda"+size) : name;
    classBytes[size] = bytes;
    size++;
  }

  public void addClass (ClassType ctype)
    throws java.io.IOException
  {
    if ((ctype.flags & ClassType.EXISTING_CLASS) != 0)
      addClass(ctype. getReflectClass());
    else
      addClass(ctype.getName(), ctype.writeToArray());
  }

  public Class loadClass (String name, boolean resolve)
       throws ClassNotFoundException
  {
    Class clas;
    for (int index = 0; ; index++)
      {
	if (index >= size)
	  {
	    clas = Class.forName(name);
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
