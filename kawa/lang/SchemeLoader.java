package kawa.lang;

public class SchemeLoader extends ClassLoader
{
  byte[][] classBytes;
  Class[] loadedClasses;

  public SchemeLoader (byte[][] classBytes)
  {
    this.classBytes = classBytes;
    loadedClasses = new Class [classBytes.length];
  }

  public Class loadClass (String name, boolean resolve)
       throws ClassNotFoundException
  {
    // System.err.println ("SchemeLoader.loadClass ("+name+")");
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
	    index = loadedClasses.length;  // Something out of range
	  }
	if (index >= loadedClasses.length)
	  clas = findSystemClass (name);
	else if (loadedClasses[index] == null)
	  {
	    byte[] bytes = classBytes[index];
	    clas = defineClass (bytes, 0, bytes.length);
	    loadedClasses[index] = clas;
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
