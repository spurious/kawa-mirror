// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.InputStream;
import java.io.IOException;
import java.io.FileInputStream;

/** Class to read a ClassType from a DataInputStream (.class file).
 * 
 * To print out the contents of a class file foo.class, you can use
 * the class <code>dump</code> as an application:
 * <pre>
 * java gnu.bytecode.dump foo.class
 * </pre>
 * This will print out the constant pool, fields, methods, superclass,
 * and implemented interfaces of class <code>foo</code>.
 * It is useful for printing out more detailed information
 * than <code>javap</code> does.
 * 
 * @author Per Bothner
 */

public class dump extends ClassFileInput
{
  ClassTypeWriter writer;

  public dump (InputStream str, boolean verbose)
       throws IOException, ClassFormatError
  {
    super(str);
    this.ctype = new ClassType();
    writer = new ClassTypeWriter (ctype, System.out, 0);
    writer.printConstants = verbose;
    if (!readHeader())
      throw new ClassFormatError("invalid magic number");
    readConstants();
    readClassInfo();
    readFields();
    readMethods();
    readAttributes(ctype);

    writer.printClassInfo();
    writer.printFields();
    writer.printMethods();
    printAttributes ();
    writer.flush();
  }

  public ConstantPool readConstants () throws IOException
  {
    ctype.constants = super.readConstants();
    if (writer.printConstants)
      writer.printConstantPool();
    return ctype.constants;
  }

  public Attribute readAttribute (String name, int length, AttrContainer container)
    throws IOException
  {
    return super.readAttribute (name, length, container);
  }

  public void printAttributes ()
  {
    AttrContainer attrs = ctype;
    writer.println();
    writer.print("Attributes (count: ");
    writer.print(Attribute.count(attrs));
    writer.println("):");
    writer.printAttributes (attrs);
  }

  /** Reads a .class file, and prints out the contents to System.out.
   * Very rudimentary - prints out the constant pool, and field and method
   * names and types, but only minimal attributes (i.e. no dis-assembly yet).
   * @param args One argument - the name of a .class file.
   */
  public static void main (String[] args)
  {
    int alen = args.length;
    if (alen == 0)
      usage();
    boolean verbose = false;
    for (int i = 0; i < alen; i++)
      {
        String filename = args[i];
        if (filename.equals("-verbose") || filename.equals("--verbose"))
          {
            verbose = true;
            continue;
          }
        try
          {
            java.io.InputStream inp = new FileInputStream(filename);
            new dump(inp, verbose);
          }
        catch (java.io.FileNotFoundException e)
          {
            System.err.println("File "+filename+" not found");
            System.exit(-1);
          }
        catch (java.io.IOException e)
          {
            System.err.println(e);
            System.exit(-1);
          }
      }
  }

  public static void usage()
  {
    System.err.println("Usage: [--verbose] foo.class");
    System.exit(-1);
  }
}
