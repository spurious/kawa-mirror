// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.InputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.FileInputStream;

/** Class to read a ClassType from a DataInputStream (.class file).
 * This currently skips all atributes, including the Code attribute.
 * @author Per Bothner
 */

public class ClassFileInput extends DataInputStream
{
  ClassType ctype;
  InputStream str;

  public ClassFileInput (ClassType ctype, InputStream str)
       throws IOException, ClassFormatError
  {
    super(str);
    this.ctype = ctype;
    if (!readHeader())
      throw new ClassFormatError("invalid magic number");
    ctype.constants = readConstants();
    readClassInfo();
    readFields();
    readMethods();
    readAttributes();
  }

  public boolean readHeader () throws IOException
  {
    int magic = readInt();
    if (magic != 0xcafebabe)
      return false;
    short minor_version = readShort();
    short major_version = readShort();
    return true;
  }

  public ConstantPool readConstants () throws IOException
  {
    return new ConstantPool(this);
  }

  public void readClassInfo () throws IOException
  {
    ctype.access_flags = readShort();
    CpoolClass clas;
    String name;

    ctype.thisClassIndex = readShort();
    clas = (CpoolClass) ctype.constants.getForced(ctype.thisClassIndex,
						  ConstantPool.CLASS);
    name = clas.name.string;
    ctype.this_name = name.replace('/', '.');
    ctype.setSignature("L"+name+";");

    ctype.superClassIndex = readShort();
    if (ctype.superClassIndex == 0)
      ctype.setSuper((ClassType) null);
    else
      {
	clas = (CpoolClass) ctype.constants.getForced(ctype.superClassIndex,
						      ConstantPool.CLASS);
	name = clas.name.string;
	ctype.setSuper(name.replace('/', '.'));
      }

    int nInterfaces = readShort();
    ctype.interfaces = new ClassType[nInterfaces];
    ctype.interfaceIndexes = new int[nInterfaces];
    for (int i = 0;  i < nInterfaces;  i++)
      {
	ctype.interfaceIndexes[i] = readShort();
	clas = (CpoolClass) ctype.constants.getForced(ctype.superClassIndex,
						      ConstantPool.CLASS);
	name = clas.name.string;
	ctype.interfaces[i] = ClassType.make(name.replace('/', '.'));
      }
  }

  public int readAttributes () throws IOException
  {
    int count = readShort() & 0xFFFF;
    for (int i = 0;  i < count;  i++)
      readAttribute();
    return count;
  }

  public void readAttribute () throws IOException
  {
    int index = readShort() & 0xFFFF;
    int length = readInt();
    int read = 0;
    while (read < length)
      {
	int skipped = (int) skip(length - read);
	if (skipped == 0)
	  {
	    if (read() < 0)
	      throw new java.io.EOFException
		("EOF while reading class files attributes"); 
	    skipped = 1;
	  }
	read += skipped;
      }
  }

  public void readFields () throws IOException
  {
    int nFields = readShort();
    ConstantPool constants = ctype.constants;
    for (int i = 0;  i < nFields;  i++)
      {
	short flags = readShort();
	short nameIndex = readShort();
	short descriptorIndex = readShort();
	CpoolUtf8 nameConstant = (CpoolUtf8)
	  constants.getForced(nameIndex, ConstantPool.UTF8);
	CpoolUtf8 sigConstant = (CpoolUtf8)
	  constants.getForced(descriptorIndex, ConstantPool.UTF8);
	Type type = Type.signatureToType(sigConstant.string);
	ctype.addField(nameConstant.string, type, flags);
		       
	readAttributes();
      }
  }

  public void readMethods () throws IOException
  {
    int nMethods = readShort();
    ConstantPool constants = ctype.constants;
    for (int i = 0;  i < nMethods;  i++)
      {
	short flags = readShort();
	short nameIndex = readShort();
	short descriptorIndex = readShort();
	CpoolUtf8 nameConstant = (CpoolUtf8)
	  constants.getForced(nameIndex, ConstantPool.UTF8);
	CpoolUtf8 sigConstant = (CpoolUtf8)
	  constants.getForced(descriptorIndex, ConstantPool.UTF8);
	ctype.addMethod(nameConstant.string, sigConstant.string, flags);
		       
	readAttributes();
      }
  }

  public static void usage()
  {
    System.err.println("Usage: foo.class");
    System.exit(-1);
  }

  /** Reads a .class file, and prints out the conents to System.out.
   * Very rudimentary - prints out constant pool, and field and method
   * names and types, but no attributes (i.e. no dis-assembly yet).
   * @param args One argument - the name of a .class file.
   */
  public static void main (String[] args)
  {
    if (args.length == 0)
      usage();
    String filename = args[0];
    try
      {
	java.io.InputStream inp = new FileInputStream(filename);
	ClassType ctype = new ClassType();
	ClassFileInput in = new ClassFileInput(ctype, inp);
	ClassTypeWriter.print(ctype, System.out, 0);
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
