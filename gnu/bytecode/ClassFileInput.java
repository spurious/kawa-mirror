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

  public ClassFileInput (InputStream str)
       throws IOException
  {
    super(str);
  }

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
    readAttributes(ctype);
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
    ctype.access_flags = readUnsignedShort();
    CpoolClass clas;
    String name;

    ctype.thisClassIndex = readUnsignedShort();
    clas = (CpoolClass) ctype.constants.getForced(ctype.thisClassIndex,
						  ConstantPool.CLASS);
    name = clas.name.string;
    ctype.this_name = name.replace('/', '.');
    ctype.setSignature("L"+name+";");

    ctype.superClassIndex = readUnsignedShort();
    if (ctype.superClassIndex == 0)
      ctype.setSuper((ClassType) null);
    else
      {
	clas = (CpoolClass) ctype.constants.getForced(ctype.superClassIndex,
						      ConstantPool.CLASS);
	name = clas.name.string;
	ctype.setSuper(name.replace('/', '.'));
      }

    int nInterfaces = readUnsignedShort();
    ctype.interfaces = new ClassType[nInterfaces];
    ctype.interfaceIndexes = new int[nInterfaces];
    for (int i = 0;  i < nInterfaces;  i++)
      {
	ctype.interfaceIndexes[i] = readUnsignedShort();
	clas = (CpoolClass) ctype.constants.getForced(ctype.superClassIndex,
						      ConstantPool.CLASS);
	name = clas.name.string;
	ctype.interfaces[i] = ClassType.make(name.replace('/', '.'));
      }
  }

  public int readAttributes (AttrContainer container) throws IOException
  {
    int count = readUnsignedShort();
    Attribute last = container.getAttributes();
    for (int i = 0;  i < count;  i++)
      {
	if (last != null)
	  {
	    for (;;)
	      {
		Attribute next = last.getNext();
		if (next == null)
		  break;
		last = next;
	      }
	  }
	
	int index = readUnsignedShort();
	CpoolUtf8 nameConstant = (CpoolUtf8)
	  ctype.constants.getForced(index, ConstantPool.UTF8);
	int length = readInt();
	nameConstant.intern();
	Attribute attr = readAttribute(nameConstant.string, length, container);
	if (attr != null)
	  {
	    if (attr.getNameIndex() == 0)
	      attr.setNameIndex(index);
	    if (last == null)
	      container.setAttributes(attr);
	    else
	      last.setNext(attr);
	    last = attr;
	  }
      }
    return count;
  }

  public final void skipAttribute (int length)
    throws IOException
  {
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

  public Attribute readAttribute (String name, int length, AttrContainer container)
    throws IOException
  {
    if (name == "SourceFile" && container instanceof ClassType)
      {
	return new SourceFileAttr(readUnsignedShort(), (ClassType) container);
      }
    else
      {
	byte[] data = new byte[length];
	readFully(data, 0, length);
	return new MiscAttr(name, data);
      }
  }

  public void readFields () throws IOException
  {
    int nFields = readUnsignedShort();
    ConstantPool constants = ctype.constants;
    for (int i = 0;  i < nFields;  i++)
      {
	int flags = readUnsignedShort();
	int nameIndex = readUnsignedShort();
	int descriptorIndex = readUnsignedShort();
	CpoolUtf8 nameConstant = (CpoolUtf8)
	  constants.getForced(nameIndex, ConstantPool.UTF8);
	CpoolUtf8 sigConstant = (CpoolUtf8)
	  constants.getForced(descriptorIndex, ConstantPool.UTF8);
	Type type = Type.signatureToType(sigConstant.string);
	Field fld = ctype.addField(nameConstant.string, type, flags);
		       
	readAttributes(fld);
      }
  }

  public void readMethods () throws IOException
  {
    int nMethods = readUnsignedShort();
    ConstantPool constants = ctype.constants;
    for (int i = 0;  i < nMethods;  i++)
      {
	int flags = readUnsignedShort();
	int nameIndex = readUnsignedShort();
	int descriptorIndex = readUnsignedShort();
	CpoolUtf8 nameConstant = (CpoolUtf8)
	  constants.getForced(nameIndex, ConstantPool.UTF8);
	CpoolUtf8 sigConstant = (CpoolUtf8)
	  constants.getForced(descriptorIndex, ConstantPool.UTF8);
	Method meth = ctype.addMethod(nameConstant.string,
				      sigConstant.string, flags);
	readAttributes(meth);
      }
  }
}
