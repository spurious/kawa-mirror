// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

/* Represents an Attribute of an AttrContainer.
 * Various sub-classses are used for standard attributes,
 * or you can use MiscAttr for a generic attribute.
 * @author      Per Bothner
 */

public abstract class Attribute
{
  Attribute next;
  public final Attribute getNext() { return next; }
  public final void setNext(Attribute next) { this.next = next; }

  /** Every Attribute belongs to some AttrContainer object. */
  AttrContainer container;
  public final  AttrContainer getContainer() { return container; }
  public final void setContainer(AttrContainer container)
  { this.container = container; }

  String name; // This is an interned string.
  int name_index;  // If non-zero, the constant-pool index of name.

  public final String getName() { return name; }
  public final void setName(String name) { this.name = name.intern(); }

  public final int getNameIndex() { return name_index; }
  public final void setNameIndex(int index) { name_index = index; }

  /** Create a new Attribute.
    * @param name - an interned String that names the Attribute. */
  public Attribute (String name)
  {
    this.name = name;
  }

  /** Find an Attribute by name, in an attribute cointainer.
    * @param container the attribute container to search
    * @param name the (interned) name of the attribute we are seeking
    * @return the matching Attribute, or null if the search failed.
    */
  public static Attribute get (AttrContainer container, String name)
  {
    for (Attribute attr = container.getAttributes();
	 attr != null;  attr = attr.next)
      {
	if (attr.getName() == name)
	  return attr;
      }
    return null;
  }

  /** Add any needed constant pool entries for this Attribute.
    * Overridden by sub-classes.
    * Do any other cleanup needed before writing out a .class file. */
  public void assignConstants (ClassType cl)
  {
    if (name_index == 0)
      name_index = cl.getConstants().addUtf8(name).getIndex();
  }

  /** Add any needed constant pool entries for all attributes in a container.
    * Do any other cleanup needed before writing out a .class file. */
  public static void assignConstants (AttrContainer container, ClassType cl)
  {
    for (Attribute attr = container.getAttributes();
	 attr != null;  attr = attr.next)
      {
	attr.assignConstants(cl);
      }
  }

  /** Return the length of the attribute in bytes.
    * Does not include the 6-byte header (for the name_index and the length).*/
  abstract public int getLength();

  /** Write out the contents of the Attribute.
    * Does not write the 6-byte attribute header. */
  abstract public void write (DataOutputStream dstr)
    throws java.io.IOException;

  public static int count (AttrContainer container)
  {
    int count = 0;
    for (Attribute attr = container.getAttributes();
	 attr != null;  attr = attr.next)
      count++;
    return count;
  }

  public static void writeAll (AttrContainer container, DataOutputStream dstr)
    throws java.io.IOException
  {
    int count = count(container);
    dstr.writeShort(count);
    for (Attribute attr = container.getAttributes();
	 attr != null;  attr = attr.next)
      {
	if (attr.name_index == 0)
	  throw new Error("Attribute.writeAll called without assignConstants");
	dstr.writeShort(attr.name_index);
	dstr.writeInt(attr.getLength());
	attr.write(dstr);
      }
  }

  public void print (ClassTypeWriter dst)
  {
    dst.print("Attribute \"");
    dst.print(getName());
    dst.print("\", length:");
    dst.println(getLength());
  }

};
