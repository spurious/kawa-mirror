// Copyright (c) 2000  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.util;

public interface Consumer
{
	public void writeChar(int v);
	public void writeBoolean(boolean v);

	public void writeFloat(float v);
	public void writeDouble(double v);
	public void writeInt(int v);
	public void writeLong(long l);

	public void beginGroup(String typeName, Object type);
	public void endGroup(String typeName);

	/** Write a attribute for the current group.
	 * This is only allowed immediately after a beginGroup. */
	public void beginAttribute(String attrName, Object attrType);

	/** No more attributes in this group. */
	public void endAttributes();

	public void writeObject(Object v);

	/** True if consumer is ignoring rest of group.
	 * The producer can use this information to skip ahead. */
	public boolean ignoring();

	public void writeChars(String str);
	// public void writeChars(AbstractString str);
	public void write(char[] buf, int off, int len);
}

// This is for people using the Emacs editor:
// Local Variables:
// c-file-style: "java"
// c-file-offsets: ((substatement-open . 0))
// tab-width: 4
// indent-tabs-mode: t
// End:
