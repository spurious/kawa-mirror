// Copyright (c) 2000  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.util;
import gnu.text.Char;
import gnu.math.IntNum;
import gnu.math.DFloNum;

/** A Consumer that append the consumed values to an FVector. */

public class Collector implements Consumer
{
	FVector value;

	public void writeDouble(double v)
	{
		writeObject(new DFloNum(v));
	}

	public void writeFloat(float v)
	{
		writeObject(new DFloNum(v));
	}

	public void writeByte(int v)
	{
		writeObject(IntNum.make(v));
	}

	public void writeShort(int v)
	{
		writeObject(IntNum.make(v));
	}

	public void writeInt(int v)
	{
		writeObject(IntNum.make(v));
	}

	public void writeLong(long v)
	{
		writeObject(IntNum.make(v));
	}

	public void writeChar(int v)
	{
		writeObject(Char.make(v));
	}

	public void writeBoolean(boolean v)
	{
		writeObject(v ? Boolean.TRUE : Boolean.FALSE);
	}

	public void beginGroup(Object tag)
	{
	}

	public void endGroup(Object tag)
	{
	}

	/** Write a attribute for the current group.
	 * This is only allowed immediately after a beginGroup. */
	public void beginAttribute(Object name)
	{
	}

	public void endAttribute(Object name)
	{
	}

	public void writeObject(Object v)
	{
		value.add(v);
	}

	/** True if consumer is ignoring rest of group.
	 * The producer can use this information to skip ahead. */
	public boolean ignoring()
	{
		return false;
	}

	public void writeChars(String str)
	{
		int len = str.length();
		for (int i = 0;  i < len;  i++)
			writeChar(str.charAt(i));
	}

	public void writeChars(AbstractString str)
	{
		int len = str.length();
		for (int i = 0;  i < len;  i++)
			writeChar(str.charAt(i));
	}

	public void write(char[] buf, int off, int len)
	{
		for (int i = len;  --i >= 0; )
			writeChar(buf[off++]);
	}

	public Object getResult()
	{
		return value;
	}
}

// This is for people using the Emacs editor:
// Local Variables:
// c-file-style: "java"
// c-file-offsets: ((substatement-open . 0))
// tab-width: 4
// indent-tabs-mode: t
// End:
