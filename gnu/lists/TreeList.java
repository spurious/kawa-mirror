// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A compact representation of a tree, that is a nested list structure.
 * The data structure can store anything that can be emitted to a Consumer.
 * This data structure is optimized for efficient forwards traversal
 * through the data structure, not random access.
 * It does have an "insertion point"; insertions and deletions are
 * efficient through the use of a buffer gap.
 * It is a reasonable choice for a "DOM" for XML data.
 */

public class TreeList extends AbstractSequence
implements Consumer, PositionConsumer, Consumable
{
  // Some public fields and methods are public which probably shouldn't be,
  // for the sake of NamespaceResolver.  FIXME.  Perhaps an abstract class
  // in gnu.lists that NamespaceResolver could extend?

  public Object[] objects;
  static final Object availObject = new String("(AVAIL");
  public char[] data;
  public int gapStart;
  public int gapEnd;

  /** If non-zero, gap is in an attribute starting (1 less than) here. */
  public int attrStart;

  public TreeList()
  {
    resizeObjects();
    gapEnd = 200;
    data = new char[gapEnd];
  }

  /**
   * Make a copy of a sub-range of a TreeList.
   * @param list the TreeList to copy
   * @param startPosition start of range, as a raw index in data
   * @param endPosition end of range, as a raw index in data
   */
  public TreeList(TreeList list, int startPosition, int endPosition)
  {
    this();
    list.consumeRange(startPosition, endPosition, this);
  }

  public TreeList(TreeList list)
  {
    this(list, 0, list.data.length);
  }

  public void clear()
  {
    gapStart = 0;
    gapEnd = data.length;
    attrStart = 0;
    if (gapEnd > 1500)
      {
	gapEnd = 200;
	data = new char[gapEnd];
      }
    objects = null;
    resizeObjects();
  }

  // The data array contains an encoding of values, as follows:
  // 0x0000 ... 0x9FFF:  A single Unicode character.
  // 0xAXXX: BEGIN_GROUP_SHORT
  // 0xBXXX:  negative integer ((short)0x0XXX<<4)>>4, range -4096 to -1
  // 0xCXXX:  positive integer 0x0XXX, in the range 0 to 4095
  // 0xDXXX:  positive integer 0x1XXX, in the range 4096 to 8191
  // 0xEXXX:: OBJECT_REF_SHORT.  The object in objects[0xXXX].
  // 0xF0XX:  A byte (encoded character or char fragment) ((byte) 0xXX).
  // 0xF100 ... 0xF101:  A Boolean (BOOL_FALSE, BOOL_TRUE)
  // 0xF102 A B:      INT_FOLLOWS - 32-bit int (big-endian)
  // 0xF103 A B C D:  LONG_FOLLOWS 64-bit long int (big-endian)
  // 0xF104 A B:      FLOAT_FOLLOWS 32-bit float (big-endian)
  // 0xF105 A B C D:  DOUBLE_FOLLOWS 64-bit double (big-endian)
  // 0xF106: CHAR_FOLLOWS
  // 0xF107: CHAR_PAIR_FOLLOWS
  // 0xF108: BEGIN_GROUP_LONG
  // 0xF109: BEGIN_ATTRIBUTE_LONG
  // 0xF10A: END_ATTRIBUTE
  // 0xF10B: END_GROUP_SHORT
  // 0xF10C: END_GROUP_LONG
  // 0xF10D A B: OBJECT_REF_FOLLOWS:  The object in objects[(A,B)].
  // 0xF10E A B: POSITION_REF_FOLLOWS:  The TreePosition in objects[(A,B)].
  // 0xF10F A B C D E F: POSITION_TRIPLE_FOLLOWS
  // 0xF110 BEGIN_DOCUMENT

  /** The largest Unicode character that can be encoded in one char. */
  static final int MAX_CHAR_SHORT = 0x9FFF;

  /** The smallest integer that can use the short int encoding. */
  static final int MIN_INT_SHORT = -0x1000;  // -4096

  /** The largest integer that can use the short int encoding. */
  static final int MAX_INT_SHORT = 0x1FFF;  // 8191

  /** The value used to encode the integer zero. */
  static final int INT_SHORT_ZERO = 0xC000;

  /** The value used to encode the object in objects[0]. */
  static final int OBJECT_REF_SHORT = 0xE000;

  /** The maximum offset in the objects array for a short object-ref. */
  static final int OBJECT_REF_SHORT_INDEX_MAX = 0xFFF;

  /** Followed by 2 chars that provide an index into objects. */
  static final char OBJECT_REF_FOLLOWS = 0xF10D;

  /** Followed by 2 chars that provide an index into objects. */
  static final char POSITION_REF_FOLLOWS = 0xF10E;

  /** A position triple referenceing som eother "nodes".
   * Followed by index of squence (2 chars), ipos (2 chars), and
   * index of xpos or -1 if xpos==null (2 chars). */
  static final char POSITION_TRIPLE_FOLLOWS = 0xF10F;

  /** Encoding prefix that indicates a byte value. */
  static final int BYTE_PREFIX = 0xF000;

  /** The value used to encode false. */
  static final char BOOL_FALSE = 0xF100;

  /** The value used to encode true. */
  static final char BOOL_TRUE = 0xF101;

  /** A 32-bit integer, non-compact form.
   *
   * [INT_FOLLOWS]
   * [word1], [word2]:  The big-endian bits of the integer.
   */
  static final int INT_FOLLOWS = 0xF102;

  /** A 64-bit long integer.
   *
   * [LONG_FOLLOWS]
   * [word1], [word2], [word3], [word4]:  The big-endian bits of the long.
   */
  static final int LONG_FOLLOWS = 0xF103;

  /** A 64-bit float floating-pointer number.
   *
   * [FLOAT_FOLLOWS]
   * [word1], [word2]:  The big-endian bits of the float.
   */
  static final int FLOAT_FOLLOWS = 0xF104;

  /** A 64-bit double floating-pointer number.
   *
   * [DOUBLE_FOLLOWS]
   * [word1], [word2], [word3], [word4]:  The big-endian bits of the double.
   */
  static final int DOUBLE_FOLLOWS = 0xF105;

  /** A 16-bit (non-compact) Unicode char follows. */
  static final int CHAR_FOLLOWS = 0xF106;

  /** A surrogate pair follows.  (not implemented). */
  static final int CHAR_PAIR_FOLLOWS = CHAR_FOLLOWS + 1;

  /** The beginning of an attribute.
   * [BEGIN_ATTRIBUTE_LONG]
   * [index], 2 shorts, where objects[index] is the attribute type name
   *   and objects[index+1] is the attribute type object.
   * [end_offset], 2 shorts, giving the location of the following
   *   END_ATTRIBUTE.  If the attribute straddles the gap, then
   *   end_offset is a negative offset relative to data.length.
   *   (Therefore allocating more space for the gap does not require
   *   adjusting end_offset.)  Otherwise, the end_offset is relative
   *   to the BEGIN_ATTRIBUTE_LONG word.
   */
  static final int BEGIN_ATTRIBUTE_LONG = 0xF109;

  /** The end of an attribute of a node. */
  static final int END_ATTRIBUTE = 0xF10A;

  /** Beginning of a document.
   * No parameters.  Used to distinguish a document from its element node.
   */
  static final int BEGIN_DOCUMENT = 0xF110;

  /** Beginning of a group, compact form.
   *
   * [BEGIN_GROUP_SHORT + index], where objects[index] is the group's
   *   type name and objects[index+1] is the type object.
   * [end_offset], the unsigned offset (from the initial word)
   *   to the corresponding END_GROUP_SHORT.
   * [parent_offset], the (unsigned absolute value of the) offset
   *   to the outer BEGIN_GROUP_SHORT/BEGIN_GROUP_LONG.  (If this is the
   *   outermost group, then parent_offset==0.)
   *
   * This should is used when index < BEGIN_GROUP_SHORT_INDEX_MAX,
   * both end_offset and parent_offset fit in 16 bits,
   * and the group does not straddle the gap.
   */
  static final int BEGIN_GROUP_SHORT = 0xA000;
  static final int BEGIN_GROUP_SHORT_INDEX_MAX = 0xFFF;

  /** End of a group, compact form.
   *
   * [END_GROUP_SHORT]
   * [begin_offset], the unsigned absolute value of the offset to the
   *   matching BEGIN.  (This is the same as the matching end_offset.)
   *
   */
  static final int END_GROUP_SHORT = 0xF10B;

  /** Begin of a group, non-compact form.
   *
   * [BEGIN_GROUP_LONG]
   * [end_offset], in 2 shorts.  The position of the matching END_GROUP_LONG.
   *   If the group straddles the gap, then end_offset is a negative offset
   *   relative to data.length.  (Therefore allocating more space for the
   *   gap does not require adjusting any end_offset.)   If the group and
   *   and its children are all on the same side of the gap, then end_offset
   *   is a positive offset relative to the BEGIN_GROUP_LONG word.  (Hence
   *   shifting an entire group when the gap is moved does not require
   *   changing its end_offset.)
   *
   * Note that the space taken by a BEGIN_GROUP_LONG is the same that
   * needed for a BEGIN_GROUP_SHORT (but a END_GROUP_LONG takes much
   * more space than a END_GROUP_SHORT).  This is to make it easier
   * to convert a BEGIN_GROUP_LONG to a BEGIN_GROUP_SHORT or vice
   * versa, as needed.
   */
  static final int BEGIN_GROUP_LONG =  0xF108;

  /** End of a group, non-compact form.
   *
   * [END_GROUP_LONG]
   * [index], 2 shorts where objects[index] is the group's type name and
   *   objects[index+1] is the type object.
   * [begin_offset], in 2 shorts.  The position of the matching
   *   BEGIN_GROUP_LONG.  If the group straddles the gap, then begin_offset
   *   is the actual index (i.e. relative to the start of data) of the
   *   matching BEGIN_GROUP_LONG.  (Therefore allocating more space for the
   *   gap does not require adjusting begin_offset.)  If the group does not
   *   straddle the gap, then begin_offset is a negative offset relative
   *   to the END_GROUP_LONG word.  (Hence shifting an entire group when
   *   the gap is moved does not require changing its begin_offset.)
   *   relative to data.length.
   * [parent_offset], in 2 shorts.  The position of the outer BEGIN_GROUP_LONG
   *   or BEGIN_GROUP_SHORT.  If the difference straddles the gap (i.e.
   *   either this group straddles the gap or the parent group does and the
   *   gap precedes this group), then parent_offset is the actual index
   *   of the parent group.  Otherwise, then parent_offset is a negative
   *   offset relative to the END_GROUP_LONG word.
   */
  static final int END_GROUP_LONG = 0xF10C;

  int currentBeginGroup = 0;

  public void ensureSpace(int needed)
  {
    int avail = gapEnd - gapStart;
    if (needed > avail)
      {
	int oldSize = data.length;
	int neededSize = oldSize - avail + needed;
	int newSize = 2 * oldSize;
	if (newSize < neededSize)
	  newSize = neededSize;
	char[] tmp = new char[newSize];
	if (gapStart > 0)
	  System.arraycopy(data, 0, tmp, 0, gapStart);
	int afterGap = oldSize - gapEnd;
	if (afterGap > 0)
	  System.arraycopy(data, gapEnd, tmp, newSize - afterGap, afterGap);
	gapEnd = newSize - afterGap;
	data = tmp;
      }
  }

  public final void resizeObjects()
  {
    int oldLength;
    int newLength;
    Object[] tmp;
    if (objects == null)
      {
	oldLength = 0;
	newLength = 100;
	tmp = new Object[newLength];
      }
    else
      {
	oldLength = objects.length;
	newLength = 2 * oldLength;
	tmp = new Object[newLength];
	System.arraycopy(objects, 0, tmp, 0, oldLength);
      }
    Object availObject = this.availObject;
    for (int i = oldLength;  i < newLength;  i++)
      tmp[i] = availObject;
    objects = tmp;
  }

  protected int find(Object arg1)
  {
    // FIXME - linear search!
    int i = 0;
    int len = objects.length;
    Object availObject = this.availObject;
    int avail = -1;
    for (; i < len; i++)
      {
	Object obj = objects[i];
	if (obj == arg1)
	  return i;
	if (obj == availObject && avail < 0)
	  avail = i;
      }
    if (avail >= 0)
      {
	objects[avail] = arg1;
	return avail;
      }
    resizeObjects();
    objects[len] = arg1;
    return len;
  }

  protected int find(Object arg1, Object arg2)
  {
    // FIXME - linear search!
    int i = 0;
    int len = objects.length;
    Object availObject = this.availObject; // Optimization.
    int avail = -1;
    Object[] objects = this.objects; // Optimization.
    for (; i + 1 < len; i++)
      {
	Object obj1 = objects[i];
	if (obj1 == arg1 && objects[i+1] == arg2)
	  return i;
	if (avail < 0 && obj1 == availObject && objects[i+1] == availObject)
	  avail = i;
      }
    if (avail >= 0)
      {
	objects[avail] = arg1;
	objects[avail+1] = arg2;
	return avail;
      }
    resizeObjects();
    objects = this.objects;
    objects[len] = arg1;
    objects[len+1] = arg2;
    return len;
  }

  /** Get a 32-bit int from the data array. */
  final protected int getIntN(int index)
  {
    return (data[index] << 16) | (data[index + 1]);
  }

  /** Get a 64-bit long from the data array. */
  final protected long getLongN(int index)
  {
    char[] data = this.data; // Optimization.
    return ((data[index] << 48) | (data[index+1] << 32)
	    | (data[index+2] << 16) | data[index + 3]);
  }

  final public void setIntN(int index, int i)
  {
    data[index] = (char) (i >> 16);
    data[index+1] = (char) i;
  }

  public boolean consume(TreePosition position)
  {
    ensureSpace(3);
    // FIXME - no need for find to search in this case!
    int index = find(new TreePosition(position));
    data[gapStart++] = POSITION_REF_FOLLOWS;
    setIntN(gapStart, index);
    gapStart += 2;
    return true;
  }

  public boolean writePosition(AbstractSequence seq, int ipos, Object xpos)
  {
    ensureSpace(7);
    data[gapStart] = POSITION_TRIPLE_FOLLOWS;
    int seq_index = find(seq);
    int xpos_index = xpos == null ? -1 : find(xpos);
    setIntN(gapStart+1, seq_index);
    setIntN(gapStart+3, ipos);
    setIntN(gapStart+5, xpos_index);
    gapStart += 7;
    return true;
  }

  public void writeObject(Object v)
  {
    ensureSpace(3);
    int index = find(v);
    if (index < 0x1000)
      data[gapStart++] = (char) (OBJECT_REF_SHORT | index);
    else
      {
	data[gapStart++] = OBJECT_REF_FOLLOWS;
	setIntN(gapStart, index);
	gapStart += 2;
      }
  }

  public void beginGroup(String typeName, Object type)
  {
    beginGroup(find(typeName, type));
  }

  public void beginDocument()
  {
    ensureSpace(1);
    data[gapStart++] = BEGIN_DOCUMENT;
  }

  public void endDocument()
  {
  }

  public void beginGroup(int index)
  {
    ensureSpace(3 + 7);
    gapEnd -= 7;
    data[gapStart++] = BEGIN_GROUP_LONG;
    setIntN(gapStart, gapEnd - data.length); // end_offset
    gapStart += 2;
    data[gapEnd] = END_GROUP_LONG;
    setIntN(gapEnd + 1, index);  // begin_offset
    setIntN(gapEnd + 3, gapStart - 3);  // begin_offset
    setIntN(gapEnd + 5, currentBeginGroup);  // parent_offset
    currentBeginGroup = gapStart - 3;
  }

  public void endGroup(String typeName)
  {
    if (data[gapEnd] != END_GROUP_LONG)
      throw new Error("unexpected endGroup "+typeName);
    int index = getIntN(gapEnd + 1);
    int begin = getIntN(gapEnd + 3);
    int parent = getIntN(gapEnd + 5);
    gapEnd += 7;
    int offset = gapStart - begin;
    int parentOffset = begin - parent;
    if (index < BEGIN_GROUP_SHORT_INDEX_MAX
	&& offset < 0x10000 && parentOffset < 0x10000)
      {
	data[begin] = (char) (BEGIN_GROUP_SHORT | index);
	data[begin + 1] = (char) offset;  // end_offset
	data[begin + 2] = (char) parentOffset;
	data[gapStart] = END_GROUP_SHORT;
	data[gapStart + 1] = (char) offset; // begin_offset
	gapStart += 2;
      }
    else
      {
	data[begin] = BEGIN_GROUP_LONG;
	setIntN(begin + 1, offset);
	data[gapStart] = END_GROUP_LONG;
	setIntN(gapStart + 1, index);
	setIntN(gapStart + 3, - offset);
	if (parent >= gapStart || begin <= gapStart)
	  parent -= gapStart;
	setIntN(gapStart + 5, parent);
	gapStart += 7;
      }
    currentBeginGroup = parent;
  }

  public void beginAttribute(String attrName, Object attrType)
  {
    beginAttribute(find(attrName, attrType));
  }

  public void beginAttribute(int index)
  {
    ensureSpace(5 + 1);
    gapEnd--;
    data[gapStart++] = BEGIN_ATTRIBUTE_LONG;
    if (attrStart != 0)
      throw new Error("nested attribute");
    attrStart = gapStart;
    setIntN(gapStart, index);
    setIntN(gapStart + 2, gapEnd - data.length);
    gapStart += 4;
    data[gapEnd] = END_ATTRIBUTE;
  }

  public void endAttribute()
  {
    if (data[gapEnd] != END_ATTRIBUTE || attrStart <= 0)
      throw new Error("unexpected endAttribute");
    // Move the END_ATTRIBUTES to before the gap.
    gapEnd++;
    setIntN(attrStart+2, gapStart - attrStart + 1);
    attrStart = 0;
    data[gapStart++] = END_ATTRIBUTE;
  }

  public void writeChar(int i)
  {
    ensureSpace(3);
    if (i <= MAX_CHAR_SHORT)
      data[gapStart++] = (char) i;
    else if (i < 0x10000)
      {
	data[gapStart++] = CHAR_FOLLOWS;
	data[gapStart++] = (char) i;
      }
    else
      {
	data[gapStart++] = CHAR_PAIR_FOLLOWS;
	// write surrogates FIXME.
      }
  }

  public void writeBoolean(boolean v)
  {
    ensureSpace(1);
    data[gapStart++] = v ? BOOL_TRUE : BOOL_FALSE;
  }

  public void writeByte(int v)
  {
    ensureSpace(1);
    data[gapStart++] = (char) (BYTE_PREFIX + (v & 0xFF));
  }

  public void writeInt(int v)
  {
    ensureSpace(3);
    if (v >= MIN_INT_SHORT && v <= MAX_INT_SHORT)
      data[gapStart++] = (char) (INT_SHORT_ZERO + v);
    else
      {
	data[gapStart++] = INT_FOLLOWS;
	setIntN(gapStart, v);
	gapStart += 2;
      }
  }

  public void writeLong(long v)
  {
    ensureSpace(5);
    data[gapStart++] = LONG_FOLLOWS;
    data[gapStart++] = (char) (v >>> 48);
    data[gapStart++] = (char) (v >>> 32);
    data[gapStart++] = (char) (v >>> 16);
    data[gapStart++] = (char) v;
  }

  public void writeFloat(float v)
  {
    ensureSpace(3);
    int i = Float.floatToIntBits(v);
    data[gapStart++] = FLOAT_FOLLOWS;
    data[gapStart++] = (char) (i >>> 16);
    data[gapStart++] = (char) i;
  }

  public void writeDouble(double v)
  {
    ensureSpace(5);
    long l = Double.doubleToLongBits(v);
    data[gapStart++] = DOUBLE_FOLLOWS;
    data[gapStart++] = (char) (l >>> 48);
    data[gapStart++] = (char) (l >>> 32);
    data[gapStart++] = (char) (l >>> 16);
    data[gapStart++] = (char) l;
  }

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

  public void write(char[] buf, int off, int len)
  {
    ensureSpace(len);
    while (len > 0)
      {
	char ch = buf[off++];
	len--;
	if (ch <= MAX_CHAR_SHORT)
	  data[gapStart++] = ch;
	else
	  {
	    writeChar(ch);
	    ensureSpace(len);
	  }
      }
  }

  public boolean isEmpty()
  {
    // FIXME does not work if we allow comment() entries!
    int pos = gapStart == 0 ? gapEnd : 0;
    return pos == data.length;
  }

  public int size()
  {
    int size = 0;
    int i = 0;
    for (;;)
      {
	i = nextDataIndex(i);
	if (i < 0)
	  return size;
	size++;
      }
  }

  protected void
  makePosition(int index, boolean isAfter,
	       PositionContainer posSet, int posNumber)
  {
    int i = 0;
    while (--index >= 0)
      {
	i = nextDataIndex(i);
	if (i < 0)
	  throw new IndexOutOfBoundsException();
      }
    posSet.setPosition(posNumber, (i << 1) | (isAfter ? 1 : 0), null);
    posSet.setSequence(posNumber, this);
  }

  public boolean gotoChildrenStart(TreePosition pos)
  {
    int index = gotoChildrenStart(pos.ipos >> 1);
    if (index < 0)
      return false;
    pos.push(this, index << 1, null);
    return true;
  }

  public int gotoChildrenStart(int index)
  {
    if (index >= gapStart)
      index += gapEnd - gapStart;
    if (index == data.length)
      return -1;
    char datum = data[index];
    if ((datum >= BEGIN_GROUP_SHORT
	 && datum <= BEGIN_GROUP_SHORT+BEGIN_GROUP_SHORT_INDEX_MAX)
	|| datum == BEGIN_GROUP_LONG)
      index += 3;
    else if (datum == BEGIN_DOCUMENT)
      return index + 1;
    else
      return -1;
    for (;;)
      {
	if (index >= gapStart)
	  index += gapEnd - gapStart;
	datum = data[index];
	if (datum == BEGIN_ATTRIBUTE_LONG)
	  {
	    int end = getIntN(index+3);
	    index = end + (end < 0 ? data.length : index);
	  }
	else if (datum == END_ATTRIBUTE)
	  index++;
	else
	  break;
      }
    return index;
  }

  public boolean gotoAttributesStart(TreePosition pos)
  {
    int index = gotoAttributesStart(pos.ipos >> 1);
    if (index < 0)
      return false;
    pos.push(this, index << 1, null);
    return true;
  }

  public int gotoAttributesStart(int index)
  {
    if (index >= gapStart)
      index += gapEnd - gapStart;
    if (index == data.length)
      return -1;
    char datum = data[index];
    if ((datum >= BEGIN_GROUP_SHORT
	 && datum <= BEGIN_GROUP_SHORT+BEGIN_GROUP_SHORT_INDEX_MAX)
	|| datum == BEGIN_GROUP_LONG)
      return index + 3;
    else
      return -1;
  }

  public Object get (int index)
  {
    throw unsupported("get");
  }

  public boolean consumeNext(int ipos, Object xpos, Consumer out)
  {
    if (! hasNext(ipos, xpos))
      return false;
    int start = ipos >> 1;
    int end = nextDataIndex(start);
    if (end >= 0)
      consumeRange(start, end, out);
    return true;
  }

  public int consumeRange(int startPosition, int endPosition, Consumer out)
  {
    int pos = startPosition;
    int limit = startPosition <= gapStart && endPosition > gapStart ? gapStart
      : endPosition;
    int index;
    for (;;)
      {
	if (pos >= limit)
	  {
	    if (pos == gapStart && endPosition > gapEnd)
	      {

		pos = gapEnd;
		limit = endPosition;
	      }
	    else
	      break;
	  }

	char datum = data[pos++];

	if (datum <= MAX_CHAR_SHORT)
	  {
	    int start = pos - 1;
	    int lim = limit;
	    for (;;)
	      {
		if (pos >= lim)
		  break;
		datum = data[pos++];
		if (datum > MAX_CHAR_SHORT)
		  {
		    pos--;
		    break;
		  }
	      }
	    out.write(data, start, pos - start);
	    continue;
	  }
	if (datum >= OBJECT_REF_SHORT
	     && datum <= OBJECT_REF_SHORT+OBJECT_REF_SHORT_INDEX_MAX)
	  {
	    out.writeObject(objects[datum-OBJECT_REF_SHORT]);
	    continue;
	  }
	if (datum >= BEGIN_GROUP_SHORT
	    && datum <= BEGIN_GROUP_SHORT+BEGIN_GROUP_SHORT_INDEX_MAX)
	  {
	    index = datum-BEGIN_GROUP_SHORT;
	    out.beginGroup(objects[index].toString(), objects[index+1]);
	    pos += 2;
	    continue;
	  }
	/*
	if ((datum & 0xFF00) == BYTE_PREFIX)
	  {
	    out.writeByte((byte) datum);
	    continue;
	  }
	*/
	if (datum >= INT_SHORT_ZERO + MIN_INT_SHORT
	    && datum <= INT_SHORT_ZERO + MAX_INT_SHORT)
	  {
	    out.writeInt(datum - INT_SHORT_ZERO);
	    continue;
	  }
	switch (datum)
	  {
	  case BEGIN_DOCUMENT:
	    continue;
	  case BOOL_FALSE:
	  case BOOL_TRUE:
	    out.writeBoolean(datum != BOOL_FALSE);
	    continue;
	  case CHAR_FOLLOWS:
	    out.write(data, pos, 1 + datum - CHAR_FOLLOWS);
	    pos++;
	    continue;
	  case CHAR_PAIR_FOLLOWS:
	    out.write(data, pos, 1 + datum - CHAR_FOLLOWS);
	    pos += 2;
	    continue;
	  case POSITION_TRIPLE_FOLLOWS:
	    {
	      AbstractSequence seq = (AbstractSequence) objects[getIntN(pos)];
	      int ipos = getIntN(pos+2);
	      int xpos_index = getIntN(pos+4);
	      Object xpos = xpos_index < 0 ? null : objects[xpos_index];
	      if (out instanceof PositionConsumer)
		((PositionConsumer) out).writePosition(seq, ipos, xpos);
	      else
		out.writeObject(SeqPosition.make(seq, ipos, xpos));
	      pos += 6;
	    }
	    continue;
	  case POSITION_REF_FOLLOWS:
	    if (out instanceof PositionConsumer)
	      {
		((PositionConsumer) out).consume((TreePosition) objects[getIntN(pos)]);
		pos += 2;
		continue;
	      }
	    // ... else fall through ...
	  case OBJECT_REF_FOLLOWS:
	    out.writeObject(objects[getIntN(pos)]);
	    pos += 2;
	    continue;
	  case END_GROUP_SHORT:
	    index = data[pos++];
	    index = data[pos - 2 - index] - BEGIN_GROUP_SHORT;
	    out.endGroup(objects[index].toString());
	    continue;
	  case BEGIN_GROUP_LONG:
	    index = getIntN(pos);
	    pos += 2;
	    index += index >= 0 ? pos - 1 : data.length;
	    index = getIntN(index + 1);
	    out.beginGroup(objects[index].toString(), objects[index+1]);
	    continue;
	  case END_GROUP_LONG:
	    index = getIntN(pos);
	    out.endGroup(objects[index].toString());
	    pos += 6;
	    continue;
	  case BEGIN_ATTRIBUTE_LONG:
	    index = getIntN(pos);
	    out.beginAttribute(objects[index].toString(), objects[index+1]);
	    pos += 4;
	    continue;
	  case END_ATTRIBUTE:
	    out.endAttribute();
	    continue;
	  case INT_FOLLOWS:
	    writeInt(getIntN(pos));
	    pos += 2;
	    continue;
	  case FLOAT_FOLLOWS:
	    writeFloat(Float.intBitsToFloat(getIntN(pos)));
	    pos += 2;
	    continue;
	  case LONG_FOLLOWS:
	    writeLong(getLongN(pos));
	    pos += 4;
	    continue;
	  case DOUBLE_FOLLOWS:
	    writeDouble(Double.longBitsToDouble(getLongN(pos)));
	    pos += 4;
	    continue;
	  default:
	    dump();
	    throw new Error("unknown code:"+(int) datum);
	  }
      }
    return pos;
  }

  public boolean hasNext(int ipos, Object xpos)
  {
    int index = ipos >>> 1;
    if (index >= gapStart)
      index += gapEnd - gapStart;
    if (index == data.length)
      return false;
    char ch = data[index];
    return ch != END_ATTRIBUTE && ch != END_GROUP_SHORT
      && ch != END_GROUP_LONG;
  }

  public int getNextKind(int ipos, Object xpos)
  {
    int index = ipos >>> 1;
    if (index >= gapStart)
      index += gapEnd - gapStart;
    if (index == data.length)
      return Sequence.EOF_VALUE;
    char datum = data[index];
    if (datum <= MAX_CHAR_SHORT)
      return Sequence.CHAR_VALUE;
    if (datum >= OBJECT_REF_SHORT
	&& datum <= OBJECT_REF_SHORT+OBJECT_REF_SHORT_INDEX_MAX)
      return Sequence.OBJECT_VALUE;
    if (datum >= BEGIN_GROUP_SHORT
	    && datum <= BEGIN_GROUP_SHORT+BEGIN_GROUP_SHORT_INDEX_MAX)
      return Sequence.GROUP_VALUE;
    if ((datum & 0xFF00) == BYTE_PREFIX)
      return Sequence.TEXT_BYTE_VALUE;
    if (datum >= INT_SHORT_ZERO + MIN_INT_SHORT
	&& datum <= INT_SHORT_ZERO + MAX_INT_SHORT)
      return Sequence.INT_S32_VALUE;
    switch (datum)
      {
      case BOOL_FALSE:
      case BOOL_TRUE:
	return Sequence.BOOLEAN_VALUE;
      case INT_FOLLOWS:
	return Sequence.INT_S32_VALUE;
      case LONG_FOLLOWS:
	return Sequence.INT_S64_VALUE;
      case FLOAT_FOLLOWS:
	return Sequence.FLOAT_VALUE;
      case DOUBLE_FOLLOWS:
	return Sequence.DOUBLE_VALUE;
      case CHAR_FOLLOWS:
      case CHAR_PAIR_FOLLOWS:
	return Sequence.CHAR_VALUE;
      case BEGIN_DOCUMENT:
	return Sequence.DOCUMENT_VALUE;
      case BEGIN_GROUP_LONG:
	return Sequence.GROUP_VALUE;
      case END_GROUP_SHORT:
      case END_GROUP_LONG:
      case END_ATTRIBUTE:
	return Sequence.EOF_VALUE;
      case BEGIN_ATTRIBUTE_LONG:
	return Sequence.ATTRIBUTE_VALUE;
      case POSITION_REF_FOLLOWS: // FIXME	
      case POSITION_TRIPLE_FOLLOWS:
      case OBJECT_REF_FOLLOWS:
      default:
	return Sequence.OBJECT_VALUE;
      }

  }

  protected int getNextTypeIndex(int ipos, Object xpos)
  {
    int index = ipos >>> 1;
    if (index >= gapStart)
      index += gapEnd - gapStart;
    if (index == data.length)
      return Sequence.EOF_VALUE;
    char datum = data[index];
    if (datum >= BEGIN_GROUP_SHORT
	&& datum <= BEGIN_GROUP_SHORT+BEGIN_GROUP_SHORT_INDEX_MAX)
      return datum-BEGIN_GROUP_SHORT;
    else if (datum == BEGIN_GROUP_LONG || datum == BEGIN_ATTRIBUTE_LONG)
      return getIntN(index + 1);
    return -1;
  }

  protected String getNextTypeName(int ipos, Object xpos)
  {
    int index = getNextTypeIndex(ipos, xpos);
    return index < 0 ? null : (String) objects[index];
  }

  public Object getNextTypeObject(int ipos, Object xpos)
  {
    int index = getNextTypeIndex(ipos, xpos);
    return index < 0 ? null : objects[index+1];
  }

  public Object getNext(int ipos, Object xpos)
  {
    int index = ipos >>> 1;
    if (index >= gapStart)
      index += gapEnd - gapStart;
    if (index == data.length)
      return Sequence.eofValue;
    char datum = data[index];
    if (datum <= MAX_CHAR_SHORT)
      return Convert.toObject(datum);
    if (datum >= OBJECT_REF_SHORT
	&& datum <= OBJECT_REF_SHORT+OBJECT_REF_SHORT_INDEX_MAX)
      return objects[datum-OBJECT_REF_SHORT];
    if (datum >= BEGIN_GROUP_SHORT
	    && datum <= BEGIN_GROUP_SHORT+BEGIN_GROUP_SHORT_INDEX_MAX)
      return new TreeList(this, index, index + data[index+1] + 2);
    /*
    if ((datum & 0xFF00) == BYTE_PREFIX)
      return Sequence.TEXT_BYTE_VALUE;
    */
    if (datum >= INT_SHORT_ZERO + MIN_INT_SHORT
	&& datum <= INT_SHORT_ZERO + MAX_INT_SHORT)
      return Convert.toObject(datum-INT_SHORT_ZERO);
    switch (datum)
      {
      case BEGIN_DOCUMENT:
	return this;  // ???
      case BOOL_FALSE:
      case BOOL_TRUE:
	return Convert.toObject(datum != BOOL_FALSE);
      case INT_FOLLOWS:
	return Convert.toObject(getIntN(index+1));
      case LONG_FOLLOWS:
	return Convert.toObject(getLongN(index+1));
      case FLOAT_FOLLOWS:
	return Convert.toObject(Float.intBitsToFloat(getIntN(index+1)));
      case DOUBLE_FOLLOWS:
	return Convert.toObject(Double.longBitsToDouble(getLongN(index+1)));
      case CHAR_FOLLOWS:
	return Convert.toObject(data[index+1]);
	/*
      case CHAR_PAIR_FOLLOWS:
	return Sequence.CHAR_VALUE;
	*/
      case BEGIN_ATTRIBUTE_LONG:
	{
	  int end_offset = getIntN(index+3);
	  end_offset += end_offset < 0 ? data.length : index;
	  return new TreeList(this, index, end_offset+1);
	}
      case BEGIN_GROUP_LONG:
	{
	  int end_offset = getIntN(index+1);
	  end_offset += end_offset < 0 ? data.length : index;
	  return new TreeList(this, index, end_offset+7);
	}
      case END_GROUP_SHORT:
      case END_GROUP_LONG:
      case END_ATTRIBUTE:
	return Sequence.eofValue;
      case POSITION_REF_FOLLOWS:
      case OBJECT_REF_FOLLOWS:
	return objects[getIntN(index+1)];
      case POSITION_TRIPLE_FOLLOWS: //FIXME
	AbstractSequence seq = (AbstractSequence) objects[getIntN(index+1)];
	ipos = getIntN(index+3);
	int xpos_index = getIntN(index+5);
	xpos = xpos_index < 0 ? null : objects[xpos_index];
	return SeqPosition.make(seq, ipos, xpos);
      default:
	throw unsupported("getNext, code="+Integer.toHexString(datum));
      }
  }

  public int stringValue(int index, StringBuffer sbuf)
  {
    Object value = null;
    int doChildren = 0;
    if (index >= gapStart)
      index += gapEnd - gapStart;
    if (index == data.length)
      return -1;
    char datum = data[index];
    index++;
    if (datum <= MAX_CHAR_SHORT)
      {
	sbuf.append(datum);
	return index;
      }
    if (datum >= OBJECT_REF_SHORT
	&& datum <= OBJECT_REF_SHORT+OBJECT_REF_SHORT_INDEX_MAX)
      {
	value = objects[datum-OBJECT_REF_SHORT];
      }
    else if (datum >= BEGIN_GROUP_SHORT
	     && datum <= BEGIN_GROUP_SHORT+BEGIN_GROUP_SHORT_INDEX_MAX)
      {
	doChildren = index + 2;
	index = data[index] + index + 1;
      }
    else if ((datum & 0xFF00) == BYTE_PREFIX)
      {
	sbuf.append(datum & 0xFF);
	return index;
      }
    else if (datum >= INT_SHORT_ZERO + MIN_INT_SHORT
	&& datum <= INT_SHORT_ZERO + MAX_INT_SHORT)
      {
	sbuf.append((int) datum - INT_SHORT_ZERO);
	return index;
      }
    else
      {
	switch (datum)
	  {
	  case BOOL_FALSE:
	  case BOOL_TRUE:
	    sbuf.append(datum != BOOL_FALSE);
	    return index;
	  case INT_FOLLOWS:
	    sbuf.append(getIntN(index+1));
	    return index + 2;
	  case LONG_FOLLOWS:
	    sbuf.append(getLongN(index+1));
	    return index + 4;
	  case FLOAT_FOLLOWS:
	    sbuf.append(Float.intBitsToFloat(getIntN(index+1)));
	    return index + 2;
	  case DOUBLE_FOLLOWS:
	    sbuf.append(Double.longBitsToDouble(getLongN(index+1)));
	    return index + 4;
	  case CHAR_FOLLOWS:
	    sbuf.append(data[index+1]);
	    return index + 1;
	  case BEGIN_DOCUMENT:
	    doChildren = 1;
	    index = data.length;
	    break;
	  case BEGIN_GROUP_LONG:
	    doChildren = index + 3;
	    int j = getIntN(index);
	    j += j < 0 ? data.length : index-1;
	    index = j + 7;
	    break;
	  case END_GROUP_SHORT:
	  case END_GROUP_LONG:
	  case END_ATTRIBUTE:
	    return -1;
	  case BEGIN_ATTRIBUTE_LONG:
	    return -1; // FIXME	
	  case POSITION_REF_FOLLOWS:
	  case POSITION_TRIPLE_FOLLOWS:
	  case OBJECT_REF_FOLLOWS:
	  case CHAR_PAIR_FOLLOWS:
	  default:
	    throw new Error("unimplemented");
	  }
      }
    if (value != null)
      sbuf.append(value);
    if (doChildren > 0)
      {
	do
	  {
	    doChildren = stringValue(doChildren, sbuf);
	  }
	while (doChildren >= 0);
      }
    return index;
  }

  protected void makeRelativePosition(int istart, Object xstart,
				      int offset, boolean isAfter,
				      PositionContainer posSet,
				      int posNumber)
  {
    if (offset < 0)
      throw unsupported("backwards makeRelativePostion");
    int pos = istart >>> 1;
    while (--offset >= 0)
      {
	pos = nextDataIndex(pos);
	if (pos < 0)
	  throw new IndexOutOfBoundsException();
      }
    posSet.setPosition(posNumber, (pos << 1) | (isAfter ? 1 : 0), null);
  }

  public int nextDataIndex(int pos)
  {
    if (pos == gapStart)
      pos = gapEnd;
    if (pos == data.length)
      return -1;
    int j;
    char datum = data[pos++];
    if (datum <= MAX_CHAR_SHORT
	|| (datum >= OBJECT_REF_SHORT
	    && datum <= OBJECT_REF_SHORT+OBJECT_REF_SHORT_INDEX_MAX)
	|| (datum >= INT_SHORT_ZERO + MIN_INT_SHORT
	    && datum <= INT_SHORT_ZERO + MAX_INT_SHORT))
      return pos;
    if (datum >= BEGIN_GROUP_SHORT
	&& datum <= BEGIN_GROUP_SHORT+BEGIN_GROUP_SHORT_INDEX_MAX)
      return data[pos] + pos + 1;
    switch (datum)
      {
      case BEGIN_DOCUMENT:
	return data.length;
      case BOOL_FALSE:
      case BOOL_TRUE:
	return pos;
      case CHAR_FOLLOWS:
	return pos + 1;
      case POSITION_REF_FOLLOWS:
      case OBJECT_REF_FOLLOWS:
      case CHAR_PAIR_FOLLOWS:
      case INT_FOLLOWS:
	return pos + 2;
      case POSITION_TRIPLE_FOLLOWS:
	return pos + 6;
      case END_GROUP_SHORT:
      case END_GROUP_LONG:
      case END_ATTRIBUTE:
	return -1;
      case BEGIN_GROUP_LONG:
	j = getIntN(pos);
	j += j < 0 ? data.length : pos-1;
	return  j + 7;
      case BEGIN_ATTRIBUTE_LONG:
	j = getIntN(pos+2);
	j += j < 0 ? data.length : pos-1;
	return j + 1;
      case LONG_FOLLOWS:
      case DOUBLE_FOLLOWS:
	return pos + 4;
      default:
	throw new Error("unknown code:"+(int) datum);
      }
  }

  public void consume(Consumer out)
  {
    consumeRange(0, data.length, out);
  }

  // /* DEBUGGING
  public void dump()
  {
    java.io.PrintWriter out = new java.io.PrintWriter(System.out);
    dump(out);
    out.flush();
  }

  public void dump(java.io.PrintWriter out)
  {
    out.println("TreeList @"+System.identityHashCode(this)
		       + " gapStart:"+gapStart+" gapEnd:"+gapEnd+" length:"+data.length);
    int toskip = 0;
    for (int i = 0;  i < data.length;  i++)
      {
	
	if (i < gapStart || i >= gapEnd)
	  {
	    int j;  long l;
	    int ch = data[i];
	    out.print(""+i+": 0x"+Integer.toHexString(ch)+'='+((short) ch));
	    if (--toskip < 0)
	      {
		if (ch <= MAX_CHAR_SHORT)
		  {
		    if (ch >= ' ' && ch < 127)
		      out.print("='"+((char)ch)+"'");
		    else if (ch=='\n')
		      out.print("='\\n'");
		    else
		      out.print("='\\u"+Integer.toHexString(ch)+"'");
		  }
		else if (ch >= OBJECT_REF_SHORT
			 && ch <= OBJECT_REF_SHORT+OBJECT_REF_SHORT_INDEX_MAX)
		  {
		    ch = ch - OBJECT_REF_SHORT;
		    Object obj = objects[ch];
		    out.print("=Object#"+((int)ch)+'='
			      +obj+':'+obj.getClass().getName());
		  }
		else if (ch >= BEGIN_GROUP_SHORT
			 && ch <= BEGIN_GROUP_SHORT+BEGIN_GROUP_SHORT_INDEX_MAX)
		  {
		    ch = ch - BEGIN_GROUP_SHORT;
		    out.print("=BEGIN_GROUP_SHORT index#"+((int)ch)+"=<"+objects[ch]+'>');
		    toskip = 2;
		  }
		else if (ch >= INT_SHORT_ZERO + MIN_INT_SHORT
			 && ch <= INT_SHORT_ZERO + MAX_INT_SHORT)
		  {
		    out.print("= INT_SHORT:"+(ch-INT_SHORT_ZERO));
		  }
		else
		  {
		    switch (ch)
		      {
		      case INT_FOLLOWS:
			j = getIntN(i+1);
			out.print("=INT_FOLLOWS value:"+j);
			toskip = 2;
			break;
		      case LONG_FOLLOWS:
			l = getLongN(i+1);
			out.print("=LONG_FOLLOWS value:"+l);
			toskip = 4;
			break;
		      case FLOAT_FOLLOWS:
			j = getIntN(i+1);
			out.print("=FLOAT_FOLLOWS value:"
				  +Float.intBitsToFloat(j));
			toskip = 2;
			break;
		      case DOUBLE_FOLLOWS:
			l = getLongN(i+1);
			out.print("=DOUBLE_FOLLOWS value:"
				  +Double.longBitsToDouble(l));
			toskip = 4;
			break;
		      case BEGIN_DOCUMENT:
			out.print("=BEGIN_DOCUMENT");
			break;
		      case BOOL_FALSE: out.print("= false");  break;
		      case BOOL_TRUE:  out.print("= true");  break;
		      case CHAR_FOLLOWS:
			out.print("=CHAR_FOLLOWS"); toskip = 1;  break;
		      case CHAR_PAIR_FOLLOWS:
			out.print("=CHAR_PAIR_FOLLOWS"); toskip = 2;  break;
		      case POSITION_REF_FOLLOWS:
		      case OBJECT_REF_FOLLOWS:
			toskip = 2;  break;
		      case END_GROUP_SHORT:
			out.print("=END_GROUP_SHORT begin:");
			j = i - data[i+1];
			out.print(j);
			j = data[j] - BEGIN_GROUP_SHORT;
			out.print(" -> #"+j+"=<"+objects[j]+'>');
			toskip = 1;  break;
		      case BEGIN_GROUP_LONG:
			j = getIntN(i+1);
			j += j < 0 ? data.length : i;
			out.print("=BEGIN_GROUP_LONG end:"+j);
			j = getIntN(j + 1);
			out.print(" -> #"+j+"=<"+objects[j]+'>');
			toskip = 2;
			break;
		      case END_GROUP_LONG:
			j = getIntN(i+1);
			out.print("=END_GROUP_LONG name:"+j
				  +"=<"+objects[j]+'>');
			j = getIntN(i+3);
			j = j < 0 ? i + j : j;
			out.print(" begin:"+j);
			j = getIntN(i+5);
			j = j < 0 ? i + j : j;
			out.print(" parent:"+j);
			toskip = 6;
			break;
		      case BEGIN_ATTRIBUTE_LONG:
			j = getIntN(i+1);
			out.print("=BEGIN_ATTRIBUTE name:"+j+"="+objects[j]);
			j = getIntN(i+3);
			j += j < 0 ? data.length : i;
			out.print(" end:"+j);
			toskip = 4;
			break;
		      case END_ATTRIBUTE: out.print("=END_ATTRIBUTE"); break;
		      case POSITION_TRIPLE_FOLLOWS:
			out.println("=POSITION_TRIPLE_FOLLOWS");
			toskip = 6;
			/*
			AbstractSequence seq = (AbstractSequence) objects[getIntN(i+1)];
			ipos = getIntN(i+3);
			int xpos_index = getIntN(i+5);
			xpos = xpos_index < 0 ? null : objects[xpos_index];
			*/
			break;
		      }
		  }
	      }
	    out.println();
	    if (false && toskip > 0)
	      {
		i += toskip;
		toskip = 0;
	      }
	  }
      }
  }
  // DEBUGGING */
}
