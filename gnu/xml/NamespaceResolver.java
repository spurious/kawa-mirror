// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.*;

/** Process namespace attributes as received from an XML parser. */

public class NamespaceResolver extends FilterConsumer
{
  // List of (name, prefix, local) for begin and each attribute.
  // Used while processing a single start tag.
  // We always have nameStack.length == 3 * startIndexes.length.
  String[] nameStack = new String[30];

  // List of indexes in tlist.data of begin of group and attribute.
  int[] startIndexes = new int[10];

  // Number of attributes seen for the current start element.
  int attrCount;

  boolean inStartTag;

  /** True if currently processing an atribute value.. */
  boolean inAttribute;

  // This is where save attributes while processing a begin element.
  // It may be the final output if cons instancedof TreeList.
  TreeList tlist;

  // The specified target Consumer that accepts the output.
  // In contrast, super.base may be either ==cons or ==tlist.
  Consumer cons;

  // Stack of prefix->uri pairs that are currently active.
  String[] namespaceStack;

  // Length of active part of namespaceStack.
  // This is twice the number of namespace declarations seem (including
  // the predefined one for "xml"), since we push but the prefix and the uri.
  int namespaceStackLength;

  // For each beginGroup seen (and not yet seen the endGroup),
  // the value of namespaceStackLength at the beginGroup.
  int[] namespaceLengthStack = new int[10];

  /** Number of beginGroups seen without close endGroup. */
  int nesting;

  /** Used if we need to save attribute values of namespace attributes. */
  StringBuffer stringValue;

  /** True if namespace declarations should be passed through.
   * Like SAX2's http://xml.org/features/namespace-prefixes. */
  public boolean namespacePrefixes = false;

  public NamespaceResolver(Consumer cons)
  {
    super(cons);
    this.cons = cons;
    if (cons instanceof TreeList)
      this.tlist = (TreeList) cons;
    else
      tlist = new TreeList(); // just for temporary storage

    namespaceStack = new String[10];
    namespaceStack[0] = "xml";
    namespaceStack[1] = "http://www.w3.org/XML/1998/namespace";
    namespaceStackLength = 2;
  }

  public void beginGroup(String name, Object type)
  {
    if (nesting >= namespaceLengthStack.length)
      {
	int[] tmp = new int[2 * nesting];
	System.arraycopy(namespaceLengthStack, 0, tmp, 0, nesting);
	namespaceLengthStack = tmp;
      }
    namespaceLengthStack[nesting] = namespaceStackLength;
    nesting++;
    inStartTag = true;

    startIndexes[0] = tlist.gapStart;
    tlist.beginGroup(0);
    base = tlist;

    name = name.intern();
    attrCount = 0;
    nameStack[0] = name;
    int colon = name.indexOf(':');
    if (colon > 0)
      {
	nameStack[1] = name.substring(0, colon).intern();
	nameStack[2] = name.substring(colon+1).intern();
      }
    else
      {
	nameStack[1] = null;
	nameStack[2] = name;
      }
  }

  public void endAttribute()
  {
    inAttribute = false;
    if (stringValue == null || namespacePrefixes)
      tlist.endAttribute();
    if (stringValue != null)
      {
	String uri = stringValue.toString();
	uri = uri.length() == 0 ? null : uri.intern();
	namespaceStack[namespaceStackLength + 1] = uri;
	namespaceStackLength += 2;
	stringValue = null;
      }
  }

  public void beginAttribute(String name, Object attrType)
  {
    if (attrCount >= startIndexes.length)
      {
	String[] tmp = new String[2 * nameStack.length];
	System.arraycopy(nameStack, 0, tmp, 0, nameStack.length);
	nameStack = tmp;
	int[] itmp = new int[2 * startIndexes.length];
	System.arraycopy(startIndexes, 0, itmp, 0, attrCount);
	startIndexes = itmp;
      }
    if (namespaceStackLength >= namespaceStack.length)
      {
	String[] tmp = new String[2 * namespaceStack.length];
	System.arraycopy(namespaceStack, 0, tmp, 0, namespaceStackLength);
	namespaceStack = tmp;
      }
    name = name.intern();
    int colon = name.indexOf(':');
    attrCount++;
    nameStack[3 * attrCount] = name;
    startIndexes[attrCount] = tlist.gapStart;
    if (colon > 0)
      {
	String prefix = name.substring(0, colon).intern();
	String local = name.substring(colon+1).intern();
	nameStack[3 * attrCount + 1] = prefix;
	nameStack[3 * attrCount + 2] = local;
	if (prefix == "xmlns")
	  {
	    namespaceStack[namespaceStackLength] = local;
	    stringValue = new StringBuffer(100);
	  }
      }
    else
      {
	nameStack[3 * attrCount + 1] = null;
	nameStack[3 * attrCount + 2] = name;
	if (name == "xmlns")
	  {
	    namespaceStack[namespaceStackLength] = null;
	    stringValue = new StringBuffer(100);
	  }
      }
    if (stringValue == null || namespacePrefixes)
      tlist.beginAttribute(0);
    inAttribute = true;
  }

  private String resolve(String prefix)
  {
    for (int i = namespaceStackLength;  (i -= 2) >= 0;  )
      {
	String nsPrefix = namespaceStack[i];
	if (nsPrefix == prefix
	    || (prefix == null && nsPrefix == null))
	  return namespaceStack[i+1];
      }
    return null; // ??
  }

  int indexHashMask = (1 << 5) - 1;
  int[] indexHash = new int[indexHashMask+1];
  int indexHashCount;
  int nextIndex;

  int getIndex(String name, Object type)
  {
    int hash = name == null ? 0 : name.hashCode();
    int i = hash & indexHashMask;
    Object[] objects = tlist.objects;
    int step = (hash << 1) | 1;
    for (;;)
      {
	int index = indexHash[i];
	if (index > 0)
	  {
	    if (objects[index-1] == name && objects[index] == type)
	      return index-1;
	    i = (i + step) & indexHashMask;
	  }
	else
	  {
	    index = nextIndex;
	    if (index >= objects.length)
	      {
		tlist.resizeObjects();
		objects = tlist.objects;
	      }
	    objects[index] = name;
	    objects[index + 1] = type;
	    indexHash[i] = index + 1;
	    nextIndex += 2;
	    indexHashCount += 2;
	    int avail2 = 2 * indexHash.length;
	    if (3 * indexHashCount >= avail2)
	      { // rehash when 2/3 full
		int[] old = indexHash;
		indexHash = new int[avail2];
		indexHashMask = avail2 - 1;
		for (i = old.length;  --i >= 0; )
		  {
		    int j = old[i];
		    if (j > 0)
		      getIndex((String) objects[j-1], objects[j]);
		  }
	      }
	    return index;
	  }
      }
  }

  void closeStartTag ()
  {
    if (! inStartTag || inAttribute)
      return;
    inStartTag = false;
    endAttributes();
  }

  protected void endAttributes()
  {
    for (int i = 0;  i <= attrCount; i++)
      {
	String name = nameStack[3 * i];
	String prefix = nameStack[3 * i + 1];
	String local = nameStack[3 * i + 2];
	boolean isNsNode = name == "xmlns" || prefix == "xmlns";
	String uri = isNsNode ? "(namespace-node)" : resolve(prefix);
	Object type = QName.make(uri, local);
	if (cons == tlist)
	  {
	    int index = getIndex(name, type);
	    if (i == 0)
	      tlist.setIntN(tlist.gapEnd + 2, index);
	    else if (! isNsNode || namespacePrefixes)
	      tlist.setIntN(startIndexes[i] + 1, index);
	  }
	else
	  {
	    if (i == 0)
	      cons.beginGroup(name, type);
	    else if (! isNsNode || namespacePrefixes)
	      {
		cons.beginAttribute(name, type);
		int start = startIndexes[i];
		int end = i < attrCount ? startIndexes[i+1] : tlist.gapStart;
		tlist.consumeRange(start, end, cons);
		cons.endAttribute();
	      }
	  }
      }
    if (cons != tlist)
      {
	base = cons;
	// Remove temporarily stored attributes.
	tlist.clear();
      }
    attrCount = 0;
  }

  public void endGroup(String typeName)
  {
    nesting--;
    namespaceStackLength = namespaceLengthStack[nesting];
    base.endGroup(typeName);
  }

  public void writeChar(int v)
  {
    if (stringValue != null)
      {
	stringValue.append((char) v);
	if (! namespacePrefixes)
	  return;
      }
    base.writeChar(v);
  }

  public void writeBoolean(boolean v)
  {
    if (stringValue != null)
      {
	stringValue.append(v);
	if (! namespacePrefixes)
	  return;
      }
    base.writeBoolean(v);
  }

  public void writeFloat(float v)
  {
    if (stringValue != null)
      {
	stringValue.append(v);
	if (! namespacePrefixes)
	  return;
      }
    base.writeFloat(v);
  }

  public void writeDouble(double v)
  {
    closeStartTag();
    if (stringValue != null)
      {
	stringValue.append(v);
	if (! namespacePrefixes)
	  return;
      }
    base.writeDouble(v);
  }

  public void writeInt(int v)
  {
    closeStartTag();
    if (stringValue != null)
      stringValue.append(v);
    writeInt(v);
  }

  public void writeLong(long v)
  {
    closeStartTag();
    if (stringValue != null)
      {
	stringValue.append(v);
	if (! namespacePrefixes)
	  return;
      }
    writeLong(v);
  }

  public void writeObject(Object v)
  {
    closeStartTag();
    if (stringValue != null)
      {
	stringValue.append(v);
	if (! namespacePrefixes)
	  return;
      }
    base.writeObject(v);
  }

  public void writeChars(String str)
  {
    closeStartTag();
    if (stringValue != null)
      {
	stringValue.append(str);
	if (! namespacePrefixes)
	  return;
      }
    base.writeChars(str);
  }

  public void write(char[] buf, int off, int len)
  {
    closeStartTag();
    if (stringValue != null)
      {
	stringValue.append(buf, off, len);
	if (! namespacePrefixes)
	  return;
      }
    base.write(buf, off, len);
  }
}
