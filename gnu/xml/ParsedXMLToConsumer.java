// Copyright (c) 2001, 2002, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.Consumer;
import gnu.lists.TreeList;
import java.net.URL;
import gnu.mapping.Symbol;
import gnu.mapping.Symbol;

public class ParsedXMLToConsumer extends ParsedXMLHandler
{
  Consumer base;

  /** Stack of entered but not exited element tag names. */
  String[] names = new String[10];
  /** Active length of names stack. */
  int depth = 0;

  XMLParserChar parser;

  // List of unresolcved MappingInfos for begin tag and each attribute.
  // Used while processing a single start tag.
  // We always have pendingNames.length == startIndexes.length.
  MappingInfo[] pendingNames = new MappingInfo[10];

  // List of indexes in tlist.data of begin of group and attribute.
  int[] startIndexes = new int[10];

  // Number of attributes seen for the current start element.
  int attrCount;

  boolean inStartTag;

  /** True if currently processing an attribute value.. */
  boolean inAttribute;

  // This is where we save attributes while processing a begin element.
  // It may be the final output if cons instanceof NodeTree.
  TreeList tlist;

  // The specified target Consumer that accepts the output.
  // In contrast, base may be either ==cons or ==tlist.
  Consumer cons;

  // Stack of prefix->uri pairs that are currently active.
  String[] namespaceStack;

  // Length of active part of namespaceStack.
  // This is twice the number of namespace declarations seen (including
  // the predefined one for "xml"), since we push both the prefix and the uri.
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

  MappingInfo[] mappingTable = new MappingInfo[128];
  int mappingTableMask = mappingTable.length - 1;

  boolean mismatchReported;

  public void setParser (XMLParserChar parser)
  {
    this.parser = parser;
  }

  private void endAttribute()
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

  private String resolve(String prefix, boolean isAttribute)
  {
    if (isAttribute && prefix == null)
      return "";
    for (int i = namespaceStackLength;  (i -= 2) >= 0;  )
      {
	String nsPrefix = namespaceStack[i];
	if (nsPrefix == prefix
	    || (prefix == null && nsPrefix == null))
	  return namespaceStack[i+1];
      }
    if (prefix != null)
      parser.error('e', "unknwon namespace prefix '" + prefix + '\'');
    return "";
  }

  void closeStartTag ()
  {
    if (! inStartTag || inAttribute)
      return;
    inStartTag = false;

    for (int i = 0;  i <= attrCount; i++)
      {
	MappingInfo info = pendingNames[i];
	String name = info.tag;
	String prefix = info.prefix;
	String local = info.local;
	boolean isNsNode = i > 0 && ( name == "xmlns" || prefix == "xmlns");
	String uri = isNsNode ? "(namespace-node)" : resolve(prefix, i > 0);
	int hash = info.tagHash;
	int bucket = hash & mappingTableMask;
	info = mappingTable[bucket];
	for (;;)
	  {
	    if (info == null)
	      {
		if (pendingNames[i].qname == null)
		    info = pendingNames[i];
		else
		  {
		    info = new MappingInfo();
		    info.tag = name;
		    info.tagHash = hash;
		    info.prefix = prefix;
		    info.local = info.local;
		    info.nextInBucket = mappingTable[bucket];
		    mappingTable[bucket] = info;
		  }
		info.uri = uri;
		info.qname = Symbol.make(uri, local);
		break;
	      }
	    if (info.tag == name
		&& info.uri == uri
		&& info.qname != null)
	      break;
	    info = info.nextInBucket;
	  }
	Object type = info.qname;
	if (cons == tlist)
	  {
	    int index = info.index;
	    if (info.index <= 0
		|| tlist.objects[index] != name
		|| tlist.objects[index+1] != type)
	      {
		index = tlist.find(name, type);
		info.index = index;
	      }
	    if (i == 0)
	      tlist.setIntN(tlist.gapEnd + 1, index);
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
		// Kludge:  Hard-wired in the size of BEGIN_ATTRIBUTE_LONG
		// and END_ATTRIBUTE from TreeList.  
		tlist.consumeIRange(start + 5, end - 1, cons);
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

  private void writeChar(int v)
  {
    closeStartTag();
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
    closeStartTag();
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
    closeStartTag();
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
    base.writeInt(v);
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
    base.writeLong(v);
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

  public ParsedXMLToConsumer(Consumer out)
  {
    this.base = out;
    this.cons = out;
    if (cons instanceof NodeTree)
      this.tlist = (NodeTree) out;
    else
      tlist = new TreeList(); // just for temporary storage

    namespaceStack = new String[10];
    namespaceStack[0] = "xml";
    namespaceStack[1] = "http://www.w3.org/XML/1998/namespace";
    namespaceStackLength = 2;
  }

  public void emitCharacters(char[] data, int start, int length)
  {
    closeStartTag();
    if (stringValue != null)
      {
	stringValue.append(data, start, length);
	if (! namespacePrefixes)
	  return;
      }
    base.write(data, start, length);
  }

  public void emitBeginElement(char[] data, int start, int count)
  {
    MappingInfo info = lookupTag(data, start, count);
    String name = info.tag;
    closeStartTag();
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

    attrCount = 0;
    pendingNames[0] = info;
    if (depth >= names.length)
      {
	String[] tmp = new String[2 * depth];
	System.arraycopy(names, 0, tmp, 0, depth);
	names = tmp;
      }
    names[depth++] = name;
  }

  public void emitBeginAttribute(char[] data, int start, int count)
  {
    MappingInfo info = lookupTag(data, start, count);
    String name = info.tag;
    if (inAttribute)
      endAttribute();
    attrCount++;
    if (attrCount >= startIndexes.length)
      {
	MappingInfo[] tmp = new MappingInfo[2 * pendingNames.length];
	System.arraycopy(pendingNames, 0, tmp, 0, pendingNames.length);
	pendingNames = tmp;
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
    pendingNames[attrCount] = info;
    startIndexes[attrCount] = tlist.gapStart;
    String prefix = info.prefix;
    String local = info.local;
    if (prefix != null)
      {
	if (prefix == "xmlns")
	  {
	    namespaceStack[namespaceStackLength] = local;
	    stringValue = new StringBuffer(100);
	  }
      }
    else
      {
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

  public void emitEndAttributes()
  {
    if (inAttribute)
      endAttribute();
  }

  public void emitEndElement(char[] data, int start, int length)
  {
    if (inAttribute)
      {
	parser.error('e', "unclosed attribute"); // FIXME
	endAttribute();
      }
    if (depth == 0)
      {
	parser.error('e', "unmatched end element"); // FIXME
	return;
      }
    String old = names[depth-1];
    String name = (data == null ? old
		   : new String(data, start, length));
    if (data != null && ! name.equals(old))
      {
	if (! mismatchReported && parser != null)
	  {
	    mismatchReported = true;
	    int nlen = length + 3;
	    parser.pos -= nlen;
	    parser.error('e', "</" + name+"> matching <"+old+">");
	    parser.pos += nlen;
	  }
      }

    if (depth > 0) // Sanity check, in case of error.
      {
	names[depth-1] = null;  // For the sake of Gc.
	depth--;
      }
    String typeName = name;
    closeStartTag();
    if (nesting <= 0)
      return; // Only if error.
    nesting--;
    namespaceStackLength = namespaceLengthStack[nesting];
    base.endGroup(typeName);
  }

  /** Handles the predefined entities, such as "&lt;" and "&quot;". */
  public void emitEntityReference(char[] name, int start, int length)
  {
    char c0 = name[start];
    char ch = '?';
    if (length == 2 && name[start+1] == 't')
      {
	
	if (c0 == 'l')
	  ch = '<';
	else if (c0 == 'g')
	  ch = '>';
      }
    else if (length == 3)
      {
	if (c0 == 'a' && name[start+1] == 'm' && name[start+2] == 'p')
	  ch = '&';
      }
    else if (length == 4)
      {
	char c1 = name[start+1];
	char c2 = name[start+2];
	char c3 = name[start+3];
	if (c0 == 'q' && c1 == 'u' && c2 == 'o' && c3 == 't')
	  ch = '"';
	else if (c0 == 'a' && c1 == 'p' && c2 == 'o' && c3 == 's')
	  ch = '\'';
      }
    writeChar(ch);
  }

  public void emitCharacterReference(int value, char[] name, int start, int length)
  {
    writeChar(value);
  }

  public void emitComment(char[] data, int start, int length)
  {
    // FIXME?
  }

  /** Process a processing incluction. */
  public void emitProcessingInstruction(char[] buffer,
                                        int target, int tlength,
                                        int data, int dlength)
  {
    // FIXME?
  }

  /** Process a DOCTYPE declaration. */
  public void emitDoctypeDecl(char[] buffer,
                              int target, int tlength,
                              int data, int dlength)
  {
    // FIXME?
  }

  static int hash (char[] data, int start, int length)
  {
    int hash = 0;
    for (int i = 0;  i < length;  i++)
      hash = 31 * hash + data[start+i];
    return hash;
  }

  MappingInfo lookupTag (char[] data, int start, int length)
  {
    int hash = hash(data, start, length);
    int index = hash & mappingTableMask;
    MappingInfo first = mappingTable[index];
    MappingInfo info = first;
    for (;;)
      {
	if (info == null)
	  {
	    info = new MappingInfo();
	    // Should we intern() ?
	    String tag = new String(data, start, length).intern();
	    info.tag = tag;
	    info.tagHash = hash;
	    info.nextInBucket = first;
	    int colon = tag.indexOf(':');
	    if (colon > 0)
	      {
		info.prefix = tag.substring(0, colon).intern();
		info.local = tag.substring(colon+1).intern();
	      }
	    else
	      {
		info.prefix = null;
		info.local = tag;
	      }
	    mappingTable[index] = first;
	    return info;
	  }
	if (hash == info.tagHash
	    && info.match(data, start, length))
	  return info;
	info = info.nextInBucket;
      }
  }

  /*
  private void popNamespaces ()
  {
    for (each namespace to be popped)
      {
	MappingInfo mapping = mapping_for_namespace(...);
	for (; mapping != null;  mapping = mapping.nextForPrefix)
	  {
	    // Unlink mapping from hash bucket:
	    MappingInfo next = mapping.nextInBucket;
	    MappingInfo prev = mapping.prevInBucket;
	    if (next != null)
	      next.prevInBucket = prev;
	    if (prev != null)
	      prev.nextInBucket = next;
	    else
	      mappingTable[hashIndex] = next;
	  }
      }
  }
  */
}

final class MappingInfo
{
  /** Next in same hash bucket. */
  MappingInfo nextInBucket;

  // maybe future: MappingInfo prevInBucket;
  // maybe future: MappingInfo nextForPrefix;

  /** The source (unresolved) QName as it appears in the XML file. */
  String tag;

  /** The hashCode of tag. */
  int tagHash;

  /** The prefix part of tag: - the part before the colon.
   * It is null if there is no colon in tag. */
  String prefix;

  /** The local name part of tag: - the part after the colon. 
   * It is the same as tag if there is no colon in tag. */
  String local;

  /** The namespace URI. */
  String uri;

  /** The Symbol/type for the resolved QName. */
  Object qname;

  /** If non-negative: An index into a TreeList objects array. */
  int index = -1;

  /** An optization of 'new String(data, start, next).equals(tag)'. */
  boolean match (char[] data, int start, int length)
  {
    if (tag.length () != length)
      return false;
    for (int i = 0;  i < length;  i++)
      if (data[start+i] != tag.charAt(i))
	return false;
    return true;
  }
}
