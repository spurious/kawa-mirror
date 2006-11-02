 // Copyright (c) 2001, 2002, 2003, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.Consumer;
import gnu.lists.XConsumer;
import gnu.lists.TreeList;
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

  // For each beginGroup seen (and not yet seen the endGroup),
  // the value of namespaceBindings at the beginGroup.
  NamespaceBinding[] namespaceBindingsAtBeginGroup = new NamespaceBinding[20];

  /** Non-null if we're processing a nameapace declaration attribute.
   * In that case it is the prefix we defining,
   * or {@code ""} in the case of a default namespace.  */
  String currentNamespacePrefix;

  /** Number of beginGroups seen without close endGroup. */
  int nesting;

  /** Used if we need to save attribute values of namespace attributes. */
  StringBuffer stringValue = new StringBuffer();

  /** True if namespace declarations should be passed through as attributes.
   * Like SAX2's http://xml.org/features/namespace-prefixes. */
  public boolean namespacePrefixes = false;

  /** Map either lexical-QName or expanded-QName to a MappingInfo.
   * This is conceptually two hash tables merged into a single data structure.
   * (1) when we first see a tag (a QName as a lexical form before namespace
   * resolution), we map the tag String to an preliminary info entry that
   * has a null qname field.  These are entered into the pendingNames table.
   * (2) After see the namespace declaration, we use the same table and keys,
   * but name the uri and qtype.namespaceNodes also have to match.
   */
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
    if (currentNamespacePrefix == null || namespacePrefixes)
      tlist.endAttribute();
    if (currentNamespacePrefix != null)
      {
        int hash = stringValue.hashCode();

	int bucket = hash & mappingTableMask;
	MappingInfo info = mappingTable[bucket];
        String prefix = currentNamespacePrefix == "" ? null
          : currentNamespacePrefix;
        // Search for a matching already-seen NamespaceBinding list.
        // We hash these to so we can share lists that are equal but
        // appear multiple times in the same XML file, as sometimes happens.
        // This not only saves memory, but keeps hash bucket chains short,
        // which is important since we don't resize the table.

        for (;; info = info.nextInBucket)
          {
            if (info == null)
              {
                info = new MappingInfo();
                info.nextInBucket = mappingTable[bucket];
                mappingTable[bucket] = info;
                String uri = stringValue.toString().intern();
                // It seems best to hash on the namespace uri, since it is more
                // likely t be unique than a shorter prefix.  We re-use the
                // same MappingInfo table that is mainly used for tag lookup,
                // but re-interpreting the meaning of the various fields.
                // Since MappingInfo hashes on the 'tag', we store the uri
                // in that field.
                info.tagHash = hash;
                info.prefix = "";
                info.local = uri;
                info.tag = uri;
                info.uri = uri;
                // We don't actually use this Symbol, but if closeStartTag
                // should come upon this MappingInfo it should be consistent.
                Symbol sym = Symbol.make(uri, uri, "");
                if (uri == "")
                  uri = null;
                NamespaceBinding namespaceNodes
                  = new NamespaceBinding(prefix, uri, namespaceBindings);
                info.type = new XName(sym, namespaceNodes);
                break;
              }
            XName type = info.type;
            if (info.tagHash == hash
                && type != null)
              {
                NamespaceBinding namespaceNodes = type.namespaceNodes;
                if (namespaceNodes != null
                    && namespaceNodes.getNext() == namespaceBindings
                    && namespaceNodes.getPrefix() == prefix
                    && info.match(stringValue))
                  {
                    break;
                  }
              }
          }
        namespaceBindings = info.type.namespaceNodes;

        stringValue.setLength(0);
	currentNamespacePrefix = null;
      }
  }

  private String resolve(String prefix, boolean isAttribute)
  {
    if (isAttribute && prefix == null)
      return "";
    String uri = namespaceBindings.resolve(prefix);
    if (uri != null)
      return uri;
    if (prefix != null)
      parser.error('e', "unknown namespace prefix '" + prefix + '\'');
    return "";
  }

  NamespaceBinding namespaceBindings;

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
	MappingInfo tagMatch = null;
	XName type;
	for (;;)
	  {
	    if (info == null)
	      {
		info = tagMatch;
		if (tagMatch != null)
		  {
		    // A mappingTable entry created by lookupTag.
		    // Might as well make it do double duty here as well.
		  }
		else
		  {
		    info = new MappingInfo();
		    info.tag = name;
		    info.tagHash = hash;
		    info.prefix = prefix;
		    info.local = local;
		    info.nextInBucket = mappingTable[bucket];
		    mappingTable[bucket] = info;
		  }
		info.uri = uri;
		info.qname = Symbol.make(uri, local, prefix);
		type = new XName(info.qname,
				 namespaceBindings);
		info.type = type;
		break;
	      }
	    if (info.tag == name)
	      {
		type = info.type;
		if (info.qname == null)
		  tagMatch = info;
		else if (info.uri == uri
			 && type.namespaceNodes == namespaceBindings)
		  break;
	      }
	    info = info.nextInBucket;
	  }
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
              tlist.setGroupName(tlist.gapEnd, index);
	    else if (! isNsNode || namespacePrefixes)
              tlist.setAttributeName(startIndexes[i], index);
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
		tlist.consumeIRange(start + TreeList.BEGIN_ATTRIBUTE_LONG_SIZE,
                                    end - TreeList.END_ATTRIBUTE_SIZE,
                                    cons);
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
    else if (tlist instanceof NodeTree)
      ((NodeTree) tlist).closeTag();
    attrCount = 0;
  }

  private void writeChar(int v)
  {
    closeStartTag();
    if (currentNamespacePrefix != null)
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
    if (currentNamespacePrefix != null)
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
    if (currentNamespacePrefix != null)
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
    if (currentNamespacePrefix != null)
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
    if (currentNamespacePrefix != null)
      stringValue.append(v);
    base.writeInt(v);
  }

  public void writeLong(long v)
  {
    closeStartTag();
    if (currentNamespacePrefix != null)
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
    if (currentNamespacePrefix != null)
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

    namespaceBindings = NamespaceBinding.predefinedXML;
  }

  /** Process raw text. */
  public void emitCharacters(char[] data, int start, int length)
  {
    // Skip whitespace not in an element.
    if (nesting == 0)
      {
        for (int i = 0; ; i++)
          {
            if (i == length)
              return;
            if (! Character.isWhitespace(data[start+i]))
              break;
          }
      }
    closeStartTag();
    if (currentNamespacePrefix != null)
      {
	stringValue.append(data, start, length);
	if (! namespacePrefixes)
	  return;
      }
    base.write(data, start, length);
  }

  /** Process a CDATA section.
   * The data (starting at start for length char).
   * Does not include the delimiters (i.e. {@code "<![CDATA["}
   * and {@code "]]>"} are excluded). */
  public void emitCDATA(char[] data, int start, int length)
  {
    closeStartTag();
    if (base instanceof XConsumer)
      ((XConsumer) base).writeCDATA(data, start, length);
    else
      emitCharacters(data, start, length);
  }

  /** Process a start tag, with the given element name. */
  public void emitBeginElement(char[] data, int start, int count)
  {
    MappingInfo info = lookupTag(data, start, count);
    String name = info.tag;
    closeStartTag();
    if (nesting >= namespaceBindingsAtBeginGroup.length)
      {
	NamespaceBinding[] tmp = new NamespaceBinding[2 * nesting];
	System.arraycopy(namespaceBindingsAtBeginGroup, 0, tmp, 0, nesting);
	namespaceBindingsAtBeginGroup = tmp;
      }
    namespaceBindingsAtBeginGroup[nesting++] = namespaceBindings;
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

  /** Process an attribute, with the given attribute name.
   * The attribute value is given using emitCharacters.
   * The value is terminated by either another emitBeginAttribute
   * or an emitEndAttributes.
   */
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
    pendingNames[attrCount] = info;
    startIndexes[attrCount] = tlist.gapStart;
    String prefix = info.prefix;
    String local = info.local;
    if (prefix != null)
      {
	if (prefix == "xmlns")
	  {
            currentNamespacePrefix = local;
	  }
      }
    else
      {
	if (name == "xmlns")
	  {
            currentNamespacePrefix = "";
	  }
      }
    if (currentNamespacePrefix == null || namespacePrefixes)
      tlist.beginAttribute(0);
    inAttribute = true;
  }

  /** Process the end of a start tag.
   * There are no more attributes. */
  public void emitEndAttributes()
  {
    if (inAttribute)
      endAttribute();
  }

  /** Process an end tag.
   * An abbreviated tag (such as {@code '<br/>'}) has a name==null.
   */
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
    if (data != null && ! MappingInfo.match(old, data, start, length))
      {
	if (! mismatchReported && parser != null)
	  {
	    mismatchReported = true;
	    int nlen = length + 3;
	    parser.pos -= nlen;
	    StringBuffer sbuf = new StringBuffer("</");
	    sbuf.append(data, start, length);
	    sbuf.append("> matching <");
	    sbuf.append(old);
	    sbuf.append('>');
	    parser.error('e', sbuf.toString());
	    parser.pos += nlen;
	  }
      }

    if (depth > 0) // Sanity check, in case of error.
      {
	names[depth-1] = null;  // For the sake of Gc.
	depth--;
      }
    closeStartTag();
    if (nesting <= 0)
      return; // Only if error.
    namespaceBindings = namespaceBindingsAtBeginGroup[--nesting];
    base.endGroup(old);
  }

  /** Process an entity reference.
   * The entity name is given.
   * This handles the predefined entities, such as "&lt;" and "&quot;".
   */
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

  /** Process a character entity reference.
   * The string encoding of the character (e.g. "xFF" or "255") is given,
   * as well as the character value. */
  public void emitCharacterReference(int value, char[] name, int start, int length)
  {
    writeChar(value);
  }

  /** Process a comment.
   * The data (starting at start for length chars).
   * Does not include the delimiters (i.e. "<!--" and "-->" are excluded). */
  public void emitComment(char[] data, int start, int length)
  {
    closeStartTag();
    if (base instanceof XConsumer)
      ((XConsumer) base).writeComment(data, start, length);
  }

  /** Process a processing instruction. */
  public void emitProcessingInstruction(char[] buffer,
                                        int tstart, int tlength,
                                        int dstart, int dlength)
  {
    // Skip XML declaration.
    if (nesting == 0 && tlength == 3
        && buffer[tstart] == 'x'
        && buffer[tstart+1] == 'm'
        && buffer[tstart+2] == 'l')
      return;
    closeStartTag();
    if (base instanceof XConsumer)
      {
	String target = new String(buffer, tstart, tlength);
	((XConsumer) base).writeProcessingInstruction(target,
						      buffer, dstart, dlength);
      }
  }

  /** Process a DOCTYPE declaration. */
  public void emitDoctypeDecl(char[] buffer,
                              int target, int tlength,
                              int data, int dlength)
  {
    // FIXME?
  }

  /** Calculate a hashCode for a string of characters in a char array.
   * Equivalent to <code>new String(data, start, length).hashCode()</code>
   * but more efficient as we don't have to allocate the String.
   */
  static int hash (char[] data, int start, int length)
  {
    int hash = 0;
    for (int i = 0;  i < length;  i++)
      hash = 31 * hash + data[start+i];
    return hash;
  }

  /** Look up an attribute/element tag (a QName as a lexical string
   * before namespace resolution), and return a MappingInfo with the
   * tag, tagHash, prefix, and local fields set.
   * The trick is to avoid allocating a new String for each element or
   * attribute node we see, but only allocate a new String when we see a
   * tag we haven't seen.  So we calculate the hash code using the
   * characters in the array, rather than using String's hashCode.
   */
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
	    // No match found - create a new MappingInfo and Strings.
	    info = new MappingInfo();
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

  /** The source (unresolved) QName as it appears in the XML file.
   * This String is interned. */
  String tag;

  /** The hashCode of tag. */
  int tagHash;

  /** The prefix part of tag: - the part before the colon.
   * It is null if there is no colon in tag.  Otherwise it is interned.  */
  String prefix;

  /** The local name part of tag: - the part after the colon. 
   * It is the same as tag if there is no colon in tag.
   * Either way it is interned.  */
  String local;

  /** The namespace URI. */
  String uri;

  /** The Symbol/type for the resolved QName. */
  Symbol qname;

  XName type;

  /** If non-negative: An index into a TreeList objects array. */
  int index = -1;

  /** An optimization of {@code new String(data, start, next).equals(tag)}. */
  boolean match (char[] data, int start, int length)
  {
    return match(tag, data, start, length);
  }

  /** An optimization of {@code sbug.toString().equals(tag)}. */
  boolean match (StringBuffer sbuf)
  {
    int length = sbuf.length();
    if (tag.length () != length)
      return false;
    for (int i = 0;  i < length;  i++)
      if (sbuf.charAt(i) != tag.charAt(i))
	return false;
    return true;
  }

  static boolean match (String tag, char[] data, int start, int length)
  {
    if (tag.length () != length)
      return false;
    for (int i = 0;  i < length;  i++)
      if (data[start+i] != tag.charAt(i))
	return false;
    return true;
  }

  public void error(XMLParserChar parser, String message)
  {
    StringBuffer sbuf = new StringBuffer();
    error(parser, message, sbuf);
    System.err.println(sbuf);
  }

  public static void error(XMLParserChar parser, String message, StringBuffer sbuf)
  {
    /*
    String name = port.getName();
    if (name != null)
      sbuf.append(name);
    */
    int line = 1;
    int lstart = 0;
    int i = 0;
    int pos = parser.pos;
    char[] buffer = parser.buffer;
    while (i < pos)
      {
        char ch = buffer[i++];
        if (ch == '\n')
          {
            line++;
            lstart = i;
          }
        else if (ch == '\r')
          {
            if (i < pos && buffer[i] == '\n')
              i++;
            line++;
            lstart = i;
          }
      }
    sbuf.append(':');
    sbuf.append(line);
    int column = pos - lstart;
    sbuf.append(':');
    sbuf.append(column + 1);
    sbuf.append(": ");

    sbuf.append("xml parse error");
    if (message != null)
      {
        sbuf.append(" - ");
        sbuf.append(message);
      }
  }
}
