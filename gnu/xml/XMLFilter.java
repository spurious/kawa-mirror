// Copyright (c) 2001, 2002, 2003, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.*;
import gnu.text.SourceLocator;
import gnu.text.SourceMessages;
import gnu.mapping.Symbol;
import gnu.kawa.xml.MakeText;  // FIXME - bad cross-package dependency.
import gnu.kawa.xml.UntypedAtomic;  // FIXME - bad cross-package dependency.
import gnu.expr.Keyword; // FIXME - bad cross-package dependency.
import gnu.kawa.xml.XDataType; // FIXME - bad cross-package dependency.

/** Fixup XML input events.
 * Handles namespace resolution, and adds "namespace nodes" if needed.
 * Does various error checking.
 * This wrapper should be used when creating a NodeTree,
 * has is done XQuery node constructor expressions.
 * Can also be called directly from XMLParser, in which case we use a slightly
 * lower-level interface where we array char array segments rather than
 * Strings.  This is to avoid duplicate String allocation and interning.
 * The combination XMLParser+XMLFilter+NodeTree makes for a fast and
 * compact way to read an XML file into a DOM.
 * Future: Subsume ConsumeSAXHandler as well.  FIXME
 */

public class XMLFilter implements XConsumer, PositionConsumer
{
  /** This is where we save attributes while processing a begin element.
   * It may be the final output if {@code out instanceof NodeTree}. */
  TreeList tlist;

  /** The specified target Consumer that accepts the output.
   * In contrast, base may be either {@code ==out} or {@code ==tlist}. */
  public Consumer out;

  Consumer base;

  public static final int COPY_NAMESPACES_PRESERVE = 1;
  public static final int COPY_NAMESPACES_INHERIT = 2;
  public transient int copyNamespacesMode = COPY_NAMESPACES_PRESERVE;

  /** A helper stack.
   * This is logically multiple separate stacks, but we combine them into
   * a single array.  While this makes the code a little harder to read,
   * it reduces memory overhead and (more importantly) should improve locality.
   * For each nested document or group there is the saved value of
   * namespaceBindings followed by a either a MappingInfo or Symbol
   * from the emitBeginElement/beginGroup.  This is followed by a MappingInfo
   * or Symbol for each attribute we seen for the current group. */
  Object[] workStack;
  NamespaceBinding namespaceBindings;

  private SourceMessages messages;
  SourceLocator locator;

  public void setSourceLocator (SourceLocator locator)
  { this.locator = locator; }
  public void setMessages (SourceMessages messages)
  { this.messages = messages; }

  /** Twice the number of active beginGroup and beginDocument calls. */
  protected int nesting;

  int previous = 0;
  private static final int SAW_KEYWORD = 2;
  private static final int SAW_WORD = 1;

  /** If {@code stringizingLevel > 0} then stringize rather than copy nodes.
   * It counts the number of nested beginAttributes that are active.
   * (In the future it should also count begun comment and
   * processing-instruction constructors, when those support nesting.)
   */
  protected int stringizingLevel;
  /** Value of stringizingLevel when beginGroup was seen.
   * More specifically, the outer-most beginGroup seen when
   * {@code stringizingLevel > 0}.
   * All output should be supressed if 
   * {@code stringizingLevel > stringizingElementLevel && stringizingElementLevel > 0}.
   * This happens, for example, after this sequence: beginAttribute, beginGroup,
   * beginAttribute.  In this case the inner attribute should be ignored,
   * because it is not port of the string value of the beginGroup.
   */
  protected int stringizingElementLevel;

  // List of indexes in tlist.data of begin of attribute.
  int[] startIndexes = null;

  /** The number of attributes.
   * Zero means we've seen an element start tag.
   * Gets reset to -1 when we no longer in the element header. */
  int attrCount = -1;

  boolean inStartTag;

  /** The local name if currently processing an attribute value. */
  String attrLocalName;
  String attrPrefix;

  /** Non-null if we're processing a namespace declaration attribute.
   * In that case it is the prefix we're defining,
   * or {@code ""} in the case of a default namespace.  */
  String currentNamespacePrefix;

  /** True if namespace declarations should be passed through as attributes.
   * Like SAX2's http://xml.org/features/namespace-prefixes. */
  public boolean namespacePrefixes = false;

  /** Map either lexical-QName or expanded-QName to a MappingInfo.
   * This is conceptually three hash tables merged into a single data structure.
   * (1) when we first see a tag (a QName as a lexical form before namespace
   * resolution), we map the tag String to an preliminary info entry that
   * has a null qname field.
   * (2) After see the namespace declaration, we use the same table and keys,
   * but name the uri and qtype.namespaceNodes also have to match.
   * (3) Used for hash-consing NamespaceBindings.
   */
  MappingInfo[] mappingTable = new MappingInfo[128];
  int mappingTableMask = mappingTable.length - 1;

  boolean mismatchReported;

  public void setLocator (SourceLocator locator)
  {
    this.locator = locator;
  }

  /** Functionally equivalent to
   * {@code new NamespaceBinding(prefix, uri, oldBindings},
   * but uses "hash consing".
   */
  public NamespaceBinding
  findNamespaceBinding (String prefix, String uri, NamespaceBinding oldBindings)
  {
    int hash = uri == null ? 0 : uri.hashCode();
    if (prefix != null)
      hash ^= prefix.hashCode();
    int bucket = hash & mappingTableMask;
    MappingInfo info = mappingTable[bucket];
    for (;; info = info.nextInBucket)
      {
        if (info == null)
          {
            info = new MappingInfo();
            info.nextInBucket = mappingTable[bucket];
            mappingTable[bucket] = info;
            info.tagHash = hash;
            info.prefix = prefix;
            info.local = uri;
            info.uri = uri;
            if (uri == "")
              uri = null;
            NamespaceBinding namespaces
              = new NamespaceBinding(prefix, uri, oldBindings);
            info.namespaces = namespaces;
            return info.namespaces;
          }
        NamespaceBinding namespaces;
        if (info.tagHash == hash
            && info.prefix == prefix
            && (namespaces = info.namespaces) != null
            && namespaces.getNext() == namespaceBindings
            && namespaces.getPrefix() == prefix
            && info.uri == uri)
          {
            return info.namespaces;
          }
      }
  }

  /** Return a MappingInfo containing a match namespaces.
   * Specifically, return a {@code  MappingInfo info} is such that
   * {@code info.namespaces} is equal to
   * {@code new NamespaceBinding(prefix, uri, oldBindings)}, where {@code uri}
   * is {@code new String(uriChars, uriStart, uriLength).intern())}.
   */
  public MappingInfo lookupNamespaceBinding (String prefix,
                                             char[] uriChars,
                                             int uriStart, int uriLength,
                                             int uriHash,
                                             NamespaceBinding oldBindings)
  {
    int hash = prefix == null ? uriHash : prefix.hashCode() ^ uriHash;
    // Search for a matching already-seen NamespaceBinding list.
    // We hash these to so we can share lists that are equal but
    // appear multiple times in the same XML file, as sometimes happens.
    // This not only saves memory, but keeps hash bucket chains short,
    // which is important since we don't resize the table.
    int bucket = hash & mappingTableMask;
    MappingInfo info = mappingTable[bucket];
    for (;; info = info.nextInBucket)
      {
        if (info == null)
          {
            info = new MappingInfo();
            info.nextInBucket = mappingTable[bucket];
            mappingTable[bucket] = info;
            String uri = new String(uriChars, uriStart, uriLength).intern();
            // We re-use the same MappingInfo table that is mainly used
            // for tag lookup, but re-interpreting the meaning of the
            // various fields. Since MappingInfo hashes on prefix^local,
            // we must do the same here.
            info.tagHash = hash;
            info.prefix = prefix;
            info.local = uri;
            info.uri = uri;
            if (uri == "")
              uri = null;
            NamespaceBinding namespaces
              = new NamespaceBinding(prefix, uri, oldBindings);
            info.namespaces = namespaces;
            return info;
          }
        NamespaceBinding namespaces;
        if (info.tagHash == hash
            && info.prefix == prefix
            && (namespaces = info.namespaces) != null
            && namespaces.getNext() == namespaceBindings
            && namespaces.getPrefix() == prefix
            && MappingInfo.equals(info.uri, uriChars, uriStart, uriLength))
          {
            return info;
          }
      }
  }

  public void endAttribute()
  {
    if (attrLocalName == null)
      ;
    else if (previous == SAW_KEYWORD)
      previous = 0;
    else if (--stringizingLevel == 0)
      {
        if (attrLocalName == "id" && attrPrefix == "xml")
          {
            // Need to normalize xml:id attributes.
            int valStart
              = startIndexes[attrCount-1] + TreeList.BEGIN_ATTRIBUTE_LONG_SIZE; 
            int valEnd = tlist.gapStart;
            char[] data = tlist.data;
            for (int i = valStart;  ;  )
              {
                if (i >= valEnd)
                  {
                    // It's normalized.  Nothing to do.
                    break;
                  }
                char datum = data[i++];
                if (((datum & 0xFFFF) > TreeList.MAX_CHAR_SHORT)
                    || datum == '\t' || datum == '\r' || datum == '\n'
                    || (datum == ' ' && (i == valEnd || data[i] == ' ')))
                  {
                    // It's either not normalized, or the value contains
                    // chars values above MAX_CHAR_SHORT or non-chars.
                    // We could try to normalize in place but why bother?
                    // I'm assuming xml:id are going be normalized already.
                    // The exception is characters above MAX_CHAR_SHORT, but
                    // let's defer that until TreeList gets re-written.
                    StringBuffer sbuf = new StringBuffer();
                    tlist.stringValue(valStart, valEnd, sbuf);
                    tlist.gapStart = valStart;
                    tlist.writeChars(XDataType
                                     .replaceWhitespace(sbuf.toString(), true));
                    break;
                  }
              }
          }

        attrLocalName = null;
        attrPrefix = null;
        if (currentNamespacePrefix == null || namespacePrefixes)
          tlist.endAttribute();
        if (currentNamespacePrefix != null)
          {
            // Handle raw namespace attribute from parser.
            int attrStart = startIndexes[attrCount-1]; 
            int uriStart = attrStart;
            int uriEnd = tlist.gapStart;
            int uriLength = uriEnd - uriStart;
            char[] data = tlist.data;

            // Check that the namespace attribute value is plain characters
            // so we an use the in-place buffer.
            // Calculate hash while we're at it.
            int uriHash = 0;
            for (int i = uriStart;  i < uriEnd;  i++)
              {
                char datum = data[i];
                if ((datum & 0xFFFF) > TreeList.MAX_CHAR_SHORT)
                  {
                    StringBuffer sbuf = new StringBuffer();
                    tlist.stringValue(uriStart, uriEnd, sbuf);
                    uriHash = sbuf.hashCode();
                    uriStart = 0;
                    uriEnd = uriLength = sbuf.length();
                    data = new char[sbuf.length()];
                    sbuf.getChars(0, uriEnd, data, 0);
                    break;
                  }
                uriHash = 31 * uriHash + datum;
              }
            tlist.gapStart = attrStart;

            String prefix = currentNamespacePrefix == "" ? null
              : currentNamespacePrefix;
            MappingInfo info
              = lookupNamespaceBinding(prefix, data, uriStart, uriLength,
                                       uriHash, namespaceBindings);
            namespaceBindings = info.namespaces;

            currentNamespacePrefix = null;
          }
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
      {
        error('e', "unknown namespace prefix '" + prefix + '\'');
      }
    return "";
  }

  void closeStartTag ()
  {
    if (attrCount < 0 || stringizingLevel > 0)
      return;
    inStartTag = false;
    previous = 0;

    if (attrLocalName != null) // Should only happen on erroneous input.
      endAttribute();
    NamespaceBinding outer = nesting == 0 ? NamespaceBinding.predefinedXML
      : (NamespaceBinding) workStack[nesting-2];
      
    NamespaceBinding bindings = namespaceBindings;

    // This first pass is to check that there are namespace declarations for
    // each Symbol.
    for (int i = 0;  i <= attrCount; i++)
      {
        Object saved = workStack[nesting+i-1];
        if (saved instanceof Symbol)
          {
            Symbol sym = (Symbol) saved;
            String prefix = sym.getPrefix();
            if (prefix == "")
              prefix = null;
            String uri = sym.getNamespaceURI();
            if (uri == "")
              uri = null;
            if (i > 0 && prefix == null && uri == null)
              continue;
            boolean isOuter = false;
            for (NamespaceBinding ns = bindings; ; ns = ns.next)
              {
                if (ns == outer)
                  isOuter = true;
                if (ns == null)
                  {
                    if (prefix != null || uri != null)
                      bindings = findNamespaceBinding(prefix, uri, bindings);
                    break;
                  }
                if (ns.prefix == prefix)
                  {
                    if (ns.uri != uri)
                      {
                        if (isOuter)
                          bindings = findNamespaceBinding(prefix, uri, bindings);
                        else
                          {
                            // Try to find an alternative existing prefix:
                            String nprefix;
                            for (NamespaceBinding ns2 = bindings;
                                 ; ns2 = ns2.next)
                              {
                                if (ns2 == null)
                                  {
                                    // We have to generate a new prefix.
                                    for (int j = 1;  ; j++)
                                      {
                                        nprefix = ("_ns_"+j).intern();
                                        if (bindings.resolve(nprefix) == null)
                                          break;
                                      }
                                    break;
                                  }
                                if (ns2.uri == uri)
                                  {
                                    nprefix = ns2.prefix;
                                    if (bindings.resolve(nprefix) == uri)
                                      break;
                                  }
                              }
                            bindings = findNamespaceBinding(nprefix, uri, bindings);
                            String local = sym.getLocalName();
                            if (uri == null)
                              uri = "";
                            workStack[nesting+i-1]
                              = Symbol.make(uri, local, nprefix);
                          }
                      }
                    break;
                  }
              }
          }

      }

    for (int i = 0;  i <= attrCount; i++)
      {
        Object saved = workStack[nesting+i-1];
        MappingInfo info;
        boolean isNsNode = false;
        String prefix, uri, local;
        if (saved instanceof MappingInfo || out == tlist)
          {
            if (saved instanceof MappingInfo)
              {
                info = (MappingInfo) saved;
                prefix = info.prefix;
                local = info.local;
                if (i > 0
                    && ((prefix == null && local == "xmlns")
                        || prefix == "xmlns"))
                  {
                    isNsNode = true;
                    uri = "(namespace-node)";
                  }
                else
                  uri = resolve(prefix, i > 0);
              }
            else
              {
                Symbol symbol = (Symbol) saved;
                info = lookupTag(symbol);
                prefix = info.prefix;
                local = info.local;
                uri = symbol.getNamespaceURI();
              }
            int hash = info.tagHash;
            int bucket = hash & mappingTableMask;

            info = mappingTable[bucket];
            MappingInfo tagMatch = null;
            Object type;
            for (;; info = info.nextInBucket)
              {
                if (info == null)
                  {
                    info = tagMatch;
                    info = new MappingInfo();
                    info.tagHash = hash;
                    info.prefix = prefix;
                    info.local = local;
                    info.nextInBucket = mappingTable[bucket];
                    mappingTable[bucket] = info;
                    info.uri = uri;
                    info.qname = Symbol.make(uri, local, prefix);
                    if (i == 0)
                      {
                        XName xname = new XName(info.qname, bindings);
                        type = xname;
                        info.type = xname;
                        info.namespaces = bindings;
                      }
                    break;
                  }
                if (info.tagHash == hash
                    && info.local == local
                    && info.prefix == prefix)
                  {
                    if (info.uri == null)
                      {
                        info.uri = uri;
                        info.qname = Symbol.make(uri, local, prefix);
                      }
                    else if (info.uri != uri)
                      continue;
                    else if (info.qname == null)
                      info.qname = Symbol.make(uri, local, prefix);
                    if (i == 0)
                      {
                        if (info.namespaces == bindings
                            || info.namespaces == null)
                          {
                            type = info.type;
                            info.namespaces = bindings;
                            if (type == null)
                              {
                                XName xname = new XName(info.qname, bindings);
                                type = xname;
                                info.type = xname;
                              }
                            break;
                          }
                      }
                    else
                      {
                        type = info.qname;
                        break;
                      }
                  }
              }
            workStack[nesting+i-1] = info;
          }
        else
          {
            Symbol sym = (Symbol) saved;
            uri = sym.getNamespaceURI();
            local = sym.getLocalName();
            info = null;
          }

        // Check for duplicated attribute names.
        for (int j = 1;  j < i;  j++)
          {
            Object other = workStack[nesting+j-1];
            Symbol osym;
            if (other instanceof Symbol)
              osym = (Symbol) other;
            else if (other instanceof MappingInfo)
              osym = ((MappingInfo) other).qname;
            else
              continue;
            if (local == osym.getLocalPart()
                && uri == osym.getNamespaceURI())
              {
                Object tag = workStack[nesting-1];
                if (tag instanceof MappingInfo)
                  tag = ((MappingInfo) tag).qname;
                error('e', XMLFilter.duplicateAttributeMessage(osym, tag));
              }
          }

	if (out == tlist)
	  {
            Object type = i == 0 ? info.type : info.qname;
	    int index = info.index;
	    if (index <= 0
		|| tlist.objects[index] != type)
	      {
		index = tlist.find(type);
		info.index = index;
	      }
	    if (i == 0)
              tlist.setGroupName(tlist.gapEnd, index);
	    else if (! isNsNode || namespacePrefixes)
              tlist.setAttributeName(startIndexes[i-1], index);
	  }
	else
	  {
            Object type = info == null ? saved
              : i == 0 ? info.type : info.qname;
	    if (i == 0)
	      out.beginGroup(type);
	    else if (! isNsNode || namespacePrefixes)
	      {
		out.beginAttribute(type);
		int start = startIndexes[i-1];
		int end = i < attrCount ? startIndexes[i] : tlist.gapStart;
		tlist.consumeIRange(start + TreeList.BEGIN_ATTRIBUTE_LONG_SIZE,
                                    end - TreeList.END_ATTRIBUTE_SIZE,
                                    out);
		out.endAttribute();
	      }
	  }
      }
    for (int i = 1;  i <= attrCount; i++)
      workStack[nesting+i-1] = null; // For GC.
    if (out != tlist)
      {
	base = out;
	// Remove temporarily stored attributes.
	tlist.clear();
      }
    attrCount = -1;
  }

  protected boolean checkWriteAtomic ()
  {
    previous = 0;
    if (stringizingLevel > stringizingElementLevel
        && stringizingElementLevel > 0)
      return false;
    closeStartTag();
    return true;
 }

  public void writeChar(int v)
  {
    if (checkWriteAtomic())
      base.writeChar(v);
  }

  public void writeBoolean (boolean v)
  {
    if (checkWriteAtomic())
      base.writeBoolean(v);
  }

  public void writeFloat (float v)
  {
    if (checkWriteAtomic())
      base.writeFloat(v);
  }

  public void writeDouble (double v)
  {
    if (checkWriteAtomic())
      base.writeDouble(v);
  }

  public void writeInt(int v)
  {
    if (checkWriteAtomic())
      base.writeInt(v);
  }

  public void writeLong (long v)
  {
    if (checkWriteAtomic())
      base.writeLong(v);
  }

  public void writeDocumentUri (Object uri)
  {
    if (nesting == 2 && base instanceof TreeList)
      ((TreeList) base).writeDocumentUri(uri);
  }

  public void consume (SeqPosition position)
  {
    writePosition(position.sequence, position.ipos);
  }

  public void writePosition(AbstractSequence seq, int ipos)
  {
    if (stringizingLevel > stringizingElementLevel
        && stringizingElementLevel > 0)
      return;
    seq.consumeNext(ipos, this);
  }

  /** If v is a node, make a copy of it. */
  public void writeObject(Object v)
  {
    if (stringizingLevel > stringizingElementLevel
        && stringizingElementLevel > 0)
      return;
    if (v instanceof SeqPosition)
      {
	SeqPosition pos = (SeqPosition) v;
	writePosition(pos.sequence, pos.getPos());
      }
    else if (v instanceof TreeList)
      ((TreeList) v).consume(this);
    else if (v instanceof Keyword)
      {
        Keyword k = (Keyword) v;
        beginAttribute(k.asSymbol());
        previous = SAW_KEYWORD;
      }
    else
      {
        closeStartTag();
        if (v instanceof UnescapedData)
          {
            base.writeObject(v);
            previous = 0;
          }
        else
          {
            if (previous == SAW_WORD)
              writeChar(' ');
            MakeText.text$C(v, this);  // Atomize.
            previous = SAW_WORD;
          }
      }
  }

  public XMLFilter (Consumer out)
  {
    this.base = out;
    this.out = out;
    if (out instanceof NodeTree)
      this.tlist = (NodeTree) out;
    else
      tlist = new TreeList(); // just for temporary storage

    namespaceBindings = NamespaceBinding.predefinedXML;
  }

  /** Process raw text. */
  public void write (char[] data, int start, int length)
  {
    // Skip whitespace not in an element.
    // This works semi-accidentally, since XMLParser doesn't call beginDocument
    // which otherwise would increment nesting.  Perhaps shipping toplevel
    // whitespace should be handled internally in XMLParser.  FIXME.
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
    if (length == 0)
      writeJoiner();
    else if (checkWriteAtomic())
      base.write(data, start, length);
  }

  public void writeChars(String v)
  {
    if (v.length() == 0)
      writeJoiner();
    else if (checkWriteAtomic())
      base.writeChars(v);
  }

  protected void writeJoiner ()
  {
    previous = 0;
    if ((stringizingLevel <= stringizingElementLevel
         || stringizingElementLevel == 0)
         && base instanceof TreeList)
      ((TreeList) base).writeJoiner();
  }

  /** Process a CDATA section.
   * The data (starting at start for length char).
   * Does not include the delimiters (i.e. {@code "<![CDATA["}
   * and {@code "]]>"} are excluded). */
  public void writeCDATA(char[] data, int start, int length)
  {
    if (checkWriteAtomic())
      {
        if (base instanceof XConsumer)
          ((XConsumer) base).writeCDATA(data, start, length);
        else
          write(data, start, length);
      }
  }

  protected void beginGroupCommon ()
  {
    closeStartTag();
    if (stringizingLevel == 0)
      {
        ensureSpaceInWorkStack(nesting);
        workStack[nesting] = namespaceBindings;
        tlist.beginGroup(0);
        base = tlist;
        attrCount = 0;
      }
    else
      {
        if (previous == SAW_WORD)
          writeChar(' ');
        previous = 0;
        if (stringizingElementLevel == 0)
          stringizingElementLevel = stringizingLevel;
        else
          stringizingLevel++;
      }
    nesting += 2;
  }

  /** Process a start tag, with the given element name. */
  public void emitBeginElement(char[] data, int start, int count)
  { 
    closeStartTag();
    MappingInfo info = lookupTag(data, start, count);
    beginGroupCommon();
    ensureSpaceInWorkStack(nesting-1);
    workStack[nesting-1] = info;
  }

  public void beginGroup(Object type)
  {
    beginGroupCommon();
    if (stringizingLevel == 0)
      {
        ensureSpaceInWorkStack(nesting-1);
        workStack[nesting-1] = type;
        if (copyNamespacesMode == 0)
          namespaceBindings = NamespaceBinding.predefinedXML;
        else if (copyNamespacesMode == COPY_NAMESPACES_PRESERVE)
          namespaceBindings
            = (type instanceof XName ? ((XName) type).getNamespaceNodes()
               : NamespaceBinding.predefinedXML);
        else
          {
            NamespaceBinding inherited
              = (NamespaceBinding) workStack[nesting-2];
            if (copyNamespacesMode == COPY_NAMESPACES_INHERIT)
              namespaceBindings = inherited;
            else if (type instanceof XName)
              {
                NamespaceBinding preserved = ((XName) type).getNamespaceNodes();
                NamespaceBinding join = NamespaceBinding.commonAncestor(inherited, preserved);
                if (join == inherited)
                  namespaceBindings = preserved;
                else
                  namespaceBindings = mergeHelper(inherited, preserved);
              }
            else
              namespaceBindings = inherited;
          }
      }
  }

  private NamespaceBinding mergeHelper (NamespaceBinding list,
                                        NamespaceBinding node)
  {
    if (node == NamespaceBinding.predefinedXML)
      return list;
    list = mergeHelper(list, node.next);
    String uri = node.uri;
    if (list == null)
      {
	if (uri == null)
	  return list;
	list = NamespaceBinding.predefinedXML;
      }
    String prefix = node.prefix;
    String found = list.resolve(prefix);
    if (found == null ? uri == null : found.equals(uri))
      return list;
    return findNamespaceBinding(prefix, uri, list);
  }

  private boolean beginAttributeCommon()
  {
    if (stringizingLevel++ > 0)
      return false;

    if (attrCount < 0) // A disembodied attribute.
      attrCount = 0; 
    ensureSpaceInWorkStack(nesting+attrCount);
    ensureSpaceInStartIndexes(attrCount);
    startIndexes[attrCount] = tlist.gapStart;
    attrCount++;
    return true;
  }

  public void beginAttribute (Object attrType)
  {
    previous = 0;
    if (attrType instanceof Symbol)
      {
        Symbol sym = (Symbol) attrType;
        String local = sym.getLocalPart();
        attrLocalName = local;
        attrPrefix = sym.getPrefix();
        String uri = sym.getNamespaceURI();
        if (uri == "http://www.w3.org/2000/xmlns/"
            || (uri == "" && local == "xmlns"))
          error('e', "arttribute name cannot be 'xmlns' or in xmlns namespace");
      }
    if (nesting == 2 && workStack[1] == null)
      error('e', "attribute not allowed at document level");
    if (attrCount < 0 && nesting > 0)
      error('e', "attribute '"+attrType+"' follows non-attribute content");
    if (! beginAttributeCommon())
      return;
    workStack[nesting+attrCount-1] = attrType;
    if (nesting == 0)
      base.beginAttribute(attrType);
    else
      tlist.beginAttribute(0);
  }

  /** Process an attribute, with the given attribute name.
   * The attribute value is given using {@code write}.
   * The value is terminated by either another emitBeginAttribute
   * or an emitEndAttributes.
   */
  public void emitBeginAttribute(char[] data, int start, int count)
  {
    if (attrLocalName != null)
      endAttribute();
    if (! beginAttributeCommon())
      return;

    MappingInfo info = lookupTag(data, start, count);
    workStack[nesting+attrCount-1] = info;
    String prefix = info.prefix;
    String local = info.local;
    attrLocalName = local;
    attrPrefix = prefix;
    if (prefix != null)
      {
	if (prefix == "xmlns")
	  {
            currentNamespacePrefix = local;
	  }
      }
    else
      {
	if (local == "xmlns" && prefix == null)
	  {
            currentNamespacePrefix = "";
	  }
      }
    if (currentNamespacePrefix == null || namespacePrefixes)
      tlist.beginAttribute(0);
  }

  /** Process the end of a start tag.
   * There are no more attributes. */
  public void emitEndAttributes()
  {
    if (attrLocalName != null)
      endAttribute();
  }

  /** Process an end tag.
   * An abbreviated tag (such as {@code '<br/>'}) has a name==null.
   */
  public void emitEndElement(char[] data, int start, int length)
  {
    if (attrLocalName != null)
      {
	error('e', "unclosed attribute"); // FIXME
	endAttribute();
      }
    if (nesting == 0)
      {
	error('e', "unmatched end element"); // FIXME
	return;
      }
    if (data != null)
      {
        MappingInfo info = lookupTag(data, start, length);
        Object old = workStack[nesting-1];
        if (old instanceof MappingInfo && ! mismatchReported)
          {
            MappingInfo mold = (MappingInfo) old;
            if (info.local != mold.local || info.prefix != mold.prefix)
              {
                StringBuffer sbuf = new StringBuffer("</");
                sbuf.append(data, start, length);
                sbuf.append("> matching <");
                String oldPrefix = mold.prefix;
                if (oldPrefix != null)
                  {
                    sbuf.append(oldPrefix);
                    sbuf.append(':');
                  }
                sbuf.append(mold.local);
                sbuf.append('>');
                error('e', sbuf.toString());
                mismatchReported = true;
              }
          }
      }
    closeStartTag();
    if (nesting <= 0)
      return; // Only if error.
    endGroup();
  }

  public void endGroup ()
  {
    closeStartTag();
    nesting -= 2;
    if (stringizingLevel == 0)
      {
        namespaceBindings = (NamespaceBinding) workStack[nesting];
        workStack[nesting] = null;
        workStack[nesting+1] = null;
        base.endGroup();
      }
    else
      {
        previous = SAW_WORD;
        if (stringizingElementLevel > 0)
          {
            if (stringizingElementLevel == stringizingLevel)
              stringizingElementLevel = 0;
            else
              stringizingLevel--;
          }
      }
    /*
    if (nesting == 0)
      {
        workStack = null;
        attrIndexes = null;
      }
    */
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

  protected void checkValidComment (char[] chars, int offset, int length)
  {
    int i = length;
    boolean sawHyphen = true;
    while (--i >= 0)
      {
        boolean curHyphen = chars[offset+i] == '-';
        if (sawHyphen && curHyphen)
          {
            error('e', "consecutive or final hyphen in XML comment");
            break;
          }
        sawHyphen = curHyphen;
      }
  }

  /** Process a comment.
   * The data (starting at start for length chars).
   * Does not include the delimiters (i.e. "<!--" and "-->" are excluded). */
  public void writeComment (char[] chars, int start, int length)
  {
    checkValidComment(chars, start, length);
    commentFromParser(chars, start, length);
  }

  /** Process a comment, when called from an XML parser.
   * The data (starting at start for length chars).
   * Does not include the delimiters (i.e. "<!--" and "-->" are excluded). */
  public void commentFromParser (char[] chars, int start, int length)
  {
    if (stringizingLevel == 0)
      {
        closeStartTag();
        if (base instanceof XConsumer)
          ((XConsumer) base).writeComment(chars, start, length);
      }
    else if (stringizingLevel < stringizingElementLevel
             || stringizingElementLevel == 0)
      base.write(chars, start, length);
  }

  public void writeProcessingInstruction(String target, char[] content,
					 int offset, int length)
  {
    for (int i = offset+length;  --i >= offset; )
      {
        char ch = content[i];
        while (ch == '>' && --i >= offset)
          {
            ch = content[i];
            if (ch == '?')
              {
                error('e', "'?>' is not allowed in a processing-instruction");
                break;
              }
          }
      }

    if ("xml".equalsIgnoreCase(target))
      error('e',
            "processing-instruction target may not be 'xml' (ignoring case)");
    if (! XName.isName(target, true))
      error('e',
            "processing-instruction target '"+target+"' is not a valid Name");

    processingInstructionCommon(target, content, offset, length);
  }

  void processingInstructionCommon (String target, char[] content,
                                    int offset, int length)
  {
    if (stringizingLevel == 0)
      {
        closeStartTag();
        if (base instanceof XConsumer)
          ((XConsumer) base)
            .writeProcessingInstruction(target, content, offset, length);
      }
    else if (stringizingLevel < stringizingElementLevel
             || stringizingElementLevel == 0)
      base.write(content, offset, length);
  }

  /** Process a processing instruction. */
  public void processingInstructionFromParser(char[] buffer,
                                        int tstart, int tlength,
                                        int dstart, int dlength)
  {
    // Skip XML declaration.
    if (nesting == 0 && tlength == 3
        && buffer[tstart] == 'x'
        && buffer[tstart+1] == 'm'
        && buffer[tstart+2] == 'l')
      return;
    String target = new String(buffer, tstart, tlength);
    processingInstructionCommon(target, buffer, dstart, dlength);
  }

  public void beginDocument()
  {
    closeStartTag();
    if (stringizingLevel > 0)
      writeJoiner();
    // We need to increment nesting so that endDocument can decrement it.
    else
      {
        if (nesting == 0)
          base.beginDocument();
        else
          writeJoiner();
        ensureSpaceInWorkStack(nesting);
        workStack[nesting] = namespaceBindings;
        // The following should be redundant, but just in case ...
        // Having workStack[nesting+1] be null identifies that nesting
        // level as being a document rather than an element.
        workStack[nesting+1] = null;
        nesting += 2;
      }
  }

  public void endDocument ()
  {
    if (stringizingLevel > 0)
      {
        writeJoiner();
        return;
      }
    nesting -= 2;
    namespaceBindings = (NamespaceBinding) workStack[nesting];
    workStack[nesting] = null;
    workStack[nesting+1] = null;
    if (nesting == 0)
      base.endDocument();
    else
      writeJoiner();
    /*
    if (nesting == 0)
      {
        workStack = null;
        attrIndexes = null;
      }
    */
  }

  /** Process a DOCTYPE declaration. */
  public void emitDoctypeDecl(char[] buffer,
                              int target, int tlength,
                              int data, int dlength)
  {
    // FIXME?
  }

  public void beginEntity (Object baseUri)
  {
    if (base instanceof XConsumer)
      ((XConsumer) base).beginEntity(baseUri);
  }

  public void endEntity ()
  {
    if (base instanceof XConsumer)
      ((XConsumer) base).endEntity();
  }

  MappingInfo lookupTag (Symbol qname)
  {
    String local = qname.getLocalPart();
    String prefix = qname.getPrefix();
    if (prefix == "")
      prefix = null;
    String uri = qname.getNamespaceURI();
    int hash = MappingInfo.hash(prefix, local);
    int index = hash & mappingTableMask;
    MappingInfo first = mappingTable[index];
    MappingInfo info = first;
    for (;;)
      {
	if (info == null)
	  {
	    // No match found - create a new MappingInfo and Strings.
	    info = new MappingInfo();
            info.qname = qname;
            info.prefix = prefix;
            info.uri = uri;
            info.local = local;
	    info.tagHash = hash;
	    info.nextInBucket = first;
	    mappingTable[index] = first;
	    return info;
	  }
        if (qname == info.qname)
          return info;
        if (local == info.local && info.qname == null
            && (uri == info.uri || info.uri == null)
            && prefix == info.prefix)
          {
            info.uri = uri;
            info.qname = qname;
            return info;
          }
	info = info.nextInBucket;
      }
  }

  /** Look up an attribute/element tag (a QName as a lexical string
   * before namespace resolution), and return a MappingInfo with the
   * tagHash, prefix, and local fields set.
   * The trick is to avoid allocating a new String for each element or
   * attribute node we see, but only allocate a new String when we see a
   * tag we haven't seen.  So we calculate the hash code using the
   * characters in the array, rather than using String's hashCode.
   */
  MappingInfo lookupTag (char[] data, int start, int length)
  {
    // Calculate hash code.  Also note presence+position of ':'.
    int hash = 0;
    int prefixHash = 0;
    int colon = -1;
    for (int i = 0;  i < length;  i++)
      {
        char ch = data[start+i];
        if (ch == ':' && colon < 0)
          {
            colon = i;
            prefixHash = hash;
            hash = 0;
          }
        else
          hash = 31 * hash + ch;
      }
    hash = prefixHash ^ hash;
    int index = hash & mappingTableMask;
    MappingInfo first = mappingTable[index];
    MappingInfo info = first;
    for (;;)
      {
	if (info == null)
	  {
	    // No match found - create a new MappingInfo and Strings.
	    info = new MappingInfo();
	    info.tagHash = hash;
	    if (colon >= 0)
	      {
		info.prefix = new String(data, start, colon).intern();
                colon++;
                int lstart = start+colon;
		info.local = new String(data, lstart, length-colon).intern();
	      }
	    else
	      {
		info.prefix = null;
		info.local = new String(data, start, length).intern();
	      }
	    info.nextInBucket = first;
	    mappingTable[index] = first;
	    return info;
	  }
	if (hash == info.tagHash
	    && info.match(data, start, length))
	  return info;
	info = info.nextInBucket;
      }
  }

  private void ensureSpaceInWorkStack (int oldSize)
  {
    if (workStack == null)
      {
        workStack = new Object[20];
      }
    else if (oldSize >= workStack.length)
      {
        Object[] tmpn = new Object[2*workStack.length];
        System.arraycopy(workStack, 0, tmpn, 0, oldSize);
        workStack = tmpn;
      }
  }

  private void ensureSpaceInStartIndexes (int oldSize)
  {
    if (startIndexes == null)
      {
        startIndexes = new int[20];
      }
    else if (oldSize >= startIndexes.length)
      {
        int[] tmpn = new int[2*startIndexes.length];
        System.arraycopy(startIndexes, 0, tmpn, 0, oldSize);
        startIndexes = tmpn;
      }
  }

  public static String
  duplicateAttributeMessage (Symbol attrSymbol, Object groupName)
  {
    StringBuffer sbuf = new StringBuffer("duplicate attribute: ");
    String uri = attrSymbol.getNamespaceURI();
    if (uri != null && uri.length() > 0)
      {
        sbuf.append('{');
        sbuf.append('}');
        sbuf.append(uri);
      }
    sbuf.append(attrSymbol.getLocalPart());
    if (groupName != null)
      {
        sbuf.append(" in <");
        sbuf.append(groupName);
        sbuf.append('>');
      }
    return sbuf.toString();
  }

  public void error(char severity, String message)
  {
    if (messages == null)
      throw new RuntimeException(message);
    else if (locator != null)
      messages.error(severity, locator, message);
    else
      messages.error(severity, message);
  }

  public boolean ignoring()
  {
    return false;
  }
}

final class MappingInfo
{
  /** Next in same hash bucket. */
  MappingInfo nextInBucket;

  // maybe future: MappingInfo prevInBucket;
  // maybe future: MappingInfo nextForPrefix;

  /** The cached value of {@code hash(prefix, local)}. */
  int tagHash;

  /** The prefix part of tag: - the part before the colon.
   * It is null if there is no colon in tag.  Otherwise it is interned.  */
  String prefix;

  /** The local name part of tag: - the part after the colon. 
   * It is interned.  */
  String local;

  /** The namespace URI.
   * The value null means "unknown". */
  String uri;

  /** The Symbol for the resolved QName.
   * If non-null, it must be the case that {@code uri!= null}, and
   * {@code qname==Symbol.make(uri, local, prefix==null?"":prefix)}.
   */
  Symbol qname;

  NamespaceBinding namespaces;

  /** An XName matching the other fields.
   * If non-null, we must have {@code qname!=null}, {@code namespaces!=null},
   * {@code type.namespaceNodes == namespaces}, and
   * {@code type.equals(qname)}. */
  XName type;

  /** If non-negative: An index into a TreeList objects array. */
  int index = -1;

  static int hash (String prefix, String local)
  {
    int hash = local.hashCode();
    if (prefix != null)
      hash ^= prefix.hashCode();
    return hash;
  }

  /** Hash a QName, handling an optional prefix+colon. */
  static int hash (char[] data, int start, int length)
  {
    int hash = 0;
    int prefixHash = 0;
    int colonPos = -1;
    for (int i = 0;  i < length;  i++)
      {
        char ch = data[start+i];
        if (ch == ':' && colonPos < 0)
          {
            colonPos = i;
            prefixHash = hash;
            hash = 0;
          }
        else
          hash = 31 * hash + ch;
      }
    return prefixHash ^ hash;
  }

  /** Match {@code "[prefix:]length"} against {@code new String(data, start, next)}. */
  boolean match (char[] data, int start, int length)
  {
    if (prefix != null)
      {
        int localLength = local.length();
        int prefixLength = prefix.length();
        return length == prefixLength + 1 + localLength
          && data[prefixLength] == ':'
          && equals(prefix, data, start, prefixLength)
          && equals(local, data, start+prefixLength+1, localLength);
      }
    else
      return equals(local, data, start, length);
  }

  /** An optimization of {@code sbuf.toString().equals(tag)}.
  */
  static boolean equals (String tag, StringBuffer sbuf)
  {
    int length = sbuf.length();
    if (tag.length () != length)
      return false;
    for (int i = 0;  i < length;  i++)
      if (sbuf.charAt(i) != tag.charAt(i))
	return false;
    return true;
  }

  static boolean equals (String tag, char[] data, int start, int length)
  {
    if (tag.length () != length)
      return false;
    for (int i = 0;  i < length;  i++)
      if (data[start+i] != tag.charAt(i))
	return false;
    return true;
  }
}