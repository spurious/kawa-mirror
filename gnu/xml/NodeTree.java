// Copyright (c) 2003, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.text.URI_utils;
import gnu.kawa.xml.KNode;
import gnu.xml.XName;
import gnu.kawa.xml.MakeText;  // FIXME - bad cross-package dependency.
import gnu.kawa.xml.UntypedAtomic;  // FIXME - bad cross-package dependency.
import gnu.kawa.xml.XDataType; // FIXME - bad cross-package dependency.
import gnu.kawa.xml.ElementType; // FIXME - bad cross-package dependency.
import gnu.expr.Keyword; // FIXME - bad cross-package dependency.

/** Use to represent a Document or Document Fragment, in the XML DOM sense.
 * More compact than traditional DOM, since it uses many fewer objects.
 */

public class NodeTree extends TreeList
{
  // A lot of these fields are only used while the tree is being constructed.
  // They should idealy be moved out to a separate class, perhaps NodeFilter.
  // However, we want to avoid allocating a NodeFilter each time we create
  // a NodeTree, so perhaps NodeFilter could also be a front-end for
  // Nodes.  Unclear what is the most efficient general solution.  Probably
  // similar to what XMLFilter does - and maybe combine those.
  // FIXME. For now these fields are marked as transient.

  /** Number of active beginGroup and beginDocument calls. */
  transient protected int groupLevel;

  transient NamespaceBinding groupNamespaces;

  transient int gapStartTag;
  transient int[] attrIndexes;

  /** A helper stack. The first groupLevel items are NamespaceBinding
   * objects for the active groups.  Then there are 2*attrCount items -
   * a name and type for the current group and each attribute. */
  transient Object[] workStack;

  /** One more than the number of attributes. */
  transient int attrCount;
  transient boolean inAttribute;

  public static final int COPY_NAMESPACES_PRESERVE = 1;
  public static final int COPY_NAMESPACES_INHERIT = 2;
  public transient int copyNamespacesMode = COPY_NAMESPACES_PRESERVE;

  /** If {@code stringizingLevel > 0} then stringize rather than copy nodes.
   * If couns the number of nested beginAttributes that are active.
   * (In the future it should also count begun comment and
   * processing-instruction constructors, when those support nesting.)
   */
  transient protected int stringizingLevel;
  /** Value of stringizingLevel when beginGroup was seen.
   * More specifically, the outer-most beginGroup seen when
   * {@code stringizingLevel > 0}.
   * All output should be supressed if 
   * {@code stringizingLevel > stringizingElementLevel && stringizingElementLevel > 0}.
   * This happens, for example, after this sequence: beginAttribute, beginGroup,
   * beginAttribute.  In this case the inner attribute should be ignored,
   * because it is not port of the string value of the beginGroup.
   */
  transient protected int stringizingElementLevel;

  public void writePosition(AbstractSequence seq, int ipos)
  {
    if (stringizingLevel > stringizingElementLevel
        && stringizingElementLevel > 0)
      return;
    seq.consumeNext(ipos, this);
  }

  transient int gapStartLastAtomic = -1;
  private static final int SAW_KEYWORD = -2;

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
    else if (v instanceof gnu.expr.Keyword)
      {
        Keyword k = (Keyword) v;
        beginAttribute(k.asSymbol());
        gapStartLastAtomic = SAW_KEYWORD;
      }
    else
      {
        closeTag();
        if (v instanceof UnescapedData)
          {
            super.writeObject(v);
            gapStartLastAtomic = -1;
          }
        else
          {
            if (gapStartLastAtomic == gapStart)
              writeChar(' ');
            // Using super.writeObject would be nice, but there are edge cases.
            // Specifically, atomic nodes with a zero-length string-value.
            // For example: <elem>{""}</elem>
            // Thus should result in zero text nodes.
            // Handling that case correctly and efficiently is left for later.
            // FIXME.
            //  super.writeObject(v);
            MakeText.text$C(v, this);  // Atomize.
            gapStartLastAtomic = gapStart;
          }
      }
  }

  public void beginGroup (Object type)
  {
    closeTag();
    groupLevel++;
    if (stringizingLevel == 0)
      {
        gapStartTag = gapStart;
        super.beginGroup(0);
        rememberGroup(type);
        attrCount = 1;
      }
    else if (stringizingElementLevel == 0)
      stringizingElementLevel = stringizingLevel;
    else
      stringizingLevel++;
  }

  public void beginGroup(int index)
  {
    closeTag();
    groupLevel++;
    attrCount = 1;
    if (stringizingLevel == 0)
      super.beginGroup(index);
    else if (stringizingElementLevel == 0)
      stringizingElementLevel = stringizingLevel;
    else
      stringizingLevel++;
  }

  public void setGroupName (int groupIndex, int nameIndex)
  {
    gapStartTag = groupIndex;
    super.setGroupName(groupIndex, nameIndex);
    Object groupType = objects[nameIndex];
    rememberGroup(groupType);
  }

  public void endGroup ()
  {
    closeTag();
    groupLevel--;
    if (stringizingLevel == 0)
      super.endGroup();
    if (stringizingElementLevel > 0)
      {
        if (stringizingElementLevel == stringizingLevel)
          stringizingElementLevel = 0;
        else
          stringizingLevel--;
      }
    if (groupLevel == 0)
      {
        workStack = null;
        attrIndexes = null;
      }
  }

  protected void rememberGroup (Object type)
  {
    if (copyNamespacesMode == 0)
      groupNamespaces = NamespaceBinding.predefinedXML;
    else if (copyNamespacesMode == COPY_NAMESPACES_INHERIT)
      groupNamespaces = groupLevel <= 1 ? NamespaceBinding.predefinedXML
        : (NamespaceBinding) workStack[groupLevel-2];
    else if (copyNamespacesMode == COPY_NAMESPACES_PRESERVE
             || groupLevel <= 1)
      {
        groupNamespaces
          = (type instanceof XName ? ((XName) type).getNamespaceNodes()
             : NamespaceBinding.predefinedXML);
      }
    else
      {
        NamespaceBinding inherited
          = (NamespaceBinding) workStack[groupLevel-2];
        if (type instanceof XName)
          {
            NamespaceBinding preserved = ((XName) type).getNamespaceNodes();
            groupNamespaces = NamespaceBinding.merge(inherited, preserved);
          }
        else
          groupNamespaces = inherited;
      }
    ensureSpaceInWorkStack(groupLevel-1);
    workStack[groupLevel-1] = groupNamespaces;
    workStack[groupLevel] = null; // FIXME - was groupName;
    workStack[groupLevel+1] = type;
  }

  protected void ensureSpaceInWorkStack (int oldSize)
  {
    if (workStack == null)
      {
        workStack = new Object[20];
      }
    // May need to extend array by three, in rememberGroup..
    else if (oldSize + 3 > workStack.length)
      {
        Object[] tmpn = new Object[2 * workStack.length];
        System.arraycopy(workStack, 0, tmpn, 0, oldSize);
        workStack = tmpn;
      }
  }

  protected void rememberAttribute (Object attrType)
  {
    if (attrIndexes == null)
      {
        attrIndexes = new int[10];
      }
    else if (attrCount >= attrIndexes.length)
      {
        int[] tmpi = new int[2 * attrCount];
        System.arraycopy(attrIndexes, 0, tmpi, 0, attrCount);
        attrIndexes = tmpi;
      }
    int oldSize = groupLevel + 2 * attrCount;
    ensureSpaceInWorkStack(oldSize);
    workStack[oldSize] = null;  // FIXME: was: attrName;
    workStack[oldSize + 1] = attrType;
    attrCount++;
  }

  public void beginAttribute (Object attrType)
  {
    if (groupLevel <= 1 && docStart != 0)
      error("attribute not allowed at document level");
    inAttribute = true;
    if (stringizingLevel++ > 0)
      return;
    if (attrCount == 0 && groupLevel > 0)
      error("attribute follows non-attribute content");
    checkAttributeSymbol((Symbol) attrType); // ???

    if (groupLevel == 0)
      {
        super.beginAttribute(super.find(attrType));
      }
    else
      {
        rememberAttribute(attrType);
        attrIndexes[attrCount-1] = super.gapStart;
        super.beginAttribute(0);
      }
  }

  public void beginAttribute(int index)
  {
    inAttribute = true;
    if (stringizingLevel++ > 0)
      return;
    super.beginAttribute(index);
  }

  /** Called from XMLFilter, only. */
  public void setAttributeName (int attrIndex, int nameIndex)
  {
    super.setAttributeName(attrIndex, nameIndex);
    Object attrType = objects[nameIndex];
    rememberAttribute(attrType);
    attrIndexes[attrCount-1] = attrIndex;
  }

  void checkAttributeSymbol (Symbol sym)
  {
    String local = sym.getLocalPart();
    String uri = sym.getNamespaceURI();
    if (uri == "http://www.w3.org/2000/xmlns/"
        || (uri == "" && local == "xmlns"))
      error("arttribute name cannot be 'xmlns or in xmlns namespace");
    for (int i = 1; i < attrCount;  i++)
      {
        Symbol symi = (Symbol) workStack[groupLevel+2*i+1];
        if (local == symi.getLocalPart()
            && uri == symi.getNamespaceURI())
          {
            error(duplicateAttributeMessage(sym, (String) workStack[groupLevel]));
          }
      }
  }

  public static String
  duplicateAttributeMessage (Symbol attrSymbol, String groupName)
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

  public void endAttribute()
  {
    if (! inAttribute)
      return;
    if (gapStartLastAtomic == SAW_KEYWORD)
      gapStartLastAtomic = -1;
    else if (--stringizingLevel == 0 && attrCount > 0)
      {
        inAttribute = false;
        int i = groupLevel + 2 * attrCount-2;
        Object name;
        boolean isId;
        if (workStack == null || i >= workStack.length)
          {
            // This can happend when we're called from XMLFilter.
            // Ideally we need the latter to eagerly pass xml:XXX and NCName
            // attributes (without waiting until the end of the element for
            // namespace processing).  FIXME.
            name = null;
            isId = false;
          }
        else
          {
            name = workStack[i];
            isId = "xml:id".equals(name);
          }
        if (isId)
          {
            int attrIndex = attrIndexes[attrCount-1];
            /* #ifdef JAVA5 */
            // StringBuilder sbuf = new StringBuilder();
            /* #else */
            StringBuffer sbuf = new StringBuffer();
            /* #endif */
            stringValue(attrIndex, sbuf);
            String id = XDataType.replaceWhitespace(sbuf.toString(), true);
            gapStart = attrIndex + BEGIN_ATTRIBUTE_LONG_SIZE;
            int len = id.length();
            for (i = 0;  i < len;  i++)
              super.writeChar(id.charAt(i));
          }
        super.endAttribute();
      }
  }

  public void beginDocument()
  {
    closeTag();
    if (stringizingLevel > 0)
      writeJoiner();
    // We need to increment groupLevel so that endDocument can decrement it.
    else
      {
        ensureSpaceInWorkStack(groupLevel);
        Object inherited;
        if (groupLevel++ == 0)
          {
            super.beginDocument();
            inherited = NamespaceBinding.predefinedXML;
          }
        else
          {
            writeJoiner();
            // copy "inherited" namespace bindings list
            inherited = workStack[groupLevel-2];
          }
        workStack[groupLevel-1] = inherited;
      }
  }

  public void endDocument ()
  {
    if (stringizingLevel > 0)
      {
        writeJoiner();
        return;
      }
    if (--groupLevel == 0)
      super.endDocument();
    else
      {
        workStack[groupLevel] = null;
        writeJoiner();
      }
    if (groupLevel == 0)
      {
        workStack = null;
        attrIndexes = null;
      }
  }

  protected void closeTag ()
  {
    if (attrCount > 0 && stringizingLevel == 0)
      {
        NamespaceBinding outer = groupLevel <= 1 ? NamespaceBinding.predefinedXML
        : (NamespaceBinding) workStack[groupLevel-2];
        NamespaceBinding bindings = groupNamespaces;
        for (int i = attrCount;  --i >= 0; )
          {
            Symbol sym = (Symbol) workStack[groupLevel+2*i+1];
            String prefix = sym.getPrefix();
            if (prefix == "")
              prefix = null;
            String uri = sym.getNamespaceURI();
            if (uri == "")
              uri = null;
            boolean isOuter = false;
            for (NamespaceBinding ns = bindings; ; ns = ns.next)
              {
                if (ns == outer)
                  isOuter = true;
                if (ns == null)
                  {
                    if (prefix != null || uri != null)
                      bindings = new NamespaceBinding(prefix, uri, bindings);
                    break;
                  }
                if (ns.prefix == prefix)
                  {
                    if (ns.uri != uri)
                      {
                        if (isOuter)
                          bindings = new NamespaceBinding(prefix, uri, bindings);
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
                            bindings = new NamespaceBinding(nprefix, uri, bindings);
                            String local = sym.getLocalName();
                            workStack[groupLevel+2*i+1] = nprefix+":"+local;
                            if (uri == null)
                              uri = "";
                            workStack[groupLevel+2*i+1] = Symbol.make(uri, local, nprefix);
                          }
                      }
                    break;
                  }
              }
          }
        Object groupType = workStack[groupLevel+1];
        if (! (groupType instanceof XName)
            || bindings != ((XName) groupType).getNamespaceNodes())
          groupType = new XName((Symbol) groupType, bindings);

        int groupIndex = super.find(groupType);
        super.setGroupName(gapStartTag, groupIndex);

        if (attrIndexes != null)
          {
            for (int i = attrCount;  --i > 0; )
              {
                int j = groupLevel + 2*i;
                int attrObjectIndex = super.find(workStack[j+1]);
                super.setAttributeName(attrIndexes[i], attrObjectIndex);
                workStack[j] = null;
                workStack[j+1] = null;
              }

          }
        workStack[groupLevel] = null;
        workStack[groupLevel+1] = null;
        attrCount = 0;
      }
  }

  protected boolean checkWriteAtomic ()
  {
    if (stringizingLevel > stringizingElementLevel
        && stringizingElementLevel > 0)
      return false;
    closeTag();
    return true;
 }

  public void writeChars(String v)
  {
    if (v.length() == 0)
      writeJoiner();
    else if (checkWriteAtomic())
      super.writeChars(v);
  }

  public void write(char[] buf, int off, int len)
  {
    if (len == 0)
      writeJoiner();
    else if (checkWriteAtomic())
      super.write(buf, off, len);
  }

  public void writeChar(int v)
  {
    if (checkWriteAtomic())
      super.writeChar(v);
  }

  public void writeBoolean (boolean v)
  {
    if (checkWriteAtomic())
      super.writeBoolean(v);
  }

  public void writeFloat (float v)
  {
    if (checkWriteAtomic())
      super.writeFloat(v);
  }

  public void writeDouble (double v)
  {
    if (checkWriteAtomic())
      super.writeDouble(v);
  }

  public void writeInt(int v)
  {
    if (checkWriteAtomic())
      super.writeInt(v);
  }

  public void writeLong (long v)
  {
    if (checkWriteAtomic())
      super.writeLong(v);
  }

  public void writeDocumentUri (Object uri)
  {
    if (groupLevel == 1)
      super.writeDocumentUri(uri);
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
            error("consecutive or final hyphen in XML comment");
            break;
          }
        sawHyphen = curHyphen;
      }
  }

  public void writeComment(char[] chars, int offset, int length)
  {
    checkValidComment(chars, offset, length);
    if (stringizingLevel == 0)
      {
        closeTag();
        super.writeComment(chars, offset, length);
      }
    else if (stringizingLevel < stringizingElementLevel
             || stringizingElementLevel == 0)
      super.write(chars, offset, length);
  }

  public void checkProcessingInstruction (String target)
  {
    int len = target.length();
    if (len == 3 && "xml".equalsIgnoreCase(target))
      error("process-instruction target mey not be 'xml' (ignoring case)");
    if (! XName.isName(target, true))
      error("process-instruction target '"+target+"' is not a valid Name");
  }

  public void writeProcessingInstruction(String target, char[] content,
					 int offset, int length)
  {
    checkProcessingInstruction(target);
    for (int i = offset+length;  --i >= offset; )
      {
        char ch = content[i];
        while (ch == '>' && --i >= offset)
          {
            ch = content[i];
            if (ch == '?')
              {
                error("'?>' is not allowed in a processing-instruction: proc: "+target+" -> "+new String(content, offset, length));
                break;
              }
          }
      }
    if (stringizingLevel == 0)
      {
        closeTag();
        super.writeProcessingInstruction(target, content, offset, length);
      }
    else if (stringizingLevel < stringizingElementLevel
             || stringizingElementLevel == 0)
      super.write(content, offset, length);
  }

  protected void writeJoiner ()
  {
    gapStartLastAtomic = -1;
    if (stringizingLevel <= stringizingElementLevel
       || stringizingElementLevel == 0)
      super.writeJoiner();
  }

  protected void error (String message)
  {
    throw new RuntimeException(message);
  }

  public int nextPos (int position)
  {
    boolean isAfter = (position & 1) != 0;
    int index = posToDataIndex(position);
    int next = nextNodeIndex(index, -1 >>> 1);
    if (next != index)
      return next << 1;
    if (index == data.length)
      return 0;
    return (index << 1) + 3;
  }

  public static NodeTree make ()
  {
    return new NodeTree();
  }

  static int counter;
  int id;

  /** Get/create a new unique number. */
  public int getId()
  {
    if (id == 0)
      id = ++counter;
    return id;
  }

  public int stableCompare (AbstractSequence other)
  {
    if (this == other)
      return 0;
    // If other is also a NodeTree it would be simpler to just compare
    // the results of getId, but if we always did that there is the
    // slight risk that counter could overflow in the case of a
    // long-running program.  So we use system.identityHashCode as
    // the primary "key" and getId only when needed as a tie-breaker.
    int comp = super.stableCompare(other);
    if (comp == 0 && other instanceof NodeTree)
      {
	int id1 = this.getId();
	int id2 = ((NodeTree) other).getId();
	comp = id1 < id2 ? -1 : id1 > id2 ? 1 : 0;
      }
    return comp;
  }

  public SeqPosition getIteratorAtPos(int ipos)
  {
    return KNode.make(this, ipos);
  }

  public String posNamespaceURI (int ipos)
  {
    Object type = getNextTypeObject(ipos);
    if (type instanceof XName)
      return ((XName) type).getNamespaceURI();
    if (type instanceof Symbol)
      return ((Symbol) type).getNamespaceURI();
    return null;
  }

  public String posPrefix (int ipos)
  {
    String name = getNextTypeName(ipos);
    if (name == null)
      return null;
    int colon = name.indexOf(':');
    return colon < 0 ? null : name.substring(0, colon);
  }

  public String posLocalName (int ipos)
  {
    Object type = getNextTypeObject(ipos);
    if (type instanceof XName)
      return ((XName) type).getLocalPart();
    if (type instanceof Symbol)
      return ((Symbol) type).getLocalName();
    return getNextTypeName(ipos);
  }

  public boolean posIsDefaultNamespace (int ipos, String namespaceURI)
  {
    throw new Error("posIsDefaultNamespace not implemented");
  }

  public String posLookupNamespaceURI (int ipos, String prefix)
  {
    int kind = getNextKind(ipos);
    if (kind != Sequence.GROUP_VALUE)
      throw new IllegalArgumentException("argument must be an element");
    Object type = getNextTypeObject(ipos);
    if (type instanceof XName)
      return ((XName) type).lookupNamespaceURI(prefix);
    else
      return null;
  }

  public String posLookupPrefix (int ipos, String namespaceURI)
  {
    throw new Error("posLookupPrefix not implemented");
  }

  public int posFirstChild(int ipos)
  {
    int index = gotoChildrenStart(posToDataIndex(ipos));
    if (index < 0)
      return -1;
    char datum = data[index];
    if (datum == END_GROUP_SHORT || datum == END_GROUP_LONG
	|| datum == END_DOCUMENT)
      return -1;
    return index << 1;
  }

  public boolean posHasAttributes (int ipos)
  {
    int index = gotoAttributesStart(posToDataIndex(ipos));
    if (index < 0)
      return false;
    return index >= 0 && data[index] == BEGIN_ATTRIBUTE_LONG;
  }

  /** Find named attribute.
   * @param namespaceURI need not be interned,
   *   or null which matches any namespace
   * @param localName need not be interned,
   *   or null which matches any local name
   * @return attribute ipos or 0
   */
  public int getAttribute (int parent, String namespaceURI, String localName)
  {
    return getAttributeI(parent,
                         namespaceURI == null ? null : namespaceURI.intern(),
                         localName == null ? null : localName.intern());
  }

  /** Find named attribute.
   * @param namespaceURI an interned String or null which matches any namespace
   * @param localName an interned String, or null which matches any local name
   * @return attribute ipos or 0
   */
  public int getAttributeI (int parent, String namespaceURI, String localName)
  {
    int attr = firstAttributePos(parent);
    for (;;)
      {
        if (attr == 0 || getNextKind(attr) != Sequence.ATTRIBUTE_VALUE)
          return 0;
        if ((localName == null || posLocalName(attr) == localName)
            && (namespaceURI == null || posNamespaceURI(attr) == namespaceURI))
          return attr;
        attr = nextPos(attr);
      }
  }

  /** Return the type-value of the node at the specified position. */
  public Object typedValue (int ipos)
  {
    // FIXME when we support validation.
    StringBuffer sbuf = new StringBuffer();
    stringValue(posToDataIndex(ipos), sbuf);
    String str = sbuf.toString();
    int kind = getNextKind(ipos);
    if (kind == Sequence.PROCESSING_INSTRUCTION_VALUE
        || kind == Sequence.COMMENT_VALUE)
      return str;
    return new UntypedAtomic(str);
  }

  /** Get the target of a process-instruction. */
  public String posTarget (int ipos)
  {
    int index = posToDataIndex(ipos);
    if (data[index] != PROCESSING_INSTRUCTION)
      throw new ClassCastException("expected process-instruction");
    return (String) objects[getIntN(index+1)];
  }

  /** Look for matching attribute in ancestor or self.
   * @param namespace namespaceURI (interned) of required attribute
   * @param name localName(interned) of required attribute 
   * @return attribute ipos or 0
   */
  public int ancestorAttribute (int ipos,
                                String namespace, String name)
  {
    for (;;)
      {
        if (ipos == -1)
          return 0;
        int attr = getAttributeI(ipos, namespace, name);
        if (attr != 0)
          return attr;
        ipos = parentPos(ipos);
      }
  }

  /** Return of the base-uri property, if known, of the node at pos. */
  public Object baseUriOfPos (int pos, boolean resolveRelative)
  /* #ifdef use:java.net.URI */
    throws java.net.URISyntaxException
  /* #end */
  {
    Object base = null;
    int index = posToDataIndex(pos);
    for (;;)
      {
	if (index == data.length)
	  return null;
	char datum = data[index];
        Object uri = null;
        if (datum == BEGIN_ENTITY)
          {
            int oindex = getIntN(index+1);
            if (oindex >= 0)
              uri = objects[oindex];
          }
	else if ((datum >= BEGIN_GROUP_SHORT
	     && datum <= BEGIN_GROUP_SHORT+BEGIN_GROUP_SHORT_INDEX_MAX)
	    || datum == BEGIN_GROUP_LONG)
          {
            int attr = getAttributeI(pos, NamespaceBinding.XML_NAMESPACE, "base");
            if (attr != 0)
              uri = KNode.getNodeValue(this, attr);
          }
        if (uri != null)
          {
            base = base == null || ! resolveRelative ? uri
              : URI_utils.resolve(base, uri);
            if (URI_utils.isAbsolute(base) || ! resolveRelative)
              return base;
          }
	index = parentOrEntityI(index);
	if (index == -1)
          return base;
        pos = index << 1;
      }
  }

  public String toString ()
  {
    CharArrayOutPort wr = new CharArrayOutPort();
    XMLPrinter xp = new XMLPrinter(wr);
    consume(xp);
    wr.close();
    return wr.toString();
  }

  /** If non-null, a hash-table of ID names. */
  String[] idNames;
  /** If non-null, a mapping of element ipos values mapped by idNames.
   * If {@code idNames[i]} is non-null, then {@code idOffsets[i]} is the
   * ipos value of the first element that has the former as its ID property. */
  int[] idOffsets;
  /** Number of non-null entries in idNames. */
  int idCount;

  public void makeIDtableIfNeeded ()
  {
    if (idNames != null)
      return;
    // Force allocation - in case there are no xml:id nodes,
    // so we don't scan multiple times.
    int size = 64;
    idNames = new String[size];
    idOffsets = new int[size];
    int limit = endPos();
    int ipos = 0;
    for (;;)
      {
	ipos = nextMatching(ipos, ElementType.anyElement, limit, true);
	if (ipos == 0)
	  break;
        // Until we do validation, we only recognize 'xml:id' as setting
        // the is-id property.  FIXME.
        int attr = getAttributeI(ipos, NamespaceBinding.XML_NAMESPACE, "id");
        if (attr != 0)
          {
            enterID(KNode.getNodeValue(this, attr), ipos);
          }
      }
  }

  void enterID (String name, int offset)
  {
    int size;
    String[] tmpNames = idNames;
    int[] tmpOffsets = idOffsets;
    if (tmpNames == null)
      {
        size = 64;
        idNames = new String[size];
        idOffsets = new int[size];
      }
    else if (4 * idCount >= 3 * (size = idNames.length))
      {
        idNames = new String[2 * size];
        idOffsets = new int[2 * size];
        idCount = 0;
        for (int i = size;  --i >= 0; )
          {
            String oldName = tmpNames[i];
            if (oldName != null)
              enterID(oldName, tmpOffsets[i]);
          }
        tmpNames = idNames;
        tmpOffsets = idOffsets;
        size = 2 * size;
      }
    int hash = name.hashCode();
    int mask = size - 1;
    int index = hash & mask;
    // Must be odd - or more specifically relatively prime with size,
    int step = (~hash << 1) | 1;
    for (;;)
      {
        String oldName = tmpNames[index];
        if (oldName == null)
          {
            tmpNames[index] = name;
            tmpOffsets[index] = offset;
            break;
          }
        if (oldName.equals(name)) // intern and == ?? FIXME
          {
            // Nothing to do.
            return;
          }
        index = (index + step) & mask;
      }
    idCount++;
  }

  /** Look for an element with matching ID.
   * Returns an element ipos, or -1 if not found.
   * Since we don't do any validation, for now only attributes with the
   * name {@code xml:id} are recognized has having the {@code is-id} property.
   * Assumes makeIDtableIfNeeded has been called at soem point.
   */
  public int lookupID (String name)
  {
    String[] tmpNames = idNames;
    int[] tmpOffsets = idOffsets;
    int size = idNames.length;
    int hash = name.hashCode();
    int mask = size - 1;
    int index = hash & mask;
    // Must be odd - or more specifically relatively prime with size,
    int step = (~hash << 1) | 1;
    for (;;)
      {
        String oldName = tmpNames[index];
        if (oldName == null)
          return -1;
        if (oldName.equals(name)) // intern and == ?? FIXME
          {
            return tmpOffsets[index];
          }
        index = (index + step) & mask;
      }
  }
}
