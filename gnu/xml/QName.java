package gnu.xml;

/** An XML "qualified name", after namesapce-resolution.
 * Consists of a local part (the part after the colon in an XML file),
 * and a namespace URI (globally-unique name).   The prefix of a name
 * (the part before the colon) is not part of a QName, as
 * it is only considered a document-local abbreviation for the namespace URI.
 * QName's are always "interned": if two QNames have the same
 * local part and namespace URI, they are the same QName object.
 */

public class QName
{
  /** The namespace URI if it exists (as an interned String), else null. */
  public String getNamespaceURI()
  {
    return namespaceURI;
  }

  /** The local name of a qualified name, as an interned String.
   * If namespaceURI is null, this is the fully-qualified name. */
  public String getLocalName()
  {
    return localName;
  }

  public int hashCode()
  {
    return localName.hashCode();
  }

  public boolean equals(Object other)
  {
    return this == other;
  }

  public String toString()
  {
    StringBuffer sbuf = new StringBuffer(100);
    sbuf.append("QName{");
    if (namespaceURI != null)
      sbuf.append(namespaceURI);
    sbuf.append("}:");
    sbuf.append(localName);
    return sbuf.toString();
  }

  protected QName(String namespaceURI, String localName)
  {
    this.namespaceURI = namespaceURI;
    this.localName = localName;
  }

  /** Search a hash table using double hashing and open addressing.
   * @param table the hash table
   * @param log2Size log2 of the (used) size of table
   * @param namespaceURI the uri part of the search key
   * @param locaName the local part of the search key
   * @param hash the hash of the search key
   * @return the index of the element in table containing the match
   * (such that table[index].getName()==key);
   * if there is no such element, returns an index
   * such that (table[index]==null || tabel[index]==DELETED). */
  static int hashSearch (QName[] table, int log2Size,
			 String namespaceURI, String localName, int hash)
  {
    int mask = (1 << log2Size) - 1;
    int index = hash & mask;
    QName element = table[index];
    if (element == null
	|| (element.namespaceURI == namespaceURI
	    && element.localName == localName))
      return index;
    int avail = -1;
    int step = (((hash >> log2Size) ^ index) << 1) + 1;
    for (;;)
      {
	if (element == hashDELETED && avail < 0)
	  avail = index;
	index = (index + step) & mask;
	element = table[index];
	if (element == null)
	  return avail < 0 ? index : avail;
	if (element.namespaceURI == namespaceURI
	    && element.localName == localName)
	  return index;
      }
  }

  /** Find a QName with the given namespaceURI and localName.
   * Repeated calls with the same (equals) namespaceURI and localName
   * will return the same QName object.
   * @param namespaceURI namespace URI or null
   * @param localName local name of a qualified name
   */
  public static synchronized QName make(String namespaceURI, String localName)
  {
    int hash = localName.hashCode();
    int index = hashSearch(table, log2Size,
			   namespaceURI, localName, hash);
    QName element = table[index];
    if (element == null || element == hashDELETED)
      {
	if (namespaceURI != null)
	  namespaceURI = namespaceURI.intern();
	element = new QName(namespaceURI, localName.intern());
	table[index] = element;
	count++;

	// Rehash if over 2/3 full.
	if (3 * count >= 2 * table.length)
	  {
	    int new_capacity = 2 * table.length;
	    QName[] new_table = new QName[new_capacity];
	    hashInsertAll(new_table, log2Size + 1, table, log2Size);
	    table = new_table;
	    log2Size++;
	  }
      }
    return element;
  }

  /** Find a QName with the given namespaceURI and localName.
   * Same as make(String, STring), but if there is no matching QName
   * returns null rather than creating one.
   */
  public static synchronized QName get(String namespaceURI, String localName)
  {
    int hash = localName.hashCode();
    int index = hashSearch(table, log2Size,
			   namespaceURI, localName, hash);
    QName element = table[index];
    return element == hashDELETED ? null : element;
  }

  static synchronized void remove(QName name, int hash)
  {
    int index = hashSearch(table, log2Size,
			   name.namespaceURI, name.localName, hash);
    if (table[index] != name)
      return; // ERROR
    table[index] = hashDELETED;
    count--;
  }

  public void finalize()
  {
    remove (this, hashCode());
  }

  static void hashInsertAll (QName[] tableDst, int log2SizeDst,
				   QName[] tableSrc, int log2SizeSrc)
  {
    int sizeSrc = 1 << log2SizeSrc;
    for (int i = sizeSrc;  --i >= 0;)
      {
	QName element = tableSrc[i];
	if (element != null && element != hashDELETED)
	  {
	    int index = hashSearch(tableDst, log2SizeDst,
				   element.namespaceURI, element.localName,
				   element.hashCode());
	    QName oldElement = tableDst[index];
	    tableDst[index] = element;
	  }
      }
  }

  /** The namespace URI if it exists, else null.
   * This String is assumed to be interned. */
  protected String namespaceURI;

  /** The local name of a qualified name.
   * If namespaceURI is null, this is the fully-qualified name.
   * This string is assumed to be interned. */
  protected String localName;

  /** A hashtable used to manage QNames and make sure they are unique.
   * The length must be a power of two. */
  static QName[] table;
  /** Must be log2(table.length). */
  static int log2Size;
  /** Number of (non-deleted) entries in the table. */
  static int count;

  /** Used to mark deleted elements in a hash table. */
  public static final QName hashDELETED = new QName(null, null);
}
