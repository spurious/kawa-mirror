// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import java.io.*;

/** A "namespace node" as a link in a linked list. */

public final class NamespaceBinding implements Externalizable
{
  /** Namespace prefix.  An interned String.
   * A default namespace declaration is represented using null. */
  String prefix;

  /** Namespace uri.  An interned String.
   * The value null "undeclares" any following namespaces; it corresponds
   * to an empty uri as in the XML Namespaces 1.1 Candidate Recommendation. */
  String uri;

  NamespaceBinding next;

  int depth;

  public final String getPrefix () { return prefix; }
  public final String getUri () { return uri; }
  public final NamespaceBinding getNext () { return next; }
  public final void setPrefix (String prefix) { this.prefix = prefix; }
  public final void setUri (String uri) { this.uri = uri; }
  public final void setNext (NamespaceBinding next) { this.next = next; }

  //  public NamespaceBinding () { }

  public NamespaceBinding (String prefix, String uri, NamespaceBinding next)
  {
    this.prefix = prefix;
    this.uri = uri;
    this.next = next;
    this.depth = next == null ? 0 : next.depth + 1;
  }

  public static final NamespaceBinding predefinedXML
  = new NamespaceBinding("xml", "http://www.w3.org/XML/1998/namespace", null);

  /** Resolve a prefix.
   * @param prefix an interned namespace prefix to search for.
   * @return a uri or null if not bound
   */
  public String resolve (String prefix)
  {
    for (NamespaceBinding ns = this;  ns != null;  ns = ns.next)
      {
	if (ns.prefix == prefix)
	  return ns.uri;
      }
    return null;
  }

  public static NamespaceBinding commonAncestor (NamespaceBinding ns1,
						 NamespaceBinding ns2)
  {
    if (ns1.depth > ns2.depth)
      {
	NamespaceBinding tmp = ns1;
	ns1 = ns2;
	ns2 = tmp;
      }
    while (ns2.depth > ns1.depth)
      ns2 = ns2.next;
    while (ns1 != ns2)
      {
	ns1 = ns1.next;
	ns2 = ns2.next;
      }
    return ns1; 
  }

  /* For debugging:
  void check ()
  {
    NamespaceBinding ns = this;
    int d = depth;
    for (;;)
      {
	if (ns == null)
	  throw new Error("null ns");
	if (ns.depth != d)
	  throw new Error("bad depth "+ns.depth+" shoudl be "+d);
	ns = ns.next;
	if (ns == null && d == 0)
	  return;
	d--;
      }
  }
  */

  public NamespaceBinding reversePrefix (NamespaceBinding fencePost)
  {
    NamespaceBinding prev = fencePost;
    NamespaceBinding t = this;
    int depth = fencePost == null ? -1 : fencePost.depth;
    while (t != fencePost)
      {
	NamespaceBinding next = t.next;
	t.next = prev;
	prev = t;
	t.depth = ++depth;
	t = next;
      }
    return prev;
  }

  /** Return the number of bindings before the <code>fencePost</code>. */
  public int count (NamespaceBinding fencePost)
  {
    int count = 0;
    for (NamespaceBinding ns = this;  ns != fencePost;  ns = ns.next)
      count++;
    return count;
  }

  /** Append a new NamespaceBinding if not redundant. */
  public static NamespaceBinding maybeAdd(String prefix, String uri,
					  NamespaceBinding bindings)
  {
    if (bindings == null)
      {
	if (uri == null)
	  return bindings;
	bindings = predefinedXML;
      }
    String found = bindings.resolve(prefix);
    if (found == null ? uri == null : found.equals(uri))
      return bindings;
    return new NamespaceBinding(prefix, uri, bindings);
  }

  /** Return a String showing just a single namespace binding. */
  public String toString()
  {
    return "Namespace{"+prefix+"="+uri+", depth:"+depth+"}";
  }

  /** Return a String showing the full namespace binding list. */
  public String toStringAll()
  {
    StringBuffer sbuf = new StringBuffer("Namespaces{");
    for (NamespaceBinding ns = this;  ns != null;  ns = ns.next)
      {
	sbuf.append(ns.prefix);
	sbuf.append("=\"");
	sbuf.append(ns.uri);
	sbuf.append(ns == null ? "\"" : "\", ");
      }
    sbuf.append('}');
    return sbuf.toString();
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeUTF(prefix);
    out.writeUTF(uri);
    out.writeObject(next);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    prefix = in.readUTF();
    uri = in.readUTF();
    next = (NamespaceBinding) in.readObject();
  }

}
