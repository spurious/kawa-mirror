// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.mapping.*;
import gnu.lists.*;

public class BaseUri extends Procedure0or1
{
  public static final BaseUri baseUri = new BaseUri();

  public static Object baseUri (Object node)
  {
    Object baseUri = null;
    if (node instanceof AbstractSequence)
      {
	AbstractSequence seq = (AbstractSequence) node;
	baseUri = seq.baseUriOfPos(seq.startPos());
      }
    else if (node instanceof SeqPosition && ! (node instanceof TreePosition))
      {
	SeqPosition pos = (SeqPosition) node;
	baseUri = pos.sequence.baseUriOfPos(pos.ipos);
      }
    return baseUri == null ? Values.empty : baseUri;
  }

  public static Object baseUri ()
  {
    CallContext ctx = CallContext.getInstance();
    String baseUri = ctx.getBaseUri();
    return baseUri == null ? Values.empty : (Object) baseUri;
  }

  /** Tests if a URL has a scheme.
   * For convenience, we treat a 1-character "scheme" as an
   * MS-DOS-style "drive letter" - i.e. not a scheme. */
  public static boolean hasScheme (String name)
  {
    return uriSchemeLength(name) > 1;
  }

  /** Helper routine to get the scheme part of a URI.
   * The scheme part is "http:" or "file:" or "ftp:" most commonly.
   * This functions searches for the first ':' that doesn't follow a '/'.
   * @return The length of the scheme component, not counting the colon,
   * (or alternatively the index of the colon,), or -1 if the is no scheme.
   */
  public static int uriSchemeLength (String uri)
  {
    int len = uri.length();
    for (int i = 0;  i < len;  i++)
      {
	char ch = uri.charAt(i);
	if (ch == ':')
	  return i;
	if (ch == '/')
	  return -1;
      }
    return -1;
  }

  /** Resolve a URI against a base URI.
   * This does not collapse redundant '..' and '.'; perhaps it should. */
  public static String resolve (String uri, String base)
  {
    if (hasScheme(uri) || base == null)
      return uri;
    char fileSep = System.getProperty ("file.separator").charAt(0);
    int lastSl = base.lastIndexOf('/');
    if (fileSep != '/')
      {
	int lastSep = base.lastIndexOf(fileSep);
	if (lastSep > lastSl)
	  lastSl = lastSep;
      }
    StringBuffer sbuf = new StringBuffer(base);
    if (lastSl >= 0)
      sbuf.setLength(lastSl+1);
    else
      sbuf.append('/');
    if (uri.length() > 0
	&& (uri.charAt(0) == '/'
	    || (fileSep != '/'
		&& (uri.charAt(0) == fileSep || uri.charAt(1) == ':'))))
      {
	// Uri is an abolste file name, but doesn't have a uri scheme.
	int baseLen = base.length();
	int pathStart = BaseUri.uriSchemeLength(base);
	if (pathStart <= 1)
	  return uri;
	pathStart++;
	// Add the "authority" - usually a host-name.
	if (pathStart + 1 < baseLen
	    && base.charAt(pathStart) == '/'
	    && base.charAt(pathStart+1) == '/')
	  {
	    int p2 = base.indexOf('/', pathStart+2);
	    if (p2 < 0)
	      p2 = baseLen; // ? append '/'? FIXME
	    pathStart = p2;
	  }
	sbuf.setLength(pathStart);
	sbuf.append(uri);
      }
    else
      {
	// Should simplify "segment/../" etc.  FIXME
	sbuf.append(uri);
      }
    return sbuf.toString();
  }

  public Object apply0 ()
  {
    return baseUri();
  }

  public Object apply1 (Object node)
  {
    return baseUri(node);
  }
}
