// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.servlet;
import javax.servlet.*;
import javax.servlet.http.*;
import java.util.*;
import java.io.*;
import java.net.*;

/** Wrapper class to allow a Servlet to be run as a CGI script.
 * Invoked as:
 *   java CGI_ARGS... gnu.kawa.servlet.CGIServletWrapper SERVLET
 * where CGI_ARGS... are properties set from CGI environment variables (for
 * example -DPATH_INFO="foo") and SERVLET is the name of the servlet class.
 * The cgi-wrapper program in ../../../bin can used do this.
 */

public class CGIServletWrapper extends ServletOutputStream
implements HttpServletRequest, HttpServletResponse,
  ServletConfig, ServletContext
{
  byte buffer[] = null;
  int bufpos = 0;
  PrintStream out = System.out;
  Vector headers = new Vector();
  String sawContentType;
  int statusCode = 0;
  String statusString;
  String servletName;

  public static void main(String[] args)
  {
    try
      {
	CGIServletWrapper wrapper = new CGIServletWrapper();
	Class servClass = Class.forName(args[0]);
	wrapper.servletName = args[0];
	HttpServlet servlet = (HttpServlet) servClass.newInstance();
	servlet.init(wrapper);
	servlet.service(wrapper, wrapper);
	wrapper.flushBuffer();
      }
    catch (Throwable ex)
      {
	ex.printStackTrace();
	System.exit(-1);
      }
  }

  private static final int defaultBufferSize = 2048;

  private void allocateBuffer()
  {
    if (buffer == null)
      buffer = new byte[defaultBufferSize];
  }

  public void write(int c)
    throws java.io.IOException
  {
    allocateBuffer();
    int bufsize = buffer.length;
    if (buffer.length == 0)
      {
	committ();
	out.write(c);
      }
    else
      {
	if (bufpos >= buffer.length)
	  flushBuffer();
	buffer[bufpos++] = (byte) c;
      }
  }

  public String getCgiEnvVar(String name)
  {
    return System.getProperty(name);
  }

  public ServletOutputStream getOutputStream()
  {
    return this;
  }

  PrintWriter writer;

  public java.io.PrintWriter getWriter()
  {
    if (writer == null)
      writer = new PrintWriter(out);
    return writer;
  }

  public java.util.Map getParameterMap()
  {
    return null; // FIXME
  }

  public void setBufferSize(int size)
  {
    if (bufpos > 0 || committed)
      throw new IllegalStateException();
    buffer = new byte[size];
  }

  public int getBufferSize()
  {

    return buffer == null ? defaultBufferSize : buffer.length;
  }

  public void setLocale (java.util.Locale locale)
  {
    // FIXME
  }

  private void committ()
    throws IOException
  {
    if (! committed)
      {
	printHeaders();
	committed = true;
      }
  }

  public void flushBuffer()
    throws IOException
  {
    committ();
    if (bufpos > 0)
      out.write(buffer, 0, bufpos);
    bufpos = 0;
  }

  public void resetBuffer()
  {
    bufpos = 0;
  }

  public void reset()
  {
    // FIXME
    resetBuffer();
  }

  java.util.Hashtable attributes;

  public Object getAttribute(String name)
  {
    return attributes == null ? null : attributes.get(name);
  }

  public void setAttribute(String name, Object value)
  {
    if (value == null)
      removeAttribute(name);
    else
      {
	if (attributes == null)
	  attributes = new java.util.Hashtable();
	attributes.put(name, value);
      }
  }

  public void removeAttribute(String name)
  {
    if (attributes != null)
      attributes.remove(name);
  }

  public Enumeration getAttributeNames()
  {
    return attributes.keys();
  }

  public String getContentType()
  {
    return getCgiEnvVar("CONTENT_TYPE");
  }


  public String getPathInfo()
  {
    return getCgiEnvVar("PATH_INFO");
  }

  public String getPathTranslated()
  {
    return getCgiEnvVar("PATH_TRANSLATED");
  }

  public String getRequestURI()
  {
    String script = getServletPath();
    String path = getPathInfo();
    if (script == null)
      return path;
    if (path == null)
      return script;
    return script + '/' + path;
  }

  public StringBuffer getRequestURL()
  {
    // Copied from Tomcat 4.0:

    StringBuffer url = new StringBuffer(100);
    String scheme = getScheme();
    int port = getServerPort();
    if (port < 0)
      port = 80; // Work around java.net.URL bug
    url.append(scheme);
    url.append("://");
    url.append(getServerName());
    if ((scheme.equals("http") && (port != 80))
	|| (scheme.equals("https") && (port != 443)))
      {
	url.append(':');
	url.append(port);
      }
    url.append('/');
    url.append(getRequestURI());
    return url;
  }

  public String getProtocol()
  {
    return getCgiEnvVar("SERVER_PROTOCOL");
  }

  public int getServerPort()
  {
    String port = getCgiEnvVar("SERVER_PORT");
    if (port != null)
      {
	try
	  {
	    return Integer.parseInt(port);
	  }
	catch (Throwable ex)
	  {
	  }
      }
    return -1;
  }

  public int getLocalPort()
  {
    return getServerPort();  // is this resonable?
  }

  public String getLocalAddr ()
  {
    try
      {
	return InetAddress.getByName(getLocalName()).getHostAddress();
      }
    catch (UnknownHostException ex)
      {
	return "127.0.0.1";
      }
  }

  public String getLocalName ()
  {
    return getServerName();  // is this resonable?
  }

  public int getRemotePort()
  {
    return -1;  // FIXME
  }

  public boolean isSecure()
  {
    return getServerPort() == 443; // FIXME
  }

  public String getServerName()
  {
    return getCgiEnvVar("SERVER_NAME");
  }

  public String getMethod()
  {
    String method = getCgiEnvVar("REQUEST_METHOD");
    return method == null ? "GET" : method;
  }

  boolean committed;
  String characterEncoding;

  public boolean isCommitted()
  {
    return committed;
  }

  public String getCharacterEncoding () { return characterEncoding; }

  public void setCharacterEncoding(String enc)
  {
    characterEncoding = enc;
  }

  public void setContentType(String type)
  {
    setHeader("Content-Type", type);
  }

  public void setContentLength (int len)
  {
    setIntHeader("Content-Length", len);
  }

  public void addCookie(Cookie cookie)
  {
    // FIXME
  }

  public boolean containsHeader(String str)
  {
    int num = headers.size();
    for (int i = 0;  i < num;  i += 2)
      if (headers.elementAt(i).equals(str))
	return true;
    return false;
  }

  public String encodeURL(String str)
  {
    return null;  // FIXME
  }

  public String encodeUrl(String str)
  {
    return null;  // FIXME
  }

  public String encodeRedirectURL(String str)
  {
    return null;  // FIXME
  }

  public String encodeRedirectUrl(String str)
  {
    return null;  // FIXME
  }

  public void sendError(int i, String str)
  {
    statusCode = i;
    statusString = str;
  }

  public void sendError(int i)
  {
    statusCode = i;
    statusString = null;
  }

  public void sendRedirect(String str)
  {
    // ignore FIXME
  }

  private void printHeader(String label, String value)
  {
    out.print(label);
    out.print(": ");
    out.println(value); // FIXME - need to quote?
  }

  private void printHeaders()
  {
    if (statusCode != 0)
      {
	out.print("Status: ");
	out.print(statusCode);
	if (statusString != null)
	  {
	    out.print(' ');
	    out.print(statusString);
	  }
	out.println();
      }
    if (sawContentType == null)
      setContentType("text/html"); // FIXME
    int num = headers.size();
    for (int i = 0;  i < num;  i += 2)
      printHeader(headers.elementAt(i).toString(),
		  headers.elementAt(i + 1).toString());
    //  if (sawContentType == null) writeRaw("Content-Type: text/html"); FIXME
    out.println();
  }

  public void setDateHeader(String str, long l)
  {
    // ignore FIXME
  }

  public void addDateHeader(String str, long l)
  {
    // ignore FIXME
  }

  public void setHeader(String label, String value)
  {
    int num = headers.size();
    for (int i = 0;  i < num;  i += 2)
      if (headers.elementAt(i).equals(label))
	{
	  if (label.equalsIgnoreCase("Content-type"))
	    sawContentType = value;
	  headers.setElementAt(value, i+1);
	  break;
	}
    addHeader(label, value);
  }

  public void setIntHeader(String str, int i)
  {
    setHeader(str, Integer.toString(i));
  }

  public void addHeader(String label, String value)
  {
    if (label.equalsIgnoreCase("Content-type"))
      sawContentType = value;
    headers.addElement(label);
    headers.addElement(value);
  }

  public void addIntHeader(String str, int i)
  {
    addHeader(str, Integer.toString(i));
  }

  public void setStatus(int i)
  {
    statusCode = i;
    statusString = null;
  }

  public void setStatus(int i, String str)
  {
    statusCode = i;
    statusString = str;
  }

  public String getScheme()
  {
    return getServerPort() == 443 ? "https" : "http";  // FIXME
  }

  public java.util.Enumeration getLocales()
  {
    return null; // FIXME
  }

  public String getRealPath(String path)
  {
    return null; // FIXME
  }

  public Locale getLocale()
  {
    return null; // FIXME
  }

  public RequestDispatcher getRequestDispatcher(String path)
  {
    return null; // FIXME
  }

  public String getRemoteAddr()
  {
    return getCgiEnvVar("REMOTE_ADDR");
  }

  public String getRemoteHost()
  {
    String host = getCgiEnvVar("REMOTE_HOST");
    return host != null ? host : getRemoteAddr();
  }

  public java.io.BufferedReader getReader()
  {
    return null; // FIXME
  }

  public ServletInputStream getInputStream()
  {
    return null; // FIXME
  }

  public String getParameter(String name)
  {
    return null; // FIXME
  }

  public Enumeration getParameterNames()
  {
    return null; // FIXME
  }

  public String[] getParameterValues(String name)
  {
    return null; // FIXME
  }

  public int getContentLength()
  {
   String length = getCgiEnvVar("CONTENT_LENGTH");
    if (length != null)
      {
	try
	  {
	    return Integer.parseInt(length);
	  }
	catch (Throwable ex)
	  {
	  }
      }
    return 0;
  }

  public String getAuthType()
  {
    return getCgiEnvVar("AUTH_TYPE");
  }

  public long getDateHeader(String str)
  {
    return -1; // FIXME
  }

  public String getHeader(String str)
  {
    return null; // FIXME
  }

  public Enumeration getHeaders(String str)
  {
    return null; // FIXME
  }

  public boolean isRequestedSessionIdValid()
  {
    return false;  // FIXME
  }

  public boolean isRequestedSessionIdFromCookie()
  {
    return false;  // FIXME
  }

  public boolean isRequestedSessionIdFromURL()
  {
    return false;  // FIXME
  }

  public boolean isRequestedSessionIdFromUrl()
  {
    return false;  // FIXME
  }

  public String getRequestedSessionId()
  {
    return null;  // FIXME
  }

  public String getServletPath()
  {
    return getCgiEnvVar("SCRIPT_NAME");
  }

  public HttpSession getSession(boolean b)
  {
    return null; // FIXME
  }

  public HttpSession getSession()
  {
    return null; // FIXME
  }

  public String getRemoteUser()
  {
    return getCgiEnvVar("REMOTE_USER");
  }

  public boolean isUserInRole(String role)
  {
    return false; // FIXME
  }

  public java.security.Principal getUserPrincipal()
  {
    return null; // FIXME
  }

  public String getContextPath()
  {
    return ""; // FIXME
  }

  public String getQueryString()
  {
    return getCgiEnvVar("QUERY_STRING");
  }

  public Enumeration getHeaderNames()
  {
    return null; // FIXME
  }

  public int getIntHeader(String str)
  {
    return -1;  // FIXME
  }

  public javax.servlet.http.Cookie[] getCookies()
  {
    return null; // FIXME
  }

  // ServletConfig methods

  public String getInitParameter(String name)
  {
    return null;  // FIXME
  }

  public Enumeration getInitParameterNames()
  {
    return nullEnumeration;
  }

  public ServletContext getServletContext()
  {
    return this;
  }

  public String getServletName()
  {
    return servletName;
  }

  // ServletContext methods:

  public ServletContext getContext (String path) { return null; }

  public int getMajorVersion () { return 2; }
  public int getMinorVersion () { return 3; }
  public String getMimeType (String file) { return null; }

  public java.util.Set getResourcePaths (String path) { return null; }
  public java.net.URL getResource (String path) { return null; }
  public java.io.InputStream getResourceAsStream (String path) { return null; }

  public RequestDispatcher getNamedDispatcher (String path) { return null; }
  public Servlet getServlet (String name) { return null; }
  static final Enumeration nullEnumeration
    = new gnu.lists.SeqPosition(gnu.lists.LList.Empty, 0, false);
  public Enumeration getServlets () { return nullEnumeration; }
  public Enumeration getServletNames () { return nullEnumeration; }

  public void log (String message) { }
  public void log (Exception ex, String mgs) { }
  public void log (String msg, Throwable ex) { }
  public String getServerInfo () { return "Kawa CGI/servlet wrapper";}

  public java.lang.String getServletContextName () { return null; }
}
