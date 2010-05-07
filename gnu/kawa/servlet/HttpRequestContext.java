// Copyright (c) 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see COPYING.

package gnu.kawa.servlet;

import java.io.*;
import java.net.*;
import java.util.*;

/** A representation of the an http request as it's being handled.
 * It abstracts over different http server's API - specially, there are are
 * concrete implementations on top of JDK6's com.sun.net.httpserver,
 * javax.servlet.http, and CGI (on top of servlets).
 */

public abstract class HttpRequestContext
{
  public static final int HTTP_OK = 200;
  public static final int HTTP_NOT_FOUND = 404;

  public int statusCode = HTTP_OK;
  public String statusReasonPhrase = null;

  protected static final ThreadLocal<HttpRequestContext> instance
    = new ThreadLocal<HttpRequestContext>();

  public static HttpRequestContext getInstance()
  {
    HttpRequestContext hctx = instance.get();
    if (hctx == null)
      throw new UnsupportedOperationException("can only be called by http-server");
    return hctx;
  }

  public static HttpRequestContext getInstance(String command)
  {
    HttpRequestContext hctx = instance.get();
    if (hctx == null)
      throw new UnsupportedOperationException(command + " can only be called within http-server");
    return hctx;
  }

  public static void setInstance (HttpRequestContext ctx)
  {
    instance.set(ctx);
  }

  /** Return an OutputStream for the result body.
   * Multiple calls will return the same OutputStream.
   */
  public abstract OutputStream getResponseStream ();

  public String getRequestParameter (String name)
  {
    List<String> p = getRequestParameters().get(name);
    return p == null || p.isEmpty() ? null : p.get(0);
  }
  public abstract Map<String, List<String>> getRequestParameters ();

  public abstract URI getRequestURI();

  /** Returns the context path, relative to the server root.
   * This is an initial substring of the {@link #getRequestURI}.
   * Like {@code ServletContext#getContextPath}, but ends with a {@code '/'}.
   * The string {@code getRequestURI()} is the same as the concatenation of
   * {@code getContextPath()}, {@code getScriptPath()}, and {code getLocationPath()}.
   */
  public abstract String getContextPath ();

  /** Returns the path of the script, relative to the context.
   * Like {@code ServletRequestt#getServletPath}, but ends with a {@code '/'},
   * and does not start with one.  (The reason for this is to produce URIs
   * that work better with operations like resolve-uri.)
   */
  public String getScriptPath () { return scriptPath; }
  /** Returns the remainder of the request path, relative to the script.
   */
  public String getLocalPath () { return localPath; }

  String scriptPath = "", localPath = "";
  public void setScriptAndLocalPath (String scriptPath, String localPath)
  {
    this.scriptPath = scriptPath;
    this.localPath = localPath;
  }

  public abstract String getPathTranslated ();

  public String getRequestPath()
  {
    return getRequestURI().getPath();
  }

  public String getRequestScheme ()
  {
    return "http";
  }

  public InetSocketAddress getLocalSocketAddress ()
  {
    return new InetSocketAddress(getLocalHost(), getLocalPort());
  }

  public String getLocalIPAddress ()
  {
    return getLocalHost().getHostAddress();
  }

  public InetAddress getLocalHost ()
  {
    try
      {
        return InetAddress.getLocalHost();
      }
    catch (Throwable ex)
      {
        throw new RuntimeException(ex);
      }
  }

  public abstract int getLocalPort();

  public InetSocketAddress getRemoteSocketAddress ()
  {
    return new InetSocketAddress(getRemoteHost(), getRemotePort());
  }

  public abstract InetAddress getRemoteHost ();
  public abstract String getRemoteIPAddress ();
  public abstract int getRemotePort ();

  public StringBuffer getRequestURLBuffer ()
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append(getRequestScheme());
    sbuf.append("://");
    String host = getRequestHeader("Host");
    if (host != null)
      sbuf.append(host);
    else
      {
        sbuf.append(getLocalIPAddress());
        sbuf.append(':');
        sbuf.append(getLocalPort());
      }
    sbuf.append(getRequestPath());
    return sbuf;
  }

  public abstract String getQueryString();
  public abstract String getRequestMethod();

  public abstract String getRequestHeader (String name);
  public abstract List<String> getRequestHeaders (String name);
  public abstract Map<String,List<String>> getRequestHeaders ();

  public abstract void setResponseHeader(String name, String value);

  public void setContentType(String type)
  {
    setResponseHeader("Content-Type", type);
  }

  protected String normalizeToContext (String path)
  {
    if (path.length() > 0 && path.charAt(0) == '/')
      path = path.substring(1);
    else
      path = getScriptPath() + path;
    if (path.indexOf("..") >= 0)
      {
        path = URI.create(path).normalize().toString();
        if (path.startsWith("../"))
          return null;
      }
    return path;
  }

  /** Returns the URL of a resource.
   * The resource is relative to the script path, if the path is relative;
   * otherwise (if it starts with a {@code '/'} it is relative to the context path.
   */
  public abstract URL getResourceURL (String path);

  /** Get attribute from the server context. */
  public abstract Object getAttribute(String name);
  /** Set attribute in the server context. */
  public abstract void setAttribute(String name, Object value);

  /** Send headers.
   * @param reasonCode response code - e.g. 200 for OK.
   * @param reasonCode response string - e.g. "OK" or "Not Found".
   * @param responseLength response length in bytes, or -1 (unspecified).
   *  Note this is different from HttpExchange.sendResponseHeaders.
   * This method must be called before getResponseStream.
   */
  public abstract void sendResponseHeaders(int reasonCode, String reasonPhrase, long responseLength)
    throws IOException;

  public void sendNotFound(String path)
    throws IOException
  {
    String msg = "The requested URL "+path+" was not found on this server.\r\n";
    byte[] bmsg = msg.getBytes();
    sendResponseHeaders(HTTP_NOT_FOUND, null, bmsg.length);
    OutputStream out = getResponseStream();
    try
      {
        out.write(bmsg);
      }
    catch (IOException ex)
      {
        throw new RuntimeException(ex);
      }
  }

  public abstract void log (String message);
}

class HttpOutputStream extends OutputStream
{
  HttpRequestContext context;
  byte[] buffer;
  OutputStream out;
  int count;

  public HttpOutputStream(HttpRequestContext context, int bufSize)
  {
    this.context = context;
    buffer = new byte[bufSize];
  }

  public void write(int b)
    throws IOException
  {
    if (count >= buffer.length)
      flush();
    buffer[count++] = (byte) b;
  }

  public void write(byte[] data, int offset, int length)
           throws IOException
  {
    int avail = buffer.length - count;
    while (length > avail)
      {
        System.arraycopy(data, offset, buffer, count, avail);
        count += avail;
        flush();
        offset += avail;
        length -= avail;
        avail = buffer.length;
      }
    if (length > 0)
      {
        System.arraycopy(data, offset, buffer, count, length);
        count += length;
      }
  }

  public void flush()
           throws IOException
  {
    if (out == null)
      {
        context.sendResponseHeaders(context.statusCode, context.statusReasonPhrase, -1);
        out = context.getResponseStream();
      }
    if (count > 0)
      {
        out.write(buffer, 0, count);
        count = 0;
      }
  }

  public void close()
           throws IOException
  {
    if (out == null)
      {
        context.sendResponseHeaders(context.statusCode, context.statusReasonPhrase, count);
        out = context.getResponseStream();
      }
    flush();
    out.close();
  }
}