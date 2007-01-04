// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.servlet;
import javax.servlet.*;
import javax.servlet.http.*;
import gnu.mapping.*;

public class ServletCallContext extends CallContext
{
  public HttpServletRequest request;
  public HttpServletResponse response;
  public HttpServlet servlet;

  static ServletCallContext getServletCallContext ()
  {
    return (ServletCallContext) CallContext.getOnlyInstance();
  }

  public static HttpServletRequest getRequest ()
  {
    return getServletCallContext().request;
  }

  public static HttpServletResponse getResponse ()
  {
    return getServletCallContext().response;
  }

  public static HttpServlet getServlet ()
  {
    return getServletCallContext().servlet;
  }

  public static ServletConfig getServletConfig ()
  {
    return getServletCallContext().servlet.getServletConfig();
  }

  public static ServletContext getServletContext ()
  {
    return getServletCallContext().servlet.getServletConfig().getServletContext();
  }
}
