// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.servlet;
import javax.servlet.*;
import javax.servlet.http.*;
import gnu.mapping.*;

class ServletCallContext extends CallContext
{
  public HttpServletRequest request;
  public HttpServletResponse response;
  public HttpServlet servlet;
}
