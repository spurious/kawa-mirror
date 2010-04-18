// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.servlet;

import gnu.expr.*;
import gnu.mapping.*;
import gnu.text.*;
import java.io.*;
import java.net.*;
import java.util.*;
import javax.servlet.*;
import javax.servlet.http.*;

/**
 * The Kawa servlet interpreter
 *
 * This servlet is responsible for reading and interpeting Kawa language files
 * using the QEXO GNU library.
 *
 * The implementation borrows ideas from Apache Jakarta Tomcat Jasper.
 *
 * @author Ivelin Ivanov
 * @author Tom Reilly
 * @author Per Bothner
 */
public class KawaPageServlet extends KawaServlet
{
  private ServletContext context;

  public void init(ServletConfig config)
      throws ServletException
  {
    super.init(config);
    context = config.getServletContext();
  }

  public void run(HttpRequestContext hctx, CallContext ctx) throws Throwable
  {
    KawaAutoHandler.run(hctx, ctx);
  }
}
