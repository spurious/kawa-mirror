// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.servlet;
import javax.servlet.*;
import javax.servlet.http.*;
import gnu.mapping.*;
import gnu.xml.*;
import java.io.IOException;

public abstract class KawaServlet
extends HttpServlet implements CpsMethodContainer
{
  public void doGet(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException
  {
    response.setContentType("text/html");
    ServletCallContext ctx = new ServletCallContext();
    ctx.request = request;
    ctx.response = response;
    ctx.servlet = this;
    ctx.values = Values.noArgs;
    OutPort out = new OutPort(response.getOutputStream());

    /* FIXME should use fluid binding!
    gnu.expr.Interpreter interp = gnu.expr.Interpreter.getInterpreter();
    String lang = interp.getName();
    Environment env = Environment.getCurrent();
    if (lang == "XQuery")
      {
	env.defineValue("request", request);
	env.defineValue("response", response);
	env.defineValue("servlet", this);
	env.defineValue("out", out);
      }
    else
      {
	env.defineValue("*request*", request);
	env.defineValue("*response*", response);
	env.defineValue("*servlet*", this);
	env.defineValue("*out*", out);
      }
    */

    out.println("<html>");
    ctx.consumer = new XMLPrinter(out);
    apply(ctx);
    ctx.run();
    out.println("</html>");
  }

  public void apply(CpsMethodProc proc, CallContext context)
  {
  }

  public abstract void apply(CallContext context);
}

