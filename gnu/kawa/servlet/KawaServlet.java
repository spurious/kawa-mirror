// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.servlet;
import javax.servlet.*;
import javax.servlet.http.*;
import gnu.mapping.*;
import gnu.xml.*;
import java.io.IOException;

public abstract class KawaServlet
extends HttpServlet
{
   public void doPost (HttpServletRequest request,
		       HttpServletResponse response)
     throws ServletException, IOException
  {
    doGet(request, response);
  }

  public void doGet(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException
  {
    CallContext ct = CallContext.getOnlyInstance();
    ServletCallContext ctx;
    if (ct instanceof ServletCallContext)
      {
	ctx = (ServletCallContext) ct;
      }
    else
      {
	ctx = new ServletCallContext();
	CallContext.setInstance(ctx);
      }
    ctx.consumer = new ServletPrinter(response);  // FIXME - should re-use
    ctx.request = request;
    ctx.response = response;
    ctx.servlet = this;
    ctx.values = Values.noArgs;

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

    ctx.consumer.beginDocument();
    try
      {
	// FIXME - untested!
	apply(ctx);
	//ctx.run();
      }
    catch (Throwable throwable)
      {
	// Clear partial output on an error.
	response.resetBuffer();
	if (throwable instanceof WrappedException)
	  {
	    Throwable cause = ((WrappedException) throwable).getCause();
	    if (cause != null)
	      throwable = cause;
	  }
	throw new ServletException(throwable);
      }
    ctx.consumer.endDocument();
  }

  public abstract void apply(CallContext context)  throws Throwable;
}

