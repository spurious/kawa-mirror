// Copyright (c) 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.servlet;
import gnu.kawa.xml.*;
import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

/** A Consumer that sends output to a ServletResponse.
 * This is the initial result destination when running a KawaServlet.
 */

public class ServletPrinter extends HttpPrinter
{
  HttpServletResponse response;

  public ServletPrinter(HttpServletResponse response)
    throws IOException
  {
    super(response.getOutputStream());
    this.response = response;
  }

  public ServletPrinter(OutputStream ostream)
  {
    super(ostream);
  }

  public void addHeader(String label, String value)
  {
    if (label.equalsIgnoreCase("Content-type"))
      {
	super.sawContentType = value;
	response.setContentType(value);
      }
    else if (label.equalsIgnoreCase("Status"))
      {
	int lval = value.length();
	int code = 0;
	int i;
	for (i = 0;  i < lval;  i++)
	  {
	    if (i >= lval)
	      {
		try
		  {
		    response.sendError(code);
		  }
		catch (java.io.IOException ex)
		  {
		    System.err.println("caught "+ex);
		  }
		break;
	      }
	    char ch = value.charAt(i);
	    int digit = Character.digit(ch, 10);
	    if (digit >= 0)
	      code = 10 * code + digit;
	    else
	      {
		if (ch == ' ')
		  i++;
		try
		  {
		response.sendError(code, value.substring(i));
		  }
		catch (java.io.IOException ex)
		  {
		    System.err.println("caught "+ex);
		  }
		break;
	      }
	  }
      }
    else
      response.addHeader(label, value);
  }

  public void printHeaders()
  {
  }
}
