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
    super.addHeader(label, value);
    if (label.equalsIgnoreCase("Content-type"))
      response.setContentType(value);
    else
      response.addHeader(label, value);
  }

  public void printHeader(String label, String value)
    throws java.io.IOException
  {
  }
}
