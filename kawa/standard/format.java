package kawa.standard;
import kawa.lang.*;
import java.text.MessageFormat;
import gnu.text.ReportFormat;
import gnu.mapping.*;

public class format extends ProcedureN
{
  char magic = '%';

  public boolean ELispStyle() { return true; }
  public boolean CLispStyle() { return false; }

  /*
  public int format(OutPort dst,
		    char[] format, int fmt_offset, int fmt_length,
		    Object[] args, int arg_offset, int arg_length)
  {
    int arg_index = arg_offset;
    int arg_max = arg_index + arg_length;
    for (;;)
      {
	if (fmt_offset >= fmt_length)
	  return arg_index;
	char ch = format[fmt_offset++];
	if (ch != magic)
	  {
	    dst.print(ch);
	    continue;
	  }

	// handle options ...  TODO

	if (fmt_offset >= fmt_length) 
          return arg_index;  // ERROR
	ch = format[fmt_offset++];
	Object arg = null;
	if (ch == 's' || ch == 'S' || ch == 'a')
	  {
	    if (arg_index >= arg_max)
	      return arg_index;  // ERROR
	    arg = args[arg_index++];
	  }
	switch (ch)
	  {
	  case '%':
	  case '~':
	    dst.print(ch); 
	    continue; 
	  case 's':
	    dst.writeSchemeObject(arg, CLispStyle());
	    break;
	  case 'a':
	  case 'S':
	    dst.writeSchemeObject(arg, true);
	    break;
	  }
      }
  }
  */

  public void format(OutPort dst, Object[] args, int arg_offset)
  {
    Object format = args[arg_offset++];
    Object[] vals = new Object[args.length - arg_offset];
    System.arraycopy(args, arg_offset, vals, 0, vals.length);
    if (format instanceof MessageFormat)
      {
	String out = ((MessageFormat) format).format(vals);
	dst.print(out);
      }
    else
      {
	if (! (format instanceof ReportFormat))
	  format = Scheme.parseFormat.apply1(format);
	try
	  {
	    ((ReportFormat) format).format(vals, 0, dst, null);
	  }
	catch (java.io.IOException ex)
	  {
	    throw new GenericError("Error in format: "+ ex);
	  }
      }
  }

  public FString formatToString (Object[] args, int arg_offset)
  {
    java.io.CharArrayWriter wr = new java.io.CharArrayWriter ();
    OutPort port = new OutPort(wr, "<string>");
    format(port, args, arg_offset);
    port.close ();
    return new FString(wr.toCharArray());
  }

  public Object applyN (Object[] args)
  {
    Object port_arg = args[0];
    if (port_arg == Boolean.TRUE)
      {
	format(OutPort.outDefault(), args, 1);
	return Boolean.TRUE;
      }
    else if (port_arg == Boolean.FALSE)
      {
	return formatToString(args, 1);
      }
    else if (port_arg instanceof FString
	     || port_arg instanceof MessageFormat
	     || port_arg instanceof ReportFormat)
      {
	return formatToString(args, 0);
      }
    else if (port_arg instanceof OutPort)
      {
	format((OutPort) port_arg, args, 1);
	return Boolean.TRUE;
      }
    else if (port_arg instanceof java.io.Writer)
      { 
	OutPort port = new OutPort((java.io.Writer) port_arg);
        format(port, args, 1);
	port.flush();
        return Boolean.TRUE; 
      } 
    else if (port_arg instanceof java.io.OutputStream)
      { 
	OutPort port = new OutPort((java.io.OutputStream) port_arg);
        format(port, args, 1);
	port.flush();
        return Boolean.TRUE; 
      }
    else
      throw new GenericError("bad first argument to format");
  }
}
