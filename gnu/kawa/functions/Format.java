package gnu.kawa.functions;
import gnu.lists.*;
import java.text.MessageFormat;
import gnu.text.ReportFormat;
import gnu.mapping.*;

public class Format extends ProcedureN
{
  public static final Format format = new Format();
  static { format.setName("format"); }

  public static void format(OutPort dst, Object[] args, int arg_offset)
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
	  format = ParseFormat.parseFormat.apply1(format);
	try
	  {
	    ((ReportFormat) format).format(vals, 0, dst, null);
	  }
	catch (java.io.IOException ex)
	  {
	    throw new RuntimeException("Error in format: "+ ex);
	  }
      }
  }

  public static FString formatToString (Object[] args, int arg_offset)
  {
    CharArrayOutPort port = new CharArrayOutPort();
    format(port, args, arg_offset);
    char[] chars = port.toCharArray();
    port.close ();
    return new FString(chars);
  }

  /**
   * Apply format and argument, yielding an FString.
   * @param style either '%' (C/Emacs-style format specifiers), or
   *   '~' (Common Lisp-style format specifiers).
   * @param fmt the format string or specification
   * @param args the arguments to be formatted
   */
  public static FString formatToString(char style, Object fmt, Object[] args)
  {
    ReportFormat rfmt = ParseFormat.asFormat(fmt, style);
    CharArrayOutPort port = new CharArrayOutPort();
    try
      {
	rfmt.format(args, 0, port, null);
      }
    catch (java.io.IOException ex)
      {
	throw new RuntimeException("Error in format: "+ ex);
      }
    char[] chars = port.toCharArray();
    port.close ();
    return new FString(chars);
  }

  public Object applyN (Object[] args)
  {
    return format$V(args);
  }

  public static Object format$V (Object[] args)
  {
    Object port_arg = args[0];
    if (port_arg == Boolean.TRUE)
      {
	format(OutPort.outDefault(), args, 1);
	return Values.empty;
      }
    else if (port_arg == Boolean.FALSE)
      {
	return formatToString(args, 1);
      }
    else if (port_arg instanceof MessageFormat
             /* #ifdef use:java.lang.CharSequence */
             || port_arg instanceof CharSequence
             /* #else */
             // || port_arg instanceof String || port_arg instanceof CharSeq
             /* #endif */
	     || port_arg instanceof ReportFormat)
      {
	return formatToString(args, 0);
      }
    else if (port_arg instanceof OutPort)
      {
	format((OutPort) port_arg, args, 1);
	return Values.empty;
      }
    else if (port_arg instanceof java.io.Writer)
      { 
	OutPort port = new OutPort((java.io.Writer) port_arg);
        format(port, args, 1);
	port.closeThis();
	return Values.empty;
      } 
    else if (port_arg instanceof java.io.OutputStream)
      { 
	OutPort port = new OutPort((java.io.OutputStream) port_arg);
        format(port, args, 1);
	port.closeThis();
	return Values.empty;
      }
    else
      throw new RuntimeException("bad first argument to format");
  }
}
