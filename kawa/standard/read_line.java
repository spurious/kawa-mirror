package kawa.standard;
import kawa.lang.*;
import gnu.text.LineBufferedReader;
import gnu.mapping.*;

public class read_line extends ProcedureN
{
  public int numArgs() { return 0x2000; }

  public Object applyN(Object[] args)
  {
    int len = args.length;
    if (len == 0)
      return apply0();
    if (len == 1)
      return apply2(args[0], "trim");
    if (len == 2)
      return apply2(args[0], args[1]);
    throw new WrongArguments(this, len);
  }
   
  public final Object apply0 ()
  {
    return apply2(InPort.inDefault(), "trim");
  }

  public final Object apply1 (Object arg1)
  {
    return apply2(arg1, "trim");
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    LineBufferedReader in;
    String handling;
    try
      {
        in = (LineBufferedReader) arg1;
      }
    catch (ClassCastException ex)
      {
        throw WrongType.make(ex, this, 0);
      }
    try
      {
        handling = (String) arg2;
      }
    catch (ClassCastException ex)
      {
        throw WrongType.make(ex, this, 1);
      }
    try
      {
        return apply(in, handling);
      }
    catch (java.io.IOException e)
      {
	throw new RuntimeException ("I/O exception in read-line: " + e.toString ());
      }
  }

  public static Object apply(LineBufferedReader in, String handling)
    throws java.io.IOException
  {
    int ch = in.read();
    if (ch < 0)
      return gnu.expr.Special.eof;
    int start = in.pos - 1;
    int pos = start;
    int limit = in.limit;
    char[] buffer = in.buffer;
    int delim = -1;  // Length of delimiter.
    while (pos < limit)
      {
        ch = buffer[pos++];
        if (ch == '\r' || ch == '\n')
          {
            pos--;
            if (handling == "trim" || handling == "peek")
              {
                if (handling == "peek")
                  delim = 0;
                if (ch == '\n')
                  delim = 1;
                else if (pos < limit)
                  delim = buffer[pos] == '\n' ? 2 : 1;
                else
                  break;
                in.pos = pos + delim;
              }
            else if (handling == "concat" && ch == '\n')
              {
                in.pos = ++pos;
              }
            else
              break;
            return new FString(buffer, start, pos - start);
          }
      }
    StringBuffer sbuf = new StringBuffer(100);
    if (pos > start)
      sbuf.append(buffer, start, pos - start);
    in.pos = pos;
    char mode = handling == "peek" ? 'P'
      : handling == "concat" || handling == "split" ? 'A'
      : 'I';
    in.readLine(sbuf, mode);
    int length = sbuf.length();
    if (handling == "split")
      {
        if (length == 0)
          delim = 0;
        else
          {
            char last = sbuf.charAt(length - 1);
            if (last == '\r')
              delim = 1;
            else if (last != '\n')
              delim = 0;
            else if (last > 2 && sbuf.charAt(length-2) == '\r')
              delim = 2;
            else
              delim = 1;
            length -= delim;
          }
      }
    FString dataStr = new FString(sbuf, 0, length);
    if (handling == "split")
      {
        FString delimStr = new FString(sbuf, length-delim, delim);
        Object[] values = { dataStr, delimStr };
        return new Values(values);
      }
    else
      return dataStr;
  }
}
