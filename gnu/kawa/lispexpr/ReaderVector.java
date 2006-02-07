// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.*;
import gnu.mapping.InPort;
import gnu.mapping.Values;
import gnu.lists.FVector;

public class ReaderVector extends ReadTableEntry
{
  char close;

  public ReaderVector(char close)
  {
    this.close = close;
  }

  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    return readVector((LispReader) in, in.getPort(), count, close);
  }

  public static FVector readVector(LispReader lexer, LineBufferedReader port, int count, char close)
    throws java.io.IOException, SyntaxException
  {
    char saveReadState = ' ';
    if (port instanceof InPort)
      {	
	saveReadState = ((InPort) port).readState;
	((InPort) port).readState = close == ']' ? '[' : '(';
      }
     try
       {
	 java.util.Vector vec = new java.util.Vector();
         ReadTable rtable = ReadTable.getCurrent();
	 for (;;)
	   {
	     int ch = lexer.read();
	     if (ch < 0)
	       lexer.eofError("unexpected EOF in vector");
	     if (ch == close)
	       break;
	     Object value = lexer.readValues(ch, rtable);
	     if (value instanceof Values)
	       {
		 Object[] values = ((Values) value).getValues();
		 int n = values.length;
		 for (int i = 0;  i < n;  i++)
		   vec.addElement(values[i]);
	       }
	     else
	       {
		 if (value == gnu.expr.QuoteExp.voidExp)
		   value = Values.empty;
		 vec.addElement(value);
	       }
	   }
	 Object[] objs = new Object[vec.size()];
	 vec.copyInto(objs);
	 return new FVector(objs);
       }
     finally
       {
	 if (port instanceof InPort)
	   ((InPort) port).readState = saveReadState;
       }
  }
}
