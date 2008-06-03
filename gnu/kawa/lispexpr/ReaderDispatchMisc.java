// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.*;
import gnu.mapping.InPort;
import gnu.mapping.Values;
import gnu.mapping.Procedure;
import gnu.bytecode.Type;
import gnu.lists.*;

public class ReaderDispatchMisc extends ReadTableEntry
{
  /** A code which specifies which particular reader-action to perform.
   * The code is one the CommonLisp or Scheme '#' reader characters.
   * For example, if code=='x' then read a hexadecimal integer.
   * If code==-1, perform the standard action for the character read. */
  protected int code;

  private static ReaderDispatchMisc instance = new ReaderDispatchMisc();

  public static ReaderDispatchMisc getInstance() { return instance; }

  public ReaderDispatchMisc()
  {
    code = -1;
  }

  public ReaderDispatchMisc(int code)
  {
    this.code = code;
  }

  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    LispReader reader = (LispReader) in;
    char saveReadState = '\0';
    LineBufferedReader port;
    int length;
    String name;
    if (code >= 0)
      ch = code;
    switch (ch)
      {
      case ':':
	// Handle Guile-style keyword syntax: '#:KEYWORD'
	// Note this conflicts with Common Lisp uninterned symbols.  FIXME
	int startPos = reader.tokenBufferLength;
	reader.readToken(reader.read(), 'P', ReadTable.getCurrent());
	length = reader.tokenBufferLength - startPos;
	name = new String(reader.tokenBuffer, startPos, length);
	reader.tokenBufferLength = startPos;
	return gnu.expr.Keyword.make(name.intern());
      case '\\':
	return LispReader.readCharacter(reader);
      case '!':
	return LispReader.readSpecial(reader);
      case 'T':
	return Boolean.TRUE;
      case 'F':
	ch = in.peek();
	if (Character.isDigit((char) ch))
	  return LispReader.readSimpleVector(reader, 'F');
	return Boolean.FALSE;
      case 'S':
      case 'U':
	return LispReader.readSimpleVector(reader, (char) ch);
      case 'R':
	if (count > 36)
	  {
	    in.error("the radix "+count+" is too big (max is 36)");
	    count = 36;
	  }
	return LispReader.readNumberWithRadix(0, reader, count);
      case 'X':
	return LispReader.readNumberWithRadix(0, reader, 16);
      case 'D':
	return LispReader.readNumberWithRadix(0, reader, 10);
      case 'O':
	return LispReader.readNumberWithRadix(0, reader, 8);
      case 'B':
	return LispReader.readNumberWithRadix(0, reader, 2);
      case 'I':
      case 'E':
	reader.tokenBufferAppend('#');
	reader.tokenBufferAppend(ch);
	return LispReader.readNumberWithRadix(2, reader, 0);
      case '|':
	port = reader.getPort();
	if (port instanceof InPort)
	  {
	    saveReadState = ((InPort) port).readState;
	    ((InPort) port).readState = '|';
	  }
	try
	  {
	    reader.readNestedComment('#', '|');
	  }
	finally
	  {
	    if (port instanceof InPort)
	      ((InPort) port).readState = saveReadState;
	  }
	return Values.empty;
      case ',':
	port = reader.getPort();
        Object list;
        if (port.peek() == '('
            && ((length
                 = LList.listLength(list = reader.readObject(), false))
                > 0)
            && ((Pair) list).getCar() instanceof String)
          {
            name = (String) ((Pair) list).getCar();
            Object proc = ReadTable.getCurrent().getReaderCtor(name);
            if (proc == null)
              in.error("unknown reader constructor "+name);
            else if (! (proc instanceof Procedure || proc instanceof Type))
              in.error("reader constructor must be procedure or type name");
            else
              {
                length--;  // Subtract 1 for the constructor name.
                int parg = proc instanceof Type ? 1 : 0;
                Object[] args = new Object[parg+length];
                Object argList = ((Pair) list).getCdr();
                for (int i = 0;  i < length;  i++)
                  {
                    Pair pair = (Pair) argList;
                    args[parg+i] = pair.getCar();
                    argList = pair.getCdr();
                  }
                try
                  {
                    if (parg > 0)
                      {
                        args[0] = proc;
                        return gnu.kawa.reflect.Invoke.make.applyN(args);
                      }
                    return ((Procedure) proc).applyN(args);
                  }
                catch (Throwable ex)
                  {
                    in.error("caught "+ex+" applying reader constructor "+name);
                  }
              }
          }
        else
          in.error("a non-empty list starting with a symbol must follow #,");
	return Boolean.FALSE;
      default:
	in.error("An invalid #-construct was read.");
	return Values.empty;
      }
  }
}
