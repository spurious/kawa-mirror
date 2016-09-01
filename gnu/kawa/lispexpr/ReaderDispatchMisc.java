// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.*;
import gnu.mapping.*;
import gnu.bytecode.PrimType;
import gnu.bytecode.Type;
import gnu.lists.*;
import gnu.kawa.io.InPort;
import gnu.kawa.io.OutPort;
import gnu.kawa.util.GeneralHashTable;
/* #ifdef use:java.util.regex */
import java.util.regex.*;
/* #endif */

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
    InPort port;
    int length;
    String name;
    if (code >= 0)
      ch = code;
    switch (ch)
      {
      case '*':
        name = reader.readTokenString(-1, ReadTable.getCurrent());
        int nlen = name.length();
        int len = nlen;
        if (count >= 0) {
            if (nlen > count)
                in.error("too many bits in bit vector");
            len = count;
        }
        boolean[] arr = new boolean[len];
        char prev = '0';
        for (int i = 0; i < len; i++) {
            char c = i < nlen ? name.charAt(i) : prev;
            prev = c;
            if (c == '1' || c == 't' || c == 'F')
                arr[i] = true;
            else if (! (c == '0' || c == 'f' || c == 'F')) {
                prev = '0';
                in.error("invalid character (at offset "+i+") in bitvector");
            }
        }
        return new BitVector(arr);
      case ':':
	// Handle Guile-style keyword syntax: '#:KEYWORD'
	// Note this conflicts with Common Lisp uninterned symbols.  FIXME
        name = reader.readTokenString(-1, ReadTable.getCurrent());
        return gnu.expr.Keyword.make(name.intern());
      case '\\':
	return LispReader.readCharacter(reader);
      case '!':
	return LispReader.readSpecial(reader);
      case 'T':
      case 'F':
          int startPos = reader.tokenBufferLength;
          while (ch >= 0 && Character.isLetterOrDigit(ch)) {
              reader.tokenBufferAppend(ch);
              ch = reader.read();
          }
          reader.unread(ch);
          name = new String(reader.tokenBuffer, startPos,
                            reader.tokenBufferLength - startPos);
          reader.tokenBufferLength = startPos;
          String nameLC = name.toLowerCase();
          if (nameLC.equals("t") || nameLC.equals("true"))
              return Boolean.TRUE;
          if (nameLC.equals("f") || nameLC.equals("false"))
              return Boolean.FALSE;
          PrimType elementType;
          if (nameLC.equals("f32"))
              elementType = LangPrimType.floatType;
          else if (nameLC.equals("f64"))
              elementType = LangPrimType.doubleType;
          else
            {
              in.error("unexpected characters following '#'");
              return Boolean.FALSE;
            }
          return LispReader.readGeneralArray(reader, count, elementType);
      case 'S':
      case 'U':
          int size = reader.readIntDigits();
          switch (size) {
          case 8:
              elementType = ch == 'U' ? LangPrimType.unsignedByteType
                  : LangPrimType.byteType;
              break;
          case 16:
              elementType = ch == 'U' ? LangPrimType.unsignedShortType
                  : LangPrimType.shortType;
              break;
           case 32:
              elementType = ch == 'U' ? LangPrimType.unsignedIntType
                  : LangPrimType.intType;
              break;
           case 64:
              elementType = ch == 'U' ? LangPrimType.unsignedLongType
                  : LangPrimType.longType;
              break;
          default:
              in.error("expected 8, 16, 32, or 64 after #S or #U");
              elementType = null;
          }
          return LispReader.readGeneralArray(reader, count, elementType);
      case 'R':
	if (count > 36)
	  {
            StringBuilder sbuf = new StringBuilder("the radix ");
            if (count < Integer.MAX_VALUE)
              {
                sbuf.append(count);
                sbuf.append(' ');
              }
            sbuf.append("is too big (max is 36)");
            in.error(sbuf.toString());
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
      case 'A':
          return LispReader.readGeneralArray(reader, count, null);
      /* #ifdef use:java.util.regex */
      case '/':
      return readRegex(in, ch, count);
      /* #endif */
      case ';':
	port = reader.getPort();
	if (port instanceof InPort)
	  {
	    saveReadState = ((InPort) port).readState;
	    ((InPort) port).readState = ';';
	  }
	try
	  {
            reader.readObject();
	  }
	finally
	  {
	    if (port instanceof InPort)
	      ((InPort) port).readState = saveReadState;
	  }
	return Values.empty;
      case ',':
        return ReaderDispatchSyntaxQuote.readNamedConstructor(reader);
      case '=':
        return reader.readObject(count, false);
      case '#':
        if (in instanceof LispReader)
          {
            GeneralHashTable<Integer,Object> map
                = ((LispReader) in).sharedStructureTable;
            if (map != null)
              {
                Integer key = Integer.valueOf(count);
                Object object = map.get(key, in);
                if (object != in)
                  return object;
              }
          }
        in.error("an unrecognized #n# back-reference was read");
	return Boolean.FALSE;
      default:
	in.error("An invalid #-construct was read.");
	return Values.empty;
      }
  }

  /* #ifdef use:java.util.regex */
  public static Pattern readRegex (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    int startPos = in.tokenBufferLength;
    InPort port = in.getPort();
    char saveReadState = '\0';
    int flags = 0;
    if (port instanceof InPort)
      {
        saveReadState = ((InPort) port).readState;
        ((InPort) port).readState = '/';
      }
    try
      {
        for (;;)
          {
            int next;

            int c = port.read();
            if (c < 0)
              in.eofError("unexpected EOF in regex literal");
	    if (c == ch)
              break;
            if (c == '\\')
              {
                c = port.read();
                if ((c == ' ' ||  c == '\t' || c == '\r' || c == '\n')
                    && in instanceof LispReader)
                  {
                    c = ((LispReader) in).readEscape(c);
                    if (c == -2)
                      continue;
                  }
                if (c < 0)
                  in.eofError("unexpected EOF in regex literal");
                if (c != ch)
                  in.tokenBufferAppend('\\');
              }
            in.tokenBufferAppend(c);
          }
        String pattern = new String(in.tokenBuffer, startPos,
                                    in.tokenBufferLength - startPos);
        for (;;)
          {
            int c = in.peek();
            if (c == 'i' || c == 'I')
              flags |= Pattern.CASE_INSENSITIVE|Pattern.UNICODE_CASE;
            else if (c == 's' || c == 'S')
              flags |= Pattern.DOTALL;
            else if (c == 'm' || c == 'M')
              flags |= Pattern.MULTILINE;
            /* Think this through more before adding this feature:
            Perhaps we should use the 'x' handling from
            gnu.xquery.util.StringUtils.makePattern (which is
            smart enogh to handle space in character classes).
            Perhaps we should handle Scheme comments?

            else if (c == 'x' || c == 'X')
              flags |= Pattern.COMMENTS;
            */
            else if (Character.isLetter(c))
              {
                in.error("unrecognized regex option '"+((char) c)+'\'');
              }
            else
              break;
            in.skip();
          }
        return Pattern.compile(pattern, flags);
      }
    finally
      {
        in.tokenBufferLength = startPos;
        if (port instanceof InPort)
          ((InPort) port).readState = saveReadState;
      }
  }
  /* #endif */
}
