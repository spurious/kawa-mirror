// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.Lexer;
import gnu.text.SyntaxException;

public abstract class ReadTableEntry
{
  public static final ReadTableEntry illegal
    = new ReaderMisc(ReadTable.ILLEGAL);
  public static final ReadTableEntry whitespace
    = new ReaderMisc(ReadTable.WHITESPACE);
  public static final ReadTableEntry singleEscape
    = new ReaderMisc(ReadTable.SINGLE_ESCAPE);
  public static final ReadTableEntry multipleEscape
    = new ReaderMisc(ReadTable.MULTIPLE_ESCAPE);
  public static final ReadTableEntry constituent
    = new ReaderMisc(ReadTable.CONSTITUENT);
  public static final ReadTableEntry brace // special handling for '{' and '}'
    = new ReaderMisc(ReadTable.CONSTITUENT);

  public static ReadTableEntry getIllegalInstance()
  { return illegal; }
  public static ReadTableEntry getWhitespaceInstance()
  { return whitespace; }
  public static ReadTableEntry getSingleEscapeInstance()
  { return singleEscape; }
  public static ReadTableEntry getMultipleEscapeInstance()
  { return multipleEscape; }
  public static ReadTableEntry getDigitInstance()
  { return constituent; }
  public static ReadTableEntry getConstituentInstance()
  { return constituent; }

  public int getKind()
  {
    return ReadTable.TERMINATING_MACRO;
  }

  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    throw new Error("invalid character");
  }

}
