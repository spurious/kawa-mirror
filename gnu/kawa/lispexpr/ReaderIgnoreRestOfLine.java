// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.Lexer;
import gnu.text.SyntaxException;
import gnu.lists.Sequence;
import gnu.mapping.Values;

public class ReaderIgnoreRestOfLine extends ReadTableEntry
{
  static ReaderIgnoreRestOfLine instance = new ReaderIgnoreRestOfLine();

  public static ReaderIgnoreRestOfLine getInstance() { return instance; }

  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    do
      {
	ch = in.read();
	if (ch < 0)
	  return Sequence.eofValue;
      } while (ch != '\n' && ch!= '\r');
    return Values.empty;
  }
}
