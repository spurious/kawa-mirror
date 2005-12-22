// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.Lexer;
import gnu.text.SyntaxException;
import gnu.lists.PairWithPosition;

public class ReaderQuote extends ReadTableEntry
{
  Object magicSymbol;
  char next;
  Object magicSymbol2;

  /** Read an expression EXP and return (magicSymbol EXP). */
  public ReaderQuote(Object magicSymbol)
  {
    this.magicSymbol = magicSymbol;
  }

  /** If the next character is 'next' read an expression EXP
   * and return (magicSymbol2 EXP).
   * Otherwise, read EXP and return (magicSymbol EXP). */
  public ReaderQuote(Object magicSymbol, char next, Object magicSymbol2)
  {
    this.next = next;
    this.magicSymbol = magicSymbol;
    this.magicSymbol2 = magicSymbol2;
  }

  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    LispReader reader = (LispReader) in;
    String file = reader.getName();
    int line1 = reader.getLineNumber() + 1;
    int column1 = reader.getColumnNumber() + 1;
    Object magic = magicSymbol;
    if (next != '\0')
      {
	ch = reader.read();
	if (ch == next)
	  magic = magicSymbol2;
	else if (ch >= 0)
	  reader.unread(ch);
      }
    int line2 = reader.getLineNumber() + 1;
    int column2 = reader.getColumnNumber() + 1;
    Object operand = reader.readObject();
    return PairWithPosition.make(magic,
                                 PairWithPosition.make(operand,
                                                       reader.makeNil(),
                                                       file, line2, column2),
                                 file, line1, column1);
  }
}
