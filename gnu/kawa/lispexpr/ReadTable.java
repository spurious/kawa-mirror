// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.kawa.util.RangeTable;
import gnu.expr.Interpreter;

public class ReadTable extends RangeTable
{
  /** Kinds of characters. */
  public static final int ILLEGAL = 0;
  public static final int WHITESPACE = 1;
  public static final int CONSTITUENT = 2;
  public static final int SINGLE_ESCAPE = 3;
  public static final int MULTIPLE_ESCAPE = 4;
  public static final int TERMINATING_MACRO = 5;
  public static final int NON_TERMINATING_MACRO = 6;

  /** Default value to pass to setBracketMode() unless overridden. */
  public static int defaultBracketMode = -1;

  static ReadTable current = getInitial();

  public ReadTable()
  {
  }

  public static ReadTable getInitial()
  {
    ReadTable tab = new ReadTable();
    ReadTableEntry entry;
    entry = ReadTableEntry.getWhitespaceInstance();
    tab.set(' ',  entry);
    tab.set('\t', entry);
    tab.set('\n', entry);
    tab.set('\r', entry);
    tab.set('\f', entry);
    //tab.set('\v', entry);
    tab.set('|',  ReadTableEntry.getMultipleEscapeInstance());
    tab.set('\\', ReadTableEntry.getSingleEscapeInstance());
    tab.set('0',  '9',  ReadTableEntry.getDigitInstance());
    entry = ReadTableEntry.getConstituentInstance();
    tab.set('a',  'z',  entry);
    tab.set('A',  'Z',  entry);
    tab.set('!',  entry);
    tab.set('$',  entry);
    tab.set('%',  entry);
    tab.set('&',  entry);
    tab.set('*',  entry);
    tab.set('+',  entry);
    tab.set('-',  entry);
    tab.set('.',  entry);
    tab.set('/',  entry);
    tab.set(':',  entry);
    tab.set('=',  entry);
    tab.set('>',  entry);
    tab.set('?',  entry);
    tab.set('@',  entry);
    tab.set('^',  entry);
    tab.set('_',  entry);
    tab.set('{',  entry);
    tab.set('}',  entry);
    tab.set('~',  entry);
    tab.set('\177',entry);
    tab.set('\b', entry);
    tab.set('\"', new ReaderString());
    tab.set('#',  ReaderDispatch.getInitial());
    tab.set(';',  ReaderIgnoreRestOfLine.getInstance());
    tab.set('(',  ReaderParens.getInstance('(', ')'));

    tab.set('\'', new ReaderQuote(Interpreter.quote_sym));
    tab.set('`',  new ReaderQuote(Interpreter.quasiquote_sym));
    tab.set(',',  new ReaderQuote(Interpreter.unquote_sym,
				 '@', Interpreter.unquotesplicing_sym));

    tab.setBracketMode();  // Sets the entries for '[', ']', and '<'.

    return tab;
  }

  /** Specify how '[' and ']' (and '<') are handled.
   * The value -1 means that '[' and ']' are plain token constituents.
   * The value 0 means that '[' and ']' are equivalent to '(' and ')'.
   * The value 1 means that '[' and ']' are equivalent to '(' and ')', except
   * within a token starting with '<', in which case they are constituents.
   * This is so '[' is non-terminating when reading say '<char[]>'
   */
  public void setBracketMode(int mode)
  {
    if (mode <= 0)
      {
	ReadTableEntry token = ReadTableEntry.getConstituentInstance();
	set('<', token);
	if (mode < 0)
	  {
	    set('[', token);
	    set(']', token);
	  }
      }
    else
      set('<', new ReaderTypespec());
    if (mode >= 0)
      {
	set('[', ReaderParens.getInstance('[', ']'));
	remove(']');
      }
  }

  /** Specify how '[' and ']' are handled.
   * Overless overridden, uses defaultBracketMode. */
  public void setBracketMode()
  {
    setBracketMode(defaultBracketMode);
  }
  

  public static ReadTable getCurrent() { return current; }

  public ReadTableEntry lookup (int ch)
  {
    ReadTableEntry entry = (ReadTableEntry) lookup(ch, null);
    if (entry == null && ch >= 0 && ch < 0x10000)
      {
	if (Character.isDigit((char) ch))
	  {
	    entry = (ReadTableEntry) lookup('0', null);
	    if (entry == null)
	      {
		entry = ReadTableEntry.getDigitInstance();
		set(ch, ch, entry);
	      }
	    return entry;
	  }
	if (Character.isLowerCase((char) ch))
	  {
	    entry = (ReadTableEntry) lookup('a', null);
	    if (entry == null)
	      {
		entry = ReadTableEntry.getConstituentInstance();
		set(ch, ch, entry);
	      }
	    return entry;
	  }
	if (Character.isLetter((char) ch))
	  {
	    entry = (ReadTableEntry) lookup('A', null);
	    if (entry == null)
	      {
		entry = ReadTableEntry.getConstituentInstance();
		set(ch, ch, entry);
	      }
	    return entry;
	  }
      }
    return entry;
  }
}
