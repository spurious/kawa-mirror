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
    tab.set('<',  entry);
    tab.set('=',  entry);
    tab.set('>',  entry);
    tab.set('?',  entry);
    tab.set('@',  entry);
    tab.set('[',  entry);
    tab.set(']',  entry);
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

    // Scheme, CommonLisp only - Elisp has new ReaderVector(']')
    // We want '[' to be non-terminating for the sake of say '<char[]>'.
    tab.set('[',  ReaderParens.getInstance('[', ']',
					   ReadTable.NON_TERMINATING_MACRO));

    tab.set('\'', new ReaderQuote(Interpreter.quote_sym));
    tab.set('`',  new ReaderQuote(Interpreter.quasiquote_sym));
    tab.set(',',  new ReaderQuote(Interpreter.unquote_sym,
				 '@', Interpreter.unquotesplicing_sym));
    return tab;
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
