package gnu.kawa.lispexpr;
import gnu.math.*;
import java.io.*;
import gnu.text.Char;
import gnu.text.SyntaxException;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.SourceMessages;
import gnu.lists.*;
import java.util.Vector;

/** A class to read Scheme forms (S-expressions). */

public class ScmRead extends LispReader
{
  public ScmRead(InPort port)
  {
    super(port);
  }
  
  public ScmRead(InPort port, SourceMessages messages)
  {
    super(port, messages);
  }
  
  protected ReadTable getReadTable () { return scmReadTable; }

  protected Object makeNil ()
  {
    return LList.Empty;
  }

  protected Object makePair (Object car, int line, int column)
  {
    PairWithPosition pair = new PairWithPosition (port, car, LList.Empty);
    pair.setLine(line + 1, column + 1);
    pair.setFile (port.getName ());
    return pair;
  }

  protected void setCdr (Object pair, Object cdr)
  {
    ((Pair) pair).cdr = cdr;
  }

  /*
  public Object readNumber (int c, int radix)
       throws java.io.IOException
  {
    char exactness = ' ';
    int explicit_radix = 0;
    while (c == '#')
      {
	c = read ();
	switch (c)
	  {
	  case 'e':
	  case 'i':
	    if (exactness != ' ')
	      error("extra exactness specifier (#" + (char)c + ")");
	    exactness = (char) c;
	    break;
	  case 'x':
	  case 'd':
	  case 'o':
	  case 'b':
	    if (explicit_radix != 0)
	      error("extra radix specifier (#" + (char)c + ")");
	    explicit_radix = c == 'x' ? 16 : c == 'd' ? 10 : c == 'o' ? 8 : 2;
	    break;
	  default:
	    error( "unrecognized #-construct in number");
	  }
	c = read ();
      }
  }
  */

  /* Signal error(message) and skip to the end of this word. */
  RealNum numError(String message)
    throws java.io.IOException
  {
    error(message);
    for (;;)
      {
	int c = read();
	if (c < 0) break;
	if (isDelimiter((char) c))
	  {
	    unread(c);
	    break;
	  }
      }
    return IntNum.zero();
  }


  public static Object readObject(InPort port)
      throws java.io.IOException, SyntaxException
  {
    return (new ScmRead(port)).readObject();
  }

  public static ReadTable scmReadTable;
  static
  {
    scmReadTable = ReadTable.getInitial();
  }
}
