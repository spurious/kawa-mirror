package gnu.jemacs.lang;
import gnu.commonlisp.lang.*;
import gnu.kawa.lispexpr.*;
import gnu.mapping.*;
import gnu.text.*;

/** A class to read Emacs Lisp forms (S-expressions). */

public class ELispReader extends CLispReader
{
  public ELispReader (InPort port)
  {
    super(port);
  }

  public ELispReader(InPort port, SourceMessages messages)
  {
    super(port, messages);
  }
  
  protected ReadTable getReadTable () { return elispReadTable; }

  public static Object readObject(InPort port)
      throws java.io.IOException, SyntaxException
  {
    return (new ELispReader(port)).readObject();
  }

  public static ReadTable elispReadTable;
  static
  {
    elispReadTable = ReadTable.getInitial();
    elispReadTable.set('[', new ReaderVector(']'));
    elispReadTable.remove(']');
    elispReadTable.set('?', new ELispReadTableEntry('?'));
  }
}

class ELispReadTableEntry extends ReaderDispatchMisc
{
  public ELispReadTableEntry(int code)
  {
    super(code);
  }

  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    LispReader reader = (LispReader) in;
    if (code >= 0)
      ch = code;
    switch (ch)
      {
      case '?':
	ch = reader.read();
	if (ch == '\\')
	  {
	    ch = reader.read();
	    if (ch != ' ' && ch >= 0)
	      ch = reader.readEscape(ch);
	  }
	if (ch < 0)
	  {
	    reader.error("unexpected EOF in character literal");
	    ch = '?';
	  }
	return ELisp.getCharacter(ch);
      }
    reader.error("unexpected dispatch character");
    return null;
  }
}
