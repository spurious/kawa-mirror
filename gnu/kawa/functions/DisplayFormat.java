// Copyright (c) 2001, 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.mapping.*;
import gnu.lists.*;
import java.io.PrintWriter;
import gnu.text.Char;
import gnu.kawa.util.AbstractFormat;

/** Handle formatted output for Lisp-like languages. */

public class DisplayFormat extends AbstractFormat
{
  /** Create a new instance.
   * @param readable if output should be formatted so it could be read
   *   back in again, for example strings shoudl be quoted.
   * @param language the programming language style to use, where
   *   'S' is Scheme, 'C' is Common Lisp, and 'E' is Emacs Lisp.
   */
  public DisplayFormat(boolean readable, char language)
  {
    this.readable = readable;
    this.language = language;
  }

  public static DisplayFormat getEmacsLispFormat(boolean readable)
  {
    return new DisplayFormat(readable, 'E');
  }

  public static DisplayFormat getCommonLispFormat(boolean readable)
  {
    return new DisplayFormat(readable, 'C');
  }

  public static DisplayFormat getSchemeFormat(boolean readable)
  {
    return new DisplayFormat(readable, 'S');
  }

  boolean readable;

  /** 'S' is Scheme-style; 'C' is CommonLisp-style;  'E' is Emacs-style.
   * Note Emacs has its own sub-class gnu.jemacs.lang.Print. */
  char language;

  public boolean getReadableOutput () { return readable; }

  public void writeBoolean(boolean v, Consumer out)
  {
    write (language == 'S' ? (v ? "#t" : "#f") : (v ? "t" : "nil"), out);
  }

  public void writeChar(int v, Consumer out)
  {
    if (! getReadableOutput ())
      out.writeChar(v);
    else
      {
	if (language == 'E'
	    && v > ' ')
	  {
	    out.writeChar('?');
	    out.writeChar(v);
	  }
	// else if (language == 'E') ...
	else
	  write(Char.toScmReadableString(v), out);
      }
  }

  public void writeList(LList value, OutPort out)
  {
    Object list = value;
    out.startLogicalBlock("(", false, ")");
    while (list instanceof Pair)
      {
	if (list != value)
	  out.writeSpaceFill();
	Pair pair = (Pair) list;
	writeObject(pair.car, (Consumer) out);
	list = pair.cdr;
      }
    if (list != LList.Empty)
      {
	out.writeSpaceFill();
	out.writeChars(". ");
	writeObject(LList.checkNonList(list), (Consumer) out);
      }
    out.endLogicalBlock(")");
  }

  public void writeObject(Object obj, Consumer out)
  {
    if (obj instanceof Boolean)
      writeBoolean(((Boolean)obj).booleanValue(), out);
    else if (obj instanceof Char)
      writeChar(((Char)obj).charValue(), out);
    else if (obj instanceof Character)
      writeChar(((Character)obj).charValue(), out);
    else if (obj instanceof Symbol)
      writeObject(obj.toString(), out);
    else if (obj instanceof CharSeq)
      {
	CharSeq str = (CharSeq) obj;
	if (getReadableOutput () && out instanceof PrintWriter)
	  Strings.printQuoted(str, (PrintWriter) out, 0);
	else if (obj instanceof FString) // FIXME Do we need this case?
	  {
	    FString fstr = (FString) obj;
	    out.write(fstr.data, 0, fstr.size());
	  }
	else
	  str.consume(0, str.size(), out);
      }
    else if (obj instanceof LList && out instanceof OutPort)
      writeList((LList) obj, (OutPort) out);
    else if (obj instanceof SimpleVector)
      {
	SimpleVector vec = (SimpleVector) obj;
	String tag = vec.getTag();
	String start, end;
	if (language == 'E')
	  {
	    start = "[";
	    end = "]";
	  }
	else
	  {
	    start = tag == null ? "#(" : ("#" + tag + "(");
	    end = ")";
	  }
	if (out instanceof OutPort)
	  ((OutPort) out).startLogicalBlock(start, false, end);
	else
	  write (start, out);
	int endpos = vec.size() << 1;
	for (int ipos = 0;  ipos < endpos;  ipos += 2)
	  {
	    if (ipos > 0 && out instanceof OutPort)
	      ((OutPort) out).writeSpaceFill();
	    if (! vec.consumeNext(ipos, out))
	      break;
	  }
	if (out instanceof OutPort)
	  ((OutPort) out).endLogicalBlock(end);
	else
	  write (end, out);
      }
    else if (obj instanceof Array)
      {
	write((Array) obj, 0, 0, out);
      }
    else if (obj instanceof Consumable)
      ((Consumable) obj).consume(out);
    else if (obj instanceof Printable && out instanceof PrintWriter)
      ((Printable) obj).print((PrintWriter) out);
    else
      {
	String asString = obj != null ? obj.toString() : null;
	if (asString == null)
	  write("#!null", out);
	else
	  write(asString, out);
      }
  }

  /** Recursive helper method for writing out Array (sub-) objects.
   * @param array the Array to write out (part of).
   * @param index the row-major index to start
   * @param level the recurssion level, from 0 to array.rank()-1.
   * @param out the destination
   */
  int write(Array array, int index, int level, Consumer out)
  {
    int rank = array.rank();
    int count = 0;
    String start = level > 0 ? "("
      : rank == 1 ? "#("
      : "#" + rank + "a(";
    if (out instanceof OutPort)
      ((OutPort) out).startLogicalBlock(start, false, ")");
    else
      write (start, out);
    if (rank > 0)
      {
	int size = array.getSize(level);
	level++;
	for (int i = 0;  i < size;  i++)
	  {
	    if (i > 0 && out instanceof OutPort)
	      ((OutPort) out).writeSpaceFill();
	    int step;
	    if (level == rank)
	      {
		writeObject(array.getRowMajor(index), out);
		step = 1;
	      }
	    else
	      step = write(array, index, level, out);
	    index += step;
	    count += step;
	  }
      }
    if (out instanceof OutPort)
      ((OutPort) out).endLogicalBlock(")");
    else
      write(")", out);
    return count;
  }
}
