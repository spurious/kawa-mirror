// Copyright (c) 2001  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.mapping.*;
import gnu.lists.*;
import java.io.PrintWriter;
import gnu.text.Char;

/** Handle formatted output for Lisp-like languages. */

public class DisplayFormat extends Procedure1or2 implements FormatToConsumer
{
  public DisplayFormat(boolean readable, char language)
  {
    this.readable = readable;
    this.language = language;
  }

  boolean readable;

  /** 'S' is Scheme-style; 'C' is CommonLisp-style;  'E' is Emacs-style.
   * Note Emacs has its own sub-class gnu.jemacs.lang.Print. */
  char language;

  public boolean getReadableOutput () { return readable; }

  protected void write(String str, Consumer out)
  {
    if (out instanceof OutPort)
      ((OutPort) out).write(str);
    else
      out.writeChars(str);
  }

  public void writeBoolean(boolean v, Consumer out)
  {
    write (language == 'S' ? (v ? "#t" : "#f") : (v ? "t" : "nil"), out);
  }

  public void writeChar(int v, Consumer out)
  {
    if (! getReadableOutput ())
      out.writeChar(v);
    else
      write(Char.toScmReadableString(v), out);
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
	out.writeObject(pair.car);
	list = pair.cdr;
      }
    if (list != LList.Empty)
      {
	out.writeSpaceFill();
	out.writeChars(". ");
	out.writeObject(list);
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
    else if (obj instanceof CharSequence)
      {
	CharSequence str = (CharSequence) obj;
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
	    if (! vec.consumeNext(ipos, null, out))
	      break;
	  }
	if (out instanceof OutPort)
	  ((OutPort) out).endLogicalBlock(end);
	else
	  write (end, out);
      }
    else if (obj instanceof Consumable)
      ((Consumable) obj).consume(out);
    else if (obj instanceof Printable && out instanceof PrintWriter)
      ((Printable) obj).print((PrintWriter) out);
    else if (obj == null)
      write("#!null", out);
    else
      write (obj.toString(), out);
  }

  public Object apply1 (Object arg1)
  {
    format (arg1, OutPort.outDefault());
    return Values.empty;
  }

  public Object apply2 (Object arg1,Object arg2)
  {
    format (arg1, (Consumer) arg2); 
    return Values.empty;
  }

  public void format (Object value, Consumer out)
  {
    if (out instanceof OutPort)
      {
	OutPort pout = (OutPort) out;
	FormatToConsumer saveFormat = pout.objectFormat;
	try
	  {
	    pout.objectFormat = this;
	    out.writeObject(value);
	  }
	finally
	  {
	    pout.objectFormat = saveFormat;
	  }
      }
    else
      out.writeObject(value);
  }
}
