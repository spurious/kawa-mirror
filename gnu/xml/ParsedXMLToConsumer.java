// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.Consumer;
import gnu.lists.TreeList;
import java.net.URL;

public class ParsedXMLToConsumer extends ParsedXMLHandler
{
  public Consumer out;
  String[] names = new String[10];
  int depth = 0;

  public ParsedXMLToConsumer(Consumer out)
  {
    this.out = out;
  }

  public void emitCharacters(char[] data, int start, int length)
  {
    out.write(data, start, length);
  }

  public void emitBeginElement(char[] data, int start, int count)
  {
    String name = new String(data, start, count);
    out.beginGroup(name, name);
    if (depth >= names.length)
      {
	String[] tmp = new String[2 * depth];
	System.arraycopy(names, 0, tmp, 0, depth);
	names = tmp;
      }
    names[depth++] = name;
  }

  boolean inAttribute;

  public void emitBeginAttribute(char[] data, int start, int count)
  {
    if (inAttribute)
      out.endAttribute();
    String name = new String(data, start, count);
    out.beginAttribute(name, name);
    inAttribute = true;
  }

  public void emitEndAttributes()
  {
    if (inAttribute)
      out.endAttribute();
    inAttribute = false;
  }

  public void emitEndElement(char[] data, int start, int length)
  {
    String name = (data == null ? names[depth-1] 
		   : new String(data, start, length));
    names[depth-1] = null;  // For the sake of Gc.
    depth--;
    out.endGroup(name);
  }

  /** Handles the predefined entities, such as "&lt;" and "&quot;". */
  public void emitEntityReference(char[] name, int start, int length)
  {
    char c0 = name[start];
    char ch = '?';
    if (length == 2 && name[start+1] == 't')
      {
	
	if (c0 == 'l')
	  ch = '<';
	else if (c0 == 'g')
	  ch = '>';
      }
    else if (length == 3)
      {
	if (c0 == 'a' && name[start+1] == 'm' && name[start+2] == 'p')
	  ch = '&';
      }
    else if (length == 4)
      {
	char c1 = name[start+1];
	char c2 = name[start+2];
	char c3 = name[start+3];
	if (c0 == 'q' && c1 == 'u' && c2 == 'o' && c3 == 't')
	  ch = '"';
	else if (c0 == 'a' && c1 == 'p' && c2 == 'o' && c3 == 's')
	  ch = '\'';
      }
    out.writeChar(ch);
  }

  public void emitCharacterReference(int value, char[] name, int start, int length)
  {
    out.writeChar(value);
  }

  public void emitComment(char[] data, int start, int length)
  {
    // FIXME?
  }

  /** Process a processing incluction. */
  public void emitProcessingInstruction(char[] buffer,
                                        int target, int tlength,
                                        int data, int dlength)
  {
    // FIXME?
  }

  /** Process a DOCTYPE declaration. */
  public void emitDoctypeDecl(char[] buffer,
                              int target, int tlength,
                              int data, int dlength)
  {
    // FIXME?
  }
}
