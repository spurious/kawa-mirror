package gnu.kawa.xml;
import gnu.kawa.util.Consumer;
import gnu.kawa.util.TreeList;

public class ParsedXMLToConsumer extends ParsedXMLHandler
{
  public Consumer out;
  String[] names = new String[10];
  int depth = 0;

  public ParsedXMLToConsumer(Consumer out)
  {
    this.out = out;
  }

  public static void parse(java.net.URL url, Consumer out)
    throws java.io.IOException
  {
    XMLParser parser = new XMLParser(url, new ParsedXMLToConsumer(out));
    parser.parse();
  }

  public static TreeList parse(java.net.URL url)
    throws java.io.IOException
  {
    TreeList doc = new TreeList();
    XMLParser parser = new XMLParser(url, new ParsedXMLToConsumer(doc));
    parser.parse();
    return doc;
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

  public void emitBeginAttribute(char[] data, int start, int count)
  {
    String name = new String(data, start, count);
    out.beginAttribute(name, name);
  }

  public void emitEndAttributes()
  {
    out.endAttributes();
  }

  public void emitEndElement(char[] data, int start, int length)
  {
    String name = (data == null ? names[depth-1] 
		   : new String(data, start, length));
    names[depth-1] = null;  // For the sake of Gc.
    depth--;
    out.endGroup(name);
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
