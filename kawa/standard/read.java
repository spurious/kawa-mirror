package kawa.standard;
import gnu.kawa.lispexpr.ScmRead;
import gnu.mapping.*;

public class read extends Procedure0or1 {
  public final Object apply0 ()
  {
    return apply1 (InPort.inDefault());
  }

  public final Object apply1 (Object arg1)
  {
    if (! (arg1 instanceof InPort))
      throw new WrongType (this.name(), 0, "input port");
    try
      {
	ScmRead lexer = new ScmRead((InPort)arg1);
	Object result = lexer.readObject((InPort)arg1);
	gnu.text.SourceError errors = lexer.getErrors();
	if (errors != null)
	  {
	    lexer.checkErrors(null, 0);
	    throw new RuntimeException("syntax error in read: "
				       + errors.toString());
	  }
	return result;
      }
    catch (gnu.text.SyntaxException e)
      {
	throw new RuntimeException("syntax error in read: " + e.toString ());
      }
    catch (java.io.IOException e)
      {
	throw new WrappedException(e);
      }
  }
}
