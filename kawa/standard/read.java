package kawa.standard;
import gnu.kawa.lispexpr.LispReader;
import gnu.mapping.*;
import gnu.text.*;

public class read extends Procedure0or1
{
  public final Object apply0 ()
    throws Throwable
  {
    return apply1 (InPort.inDefault());
  }

  public final Object apply1 (Object arg1)
    throws Throwable
  {
    if (! (arg1 instanceof InPort))
      throw new WrongType (this, 0, arg1, "input port");
    LispReader lexer = new LispReader((InPort)arg1);
    try
      {
	Object result = lexer.readObject();
	if (lexer.seenErrors())
	  throw new SyntaxException(lexer.getMessages());
	return result;
      }
    catch (SyntaxException ex)
      {
	ex.setHeader("syntax error in read:");
	throw ex;
      }
  }
}
