package kawa.lang;
import gnu.expr.*;
import gnu.mapping.*;

/** This class is temporary - I plan to move things to Scheme instead. */

public abstract class Interpreter
{
  static public final Boolean  trueObject = Boolean.TRUE;
  static public final Boolean  falseObject = Boolean.FALSE;

  static public final Undefined undefinedObject = new Undefined().getInstance();
  static public final Object voidObject = Values.empty;

  static public final String quote_sym = "quote";
  static public final String unquote_sym = "unquote";
  static public final String unquotesplicing_sym = "unquote-splicing";
  static public final String quasiquote_sym = "quasiquote";

  public static final Boolean boolObject (boolean b)
  {
    return b ? Boolean.TRUE : Boolean.FALSE;
  }


  protected Environment environ;

  public Environment getEnvironment() { return environ; }
  public void setEnvironment(Environment environ) { this.environ = environ; }

  public void define(String sym, Object p)
  {
    environ.define (sym, p);
  }

  public Object lookup(String name)
  {
    return environ.get (name);
  }

  public abstract Object read (InPort in)
    throws java.io.IOException, gnu.text.SyntaxException;
  public abstract void print (Object obj, OutPort out);

  public abstract gnu.text.Lexer getLexer(InPort inp, gnu.text.SourceMessages messages);
}
