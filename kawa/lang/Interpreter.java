package kawa.lang;

/** This class is temporary - I plan to move things to Scheme instead. */

public class Interpreter
{
  // Replace by Boolean.TRUE and Boolean.FALSE in JDK 1.1.  FIXME.
  static public Boolean  trueObject = new Boolean(true);
  static public Boolean  falseObject = new Boolean(false);

  static public Undefined undefinedObject = new kawa.lang.Undefined();
  static public Object voidObject = Values.empty;

  static public Symbol quote_sym = Symbol.make ("quote");
  static public Symbol unquote_sym = Symbol.make ("unquote");
  static public Symbol unquotesplicing_sym = Symbol.make ("unquote-splicing");
  static public Symbol quasiquote_sym = Symbol.intern ("quasiquote");

  public static final Boolean boolObject (boolean b)
  {
    return b ? trueObject : falseObject;
  }

}
