package kawa.lang;

/** This class is temporary - I plan to move things to Scheme instead. */

public class Interpreter
{
  static public Boolean  trueObject = new Boolean(true);
  static public Boolean  falseObject = new Boolean(false);

  static public List nullObject = List.Empty;
  static public Undefined undefinedObject = new kawa.lang.Undefined();
  static public Undefined voidObject = new kawa.lang.Undefined();

  static public Symbol quote_sym = Symbol.make ("quote");
  static public Symbol unquote_sym = Symbol.make ("unquote");
  static public Symbol unquotesplicing_sym = Symbol.make ("unquote-splicing");
  static public Symbol quasiquote_sym = Symbol.intern ("quasiquote");

  public static final Boolean boolObject (boolean b)
  {
    return b ? trueObject : falseObject;
  }

}
