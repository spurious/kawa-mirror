package kawa.lang;

/** This class is temporary - I plan to move things to Scheme instead. */

public abstract class Interpreter
{
  static public final Boolean  trueObject = Boolean.TRUE;
  static public final Boolean  falseObject = Boolean.FALSE;

  static public final Undefined undefinedObject = new kawa.lang.Undefined();
  static public final Object voidObject = Values.empty;

  static public final String quote_sym = "quote";
  static public final String unquote_sym = "unquote";
  static public final String unquotesplicing_sym = "unquote-splicing";
  static public final String quasiquote_sym = "quasiquote";

  public static final Boolean boolObject (boolean b)
  {
    return b ? Boolean.TRUE : Boolean.FALSE;
  }


  public /* FOR NOW */ Environment environ;

  public abstract Object read (InPort in)
    throws java.io.IOException, kawa.lang.ReadError;
  public abstract void print (Object obj, OutPort out);
}
