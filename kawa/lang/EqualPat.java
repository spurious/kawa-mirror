package kawa.lang;

/**
 * A pattern that requires an exact match (using equal?).
 */

public class EqualPat extends Pattern {

  Object value;

  public EqualPat (Object obj) { value = obj; }

  public Object[] match (Object obj) {
    if (kawa.standard.equal_p.equal_p (value, obj))
      return new Object [0];
    else
      return null;
  }
}
