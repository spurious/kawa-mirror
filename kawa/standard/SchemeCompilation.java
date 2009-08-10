package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.kawa.lispexpr.*;

public class SchemeCompilation
{
  public static final kawa.repl repl;

  public static final Lambda lambda = new kawa.lang.Lambda();

  static
  {
    repl = new kawa.repl(Scheme.instance);
    lambda.setKeywords(Special.optional, Special.rest, Special.key); 
  }
}
