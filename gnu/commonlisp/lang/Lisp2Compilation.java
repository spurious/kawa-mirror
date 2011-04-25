package gnu.commonlisp.lang;
import kawa.lang.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.text.*;

public class Lisp2Compilation extends Translator
{
  public Lisp2Compilation (Language language, SourceMessages messages, NameLookup lexical)
  {
    super(language, messages, lexical);
  }

  public void emitPushBoolean(boolean value)
  {
    CodeAttr code = getCode();
    if (value)
      code.emitGetStatic(ClassType.make("gnu.commonlisp.lang.Lisp2").getDeclaredField("TRUE"));
    else
      code.emitGetStatic(Compilation.scmListType.getDeclaredField("Empty"));
  }

}
