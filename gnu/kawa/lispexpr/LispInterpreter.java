// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.*;
import gnu.lists.*;
import kawa.lang.Translator; // FIXME

/** Interpreter sub-class for Lisp-like languages (including Scheme). */

public abstract class LispInterpreter extends Interpreter
{
  public Compilation parse(InPort port, gnu.text.SourceMessages messages,
			   int options)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    kawa.lang.Translator tr = new Translator (this, messages);
    tr.immediate = (options & PARSE_IMMEDIATE) != 0;
    ModuleExp mexp = new ModuleExp();
    if (Compilation.generateAppletDefault)
      mexp.setFlag(ModuleExp.SUPERTYPE_SPECIFIED);
    String fname = port.getName();
    mexp.setFile(fname);
    java.util.Vector forms = new java.util.Vector(20);
    tr.push(mexp);
    LispReader lexer = (LispReader) getLexer(port, messages);
    for (;;)
      {
	Object sexp = lexer.readObject(); // FIXME
	if (sexp == Sequence.eofValue)
	  {
	    if ((options & PARSE_ONE_LINE) != 0)
	      return null;  // FIXME
	    break;
	  }
	if (! tr.scan_form (sexp, forms, mexp)
	    || (options & PARSE_ONE_LINE) != 0)
	  break;
      }
    if (port.peek() == ')')
      lexer.fatal("An unexpected close paren was read.");
    tr.finishModule(mexp, forms);
    return tr;
  }

  /** Combine a <body> consisting of a list of expression. */
  public Expression makeBody(Expression[] exps)
  {
    return new BeginExp (exps);
  }

}
