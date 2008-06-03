package gnu.jemacs.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import kawa.lang.*;

/**
 * The Syntax transformer that re-writes the lambda builtin.
 * @author	Per Bothner
 */

public class lambda extends Lambda
{
  /** True if parameters should be bound fluidly. */
  boolean fluidBindings = true;

  public void rewriteBody(LambdaExp lexp, Object body, Translator tr)
  {
    tr.push(lexp);
    if (lexp.defaultArgs != null)
      for (int i = 0, n = lexp.defaultArgs.length;  i < n;  i++)
	lexp.defaultArgs[i] = tr.rewrite(lexp.defaultArgs[i]);

    Pair pair;
    int i = 0;
    if (body instanceof Pair)
      {
        pair = (Pair) body;
	if (
            /* #ifdef use:java.lang.CharSequence */
            pair.getCar() instanceof CharSequence
            /* #else */
            // pair.getCar() instanceof String || pair.getCar() instanceof CharSeq
            /* #endif */
            )
          {
            // Process documentation string.  FIXME.
            body = pair.getCdr();
          }
      }
    Object interactive = null;
    if (body instanceof Pair
	&& (pair = (Pair) body).getCar() instanceof Pair)
      {
	Pair first_application = (Pair) pair.getCar();
	Object first_function = first_application.getCar();
	if (first_function instanceof Symbol
	    && ((Symbol) first_function).getName() == "interactive")
	  {
	    interactive = first_application.getCdr();
	    if (interactive != LList.Empty
		&& ! (interactive instanceof Pair
		      && ((Pair) interactive).getCdr() == LList.Empty))
	      {
		tr.syntaxError ("missing 'interactive' specification");
		interactive = null;
	      }
	    body = pair.getCdr();
	  }
      }
    if (body instanceof PairWithPosition)
      lexp.setFile(((PairWithPosition) body).getFileName());
    FluidLetExp let = null;

    int decl_count = lexp.min_args;
    if (lexp.defaultArgs != null)
      decl_count += lexp.defaultArgs.length;
    if (lexp.max_args < 0)
      decl_count++;

    if (fluidBindings && decl_count > 0)
      {
	Expression[] inits = new Expression[decl_count];
	let = new FluidLetExp (inits);
	i = 0;
	for (Declaration arg = lexp.firstDecl();  arg != null;
	     arg = arg.nextDecl(), i++)
	  {
	    Declaration decl = let.addDeclaration(arg.getSymbol());
            decl.setCanWrite(true);
	    decl.setFluid(true);
	    decl.setIndirectBinding(true);
	    inits[i] = new ReferenceExp(arg);
	    decl.noteValue(inits[i]);
	  }
	tr.push(let);
	let.body = tr.rewrite_body (body);
	tr.pop(let);
	lexp.body = let;
      }
    else
      lexp.body = tr.rewrite_body (body);
    tr.pop(lexp);

    if (interactive != null)
      {
        if (interactive == LList.Empty)
          interactive = QuoteExp.nullExp;
        else
          {
            Object arg = ((Pair) interactive).getCar();
            if (
                /* #ifdef use:java.lang.CharSequence */
                arg instanceof CharSequence
                /* #else */
                // arg instanceof String || arg instanceof CharSeq
                /* #endif */
                )
              interactive = new QuoteExp(arg.toString());
            else
              {
                LambdaExp ilexp = new LambdaExp();
                rewrite(ilexp, LList.Empty, interactive, tr, null);
                ilexp.setCanRead(true);
                interactive = ilexp;
              }
          }
        lexp.setProperty("emacs-interactive", interactive);
      }
  }
}
