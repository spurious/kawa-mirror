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
    Pair pair;
    int i = 0;
    if (body instanceof Pair
	&& (pair = (Pair) body).car instanceof FString)
      {
	// Process documentation string.  FIXME.
	body = pair.cdr;
      }
    Object interactive = null;
    if (body instanceof Pair
	&& (pair = (Pair) body).car instanceof Pair
	&& ((Pair) pair.car).car == "interactive")
      {
	interactive = ((Pair) pair.car).cdr;
	if (interactive != LList.Empty
            && ! (interactive instanceof Pair
                  && ((Pair) interactive).cdr == LList.Empty))
          {
            tr.syntaxError ("missing 'interactive' specification");
            interactive = null;
          }
	body = pair.cdr;
      }
    if (body instanceof PairWithPosition)
      lexp.setFile(((PairWithPosition) body).getFile());
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
	    Declaration decl = let.addDeclaration(arg.getName());
	    decl.setFluid(true);
	    decl.setType(gnu.expr.FluidLetExp.typeFluidBinding);
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
            Object arg = ((Pair) interactive).car;
            if (arg instanceof FString)
              interactive = new QuoteExp(arg.toString());
            else
              {
                LambdaExp ilexp = new LambdaExp();
                rewrite(ilexp, LList.Empty, interactive, tr);
                ilexp.setCanRead(true);
                interactive = ilexp;
              }
          }
        lexp.setProperty("emacs-interactive", interactive);
      }
  }
}
