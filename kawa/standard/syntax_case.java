package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.mapping.Procedure;
import gnu.expr.*;
import gnu.math.IntNum;
import gnu.bytecode.*;
import gnu.kawa.reflect.*;

public class syntax_case extends Syntax
{
  PrimProcedure call_error;

  Expression rewriteClauses (Object clauses, syntax_case_work work,
                             Translator tr)
  {
    if (clauses == LList.Empty)
      {
        /*
        // FIXME - throw exception instead??  perhaps SyntaxException?
        return new QuoteExp(new Pair("quote",
                                     new Pair("((no match in syntax-case))",
                                     LList.Empty)));
        */
        Expression[] args = new Expression[2];
        args[0] = new QuoteExp("syntax-case");
        args[1] = new ReferenceExp(work.inputExpression);
        if (call_error == null)
          {
            ClassType clas = ClassType.make("kawa.standard.syntax_case");
            Type[] argtypes = new Type[2];
            argtypes[0] = Compilation.javaStringType;
            argtypes[1] = Type.pointer_type;
            Method method = clas.addMethod("error", argtypes,
                                           Type.pointer_type,
                                           Access.PUBLIC|Access.STATIC);
            call_error = new PrimProcedure(method);
          }
        return new ApplyExp(call_error, args);
      }
    Object savePos = tr.pushPositionOf(clauses);
    Object clause;
    try
      {
	if (! (clauses instanceof Pair)
	    || ! ((clause = ((Pair) clauses).car) instanceof Pair))
	  return tr.syntaxError("syntax-case:  bad clause list");
	Pair pair = (Pair) clause;
	PatternScope clauseScope = PatternScope.push(tr);
	clauseScope.matchArray = work.matchArray;
	tr.push(clauseScope);

	SyntaxPattern pattern
	  = new SyntaxPattern(pair.car, work.literal_identifiers, tr);
	int varCount = pattern.varCount();
	if (varCount > work.maxVars)
	  work.maxVars = varCount;

	BlockExp block = new BlockExp();
	Expression[] args = new Expression[4];
	args[0] = new QuoteExp(pattern);
	args[1] = new ReferenceExp(work.inputExpression);
	args[2] = new ReferenceExp(work.matchArray);
	args[3] = new QuoteExp(IntNum.zero());
	Expression tryMatch
	  = new ApplyExp(new PrimProcedure(Pattern.matchPatternMethod), args);

	Expression[] inits = new Expression[varCount];
	for (int i = 0;  i < varCount;  i++)
	  inits[i] = QuoteExp.undefined_exp;
	clauseScope.inits = inits;

	Expression output;
	pair = (Pair) pair.cdr;
	if (pair.cdr == LList.Empty)
	  output = tr.rewrite(pair.car);
	else
	  {
	    Expression fender = tr.rewrite(pair.car);
	    if (! (pair.cdr instanceof Pair
		   && (pair = (Pair) pair.cdr).cdr == LList.Empty))
	      return tr.syntaxError("syntax-case:  bad clause");
	    output = new IfExp(fender, tr.rewrite(pair.car), new ExitExp(block));
	  }
	clauseScope.setBody(output);
	tr.pop(clauseScope);
	PatternScope.pop(tr);
	block.setBody(new IfExp(tryMatch, clauseScope, new ExitExp(block)),
		      rewriteClauses(((Pair) clauses).cdr, work, tr));
	return block;
      }
    finally
      {
	tr.popPositionOf(savePos);
      }
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    syntax_case_work work = new syntax_case_work();

    Object obj = form.cdr;
    if (obj instanceof Pair)
      {
        form = (Pair) obj;
        Expression input = tr.rewrite(form.car);
        obj = form.cdr;
        if (obj instanceof Pair)
          {
            form = (Pair) obj;
            obj = form.car;
            int num_literals = LList.length(obj);
            work.literal_identifiers = new Object [num_literals + 1];
            work.literal_identifiers[0] = null; // FIXME
            for (int i = 0;  i < num_literals;  i++)
              {
                Pair lit_pair = (Pair) obj;
		Object savePos = tr.pushPositionOf(obj);
		try
		  {
		    if (! (lit_pair.car instanceof String))
		      return tr.syntaxError ("syntax-case: non-symbol in literals list");
		  }
		finally
		  {
		    tr.popPositionOf(savePos);
		  }
                work.literal_identifiers[i+1] = lit_pair.car;
                obj = lit_pair.cdr;
              }
            obj = form.cdr;

            Expression[] linits = new Expression[2];
            linits[0] = input;
            LetExp let = new LetExp(linits);
            work.inputExpression = let.addDeclaration((String) null);
            work.inputExpression.noteValue(linits[0]);
            work.matchArray = let.addDeclaration((String) null);
            work.primArrayGet = new ArrayGet(Type.pointer_type);
            work.matchArray.setType(Compilation.objArrayType);
            work.inputExpression.setCanRead(true);
            work.matchArray.setCanRead(true);
            tr.push(let);
            let.body = rewriteClauses(obj, work, tr);
            tr.pop(let);
            Procedure arrayNew = new ArrayNew(Type.pointer_type);
            Expression[] args = new Expression[1];
            args[0] = new QuoteExp(IntNum.make(work.maxVars));
            linits[1] = new ApplyExp(arrayNew, args);
            work.matchArray.noteValue(linits[1]);
            return let;
          }
      }
    return tr.syntaxError("insufficiant arguments to syntax-case");
  }

  /** Called (at run-time) if syntax-case has no match. */
  public static Object error(String kind, Object arg)
  {
    Translator tr = (Translator) Compilation.getCurrent();
    if (tr == null)
      throw new RuntimeException("no match in syntax-case");
    Syntax syntax = tr.getCurrentSyntax();
    String name = syntax == null ? "some syntax" : syntax.getName();
    String msg = "no matching case while expanding " + name;
    return tr.syntaxError(msg);
  }
}

class syntax_case_work
{
  LetExp let;
  Object[] literal_identifiers;

  /** A temporary to hold the value of the input expression. */
  Declaration inputExpression;

  /** A variable to hold the matched values for the pattern variables. */
  Declaration matchArray;

  /** The maximum of the varCount() for the patterns seen so far. */
  int maxVars;

  ArrayGet primArrayGet;
}
