// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.xml.*;
import gnu.math.IntNum;
import gnu.kawa.functions.*;
import gnu.kawa.reflect.OccurrenceType;

/** Implements XPath path expression.
 * The XPath expression E1/E2 is compiled into:
 * (relative-step E1 (lambda (dot position last) E2)).
 */

public class RelativeStep extends MethodProc implements CanInline, Inlineable
{
  public static final RelativeStep relativeStep = new RelativeStep();

  public int numArgs() { return 0x2002; }

  public void apply (CallContext ctx) throws Throwable
  {
    Object arg = ctx.getNextArg();
    Object next = ctx.getNextArg();
    Procedure proc = (Procedure) next;
    Consumer out = ctx.consumer;
    IntNum countObj;
    Nodes values;
    if (arg instanceof Nodes)
      values = (Nodes) arg;
    else
      {
	values = new Nodes();
	Values.writeValues(arg, values);
      }
    int count = values.size();
    int it = 0;
    countObj = IntNum.make(count);
    RelativeStepFilter filter = new RelativeStepFilter(out);
    for (int pos = 1; pos <= count; pos++)
      {
	it = values.nextPos(it);
	Object dot = values.getPosPrevious(it);
	proc.check3(dot, IntNum.make(pos), countObj, ctx);
        Values.writeValues(ctx.runUntilValue(), filter);
      }
    filter.finish();
  }

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression[] args = exp.getArgs();
    Expression exp1 = args[0];
    Expression exp2 = args[1];
    LambdaExp lexp2;
    Compilation comp = walker.getCompilation();
    if (! (exp2 instanceof LambdaExp)
        // The following optimization breaks when interpreting, because
        // then CoerceToNodes may not work.
        || ! comp.mustCompile
	|| (lexp2 = (LambdaExp) exp2).min_args != 3
	|| lexp2.max_args != 3)
      return exp;

    lexp2.setInlineOnly(true);
    lexp2.returnContinuation = exp;

    exp2 = lexp2.body;

    Declaration dotArg = lexp2.firstDecl();
    Declaration posArg = dotArg.nextDecl();
    Declaration lastArg = posArg.nextDecl();
    // Splice out the "last" argument - we'll move it out.
    // The remaining two arguments are suitable for a ValuesMap.
    posArg.setNext(lastArg.nextDecl());
    lastArg.setNext(null);
    lexp2.min_args = 2;
    lexp2.max_args = 2;

    Type type1 = exp1.getType();
    Type rtype = exp.getTypeRaw();
    Type rtypePrime;
    int nodeCompare;
    if (rtype == null || rtype == Type.pointer_type)
      {
        Type type2 = exp2.getType();
        rtypePrime = OccurrenceType.itemPrimeType(type2);
        nodeCompare = NodeType.anyNodeTest.compare(rtypePrime);
        if (nodeCompare >= 0)
          rtype = NodeSetType.getInstance(rtypePrime);
        else
          rtype = OccurrenceType.getInstance(rtypePrime, 0, -1);
        exp.setType(rtype);
      }
    if (lastArg.getCanRead())
      {
        ClassType typeNodes = CoerceNodes.typeNodes;
        comp.letStart();
        Declaration sequence
          = comp.letVariable(null, typeNodes,
                             new ApplyExp(CoerceNodes.coerceNodes,
                                          new Expression [] { exp1 }));
        comp.letEnter();

        Method sizeMethod = typeNodes.getDeclaredMethod("size", 0);
        Expression lastInit
          =  new ApplyExp(sizeMethod,
                          new Expression[] {new ReferenceExp(sequence)});
        LetExp lastLet = new LetExp(new Expression[] { lastInit });
        lastLet.addDeclaration(lastArg);
        lastLet.body = new ApplyExp(exp.getFunction(),
                                    new Expression[] { new ReferenceExp(sequence),
                                                       lexp2 });
        return comp.letDone(lastLet);
      }

    ApplyExp result = exp;

    // Try to rewrite A/B[P] to (A/B)[P].
    // This only works if P doesn't depend in position() or last().
    if (exp2 instanceof ApplyExp)
      {
        ApplyExp aexp2 = (ApplyExp) exp2;
        Object proc2 = aexp2.getFunction().valueIfConstant();
        Expression vexp2;
        if (proc2 instanceof ValuesFilter
            && (vexp2 = aexp2.getArgs()[1]) instanceof LambdaExp)
          {
            LambdaExp lvexp2 = (LambdaExp) vexp2;
            Declaration dot2 = lvexp2.firstDecl();
            Declaration pos2;
            if (dot2 != null && (pos2 = dot2.nextDecl()) != null
                && pos2.nextDecl() == null
                && ! pos2.getCanRead()
                // If the predicate can evaluate to a number, then the
                // optimization is unsafe, since we implicitly
                // compare against position().
                && ClassType.make("java.lang.Number").compare(lvexp2.body.getType()) == -3)
              {
                exp2 = aexp2.getArg(0);
                lexp2.body = exp2;
                aexp2.setArg(0, exp);
                result = aexp2;
              }
          }
      }
    // Now we can rewrite 'descendant-or-self::node()/B' (which is the
    // expansion of the abbreviated syntax '//B') to /descdendant::B'.
    if (exp1 instanceof ApplyExp && exp2 instanceof ApplyExp)
      {
        ApplyExp aexp1 = (ApplyExp) exp1;
        ApplyExp aexp2 = (ApplyExp) exp2;
        Object p1 = aexp1.getFunction().valueIfConstant();
        Object p2 = aexp2.getFunction().valueIfConstant();
        Expression exp12;
        if (p1 == relativeStep && p2 instanceof ChildAxis
            && aexp1.getArgCount() == 2
            && (exp12 = aexp1.getArg(1)) instanceof LambdaExp)
          {
            LambdaExp lexp12 = (LambdaExp) exp12;
            ApplyExp aexp12;
            if (lexp12.body instanceof ApplyExp
                && (aexp12 = (ApplyExp) lexp12.body).getFunction().valueIfConstant() == DescendantOrSelfAxis.anyNode)
              {
                exp.setArg(0, aexp1.getArg(0));
                aexp2.setFunction(new QuoteExp(DescendantAxis.make(((ChildAxis) p2).getNodePredicate())));
              }
          }
      }
    
    return result;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    Expression exp1 = args[0];
    Expression exp2 = args[1];
    if (target instanceof IgnoreTarget)
      {
        exp1.compile(comp, target);
        exp2.compile(comp, target);
        return;
      }

    Type rtype = exp.getTypeRaw();
    if (rtype == null) // should never happen
      rtype = Type.pointer_type;
    Type rtypePrime = OccurrenceType.itemPrimeType(rtype);
    int nodeCompare = NodeType.anyNodeTest.compare(rtypePrime);
    // 'A' - atomic; 'N' - nodes; 'S' - pre-sorted nodes; ' ' - unknown.
    char expectedKind;
    if (nodeCompare >= 0)
      expectedKind = 'N';
    else if (nodeCompare == -3)
      expectedKind = 'A';
    else
      expectedKind = ' ';
    TreeScanner step = extractStep(exp2);
    if (step != null)
      {
        Type type1 = exp1.getType();
        if (step instanceof ChildAxis
            || step instanceof AttributeAxis
            || step instanceof SelfAxis)
          {
            if (type1 instanceof NodeSetType
                || (expectedKind == 'N'
                    && OccurrenceType.itemCountIsZeroOrOne(exp1.getType())))
              expectedKind = 'S';
            /*
            // It's presumably more efficient to sort the argument
            // nodes rather than the result nodes.  FIXME
            else
              {
                exp1 = SortNodes(exp1);
                expectedKind = 'S';
              }
            */
          }
      }

    if (! (target instanceof ConsumerTarget
           || (target instanceof SeriesTarget
               && (expectedKind == 'A' || expectedKind == 'S'))))
      {
	ConsumerTarget.compileUsingConsumer(exp, comp, target);
	return;
      }

    CodeAttr code = comp.getCode();
    Target mtarget;
    Scope scope = code.pushScope();
    Variable mconsumer;
    Variable tconsumer;
    ClassType mclass;

    if (expectedKind == 'A' || expectedKind == 'S')
      {
        mtarget = target;
        mclass = null;
        mconsumer = null;
        tconsumer = null;
      }
    else
      {
        // We need a helper consumer.
        Method initMethod;
        if (expectedKind == 'N')
          {
            mclass = ClassType.make("gnu.kawa.xml.SortedNodes");
            initMethod = mclass.getDeclaredMethod("<init>", 0);
          }
        else
          {
            mclass = ClassType.make("gnu.xquery.util.RelativeStepFilter");
            initMethod = mclass.getDeclaredMethod("<init>", 1);
          }
        mconsumer = scope.addVariable(code, mclass, null);
        mtarget = new ConsumerTarget(mconsumer);
        code.emitNew(mclass);
        code.emitDup(mclass);
        tconsumer = ((ConsumerTarget) target).getConsumerVariable();
        if (expectedKind != 'N')
          code.emitLoad(tconsumer);
        code.emitInvoke(initMethod);
        code.emitStore(mconsumer);     
      }

    ValuesMap.compileInlined((LambdaExp) exp2, exp1, 1, null, comp, mtarget);

    // Now finish up from the helper consumer.
    if (expectedKind == 'N')
      {
        code.emitLoad(mconsumer);
        code.emitLoad(tconsumer);
        code.emitInvokeStatic(Compilation.typeValues
                              .getDeclaredMethod("writeValues", 2));
      }
    else if (expectedKind == ' ')
      {
        code.emitLoad(mconsumer);
        code.emitInvoke(mclass.getDeclaredMethod("finish", 0));
      }

    code.popScope();
  }

  public Type getReturnType (Expression[] args)
  {
    // Needlessly convervative, but it shouldn't matter, since this
    // shouldn't be called if the ApplyExp.setType has been done.
    return Type.pointer_type;
  }

  public static TreeScanner extractStep (Expression exp)
  {
    for (;;)
      {
        if (! (exp instanceof ApplyExp))
          return null;
        ApplyExp aexp = (ApplyExp) exp;
        Expression func = aexp.getFunction();
        if (func instanceof QuoteExp)
          {
            Object value = ((QuoteExp) func).getValue();
            if (value instanceof TreeScanner)
              return (TreeScanner) value;
            // This doesn't work, if we've already inlined ValuesFilter. FIXME
            if (value instanceof ValuesFilter)
              {
                exp = aexp.getArgs()[0];
                continue;
              }
          }
        return null;
      }
  }
}
