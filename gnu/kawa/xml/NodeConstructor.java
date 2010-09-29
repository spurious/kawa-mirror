// Copyright (c) 2001, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.xml.*;
import gnu.lists.*;

public abstract class NodeConstructor extends MethodProc
implements Inlineable
{
  public abstract void compileToNode (ApplyExp exp, Compilation comp,
				      ConsumerTarget target);

  public static XMLFilter pushNodeConsumer (Consumer out)
  {
    if (out instanceof XMLFilter)
      return (XMLFilter) out;
    else
      return new XMLFilter(new NodeTree());
  }

  public static void popNodeConsumer (Consumer saved, Consumer current)
  {
    if (saved != current)
      saved.writeObject(current instanceof XMLFilter
                        ? (Object) KNode.make((NodeTree) ((XMLFilter) current).out)
                        : (Object) current);
  }

  public static XMLFilter pushNodeContext (CallContext ctx)
  {
    Consumer out = ctx.consumer;
    if (out instanceof XMLFilter)
      return (XMLFilter) out;
    else
      {
        // FIXME: It would be more efficinet to just do:
        // filter = new XMLFilter(out);
        // There is at least one problem "JOINER" isn't handled properly;
        // it's not obvious how to ensure we get the right whitespace.
	XMLFilter filter = new XMLFilter(new NodeTree());
	ctx.consumer = filter;
	return filter;
      }
  }

  public static void popNodeContext (Consumer saved, CallContext ctx)
  {
    Object current = ctx.consumer;
    if (saved != current)
      {
	if (current instanceof XMLFilter)
	  current = KNode.make((NodeTree) ((XMLFilter) current).out);
	saved.writeObject(current);
	ctx.consumer = saved;
      }
  }

  public static void compileChild (Expression arg,
				   Compilation comp, ConsumerTarget target)
  {
    if (arg instanceof ApplyExp)
      {
	ApplyExp app = (ApplyExp) arg;
	Expression func = app.getFunction();
	if (func instanceof QuoteExp)
	  {
	    Object proc = ((QuoteExp) func).getValue();
	    if (proc instanceof NodeConstructor)
	      {
		((NodeConstructor) proc).compileToNode(app, comp, target);
		return;
	      }
	  }
      }
    arg.compileWithPosition(comp, target);
  }

  /** Compile an expression using a fresh NodeTree.
   * Compare with ConsumerTarget.compileUsingConsumer, but creates a NodeTree.
   */
  public static void compileUsingNodeTree(Expression exp,
					  Compilation comp, Target target)
  {
    Method makeMethod = typeNodeConstructor.getDeclaredMethod("makeNode", 0);
    Method makeKNodeMethod = typeNodeConstructor.getDeclaredMethod("finishNode", 1);
    ConsumerTarget.compileUsingConsumer(exp, comp, target,
					makeMethod, makeKNodeMethod);
  }

  public static XMLFilter makeNode ()
  {
    return new XMLFilter(new NodeTree());
  }

  public static KNode finishNode (XMLFilter filter)
  {
    return KNode.make((NodeTree) filter.out);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      ApplyExp.compile(exp, comp, target);
    else if (! (target instanceof ConsumerTarget))
      compileUsingNodeTree(exp, comp, target);
    else
      {
	ConsumerTarget ctarget = (ConsumerTarget) target;
	Variable cvar = ctarget.getConsumerVariable();
	Type ctype = cvar.getType();
	if (ctype.isSubtype(typeXMLFilter))
	  compileToNode(exp, comp, ctarget);
	else
	  {
	    Expression[] args = exp.getArgs();
	    int nargs = args.length;
	    CodeAttr code = comp.getCode();
	    Scope scope = code.pushScope();
            Variable xvar
	      = scope.addVariable(code, typeXMLFilter, null);
	    if (ctarget.isContextTarget())
	      {
		comp.loadCallContext();
		code.emitInvokeStatic(pushNodeContextMethod);
	      }
	    else
	      {
		code.emitLoad(cvar);
		code.emitInvokeStatic(pushNodeConsumerMethod);
	      }
	    code.emitStore(xvar);
	    code.emitTryStart(true, Type.void_type);
            ConsumerTarget xtarget = new ConsumerTarget(xvar);
	    compileToNode(exp, comp, xtarget);
	    code.emitTryEnd();
	    code.emitFinallyStart();
	    code.emitLoad(cvar);
	    if (ctarget.isContextTarget())
	      {
		comp.loadCallContext();
		code.emitInvokeStatic(popNodeContextMethod);
	      }
	    else
	      {
		code.emitLoad(xvar);
		code.emitInvokeStatic(popNodeConsumerMethod);
	      }
	    code.emitFinallyEnd();
	    code.emitTryCatchEnd();
	    code.popScope();
	  }
      }
  }

  public Type getReturnType (Expression[] args)
  {
    return Compilation.typeObject;
  }

  static final ClassType typeXMLFilter
    = ClassType.make("gnu.xml.XMLFilter");
  static final ClassType typeKNode
    = ClassType.make("gnu.kawa.xml.KNode");
  static final ClassType typeNodeConstructor
    = ClassType.make("gnu.kawa.xml.NodeConstructor");
  static final Method pushNodeContextMethod
    = typeNodeConstructor.getDeclaredMethod("pushNodeContext", 1);
  static final Method popNodeContextMethod
    = typeNodeConstructor.getDeclaredMethod("popNodeContext", 2);
  static final Method pushNodeConsumerMethod
    = typeNodeConstructor.getDeclaredMethod("pushNodeConsumer", 1);
  static final Method popNodeConsumerMethod
    = typeNodeConstructor.getDeclaredMethod("popNodeConsumer", 2);
}
