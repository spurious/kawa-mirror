package gnu.expr;
import gnu.bytecode.*;

/** Implements a function call in the LHS of an assignment.
  * In Scheme terms, represents:  ((setter proc) rhs . args)
  * which is what results from a: (set! (proc . args) rhs
  */

public class SetApplyExp extends ApplyExp
{
  public static QuoteExp setterProcedure
  = new QuoteExp(kawa.standard.setter.setterProcedure);

  public SetApplyExp(Expression proc, Expression[] args)
  {
    super(proc, args);

    Expression[] setter_args = new Expression[1];
    setter_args[0] = proc;
    func = new ApplyExp(setterProcedure, setter_args);
  }

  public static void compile (ApplyExp exp, Compilation comp, Target target)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    ((ApplyExp) exp.func).args[0].compile(comp, Target.pushObject);
    comp.method.compile_checkcast (comp.typeProcedure);
    // Number of arguments, not counting the rhs value.
    int args_length = exp.args.length - 1;

    // One complication is that re-writing the Scheme expression
    // (set! (proc . args) rhs) to ((sett! proc) rhs . args)
    // causes rhs to be evaluated before the other args.
    // Note that Scheme does not specify the order of evaluation, but
    // some other languages (including Java and Common Lisp) do.
    // In any case, it is preferable to maintain the original order.
    // That explain some of the convolutions below.
    if (args_length <= 1)
      {
	for (int i = 1; i <= args_length; ++i)
	  exp.args[i].compile (comp, Target.pushObject);
	exp.args[0].compile (comp, Target.pushObject);
	if (args_length == 2)
	  code.emitSwap();
      }
    else
      {
	code.emitPushInt(args_length+1);
	code.emitNewArray(comp.scmObjectType);
	for (int i = 0; i <= args_length; ++i)
	  {
	    code.emitDup(comp.objArrayType);
	    int index = i == args_length ? 0 : i + 1;
	    code.emitPushInt(index);
	    exp.args[index].compile (comp, Target.pushObject);
	    code.emitArrayStore(comp.scmObjectType);
	  }
      }
    code.emitInvokeVirtual(getSetMethod(args_length));
    comp.compileConstant(gnu.mapping.Values.empty, target);
  }

  Object walk (ExpWalker walker) { return walker.walkSetApplyExp(this); }

  static synchronized Method getSetMethod (int arg_count)
  {
    ClassType proc = Compilation.typeProcedure;
    if (arg_count == 0)
      {
	if (set0Method == null)
	  set0Method
	    = proc.addMethod ("set0", Compilation.apply1args, Type.void_type,
			      Access.PUBLIC);
	return set0Method;
      }
    else if (arg_count == 1)
      {
	if (set1Method == null)
	  set1Method
	    = proc.addMethod ("set1", Compilation.apply2args, Type.void_type,
			      Access.PUBLIC);
	return set1Method;
      }
    else
      {
	if (setNMethod == null)
	  setNMethod
	    = proc.addMethod ("setN", Compilation.applyNargs, Type.void_type,
			      Access.PUBLIC);
	return setNMethod;
      }
  }

  public static Method set0Method, set1Method, setNMethod;
}
