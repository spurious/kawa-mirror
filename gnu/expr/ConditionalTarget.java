package gnu.expr;

import gnu.bytecode.*;
/** This is the Target of a boolean expression, in a conditional context.
  * If the expression evaluates to, transfer to the ifTrue label;
  * if false, tranfer to the ifFalse label. */

public class ConditionalTarget extends Target
{
  public Label ifTrue, ifFalse;

  /**
    * @param ifTrue label to jump to if this evaluates to true
    * @param ifFalse label to jump to if true
    */

  public ConditionalTarget (Label ifTrue, Label ifFalse)
  {
    this.ifTrue = ifTrue;
    this.ifFalse = ifFalse;
  }

  /** True if the ifTrue label comes before the ifFalse label.
    * This is used in the hope we can optimize away a branch followed by
    * its target. */
  public boolean trueBranchComesFirst = true;

  public Type getType() { return Type.boolean_type; }

  public void compileFromStack(Compilation comp, Type stackType)
  {
    CodeAttr code = comp.getCode();
    char sig = stackType.getSignature().charAt(0);
    switch (sig)
      {
      case 'J':
	code.emitPushLong(0);
	break;
      case 'D':
	code.emitPushDouble(0.0);
	break;
      case 'F':
	code.emitPushFloat(0.0f);
	break;
      default:
	if (trueBranchComesFirst)
	  {
	    code.emitGotoIfIntEqZero(ifFalse);
	    code.emitGoto(ifTrue);
	  }
	else
	  {
	    code.emitGotoIfIntNeZero(ifTrue);
	    code.emitGoto(ifFalse);
	  }
	return;
      case 'L':  case '[':
	comp.compileConstant(Boolean.FALSE);
	break;
      }
    if (trueBranchComesFirst)
      {
	code.emitGotoIfEq(ifFalse);
	code.emitGoto(ifTrue);
      }
    else
      {
	code.emitGotoIfNE(ifTrue);
	code.emitGoto(ifFalse);
      }
  }
}
