package gnu.expr;

import gnu.bytecode.*;
import gnu.mapping.Values;

/** This is the Target of a boolean expression, in a conditional context.
  * If the expression evaluates to true, transfer to the ifTrue label;
  * if false, transfer to the ifFalse label. */

public class ConditionalTarget extends Target
{
  public Label ifTrue, ifFalse;
  Language language;

  /**
    * @param ifTrue label to jump to if this evaluates to true
    * @param ifFalse label to jump to if true
    * @param language specifies what values are true
    */

  public ConditionalTarget (Label ifTrue, Label ifFalse,
			    Language language)
  {
    this.ifTrue = ifTrue;
    this.ifFalse = ifFalse;
    this.language = language;
  }

  /** True if the ifTrue label comes before the ifFalse label.
    * This is used in the hope we can optimize away a branch followed by
    * its target. */
  public boolean trueBranchComesFirst = true;

  public Type getType() { return Type.booleanType; }

    public void compileFromStack(Compilation comp, Type stackType) {
        CodeAttr code = comp.getCode();
        char sig = stackType.getSignature().charAt(0);

        if (language != null) {
            // For primitive types, check if zero is considered true.
            // If so any value of the type can be considered true.
            // FIXME This should probably be done at validate-apply time.
            Object zero;
            switch (sig) {
            case 'I':  case 'J':  case 'B':  case 'S':  case 'F': case 'D':
                zero = Integer.valueOf(0);
                break;
            case 'C':
                zero = Character.valueOf('\0');
                break;
            default:
                zero = null;
            }
            if (zero != null && language.isTrue(zero)) {
                code.emitGoto(ifTrue);
                return;
            }
        }

    switch (sig)
      {
      case 'V':
          code.emitGoto(language == null || language.isTrue(Values.empty)
                        ? ifTrue
                        : ifFalse);
          return;
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
	comp.compileConstant(language == null ? Boolean.FALSE
			     : language.booleanObject(false));
	break;
      }
    if (trueBranchComesFirst)
      code.emitGotoIfEq(ifFalse);
    else
      code.emitGotoIfNE(ifTrue);
    emitGotoFirstBranch(code);  // Usually a no-op.
  }

  /** Goto whichever of IfTrue or ifFalse is specified by trueBranchComesFirst.
   * Normally, the goto should get optimized away as a no-op. */
  public final void emitGotoFirstBranch(CodeAttr code)
  {
    code.emitGoto(trueBranchComesFirst ? ifTrue : ifFalse);
  }
}
