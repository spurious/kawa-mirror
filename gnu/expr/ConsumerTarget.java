// Copyright (c) 2000  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;

/**
 * A Target which is some variable that implements gnu.kawa.util.Consumer.
 */

public class ConsumerTarget extends Target
{
	Declaration consumer;

	public ConsumerTarget(Declaration consumer)
	{
		this.consumer = consumer;
	}

	public void compileFromStack(Compilation comp, Type stackType)
	{
		CodeAttr code = comp.getCode();
		String methodName = null;
		Method method = null;
		if (stackType instanceof PrimType)
		{
			char sig = stackType.getSignature().charAt(0);
			switch (sig)
			{
			case 'B': case 'S': case 'I':
				methodName = "writeInt";  break;
			case 'J':	methodName = "writeLong";  break;
			case 'F':	methodName = "writeFloat";  break;
			case 'D':	methodName = "writeDouble";  break;
			case 'C':	methodName = "writeChar";  break;
			case 'Z':	methodName = "writeBoolean";  break;
			case 'V':     return;
			}
		}
		else
		{
			methodName = "writeObject";
		}
		code.emitLoad(consumer.getVariable());
		code.emitSwap();
		if (method == null && methodName != null)
			method = ClassType.make("gnu.kawa.util.Consumer")
				.getDeclaredMethod(methodName, 1);
		if (method != null)
			code.emitInvokeInterface(method);
	}

	public Type getType() { return Compilation.scmSequenceType; }
}

// This is for people using the Emacs editor:
// Local Variables:
// c-file-style: "java"
// c-file-offsets: ((substatement-open . 0))
// tab-width: 4
// indent-tabs-mode: t
// End:
