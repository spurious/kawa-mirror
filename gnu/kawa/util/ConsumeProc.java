package gnu.kawa.util;
import gnu.lists.*;

public abstract class ConsumeProc extends gnu.mapping.ProcedureN
{
	public Object applyN (Object[] args)
	{
		TreeList collector = new TreeList();
		applyN(collector, args);
		return collector;
	}

	public abstract void applyN (Consumer consumer, Object[] args);
}

// This is for people using the Emacs editor:
// Local Variables:
// c-file-style: "java"
// c-file-offsets: ((substatement-open . 0))
// tab-width: 4
// indent-tabs-mode: t
// End:
