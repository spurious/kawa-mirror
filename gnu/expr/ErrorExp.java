package gnu.expr;
import gnu.kawa.io.OutPort;
import gnu.text.SourceMessages;

/**
 * Class used to mark an erroneous expression
 * @author	Per Bothner
 */

public class ErrorExp extends Expression
{
    String message;

    public ErrorExp(String message) {
        this.message = message;
    }

    public ErrorExp(String message, SourceMessages messages) {
        this(message);
        messages.error('e', message);
    }

    public ErrorExp(String message, Compilation comp) {
        this(message, comp.getMessages());
    }

    protected boolean mustCompile() { return false; }

    public String toString() { return "ErrorExp["+message+"]"; }

    public void print(OutPort out) {
        out.startLogicalBlock("(Error", false, ")");
        out.writeSpaceLinear();
        out.print(message);
        out.endLogicalBlock(")");
    }

    public void compile(Compilation comp, Target target) {
        // Should never happen!
        throw new Error(comp.getFileName()+":"+comp.getLineNumber()
                        +": internal error: compiling error expression: "+message);
    }
}
