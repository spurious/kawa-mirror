package gnu.kawa.functions;

import gnu.mapping.Procedure;
import gnu.mapping.Procedure1;

/** Implement the 'dynamic' constructor function.
 * The coerces the argument to the 'dynamic' type.
 */

public class MakeDynamic extends Procedure1 {
    public static final MakeDynamic instance = new MakeDynamic();
    static { instance.setName("dynamic");
        instance.setProperty(Procedure.validateXApplyKey,
                             "gnu.kawa.functions.CompileMisc:validateApplyMakeDynamic");
        instance.setProperty(Procedure.compilerXKey,
                             "gnu.kawa.functions.CompileMisc:compileMakeDynamic");
    }

    public Object apply1 (Object value) { return value; }
}
