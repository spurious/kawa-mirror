package gnu.kawa.functions;
import gnu.bytecode.Type;
import gnu.mapping.*;

public class Convert extends Procedure2 {
    public static final Convert as = new Convert();
    static {
        as.setName("as"); 
        as.setProperty(Procedure.validateApplyKey,
                       "gnu.kawa.functions.CompileMisc:validateApplyConvert");
        as.setProperty(Procedure.compilerXKey,
                       "gnu.kawa.functions.CompileMisc:compileConvert");
    }

    public static Convert getInstance() {
        return as;
    }

    public Object apply2(Object arg1, Object arg2) {
        Type type;
        if (arg1 instanceof Class)
            type = Type.make((Class) arg1);
        else
            type = (Type) arg1;
        return type.coerceFromObject (arg2);
    }
}
