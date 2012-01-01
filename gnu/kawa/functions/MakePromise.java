package gnu.kawa.functions;
import gnu.mapping.*;

public class MakePromise extends Procedure1
{
    public static final MakePromise makePromise = new MakePromise();
    static {
        makePromise.setName("make-promise");
        makePromise.setProperty(Procedure.validateApplyKey,
                                "gnu.kawa.functions.CompileMisc:validateApplyMakePromise");
    }

    public static <T> Promise<T> makePromise(Procedure thunk) {
        return new Promise<T>((Procedure) thunk);
    }

    public Object apply1 (Object thunk) {
      return makePromise((Procedure) thunk);
    }
}
