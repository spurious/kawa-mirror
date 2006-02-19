package gnu.kawa.functions;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.kawa.reflect.*;
import gnu.expr.*;

/** Procedure to get the value of a named component of an object. */

public class SetNamedPart extends Procedure3 implements HasSetter, CanInline
{
  public static final SetNamedPart setNamedPart = new SetNamedPart();
  static { setNamedPart.setName("setNamedPart"); }

  public Expression inline (ApplyExp exp, ExpWalker walker)
  {
    Expression[] args = exp.getArgs();
    if (args.length != 3 || ! (args[1] instanceof QuoteExp))
      return exp;
    Expression context = args[0];
    String mname = ((QuoteExp) args[1]).getValue().toString();
    Type type = context.getType();
    Compilation comp = walker.getCompilation();
    Language language = comp.getLanguage();
    Type typeval = language.getTypeFor(context);
    ClassType caller = comp == null ? null
      : comp.curClass != null ? comp.curClass
      : comp.mainClass;
    if (typeval instanceof ClassType)
      return new ApplyExp(SlotSet.set$Mnstatic$Mnfield$Ex, args);

    if (type instanceof ClassType)
      {
        Object part = SlotSet.lookupMember((ClassType) type, mname, caller);
        if (part != null)
          {
            // FIXME: future kludge to avoid re-doing SlotGet.getField.
            // args = new Expression[] { context, new QuoteExp(part) });
            return new ApplyExp(SlotSet.set$Mnfield$Ex, args);
          }
      }
    return exp;
  }

  public Object apply3 (Object container, Object part, Object value)
  {
    /*
    if (container implements HasNamedParts)
      return ((HasNamedParts) container).getNamedPart(part);
    */
    if (container instanceof Namespace)
      {
        Namespace ns = (Namespace) container;
        String uri = ns.getName();
        if (uri.startsWith("class:"))
          container = ClassType.make(uri.substring(6));
        else
          {
            Symbol sym = ns.getSymbol(part.toString());
            Environment env = Environment.getCurrent();
            Environment.getCurrent().put(sym, value);
            return Values.empty;
          }
      }
    if (container instanceof Class)
      container = (ClassType) Type.make((Class) container);
    if (container instanceof ClassType)
      {
        try
          {
            gnu.kawa.reflect.SlotSet.setStaticField(container, part.toString(), value);
            return Values.empty;
          }
        catch (Throwable ex)
          {
            // FIXME!
          }
      }

    gnu.kawa.reflect.SlotSet.setField(container, part.toString(), value); 
    return Values.empty;
  }
}
