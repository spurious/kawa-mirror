package gnu.kawa.reflect;
import gnu.bytecode.ClassType;
import gnu.bytecode.Access;
import gnu.mapping.*;
import gnu.expr.*;

public class StaticFieldLocation extends FieldLocation
{
  public StaticFieldLocation(String cname, String fname)
  {
    super(null, ClassType.make(cname), fname);
  }

  public StaticFieldLocation(ClassType type, String mname)
  {
    super(null, type, mname);
  }

  public boolean isConstant ()
  {
    return true;
  }

  public Object get (Object defaultValue)
  {
    if (value == null)
      value = super.get(defaultValue);
    if (value instanceof kawa.lang.Macro)
      getDeclaration();
    return value;
  }

  public static void define(Environment environ, Symbol sym, Object property,
                            String cname, String fname)
  {
    environ.addLocation(sym, property,
			new StaticFieldLocation(cname, fname));
  }

  public static StaticFieldLocation make (/*Object name,*/ String cname, String fldName)
  { 
    return new StaticFieldLocation(cname, fldName);
  }
}
