package gnu.kawa.reflect;
import gnu.kawa.reflect.ClassMemberLocation;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

public class StaticFieldLocation extends ClassMemberLocation
{
  Declaration decl;
  Object value;

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

  public synchronized Declaration getDeclaration ()
  {
    Declaration d = decl;
    if (d == null)
      {
	String fname = getMemberName();
	ClassType t = getDeclaringClass();
	gnu.bytecode.Field procField = t.getDeclaredField(fname);
	Object val;
	if (value == null)
	  value = super.get(null);
	val = value;
	if (procField != null && procField.getStaticFlag() && val != null)
	  {
	    int fflags = procField.getModifiers();
	    Object dname;
	    if (val instanceof Named)
	      dname = ((Named) val).getSymbol();
	    else
	      dname = fname;
	    d = new Declaration(dname, procField);
	    d.noteValue(new QuoteExp(val));
	    if ((fflags & Access.FINAL) != 0)
	      d.setFlag(Declaration.IS_CONSTANT);
	    if (val instanceof kawa.lang.Syntax)
	      d.setFlag(Declaration.IS_SYNTAX);
	    if (val instanceof kawa.lang.Macro)
	      d.setSyntax();
	    decl = d;
	  }
	else
	  {
	    System.err.println("getDecl loc:"+this+" val:"+get("(unbound)"));
	  }
      }
    return d;
  }

  public static void define(Environment environ, Symbol sym, Object property,
                            String cname, String fname)
  {
    environ.addLocation(sym, property,
			new StaticFieldLocation(cname, fname));
  }
}
