package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.mapping.Location;  // As opposed to gnu.bytecode.Location.
import gnu.expr.*;

public class FieldLocation extends ClassMemberLocation
{
  Declaration decl;
  /** The cached location of the field, if final.
   * This is the value of this Location, exception if isIndirectLocation(),
   * we need to do an extra indiorection. */
  Object value;
  static final int SETUP_DONE = 1;
  static final int INDIRECT_LOCATION = 2;
  static final int CONSTANT = 4;
  static final int VALUE_SET = 8;
  // The PROCEDURE and SYNTAX flags aren't current used by getDeclaration,
  // but probably should be, assuming we can count on them.
  public static final int PROCEDURE = 16;
  public static final int SYNTAX = 32;
  private int flags;

  protected boolean isIndirectLocation ()
  { return (flags & INDIRECT_LOCATION) != 0; }

  public void setProcedure ()
  {
    flags |= PROCEDURE;
  }

  public void setSyntax ()
  {
    flags |= SYNTAX;
  }

  /** Not reliable, yet. */
  public boolean isProcedureOrSyntax ()
  {
    return (flags & (PROCEDURE+SYNTAX)) != 0;
  }

  public FieldLocation(Object instance, String cname, String fname)
  {
    super(instance, ClassType.make(cname), fname);
  }

  public FieldLocation(Object instance, ClassType type, String mname)
  {
    super(instance, type, mname);
  }


  public void setDeclaration (Declaration decl)
  {
    this.decl = decl;
  }

  public synchronized Declaration getDeclaration ()
  {
    Declaration d = decl;
    if (d == null)
      {
	String fname = getMemberName();
	ClassType t = getDeclaringClass();
	gnu.bytecode.Field procField = t.getDeclaredField(fname);
	if (procField == null)
	  return null;
	Object val;
	if (value == null)
	  value = get(null);
	val = value;
	if (val == null)
	  return null;
	int fflags = procField.getModifiers();
	Object dname;
	if (val instanceof Named)
	  dname = ((Named) val).getSymbol();
	else
	  dname = fname;
	d = new Declaration(dname, procField);
	d.field = procField;
	d.noteValue(new QuoteExp(val));
	if ((fflags & Access.FINAL) != 0)
	  d.setFlag(Declaration.IS_CONSTANT);
	if (val instanceof kawa.lang.Syntax)
	  d.setFlag(Declaration.IS_SYNTAX);
	if (val instanceof kawa.lang.Macro)
	  d.setSyntax();
	if (procField.getType().isSubtype(Compilation.typeLocation))
	  d.setIndirectBinding(true);
	decl = d;
      }
    return d;
  }

  void setup ()
  {
    synchronized (this)
      {
	if ((flags & SETUP_DONE) != 0)
	  return;
	super.setup();
	String fname = getMemberName();
	ClassType t = getDeclaringClass();
	gnu.bytecode.Field fld = t.getDeclaredField(fname);
	int fflags = fld.getModifiers();
	int fl = SETUP_DONE;
	if ((fflags & Access.FINAL) != 0)
	  {
	    Type ftype = fld.getType();
	    if (ftype.isSubtype(Compilation.typeLocation))
	      fl |= INDIRECT_LOCATION;
	    else
	      fl |= CONSTANT;
	  }
	flags |= fl;
      }
  }

  public Object get (Object defaultValue)
  {
    setup();
    Object v;
    if ((flags & VALUE_SET) != 0)
      v = value;
    else
      {
	v = getFieldValue();
	if ((type.getDeclaredField(mname).getModifiers() & Access.FINAL) != 0)
	  {
	    flags |= VALUE_SET;
	    if ((flags & INDIRECT_LOCATION) == 0)
	      {
		flags |= CONSTANT;
	      }
	    value = v;
	  }
      }
    if ((flags & (INDIRECT_LOCATION|CONSTANT)) == INDIRECT_LOCATION)
      {
	Object unb = Location.UNBOUND;
	Location loc = (Location) v;
	v = loc.get(unb);
	if (v == unb)
	  return defaultValue;
	if (loc.isConstant())
	  {
	    flags |= CONSTANT;
	    value = v;
	  }
      }
    return v;
  }

  private Object getFieldValue ()
  {
    try
      {
	return rfield.get(instance);
      }
    catch (Throwable ex)
      {
	throw WrappedException.wrapIfNeeded(ex);
      }
  }

  public void set (Object newValue)
  {
    setup();
    if ((flags & INDIRECT_LOCATION) == 0)
      {
	try
	  {
	    rfield.set(instance, newValue);
	  }
	catch (Throwable ex)
	  {
	    throw WrappedException.wrapIfNeeded(ex);
	  }
      }
    else
      {
	Object v;
	if ((flags & VALUE_SET) != 0)
	  v = value;
	else
	  {
	    flags |= VALUE_SET;
	    v = getFieldValue();
	    value = v;
	  }
	if (++xxx > 2)
	  throw new Error("cycle "+this+" type:"+type+" fld:"+rfield);
	((Location) v).set(newValue);
      }
  }

  int xxx = 0;

  public boolean isConstant ()
  {
    setup();
    if ((flags & CONSTANT) != 0)
      return true;
    if (isIndirectLocation())
      {
	Object v;
	if ((flags & VALUE_SET) != 0)
	  v = value;
	else
	  {
	    v = getFieldValue();
	    flags |= VALUE_SET;
	    value = v;
	  }
	return ((Location) v).isConstant();
      }
    return false;
  }

  public boolean isBound ()
  {
    try
      {
	setup();
      }
    catch (Throwable ex)
      {
	return false;
      }
    if (! isIndirectLocation())
      return true;
    Object v;
    if ((flags & VALUE_SET) != 0)
      v = value;
    else
      {
	v = getFieldValue();
	flags |= VALUE_SET;
	value = v;
      }
    return ((Location) v).isBound();
  }

  public static FieldLocation make (/*Object name,*/ Object instance, String cname, String fldName)
  {
    return new FieldLocation(/*name,*/ instance, ClassType.make(cname), fldName);
  }

  public String toString()
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append("FieldLocation[");
    if (instance != null)
      {
	sbuf.append(instance);
	sbuf.append(' ');
      }
    sbuf.append(type.getName());
    sbuf.append('.');
    sbuf.append(mname);
    /* DEBGUGGING:
    sbuf.append(" #:");
    sbuf.append(id);
    */
    sbuf.append(']');
    return sbuf.toString();
  }
}
