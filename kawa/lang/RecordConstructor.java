package kawa.lang;
import java.lang.reflect.Field;
import gnu.bytecode.ClassType;

public class RecordConstructor extends ProcedureN
{
  Class clas;
  Field[] fields;

  public RecordConstructor (Class clas, Field[] fields)
  {
    this.clas = clas;
    this.fields = fields;
  }

  public RecordConstructor (Class clas)
  {
    this.clas = clas;
    this.fields = clas.getDeclaredFields();
  }

  public RecordConstructor (Class clas, String[] fnames) throws GenericError
  {
    this.clas = clas;
    this.fields = new Field[fnames.length];
    for (int i = 0;  i < fnames.length;  i++)
      {
	String fname = fnames[i];
	try
	  {
	    this.fields[i] = clas.getField(fname);
	  }
	catch (NoSuchFieldException ex)
	  {
	    throw new GenericError ("no such field "+fname+" in "+clas.getName());
	  }
      }
  }

  public RecordConstructor (ClassType ctype, Object fieldsList)
  {
    this(ctype.getReflectClass(), fieldsList);
  }

  public RecordConstructor (Class clas, Object fieldsList)
  {
    this.clas = clas;
    if (fieldsList == null)
      {
	 this.fields = clas.getDeclaredFields();
      }
    else
      {
	int nfields = List.length(fieldsList);
	this.fields = new Field[nfields];
	for (int i = 0;  i < nfields;  i++)
	  {
	    Pair pair = (Pair) fieldsList;
	    String fname = pair.car.toString();
	    try
	      {
		this.fields[i] = clas.getField(fname);
	      }
	    catch (NoSuchFieldException ex)
	      {
		throw new GenericError ("no such field "+fname+" in "+clas.getName());
	      }
	    fieldsList = pair.cdr;
	  }
      }
  }

  public int numArgs()
  {
    int nargs = fields.length;
    return (nargs<<12)|nargs;
  }

  public String getName()
  {
    return clas.getName()+" constructor";
  }

  public Object applyN (Object[] args)
  {
    Object obj;
    try
      {
	obj = clas.newInstance();
      }
    
    catch (InstantiationException ex)
      {
	throw new GenericError (ex.toString());
      }
    catch (IllegalAccessException ex)
      {
	throw new GenericError (ex.toString());
      }
    if (args.length != fields.length)
      throw new WrongArguments(this, args.length);
    for (int i = 0;  i < args.length;  i++)
      {
	Field fld = fields[i];
	try
	  {
	    fld.set(obj, args[i]);
	  }
	catch (IllegalAccessException ex)
	  {
	    throw new GenericError("illegal access for field "+fld.getName());
	  }
      }
    return obj;
  }
}
