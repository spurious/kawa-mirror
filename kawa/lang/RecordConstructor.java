package kawa.lang;
import java.lang.reflect.Field;

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

  public RecordConstructor (Class clas, Object fieldsList) throws GenericError
  {
    this.clas = clas;
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

  /*
  public RecordConstructor (Class clas, Object fieldsList) throws GenericError
  {
    this.clas = clas;
    if (fieldsList == List.Empty)
      {
	 this.fields = clas.getDeclaredFields();
      }
    else
      {
	fieldsList = ((Pair) fieldsList).car;
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
  */

  public Object applyN (Object[] args)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
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
      throw new GenericError("wrong number of arguments to record constructor");
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
