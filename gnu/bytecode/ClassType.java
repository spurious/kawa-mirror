// Copyright (c) 1997, 1998, 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

public class ClassType extends ObjectType implements AttrContainer {
  public static final int minor_version = 3;
  public static final int major_version = 45;

  /** Find a ClassType with the given name, or create a new one.
   * Use this for "library classes", where you need the field/method types,
   * but not one where you are about to generate code for.
   * @param name the name of the class (e..g. "java.lang.String").
   */
  public static ClassType make(String name)
  {
    return (ClassType) Type.getType(name);
  }

  public static ClassType make (String name, ClassType superClass)
  {
    ClassType type = make(name);
    if (type.superClass == null)
      type.setSuper(superClass);
    return type;
  }

  int thisClassIndex;

  /** The super (base) class of the current class.
   * X.superClass == null means the superClass has not been specified,
   * and defaults to java.lang.Object. */
  ClassType superClass;
  /** The constant pool index of the superClass, or -1 if unassigned. */
  int superClassIndex = -1;

  ClassType[] interfaces;
  int[] interfaceIndexes;
  public int access_flags;

  Attribute attributes;
  public final Attribute getAttributes () { return attributes; }
  public final void setAttributes (Attribute attributes)
    { this.attributes = attributes; }

  String sourcefile;

  boolean emitDebugInfo = true;

  ConstantPool constants;

  public final ConstantPool getConstants () { return constants; }

  public final CpoolEntry getConstant(int i)
  {
    if (constants == null || constants.pool == null
	|| i > constants.count)
      return null;
    return constants.pool[i];
  }

  /** Return the modifiers (access flags) for this class. */
  public final int getModifiers()
  {
    if (access_flags == 0 && getReflectClass() != null)
      access_flags = reflectClass.getModifiers();
    return access_flags;
  }

  /** Set the modifiers (access flags) for this class. */
  public final void setModifiers(int flags) { access_flags = flags; }

  /** Sets the name of the class being defined in this classfile.
   * @param name the name to give to the class
   */
  public void setName (String name)
  {
    this_name = name;
    name = name.replace ('.', '/');
    setSignature("L"+name+";");
  }

  /** Set the name of the SourceFile associated with this class. */
  public void setSourceFile (String name)
  {
    SourceFileAttr.setSourceFile(this, name);
  }

  /**
   * Set the superclass of the is class.
   * @param name name of super class, or null if this is "Object".
   */
  public void setSuper (String name)
  {
    setSuper(name == null ? Type.pointer_type : ClassType.make(name));
  }

  public void setSuper (ClassType superClass)
  {
    this.superClass = superClass;
  }

  public ClassType getSuperclass ()
  {
    if (superClass == null && reflectClass != null)
      {
        Class superReflectClass = reflectClass.getSuperclass();
        // Neither Object nor interfaces have a superclass.
        // I.e. equivalent to if(this!=Type.pointer_type && !isInterface()).
        if (superReflectClass != null)
          superClass = (ClassType) make(superReflectClass);
      }
    return superClass;
  }

  /**
   * @return the interfaces this class is declared to implement
   * (not those inherited from its superclass/superinterfaces).
   */
  public ClassType[] getInterfaces()
  {
    if (interfaces == null && reflectClass != null)
      {
	Class[] reflectInterfaces = reflectClass.getInterfaces();
	interfaces = new ClassType[reflectInterfaces.length];

	for (int i = 0; i < reflectInterfaces.length; i++)
	  interfaces[i] = (ClassType) Type.make(reflectInterfaces[i]);
      }
    return interfaces;
  }

  public void setInterfaces (ClassType[] interfaces)
  { this.interfaces = interfaces; }

  /** Add an interface to the list of implemented interfaces. */
  public void addInterface (ClassType newInterface)
  {
    int oldCount;
    if (interfaces == null || interfaces.length == 0)
      {
	oldCount = 0;
	interfaces = new ClassType[1];
      }
    else
      {
	oldCount = interfaces.length;
	for (int i = oldCount;  --i >= 0; )
	  if (interfaces[i] == newInterface)
	    return;
	ClassType[] newInterfaces = new ClassType[oldCount+1];
	System.arraycopy(interfaces, 0, newInterfaces, 0, oldCount);
	interfaces = newInterfaces;
      }
    interfaces[oldCount] = newInterface;
  }

  public final boolean isInterface()
  { return (getModifiers() & Access.INTERFACE) != 0; }

  public ClassType () { }

  public ClassType (String class_name)
  {
    super();
    setName(class_name);
  }

  Field fields;
  int fields_count;
  Field last_field;
  /**  Constant pool index of "ConstantValue". */
  int ConstantValue_name_index;

  /** Constant pool index of "Code". */
  int Code_name_index;

  /** Constant pool index of "LocalVariableTable". */
  int LocalVariableTable_name_index;

  /** Constant pool index of "LineNumberTable". */
  int LineNumberTable_name_index;

  /** Get the fields of this class. */
  public final Field getFields()
  {
    if ((flags & (ADD_FIELDS_DONE|EXISTING_CLASS)) == EXISTING_CLASS)
      addFields();
    return fields;
  }

  public final int getFieldCount()
  {
    return fields_count;
  }

  /** Find a field with the given name declared in this class.
   * @return the matching field, or null if there is no such field.
   */
  public Field getDeclaredField(String name)
  {
    if ((flags & (ADD_FIELDS_DONE|EXISTING_CLASS)) == EXISTING_CLASS)
      addFields();
    Field field;
    for (field = fields;   field != null;  field = field.next)
      {
	if (name.equals(field.name))
	  return field;
      }
    return null;
  }

  /** Find a field with the given name declared in this class or its ancestors.
   * @return the matching field, or null if there is no such field.
   */
  public Field getField(String name)
  {
    ClassType cl = this;
    for (;;)
      {
        Field field = cl.getDeclaredField(name);
        if (field != null)
          return field;
        cl = cl.getSuperclass();
        if (cl == null)
          return null;
      }
  }

  /**
   * Add a new field to this class.
   */
  public Field addField () { return new Field (this); }

  /**
   * Add a new field to this class, and name the field.
   * @param name the name of the new field
   */
  public Field addField (String name) {
    Field field = new Field (this);
    field.setName(name);
    return field;
  }

  public final Field addField (String name, Type type) {
    Field field = new Field (this);
    field.setName(name);
    field.setType(type);
    return field;
  }

  public final Field addField (String name, Type type, int flags)
  {
    Field field = addField (name, type);
    field.flags = flags;
    return field;
  }

  /** Use reflection to add all the declared fields of this class.
   * Does not add private or package-private fields.
   * Does not check for duplicate (already-known) fields. */
  public void addFields()
  {
    Class clas = getReflectClass();
    java.lang.reflect.Field[] fields;
    try
      {
        fields = clas.getDeclaredFields();
      }
    catch (SecurityException ex)
      {
        fields = clas.getFields();
      }
    int count = fields.length;
    for (int i = 0;  i < count;  i++)
      {
        java.lang.reflect.Field field = fields[i];
        if (! field.getDeclaringClass().equals(clas))
          continue;
        int modifiers = field.getModifiers();
        if ((modifiers & (Access.PUBLIC|Access.PROTECTED)) == 0)
          continue;
        addField(field.getName(), Type.make(field.getType()), modifiers);
      }
    flags |= ADD_FIELDS_DONE;
  }

  Method methods;
  int methods_count;
  Method last_method;
  public Method constructor;

  /** Get the methods of this class. */
  public final Method getMethods()
  {
    return methods;
  }
 
  public final int getMethodCount() {
    return methods_count;
  }
 
  Method addMethod () {
    return new Method (this, 0);
  }

  public Method addMethod (String name) {
    Method method = new Method (this, 0);
    method.setName(name);
    return method;
  }

  public Method addMethod (String name, int flags) {
    Method method = new Method (this, flags);
    method.setName(name);
    return method;
  }

  // deprecated:
  public Method addMethod (String name,
			   Type[] arg_types, Type return_type,
			   int flags) {
    return addMethod(name, flags, arg_types, return_type);
  }

  /** Add a method to this ClassType.
    * If an existing method matches, return that.  Otherwise, create
    * a new one.
    * In contrast, the other addMethod methods always create new Methods. */
  public Method addMethod (String name, int flags,
			   Type[] arg_types, Type return_type)
  {
    Method method = getDeclaredMethod(name, arg_types);
    if (method != null
        && return_type.equals(method.getReturnType())
        && (flags & method.access_flags) == flags)
      return method;
    method = new Method (this, flags);
    method.setName(name);
    method.arg_types = arg_types;
    method.return_type = return_type;
    return method;
  }

  public Method addMethod (String name,  String signature, int flags)
  {
    Method meth = addMethod(name, flags);
    meth.setSignature(signature);
    return meth;
  }

  public Method getDeclaredMethods()
  {
    if ((flags & (ADD_METHODS_DONE|EXISTING_CLASS)) == EXISTING_CLASS)
      addMethods(getReflectClass());
    return methods;
  }

  /** Get methods matching the given filter. */
  public Method[] getMethods (Filter filter, boolean searchSupers)
  {
    int count = 0;
    for (ClassType ctype = this;  ctype != null;
	 ctype = ctype.getSuperclass())
    {
      for (Method meth = ctype.getDeclaredMethods();
	   meth != null;  meth = meth.getNext())
	if (filter.select(meth))
	  count++;
      if (! searchSupers)
	break;
    }
    Method[] methods = new Method[count];
    count = 0;
    for (ClassType ctype = this;  ctype != null;
	 ctype = ctype.getSuperclass())
    {
      for (Method meth = ctype.getDeclaredMethods();
	   meth != null;  meth = meth.getNext())
	if (filter.select(meth))
	  methods[count++] = meth;
      if (! searchSupers)
	break;
    }
    return methods;
  }

  public Method getDeclaredMethod(String name, Type[] arg_types)
  {
    if ((flags & (ADD_METHODS_DONE|EXISTING_CLASS)) == EXISTING_CLASS)
      addMethods(getReflectClass());
    Method method;
    for (method = methods;  method != null;  method = method.next)
      {
	if (! name.equals(method.getName()))
	  continue;
	Type[] method_args = method.getParameterTypes();
	if (arg_types == null || arg_types == method_args)
	  return method;
	int i = arg_types.length;
	if (i != method_args.length)
	  continue;
	while (-- i >= 0)
	  {
	    if (arg_types[i] != method_args[i])
	      break;
	  }
	if (i < 0)
	  return method;
      }
    return null;
  }

  /** Get a method with matching name and number of arguments. */
  public Method getDeclaredMethod(String name, int argCount)
  {
    if ((flags & (ADD_METHODS_DONE|EXISTING_CLASS)) == EXISTING_CLASS)
      addMethods(getReflectClass());
    Method result = null;
    for (Method method = methods;  method != null;  method = method.next)
      {
	if (name.equals(method.getName())
	    && argCount == method.getParameterTypes().length)
	  {
	    if (result != null)
	      throw new Error("ambiguous call to getDeclaredMethod(\""
			      + name + "\", " + argCount+
			      ")\n - " + result + "\n - " + method);
	    result = method;
	  }
      }
    return result;
  }

  public Method getMethod(String name, Type[] arg_types)
  {
    ClassType cl = this;
    for (;;)
      {
        Method method = cl.getDeclaredMethod(name, arg_types);
        if (method != null)
          return method;
        cl = cl.getSuperclass();
        if (cl == null)
          return null;
      }
  }

  /** Use reflection to add all the declared methods of this class.
   * Does not add constructors nor private or package-private methods.
   * Does not check for duplicate (already-known) methods.
   * @param clas should be the same as getReflectClass(). */
  public void addMethods(Class clas)
  {
    java.lang.reflect.Method[] methods;
    try
      {
        methods = clas.getDeclaredMethods();
      }
    catch (SecurityException ex)
      {
        methods = clas.getMethods();
      }
    int count = methods.length;
    for (int i = 0;  i < count;  i++)
      {
        java.lang.reflect.Method method = methods[i];
        if (! method.getDeclaringClass().equals(clas))
          continue;
        int modifiers = method.getModifiers();
        if ((modifiers & (Access.PUBLIC|Access.PROTECTED)) == 0)
          continue;
        Class[] paramTypes = method.getParameterTypes();
        int j = paramTypes.length;
        Type[] args = new Type[j];
        while (--j >= 0)
          args[j] = Type.make(paramTypes[j]);
        Method meth = new Method (this, modifiers);
        meth.setName(method.getName());
        meth.arg_types = args;
        meth.return_type = Type.make(method.getReturnType());
      }

    java.lang.reflect.Constructor[] cmethods;
    try
      {
        cmethods = clas.getDeclaredConstructors();
      }
    catch (SecurityException ex)
      {
        cmethods = clas.getConstructors();
      }
    count = cmethods.length;
    for (int i = 0;  i < count;  i++)
      {
        java.lang.reflect.Constructor method = cmethods[i];
        if (! method.getDeclaringClass().equals(clas))
          continue;
        int modifiers = method.getModifiers();
        if ((modifiers & (Access.PUBLIC|Access.PROTECTED)) == 0)
          continue;
        Class[] paramTypes = method.getParameterTypes();
        int j = paramTypes.length;
        Type[] args = new Type[j];
        while (--j >= 0)
          args[j] = Type.make(paramTypes[j]);
        Method meth = new Method (this, modifiers);
        meth.setName("<init>");
        meth.arg_types = args;
        meth.return_type = Type.void_type;
      }

    flags |= ADD_METHODS_DONE;
  }

  public Method[] getMatchingMethods(String name, Type[] paramTypes, int flags)
  {
    int i = getMethodCount();
    int nMatches = 0;
    java.util.Vector matches = new java.util.Vector(10);
    for (Method method = methods;  method != null;  method = method.getNext())
    {
      if (! name.equals(method.getName()))
        continue;
      if ((flags & Access.STATIC) != (method.access_flags & Access.STATIC))
        continue;
      if ((flags & Access.PUBLIC) > (method.access_flags & Access.PUBLIC))
        continue;
      Type[] mtypes = method.arg_types;
      if (mtypes.length != paramTypes.length)
        continue;
      nMatches++;
      matches.addElement(method);
    }
    Method[] result = new Method[nMatches];
    matches.copyInto(result);
    return result;
  }

  /** Do various fixups after generating code but before we can write it out.
   * This includes assigning constant pool indexes where needed,
   * finalizing labels, etc. */
  public void doFixups ()
  {
    if (constants == null)
      constants = new ConstantPool();
    if (thisClassIndex == 0)
      thisClassIndex = constants.addClass(this).index;
    if (superClass == this)
      setSuper((ClassType) null);
    if (superClassIndex < 0)
      superClassIndex = superClass == null ? 0
	: constants.addClass(superClass).index;
    if (interfaces != null && interfaceIndexes == null)
      {
	int n = interfaces.length;
	interfaceIndexes = new int [n];
	for (int i = 0;  i < n;  i++)
	  interfaceIndexes[i] = constants.addClass(interfaces[i]).index;
      }
    for (Field field = fields; field != null; field = field.next) {
      field.assign_constants (this);
    }
    for (Method method = methods; method != null; method = method.next) {
      CodeAttr code = method.code;
      if (code == null)
        continue;
      for (;;)
        {
          CodeFragment frag = code.fragments;
          if (frag == null)
            break;
          code.fragments = frag.next;
          frag.emit(code);
        }
      method.assign_constants ();
      method.code.finalize_labels ();
    }
    Attribute.assignConstants(this, this);
  }

  public void writeToStream (OutputStream stream)
    throws java.io.IOException
  {
    java.io.DataOutputStream dstr = new java.io.DataOutputStream (stream);
    int i;

    doFixups ();

    dstr.writeInt (0xcafebabe);  // magic
    dstr.writeShort (minor_version);
    dstr.writeShort (major_version);

    // Write out the constant pool.
    if (constants == null)
      dstr.writeShort (1);
    else
      constants.write(dstr);

    dstr.writeShort (access_flags);
    dstr.writeShort (thisClassIndex);
    dstr.writeShort (superClassIndex);
    if (interfaceIndexes == null)
      dstr.writeShort (0);  // interfaces_count
    else
      {
	int interfaces_count = interfaceIndexes.length;
	dstr.writeShort (interfaces_count);
	for (i = 0;  i < interfaces_count; i++)
	  dstr.writeShort (interfaceIndexes[i]);
      }

    dstr.writeShort (fields_count);
    for (Field field = fields;  field != null;  field = field.next)
      field.write (dstr, this);

    dstr.writeShort (methods_count);
    for (Method method = methods;  method != null;  method = method.next)
      method.write (dstr, this);

    Attribute.writeAll (this, dstr);

    flags |= ADD_FIELDS_DONE | ADD_METHODS_DONE;
  }

  public void writeToFile (String filename)
    throws java.io.IOException
 {
    OutputStream stream
      = new BufferedOutputStream(new FileOutputStream (filename));
    writeToStream (stream);
    stream.close ();
  }

  public void writeToFile ()
    throws java.io.IOException
  {
    writeToFile (this_name.replace ('.', File.separatorChar) + ".class");
  }

  public byte[] writeToArray ()
    throws java.io.IOException
  {
    ByteArrayOutputStream stream = new ByteArrayOutputStream (500);
    writeToStream(stream);
    return stream.toByteArray ();    
  }

  /**
   * Convert a String to a Utf8 byte array.
   * @param str the input String.
   * @return the input encoded as a utf8 byte array.
   */
  public static byte[] to_utf8 (String str)
  {
    if (str == null)
      return null;
    int str_len = str.length ();
    int utf_len = 0;
    for (int i = 0; i < str_len; i++) {
      int c = str.charAt(i);
      if ((c > 0) && (c <= 0x7F))
	utf_len++;
      else if (c <= 0x7FF)
	utf_len += 2;
      else
	utf_len += 3;
    }
    byte[] buffer = new byte[utf_len];
    int j = 0;
    for (int i = 0; i < str_len; i++) {
      int c = str.charAt(i);
      if ((c > 0) && (c <= 0x7F))
	buffer[j++] = (byte) c;
      else if (c <= 0x7FF) {
	buffer[j++] = (byte) (0xC0 | ((c >>  6) & 0x1F));
	buffer[j++] = (byte) (0x80 | ((c >>  0) & 0x3F));
      } else {
	buffer[j++] = (byte) (0xE0 | ((c >> 12) & 0x0F));
	buffer[j++] = (byte) (0x80 | ((c >>  6) & 0x3F));
	buffer[j++] = (byte) (0x80 | ((c >>  0) & 0x3F));
      }
    }
    return buffer;
  }

  public final boolean isSubclass(ClassType other)
  {
    ClassType baseClass = this;
    while (baseClass != null)
      {
        if (baseClass == other)
          return true;
        baseClass = baseClass.getSuperclass();
      }
    return false;
  }

  public int compare(Type other)
  {
    if (other == nullType)
      return 1;
    if (other instanceof PrimType)
      return swappedCompareResult(((PrimType) other).compare(this));
    if (other instanceof ArrayType)
      return swappedCompareResult(((ArrayType) other).compare(this));
    if (! (other instanceof ClassType))
      return -3;
    String name = getName();
    if (name != null && name.equals(other.getName()))
      return 0;
    ClassType cother = (ClassType) other;
    if (this.isInterface() || cother.isInterface())
      return -2;
    if (isSubclass(cother))
      return -1;
    if (cother.isSubclass(this))
      return 1;
    return -3;
  }

  public String toString()
  {
    return "ClassType " + getName();
  }
}
