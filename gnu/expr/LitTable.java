package gnu.expr;
import java.io.*;
import gnu.bytecode.*;
import java.lang.reflect.Array;

/** Manages the literals of a Compilation.
 * Implements ObjectOutput, because we use externalization to determine
 * how literals get compiled into code that re-creates the literal. */

public class LitTable implements ObjectOutput
{
  Compilation comp;

  public LitTable(Compilation comp)
  {
    this.comp = comp;
  }

  public void emit() throws IOException
  {
    // We use two passes.  The first generates the graph of
    // objects and how they are generated.
    // The second pass actually emits code.
    // The reason for using two passes is so we can detect cycles
    // and sharing using the first pass.  This generates better code:
    // If an object is only used once, and as not a top-level literal,
    // they we don't need to allocate a Field for it.  And if an object
    // does not cyclically depend on itself, we can allocate *and*
    // initialize using a single call, which generates better code.

    // Here is the first pass.
    for (Literal init = comp.literalsChain;  init != null;
	 init = init.next)
      {
	writeObject(init.value);
      }

    // Here is the second pass.
    for (Literal init = comp.literalsChain;  init != null;
	 init = init.next)
      {
	emit(init, true);
      }

    // For speedier garbage collection.
    comp.literalTable = null;
    comp.literalsCount = 0;
  }

  Object[] valueStack = new Object[20];
  Type[] typeStack = new Type[20];
  int stackPointer;
  
  void push(Object value, Type type)
  {
    if (stackPointer >= valueStack.length)
      {
	Object[] newValues = new Object[2 * valueStack.length];
	Type[] newTypes = new Type[2 * typeStack.length];
	System.arraycopy(valueStack, 0, newValues, 0, stackPointer);
	System.arraycopy(typeStack, 0, newTypes,  0, stackPointer);
	valueStack = newValues;
	typeStack = newTypes;
      }
    valueStack[stackPointer] = value;
    typeStack[stackPointer] = type;
    stackPointer++;
  }

  void error(String msg)
  {
    throw new Error(msg);
  }

  public void flush()
  {
  }

  public void close()
  {
  }

  public void write(int b) throws IOException
  {
    error("cannot handle call to write(int) when externalizing literal");
  }

  public void writeBytes(String s) throws IOException
  {
    error("cannot handle call to writeBytes(String) when externalizing literal");
  }

  public void write(byte[] b) throws IOException
  {
    error("cannot handle call to write(byte[]) when externalizing literal");
  }

  public void write(byte[] b, int off, int len) throws IOException
  {
    error("cannot handle call to write(byte[],int,int) when externalizing literal");
  }

  public void writeBoolean(boolean v)
  {
    push(new Boolean(v), Type.boolean_type);
  }

  public void writeChar(int v)
  {
    push(new Character((char) v), Type.char_type);
  }

  public void writeByte(int v)
  {
    push(new Byte((byte) v), Type.byte_type);
  }

  public void writeShort(int v)
  {
    push(new Short((short) v), Type.short_type);
  }

  public void writeInt(int v)
  {
    push(new Integer(v), Type.int_type);
  }

  public void writeLong(long v)
  {
    push(new Long(v), Type.long_type);
  }

  public void writeFloat(float v)
  {
    push(new Float(v), Type.float_type);
  }

  public void writeDouble(double v)
  {
    push(new Double(v), Type.double_type);
  }

  public void writeUTF(String v) 
  {
    push(v, Type.string_type);
  }

  public void writeChars(String v) 
  {
    push(v, Type.string_type);
  }

  public void writeObject(Object obj) throws IOException
  {
    Literal lit = comp.findLiteral(obj);
    if ((lit.flags & (Literal.WRITTEN|Literal.WRITING)) != 0)
      {
	// It is referenced more than once, so we we need a Field
	// to save the value.
	if (lit.field == null && ! (obj instanceof String))
	  lit.assign (comp);
	if ((lit.flags & Literal.WRITTEN) == 0)
	  lit.flags |= Literal.CYCLIC;
      }
    else
      {
	lit.flags |= Literal.WRITING;
	int oldStack = stackPointer;
	if (obj instanceof gnu.lists.FString
	    && ((gnu.lists.FString) obj).size() < 65535)
	  { // Optimization.
	    push(obj.toString(), Type.string_type);
	  }
	else if (obj instanceof Externalizable)
	  {
	    ((Externalizable) obj).writeExternal(this);
	  }
	else if (obj instanceof Object[])
	  {
	    Object[] arr = (Object[]) obj;
	    for (int i = 0;  i < arr.length;  i++)
	      {
		writeObject(arr[i]);
	      }
	  }
	else if (obj == null
                 || obj instanceof String || lit.type instanceof ArrayType)
	  {
	    // nothing to do
	  }
	else
	  error(obj.getClass().getName()+" does not implement Externalizable");
	int nargs = stackPointer - oldStack;
	if (nargs == 0)
	  {
	    lit.argValues = gnu.mapping.Values.noArgs;
	    lit.argTypes = Type.typeArray0;
	  }
	else
	  {
	    lit.argValues = new Object[nargs];
	    lit.argTypes = new Type[nargs];
	    System.arraycopy(valueStack, oldStack, lit.argValues, 0, nargs);
	    System.arraycopy(typeStack, oldStack, lit.argTypes, 0, nargs);
	    stackPointer = oldStack;
	  }
	lit.flags |= Literal.WRITTEN;
      }
    push(lit, lit.type);
  }

  Method getMethod (ClassType type, String name,
		    Literal literal, boolean isStatic)
  {
    Type[] argTypes = literal.argTypes;
    Method method = type.getDeclaredMethods();
    int argLength = argTypes.length;
    Method best = null;
    long bestArrayArgs = 0;
    boolean ambiguous = false;
    Type[] bParameters = null;
  methodLoop:
    for (; method != null;  method = method.getNext())
      {
	if (! name.equals(method.getName()))
	  continue;
	boolean mstatic = method.getStaticFlag();
	if (isStatic != mstatic)
	  continue;
	// One bit set for each array parameter.
	long arrayArgs = 0;
	Type[] mParameters = method.getParameterTypes();
	int iarg = 0;  int iparam = 0;
	for (;; iarg++, iparam++)
	  {
	    if (iarg == argLength && iparam == mParameters.length)
	      {
		if (best == null || (bestArrayArgs != 0 && arrayArgs == 0))
		  {
		    best = method;
		    bParameters = mParameters;
		    bestArrayArgs = arrayArgs;
		  }
		else if (arrayArgs == 0)
		  {
		    // Now see which of 'best' and 'method' is more specific.

		    // True if we know best cannot be the more specific.
		    boolean not1 = false;
		    // True if we know new method cannot be the more specific.
		    boolean not2 = false;
		    for (int j = argLength;  --j >= 0; )
		      {
			int c = bParameters[j].compare(mParameters[j]);
			if (c != 1)
			  {
			    not2 = true;
			    if (not1)
			      break;
			  }
			if (c != -1)
			  {
			    not1 = true;
			    if (not2)
			      break;
			  }
		      }
		    if (not1)
		      {
			best = method;
			bParameters = mParameters;
		      }
		    ambiguous = not1 && not2;
		  }
		continue methodLoop;  // Look for other matches.
	      }
	    if (iarg == argLength || iparam == mParameters.length)
	      continue methodLoop;  // fail on this method
	    Type aType = argTypes[iarg];
	    Type pType = mParameters[iparam];
	    if (aType.isSubtype(pType))
	      ; // OK so far
	    else if (pType instanceof ArrayType && iparam < 64
		     && (aType == Type.int_type || aType == Type.short_type))
	      {
		int count = ((Number) literal.argValues[iarg]).intValue();
		if (count < 0 && type.getName().equals("gnu.math.IntNum"))
		  count -= 0x80000000; // special hack for IntNum.
		Type elementType = ((ArrayType) pType).getComponentType();
		if (count < 0 || iarg + count >= argLength)
		  continue methodLoop;  // fail on this method
		else
		  {
		    for (int j = count;  --j >= 0; )
		      {
			Type t = argTypes[iarg + j + 1];
			if (elementType instanceof PrimType
			    ? elementType.getSignature() != t.getSignature()
			    : ! t.isSubtype(elementType))
			  continue methodLoop;  // fail on this method
		      }
		    iarg += count;
		    arrayArgs |= 1 << iparam;
		  }
	      }
	    else
	      {
	      continue methodLoop;  // fail on this method
	      }
	  }
      }
    if (ambiguous)
      return null;
    if (bestArrayArgs != 0)
      {
	Object[] args = new Object[bParameters.length];
	Type[] types = new Type[bParameters.length];
	int iarg = 0;  int iparam = 0;
	for (;; iarg++, iparam++)
	  {
	    if (iarg == argLength)
	      break;
	    Type aType = argTypes[iarg];
	    Type pType = bParameters[iparam];
	    if ((bestArrayArgs & (1 << iparam)) == 0)
	      {
		args[iparam] = literal.argValues[iarg];
		types[iparam] = literal.argTypes[iarg];
	      }
	    else
	      {
		int count = ((Number) literal.argValues[iarg]).intValue();
		boolean isIntNum = type.getName().equals("gnu.math.IntNum");
		if (isIntNum)
		  count -= 0x80000000; // special hack for IntNum.
		Type elementType = ((ArrayType) pType).getComponentType();
		types[iparam] = pType;
		args[iparam] = Array.newInstance(elementType.getReflectClass(),
						 count);
		Object[] argValues = literal.argValues;
		if (isIntNum)
		  {
		    // Special kludge for IntNum:  words are Externalized
		    // in big-endian (network) order, but the representation
		    // is little-endian.
		    int[] arr = (int[]) args[iparam];
		    for (int j = count;  j > 0;  j--)
		      arr[count - j]
			= ((Integer) argValues[iarg + j]).intValue();
		  }
		else
		  {
		    for (int j = count;  --j >= 0; )
		      Array.set(args[iparam], j, argValues[iarg + 1 + j]);
		  }
		Literal arrayLiteral = new Literal(args[iparam], pType);
		if (elementType instanceof ObjectType)
		  arrayLiteral.argValues = (Object[]) args[iparam];
		args[iparam] = arrayLiteral;
		iarg += count;
	      }
	  }
	literal.argValues = args;
	literal.argTypes = types;
      }
    return best;
  }

  void putArgs(Literal literal, CodeAttr code)
  {
    Type[] argTypes = literal.argTypes;
    int len = argTypes.length;
    for (int i = 0;  i < len;  i++)
      {
	Object value = literal.argValues[i];
	if (value instanceof Literal)
	  emit((Literal) value, false);
	else
	  comp.compileConstant(value, new StackTarget(argTypes[i]));
      }
  }

  // FIXME - move this to CodeAttr?
  void emitPrimArray(Object value, ArrayType arrayType, CodeAttr code)
  {
    Type elementType = arrayType.getComponentType();
    int len = java.lang.reflect.Array.getLength(value);
    code.emitPushInt(len);
    code.emitNewArray(elementType);
    char sig = elementType.getSignature().charAt(0);
    for (int i = 0;  i < len;  i++)
      {
	long ival = 0;  float fval = 0;  double dval = 0;
	switch (sig)
	  {
	  case 'J':
	    ival = ((long[]) value)[i];
	    if (ival == 0)
	      continue;
	    break;
	  case 'I':
	    ival = ((int[]) value)[i];
	    if (ival == 0)
	      continue;
	    break;
	  case 'S':
	    ival = ((short[]) value)[i];
	    if (ival == 0)
	      continue;
	    break;
	  case 'C':
	    ival = ((char[]) value)[i];
	    if (ival == 0)
	      continue;
	    break;
	  case 'B':
	    ival = ((byte[]) value)[i];
	    if (ival == 0)
	      continue;
	    break;
	  case 'Z':
	    ival = ((boolean[]) value)[i] ? 1 : 0;
	    if (ival == 0)
	      continue;
	    break;
	  case 'F':
	    fval = ((float[]) value)[i];
	    if (fval == 0.0)
	      continue;
	    break;
	  case 'D':
	    dval = ((double[]) value)[i];
	    if (dval == 0.0)
	      continue;
	    break;
	  }
	code.emitDup(arrayType);
	code.emitPushInt(i);
	switch (sig)
	  {
	  case 'Z':
	  case 'C':
	  case 'B':
	  case 'S':
	  case 'I':
	    code.emitPushInt((int) ival);
	    break;
	  case 'J':
	    code.emitPushLong(ival);
	    break;
	  case 'F':
	    code.emitPushFloat(fval);
	    break;
	  case 'D':
	    code.emitPushDouble(dval);
	    break;
	  }
	code.emitArrayStore(elementType);
      }
  }

  void emit(Literal literal, boolean ignore)
  {
    CodeAttr code = comp.getCode();
    if (literal.value == null)
      {
	if (! ignore)
	  code.emitPushNull();
      }
    else if (literal.value instanceof String)
      {
	if (! ignore)
	  code.emitPushString(literal.value.toString ());
      }
    else if ((literal.flags & Literal.EMITTED) != 0)
      {
	if (! ignore)
	  code.emitGetStatic(literal.field);
      }
    else if (literal.value instanceof Object[])
      {
	int len = literal.argValues.length;
	Type elementType = ((ArrayType) literal.type).getComponentType();
	code.emitPushInt(len);
	code.emitNewArray(elementType);
	if (literal.field != null)
	  {
	    if (! ignore)
	      code.emitDup(literal.type);
	    code.emitPutStatic(literal.field);
	  }
	literal.flags |= Literal.EMITTED;
	for (int i = 0;  i < len;  i++)
	  {
	    Literal el = (Literal) literal.argValues[i];
	    if (el.value == null)
	      continue;
	    code.emitDup(elementType);
	    code.emitPushInt(i);
	    emit(el, false);
	    code.emitArrayStore(elementType);
	  }
      }
    else if (literal.type instanceof ArrayType)
      {
	emitPrimArray(literal.value, (ArrayType) literal.type, code);
	if (literal.field != null)
	  {
	    if (! ignore)
	      code.emitDup(literal.type);
	    code.emitPutStatic(literal.field);
	  }
	literal.flags |= Literal.EMITTED;
      }
    else
      {
	Interpreter interpreter = comp.getInterpreter();
	ClassType type = (ClassType) literal.type;
	boolean useDefaultInit = (literal.flags & Literal.CYCLIC) != 0;
	Method method = null;
	boolean makeStatic = false;
	if (! useDefaultInit)
	  {
	    // look for matching "make" method;
	    method = getMethod(type, "make", literal, true);
	    // otherwise look for matching constructor;
	    if (method != null)
	      makeStatic = true;
	    else if (literal.argTypes.length > 0)
	      method = getMethod(type, "<init>", literal, false);
	    if (method == null)
	      useDefaultInit = true;
	  }
	if (useDefaultInit)
	  {
	    method = getMethod(type, "set", literal, false);
	    // otherwise error;
	  }
	if (method == null && literal.argTypes.length > 0)
	  error("no method to construct "+literal.type);
	if (makeStatic)
	  {
	    putArgs(literal, code);
	    code.emitInvokeStatic(method);
	  }
	else if (useDefaultInit)
	  {
	    code.emitNew(type);
	    code.emitDup(type);
	    Method init0 = type.getDeclaredMethod("<init>", 0);
	    code.emitInvokeSpecial(init0);
	  }
	else
	  {
	    code.emitNew(type);
	    code.emitDup(type);
	    putArgs(literal, code);
	    code.emitInvokeSpecial(method);
	  }
	Method resolveMethod
	  = makeStatic ? null : type.getDeclaredMethod("readResolve", 0);
	if (resolveMethod != null)
	  {
	    code.emitInvokeVirtual(resolveMethod);
	    type.emitCoerceFromObject(code);
	  }
	if (literal.field != null)
	  {
	    if (! ignore || (useDefaultInit && method != null))
	      code.emitDup(type);
	    code.emitPutStatic(literal.field);
	  }
	literal.flags |= Literal.EMITTED;
	if (useDefaultInit && method != null)
	  {
	    if (! ignore)
	      code.emitDup(type);
	    putArgs(literal, code);
	    code.emitInvokeVirtual(method);
	  }
      }
  }

}
