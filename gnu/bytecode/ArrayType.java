package codegen;

public class ArrayType extends Type
{
  Type elements;

  static private byte[] array_signature (byte[] element_signature)
  {
    byte[] sig = new byte[element_signature.length+1];
    System.arraycopy (element_signature, 0, sig, 1, element_signature.length);
    sig[0] = '[';
    return sig;
  }

  static private byte[] array_typename (byte[] element_typename)
  {
    byte[] name = new byte[element_typename.length+2];
    System.arraycopy (element_typename, 0, name, 0, element_typename.length);
    name[element_typename.length] = '[';
    name[element_typename.length+1] = ']';
    return name;
  }

  public ArrayType (Type elements)
  {
    super (array_typename (elements.name),
	   array_signature (elements.signature));
    this.elements = elements;
  }
}
