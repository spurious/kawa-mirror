package gnu.bytecode;

public class ArrayType extends Type
{
  public Type elements;

  public ArrayType (Type elements)
  {
    super (elements.getName() + "[]", "[" + elements.getSignature());
    this.elements = elements;
  }
}
