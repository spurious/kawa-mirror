package gnu.expr;
import gnu.bytecode.*;

public class BindingInitializer extends Initializer
{
  Declaration decl;
  Expression value;

  static final Method makeBindingMethod
  = Compilation.typeBinding.getDeclaredMethod("make", Compilation.string1Arg);

  public BindingInitializer(Declaration decl, Field field, Expression value)
  {
    this.decl = decl;
    this.value = value;
    this.field = field;
  }

  boolean createNewBinding = false;

  public void emit(Compilation comp)
  {
    CodeAttr code = comp.getCode();

    if (value instanceof QuoteExp)
      {
	Object val = ((QuoteExp) value).getValue();
	if (val == null || val instanceof String)
	  return;
	Literal lit = comp.litTable.findLiteral(val);
	if (lit.field == this.field)
	  return;
      }

    if (! field.getStaticFlag())
      code.emitPushThis();

    if (value == null)
      {
	// FIXME - this should be cached in a local Variable:
	if (! createNewBinding)
	  code.emitInvokeStatic(comp.getCurrentEnvironmentMethod);

	String name = decl.getName();
	if (name == null)
	  code.emitPushNull();
	else
	  code.emitPushString(name);
	if (createNewBinding)
	  code.emitInvokeStatic(makeBindingMethod);
	else
	  code.emitInvokeVirtual(comp.getBindingEnvironmentMethod);
      }
    else
      {
	value.compile (comp, field.getType());
      }

    if (field.getStaticFlag())
      code.emitPutStatic(field);
    else
      code.emitPutField(field);
  }
}
