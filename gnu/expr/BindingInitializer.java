package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

public class BindingInitializer extends Initializer
{
  Declaration decl;
  Expression value;

  /** Create a BindingInitializer and link it into the correct
   * intializer chain. */
  public static void create (Declaration decl, Expression value,
                             Compilation comp)
  {
    BindingInitializer init = new BindingInitializer(decl, value);
    if (decl.field != null && decl.field.getStaticFlag())
      {
        init.next = comp.clinitChain;
        comp.clinitChain = init;
      }
    else
      {
        init.next = comp.mainLambda.initChain; // FIXME why mainLambda?
        comp.mainLambda.initChain = init;
      }
  }

  public BindingInitializer(Declaration decl, Expression value)
  {
    this.decl = decl;
    this.value = value;
    this.field = decl.field;
  }

  public void emit(Compilation comp)
  {
    CodeAttr code = comp.getCode();

    if (value instanceof QuoteExp)
      {
	Object val = ((QuoteExp) value).getValue();
	if (val != null && ! (val instanceof String))
	  {
	    Literal lit = comp.litTable.findLiteral(val);
	    if (lit.field == this.field)
	      return;
	  }
      }

    int line = decl.getLineNumber();
    if (line > 0)
      code.putLineNumber(decl.getFileName(), line);

    if (field != null && ! field.getStaticFlag())
      code.emitPushThis();

    if (value == null)
      {
	boolean func = comp.getLanguage().hasSeparateFunctionNamespace();
	Object property
	  = func && decl.isProcedureDecl() ? EnvironmentKey.FUNCTION : null;

	Object name = decl.getSymbol();

	if (decl.isAlias())
          {
            comp.compileConstant(name, Target.pushObject);
            code.emitInvokeStatic(makeLocationMethod(name));
          }
	else
	  {
	    ClassType t = ClassType.make("gnu.mapping.ThreadLocation");
	    if (decl.getFlag(Declaration.IS_UNKNOWN
			     |Declaration.IS_DYNAMIC|Declaration.IS_FLUID))
	      {
                if (name instanceof String)
                  name = Namespace.EmptyNamespace.getSymbol((String) name);
                comp.compileConstant(name, Target.pushObject);
		if (property == null)
		  code.emitPushNull();
		else
		  comp.compileConstant(property, Target.pushObject);
		code.emitInvokeStatic(t.getDeclaredMethod("getInstance", 2));
	      }
	    else
	      {
                Type[] atypes = new Type[1];
                if (name instanceof Symbol)
                  atypes[0] = Compilation.typeSymbol;
                else
                  atypes[0] = Type.tostring_type;
                comp.compileConstant(name, Target.pushObject);
		code.emitInvokeStatic(t.getDeclaredMethod("makePrivate",
                                                          atypes));
	      }
	  }
      }
    else
      {
        Type type = field == null ? decl.getType() : field.getType();
	value.compileWithPosition(comp, StackTarget.getInstance(type));
      }

    // Optimization of Declaration.compileStore, to avoid swap.
    if (field == null)
      {
	Variable var = decl.getVariable();
	if (var == null)
	  var = decl.allocateVariable(code);
	code.emitStore(var);
      }
    else if (field.getStaticFlag())
      code.emitPutStatic(field);
    else
      code.emitPutField(field);
  }

  public static Method makeLocationMethod (Object name)
  {
    Type[] atypes = new Type[1];
    if (name instanceof Symbol)
      atypes[0] = Compilation.typeSymbol;
    else
      atypes[0] = Type.string_type;
    return Compilation.typeLocation.getDeclaredMethod("make", atypes);
  }
}
