package gnu.kawa.lispexpr;
import kawa.lang.*;
import gnu.expr.*;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.bytecode.ClassType;

public class DefineNamespace extends Syntax
{
  private boolean makePrivate;
  private boolean makeXML;
  
  public static final DefineNamespace define_namespace
    = new DefineNamespace();
  public static final DefineNamespace define_private_namespace
    = new DefineNamespace();
  public static final DefineNamespace define_xml_namespace
    = new DefineNamespace();
  static {
    define_namespace.setName("define-namespace");
    define_private_namespace.setName("define-private-namespace");
    define_private_namespace.makePrivate = true;
    define_xml_namespace.setName("define-xml-namespace");
    define_xml_namespace.makeXML = true;
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Pair p1, p2;
    if (! (st.cdr instanceof Pair)
        || ! ((p1 = (Pair) st.cdr).car instanceof String)
	|| ! (p1.cdr instanceof Pair)
	|| (p2 = (Pair) p1.cdr).cdr != LList.Empty)
      {
	tr.error('e', "invalid syntax for define-namespace");
	return false;
      }
    String name = (String) p1.car;
    Declaration decl = defs.getDefine(name, 'w', tr);
    tr.push(decl);
    decl.setFlag(Declaration.IS_CONSTANT|Declaration.IS_NAMESPACE_PREFIX);
    if (makePrivate)
      {
        decl.setFlag(Declaration.PRIVATE_SPECIFIED);
        decl.setPrivate(true);
      }
    else if (defs instanceof ModuleExp)
      decl.setCanRead(true);
    Translator.setLine(decl, p1);
    Expression value;
    String literal = null;
    if (p2.car instanceof FString)
      {
        literal = p2.car.toString();
        Namespace namespace;
        if (literal.startsWith("class:"))
          {
            String cname = literal.substring(6);
            namespace
              = ClassNamespace.getInstance(literal, ClassType.make(cname));
            decl.setType(ClassType.make("gnu.kawa.lispexpr.ClassNamespace"));
          }
        else if (makeXML)
          {
            namespace = XmlNamespace.getInstance(name, literal);
            decl.setType(ClassType.make("gnu.kawa.lispexpr.XmlNamespace"));
          }
        else
          {
            namespace = Namespace.getInstance(literal);
            decl.setType(ClassType.make("gnu.mapping.Namespace"));
          }
        value = new QuoteExp(namespace);
	decl.setFlag(Declaration.TYPE_SPECIFIED);
       }
    else
      value = tr.rewrite_car (p2, false);
    decl.noteValue(value);
    forms.addElement(SetExp.makeDefinition(decl, value));
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return tr.syntaxError ("define-namespace is only allowed in a <body>");
  }

  public static final String XML_NAMESPACE_MAGIC = "&xml&";
}
