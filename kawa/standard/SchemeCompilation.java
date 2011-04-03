package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.math.*;
import gnu.text.*;
import gnu.kawa.functions.*;
import gnu.kawa.lispexpr.*;
import gnu.kawa.reflect.MakeAnnotation;
import gnu.kawa.xml.XmlNamespace;
import java.util.*;

public class SchemeCompilation extends Translator
{
  public SchemeCompilation (Language language, SourceMessages messages, NameLookup lexical)
  {
    super(language, messages, lexical);
  }

  public static final Declaration applyFieldDecl =
    Declaration.getDeclarationFromStatic("kawa.standard.Scheme", "applyToArgs");

  public ApplyExp makeApply (Expression func, Expression[] args)
  {
    Expression[] exps = new Expression[args.length+1];
    exps[0] = func;
    System.arraycopy(args, 0, exps, 1, args.length);
    return new ApplyExp(new ReferenceExp(applyFieldDecl), exps);
  }

  /** Should the values of body/block be appended as multiple values?
   * Otherwise, just return the result of the final expression.
   */
  public boolean appendBodyValues ()
  {
    return ((Scheme) getLanguage()).appendBodyValues();
  }

  public static final kawa.repl repl;

  public static final Lambda lambda = new kawa.lang.Lambda();

  static
  {
    repl = new kawa.repl(Scheme.instance);
    lambda.setKeywords(Special.optional, Special.rest, Special.key); 
  }

  /** If a symbol is lexically unbound, look for a default binding.
   * If the symbol is the name of an existing Java type/class,
   * return that Class.
   * Handles both with and without (semi-deprecated) angle-brackets:
   * {@code <java.lang.Integer>} and {@code java.lang.Integer}.
   * Also handles arrays, such as {@code java.lang.String[]}.
   */
  public Expression checkDefaultBinding (Symbol symbol, Translator tr)
  {
    Namespace namespace = symbol.getNamespace();
    String local = symbol.getLocalPart();
    if (namespace instanceof XmlNamespace)
      return makeQuoteExp(((XmlNamespace) namespace).get(local));
    if (namespace.getName() == Scheme.unitNamespace.getName())
      {
        Object val = Unit.lookup(local);
        if (val != null)
          return makeQuoteExp(val);
      }
    String name = symbol.toString();
    int len = name.length();
    if (len == 0)
      return null;
    if (len > 1 && name.charAt(len-1) == '?')
      {
        int llen = local.length();
        if (llen > 1)
          {
            String tlocal = local.substring(0, llen-1).intern();
            Symbol tsymbol = namespace.getSymbol(tlocal);
            Expression texp = tr.rewrite(tsymbol, false);
            if (texp instanceof ReferenceExp)
              {
                Declaration decl = ((ReferenceExp) texp).getBinding();
                if (decl == null || decl.getFlag(Declaration.IS_UNKNOWN))
                  texp = null;
              }
            else if (! (texp instanceof QuoteExp))
              texp = null;
            if (texp != null)
              {
                LambdaExp lexp = new LambdaExp(1);
                lexp.setSymbol(symbol);
                Declaration param = lexp.addDeclaration((Object) null);
                lexp.body = new ApplyExp(Scheme.instanceOf,
                                         new Expression[] {
                                           new ReferenceExp(param), texp, });
                return lexp;
              }
          }
      }
    char ch0 = name.charAt(0);

    if (ch0 == '@')
      {
        String rest = name.substring(1);
        Expression classRef = tr.rewrite(Symbol.valueOf(rest));
        return MakeAnnotation.makeAnnotationMaker(classRef);
      }

    // Look for quantities.
    if (ch0 == '-' || ch0 == '+' || Character.digit(ch0, 10) >= 0)
      {
        // 1: initial + or -1 seen.
        // 2: digits seen
        // 3: '.' seen
        // 4: fraction seen
        // 5: [eE][=+]?[0-9]+ seen
        int state = 0;
        int i = 0;
        for (;  i < len;  i++)
          {
            char ch = name.charAt(i);
            if (Character.digit(ch, 10) >= 0)
              state = state < 3 ? 2 : state < 5 ? 4 : 5;
            else if ((ch == '+' || ch == '-') && state == 0)
              state = 1;
            else if (ch == '.' && state < 3)
              state = 3;
            else if ((ch == 'e' || ch == 'E') && (state == 2 || state == 4)
                     && i + 1 < len)
              {
                int j = i+1;
                char next = name.charAt(j);
                if ((next == '-' || next == '+') && ++j < len)
                  next = name.charAt(j);
                if (Character.digit(next, 10) < 0)
                  break;
                state = 5;
                i = j+1;
              }
            else
              break;
          }
      tryQuantity:
        if (i < len && state > 1)
          {
            DFloNum num = new DFloNum(name.substring(0,i));
            boolean div = false;
            Vector vec = new Vector();
            for (; i < len ;)
              {
                char ch = name.charAt(i++);
                if (ch == '*')
                  {
                    if (i == len) break tryQuantity;
                    ch = name.charAt(i++);
                  }
                else if (ch == '/')
                  {
                    if (i == len || div) break tryQuantity; 
                    div = true;
                    ch = name.charAt(i++);
                  }
                int unitStart = i-1;
                int unitEnd;
                for (;;)
                  {
                    if (! Character.isLetter(ch))
                      {
                        unitEnd = i - 1;
                        if (unitEnd == unitStart)
                          break tryQuantity;
                        break;
                      }
                    if (i == len)
                      {
                        unitEnd = i;
                        ch = '1';
                        break;
                      }
                    ch = name.charAt(i++);
                  }
                vec.addElement(name.substring(unitStart, unitEnd));
                boolean expRequired = false;
                if (ch == '^')
                  {
                    expRequired = true;
                    if (i == len) break tryQuantity; 
                    ch = name.charAt(i++);
                  }
                boolean neg = div;
                if (ch == '+')
                  {
                    expRequired = true;
                    if (i == len) break tryQuantity; 
                    ch = name.charAt(i++);
                  }
                else if (ch == '-')
                  {
                    expRequired = true;
                    if (i == len) break tryQuantity; 
                    ch = name.charAt(i++);
                    neg = ! neg;
                  }
                int nexp = 0;
                int exp = 0;
                for (;;)
                  {
                    int dig = Character.digit(ch, 10);
                    if (dig <= 0)
                      {
                        i--;
                        break;
                      }
                    exp = 10 * exp + dig;
                    nexp++;
                    if (i == len)
                      break;
                    ch = name.charAt(i++);
                  }
                if (nexp == 0)
                  {
                    exp = 1;
                    if (expRequired)
                      break tryQuantity;
                  }
                if (neg)
                  exp = - exp;
                vec.addElement(IntNum.make(exp));
              }
            if (i == len)
              {
                int nunits = vec.size() >> 1;
                Expression[] units = new Expression[nunits];
                for (i = 0;  i < nunits;  i++)
                  {
                    String uname = (String) vec.elementAt(2*i);
                    Symbol usym = Scheme.unitNamespace.getSymbol(uname.intern());
                    Expression uref = tr.rewrite(usym);
                    IntNum uexp = (IntNum) vec.elementAt(2*i+1);
                    if (uexp.longValue() != 1)
                      uref = new ApplyExp(expt.expt,
                                          new Expression[] { uref , makeQuoteExp(uexp) });
                    units[i] = uref;
                  }
                Expression unit;
                if (nunits == 1)
                  unit = units[0];
                else
                  unit = new ApplyExp(MultiplyOp.$St, units);
                return new ApplyExp(MultiplyOp.$St,
                                    new Expression[] { makeQuoteExp(num),
                                                       unit });
              }
          }
      }

    boolean sawAngle;
    if (len > 2 && ch0 == '<' && name.charAt(len-1) == '>')
      {
        name = name.substring(1, len-1);
        len -= 2;
        sawAngle = true;
      }
    else
      sawAngle = false;
    int rank = 0;
    while (len > 2 && name.charAt(len-2) == '[' && name.charAt(len-1) == ']')
      {
        len -= 2;
        rank++;
      }

    String cname = name;
    if (rank != 0)
      cname = name.substring(0, len);
    try
      { 
        Class clas;
        Type type = Scheme.getNamedType(cname);
        if (rank > 0 && (! sawAngle || type == null))
          {
            Symbol tsymbol = namespace.getSymbol(cname.intern());
            Expression texp = tr.rewrite(tsymbol, false);
            texp = InlineCalls.inlineCalls(texp, tr);
            if (! (texp instanceof ErrorExp))
              type = tr.getLanguage().getTypeFor(texp);
          }
        if (type != null)
          {
            // Somewhat inconsistent: Types named by getNamedType are Type,
            // while standard type/classes are Class.  FIXME.
            while (--rank >= 0)
              type = gnu.bytecode.ArrayType.make(type);
            return makeQuoteExp(type);
          }
        else
          {
            type = Type.lookupType(cname);
            if (type instanceof gnu.bytecode.PrimType)
              clas = type.getReflectClass();
            else
              {
                if (cname.indexOf('.') < 0)
                  cname = (tr.classPrefix
                           + Compilation.mangleNameIfNeeded(cname));
                clas = ClassType.getContextClass(cname);
              }
          }
        if (clas != null)
          {
            if (rank > 0)
              {
                type = Type.make(clas);
                while (--rank >= 0)
                  type = gnu.bytecode.ArrayType.make(type);
                clas = type.getReflectClass();
              }
            return makeQuoteExp(clas);
          }
      }
    catch (ClassNotFoundException ex)
      {
        Package pack = gnu.bytecode.ArrayClassLoader.getContextPackage(name);
        if (pack != null)
          return makeQuoteExp(pack);
      }
    catch (Throwable ex)
      {
      }
    return null;
  }


}
