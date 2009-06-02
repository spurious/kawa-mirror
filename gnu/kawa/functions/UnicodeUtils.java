package gnu.kawa.functions;
import gnu.mapping.*;
import static java.lang.Character.*;
import java.text.*;

public class UnicodeUtils
{
  public static boolean isWhitespace (int ch)
  {
    // http://unicode.org/Public/UNIDATA/PropList.txt
    if (ch == ' ' || (ch >= 0x9 && ch <= 0xD))
      return true;
    if (ch < 0x85) // short-circuit optimization.
      return false;
    if (ch == 0x85 || ch == 0xA0 || ch == 0x1680 || ch == 0x180E)
      return true;
    if (ch < 0x2000 || ch > 0x3000)
      return false;
    return ch <= 0x200A || ch == 0x2028 || ch == 0x2029
      || ch == 0x202F || ch == 0x205F || ch == 0x3000;
  }

  public static String capitalize (String str)
  {
    /* #ifdef JAVA5 */
    StringBuilder sbuf = new StringBuilder();
    /* #else */
    // StringBuffer sbuf = new StringBuffer();
    /* #endif */
    BreakIterator wb = BreakIterator.getWordInstance();
    wb.setText(str);
    int start = wb.first();
    for (int end = wb.next();
         end != BreakIterator.DONE;
         start = end, end = wb.next())
      {
        boolean isWord = false;
        for (int p = start; p < end; p++)
          {
            if (Character.isLetter(str.codePointAt(p)))
              {
                isWord = true;
                break;
              }
          }
        if (! isWord)
          sbuf.append(str, start, end);
        else
          {
            char first = str.charAt(start);
            // Note this isn't quite right.  For example if a word
            // starts with "LATIN SMALL LIGATURE FL" then we want "Fl"
            // as two separate letters.
            char title = Character.toTitleCase(first);
            sbuf.append(title);
            sbuf.append(str.substring(start+1,end).toLowerCase());
          }
      }
    return sbuf.toString();
  }

  public static String foldCase (CharSequence str)
  {
    int len = str.length();
    if (len == 0)
      return "";
    /* #ifdef JAVA5 */
    StringBuilder sbuf = null;
    /* #else */
    // StringBuffer sbuf = null;
    /* #endif */
    int start = 0;
    for (int i = 0;  ;  i++)
      {
        int ch = i == len ? -1 : str.charAt(i);
        boolean sigma = ch == 0x3A3 || ch == 0x3C3 || ch == 0x3C2;
          if (ch < 0 || ch == 0x130 || ch == 0x131 || sigma)
          {
            if (sbuf == null && ch >= 0)
              {
                /* #ifdef JAVA5 */
                sbuf = new StringBuilder();
                /* #else */
                // sbuf = new StringBuffer();
                /* #endif */
              }
            if (i > start)
              {
                String converted = (str.subSequence(start, i).toString()
                                    .toUpperCase().toLowerCase());
                if (sbuf == null)
                  return converted;
                sbuf.append(converted);
              }
            if (ch < 0)
              break;
            if (sigma)
              ch = 0x3c3;
            sbuf.append((char) ch);
            start = i+1;
          }
      }
    return sbuf.toString();
  }

  public static Symbol generalCategory (int ch)
  {
    switch (Character.getType(ch))
      {
      case COMBINING_SPACING_MARK:
        return Mc;
      case CONNECTOR_PUNCTUATION:
        return Pc;
      case CONTROL:
        return Cc;
      case CURRENCY_SYMBOL:
        return Sc;
      case DASH_PUNCTUATION:
        return Pd;
      case DECIMAL_DIGIT_NUMBER:
        return Nd;
      case ENCLOSING_MARK:
        return Me;
      case END_PUNCTUATION:
        return Pe;
      case FINAL_QUOTE_PUNCTUATION:
        return Pf;
      case FORMAT:
        return Cf;
      case INITIAL_QUOTE_PUNCTUATION:
        return Pi;
      case LETTER_NUMBER:
        return Nl;
      case LINE_SEPARATOR:
        return Zl;
      case LOWERCASE_LETTER:
        return Ll;
      case MATH_SYMBOL:
        return Sm;
      case MODIFIER_LETTER:
        return Lm;
      case MODIFIER_SYMBOL:
        return Sk;
      case NON_SPACING_MARK:
        return Mn;
      case OTHER_LETTER:
        return Lo;
      case OTHER_NUMBER:
        return No;
      case OTHER_PUNCTUATION:
        return Po;
      case OTHER_SYMBOL:
        return So;
      case PARAGRAPH_SEPARATOR:
        return Zp;
      case PRIVATE_USE:
        return Co;
      case SPACE_SEPARATOR:
        return Zs;
      case START_PUNCTUATION:
        return Ps;
      case SURROGATE:
        return Cs;
      case TITLECASE_LETTER:
        return Lt;
      case UPPERCASE_LETTER:
        return Lu;
      default:
      case UNASSIGNED:
        return Cn;
      }
  }

  static final Symbol Mc;
  static final Symbol Pc;
  static final Symbol Cc;
  static final Symbol Sc;
  static final Symbol Pd;
  static final Symbol Nd;
  static final Symbol Me;
  static final Symbol Pe;
  static final Symbol Pf;
  static final Symbol Cf;
  static final Symbol Pi;
  static final Symbol Nl;
  static final Symbol Zl;
  static final Symbol Ll;
  static final Symbol Sm;
  static final Symbol Lm;
  static final Symbol Sk;
  static final Symbol Mn;
  static final Symbol Lo;
  static final Symbol No;
  static final Symbol Po;
  static final Symbol So;
  static final Symbol Zp;
  static final Symbol Co;
  static final Symbol Zs;
  static final Symbol Ps;
  static final Symbol Cs;
  static final Symbol Lt;
  static final Symbol Cn;
  static final Symbol Lu;

  static
  {
    Namespace empty = Namespace.EmptyNamespace;
    Mc = empty.getSymbol("Mc");
    Pc = empty.getSymbol("Pc");
    Cc = empty.getSymbol("Cc");
    Sc = empty.getSymbol("Sc");
    Pd = empty.getSymbol("Pd");
    Nd = empty.getSymbol("Nd");
    Me = empty.getSymbol("Me");
    Pe = empty.getSymbol("Pe");
    Pf = empty.getSymbol("Pf");
    Cf = empty.getSymbol("Cf");
    Pi = empty.getSymbol("Pi");
    Nl = empty.getSymbol("Nl");
    Zl = empty.getSymbol("Zl");
    Ll = empty.getSymbol("Ll");
    Sm = empty.getSymbol("Sm");
    Lm = empty.getSymbol("Lm");
    Sk = empty.getSymbol("Sk");
    Mn = empty.getSymbol("Mn");
    Lo = empty.getSymbol("Lo");
    No = empty.getSymbol("No");
    Po = empty.getSymbol("Po");
    So = empty.getSymbol("So");
    Zp = empty.getSymbol("Zp");
    Co = empty.getSymbol("Co");
    Zs = empty.getSymbol("Zs");
    Ps = empty.getSymbol("Ps");
    Cs = empty.getSymbol("Cs");
    Lt = empty.getSymbol("Lt");
    Cn = empty.getSymbol("Cn");
    Lu = empty.getSymbol("Lu");
  }
}
