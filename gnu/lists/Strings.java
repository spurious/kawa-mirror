// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

import gnu.text.Char;
import java.io.IOException;

/** Various static utility methods for general strings (CharSeqs). */

public class Strings
{
    public static int characterAt(CharSequence cseq, int index) {
        return characterAt(cseq, 0, cseq.length(), index);
    }
    public static int characterAt(CharSequence cseq, int start, int end,
                                  int index) {
        if (index < start || index >= end)
            throw new IndexOutOfBoundsException();
        char ch1 = cseq.charAt(index);
        if (ch1 >= 0xD800 && ch1 <= 0xDBFF) {
            if (index + 1 < end) {
                char ch2 = cseq.charAt(index+1);
                if (ch2 >= 0xDC00 && ch2 <= 0xDFFF)
                    return ((ch1 - 0xD800) << 10) + (ch2 - 0xDC00) + 0x10000;
            }
        } else if (ch1 >= 0xDC00 && ch1 <= 0xDFFF) {
            if (index > start) {
                char ch0 = cseq.charAt(index-1);
                if (ch0 >= 0xD800 && ch0 <= 0xDBFF)
                    return Char.IGNORABLE_CHAR;
            }
        }
        return ch1;
    }

    public static int sizeInCodePoints(CharSequence str) {
        int len = str.length();
        int nsurr = 0;
        for (int i = 0; i < len;  ) {
            char ch = str.charAt(i++);
            if (ch >= 0xD800 && ch <= 0xDBFF && i < len) {
                int next = str.charAt(i);
                if (next >= 0xDC00 && next <= 0xDFFF) {
                    i++;
                    nsurr++;
                }
            }
        }
        return len-nsurr;
    }

  /** Change every character to be uppercase. */
  public static void makeUpperCase(CharSeq str)
  {
    for (int i = str.length();  --i >= 0; )
      str.setCharAt(i, Character.toUpperCase(str.charAt(i)));
  }

  /** Change every character to be lowercase. */
  public static void makeLowerCase(CharSeq str)
  {
    for (int i = str.length();  --i >= 0; )
      str.setCharAt(i, Character.toLowerCase(str.charAt(i)));
  }

  /** Capitalize this string.
   * Change first character of each word to titlecase,
   * and change the other characters to lowercase. */
  public static void makeCapitalize(CharSeq str)
  {
    char prev = ' ';
    int len = str.length();
    for (int i = 0;  i < len;  i++)
      {
	char ch = str.charAt(i);
	if (! Character.isLetterOrDigit(prev))
	  ch = Character.toTitleCase(ch); 
        else 
          ch = Character.toLowerCase(ch);
	str.setCharAt(i, ch);
	prev = ch;
      }
  }

    /** Print a string with quotes and escapes.
     * @param escapes The value 0 means only escape '"' and '\\';
     *   the value 1 means escape standard escape characters like '\\b';
     *   the value 2 means escape all non-asci or control characters.
     */
    public static void printQuoted(CharSequence str,
                                   Appendable ps, int escapes) {
        int len = str.length();
        try {
            ps.append('\"');
            for (int i = 0;  i < len; i++) {
                char ch = str.charAt(i);
                if ((ch == '\\' || ch == '\"'))
                    ps.append('\\');
                else if (escapes > 0) {
                    // These escapes are R6RS:
                    if (ch == '\n')
                    { ps.append("\\n"); continue; }
                    else if (ch == '\r')
                    { ps.append("\\r"); continue; }
                    else if (ch == '\t')
                    { ps.append("\\t"); continue; }
                    else if (ch == '\007')
                    { ps.append("\\a"); continue; }
                    else if (ch == '\b')
                    { ps.append("\\b"); continue; }
                    else if (ch == '\013')
                    { ps.append("\\v"); continue; }
                    else if (ch == '\f')
                    { ps.append("\\f"); continue; }
                    else if (escapes > 1 && (ch < ' ' || ch >= 127))
                    {
                        ps.append("\\x");
                        ps.append(Integer.toHexString(ch));
                        ps.append(';');
                        continue;
                    }
                }
                ps.append(ch);
            }
            ps.append('\"');
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    public static void copyInto(CharSequence src, int start, int end,
                                CharSeq dst, int at) {
        int dstLen = dst.length();
        int srcLen = src.length();
        if (at < 0 || at > dstLen || start < 0 || end > srcLen || end < start
            || dstLen - at < end - start)
            throw new StringIndexOutOfBoundsException();
        if (at < start) {
            int i = at;
            int j = start;
            for (; j < end; i++, j++) {
                dst.setCharAt(i, src.charAt(j));
            }
        }
        else {
            int i = at + end - start;
            int j = end;
            while (--j >= start) {
                dst.setCharAt(--i, src.charAt(j));
            }
        }
    }

    public static CharSequence indirectIndexed(CharSequence base,
                                               IntSequence indexes) {
        if (base instanceof FString) {
            return (FString) ((FString) base).select(indexes);
        }
        if (indexes instanceof Range.IntRange) {
            Range.IntRange range = (Range.IntRange) indexes;
            if (range.getStepInt() == 1) {
                int start = range.getStartInt();
                int bsize = base.length();
                if (start < 0 || start > bsize)
                    throw new IndexOutOfBoundsException();
                if (range.isUnbounded())
                    return SubCharSeq.valueOf(base, start, bsize);
                int size = range.size();
                if (start+size < 0 || start+size > bsize)
                    throw new IndexOutOfBoundsException();
                return SubCharSeq.valueOf(base, start, start+size);
            }
        }
        int len = indexes.size();
        StringBuilder sbuf = new StringBuilder(len);
        for (int i = 0; i < len; i++)
            sbuf.append(base.charAt(indexes.intAt(i)));
        return sbuf.toString();
    }

    public static String toUtf8(byte[] bytes, int start, int length) {
        /* #ifdef JAVA7 */  
        return new String(bytes, start, length, java.nio.charset.StandardCharsets.UTF_8);
        /* #else */
        // try {
        //   return new String(bytes, start, length, "UTF-8");
        // } catch (UnsupportedEncodingException ex) {
        //     throw new RuntimeException(ex);
        // }
        /* #endif */
    }
}
