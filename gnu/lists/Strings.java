// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** Various static utility methods for general strings (CharSeqs). */

public class Strings
{
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

  public static void printQuoted (
                                  /* #ifdef use:java.lang.CharSequence */
                                  CharSequence str,
                                  /* #else */
                                  // String str,
                                  /* #endif */
				  java.io.PrintWriter ps, int escapes)
  {
    int len = str.length();
    ps.print ('\"');
    for (int i = 0;  i < len; i++)
      {
	char ch = str.charAt(i);
	if ((ch == '\\' || ch == '\"'))
	  ps.print ('\\');
	else if (escapes > 0)
	  {
	    // These escapes are R6RS:
	    if (ch == '\n')
	     { ps.print("\\n"); continue; }
	    else if (ch == '\r')
	      { ps.print("\\r"); continue; }
	    else if (ch == '\t')
	      { ps.print("\\t"); continue; }
            else if (ch == '\007')
              { ps.print("\\a"); continue; }
            else if (ch == '\b')
              { ps.print("\\b"); continue; }
            else if (ch == '\013')
              { ps.print("\\v"); continue; }
            else if (ch == '\f')
              { ps.print("\\f"); continue; }
	  }
	ps.print (ch);
      }
    ps.print ('\"');
  }

}
