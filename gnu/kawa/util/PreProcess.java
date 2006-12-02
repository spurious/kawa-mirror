// Copyright (c) 2005  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see COPYING.

package gnu.kawa.util;
import java.io.*;
import java.util.*;

/** Help class to pre-process Java source. */

public class PreProcess
{
  // JAVA5:
  // Hashtable<String,Boolean> keywords = new Hashtable<String,Boolean>();
  Hashtable keywords = new Hashtable();

  String filename;
  int lineno;

  void error(String msg)
  {
    System.err.println(filename+':'+lineno+": "+msg);
    System.exit(-1);
  }

  public void filter (String filename) throws Throwable
  {
    this.filename = filename;
    boolean changed = false;
    BufferedInputStream in
      = new BufferedInputStream(new FileInputStream(filename));
    
    byte[] buf = new byte[2000];
    int len = 0;;
    int lineStart = 0;
    int dataStart = -1;
    int cmdLine= 0;
    lineno = 1;
    // If non-negative, comment out at this indentation.
    int commentAt = -1;
    int curIndent = 0;
    int nesting = 0;
    // If currently skipping, the nesting level of the controlling
    // conditional.  Otherwise, 0.
    int skipNesting = 0;
    String cmd = null;
    int changedLine = 0; // -1: comment added or moved; +1 comment removed
    for (;;)
      {
	int c = in.read();
	if (c < 0)
	  break;
	if (len + 10 >= buf.length) // Allow a little extra for look-ahead.
	  {
	    byte[] nbuf = new byte[2 * len];
	    System.arraycopy(buf, 0, nbuf, 0, len);
	    buf = nbuf;
	  }
	if (c == '\n' && len > 0 && buf[len-1] == '\r')
	  {
	    buf[len++] = (byte) c;
	    continue;
	  }
	if (commentAt >= 0 && dataStart < 0 && changedLine <= 0
	    && c != '\r' && c != '\n'
	    && (commentAt == curIndent
		|| (c != ' ' && c != '\t')))
	  {
	    boolean doComment;
	    if (c == '/')
	      {
		// This is a little tricky.  We want to comment out regular
		// comments, because they might continue over multiple lines,
		// or because they might be documentation comments (which
		// we want to comment out so javadoc doesn't see them).
		// However, we don't want to comment out directives.
		in.mark(100);
		int d = in.read();
		if (d == '/')
		  doComment = false;
		else if (d == '*')
		  {
		    do { d = in.read(); } while (d == ' ' || d == '\t');
		    doComment = d != '#';
		  }
		else
		  doComment = true;
		in.reset();
	      }
	    else
	      doComment = true;
	    if (doComment)
	      {
		buf[len++] = '/';
		buf[len++] = '/';
		buf[len++] = ' ';
		changedLine = 1;
                changed = true;
	      }
	  }
	if (c != ' ' && c != '\t' && dataStart < 0)
	  {
	    // First non-space character.
	    dataStart = len;
	    if (nesting > 0 && commentAt != curIndent && c == '/')
	      {
		c = in.read();
		if (c < 0)
		  break;
		else if (c != '/')
		  buf[len++] = '/';
		else
		  {
		    c = in.read();
		    if (c < 0)
		      break;
		    changedLine = -1;
                    changed = true;
		    if (c == ' ')
                      {
                        c = in.read();
                        if (c == ' ' || c == '\t')
                          dataStart = -1;
                      }
		  }
	      }
	  }
	buf[len] = (byte) c;
	len++;
	if (c == '\r' || c == '\n')
	  {
	    int firstNonSpace = -1;
	    int lastNonSpace = 0;
	    for (int i = lineStart; i < len-1; i++)
	      {
		if (buf[i] != ' ' && buf[i] != '\t')
		  {
		    lastNonSpace = i;
		    if (firstNonSpace < 0)
		      firstNonSpace = i;
		  }
	      }
	    if (lastNonSpace - firstNonSpace >= 4
		&& buf[firstNonSpace] == '/'
		&& buf[firstNonSpace+1] == '*'
		&& buf[lastNonSpace-1] == '*'
		&& buf[lastNonSpace] == '/')
	      {
		firstNonSpace += 2;
		while (firstNonSpace < lastNonSpace
		       && buf[firstNonSpace] == ' ')
		  firstNonSpace++;
		lastNonSpace -= 2;
		while (lastNonSpace > firstNonSpace
		       && buf[lastNonSpace] == ' ')
		  lastNonSpace--;
		if (buf[firstNonSpace] == '#')
		  {
		    String cmnt = new String(buf, firstNonSpace,
					     lastNonSpace - firstNonSpace + 1,
					     "ISO-8859-1");
		    int sp = cmnt.indexOf(' ');
		    String rest;
		    Object binding;
		    cmdLine = lineno;
		    if (sp > 0)
		      {
			cmd = cmnt.substring(0, sp);
			rest = cmnt.substring(sp).trim();
			binding = keywords.get(rest);
		      }
		    else
		      {
			cmd = cmnt;
			rest = "";
			binding = null;
		      }
		    if ("#ifdef".equals(cmd) || "#ifndef".equals(cmd))
		      {
			if (binding == null)
			  {
			    System.err.println(filename+":"+lineno
					       +": warning - undefined keyword: "+rest);
			    binding = Boolean.FALSE;
			  } 
			nesting++;
                        if (skipNesting > 0)
                          commentAt = curIndent;
                        else if ((cmd.charAt(3) == 'n')
                                 != (binding == Boolean.FALSE))
                          {
                            commentAt = curIndent;
                            skipNesting = nesting;
                          }
		      }
		    else if ("#else".equals(cmd))
		      {
			if (nesting == 0)
			  error("unexpected "+cmd);
                        else if (nesting == skipNesting)
                          {
                            commentAt = -1;
                            skipNesting = 0;
                          }
                        else
                          {
                            commentAt = curIndent;
                            if (skipNesting == 0)
                              skipNesting = nesting;
                          }
		      }
		    else if ("#endif".equals(cmd))
		      {
			if (nesting == 0)
			  error("unexpected "+cmd);
                        if (nesting == skipNesting)
                          {
                            skipNesting = 0;
                            commentAt = -1;
                          }
                        else if (skipNesting > 0)
                          commentAt = curIndent;
			nesting--;
		      }
		    else
		      error("unknown command: "+cmnt);
		  }
	      }
	    lineStart = len;
	    dataStart = -1;
	    curIndent = 0;
	    lineno++;
	    changedLine = 0;
	  }
	else if (dataStart < 0)
	  curIndent = c == '\t' ? (curIndent + 8) & ~7 : curIndent + 1;
      }
    if (nesting != 0)
      {
	lineno = cmdLine;
	error("unterminated "+cmd);
      }
    if (changed)
      {
	FileOutputStream out = new FileOutputStream(filename);
	out.write(buf, 0, len);
	out.close();
	System.err.println("Pre-processed "+filename);
      }
  }

  public static void main (String[] args)
  {
    PreProcess pp = new PreProcess();
 
    pp.keywords.put("true", Boolean.TRUE);
    pp.keywords.put("false", Boolean.FALSE);

    for (int i = 0;  i < args.length;  i++)
      {
	String arg = args[i];
	if (arg.charAt(0) == '+')
	  pp.keywords.put(arg.substring(1), Boolean.TRUE);
	else if (arg.charAt(0) == '-')
	  {
	    int eq = arg.indexOf('=');
	    if (eq > 1)
	      {
		String keyword
		  = arg.substring(arg.charAt(1) == '-' ? 2 :1, eq);
		String value = arg.substring(eq+1);
		Boolean b = Boolean.FALSE;
		if (value.equalsIgnoreCase("true"))
		  b = Boolean.TRUE;
		else if (! value.equalsIgnoreCase("false"))
		  {
		    System.err.println("invalid value "+value+" for "+keyword);
		    System.exit(-1);
		  }
		pp.keywords.put(keyword, b);
	      }
	    else
	      pp.keywords.put(arg.substring(1), Boolean.FALSE);
	  }
	else
	  {
	    try
	      {
		pp.filter(arg);
	      }
	    catch (Throwable ex)
	      {
		System.err.println("caught "+ex);
		ex.printStackTrace();
		System.exit(-1);
	      }
	  }
      }
  }
}
