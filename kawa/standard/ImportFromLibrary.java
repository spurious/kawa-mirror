// Copyright (C) 2009 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ../../COPYING.

package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.mapping.*;
import java.util.*;

/** Implement R6RS import form.
 * This actually only implements simplified import;
 * we assumes it has been simplified by import macro defined in syntax.scm.
 */

public class ImportFromLibrary extends Syntax
{
  public static final ImportFromLibrary instance = new ImportFromLibrary();

  public String[] classPrefixPath = { "", "kawa.lib." };

  public boolean scanForDefinitions (Pair st, Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Procedure mapper = null;
    Object args = st.getCdr();
    if (! (args instanceof Pair))
      return false; // Should never happen.
    Pair pair = (Pair) args;
    Object libref = pair.getCar();
    if (LList.listLength(libref, false) <= 0)
      {
        tr.error('e', "expected <library reference> which must be a list");
        return false;
      }
    Object rest = pair.getCdr();
    if (rest instanceof Pair && ((Pair) rest).getCar() instanceof Procedure)
      mapper = (Procedure) ((Pair) rest).getCar();

    Object versionSpec = null;
    String sourcePath = null;
    StringBuffer sbuf = new StringBuffer();
    while (libref instanceof Pair)
      {
        pair = (Pair) libref;
        Object car = pair.getCar();
        Object cdr = pair.getCdr();
        if (car instanceof Pair)
          {
            if (versionSpec != null)
              {
                tr.error('e', "duplicate version reference - was "+versionSpec);
              }
            versionSpec = car;
            System.err.println("import version "+car);
          }
        else if (car instanceof String)
          {
            if (cdr instanceof Pair)
              tr.error('e', "source specifier must be last elemnt in library reference");
            sourcePath = (String) car;
          }
        else
          {
            if (sbuf.length() > 0)
              sbuf.append('.');
            sbuf.append(Compilation.mangleNameIfNeeded(car.toString()));
          }
        libref = cdr;
      }

    ModuleInfo minfo = null;
    if (sourcePath != null)
      {
        minfo = require.lookupModuleFromSourcePath(sourcePath, defs);
        if (minfo == null)
          {
            tr.error('e', "malformed URL: "+sourcePath);
            return false;
          }
      }

    int classPrefixPathLength = classPrefixPath.length;
    String lname = sbuf.toString();
    for (int i = 0;  i < classPrefixPathLength;  i++)
      {
        String tname = classPrefixPath[i] + lname;
        try
          {
            minfo = ModuleManager.getInstance().findWithClassName(tname);
          }
        catch (Exception ex)
          {
            continue;
          }
      }
    if (minfo == null)
      {
	tr.error('e', "unknown class "+lname);
	return false;
      }
    require.importDefinitions(null, minfo, mapper,
                              forms, defs, tr);
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return null;
  }
}
