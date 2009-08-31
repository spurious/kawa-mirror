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

  private static final String BUILTIN = "";
  private static final String MISSING = null;

  static final String[][] SRFI97Map = {
    { "1", "lists", "gnu.kawa.slib.srfi1" },
    { "2", "and-let*", "gnu.kawa.slib.srfi2" },
    { "5", "let", MISSING },
    { "6", "basic-string-ports", BUILTIN },
    { "8", "receive", BUILTIN },
    { "9", "records", BUILTIN },
    { "11", "let-values", BUILTIN },
    { "13", "strings", "gnu.kawa.slib.srfi13" },
    { "14", "char-sets", MISSING },
    { "16", "case-lambda", BUILTIN },
    { "17", "generalized-set!", BUILTIN },
    { "18", "multithreading", MISSING },
    { "19", "time", MISSING },
    { "21", "real-time-multithreading", MISSING },
    { "23", "error", BUILTIN },
    { "25", "multi-dimensional-arrays", BUILTIN },
    { "26", "cut", BUILTIN },
    { "27", "random-bits", MISSING },
    { "28", "basic-format-strings", BUILTIN },
    { "29", "localization", MISSING },
    { "31", "rec", MISSING },
    { "38", "with-shared-structure", MISSING },
    { "39", "parameters", BUILTIN },
    { "41", "streams", MISSING },
    { "42", "eager-comprehensions", MISSING },
    { "43", "vectors", MISSING },
    { "44", "collections", MISSING },
    { "45", "lazy", MISSING },
    { "46", "syntax-rules", MISSING },
    { "47", "arrays", MISSING },
    { "48", "intermediate-format-strings", MISSING },
    { "51", "rest-values", MISSING },
    { "54", "cat", MISSING },
    { "57", "records", MISSING },
    { "59", "vicinities", MISSING },
    { "60", "integer-bits", MISSING },
    { "61", "cond", MISSING },
    { "63", "arrays", MISSING },
    { "64", "testing", "gnu.kawa.slib.testing" },
    { "66", "octet-vectors", MISSING },
    { "67", "compare-procedures", MISSING },
    { "69", "basic-hash-tables", "gnu.kawa.slib.srfi69" },
    { "71", "let", MISSING },
    { "74", "blobs", MISSING },
    { "78", "lightweight-testing", MISSING },
    { "86", "mu-and-nu", MISSING },
    { "87", "case", MISSING },
    { "95", "sorting-and-merging", "kawa.lib.srfi95" }
  };

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
    String lname = sbuf.toString();

    if (lname.startsWith("srfi."))
      {
        String demangled = Compilation.demangleName(lname.substring(5));
        int dot = demangled.indexOf('.');
        String srfiName;
        if (dot < 0)
          {
            srfiName = null;
            dot = demangled.length();
          }
        else
          srfiName = demangled.substring(dot+1);
        String srfiNumber = null;
        if (dot >= 2 || demangled.charAt(0) == ':')
          {
            for (int i = 1;  ;  i++)
              {
                if (i == dot)
                  {
                    srfiNumber = demangled.substring(1, dot);
                    break;
                  }
                if (Character.digit(demangled.charAt(i), 10) < 0)
                  break;
              }
          }
        if (srfiNumber == null)
          {
            tr.error('e', "SRFI library reference must have the form: (srfi :NNN [name])");
            return false;
          }
        int srfiIndex = SRFI97Map.length;
        for (;;)
          {
            if (--srfiIndex < 0)
              {
                tr.error('e', "unknown SRFI number '"+srfiNumber+"' in SRFI library reference");
                return false;
              }
            if (SRFI97Map[srfiIndex][0].equals(srfiNumber))
              break;
          }
        String srfiNameExpected = SRFI97Map[srfiIndex][1];
        String srfiClass = SRFI97Map[srfiIndex][2];
        if (srfiName != null && ! srfiName.equals(srfiNameExpected))
          {
            tr.error('w', "the name of SRFI "+srfiNumber+" should be '"+srfiNameExpected+'\'');
          }

        if (srfiClass == BUILTIN)
          return true; // Nothing to do.
        else if (srfiClass == MISSING)
          {
            tr.error('e', "sorry - Kawa does not support SRFI "+srfiNumber+" ("+srfiNameExpected+')');
            return false;
          }
        else
          lname = srfiClass;
      }

    int classPrefixPathLength = classPrefixPath.length;
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
