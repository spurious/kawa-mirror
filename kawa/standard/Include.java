package kawa.standard;

import gnu.expr.*;
import gnu.lists.*;
import gnu.kawa.io.*;
import gnu.kawa.lispexpr.LispReader;
import gnu.text.*;
import kawa.lang.*;
import java.io.*;
import java.nio.charset.*;

/** Syntax class for source-file inclusion. */

public class Include extends Syntax {
    boolean ignoreCase;
    boolean relative;

    public static final Object[] currentFirst = {
        Path.PATH_CURRENT, Path.PATH_RELATIVE
    };
    public static final Object[] relativeFirst = {
        Path.PATH_RELATIVE, Path.PATH_CURRENT
    };

    public static final Include include =
        new Include("include", false, false);
    public static final Include includeRelative =
        new Include("include-relative", true, false);
    public static final Include includeCi =
        new Include("include-ci", true, true);

    public Include(String name, boolean relative, boolean ignoreCase) {
        super(name);
        this.relative = relative;
        this.ignoreCase = ignoreCase;
    }

    @Override
    public void scanForm(Pair st, ScopeExp defs, Translator tr) {
        Object[] searchPath = relative ? relativeFirst : currentFirst;
        process(st.getCdr(), tr, defs, ignoreCase, searchPath);
    }

    @Override
    public Expression rewrite(Object obj, Translator tr) {
        Object[] searchPath = relative ? relativeFirst : currentFirst;
        return tr.rewrite_body(process(obj, tr, null, ignoreCase, searchPath));
    }

    public static LList process(Object rest, Translator tr, ScopeExp defs,
                                boolean ignoreCase, Object[] searchPath) {
        LList result = LList.Empty;
        Pair lastPair = null;
        while (rest instanceof Pair) {
            Pair pair = (Pair) rest;
            Object paircar = pair.getCar();
            Object savePos1 = tr.pushPositionOf(pair);
            if (! (paircar instanceof CharSequence)) {
                tr.error('e', "include parameters must be strings");
            }
            String fname = paircar.toString();
            Object[] found = Path.search(searchPath, fname, tr.getFileName());
            if (found == null) {
                tr.error('e', "cannot open file \""+fname+"\"");
                return result;
            } 
            InputStream istrm = (InputStream) found[0];
            Path path = (Path) found[1];
            BinaryInPort inp;
            try {
                inp = BinaryInPort.openHeuristicFile(istrm, path);
            } catch (Exception ex) {
                tr.error('e', "error reading file \""+path+"\": "+ex.getMessage());
                return result;
            }
            tr.popPositionOf(savePos1);
            LispReader reader = new LispReader(inp, tr.getMessages());
            if (ignoreCase)
                reader.setReadCase('D');
            Lexer saveLexer = tr.lexer;
            tr.lexer = reader;
            try {
                if (inp.getCharset() == null && saveLexer != null) {
                    InPort savePort = saveLexer.getPort();
                    if (savePort instanceof BinaryInPort) {
                        Charset saveCset =
                            ((BinaryInPort) savePort).getCharset();
                        if (saveCset != null)
                            inp.setDefaultCharset(saveCset);
                    }
                }
                for (;;) {
                    Object sexp;
                    try {
                        sexp = reader.readCommand();
                        if (sexp == Sequence.eofValue)
                            break;
                    } catch (Exception ex) {
                        tr.error('e', "error reading file \""+path+"\": "+ex.getMessage());
                        return result;
                    }
                    // FIXME do we need to handle Syntax context?
                    if (defs != null) {
                        // In scan context.
                        tr.scanForm(sexp, defs);
                    } else {
                        // In rewrite context - create list.
                        Pair npair = new Pair(sexp, LList.Empty);
                        if (lastPair == null)
                            result = npair;
                        else
                            lastPair.setCdrBackdoor(npair);
                        lastPair = npair;
                    }
                }
            } finally {
                tr.lexer = saveLexer;
            }
            rest = pair.getCdr();
        }
        if (rest != LList.Empty)
            tr.error('e', "improper list");
        return result;
    }
}
