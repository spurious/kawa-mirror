package kawa.standard;

import gnu.expr.*;
import gnu.lists.Pair;
import kawa.lang.*;

/** Create a scan expression from a sequence expression. */

public class Scan extends Syntax {
    public static final Scan scan = new Scan();
    static { scan.setName("scan"); }

    @Override
    public Expression rewrite (Object obj, final Translator tr) {
        if (Translator.listLength(obj) != 1)
            return tr.syntaxError("'scan' requires a single argument");
        Translator.ScanContext savedScanContext = tr.getScanContext();
        if (savedScanContext == null)
            tr.error('e', "'scan' not in a '...'- context");
        try {
            tr.setScanContext(null);
            Expression scanExp = tr.rewrite_car((Pair) obj, false);
            Declaration paramDecl =
                savedScanContext.getLambda().addParameter(null);
            savedScanContext.addSeqExpression(scanExp);
            return new ReferenceExp(paramDecl);
        } finally {
            tr.setScanContext(savedScanContext);
        }
    }
}
