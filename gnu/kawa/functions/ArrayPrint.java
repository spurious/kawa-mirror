package gnu.kawa.functions;

import gnu.lists.*;
import gnu.kawa.io.CharArrayOutPort;

/** Pretty-print an array (in APL-style) */

public class ArrayPrint {
    private ArrayPrint() {
    }

    /** Print a given value 2-dimensionally. */
    public static String print(Object value, String format) {
        if (value instanceof Array) {
            StringBuilder sbuf = new StringBuilder();
            Array arr = (Array) value;
            int rank = arr.rank();
            int size = getSize(arr);
            int nfields = rank == 0 ? 1 : arr.getSize(rank-1);
            int[] fieldw = new int[nfields];
            String[][] cells = new String[size][];
            boolean[] rightAlign = new boolean[size];
            int icell = 0;
            String vectag = arr instanceof GeneralArray
                ? ((GeneralArray) arr).getTag()
                : "a";
            int headStart = sbuf.length();
            sbuf.append("#" + rank + (vectag==null ? "a" : vectag));
            int headEnd = sbuf.length();
            boolean dimsNeeded = false;
            for (int r = 0; r < rank; r++) {
                int low = arr.getLowBound(r);
                if (low != 0) {
                    sbuf.append('@');
                    sbuf.append(low);
                    dimsNeeded = true;
                }
                sbuf.append(':');
                sbuf.append(arr.getSize(r));
            }
            if (rank == 0) {
                String str = print(arr.getRowMajor(0), format);
                sbuf.append(' ');
                sbuf.append(str);
                return sbuf.toString();
            }
            if (size == 0) {
                sbuf.append(" ()");
                return sbuf.toString();
            }

            for (int i = 0; i < size; i++) {
                int col = i % nfields;
                Object element = arr.getRowMajor(i);
                String str = print(element, format);
                String[] lines = splitLines(str);
                rightAlign[icell] = element instanceof Number;
                cells[icell] = lines;
                icell++;
                fieldw[col] = columnWidth(lines, fieldw[col]);
            }
            int twidth = nfields+1;
            for (int i = 0; i < nfields; i++)
                twidth += fieldw[i];
            int slen = sbuf.length();
            if (twidth > slen)
                sbuf.insert(0, '\u2554');
            else if (twidth < slen && ! dimsNeeded) {
                sbuf.setLength(headEnd);
            }
            headEnd = sbuf.length();
            int headSize = headEnd-headStart;
            if (headSize >= twidth)
                sbuf.append('\n');
            else {
                putLine(0, fieldw, sbuf);
                sbuf.delete(headEnd, headEnd+headSize);
            }
            int groupSize = rank > 2 ? nfields * arr.getSize(rank-2) : size;
            int rows = getSizePrefix(arr);
            for (int ii = 0; ii < rows; ) {
                int i = ii * nfields;
                int nlines = 0;
                for (int k = 0; k < nfields; k++) {
                    
                    int nl = cells[i+k].length;
                    if (nl > nlines)
                        nlines = nl;
                }

                for (int j = 0; j < nlines; j++) {
                    sbuf.append('\u2551');
                    for (int k = 0; k < nfields; k++) {
                        String[] lines = cells[i+k];
                        String line = j >= lines.length ? "" : lines[j];
                        boolean padLeft = rightAlign[i+k];
                        int linelen = line.length();
                        int width = fieldw[k];
                        if (padLeft)
                            putSpaces(sbuf, width-linelen);
                        sbuf.append(line);
                        if (! padLeft)
                            putSpaces(sbuf, width-linelen);
                        sbuf.append(k + 1 < nfields ? '\u2502' : '\u2551');
                    }
                    sbuf.append('\n');
                }
                if (++ii == rows)
                    break;
                int hmode =
                    (rank > 2 && arr.getSize(rank-2) > 0
                     && (ii % arr.getSize(rank-2)) == 0)
                    ? 2
                    : 1;
                putLine(hmode, fieldw, sbuf);
            }
            putLine(3, fieldw, sbuf);
            return sbuf.toString();
        } else {
            if (format == null)
                format = "~a";
            return Format.formatToString(0, format, value);
        }
    }

    // hmode meanings: 0: top line; 1: regular; 2: double; 3: bottom
    private static void putLine(int hmode, int[] fieldw, StringBuilder sbuf) {
        int nfields = fieldw.length;
                sbuf.append("\u2554\u255f\u2560\u255a".charAt(hmode));
                for (int k = 0; k < nfields; ) {
                    putChars(sbuf, fieldw[k],
                             "\u2550\u2500\u2550\u2550".charAt(hmode));
                    if (++k >= nfields)
                        break;
                    sbuf.append("\u2564\u253c\u256a\u2567".charAt(hmode));
                }
                sbuf.append("\u2557\u2562\u2563\u255d".charAt(hmode));
                sbuf.append('\n');
    }

    static void putSpaces(StringBuilder sbuf, int n) {
        putChars(sbuf, n, ' ');
    }
    static void putChars(StringBuilder sbuf, int n, char ch) {
        while (--n >= 0)
            sbuf.append(ch);
    }

    static int columnWidth(CharSequence str) {
        return str.length(); // Over-simplified ...
    }

    static int columnWidth(String[] strs, int wmax) {
        for (int i = strs.length;  --i >= 0; ) {
            int w = columnWidth(strs[i]);
            if (w > wmax)
                wmax = w;
        }
        return wmax;
    }

    public static int getSize(Array arr) {
        // FIXME future: arr.getSize();
        int r = arr.rank();
        int sz = 1;
        while (--r >= 0)
            sz *= arr.getSize(r);
        return sz;
    }

    public static int getSizePrefix(Array arr) {
        // FIXME future: arr.getSize();
        int r = arr.rank()-1;
        int sz = 1;
        while (--r >= 0)
            sz *= arr.getSize(r);
        return sz;
    }

    static String[] splitLines(String str) {
        int len = str.length();
        int count = 0;
        for (int i = len; --i >= 0; ) {
            if (str.charAt(i) == '\n')
                count++;
        }
        if (len == 0 || str.charAt(len-1) != '\n')
            count++;
        String[] lines = new String[count];
        int j = 0;
        int prevStart = 0;
        for (int i = 0; ; i++) {
            if (i == len || str.charAt(i) == '\n') {
                lines[j++] = str.substring(prevStart, i);
                prevStart = i + 1;
                if (prevStart >= len)
                    break;
            }
        }
        return lines;
    }
}
