// Copyright (c) 2005, 2009, 2010, 2015  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see COPYING.

package gnu.kawa.util;
import java.io.*;
import java.util.*;

/** Helper class to pre-process Java source. */

public class PreProcess
{
    Hashtable<String,Boolean> keywords = new Hashtable<String,Boolean>();
    @SuppressWarnings("unchecked")
    ArrayList<String>[] substitutions =
        (ArrayList<String>[]) new ArrayList[128];
    int maxkey;

    String filename;
    int lineno;
    String outFilename;

    static final String JAVA4_FEATURES = "+JAVA2 +use:java.util.IdentityHashMap +use:java.lang.CharSequence +use:java.lang.Throwable.getCause +use:java.net.URI +use:java.util.regex +SAX2 +use:java.nio";
    static final String NO_JAVA4_FEATURES = "-JAVA5 -use:java.util.IdentityHashMap -use:java.lang.CharSequence -use:java.lang.Throwable.getCause -use:java.net.URI -use:java.util.regex -use:org.w3c.dom.Node -JAXP-1.3 -use:javax.xml.transform -JAVA5 -JAVA6 -JAVA6COMPAT5 -JAXP-QName -use:java.text.Normalizer -use:javax.lang.model -SAX2 -use:java.nio -Android";
    static final String JAVA5_FEATURES = "+JAVA5 "+JAVA4_FEATURES+" +use:org.w3c.dom.Node +use:javax.xml.transform +JAXP-1.3 -JAXP-QName";
    static final String NO_JAVA6_FEATURES = "-JAVA6 -JAVA7 -JAVA8 -JAVA9 -use:java.lang.invoke -use:java.text.Normalizer -use:javax.lang.model";

    static String[] version_features = {
        "java1", "-JAVA2 "+NO_JAVA4_FEATURES+" "+NO_JAVA6_FEATURES,
        "java2", "+JAVA2 "+NO_JAVA4_FEATURES+" "+NO_JAVA6_FEATURES,
        // We don't use Node for java4 because there are inconsistencies between
        // the version of DOM used in Java4 and the one in Java 5 (and GCJ).
        "java4", "-JAVA5 "+JAVA4_FEATURES+" -use:org.w3c.dom.Node -JAXP-1.3 -use:javax.xml.transform -JAXP-QName -JAVA6COMPAT5 -Android "+NO_JAVA6_FEATURES,
        "java4x", "-JAVA5 "+JAVA4_FEATURES+" +use:org.w3c.dom.Node +JAXP-1.3 +use:javax.xml.transform -JAXP-QName -JAVA6COMPAT5 -Android "+NO_JAVA6_FEATURES,
        "java5", JAVA5_FEATURES+" -JAVA6COMPAT5 -Android "+NO_JAVA6_FEATURES,
        "java6compat5", JAVA5_FEATURES+" -JAVA6 -JAVA7 -JAVA8 -JAVA9 +JAVA6COMPAT5 +use:java.text.Normalizer -use:javax.lang.model -use:java.lang.invoke -Android",
        "java6", JAVA5_FEATURES+" +JAVA6 -JAVA7 -JAVA8 -JAVA9 -JAVA6COMPAT5 +use:java.text.Normalizer +use:javax.lang.model -use:java.lang.invoke -Android",
        "java7", JAVA5_FEATURES+" +JAVA6 +JAVA7 -JAVA8 -JAVA9 -JAVA6COMPAT5 +use:java.text.Normalizer +use:javax.lang.model +use:java.lang.invoke -Android",
        "java8", JAVA5_FEATURES+" +JAVA6 +JAVA7 +JAVA8 -JAVA9 -JAVA6COMPAT5 +use:java.text.Normalizer +use:javax.lang.model +use:java.lang.invoke -Android",
        "java9", JAVA5_FEATURES+" +JAVA6 +JAVA7 +JAVA8 +JAVA9 -JAVA6COMPAT5 +use:java.text.Normalizer +use:javax.lang.model +use:java.lang.invoke -Android",
        "android", "+JAVA5 "+JAVA4_FEATURES+" +use:org.w3c.dom.Node +JAXP-1.3 -JAXP-QName -use:javax.xml.transform -JAVA6 -JAVA6COMPAT5 +Android "+NO_JAVA6_FEATURES,

        "OBJECT",
        "/+OBJECT/$DESC$=objects"
        +"/$TAG$=F/$ptype$=Object/$BTYPE$=Object"
        +"/$CNAME$=FVector<E>/$SUPER$=SimpleVector<E>/$KIND$=OBJECT"
        +"/$RETURN_IF_UNEQUAL$(v1,v2)={int d = ((Comparable) v1).compareTo((Comparable) v2); if (d != 0)  return d; }"
        +"/$WRITE(i,out)=out.writeObject(get(i))/$ZERO$=null",
        "BIT",
        "/+BIT/$DESC$=Boolean values"
        +"/$TAG$=Bit/$tag$=b/$ptype$=boolean/$BTYPE$=Boolean"
        +"/$CNAME$=BitVector/$SUPER$=SimpleVector<Boolean>/$KIND$=BOOLEAN"
        +"/$RETURN_IF_UNEQUAL$(v1,v2)=return v1 && ! v2 ? 1 : -1"
        +"/$WRITE(i,out)=out.writeBoolean(getBoolean(i))/$ZERO$=false",
        "BYTE",
        "/+BYTE/$DESC$=signed or unsigned 8-bit integers (bytes)"
        +"/$TAG$=Byte/$ptype$=byte/$BTYPE$=E"
        +"/$CNAME$=ByteVector<E>/$SUPER$=PrimIntegerVector<E>/@Abstract=abstract",
        "SHORT",
        "/+SHORT/$DESC$=signed or unsigned 16-bit integers (shorts)"
        +"/$TAG$=Short/$ptype$=short/$BTYPE$=E"
        +"/$CNAME$=ShortVector<E>/$SUPER$=PrimIntegerVector<E>/@Abstract=abstract",
        "INT",
        "/+INT/$DESC$=signed or unsigned 32-bit integers (ints)"
        +"/$TAG$=Int/$ptype$=int/$BTYPE$=E"
        +"/$CNAME$=IntVector<E>/$SUPER$=PrimIntegerVector<E>/@Abstract=abstract",
        "LONG",
        "/+LONG/$DESC$=signed or unsigned 64-bit integers (longs)"
        +"/$TAG$=Long/$ptype$=long/$BTYPE$=E"
        +"/$CNAME$=LongVector<E>/$SUPER$=PrimIntegerVector<E>/@Abstract=abstract",
        "F32",
        "/+F32/$DESC$=32-bit floats"
        +"/$TAG$=F32/$ptype$=float/$BTYPE$=Float"
        +"/$CNAME$=F32Vector/$SUPER$=SimpleVector<Float>/$KIND$=FLOAT"
        +"/$WRITE(i,out)=out.writeFloat(getFloat(i))",
        "F64",
        "/+F64/$DESC$=64-bit doubles"
        +"/$TAG$=F64/$ptype$=double/$BTYPE$=Double"
        +"/$CNAME$=F64Vector/$SUPER$=SimpleVector<Double>/$KIND$=DOUBLE"
        +"/$WRITE(i,out)=out.writeDouble(getDouble(i))",
        "S8",
        "/+S8/$DESC$=signed 8-bit integers (bytes)"
        +"/$TAG$=S8/$ptype$=byte/$BTYPE$=Byte/$KIND$=INT_S8"
        +"/$CNAME$=S8Vector/$SUPER$=ByteVector<Byte>",
        "S16",
        "/+S16/$DESC$=signed 16-bit integers (shorts)"
        +"/$TAG$=S16/$ptype$=short/$BTYPE$=Short/$KIND$=INT_S16"
        +"/$CNAME$=S16Vector/$SUPER$=ShortVector<Short>",
        "S32",
        "/+S32/$DESC$=signed 32-bit integers (ints)"
        +"/$TAG$=S32/$ptype$=int/$BTYPE$=Integer/$KIND$=INT_S32"
        +"/$CNAME$=S32Vector/$SUPER$=IntVector<Integer>",
        "S64",
        "/+S64/$DESC$=signed 64-bit integers (longs)"
        +"/$TAG$=S64/$ptype$=long/$BTYPE$=Long/$KIND$=INT_S64"
        +"/$CNAME$=S64Vector/$SUPER$=LongVector<Long>"
        +"/$WRITE(i,out)=out.writeLong(getLong(i))",
        "U8",
        "/+U8/$DESC$=unsigned 8-bit integers (bytes)"
        +"/$TAG$=U8/$ptype$=byte/$BTYPE$=UByte/$KIND$=INT_U8"
        +"/$CNAME$=U8Vector/$SUPER$=ByteVector<UByte>/$MASK$= & 0xff",
        "U16",
        "/+U16/$DESC$=unsigned 16-bit integers (shorts)"
        +"/$TAG$=U16/$ptype$=short/$BTYPE$=UShort/$KIND$=INT_U16"
        +"/$CNAME$=U16Vector/$SUPER$=ShortVector<UShort>/$MASK$= & 0xffff",
        "U32",
        "/+U32/$DESC$=unsigned 32-bit integers (ints)"
        +"/$TAG$=U32/$ptype$=int/$BTYPE$=UInt/$KIND$=INT_U32"
        +"/$CNAME$=U32Vector/$SUPER$=IntVector<UInt>/$MASK$= & 0xffffffffL"
        +"/$WRITE(i,out)=Sequences.writeUInt(getInt(i), out)",
        "U64",
        "/+U64/$DESC$=unsigned 64-bit integers (longs)"
        +"/$TAG$=U64/$ptype$=long/$BTYPE$=ULong/$KIND$=INT_U64"
        +"/$CNAME$=U64Vector/$SUPER$=LongVector<ULong>"
        +"/$WRITE(i,out)=Sequences.writeULong(getLong(i), out)"
        +"/$RETURN_IF_UNEQUAL(v1,v2)=return (v1^0x8000000000000000L) > (v2^0x8000000000000000L) ? 1 : -1",
    };

    void error(String msg) {
        System.err.println(filename+':'+lineno+": "+msg);
        System.exit(-1);
    }

    byte[] resultBuffer;
    int resultLength;

    public void filter(String filename) throws Throwable {
        boolean changed =
            filter(filename,
                   new BufferedInputStream(new FileInputStream(filename)));
        String outname = outFilename == null ? filename : outFilename;
        boolean overwrite = filename.equals(outname);
        if (changed || ! overwrite) {
            FileOutputStream out = new FileOutputStream(outname);
            out.write(resultBuffer, 0, resultLength);
            out.close();
            System.err.println("Pre-processed "+filename
                               +(overwrite ? "" : (" to "+outname)));
        }
    }

    public boolean filter(String filename, BufferedInputStream in)
            throws Throwable {
        this.filename = filename;
        boolean changed = false;
    
        byte[] buf = new byte[2000];
        int len = 0;
        int lineStart = 0;
        int dataStart = -1;
        int cmdLine = 0;
        boolean removeCommented = outFilename != null
            && ! filename.equals(outFilename);
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
        boolean skipLine = false;
        for (;;) {
            int c = in.read();
            if (c < 0)
                break;
            // Allow a little extra for look-ahead and substitution.
            int needed = len + maxkey + 10;
            int buflen = buf.length;
            if (needed >= buflen) {
                byte[] nbuf = new byte[needed + (buflen >> 2)];
                System.arraycopy(buf, 0, nbuf, 0, len);
                buf = nbuf;
            }
            if (commentAt >= 0 && dataStart < 0 && changedLine <= 0
                && c != '\r' && c != '\n'
                && (commentAt == curIndent
                    || (c != ' ' && c != '\t'))) {
                boolean doComment;
                if (c == '/') {
                    // This is a little tricky.  We want to comment out regular
                    // comments, because they might continue over multiple lines
                    // or because they might be documentation comments (which
                    // we want to comment out so javadoc doesn't see them).
                    // However, we don't want to comment out directives.
                    in.mark(100);
                    int d = in.read();
                    if (d == '/')
                        doComment = false;
                    else if (d == '*') {
                        do { d = in.read(); } while (d == ' ' || d == '\t');
                        doComment = d != '#';
                    }
                    else
                        doComment = true;
                    in.reset();
                }
                else
                    doComment = true;
                if (doComment) {
                    skipLine = removeCommented;
                    buf[len++] = '/';
                    buf[len++] = '/';
                    buf[len++] = ' ';
                    changedLine = 1;
                    changed = true;
                }
            }
            if (c != ' ' && c != '\t' && dataStart < 0) {
                // First non-space character.
                dataStart = len;
                if (nesting > 0 && commentAt != curIndent && c == '/') {
                    c = in.read();
                    if (c < 0)
                        break;
                    else if (c != '/')
                        buf[len++] = '/';
                    else {
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
            buf[len++] = (byte) c;
            if (c < 127 && c > ' ' && substitutions[c] != null) {
                int keystart = len-1;
                ArrayList<String> subs = substitutions[c];
                int nsub = subs.size();
                int next = in.read();
                for (int i = 0; ; i+=2) {
                    if (i == nsub) {
                        break;
                    }
                    String key = subs.get(i);
                    String val = subs.get(i+1);
                    int keylen = key.length();
                    int j = 1; // already matched key.charAt(0).
                    for (; j < keylen; j++) {
                        if (j == len-keystart) {
                            if (next == key.charAt(j)) {
                                buf[len++] = (byte) next;
                                next = in.read();
                            } else {
                                break;
                            }
                        } else if (buf[keystart+j] != key.charAt(j))
                            break;
                    }
                    if (j == keylen) {
                        // found match
                        len = keystart;
                        int vallen = val.length();
                        for (int k = 0; k < vallen; k++) {
                            if (len >= buf.length)
                                throw new ArrayIndexOutOfBoundsException("index:"+len+" arr-len:"+buf.length+" vallen:"+vallen);
                            buf[len++] = (byte) val.charAt(k);
                        }
                        break;
                    }
                }
                if (next < 0)
                    break;
                c = next;
                buf[len++] = (byte) c;
            }
            if (c == '\n') {
                if (len == lineStart+1 && commentAt >= 0)
                    skipLine = removeCommented;
                int firstNonSpace = -1;
                int lastNonSpace = 0;
                for (int i = lineStart; i < len-1; i++) {
                    if (buf[i] != ' ' && buf[i] != '\t' && buf[i] != '\r') {
                        lastNonSpace = i;
                        if (firstNonSpace < 0)
                            firstNonSpace = i;
                    }
                }
                if (lastNonSpace - firstNonSpace >= 4
                    && buf[firstNonSpace] == '/'
                    && buf[firstNonSpace+1] == '*'
                    && buf[lastNonSpace-1] == '*'
                    && buf[lastNonSpace] == '/') {
                    firstNonSpace += 2;
                    while (firstNonSpace < lastNonSpace
                           && buf[firstNonSpace] == ' ')
                        firstNonSpace++;
                    lastNonSpace -= 2;
                    while (lastNonSpace > firstNonSpace
                           && buf[lastNonSpace] == ' ')
                        lastNonSpace--;
                    if (buf[firstNonSpace] == '#') {
                        String cmnt = new String(buf, firstNonSpace,
                                                 lastNonSpace - firstNonSpace + 1,
                                                 "ISO-8859-1");
                        int sp = cmnt.indexOf(' ');
                        String rest;
                        Object binding;
                        cmdLine = lineno;
                        if (sp > 0) {
                            cmd = cmnt.substring(0, sp);
                            rest = cmnt.substring(sp).trim();
                            for(;;) {
                                int bar = rest.indexOf('|');
                                if (bar < 0) {
                                    binding = keywords.get(rest);
                                    break;
                                }
                                binding = keywords.get(rest.substring(0,bar));
                                if (binding == Boolean.TRUE)
                                    break;
                                rest = rest.substring(bar+1);
                            }
                        } else {
                            cmd = cmnt;
                            rest = "";
                            binding = null;
                        }
                        if ("#ifdef".equals(cmd) || "#ifndef".equals(cmd)) {
                            if (binding == null) {
                                /*
                                System.err.println(filename+":"+lineno
                                                   +": warning - undefined keyword: "+rest);
                                */
                                binding = Boolean.FALSE;
                            } 
                            nesting++;
                            if (skipNesting > 0)
                                commentAt = curIndent;
                            else if ((cmd.charAt(3) == 'n')
                                     != (binding == Boolean.FALSE)) {
                                commentAt = curIndent;
                                skipNesting = nesting;
                            }
                            skipLine = removeCommented;
                        } else if ("#else".equals(cmd)) {
                            if (nesting == 0)
                                error("unexpected "+cmd);
                            else if (nesting == skipNesting) {
                                commentAt = -1;
                                skipNesting = 0;
                            } else {
                                commentAt = curIndent;
                                if (skipNesting == 0)
                                    skipNesting = nesting;
                            }
                        } else if ("#endif".equals(cmd)) {
                            if (nesting == 0)
                                error("unexpected "+cmd);
                            if (nesting == skipNesting) {
                                skipNesting = 0;
                                commentAt = -1;
                            } else if (skipNesting > 0)
                                commentAt = curIndent;
                            nesting--;
                            skipLine = removeCommented;
                        } else
                            error("unknown command: "+cmnt);
                    }
                }
                if (skipLine)
                    len = lineStart;
                else
                    lineStart = len;
                dataStart = -1;
                curIndent = 0;
                lineno++;
                skipLine = false;
                changedLine = 0;
            } else if (dataStart < 0)
                curIndent = c == '\t' ? (curIndent + 8) & ~7 : curIndent + 1;
        }
        if (nesting != 0) {
            lineno = cmdLine;
            error("unterminated "+cmd);
        }
        resultBuffer = buf;
        resultLength = len;
        return changed;
    }

    void putSubstitution(String key, String val) {
        char key0 = key.charAt(0);
        if (key0 <= ' ' || key0 >= 127)
            error("invalid start character of substituton "+key);
        ArrayList<String> substitution = substitutions[key0];
        if (substitution == null) {
            substitution = new ArrayList<String>();
            substitutions[key0] = substitution;
        }
        int keylen = key.length();
        if (keylen > maxkey)
            maxkey = keylen;
        int vallen = val.length();
        if (vallen > maxkey)
            maxkey = vallen;
        substitution.add(key);
        substitution.add(val);
    }

    String getSubstitution(String key) {
        char key0 = key.charAt(0);
        if (key0 <= ' ' || key0 >= 127)
            return null;
        ArrayList<String> substitution = substitutions[key0];
        if (substitution == null)
            return null;
        int sz = substitution.size();
        for (int i = 0; i < sz; i+=2) {
            if (substitution.get(i).equals(key))
                return substitution.get(i+1);
        }
        return null;
    }

    void handleArg(String arg) {
        char arg0 = arg.charAt(0);
        if (arg0 == '=' || arg0 == '$' || arg0 == '@') {
            int eq = arg.indexOf('=', 1);
            if (eq < 0)
                error("missing substiution keyword in "+arg);
            String key = arg.substring(arg0 == '=' ? 1 : 0, eq);
            String val = arg.substring(eq+1);
            putSubstitution(key, val);            
        } else if (arg.charAt(0) == '%') {
            arg = arg.substring(1);
            for (int i = 0;  ;  i += 2 ) {
                if (arg.equals("UniformVector")) {
                    // Special hacks for gnu.lists.
                    putSubstitution("-*- java -*-", "");
                    putSubstitution("$PREAMBLE$",
                                    "This file is generated from PrimVector.template. DO NOT EDIT!");
                    String TAG = getSubstitution("$TAG$");
                    if (TAG != null && getSubstitution("$tag$") == null)
                        putSubstitution("$tag$", TAG.toLowerCase());
                    String ptype = getSubstitution("$ptype$");
                    if (ptype != null && getSubstitution("$Ptype$") == null)
                        putSubstitution("$Ptype$",
                                        Character.toUpperCase(ptype.charAt(0))
                                        +ptype.substring(1));
                    if (getSubstitution("$MASK$") == null)
                        putSubstitution("$MASK$", "");
                    if (getSubstitution("@Abstract") == null)
                        putSubstitution("@Abstract", "");
                    if (getSubstitution("$ZERO$") == null)
                        putSubstitution("$ZERO$", "0");
                    if (getSubstitution("$RETURN_IF_UNEQUAL$(v1,v2)") == null)
                        putSubstitution("$RETURN_IF_UNEQUAL$(v1,v2)",
                                        "return v1 > v2 ? 1 : -1");
                    //if (getSubstitution("$GT$(v1,v2)") == null)
                    //    putSubstitution("$GT$(v1,v2)", "v1 > v2");
                    break;
                }
                if (i >= version_features.length) {
                    System.err.println("Unknown version: "+arg);
                    System.exit(-1);
                }
                if (arg.equals(version_features[i])) {
                    String features = version_features[i+1];
                    System.err.println("(variant "+arg+" maps to: "+features+")");
                    char feat0 = features.charAt(0);
                    char sep = feat0 == '/' || feat0 == ';' ? feat0 : ' ';
                    int start = 0;
                    while (start >= 0) {
                        int ind = features.indexOf(sep, start);
                        String farg;
                        if (ind >= 0) {
                            farg = features.substring(start, ind);
                            start = ind + 1;
                        } else {
                            farg = features.substring(start);
                            start = -1;
                        }
                        if (farg.length() > 0)
                            handleArg(farg);
                    }
                    break;
                }
            }
        } else if (arg.charAt(0) == '+')
            keywords.put(arg.substring(1), Boolean.TRUE);
        else if (arg.charAt(0) == '-') {
            int eq = arg.indexOf('=');
            if (eq > 1) {
                String keyword
                    = arg.substring(arg.charAt(1) == '-' ? 2 :1, eq);
                String value = arg.substring(eq+1);
                Boolean b = Boolean.FALSE;
                if (value.equalsIgnoreCase("true"))
                    b = Boolean.TRUE;
                else if (! value.equalsIgnoreCase("false")) {
                    System.err.println("invalid value "+value+" for "+keyword);
                    System.exit(-1);
                }
                keywords.put(keyword, b);
            } else
                keywords.put(arg.substring(1), Boolean.FALSE);
        } else {
            try {
                filter(arg);
            } catch (Throwable ex) {
                System.err.println("caught "+ex);
                ex.printStackTrace();
                System.exit(-1);
            }
        }
    }

    public static void main(String[] args){
        PreProcess pp = new PreProcess();
 
        pp.keywords.put("true", Boolean.TRUE);
        pp.keywords.put("false", Boolean.FALSE);

        for (int i = 0;  i < args.length;  i++) {
            String arg = args[i];
            if (arg.equals("-o") && i + 1 < args.length)
                pp.outFilename = args[++i];
            else
                pp.handleArg(arg);
        }
    }
}
