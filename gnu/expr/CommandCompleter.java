package gnu.expr;

import gnu.kawa.io.*;
import gnu.text.*;
import java.util.List;

/** Handle command completion based on expression context.
 */

public class CommandCompleter extends RuntimeException {
    /** A pseudo-letter returned on a completion request.
     * This is typically where a TAB was typed. */
    public static final char COMPLETE_REQUEST = '\uF102';

    int prefixLength;
    public List<String> candidates;
    public CommandCompleter(int prefixLength, List<String> candidates) {
        this.prefixLength = prefixLength;
        this.candidates = candidates;
    }
    @Override
    public Throwable fillInStackTrace() { return this; }

    public static int complete(CharArrayInPort cin,
                               List<CharSequence> candidates) {
        Compilation comp = Compilation.getCurrent();
        if (comp == null)
            return -1;
        SourceMessages messages = new SourceMessages();
        Language language = comp.getLanguage();
        int startPos = -1;
        Compilation tcomp = null;
        gnu.text.Lexer lexer = language.getLexer(cin, messages);
        lexer.setTentative(true);
        try {
            tcomp = language.parse(lexer, Language.PARSE_FOR_EVAL|Language.PARSE_ONE_LINE|Language.PARSE_INTERACTIVE_MODULE, null);
            language.resolve(tcomp);
        } catch (CommandCompleter ex) {
            java.util.Collections.sort(ex.candidates);
            candidates.addAll(ex.candidates);
            startPos = ex.candidates.isEmpty() ? -1
                : ex.prefixLength;
        } catch (Throwable ex) {
        } finally {
            if (tcomp != null)
                tcomp.setCurrentScope(null);
        }
        return startPos;
    }
}
