package gnu.expr;

import gnu.kawa.io.*;
import gnu.text.*;
import java.util.List;

/** Handle command completion based on expression context.
 * The main entrypoint is the complete method.
 */

public class CommandCompleter extends RuntimeException {
    /** A pseudo-letter returned on a completion request.
     * This is typically where a TAB was typed. */
    public static final char COMPLETE_REQUEST = '\uF102';

    int prefixLength;
    public String word;
    public int wordCursor;
    public List<String> candidates;
    Compilation comp;

    public CommandCompleter(int prefixLength, List<String> candidates,
                            String word, int wordCursor, Compilation comp) {
        this.prefixLength = prefixLength;
        this.candidates = candidates;
        this.word = word;
        this.wordCursor = wordCursor;
        this.comp = comp;
    }

    @Override
    public Throwable fillInStackTrace() { return this; }

    /* #ifndef with:jline3 */
    // /** Parse a partial expression containing a completion request.
    //  *
    //  * The port cin contains the partial command followed by the
    //  * special COMPLETE_REQUEST character, possibly follewed by more context.
    //  * When the symbol resolver sees a symbol ending with (or more generally
    //  * containing) the COMPLETE_REQUEST character, it looks for potentially
    //  * matching definitions whose prefix match the symbol.  Note we can
    //  * look for matches within the actual lexical context of the prefix.
    //  * The potential matches are bundled in a List, which is used to
    //  * construct a CommandCompleter exception, which we catch in this method.
    //  */
    // public static int complete(CharArrayInPort cin,
    //                            List<CharSequence> candidates) {
    //     SourceMessages messages = new SourceMessages();
    //     Language language = Language.getDefaultLanguage();
    //     int startPos = -1;
    //     Compilation tcomp = null;
    //     gnu.text.Lexer lexer = language.getLexer(cin, messages);
    //     lexer.setTentative(true);
    //     try {
    //         tcomp = language.parse(lexer, Language.PARSE_FOR_EVAL|Language.PARSE_INTERACTIVE_MODULE, null);
    //         language.resolve(tcomp);
    //     } catch (CommandCompleter ex) {
    //         java.util.Collections.sort(ex.candidates);
    //         candidates.addAll(ex.candidates);
    //         startPos = ex.candidates.isEmpty() ? -1
    //             : ex.prefixLength;
    //     } catch (Throwable ex) {
    //         System.err.println("ComComl caught2 "+ex);
    //     } finally {
    //         if (tcomp != null)
    //             tcomp.setCurrentScope(null);
    //     }
    //     return startPos;
    // }
    /* #endif */

    public Compilation getCompilation() { return comp; }
}
