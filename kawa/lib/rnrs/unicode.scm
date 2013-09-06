(module-name <kawa.lib.rnrs.unicode>)
(module-export char-upcase char-downcase char-titlecase char-foldcase
	       char-alphabetic? char-numeric? char-whitespace?
	       char-upper-case? char-lower-case? char-title-case?
	       char-general-category
	       string-upcase string-downcase string-titlecase string-foldcase
	       string-normalize-nfd string-normalize-nfkd
	       string-normalize-nfc string-normalize-nfkc)
(require <kawa.lib.characters>)

(define (char-upcase (ch :: character)) :: character
  (character:make (java.lang.Character:toUpperCase (ch:intValue))))

(define (char-downcase (ch :: character)) :: character
  (character:make (java.lang.Character:toLowerCase (ch:intValue))))

(define (char-titlecase (ch :: character)) :: character
  (character:make (java.lang.Character:toTitleCase (ch:intValue))))

(define (char-alphabetic? (ch :: character)) :: boolean
  (java.lang.Character:isLetter (ch:intValue)))

(define (char-numeric? (ch :: character)) :: boolean
  (java.lang.Character:isDigit (ch:intValue)))

(define (char-whitespace? (ch :: character)) :: boolean
  (gnu.kawa.functions.UnicodeUtils:isWhitespace (ch:intValue)))

(define (char-upper-case? (ch :: character)) :: boolean
  (java.lang.Character:isUpperCase (ch:intValue)))

(define (char-lower-case? (ch :: character)) :: boolean
  (java.lang.Character:isLowerCase (ch:intValue)))

(define (char-title-case? (ch :: character)) :: boolean
  (java.lang.Character:isTitleCase (ch:intValue)))

(define (char-foldcase (ch :: character)) :: character
  (let ((val (ch:intValue)))
    (if (or (= val #x130) (= val #x131))
	ch
	(character:make (java.lang.Character:toLowerCase
		    (java.lang.Character:toUpperCase val))))))

(define (char-general-category (ch :: character)) :: symbol
  (gnu.kawa.functions.UnicodeUtils:generalCategory (ch:intValue)))

(define (string-upcase (str :: string)) :: string
  (gnu.lists.FString ((str:toString):toUpperCase java.util.Locale:ENGLISH)))

(define (string-downcase (str :: string)) :: string
  (gnu.lists.FString ((str:toString):toLowerCase java.util.Locale:ENGLISH)))

(define (string-titlecase (str :: string)) :: string
  (gnu.lists.FString (gnu.kawa.functions.UnicodeUtils:capitalize str)))

(define (string-foldcase (str :: string)) :: string
  (gnu.lists.FString (gnu.kawa.functions.UnicodeUtils:foldCase str)))

(define-syntax string-normalize
  (syntax-rules ()
    ((string-normalize str kind)
     (cond-expand (string-normalize-unicode
		   (try-catch
		    (java.text.Normalizer:normalize str (static-field java.text.Normalizer$Form 'kind))
		    (ex java.lang.NoClassDefFoundError
			(error "unicode string normalization not available"))))
		  (else (error "unicode string normalization not available"))))))

(define (string-normalize-nfd (str :: string)) :: string
  (string-normalize str NFD))

(define (string-normalize-nfkd (str :: string)) :: string
  (string-normalize str NFKD))

(define (string-normalize-nfc (str :: string)) :: string
  (string-normalize str NFC))

(define (string-normalize-nfkc (str :: string)) :: string
  (string-normalize str NFKC))
