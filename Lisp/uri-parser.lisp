;;; Definizione della struttura URI
(defstruct urilib-structure
  scheme
  userinfo
  host
  port
  path
  query
  fragment)

;;; Funzioni di accesso esplicite
(defun urilib-scheme (uri) (urilib-structure-scheme uri))
(defun urilib-userinfo (uri) (urilib-structure-userinfo uri))
(defun urilib-host (uri) (urilib-structure-host uri))
(defun urilib-port (uri) (urilib-structure-port uri))
(defun urilib-path (uri) (urilib-structure-path uri))
(defun urilib-query (uri) (urilib-structure-query uri))
(defun urilib-fragment (uri) (urilib-structure-fragment uri))

;;; Funzione principale di parsing
(defun urilib-parse (input-string)
  (let ((chars (coerce input-string 'list)))
    (parse-uri chars)))

(defun parse-uri (chars)
  (let* ((scheme-out (parse-scheme chars)) ;; Scheme Parsing
         (scheme (car scheme-out))
         (scheme-tail (cadr scheme-out))
         (authority-out (parse-authority scheme-tail)) ;; Authority Parsing
         (authority (car authority-out))
         (auth-tail (cadr authority-out))
         (path-out (parse-path auth-tail)) ;; Path Parsing
         (path (car path-out))
         (path-tail (cadr path-out))
         (query-out (parse-query path-tail)) ;; Query Parsing
         (query (car query-out))
         (query-tail (cadr query-out))
         (fragment-out (parse-fragment query-tail)) ;; Fragment Parsing
         (fragment (car fragment-out))
         (fragment-tail (cadr fragment-out)))
    (make-urilib-structure
     :scheme scheme
     :userinfo (first authority)
     :host (second authority)
     :port (third authority)
     :path path
     :query query
     :fragment fragment)))

(defun parse-scheme (chars &optional (scheme-acc nil))
  (cond
    ((null chars)
     (error "Invalid scheme: input is empty"))
    ((char= (car chars) #\:)
     (list (accumulate-to-string scheme-acc) (cdr chars)))
    ((is_character (car chars))
     (parse-scheme (cdr chars) (cons (car chars) scheme-acc)))
    (t (error "Invalid scheme character: ~a" (car chars)))))

(defun parse-authority (chars)
  (if (and (not (null chars))
           (char= (car chars) #\/)
           (not (null (cdr chars)))
           (char= (cadr chars) #\/))
      (let* (
            ;; userinfo parse
            (userinfo-out (parse-userinfo (cddr chars)))
            (userinfo (car userinfo-out))
            (userinfo-tail (cadr userinfo-out))
            ;; host parse
            (host-out (parse-host userinfo-tail))
            (host (car host-out))
            (host-tail (cadr host-out))
            ;; port parse
            (port-out (parse-port host-tail))
            (port (car port-out))
            (port-tail (cadr port-out)))
        (list (list userinfo host port) port-tail))
    (list nil chars)))

(defun parse-userinfo (input &optional (userinfo-acc nil) (original-input input))
  (cond
    ((null input)
     (list nil original-input))
    ((char= (car input) #\@)
     (list (accumulate-to-string userinfo-acc) (cdr input)))
    ((is_character (car input))
     (parse-userinfo (cdr input) (cons (car input) userinfo-acc) original-input))
    (t
     (list nil original-input))))

(defun parse-host (input)
  (let* ((host-out (catch-host input))
         (host (car host-out))
         (host-tail (cadr host-out)))
    (if (or (is-host-valid host) (parse-ip host))
        (list (coerce host 'string) host-tail)
        (error "Invalid host"))))

(defun catch-host (input &optional (host-acc nil))
  (cond
    ((null input)
     (list (reverse host-acc) nil))
    ((is-host-separator (car input))
     (list (reverse host-acc) input))
    (t
     (catch-host (cdr input) (cons (car input) host-acc)))))

(defun is-host-valid (input &optional (restart t))
  (if restart
      (cond
        ((null input) nil)
        ((letter (car input))
         (is-host-valid (cdr input) nil))
        ((char= (car input) #\.)
         (error "Two consecutive dots"))
        (t nil))
      (cond
        ((null input) t)
        ((or (letter (car input)) (digit (car input)))
         (is-host-valid (cdr input) nil))
        ((char= (car input) #\.)
         (is-host-valid (cdr input) t))
        (t nil))))

(defun parse-ip (chars &optional (value 0) (counter 0) (digits 0))
  (cond
    ((and (null chars) (= counter 3) (> digits 0)) t)
    ((or (null chars) (> counter 3) (> digits 3)) nil)
    ((and (char= (car chars) #\.) (= digits 0)) nil)
    ((digit (car chars))
     (let ((new-value (+ (* value 10) (- (char-code (car chars)) (char-code #\0)))))
       (if (> new-value 255)
           nil
           (parse-ip (cdr chars) new-value counter (+ digits 1)))))
    ((char= (car chars) #\.)
     (parse-ip (cdr chars) 0 (+ counter 1) 0))
    (t nil)))

(defun is-host-separator (char)
  (or (char= char #\:)
      (char= char #\/)
      (char= char #\?)
      (char= char #\#)))

(defun parse-port (chars &optional (first t) (port-acc nil))
  (if first
      (cond
        ((null chars)
         (list 80 nil))
        ((char= (car chars) #\:)
         (if (null (cdr chars))
             (error "Invalid port: missing digits after ':'")
             (parse-port (cdr chars) nil port-acc)))
        (t (list 80 chars)))

    (cond
      ((null chars)
       (list (accumulate-to-string port-acc) nil))
      ((is-port-separator (car chars))
       (list (accumulate-to-string port-acc) chars))
      ((digit (car chars))
       (parse-port (cdr chars) nil (cons (car chars) port-acc)))
      (t (error "Invalid character in port: ~a" (car chars))))))

(defun is-port-separator (char)
  (or (char= char #\/)
      (char= char #\?)
      (char= char #\#)))

(defun parse-path (chars &optional (restart nil) (path-acc nil))
  (if restart
      (cond
        ((null chars)
         (if (null path-acc)
             (list nil nil)
             (error "Invalid path: path cannot end with '/'.")))
        ((char= (car chars) #\/)
          (error "Invalid path: two consecutive '/'."))
        ((is_character (car chars))
          (parse-path (cdr chars) nil (cons (car chars) path-acc)))
        (t 
         (error "Invalid character in path: ~a" (car chars))))

      (cond
        ((null chars)
         (list (accumulate-to-string path-acc) nil))
        ((is-path-separator (car chars))
         (list (accumulate-to-string path-acc) chars))
        ((char= (car chars) #\/)
         (if (null path-acc)
             (parse-path (cdr chars) t nil)
             (parse-path (cdr chars) t (cons (car chars) path-acc))))
        ((is_character (car chars))
          (parse-path (cdr chars) nil (cons (car chars) path-acc)))
        (t 
         (error "Invalid character in path: ~a" (car chars))))))

(defun is-path-separator (char)
  (or (char= char #\?)
      (char= char #\#)))

(defun parse-query (chars &optional (first t) (query-acc nil))
  (if first
      (cond
        ((null chars)
         (list nil nil))
        ((char= (car chars) #\?)
         (if (null (cdr chars))
             (error "Invalid query: missing characters after '?'")
             (parse-query (cdr chars) nil query-acc)))
        (t (list nil chars)))
    (cond
      ((null chars)
       (list (accumulate-to-string query-acc) nil))
      ((char= (car chars) #\#)
       (list (accumulate-to-string query-acc) chars))
      ((is_character (car chars))
       (parse-query (cdr chars) nil (cons (car chars) query-acc)))
      (t (error "Invalid character in query: ~a" (car chars))))))

(defun parse-fragment (chars &optional (first t) (fragment-acc nil))
  (if first
      (cond
        ((null chars)
         (list nil nil))
        ((char= (car chars) #\#)
         (if (null (cdr chars))
             (error "Invalid fragment: missing characters after '#'")
             (parse-fragment (cdr chars) nil fragment-acc)))
        (t (error "Invalid fragment: input does not start with '#'")))

      (cond
        ((null chars)
          (list (accumulate-to-string fragment-acc) nil))
        ((is_character (car chars))
          (parse-fragment (cdr chars) nil (cons (car chars) fragment-acc)))
        (t (error "Invalid character in fragment: ~a" (car chars))))))

(defun accumulate-to-string (chars)
  (if (null chars)
      nil
      (coerce (reverse chars) 'string)))
    
;;------------------------------------------
;;
;;              GRAMMAR
;;
;;------------------------------------------

(defun is_identifier (code)
  (and (not (null code))
        (is_characters code)))

(defun host-identifier (code)
  (and (listp code)
       (not (null code))
       (letter (car code))
       (alphanum (cdr code))))

(defun is_characters (code)
  (cond
    ((null code) t)
    ((is_character code) t)
    ((listp code)
      (and (is_character (car code)) (is_characters (cdr code))))
    (t nil)))

(defun is_character (code)
  (and (characterp code)
        (or (char= code #\_)
            (char= code #\=)
            (char= code #\+)
            (char= code #\-)
            (letter code)
            (digit code))))

(defun letter (code)
  (or (and (char>= code #\A) (char<= code #\Z))
      (and (char>= code #\a) (char<= code #\z))))

(defun digit (code)
  (cond
    ((null code) t)
    ((listp code)
     (and (digit (car code)) (digit (cdr code))))
    ((characterp code)
     (and (char>= code #\0) (char<= code #\9)))
    (t nil)))

(defun alphanum (code)
    (cond
      ((null code) t)
      ((listp code)
        (and (or (letter (car code)) (digit (car code))) (alphanum (cdr code))))
      ((characterp code)
        (or (letter code) (digit code)))
      (t nil)))
