;;  Cristiano Rotunno, Matricola: "914317"
;;  Alessandro Rutigliano, Matricola: "909971"

(defstruct urilib-structure
  scheme
  userinfo
  host
  port
  path
  query
  fragment)

(defun urilib-scheme (uri) (urilib-structure-scheme uri))
(defun urilib-userinfo (uri) (urilib-structure-userinfo uri))
(defun urilib-host (uri) (urilib-structure-host uri))
(defun urilib-port (uri) (urilib-structure-port uri))
(defun urilib-path (uri) (urilib-structure-path uri))
(defun urilib-query (uri) (urilib-structure-query uri))
(defun urilib-fragment (uri) (urilib-structure-fragment uri))

;;------------------------------------------
;;
;;              URILIB PARSE FUNCTIONS
;;
;;------------------------------------------

(defun urilib-parse (input-string)
  (let* ((chars (coerce input-string 'list))
         (scheme-out (parse-scheme chars)))
    (urilib-parse-scheme-based (first scheme-out) (second scheme-out))))

(defun urilib-parse-scheme-based (scheme scheme-tail)
  (cond
    ((string= scheme "mailto")
     (urilib-parse-mailto scheme-tail))
    ((string= scheme "news")
     (urilib-parse-news scheme-tail))
    ((string= scheme "tel")
     (urilib-parse-tel scheme-tail))
    ((string= scheme "fax")
     (urilib-parse-fax scheme-tail))
    ((string= scheme "zos")
     (urilib-parse-zos scheme-tail))
    (t
     (urilib-parse-default scheme scheme-tail))))

(defun urilib-parse-mailto (scheme-tail)
  (let* ((userinfo-out (parse-userinfo-mailto scheme-tail))
         (userinfo (car userinfo-out))
         (userinfo-tail (cadr userinfo-out))
         (host (if userinfo-tail
                (let ((host-out (parse-host userinfo-tail)))
                  (if (null (second host-out))
                      (first host-out)
                      (error "Invalid mailto"))
                  )
                nil)))
    (make-urilib-structure
      :scheme "mailto"
      :userinfo userinfo
      :host host
      :port nil
      :path nil
      :query nil
      :fragment nil)))

(defun urilib-parse-news (scheme-tail)
  (let* ((host-out (parse-host scheme-tail))  ;; host parse
         (host (first host-out))
         (host-tail (second host-out)))
    (if (null host-tail)
        (make-urilib-structure
         :scheme "news"
         :userinfo nil
         :host host
         :port nil
         :path nil
         :query nil
         :fragment nil)
        (error "News scheme can only contain the host. 
                Unexpected input: ~a" host-tail))))

(defun urilib-parse-tel (scheme-tail)
  (let* ((userinfo-out (parse-userinfo-telfax scheme-tail))  ;; userinfo parse
         (userinfo (first userinfo-out))
         (userinfo-tail (second userinfo-out)))
    (if (null userinfo-tail)
        (make-urilib-structure
         :scheme "tel"
         :userinfo userinfo
         :host nil
         :port nil
         :path nil
         :query nil
         :fragment nil)
        (error "Tel scheme can only contain the userinfo. 
        Unexpected input: ~a" userinfo-tail))))

(defun urilib-parse-fax (scheme-tail)
  (let* ((userinfo-out (parse-userinfo-telfax scheme-tail))  ;; userinfo parse
         (userinfo (first userinfo-out))
         (userinfo-tail (second userinfo-out)))
    (if (null userinfo-tail)
        (make-urilib-structure
         :scheme "fax"
         :userinfo userinfo
         :host nil
         :port nil
         :path nil
         :query nil
         :fragment nil)
        (error "Fax scheme can only contain the userinfo. 
        Unexpected input: ~a" userinfo-tail))))

(defun urilib-parse-zos (scheme-tail)
  (let* ((authority-out (parse-authority scheme-tail)) ;; Authority Parsing
         (authority (first authority-out))
         (auth-tail (second authority-out))
         (path-out (parse-path-zos auth-tail)) ;; Path Parsing
         (path (first path-out))
         (path-tail (second path-out))
         (query-out (parse-query path-tail)) ;; Query Parsing
         (query (first query-out))
         (query-tail (second query-out))
         (fragment-out (parse-fragment query-tail)) ;; Fragment Parsing
         (fragment (first fragment-out)))
    (make-urilib-structure
     :scheme "zos"
     :userinfo (first authority)
     :host (second authority)
     :port (third authority)
     :path path
     :query query
     :fragment fragment)))

(defun urilib-parse-default (scheme scheme-tail)
  (let* ((authority-out (parse-authority scheme-tail)) ;; Authority Parsing
         (authority (first authority-out))
         (auth-tail (second authority-out))
         (path-out (parse-path auth-tail)) ;; Path Parsing
         (path (first path-out))
         (path-tail (second path-out))
         (query-out (parse-query path-tail)) ;; Query Parsing
         (query (first query-out))
         (query-tail (second query-out))
         (fragment-out (parse-fragment query-tail)) ;; Fragment Parsing
         (fragment (first fragment-out)))
    (make-urilib-structure
     :scheme scheme
     :userinfo (first authority)
     :host (second authority)
     :port (third authority)
     :path path
     :query query
     :fragment fragment)))

;;------------------------------------------
;;
;;              ELEMENTS PARSE FUNCTIONS
;;
;;------------------------------------------

(defun parse-scheme (chars &optional (scheme-acc nil))
  (cond
    ((null chars)
     (error "Invalid scheme: input is empty"))
    ((char= (car chars) #\:)
      (if (null scheme-acc)
          (error "Invalid scheme: must contain at least one character")
          (list (accumulate-to-string scheme-acc) (cdr chars))))
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

        (if (and port-tail (char= (car port-tail) #\/))
          (list (list userinfo host port) (cdr port-tail)) ;; Remove '/'
          (list (list userinfo host port) port-tail)))
    
    (if (and chars (char= (car chars) #\/))
          (list (list nil nil 80) (cdr chars)) ;; Remove '/'
          (list (list nil nil 80) chars))))

(defun parse-userinfo (input &optional (userinfo-acc nil) (or-input input))
  (cond
    ((null input)
     (list nil or-input))
    ((char= (car input) #\@)
     (list (accumulate-to-string userinfo-acc) (cdr input)))
    ((is_character (car input))
     (parse-userinfo (cdr input) (cons (car input) userinfo-acc) or-input))
    (t
     (list nil or-input))))

(defun parse-host (input)
  (if (null input)
    (error "Invalid host: input is empty")
    (let* ((result (if (letter (car input))
                       (parse-host-alpha input)
                       (parse-ip input)))
            (host (first result))
            (tail (second result)))
      (list host tail))))

(defun parse-host-alpha (chars &optional (host-acc nil))
  (cond
    ((null chars)
      (if (null host-acc)
        (error "Invalid host: must contain at least one character")
        (list (accumulate-to-string host-acc) nil)))

    ((and (null host-acc) (not (letter (car chars))))
      (error "Invalid host: must start with a letter"))

    ((char= (first chars) #\.)
      (cond
        ((null (second chars))
          (error "Invalid host: host cannot end with '.'."))
        ((letter (second chars))
          (parse-host-alpha (cdr chars) (cons (car chars) host-acc)))
        (t
          (error "Invalid character in path after '.': ~a" (second chars)))))
    ((alphanum (car chars))
      (parse-host-alpha (cdr chars) (cons (car chars) host-acc)))

    ((is-host-separator (car chars))
      (list (accumulate-to-string host-acc) chars))
    (t
      (error "Invalid character in host: ~a" (car chars)))))

(defun parse-ip (chars &optional (value 0) (counter 0) (digits 0) (ip-acc nil))
  (cond
    ((or (null chars) (is-host-separator (car chars)))
     (if (and (= counter 3) (> digits 0))
        (list (accumulate-to-string ip-acc) chars)
        (error "Invalid IP: too few blocks")))
        
    ((or (> counter 3) (> digits 3))
      (error "Invalid IP: invalid format"))

    ((and (char= (car chars) #\.) (= digits 0))
      (error "Invalid IP: missing digits after '.'"))

    ((digit (car chars))
     (let ((new-value (+ (* value 10) 
                          (- (char-code (car chars)) 
                            (char-code #\0)))))
       (if (> new-value 255)
          (error "Invalid IP: block value exceeds 255")
          (parse-ip (cdr chars) 
                    new-value counter 
                    (1+ digits) 
                    (cons (car chars) ip-acc)))))

    ((char= (car chars) #\.)
      (parse-ip (cdr chars) 0 (1+ counter) 0 (cons (car chars) ip-acc)))
    
    (t
      (error "Invalid character in IP: ~a" (car chars)))))

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
       (list (accumulate-to-int port-acc) nil))
      ((char= (car chars) #\/)
       (list (accumulate-to-int port-acc) (cdr chars)))
      ((is-port-separator (car chars))
       (list (accumulate-to-int port-acc) chars))
      ((digit (car chars))
       (parse-port (cdr chars) nil (cons (car chars) port-acc)))
      (t (error "Invalid character in port: ~a" (car chars))))))

(defun is-port-separator (char)
  (or (char= char #\/)
      (char= char #\?)
      (char= char #\#)))

(defun parse-path (chars &optional (path-acc nil))
  (cond
    ((null chars)
      (list (accumulate-to-string path-acc) nil))
    ((char= (first chars) #\/)
      (cond
        ((null (second chars))
          (if (null path-acc)
              (list nil nil)
              (error "Invalid path: path cannot end with '/'.")))
        ((is_character (second chars))
          (parse-path (cdr chars) (cons (car chars) path-acc)))
        (t
          (error "Invalid character in path after '/': ~a" (second chars)))))
    ((is_character (car chars))
      (parse-path (cdr chars) (cons (car chars) path-acc)))
    ((is-path-separator (car chars))
      (list (accumulate-to-string path-acc) chars))
    (t
      (error "Invalid character in path: ~a" (car chars)))))
    
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


;;------------------------------------------
;;
;;              SPECIAL FUNCTIONS
;;
;;------------------------------------------

(defun parse-userinfo-mailto (input &optional (userinfo-acc nil))
  (cond
    ((null input)
      (if (null userinfo-acc)
        (error "Invalid userinfo: must contain at least one valid character")
        (list (accumulate-to-string userinfo-acc) nil)))
    ((char= (car input) #\@)
      (if (null userinfo-acc)
        (error "Invalid userinfo: must contain at least one valid character")
        (if (null (cdr input))
          (error "Invalid mailto URI: missing host after '@'")
          (list (accumulate-to-string userinfo-acc) (cdr input)))))
    ((is_character (car input))
      (parse-userinfo-mailto (cdr input) (cons (car input) userinfo-acc)))
    (t
      (error "Invalid character in userinfo: ~a" (car input)))))

(defun parse-userinfo-telfax (input &optional (ui-acc nil) (or-input input))
  (cond
    ((null input)
      (if (null ui-acc)
        (error "Invalid userinfo: must contain at least one valid character")
        (list (accumulate-to-string ui-acc) nil)))
    ((is_character (car input))
      (parse-userinfo-telfax (cdr input) (cons (car input) ui-acc) or-input))
    (t
      (error "Invalid character in userinfo: ~a" (car input)))))


(defun parse-path-zos (chars)
  (let* ((path-input 
          (if (and (not (null chars))
                   (char= (car chars) #\/))
            (cdr chars)
            chars))
          (path-out (parse-id44 path-input))
          (path (first path-out))
          (path-tail (second path-out)))
    (list path path-tail)))

(defun parse-id44 (chars &optional (id44-acc nil) (id44-length 0))
  (cond
    ((null chars)
      (if (<= id44-length 44)
        (list (accumulate-to-string id44-acc) nil)
        (error "Invalid id44: must contain at most 44 characters")))

    ((is-path-separator (car chars))
      (if (<= id44-length 44)
        (list (accumulate-to-string id44-acc) chars)
        (error "Invalid id44: must contain at most 44 characters")))

    ((and (= id44-length 0)
          (not (letter (car chars))))
      (error "Invalid id44: must start with a letter"))

    ((char= (first chars) #\.)
      (cond
        ((null (second chars))
          (error "Invalid path: path cannot end with '.'."))
        ((alphanum (second chars))
          (parse-id44 (cdr chars) (cons (car chars) id44-acc) (1+ id44-length)))
        (t
          (error "Invalid character in path after '.': ~a" (second chars)))))
    ((alphanum (car chars))
      (parse-id44 (cdr chars) (cons (car chars) id44-acc) (1+ id44-length)))

    ((= (char-code (car chars)) 40) ;; ( character found
      (if (<= id44-length 44)
        (parse-id8 (cdr chars) (cons (car chars) id44-acc))
        (error "Invalid id44: must contain at most 44 characters")))
    (t
      (error "Invalid character in path: ~a" (car chars)))))

(defun parse-id8 (chars &optional (id8-acc nil) (id8-length 0))
  (cond
    ((null chars)
      (error "Invalid id8: missing ')' character"))
    ((= (char-code (car chars)) 41) ;; ) character found
      (if (> id8-length 0)
        (if (<= id8-length 8)
          (list (accumulate-to-string (cons (car chars) id8-acc)) (cdr chars))
          (error "Invalid id8: must contain at most 8 characters"))
        (error "Invalid id8: must contain at least one character")))

    ((and (= id8-length 0) (not (letter (car chars))))
      (error "Invalid id8: must start with a letter"))

    ((alphanum (car chars))
      (parse-id8 (cdr chars) (cons (car chars) id8-acc) (1+ id8-length)))

    (t
      (error "Invalid character in path: ~a" (car chars)))))


(defun accumulate-to-string (chars)
  (if (null chars)
      nil
      (coerce (reverse chars) 'string)))

(defun accumulate-to-int (chars)
  (if (null chars)
      nil
      (parse-integer (coerce (reverse chars) 'string))))
    
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
        (and (or (letter (car code)) (digit (car code))) 
              (alphanum (cdr code))))
      ((characterp code)
        (or (letter code) (digit code)))
      (t nil)))

;;------------------------------------------
;;
;;              DISPLAY
;;
;;------------------------------------------     
(defun urilib-display (uri)
  (format t "Scheme: ~a~%" (urilib-structure-scheme uri))
  (format t "Userinfo: ~a~%" (urilib-structure-userinfo uri))
  (format t "Host: ~a~%" (urilib-structure-host uri))
  (format t "Port: ~a~%" (urilib-structure-port uri))
  (format t "Path: ~a~%" (urilib-structure-path uri))
  (format t "Query: ~a~%" (urilib-structure-query uri))
  (format t "Fragment: ~a~%" (urilib-structure-fragment uri)))