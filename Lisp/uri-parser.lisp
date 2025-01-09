(defstruct uri scheme userinfo host port path query fragment)

;; Funzione principale: parser URI
(defun urilib-parse (input-string)
  (let ((uri-list (coerce input-string 'list)))
    (if (null uri-list)
        (error "Input vuoto, non è una URI valida")
        (parse-scheme uri-list))))

;; Parsing dello schema
(defun parse-scheme (uri-list)
  (let* ((scheme-end (position #\: uri-list))
         (scheme (subseq uri-list 0 scheme-end))
         (remainder (subseq uri-list (1+ scheme-end))))
    (if (or (null scheme)
            (not (valid-scheme scheme)))
        (error "URI non contiene uno schema valido")
        (let ((parsed-scheme (string-downcase (coerce scheme 'string))))
          (if (member parsed-scheme '("mailto" "news" "tel" "fax" "zos") :test #'string=)
              (parse-special-scheme parsed-scheme remainder)
              (parse-authority remainder parsed-scheme))))))

;; Validazione dello schema
(defun valid-scheme (scheme)
  (and (not (null scheme))
       (every #'alpha-char-p scheme)))

;; Parsing per schemi speciali
(defun parse-special-scheme (scheme remainder)
  (cond
    ((equal scheme "mailto") (parse-mailto remainder scheme))
    ((equal scheme "news") (parse-news remainder scheme))
    ((or (equal scheme "tel") (equal scheme "fax")) (parse-tel-fax remainder scheme))
    ((equal scheme "zos") (parse-zos-path remainder scheme))
    (t (error "Schema speciale non riconosciuto"))))

;; Parsing e validazione authority per schemi generici
(defun parse-authority (uri-list scheme)
  (if (and (>= (length uri-list) 2)
           (equal (subseq uri-list 0 2) '(#\/ #\/))) ;; Controlla che sia presente "//"
      (let ((authority-end (or (position #\/ uri-list :start 2)
                               (length uri-list))))
        (let* ((authority (subseq uri-list 2 authority-end))
               (userinfo-host-port (split-userinfo-host-port authority))
               (remainder (subseq uri-list authority-end))
               (host (cadr userinfo-host-port))) ;; Estrae l'host
          ;; Verifica se l'host è nullo o vuoto
          (if (or (null host) (string= host ""))
              (error "Host non valido o mancante nella URI")
              ;; Verifica che il primo carattere dell'host sia una lettera o cifra
              (if (not (or (alpha-char-p (char host 0))
                           (digit-char-p (char host 0))))
                  (error "Host deve iniziare con una lettera o cifra dopo '//'")
                  ;; Se valido, continua il parsing
                  (parse-path-query-fragment remainder
                                             scheme
                                             (car userinfo-host-port)
                                             host
                                             (caddr userinfo-host-port))))))
      ;; Errore se non è presente "//"
      (error "URI malformata: manca l'authority o i caratteri '//'. Controlla il formato.")))

(defun split-userinfo-host-port (authority)
  (let* ((at-pos (position #\@ authority))
         (userinfo (if at-pos
                       (coerce (subseq authority 0 at-pos) 'string)
                       nil))
         (host-port (subseq authority (if at-pos (1+ at-pos) 0)))
         (colon-pos (position #\: host-port))
         (host (if colon-pos
                   (coerce (subseq host-port 0 colon-pos) 'string)
                   (coerce host-port 'string)))
         (port (if colon-pos
                   (coerce (subseq host-port (1+ colon-pos)) 'string)
                   nil)))
    (list userinfo host port)))

;; Parsing path, query, fragment
(defun parse-path-query-fragment (uri-list scheme userinfo host port)
  (let* ((query-pos (position #\? uri-list))
         (fragment-pos (position #\# uri-list))
         (path (subseq uri-list 0 (or query-pos fragment-pos (length uri-list))))
         (query (if query-pos
                    (subseq uri-list (1+ query-pos) (or fragment-pos (length uri-list)))
                    nil))
         (fragment (if fragment-pos
                       (subseq uri-list (1+ fragment-pos))
                       nil)))
    (make-uri :scheme scheme
              :userinfo userinfo
              :host host
              :port (if port (parse-port port) "80")
              :path (if (null path) nil (coerce path 'string))
              :query (if (null query) nil (coerce query 'string))
              :fragment (if (null fragment) nil (coerce fragment 'string)))))

;; Parsing e validazione della porta
(defun parse-port (port)
  (if (every #'digit-char-p port)
      (let ((port-num (parse-integer port)))
        (if (and (>= port-num 0) (<= port-num 65535))
            (coerce port 'string)
            (error "Porta non valida")))
      (error "Porta contiene caratteri non validi")))

;; Parsing dello schema mailto
(defun parse-mailto (remainder scheme)
  (let ((at-pos (position #\@ remainder)))
    (if at-pos
        (let* ((userinfo (subseq remainder 0 at-pos))
               (host (subseq remainder (1+ at-pos))))
          (make-uri :scheme scheme
                    :userinfo (coerce userinfo 'string)
                    :host (coerce host 'string)))
        (make-uri :scheme scheme
                  :userinfo (coerce remainder 'string)))))

;; Parsing dello schema news
(defun parse-news (remainder scheme)
  (if (null remainder)
      (error "Host mancante per lo schema 'news'")
      (make-uri :scheme scheme :host (coerce remainder 'string))))

;; Parsing schemi tel e fax
(defun parse-tel-fax (remainder scheme)
  (if (and (not (null remainder))
           (every #'digit-char-p remainder))
      (make-uri :scheme scheme :userinfo (coerce remainder 'string))
      (error "Schema 'tel' e 'fax' accettano solo numeri.")))

;; Parsing dello schema zos
(defun parse-zos-path (path scheme)
  (let ((paren-pos (position #\( path)))
    (if (null path)
        (error "Path ZOS non valido: il path è vuoto")
        (if paren-pos
            ;; Caso con parentesi (es: ABC.DEF(GHI))
            (let* ((id44 (coerce (subseq path 0 paren-pos) 'string))
                   (id8 (coerce (subseq path (1+ paren-pos) (position #\) path)) 'string)))
              (format t "DEBUG: Validazione ID44='~a', ID8='~a'~%" id44 id8)
              (if (and (valid-id44 id44) (valid-id8 id8))
                  (make-uri :scheme scheme
                            :path (coerce path 'string))
                  (error "Path ZOS non valido: ID44 o ID8 non validi")))
            ;; Caso senza parentesi (es: ABC.DEF)
            (let ((id44 (coerce path 'string)))
              (format t "DEBUG: Validazione ID44='~a'~%" id44)
              (if (valid-id44 id44)
                  (make-uri :scheme scheme
                            :path (coerce path 'string))
                  (error "Path ZOS non valido: ID44 non valido")))))))

(defun valid-id44 (id44)
  (and (<= (length id44) 44)
       (alpha-char-p (char id44 0))
       (every (lambda (c) (or (alphanumericp c) (char= c #\.))) id44)
       (not (char= (char id44 (1- (length id44))) #\.))))

(defun valid-id8 (id8)
  (and (<= (length id8) 8) ;; Massimo 8 caratteri
       (every #'alphanumericp id8))) ;; Solo caratteri alfanumerici

;; Funzione per visualizzare la URI
(defun urilib-display (uri &optional (stream t))
  (let ((scheme (uri-scheme uri)))
    (cond
      ((equal scheme "news")
       (format stream "Host: ~a~%" (or (uri-host uri) "NIL")))
      ((equal scheme "mailto")
       (format stream "Userinfo: ~a~%Host: ~a~%"
               (or (uri-userinfo uri) "NIL")
               (or (uri-host uri) "NIL")))
      ((or (equal scheme "tel") (equal scheme "fax"))
       (format stream "Userinfo: ~a~%" (or (uri-userinfo uri) "NIL")))
      ((equal scheme "zos")
       (format stream "Path: ~a~%" (or (uri-path uri) "NIL")))
      (t
       (format stream "Scheme: ~a~%Userinfo: ~a~%Host: ~a~%Port: ~a~%Path: ~a~%Query: ~a~%Fragment: ~a~%"
               (or (uri-scheme uri) "NIL")
               (or (uri-userinfo uri) "NIL")
               (or (uri-host uri) "NIL")
               (or (uri-port uri) "NIL")
               (or (uri-path uri) "NIL")
               (or (uri-query uri) "NIL")
               (or (uri-fragment uri) "NIL"))))))

