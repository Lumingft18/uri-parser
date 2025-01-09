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
  "Parsa una stringa URI e restituisce una struttura urilib-structure."
  (let ((chars (coerce input-string 'list)))
    (parse-uri chars)))

;;; Parser basato su funzioni mutuamente ricorsive
(defun parse-uri (chars)
  "Inizia il parsing della URI."
  (multiple-value-bind (scheme rest-chars)
      (parse-scheme chars)
    (cond
      ;; Schemi speciali
      ((member scheme '("mailto" "news" "tel" "fax" "zos") :test #'string=)
       (parse-special-scheme scheme rest-chars))
      ;; Schemi standard
      (t
       (parse-authority rest-chars scheme)))))

(defun parse-scheme (chars)
  "Parsa lo schema della URI senza usare loop."
  (labels ((parse-scheme-rec (chs scheme)
             (cond
               ((null chs)
                (error "URI non contiene uno schema valido"))
               ((char= (first chs) #\: )
                (let ((scheme-str (string-downcase (coerce (reverse scheme) 'string))))
                  (if (valid-scheme scheme)
                      (values scheme-str (rest chs))
                      (error "URI non contiene uno schema valido"))))
               (t
                (if (alpha-char-p (first chs))
                    (parse-scheme-rec (rest chs) (cons (first chs) scheme))
                    (error "URI contiene caratteri non validi nello schema"))))))
    (parse-scheme-rec chars '())))

(defun valid-scheme (scheme)
  "Verifica la validità dello schema."
  (and (not (null scheme))
       (every #'alpha-char-p scheme)))

(defun parse-special-scheme (scheme chars)
  "Parsa schemi speciali come mailto, news, tel, fax, zos."
  (cond
    ((string= scheme "mailto") (parse-mailto chars))
    ((string= scheme "news") (parse-news chars))
    ((or (string= scheme "tel") (string= scheme "fax")) (parse-tel-fax scheme chars))
    ((string= scheme "zos") (parse-zos chars))
    (t (error "Schema speciale non riconosciuto"))))

(defun parse-mailto (chars)
  "Parsa lo schema mailto senza usare loop."
  (labels ((split-at-@ (chs userinfo)
             (cond
               ((null chs)
                (make-urilib-structure
                 :scheme "mailto"
                 :userinfo (when (not (null userinfo)) (coerce (reverse userinfo) 'string))))
               ((char= (first chs) #\@)
                (let ((host-chars (rest chs)))
                  (make-urilib-structure
                   :scheme "mailto"
                   :userinfo (coerce (reverse userinfo) 'string)
                   :host (when (not (null host-chars)) (coerce host-chars 'string)))))
               (t
                (split-at-@ (rest chs) (cons (first chs) userinfo))))))
    (split-at-@ chars '())))

(defun parse-news (chars)
  "Parsa lo schema news."
  (if (null chars)
      (error "Host mancante per lo schema 'news'")
      (make-urilib-structure
       :scheme "news"
       :host (coerce chars 'string))))

(defun parse-tel-fax (scheme chars)
  "Parsa gli schemi tel e fax senza usare loop."
  (if (and (not (null chars))
           (every #'digit-char-p chars))
      (make-urilib-structure
       :scheme scheme
       :userinfo (coerce chars 'string))
      (error "Schema 'tel' e 'fax' accettano solo numeri.")))

;;; Definizione di parse-zos-path prima di parse-zos per evitare riferimenti non definiti
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
  "Valida l'id44 per lo schema zos."
  (and (<= (length id44) 44)
       (alpha-char-p (char id44 0))
       (every (lambda (c) (or (alphanumericp c) (char= c #\.))) id44)
       (not (char= (char id44 (1- (length id44))) #\.))))

(defun valid-id8 (id8)
  "Valida l'id8 per lo schema zos."
  (and (<= (length id8) 8)
       (alpha-char-p (char id8 0))
       (every #'alphanumericp id8)))

(defun is-ip-address (host)
  "Determina se l'host è un indirizzo IP."
  (let ((parts (split-ip host)))
    (= (length parts) 4)))

(defun split-ip (host)
  "Dividi un indirizzo IP in parti senza usare loop."
  (labels ((split-rec (chs current result)
             (cond
               ((null chs)
                (reverse (if current
                             (cons (coerce (reverse current) 'string) result)
                             result)))
               ((char= (first chs) #\.)
                (split-rec (rest chs) '() (cons (coerce (reverse current) 'string) result)))
               (t
                (split-rec (rest chs) (cons (first chs) current) result))))))
    (split-rec (coerce host 'list) '() '()))

(defun valid-ip-address (ip)
  "Verifica che ogni segmento dell'indirizzo IP sia compreso tra 0 e 255."
  (and (is-ip-address ip)
       (every (lambda (part)
                (let ((num (parse-integer part :junk-allowed t)))
                  (and num (>= num 0) (<= num 255))))
              (split-ip ip))))


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

(defun parse-authority-rec (chars)
  "Parsing ricorsivo dell'authority senza usare POSITION.
   Restituisce urilib-structure e remaining chars."
  (labels ((split-userinfo-rec (chs current-userinfo)
             (cond
               ((null chs)
                (values (reverse current-userinfo) nil))
               ((char= (first chs) #\@)
                (values (reverse current-userinfo) (rest chs)))
               (t
                (split-userinfo-rec (rest chs) (cons (first chs) current-userinfo))))))
           (split-host-port-rec (chs current-host current-port)
             (cond
               ((null chs)
                (values (reverse current-host) (reverse current-port)))
               ((char= (first chs) #\: )
                (split-host-port-rec (rest chs) current-host '()))
               (t
                (split-host-port-rec (rest chs) (cons (first chs) current-host) current-port)))))
    (multiple-value-bind (userinfo remaining)
        (split-userinfo-rec chars '())
      (multiple-value-bind (host port)
          (split-host-port-rec remaining '())
        (values
         (make-urilib-structure
          :userinfo (when (not (null userinfo)) (coerce userinfo 'string))
          :host (coerce host 'string)
          :port (when (not (null port)) (coerce port 'string)))
         ;; remaining-chars è la lista di caratteri rimanenti dopo l'authority
         remaining))))

(defun parse-port (port)
  "Valida e restituisce la porta, applicando il default se necessario."
  (cond
    (port
     (if (every #'digit-char-p port)
         (let ((port-num (parse-integer port :junk-allowed t)))
           (if (and port-num (>= port-num 0) (<= port-num 65535))
               port
               (error "Porta non valida")))
         (error "Porta contiene caratteri non validi")))
    (t "80"))) ; Default generale

(defun parse-path-query-fragment (chars uri)
  "Parsa path, query e fragment senza usare loop."
  (labels ((parse-pqf-rec (chs path query fragment current)
             (cond
               ((null chs)
                (assign-pqf path query fragment uri))
               ((char= (first chs) #\?)
                (parse-pqf-rec (rest chs) path query fragment 'query))
               ((char= (first chs) #\#)
                (parse-pqf-rec (rest chs) path query fragment 'fragment))
               (t
                (cond
                  ((eq current 'path)
                   (parse-pqf-rec (rest chs) (cons (first chs) path) query fragment 'path))
                  ((eq current 'query)
                   (parse-pqf-rec (rest chs) path (cons (first chs) query) fragment 'query))
                  ((eq current 'fragment)
                   (parse-pqf-rec (rest chs) path query (cons (first chs) fragment) 'fragment))))))
           
           (assign-pqf (path query fragment uri)
             ;; Converti le liste di caratteri in stringhe, invertendo l'ordine
             (let ((path-str (unless (null path) (coerce (reverse path) 'string)))
                   (query-str (unless (null query) (coerce (reverse query) 'string)))
                   (fragment-str (unless (null fragment) (coerce (reverse fragment) 'string))))
               ;; Assegna il path e aggiorna l'uri
               (when path-str
                 (setf (urilib-structure-path uri) (validate-path path-str uri)))
               (when query-str
                 (setf (urilib-structure-query uri) query-str))
               (when fragment-str
                 (setf (urilib-structure-fragment uri) fragment-str))
               uri)))
    ;; Chiamata corretta con 5 argomenti: chs, path, query, fragment, current
    (parse-pqf-rec chars '() '() '() 'path)))

(defun validate-path (path uri)
  "Valida e assegna il path alla struttura URI."
  (let ((scheme (urilib-scheme uri)))
    (cond
      ((string= scheme "zos")
       (let ((parsed-path (parse-zos-path (coerce path 'list))))
         (if (stringp parsed-path)
             parsed-path
             (error "Path ZOS non valido: sintassi non corretta"))))
      (t
       path))))

;;; Funzione di visualizzazione
(defun urilib-display (uri &optional (stream t))
  "Visualizza i componenti della URI."
  (let ((scheme (urilib-scheme uri)))
    (cond
      ((string= scheme "news")
       (format stream "Host: ~a~%" (or (urilib-host uri) "NIL")))
      ((string= scheme "mailto")
       (format stream "Userinfo: ~a~%Host: ~a~%"
               (or (urilib-userinfo uri) "NIL")
               (or (urilib-host uri) "NIL")))
      ((or (string= scheme "tel") (string= scheme "fax"))
       (format stream "Userinfo: ~a~%" (or (urilib-userinfo uri) "NIL")))
      ((string= scheme "zos")
       (format stream "Path: ~a~%" (or (urilib-path uri) "NIL")))
      (t
       (format stream "Scheme: ~a~%Userinfo: ~a~%Host: ~a~%Port: ~a~%Path: ~a~%Query: ~a~%Fragment: ~a~%"
               (or (urilib-scheme uri) "NIL")
               (or (urilib-userinfo uri) "NIL")
               (or (urilib-host uri) "NIL")
               (or (urilib-port uri) "NIL")
               (or (urilib-path uri) "NIL")
               (or (urilib-query uri) "NIL")
               (or (urilib-fragment uri) "NIL"))))))

;;; Esempi di utilizzo
;; Schemi Standard
;; (defparameter disco (urilib-parse "http://disco.unimib.it"))
;; (urilib-display disco)   ;; Stampa dettagli

;; Schemi Speciali
;; Mailto
;; (defparameter email (urilib-parse "mailto:user@example.com"))
;; (urilib-display email)   ;; Stampa dettagli

;; News
;; (defparameter news-uri (urilib-parse "news:comp.lang.lisp"))
;; (urilib-display news-uri) ;; Stampa dettagli

;; Tel
;; (defparameter tel-uri (urilib-parse "tel:1234567890"))
;; (urilib-display tel-uri) ;; Stampa dettagli

;; ZOS
;; (defparameter zos-uri (urilib-parse "zos:ABC.DEF(GHIJKL)"))
;; (urilib-display zos-uri) ;; Stampa dettagli

;; URI con Indirizzo IP e Porta Personalizzata
;; (defparameter ip-uri (urilib-parse "http://user:pass@192.168.1.1:8080/path?query=param#frag"))
;; (urilib-display ip-uri)    ;; Stampa dettagli 