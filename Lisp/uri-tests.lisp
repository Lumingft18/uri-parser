;;; uri-tests.lisp
;;; File per eseguire test sul parser URI

(defpackage :uri-tests
  (:use :cl)
  (:export :run-uri-tests)) ;; Esporta la funzione per eseguire i test

(in-package :uri-tests)

;; Carica il parser URI se non è già stato caricato
;; Modifica il percorso del file secondo la tua configurazione
(load "/Users/alessandrorutigliano/Desktop/Uni/2 anno/lp/Progetto/Lisp/uri-parser-copy.lisp")

;;; Definizione dei test
(defparameter *uri-tests*
  '(
    ;; Testset "Standard Scheme (http)"
    (:input "http://disco.unimib.it"
     :scheme "http"
     :userinfo nil
     :host "disco.unimib.it"
     :port 80
     :path nil
     :query nil
     :fragment nil)

    (:input "http://user@server.example.org:8080/path/to/resource?key=value#section1"
     :scheme "http"
     :userinfo "user"
     :host "server.example.org"
     :port 8080
     :path "path/to/resource"
     :query "key=value"
     :fragment "section1")

    (:input "http://www.ietf.org/rfc/rfc2396.txt"
     :scheme "http"
     :userinfo nil
     :host "www.ietf.org"
     :port 80
     :path "rfc/rfc2396.txt"
     :query nil
     :fragment nil)

    ;; Testset "Standard Scheme (https)"
    (:input "https://example.com"
     :scheme "https"
     :userinfo nil
     :host "example.com"
     :port 443
     :path nil
     :query nil
     :fragment nil)

    (:input "https://www.ietf.org/rfc/rfc2396.txt"
     :scheme "https"
     :userinfo nil
     :host "www.ietf.org"
     :port 443
     :path "rfc/rfc2396.txt"
     :query nil
     :fragment nil)

    ;; Testset "FTP Scheme"
    (:input "ftp://ftp.example.com/files"
     :scheme "ftp"
     :userinfo nil
     :host "ftp.example.com"
     :port 21
     :path "files"
     :query nil
     :fragment nil)

    (:input "ftp://ftp.is.co.za/rfc/rfc1808.txt"
     :scheme "ftp"
     :userinfo nil
     :host "ftp.is.co.za"
     :port 21
     :path "rfc/rfc1808.txt"
     :query nil
     :fragment nil)

    ;; Testset "Valid IPv4 Addresses"
    (:input "http://192.168.1.1"
     :scheme "http"
     :userinfo nil
     :host "192.168.1.1"
     :port 80
     :path nil
     :query nil
     :fragment nil)

    (:input "https://255.255.255.255:443/path?query=123#frag"
     :scheme "https"
     :userinfo nil
     :host "255.255.255.255"
     :port 443
     :path "path"
     :query "query=123"
     :fragment "frag")

    ;; Testset "Invalid IPv4 Addresses"
    (:input "http://256.100.50.25"
     :error "Valore IP fuori range: 256")

    (:input "ftp://192.168.1"
     :error "IP incompleto, mancano ottetti")

    (:input "ftp://192..168.1.1"
     :error "Ottetto IP vuoto")

    (:input "ftp://192.abc.1.1"
     :error "Ottetto IP non valido")

    ;; Testset "mailto"
    (:input "mailto:user@example.com"
     :scheme "mailto"
     :userinfo "user"
     :host "example.com"
     :port nil
     :path nil
     :query nil
     :fragment nil)

    (:input "mailto:John.Doe@example.com"
     :scheme "mailto"
     :userinfo "John.Doe"
     :host "example.com"
     :port nil
     :path nil
     :query nil
     :fragment nil)

    ;; Testset "news"
    (:input "news:my.news.server"
     :scheme "news"
     :userinfo nil
     :host "my.news.server"
     :port nil
     :path nil
     :query nil
     :fragment nil)

    (:input "news:comp.lang.julia"
     :scheme "news"
     :userinfo nil
     :host "comp.lang.julia"
     :port nil
     :path nil
     :query nil
     :fragment nil)

    (:input "news:comp.infosystems.www.servers.unix"
     :scheme "news"
     :userinfo nil
     :host "comp.infosystems.www.servers.unix"
     :port nil
     :path nil
     :query nil
     :fragment nil)

    ;; Testset "tel"
    (:input "tel:0123456789"
     :scheme "tel"
     :userinfo "0123456789"
     :host nil
     :port nil
     :path nil
     :query nil
     :fragment nil)

    (:input "tel:+39-3541237567"
     :scheme "tel"
     :userinfo "+39-3541237567"
     :host nil
     :port nil
     :path nil
     :query nil
     :fragment nil)

    (:input "tel:+1-816-555-1212"
     :scheme "tel"
     :userinfo "+1-816-555-1212"
     :host nil
     :port nil
     :path nil
     :query nil
     :fragment nil)

    ;; Testset "fax"
    (:input "fax:0123456789"
     :scheme "fax"
     :userinfo "0123456789"
     :host nil
     :port nil
     :path nil
     :query nil
     :fragment nil)

    (:input "fax:0184-567345"
     :scheme "fax"
     :userinfo "0184-567345"
     :host nil
     :port nil
     :path nil
     :query nil
     :fragment nil)

    (:input "fax:+1-816-555-1212"
     :scheme "fax"
     :userinfo "+1-816-555-1212"
     :host nil
     :port nil
     :path nil
     :query nil
     :fragment nil)

    ;; Testset "zos"
    (:input "zos://user@mainframe.host:1234/DATASET.NAME(ENTRY)?q=test#frag"
     :scheme "zos"
     :userinfo "user"
     :host "mainframe.host"
     :port 1234
     :path "DATASET.NAME(ENTRY)"
     :query "q=test"
     :fragment "frag")

    (:input "zos://u@h:80/DATASETNAME"
     :scheme "zos"
     :userinfo "u"
     :host "h"
     :port 80
     :path "DATASETNAME"
     :query nil
     :fragment nil)

    ;; Error Testset "zos"
    (:input "zos:"
     :error "Path vuoto")

    (:input "zos:INVALID."
     :error "Terminazione non valida")

    (:input "zos:ABC.DEF(GHIX)"
     :error "ID8 troppo lungo")

    (:input "zos:123.DEF"
     :error "ID44 deve iniziare con una lettera")

    ;; Altri Test aggiuntivi dal codice esistente
    (:input "https://unimib.it"
     :scheme "https"
     :userinfo nil
     :host "unimib.it"
     :port 443
     :path nil
     :query nil
     :fragment nil)

    (:input "https:///unimib.it"
     :error "Troppi /")

    (:input "ftp://123example.com"
     :scheme "ftp"
     :userinfo nil
     :host "123example.com"
     :port 21
     :path nil
     :query nil
     :fragment nil)

    (:input "ftp:///badexample"
     :error "Troppi /")

    (:input "http://example.com:80/path?query=1#frag"
     :scheme "http"
     :userinfo nil
     :host "example.com"
     :port 80
     :path "path"
     :query "query=1"
     :fragment "frag")

    (:input "https://123.com"
     :scheme "https"
     :userinfo nil
     :host "123.com"
     :port 443
     :path nil
     :query nil
     :fragment nil)

    (:input "https://.com"
     :error "Host invalido (non può iniziare con '.')")

    (:input "https://com"
     :scheme "https"
     :userinfo nil
     :host "com"
     :port 443
     :path nil
     :query nil
     :fragment nil)

    (:input "https://:/path"
     :error "Porta mancante dopo il `:`")

    (:input "mailto:user@"
     :scheme "mailto"
     :userinfo "user"
     :host nil
     :port nil
     :path nil
     :query nil
     :fragment nil)

    (:input "fax:"
     :error "Fax senza numero")

    (:input "mailto:"
     :scheme "mailto"
     :userinfo nil
     :host nil
     :port nil
     :path nil
     :query nil
     :fragment nil)

    (:input "http://[::1]"
     :scheme "http"
     :userinfo nil
     :host "[::1]"
     :port 80
     :path nil
     :query nil
     :fragment nil)

    (:input "http://[::1]:8080"
     :scheme "http"
     :userinfo nil
     :host "[::1]"
     :port 8080
     :path nil
     :query nil
     :fragment nil)

    (:input "http://[::1]:invalid"
     :error "Porta non valida")

    (:input "http://-invalid.com"
     :error "Host con carattere iniziale invalido")

    (:input "http://example.com:-80"
     :error "Porta negativa")

    (:input "http://example.com:99999"
     :error "Porta fuori range")

    (:input "news:"
     :error "News senza host")

    (:input "mailto:user"
     :scheme "mailto"
     :userinfo "user"
     :host nil
     :port nil
     :path nil
     :query nil
     :fragment nil)
  ))

;;; Funzione per eseguire i test
(defun run-uri-tests ()
  (format t "~%Esecuzione dei test URI...~%")
  (loop for test in *uri-tests*
        for input = (getf test :input)
        for expected-error = (getf test :error)
        do (if expected-error
               ;; Test che si aspetta un errore
               (let ((result (handler-case
                              (progn
                                (urilib-parse input)
                                nil) ;; Se parsing ha successo, ritorna nil
                              (urilib-error (e)
                                (urilib-error-message e))))) ;; Recupera il messaggio d'errore
                 (if (string= result expected-error)
                     (format t " Test URI: ~a~%  Errore atteso: ~a~%  Errore ottenuto: ~a~%~%"
                             input expected-error result)
                     (format t " Test fallito: ~a~%  Errore atteso: ~a~%  Errore ottenuto: ~a~%~%"
                             input expected-error result)))
               ;; Test che si aspetta successo e verifica componenti
               (let ((uri (urilib-parse input)))
                 (let ((scheme (urilib-scheme uri))
                       (userinfo (urilib-userinfo uri))
                       (host (urilib-host uri))
                       (port (urilib-port uri))
                       (path (urilib-path uri))
                       (query (urilib-query uri))
                       (fragment (urilib-fragment uri)))
                   (format t " Test URI: ~a~%" input)
                   ;; Verifica ciascuna componente
                   (when (not (equal scheme (getf test :scheme)))
                     (format t "    Scheme: Atteso ~a, Ottenuto ~a~%" (getf test :scheme) scheme))
                   (when (not (equal userinfo (getf test :userinfo)))
                     (format t "    Userinfo: Atteso ~a, Ottenuto ~a~%" (getf test :userinfo) userinfo))
                   (when (not (equal host (getf test :host)))
                     (format t "    Host: Atteso ~a, Ottenuto ~a~%" (getf test :host) host))
                   (when (not (equal port (getf test :port)))
                     (format t "    Port: Atteso ~a, Ottenuto ~a~%" (getf test :port) port))
                   (when (not (equal path (getf test :path)))
                     (format t "    Path: Atteso ~a, Ottenuto ~a~%" (getf test :path) path))
                   (when (not (equal query (getf test :query)))
                     (format t "    Query: Atteso ~a, Ottenuto ~a~%" (getf test :query) query))
                   (when (not (equal fragment (getf test :fragment)))
                     (format t "    Fragment: Atteso ~a, Ottenuto ~a~%" (getf test :fragment) fragment)))))
        finally (format t "~%Test completati.~%")))