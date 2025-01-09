;;; uri-tests.lisp
;;; File per eseguire test sul parser URI

(defpackage :uri-tests
  (:use :cl)
  (:export :run-uri-tests)) ;; Esporta la funzione per eseguire i test

(in-package :uri-tests)

;; Carica il parser URI se non è già stato caricato
(load "/Users/alessandrorutigliano/Desktop/Uni/2\ anno/lp/Progetto/Lisp/uri-parser-copy.lisp") ;; Modifica con il percorso del tuo file principale

;;; Definizione dei test
(defparameter *uri-tests*
  '((:input "https://unimib.it" :expected "success") ;; URI valida
    (:input "https:///unimib.it" :expected "error") ;; Troppi /
    (:input "ftp://123example.com" :expected "success") ;; URI valida con numeri
    (:input "ftp:///badexample" :expected "error") ;; Troppi /
    (:input "http://example.com:80/path?query=1#frag" :expected "success") ;; URI completa
    (:input "https://123.com" :expected "success") ;; Host numerico valido
    (:input "https://.com" :expected "error") ;; Host invalido (non può iniziare con '.')
    (:input "https://com" :expected "success") ;; URI valida con host minimo
    (:input "https://:/path" :expected "error") ;; Porta mancante dopo il `:`
    (:input "mailto:user@example.com" :expected "success") ;; Mailto valido
    (:input "mailto:user@" :expected "success") ;; Mailto valido senza host
    (:input "news:example.com" :expected "success") ;; News valido
    (:input "fax:1234567890" :expected "success") ;; Fax valido
    (:input "zos:ABC.DEF(GHI)" :expected "success") ;; ZOS valido
    (:input "zos:ABC" :expected "success") ;; ZOS valido senza ID8
    (:input "zos:" :expected "error") ;; Path vuoto
    (:input "zos:INVALID." :expected "error") ;; Terminazione non valida
    (:input "zos:ABC.DEF(GHIX)" :expected "error") ;; ID8 troppo lungo
    (:input "zos:123.DEF" :expected "error") ;; ID44 deve iniziare con una lettera
    (:input "ftp://host.com/path/to/resource" :expected "success") ;; FTP valido
    (:input "tel:123456789" :expected "success") ;; Telefono valido
    (:input "tel:" :expected "error") ;; Telefono senza numero
    (:input "mailto:" :expected "success") ;; Mailto vuoto (valido)
    (:input "http://[::1]" :expected "success") ;; Indirizzo IPv6 valido
    (:input "http://[::1]:8080" :expected "success") ;; IPv6 con porta
    (:input "http://[::1]:invalid" :expected "error") ;; Porta non valida
    (:input "http://-invalid.com" :expected "error") ;; Host con carattere iniziale invalido
    (:input "http://example.com:-80" :expected "error") ;; Porta negativa
    (:input "http://example.com:99999" :expected "error") ;; Porta fuori range
    (:input "news:" :expected "error") ;; News senza host
    (:input "mailto:user" :expected "success"))) ;; Mailto valido senza host

;;; Funzione per eseguire i test
(defun run-uri-tests ()
  (format t "~%Esecuzione dei test URI...~%")
  (loop for test in *uri-tests*
        for input = (getf test :input)
        for expected = (getf test :expected)
        for result = (handler-case
                         (progn
                           (urilib-parse input)
                           "success")
                       (error (e) "error"))
        do (format t "Test URI: ~a~%Risultato atteso: ~a~%Risultato ottenuto: ~a~%~%"
                   input expected result)
        when (not (equal expected result))
        do (format t "❌ Test fallito: ~a~%" input)
        finally (format t "~%Test completati.~%")))