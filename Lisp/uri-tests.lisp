;;; uri-tests.lisp
;;; File per eseguire test sul parser URI

(defpackage :uri-tests
  (:use :cl)
  (:export :run-uri-tests)) ;; Esporta la funzione per eseguire i test

(in-package :uri-tests)

;; Carica il parser URI se non è già stato caricato
;; Modifica il percorso del file secondo la tua configurazione
(load "/Users/alessandrorutigliano/Desktop/Uni/Rotunno_Rutigliano_Cristiano_Alessandro_914317_909971_LP_E1P_2025/Lisp/uri-parser.lisp")

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

    (:input "http://www.ietf.org/rfc/rfc2396txt"
     :scheme "http"
     :userinfo nil
     :host "www.ietf.org"
     :port 80
     :path "rfc/rfc2396txt"
     :query nil
     :fragment nil)

    ;; Testset "Standard Scheme (https)"
    (:input "https://example.com"
     :scheme "https"
     :userinfo nil
     :host "example.com"
     :port 80
     :path nil
     :query nil
     :fragment nil)

    (:input "https://www.ietf.org/rfc/rfc2396txt"
     :scheme "https"
     :userinfo nil
     :host "www.ietf.org"
     :port 80
     :path "rfc/rfc2396txt"
     :query nil
     :fragment nil)

    ;; Testset "FTP Scheme"
    (:input "ftp://ftp.example.com/files"
     :scheme "ftp"
     :userinfo nil
     :host "ftp.example.com"
     :port 80
     :path "files"
     :query nil
     :fragment nil)

    (:input "ftp://ftp.is.co.za/rfc/rfc1808txt"
     :scheme "ftp"
     :userinfo nil
     :host "ftp.is.co.za"
     :port 80
     :path "rfc/rfc1808txt"
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

    (:input "mailto:JohnDoe@example.com"
     :scheme "mailto"
     :userinfo "JohnDoe"
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

    (:input "zos:INVALID."
     :error "Terminazione non valida")

    (:input "zos:123.DEF"
     :error "ID44 deve iniziare con una lettera")

    ;; Altri Test aggiuntivi dal codice esistente
    (:input "https://unimib.it"
     :scheme "https"
     :userinfo nil
     :host "unimib.it"
     :port 80
     :path nil
     :query nil
     :fragment nil)

    (:input "https:///unimib.it"
     :error "Troppi /")

    (:input "ftp://123example.com"
     :error "host lettera")

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
     :error "host lettera")

    (:input "https://.com"
     :error "Host invalido (non può iniziare con '.')")

    (:input "https://com"
     :scheme "https"
     :userinfo nil
     :host "com"
     :port 80
     :path nil
     :query nil
     :fragment nil)

    (:input "https://:/path"
     :error "Porta mancante dopo il `:`")

    (:input "mailto:user@"
     :error "la @")

    (:input "fax:"
     :error "Fax senza numero")

    (:input "mailto:"
     :error "ci deve essere per forza uno user info")

    (:input "http://[::1]"
     :error "host non valido")

    (:input "http://[::1]:invalid"
     :error "Porta non valida")

    (:input "http://-invalid.com"
     :error "Host con carattere iniziale invalido")

    (:input "http://example.com:-80"
     :error "Porta negativa")

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

    ;; -------------------------------------------------
    ;; Aggiunta di nuovi test VALID
    ;; -------------------------------------------------

    ;; Testset "Valid URLs aggiuntivi"

    ;; Valid https URLs
    (:input "https://user1@google.com"
     :scheme "https"
     :userinfo "user1"
     :host "google.com"
     :port 80
     :path nil
     :query nil
     :fragment nil)

    (:input "https://google.com"
     :scheme "https"
     :userinfo nil
     :host "google.com"
     :port 80
     :path nil
     :query nil
     :fragment nil)

    (:input "https://user1@google.com:123"
     :scheme "https"
     :userinfo "user1"
     :host "google.com"
     :port 123
     :path nil
     :query nil
     :fragment nil)

    (:input "https://user1@google.com.c81z:123"
     :scheme "https"
     :userinfo "user1"
     :host "google.com.c81z"
     :port 123
     :path nil
     :query nil
     :fragment nil)

    (:input "https://user1@1.234.12.98"
     :scheme "https"
     :userinfo "user1"
     :host "1.234.12.98"
     :port 80
     :path nil
     :query nil
     :fragment nil)

    (:input "https://user1@1.234.12.98:98"
     :scheme "https"
     :userinfo "user1"
     :host "1.234.12.98"
     :port 98
     :path nil
     :query nil
     :fragment nil)

    (:input "https://user1@g.c.c:45"
     :scheme "https"
     :userinfo "user1"
     :host "g.c.c"
     :port 45
     :path nil
     :query nil
     :fragment nil)

    (:input "https://google.com:123"
     :scheme "https"
     :userinfo nil
     :host "google.com"
     :port 123
     :path nil
     :query nil
     :fragment nil)

    (:input "https:/"
     :scheme "https"
     :userinfo nil
     :host nil
     :port 80
     :path nil
     :query nil
     :fragment nil)

    (:input "https:/path/to/res"
     :scheme "https"
     :userinfo nil
     :host nil
     :port 80
     :path "path/to/res"
     :query nil
     :fragment nil)

    (:input "https:/?query=value"
     :scheme "https"
     :userinfo nil
     :host nil
     :port 80
     :path nil
     :query "query=value"
     :fragment nil)

    (:input "https:/?query=value#frag"
     :scheme "https"
     :userinfo nil
     :host nil
     :port 80
     :path nil
     :query "query=value"
     :fragment "frag")

    (:input "https:/#frag"
     :scheme "https"
     :userinfo nil
     :host nil
     :port 80
     :path nil
     :query nil
     :fragment "frag")

    (:input "https:/path/to/res?query=value"
     :scheme "https"
     :userinfo nil
     :host nil
     :port 80
     :path "path/to/res"
     :query "query=value"
     :fragment nil)

    (:input "https:/path/to/res#frag"
     :scheme "https"
     :userinfo nil
     :host nil
     :port 80
     :path "path/to/res"
     :query nil
     :fragment "frag")

    (:input "https:/path/to/res?query=value#frag"
     :scheme "https"
     :userinfo nil
     :host nil
     :port 80
     :path "path/to/res"
     :query "query=value"
     :fragment "frag")

    (:input "https:"
     :scheme "https"
     :userinfo nil
     :host nil
     :port 80
     :path nil
     :query nil
     :fragment nil)

    (:input "https:path/to/res"
     :scheme "https"
     :userinfo nil
     :host nil
     :port 80
     :path "path/to/res"
     :query nil
     :fragment nil)

    (:input "https:?query=value"
     :scheme "https"
     :userinfo nil
     :host nil
     :port 80
     :path nil
     :query "query=value"
     :fragment nil)

    (:input "https:#frag"
     :scheme "https"
     :userinfo nil
     :host nil
     :port 80
     :path nil
     :query nil
     :fragment "frag")

    (:input "https:path/to/res?query=value"
     :scheme "https"
     :userinfo nil
     :host nil
     :port 80
     :path "path/to/res"
     :query "query=value"
     :fragment nil)

    (:input "https:path/to/res#frag"
     :scheme "https"
     :userinfo nil
     :host nil
     :port 80
     :path "path/to/res"
     :query nil
     :fragment "frag")

    (:input "https:path/to/res?query=value#frag"
     :scheme "https"
     :userinfo nil
     :host nil
     :port 80
     :path "path/to/res"
     :query "query=value"
     :fragment "frag")

    (:input "https:?query=value#frag"
     :scheme "https"
     :userinfo nil
     :host nil
     :port 80
     :path nil
     :query "query=value"
     :fragment "frag")

    (:input "https:google.com/"
     :scheme "https"
     :userinfo nil
     :host "google.com"
     :port 80
     :path "/"
     :query nil
     :fragment nil)

    (:input "https:google.comgoogle.com/path/to/res"
     :scheme "https"
     :userinfo nil
     :host "google.comgoogle.com"
     :port 80
     :path "path/to/res"
     :query nil
     :fragment nil)

    (:input "https:google.com/?query=value"
     :scheme "https"
     :userinfo nil
     :host "google.com"
     :port 80
     :path "/"
     :query "query=value"
     :fragment nil)

    (:input "https:google.com/?query=value#frag"
     :scheme "https"
     :userinfo nil
     :host "google.com"
     :port 80
     :path "/"
     :query "query=value"
     :fragment "frag")

    (:input "https:google.com/#frag"
     :scheme "https"
     :userinfo nil
     :host "google.com"
     :port 80
     :path "/"
     :query nil
     :fragment "frag")

    (:input "https:google.com/path/to/res?query=value"
     :scheme "https"
     :userinfo nil
     :host "google.com"
     :port 80
     :path "path/to/res"
     :query "query=value"
     :fragment nil)

    (:input "https:google.com/path/to/res#frag"
     :scheme "https"
     :userinfo nil
     :host "google.com"
     :port 80
     :path "path/to/res"
     :query nil
     :fragment "frag")

    (:input "https:google.com/path/to/res?query=value#frag"
     :scheme "https"
     :userinfo nil
     :host "google.com"
     :port 80
     :path "path/to/res"
     :query "query=value"
     :fragment "frag")

    ;; Valid ZOS URLs
    (:input "zos://user1@google.com"
     :scheme "zos"
     :userinfo "user1"
     :host "google.com"
     :port 80
     :path nil
     :query nil
     :fragment nil)

    (:input "zos://google.com"
     :scheme "zos"
     :userinfo nil
     :host "google.com"
     :port 80
     :path nil
     :query nil
     :fragment nil)

    (:input "zos://user1@google.com:123"
     :scheme "zos"
     :userinfo "user1"
     :host "google.com"
     :port 123
     :path nil
     :query nil
     :fragment nil)

    (:input "zos://user1@google.com.c81z:123"
     :scheme "zos"
     :userinfo "user1"
     :host "google.com.c81z"
     :port 123
     :path nil
     :query nil
     :fragment nil)

    (:input "zos://user1@1.234.12.98"
     :scheme "zos"
     :userinfo "user1"
     :host "1.234.12.98"
     :port 80
     :path nil
     :query nil
     :fragment nil)

    (:input "zos://user1@1.234.12.98:98"
     :scheme "zos"
     :userinfo "user1"
     :host "1.234.12.98"
     :port 98
     :path nil
     :query nil
     :fragment nil)

    (:input "zos://user1@g.c.c:45"
     :scheme "zos"
     :userinfo "user1"
     :host "g.c.c"
     :port 45
     :path nil
     :query nil
     :fragment nil)

    (:input "zos://google.com:123"
     :scheme "zos"
     :userinfo nil
     :host "google.com"
     :port 123
     :path nil
     :query nil
     :fragment nil)

    (:input "zos:/"
     :scheme "zos"
     :userinfo nil
     :host nil
     :port 80
     :path nil
     :query nil
     :fragment nil)

    (:input "zos:/PATH.ZOS.EXAMPLE(CIAO)"
     :scheme "zos"
     :userinfo nil
     :host nil
     :port 80
     :path "PATH.ZOS.EXAMPLE(CIAO)"
     :query nil
     :fragment nil)

    (:input "zos:/?query=value"
     :scheme "zos"
     :userinfo nil
     :host nil
     :port 80
     :path nil
     :query "query=value"
     :fragment nil)

    (:input "zos:/?query=value#frag"
     :scheme "zos"
     :userinfo nil
     :host nil
     :port 80
     :path nil
     :query "query=value"
     :fragment "frag")

    (:input "zos:/#frag"
     :scheme "zos"
     :userinfo nil
     :host nil
     :port 80
     :path nil
     :query nil
     :fragment "frag")

    (:input "zos:/PATH.ZOS.EXAMPLE(CIAO)?q=test"
     :scheme "zos"
     :userinfo nil
     :host nil
     :port 80
     :path "PATH.ZOS.EXAMPLE(CIAO)"
     :query "q=test"
     :fragment nil)

    (:input "zos:/PATH.ZOS.EXAMPLE(CIAO)#frag"
     :scheme "zos"
     :userinfo nil
     :host nil
     :port 80
     :path "PATH.ZOS.EXAMPLE(CIAO)"
     :query nil
     :fragment "frag")

    (:input "zos:/PATH.ZOS.EXAMPLE(CIAO)?q=test#frag"
     :scheme "zos"
     :userinfo nil
     :host nil
     :port 80
     :path "PATH.ZOS.EXAMPLE(CIAO)"
     :query "q=test"
     :fragment "frag")

    (:input "zos:"
     :scheme "zos"
     :userinfo nil
     :host nil
     :port 80
     :path nil
     :query nil
     :fragment nil)

    (:input "zos:PATH.ZOS.EXAMPLE(CIAO)"
     :scheme "zos"
     :userinfo nil
     :host nil
     :port 80
     :path "PATH.ZOS.EXAMPLE(CIAO)"
     :query nil
     :fragment nil)

    (:input "zos:?query=value"
     :scheme "zos"
     :userinfo nil
     :host nil
     :port 80
     :path nil
     :query "query=value"
     :fragment nil)

    (:input "zos:#frag"
     :scheme "zos"
     :userinfo nil
     :host nil
     :port 80
     :path nil
     :query nil
     :fragment "frag")

    (:input "zos:PATH.ZOS.EXAMPLE(CIAO)?q=test"
     :scheme "zos"
     :userinfo nil
     :host nil
     :port 80
     :path "PATH.ZOS.EXAMPLE(CIAO)"
     :query "q=test"
     :fragment nil)

    (:input "zos:PATH.ZOS.EXAMPLE(CIAO)#frag"
     :scheme "zos"
     :userinfo nil
     :host nil
     :port 80
     :path "PATH.ZOS.EXAMPLE(CIAO)"
     :query nil
     :fragment "frag")

    (:input "zos:PATH.ZOS.EXAMPLE(CIAO)?q=test#frag"
     :scheme "zos"
     :userinfo nil
     :host nil
     :port 80
     :path "PATH.ZOS.EXAMPLE(CIAO)"
     :query "q=test"
     :fragment "frag")

    (:input "zos:?query=value#frag"
     :scheme "zos"
     :userinfo nil
     :host nil
     :port 80
     :path nil
     :query "query=value"
     :fragment "frag")

    (:input "zos:google.com/"
     :scheme "zos"
     :userinfo nil
     :host "google.com"
     :port 80
     :path "/"
     :query nil
     :fragment nil)

    (:input "zos:google.comgoogle.com/PATH.ZOS.EXAMPLE(CIAO)"
     :scheme "zos"
     :userinfo nil
     :host "google.comgoogle.com"
     :port 80
     :path "PATH.ZOS.EXAMPLE(CIAO)"
     :query nil
     :fragment nil)

    (:input "zos:google.com/?query=value"
     :scheme "zos"
     :userinfo nil
     :host "google.com"
     :port 80
     :path "/"
     :query "query=value"
     :fragment nil)

    (:input "zos:google.com/?query=value#frag"
     :scheme "zos"
     :userinfo nil
     :host "google.com"
     :port 80
     :path "/"
     :query "query=value"
     :fragment "frag")

    (:input "zos:google.com/#frag"
     :scheme "zos"
     :userinfo nil
     :host "google.com"
     :port 80
     :path "/"
     :query nil
     :fragment "frag")

    (:input "zos:google.com/PATH.ZOS.EXAMPLE(CIAO)?q=test"
     :scheme "zos"
     :userinfo nil
     :host "google.com"
     :port 80
     :path "PATH.ZOS.EXAMPLE(CIAO)"
     :query "q=test"
     :fragment nil)

    (:input "zos:google.com/PATH.ZOS.EXAMPLE(CIAO)#frag"
     :scheme "zos"
     :userinfo nil
     :host "google.com"
     :port 80
     :path "PATH.ZOS.EXAMPLE(CIAO)"
     :query nil
     :fragment "frag")

    (:input "zos:google.com/PATH.ZOS.EXAMPLE(CIAO)?q=test#frag"
     :scheme "zos"
     :userinfo nil
     :host "google.com"
     :port 80
     :path "PATH.ZOS.EXAMPLE(CIAO)"
     :query "q=test"
     :fragment "frag")

    ;; Valid mailto URLs
    (:input "mailto:user"
     :scheme "mailto"
     :userinfo "user"
     :host nil
     :port nil
     :path nil
     :query nil
     :fragment nil)

    (:input "mailto:user@google.com"
     :scheme "mailto"
     :userinfo "user"
     :host "google.com"
     :port nil
     :path nil
     :query nil
     :fragment nil)

    (:input "mailto:user@192.12.124.21"
     :scheme "mailto"
     :userinfo "user"
     :host "192.12.124.21"
     :port nil
     :path nil
     :query nil
     :fragment nil)

    ;; Valid news URLs
    (:input "news:google"
     :scheme "news"
     :userinfo nil
     :host "google"
     :port nil
     :path nil
     :query nil
     :fragment nil)

    (:input "news:google.com"
     :scheme "news"
     :userinfo nil
     :host "google.com"
     :port nil
     :path nil
     :query nil
     :fragment nil)

    (:input "news:192.12.124.21"
     :scheme "news"
     :userinfo nil
     :host "192.12.124.21"
     :port nil
     :path nil
     :query nil
     :fragment nil)

    ;; Valid tel URLs
    (:input "tel:+39-3209439015"
     :scheme "tel"
     :userinfo "+39-3209439015"
     :host nil
     :port nil
     :path nil
     :query nil
     :fragment nil)

    (:input "tel:giacomo"
     :scheme "tel"
     :userinfo "giacomo"
     :host nil
     :port nil
     :path nil
     :query nil
     :fragment nil)

    ;; Valid fax URLs
    (:input "fax:+39-3209439015"
     :scheme "fax"
     :userinfo "+39-3209439015"
     :host nil
     :port nil
     :path nil
     :query nil
     :fragment nil)

    (:input "fax:giacomo"
     :scheme "fax"
     :userinfo "giacomo"
     :host nil
     :port nil
     :path nil
     :query nil
     :fragment nil)

    ;; -------------------------------------------------
    ;; Aggiunta di nuovi test INVALID
    ;; -------------------------------------------------

    ;; Testset "Invalid URLs aggiuntivi"

    ;; Invalid https URLs
    (:input "https://user1@1.234.12.256"
     :error "Valore IP fuori range: 256")

    (:input "https://user1@1.12.98:98"
     :error "IP incompleto, mancano ottetti")

    (:input "https://user1@google.1com.com:123"
     :error "Host con carattere invalido")

    (:input "https://user1@1oogle.com:123"
     :error "Host non valido, inizia con un numero")

    (:input "https://user1@:123"
     :error "Host mancante dopo l'@")

    (:input "https://user1@google.com:abc"
     :error "Porta non valida: abc")

    (:input "https://user1@google.com:"
     :error "Porta mancante dopo il `:`")

    (:input "https:user1@"
     :error "Formato URL non valido")

    (:input "https://user1@"
     :error "Host mancante dopo l'@")

    (:input ":user@google.com"
     :error "Schema mancante")

    (:input ":"
     :error "URL vuoto o schema mancante")

    (:input ":google.com"
     :error "Schema mancante")

    (:input ":/path/to/resource"
     :error "Schema mancante")

    ;; Invalid ZOS URLs
    (:input "zos:/PATH.ZOS.EXAMPLE(CIAO)"
     :error "Path ZOS non valido senza host o userinfo")

    (:input "zos:(CIAO)?query=value"
     :error "Formato ZOS non valido")

    (:input "zos://user1@1.234.12.256"
     :error "Valore IP fuori range: 256")

    (:input "zos://user1@1.12.98:98"
     :error "IP incompleto, mancano ottetti")

    (:input "zos://user1@google.1com.com:123"
     :error "Host con carattere invalido")

    (:input "zos://user1@1oogle.com:123"
     :error "Host non valido, inizia con un numero")

    (:input "zos://user1@:123"
     :error "Host mancante dopo l'@")

    (:input "zos://user1@google.com:abc"
     :error "Porta non valida: abc")

    (:input "zos://user1@google.com:"
     :error "Porta mancante dopo il `:`")

    (:input "zos:user1@"
     :error "Formato ZOS non valido")

    (:input "zos://user1@"
     :error "Host mancante dopo l'@")

    ;; Invalid mailto URLs
    (:input "mailto:"
     :error "ci deve essere per forza uno user info")

    (:input "mailto:user@"
     :error "Formato mailto non valido")

    (:input "mailto:@google.com"
     :error "User info mancante prima della @")

    (:input "mailto:user@google.com/path/to/res"
     :error "Path non valido per mailto")

    (:input "mailto:user@google.com?query"
     :error "Query non valida per mailto")

    (:input "mailto:user@google.com#fragment"
     :error "Fragment non valido per mailto")

    ;; Invalid news URLs
    (:input "news:user@google"
     :error "Formato news non valido")

    (:input "news:user@google.com"
     :error "Formato news non valido")

    (:input "news:user@192.12.124.21"
     :error "Formato news non valido")

    (:input "mailto:google.com/path/to/res"
     :error "Path non valido per mailto")

    (:input "mailto:google.com?query"
     :error "Query non valida per mailto")

    (:input "mailto:google.com#fragment"
     :error "Fragment non valido per mailto")

    ;; Invalid tel URLs
    (:input "tel:+39-320943#9015"
     :error "Caratteri non validi nel numero di telefono")

    (:input "tel:giacomo@host"
     :error "Formato tel non valido con @")

    (:input "tel:host.com"
     :error "Formato tel non valido, atteso un numero")

    (:input "tel:192.12.124.21"
     :error "Formato tel non valido, atteso un numero")

    ;; Invalid fax URLs
    (:input "fax:+39-320943#9015"
     :error "Caratteri non validi nel numero di fax")

    (:input "fax:giacomo@host"
     :error "Formato fax non valido con @")

    (:input "fax:host.com"
     :error "Formato fax non valido, atteso un numero")

    (:input "fax:192.12.124.21"
     :error "Formato fax non valido, atteso un numero")
  ))

;;; Funzione per eseguire un singolo test
(defun run-single-test (test)
  (let ((input (getf test :input))
        (expected-scheme (getf test :scheme))
        (expected-userinfo (getf test :userinfo))
        (expected-host (getf test :host))
        (expected-port (getf test :port))
        (expected-path (getf test :path))
        (expected-query (getf test :query))
        (expected-fragment (getf test :fragment))
        (expected-error (getf test :error)))
    (handler-case
        ;; Chiama la funzione del parser sul test :input
        (let ((parsed-uri (urilib-parse input)))
          (if expected-error
              ;; Se è atteso un errore ma il parser non lo solleva, il test fallisce
              (values nil (format nil "Expected error but parsing succeeded: ~a" input))
              ;; Altrimenti confronta i valori attesi
              (if (and (equal (urilib-scheme parsed-uri) expected-scheme)
                       (equal (urilib-userinfo parsed-uri) expected-userinfo)
                       (equal (urilib-host parsed-uri) expected-host)
                       (equal (urilib-port parsed-uri) expected-port)
                       (equal (urilib-path parsed-uri) expected-path)
                       (equal (urilib-query parsed-uri) expected-query)
                       (equal (urilib-fragment parsed-uri) expected-fragment))
                  (values t nil) ; Test passato
                  (values nil "Mismatch in expected vs. actual output")))) ; Test fallito
      (error (e)
        ;; Se un errore è previsto e si verifica, il test è considerato riuscito
        (if expected-error
            (values t nil) ; Test passato
            (values nil (format nil "Error during parsing: ~a" e))))))) ; Test fallito

;;; Funzione per eseguire tutti i test
(defun run-uri-tests ()
  (format t "Esecuzione dei test URI...~%")
  (let ((success-count 0)
        (failure-count 0))
    (dolist (test *uri-tests*)
      (multiple-value-bind (passed error) (run-single-test test)
        (if passed
            (progn
              (incf success-count)
              (format t "\u2713 Test riuscito: ~a~%" (getf test :input)))
            (progn
              (incf failure-count)
              (format t "\u2717 Test fallito: ~a~%    Errore: ~a~%"
                      (getf test :input)
                      error)))))
    (format t "~%Test completati. Riusciti: ~d, Falliti: ~d~%"
            success-count failure-count)))