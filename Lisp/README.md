
# URILib - Libreria Lisp per il Parsing delle URI

## Descrizione

URILib è una libreria Lisp progettata per analizzare Uniform Resource Identifiers (URI). Fornisce una serie di funzioni che consentono di scomporre una URI nelle sue componenti fondamentali, come schema, informazioni utente, host, porta, percorso, query e frammento. La libreria supporta diversi schemi comuni, inclusi `http`, `https`, `ftp`, `mailto`, `news`, `tel`, `fax` e `zos`.

## Struttura del Codice

Il codice è suddiviso in diverse sezioni, ognuna delle quali si occupa di un aspetto specifico del parsing delle URI:

1. **Parsing dello Schema**: Identifica e estrae lo schema della URI (es. `http`, `ftp`).
2. **Parsing dell’Autorità**: Analizza le informazioni relative all’autorità, inclusi `userinfo`, `host` e `port`.
3. **Parsing del Percorso**: Scompone il percorso della URI.
4. **Parsing del Percorso ZOS**: Gestisce percorsi specifici per lo schema `zos`.
5. **Parsing della Query**: Estrae la query dalla URI.
6. **Parsing del Frammento**: Identifica e estrae il frammento della URI.
7. **Validazione**: Controlla la validità delle diverse componenti della URI, come `host` e `porta`.
8. **Funzioni di Visualizzazione**: Consente di visualizzare le componenti di una URI.

## Utilizzo

### Caricamento della Libreria

Per utilizzare URILib, carica il file Lisp nel tuo ambiente di sviluppo Lisp:

```lisp
(load "uri-parser.lisp")
```

### Parsing di una URI

La funzione principale per il parsing è `parse-uri`. Accetta una stringa rappresentante una URI e restituisce una struttura che contiene le componenti estratte.

**Sintassi:**

```lisp
(parse-uri uri-string)
```

**Esempio:**

```lisp
(let ((result (parse-uri "http://user@server.example.org:8080/path/to/resource?key=value#section1")))
  (print result))
```

Output:

```lisp
(:SCHEMA "http"
 :USERINFO "user"
 :HOST "server.example.org"
 :PORT 8080
 :PATH "/path/to/resource"
 :QUERY "key=value"
 :FRAGMENT "section1")
```

### Visualizzazione delle Componenti della URI

La libreria fornisce una funzione per visualizzare le componenti di una URI:

```lisp
(display-uri uri-struct)
```

**Esempio:**

```lisp
(let ((uri (parse-uri "http://disco.unimib.it")))
  (display-uri uri))
```

Output:

```
Schema: http
Informazioni utente: N/A
Host: disco.unimib.it
Porta: 80
Percorso: N/A
Query: N/A
Fragmento: N/A
```

## Requisiti

- **Common Lisp**: La libreria è compatibile con i principali ambienti Lisp, come SBCL e Clisp.

## Autori

- **Cristiano Rotunno**, Matricola: `""`
- **Alessandro Rutigliano**, Matricola: `""`

## Licenza

Questa libreria è distribuita sotto licenza MIT.
