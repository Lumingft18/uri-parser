URILib - Libreria Prolog per il Parsing delle URI

Descrizione

URILib è una libreria Prolog progettata per analizzare Uniform Resource Identifiers (URI). Fornisce una serie di predicati che permettono di scomporre una URI nelle sue componenti fondamentali, come schema, informazioni utente, host, porta, percorso, query e frammento. La libreria supporta diversi schemi comuni, inclusi http, https, ftp, mailto, news, tel, fax e zos.

Struttura del Codice

Il codice è suddiviso in diverse sezioni, ognuna delle quali si occupa di un aspetto specifico del parsing delle URI:
	1.Parsing dello Schema: Identifica e estrae lo schema della URI (es. http, ftp).
	2.Parsing dell’Autorità: Analizza le informazioni relative all’autorità, inclusi userinfo, host e port.
	3.Parsing del Percorso: Scompone il percorso della URI.
	4.Parsing del Percorso ZOS: Gestisce percorsi specifici per lo schema zos.
	5.Parsing della Query: Estrae la query dalla URI.
	6.Parsing del Frammento: Identifica e estrae il frammento della URI.
	7.Validazione: Controlla la validità delle diverse componenti della URI, come host e porta.
	8.Grammatica: Definisce le regole grammaticali utilizzate nel parsing, inclusi identificatori, caratteri validi, lettere e cifre.
	9.Predicati di Visualizzazione: Fornisce predicati per visualizzare le componenti di una URI sia sullo schermo che su un file.

Utilizzo:

Caricamento della Libreria:

Per utilizzare URILib, carica il file Prolog nel tuo ambiente di sviluppo Prolog:

?- [urilib].

Parsing di una URI:

Il predicato principale per il parsing è urilib_parse/2. Accetta una stringa rappresentante una URI e restituisce una struttura uri/7 con le componenti estratte.

Sintassi:

urilib_parse(+URIString, -URI).

Esempio:

?- urilib_parse('http://user@server.example.org:8080/path/to/resource?key=value#section1', URI).
URI = uri("http", "user", "server.example.org", 8080, "path/to/resource", "key=value", "section1").

Visualizzazione delle Componenti della URIL:

La libreria fornisce due predicati per visualizzare le componenti di una URI:

	1.Sullo Schermo:
urilib_display(+URI).

Esempio:

?- urilib_parse('http://disco.unimib.it', URI), urilib_display(URI).
Schema: http
Informazioni utente: N/A
Host: disco.unimib.it
Porta: 80
Percorso: N/A
Query: N/A
Fragmento: N/A
true.

	2.Su un File:
urilib_display(+URI, +Stream).

Esempio:

?- urilib_parse('http://disco.unimib.it', URI),
      open('output.txt', write, Stream),
      urilib_display(URI, Stream),
      close(Stream).

Requisiti:
	•Prolog: La libreria è compatibile con i principali interpreti Prolog, come SWI-Prolog.

Autori:
	•Cristiano Rotunno, Matricola: “”
	•Alessandro Rutigliano, Matricola: “”

Licenza:

Questa libreria è distribuita sotto licenza MIT. 

