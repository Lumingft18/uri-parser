
# URILib - Librerie per il Parsing delle URI

## Descrizione

URILib include due librerie progettate per analizzare Uniform Resource Identifiers (URI), disponibili in Prolog e Lisp. Entrambe le implementazioni permettono di scomporre una URI nelle sue componenti fondamentali, come schema, informazioni utente, host, porta, percorso, query e frammento.

## Librerie Disponibili

1. **URILib Prolog**: Una libreria scritta in Prolog che utilizza predicati per il parsing delle URI. Ideale per chi lavora in ambiente logico e con strumenti Prolog.
   - **Caricamento**: `?- [urilib].`
   - **Funzione principale**: `urilib_parse/2`

2. **URILib Lisp**: Una libreria scritta in Common Lisp per il parsing e la gestione delle URI. Ideale per sviluppatori Lisp.
   - **Caricamento**: `(load "uri-parser.lisp")`
   - **Funzione principale**: `(parse-uri uri-string)`

## Requisiti

- **Prolog**: Compatibile con SWI-Prolog.
- **Common Lisp**: Testato con LispWorks.

## Autori

- **Cristiano Rotunno**, Matricola: `"914317"`
- **Alessandro Rutigliano**, Matricola: `"909971"`

## Licenza

Entrambe le librerie sono distribuite sotto licenza MIT.
