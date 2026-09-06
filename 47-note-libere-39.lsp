================

 NOTE LIBERE 39

================

  "Esiste una sequenza per ogni cosa..."

------------------------
Analisi di lanci di dadi
------------------------

Per calcolare tutti i risultati (come liste) del lancio di N dadi con numero di facce non necessariamente uguali M1, M2,...MN occorre generare il 'prodotto cartesiano' degli insiemi dei possibili valori di ciascun dado.

Se i dadi hanno rispettivamente M1, M2, ..., MN facce, la funzione deve generare tutte le tuple:
  (1 1 ... 1)
  (1 1 ... 2)
  ...
  (M1 M2 ... MN)

Il numero totale dei risultati vale: M1 * M2 * ... * MN.

Esempio:
  Con 2 dadi: D1 = (1 2) e D1 = (1 2 3),
  abbiamo M = (2 3) e si ottiene: ((1 1) (1 2) (1 3) (2 1) (2 2) (2 3)).

(define (cartesian)
"Calculate the cartesian product of lists"
  (let (out '(()) )
    (dolist (lst (args))
      (let (tmp '())
        (dolist (parz out)
          (dolist (el lst)
            (push (append parz (list el)) tmp -1)))
        (setq out tmp)))
    out))

; Analizza il risultato di tutti i possibili lanci di N dadi qualunque
(define (analyze dadi)
  (local (lanci totale-lanci valori unici frequenze)
    ; ordinamento dei dadi
    (setq dadi (sort (map sort dadi)))
    ; lista con tutti i lanci possibili (lista di liste)
    (setq lanci (apply cartesian dadi))
    ; numero totale di lanci
    (setq totale-lanci (length lanci))
    ; lista con i valori di tutti i lanci
    (setq valori (map (fn(x) (apply + x)) lanci))
    ; lista con i valori unici di tutti i lanci
    (setq unici (unique valori))
    ; lista con le frequenze/occorrenze dei valori unici
    (setq frequenze (count unici valori))
    ; lista di output: (numero frequenza totale-lanci probabilità)
    (map (fn(x y) (list x y totale-lanci
                  (float (format "%3.4f" (div y totale-lanci)))))
         unici frequenze)))

Proviamo:

(setq D (sequence 1 6))
(analyze (list D D))
;-> ((2 1 36 0.0278) (3 2 36 0.0556) (4 3 36 0.0833) (5 4 36 0.1111)
;->  (6 5 36 0.1389) (7 6 36 0.1667) (8 5 36 0.1389) (9 4 36 0.1111)
;->  (10 3 36 0.0833) (11 2 36 0.0556) (12 1 36 0.0278))
(sort (analyze (list D D)) (fn(x y) (>= (last x) (last y))))
;-> ((7 6 36 0.1667) (6 5 36 0.1389) (8 5 36 0.1389) (5 4 36 0.1111)
;->  (9 4 36 0.1111) (4 3 36 0.0833) (10 3 36 0.0833) (3 2 36 0.0556)
;->  (11 2 36 0.0556) (2 1 36 0.0278) (12 1 36 0.0278))

(setq D1 '(1 3 5 7))
(setq D2 '(2 4 6 8))
(analyze (list D1 D2))
;-> ((3 1 16 0.0625) (5 2 16 0.125) (7 3 16 0.1875) (9 4 16 0.25)
;->  (11 3 16 0.1875) (13 2 16 0.125) (15 1 16 0.0625))

(setq D1 '(1 2 3))
(setq D2 '(4 5 6))
(setq D3 '(7 8 9))
(analyze (list D1 D2 D3))
;-> ((12 1 27 0.037) (13 3 27 0.1111) (14 6 27 0.2222) (15 7 27 0.2593)
;->  (16 6 27 0.2222) (17 3 27 0.1111) (18 1 27 0.037))

(setq D1 '(1 5 9))
(setq D2 '(2 4 8))
(setq D3 '(3 6 7))
(analyze (list D1 D2 D3))
;-> ((6 1 27 0.037) (9 1 27 0.037) (10 2 27 0.0741) (8 1 27 0.037)
;->  (11 1 27 0.037) (12 3 27 0.1111) (15 2 27 0.0741) (16 4 27 0.1481)
;->  (13 1 27 0.037) (14 2 27 0.0741) (19 2 27 0.0741) (20 3 27 0.1111)
;->  (17 1 27 0.037) (18 1 27 0.037) (23 1 27 0.037) (24 1 27 0.037))


------------------------
Istogramma di intervalli
------------------------

Data una lista di coppie di interi (L R) (intervallo di numeri da L a R), contare il numero di coppie univoche che si intersecano.
Una coppia (L1 R1) interseca la coppia (L2 R2) se L1 <= L2 <= R1 o L2 <= L1 <= R2.
Inoltre risulta sempre: L <= R per una singola coppia.
Vogliamo conoscere quanti intervalli (segmenti) si intersecano in ogni punto intero (istogramma) e il numero di coppie uniche di intervalli che si intersecano.
Nota: gli intervalli che hanno un estremo in comune si intersecano (Es. (1 3) (3 5))

Per esempio:
Intervalli = ((2 8) (11 19) 
              (7 14) (17 27)
              (5 10) (13 18) (25 27) (29 30)
              (1 3) (5 14) (27 30))

Graficamente:

12345678901234567890123456789
 -------  ---------
      --------  -----------
    ------  ------      --- --
--- ----------            ----
12345678901234567890123456789

Numero di segmenti intersecanti a ogni posizione:
122133443333442232111111223122

Istogramma = 1 2 2 1 3 3 4 4 3 3 3 3 4 4 2 2 3 2 1 1 1 1 1 1 2 2 3 1 2 2

Numero di coppie uniche di intervalli che si intersecano = 18

-----------------------
A) Metodo "Brute-Force"
-----------------------
Per ogni punto intero da 1 al valore massimo degli intervalli (max-point), contiamo quanti intervalli della lista lo contengono.
Per ogni posizione 'i' si scandisce l'intera lista degli intervalli (nessuna ottimizzazione).
Complessità: O(max-point * n).

; **********************************
; 1) Copertura per ogni punto intero
; **********************************

; Verifica se una coppia (intervallo) contiene un punto
(define (contains? coppia punto)
  (and (<= (coppia 0) punto) (<= punto (coppia 1))))

(contains? '(5 14) 5)
;-> true
(contains? '(5 14) 15)
;-> nil

; ---------------------------------------------------------------------------
; Calcola la copertura degli intervalli per ogni punto intero
; Versione Brute-Force unificata a doppia modalita'.
;
; Il parametro 'values' funziona da interruttore tra due comportamenti:
;   - values = true  -> per ogni punto, restituisce la LISTA degli intervalli
;                        che lo contengono (il dettaglio, "quali")
;   - values = nil   -> per ogni punto, restituisce il NUMERO di intervalli
;                        che lo contengono (l'aggregato, "quanti")
;
; E' la naturale unione di due funzioni che farebbero la stessa scansione:
; invece di duplicare il doppio ciclo, si condivide la struttura e si
; cambia solo cosa viene accumulato nel ramo interno.
;
; Complessita': O(max-point * n), dove n = (length lst).
; A differenza della versione ottimizzata con diff array (che scende a
; O(n + max-point) ma puo' calcolare SOLO il conteggio, mai il dettaglio),
; qui per ogni singolo punto si riscandisce sempre l'intera lista di
; intervalli da capo: e' il prezzo da pagare per poter, se richiesto,
; sapere anche "quali" intervalli contribuiscono, non solo "quanti".
; ---------------------------------------------------------------------------
(define (coverage lst values)
  (let (
        coverage '()     ; lista finale: un elemento per ogni punto scandito,
                          ; costruita in ordine di posizione crescente
        conta 0           ; contatore di intervalli attivi nel punto corrente
                          ; (usato solo quando values = nil)
        cur-point '()     ; lista di intervalli attivi nel punto corrente
                          ; (usata solo quando values = true)
        max-point (apply max (flat lst)) ; il punto piu' a destra possibile:
                          ; nessun intervallo puo' estendersi oltre il piu'
                          ; grande valore R presente nei dati, quindi non
                          ; serve scandire oltre questo limite
        )
    ; --- Scansione di OGNI punto intero da 1 a max-point ---
    ; Nota: si parte sempre da 1, non da min-point (vedi discussione a
    ; parte sulla traslazione): eventuali punti prima del primo intervallo
    ; vengono comunque scanditi e risultano semplicemente a copertura 0.
    (for (pos 1 max-point)
      ; Reset degli accumulatori per il punto corrente: ad ogni nuova
      ; posizione si riparte da zero, perche' il calcolo per ogni punto
      ; e' indipendente da quello dei punti precedenti (non c'e' nessuna
      ; forma di somma progressiva/prefissa, a differenza del diff array).
      (setq conta 0)
      (setq cur-point '())
      ; --- Scansione di OGNI intervallo della lista, per il punto corrente ---
      ; Questo e' la base del costo O(max-point * n): per ciascuno dei
      ; max-point punti, si ripete un ciclo su tutti gli n intervalli.
      (dolist (el lst)
        (if (contains? el pos)   ; el = (L R); contains? verifica L <= pos <= R
          (if values
             (push el cur-point -1)  ; modalita' dettaglio: accumula
                                     ; l'intervallo stesso (in coda, per
                                     ; mantenere l'ordine della lista lst)
             ;else
             (++ conta))))           ; modalita' conteggio: incrementa
                                     ; solo il numero di intervalli attivi
      ; --- Salvataggio del risultato per il punto corrente ---
      ; A seconda della modalita', si accoda alla lista finale o la lista
      ; di intervalli attivi (dettaglio) o il semplice numero (conteggio).
      (if values
        (push cur-point coverage -1)
        ;else
        (push conta coverage -1)))
    coverage))

; Calcola la copertura degli intervalli per ogni punto intero
; Versione senza commenti
(define (coverage lst values)
  (let ( (coverage '())
         (conta 0)
         (cur-point '())
         (max-point (apply max (flat lst))) ) 
    (for (pos 1 max-point)
      (setq conta 0)
      (setq cur-point '())
      (dolist (el lst)
        (if (contains? el pos)
          (if values 
             (push el cur-point -1)
             (++ conta))))
      (if values
        (push cur-point coverage -1)
        (push conta coverage -1)))
    coverage))

Proviamo:

(setq intervals '((2 8) (11 19)
                  (7 14) (17 27)
                  (5 10) (13 18) (25 27) (29 30)
                  (1 3) (5 14) (27 30)))
                  
(coverage intervals)
;-> (1 2 2 1 3 3 4 4 3 3 3 3 4 4 2 2 3 3 2 1 1 1 1 1 2 2 3 1 2 2)
(map length (coverage intervals true))
;-> (1 2 2 1 3 3 4 4 3 3 3 3 4 4 2 2 3 3 2 1 1 1 1 1 2 2 3 1 2 2)
(coverage intervals true)
;-> (((1 3)) 
;->  ((2 8) (1 3))
;->  ((2 8) (1 3))
;->  ((2 8))
;->  ((2 8) (5 10) (5 14))
;->  ((2 8) (5 10) (5 14))
;->  ((2 8) (7 14) (5 10) (5 14))
;->  ...
;->  ((17 27) (25 27) (27 30))
;->  ((27 30))
;->  ((29 30) (27 30))
;->  ((29 30) (27 30)))

(setq intervals2 '((2 3) (2 4)
                   (6 9) (7 8)
                   (15 20) (16 19) (18 22)
                   (30 33) (31 35)
                   (38 40) (39 39)))

(coverage intervals2)
;-> (0 2 2 1 0 1 2 2 1 0 0 0 0 0 1 2 2 3 3 2 1 1 0 0 0 0 0 0 0 1 2 2 2 1 1 0 0 1 2 1)
(map length (coverage intervals2 true))
;-> (0 2 2 1 0 1 2 2 1 0 0 0 0 0 1 2 2 3 3 2 1 1 0 0 0 0 0 0 0 1 2 2 2 1 1 0 0 1 2 1)
(coverage intervals2 true)
;-> (() ((2 3) (2 4)) ((2 3) (2 4)) ((2 4)) () ((6 9)) ((6 9) (7 8))
;->  ((6 9) (7 8)) ((6 9))
;->  () () () () ()
;->  ((15 20))
;->  ((15 20) (16 19))
;->  ((15 20) (16 19))
;->  ((15 20) (16 19) (18 22))
;->  ((15 20) (16 19) (18 22))
;->  ((15 20) (18 22))
;->  ((18 22))
;->  ((18 22))
;->  () () () () () () ()
;->  ((30 33))
;->  ((30 33) (31 35))
;->  ((30 33) (31 35))
;->  ((30 33) (31 35))
;->  ((31 35))
;->  ((31 35))
;->  () ()
;->  ((38 40))
;->  ((38 40) (39 39))
;->  ((38 40)))

; ***********************************************************
; 2) Numero di coppie uniche di intervalli che si intersecano
; ***********************************************************

; Verifica se due coppie si intersecano
(define (cross? c1 c2)
  (or (and (<= (c1 0) (c2 0)) (<= (c2 0) (c1 1)))
      (and (<= (c2 0) (c1 0)) (<= (c1 0) (c2 1)))))

(cross? '(2 8) '(7 14))
;-> true
(cross? '(1 3) '(5 14))
;-> nil
(cross? '(1 3) '(3 5))
;-> true

; ---------------------------------------------------------------------------
; Calcola le coppie univoche di intervalli che si intersecano
; Versione Brute-Force unificata a doppia modalita'.
;
;   - values = true  -> restituisce la LISTA delle coppie di intervalli
;                        che si intersecano (il dettaglio, "quali coppie")
;   - values = nil   -> restituisce il NUMERO di coppie di intervalli
;                        che si intersecano (l'aggregato, "quante coppie")
;
; STRATEGIA: doppio ciclo annidato che confronta ESPLICITAMENTE ogni
; possibile coppia (i, j) con i < j, esattamente una volta ciascuna.
; E' l'approccio piu' diretto possibile alla definizione del problema
; ("per ogni coppia di intervalli, verifica se si intersecano"), usato
; come riferimento (ground truth) per validare la versione ottimizzata
; 'count-intersecting-pairs' basata su sweep-line.
;
; COMPLESSITA': O(n^2), dove n = (length lst) — si esaminano tutte le
; C(n,2) = n*(n-1)/2 coppie possibili, ciascuna con un test O(1)
; (la funzione 'cross?'). Nessuna ottimizzazione: la semplicita' della
; logica e' cio' che la rende affidabile come oracolo di verifica.
; ---------------------------------------------------------------------------
(define (cross-pairs lst values)
  (let (
        conta 0    ; contatore di coppie intersecanti trovate
                   ; (usato solo quando values = nil)
        pairs '()  ; lista delle coppie intersecanti trovate
                   ; (usata solo quando values = true)
        )
    ; --- Ciclo esterno: indice i, dal primo elemento al penultimo ---
    ; (- (length lst) 2) e' l'indice del penultimo elemento (indicizzazione
    ; da 0): non ha senso far arrivare i fino all'ultimo elemento, perche'
    ; a quel punto non ci sarebbe piu' nessun j > i da confrontare con lui.
    (for (i 0 (- (length lst) 2))
      ; --- Ciclo interno: indice j, sempre STRETTAMENTE dopo i ---
      ; Partire da (+ i 1) e non da 0 e' cio' che garantisce che ogni
      ; coppia (i, j) venga esaminata UNA SOLA VOLTA: senza questo
      ; accorgimento si confronterebbero sia (i, j) sia (j, i),
      ; duplicando ogni coppia e sballando il conteggio. Analogamente,
      ; i < j esclude anche il confronto di un intervallo con se stesso.
      (for (j (+ i 1) (- (length lst) 1))
          ; --- Test di intersezione tra l'intervallo i-esimo e j-esimo ---
          ; (lst i) e (lst j) accedono agli intervalli tramite
          ; indicizzazione (le liste in NewLisp si comportano come
          ; funzioni); cross? verifica la definizione di intersezione
          ; L1<=L2<=R1 oppure L2<=L1<=R2.
          (when (cross? (lst i) (lst j))
              ; --- Azione da compiere quando la coppia si interseca ---
              ; if a due rami then/else, mutuamente esclusivi: per ogni
              ; coppia intersecante si esegue esattamente una delle due
              ; azioni, mai entrambe.
              (if values
                  ; then (values = true): modalita' dettaglio -> si
                  ; costruisce una lista di due elementi con i due
                  ; intervalli originali, nell'ordine (i-esimo, j-esimo),
                  ; e la si accoda in fondo a 'pairs' (parametro -1 di
                  ; push = inserimento in coda, per preservare l'ordine
                  ; di scoperta delle coppie durante la scansione)
                  (push (list (lst i) (lst j)) pairs -1)
                  ;else
                  ; else (values = nil): modalita' conteggio -> si
                  ; incrementa semplicemente il contatore, senza mai
                  ; costruire la lista 'pairs' (che resta vuota)
                  (++ conta)))))
    ; --- Restituzione del risultato in base alla modalita' richiesta ---
    ; Se values e' true, il risultato utile e' la lista 'pairs'
    ; (il contatore 'conta' non e' mai stato toccato in questo caso).
    ; Se values e' nil, il risultato utile e' il numero 'conta'
    ; (la lista 'pairs' non e' mai stata popolata in questo caso).
    (if values pairs conta)))

; Calcola le coppie univoche di intervalli che si intersecano
; Versione senza commenti
(define (cross-pairs lst values)
  (let ( (conta 0) (pairs '()) )
    (for (i 0 (- (length lst) 2))
      (for (j (+ i 1) (- (length lst) 1))
          (when (cross? (lst i) (lst j))
              (if values
                (push (list (lst i) (lst j)) pairs -1)
                (++ conta)))))
    (if values pairs conta)))

Proviamo:

(cross-pairs intervals)
;-> 18
(cross-pairs intervals true)
;-> (((2 8) (7 14)) ((2 8) (5 10)) ((2 8) (1 3)) ((2 8) (5 14)) ((11 19)
;->  (7 14)) ((11 19) (17 27)) ((11 19) (13 18)) ((11 19) (5 14))
;->  ((7 14) (5 10)) ((7 14) (13 18)) ((7 14) (5 14)) ((17 27) (13 18))
;->  ((17 27) (25 27)) ((17 27) (27 30)) ((5 10) (5 14)) ((13 18) (5 14))
;->  ((25 27) (27 30)) ((29 30) (27 30)))
(length (cross-pairs intervals true))
;-> 18

(cross-pairs intervals2)
;-> 7
(cross-pairs intervals2 true)
;-> (((2 3) (2 4)) ((6 9) (7 8)) ((15 20) (16 19)) ((15 20) (18 22))
;->  ((16 19) (18 22)) ((30 33) (31 35)) ((38 40) (39 39)))
(length (cross-pairs intervals2 true))
;-> 7

----------------------
B) Metodo "Sweep line"
----------------------
Dato un insieme di intervalli (L R) con L <= R, dobbiamo due problemi:
  1) Per ogni punto intero della retta, quanti intervalli lo coprono?
  2) Quante coppie DISTINTE di intervalli si intersecano tra loro?

Entrambi vengono risolti con la tecnica dello "sweep line" (linea di scansione).
Invece di confrontare ogni intervallo con tutti gli altri (approccio O(n^2)), si trasformano gli intervalli in "eventi" puntuali (inizio/fine) e si scansionano gli eventi in ordine di posizione, mantenendo un contatore che rappresenta lo stato "attuale" mentre si avanza lungo la retta.
Questo abbassa la complessità a O(n*log(n)).

; *************************************************************
; 1) Copertura per ogni punto intero — tecnica del "diff array"
; *************************************************************
; Idea:
; Se dovessimo sommare +1 a ogni posizione coperta da un intervallo
; (L,R), per un intervallo lungo servirebbero O(R-L) operazioni, e con
; molti intervalli lunghi il costo totale esploderebbe.
;
; Il trucco del "diff array" (array delle differenze) evita questo:
; invece di incrementare tutte le posizioni da L a R, registriamo SOLO
; due eventi puntuali:
;
;    diff[L]   += 1   -> "da qui in poi, un intervallo in piu' e' attivo"
;    diff[R+1] -= 1   -> "da qui in poi, quell'intervallo non e' piu' attivo"
;
; Poi, scorrendo diff da sinistra a destra e accumulando una somma
; progressiva (prefix sum / somma prefissa), otteniamo esattamente il
; numero di intervalli attivi in ogni posizione. Ogni intervallo costa
; O(1) in fase di registrazione, indipendentemente dalla sua lunghezza;
; il costo totale e' O(n + max_point).
;
; ESEMPIO INTUITIVO:
; Intervallo (2 5) genera: diff[2] += 1 (si "accende" in 2)
;                           diff[6] -= 1 (si "spegne" subito dopo 5)
; Sommando progressivamente, il contatore sale a 2 e resta a 2 fino alla
; posizione 5 inclusa, poi ridiscende a 6.
;
(define (coverage-per-point intervals)
  (letn (; diff e' l'array delle differenze. Serve una cella extra oltre
         ; max-point perche' un intervallo con R = max-point genera un
         ; evento di "spegnimento" in R+1 = max-point+1.
         (max-point (apply max (flat intervals)))
         (diff (dup 0 (+ max-point 2)))
         (coverage '())   ; risultato finale: lista con la copertura di ogni punto
         (running 0)      ; somma progressiva (accumulatore dello sweep)
         (L 0)
         (R 0))
    ; --- FASE 1: registrazione degli eventi di inizio/fine ---
    ; Per ogni intervallo (L R) non tocchiamo tutte le posizioni interne,
    ; ma solo i due "bordi" dell'evento: apertura in L, chiusura in R+1.
    (dolist (iv intervals)
      (setq L (iv 0) R (iv 1))
      (setf (diff L) (+ (diff L) 1))              ; apertura: +1 da L in poi
      (setf (diff (+ R 1)) (- (diff (+ R 1)) 1))) ; chiusura: -1 da R+1 in poi
    ; --- FASE 2: somma prefissa (prefix sum) ---
    ; Scorrendo da sinistra a destra, accumuliamo i +1/-1 registrati.
    ; Il valore accumulato in ogni posizione rappresenta esattamente
    ; quanti intervalli sono "aperti" (attivi) in quel punto, perche'
    ; ogni apertura ancora non richiusa contribuisce ancora alla somma.
    (for (pos 1 max-point)
      (setq running (+ running (diff pos)))
      (push running coverage -1))  ; -1 = inserisci in coda (mantiene l'ordine)
    coverage))

; Copertura per ogni punto intero — tecnica del "diff array"
(define (coverage-per-point intervals)
  (letn ((max-point (apply max (flat intervals)))
         (diff (dup 0 (+ max-point 2)))
         (coverage '())
         (running 0)   
         (L 0)
         (R 0))
    (dolist (iv intervals)
      (setq L (iv 0) R (iv 1))
      (setf (diff L) (+ (diff L) 1))              
      (setf (diff (+ R 1)) (- (diff (+ R 1)) 1))) 
    (for (pos 1 max-point)
      (setq running (+ running (diff pos)))
      (push running coverage -1))
    coverage))

Proviamo:

(coverage-per-point intervals)
;-> (1 2 2 1 3 3 4 4 3 3 3 3 4 4 2 2 3 3 2 1 1 1 1 1 2 2 3 1 2 2)
(coverage-per-point intervals2)
;-> (0 2 2 1 0 1 2 2 1 0 0 0 0 0 1 2 2 3 3 2 1 1 0 0 0 0 0 0 0 1 2 2 2 1 1 0 0 1 2 1)

; ***********************************************************
; 2) Numero di coppie uniche di intervalli che si intersecano
; ***********************************************************
; Idea:
; Confrontare ogni coppia di intervalli a due a due costerebbe O(n^2),
; troppo lento per n grande. Usiamo invece lo stesso principio dello
; sweep line della funzione precedente, ma invece di sommare unita' di
; copertura, contiamo le coppie che si "incrociano" nel tempo.
;
; Trasformazione in eventi:
; Ogni intervallo (L R) genera due eventi:
;    - un evento di tipo START in posizione L
;    - un evento di tipo END   in posizione R+1
;      (R+1 e non R, perche' un intervallo che finisce in R deve ancora
;      essere considerato "attivo" nel punto R stesso: l'intersezione
;      e' definita come L1<=L2<=R1 oppure L2<=L1<=R2, quindi toccarsi
;      esattamente in un punto conta come intersezione, es. [1,3] e [3,5])
;
; Ordinamento degli eventi:
; Gli eventi vengono rappresentati come coppie (posizione tipo), con
; tipo = 0 per END e tipo = 1 per START. Ordinandoli lessicograficamente
; (prima per posizione, poi per tipo), a parita' di posizione un evento
; END precede sempre un evento START: questo garantisce che, se un
; intervallo finisce esattamente dove un altro inizia, l'intervallo che
; finisce venga "chiuso" solo DOPO aver considerato l'intersezione nel
; punto di contatto (dato che END e' posizionato in R+1, non in R, il
; contatto in R e' comunque gia' garantito prima che END scatti).
;
; Logica di conteggio — come funziona:
; Manteniamo un contatore "active" = quanti intervalli sono correntemente
; aperti mentre scandiamo gli eventi in ordine di posizione.
;   - Quando incontriamo un evento START per l'intervallo X, significa
;     che X si sta "aprendo" adesso. Tutti gli intervalli GIA' aperti
;     in questo momento (cioe' il valore corrente di "active") si
;     intersecano necessariamis con X, perche' sono entrambi attivi
;     nello stesso punto della retta. Quindi sommiamo "active" al
;     totale: sono esattamente le nuove coppie (X, ciascuno degli
;     intervalli gia' aperti) create dall'apertura di X.
;     Poi incrementiamo "active" di 1 (X e' ora attivo anche lui).
;   - Quando incontriamo un evento END, un intervallo si "chiude":
;     decrementiamo "active" di 1. Non serve fare nulla per il conteggio
;     delle coppie in questo momento, perche' tutte le coppie che
;     coinvolgono l'intervallo che si chiude sono gia' state contate
;     nel momento in cui gli OTHER intervalli con cui si interseca
;     sono stati aperti (o quando lui stesso si e' aperto).
;
; Ogni coppia (A, B) che si interseca viene quindi contata ESATTAMENTE
; UNA VOLTA: nel momento in cui il secondo dei due (in ordine di
; apertura) viene aperto, trovando il primo gia' "active".
;
; Complessita': O(n*log(n)), dominata dall'ordinamento di 2n eventi.
;
(define (build-events intervals)
  ; Trasforma ogni intervallo (L R) in due eventi puntuali:
  ;   (L   1)  -> START in L
  ;   (R+1 0)  -> END   in R+1 (esclusivo, per includere i punti di contatto)
  (let (events '())
    (dolist (iv intervals)
      (push (list (iv 0) 1) events -1)         ; evento di apertura
      (push (list (+ (iv 1) 1) 0) events -1))  ; evento di chiusura (esclusiva)
    events))

(define (count-intersecting-pairs intervals)
  (let ((events (build-events intervals))
        (active 0)    ; quanti intervalli sono correntemente "aperti"
        (total 0))    ; numero totale di coppie intersecanti trovate finora
    ; Ordinamento lessicografico naturale delle liste (pos tipo):
    ; si ordina prima per "pos" crescente, e a parita' di "pos" per
    ; "tipo" crescente. Poiche' END=0 < START=1, a parita' di posizione
    ; gli eventi di chiusura vengono processati PRIMA di quelli di
    ; apertura: questo e' cio' che vogliamo, dato che END e' gia'
    ; posizionato in R+1 (un passo oltre l'ultimo punto coperto), quindi
    ; non interferisce mai con un'apertura che avviene esattamente in R.
    (sort events)
    ; Scansione della linea: per ogni evento in ordine di posizione...
    (dolist (ev events)
      (if (= (ev 1) 1)
          ; --- Evento di START ---
          ; Tutti gli "active" intervalli correnti si intersecano con
          ; quello che si sta aprendo ora: aggiungiamo "active" nuove
          ; coppie al totale, poi rendiamo attivo anche il nuovo.
          (begin
            (setq total (+ total active))
            (setq active (+ active 1)))
          ; --- Evento di END ---
          ; Un intervallo si chiude: il numero di intervalli attivi
          ; diminuisce di 1. Le sue intersezioni sono gia' state
          ; contate in precedenza, quindi qui non si somma nulla al
          ; totale.
          (setq active (- active 1))))
    total))

; Trasforma ogni intervallo (L R) in due eventi puntuali
; Versione senza commenti
(define (build-events intervals)
  (let (events '())
    (dolist (iv intervals)
      (push (list (iv 0) 1) events -1)        
      (push (list (+ (iv 1) 1) 0) events -1)) 
    events))

; Numero di coppie uniche di intervalli che si intersecano
; Versione senza commenti
(define (count-intersecting-pairs intervals)
  (let ( (events (build-events intervals))
         (active 0) (total 0) )
    (sort events)
    (dolist (ev events)
      (if (= (ev 1) 1)
          (begin
            (setq total (+ total active))
            (setq active (+ active 1)))
          (setq active (- active 1))))
    total))

Proviamo:

(count-intersecting-pairs intervals)
;-> 18

Test di correttezza delle funzioni
----------------------------------
(= (coverage intervals) (map length (coverage intervals true)) (coverage-per-point intervals))
;-> true
(= (cross-pairs intervals) (length (cross-pairs intervals true)) (count-intersecting-pairs intervals))
;-> true

(= (coverage intervals2) (map length (coverage intervals2 true)) (coverage-per-point intervals2))
;-> true
(= (cross-pairs intervals2) (length (cross-pairs intervals2 true)) (count-intersecting-pairs intervals2))
;-> true

Test di velocità
----------------
(time (coverage intervals) 1e4)
;-> 656.085
(time (coverage-per-point intervals) 1e4)
;-> 58.132
(time (coverage intervals2) 1e4)
;-> 845.142
(time (coverage-per-point intervals2) 1e4)
;-> 93.757

(time (cross-pairs intervals) 1e4)
;-> 203.041
(time (count-intersecting-pairs intervals) 1e4)
;-> 78.133
(time (cross-pairs intervals2) 1e4)
;-> 187.909
(time (count-intersecting-pairs intervals2) 1e4)
;-> 78.134

Differenza tra 'contare' ed 'elencare'
--------------------------------------
Le funzioni che usano "Sweep-line" 'contano' gli eventi.
Le funzioni che usano "Brute-Force" 'contano' ed 'elencano' gli eventi.
Qui sta il punto cruciale: 'contare' le intersezioni e 'elencarle' sono due problemi con costi intrinsecamente diversi.
- Contare puo essere fatto in O(n log n) perché un singolo numero ('active') riassume in O(1) l'informazione "quante coppie si formano ora", indipendentemente da quante siano.
- Elencare le coppie non può scendere sotto il costo proporzionale al **numero di coppie stesse**: se, nel caso peggiore, tutti gli n intervalli si sovrappongono a vicenda, ci sono O(n^2) coppie da restituire, e nessun algoritmo — per quanto elegante — può enumerarle in meno tempo di quello necessario a scriverle tutte.
Anche 'coverage-per-point' funziona con il 'diff array' proprio perché non ha mai bisogno di sapere quali intervalli coprono un punto, solo quanti.
Il trucco diff[L] += 1 / diff[R+1] -= 1 seguito dalla somma prefissa produce un numero aggregato — l'informazione su quale intervallo abbia contribuito a quel numero va persa nel momento stesso in cui si fa +1, perché un +1 è indistinguibile da un altro +1 proveniente da un intervallo diverso.
È esattamente l'analogo del contatore active nello sweep-line: un numero riassuntivo, non una lista di riferimenti.

Nota: tutte le funzioni calcolano da 1 al valore massimo degli intervalli (max-point).
Se gli intervalli iniziano molto distanti da 1, allora possiamo prima traslare gli intervalli verso 1 e poi traslare nuovamente il risultato nelle posizioni originali.
Questo è il principio del 'separation of concerns': la logica "cosa significa intersecarsi/coprire un punto" resta isolata dalla logica "come sono numerati i punti nel mondo reale".

Vedi anche "Intersezione di intervalli" su "Note libere 30".


----------------------------------------
Problemi su strutture grafiche numeriche
----------------------------------------

Problema 1
----------

          +---+                          +---+
          | a |                          | 0 |
          +---+                          +---+
      +---+   +---+                  +---+   +---+
      | b |   | c |                  | 1 |   | 2 |
      +---+   +---+                  +---+   +---+
  +---+   +---+   +---+          +---+   +---+   +---+
  | d |   | e |   | f |          | 3 |   | 4 |   | 5 |
  +---+   +---+   +---+          +---+   +---+   +---+

Inserire i numeri da 1 a 6 nei quadrati in modo che risulti:
  1) a + c + e + d = 14
  2) a + b + e + f = 14
  3) d + b + c + f = 14

(define (perm lst)
"Generate all permutations without repeating from a list of items"
  (local (i indici out)
    (setq indici (dup 0 (length lst)))
    (setq i 0)
    ; aggiungiamo la lista iniziale alla soluzione
    (setq out (list lst))
    (while (< i (length lst))
      (if (< (indici i) i)
          (begin
            (if (zero? (% i 2))
              (swap (lst 0) (lst i))
              (swap (lst (indici i)) (lst i)))
            (push lst out -1)
            (++ (indici i))
            (setq i 0))
          (begin
            (setf (indici i) 0)
            (++ i))))
    out))

(define (solve lst test)
  (let ((out '()) (permute (perm lst)))
    (dolist (p permute)
      (if (test p) (push p out -1)))
    out))

(define (check1? lst)
  (and (= (apply + (select lst '(0 2 3 4))) 14)
       (= (apply + (select lst '(0 1 4 5))) 14)
       (= (apply + (select lst '(1 2 3 5))) 14)))

(setq sol (solve (sequence 1 6) check1?))
;-> ((2 1 3 4 5 6) (2 1 4 3 5 6) (5 1 3 4 2 6)
;->  (5 1 4 3 2 6) (4 1 2 5 3 6) (4 1 5 2 3 6)
;->  ...
;->  (2 6 4 3 5 1) (2 6 3 4 5 1) (5 6 3 4 2 1)
;->  (5 6 4 3 2 1) (4 6 2 5 3 1) (4 6 5 2 3 1))
(length sol)
;-> 48

Problema 2
----------

        +---+                      +---+
        | a |                      | 0 |
        +---+                      +---+
       /  |  \                    /  |  \
  +---+ +---+ +---+          +---+ +---+ +---+
  | b |-| c |-| d |          | 1 |-| 2 |-| 3 |
  +---+ +---+ +---+          +---+ +---+ +---+
    |  X  |  X  |              |  X  |  X  |
  +---+ +---+ +---+          +---+ +---+ +---+
  | e |-| f |-| g |          | 4 |-| 5 |-| 6 |
  +---+ +---+ +---+          +---+ +---+ +---+
       \  |  /                    \  |  /
        +---+                      +---+
        | h |                      | 7 |
        +---+                      +---+

Inserire i numeri da 1 a 8 nei quadrati in modo che nessun numero è connesso da una linea con un numero che è +1 o -1 del numero stesso.
Per esempio, il 4 non può essere connesso con il 3 o il 5.

(define (collide? x y)
  (or (= x (+ y 1)) (= x (- y 1))))

(define (check2? lst)
  (let ((stop nil)
        (coppie '((0 1) (0 2) (0 3)   ;((a b) (a c) (a d)
                  (1 2) (2 3)         ; (b c) (c d)
                  (1 4) (1 5)         ; (b e) (b f)
                  (2 4) (2 5) (2 6)   ; (c e) (c f) (c g)
                  (3 5) (3 6)         ; (d f) (d g)
                  (4 7) (4 5)         ; (e h) (e f)
                  (5 7) (5 6)         ; (f h) (f g)
                  (6 7))))            ; (g h)))
    (dolist (c coppie stop)
      (if (collide? (lst (c 0)) (lst (c 1)))
          (setq stop true)))
    (not stop)))

(setq sol (solve (sequence 1 8) check2?))
;-> ((2 5 8 6 3 1 4 7) (2 6 8 5 4 1 3 7) (7 4 1 3 6 8 5 2) (7 3 1 4 5 8 6 2))

Problema 3
----------

  +---+         +---+          +---+         +---+
  | a |         | b |          | 0 |         | 1 |
  +---+         +---+          +---+         +---+
    |             |              |             |
  +---+  +---+  +---+          +---+  +---+  +---+
  | c |--| d |--| e |          | 2 |--| 3 |--| 4 |
  +---+  +---+  +---+          +---+  +---+  +---+
    |             |              |             |
  +---+         +---+          +---+         +---+
  | f |         | g |          | 5 |         | 6 |
  +---+         +---+          +---+         +---+

Inserire 7 cifre diverse da 1 a 9 nei quadrati in modo che risulti:

  a * c * f = c * d * e = b * e * g

(define (comb k lst (r '()))
"Generate all combinations of k elements without repetition from a list of items"
  (if (= (length r) k)
    (list r)
    (let (rlst '())
      (dolist (x lst)
        (extend rlst (comb k ((+ 1 $idx) lst) (append r (list x)))))
      rlst)))

(define (check3? lst)
  (= (* (lst 0) (lst 2) (lst 5))
     (* (lst 1) (lst 4) (lst 6))
     (* (lst 2) (lst 3) (lst 4))))

(define (solve3 lst test)
  (let ((out '())
        (combine (comb 7 lst)))
    (dolist (c combine)
      (setq permute (perm c))
      (dolist (p permute)
        (if (test p) (push p out -1)))
      out)))

(setq sol (solve3 (sequence 1 9) check3?))
;-> ((8 6 9 2 4 1 3) (1 6 9 2 4 8 3) (8 3 9 2 4 1 6) (1 3 9 2 4 8 6)
;->  (6 1 4 2 9 3 8) (3 1 4 2 9 6 8) (3 8 4 2 9 6 1) (6 8 4 2 9 3 1))

Problema 4
----------

Nella figura seguente i numeri sono disposti in modo che la differenza assoluta tra due numeri adiacenti si trova sotto ai numeri nella riga successiva:

  +---+   +---+   +---+
  | 2 |   | 6 |   | 5 |
  +---+   +---+   +---+
      +---+   +---+
      | 4 |   | 1 |
      +---+   +---+
          +---+
          | 3 |
          +---+

  6 - 2 = 4
  6 - 5 = 1
  4 - 1 = 3

Usare i numeri da 1 a 10 per riempire con lo stesso criterio la figura seguente:

  +---+   +---+   +---+   +---+          +---+   +---+   +---+   +---+
  | a |   | b |   | c |   | d |          | 0 |   | 1 |   | 2 |   | 3 |
  +---+   +---+   +---+   +---+          +---+   +---+   +---+   +---+
      +---+   +---+   +---+                  +---+   +---+   +---+
      | e |   | f |   | g |                  | 4 |   | 5 |   | 6 |
      +---+   +---+   +---+                  +---+   +---+   +---+
          +---+   +---+                          +---+   +---+
          | h |   | i |                          | 7 |   | 8 |
          +---+   +---+                          +---+   +---+
              +---+                                  +---+
              | l |                                  | 9 |
              +---+                                  +---+

(define (check4? lst)
  (and (= (abs (- (lst 0) (lst 1))) (lst 4))
       (= (abs (- (lst 1) (lst 2))) (lst 5))
       (= (abs (- (lst 2) (lst 3))) (lst 6))
       (= (abs (- (lst 4) (lst 5))) (lst 7))
       (= (abs (- (lst 5) (lst 6))) (lst 8))
       (= (abs (- (lst 7) (lst 8))) (lst 9))))

(setq sol (solve (sequence 1 10) check4?))
;-> ((8 1 10 6 7 9 4 2 5 3) (8 10 1 6 2 9 5 7 4 3) (6 10 1 8 4 9 7 5 2 3)
;->  (6 1 10 8 5 9 2 4 7 3) (8 3 10 9 5 7 1 2 6 4) (9 3 10 8 6 7 2 1 5 4)
;->  (9 10 3 8 1 7 5 6 2 4) (8 10 3 9 2 7 6 5 1 4))


----------------------------------
Analisi delle funzioni rand e seed
----------------------------------

Vogliamo verificare la correttezza delle funzioni "rand" e "seed".
Per fare questo scriviamo due funzioni che effettuano lo stesso compito in due modi diversi.

Algoritmo Funzione 1
--------------------
1) Impostare il contatore a 0
2) Generare due numeri casuali compresi tra 0 e il valore massimo:
   Se i due numeri sono uguali, allora aumentare il contatore di 1
3) Ripetere il passo 2) per un dato numero di volte
4) Restituire il valore del contatore

Algoritmo Funzione 2
--------------------
1) Impostare il contatore a 0
2) Generare due liste di una data lunghezza con numeri casuali
   compresi tra 0 e il valore massimo
3) Attraversare le due liste:
   Se lista1(i) = lista2(i), allora incremetare il contatore di 1 
4) Restituire il valore del contatore

Le due funzioni dovrebbero restituire risultati simili perchè calcolano la stessa cosa.

; Funzione 1
; Genera 2 numeri casuali (0..max-val) e conta quante volte sono uguali
; in un dato numero di iterazioni (iter)
(define (test1 max-val iter)
  (let (conta 0)
    (for (i 1 iter)
      ; numeri casuali uguali?
      (if (= (rand max-val) (rand max-val))
          (++ conta)))
    conta))

; Funzione 2
; Genera due liste di una data lunghezza (iter) con numeri casuali (0..max-val)
; e conta quanti numeri sono uguali nelle stesse posizioni
(define (test2 max-val iter)
  (let ((conta 0)
        (num1 (rand max-val iter))
        (num2 (rand max-val iter)))
    (length (filter true? (map (fn(x y) (= x y)) num1 num2)))))

Ci aspettiamo il seguente risultato da entrambe le funzioni:

                                   Numero di iterazioni
  Numero di coppie uguali =  --------------------------------
                               Numero di elementi possibili    

cioè:
                                iter
  Numero di coppie uguali =  -----------
                               max-val

Inizializziamo il generatore random interno:
(seed (time-of-day))

100 elementi:
(map (curry test1 1e2) '(1e4 1e5 1e6 1e7))
;-> (96 978 10008 99725)
(map (curry test2 1e2) '(1e4 1e5 1e6 1e7))
;-> (96 978 10008 99725)

1000 elementi:
(map (curry test1 1e3) '(1e4 1e5 1e6 1e7))
;-> (9 97 977 10040)
(map (curry test2 1e3) '(1e4 1e5 1e6 1e7))
;-> (10 100 955 9953)

10000 elementi:
(map (curry test1 1e4) '(1e4 1e5 1e6 1e7))
;-> (3 8 117 998)
(map (curry test2 1e4) '(1e4 1e5 1e6 1e7))
;-> (2 7 92 1032)

Per adesso i risultati delle funzioni sono simili e confermano i valori teorici.

100000 elementi:
(map (curry test1 1e5) '(1e4 1e5 1e6 1e7))
;-> (0 1 41 322)
(map (curry test2 1e5) '(1e4 1e5 1e6 1e7))
;-> (1 7 33 279)

1 milione di elementi:
(map (curry test1 1e6) '(1e4 1e5 1e6 1e7))
;-> (1 2 31 285)
(map (curry test2 1e6) '(1e4 1e5 1e6 1e7))
;-> (0 4 33 323)

10 milione di elementi:
(map (curry test1 1e7) '(1e4 1e5 1e6 1e7))
;-> (1 1 35 327)
(map (curry test2 1e7) '(1e4 1e5 1e6 1e7))
;-> (2 3 32 295)

Negli ultimi tre risultati c'è qualcosa che non torna.
I valori 285, 323, 327 e 295 sono evidentemente errati.
Qual'è il problema?

Vediamo la definizione della funzione "seed" dal manuale di riferimento:

*******************
>>> funzione SEED
*******************

sintassi: (seed int-seed)
sintassi: (seed int-seed true [int-pre-N])
sintassi: (seed)

Il parametro "int-seed" inizializza il generatore di numeri casuali interno che genera i numeri per le funzioni "amb", "normal", "rand" e "random".
Si noti che la prima sintassi utilizza un generatore di numeri casuali basato sulla funzione "rand()" della libreria C.
Tutte le funzioni di randomizzazione in newLISP si basano su questa funzione.

Utilizzando la seconda sintassi, tutte le funzioni di randomizzazione si basano su un generatore di numeri casuali indipendente dalla piattaforma e dal compilatore utilizzati per compilare newLISP.
Quando si utilizza la seconda sintassi per l'inizializzazione, tutte le funzioni di randomizzazione chiamate successivamente, come "amb", "normal", "rand", "random" e "randomize", si basano su questo generatore di numeri casuali indipendente dalla piattaforma.

Il parametro opzionale "int-pre-N" specifica il numero di numeri casuali da precaricare durante la procedura di inizializzazione.
Se questo parametro viene omesso, "seed" assume il valore 50.
Si noti che il valore massimo per "int-seed" è limitato a 16 o 32 bit, a seconda del sistema operativo utilizzato.
Internamente, solo i 32 bit meno significativi vengono passati alla funzione di seeding del sistema operativo.

(seed 12345)
(seed (ora del giorno))

Dopo aver utilizzato lo stesso seed, il generatore di numeri casuali avvia la stessa sequenza di numeri.
Questo facilita il debug quando si utilizzano dati casuali.
Utilizzando il seed, è possibile generare ripetutamente le stesse sequenze casuali.
Il secondo esempio è utile per garantire un seed diverso ogni volta che il programma viene avviato.

L'esempio seguente mostra l'utilizzo dello stato interno del seed nel generatore di numeri casuali integrato:

(seed 123 true) ; use the true parameter
;-> 123
(random)
;-> 0.2788576787704871
(random)
;-> 0.7610070955758016
(random)
;-> 0.2462553424976092
(random)
;-> 0.8135413573186572
(set 'state (seed)) ; save current state
;-> 1747066761
(random)
;-> 0.1895924546707387
(random)
;-> 0.4803856511043318
(seed state true 0) ; seed with saved state
;-> 1747066761
(random)            ; produces old sequence
;-> 0.1895924546707387       
(random)
;-> 0.4803856511043318      

Nell'ultima parte della sintassi, "seed" restituisce lo stato corrente del seed.
---------------------

Quindi utilizzando (seed (time-of-day) newLISP usa il generatore rand() del compilatore C usato per generare l'eseguibile di newLISP.
Mentre utilizzando (seed (time-of-day) true) newLISP usa un generatore proprio.
Proviamo ad utilizzare il generatore proprio:

(seed (time-of-day) true)

(map (curry test1 1e5) '(1e4 1e5 1e6 1e7))
;-> (0 0 8 96)
(map (curry test2 1e5) '(1e4 1e5 1e6 1e7))
;-> (0 1 11 94)

(map (curry test1 1e6) '(1e4 1e5 1e6 1e7))
;-> (0 0 4 11)
(map (curry test2 1e6) '(1e4 1e5 1e6 1e7))
;-> (0 0 2 12)

(map (curry test1 1e7) '(1e4 1e5 1e6 1e7))
;-> (0 0 0 1)
(map (curry test2 1e7) '(1e4 1e5 1e6 1e7))
;-> (0 0 0 1)

In questo caso i risultati sono corretti.

(seed (time-of-day) true)
;-> 76533017
(map (curry test 1e5) '(1e4 1e5 1e6 1e7))
;-> (0 3 11 102)
(map (curry test 1e6) '(1e4 1e5 1e6 1e7))
;-> (0 0 0 11)
(map (curry test 1e7) '(1e4 1e5 1e6 1e7))
;-> (0 0 0 1)

Per compilare newLISP ho usato TDM-gcc, vediamo che limiti ha la funzione rand() in questo compilatore.
Per il runtime Microsoft/MinGW utilizzato da queste versioni, RAND_MAX è:
  0x7fff = 32767
e quindi rand() restituisce valori nell'intervallo (0...32767)
La definizione presente negli header MinGW è proprio:
#define RAND_MAX 0x7fff
Microsoft documenta anch'essa RAND_MAX = 32767 per il proprio CRT.
Ma questo NON spiega direttamente il nostro problema.
Infatti newLISP fa qualcosa in più quando chiamiamo "rand" con un numero maggiore di RAND_MAX:
costruisce il risultato usando il generatore C sottostante.
Non basta quindi guardare RAND_MAX: dobbiamo vedere il codice sorgente di newLISP, precisamente l'implementazione di "rand", perché è lì che probabilmente avviene la trasformazione da rand() del C a (rand n).
Il problema quindi è nella struttura della sequenza generata dal rand() sottostante e nel modo in cui newLISP combina quei valori.

La morale finale è quella di utilizzare SEMPRE "seed" con il parametro 'true':

  (seed (time-of-day) true)


-----------------
Catturare il topo
-----------------

In una parete ci sono sette buchi allineati.
Dentro un buco si trova un topo.
Dobbiamo scoprire dove si trova il topo.
Ogni giorno possiamo controllare solo un buco.
Se troviamo il topo, abbiamo terminato.
Se il topo non c'è, dobbiamo aspettare il giorno successivo prima di poter controllare di nuovo un buco.
Il topo non sta fermo e ogni notte si sposta in un altro buco.
Il buco in cui si sposta è o quello immediatamente a sinistra o quello immediatamente a destra da dove si trovava prima.
Il topo può anche ritornare nei buchi in cui era già stato.
Quanti giorni occorrono per essere sicuri di trovare il topo?
   
  +------+  +------+  +------+  +------+  +------+  +------+  +------+  
  | 1    |  | 2    |  | 3    |  | 4    |  | 5    |  | 6    |  | 7    |
  |      |  |      |  |      |  |      |  |      |  |      |  |      |
  +------+  +------+  +------+  +------+  +------+  +------+  +------+

Immaginiamo che ci siano solo tre buchi.
Se controlliamo il buco centrale per due giorni consecutivi, prendiamo sicuramente il topo.
Questo perché se il topo non si trova nel buco centrale il primo giorno, deve per forza essere dietro una delle buche laterali.
E se il primo giorno si trova in un buco laterale, il secondo giorno non avrà altra scelta che spostarsi nel buco centrale.

Adeso immaginiamo che ci siano quattro buchi.
Nella tabella seguente, ogni riga mostra le possibili posizioni del topo (T) in ogni giorno.
La X indica il buco controllato ogni giorno.

           Buco 1 Buco 2 Buco 3 Buco 4
          +------+------+------+------+
giorno 1  | T    | T    | T    | T    |
          |      |    X |      |      |
          +------+------+------+------+
giorno 2  |      | T    | T    | T    |
          |      |      |    X |      |
          +------+------+------+------+
giorno 3  | T    |      | T    |      |
          |      |      |    X |      |
          +------+------+------+------+
giorno 4  |      | T    |      |      |
          |      |    X |      |      |
          +------+------+------+------+

Il primo giorno il topo potrebbe trovarsi dietro qualsiasi buco, quindi ci sono T in ogni cella.
Controlliamo il secondo buco. Se il topo è lì, gioco finito.
Ma se il topo non c'è, posso eliminare la possibilità che il topo si trovi nel primo buco il secondo giorno, poiché l'unico modo in cui il topo potrebbe trovarsi lì è se si trovava nel secondo buco il primo giorno.
Il secondo giorno, quindi, il topo può trovarsi solo in tre possibili buchi.

Il secondo giorno controlliamo il terzo buco. Se il topo è lì, gioco finito.
Altrimenti, posso eliminare la possibilità che il topo si trovi nel quarto buco il terzo giorno.
Posso anche eliminare la possibilità che il topo si trovi nel secondo buco il terzo giorno, poiché per arrivarci avrebbe dovuto spostarsi dal buco 1 o dal buco 3, entrambi noti per non nascondere un topo.
Abbiamo ridotto le possibilità a due.

Il terzo giorno controlliamo il terzo buco. Se il topo è lì, è finita.
Se il topo non c'è, deve essere stato nel buco 1, il che significa che controllando il secondo buco il quarto giorno posso garantire la cattura del topo.
Quindi possiamo catturare il topo in 4 giorni controllando i buchi 2, 3, 3, 2 in quest'ordine.

Con un ragionamento analogo possiamo risolvere il caso con 5 buchi:

           Buco 1 Buco 2 Buco 3 Buco 4 Buco 5
          +------+------+------+------+------+
giorno 1  | T    | T    | T    | T    | T    |
          |      |    X |      |      |      |
          +------+------+------+------+------+
giorno 2  |      | T    | T    | T    | T    |
          |      |      |    X |      |      |
          +------+------+------+------+------+
giorno 3  | T    |      | T    | T    | T    |
          |      |      |      |    X |      |
          +------+------+------+------+------+
giorno 4  |      | T    |      | T    |      |
          |      |      |      |    X |      |
          +------+------+------+------+------+
giorno 5  |T     |      | T    |      |      |
          |      |      |    X |      |      |
          +------+------+------+------+------+
giorno 6  |      | T    |      |      |      |
          |      |    X |      |      |      |
          +------+------+------+------+------+

In questo caso possiamo catturare il topo in 6 giorni controllando i buchi 2, 3, 4, 4, 3, 2 in quest'ordine.

Mettiamo insieme i tre risultati (3 buchi, 4 buchi e 5 buchi):

  +-------+--------------+--------+
  | Buchi | Sequenza     | Giorni |
  +-------+--------------+--------+
  |  3    | 2 2          | 2      |
  |  4    | 2 3 3 2      | 4      |
  |  5    | 2 3 4 4 3 2  | 6      |
  +-------+--------------+--------+

Quindi se i buchi sono N, possiamo catture il topo in:

  giorni = (N - 2)*2

La sequenza di apertura dei buchi per catturare sicuramente il topo vale:
 
  sequenza = 2 3 ... (N - 2) (N - 1) (N - 1) (N - 2) ... 3 2

Per 7 buchi la sequenza di cattura vale: 2, 3, 4, 5, 6, 6, 5, 4, 3, 2 e quindi occorrono 10 giorni per catturare sicuramente il topo.

(define (topo N)
  (local (sequenza T cattura giorni teorico)
    ; valore massimo di giorni per la cattura
    (setq teorico (* (- N 2) 2))
    ; sequenza di cattura
    (setq sequenza (append (sequence 2 (- N 1)) (sequence (- N 1) 2)))
    ;(println sequenza)
    ; posizione iniziale del topo
    (setq T (+ 1 (rand N)))
    (setq cattura nil)
    (setq giorni 0)
    ; Controllo dei buchi della sequenza
    (dolist (buco sequenza cattura)
      (++ giorni)
      ;(println giorni { } T { } buco)
      (if (= buco T) ; abbiamo trovato il topo
        (setq cattura true)
        ;else
        ; Spostamento del topo
        (cond ((= T 1) (setq T 2)) ; spostamento a sinistra
              ((= T N) (setq T (- N 1))) ; spostamento a destra
              (true ; spostamento a sinistra o a destra
                (if (zero? (rand 2))
                    (-- T)
                    (++ T))))))
    ;(if (> giorni teorico) nil giorni)))
    giorni))

Proviamo:

(seed (time-of-day) true) ; per il 'rand' delle funzione 'topo'

(collect (topo 7) 10)
;-> (5 1 10 5 8 10 1 5 10 6)

Con 100 buchi e 10000 prove:
(find (* (- 100 2) 2) (collect (topo 100) 10000) <)
;-> nil

Vediamo quanti giorni occorrono in media per catturare un topo avendo N buchi:

(define (media N iter)
  (div (apply + (collect (topo N) iter)) iter))

(println "Buchi  Media-giorni  Buchi/Media-giorni")
(for (i 3 20)
  (setq m (media i 1e6))
  (println i { } m { } (div i m)))
;-> Buchi  Media-giorni  Buchi/Media-giorni
;-> 3 1.666501 1.800178937786416
;-> 4 2.43733 1.641140100027489
;-> 5 3.546799 1.409721836506664
;-> 6 4.359293 1.376369975590079
;-> 7 5.52161 1.267746182725691
;-> 8 6.336201 1.262586208991792
;-> 9 7.50471 1.199246872963779
;-> 10 8.3347 1.199803232269908
;-> 11 9.506385 1.157117032394543
;-> 12 10.333099 1.161316658245508
;-> 13 11.498897 1.130543216449369
;-> 14 12.354373 1.133201984430938
;-> 15 13.492459 1.1117321164363
;-> 16 14.344023 1.115447179637121
;-> 17 15.485385 1.097809321498949
;-> 18 16.322269 1.102787853821059
;-> 19 17.51138 1.085008720043766
;-> 20 18.333053 1.090925772155898

Media giorni di cattura con 1000 buchi:
(time (println (media 1000 1e4)))
;-> 999.20442
;-> 21720.845

Media giorni di cattura con 10000 buchi:
(time (println (media 10000 1e4)))
;-> 10019.4476
;-> 22288.166

All'aumentare del numero di buchi N, il numero medio di giorni per catturare il topo vale ~N.


-------------------
I salti del canguro
-------------------

Un canguro si trova all'inizio di una linea orizzontale (nel punto 0).
La linea è lunga da 0 a N.
Il canguro può fare salti di lunghezza compresa tra 1 e M (con M <= N).
Quanti modi ha il canguro di raggiungere la fine della linea (il punto N).

Esempio:
  N = 6
  M = 2
  C
  0--1--2--3--4--5--6

Se il canguro può saltare di lunghezza da 1 a M, il numero di modi per arrivare a N soddisfa la ricorrenza:

  f(0) = 1
  f(N) = f(N-1) + f(N-2) + ... + f(N-M)
dove i termini con indice negativo valgono 0.
Questa è la M-step Fibonacci (o Fibonacci generalizzata).

Esempio:
N = 6
M = 2, Il canguro può saltare solo di 1 oppure  di 2.
f(6) = 13, quindi ci sono 13 modi.
Le sequenze dei salti sono:
6 = 1+1+1+1+1+1
  = 1+1+1+1+2
  = 1+1+1+2+1
  = 1+1+2+1+1
  = 1+2+1+1+1
  = 2+1+1+1+1
  = 1+1+2+2
  = 1+2+1+2
  = 1+2+2+1
  = 2+1+1+2
  = 2+1+2+1
  = 2+2+1+1
  = 2+2+2

Possiamo quindi definire il caso generale:

            | 1,  per N = 0
  f(N, M) = |
            | Sum[k=1,min(N,M)]f(N - k, M),  per N > 0

e il caso in cui il salto può arrivare fino a N, è semplicemente M = N, da cui:

  f(N,N)=2^(N-1), per (N > 0).

Infatti in questo caso il canguro può scegliere ognuno dei punti interni come punto di arrivo di un salto oppure può oltrepassarlo.
Quindi il numero di modi per raggiungere la fine vale: 2^(N-1).
Per N = 6 abbiamo 5 punti interni (1 2 3 4 5), quindi abbiamo:
  2^(6-1) = 2^5 = 32 modi

Scriviamo una funzione iterativa per calcolare il numero di modi dato N e M.

(define (kangaroo1 N M)
  ; f[i] = numero di modi per raggiungere il punto i
  (let (f (array (+ N 1) '(0L)))
    ; Esiste un modo per raggiungere il punto 0: non fare salti
    (setf (f 0) 1L)
    ; Calcola f[1], f[2], ..., f[N]
    (for (i 1 N)
      (for (k 1 (min M i))
        (setf (f i)
          (+ (f i) (f (- i k))))))
    (f N)))

Proviamo:

(kangaroo1 6 2)
;-> 13L
(kangaroo1 6 6)
;-> 32L
(kangaroo1 6 1)
;-> 1L
(kangaroo1 100 100)
;-> 633825300114114700748351602688L
(time (println (kangaroo1 1000 1000)))
;-> 535754303593133660474212524530000905280702405852766803721875194185175525
;-> 562468061246599189407847929063797336458776573412593572642846157021799228
;-> 878734928740196728388741211549271053730253118557093897709107652323749179
;-> 097063369938377958277197303853145728559823884327108383021491582631219341
;-> 8602834034688L
;-> 93.721

Si può anche ottimizzare evitando il ciclo interno, perché la somma degli ultimi M termini può essere mantenuta con una 'sliding window' (finestra scorrevole), portando il calcolo da O(N*M) a O(N).
Eliminiamo il ciclo interno mantenendo la somma degli ultimi 'M' valori.
La differenza è che la versione precedente calcola ogni volta:
  f(i-1) + f(i-2) + ... + f(i-M)
da zero, quindi richiede O(N*M) operazioni.
Questa invece mantiene tale somma in 'sum': a ogni passo aggiunge il nuovo termine ed elimina quello che esce dalla finestra.
La complessità diventa quindi O(N), mentre la memoria rimane O(N).

(define (kangaroo2 N M)
  ; f[i] = numero di modi per raggiungere il punto i
  (let ( (f (array (+ N 1) '(0L)))
         (sum 1L))
    (setf (f 0) 1L)
    (for (i 1 N)
      ; La somma corrente degli ultimi M valori e' f[i]
      (setf (f i) sum)
      ; Aggiunge f[i] alla finestra
      (setq sum (+ sum (f i)))
      ; Se la finestra supera M elementi, elimina il piu' vecchio
      (if (>= i M)
        (setq sum (- sum (f (- i M))))))
    (f N)))

(kangaroo2 6 2)
;-> 13L
(kangaroo2 6 6)
;-> 32L
(kangaroo2 6 1)
;-> 1L
(kangaroo2 100 100)
;-> 633825300114114700748351602688L
(time (println (kangaroo2 1000 1000)))
;-> 535754303593133660474212524530000905280702405852766803721875194185175525
;-> 562468061246599189407847929063797336458776573412593572642846157021799228
;-> 878734928740196728388741211549271053730253118557093897709107652323749179
;-> 097063369938377958277197303853145728559823884327108383021491582631219341
;-> 8602834034688L
;-> 15.585


-----------------------------------
Eventi statisticamente indipendenti
-----------------------------------

Si dice che due eventi A e B sono statisticamente indipendenti se e solo se:

  P(A and B) = P(A)P(B)

Intuitivamente, e' chiaro che se A e B sono eventi indipendenti, allora A e not(B) (la negazione di B) devono essere anch'essi indipendenti.

Partendo da:

  P(A) = P(A and (B or not(B))) =  P(A and B) + P(A and not(B))

Poichè, per ipotesi:

  P(A and B) = P(A)P(B)

ne consegue:

  P(A and not(B)) = P(A) - P(A)P(B) = P(A)(1 - P(B)) = P(A)P(not(B))

Quindi A e not(B) sono indipendenti.

Analogamente, anche not(A) e B sono statisticamente indipendenti:

  P(not(A) and B) = P(B) - P(A and B) = P(B) - P(A)P(B)= P(B)(1 - P(A)) =
                  = P(B)P(not(A))

quindi:

  P(not(A) and B) = P(not(A))P(B)

Anzi, si può completare il risultato: se A e B sono indipendenti, allora tutte e quattro le combinazioni sono indipendenti a coppie:

- A and B
- A and not(B)
- not(A) and B
- not(A) and not(B)

L'ultima si ottiene, ad esempio, da:

  P(not(A) and not(B)) = 1 - P(A or B) = 1 - P(A) - P(B) + P(A)P(B)

e quindi:

  P(not(A) and not(B)) = (1 - P(A))(1 - P(B)) = P(not(A))P(not(B))

E questo si estende naturalmente anche alla quarta combinazione.


-------------------
Non c'è più nessuno
-------------------

Supponiamo di avere N oggetti, ciascuno con una probabilità di 1/m di scomparire ogni secondo.
1) Qual è la media del numero di secondi necessari affinché tutti gli oggetti siano scomparsi?
2) Qual è la probabilità che dopo k secondi siano scomparsi tutti gli N oggetti?

Nota: "1/m per secondo" deve essere applicato in modo discontinuo su una sequenza di intervalli discreti di 1 secondo.
In altre parole, al termine di ogni secondo viene applicata la regola "1/m per secondo" ad ogni oggetto presente.

1) Calcolo della media
----------------------
Per calcolare la media scriviamo una funzione che simula tante volte questo processo.

(define (media N m iter)
  (let ((sec 0) (tot-sec 0) (prob (div m)))
    ; Ciclo di 'iter' simulazioni...
    (for (prove 1 iter)
      ; Inizio di una simulazione...
      ; all'inizio il numero degli oggetti vale N
      (setq oggetti N)
      ; e i secondi valgono 0
      (setq sec 0)
      ; Finchè ci sono oggetti...
      (while (> oggetti 0)
        (++ sec)
        ; Ciclo per la scomparsa degli oggetti
        (for (t 1 oggetti)
          (if (> prob (random)) (-- oggetti))))
      ; ... prova finita.
      ; Aggiorna i secondi totali
      (++ tot-sec sec))
    (div tot-sec iter)))

Proviamo:

(seed (time-of-day) true)

(media 100 5 1e5)
;-> 23.75195
(media 100 1 1e5)
;-> 1
(media 100 2 1e5)
;-> 7.9837
(media 10 10 1e5)
;-> 28.30796
(media 10 100 1e5)
;-> 292.13475

Esiste anche una formula esatta per la media del tempo di scomparsa T.
Per una variabile casuale intera positiva vale:

  E(T) = Sum[k=0,infinito] P(T > k)

Poiche:

  P(T > k) = 1 - P(T <= k)

si ottiene:

  E(T) = Sum[k=0,infinito] (1 - (1 - q^k)^N)

e quindi:

  E(T) =  Sum[k=0,infinito] (1 - (1 - ((m-1)/m)^k)^N)

Questa formula fornisce la media teorica e puo essere confrontata con la funzione di simulazione.

(define (media-teorica N m iter)
  (let (media 0)
    (for (k 0 iter)
      (setq media (add media (sub 1 (pow (sub 1 (pow (div (sub m 1) m) k)) N)))))
    media))

Proviamo:
(media-teorica 100 5 1e5)
;-> 23.74681796578223
(media-teorica 100 1 1e5)
;-> 1
(media-teorica 100 2 1e5)
;-> 7.983801535156916
(media-teorica 10 10 1e5)
;-> 28.29948670221649
(media-teorica 10 100 1e5)
;-> 291.9298881810799

I valori teorici sono molto simili a quelli della simulazione.

2) Calcolo della probabilità
----------------------------
Adesso calcoliamo la probabilità che dopo k secondi siano scomparsi tutti gli N oggetti.
Ogni singolo oggetto ha una probabilità di 1.0 di essere presente all'inizio del primo intervallo di 1 secondo e una probabilità di (m-1)/m di essere presente alla fine del primo intervallo.
In generale, ogni oggetto ha una probabilità di [(m-1)/m]^k di essere presente alla fine del k-esimo intervallo, quindi la sua probabilità di NON essere presente alla fine del k-esimo intervallo è semplicemente il complemento di questo valore, ovvero 1 - [(m-1)/m]^k.
Ne consegue che la probabilità che TUTTI gli n oggetti siano scomparsi alla fine del k-esimo intervallo è il prodotto delle loro singole probabilità di scomparire, quindi è data da (1 - ((m-1)/m)^k)^N.
Questa è la probabilità cumulativa, che tende a 1.0 all'aumentare di k.

(define (prob N m k)
  (pow (sub 1 (pow (div (sub m 1) m) k)) N))

Proviamo:
(prob 100 1 1)
;-> 1

(prob 100 5 25)
;-> 0.6848847213284477
cioè, con 100 oggetti e m = 5, dopo 25 secondi c'e circa il 68.49% di probabilita che siano gia scomparsi tutti.

Quindi i risultati della simulazione sono coerenti con la formula teorica.

Possiamo inoltre calcolare la probabilita che T sia esattamente uguale a k, anziche essere minore o uguale a k.
Basta fare la differenza tra due probabilita cumulative:

  P(T = k) = P(T <= k) - P(T <= k-1)

quindi:

  P(T = k) = (1 - q^k)^N - (1 - q^(k-1))^N
  P(T = k) = (1 - ((m-1)/m)^k)^N  - (1 - ((m-1)/m)^(k-1))^N

(define (prob-exact N m k)
  (let (q (div (sub m 1) m))
    (sub (pow (sub 1 (pow q k)) N)
         (pow (sub 1 (pow q (sub k 1))) N))))

Questa è la distribuzione di probabilita del numero di secondi necessari affinche scompaia l'ultimo dei N oggetti.

(prob-exact 100 5 25)
;-> 0.06197595190287508
cioè con 100 oggetti e m=5, la probabilità che l'ultimo oggetto scompaia esattamente dopo 25 secondi vale circa il 6.2%.

(prob-exact 10 1 2)
;-> 0
cioè con 10 oggetti e m=1, la probabilità che l'ultimo oggetto scompaia esattamente dopo 2 secondi vale circa lo 0%.
Questo perchè la probabilità di scomparsa vale 1/m = 1, quindi dopo 1 secondo sono scomparsi tutti gli oggetti e nei successivi secondi non può più scomparire alcun oggetto.

L'espressione (prob 100 5 25) calcola la probabilita che entro il secondo 25 siano scomparsi tutti gli oggetti (cioè gli oggetti possono essere scomparsi tutti anche prima del 25 secondo). 
Mentre (prob-exact 100 5 25) calcola la probabilitè che esattamente al secondo 25 scompaia l'ultimo oggetto.

Una verifica interessante e che sommando (prob-exact) per tutti i possibili secondi si deve ottenere 1:

  P(T=1) + P(T=2) + P(T=3) + ... = 1

(setq tot 0)
(for (i 1 100) (setq tot (add tot (prob-exact 100 5 i))))
;-> 0.9999999796296392

Quindi (prob-exact) descrive la distribuzione del tempo di completamento, mentre (prob) descrive la sua distribuzione cumulativa.


------------------
Monete con memoria
------------------

Una moneta "equa" ha due caratteristiche:
1) la probabilità delle due facce è la stessa (1/2 = 0.5)
2) non ha memoria, vale a dire che la probabilità di un risultato particolare al lancio successivo è indipendente dal lancio precedente.
Comunque possiamo pensare ad una moneta che ha il seguente comportamento:
1) al primo lancio esce Testa o Croce con entrambe le probabilità pari a 1/2.
2) ad ogni lancio seguente, c'è una probabilità di 2/3 che il risultato sia uguale a quello del lancio precedente e una probabilità di 1/3 che il risultato sia diverso.

Supponiamo di lanciare questa questa moneta 4 volte e vediamo tutti i possibili risultati:

                                    +-----+
                                    |  T  |
                                    |  1  |
                                    +-----+
                                      | |
                   ___________________| |___________________
                   |                                       |
                   |                                       |
                +-----+                                 +-----+
                |  T  |                                 |  C  |
                | 2/3 |                                 | 1/3 |
                +-----+                                 +-----+
          2/3     | |     1/3                     1/3     | |     2/3
         _________| |_________                   _________| |_________
         |                   |                   |                   |
         |                   |                   |                   |
      +-----+             +-----+             +-----+             +-----+
      |  T  |             |  C  |             |  T  |             |  C  |
      | 4/9 |             | 2/9 |             | 1/9 |             | 2/9 |
      +-----+             +-----+             +-----+             +-----+
    2/3 | | 1/3             | |                 | |                 | |
    ____| |____         ____| |____         ____| |____         ____| |____
    |         |         |         |         |         |         |         |
    |         |         |         |         |         |         |         |
 +-----+   +-----+   +-----+   +-----+   +-----+   +-----+   +-----+   +-----+
 |  T  |   |  C  |   |  T  |   |  C  |   |  T  |   |  C  |   |  T  |   |  C  |
 | 8/27|   | 4/27|   | 2/27|   | 4/27|   | 1/17|   | 1/27|   | 2/27|   | 4/27|
 +-----+   +-----+   +-----+   +-----+   +-----+   +-----+   +-----+   +-----+


Pertanto, i risultati del terzo lancio dopo una testa hanno le probabilità a priori:

  P(T) = 8/27 + 2/27 + 2/27 + 2/27 = 14/27
  P(C) = 4/27 + 4/27 + 1/27 + 4/27 = 13/27

Quindi le probabilità di Testa e Croce al k-esimo lancio dopo una Testa iniziale valgono:

  P(Tk-T) = (1/2)*(1 + 1/3^k)
  P(Ck-T) = (1/2)*(1 - 1/3^k)

(define (TkT k) (mul 0.5 (add 1 (div (pow 3 k)))))
(define (CkT k) (mul 0.5 (sub 1 (div (pow 3 k)))))

(TkT 3)
;-> 0.5185185185185185
(div 14 27)
;-> 0.5185185185185185

(CkT 3)
;-> 0.4814814814814815
(div 13 27)
;-> 0.4814814814814815

Analogamente le probabilità di Testa e Croce al k-esimo lancio dopo una Croce iniziale valgono:

  P(Ck-C) = (1/2)*(1 + 1/3^k)
  P(Tk-C) = (1/2)*(1 - 1/3^k)

(define (CkC k) (mul 0.5 (add 1 (div (pow 3 k)))))
(define (TkC k) (mul 0.5 (sub 1 (div (pow 3 k)))))

(CkC 3)
;-> 0.5185185185185185
(div 14 27)
;-> 0.5185185185185185

(TkC 3)
;-> 0.4814814814814815
(div 13 27)
;-> 0.4814814814814815

La somma delle probabilità di Testa o Croce al k-esimo lancio dopo una Testa o una Croce valgono:

 P(Tk) = P(TkT) + P(TkC) = 1
 P(Ck) = P(CkT) + P(CkC) = 1

Quindi le probabilità complessive convergono a 1/2.

Una moneta del genere tratta le due facce allo stesso modo e non favorisce un risultato rispetto a un altro poichè le frazioni asintotiche di Testa e Croce sono entrambe pari a 1/2.
Però esistono correlazioni tra i lanci consecutivi.
Anche se la memoria esplicita della moneta si estende solo a un lancio precedente, le correlazioni si propagano per un numero infinito di lanci, sebbene il loro peso diminuisce con l'aumentare dei lanci.

Verifichiamo con una simulazione che questa moneta non favorisce alcun risultato.

; Imposta la probabilità delle due facce della moneta
; in base al risultato del lancio precedente 'prev'
(define (set-prob prev)
  (if (= prev 'T)
    (begin
      (setq pT (div 2 3))
      (setq pC (sub 1 pT)))
    ;else
    (begin
      (setq pC (div 2 3))
      (setq pT (sub 1 pC)))))

; Calcola il risultato della moneta al termine di un dato numero di lanci
; Il lancio 0 vale Testa o Croce con probabilità al 50% (1/2)
(define (moneta lanci)
  (local (prev pT pC)
    ; lancio iniziale:
    ; P(T) = 1/2
    ; P(C) = 1/2
    (if (zero? (rand 2))
        (setq prev 'T)
        (setq prev 'C))
    ; ciclo di k lanci...
    (for (k 1 lanci)
      ; imposta le probabilità correnti (pT e pC) in base
      ; al risultato del lancio precedente
      (set-prob prev)
      ;(print prev { } pT { } pC) (read-line)
      ; calcola il risultato del lancio corrente
      (if (>= pT pC)
          (if (> pT (random))
              (setq prev 'T)
              (setq prev 'C))
          (if (> pC (random))
              (setq prev 'C)
              (setq prev 'T))))
    prev))

(seed (time-of-day) true)
(count '(T) (collect (moneta 1000) 1e5))
;-> (49838)
(count '(C) (collect (moneta 1000) 1e5))
;-> (50186)

Scriviamo una funzione che calcola la probabilita di ottenere T o C al k-esimo lancio dopo una prima Testa o una prima Croce.

(define (moneta-fissa init lanci)
  (local (prev pT pC)
    ; lancio iniziale:
    (setq prev init)
    ; ciclo di k lanci...
    (for (k 1 lanci)
      ; imposta le probabilità correnti (pT e pC) in base
      ; al risultato del lancio precedente
      (set-prob prev)
      ;(print prev { } pT { } pC) (read-line)
      ; calcola il risultato del lancio corrente
      (if (>= pT pC)
          (if (> pT (random))
              (setq prev 'T)
              (setq prev 'C))
          (if (> pC (random))
              (setq prev 'C)
              (setq prev 'T))))
    prev))

(seed (time-of-day) true)

prima Testa
Testa
(dolist (lanci '(1 2 3 4 100 1000))
  (println (count '(T) (collect (moneta-fissa 'T lanci) 1e5))))
;-> (66737)
;-> (55592)
;-> (51803)
;-> (50567)
;-> (49741)
;-> (49863)
;-> Croce
(dolist (lanci '(1 2 3 4 100 1000))
  (println (count '(C) (collect (moneta-fissa 'T lanci) 1e5))))

prima Croce
Testa
(dolist (lanci '(1 2 3 4 100 1000))
  (println (count '(T) (collect (moneta-fissa 'C lanci) 1e5))))
;-> (33284)
;-> (44435)
;-> (48004)
;-> (49743)
;-> (49971)
;-> (50093)
Croce
(dolist (lanci '(1 2 3 4 100 1000))
  (println (count '(C) (collect (moneta-fissa 'C lanci) 1e5))))
;-> (66495)
;-> (55603)
;-> (51641)
;-> (50182)
;-> (49988)
;-> (49847)
;-> ()


---------------------
Paradosso di Bertrand
---------------------

Il paradosso della scatola di Bertrand è un paradosso veritiero nella teoria elementare della probabilità.
Fu formulato per la prima volta da Joseph Bertrand nel 1889.
Un paradosso veritiero è un paradosso la cui soluzione corretta sembra essere controintuitiva.

Ci sono tre scatole:
1) una scatola contenente due monete d'oro,
2) una scatola contenente due monete d'argento,
3) una scatola contenente una moneta d'oro e una d'argento.
Una moneta estratta a caso da una delle tre scatole risulta essere d'oro.
Qual è la probabilità che anche l'altra moneta estratta dalla stessa scatola sia d'oro?

Potrebbe sembrare intuitivo che la probabilità che la moneta rimanente sia d'oro sia 1/2, ma la probabilità corretta vale 2/3.
L'attenzione deve essere posta all'inizio, cioè all'azione in cui si estrae casualmente una moneta d'oro da una delle tre scatole.
La moneta è d'oro deve essere stata estratta dalla scatola 1 o dalla scatola 3.
La probabilità di scegliere la moneta d'oro della scatola 1 vale 2/3, mentre la probabilità di scegliere la moneta d'oro della scatola 3 vale 1/3.
Quindi abbiamo 2/3 di probabilità di scegliere all'inizio la scatola 1.
Questo significa che 2/3 delle volte la moneta della scatola scelta è d'oro (perchè 2/3 delle volte scegliamo la scatola 1).
Alternativamente possiamo notare che:
All'inizio tutte e 3 le monete d'oro hanno la stessa probabilità di essere scelte, ma poichè nella scatola 1 ci sono 2 monete d'oro, allora la probabilità di scegliere la scatola 1 vale 2/3.

Scriviamo una funzione per simulare il processo.

(define (paradox iter)
  (let ((conta1 0) (conta3 0))
    (for (prove 1 iter)
      (setq gold-selected (rand 3))
      (cond ((= gold-selected 0) (setq box 1))
            ((= gold-selected 1) (setq box 1))
            ((= gold-selected 2) (setq box 3)))
      (if (= box 1) (++ conta1) (++ conta3)))
    (list (div conta1 iter) (div conta3 iter))))

(paradox 1e7)
;-> (0.666874 0.333126)

Anche se simuliamo prima la scelta della scatola e poi l'estrazione della moneta d'oro, il risultato è lo stesso.

(define (paradox2 iter)
  (let ((conta1 0) (conta3 0) (prove 0))
    (while (< prove iter)
      ; scelta della scatola (tranne la 1)
      (while (= (setq box-selected (+ (rand 3) 1)) 2))
      (cond ((= box-selected 1) ; abbiamo scelto la scatola 1
              (++ conta1)
              (++ prove))
            ((= box-selected 3) ; abbiamo scelto la scatola 3
              ; estrazione della moneta dalla scatola 3
              ; 0 = oro, 1 = argento
              (if (zero? (rand 2)) ; la prova vale solo se estraiamo oro (0)
                  (++ conta3)
                  (++ prove)))))
    (list (div conta1 iter) (div conta3 iter))))

(paradox2 1e7)
;-> (0.6663978 0.3332409)


----------------------------------------
Formiche che passeggiano lungo una linea
----------------------------------------

Lungo una linea lunga L centimetri camminano N formiche.
All'inizio le formiche si trovano tutte in posizioni casuali lungo la linea.
Ogni formica ha anche una direzione casuale iniziale (destra o sinistra) e si muove di un centimetro ad ogni secondo.
Quando due formiche si scontrano entrambe invertono la loro direzione.
Se una formica supera l'inizio (0) o la fine della tavola (L), allora viene eliminata.
Scrivere una funzione che simula il processo della passeggiata.
Scrivere una funzione più corta che calcola quanti secondi occorrono per eliminare tutte le formiche.

Esempio:
  L = 7
  Formiche = 3
      <       >           <
      F1      F2          F3
  -----------------------------
  0   1   2   3   4   5   6   7

Scontro tra due formiche
------------------------

Caso 1: distanza tra formiche = 0
---------------------------------
Posizione F1 = 4
Direzione = > (destra)

Posizione F2 = 5
Direzione = < (sinistra)

                  >   <
                  F1  F2
  -----------------------------
  0   1   2   3   4   5   6

Le formiche F1 e F2 si scontreranno al prossimo secondo.
Lo scontro non produce uno spostamento delle due formiche (F1 rimane a 4 e F2 rimane a 5), vengono invertite solo le loro direzioni, cioè al successivo secondo F1 andrà a sinistra e F2 andrà a destra.

Caso 2: distanza tra formiche = 0
---------------------------------
Posizione F1 = 3
Direzione = -> (destra)

Posizione F2 = 5
Direzione = <- (sinistra)

              >       <
              F1      F2
  -----------------------------
  0   1   2   3   4   5   6

Le formiche F1 e F2 occuperanno la stessa casa al prossimo secondo.
Questo produce uno spostamento delle due formiche (F1 va a 4 e F2 va a 4) e l'inversione delle loro direzioni, cioè al successivo secondo F1 andrà sinistra (partendo da 4) e F2 va a destra (partendo da 4).
Lo scontro produce uno spostamento delle due formiche (F1 va a 4 e F2 va a 4) e vengono invertite le loro direzioni, cioè al successivo secondo F1 andrà a sinistra (partendo da 4) e F2 andrà a destra (partendo da 4).

Scriviamo la funzione che simula il processo.

(define (find-from-end value lst)
"Find an element from the end of a list"
  (let (idx (find value (reverse lst)))
    (if idx (- (length lst) 1 idx))))

; stampa la posizione delle formiche
(define (print-ants)
  (let ((pos-str (dup " " (+ L 1)))  ; posizione "*"
        (dir-str (dup " " (+ L 1)))  ; direzione ">" o "<"
        (dir2-str (dup " " (+ L 1)))) ; direzione per punti doppi
    (dolist (el ants)
      (if (= (el 1) 0) ; direzione formica corrente: sinistra
          (if (= (pos-str (el 0)) " ")
              (begin
                (setf (pos-str (el 0)) "*")      ; posizione
                (setf (dir-str (el 0)) "<"))     ; sinistra
              (begin
                (setf (pos-str (el 0)) "*")
                (setf (dir2-str (el 0)) "<")))   ; sinistra (punti doppi)
          ;else ; direzione formica corrente: destra
          (if (= (pos-str (el 0)) " ")
              (begin
                (setf (pos-str (el 0)) "*")      ; posizione
                (setf (dir-str (el 0)) ">"))     ; destra
              (begin
                (setf (pos-str (el 0)) "*")
                (setf (dir2-str (el 0)) ">"))))) ; destra (punti doppi)
    (println "Passi: " sec)
    (println dir2-str) ; direzione punti doppi
    (println dir-str)  ; direzione
    (println pos-str)  ; posizione
    (println (dup "-" (+ L 1))) '>))

La funzione 'move-ants' muove tutte le formiche di un passo e non simula gli scontri tra due formiche perchè non è necessario.
Infatti possiamo considerare ogni scontro (di entrambi i tipi) come uno scambio di direzione tra le due formiche.
In questo modo possiamo considerare che entrambe le formiche proseguano il loro percorso indisturbate.
Per capire meglio, immaginiamo che ogni formica ha una maglietta con stampata la propria direzione:
quando due formiche si scontrano si scambiano le magliette.
Questo fa in modo che le due magliette proseguono il loro percorso indisturbate.
I passi che fanno le due magliette sono gli stessi che fanno le due formiche.

; Muove di un passo tutte le formiche
(define (move-ants)
  (local (cur-ants cur-pos cur-dir new-pos idx)
    ; copia della lista delle formiche
    (setq cur-ants ants)
    ; scorriamo la copia mentre modifichiamo la lista originale 'ants'
    (dolist (el cur-ants)
      ; posizione formica corrente
      (setq cur-pos (el 0))
      ; direzione formica corrente
      (setq cur-dir (el 1))
      ; calcolo della nuova posizione della formica corrente
      (if (= cur-dir 0)
          (setq new-pos (- cur-pos 1)) ; -1 -> va a sinistra
          (setq new-pos (+ cur-pos 1))); +1 -> va a destra
      ; controllo se la formica si trova ai bordi (0 o L)
      (if (or (and (= cur-pos 0) (= cur-dir 0))
              (and (= cur-pos L) (= cur-dir 1)))
              ; elimina la formica corrente
              (pop ants (ref el ants))
              ;else
              ; aggiorna la posizione della formica corrente
              (begin
                ; Usiamo 'update-from-end' per evitare che
                ; nel caso di punti coincidenti venga aggiornato il
                ; punto sbagliato (occorre aggiornare sempre l'ultimo punto)
                (setq idx (find-from-end el ants))
                (setf (ants idx) (list new-pos cur-dir)))))))

; Simula il processo di una passeggiata di N formiche su una linea lunga L
(define (walk L N show formiche)
  (local (sec line pos dir ants)
    (if formiche
        ; la lista delle formiche viene data come parametro (formiche)
        ; la lunghezza della linea viene data come parametro (L)
        (begin
          (setq ants formiche)
          (setq N (length ants)))
        ;else
          (begin
          ; la lista delle formiche viene costruita in modo casuale
          ; con una linea lunga L in cui ci sono N formiche
          ; linea: lista di numeri da 0 a L
          (setq line (sequence 0 L))
          ; posizione delle formiche lungo la linea
          (setq pos (slice (randomize line) 0 N))
          ; direction: 0 = sx, 1 = dx
          (setq dir (rand 2 N))
          ; lista delle formiche ((f1 dir1) (f2 dir2) ... (fN dirN))
          (setq ants (map list pos dir))))
    (println "Formiche: " ants)
    ; numero secondi
    (setq sec 0)
    ; stampa posizione iniziale
    (when show (print-ants) (read-line))
    ; ciclo finchè esistono formiche nella tavola...
    (while ants
      (++ sec)
      ; muove tutte le formiche di un passo
      (move-ants)
      ; stampa posizione corrente delle formiche
      (when show (print-ants) (read-line)))
    ; numero di secondi necessario per eliminare tutte le formiche
    (println "Secondi: " sec) '>))

Proviamo:

; Usare sempre la seguente espressione prima di uilizzare
; le funzioni casuali: rand, random, amb, ecc.
(seed (time-of-day) true)

(walk 10 4 true)
;-> Formiche: ((10 1) (6 0) (4 1) (1 0))
;-> Passi: 0           Passi: 1           Passi: 3           Passi: 4
;->                         >
;->  <  > <   >        <    <                <   >             <     >
;->  *  * *   *        *    *                *   *             *     *
;-> -----------        -----------        -----------        -----------
;->
;-> Passi: 5           Passi: 6           Passi: 7
;->
;->  <       >         <         >
;->  *       *         *         *
;-> -----------        -----------        -----------
;->
;-> Secondi: 7

(walk 20 6)
;-> Formiche: ((15 1) (10 1) (5 0) (16 0) (11 1) (12 0))
;-> Secondi: 17

(walk 40 10 true)
;-> Formiche: ((2 1) (28 1) (21 0) (8 0) (39 0) (10 1)
;->            (37 0) (30 0) (18 0) (22 1))
;-> Passi: 0
;->
;->   >     < >       <  <>     > <      < <
;->   *     * *       *  **     * *      * *
;-> -----------------------------------------
;-> ...
;-> Secondi: 40

(walk 70 20 true)
;-> Formiche: ((49 1) (41 1) (26 0) (36 1) (9 1) (53 1) (45 0) (46 0) (51 1)
;->            (58 0) (19 1) (6 1) (63 0) (65 1) (0 0) (55 0) (18 1) (5 1)
;->            (69 0) (35 1))
;-> Passi: 0
;->
;-> <    >>  >        >>      <        >>    >   <<  > > > <  <    < >   <
;-> *    **  *        **      *        **    *   **  * * * *  *    * *   *
;-> -----------------------------------------------------------------------
;-> ...
;-> Secondi: 70

Adesso scriviamo una funzione che calcola quanti secondi occorrono per eliminare tutte le formiche.
La considerazione che ci ha permesso di non simulare gli effetti degli scontri tra formiche ci permette anche di calcolare in tempo di eliminazione in modo semplice.
Infatti possiamo considerare che ogni formica parte dalla sua posizione iniziale e arriva al termine della linea (inizio o fine) in base alla propria posizione e alla propria direzione.
Per esempio, se abbiamo una linea lunga 10, una formica in posizione 3 e direzione sinistra (verso lo 0), allora la formica impiega 3 + 1 = 4 secondi per essere terminata.
Se la direzione fosse stata la destra, allora la formica stessa sarebbe terminata in 10 - 3 + 1 = 8 secondi.
Quindi il tempo massimo è dato dalla formica che si trova più lontana da uno dei termini (0 o L) della linea.
In questo caso supponiamo che la lista iniziale delle formiche sia data nel seguente formato:
((posizione1 direzione1) (posizione2 direzione2) ... (posizioneN direzioneN))
Dove le direzioni ha uno dei seguenti valori:
  direzione sinistra = 0
  direzione destra = 1

(define (total-time L ants)
  (let ((cur-tempo 0) (max-tempo 0))
    (dolist (el ants)
      (if (= (el 1) 0)
        ; tempo necessario per eliminare la formica corrente
        ; andando verso sinistra (secondi)
        (setq cur-tempo (+ (el 0) 1))
        ;else
        ; tempo necessario per eliminare la formica corrente
        ; andando verso destra (secondi)
        (setq cur-tempo (- L (el 0) (- 1))))
      ; aggiornamento del tempo massimo (secondi)
      (if (> cur-tempo max-tempo) (setq max-tempo cur-tempo)))
      max-tempo))

Proviamo:

(total-time 10 '((10 1) (6 0) (4 1) (1 0)))
;-> 7

(total-time 20 '((15 1) (10 1) (5 0) (16 0) (11 1) (12 0)))
;-> 17

(total-time 40 '((2 1) (28 1) (21 0) (8 0) (39 0) (10 1) (37 0) (30 0)
                (18 0) (22 1)))
;-> 40

(total-time 70 '((49 1) (41 1) (26 0) (36 1) (9 1) (53 1) (45 0) (46 0) (51 1)
     (58 0) (19 1) (6 1) (63 0) (65 1) (0 0) (55 0) (18 1) (5 1)
     (69 0) (35 1)))
;-> 70

Tutti i tempi di eliminazione calcolati da 'walk' e 'total-time' coincidono.
All'aumentare delle formiche aumenta la probabilità che una di esse si possa trovare a 0 con direzione destra o a L con direzione sinistra, in questo caso il numero dei secondi vale (L + 1).

Versione code-golf (112 caratteri):

(define(f L a)(let((t 0)(T 0))
(dolist(x a)(setq t(if(=(x 1)0)(+(x 0)1)(- L(x 0)(- 1))))
(if(> t T)(setq T t)))T))

(f 10 '((10 1) (6 0) (4 1) (1 0)))
;-> 7

(f 20 '((15 1) (10 1) (5 0) (16 0) (11 1) (12 0)))
;-> 17

(f 40 '((2 1) (28 1) (21 0) (8 0) (39 0) (10 1) (37 0) (30 0)
                (18 0) (22 1)))
;-> 40

(f 70 '((49 1) (41 1) (26 0) (36 1) (9 1) (53 1) (45 0) (46 0) (51 1)
     (58 0) (19 1) (6 1) (63 0) (65 1) (0 0) (55 0) (18 1) (5 1)
     (69 0) (35 1)))
;-> 70


----------------------------
La formica lungo un elastico
----------------------------

Una formica si trova a un'estremità di un elastico lungo 100 metri, come mostrato di seguito.

Si muove verso l'altra estremità a una velocità costante di 1 cm al secondo.
Alla fine di ogni secondo, l'elastico si allunga di 100 metri.
In altre parole, quando la formica ha percorso 1 cm, l'elastico è lungo 200 m, quando ne ha percorsi 2 cm, è lungo 300 m e così via.
Tuttavia, allungando l'elastico di un metro, anche la posizione della formica si allunga/sposta.
Riuscirà la formica a raggiungere l'estremità dell'elastico?

Come si muove la formica
------------------------
Dopo un secondo, la formica si è spostata di 1 cm lungo l'elastico, che, allungandosi, sposta la formica a 2 cm, poiché allungare l'elastico da 1 m a 2 m ha l'effetto di raddoppiare la distanza tra due punti qualsiasi.
Dopo un altro secondo, la formica si trova a 3 cm dall'estremità sinistra, distanza che, quando si allunga l'elastico, diventa di 4.5 cm, poiché allungare l'elastico da 2 m a 3 m ha l'effetto di moltiplicare per 3/2 la distanza tra due punti qualsiasi.
In altre parole, la formica viene trascinata in avanti dall'allungamento e percorre una distanza crescente ogni secondo, il che forse le permette di arrivare in fondo.

Dal punto di vista matematico.
Nel primo secondo, la formica percorre 1 cm.
In altre parole, la formica percorre 1/100 della lunghezza dell'elastico, che è lungo 1 m.
L'elastico si allunga istantaneamente fino a 2 m.
Nel secondo successivo, la formica percorre un altro cm, che ora rappresenta 1/200 della lunghezza dell'elastico.
Dopo il terzo secondo, la lunghezza dell'elastico è di 3 m, quindi il cm percorso dalla formica rappresenta 1/300 della lunghezza dell'elastico.
E così via.
Sommando tutte queste frazioni, otteniamo la seguente espressione:

  1/100*(1 + 1/2 + 1/3 + 1/4 + ... + 1/N)

rappresenta la distanza percorsa dalla formica dopo N secondi, espressa come frazione della lunghezza totale dell'elastico.

Quindi quando il termine (1 + 1/2 + 1/3 + 1/4 + ... + 1/N) raggiunge 100, allora la formica ha raggiunto il termine della linea in N secondi.
Poichè (1 + 1/2 + 1/3 + 1/4 + ... + 1/N) è la 'serie armonica' che cresce indefinitamente, allora possiamo dire che la formica raggiungerà sicuramente la fine dell'elastico.

Per calcolare il valore della serie armonica per un dato N usiamo la seguente formula:

  Sum[k=1,N](1/k) ≈ ln(N) + K + 1/(2*N)

La costante K è la costante di Eulero-Mascheroni che vale:

  K = 0.577215664901532860606512090082402431042159335...

(define (harmonic num)
  (add (log num) 0.57721566490153286 (div (mul 2 num))))

Possiamo calcolare N utilizzando Newton-Raphson.

Definiamo:

  f(N) = ln(N) + K + 1/2N - X
  f'(N) = 1/N - (1/N^2) = (2N - 1)/(2N^2)

dove X è il valore della serie armonica per un certo N.

Quindi l'iterazione Newton vale:

            ln(N(i)) + K + 1/2N(i) - X
  N(i+1) = ----------------------------
                1/N(i) - 1/2N(i)^2

che per N >= 1 converge rapidamente.

La stima iniziale (exp (- X K)) deriva semplicemente dall'ignorare inizialmente il termine 1/(2*N).
Questa funzione restituisce il valore reale di N che soddisfa l'approssimazione

  X = log(N) + K + 1/(2*N)

non necessariamente l'intero N della serie armonica esatta.

(define (harmonic-inv X)
  ; Costante di Eulero-Mascheroni
  (let ((K 0.5772156649015329)
        (N (exp (sub X 0.5772156649015329)))
        (old 0.0)
        (stop nil))
    ; Iterazione di Newton-Raphson
    (for (i 0 20 1 stop)
      (setq old N)
      (setq N (sub N
              (div
                (sub (add (log N) K (div 1 (mul 2 N))) X)
                (sub (div 1 N) (div 1 (mul 2 N N))))))
      ; Se la variazione e' trascurabile, termina
      (if (< (abs (sub N old)) 1e-12)
        (setq stop true)))
    N))

Nel nostro caso X = 100:

(harmonic-inv 100)
;-> 1.509268862211383e+043

Verifichiamo questo risultato:

(harmonic (harmonic-inv 100))
;-> 100

Quindi dopo 1.509268862211383e+043 secondi la formica raggiunge la fine dell'elastico.

============================================================================

