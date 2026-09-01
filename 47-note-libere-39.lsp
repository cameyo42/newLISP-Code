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
- Elencare le coppie non può scendere sotto il costo proporzionale al **numero di coppie stesse**: se, nel caso peggiore, tutti gli `n` intervalli si sovrappongono a vicenda, ci sono O(n^2) coppie da restituire, e nessun algoritmo — per quanto elegante — può enumerarle in meno tempo di quello necessario a scriverle tutte.
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

============================================================================

