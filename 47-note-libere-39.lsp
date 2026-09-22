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
- Elencare le coppie non può scendere sotto il costo proporzionale al numero di coppie stesse: se, nel caso peggiore, tutti gli n intervalli si sovrappongono a vicenda, ci sono O(n^2) coppie da restituire, e nessun algoritmo — per quanto elegante — può enumerarle in meno tempo di quello necessario a scriverle tutte.
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


---------------------------
La formica lungo l'elastico
---------------------------

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


-------------
Boys and girl
-------------

Ipotizzando che ogni bambino nato abbia la stessa probabilità di essere maschio o femmina, e scegliendo a caso un genitore tra tutti quelli che hanno due figli, la probabilità che questo genitore abbia ALMENO una figlia è 3/4, poiché tre dei quattro esiti ugualmente probabili MM, MF, FM, FF includono almeno una figlia.

Invece, se scegliamo a caso un genitore tra tutti quelli che hanno due figli, di cui ALMENO uno maschio, allora i tre esiti ugualmente probabili sono MM, MF, FM, due dei quali hanno una figlia, quindi la probabilità che il genitore scelto abbia una figlia è 2/3. 

Naturalmente, se scegliamo a caso un genitore tra tutti quelli che hanno due figli, di cui il MAGGIORE è maschio, allora le famiglie possibili sono MM e MF, quindi la probabilità che il figlio minore sia una femmina è 1/2.

Analogamente, se selezioniamo a caso un genitore tra tutti i genitori che hanno due figli, il più GIOVANE dei quali è un maschio, allora le famiglie possibili sono MM e FM, quindi la probabilità che il figlio maggiore sia una femmina è 1/2.


-----------------------------------------------------
Principio di riflessione (disuguaglianza triangolare)
-----------------------------------------------------

Dati due punti A e B in un piano cartesiano, trovare il percorso minimo che congiunge i due punti A e B e passa per un punto qualunque di una linea orizzontale L.

Ipotesi A: la linea L si trova al di sotto di entrambi i punti A e B.

          A


                     B


  L
  -------------------------

La soluzione si trova eseguendo i seguenti passi:
1) Individuare il punto riflesso di B rispetto alla linea L.
   Chiamiamo questo punto B'.
2) Congiungere il punto A con il punto B' (segmento AB').
   Il segmento AB' attraversa la linea L in un punto P.
3) La distanza minima è data dalla somma dei segmenti AP e PB.
   Questa distanza è equivalente alla lunghezza del segmento AB'.

          A
           \
            \
             \       B
              \     /|
               \   / |
  L             \ /  |
  ---------------+---------
                 P\  |
                   \ |
                    \|
                     B'

Ipotesi B: la linea L si trova al di sopra di entrambi i punti A e B.

La soluzione si trova con gli stessi passi visti sopra per l'ipotesi A.

                     B'
                    /|
                   / |
  L              P/  |
  ---------------+---------
                / \  |
               /   \ |
              /     \|
             /       B
            /
           /
          A

Ipotesi C: la linea L si tra i punti A e B.

In questo caso la distanza minima è data dalla lunghezza del segmento AB (che attraversa necessariamente la linea L in un punto P).

          A
 L         \
 --------------------------
             \
              \
               \
                B

Scriviamo la funzione che calcola la distanza minima tra due punti A e B e che attraversa un punto qualunque di una linea orizzontale.

(define (dist2d p1 p2)
"Calculate 2D Cartesian distance of two points P1 = (x1 y1) and P2 = (x2 y2)"
  (let ( (x1 (p1 0)) (y1 (p1 1))
         (x2 (p2 0)) (y2 (p2 1)) )
    (sqrt (add (mul (sub x1 x2) (sub x1 x2))
               (mul (sub y1 y2) (sub y1 y2))))))

(define (minima A B L)
  (let ( (xa (A 0)) (ya (A 1))
         (xb (B 0)) (yb (B 1)) )
    (cond 
      ; la linea si trova tra i punti A e B
      ((or (apply > (list ya L yb))
           (apply < (list ya L yb)))
        (setq out (dist2d (list xa ya) (list xb yb))))
      ; la linea si trova sotto o sopra entrambi i punti A e B
      (true
        ; calcolo del punto riflesso di B rispetto alla linea L (xbr ybr)
        (setq xbr xb)
        (setq ybr (- (* 2 L) yb))
        (setq out (dist2d (list xa ya) (list xbr ybr)))))
    out))

Proviamo:

(minima '(3 3) '(4 6) 5)
;-> 3.16227766016838

(minima '(5 5) '(3 8) 12)
;-> 11.18033988749895

La soluzione usa il 'principio della riflessione' o 'disuguaglianza triangolare'.
Supponiamo che A e B siano dalla stessa parte della retta orizzontale L, e riflettiamo B ottenendo B'.

Per ogni punto P appartenente a L:

  AP + PB = AP + PB'

perché P appartiene alla retta di riflessione e quindi:

  PB = PB'

Ora A, P e B' formano un triangolo, quindi per la disuguaglianza triangolare:

  AP + PB' >= AB'

L'uguaglianza si verifica esattamente quando A, P e B' sono allineati.
Quindi:

  min(AP + PB) = AB'

Ed è proprio il metodo implementato dalla funzione 'minima'.

Possiamo calcolare anche le coordinate di P.
Se la retta L si trova sopra o sotto entrambi i punti A e B, si riflette B ottenendo B' e si calcola P come intersezione della retta AB' con L.
Il parametro della retta è:
  t = (L - ya) / (ybr - ya)
e quindi:
  xp = xa + t * (xb - xa)
  yp = L
Nel caso in cui L sia tra A e B, la distanza minima ♪ la lunghezza del segmanto AB e il punto P del percorso è invece l'intersezione del segmento AB con L, quindi la stessa formula usando yb non riflesso.

Versione code-golf (151 caratteri):

(define(f x y w z L)(if-not(or(apply >(list y L z))(apply <(list y L z)))
(setq z(-(* 2 L)z)))(sqrt(add(mul(sub x w)(sub x w))(mul(sub y z)(sub y z)))))

(f 3 3 4 6 5)
;-> 3.16227766016838

(f 5 5 3 8 12)
;-> 11.18033988749895


---------------
Ponte di barche
---------------

Lungo un fiume si trova un ponte di barche disposte a matrice.

  ======B==B==B==B==B==B==B==B==B====== riva superiore
        |  |  |  |  |  |  |  |  |
        B--B--B--B--B--B--B--B--B
        |  |  |  |  |  |  |  |  |
        B--B--B--B--B--B--B--B--B
        |  |  |  |  |  |  |  |  |
        B--B--B--B--B--B--B--B--B
        |  |  |  |  |  |  |  |  |
  ======B==B==B==B==B==B==B==B==B====== riva inferiore

Per attraversare il fiume si usano le barche attraversandole solo in direzione verticale o orizzontale.
Ogni barca, tranne quelle sulle due rive, ha una probabilità P di rompersi dopo un certo periodo di tempo T.
Le barche che stanno sulla riva non si rompono mai.
Quanto tempo in media occorre affinchè il passaggio tra le due rive non sia più possibile?

Rappresentiamo il ponte come una matrice binaria (MxN) in cui ogni cella rappresenta il vertice di un grafo.
Il valore 1 rappresenta la presenza di un vertice.
Il valore 0 rappresenta l'assenza di un vertice.
Ogni vertice (cella) è collegato con la cella adiacente per riga e con la cella adiacente per colonna se entrambe le celle valgono 1 (distanza di manhattan = 1).

Esempi:

                            B--B--B      
          1 1 1             |  |  |
matrice = 1 1 1     grafo = B--B--B  
          1 1 1             |  |  |
                            B--B--B


                 
                            B     B      
          1 0 1             |     |
matrice = 1 1 1     grafo = B--B--B  
          1 1 1             |  |  |
                            B--B--B
        
                            B     B
          1 0 1             |
matrice = 1 1 0     grafo = B--B
          1 1 1             |  |
                            B--B--B

                            B     B      
          1 0 1                
matrice = 0 0 0     grafo = 
          1 0 1             
                            B     B

Un altro modo di vedere la matrice è quella di considerarla un labirinto.
Il valore 1 è un passaggio, mentre il valore 0 è un muro.
Gli spostamenti possibili sono solo orizzontale e verticale (manhattan).
L'obiettivo è quello di verificare se, data una matrice, sia possibile partire da una qualunque cella della prima riga e raggiungere una qualunque cella dell'ultima riga.
All'inizio la matrice è composta da tutti 1.
Ad ogni secondo i vertici (celle) hanno una probabilità di scomparire pari a P (tranne quelli della prima e ultima riga).
Cioè ad ogni secondo, i valori 1 hanno una probabilità P di scomparire (cioè diventare 0).
Quanto tempo in media occorre affinchè il passaggio tra la prima riga e l'ultima riga della matrice non sia più possibile?

Iniziamo scrivendo la funzione di ricerca di un percorso in una matrice.

Consideriamo un labirinto rappresentato da una matrice binaria MxN in cui 1 è un passaggio e 0 è un muro.
Poichè ci interessa solo sapere se esiste un percorso qualsiasi, senza doverlo costruire né minimizzare, la scelta più semplice e veloce è una DFS (Depth-First Search) iterativa.

Per una matrice binaria:
- 1 = cella attraversabile
- 0 = muro
- si possono usare i 4 movimenti: sopra, sotto, sinistra, destra
- si marca ogni cella visitata una sola volta (sulla matrice 'visited')
- appena si raggiunge B, si restituisce true
- se si esauriscono le celle raggiungibili, 'nil'
'visited' viene marcato quando la cella viene inserita nello stack, non quando viene estratta. Questo evita che la stessa cella venga aggiunta più volte allo stack e rende la DFS più efficiente.
Complessità temporale: O(M*N)
Non esiste, in generale, un algoritmo che garantisca di dover esaminare meno di O(M*N) celle: nel caso peggiore bisogna verificare praticamente tutto il labirinto.

Implementazione
Usiamo una DFS iterativa, usando uno stack(lista) per evitare la ricorsione.
Inoltre usiamo una matrice separata per le celle visitate.

A e B sono celle (riga colonna).

(define (path? M A B)
  (letn ( (rows (length M))
          (cols (length (M 0)))
          (visited (array-list (array rows cols '(nil))))
          (stack (list A))
          ; Direzioni: sopra, sotto, sinistra, destra
          (dirs '((-1 0) (1 0) (0 -1) (0 1)))
          (found nil) )
    ; A e B devono essere celle attraversabili
    (if (or (= (M (A 0) (A 1)) 0)
            (= (M (B 0) (B 1)) 0))
        nil
        ; DFS
        (begin
          ; Marca A come visitata
          (setf (visited (A 0) (A 1)) true)
          ; Inserisce A nello stack
          (while (and stack (not found))
            ; Estrae una cella dallo stack
            (let ((p (pop stack)))
              ; Se abbiamo raggiunto B, il percorso esiste
              (if (= p B)
                  (setq found true)
                  ; Esamina le quattro celle adiacenti
                  (dolist (d dirs)
                    (letn (
                      (r (+ (p 0) (d 0)))
                      (c (+ (p 1) (d 1)))
                      )
                      ; Verifica che la cella sia valida,
                      ; sia un passaggio e non sia già stata visitata
                      (if (and (>= r 0) (< r rows) (>= c 0) (< c cols)
                               (= (M r c) 1)
                               (not (visited r c)))
                          (begin
                            ; Marca la cella prima di inserirla
                            ; nello stack per evitare duplicati
                            (setf (visited r c) true)
                            (push (list r c) stack))))))))
          found))))

(setq M '(
  (1 0 1 1 1)
  (1 1 1 0 1)
  (0 1 0 0 1)
  (1 1 1 1 1)
))

(path? M '(0 0) '(3 4))
;-> true

(path? M '(0 0) '(2 2))
;-> nil

Adesso scriviamo la funzione che simula il processo.

(define (simula M P show)
  (letn ( (rows (length M))
          (cols (length (M 0)))
          (secondi 0)
          (pass true) )
    ;(while (and (ref 1 (M 0)) (ref 1 (M -1)))
    (while pass
      (++ secondi)
      ; applica la probabilità di scomparire a tutte le celle della matrice
      ; tranne alla prima e all'ultima riga (che rappresentano le due rive)
      (for (r 1 (- rows 2))
        (for (c 0 (- cols 1))
          (if (and (= (M r c) 1) (< (random) P)) (setf (M r c) 0))))
      (when show (map println M) (read-line))
      ; calcola se esiste un passaggio valido tra la prima e l'ultima riga
      (setq pass nil)
      (for (c1 0 (- cols 1) 1 pass)
        (for (c2 0 (- cols 1) 1 pass)
          (if (path? M (list 0 c1) (list (- rows 1) c2))
              (setq pass true))))
    )
    secondi))

Proviamo:

(seed (time-of-day) true)

(setq M '((1 1 1 1) (1 1 1 1) (1 1 1 1) (1 1 1 1)))
(simula M 1 true)
;-> (1 1 1 1)
;-> (0 0 0 0)
;-> (0 0 0 0)
;-> (1 1 1 1)
;-> 1
(simula M 0.2)
;-> 6
(simula M 0.1 true)
;-> (1 1 1 1)
;-> (1 1 1 1)
;-> (1 1 1 1)
;-> (1 1 1 1)
;-> 
;-> (1 1 1 1)
;-> (1 1 1 1)
;-> (1 1 0 1)
;-> (1 1 1 1)
;-> ...
;-> (1 1 1 1)
;-> (0 0 0 0)
;-> (1 0 0 1)
;-> (1 1 1 1)
;-> 17

(setq N (array 4 4 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
(simula N 1)
;-> 1
(simula N 0.2)
;-> 4
(simula N 0.0001)
;-> 5338


-------------------------------
Pandigitali raddoppiando numeri
-------------------------------

Dato un numero intero positivo maggiore di 0, eseguire le seguenti operazioni:
1) Impostare un contatore a 0
2) se il numero contiene tutte e dieci le cifre (0..9) almeno una volta,
   allora restituirre il valore del contatore e terminare
   altrimenti, raddoppiare il numero, aumentare di 1 il contatore e ripetere l'operazione 2).

Il contatore conta il numero di raddoppi necessari per fare in modo che il numero contenga tutte e dieci le cifre (0..9) almeno una volta.

; Verifica se un numero intero contiene almeno una volta tutte le cifre (0..9)
(define (pandi? num)
  (= (slice (sort (unique (explode (string num)))) 0 10)
     '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))

; Trova un numero pandigitale raddoppiando sempre un dato numero intero
(define (pandix num)
  (let (conta 0)
    (setq num (bigint num))
    (until (pandi? num)
      (setq num (* 2L num))
      (++ conta))
    conta))

Proviamo:

(pandix 1234567890)
;-> 0 ; non è necessario nessun raddoppio
(pandix 617283945)
;-> 1
(pandix 2L)
;-> 67
(pandix 66833L)
;-> 44
(pandix 100L)
;-> 51
(pandix 42L)
;-> 55

(apply max (map pandix (sequence 1 1e3)))
;-> 68

(time (println (apply max (map pandix (sequence 1 1e4)))))
;-> 78
;-> 1955.297
(find 78 (map pandix (sequence 1 1e4)))
;-> 1470
(pandix 1471)
;-> 78

(time (println (apply max (map pandix (sequence 1 1e5)))))
;-> 78
;-> 18599.608

Proviamo con una altro algoritmo che verifica se un numero intero contiene almeno una volta tutte le dieci cifre decimali 0..9.

Algoritmo
Usiamo un intero di 10 bit come "maschera".
Ogni bit della maschera corrisponde a una cifra:
  bit 0 -> cifra 0
  bit 1 -> cifra 1
  bit 2 -> cifra 2
  ...
  bit 9 -> cifra 9
Inizialmente tutti i bit sono a 0:
  0000000000
Quando incontriamo una cifra, impostiamo a 1 il bit
corrispondente.
Per esempio, per la cifra 3:
  1 << 3
produce:
  0000001000
La maschera viene aggiornata con OR bit-a-bit:
  mask = mask | (1 << digit)
Se una cifra compare più volte, il bit corrispondente
è già a 1 e quindi non cambia nulla.
Quando tutte le dieci cifre sono state incontrate,
tutti i 10 bit sono a 1:
  1111111111
Il valore decimale di questa maschera è:
  2^10 - 1 = 1023
Quindi è sufficiente verificare:
  (= mask 1023)
La scansione viene interrotta appena la maschera raggiunge
1023, senza dover esaminare le eventuali cifre rimanenti.

(define (pandi? num)
  (let ((s (string num))
        (mask 0)
        (digit 0)
        (stop nil))
    ; Scorre tutte le cifre del numero.
    ; Un bigint termina con "L", quindi escludiamo l'ultimo carattere.
    (for (i 0 (- (length s) 2) 1 stop)
      ; Converte il carattere ASCII della cifra nel suo valore numerico.
      (setq digit (int (s i) 0 10))
      ; Costruisce una maschera con un solo bit a 1.
      ; Per esempio, se digit = 3:
      ;   (<< 1 3) -> 0000001000
      ; Il bit corrispondente alla cifra viene quindi
      ; impostato nella maschera generale tramite OR.
      (setq mask (| mask (<< 1 digit)))
      ; Se tutti i dieci bit sono a 1, tutte le cifre
      ; 0..9 sono gia' state incontrate.
      (if (= mask 1023)
          (setq stop true)))
    ; true se e solo se tutte le dieci cifre sono presenti.
    stop))

Proviamo:

(pandix 2L)
;-> 67
(pandix 66833L)
;-> 44
(pandix 100L)
;-> 51
(pandix 42L)
;-> 55

(apply max (map pandix (sequence 1 1e3)))
;-> 68

(time (println (apply max (map pandix (sequence 1 1e4)))))
;-> 78
;-> 1797.112
(time (println (apply max (map pandix (sequence 1 1e5)))))
;-> 78
;-> 17126.488


--------------------------------------
Attraversamento efficiente di stringhe
--------------------------------------

newLISP possiede diversi metodi per attraversare le stringhe.
Vediamo quali sono e la loro efficienza.

; Funzione 'for'
(define (test-for str)
  (let (out '())
    (for (i 0 (- (length str) 1))
      (push (str i) out -1))))

; Funzione 'dolist'
(define (test-dolist str)
  (let (out '())
    (dolist (el (explode str))
      (push el out -1))))

; Funzione 'dostring'
(define (test-dostring str)
  (let (out '())
    (dostring (ch str)
      (push (char ch) out -1))))

; Funzione 'dostring' (codici ASCII)
(define (test-dostring-int str)
  (let (out '())
    (dostring (ch str)
      (push ch out -1))))

; Funzione 'unpack'
(define (test-unpack-int str)
  (unpack (dup "c" (length str)) str))

; Funzione 'unpack' (codici ASCII)
(define (test-unpack str)
  (map char (unpack (dup "c" (length str)) str)))

Proviamo:

(setq str "wergeeyrgt348trfehrjf138y438f734hfafseqhjef1238rt12345r74r7efq")

; verifica delle funzioni che producono caratteri
(= (test-for str) (test-dolist str) (test-dostring str) (test-unpack str))
;-> true
; verifica delle funzioni che producono codici ASCII
(= (test-dostring-int str) (test-unpack-int str))
;-> true

Test di velocità

; velocità delle funzioni che producono caratteri
(time (test-for str) 1e5)
;-> 1828.339
(time (test-dolist str) 1e5)
;-> 1937.686
(time (test-dostring str) 1e5)
;-> 1687.664
(time (test-unpack str) 1e5)
;-> 1046.947

; velocità delle funzioni che producono codici ASCII
(time (test-dostring-int str) 1e5)
;-> 265.888
(time (test-unpack-int str) 1e5)
;-> 93.72

Per attraversare una stringa di caratteri per usare i caratteri o i codici ASCII il metodo più veloce è quello di utilizzare la primitiva 'unpack'.


------------------------------------------------------------------
Numero minimo e numero massimo di una lista con elementi qualsiasi
------------------------------------------------------------------

Data una lista di elementi, determinare il numero minimo e il numero massimo.
Gli elementi della lista possono essere:
1) numero intero
2) numero float
3) stringa
4) lista di elementi
L'elemento 4) indica che la lista data può essere annidata.

Esempio:
  lista = (1 "abc" 1.1 (8 4 "xx" 0.45) (("dot" 6)) (5 ("h" ("ops" (6.15)) 2)))
  numero minimo = 0.45
  numero massimo = 8

(define (min-max1 lst)
  (let ((minimo 1e99)
        (massimo 0)
        (lst (flat lst)))
  (dolist (el lst)
    (if (or (integer? el) (float? el))
      (begin
        (if (< el minimo) (setq minimo el))
        (if (> el massimo) (setq massimo el)))))
  (list minimo massimo)))

(define (min-max2 lst)
  (let (lst (filter (fn(x) (or (integer? x) (float? x))) (flat lst)))
    (list (apply min lst) (apply max lst))))

; Genera una lista con elementi casuali (interi, float e stringhe)
; Parametri
;   nums    -> numero di elementi da generare
;   min-val -> valore minimo dell'intervallo (numero)
;   max-val -> valore massimo dell'intervallo (numero)
;   min-len -> lunghezza minima della stringa
;   max-len -> lunghezza massima della stringa
(define (rand-list nums min-val max-val min-len max-len)
  (let ((out '()) (tipi '("i" "f" "s")))
    (for (el 1 nums)
      (setq type (tipi (rand 3)))
      (cond ((= type "i") ; numeri interi
              (push (+ min-val (rand (+ (- max-val min-val) 1))) out -1))
            ((= type "f") ; numeri floating
              (push (add min-val (random 0 (sub max-val min-val))) out -1))
            ((= type "s") ; stringhe
              (setq str "")
              (setq len (+ min-len (rand (+ (- max-len min-len) 1))))
              (for (c 1 len)
                (extend str (char (+ 32 (rand 95)))))
              (push str out -1))))
    out))

Proviamo:

(seed (time-of-day true))

(silent 
(setq L (rand-list 1e4 -1e6 1e6 1 8))
(for (k 2 4) (setq L (explode L k))))

(min-max1 L)
;-> (-999817 999572.7408673361)
(min-max2 L)
;-> (-999817 999572.7408673361)

(time (min-max1 L) 1e3)
;-> 2234.493
(time (min-max2 L) 1e3)
;-> 2968.909


----------------------------------------
Numero + triangolare(x) = triangolare(y)
----------------------------------------

Un numero triangolare è un numero che rappresenta la somma dei primi n numeri naturali (da 1 a n).
Ad esempio, 1 + 2 + 3 + 4 = 10, quindi 10 è un numero triangolare.

Dato un numero intero positivo (0 <= N <= 10000) scrivere una funzione che restituisce il più piccolo numero triangolare che, sommato a N, genera un altro numero triangolare.
Per N = 0 l'output vale 0.
Se N è un numero triangolare, l'output vale 0.

Esempio:
  N = 26
  Aggiungendo 10 (che è triangolare) otteniamo 36, che è un numero triangolare.

Metodo di soluzione
-------------------

L'n-esimo numero triangolare vale:

          n*(n + 1)
  T(n) = -----------
              2

Per verificare se un numero intero positivo N è un numero triangolare, dobbiamo determinare se esiste un numero naturale n per cui risulta:

       n*(n + 1)
  N = -----------
           2

Invertiamo la formula:

       sqrt(8*N + 1) - 1
  n = -------------------
              2

Se n è un valore intero, allora N è il triangolare di n.

; Restituisce l'n-esimo numero triangolare.
; La successione dei numeri triangolari e':
;   T(0) = 0
;   T(1) = 1
;   T(2) = 3
;   T(3) = 6
; Formula:
;              n * (n + 1)
;   T(n) =  ---------------
;                   2
(define (tri n) (/ (* n (+ n 1)) 2))

; Restituisce l'indice n del numero triangolare N.
; Partendo da:
;          n * (n + 1)
;   N =  ---------------
;               2
; si ottiene:
;   n^2 + n - 2*N = 0
; e quindi, usando la soluzione positiva dell'equazione di secondo grado:
;         sqrt(8*N + 1) - 1
;   n = ---------------------
;                 2
; Se N e' triangolare, questo valore e' intero.
(define (inv-tri N) (div (sub (sqrt (add (mul 8 N) 1)) 1) 2))


; Verifica se num e' un numero intero.
; Converte il numero nella sua rappresentazione intera in base 10:
; (int num 0 10)
; Se il valore convertito coincide con num, num e' intero.
(define (intero? num) (= (int num 0 10) num))

; Cerca il piu' piccolo numero triangolare T(k) tale che:
;                 N + T(k)
; sia a sua volta un numero triangolare.
; L'algoritmo procede in ordine crescente di k:
;   T(0), T(1), T(2), T(3), ...
; quindi il primo valore trovato e' necessariamente il piu'
; piccolo numero triangolare che soddisfa la condizione.
; Ad ogni iterazione:
;   1. si aggiorna T(k)
;   2. si verifica se N + T(k) e' triangolare
;   3. se non lo e', si incrementa k
; L'aggiornamento:
;   (++ tri-k k)
; sfrutta direttamente la relazione:
;   T(k) = T(k-1) + k
; Infatti, partendo da T(0) = 0:
;   prima iterazione:  T(0) = 0 + 0 = 0
;   seconda:           T(1) = 0 + 1 = 1
;   terza:             T(2) = 1 + 2 = 3
;   quarta:            T(3) = 3 + 3 = 6
;   ...
; In questo modo non e' necessario ricalcolare ogni volta
; T(k) mediante moltiplicazione e divisione.
; Inoltre il test viene scritto come:
;   (if condizione
;       (setq found true)
;       (++ k))
; evitando un (begin ...) nel ramo else.
(define (make-tri num)
  (let ((found nil)
        (k 0)
        (tri-k 0))
    (until found
      ; Aggiorna T(k) usando:
      ;   T(k) = T(k-1) + k
      ; Al primo giro k vale 0, quindi tri-k rimane 0.
      (++ tri-k k)
      ; Verifica se: num + T(k) e' un numero triangolare.
      ; inv-tri restituisce l'indice triangolare corrispondente.
      ; Se tale indice e' intero, il numero e' triangolare.
      (if (intero? (inv-tri (+ num tri-k)))
          ; Trovato il primo risultato.
          (setq found true)
          ; Altrimenti passa al triangolare successivo.
          (++ k)))
    ; Restituisce: (T(k) (num + T(k)))
    ; cioe' il piu' piccolo triangolare da aggiungere a num
    ; e il triangolare risultante.
    (list tri-k (+ num tri-k))))

Proviamo:

(make-tri 26)
;-> (10 36)
(make-tri 0)
;-> (0 0)
(make-tri 4)
;-> (6 10)
(make-tri 10)
;-> (0 10)
(make-tri 10000)
;-> 153 (10153)

(time (map make-tri (sequence 0 1e4)))
;-> 1875.058


------------------
I lupi e la pecora
------------------

Un dato numero di lupi affamati si trovano in una foresta.
All'improvviso compare una pecora.
I lupi sanno però che se un lupo mangia una pecora, poi si addormenta e viene sicuramente mangiato da un altro lupo (se esiste).
Anche un lupo che mangia un altro lupo si addormenta e viene a sua volta mangiato (se esiste un altro lupo).
Lo scopo principale di ogni lupo è sopravvivere, quindi ognuno desidera mangiare la pecora, ma lo farà soltanto se avrà la certezza di non finire mangiato a sua volta.
La pecora non può essere divisa tra i lupi, cioè può essere mangiata da un solo lupo.
Che fine fa la pecora?

Scriviamo una funzione che prende come parametro il numero di lupi N e restituisce 'true' se la pecora vive, altrimenti restituisce 'nil'.

(define (sheep N) (even? N))

Come funziona?
--------------
Per risolvere logicamente il problema partiamo con un solo lupo (N=1) e poi aumentiamo di 1.

Un lupo:
Il lupo mangia la pecora e non ci sono conseguenze per lui.
La pecora muore.

Due lupi:
Nessuno dei due lupi mangerà la pecora, poiché se uno di loro lo facesse, si addormenterebbe e verrebbe poi mangiato dall'altro lupo.
La pecora vive.

Tre lupi:
Il più veloce dei lupi mangerà la pecora, perché sa che quando si addormenterà, nessuno degli altri due lupi oserà mangiarlo perchè verrebbe poi mangiato dall'ultimo lupo.
La pecora muore.

Quattro lupi:
Se uno dei quattro lupi mangia la pecora, allora si addormenta e la situazione diventa equivalente a quella con tre lupi, solo che al posto della pecora c'è il lupo addormentato.
Dato che nello scenario con tre lupi e una pecora la pecora muore, nessuno dei nostri quattro lupi mangerà la pecora.
La pecora vive.

A questo punto abbiamo capito lo schema:
il fatto che un lupo mangi o meno la pecora dipende dallo scenario in cui è presente un lupo in meno.
Se nello scenario con un lupo in meno la pecora sopravvive, allora il lupo mangerà la pecora
Se invece nello scenario con un lupo in meno la pecora non sopravvive, il lupo non la mangerà.
In altre parole, la sorte della pecora (viva o morta) si inverte ogni volta che si aumenta di uno il numero dei lupi.
Se il numero di lupi è pari la pecora sopravvive, mentre se è dispari la pecora muore.

(map sheep (sequence 1 10))
;-> (nil true nil true nil true nil true nil true)


------------------------
Tre scatole e una chiave
------------------------

Siamo prigionieri all'interno di una stanza con la porta chiusa a chiave.
Davanti a noi abbiamo tre scatole di colori diversi con le seguenti scritte:

  +--------------------+    +--------------------+    +--------------------+
  |        NERA        |    |       BIANCA       |    |       ROSSA        |
  +--------------------+    +--------------------+    +--------------------+
  | La chiave è in     |    | La chiave non è    |    | La chiave non è    |
  | in questa scatola  |    | in questa scatola  |    | nella scatola NERA |
  +--------------------+    +--------------------+    +--------------------+
            1                         2                         3

La chiave per aprire la porta della prigione si trova in una delle tre scatole.
Ci viene data la possibilità di aprire una sola scatola e ci viene detto che una sola delle frasi è vera.
Quale scatola contiene la chiave?

Le tre frasi formano tre proposizioni:

1) nera = true
2) bianca = nil
3) nera = nil

Che possiamo rappresentare come:

(setq frase1 '("black" true))
(setq frase2 '("white" nil))
(setq frase3 '("black" nil))

Adeso scriviamo una funzione che verifica la veridicità delle frasi considerando i tre casi possibili:

(define (check-frasi key)
  (let ((out1 '()) (out2 '()) (out3 '()))
    (if (= key (frase1 0))
      (setq out1 (frase1 1))
      (setq out1 (not (frase1 1))))
    (if (= key (frase2 0))
      (setq out2 (frase2 1))
      (setq out2 (not (frase2 1))))
    (if (= key (frase3 0))
      (setq out3 (frase3 1))
      (setq out3 (not (frase3 1))))
    (list out1 out2 out3)))

Caso 1: chiave nella scatola NERA

(setq key "black")
(check-frasi key)
;-> (true true nil)

In questo caso abbiamo due frasi valide, quindi la chiave non si trova nella scatola NERA.

Caso 2: chiave nella scatola BIANCA

(setq key "white")
(check-frasi key)
;-> (nil nil true)

Questo è il caso corretto (due frasi false e una frase vera).
La frase 1 è falsa e dice che la scatola si trova nella scatola NERA.
La frase 2 è falsa e dice che la scatola non si trova nella scatola BIANCA.
La frase 3 è vera e dice che la scatola non si trova nella scatola NERA.
Questo risultato comporta che la chiave si trova nella scatola BIANCA.

Caso 3: chiave nella scatola ROSSA

(setq key "red")
(check-frasi key)
;-> (nil true true)
In questo caso abbiamo due frasi valide, quindi la chiave non si trova nella scatola ROSSA.

In un'altra stanza si trova un prigioniero con le seguenti scatole:

  +----------------------+    +--------------------+    +--------------------+
  |         NERA         |    |       BIANCA       |    |       ROSSA        |
  +----------------------+    +--------------------+    +--------------------+
  | La chiave non è      |    | La chiave non è    |    | La chiave è in     |
  | nella scatola BIANCA |    | in questa scatola  |    | questa scatola     |
  +----------------------+    +--------------------+    +--------------------+
            1                         2                         3
Il prigioniero può aprire una sola scatola e gli viene detto che almeno una frase è vera e almeno una frase è falsa.

Proposizioni:

1) white = nil
2) white = nil
3) red = true

Rappresentazione:

(setq frase1 '("white" nil))
(setq frase2 '("white" nil))
(setq frase3 '("red" true))

Caso 1: chiave nella scatola NERA

(setq key "black")
(check-frasi key)
;-> (true true nil)

Caso 3: chiave nella scatola BIANCA

(setq key "white")
(check-frasi key)
;-> (nil nil nil)

Caso 3: chiave nella scatola ROSSA

(setq key "red")
(check-frasi key)
;-> (true true true)

Il caso 1 è l'unico caso in cui almeno una frase è vera e almeno una frase è falsa.
Questo vuol dire che la chiave si trova nella scatola NERA.


----
DWIM
----

Medley Interlisp Project
https://interlisp.org/

DWIM (Do What I Mean) was an early, highly advanced error-correction and user-intent prediction system.
It was built by Warren Teitelman in the late 1960s (~1966).
DWIM was the crowning jewel of Interlisp (developed at BBN and Xerox PARC).
If you mistyped a function name, forgot a parenthesis, or transposed arguments, DWIM would look at the environment, guess what you actually meant, correct the code on the fly, and keep running.
It attempt to anticipate what users intend to do, correcting trivial errors automatically rather than blindly executing users' explicit but potentially incorrect input.

Larry Masinter (Medley Interlisp Project):
"DWIM was the ChatGPT of the day.
A lot of the problems with this and other analyses are that:
- DWIM isn't a single feature. It's part of a suite of tools that work together.
- DWIM (usually) only gets into the picture if otherwise it would signal an error."


----------------------------
Quadrati in lattice di punti
----------------------------

Abbiamo una griglia di punti (lattice) di dimensioni MxN.
Quanti e quali quadrati possiamo disegnare sulla griglia?
I lati dei quadrati possono sovrapporsi ai lati di altri quadrati.

Esempio:
lattice (3x4) =

    0   1   2   3
  0 *   *   *   *

  1 *   *   *   *

  2 *   *   *   *

q1 = (0 0) (0 1) (1 1) (1 0)
q2 = (0 0) (0 2) (2 2) (2 0)
q3 = (1 0) (0 2) (2 2) (2 0)
q4 = ...

Abbiamo due tipi di quadrati
1) quadrati con lati paralleli alla griglia (es. q1 e q2);
2) quadrati ruotati, purché tutti i vertici siano punti del lattice (es. q3).

Contiamo i quadrati di tipo 1 (quelli allineati alla griglia).
Nell'esempio sopra abbiamo 6 quadrati di lato 1 e 2 quadrati di lato 2.
Quindi per un lattice (3x4) -> 8 quadrati.

La formula generale per il numero di quadrati con lati paralleli alla griglia su un lattice di MxN punti, vale:

  Q1(M,N) = Sum[k=1,min(M,N)-1](M-k)*(N-k)
  dove k è la lunghezza del lato.

Q1(3,4) = (3-1)(4-1) + (3-2)(4-2) = 2*3 + 1*2 = 8

Contiamo i quadrati di tipo 2 (quelli ruotati).
Nell'esempio sopra abbiamo 2 quadrati ruotati di lato sqrt(2).
q9  = (0 1) (1 0) (2 1) (1 2)
q10 = (0 2) (1 1) (2 2) (1 3)

Quindi il lattice 3x4 contiene:
  6 quadrati di lato 1
  2 quadrati di lato 2
  2 quadrati ruotati
  --------------------
  10 quadrati totali

La formula generale per il numero di quadrati con lati paralleli alla griglia su un lattice di MxN punti, vale:

  Q2(M,N) = Sum[k=2,min(M,N)-1](k-1)*(M-k)*(N-k)
  dove k è la dimensione del bounding box del quadrato.

In questo caso k vale:
  k = a + b
dove (a,b) è il vettore di un lato del quadrato.
Per avere un quadrato ruotato, sia a che b devono essere diversi da zero:
  a >= 1, b >= 1
Quindi il valore minimo possibile di k vale: k = a + b = 1 + 1 = 2.
Il motivo del fattore (k-1) è dato dal fatto che per un dato k, esistono k-1 orientamenti distinti di quadrati non allineati alla griglia.
In generale:
  2 <= k <= min(M,N) - 1
e per ogni k ci sono k-1 coppie positive:
  (1,k-1),(2,k-2),...,(k-1,1)
Da qui nasce il termine:
  (k-1)(M-k)(N-k)
nella formula dei quadrati ruotati.

La formula generale per il numero di TUTTI i quadrati su un lattice di MxN punti, vale:

  Q(M,N) = Q1(M,N) + Q2(M,N) = Sum[k=1,min(M,N)-1]k*(M-k)*(N-k)

(define (count-squares M N)
  (let (somma 0)
  (for (k 1 (- (min M N) 1))
    (++ somma (* k (- M k) (- N k))))))

(count-squares 3 4)
;-> 10

Adesso scriviamo la funzione 'list-squares' che genera tutti i quadrati (ogni qudrato è una lista con 4 vertici).
Per generare tutti i quadrati basta considerare un vettore intero (a,b) come lato e il vettore perpendicolare (-b,a).
Possiamo costruire la soluzione direttamente usando il parametro k e i diversi vettori (a,b) con:
  a + b = k
Per ogni k ci sono k orientamenti: uno allineato (a=k, b=0) e k-1 ruotati.
Il numero di quadrati generati per ogni k è direttamente:
  k*(M-k)*(N-k)
per cui la funzione genera esattamente il numero calcolato dalla funzione 'count-squares'.

; Restituisce tutti i quadrati di un lattice MxN (senza duplicati)
(define (list-squares M N)
  (if (or (= M 1) (= N 1))
      '()
  ;else
      (let (out '())
        ; k e' la dimensione del bounding box del quadrato.
        ; Per ogni k consideriamo tutti i vettori (a,b) tali che a+b=k.
        (for (k 1 (- (min M N) 1))
          ; a=k, b=0: quadrati con lati paralleli alla griglia.
          (let (b 0)
            ; Ogni posizione possibile del bounding box genera un quadrato.
            (for (y 0 (- M k 1))
              (for (x 0 (- N k 1))
                (push (list
                        (list x y)
                        (list (+ x k) y)
                        (list (+ x k) (+ y k))
                        (list x (+ y k)))
                      out -1))))
          (if (> k 1) ; k deve valere almeno 2
              ; a=1..k-1: quadrati ruotati.
              (for (a 1 (- k 1))
                ; b completa la relazione a+b=k.
                (let (b (- k a))
                  ; Il quadrato ha bounding box k x k.
                  (for (y 0 (- M k 1))
                    (for (x 0 (- N k 1))
                      ; I quattro vertici sono ottenuti usando
                      ; i vettori (a,b) e (-b,a), che sono perpendicolari.
                      (push (list
                              (list (+ x b) y)
                              (list (+ x k) (+ y b))
                              (list (+ x a) (+ y k))
                              (list x (+ y a)))
                            out -1)))))))
        out)))

Proviamo:

(list-squares 3 4)
;-> (((0 0) (1 0) (1 1) (0 1)) ((1 0) (2 0) (2 1) (1 1))
;->  ((2 0) (3 0) (3 1) (2 1)) ((0 1) (1 1) (1 2) (0 2))
;->  ((1 1) (2 1) (2 2) (1 2)) ((2 1) (3 1) (3 2) (2 2))
;->  ((0 0) (1 0) (1 1) (0 1)) ((1 0) (2 0) (2 1) (1 1))
;->  ((2 0) (3 0) (3 1) (2 1)) ((0 1) (1 1) (1 2) (0 2))
;->  ((1 1) (2 1) (2 2) (1 2)) ((2 1) (3 1) (3 2) (2 2))
;->  ((1 0) (1 1) (0 1) (0 0)) ((2 0) (2 1) (1 1) (1 0))
;->  ((3 0) (3 1) (2 1) (2 0)) ((1 1) (1 2) (0 2) (0 1))
;->  ((2 1) (2 2) (1 2) (1 1)) ((3 1) (3 2) (2 2) (2 1))
;->  ((0 0) (2 0) (2 2) (0 2)) ((1 0) (3 0) (3 2) (1 2))
;->  ((1 0) (2 1) (1 2) (0 1)) ((2 0) (3 1) (2 2) (1 1)))

(length (list-squares 3 4))
;-> 10

(length (list-squares 6 5))
(count-squares 6 5)

Verifichiamo che il risultato sia corretto, cioè che ogni quadrupla di punti sia un quadrato.

(define (dist2d-2 p q)
"Calculates the square of 2D Cartesian distance of two points p = (x1 y1) and q = (x2 y2)"
  (let ((x1 (first p)) (y1 (last p))
        (x2 (first q)) (y2 (last q)))
    (add (mul (sub x1 x2) (sub x1 x2))
        (mul (sub y1 y2) (sub y1 y2)))))

(define (square? p1 p2 p3 p4)
  (local (d2 d3 d4)
    (setq d2 (dist2d-2 p1 p2))
    (setq d3 (dist2d-2 p1 p3))
    (setq d4 (dist2d-2 p1 p4))
    (cond ((or (zero? d2) (zero? d3) (zero? d4))
            nil)
     ; Se le lunghezze se (p1, p2) e (p1, p3) sono uguali, allora devono
     ;  essere soddisfatte le seguenti condizioni per formare un quadrato:
     ; 1) Il quadrato di lunghezza di (p1, p4) è uguale a due volte
     ;    il quadrato di (p1, p2)
     ; 2) Il quadrato di lunghezza di (p2, p3) è uguale a due volte
     ;    il quadrato di (p2, p4)
          ((and (= d2 d3) (= (mul 2 d2) d4)
                (= (mul 2 (dist2d-2 p2 p4)) (dist2d-2 p2 p3)))
          true)
     ; condizione analoga
          ((and (= d3 d4) (= (mul 2 d3) d2)
                (= (mul 2 (dist2d-2 p3 p2)) (dist2d-2 p3 p4)))
          true)
     ; condizione analoga
          ((and (= d2 d4) (= (mul 2 d2) d3)
                (= (mul 2 (dist2d-2 p2 p3)) (dist2d-2 p2 p4)))
          true)
     ; altrimenti restituisce nil
          (true nil))))

Proviamo:

(setq all (list-squares 3 4))
(map (fn(x) (apply square? x)) all)
;-> (true true true true true true true true true true)
(clean (fn(x) (apply square? x)) all)
;-> ()

Quindi le quadruple di punti generate da 'list-squares' sono tutti quadrati.


-----------------------------------------------
Simulazione del problema di Giuseppe (Josephus)
-----------------------------------------------

Abbiamo N oggetti disposti in cerchio in modo equidistante.
Dato un numero k > 0 effettuare le seguenti operazioni:
1) partire dall'inizio e muoversi contare in senso orario di k oggetti
2) eliminare l'oggetto raggiunto
3) contare in senso orario di k oggetti
4) ripetere 2) e 3) fino a che non rimane un solo oggetto.

Quando un oggetto viene eliminato, non viene incluso in alcun conteggio successivo.

Esempio:
  Oggetti = (1 2 3 4) (lista circolare)
  k = 2
  Parto dall'inizio e conto di 2 -> 2 (eliminato)
  (1 3 4)
  Parto da 3 e conto 2 -> 4 (eliminato)
  (1 3)
  parto da 3 e conto 2 -> 3 (eliminato)
  (1)

Scriviamo una funzione in newlisp che prende una lista di elementi e un valore k e stampa l'indice e il valore dell'elemento che deve essere eliminato ad ogni conteggio k e la lista corrente.

Per il 'Josephus classico', la posizione da eliminare è (k - 1) rispetto alla posizione di partenza.
Se k è il numero di persone che si contano, la posizione da eliminare a ogni iterazione vale:

  p(0) = k mod N
  p(i) = (p(i-1) + k - 1) mod (N - i)

Possiamo modificare direttamente la lista, eliminando ogni volta l'elemento trovato.
Gli indici stampati sono quelli della lista nel momento dell'eliminazione, quindi dopo ogni 'pop' gli indici degli elementi successivi vengono ricompattati.

(define (giuseppe-elimina lst k)
  (let ((pos 0)
        (n (length lst)))
    ; Continua finché rimane un solo elemento.
    (while (> n 1)
      ; Calcola la posizione dell'elemento da eliminare.
      (setq pos (% (+ pos k -1) n))
      ; Stampa indice e valore dell'elemento.
      (println pos " -> " (lst pos))
      ; Elimina l'elemento dalla lista.
      (pop lst pos)
      ; Stampa la lista corrente
      (println lst)
      ; Aggiorna il numero di elementi.
      (-- n)
      ; Dopo l'eliminazione il conteggio riparte dall'elemento
      ; che occupava la posizione successiva.
      (if (= pos n) (setq pos 0))
      )
    ; Stampa anche l'ultimo elemento rimasto.
    (println pos " -> " (lst pos))))

Proviamo:

(giuseppe-elimina '(1 2 3 4) 2)
;-> 1 -> 2
;-> (1 3 4)
;-> 2 -> 4
;-> (1 3)
;-> 1 -> 3
;-> (1)
;-> 0 -> 1
;-> 1

(giuseppe-elimina '(a b c d e) 3)
;-> 2 -> c
;-> (a b d e)
;-> 0 -> a
;-> (b d e)
;-> 2 -> e
;-> (b d)
;-> 0 -> b
;-> (d)
;-> 0 -> d
;-> d

Vedi anche "Il problema di Giuseppe (Josephus problem)" su "Rosetta Code".


------------------------------------------------------
Espansioni di rapporti tra interi in frazioni unitarie
------------------------------------------------------

Qualsiasi rapporto tra interi m/n può essere espresso come somma finita di frazioni unitarie, ovvero frazioni con numeratore unitario.
Un metodo per scomporre una data frazione in una somma di frazioni unitarie è l'algoritmo 'greedy' (o algoritmo ingordo), secondo il quale, a ogni passaggio, si sceglie la frazione unitaria più grande possibile (cioè con il denominatore minore) che sia minore o uguale al resto corrente.

Sia data una frazione positiva m/n, 0 < m < n e supponiamo che m e n siano coprimi (frazione ai minimi termini).
L'algoritmo greedy di 'Fibonacci-Sylvester' costruisce una rappresentazione

   m     1       1            1
  --- = ---- +  ---- + ... + ----
   n     d1      d2           dk

con denominatori distinti e strettamente crescenti.

1) Scelta della prima frazione unitaria
---------------------------------------
Cerchiamo la frazione unitaria più grande che non superi (m/n).
Una frazione 1/d soddisfa 1/d <= m/n se e solo se n <= m*d e quindi d >= m/n.
Il più piccolo intero possibile vale:

  d1 = ceil(n/m)

Pertanto scegliamo:

  1/d1 = 1/ceil(n/m)

Il nuovo resto vale:

  r1= m/n - 1/d1

Se d1=n/m, la frazione originale era già unitaria e l'algoritmo termina.

2) Si ripete sul resto
----------------------
Scriviamo il resto nella forma ridotta

  r1 = m1/n1

A questo punto scegliamo nuovamente la più grande frazione unitaria non superiore al resto:

  d2 = ceil(n1/m1)

Quindi

r2 = m1/n1 - 1/d2

In generale, se al passo i abbiamo

  r(i) = m(i)/n(i) > 0,

scegliamo

  d(i) = ceil(n(i)/m(i))

e poniamo

  r(i+1) = m(i)/n(i) - 1/d(i)

Il processo termina quando r(i+1) = 0.

3) Formula del nuovo resto
--------------------------
Possiamo esplicitare il resto:

                                 m(i)*d(i) - n(i)
  r(i+1) = m(i)/n(i) - 1/d(i) = ------------------
                                    n(i)*d(i)

Quindi:

  m(i+1) = m(i)*d(i) - n(i)
  n(i+1) = n(i)*d(i)

prima dell'eventuale riduzione ai minimi termini.

La scelta di d(i) garantisce:

  1/d(1) <= m(i)/n(i) < 1/(d(i) - 1)

La seconda disuguaglianza caratterizza la scelta greedy: nessuna frazione unitaria con denominatore più piccolo può essere utilizzata, perché sarebbe troppo grande.

Esempio:
  frazione = 5/7
  m = 5, n = 7
  Il denominatore della prima frazione vale: d1 = ceil(7/5) = 2
  Quindi: 5/7 = 1/2 + (5/7 - 1/2) = 1/2 + 3/14
  Ora lavoriamo sul resto: d2 = ceil(14/3) = 5
  Otteniamo: 3/14 = 1/5 + 1/70 
  Infatti: 3/14 - 1/5 = 1/70
  Quindi 5/7 = 1/2 + 1/5 + 1/70

Perché il metodo funziona
-------------------------
Il punto fondamentale è che, ad ogni passo, 0 <= r(i+1) < r(i).
Inoltre, quando r(i) = m(i)/n(i)) è positiva e propria,

  d(i) = ceil(n(i)/m(i))

fa sì che il nuovo numeratore sia m(i)*d(i) - n(i).

Poiché d(i) - 1 < n(i)/m(i) <= d(i)
abbiamo m(i)*(d(i)-1) < n(i) <= m(i)*d(i),
da cui 0 <= m(i)*d(i)- n(i) < m(i).
Pertanto: 0 <= m(i+1) < m(i).
Il numeratore positivo diminuisce strettamente ad ogni passo.
Essendo un intero, non può diminuire indefinitamente: prima o poi diventa zero.
Questo dimostra la 'terminazione dell'algoritmo' e quindi che ogni frazione razionale positiva propria può essere rappresentata come somma finita di frazioni unitarie.

Scriviamo una funzione che dato m e n (con m < n) restituisce i denominatori della somma finita.
(m e n potrebbero anche non essere coprimi).

(define (egyptian m n)
  ; Riduce la frazione iniziale ai minimi termini.
  (let (g (gcd m n)
        out '())
    (setq m (/ m g))
    (setq n (/ n g))
    ; Continua finche' il resto non e' zero.
    (while (> m 0)
      ; Il piu' piccolo denominatore d per cui 1/d <= m/n
      ; e' ceil(n/m), calcolato con aritmetica intera.
      (let (d (/ (+ n m -1) m))
        ; Aggiunge il denominatore alla lista dei risultati.
        (push d out -1)
        ; Calcola il nuovo resto:
        ; m/n - 1/d = (m*d-n)/(n*d).
        (setq m (- (* m d) n))
        (setq n (* n d))))
    out))

(egyptian 5 7)
(egyptian 4 13)
(egyptian 6 14)

(egyptian 12 5)
(egyptian 2 5)

La funzione restituisce risultati corretti anche quando m > n:
(egyptian 12 5)
;-> (1 1 3 15)

Questa è una conseguenza naturale dell'algoritmo: non è necessario imporre (m < n).
Per una frazione impropria, il greedy produce semplicemente una o più frazioni 1/1 all'inizio, finché il resto diventa una frazione propria.

Le frazioni con numeratore uguale a 1 sono chiamate frazioni egiziane.
Il nome deriva dal fatto che gli antichi Egizi rappresentavano le frazioni principalmente attraverso frazioni unitarie.
Per esempio, una quantità come 5/7 veniva rappresentata come somma di termini del tipo 1/n, anziché usando direttamente una frazione con numeratore maggiore di 1.
Il famoso Papiro di Rhind contiene numerose decomposizioni di questo tipo.
Una caratteristica importante è che il greedy produce, per una frazione propria, denominatori strettamente crescenti:

  d(1) < d(2) < ... < d(k)

Quindi la funzione 'egyptian' calcola la rappresentazione egiziana greedy della frazione data.

Vedi anche "Frazioni egizie" su "Note libere 6".


-----------------------------------
Auto-divisione massima di un numero
-----------------------------------

https://codegolf.stackexchange.com/questions/151849/how-small-can-it-get

Partendo da un intero positivo N, trova il più piccolo intero M ottenibile dividendo ripetutamente N per una delle sue cifre (in base 10). Ogni cifra selezionata deve essere un divisore di N maggiore di 1.

Esempio 1
Il risultato atteso per N = 230 è M = 23:
230/2=115, 115/5=23

Esempio 2
Il risultato atteso per N = 129528 è M = 257:
129528/8=16191, 16191/9=1799, 1799/7=257

Attenzione ai percorsi non ottimali!
Potremmo iniziare con 129528 / 9 = 14392, ma ciò non porterebbe al risultato più piccolo possibile.
Il risultato migliore ottenibile dividendo inizialmente per 9 è:
129528/9=14392, 14392/2=7196, 7196/7=1028, 1028/2=514 --> errato!

Un approccio greedy non è sufficiente.
La scelta della cifra che divide N e produce il quoziente più piccolo nell'immediato può portare a un risultato finale peggiore.

Il problema si può vedere come un 'grafo aciclico di stati':
- ogni intero raggiungibile è uno stato;
- da n partono archi verso n/d, dove d è una cifra di n, d > 1 e n % d = 0;
- ogni divisione riduce strettamente il numero, quindi non ci sono cicli;
- per ogni stato dobbiamo trovare il minimo risultato terminale raggiungibile.

Per esempio, da 129528:
129528
   |
   +-- /8 --> 16191 --> /9 --> 1799 --> /7 --> 257
   |
   +-- /9 --> 14392
               |
               +-- /2 --> 7196 --> /7 --> 1028 --> /2 --> 514

Il secondo ramo si ferma a 514, ma il primo arriva a 257, quindi bisogna esplorare entrambi.
È sufficiente una ricerca ricorsiva in profondità (DFS con memoizzazione) che, per ogni N, prova tutte le divisioni valide e restituisce il minimo tra i risultati ottenuti.

Dato che ogni divisione deve essere esatta, ogni percorso corrisponde a una fattorizzazione di N in cui ciascun fattore utilizzato è una cifra che compare nel numero corrente.
È questa condizione che rende il problema non greedy e richiede l'esplorazione dei diversi percorsi.

La parte importante è che non scegliamo la cifra migliore localmente.
Per ogni cifra valida calcoliamo il miglior risultato dell'intero sottoproblema e poi prendiamo il minimo.
In altre parole, vale la ricorrenza:
  F(n) = min(F(n/d))
per tutte le cifre d di n che soddisfano (d > 1) e (n mod d) = 0.
Se non esiste nessuna cifra valida: F(n) = n

(define (solve n)
  ; Se il risultato per questo numero è già stato calcolato,
  ; lo restituiamo immediatamente senza ripetere la ricerca.
  (if (assoc n memo)
      (lookup n memo)
      ; Inizialmente il miglior risultato possibile è il numero stesso.
      ; Se non esiste alcuna divisione valida, questo sarà infatti
      ; il risultato terminale.
      (let ((best n)
            (digits (map int (explode (string n))))
            d q r)
        ; Esamina tutte le cifre del numero corrente.
        (dolist (d digits)
          ; Sono utilizzabili soltanto cifre maggiori di 1
          ; che dividono esattamente il numero corrente.
          (if (and (> d 1) (= (% n d) 0))
              (begin
                ; Effettua la divisione e cerca ricorsivamente
                ; il minimo risultato ottenibile dal quoziente.
                (setq q (/ n d))
                (setq r (solve q))
                ; Mantiene il più piccolo risultato trovato
                ; tra tutti i possibili percorsi.
                (if (< r best)
                    (setq best r)))))
        ; Memorizza il risultato associandolo allo stato n.
        (push (list n best) memo)
        best)))

(define (min-div n)
  ; La tabella memo contiene il miglior risultato già trovato
  ; per ogni numero raggiunto durante la ricerca.
  (setq memo '())
  (solve n))

Proviamo:

(min-div 230)
;-> 23

(min-div 129528)
;-> 257


-------------------
La funzione "round"
-------------------

*******************
>>> funzione ROUND
*******************
sintassi: (round number [int-digits])
Arrotonda il valore specificato in 'number' al numero di cifre indicato in 'int-digits'.
Se si arrotondano le cifre decimali, 'int-digits' è negativo, mentre è positivo se si arrotonda la parte intera del numero.

Se 'int-digits' viene omesso, la funzione arrotonda a 0 cifre decimali.

(round 123.49 2)    -> 100
(round 123.49 1)    -> 120
(round 123.49 0)    -> 123
(round 123.49)      -> 123
(round 123.49 -1)   -> 123.5
(round 123.49 -2)   -> 123.49

Si noti che, per scopi di visualizzazione, è preferibile utilizzare 'format' per l'arrotondamento.
-------------------

Altri linguaggi usano un metodo diverso per arrotondare un numero.
Per esempio, python 3 usa il metodo 'round half to even' ('banker's rounding') che oggi è considerato il metodo di arrotondamento standard, sebbene alcune implementazioni di linguaggi non l'abbiano ancora adottata.
La semplice tecnica che prevede di "arrotondare sempre lo 0.5 per eccesso" comporta una leggera distorsione verso il valore più alto.
In presenza di un numero elevato di calcoli, tale scostamento può diventare significativo.
Il metodo 'round half to even' elimina questo problema.
Infatti i casi esattamente a metà strada vengono ora arrotondati al numero pari più vicino, anziché allontanandosi dallo zero (ad esempio, round(2.5) restituisce 2 invece di 3).
Per i tipi integrati di python 3 che supportano 'round', i valori vengono arrotondati al multiplo più vicino di 10 elevato alla meno n. Se due multipli sono equidistanti, l'arrotondamento avviene verso il numero pari.

Esempi:

(round-even 1.2))       --> 1
(round-even 2.2))       --> 2
(round-even 1.5))       --> 2
(round-even 2.5))       --> 2
(round-even 3.5))       --> 4
(round-even 2.25 1)     --> 2.2
(round-even 2.35 1)     --> 2.4
(round-even 2.125 2)    --> 2.12
(round-even 2.135 2)    --> 2.13
(round-even 2.225 2)    --> 2.23
(round-even 2.235 2)    --> 2.23
(round-even 2.675 2)    --> 2.67
(round-even 2.685 2)    --> 2.69

Scriviamo una funzione che usa il metodo 'round half to even' ('banker's rounding') per arrotondare i numeri float.
Con questo metodo i valori vengono arrotondati al multiplo più vicino di 10 elevato alla meno n.
Se due multipli sono equidistanti, l'arrotondamento avviene verso il numero pari.

Possiamo sfruttare 'round' per ottenere prima il multiplo più vicino e poi gestire esplicitamente il caso di equidistanza.

(define (round-even x (n 0))
  ; Arrotonda x al multiplo di 10^(-n) più vicino.
  ; In caso di equidistanza sceglie il multiplo pari.
  ; Il round di newLISP viene usato per determinare il multiplo
  ; più vicino, preservando anche gli effetti della rappresentazione
  ; binaria dei numeri floating-point.
  (letn ((r (round x (- n)))
         (p (pow 10 (- n)))
         (d (sub x r))
         (other (if (> d 0)
                    (add r p)
                    (sub r p))))
    ; Se le due distanze sono identiche, siamo esattamente a metà.
    ; In questo caso scegliamo il multiplo pari.
    (if (= (abs d) (abs (sub x other)))
        (if (even? (int (div r p)))
            r
            other)
        r)))

Proviamo:

(round-even 1.2)
;-> 1 (ok)
(round-even 2.2)
;-> 2 (ok)
(round-even 1.5)
;-> 2 (ok)
(round-even 2.5)
;-> 2 (ok)
(round-even 3.5)
;-> 4 (ok)
(round-even 2.25 1)
;-> 2.3 (error)
(round-even 2.35 1)
;-> 2.4 (ok)
(round-even 2.125 2)
;-> 2.13  (error)
(round-even 2.135 2)
;-> 2.13 (ok)
(round-even 2.225 2)
;-> 2.23 (ok)
(round-even 2.235 2)
;-> 2.23 (ok)
(round-even 2.675 2)
;-> 2.67 (ok)
(round-even 2.685 2)
;-> 2.69 (ok)

Non funziona sempre perchè il problema è nel confronto delle due distanze:
con i float l'uguaglianza (= (abs d) (abs (sub x other))) non è affidabile.
In particolare 2.125 è rappresentabile esattamente in binario, ma le operazioni intermedie introducono una piccola differenza.
Quando newLISP usa il valore binario reale del float, il comportamento dipendente dall'approssimazione IEEE.

Un altro metodo è quello di rappresentare e trattare il float come una stringa.
In questo modo il 'banker's rounding' diventa esattamente quello atteso guardando il numero decimale.
Comunque diventa complicato gestire i calcoli con i riporti (es. 1.99995 --> 2.0).

Un altro modo è quello di usare 'pack' e 'unpack' per gestire il float in formato IEEE-754
L'idea è estrarre i 64 bit con pack/unpack "lf"/"Lu", separare segno, esponente e mantissa, e poi eseguire il confronto con 0.5 usando solo aritmetica intera bigint.
In questo modo non facciamo nessun confronto fra float durante la decisione di arrotondamento.
La soluzione completa deve distinguere i tipi zero, subnormal, normal, infinito e NaN, e soprattutto non deve usare (pow 10 n) per costruire la scala, perché pow introduce nuovamente un'approssimazione floating-point.

(define (ieee x)
  (letn ((bites (unpack "Lu" (pack "lf" x)))
         (sign 0)
         (expo 0)
         (frac 0L))
    ; Mostra il valore restituito direttamente da unpack.
    (println "bites = " bites)
    ; Estrae il primo elemento della lista restituita da unpack.
    (setq bites (bites 0))
    (println "bites[0] = " bites)
    ; Estrae il bit di segno.
    (setq sign (& (>> bites 63) 1))
    (println "sign = " sign)
    ; Estrae gli 11 bit dell'esponente.
    (setq expo (& (>> bites 52) 2047))
    (println "expo = " expo)
    ; Estrae i 52 bit della frazione.
    (setq frac (& bites 4503599627370495L))
    (println "frac = " frac)
    ; Restituisce temporaneamente i tre componenti.
    (list sign expo frac)))

(ieee 1.2)
;-> bites = (4608083138725491507)
;-> bites[0] = 4608083138725491507
;-> sign = 0
;-> expo = 1023
;-> frac = 900719925474099
;-> (0 1023 900719925474099)

dove i tipi hanno i seguenti valori:
  1) zero         -> expo=0, frac=0
  2) subnormal    -> expo=0, frac<>0
  3) normal       -> 1 <= expo <= 2046
  4) infinito/NaN -> expo=2047

Anche questa soluzione mi sembra abbastanza complicata.


------------------
Numeri digit-small
------------------

https://codegolf.stackexchange.com/questions/237308/digit-small-numbers

Un numero "digit-small" è un intero positivo N tale che, per qualsiasi coppia di numeri (a, b) il cui prodotto è N, il numero totale delle loro cifre è superiore al numero di cifre di N.
In altre parole: non esistono due interi positivi a e b tali che:
1) a x b = N
2) floor(log10(a)) + floor(log10(b)) < floor(log10(n))

Ad esempio, il numero 363 è "digit-small".
Può essere espresso come prodotto di due numeri in tre modi:
  1 x 363 = 363 (sempre valido per qualunque N)
  3 x 121 = 363 (valido)
  11 x 33 = 363 (valido)
In ogni caso, si hanno 4 cifre a sinistra dell'uguale e 3 cifre a destra.

Un altro esempio: il numero 48 non è "digit-small" perché può essere scritto come:
  1 x 48 = 48  (sempre valido per qualunque N)
  2 x 24 = 48  (valido)
  3 x 16 = 48  (valido)
  4 x 12 = 48  (valido)
  6 x 8  = 48  (non valido, perchè il prodotto 6 x 8 ha solo 2 cifre).

Sequenza OEIS A122427:
Numbers m such that in decimal representation m equals the lexicographically greatest divisor of m.
  1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 13, 17, 19, 22, 23, 26, 29, 31, 33, 34,
  37, 38, 39, 41, 43, 44, 46, 47, 50, 51, 52, 53, 55, 57, 58, 59, 60,
  61, 62, 65, 66, 67, 68, 69, 70, 71, 73, 74, 75, 76, 77, 78, 79, 80,
  82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, ...

(define (factorizations num)
"Calculate all the factorizations of an integer number"
  (let (afc '())
    (factorizations-aux num '() num)))
; funzione ausiliaria
(define (factorizations-aux num parfac parval)
  (let ((newval parval) (i (- num 1)))
    (while (>= i 2)
      (cond ((zero? (% num i))
              (if (> newval 1) (setq newval i))
              (if (and (<= (/ num i) parval) (<= i parval) (>= (/ num i) i))
                  (begin
                    (push (append parfac (list i (/ num i))) afc -1)
                    (setq newval (/ num i))))
              (if (<= i parval)
                  (factorizations-aux (/ num i) (append parfac (list i)) newval))))
      (-- i))
    (sort (unique (map sort afc)))))

(factorizations 363)
;-> ((3 11 11) (3 121) (11 33))

(define (small? num)
  (let ( (len (length num))
         (prod2 (filter (fn(x) (= (length x) 2)) (factorizations num))) )
  (for-all (fn(x) (> (+ (length (x 0)) (length (x 1))) len)) prod2)))

Proviamo:

(small? 363)
;-> true
(small? 48)
;-> nil

(filter small? (sequence 1 100))
;-> (1 2 3 4 5 6 7 8 9 11 13 17 19 22 23 26 29 31 33 34
;->  37 38 39 41 43 44 46 47 50 51 52 53 55 57 58 59 60
;->  61 62 65 66 67 68 69 70 71 73 74 75 76 77 78 79 80
;->  82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99)

Sequenza OEIS A122426:
Numbers m such that in decimal representation the lexicographically greatest divisor of m is smaller than m.
  10, 12, 14, 15, 16, 18, 20, 21, 24, 25, 27, 28, 30, 32, 35, 36, 40, 42,
  45, 48, 49, 54, 56, 63, 64, 72, 81, 100, 102, 104, 105, 106, 108, 110,
  111, 112, 114, 115, 116, 117, 118, 119, 120, 122, 123, 124, 125, 126,
  128, 129, 130, 132, 133, 134, 135, 136, 138, 140, ...

(clean small? (sequence 1 140))
;-> (10 12 14 15 16 18 20 21 24 25 27 28 30 32 35 36 40 42
;->  45 48 49 54 56 63 64 72 81 100 102 104 105 106 108 110
;->  111 112 114 115 116 117 118 119 120 122 123 124 125 126
;->  128 129 130 132 133 134 135 136 138 140)

"the decimal representation m equals the lexicographically greatest divisor of m":
significa che si considera la rappresentazione decimale dei divisori di m e li si confronta 'lessicograficamente come stringhe', cioè nello stesso modo in cui si confrontano le parole in un dizionario.
Tra tutti i divisori di m, m deve essere quello 'lessicograficamente più grande'.

Per esempio:
  m = 11: divisori 1, 11.
  Confronto: "11" > "1", quindi 11 appartiene alla sequenza.

  m = 13: divisori 1, 13
  "13" > "1", 13 appartiene.

  m = 12: divisori 1, 2, 3, 4, 6, 12.
  Lessicograficamente il maggiore è "6", perché "6" > "12", 12 non appartiene.

  m = 22: divisori 1, 2, 11, 22.
  "22" è maggiore di "2" e "11", 22 appartiene.

m = 34: divisori 1, 2, 17, 34
  "1" < "17" < "2" < "34", "34" appartiene

Non si tratta quindi del 'massimo divisore numerico': quello sarebbe sempre m, rendendo la definizione banale.
Si tratta del massimo secondo l'ordine delle 'stringhe decimali'.
Per esempio:
"9" > "34" in senso lessicografico, perché si confronta dal primo carattere: "9" > "3".
Mentre numericamente risulta: 9 < 34

(define (seq num)
  (= (string num) ((sort (map string (filter (fn(x) (zero? (% num x))) (sequence 1 num)))) -1)))

(filter seq (sequence 1 30))
;-> (1 2 3 4 5 6 7 8 9 11 13 17 19 22 23 26 29)

Versione code-golf (91 caratteri):
(define(f n)(=(string n)((sort(map string(filter(fn(x)(zero?(% n x)))(sequence 1 n))))-1)))

(filter f (sequence 1 30))
;-> (1 2 3 4 5 6 7 8 9 11 13 17 19 22 23 26 29)
(clean f (sequence 1 30))
;-> (10 12 14 15 16 18 20 21 24 25 27 28 30)

Test di velocità:

(time (filter small? (sequence 1 100)) 100)
;-> 718.579
(time (filter seq (sequence 1 100)) 100)
;-> 109.376

(time (filter small? (sequence 1 1000)))
;-> 11563.373
(time (filter seq (sequence 1 1000)))
;-> 93.718
(time (filter seq (sequence 1 10000)))
;-> 7797.436

(= (filter small? (sequence 1 1000))
   (filter seq (sequence 1 1000)))
;-> true


--------------------------------------------------------
Cambiare lo stato di alcuni bit per ottenere un quadrato
--------------------------------------------------------

https://codegolf.stackexchange.com/questions/170281/toggle-some-bits-and-get-a-square

Dato un intero N > 3, trovare il numero minimo di bit da invertire in N per trasformarlo in un quadrato perfetto.
È consentito invertire solo i bit meno significativi rispetto a quello più significativo.

Esempi
N=4, è già un quadrato perfetto (2^2), quindi l'output atteso è 0.

N=24, può essere trasformato in un quadrato perfetto invertendo 1 bit: 11000→11001 (25 = 5^2), quindi l'output atteso è 1.

N=22, non può essere trasformato in un quadrato perfetto invertendo un singolo bit (i risultati possibili sono 23, 20, 18 e 30), ma è possibile farlo invertendo 2 bit: 10110 -> 10000 (16 = 4^2), quindi l'output atteso è 2.

Il programma dovrebbe calcolare tutti i valori per 3 < N < 10000 in meno di un minuto.

Esempi:

      Input | Output
  ----------+--------
          4 | 0
         22 | 2
         24 | 1
         30 | 3
         94 | 4
        831 | 5
        832 | 1
       1055 | 4
       6495 | 6
       9999 | 4
      40063 | 6
     247614 | 7        (smallest N for which the answer is 7)
    1049310 | 7        (clear them all!)
    7361278 | 8        (smallest N for which the answer is 8)
  100048606 | 8        (a bigger "8")

1) Problema
Dato un intero positivo N, possiamo cambiare arbitrariamente alcuni dei suoi bit, tranne il bit più significativo, e dobbiamo ottenere un quadrato perfetto.
Per esempio, se: N = 1100101011111 il primo 1 a sinistra non può essere modificato.
Possiamo invece modificare liberamente tutti i bit successivi.
Quindi il quadrato Q che cerchiamo deve avere lo stesso numero di bit di N e, soprattutto, lo stesso bit più significativo.

2) Determinare l'intervallo dei quadrati possibili
Supponiamo che il bit più significativo di N sia quello in posizione k.
Allora:
  2^k <= N < 2^(k+1)
Poichè il bit più significativo non può essere modificato, anche Q deve appartenere allo stesso intervallo:
  2^k <= Q < 2^(k+1)
Essendo Q un quadrato:
  Q = r^2
possiamo quindi limitare la ricerca alle radici che soddisfano:
  2^k <= r^2 < 2^(k+1)
ovvero:
  ceil(sqrt(2^k)) <= r <= floor(sqrt(2^(k+1)-1))
Quindi non dobbiamo provare tutti i quadrati fino a N.

3) Uso di XOR
Consideriamo N e un quadrato candidato Q.
Calcoliamo:
  D = N XOR Q
XOR ha questa proprietà:
  0 XOR 0 = 0
  1 XOR 1 = 0
  0 XOR 1 = 1
  1 XOR 0 = 1
Quindi, posizione per posizione:
D[i] = 0
significa che il bit i di N e Q è uguale.
Mentre:
D[i] = 1
significa che il bit i deve essere cambiato.
Pertanto D è direttamente una maschera dei bit da modificare.

4) Esempio
Supponiamo N = 1100101011111 e Q = 1100100010111
Facciamo XOR:
      1100101011111
  XOR 1100100010111
      --------------
      0000001001000
La maschera vale: 0000001001000
Gli 1 indicano i bit differenti.
Quindi dobbiamo modificare esattamente quei due bit.
Il numero di modifiche è semplicemente:
  popcount(D)
Nel nostro caso:
  popcount(0000001001000) = 2

5) Minimizzare popcount
Durante la ricerca basta quindi minimizzare popcount.
Per ogni radice r nell'intervallo valido:
  Q = r * r
poi:
  D = N XOR Q
e infine:
  c = popcount(D)
c è il numero di bit che dobbiamo modificare per trasformare N in Q.
Conserviamo il quadrato per cui c è minimo.
In altre parole, stiamo cercando:
  min popcount(N XOR r^2)
nell'intervallo delle radici ammissibili.
Questa è in realtà una distanza di Hamming: stiamo cercando il quadrato che ha la minima distanza binaria da N.

6) Il bit più significativo non cambia
Abbiamo limitato Q all'intervallo:
  2^k <= Q < 2^(k+1)
e quindi Q ha necessariamente il bit k uguale a 1.
Anche N ha il bit k uguale a 1.
Pertanto N XOR Q ha necessariamente 0 in quella posizione.
Quindi non c'è nessun rischio che l'algoritmo proponga di modificare il bit che il problema vieta di modificare.

7) Calcolo dei i bit da modificare
Una volta trovato il miglior quadrato, conserviamo la sua maschera:
  mask = N XOR Q
Per esempio:
  mask = 40
In binario:
  40 = 101000
I bit a 1 sono alle posizioni:
  5 e 3
contando da destra e partendo da 0.
Quindi possiamo dire:
  bit 5 -> modifica
  bit 3 -> modifica
e tutti gli altri bit rimangono invariati.

8) Ricostruzione del quadrato
La maschera permette anche di ricostruire il quadrato
C'è una proprietà particolarmente elegante:
  Q = N XOR mask
perchè:
  mask = N XOR Q
e XOR applicato due volte annulla l'operazione:
N XOR mask = N XOR (N XOR Q) = Q
Quindi la maschera non è semplicemente un'informazione aggiuntiva: è una descrizione completa delle modifiche necessarie per trasformare N nel quadrato trovato.

Complessità temporale
----------------------
La parte più importante dal punto di vista dell'efficienza è la restrizione dell'intervallo.
Se N ha circa k bit, le radici considerate sono nell'intervallo approssimativo:
  2^(k/2) ... 2^((k+1)/2)
Quindi il numero di candidati è dell'ordine di sqrt(2^k) * (sqrt(2) - 1) e non dell'ordine di N.
Per i limiti del problema questo intervallo è molto piccolo.
Ad esempio, se:
  N < 10000
il caso peggiore riguarda numeri con 14 bit, e le radici possibili sono soltanto poche decine.

Pseudo-algoritmo
----------------
L'intero procedimento puo' essere visto come questa sequenza:

  N
  |
  +-- trova il bit piu' significativo
  |
  +-- determina 2^k
  |
  +-- determina le radici r tali che
  |      2^k <= r^2 < 2^(k+1)
  |
  +-- per ogni r:
  |      |
  |      +-- Q = r^2
  |      |
  |      +-- mask = N XOR Q
  |      |
  |      +-- costo = popcount(mask)
  |      |
  |      +-- conserva il minimo
  |
  +-- dal migliore:
         |
         +-- Q     = quadrato ottenuto
         +-- mask  = bit da modificare
         +-- popcount(mask) = numero di modifiche
         +-- posizioni degli 1 = bit da modificare

Notiamo che non serve simulare le modifiche una per una.
Per ogni quadrato candidato, una singola operazione XOR ci dice contemporaneamente:
a) quali bit sono diversi
b) quali bit devono essere modificati
c) quanti bit devono essere modificati, tramite popcount
d) come ricostruire il quadrato tramite N XOR mask.
Quindi il problema si riduce essenzialmente alla ricerca del quadrato Q che minimizza la distanza di Hamming popcount(N XOR Q).

(define (popcount num)
  (let (counter 0)
    (while (> num 0)
      (setq num (& num (- num 1)))
      (++ counter))
    counter))

(define (toggle-square n)
  ; Trova la più grande potenza di 2 non superiore a n.
  (let ((p 1)
        (lo 0)
        (hi 0)
        (r 0)
        (q 0)
        (mask 0)
        (best nil)
        (bites nil))
    ; p è 2^k, dove k è la posizione del bit più significativo di n.
    (while (<= (* 2 p) n)
      (setq p (* 2 p)))
    ; La radice minima produce il primo quadrato con il bit più significativo corretto.
    (setq lo (int (sqrt p)))
    (if (< (* lo lo) p)
        (++ lo))
    ; La radice massima produce l'ultimo quadrato che non supera 2^(k+1)-1.
    (setq hi (int (sqrt (- (* 2 p) 1))))
    ; Esamina tutti i quadrati possibili.
    (for (r lo hi)
      (setq q (* r r))
      ; XOR: i bit a 1 sono esattamente quelli da modificare.
      (setq mask (^ n q))
      ; Conserva il quadrato che richiede meno modifiche.
      (if (or (nil? best) (< (popcount mask) (best 0)))
          (setq best (list (popcount mask) q mask))))
    ; Estrae dalla maschera le posizioni dei bit da modificare.
    (setq mask (best 2))
    (setq r 0)
    (while (> mask 0)
      (if (& mask 1)
          (push r bites -1))
      (setq mask (>> mask 1))
      (++ r))
    ; Restituisce: numero modifiche, quadrato, maschera, posizioni.
    (list (best 0) (best 1) (best 2) bites)))

Proviamo:

(toggle-square 4)
;-> (0 4 0 nil)
(toggle-square 24)
;-> (1 16 8 (0 1 2 3))
(toggle-square 22)
;-> (2 16 6 (0 1 2))

(setq L '(4 22 24 30 94 831 832 1055 6495 9999 40063
          247614 1049310 7361278 100048606))

(setq sol '(0 2 1 3 4 5 1 4 6 4 6 7 7 8 8))

(map first (map toggle-square L))
;-> (0 2 1 3 4 5 1 4 6 4 6 7 7 8 8)

(time (map toggle-square (sequence 4 1e4)))
;-> 270.976

Sequenza OEIS A358701:
a(n) is the least number > 1 that needs n toggles in the trailing bits of its binary representation to become a square.
  4, 5, 7, 14, 79, 831, 6495, 247614, 7361278, 743300286, 121387475838, ...

(setq oeis '(4 5 7 14 79 831 6495 247614 7361278 743300286 121387475838))
(map first (map toggle-square oeis))
;-> (0 1 2 3 4 5 6 7 8 9 10)

(define (seq limite)
  (let ( (out '()) (current 0) )
    (for (num 1 limite)
      (when (= ((toggle-square num) 0) current)
        (push num out -1)
        (++ current)))
    out))

(time (println (seq 1e4)))
;-> (1 3 7 14 79 831 6495)
;-> 277.202
(time (println (seq 1e5)))
;-> (1 3 7 14 79 831 6495)
;-> 8864.938
(time (println (seq 2e5)))
;-> (1 3 7 14 79 831 6495)
;-> 25651.506


---------------------------------------
Da sequenza binaria a sequenza decimale
---------------------------------------

Abbiamo una sequenza di cifre binarie, per esempio (1 1 0 1 0 0 1).
Queste cifre rappresentano la seguente sequenza di numeri decimali:
  a(1): decimale(1) = 1
  a(2): decimale(1 1) = 3
  a(3): decimale(1 1 0) = 6
  a(4): decimale(1 1 0 1) = 13
  a(5): decimale(1 1 0 1 0) = 26
  a(6): decimale(1 1 0 1 0 0) = 52
  a(7): decimale(1 1 0 1 0 0 1) = 105
Quindi, la sequenza binaria dell'esempio corrisponde alla sequenza di numeri decimali:
(1 3 6 13 26 52 105)

(define (binary-bigint bin)
"Convert a binary string to big integer"
  (let (num 0L)
    ; remove left padded 0
    (while (= (bin 0) "0") (pop bin))
    (if (= bin "") 0L
        ; else, build big integer number
        (dolist (el (explode bin))
          (setq num (+ (* num 2) (int el)))))))

; Converte una lista binaria in una lista di interi
(define (convert binary)
  (let ( (str "") (out '()) )
    (dolist (bit binary)
      (push (binary-bigint (extend str (string bit))) out -1))))

(setq L '(1 1 0 1 0 0 1))
(convert '(1 1 0 1 0 0 1))
;-> (1L 3L 6L 13L 26L 52L 105L)

(convert (rand 2 30))
;-> (1L 2L 4L 8L 16L 32L 65L 131L 263L 527L 1054L 2109L 4218L 8436L 16872L
;->  33745L 67491L 134983L 269967L 539934L 1079869L 2159739L 4319479L
;->  8638959L 17277919L 34555838L 69111676L 138223352L 276446705L 552893410L)

Calcoliamo la frequenza delle cifre dei numeri generati da una sequenza binaria.

(define (int-list num)
"Convert an integer to a list of digits"
  (if (zero? num) '(0)
  (let (out '())
    (while (!= num 0)
      (push (% num 10) out)
      (setq num (/ num 10))) out)))

(int-list 1230L)
;-> (1L 2L 3L 0L)

Versione 1:

(define (digit-freq1 lst)
  (let ((cifre '()) (unici '()))
    (dolist (el lst)
      (extend cifre (int-list el)))
    (setq unici (sort (unique cifre)))
    (map list unici (count unici cifre))))

Versione 2:

(define (digit-freq2 lst)
  (let (freq (array 10 '(0)))
    (dolist (el lst)
      (if (zero? el)
        (++ (freq 0))
        ; else
        (let ((out '()) (num el))
          (while (!= num 0)
            (++ (freq (% num 10)))
            (setq num (/ num 10))))))
    freq))

Versione 3:

(define (digit-freq3 lst)
  (let (freq (array 10 '(0)))
    (dolist (el lst)
      (if (zero? el)
        (++ (freq 0))
        ; else
          (dolist (cifra (chop (explode (string el))))
            (++ (freq (int cifra))))))
    freq))

Proviamo:

(seed (time-of-day) true)

(digit-freq1 (convert L))
;-> ((0L 1) (1L 3) (2L 2) (3L 2) (5L 2) (6L 2))
(digit-freq2 (convert L))
;-> (1 3 2 2 0 2 2 0 0 0)
(digit-freq3 (convert L))
;-> (1 3 2 2 0 2 2 0 0 0)

(setq B (rand 2 1000))
((convert B) -1)
;-> 472871450741411181622016926667273302450719524398034001933942696945938521
;-> 431570884537771077698696714570695735191693860488849771924243712054143590
;-> 137389849992177641373003529884583783772606935438032381095414483753078917
;-> 835848609428967127671823258803680828435697881122930982797315506622430812
;-> 7874998214164L

(digit-freq1 (convert B))
;-> ((0L 14896) (1L 15378) (2L 15263) (3L 14970) (4L 15127) (5L 15145)
;->  (6L 14924) (7L 15057) (8L 15074) (9L 15090))
(digit-freq2 (convert B))
;-> (14896 15378 15263 14970 15127 15145 14924 15057 15074 15090)
(digit-freq3 (convert B))
;-> (14896 15378 15263 14970 15127 15145 14924 15057 15074 15090)

(time (digit-freq1 (convert B)))
;-> 517.374
(time (digit-freq2 (convert B)))
;-> 417.148
(time (digit-freq3 (convert B)))
;-> 304.857


------------------------
Numeri root-factor-prime
------------------------

Calcolare la sequenza dei numeri in cui la somma ripetuta delle cifre della somma dei suoi fattori primi (con molteplicità) è un numero primo.

Esempi:
  N = 28
  fattori = 2 2 7
  somma dei fattori = 2 + 2 + 7 = 11
  somma ripetuta della somma dei fattori = 1 + 1 = 2 --> numero primo

N = 21
  fattori = 3 7
  somma dei fattori = 3 + 7 = 10
  somma ripetuta della somma dei fattori = 1 + 0 = 1 --> numero non primo

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (digit-root num)
"Calculate the repeated sum of the digits of an integer"
    (+ 1 (% (- (abs num) 1) 9)))

(define (pp? num)
  (prime? (digit-root (apply + (factor num)))))

(filter pp? (sequence 1 100))
;-> (2 3 5 6 7 10 11 12 23 28 29 33 35 38 39 40 41 42 43 45 46 47 48 49 50 51
;->  54 55 59 60 61 64 66 68 70 72 74 76 79 81 82 83 84 87 91 93 97 98 100)

Questa sequenza non esiste in OEIS (settembre 2026).


------------------------------------------------------
a(n) = p - n!, where p is the k-th smallest prime > n!
------------------------------------------------------

Sequenza OEIS A033932:
Least positive m such that n! + m is prime.
a(n) = p - n!, where p is the smallest prime > n!.
  1, 1, 1, 1, 5, 7, 7, 11, 23, 17, 11, 1, 29, 67, 19, 43, 23, 31, 37, 89,
  29, 31, 31, 97, 131, 41, 59, 1, 67, 223, 107, 127, 79, 37, 97, 61, 131,
  1, 43, 97, 53, 1, 97, 71, 47, 239, 101, 233, 53, 83, 61, 271, 53, 71,
  223, 71, 149, 107, 283, 293, 271, 769, 131, 271, ...

Sequenza OEIS A275272:
a(n) = p - n!, where p is the second smallest prime > n!.
  2, 3, 5, 7, 11, 13, 19, 31, 23, 19, 17, 43, 73, 41, 149, 41, 53, 61,
  109, 37, 37, 71, 109, 193, 97, 173, 47, 101, 229, 163, 241, 83, 139,
  103, 83, 577, 311, 47, 269, 61, 61, 107, 97, 89, 379, 149, 269, 83,
  137, 167, 281, 89, 79, 443, 229, 157, 179, 563, 389, ...

Sequenza OEIS A275273:
a(n) = p - n!, where p is the third smallest prime > n!.
  4, 5, 7, 13, 17, 19, 37, 37, 31, 41, 19, 59, 109, 71, 179, 73, 59, 73,
  113, 53, 47, 127, 149, 263, 107, 241, 59, 103, 317, 241, 317, 113, 197,
  127, 109, 647, 397, 67, 281, 67, 211, 163, 109, 107, 439, 521, 709, 101,
  383, 337, 397, 223, 337, 601, 281, 311, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (fact-i num)
"Calculate the factorial of an integer number"
  (if (zero? num)
      1
      (let (out 1L)
        (for (x 1L num)
          (setq out (* out x))))))

La funzione 'prime?' non gestisce i big-integer, quindi possiamo arrivare al massimo a 20 elementi di ogni sequenza:
(map fact-i (sequence 1 20))
;-> (1L 2L 6L 24L 120L 720L 5040L 40320L 362880L 3628800L 39916800L 479001600L
;->  6227020800L 87178291200L 1307674368000L 20922789888000L 355687428096000L
;->  6402373705728000L 121645100408832000L 2432902008176640000L)

(define (seq num prime-order)
  (letn ((num-prime 0) (f (fact-i num)) (p f))
    (while (< num-prime prime-order)
      (++ p)
      (if (prime? p) (++ num-prime)))
    (- p f)))

A033932
(time (println (map (fn(x) (seq x 1)) (sequence 0 20))))
;-> (1L 1L 1L 1L 5L 7L 7L 11L 23L 17L 11L 1L 29L 67L 19L 43L 23L 31L 37L 89L
;->  29L)
;-> 12001.011

A275272:
(time (println (map (fn(x) (seq x 2)) (sequence 1 20))))
;-> (2L 3L 5L 7L 11L 13L 19L 31L 23L 19L 17L 43L 73L 41L 149L 41L 53L 61L
;->  109L 37L)
;-> 20127.149

A275273:
(time (println (map (fn(x) (seq x 3)) (sequence 1 20))))
;-> (4L 5L 7L 13L 17L 19L 37L 37L 31L 41L 19L 59L 109L 71L 179L 73L 59L 73L
;->  113L 53L)
;-> 28245.642


----------------------------
Capacità idrica di un numero
----------------------------

Definiamo la "capacità idrica" di un numero intero positivo nel modo seguente: se N ha la scomposizione in fattori primi p1^e1 * p2^e2 * ... * pk^ek, sia c(i) una colonna di altezza p(i)^e(i) e larghezza 1.
Affiancando le colonne c(i) si ottiene un istogramma che, figurativamente, può essere riempito d'acqua dall'alto.
La capacità idrica di un numero è il numero massimo di celle che possono essere riempite d'acqua.

Esempio:
Il numero 48300 ha la scomposizione in fattori primi 2^2 * 3 * 5^2 * 7 * 23.
L'istogramma sottostante deve essere ruotato di 90 gradi in senso antiorario.

  2^2   ****
  3     ***W
  5^2   *************************
  7     *******WWWWWWWWWWWWWWWW
  23    ***********************

Il numero 48300 ha una capacità idrica pari a 17.

Per calcolare l'acqua in una colonna non dobbiamo conoscere tutta la parte interna dell'istogramma.
Basta sapere quale dei due bordi, sinistro o destro, è più basso.

Prendiamo il nostro esempio:
altezza:  4   3   25   7   23
indice:   0   1    2   3    4

1) Due indici
Usiamo due indici:
  L ->                 <- R
  4   3   25   7   23
L parte da sinistra e R da destra.
Manteniamo anche:
  lm = massimo incontrato da sinistra
  rm = massimo incontrato da destra
All'inizio: L = 0, R = 4, lm = 0, rm = 0

2) Cerchiamo il lato più basso
Abbiamo: h[L] = 4, h[R] = 23
Il lato sinistro è più basso.
Questo è fondamentale: la quantità d'acqua sopra la colonna L può essere determinata con certezza dal lato sinistro, perché a destra sappiamo già che esiste un muro alto almeno 23, quindi sicuramente superiore a 4.
In altre parole, per una posizione interna vale:
  acqua = min(max_sinistro, max_destro) - altezza
Se il limite sinistro è quello più basso, il limite destro non può essere il fattore limitante.
Quindi possiamo elaborare L e avanzarlo.

3) Primo passo
Abbiamo:
4   3   25   7   23
^               ^
L               R
Poiché 4 < 23: lm = max(lm, 4) = 4
Sopra la colonna di altezza 4 non c'è acqua: 4 - 4 = 0
Avanziamo L:
    L
    v
4   3   25   7   23
                ^
                R

4) Secondo passo
Ora: h[L] = 3, h[R] = 23
Ancora: 3 < 23
Quindi possiamo determinare definitivamente l'acqua sopra la colonna 3.
Il massimo sinistro è: lm = 4
Pertanto: acqua = 4 - 3 = 1
La situazione è:
  4   3   25   7   23
      W
Abbiamo accumulato water = 1 e incrementiamo L.

5) Terzo passo
Ora:
  4   3   25   7   23
          ^       ^
          L       R
Abbiamo: h[L] = 25, h[R] = 23
Questa volta è il lato destro ad essere più basso: 25 > 23
Quindi lavoriamo su R.
Aggiorniamo: rm = max(rm, 23) = 23
La colonna di altezza 23 non contiene acqua: 23 - 23 = 0 e spostiamo R verso sinistra.

6) Quarto passo
Ora:
4   3   25   7   23
        ^   ^
        L   R
Abbiamo: h[L] = 25, h[R] = 7
Il lato destro è più basso: 7 < 25
Quindi possiamo determinare definitivamente l'acqua sopra 7.
Il massimo destro è rm = 23 perciò acqua = 23 - 7 = 16
Ora abbiamo water = 1 + 16 = 17 e R viene decrementato.
A questo punto L = R e l'algoritmo termina.

Perché funziona?
----------------
La parte più importante è questa regola:
if h[L] < h[R]
    possiamo risolvere L
else
    possiamo risolvere R

A) Caso sinistro
Se h[L] < h[R] allora sappiamo che esiste a destra un muro almeno alto quanto h[R], che è maggiore di h[L].
Quindi, per la posizione L, il limite destro non può essere inferiore a h[L].
Il limite effettivo sarà quindi determinato dal massimo incontrato a sinistra: lm e possiamo calcolare definitivamente: lm - h[L].

B) Caso destro
Simmetricamente, se h[R] < h[L] sappiamo che esiste a sinistra un muro più alto di h[R].
Quindi possiamo risolvere definitivamente R usando: rm - h[R]

Il vantaggio rispetto al metodo che calcola, per ogni colonna, il massimo a sinistra e a destra è che non dobbiamo effettuare due scansioni per ogni posizione.
Il doppio indice percorre l'istogramma una sola volta, quindi, una volta ottenute le altezze dei fattori primi, il calcolo della capacità è O(k), dove k è il numero di fattori primi distinti.

Per stampare l'istogramma usiamo una lista w parallela a h, che memorizza quanta acqua c'è sopra ogni colonna. Memorizziamo anche le etichette dei fattori per la stampa (labels).

(define (acqua num show)
  (let ((f (factor num))
        (h '()) (labels '()) (w '())
        (i 0) (p 0) (e 0) (v 0) (l 0) (r 0) (lm 0) (rm 0) (water 0))
    ; Raggruppa i fattori uguali e calcola p^e.
    ; Contemporaneamente costruisce l'etichetta della colonna.
    (while (< i (length f))
      (setq p (f i))
      (setq e 1)
      (setq v p)
      (++ i)
      (while (and (< i (length f)) (= (f i) p))
        (setq v (* v p))
        (++ e)
        (++ i))
      (push v h -1)
      (push (if (= e 1) (string p) (string p "^" e)) labels -1))
    ; Crea una lista parallela alle altezze per memorizzare
    ; la quantita' di acqua presente sopra ogni colonna.
    (setq w (dup 0 (length h)))
    ; Con meno di tre colonne non si puo' formare una cavita'.
    (if (>= (length h) 3)
        (begin
          ; I due indici partono dalle estremita' dell'istogramma.
          (setq r (- (length h) 1))
          (while (< l r)
            ; Si lavora dal lato con altezza minore.
            (if (< (h l) (h r))
                (begin
                  ; Aggiorna il massimo raggiunto a sinistra.
                  (setq lm (max lm (h l)))
                  ; Calcola e memorizza l'acqua sopra la colonna.
                  (setf (w l) (- lm (h l)))
                  ; Accumula l'eventuale differenza.
                  (setq water (+ water (w l)))
                  (++ l))
                (begin
                  ; Aggiorna il massimo raggiunto a destra.
                  (setq rm (max rm (h r)))
                  ; Calcola e memorizza l'acqua sopra la colonna.
                  (setf (w r) (- rm (h r)))
                  ; Accumula l'eventuale differenza.
                  (setq water (+ water (w r)))
                  (-- r))))))
    ; Stampa l'istogramma in forma compatta.
    ; Gli asterischi "*" rappresentano la colonna e le "W" l'acqua.
    (if show
      (dolist (i (sequence 0 (- (length h) 1)))
        (println (format "%-5s %s%s"
                 (labels i) (dup "*" (h i)) (dup "W" (w i))))))
    ; Capacita' idrica totale.
    water))

Proviamo:

(acqua 48300 true)
;-> 2^2   ****
;-> 3     ***W
;-> 5^2   *************************
;-> 7     *******WWWWWWWWWWWWWWWW
;-> 23    ***********************
;-> 17

La lista w contiene direttamente la distribuzione dell'acqua:
  (0 1 0 16 0)
quindi la funzione calcola la capacità totale e conserva anche dove sono e quante sono le celle d'acqua.

(acqua 243600 true)
;-> 2^4   ****************
;-> 3     ***WWWWWWWWWWWWW
;-> 5^2   *************************
;-> 7     *******WWWWWWWWWWWWWWWWWW
;-> 29    *****************************
;-> 31

(acqua 10 true)
;-> 2     **
;-> 5     *****
;-> 0

Sequenza OEIS A275339:
a(n) is the smallest number which has a water-capacity of n.
  60, 120, 440, 168, 264, 840, 2448, 528, 1904, 624, 1360, 2295, 816, 1632,
  20128, 1824, 48300, 3105, 15392, 2208, 13024, 2400, 10656, 4080, 8288,
  2784, 5920, 2976, 3552, 9120, 243600, 11840, 28560, 7104, 124352, 13120,
  115776, 7872, 107200, 8256, 98624, 15040, 685608, ...

(define (seq limite)
  (let ((out '()) (num 0))
    (for (i 1 limite)
      (setq num 1)
      (until (= (acqua num) i) (++ num))
      (push num out -1))))

(time (println (seq 50)))
;-> (60 120 440 168 264 840 2448 528 1904 624 1360 2295 816 1632
;->  20128 1824 48300 3105 15392 2208 13024 2400 10656 4080 8288
;->  2784 5920 2976 3552 9120 243600 11840 28560 7104 124352 13120
;->  115776 7872 107200 8256 98624 15040 685608 9024 81472 9408 72896
;->  16960 973500 10176)
;-> 10547.751

(acqua 685608 true)
;-> 2^3   ********
;-> 3     ***WWWWW
;-> 7^2   *************************************************
;-> 11    ***********WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
;-> 53    *****************************************************
;-> 43


----------------
Cambio di parità
----------------

Dato un numero intero positivo N calcolare:
a) N - 1 se N è pari
b) N + 1 se N è dispari
Scrivere la funzione più corta possibile che risolve il problema.

1) Metodo lineare (come da definizione)
(define (f1 N) (if (odd? N) (+ N 1) (- N 1)))
(map f1 (sequence 1 10))
;-> (2 1 4 3 6 5 8 7 10 9)

2) Metodo XOR
a) Moltiplicare N per -1
b) Invertire il bit meno significativo del valore (ovvero (XOR (N * -1) 1))
c) Moltiplicare il risultato per -1
(define (f2 N) (* (^ (* -1 N) 1) -1))
(map f2 (sequence 1 10))
;-> (2 1 4 3 6 5 8 7 10 9)

3) Metodo XOR (abbreviato)
a) (- N) -> -N
b) (^ (- N) 1) -> (-N) ^ 1
c) (- ...) -> negazione del risultato.
(define (f3 N) (- (^ (- N) 1)))
(map f3 (sequence 1 10))
;-> (2 1 4 3 6 5 8 7 10 9)

4) Metodo pow
|(pow(-1, N) - N)|
(define (f4 N) (abs (- (pow -1 N) N)))
(map f4 (sequence 1 10))
;-> (2 1 4 3 6 5 8 7 10 9)

La funzione più corta è la 3).

Versione code-golf (25 caratteri):

(define(f N)(-(^(- N)1)))
(map f (sequence 1 10))
;-> (2 1 4 3 6 5 8 7 10 9)


---------
Fast -1^N
---------

Una funzione più veloce di 'pow' per il calcolo di (-1^N).

(define (f1 N) (pow -1 N))
(define (f2 N) (if (even? N) 1 -1))

Test di correttezza
(= (map f1 (sequence 1 101)) (map f2 (sequence 1 101)))
;-> true

Test di velocità
(time (map f1 (sequence 1 1001)) 1e4)
;-> 1139.936
(time (map f2 (sequence 1 1001)) 1e4)
(t2 3)
;-> 749.423
(div 749 1139)

La funzione 'f2' è circa 1.5 volte più veloce di 'f1'.


-------------------------------------------------
Interi positivi dispari e pari scambiati di posto
-------------------------------------------------

Dalla sequenza dei numeri naturali 1,2,3,...,N (con N pari) restituire la sequenza in cui i numeri pari sono scambiati con i numeri dispari.
Lo scambio avviene tra il primo e il secondo numero, poi tra il terzo e il quarto, poi tra il quinto e il sesto, e così via.
Esempio:
  N = 6
  sequenza numeri naturali = 1 2 3 4 5 6
  sequenza scambiata = 2 1 4 3 6 5

Sequenza OEIS A103889:
Odd and even positive integers swapped.
  2, 1, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11, 14, 13, 16, 15, 18, 17, 20, 19,
  22, 21, 24, 23, 26, 25, 28, 27, 30, 29, 32, 31, 34, 33, 36, 35, 38, 37,
  40, 39, 42, 41, 44, 43, 46, 45, 48, 47, 50, 49, 52, 51, 54, 53, 56, 55,
  58, 57, 60, 59, 62, 61, 64, 63, 66, 65, 68, 67, 70, 69, 72, 71,...

Formula 1
---------
  a(2k) = 2k-1 
  a(2k-1) = 2k 

(define (f1 N out)
  (for (num 1 N 2)
    (extend out (list (+ num 1) num))))

(f1 20)

Formula 2
---------
  a(n) = n - 1 + 2*(n mod 2)

(define (f2 N out)
  (for (num 1 N)
    (push (+ (- num 1) (* 2 (% num 2))) out -1))
  out)

(f2 20)

Formula 3
---------
a(n) = n - (-1)^n

(define (f3 N out)
  (for (num 1 N)
    (push (- num (if (even? num) 1 -1)) out -1))
  out)

(f3 20)

Test di correttezza:
(= (f1 100) (f2 100) (f3 100))
;-> true

Test di velocità:
(time (f1 100000) 100)
;-> 504.987
(time (f2 100000) 100)
;-> 1244.339
(time (f3 100000) 100)
;-> 854.815


-----------------------
Matrici a segni alterni
-----------------------

Una matrice a segni alterni (alternating sign matrix) è una matrice NxN costituita dai numeri -1, 0 e 1, tale che:
1) La somma di ciascuna riga e di ciascuna colonna sia pari a 1
2) Gli elementi non nulli (1 e -1) di ciascuna riga e di ciascuna colonna abbiano segno alterno

Scrivere una funzione che verifica se una matrice è a segni alterni.

; Verifica se una ha lista ha 1 e -1 (o -1 e 1) alternati
; Il primo termine non nullo può essere 1 o -1
(define (alterni? lst)
  (let ((prev 0) (error nil))
    (dolist (x lst error)
      ; Gli zeri non partecipano alla sequenza dei segni
      (if (!= x 0)
          ; Il primo valore non nullo non ha un precedente da confrontare
          (if (zero? prev)
              (setq prev x)
              ; Ogni valore successivo deve essere l'opposto del precedente
              (if (= x (- prev))
                  (setq prev x)
                  ;else
                  (setq error true)))))
    (not error)))

(alterni? '(0 1 0 0 -1 1))
;-> true
(alterni? '(0 -1 0 0 1 -1))
;-> true
(alterni? '(0 1 1 0 -1 1))
;-> nil

; Verifica se una ha lista ha 1 e -1 alternati
; Il primo termine non nullo vale sempre 1.
(define (alterni1? lst)
  (let ((prev -1) (error nil))
     ; 'prev' viene posto a -1 perchè il primo valore non nullo che dobbiamo
     ; incontrare nella lista vale 1.
    (dolist (x lst error)
      ; Gli zeri non partecipano alla sequenza dei segni
      (if (!= x 0)
        ; Ogni valore non nullo deve essere l'opposto del precedente
        (if (= x (- prev))
            (setq prev x)
            ;else
            (setq error true))))
    (not error)))

(alterni1? '(0 1 -1 0 0 1 0))
;-> true
(alterni1? '(0 -1 1 0 0 -1 0))
;-> nil
(alterni? '(0 -1 1 0 0 -1 0))
;-> true

; Verifica se una matrice è a segni alterni
(define (check matrix)
  (setq error nil)
  ; check sum of each row == 1
  (dolist (row matrix error)
    (if-not (= (apply + row) 1) (setq error true)))
  ; check sum of each column == 1
  (dolist (row (transpose matrix) error)
    (if-not (= (apply + row) 1) (setq error true)))
  ; check each row for alternate sign
  (dolist (row matrix error)
    (if-not (alterni? row) (setq error true)))
  ; check each column for alternate sign
  (dolist (row (transpose matrix) error)
    (if-not (alterni? row) (setq error true)))
  (not error))

Proviamo:

(setq m1 '((0  1  0  0) (0  0  1  0) (1  0  0  0) (0  0  0  1)))
(check m1)
;-> true

(setq m2 '((1  0  0  0) (0  0  1  0) (0  1 -1  1) (0  0  1  0)))
(check m2)
;-> true

(setq m3 '((0  0  1  0) (0  1 -1  1) (1 -1  1  0) (0  1  0  0)))
(check m3)
;-> true

(setq m4 '((0  0  1  0) (1  0 -1  1) (0  1  0  0) (0  0  1  0)))
(check m4)
;-> true

(setq m5 '((0  1  0  0) (0  0  0  1) (1  0  0  0) (0  0  1 -1)))
(check m5)
;-> nil

(setq m6 '((0  0  0  1) (1  0  0  0) (-1  1  1  0) (1  0  0  0)))
(check m6)
;-> nil

(setq m7 '((0 -1 0 1 1) (1 -1 1 -1 1) (0 1 1 0 -1) (1 1 -1 1 -1) (-1 1 0 0 1)))
(check m7)
;-> nil

(setq m8 '((0 1 0) (1 0 1) (0 1 0)))
(check m8)
;-> nil


-----------------------------------------
Massima potenza di 2 che divide un intero
-----------------------------------------

Dato un intero positivo N, determinare la massima potenza di 2 che divide N.

Sequenza OEIS A006519:
Highest power of 2 dividing n.
  1, 2, 1, 4, 1, 2, 1, 8, 1, 2, 1, 4, 1, 2, 1, 16, 1, 2, 1, 4, 1, 2, 1, 8,
  1, 2, 1, 4, 1, 2, 1, 32, 1, 2, 1, 4, 1, 2, 1, 8, 1, 2, 1, 4, 1, 2, 1, 16,
  1, 2, 1, 4, 1, 2, 1, 8, 1, 2, 1, 4, 1, 2, 1, 64, 1, 2, 1, 4, 1, 2, 1, 8,
  1, 2, 1, 4, 1, 2, 1, 16, 1, 2, 1, 4, 1, 2, 1, 8, 1, 2, 1, 4, 1, 2, 1, 32,
  1, 2, 1, 4, 1, 2, ...

Se N è dispari, il risultato è sempre 1 (2^0).

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

Metodo 1
--------
Calcoliamo la massima potenza di 2 contenuta nella fattorizzazione di N, cioè:

  2^k divide N, 2^(k+1) non divide N

Per fare questo basta moltiplicare tutti i 2 che compaiono nella fattorizzazione di N.

(define (high1a N)
  (if (odd? N) 1L
      (** 2 (first (count '(2) (factor N))))))

(define (high1b N)
  (if (odd? N) 1L
    (let (res 1)
      (dolist (el (factor N) (!= el 2))
          (setq res (* res 2)))
      res)))

(map high1a (sequence 1 20))
;-> (1L 2L 1L 4L 1L 2L 1L 8L 1L 2L 1L 4L 1L 2L 1L 16L 1L 2L 1L 4L)
(map high1b (sequence 1 20))
;-> (1L 2L 1L 4L 1L 2L 1L 8L 1L 2L 1L 4L 1L 2L 1L 16L 1L 2L 1L 4L)

Metodo 2
--------
Dividiamo ripetutamente N per 2 finché diventa dispari.
Il numero di divisioni effettuate è proprio k, e la potenza cercata è 2^k.
Questo metodo non richiede la fattorizzazione completa.

(define (high2 N)
  (if (odd? N) 1L
    (let (k 1)
      (until (odd? (setq N (/ N 2))) (++ k))
    (** 2 k))))

(map high2 (sequence 1 20))
;-> (1L 2L 1L 4L 1L 2L 1L 8L 1L 2L 1L 4L 1L 2L 1L 16L 1L 2L 1L 4L)

Metodo 3
--------
Formula: a(n) = gcd(2^n, n)

Possiamo scrivere:
  gcd(2^N, N) = 2^v2(N)
dove v2(N) è l'esponente della massima potenza di 2 che divide N.
Il motivo è che 2^N contiene almeno N fattori 2, mentre N ne contiene solo v2(N).
Quindi il massimo fattore comune può contenere esattamente quei v2(N) fattori 2:
  gcd(2^N, N) = 2^v2(N)
Per esempio, con N = 48:
  48 = 2^4 * 3
e quindi gcd(2^48, 48) = 2^4 = 16
La formula è semplice, ma dal punto di vista computazionale (** 2 N) può produrre un intero enorme.

(define (high3 N)
  (if (odd? N) 1L
      (gcd (** 2 N) N)))

(map high3 (sequence 1 20))
;-> (1L 2L 1L 4L 1L 2L 1L 8L 1L 2L 1L 4L 1L 2L 1L 16L 1L 2L 1L 4L)

Metodo 4
--------
Formula bitwise: a(n) = n & -n

La funzione (N & -N) usa una proprieta' classica della rappresentazione binaria:
isola il bit meno significativo impostato a 1.
Quel bit corrisponde esattamente alla massima potenza di 2 che divide N.
Esempi:

  N = 12:
  12 = 1100
 -12 = 0100    ; considerando 4 bit
 ----------
       0100
Il risultato è 4, quindi 12 = 4 * 3

  N = 40:
  40 = 101000
 -40 = 011000
 ------------
      001000
Il risultato è 8 che è la massima potenza di 2 che divide 40.

Per un numero dispari, il bit meno significativo è già 1, quindi: N & (-N) = 1

Con questo metodo non servono divisioni, fattorizzazione, gcd o potenze, solo una negazione e un'operazione bitwise.
Questo esprime direttamente, a livello binario, cio' che stiamo cercando.

(define (high4 N) (& N (- N)))

(map high4 (sequence 1 20))
;-> (1 2 1 4 1 2 1 8 1 2 1 4 1 2 1 16 1 2 1 4)

Test di correttezza:

(= (map high1a (sequence 1 1000)) (map high1b (sequence 1 1000))
   (map high2 (sequence 1 1000))  (map high3 (sequence 1 1000))
   (map high4 (sequence 1 1000)))
;-> true

Test di velocità:

(time (map high1a (sequence 1 1000)) 100)
;-> 68.992
(time (map high1b (sequence 1 1000)) 100)
;-> 31.263
(time (map high2 (sequence 1 1000)) 100)
;-> 62.448
(time (map high3 (sequence 1 1000)) 100)
;-> 4203.04
(time (map high4 (sequence 1 1000)) 100)
;-> 15.586

(time (map high1b (sequence 1 1000)) 1000)
;-> 287.492
(time (map high4 (sequence 1 1000)) 1000)
;-> 84.598


------------------
The ruler function
------------------

Dato un intero positivo N, determinare la massima potenza di 2 che divide 2N.

Sequenza OEIS A001511:
The ruler function: exponent of the highest power of 2 dividing 2n.
Equivalently, the 2-adic valuation of 2n.
  1, 2, 1, 3, 1, 2, 1, 4, 1, 2, 1, 3, 1, 2, 1, 5, 1, 2, 1, 3, 1, 2, 1, 4,
  1, 2, 1, 3, 1, 2, 1, 6, 1, 2, 1, 3, 1, 2, 1, 4, 1, 2, 1, 3, 1, 2, 1, 5,
  1, 2, 1, 3, 1, 2, 1, 4, 1, 2, 1, 3, 1, 2, 1, 7, 1, 2, 1, 3, 1, 2, 1, 4,
  1, 2, 1, 3, 1, 2, 1, 5, 1, 2, 1, 3, 1, 2, 1, 4, 1, 2, 1, 3, 1, 2, 1, 6,
  1, 2, 1, 3, 1, 2, 1, 4, 1, ...

Se N è dispari, il risultato è sempre 1 (2^0).

Metodo 1
--------
a(n) è l'esponente della più piccola potenza di 2 che non divide N.

(define (ruler1a N)
  (if (odd? N) 1
      (+ (first (count '(2) (factor N))) 1)))

(map ruler1a (sequence 1 20))
;-> (1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3)

(define (ruler1b N)
  (if (odd? N) 1
    (let (res 1)
      (dolist (el (factor N) (!= el 2))
          (++ res))
      res)))

(map ruler1b (sequence 1 20))
;-> (1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3)

Metodo 2
--------
Dobbiamo calcolare il numero di fattori 2 che dividono 2N.
Dividiamo ripetutamente 2N per 2 finché diventa dispari.
Il numero di divisioni effettuate è proprio il valore cercato.

(define (ruler2 N)
  (if (odd? N) 1
    (let ((N (* 2 N)) (k 1))
      (until (odd? (setq N (/ N 2))) (++ k))
    k)))

(map ruler2 (sequence 1 20))
;-> (1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3)

Metodo 3
--------
a(n) - 1 è il numero di zeri finali nella rappresentazione binaria di n.

Se N è dispari, il bit meno significativo è 1, quindi il risultato è 0.
Altrimenti, possiamo contare quante volte è possibile fare uno shift a destra prima di ottenere un numero dispari.

(define (trailing-zeros N)
  (let (k 0)
    (while (= (& N 1) 0)
      (setq N (/ N 2))
      (++ k))
    k))

(define (ruler3 N)
  (+ (trailing-zeros N) 1))

(map ruler3 (sequence 1 20))
;-> (1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3)

Metodo 4
--------
Contare gli zeri finali nella rappresentazione binaria di N.

(define (ruler4 N)
  (let ((bin (bits N)) (k 1))
    (while (= (pop bin -1) "0") (++ k))
    k))

(map ruler4 (sequence 1 20))
;-> (1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3)

Metodo 5
--------
Se si conta in binario e al bit meno significativo viene assegnato il numero 1, al bit successivo il 2, e così via, qual è il bit che viene incrementato nel passaggio da N-1 a N?
Esempio:
 N - 1 = 12 --> binario = 1100
     N = 13 --> binario = 1101
                             ^
Il bit incrementato passando da N-1 a N è N & -N.
E questo significa anche che il numero di zeri finali di N è l'esponente della potenza di 2 rappresentata da quel bit.
Esempio:
  40 = 101000
            ^
  N & -N = 8 = 2^3
  quindi 40 ha 3 zeri finali.

(define (bit-increment N)
  ; Il bit meno significativo a 1 di N
  ; e' quello che viene acceso passando da N-1 a N.
  (& N (- N)))

In altre parole, il problema degli zeri finali può essere visto come:
"qual è il valore del bit che viene acceso quando si passa da N-1 a N?".

(define (trail-zeros N)
  ; N & (- N) estrae la potenza di 2 corrispondente agli zeri finali.
  (setq N (& N (- N)))
  ; L'esponente della potenza di 2 e' il numero di zeri finali.
  (let (k 0)
    (while (> N 1)
      (setq N (/ N 2))
      (++ k))
    k))

(define (ruler5a N)
  (+ (trail-zeros N) 1))

(map ruler5a (sequence 1 20))
;-> (1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3)

La funzione può essere vista semplicemente come:
  trail-zeros(N) = log2(N & -N)

(define (ruler5b N)
  (+ (log (& N (- N)) 2) 1))

(map ruler5b (sequence 1 20))
;-> (1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3)

Test di correttezza:

(= (map ruler1a (sequence 1 1000)) (map ruler1b (sequence 1 1000))
   (map ruler2 (sequence 1 1000)) (map ruler3 (sequence 1 1000))
   (map ruler4 (sequence 1 1000)) (map ruler5a (sequence 1 1000))
   (map ruler5b (sequence 1 1000)))
;-> true

Test di velocità:

(time (map ruler1a (sequence 1 1000)) 1000)
;-> 390.96
(time (map ruler1b (sequence 1 1000)) 1000)
;-> 234.219
(time (map ruler2 (sequence 1 1000)) 1000)
;-> 249.968
(time (map ruler3 (sequence 1 1000)) 1000)
;-> 344.087
(time (map ruler4 (sequence 1 1000)) 1000)
;-> 625.002
(time (map ruler5a (sequence 1 1000)) 1000)
;-> 343.696
(time (map ruler5b (sequence 1 1000)) 1000)
;-> 171.833


----------------------------------
Conteggio di bit dei numeri binari
----------------------------------

(define (pop-count1 num)
"Calculate the number of 1 in binary value of an integer number"
  (let (counter 0)
    (while (> num 0)
      (setq num (& num (- num 1)))
      (++ counter))
    counter))

(define (pop-count0 num)
"Calculate the number of 0 in binary value of an integer number"
  (- (length (bits num)) (pop-count1 num)))

1) Numeri in base 2 per cui risulta: (numero di bit 1 > numero di bit 0)

Sequenza OEIS A072600:
Numbers which in base 2 have fewer 0's than 1's.
  1, 3, 5, 6, 7, 11, 13, 14, 15, 19, 21, 22, 23, 25, 26, 27, 28, 29, 30,
  31, 39, 43, 45, 46, 47, 51, 53, 54, 55, 57, 58, 59, 60, 61, 62, 63, 71,
  75, 77, 78, 79, 83, 85, 86, 87, 89, 90, 91, 92, 93, 94, 95, 99, 101,
  102, 103, 105, 106, 107, 108, 109, 110, 111, 113, 114, 115, ...

(define (most1? num)
  (let (ones (pop-count1 num))
    (< (- (length (bits num)) ones) ones)))

(filter most1? (sequence 1 115))
;-> (1 3 5 6 7 11 13 14 15 19 21 22 23 25 26 27 28 29 30
;->  31 39 43 45 46 47 51 53 54 55 57 58 59 60 61 62 63 71
;->  75 77 78 79 83 85 86 87 89 90 91 92 93 94 95 99 101
;->  102 103 105 106 107 108 109 110 111 113 114 115)

2) Numeri in base 2 per cui risulta: (numero di bit 1 < numero di bit 0)

Sequenza OEIS A072603:
Numbers which in base 2 have more 0's than 1's.
  4, 8, 16, 17, 18, 20, 24, 32, 33, 34, 36, 40, 48, 64, 65, 66, 67, 68, 69,
  70, 72, 73, 74, 76, 80, 81, 82, 84, 88, 96, 97, 98, 100, 104, 112, 128,
  129, 130, 131, 132, 133, 134, 136, 137, 138, 140, 144, 145, 146, 148,
  152, 160, 161, 162, 164, 168, 176, 192, 193, ...

(define (most0? num)
  (let (ones (pop-count1 num))
    (> (- (length (bits num)) ones) ones)))

(filter most0? (sequence 1 193))
;-> (4 8 16 17 18 20 24 32 33 34 36 40 48 64 65 66 67 68 69
;->  70 72 73 74 76 80 81 82 84 88 96 97 98 100 104 112 128
;->  129 130 131 132 133 134 136 137 138 140 144 145 146 148
;->  152 160 161 162 164 168 176 192 193)

3) Numeri in base 2 per cui risulta: (numero di bit 1 = numero di bit 0)

Sequenza OEIS A031443:
Digitally balanced numbers: positive numbers that in base 2 have the same number of 0's as 1's.
  2, 9, 10, 12, 35, 37, 38, 41, 42, 44, 49, 50, 52, 56, 135, 139, 141, 142,
  147, 149, 150, 153, 154, 156, 163, 165, 166, 169, 170, 172, 177, 178, 180,
  184, 195, 197, 198, 201, 202, 204, 209, 210, 212, 216, 225, 226, 228, 232,
  240, 527, 535, 539, 541, 542, 551, ...

(define (eq01? num)
  (let (ones (pop-count1 num))
    (= (- (length (bits num)) ones) ones)))

(filter eq01? (sequence 1 551))
;-> (2 9 10 12 35 37 38 41 42 44 49 50 52 56 135 139 141 142
;->  147 149 150 153 154 156 163 165 166 169 170 172 177 178 180
;->  184 195 197 198 201 202 204 209 210 212 216 225 226 228 232
;->  240 527 535 539 541 542 551)

Test di correttezza:

(= (sequence 1 1000)
   (sort (append (filter most1? (sequence 1 1000))
                 (filter most0? (sequence 1 1000))
                 (filter eq01? (sequence 1 1000)))))
;-> true

; Funzione unica 
(define (conta-bit num op)
  (let (conta (count '("0" "1") (explode (bits num))))
    (op (conta 0) (conta 1))))

(filter (fn(x) (conta-bit x >)) (sequence 1 50))
;-> (4 8 16 17 18 20 24 32 33 34 36 40 48)

(filter (fn(x) (conta-bit x <)) (sequence 1 50))
;-> (1 3 5 6 7 11 13 14 15 19 21 22 23 25 26 27 28 29 30 31 39 43 45 46 47)

(filter (fn(x) (conta-bit x =)) (sequence 1 50))
;-> (2 9 10 12 35 37 38 41 42 44 49 50)


-----------------------------------
Un problema geometrico (5 quadrati)
-----------------------------------

Nella seguente figura ci sono 5 quadrati.
Conosciamo solo l'area del quadrato più piccolo, che vale 1.
Quanto vale l'altezza h?

      +------------+---------------+
      |            |               |
      |            |               |
      |            |               |
      |            |               |
      +--------+---+               |
      |        | 1 |               |
      |        +---+---------------+
      |        |                   |
      +--------+                   |
      |        |                   |
      |        |                   |
    h |        |                   |
      |        |                   |
      |        |                   |
      +        +-------------------+
  
  
           x+1           x+2
      +------------+---------------+
      |            |               |
  x+1 |        x+1 |               |
      |            |               | x+2
      |    x+1     |               |
      +--------+---+               |
      |        | 1 |     x+2       |
    x |      x +---+---------------+
      |   x    |                   |
      +--------+                   |
               |                   |
               | x+3               | x+3
               |                   |
               |                   |
               |                   |
               +-------------------+
                     x+3

  h = ((x+2) + (x+3)) - ((x+1) + x) =
    = (2x + 5) - (2x + 1) = 
    = 2x + 5 - 2x - 1 = 5 - 1 = 4


-----------------------------
Numeri P-smooth (o P-friable)
-----------------------------

Un numero P-smooth (o P-friable) è un numero intero il cui fattore primo più grande è minore o uguale a P.
In altre parole, un intero è P-smooth se non ha fattori primi maggiori di P.
Dati N e P, dobbiamo scrivere un programma per verificare se N sia P-smooth o meno.

Esempi:
  N = 24
  P = 7
  fattori di 24: 2 2 2 3
  Quindi 24 è 7-smooth perchè (3 < 7).

N = 22
P = 5
fattori di 22: 2 11
Quindi 2 non è 5-smooth perchè 11 > 5.

(define (psmooth? num p)
  (if (= num 1) 1
      ;else
      (<= (last (factor num)) p)))

Proviamo:

(psmooth? 24 7)
;-> true
(psmooth? 22 5)
;-> nil

Sequenza OEIS A000079:
2-smooth numbers: positive numbers whose prime divisors are all <= 2.
  1, 2, 4, 8, 16, 32, 64, 128, 256, 512, ...
(filter (fn(x) (psmooth? x 2)) (sequence 1 60))

Sequenza OEIS A003586:
3-smooth numbers: positive numbers whose prime divisors are all <= 3.
  1, 2, 3, 4, 6, 8, 9, 12, 16, 18, 24, 27, 32, 36, 48, 54, ...
(filter (fn(x) (psmooth? x 3)) (sequence 1 60))

Sequenza OEIS A051037:
5-smooth numbers: positive numbers whose prime divisors are all <= 5.
 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36,
 40, 45, 48, 50, 54, 60, ...
(filter (fn(x) (psmooth? x 5)) (sequence 1 60))

Sequenza OEIS A002473: 
7-smooth numbers: positive numbers whose prime divisors are all <= 7.
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 15, 16, 18, 20, 21, 24, 25, 27,
  28, 30, 32, 35, 36, 40, 42, 45, 48, 49, 50, 54, 56, 60, ...
(filter (fn(x) (psmooth? x 7)) (sequence 1 60))

Sequenza OEIS A051038:
11-smooth numbers: positive numbers whose prime divisors are all <= 11.
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 18, 20, 21, 22, 24,
  25, 27, 28, 30, 32, 33, 35, 36, 40, 42, 44, 45, 48, 49, 50, 54, 55,
  56, 60, ...
(filter (fn(x) (psmooth? x 11)) (sequence 1 60))

Sequenza OEIS A080197:
13-smooth numbers: positive numbers whose prime divisors are all <= 13.
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 20, 21, 22,
  24, 25, 26, 27, 28, 30, 32, 33, 35, 36, 39, 40, 42, 44, 45, 48, 49,
  50, 52, 54, 55, 56, 60, ...
(filter (fn(x) (psmooth? x 13)) (sequence 1 60))

Sequenza OEIS A080681:
17-smooth numbers: positive numbers whose prime divisors are all <= 17.
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 22,
  24, 25, 26, 27, 28, 30, 32, 33, 34, 35, 36, 39, 40, 42, 44, 45, 48, 49,
  50, 51, 52, 54, 55, 56, 60, ...
(filter (fn(x) (psmooth? x 17)) (sequence 1 60))

Sequenza OEIS A080682:
19-smooth numbers: numbers whose prime divisors are all <= 19.
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
  22, 24, 25, 26, 27, 28, 30, 32, 33, 34, 35, 36, 38, 39, 40, 42, 44, 45,
  48, 49, 50, 51, 52, 54, 55, 56, 57, 60, ...
(filter (fn(x) (psmooth? x 19)) (sequence 1 60))

Dato un numero intero positivo casuale <= N, qual è la probabilità che sia P-smooth?
La probabilità che un intero positivo casuale minore o uguale a N sia p-smooth è psi(N,P)/N, dove psi(N,P) è il numero di interi P-smooth minori o uguali a N.

(define (prob-smooth num p)
  (if (>= p num) 1
      ;else
      (div (length (filter (fn(x) (psmooth? x p)) (sequence 1 num))) num))

Proviamo:

(prob-smooth 2 2)
;-> 1
(map (fn(x) (prob-smooth 20 x)) '(2 3 5 7 11 13 17 19))
;-> (0.25 0.5 0.7 0.8 0.85 0.9 0.95 1)


--------------------------------------------
Indovinare un numero con cifre tutte diverse
--------------------------------------------

Indovinare un numero con 3 cifre tutte diverse dai seguenti indizi:
(anche gli indizi hanno tutte cifre diverse)

+---+---+---+
| 6 | 8 | 2 |
+---+---+---+
Una cifra è corretta e nella posizione giusta

+---+---+---+
| 6 | 4 | 5 |
+---+---+---+
Una cifra è corretta, ma nella posizione sbagliata

+---+---+---+
| 2 | 0 | 6 |
+---+---+---+
Due cifre sono corrette, ma nelle posizioni sbagliate

+---+---+---+
| 7 | 3 | 8 |
+---+---+---+
Nessuna cifra è corretto

+---+---+---+
| 7 | 8 | 0 |
+---+---+---+
Una cifra è corretta, ma nella posizione sbagliata

Regole codificate:

(define (r1 num)
  (or
    (and (= (num 0) 6) (not (find 8 num)) (not (find 2 num)))
    (and (= (num 1) 8) (not (find 6 num)) (not (find 2 num)))
    (and (= (num 2) 2) (not (find 6 num)) (not (find 8 num)))))

(define (r2 num)
  (or (and (!= (num 0) 6) (find 6 num) (not (find 4 num)) (not (find 5 num)))
      (and (!= (num 1) 4) (find 4 num) (not (find 6 num)) (not (find 5 num)))
      (and (!= (num 2) 5) (find 5 num) (not (find 6 num)) (not (find 4 num)))))

(define (r3 num)
  (or (and (!= (num 0) 2) (find 2 num) (!= (num 1) 0) (find 0 num)
           (not (find 6 num)))
      (and (!= (num 0) 2) (find 2 num) (!= (num 2) 6) (find 6 num)
           (not (find 0 num)))
      (and (!= (num 1) 0) (find 0 num) (!= (num 2) 6) (find 6 num)
           (not (find 2 num)))))

(define (r4 num)
  (and (not (find 7 num)) (not (find 3 num)) (not (find 8 num))))

(define (r5 num)
  (or (and (!= (num 0) 7) (find 7 num) (not (find 8 num)) (not (find 0 num)))
      (and (!= (num 1) 8) (find 8 num) (not (find 7 num)) (not (find 0 num)))
      (and (!= (num 2) 0) (find 0 num) (not (find 7 num)) (not (find 8 num)))))

; Applica una regola ad una lista di numeri rappresentati come lista di cifre
; lista = ((0 3 2) (7 5 4) ...)
(define (applica rule lst)
  (let (out '())
    (dolist (el lst)
      (if (rule el) (push el out -1)))
  out))

; Crea la lista dei numeri: (0 0 1) (0 0 2) ... (9 9 9)
(setq nums  (map (fn(x) (map int x))
                 (map explode
                      (map (fn(x) (format "%03d" x)) (sequence 1 999)))))

Applichiamo le regole:

(setq sol (applica r1 nums))
;-> ((0 0 2) (0 1 2) (0 2 2) (0 3 2) (0 4 2) (0 5 2) (0 7 2)
;->  ...
;->  (9 8 3) (9 8 4) (9 8 5) (9 8 7) (9 8 8) (9 8 9) (9 9 2))

(setq sol (applica r2 sol))
;-> ((0 5 2) (0 8 4) (1 5 2) (1 8 4) (2 5 2) (3 5 2) (3 8 4) (4 0 2)
;->  ...
;->  (5 8 8) (5 8 9) (5 9 2) (7 5 2) (7 8 4) (8 8 4) (9 5 2) (9 8 4))

(setq sol (applica r3 sol))
;-> ((0 5 2))

Le regole r4 e r5 non servono.

(setq sol (applica r4 sol))
;-> ((0 5 2))

(setq sol (applica r5 sol))
;-> ((0 5 2))

Applichiamo r3 alla fine:

(setq sol (applica r1 nums))
;-> ((0 0 2) (0 1 2) (0 2 2) (0 3 2) (0 4 2) (0 5 2) (0 7 2)
;->  ...
;->  (9 8 3) (9 8 4) (9 8 5) (9 8 7) (9 8 8) (9 8 9) (9 9 2))

(setq sol (applica r2 sol))
;-> ((0 5 2) (0 8 4) (1 5 2) (1 8 4) (2 5 2) (3 5 2) (3 8 4) (4 0 2)
;->  ...
;->  (5 8 8) (5 8 9) (5 9 2) (7 5 2) (7 8 4) (8 8 4) (9 5 2) (9 8 4))

(setq sol (applica r4 sol))
;-> ((0 5 2) (1 5 2) (2 5 2) (4 0 2) (4 1 2) (4 2 2) (4 9 2)
;->  (5 0 2) (5 1 2) (5 2 2) (5 5 2) (5 9 2) (9 5 2))

(setq sol (applica r5 sol))
;-> ((0 5 2) (4 0 2) (5 0 2))

(setq sol (applica r3 sol))
;-> ((0 5 2))

Possiamo fare una semplificazione concettuale: ogni indizio può essere espresso contando quante cifre sono presenti e quante sono nella posizione corretta.
In questo modo non servono tutti gli or/and combinatori che abbiamo scritto.

Per esempio, 682 significa semplicemente:
cifre presenti = 1
posizioni corrette = 1

mentre 645 significa:
cifre presenti = 1
posizioni corrette = 0

e 206:
cifre presenti = 2
posizioni corrette = 0

Questo permette di costruire una funzione generica per gli indizi, invece di scrivere una funzione specifica r1, r2, ... r5 per ciascuno.

Possiamo rappresentare ogni indizio come:
  (indizio cifre-corrette posizioni-corrette)
e scrivere una sola funzione generica che gestisce numeri con N cifre (purché 'num' e 'clue' abbiano la stessa lunghezza e ognuno abbia cifre tutte diverse).

(define (indizio num clue presenti corrette)
  ; Conta le cifre dell'indizio presenti nel numero
  ; e quelle che occupano la posizione corretta.
  (let (n-presenti 0 n-corrette 0)
    (for (i 0 (- (length clue) 1))
      ; Controlla se la cifra dell'indizio compare nel numero.
      (if (find (clue i) num)
          (++ n-presenti))
      ; Controlla se la cifra si trova nella posizione corretta.
      (if (= (clue i) (num i))
          (++ n-corrette)))
    ; Verifica le due condizioni richieste dall'indizio.
    (and (= n-presenti presenti)
         (= n-corrette corrette))))

Gli indizi diventano quindi:

  (indizio num '(6 8 2) 1 1)
  (indizio num '(6 4 5) 1 0)
  (indizio num '(2 0 6) 2 0)
  (indizio num '(7 3 8) 0 0)
  (indizio num '(7 8 0) 1 0)

e possiamo applicarli direttamente:

(setq sol (applica (fn (x) (indizio x '(6 8 2) 1 1)) nums))
(setq sol (applica (fn (x) (indizio x '(6 4 5) 1 0)) sol))
(setq sol (applica (fn (x) (indizio x '(2 0 6) 2 0)) sol))
(setq sol (applica (fn (x) (indizio x '(7 3 8) 0 0)) sol))
(setq sol (applica (fn (x) (indizio x '(7 8 0) 1 0)) sol))
;-> ((0 5 2))

Proviamo con un altro numero:

X = 1 0 7

(setq sol (applica (fn (x) (indizio x '(1 2 3) 1 1)) nums))
(setq sol (applica (fn (x) (indizio x '(9 7 8) 1 0)) sol))
(setq sol (applica (fn (x) (indizio x '(2 7 0) 2 0)) sol))
(setq sol (applica (fn (x) (indizio x '(2 3 4) 0 0)) sol))
;-> ((1 0 7))

Proviamo con un numero di 4 cifre:

; Crea la lista dei numeri: (0 0 0 1) (0 0 0 2) ... (9 9 9 9)
(setq nums  (map (fn(x) (map int x))
                 (map explode
                      (map (fn(x) (format "%04d" x)) (sequence 1 9999)))))

X = 1 0 7 4

(setq sol (applica (fn (x) (indizio x '(1 2 3 5) 1 1)) nums))
(setq sol (applica (fn (x) (indizio x '(9 7 8 6) 1 0)) sol))
(setq sol (applica (fn (x) (indizio x '(2 7 0 3) 2 0)) sol))
(setq sol (applica (fn (x) (indizio x '(2 3 4 1) 2 0)) sol))
(setq sol (applica (fn (x) (indizio x '(3 5 1 4) 2 1)) sol))
(setq sol (applica (fn (x) (indizio x '(2 3 5 6) 0 0)) sol))
;-> (1 0 7 4)

Questa versione considera le cifre individualmente e quindi funziona correttamente solo per numeri dove le cifre del numero e degli indizi sono tutte distinte.
Per una funzione veramente generica che gestisca anche numeri con cifre ripetute, bisognerebbe contare le occorrenze, non semplicemente usare 'find'.
                              
; Crea una lista di numeri da 'start' a 'end'
; Ogni numero è una lista di cifre di lunghezza: length(end) (con leading 0)
; Es. da 0 a 99: ((0 1) (0 2) (0 3) ... (9 8) (9 9))
(define (genera-nums start end)
  (letn ((pad (length end))
        (fmt (string "%0" pad "d")))
    (map (fn(x) (map int x))
                (map explode
                (map (fn(x) (format fmt x)) (sequence start end))))))


----------------------
Prigionieri e cappelli
----------------------

Due prigionieri vengono chiamati dal direttore che vuole dare loro una possibilità di uscire dal carcere.
A ciascuno di loro verrà messo in testa un cappello bianco o nero:
il colore di ogni cappello sarà determinato dal lancio di una moneta.
Ogni prigioniero potrà vedere il cappello dell'altro, ma non il proprio.
Una volta visti i cappelli altrui, dovranno indovinare il colore del proprio cappello.
Se almeno uno dei due risponderà correttamente, allora saranno liberati entrambi.
I prigionieri possono concordare una strategia prima dell'inizio del test, ma una volta indossati i cappelli non potranno comunicare tra loro in alcun modo.
Quale strategia garantisce la loro libertà?

Le configurazioni possibili dei cappelli sono 4:
1) Bianco Bianco
2) Bianco Nero
3) Nero Bianco
4) Nero Nero

Strategia 1
-----------
Ogni prigioniero dichiara il colore del cappello dell'altro prigioniero.

Colore P1    Colore P2    Dichiara P1    Dichiara P2    Corretto?
Bianco       Bianco       Bianco         Bianco         Si
Bianco       Nero         Nero           Bianco         No
Nero         Bianco       Bianco         Nero           No
Nero         Nero         Nero           Nero           Si

Strategia 2  --> 50% di possibilità
-----------
Ogni prigioniero dichiara il colore diverso dal cappello dell'altro prigioniero.

Colore P1    Colore P2    Dichiara P1    Dichiara P2    Corretto?
Bianco       Bianco       Nero           Nero           No
Bianco       Nero         Bianco         Nero           Si
Nero         Bianco       Nero           Bianco         Si
Nero         Nero         Bianco         Bianco         No

Strategia 3  --> 75% di possibilità
-----------
Un prigioniero (P1) dichiara il colore del cappello dell'altro prigioniero.
L'altro prigioniero (P2) dichiara sempre Bianco

Colore P1    Colore P2    Dichiara P1    Dichiara P2    Corretto?
Bianco       Bianco       Bianco         Bianco         Si
Bianco       Nero         Nero           Bianco         No
Nero         Bianco       Bianco         Bianco         Si
Nero         Nero         Nero           Bianco         Si

Strategia 4  --> 100% di possibilità
-----------
Un prigioniero (P1) dichiara il colore del cappello dell'altro prigioniero.
L'altro prigioniero (P2) dichiara il colore diverso dal cappello dell'altro prigioniero.

Colore P1    Colore P2    Dichiara P1    Dichiara P2    Corretto?
Bianco       Bianco       Bianco         Nero           Si
Bianco       Nero         Nero           Nero           Si
Nero         Bianco       Bianco         Bianco         Si
Nero         Nero         Nero           Bianco         Si

; Funzione che implementa la strategia 4
(define (test show)
  (local (coloreP1 coloreP2 dichiaraP1 dichiaraP2)
    ; Colore casuale del cappello di P1
    (setq coloreP1 (rand 2))
    ; Colore casuale del cappello di P2
    (setq coloreP2 (rand 2))
    ; Dichiarazione di P1
    (if (= coloreP2 0)
        (setq dichiaraP1 0)
        (setq dichiaraP1 1))
    ; Dichiarazione di P2
    (if (= coloreP1 0)
        (setq dichiaraP2 1)
        (setq dichiaraP2 0))
    (if show (begin
        (println "Cappello P1: " coloreP1 " - Dichiara P1: " dichiaraP1)
        (println "Cappello P2: " coloreP2 " - Dichiara P2: " dichiaraP2)))
    ; Controlla se almeno una dichiarazione è corretta
    (or (= dichiaraP1 coloreP1) (= dichiaraP2 coloreP2))))

Proviamo:

(test true)
;-> Cappello P1: 1 - Dichiara P1: 0
;-> Cappello P2: 0 - Dichiara P2: 0
;-> true
(test true)
;-> Cappello P1: 0 - Dichiara P1: 1
;-> Cappello P2: 1 - Dichiara P2: 1
;-> true

(filter nil? (collect (test) 1000))
;-> ()


------------------------
Da rettangolo a quadrato
------------------------

Abbiamo un rettangolo di 25 per 16 (la sua area è quindi di 400).
Vogliamo tagliare il rettangolo in due pezzi che, uniti, formino un quadrato.
L'obiettivo è ottenere un quadrato di 20 per 20.
La soluzione consiste nell'effettuare un taglio "a gradini".
Si taglia il rettangolo seguendo un percorso a zigzag, come se si disegnassero dei gradini alti 4 e larghi 5.
In questo modo, i due pezzi si incastreranno perfettamente.

  rettangolo: 25 x 16
  area: 25 * 16 = 400
  quadrato finale: 20 x 20
  area: 20 * 20 = 400
  
    5
  +---+-----------+
  |  4| 5         |
  |   +---+       |
  |      4| 5     |
  |       +---+   |
  |          4|   |
  +-----------+---+

Il motivo per cui i gradini hanno dimensioni 5 x 4 è il seguente:
- la differenza tra i lati lunghi è 25 - 20 = 5
- la differenza tra i lati corti è 20 - 16 = 4

Quindi il profilo del taglio può essere costruito alternando:
orizzontale: 5 cm
verticale:   4 cm
e il pezzo ottenuto da un lato viene traslato/riposizionato sull'altro in modo da colmare esattamente le eccedenze, trasformando il rettangolo 25 x 16 nel quadrato 20 x 20.
Il taglio non cambia l'area: redistribuisce semplicemente le parti del rettangolo.

Supponiamo di avere un rettangolo di lati A >= B e vogliamo trasformarlo, con un unico taglio a gradini, in un quadrato di lato S.
Per il tipo di costruzione che stiamo usando, il numero dei gradini è determinato da una condizione aritmetica precisa.
La costruzione è nota come 'step dissection' per un rettangolo A x B (A > B), il lato del quadrato è S = sqrt(A*B).
Dobbiamo verificare se esiste un intero N tale che:

  A/B = N^2/(N-1)^2

Questo significa:
- ci sono N segmenti orizzontali
- ci sono N-1 segmenti verticali
- ogni segmento orizzontale misura A/N
- ogni segmento verticale misura B/(N-1)

Per un rettangolo arbitrario che non soddisfa questa relazione, la dissezione a gradini uniformi non esiste: bisogna usare un'altra dissezione, eventualmente con più pezzi o con gradini non uniformi.

Per risolvere il problema possiamo evitare completamente la ricerca di N: basta ridurre il rapporto A:B dividendo per gcd(A,B).
Se
  A = k*N^2
  B = k*(N-1)^2
allora, dopo la riduzione:
  A/gcd(A,B) = N^2
  B/gcd(A,B) = (N-1)^2
Quindi dobbiamo solo verificare che i due valori ridotti siano quadrati perfetti di interi consecutivi.

Descrizione dell'algoritmo
--------------------------
1) Condizione del rettangolo
Indichiamo con:
  A = lato maggiore
  B = lato minore
e supponiamo che esista un intero N tale che:
  A = k * N^2
  B = k * (N-1)^2
per un certo fattore comune k.
Per esempio:
  36 = 4 * 3^2
  16 = 4 * 2^2
quindi k = 4 e N = 3.

2) Calcolo del GCD
Calcoliamo:
  g = gcd(A,B)
Nel caso 36 x 16:
  gcd(36,16) = 4
Dividendo entrambi i lati per g otteniamo:
  A/g = 36/4 = 9
  B/g = 16/4 = 4
Questi devono essere:
  N^2
  (N-1)^2
rispettivamente.
Quindi:
  9 = 3^2
  4 = 2^2
e le radici devono essere consecutive:
  3 = 2 + 1
Se questa condizione non e' verificata, la funzione restituisce nil.

3) Le radici devono essere consecutive
Il numero dei segmenti orizzontali e verticali differisce di uno.
Se abbiamo N segmenti orizzontali, abbiamo N-1 segmenti verticali.
Le loro lunghezze sono:
  dx = A/N
  dy = B/(N-1)
Nel caso 36 x 16:
  dx = 36/3 = 12
  dy = 16/2 = 8
Il percorso e':
(0,0) (12,0) (12,8) (24,8) (24,16) (36,16)
Quindi abbiamo 3 segmenti orizzontali e 2 segmenti verticali.

4) Lato del quadrato
Il lato del quadrato risultante e':
  S = k * n * (n-1)
Per 36 x 16:
  S = 4 * 3 * 2 = 24
quindi il quadrato e' 24 x 24
Infatti:
  36 * 16 = 576
  24 * 24 = 576
Quindi il rettangolo deve avere rapporto tra i lati uguale al quadrato del rapporto di due interi consecutivi.

5) Passi dell'algoritmo
La funzione esegue quindi questi passi:
1. Ordina i lati in modo che A >= B.
2. Se A = B:
   non serve alcun taglio -> nil.
3. Calcola:
       g = gcd(A,B)
4. Riduce il rapporto:
       a = A/g
       b = B/g
5. Calcola le radici intere candidate:
       n = round(sqrt(a))
       m = round(sqrt(b))
6. Verifica:
       n*n = a
       m*m = b
       n = m+1
7. Se una verifica fallisce:
       restituisce nil.
8. Altrimenti:
       dx = A/n
       dy = B/m
9. Genera alternativamente:
       dx orizzontale
       dy verticale
10. Restituisce la lista dei vertici del percorso.

La parte fondamentale e' quindi la riduzione del rapporto tramite gcd.
In pratica, per sapere se il rettangolo appartiene a questa famiglia, basta controllare se:
  A/gcd(A,B) = N^2
  B/gcd(A,B) = (N-1)^2
per due interi consecutivi N e N-1.

Ad esempio:
  25 x 16  -> 25,16 -> 5^2,4^2 -> possibile
  36 x 16  -> 9,4   -> 3^2,2^2 -> possibile
  50 x 32  -> 25,16 -> 5^2,4^2 -> possibile
  20 x 16  -> 5,4   -> non sono entrambi quadrati -> impossibile

Questa e' anche la ragione per cui non e' necessario che A e B siano quadrati perfetti: e' sufficiente che lo diventino dopo aver eliminato il loro fattore comune massimo.

(define (staircase A B)
  ; Porta il lato maggiore in A.
  (if (< A B) (swap A B))
  ; Un quadrato non necessita di una dissezione.
  (if (= A B)
      nil
      (letn ((g (gcd A B))
             ; Riduce il rapporto A:B ai suoi minimi termini.
             (a (/ A g))
             (b (/ B g))
             ; Calcola le radici quadrate dei due numeri ridotti.
             (n (round (sqrt a)))
             (m (round (sqrt b))))
        ; I numeri ridotti devono essere quadrati perfetti
        ; e le loro radici devono essere consecutive.
        (if (and (= (* n n) a)
                 (= (* m m) b)
                 (= n (+ m 1)))
            (letn ((dx (/ A n))
                   (dy (/ B m))
                   (x 0)
                   (y 0)
                   (path '()))
              ; Inserisce il vertice iniziale del percorso.
              (push (list x y) path -1)
              ; Genera n segmenti orizzontali e m segmenti verticali.
              (for (i 1 n)
                ; Aggiunge un segmento orizzontale.
                (setq x (+ x dx))
                (push (list x y) path -1)
                ; Aggiunge il segmento verticale successivo.
                (if (<= i m)
                    (begin
                      (setq y (+ y dy))
                      (push (list x y) path -1))))
              path)
            nil))))

Esempi:

lisp
(staircase 25 16)
;-> ((0 0) (5 0) (5 4) (10 4) (10 8)
;    (15 8) (15 12) (20 12) (20 16) (25 16))

(staircase 36 16)
;-> ((0 0) (12 0) (12 8) (24 8) (24 16) (36 16))

(staircase 50 32)
;-> ((0 0) (10 0) (10 8) (20 8) (20 16)
;    (30 16) (30 24) (40 24) (40 32) (50 32))

(staircase 20 16)
;-> nil

(staircase 36 16)
;-> ((0 0) (12 0) (12 8) (24 8) (24 16) (36 16))
  gcd(36,16) = 4
  36/4 = 9 = 3^2
  16/4 = 4 = 2^2
  n = 3, m = 2.
  dx = 36/3 = 12
  dy = 16/2 = 8
  12 * 2 = 24
  24 x 24.

Il matematico tedesco David Hilbert dimostrò che qualsiasi poligono (una figura dai lati rettilinei) può essere trasformato in un altro poligono di pari area tagliandolo in un numero finito di pezzi e ricomponendoli.
Invece la trasformazione dei poliedri (solidi tridimensionali dalle facce piane) non è possibile.
In altre parole, dati due poliedri di pari volume non è possibile tagliare il primo in un numero finito di pezzi poliedrici che possano essere ricomposti per formare il secondo.


--------------------------
Indici dei numeri ordinati
--------------------------

Data una lista di numeri interi, restiture l'indice che ogni intero occuperebbe dopo l'ordinamento crescente della lista.

Esempio:
  lista =  (0 7 -2 3 7)
  indici = (0 1  2 3 4)
  lista ordinata = -2 0 3 7 7
  output = (1 3 0 2 4).
Notare che i due 7 mantengono il loro ordine relativo (l'ordinamento è stabile).

(define (sort-index lst)
  (let ((out lst)
        (pair (sort (map (fn(x) (list x $idx)) lst))))
    (dolist (el pair)
      (setf (out (el 1)) $idx))
    out))

Proviamo:

(sort-index '(0 7 -2 3 7))
;-> (1 3 0 2 4)

(sort-index '(0))
;-> (0)
(sort-index '(23))
;-> (0)
(sort-index '(2 2))
;-> (0 1)
(sort-index '(1 2 3 4 5))
;-> (0 1 2 3 4)
(sort-index '(5 4 3 2 1))
;-> (4 3 2 1 0)
(sort-index '(4 4 0 1 1 2 0 1))
;-> (6 7 0 2 3 5 1 4)
(sort-index '(1 1 1 1 1 1 1 0))
;-> (1 2 3 4 5 6 7 0)

Versione code-golf (82 caratteri):

(define(f l(o l))
(dolist(el(sort(map(fn(x)(list x $idx))l)))(setf(o(el 1))$idx))o)

(f '(0))
;-> (0)
(f '(2 2))
;-> (0 1)
(f '(4 4 0 1 1 2 0 1))
;-> (6 7 0 2 3 5 1 4)
(f '(1 1 1 1 1 1 1 0))
;-> (1 2 3 4 5 6 7 0)

============================================================================

