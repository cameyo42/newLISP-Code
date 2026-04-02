================

 NOTE LIBERE 36

================

  "All is true, only nil is false"

----------------
Scatole e monete
----------------

Abbiamo 10 scatole chiuse. In 3 di queste scatole è racchiusa una moneta.
Possiamo scegliere 5 scatole e tenere le monete che troviamo.
Qual è una scommessa equa per giocare a questo gioco in modo che sia equo (cioè non vinciamo e non perdiamo)?
Quali sono le scommesse eque se cambiano sia il numero di scatole con le monete che il numero di scatole scelte ad ogni turno?

Possiamo determinare una scommessa equa facendo una simulaione del gioco.
Ripetendo la simulazione per tante volte (N) siamo in grado di definire quanto guadagniamo.
A questo punto la scommessa equa è data da: guadagno/N.

(define (simula num-monete num-box iter)
  (local (tot box-money box-selected box-valid)
    (setq tot 0)
    (for (i 1 iter)
      (setq box-money (slice (randomize (sequence 0 9)) 0 num-monete))
      (setq box-selected (slice (randomize (sequence 0 9)) 0 num-box))
      (setq box-valid (intersect box-money box-selected))
      ;(println box-money { } box-selected { } box-valid)
      (++ tot (length box-valid)))
    (println "Scommessa equa ("
        num-monete "," num-box ") = " (div tot iter)) '>))

Proviamo:

(simula 3 5 1e6)
;-> Scommessa equa (3,5) = 1.500956

(map (fn(x) (simula 3 x 1e6)) (sequence 1 10))
;-> Scommessa equa (3,1) = 0.300931
;-> Scommessa equa (3,2) = 0.598953
;-> Scommessa equa (3,3) = 0.900397
;-> Scommessa equa (3,4) = 1.200057
;-> Scommessa equa (3,5) = 1.499535
;-> Scommessa equa (3,6) = 1.799221
;-> Scommessa equa (3,7) = 2.100577
;-> Scommessa equa (3,8) = 2.399786
;-> Scommessa equa (3,9) = 2.699307
;-> Scommessa equa (3,10) = 3

(map (fn(x) (simula 1 x 1e6)) (sequence 1 10))
;-> Scommessa equa (1,1) = 0.1003
;-> Scommessa equa (1,2) = 0.199855
;-> Scommessa equa (1,3) = 0.2999
;-> Scommessa equa (1,4) = 0.399957
;-> Scommessa equa (1,5) = 0.500378
;-> Scommessa equa (1,6) = 0.599823
;-> Scommessa equa (1,7) = 0.7000340000000001
;-> Scommessa equa (1,8) = 0.800804
;-> Scommessa equa (1,9) = 0.900355
;-> Scommessa equa (1,10) = 1

(map (fn(x) (simula 10 x 1e6)) (sequence 1 10))
;-> Scommessa equa (10,1) = 1
;-> Scommessa equa (10,2) = 2
;-> Scommessa equa (10,3) = 3
;-> Scommessa equa (10,4) = 4
;-> Scommessa equa (10,5) = 5
;-> Scommessa equa (10,6) = 6
;-> Scommessa equa (10,7) = 7
;-> Scommessa equa (10,8) = 8
;-> Scommessa equa (10,9) = 9
;-> Scommessa equa (10,10) = 10


-------------------
Sondaggi e elezioni
-------------------

I candidati X e Y si contendono la presidenza di una Nazione.
I sondaggi hanno calcolato che la popolazione in età di voto è divisa in tre gruppi (A, B e C) di uguale numero con diverse probabilità di votare.
Ci sono gli elettori sicuri, gli elettori più propensi a votare e gli elettori meno propensi a votare.
Gruppo A: gli elettori sicuri votano sempre (100%).
Gruppo B: gli elettori più propensi a votare votano con una probabilità del 75%.
Gruppo C: gli elettori meno propensi a votare votano con una probabilità del 25%.
Il candidato X ha il 75% dei voti sicuri, il 35% dei voti degli elettori più propensi a votare e il 30% dei voti degli elettori meno propensi a votare.
Qual è la probabilità che il candidato X vinca le elezioni?

Ipotizziamo che ci siano N votanti in tutto (N/3 per ogni gruppo A, B e C).

Rappresentiamo i gruppi nel modo segeunte:
  A = (100 75) ; votanti sicuri, percentuale per X
  B = (75 35)  ; votanti più propensi, percentuale per X
  C = (25 30)  ; votanti meno propensi, percentuale per X

; Funzione che simula la votazione di N persone
(define (voto A B C N show)
  (local (vX vY aX aY bX bY cX cY gruppo pvA pvB pvC pA pB pC)
    (setq vX 0) ; numero totale voti per X
    (setq vY 0) ; numero totale voti per Y
    (setq aX 0 aY 0) ; numero voti di A per X e per Y
    (setq bX 0 bY 0) ; numero voti di B per X e per Y
    (setq cX 0 cY 0) ; numero voti di C per X e per Y
    (setq gruppo (/ N 3))
    (setq pvA (A 0)) ; percentuale di votare di A
    (setq pvB (B 0)) ; percentuale di votare di B
    (setq pvC (C 0)) ; percentuale di votare di C
    (setq pA (A 1)) ; percentuale di A di votare X
    (setq pB (B 1)) ; percentuale di B di votare X
    (setq pC (C 1)) ; percentuale di C di votare X
    ; ciclo delle votazioni
    (for (g 1 gruppo)
      ; il gruppo A vota sempre (pA per X e (1 - pA) per Y)
      (if (>= pA (random 0 1))
        (begin (++ vX) (++ aX))
        (begin (++ vY) (++ aY)))
      ; il gruppo B vota
      (if (>= pvB (random 0 1))
          (if (>= pB (random 0 1))
              (begin (++ vX) (++ bX))
              (begin (++ vY) (++ bY))))
      ; il gruppo C vota
      (if (>= pvC (random 0 1))
          (if (>= pC (random 0 1))
              (begin (++ vX) (++ cX))
              (begin (++ vY) (++ cY)))))
    (when show
      (println "A: " aX { } aY { } (+ aX aY))
      (println "B: " bX { } bY { } (+ bX bY))
      (println "C: " cX { } cY { } (+ cX cY))
      (println "Totale: " vX " per A e " vY " per Y")
      ;(println (+ aX bX cX) { } (+ aY bY cY))
      (println 
        "Perc: " (div vX (+ vX vY)) " per A e " (div vY (+ vX vY)) " per Y"))
  (list vX vY)))

Proviamo:

(voto '(1 0.75) '(0.75 0.35) '(0.25 0.30) 3000 true)
;-> A: 754 246 1000
;-> B: 251 504 755
;-> C: 75 193 268
;-> Totale: 1080 per A e 943 per Y
;-> Perc: 0.5338606030647554 per A e 0.4661393969352447 per Y
;-> (1080 943)

(voto '(1 0.75) '(0.75 0.35) '(0.25 0.30) 300000 true)
;-> A: 75226 24774 100000
;-> B: 26192 48921 75113
;-> C: 7627 17548 25175
;-> Totale: 109045 per A e 91243 per Y
;-> Perc: 0.5444410049528679 per A e 0.4555589950471322 per Y
;-> (109045 91243)

(voto '(1 0.75) '(0.75 0.35) '(0.25 0.30) 30000000 true)
;-> A: 7499301 2500699 10000000
;-> B: 2624944 4876060 7501004
;-> C: 749362 1749986 2499348
;-> Totale: 10873607 per A e 9126745 per Y
;-> Perc: 0.5436707813942475 per A e 0.4563292186057525 per Y
;-> (10873607 9126745)

Matematicamente
---------------
Valore atteso dei voti per X:
Gruppo A (votano sempre)
  quota per X = 0.75
  contributo: (N/3) * 1 * 0.75 = (N/3) * 0.75
Gruppo B
  affluenza = 0.75
  quota per X = 0.35
  contributo: (N/3) * 0.75 * 0.35 = (N/3) * 0.2625
Gruppo C
  affluenza = 0.25
  quota per X = 0.30
  contributo: (N/3) * 0.25 * 0.30 = (N/3) * 0.075

Totale voti attesi per X:
  VX = (N/3) * (0.75 + 0.2625 + 0.075)
  VX = (N/3) * 1.0875

Totale votanti attesi:
  Vtot = (N/3) * (1 + 0.75 + 0.25)
  Vtot = (N/3) * 2

Percentuale attesa per X
  PX = 1.0875 / 2 = 0.54375

La simulazione converge a circa 0.54367 che e coerente con 0.54375.

Attenzione: non abbiamo calcolato la probabilita di vittoria, ma la 'percentuale media dei voti'.
Calcolo della probabilita di vittoria:
Per N grande la frazione di voti si concentra attorno a 0.54375 con varianza piccola.
Poiche 0.54375 > 0.5 segue che probabilita(X vince) -> 1
Quindi:
  per N grande  -> probabilita circa 1
  per N medio   -> probabilita alta ma < 1
  per N piccolo -> probabilita anche sensibilmente < 1

Calcoliamo la probabilità di vittoria con un'altra simulazione:

(define (calcola-prob A B C N iter)
  (local (winX vX vY)
    (setq winX 0)
    (for (i 1 iter)
      (setq v (voto A B C N))
      (setq vX (v 0) vY (v 1))
      (if (> vX vY) (++ winX)))
    (div winX iter)))

Proviamo:

(calcola-prob '(1 0.75) '(0.75 0.35) '(0.25 0.30) 30 1e4)
;-> 0.6153
(calcola-prob '(1 0.75) '(0.75 0.35) '(0.25 0.30) 300 1e4)
;-> 0.9036
(calcola-prob '(1 0.75) '(0.75 0.35) '(0.25 0.30) 3000 1e4)
;-> 1
(calcola-prob '(1 0.75) '(0.75 0.35) '(0.25 0.30) 30000 1e4)

Versione generalizzata
----------------------
Adesso scriviamo una simulazione di voto (simile a 'voto') che prende una lista di gruppi.

Ogni gruppo viene rappresentato con la seguente sottolista:
  (percentuale-di-votare (p(0) p(1) ... p(K-1)) (num-gruppo))
dove:
  'percentuale-di-votare' rappresenta la probabilita che un elemento del gruppo voti
  '(p(0) p(1) ... p(K-1)' percentuali di votare i candidati da 0 a K
  'num-gruppo' numero di persone del gruppo

Per ogni gruppo deve valere p(0) + p(1) + ... + p(K-1) = 1.
La funzione non lo controlla (per velocita), quindi se non sommano a 1 abbiamo una distorsione nei risultati.

(define (voto-m gruppi)
  (local (K voti tot-voti g pvv probs num r cum scelto)
    ; numero di candidati (lunghezza della lista delle probabilita del primo gruppo)
    (setq K (length (gruppi 0 1)))
    ; array dei voti per ciascun candidato inizializzato a 0
    (setq voti (array-list (array K '(0))))
    ; totale dei votanti effettivi
    (setq tot-voti 0)
    ; ciclo su tutti i gruppi
    (dolist (gr gruppi)
      ; probabilita di votare
      (setq pvv (gr 0))
      ; lista delle probabilita dei candidati
      (setq probs (gr 1))
      ; numero di persone nel gruppo
      (setq num (gr 2))
      ; ciclo sulle persone del gruppo
      (for (i 1 num)
        ; verifica se la persona vota
        (if (>= pvv (random 0 1))
          (begin
            ; incrementa numero totale votanti
            (++ tot-voti)
            ; selezione del candidato con metodo cumulativo (roulette)
            (setq r (random 0 1)) ; numero casuale in [0,1)
            (setq cum 0)          ; cumulata delle probabilita
            (setq scelto nil)     ; candidato scelto (indice)
            ; scorri i candidati finche la cumulata supera r
            (for (k 0 (- K 1) 1 scelto)
              (setq cum (add cum (probs k)))
              (if (>= cum r)
                  (setq scelto k)))
            ; fallback nel caso di errori numerici (arrotondamenti)
            (if (nil? scelto) (setq scelto (- K 1)))
            ; aggiorna il numero di voti del candidato scelto
            (setf (voti scelto) (add (voti scelto) 1))))))
    ; stampa risultati
    (println "Totale votanti: " tot-voti)
    (for (k 0 (- K 1))
      (println "Candidato " k ": " (voti k)
               " (" (div (voti k) tot-voti) ")"))
    ; restituisce il vettore dei voti
    voti))

Proviamo:

(setq gruppi '((1    (0.75 0.25) 100000)
               (0.75 (0.35 0.65) 100000)
               (0.25 (0.30 0.70) 100000)))

(voto-m gruppi)
;-> Totale votanti: 199889
;-> Candidato 0: 108495 (0.5427762408136516)
;-> Candidato 1: 91394 (0.4572237591863484)
;-> (108495 91394)

(setq gruppi '((1    (0.75 0.15 0.10) 100)
               (0.75 (0.40 0.35 0.25) 1000)
               (0.25 (0.60 0.30 0.10) 100000)))

(voto-m gruppi)
;-> Totale votanti: 25686
;-> Candidato 0: 15170 (0.5905940979521919)
;-> Candidato 1: 7783 (0.3030055283033559)
;-> Candidato 2: 2733 (0.1064003737444522)
;-> (15170 7783 2733)

============================================================================

