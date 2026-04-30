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


------------------------------
Contare i punti di una stringa
------------------------------

Data una stringa ASCII (32-126), scrivere una funzione che conta i 'punti'.
I caratteri "i", "j", ".", "!", "?", ";" hanno un 'punto' ciascuno.
Il carattere ":" ha due 'punti'.
Nessun altro carattere ha 'punti'.

Scrivere la funzione più veloce e la funzione più corta.

(setq word "ij.!?;:")
(setq chars (explode word))
;-> ("i" "j" "." "!" "?" ";" ":")
(setq ascii (map char chars))
;-> (105 106 46 33 63 59 58)

; Funzione 1
; if multiplo
(define (conta1 str)
  (let (tot 0)
    (dostring (ch str)
      (if (or (= ch 105) (= ch 106) (= ch 46)
              (= ch 33) (= ch 63) (= ch 59))
          (++ tot)
          (= ch 58) (++ tot 2)))
    tot))

(setq s '("newlisp" "ij.!?;" "ij.!?;:" "ij.!?;: ij.!?;:"))
(map conta1 s)
;-> (1 6 8 16)

; Funzione 2
; cond
(define (conta2 str)
  (let (tot 0)
    (dostring (ch str)
      (cond ((or (= ch 105) (= ch 106) (= ch 46)
                 (= ch 33) (= ch 63) (= ch 59))
              (++ tot))
            ((= ch 58) (++ tot 2))))
    tot))

(map conta2 s)
;-> (1 6 8 16)

; Funzione 3
; dolist (explode str)
(define (conta3 str)
  (let (tot 0)
    (dolist (ch (explode str))
      (cond ((or (= ch "i") (= ch "j") (= ch ".")
                 (= ch "!") (= ch "?") (= ch ";"))
              (++ tot))
            ((= ch ":") (++ tot 2))))
    tot))

(map conta3 s)
;-> (1 6 8 16)

; Funzione 4
; count e apply
(define (conta4 str)
  (let (lst (explode str))
    (+ (apply + (count '("i" "j" "." "!" "?" ";") lst))
       (* 2 (apply + (count '(":") lst))))))

(map conta4 s)
;-> (1 6 8 16)

Test di velocità:

(setq test (dup "ij.!?;: " 100))
(time (conta1 test) 1e3)
;-> 124.11
(time (conta2 test) 1e3)
;-> 125.978
(time (conta3 test) 1e3)
;-> 233.872
(time (conta4 test) 1e3)
;-> 221.441

; Funzione 5
; funzione più corta: 89 caratteri
; che è anche la più veloce
(define(f s,t)(dostring(c s)(if(ref c '(33 46 59 63 105 106))(inc t)(= c 58)(inc t 2)))t)

(map f s)
;-> (1 6 8 16)
(time (f test) 1e3)
;-> 84.914


-------------
Windows years
-------------

(setq data '(("Windows 1.0" 1985)
             ("Windows 2.0" 1987)
             ("Windows 3.0" 1990)
             ("Windows 3.1" 1992)
             ("Windows NT 3.1" 1993)
             ("Windows NT 3.5" 1994)
             ("Windows NT 3.51" 1995)
             ("Windows 95" 1996)
             ("Windows NT 4.0" 1996)
             ("Windows 98" 1998)
             ("Windows 98 SE" 1999)
             ("Windows 2000" 2000)
             ("Windows Me" 2000)
             ("Windows XP" 2001)
             ("Windows Vista" 2007)
             ("Windows 7" 2009)
             ("Windows 8" 2012)
             ("Windows 8.1" 2013)
             ("Windows 10" 2015)
             ("Windows 11" 2021)))

In quale anno è uscito Windows 3.0 ?
(setq name "Windows 3.0")
(find (list name '?) data match)
;-> 2
$0
;-> ("windows 3.0" 1990)

Quali sono le versioni uscite fino a Windows 3.0 ?
(filter (fn(x) (<= (x 1) ($0 1))) data)
;-> (("Windows 1.0" 1985) ("Windows 2.0" 1987) ("Windows 3.0" 1990))

Che versione è uscita nel 2001 ?
(setq year 2001)
(find (list '? year) data match)
;-> 13
$0
;-> ("Windows XP" 2001)

Quali sono le versioni uscite fino al 2001 ?
(filter (fn(x) (<= (x 1) year)) data)
;-> (("Windows 1.0" 1985) ("Windows 2.0" 1987) ("Windows 3.0" 1990) ("Windows 3.1" 1992)
;->  ("Windows NT 3.1" 1993)
;->  ("Windows NT 3.5" 1994)
;->  ("Windows NT 3.51" 1995)
;->  ("Windows 95" 1996)
;->  ("Windows NT 4.0" 1996)
;->  ("Windows 98" 1998)
;->  ("Windows 98 SE" 1999)
;->  ("Windows 2000" 2000)
;->  ("Windows Me" 2000)
;->  ("Windows XP" 2001))

Quali sono le versioni uscite dal 2000 al 2005 ?
(setq year1 2000)
(setq year2 2005)
(filter (fn(x) (and (>= (x 1) year1) (<= (x 1) year2))) data)
;-> (("Windows 2000" 2000) ("Windows Me" 2000) ("Windows XP" 2001))


-----------------
Numero più grande
-----------------

Quale numero è più grande 2^120 oppure 3^90?

Occorre usare la proprietà: x^(a*b) = (x^a)^b

  2^120 = 2^(4*30) = (2^4)^30 = 16^30
  3^90  = 3^(3*30) = (3^3)^30 = 27^30

Poichè 27^30 > 16^30, risulta 27^90 > 2^120.

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(> (** 3 90) (** 2 120))
;-> true


---------------
Corde parallele
---------------

In una circonferenza ci sono due corde parallele che distano 8.
La lunghezza delle corde vale 18 e 14.
Quanto vale il diametro della circonferenza?

Vedi figura "corde-parallele.jpg" nella cartella "data".

  d = 2*r

Teorema di pitagora:
  x^2 + 9^2 = r^2
  y^2 + 7^2 = r^2

Inoltre:
  x + y = 8

Risolviamo il sistema (incognite x, y e r):

| x^2 + 9^2 = r^2            (1)
| y^2 + 7^2 = r^2            (2)
| x + y = 8                  (3)

Ricaviamo x dalla (1)
  
  x = sqrt(r^2 - 81)         (4)

Ricaviamo y^2 dalla (2):
  
  y^2 = r^2 - 49             (5)

Ricaviamo y dalla (3):
  
  y = 8 - x                  (6)  

Sostituiamo la (6) nella (5):
(a + b)^2 = a^2 + b^2 + 2ab

  (8 - x)^2 = r^2 - 49
  64 - 16x + x^2 = r^2 - 49  (7)

Ricaviamo r^2 dalla (7):

  r^2 = x^2 - 16x + 113      (8)

Sotituiamo la (4) nella (8)

 r^2 = r^2 - 81 - 16*sqrt(r^2 - 81) + 113  (9)

Semplificando la (9) otteniamo:
 
  16*sqrt(r^2 - 81) = 32
  sqrt(r^2 - 81) = 2
  (r^2 - 81) = 4
  r^2 = 85
  r = sqrt(85)

Quindi il diametro vale:

  d = 2*r = 2*(sqrt(85)) = 18.43908891458577...


-------------------------------
Attacco tra pezzi degli scacchi
-------------------------------

Date le posizioni di due pezzi degli scacchi in una scacchiera, determinare se si attaccano tra loro.
I pezzi sono pedone (P), cavallo (C), alfiere (A), torre (T), regina (Q) e re (K).

Conversione di una posizione di scacchi tra Notazione algebrica <-> Notazione matriciale

     Scacchiera con                          Matrice della posizione
     coordinate algebriche
                                              0   1   2   3   4   5   6   7
    +---+---+---+---+---+---+---+---+       +---+---+---+---+---+---+---+---+
  8 |   |   |   |   |   |   |   |   |     0 |   |   |   |   |   |   |   |   |
    +---+---+---+---+---+---+---+---+       +---+---+---+---+---+---+---+---+
  7 |   |   |   |   |   |   |   |   |     1 |   |   |   |   |   |   |   |   |
    +---+---+---+---+---+---+---+---+       +---+---+---+---+---+---+---+---+
  6 |   |   |   |   |   |   |   |   |     2 |   |   |   |   |   |   |   |   |
    +---+---+---+---+---+---+---+---+       +---+---+---+---+---+---+---+---+
  5 |   |   |   |   |   |   |   |   |     3 |   |   |   |   |   |   |   |   |
    +---+---+---+---+---+---+---+---+       +---+---+---+---+---+---+---+---+
  4 |   |   |   |   |   |   |   |   |     4 |   |   |   |   |   |   |   |   |
    +---+---+---+---+---+---+---+---+       +---+---+---+---+---+---+---+---+
  3 |   |   |   |   |   |   |   |   |     5 |   |   |   |   |   |   |   |   |
    +---+---+---+---+---+---+---+---+       +---+---+---+---+---+---+---+---+
  2 |   |   |   |   |   |   |   |   |     6 |   |   |   |   |   |   |   |   |
    +---+---+---+---+---+---+---+---+       +---+---+---+---+---+---+---+---+
  1 |   |   |   |   |   |   |   |   |     7 |   |   |   |   |   |   |   |   |
    +---+---+---+---+---+---+---+---+       +---+---+---+---+---+---+---+---+
      a   b   c   d   e   f   g   h

Liste di associazione che collegano Coordinate algebriche <-> Indici della matrice

(setq indexes-algebric
     '(((0 0) "a8") ((0 1) "b8") ((0 2) "c8") ((0 3) "d8")
       ((0 4) "e8") ((0 5) "f8") ((0 6) "g8") ((0 7) "h8")
       ((1 0) "a7") ((1 1) "b7") ((1 2) "c7") ((1 3) "d7")
       ((1 4) "e7") ((1 5) "f7") ((1 6) "g7") ((1 7) "h7")
       ((2 0) "a6") ((2 1) "b6") ((2 2) "c6") ((2 3) "d6")
       ((2 4) "e6") ((2 5) "f6") ((2 6) "g6") ((2 7) "h6")
       ((3 0) "a5") ((3 1) "b5") ((3 2) "c5") ((3 3) "d5")
       ((3 4) "e5") ((3 5) "f5") ((3 6) "g5") ((3 7) "h5")
       ((4 0) "a4") ((4 1) "b4") ((4 2) "c4") ((4 3) "d4")
       ((4 4) "e4") ((4 5) "f4") ((4 6) "g4") ((4 7) "h4")
       ((5 0) "a3") ((5 1) "b3") ((5 2) "c3") ((5 3) "d3")
       ((5 4) "e3") ((5 5) "f3") ((5 6) "g3") ((5 7) "h3")
       ((6 0) "a2") ((6 1) "b2") ((6 2) "c2") ((6 3) "d2")
       ((6 4) "e2") ((6 5) "f2") ((6 6) "g2") ((6 7) "h2")
       ((7 0) "a1") ((7 1) "b1") ((7 2) "c1") ((7 3) "d1")
       ((7 4) "e1") ((7 5) "f1") ((7 6) "g1") ((7 7) "h1")))

Funzione che converte da indici della matrice a notazione algebrica:

(define (mat-alg ij)
  (lookup (list ij) indexes-algebric))

(mat-alg '(0 0))
;-> "a8"
(mat-alg '(7 7))
;-> "h1"
(mat-alg '(2 6))
;-> "g6"

(setq algebric-indexes
   '(("a8" (0 0)) ("b8" (0 1)) ("c8" (0 2)) ("d8" (0 3))
     ("e8" (0 4)) ("f8" (0 5)) ("g8" (0 6)) ("h8" (0 7))
     ("a7" (1 0)) ("b7" (1 1)) ("c7" (1 2)) ("d7" (1 3))
     ("e7" (1 4)) ("f7" (1 5)) ("g7" (1 6)) ("h7" (1 7))
     ("a6" (2 0)) ("b6" (2 1)) ("c6" (2 2)) ("d6" (2 3))
     ("e6" (2 4)) ("f6" (2 5)) ("g6" (2 6)) ("h6" (2 7))
     ("a5" (3 0)) ("b5" (3 1)) ("c5" (3 2)) ("d5" (3 3))
     ("e5" (3 4)) ("f5" (3 5)) ("g5" (3 6)) ("h5" (3 7))
     ("a4" (4 0)) ("b4" (4 1)) ("c4" (4 2)) ("d4" (4 3))
     ("e4" (4 4)) ("f4" (4 5)) ("g4" (4 6)) ("h4" (4 7))
     ("a3" (5 0)) ("b3" (5 1)) ("c3" (5 2)) ("d3" (5 3))
     ("e3" (5 4)) ("f3" (5 5)) ("g3" (5 6)) ("h3" (5 7))
     ("a2" (6 0)) ("b2" (6 1)) ("c2" (6 2)) ("d2" (6 3))
     ("e2" (6 4)) ("f2" (6 5)) ("g2" (6 6)) ("h2" (6 7))
     ("a1" (7 0)) ("b1" (7 1)) ("c1" (7 2)) ("d1" (7 3))
     ("e1" (7 4)) ("f1" (7 5)) ("g1" (7 6)) ("h1" (7 7))))

Funzione che converte da notazione algebrica a indici della matrice:

(define (alg-mat c)
  (lookup (list c) algebric-indexes))

(alg-mat "a8")
;-> (0 0)
(alg-mat "h1")
;-> (7 7)
(alg-mat "g6")
;-> (2 6)

Vediamo quando due pezzi si attaccano.

Coordinate:
  P1 = (r1,c1)
  P2 = (r2,c2)
Convenzioni:
  dr = abs(r1 - r2)
  dc = abs(c1 - c2)

REGINA (Q)
(Q,Q): (r1 = r2) or (c1 = c2) or (dr = dc)
(Q,T): (r1 = r2) or (c1 = c2)
(Q,A): dr = dc
(Q,C): (dr = 1 and dc = 2) or (dr = 2 and dc = 1)
(Q,K): max(dr,dc) = 1
(Q,P): (dr = 1 and dc = 1)

TORRE (T)
(T,T): (r1 = r2) or (c1 = c2)
(T,A): (r1 = r2) or (c1 = c2)
(T,C): (dr = 1 and dc = 2) or (dr = 2 and dc = 1)
(T,K): max(dr,dc) = 1
(T,P): (dr = 1 and dc = 1)

ALFIERE (A)
(A,A): dr = dc
(A,C): (dr = 1 and dc = 2) or (dr = 2 and dc = 1)
(A,K): max(dr,dc) = 1
(A,P): (dr = 1 and dc = 1)

CAVALLO (C)
(C,C): (dr = 1 and dc = 2) or (dr = 2 and dc = 1)
(C,K): max(dr,dc) = 1
(C,P): (dr = 1 and dc = 1)

RE (K)
(K,K): max(dr,dc) = 1
(K,P): (dr = 1 and dc = 1)

PEDONE (P)
(P,P): (dr = 1 and dc = 1)

Adesso dobbiamo considerare la direzione del pedone (bianco/nero).

  P1 = pezzo bianco
  P2 = pezzo nero
  P1 = (r1,c1), P2 = (r2,c2)
  dr = r2 - r1
  dc = c2 - c1
  adr = abs(dr)
  adc = abs(dc)

Il pedone bianco attacca in avanti (supponendo che il bianco 'salga' sulla scacchiera):

Pedone bianco (P1)
  attacca = (dr = -1 and abs(c2 - c1) = 1)

Pedone nero (P2)
  attacca = (dr = 1 and abs(c2 - c1) = 1)

Adesso scriviamo una funzione unica 'attacco(P1,P2,tipo1,tipo2)'.

B = pezzo bianco (tipo1, r1, c1)
N = pezzo nero (tipo2, r2, c2)
tipi: "Q" "T" "A" "C" "K" "P"

(define (attacco tipo1 tipo2 r1 c1 r2 c2)
  (letn (
         ; differenze tra le coordinate
         dr (- r2 r1)
         dc (- c2 c1)
         ; valori assoluti (utili per molte regole)
         adr (abs dr)
         adc (abs dc)
         ; flag: attacco Bianco->Nero e Nero->Bianco
         b nil
         n nil)
    ; Attacco BIANCO -> NERO
    (cond
      ; Regina: riga, colonna o diagonale
      ((= tipo1 "Q")
        (setq b (or (= r1 r2) (= c1 c2) (= adr adc))))
      ; Torre: riga o colonna
      ((= tipo1 "T")
        (setq b (or (= r1 r2) (= c1 c2))))
      ; Alfiere: diagonale
      ((= tipo1 "A")
        (setq b (= adr adc)))
      ; Cavallo: mossa a L
      ((= tipo1 "C")
        (setq b (or (and (= adr 1) (= adc 2))
                    (and (= adr 2) (= adc 1)))))
      ; Re: una casella in qualsiasi direzione
      ((= tipo1 "K")
        (setq b (= (max adr adc) 1)))
      ; Pedone bianco: attacca in avanti (dr = -1) in diagonale
      ((= tipo1 "P")
        (setq b (and (= dr -1) (= adc 1)))))
    ; Attacco NERO -> BIANCO
    (cond
      ; Regina
      ((= tipo2 "Q")
        (setq n (or (= r1 r2) (= c1 c2) (= adr adc))))
      ; Torre
      ((= tipo2 "T")
        (setq n (or (= r1 r2) (= c1 c2))))
      ; Alfiere
      ((= tipo2 "A")
        (setq n (= adr adc)))
      ; Cavallo
      ((= tipo2 "C")
        (setq n (or (and (= adr 1) (= adc 2))
                    (and (= adr 2) (= adc 1)))))
      ; Re
      ((= tipo2 "K")
        (setq n (= (max adr adc) 1)))
      ; Pedone nero: attacca in avanti (dr = +1) in diagonale
      ((= tipo2 "P")
        (setq n (and (= dr 1) (= adc 1)))))
    ; Risultato finale
    (cond
      ((and b n) "B<->N") ; attacco reciproco
      (b "B->N")          ; solo bianco attacca
      (n "N->B")          ; solo nero attacca
      (true nil))))       ; nessun attacco

Proviamo:

(attacco "P" "K" 4 4 3 5)
;-> "B<->N"
(attacco "K" "P" 4 4 5 5)
;-> "B<->N"
(attacco "T" "A" 1 1 1 8)
;-> "B->N"
(attacco "Q" "T" 0 0 1 1)
;-> "B->N"
(attacco "Q" "T" 4 4 4 7)
;-> "B<->N"
(attacco "Q" "T" 4 4 7 7)
;-> "B->N"
(attacco "P" "K" 4 4 3 5)
;-> "B<->N"
(attacco "K" "P" 4 4 5 5)
;-> "N<->B"
(attacco "P" "K" 4 4 5 5)
;-> "N->B"
(attacco "Q" "Q" 0 0 3 2)
;-> nil

Versione senza commenti

(define (attacco tipo1 tipo2 r1 c1 r2 c2)
  (letn (dr (- r2 r1) dc (- c2 c1) adr (abs dr) adc (abs dc) b nil n nil)
    (cond ; attacco Bianco -> Nero
      ((= tipo1 "Q") (setq b (or (= r1 r2) (= c1 c2) (= adr adc))))
      ((= tipo1 "T") (setq b (or (= r1 r2) (= c1 c2))))
      ((= tipo1 "A") (setq b (= adr adc)))
      ((= tipo1 "C") (setq b (or (and (= adr 1) (= adc 2)) (and (= adr 2) (= adc 1)))))
      ((= tipo1 "K") (setq b (= (max adr adc) 1)))
      ((= tipo1 "P") (setq b (and (= dr -1) (= adc 1)))))
    (cond ; attacco Nero -> Bianco
      ((= tipo2 "Q") (setq n (or (= r1 r2) (= c1 c2) (= adr adc))))
      ((= tipo2 "T") (setq n (or (= r1 r2) (= c1 c2))))
      ((= tipo2 "A") (setq n (= adr adc)))
      ((= tipo2 "C") (setq n (or (and (= adr 1) (= adc 2)) (and (= adr 2) (= adc 1)))))
      ((= tipo2 "K") (setq n (= (max adr adc) 1)))
      ((= tipo2 "P") (setq n (and (= dr 1) (= adc 1)))))
    (cond ; risultato
      ((and b n) "B<->N")
      (b "B->N")
      (n "N->B")
      (true nil))))

Ecco una versione che genera direttamente le caselle intermedie.
Utile per verificare ostacoli (basta controllare la lista restituita).
; Q/T/A -> lista caselle tra i pezzi
; C/K/P -> lista vuota () perché saltano o sono adiacenti
; Se non c'è attacco -> nil

(define (attacco-path tipo1 tipo2 r1 c1 r2 c2)
  (letn (dr (- r2 r1) dc (- c2 c1) adr (abs dr) adc (abs dc) sr (if (> dr 0) 1 (if (< dr 0) -1 0)) sc (if (> dc 0) 1 (if (< dc 0) -1 0))
    (valid?
      (lambda (tipo dr dc adr adc)
        (cond
          ((= tipo "Q") (or (= adr 0) (= adc 0) (= adr adc)))
          ((= tipo "T") (or (= adr 0) (= adc 0)))
          ((= tipo "A") (= adr adc))
          ((= tipo "C") (or (and (= adr 1) (= adc 2)) (and (= adr 2) (= adc 1))))
          ((= tipo "K") (= (max adr adc) 1))
          ((= tipo "P") (and (= dr -1) (= adc 1))))))
    (lineare?
      (lambda (tipo)
        (or (= tipo "Q") (= tipo "T") (= tipo "A"))))
    (path
      (lambda ()
        (let (p '())
          (for (k 1 (- (max adr adc) 1))
            (push (list (+ r1 (* k sr)) (+ c1 (* k sc))) p -1))
          p))))
    (let (b nil n nil pb '() pn '())
      (if (valid? tipo1 dr dc adr adc)
        (begin
          (setq b true)
          (if (lineare? tipo1)
            (setq pb (path))
            (setq pb '()))))
      (if (valid? tipo2 (- dr) (- dc) adr adc)
        (begin
          (setq n true)
          (if (lineare? tipo2)
            (setq pn (path))
            (setq pn '()))))
      (cond
        ((and b n)
          (list "B<->N" (if (= pb pn) pb (list pb pn))))
        (b
          (list "B->N" pb))
        (n
          (list "N->B" pn))
        (true
          (list nil '()))))))

Proviamo:

(attacco "P" "K" 4 4 3 5)
;-> "B<->N"
(attacco-path "P" "K" 4 4 3 5)
;-> ("B<->N" ())
(attacco-path "K" "P" 4 4 5 5)
;-> ("B<->N" ())
(attacco-path "T" "A" 1 1 1 8)
;-> ("B->N" ((1 2) (1 3) (1 4) (1 5) (1 6) (1 7)))
(attacco-path "Q" "T" 0 0 1 1)
;-> ("B->N" ((1 1) (0 0)))
(attacco-path "Q" "T" 4 4 4 7)
;-> ("B<->N" ((4 5) (4 6)))
(attacco-path "Q" "T" 4 4 7 7)
;-> ("B->N" ((5 5) (6 6)))
(attacco-path "P" "K" 4 4 3 5)
;-> ("B<->N" ())
(attacco-path "K" "P" 4 4 5 5)
;-> ("B<->N" ())
(attacco-path "P" "K" 4 4 5 5)
;-> ("N->B" ())
(attacco-path "Q" "Q" 0 0 3 2)
;-> (nil ())

Per verificare eventuali ostacoli basta controllare che tutte le caselle del 'path' siano libere.


----------------------
Numeri e cifre casuali
----------------------

Per generare un numero casuale tra 0 e N usiamo due metodi:
1) generazione del numero casuale con la primitiva 'rand'.
2) generazione del numero casuale usando K volte la primitiva 'rand' per generare le K cifre di cui è composto il numero.
Per esempio con N = 999:

Metodo 1
--------
(setq N 999)
(rand (+ N 1))
;-> 563

Metodo 2
--------
; numero delle cifre
(setq K (length N))
; creazione del numero casuale unendo K cifre causali
(setq val (int (join (collect (string (rand 10)) K)) 0 10))
;-> 175

I due metodi sono equivalenti?
Cioè, la probabilità di ottenere un numero qualsiasi tra 0 e N è la stessa per entrambi i metodi?

Per semplicità consideriamo il numero N composto da K cifre tutte uguali a 9.
In questo caso con il primo metodo la probabilità di ottenere un certo numero tra 0 e N vale:

  P(x) = 1/(N+1)

Con il secondo metodo la probabilità di ottenere un certo numero tra 0 e N vale:

  P(x) = Prod[i=0..K](1/10)

Per K = 2
1) N = 99 --> P(x) = 1/100 = 0.01
2) P(x) = 1/10 * 1/10 = 0.01

Per K = 3
1) N = 999 --> P(x) = 1/1000 = 0.001
2) P(x) = 1/10 * 1/10 * 1/10 = 0.001

Quindi i due metodi sono equivalenti.

Scriviamo una funzione di simulazione per verificare questo risultato.
Per confrontare i due metodi calcoliamo l'errore percentuale tra i valori teorici e i valori ottenuti dalla simulazione.
L'errore percentuale si calcola dividendo l'errore assoluto (cioè la differenza assoluta tra valore misurato e reale) per il valore reale (o atteso), moltiplicando poi il risultato per 100.
La formula è:

              abs(ValoreMisurato - ValoreReale)
  Errore% = ------------------------------------- * 100
                         ValoreReale

Esprime l'incertezza relativa in percentuale, indicando la precisione della misura.

; Funzione di simulazione dei due metodi di generazione di numeri casuali
; Restituisce l'errore percentuale massimo di entrambi i metodi
; Genera risultati corretti solo per num = 9, 99, 999, ...
(define (simula num iter)
  (local (len l1 l2 freq freq2 val1 val2 f1 f2 fp1 fp2 err1 err2)
    (setq len (length num))
    (setq l1 (pow 10 len))
    (setq l2 l1)
    ; vettore per contare le occorrenze dei numeri ottenuti col metodo 1
    (setq freq1 (array l1 '(0)))
    ; vettore per contare le occorrenze dei numeri ottenuti col metodo 2
    (setq freq2 (array l2 '(0)))
    ; ciclo di generazione dei numeri casuali...
    (for (i 1 iter)
      ; metodo 1
      (setq val1 (rand (+ num 1)))
      (++ (freq1 val1))
      ; metodo 2
      ;(setq val2 (int (join (collect (string (rand 10)) len)) 0 10))
      (setq val2 0)
      (for (k 1 len) (setq val2 (+ (rand 10) (* val2 10))))
      (++ (freq2 val2))
      ;(print val1 { } val2) (read-line)
    )
    ; calcolo delle frequenze dei numeri casuali generati
    (setq f1 (array-list freq1))
    (setq f2 (array-list freq2))
    (setq fp1 (map (fn(x) (div x iter)) f1))
    (setq fp2 (map (fn(x) (div x iter)) f2))
    ; Probabilità teorica di ogni numero
    (setq prob (div (+ num 1)))
    ; calcolo degli errori percentuali tra la probabilità teorica e
    ; le frequenze dei numeri casuali generati con i due metodi
    (setq err1 (map (fn(x) (mul (div (abs (sub x prob)) prob) 100)) fp1))
    (setq err2 (map (fn(x) (mul (div (abs (sub x prob)) prob) 100)) fp2))
    ; restituisce i valori degli errori massimi per i due metodi
    (list (apply max err1) (apply max err2))))

Proviamo:

(simula 9 1e4)
;-> (6.499999999999992 4.600000000000007)
(simula 9 1e5)
;-> (1.680000000000001 1.990000000000006)
(simula 9 1e6)
;-> (0.5259999999999987 0.5750000000000061)

(simula 999 1e4)
;-> (100 130)
(simula 999 1e5)
;-> (33 31)
(simula 999 1e6)
;-> (12.6 9.799999999999999)
(time (println (simula 999 1e7)))
;-> (4.770000000000002 3.729999999999988)
;-> 4859.716

(simula 9999 1e4)
;-> (499.9999999999999 599.9999999999999)
(simula 9999 1e5)
;-> (150 130)
(simula 9999 1e6)
;-> (71 46.99999999999999)
(time (println (simula 9999 1e7)))
;-> (33.49999999999999 11.60000000000001)
;-> 5812.763

Gli errori dei due metodi sono sempre simili (stesso ordine di grandezza).
I risultati della simulazione confermano che i due metodi sono equivalenti.

Attenzione:
Il metodo 2 è valido solo se N vale 9, 99, 999, ecc.
Perchè è uniforme su (0, 1, ..., 10^K - 1) solo quando l'intervallo coincide esattamente con questo insieme.
Appena cambiamo N, NON sono più equivalenti
Esempio: N=500 e K=3
Il metodo 2 genera numeri da 000 a 999, tutti con probabilità 1/1000.
Ma noi vogliamo solo (0..500).
Quindi i numeri 0–500 compaiono con probabilità 1/1000, ma i numeri 501–999 vengono comunque generati.
Se li scartiamo (rejection sampling), allora la distribuzione finale torna uniforme.
Se NON li scartiamo, allora la distribuzione NON è uniforme.

Rejection sampling
------------------
Generiamo un numero come nel metodo 2
Se il numero è <= N -> lo accettiamo
Se il numero è > N -> lo scartiamo e generiamo un nuovo numero

; Funzione di simulazione dei due metodi di generazione di numeri casuali
; Restituisce l'errore percentuale massimo di entrambi i metodi
(define (simula2 num iter)
  (local (len l1 l2 freq freq2 val1 val2 f1 f2 fp1 fp2 err1 err2 ok)
    (setq len (length num))
    (setq l1 (+ num 1))
    (setq l2 l1)
    ; vettore per contare le occorrenze dei numeri ottenuti col metodo 1
    (setq freq1 (array l1 '(0)))
    ; vettore per contare le occorrenze dei numeri ottenuti col metodo 2
    (setq freq2 (array l2 '(0)))
    ; ciclo di generazione dei numeri casuali...
    (for (i 1 iter)
      ; metodo 1
      (setq val1 (rand (+ num 1)))
      (++ (freq1 val1))
      ; metodo 2 (rejection sampling)
      (setq ok nil)
      (until ok
        (setq val2 0)
        (for (k 1 len) (setq val2 (+ (rand 10) (* val2 10))))
        (if (<= val2 num) (setq ok true)))
      (++ (freq2 val2))
      ;(print val1 { } val2) (read-line)
    )
    ; calcolo delle frequenze dei numeri casuali generati
    (setq f1 (array-list freq1))
    (setq f2 (array-list freq2))
    (setq fp1 (map (fn(x) (div x iter)) f1))
    (setq fp2 (map (fn(x) (div x iter)) f2))
    ; Probabilità teorica di ogni numero
    (setq prob (div (+ num 1)))
    ; calcolo degli errori percentuali tra la probabilità teorica e
    ; le frequenze dei numeri casuali generati con i due metodi
    (setq err1 (map (fn(x) (mul (div (abs (sub x prob)) prob) 100)) fp1))
    (setq err2 (map (fn(x) (mul (div (abs (sub x prob)) prob) 100)) fp2))
    ; restituisce i valori degli errori massimi per i due metodi
    (list (apply max err1) (apply max err2))))

Proviamo:

(simula2 50 1e4)
;-> (19.41999999999999 16.79)
(simula2 50 1e5)
;-> (5.904999999999993 5.417000000000006)
(simula2 50 1e6)
;-> (1.473100000000002 1.6073)

(simula2 500 1e4)
;-> (85.37000000000002 59.92)
(simula2 500 1e5)
;-> (20.24 23.74700000000001)
(simula2 500 1e6)
;-> (7.615599999999996 6.81320000000002)
(time (println (simula2 500 1e7)))
;-> (3.200990000000005 2.239070000000003)
;-> 9703.915000000001

(simula2 5000 1e4)
;-> (350.09 400.1)
(simula2 5000 1e5)
;-> (110.042 85.03700000000001)
(simula2 5000 1e6)
;-> (30.02599999999999 26.02520000000001)
(time (println (simula2 5000 1e7)))
;-> (15.53310999999999 7.631530000000002)
;-> 11485.042
(time (println (simula2 5000 1e8)))
;-> (10.277059 2.445485)
;-> 115422.249


---------------------------------
Indice di correlazione di Pearson
---------------------------------

Il coefficiente di correlazione di Pearson (r) è una misura statistica che valuta la forza e la direzione della relazione lineare tra due variabili quantitative.
Varia da -1 (correlazione negativa perfetta) a +1 (correlazione positiva perfetta), dove 0 indica assenza di relazione lineare.

Indica come una variabile cambia al variare dell'altra e misura solo relazioni di tipo lineare (una linea retta).
Un valore vicino a +1 indica che all'aumentare di una variabile, l'altra aumenta.
Un valore vicino a -1 indica che all'aumentare di una, l'altra diminuisce.
Un valore vicino a 0 suggerisce nessuna relazione lineare.

Non implica causalità, cioè una correlazione alta non significa che una variabile causa l'altra.
Questo indice è molto sensibile agli 'outlier'.

Formula matematica:

Date due liste X e Y con lo stesso numero di elementi:

 X = (x1, x2, ..., xn)
 Y = (y1, y2, ..., yn)

L'indice si calcola dividendo la covarianza tra le due variabili per il prodotto dei loro scarti quadratici medi:


        Sum[i=1,n](x(i) - xm)*(y(i) - ym)
  r = ---------------------------------------------------
        Sum[i=1,n](x(i) - xm)^2*Sum[i=1,n](y(i) - ym)^2

  dove xm = media di X, ym = media di Y

Formula computazionale:

  r = (n*sum(x*y) - sum(x)*sum(y)) / sqrt((n*sum(x^2) - (sum(x))^2) * (n*sum(y^2) - (sum(y))^2))

1) Calcolare le medie delle due liste
2) Sottrarre la media a ogni valore (centratura)
3) Moltiplicare i valori corrispondenti
4) Sommare tutto (numeratore)
5) Calcolare le somme dei quadrati per ciascuna lista
6) Dividere per il prodotto delle radici

Casi particolari:
- liste di lunghezza diversa -> errore
- n < 2 -> correlazione non definita
- varianza nulla (tutti i valori uguali in X o Y) -> denominatore = 0

(define (pearson x y)
  ; calcolo lunghezza delle liste
  (letn (n (length x))
    (cond
      ; caso 1: liste di lunghezza diversa -> non definito
      ((!= n (length y)) nil)
      ; caso 2: meno di 2 elementi -> non definito
      ((< n 2) nil)
      ; caso normale:
      (true
        ; calcolo delle somme necessarie
        (letn ( ; somma elementi di x
                (sx (apply add x))
                ; somma elementi di y
                (sy (apply add y))
                ; somma dei prodotti x_i * y_i
                (sxy (apply add (map mul x y)))
                ; somma dei quadrati di x
                (sx2 (apply add (map (fn (v) (mul v v)) x)))
                ; somma dei quadrati di y
                (sy2 (apply add (map (fn (v) (mul v v)) y)))
                ; numeratore della formula di Pearson
                (num (sub (mul n sxy) (mul sx sy)))
                ; termine varianza (non normalizzata) di x
                (denx (sub (mul n sx2) (mul sx sx)))
                ; termine varianza (non normalizzata) di y
                (deny (sub (mul n sy2) (mul sy sy)))
              )
          ; controllo dei casi degeneri: varianza nulla
          (if (or (= denx 0) (= deny 0))
              nil
              ; valore finale: divisione per il prodotto delle radici
              (div num (sqrt (mul denx deny)))))))))

Proviamo:

(setq a '(1 2 3 4 5))
(setq b '(2 4 6 8 10))
(setq c '(10 8 6 4 2))
(setq d '(1 5 2 4 3))
(pearson a b)
;-> 1
(pearson a c)
;-> -1
(pearson b c)
;-> -1
(pearson a d)
;-> 0.3
(pearson b d)
;-> 0.3
(pearson c d)
;-> -0.3


----------------------------------
Indice di correlazione di Spearman
----------------------------------

L'indice di correlazione di Spearman (rs) misura la relazione monotona tra due liste, usando il metodo di Pearson con i ranghi invece dei valori originali.
È più robusto di Pearson quando i dati non sono lineari o hanno outlier.

Formula:
Se non ci sono valori uguali:
  
  rs = 1 - (6 * sum(d^2)) / (n * (n^2 - 1))

dove:

  d(i) = rank(x(i)) - rank(y(i))

Metodo di calcolo:
1. Trasformare le due liste nei rispettivi ranghi
2. Calcolare le differenze d(i)
3. Applicare la formula sopra

Se ci sono valori uguali:
- bisogna assegnare il rango medio (average rank)
- oppure usare direttamente Pearson sui ranghi (metodo generale)

Casi particolari (non considerati):
- lunghezze diverse -> nil
- n < 2 -> nil
- tutti i ranghi uguali -> nil

Funzione generale (funziona anche con valori duplicati e usa Pearson sui ranghi)

; Funzione che calcola i ranghi con gestione dei pari valori (media dei ranghi)
(define (rank lst)
  (letn ((sorted (sort (copy lst)))
         (ranks '()))
    (dolist (x lst)
      ;(letn ((pos (first (ref x sorted)))
      (letn ((pos (find x sorted))
             (counts (length (filter (fn (v) (= v x)) sorted)))
             ; rango medio: posizione + (count-1)/2 + 1 (per indice base 1)
             (r (add pos (div (sub counts 1) 2) 1)))
        (push r ranks -1)))
    ranks))

; Spearman = Pearson applicato ai ranghi
(define (spearman x y)
  (letn (rx (rank x)
         ry (rank y))
    (pearson rx ry)))

Proviamo:

(setq a '(1 2 3 4 5))
(setq b '(2 4 6 8 10))
(setq c '(10 8 6 4 2))
(setq d '(1 5 2 4 3))
(spearman a b)
;-> 1
(spearman a c)
;-> -1
(spearman b c)
;-> -1
(spearman a d)
;-> 0.3
(spearman b d)
;-> 0.3
(spearman c d)
;-> -0.3


--------------------
Sequenze di Sturmian
--------------------

Le sequenze di Sturmian sono sequenze binarie infinite (su {0,1}) che stanno esattamente 'al confine' tra ordine e complessità.
Una sequenza binaria infinita è sturmiana se ha complessità minima non banale:

Il numero di sottostringhe distinte di lunghezza n è:
  p(n) = n + 1
Questo è il minimo possibile per una sequenza non periodica.

Costruzione delle sequenze

Le sequenze di Sturmian si ottengono da una 'rotazione su un cerchio' (costruzione geometrica).

Poniamo:
 alpha in (0,1) irrazionale (pendenza)
 rho in [0,1) (fase iniziale)

Definiamo:

 s(n) = floor((n+1)*alpha + rho) - floor((n*alpha) + rho)

Questo produce una sequenza di 0 e 1.

Interpretazione:
Stiamo seguendo la retta y = alpha*x + rho
Ogni volta che attraversiamo un intero -> mettiamo 1, altrimenti 0

Proprietà
1. Bilanciamento
Per ogni due sottostringhe della stessa lunghezza, il numero di 1 differisce al massimo di 1
Questa proprietà indica che le sequenze sono uniformemente distribuite.
2. Non periodicità
Non sono periodiche, ma sono 'quasi periodiche' (struttura altamente regolare)
3. Codifica di rotazioni
Le sequenze di Sturmian sono equivalenti a orbite di rotazioni irrazionali su S^1.
Questo le collega alla teoria ergodica e ai sistemi dinamici.
4. Parole caratteristiche
Ogni pendenza alpha ha una sequenza speciale detta 'parola caratteristica', che è la più 'canonica' per quella pendenza.

Un altro modo di vederle:
- Tracciamo una retta con pendenza irrazionale su una griglia
- Ogni volta che attraversiamo:
  a) una linea verticale -> 0
  b) una linea orizzontale -> 1
In questo modo ottieniamo una sequenza sturmiana

Altra definizione:
Una sequenza è sturmiana se e solo se:
- ha complessità (n+1)
- è bilanciata e non periodica
- è codifica di una rotazione irrazionale
- è limite di parole ottenute da frazioni continue

Scriviamo una funzione che genera i primi 'n' termini della sequenza.
alpha = pendenza (idealmente irrazionale)
rho = offset iniziale
Ogni passo verifica se la retta y = alpha*x+rho supera un intero
se sì -> 1
altrimenti -> 0

(define (sturmian-line n alpha rho)
  ; genera i primi n termini di una sequenza sturmiana
  ; usando la definizione con le parti intere:
  ; s(k) = floor((k+1)*alpha + rho) - floor(k*alpha + rho)
  (letn (res '() a0 0 a1 0)
    ; res = lista risultato
    ; a0, a1 = valori delle parti intere successive
    (for (k 0 (- n 1))
      ; calcola floor(k*alpha + rho)
      (setq a0 (floor (add (mul k alpha) rho)))
      ; calcola floor((k+1)*alpha + rho)
      (setq a1 (floor (add (mul (add k 1) alpha) rho)))
      ; differenza tra i due valori:
      ; vale 1 se si attraversa un intero, altrimenti 0
      (push (sub a1 a0) res -1))
    ; restituisce la sequenza binaria
    res))

(sturmian-line 20 0.6180339887 0)
;-> (0 1 0 1 1 0 1 0 1 1 0 1 1 0 1 0 1 1 0 1)

Nota che 'alpha' deve essere irrazionale per avere una vera sequenza sturmiana.
Con valori razionali otteniamo una sequenza periodica.
La versione 'sturmian-line' può introdurre errori numerici.
Per sequenze lunghe è meglio la seguente versione con (p q) interi.

(define (sturmian-rat n p q)
  ; genera i primi n termini della sequenza sturmiana
  ; usando alpha = p/q (approssimazione razionale)
  (letn (res '() x 0)
    ; x rappresenta la posizione accumulata (mod q)
    (for (k 0 (- n 1))
      ; aggiunge p (simula x <- x + alpha)
      (setq x (add x p))
      ; se si supera q, si "attraversa" un intero -> output 1
      (if (>= x q)
          (begin
            (push 1 res -1) ; aggiunge 1 alla sequenza
            (setq x (sub x q))) ; riduce x modulo q
          ; altrimenti non si attraversa -> output 0
          (push 0 res -1)))
    res))

(sturmian-rat 20 987 1597)
;-> (0 1 0 1 1 0 1 0 1 1 0 1 1 0 1 0 1 1 0 1)

Adesso scriviamo una funzione per verificare se una data parola è 'sturmina'.
Nota: una stringa finita non può essere 'sturmiana' in senso stretto (la definizione è per sequenze infinite).
Però si usa un criterio equivalente: una parola finita è 'sturmiana' se e solo se è 'bilanciata'.
Una parola è 'bilanciata' se: per ogni lunghezza k, e per ogni due sottostringhe di lunghezza k, il numero di '1' differisce al massimo di 1.

Algoritmo
1) Per ogni lunghezza ( k = 1..n )
2) scorrere tutte le sottostringhe di lunghezza ( k )
3) calcolare il numero di '1'
4) controllare: max - min <= 1

Funzione per verificare se una sequenza finita è sturmiana

(define (sturmian? lst)
  ; verifica se una lista binaria è sturmiana (cioè bilanciata)
  (letn (n (length lst) ok true)
    ; per ogni lunghezza k delle sottostringhe
    (for (k 1 n)
      (letn (min1 nil max1 nil cnt 0)
        ; scorri tutte le sottostringhe di lunghezza k
        (for (i 0 (- n k))
          ; conta il numero di 1 nella sottostringa corrente
          (setq cnt (apply add (slice lst i k)))
          ; aggiorna minimo
          (if (or (= min1 nil) (< cnt min1))
              (setq min1 cnt))
          ; aggiorna massimo
          (if (or (= max1 nil) (> cnt max1))
              (setq max1 cnt)))
        ; se la differenza supera 1 → non sturmiana
        (if (> (sub max1 min1) 1)
            (setq ok nil))))
    ok))

(sturmian? (sturmian-line 20 0.6180339887 0))
;-> true
(sturmian? (sturmian-rat 100 987 1597))
;-> true
(sturmian? '(0 1 1 0 1 1 1 1 1 1))
;-> nil

Vediamo una parola 'sturmiana' particolare: Fibonacci-word.

Sequenza OEIS A003849:
The infinite Fibonacci word (start with 0, apply 0->01, 1->0, take limit).
223
  0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0,
  1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0,
  0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1,
  0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, ...

(define (fibonacci-word n)
  (letn (a '(0) b '(0 1) tmp)
    (while (< (length b) n)
      (setq tmp (append b a))
      (setq a b)
      (setq b tmp))
    (slice b 0 n)))

(fibonacci-word 50)
;-> (0 1 0 0 1 0 1 0 0 1 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0
;->  1 0 1 0 0 1 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0 1 0 1 0)


--------------------------
La funzione matematica EML
--------------------------

"All elementary functions from a single binary operator" Andrzej Odrzywołek
https://arxiv.org/abs/2603.21852

A single two-input gate suffices for all of Boolean logic in digital hardware.
No comparable primitive has been known for continuous mathematics: computing elementary functions such as sin, cos, sqrt, and log has always required multiple distinct operations.
Here I show that a single binary operator, eml(x,y)=exp(x)-ln(y), together with the constant 1, generates the standard repertoire of a scientific calculator.
This includes constants such as e, pi, and 'i' and arithmetic operations including addition, subtraction, multiplication, division, and exponentiation as well as the usual transcendental and algebraic functions.
For example, exp(x)=eml(x,1), ln(x)=eml(1,eml(eml(1,x),1)), and likewise for all other operations.
That such an operator exists was not anticipated.
I found it by systematic exhaustive search and established constructively that it suffices for the concrete scientific-calculator basis.
In EML (Exp-Minus-Log) form, every such expression becomes a binary tree of identical nodes, yielding a grammar as simple as S -> 1 | eml(S,S).
This uniform structure also enables gradient-based symbolic regression: using EML trees as trainable circuits with standard optimizers (Adam), I demonstrate the feasibility of exact recovery of closed-form elementary functions from numerical data at shallow tree depths up to 4.
The same architecture can fit arbitrary data, but when the generating law is elementary, it may recover the exact formula.

"All elementary functions from a single binary operator" Andrzej Odrzywołek
https://arxiv.org/abs/2603.21852

In poche parole con la formula eml(x,y)=exp(x)-ln(y), insieme alla costante 1, è possibile generare il repertorio standard di una calcolatrice scientifica.

Formula: eml(x,y = exp(x) - ln(x)

(define (eml x y) (sub (exp x) (log y)))

Nota:
(log 0)
;-> -1.#INF
(exp (log 0))
;-> 0

Operazioni equivalenti:

Costante di Eulero
(define (_e) (eml 1 1))
(_e)

Esponenziale:
(define (_exp x) (eml x 1))
(_exp)

Zero:
(define (_zero) (eml 1 (_exp (_exp 1))))
(_zero)

Infinito:
(define (_inf) (eml 1 0))
(_inf)
;-> 1.#INF

Infinito negativo:
(define (_neg-inf) (eml 1 (_inf)))
(_neg-inf)
;-> -1.#INF

Negazione:
(define (_neg x) (eml (_neg-inf) (_exp x)))
(_neg 2)
;-> -2

Logaritmo naturale:
(define (_log x) (eml 1 (_exp (eml 1 x))))
(_log 10)
;-> 2.302585092994046

Inversione:
(define (_inv x) (_exp (eml (_neg-inf) x)))
(_inv 2)
;-> 0.5

Sottrazione:
(define (_sub x y) (eml (_log x) (_exp y)))
(_sub 10.3 1.1)
;-> 9.199999999999999

Addizione:
(define (_add x y) (eml 1 (_exp (eml (_log (eml 1 (_exp x))) (_exp y)))))
(_add 2 2)
;-> 4
(_add 3 4)
;-> 1.#QNAN
La formula è formalmente corretta, ma instabile numericamente.

(_add 10 2)
Moltiplicazione:
(define (_mul x y) (_exp (eml 1 (_exp (eml (_log (eml 1 x)) y)))))
(_mul 3.5 4)
;-> 14

Divisione:
(define (_div x y) (_exp(eml (_log (_log x)) y)))
(_div 14 4)
;-> 3.500000000000001

Potenza:
(define (_pow x y) (_exp (_mul (_log x) y)))
(_pow 10 2)
;-> 100.0000000000001

Costante 2:
(define (_two) (_add 1 1))
(_two)
;-> 2

Costante immaginaria:
(define (_i) (_exp (_div (_log (_neg 1)) (_two))))
(_i)
;-> 1.#QNAN

Radice quadrata:
(define (_sqrt x) (_pow x (_inv (_two))))
(_sqrt 16)
;-> 4

Pi greco: (sqrt(- (log(-1))^2))
(define (_pi) (_sqrt (_neg (_pow (_log (_neg 1)) (two)))))
(_pi)
;-> 1.#QNAN ; otteniamo NaN, ma formalmente è corretto

Radice n-esima:
(define (_root x n) (_pow x (_inv n)))

Valore assoluto: abs(x) = sqrt(x^2)
(define (_abs x) (_sqrt (_mul x x)))

Segno: sign(x) = x/|x|
(define (_sign x) (_div x (_abs x)))

Logaritmo in base arbitraria: log-b(x) = log(x)/log(b)
(define (_logb x b) (_div (_log x) (_log b)))

Funzione sigmoid: s = 1/(1 + e^(-x))
(define (_sigmoid x) (_inv (_add 1 (_exp (_neg x)))))

Min e Max (senza branching):
min(x, y) = (x + y - abs(x - y))/2
(define (_min x y) (_div (_sub (_add x y) (_abs (_sub x y))) (_two)))
max(x, y) = (x + y + abs(x - y))/2
(define (_max x y) (_div (_add (_add x y) (_abs (_sub x y))) (_two)))

La formula EML mescola una funzione moltiplicativa (exp) e una inversa (log), quindi  è una sorta di 'ponte' tra struttura additiva e moltiplicativa.
Questo è probabilmente il motivo per cui riesce a generare tutto.
Comunque anche se EML è funzionalmente completo non tutte le rappresentazioni sono numericamente equivalenti.

---------------------
Lichess Time controls
---------------------

Il controllo del tempo di Lichess si basa sulla durata stimata della partita = (tempo iniziale dell'orologio in secondi) + 40*(incremento dell'orologio).
Ad esempio, la durata stimata di una partita 5+3 è 5*60 + 40*3 = 420 secondi.

(define (durata t plus) (add t (mul 40 plus)))
t = tempo iniziale (secondi)
plus = tempo aggiunto per ogni mossa (secondi)

3m+3s
(durata 180 3)
;-> 300 (5 minuti)
5m+3s
(durata 300 3)
;-> 420 (7 minuti)


--------------------------
Swap letter and digit runs
--------------------------

https://codegolf.stackexchange.com/questions/129840/swap-letter-and-digit-runs
Nota:
Tutto il contenuto dei siti di Stack Exchange è rilasciato sotto la licenza CC BY-SA 4.0 (Creative Commons Attribution-ShareAlike 4.0).

Given an input string containing only alphanumeric ASCII characters and starting with a letter, swap each letter run with the digit run which follows.
A run is a sequence of consecutive letters or digits.
Note that in the case where the input string ends with a run of letters, this run is left untouched.

Example:
  input string = uV5Pt3I0
  Separate runs of letters and runs of digits: uV 5 Pt 3 I 0
  Identify pairs of runs: (uV 5) (Pt 3) (I 0)
  Swap pairs of runs: (5 uV) (3 Pt) (0 I)
  Concatenate: 5uV3Pt0I

Examples:
  uV5Pt3I0 -> 5uV3Pt0I
  J0i0m8 -> 0J0i8m
  abc256 -> 256abc
  Hennebont56Fr -> 56HennebontFr
  Em5sA55Ve777Rien -> 5Em55sA777VeRien
  nOoP -> nOoP

(define (digit? c) (and (>= c "0") (<= c "9")))

Metodo
'tmp' accumula le lettere (o non-cifre)
Quando arrivano le cifre: emettere prima le cifre poi tmp.
Alla fine, se restano non-cifre -> append finale

(define (swap-digits s)
  (letn (res '() tmp '() i 0 n (length s))
    ; res = risultato finale (lista di caratteri)
    ; tmp = buffer temporaneo per blocco di non-cifre
    (while (< i n)
      (if (digit? (s i))
          ; caso: blocco di cifre
          (let (j i)
            (while (and (< j n) (digit? (s j))) (inc j))
            ; prima aggiungi le cifre
            (let (k i)
              (while (< k j)
                (push (s k) res -1)
                (++ k)))
            ; poi eventuale blocco precedente di non-cifre
            (if tmp (extend res tmp))
            (set 'tmp '())   ; svuota buffer non-cifre
            (setq i j))
          ; caso: non-cifra → accumula in tmp
          (begin
            (push (s i) tmp -1)
            (++ i))))
    ; se la stringa finisce con non-cifre, aggiungile
    (if tmp (extend res tmp))
    (join res)))

Proviamo:

(swap-digits "uV5Pt3I0")
;-> 5uV3Pt0I
(swap-digits "J0i0m8")
;-> 0J0i8m
(swap-digits "abc256")
;-> 256abc
(swap-digits "Hennebont56Fr")
;-> 56HennebontFr
(swap-digits "Em5sA55Ve777Rien")
;-> 5Em55sA777VeRien
(swap-digits "nOoP")
;-> nOoP

Adesso vediamo una versione che utilizza le espressioni regolari.

(define (swap-digits2 s) (replace {(\D+)(\d+)} s (append $2 $1) 0))

(swap-digits2 "uV5Pt3I0")
;-> 5uV3Pt0I
(swap-digits2 "J0i0m8")
;-> 0J0i8m
(swap-digits2 "abc256")
;-> 256abc
(swap-digits2 "Hennebont56Fr")
;-> 56HennebontFr
(swap-digits2 "Em5sA55Ve777Rien")
;-> 5Em55sA777VeRien
(swap-digits2 "nOoP")
;-> nOoP

'replace', quando usiamo una funzione o un'espressione, rende disponibili i gruppi catturati come variabili '$0', '$1', '$2', ...
'(append $2 $1)' costruisce la nuova stringa concatenando:
- prima le cifre '$2'
- poi i caratteri non numerici '$1'

Questo è un punto in cui newLISP è più LISP-like di JavaScript:
non usa una sostituzione testuale, ma una valutazione dinamica, quindi possiamo fare cose più complesse dei semplici '$1', '$2'.

Ad esempio anche trasformare i gruppi:

(define (swap-upper s)
  (replace {(\D+)(\d+)} s (append (upper-case $2) $1) 0))
  
Qui modifichiamo direttamente il contenuto durante la sostituzione.

Test di velocità

(define (rand-str n) (join (slice (randomize 
              (map char (flat (map sequence '(48 65 97) '(57 90 122))))) 0 n)))

(setq nums (map string (rand 10000 300)))
(setq letters (collect (rand-str (+ (rand 4) 1)) 300))
(setq test (join (flat (map list nums letters))))

(= (swap-digits test) (swap-digits2 test))
;-> true

(time (swap-digits test) 100)
;-> 2702.742
(time (swap-digits2 test) 100)
;-> 19.68

Infine la versione più corta (51 caratteri):

(define(f s)(replace {(\D+)(\d+)}s(append $2 $1)0))
(f "Em5sA55Ve777Rien")
;-> 5Em55sA777VeRien


---------------------------
Numeri random con blacklist
---------------------------

Dato un numero N e una lista con numeri minori di N (blacklist), scrivere una funzione che sceglie a caso un numero tra 0 e N-1 in modo uniforme.

Rejection sampling
------------------
Generiamo un numero casuale tra 0 e N-1.
Se il numero si trova nella blacklist, allora lo scartiamo, altrimenti è un risultato valido.
Tutti i valori nella blacklist sono unici.

(define (rand1 n lst)
  (let (num (rand n))
    (while (member num lst) (setq num (rand n)))
    num))

(collect (rand1 10 '(1 2 3 4 5)) 10)
;-> (6 0 6 7 8 8 7 9 9 8)

Questo metodo è inefficiente se la blacklist è grande.
Se k = length(lst), la probabilità di accettazione è:

  p = (n - k) / n

Numero medio di tentativi:

  E = 1 / p = n / (n - k)

Quindi quando k è vicino a n, diventa molto lento.

Valid List creation
-------------------
Creiamo una lista che contiene solo i numeri validi (difference).
Selezioniamo in modo random un indice dalla lista creata e restituiamo il valore associato.

(define (rand2 n lst)
  (let (lista (difference (sequence 0 (- n 1)) lst))
    (lista (rand (length lista)))))

(collect (rand2 10 '(1 2 3 4 5)) 10)
;-> (6 9 0 6 0 8 0 0 9 6)

Il tempo di costruzione della lista è O(n) con memoria: O(n) e non va bene se n è molto grande.

Remapping strategy
------------------
In questo caso non costruiamo tutta la lista, ma solo una mappa per "riparare" i valori vietati.
Sia m = n - k (numero di valori validi)
Generiamo x uniforme in [0, m-1]
Se x è nella blacklist -> lo rimappiamo a un valore valido ≥ m

; Costruisce la mappa di remapping per evitare i valori in blacklist
; n   = limite superiore (numeri da 0 a n-1)
; lst = blacklist (valori unici < n)
; Metodo:
; - m = numero di valori validi = n - |lst|
; - genereremo numeri solo in [0, m-1]
; - se un valore in [0, m-1] è nella blacklist,
;   lo rimappiamo a un valore valido nella parte alta [m, n-1]
(define (build-map n lst)
  (letn ( ; numero di valori validi
          (m (- n (length lst)))
          ; blacklist ordinata (serve per controlli sequenziali)
          (bad (sort lst))
          ; lista associativa (chiave -> valore rimappato)
          (mp '())
          ; puntatore all'ultimo valore disponibile (parte alta)
          (ultimo (- n 1)) )
    ; scorriamo tutti i valori della blacklist
    (dolist (b bad)
      ; consideriamo solo quelli nella parte bassa [0, m-1]
      (if (< b m)
          (begin
            ; troviamo un valore valido nella parte alta
            ; (saltiamo quelli già nella blacklist)
            (while (ref ultimo bad) (-- ultimo))
            ; creiamo il mapping b -> ultimo
            (push (list b ultimo) mp -1)
            ; passiamo al prossimo candidato alto
            (-- ultimo)
          )
      )
    )
    mp))

; Estrae un numero casuale uniforme tra i valori NON in blacklist
; Metodo:
; - generiamo x in [0, m-1]
; - se x è blacklistato, lo rimappiamo usando mp
; - altrimenti x è già valido
(define (rand3 n lst)
  (letn ( ; numero di valori validi
          (m (- n (length lst)))
          ; mappa di remapping
          ; 'mp' contiene solo i mapping necessari -> dimensione O(k)
          (mp (build-map n lst))
          ; numero casuale nella parte bassa
          (x (rand m))
          ; cerchiamo se x va rimappato
          (r (assoc x mp)) )
    ; se esiste mapping restituiamo il valore associato
    ; altrimenti restituiamo x direttamente
    (if r (r 1) x)))

(collect (rand3 10 '(1 2 3 4 5)) 10)
;-> (8 0 9 6 0 8 7 8 0 6)

Test di correttezza

(define (test f n lst iter)
  (let ( (valid (difference (sequence 0 (- n 1)) lst))
         (nums (collect (f n lst) iter)) )
    (count valid nums)))

(test rand1 20 '(1 2 3 4 5) 1e5)
;-> (6754 6769 6749 6592 6694 6600 6718 6765 6739 6654 6638 6542 6740 6480 6566)
(test rand2 20 '(1 2 3 4 5) 1e5)
;-> (6514 6585 6493 6660 6601 6874 6692 6561 6747 6731 6670 6697 6720 6714 6741)
(test rand3 20 '(1 2 3 4 5) 1e5)
;-> (6762 6767 6528 6797 6639 6622 6775 6565 6604 6586 6669 6715 6544 6700 6727)

Test di velocità (i risultati reali sono un pò diversi dalla teoria)

(define (speed f n lst iter) (time (collect (f n lst) iter)))

(speed rand1 10 '(1 2 3 4 5 6 7 8) 1e6)
;-> 703.142
(speed rand2 10 '(1 2 3 4 5 6 7 8) 1e6)
;-> 1158.897
(speed rand3 10 '(1 2 3 4 5 6 7 8) 1e6)
;-> 1363.882

(speed rand1 20 '(1 2 3 4 5) 1e6)
;-> 250.537
(speed rand2 20 '(1 2 3 4 5) 1e6)
;-> 1892.05
(speed rand3 20 '(1 2 3 4 5) 1e6)
;-> 1766.539

(speed rand1 1000 (sequence 0 999 2) 1e4)
;-> 93.245
(speed rand2 1000 (sequence 0 999 2) 1e4)
;-> 1248.688
(speed rand3 1000 (sequence 0 999 2) 1e4)
;-> 17015.836

Il benchmark misura:
  
  funzione completa = build + sampling

ma rand3 è pensato per:

build una volta + sampling molte volte

Scriviamo una funzione ad hoc per misurare la velocità di 'rand3':

(define (speed3 n lst iter)
  (let (mp (build-map n lst))
    (time (collect
            (let (x (rand n))
              (let (r (assoc x mp))
                (if r (r 1) x)))
          iter))))

Proviamo:

(speed3 20 '(1 2 3 4 5) 1e6)
;-> 141.141
(speed3 10 '(1 2 3 4 5 6 7 8) 1e6)
;-> 140.631
(speed3 1000 (sequence 0 999 2) 1e4)
;-> 15.65


------------------------------------------------------------------------
Ordinare una lista con distanza assoluta unitaria tra elementi adiacenti
------------------------------------------------------------------------

Data una lista non ordinata di numeri interi, ordinarla in modo tale che la differenza assoluta tra due elementi adiacenti qualsiasi sia sempre uguale a 1.
Se non è possibile, restituire nil.
La lista data ha almeno due elementi.
Esempio: (3 0 1 1 2 4) -> (1 0 1 2 3 4) o (4 3 2 1 0 1)

Rappresentiamo la lista come un grafo lineare con multiset di nodi:
- ogni valore è un nodo
- ogni occorrenza è una 'copia' del nodo
- possiamo muoverci solo a:
  a) x - 1
  b) x + 1

Trovare un cammino che:
- parte da un qualsiasi nodo
- usa tutte le occorrenze
- non viola mai la disponibilità dei nodi

Stiamo cercando un cammino che visita ogni copia esattamente una volta, rispettando solo transizioni +-1 tra valori.
Questo è equivalente a:
Hamiltonian path su grafo con multiset di nodi compressi
oppure più correttamente:
Euler-like trail su struttura di adiacenza lineare con vincoli di capacità

Usiamo:
- backtracking iterativo (stack esplicito)
- stato = (posizione corrente, frequenze residue, path)
- esplorazione di entrambe le direzioni +-1
- terminazione garantita (spazio finito)

La funzione:
- costruisce le frequenze dei numeri
- prova ogni possibile punto di partenza
- esplora tutte le sequenze valide con differenza ±1
- usa backtracking iterativo (stack)
- restituisce la prima soluzione completa trovata
- oppure 'nil' se impossibile

(define (ordina-diff1 lst)
(catch
  (letn (freq '() n (length lst))
    ; 1. COSTRUZIONE FREQUENZE
    ; freq è una lista associativa del tipo:
    ; ((valore1 occorrenze1) (valore2 occorrenze2) ...)
    ; Serve per sapere quante volte ogni numero è disponibile.
    (dolist (x lst)
      (if (lookup x freq)
          (setf (lookup x freq) (+ $it 1))   ; incremento conteggio
          (push (list x 1) freq -1)))       ; prima occorrenza
    ; 2. SCELTA DEI POSSIBILI PUNTI DI PARTENZA
    ; Proviamo a partire da ogni valore distinto presente.
    ; Questo evita di perdere soluzioni dipendenti dal punto iniziale.
    (setq start-vals (map first freq))
    ; 3. BACKTRACKING ITERATIVO (STACK)
    ; Ogni stato nello stack è una tripla:
    ; (valore_corrente frequenze_residue percorso_costruito)
    (dolist (start start-vals)
      (letn (stack (list (list start (copy freq) (list start))))
        (while stack
          ; Estrazione ultimo stato (DFS iterativo)
          (letn (top (pop stack))
            (letn (cur (top 0)
                   f   (top 1)
                   path (top 2))
              ; 4. CASO FINALE: percorso completo
              ; Se abbiamo usato tutti gli elementi, abbiamo
              ; trovato una soluzione valida.
              (if (= (length path) n)
                  (throw path)
                  ; 5. ESPANSIONE DELLO STATO
                  ; Da ogni nodo possiamo muoverci solo a:
                  ;   cur + 1 oppure cur - 1
                  ; purché il valore sia ancora disponibile.
                  (begin
                    ; RAMO +1
                    (letn (nx (+ cur 1) v (lookup nx f))
                      (if (and v (> v 0))
                        (push (list nx
                                    ; copia delle frequenze
                                    ; per evitare contaminazione tra rami
                                    (letn (nf (copy f))
                                      (setf (lookup nx nf) (- v 1))
                                      nf)
                                    (append path (list nx)))
                              stack)))
                    ; RAMO -1
                    (letn (nx (- cur 1) v (lookup nx f))
                      (if (and v (> v 0))
                        (push (list nx
                                    (letn (nf (copy f))
                                      (setf (lookup nx nf) (- v 1))
                                      nf)
                                    (append path (list nx)))
                              stack))))))))))
    ; 6. SE NON ESISTE SOLUZIONE
    nil)))

Proviamo:

(ordina-diff1 '(3 0 1 1 2 4))
;-> (3 4 3 2 1 0)
(ordina-diff1 '(2 2 3 3 4 4))
;-> (2 3 4 3 2 3)
(ordina-diff1 '(1 3 5))
;-> nil
(ordina-diff1 '(1 7 5 2 6 4 4 3 3))
;-> (1 2 3 4 3 4 5 6 7)
(ordina-diff1 '(6 10 2 12 13 5 7 1 14 9 2 1 4 10 13 11 11 12 8 3 9))
;-> (10 9 10 11 12 13 14 13 12 11 10 9 8 7 6 5 4 3 2 1 2)
(ordina-diff1 '(1 3 5 4 2 8 7 9))
;-> nil

Adesso scriviamo una funzione che verifica la 'fattibilità', cioè se la lista data può essere ordinata oppure no.
Il test di fattibilità è basato sulle due condizioni necessarie e sufficienti:
1) nessun 'buco' nell'intervallo: per ogni k in [min, max], --> f(k) > 0
L'intervallo deve essere intero continuo (senza buchi).
2) vincolo locale: per ogni nodo interno di k deve valre: f(k) ≤ f(k-1) + f(k+1) + 1
Ogni 'punto' può essere attraversato entrando da sinistra e destra eventualmente rimanendo 'centrale'.
Quindi i vicini devono avere abbastanza 'capacità' per sostenere tutte le visite di k.

(define (fattibile? lst)
  (letn (freq '() keys '() ok true)
    ; 1. COSTRUZIONE FREQUENZE
    (dolist (x lst)
      (if (lookup x freq)
          (setf (lookup x freq) (+ $it 1))
          (push (list x 1) freq -1)))
    ; 2. CHIAVI ORDINATE
    (setq keys (sort (map first freq)))
    ; 3. CONTROLLO "BUCHI" (continuità intervallo)
    (for (i 0 (- (length keys) 2))
      (if (and ok
               (!= (+ (keys i) 1) (keys (+ i 1))))
          (setq ok nil)))
    ; 4. CONTROLLO VINCOLO LOCALE
    ;    f(k) ≤ f(k-1) + f(k+1) + 1
    (if ok
      (dolist (k keys)
        (letn (fk (lookup k freq)
               fL (lookup (- k 1) freq)
               fR (lookup (+ k 1) freq))
          (setq fL (if fL fL 0))
          (setq fR (if fR fR 0))
          (if (> fk (+ fL fR 1))
              (setq ok nil)))))
    ok))

Proviamo:

(fattibile? '(3 0 1 1 2 4))
;-> true
(fattibile? '(2 2 3 3 4 4))
;-> true
(fattibile? '(1 3 5))
;-> nil
(fattibile? '(1 7 5 2 6 4 4 3 3))
;-> true
(fattibile? '(6 10 2 12 13 5 7 1 14 9 2 1 4 10 13 11 11 12 8 3 9))
;-> true
(fattibile? '(1 3 5 4 2 8 7 9))
;-> nil


-------------------------------------------------
Somme distinte della fattorizzazione di un intero
-------------------------------------------------

Dato un intero N:
a) calcolare tutte le possibili fattorizzazioni
b) per ogni fattorizzazione sommare i relativi numeri
c) determinare le somme con valore unico
d) restituire il numero delle somme uniche

Esempi
  Le fattorizzazioni di 12 sono (2,2,3), (2,6), (3,4) e (12).
  Le somme distinte sono: 7, 8 e 12.
  Quindi a(12) = 3.

  Le fattorizzazioni di 30 sono (2,3,5), (2,15), (3,10), (5,6) e (30).
  Le 5 somme distinte sono: 10, 17, 13, 11 e 30.
  Quindi a(30) = 5.

Sequenza OEIS A069016:
Look at all the different ways to factorize n as a product of numbers bigger than 1, and for each factorization write down the sum of the factors: a(n) = number of different sums.
  1, 1, 1, 1, 1, 2, 1, 2, 2, 2, 1, 3, 1, 2, 2, 3, 1, 4, 1, 3, 2, 2, 1, 5,
  2, 2, 3, 3, 1, 5, 1, 4, 2, 2, 2, 7, 1, 2, 2, 5, 1, 5, 1, 3, 4, 2, 1, 8,
  2, 4, 2, 3, 1, 7, 2, 5, 2, 2, 1, 9, 1, 2, 4, 6, 2, 5, 1, 3, 2, 5, 1, 10,
  1, 2, 4, 3, 2, 5, 1, 8, 5, 2, 1, 8, 2, 2, 2, 5, 1, 10, 2, 3, 2, 2, 2, 12,
  1, 4, 4, 7, 1, 5, 1, ...

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
                  (factorizations-aux (/ num i) (append parfac (list i)) newval)
              ))
      )
      (-- i)
    )
    (sort (unique (map sort afc)))))

(setq n 24)
(setq fac (list (list n)))
;-> ((24))
(extend fac (factorizations n))
;-> ((24) (2 2 2 3) (2 2 6) (2 3 4) (2 12) (3 8) (4 6))
(setq sums (map (fn(x) (apply + x)) fac))
;-> (24 9 10 9 14 11 10)
(setq unique-sums (unique sums))
;-> (24 9 10 14 11)
(setq num-sums (length unique-sums))
;-> 5

(define (distinct-sums n)
  (let (fac (list (list n)))
    (extend fac (factorizations n))
    (length (unique (map (fn(x) (apply + x)) fac)))))

(distinct-sums 24)
;-> 5

Calcoliamo i primi 72 termini della sequenza:

(map distinct-sums (sequence 1 72))
;-> (1 1 1 1 1 2 1 2 2 2 1 3 1 2 2 3 1 4 1 3 2 2 1 5
;->  2 2 3 3 1 5 1 4 2 2 2 7 1 2 2 5 1 5 1 3 4 2 1 8
;->  2 4 2 3 1 7 2 5 2 2 1 9 1 2 4 6 2 5 1 3 2 5 1 10)


----------------------------------
Mediana dalle occorrenze di valori
----------------------------------

Data una lista di coppie del tipo (valore occorrenze), determinare la mediana dei valori.

Esempio:
lista = ((1 3) (2 1) (3 4))
lista-valori = (1 1 1 2 3 3 3 3)
mediana = 2.5

La mediana di una lista finita di numeri è il numero che si trova "in mezzo ", quando i numeri sono ordinati dal più piccolo al più grande (ordine crescente).

            | x[(n+1)/2],                 se n è dispari
  Mediana = | 
            | (x[(n/2)] + x[(n/2)+1)])/2, se n è pari

(define (median lst)
"Calculate the median of a list of numbers"
  (let (len (length lst))
    (sort lst)
    (if (odd? len)
        (lst (/ len 2))
        (div (add (lst (- (/ len 2) 1)) (lst (/ len 2))) 2))))

(median '(1 1 1 2 3 3 3 3))
;-> 2.5

Metodo 1 (Espansione della lista)
---------------------------------
Ricreiamo la lista originale e poi calcoliamo la mediana.

(define (to-median lst)
  (let (L '())
    (dolist (el lst)
      (extend L (dup (el 0) (el 1))))
    (median L)))

(to-median '((1 3) (2 1) (3 4)))
;-> 2.5

Metodo2 (Calcolo diretto senza espansione)
------------------------------------------
Per trovare la mediana senza espandere la lista:
1. si calcola il numero totale di elementi (`tot`)
2. si individuano le posizioni centrali:
   - pos1 = (tot+1)/2
   - pos2 = pos1 se 'tot' è dispari, altrimenti 'pos1+1'
3. si scorre la lista accumulando le occorrenze ('cum')
4. appena 'cum' raggiunge 'pos1' si salva il primo valore ('res1')
5. appena 'cum' raggiunge 'pos2' si salva il secondo valore ('res2')
6. risultato:
   - se dispari -> res1
   - se pari -> (res1 + res2)/2
Questo metodo evita di costruire la lista completa ed è lineare nel numero di coppie.

(define (to-median2 lst)
  ; tot = numero totale di elementi
  ; pos1,pos2 = posizioni mediane
  ; cum = somma cumulativa delle occorrenze
  ; res1,res2 = valori mediani trovati
  (letn (tot 0 pos1 0 pos2 0 cum 0 res1 nil res2 nil)
    ; ordinamento della lista
    (sort lst)
    ; calcolo del totale delle occorrenze
    (dolist (el lst)
      (inc tot (el 1)))
    ; calcolo delle posizioni mediane
    (setq pos1 (/ (+ tot 1) 2))
    (setq pos2 (if (= (mod tot 2) 0) (+ pos1 1) pos1))
    ; scansione unica della lista
    (dolist (el lst)
      ; aggiorna cumulata
      (setq cum (add cum (el 1)))
      ; se non ancora trovato, verifica prima posizione
      (if (and (nil? res1) (>= cum pos1))
          (setq res1 (el 0)))
      ; se non ancora trovato, verifica seconda posizione
      (if (and (nil? res2) (>= cum pos2))
          (setq res2 (el 0))))
    ; restituzione risultato finale
    (if (= pos1 pos2)
        res1
        (div (add res1 res2) 2))))

(to-median2 '((1 3) (2 1) (3 4)))
;-> 2.5

(setq a '( (7 10) (8 11) (9 5) (1 4) (2 5) (3 8) (4 4) (5 20) (6 15)))
(to-median a)
;-> 5.5
(to-median2 a)
;-> 5.5

(setq b '( (7 10) (8 11) (9 5) (1 4) (2 5) (3 8) (4 4) (5 20) (6 14)))
(to-median b)
;-> 5
(to-median2 b)
;-> 5


-----------------
The Lost sequence
-----------------

La sequenza 4, 8, 15, 16, 23, 42 proviene dalla serie TV "Lost".
Questi numeri sono chiamati 'The Numbers' nella trama.
Compaiono in diversi contesti:
a) nella lotteria vinta da Hugo
b) nel codice della botola (la 'hatch')
c) associati ai personaggi principali

Sequenza OEIS A104101:
The Lost Numbers.
  4, 8, 15, 16, 23, 42

Scriviamo una funzione per generarli.

(define (lost n)
  (let (x 45487288836) ; numero codificato
    (& 63 (>> x (* 6 n)))))

(map lost (sequence 0 5))
;-> (4 8 15 16 23 42)

Come funziona?

Vediamo alcune funzioni per definire il 'metodo di codifica a blocchi di bit'.
Questo metodo crea una specie di 'array compresso in bit'.
- ogni numero occupa k bit
- tutti i numeri vengono 'impacchettati' in un intero
- accesso tramite:
  - shift -> seleziona blocco
  - mask -> estrae valore
è una specie di 'array compresso in bit'.
Il metodo funziona solo per interi maggiori o uguali a 0.

; Funzione 'bits-needed'
; calcola quanti bit servono per rappresentare il massimo
(define (bits-needed x)
  ; Calcola il numero minimo di bit necessari
  ; per rappresentare l'intero non negativo x
  (let (b 0)
    (while (> x 0)
      ; shift a destra di 1 bit (divide per 2)
      (setq x (>> x 1))
      ; conta quanti shift servono
      (++ b))
    ; caso speciale: x = 0 -> serve comunque 1 bit
    (if (= b 0) 1 b)))

  x = 42 -> 6 bit (101010)
  x = 0  -> 1 bit

; Funzione 'pack-list'
; Usa k bit per ogni numero e costruisce un unico intero 'acc'
(define (pack-list lst)
  ; Codifica una lista di interi in un unico numero
  ; usando blocchi di k bit per elemento
  (letn (maxv (apply max lst)   ; valore massimo della lista
         k (bits-needed maxv)   ; bit necessari per ogni elemento
         acc 0                  ; accumulatore finale (numero codificato)
         i 0)                   ; indice posizione elemento
    (dolist (v lst)
      ; inserisce v nel blocco i-esimo:
      ; shift a sinistra di (k * i) bit e OR con acc
      (setq acc (| acc (<< v (* k i))))
      (++ i))
    ; restituisce:
    ; acc = numero codificato
    ; k   = bit per elemento
    (list acc k)))

[ v0 | v1 | v2 | ... ]   (ognuno largo k bit)

; Funzione 'make-packed-func'
; Crea una funzione anonima che estrae l'elemento n
(define (make-packed-func lst)
  ; Costruisce una funzione che restituisce l'elemento n-esimo
  ; della lista codificata, senza usare la lista stessa
  (letn (pk (pack-list lst)
         x (pk 0)   ; numero codificato
         k (pk 1)   ; bit per elemento
         mask (- (<< 1 k) 1)) ; maschera: k bit a 1 (es. 6 bit -> 63)
    ; 'letex' sostituisce i valori direttamente nel codice della funzione
    ; evitando problemi di scope dinamico
    (letex (x x k k mask mask)
      (fn (n)
        ; Estrazione:
        ; 1. shift a destra di k*n -> porta il valore in fondo
        ; 2. AND con mask -> estrae solo i k bit
        (& mask (>> x (mul k n)))))))

Esempio:

(setq lost (make-packed-func '(4 8 15 16 23 42)))
;-> (lambda (n) (& 63 (>> 45487288836 (mul 6 n))))

(map lost (sequence 0 5))
;-> (4 8 15 16 23 42)


------------------------------------------------------
Precedente e prossima stringa in ordine lessicografico
------------------------------------------------------

Data una stringa, determinare la precedente e la prossima stringa in ordine lessicografico.

Prossima stringa (in ordine lessicografico)
-------------------------------------------
Algoritmo
Parti dall'ultimo carattere e spostati a sinistra finché tutti i caratteri sono uguali a 'z'.
Se tutti i caratteri sono 'z', aggiungi 'a' alla stringa originale e restituiscila.
Altrimenti, trova il carattere più a destra diverso da 'z' e incrementalo di uno.
Rimuovi tutti i caratteri a destra della posizione incrementata.
Restituisci la stringa finale aggiornata, che ora rappresenta la successiva stringa lessicografica.

(define (str-next str)
  (let (i (- (length str) 1))
    ; Muove a sinistra mentre i caratteri sono 'z'
    (while (and (>= i 0) (= (str i) "z"))
      (-- i))
    ; ora 'i' è l'indice del prossimo carattere della 'z' più a destra
    ;(println "i: " i)
    (if (= i -1)
          ; quando tutti i caratteri sono 'z', aggiunge il carattere 'a'
          (setq out (extend str "a"))
          ; altrimenti, incrementa il prossimo carattere della 'z' più a destra
          (setf (str i) (char (+ (char (str i)) 1))))
          ; e rimuove i caratteri a destra della posizione incrementata
          (setq str (slice str 0 (+ i 1)))
    str))

(str-next "pazzo")
;-> "pazzp"
(str-next "abczz")
;-> "abd"
(str-next "azz")
;-> "b"

Precedente stringa (in ordine lessicografico)
---------------------------------------------
Algoritmo
La funzione implementa una 'sottrazione in base 26' con alfabeto 'a..z':
- 'a' è lo 'zero'
- 'z' è il valore massimo
- quando trovi 'a', devi fare 'prestito' (come 0 -> 9 nei numeri decimali)
Per esempio, "ac"  -> "abz"
- parti da destra: 'c' != 'a' -> decrementi -> 'b'
- tutto ciò che sta a destra diventa 'z'

(define (str-prev str)
  ; i parte dall'ultimo indice della stringa
  (let (i (- (length str) 1))
    ; scorri da destra verso sinistra finché trovi solo 'a'
    ; (gli 'a' sono il minimo, quindi vanno "presi in prestito")
    (while (and (>= i 0) (= (str i) "a"))
      (-- i))
    ; se i == -1 significa che tutti i caratteri erano 'a'
    ; quindi non esiste una stringa precedente (caso minimo assoluto)
    (if (= i -1)
        nil
        (begin
          ; decrementa il primo carattere da destra diverso da 'a'
          ; (equivalente al "prestito" in una sottrazione)
          (setf (str i) (char (- (char (str i)) 1)))
          ; ricostruisce la stringa finale:
          ; - prende il prefisso fino a i incluso
          ; - riempie la parte destra con 'z' (massimo possibile)
          ;   per ottenere la stringa lessicograficamente immediatamente precedente
          (extend (slice str 0 (+ i 1))
                  (dup "z" (- (length str) (+ i 1))))))))

(str-prev "abd")
;-> "abc"
(str-prev "pazzp")
;-> "pazzo"
(str-prev "abc")
;-> "abb"
(str-prev "b")
;-> "a"
(str-prev (str-next "aaa"))
;-> "aaa"
(str-prev (str-next "zzz"))
;-> nil

Per le due funzioni vale:

1) (str-prev (str-next str) = str
Solo sulle stringhe che NON sono del tipo "zzz...z".

2) (str-next (str-next prev) = str
Solo sulle stringhe che NON sono del tipo "aaa...a"

(str-prev "a")
;-> nil
(str-prev "aaa")
;-> nil
(str-next "zzz")
;-> ""


-------------
K Empty Slots
-------------

Abbiamo n lampadine disposte in fila, numerate da 1 a n.
Inizialmente, tutte le lampadine sono spente.
Ogni giorno accendiamo esattamente una lampadina e, dopo n giorni, tutte le lampadine saranno accese.
Ti viene fornito un array di lampadine di lunghezza n, dove bulbs[i] = x significa che il giorno (i+1) accendiamo la lampadina in posizione x.
Nota che i è indicizzato da 0 (inizia da 0) mentre le posizioni delle lampadine x sono indicizzate da 1 (iniziano da 1).
Dato un intero k, trovare il numero minimo di giorni in cui esistono due lampadine accese che hanno esattamente k lampadine tra di loro e tutte le k lampadine intermedie sono spente.
Ad esempio, se k = 1 e in un certo giorno abbiamo le lampadine nelle posizioni 3 e 5 accese e la lampadina 4 spenta, questo soddisfa la condizione (esattamente 1 lampadina spenta tra di loro).
Se non esiste un giorno in cui questa condizione sia soddisfatta, restituire nil.

Questo è il classico problema noto come 'K Empty Slots': si cerca il primo giorno in cui due lampadine accese hanno esattamente 'k' lampadine spente tra loro.

Algoritmo
---------
Invece di simulare giorno per giorno (che sarebbe costoso), si fa una trasformazione fondamentale:
a) trasformiamo il problema in uno statico sui giorni
b) poi usiamo una 'finestra intelligente' per verificare la condizione

1) Costruzione dell'array 'day'

  day(pos) = giorno in cui si accende la lampadina in posizione 'pos'

Esempio:
  bulbs = (2 1 3 5 4)
  giorno 1 -> accendiamo posizione 2
  giorno 2 -> accendiamo posizione 1
  giorno 3 -> accendiamo posizione 3
  giorno 4 -> accendiamo posizione 5
  giorno 5 -> accendiamo posizione 4
Otteniamo day = (2 1 3 5 4), cioè
- posizione 1 -> giorno 2
- posizione 2 -> giorno 1
- ecc.

2) Riduzione del problema

Cerchiamo due posizioni 'l' e 'r' tali che:
- r = l + k + 1
- tutte le posizioni tra 'l' e 'r' si accendono dopo entrambe

Formalmente:
per ogni i in (l+1 ... r-1):
    day[i] > max(day[l], day[r])

Questo garantisce che le due estremità sono accese e quelle in mezzo sono ancora spente.

3) Sliding window

Usiamo una finestra [l ........ r] con lunghezza 'k + 2'.
(l = left, sinistra e r = right, destra)
Si scorre con indice 'i' tra 'l' e 'r' e abbiamo due casi:
Caso A — finestra valida
 Se arriviamo a (i == r) senza violazioni:
 - abbiamo trovato una soluzione
 - giorno = 'max(day[l], day[r])'
 - aggiorniamo 'out'
 - facciamo scorrere la finestra
Caso B — violazione
Se troviamo un indice 'i' tale che day[i] < max(day[l], day[r]) significa che la lampadina interna si accende troppo presto -> rompe la condizione.
Allora spostiamo 'l = i' e ricostruiamo 'r = i + k + 1'.

(define (kempty bulbs k)
  (local (n out day left right i)
    ; numero totale di lampadine
    (setq n (length bulbs))
    ; valore sentinella per "nessuna soluzione trovata"
    (setq out 99999999)
    ; day[i] = giorno in cui si accende la lampadina i+1
    (setq day (array n '(0)))
    ; costruzione dell'array day
    ; bulbs[i] = posizione accesa al giorno i+1
    (for (i 0 (- n 1))
      (setf (day (- (bulbs i) 1)) (+ i 1)))
    ; inizializzazione finestra
    ; left = sinistra, right = destra
    ; distanza tra left e right = k lampadine in mezzo
    (setq left 0)
    (setq right (+ left k 1))
    ; i scorre tra left e right
    (setq i (+ left 1))
    (while (< right n)
      ; caso 1: abbiamo controllato tutta la finestra
      (if (= i right)
        (begin
          ; finestra valida:
          ; giorno in cui entrambe sono accese
          (setq out (min out (max (day left) (day right))))
          ; sposta la finestra
          (setq left i)
          (setq right (+ left k 1))
          (setq i (+ left 1))
        )
        ; caso 2: controllo posizione interna
        (begin
          ; se una lampadina interna si accende troppo presto
          (if (< (day i) (max (day left) (day right)))
            (begin
              ; finestra non valida -> riparti da i
              (setq left i)
              (setq right (+ left k 1))
              (setq i (+ left 1))
            )
            ; altrimenti continua a controllare
            (++ i)
          )
        )
      )
    )
    ; se mai aggiornato -> nessuna soluzione
    (if (= out 99999999) nil out)))

Proviamo:

(kempty '(2 1 3 5 4) 1)
;-> 4

(kempty '(9 8 2 1 3 5 4 6 7) 1)
;-> 6

Complessità: 
Tempo:  O(n) (ogni indice viene visitato al massimo una volta)
Spazio: O(n) (array 'day')


---------------------
N come somma di 2 e 3
---------------------

Dato un intero N maggiore di 0, scriverlo come somma di 2 e 3.

Algoritmo 1
-----------
Sottrarre 2 finché la somma non è un multiplo di 3, quindi calcolare i 3 che rimangono (oppure viceversa).
Restituiamo una lista del tipo (a b), dove 'a' è il numero di 2 e 'b' e il numero di 3 che devono essere sommati per arrivare a N.

(define (sum23 n)
  (let (tmp n)
    ; sottrae 2 a N (tmp) fino a che non N (tmp) diventa multiplo di 3
    (until (zero? (% tmp 3)) (-- tmp 2))
    ; calcola i 2 e i 3 necessari
    (list (/ (- n tmp) 2) (/ tmp 3))))

(sum23 6)
;-> (0 2)
(sum23 10)
;-> (2 2)
(sum23 22)
;-> (2 6)

(define (sum32 n)
  (let (tmp n)
    ; sottrae 3 a N (tmp) fino a che non N (tmp) diventa multiplo di 2
    (until (zero? (% tmp 2)) (-- tmp 3))
    ; calcola i 2 e i 3 necessari
    (list (/ tmp 2) (/ (- n tmp) 3))))

(sum32 6)
;-> (3 0)
(sum32 22)
;-> (11 0)

Algoritmo 2
-----------
Dobbiamo trovare tutte le soluzioni intere non negative dell'equazione diofantea:

  2x + 3y = N,  con x,y > 0

1) Condizione di esistenza
Poichè gcd(2,3) = 1, esiste sempre almeno una soluzione intera per ogni N >= 0.

2) Forma generale delle soluzioni
Ricaviamo x:
  x = (N - 3y) / 2
Per avere x intero N - 3y deve essere pari, cioè deve risultare N - y ≡ 0 (mod 2), quindi y ≡ N (mod 2).
3) Vincoli
Inoltre deve essere:
- y >= 0
- x >= 0  ->  N - 3y >= 0  ->  y <= floor(N/3)

4) Procedura
Tutte le soluzioni si ottengono nel modo seguente:
- si fa variare y da 0 a floor(N / 3)
- si prendono solo i valori di y con stessa parità di N
- poi si calcola: x = (N - 3y) / 2

Esempio
  Per N = 10:
  - y <= 3
  - N è pari -> y deve essere pari
  Valori validi:
  - y = 0 -> x = 5
  - y = 2 -> x = 2
  Soluzioni: (5,0) e (2,2)

Il numero totale di soluzioni dipende da N:

  numero_soluzioni = floor((floor(N/3) - (N mod 2)) / 2) + 1
  (se il risultato è >= 0)

Ecco la funzione `soluzioni` riscritta con commenti dettagliati (seguendo il tuo stile):

(define (sum23-all N)
  ; Restituisce tutte le coppie (x y) tali che:
  ;   2x + 3y = N
  ; con x >= 0 e y >= 0 interi
  (let (out '()) ; lista delle soluzioni trovate
    ; y può variare da 0 fino a floor(N / 3)
    ; oltre questo valore, 3y > N e quindi x diventerebbe negativo
    (for (y 0 (/ N 3))
      ; condizione di integrita':
      ; x = (N - 3y) / 2 deve essere intero
      ; questo equivale a richiedere che (N - 3y) sia pari
      ; cioe': N - y è pari  <=>  y ha la stessa parità di N
      (if (= (% (- N y) 2) 0)
        ; calcolo di x
        (let (x (/ (- N (* 3 y)) 2))
          ; controllo di sicurezza (in teoria sempre vero per il range scelto)
          ; garantisce x >= 0
          (if (>= x 0)
            ; aggiunge la soluzione in fondo alla lista
            ; formato: (x y)
            (push (list x y) out -1)
          )
        )
      )
    )
    ; restituisce la lista completa delle soluzioni
    out))

Proviamo:

(sum23-all 6)
;-> ((3 0) (0 2))
(sum23-all 10)
;-> ((5 0) (2 2))
(sum23-all 22)
;-> ((11 0) (8 2) (5 4) (2 6))

Scriviamo una versione piu' efficiente che evita il test di parita' dentro il ciclo (salta direttamente ai valori validi di y).

(define (sum23-all-fast N)
  ; Restituisce tutte le coppie (x y) tali che:
  ;   2x + 3y = N
  ; con x >= 0 e y >= 0 interi
  ; Invece di provare tutti i valori di y e filtrare per parita',
  ; generiamo direttamente solo i valori validi di y.
  (letn (
         out '()              ; lista delle soluzioni
         ymax (/ N 3)         ; massimo valore possibile per y (floor(N/3))
         ; y deve avere la stessa parita' di N:
         ; y == N (mod 2)
         ; quindi:
         ; - se N e' pari -> y parte da 0
         ; - se N e' dispari -> y parte da 1
         y0 (% N 2)
        )
    ; ciclo sui soli valori validi di y:
    ; partiamo da y0 e incrementiamo di 2 per mantenere la parita'
    (for (y y0 ymax 2)
      ; calcolo di x:
      ; x = (N - 3y) / 2
      ; qui e' garantito intero per costruzione (parita' gia' rispettata)
      (let (x (/ (- N (* 3 y)) 2))
        ; non serve controllare la parita' ne' x >= 0:
        ; - la parita' e' gia' soddisfatta
        ; - x >= 0 e' garantito da y <= ymax
        ; aggiungiamo la soluzione
        (push (list x y) out -1)
      )
    )
    ; lista finale delle soluzioni
    out))

Proviamo:

(sum23-all-fast 6)
;-> ((3 0) (0 2))
(sum23-all-fast 10)
;-> ((5 0) (2 2))
(sum23-all-fast 22)
;-> ((11 0) (8 2) (5 4) (2 6))

(= (sum23-all 1000) (sum23-all-fast 1000))

(length (sum23-all 500))

(time (sum23-all 500) 1e3)
;-> 100.031
(time (sum23-all-fast 500) 1e3)
;-> 84.982

(time (sum23-all 2000) 1e3)
;-> 1255.283
(time (sum23-all-fast 500) 1e3)
;-> 1180.370

(time (sum23-all 5000) 1e3)
;-> 7175.231
(time (sum23-all-fast 5000) 1e3)
;-> 6967.301

Algoritmo 3
-----------
Possiamo evitare completamente il ciclo 'for' costruendo la lista tramite una 'mappa su una sequenza di indici':
I valori validi di y sono: y = y0 + 2k
con (y0 = N mod 2) e   - (k = 0,1,...,kmax)
dove: (kmax = floor((ymax - y0)/2)) e  (ymax = floor(N/3))
Poi per ogni y calcoliamo x = (N - 3y)/2.
In pratica, non si itera su y, ma su un indice k e si generano solo i valori validi.
Non occorre alcun controllo dentro il ciclo perchè tutto è già garantito.

(define (sum23-all-fast2 N)
  ; Restituisce tutte le coppie (x y) tali che:
  ;   2x + 3y = N
  ; con x >= 0 e y >= 0 interi
  ;
  ; Versione senza ciclo esplicito:
  ; costruiamo direttamente le soluzioni usando una sequenza di indici k
  (letn (
         ymax (/ N 3)                 ; massimo y possibile
         y0 (% N 2)                   ; prima y valida (stessa parita' di N)
         ; numero massimo di passi k:
         ; y = y0 + 2k <= ymax
         ; -> k <= (ymax - y0)/2
         kmax (if (>= ymax y0)
                  (/ (- ymax y0) 2)
                  -1)
        )
    ; se non esistono soluzioni, restituisce lista vuota
    (if (< kmax 0)
        '()
        ; per ogni k costruiamo (x y)
        (map
          (fn (k)
            (letn (
                   y (+ y0 (* 2 k))          ; valore di y
                   x (/ (- N (* 3 y)) 2)     ; valore di x (intero garantito)
                  )
              (list x y)))
          (sequence 0 kmax)))))

Proviamo:

(sum23-all-fast2 6)
;-> ((3 0) (0 2))
(sum23-all-fast2 10)
;-> ((5 0) (2 2))
(sum23-all-fast2 22)
;-> ((11 0) (8 2) (5 4) (2 6))

Test di correttezza

(define (check-sol N lst)
  (let (out nil)
    (dolist (el lst out)
      (if (!= N (+ (* (el 0) 2) (* (el 1) 3))) (setq out true)))
    (not out)))

(check-sol 1000 (sum23-all 1000))
;-> true

(= (sum23-all 1000) (sum23-all-fast 1000) (sum23-all-fast2 1000))
;-> true

Test di velocità

(time (sum23-all 500) 1e3)
;-> 100.031
(time (sum23-all-fast 500) 1e3)
;-> 84.982
(time (sum23-all-fast2 500) 1e3)
;-> 19.984

(time (sum23-all 2000) 1e3)
;-> 1255.283
(time (sum23-all-fast 2000) 1e3)
;-> 1180.370
(time (sum23-all-fast2 2000) 1e3)
;-> 82.982

(time (sum23-all 5000) 1e3)
;-> 7175.231
(time (sum23-all-fast 5000) 1e3)
;-> 6967.301
(time (sum23-all-fast2 5000) 1e3)
;-> 195.331

Infine una versione 'one-liner':
La logica è la stessa di 'sum23-fast2':
- genera (k = 0..kmax)
- costruisce (y = y0 + 2k)
- calcola (x = (N - 3y)/2)
- restituisce direttamente la lista delle soluzioni

(define (f N) (letn (ymax (/ N 3) y0 (% N 2) kmax (if (>= ymax y0) (/ (- ymax y0) 2) -1)) (if (< kmax 0) '() (map (fn (k) (let (y (+ y0 (* 2 k))) (list (/ (- N (* 3 y)) 2) y))) (sequence 0 kmax)))))

  ymax -> Y
  kmax -> K
  y0 -> B

(define(f N)(letn(Y(/ N 3)B(% N 2)K(if(>= Y B)(/(- Y B)2)-1))(if(< K 0)'()(map(fn(k)(let(y(+ B(* 2 k)))(list(/(- N(* 3 y))2)y)))(sequence 0 K)))))

Proviamo:

(= (sum23-all 1000) (f 1000))
;-> true
(time (f 5000) 1e3)
;-> 201.651

============================================================================

