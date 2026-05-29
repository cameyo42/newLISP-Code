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
Se x è nella blacklist -> lo rimappiamo a un valore valido >= m

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
1. si calcola il numero totale di elementi 'tot'
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


---------------------
Lancio di 2 dadi su 3
---------------------

Abbiamo 3 dadi con 6 valori interi nelle facce.
I dadi non hanno necessariamente gli stessi valori.
Il giocatore A sceglie un dado e poi il giocatore B ne sceglie uno dei due rimanenti.
Entrambi lanciano il dado scelto.
Se ottengono lo stesso numero, entrambi rilanciano il dado.
Altrimenti, vince chi ha ottenuto il numero più alto.

Dati i valori numerici di 3 dadi, determinare le probabilità di vittoria di ogni dado.

Algoritmo Brute-force
Le probabilità di vittoria si trovano calcolando tutte le combinazioni dei risultati del lancio dei dadi tra loro.

Esempio:
  Dadi -> D1, D2, D3
  P(D1)= P(D1 vs D2) + P(D1 vs D3)
  P(D2)= P(D2 vs D1) + P(D2 vs D3)
  P(D3)= P(D3 vs D1) + P(D3 vs D2)

(define (lanci L1 L2)
; prende due liste (facce dei dadi)
; restituisce tutti i lanci possibili tra due dadi (coppie di valori)
  (let (out '())
    (dolist (el1 L1)
      (dolist (el2 L2)
        (push (list el1 el2) out -1)))
    out))

(define (calc-prob lst)
; prende la lista di tutti i lanci possibili tra due dadi
; restituisce una lista: (vittorie pareggi sconfitte)
  (list  (length (filter (fn(x) (> (x 0) (x 1))) lst))
         (length (filter (fn(x) (= (x 0) (x 1))) lst))
         (length (filter (fn(x) (< (x 0) (x 1))) lst))))

(define (print-result-vs dado)
; stampa i risultati del dado corrente contro 'dado'
  (println "contro " dado ": "
          (format "%0.2f" (div (res 0) tot-lanci)) "% win, "
          (format "%0.2f" (div (res 1) tot-lanci)) "% draw, "
          (format "%0.2f" (div (res 2) tot-lanci)) "% lose"))

; Funzione che calcola le probabilità di vittoria di tutti i dadi
(define (dado D1 D2 D3)
  (local (facce tot-lanci res)
    (setq facce (length D1))
    (setq tot-lanci (* facce facce))
    ; *** D1 ***
    (println "*** D1 ***")
    ; D1 vs D2
    (setq res (calc-prob (lanci D1 D2)))
    (print-result-vs "D2")
    ; D1 vs D3
    (setq res (calc-prob (lanci D1 D3)))
    (print-result-vs "D3")
    ; *** D2 ***
    (println "*** D2 ***")
    ; D2 vs D1
    (setq res (calc-prob (lanci D2 D1)))
    (print-result-vs "D1")
    ; D2 vs D3
    (setq res (calc-prob (lanci D2 D3)))
    (print-result-vs "D3")
    ; *** D3 ***
    (println "*** D3 ***")
    ; D3 vs D1
    (setq res (calc-prob (lanci D3 D1)))
    (print-result-vs "D1")
    ; D3 vs D2
    (setq res (calc-prob (lanci D3 D2)))
    (print-result-vs "D2") '>))

Proviamo:

(setq D1 '(1 2 3 4 5 6))
(setq D2 '(1 2 3 4 5 6))
(setq D3 '(1 2 3 4 5 6))
(dado D1 D2 D3)
;-> *** D1 ***
;-> contro D2: 0.42% win, 0.17% draw, 0.42% lose
;-> contro D3: 0.42% win, 0.17% draw, 0.42% lose
;-> *** D2 ***
;-> contro D1: 0.42% win, 0.17% draw, 0.42% lose
;-> contro D3: 0.42% win, 0.17% draw, 0.42% lose
;-> *** D3 ***
;-> contro D1: 0.42% win, 0.17% draw, 0.42% lose
;-> contro D2: 0.42% win, 0.17% draw, 0.42% lose

; dadi non-transitivi
(setq D1 '(2 2 4 4 9 9))
(setq D2 '(1 1 6 6 8 8))
(setq D3 '(7 7 5 5 3 3))
(dado D1 D2 D3)
;-> *** D1 ***
;-> contro D2: 0.56% win, 0.00% draw, 0.44% lose
;-> contro D3: 0.44% win, 0.00% draw, 0.56% lose
;-> *** D2 ***
;-> contro D1: 0.44% win, 0.00% draw, 0.56% lose
;-> contro D3: 0.56% win, 0.00% draw, 0.44% lose
;-> *** D3 ***
;-> contro D1: 0.56% win, 0.00% draw, 0.44% lose
;-> contro D2: 0.44% win, 0.00% draw, 0.56% lose

(setq D1 '(3 3 3 4 5 6))
(setq D2 '(2 2 3 5 5 5))
(setq D3 '(2 2 3 4 6 6))
(dado D1 D2 D3)
;-> *** D1 ***
;-> contro D2: 0.50% win, 0.17% draw, 0.33% lose
;-> contro D3: 0.47% win, 0.17% draw, 0.36% lose
;-> *** D2 ***
;-> contro D1: 0.33% win, 0.17% draw, 0.50% lose
;-> contro D3: 0.39% win, 0.14% draw, 0.47% lose
;-> *** D3 ***
;-> contro D1: 0.36% win, 0.17% draw, 0.47% lose
;-> contro D2: 0.47% win, 0.14% draw, 0.39% lose


--------------------------------------
Inserimento di N elementi in una lista
--------------------------------------

Vogliamo inserire gli elementi di una lista in un'altra lista in determinati indici.

Esempio:
  Lista = (0 1 2 3 4 5 6 7 8 9)
  Elementi = ("one" "three" "four" "nine")
  Indici = (1 3 4 9)

Dobbiamo inserire "one" all'indice 1 di Lista
Dobbiamo inserire "three" all'indice 3 di Lista
Dobbiamo inserire "four" all'indice 4 di Lista
Dobbiamo inserire "nine" all'indice 9 di Lista

Possiamo farlo in due modi:

1) inserendo gli elementi agli indici dati (1 3 4 9)
  indici = (1 3 4 9)
  risultato = (0 "one" 1 "three" "four" 2 3 4 5 "nine" 6 7 8 9)

2) inserendo gli elementi considerando che ogni inserimento sposta in avanti di 1 gli indici successivi (1 4 6 12).
  indici = (1 4 6 12)
  risultato = (0 "one" 1 2 "three" 3 "four" 4 5 6 7 8 "nine" 9)
In questo caso gli elementi inseriti ("one" "three" "four" "nine") si trovano nelle posizioni relative che avevano gli elementi originali (cioè "one" al posto che aveva '1', "three" al posto che aveva '3', ecc).

(define (insert-at lst indexes values shift)
"Insert elements at indexes of a list"
  ; shift indexes to new positions
  (if shift (setq indexes (map (curry + $idx) indexes)))
  (println indexes)
  (dolist (el indexes)
    (push (values $idx) lst el))
  lst)

Proviamo:

(setq L '(0 1 2 3 4 5 6 7 8 9))
(setq E '("one" "three" "four" "nine"))
(setq I '(1 3 4 9))

(insert-at L I E)
;-> (0 "one" 1 "three" "four" 2 3 4 5 "nine" 6 7 8 9)

(insert-at L I E true)
;-> (0 "one" 1 2 "three" 3 "four" 4 5 6 7 8 "nine" 9)


------------------------------
Visita di punti in una griglia
------------------------------

Dati K punti casuali in una griglia MxN, visitarli tutti in ordine senza mai intersecare il percorso (evitare anche di visitare lo stesso punto due volte).
Il percorso può attraversare i valori le celle vuote per connettere i punti.
Ogni riga della griglia contiene almeno un punto.

; Funzione che stampa la griglia
(define (print-grid grid ch0 ch1 coord)
"Print a matrix with only digits (0..9)"
  (local (row col)
    (setq row (length grid))
    (setq col (length (first grid)))
    ; indici di colonna della griglia
    (if coord
        (println "  " (join (map (fn(x) (format "%2d" x)) (sequence 0 (- col 1))))))
    (for (i 0 (- row 1))
      ; indice di riga della griglia
      (if coord (print (format "%2d" i)))
      ; stampa della griglia
      (for (j 0 (- col 1))
        (if (and (!= ch0 "") (!= ch1 ""))
            (begin
              (cond ((= (grid i j) 0) (print ch0))
                    ((= (grid i j) 1) (print ch1))
                    (true
                      (print (format "%2d" (grid i j))))))
            ;else
            (print (format "%2d" (grid i j)))))
      (println))))

(setq g (collect (rand 2 10) 8))

(print-grid g "0 " "1 ")
;-> 0 0 1 1 1 0 1 1 1 1
;-> 0 0 0 1 0 1 0 1 0 0
;-> 1 0 1 0 0 1 0 0 1 1
;-> 0 0 1 1 1 1 1 1 1 0
;-> 0 1 1 1 0 0 1 1 1 0
;-> 0 0 1 1 1 1 1 0 1 1
;-> 1 1 1 0 1 1 0 1 0 0
;-> 0 1 1 1 0 0 0 0 1 1

Algoritmo
---------
Creazione di un percorso a zig-zag (tipo serpente)
  Cella vuota -> 0
  Cella piena -> 1
Cominciando dalla riga 0:
1)
  1.1) Partire dal primo '1' a sinistra della riga corrente
  1.2) Andare a destra nella riga fino all'indice massimo tra l'indice dell'ultimo '1' della riga corrente e l'indice dell'ultimo '1' della riga successiva
  1.3) Memorizzare tutti gli indici attraversati (riga colonna)
  1.4) Passare alla riga successiva
2)
  2.1) Partire dal primo '1' a destra della riga corrente
  2.2) Andare a sinistra nella riga fino all'indice minimo tra l'indice del primo '1' della riga corrente e l'indice del primo '1' della riga successiva
  1.3) Memorizzare tutti gli indici attraversati (riga colonna)
  1.4) Passare alla riga successiva
3) Ripetere 1) e 2) fino al termine delle righe.

Prima di ogni operazione sulla 'riga successiva' bisogna controllare che la riga corrente non sia l'ultima riga della griglia.

Esempio:

griglia = (0 0 0 1)
          (0 1 0 0)
          (0 0 1 0)
          (1 0 0 0)

Il percorso di soluzione sono le celle numerate da 1 a 9:
  0 0 0 1
  0 4 3 2
  0 5 6 0
  9 8 7 0

Le coordinate delle celle del percorso sono:
  (0 3) (1 3) (1 2) (1 1) (2 1) (2 2) (3 2) (3 1) (3 0)

La matrice soluzione è la seguente:

; Funzione per cercare l'indice del valore '1' più a destra di una lista
(define (right1 row) (let (idx (ref-all 1 row)) (if idx ((idx -1) 0) nil)))
(right1 '(0 1 0 1 0))
;-> 3

; Funzione per cercare l'indice del valore '1' più a sinistra di una lista
(define (left1 row) (find 1 row))
(left1 '(0 1 0 1 0))
;-> 1

; Funzione che trova il percorso a serpente (per griglie senza righe vuote)
(define (snake grid)
  (letn ( (rows (length grid))        ; numero colonne della griglia
          (cols (length (grid 0)))    ; numero colonne della griglia
          (path '()) ; lista delle celle del percorso di soluzione
          (mt (array rows cols '(0))) ; matrice soluzione
          (dir 1)    ; 1 = sx -> dx , -1 = sx <- dx
          (riga '()) ; riga corrente
          (sx1 -1)   ; indice dell'1 più a sinistra
          (dx1 -1)   ; indice dell'1 più a destra
          (r 0)      ; numero riga corrente
          (c 0) )    ; numero colonna corrente
    (while (< r rows)
      (setq riga (grid r))
      (cond ((= dir 1) ; da sinistra a destra  --->
              ; indice di sinistra
              (setq sx1 (left1 riga))
              ; indice di destra
              (if (= r (- rows 1)) ; ultima riga?
                (setq dx1 (right1 riga))
                ; else
                ; calcola l'indice dell'1 più a destra
                (setq dx1 (max (right1 riga) (right1 (grid (+ r 1))))))
              ; aggiorna path
              (for (c sx1 dx1)
                (push (list r c) path -1))
              ; prossima riga
              (++ r)
              ; cambia direzione per la prossima riga
              (setq dir -1))
            ((= dir -1) ; da destra a sinistra  <---
              ; indice di destra
              (setq dx1 (right1 riga))
              ; indice di sinistra
              (if (= r (- rows 1)) ; ultima riga?
                (setq sx1 (left1 riga))
                ; else
                ; calcola l'indice dell'1 più a sinistra
                (setq sx1 (min (left1 riga) (left1 (grid (+ r 1))))))
              ; aggiorna path
              (for (c dx1 sx1)
                (push (list r c) path -1))
              ; prossima riga
              (++ r)
              ; cambia direzione per la prossima riga
              (setq dir 1))
      )
    )
    (println path)
    ; Costruzione della matrice soluzione
    (dolist (el path) (setf (mt (el 0) (el 1)) 1))
    ; restituisce la matrice soluzione
    mt))

Proviamo:

(setq g '((0 0 1 0)))
(snake g)
;-> ((0 2))
;-> ((0 0 1 0))

(setq g '((0 0 1 0)
          (1 0 1 0)))
(snake g)
;-> ((0 2) (1 2) (1 1) (1 0))
;-> ((0 0 1 0) (1 1 1 0))

(setq g '((0 0 0 1 0)
          (0 1 0 1 0)
          (1 0 0 1 0)))
(print-grid (snake g) "0 " "1 ")
;-> ((0 3) (1 3) (1 2) (1 1) (1 0) (2 0) (2 1) (2 2) (2 3))
;-> 0 0 0 1 0
;-> 1 1 1 1 0
;-> 1 1 1 1 0

(setq g '((0 0 1 1 0)
          (0 1 0 1 0)
          (1 0 0 1 0)
          (0 0 0 0 1)))

(print-grid (snake g) "0 " "1 ")
;-> ((0 2) (0 3)
;->  (1 3) (1 2) (1 1) (1 0)
;->  (2 0) (2 1) (2 2) (2 3) (2 4)
;->  (3 4))
;-> 0 0 1 1 0
;-> 1 1 1 1 0
;-> 1 1 1 1 1
;-> 0 0 0 0 1


-----------------------------------------
Punto intero esterno ad una circonferenza
-----------------------------------------

Data una circonferenza di raggio intero R e centro C in (0,0), determinare il punto P(x,y) di coordinate intere ed esterno alla circonferenza la cui distanza dal centro è minima.
Scrivere la funzione più corta possibile.

Equazione circonferenza centrata in C = (0,0) di raggio R:

  X^2 + Y^2 = R^2

per il punto P(x,y) deve risultare:

  x^2 + y^2 > R^2 <--> x^2 + y^2 >= (+ R^2 1)

Dato 'R' bisogna la seguente equazione in x e y:

  x^2 + y^2 >= (+ R^2 1)

Algoritmo
Costruire un quadrato minimo centrato in (0,0) di lato (R + 1) che include il cerchio
Scansionare solo il quadrato minimo centrato in (0,0)
Calcolare i punti dentro il box, ma fuori dal cerchio
Calcolare tra questi punti quello a distanza minima dal centro.

(define (dist2d p1 p2)
"Calculate 2D Cartesian distance of two points P1 = (x1 y1) and P2 = (x2 y2)"
  (let ( (x1 (p1 0)) (y1 (p1 1))
         (x2 (p2 0)) (y2 (p2 1)) )
    (sqrt (add (mul (sub x1 x2) (sub x1 x2))
               (mul (sub y1 y2) (sub y1 y2))))))

(define (dist2d-2 p1 p2)
"Calculate the square of 2D Cartesian distance of two points P1 = (x1 y1) and P2 = (x2 y2)"
  (let ( (x1 (p1 0)) (y1 (p1 1))
         (x2 (p2 0)) (y2 (p2 1)) )
  (add (mul (sub x1 x2) (sub x1 x2))
       (mul (sub y1 y2) (sub y1 y2)))))

; Funzione che calcola i punti esterni al cerchio e interni al quadrato minimo
(define (punti-esterni r)
  (letn (res '() soglia (+ (* r r) 1))
    (for (x (- r) r)
      (for (y (- r) r)
        (if (>= (+ (* x x) (* y y)) soglia)
            (push (list x y) res -1))))
    res))

Proviamo:

(setq pts (punti-esterni 5))
;-> ((-5 -5) (-5 -4) (-5 -3) (-5 -2) (-5 -1) (-5 1) (-5 2) (-5 3) (-5 4)
;->  (-5 5) (-4 -5) (-4 -4) (-4 4) (-4 5) (-3 -5) (-3 5) (-2 -5) (-2 5)
;->  (-1 -5) (-1 5) (1 -5) (1 5) (2 -5) (2 5) (3 -5) (3 5) (4 -5) (4 -4)
;->  (4 4) (4 5) (5 -5) (5 -4) (5 -3) (5 -2) (5 -1) (5 1) (5 2) (5 3)
;->  (5 4) (5 5))

Calcoliamo la distanza dal centro di questi punti:

(map (fn(x) (dist2d-2 '(0 0) x)) pts)
;-> (50 41 34 29 26 26 29 34 41 50 41 32 32 41 34 34 29 29 26 26
;->  26 26 29 29 34 34 41 32 32 41 50 41 34 29 26 26 29 34 41 50)

Possiamo costruire direttamente la 'corona minima', cioè tutti i punti interi (x,y) tali che:

  x^2 + y^2 >= R^2 + 1

che sono 'appena fuori' (cioè vicini al bordo)

Per ogni x, troviamo il più piccolo y >= 0 che soddisfa la disequazione.

  y = (ceil(sqrt(max(0, R^2 + 1 - x^2))))

Questa dà il primo punto valido sopra il bordo del cerchio.

Algoritmo
1) per ogni (x in x >= 0)
2) calcolare il primo punto fuori dal cerchio
3) riflettere nei 4 quadranti evitando duplicati

(define (corona-esterna r)
  (letn (res '() soglia (+ (* r r) 1) y0)
    (for (x (- r) r)
      (setq y0 (ceil (sqrt (max 0 (- soglia (* x x))))))
      (push (list x y0) res -1)
      (if (!= y0 0)
          (push (list x (- y0)) res -1)))
    res))

Proviamo:

(setq pts (corona-esterna 5))
;-> ((0 6) (0 -6) (1 5) (-1 5) (1 -5) (-1 -5) (2 5) (-2 5) (2 -5) (-2 -5)
;->  (3 5) (-3 5) (3 -5) (-3 -5) (4 4) (-4 4) (4 -4) (-4 -4) (5 1) (-5 1)
;->  (5 -1) (-5 -1))

(map (fn(x) (dist2d-2 '(0 0) x)) pts)
;-> (36 36 26 26 26 26 29 29 29 29 34 34 34 34 32 32 32 32 26 26 26 26)

Osservazione:
Se il cerchio è centrato in (0,0) e il raggio R è intero, allora il punto intero piu' vicino al centro, ma fuori dal cerchio è sempre dato dal punto Q = (R, 1) o uno degli altri 7 punti simmetrici (es. Q = (-R, 1)).

Infatti la condizione

  x^2 + y^2 >= R^2 + 1

indica che stiamo cercando il punto intero che minimizza** (x^2 + y^2) sotto questo vincolo.
Il minimo valore possibile fuori dal cerchio è (R^2 + 1) (il primo intero dopo (R^2))
Il punto (R,1) soddisfa:

  R^2 + 1^2 = R^2 + 1

Quindi raggiunge esattamente il minimo possibile.

Non può esistere di meglio, perchè se esistesse un punto più vicino, dovrebbe avere:

  x^2 + y^2 < R^2 + 1

ma allora:

  x^2 + y^2 >= R^2

sarebbe dentro o sul cerchio, quindi non valido.

Per simmetria, abbiamo sempre almeno questi 8 punti:

  (+-R, +-1) e (+-1, +-R)

Quindi la funzione che risolve il problema è molto semplice e corta.

(define (punto r) (list r 1))

Versione code-golf (23 caratteri):

(define(f r)(list r 1))

Comunque, oltre a questi 8 punti, potrebbero esistere altre soluzioni (cioè altri punti fuori dal cerchio che hanno distanza minima), però una soluzione del tipo (R,1) esiste sempre ed è sempre a distanza minima.

Se vogliamo calcolare quando esistono altre soluzioni oltre (R,1) dobbiamo utilizzare il seguente teorema:

Un intero N è somma di due quadrati in più modi se e solo se nella sua fattorizzazione:
- i primi p≡1(mod4) possono comparire liberamente
- i primi p≡3(mod4) devono comparire con esponente pari
ma il numero di rappresentazioni dipende da quanti primi ≡1(mod4) compaiono.

Vogliamo le soluzioni intere di:

  x^2 + y^2 = R^2 + 1

Le soluzioni "banali" esistono sempre:

  (+-R, +-1) e (+-1, +-R)

Esistono ALTRE soluzioni se e solo se (R^2 + 1) ha almeno due fattori primi distinti p == 1 (mod 4).
Se (R^2 + 1) e' primo, oppure ha al piu' un fattore p == 1 (mod 4), allora NON ci sono altre soluzioni.

; Funzione che conta i fattori primi == 1 (mod 4)
(define (conta-primi-1mod4 n)
  (letn (f (factor n) unici '() cnt 0)
    (dolist (p f)
      (if (nil? (ref p unici)) (push p unici -1)))
    (dolist (p unici)
      (if (= (% p 4) 1) (++ cnt)))
    cnt))

; Funzione che verifica se esistono soluzioni extra
; Restituisce 'nil' quando esistono solo (R,1) e simmetrici
; oppure 'true' quando esistono altre coppie (piu' "bilanciate")
(define (soluzioni-extra? R)
  (let (n (+ (* R R) 1))
    (>= (conta-primi-1mod4 n) 2)))

Proviamo:

(soluzioni-extra? 5)
;-> nil

(soluzioni-extra? 8)
;-> true   ; 65 = 5 * 13

(soluzioni-extra? 18)
;-> true   ; 325 = 5^2 * 13


---------------------
Problema di Apollonio
---------------------

Dati 3 cerchi di raggi r1, r2 e r3 e centri (x1, y1), (x2, y2) e (x3, y3), determinare il centro e il raggio di un quarto cerchio tangente ai cerchi dati.

Dati

Tre cerchi C1, C2, C3 con centri (x1,y1), (x2,y2), (x3,y3) e raggi r1, r2, r3.
Cerchio soluzione: centro (x, y), raggio r.

Equazioni di tangenza

  (x - x1)^2 + (y - y1)^2 = (r + s1*r1)^2
  (x - x2)^2 + (y - y2)^2 = (r + s2*r2)^2
  (x - x3)^2 + (y - y3)^2 = (r + s3*r3)^2
  con (s1, s2, s3) in {-1, +1}^3  (8 combinazioni).

Ogni sistema si ottiene scegliendo i segni s1, s2, s3 in {+1, -1}:
Per ogni combinazione si ottiene una soluzione (x, y, r) con r >= 0.

Passo 1 - Linearizzazione
-------------------------
Espandendo la eq(i) e sottraendo eq(1) da eq(2) e da eq(3), i termini x^2, y^2, r^2 si cancellano.
Si ottiene il sistema lineare 2x2 con r come parametro:

  A21*x + B21*y + C21*r = D21
  A31*x + B31*y + C31*r = D31

con coefficienti:

  A21 = 2*(x1 - x2)
  B21 = 2*(y1 - y2)
  C21 = 2*(s1*r1 - s2*r2)
  D21 = x1^2 - x2^2 + y1^2 - y2^2 + r2^2 - r1^2

  A31 = 2*(x1 - x3)
  B31 = 2*(y1 - y3)
  C31 = 2*(s1*r1 - s3*r3)
  D31 = x1^2 - x3^2 + y1^2 - y3^2 + r3^2 - r1^2

Passo 2 - Soluzione parametrica con la regola di Cramer
-------------------------------------------------------
Determinante del sottosistema 2x2 in x e y:

  det = A21*B31 - A31*B21

Se |det| < eps: i tre centri sono collineari, caso degenere, skip.

Con la regola di Cramer, x e y risultano lineari in r:

  x(r) = x0 + xr * r
  y(r) = y0 + yr * r

con:

  x0 = (D21*B31 - D31*B21) / det
  xr = (C31*B21 - C21*B31) / det

  y0 = (A21*D31 - A31*D21) / det
  yr = (A31*C21 - A21*C31) / det

Passo 3 - Equazione quadratica in r
-----------------------------------
Sostituendo x(r) e y(r) nella eq(1) espansa:

  (x(r) - x1)^2 + (y(r) - y1)^2 = (r + s1*r1)^2

Posti dx0 = x0 - x1 e dy0 = y0 - y1, espandendo e raccogliendo:

  P * r^2 + 2*Q * r + R = 0

con:

  P = xr^2 + yr^2 - 1
  Q = dx0*xr + dy0*yr - s1*r1
  R = dx0^2  + dy0^2  - r1^2

Passo 4 - Radici
----------------
Caso generale,  |P| > eps:

  delta = Q^2 - P*R

Se delta < 0: nessuna soluzione reale per questo vettore (s1,s2,s3).

  r = (-Q +/- sqrt(delta)) / P       (due candidati)

Caso degenere,  |P| <= eps  (tre cerchi tangenti a una retta comune):

  r = -R / (2*Q)                     (un candidato)

Si scartano i candidati con r < 0.

Passo 5 - Centro della soluzione
--------------------------------
  x = x0 + xr * r
  y = y0 + yr * r

Passo 6 - Verifica
------------------
Per ogni cerchio Ci la soluzione (x, y, r) deve soddisfare
esattamente una delle due condizioni:

  dist((x,y), (xi,yi)) = r + ri      (tangenza esterna)
  dist((x,y), (xi,yi)) = |r - ri|    (tangenza interna)
  dove dist((x,y), (xi,yi)) = sqrt((x-xi)^2 + (y-yi)^2).

Riepilogo
---------
Per ognuna delle 8 combinazioni (s1,s2,s3) in {-1,+1}^3:
  1. Calcola A21, B21, C21, D21, A31, B31, C31, D31
  2. Calcola det = A21*B31 - A31*B21
     Se |det| < eps: skip
  3. Calcola x0, xr, y0, yr  (Cramer)
  4. Calcola P, Q, R
  5. Se |P| > eps:
       delta = Q^2 - P*R
       Se delta < 0: skip
       r = (-Q +/- sqrt(delta)) / P   --> fino a 2 soluzioni
     Se |P| <= eps:
       r = -R / (2*Q)                 --> 1 soluzione
  6. Scarta r < 0 (solo cerchi reali)
  7. x = x0 + xr*r,   y = y0 + yr*r
  8. Verifica tangenza

In totale si ottengono al piu' 16 soluzioni candidate (8 sistemi x 2 radici), che si riducono alle classiche 8 soluzioni di Apollonio dopo aver scartato i duplicati e le soluzioni con r < 0.

; Problema di Apollonio
; Ogni cerchio e' rappresentato come lista (x y r)
; Restituisce lista di soluzioni ((x y r) ...)
; Per ogni soluzione (xc yc rc) vale:
;   dist((xc,yc), (xi,yi)) = rc + ri  (tangenza esterna)
;   dist((xc,yc), (xi,yi)) = |rc - ri| (tangenza interna)
(define (apollonius c1 c2 c3)
  (let ((x1 (c1 0)) (y1 (c1 1)) (r1 (c1 2))
        (x2 (c2 0)) (y2 (c2 1)) (r2 (c2 2))
        (x3 (c3 0)) (y3 (c3 1)) (r3 (c3 2))
        (solutions '()))
    (dolist (signs '((1  1  1) (1  1 -1) (1 -1  1) (1 -1 -1)
                    (-1  1  1)(-1  1 -1)(-1 -1  1)(-1 -1 -1)))
      (let ((s1 (signs 0)) (s2 (signs 1)) (s3 (signs 2)))
        ; Linearizzazione: eq(i)-eq(1) elimina x^2, y^2, r^2
        ; Risultato: A*x + B*y + C*r = D  con
        ;   A = 2*(x1-xi), B = 2*(y1-yi), C = 2*(s1*r1 - si*ri)
        ;   D = x1^2-xi^2 + y1^2-yi^2 + ri^2-r1^2
        (let ((A21 (mul 2 (sub x1 x2)))
              (B21 (mul 2 (sub y1 y2)))
              (C21 (mul 2 (sub (mul s1 r1) (mul s2 r2))))
              (D21 (add (sub (mul x1 x1) (mul x2 x2))
                        (sub (mul y1 y1) (mul y2 y2))
                        (sub (mul r2 r2) (mul r1 r1))))
              (A31 (mul 2 (sub x1 x3)))
              (B31 (mul 2 (sub y1 y3)))
              (C31 (mul 2 (sub (mul s1 r1) (mul s3 r3))))
              (D31 (add (sub (mul x1 x1) (mul x3 x3))
                        (sub (mul y1 y1) (mul y3 y3))
                        (sub (mul r3 r3) (mul r1 r1)))))
          ; Cramer 2x2: x(r) = x0 + xr*r,  y(r) = y0 + yr*r
          (let ((deter (sub (mul A21 B31) (mul A31 B21))))
            (when (> (abs deter) 1e-12)
              (let ((x0 (div (sub (mul D21 B31) (mul D31 B21)) deter))
                    (xr (div (sub (mul C31 B21) (mul C21 B31)) deter))
                    (y0 (div (sub (mul A21 D31) (mul A31 D21)) deter))
                    (yr (div (sub (mul A31 C21) (mul A21 C31)) deter)))
                ; Sostituisce in eq(1): P*r^2 + 2*Q*r + R = 0
                (let ((dx0 (sub x0 x1))
                      (dy0 (sub y0 y1)))
                  (let ((P (sub (add (mul xr xr) (mul yr yr)) 1))
                        (Q (sub (add (mul dx0 xr) (mul dy0 yr)) (mul s1 r1)))
                        (R (sub (add (mul dx0 dx0) (mul dy0 dy0)) (mul r1 r1))))
                    (if (> (abs P) 1e-12)
                      ; Caso generale: due radici
                      (let ((delta (sub (mul Q Q) (mul P R))))
                        (when (>= delta 0)
                          (let ((sq (sqrt delta)))
                            (dolist (sign '(1 -1))
                              (let ((r (div (add (sub Q) (mul sign sq)) P)))
                                (when (>= r -1e-9)
                                  (push (list (add x0 (mul xr (max 0 r)))
                                              (add y0 (mul yr (max 0 r)))
                                              (max 0 r))
                                        solutions -1)))))))
                      ; Caso degenere P=0: una radice
                      (when (> (abs Q) 1e-12)
                        (let ((r (div (sub R) (mul 2 Q))))
                          (when (>= r -1e-9)
                            (push (list (add x0 (mul xr (max 0 r)))
                                        (add y0 (mul yr (max 0 r)))
                                        (max 0 r))
                                  solutions -1))))))))))))
    ; Rimuove duplicati (entro tolleranza 1e-6)
    (remove-duplicates solutions 1e-6))))

; Rimuove soluzioni duplicate entro una tolleranza eps
(define (remove-duplicates sols eps)
  (let ((result '()))
    (dolist (s sols)
      (unless (exists (fn (u) (and (< (abs (sub (u 0) (s 0))) eps)
                                   (< (abs (sub (u 1) (s 1))) eps)
                                   (< (abs (sub (u 2) (s 2))) eps))) result)
        (push s result -1)))
    result))

(define (print-solutions sols)
  (if (null? sols)
    (println "Nessuna soluzione reale trovata.")
    (begin
      (println (format "Trovate %d soluzioni:" (length sols)))
      (let ((i 1))
        (dolist (s sols)
          (println (format "  [%d]  centro=(%.6f, %.6f)  raggio=%.6f"
                           i (s 0) (s 1) (s 2)))
          (++ i))))))

(define (verify sol c1 c2 c3)
  (let ((xc (sol 0)) (yc (sol 1)) (rc (sol 2)) (ok true))
    (dolist (c (list c1 c2 c3))
      (let ((d    (sqrt (add (mul (sub xc (c 0)) (sub xc (c 0)))
                             (mul (sub yc (c 1)) (sub yc (c 1))))))
            (ext  (add rc (c 2)))
            (intern  (abs (sub rc (c 2)))))
        (let ((tang (or (< (abs (sub d ext)) 1e-4)
                        (< (abs (sub d intern)) 1e-4))))
          (unless tang (setq ok nil))
          (println (format "    C%d: dist=%.6f  ext=%.6f  int=%.6f  %s"
                           (+ $idx 1) d ext intern
                           (if tang "OK" "FAIL"))))))
    ok))

(define (test name c1 c2 c3)
  (println (format "\n=== %s ===" name))
  (let ((sols (apollonius c1 c2 c3)))
    (print-solutions sols)
    (let ((all-ok true))
      (dolist (s sols)
        (println (format "Sol [%d]:" (+ $idx 1)))
        (unless (verify s c1 c2 c3) (setq all-ok nil)))
      (println (if all-ok ">>> Tutte le soluzioni verificate OK"
                          ">>> ATTENZIONE: alcune soluzioni falliscono")))))

Proviamo:

(test "C1=(0,0,1) C2=(4,0,1) C3=(2,3,1)"
      '(0 0 1) '(4 0 1) '(2 3 1))

(test "C1=(10,0,1) C2=(13,0,1) C3=(18,15,2)"
      '(10 0 1) '(13 0 1) '(18 15 2))

(test "Cerchi concentrici-like C1=(0,0,3) C2=(10,0,2) C3=(5,8,1)"
      '(0 0 3) '(10 0 2) '(5 8 1))
;-> === Cerchi concentrici-like C1=(0,0,3) C2=(10,0,2) C3=(5,8,1) ===
;-> Trovate 8 soluzioni:
;->   [1]  centro=(5.608679, 3.453774)  raggio=3.586792
;->   [2]  centro=(5.687531, 4.695450)  raggio=4.375315
;->   [3]  centro=(7.873373, 2.453328)  raggio=5.246746
;->   [4]  centro=(8.386855, 3.957571)  raggio=6.273711
;->   [5]  centro=(2.318063, 1.681774)  raggio=5.863874
;->   [6]  centro=(1.891025, 3.201122)  raggio=6.717951
;->   [7]  centro=(4.497364, -0.511533)  raggio=7.526361
;->   [8]  centro=(4.482141, 1.341514)  raggio=7.678594
;-> Sol [1]:
;->     C1: dist=6.586792  ext=6.586792  int=0.586792  OK
;->     C2: dist=5.586792  ext=5.586792  int=1.586792  OK
;->     C3: dist=4.586792  ext=4.586792  int=2.586792  OK
;-> Sol [2]:
;->     C1: dist=7.375315  ext=7.375315  int=1.375315  OK
;->     C2: dist=6.375315  ext=6.375315  int=2.375315  OK
;->     C3: dist=3.375315  ext=5.375315  int=3.375315  OK
;-> Sol [3]:
;->     C1: dist=8.246746  ext=8.246746  int=2.246746  OK
;->     C2: dist=3.246746  ext=7.246746  int=3.246746  OK
;->     C3: dist=6.246746  ext=6.246746  int=4.246746  OK
;-> Sol [4]:
;->     C1: dist=9.273711  ext=9.273711  int=3.273711  OK
;->     C2: dist=4.273711  ext=8.273711  int=4.273711  OK
;->     C3: dist=5.273711  ext=7.273711  int=5.273711  OK
;-> Sol [5]:
;->     C1: dist=2.863874  ext=8.863874  int=2.863874  OK
;->     C2: dist=7.863874  ext=7.863874  int=3.863874  OK
;->     C3: dist=6.863874  ext=6.863874  int=4.863874  OK
;-> Sol [6]:
;->     C1: dist=3.717951  ext=9.717951  int=3.717951  OK
;->     C2: dist=8.717951  ext=8.717951  int=4.717951  OK
;->     C3: dist=5.717951  ext=7.717951  int=5.717951  OK
;-> Sol [7]:
;->     C1: dist=4.526361  ext=10.526361  int=4.526361  OK
;->     C2: dist=5.526361  ext=9.526361  int=5.526361  OK
;->     C3: dist=8.526361  ext=8.526361  int=6.526361  OK
;-> Sol [8]:
;->     C1: dist=4.678594  ext=10.678594  int=4.678594  OK
;->     C2: dist=5.678594  ext=9.678594  int=5.678594  OK
;->     C3: dist=6.678594  ext=8.678594  int=6.678594  OK
;-> >>> Tutte le soluzioni verificate OK


---------------------------------------------------
Riempimento simmetrico di una griglia lineare (1xN)
---------------------------------------------------

Abbiamo, una griglia rettilinea lunga N (cioè 1xN).
Vogliamo inserire nella griglia dei segni partendo dal centro e in modo simmetrico.
Ogni segno ha una data larghezza (S celle).
Vincolo: la larghezza del segno S e la lunghezza della griglia N hanno la stessa parità (sono entrambe pari o entrambe dispari).
Inoltre, lo spazio tra i segni è essere sempre maggiore di 0.

Esempio:
  Lunghezza Griglia N = 15
  Larghezza Segno -> S = 1
  Distanza tra i segni = 2 (simmetria)

                             Centro
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |   | S |   |   | S |   |   | S |   |   | S |   |   | S |   |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     0   1   2   3   4   5   6   7   8   9  10  11  12  13  14

  Distanza tra i segni = 3 (simmetria)

Dati N e S, scriviamo una funzione che genera tutti i possibili modi di inserire i segni nella griglia.

(define (configurazioni N S)
  ; Restituisce tutte le configurazioni simmetriche di segni su una griglia 1xN (celle 0..N-1)
  ; Ogni segno ha larghezza S ed è rappresentato come intervallo (start end)
  ; Vincolo: N e S devono avere la stessa parità (altrimenti non esiste un centro discreto coerente)
  (let (res '())
    (if (!= (% N 2) (% S 2)) nil
      (let (c (/ N 2)) ; centro discreto della griglia (indice intero)
        (let (s0 (- c (/ S 2))) ; inizio del segno centrale (sempre intero grazie alla parità)
          (for (d 1 N) ; d = spazio tra segni (>=1), genera tutte le configurazioni possibili
            (let (step (+ S d)) ; distanza tra inizi di segni consecutivi
              (letn (
                     ; calcolo diretto dei limiti per k tale che i segni restino dentro [0, N-S]
                     ; s(k) = s0 + k*step deve soddisfare: 0 <= s(k) <= N-S
                     kmin (ceil (/ (- s0) step))  ; limite inferiore per k
                     kmax (floor (/ (- (- N S) s0) step))  ; limite superiore per k
                    )
                ;(println "kmin: " kmin ", kmax: " kmax)
                (if (<= kmin kmax) ; esiste almeno un segno valido
                  (let (conf '()) ; costruisce una configurazione per questo valore di d
                    (for (k kmin kmax) ; genera tutti i segni simmetrici rispetto al centro
                      (let (start (+ s0 (* k step))) ; posizione iniziale del segno
                        (push (list start (- (+ start S) 1)) conf -1))) ; intervallo [start, start+S-1]
                        (push conf res -1)))))))) ; aggiunge la configurazione alla lista dei risultati
    (unique res))))

Proviamo:

(configurazioni 5 1)
;-> (((0 0) (2 2) (4 4)) ((2 2)))
(configurazioni 15 1)
;-> (((1 1) (3 3) (5 5) (7 7) (9 9) (11 11) (13 13))
;->  ((1 1) (4 4) (7 7) (10 10) (13 13))
;->  ((3 3) (7 7) (11 11))
;->  ((2 2) (7 7) (12 12))
;->  ((1 1) (7 7) (13 13))
;->  ((0 0) (7 7) (14 14))
;->  ((7 7)))
(configurazioni 15 3)
;-> (((2 4) (6 8) (10 12))
;->  ((1 3) (6 8) (11 13))
;->  ((0 2) (6 8) (12 14))
;->  ((6 8)))
(configurazioni 13 9)
;-> (((2 10)))
(configurazioni 16 2)
;-> (((1 2) (4 5) (7 8) (10 11) (13 14))
;->  ((3 4) (7 8) (11 12)) ((2 3) (7 8) (12 13))
;->  ((1 2) (7 8) (13 14))
;->  ((0 1) (7 8) (14 15))
;->  ((7 8)))
(configurazioni 16 4)
;-> (((1 4) (6 9) (11 14))
;->  ((0 3) (6 9) (12 15)) ((6 9)))

Adesso scriviamo una funzione che stampa il grafico di una configurazione.
Esempio:
                             Centro
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
  |   | S |   |   | S |   |   | S |   |   | S |   |   | S |   |
  +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
     0   1   2   3   4   5   6   7   8   9  10  11  12  13  14

; Funzione che stampa graficamente una configurazione
; di segni su una griglia 1xN
(define (mostra-config N conf)
  ; conf = lista di intervalli ((a b) (c d) ...)
  ; Esempio:
  ; ((3 3) (7 7) (11 11))
  (let (grid (dup " " N))
    ; inserisce i segni nella griglia
    (dolist (segno conf)
      (for (i (segno 0) (segno 1))
        (setf (grid i) "S")))
    ; posizione centrata della scritta "Centro"
    (letn (
           larg (+ (* 4 N) 1)
           pos (- (/ larg 2) 3)
          )
      (println (dup " " pos) "Centro"))
    ; bordo superiore
    (print "+")
    (for (i 0 (- N 1))
      (print "---+"))
    (println)
    ; contenuto celle
    (print "|")
    (for (i 0 (- N 1))
      (print " " (grid i) " |"))
    (println)
    ; bordo inferiore
    (print "+")
    (for (i 0 (- N 1))
      (print "---+"))
    (println)
    ; numeri celle (0..N-1)
    (for (i 0 (- N 1))
      (print (format "%4d" i)))
    (println)))

Proviamo:

(setq c '((3 3) (7 7) (11 11)))
(mostra-config 15 c)
                           Centro
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |   |   | S |   |   |   | S |   |   |   | S |   |   |   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14

(dolist (c (configurazioni 15 1))
  (mostra-config 15 c)
  (println))
;->                            Centro
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;-> |   | S |   | S |   | S |   | S |   | S |   | S |   | S |   |
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;->    0   1   2   3   4   5   6   7   8   9  10  11  12  13  14
;-> 
;->                            Centro
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;-> |   | S |   |   | S |   |   | S |   |   | S |   |   | S |   |
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;->    0   1   2   3   4   5   6   7   8   9  10  11  12  13  14
;-> ...
;-> ...
;->                            Centro
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;-> |   |   |   |   |   |   |   | S |   |   |   |   |   |   |   |
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;->    0   1   2   3   4   5   6   7   8   9  10  11  12  13  14

(dolist (c (configurazioni 13 9))
  (mostra-config 13 c)
  (println))
;->                        Centro
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+
;-> |   |   | S | S | S | S | S | S | S | S | S |   |   |
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+
;->    0   1   2   3   4   5   6   7   8   9  10  11  12

(dolist (c (configurazioni 16 2))
  (mostra-config 16 c)
  (println))
;->                              Centro
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;-> |   | S | S |   | S | S |   | S | S |   | S | S |   | S | S |   |
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;->    1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
;-> 
;->                              Centro
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;-> |   |   |   | S | S |   |   | S | S |   |   | S | S |   |   |   |
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;->    1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
;-> ...
;-> ...
;->                              Centro
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;-> |   |   |   |   |   |   |   | S | S |   |   |   |   |   |   |   |
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;->    1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16

(dolist (c (configurazioni 16 4))
  (mostra-config 16 c)
  (println))
;->                              Centro
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;-> |   | S | S | S | S |   | S | S | S | S |   | S | S | S | S |   |
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;->    0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
;-> 
;->                              Centro
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;-> | S | S | S | S |   |   | S | S | S | S |   |   | S | S | S | S |
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;->    0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
;-> 
;->                              Centro
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;-> |   |   |   |   |   |   | S | S | S | S |   |   |   |   |   |   |
;-> +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
;->    0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15


--------------------------------------
Liste che differiscono per un elemento
--------------------------------------

Abbiamo una lista di elementi e poi un'altra lista degli stessi elementi, tranne uno.
Quale elemento è stato rimosso?

Se nelle liste non esistono elementi ripetuti (cioè le liste sono 'set') allora possiamo usare la primitiva 'difference'.

(define (absent1 lst1 lst2)
  (difference lst1 lst2))

(setq L1 '(1 4 3 8 5 2))
(setq L2 '(1 4   8 5 2))
(absent1 L1 L2)
;-> (3)

Se nelle liste esistono elementi ripetuti, allora 'difference' fallisce:

(setq L1 '(1 4 3 8 5 1 3 2))
(setq L2 '(1 4   8 5 1 3 2))
(absent1 L1 L2)
;-> ()

Se la lista contiene solo numeri interi, allora il numero mancante è dato dalla differenza tra la somma dei numeri della prima lista e la somma dei numeri della seconda lista:

  x = Sum(lst1) - Sum(lst2))

(define (absent2 lst1 lst2)
  (- (apply + lst1) (apply + lst2)))

(setq L1 '(1 4 3 8 5 1 3 2))
(setq L2 '(1 4   8 5 1 3 2))
(absent2 L1 L2)
;-> 3

Questo metodo potrebbe fallire nel caso di numeri float a causa di arrotondamenti durante le operazioni.

Se la lista contiene solo caratteri (stringhe di lunghezza 1), allora possiamo usare lo stesso metodo convertendo i caratteri nei rispettivi codici ASCII:

(define (absent3 lst1 lst2)
  (char (- (apply + (map char lst1)) (apply + (map char lst2)))))

(setq L1 '("a" "c" "h" "k" "c" "a" "z"))
(setq L2 '("a" "c" "h" "k" "c"     "z"))
(absent3 L1 L2)
;-> "a"

Se le liste contengono elementi diversi (es. interi, float e stringhe), allora dobbiamo usare un altro metodo.

Algoritmo
1) contare le occorrenze nella lista corta
2) consumare le occorrenze scorrendo la lista lunga
3) il primo elemento senza frequenza disponibile è quello mancante
Funziona anche con simboli, liste annidate e valori eterogenei.

(define (absent4 lst1 lst2)
  (let ( (freq '()) (out nil) (stop nil) (res nil) )
    ; ciclo per creare la lista delle frequenze (elemento occorrenze)
    ; della seconda lista (quella con elemento mancante)
    (dolist (el lst2)
      (if (lookup el freq)
          ; se elemento esiste, allora aumenta la frequenza
          (++ (lookup el freq))
          ;else
          ; altrimenti inserisce l'elemento nella lista 'freq'
          ; con frequenza 1
          (push (list el 1) freq -1)))
    ; ciclo per ogni elemento della prima lista...
    (setq stop nil)
    (dolist (el lst1 stop)
      (setq res (lookup el freq))
      (cond
          ; se l'elemento corrente non esiste,
          ; allora è l'elemento mancante
          ((nil? res)
            (setq out el) 
            (setq stop true))
          ; se la frequenza dell'elemento corrente vale 0,
          ; allora è l'elemento mancante
          ((zero? res)
            (setq out el) 
            (setq stop true))          
          (true ; altrimenti diminuisce la frequenza
            (-- (lookup el freq)))))
    out))

Proviamo:

(setq L1 '("a" "c" "h" "k" "c" "a" "z"))
(setq L2 '("a" "c" "h" "k" "c"     "z"))
(absent4 L1 L2)
;-> "a"

(setq L1 '("a" 1.2345 "ha" "k1" c 'a 2.89 "z"))
(setq L2 '("a" 1.2345 "ha" "k1" c 'a      "z"))
(absent4 L1 L2)
;-> 2.89


------------------
Coppia di formiche
------------------

Una coppia di specie di formiche appare ogni 'a' e 'b' anni, ed è stata osservata l'ultima volta nell'anno 'y'.
Trovare il prossimo anno in cui appariranno di nuovo insieme.

Algoritmo
Per ogni anno z = y + 1, y + 2, y + 3, ...
  Verificare se z - y è divisibile sia per a che per b.
  Interrompere quando viene trovato il primo z di questo tipo.
Questo processo continua al massimo per a*b passaggi, poiché a*b divide sicuramente sia a che b.

(define (next-year a b y)
  (let ( (out nil) (stop nil) )
    (for (z (+ y 1) (+ y (* a b)) 1 stop)
      (if (and (zero? (% (- z y) a)) (zero? (% (- z y) b)))
        (set 'out z 'stop true)))
    out))

(next-year 10 13 1980)
;-> 2110
(next-year 2 4 1980)
;-> 1984

In altre parole, le due specie ricompaiono insieme ogni: mcm(a,b)

; Funzione che calcola il Minimo Comune Multiplo di due numeri interi
(define (mcm a b) (/ (* a b) (gcd a b)))

(define (next-year a b y) (+ y (mcm a b)))

(next-year 10 13 1980)
;-> 2110
(next-year 2 4 1980)
;-> 1984


---------------------------
Rimozione di pile di monete
---------------------------

Abbiamo N pile ognuna contenente un certo numero di monete.
Possiamo rimuovere tutte le monete prendendone una da due pile distinte a ogni mossa?

È possibile rimuovere tutte le monete se:
1) il numero di monete è pari e
2) nessuna pila contiene più monete di tutte le altre pile messe insieme.

Metodo: scegliere sempre le due pile con il maggior numero di monete.
Con questa strategia se 1) e 2) sono valide prima di una tale mossa, lo sono anche dopo.

Rappresentiamo le pile con una lista, es. P = (3 5 2 7 1).

(define (svuota-pile P)
  (let ( (max-val (apply max P))
         (somma (apply + P)) )
    (cond ((odd? somma)
            (println "Impossibile: numero dispari di monete"))
          ((> max-val (- somma max-val))
            (println "Impossibile: esiste una pila troppo grande"))
          (true
            (until (for-all zero? P)
              (sort P >)
              (println (P 0) " - 1 = " (- (P 0) 1))
              (println (P 1) " - 1 = " (- (P 1) 1))
              (-- (P 0))
              (-- (P 1))
              (println P)))) '>))

Proviamo:

(svuota-pile '(1 2 3 4 5))
;-> Impossibile: numero dispari di monete

(svuota-pile '(1 2 3 4 12))
;-> Impossibile: esiste una pila troppo grande

(setq lst '(3 5 2 7 1))
(rimuove-pile lst)
;-> 7 - 1 = 6
;-> 5 - 1 = 4
;-> (6 4 3 2 1)
;-> 6 - 1 = 5
;-> 4 - 1 = 3
;-> (5 3 3 2 1)
;-> 5 - 1 = 4
;-> 3 - 1 = 2
;-> (4 2 3 2 1)
;-> 4 - 1 = 3
;-> 3 - 1 = 2
;-> (3 2 2 2 1)
;-> 3 - 1 = 2
;-> 2 - 1 = 1
;-> (2 1 2 2 1)
;-> 2 - 1 = 1
;-> 2 - 1 = 1
;-> (1 1 2 1 1)
;-> 2 - 1 = 1
;-> 1 - 1 = 0
;-> (1 0 1 1 1)
;-> 1 - 1 = 0
;-> 1 - 1 = 0
;-> (0 0 1 1 0)
;-> 1 - 1 = 0
;-> 1 - 1 = 0
;-> (0 0 0 0 0)


----------------------------------------------------
Ordinamento tramite confronto delle cifre dei numeri
----------------------------------------------------

Ordinare una lista di numeri interi con il seguente metodo:
1) due numeri 'a' e 'b' sono uguali solo se a == b.
2) 'a' è maggiore di 'b' se lenght(a) > lenght(b).
   'a' è maggiore di 'b' se nel confronto delle cifre ha più cifre maggiori.
   Se il confronto delle cifre è pari, allora verificare se a > b.
3) 'a' è minore di 'b' se lenght(a) < lenght(b).
   'a' è minore di 'b' se nel confronto delle cifre ha più cifre minori.
    Se il confronto delle cifre è pari, allora verificare se a < b.

Confronto delle cifre
---------------------
Esempio: a = 237, b = 189
Si confrontano le cifre che hanno la stessa posizione:
  2 > 1
  3 < 8
  7 < 9
Abbiamo una cifra maggiore (2) e due cifre minori (3 e 7), quindi 237 è minore di 189.

Scriviamo una funzione che prende due liste di interi e calcola quanti elementi della prima lista sono maggiori, quanti minori e quanti uguali confrontando le coppie di numeri con lo stesso indice.

(define (confronta L1 L2)
  (let ( (maggiori 0) (uguali 0) (minori 0) (diff '()) )
    (setq diff (map - L1 L2))
    (dolist (el diff)
      (if (= el 0) (++ uguali)
          (> el 0) (++ maggiori)
          (< el 0) (++ minori)))
    (list maggiori uguali minori)))

Proviamo:

(confronta '(2 3 7) '(1 8 9))
;-> (1 0 2)
(confronta '(3 4 6) '(3 7 1))
;-> (1 1 1)
(confronta (sort (rand 10 12)) (sort (rand 10 12)))
;-> (10 1 1)

(define (int-list num)
"Convert an integer to a list of digits"
  (if (zero? num) '(0)
  (let (out '())
    (while (!= num 0)
      (push (% num 10) out)
      (setq num (/ num 10))) out)))

(confronta (int-list 237) (int-list 189))
;-> (1 0 2)

Adesso scriviamo una funzione che confronta due numeri con questo metodo.

(define (comp op a b)
  (cond ((= op =) (= a b))
        ((= op >)
          (if (= (length a) (length b))
              (let ( (out nil) (res (confronta (int-list a) (int-list b))) )
                ;(println res)
                (cond ((> (res 0) (res 2)) (setq out true))
                      ((= (res 0) (res 2))
                        (if (>= a b) (setq out true))))
                out)
              ;else
              (if (> (length a) (length b)) true nil)))
        ((= op <)
          (if (= (length a) (length b))
              (let ( (out nil) (res (confronta (int-list a) (int-list b))) )
                ;(println res)
                (cond ((< (res 0) (res 2)) (setq out true))
                      ((= (res 0) (res 2))
                        (if (< a b) (setq out true))))
                out)
              ;else
              (if (< (length a) (length b)) true nil)))
        (true (println "Errore: operatore " op " sconosciuto."))))

Proviamo:

(comp = 222 222)
;-> true
(comp > 237 189)
;-> nil
(comp < 237 189)
;-> true
(comp > 123 321)
;-> nil
(comp < 123 321)
;-> true
(comp < 1234 321)
;-> nil
(comp > 1234 321)
;-> true

Adesso usiamo la funzione di ordinamento (bubble-sort):

(define (bubbleSort lst)
  (let ( (len (length lst)) (cambio true) )
    (while cambio
      (setq cambio nil)
      (for (i 1 (- len 1))
        (when (< (lst i) (lst (- i 1)))
               (swap (lst i) (lst (- i 1)))
               (setq cambio true)))
      (-- j))
  lst))

(bubbleSort (sequence 10 1))
;-> (1 2 3 4 5 6 7 8 9 10)
(bubbleSort '(2 3 6 1 7 9 4 3 7 8 2))
;-> (1 2 2 3 3 4 6 7 7 8 9)

Modifichiamo 'bubbleSort' per ordinare con il nostro metodo:

(define (bubble lst)
  (let ( (len (length lst)) (cambio true) )
    (while cambio
      (setq cambio nil)
      (for (i 1 (- len 1))
        ;(when (< (lst i) (lst (- i 1)))     ; 'bubbleSort' originale
        (when (comp < (lst i) (lst (- i 1))) ; linea modificata
               (swap (lst i) (lst (- i 1)))
               (setq cambio true)))
      (-- j))
  lst))

Proviamo:

(bubble (sequence 10 1))
;-> (1 2 3 4 5 6 7 8 9 10)
(bubble '(2 3 6 1 7 9 4 3 7 8 2))
;-> (1 2 2 3 3 4 6 7 7 8 9)

I numeri da 0 a 199 hanno lo stesso ordinamento anche con questo metodo.
(bubble (sequence 0 199))
;-> (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
;->  27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
;-> ...
;->  167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184
;->  185 186 187 188 189 190 191 192 193 194 195 196 197 198 199)

Le differenze cominciano con i numeri che hanno almeno 3 cifre e sono maggiori di 199:

(setq a '(4123 389 426 70 50 73 409  569 911 612 198 1877 187 587 11111))
(bubble a)
;-> (50 70 73 426 389 409 612 911 187 198 569 587 4123 1877 11111)

Verifica che ogni elemento dell'output è minore o uguale al successivo:

(setq res (bubble a))
(for (i 0 (- (length res) 2))
  (if (comp > (res i) (res (+ i 1)))
      (println "Errore: " (res i) " " (res (+ i 1)))))
;-> nil

(bubble (rand 1e5 20))
;-> (3509 12302 36780 51701 42622 72191 47529 68224 10467 47172 31687
;->  92138 83468 34598 94933 54954 75585 66298 37498 84698)

La funzione è lenta (ma non è importante):

(time (bubble (sequence 1000 9999)))
;-> 14274.59


--------------------------------------------
Costruzione di un polinomio dalle sue radici
--------------------------------------------

Data una lista di numeri interi che rappresentano le soluzioni di un polinomio, generare i coefficienti del polinomio.

Esempio:
S = (1 -3)
Polinomio = (x - 1)*(x + 3) = x^2 + 3x - x - 3 = x^2 + 2x - 3

Se le radici (soluzioni) del polinomio sono:
  
  r1, r2, r3, ..., rn

allora il polinomio associato è:

  P(x) = (x - r1)(x - r2)...(x - rn)

Algoritmo
I coefficienti si ottengono espandendo il prodotto.
Si parte dal polinomio costante:

  1

e per ogni radice 'r' si moltiplica il polinomio corrente per:

  (x - r)

Esempio con radici (1 2 3):

  1
  (x - 1) => (1 -1)
  (x - 1)(x - 2) => x^2 - 3x + 2 => (1 -3 2)
  (x - 1)(x - 2)(x - 3) => x^3 - 6x^2 + 11x - 6 => (1 -6 11 -6)

; Funzione che ricostruisce un polinomio partendo dalla lista delle sue radici
(define (roots-poly roots)
  (let (poly '(1))
    ; ciclo per ogni radice...
    (dolist (r roots)
      (let (pl (dup 0 (+ (length poly) 1)))
        (for (i 0 (- (length poly) 1))
          ; termine * x
          (++ (pl i) (poly i))
          ; termine * (-r)
          (++ (pl (+ i 1)) (* -1 r (poly i))))
        (setq poly pl)))
    poly))

Proviamo:

(roots-poly '(1 -3))
;-> (1 2 -3)

(roots-poly '(1 2 3))
;-> (1 -6 11 -6)

(roots-poly '(2 2))
;-> (1 -4 4)

(roots-poly '(0 1))
;-> (1 -1 0)

La lista (a b c d) rappresenta a*x^3 + b*x^2 + c*x + d.
Per esempio, (1 -6 11 -6) significa x^3 - 6x^2 + 11x - 6.


-----------------------
Dividere o moltiplicare
-----------------------

Calcolare la seguente sequenza:

  a(1) = 1
  a(n) = a(n-1)*n,        se a(n-1) < n
  a(n) = floor(a(n-1)/n), se a(n-1) >= n

Sequenza OEIS A076039
Start with 1. Multiply or divide by n accordingly as a(n-1) is smaller or greater than n and then take the integer value (this is to ensure that a(n) > 0 for all n).
  1, 2, 6, 1, 5, 30, 4, 32, 3, 30, 2, 24, 1, 14, 210, 13, 221, 12, 228,
  11, 231, 10, 230, 9, 225, 8, 216, 7, 203, 6, 186, 5, 165, 4, 140, 3,
  111, 2, 78, 1, 41, 1722, 40, 1760, 39, 1794, 38, 1824, 37, 1850, 36,
  1872, 35, 1890, 34, 1904, 33, 1914, 32, 1920, 31, 1922, 30, 1920, ...

(define (seq limit)
  (let (a (array (+ limit 1) '(0)))
    (setf (a 1) 1)
    (for (n 2 limit)
      (if (< (a (- n 1)) n)
        (setf (a n) (* (a (- n 1)) n))
        (setf (a n) (int (div (a (- n 1)) n)))))
    a))

(seq 64)
;-> (0 1 2 6 1 5 30 4 32 3 30 2 24 1 14 210 13 221 12 228
;->  11 231 10 230 9 225 8 216 7 203 6 186 5 165 4 140 3
;->  111 2 78 1 41 1722 40 1760 39 1794 38 1824 37 1850 36
;->  1872 35 1890 34 1904 33 1914 32 1920 31 1922 30 1920)


--------------------------------
Somma delle cifre di una stringa
--------------------------------

Scrivere una funzione che somma tutte le cifre (0..9) contenute in una stringa di caratteri ASCII (32..126).
La funzione deve essere la più corta possibile.

Tutte le cifre vengono considerate come intero positivo.
Per esempio, la stringa str = "1abc -2 xyz3" contiene le cifre 1, 2 e 3, quindi la loro somma vale 6.
Il segno "-" in "-2" non viene considerato.

(char "0")
;-> 48
(char "9")
;-> 57

Metodo 1 (ciclo 'dostring')
---------------------------

(define (summa1 str)
  (let (tot 0)
    (dostring (ch str)
      (if (and (> ch 47) (< ch 58))
          (++ tot (- ch 48))))
    tot))

(summa1 "1abc -2 xyz3")
;-> 6
(setq s "1aba4.1xyz-10.4e-4qwe +10.4e1 last -1")
(summa1 s)
;-> 22

Metodo 2 (espressione regolare)
-------------------------------

(define (summa2 str)
  (apply + (map int (find-all {\d} str))))

(summa2 "1abc -2 xyz3")
;-> 6
(setq s "1aba4.1xyz-10.4e-4qwe +10.4e1 last -1")
(summa2 s)
;-> 22

Test di velocità

; Funzione che genera una stringa casuale in un intervallo di caratteri
; la funzione prende caratteri e/o codici ASCII dei caratteri
(define (rand-ascii len start-char end-char)
  (let ( (min-val (if (string? start-char) (char start-char) start-char))
         (max-val (if (string? end-char)   (char end-char) end-char))
       )
    (if (> min-val max-val) (swap min-val max-val))
    (join (collect (char (+ min-val (rand (+ (- max-val min-val) 1)))) len))))

(setq t (rand-ascii 2000 32 126))

(summa1 t)
;-> 952
(summa2 t)
;-> 952

(time (summa1 t) 1e4)
;-> 1562.179
(time (summa2 t) 1e4)
;-> 751.961

Versione code-golf (47 caratteri):
(define(f s)(apply +(map int(find-all {\d}s))))
(f t)
;-> 952


---------------------
Sequenza Look and Say
---------------------

Sequenza OEIS A045918:
Describe n. Also called the "Say What You See" or "Look and Say" sequence LS(n).
  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 1110, 21, 1112, 1113, 1114, 1115,
  1116, 1117, 1118, 1119, 1210, 1211, 22, 1213, 1214, 1215, 1216, 1217,
  1218, 1219, 1310, 1311, 1312, 23, 1314, 1315, 1316, 1317, 1318, 1319,
  1410, 1411, 1412, 1413, 24, 1415, 1416, 1417, ...

0 ha "uno 0", quindi a(0) = 10
23 ha "un 2, un 3", quindi a(23) = 1213.
121 ha "un 1, un 2, un 1", quindi a(121) = 111211
122 ha "un 1, due 2", quindi a(122) = 1122

(define (rle-encode lst)
"Encode list with Run Length Encoding"
  (local (palo conta out)
    (cond ((= lst '()) '())
          (true
           (setq out '())
           (setq palo (first lst))
           (setq conta 0)
           (dolist (el lst)
              ; se l'elemento è uguale al precedente aumentiamo il suo conteggio
              (if (= el palo) (++ conta)
                  ; altrimenti costruiamo la coppia (conta el) e la aggiungiamo al risultato
                  (begin (extend out (list (list conta palo)))
                         (setq conta 1)
                         (setq palo el)
                  )
              )
           )
           ; aggiungiamo l'ultima coppia di valori
           (extend out (list (list conta palo)))
           out))))

; Funzione che prende un intero e genera il relativo numero 'Look and Say'
(define (LS num)
  (local (digits res)
    ; converte il numero in lista di cifre
    (setq digits (map int (explode (string num))))
    ; rimuove l'eventuale nil (dovuto alla 'L' dei big-integer)
    (replace nil digits)
    ; calcola il numero 'Look and Say' (lista di cifre)
    (setq res (flat (rle-encode digits)))
    ; genera in numero intero (anche big-integer) 'Look and Say'
    ; (int 321478135781235789123579125)
    ; -> ERR: number out of range in function int
    (eval-string (join (map string res)))))

Proviamo:

(LS 10)
;-> 1110
(LS 121)
;-> 111211
(LS 122)
;-> 1122
(LS 3333)
;-> 43

(map LS (sequence 0 50))
;-> (10 11 12 13 14 15 16 17 18 19 1110 21 1112 1113 1114 1115
;->  1116 1117 1118 1119 1210 1211 22 1213 1214 1215 1216 1217
;->  1218 1219 1310 1311 1312 23 1314 1315 1316 1317 1318 1319
;->  1410 1411 1412 1413 24 1415 1416 1417 1418 1419 1510)

Versione stile lisp:

(define (LS1 num)
  (eval-string
    (join
      (map string
           (flat
             (rle-encode
               (map int
                    (explode
                      (replace "L" (string num) "")))))))))

(map LS1 (sequence 0 50))
;-> (10 11 12 13 14 15 16 17 18 19 1110 21 1112 1113 1114 1115
;->  1116 1117 1118 1119 1210 1211 22 1213 1214 1215 1216 1217
;->  1218 1219 1310 1311 1312 23 1314 1315 1316 1317 1318 1319
;->  1410 1411 1412 1413 24 1415 1416 1417 1418 1419 1510)


---------------------------------------
Sequenze conta cifre (Look All and Say)
---------------------------------------

Dato un numero intero applicare i passi seguenti:
1) contare quante volte appare ogni cifra del numero e 
   costruire una lista di coppie: (occorrenze-cifre cifra)
2) ordinare la lista in modo non decrescente sulle occorrenze e poi sulle cifre
3) creare un nuovo numero unendo tutti i numeri della lista ordinata

Esempio:
numero =  423220
1) lista di coppie (occorrenze-cifra cifra)
  ((1 0) (3 2) (1 3) (1 4))
Il numero contiene una cifra 0, tre cifre 2, una cifra 3 e 1 cifra 4.
2) ordinamento non decrescente sulle occorrenze e poi sulle cifre
  ((1 0) (1 3) (1 4) (3 2))
3) creazione nuovo numero
  ((1 0) (1 3) (1 4) (3 2)) --> 1 0 1 3 1 4 3 2 --> 10131432

Questo metodo applicato ai numeri naturali genera una sequenza 'Look All and Say'.

; Funzione che prende un intero e genera un nuovo numero conta cifre
; gestisce anche i big-integer
(define (conta num)
  (local (s unici coppie)
    ; converte il numero in lista di cifre
    (setq s (sort (map int (explode (string num)))))
    ; rimuove l'eventuale nil (dovuto alla 'L' dei big-integer)
    (replace nil s)
    ; cifre uniche
    (setq unici (unique s))
    ; crazione coppie e ordinamento
    (setq coppie (sort (map (fn(x y) (list x y)) (count unici s) unici)))
    ; costruzione nuovo numero
    (eval-string (join (map string (flat coppie))))))

Proviamo:

(conta 423220)
;-> 10131432

(conta 3184712348712345871234581273451283745L)
;-> 45525758616364

(conta 111111111111111111111111111111111111111111)
;-> 421

Se applichiamo la funzione 'conta' in modo iterativo partendo da un numero N, otteniamo una sequenza di numeri:

(series 423220 conta 10)
;-> (423220 10131432 1012142331 1014222341 1013243132 1014223133
;->  1014223133 1014223133 1014223133 1014223133)

(series 1 conta 15)
;-> (1 11 21 1112 1231 121321 132231 212223 111342 12131431 12142341
;->  13222431 14212332 14212332 14212332)

(series 2 conta 15)
;-> (2 12 1112 1231 121321 132231 212223 111342 12131431 12142341 13222431
;->  14212332 14212332 14212332 14212332)

Notiamo che le sequenze generate raggiungono un punto fisso, cioè un numero che rigenera se stesso:
(conta 14212332)
;-> 14212332

In altri casi le sequenze incontrano un ciclo, cioè un numero genera un nuovo numero già presente nella sequenza:
(series 94 conta 12)
;-> (94 1419 141921 12141931 1213141941 1213192451 131415192241
;->  131519222451 131419253241 151922232441 131519243142 151922232441)
                               ------------              ------------

Perchè partendo da qualsiasi intero positivo arriviamo sempre ad un ciclo (o a un punto fisso)?

Partendo da qualunque intero positivo, la sequenza deve inevitabilmente entrare in un ciclo (eventualmente di lunghezza 1).

L'idea fondamentale è che la funzione 'conta' riduce drasticamente lo spazio dei numeri possibili.
Sia N un numero con K cifre.
Dopo l'applicazione di 'conta':
- ci sono al massimo 10 coppie (occorrenze cifra)
- ogni cifra occupa 1 cifra decimale
- il conteggio di una cifra è al massimo K

Quindi il nuovo numero ha una dimensione molto più controllata del precedente.
Per esempio, 999999999999999999999999999999999999 diventa 369, perché ci sono 36 cifre 9.

Dopo poche iterazioni, tutti i numeri finiscono dentro un insieme finito di stati possibili.
A questo punto vale il principio dei cassetti:
- una sequenza infinita in un insieme finito deve ripetere un valore.
E siccome la trasformazione è deterministica abbiamo che:
se x(i) = x(j), allora tutta la sequenza successiva si ripete periodicamente, quindi si entra necessariamente in un ciclo.

Quindi per ogni intero positivo N, la successione N, conta(N), conta(conta(N)), ... è ultimamente periodica.
I cicli possono avere:
- lunghezza 1 (punti fissi)
- lunghezza > 1

Per esempio,
  14212332 è un punto fisso.
  151922232441 <-> 131519243142 è un ciclo di periodo 2.

Scriviamo una funzione che restituisce la sequenza di un numero fino al raggiugimento di un ciclo (o punto fisso):

(define (fixed-point num)
  (local (values cur fixed)
    ; lista dei valori della sequenza
    (setq values (list num))
    ; numero corrente
    (setq cur num)
    (setq fixed nil)
    ; ciclo di trasformazione del numero corrente fino
    ; a raggiungere un punto fisso o un ciclo
    (until fixed
      (setq cur (conta cur))
      ;(println values)
      ; verifica se il numero corrente è già presente nella sequenza
      (if (member cur values) (setq fixed true))
      ; inserisce il numero corrente nella lista
      (push cur values -1))
    values))

Proviamo:

(fixed-point 0)
;-> (0 10 1011 1031 101321 10121331 10122341 1013142231 1014222341
;->  1013243132 1014223133 1014223133)

(fixed-point 123)
;-> (123 111213 121341 12131431 12142341 13222431 14212332 14212332)

Calcoliamo la lunghezza delle sequenze partendo dai numeri 0..1000:

(map (fn(x) (length (fixed-point x))) (sequence 0 1000))
;-> (12 14 13 14 10 12 12 12 12 12 11 13 12 13 9 11 11 11 11 11 8 12 2
;->  ...
;->  15 16 16 16 15 16 15 16 15 15 16 16 16 8 8 14 13 12 15 16 16 11 10)


---------------------
Numeri con la cifra x
---------------------

Quanti sono i numeri di k cifre che hanno la cifra x?

Per esempio, quanti sono i numeri di 3 cifre che hanno la cifra 7?

Metodo 1 (ciclo for per ogni numero di k cifre)
-----------------------------------------------

(define (with1 k x)
  (let ((min-val (** 10 (- k 1)))
        (max-val (- (** 10 k) 1))
        (con 0)
        (d (string x))
       )
    (for (i min-val max-val)
      (when (find d (string i)) 
        ;(println i)
        (++ con)))
    con))

(with1 3 7)
;-> 252

(with1 3 0)
;-> 171

Metodo 2 (calcolo del totale dei numeri)
----------------------------------------
Calcolare quanti sono i numeri di k cifre (tutti).
Calcolare quanti sono i numeri di k cifre SENZA la cifra x (senza).
Numeri di k cifre CON la cifra x: con = tutti - senza

Per generare un numero di k cifre dobbiamo:
1) per la prima cifra possiamo scegliere 9 cifre (tutte meno lo 0)
2) per le altre (k - 1) cifre possiamo scegliere 10 cifre (0..9))
Quindi il totale dei numeri di k cifre vale:
  
  tutti = 9 * 10^(k-1)

Per generare un numero di k cifre SENZA la cifra x dobbiamo:
1) se x = 0, allora per la prima cifra possiamo scegliere 9 cifre (tutte meno la cifra x)
   se x <> 0, allora per la prima cifra possiamo scegliere 8 cifre (tutte meno lo 0 e la cifra x)
2) per le altre (k - 1) cifre possiamo scegliere 9 cifre (tutte meno la cifra x)
Quindi il totale dei numeri di k cifre senza la cifra x vale:

          9 * 9^(k-1),  se x = 0
  senza = 
          8 * 9^(k-1),  se x <> 0

Quindi il totale di numeri di k cifre CON la cifra x vale:

  con = tutti - senza

(define (with2 k x)
  (let ( (tutti (* 9 (pow 10 (- k 1))))
         (senza (if (zero? x) (pow 9 k) (* 8 (pow 9 (- k 1))))) )
    (- tutti senza)))

(with2 3 7)
;-> 252

(with1 3 0)
;-> 171

(for (k 1 5) (println "Cifre " k ": " (map (curry with1 k) '(0 1))))
;-> Cifre 1: (0 1)
;-> Cifre 2: (9 18)
;-> Cifre 3: (171 252)
;-> Cifre 4: (2439 3168)
;-> Cifre 5: (30951 37512)
I numeri di 3 cifre con la cifra 0 sono 171.
I numeri di 3 cifre con la cifra 1..9 sono 252.


------------------------------------------------------
L'Aritmetica di Treviso (Problema Signoria di Venezia)
------------------------------------------------------

Questo problema è tratto dal libro del Quattrocento "L'Aritmetica di Treviso", il più antico manuale di argomento matematico edito a stampa in Occidente.

Problema
Il Santo Padre ha mandato un corriere da Roma a Venezia, ordinandogli di raggiungere Venezia in 7 giorni.
La più illustre signoria di Venezia ha mandato un altro corriere a Roma e dovrebbe raggiungere Roma in 9 giorni.
Da Roma a Venezia ci sono 250 miglia.
I due corrieri hanno iniziato il loro viaggio contemporaneamente.
In quanti giorni i corrieri si incontreranno?
Quante miglia avranno percorso?

   v1                     v2
   -->                    <-
   ..............X..........
   <----- s1 ---> <-- s2 -->
   <---------- s ---------->

Spazio totale:

  s = 250

Tempo di percorrenza dei corrieri:

  t1 = 7
  t2 = 9

Velocità dei corrieri:

  v1 = s/t1
  v2 = s/t2

Spazio percorso dai corrieri quando si incontrano (al tempo tx):

  s1 = v1*tx
  s2 = v2*tx

Inoltre risulta:

  s = s1 + s2

Quindi al tempo tx deve risultare:

  s1/v1 = s2/v2
  s1/v1 = (s - s1)/v2

Ricaviamo s1:

  s1/v1 = s/v2 - s1/v2
  s1/v1 + s1/v2 = s/v2
  s1*(v1 + v2)/(v1 * v2) = s/v2

        s*(v1 * v2)
  s1 = --------------
        v2*(v1 + v2)

Calcoliamo s2:

  s2 = s - s1

Tempo di incontro dei corrieri:

  tx = s1/v1
  tx = s2/v2

Verifica del risultato:

 tx = s1/v1
 s2 = v2*tx deve essere uguale a s2 = s - s1

(setq s 250)
250
(setq v1 (div 250 7))
;-> 35.71428571428572
(setq v2 (div 250 9))
;-> 27.77777777777778
(setq s1 (div (mul s v1 v2) (mul v2 (add v1 v2))))
;-> 140.625
(setq tx (div s1 v1))
;-> 3.9375
(setq s2 (mul tx v2))
;-> 109.375
(setq s2 (sub s s1))
;-> 109.375


----------------------------------
Coppie con somme uguali con 4 dadi
----------------------------------

Lanciando quattro dadi a sei facce non truccati, qual è la probabilità di poter suddividere i dadi in due coppie, in modo che ciascuna coppia abbia la stessa somma?

Con 4 elementi (a, b, c, d) esistono solo 3 suddivisioni possibili:

1) (a,b) e (c,d)
2) (a,c) e (b,d)
3) (a,d) e (b,c)

Quindi basta controllare una delle seguenti condizioni:

  a + b == c + d
  a + c == b + d
  a + d == b + c

Se almeno una è vera, allora la suddivisione esiste.

Condizione unica:

  (a+b=c+d) OR (a+c=b+d) OR (a+d=b+c)

Equivalentemente:

  1) a + b + c + d deve essere pari e
  2) deve esistere una coppia con somma = totale / 2

Infatti, se (a + b) = (c + d), allora entrambe le somme valgono metà del totale.

; Funzione che simula il processo
(define (simula iter)
  (local (conta a b c d)
    (setq conta 0)
    ; ciclo di 'iter' simulazioni...
    (for (i 1 iter)
      ; generazione casuale del lancio di 4 dadi
      (setq a (+ (rand 6) 1))
      (setq b (+ (rand 6) 1))
      (setq c (+ (rand 6) 1))
      (setq d (+ (rand 6) 1))
      ; verifica esistenza di coppie con somme uguali
      (if (or (= (+ a b) (+ c d)) (= (+ a c) (+ b d)) (= (+ a d) (+ b c)))
          (++ conta)))
    (div conta iter)))

(simula 1e5)
;-> 0.25975
(simula 1e6)
;-> 0.259373
(simula 1e6)
;-> 0.259824
(simula 1e7)
;-> 0.2594451
(time (println (simula 1e8)))
;-> 0.25923368
;-> 46969.786

; Funzione che calcola la probabilità del processo
; generando tutte le possibili combinazioni
(define (calcola)
  (local (tot conta)
    ; numero totale di combinazioni possibili
    (setq tot (* 6 6 6 6))
    ; combinazioni favorevoli --> coppie con somme uguali
    (setq conta 0)
    ; generazione di ogni combinazione con 4 cicli for...
    (for (a 1 6)
      (for (b 1 6)
        (for (c 1 6)
          (for (d 1 6)
          ; verifica esistenza di coppie con somme uguali
            (if (or (= (+ a b) (+ c d)) (= (+ a c) (+ b d)) (= (+ a d) (+ b c)))
                (++ conta))))))
    (println "Totale combinazioni: " tot)
    (println "Combinazioni favorevoli: " conta)
    (println "Probabilità(somme uguali): " (mul 100 (div conta tot)))))

(calcola)
;-> Totale combinazioni: 1296
;-> Combinazioni favorevoli: 336
;-> Probabilità(somme uguali): 25.92592592592592
;-> 25.92592592592592


------------------------------------------
Sequenze di bit in una stringa (e ritorno)
------------------------------------------

Data una stringa di bit determinare quante sequenze esistono di "00", "01", "10", "11".
Le sequenze possono essere sovrapposte.

Esempio
stringa = "000101"
sequenze = 00        00        01        10        01   
          "000101" "000101" "000101" "000101" "000101"

Metodo 1 (iterativo)
--------------------

(define (coppie1 str)
  (let ( (len (length str))
         (b00 0) (b01 0) (b10 0) (b11 0)
         (cur "") )
    (for (i 0 (- len 2))
      (setq cur (string (str i) (str (+ i 1))))
      (if (= cur "00") (++ b00)
          (= cur "01") (++ b01)
          (= cur "10") (++ b10)
          (= cur "11") (++ b11)))
    (list b00 b01 b10 b11)))

(coppie1 "000101")
;-> (2 2 1 0)

(coppie1 "0001011010011011001")
;-> (4 6 5 3)

Metodo 2 (funzionale)
---------------------

(define (coppie2 str)
  (let (lst (explode str))
    (count '("00" "01" "10" "11") (map string (chop lst) (rest lst)))))

(coppie2 "000101")
;-> (2 2 1 0)

(coppie2 "0001011010011011001")
;-> (4 6 5 3)

Adesso vogliamo fare il contrario, cioè vogliamo ricostruire la stringa partendo dal numero delle sequenze.

Data una stringa binaria, definiamo:
  a = numero di "00"
  b = numero di "01"
  c = numero di "10"
  d = numero di "11"
dove le coppie sono sottostringhe consecutive e sovrapposte.

Esempio:
  stringa = "000101"
  coppie: 00 00 01 10 01
quindi:
  a=2 b=2 c=1 d=0

Ogni coppia rappresenta una transizione tra bit consecutivi:
  00: 0 -> 0
  01: 0 -> 1
  10: 1 -> 0
  11: 1 -> 1

Quindi il problema equivale a costruire un cammino che usa:
a archi 00, b archi 01, c archi 10, d archi 11

Algoritmo:
1. Da: x(x-1)/2 = numero_00 ricaviamo quanti zeri contiene la stringa.
2. Analogamente per gli uni.
3. Sapendo quanti 0 e 1 esistono il numero totale di coppie miste deve essere:
   01 + 10 = zeri * uni
4. A questo punto basta costruire una stringa che produca esattamente:
   - numero desiderato di 01
   - automaticamente il resto sarà 10.
   Costruzione greedy:
   - mettere uno 0 il più a sinistra possibile massimizza i contributi a 01
   - spostarlo verso destra diminuisce il numero di 01.

Condizioni di esistenza
Il sistema è consistente solo se:
  a = x(x-1)/2
  d = y(y-1)/2
ammettono soluzioni intere x,y.
e inoltre:
  b + c = x*y
dove: a=00, b=01, c=10, d=11
Queste condizioni sono anche sufficienti.

Esempio:
  00 = 3
  01 = 5
  10 = 1
  11 = 1
  Da: x(x-1)/2 = 3 --> x = 3
  Da: y(y-1)/2 = 1 --> y = 2
Verifica:
  x*y = 6 e infatti 01 + 10 = 5 + 1 = 6, quindi la stringa esiste.
Una soluzione: 00101

Casi degeneri
Il caso 00 = 0, produce x(x-1)/2 = 0 e ha due soluzioni:
- x=0
- x=1
stesso discorso per 11.

Questi casi sono importanti perché:
- la stringa deve essere non vuota
- se tutte le quantità sono 0, la risposta può essere "0" o "1"

Condizione di esistenza
Una stringa esiste se e solo se abs(b - c) <= 1
Infatti:
- ogni passaggio 0->1 deve quasi sempre essere compensato da un passaggio 1->0
- quindi i conteggi di 01 e 10 possono differire al massimo di 1

Scelta del bit iniziale
Caso 1: (b > c)
La stringa deve iniziare con 0 e terminare con 1

Caso 2: (c > b)
La stringa deve iniziare con 1 e terminare con 0

Caso 3: (b = c)
La stringa puo' iniziare indifferentemente con 0 o 1

Algoritmo greedy
----------------
Una volta scelto il bit corrente:
se il bit corrente e' 0:
- usare prima una coppia 00 se disponibile
- altrimenti usare una coppia 01
se il bit corrente e' 1:
- usare prima una coppia 11 se disponibile
- altrimenti usare una coppia 10

Ogni coppia consumata aggiunge un nuovo bit alla stringa.

La lunghezza finale vale sempre a + b + c + d + 1 perche' le coppie consecutive condividono un bit.

(define (coppie->str b00 b01 b10 b11)
  (local (out cur)
    ; verifica la condizione necessaria e sufficiente:
    ; il numero di transizioni 01 e 10 deve differire
    ; al massimo di 1
    (if (> (abs (- b01 b10)) 1)
        nil
        (begin
          ; scelta del bit iniziale:
          ; se 01 > 10 bisogna iniziare con 0
          ; se 10 > 01 bisogna iniziare con 1
          ; se sono uguali si preferisce:
          ; - 0 se esistono coppie 00
          ; - altrimenti 1
          (cond ((> b01 b10) (setq cur "0"))
                ((> b10 b01) (setq cur "1"))
                (true
                  (if (> b00 0)
                      (setq cur "0")
                      (setq cur "1"))))
          ; inizializza la stringa risultato
          ; con il primo bit
          (setq out cur)
          ; continua finche' esistono coppie da usare
          (while (or (> b00 0) (> b01 0)
                     (> b10 0) (> b11 0))
            (cond
              ; caso: bit corrente = 0
              ((= cur "0")
               (cond
                 ; usa preferibilmente una coppia 00
                 ; per rimanere nello stesso stato
                 ((> b00 0)
                  (-- b00)
                  (push "0" out -1)
                  (setq cur "0"))
                 ; altrimenti usa una coppia 01
                 ; passando allo stato 1
                 ((> b01 0)
                  (-- b01)
                  (push "1" out -1)
                  (setq cur "1"))
                 ; nessuna coppia disponibile:
                 ; costruzione impossibile
                 (true
                  (setq out nil)
                  (setq b00 0 b01 0 b10 0 b11 0))))
              ; caso: bit corrente = 1
              (true
               (cond
                 ; usa preferibilmente una coppia 11
                 ; per rimanere nello stesso stato
                 ((> b11 0)
                  (-- b11)
                  (push "1" out -1)
                  (setq cur "1"))
                 ; altrimenti usa una coppia 10
                 ; passando allo stato 0
                 ((> b10 0)
                  (-- b10)
                  (push "0" out -1)
                  (setq cur "0"))
                 ; nessuna coppia disponibile:
                 ; costruzione impossibile
                 (true
                  (setq out nil)
                  (setq b00 0 b01 0 b10 0 b11 0))))))
          ; verifica finale:
          ; tutte le coppie devono essere state consumate
          (if (and out
                   (= b00 0) (= b01 0)
                   (= b10 0) (= b11 0))
              out
              nil)))))

Proviamo:

(coppie->str 2 2 1 0)
;-> "000101"

(coppie1 (coppie->str 2 2 1 0))
;-> (2 2 1 0)

(coppie->str 4 6 5 3)
;-> "0000011110101010101"

(coppie1 (coppie->str 4 6 5 3))
;-> (4 6 5 3)

(coppie->str 0 4 1 0)
;-> nil

(coppie->str 1 0 0 1)
;-> nil

(coppie->str 0 0 0 0)
;-> "1"

; Funzione che verifica una stringa e una coppia
(define (check str i j k l)
  (letn ( (lst (explode str))
          (coppie (map string (chop lst) (rest lst)))
          (conta (count '("00" "01" "10" "11") coppie)) )
    (= conta (list i j k l))))

(check "0000011110101010101" 4 6 5 3)
;-> true


---------------
Staffetta 4x100
---------------

Dobbiamo selezionare i velocisti per una staffetta 4x100 metri.
Si potrebbe pensare che la squadra migliore sia semplicemente composta dai 4 velocisti più veloci sui 100 metri, ma c'è un dettaglio importante da tenere in considerazione: la partenza lanciata.
Nella seconda, terza e quarta frazione, il corridore è già in corsa quando viene passato il testimone.
Ciò significa che alcuni corridori – quelli con una fase di accelerazione lenta – possono ottenere prestazioni migliori in una staffetta se si trovano nella seconda, terza o quarta frazione.
Abbiamo a disposizione una lista di corridori tra cui scegliere.
Per ogni corridore sono forniti due tempi (partenza da fermo e lanciata):
- il tempo che impiegherebbe per correre la prima frazione (partenza da fermo)
- il tempo che impiegherebbe per correre le altre frazioni (partenza lanciata)
Un velocista può correre solo una frazione.
Determinare la squadra più veloce.

Se abbiamo pochi velocisti (al massimo 10) possiamo utilizzare il metodo brute-force calcolando i tempi di tutte le possibili formazioni (permutazioni) e trovare il tempo migliore.

Se abbiamo un numero maggiore di velocisti possiamo usare il seguente algoritmo:

1) Pre-ordinare i corridori in base al loro tempo di partenza lanciata.
2) Provare ogni corridore nella prima frazione,
   per ogni scelta, completare con i 3 corridori rimanenti
   che hanno il tempo di partenza lanciata più veloce.

Gli elementi della lista dei tempi hanno la seguente struttura:

  (numero tf tl)

dove: numero -> codice velocista (0..N-1)
      tf --> tempo partenza da fermo
      tl --> tempo partenza da lanciato

(define (staffetta lst)
  (local (out tmin fast cur-staf stop t)
    ; lista dei velocisti (soluzione)
    (setq out '())
    ; tempo minimo iniziale
    (setq tmin 999)
    ; lista dei velocisti ordinata dal più veloce con partenza lanciata
    (setq fast (sort (copy lst) (fn (x y) (<= (last x) (last y)))))
    ; ciclo per tutti i velocisti...
    (dolist (vel lst)
      ; staffetta corrente
      (setq cur-staf (list vel))
      (setq stop nil)
      ; aggiunge alla staffetta corrente i 3 velocisti
      ; che hanno tempi migliori nella partenza da lanciati
      (dolist (f fast stop)
        (if (!= (f 0) (vel 0)) (push f cur-staf -1))
        ; controllo staffetta corrente terminata
        (if (= (length cur-staf) 4) (setq stop true))
      )
      ; calcolo del tempo totale della staffetta corrente
      (setq t (add (cur-staf 0 1) (cur-staf 1 2) (cur-staf 2 2) (cur-staf 3 2)))
      ; confronto tra tempo minimo e tempo corrente
      ; ed eventuale aggiornamento della soluzione
      (when (< t tmin) (setq tmin t) (setq out cur-staf)))
    (println tmin)
    out))

Proviamo:

(setq L '((0 4.3 3.14) (1 4.3 3.15) (2 4.1 3.12) (3 4.2 3.17) (4 3.9 3.11)))
(staffetta L)
;-> 13.31
;-> ((4 3.9 3.11) (2 4.1 3.12) (0 4.3 3.14) (1 4.3 3.15))

(setq L '((0 4.3 3.14) (1 4.3 3.15) (2 4.1 3.12) (3 4.2 3.17) (4 3.9 2.0)))
(staffetta L)
;-> 12.39
;-> ((2 4.1 3.12) (4 3.9 2) (0 4.3 3.14) (1 4.3 3.15))


-------------------------------------
Aggiornare la posizione in classifica
-------------------------------------

In una gara di programmazione, N squadre risolvono M problemi.
Dopo ogni problema risolto da qualunque squadra, restituire la posizione in classifica della propria squadra.
I punteggi uguali sono tutti alla stessa posizione.
Per esempio:
  punteggi = (23 34 22 23 26 25 26 25)
  ordinamento =
  1) 34
  2) 26 26
  3)
  4) 25 25
  5)
  6) 23 23
  7)
  8) 22

Algoritmo
1) Mantenere una lista S delle squadre e relativo punteggio.
2) Calcolare quante squadre (K) hanno un punteggio maggiore del nostro.
   La nostra posizione in classifica è (K + 1).

(define (rank team solved)
  (local (teams punteggi punti pos)
    (setq teams (sort (unique solved)))
    (setq punteggi (map list teams (dup 0 (length teams))))
    (dolist (el solved)
      ; aumenta di 1 il punteggio del team corrente
      (++ (lookup el punteggi))
      ; punti del nostro team
      (setq punti (lookup team punteggi))
      ; posizione del nostro team
      (setq pos (+ (length (find-all punti (map last punteggi) $it <)) 1))
      (println "Punteggi: " punteggi)
      (println "Punti " team ": " punti)
      (println "Posizione " team ": " pos)
      (read-line)) '>))

Proviamo:

(setq solved '("A" "B" "C" "D" "A" "B" "B" "C" "D" "A" "B" "D" "D" "A"))
(rank "A" solved)
;-> Punteggi: (("A" 1) ("B" 0) ("C" 0) ("D" 0))
;-> Punti A: 1
;-> Posizione A: 1
;-> 
;-> Punteggi: (("A" 1) ("B" 1) ("C" 0) ("D" 0))
;-> Punti A: 1
;-> Posizione A: 1
;-> 
;-> ...
;-> 
;-> Punteggi: (("A" 2) ("B" 3) ("C" 1) ("D" 1))
;-> Punti A: 2
;-> Posizione A: 2
;-> 
;-> ...
;-> 
;-> Punteggi: (("A" 3) ("B" 4) ("C" 2) ("D" 4))
;-> Punti A: 3
;-> Posizione A: 3
;-> 
;-> Punteggi: (("A" 4) ("B" 4) ("C" 2) ("D" 4))
;-> Punti A: 4
;-> Posizione A: 1


----------------
Solitario del re
----------------

Regole del solitario:
- si usano 36 carte disposte in 4 file da 9 carte coperte
- le 4 carte rimanenti sono la 'mano iniziale'
- ogni carta scoperta determina la prossima posizione da aprire sulla griglia
- i Re chiudono la catena e vengono riciclati in fondo (nuova carta iniziale)
- lo scopo è scoprire tutta la griglia

Struttura dati:
  Semi: intero (0..3)
  Numeri: intero (0..9)
  Carte: lista (seme numero)

; Funzione che inizializza un solitario
(define (init)
  (let (m '())
    ; creazione mazzo di carte (lista 4x10)
    (for (r 0 3)
      (for (c 0 9)
        (push (list r c) m -1)))
    ; creazione griglia casuale 4x9 (36 carte)
    (setq g (array 4 9 (randomize m)))
    ; carte rimaste (4) (mano iniziale)
    (setq c4 (difference m (flat (array-list g) 1)))
    ; rimozione dei Re (? 9) dalla mano iniziale
    (while (find '(? 9) c4 match)
      (pop c4 (find '(? 9) c4 match)))
    ; carta iniziale corrente
    ; controlla se la mano iniziale era composta solo da Re
    (if c4 (setq cur (pop c4))
        ;else
        nil)))

(init)

; Funzione che stampa la griglia 4x9 (36 carte) e le carte iniziali
(define (print-sol grid carte corrente)
  (for (r 0 3)
    (for (c 0 8)
      (if (= (grid r c) (list r c))
          (print " !!!  ")
          (print (grid r c) " ")))
    (println))
  (println "Carte: " carte)
  (println "Corrente: " corrente) '>)

(print-sol g c4 cur)

; Funzione che controlla se tutte le carte sono al posto giusto
(define (check grid)
  (let (stop nil)
    (for (r 0 3 1 stop)
      (for (c 0 8 1 stop)
        (if (!= (grid r c) (list r c)) (setq stop true))))
    (not stop)))

(check g)

; Funzione che cambia la carta corrente con la relativa carta della griglia
; se dalla griglia pesca un Re,
; allora prende una carta dalla mano iniziale (se possibile)
; restituisce true se possibile, nil altrimenti
(define (exchange-current)
  (swap cur (g (cur 0) (cur 1)))
  ;se dalla griglia abbiamo pescato un 10...
  (if (= (cur 1) 9)
      ; allora, se esistono carte iniziali disponibili...
      (if c4 (setq cur (pop c4)) ; prendiamo la prima
                ; altrimenti restituisce nil (gioco terminato)
                nil)
      ; altrimenti restituisce true
      true))

; Funzione che gestisce il solitario
(define (solitaire)
  (local (g c4 cur sfiga finito)
    ; inizializza il gioco
    ; sfiga è quando la mano iniziale è composta da 4 Re.
    (if (nil? (init)) (setq sfiga true) (setq sfiga nil))
    ; stampa situazione corrente
    (print-sol g c4 cur)
    (setq finito nil)
    ; ciclo di scambi ...
    (until (or finito sfiga)
      ; scambio corrente possibile
      (if (nil? (exchange-current)) (setq finito true))
      ; stampa situazione corrente
      (print-sol g c4 cur)
      (read-line)
    )
    ; controllo solitario risolto
    (if (check g)
        (println "Bravo!!!")
        (println "Riprova, sarai più fortunato")) '>))

Proviamo:

(solitaire)

;-> (3 7) (0 8) (3 1) (2 6) (2 7) (3 6) (2 1) (0 0) (2 9)
;-> (1 3) (0 1) (0 4) (2 3) (3 8) (2 8) (1 0) (1 4) (2 2)
;-> (0 3) (1 8) (2 0) (0 5) (2 5) (2 4) (3 0) (3 3) (1 9)
;-> (1 5) (1 1) (0 6) (3 4) (1 6) (0 7) (0 2) (0 9) (1 7)
;-> Carte: ((3 2) (3 5))
;-> Corrente: (1 2)
;-> (3 7) (0 8) (3 1) (2 6) (2 7) (3 6) (2 1) (0 0) (2 9)
;-> (1 3) (0 1)  !!!  (2 3) (3 8) (2 8) (1 0) (1 4) (2 2)
;-> (0 3) (1 8) (2 0) (0 5) (2 5) (2 4) (3 0) (3 3) (1 9)
;-> (1 5) (1 1) (0 6) (3 4) (1 6) (0 7) (0 2) (0 9) (1 7)
;-> Carte: ((3 2) (3 5))
;-> Corrente: (0 4)
;->
;-> (3 7) (0 8) (3 1) (2 6)  !!!  (3 6) (2 1) (0 0) (2 9)
;-> (1 3) (0 1)  !!!  (2 3) (3 8) (2 8) (1 0) (1 4) (2 2)
;-> (0 3) (1 8) (2 0) (0 5) (2 5) (2 4) (3 0) (3 3) (1 9)
;-> (1 5) (1 1) (0 6) (3 4) (1 6) (0 7) (0 2) (0 9) (1 7)
;-> Carte: ((3 2) (3 5))
;-> Corrente: (2 7)
;->
;-> ...
;->
;-> (3 7)  !!!   !!!  (2 6)  !!!   !!!  (2 1) (0 0) (2 9)
;->  !!!   !!!   !!!   !!!  (3 8) (2 8)  !!!  (1 4) (2 2)
;-> (0 3) (1 8) (2 0)  !!!  (2 5) (2 4) (3 0)  !!!  (1 9)
;-> (1 5)  !!!  (0 6)  !!!   !!!  (0 7)  !!!  (0 9) (1 7)
;-> Carte: ((3 2) (3 5))
;-> Corrente: (0 8)
;->
;-> (3 7)  !!!   !!!  (2 6)  !!!   !!!  (2 1) (0 0)  !!!
;->  !!!   !!!   !!!   !!!  (3 8) (2 8)  !!!  (1 4) (2 2)
;-> (0 3) (1 8) (2 0)  !!!  (2 5) (2 4) (3 0)  !!!  (1 9)
;-> (1 5)  !!!  (0 6)  !!!   !!!  (0 7)  !!!  (0 9) (1 7)
;-> Carte: ((3 2) (3 5))
;-> Corrente: (2 9)
;->
;-> Riprova, sarai più fortunato

Il solitario non è realmente 'casuale'.
Una volta fissata la disposizione iniziale la partita è completamente deterministica.
Quindi o è risolvibile o non lo è fin dall'inizio.

Calcoliamo la probabilità di riuscita del solitario simulando un certo numero di partite.

; Funzione che simula un solitario casuale (Metodo di Monte Carlo)
; restituisce true se il solitario viene risolto, nil altrimenti
(define (solitaire-prob)
  (local (g c4 cur sfiga finito)
    ; inizializza il gioco
    ; sfiga è quando la mano iniziale è composta da 4 Re.
    (if (nil? (init)) (setq sfiga true) (setq sfiga nil))
    (setq finito nil)
    ; ciclo di scambi ...
    (until (or finito sfiga)
      ; scambio corrente possibile
      (if (nil? (exchange-current)) (setq finito true))
    )
    ; controllo solitario risolto
    (if (check g) true nil)))

(solitaire-prob)
;->  nil

; Funzione che calcola la probabilità di riuscita del solitario
; con una simulazione di 'iter' partite
(define (prob iter)
  (let (sol 0)
    (for (i 1 iter)
      (if (solitaire-prob) (++ sol)))
    (div sol iter)))

(time (println (prob 1e5)))
;-> 0.25026
;-> 2500.484

(time (println (prob 1e6)))
;-> 0.25157
;-> 24899.407

(time (println (prob 1e7)))
;-> 0.2514447
;-> 248870.123

L'errore statistico di Monte Carlo è circa:

  sigma ~= sqrt(p*(1-p)/N)

Con p ~= 0.25 e N = 10^7 si ottiene:

  sigma ~= 1.4x10-4

quindi il risultato 0.2514447 è già molto accurato.

Esiste anche un metodo per vedere se il solitario e' risolvibile senza effettuare tutto il processo.

Il solitario è equivalente alla 'copertura totale di un grafo funzionale mediante 4 ingressi iniziali' che è un problema classico di teoria dei grafi e permutazioni.
Formalmente il gioco è una permutazione su 36 posizioni con:
- 4 punti di ingresso iniziali (le 4 carte fuori griglia)
- 4 stati speciali (i Re)
Quindi la risolvibilità può essere determinata analizzando la struttura dei cicli della permutazione, senza simulare tutte le mosse.

1) Interpretazione come funzione
Ogni posizione della griglia contiene una carta: (riga colonna)
La carta indica direttamente la prossima posizione da visitare.
Quindi ogni cella definisce una funzione: f(posizione) = nuova-posizione
Esempio:
  g[1][4] = (2 7)
significa:
  (1,4) -> (2,7)

2) Grafo funzionale
La griglia genera un grafo orientato dove ogni nodo ha uscita 1 e i Re sono nodi terminali speciali.
Quindi il grafo è composto da catene e cicli

3) Quando il solitario è risolvibile
Il solitario è risolvibile se e solo se:
a) ogni ciclo del grafo contiene almeno un Re
oppure
b) è raggiungibile da una delle 4 carte iniziali
Perchè se esiste un ciclo chiuso senza Re e ingressi iniziali, allora quel ciclo non verrà mai visitato.
Le carte del ciclo resteranno sempre errate.

4) Caso fondamentale
Supponiamo:
A -> B
B -> C
C -> A
e nessuna delle 4 carte iniziali punta a A,B,C
Allora il ciclo è isolato e non verrà mai attraversato, quindi il solitario è impossibile.

5) Interpretazione equivalente
Risolvere il solitario = visitare tutte le posizioni almeno una volta.
Quindi il problema diventa che le 4 carte iniziali + i Re devono raggiungere tutto il grafo.

6) Algoritmo
Passo 1
  Costruire il grafo:
  nodo = posizione
  arco = carta contenuta
Passo 2
  Marcare come sorgenti:
  - le 4 carte iniziali
  - i Re
Passo 3
  Fare DFS/BFS seguendo gli archi.
Passo 4
  Controllare se tutti i 36 nodi sono stati visitati:
  - sì -> solitario risolvibile
  - no -> impossibile


-----------------------------
Eliminazione di un istogramma
-----------------------------

Dato un istogramma e due mosse (rimuovi-riga, rimuovi-colonna):
trovare il numero minimo di mosse per svuotare l'intero istogramma.
L'istogramma viene rappresentato con una lista di numeri che sono l'altezza di ogni colonna.

Per minimizzare le mosse:
a) Se rimuoviamo una colonna, dobbiamo rimuovere quella più alta.
b) Se rimuoviamo una riga, dobbiamo rimuovere quella più bassa.

Quindi se rimuoviamo le X colonne più alte, il numero di righe rimanenti da rimuovere sarà pari all'altezza della colonna più alta rimasta: la (X + 1)-esima più alta.

Algoritmo
Se eliminiamo X colonne, conviene eliminare le X più alte.
Dopo averle eliminate, il numero minimo di righe da togliere è l'altezza massima rimasta.
Ordinando in ordine decrescente:
  h0 >= h1 >= h2 >= ...
se eliminiamo le prime X colonne, restano colonne con altezza massima hX.
Quindi il costo totale è:
- X + hX se 0 <= X < N
- N se eliminiamo tutte le colonne.
L'algoritmo quindi prova tutti i possibili valori di X e prende il minimo:
min( h0,
     1 + h1,
     2 + h2,
     ...
     (N-1) + h(N-1),
     N )
La complessità è O(N*log*N) (ordinamento: O(N*log*N) e scansione: O(N))

; Funzione che elimina un'istogramma con il minor numero di mosse
(define (clear histo)
  ; se le colonne sono tutte alte 1, allora basta eliminare una riga
  (if (for-all (fn(x) (= x 1)) histo)
    '(0 1 1)
  ;else
  (local (len min-mosse mosse sol)
    ; numero colonne dell'istogramma
    (setq len (length histo))
    ; numero minimo di moss
    (setq min-mosse 999999)
    ; ordina i valori dell'istogramma (decrescente)
    (sort histo >)
    ; se non togliamo alcuna colonna, allora il numero di mosse vale:
    ; max(histo)
    (when (> min-mosse (histo 0)) 
      (setq min-mosse (histo 0))
      (setq sol (list 0 (histo 0) (histo 0))))
    (println 0 { } (histo 0) { } (histo 0))
    ; se togliamo X colonne (da 1 a N-1), allora le righe da togliere
    ; sono il prossimo valore dopo quelli tolti.
    ; X = c + 1
    ; (X+1) = (histo (+ c 1))
    (for (c 0 (- len 2))
      (setq mosse (+ (+ c 1) (histo (+ c 1))))
      (println (+ c 1) { } (histo (+ c 1)) { } mosse)
      ; verifica se le mosse correnti sono il minimo
      (when (< mosse min-mosse)
        (setq min-mosse mosse)
        (setq sol (list (+ c 1) (histo (+ c 1)) mosse))))
    ; se togliamo tutte le colonne, allora il numero di mosse vale:
    ; length(histo)
    (when (> min-mosse len)
      (setq min-mosse len)
      (setq sol (list len 0 len)))
    (println len { } 0 { } len)
    sol)))

Proviamo:

(setq h '(2 2 2 2))
(clear h)
;-> 0 2 2  ; colonne tolte, righe tolte, mosse minime
;-> 1 2 3 
;-> 2 2 4
;-> 3 2 5
;-> 4 0 4
;-> (0 2 2)

(setq h '(4 4 4 4))
(clear h)
;-> 0 4 4
;-> 1 4 5
;-> 2 4 6
;-> 3 4 7
;-> 4 0 4
;-> (0 4 4)

(setq h '(5 5 2 2 7 3 5 4))
(clear h)
;-> 0 7 7
;-> 1 5 6
;-> 2 5 7
;-> 3 5 8
;-> 4 4 8
;-> 5 3 8
;-> 6 2 8
;-> 7 2 9
;-> 8 0 8
;-> (1 5 6)

(setq h '(15 5 12 2 6 11 10 10))
(clear h)
;-> 0 15 15
;-> 1 12 13
;-> 2 11 13
;-> 3 10 13
;-> 4 10 14
;-> 5 6 11
;-> 6 5 11
;-> 7 2 9
;-> 8 0 8
;-> (8 0 8)

(setq h '(1 1 1 2 2))
(clear h)
;-> 0 2 2
;-> 1 2 3
;-> 2 1 3
;-> 3 1 4
;-> 4 1 5
;-> 5 0 5
;-> (0 2 2)

(setq h '(1 1 1 1))
(clear h)
;-> (0 1 1)

(setq h '(0 0 0))
(clear h)
;-> 0 0 0
;-> 1 0 1
;-> 2 0 2
;-> 3 0 3
;-> (0 0 0)

(setq h '(1 0 2))
(clear h)
;-> 0 2 2
;-> 1 1 2
;-> 2 0 2
;-> 3 0 3
;-> (0 2 2)

Due funzioni per visualizzare l'istogramma.

; Funzione che trasforma l'istogramma in una matrice binaria
; ogni colonna della matrice rappresenta una colonna dell'istogramma
(define (make-matrix histo)
  (local (matrix max-col col)
    (setq matrix '())
    ; valore massimo dell' istogramma (colonna più alta)
    (setq max-col (apply max histo))
    ; crea la matrice  dell'istogramma (riga x riga)
    ; 1 -> valore pieno, 0 -> valore vuoto
    (dolist (el histo)
      (setq col (append (dup 0 (- max-col el)) (dup 1 el)))
      (push col matrix -1))
    ; traspone le righe della matrice in colonne
    (transpose matrix)))

(make-matrix h)

; Funzione che stampa una matrice binaria
(define (print-matrix matrix)
  (let ((row (length matrix))
        (col (length (matrix 0))))
    (for (r 0 (- row 1))
      (for (c 0 (- col 1))
        (print (matrix r c) " "))
        (println)) '>))

(setq h '(5 5 2 2 7 3 5 4))
(print-matrix (make-matrix h))
;-> 0 0 0 0 1 0 0 0
;-> 0 0 0 0 1 0 0 0
;-> 1 1 0 0 1 0 1 0
;-> 1 1 0 0 1 0 1 1
;-> 1 1 0 0 1 1 1 1
;-> 1 1 1 1 1 1 1 1
;-> 1 1 1 1 1 1 1 1
(print-matrix (make-matrix (sort h >)))
;-> 1 0 0 0 0 0 0 0
;-> 1 0 0 0 0 0 0 0
;-> 1 1 1 1 0 0 0 0
;-> 1 1 1 1 1 0 0 0
;-> 1 1 1 1 1 1 0 0
;-> 1 1 1 1 1 1 1 1
;-> 1 1 1 1 1 1 1 1


---------------
Piantare alberi
---------------

Abbiamo N piante che producono frutti dopo tempi diversi.
Vogliamo trovare i tempi di posa di ciascun albero in modo che tutti gli alberi producano frutta nel minor tempo possibile.
Possiamo posare (piantare) solo un albero al giorno.
I tempi sono calcolati in giorni.
Per esempio:
  N = 4
  tempi di produzione = (6 3 8 1)

Il modo ottimale di piantare gli alberi è in ordine decrescente di tempo di crescita.
Perchè se due alberi qualsiasi non sono in quest'ordine, scambiarli non può migliorare il risultato.

Algoritmo: ordinare gli alberi in base al tempo di crescita decrescente e calcolare il valore massimo di (posizione + tempo di crescita).

; ------------------------------------------------------------
; piantagione
; ------------------------------------------------------------
; Data una lista di tempi di crescita degli alberi,
; stampa:
;
; - l'ordine ottimale di piantagione
; - il giorno in cui ogni albero viene piantato
; - il giorno in cui produrrà frutti
; - il tempo minimo totale necessario
;
; Ipotesi:
; - ogni giorno può essere piantato un solo albero
; - un albero con tempo T produce frutti T giorni
;   dopo essere stato piantato
;
; Strategia ottimale:
; - piantare prima gli alberi con crescita più lenta
; - quindi ordinare i tempi in ordine decrescente
; ------------------------------------------------------------
;
(define (piantagione lst)
  (local (ordinata massimo giorno-produzione)
    ; ordina i tempi in ordine decrescente
    (setq ordinata (sort lst >))
    ; inizializza il massimo
    (setq massimo 0)
    (println "Ordine ottimale di piantagione:")
    (println ordinata)
    (println)
    ; per ogni albero:
    ; i = giorno di piantagione
    (for (i 0 (- (length ordinata) 1))
      ; giorno in cui l'albero produce frutti
      (setq giorno-produzione (+ i (ordinata i)))
      ; aggiorna il massimo
      (setq massimo (max massimo giorno-produzione))
      ; stampa informazioni
      (println "Albero con crescita "
               (ordinata i)
               " giorni")
      (println "  piantato al giorno: " i)
      (println "  produce frutti al giorno: "
               giorno-produzione)
      (println))
    (println "Tutti gli alberi produrranno")
    (println "frutti entro il giorno: "
             massimo)
    ; restituisce il valore finale
    massimo))

Proviamo:

(piantagione '(6 3 8 1))
;-> Ordine ottimale di piantagione:
;-> (8 6 3 1)
;-> 
;-> Albero con crescita 8 giorni
;->   piantato al giorno: 0
;->   produce frutti al giorno: 8
;-> 
;-> Albero con crescita 6 giorni
;->   piantato al giorno: 1
;->   produce frutti al giorno: 7
;-> 
;-> Albero con crescita 3 giorni
;->   piantato al giorno: 2
;->   produce frutti al giorno: 5
;-> 
;-> Albero con crescita 1 giorni
;->   piantato al giorno: 3
;->   produce frutti al giorno: 4
;-> 
;-> Tutti gli alberi produrranno
;-> frutti entro il giorno: 8


-------------------------------------------
Posizionamento di rettangoli in una griglia
-------------------------------------------

In una griglia MxN in quanti modi possiamo posizionare un rettangolo AxB.
Il rettangolo può essere ruotato di 90 gradi.

Esempio:

griglia 2x4
  +---+---+---+---+
  |   |   |   |   |
  +---+---+---+---+
  |   |   |   |   |
  +---+---+---+---+
  |   |   |   |   |
  +---+---+---+---+

rettangolo 2x3
  +---+---+---+
  |   |   |   |
  +---+---+---+
  |   |   |   |
  +---+---+---+

Numero posizioni = 7

+---+---+---+---+    +---+---+---+---+    +---+---+---+---+   +---+---+---+---+   
| * | * | * |   |    |   | * | * | * |    |   |   |   |   |   |   |   |   |   |
+---+---+---+---+    +---+---+---+---+    +---+---+---+---+   +---+---+---+---+
| * | * | * |   |    |   | * | * | * |    | * | * | * |   |   |   | * | * | * |
+---+---+---+---+    +---+---+---+---+    +---+---+---+---+   +---+---+---+---+
|   |   |   |   |    |   |   |   |   |    | * | * | * |   |   |   | * | * | * |
+---+---+---+---+    +---+---+---+---+    +---+---+---+---+   +---+---+---+---+

+---+---+---+---+    +---+---+---+---+    +---+---+---+---+
| * | * |   |   |    |   | * | * |   |    |   |   | * | * |
+---+---+---+---+    +---+---+---+---+    +---+---+---+---+   
| * | * |   |   |    |   | * | * |   |    |   |   | * | * |
+---+---+---+---+    +---+---+---+---+    +---+---+---+---+
| * | * |   |   |    |   | * | * |   |    |   |   | * | * |
+---+---+---+---+    +---+---+---+---+    +---+---+---+---+

Algoritmo
---------
1) Numero di posizioni senza rotazione
Un rettangolo AxB entra nella griglia se:
  A <= M e B <= N
Le posizioni possibili sono:
  (M - A + 1)*(N - B + 1)
perchè ci sono M-A+1 scelte verticali e ci sono N-B+1 scelte orizzontali.

2) Numero di posizioni con rotazione
Ruotando il rettangolo otteniamo BxA.
Un rettangolo AxB entra nella griglia se:
  B <= M e A <= N
Le posizioni possibili sono:
  (M - B + 1)*(N - A + 1)

3) Caso speciale: quadrato
Se A = B, la rotazione non produce nuove configurazioni.
Quindi bisogna contare una sola volta.

4) Formula finale
Se (A != B) --> P = (M - A + 1)*(N - B + 1) + (M - B + 1)*(N - A + 1)
Se (A = B)  --> P = (M - B + 1)*(N - A + 1)

Esempio:
  Griglia 3x4
  Rettangolo 2x3
  Orientazione 2x3
    (3-2+1)*(4-3+1) = 2*2 = 4
  Orientazione 3x2
    (3-3+1)*(4-2+1) = 1*3 = 3
  Totale:
    4 + 3 = 7

La formula funziona anche quando il rettangolo non entra nella griglia.
Per esempio, (M-A+1) <= 0 significa semplicemente:
- nessuna posizione valida
- contributo uguale a zero
quindi in pratica: max(0,(M-A+1)*(N-B+1)).
E analogamente per la rotazione.

; -------------------------------------------------------------------------
; Conta il numero di posizioni di un rettangolo AxB dentro una griglia MxN.
; Il rettangolo puo' anche essere ruotato di 90 gradi.
; posizioni(AxB) = (M-A+1)*(N-B+1)
; posizioni(BxA) = (M-B+1)*(N-A+1)
; Se A = B la rotazione non aggiunge nuove posizioni.
; -------------------------------------------------------------------------
;
(define (rettangoli M N A B)
  (letn ( (p1 0) (p2 0) )
    ; numero posizioni rettangolo AxB
    (if (and (>= M A) (>= N B)) ; il rettangolo entra nella griglia?
        (setq p1 (* (+ M (- A) 1)
                    (+ N (- B) 1))))
    ; numero posizioni rettangolo BxA (solo se il rettangolo non è quadrato)
    (if (and (!= A B) ; il rettangolo non è quadrato?
             (>= M B) (>= N A)) ; il rettangolo entra nella griglia?
        (setq p2 (* (+ M (- B) 1)
                    (+ N (- A) 1))))
    ; totale posizioni
    (+ p1 p2)))

Proviamo:

(rettangoli 3 4 2 3)
;-> 7
(rettangoli 4 4 2 2)
;-> 9
(rettangoli 5 7 1 3)
;-> 46
(rettangoli 2 2 3 3)
;-> 0
(rettangoli 10 10 1 1)
;-> 100


----------------------------
Consumo di bottiglie d'acqua
----------------------------

Una bottiglia d'acqua può essere acquistata con C bottiglie vuote della stessa acqua.
Alcune bottiglie vuote possono trovarsi in giro.
Se abbiamo P bottiglie piene, E bottiglie vuote, F bottiglie trovate e una bottiglia piena costa C bottiglie vuote, quante bottiglie d'acqua possiamo bere?

Algoritmo
Mantenere il numero attuale di bottiglie vuote e il numero di bottiglie consumate.
Continuare a bere finché è possibile comprarne un'altra:
- Comprare una nuova bottiglia.
- Bere tutta l'acqua della nuova bottiglia
- Conservare la bottiglia vuota.

(define (acqua piene-inizio vuote-inizio trovate costo-bottiglia)
  (cond ((= costo-bottiglia 0)
          (list 'INF 0))
        ((> costo-bottiglia (+ piene-inizio vuote-inizio trovate))
          (list 0 (+ piene-inizio vuote-inizio trovate)))
    (true
      (let ( (vuote-totale (+ piene-inizio vuote-inizio trovate))
            (bevute-totale piene-inizio) )
      (while (>= vuote-totale costo-bottiglia)
        (setq vuote-totale (- vuote-totale costo-bottiglia))
        (setq bevute-totale (+ bevute-totale 1))
        (setq vuote-totale (+ vuote-totale 1)))
      (list bevute-totale vuote-totale)))))

Proviamo:

(acqua 0 9 0 3)
;-> (4 1)
(acqua 0 5 5 2)
;-> (9 1)
(acqua 1 5 5 2)
;-> (11 1)
(acqua 2 5 5 3)
;-> (7 2)
(acqua 0 0 0 1)
;-> (0 0)
(acqua 0 0 0 0)
;-> (INF 0)
(acqua 1 0 0 2)
;-> (0 1)

È possibile derivare una formula chiusa.

Poniamo:
  P --> piene-inizio
  E --> vuote-inizio
  F --> trovate
  C --> costo-bottiglia
  V --> vuote-totale
  T --> bevute-totale
  K --> vuote-finale

Se il numero di bottiglie vuote totali vale:

  V = P + E + F

allora il numero massimo di bottiglie bevibili aggiuntive è:

  floor((V - 1)/(C - 1))

quindi:

  T = P + floor((V - 1)/(C - 1)), per (C > 1)
  (se C = 1, allora si possono bere infinite bottiglie)

Alla fine le bottiglie vuote rimanenti sono:

  K = ((V - 1) % (C - 1)) + 1

Per C = 2, il risultato finale è sempre 1
Per C > 2, il risultato può essere 1, 2, ..., C-1

(define (acqua2 P E F C)
  (let (V (+ P E F))
    (cond ((> C V) (list 0 V))
          ((= C 0) (list 'INF 0))
          ((> C 1)
            (list (+ P (/ (- V 1) (- C 1)))
                  (+ (% (- V 1) (- C 1)) 1))))))

Proviamo:

(acqua2 0 9 0 3)
;-> (4 1)
(acqua2 0 5 5 2)
;-> (9 1)
(acqua2 1 5 5 2)
;-> (11 1)
(acqua2 2 5 5 3)
;-> (7 2)
(acqua2 0 0 0 1)
;-> (0 0)
(acqua2 0 0 0 0)
;-> (INF 0)
(acqua2 1 0 0 2)
;-> (0 1)


--------------------------------
Misura della quantità di pioggia
--------------------------------

La quantità di pioggia si misura in millimetri.
La pioggia viene raccolta in un tubo trasparente verticale con tacche graduate in millimetri e, una volta che la pioggia ha smesso di cadere, è possibile controllare l'altezza dell'acqua nel tubo.
Nel nostro problema, il tubo presenta purtroppo una perdita all'altezza L millimetri (mm).
Se il livello dell'acqua è al di sopra della perdita, l'acqua fuoriesce dal tubo a una velocità di K millimetri all'ora (mm/h).
Vogliamo calcolare quanta pioggia è caduta durante una particolare precipitazione.
Supponiamo che il tubo sia sufficientemente alto da non traboccare.
Supponiamo inoltre che la pioggia cada a un'intensità uniforme (sconosciuta)
Supponiamo che l'acqua non evapori dal tubo.
L'altezza della perdita è inoltre trascurabile.

Input:
Cinque numeri positivi L, K, T1, T2, H, dove:
- L indica la posizione della perdita (mm)
- K indica la velocità di perdita dell'acqua (mm/h)
- T1 indica la durata della pioggia (h)
- T2 indica il tempo intercorso tra la fine della pioggia e l'osservazione del livello dell'acqua (h)
- H indica il livello dell'acqua nel tubo al momento dell'osservazione (mm)

Output:
Due numeri F1 e F2 dove:
- F1 indica la quantità minima di pioggia (mm) che porterebbe all'osservazione data,
- F2 indica la quantità massima di pioggia (mm) che porterebbe all'osservazione data.

Durante la pioggia:
- prima di raggiungere L: nessuna perdita
- dopo aver raggiunto L: l'acqua fuoriesce con una velocità K
- l'aumento netto diventa (R - K)

Dopo la pioggia:
- nessuna pioggia, solo perdite se il livello > L

Caso 1: H < L
-------------
Il livello dell'acqua non ha mai raggiunto la perdita.
Pertanto: F1 = F2 = H

Caso 2: H > L
-------------
L'acqua deve aver superato il livello della perdita.
Subito dopo la pioggia:
  livello = H + K*T2
Durante la pioggia:
- tempo per raggiungere la perdita: L / R
- tempo rimanente: T1 - L/R
- guadagno netto dopo la perdita: (R - K)
Equazione:
  L + (R - K)*(T1 - L/R) = H + K*T2
Espansione:
  R*T1 - K*T1 + K*L/R = H + K*T2
Moltiplicazione per R:
  T1*R^2 - (K*T1 + H + K*T2)*R + K*L = 0
Questa è un'equazione quadratica in R.
Soluzioni:
  R1 = (B - sqrt(D)) / (2*T1)
  R2 = (B + sqrt(D)) / (2*T1)
dove:
  B = K*T1 + H + K*T2
  D = B^2 - 4*T1*K*L
Quantità di pioggia:
  F1 = R1 * T1
  F2 = R2 * T1

Caso 3: H = L
-------------
Questo è un caso limite.
Esistono due possibilità:
(1) Non si è verificato alcun overflow:
    F = L
(2) Si è verificato un overflow e il sistema è tornato a L:
    Si applica lo stesso modello quadratico di H > L
    Pertanto:
    F1 = L
    F2 = R2 * T1 (radice superiore dell'equazione quadratica nel caso 2)

Formule finali
--------------
Se H < L:  F1 = F2 = H
Se H >= L: Equazione --> T1*R^2 - (K*T1 + H + K*T2)*R + K*L = 0
           F1 = min(R1, R2) * T1
           F2 = max(R1, R2) * T1

; -----------------------------------------------------------
; Funzione rain
; Calcola l'intervallo delle possibili quantità di pioggia
; Input:
;   L  = altezza perdita (mm)
;   K  = perdita (mm/h)
;   T1 = durata pioggia (h)
;   T2 = tempo dopo pioggia (h)
;   H  = livello osservato (mm)
; Output:
;   F1 = livello minimo di pioggia possibile
;   F2 = livello massimo di pioggia possibile
; -----------------------------------------------------------
(define (rain L K T1 T2 H)
  (letn (A B C D R1 R2 F1 F2)
    ;--------------------------------------------
    ; caso: mai raggiunta la perdita
    ;--------------------------------------------
    (if (< H L)
        (list H H)
        ;----------------------------------------
        ; caso H >= L (caso generale + limite)
        ;----------------------------------------
        (begin
          ; quadratica:
          ; T1*R^2 - (K*T1 + H + K*T2)*R + K*L = 0
          (setq A T1)
          (setq B (add (mul K T1) H (mul K T2)))
          (setq C (mul K L))
          (setq D (sub (mul B B) (mul 4 A C)))
          (if (< D 0) ; caso di soluzioni immaginarie
              (list nil nil)
              (begin
                ; radici
                (setq R1 (div (sub B (sqrt D)) (mul 2 A)))
                (setq R2 (div (add B (sqrt D)) (mul 2 A)))
                ; conversione in pioggia totale
                (setq F1 (mul (min R1 R2) T1))
                (setq F2 (mul (max R1 R2) T1))
                (list F1 F2)))))))

Proviamo:

(rain 10 2 5 1 7)
; -> (7 7)

(rain 10 2 5 1 12)
; -> (5.36675 18.63325)

(rain 10 2 5 3 10)
;-> (4.693376137081925 21.30662386291807)


----------------
Somma dei debiti
----------------

Ci sono N persone che hanno dei debiti tra loro.
Un debito, 'a' deve dare 'x' soldi a 'b', viene rappresentato con una lista del tipo:
  (a b x)
Scrivere una funzione che somma i debiti e i crediti di ogni persona.

Esempio:
  debiti = (a b 40) (b a 50) (c f 30) (d f 20) (c a 20) (f e 10)
  somme = (a 30) (b -10) (c -50) (d -20) (e 10) (f 40)

(define (debiti lst)
  ; lista associativa delle persone (a 0) (b 0) ...
  (let (persone (map (fn(x) (list x 0)) (sort (unique (flat (map chop lst))))))
    ; ciclo per ogni debito
    (dolist (el lst)
      (-- (lookup (el 0) persone) (el 2))  ; debito
      (++ (lookup (el 1) persone) (el 2))) ; credito
    persone))

(debiti '((a b 40) (b a 50) (c f 30) (d f 20) (c a 20) (f e 10)))
;-> ((a 30) (b -10) (c -50) (d -20) (e 10) (f 40))


----------------
Fila al semaforo
----------------

Siamo fermi ad un semaforo rosso dietro a N auto.
Tutte le auto sono lunghe X metri e distano tra loro Y metri.
Il semaforo diventa verde e ogni auto parte una dopo l'altra con un ritardo di W secondi.
Le auto si muovono con una velocità costante di K metri/sec
Dopo quanto tempo arriviamo al semaforo?

Abbiamo
  N = numero di auto davanti a noi
  X = lunghezza di ogni auto
  Y = distanza tra due auto consecutive
  W = ritardo di partenza tra un'auto e la successiva
  K = velocità costante delle auto (m/s)

Assumiamo che:
  - anche la nostra auto abbia lunghezza X
  - siamo fermi dietro l'ultima auto
  - il primo veicolo parte al tempo t = 0
  - ogni auto successiva parte dopo W secondi
  - tutte mantengono velocità costante K

La nostra auto è la numero N+1 della fila, quindi parte dopo:

  Tpartenza = N*W

Quando iniziamo a muoverci, dobbiamo ancora percorrere la distanza che ci separa dal semaforo.
Davanti a noi ci sono N auto lunghe X e N spazi lunghi Y
Quindi la distanza totale dal semaforo vale:

  D = N*(X + Y)

Il tempo necessario per percorrere questa distanza a velocità K è:

  Tmoto = D/K = N*(X + Y)/K

Pertanto il tempo totale dall'istante in cui il semaforo diventa verde fino a quando arriviamo al semaforo è:

  T = N*W + N*(X + Y)/K = N * (W + (X + Y)/K)

Esempio:
  N = 5
  X = 4 m
  Y = 2 m
  W = 1.5 s
  K = 10 m/s
  T = 5 * (1.5 + (4 + 2)/10) = 5 * (1.5 + 0.6) = 5 * 2.1 = 10.5 secondi

(define (fila N X Y W K)
  (mul N (add W (div (add X Y) K))))

(fila 5 4 2 1.5 10)
;-> 10.5


------------------------
Distanza minima tra auto
------------------------

Siamo in fondo a una lunga fila di auto.
Vogliamo mantenere la giusta distanza dall'auto che ci precede.
Abbiamo calcolato calcolato che per non dover mai frenare, dobbiamo mantenere una distanza di almeno p(n + 1), dove 'n' è il numero di auto che ci precedono e 'p' è una costante determinata dal tipo di auto che stiamo guidando.
Dato il valore di 'p' e le distanze attuali (in ordine casuale) da ciascuna delle auto davanti, calcolare la distanza minima che bisonga mantenere dall'auto che ci precede per non dover frenare.

Abbiamo:
 S[i] = distanza dell'i-esima auto davanti dopo l'ordinamento crescente
 p(k) = distanza minima necessaria quando ci sono k auto davanti
 noi siamo in fondo alla fila

Se un'auto si trova a distanza S[i], allora tra noi e quell'auto ci sono esattamente (i + 1) auto.
Per evitare di frenare dobbiamo avere:

  D + S[i] >= p(i + 1)
dove:
  D e' la nostra distanza dalla macchina immediatamente davanti
  S[i] e' la distanza dell'auto considerata rispetto alla macchina davanti a noi

Quindi:

  D >= p(i + 1) - S[i]

e questo deve valere per tutti gli i.

Pertanto:

  M = max(p(i + 1) - S[i])   con i in [0..n-1]

e la distanza minima corretta dalla macchina davanti sara':

  D = M

D rappresenta la distanza finale che dobbiamo mantenere dalla macchina immediatamente davanti.

Esempio 1
---------
Supponiamo:
  p(k) = 10*k
e distanze casuali:
  (35 12 27 50)
Ordiniamo:
  S = (12 27 35 50)
Calcoliamo:
  10 - 12 = -2
  20 - 27 = -7
  30 - 35 = -5
  40 - 50 = -10
Quindi:
  M = -2
Questo significa che siamo gia' troppo lontani e possiamo avvicinarci di 2 metri.
La distanza finale minima e':
  D = 10
infatti:
  D = max(10 - 12, 20 - 27, 30 - 35, 40 - 50) = -2
ma la distanza reale dalla prima auto e':
  12 + (-2) = 10
quindi il valore finale corretto della distanza e' 10.

Esempio 2
---------
S = (3 11 25 31)
Calcoliamo:
  10 - 3  = 7
  20 - 11 = 9
  30 - 25 = 5
  40 - 31 = 9
Quindi:
  M = 9
La nuova distanza dalla macchina davanti deve essere:
  D = 3 + 9 = 12
Verifica:
  auto 1: 12 >= 10
  auto 2: 12 + 11 = 23 >= 20
  auto 3: 12 + 25 = 37 >= 30
  auto 4: 12 + 31 = 43 >= 40
Tutti i vincoli sono soddisfatti.

; pfun = funzione p(k)
; dist = lista delle distanze casuali delle auto davanti
;
; Restituisce:
; (distanza-minima incremento lista-ordinata)

(define (distanza-minima pfun dist)
  (local (s n m cur distanza)
    ; ordina le distanze
    (setq s (sort dist))
    ; numero auto
    (setq n (length s))
    ; valore iniziale
    (setq m -1e99)
    ; calcola:
    ; M = max(p(i + 1) - S[i])
    (for (i 0 (- n 1))
      (setq cur (sub (pfun (+ i 1)) (s i)))
      (if (> cur m) (setq m cur)))
    ; distanza finale dalla prima auto
    (setq distanza (add (s 0) m))
    ; risultati
    (list distanza m s)))

Proviamo:

; funzione p(k) = 10*k
(define (p k) (mul 10 k))

(distanza-minima p '(35 12 27 50))
;-> (10 -2 (12 27 35 50))

(distanza-minima p '(3 11 25 31))
;-> (12 9 (3 11 25 31))


-----------------
Vacanze di gruppo
-----------------

Per organizzare una vacanza abbiamo i seguenti dati:
  N, il numero di partecipanti,
  B, il budget,
  W, il numero di settimane tra cui scegliere.
  H, la lista di hotel da considerare

Ogni hotel viene rappresentato con la seguente lista:

  ((P(0) P(1) ... P(W-1)) (D(0) D(1) ... D(W-1)))

dove:
  P = lista di W numeri che indicano il prezzo per una persona che soggiorna in hotel per ogni weekend.
  D = lista di W numeri che indicano il numero di posti letto disponibili in hotel per ogni weekend.

Trovare il costo minimo della vacanza, oppure "restare a casa" se non è possibile trovare nulla che rientri nel budget.

(define (vacanze N B W H)
  (local (out costo-minimo week-minimo hotel-minimo
          costi posti costo-totale letti)
    (setq out '())
    (setq costo-minimo (+ B 1))
    (setq week-minimo nil)
    (setq hotel-minimo nil)
    ; ciclo per ogni hotel...
    (dolist (hotel H)
      (println "HOTEL: " (+ $idx 1))
      ; costi hotel corrente
      (setq costi (hotel 0))
      ; posti hotel corrente
      (setq posti (hotel 1))
      ; ciclo per ogni settimana...
      (for (i 0 (- W 1))
        (print "  Settimana: " (+ i 1))
        ; costo totale per la settimana corrente
        (setq costo-totale (* N (costi i)))
        ; posti/letti disponibili per la settimana corrente
        (setq letti (posti i))
        (print "  Costo: " costo-totale)
        (print "  Posti: " letti)
        ; verifica possibilità di soggiorno nella settimana corrente
        (if (and (>= letti N) (>= B costo-totale))
            (begin
              (println "  -->  POSSIBILE")
              ; controllo costo minimo
              (when (<= costo-totale costo-minimo)
                (setq hotel-minimo (+ $idx 1))
                (setq costo-minimo costo-totale)
                (setq week-minimo (+ i 1))
                (push (list hotel-minimo week-minimo costo-minimo) out -1)))
            ;else
            (println "  -->  IMPOSSIBILE"))))
    out))

Proviamo:

(setq N 3)
(setq B 20)
(setq W 3)
(setq H '(((4 4 5) (3 2 2)) ((4 5 5) (4 3 2)) ((4 5 6) (2 4 4))))

(vacanze N B W H)
;-> HOTEL: 1
;->   Settimana: 1  Costo: 12  Posti: 3  -->  POSSIBILE
;->   Settimana: 2  Costo: 12  Posti: 2  -->  IMPOSSIBILE
;->   Settimana: 3  Costo: 15  Posti: 2  -->  IMPOSSIBILE
;-> HOTEL: 2
;->   Settimana: 1  Costo: 12  Posti: 4  -->  POSSIBILE
;->   Settimana: 2  Costo: 15  Posti: 3  -->  POSSIBILE
;->   Settimana: 3  Costo: 15  Posti: 2  -->  IMPOSSIBILE
;-> HOTEL: 3
;->   Settimana: 1  Costo: 12  Posti: 2  -->  IMPOSSIBILE
;->   Settimana: 2  Costo: 15  Posti: 4  -->  POSSIBILE
;->   Settimana: 3  Costo: 18  Posti: 4  -->  POSSIBILE
;-> ((1 1 12) (2 1 12))


-----------
Tre per due
-----------

Il negozio sotto casa vende tutti gli articoli con la promozione "3x2".
Per 3 articoli acquistati, l'articolo con prezzo minore viene regalato.
Se si acquistano N articoli, gli N/3 articoli con prezzo minore vengono regalati.

Esempio:
  Prezzo Articoli acquistati: 10 35 20 30 20 40
  Articoli da pagare: 40 35 30 20
  Costo: 40 + 35 + 30 + 20 = 125
  Articoli in regalo: 20 10
  Risparmio = 20 + 10 = 30

Dati i prezzi degli articoli da acquistare, scrivere una funzione che restituisce il risparmio massimo.
La funzione deve essere la più corta possibile.

Il risparmio massimo si ottiene comprando ogni volta i 3 articoli con il prezzo massimo.
Quindi basta ordinare i prezzi e sommare ogni 3 articoli.

(define (risparmio lst)
  (apply + (select (sort lst >) (sequence 2 (- (length lst) 1) 3))))

(setq prezzi '(10 35 20 30 20 40))
(risparmio prezzi)
;-> 40

Versione code-golf (67 caratteri):

(define(r l)(apply +(select(sort l >)(sequence 2(-(length l)1)3))))
(r prezzi)
;-> 40


---------------------------------
Griglia con tesori da raccogliere
---------------------------------

In una griglia MxN ci sono K celle che contengono un tesoro.
Partendo da una posizione P(x,y) trovare un percorso 'soddisfacente' che raccoglie tutti i tesori.
I movimenti possono essere solo verticali o orizzontali.
La posizione iniziale P e i tesori si trovano tutti i posizioni diverse.

Cosa intendiamo per 'soddisfacente'?
Intendiamo un percorso abbastanza vicino all'ottimo.

Tra due punti qualunque A(x1,y1) e B(x2,y2), il percorso minimo sulla griglia è:

  |x1 - x2| + |y1 - y2|

Quindi non serve cercare cammini: la griglia induce già una metrica completa.

Algoritmo ottimo
----------------
Trasformazione del problema
Abbiamo un punto iniziale P e K celle con tesoro.
Costruire un insieme di nodi:
- nodo 0 = P
- nodi 1..K = tesori
Definire una matrice distanze:
- dist(i,j) = distanza Manhattan tra i e j
A questo punto il problema diventa:
trovare il percorso minimo che parte da 0 e visita tutti i nodi 1..K almeno una volta
cioè un "TSP con partenza fissata".

Algoritmo greedy
----------------
Attraversare tutta la matrice e trovare dove sono i tesori.
  Da P andare al tesoro più vicino,
  poi da lì scegliere sempre il tesoro non ancora visitato più vicino
  ripetere finché sono stati visitati tutti i tesori.

L'algoritmo è semplice e veloce e sfrutta bene la distanza Manhattan.
Quindi come euristica pratica è una buona scelta.
Comunque non garantisce ottimalità.
Infatti una scelta localmente ottima può costringerci a fare un grande salto dopo.
Quindi può chiuderci in una regione e lasciarci l 'ultimo tesoro molto lontano.
In pratica funziona discretamente se:
- i tesori sono 'sparsi uniformemente'
- non ci sono cluster molto separati
- la distribuzione è quasi isotropa
In questi casi spesso è vicino all'ottimo.

; greedy nearest-neighbor per raccolta tesori su griglia (distanza Manhattan)
; input:
; P = posizione iniziale (x y)
; T = lista di tesori ((x1 y1) (x2 y2) ...)

; ------------------------------------------------------------
; Distanza Manhattan tra due celle della griglia
; La distanza e':
; |x1 - x2| + |y1 - y2|
; ------------------------------------------------------------
(define (manha a b)
  (+ (abs (- (a 0) (b 0)))
     (abs (- (a 1) (b 1)))))

; ------------------------------------------------------------
; Cerca il tesoro non ancora visitato piu' vicino
; alla posizione corrente.
; Input:
; pos = posizione corrente (x y)
; lst = lista di tesori ((x1 y1) (x2 y2) ...)
; used = array booleano dei tesori gia' visitati
; Output:
; indice del tesoro piu' vicino
; ------------------------------------------------------------
(define (min-index-from-pos pos lst used)
  (letn (
         ; indice del miglior tesoro trovato
         best-i -1
         ; distanza minima corrente
         best-d 999999999
         ; variabili temporanee
         i 0
         d 0)
    ; analizza tutti i tesori
    (for (i 0 (- (length lst) 1))
      ; considera solo i tesori non visitati
      (if (not (used i))
        (begin
          ; distanza tra posizione corrente
          ; e tesoro i-esimo
          (setq d (manha pos (lst i)))
          ; se troviamo un tesoro piu' vicino
          ; aggiorniamo il migliore
          (if (< d best-d)
            (begin
              (setq best-d d)
              (setq best-i i))))))
    ; restituisce l'indice del tesoro scelto
    best-i))

; ------------------------------------------------------------
; Algoritmo greedy nearest-neighbor
; Strategia:
; 1) partiamo da P
; 2) scegliamo il tesoro piu' vicino
; 3) ci spostiamo su quel tesoro
; 4) ripetiamo fino a visitare tutti i tesori
; Input:
; P = posizione iniziale -> (x y)
; T = lista tesori
; Output:
; lista ordinata delle posizioni visitate
; ------------------------------------------------------------
(define (greedy-treasure P T)
  (letn ( ; numero totale dei tesori
          (n (length T))
          ; array booleano:
          ; true  -> tesoro gia' visitato
          ; nil   -> tesoro non visitato
          (used (array n '(nil)))
          ; posizione corrente
          (cur P)
          ; percorso finale
          (path '())
          ; variabili temporanee
          (i 0)
          (next 0) )
    ; eseguiamo una scelta greedy
    ; per ogni tesoro
    (for (i 0 (- n 1))
      ; trova il tesoro piu' vicino
      ; alla posizione corrente
      (setq next (min-index-from-pos cur T used))
      ; marca il tesoro come visitato
      (setf (used next) true)
      ; aggiunge il tesoro al percorso
      (push (T next) path -1)
      ; aggiorna la posizione corrente
      (setq cur (T next)))
    ; aggiunge la posizione iniziale
    ; all'inizio del percorso
    (push P path)
    ; restituisce il percorso completo
    path))

Proviamo:

(setq pp '(0 0))
(setq tt '((2 3) (3 4) (5 5) (10 10)))
(greedy-treasure pp tt)
;-> ((0 0) (2 3) (3 4) (5 5) (10 10))


-----------------------------------------
Percorsi minimi tra due punti (manhattan)
-----------------------------------------

Scrivere una funzione che prende due punti P1 e P2 e restituisce una lista con tutti i percorsi minimi possibili da P1 a P2 (lista di punti per ogni percorso).
Le connessioni possibili sono quelle della distanza di manhattan (solo movimenti orizzontali e verticali).

Dati i punti P1 = (x1 y1) e P2 = (x2 y2), un percorso e' una lista di punti consecutivi che collega P1 a P2 usando solo mosse:
- destra  (x+1 y)
- sinistra (x-1 y)
- alto    (x y+1)
- basso   (x y-1)

Consideriamo solo i percorsi minimi.
Se:

  dx = |x2 - x1|
  dy = |y2 - y1|

allora ogni percorso minimo contiene dx mosse orizzontali e dy mosse verticali in ordine arbitrario.
Il numero totale dei percorsi e':

  binom((dx + dy) dx)

Esempio:
  P1 = (0 0)
  P2 = (2 1)
Occorrono:
  2 mosse a destra (Right)
  1 mossa in alto (Up)
I percorsi minimi sono:
  1) R R U
  2) R U R
  3) U R R

; Funzione che genera tutti i percorsi minimi tra p1 e p2
; Ogni percorso e' una lista di punti
(define (percorsi p1 p2)
  (local (x1 y1 x2 y2 sx sy)
    (setq x1 (p1 0))
    (setq y1 (p1 1))
    (setq x2 (p2 0))
    (setq y2 (p2 1))
    ; direzione orizzontale
    (setq sx (if (< x1 x2) 1 -1))
    ; direzione verticale
    (setq sy (if (< y1 y2) 1 -1))
    ; funzione ricorsiva
    (define (walk x y)
      (cond
        ; arrivati
        ((and (= x x2) (= y y2))
          (list (list (list x y))))
        ; possiamo muoverci sia in x che in y
        ((and (!= x x2) (!= y y2))
          (append
            (map
              (fn (p) (cons (list x y) p))
              (walk (+ x sx) y))
            (map
              (fn (p) (cons (list x y) p))
              (walk x (+ y sy)))))
        ; solo movimento orizzontale
        ((!= x x2)
          (map
            (fn (p) (cons (list x y) p))
            (walk (+ x sx) y)))
        ; solo movimento verticale
        (true
          (map
            (fn (p) (cons (list x y) p))
            (walk x (+ y sy))))))
    (walk x1 y1)))

Proviamo:

(percorsi '(0 0) '(2 1))
;-> (((0 0) (1 0) (2 0) (2 1))
;->  ((0 0) (1 0) (1 1) (2 1))
;->  ((0 0) (0 1) (1 1) (2 1)))

(length (percorsi '(0 0) '(3 2)))
;-> 10

Infatti, binom(5 3) = 10


-----------------------------
Sovrapposizione di rettangoli
-----------------------------

Abbiamo N rettangoli posizionati con la base lungo l'asse X.
I rettangoli possono anche sovrapporsi tra loro (totalmente o parzialmente).
Ogni rettangolo viene rappresentato con due coordinate intere:
1) P1 = (x1 y1) --> punto basso sinistra
2) P2 = (x2 y2) --> punto alto destra
L'indice di ogni rettangolo rappresenta l'ordine con cui sono stati posizionati.
In questo modo prendendo due rettangoli possiamo stabilire quale rettangolo si trova sopra (e quale sotto).

Esempio:     
rettangoli: R1 = ((a1 b1) (a2 b2))
            R2 = ((a3 b3) (a4 b4))
            R3 = ((a5 b5) (a6 b7))

R1 viene posizionato per primo, R2 per secondo e R3 per terzo.

   Y
   |
   |      +----------+
   |      | R1       |
   |      |     +--------+
   | +------+   | R3 |   |
   | | R2 | |   |    |   |
   | |    | |   |    |   |
   | |    | |   |    |   |
   +-+----+-+---+----+---+----X

Data una lista di rettangoli trovare per ogni rettangolo l'area della parte visibile.
L'area deve essere un numero da 0 (rettangolo completamente coperto) e 1 (rettangolo completamente visibile).

Algoritmo
---------
Per ogni rettangolo:
  a) calcolare la sua area (A)
  b) calcolare le aree di sovrapposizione del rettangolo corrente con tutti gli altri rettangoli che sono stati posizionati dopo il rettangolo corrente.
  c) Sommare le aree di sovrapposizione (AS)
  d) Calcolare la percentuale visibile del rettangolo corrente P = (A - AS)/A.

Nota:
Questo algoritmo è valido SOLO SE non esistono tre o più rettangoli che hanno un'area di sovrapposizione in comune.
In altre parole, un rettangolo puo essere coperto da più di un rettangolo, basta che i rettangoli di 'copertura' non si sovrappongono tra loro in alcun modo.

Calcolo dell'intersezione di due rettangoli:

Rettangolo A: ((x0A y0A) (x1A y1A))
(x0A y0A) (angolo inferiore sinistro)
(x1A y1A) (angolo superiore destro)

Rettangolo B: ((x0B y0B) (x1B y1B))
(x0B y0B) (angolo inferiore sinistro)
(x1B y1B) (angolo superiore destro)

La coordinata x-minima dell'intersezione sarà il massimo tra x0A e x0B.
La coordinata x-massima dell'intersezione sarà il minimo tra x1A e x1B.
La coordinata y-minima dell'intersezione sarà il massimo tra y0A e y0B.
La coordinata y-massima dell'intersezione sarà il minimo tra y1A e y1B.

L'intersezione esiste solo se:
1) Il valore massimo della coordinata x-minima è minore del valore minimo della coordinata x-massima e
2) Il valore massimo della coordinata y-minima è minore del valore minimo della coordinata y-massima.

(define (overlay A B)
  (local (x0A y0A x1A y1A x0B y0B x1B y1B
          xmin ymin xmax ymax)
    (setq x0A (A 0 0)) (setq y0A (A 0 1))
    (setq x1A (A 1 0)) (setq y1A (A 1 1))
    (setq x0B (B 0 0)) (setq y0B (B 0 1))
    (setq x1B (B 1 0)) (setq y1B (B 1 1))
    (setq xmin (max x0A x0B))
    (setq ymin (max y0A y0B))
    (setq xmax (min x1A x1B))
    (setq ymax (min y1A y1B))
    # verifica dell'intersezione
    (if (and (< xmin xmax) (< ymin ymax))
        (list (list xmin ymin)
              (list xmax ymax))
        ; else (nessuna intersezione)
        nil)))

(define (overlay1 A B)
  (let ( (xmin (max (A 0 0) (B 0 0)))
         (ymin (max (A 0 1) (B 0 1)))
         (xmax (min (A 1 0) (B 1 0)))
         (ymax (min (A 1 1) (B 1 1))) )
    # verifica dell'intersezione
    (if (and (< xmin xmax) (< ymin ymax))
        (list (list xmin ymin)
              (list xmax ymax))
        ; else (nessuna intersezione)
        nil)))

(overlay '((1 1) (4 4)) '((2 2) (5 5)))
(overlay1 '((1 1) (4 4)) '((2 2) (5 5)))
;-> ((2 2) (4 4))

(overlay '((2 2) (4 4)) '((3 3) (4 5)))
(overlay1 '((2 2) (4 4)) '((3 3) (4 5)))
;-> ((3 3) (4 4))

(overlay '((1 1) (4 4)) '((4 1) (6 4)))
(overlay1 '((1 1) (4 4)) '((4 1) (6 4)))
;-> nil

; Funzione che stampa una matrice di numeri interi
; nil -> "-"
(define (print-grid grid)
  (local (row col)
    (setq row (length grid))
    (setq col (length (first grid)))
    (for (i 0 (- row 1))
      ; stampa della griglia
      (for (j 0 (- col 1))
        (if (= (grid i j) nil)
            (print "  -")
            ;else
            (print (format "%3d" (grid i j)))))
      (println))))

Se i rettangoli non sono molto grandi, allora possiamo 'discretizzare' il problema.

Algoritmo (lento)
-----------------
Per ogni rettangolo:
  - creare una matrice vuota che copre tutti i rettangoli
  Per ogni rettangolo:
  - aggiornare le celle della matrice aggiungendo 1 nelle celle coperte dal rettangolo corrente
  Calcolare il rapporto tra area visibile e area totale del rettangolo corrente
  - leggere dalla matrice le celle che ricopre
  - le celle con valore 1 sono quelle visibili

Il metodo della discretizzazione evita automaticamente tutti i problemi di:
- doppio conteggio
- tripla sovrapposizione
- inclusione/esclusione
- geometria complicata
perchè lavora direttamente sulle celle elementari della griglia.
Per ogni rettangolo Ri:
1) costruisce una matrice contenente:
   - solo Ri
   - tutti i rettangoli sopra Ri
2) ogni cella contiene:
   - 0 -> vuota
   - 1 -> coperta solo da Ri
   - > 1 -> coperta anche da rettangoli superiori
3) le celle con valore 1 sono visibili

Quindi: visibile = celle-con-valore-1 / area-rettangolo
Il trucco fondamentale è: (pop rects)
Perchè in questo modo:
- il primo rettangolo viene confrontato con tutti
- il secondo solo con quelli sopra
- il terzo solo con quelli ancora sopra
- ecc.
quindi rispetta perfettamente l'ordine di disegno.
La griglia rappresenta una rasterizzazione del piano.
Ogni cella (row,col) rappresenta il quadratino [col,col+1] x [row,row+1]
Quindi l'algoritmo calcola l'area visibile tramite conteggio di pixel/celle.
Questo equivale ad un algoritmo Z-buffer discreto usato in grafica 2D/3D.
La complessità vale circa O(N^2 * W * H)
dove N = numero rettangoli, W = larghezza griglia, H = altezza griglia
perchè per ogni rettangolo ricrea tutta la griglia e ridisegni tutti i rettangoli superiori
Quindi è corretto ma lento.

(define (rettangoli rects show)
  ; lista delle soluzioni
  (setq out '())
  ; calcolo coordinate minime e massime della griglia 
  ; che ricopre tutti i rettangoli
  (setq xcoords (flat (map (fn(x) (list (x 0 0) (x 1 0))) rects)))
  (setq ycoords (flat (map (fn(y) (list (y 0 1) (y 1 1))) rects)))
  (setq xmin (apply min xcoords))
  (setq xmax (apply max xcoords))
  (setq ymin (apply min ycoords))
  (setq ymax (apply max ycoords))
  ; Ciclo per ogni rettangolo per calcolare la visibilità
  (while rects
    (setq r (first rects))
    ; Crea una griglia vuota (0)
    (setq grid (array (+ ymax 1) (+ xmax 1) '(0)))
    ; Aggiorna i valori della griglia sovrapponendo tutti i rettangoli
    ; (+1 per ogni sovrapposizione)
    (dolist (s rects)
      ; Calcolo coordinate minime e massime del rettangolo corrente
      (setq row-min (s 0 1))
      (setq row-max (s 1 1))
      (setq col-min (s 0 0))
      (setq col-max (s 1 0))
      (for (row row-min (- row-max 1))
        (for (col col-min (- col-max 1))
          (++ (grid row col)))))
    (if show (print-grid (reverse (copy grid)) "" ""))
    ; calcolo coordinate minime e massime del rettangolo corrente
    (setq row-min (r 0 1))
    (setq row-max (r 1 1))
    (setq col-min (r 0 0))
    (setq col-max (r 1 0))
    (setq conta 0)
    ; Conta il numero di 1 contenuti nelle celle del rettangolo
    ; contenuto nella griglia (celle visibili)
    (for (row row-min (- row-max 1))
      (for (col col-min (- col-max 1))
        (if (= (grid row col) 1) (++ conta))))
    ; Calcola l'area visibile del rettangolo corrente
    (setq area (* (- row-max row-min) (- col-max col-min)))
    (setq visibile (div conta area))
    (if show (println conta { } area { } visibile))
    ; Aggiorna la lista delle soluzioni
    (push (list conta area visibile) out -1)
    ; elimina il rettangolo corrente
    ; (perchè non deve essere più usato)
    (pop rects))
  out)

(setq lst '(((3 0) (9 8)) ((1 0) (5 4)) ((4 0) (11 6))))
(rettangoli lst true)
;->  0 0 0 0 0 0 0 0 0 0 0 0
;->  0 0 0 1 1 1 1 1 1 0 0 0
;->  0 0 0 1 1 1 1 1 1 0 0 0
;->  0 0 0 1 2 2 2 2 2 1 1 0
;->  0 0 0 1 2 2 2 2 2 1 1 0
;->  0 1 1 2 3 2 2 2 2 1 1 0
;->  0 1 1 2 3 2 2 2 2 1 1 0
;->  0 1 1 2 3 2 2 2 2 1 1 0
;->  0 1 1 2 3 2 2 2 2 1 1 0
;-> 14 48 0.2916666666666667
;->  0 0 0 0 0 0 0 0 0 0 0 0
;->  0 0 0 0 0 0 0 0 0 0 0 0
;->  0 0 0 0 0 0 0 0 0 0 0 0
;->  0 0 0 0 1 1 1 1 1 1 1 0
;->  0 0 0 0 1 1 1 1 1 1 1 0
;->  0 1 1 1 2 1 1 1 1 1 1 0
;->  0 1 1 1 2 1 1 1 1 1 1 0
;->  0 1 1 1 2 1 1 1 1 1 1 0
;->  0 1 1 1 2 1 1 1 1 1 1 0
;-> 12 16 0.75
;->  0 0 0 0 0 0 0 0 0 0 0 0
;->  0 0 0 0 0 0 0 0 0 0 0 0
;->  0 0 0 0 0 0 0 0 0 0 0 0
;->  0 0 0 0 1 1 1 1 1 1 1 0
;->  0 0 0 0 1 1 1 1 1 1 1 0
;->  0 0 0 0 1 1 1 1 1 1 1 0
;->  0 0 0 0 1 1 1 1 1 1 1 0
;->  0 0 0 0 1 1 1 1 1 1 1 0
;->  0 0 0 0 1 1 1 1 1 1 1 0
;-> 42 42 1
;-> ((14 48 0.2916666666666667) (12 16 0.75) (42 42 1))

(setq lst '(((2 0) (10 8)) ((2 0) (14 4))))
(rettangoli lst true)
;->  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;->  0 0 1 1 1 1 1 1 1 1 0 0 0 0 0
;->  0 0 1 1 1 1 1 1 1 1 0 0 0 0 0
;->  0 0 1 1 1 1 1 1 1 1 0 0 0 0 0
;->  0 0 1 1 1 1 1 1 1 1 0 0 0 0 0
;->  0 0 2 2 2 2 2 2 2 2 1 1 1 1 0
;->  0 0 2 2 2 2 2 2 2 2 1 1 1 1 0
;->  0 0 2 2 2 2 2 2 2 2 1 1 1 1 0
;->  0 0 2 2 2 2 2 2 2 2 1 1 1 1 0
;-> 32 64 0.5
;->  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;->  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;->  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;->  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;->  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;->  0 0 1 1 1 1 1 1 1 1 1 1 1 1 0
;->  0 0 1 1 1 1 1 1 1 1 1 1 1 1 0
;->  0 0 1 1 1 1 1 1 1 1 1 1 1 1 0
;->  0 0 1 1 1 1 1 1 1 1 1 1 1 1 0
;-> 48 48 1
;-> ((32 64 0.5) (48 48 1))

Ottimizzazione
--------------
Possiamo creare UNA SOLA griglia finale.
Invece di contare le sovrapposizioni, scriviamo nella cella l'indice del rettangolo visibile più alto.
Esempio:
(grid row col) = indice-ultimo-rettangolo
Poi percorriamo la griglia una sola volta e contiamo quante celle appartengono a ciascun rettangolo
Questa volta la complessità vale O(N * area-media) invece di O(N^2 * area-griglia).

Inizializzare: (grid row col) = nil
Poi disegnare i rettangoli in ordine: (setf (grid row col) idx)
Alla fine ogni cella contiene il rettangolo visibile.
Poi contiamo: conteggio[idx] e ottieniamo direttamente l'area visibile di ogni rettangolo.

In pratica passiamo da "contare le coperture" a: "salvare direttamente il rettangolo visibile finale" che è il metodo tipico dei renderer raster.

;------------------------------------------------------------
; Calcola la parte visibile di una lista di rettangoli
;
; Ogni rettangolo ha forma:
; ((x1 y1) (x2 y2))
; dove:
; (x1,y1) = basso-sinistra
; (x2,y2) = alto-destra
;
; I rettangoli vengono disegnati nell'ordine della lista.
; I rettangoli successivi coprono quelli precedenti.
;
; La funzione restituisce per ogni rettangolo:
; (celle-visibili area-totale percentuale-visibile)
;
; Metodo:
; -------
; 1) Creare una sola griglia
; 2) Disegnare tutti i rettangoli in ordine
; 3) Ogni cella contiene l'indice dell'ultimo rettangolo
;    che la copre (quello visibile)
; 4) Contare le celle visibili per ogni rettangolo
;
; Complessità:
; -------------
; O(area totale coperta dai rettangoli)
;------------------------------------------------------------

(define (rettangoli-fast rects show)
  (local (out xcoords ycoords xmin xmax ymin ymax larghezza altezza grid
          aree visibili idx r row-min row-max col-min col-max area)
    ; Calcolo coordinate minime e massime
    (setq xcoords (flat (map (fn(x) (list (x 0 0) (x 1 0))) rects)))
    (setq ycoords (flat (map (fn(y) (list (y 0 1) (y 1 1))) rects)))
    (setq xmin (apply min xcoords))
    (setq xmax (apply max xcoords))
    (setq ymin (apply min ycoords))
    (setq ymax (apply max ycoords))
    ; Dimensioni della griglia
    (setq larghezza (- xmax xmin))
    (setq altezza  (- ymax ymin))
    ; Creazione griglia
    ; Ogni cella contiene:
    ; - nil -> vuota
    ; - indice del rettangolo visibile
    (setq grid (array altezza larghezza '(nil)))
    ; Lista aree rettangoli
    (setq aree '())
    (dolist (r rects)
      (setq area
            (* (- (r 1 1) (r 0 1))
               (- (r 1 0) (r 0 0))))
      (push area aree -1))
    ; Disegna i rettangoli in ordine
    ; I rettangoli successivi sovrascrivono quelli precedenti.
    (setq idx 0)
    (dolist (r rects)
      (setq row-min (- (r 0 1) ymin))
      (setq row-max (- (r 1 1) ymin))
      (setq col-min (- (r 0 0) xmin))
      (setq col-max (- (r 1 0) xmin))
      ; Disegna il rettangolo corrente
      (for (row row-min (- row-max 1))
        (for (col col-min (- col-max 1))
          (setf (grid row col) idx)))
      (++ idx))
    ; Visualizza la griglia finale
    (if show
      (begin
        (println "Griglia finale:")
        (print-grid (reverse (copy grid)) "" "")))
    ; Conta le celle visibili
    (setq visibili (dup 0 (length rects)))
    (for (row 0 (- altezza 1))
      (for (col 0 (- larghezza 1))
        (if (grid row col)
            (++ (visibili (grid row col))))))
    ; Costruzione output finale
    (setq out '())
    (for (i 0 (- (length rects) 1))
      (push (list (visibili i) (aree i) (div (visibili i) (aree i))) out -1))
    out))

Proviamo:

(setq lst '( ((3 0) (9 8)) ((1 0) (5 4)) ((4 0) (11 6)) ))
(rettangoli-fast lst true)
;-> Griglia finale:
;->   -  -  0  0  0  0  0  0  -  -
;->   -  -  0  0  0  0  0  0  -  -
;->   -  -  0  2  2  2  2  2  2  2
;->   -  -  0  2  2  2  2  2  2  2
;->   1  1  1  2  2  2  2  2  2  2
;->   1  1  1  2  2  2  2  2  2  2
;->   1  1  1  2  2  2  2  2  2  2
;->   1  1  1  2  2  2  2  2  2  2
;-> ((14 48 0.2916666666666667) (12 16 0.75) (42 42 1))

(setq lst '(((2 0) (10 8)) ((2 0) (14 4))))
(rettangoli-fast lst true)
;-> Griglia finale:
;->   0  0  0  0  0  0  0  0  -  -  -  -
;->   0  0  0  0  0  0  0  0  -  -  -  -
;->   0  0  0  0  0  0  0  0  -  -  -  -
;->   0  0  0  0  0  0  0  0  -  -  -  -
;->   1  1  1  1  1  1  1  1  1  1  1  1
;->   1  1  1  1  1  1  1  1  1  1  1  1
;->   1  1  1  1  1  1  1  1  1  1  1  1
;->   1  1  1  1  1  1  1  1  1  1  1  1
;-> ((32 64 0.5) (48 48 1))

Vedi anche "Area di sovrapposizione di rettangoli" su "Note libere 14".
Vedi anche "Intersezione di rettangoli" su "Note libere 27".


-----------------------------------------
Generazione di liste con elementi casuali
-----------------------------------------

Scriviamo una funzione che genera liste con elementi casuali dello stesso tipo.

Parametri
  nums    -> numero di elementi da generare
  min-val -> valore minimo dell'intervallo (numero o carattere)
  max-val -> valore massimo dell'intervallo (numero o carattere)
  type    -> genera interi "i" o floating "f" o caratteri "c" o stringhe "s"
  min-len -> lunghezza minima della stringa
  max-len -> lunghezza massima della stringa

(define (rand-list nums min-val max-val type min-len max-len)
  (let (out '())
    (cond ((= type "i") ; numeri interi
            (for (i 1 nums)
              (push (+ min-val (rand (+ (- max-val min-val) 1))) out -1)))
          ((= type "f") ; numeri floating
            (for (i 1 nums)
              (push (add min-val (random 0 (sub max-val min-val))) out -1)))
          ((= type "c") ; caratteri
            (if (string? min-val) (setq min-val (char min-val)))
            (if (string? max-val) (setq max-val (char max-val)))
            (for (i 1 nums)
               (push (char (+ min-val (rand (+ (- max-val min-val) 1)))) out -1)))
          ((= type "s") ; stringhe
            (if (string? min-val) (setq min-val (char min-val)))
            (if (string? max-val) (setq max-val (char max-val)))
            (for (i 1 nums)
              (setq len (+ min-len (rand (+ (- max-len min-len) 1))))
              (push (join (collect (char (+ min-val (rand (+ (- max-val min-val) 1)))) len)) out -1)))
    )
    out))

Proviamo:

(rand-list 10 5 25 "i")
;-> (8 5 13 10 7 22 6 13 25 11)

(rand-list 10 0 1 "f")
;-> (0.6226386303292947 0.6444898831141087 0.7817926572466201
;->  0.5518051698355052 0.8243964964751122 0.3734550004577776
;->  0.7815790276802881 0.424054689168981 0.2977996154667806
;->  0.5674306466872158)

(rand-list 10 48 57 "c")
;-> ("7" "1" "9" "9" "3" "9" "4" "8" "6" "0")

(rand-list 10 "A" "Z" "c")
;-> ("R" "I" "X" "H" "A" "A" "L" "F" "X" "M")

(rand-list 4 "a" "z" "s" 1 6)  --> ("ucdiwl" "pjw" "m" "qtd")
(rand-list 10 "a" "z" "s" 1 6)
;-> ("asc" "wmtm" "uoj" "qejdty" "aibq" "w" "pswads" "fbeq" "ijr" "unpux")

Scriviamo una funzione che genera liste con la struttura di una lista data con valori casuali in determinati intervalli.

(define (index-list lst)
"Create a list of indexes for all the elements of a list"
  (ref-all nil lst (fn(x) true)))

(setq L1 '((1 "c") "abc" -1.45 (1 ("xyz" (("qwe"))))))

(index-list L1)
;-> ((0) (0 0) (0 1) (1) (2) (3) (3 0) (3 1)
;->  (3 1 0) (3 1 1) (3 1 1 0) (3 1 1 0 0))

; Funzione che genera liste con la struttura di una lista data
; con valori casuali in determinati intervalli
(define (random-list nums lst min-int max-int min-float max-float min-len max-len min-char max-char)
  ; Parametri:
  ; nums      -> numero di elementi da generare
  ; lst       -> lista con la struttura desiderata
  ; min-int   -> valore minimo per gli interi
  ; max-int   -> valore massimo per gli interi
  ; min-float -> valore minimo per i float
  ; max-float -> valore massimo per i float
  ; min-len   -> lunghezza minima delle stringhe
  ; max-len   -> lunghezza massima delle stringhe
  ; min-char  -> valore minimo caratteri (numero o carattere)
  ; max-char  -> valore massimo dei caratteri (numero o carattere)
  (local (out indexes cur)
    ; lista di output
    (setq out '())
    ; lista degli indici della lista data
    (setq indexes (index-list lst))
    (for (i 1 nums)
      ; ciclo per ogni indice della lista data
      (dolist (el indexes)
        (setq cur (lst el))
        ; aggiorna solo i valori atomici
        ;(if-not (list? cur)
        (if (atom? cur)
          (cond ((integer? cur) ; intero
                  (setf (lst el) (+ min-int (rand (+ (- max-int min-int) 1)))))
                ((float? cur)   ; float
                  (setf (lst el) 
                      (add min-float (random 0 (sub max-float min-float)))))
                ((string? cur)  ; stringa
                  (if (string? min-char) (setq min-char (char min-char)))
                  (if (string? max-char) (setq max-char (char max-char)))
                  (if (> min-char max-char) (swap min-char max-char))
                  (setq len (+ min-len (rand (+ (- max-len min-len) 1))))
                  (setf (lst el)
                    (join (collect 
                        (char (+ min-char (rand (+ (- max-char min-char) 1))))
                        len)))))))
      ; aggiunge la lista corrente alla lista di output
      (push lst out))
    out))

Proviamo:

(setq L1 '((1 "c") "df" -1.45 (1 ("gh" "gh"))))
(random-list 10 L1 0 9 0 1 1 5 48 57)
;-> (((0 "535") "31" 0.8080385753959777 (1 ("78" "7638")))
;->  ((6 "33") "8856" 0.7981810968352305 (9 ("6" "218")))
;->  ((1 "17") "60306" 0.1316263313699759 (8 ("57801" "6201")))
;->  ((5 "65") "3071" 0.6230658894619587 (8 ("743" "5396")))
;->  ((7 "690") "63152" 0.2902920621356853 (9 ("614" "47")))
;->  ((0 "8544") "45" 0.152653584398938 (3 ("3898" "3910")))
;->  ((1 "896") "0693" 0.5692007202368237 (3 ("1" "87130")))
;->  ((8 "67531") "5921" 0.6047242652668844 (6 ("340" "6668")))
;->  ((1 "49060") "908" 0.2900173955504013 (2 ("4266" "456")))
;->  ((0 "262") "255" 0.6286812952055422 (1 ("969" "3"))))

Se abbiamo bisogno di vincoli più stringenti possiamo creare delle funzioni specifiche.

Esempio:

Struttura dell'elemento della lista:
  (codice (anno mese giorno) (spesa nome)
dove:
 codice --> indice sequenziale (1..N)
 anno, mese, giorno --> data (dal 2000 al 2026)
 spesa --> numero intero (tra 1 e 1e4)
 nome  --> lista di nomi predefinita

(setq L '(2 (2026 11 4) (150 "Piero")))

; Calcolo degli indici della lista
(setq indici (index-list L))
;-> ((0) (1) (1 0) (1 1) (1 2) (2) (2 0) (2 1))

; Posizione degli elementi nella lista
(dolist (idx indici) (println idx { } (L idx)))
;-> (0) 2
;-> (1) (2026 11 4)
;-> (1 0) 2026
;-> (1 1) 11
;-> (1 2) 4
;-> (2) (150 "Piero")
;-> (2 0) 150
;-> (2 1) Piero

Adesso scriviamo una funzione che genera 'nums' sottoliste con valori casuali.

(define (my-list nums lst)
  ; Parametri:
  ; nums      -> numero di elementi da generare
  ; lst       -> lista con la struttura desiderata
  (local (out nomi)
    ; lista di output
    (setq out '())
    ; lista dei nomi
    (setq nomi '("Paolo" "Sara" "Luca" "Piero"))
    ; ciclo per nums elementi...
    (for (i 1 nums)
      ;(setq cur lst)
      ; codice sequenziale
      (setf (lst 0) i)
      ; data: anno
      (setf (lst 1 0) (+ 2000 (rand 27)))
      ; data: mese
      (setf (lst 1 1) (+ 1 (rand 12)))
      ; data: giorno
      (setf (lst 1 1) (+ 1 (rand 31)))
      ; spesa
      (setf (lst 2 0) (+ 1 (rand 10000)))
      ; nome
      (setf (lst 2 1) (nomi (rand (length nomi))))
      ; aggiunge l'elemento corrente (sottilista) alla lista di output
      (push lst out -1))
    out))

Proviamo:

(setq L '(2 (2026 11 4) (150 "Piero")))
(my-list 10 L)
;-> ((1 (2017 27 4) (1234 "Paolo"))
;->  (2 (2020 30 4) (2861 "Sara"))
;->  (3 (2003 26 4) (7080 "Luca"))
;->  (4 (2020 5 4) (17 "Paolo"))
;->  (5 (2021 7 4) (1157 "Luca"))
;->  (6 (2000 15 4) (7523 "Luca"))
;->  (7 (2014 14 4) (2020 "Luca"))
;->  (8 (2007 8 4) (5779 "Luca"))
;->  (9 (2016 16 4) (9631 "Luca"))
;->  (10 (2024 11 4) (1784 "Piero")))

Vedi anche "Creazione di vettori/matrici (array) e liste con valori casuali" su "Note libere 7".


-----------------------
Parcheggio intelligente
-----------------------

Lungo una strada rettilinea esistono N negozi.
Vogliamo visitare tutti i negozi una volta.
Arrivando con un automobile, in quale punto dobbiamo parcheggiare per minimizzare il percorso del nostro shopping?
Scrivere una funzione che calcola la lunghezza del percorso minimo che permette di visitare tutti i negozi.
La funzione deve essere la più corta possibile.

Modello matematico
Abbiamo N punti diversi con posizionati lungo l'asse X.
I punti hanno coordinate intere.
Dobbiamo visitare tutti i punti partendo da un punto P lungo la linea.
Quale punto P minimizza il percorso di visita?

È ovvio che parcheggiare a sinistra del negozio più a sinistra o a destra del negozio più a destra non è la soluzione ottimale.
Parcheggiando tra questi due estremi, dobbiamo sempre camminare dall'auto al negozio più a sinistra e tornare indietro, e poi dall'auto al negozio più a destra e tornare indietro.
Questo significa che percorriamo sempre il doppio della distanza che separa il negozio più a sinistra da quello più a destra.

(define (park pts)
  (let ( (min-val (first pts))
         (max-val (last pts))
         (dx 0) (sx 0) )
    ; prova ogni posizione nell'intervallo dei punti
    (for (p min-val max-val)
      ; percorso dei punti a destra di p
      (setq dx (* 2 (- max-val p)))
      ; percorso dei punti a sinistra di p
      (setq sx (* 2 (- p min-val)))
      (println (format "%s %2d %s %2d %s %2d %s %2d"
               "Pos:" p "sx:" sx "dx:" dx "tot:" (+ sx dx)))) '>))

Proviamo:

(setq lst '(1 2 5 7))
(park lst)
;-> Pos:  1 sx:  0 dx: 12 tot: 12
;-> Pos:  2 sx:  2 dx: 10 tot: 12
;-> Pos:  3 sx:  4 dx:  8 tot: 12
;-> Pos:  4 sx:  6 dx:  6 tot: 12
;-> Pos:  5 sx:  8 dx:  4 tot: 12
;-> Pos:  6 sx: 10 dx:  2 tot: 12
;-> Pos:  7 sx: 12 dx:  0 tot: 12

Morale: parcheggiamo dove c'è posto!

; Funzione che calcola la lunghezza del percorso

(define (len path) (* 2 (- (path -1) (path 0))))
(len lst)
;-> 12

Versione code-golf (32 caratteri):

(define(l p)(* 2(-(p -1)(p 0))))
(l lst)
;-> 12


-------------------
Segmenti e poligoni
-------------------

Dati N segmenti di lunghezza diversa, quali poligoni possiamo costruire?

Esempio: 
  segmenti = (1 3 4 6 9)
  poligoni = (1 4 6) (1 6 9) (1 3 4 6) (3 4 6 9) ecc.

Per costruire un poligono con un insieme di segmenti, deve valere la condizione:
il segmento più lungo deve essere strettamente minore della somma di tutti gli altri segmenti.
In formula: 

  (max-length < sum-others)

oppure equivalentemente:

  (sum-total > (2 * max-length))

Questa è la generalizzazione della disuguaglianza triangolare a poligoni con qualunque numero di lati.

Nell'esempio:
  (1 3 4 6 9)
  (1 4 6)        -> 1+4 > 6
  (1 6 9)        -> 1+6 > 9
  (1 3 4 6)      -> 1+3+4 > 6
  (3 4 6 9)      -> 3+4+6 > 9

Algoritmo
---------
1) Generare tutti i sottoinsiemi con almeno 3 segmenti.
2) Per ogni sottoinsieme:
   - calcolare la somma totale
   - trovare il massimo
   - verificare: (somma > 2 * massimo)
3) Aggiornare la soluzione

(define (powerset lst)
"Generate all sublists of a list"
  (if (empty? lst)
      (list '())
      (let ((element (first lst))
            (p (powerset (rest lst))))
         (extend (map (fn (subset) (cons element subset)) p) p))))

; Verifica se i segmenti formano un poligono
(define (poligono? lst)
  (letn ((somma (apply + lst))
         (maxi (apply max lst)))
    (> somma (mul 2 maxi))))

(poligono? '(1 4 6))
;-> nil

; Restituisce tutti i poligoni possibili
(define (poligoni segmenti)
  (let (out '())
    (dolist (p (powerset segmenti))
      (if (and (>= (length p) 3)
               (poligono? p))
          (push p out -1)))
    out))

Proviamo:

(poligoni '(1 3 4 6 9))
;-> ((1 3 4 6 9) (1 3 4 6) (1 3 6 9) (1 4 6 9) (3 4 6 9) (3 4 6) (4 6 9))

La soluzione brute-force genera tutti i sottoinsiemi O(2^N), quindi va bene per valori piccoli o medi di N.


-------------------
Trucco con le carte
-------------------

Prendiamo un mazzo di carte, lo teniamo a faccia in giù ed eseguiamo la seguente procedura:
1. La prima carta in cima al mazzo viene spostata in fondo.
La nuova carta in cima viene distribuita scoperta sul tavolo. È l'Asso di Cuori.
2. Due carte vengono spostate una alla volta dall'alto verso il basso.
La carta successiva viene distribuita scoperta sul tavolo. È il Due di Cuori.
3. Tre carte vengono spostate una alla volta...
4. Si continua così fino a quando l'n-esima e ultima carta non risulta essere il N di Cuori.
Supponendo N = 13 (cioè tutte le carte di Cuori in sequenza dall'Asso fino al Re), come bisogna ordinare le carte per eseguire questo trucco?

La soluzione non è banale. Infatti non basta inserire i Cuori dopo 1,2,3, ecc. carte, perchè il mazzo finisce prima di inserire tutte le 13 carte.
Quindi alcune carte di Cuori vanno inserite considerando le carte che vengono messe in fondo al mazzo durante il processo.

La soluzione per un mazzo di 52 carte è la seguente (indici 0-based):

  Indice   Carta
  1        (Asso Cuori))
  2        (9 Cuori))
  4        (2 Cuori))
  8        (3 Cuori))
  11       (Re Cuori))
  13       (4 Cuori))
  16       (10 Cuori))
  19       (5 Cuori))
  26       (6 Cuori))
  30       (Jack Cuori))
  34       (7 Cuori))
  43       (8 Cuori))
  45       (Donna Cuori))))

Vediamo una funzione che simula il preocesso del trucco:

(define (magic)
  (local (pos values suits cards hearts others)
    ; posizioni dei Cuori
    (setq pos '((1 (Asso Cuori)) (2 (9 Cuori)) (4 (2 Cuori)) (8 (3 Cuori))
                (11 (Re Cuori)) (13 (4 Cuori)) (16 (10 Cuori)) (19 (5 Cuori))
                (26 (6 Cuori)) (30 (Jack Cuori)) (34 (7 Cuori)) (43 (8 Cuori))
                (45 (Donna Cuori))))
    ; valori delle carte
    (setq values '(Asso 2 3 4 5 6 7 8 9 10 Jack Donna Re))
    ; semi delle carte
    (setq suits '(Cuori Quadri Fiori Picche))
    ; creazione mazzo di carte completo
    (setq cards '())
    (dolist (suit suits)
      (dolist (val values)
        (push (list val suit) cards)))
    ; creazione carte di Cuori
    (setq hearts '())
    (dolist (val values) (push (list val 'Cuori) hearts -1))
    ; mischia il mazzo senza le carte Cuori
    (setq others (randomize (difference cards hearts)))
    ; inserisce le carte di Cuori nelle posizioni corrette
    (dolist (p pos) (push (p 1) others (p 0)))
    ; visualizza tutto il processo
    (setq trick others)
    (for (i 1 13)
      (print "Sposto " i " carte e poi scopro la carta: ")
      (for (k 1 i) (push (pop trick) trick -1))
      (print (pop trick)) (read-line)) '>))

(magic)
;-> Sposto 1 carte e poi scopro la carta: (Asso Cuori)
;-> Sposto 2 carte e poi scopro la carta: (2 Cuori)
;-> Sposto 3 carte e poi scopro la carta: (3 Cuori)
;-> Sposto 4 carte e poi scopro la carta: (4 Cuori)
;-> Sposto 5 carte e poi scopro la carta: (5 Cuori)
;-> Sposto 6 carte e poi scopro la carta: (6 Cuori)
;-> Sposto 7 carte e poi scopro la carta: (7 Cuori)
;-> Sposto 8 carte e poi scopro la carta: (8 Cuori)
;-> Sposto 9 carte e poi scopro la carta: (9 Cuori)
;-> Sposto 10 carte e poi scopro la carta: (10 Cuori)
;-> Sposto 11 carte e poi scopro la carta: (Jack Cuori)
;-> Sposto 12 carte e poi scopro la carta: (Donna Cuori)
;-> Sposto 13 carte e poi scopro la carta: (Re Cuori)

Vediamo un metodo per risolvere il problema.
Sia M il numero totale di carte del mazzo e siano numerate le posizioni (0..M-1) (0-based).
Nel nostro caso M = 52 e vogliamo che le 13 carte di cuori escano nell'ordine:
  Asso,2,3,...,10,Jack,Donna,Re
La procedura per ogni passo 'k' (1-based) è:
- si spostano 'k' carte dalla cima al fondo
- la carta successiva viene rivelata

Invece di simulare le carte, simuliamo le POSIZIONI del mazzo.
Consideriamo una lista circolare delle posizioni ancora disponibili.
All'inizio: 0 1 2 3 4 ... 51
Quando al passo 'k' spostiamo 'k' carte dalla cima al fondo, equivale a fare una rotazione di 'k' posizioni nella lista circolare.
La posizione successiva è quella dove va la carta richiesta.
Dopo aver usato quella posizione, la eliminiamo dalla lista.
La regola è:
- ruotare di k
- prendere quella posizione
- rimuoverla dalla lista
- il conteggio successivo riparte dalla posizione immediatamente dopo quella rimossa

Algoritmo
---------
'L' è la lista delle posizioni ancora libere e 'p' è l'indice corrente, allora al passo 'k':

  p = (p + k) mod length(L)

la posizione scelta è:

  L[p]

poi si elimina L[p].

Sia:
- Lk = lista delle posizioni ancora libere al passo 'k'
- mk = length(Lk)

Manteniamo un indice circolare 'p'.

All'inizio:

  p = 0
  L1 = (0 1 2 ... 51)

Al passo 'k':

  p = (p + k) mod mk

La posizione scelta è:

  Lk[p]

Poi quella posizione viene rimossa.

Eseguiamo i primi passi:

Passo 1
  L = 0 1 2 3 ... 51
  m = 52
  p = (0 + 1) mod 52 = 1
  Posizione scelta: 1
  Quindi: Asso di Cuori -> posizione 1
  Rimuoviamo 1.

Passo 2
  Ora:
  L = 0 2 3 4 5 ... 51
  m = 51
  L'indice corrente resta sul punto successivo alla rimozione.
  p = (1 + 2) mod 51 = 3
  Elemento L[3]: 4
  Quindi: 2 di Cuori -> posizione 4

Passo 3
  m = 50
  p = (3 + 3) mod 50 = 6
  La posizione corrispondente è: 8
  Quindi: 3 di Cuori -> posizione 8

Continuando si ottiene:

  1   -> Asso
  4   -> 2
  8   -> 3
  13  -> 4
  19  -> 5
  26  -> 6
  34  -> 7
  43  -> 8
  2   -> 9
  16  -> 10
  30  -> 11 (Jack)
  45  -> 12 (Donna)
  11  -> 13 (Re)

che è la soluzione.

Questo problema è una variante del problema di Giuseppe (Josephus Problem).
Infatti, le posizioni rimaste formano un cerchio, ad ogni passo si conta ciclicamente e una posizione viene eliminata.
La differenza rispetto al Josephus classico è che il passo cambia:
1,2,3,4,..., invece di essere costante.

; Funzione che inserisce N carte in un mazzo di M carte
; per simulare il trucco
(define (trick-pos num-carte num-valori)
  (local (libere p out)
    ; posizioni libere del mazzo
    (setq libere (sequence 0 (- num-carte 1)))
    ; indice circolare
    (setq p 0)
    (setq out '())
    ; k = numero di carte spostate
    (for (k 1 num-valori)
      ; nuova posizione circolare
      (setq p (% (+ p k) (length libere)))
      ; salva posizione della carta
      (push (list (libere p) k) out -1)
      ; elimina posizione usata
      (pop libere p))
    out))

Proviamo:

(trick-pos 52 13)
;-> ((1 1)
;->  (4 2)
;->  (8 3)
;->  (13 4)
;->  (19 5)
;->  (26 6)
;->  (34 7)
;->  (43 8)
;->  (2 9)
;->  (16 10)
;->  (30 11)
;->  (45 12)
;->  (11 13))


-------------------------
Lisp 1960 (John McCarthy)
-------------------------

https://paulgraham.com/rootsoflisp.html
Di seguito le funzioni LISP definite da McCarthy nel 1960 e tradotte in Common Lisp (CL) da Paul Graham.
Dopo ogni funzione in CL troviamo la funzione tradotta in newLISP.

;-----------------------------------------------------------------
; The Lisp defined in McCarthy's 1960 paper, translated into CL.
; Assumes only quote, atom, eq, cons, car, cdr, cond.
; Written by Paul Graham
; Bug reports to lispcode@paulgraham.com
; The symbol nil is () (the empty list).
;
; Translate in newLISP by cameyo
;-----------------------------------------------------------------
;
;-----------------------------------------------------------------
; C????R (four levels)
;-----------------------------------------------------------------
(define (car x)    (first x))
(define (cdr x)    (rest x))
(define (caar x)   (first (first x)))
(define (cadr x)   (first (rest x)))
(define (cdar x)   (rest (first x)))
(define (cddr x)   (rest (rest x)))
(define (caaar x)  (first (first (first x))))
(define (caadr x)  (first (first (rest x))))
(define (cadar x)  (first (rest (first x))))
(define (caddr x)  (first (rest (rest x))))
(define (cdaar x)  (rest (first (first x))))
(define (cdadr x)  (rest (first (rest x))))
(define (cddar x)  (rest (rest (first x))))
(define (cdddr x)  (rest (rest (rest x))))
(define (caaaar x) (first (first (first (first x)))))
(define (caaadr x) (first (first (first (rest x)))))
(define (caadar x) (first (first (rest (first x)))))
(define (caaddr x) (first (first (rest (rest x)))))
(define (cadaar x) (first (rest (first (first x)))))
(define (cadadr x) (first (rest (first (rest x)))))
(define (caddar x) (first (rest (rest (first x)))))
(define (cadddr x) (first (rest (rest (rest x)))))
(define (cdaaar x) (rest (first (first (first x)))))
(define (cdaadr x) (rest (first (first (rest x)))))
(define (cdadar x) (rest (first (rest (first x)))))
(define (cdaddr x) (rest (first (rest (rest x)))))
(define (cddaar x) (rest (rest (first (first x)))))
(define (cddadr x) (rest (rest (first (rest x)))))
(define (cdddar x) (rest (rest (rest (first x)))))
(define (cddddr x) (rest (rest (rest (rest x)))))

;-----------------------------------------------------------------
; ATOM.
;-----------------------------------------------------------------
; Nel Lisp originale (atom '()) restituisce 't' (true)
; In newLISP (atom? '()) restituisce 'nil'
; 
; Questo è IMPORTANTISSIMO perché cambia il comportamento di 'eval.'
; Quindi definiamo una funzione 'atom.' che si comporta come il Lisp e
; la usiamo al posto di 'atom'
; Questo è necessario perché nel Lisp del 1960 'nil' e '()' sono atomi.
; In newLISP invece no.
; newLISP
(define (atom. x)
  (or (atom? x) (null. x)))

(atom. '())
;-> true
(atom. '(1))
;-> nil
(atom. zz)
;-> true
(atom. 'zz)
;-> true
(setq ww 1)
(atom. ww)
;-> true
(atom. 'ww)
;-> true

;-----------------------------------------------------------------
; NULL
;-----------------------------------------------------------------
; Common Lisp
(defun null. (x)
  (eq x '()))

; newLISP
(define (null. x)
  (= x '()))

(null. 1)
;-> nil
(null. '())
;-> true

;-----------------------------------------------------------------
; AND
;-----------------------------------------------------------------
; Common Lisp
(defun and. (x y)
  (cond (x (cond (y 't) ('t '())))
        ('t '())))

; newLISP
(define (and. x y)
  (cond (x (cond (y true) (true '())))
        (true '())))

(and. true true)
;-> true
(and. true '())
;-> ()
(and. '() '())
;-> ()
(and. true '())
;-> ()

;-----------------------------------------------------------------
; NOT
;-----------------------------------------------------------------
; Common Lisp
(defun not. (x)
  (cond (x '())
        ('t 't)))

; newLISP
(define (not. x)
  (cond (x '())
        (true true)))

(not. true)
;-> ()
(not. '())
;-> true

;-----------------------------------------------------------------
; APPEND
;-----------------------------------------------------------------
; Common Lisp
(defun append. (x y)
  (cond ((null. x) y)
        ('t (cons (car x) (append. (cdr x) y)))))

; newLISP
(define (append. x y)
  (cond ((null. x) y)
        (true (cons (car x) (append. (cdr x) y)))))

(append. '(1 2) '(3 4))
;-> (1 2 3 4)
(append. '() '(3 4))
;-> (3 4)
(append. '(1 2) '())
;-> (1 2)

;-----------------------------------------------------------------
; LIST
;-----------------------------------------------------------------
; Common Lisp
(defun list. (x y)
  (cons x (cons y '())))

; newLISP
(define (list. x y)
  (cons x (cons y '())))

(list. 1 3)
;-> (1 3)

;-----------------------------------------------------------------
; PAIR
;-----------------------------------------------------------------
; Common Lisp
(defun pair. (x y)
  (cond ((and. (null. x) (null. y)) '())
        ((and. (not. (atom x)) (not. (atom y)))
         (cons (list. (car x) (car y))
               (pair. (cdr x) (cdr y))))))

; newLISP
(define (pair. x y)
  (cond ((and. (null. x) (null. y)) '())
        ((and. (not. (atom. x)) (not. (atom. y)))
         (cons (list. (car x) (car y))
               (pair. (cdr x) (cdr y))))))

(pair. '(1) '(2))
;-> ((1 2))

(pair. '(1 2) '(3 4))
;-> ((1 3) (2 4))

;-----------------------------------------------------------------
; ASSOC
;-----------------------------------------------------------------
; Common Lisp
(defun assoc. (x y)
  (cond ((eq (caar y) x) (cadar y))
        ('t (assoc. x (cdr y)))))

; newLISP
(define (assoc. x y)
  (cond ((null. y) '()) ; perchè (cdr y) può restituire errore in newLISP (in CL restituisce nil)
        ((= (caar y) x) (cadar y))
        (true (assoc. x (cdr y)))))

(setq L '((1 a) (2 b)))
(assoc. 1 L)
;-> a
(assoc. 3 L)
;-> ()

;-----------------------------------------------------------------
; EVCON
;-----------------------------------------------------------------
; Common Lisp
(defun evcon. (c a)
  (cond ((eval. (caar c) a)
         (eval. (cadar c) a))
        ('t (evcon. (cdr c) a))))

; newLISP
(define (evcon. c a)
  (cond ((eval. (caar c) a)
         (eval. (cadar c) a))
        (true (evcon. (cdr c) a))))

;-----------------------------------------------------------------
; EVLIS
;-----------------------------------------------------------------
; Common Lisp
(defun evlis. (m a)
  (cond ((null. m) '())
        ('t (cons (eval.  (car m) a)
                  (evlis. (cdr m) a)))))

; newLISP
(define (evlis. m a)
  (cond ((null. m) '())
        (true (cons (eval.  (car m) a)
                    (evlis. (cdr m) a)))))

;-----------------------------------------------------------------
; EVAL
;-----------------------------------------------------------------
; Common Lisp
(defun eval. (e a)
  (cond
    ((atom e) (assoc. e a))
    ((atom (car e))
     (cond
       ((eq (car e) 'quote) (cadr e))
       ((eq (car e) 'atom)  (atom   (eval. (cadr e) a)))
       ((eq (car e) 'eq)    (eq     (eval. (cadr e) a)
                                    (eval. (caddr e) a)))
       ((eq (car e) 'car)   (car    (eval. (cadr e) a)))
       ((eq (car e) 'cdr)   (cdr    (eval. (cadr e) a)))
       ((eq (car e) 'cons)  (cons   (eval. (cadr e) a)
                                    (eval. (caddr e) a)))
       ((eq (car e) 'cond)  (evcon. (cdr e) a))
       ('t (eval. (cons (assoc. (car e) a)
                        (cdr e))
                  a))))
    ((eq (caar e) 'label)
     (eval. (cons (caddar e) (cdr e))
            (cons (list. (cadar e) (car e)) a)))
    ((eq (caar e) 'lambda)
     (eval. (caddar e)
            (append. (pair. (cadar e) (evlis. (cdr e) a))
                     a)))))

; newLISP
(define (eval. e a)
  (cond
    ; variabile
    ((atom. e)
     (assoc. e a))
    ; forma speciale o funzione
    ((atom. (car e))
     (cond
       ; quote
       ((= (car e) 'quote)
        (cadr e))
       ; atom
       ((= (car e) 'atom)
        (atom. (eval. (cadr e) a)))
       ; eq
       ((= (car e) 'eq)
        (= (eval. (cadr e) a)
           (eval. (caddr e) a)))
       ; car
       ((= (car e) 'car)
        (car (eval. (cadr e) a)))
       ; cdr
       ((= (car e) 'cdr)
        (cdr (eval. (cadr e) a)))
       ; cons
       ((= (car e) 'cons)
        (cons (eval. (cadr e) a)
              (eval. (caddr e) a)))
       ; cond
       ((= (car e) 'cond)
        (evcon. (cdr e) a))
       ; applicazione funzione
       (true
        (eval.
          (cons (assoc. (car e) a)
                (cdr e))
          a))))
    ; label
    ((= (caar e) 'label)
     (eval.
       (cons (caddar e) (cdr e))
       (cons (list. (cadar e) (car e)) a)))
    ; lambda
    ((= (caar e) 'lambda)
     (eval.
       (caddar e)
       (append.
         (pair. (cadar e)
                (evlis. (cdr e) a))
         a)))))

La linea "(= (caar e) lambda)" è stata sostituita con: "(= (caar e) 'lambda.)".
Infatti nel meta-circolare di John McCarthy (lambda (x) ...) non è una lambda di newLISP.
È soltanto una LISTA DI DATI che rappresenta una funzione Lisp.
Quindi (caar e) deve restituire il simbolo lambda e non una vera closure/function di newLISP.
Ma 'lambda e' riservata in newlisp, quindi ridefiniamo il simbolo 'lambda' come 'lambda.'
In questo modo le espressioni da valutare diventano:
  '((lambda. (x) (cons x '(b))) '(a))
invece di:
  '((lambda (x) (cons x '(b))) '(a))

Il problema reale è che il meta-interprete di McCarthy assume:
- valutazione simbolica pura
- nessuna valutazione automatica degli argomenti
mentre newLISP valuta automaticamente gli argomenti delle funzioni.

La soluzione migliore sarebbe quella di rinominare i simboli speciali del Lisp originale.
Per esempio:
  lambda  -> lambda.
  label   -> label.
  quote   -> quote.
  cond    -> cond.

Questa è una tecnica standard quando si implementa un meta-circolare Lisp dentro un altro Lisp con simboli riservati.

Per esempio questo è SBAGLIATO: (eval. ((lambda (x) x) 'a) '())
perché newLISP tenta di valutare: ((lambda (x) x) 'a) prima di chiamare eval..
Bisogna invece scrivere: (eval. '((lambda (x) x) 'a) '())
cioè con quote esterna.


------------------
Cuscinetti a sfera
------------------

Abbiamo un cuscinetto a sfera che rappresentiamo come da due cerchi concentrici e con un numero di cerchi (sfere del cuscinetto) posizionate in modo uniforme tra i due cerchi.
Dati il diametro del cerchio esterno D1 e il diametro del cerchio interno D2, determinare il massimo numero di cerchi/sfere che possiamo inserire tra i due cerchi (N) e la distanza tra due cerchi/sfere (gap).

Le sfere si dispongono sulla pista mediana di diametro:

  Dm = (D1 + D2) / 2.

Il diametro massimo di ogni sfera è limitato dallo spazio radiale disponibile:

  ds = (D1 − D2) / 2.

Il numero massimo di sfere N si ricava dalla condizione che la corda tra due centri adiacenti (sulla pista mediana) sia almeno pari a 'ds':

  (2 * Rm sin(pi/N) >= ds  -->  N = floor(pi / arcsin(ds / (D1 + D2)))
  dove Rm è il raggio della pista mediana.

La distanza libera tra due sfere consecutive è la corda tra i centri meno il diametro di una sfera:

  gap = 2 * Rm * sin(pi/N) − ds.

Per N massimo questo valore è minimo (>= 0).

Angolo tra due sfere adiacenti:
  theta = 2*pi/N      (radianti)
  theta = 360/N       (gradi)

Distanza tra i centri di due sfere adiacenti (corda):

  dcc = 2 * Rm * sin(pi/N)
  dove Rm = (D1 + D2) / 4

Distanza libera tra le superfici (gap):

  gap = dcc - ds = 2 * Rm * sin(pi/N) - (D1 - D2) / 2

Relazione tra le tre quantità:

  theta = 2*pi/N
  dcc   = 2 * Rm * sin(theta/2)
  gap   = dcc - ds

; Funzione che prende due diametri D1 e D2 e restituisce una lista:
; (N gap theta dcc-arc dcc-chord)
; dove: N = il numero massimo di sfere tra D1 e D2
;       gap = la distanza tra due sfere
;       theta = l'angolo tra due sfere
;       dcc-arc = la distanza tra i centri di due sfere (arco)
;       dcc-chord = la distanza tra i centri di due sfere (corda)
(define (ball-bearing D1 D2)
  (local (ds Rm sin-val N gap theta dcc pi)
    ; pi
    (setq pi (mul 2 (acos 0)))
    ; ds = diametro massimo delle sfere = spazio radiale disponibile
    (setq ds (div (sub D1 D2) 2))
    ; Rm = raggio della pista mediana
    (setq Rm (div (add D1 D2) 4))
    ; sin-val = ds / (2*Rm) usato per calcolare l'angolo minimo tra due sfere
    (setq sin-val (div ds (mul 2 Rm)))
    ; valori di default
    (setq N 0  gap 0  theta 0  dcc 0)
    (when (and (> D1 D2) (> D2 0) (<= sin-val 1))
      ; N = floor(pi / arcsin(ds / (2*Rm)))
      (setq N (int (div pi (asin sin-val))))
      (when (< N 2) (setq N 0))
      (when (> N 0)
        ; theta = 360/N  (angolo tra due sfere adiacenti, in gradi)
        (setq theta (div 360 N))
        ; dcc-arc  = Rm * 2*pi/N  (distanza centro-centro lungo l'arco)
        (setq dcc-arc (mul Rm (div (mul 2 pi) N)))
        ; dcc-chord = 2 * Rm * sin(pi/N)  (distanza centro-centro in linea retta)
        (setq dcc-chord (mul 2 Rm (sin (div pi N))))
        ; gap = dcc-chord - ds  (distanza libera tra le superfici, usando la corda)
        (setq gap (sub dcc-chord ds))))
    (list N gap theta dcc-arc dcc-chord)))

Proviamo:

(ball-bearing 80 40)
;-> (9 0.5212085995401239 40 20.94395102393196 20.52120859954012)
(ball-bearing 100 60)
;-> (12 0.7055236082016592 30 20.94395102393195 20.70552360820166)
(ball-bearing 50 45)
;-> (59 0.02805330500348591 6.101694915254237 2.529248322805342 2.528053305003486)
(ball-bearing 100 10)
;-> (3 2.631397208144122 120 57.59586531581287 47.63139720814412)
(ball-bearing 30 28)
;-> (91 0.00096803012153579 3.956043956043956 1.001166889605538 1.000968030121536)


-------------------
Matrici di Hadamard
-------------------

Una matrice di Hadamard (Hn) di ordine n è una matrice n x n contenente solo 1 e -1 tale che:

  Hn*Hn(Trasposta) = n*In

dove In è la matrice identità n x n.

In base a questa definizione la matrice H è non singolare ed invertibile e quindi il prodotto è commutativo:

 Hn*Hn(Trasposta) = Hn(Trasposta)*Hn = Hn*Hn

Una matrice di Hadamard ha il più grande determinante possibile (in valore assoluto) per qualsiasi matrice nxn i cui elementi sono compresi nell'intervallo [1,-1].

Le matrici di Hadamard trovano applicazione nei codici di correzione degli errori e nei modelli di pesatura ottimali.

Costruzione di Sylvester
------------------------
La costruzione di Sylvester è un metodo per creare una matrice di Hadamard di dimensione 2*n data Hn.
H2n può essere costruita come:

       |Hn  Hn|
 H2n = |      |
       |Hn -Hn|

Esempio:

  H1 = ((1))
  
       |1  1|
  H2 = |    |
       |1 -1|

; Funzione che stampa una matrice di Hadamard [- 1 1]
(define (print-grid matrix)
  (let ( (row (length matrix)) (col (length (first matrix))) )
    (for (i 0 (- row 1))
      (for (j 0 (- col 1))
        (print (format "%3d" (matrix i j))))
      (println)) '>))

; Funzione che crea una matrice di Hadamard (metodo di Sylvester)
; Input:  matrice Hn
; Output: matrice H2n
(define (sylv h)
  (let (h2 '())
    ; inserisce |Hn  Hn| in h2
    (dolist (el h) (push (append el el) h2 -1))
    ; inserisce |Hn -Hn| in h2
    (dolist (el h) (push (append el (map (curry * -1) el)) h2 -1))
  h2))

(setq a '((1)))
(print-grid (setq b (sylv a)))
;-> 1  1
;-> 1 -1

(setq c '((1 1) (1 -1)))
(print-grid (setq c (sylv b)))
;-> 1  1  1  1
;-> 1 -1  1 -1
;-> 1  1 -1 -1
;-> 1 -1 -1  1

(print-grid (setq d (sylv c)))
;-> 1  1  1  1  1  1  1  1
;-> 1 -1  1 -1  1 -1  1 -1
;-> 1  1 -1 -1  1  1 -1 -1
;-> 1 -1 -1  1  1 -1 -1  1
;-> 1  1  1  1 -1 -1 -1 -1
;-> 1 -1  1 -1 -1  1 -1  1
;-> 1  1 -1 -1 -1 -1  1  1
;-> 1 -1 -1  1 -1  1  1 -1

(hadamard? (sylv c))
;-> true

(setq d '((-1 1 -1) (1 -1 -1) (-1 -1 1)))

; Funzione che verifica se una data matrice è una matrice Identità
; (1 nella diagonale principale e tutti gli altri elementi 0)
(define (identity? matrix)
  (let ( (row (length matrix)) (col (length (first matrix))) (error nil) )
    (for (r 0 (- row 1))
      (for (c 0 (- col 1))
        (if (or (and (= r c) (!= (matrix r c) 1))
                (and (!= r c) (!= (matrix r c) 0)))
            (setq error true))))
    (not error)))

(identity? '((1 0 0) (0 1 0) (0 0 1)))
;-> true
(identity? '((1 0 0) (0 1 0) (0 1 1)))
;-> nil

; Funzione che verifica se una data matrice è una matrice di Hadamard
(define (hadamard? matrix)
  ;(tutti valori 1 o -1) AND (Hn*Hn = In)
  (and (for-all (fn(x) (or (= x 1) (= x -1))) (flat matrix))
       (identity? (mat / (multiply matrix matrix) (length matrix)))))

Proviamo:

(map hadamard? (list a b c d))
;-> (true true true true)

Costruzione di Kronecker
------------------------
Il prodotto di Kronecker, indicato con "**", è un'operazione tra due matrici di dimensioni arbitrarie, sempre applicabile, al contrario della normale moltiplicazione di matrici.
Se A è una matrice (m x n) e B è una matrice (p x q), allora il loro prodotto di Kronecker A**B è una matrice (m*p x n*q) definita a blocchi nel modo seguente:

           | a11*B ... a1n*B |
  A ** B = |  ...  ... ...   | = C
           | am1*B ... amn*B |

Per esempio se A è una matrice 2x2:

                             | a11*b11 a11*b12  a12*b11 a12*b12 |
           | a11*B a12*B |   | a11*b21 a11*b22  a12*b21 a12*b22 |
  A ** B = |             | = |                                  | = C
           | a21*B a22*B |   | a21*b11 a21*b12  a22*b11 a22*b12 |
                             | a21*b21 a21*b22  a22*b21 a22*b22 |

Se A e B sono matrici di Hadamard, allora anche C è una matrice di Hadamard.

; Funzione che calcola il prodotto di kronecker tra due matrici
; Matrice A(m x n) --> (r1xc1)
; Matrice B(p x q) --> (r2xc2)
(define (kron A B)
  (letn ( (r1 (length A))
          (c1 (length (A 0)))
          (r2 (length B))
          (c2 (length (B 0)))
          (C (array (mul r1 r2) (mul c1 c2) '(0))) )
    (for (i 0 (- r1 1))
      (for (j 0 (- c1 1))
        (for (k 0 (- r2 1))
          (for (l 0 (- c2 1))
            ; ogni elemento della matrice A viene
            ; moltiplicato per tutta la matrice B
            ; e memorizzato nella matrice C (C)
            (setf (C (+ (* i r2) k) (+ (* j c2) l))
                  (mul (A i j) (B k l)))))))
    (array-list C)))

Proviamo:

(hadamard? (kron a b))
;-> true

(hadamard? (kron b c))
;-> true

(hadamard? (kron c d))
;-> true

(setq m1 '((2 5) (2 9)))
(setq m2 '((1 6) (-7 4)))
(hadamard? (kron m1 m2))
;-> nil

(setq m1 '((1 1) (1 1)))
(setq m2 '((1 1) (-1 1)))
(hadamard? (kron m1 m2))
;-> nil

Matrici equivalenti
-------------------
Ricordiamo che su una matrice quadrata M possiamo eseguire le operazioni elementari sulle righe per trasformarla in una equivalente.
Operazioni elementari:
- Scambiare tra loro due righe.
- Sommare o sottrarre ad una riga un'altra riga.
- Moltiplicare una riga per uno scalare diverso da zero.
Operazioni elementari combinate:
- Sommare o sottrarre ad una riga un'altra riga moltiplicata per uno scalare diverso da zero.
- Sommare o sottrarre ad una riga la somma di due o più righe.
E poiché risulta det(M) = det(M(Trasposta)) le stesse operazioni possono essere eseguite sulle colonne.

Sulle matrici di Hadamard possiamo invece eseguire solo un numero limitato di queste operazioni per ottenere una matrice equivalente.
Operazioni consentite:
- scambiare tra loro due righe o due colonne
- moltiplicare gli elementi di una riga o di una colonna per -1 (negazione di riga o di colonna)
- effettuare una trasposizione (operazione che combina le due precedenti)

Esistono altri metodi per la costruzione delle matrici di Hadamard:
- costruzione di Paley
- costruzione di Williamson
- costruzione con matrici binarie

Codici sequenziali
------------------
Definiamo come sequenze di riga il numero di cambiamenti di segno di ogni riga e come codice sequenziale la sequenza di questi numeri, come spiega la seguente tabella:

  Righe 0 1 2 3 4 5 6 7
  H2    0 1
  H4    0 3 1 2
  H8    0 7 3 4 1 6 2 5

Ogni matrice H ha quindi un suo codice sequenziale che è unico e la identifica.
Come si vede i codici sequenziali non collimano con l'ordine delle righe.
Quindi per calcolare il codice sequenziale di una matrice occorre contare e memorizzare i passaggi di segno di ogni riga.

(define (same-sign? x y)
"Check if two numbers have same sign"
  (= (> x 0) (> y 0)))

; Funzione che calcola il codice sequenziale di una matrice
; Restituisce la lista dei passaggi di segno di ogni riga della matrice
(define (code matrix)
  (let (out '())
    (dolist (row matrix)
      (push (first (count '(nil)
                    (map same-sign? (rest row) (chop row)))) out -1))))

Proviamo:

(code b)
;-> (0 1)
(code c)
;-> (0 3 1 2)
(code d)
;-> (0 7 3 4 1 6 2 5)


------------------
Telefono difettoso
------------------

Abbiamo un telefono che funziona in modo anomalo.
Quando inseriamo un numero la telefonata parte appena trova una corrispondenza con un numero in rubrica.
Data una lista di numeri telefonici, determinare quali numeri non possono essere chiamati con la tastiera.

Esempio:
Rubrica = (911 97625999 91125426)
Con questa rubrica non possiamo mai inserire il numero 91125426, perchè appena digitiamo 911 parte la chiamata al 911 (cioè un numero che abbiamo in rubrica).

Algoritmo
1) Ordinare i numeri di telefono in ordine lessicografico non decrescente.
2) Controllare se ogni numero di telefono è un prefisso del numero successivo.


(define (phone lst)
  (sort lst)
  (dolist (el (chop lst))
    (if (starts-with (lst (+ $idx 1)) el)
        (println el { } (lst (+ $idx 1))))) '>)

(setq L '("113" "054442781" "054123456" "12345" "123"))
(phone L)
;-> 123 12345

(setq L '("911" "97625999" "91125426"))
(phone L)
;-> 911 91125426


----------------------------
Compressione teorica di file
----------------------------

Dati N file diversi, è possibile memorizzarli come file compressi di lunghezza al massimo b?
Teoricamente, il codice sorgente del programma di decompressione potrebbe contenere il contenuto di tutti i file, ma deve essere in grado di distinguerli.
Quindi, la domanda è: quanti file unici di lunghezza al massimo b bit si possono avere?

Considerazioni:
- Si possono avere file compressi di lunghezza 0, ..., b.
- 2^b file unici di lunghezza b, 2^(b-1) di lunghezza (b - 1), ecc.
- Somma[i=0,b](2^i) = 2^(b+1) - 1
Quindi:

  if ((2^(b+1) - 1) >= N)
     (print "si")
     ;else
     (print "no")

Se consideriamo tutti i possibili bitstring di lunghezza da '0' a 'b' e vogliamo associare a ciascun file originale un codice compresso univoco, allora il numero massimo di codici distinti disponibili è:

  Sum[i=0,b](2^i) = 2^(b+1) - 1

Di conseguenza, per poter rappresentare N file distinti serve:

  N <= 2^(b+1) - 1

e quindi il codice logico è:

  (if (>= (- (pow 2 (+ b 1)) 1) N)
      (println "si")
      (println "no"))

La parte teorica importante è questa:
- il decompressore può contenere arbitrariamente molti dati "hardcoded"
- il file compresso serve solo come indice/selettore
- quindi basta avere abbastanza codici distinti

Per esempio con b = 3 i codici possibili sono:

  lunghezza 0: 1
  lunghezza 1: 2
  lunghezza 2: 4
  lunghezza 3: 8
  totale = 15

quindi si possono distinguere al massimo 15 file diversi.

Questo è anche collegato al principio fondamentale della teoria dell'informazione:
non esiste una compressione lossless universale che accorci tutti i file, perché i codici corti disponibili sono troppo pochi rispetto ai file possibili.


----------------
Testo scorrevole
----------------

Creazione di un semplice testo scorrevole.

(define (cls)
"Clear screen of REPL (ANSI sequence)"
  (print "\027[H\027[2J"))

(setq A1 " L      OOO  V   V EEEEE             ")
(setq A2 " L     O   O V   V E                 ")
(setq A3 " L     O   O V   V EEEE              ")
(setq A4 " L     O   O  V V  E                 ")
(setq A5 " LLLLL  OOO    V   EEEEE             ")
(setq banner (list A1 A2 A3 A4 A5))

(define (move txt dir pause)
  (let ((linee (length txt))
        (vuota (dup " " (length (txt 0)))))
    (while true
      (cls)
      (for (i 0 (- linee 1))
        (print vuota "\r")
        (println (txt i))
        (rotate (txt i) dir))
      (sleep pause))))

Proviamo:

; da sx a dx
(move banner 1 50)
; da dx a sx
(move banner -1 75)

============================================================================

