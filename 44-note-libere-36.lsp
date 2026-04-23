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

============================================================================

