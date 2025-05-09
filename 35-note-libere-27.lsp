================

 NOTE LIBERE 27

================

  Questo non è Pi Greco
  3.1415926535897932384626433832795028841971693993751058209749445923078164
  062862089986280348253421170679821480865132823066470938446095505822317253
  594081284811174502841027019385211055596446229489549303819644288109756659
  334461284756482337867831652712019091456485669234603486104543266482133936
  072602491412737245870066063155881748815209209628292540917153643678925903
  600113305305488204665213841469519415116094330572703657595919530921861173
  819326117931051185480744623799627495673518857527248912279381830119491298
  336733624406566430860213949463952247371907021798609437027705392171762931
  767523846748184676694051320005681271452635608277857713427577896091736371
  787214684409012249534301465495853710507922796892589235420199561121290219
  608640344181598136297747713099605187072113499999983729780499510597317328
  160963185950244594553469083026425223082533446850352619311881710100031378
  387528865875332083814206171776691473035982534904287554687311595628638823
  537875937519577818577805321712268066130019278766111959092164201989......

----------------------------------------
Percorso minimo e massimo in una matrice
----------------------------------------

Data una matrice MxN di numeri interi determinare il percorso a somma minima partendo dalla cella in alto a sinistra (0,0) e terminando nella cella in basso a destra (M-1,N-1).
I movimenti possibili sono a destra e in basso.

Partendo da (0,0) scegliamo il valore minimo (massimo) per il prossimo movimento e andiamo avanti a riempire la matrice con la somma del percorso fino a quel punto.
Al termine il valore del percorso minimo (massimo) si trova nella cella (M-1,N-1) della matrice.
Per capire meglio il procedimento, le funzioni stampano i risultati intermedi.

(setq m '((1 3 2) (2 6 2) (3 3 1)))

(define (print-matrix matrix ch0 ch1)
  (local (row col)
    (setq row (length matrix))
    (setq col (length (matrix 0)))
    (for (i 0 (- row 1))
      (for (j 0 (- col 1))
        (print (format "%3d" (matrix i j)))
      )
      (println)) '>))

(print-matrix m)
;-> 1  3  2
;-> 2  6  2
;-> 3  3  1

Funzione che calcola il percorso minimo:

(define (path-min matrix)
  (local (rows cols destra baso))
  (setq rows (length matrix))
  (setq cols (length (matrix 0)))
  (print-matrix matrix)
  (for (r 0 (- rows 1))
    (for (c 0 (- rows 1))
      (cond ((and (zero? r) (zero? c)) nil)
            (true
              (setq destra 999999) (setq basso 999999)
              (if (!= r 0) (setq basso (+ (matrix r c) (matrix (- r 1) c))))
              (if (!= c 0) (setq destra (+ (matrix r c) (matrix r (- c 1)))))
              (println "basso: " basso { - } "destra: " destra)
              (setf (matrix r c) (min destra basso))
              (print-matrix matrix)))))
  (matrix (- rows 1) (- cols 1)))

(path-min m)
;->   1  3  2
;->   2  6  2
;->   3  3  1
;-> basso: 999999 - destra: 4
;->   1  4  2
;->   2  6  2
;->   3  3  1
;-> basso: 999999 - destra: 6
;->   1  4  6
;->   2  6  2
;->   3  3  1
;-> basso: 3 - destra: 999999
;->   1  4  6
;->   3  6  2
;->   3  3  1
;-> basso: 10 - destra: 9
;->   1  4  6
;->   3  9  2
;->   3  3  1
;-> basso: 8 - destra: 11
;->   1  4  6
;->   3  9  8
;->   3  3  1
;-> basso: 6 - destra: 999999
;->   1  4  6
;->   3  9  8
;->   6  3  1
;-> basso: 12 - destra: 9
;->   1  4  6
;->   3  9  8
;->   6  9  1
;-> basso: 9 - destra: 10
;->   1  4  6
;->   3  9  8
;->   6  9  9
;-> 9

Funzione che calcola il percorso massimo:

(define (path-max matrix)
  (local (rows cols destra baso))
  (setq rows (length matrix))
  (setq cols (length (matrix 0)))
  (print-matrix matrix)
  (for (r 0 (- rows 1))
    (for (c 0 (- rows 1))
      (cond ((and (zero? r) (zero? c)) nil)
            (true
              (setq destra -1) (setq basso -1)
              (if (!= r 0) (setq basso (+ (matrix r c) (matrix (- r 1) c))))
              (if (!= c 0) (setq destra (+ (matrix r c) (matrix r (- c 1)))))
              (println "basso: " basso { - } "destra: " destra)
              (setf (matrix r c) (max destra basso))
              (print-matrix matrix)))))
  (matrix (- rows 1) (- cols 1)))

(path-max m)
;->   1  3  2
;->   2  6  2
;->   3  3  1
;-> basso: -1 - destra: 4
;->   1  4  2
;->   2  6  2
;->   3  3  1
;-> basso: -1 - destra: 6
;->   1  4  6
;->   2  6  2
;->   3  3  1
;-> basso: 3 - destra: -1
;->   1  4  6
;->   3  6  2
;->   3  3  1
;-> basso: 10 - destra: 9
;->   1  4  6
;->   3 10  2
;->   3  3  1
;-> basso: 8 - destra: 12
;->   1  4  6
;->   3 10 12
;->   3  3  1
;-> basso: 6 - destra: -1
;->   1  4  6
;->   3 10 12
;->   6  3  1
;-> basso: 13 - destra: 9
;->   1  4  6
;->   3 10 12
;->   6 13  1
;-> basso: 13 - destra: 14
;->   1  4  6
;->   3 10 12
;->   6 13 14
;-> 14

Vedi anche "Percorsi in una matrice" su "Note libere 19".


--------------------
Centrare una stringa
--------------------

Scriviamo una funzione che centra una stringa all'interno di uno spazio di caratteri prefissato.
Per esempio:
  stringa = "123"
  spazio = 7
  stringa centrata = "  123  "
La 'stringa centrata' è lunga 7 caratteri e contiene al centro la stringa "123".

Quando la lunghezza della stringa e lo spazio hanno parità diverse (pari e dispari o dispari e pari), allora non è possibile centrare esattamente la stringa. In tal caso la stringa viene centrata spostandola di un carattere verso sinistra).
Per esempio:
  stringa = "casa"
  spazio = 7
  stringa centrata = " casa  "
La stringa "casa" non può essere centrata esattamente in 7 caratteri e quindi ha un carattere di spaziatura in meno sulla sinistra.

Quando la stringa è più lunga dello spazio, allora restituiamo la stringa.
Per rendere più generica la funzione inseriamo il parametro 'fill-char' che rappresenta il carattere di riempimento degli spazi (default " ").

Funzione che centra una stringa in un determinato spazio di caratteri:

(define (center-string str num-chars fill-char)
  (letn ( (len-str (length str)) (space (- num-chars len-str)) )
    (setq fill-char (or fill-char " "))
    (if (even? space)
      ; centratura pari
      (append (dup fill-char (/ space 2)) str (dup fill-char (/ space 2)))
      ; centratura dispari
      (append (dup fill-char (/ space 2)) str (dup fill-char (+ (/ space 2) 1))))))

Proviamo:

(center-string "newLISP" 15 " ")
;-> "    newLISP    "

(center-string "newLISP" 15)
;-> "    newLISP    "

(center-string "newLISP" 15 "-")
;-> "----newLISP----"

(center-string "newLISP" 5)
;-> "newLISP"

Funzione che disegna un albero di natale:

(define (draw-tree levels)
  (let ( (last-line-length (- (* levels 2) 1))
         (stars 1) )
  (for (i 1 levels)
    (println (center (dup "*" stars) last-line-length))
    (++ stars 2)) '>))

(draw-tree 11)
(draw-tree 40)
;->           *
;->          ***
;->         *****
;->        *******
;->       *********
;->      ***********
;->     *************
;->    ***************
;->   *****************
;->  *******************
;-> *********************


----------------------
Numero di coppie buone
----------------------

Data una lista di numeri interi L, scrivere una funzione che restituisce il numero di coppie buone.
Una coppia di indici (i, j) è detta 'Buona' se L[i] == L[j] e i < j.

Soluzione Brute-Force O(n^2):

Funzione che restituisce tutte le coppie buone:

(define (buone1 lst)
  (local (out len)
    (setq out '())
    (setq len (length lst))
    (for (i 0 (- len 2))
      (for (j (+ i 1) (- len 1))
        (if (= (lst i) (lst j)) (push (list i j) out -1))))
    out))

(setq a '(2 4 5 2 2 5))
(buone1 a)
;-> ((0 3) (0 4) (2 5) (3 4))

(setq b '(2 2 2 2))
(buone1 b)
;-> ((0 1) (0 2) (0 3) (1 2) (1 3) (2 3))

(setq c '(1 2 3 4 5))
(buone1 c)
;-> ()

Funzione che conta le coppie buone:

(define (buone2 lst)
  (local (conta len)
    (setq conta 0)
    (setq len (length lst))
    (for (i 0 (- len 2))
      (for (j (+ i 1) (- len 1))
        (if (= (lst i) (lst j)) (++ conta))))
    conta))

(buone2 a)
;-> 4

(buone2 b)
;-> 6

(buone2 c)
;-> 0

Soluzione con lista associativa (oppure hash-map) O(n):

Algoritmo:
(setq lst '(2 4 5 2 2 5))

Calcoliamo una lista associativa con elementi del tipo: (numero occorrenze)
(setq unici (unique lst))
;-> (2 4 5)
(setq link (map list unici (count unici lst)))
;-> ((2 3) (4 1) (5 2))

Adesso attraversiamo la lista e per ogni numero x che incontriamo:
1) diminuire di 1 le occorrenze di x nel relativo elemento della lista associata
2) aumentare il conteggio delle coppie buone del valore attuale delle occorrenze di x nel relativo elemento della lista associata

(setq coppie 0)
;-> 0
(dolist (el lst)
  (setf (lookup el link) (- $it 1))
  (++ coppie (lookup el link)))
;-> 4

(define (buone3 lst)
  (local (coppie unici link)
    (setq coppie 0)
    (setq unici (unique lst))
    (setq link (map list unici (count unici lst)))
    (dolist (el lst)
      (setf (lookup el link) (- $it 1))
      (++ coppie (lookup el link)))
    coppie))

(buone3 a)
;-> 4

(buone3 b)
;-> 6

(buone3 c)
;-> 0

Vediamo la velocità delle due funzioni:

(setq test (rand 100 1000))
(time (buone2 test))
;-> 471.736
(time (buone3 test))
;-> 0.952

La funzione 'buone3' è molto veloce, ma non è possibile estrapolare il valore delle coppie.


-----------------------
Madhava di Sangamagrama
-----------------------

Madhava di Sangamagrama (1350-1425) è un matematico indiano che intorno al 1400 scrisse la seguente poesia su Pi greco:

Dei (33), occhi (2), elefanti (8), serpenti (8), fuochi (3), qualità (3), veda (4), naksatra (27), elefanti (8) e braccia (2): il saggio dice che questa è la misura della circonferenza quando il raggio è 90,000,000,000.

Pi greco = 3.1415926535897931...
(setq pi 3.1415926535897931)

Prendiamo la lista dei numeri partendo dalla fine 282743388233 e dividiamolo per 90,000,000,000:

(setq madha (div 282743388233 90000000000))
;-> 3.141593202588889

Errore assoluto:
(abs (sub madha pi))
;-> 5.48999095961733e-007
(abs (sub 3.141593202588889 3.1415926535897931))
;-> 5.48999095961733e-007

Errore percentuale:
(div (abs (sub madha pi)) pi)
;-> 1.747518397505832e-007
(div (abs (sub 3.141593202588889 3.1415926535897931)) 3.1415926535897931)
;-> 1.747518397505832e-007

Madhava ha anche trovato diverse serie per il calcolo di Pi greco, tra cui la seguente che converge molto velocemente:

  Pi greco = sqrt(12)*(1 - 1/(3*3) + 1/(5 * 3^2) - 1/(7 * 3^3) + ...)

(define (madhava termini)
  (let ( (somma nil) (potenza 1) (base 3) (numero 3) (serie 1) )
    (for (i 1 termini)
      (if somma
          (setq serie (add serie (div (mul numero (pow base potenza)))))
          ;else
          (setq serie (sub serie (div (mul numero (pow base potenza)))))
      )
      (++ potenza)
      (++ numero 2)
      (setq somma (not somma))
    )
  (mul (sqrt 12) serie))

Proviamo:

(setq m20 (madhava 20))
;-> 3.141592653595635

(abs (sub pi m20))
;-> 5.841993555577574e-012


----------------------
Numero di coppie buone
----------------------

Data una lista di numeri interi L, scrivere una funzione che restituisce il numero di coppie buone.
Una coppia di indici (i, j) è detta 'Buona' se L[i] == L[j] e i < j.

Soluzione Brute-Force O(n^2):

Funzione che restituisce tutte le coppie buone:

(define (buone1 lst)
  (local (out len)
    (setq out '())
    (setq len (length lst))
    (for (i 0 (- len 2))
      (for (j (+ i 1) (- len 1))
        (if (= (lst i) (lst j)) (push (list i j) out -1))))
    out))

(setq a '(2 4 5 2 2 5))
(buone1 a)
;-> ((0 3) (0 4) (2 5) (3 4))

(setq b '(2 2 2 2))
(buone1 b)
;-> ((0 1) (0 2) (0 3) (1 2) (1 3) (2 3))

(setq c '(1 2 3 4 5))
(buone1 c)
;-> ()

Funzione che conta le coppie buone:

(define (buone2 lst)
  (local (conta len)
    (setq conta 0)
    (setq len (length lst))
    (for (i 0 (- len 2))
      (for (j (+ i 1) (- len 1))
        (if (= (lst i) (lst j)) (++ conta))))
    conta))

(buone2 a)
;-> 4

(buone2 b)
;-> 6

(buone2 c)
;-> 0

Soluzione con lista associativa (oppure hash-map) O(n):

Algoritmo:
(setq lst '(2 4 5 2 2 5))

Calcoliamo una lista associativa con elementi del tipo: (numero occorrenze)
(setq unici (unique lst))
;-> (2 4 5)
(setq link (map list unici (count unici lst)))
;-> ((2 3) (4 1) (5 2))

Adesso attraversiamo la lista e per ogni numero x che incontriamo:
1) diminuire di 1 le occorrenze di x nel relativo elemento della lista associata
2) aumentare il conteggio delle coppie buone del valore attuale delle occorrenze di x nel relativo elemento della lista associata

(setq coppie 0)
;-> 0
(dolist (el lst)
  (setf (lookup el link) (- $it 1))
  (++ coppie (lookup el link)))
;-> 4

(define (buone3 lst)
  (local (coppie unici link)
    (setq coppie 0)
    (setq unici (unique lst))
    (setq link (map list unici (count unici lst)))
    (dolist (el lst)
      (setf (lookup el link) (- $it 1))
      (++ coppie (lookup el link)))
    coppie))

(buone3 a)
;-> 4

(buone3 b)
;-> 6

(buone3 c)
;-> 0

Vediamo la velocità delle due funzioni:

(setq test (rand 100 1000))
(time (buone2 test))
;-> 471.736
(time (buone3 test))
;-> 0.952

La funzione 'buone3' è molto veloce, ma non è possibile estrapolare il valore delle coppie.


-----------------------------------------------------------
Numeri di Bernoulli e somma di potenze di interi successivi
-----------------------------------------------------------

La somma delle potenze di numeri successivi è definita nel modo seguente:

  Sum[k=1,n]k^m = 1^m + 2^m + 3^m + ...+ n^m

dove m e n sono numeri interi positivi.

Per m = 1: Sum[k=1,n]k = n*(n+1)/2

Per m = 2: Sum[k=1,n]k^2 = n*(n + 1)*(2*n + 1)/6

Per m = 3: Sum[k=1,n]k^3 = n^2*(n + 1)^2/4

La formula per un generico m è stata dimostrata da Jacobi ed è chiamata "Formula di Faulhaber":

                      1
  Sum[k=1,n]k^m = ---------*Sum[k=0,m]*(-1)^k*binom(m+1,k)*B(k)*n^(m+1-k)
                   (m + 1)

dove B(k) è il k-esimo numero di Bernoulli.

I numeri di Bernoulli sono una sequenza di costanti che compaiono nella formula per il calcolo della somma delle potenze dei primi N numeri interi positivi.
Le costanti e la formula per il loro calcolo sono state scoperte dal matematico svizzero Jacob Bernoulli (1654-1705) fu il primo a rendersi conto dell’esistenza di una singola sequenza di costanti che prevedono una formula uniforme per tutte le somme di potenze.

Data la funzione:

             x
  f(x) = ---------
          e^x - 1

I numeri di Bernoulli sono definiti come:

  B(n) = lim f(n)(x)
         x->0

dove f(n)(x) rappresenta la derivata n-esima di f(x)

  B(0) = lim f(x) = 1
         x->0

  B(1) = lim f'(x) = -1/2
         x->0

  B(2) = lim f''(x) = 1/6
         x->0

  B(3) = ...

Bernoulli ha dimostrato che i numeri B(n) possono essere calcolati ricorsivamente con la seguente formula:

  B(0) = 1, per n = 0

               1
  B(n) = - ---------*Sum[k=0,n-1]binom(n+1,k)*B(k), per n > 0
            (n + 1)

I primi valori dei numeri di Bernoulli sono i seguenti:

  +----+--------------+
  |  n |  B(n)        |
  +----+--------------+
  |  0 |  1           |
  |  1 |  -1/2        |
  |  2 |  1/6         |
  |  3 |  0           |
  |  4 |  -1/30       |
  |  5 |  0           |
  |  6 |  1/42        |
  |  7 |  0           |
  |  8 |  -1/30       |
  |  9 |  0           |
  | 10 |  5/66        |
  | 11 |  0           |
  | 12 |  -691/2730   |
  | 13 |  0           |
  | 14 |  7/6         |
  | 15 |  0           |
  | 16 |  -3617/510   |
  | 17 |  0           |
  | 18 |  43867/798   |
  | 19 |  0           |
  | 20 |  -174611/330 |
  | 21 |  0           |
  | 22 |  ...         |
  +-----+--------------+

(define (binom num k)
"Calculates the binomial coefficient (n k) = n!/(k!*(n - k)!) (combinations of k elements without repetition from n elements)"
  (cond ((> k num) 0)
        ((zero? k) 1)
        (true
          (let (r 1L)
            (for (d 1 k)
              (setq r (/ (* r num) d))
              (-- num)
            )
          r))))

(define (semplifica-frazione frazione)
    (letn ((num (first frazione))
          (den (last frazione))
          (divisor (gcd (abs num) (abs den))))
        (list (/ num divisor) (/ den divisor))))

(define (somma-frazioni a b)
    (let ((num-a (first a)) (den-a (last a))
          (num-b (first b)) (den-b (last b)))
        (semplifica-frazione (list (+ (* num-a den-b) (* num-b den-a)) (* den-a den-b)))))

(define (moltiplica-frazioni a b)
    (semplifica-frazione (list (* (first a) (first b)) (* (last a) (last b)))))

(define (bernoulli num)
  (setq bernoulli-memo '((0 (1 1))))
  (define (bernoulli-aux n)
    (local (memo bk binom-nk somma num den)
      (cond ((= n 0) '(1 1))  ; B(0) = 1 come frazione (1/1)
            (true
              (setq memo (lookup n bernoulli-memo))
              (if memo
                  memo
                  (let (somma '(0 1))
                    (for (k 0 (- n 1))
                      (setq bk (bernoulli-aux k))
                      (setq binom-nk (binom (+ n 1) k))
                      (setf somma (somma-frazioni somma (moltiplica-frazioni bk (list binom-nk 1))))
                      (setq num (- (somma 0)))
                      (setq den (* (+ n 1) (somma 1)))
                    )
                    (push (list n (semplifica-frazione (list num den))) bernoulli-memo -1)
                    (semplifica-frazione (list num den))))))))
  (bernoulli-aux num)
  bernoulli-memo)

Proviamo:

(bernoulli 22)
;-> ((0 (1 1)) (1 (-1 2)) (2 (1 6)) (3 (0 1)) (4 (-1 30)) (5 (0 1))
;->  (6 (1 42)) (7 (0 1)) (8 (-1 30)) (9 (0 1)) (10 (5 66)) (11 (0 1))
;->  (12 (-691 2730)) (13 (0 1)) (14 (7 6)) (15 (0 1)) (16 (-3617 510))
;->  (17 (0 1)) (18 (43867 798)) (19 (0 1)) (20 (-174611 330)) (21 (0 1))
;->  (22 (854513 138)))


-------------------------------
Selezione casuale con blacklist
-------------------------------

Abbiamo un intero n e una lista di interi univoci nell'intervallo [0, n - 1] (blacklist).
Scrivere una funzione che seleziona casualmente un intero casuale nell'intervallo [0, n - 1] che non sia nella blacklist.
Qualsiasi intero nell'intervallo e non nella blacklist deve avere la stessa probabilità di essere selezionato.

Algoritmo
1) Generare la sequenza dei numeri 0..(n-1)
2) Calcolare la differenza insiemistica tra la sequenza e la blacklist.
3) Utilizzare rand per generare un indice compreso tra 0 e la lunghezza della lista differenza
4) Selezionare l'elemento di questo indice dalla lista differenza

Per esempio:

(setq n 7)
;-> 7
(setq nums (sequence 0 (- n 1)))
;-> (0 1 2 3 4 5 6)
(setq blacklist '(2 3 5))
;-> (2 3 5)
(setq diff (difference nums blacklist))
;-> (0 1 4 6)
(diff (rand (length diff)))
;-> 0
(diff (rand (length diff)))
;-> 4

Scriviamo la funzione:

(define (rand-bl num blacklist)
  (letn ( (nums (sequence 0 (- num 1)))
          (diff (difference nums blacklist)) )
    (diff (rand (length diff)))))

Proviamo:

(rand-bl 10 '(1 6 9))
;-> 0
(rand-bl 10 '(1 6 9))
;-> 8
(rand-bl 10 '(1 6 9))
;-> 5

Vediamo la frequenza dei numeri selezionati:

(count (sequence 0 9) (collect (rand-bl 10 '(1 6 9)) 10000))
;-> (1399 0 1429 1386 1410 1488 0 1438 1450 0)

Nota: l'algoritmo minimizza il numero di chiamate alla funzione casuale integrata "rand".


--------------------------------------------------
Operazioni aritmetiche (+,-,*,/) su interi-stringa
--------------------------------------------------

Funzioni per il calcolo delle quattro operazioni aritmetiche (+,-,*,/) su stringhe che rappresentano numeri interi.
I numeri negativi devono avere il segno "-".
I numeri positivi non devono avere il segno "+".

;---------------------
; Funzioni di supporto
;---------------------

; Funzione che verifica i risultati "-0":
(define (check-0 str) (if (= str "-0") "0" str))

; Funzione che verifica se il numero str1 è minore del numero str2
(define (smaller? str1 str2)
  (local (n1 n2 out)
    (setq n1 (length str1))
    (setq n2 (length str2))
    (cond ((> n1 n2) (setq out nil))
          ((< n1 n2) (setq out true))
          (true
            (setq out nil)
            (setq stop nil)
            (for (i 0 (- n1 1) 1 stop)
              (cond ((< (str1 i) (str2 i))
                     (set 'out true 'stop true))
                    ((> (str1 i) (str2 i))
                     (set 'out nil 'stop true))))))
    out))

;-----------
; ADDIZIONE
;-----------

; Funzione che calcola la somma di due numeri-stringa
(define (string-add str1 str2)
  (local (s1 s2)
    (setq s1 (str1 0))
    (if (= s1 "-") (pop str1))
    (setq s2 (str2 0))
    (if (= s2 "-") (pop str2))
    (cond ((and (= s1 "-") (= s2 "-"))  ; due numeri negativi
            (check-0 (push "-" (add-string str1 str2))))
          ((= s1 "-") ; primo numero negativo
            (if (smaller? str1 str2)
                (check-0 (sub-string str2 str1))
                (check-0 (push "-" (sub-string str1 str2)))))
          ((= s2 "-") ; secondo numero negativo
            (if (smaller? str1 str2)
                (check-0 (push "-" (sub-string str2 str1)))
                (check-0 (sub-string str2 str1))))
          (true (add-string str1 str2))))) ; due numeri positivi

; Funzione ausiliaria per la somma di due numeri-stringa
(define (add-string str1 str2)
  (local (n1 n2 str val carry z)
    (setq z (char "0"))
    ; str2 deve essere la più lunga
    (if (> (length str1) (length str2)) (swap str1 str2))
    (setq str "")
    (setq n1 (length str1))
    (setq n2 (length str2))
    ; inversione delle stringhe
    (reverse str1)
    (reverse str2)
    (setq carry 0)
    ; Ciclo per tutta la stringa più corta
    ; sottrae le cifre di str2 a str1
    (for (i 0 (- n1 1))
      ; calcolo della somma delle cifre correnti e
      ; del riporto (carry)
      (setq val (+ (- (char (str1 i)) z)
                   (- (char (str2 i)) z)
                  carry))
      (extend str (char (+ (% val 10) z)))
      (setq carry (int (div val 10)))
    )
    (if (!= n1 n2) (begin
        ; aggiunge le cifre rimanenti di str2
        (for (i n1 (- n2 1))
          (setq val (+ (- (char (str2 i)) z) carry))
          (extend str (char (+ (% val 10) z)))
          (setq carry (int (div val 10)))
        ))
    )
    ; se esiste, aggiunge il riporto (carry)
    (if (> carry 0) (extend str (char (+ carry z))))
    ; inverte la stringa
    (reverse str)))

;-------------
; SOTTRAZIONE
;-------------

; Funzione che calcola la differenza di due numeri-stringa
(define (string-sub str1 str2)
  (local (s1 s2)
    (setq s1 (str1 0))
    (if (= s1 "-") (pop str1))
    (setq s2 (str2 0))
    (if (= s2 "-") (pop str2))
    (cond ((and (= s1 "-") (= s2 "-")) ; due numeri negativi
            (if (smaller? str1 str2)
                (sub-string str2 str1)
                (check-0 (push "-" (sub-string str1 str2)))))
          ((= s1 "-") ; primo numero negativo
            (if (smaller? str1 str2)
                (check-0 (push "-" (add-string str2 str1)))
                (check-0 (push "-" (add-string str1 str2)))))
          ((= s2 "-") ; secondo numero negativo
            (check-0 (add-string str2 str1)))
          (true ; due numeri positivi
            (if (smaller? str1 str2)
                (check-0 (push "-" (sub-string str1 str2)))
                (check-0 (sub-string str1 str2)))))))

; Funzione ausiliaria per la differenza di due numeri-stringa
(define (sub-string str1 str2)
  (local (n1 n2 str val carry z)
    (setq z (char "0"))
    ; str1 deve essere maggiore o uguale a str2
    (if (smaller? str1 str2) (swap str1 str2))
    (setq str "")
    (setq n1 (length str1))
    (setq n2 (length str2))
    ; inversione delle stringhe
    (reverse str1)
    (reverse str2)
    (setq carry 0)
    ; Ciclo per tutta la stringa più corta
    ; sottrae le cifre di str1 a str2
    (for (i 0 (- n2 1))
      (setq val (int (str1 i)))
      (setq val (- (- (char (str1 i)) z)
                   (- (char (str2 i)) z)
                   carry))
      ; Se la sottrazione è minore di zero
      ; allora aggiungiamo 10 a val e
      ; poniamo il riporto (carry) a 1
      (if (< val 0)
          (set 'val (+ val 10) 'carry 1)
          ; else
          (set 'carry 0)
      )
      (extend str (char (+ val z)))
    )
    ; sottrae le cifre rimanenti del numero maggiore
    (if (!= n1 n2) (begin
        ; sottrae le cifre che rimangono di str1
        (for (i n2 (- n1 1))
          (setq val (- (- (char (str1 i)) z) carry))
          ; se il valore val è negativo, allora lo rende positivo
          (if (< val 0)
            (set 'val (+ val 10) 'carry 1)
            ;else
            (set 'carry 0)
          )
          (extend str (char (+ val z)))
        ))
    )
    (reverse str)
    ; toglie gli (eventuali) zeri iniziali
    (while (= (str 0) "0") (pop str))
    (if (= str "") (setq str "0"))
    str))

;-----------------
; MOLTIPLICAZIONE
;-----------------

; Funzione che calcola la moltiplicazione di due numeri-stringa
(define (string-mul str1 str2)
  (local (s1 s2)
    (setq s1 (str1 0))
    (if (= s1 "-") (pop str1))
    (setq s2 (str2 0))
    (if (= s2 "-") (pop str2))
    (cond ((and (= s1 "-") (= s2 "-")) ; due numeri negativi
            (mul-string str1 str2))
          ((or (= s1 "-") (= s2 "-"))
            (check-0 (push "-" (mul-string str1 str2))) ; uno dei due negativo
          (true (mul-string str1 str2))))) ; due numeri positivi

; Funzione ausiliaria per la moltiplicazione di due numeri-stringa
(define (mul-string x y) ; a and b are strings of decimal digits
    (letn ( nx (length x)
            ny (length y)
            np (+ nx ny)
            X (array nx (reverse (map int (explode x))))
            Y (array ny (reverse (map int (explode y))))
            Q (array (+ nx 1) (dup 0 (+ nx 1)))
            P (array np (dup 0 np))
            carry 0
            digit 0 )
        (dotimes (i ny) ; for each digit of the multiplier
            (dotimes (j nx) ; for each digit of the multiplicant
                (setq digit (+ (* (Y i) (X j)) carry) )
                (setq carry (/ digit 10))
                (setf (Q j) (% digit 10)) )
            (setf (Q nx ) carry)
            ; add Q to P shifted by i
            (setq carry 0)
            (dotimes (j (+ nx 1))
                (setq digit (+ (P (+ j i)) (Q j) carry))
                (setq carry (/ digit 10))
                (setf (P (+ j i)) (% digit 10)) )
        )
    ; translate P to string and return
    (setq P (reverse (array-list P)))
    (if (zero? (P 0)) (pop P))
    (join (map string P))))

;-----------
; DIVISIONE
;-----------

; Funzione che calcola la divisione (intera) di due numeri-stringa
(define (string-div str1 str2)
  (local (s1 s2)
    (setq s1 (str1 0))
    (if (= s1 "-") (pop str1))
    (setq s2 (str2 0))
    (if (= s2 "-") (pop str2))
    (cond ((and (= s1 "-") (= s2 "-")) ; due numeri negativi
            (div-string str1 str2))
          ((or (= s1 "-") (= s2 "-")) ; uno dei due negativo
            (check-0 (push "-" (div-string str1 str2))))
          (true (div-string str1 str2))))) ; due numeri positivi

; Funzione ausiliaria per la divisione di due numeri-stringa
(define (div-string str1 str2)
  (let (conta 0)
    (cond
      ((= str1 str2 0) (setq conta "0/0"))
      ((= str1 0) (setq conta "0"))
      ((= str2 0) (setq conta "1/0"))
      ((smaller? str1 str2) (setq conta "0"))
      ((= str1 str2) (setq conta "1"))
      (true
        (while (smaller? str2 str1)
          (setq str1 (sub-string str1 str2))
          (++ conta)
        )
        (if (= str1 str2) (++ conta)))
    )
    (string conta)))

;------
; TEST
;------

(define (rand-range min-val max-val)
"Generate a random integer in a closed range"
  (if (> min-val max-val) (swap min-val max-val))
  (+ min-val (rand (+ (- max-val min-val) 1))))

; Funzione per il test delle quattro operazioni con i numeri-stringa
; Numeri nell'intervallo [-1e9,1e9]
(define (test num-oper)
  (local (v1 v2 re1 res2)
    (setq v1 (collect (rand-range -1e9 1e9) num-oper))
    (setq v2 (collect (rand-range -1e9 1e9) num-oper))
    (for (i 0 (- num-oper 1))
      ;(if (zero? (% i (/ num-oper 10))) (println i))
      ; test addizione
      (setq res1 (string (+ (v1 i) (v2 i))))
      (setq res2 (string-add (string (v1 i)) (string (v2 i))))
      (if (!= res1 res2) (println "add: " res1 { } res2 { } (v1 i) { } (v2 i)))
      ; test sottrazione
      (setq res1 (string (- (v1 i) (v2 i))))
      (setq res2 (string-sub (string (v1 i)) (string (v2 i))))
      (if (!= res1 res2) (println "sub: "res1 { } res2 { } (v1 i) { } (v2 i)))
      ; test moltiplicazione
      (setq res1 (string (* (v1 i) (v2 i))))
      (setq res2 (string-mul (string (v1 i)) (string (v2 i))))
      (if (!= res1 res2) (println "mul: "res1 { } res2 { } (v1 i) { } (v2 i)))
      ; test divisione
      (setq res1 (string (/ (v1 i) (v2 i))))
      (setq res2 (string-div (string (v1 i)) (string (v2 i))))
      (if (!= res1 res2) (println "div: "res1 { } res2 { } (v1 i) { } (v2 i)))
    )))

(time (test 1e4))
;-> 2661.948
(time (test 1e5))
;-> 145004.108

Nota: le funzioni sono molto lente (specialmente la divisione).


-------------------
Sequenza di Berstel
-------------------

Sequenza OEIS A007420:
Berstel sequence: a(n+1) = 2*a(n) - 4*a(n-1) + 4*a(n-2).
  0, 0, 1, 2, 0, -4, 0, 16, 16, -32, -64, 64, 256, 0, -768, -512, 2048,
  3072, -4096, -12288, 4096, 40960, 16384, -114688, -131072, 262144,
  589824, -393216, -2097152, -262144, 6291456, 5242880, -15728640,
  -27262976, 29360128, 104857600, -16777216, ...

Definizione alternativa:

  a(0)=0, a(1)=0, a(2)=1, a(n) = 2*a(n-1) - 4*a(n-2) + 4*a(n-3)

Usiamo una lista per memorizzare i risultati intermedi (memoization).

(define (seq num)
  (let (out '())
    (setq memoize '((0 0L) (1 0L) (2 1L)))
    (define (seq-aux num)
      (cond ((= num 0) 0L)
            ((= num 1) 0L)
            ((= num 2) 1L)
            (true
              (let (memo (lookup num memoize))
                (if memo memo
                  (begin
                    (setq out (+ (* 2L (seq-aux (- num 1))) (* -4L (seq-aux (- num 2))) (* 4L (seq-aux (- num 3)))))
                    (push (list num out) memoize -1)
                    out))))))
    (seq-aux num)
    (map last memoize)))

(seq 52)
;-> (0L 0L 1L 2L 0L -4L 0L 16L 16L -32L -64L 64L 256L 0L -768L -512L 2048L
;->  3072L -4096L -12288L 4096L 40960L 16384L -114688L -131072L 262144L
;->  589824L -393216L -2097152L -262144L 6291456L 5242880L -15728640L
;->  -27262976L 29360128L 104857600L -16777216L -335544320L -184549376L
;->  905969664L 1207959552L -1946157056L -5100273664L 2415919104L 17448304640L
;->  4831838208L -50465865728L -50465865728L 120259084288L 240518168576L
;->  -201863462912L -884763262976L 0L)


---------------------
Sequenza Schatunowsky
---------------------

Determinare la sequenza dei numeri a(n) in cui tutti i numeri più piccoli di a(n) sono relativamente primi con a(n) e sono anche primi.
Nel 1893 Schatunowsky ha dimostrato che il più grande numero della sequenza è 30.
Non esistono numeri maggiori di 30 che soddisfano la proprietà.

Sequenza OEIS A048597:
Very round numbers: reduced residue system consists of only primes and 1.
  1, 2, 3, 4, 6, 8, 12, 18, 24, 30

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (coprime? a b)
"Check if two integer are coprime"
  (= (gcd a b) 1))

(define (schatunowsky max-num)
  (local (out cont num stop)
    (setq out '(1 2))
    (setq conta 2)
    (setq num 3)
    (while (< num max-num)
      (setq stop nil)
      (for (k 2 (- num 1) 1 stop)
        (if (and (coprime? num k) (not (prime? k))) (setq stop true))
      )
      (when (not stop) (push num out -1) (++ conta))
      (++ num))
    out))

Proviamo:

(schatunowsky 100)
;-> (1 2 3 4 6 8 12 18 24 30)

(time (println (schatunowsky 1e6)))
;-> (1 2 3 4 6 8 12 18 24 30)
;-> 2312.615


-------------
Legge di Zipf
-------------

Nel 1935 il linguista statunitense George Kingsley Zipf notò un curioso schema nella frequenza delle parole.
In un qualsiasi testo la parola più frequente appare due volte più spesso della seconda più frequente, la terza parola più frequente appare un terzo delle volte rispetto alla prima, la quarta un quarto e così via.

La legge di Zipf è una legge empirica che descrive la frequenza di un evento P(i) facente parte di un insieme, in funzione della posizione i (detta rango) nell'ordinamento decrescente rispetto alla frequenza stessa di tale evento:

             c
  f(P(i)) = ---
             i

dove: i indica il rango,
      P(i) indica l'evento che occupa l'i-esimo rango (ovvero l'i-esimo evento più frequente)
      f(P(i)) è il numero di volte (frequenza) che si verifica l'evento P(i)
      c è una costante di normalizzazione, pari al valore f(P(1)).

La legge di Zipf non vale solamente per la lingua inglese, ma anche per l'italiano e tutte le altre lingue, perfino quelle che non sono state ancora decifrate.
Per quanto lo schema sia stato confermato da tempo, nessuno è mai stato in grado di spiegarlo.
Il linguista Sander Lestrade (Paesi Bassi) suppone che la legge di Zipf può essere spiegata dall'interazione in un testo tra la struttura delle frasi (sintassi) e il significato delle parole (semantica).

Tipi di raccolte di dati che seguono la legge di Zipf:
a) frequenza degli accessi alle pagine internet
b) frequenza delle parole in determinati testi
c) note in spartiti musicali
d) dimensione degli abitati, città
e) distribuzione dei redditi
f) distribuzione delle imprese
g) forza dei terremoti

Il parametro lst è una lista con la struttura (frequenza parola) ordinata con frequenza decrescente.

(define (zipf lst)
  (let ( (out '()) (c (lst 0 0)) )
    (dolist (el lst)
      (push (list (div c (+ $idx 1)) (el 1)) out -1))
    out))

Proviamo con la frequenza delle parole del libro di Conan Doyle "The Sign of Four":
(Vedi "Frequenza Parole" su "Note libere 4")

(setq freq '((2341 "the")
             (1237 "i")
             (1187 "and")
             (1122 "of")
             (1095 "a")
             (1093 "to")
             (697 "it")
             (681 "in")
             (645 "he")
             (631 "that")))

(zipf freq)
;-> ((2341 "the") (1170.5 "i") (780.3333333333334 "and")
;->  (585.25 "of") (468.2 "a") (390.1666666666667 "to")
;->  (334.4285714285714 "it") (292.625 "in")
;->  (260.1111111111111 "he") (234.1 "that"))


----------------------------------------------
Numeri k-trasponibili e k-trasponibili-inversi
----------------------------------------------

Dato un intero positivo k, un altro intero positivo x si dice "k-trasponibile" se, quando la sua cifra più a sinistra viene spostata al posto dell'unità, l'intero risultante è k*x.
Per esempio, 142857 è 3-trasponibile poiché 428571 = 3*142857.

Il matematico Kahan ha dimostrato che:
per k = 1 tutte le cifre di un intero k-trasponibile devono essere le stesse
per k > 1 non ci sono tali interi a meno che k = 3 con i seguenti numeri:
  142857, 285714, 142857142857, 285714285714,
  142857142857142857, 285714285714285714, ...

; Sposta la prima cifra a sinistra in fondo al numero
(define (k-tran1 num k)
  (setq s (string num))
  (setq tr (int (push (pop s) s -1) 0 10))
  (= (* k num) tr))

(k-tran1 142857 3)
;-> true

(filter (fn(x) (k-tran1 x 1)) (sequence 1 1e4))
;-> (1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 111 222 333 444 555 666
;->  777 888 999 1111 2222 3333 4444 5555 6666 7777 8888 9999)
(filter (fn(x) (k-tran1 x 2)) (sequence 1 1e6))
;-> ()
(filter (fn(x) (k-tran1 x 3)) (sequence 1 1e6))
;-> (142857 285714)
(filter (fn(x) (k-tran1 x 4)) (sequence 1 1e6))
;-> ()
(filter (fn(x) (k-tran1 x 5)) (sequence 1 1e6))
;-> ()
(filter (fn(x) (k-tran1 x 6)) (sequence 1 1e6))
;-> ()
(filter (fn(x) (k-tran1 x 7)) (sequence 1 1e6))
;-> ()
(filter (fn(x) (k-tran1 x 8)) (sequence 1 1e6))
;-> ()
(filter (fn(x) (k-tran1 x 9)) (sequence 1 1e6))
;-> ()

Dato un intero positivo k, un altro intero positivo x si dice "k-trasponibile-inverso" se, quando la sua cifra più a destra viene spostata davanti al numero, l'intero risultante è k*x.
Per esempio, 102564 è 4-trasponibile poiché 410256 = 4*102564.

; Sposta la prima cifra a destra in cima al numero
(define (k-tran2 num k)
  (setq s (string num))
  (setq tr (int (push (pop s -1) s) 0 10))
  (= (* k num) tr))

(filter (fn(x) (k-tran2 x 1)) (sequence 1 1e4))
;-> (1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 111 222 333 444 555 666
;->  777 888 999 1111 2222 3333 4444 5555 6666 7777 8888 9999)
(filter (fn(x) (k-tran2 x 2)) (sequence 1 1e6))
;-> ()
(filter (fn(x) (k-tran2 x 3)) (sequence 1 1e6))
;-> (142857 285714)
(filter (fn(x) (k-tran2 x 4)) (sequence 1 1e6))
;-> (102564 128205 153846 179487 205128 230769)
(filter (fn(x) (k-tran2 x 5)) (sequence 1 1e6))
;-> (142857)
(filter (fn(x) (k-tran2 x 6)) (sequence 1 1e6))
;-> ()
(filter (fn(x) (k-tran2 x 7)) (sequence 1 1e6))
;-> ()
(filter (fn(x) (k-tran2 x 8)) (sequence 1 1e6))
;-> ()
(filter (fn(x) (k-tran2 x 9)) (sequence 1 1e6))
;-> ()


------------------------------------------------------------------
Numero intero come somma di quattro quadrati - Teorema di Lagrange
------------------------------------------------------------------

Esiste un teorema, attribuito a Lagrange, che afferma che ogni numero intero N può essere scritto come somma di quattro quadrati (considerando anche 0^2 = 0):

  N = a^2 + b^2 + c^2 + d^2

Nota: il teorema è chiamato anche "congettura di Bachet".

Scriviamo una funzione che, dato un numero N, restituisce una lista con tutte le quadruple (a^2, b^2, c^2 e d^2) che sommano a N.

La soluzione è brute-force: generiamo tutte le combinazioni di interi a^2, b^2, c^2 e d^2 e verifichiamo quale combinazione somma a N.

Funzione 1 (lista):

(setq square (map (fn(x) (* x x)) (sequence 0 50)))

(define (sum4 N)
  (let ( (len (length square)) (out '()) )
    (dolist (a square)
      (dolist (b square)
        (dolist (c square)
            (let (d (- N a b c))
              (if (ref d square) ; verifica se d è nella lista dei quadrati
                  (push (list a b c d) out -1))))))
    (unique (map sort out))))

(sum4 123)
;-> ((0 1 1 121) (0 25 49 49) (1 9 49 64)
;->  (1 16 25 81) (9 16 49 49) (9 25 25 64))

Funzione 2 (vettori):

(define (square? num)
"Check if an integer is a perfect square"
  (let (isq (int (sqrt num)))
    (= num (* isq isq))))

(setq quadrati (array 51 (map (fn(x) (* x x)) (sequence 0 50))))

(define (somma4 N)
  (let ( (len (length quadrati)) (out '()) )
    (dotimes (a len)
      (dotimes (b len)
        (dotimes (c len)
          (letn ( (a1 (quadrati a)) (b1 (quadrati b)) (c1 (quadrati c))
                  (d1 (- N a1 b1 c1)) )
            (if (square? d1) ; Verifica se 'd1' è un quadrato
                (push (list a1 b1 c1 d1) out -1))))))
    (unique (map sort out))))

(somma4 123)
;-> ((0 1 1 121) (0 25 49 49) (1 9 49 64)
;->  (1 16 25 81) (9 16 49 49) (9 25 25 64))
(= (sum4 123) (somma4 123))
;-> true

Vediamo la velocità delle funzioni (che dipende fortemente dalla lunghezza della lista dei quadrati):

(setq square (map (fn(x) (* x x)) (sequence 0 50)))
(time (sum4 1350))
;-> 134.668
(time (sum4 7777))
;-> 55.883
(setq quadrati (array 51 (map (fn(x) (* x x)) (sequence 0 50))))
(time (somma4 1350))
;-> 155.583
(time (somma4 7777))
;-> 68.843

(setq square (map (fn(x) (* x x)) (sequence 0 100)))
(time (sum4 1350))
;-> 827.813
(time (sum4 7777))
;-> 1550.854
(setq quadrati (array 101 (map (fn(x) (* x x)) (sequence 0 100))))
(time (somma4 1350))
;-> 440.851
(time (somma4 7777))
;-> 1265.585

(setq square (map (fn(x) (* x x)) (sequence 0 200)))
(time (sum4 1350))
;-> 10308.351
(time (sum4 7777))
;-> 11156.154
(setq quadrati (array 201 (map (fn(x) (* x x)) (sequence 0 200))))
(time (somma4 1350))
;-> 2856.364
(time (somma4 7777))
;-> 3717.054

La funzione 2 è più veloce (ma entrambe sono molto lente).

Calcoliamo alcune scomposizioni di numeri:

(setq quadrati (array 11 (map (fn(x) (* x x)) (sequence 0 10))))
(apply + quadrati)
;-> 385
(map (fn(x) (list $idx (somma4 x))) (sequence 0 20))
;-> ((0 ((0 0 0 0)))
;->  (1 ((0 0 0 1)))
;->  (2 ((0 0 1 1)))
;->  (3 ((0 1 1 1)))
;->  (4 ((0 0 0 4) (1 1 1 1)))
;->  (5 ((0 0 1 4)))
;->  (6 ((0 1 1 4)))
;->  (7 ((1 1 1 4)))
;->  (8 ((0 0 4 4)))
;->  (9 ((0 0 0 9) (0 1 4 4)))
;->  (10 ((0 0 1 9) (1 1 4 4)))
;->  (11 ((0 1 1 9)))
;->  (12 ((0 4 4 4) (1 1 1 9)))
;->  (13 ((0 0 4 9) (1 4 4 4)))
;->  (14 ((0 1 4 9)))
;->  (15 ((1 1 4 9)))
;->  (16 ((0 0 0 16) (4 4 4 4)))
;->  (17 ((0 0 1 16) (0 4 4 9)))
;->  (18 ((0 0 9 9) (0 1 1 16) (1 4 4 9)))
;->  (19 ((0 1 9 9) (1 1 1 16)))
;->  (20 ((0 0 4 16) (1 1 9 9))))

Alcuni numeri hanno più di una scomposizione (es. 4, 12), mentre altri hanno una scomposizione unica (es. 5, 14, 15)

Scriviamo una funzione che genera la sequenza dei numeri che hanno una scomposizione unica.

Sequenza OEIS A006431:
Numbers that have a unique partition into a sum of four nonnegative squares.
  0, 1, 2, 3, 5, 6, 7, 8, 11, 14, 15, 23, 24, 32, 56, 96, 128, 224, 384,
  512, 896, 1536, 2048, 3584, 6144, 8192, 14336, 24576, 32768, 57344,
  98304, 131072, 229376, 393216, 524288, 917504, 1572864, 2097152, 3670016,
  6291456, 8388608, 14680064, ...

(setq quadrati (array 51 (map (fn(x) (* x x)) (sequence 0 50))))
(time (println (filter (fn(x) (= (length (somma4 x)) 1)) (sequence 0 1000))))
;-> (0 1 2 3 5 6 7 8 11 14 15 23 24 32 56 96 128 224 384 512 896)
;-> 50943.26

Nota: From a(16) = 96 onwards, the terms of this sequence satisfy the third-order recurrence relation a(n) = 4*a(n-3).

(define (seq limite)
  (setq out '(0 1 2 3 5 6 7 8 11 14 15 23 24 32 56))
  (setq conta (length out))
  (while (< conta limite)
    (push (* 4 (out (- conta 3))) out -1)
    (++ conta))
  out)

(seq 42)
;-> (0 1 2 3 5 6 7 8 11 14 15 23 24 32 56 96 128 224 384
;->  512 896 1536 2048 3584 6144 8192 14336 24576 32768 57344
;->  98304 131072 229376 393216 524288 917504 1572864 2097152 3670016
;->  6291456 8388608 14680064)

Adesso vediamo la sequenza del numero di scomposizioni in 4 quadrati dei numeri 0..n:

Sequenza OEIS A002635:
Number of partitions of n into 4 squares.
  1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 2, 1, 2, 2, 1, 1, 2, 2, 3, 2, 2, 2, 2, 1,
  1, 3, 3, 3, 3, 2, 2, 2, 1, 3, 4, 2, 4, 3, 3, 2, 2, 3, 4, 3, 2, 4, 2, 2,
  2, 4, 5, 3, 5, 3, 5, 3, 1, 4, 5, 3, 3, 4, 3, 4, 2, 4, 6, 4, 4, 4, 5, 2,
  3, 5, 5, 5, 5, 4, 4, 3, 2, 6, 7, 4, 5, 5, 5, 4, 2, 5, 9, 5, 3, 5, 4, 3,
  1, 6, 7, 6, 7, 5, 7, 5, 3, 6, 7, 4, ...

(setq quadrati (array 51 (map (fn(x) (* x x)) (sequence 0 50))))
(time (println (map (fn(x) (length (somma4 x))) (sequence 0 107))))
;-> (1 1 1 1 2 1 1 1 1 2 2 1 2 2 1 1 2 2 3 2 2 2 2 1
;->  1 3 3 3 3 2 2 2 1 3 4 2 4 3 3 2 2 3 4 3 2 4 2 2
;->  2 4 5 3 5 3 5 3 1 4 5 3 3 4 3 4 2 4 6 4 4 4 5 2
;->  3 5 5 5 5 4 4 3 2 6 7 4 5 5 5 4 2 5 9 5 3 5 4 3
;->  1 6 7 6 7 5 7 5 3 6 7 4)


---------------------------------
Numeri somma di quadrati distinti
---------------------------------

Determinare i numeri che sono rappresentabili come somma di un qualunque numero di quadrati distinti.

  n = a^2 + b^2 + c^2 + d^2
  dove 0 <= a < b < c < d

Sequenza OEIS A003995:
Sum of (any number of) distinct squares: of form r^2 + s^2 + t^2 + ... with 0 <= r < s < t < ...
  0, 1, 4, 5, 9, 10, 13, 14, 16, 17, 20, 21, 25, 26, 29, 30, 34, 35, 36,
  37, 38, 39, 40, 41, 42, 45, 46, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58,
  59, 61, 62, 63, 64, 65, 66, 68, 69, 70, 71, 73, 74, 75, 77, 78, 79, 80,
  81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 93, 94, 95, 97, ...

(define (sum-num-from-list numbers num)
"Find numbers in a list that add to a given number"
  (let ( (dp (array (+ num 1) (dup nil (+ num 1))))
         (used-numbers (array (+ num 1) (dup nil (+ num 1)))) )
    (setf (dp 0) true)
    (dolist (el numbers)
      (for (i num 0 -1)
        (if (and (>= i el) (dp (- i el)))
          (begin
            (setf (dp i) true)
            (if (not (used-numbers i))
              (if (used-numbers (- i el))
                (setf (used-numbers i) (append (used-numbers (- i el)) (list el)))
                (setf (used-numbers i) (list el))))))))
    (used-numbers num)))

Proviamo con la lista dei quadrati:

(setq square (map (fn(x) (* x x)) (sequence 0 50)))
;-> (0 1 4 9 16 25 36 49 64 81 100 121 144 169 196 225 256 289 324 361
;->  400 441 484 529 576 625 676 729 784 841 900 961 1024 1089 1156 1225
;->  1296 1369 1444 1521 1600 1681 1764 1849 1936 2025 2116 2209 2304
;->  2401 2500)

(map (fn(x) (list $idx (sum-num-from-list square x))) (sequence 0 100))
;-> ((0 (0)) (1 (0 1)) (2 nil) (3 nil) (4 (0 4)) (5 (0 1 4)) (6 nil)
;->  (7 nil) (8 nil) (9 (0 9)) (10 (0 1 9)) (11 nil) (12 nil)
;-> ...
;->  (96 nil) (97 (0 16 81)) (98 (0 4 9 36 49)) (99 (0 9 16 25 49))
;->  (100 (0 1 9 16 25 49)))

Troviamo i numeri che sono rappresentabili come somma di un qualunque numero di quadrati distinti.

(filter (fn(x) (not (null? (sum-num-from-list square x)))) (sequence 0 100))
;-> (0 1 4 5 9 10 13 14 16 17 20 21 25 26 29 30 34 35 36
;->  37 38 39 40 41 42 45 46 49 50 51 52 53 54 55 56 57 58
;->  59 61 62 63 64 65 66 68 69 70 71 73 74 75 77 78 79 80
;->  81 82 83 84 85 86 87 88 89 90 91 93 94 95 97 98 99 100)

In modo analogo possiamo trovare i numeri che non sono esprimibili come somma di un numero qualunque di quadrati distinti.

Sequenza OEIS A001422:
Numbers which are not the sum of distinct squares.
  2, 3, 6, 7, 8, 11, 12, 15, 18, 19, 22, 23, 24, 27, 28, 31, 32, 33, 43,
  44, 47, 48, 60, 67, 72, 76, 92, 96, 108, 112, 128, ...

(filter (fn(x) (null? (sum-num-from-list square x))) (sequence 0 128))
;-> (2 3 6 7 8 11 12 15 18 19 22 23 24 27 28 31 32 33 43
;->  44 47 48 60 67 72 76 92 96 108 112 128)


-----------------------------------------
Celle della matrice in ordine di distanza
-----------------------------------------

Data una matrice righe x colonne e le coordinate di una cella della matrice r0,c0, restituire le coordinate di tutte le celle nella matrice, ordinate in modo crescente in base alla loro distanza da (r0, c0)
La distanza tra due celle (r1, c1) e (r2, c2) vale |r1 - r2| + |c1 - c2|.

(define (dist-cell righe colonne r0 c0)
  (setq dist '())
  (for (r 0 (- righe 1))
    (for (c 0 (- colonne 1))
      (setq d (+ (abs (- r0 r)) (abs (- c0 c))))
      (println d)
      (push (list d (list r c)) dist)
    )
  )
  (sort dist))

Proviamo:

(dist-cell 2 2 0 1)
;-> ((0 (0 1)) (1 (0 0)) (1 (1 1)) (2 (1 0)))

(dist-cell 3 3 1 1)
;-> ((0 (1 1)) (1 (0 1)) (1 (1 0)) (1 (1 2)) (1 (2 1)) (2 (0 0))
;->  (2 (0 2)) (2 (2 0)) (2 (2 2)))


------------------------------
Numero pari e dispari di cifre
------------------------------

Data una lista di numeri interi restituire quanti numeri hanno un numero pari di cifre e quanti numeri hanno un numero dispari di cifre.

(define (cifre lst)
  (let ( (len (length lst)) (pari 0) )
    (setq pari (length (filter (fn(x) (even? (length x))) lst)))
    (list pari (- len pari))))

Proviamo:

(cifre '(-1 1 22 -22 333 -333))
;-> 2 4

(cifre '(234 67213 982347 66 5 0 2346 -132 -34834 87234 -2 -237 ))
;-> (3 9)


-----------------------------------------------
Compito di Wason (Problema delle quattro carte)
-----------------------------------------------

Il compito di selezione di Wason (o problema delle quattro carte) è un problema logico ideato da Peter Cathcart Wason nel 1966.
Si tratta di uno dei più famosi rompicapo nello studio del ragionamento deduttivo.
Un esempio del compito è il seguente:

Le seguenti quattro carte con una lettera su un lato e un numero sull'altro vengono poste sul tavolo di fronte a te:

  +---+   +---+   +---+   +---+
  |   |   |   |   |   |   |   |
  | A |   | K |   | 2 |   | 7 |
  |   |   |   |   |   |   |   |
  +---+   +---+   +---+   +---+

Domanda:
Quale carta o carte devi girare per verificare l'affermazione, "Se c'è una vocale su un lato della carta, allora c'è un numero pari sull'altro lato" ?

Una risposta che identifica una carta che non deve essere girata, o che non riesce a identificare una carta che deve essere girata, non è corretta.

Vediamo cosa significa girare ogni carta:

Carta "A"
La prima carta è una vocale, quindi girandola verifichiamo se l'affermazione è valida o meno (se troviamo un numeri pari allora è vera, altrimenti è falsa).

Carta "K"
La seconda carta è una consonante, quindi girandola non otteniamo alcuna informazione per verificare l'affermazione.

Carta "2"
La terza carta è un numero pari. Girando la carta possiamo ottenere quattro risultati:
1) numero pari, nessuna informazione sull'affermazione
2) numero dispari, nessuna informazione sull'affermazione
3) consonante, nessuna informazione sull'affermazione
4) vocale, , nessuna informazione sull'affermazione
Questo perchè l'affermazione data NON implica che se un lato è pari, allora dall'altro c'è una vocale.

Carta "7"
La quarta carta è un numero dispari. Girando la carta possiamo ottenere quattro risultati:
1) numero pari, nessuna informazione sull'affermazione
2) numero dispari, nessuna informazione sull'affermazione
3) consonante, nessuna informazione sull'affermazione
4) vocale, l'affermazione data non è valida (perchè da un lato abbiamo una vocale e dall'altro non abbiamo un numero pari)

Quindi la risposta corretta è quella di girare le carte A e 7.

Nota: nello studio di Wason, meno del 10% dei soggetti ha trovato la soluzione corretta.

Rappresentiamo l'affermazione con una funzione:

(define (rule vocale pari)
  (cond ((and vocale pari) true)
        ((and vocale (not pari)) nil)
        (true 'undefined)))

(rule true true)
;-> true
(rule true nil)
;-> nil
(rule nil true)
;-> undefined
(rule nil nil)
;-> undefined


------------------------------------
Numeri interi univoci con somma zero
------------------------------------

Dato un numero intero N, restituire una lista con N numeri interi univoci tali che la loro somma sia pari a 0.

somma = sequenza(1 N-1) - somma(sequenza(1 N-1))

(define (sommazero1 N)
  (cond ((= N 0) '())
        ((= N 1) '(0))
        (true
          (let (s (sequence 1 (- N 1)))
            (push (- (apply + s)) s)))))

(sommazero1 0)
;-> ()
(sommazero1 1)
;-> (0)
(sommazero1 2)
;-> (-1 1)
(sommazero1 5)
;-> (-10 1 2 3 4)

somma = sequenza(1 (N/2)) + sequenza(-1 -(N/2))

(define (sommazero2 N)
  (cond ((= N 0) '())
        ((= N 1) '(0))
        (true
          (if (odd? N)
            (append '(0) (sequence 1 (/ N 2)) (sequence -1 (/ N -2)))
            (append (sequence 1 (/ N 2)) (sequence -1 (/ N -2)))))))

(sommazero2 0)
;-> ()
(sommazero2 1)
;-> (0)
(sommazero2 2)
;-> (-1 1)
(sommazero2 5)
;-> (0 1 2 -1 -2)

somma = ciclo(i=1..(N - 1)) [i * 2 - N + 1];

(define (sommazero3 N)
  (cond ((= N 0) '())
        ((= N 1) '(0))
        (true
          (let (out '())
            (for (i 0 (- N 1))
              (push (add (* i 2) (- N) 1) out))
            out))))

(sommazero3 0)
;-> ()
(sommazero3 1)
;-> (0)
(sommazero3 2)
;-> (1 -1)
(sommazero3 5)
;-> (4 2 0 -2 -4)

Se i numeri della lista possono essere non univoci:

somma = rand(N - 1) - somma(rand(N - 1))

(define (sommazero4 N)
  (cond ((= N 0) '())
        ((= N 1) '(0))
        (true
          (letn ( (r (rand (* N 10) (- N 1))) (sum (- (apply + r))) )
            (push sum r)))))

(sommazero4 0)
;-> ()
(sommazero4 1)
;-> (0)
(sommazero4 2)
;-> (-3 3)
(sommazero4 5)
;-> (-113 29 23 17 44)

somma = ciclo(i=1..(N - 1))[+rand -rand] + differenza

(define (sommazero5 N)
  (cond ((= N 0) '())
        ((= N 1) '(0))
        (true
          (let ( (a 0) (b 0) (out '()) (val 0) )
            (for (i 1 (- N 1))
              (cond ((odd? i)
                      (setq val (rand (* N 10)))
                      (push val out)
                      (++ a val))
                    (true
                      (setq val (rand (* N -10)))
                      (push val out)
                      (++ b val))))
            (push (- (+ a b)) out)))))

(sommazero5 0)
;-> ()
(sommazero5 1)
;-> (0)
(sommazero5 2)
;-> (-12 12)
(sommazero5 5)
;-> (-6 -11 7 -18 28)


---------------------
Matrici di Hessenberg
---------------------

Una matrice di Hessenberg è una matrice quadrata in cui tutti gli elementi al di sotto della prima diagonale sotto la diagonale principale sono nulli.

Vediamo un esempio di implementazione che utilizza trasformazioni elementari per trasformare una matrice quadrata in una matrice di Hessenberg:

(define (hessenberg1 matrice)
  (let ((len (length matrice)))
    (for (k 0 (- len 2))
      (for (i (+ k 1) (- len 1))
        (let ((fattore (div (matrice i k) (matrice k k))))
          (for (j k (- len 1))
            (setf (matrice i j)
                  (sub (matrice i j) (mul fattore (matrice k j)))))))))
    matrice)

(setq A '((4 1 -2 2)
          (1 2 0 1)
          (-2 0 3 -2)
          (2 1 -2 -1)))

(hessenberg1 A)
;-> ((4 1 -2 2)
;->  (0 1.75 0.5 0.5)
;->  (0 0 1.857142857142857 -1.142857142857143)
;->  (0 0 0 -2.846153846153846))

(setq B '((1 2 3 4 5)
          (6 7 8 9 6)
          (1 3 2 5 4)
          (3 5 8 7 4)
          (5 2 3 7 9)))

(hessenberg1 B)
;-> ((1 2 3 4 5)
;-> (0 -5 -10 -15 -24)
;-> (0 0 -3 -2 -5.8)
;-> (0 0 0 -2.666666666666667 -8.133333333333333)
;-> (0 0 0 0 -10.75))

Possiamo utilizzare la tecnica del pivot per garantire che l'elemento diagonale (o un altro scelto come pivot) sia il più adatto per l'eliminazione, migliorando la stabilità numerica.

Algoritmo:
1) Trova il pivot massimo
Per ogni colonna k, cerca la riga con il valore massimo (in valore assoluto) nella posizione k.
2) Scambia le righe
Se la riga del pivot massimo non è k, scambia la riga k con la riga del pivot massimo.
3) Eliminazione
Riduce a zero gli elementi sotto la diagonale nella colonna k, utilizzando il pivot come divisore.

Implementazione con il pivot:

(define (hessenberg2 matrice)
  (let ((n (length matrice)))
    (for (k 0 (- n 2))
      ; Trova il pivot massimo in valore assoluto
      (let ((max-row k))
        (for (i (+ k 1) (- n 1))
          (if (> (abs (matrice i k)) (abs (matrice max-row k)))
              (setq max-row i)))
        ; Scambia le righe se necessario
        (if (!= max-row k)
            (let ((temp-row (matrice k)))
              (setf (matrice k) (matrice max-row))
              (setf (matrice max-row) temp-row))))
      ; Elimina gli elementi sotto la diagonale
      (for (i (+ k 1) (- n 1))
        (let ((fac (div (matrice i k) (matrice k k))))
          (for (j k (- n 1))
            (setf (matrice i j)
                  (sub (matrice i j) (mul fac (matrice k j)))))))))
    matrice)

(setq A '((4 1 -2 2)
          (1 2 0 1)
          (-2 0 3 -2)
          (2 1 -2 -1)))

(hessenberg2 A)
;-> ((4 1 -2 2)
;->  (0 1.75 0.5 0.5)
;->  (0 0 1.857142857142857 -1.142857142857143)
;->  (0 0 0 -2.846153846153846))

(setq B '((1 2 3 4 5)
          (6 7 8 9 6)
          (1 3 2 5 4)
          (3 5 8 7 4)
          (5 2 3 7 9)))

(hessenberg2 B)
;-> ((6 7 8 9 6)
;->  (0 -3.833333333333334 -3.666666666666667 -0.5 4)
;->  (0 0 2.565217391304348 2.304347826086957 2.565217391304348)
;->  (0 0 0 4.23728813559322 6)
;->  (0 0 0 0 1.72))

Nota: la precisione del calcolo dipende dalla rappresentazione dei numeri (es. frazioni o floating-point).


----------------------------------
Frequenza caratteri di una stringa
----------------------------------

Scrivere una funzione che prende una stringa e restituisce una lista dei caratteri della stringa con la relativa frequenza.

(define (count-chars str)
  (letn ( (lst (explode str))
          (unici (unique lst)) )
    (map list unici (count unici lst))))

Proviamo:

(count-chars "supercalifragilistichespiradiloso")
;-> (("s" 4) ("u" 1) ("p" 2) ("e" 2) ("r" 3) ("c" 2) ("a" 3) ("l" 3)
;->  ("i" 6) ("f" 1) ("g" 1) ("t" 1) ("h" 1) ("d" 1) ("o" 2))

(sort (count-chars "supercalifragilistichespiradiloso")
      (fn (x y) (>= (last x) (last y))))
;-> (("i" 6) ("s" 4) ("r" 3) ("a" 3) ("l" 3) ("p" 2) ("e" 2) ("c" 2)
;->  ("o" 2) ("u" 1) ("f" 1) ("g" 1) ("t" 1) ("h" 1) ("d" 1))

(count-chars (string count-chars))
;-> (("(" 10) ("l" 7) ("a" 3) ("m" 2) ("b" 1) ("d" 2) (" " 14) ("s" 6)
;->  ("t" 8) ("r" 2) (")" 10) ("e" 4) ("n" 6) ("x" 1) ("p" 2) ("o" 2)
;->  ("u" 6) ("i" 8) ("c" 4) ("q" 1))


-------------------------------------------
Stampa di stringhe con indice dei caratteri
-------------------------------------------

Data una stringa, stamparla in modo tale che ogni lettera sia in una nuova riga e ripetuta tante volte quanto vale la sua posizione nell'alfabeto inglese ("A" posizione 1, "Z" posizione 26).
Per esempio la stringa "BACIO" dovrebbe essere stampata nel modo seguente:

  BB
  A
  CCC
  IIIIIIIII
  OOOOOOOOOOOOOOO

(define (pridx str)
  (map (fn(x) (println (dup x (- (char x) 64)))) (explode (upper-case str))) '>)

Proviamo:

(pridx "BACIO")
;-> BB
;-> A
;-> CCC
;-> IIIIIIIII
;-> OOOOOOOOOOOOOOO

(pridx "newLISP")
;-> NNNNNNNNNNNNNN
;-> EEEEE
;-> WWWWWWWWWWWWWWWWWWWWWWW
;-> LLLLLLLLLLLL
;-> IIIIIIIII
;-> SSSSSSSSSSSSSSSSSSS
;-> PPPPPPPPPPPPPPPP


----------
Alfabeto-L
----------

Scrivere una funzione che stampa l'alfabeto-L:

  ABCDEFGHIJKLMNOPQRSTUVWXYZ
  BBCDEFGHIJKLMNOPQRSTUVWXYZ
  CCCDEFGHIJKLMNOPQRSTUVWXYZ
  DDDDEFGHIJKLMNOPQRSTUVWXYZ
  EEEEEFGHIJKLMNOPQRSTUVWXYZ
  FFFFFFGHIJKLMNOPQRSTUVWXYZ
  GGGGGGGHIJKLMNOPQRSTUVWXYZ
  HHHHHHHHIJKLMNOPQRSTUVWXYZ
  IIIIIIIIIJKLMNOPQRSTUVWXYZ
  JJJJJJJJJJKLMNOPQRSTUVWXYZ
  KKKKKKKKKKKLMNOPQRSTUVWXYZ
  LLLLLLLLLLLLMNOPQRSTUVWXYZ
  MMMMMMMMMMMMMNOPQRSTUVWXYZ
  NNNNNNNNNNNNNNOPQRSTUVWXYZ
  OOOOOOOOOOOOOOOPQRSTUVWXYZ
  PPPPPPPPPPPPPPPPQRSTUVWXYZ
  QQQQQQQQQQQQQQQQQRSTUVWXYZ
  RRRRRRRRRRRRRRRRRRSTUVWXYZ
  SSSSSSSSSSSSSSSSSSSTUVWXYZ
  TTTTTTTTTTTTTTTTTTTTUVWXYZ
  UUUUUUUUUUUUUUUUUUUUUVWXYZ
  VVVVVVVVVVVVVVVVVVVVVVWXYZ
  WWWWWWWWWWWWWWWWWWWWWWWXYZ
  XXXXXXXXXXXXXXXXXXXXXXXXYZ
  YYYYYYYYYYYYYYYYYYYYYYYYYZ
  ZZZZZZZZZZZZZZZZZZZZZZZZZZ

(define (print-L)
  (let (base "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    ;(println base)
    (for (i 0 25)
      (println (dup (char (+ 65 i)) i) (slice base i))) '>))

(print-L)
;-> ABCDEFGHIJKLMNOPQRSTUVWXYZ
;-> BBCDEFGHIJKLMNOPQRSTUVWXYZ
;-> CCCDEFGHIJKLMNOPQRSTUVWXYZ
;-> DDDDEFGHIJKLMNOPQRSTUVWXYZ
;-> EEEEEFGHIJKLMNOPQRSTUVWXYZ
;-> FFFFFFGHIJKLMNOPQRSTUVWXYZ
;-> GGGGGGGHIJKLMNOPQRSTUVWXYZ
;-> HHHHHHHHIJKLMNOPQRSTUVWXYZ
;-> IIIIIIIIIJKLMNOPQRSTUVWXYZ
;-> JJJJJJJJJJKLMNOPQRSTUVWXYZ
;-> KKKKKKKKKKKLMNOPQRSTUVWXYZ
;-> LLLLLLLLLLLLMNOPQRSTUVWXYZ
;-> MMMMMMMMMMMMMNOPQRSTUVWXYZ
;-> NNNNNNNNNNNNNNOPQRSTUVWXYZ
;-> OOOOOOOOOOOOOOOPQRSTUVWXYZ
;-> PPPPPPPPPPPPPPPPQRSTUVWXYZ
;-> QQQQQQQQQQQQQQQQQRSTUVWXYZ
;-> RRRRRRRRRRRRRRRRRRSTUVWXYZ
;-> SSSSSSSSSSSSSSSSSSSTUVWXYZ
;-> TTTTTTTTTTTTTTTTTTTTUVWXYZ
;-> UUUUUUUUUUUUUUUUUUUUUVWXYZ
;-> VVVVVVVVVVVVVVVVVVVVVVWXYZ
;-> WWWWWWWWWWWWWWWWWWWWWWWXYZ
;-> XXXXXXXXXXXXXXXXXXXXXXXXYZ
;-> YYYYYYYYYYYYYYYYYYYYYYYYYZ
;-> ZZZZZZZZZZZZZZZZZZZZZZZZZZ


---------------
Triangoli retti
---------------

Date le coordinate di tre punti in un piano cartesiano 2D, determinare se il triangolo formato dai punti è retto.

Un triangolo è retto quando ha un angolo retto (90 gradi).
In un triangolo retto vale il teorema di Pitagora: a^2 + b^2 = c^2, dove a e b sono i cateti e c è l'ipotenusa del triangolo.

(define (dist2d p1 p2)
"Calculates 2D Cartesian distance of two points P1 = (x1 y1) and P2 = (x2 y2)"
  (let ( (x1 (p1 0)) (y1 (p1 1))
         (x2 (p2 0)) (y2 (p2 1)) )
    (sqrt (add (mul (sub x1 x2) (sub x1 x2))
               (mul (sub y1 y2) (sub y1 y2))))))

(define (retto-lati a b c)
  (or (= (add (mul a a) (mul b b)) (mul c c))
      (= (add (mul a a) (mul c c)) (mul b b))
      (= (add (mul b b) (mul c c)) (mul a a))))

(retto-lati 5 3 4)
;-> true
(retto-lati 3 5 4)
;-> true
(retto-lati 12 37 35)
;-> true
(retto-lati 21 42 50)
;-> nil
(retto-lati 121 130 238)
;-> nil

Esempio con risultato errato:

(retto-lati 1 1 (sqrt 2))
;-> nil ;??

(define (retto-punti p1 p2 p3)
  (let ( (a (dist2d p1 p2))
         (b (dist2d p1 p3))
         (c (dist2d p2 p3)) )
    ;(println a { } b { } c)
    (retto-lati a b c)))

(retto-punti '(0 0) '(0 5) '(3 0))
;-> true
(retto-punti '(0 0) '(0 10) '(0 20))
;-> nil
(retto-punti '(1 1) '(1 5) '(4 1))
;-> true
(retto-punti '(1 1) '(1 5) '(6 1))
;-> true

Esempi con risultato errato:

(retto-punti '(1 0) '(0 0) '(0 1))
;-> nil ;??
(retto-punti '(1 1) '(-1 3) '(4 4))
;-> nil ;??

Riscriviamo la funzione "retto-lati" introducendo un valore di soglia per il confronto (eps):

(define (retto-lati a b c eps)
  (let (eps (or eps 1e-6))
    (or (> eps (abs (sub (add (mul a a) (mul b b)) (mul c c))))
        (> eps (abs (sub (add (mul a a) (mul c c)) (mul b b))))
        (> eps (abs (sub (add (mul b b) (mul c c)) (mul a a)))))))

Proviamo:

(retto-lati 5 3 4)
;-> true
(retto-lati 3 5 4)
;-> true
(retto-lati 12 37 35)
;-> true
(retto-lati 21 42 50)
;-> nil
(retto-lati 121 130 238)
;-> nil
(retto-lati 1 1 (sqrt 2))
;-> true

(retto-punti '(0 0) '(0 5) '(3 0))
;-> true
(retto-punti '(0 0) '(0 10) '(0 20))
;-> nil
(retto-punti '(1 1) '(1 5) '(4 1))
;-> true
(retto-punti '(1 1) '(1 5) '(6 1))
;-> true

(retto-punti '(1 0) '(0 0) '(0 1))
;-> true
(retto-punti '(1 1) '(-1 3) '(4 4))
;-> true


---------------------------
Divisione massima di numeri
---------------------------

Abbiamo una lista di N numeri interi (N <= 10).
Gli interi adiacenti nella lista vengono divisi (float) in sequenza per ottenere un valore.
Ad esempio, per lista (20 5 2) valutiamo l'espressione "20/5/2" e otteniamo il valore 2 (20/5=4 --> 4/2=2).
(div 20 5 2)
;-> 2
Per modificare il valore finale possiamo effettuare due operazioni:
1) aggiungere un numero qualsiasi di parentesi in qualsiasi posizione per modificare la priorità delle operazioni.
2) cambiare l'ordine dei numeri nella lista.

Scrivere una funzione che restituisce l'espressione che produce il valore massimo.
Per esempio la lista (20 5 2) ha valore massimo 50 con l'espressione (div 20 (div 2 5)) = 50 oppure con
(div 5 (div 2 20)) = 50.

Per massimizzare il risultato di una divisione occorre dividere per il numero più piccolo possibile.
In altre parole dobbiamo minimizzare il denominatore il più possibile per massimizzare il risultato dell'intera espressione.
a) Quando la lista ha un solo numero restituiamo il singolo numero.
b) Quando la lista ha esattamente due numeri (a e b), allora restituiamo l'espressione a/b, (div a b).
c) Quando la lista ha tre o più numeri, il risultato massimo si ottiene quando dividiamo il primo numero per il risultato della divisione di tutti i numeri rimanenti.
Pertanto, con la lista (a b c ... n), la divisione ottimale è a/(b/c/.../n), (div a (div b c ... n).
Il primo numero "a" viene diviso per il valore più piccolo possibile ottenuto dalla divisione del resto dei numeri.

Scriviamo una funzione che data una lista di interi calcola l'espressione che produce la divisione con valore massimo (senza modificare l'ordine dei numeri della lista):

(define (divide-max nums)
  ;(sort nums)
  (let ( (len (length nums)) (out (string (nums 0))) )
    (cond ((= len 1) out)
          ((= len 2) (string "(div " out " " (nums 1) ")"))
          (true
            (setq out (string "(div " out " (div "
                  (join (map string (slice nums 1)) " ") "))"))))))

Proviamo:

(divide-max '(20 5 2))
;-> "(div 20 (div 5 2))"
(eval-string (divide-max '(20 5 2)))
;-> 8

Adesso dobbiamo considerare la possibilità di modificare l'ordine dei numeri della lista.
Per fare questo generiamo tutte le permutazioni dei numeri, per ogni permutazione calcoliamo il valore della divisione massima e teniamo traccia del massimo di questi valori (con la relativa espressione).

(define (perm lst)
"Generates all permutations without repeating from a list of items"
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
              (swap (lst (indici i)) (lst i))
            )
            ;(println lst);
            (push lst out -1)
            (++ (indici i))
            (setq i 0)
          )
          (begin
            (setf (indici i) 0)
            (++ i))))
    out))

Funzione che genera l'espressione con il valore massimo della divisione tra i numeri di una lista:

(define (solve lst prt)
  (local (expr-max val-max permute expr val)
    (setq expr-max "")
    (setq val-max 0)
    (setq permute (perm lst))
    (dolist (p permute)
      (setq expr (divide-max p))
      (setq val (eval-string expr))
      (when (> val val-max)
        (setq val-max val)
        (setq expr-max expr))
      (if prt (println expr { = } val))
    )
    (println "Solution: " expr-max { = } val-max) '>))

Proviamo:

(solve '(20 5 2) true)
;-> (div 20 (div 5 2)) = 8
;-> (div 5 (div 20 2)) = 0.5
;-> (div 2 (div 20 5)) = 0.5
;-> (div 20 (div 2 5)) = 50
;-> (div 5 (div 2 20)) = 50
;-> (div 2 (div 5 20)) = 8
;-> Solution: (div 20 (div 2 5)) = 50

(solve '(1000 100 2 10) true)
;-> (div 1000 (div 100 2 10)) = 200
;-> (div 100 (div 1000 2 10)) = 2
;-> (div 2 (div 1000 100 10)) = 2
;-> (div 1000 (div 2 100 10)) = 500000
;-> (div 100 (div 2 1000 10)) = 500000
;-> (div 2 (div 100 1000 10)) = 200
;-> (div 10 (div 100 1000 2)) = 200
;-> (div 100 (div 10 1000 2)) = 20000
;-> (div 1000 (div 10 100 2)) = 20000
;-> (div 10 (div 1000 100 2)) = 2
;-> (div 100 (div 1000 10 2)) = 2
;-> (div 1000 (div 100 10 2)) = 200
;-> (div 1000 (div 2 10 100)) = 500000
;-> (div 2 (div 1000 10 100)) = 2
;-> (div 10 (div 1000 2 100)) = 2
;-> (div 1000 (div 10 2 100)) = 20000
;-> (div 2 (div 10 1000 100)) = 20000
;-> (div 10 (div 2 1000 100)) = 499999.9999999999
;-> (div 10 (div 2 100 1000)) = 499999.9999999999
;-> (div 2 (div 10 100 1000)) = 20000
;-> (div 100 (div 10 2 1000)) = 20000
;-> (div 10 (div 100 2 1000)) = 200
;-> (div 2 (div 100 10 1000)) = 200
;-> (div 100 (div 2 10 1000)) = 500000
;-> Solution: (div 1000 (div 2 100 10)) = 500000

(solve '(1 2 3 4 5 6 7 8 9))
;-> Solution: (div 3 (div 1 2 4 5 6 7 8 9)) = 362880.0000000001


------------------------------------------------------------------
Passeggiata casuale su una circonferenza (Random walk on a circle)
------------------------------------------------------------------

Un punto si muove lungo una circonferenza in modo casuale.
Ad ogni passo il punto si sposta di un grado a sinistra o a destra in modo casuale.
Il punto parte dalla posizione 0.

In media, quanti passi occorrono affinchè il punto effettui un giro completo (orario oppure antiorario) ?

Possiamo simulare il processo facendo muovere il punto lungo una linea di interi da -360 a 360.

(define (simula iter)
  (local (numero-giri numero-passi step moves continua)
    (setq numero-giri 0)
    (setq numero-passi 0)
    (setq step 1)
    (setq moves (list (- step) step))
    (for (i 1 iter)
      (setq pos 0)
      (setq passi 0)
      (setq continua true)
      (while continua
        (++ pos (moves (rand 2)))
        (++ passi)
        ;(print pos) (read-line)
        (when (and (!= pos 0) (= (% pos 360) 0))
            ;(println "pos: " pos)
            (++ numero-giri)
            (++ numero-passi passi)
            (setq continua nil))
      )
    )
    (println "numero giri: " numero-giri)
    (println "numero passi: " numero-passi)
    (div numero-passi numero-giri))

Proviamo:

(time (println (simula 1000)))
;-> numero giri: 1000
;-> numero passi: 128194206
;-> 128194.206
;-> 23549.289

(time (println (simula 10000)))
;-> numero giri: 10000
;-> numero passi: 1289216520
;-> 128921.652
;-> 236756.372

(time (println (simula 100000)))
;-> numero giri: 100000
;-> numero passi: 12948339304
;-> 129483.39304
;-> 2372966.967

Nota: dal punto di vista matematico il numero medio di passi, noto anche come tempo di assorbimento T, vale:

  T(N) = N^2

cioè il tempo medio per raggiungere +N o -N partendo da 0 è il quadrato di N.

(* 360 360)
;-> 129600


----------------------------------------------------------
Capacità d'acqua di un numero (water-capacity of a number)
----------------------------------------------------------

Define the water-capacity of a number as follows:
If n has the prime factorization p1^e1*p2^e2*...*pk^ek let ci be a column of height pi^ei and width 1.
Juxtaposing the ci leads to a bar graph which figuratively can be filled by water from the top.
The water-capacity of a number is the maximum number of cells which can be filled with water.

For example 48300 has the prime factorization 2^2*3*5^2*7*23.
The bar graph below has to be rotated counterclockwise for 90 degree:

  2^2   ****
  3     ***W
  5^2   *************************
  7     *******WWWWWWWWWWWWWWWW
  23    ***********************
  48300 is the smallest number which has a water-capacity of 17.

Sequenza OEIS A275339:
a(n) is the smallest number which has a water-capacity of n.
  60, 120, 440, 168, 264, 840, 2448, 528, 1904, 624, 1360, 2295, 816, 1632,
  20128, 1824, 48300, 3105, 15392, 2208, 13024, 2400, 10656, 4080, 8288,
  2784, 5920, 2976, 3552, 9120, ...

Funzione che calcola la quantità massima d'acqua che può contenere un instogramma (lista):
(Vedi "Quantità d'acqua in un bacino (Facebook)" in "Domande programmatori".)

(define (bacino lst)
  (if (< (length lst) 2) 0
  ;else
  (local (lun sx dx acqua)
      (setq lun (length lst))
      (setq sx (array lun))
      (setq dx (array lun))
      (setq acqua 0)
      ; riempimento sx
      (setf (sx 0) (lst 0))
      (for (i 1 (- lun 1))
        (setf (sx i) (max (sx (- i 1)) (lst i)))
      )
      ; riempimento dx
      (setf (dx (- lun 1)) (lst (- lun 1)))
      (for (i (- lun 2) 0 -1)
        (setf (dx i) (max (dx (+ i 1)) (lst i)))
      )
      ; bar vale min(sx[i], dx[i]) - arr[i]
      (for (i 0 (- lun 1))
        (setq bar-acqua (- (min (sx i) (dx i)) (lst i)))
        ;(print bar-acqua { })
        (setq acqua (+ acqua bar-acqua))
      )
      acqua)))

(define (factor-group num)
"Factorize an integer number"
  (if (= num 1) '((1 1))
    (letn ( (fattori (factor num))
            (unici (unique fattori)) )
      (transpose (list unici (count unici fattori))))))

Funzione che calcola la sequenza:

(define (sequenza limite)
  (local (out acqua idx)
    (setq out '())
    (setq acqua '(0))
    ; Calcola le capacità d'acqua di tutti i numeri da 1 fino a limite
    (for (i 1 limite)
      (push (bacino (map (fn(x) (* (pow (x 0) (x 1))))
                        (factor-group i))) acqua -1))
    ; Trova le capacità minime per la sequenza 1, 2, ..., N
    ; (fino a che N si trova su acqua)
    (setq idx 1)
    (while (setq value (ref idx acqua))
      (push value out -1)
      (++ idx))
    (flat out)))

Proviamo:

(sequenza 1e5)
;-> (60 120 440 168 264 840 2448 528 1904 624 1360 2295 816 1632
;->  20128 1824 48300 3105 15392 2208 13024 2400 10656 4080 8288
;->  2784 5920 2976 3552 9120)

(time (println (sequenza 1e6)))
;-> (60 120 440 168 264 840 2448 528 1904 624 1360 2295 816 1632 20128 1824
;->  48300 3105 15392 2208 13024 2400 10656 4080 8288 2784 5920 2976 3552 9120
;->  243600 11840 28560 7104 124352 13120 115776 7872 107200 8256 98624 15040
;->  685608 9024 81472 9408 72896 16960 973500 10176 55744 20832 47168 14880
;->  38592 11328 30016 11712 21440 37989 12864 27135 571872 25728 408480 28755
;->  472416 27264 337440 28032 301920 44793 323232 31995 230880 30336 195360
;->  41280 821632 31872 124320 77875 934500 56960 721024 34176 687488 111744)
;-> 5156.227

(time (println (length (sequenza 1e7))))
;-> 178
;-> 63874.246


---------------------------------------
Intersezione di segmenti lungo l'asse X
---------------------------------------

Data una lista di N segmenti lungo l'asse X (xi1 xf1) (xi2 xf2)...(xiN xfN), determinare, se esistono, quali sono i valori di X interi che sono comuni a tutti i segmenti.

Trovare gli interi X che sono comuni a tutti i segmenti significa che un intero X deve appartenere all'intervallo di ciascun segmento.
In termini matematici, X deve soddisfare:  xi <= X <= xf

Algoritmo
1. Calcoliamo il massimo dei valori xi (limite inferiore comune).
2. Calcoliamo il minimo dei valori xf (limite superiore comune).
3. Se il massimo dei valori xi è minore o uguale al minimo dei valori xf, gli interi compresi in questo intervallo sono i numeri comuni a tutti i segmenti, altrimenti non ci sono interi in comune tra i segmenti.

(define (segmenti-comuni segmenti)
  (let ((xi-max (apply max (map first segmenti)))
        (xf-min (apply min (map last segmenti))))
    (if (<= xi-max xf-min)
        (sequence xi-max xf-min) ; Valori in comune
        nil))) ; Nessun valore comune

Proviamo:

(segmenti-comuni '((10 13) (9 18) (5 11)))
;-> (10 11)

(segmenti-comuni '((1 5) (2 6) (4 8)))
;-> (4 5)
(segmenti-comuni '((1 3) (4 6)))
;-> nil

Questo approccio ha complessità lineare O(N).

Nota: i segmenti potrebbero rappresentare degli intervalli orari (ora iniziale, ora finale).

Vedi anche "Segmenti sovrapposti" su "Note libere 10".


-----------------------------
Sequenza di Golomb (memoized)
-----------------------------

Sequenza OEIS A001462:
Golomb's sequence: a(n) is the number of times n occurs, starting with a(1) = 1.
  1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9,
  9, 9, 9, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 12,
  13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 15,
  16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 17, 17, 18, 18, 18, 18,
  18, 18, 18, 19, ...

La sequenza è definita ricorsivamente nel modo seguente:

  a(1) = 1;
  a(n+1) = 1 + a(n+1 - a(a(n)))

(define (a n)
  (if (= n 1)
      1 ; valore iniziale a(1) = 1
      (+ 1 (a (- n (a (a (- n 1)))))))) ; valore ricorsivo

Proviamo:

(map a (sequence 1 10))
;-> (1 2 2 3 3 4 4 4 5 5)
(map a (sequence 1 25))
;-> (1 2 2 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9)

La funzione è molto lenta:
(time (println (map a (sequence 1 50))))
;-> (1 2 2 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 9 10 10 10 10 10
;->  11 11 11 11 11 12 12 12 12 12 12 13 13 13 13 13 13)
;-> 34237.734

Usiamo la tecnica della memoization:

(define-macro (memoize mem-func func)
"Memoize a function"
  (set (sym mem-func mem-func)
    (letex (f func c mem-func)
      (lambda ()
        (or (context c (string (args)))
        (context c (string (args)) (apply f (args))))))))

(memoize a-memo
  (lambda (n)
  (if (= n 1)
    1
    (+ 1 (a-memo (- n (a-memo (a-memo (- n 1)))))))))

(time (println (map a-memo (sequence 1 50))))
;-> (1 2 2 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 9 10 10 10 10 10
;->  11 11 11 11 11 12 12 12 12 12 12 13 13 13 13 13 13)
;-> 0

(map a-memo (sequence 1 100))
;-> (1 2 2 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 9 10 10 10 10 10
;->  11 11 11 11 11 12 12 12 12 12 12 13 13 13 13 13 13 14 14 14 14 14 14
;->  15 15 15 15 15 15 16 16 16 16 16 16 16 17 17 17 17 17 17 17
;->  18 18 18 18 18 18 18 19 19 19 19 19 19 19 20 20 20 20 20 20 20 20
;->  21 21)

Vedi anche "Sequenza di Golomb" su "Note libere 5".


---------------------
Raggiungere un numero
---------------------

Un canguro si trova nella posizione 0 su una retta numerica infinita.
Il canguro deve raggiungere una certa posizione (bersaglio)
Il canguro si muove nel modo seguente:
A ogni mossa, può andare a sinistra o a destra.
Alla i-esima mossa si sposta di "i" passi a destra o a sinistra.
Dato un numero intero come bersaglio, trovare il numero minimo di mosse affinchè il canguro raggiunga il bersaglio.

Esempio 1:
Bersaglio = 2
Mosse: 3
Spiegazione:
Alla prima mossa, passiamo da 0 a 1 (1 passo).
Alla seconda mossa, passiamo da 1 a -1 (2 passi).
Alla terza mossa, passiamo da -1 a 2 (3 passi).

Esempio 2:
Bersaglio = 3
Passi: 2
Spiegazione:
Nella prima mossa, passiamo da 0 a 1 (1 passo).
Nella seconda mossa, passiamo da 1 a 3 (2 passi).

Poiché la linea è infinita e gli spostamenti possono essere in entrambe le direzioni, raggiungere una posizione con un numero minimo di spostamenti implica una combinazione di passaggi che sommati danno come risultato il bersaglio stesso o un numero in cui se cambiassimo la direzione di uno spostamento, potremmo finire al bersaglio.
Ad esempio, se la somma supera il bersaglio di un numero pari, possiamo invertire la direzione di uno spostamento che corrisponde alla metà di quel numero in eccesso, poiché muovendoci nella direzione opposta ridurremo la somma del doppio del numero di quello spostamento.

Notiamo che muoversi a sinistra o a destra può essere pensato in termini di somme e differenze.
L'obiettivo è il numero minimo di spostamenti (k), tale che quando sommiamo i numeri da 1 a k, il risultato sia uguale o maggiore del bersaglio.
Tuttavia, questo non è sufficiente.
Poiché dobbiamo essere in grado di raggiungere il bersaglio esatto, l'eccesso (la differenza tra la somma e il bersaglio) deve essere un numero pari.
Questo perché qualsiasi eccesso dispari non può essere compensato invertendo la direzione di una singola mossa.
Una volta raggiunto o superato l'obiettivo, se l'eccesso è pari, possiamo immaginare di poter invertire la direzione di una o più mosse per adattare la somma esattamente all'obiettivo.

Algoritmo
1) Iniziamo con "s" e "m" a 0, dove "s" rappresenta la somma delle mosse e "m" rappresenta il conteggio delle mosse.
2) Ciclo che incrementa "m" a ogni iterazione, simula ogni passo, e aggiunge "m" a "s".
   Ad ogni passaggio, controlliamo se "s" ha raggiunto o superato il bersaglio e se (s - bersaglio) è un numero pari.
   Se entrambe le condizioni sono soddisfatte, "m" è il numero minimo di mosse richieste e il ciclo termina.

(define (step-min bersaglio)
  (local (somma mosse stop)
        ; Valore assoluto del bersaglio (simmetrico rispetto a 0)
        (setq bersaglio (abs bersaglio))
        ; somma totale
        (setq somma 0)
        ; mosse
        (setq mosse 0)
        ; ciclo fino al raggiugimento della soluzione...
        (setq stop nil)
        (until stop
          ; Controlla se somma raggiunge o supera il bersaglio e
          ; la differenza tra somma e bersaglio è pari
          ; (consentendo di raggiungere bersaglio invertendo alcuni passaggi)
          (if (and (>= somma bersaglio) and (even? (- somma bersaglio)))
              ; se la condizione è raggiunta, abbiamo trovato la soluzione e
              ; fermiamo il loop
              (setq stop true)
              ;else
              (begin
                ; incremnta le mosse
                (++ mosse)
                # aggiorna somma aggiungendo le mosse correnti
                (++ somma mosse))))
        mosse))

Proviamo:

(step-min 3)
;-> 2
(step-min 3)
;-> 2
(step-min 4)
;-> 3
(step-min 5)
;-> 5
(step-min 21)
;-> 6

Funzione che sfrutta lo stesso criterio di soluzione con una diversa implementazione:

(define (step-min2 target)
  (local (ans pos)
    (setq ans 0)
    (setq pos 0)
    (setq target (abs target))
    (while (< pos target)
      (++ ans)
      (++ pos ans))
    (while (odd? (- pos target))
      (++ ans)
      (++ pos ans))
    ans))

Proviamo:

(step-min2 2)
;-> 3
(step-min2 3)
;-> 2
(step-min2 4)
;-> 3
(step-min2 5)
;-> 5
(step-min2 21)
;-> 6


---------
Lista + 1
---------

Data una lista di interi positivi (che rappresenta un numero X), restituire una lista che rappresenta il numero X + 1.
Per esempio:

  lista = (1 2 3) --> X = 123 --> X + 1 = 124 --> (1 2 4)

  lista = (2 9) --> X = 29 --> X + 1 = 30 --> (30)

(define (plus-one lst)
  (let (stop nil)
    ; ciclo dall'ultima cifra della lista
    (for (i (- (length lst) 1) 0 -1 stop)
      (cond
        ; se la cifra è minore di 9...
        ((< (lst i) 9)
          (++ (lst i))      ;... aggiungiamo 1 alla cifra e
          (setq stop true)) ; fermiamo il ciclo
        ; altrimenti la cifra corrente vale 0
        (true (setq (lst i) 0)))
    )
    ; se tutte le cifre valgono 0, allora aggiungiamo 1 all'inizio della lista
    (if (for-all zero? lst) (push 1 lst))
    lst))

Proviamo:

(plus-one '(1 2 3))
;-> (1 2 4)

(plus-one '(2 9))
;-> (3 0)

(plus-one '(8 9 9 9))
;-> ' (9 0 0 0)

(plus-one '(9))
;-> (1 0)

(plus-one '(9 9 9 9))
;-> (1 0 0 0 0)


----------------------------------------------------------
Generare un equazione di secondo grado dalle sue soluzioni
----------------------------------------------------------

Dati due numeri x1 e x2, generare l'equazione di secondo grado che ha x1 e x2 come soluzioni.

Si tratta di determinare a,b e c della seguente espressione:

  ax^2 + bx + c = 0  (1)

che può essere scritta come:

  a*x^2 - a*(x1 + x2)*x + a*(x1 * x2) = 0

Infatti, le soluzioni di (1) valgono:

          - b + sqrt(b^2 - 4ac)
  x1 = - -----------------------
                  2a

          - b - sqrt(b^2 - 4ac)
  x2 = - -----------------------
                  2a

Svolgendo le operazioni otteniamo:

               b
  x1 + x2 = - ---
               a

             c
  x1 * x2 = ---
             a

Se a=1 (coefficiente normalizzato), queste formule diventano:

  x1 + x2 = b

  x1 * x2 = c

Quindi risulta:

1) "a" è un coefficiente arbitrario diverso da zero (spesso scelto come a = 1)
2) b = -a*(x1 + x2)
3) c = a*(x1 * x2)

Funzione che genera "a", "b" e "c" dati "x1", "x2" e "a" (opzionale):

(define (eq2 x1 x2 a)
  (letn ( (a (or a 1))
        (b (- (* a (+ x1 x2))))
        (c (* a x1 x2)) )
    (list a b c)))

Proviamo:

(eq2 1 1)
;-> (1 -2 1)
(eq2 -2 3)
;-> (1 -1 -6)
(eq2 -2 3 -2)
;-> (-2 2 12)

Funzione che stampa l'equazione quadratica dati "x1", "x2" e "a" (opzionale):

(define (print-eq2 x1 x2 a)
  (let (abc (eq2 x1 x2 a))
    (if (>= (abc 1) 0)
      (setf (abc 1) (string "+ " (abc 1)))
      (setf (abc 1) (string "- " (abs (abc 1)))))
    (if (>= (abc 2) 0)
      (setf (abc 2) (string "+ " (abc 2)))
      (setf (abc 2) (string "- " (abs (abc 2)))))
    (if (!= a 1)
      (println (abc 0) "x^2 " (abc 1) "x " (abc 2) " = 0")
      (println "x^2 " (abc 1) "x " (abc 2) " = 0")) '>))

Proviamo:

(print-eq2 1 1)
;-> x^2 - 2x + 1 = 0
(print-eq2 -2 3)
;-> x^2 - 1x - 6 = 0
(print-eq2 -2 3 -2)
;-> -2x^2 + 2x + 12 = 0


---------
Boomerang
---------

Data una lista di tre punti sul piano X-Y, determinare se questi punti sono un boomerang.
Un boomerang è un insieme di tre punti che sono tutti distinti e non allineati.

Algoritmo
Calcolare area del triangolo formato dai 3 punti.
Se (area > 0), allora i punti sono un boomerang.

Nota: se tre punti sono allineati, l'area del triangolo vale 0.

Per calcolare l'area di un triangolo dato da tre punti usiamo la formula:

  Area = (1/2)*abs(x1*(y2 - y3) + x2*(y3 - y1) + x3*(y1 - y2))

(define (area p1 p2 p3)
  (let ((x1 (p1 0)) (y1 (p1 1))
        (x2 (p2 0)) (y2 (p2 1))
        (x3 (p3 0)) (y3 (p3 1)))
    (div (abs (add (mul x1 (sub y2 y3))
                   (mul x2 (sub y3 y1))
                   (mul x3 (sub y1 y2)))) 2)))

(define (boomerang? p1 p2 p3) (> (area p1 p2 p3) 0))

(boomerang? '(1 1) '(2 3) '(3 2))
;-> true
(boomerang? '(1 1) '(2 2) '(3 3))
;-> nil

Vedi anche "Punti allineati" su "Note libere 26".


------------------------------
Somma dei fattori di un numero
------------------------------

Sequenza OEIS A036288:
a(n) = 1 + integer log of n: if the prime factorization of n is n = Product (p_j^k_j) then a(n) = 1 + Sum (p_j * k_j).
  1, 3, 4, 5, 6, 6, 8, 7, 7, 8, 12, 8, 14, 10, 9, 9, 18, 9, 20, 10, 11, 14,
  24, 10, 11, 16, 10, 12, 30, 11, 32, 11, 15, 20, 13, 11, 38, 22, 17, 12,
  42, 13, 44, 16, 12, 26, 48, 12, 15, 13, 21, 18, 54, 12, 17, 14, 23, 32,
  60, 13, 62, 34, 14, 13, 19, 17, 68, 22, ...

(define (a n)
  (+ 1 (apply + (factor n))))

(map a (sequence 1 100))
;-> (1 3 4 5 6 6 8 7 7 8 12 8 14 10 9 9 18 9 20 10 11 14
;->  24 10 11 16 10 12 30 11 32 11 15 20 13 11 38 22 17 12
;->  42 13 44 16 12 26 48 12 15 13 21 18 54 12 17 14 23 32
;->  60 13 62 34 14 13 19 17 68 22 27 15 72 13 74 40 14 24
;->  19 19 80 14 13 44 84 15 23 46 33 18 90 14 21 28 35 50
;->  25 14 98 17 18 15)


---------------------------
Somma di numeri consecutivi
---------------------------

Dato un numero intero N, restituire il numero di modi in cui è possibile scrivere N come somma di numeri interi positivi consecutivi.

Esempio:
N = 5
2 + 3, 5 --> 2

Esempio:
N = 9
4 + 5, 2 + 3 + 4, 9 --> 3

Esempio:
N = 15
8 + 7, 4 + 5 + 6, 1 + 2 + 3 + 4 + 5, 15 --> 4

(define (somma-numeri-consecutivi n)
  (let ( (out 0) (i 1) (triangolare 1) )
    ;(while (< triangolare n) ; non conta N come somma
    (while (<= triangolare n)
      (if (zero? (% (- n triangolare) i)) (++ out))
      (++ i)
      (++ triangolare))
    out))

Ecco come funziona:
1. Inizializzazione
   "out": conta il numero di modi validi in cui può essere espresso n.
   "i": rappresenta il numero di termini consecutivi nella somma.
   "triangolare": la somma dei primi i numeri naturali (1 + 2 + ... + i = i*(i+1)/2).
2. Ciclo While
   Continua mentre "triangolare" i*(i+1)/2 è minore o uguale a n.
   Questo assicura che stiamo controllando solo somme che potrebbero essere valide.
3. Verifica validità
   Se n - "triangolare" è divisibile per i, allora n può essere espresso come la somma di i numeri interi consecutivi.
   Questo funziona perché la somma di i numeri interi consecutivi può essere scritta come:
   i*inizio + triangolare = n.
4. Aggiorna valori
   Incrementa "i" (numero di termini).
   Aggiorna "triangolare" alla somma dei primi i numeri naturali.
5. Restitusce il risultato
   Dopo aver controllato tutti i valori possibili, restituisce "out".
Complessità temporale: O(sqrt(n)), poiché il ciclo viene eseguito finché triangolare <= N
Complessità spaziale: O(1), non viene utilizzato spazio aggiuntivo.

Proviamo:

(somma-numeri-consecutivi 4)
;-> 3
(somma-numeri-consecutivi 5)
;-> 2
(somma-numeri-consecutivi 9)
;-> 3
(somma-numeri-consecutivi 15)
;-> 4

(map somma-numeri-consecutivi (sequence 1 100))
;-> (1 2 2 3 2 4 2 4 3 4 2 6 2 4 4 5 2 6 2 6 4 4 2 8 3 4 4 6 2 8 2 6 4 4 4
;->  9 2 4 4 8 2 8 2 6 6 4 2 10 3 6 4 6 2 8 4 8 4 4 2 12 2 4 6 7 4 8 2 6 4
;->  8 2 12 2 4 6 6 4 8 2 10 5 4 2 12 4 4 4 8 2 12 4 6 4 4 4 12 2 6 6 9)

Curiosamente questa sequenza rappresenta anche il numero di divisori di N.

Sequenza OEIS A000005:
d(n) (also called tau(n) or sigma_0(n)), the number of divisors of n.
  1, 2, 2, 3, 2, 4, 2, 4, 3, 4, 2, 6, 2, 4, 4, 5, 2, 6, 2, 6, 4, 4, 2, 8,
  3, 4, 4, 6, 2, 8, 2, 6, 4, 4, 4, 9, 2, 4, 4, 8, 2, 8, 2, 6, 6, 4, 2, 10,
  3, 6, 4, 6, 2, 8, 4, 8, 4, 4, 2, 12, 2, 4, 6, 7, 4, 8, 2, 6, 4, 8, 2, 12,
  2, 4, 6, 6, 4, 8, 2, 10, 5, 4, 2, 12, 4, 4, 4, 8, 2, 12, 4, 6, 4, 4, 4,
  12, 2, 6, 6, 9, 2, 8, 2, 8, ...

(define (factor-group num)
"Factorize an integer number"
  (if (= num 1) '((1 1))
    (letn ( (fattori (factor num))
            (unici (unique fattori)) )
      (transpose (list unici (count unici fattori))))))

(define (divisors-count num)
"Count the divisors of an integer number"
  (if (= num 1)
      1
      (let (lst (factor-group num))
        (apply * (map (fn(x) (+ 1 (last x))) lst)))))

(map divisors-count (sequence 1 100))
;-> (1 2 2 3 2 4 2 4 3 4 2 6 2 4 4 5 2 6 2 6 4 4 2 8 3 4 4 6 2 8 2 6 4 4 4
;->  9 2 4 4 8 2 8 2 6 6 4 2 10 3 6 4 6 2 8 4 8 4 4 2 12 2 4 6 7 4 8 2 6 4
;->  8 2 12 2 4 6 6 4 8 2 10 5 4 2 12 4 4 4 8 2 12 4 6 4 4 4 12 2 6 6 9)

(= (map somma-numeri-consecutivi (sequence 1 100))
   (map divisors-count (sequence 1 100)))
;-> true

Vedi anche "Numeri somma di numeri consecutivi" su "Note libere 14".


---------------------------
Cifre nelle proprie colonne
---------------------------

Data una lista di numeri interi, stampare ogni numero in colonna nell'ordine -0123456789, ignorando eventuali cifre duplicate.

Esempio:
Lista: (41 1729 1 57869 -45613839 3483425877823495 -10984 -984938744 0 22)
Output:
;-> -0123456789  <- Aggiunto solo a scopo chiarificatore, non fa parte dell'output.
;->   1  4
;->   12    7 9
;->   1
;->       56789
;-> - 1 3456 89
;->    2345 789
;-> -01  4   89
;-> -   34  789
;->  0
;->    2

Funzione che allinea le cifre di un numero nel formato "-0123456789":

(define (line-up num)
  (let (str (dup " " 11)) ; " ", "-" e "0"..."9"
    (if (< num 0) (begin (setf (str 0) "-") (setq num (abs num))))
    (if (zero? num) (setf (str 1) "0"))
    (while (!= num 0)
      (setf (str (+ (% num 10) 1)) (string (% num 10)))
      (setq num (/ num 10)))
    str))

(line-up -13728553)
;-> "- 123 5 78 "

Funzione che allinea le cifre dei numeri di una lista nel formato "-0123456789":

(define (align-digits lst)
  (dolist (el lst)
    (println (line-up el))) '>)

Proviamo:

(align-digits '(1 729 4728510 -3832 748129321 89842 -938744 0 11111))
;->   1
;->    2    7 9
;->  012 45 78
;-> -  23    8
;->   1234  789
;->    2 4   89
;-> -   34  789
;->  0
;->   1

(align-digits '(4 534 4 4 53 26 71 835044))
;->      4
;->     345
;->      4
;->      4
;->     3 5
;->    2   6
;->   1     7
;->  0  345  8

(align-digits '(99 88 77 66 55 44 33 22 11 0))
;->           9
;->          8
;->         7
;->        6
;->       5
;->      4
;->     3
;->    2
;->   1
;->  0

Nota: Progetto codice segreto
Per il seguente alfabeto vedi "Caratteri con matrice 5x5" su "Note libere 22".

   █   ████   ████ ████  █████ █████  ████ █   █ █████
  █ █  █   █ █     █   █ █     █     █     █   █   █
 █   █ ████  █     █   █ ███   ███   █  ██ █████   █
 █████ █   █ █     █   █ █     █     █   █ █   █   █
 █   █ ████   ████ ████  █████ █      ████ █   █ █████


 █████ █   █ █     █   █ █   █  ███  ████   ███  ████
    █  █  █  █     ██ ██ ██  █ █   █ █   █ █   █ █   █
    █  ███   █     █ █ █ █ █ █ █   █ ████  █ █ █ ████
 █  █  █  █  █     █   █ █  ██ █   █ █     █  █  █   █
  ██   █   █ █████ █   █ █   █  ███  █      ██ █ █   █

  ████ █████ █   █ █   █ █   █ █   █ █   █ █████
 █       █   █   █ █   █ █   █  █ █   █ █     █
  ███    █   █   █ █ █ █ █   █   █     █     █
     █   █   █   █ ██ ██  █ █   █ █    █    █
 ████    █    ███  █   █   █   █   █   █   █████

Modi di indicizzare la lettera "A" con il formato "-0123456789":

 -0123456789 -0123456789 -0123456789 -0123456789 -0123456789 -0123456789
   █            █            █            █            █            █
  █ █          █ █          █ █          █ █          █ █          █ █
 █   █        █   █        █   █        █   █        █   █        █   █
 █████        █████        █████        █████        █████        █████
 █   █        █   █        █   █        █   █        █   █        █   █
   1            2          ...
  0 2          1 3         ...
 -3           0   4        ...
 -0123        01234        ...
 -3           0   4        ...

Nel codice segreto la lista (1 20 -3 -1230 -3) rappresenta il carattere "A".

(align-digits '(1 20 -3 -1230 -3))
;->   1
;->  0 2
;-> -   3
;-> -0123
;-> -   3

Oppure anche (usando la seconda codifica della "A"):

(align-digits '(2 13 40 12340 40))
;->    2
;->   1 3
;->  0   4
;->  01234
;->  0   4

Possiamo "offuscare" i numeri della lista inserendo ad ogni numero alcune cifre che appartengono al numero stesso.
Per esempio:
  (1 20 -3 -1230 -3) --> (11 2020 -33 -132003 -3)

(align-digits '(11 2020 -33 -132003 -3))
;->   1
;->  0 2
;-> -   3
;-> -0123
;-> -   3


-------------
Leetcode 1227
-------------

Vediamo un problema dal sito Leetcode:

https://leetcode.com/problems/airplane-seat-assignment-probability/description/

1227. Airplane Seat Assignment Probability
------------------------------------------
n passengers board an airplane with exactly n seats.
The first passenger has lost the ticket and picks a seat randomly.
But after that, the rest of the passengers will:

a) Take their own seat if it is still available, or

b) Pick other seats randomly when they find their seat occupied.

Return the probability that the nth person gets his own seat.

Example 1:
Input: n = 1
Output: 1.00000
Explanation: The first person can only get the first seat.

Example 2:
Input: n = 2
Output: 0.50000
Explanation: The second person has a probability of 0.5 to get the second seat (when first person gets the first seat).

Constraints:

1 <= n <= 10^5

Soluzione

(define (own-seat? n) (if (= n 1) 1 0.5))

(map own-seat? (sequence 1 10))
;-> (1 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5)

Per la spiegazione vedi "Volo completo (Programming Praxis)" su "Domande programmatori".


-----------------------
Generazione di frazioni
-----------------------

Dato un numero intero N, restituire una lista di tutte le frazioni semplificate comprese tra 0 e 1 (esclusi) tali che il denominatore sia minore o uguale a N.

Esempio:
N = 3
Frazioni: 1/2, 1/3 , 2/3

Esempio:
N = 4
Frazioni: 1/2, 1/3, 1/4, 2/3, 3/4
In questo caso 2/4 = 1/2.

(define (fractions n)
  (let (out '())
    (for (den 2 n)
      (for (num 1 den)
        ;(print num { } den) (read-line)
        (if (= (gcd num den) 1) (push (list num den) out -1))))
    out))

Proviamo:

(fractions 3)
;-> ((1 2) (1 3) (2 3))

(fractions 4)
;-> ((1 2) (1 3) (2 3) (1 4) (3 4))

(fractions 10)
;-> ((1 2) (1 3) (2 3) (1 4) (3 4) (1 5) (2 5) (3 5) (4 5) (1 6) (5 6) (1 7)
;->  (2 7) (3 7) (4 7) (5 7) (6 7) (1 8) (3 8) (5 8) (7 8) (1 9) (2 9) (4 9)
;->  (5 9) (7 9) (8 9) (1 10) (3 10) (7 10) (9 10))

(length (fractions 10))
;-> 31
(length (fractions 100))
;-> 3043
(length (fractions 1000))
;-> 304191


----------------------------
Quanto conta veloce newLISP?
----------------------------

Vediamo la velocità di newLISP nel contare da 1 a N.

Contatore con ciclo "while":

(define (speed N)
  (local (contatore start elapsed)
    (setq contatore 0)
    (setq elapsed 0)
    (setq start (time-of-day))
    (while (< contatore N) (++ contatore))
    (setq elapsed (- (time-of-day) start))
    (println "Conteggio: " contatore)
    (println "Tempo: " elapsed " millisec")
    (println "Velocità: " (int (div contatore (div elapsed 1000))) " numeri/sec") '>))

(speed 1e8)
;-> Conteggio: 100000000
;-> Tempo: 4835 millisec
;-> Velocità: 20682523 numeri/sec

Tempo necessario per arrivare a MAX-INT:

(setq MAX-INT 9223372036854775807)

(define (period msec show)
"Print a millisec number in hours, minutes, seconds, millisec"
  (local (conv unit expr val out)
    (setq conv '(("d" 86400000) ("h" 3600000) ("m" 60000) ("s" 1000) ("ms" 1)))
    (setq out '())
    (setq msec (int msec))
    (dolist (el conv)
      (setq unit (el 0))
      (setq expr (el 1))
      (setq val (/ msec expr))
      ; numero di millisecondi rimasti
      ; (resto della divisione)
      (setq msec (% msec expr))
      (push val out -1)
      (if (and (> val 0) show)
        (print val unit " ")
      )
    )
    (if show (println))
    out))

(period (div MAX-INT 20682523) true)
;-> 5161d 11h 43s 756ms

Contatore con ciclo "dotimes":

(define (speed1 N)
  (local (contatore start elapsed)
    (setq elapsed 0)
    (setq start (time-of-day))
    ;(setq contatore (dotimes (i N) i))
    (setq contatore 0)
    (dotimes (i N) (++ contatore))
    (setq elapsed (- (time-of-day) start))
    (println "Conteggio: " contatore)
    (println "Tempo: " elapsed " millisec")
    (println "Velocità: " (int (div contatore (div elapsed 1000))) " numeri/sec") '>))

(speed1 1e8)
;-> Conteggio: 100000000
;-> Tempo: 2855 millisec
;-> Velocità: 35026269 numeri/sec

Tempo necessario per arrivare a MAX-INT:

(period (div MAX-INT 35026269) true)
;-> 3047d 18h 27m 56s 931ms

Contatore del ciclo "dotimes":

(define (speed2 N)
  (local (contatore start elapsed)
    (setq elapsed 0)
    (setq start (time-of-day))
    ;(setq contatore 0)
    ;(dotimes (i N) (++ contatore))
    (setq contatore (dotimes (i N) i))
    (setq elapsed (- (time-of-day) start))
    (println "Conteggio: " (+ 1 contatore))
    (println "Tempo: " elapsed " millisec")
    (println "Velocità: " (int (div contatore (div elapsed 1000))) " numeri/sec") '>))

(speed2 1e8)
;-> Conteggio: 100000000
;-> Tempo: 971 millisec
;-> Velocità: 102986610 numeri/sec

Tempo necessario per arrivare a MAX-INT:

(period (div MAX-INT 102986610) true)
;-> 1036d 13h 29m 3s 991ms


---------------------------
Terne pitagoriche primitive
---------------------------

Una terna pitagorica è costituita da tre numeri interi positivi a, b e c con a < b < c tale che a^2 + b^2 = c^2.
Ad esempio, i tre numeri 3, 4 e 5 formano una tripla pitagorica perché 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
Le espressioni matematiche utilizzate per generare le terne pitagoriche primitive sono quelle del metodo di Euclide:

1. Calcolo dei numeri della terna
   - a = m^2 - n^2
   - b = 2*m*n
   - c = m^2 + n^2

Dove m e n sono numeri interi positivi con m > n > 0 .

Queste formule permettono di generare una terna pitagorica (a, b, c) che soddisfa l'equazione:
 a^2 + b^2 = c^2

2. Condizione di primitività:
   - Una terna è considerata primitiva se il massimo comune divisore (gcd) di a, b e c è 1.
   - Utilizzando la funzione gcd: gcd(a, b) = gcd(gcd(a, b), c) = 1

Il programma seguente genera tutte le terne pitagoriche primitive con "c" minore del parametro "limite".

(define (terna-primitiva? a b c)
  (= 1 (gcd (gcd a b) c)))

(define (terne-primitive limite)
  (let (out '())
    (for (m 2 (sqrt limite))
      (for (n 1 (- m 1))
        (let ((a (- (* m m) (* n n)))
              (b (* 2 m n))
              (c (+ (* m m) (* n n))))
          (when (and (< c limite) (terna-primitiva? a b c))
            (push (list a b c) out -1)))))
    out))

(terne-primitive 200)
;-> ((3 4 5) (5 12 13) (15 8 17) (7 24 25) (21 20 29) (9 40 41) (35 12 37)
;->  (11 60 61) (45 28 53) (33 56 65) (13 84 85) (63 16 65) (55 48 73)
;->  (39 80 89) (15 112 113) (77 36 85) (65 72 97) (17 144 145) (99 20 101)
;->  (91 60 109) (51 140 149) (19 180 181) (117 44 125) (105 88 137)
;->  (85 132 157) (57 176 185) (143 24 145) (119 120 169) (95 168 193)
;->  (165 52 173) (153 104 185) (195 28 197))

Vedi anche "Terne pitagoriche" su "Funzioni varie".


--------------------------------------------
Coppie di una lista divisibili per un numero
--------------------------------------------

Data una lista di numeri interi e un intero k, trovare il numero di coppie (i,j) tali che il prodotto di lista[i] e lista[j] sia divisibile per k.

Algoritmo:
Calcolare il massimo comune divisore (MCD) di ogni numero della lista con k.
Quindi, per ogni coppia (nums[i], nums[j]), controllare se i relativi valori MCD moltiplicati tra loro sono divisibili per k.
In caso affermativo, aggiungerli al conteggio.

(define (coppie-divisibili lst k)
  (let ( (out '()) (mcds (map (fn(x) (gcd x k)) lst)) )
    (for (i 0 (- (length mcds) 2))
      (for (j (+ i 1) (- (length mcds) 1))
        (if (zero? (% (* (mcds i) (mcds j)) k))
            (push (list i j) out -1))))
    out))

Proviamo:

(setq lst '(1 2 3 4 5))
(setq coppie (coppie-divisibili lst 2))
;-> ((0 1) (0 3) (1 2) (1 3) (1 4) (2 3) (3 4))
(length coppie)
;-> 7
(map (fn(x) (* (lst (x 0)) (lst (x 1)))) coppie)
;-> (2 4 6 8 10 12 20)


--------------------------------------
Sequenze contigue lanciando una moneta
--------------------------------------

Lanciando una moneta N volte, qual'è la lunghezza media della sequenza tutte Teste o tutte Croci?
La lunghezza di una sequenza (tutte Teste o Croci) è data dal massimo numero di volte consecutive che compare Testa o Croce.
Per esempio, nella sequenza "C C T T C C T T T T T C C T C T T T C C T" la lunghezza massima vale 5 ed è data dalla sottosequenza "T T T T T".

0 = T
1 = C

Metodo 1
--------
Generiamo un elemento casuale alla volta e teniamo traccia della lunghezza massima delle sottosequenze create.

Funzione che genera una sequenza di 1 e 0 lunga N e restituisce la lunghezza massima delle sottosequenze di 0 o 1 consecutivi:

(define (subseq-base N)
  (local (out max-len cur-len cur old num)
    (setq out '())
    (setq max-len 0)
    (setq cur (rand 2))
    (push cur out -1)
    (setq cur-len 1)
    (setq old cur)
    (for (i 2 N)
      (setq cur (rand 2))
      (push cur out -1)
      (cond ((= cur old) (++ cur-len))
            (true ; (!= cur old)
              (when (> cur-len max-len) (setq max-len cur-len) (setq num old))
              (setq old cur)
              (setq cur-len 1)))
      (when (> cur-len max-len) (setq max-len cur-len) (setq num old)))
    (println out)
    (list max-len num)))

Proviamo:

(subseq-base 10)
;-> (0 0 1 0 0 0 1 0 1 1)
;-> (3 0)

(subseq-base 20)
;-> (1 0 1 0 0 1 0 0 1 1 1 1 1 0 0 0 1 0 0 0)
;-> (5 1)

Funzione che genera una sequenza di 1 e 0 lunga N e restituisce la lunghezza massima delle sottosequenze di 0 o 1 consecutivi (ottimizzata per la simulazione):

(define (subseq N)
  (local (max-len cur-len cur old)
    (setq max-len 0)
    (setq cur (rand 2))
    (setq cur-len 1)
    (setq old cur)
    (for (i 2 N)
      (setq cur (rand 2))
      (cond ((= cur old) (++ cur-len))
            (true ; (!= cur old)
              (setq max-len (max max-len cur-len))
              (setq old cur)
              (setq cur-len 1)))
      (setq max-len (max max-len cur-len)))
    max-len))

Proviamo:

(subseq 10)
;-> 4

(subseq 20)
;-> 5

Funzione che simula il lancio di N monete per determinato numero di volte:

(define (simula N iter)
  (local (totale massima media current)
    (setq totale 0)
    (setq massima 0)
    (for (i 1 iter)
      (setq current (subseq N))
      (setq massima (max current massima))
      (++ totale current))
    (println "N = " N)
    (println "Massima = " massima)
    (println "Media = " (div totale iter)) '>))

Proviamo:

(simula 10 1e5)
;-> N = 10
;-> Massima = 10
;-> Media = 3.66759

(simula 100 1e5)
;-> N = 100
;-> Massima = 23
;-> Media = 6.97662

(time (simula 1000 1e5))
;-> N = 1000
;-> Massima = 26
;-> Media = 10.29992
;-> 16836.908

Metodo 2
--------
Costruiamo la lista casuale con "(rand 2 N)" e poi cerchiamo la sottosequenza di elementi uguali più lunga.

Funzione che prende una lista di 0 e 1 e restituisce la lunghezza massima delle sottosequenze di 0 o 1 consecutivi:

(define (subseq2 lst)
  (local (max-len current-len prev current)
    (setq max-len 1)
    (setq current-len 1)
    (setq prev (lst 0))
    (dolist (current (rest lst))
      (if (= current prev)
          (++ current-len)
          (begin
            (setq max-len (max max-len current-len))
            (setq current-len 1)
            (setq prev current))))
    (max max-len current-len)))

Proviamo:

(subseq2 '(1 1 1 0 0 1 0 1 0 0 0 0))
;-> 4
(subseq2 '(1 1 1 1 1 0 0 1 0 1 0 0 0 0))
;-> 5
(subseq2 '(0 1 1 1 1 0))
;-> 4

Funzione che simula il lancio di N monete per determinato numero di volte:

(define (simula2 N iter)
  (local (totale massima media current)
    (setq totale 0)
    (setq massima 0)
    (for (i 1 iter)
      (setq current (subseq2 (rand 2 N)))
      (setq massima (max current massima))
      (++ totale current))
    (println "N = " N)
    (println "Massima = " massima)
    (println "Media = " (div totale iter)) '>))

Proviamo:

(simula2 10 1e5)
;-> N = 10
;-> Massima = 10
;-> Media = 3.66759

(simula2 100 1e5)
;-> N = 100
;-> Massima = 23
;-> Media = 6.97662

(time (simula2 1000 1e5))
;-> N = 1000
;-> Massima = 26
;-> Media = 10.29992
;-> 11580.025

(time (simula2 10000 1e5))
;-> N = 10000
;-> Massima = 28
;-> Media = 13.61738
;-> 115152.254

Altra funzione che prende una lista di 0 e 1 e restituisce la lunghezza massima delle sottosequenze di 0 o 1 consecutivi:

(define (subseq3 lst)
  (local (longest long0 long1)
    (setq longest 0)
    (dolist (el lst)
      (if (zero? el)
          (begin (++ long0) (setq long1 0))
          (begin (++ long1) (setq long0 0)))
      (setq longest (max longest long0 long1))
    )
    longest))

Proviamo:

(subseq3 '(1 1 1 0 0 1 0 1 0 0 0 0))
;-> 4
(subseq3 '(1 1 1 1 1 0 0 1 0 1 0 0 0 0))
;-> 5
(subseq3 '(0 1 1 1 1 0))
;-> 4

Funzione che simula il lancio di N monete per determinato numero di volte:

(define (simula3 N iter)
  (local (totale massima media current)
    (setq totale 0)
    (setq massima 0)
    (for (i 1 iter)
      (setq current (subseq3 (rand 2 N)))
      (setq massima (max current massima))
      (++ totale current))
    (println "N = " N)
    (println "Massima = " massima)
    (println "Media = " (div totale iter)) '>))

Proviamo:

(simula3 10 1e5)
;-> N = 10
;-> Massima = 10
;-> Media = 3.66759

(simula3 100 1e5)
;-> N = 100
;-> Massima = 23
;-> Media = 6.97662

(time (simula3 1000 1e5))
;-> N = 1000
;-> Massima = 25
;-> Media = 10.29318
;-> 14871.415

(time (simula3 10000 1e5))
;-> N = 10000
;-> Massima = 28
;-> Media = 13.6133
;-> 147778.812

La funzione più veloce è "simula2".

(time (simula2 100000 1e3))
;-> N = 100000
;-> Massima = 26
;-> Media = 16.812
;-> 11376.299

(time (simula2 1000000 1e3))
;-> N = 1000000
;-> Massima = 28
;-> Media = 20.227
;-> 121220.188


-----------------------------------------------
Numero più piccolo con N cifre divisibile per N
-----------------------------------------------

Dato un numero intero positivo N, determinare il più piccolo numero di N cifre divisibile per N.

Sequenza OEIS A053041:
Smallest n-digit number divisible by n.
  1, 10, 102, 1000, 10000, 100002, 1000006, 10000000, 100000008, 1000000000,
  10000000010, 100000000008, 1000000000012, 10000000000004, 100000000000005,
  1000000000000000, 10000000000000016, 100000000000000008, 1000000000000000018,
  10000000000000000000, ...

Metodo brute-force:

(define (smallest N)
  (let ( (massimo (- (pow 10 N) 1)) ; valore massimo di N cifre
         (minimo (pow 10 (- N 1)))  ; valore minimo di N cifre
         (out nil) (stop nil) )
    (for (num minimo massimo 1 stop) ; ciclo da minimo a massimo
      (when (zero? (% num N))
        (setq out num) (setq stop true)))
    out))

Proviamo:

(smallest 2)
;-> 10

(smallest 3)
;-> 102

(smallest 4)
;-> 1000

(map smallest (sequence 1 10))
;-> (1 10 102 1000 10000 100002 1000006 10000000 100000008 1000000000)

Metodo matematico:
Se il numero è divisibile per N, allora il numero sarà della forma N * X per un numero intero positivo X.
Poiché deve essere il più piccolo numero di N cifre, allora X sarà dato da:

   10^(N-1)
  ----------
      N

Pertanto, il più piccolo numero di N cifre è dato da:

          10^(N-1)
  N*ceil(----------)
             N

(define (smallest2 N)
    (* N (ceil (div (pow 10 (- N 1)) N))))

(map smallest2 (sequence 1 10))
;-> (1 10 102 1000 10000 100002 1000006 10000000 100000008 1000000000)


------------------------------------------------------
Numero più piccolo con N cifre divisibile per K interi
------------------------------------------------------

Data una lista di K interi positivi e un numero intero N, determinare il numero più piccolo con N cifre che è divisibile per ognuno dei K interi della lista.

Esempio 1
---------
Trovare il più piccolo numero a due cifre divisibile per 2, 3 e 4.

Calcoliamo il minimo comune multiplo di tutti i numeri:

  mcm(2 3 4) = 12

Adesso dobbiamo trovare il più piccolo multiplo del mcm che sia maggiore o uguale al più piccolo numero a due cifre.

Il più piccolo numero a due cifre vale 10.

Calcoliamo il rapporto tra il numero più piccolo con due cifre (10) e il minimo comune multiplo (12):

  base = 10/12 = 0.8333...

Poichè il rapporto deve essere intero, prendiamo l'intero successivo (ceil):

  base = ceil(10/12) = 1

Quindi il primo numero con due cifre da provare vale:

  numero = base * mcm = 1 * 12 = 12

Se il numero ha due cifre, allora è la soluzione (come in questo caso 12).
Altrimenti provare con il prossimo multiplo del numero (2*12=24).

Esempio 2
---------
Trovare il più piccolo numero a quattro cifre divisibile per 3, 4, 5 e 6.

  mcm(3 4 5 6) = 60
  Numero più piccolo a 4 cifre = 1000
  base = ceil(1000/60) = 17
  numero = base * mcm = 17 * 60 = 1020
Il numero ha quattro cifre ed è la soluzione.

Esempio 3
---------
Trovare il più piccolo numero a sei cifre divisibile per 4, 8, 12 e 16.

  mcm(4 8 12 16) = 48
  Numero più piccolo a 6 cifre = 100000
  base = ceil(100000/48) = 2084
  numero = 2084*48 = 99912
Il numero 99912 ha solo cinque cifre, quindi proviamo con il prossimo multiplo:
  numero = 2085*48 = 100032
Il numero 100032 ha sei cifre ed è la soluzione.


Esempio 4
---------
Trovare il più piccolo numero a due cifre divisibile per 3, 4, 5, 6 e 7.

  mcm(3 4 5 6 7) = 420
  Numero più piccolo a 2 cifre = 10
  base = ceil(10/420) = 1
  numero = base * mcm = 1 * 420 = 420
Il primo multiplo vale 420 ed è maggiore di 99 (che è il più grande numero a due cifre), quindi non esiste nessun numero a due cifre divisibile per 3, 4, 5, 6 e 7.

(define (lcm_ a b) (/ (* a b) (gcd a b)))
(define-macro (lcm)
"Calculates the lcm of two or more number"
  (apply lcm_ (map eval (args)) 2))

(apply lcm '(2 3 4))
;-> 12

1) Soluzione (brute-force)

(define (brute digit divisors)
  (local (numero found impossible)
    (setq numero (pow 10 (- digit 1)))
    (setq found nil)
    (setq impossible nil)
    (until (or found impossible)
      (cond ((> (length numero) digit) ; soluzione impossibile
              (setq impossible true))
            ((for-all (fn(x) (zero? (% numero x))) divisors) ; soluzione
            (setq found true))
            (true (++ numero))) ; proviamo il prossimo numero
    (cond (impossible nil)
          (found numero))))

Proviamo:

(brute 2 '(2 3 4))
;-> 12
(brute 4 '(3 4 5 6))
;-> 1020
(brute 6 '(4 8 12 16))
;-> 100032
(brute 2 '(3 4 5 6 7))
;-> nil
(brute 6 '(5 7 11 13))
;-> 100100
(brute 4 '(1 2 3 4 5 6 7 8 9 10))
;-> 2520

2) Soluzione (minimo comune multiplo)

(define (lowest-number digit divisors)
  (local (mcm minimo base found impossible numero)
    (setq mcm (apply lcm divisors))
    (setq minimo (pow 10 (- digit 1)))
    (setq base (ceil (div minimo mcm)))
    (setq found nil)
    (setq impossible nil)
    (until (or found impossible)
      (setq numero (* base mcm))
      (cond ((and (= (length numero) digit) (>= numero minimo)) ; soluzione
            (setq found true))
            ((> (length numero) digit) ; soluzione impossibile
              (setq impossible true))
            ((< numero minimo) ; proviamo il prossimo multiplo
              (++ base))))
    (cond (impossible nil)
          (found numero))))

Proviamo:

(lowest-number 2 '(2 3 4))
;-> 12
(lowest-number 4 '(3 4 5 6))
;-> 1020
(lowest-number 6 '(4 8 12 16))
;-> 100032
(lowest-number 2 '(3 4 5 6 7))
;-> nil
(lowest-number 6 '(5 7 11 13))
;-> 100100
(lowest-number 4 '(1 2 3 4 5 6 7 8 9 10))
;-> 2520


----------------------------------------
Prodotto delle cifre minimo e divisibile
----------------------------------------

Dati due numeri interi positivi k e t:

1) determinare il più piccolo numero maggiore o uguale a k tale che il prodotto delle sue cifre sia divisibile per t. Se tale numero non esiste, restituire "nil".

2) determinare il più piccolo numero zero-free maggiore o uguale a k tale che il prodotto delle sue cifre sia divisibile per t. Se tale numero non esiste, restituire "nil".

Un numero si dice "zero-free" se nessuna delle sue cifre è 0.

Problema 1
----------
(define (digit-prod num)
"Calculates the product of the digits of an integer"
  (if (zero? num)
      0
      (let (out 1)
        (while (!= num 0)
          (setq out (* out (% num 10)))
          (setq num (/ num 10))
        )
    out)))

(define (p1 k t)
  (let ( (out nil) (stop nil) (num k) )
    (until stop
      (if (zero? (% (digit-prod num) t)) (set 'out num 'stop true))
      (++ num))
    out))

Proviamo:

(p1 10 2)
;-> 10

(p1 1234 256)
;-> 1240

(p1 12355 50)
;-> 12355

(p1 11111 26)
;-> 11120

(p2 12355 26)

Problema 2
----------

(define (p2 k t)
  (let ( (out nil) (stop nil) (num k))
    (until stop
      (setq product (digit-prod num))
      (if (and (!= product 0) (zero? (% product t))) (set 'out num 'stop true))
      (++ num))
    out))

Proviamo:

(p2 1234 256)
;-> 1488

(p2 12355 50)
;-> 12355

Sembra che funzioni... purtroppo il seguente esempio non termina:

(p2 11111 26)

Per risolvere il problema dobbiamo utilizzare un pò di matematica.

Consideriamo un numero N.
Se tutte le cifre di N non contengono zeri, il prodotto delle cifre di N è costituito dai fattori primi di queste cifre.
Le cifre di un numero (da 1 a 9) hanno i seguenti fattori primi:

  - 1: Nessun fattore primo
  - 2: 2
  - 3: 3
  - 4: 2^2
  - 5: 5
  - 6: 2*3
  - 7: 7
  - 8: 2^3
  - 9: 3^2

Prendendo solo i fattori unici otteniamo: 2, 3, 5 e 7.
Quindi gli unici fattori primi del prodotto delle cifre di qualunque numero N sono 2, 3, 5, e 7.
Affinché il prodotto delle cifre di un numero sia divisibile per t, t deve essere scomponibile nei fattori primi disponibili dalle cifre (da 1 a 9), cioè 2, 3, 5 e 7.
Se t contiene fattori primi che non possono essere formati dal prodotto delle cifre 1-9, allora esiste un numero zero-free il cui prodotto delle cifre è divisibile per t.

In altre parole, se i fattori di t sono diversi da (2 3 5 7), allora non esiste soluzione.
Questo perchè (2 3 5 7) sono gli unici fattori del prodotto delle cifre di qualunque numero N.

(map factor (sequence 1 9))
;-> (nil (2) (3) (2 2) (5) (2 3) (7) (2 2 2) (3 3))
(unique (flat (map factor (sequence 1 9))))
;-> (nil 2 3 5 7)
(unique (flat (map factor (sequence 2 9))))
;-> (2 3 5 7)

(define (digit-prod num)
"Calculates the product of the digits of an integer"
  (if (zero? num)
      0
      (let (out 1)
        (while (!= num 0)
          (setq out (* out (% num 10)))
          (setq num (/ num 10))
        )
    out)))

(define (p21 k t)
  (let ( (divisors '(2 3 5 7))
         (t-factors (unique (factor t))) )
    ; controlla se i fattori di t sono un sottoinsieme di (2 3 5 7)
    (if (null? (difference t-factors divisors))
        (let ( (out nil) (stop nil) (num k) )
          (until stop
            (setq product (digit-prod num))
            (if (and (!= product 0) (zero? (% product t))) (set 'out num 'stop true))
            (++ num))
          out)
        ;else
        nil)))

Proviamo:

(p21 1234 256)
;-> 1488
(p21 12355 50)
;-> 12355
(p21 11111 26)
;-> nil

Verifichiamo che il prodotto delle cifre dei numeri da 1 ad un dato limite contiene solo i fattori 2, 3, 5 e 7.

(define (test limite)
  (let ( (divisors '(2 3 5 7))
         (prodotti (map digit-prod (sequence 1 limite))) )
    (dolist (el prodotti)
      (if (> el 1) ; non considera 0 e 1
        (begin
          (setq fattori (unique (factor el)))
          ; esiste qualche fattore di t diverso da 2 o 3 o 5 o 7?
          (if (!= (difference fattori '(2 3 5 7)) '())
            (println el { } fattori)))))))

(test 1e7)
;-> nil


------------------------------
Fattori in comune tra N numeri
------------------------------

Dati N numeri interi positivi maggiori di 1, determinare i fattori che sono in comune a tutti gli interi.

(define (take-common lst1 lst2)
"Take common element of two lists (take 1:1)"
  (local (a b f out)
    (setq out '())
    ; copy of lst1
    (setq a lst1)
    ; copy of lst2
    (setq b lst2)
    ; loop
    (dolist (el lst1)
      ; if currente element of lst1 exists in lst2...
      (if (setq f (ref el b))
        (begin
          ; then insert current element of lst1 in list 'out'
          (push el out -1)
          ; and delete current element from both lists
          (pop a (ref el a)) (pop b f))
      )
    )
    ;(println a) (println b)
    out))

(define-macro (take-commons)
"Take common element of n lists (take 1:1)"
  (apply take-common (map eval (args)) 2))

Funzione che trova i fattori comuni di N liste:

(define (common-factors lst) (apply take-commons (map factor lst)))

Proviamo:

(common-factors '(3 5 24 56 128 78))
;-> ()
(common-factors '(2 1246 24 56 128 78))
;-> (2)


--------------------------
Intersezione di rettangoli
--------------------------

Dati due rettangoli, determinare il rettangolo che rappresenta la loro intersezione (overlay).
1) ogni rettangolo è rappresentato da una lista con i punti lower-left e upper-right: (x0,y0) e (x1,y1).
2) non sono ruotati, sono tutti rettangoli con i lati a due a due paralleli all'asse X e all'asse Y.
3) sono posizionati in modo casuale: possono toccarsi ai bordi, sovrapporsi o non avere alcun contatto.

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
        '())))

Proviamo:

(overlay '((1 1) (4 4)) '((2 2) (5 5)))
;-> ((2 2) (4 4))

(overlay '((2 2) (4 4)) '((3 3) (4 5)))
;-> ((3 3) (4 4))

(overlay '((1 1) (4 4)) '((4 1) (6 4)))
;-> ()


------------------------------------------------------------------------
Posizione migliore per un centro di servizi (Gradient descent algorithm)
------------------------------------------------------------------------

Un'azienda vuole costruire un nuovo centro di servizi in città.
L'azienda conosce le posizioni di tutti i clienti della città (coordinate 2D) e vuole costruire il nuovo centro in una posizione tale che la somma delle distanze euclidee da tutti i clienti sia minima.

Data una lista di posizioni in cui lista[i] = (xi, yi) è la posizione dell'i-esimo cliente, restituire la posizione del nuovo centro per cui è minima la somma delle distanze euclidee da tutti i clienti.

In altre parole, bisogna determinare la posizione del centro di servizi (xc, yc) tale che la seguente formula sia minimizzata:

  Distanza = Sum[i=1,n](sqrt((xi - xc)^2) + (yi - yc)^2)

Per risolvere il problema usiamo la tecnica della "discesa del gradiente" (gradient descent).
Si tratta di un algoritmo di ottimizzazione iterativo per trovare il minimo di una funzione.
In questo caso la funzione che vogliamo minimizzare è la somma delle distanze.
Partendo da una posizione iniziale e muovendoci iterativamente nella direzione opposta al gradiente (la direzione dell'aumento più ripido), possiamo trovare il minimo locale della funzione.
Possiamo usare questo metodo perchè in questo problema, il minimo locale è anche il minimo globale.
Come posizione iniziale possiamo scegliere il centroide dei punti, che rappresenta la media aritmetica di tutti i punti dati.
Mentre il centroide riduce al minimo la somma delle distanze euclidee al quadrato, non minimizza necessariamente la somma delle distanze euclidee, ma fornisce un buon punto di partenza.

La soluzione inizia con il centroide dei punti dati e poi ne regola ripetutamente la posizione calcolando il gradiente (la direzione e la velocità dell'aumento più rapido della somma delle distanze) e spostandosi di un piccolo passo nella direzione opposta.
Questo processo viene ripetuto finché il cambiamento nella posizione del punto non diventa inferiore ad una data soglia, indicando che la somma minima delle distanze è stata raggiunta (approssimativamente).
Durante l'avvicinamento al minimo i passi di spostamento diminuiscono per rendere la soluzione più precisa.

(define (centroide points)
  (local (sum-x sum-y num-points out)
    (setq num-points (length points))
    (setq sum-x 0)
    (setq sum-y 0)
    (dolist (p points)
      (setq sum-x (add sum-x (p 0)))
      (setq sum-y (add sum-y (p 1)))
    )
    (list (div sum-x num-points)
          (div sum-y num-points))))

(setq pts '((1 2) (3 4) (5 6)))
(centroide pts)
;-> (3 4)

(define (solve pts)
  (local (centroid point-x point-y decay eps shift-rate
          continua grad-x grad-y total-dist diff-x diff-y
          dist delta-x delta-y)
    ; calcola il centroide dei punti (punto iniziale)
    (setq centroid (centroide pts))
    (setq point-x (centroid 0))
    (setq point-y (centroid 1))
    ; Parametri dell'algoritmo "Discesa del gradiente"
    (setq eps 1e-6)
    (setq shift-rate 0.5)
    (setq decay 0.999)
    ; Algoritmo "Discesa del gradiente" per minimizzare la somma delle distanze
    (setq continua true)
    (while continua
      ; inizializza gradienti e la distanza totale
      (setq grad-x 0.0)
      (setq grad-y 0.0)
      (setq total-dist 0.0)
      ; calcola i gradienti e la somma delle distanze di tutti i punti
      (dolist (p pts)
        (setq diff-x (sub point-x (p 0)))
        (setq diff-y (sub point-y (p 1)))
        (setq dist (sqrt (add (mul diff-x diff-x) (mul diff-y diff-y))))
        ;(setq dist (add (mul diff-x diff-x) (mul diff-y diff-y)))
        ; evita la divisione per 0
        (if (< dist 1e-8) (setq dist 1e-8))
        (setq grad-x (add grad-x (div diff-x dist)))
        (setq grad-y (add grad-y (div diff-y dist)))
        (setq total-dist (add total-dist dist))
      )
      ; calcola la lunghezza dello spostamento corrente
      (setq delta-x (mul grad-x shift-rate))
      (setq delta-y (mul grad-y shift-rate))
      ; aggiorna le coordinate del punto con il gradiente (direzione opposta)
      (setq point-x (sub point-x delta-x))
      (setq point-y (sub point-y delta-y))
      ; diminuisce il tasso di spostamento ad ogni iterazione
      (setq shift-rate (mul shift-rate decay))
      # controllo della convergenza (soluzione)
      (when (and (<= (abs delta-x) eps) (<= (abs delta-y) eps))
        (setq continua nil))
    )
    (list total-dist point-x point-y)))

Proviamo:

(setq pts '((1 2) (3 4) (5 6)))
(solve pts)
;-> (5.656854259492381 3 4)

(setq pts '((0 0) (4 0) (0 4) (4 4)))
(solve pts)
;-> (11.31370849898476 2 2)

(setq pts '((1 2) (3 4) (5 6) (7 4) (5 2)))
(solve pts)
;-> (11.98303134343762 4.091519582026579 3.657006989118677)

Vediamo la velocità della funzione:

Diecimila punti (1e4)
(silent
  (setq px (rand 10000 1e4))
  (setq py (rand 10000 1e4))
  (setq pts (map list px py)))
(time (println (solve pts)))
;-> (38002963.16131432 4944.260552594469 4983.856553035509)
;-> 38.914

Centomila punti (1e5)
(silent
  (setq px (rand 10000 1e5))
  (setq py (rand 10000 1e5))
  (setq pts (map list px py)))
(time (println (solve pts)))
;-> (383231163.9428849 5002.541041950339 4992.217882055365)
;-> 69744.413

Un milione di punti (1e6)
(silent
  (setq px (rand 10000 1e6))
  (setq py (rand 10000 1e6))
  (setq pts (map list px py)))
(time (println (solve pts)))
;-> (3824815946.535104 4998.716331585972 5006.518203239871)
;-> 1750068.13


---------------------------------------------
Il coniglio e la carota (punto raggiungibile)
---------------------------------------------

Un coniglio si trova nella cella (1, 1) di una griglia infinita.
Nella cella (xc, yc) della griglia si trova una carota.
Ad ogni passo, dalla posizione corrente (x, y) il coniglio può muoversi in uno dei modi seguenti:

1) (x, y - x)
2) (x - y, y)
3) (2*x, y)
4) (x, 2*y)

Date le coordinate intere (xc, yc) della carota, determinare se il coniglio (partendo da (1, 1)), può raggiungere la carota in un numero finito di passi.

Nota: la griglia non ammette valori negativi, (0 <= x <= Infinito) e (0 <= y <= Infinito).

Per risolvere il problema bisogna notare che:

a) muoversi da (1, 1) a (xc, xc) è equivalente a muoversi da (xc, yc) a (1, 1), cioè possiamo risolvere il problema anche immaginando che la carota si muove verso il coniglio.

b) poichè la griglia non ha numeri negativi i movimenti 1) e 2) potrebbero determinare degli spostamenti impossibili (celle con coordinate negative).

Quindi, sulla base di a) e b), possiamo dedurre che affinchè il coniglio raggiunga la carota (o viceversa), occorre che xc e yc abbiano un fattore in comune che è potenza di 2 (questo perchè usiamo solo le regole 3) e 4) per spostarsi).

ALlora la soluzione è calcolare il massimo comune divisore (MCD) di xc e yc.
Se questo MCD è una potenza di 2, signfica che possiamo ridimensionare entrambe le coordinate dividendo per il MCD e alla fine raggiungere (1, 1) senza incontrare problemi con i numeri negativi.

(define (power-of-2? num)
"Checks if an integer is a power of 2"
  (zero? (& num (- num 1))))

(define (coniglio xc yc) (power-of-2? (gcd xc yc)))

Proviamo:

(coniglio 7 14)
;-> nil
(coniglio 3 5)
;-> true
(coniglio 1 1)
;-> true
(coniglio 1001 1001)
;-> nil
(coniglio 1024 1024)
;-> true


---------------------
AlgoMonster FlowChart
---------------------

Il diagramma di flusso AlgoMonster, sviluppato da ex-Googler e programmatori competitivi, si basa sulla risoluzione di migliaia di problemi e sull'identificazione di pattern comuni.
Il suo scopo è quello di fornire un metodo strutturato per risolvere problemi di programmazione.

https://algo.monster/flowchart
https://youtu.be/s5gWz9Fa1yo
Vedi immagine "flow-chart" nella cartella "data".


------------------------
Propagazione di un virus
------------------------

In una griglia MxN ogni cella ha uno dei seguenti valori:

  0: rappresenta una cella vuota.
  1: rappresenta una cella che contiene una cellula sana.
  2: rappresenta una cella che contiene una cellula contaminata.

Ad ogni minuto ogni cellula contaminata trasmette il virus alle celle adiacenti nelle 4-direzioni (Nord, Est, Sud, Ovest).

Determinare la situazione della griglia al termine della propagazione del virus.

Esempio 1:
 minuto:    0         1         2         3         4
          2 1 1     2 2 1     2 2 2     2 2 2     2 2 2
griglia:  1 1 0 --> 2 1 0 --> 2 2 0 --> 2 2 0 --> 2 2 0
          0 1 1     0 1 1     0 1 1     0 2 1     0 2 2

Esempio 2:
 minuto:    0         1         2         3         4
          2 1 1     2 2 1     2 2 2     2 2 2     2 2 2
griglia:  1 1 0 --> 2 1 0 --> 2 2 0 --> 2 2 0 --> 2 2 0
          0 0 1     0 0 1     0 0 1     0 0 1     0 0 1

Funzione che stampa la griglia:

(define (print-matrix matrix ch0 ch1)
  (local (row col)
    (setq row (length matrix))
    (setq col (length (matrix 0)))
    (for (i 0 (- row 1))
      (for (j 0 (- col 1))
        (print (format "%3d" (matrix i j)))
      )
      (println)) '>))

Funzione che prende le coordinate di una cella della griglia e restituisce la lista delle celle contaminate dalla cella data:

(define (contamina i j)
  (local (virus Ni Nj Ei Ej Si Sj Oi Oj)
    (setq virus '())
    (when (= (grid i j) 2) ; se la cella corrente è contaminata
      (setq Ni (- i 1)) (setq Nj j)
      (setq Ei i)      (setq Ej (+ j 1))
      (setq Si (+ i 1)) (setq Sj j)
      (setq Oi i)      (setq Oj (- j 1))
      (if (and (>= Ni 0) (< Ni rows) (>= Nj 0) (< Nj cols))
        (if (= (grid Ni Nj) 1) (push (list Ni Nj) virus)))
      (if (and (>= Ei 0) (< Ei rows) (>= Ej 0) (< Ej cols))
        (if (= (grid Ei Ej) 1) (push (list Ei Ej) virus)))
      (if (and (>= Si 0) (< Si rows) (>= Sj 0) (< Sj cols))
        (if (= (grid Si Sj) 1) (push (list Si Sj) virus)))
      (if (and (>= Oi 0) (< Oi rows) (>= Oj 0) (< Oj virus))
        (if (= (grid Oi Oj) 1) (push (list Oi Oj) virus))))
    virus))

Funzione che simula la propagazione del virus:

(define (virus-spread grid verbose)
  (local (rows cols minuti contaminazioni total-virus conta)
    (setq rows (length grid))
    (setq cols (length (grid 0)))
    (setq minuti 0)
    (setq contaminazioni true)
    ; ciclo affinchè esistono contaminazioni
    (while contaminazioni
      ; lista delle celle contaminate ad ogni ciclo
      (setq total-virus '())
      (for (r 0 (- rows 1))
        (for (c 0 (- cols 1))
          ; aggiorna la lista delle celle contaminate per il ciclo corrente
          (extend total-virus (contamina r c))))
      (cond ((= total-virus '())
               ; nessuna cella contaminata nel ciclo corrente
               ; stop ciclo
              (setq contaminazioni nil))
            (true
              ; aggiorna la griglia con le nuove celle contaminate
              (dolist (v total-virus) (setf (grid (v 0) (v 1)) 2))
              (++ minuti)))
      ; (print-matrix grid) (read-line)
    )
    ; conta la frequenza delle tipologie delle celle (0, 1 e 2)
    (setq conta (count '(0 1 2) (flat (array-list grid))))
    ; stampa risultati (verbose = true)
    (when verbose
      (println "Tempo: " minuti " minuti")
      (println "Celle vuote: " (conta 0))
      (println "Celle sane: " (conta 1))
      (println "Celle contaminate: " (conta 2))
      (print-matrix grid))
    conta))

Proviamo:

(setq g (array 3 3 '(2 1 1 1 1 0 0 1 1)))
(virus-spread g true)
;-> Tempo: 4 minuti
;-> Celle vuote: 2
;-> Celle sane: 0
;-> Celle contaminate: 7
;->   2  2  2
;->   2  2  0
;->   0  2  2
;-> (2 0 7)

(setq g (array 3 3 '(2 1 1 1 1 0 0 0 1)))
(virus-spread g true)
;-> Tempo: 2 minuti
;-> Celle vuote: 3
;-> Celle sane: 1
;-> Celle contaminate: 5
;->   2  2  2
;->   2  2  0
;->   0  0  1
;-> (3 1 5)

Vediamo la velocità della funzione O(M*N*minuti):

(setq righe 10)
(setq colonne 10)
(setq test (array righe colonne (rand 3 (* righe colonne))))
(time (println (virus-spread test)))
;-> (28 0 72)
;-> 0

(setq righe 100)
(setq colonne 100)
(setq test (array righe colonne (rand 3 (* righe colonne))))
(time (println (virus-spread test)))
;-> (3319 49 6632)
;-> 124.934

(setq righe 200)
(setq colonne 200)
(silent (setq test (array righe colonne (rand 3 (* righe colonne)))))
(time (println (virus-spread test)))
;-> (13419 197 26384)
;-> 1436.707

(setq righe 400)
(setq colonne 400)
(silent (setq test (array righe colonne (rand 3 (* righe colonne)))))
(time (println (virus-spread test)))
;-> (53104 770 106126)
;-> 22720.052

(setq righe 1000)
(setq colonne 1000)
(silent (setq test (array righe colonne (rand 3 (* righe colonne)))))
(time (println (virus-spread test)))
;-> (333013 5112 661875)
;-> 2257352.978


----------------------------
Somma 0 di tre numeri (3Sum)
----------------------------

Data una lista di interi, restituire tutte le terne lista[i], lista[j], lista[k] tali che i!=j, i!=k, e j!=k, e lista[i] + lista[j] + lista[k] = 0.
La soluzione non deve contenere terne duplicate.

Algoritmo
1) Ordinamento della lista in modo crescente
2) Ciclo sulla lista ordinata e utilizzo della tecnica dei due puntatori per cercare le terne per ogni numero

(define (somma0 lst)
  (local (len terne stop sx dx somma)
    ; Lunghezza della lista (numeri della lista)
    (setq len (length lst))
    ; Ordina la lista
    (sort lst)
    ; Lista delle terne
    (setq terne '())
    ; Condizione di fermata del ciclo
    (setq stop nil)
    ; Ciclo per ogni numero della lista...
    (for (i 0 (- len 3) 1 stop)
      (cond
        ; Se il numero corrente è maggiore di 0,
        ; allora nessuna terna ha somma 0
        ; (questo perchè la lista è ordinata in modo crescente)
        ((> (lst i) 0) (setq stop true))
        ; Salta gli elementi uguali (per evitare terne duplicate)
        ((and (> i 0) (= (lst i) (lst (- i 1)))) nil)
        (true ; Tecnica dei due puntatori
          ; Imposta i due puntatori:
          ; sx (sinistra) dopo l'elemento corrente
          ; dx (destra) alla fine della lista
          (setq sx (+ i 1))
          (setq dx (- len 1))
          ; Affinchè sinistra è minore di destra...
          (while (< sx dx)
            ; Calcola la somma dei tre elementi correnti
            (setq somma (+ (lst i) (lst sx) (lst dx)))
            (cond
              ; Se la somma è minore di 0, allora sposta sx verso destra
              ((< somma 0) (++ sx))
              ; Se la somma è maggiore di 0, allora sposta dx verso sinistra
              ((> somma 0) (-- dx))
              ; Se la somma vale 0, allora abbiamo trovato una terna valida
              (true
                ; Aggiunge la terna corrente al risultato
                (push (list (lst i) (lst sx) (lst dx)) terne -1)
                ; Sposta i puntatori sx verso destra e dx verso sinistra
                (++ sx)
                (-- dx)
                ; Salta gli elementi duplicati spostando sx a destra
                (while (and (< sx dx) (= (lst sx) (lst (- sx 1))))
                  (++ sx))
                ; Salta gli elementi duplicati spostando dx a sinistra
                (while (and (< sx dx) (= (lst dx) (lst (+ dx 1))))
                  (-- dx))
              );true
            );cond
          );while
        );true
      );cond
    );for
    terne))

Proviamo:

(somma0 '(-1 0 1 2 -1 -4))
;-> ((-1 -1 2) (-1 0 1))
(somma0 '(0 1 1))
;-> ()
(somma0 '(0 0 0))
;-> ((0 0 0))
(setq nums '(5 -7 -9 -9 -3 -9 4 -2 -8 -5 0 1 7 2 -2 8 4 1 -8 4 -7 -4 -9 -7
             7 0 0 -2 -3 2 -6 -9 8 -3 0 -3 6 -5 0 -4 1 -5 4 -6 4 3 4 8 8
             -9 -6 0 -4 2 -8 4 9 0 3 2 3 9 8 9 8 4 7 -4 -4 8 5 3 5 -6 -1
             0 -9 -2 5 5 8 9 9 4 0 9 5 9 -8 -7 6 -3 -7 5 4 -7 -1 0 8 -9 -7
             -8 -5 -8 6 -2 5 3 -8 2 -5 -8 -7 7 5 -2 5 9 5 -1 -1 -6 4 4 0
             7 -2 -6 -4 8 2 7 2 2 -8 8 1 -1 -3 2 -9 -9 4 9 6 -6 2 0 -8 -3
             6 -6 0 6 0 3 4 3 0 -5 -1 5 -2 -2 0 -1 -8 -5 -7 9 8 3 -5 -6 -1
             3 -2 -5 -3 -8 -7 0 -1 -8 2 -9 2 0 -8 -7 3 0 0 9 2 6 -8 -7 -6 9))
(somma0 nums)
;-> ((-9 0 9) (-9 1 8) (-9 2 7) (-9 3 6) (-9 4 5) (-8 -1 9) (-8 0 8) (-8 1 7)
;->  (-8 2 6) (-8 3 5) (-8 4 4) (-7 -2 9) (-7 -1 8) (-7 0 7) (-7 1 6) (-7 2 5)
;->  (-7 3 4) (-6 -3 9) (-6 -2 8) (-6 -1 7) (-6 0 6) (-6 1 5) (-6 2 4) (-6 3 3)
;->  (-5 -4 9) (-5 -3 8) (-5 -2 7) (-5 -1 6) (-5 0 5) (-5 1 4) (-5 2 3)
;->  (-4 -4 8) (-4 -3 7) (-4 -2 6) (-4 -1 5) (-4 0 4) (-4 1 3) (-4 2 2)
;->  (-3 -3 6) (-3 -2 5) (-3 -1 4) (-3 0 3) (-3 1 2) (-2 -2 4) (-2 -1 3)
;->  (-2 0 2) (-2 1 1) (-1 -1 2) (-1 0 1) (0 0 0))


--------------------------------
Rettangoli da una lista di punti
--------------------------------

Data una lista di punti 2D (xi, yi), determinare tutti i rettangoli che possono essere formati con i punti (come vertici).
Ogni punto può essere utilizzato per formare più di un rettangolo.
I rettangoli hanno i lati paralleli all'asse X o all'asse Y.

Algoritmo brute-force O(N^4)
----------------------------
Quattro cicli che verificano tutte le combinazioni di punti O(N^4)

(define (rettangoli1 punti)
  (let (rectangle '())
    (dolist (p1 punti)
      (dolist (p2 punti)
        (when (and (not (= p1 p2)) ; I punti devono essere diversi
                   (= (p1 0) (p2 0))) ; Stessa X
          (dolist (p3 punti)
            (when (and (not (= p3 p1))
                       (not (= p3 p2))
                       (= (p3 1) (p1 1))) ; Stessa Y del primo punto
              (dolist (p4 punti)
                (when (and (not (= p4 p1))
                           (not (= p4 p2))
                           (not (= p4 p3))
                           (= (p4 0) (p3 0)) ; Stessa X del terzo punto
                           (= (p4 1) (p2 1))) ; Stessa Y del secondo punto
                  (push (list p1 p2 p3 p4) rectangle))))))))
    ; Elimina i rettangoli duplicati
    (unique (map sort rectangle))))

(setq punti '((1 1) (1 3) (3 1) (3 3) (2 2) (4 2)))
(rettangoli1 punti)
;-> (((1 1) (1 3) (3 1) (3 3)))

Algoritmo brute-force O(<N^4)
-----------------------------
Due cicli p1 e p2 O(N^2)
  Ricerca di p3 e p4 nella lista dei punti O(<N^2)

  p1                  p3
    +----------------+
    |                |
    |                |
    |                |
    +----------------+
  p4                  p2

  p1 = (x1 y1)
  p2 = (x2 y2)
  p3 = (x2 y1)
  p4 = (x1 y2)

(define (rettangoli2 punti)
  (local (rectangle x1 x2)
    (setq rectangle '())
    (dolist (p1 punti)
      (dolist (p2 punti)
        (setq x1 (p1 0)) (setq y1 (p1 1))
        (setq x2 (p2 0)) (setq y2 (p2 1))
        (when (and (> x1 x2) (> y1 y2)) ; evita di considerare lo stesso punto
          ; ricerca di p3 e p4 nella lista di punti
          (if (and (ref (list x1 y2) punti) (ref (list x2 y1) punti))
              (push (list p1 p2 (list x1 y2) (list x2 y1)) rectangle)))))
  ; Elimina i rettangoli duplicati
  (unique (map sort rectangle))))

(setq punti '((1 1) (1 3) (3 1) (3 3) (2 2) (4 2)))
(rettangoli2 punti)
;-> (((1 1) (1 3) (3 1) (3 3)))

In Lisp, l'accesso a un elemento di una lista ha una complessità temporale di O(n) nel caso peggiore.
Questo perché le liste in Lisp sono implementate come liste concatenate (linked list), dove ogni elemento contiene un riferimento al successivo.
Per accedere a un elemento specifico, bisogna attraversare la lista a partire dall'inizio fino a raggiungere la posizione desiderata. Di conseguenza:
- Se vogliamo accedere al primo elemento (head), l'operazione è O(1).
- Se vogliamo accedere a un elemento in posizione k, l'operazione richiede il passaggio attraverso i k elementi precedenti, risultando in O(k).
Per un accesso più veloce possiamo considerare l'uso di altre strutture dati come:
1. Array: In Lisp, un array (o vettore) consente l'accesso in O(1) per qualsiasi posizione.
2. Hash Table: Se dobbiamo accedere ai dati usando chiavi specifiche, le tabelle hash offrono una complessità media di O(1) per lettura e scrittura.

Algoritmo hash-map O(N^2)
-------------------------
a) hash-map di tutti i punti
b) doppio ciclo p1 e p2 O(N^4)
      cerchiamo p3 e p4 nella hash-map O(1)

(define (rettangoli3 punti)
  (new Tree 'pts)
  (local (rectangles x1 x2)
    (setq rectangle '())
    (dolist (p punti) (pts (string p) p))
    (dolist (p1 punti)
      (dolist (p2 punti)
        (setq x1 (p1 0)) (setq y1 (p1 1))
        (setq x2 (p2 0)) (setq y2 (p2 1))
        (when (and (> x1 x2) (> y1 y2)) ; evita di considerare lo stesso punto
          ; ricerca di p3 e p4 nella hash-map
          (if (and (pts (string (list x1 y2))) (pts (string (list x2 y1))))
              (push (list p1 p2 (list x1 y2) (list x2 y1)) rectangle)))))
    (delete 'pts) ; elimina la hash-map
    ; Elimina i rettangoli duplicati
    (unique (map sort rectangle))))

(setq punti '((1 1) (1 3) (3 1) (3 3) (2 2) (4 2)))
(rettangoli3 punti)
;-> (((1 1) (1 3) (3 1) (3 3)))

(setq test '((1 1) (1 3) (3 1) (3 3) (2 2) (4 2) (2 4) (4 4) (1 5) (5 1) (5 5)))
(rettangoli1 test)
;-> (((1 1) (1 5) (5 1) (5 5))
;->  ((2 2) (2 4) (4 2) (4 4))
;->  ((1 1) (1 3) (3 1) (3 3)))

(rettangoli2 test)
;-> (((1 1) (1 5) (5 1) (5 5))
;->  ((2 2) (2 4) (4 2) (4 4))
;->  ((1 1) (1 3) (3 1) (3 3)))

(rettangoli3 test)
;-> (((1 1) (1 5) (5 1) (5 5))
;->  ((2 2) (2 4) (4 2) (4 4))
;->  ((1 1) (1 3) (3 1) (3 3)))

Vediamo la velocità delle funzioni:

(setq points (map list (rand 10 100) (rand 10 100)))

(= (length (rettangoli1 points))
   (length (rettangoli2 points))
   (length (rettangoli3 points)))
;-> true

(time (rettangoli1 points))
;-> 225.078
(time (rettangoli2 points))
;-> 10.047
(time (rettangoli3 points))
;-> 24.55

(setq points (map list (rand 10 250) (rand 10 250)))

(time (rettangoli1 points))
;-> 8355.163
(time (rettangoli2 points))
;-> 114.938
(time (rettangoli3 points))
;-> 89.921

(setq points (map list (rand 10 1000) (rand 10 1000)))
;(time (rettangoli1 points))
(time (rettangoli2 points))
;-> 2322.849
(time (rettangoli3 points))
;-> 1352.475


-------------------------------
Triangoli da una lista di punti
-------------------------------

Data una lista di punti 2D (xi, yi), determinare tutti i triangoli che possono essere formati con i punti (come vertici).
Ogni punto può essere utilizzato per formare più di un triangolo.

Metodo 1
--------

Funzione che genera tutti i triangoli da una lista di punti:

(define (triangoli1 punti)
  (let ((triangle '()))
    (dolist (p1 punti)
      (dolist (p2 punti)
        (when (not (= p1 p2)) ; I punti devono essere diversi
          (dolist (p3 punti)
            (when (and (not (= p3 p1))
                       (not (= p3 p2)) ; Tutti i punti devono essere distinti
                       ; Calcolo determinante per il controllo di collinearità
                       ; I punti devono essere  non allineati
                       (not (= (* (- (p2 0) (p1 0)) (- (p3 1) (p1 1)))
                               (* (- (p3 0) (p1 0)) (- (p2 1) (p1 1))))))
              (push (list p1 p2 p3) triangle))))))
    (unique (map sort triangle))))

(setq punti '((1 1) (1 3) (3 1) (3 3) (2 2)))
(triangoli1 punti)
;-> (((2 2) (3 1) (3 3)) ((1 3) (2 2) (3 3)) ((1 1) (2 2) (3 1))
;->  ((1 1) (1 3) (2 2)) ((1 3) (3 1) (3 3)) ((1 1) (3 1) (3 3))
;->  ((1 1) (1 3) (3 3)) ((1 1) (1 3) (3 1)))

Metodo 2
--------

(define (comb k lst (r '()))
"Generates all combinations of k elements without repetition from a list of items"
  (if (= (length r) k)
    (list r)
    (let (rlst '())
      (dolist (x lst)
        (extend rlst (comb k ((+ 1 $idx) lst) (append r (list x)))))
      rlst)))

Con 100 punti abbiamo 161700 rettangoli:

(length (comb 3 (sequence 1 100)))
;-> 161700

Per calcolare l'area di un triangolo dato da tre punti usiamo la formula:

  Area = (1/2)*abs(x1*(y2 - y3) + x2*(y3 - y1) + x3*(y1 - y2))

(define (area p1 p2 p3)
  (div (abs (add (mul (p1 0) (sub (p2 1) (p3 1)))
                 (mul (p2 0) (sub (p3 1) (p1 1)))
                 (mul (p3 0) (sub (p1 1) (p2 1))))) 2))

Funzione che genera tutti i triangoli da una lista di punti:

(define (triangoli2 punti)
  (local (triangle pts aree)
    (setq tri '())
    ; genera tutte le combinazioni di tre punti (triangoli)
    (setq pts3 (comb 3 punti))
    ; calcola l'area di tutti i triangoli
    (setq aree (map (fn(p) (area (p 0) (p 1) (p 2))) pts3))
    ; seleziona da pts3 tutti i triangoli che hanno area maggiore di 0
    (setq triangle (select pts3 (index > aree)))
    ; elimina i triangoli duplicati
    (unique (map sort triangle))))

(setq punti '((0 0) (0 1) (1 0) (0 2) (2 0)))
(triangoli2 punti)
;-> (((1 1) (1 3) (3 1)) ((1 1) (1 3) (3 3)) ((1 1) (1 3) (2 2))
;->  ((1 1) (3 1) (3 3)) ((1 1) (2 2) (3 1)) ((1 3) (3 1) (3 3))
;->  ((1 3) (2 2) (3 3)) ((2 2) (3 1) (3 3)))

Metodo 3
--------

Funzione che genera tutti i triangoli da una lista di punti:

(define (triangoli3 punti)
  (setq triangle '())
  (setq len (length punti))
  (setq pts (array len punti))
  (for (i 0 (- len 3))
    (for (j (+ i 1) (- len 2))
      (for (k (+ j 1) (- len 1))
        (if (> (area (pts i) (pts j) (pts k)) 0)
          (push (list (pts i) (pts j) (pts k)) triangle)))))
  (unique (map sort triangle)))

(setq punti '((0 0) (0 1) (1 0) (0 2) (2 0)))
(triangoli3 punti)
;-> (((2 2) (3 1) (3 3)) ((1 3) (2 2) (3 3)) ((1 3) (3 1) (3 3))
;->  ((1 1) (2 2) (3 1)) ((1 1) (3 1) (3 3)) ((1 1) (1 3) (2 2))
;->  ((1 1) (1 3) (3 3)) ((1 1) (1 3) (3 1)))

Vediamo se le tre funzioni generano gli stessi risultati:

(setq test (map list (rand 100 10) (rand 100 10)))
;-> ((16 60) (6 75) (31 40) (78 34) (21 84)
;->  (32 94) (48 62) (0 99) (43 5) (50 56))
(length (triangoli1 test))
;-> 120
(length (triangoli2 test))
;-> 120
(length (triangoli3 test))
;-> 120
(setq t1 (triangoli1 test))
(setq t2 (triangoli2 test))
(setq t3 (triangoli3 test))
(difference t1 t2)
;-> ()
(difference t2 t1)
;-> ()
(difference t1 t3)
;-> ()
(difference t3 t1)
;-> ()
(difference t2 t3)
;-> ()
(difference t3 t2)
;-> ()

Vediamo la velocità delle tre funzioni:

(silent (setq test (map list (rand 100 100) (rand 100 100))))
(time (triangoli1 test))
;-> 3300.171
(time (triangoli2 test))
;-> 958.783
(time (triangoli3 test))
;-> 642.622

(silent (setq test (map list (rand 100 200) (rand 100 200))))
(time (triangoli2 test))
;-> 9412.194
(time (triangoli3 test))
;-> 6621.569


----------------------------------------------
Circonferenze di raggio R passanti per 2 punti
----------------------------------------------

Dati due punti 2D e un raggio R, determinare le equazioni delle circonferenze che passano per i punti dati.

Equazione circonferenza

  Xc^2 + Yc^2 = R^2

dove: Xc, Yc sono le coordinate del centro del cerchio
      R è il raggio del cerchio

Dati un raggio R e due punti P1(x1,y1), P2(x2,y), esistono due circonferenze passanti per i due punti.
La differenza tra i due cerchi è che hanno centri simmetrici rispetto al segmento che unisce i due punti.
Calcoliamo i centri e i raggi di entrambi i cerchi:

1. Corda del cerchio
Il segmento che unisce i due punti è la corda del cerchio.
I centri dei cerchi si trovano sulla perpendicolare della bisettrice di questo segmento.

2. Distanza tra i punti
La distanza tra P1 e P2 vale:

  d = sqrt((x2 - x1)^2 + (y2 - y1)^2)

3. Controllo esistenza delle circonferenze
Se R < d/2, allora i cerchi non esistono (non possono esistere cerchi con raggio R che contengono i due punti).
Se R = d/2, allora esiste un solo cerchio (la corda è il diametro del cerchio).
Se R > d/2, allora esistono due cerchi.

4. Centri dei due cerchi
Il punto medio del segmento tra i due punti è:

  Mx = (x1 + x2)/2, My = (y1 + y2)/2

La distanza h tra il punto medio e ciascun centro è data da Pitagora:

  h = sqrt(R^2 - (d/2)^2)

La direzione perpendicolare è data ruotando 90 gradi il vettore  (x2 - x1, y2 - y1), ottenendo:

  dx = - (y2 - y1), dy = (x2 - x1)

Per calcolare i due centri, ci spostiamo dal punto medio nella direzione perpendicolare di una quantità uguale al raggio del cerchio:

  C(1,2) = (Mx +- (h*dx)/d), (My +- (h*dy)/d)

Funzione che calcola i cerchi (xc, yc, r) passanti per due punti P1(x1,y1) e P2(x2,y2):

(define (cerchi-passanti p1 p2 r)
  (let ((x1 (p1 0))
        (y1 (p1 1))
        (x2 (p2 0))
        (y2 (p2 1)))
    (let ((d (sqrt (add (mul (sub x2 x1) (sub x2 x1))
                      (mul (sub y2 y1) (sub y2 y1))))))
      (if (< r (div d 2))
          nil ; Non esiste un cerchio
          (let ((mx (div (add x1 x2) 2))
                (my (div (add y1 y2) 2))
                (h (sqrt (sub (mul r r) (mul (div d 2) (div d 2))))))
            (let ((dx (div (sub y2 y1) d))
                  (dy (div (sub x1 x2) d)))
              ; cerchio (cx cy raggio)
              (let ((c1 (list (add mx (mul h dx)) (add my (mul h dy)) r))
                    (c2 (list (sub mx (mul h dx)) (sub my (mul h dy)) r)))
                (list c1 c2))))))))

Proviamo:

(cerchi-passanti '(1 1) '(4 1) 4)
;-> ((2.5 -2.708099243547832 4) (2.5 4.708099243547832 4))

(cerchi-passanti '(-2 0) '(2 0) 2)
;-> ((0 0 2) (0 0 2))

(cerchi-passanti '(-2 0) '(2 0) 1)
;-> nil

Vedi anche "Circonferenza passante per tre punti" su "Note libere 10".
Vedi anche "Circonferenza per tre punti" su "Note libere 25".


---------------------------
Punto interno ad un cerchio
---------------------------

Dato un punto P(x,y) nel piano cartesiano e un cerchio di centro C(cx,cy) e raggio R, determinare se il punto è interno al cerchio (anche sulla circonferenza).

La distanza tra il punto P e il centro C vale:

  d = sqrt((x - cx)^2 + (y - cy)^2)

Se la distanza d è minore o uguale al raggio, il punto P è interno al cerchio:
  se d <= R, allora P interno al cerchio

Se la distanza d è minore del raggio, il punto P è interno al cerchio:
  se d > R, allora P asterno al cerchio

Nella funzione usiamo d^2 e R^2 per velocizzare i calcoli.

Funzione con numeri come parametri:

(define (interno? x y cx cy r)
  (<= (add (mul (sub x cx) (sub x cx)) (mul (sub y cy) (sub y cy))) (mul r r)))

Funzione con liste come parametri:

(define (within? p c)
  (<= (add (mul (sub (p 0) (c 0)) (sub (p 0) (c 0)))
         (mul (sub (p 1) (c 1)) (sub (p 1) (c 1)))) (mul (c 2) (c 2))))

Proviamo:

(setq p1 '(2 2))
(setq c1 '(0 0 5))
(within? p1 c1)
;-> true
(interno? 2 2 0 0 5)
;-> true

(setq p2 '(0 5))
(within? p2 c1)
;-> true
(interno? 0 5 0 0 5)
;-> true

(setq p3 '(4 5))
(within? p3 c1)
;-> nil
(interno? 4 5 0 0 5)
;-> nil


--------------------------------------
Cerchio di massima inclusione di punti
--------------------------------------

Data una lista di N punti 2D non duplicati e un cerchio di raggio R, determinare la posizione del cerchio (centro) che contiene il maggior numero di punti.

Algoritmo
Per ogni coppia di punti:
  a) calcolare i due cerchi di raggio R che passano per la coppia di punti
  b) calcolare i punti che sono contenuti da questo cerchio
  c) tenere traccia del cerchio che contiene il maggior numero di punti

Funzione che verifica se un punto (x y) è interno ad un cerchio (xc yx r):

(define (within? p c)
  (<= (add (mul (sub (p 0) (c 0)) (sub (p 0) (c 0)))
           (mul (sub (p 1) (c 1)) (sub (p 1) (c 1)))) (mul (c 2) (c 2))))

Funzione che calcola i cerchi (xc, yc, r) passanti per due punti P1(x1,y1) e P2(x2,y2):

(define (cerchi-passanti p1 p2 r)
  (let ((x1 (p1 0))
        (y1 (p1 1))
        (x2 (p2 0))
        (y2 (p2 1)))
    (let ((d (sqrt (add (mul (sub x2 x1) (sub x2 x1))
                      (mul (sub y2 y1) (sub y2 y1))))))
      (if (< r (div d 2))
          nil ; Non esiste un cerchio
          (let ((mx (div (add x1 x2) 2))
                (my (div (add y1 y2) 2))
                (h (sqrt (sub (mul r r) (mul (div d 2) (div d 2))))))
            (let ((dx (div (sub y2 y1) d))
                  (dy (div (sub x1 x2) d)))
              (let ((c1 (list (add mx (mul h dx)) (add my (mul h dy)) r))
                    (c2 (list (sub mx (mul h dx)) (sub my (mul h dy)) r)))
                (list c1 c2))))))))

(cerchi-passanti '(1 1) '(4 1) 4)
;-> ((2.5 -2.708099243547832 4) (2.5 4.708099243547832 4))

(cerchi-passanti '(-2 0) '(2 0) 2)
;-> ((0 0 2) (0 0 2))

(cerchi-passanti '(-2 0) '(2 0) 1)
;-> nil

Funzione che restituisce una lista con tutte le possibili coppie tra gli elementi di una lista data:

(define (pair-list lst)
  (let (out '())
    (for (i 0 (- (length lst) 2))
      (for (j (+ i 1) (- (length lst) 1))
          (push (list (lst i) (lst j)) out -1)))))

Funzione che calcola il cerchio di raggio R che contiene il maggior numero di punti :

(define (cerchio-max punti r)
  (local (max-punti max-cerchio coppie cerchi curr1 curr2)
    ; Numero massimo di punti contenuti
    (setq max-punti 0)
    ; Cerchio che contiene il numero massimo di punti
    (setq max-cerchio '())
    ; Creazione di tutte le coppie di punti
    (setq coppie (pair-list punti))
    ; Ciclo per tutte le coppi di punti...
    (dolist (el coppie)
      ; Calcolo dei cerchi passanti per la coppia di punti corrente
      (setq cerchi (cerchi-passanti (el 0) (el 1) r))
      ; Se esistono i cerchi...
      (when cerchi
        ; punti contenuti nel cerchio 1
        (setq curr1 0)
        ; punti contenuti nel cerchio 2
        (setq curr2 0)
        ; Calcola i punti contenuti da ogni cerchio
        (dolist (p punti)
          (when (within? p (cerchi 0)) (++ curr1))
          (when (within? p (cerchi 1)) (++ curr2))
        )
        ; Aggiorna il valore massimo dei punti e il cerchio che li contiene
        (when (> curr1 max-punti)
          (setq max-punti curr1)
          (setq max-cerchio (cerchi 0)))
        (when (> curr2 max-punti)
          (setq max-punti curr2)
          (setq max-cerchio (cerchi 1)))
      )
    )
    (list max-punti max-cerchio)))

Proviamo:

(cerchio-max '((-2 0) (2 0) (0 2) (0 -2)) 2)
;-> (4 (0 0 2))

(cerchio-max '((-3 0) (3 0) (2 6) (5 4) (0 9) (7 8)) 5)
;-> (5 (0 4 5))

Vediamo la velocità dell'algoritmo:

(length (setq pts (unique (map list (rand 100 100) (rand 100 100)))))
;-> 99
(time (println (cerchio-max pts 20)))
;-> (20 (17.81892663155036 16.23771057941913 20))
;-> 160.622

(length (setq pts (unique (map list (rand 200 200) (rand 200 200)))))
;-> 200
(time (println (cerchio-max pts 20)))
;-> (33 (16.29097248200296 78.89132113301923 20))
;-> 414.049

(length (setq pts (unique (map list (rand 400 400) (rand 400 400)))))
;-> 400
(time (println (cerchio-max pts 20)))
;-> 996.741

Se i punti sono molto densi, allora la funzione ci mette più tempo (questo perchè esistono molti cerchi da verificare):

(length (setq pts (unique (map list (rand 100 400) (rand 100 400)))))
;-> 393
(time (println (cerchio-max pts 20)))
;-> (71 (60.12740473541479 60.8431488161463 20))
;-> 10302.174

Se il raggio è grande, allora la funzione ci mette più tempo (questo perchè esistono molti cerchi da verificare):

(length (setq pts (unique (map list (rand 400 400) (rand 400 400)))))
;-> 400
(time (println (cerchio-max pts 75)))
;-> (61 (291.8472970881422 241.2417370039397 75))
;-> 9642.548000000001

Con punti densi e raggio grande la funzione rallenta molto:

(length (setq pts (unique (map list (rand 100 400) (rand 100 400)))))
;-> 389
(time (println (cerchio-max pts 75)))
;-> (389 (40.37188186310436 50 75))
;-> 29441.91

Altro esempio:

  Punti: 100
  Intervallo: 0 <= xi,yi <= 10^4
  Raggio: 1 <= R <= 5000

(length (setq pts (unique (map list (rand 10000 100) (rand 10000 100)))))
;-> 100
(time (println (cerchio-max pts 5000)))
;-> (82 (5492.421418386987 5340.867601591941 5000))
;-> 488.995
(time (println (cerchio-max pts 1)))
;-> (0 ())
;-> 5.965

Vedi anche "Cerchio minimo di inclusione (Minimum Enclosing Circle)" su "Note libere 10".
Vedi anche "Circonferenza con copertura massima di una lista di punti interi" su "Note libere 15"


-----------------------------------------------------------
K punti più vicini all'origine (K closest points to origin)
-----------------------------------------------------------

Dato una lista di N punti 2D (xi, yi) e un intero k, restituere i k punti più vicini all'origine (0, 0).
La distanza tra due punti è la distanza euclidea.

Distanza euclidea di un punto dall'origine (0, 0):

  d = sqrt((x1 - x2)*(x1 - x2) + (x1 - x2)*(x1 - x2)) = sqrt(x1^2 + y1^2)

Poichè non ci interessano i valori delle distanze possiamo fare a meno di calcolare la radice quadrata.

Algoritmo
1) Ordinamento crescente dei punti in base alla loro distanza dal punto (0, 0)
2) Selezione dei primi K punti ordinati

Funzione di comparazione per l'ordinamento:

(define (compare p1 p2)
  (setq dist1 (add (mul (p1 0) (p1 0)) (mul (p1 1) (p1 1))))
  (setq dist2 (add (mul (p2 0) (p2 0)) (mul (p2 1) (p2 1))))
  (< dist1 dist2))

Funzione che trova i k punti più vicini all'origine:

(define (k-vicini punti k) (slice (sort punti compare) 0 k))

Proviamo:

(k-vicini '((1 3) (-2 2)) 1)
((-2 2))

(k-vicini '((3 3) (5 -1) (-2 4)) 2)
((3 3) (-2 4))

Vediamo la velocità della funzione:

1000 punti:
(silent (setq pts (map list (rand 100 1000) (rand 100 1000))))
(time (println (k-vicini pts 10)))
;-> ((0 0) (0 1) (2 3) (4 0) (5 4) (2 7) (8 0) (1 8) (7 5) (1 9))
;-> 7.979

10000 punti:
(silent (setq pts (map list (rand 100 10000) (rand 100 10000))))
(time (println (k-vicini pts 10)))
;-> ((1 0) (1 1) (1 1) (1 1) (2 1) (1 2) (2 2) (2 2) (3 0) (3 0))
;-> 101.67

100000 punti:
(silent (setq pts (map list (rand 100 100000) (rand 100 100000))))
(time (println (k-vicini pts 10)))
;-> ((0 0) (0 0) (0 0) (0 1) (1 0) (1 0) (0 1) (0 1) (1 0) (1 0))
;-> 1272.128

1000000 punti:
(silent (setq pts (map list (rand 100 1000000) (rand 100 1000000))))
(time (println (k-vicini pts 10)))
;-> ((0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0))
;-> 15514.087

Vedi anche "K punti più vicini - K Nearest points (LinkedIn)" su "Domande".


-----------------------------------------------------
Lato più grande di un quadrato interno a N rettangoli
-----------------------------------------------------

Abbiamo N rettangoli in un piano 2D con lati paralleli agli assi x e y.
Trovare l'area massima di un quadrato che può essere inserita nella regione di intersezione di almeno due rettangoli.
Un rettangolo viene rappresentato dalla seguente lista: ((xb yb) (xt xt))
dove: (xb,yb) è la coordinata in basso a sinistra del rettangolo (bottom),
      (xt,yt) è la coordinata in alto a destra del rettangolo (top)

                     (xt,yt)
   +--------------------+
   |                    |
   |                    |
   |                    |
   |                    |
   +--------------------+
(xb,yb)

Algoritmo
Per ogni coppia di rettangoli...
  Calcolare il quadrato di sovrapposizione tra i due rettangoli
  Aggiornare il quadrato massimo

Funzione che calcola il lato più grande di un quadrato interno a N rettangoli

(define (quadrato-max lst)
  ; Numero di rettangoli
  (setq len (length lst))
  ; Valore massimo del lato del quadrato
  (setq max-side 0)
  ; Lista dei punti basso-sx dei rettangoli
  (setq basso-sx (map (fn(p) (p 0)) lst))
  ; Lista dei punti alto-dx dei rettangoli
  (setq alto-dx (map (fn(p) (p 1)) lst))
  ;(println basso-sx)
  ;(println alto-dx)
  (for (i 0 (- len 2))
    ; Coordinate basso-sx e alto-dx del primo rettangolo
    (setq ax1 (basso-sx i 0)) (setq ay1 (basso-sx i 1))
    (setq ax2 (alto-dx i 0))  (setq ay2 (alto-dx i 1))
    (for (j (+ i 1) (- len 1))
      ; Coordinate basso-sx e alto-dx del secondo rettangolo
      (setq bx1 (basso-sx j 0)) (setq by1 (basso-sx j 1))
      (setq bx2 (alto-dx j 0))  (setq by2 (alto-dx j 1))
      ; Calcola la larghezza e l'altezza del rettangolo di sovrapposizione
      (setq overlapX (sub (min ax2 bx2) (max ax1 bx1)))
      (setq overlapY (sub (min ay2 by2) (max ay1 by1)))
      ; Calcola il lato del quadrato
      ; valore minimo tra larghezza e altezza del rettangolo di sovrapposizione
      (setq min-side (min overlapX overlapY))
      ; Aggiorna il valore massimo del lato del quadrato
      (setq max-side (max max-side min-side))
      (println i { } j)
      (print overlapX { } overlapY { } max-side)
      (read-line)
    )
  )
  max-side)

Proviamo:

(quadrato-max '(((1 1) (3 3)) ((2 2) (4 4)) ((3 1) (6 6))))
;-> 1
(quadrato-max '(((1 1) (5 5)) ((1 3) (5 7)) ((1 5) (5 9))))
;-> 4
(quadrato-max '(((1 1) (2 2)) ((3 3) (4 4)) ((3 1) (4 2))))
;-> 0 ; nessuna sovrapposizione tra tutti i rettangoli


--------------------------------------------------------------------------
Numero di linee continue per rappresentare un grafico a linee (Line Chart)
--------------------------------------------------------------------------

Abbiamo una lista di coppie di interi che rappresenta l'andamento della Temperatura per N giorni.
Un grafico a linee viene creato dalla lista tracciando i punti su un piano XY con l'asse X che rappresenta il giorno e l'asse Y che rappresenta la temperatura e collegando i punti adiacenti.
Restituisce il numero minimo di linee necessarie per rappresentare il grafico a linee.
Vediamo un paio di esempi:
  N = 6
  lista = ((1 6) (2 8) (3 4) (4 2) (5 7))
  Grafico a Linee

  8  |   *
  7  |   2
  6  | *
  5  | 1           *
  4  |     *       5
  3  |     3
  2  |       *
  1  |       4
  0 -+---------------------------
     0 1 2 3 4 5 6 7

In questo caso occorrono 4 linee: 1-2, 2-3, 3-4 e 4-5.
  N = 6
  lista = ((1 6) (2 6) (3 4) (4 2) (5 7))
  Grafico a Linee

  8  |
  7  |         *
  6  | * *     5
  5  | 1 2
  4  |     *
  3  |     3
  2  |       *
  1  |       4
  0 -+---------------------------
     0 1 2 3 4 5 6 7

In questo caso occorrono 3 linee: 1-2, 2-4, e 4-5 (perchè i punti 2, 3 e 4 sono allineati).

Algoritmo
Ordinamento crescente della lista in base al giorno.
Per ogni coppia di punti adiacenti...
  Calcolare la pendenza della coppia di punti corrente per decidere se il segmento corrente può essere collegato a quello precedente o se occorre iniziare una nuova linea.
  Tenere traccia del numero di linee utilizzate.

Il numero di linee viene incrementato quando viene rilevato un cambiamento di pendenza tra i segmenti (in qual caso occorre aggiornare la pendenza corrente per la coppia di punti successiva).

Funzione che calcola la pendenza tra due punti:

(define (pendenza p1 p2)
  (setq dx (sub (p1 0) (p2 0)))
  (setq dy (sub (p1 1) (p2 1)))
  (cond ((zero? dx) (list 0 (p1 0)))
        ((zero? dy) (list (p1 1) 0))
        (true
          (setq mcd (gcd dx dy))
          (list (div dx mcd) (div dy mcd)))))

(pendenza '(2 6) '(3 4))
(pendenza '(3 4) '(4 2))

Funzione che calcola il numero di linee del Line Chart:

(define (linee punti)
  (local (len num-linee pend1 pend2)
    (setq len (length punti))
    (cond ((zero? len) 0)
          ((= len 1) 0)
          ((= len 2) 1)
          (true
            (setq num-linee 0)
            (sort punti)
            (for (i 2 (- len 1))
              (setq pend1 (pendenza (punti (- i 2)) (punti (- i 1))))
              (setq pend2 (pendenza (punti (- i 1)) (punti i)))
              (if (!= pend1 pend2) (++ num-linee))
            )
            (++ num-linee)))))

Proviamo:

(linee '((1 6) (2 8) (3 4) (4 2) (5 7)))
;-> 4
(linee '((1 6) (2 6) (3 4) (4 2) (5 7)))
;-> 3
(linee '((1 1) (2 2)))
;-> 1
(linee '((1 1) (2 2) (4 4)))
;-> 1
(linee '((1 1) (2 2) (4 4) (5 4) (6 4)))
;-> 2


--------------------
Simulare N cicli for
--------------------

Scrivere una funzione che prende due interi N e M restituisce una lista di elementi che rappresentano il valore degli indici di N cicli 'for' innestati i cui indici vanno da 0 a M.
In altre parole, partendo da un vettore lungo N con tutti 0, dobbiamo modificare gli elementi del vettore in modo che ad ogni passo rappresentino gli indici di N cicli for innestati.

Per esempio con N = 3, M = 2
indici =   i j k
vettore =  0 0 0  (passo 0)
           0 0 1  (passo 1)
           0 0 2  (passo 2)
           0 1 0  (passo 3)
           0 1 1  (passo 4)
           0 1 2  (passo 5)
           .....  (passo ...)
           2 2 1  (passo (M+1)^N - 2)
           2 2 2  (passo (M+1)^N - 1)

Il numero totale di passi (partendo da 0) vale (M+1)^N.

Metodo 1
--------

(define (for-indexes N M)
  (let ( (indici (dup 0 N)) (out '()) (continua true) )
    (while (and (<= (indici 0) M) continua)
      ; 'indici' contiene gli N indici correnti
      ;(println indici)
      (push indici out -1)
      (setf indici (aggiorna-indici indici N M))
      ; condizione di terminazione
      (if (for-all zero? indici) (setq continua nil)))
    out))

(define (aggiorna-indici indici N M)
  (let ( (i (- N 1)) (riporto true) )
    ; aggiorna gli indici utilizzando un riporto
    (while (and (>= i 0) riporto)
      (if (< (indici i) M)
          (begin
            (++ (indici i))
            (setf riporto nil))
          (setf (indici i) 0))
      (-- i))
    indici))

Proviamo:

(for-indexes 2 2)
;-> ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2))

(for-indexes 3 2)
;-> ((0 0 0) (0 0 1) (0 0 2) (0 1 0) (0 1 1) (0 1 2) (0 2 0) (0 2 1) (0 2 2)
;->  (1 0 0) (1 0 1) (1 0 2) (1 1 0) (1 1 1) (1 1 2) (1 2 0) (1 2 1) (1 2 2)
;->  (2 0 0) (2 0 1) (2 0 2) (2 1 0) (2 1 1) (2 1 2) (2 2 0) (2 2 1) (2 2 2))

(for-indexes 4 2)
;-> ((0 0 0 0) (0 0 0 1) (0 0 0 2) (0 0 1 0) (0 0 1 1) (0 0 1 2) (0 0 2 0)
;->  (0 0 2 1) (0 0 2 2) (0 1 0 0) (0 1 0 1) (0 1 0 2) (0 1 1 0) (0 1 1 1)
;->  (0 1 1 2) (0 1 2 0) (0 1 2 1) (0 1 2 2) (0 2 0 0) (0 2 0 1) (0 2 0 2)
;->  (0 2 1 0) (0 2 1 1) (0 2 1 2) (0 2 2 0) (0 2 2 1) (0 2 2 2) (1 0 0 0)
;->  (1 0 0 1) (1 0 0 2) (1 0 1 0) (1 0 1 1) (1 0 1 2) (1 0 2 0) (1 0 2 1)
;->  (1 0 2 2) (1 1 0 0) (1 1 0 1) (1 1 0 2) (1 1 1 0) (1 1 1 1) (1 1 1 2)
;->  (1 1 2 0) (1 1 2 1) (1 1 2 2) (1 2 0 0) (1 2 0 1) (1 2 0 2) (1 2 1 0)
;->  (1 2 1 1) (1 2 1 2) (1 2 2 0) (1 2 2 1) (1 2 2 2) (2 0 0 0) (2 0 0 1)
;->  (2 0 0 2) (2 0 1 0) (2 0 1 1) (2 0 1 2) (2 0 2 0) (2 0 2 1) (2 0 2 2)
;->  (2 1 0 0) (2 1 0 1) (2 1 0 2) (2 1 1 0) (2 1 1 1) (2 1 1 2) (2 1 2 0)
;->  (2 1 2 1) (2 1 2 2) (2 2 0 0) (2 2 0 1) (2 2 0 2) (2 2 1 0) (2 2 1 1)
;->  (2 2 1 2) (2 2 2 0) (2 2 2 1) (2 2 2 2))

(length (for-indexes 4 3))
;-> 256

Metodo 2
--------

(transpose (for-indexes 3 2))
;-> ((0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2)
;->  (0 0 0 1 1 1 2 2 2 0 0 0 1 1 1 2 2 2 0 0 0 1 1 1 2 2 2)
;->  (0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2))

Il metodo è quello di costruire la lista intera della trasposta.
Per esempio, nel caso sopra, la funzione si comporta nel modo seguente:
  Prima riga
    base = 9 (numeri di zeri)
    pattern = (0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2)
    riga = (0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2)
  Seconda riga
    base = 3 (numeri di zeri)
    pattern = (0 0 0 1 1 1 2 2 2)
    riga = (0 0 0 1 1 1 2 2 2 0 0 0 1 1 1 2 2 2 0 0 0 1 1 1 2 2 2)
  Terza riga
    base = 1 (numeri di zeri)
    pattern = (0 1 2)
    riga = (0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2)

(define (genera-indici N M)
  (local (out elementi base pattern riga)
    (setq out '())
    ; numero di elementi
    (setq elementi (/ (pow (+ M 1) N)))
    ; lunghezza della prima ripetizione della prima riga
    ; (numero di zeri della prima riga)
    (setq base (/ elementi (+ M 1)))
    (while (> base 0)
      ; creazione del pattern da ripetere
      (setq pattern (flat (map (fn(x) (dup x base)) (sequence 0 M))))
      ; creazione della riga
      (setq riga (flat (dup pattern (/ elementi (length pattern)))))
      ; inserimento della riga nella soluzione
      (push riga out -1)
      ; aggiorna 'base' per la prossima riga
      (setq base (/ base (+ M 1)))
    )
    ; trasposta della soluzione
    (transpose out)))

(genera-indici 2 2)
;-> ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2))

(genera-indici 3 2)
;-> ((0 0 0) (0 0 1) (0 0 2) (0 1 0) (0 1 1) (0 1 2) (0 2 0) (0 2 1) (0 2 2)
;->  (1 0 0) (1 0 1) (1 0 2) (1 1 0) (1 1 1) (1 1 2) (1 2 0) (1 2 1) (1 2 2)
;->  (2 0 0) (2 0 1) (2 0 2) (2 1 0) (2 1 1) (2 1 2) (2 2 0) (2 2 1) (2 2 2))

Vediamo se le due funzioni generano gli stessi risultati:

(= (genera-indici 4 2) (for-indexes 4 2))
;-> true

Vediamo la velocità delle funzioni:

(time (for-indexes 5 20))
;-> 3564.437
(time (genera-indici 5 20))
;-> 880.671

(time (for-indexes 3 100))
;-> 683.199
(time (genera-indici 3 100))
;-> 226.394

La seconda funzione è più veloce (anche se entrambe sono piuttosto lente).

Possiamo generalizzare il primo metodo in modo che ogni ciclo for abbia un proprio indice di inizio e di fine.

(define (for-indexes limiti)
  (letn ((N (length limiti))
         (indici (map (fn (lim) (lim 0)) limiti)) ; Inizializza con i valori di partenza
         (out '())
         (continua true))
    (setq base indici)
    (while continua
      (push indici out -1)
      (setq indici (aggiorna-indici indici limiti N))
      (if (= indici base) (setq continua nil))
    )
    out))

(define (aggiorna-indici indici limiti N)
  (let ((i (- N 1))
        (riporto true))
    (while (and (>= i 0) riporto)
      (if (< (indici i) ((limiti i) 1))
          (begin
            (++ (indici i))
            (setq riporto nil))
          (setq (indici i) ((limiti i) 0)))
      (-- i)
    )
    indici))

Proviamo:

(for-indexes '((2 4) (2 6)))
;-> ((2 2) (2 3) (2 4) (2 5) (2 6) (3 2) (3 3) (3 4)
;->  (3 5) (3 6) (4 2) (4 3) (4 4) (4 5) (4 6))

(= (for-indexes '((0 3) (0 3))) (genera-indici 2 3))
;-> true

(= (for-indexes '((0 3) (0 3) (0 3))) (genera-indici 3 3))
;-> true

Vediamo la differenza di velocità con i cicli "for":

(define (for3 i1 i2 j1 j2 k1 k2)
  (let (out '())
    (for (i i1 i2)
      (for (j j1 j2)
        (for (k k1 k2)
          (push (list i j k) out -1))))))

(= (for3 1 2 3 4 5 6) (for-indexes '((1 2) (3 4) (5 6))))
;-> true

(time (for3 1 100 1 100 1 100))
;-> 171.828
(time (for-indexes '((1 100) (1 100) (1 100))))
;-> 640.905
(div 640.905 171.828)
;-> 3.729921782247363

(time (for3 1 200 1 200 1 200))
;-> 890.903
(time (for-indexes '((1 200) (1 200) (1 200))))
;-> 5063.102
(div 5063.102 890.903)
;-> 5.683112527401973

(time (for3 1 400 1 400 1 400))
;-> 10157.162
(time (for-indexes '((1 400) (1 400) (1 400))))
;-> 41050.7
(div 41050.7 10157.162)
;-> 4.041552157974835

Vedi anche "Cicli variabili" su "Note libere 28".


--------------------------
Somma X di N numeri (NSum)
--------------------------

Data una lista di interi e un intero N, restituire tutte le N-ple (lista[a], lista[b], ..., lista[N]) tali che gli elementi hanno tutti indici diversi tra loro e (lista[a] + lista[b] + ... + lista[N]) = X.

Se N = 4, possiamo scrivere:

Quadrupla: (lista[a], lista[b], lista[c], lista[d])
Somma: lista[a] + lista[b] + lista[c] + lista[d] = X

(define (quadruple lista target)
  (let ((len (length lista))
        (out '()))
    (for (a 0 (- len 4))
      (for (b (+ a 1) (- len 3))
        (for (c (+ b 1) (- len 2))
          (for (d (+ c 1) (- len 1))
            (when (= (+ (lista a) (lista b) (lista c) (lista d)) target)
                (push (list (lista a) (lista b) (lista c) (lista d)) out))))))
    out))

Proviamo:

(setq lista '(1 -1 2 -2 0 1))
(quadruple lista 3)
;-> ((1 -1 2 1))
(setq lista '(1 5 -2 0 7 -4 2))
(quadruple lista 3)
;-> ((-2 7 -4 2) (5 0 -4 2))

Se N = 5, possiamo scrivere:

Quintupla: (lista[a], lista[b], lista[c], lista[d], lista[e])
lista[a] + lista[b] + lista[c] + lista[d] + lista[e] = X

(define (quintuple lista target)
  (let ((len (length lista))
        (out '()))
    (for (a 0 (- len 5))
      (for (b (+ a 1) (- len 4))
        (for (c (+ b 1) (- len 3))
          (for (d (+ c 1) (- len 2))
            (for (e (+ d 1) (- len 1))
              (when (= (+ (lista a) (lista b) (lista c) (lista d) (lista e)) target)
                (push (list (lista a) (lista b) (lista c) (lista d) (lista e)) out)))))))
    out))

Proviamo:

(setq lista '(1 -1 2 -2 0 1))
(quintuple lista 3)
;-> ((1 -1 2 0 1))

(setq lista '(1 5 -2 0 7 -4 2))
(quintuple lista 3)
;-> ((-2 0 7 -4 2))

Comunque non possiamo scrivere una funzione diversa per ogni valore di N.
Un metodo potrebbe essere quello di generare gli indici dei cicli 'for' con una funzione.

(define (genera-indici N M)
  (local (out elementi base pattern riga)
    (setq out '())
    ; numero di elementi
    (setq elementi (/ (pow (+ M 1) N)))
    ; lunghezza della prima ripetizione della prima riga
    ; (numero di zeri della prima riga)
    (setq base (/ elementi (+ M 1)))
    (while (> base 0)
      ; creazione del pattern da ripetere
      (setq pattern (flat (map (fn(x) (dup x base)) (sequence 0 M))))
      ; creazione della riga
      (setq riga (flat (dup pattern (/ elementi (length pattern)))))
      ; inserimento della riga nella soluzione
      (push riga out -1)
      ; aggiorna 'base' per la prossima riga
      (setq base (/ base (+ M 1)))
    )
    ; trasposta della soluzione
    (transpose out)))

(genera-indici 2 3)
;-> ((0 0) (0 1) (0 2) (0 3)
;->  (1 0) (1 1) (1 2) (1 3)
;->  (2 0) (2 1) (2 2) (2 3)
;->  (3 0) (3 1) (3 2) (3 3))

Adesso possiamo usare questi indici per i nostri calcoli.
Nota: in questo caso la funzione troverà delle liste che sommano al nostro target, ma potrebbero usare gli stessi elementi (cioè non tutti gli indici sono diversi).
Questo perchè tutti i nostri cicli vanno da 0 al numero di elementi della lista data, mentre le funzioni "quadrple" e "quintuple" hanno per i cicli dei valori diversi che permettono di evitare gli elementi multipli.

(define (somma lista num target)
  (local (out len indici lista-val somma)
    (setq out '())
    (setq len (length lista))
    (setq indici '())
    ; genera gli indici
    (setq indici (genera-indici num (- len 1)))
    ; calcola i gruppi di 'num' elementi che sommano a 'target'
    (dolist (idx indici)
      (setq lista-val (map (fn(x) (lista x)) idx))
      (setq somma (apply + lista-val))
      (if (= somma target) (push lista-val out -1))
      ;(if (= val target) (println lista-val))
    )
    ; elimina le liste duplicate
    (unique (map sort out))))

Proviamo:

(setq lista '(1 -1 2 -2 0 1))
(somma lista 4 3)
;-> ((0 1 1 1) (-1 1 1 2) (-2 1 2 2) (0 0 1 2) (-1 0 2 2))

(setq lista '(1 5 -2 0 7 -4 2))
(somma lista 4 3)
;-> ((0 1 1 1) (-4 1 1 5) (-2 1 2 2) (0 0 1 2) (-2 -2 2 5) (-2 0 0 5)
;->  (-4 0 2 5) (-2 -2 0 7) (-4 -2 2 7) (-4 0 0 7))

(setq lista '(1 -1 2 -2 0 1))
(somma lista 5 3)
;-> ((-1 1 1 1 1) (-2 1 1 1 2) (0 0 1 1 1) (-1 0 1 1 2) (-1 -1 1 2 2)
;->  (-2 0 1 2 2) (0 0 0 1 2) (-2 -1 2 2 2) (-1 0 0 2 2))

(setq lista '(1 5 -2 0 7 -4 2))
(somma lista 5 3)
;-> ((-2 1 1 1 2) (0 0 1 1 1) (-2 -2 1 1 5) (-4 0 1 1 5) (-4 -2 1 1 7)
;->  (-4 -4 1 5 5) (-2 0 1 2 2) (0 0 0 1 2) (-4 1 2 2 2) (-2 -2 0 2 5)
;->  (-2 0 0 0 5) (-4 -2 2 2 5) (-4 0 0 2 5) (-2 -2 -2 2 7) (-2 -2 0 0 7)
;->  (-4 -2 0 2 7) (-4 0 0 0 7) (-4 -4 2 2 7))


---------------------------------------------------------
Segmenti paralleli e segmenti perpendicolari (ortogonali)
---------------------------------------------------------

Segmenti paralleli
------------------
Per verificare se due segmenti s1(p1, p2) e s2(p3, p4) sono paralleli, possiamo confrontare i loro vettori direzionali.
Due segmenti sono paralleli se i loro vettori direzionali sono proporzionali.

Vettori direzionali dei segmenti:
  vec1 = (x2 - x1, y2 - y1) per s1
  vec2 = (x4 - x3, y4 - y3) per s2

Verifica della proporzionalità dei vettori direzionali:
Due vettori vec1 = (v1x, v1y) e vec2 = (v2x, v2y) sono paralleli se il rapporto dei componenti è lo stesso:

  Se v1x/v2x = v1y/v2y, allora i segmenti sono paralleli.

Oppure, evitando divisioni (per gestire i casi con componenti zero), quando risulta:

  v1x * v2y = v1y * v2x

(define (parallel-segments? p1 p2 p3 p4 eps)
  (let ( (v1x (sub (p2 0) (p1 0)))
         (v1y (sub (p2 1) (p1 1)))
         (v2x (sub (p4 0) (p3 0)))
         (v2y (sub (p4 1) (p3 1)))
         (eps (or eps 0)) )
    ; due segmenti sono paralleli se risulta (v1x * v2y) = (v1y  * v2x)
    ; eps: soglia di accettazione
    (<= (abs (sub (mul v1x v2y) (mul v1y v2x))) eps)))

Proviamo:

(parallel-segments? '(1 1) '(3 3) '(2 2) '(4 4))
;-> true
(parallel-segments? '(1 1) '(3 3) '(2 2) '(4 4.0001))
;-> nil
(parallel-segments? '(1 1) '(3 3) '(2 2) '(4 4.0001) 0.01)
;-> true
(parallel-segments? '(1 1) '(3 3) '(1 2) '(3 5))
;-> nil

Nota: i segmenti non devono essere punti (cioè ogni segmento deve avere da due punti diversi).

Segmenti perpendicolari (ortogonali)
------------------------------------
Per verificare se due segmenti s1(p1, p2) e s2(p3, p4) sono perpendicolari, utilizziamo il prodotto scalare dei loro vettori direzionali.
Due segmenti sono perpendicolari se il prodotto scalare dei loro vettori direzionali è uguale a zero.

Vettori direzionali dei segmenti:
  vec1 = (x2 - x1, y2 - y1) per s1
  vec2 = (x4 - x3, y4 - y3) per s2

Prodotto scalare dei due vettori:

   vec1 * vec2 = (v1x * v2x) + (v1y * v2y)

Verifica se il prodotto scalare è uguale a zero:

   Se (v1x * v2x) + (v1y * v2y) = 0, allora i segmenti sono perpendicolari.

(define (orthogonal-segments? p1 p2 p3 p4 eps)
  (let ( (v1x (sub (p2 0) (p1 0)))
         (v1y (sub (p2 1) (p1 1)))
         (v2x (sub (p4 0) (p3 0)))
         (v2y (sub (p4 1) (p3 1)))
         (eps (or eps 0)) )
    ; due segmenti sono perpendicolari se il loro prodotto scalare vale 0
    ; eps: soglia di accettazione
    (<= (abs (add (mul v1x v2x) (mul v1y v2y))) eps)))

Proviamo:

(orthogonal-segments? '(0 0) '(2 0) '(0 1) '(0 4))
;-> true
(orthogonal-segments? '(0 0) '(2 0) '(0 1) '(0.1 4))
;-> nil
(orthogonal-segments? '(0 0) '(2 0) '(0 1) '(0.001 4) 1e-6)
;-> nil
(orthogonal-segments? '(0 0) '(2 0) '(0 1) '(0.0000001 4) 1e-6)
;-> true
(orthogonal-segments? '(0 0) '(1 1) '(0 0) '(1 -1))
;-> true
(orthogonal-segments? '(0 0) '(1 1) '(0 0) '(2 2))
;-> nil

Nota: i segmenti non devono essere punti (cioè ogni segmento deve avere da due punti diversi).


--------------------------------------
Numeri di Schroeder (Piccoli e Grandi)
--------------------------------------

Piccoli numeri di Schroeder
---------------------------

A001003
Schroeder's second problem (generalized parentheses), also called super-Catalan numbers or little Schroeder numbers.
  1, 1, 3, 11, 45, 197, 903, 4279, 20793, 103049, 518859, 2646723, 13648869,
  71039373, 372693519, 1968801519, 10463578353, 55909013009, 300159426963,
  1618362158587, 8759309660445, 47574827600981, 259215937709463,
  1416461675464871, ...

Una formula per generare la sequenza è la seguente:

  a(n) = (1/n)*Sum[k=1..n] C(n, k)*C(n+k, k-1)

(define (binom num k)
"Calculates the binomial coefficient (n k) = n!/(k!*(n - k)!) (combinations of k elements without repetition from n elements)"
  (cond ((> k num) 0)
        ((zero? k) 1)
        ((< k 0) 0)
        (true
          (let (r 1L)
            (for (d 1 k)
              (setq r (/ (* r num) d))
              (-- num))
          r))))

(define (** num power)
"Calculates the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(define (a n)
  (cond
    ((zero? n) 1L)
    (true
      (let (sum 0L)
        (for (k 1 n)
          (setq sum (+ sum (* (binom n k) (binom (+ n k) (- k 1))))))
        (/ sum n)))))

(map a (sequence 0 23))
;-> (1L 1L 3L 11L 45L 197L 903L 4279L 20793L 103049L 518859L 2646723L 13648869L
;->  71039373L 372693519L 1968801519L 10463578353L 55909013009L 300159426963L
;->  1618362158587L 8759309660445L 47574827600981L 259215937709463L
;->  1416461675464871L)

Grandi numeri di Schroeder
--------------------------

Sequenza OEIS A006318:
Large Schröder numbers (or large Schroeder numbers, or big Schroeder numbers).
  1, 2, 6, 22, 90, 394, 1806, 8558, 41586, 206098, 1037718, 5293446, 27297738,
  142078746, 745387038, 3937603038, 20927156706, 111818026018, 600318853926,
  3236724317174, 17518619320890, 95149655201962, 518431875418926,
  2832923350929742, 15521467648875090, ...

Una formula per generare la sequenza è la seguente:

  a(n) = (1/n)*Sum[k = 0..n] 2^k*C(n, k)*C(n, k-1), per n > 0.

(define (a n)
  (cond
    ((zero? n) 1L)
    (true
      (let (sum 0L)
        (for (k 0 n)
          (setq sum (+ sum (* (** 2 k) (binom n k) (binom n (- k 1))))))
        (/ sum n)))))

(map a (sequence 0 22))
;-> (1L 2L 6L 22L 90L 394L 1806L 8558L 41586L 206098L 1037718L 5293446L 27297738L
;->  142078746L 745387038L 3937603038L 20927156706L 111818026018L 600318853926L
;->  3236724317174L 17518619320890L 95149655201962L 518431875418926L)


-------------------------------
Trasformazione dei numeri primi
-------------------------------

Sequenza OEIS A159006:
Transformation of prime(n): flip digits in the binary representation, revert the sequence of digits, and convert back to decimal.
  2, 0, 2, 0, 2, 4, 14, 6, 2, 8, 0, 22, 26, 10, 2, 20, 8, 16, 30, 14, 54,
  6, 26, 50, 60, 44, 12, 20, 36, 56, 0, 62, 110, 46, 86, 22, 70, 58, 26,
  74, 50, 82, 2, 124, 92, 28, 52, 4, 56, 88, 104, 8, 112, 32, 254, 62,
  158, 30, 174, 206, 78, 182, 102, 38, 198, 134, 90, 234, 74, 138, 242, ...

37->100101->change digits 011010->read from right to left 010110->22 281->100011001->change digits 011100110->read from right to left 011001110->206

Esempi:

n = 37
binario: 100101 --> flip: 011010 --> revert: 010110 --> decimale: 22

n = 281
binario: 100011001 --> flip: 011100110 --> revert: 011001110 --> decimale: 206

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (a n)
  (if (prime? n)
      (let (out (bits n)) ; binario
        ; flip
        (for (i 0 (- (length out) 1))
          (if (= "0" (out i))
              (setf (out i) "1")
              (setf (out i) "0")))
        (reverse out)
        ; reverse and convert to decimal
        (int out 0 2))
      ;else
      nil))

Proviamo:

(a 37)
;-> 22
(a 281)
;-> 206
(a 4)
;-> nil

(clean nil? (map a (sequence 0 200)))
;-> (2 0 2 0 2 4 14 6 2 8 0 22 26 10 2 20 8 16 30 14 54
;->  6 26 50 60 44 12 20 36 56 0 62 110 46 86 22 70 58 26
;->  74 50 82 2 124 92 28)

Se al posto dei soli numeri primi calcoliamo la trasformazione per tutti i numeri otteniamo:

Sequenza OEIS A036044:
BCR(n): write in binary, complement, reverse.
  1, 0, 2, 0, 6, 2, 4, 0, 14, 6, 10, 2, 12, 4, 8, 0, 30, 14, 22, 6, 26, 10,
  18, 2, 28, 12, 20, 4, 24, 8, 16, 0, 62, 30, 46, 14, 54, 22, 38, 6, 58,
  26, 42, 10, 50, 18, 34, 2, 60, 28, 44, 12, 52, 20, 36, 4, 56, 24, 40, 8,
  48, 16, 32, 0, 126, 62, 94, 30, 110, 46, 78, 14, 118, 54, 86, ...

(define (a n)
  (let (out (bits n)) ; binario
    ; flip
    (for (i 0 (- (length out) 1))
      (if (= "0" (out i))
          (setf (out i) "1")
          (setf (out i) "0")))
    (reverse out)
    ; reverse and convert to decimal
    (int out 0 2)))

(map a (sequence 1 59))
;-> (0 2 0 6 2 4 0 14 6 10 2 12 4 8 0 30 14 22 6 26 10
;->  18 2 28 12 20 4 24 8 16 0 62 30 46 14 54 22 38 6 58
;->  26 42 10 50 18 34 2 60 28 44 12 52 20 36 4 56 24 40 8)


-------------------------------
La funzione Tally (Mathematica)
-------------------------------

In Wolfram Mathematica la funzione "Tally" prende una lista ed elenca tutti gli elementi distinti insieme alle loro molteplicità.
Per esempio:
lista  = (a a b a c b a)
output = ((a 4) (b 2) (c 1))

Scriviamo la funzione:

(define (tally lst)
  (map list (unique lst) (count (unique lst) lst)))

Proviamo:

(setq a '(a a b a c b a))
(tally a)
;-> ((a 4) (b 2) (c 1))

(tally (rand 6 100))
;-> ((1 18) (4 15) (5 16) (2 13) (3 22) (0 16))


-----------------------------------
La funzione "Entropy" (Mathematica)
-----------------------------------

La funzione Entropy in Mathematica calcola l'entropia di un dataset o di una distribuzione di probabilità.
La formula utilizzata dipende dal tipo di input fornito.
Di seguito sono descritte le principali modalità di calcolo:

1. Per una distribuzione discreta o una lista di probabilità
------------------------------------------------------------
Se l'input è una lista di probabilità (p1, p2, p3, ..., pn), l'entropia è calcolata come:

  H = - Sum[i=1,..,n] p(i)*log2(i)

dove: p(i) sono le probabilità associate agli eventi e il logaritmo è in base 2.

2. Per un dataset (frequenze di eventi)
---------------------------------------
Se l'input è una lista di frequenze (f, f2, f3, ..., fn), mathematica normalizza le frequenze per ottenere le probabilità:

                f(i)
  p(i) = -------------------
          Sum[j=1,..,n] f(j)

Poi calcola l'entropia con la formula vista sopra.

Esempi in Mathematica:
Con una lista di probabilità:
Entropy[{0.5, 0.5}]
Risultato: 1 (bit)

Con una lista di frequenze:
Entropy[{2, 2, 1}]
Qui le probabilità saranno calcolate come {0.4,0.4,0.2}.

3. Per una distribuzione simbolica o continua
---------------------------------------------
Se l'input è una distribuzione simbolica (ad esempio, una distribuzione normale), Mathematica utilizza la formula analitica appropriata per l'entropia differenziale.
Per una distribuzione continua P(x) l'entropia è definita come:

  H = - Integrale[P(x)*log2(P(x))dx]

4. Per una lista di numeri o simboli/caratteri
----------------------------------------------
Se passiamo a Mathematica una lista di numeri (o simboli), la funzione Entropy tratterà i numeri come dati grezzi e calcolerà l'entropia in base alle frequenze dei valori nella lista.
Il processo consiste nei seguenti passi:

a. Normalizzazione delle frequenze:
Mathematica costruisce un istogramma implicito dei dati e calcola la frequenza relativa di ciascun valore (o intervallo).
Le frequenze sono poi normalizzate per ottenere le probabilità associate ai valori (o agli intervalli).

          conteggio delle occorrenze di i
  p(i) = ---------------------------------
            conteggio di tutti i dati

b. Calcolo dell'entropia:
L'entropia viene quindi calcolata utilizzando la formula:

  H = - Sum[i=1,..,n] p(i)*log(i)

dove p(i) rappresenta la probabilità di ciascun valore discreto (o intervallo).

Esempio:

Entropy[{1.2, 2.3, 1.2, 3.4, 3.4, 3.4, 2.3}]

Frequenze:
  1.2: 2 occorrenze
  2.3: 2 occorrenze
  3.4: 3 occorrenze

Probabilità:
  p(1.2) = 2/7
  p(2.3) = 2/7
  p(3.4) = 3/7

Entropia:

  H = - (2/7*log2(2/7) + 2/7*log2(2/7) + 3/7*log2(3/7)) = 1.5566567

Scriviamo una funzione che calcola l'entropia di una lista di numeri o simboli.

(define (entropy lst)
  (local (freqs probs sum-freqs))
    ; calcolo delle frequenze degli elementi distinti della lista
    (setq freqs (count (unique lst) lst))
    ; somma delle frequenze
    (setq sum-freqs (apply + freqs))
    ; calcolo delle probabilità associate ad ogni frequenza
    (setq probs (map (fn(x) (div x sum-freqs)) freqs))
    ; calcolo dell'entropia
    (sub (apply add (map (fn(x) (mul x (log x 2))) probs))))

Proviamo

(entropy '(1.2 2.3 1.2 3.4 3.4 3.4 2.3))
;-> 1.556656707462823

(entropy (explode "entropia di una stringa"))
;-> 3.522711955680758

Vedi anche "Entropia di Shannon" su "Note libere 10".


---------------------------------
Taglio di una torta (Numeri Cake)
---------------------------------

Dato un cubo (o una torta) determinare il numero massimo di pezzi risultanti da n tagli planari attraverso il cubo (o la torta).
Partendo da un cubo, il primo piano che taglia il cubo lo divide in due parti,
il secondo taglio divide il cubo in quattro parti,
il terzo taglio divide il cubo in otto parti,
ecc.

Sequenza OEIS A000125:
Cake numbers: maximal number of pieces resulting from n planar cuts through a cube (or cake): C(n+1,3) + n + 1.
  1, 2, 4, 8, 15, 26, 42, 64, 93, 130, 176, 232, 299, 378, 470, 576, 697,
  834, 988, 1160, 1351, 1562, 1794, 2048, 2325, 2626, 2952, 3304, 3683, 4090,
  4526, 4992, 5489, 6018, 6580, 7176, 7807, 8474, 9178, 9920, 10701, 11522,
  12384, 13288, 14235, 15226, ...

La formula per calcolare la sequenza è la seguente:

  pezzi-max = C(n+1, 3) + n + 1

(define (binom num k)
"Calculates the binomial coefficient (n k) = n!/(k!*(n - k)!) (combinations of k elements without repetition from n elements)"
  (cond ((> k num) 0L)
        ((zero? k) 1L)
        ((< k 0) 0L)
        (true
          (let (r 1L)
            (for (d 1 k)
              (setq r (/ (* r num) d))
              (-- num))
          r))))

(define (cake n) (+ (binom (+ n 1) 3) n 1))

(map cake (sequence 0 30))
;-> (1L 2L 4L 8L 15L 26L 42L 64L 93L 130L 176L 232L 299L 378L 470L 576L 697L
;->  834L 988L 1160L 1351L 1562L 1794L 2048L 2325L 2626L 2952L 3304L 3683L
;->  4090L 4526L)

Vedi anche "Taglio di una torta" su "Note libere 10".


------------------------------------------------------------
Parte intera della radice quadrata dell'n-esimo numero primo
------------------------------------------------------------

Sequenza OEIS A000006:
Integer part of square root of n-th prime.
  1, 1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9,
  10, 10, 10, 10, 10, 11, 11, 11, 11, 12, 12, 12, 12, 12, 13, 13, 13, 13, 13,
  14, 14, 14, 14, 15, 15, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 17, 17,
  17, 17, 17, 18, 18, 18, 18, 18, ...

Conjecture:
No two successive terms in the sequence differ by more than 1.
Proof of this would prove the converse of the theorem that every prime is surrounded by two consecutive squares, namely |sqrt(p)|^2 and (|sqrt(p)|+1)^2. - Cino Hilliard, Jan 22 2003

Funzione che calcola un dato numero di numeri primi:

(define (primes num-primes)
  (let ( (k 2) (tot 0) (out '()) )
    (until (= tot num-primes)
      (when (= 1 (length (factor k)))
        (push k out -1)
        (++ tot))
      (++ k))
    out))

(primes 10)
;-> (2 3 5 7 11 13 17 19 23 29)

(time (primes 1000))
;-> 0
(time (primes 100000))
;-> 1028.681
(time (primes 1000000))
;-> 29542.694

(define (seq num-items)
  (let (primi (primes num-items))
    (map (fn(x) (int (sqrt x))) primi)))

(seq 100)
;-> (1 1 2 2 3 3 4 4 4 5 5 6 6 6 6 7 7 7 8 8 8 8 9 9 9
;->  10 10 10 10 10 11 11 11 11 12 12 12 12 12 13 13 13 13 13
;->  14 14 14 14 15 15 15 15 15 15 16 16 16 16 16 16 16 17 17
;->  17 17 17 18 18 18 18 18 18 19 19 19 19 19 19 20 20 20 20
;->  20 20 20 21 21 21 21 21 21 21 22 22 22 22 22 22 22 23)


--------------------------------------------------------
Somma N con una lista di numeri (Subset Sum Problem SSP)
--------------------------------------------------------

Data una lista di numeri interi positivi e un numero intero positivo N, determinare in quanti modi possiamo usare i numeri della lista per sommare a N.
I numeri della lista possono essere usati più di una volta.

Esempio 1:
  Lista = (2 3 5 6)
  N = 10
  Modi: 5 --> (2 2 2 2 2) (2 2 3 3) (2 2 6) (2 3 5) (5 5)

Esempio 2:
  Lista = (1 2 3)
  N = 4
  Modi = 4 --> (1 1 1 1 1) (1 1 2) (2 2) (1 3)

Esempio 3:
Numero di modi di sommare a N con i numeri 1, 2, 5 e 10.
Sequenza OEIS A000008:
Number of ways of making change for n cents using coins of 1, 2, 5, 10 cents.
  1, 1, 2, 2, 3, 4, 5, 6, 7, 8, 11, 12, 15, 16, 19, 22, 25, 28, 31, 34, 40,
  43, 49, 52, 58, 64, 70, 76, 82, 88, 98, 104, 114, 120, 130, 140, 150, 160,
  170, 180, 195, 205, 220, 230, 245, 260, 275, 290, 305, 320, 341, 356, 377,
  392, 413, 434, 455, 476, 497, 518, 546, ...

Problema 1: Calcolo del numero di combinazioni
----------------------------------------------

Funzione che data una lista di interi positivi (nums) e un intero positivo (target) restituisce il numero di combinazioni dei numeri della lista (nums) che sommano all'intero (target):
(Vedi "Cambio monete 1 (LinkedIn)" su "Domande")

(define (ssp-count nums target)
  ; vett[i] memorizza il numero di soluzioni per il valore i.
  ; Servono (n + 1) righe perchè la tabella viene costruita
  ; in modo bottom-up usando il caso base (n = 0).
  (let ( (vett (array (+ target 1) '(0)))
         (len (length nums))
         (i 0) (j 0) )
    ; caso base
    (setf (vett 0) 1)
    ; Prende tutte le nums una per una e aggiorna i valori
    ; di vett dove l'indice è maggiore o uguale a quello
    ; del numero scelto.
    (while (< i len)
      (setq j (nums i))
      (while (<= j target)
        (setf (vett j) (+ (vett j) (vett (- j (nums i)))))
        (++ j))
      (++ i))
    (vett target)))

Proviamo:

(ssp-count '(2 3 5 6) 10)
;-> 5

(ssp-count '(1 2 3) 4)
;-> 4

(ssp-count '(5 10) 12)
;-> 0

(map (fn(x) (ssp-count '(1 2 5 10) x)) (sequence 1 60))
;-> (1 2 2 3 4 5 6 7 8 11 12 15 16 19 22 25 28 31 34 40
;->  43 49 52 58 64 70 76 82 88 98 104 114 120 130 140 150 160
;->  170 180 195 205 220 230 245 260 275 290 305 320 341 356 377
;->  392 413 434 455 476 497 518 546)

Problema 2: Calcolo della lista delle combinazioni
--------------------------------------------------

Funzione che data una lista di interi positivi (nums) e un intero positivo (target) restituisce tutte le di combinazioni dei numeri della lista (nums) che sommano all'intero (target):
(Questa funzione utilizza una versione iterativa del backtracking per trovare tutte le combinazioni di numeri nella lista "nums" che sommano a "target")

(define (ssp-list nums target)
  ;(setq result '())
  (let (result '()) ; variabile globale per le chiamate ricorsive di backtrack
    (define (backtrack temp remaining start)
      (cond ((= remaining 0) (push temp result -1))
            ((< remaining 0) nil)
            (true
            (for (i start (- (length nums) 1))
              (let ((current (nums i)))
                (backtrack (cons current temp) (- remaining current) i)))))
      result)
    (backtrack '() target 0)
    result))

Proviamo:

(ssp-list '(2 3 5 6) 10)
;-> ((2 2 2 2 2) (3 3 2 2) (6 2 2) (5 3 2) (5 5))

(ssp-list '(1 2 3) 4)
;-> ((1 1 1 1) (2 1 1) (3 1) (2 2))

(ssp-list '(5 10) 12)
;-> ()

(ssp-list '(1 2 5 10) 10)
;-> ((1 1 1 1 1 1 1 1 1 1) (2 1 1 1 1 1 1 1 1) (2 2 1 1 1 1 1 1)
;->  (5 1 1 1 1 1) (2 2 2 1 1 1 1) (5 2 1 1 1) (2 2 2 2 1 1)
;->  (5 2 2 1) (2 2 2 2 2) (5 5) (10))
(length (ssp-list '(1 2 5 10) 10))
;-> 11
(ssp-count '(1 2 5 10) 10)
;-> 11

(length (ssp-list '(1 2 5 10) 30))
;-> 98
(ssp-count '(1 2 5 10) 30)
;-> 98

(length (ssp-list '(1 2 5 10) 50))
;-> 341
(ssp-count '(1 2 5 10) 50)
;-> 341

Vedi anche "Cambio monete 1 (LinkedIn)" su "Domande".
Vedi anche "Somma di numeri in una lista (Google)" su "Domande".
Vedi anche "Somme dei sottoinsiemi di una lista" su "Note libere 8".
Vedi anche "Somma dei sottoinsiemi (Subset Sum Problem)" su "Note libere 8".
Vedi anche "Subset Sum Problem" su "Note libere 17".


-------------------------
Smallest prime power >= n
-------------------------

"Smallest prime power >= n" (la più piccola potenza prima >= n) significa trovare il più piccolo numero che sia una potenza di un numero primo (ad esempio 2^3 = 8, 3^2 = 9, ecc.) e che sia maggiore o uguale a un dato numero n.

Potenza di un numero primo: Un numero che può essere scritto come p^k, dove:
  - p è un numero primo (es. 2, 3, 5, 7, ...),
  - k è un numero intero positivo (k >= 1 ).

Quindi l'obiettivo è trovare il più piccolo p^k tale che p^k >= n.
Questa definizione viene utilizzata in matematica discreta, teoria dei numeri, o analisi di algoritmi.

Esempio per n = 10:
1. I numeri primi fino a 10: 2, 3, 5, 7, 11.
2. Calcolo delle potenze:
   - 2^1 = 2, 2^2 = 4, 2^3 = 8, 2^4 = 16 (16 >= 10).
   - 3^1 = 3, 3^2 = 9, 3^3 = 27 (27 >= 10).
   - 5^1 = 5, 5^2 = 25 (25 >= 10).
   - 7^1 = 7, 7^2 = 49 (49 >= 10).
   - 11^1 = 11 (11 >= 10).
3. Ricerca della potenza più piccola:
   La potenza più piccola >= 10 è 11 (11^1).

Sequenza OEIS A000015:
Smallest prime power >= n. With a(1) = 1.
  1, 2, 3, 4, 5, 7, 7, 8, 9, 11, 11, 13, 13, 16, 16, 16, 17, 19, 19, 23, 23,
  23, 23, 25, 25, 27, 27, 29, 29, 31, 31, 32, 37, 37, 37, 37, 37, 41, 41, 41,
  41, 43, 43, 47, 47, 47, 47, 49, 49, 53, 53, 53, 53, 59, 59, 59, 59, 59, 59,
  61, 61, 64, 64, 64, 67, 67, 67, 71, 71, 71, 71, 73, ...

Algoritmo:
1. Generare i primi fino al primo valore per cui risulta p^k >= n
È sufficiente considerare i numeri primi fino che risulta p^k >= n, perché se p = n, allora la soluzione vale p^1.
Questo ci assicura che nessuna potenza rilevante venga tralasciata.
Il primo valore per cui risulta sicuramente p^k >= n si trova tra p(n) e p(2n).

2. Calcolare le potenze p^k
Per ogni primo p, calcolare p^k (con k >= 1 fino a quando (p^k >= n).

3. Scegliere il valore più piccolo >= n
Tra tutte le potenze p^k calcolate, il risultato sarà il più piccolo valore >= n).

(define (primes-to num)
"Generates all prime numbers less than or equal to a given number"
  (cond ((= num 1) '())
        ((= num 2) '(2))
        (true
          (let ((lst '(2)) (arr (array (+ num 1))))
            (for (x 3 num 2)
              (when (not (arr x))
                (push x lst -1)
                (for (y (* x x) num (* 2 x) (> y num))
                  (setf (arr y) true)))) lst))))

Funzione che calcola le potenze di una lista di primi fino al primo valore per cui risulta p^k >= n:

(define (prime-power lst n)
  (setq out '())
  (dolist (p lst)
    (setq temp '())
    (setq k 1)
    (setq power (pow p k))
    (while (>= n power)
      (push power temp -1)
      (++ k)
      (setq power (pow p k))
    )
    ; aggiunge il primo p^k maggiore di n
    (push power temp -1)
    (extend out temp)))

(setq n 10)
; numeri primi fino a 2*n
(setq primi (primes-to (* n 2)))
;-> (2 3 5 7 11 13 17 19)
; potenze dei primi fino al primo p^k >= n
(setq potenze (prime-power primi n))
;-> (2 4 8 16 3 9 27 5 25 7 49 11 13 17 19)
; ordina le potenze
(sort potenze)
;-> (2 3 4 5 7 8 9 11 13 16 17 19 25 27 49)
; ricerca il primo numero maggiore di n (10) su potenze
(potenze (find n potenze <=))
;-> 11

Funzione che genera la sequenza fino ad una dato numero di elementi:

(define (small n)
  (let (numeri (sort (prime-power (primes-to (* n 2)) n)))
    (push 1 (map (fn(x) (numeri (find x numeri <=))) (sequence 2 n)))))

Proviamo:

(small 100)
;-> (1 2 3 4 5 7 7 8 9 11 11 13 13 16 16 16 17 19 19 23 23
;->  23 23 25 25 27 27 29 29 31 31 32 37 37 37 37 37 41 41 41
;->  41 43 43 47 47 47 47 49 49 53 53 53 53 59 59 59 59 59 59
;->  61 61 64 64 64 67 67 67 71 71 71 71 73 73 79 79 79 79 79
;->  79 81 81 83 83 89 89 89 89 89 89 97 97 97 97 97 97 97 97
;->  101 101 101)


----------------------------------------------------------
Numero di interi positivi <= 2^n nella forma a*x^2 + b*y^2
----------------------------------------------------------

Algoritmo
Ricerca esaustiva (brute-force) dei valori di x e y che soddisfano l'equazione.
Nota: i due cicli "for" innestati vanno da 0 a 2^n... e 2^18 = 262144.

(define (seq n a b)
  (let ((limite (pow 2 n))
        (numeri '()))
    (for (x 0 (+ (int (sqrt (div limite (abs a)))) 1))
      (for (y 0 (+ (int (sqrt (div limite (abs b)))) 1))
        ;(print x { } y { } (+ (* a x x) (* b y y))) (read-line)
        (let (valore (+ (* a x x) (* b y y)))
          (if (<= valore limite) (push valore numeri)))))
    ; calcoliamo solo i valori unici e
    ; togliamo 1 che rappresenta la soluzione non valida x=0 e y=0
    (- (length (unique numeri)) 1)))

a = 1, b = 1
------------
Sequenza OEIS A000050:
Number of positive integers <= 2^n of form x^2 + y^2.
With a(0)=1, a(1)=2, a(2)=3
  1, 2, 3, 5, 9, 16, 29, 54, 97, 180, 337, 633, 1197, 2280, 4357, 8363,
  16096, 31064, 60108, 116555, 226419, 440616, 858696, 1675603, 3273643,
  6402706, 12534812, 24561934, 48168461, 94534626, 185661958, 364869032,
  717484560, 1411667114, 2778945873, 5473203125, ...

(map (fn(x) (seq x 1 1)) (sequence 3 15))
;-> (5 9 16 29 54 97 180 337 633 1197 2280 4357 8363)

Vediamo la velocità della funzione:

(time (map (fn(x) (seq x 1 1)) (sequence 3 10)))
;-> 4.997
(time (map (fn(x) (seq x 1 1)) (sequence 3 15)))
;-> 6310.474
(time (map (fn(x) (seq x 1 1)) (sequence 3 16)))
La funzione e lenta perchè i cicli "for" innestati vanno da 0 a 2^n... e 2^16 = 65536.

L'approccio matematico per risolvere il problema è basato sulla teoria delle forme quadratiche e sui numeri gaussiani (ma non lo conosco per niente).

a = 1, b = 16
-------------
Sequenza OEIS A000018:
Number of positive integers <= 2^n of form x^2 + 16*y^2.
With: a(0)=1, a(1)=1, a(2)=2, a(3)=2
  1, 1, 2, 2, 4, 8, 13, 25, 44, 83, 152, 286, 538, 1020, 1942, 3725, 7145,
  13781, 26627, 51572, 100099, 194633, 379037, 739250, 1443573, 2822186,
  5522889, 10818417, 21209278, 41613288, 81705516, 160532194, 315604479,
  620834222, 1221918604, 2406183020, 4740461247, ...
(map (fn(x) (seq x 1 16)) (sequence 4 15))
;-> (4 8 13 25 44 83 152 286 538 1020 1942 3725)

a = 1, b = 12
-------------
Sequenza OEIS A000021:
Number of positive integers <= 2^n of form x^2 + 12*y^2.
With: a(0)=1, a(1)=1, a(2)=2, a(3)=2
  1, 1, 2, 2, 6, 9, 17, 30, 54, 98, 183, 341, 645, 1220, 2327, 4451, 8555,
  16489, 31859, 61717, 119779, 232919, 453584, 884544, 1727213, 3376505,
  6607371, 12942012, 25371540, 49777187, 97731027, 192010355, 377475336,
  742512992, 1461352025, 2877572478, 5668965407, ...
(map (fn(x) (seq x 1 12)) (sequence 4 15))
;-> (6 9 17 30 54 98 183 341 645 1220 2327 4451)

a = 1, b = 10
-------------
Sequenza OEIS A000024:
Number of positive integers <= 2^n of form x^2 + 10*y^2.
With: a(0)=1, a(1)=1, a(2)=2, a(3)=2
  1, 1, 2, 2, 7, 10, 20, 36, 65, 118, 221, 409, 776, 1463, 2788, 5328,
  10222, 19714, 38054, 73685, 142944, 277838, 540889, 1054535, 2058537,
  4023278, 7871313, 15414638, 30213190, 59266422, 116343776, 228545682,
  449240740, 883570480, 1738769611, 3423469891, 6743730746, ...
(map (fn(x) (seq x 1 10)) (sequence 4 15))
;-> (7 10 20 36 65 118 221 409 776 1463 2788 5328)

a = 3, b = 4
------------
Sequenza OEIS A000049:
Number of positive integers <= 2^n of the form 3*x^2 + 4*y^2.
With: a(0)=0, a(1)=0, a(2)=2, a(3)=3
  0, 0, 2, 3, 5, 9, 16, 29, 53, 98, 181, 341, 640, 1218, 2321, 4449, 8546,
  16482, 31845, 61707, 119760, 232865, 453511, 884493, 1727125, 3376376,
  6607207, 12941838, 25371086, 49776945, 97730393, 192009517, 377473965,
  742511438, 1461351029, 2877568839, 5668961811, ...
(map (fn(x) (seq x 3 4)) (sequence 4 15))
;-> (5 9 16 29 53 98 181 341 640 1218 2321 4449)

a = 1, b = 2
------------
Sequenza OEIS A000067:
Number of positive integers <= 2^n of form x^2 + 2*y^2.
With: a(0)=1, a(1)=2, a(2)=4
  1, 2, 4, 6, 10, 18, 33, 60, 111, 205, 385, 725, 1374, 2610, 4993, 9578,
  18426, 35568, 68806, 133411, 259145, 504222, 982538, 1917190, 3745385,
  7324822, 14339072, 28095711, 55095559, 108124461, 212342327, 417283564,
  820520378, 1614331755, 3177789615, 6258525127, ...
(map (fn(x) (seq x 1 2)) (sequence 3 15))
;-> (6 10 18 33 60 111 205 385 725 1374 2610 4993 9578)

a = 1, b = 4
------------
Sequenza OEIS A000072:
Number of positive integers <= 2^n of form x^2 + 4*y^2.
With: a(0)=1, a(1)=1, a(2)=2, a(3)=4
  1, 1, 2, 4, 7, 12, 22, 41, 72, 137, 254, 476, 901, 1716, 3274, 6286,
  12090, 23331, 45140, 87511, 169972, 330752, 644499, 1257523, 2456736,
  4804666, 9405749, 18429828, 36141339, 70928099, 139295793, 273741700,
  538277486, 1059051586, 2084763319, 4105924366, ...
(map (fn(x) (seq x 1 4)) (sequence 4 15))
;-> (7 12 22 41 72 137 254 476 901 1716 3274 6286)

a = 2, b = 3
------------
Sequenza OEIS A000075:
Number of positive integers <= 2^n of form 2*x^2 + 3*y^2.
With: a(0)=0, a(1)=1, a(2)=2, a(3)=4
  0, 1, 2, 4, 7, 14, 23, 42, 76, 139, 258, 482, 907, 1717, 3269, 6257,
  12020, 23171, 44762, 86683, 168233, 327053, 636837, 1241723, 2424228,
  4738426, 9271299, 18157441, 35591647, 69820626, 137068908, 269270450,
  529312241, 1041093048, 2048825748, 4034059456, ...
(map (fn(x) (seq x 2 3)) (sequence 4 15))
;-> (7 14 23 42 76 139 258 482 907 1717 3269 6257)

a = 1, b = 6
------------
Sequenza OEIS A000077:
Number of positive integers <= 2^n of form x^2 + 6*y^2.
With: a(0)=1, a(1)=1, a(2)=2, a(3)=4
  1, 1, 2, 4, 8, 13, 24, 42, 76, 140, 257, 483, 907, 1717, 3272, 6261,
  12027, 23172, 44769, 86708, 168245, 327073, 636849, 1241720, 2424290,
  4738450, 9271396, 18157630, 35591729, 69820804, 137069245, 269270791,
  529312776, 1041093937, 2048826229, 4034062310, ...
(map (fn(x) (seq x 1 6)) (sequence 4 15))
;-> (8 13 24 42 76 140 257 483 907 1717 3272 6261)

a = 1, b = 3
------------
Sequenza OEIS A000205:
Number of positive integers <= 2^n of form x^2 + 3*y^2.
With: a(0)=1, a(1)=1, a(2)=3, a(3)=4
  1, 1, 3, 4, 8, 14, 25, 45, 82, 151, 282, 531, 1003, 1907, 3645, 6993,
  13456, 25978, 50248, 97446, 189291, 368338, 717804, 1400699, 2736534,
  5352182, 10478044, 20531668, 40264582, 79022464, 155196838, 304997408,
  599752463, 1180027022, 2322950591, 4575114295, ...
(map (fn(x) (seq x 1 3)) (sequence 4 15))
;-> (8 14 25 45 82 151 282 531 1003 1907 3645 6993)

a = 2, b = 5
------------
Sequenza OEIS A000286:
Number of positive integers <= 2^n of form 2*x^2 + 5*y^2.
With: a(0)=0, a(1)=1, a(2)=1, a(3)=4
  0, 1, 1, 4, 5, 11, 20, 36, 65, 119, 218, 412, 770, 1466, 2784, 5322,
  10226, 19691, 38048, 73665, 142927, 277822, 540851, 1054502, 2058507,
  4023164, 7871226, 15414517, 30213010, 59266164, 116343183, 228545303,
  449240025, 883569304, 1738768584, 3423466797, 6743729031, ...
(map (fn(x) (seq x 2 5)) (sequence 4 15))
;-> (5 11 20 36 65 119 218 412 770 1466 2784 5322)

a = 1, b = 7
------------
Sequenza OEIS A054151:
Number of positive integers <= 2^n of form x^2 + 7*y^2.
With: a(0)=1, a(1)=1, a(2)=2, a(3)=4
  1, 1, 2, 4, 7, 12, 21, 39, 69, 130, 240, 451, 854, 1620, 3101, 5941,
  11443, 22080, 42724, 82835, 160914, 313149, 610230, 1190802, 2326448,
  4550161, 8908095, 17455391, 34232368, 67184426, 131949378, 259313689,
  509925468, 1003302084, ...
(map (fn(x) (seq x 1 7)) (sequence 4 15))
;-> (7 12 21 39 69 130 240 451 854 1620 3101 5941)

a = 1, b = 8
------------
Sequenza OEIS A054152:
Number of positive integers <= 2^n of form x^2 + 8*y^2.
With: a(0)=1, a(1)=1, a(2)=2, a(3)=3
  1, 1, 2, 3, 6, 10, 18, 32, 59, 107, 202, 375, 709, 1344, 2567, 4905,
  9423, 18151, 35051, 67879, 131688, 255972, 498339, 971558, 1896661,
  3706983, 7252576, 14203258, 27839240, 54610355, 107204740, 210595574,
  413961363, 814190153, 1602250651, ...
(map (fn(x) (seq x 1 8)) (sequence 4 15))
;-> (6 10 18 32 59 107 202 375 709 1344 2567 4905)

a = 1, b = 9
------------
Sequenza OEIS A054153:
Number of positive integers <= 2^n of form x^2 + 9*y^2.
With: a(0)=1, a(1)=1, a(2)=2, a(3)=2
  1, 1, 2, 2, 6, 8, 18, 30, 57, 103, 194, 360, 682, 1292, 2464, 4721,
  9078, 17481, 33808, 65485, 127116, 247179, 481426, 938950, 1833572,
  3584679, 7015237, 13741715, 26940645, 52858250, 103785060, 203913141,
  400891200, 788601863, 1552114600, ...
(map (fn(x) (seq x 1 9)) (sequence 4 15))
;-> (6 8 18 30 57 103 194 360 682 1292 2464 4721)

a = 2, b = 7
------------
Sequenza OEIS A054157:
Number of positive integers <= 2^n of form 2*x^2 + 7*y^2.
With: a(0)=1, a(1)=1, a(2)=2, a(3)=4, a(4)=5
  0, 1, 1, 3, 5, 10, 17, 33, 58, 109, 202, 379, 720, 1369, 2618, 5031,
  9701, 18766, 36387, 70688, 137572, 268208, 523581, 1023425, 2002829,
  3923400, 7692930, 15096706, 29649082, 58270386, 114596178, 225503077,
  443996993, 874648723, 1723849033, ...
(map (fn(x) (seq x 2 7)) (sequence 5 15))
;-> (10 17 33 58 109 202 379 720 1369 2618 5031)

a = 2, b = 9
------------
Sequenza OEIS A054159:
Number of positive integers <= 2^n of form 2*x^2 + 9*y^2.
With: a(0)=0, a(1)=1, a(2)=1, a(3)=2, a(4)=4
  0, 1, 1, 2, 4, 8, 15, 27, 51, 93, 174, 329, 620, 1177, 2250, 4308,
  8286, 15961, 30871, 59792, 116083, 225754, 439666, 857554, 1674630,
  3273944, 6407147, 12550615, 24605525, 48277287, 94790704, 186242926,
  366153007, 720274117, 1417640576, ...
(map (fn(x) (seq x 2 9)) (sequence 5 15))
;-> (8 15 27 51 93 174 329 620 1177 2250 4308)

a = 3, b = 3
------------
Sequenza OEIS A054161:
Number of positive integers <= 2^n of form 3*x^2 + 3*y^2.
With: a(0)=0, a(1)=1, a(2)=1, a(3)=2, a(4)=4
  0, 0, 1, 2, 4, 7, 12, 21, 38, 69, 126, 234, 437, 825, 1565, 2982,
  5710, 10967, 21138, 40842, 79099, 153510, 298435, 581095, 1133110,
  2212231, 4324045, 8460649, 16570079, 32480219, 63717408, 125088092,
  245735287, 483050536, 950104880
(map (fn(x) (seq x 3 3)) (sequence 5 15))
;-> (7 12 21 38 69 126 234 437 825 1565 2982)

a = 3, b = 5
------------
Sequenza OEIS A054162:
Number of positive integers <= 2^n of form 3*x^2 + 5*y^2.
With: a(0)=0, a(1)=0, a(2)=1, a(3)=3, a(4)=4
  0, 0, 1, 3, 4, 9, 14, 28, 51, 92, 173, 319, 603, 1148, 2188, 4191,
  8051, 15535, 30003, 58138, 112855, 219452, 427408, 833549, 1627633,
  3181948, 6226802, 12196599, 23910445, 46911094, 92104226, 180957097,
  355747623, 699778072, 1377253775, ...
(map (fn(x) (seq x 3 5)) (sequence 5 15))
;-> (9 14 28 51 92 173 319 603 1148 2188 4191)

a = 3, b = 6
------------
Sequenza OEIS A054163:
Number of positive integers <= 2^n of form 3*x^2 + 6*y^2.
With: a(0)=0, a(1)=0, a(2)=1, a(3)=2, a(4)=4
  0, 0, 1, 2, 4, 7, 13, 23, 43, 78, 144, 269, 502, 945, 1794, 3414,
  6537, 12559, 24195, 46751, 90539, 175693, 341530, 664965, 1296520,
  2531118, 4946995, 9678777, 18954593, 37152384, 72879304, 143067454,
  281043183, 552433040, 1086529470, ...
(map (fn(x) (seq x 3 6)) (sequence 5 15))
;-> (7 13 23 43 78 144 269 502 945 1794 3414)

a = 3, b = 7
------------
Sequenza OEIS A054164:
Number of positive integers <= 2^n of form 3*x^2 + 7*y^2.
With: a(0)=0, a(1)=0, a(2)=1, a(3)=2, a(4)=4
  0, 0, 1, 2, 4, 8, 13, 24, 44, 79, 147, 275, 516, 976, 1859, 3552,
  6810, 13102, 25281, 48908, 94819, 184144, 358279, 698183, 1362298,
  2661409, 5204770, 10189097, 19964562, 39151220, 76835090, 150897513,
  296542636, 583115721, 1147276480, ...
(map (fn(x) (seq x 3 7)) (sequence 5 15))
;-> (8 13 24 44 79 147 275 516 976 1859 3552)

a = 3, b = 8
------------
Sequenza OEIS A054165:
Number of positive integers <= 2^n of form 3*x^2 + 8*y^2.
With: a(0)=0, a(1)=0, a(2)=1, a(3)=2, a(4)=4
  0, 0, 1, 2, 4, 7, 12, 23, 41, 73, 136, 253, 471, 888, 1684, 3211,
  6155, 11831, 22820, 44137, 85539, 166105, 323119, 629511, 1228111,
  2398846, 4690672, 9181509, 17988393, 35272010, 69215936, 135921301,
  267089907, 525160968, 1033179029, ...
(map (fn(x) (seq x 3 8)) (sequence 5 15))
;-> (7 12 23 41 73 136 253 471 888 1684 3211)

a = 3, b = 9
------------
Sequenza OEIS A054166:
Number of positive integers <= 2^n of form 3*x^2 + 9*y^2.
With: a(0)=0, a(1)=0, a(2)=1, a(3)=1, a(4)=3, a(5)=5
  0, 0, 1, 1, 3, 5, 10, 17, 32, 56, 106, 195, 365, 691, 1308, 2495,
  4773, 9174, 17671, 34144, 66131, 128330, 249482, 485761, 947206,
  1849299, 3614605, 7072464, 13851148, 27150649, 53262111, 104562496,
  205412762, 403786919, 794202162, ...
(map (fn(x) (seq x 3 9)) (sequence 6 15))
;-> (10 17 32 56 106 195 365 691 1308 2495)

a = 3, b = 10
-------------
Sequenza OEIS A054167:
Number of positive integers <= 2^n of form 3*x^2 + 10*y^2.
With: a(0)=0, a(1)=0, a(2)=1, a(3)=1, a(4)=4, a(5)=6
  0, 0, 1, 1, 4, 6, 12, 23, 42, 76, 143, 262, 496, 938, 1783, 3397,
  6515, 12523, 24152, 46711, 90543, 175867, 342161, 666712, 1300805,
  2541033, 4969255, 9727606, 19059560, 37374839, 73347038, 144042897,
  283064780, 556601613, 1095087697, ...
(map (fn(x) (seq x 3 10)) (sequence 6 15))
;-> (12 23 42 76 143 262 496 938 1783 3397)

a = 4, b = 5
------------
Sequenza OEIS A054169:
Number of positive integers <= 2^n of form 4*x^2 + 5*y^2.
With: a(0)=0, a(1)=0, a(2)=1, a(3)=2, a(4)=4
  0, 0, 1, 2, 4, 7, 14, 25, 45, 84, 154, 290, 549, 1043, 1990, 3820,
  7352, 14198, 27473, 53327, 103697, 201926, 393781, 769123, 1503776,
  2943595, 5767727, 11310982, 22200249, 43604981, 85706123, 168562900,
  331717805, 653148282, 1286697640, ...
(map (fn(x) (seq x 4 5)) (sequence 5 15))
;-> (7 14 25 45 84 154 290 549 1043 1990 3820)

a = 4, b = 7
------------
Sequenza OEIS A054171:
Number of positive integers <= 2^n of form 4*x^2 + 7*y^2.
With: a(0)=0, a(1)=0, a(2)=1, a(3)=2, a(4)=4
  0, 0, 1, 2, 4, 7, 12, 23, 41, 77, 142, 270, 504, 958, 1834, 3505,
  6752, 13016, 25170, 48775, 94718, 184210, 358805, 699919, 1366853,
  2672452, 5230345, 10245791, 20088113, 39415538, 77394348, 152067981,
  298975703, 588143637, 1157617442, ...
(map (fn(x) (seq x 4 7)) (sequence 5 15))
;-> (7 12 23 41 77 142 270 504 958 1834 3505)

a = 4, b = 9
------------
Sequenza OEIS A054173:
Number of positive integers <= 2^n of form 4*x^2 + 9*y^2.
With: a(0)=0, a(1)=0, a(2)=1, a(3)=1, a(4)=4
  0, 0, 1, 1, 4, 5, 10, 18, 35, 65, 120, 225, 430, 814, 1549, 2981,
  5739, 11084, 21474, 41663, 81033, 157830, 307912, 601482, 1176314,
  2302911, 4513106, 8851907, 17376317, 34134944, 67101922, 131990741,
  259779732, 511566355, 1007901666, ...

(map (fn(x) (seq x 4 9)) (sequence 5 15))
;-> (5 10 18 35 65 120 225 430 814 1549 2981)

a = 5, b = 5
------------
Sequenza OEIS A054175:
Number of positive integers <= 2^n of form 5*x^2 + 5*y^2.
With: a(0)=0, a(1)=0, a(2)=0, a(3)=1, a(4)=2, a(5)=4
  0, 0, 0, 1, 2, 4, 7, 13, 24, 44, 80, 149, 276, 517, 975, 1852,
  3537, 6775, 13027, 25128, 48589, 94151, 182809, 355553, 692591,
  1350973, 2638450, 5158567, 10096032, 19777579, 38775771, 76082812,
  149390879, 293528094, 577088731, 1135234401, ...
(map (fn(x) (seq x 5 5)) (sequence 6 15))
;-> (7 13 24 44 80 149 276 517 975 1852)

a = 5, b = 6
------------
Sequenza OEIS A054176:
Number of positive integers <= 2^n of form 5*x^2 + 6*y^2.
With: a(0)=0, a(1)=0, a(2)=0, a(3)=2, a(4)=3
  0, 0, 0, 2, 3, 7, 12, 22, 42, 78, 141, 264, 494, 936, 1778, 3397,
  6511, 12523, 24149, 46713, 90531, 175865, 342166, 666693, 1300774,
  2541095, 4969243, 9727574, 19059405, 37374922, 73346919, 144042828,
  283064512, 556601031, 1095087423, ...
(map (fn(x) (seq x 5 6)) (sequence 5 15))
;-> (7 12 22 42 78 141 264 494 936 1778 3397)

a = 5, b = 7
------------
Sequenza OEIS A054177:
Number of positive integers <= 2^n of form 5*x^2 + 7*y^2.
With: a(0)=0, a(1)=0, a(2)=0, a(3)=2, a(4)=3
  0, 0, 0, 2, 3, 6, 11, 20, 37, 69, 128, 241, 458, 869, 1666, 3202,
  6183, 11966, 23227, 45149, 87983, 171703, 335520, 656541, 1286071,
  2521752, 4949262, 9721510, 19109550, 37589316, 73985978, 145708602,
  287115841, 566037092, 1116437422, ...
(map (fn(x) (seq x 5 7)) (sequence 5 15))
;-> (6 11 20 37 69 128 241 458 869 1666 3202)

a = 5, b = 8
------------
Sequenza OEIS A054178:
Number of positive integers <= 2^n of form 5*x^2 + 8*y^2.
With: a(0)=0, a(1)=0, a(2)=0, a(3)=2, a(4)=3
  0, 0, 0, 2, 3, 6, 10, 19, 35, 63, 115, 215, 400, 759, 1430, 2735,
  5236, 10058, 19392, 37503, 72686, 141120, 274439, 534633, 1042879,
  2036824, 3982508, 7794887, 15270563, 29941088, 58751556, 115367299,
  226690692, 445709729, 876839450, ...
(map (fn(x) (seq x 5 8)) (sequence 5 15))
;-> (6 10 19 35 63 115 215 400 759 1430 2735)

a = 5, b = 9
------------
Sequenza OEIS A054179:
Number of positive integers <= 2^n of form 5*x^2 + 9*y^2.
With: a(0)=0, a(1)=0, a(2)=0, a(3)=1, a(4)=3, a(5)=5
  0, 0, 0, 1, 3, 5, 10, 18, 32, 60, 110, 207, 391, 739, 1411, 2695,
  5158, 9941, 19187, 37151, 72038, 140000, 272470, 531133, 1036727,
  2025812, 3962913, 7759632, 15207510, 29828260, 58549403, 115004444,
  226040861, 444547031, 874762315, ...
(map (fn(x) (seq x 5 9)) (sequence 5 15))
;-> (5 10 18 32 60 110 207 391 739 1411 2695)

a = 5, b = 10
-------------
Sequenza OEIS A054180:
Number of positive integers <= 2^n of form 5*x^2 + 10*y^2.
With: a(0)=0, a(1)=0, a(2)=0, a(3)=1, a(4)=3, a(5)=5
  0, 0, 0, 1, 3, 5, 9, 16, 29, 52, 93, 169, 315, 591, 1119, 2124,
  4052, 7763, 14922, 28781, 55629, 107775, 209241, 406916, 792579,
  1545812, 3018687, 5901609, 11549484, 22623452, 44352970, 87021370,
  170860941, 335698520, 659971004, 1298228239, ...
(map (fn(x) (seq x 5 10)) (sequence 6 15))
;-> (9 16 29 52 93 169 315 591 1119 2124)

a = 6, b = 7
------------
Sequenza OEIS A054182:
Number of positive integers <= 2^n of form 6*x^2 + 7*y^2.
With: a(0)= 0, a(1)=0, a(2)=0, a(3)=2, a(4)=3
  0, 0, 0, 2, 3, 6, 11, 20, 36, 69, 126, 237, 446, 841, 1595, 3046,
  5840, 11238, 21684, 41955, 81333, 158007, 307378, 598968, 1168767,
  2283307, 4465653, 8742429, 17130723, 33594988, 65933365, 129491817,
  254484590, 500430065, 984625784, ...
(map (fn(x) (seq x 6 7)) (sequence 5 15))
;-> (6 11 20 36 69 126 237 446 841 1595 3046)

a = 6, b = 9
------------
Sequenza OEIS A054184:
Number of positive integers <= 2^n of form 6*x^2 + 9*y^2.
With: a(0)=0, a(1)=0, a(2)=0, a(3)=1, a(4)=3, a(5)=4
  0, 0, 0, 1, 3, 4, 10, 15, 30, 51, 97, 177, 334, 621, 1182, 2243,
  4278, 8203, 15779, 30446, 58882, 114125, 221636, 431155, 840041,
  1638853, 3201255, 6259884, 12253156, 24006205, 47072013, 92370614,
  181390365, 356434492, 700827326, ...
(map (fn(x) (seq x 6 9)) (sequence 6 15))
;-> (10 15 30 51 97 177 334 621 1182 2243)

a = 7, b = 7
------------
Sequenza OEIS A054186:
Number of positive integers <= 2^n of form 7*x^2 + 7*y^2.
With: a(0)=0, a(1)=0, a(2)=0, a(3)=1, a(4)=2, a(5)=3
  0, 0, 0, 1, 2, 3, 6, 11, 18, 33, 60, 110, 204, 381, 717, 1355,
  2582, 4939, 9485, 18264, 35273, 68275, 132440, 257385, 500985,
  976621, 1906221, 3725007, 7286865, 14268275, 27963114, 54846674,
  107656257, 211459271, 415616221, 817365314, ...
(map (fn(x) (seq x 7 7)) (sequence 6 15))
;-> (6 11 18 33 60 110 204 381 717 1355)

a = 7, b = 8
------------
Sequenza OEIS A054187:
Number of positive integers <= 2^n of form 7*x^2 + 8*y^2.
With: a(0)=0, a(1)=0, a(2)=0, a(3)=2, a(4)=3
  0, 0, 0, 2, 3, 5, 9, 17, 30, 57, 105, 199, 373, 701, 1345, 2575,
  4954, 9562, 18509, 35916, 69837, 136010, 265251, 518118, 1013282,
  1983707, 3887580, 7625434, 14969119, 29407328, 57811961, 113724176,
  223843323, 440828855, 868596930, ...
(map (fn(x) (seq x 7 8)) (sequence 5 15))
;-> (5 9 17 30 57 105 199 373 701 1345 2575)

a = 7, b = 9
------------
Sequenza OEIS A054188:
Number of positive integers <= 2^n of form 7*x^2 + 9*y^2.
With: a(0)=0, a(1)=0, a(2)=0, a(3)=1, a(4)=3, a(5)=4
  0, 0, 0, 1, 3, 4, 9, 16, 30, 54, 102, 190, 360, 688, 1313, 2535,
  4889, 9464, 18376, 35741, 69623, 135899, 265500, 519498, 1017399,
  1994552, 3913769, 7685870, 15104369, 29704016, 58451497, 115087453,
  226721774, 446861878, 881163356, ...
(map (fn(x) (seq x 7 9)) (sequence 6 15))
;-> (9 16 30 54 102 190 360 688 1313 2535)

a = 7, b = 10
-------------
Sequenza OEIS A054189:
Number of positive integers <= 2^n of form 7*x^2 + 10*y^2.
With: a(0)=0, a(1)=0, a(2)=0, a(3)=1, a(4)=2, a(5)=4
  0, 0, 0, 1, 2, 4, 8, 16, 28, 56, 104, 197, 369, 699, 1331, 2540,
  4870, 9354, 18066, 34949, 67788, 131656, 256271, 499498, 974856,
  1904908, 3726067, 7295536, 14297372, 28041681, 55040986, 108110861,
  212487489, 417884442, 822287416, ...
(map (fn(x) (seq x 7 10)) (sequence 6 15))
;-> (8 16 28 56 104 197 369 699 1331 2540)

a = 8, b = 9
------------
Sequenza OEIS A054191:
Number of positive integers <= 2^n of form 8*x^2 + 9*y^2.
With: a(0)=0, a(1)=0, a(2)=0, a(3)=1, a(4)=2, a(5)=4
  0, 0, 0, 1, 2, 4, 7, 14, 26, 48, 92, 168, 320, 605, 1157, 2205,
  4235, 8141, 15730, 30427, 59003, 114611, 223037, 434661, 848194,
  1657194, 3241128, 6345672, 12434563, 24386363, 47862135, 94003161,
  184745311, 363303064, 714836493, 1407256329, ...
(map (fn(x) (seq x 8 9)) (sequence 6 15))
;-> (7 14 26 48 92 168 320 605 1157 2205)

a = 9, b = 9
------------
Sequenza OEIS A054193:
Number of positive integers <= 2^n of form 9*x^2 + 9*y^2.
With: a(0)=0, a(1)=0, a(2)=0, a(3)=0, a(4)=1, a(5)=2, a(6)=4
  0, 0, 0, 0, 1, 2, 4, 8, 14, 26, 48, 87, 162, 303, 569, 1076,
  2044, 3903, 7485, 14401, 27778, 53724, 104135, 202242, 393450,
  766556, 1495546, 2921307, 5712535, 11181823, 21907443, 42957050,
  84296463, 165535959, 325281762, 639576071, ...
(map (fn(x) (seq x 9 9)) (sequence 7 15))
;-> (8 14 26 48 87 162 303 569 1076)

a = 9, b = 10
-------------
Sequenza OEIS A054194:
Number of positive integers <= 2^n of form 9*x^2 + 10*y^2.
With: a(0)=0, a(1)=0, a(2)=0, a(3)=0, a(4)=2, a(5)=3
  0, 0, 0, 0, 2, 3, 7, 14, 26, 49, 92, 171, 328, 618, 1182, 2267,
  4375, 8451, 16383, 31797, 61891, 120644, 235516, 460327, 900867,
  1765050, 3461135, 6793369, 13343702, 26229268, 51591736, 101539731,
  199957547, 393974549, 776621316, 1531612324, ...
(map (fn(x) (seq x 9 10)) (sequence 6 15))
;-> (7 14 26 49 92 171 328 618 1182 2267)

a = 1, b = 11
-------------
Sequenza OEIS A054226:
Number of positive integers <= 2^n of form x^2 + 11*y^2.
With: a(0)=1, a(1)=1, a(2)=2, a(3)=2
  1, 1, 2, 2, 7, 10, 19, 34, 60, 112, 214, 400, 761, 1450, 2774, 5341,
  10322, 19992, 38823, 75551, 147250, 287503, 562102, 1100266, 2155940,
  4228715, 8301245, 16309119, 32064447, 63082900, 124181649, 244597440,
  482026958, 950392529, ...
(map (fn(x) (seq x 1 11)) (sequence 4 15))
;-> (7 10 19 34 60 112 214 400 761 1450 2774 5341)

a = 1, b = 13
-------------
Sequenza OEIS A054227:
Number of positive integers <= 2^n of form x^2 + 13*y^2.
With: a(0)=1, a(1)=1, a(2)=2, a(3)=2
  1, 1, 2, 2, 6, 10, 19, 32, 58, 105, 194, 364, 682, 1295, 2467, 4723,
  9059, 17448, 33706, 65256, 126643, 246179, 479336, 934603, 1824646,
  3566467, 6978040, 13666391, 26788334, 52551600, 103168475, 202676349,
  398411711, 783639902, 1542192977, ...
(map (fn(x) (seq x 1 13)) (sequence 4 15))
;-> (6 10 19 32 58 105 194 364 682 1295 2467 4723)

a = 1, b = 14
-------------
Sequenza OEIS A054228:
Number of positive integers <= 2^n of form x^2 + 14*y^2.
With: a(0)=1, a(1)=1, a(2)=2, a(3)=2
  1, 1, 2, 2, 6, 10, 19, 32, 61, 109, 203, 381, 723, 1371, 2623, 5040,
  9711, 18781, 36405, 70710, 137603, 268242, 523655, 1023567, 2002923,
  3923596, 7693084, 15097117, 29649512, 58271080, 114596811, 225504427,
  443998492, 874651263, 1723852015, ...
(map (fn(x) (seq x 1 14)) (sequence 4 15))
;-> (6 10 19 32 61 109 203 381 723 1371 2623 5040)

a = 1, b = 15
-------------
Sequenza OEIS A054229:
Number of positive integers <= 2^n of form x^2 + 15*y^2.
With: a(0)=1, a(1)=1, a(2)=2, a(3)=2
  1, 1, 2, 2, 5, 9, 16, 27, 52, 91, 173, 322, 606, 1152, 2191, 4191,
  8060, 15540, 30018, 58161, 112891, 219479, 427440, 833615, 1627752,
  3182068, 6226916, 12196740, 23910690, 46911434, 92104753, 180957883,
  355748937, 699779680, 1377255678, ...
(map (fn(x) (seq x 1 15)) (sequence 4 15))
;-> (5 9 16 27 52 91 173 322 606 1152 2191 4191)

a = 1, b = 17
-------------
Sequenza OEIS A054230:
Number of positive integers <= 2^n of form x^2 + 17*y^2.
With: a(0)=1, a(1)=1, a(2)=2, a(3)=2
  1, 1, 2, 2, 4, 9, 15, 28, 53, 100, 184, 342, 656, 1243, 2381, 4579,
  8827, 17073, 33108, 64330, 125224, 244171, 476748, 932093, 1824397,
  3574605, 7010170, 13759682, 27027854, 53128077, 104499533, 205667340,
  405000500, 797940280, 1572873981, ...
(map (fn(x) (seq x 1 17)) (sequence 4 15))
;-> (4 9 15 28 53 100 184 342 656 1243 2381 4579)

a = 1, b = 18
-------------
Sequenza OEIS A054231:
Number of positive integers <= 2^n of form x^2 + 18*y^2.
With: a(0)=1, a(1)=1, a(2)=2, a(3)=2
  1, 1, 2, 2, 4, 9, 15, 28, 51, 94, 177, 329, 624, 1180, 2253, 4318,
  8285, 15984, 30872, 59824, 116113, 225769, 439726, 857599, 1674693,
  3274082, 6407312, 12550839, 24605967, 48277606, 94791459, 186243530,
  366154704, 720274798, 1417642821, ...
(map (fn(x) (seq x 1 18)) (sequence 4 15))
;-> (4 9 15 28 51 94 177 329 624 1180 2253 4318)

a = 1, b = 19
-------------
Sequenza OEIS A054232:
Number of positive integers <= 2^n of form x^2 + 19*y^2.
With: a(0)=1, a(1)=1, a(2)=2, a(3)=2
  1, 1, 2, 2, 4, 9, 15, 29, 52, 96, 179, 336, 640, 1218, 2338, 4507,
  8706, 16874, 32808, 63863, 124599, 243437, 476164, 932611, 1828498,
  3588303, 7047954, 13853566, 27250355, 53636267, 105633670, 208154090,
  410381069, 809456768, 1597319534, ...
(map (fn(x) (seq x 1 19)) (sequence 4 15))
;-> (4 9 15 29 52 96 179 336 640 1218 2338 4507)

a = 1, b = 20
-------------
Sequenza OEIS A054233:
Number of positive integers <= 2^n of form x^2 + 20*y^2.
With: a(0)=1, a(1)=1, a(2)=2, a(3)=2
  1, 1, 2, 2, 4, 9, 14, 26, 47, 85, 158, 295, 552, 1048, 2000, 3829,
  7373, 14216, 27505, 53378, 103728, 201981, 393900, 769192, 1503947,
  2943906, 5767973, 11311511, 22200845, 43605974, 85707102, 168564864,
  331720141, 653151962, 1286702079, ...
(map (fn(x) (seq x 1 20)) (sequence 4 15))
;-> (4 9 14 26 47 85 158 295 552 1048 2000 3829)


-----------------------------------------------
Numeri mosaico o proiezione moltiplicativa di N
-----------------------------------------------

Data la scomposizione in fattori primi di un numero N:

  N = p1^k1 * p2^k2 * ... * pm^km

la Proiezione Moltiplicativa di N vale:

  PM = p1*k1 * p2*k2 * ... * pm*km

Sequenza OEIS A000026:
Mosaic numbers or multiplicative projection of n: if n = Product (p_j^k_j) then a(n) = Product (p_j * k_j).
  1, 2, 3, 4, 5, 6, 7, 6, 6, 10, 11, 12, 13, 14, 15, 8, 17, 12, 19, 20, 21,
  22, 23, 18, 10, 26, 9, 28, 29, 30, 31, 10, 33, 34, 35, 24, 37, 38, 39, 30,
  41, 42, 43, 44, 30, 46, 47, 24, 14, 20, 51, 52, 53, 18, 55, 42, 57, 58, 59,
  60, 61, 62, 42, 12, 65, 66, 67, 68, 69, 70, 71, 36, ...

(define (factor-group num)
"Factorize an integer number"
  (if (= num 1) '((1 1))
    (letn ( (fattori (factor num))
            (unici (unique fattori)) )
      (transpose (list unici (count unici fattori))))))

(define (pm n)
  (apply * (flat (factor-group n))))

Proviamo:

(map pm (sequence 1 72))
;-> (1 2 3 4 5 6 7 6 6 10 11 12 13 14 15 8 17 12 19 20 21
;->  22 23 18 10 26 9 28 29 30 31 10 33 34 35 24 37 38 39 30
;->  41 42 43 44 30 46 47 24 14 20 51 52 53 18 55 42 57 58 59
;->  60 61 62 42 12 65 66 67 68 69 70 71 36)


----------------------
Sequenza Dying rabbits
----------------------

La sequenza "Dying rabbits" è una variazione della sequenza di Fibonacci, che incorpora un elemento di "mortalità" per i conigli.
Viene definita come segue:

Per n <= 12 si tratta della sequenza di Fibonacci:

  a(n) = a(n-1) + a(n-2), per n <= 12
  con a(0) = 0 e a(1) = 1.

Per n >= 13 si tiene conto della "morte" dei conigli che hanno raggiunto un'età di 13 mesi, quindi il termine della sequenza è calcolato come:

  a(n) = a(n-1) + a(n-2) - a(n-13), per n >= 13
  dove: a(n-1) + a(n-2) rappresenta la generazione attuale e la precedente che contribuiscono alla popolazione.
        e a(n-13) rappresenta i conigli che muoiono dopo 13 mesi.

Questa sequenza modella una popolazione di conigli dove ogni coppia produce un'altra coppia ogni mese (come nella sequenza di Fibonacci), ma con la differenza che i conigli hanno una vita massima di 13 mesi.
Dopo il 12-esimo mese, si sottrae il numero di conigli che "muoiono" (ossia quelli che sono stati aggiunti alla popolazione 13 mesi fa).

La sequenza introduce un elemento di mortalità costante, rendendo il modello più realistico rispetto al classico modello di Fibonacci., mentre la crescita della sequenza rallenta rispetto a Fibonacci puro, poiché la sottrazione di a(n-13) mitiga l'esplosione esponenziale della popolazione.
Può essere utilizzata per modellare popolazioni con generazioni discrete e durata di vita definita.

Sequenza OEIS A107358:
Dying rabbits: a(n) = Fibonacci(n) for n <= 12, for n >= 13, a(n) = a(n-1) + a(n-2) - a(n-13).
  0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 376, 608, 982, 1587,
  2564, 4143, 6694, 10816, 17476, 28237, 45624, 73717, 119108, 192449,
  310949, 502416, 811778, 1311630, 2119265, 3424201, 5532650, 8939375,
  14443788, 23337539, 37707610, 60926041, 98441202, 159056294, ...

(define (fibo-i num)
"Calculates the Fibonacci number of an integer number"
  (if (zero? num) 0L
  ;else
  (local (a b c)
    (setq a 0L b 1L c 0L)
    (for (i 0 (- num 1))
      (setq c (+ a b))
      (setq a b)
      (setq b c)
    )
    a)))

(define (seq limite)
  (let (arr (array limite '(0)))
    (for (i 0 12)
      (setf (arr i) (fibo-i i)))
    (for (i 13 (- limite 1))
      (setf (arr i) (+ (arr (- i 1)) (arr (- i 2)) (- (arr (- i 13))))))
    arr))

Proviamo:

(seq 42)
;-> (0L 1L 1L 2L 3L 5L 8L 13L 21L 34L 55L 89L 144L 233L 376L 608L 982L 1587L
;->  2564L 4143L 6694L 10816L 17476L 28237L 45624L 73717L 119108L 192449L
;->  310949L 502416L 811778L 1311630L 2119265L 3424201L 5532650L 8939375L
;->  14443788L 23337539L 37707610L 60926041L 98441202L 159056294L)


-------------------------
Primi nella forma k^4 + 1
-------------------------

Sequenza OEIS A037896:
Primes of the form k^4 + 1.
  2, 17, 257, 1297, 65537, 160001, 331777, 614657, 1336337, 4477457,
  5308417, 8503057, 9834497, 29986577, 40960001, 45212177, 59969537,
  65610001, 126247697, 193877777, 303595777, 384160001, 406586897,
  562448657, 655360001, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (pk41 limite)
  (local (out totale k)
    (setq out '())
    (setq totale 0)
    (setq k 0L)
    (until (= totale limite)
      (setq num (+ 1L (* k k k k)))
      (when (prime? num)
        (push num out -1)
        (++ totale))
      (++ k))
    out))

Proviamo:

(pk41 25)
;-> (2L 17L 257L 1297L 65537L 160001L 331777L 614657L 1336337L 4477457L
;->  5308417L 8503057L 9834497L 29986577L 40960001L 45212177L 59969537L
;->  65610001L 126247697L 193877777L 303595777L 384160001L 406586897L
;->  562448657L 655360001L)

Sequenza OEIS A000068:
Numbers k such that k^4 + 1 is prime.
  1, 2, 4, 6, 16, 20, 24, 28, 34, 46, 48, 54, 56, 74, 80, 82, 88, 90, 106,
  118, 132, 140, 142, 154, 160, 164, 174, 180, 194, 198, 204, 210, 220, 228,
  238, 242, 248, 254, 266, 272, 276, 278, 288, 296, 312, 320, 328, 334, 340,
  352, 364, 374, 414, 430, 436, 442, 466, ...


(define (k41 limite)
  (local (out totale k)
    (setq out '())
    (setq totale 0)
    (setq k 0L)
    (until (= totale limite)
      (setq num (+ 1L (* k k k k)))
      (when (prime? num)
        (push k out -1)
        (++ totale))
      (++ k))
    out))

Proviamo:

(k41 57)
;-> (1L 2L 4L 6L 16L 20L 24L 28L 34L 46L 48L 54L 56L 74L 80L 82L 88L 90L 106L
;->  118L 132L 140L 142L 154L 160L 164L 174L 180L 194L 198L 204L 210L 220L 228L
;->  238L 242L 248L 254L 266L 272L 276L 278L 288L 296L 312L 320L 328L 334L 340L
;->  352L 364L 374L 414L 430L 436L 442L 466L)

Sequenza OEIS A000059:
Numbers k such that (2k)^4 + 1 is prime.
  1, 2, 3, 8, 10, 12, 14, 17, 23, 24, 27, 28, 37, 40, 41, 44, 45, 53, 59,
  66, 70, 71, 77, 80, 82, 87, 90, 97, 99, 102, 105, 110, 114, 119, 121, 124,
  127, 133, 136, 138, 139, 144, 148, 156, 160, 164, 167, 170, 176, 182, 187,
  207, 215, 218, 221, 233, 236, 238, 244, 246, ...

(define (k241 limite)
  (local (out totale k k2)
    (setq out '())
    (setq totale 0)
    (setq k 0L)
    (until (= totale limite)
      (setq k2 (* k 2L))
      (setq num (+ 1L (* k2 k2 k2 k2)))
      (when (prime? num)
        (push k out -1)
        (++ totale))
      (++ k))
    out))

Proviamo:

(k241 60)
;-> (1L 2L 3L 8L 10L 12L 14L 17L 23L 24L 27L 28L 37L 40L 41L 44L 45L 53L 59L
;->  66L 70L 71L 77L 80L 82L 87L 90L 97L 99L 102L 105L 110L 114L 119L 121L 124L
;->  127L 133L 136L 138L 139L 144L 148L 156L 160L 164L 167L 170L 176L 182L 187L
;->  207L 215L 218L 221L 233L 236L 238L 244L 246L)


---------------
Sequenza Beatty
---------------

La sequenza Beatty è una sequenza di interi derivati dalla funzione floor dei multipli di un numero irrazionale.
Viene definita nel modo seguente:

  a(n) = floor(n/(e - 2))

dove "e" è il numero di Eulero 2.7182818284590451...

Sequenza OEIS A000062:
A Beatty sequence: a(n) = floor(n/(e-2)).
  1, 2, 4, 5, 6, 8, 9, 11, 12, 13, 15, 16, 18, 19, 20, 22, 23, 25, 26, 27,
  29, 30, 32, 33, 34, 36, 37, 38, 40, 41, 43, 44, 45, 47, 48, 50, 51, 52,
  54, 55, 57, 58, 59, 61, 62, 64, 65, 66, 68, 69, 71, 72, 73, 75, 76, 77,
  79, 80, 82, 83, 84, 86, 87, 89, 90, 91, 93, 94, 96, 97, 98, ...

(define (beatty limite)
  (let (costante (div 1 (sub (exp 1) 2)))
    (map (fn (x) (floor (mul x costante))) (sequence 1 limite))))

Proviamo:

(beatty 71)
;-> (1 2 4 5 6 8 9 11 12 13 15 16 18 19 20 22 23 25 26 27
;->  29 30 32 33 34 36 37 38 40 41 43 44 45 47 48 50 51 52
;->  54 55 57 58 59 61 62 64 65 66 68 69 71 72 73 75 76 77
;->  79 80 82 83 84 86 87 89 90 91 93 94 96 97 98)


--------------
Numeri di Pell
--------------

I numeri di Pell, chiamati anche numeri lambda, sono una sequenza di interi definita nel modo seguente:

  a(0) = 0
  a(1) = 1
  a(n) = 2*a(n-1) + a(n-2), per n > 1.

Sequenza OEIS A000129:
Pell numbers: a(0) = 0, a(1) = 1, for n > 1, a(n) = 2*a(n-1) + a(n-2).
  0, 1, 2, 5, 12, 29, 70, 169, 408, 985, 2378, 5741, 13860, 33461, 80782,
  195025, 470832, 1136689, 2744210, 6625109, 15994428, 38613965, 93222358,
  225058681, 543339720, 1311738121, 3166815962, 7645370045, 18457556052,
  44560482149, 107578520350, 259717522849, ...

(define (pell limite)
  (let (arr (array limite '(0)))
    (setf (arr 0) 0)
    (setf (arr 1) 1)
    (for (i 2 (- limite 1))
      (setf (arr i) (+ (* 2 (arr (- i 1))) (arr (- i 2)))))
    arr))

Proviamo:

(pell 32)
;-> (0 1 2 5 12 29 70 169 408 985 2378 5741 13860 33461 80782
;->  195025 470832 1136689 2744210 6625109 15994428 38613965 93222358
;->  225058681 543339720 1311738121 3166815962 7645370045 18457556052
;->  44560482149 107578520350 259717522849)


------------------------------------------
Number of Boolean functions of n variables
------------------------------------------

"Number of Boolean functions of n variables" si riferisce al conteggio totale delle funzioni che possono essere definite su n variabili booleane.

Spiegazione:

1. Variabili booleane: Ogni variabile può assumere due valori: 0 (falso) o 1 (vero).

2. Definizione di una funzione booleana: Una funzione booleana f su n variabili mappa ogni possibile combinazione di n variabili booleane a un risultato booleano (0 o 1).
In simboli:

   f:[0, 1]^n to [0, 1]

3. Numero totale di combinazioni: Se abbiamo n variabili, il numero totale di combinazioni dei valori delle variabili è 2^n (perché ogni variabile ha due stati possibili).

4. Possibili funzioni: Per ciascuna delle 2^n combinazioni, il risultato della funzione può essere scelto arbitrariamente come 0 o 1. Questo porta a 2^(2^n) funzioni booleane totali, perché ogni combinazione può essere mappata a due valori (0 o 1).

Esempi:
Per n = 1: Le variabili sono x, con due combinazioni (0) e (1). Puoi definire 2^(2^1) = 4 funzioni:
  1. f(x) = 0
  2. f(x) = 1
  3. f(x) = x (identità)
  4. f(x) = not(x) (negazione)

Per n = 2: Le variabili sono x e y, con quattro combinazioni: (0,0), (0,1), (1,0), (1,1). Il numero totale di funzioni è 2^(2^2) = 16.

Questo ragionamento non considera le simmetrie tra le variabili.
La formula seguente per il numero di funzioni booleane non isomorfe (o equivalenti) di n variabili tiene conto delle simmetrie tra le variabili (In altre parole, due funzioni sono considerate equivalenti se si possono ottenere una dall'altra semplicemente riordinando o rinominando le variabili):

               a                  b                c
            -------    ----------------------  -------
  seq(n) = (2^(2^n) + (2^n-1) * 2^(2^(n-1)+1))/2^(n+1).

Analisi dei termini:

Termine a: 2^(2^n)
Questo è il numero totale di funzioni booleane su n variabili, senza considerare l'isomorfismo.

Termine b: (2^n - 1) * 2^(2^(n-1) + 1):
Questo termine aggiustativo considera il numero di funzioni che non sono uniche a causa delle simmetrie. Le simmetrie derivano dal fatto che permutazioni delle variabili non cambiano alcune funzioni.

Termine c: Divisione per 2^(n+1)
Normalizza il risultato dividendo per il numero di permutazioni delle variabili e altre trasformazioni che preservano l'isomorfismo.

Esempio per n = 2:
Passo 1:
Calcola 2^(2^2) = 2^4 = 16
Questo è il numero totale di funzioni booleane su due variabili.

Passo 2:
Calcola (2^2 - 1) * 2^(2^(2-1) + 1) = 24

Passo 3:
Somma e dividi per 2^(n+1) = 8:
a(2) = (16 + 24)/8 = 40/8 = 5

Per n = 2, il numero di funzioni booleane non isomorfe è 5, che corrisponde alle seguenti classi di funzioni:
1. Costante 0 (sempre falso).
2. Costante 1 (sempre vero).
3. Identità di una variabile (x o y).
4. Negazione di una variabile (not(x) o not(y)).
5. XOR (x xor y) o equivalenti.

Sequenza OEIS A000133:
Number of Boolean functions of n variables.
  2, 5, 30, 2288, 67172352, 144115192303714304,
  1329227995784915891206435945914040320,
  226156424291633194186662080095093570364871077725232774230036394136943198208,

(define (** num power)
"Calculates the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(define (seq n)
  (setq a (** 2 (** 2 n)))
  (setq b (* (- (** 2 n) 1) (** 2 (+ (** 2 (- n 1)) 1))))
  (setq c (** 2 (+ n 1)))
  (/ (+ a b) c))

Proviamo:

(seq 2)
;-> 5L

(map seq (sequence 1 8))
;-> (2L 5L 30L 2288L 67172352L 144115192303714304L
;->  1329227995784915891206435945914040320L
;->  22615642429163319418666208009509357036487107
;->  7725232774230036394136943198208L)


---------------------------------
Paradosso logico autoreferenziale
---------------------------------

Un paradosso logico autoreferenziale è una domanda o più in generale un'affermazione che fa riferimento a se
stessa in modo tale da generare una contraddizione chiamata appunto un paradosso logico autoreferenziale.

Uno dei paradossi più famosi è l'affermazione: "Questa frase è falsa".

Vediamo un altro esempio di paradosso logico autoreferenziale.

Supponiamo di avere una domanda scelta multipla (quindi qui al posto degli underscore "_"
c'è una qualsiasi domanda) con quattro risposte:

Domanda a scelta multipla: _____________________________
Risposte:
A) ________
B) ________
C) ________
D) ________

Possiamo avere i seguenti casi:

1) Una sola risposta è corretta
2) Più di una risposta corretta (anche tutte)
3) Nessuna risposta è corretta

Adesso poniamo che la "domanda a scelta multipla" (con le relative risposte) sia la seguente:

Se rispondiamo a caso a questa domanda, con quale probabilità la risposta sarà corretta?
A) 25%
B) 50%
C) 25%
D) 0%

Immaginiamo di scegliere una risposta a caso (ad esempio scegliendo una pallina da un'urna che ne contiene quattro)
ed analizziamo una per una le varie risposte:

Poichè ci sono quattro opzioni e noi scegliamo a caso abbiamo una possibilità su quattro di indovinare, la risposta spontanea è il 25%. Comunque il 25% compare in due risposte (A e C), quindi la risposta è il 50%.

Ma se la risposta è 50%, non è più vero che abbiamo una possibilità su quattro di indovinare. Quindi neanche il 50% è la risposta corretta.

Adesso rimane solo una opzione, la risposta potrebbe essere 0%.
Ma se la risposta è 0% allora vuol dire che è impossibile rispondere correttamente, invece se scegliamo risposta D vuol dire che abbiamo risposto correttamente e quindi la risposta non può essere 0%.

Quindi sembra che tutte le risposte siano sbagliate, ma se tutte le risposte fossero sbagliate, ad esempio se la risposta corretta fosse un'altra (per esempio 33%), allora la probabilità di rispondere correttamente sarebbe dello 0% perché se
la risposta giusta non esiste, allora questo vale 0% e quindi anche l'ultima riposta è sbagliata.

Alla fine del ragionamento risulta:
1) non è vero che c'è una risposta giusta
2) non è vero che ce n'è più di una
3) non è  vero che sono tutte sbagliate quello che avete visto è

Questo è un paradosso logico autoreferenziale.

Nota: la risposta corretta non è una qualunque delle quattro proposte, ma è intrinsecamente legata alla nostra scelta (in modo logicamente incoerente).

Nota:
Il paradosso "questa frase è falsa" è un esempio classico di autoreferenza che genera un problema di coerenza logica. Questo paradosso mette in evidenza i limiti del linguaggio e della logica tradizionale quando si tratta di affermazioni autoreferenziali. 

La difficoltà nasce dal fatto che:
1. Se l'affermazione è vera, allora ciò che dichiara ("questa frase è falsa") deve essere vero, il che implica che è falsa. Contraddizione.
2. Se l'affermazione è falsa, allora ciò che dichiara non è vero, il che implica che è vera. Di nuovo una contraddizione.

Il problema si pone perché l'affermazione tenta di definire il proprio stato di verità.
Quando un sistema logico include affermazioni che si riferiscono a se stesse, si entra in un dominio di meta-livelli che spesso richiede strumenti più sofisticati per essere compreso o modellato.

Tra le soluzioni proposte:
a) Teoria dei tipi di Russell
Bertrand Russell propose di evitare i paradossi autoreferenziali vietando che una dichiarazione possa parlare di se stessa direttamente.
b) Logica paraconsistente
Alcuni sistemi logici permettono che una proposizione possa essere sia vera che falsa contemporaneamente (superando il principio di non contraddizione).
c) Gerarchia semantica di Tarski
Alfred Tarski introdusse l'idea che la verità può essere definita solo in un meta-linguaggio, mai nello stesso linguaggio in cui l'affermazione è espressa.

In definitiva, il paradosso sottolinea che il meta-ragionamento (ragionare sulle regole del proprio ragionamento) richiede strumenti logici o filosofici specifici per evitare incoerenze.


----------------
Numeri di Franel
----------------

I numeri Franel hanno la seguente definizione:

  F(a) = Sum[k=0..n] binomial(n,k)^3.

Sequenza OEIS A000172:
The Franel number a(n) = Sum[k=0..n] binomial(n,k)^3.
  1, 2, 10, 56, 346, 2252, 15184, 104960, 739162, 5280932, 38165260,
  278415920, 2046924400, 15148345760, 112738423360, 843126957056,
  6332299624282, 47737325577620, 361077477684436, 2739270870994736,
  20836827035351596, 158883473753259752, 1214171997616258240, ...

(define (** num power)
"Calculates the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(define (binom num k)
"Calculates the binomial coefficient (n k) = n!/(k!*(n - k)!) (combinations of k elements without repetition from n elements)"
  (cond ((> k num) 0L)
        ((zero? k) 1L)
        ((< k 0) 0L)
        (true
          (let (r 1L)
            (for (d 1 k)
              (setq r (/ (* r num) d))
              (-- num))
          r))))

(define (franel n)
  (let (somma 0)
    (for (k 0 n) (++ somma (** (binom n k) 3)))
    somma))

(map franel (sequence 0 22))
;-> (1 2 10 56 346 2252 15184 104960 739162 5280932 38165260
;->  278415920 2046924400 15148345760 112738423360 843126957056
;->  6332299624282 47737325577620 361077477684436 2739270870994736
;->  20836827035351596 158883473753259752 1214171997616258240)


------------------------------
Somma del quadrato delle cifre
------------------------------

Dato un numero intero N con x cifre, la somma del quadrato delle cifre vale:

  SQC = x(1)^2 + x(2)^2 + ... + x(n)^2

Sequenza OEIS A000216:
Take sum of squares of digits of previous term, starting with 2.
  2, 4, 16, 37, 58, 89, 145, 42, 20, 4, 16, 37, 58, 89, 145, 42, 20, 4,
  16, 37, 58, 89, 145, 42, 20, 4, 16, 37, 58, 89, 145, 42, 20, 4, 16,
  37, 58, 89, 145, 42, 20, 4, 16, 37, 58, 89, 145, 42, 20, 4, 16, 37,
  58, 89, 145, 42, 20, 4, 16, 37, 58, 89, 145, 42, 20, 4, 16, 37, ...

(define (sqc num)
  (let (out '())
    (while (!= num 0)
      (push (* (% num 10) (% num 10)) out)
      (setq num (/ num 10)))
    (apply + out)))

(sqc 2)
;-> 4
(sqc 123)
;-> 14

(define (seq limite)
  (let ( (val 2) (out (list 2)) )
    (for (i 2 limite)
      (setq val (sqc val))
      (push val out -1))
    out))

Proviamo:

(seq 76)
;-> (2 4 16 37 58 89 145 42 20 4 16 37 58 89 145 42 20 4
;->  16 37 58 89 145 42 20 4 16 37 58 89 145 42 20 4 16
;->  37 58 89 145 42 20 4 16 37 58 89 145 42 20 4 16 37
;->  58 89 145 42 20 4 16 37 58 89 145 42 20 4 16 37 58
;->  89 145 42 20 4 16 37)

La sequenza è periodica con perido: 4 16 37 58 89 145 42 20.


----------------------------
Parte dispari di un numero N
----------------------------

La parte dispari di un numero si ottiene eliminando tutti i fattori 2 dalla scomposizione in fattori primi di N.

Sequenza OEIS A000265:
Remove all factors of 2 from n, or largest odd divisor of n, or odd part of n.
With a(1) = 1, a(2) = 1, a(3)=3
  1, 1, 3, 1, 5, 3, 7, 1, 9, 5, 11, 3, 13, 7, 15, 1, 17, 9, 19, 5, 21, 11,
  23, 3, 25, 13, 27, 7, 29, 15, 31, 1, 33, 17, 35, 9, 37, 19, 39, 5, 41, 21,
  43, 11, 45, 23, 47, 3, 49, 25, 51, 13, 53, 27, 55, 7, 57, 29, 59, 15, 61,
  31, 63, 1, 65, 33, 67, 17, 69, 35, 71, 9, 73, 37, 75, 19, 77, ...

(define (dispari num)
  (apply * (clean (fn(x) (= x 2)) (factor num))))

(map dispari (sequence 2 77))
;-> (1 3 1 5 3 7 1 9 5 11 3 13 7 15 1 17 9 19 5 21 11 23 3 25 13 27 7 29
;->  15 31 1 33 17 35 9 37 19 39 5 41 21 43 11 45 23 47 3 49 25 51 13 53
;->  27 55 7 57 29 59 15 61 31 63 1 65 33 67 17 69 35 71 9 73 37 75 19 77)

La sequenza può essere definita anche nel modo seguente:

  a(1) = 1
  a(n) = n, se n è dispari (n > 1)
  a(n) = a(n/2), se n è pari (n > 1)

(define (seq-dispari limite)
  (let (arr (array (+ limite 1) '(0)))
    (setf (arr 1) 1)
    (for (i 2 limite)
      (if (even? i)
          (setf (arr i) (arr (/ i 2)))
          (setf (arr i) i)))
    (slice arr 1)))

(seq-dispari 77)
;-> (1 1 3 1 5 3 7 1 9 5 11 3 13 7 15 1 17 9 19 5 21 11 23 3 25 13 27 7 29
;->  15 31 1 33 17 35 9 37 19 39 5 41 21 43 11 45 23 47 3 49 25 51 13 53
;-> 27 55 7 57 29 59 15 61 31 63 1 65 33 67 17 69 35 71 9 73 37 75 19 77)

============================================================================

