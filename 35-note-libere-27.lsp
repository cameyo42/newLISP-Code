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
    (div (abs (add (mul x1 (- y2 y3))
                   (mul x2 (- y3 y1))
                   (mul x3 (- y1 y2)))) 2)))

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

  0 rappresenta una cella vuota.
  1 rappresenta una cella che contiene una cellula sana.
  2 rappresenta una cella che contiene una cellula contaminata.

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
    (setq Ni (- i 1)) (setq Nj j)
    (setq Ei i)      (setq Ej (+ j 1))
    (setq Si (+ i 1)) (setq Sj j)
    (setq Oi i)      (setq Oj (- j 1))
    (when (= (grid i j) 2) ; se la cella corrente è contaminata
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
      (when verbose (print-matrix grid) (read-line))
    )
    ; conta la frequenza delle tipologie delle celle (0, 1 e 2)
    (setq conta (count '(0 1 2) (flat grid)))
    (println "Tempo: " minuti " minuti")
    (println "Celle vuote: " (conta 0))
    (println "Celle sane: " (conta 1))
    (println "Celle contaminate: " (conta 2))
    (print-matrix grid)))

Proviamo:

(setq g '((2 1 1) (1 1 0) (0 1 1)))
;-> Tempo: 4 minuti
;-> Celle vuote: 2
;-> Celle sane: 0
;-> Celle contaminate: 7
;->   2  2  2
;->   2  2  0
;->   0  2  2

(setq g '((2 1 1) (1 1 0) (0 0 1)))
(virus-spread g)
;-> Tempo: 2 minuti
;-> Celle vuote: 3
;-> Celle sane: 1
;-> Celle contaminate: 5
;->   2  2  2
;->   2  2  0
;->   0  0  1

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

============================================================================

