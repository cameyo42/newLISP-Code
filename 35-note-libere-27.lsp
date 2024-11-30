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

Alcuni numeri hanno più di una scomposizione (es. 4, 12), mntre altri hanno una scomposizione unica (es. 5, 14, 15)

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

============================================================================

