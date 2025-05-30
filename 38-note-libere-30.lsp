================

 NOTE LIBERE 30

================

  "Discuss the idea if you want, not the human behind it."

-----------------
Aritmetica Lunare
-----------------

https://www.numberphile.com/videos/lunar-arithmetic

L'addizione e la moltiplicazione lunari funzionano cifra per cifra, come quelle terrestri, con la differenza che le tabelline del più e del più sono insolite.
L'addizione si esegue prendendo la maggiore delle due cifre, quindi 1 + 3 = 3 e 7 + 4 = 7.
La moltiplicazione si esegue prendendo la minore delle due cifre, quindi 1 * 3 = 1 e 7 * 4 = 4.
Quindi:

      3 5 7 +       3 5 7 *
        6 4 =         6 4 =
     ---------    ---------
      3 6 7         3 4 4
                  3 5 6
                  -------
                  3 5 6 4

Per l'addizione e la moltiplicazione vale la proprietà commutativa:
a + b = b + a
a * b = b * a
Non ci sono riporti.
Non ci sono sottrazioni o divisioni, poiché i risultati non sarebbero univoci.

Scriviamo due funzioni cha implementano l'addizione e la moltiplicazione lunare.

(define (int-list num)
"Convert an integer to a list of digits"
  (if (zero? num) '(0)
  (let (out '())
    (while (!= num 0)
      (push (% num 10) out)
      (setq num (/ num 10))) out)))

(define (list-int lst)
"Convert a list of digits to integer"
  (let (num 0)
    (dolist (el lst) (setq num (+ el (* num 10))))))

Funzione che calcola la somma lunare di due numeri:

(define (lunar-add a b)
  (let ( (len-a (length a)) (len-b (length b)) )
    ; converte i numeri in liste di cifre
    (setq a (int-list a))
    (setq b (int-list b))
    ; rende le liste di lunghezza uguale (add 0 in front)
    (while (> len-a len-b) (push 0 b) (++ len-b))
    (while (< len-a len-b) (push 0 a) (++ len-a))
    ; applica il minimo a tutte le coppie di cifre e
    ; converte la lista di cifre in numero
    (list-int (map max a b))))

Proviamo:

(lunar-add 357 64)
;-> 367
(lunar-add 57 64)
;-> 67
(lunar-add 11 11)
;-> 11

(lunar-add (lunar-add 357 64) 129)
;-> 369

Macro che calcola la somma lunare di due o più numeri:

(define-macro (luadd)
  (apply lunar-add (map eval (args)) 2))

(luadd 357 64 129)
;-> 369

Funzione che calcola la somma lunare di due numeri:

(define (lunar-mul a b)
  (let ( (result 0) (cur '()) )
    (setq a (int-list a))
    (setq b (int-list b))
    ; moltiplica ogni cifra del secondo numero
    ; per tutte le cifre del primo numero
    ; e aggiorna il risultato
    (dolist (el (reverse b))
      ; moltiplicazione
      (setq cur (map (fn(x) (min x el)) a))
      ;(println cur)
      ; padding con 0 iniziali
      (extend cur (dup 0 $idx))
      ; aggiornamento somma lunare parziale
      (setq result (lunar-add result (list-int cur))))
    result))

Proviamo:

(lunar-mul 357 64)
;-> 3564
(lunar-mul 64 357)
;-> 3564

(lunar-mul (lunar-mul 64 357) 21)
;-> 22221
(lunar-mul (lunar-mul 21 64) 357)
;-> 22221

Macro che calcola la moltiplicazione lunare di due o più numeri:

(define-macro (lumul)
  (apply lunar-mul (map eval (args)) 2))

(lumul 64 357 21)
;-> 22221
(lumul 21 64 357)
;-> 22221

Tavola pitagorica delle somme lunari
------------------------------------

(define (pitagora-lunar-add)
  (print "\n    ·")
  (for (i 0 9) (print (format "%4d" i)))
  (println (format "\n%s" (dup "·" 46)))
  (for (i 0 9)
    (print (format "%3d%s" i " ·"))
    (for (j 0 9)
      (print (format "%4d" (lunar-add i j))))
    (println "\n    ·")) '>)

(pitagora-lunar-add)
;->     ·   0   1   2   3   4   5   6   7   8   9
;-> ··············································
;->   0 ·   0   1   2   3   4   5   6   7   8   9
;->     ·
;->   1 ·   1   1   2   3   4   5   6   7   8   9
;->     ·
;->   2 ·   2   2   2   3   4   5   6   7   8   9
;->     ·
;->   3 ·   3   3   3   3   4   5   6   7   8   9
;->     ·
;->   4 ·   4   4   4   4   4   5   6   7   8   9
;->     ·
;->   5 ·   5   5   5   5   5   5   6   7   8   9
;->     ·
;->   6 ·   6   6   6   6   6   6   6   7   8   9
;->     ·
;->   7 ·   7   7   7   7   7   7   7   7   8   9
;->     ·
;->   8 ·   8   8   8   8   8   8   8   8   8   9
;->     ·
;->   9 ·   9   9   9   9   9   9   9   9   9   9

Tavola pitagorica delle moltiplicazioni lunari
----------------------------------------------

(define (pitagora-lunar-mul)
  (print "\n    ·")
  (for (i 0 9) (print (format "%4d" i)))
  (println (format "\n%s" (dup "·" 46)))
  (for (i 0 9)
    (print (format "%3d%s" i " ·"))
    (for (j 0 9)
      (print (format "%4d" (lunar-mul i j))))
    (println "\n    ·")) '>)

(pitagora-lunar-mul)
;->     ·   0   1   2   3   4   5   6   7   8   9
;-> ··············································
;->   0 ·   0   0   0   0   0   0   0   0   0   0
;->     ·
;->   1 ·   0   1   1   1   1   1   1   1   1   1
;->     ·
;->   2 ·   0   1   2   2   2   2   2   2   2   2
;->     ·
;->   3 ·   0   1   2   3   3   3   3   3   3   3
;->     ·
;->   4 ·   0   1   2   3   4   4   4   4   4   4
;->     ·
;->   5 ·   0   1   2   3   4   5   5   5   5   5
;->     ·
;->   6 ·   0   1   2   3   4   5   6   6   6   6
;->     ·
;->   7 ·   0   1   2   3   4   5   6   7   7   7
;->     ·
;->   8 ·   0   1   2   3   4   5   6   7   8   8
;->     ·
;->   9 ·   0   1   2   3   4   5   6   7   8   9

Quadrati lunari
---------------

(Sequenza OEIS A087019:
Lunar squares: n*n where * is lunar multiplication (A087062).
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 100, 111, 112, 113, 114, 115, 116, 117,
  118, 119, 200, 211, 222, 223, 224, 225, 226, 227, 228, 229, 300, 311,
  322, 333, 334, 335, 336, 337, 338, 339, 400, 411, 422, 433, 444, 445,
  446, 447, 448, 449, 500, 511, 522, 533, 544, 555, 556, 557, 558, ...

(define (lunar-square n) (lunar-mul n n))

(map lunar-square (sequence 0 58))
;-> (0 1 2 3 4 5 6 7 8 9 100 111 112 113 114 115 116 117
;->  118 119 200 211 222 223 224 225 226 227 228 229 300 311
;->  322 333 334 335 336 337 338 339 400 411 422 433 444 445
;->  446 447 448 449 500 511 522 533 544 555 556 557 558)

Quali moltiplicazioni producono lo stesso risultato nell'aritmetica lunare e in quella terrestre?

(setq out '())
(for (a 1 1000)
  (for (b 1 1000)
    (if (= (* a b) (lunar-mul a b)) (push (list a b) out -1))))
;-> ((1 1) (1 10) (1 11) (1 100) (10 1) (10 10) (10 11) (10 100) (11 1)
;->  (11 10) (11 100) (100 1) (100 10) (100 11) (100 100))
Quindi alcuni numeri composti solo da 1 e 0 moltiplicati tra loro producono lo stesso risultato.

Quali addizioni producono lo stesso risultato nell'aritmetica lunare e in quella terrestre?

(setq out '())
(for (a 1 10)
  (for (b 1 10)
    (if (= (* a b) (lunar-add a b)) (push (list a b) out -1))))
out
;-> ((1 1) (1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (1 8) (1 9) (2 1)
;->  (3 1) (4 1) (5 1) (6 1) (7 1) (8 1) (9 1))
Notiamo che quando uno degli addendi vale 1, allora l'addizione lunare e quelle terrestre producono lo stesso risultato.

Numeri primi lunari
-------------------

Nell'aritmetica terrestre l'elemento neutro della moltiplicazione vale 1:

  n * 1 = 1 * n = n

Nell'aritmetica lunare l'elemento neutro della moltiplicazione vale 9:

  n * 9 = 9 * n = n

(map (curry lunar-mul 9) (sequence 0 20))
;-> (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)

I primi lunari sono quei numeri la cui fattorizzazione vale:

  N = N * 9

Un numero N è un primo lunare se:
Non esistono due numeri a, b >= 2 (escludendo 1 e 9), tali che lunar-mul(a, b) = n.

Sequenza OEIS A087097:
Lunar primes (formerly called dismal primes) (cf. A087062).
  19, 29, 39, 49, 59, 69, 79, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,
  99, 109, 209, 219, 309, 319, 329, 409, 419, 429, 439, 509, 519, 529,
  539, 549, 609, 619, 629, 639, 649, 659, 709, 719, 729, 739, 749, 759,
  769, 809, 819, 829, 839, 849, 859, 869, 879, 901, 902, 903, 904, 905,
  906, 907, 908, 909, 912, 913, 914, 915, 916, 917, 918, 919, 923, 924,
  925, 926, 927, 928, 929, 934, 935, 936, 937, 938, 939, 945, 946, 947,
  948, 949, 956, 957, 958, 959, 967, 968, 969, 978, 979, 989, ...

(define (lunar-primo? n)
  ; Supponiamo inizialmente che n sia primo lunare
  (setq trovato nil)
  ; Cerchiamo due numeri a e b >= 1 tali che (lunar-mul a b) = n
  ; Escludiamo la moltiplicazione con 9 (che è l'identità lunare)
  (for (a 1 n 1 trovato)            ; 'a' va da 1 fino a n-1
    (for (b 1 a 1 trovato)          ; 'b' va da 1 fino ad 'a' (così b <= a, evitando duplicati)
      (unless (or (= a 9) (= b 9))  ; salta le moltiplicazioni che coinvolgono 9
        (if (= (lunar-mul a b) n)   ; se troviamo una fattorizzazione lunare diversa da 9
            (set 'trovato true))))) ; allora n non è primo
  ; Se non abbiamo trovato alcuna fattorizzazione: è primo lunare
  (not trovato))

(define (lunar-primi limite)
  (filter lunar-primo? (sequence 10 limite)))

(time (println (lunar-primi 100)))
;-> (19 29 39 49 59 69 79 89 90 91 92 93 94 95 96 97 98 99)
;-> 841.761

(time (println (lunar-primi 200)))
;-> (19 29 39 49 59 69 79 89 90 91 92 93 94 95 96 97 98 99 109)
;-> 1943.657

(time (println (lunar-primi 400)))
;-> (19 29 39 49 59 69 79 89 90 91 92 93 94 95 96 97 98 99 109
;->  209 219 309 319 329)
;-> 16557.071

(time (println (lunar-primi 1000)))
;-> (19 29 39 49 59 69 79 89 90 91 92 93 94 95 96 97 98
;->  99 109 209 219 309 319 329 409 419 429 439 509 519 529
;->  539 549 609 619 629 639 649 659 709 719 729 739 749 759
;->  769 809 819 829 839 849 859 869 879 901 902 903 904 905
;->  906 907 908 909 912 913 914 915 916 917 918 919 923 924
;->  925 926 927 928 929 934 935 936 937 938 939, ...
;->  945 946 947 948 949 956 957 958 959 967 968 969 978 979 989)
;-> 626574.289

Fattorizzazione lunare
----------------------

Per fattorizzare un numero lunare dobbiamo trovare tutte le coppie (a b) tali che lunar-mul a b = n, escludendo i casi banali con 9, dato che:
  (lunar-mul a 9) = a ; 9 è l'elemento neutro per la moltiplicazione

(define (lunar-fattorizzazioni n)
  (let (fattori '())
    ; 'a' va da 1 a n
    (for (a 1 n)
      ; 'b' va da 1 a 'a', così evitiamo duplicati simmetrici
      (for (b 1 a)
        ; escludiamo moltiplicazioni con 9,
        ; che non contano come fattorizzazioni reali
        (unless (or (= a 9) (= b 9))
          (when (= (lunar-mul a b) n)
            (push (list a b) fattori -1)))))
    fattori))

(setq fattori (map list (sequence 1 30)
                        (map lunar-fattorizzazioni (sequence 1 30))))
;-> ((1 ((1 1))) (2 ((2 2))) (3 ((3 3))) (4 ((4 4))) (5 ((5 5)))
;->  (6 ((6 6))) (7 ((7 7))) (8 ((8 8)))
;->  (10 ((10 1) (10 2) (10 3) (10 4) (10 5) (10 6) (10 7) (10 8)))
;->  (11 ((11 1) (11 2) (11 3) (11 4) (11 5) (11 6) (11 7) (11 8)))
;->  (12 ((12 2) (12 3) (12 4) (12 5) (12 6) (12 7) (12 8)))
;->  (13 ((13 3) (13 4) (13 5) (13 6) (13 7) (13 8)))
;->  (14 ((14 4) (14 5) (14 6) (14 7) (14 8)))
;->  (15 ((15 5) (15 6) (15 7) (15 8)))
;->  (16 ((16 6) (16 7) (16 8)))
;->  (17 ((17 7) (17 8)))
;->  (18 ((18 8)))
;->  (20 ((20 2) (20 3) (20 4) (20 5) (20 6) (20 7) (20 8)))
;->  (21 ((21 2) (21 3) (21 4) (21 5) (21 6) (21 7) (21 8)))
;->  (22 ((22 2) (22 3) (22 4) (22 5) (22 6) (22 7) (22 8)))
;->  (23 ((23 3) (23 4) (23 5) (23 6) (23 7) (23 8)))
;->  (24 ((24 4) (24 5) (24 6) (24 7) (24 8)))
;->  (25 ((25 5) (25 6) (25 7) (25 8)))
;->  (26 ((26 6) (26 7) (26 8)))
;->  (27 ((27 7) (27 8)))
;->  (28 ((28 8)))
;->  (30 ((30 3) (30 4) (30 5) (30 6) (30 7) (30 8))))

I numeri primi sono accoppiati con la lista vuota:

(filter (fn(x) (= (last x) '())) fattori)
;-> ((9 ()) (19 ()) (29 ()))

Nell'aritmetica lunare la fattorizzazione non è unica.
Questo perchè a differenza dell'aritmetica terrestre, la moltiplicazione lunare è basata sul minimo delle cifre, quindi lo stesso numero può essere ottenuto con molteplici coppie diverse:
(lunar-mul 27 31)
;-> 231
(lunar-mul 21 34)
;-> 221

Fattoriali lunari
-----------------

Sequenza OEIS A189788:
Base-10 lunar factorials: a(n) = (lunar) Product_{i=1..n} i.
  9, 1, 1, 1, 1, 1, 1, 1, 1, 1, 10, 110, 1110, 11110, 111110, 1111110,
  11111110, 111111110, 1111111110, 11111111110, 111111111100,
  1111111111100, 11111111111100, 111111111111100, 1111111111111100,
  11111111111111100, 111111111111111100, 1111111111111111100,
  11111111111111111100, 111111111111111111100, 1111111111111111111000, ...

Calcoliamo il fattoriale lunare usando la moltiplicazione lunare.
(solo per risultati fino al massimo di Int64)

(define (lunar-fattoriale n)
  (if (zero? n) 9
      ;else
      (let (out 1)
        (for (x 1 n) (setq out (lunar-mul out x))))))

(lunar-fattoriale 20)
;-> 111111111100

(define (lunar-fattoriale2 n)
  (if (zero? n) 9
      (= 1 n) 1
      (apply lumul (sequence 1 n))))

(lunar-fattoriale2 20)
;-> 111111111100

(= (map lunar-fattoriale (sequence 0 25))
   (map lunar-fattoriale2 (sequence 0 25)))
;-> true

(map lunar-fattoriale (sequence 0 25))
;-> (9 1 1 1 1 1 1 1 1 1 10 110 1110 11110 111110 1111110
;->  11111110 111111110 1111111110 11111111110 111111111100
;->  1111111111100 11111111111100 111111111111100 1111111111111100
;->  11111111111111100)


----------------------------------------------
Valutazione di una lista di numeri e operatori
----------------------------------------------

Alle scuole elementari il maestro proponeva spesso esercizi del tipo:
- Partiamo con il numero 9.
- Poi sommiamo 7.
- Poi togliamo 2.
- Poi dividiamo per 2.
- Poi moltiplichiamo per 3.
- Risultato?

Possiamo rappresentare l'esercizio con una lista: (9 + 7 - 2 / 2 * 3)
Poi applichiamo gli operatori da sinistra a destra (senza alcuna precedenza degli operatori):
(9 + 7 - 2 / 2 * 3) = (16 - 2 / 2 * 3) = (14 / 2 * 3) = (7 * 3) = 21

Scriviamo una funzione che calcola l'espressione rappresentata da una lista di questo tipo.
Utilizziamo gli operatori "+", "-", "*", "/", "%" e "^".

(define (calcola lst parziali)
  (local (val coppie out)
    ; valore di partenza
    (setq val (pop lst))
    ; lista dei valori parziali
    (setq out (list val))
    ; divide la lista in coppie (operatore numero)
    (setq coppie (explode lst 2))
    ; ciclo per applicare le coppie al valore corrente
    (dolist (el coppie)
      (if (= (el 0) '^) ; caso della potenza
          (setq val (pow val (el 1)))
          ; tutti gli altri casi
          (setq val ((eval (el 0)) val (el 1))))
      (push val out -1)
    )
    ; soluzione
    (if parziali out val)))

Proviamo:

(calcola '(9 + 7 - 2 / 2 * 3))
;-> 21
(calcola '(9 + 7 - 2 / 2 * 3) true)
;-> (9 16 14 7 21)

(calcola '(3 + 4 % 5 * 2))
;-> 4
(calcola '(3 + 4 % 5 * 2) true)
;-> (3 7 2 4)

(calcola '(-3 + 4 % 5 * -2) true)
;-> (-3 1 1 -2)

Scriviamo una funzione che genera un problema (lista) di questo tipo.
Parametri:
numeri = quanti numeri utilizzare
min-val = valore minimo dei numeri
max-val = valore massiomo dei numeri
ops = lista degli operatori utilizzabili

(define (genera numeri min-val max-val ops)
       ; valore di partenza
  (let (out (list (+ min-val (rand (+ (- max-val min-val) 1)))))
    (for (i 2 numeri)
      ; operatore
      (push (ops (rand (length ops))) out -1)
      ; numero
      (push (+ min-val (rand (+ (- max-val min-val) 1))) out -1))
    out))

Proviamo:

(setq eq (genera 6 2 10 '(+ - *)))
;-> (5 + 8 + 2 * 4 + 7 * 9)
(calcola eq true)
;-> 603
;-> (5 13 15 60 67 603)

(for (i 1 5)
  (setq eq (genera 6 2 10 '(+ - * / %)))
  (println eq { } (calcola eq true)))
;-> (8 / 6 + 5 - 8 - 5 - 7) (8 1 6 -2 -7 -14)
;-> (6 / 3 * 10 / 10 + 5 + 10) (6 2 20 2 7 17)
;-> (6 % 2 / 2 * 10 + 10 - 4) (6 0 0 0 10 6)
;-> (8 * 3 / 7 * 6 * 7 % 9) (8 24 3 18 126 0)
;-> (7 / 7 - 3 / 6 % 4 + 7) (7 1 -2 0 0 7)


------------------------------------
Permutazioni incrociate di due liste
------------------------------------

Date due liste (es. numeri e lettere), vogliamo creare una lista con le seguenti caratteristiche:
1) Gli elementi di numeri e lettere sono permutati separatamente.
2) Vengono alternati in output: numero, lettera, numero, lettera, ...
3) Se una lista finisce, si utilizzano i suoi elementi ciclicamente.
4) Nessun elemento può essere adiacente a un altro della stessa lista originale.

Esempi:
  numeri = (1 2 3)
  lettere = (a b)
  output =
  (1 a 2 b 3 a) (1 b 2 a 3 b) (2 a 1 b 3 a) (2 b 1 a 3 b) (3 a 1 b 2 a)
  (3 b 1 a 2 b) (1 a 3 b 2 a) (1 b 3 a 2 b) (2 a 3 b 1 a) (2 b 3 a 1 b)
  (3 a 2 b 1 a) (3 b 2 a 1 b)

  numeri = (1 2)
  lettere = (a b c)
  output =
  (1 a 2 b 1 c) (1 b 2 a 1 c) (1 c 2 a 1 b) (1 a 2 c 1 b) (1 b 2 c 1 a)
  (1 c 2 b 1 a) (2 a 1 b 2 c) (2 b 1 a 2 c) (2 c 1 a 2 b) (2 a 1 c 2 b)
  (2 b 1 c 2 a) (2 c 1 b 2 a))

  numeri = (1 2 3)
  lettere = (a b c)
  output =
  (1 a 2 b 3 c) (1 b 2 a 3 c) (1 c 2 a 3 b) (1 a 2 c 3 b) (1 b 2 c 3 a)
  (1 c 2 b 3 a) (2 a 1 b 3 c) (2 b 1 a 3 c) (2 c 1 a 3 b) (2 a 1 c 3 b)
  (2 b 1 c 3 a) (2 c 1 b 3 a) (3 a 1 b 2 c) (3 b 1 a 2 c) (3 c 1 a 2 b)
  (3 a 1 c 2 b) (3 b 1 c 2 a) (3 c 1 b 2 a) (1 a 3 b 2 c) (1 b 3 a 2 c)
  (1 c 3 a 2 b) (1 a 3 c 2 b) (1 b 3 c 2 a) (1 c 3 b 2 a) (2 a 3 b 1 c)
  (2 b 3 a 1 c) (2 c 3 a 1 b) (2 a 3 c 1 b) (2 b 3 c 1 a) (2 c 3 b 1 a)
  (3 a 2 b 1 c) (3 b 2 a 1 c) (3 c 2 a 1 b) (3 a 2 c 1 b) (3 b 2 c 1 a)
  (3 c 2 b 1 a)

Nota:
(map list '(1 2 3) '(a b c))
;-> ((1 a) (2 b) (3 c))
(map list '(1 2) '(a b c))
;-> ((1 a) (2 b))
(map list '(1 2 3) '(a b))
;-> ((1 a) (2 b) (3))

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
              (swap (lst (indici i)) (lst i))
            )
            ;(println lst);
            (push lst out -1)
            (++ (indici i))
            (setq i 0)
          )
          (begin
            (setf (indici i) 0)
            (++ i)
          )
      )
    )
    out))

Se vogliamo che gli elementi della seconda lista (es. lettere) vengano ripetuti dobbiamo usare la funzione "perm-rep" (permutazioni con ripetizione) nel ciclo 'for' annidato (questo crea un maggior numero di risultati).

(define (perm-rep k lst)
"Generate all permutations of k elements with repetition from a list of items"
  (if (zero? k) '(())
      (flat (map (lambda (p) (map (lambda (e) (cons e p)) lst))
                         (perm-rep (- k 1) lst)) 1)))

;; Ripete ciclicamente la lista 'lst' fino alla lunghezza 'k'.
;; Es: (allunga '(1 2 3) 5) -> (1 2 3 1 2)
(define (allunga lst k)
  ;; Duplica la lista per quante volte serve, poi la taglia a lunghezza k
  (slice (extend lst (flat (dup lst (/ k (length lst))))) 0 k))

;; Genera tutte le combinazioni in cui elementi di lst1 e lst2 si alternano.
;; Se le liste hanno lunghezza diversa, la più corta viene allungata ciclicamente.
;; Se 'all' è true, usa permutazioni con ripetizione per la seconda lista.
(define (cross-perm lst1 lst2 all)
  (local (out len1 len2 more more2 permute1 permute2)
    (setq out '())                     ; lista dei risultati
    (setq len1 (length lst1))
    (setq len2 (length lst2))
    (setq more1 nil)                  ; indica se lst1 è più lunga
    (setq more2 nil)                  ; indica se lst2 è più lunga
    ;; Determina quale lista va estesa ciclicamente
    (if (> len1 len2) (setq more2 true))
    (if (< len1 len2) (setq more1 true))
    ;; Permutazioni della prima lista
    (setq permute1 (perm lst1))
    ;; Permutazioni della seconda lista (con o senza ripetizione)
    (if all
        (setq permute2 (perm-rep (length lst2) lst2))  ; con ripetizione
        (setq permute2 (perm lst2)))                   ; senza ripetizione
    ;; Combina ogni coppia di permutazioni
    (dolist (p1 permute1)
      (dolist (p2 permute2)
        ;; Allunga la lista più corta per poter alternare gli elementi
        (cond (more1 (setq p1 (allunga p1 len2)))
              (more2 (setq p2 (allunga p2 len1))))
        ;; Intercala p1 e p2: (map list p1 p2) produce ((x1 y1) (x2 y2) ...)
        ;; 'flat' la rende (x1 y1 x2 y2 ...)
        (push (flat (map list p1 p2)) out -1)))
    ;; Se con ripetizione, elimina i duplicati
    (if all (unique out) out)))

Proviamo:
(cross-perm '(1 2 3) '(a b))
;-> ((1 a 2 b 3 a) (1 b 2 a 3 b) (2 a 1 b 3 a) (2 b 1 a 3 b) (3 a 1 b 2 a)
;->  (3 b 1 a 2 b) (1 a 3 b 2 a) (1 b 3 a 2 b) (2 a 3 b 1 a) (2 b 3 a 1 b)
;->  (3 a 2 b 1 a) (3 b 2 a 1 b))

(cross-perm '(1 2) '(a b c))
;-> ((1 a 2 b 1 c) (1 b 2 a 1 c) (1 c 2 a 1 b) (1 a 2 c 1 b) (1 b 2 c 1 a)
;->  (1 c 2 b 1 a) (2 a 1 b 2 c) (2 b 1 a 2 c) (2 c 1 a 2 b) (2 a 1 c 2 b)
;->  (2 b 1 c 2 a) (2 c 1 b 2 a))

(cross-perm '(1 2 3) '(a b c))
;-> ((1 a 2 b 3 c) (1 b 2 a 3 c) (1 c 2 a 3 b) (1 a 2 c 3 b) (1 b 2 c 3 a)
;->  (1 c 2 b 3 a) (2 a 1 b 3 c) (2 b 1 a 3 c) (2 c 1 a 3 b) (2 a 1 c 3 b)
;->  (2 b 1 c 3 a) (2 c 1 b 3 a) (3 a 1 b 2 c) (3 b 1 a 2 c) (3 c 1 a 2 b)
;->  (3 a 1 c 2 b) (3 b 1 c 2 a) (3 c 1 b 2 a) (1 a 3 b 2 c) (1 b 3 a 2 c)
;->  (1 c 3 a 2 b) (1 a 3 c 2 b) (1 b 3 c 2 a) (1 c 3 b 2 a) (2 a 3 b 1 c)
;->  (2 b 3 a 1 c) (2 c 3 a 1 b) (2 a 3 c 1 b) (2 b 3 c 1 a) (2 c 3 b 1 a)
;->  (3 a 2 b 1 c) (3 b 2 a 1 c) (3 c 2 a 1 b) (3 a 2 c 1 b) (3 b 2 c 1 a)
;->  (3 c 2 b 1 a))

(cross-perm '(1 2) '(a b))
;-> ((1 a 2 b) (1 b 2 a) (2 a 1 b) (2 b 1 a))

(cross-perm '(1 2) '(a b) true)
;-> ((1 a 2 a) (1 b 2 a) (1 a 2 b) (1 b 2 b)
;->  (2 a 1 a) (2 b 1 a) (2 a 1 b) (2 b 1 b))

Costruiamo un generatore che emette una combinazione ad ogni chiamata.

Generatore Lazy (closure) (non funziona):

;; Generatore lazy di permutazioni incrociate
(define (crossing-generator lst1 lst2 all)
  (letn (
    (len1 (length lst1))
    (len2 (length lst2))
    (more1 (< len1 len2))
    (more2 (> len1 len2))
    (permute1 (perm lst1))
    (permute2 (if all (perm-rep len2) lst2) (perm lst2)))
    (i 0) ; indice permute1
    (j 0) ; indice permute2
  )
  ;; restituisce una funzione generatrice
  (lambda ()
    (if (and (>= i (length permute1)) (>= j (length permute2)))
        nil
        (begin
          (when (>= j (length permute2)) (setq j 0) (++ i))
          (if (>= i (length permute1))
              nil
              (let (
                (p1 (permute1 i))
                (p2 (permute2 j))
              )
                (if more1 (setq p1 (allunga p1 len2)))
                (if more2 (setq p2 (allunga p2 len1)))
                (++ j)
                (flat (map list p1 p2)))))))))

(setq g (crossing-generator '(1 2) '(a b) nil))
(g) ;-> (1 a 2 b)
(g) ;-> (1 b 2 a)
(g) ;-> (2 a 1 b)
(g) ;-> (2 b 1 a)
(g) ;-> nil  ; fine delle combinazioni

La funzione utilizza una 'closure' per mantenere lo stato corrente... ma in newLISP la 'closure' non esiste, quindi bisogna usare i contesti (context) per simulare una 'closure'.

Generatore Lazy (contesti):

(context 'CrossGen)
;; setup
(define (setup lst1 lst2 all)
  (setq len1 (length lst1))
  (setq len2 (length lst2))
  (setq more1 (< len1 len2))
  (setq more2 (> len1 len2))
  (setq permute1 (MAIN:perm lst1))
  (setq permute2 (if all (MAIN:perm-rep len2 lst2) (MAIN:perm lst2)))
  (setq i 0)
  (setq j 0))
;; next value
(define (next)
  (if (and (>= i (length permute1)) (>= j (length permute2)))
      nil
      (begin
        (when (>= j (length permute2)) (setq j 0) (inc i))
        (if (>= i (length permute1))
            nil
            (let (
              (p1 (permute1 i))
              (p2 (permute2 j)))
              (if more1 (setq p1 (MAIN:allunga p1 len2)))
              (if more2 (setq p2 (MAIN:allunga p2 len1)))
              (inc j)
              (flat (map list p1 p2)))))))
(context MAIN)

Proviamo:

(CrossGen:setup '(1 2) '(a b) nil)
;-> 0
(CrossGen:next)
;-> (1 a 2 b)
(CrossGen:next)
;-> (1 b 2 a)
(CrossGen:next)
;-> (2 a 1 b)
(CrossGen:next)
;-> (2 b 1 a)
(CrossGen:next)
;-> nil ; fine


------------------------------------------------------
Ricostruzione di equazioni con addizioni e sottrazioni
------------------------------------------------------

Abbiamo un'equazione di addizioni e sottrazioni come "3 +- 2 +- 4 +- 1 = 4".
Determinare, se esiste, una sequenza di + e - che la renda aritmeticamente corretta.
Ad esempio, (-  +  -) risolve il nostro caso: "3 - 2 + 4 - 1 = 4".
Alcuni casi non hanno soluzioni (es. "3 +- 5 = 7").
Alcuni casi hanno più soluzioni (es. "1 +- 2 +- 4 +- 3 +- 1 = 1", sia (- + - +) che (+ - + -) sono valide).

Rappresentiamo l'equazione come una lista:

(setq lst '(3 1 4 2 0 4))

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
              (swap (lst (indici i)) (lst i))
            )
            ;(println lst);
            (push lst out -1)
            (++ (indici i))
            (setq i 0)
          )
          (begin
            (setf (indici i) 0)
            (++ i)
          )
      )
    )
    out))

Funzione che prova ricostruire un'equazione di addizioni e sottrazioni:

(define (piumeno lst)
  (local (result termini oper len ops start tot out)
    (setq out nil)
    ; Il risultato è l'ultimo elemento della lista
    (setq result (pop lst -1))
    ; Il resto della lista sono i termini dell'equazione
    (setq termini lst)
    (setq oper '(+ -))
    (setq len (length lst))
    ; Creazione lista degli operatori
    ; Ripetizione ciclica di (+ -) fino alla lunghezza della lista dei termini -1
    (setq oper (slice (extend oper (flat (dup oper (/ len 2)))) 0 (- len 1)))
    ; Creazione delle permutazioni uniche della lista degli operatori
    (setq ops (unique (perm oper)))
    ; valore iniziale dell'equazione
    (setq start (pop lst))
    ; ciclo per ogni permutazione degli operatori
    (dolist (op ops)
      ; applica gli operatori correnti ai termini della lista
      (setq tot start)
      (dolist (el lst)
        (setq tot ((eval (op $idx)) tot el)))
      ; verifica se l'equazione corrente produce il risultato corretto
      (when (= tot result)
        ; stampa l'equazione corretta
        (setq out true)
        (println (flat (map list termini op)) " = " result))
    )
    out))

Proviamo:

(piumeno lst)
;-> (3 - 1 + 4 - 2 + 0) = 4
(piumeno '(1 2 3 4 1 1))
;-> (1 + 2 + 3 - 4 - 1) = 1
;-> (1 - 2 - 3 + 4 + 1) = 1
(piumeno '(1 2 3 4 1 12))
;-> nil

Proviamo le equazioni:

  1 +- 2 +- 3 +- ... n = n
  con 1 <= n <= 10

(for (n 1 10)
  (println n)
  (setq lst (sequence 1 n))
  (push n lst -1)
  (piumeno lst)
)
;-> 1
;-> (1) = 1
;-> 2
;-> 3
;-> 4
;-> (1 + 2 - 3 + 4) = 4
;-> 5
;-> (1 - 2 - 3 + 4 + 5) = 5
;-> 6
;-> 7
;-> 8
;-> (1 - 2 + 3 + 4 - 5 + 6 - 7 + 8) = 8
;-> (1 + 2 - 3 - 4 + 5 + 6 - 7 + 8) = 8
;-> (1 + 2 - 3 + 4 - 5 - 6 + 7 + 8) = 8
;-> (1 - 2 + 3 - 4 + 5 + 6 + 7 - 8) = 8
;-> 9
;-> (1 - 2 - 3 - 4 + 5 + 6 + 7 + 8 - 9) = 9
;-> (1 - 2 + 3 - 4 - 5 + 6 - 7 + 8 + 9) = 9
;-> (1 - 2 - 3 + 4 + 5 - 6 - 7 + 8 + 9) = 9
;-> (1 + 2 - 3 - 4 - 5 - 6 + 7 + 8 + 9) = 9
;-> (1 - 2 - 3 + 4 - 5 + 6 + 7 - 8 + 9) = 9
;-> 10


-----------------------
Sequenza di Lichtenberg
-----------------------

Calolare la sequenza dei numeri senza cifre binarie consecutive uguali.
La sequenza contiene la rappresentazione decimale dei numeri binari nella forma: 10101..., dove l'n-esimo termine ha n bit.
La sequenza inizia con i seguenti numeri:

 n   binario     num
 0   0       ->  0
 1   1       ->  1
 2   10      ->  2
 3   101     ->  5
 4   1010    ->  10
 5   10101   ->  21
 6   101010  ->  42

Sequenza OEIS A000975:
a(2n) = 2*a(2n-1), a(2n+1) = 2*a(2n)+1 (also a(n) is the n-th number without consecutive equal binary digits).
  0, 1, 2, 5, 10, 21, 42, 85, 170, 341, 682, 1365, 2730, 5461, 10922,
  21845, 43690, 87381, 174762, 349525, 699050, 1398101, 2796202,
  5592405, 11184810, 22369621, 44739242, 89478485, 178956970, 357913941,
  715827882, 1431655765, 2863311530, 5726623061, 11453246122, ...

Formula: a(n) = a(n-1) + 2*a(n-2) + 1

Formula ricorsiva:

(define (a n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 2)
        (true
          (+ (a (- n 1)) (* 2 (a (- n 2))) 1))))

(map a (sequence 0 34))

Formula dynamic programming (botton-up):

(define (b n)
  (cond ((zero? n) 0)
        ((= n 1) 1)
        (true
          (setq dp (array (+ n 1) '(0)))
          (setf (dp 0) 0)
          (setf (dp 1) 1)
          (for (i 2 n)
            (setq val (+ (dp (- i 1)) (* 2 (dp (- i 2))) 1))
            (setf (dp i) val)))
          dp))

(b 34)
;-> (0 1 2 5 10 21 42 85 170 341 682 1365 2730 5461 10922
;->  21845 43690 87381 174762 349525 699050 1398101 2796202
;->  5592405 11184810 22369621 44739242 89478485 178956970 357913941
;->  715827882 1431655765 2863311530 5726623061 11453246122)

Costruzione numero binario:

(define (c n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 2)
        (true
          (if (even? n)
            (int (dup "10" (/ n 2)) 0 2)
            (int (push "1" (dup "01" (/ n 2))) 0 2)))))

(map c (sequence 1 34))
;-> (0 1 2 5 10 21 42 85 170 341 682 1365 2730 5461 10922
;->  21845 43690 87381 174762 349525 699050 1398101 2796202
;->  5592405 11184810 22369621 44739242 89478485 178956970 357913941
;->  715827882 1431655765 2863311530 5726623061 11453246122)

Test di velocità:

(time (b 35) 1e5)
;-> 712.028

(setq nums (sequence 0 35))
(time (map c nums) 1e5)
;-> 1949.712

Per calcolare un solo termine della sequenza è meglio usare la funzione "c".
Per calcolare N termini della sequenza è meglio usare la funzione "b".


----------------------
Sequenza di Jacobsthal
----------------------

Sequenza OEIS A001045:
Jacobsthal sequence (or Jacobsthal numbers): a(n) = a(n-1) + 2*a(n-2), with a(0) = 0, a(1) = 1
Also a(n) = nearest integer to 2^n/3.
  0, 1, 1, 3, 5, 11, 21, 43, 85, 171, 341, 683, 1365, 2731, 5461,
  10923, 21845, 43691, 87381, 174763, 349525, 699051, 1398101, 2796203,
  5592405, 11184811, 22369621, 44739243, 89478485, 178956971, 357913941,
  715827883, 1431655765, 2863311531, 5726623061, 11453246123, ...

Formula : a(n) = (2^n - (-1)^n)/3

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(define (a n) (/ (- (** 2 n) (** -1 n)) 3))

(map a (sequence 0 33))
;-> (0L 1L 1L 3L 5L 11L 21L 43L 85L 171L 341L 683L 1365L 2731L 5461L
;->  10923L 21845L 43691L 87381L 174763L 349525L 699051L 1398101L 2796203L
;->  5592405L 11184811L 22369621L 44739243L 89478485L 178956971L 357913941L
;->  715827883L 1431655765L 2863311531L 5726623061L)


------------------
Stringhe elastiche
------------------

Per "elasticizzare" una stringa S bisogna generare una stringa in cui l'n-esimo carattere viene ripetuto n volte.
In altre parole:
il primo carattere viene ripetuto una volta,
il secondo carattere viene ripetuto due volte,
il terzo carattere viene ripetuto tre volte e così via.

Esempio:
str = "abcde"
output = "abbcccddddeeeee"

Scrivere una funzione (più corta possibile) che "elasticizza" una stringa.

Prima funzione (75 caratteri):
(define(e1 s)(let(o "")(dostring(ch s)(extend o(dup(char ch)(+ $idx 1))))))
(e1 "abcde")
;-> "abbcccddddeeeee"
(e1 "abracadabra")
;-> "abbrrraaaacccccaaaaaadddddddaaaaaaaabbbbbbbbbrrrrrrrrrraaaaaaaaaaa"

Seconda funzione (78 caratteri):
(define(e2 s)(join(map(fn(x)(dup(s x)(+ $idx 1)))(sequence 0(-(length s)1)))))
(e2 "abcde")
;-> "abbcccddddeeeee"
(e1 "abracadabra")
;-> "abbrrraaaacccccaaaaaadddddddaaaaaaaabbbbbbbbbrrrrrrrrrraaaaaaaaaaa"


-------------------
Stirare una stringa
-------------------

Data una stringa S l'operazione di "stiramento" consiste nel generare una stringa in cui:
per ogni carattere, ripeterlo N volte l'N-esima volta che appare.
In altre parole, per ogni carattere:
usare il carattere quando appare la prima volta
raddoppiare il carattere quando appare la seconda volta
triplicare il carattere quando appare la terza volta e cosi via.

Esempio:
str = "bonobo"
output = "bonoobbooo"

Scrivere una funzione che "stira" una stringa.

(define (stira str)
  (local (lst unici link out rip)
    (setq out "")
    ; lista di caratteri della stringa
    (setq lst (explode str))
    ; caratteri unici della lista/stringa
    (setq unici (unique lst))
    ; lista associativa: (carattere occorrenze)
    (setq link (map list unici (count unici lst)))
    ; ciclo dalla fine della lista/stringa
    ; crea la stringa partendo dall'ultimo carattere
    (for (i (- (length lst) 1) 0 -1)
      ; trova il numero di occorrenze del carattere corrente
      ; nella lista associativa
      (setq rip (lookup (lst i) link))
      ; estende la stringa soluzione ripetendo il carattere corrente
      ; tante volte quante le occorrenze
      (extend out (dup (lst i) rip))
      ; diminuisce di 1 le occorrenze del carattere corrente
      (setf (lookup (lst i) link) (- $it 1)))
    ; inverte la stringa soluzione
    (reverse out)))

Proviamo:

(stira "bonobo")
;-> "bonoobbooo"
(stira "abracadabra")
;-> "abraacaaadaaaabbrraaaaa"
(stira "mamma")
;-> "mammmmmaa"


------------------------------------------------
Numerazione ordinale degli elementi di una lista
------------------------------------------------

Data una lista di interi, costruire una nuova lista in cui ogni elemento è sostituito da un intero compreso tra 0 e K - 1, dove K è il numero di elementi distinti presenti nella lista originale.
Gli interi da 0 a K - 1 rappresentano un ordinamento crescente degli elementi unici della lista originale.
In particolare:
- Il numero più piccolo viene sostituito da 0.
- Il secondo più piccolo da 1.
- E così via, fino al più grande che viene sostituito da K - 1.

Elementi uguali nella lista originale verranno sostituiti dallo stesso valore nella nuova lista.

Esempio:
  lista = (14 13 10 11 11 13 12)
  output = (4 3 0 1 1 3 2)
Spiegazione:
  lista ordinata = (10 11 11 12 13 14)
  numeri unici = K = 5 ; 10 11 12 13 14
  interi da 0 a 4 (K-1) = (0 1 2 3 4)
  associazione = (10 0) (11 1) (12 2) (13 3) (14 4)

Funzione che numera gli elementi di una lista in modo ordinale:

(define (numera-ordinali lst)
  (local (unici mappa)
    ; unique trova i valori distinti.
    ; sort li ordina in modo crescente.
    (setq unici (sort (unique lst)))
    ; map list e sequence creano la corrispondenza
    ; tra ogni valore e il suo indice.
    (setq mappa (map list unici (sequence 0 (- (length unici) 1))))
    ; map con lookup applica la trasformazione a tutta la lista originale
    (map (fn (x) (lookup x mappa)) lst)))

Proviamo:

(setq lst '(14 13 10 11 11 13 12))
(numera-ordinali lst)
;-> (4 3 0 1 1 3 2)

(numera-ordinali '(100 3 5 67 3))
;-> (3 0 1 2 0)


---------------------------------
Primi con numero primo di bit a 1
---------------------------------

Calcolare la sequenza di numeri primi che hanno un numero primo di bit a 1 nella loro rappresentazione binaria.

Sequenza OEIS A081092:
Primes having a prime number of 1's in their binary representation.
  3, 5, 7, 11, 13, 17, 19, 31, 37, 41, 47, 59, 61, 67, 73, 79, 97, 103,
  107, 109, 127, 131, 137, 151, 157, 167, 173, 179, 181, 191, 193, 199,
  211, 223, 227, 229, 233, 239, 241, 251, 257, 271, 283, 307, 313, 331,
  367, 379, 397, 409, 419, 421, 431, 433, 439, 443, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (primo-1? num)
  (and (prime? num) (prime? (length (find-all "1" (bits num))))))

(filter primo-1? (sequence 2 443))
;-> (3 5 7 11 13 17 19 31 37 41 47 59 61 67 73 79 97 103
;->  107 109 127 131 137 151 157 167 173 179 181 191 193 199
;->  211 223 227 229 233 239 241 251 257 271 283 307 313 331
;->  367 379 397 409 419 421 431 433 439 443)


-------------------
Primi con N bit a 1
-------------------

Calcolare la sequenza dei numeri primi più piccoli con peso di Hamming n (vale a dire, con esattamente n 1 quando scritti in binario).

Sequenza OEIS A061712:
Smallest prime with Hamming weight n (i.e., with exactly n 1's when written in binary).
  2, 3, 7, 23, 31, 311, 127, 383, 991, 2039, 3583, 6143, 8191, 73727,
  63487, 129023, 131071, 522239, 524287, 1966079, 4128767, 16250879,
  14680063, 33546239, 67108351, 201064447, 260046847, 536739839,
  1073479679, 5335154687, 2147483647, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (primes-to num)
"Generate all prime numbers less than or equal to a given number"
  (cond ((= num 1) '())
        ((= num 2) '(2))
        (true
          (let ((lst '(2)) (arr (array (+ num 1))))
            (for (x 3 num 2)
              (when (not (arr x))
                (push x lst -1)
                (for (y (* x x) num (* 2 x) (> y num))
                  (setf (arr y) true)))) lst))))

Funzione che calcola i primi 25 numeri della sequenza:
(non è possibile calcolare i numeri primi > 1e8 in tempo ragionevole)

(define (seq25)
  (local (out primi n found)
    (setq out '())
    ; lista di primi
    (setq primi (primes-to 1e8))
    ; indice corrente della sequenza
    (setq n 1)
    ; ciclo per trovare k elementi della sequenza
    (for (k 1 25)
      (setq found nil)
      (dolist (p primi found)
        ; se il primo corrente ha n bit?
        (when (= (length (find-all "1" (bits p))) n)
              ; lo inserisce nella sequenza
              (push p out -1)
              ; aumenta l'indice della sequenza
              (++ n)
              ; e esce dal ciclo 'dolist'
              (setq found true))
      )
    )
    out))

(time (setq seq (seq25)))
;-> 40055.704
seq
;-> (2 3 7 23 31 311 127 383 991 2039 3583 6143 8191 73727
;->  63487 129023 131071 522239 524287 1966079 4128767 16250879
;->  14680063 33546239 67108351)
(length seq)
;-> 25


----------------------------------------------------
Primi con numero di bit a 1 uguale all'n-esimo primo
----------------------------------------------------

Calcolare la sequenza di numeri dove:
a(n) è il più piccolo numero primo tale che il numero di 1 nella sua espansione binaria è uguale al numero primo n-esimo.

Sequenza OEIS A081093:
a(n) is the smallest prime such that the number of 1's in its binary expansion is equal to the n-th prime.
  3, 7, 31, 127, 3583, 8191, 131071, 524287, 14680063, 1073479679,
  2147483647, 266287972351, 4260607557631, 17591112302591,
  246290604621823, 17996806323437567, 1152917106560335871,
  2305843009213693951, ...

Formula:

a(n) = A061712(A000040(n))

(setq A061712 '(0 2 3 7 23 31 311 127 383 991 2039 3583 6143 8191 73727
                63487 129023 131071 522239 524287 1966079 4128767 16250879
                14680063 33546239 67108351))

(setq A000040 '(0 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47
                53 59 61 67 71 73 79 83 89 97))

Nota: inseriamo 0 all'inizio delle due liste perchè la formula è 1-index-based.

(define (a n) (A061712 (A000040 n)))

Proviamo:

(map a (sequence 1 9))
;-> (3 7 31 127 3583 8191 131071 524287 14680063)

Nota: il nono intero (23) indirizza al 23-esimo elemento di A061712 (14680063).
Il decimo intero indirizza il 29-esimo elemento di A061712 (che non abbiamo calcolato).
Vedi "Primi con N bit a 1" su "Note libere 30."


--------------
Debug infinito
--------------

Il programmatore GOTO sta analizzando un programma da correggere.
Il programma ha solo un bug.
Per ogni bug che risolve, GOTO inserisce 3 nuovi bug...
Alla fine della giornata GOTO ha eliminato 15 bug.
Quanti bug ha introdotto nel programma?

All'inizio il programma ha un solo bug.
Dopo la correzione il programma ha 3 bug (2 in più).
Adesso con 3 bug, risolvendone uno e aggiungendone 2, abbiamo 5 bug (2 in più).
Con 5 bug, risolvendone uno e aggiungendone 2, abbiamo 7 bug (2 in più).
...

Notiamo che dopo ogni correzione i bug aumentano di 2 (invariante):

  bug-presenti = 1 + 2 * bug-risolti

Scriviamo una funzione per simulare il processo:
"total-bugs" restituisce il numero totale di bug presenti nel programma partendo da "start" bug iniziali, aggiungendo "added" bug ogni volta che eliminiamo un bug e avendo, alla fine, eliminato "solved" bug in tutto.

(define (total-bugs start added solved)
       ; numero di bug iniziali
  (let (total start)
    ; ciclo per ogni bug eliminato
    (for (i 1 solved)
      (-- total)
      (++ total added))
    total))

Proviamo:

(total-bugs 1 3 15)
;-> 31
(total-bugs 1 1 15)
;-> 1
(total-bugs 1 0 5)
;-> -4 
Il programma funzionerà anche aggiungendo 4 bug :-)


------------------------
Ordinare un'enciclopedia
------------------------

Abbiamo un'enciclopedia in 10 volumi numerati da 1 a 10.
I volumi sono disposti in uno scaffale di una libreria e non sono in ordine.
Dobbiamo ordinare i volumi scambiando la posizione di due volumi.
Quindi un passo consiste nello scambiare di posto i volumi "i" e "j".
Quale configurazione iniziale di volumi comporta il maggior numero di passi per ordinarli?

Possiamo affermare che ogni posizione di partenza è ordinabile con 9 passi al massimo.

Consideriamo per semplicità i numeri da 0 a 9.

Iniziamo con una posizione di partenza in cui nessun numero occupa il proprio indice:
  4 7 5 3 8 9 2 1 0 6
Come primo passo cerchiamo il numero 0 e lo scambiamo con il numero che si trova alla posizione 0.
  0 7 5 3 8 9 2 1 4 6
Come secondo passo cerchiamo il numero 1 e lo scambiamo con il numero che si trova alla posizione 1.
  0 1 5 3 8 9 2 7 4 6
...
Come nono passo cerchiamo il numero 8 e lo scambiamo con il numero che si trova alla posizione 8.
  0 1 2 3 4 5 6 7 8 9
A questo punto il numero 9 deve per forza trovarsi alla posizione 9, perchè i numeri da 0 a 8 si trovano nelle posizioni da 0 a 8, quindi resta solo la posizione 9 per il numero 9 (principio dei cassetti - pigeonhole principle).

Comunque esistono delle posizioni iniziale che possono essere ordinate in un numero di passi minore di 9.
Basta pensare alla posizione già ordinata in partenza che richiede 0 passi.

(define (passi lst show)
  (local (sorted step i j)
    (setq sorted '(0 1 2 3 4 5 6 7 8 9))
    (setq step 0)
    (setq i 0)
    (until (= lst sorted)
      (setq j (find i lst))
      (when (!= i j)
        (swap (lst i) (lst j))
        (++ step)
        (if show (println step { } lst))
      )
      (++ i)
    )
    step))

Facciamo alcune prove:

(passi '(4 7 5 3 8 9 2 1 0 6) true)
;-> 1 (0 7 5 3 8 9 2 1 4 6)
;-> 2 (0 1 5 3 8 9 2 7 4 6)
;-> 3 (0 1 2 3 8 9 5 7 4 6)
;-> 4 (0 1 2 3 4 9 5 7 8 6)
;-> 5 (0 1 2 3 4 5 9 7 8 6)
;-> 6 (0 1 2 3 4 5 6 7 8 9)
;-> 6
(passi '(2 0 3 5 8 1 6 7 4 9) true)
;-> 1 (0 2 3 5 8 1 6 7 4 9)
;-> 2 (0 1 3 5 8 2 6 7 4 9)
;-> 3 (0 1 2 5 8 3 6 7 4 9)
;-> 4 (0 1 2 3 8 5 6 7 4 9)
;-> 5 (0 1 2 3 4 5 6 7 8 9)
;-> 5

Dopo diverse prove si scopre che il numero di passi vale come minimo il numero di numeri che si trovano a destra del proprio indice nella posizione iniziale.

Per esempio:
  lista =  2 0 3 5 8 1 6 7 4 9
  indici = 0 1 2 3 4 5 6 7 8 9
  2 si trova a sinistra dell'indice 2
  0 si trova a destra dell'indice 0
  3 si trova a sinistra dell'indice 3
  5 si trova a sinistra dell'indice 5
  8 si trova a sinistra dell'indice 5
  1 si trova a destra dell'indice 1
  6 si trova allo stesso indice
  7 si trova allo stesso indice
  4 si trova a destra dell'indice 4
  9 si trova allo stesso indice

Intuitivamente questo significa che tutti i numeri a destra devono essere spostati a sinistra.
I rimanenti numeri devono essere spostati a destra o rimanere al proprio posto.
Quindi i passi per ordinare devono essere almeno quanti sono i numeri a destra.

La configurazione iniziale con il massimo numero di numeri a destra dell'indice vale:
  (9 0 1 2 3 4 5 6 7 8)
in cui 9 numeri (0..8) sono a destra dell'indice.

(passi '(9 0 1 2 3 4 5 6 7 8) true)
;-> 1 (0 9 1 2 3 4 5 6 7 8)
;-> 2 (0 1 9 2 3 4 5 6 7 8)
;-> 3 (0 1 2 9 3 4 5 6 7 8)
;-> 4 (0 1 2 3 9 4 5 6 7 8)
;-> 5 (0 1 2 3 4 9 5 6 7 8)
;-> 6 (0 1 2 3 4 5 9 6 7 8)
;-> 7 (0 1 2 3 4 5 6 9 7 8)
;-> 8 (0 1 2 3 4 5 6 7 9 8)
;-> 9 (0 1 2 3 4 5 6 7 8 9)


----------------------
Numeri primi magnanimi
----------------------

I numeri primi magnanimi sono numeri primi la cui proprietà è che inserendo un "+" in qualsiasi punto tra due cifre si ottiene una somma che è prima.

Esempio:
  numero primo = 2267
  2+267 = 269 è primo
  22+67 = 89 è primo
  226+7 = 233 è primo
  Quindi 2267 è un numero primo magnanimo.

Sequenza OEIS A089392:
Magnanimous primes: primes with the property that inserting a "+" in any place between two digits yields a sum which is prime.
  2, 3, 5, 7, 11, 23, 29, 41, 43, 47, 61, 67, 83, 89, 101, 227, 229,
  281, 401, 443, 449, 467, 601, 607, 647, 661, 683, 809, 821, 863, 881,
  2221, 2267, 2281, 2447, 4001, 4027, 4229, 4463, 4643, 6007, 6067, 6803,
  8009, 8221, 8821, 20261, 24407, 26881, 28429, 40427, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

Funzione che verifica se un numero è primo magnanimo:

(define (magna? num)
  (local (str len magna)
    (setq str (string num))
    (setq len (length str))
    ; il numero deve essere primo
    (setq magna (prime? num))
    (setq i 1)
    ; ciclo per sommare ogni coppia di numeri
    ; ottenuti con l'inserimento del "+"
    (while (and (<= i (- len 1)) magna)
      ; primo numero
      (setq a (int (slice str 0 i) 0 10))
      ; secondo numero
      (setq b (int (slice str i) 0 10))
      ; verifica se la somma è prima
      (if (not (prime? (+ a b))) (setq magna nil))
      ;(println a { } b)
      (++ i)
    )
    magna))

Proviamo:

(magna? 2267)
;-> true

(filter magna? (sequence 1 50000))
;-> (2 3 5 7 11 23 29 41 43 47 61 67 83 89 101 227 229
;->  281 401 443 449 467 601 607 647 661 683 809 821 863 881
;->  2221 2267 2281 2447 4001 4027 4229 4463 4643 6007 6067 6803
;->  8009 8221 8821 20261 24407 26881 28429 40427 40483 42209)


----------------
Primi palindromi
----------------

Calcolare la sequenza dei numeri primi palindromi.

Sequenza OEIS A002385:
Palindromic primes: prime numbers whose decimal expansion is a palindrome.
  2, 3, 5, 7, 11, 101, 131, 151, 181, 191, 313, 353, 373, 383, 727, 757,
  787, 797, 919, 929, 10301, 10501, 10601, 11311, 11411, 12421, 12721,
  12821, 13331, 13831, 13931, 14341, 14741, 15451, 15551, 16061, 16361,
  16561, 16661, 17471, 17971, 18181, 18481, 19391, 19891, 19991, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(setq num 121213)
(= (string num) (reverse (string num)))

(define (palindrome? obj)
"Check if a list or a string or a number is palindrome"
  (if (integer? obj)
      (let (str (string obj)) (= str (reverse (copy str))))
      (= obj (reverse (copy obj)))))

(filter (fn(x) (and (prime? x) (palindrome? x))) (sequence 1 20000))
;-> (2 3 5 7 11 101 131 151 181 191 313 353 373 383 727 757
;->  787 797 919 929 10301 10501 10601 11311 11411 12421 12721
;->  12821 13331 13831 13931 14341 14741 15451 15551 16061 16361
;->  16561 16661 17471 17971 18181 18481 19391 19891 19991)

Scriviamo una funzione ottimizzata per vedere la differenza di velocità.

(define (primi-pali limite)
  (let (out '(2))
    (for (num 3 limite 2)
      (if (and (= 1 (length (factor num)))
              (= (string num) (reverse (string num))))
          (push num out -1)))
    out))

Vediamo i tempi di esecuzione:

(time (filter (fn(x) (and (prime? x) (palindrome? x))) (sequence 1 1e6)))
;-> 935.261

(time (primi-pali 1e6))
;-> 559.621

(silent (setq nums (sequence 1 1e6)))
(time (filter (fn(x) (and (prime? x) (palindrome? x))) nums))
;-> 918.699


-----------------------
Prodotti di cifre primi
-----------------------

Calcolare la sequenza dei numeri in cui il prodotto delle cifre è un numero primo.

Sequenza OEIS A028842:
Product of digits of n is a prime.
  2, 3, 5, 7, 12, 13, 15, 17, 21, 31, 51, 71, 112, 113, 115, 117, 121,
  131, 151, 171, 211, 311, 511, 711, 1112, 1113, 1115, 1117, 1121, 1131,
  1151, 1171, 1211, 1311, 1511, 1711, 2111, 3111, 5111, 7111, 11112,
  11113, 11115, 11117, 11121, 11131, 11151, ...

(define (digit-prod num)
"Calculate the product of the digits of an integer"
  (if (zero? num)
      0
      (let (out 1)
        (while (!= num 0)
          (setq out (* out (% num 10)))
          (setq num (/ num 10))
        )
    out)))

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(filter (fn(x) (prime? (digit-prod x))) (sequence 2 12000))
;-> (2 3 5 7 12 13 15 17 21 31 51 71 112 113 115 117 121
;->  131 151 171 211 311 511 711 1112 1113 1115 1117 1121 1131
;->  1151 1171 1211 1311 1511 1711 2111 3111 5111 7111 11112
;->  11113 11115 11117 11121 11131 11151 11171 11211 11311 11511 11711)


--------------------------------
Numeri con tutte coppie di primi
--------------------------------

Calcolare la sequenza dei numeri la cui somma è un numero primo per ogni coppia di cifre.

Sequenza OEIS A221699:
Numbers with property that each sum any pair of digits is prime.
  11, 12, 14, 16, 20, 21, 23, 25, 29, 30, 32, 34, 38, 41, 43, 47, 49,
  50, 52, 56, 58, 61, 65, 67, 70, 74, 76, 83, 85, 89, 92, 94, 98, 111,
  112, 114, 116, 121, 141, 161, 203, 205, 211, 230, 250, 302, 320, 411,
  502, 520, 611, 1111, 1112, 1114, 1116, 1121, 1141, 1161, ...

(define (couples lst all)
"Generate all the couples of a list"
  (let (out '())
    (if all
      (for (i 0 (- (length lst) 1))
        (for (j i (- (length lst) 1))
            (push (list (lst i) (lst j)) out -1)))
      ;else
      (for (i 0 (- (length lst) 2))
        (for (j (+ i 1) (- (length lst) 1))
            (push (list (lst i) (lst j)) out -1))))
    out))

(define (int-list num)
"Convert an integer to a list of digits"
  (if (zero? num) '(0)
  (let (out '())
    (while (!= num 0)
      (push (% num 10) out)
      (setq num (/ num 10))) out)))

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

Funzione che verifica se un numero appartiene alla sequenza:

(define (check? num)
  (if (< num 10)
      nil
      (for-all (fn(x) (prime? (+ (x 0) (x 1)))) (couples (int-list num)))))

Proviamo:

(check? 65)
;-> true

(filter check? (sequence 1 1200))
;-> (11 12 14 16 20 21 23 25 29 30 32 34 38 41 43 47 49
;->  50 52 56 58 61 65 67 70 74 76 83 85 89 92 94 98 111
;->  112 114 116 121 141 161 203 205 211 230 250 302 320 411
;->  502 520 611 1111 1112 1114 1116 1121 1141 1161)

============================================================================

