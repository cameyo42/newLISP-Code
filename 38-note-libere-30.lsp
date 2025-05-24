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


============================================================================

