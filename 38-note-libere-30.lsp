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


--------------------------------------------------------------------
Numero di 1 nella rappresentazione binaria dell'n-esimo numero primo
--------------------------------------------------------------------

Sequenza OEIS A014499:
Number of 1's in binary representation of n-th prime.
  1, 2, 2, 3, 3, 3, 2, 3, 4, 4, 5, 3, 3, 4, 5, 4, 5, 5, 3, 4, 3, 5, 4,
  4, 3, 4, 5, 5, 5, 4, 7, 3, 3, 4, 4, 5, 5, 4, 5, 5, 5, 5, 7, 3, 4, 5,
  5, 7, 5, 5, 5, 7, 5, 7, 2, 4, 4, 5, 4, 4, 5, 4, 5, 6, 5, 6, 5, 4, 6,
  6, 4, 6, 7, 6, 7, 8, 4, 5, 4, 5, 5, 5, 7, 5, 7, 7, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (bit1-count num)
"Count the bit 1 of an integer"
  (let (counter 0)
    (while (> num 0)
      (setq num (& num (- num 1)))
      (++ counter)
    )
    counter))

(define (seq limite)
  (let (out '())
    (for (num 2 limite)
      (if (prime? num) (push (bit1-count num) out -1)))
    out))

(seq 500)
;-> (1 2 2 3 3 3 2 3 4 4 5 3 3 4 5 4 5 5 3 4 3 5 4
;->  4 3 4 5 5 5 4 7 3 3 4 4 5 5 4 5 5 5 5 7 3 4 5
;->  5 7 5 5 5 7 5 7 2 4 4 5 4 4 5 4 5 6 5 6 5 4 6
;->  6 4 6 7 6 7 8 4 5 4 5 5 5 7 5 7 7 4 5 6 7 6 8 7 7 7)


------------------------------
Contare le occorrenze correnti
------------------------------

Data una lista di numeri interi esaminare ogni numero e sostituirlo con il suo numero di occorrenze fino a quel momento.
Esempio:
  lista = (1 3 5 1 2 2 5 6)
  numero = 1 ---> occorrenze 1
  numero = 3 ---> occorrenze 1
  numero = 5 ---> occorrenze 1
  numero = 1 ---> occorrenze 2
  numero = 2 ---> occorrenze 1
  numero = 2 ---> occorrenze 2
  numero = 5 ---> occorrenze 1
  numero = 6 ---> occorrenze 1
  output = (1 1 1 2 1 2 1 1)


Versione 1 (slice):

(define (conta lst)
  (let (out '())
    (dolist (el lst)
      ; conta le occorrenze del numero corrente fino al numero corrente
      ;(push (count (list el) (slice lst 0 (+ $idx 1))) out -1))
      ; versione più veloce 
      (push (length (ref-all el (slice lst 0 (+ $idx 1)))) out -1))
    (flat out)))

(setq lst '(1 3 5 1 2 2 5 6))
(conta lst)
;-> (1 1 1 2 1 2 2 1)

Versione 2 (lista temporanea):

(define (conta2 lst)
  (let (out '())
    (setq tmp '())
    (dolist (el lst)
      (push el tmp -1)
      (push (length (ref-all el tmp)) out -1))
    (flat out)))

(conta2 lst)
;-> (1 1 1 2 1 2 2 1)

Test di correttezza:

(setq t (rand 100 100))
(= (conta t) (conta2 t))
;-> true

Test di velocità:

(time (conta t) 1e4)
;-> 843.773
(time (conta2 t) 1e4)
;-> 532.603


--------------------------------
Programmazione non discriminante
--------------------------------

Una stringa è chiamata "non discriminante" se ciascuno dei caratteri della stringa appare lo stesso numero di volte e almeno due volte.

Esempi
"aa1a21132233" è non discriminante perché ciascuno dei caratteri , "a", "1", "2" e " 3" appare tre volte.
"abbaabb" non è non discriminante perché "b" appare 4 volte e "a" 3 volte.
"abc" non è non discriminante perché i caratteri non appaiono almeno due volte.

Scrivere una funzione non discriminante che restituisca true se una data stringa è non discriminante e un valore nil altrimenti.
Inoltre, passando alla funzione la funzione stessa (il proprio codice sorgente) devee restituire true.

Come sorgente del programma utilizziaamo la rappresentazione interna di newLISP.
Per esempio:

(define (test a b) (+ a b))
test
"(lambda (a b) (+ a b))" ; stringa da valutare

Funzione non discriminante:

(define (d s)
  (letn ( (l (explode s))
          (u (unique l))
          (c (flat (count u l))) )
    (setq x "")
    (setq y "")
    (setq z "0000000000000000000000000000011111111111111111111111111111=============================>>>>>>>>>>>>>>>>>>>>>>>>>>>>>bbbbbbbbbbbbbbbbbbbbbbbbbbbbbfffffffffffffffffffffffffffffiiiiiiiiiiiiiiiiiiiiiiiiiiiiimmmmmmmmmmmmmmmmmmmmmmmmmmmmmzzzzzzzzzzzzzzzzzzzzzzzzzzzzzooooooooooooooooooooooooooooxxxxxxxxxxxxxxxxxxxxxxxxxxxxyyyyyyyyyyyyyyyyyyyyyyyyyyyydddddddddddddddddddddddddddpppppppppppppppppppppppppppccccccccccccccccccccccccccnnnnnnnnnnnnnnnnnnnnnnnnnnqqqqqqqqqqqqqqqqqqqqqqqqqqaaaaaaaaaaaaaaaaaaaaaaaaasssssssssssssssssssssssssuuuuuuuuuuuuuuuuuuuuuuuuu\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"tttttttttttttttttttttttteeeeeeeeeeeeeeeeeeeeeeellllllllllllllllllllll(((((((((((())))))))))))\\\\\\")
    (and (apply = c) (> (c 0) 1))))

Proviamo:

(d "aa1a21132233")
;-> true
(d "abbaabb")
;-> nil
(d "abc")
;-> nil
(d (string d))
;-> true

Vediamo la rappresentazione interna della funzione:

d
(lambda (s)
 (letn ((l (explode s)) (u (unique l)) (c (flat (count u l))))
  (setq x "")
  (setq y "")
  (setq z "0000000000000000000000000000011111111111111111111111111111=============================>>>>>>>>>>>>>>>>>>>>>>>>>>>>>bbbbbbbbbbbbbbbbbbbbbbbbbbbbbfffffffffffffffffffffffffffffiiiiiiiiiiiiiiiiiiiiiiiiiiiiimmmmmmmmmmmmmmmmmmmmmmmmmmmmmzzzzzzzzzzzzzzzzzzzzzzzzzzzzzooooooooooooooooooooooooooooxxxxxxxxxxxxxxxxxxxxxxxxxxxxyyyyyyyyyyyyyyyyyyyyyyyyyyyydddddddddddddddddddddddddddpppppppppppppppppppppppppppccccccccccccccccccccccccccnnnnnnnnnnnnnnnnnnnnnnnnnnqqqqqqqqqqqqqqqqqqqqqqqqqqaaaaaaaaaaaaaaaaaaaaaaaaasssssssssssssssssssssssssuuuuuuuuuuuuuuuuuuuuuuuuu\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"tttttttttttttttttttttttteeeeeeeeeeeeeeeeeeeeeeellllllllllllllllllllll(((((((((((())))))))))))\\\\\\")
  (and (apply = c) (> (c 0) 1))))

Vediamo come è stata costruita la funzione:

1) Funzione iniziale che verifica le stringhe non discriminanti

(define (d s)
  (letn ( (l (explode s))
          (u (unique l))
          (c (flat (count u l))) )
    (and (apply = c) (> (c 0) 1))))

2) Aggiungiamo tre espressioni di assegnazione a stringhe vuote che serviranno per ospitare i caratteri che mancano per far si che la funzione stessa sia non discriminante.

(define (d s)
  (letn ( (l (explode s))
          (u (unique l))
          (c (flat (count u l))) )
    (setq x "")
    (setq y "")
    (setq z "")
    (and (apply = c) (> (c 0) 1))))

3) Crezione della lista dei caratteri mancanti:
(setq p (explode (string d)))
(setq u (unique p))
(setq ch (sort (map list (count u p) u)))
;-> ((1 "0") (1 "1") (1 "=") (1 ">") (1 "b") (1 "f") (1 "i") (1 "m")
;->  (1 "z") (2 "o") (2 "x") (2 "y") (3 "d") (3 "p") (4 "c") (4 "n")
;->  (4 "q") (5 "a") (5 "s") (5 "u") (6 "\"") (6 "t") (7 "e") (8 "l")
;->  (18 "(") (18 ")") (30 " "))

4) Creazione della stringa con tutti i caratteri mancanti:

(setq out '())
(dolist (el ch) (push (dup (el 1) (- 30 (el 0))) out -1))
(join out)
"000000...((())))))))))))"

5) Assegnazione della stringa alla variabile "z":

(define (d s)
  (letn ( (l (explode s))
          (u (unique l))
          (c (flat (count u l))) )
    (setq x "")
    (setq y "")
    (setq z "0000000000000000000000000000011111111111111111111111111111=============================>>>>>>>>>>>>>>>>>>>>>>>>>>>>>bbbbbbbbbbbbbbbbbbbbbbbbbbbbbfffffffffffffffffffffffffffffiiiiiiiiiiiiiiiiiiiiiiiiiiiiimmmmmmmmmmmmmmmmmmmmmmmmmmmmmzzzzzzzzzzzzzzzzzzzzzzzzzzzzzooooooooooooooooooooooooooooxxxxxxxxxxxxxxxxxxxxxxxxxxxxyyyyyyyyyyyyyyyyyyyyyyyyyyyydddddddddddddddddddddddddddpppppppppppppppppppppppppppccccccccccccccccccccccccccnnnnnnnnnnnnnnnnnnnnnnnnnnqqqqqqqqqqqqqqqqqqqqqqqqqqaaaaaaaaaaaaaaaaaaaaaaaaasssssssssssssssssssssssssuuuuuuuuuuuuuuuuuuuuuuuuu\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"tttttttttttttttttttttttteeeeeeeeeeeeeeeeeeeeeeellllllllllllllllllllll(((((((((((())))))))))))\\\\\\")
    (and (apply = c) (> (c 0) 1))))

6) Adesso la funzione è non discriminante

(d (string d))
;-> true

Nota: non abbiamo utilizzato le variabili "x" e "y", ma toglierle renderebbe la funzione discriminante.


-------------------------------------------------
Media massima di sottoliste con elementi contigui
-------------------------------------------------

Data una lista di interi e un intero K, determinare, tra tutte le sottoliste di lunghezza K, quella che ha la media massima.
Esempio:
lista = (1 3 6 4 -3 9 4)
K = 3
output = 

(setq lst '(1 3 6 4 -3 9 4))

(define (media-massima lst k)
  (local (out media-max len sublst media)
    (setq out '())
    (setq media-max -1e99)
    (setq len (length lst))
    (for (i 0 (- len k))
      (setq sublst (slice lst i k))
      (setq media (div (apply add sublst) k))
      ;(println media { } sublst)
      (when (> media media-max)
        (setq media-max media)
        (setq out sublst)))
    (list media-max out)))

Proviamo:

(media-massima lst 3)
;-> (4.333333333333333 (3 6 4))

(media-massima lst 1)
;-> (9 (9))


-----------------------------------------------------------------------------
Percorso minimo (BFS) e massimo (DFS) in un labirinto (shortest/longest path)
-----------------------------------------------------------------------------

Consideriamo un labirinto rappresentato da una matrice binaria MxN in cui 0 è un passaggio e 1 è un muro.

Percorso minimo (shortest path)
-------------------------------
Per trovare il percorso più breve da un punto A ad un punto B, l'algoritmo migliore dipende da alcuni fattori (ad esempio, costi uniformi, dimensione del labirinto), ma nel caso tipico di costo uniforme, la scelta migliore e più semplice è la Ricerca in ampiezza (BFS - Breadth First Search):
- Funziona perfettamente su griglie non pesate.
- Garantisce il percorso più breve in termini di numero di passi.
- Semplice da implementare.
- Utilizza una coda per esplorare tutti i nodi alla stessa distanza prima di andare in profondità.

Complessità temporale:
O(M*N) dove M e N sono le dimensioni del labirinto.

Algoritmo BFS (Breadth First Search)
------------------------------------
L'algoritmo BFS (Breadth-First Search) è una ricerca in ampiezza: esplora prima tutte le celle vicine (a distanza 1), poi quelle a distanza 2, poi 3, ecc.
In pratica: avanza "a onde" e garantisce di trovare il cammino più breve in numero di passi.

Passaggi principali:

1. Inizializzazione
   - Crea una coda ('queue') e ci inserisce la cella 'start' con il cammino iniziale.
   - Crea una matrice 'visited' per ricordare quali celle sono già state esplorate.
2. Loop principale
   - Finché la coda non è vuota:
     - Estrai il primo elemento: '(cella corrente, cammino corrente)'
     - Se la cella è il 'goal', restituisci il cammino -> è il più corto!
     - Altrimenti, esplora tutte le celle adiacenti (nord, sud, est, ovest — o diagonali se abilitate):
       - Se la cella è valida (dentro i limiti, non è un muro, non è già visitata):
         - Segnala come visitata
         - Inserisci in coda: nuova cella + cammino aggiornato
3. Se la coda si svuota senza raggiungere il goal
   - Non esiste nessun cammino -> restituisci 'nil'.

Proprietà del BFS:
- Trova il cammino più corto (se tutti i movimenti costano uguale).
- Non esplora cammini inutili dopo aver trovato il più breve.
- Funziona bene su griglie o grafi non pesati.

Limitazioni:
- Può richiedere molta memoria, perché deve tenere in coda tutte le celle da esplorare.
- Non adatto se vuoi il cammino più lungo → serve DFS o tecniche diverse.

Immagina che da 'start' si diffonda un'onda.
Appena l’onda tocca 'goal', hai trovato il cammino più breve.
Non serve continuare oltre.

Funzione che implementa l'algoritmo BFS:

(define (bfs maze start goal)
  ; get number of rows in the maze
  (letn ((rows (length maze))
         ; get number of columns
         (cols (length (maze 0)))
         ; initialize BFS queue
         (queue '())
         ; visited matrix: nil = unvisited, true = visited
         (visited (array-list (array rows cols '(nil))))
         ; parent matrix: stores previous node to reconstruct path
         (parent (array-list (array rows cols '(nil))))
         ; 4 movement directions: right, down, left, up
         (directions '((0 1) (1 0) (0 -1) (-1 0)))
         ; 8 movement directions (including diagonals)
         ;(directions '((0 1) (1 0) (0 -1) (-1 0) (1 1) (1 -1) (-1 1) (-1 -1)))
         ; path will hold the final result (or remain nil)
         (path nil)
         (node nil)
         (nx nil)
         (ny nil)
         (found nil))
    ; enqueue the start position
    (push start queue -1)
    ; mark start as visited
    (setf (visited (start 0) (start 1)) true)
    ; main BFS loop: while queue not empty and goal not found
    (while (and queue (not found))
      ; dequeue next node to explore
      (setq node (pop queue))
      ; check if goal has been reached
      (if (= node goal)
          (begin
            ; initialize backtracking from goal
            (setq cursor node)
            (setq path '())
            ; build path by walking back through parent matrix
            (while cursor
              (push cursor path)
              (setq cursor (parent (cursor 0) (cursor 1))))
            ; reverse the path so it goes from start to goal
            (setq path (reverse path))
            ; signal that path is found to break loop
            (set 'found true)))
      ; explore all 4 neighboring cells
      (dolist (dir directions)
        ; compute new x and y positions
        (setq nx (+ (node 0) (dir 0)))
        (setq ny (+ (node 1) (dir 1)))
        ; check bounds, free cell (0), and unvisited
        (if (and (>= nx 0) (< nx rows)
                 (>= ny 0) (< ny cols)
                 (= ((maze nx) ny) 0)
                 (not (visited nx ny)))
            (begin
              ; mark cell as visited
              (setf (visited nx ny) true)
              ; record parent to reconstruct path later
              (setf (parent nx ny) node)
              ; enqueue neighbor for future exploration
              (push (list nx ny) queue -1)))))
    ; return full path if found, or nil if no path exists
    path))

Proviamo:

(setq maze '((0 0 0 1)
             (1 0 1 0)
             (0 0 0 0)
             (1 1 1 0)))
(setq start '(0 0))
(setq goal  '(3 3))
(bfs maze start goal)
;-> ((3 3) (2 3) (2 2) (2 1) (1 1) (0 1) (0 0))

(setq maze '((0 0 0 0 0 0)
             (1 1 1 1 1 0)
             (0 0 0 0 0 0)
             (0 0 0 0 0 0)
             (0 0 0 0 0 0)
             (0 0 0 0 0 0)))
(setq start '(5 0))
(setq goal  '(0 0))
(bfs maze start goal)
;-> ((0 0) (0 1) (0 2) (0 3) (0 4) (0 5) (1 5) (2 5) (3 5) (4 5) (5 5)
;->  (5 4) (5 3) (5 2) (5 1) (5 0))

(setq maze '((0 0 0 0 0 0)
             (1 1 1 1 1 1)
             (0 0 0 0 0 0)
             (0 0 0 0 0 0)
             (0 0 0 0 0 0)
             (0 0 0 0 0 0)))
(setq start '(0 3))
(setq goal  '(3 2))
(bfs maze start goal)
;-> nil

Questa funzione considera il movimento in quattro direzioni:

4 Direzioni
-----------
(0 1)   ; right
(1 0)   ; down
(0 -1)  ; left
(-1 0)  ; up
(directions '((0 1) (1 0) (0 -1) (-1 0)))

Per considerare il movimento in otto direzioni:

8 Direzioni
-----------
(0 1)   ; right
(1 0)   ; down
(0 -1)  ; left
(-1 0)  ; up
(1 1)    ; down-right
(1 -1)   ; down-left
(-1 1)   ; up-right
(-1 -1)  ; up-left
(directions '((0 1) (1 0) (0 -1) (-1 0) (1 1) (1 -1) (-1 1) (-1 -1)))

Percorso massimo (longest path)
-------------------------------
Trovare il cammino semplice più lungo (senza nodi ripetuti) tra due punti in una griglia o in un grafo generico è molto più difficile che trovare il cammino più breve.
Perché?
- Il problema del cammino più breve è risolvibile in modo efficiente con BFS o Dijkstra perché quando raggiungiamo il punto finale siamo sicuri di avere il percorso ottimale.
- Il problema del cammino semplice più lungo è NP-hard nei grafi generici (incluse le griglie con ostacoli).
Ciò significa che non esi conosce un algoritmo efficiente per trovare sempre il cammino più lungo in tempo polinomiale.

Un possibile approccio per trovare il percorso più lungo da A a B è quello di utilizzare un algoritmo DFS con backtracking che esplora tutti i percorsi senza rivisitare i nodi, tenendo traccia del più lungo trovato finora. Questo metodo è esponenziale e pratico solo per labirinti di piccole dimensioni.

Trovare il cammino più lungo possibile (senza passare due volte dallo stesso punto) da una cella 'start' a una cella 'goal' in una griglia ('maze') con:
- 0 = spazio libero
- 1 = muro

Algoritmo DFS (Depth First Search)
----------------------------------
Il DFS visita profondamente (in profondità) un cammino possibile, continuando finché può avanzare.
Quando non ci sono più mosse valide, torna indietro (backtracking) e prova altre direzioni.

Passaggi principali:

1. Partenza
   - Si parte da una cella '(x y)' e si costruisce una lista 'path' con il cammino attuale.
2. Controlli
   - Se la cella è:
     - Fuori dai bordi del labirinto
     - Un muro
     - Già visitata
       allora si scarta.
3. Visita
   - Si segna la cella come visitata per non ripassarci.
   - Si aggiunge la cella a 'path'.
4. Obiettivo raggiunto
   - Se '(x y)' è il 'goal', si confronta 'path' con il cammino più lungo trovato finora.
   - Se è più lungo, si salva come nuovo massimo.
5. Espansione
   - Si prova ogni direzione possibile (anche diagonali, se incluse).
   - Per ogni direzione, si chiama ricorsivamente DFS sulla nuova cella vicina.
6. Backtracking
   - Dopo aver esplorato tutte le direzioni, si inverte la cella (da visitata a non visitata).
   - Questo permette ad altri cammini di passarci.

Vantaggi:
- Trova tutti i cammini possibili, scegliendo quello più lungo.
- È adatto per problemi dove bisogna esplorare tutte le soluzioni (es. labirinti, puzzle).

Limitazioni:
- DFS può essere lento su labirinti grandi o con molti cammini.
- Il numero di cammini cresce esponenzialmente: può diventare pesante in tempo/memoria.

Il DFS prova un cammino (es. tutto a destra), poi torna indietro e prova altre vie. Alla fine conserva il cammino più lungo che raggiunge il Goal.

Differenze BFS/DFS
------------------
| Aspetto          | BFS                            | DFS                        |
| ---------------- | ------------------------------ | -------------------------- |
| Obiettivo tipico | Cammino più corto              | Cammino più lungo          |
| Strategia        | Ampiezza (livello per livello) | Profondità (fino in fondo) |
| Struttura        | Coda                           | Stack (o ricorsione)       |
| Visita celle     | Distanza crescente             | Arbitraria                 |

Funzione che implementa l'algoritmo DFS:

(define (dfs maze start goal)
  ; get number of rows in the maze
  (letn ((rows (length maze))
         ; get number of columns in the maze
         (cols (length (maze 0)))
         ; create a visited matrix initialized to nil
         (visited (array-list (array rows cols '(nil))))
         ; 4 movement directions: right, down, left, up
         (directions '((0 1) (1 0) (0 -1) (-1 0)))
         ; 8 movement directions (including diagonals)
         ;(directions '((0 1) (1 0) (0 -1) (-1 0) (1 1) (1 -1) (-1 1) (-1 -1)))
         ; store the longest path found
         (longest-path '())
         ; store the length of the longest path
         (max-length 0))
    ; define the recursive DFS function
    (define (dfs-recursive x y path)
      ; check boundaries, walls or visited cells: stop if invalid
      (if (or (< x 0) (>= x rows) (< y 0) (>= y cols)
              (= ((maze x) y) 1)
              (visited x y))
          nil
          (begin
            ; mark current cell visited
            (setf (visited x y) true)
            ; extend current path with this cell
            (letn ((new-path (cons (list x y) path)))
              ; if goal reached, update longest path if longer
              (if (and (= x (goal 0)) (= y (goal 1)))
                  (when (> (length new-path) max-length)
                    (set 'max-length (length new-path))
                    (set 'longest-path (reverse new-path)))
                  ; else continue DFS on all neighbors
                  (dolist (dir directions)
                    (dfs-recursive (+ x (dir 0)) (+ y (dir 1)) new-path))))
            ; backtrack: unmark current cell visited
            (setf (visited x y) nil))))
    ; start DFS from the start position with empty path
    (dfs-recursive (start 0) (start 1) '())
    ; return the longest path found or nil if none
    (if (> max-length 0) longest-path nil)))

Funzione che stampa la matrice (maze) con il percorso (path):

(define (show maze path)
  (local (rows cols solved)
    ; rows of maze
    (setq rows (length maze))
    ; cols of maze
    (setq cols (length (maze 0)))
    ; copy of maze
    (setq solved maze)
    ; Add solution path (".") to solved maze
    (dolist (el path) (setf (solved (el 0) (el 1)) "."))
    ; print solved maze with solution path
    (for (i 0 (- rows 1))
      (for (j 0 (- cols 1))
        (print " " (solved i j)))
      (println)) '>))

Proviamo:

(set 'maze '((0 0 0 1)
             (1 0 1 0)
             (0 0 0 0)
             (1 1 1 0)))
(set 'start '(0 0))
(set 'goal '(3 3))
(dfs maze start goal)
;-> ((0 0) (0 1) (1 1) (2 1) (2 2) (2 3) (3 3))

(setq maze '((0 0 1 1 1 1)
             (1 0 0 1 1 1)
             (1 1 0 0 1 1)
             (1 1 1 0 0 1)
             (1 1 1 1 0 0)
             (1 1 1 1 1 0)))
(set 'start '(0 0))
(set 'goal '(5 5))
(dfs maze start goal)
;-> ((0 0) (0 1) (1 1) (1 2) (2 2) (2 3) (3 3) (3 4) (4 4) (4 5) (5 5))
(bfs maze start goal)
;-> ((5 5) (4 5) (4 4) (3 4) (3 3) (2 3) (2 2) (1 2) (1 1) (0 1) (0 0))

(setq maze '((0 0 0)
             (0 0 0)
             (0 0 0)))
(set 'start '(0 0))
(set 'goal '(2 2))

(bfs maze start goal)
;-> ((2 2) (1 2) (0 2) (0 1) (0 0))

(show maze (bfs maze start goal))
;->  . . .
;->  0 0 .
;->  0 0 .

(dfs maze start goal)
;-> ((0 0) (0 1) (0 2) (1 2) (1 1) (1 0) (2 0) (2 1) (2 2))

(show maze (longest-path maze start goal))
;->  . . .
;->  . . .
;->  . . .

Vedi anche "Algoritmo A*" su "Note libere 30".


------------------------------------------------
British Mathematical Olympiad - 1996 - Problem 1
------------------------------------------------

British Mathematical Olympiad
-----------------------------
Round 1: Wednesday, 17th January 1996

Problem 1:
Consider the pair of four-digit positive integers (M,N) = (3600, 2500).
Notice that M and N are both perfect squares, with equal digits in two places, and differing digits in the remaining
two places. Moreover, when the digits differ, the digit in M is exactly one greater than the corresponding digit in N.
Find all pairs of four-digit positive integers (M,N) with these properties.

Condizioni:

1) M e N quadrati perfetti
2) 2 cifre di M sono uguali a 2 cifre di N
3) Le cifre diverse di M (es. a b) sono maggiori di 1 delle corrispondenti cifre di N (es. a-1 b-1)

(setq a 3600)
(setq b 2500)

(define (square? num)
"Check if an integer is a perfect square"
  (let (isq (int (sqrt num)))
    (= num (* isq isq))))

(setq sq '())
(for (num 1000 9999) (if (square? num) (push num sq -1)))
sq
;-> (1024 1089 1156 1225 1296 1369 1444 1521 1600 1681 1764 1849 1936 2025
;->  2116 2209 2304 2401 2500 2601 2704 2809 2916 3025 3136 3249 3364 3481
;->  3600 3721 3844 3969 4096 4225 4356 4489 4624 4761 4900 5041 5184 5329
;->  5476 5625 5776 5929 6084 6241 6400 6561 6724 6889 7056 7225 7396 7569
;->  7744 7921 8100 8281 8464 8649 8836 9025 9216 9409 9604 9801)
(length sq)
;-> 68

Funzione che trova tutte le coppie:

(define (find-pairs)
  (setq coppie '())
  ; calcolo di tutti i numeri quadrati di quattro cifre
  (setq sq '())
  (for (num 1000 9999) (if (square? num) (push num sq -1)))
  (setq len-sq (length sq))
  ; ciclo per ogni coppia di numeri della lista di quadrati
  (for (i 0 (- len-sq 2))
    (for (j (+ i 1) (- len-sq 1))
      (setq a (sq i))
      (setq b (sq j))
      ; confronto tra le cifre
      (setq sa (explode (string a)))
      (setq sb (explode (string b)))
      ; calcolo delle cifre differenti
      (setq da (map int (difference sa sb true)))
      (setq db (map int (difference sb sa true)))
      ;(print a { } b { } da { } db) (read-line)
      ; entrambe le differenze devono valere 2
      (when (and (= (length da) 2) (= (length db) 2))
        ; controllo se le cifre differenti sono maggiori/minori di 1 tra loro
        (if (or (and (= (da 0) (+ (db 0) 1)) (= (da 1) (+ (db 1) 1)))
                (and (= (da 0) (- (db 0) 1)) (= (da 1) (- (db 1) 1))))
            (push (list a b) coppie -1)))
    )
  )
  coppie)

(find-pairs)
;-> ((1296 9025) (1521 2500) (2500 3600) (2916 9025)
;->  (3969 8836) (4624 5625) (7396 8649) (9025 9216))


------------------------------------------------
British Mathematical Olympiad - 1996 - Problem 4
------------------------------------------------

British Mathematical Olympiad
Round 1: Wednesday, 17th January 1996

Problem 4:
For any real number x, let [x] denote the greatest integer which is less than or equal to x.
Define:

  q(n) = floor(n / floor(sqrt(n))) for n = 1, 2, 3,...

Determine all positive integers n for which q(n) > q(n + 1).

Solution: a(n) > a(n+1) iff n = m^2 - 1 with m >= 2

Sequenza OEIS A079643:
a(n) = floor(n/floor(sqrt(n))).
  1, 2, 3, 2, 2, 3, 3, 4, 3, 3, 3, 4, 4, 4, 5, 4, 4, 4, 4, 5, 5, 5, 5, 6,
  5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8,
  7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 9, 8, 8, 8, 8, 8, 8, 8, 8, 9,
  9, 9, 9, 9, 9, 9, 9, 10, 9, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10,
  10, 10, 10, 10, 11, ...

(define (a n) (/ n (int (sqrt n))))

(map a (sequence 1 99))
;-> (1 2 3 2 2 3 3 4 3 3 3 4 4 4 5 4 4 4 4 5 5 5 5 6
;->  5 5 5 5 5 6 6 6 6 6 7 6 6 6 6 6 6 7 7 7 7 7 7 8
;->  7 7 7 7 7 7 7 8 8 8 8 8 8 8 9 8 8 8 8 8 8 8 8 9
;->  9 9 9 9 9 9 9 10 9 9 9 9 9 9 9 9 9 10 10 10 10 10
;->  10 10 10 10 11)


----------------
Liste visitabili
----------------

Abbiamo una lista di numeri interi positivi.
L'obiettivo è visitare ogni indice della lista una volta (e una sola volta).
Partendo da un elemento, dobbiamo esaminare il valore memorizzato in quell'elemento e poi spostarsi di altrettanti spazi a sinistra o a destra (se è possibile farlo entro i limiti della lista).
Se è possibile visitare tutti gli elementi in questo modo, restituiamo 'true', altrimenti 'nil'.
Regole
- Possiamo iniziare da qualsiasi indice.
- Possiamo visitare ogni elemento una sola volta.
- Un elemento è "visitato" una volta che ci si è arrivati (ovvero: passare sopra un elemento non conta come visitarlo).
- Non è necessario spostarsi dall'elemento visitato per ultimo.

Algoritmo
Per ogni indice iniziale possibile:
  Effettuare una visita in profondità (DFS) (o in ampiezza (BFS)).
  Tenere traccia dei nodi visitati.
  Se riusciamo a visitare tutti gli indici esattamente una volta, restituiamo true.
  Se nessun indice iniziale porta a una visita completa, restituiamo nil.

Funzione che verifica se una lista è visitabile:

(define (visita-possibile? lst)
  ; Usiamo variabili locali: 'trovato' per indicare se una soluzione è stata trovata,
  ; 'visitati' per memorizzare gli indici visitati durante la ricerca.
  (local (trovato visitati)
    ; Definizione interna della funzione ricorsiva DFS (visita in profondità).
    ; Si sposta da un indice 'pos' a sinistra o a destra secondo il valore presente nella lista.
    (define (dfs pos)
      ; Se l'indice è fuori dai limiti oppure già visitato, interrompiamo la ricorsione.
      (if (or (< pos 0) (>= pos (length lst)) (ref pos visitati))
          nil
          (begin
            ; Aggiungiamo l'indice corrente alla lista dei visitati.
            (push pos visitati)
            (let ((val (lst pos))) ; Prendiamo il valore corrente dalla lista.
              ; Proviamo a muoverci sia in avanti che indietro ricorsivamente.
              (dfs (+ pos val))
              (dfs (- pos val))))))
    ; Inizializziamo 'trovato' a nil. Verrà impostato a true se troviamo una partenza valida.
    (setq trovato nil)
    ; Cicliamo su tutti gli indici della lista come possibili punti di partenza.
    ; Il ciclo si interrompe se 'trovato' diventa true (grazie al quarto parametro di 'for').
    (for (i 0 (- (length lst) 1) 1 trovato)
      ; Reinizializziamo 'visitati' a ogni nuova partenza.
      (setq visitati '())
      ; Lanciamo la DFS a partire dall'indice corrente.
      (dfs i)
      ; Se la lunghezza della lista dei visitati corrisponde alla lunghezza della lista,
      ; significa che siamo riusciti a visitare ogni indice una sola volta.
      (when (= (length visitati) (length lst))
        (setq trovato true) ; Abbiamo trovato una soluzione valida.
        (println "Partenza valida da indice: " i)
        (println "Percorso: " visitati)))
    ; La funzione restituisce true se abbiamo trovato un percorso valido, altrimenti nil.
    trovato))

Proviamo:

(visita-possibile? '(1 1 1 1))
;-> Partenza valida da indice: 0
;-> Percorso: (3 2 1 0)
;-> true
(visita-possibile? '(2 1 2 1))
;-> nil
(visita-possibile? '(4 2 6 1 3))
;-> Partenza valida da indice: 0
;-> Percorso: (2 3 1 4 0)
;-> true
(visita-possibile? '(6 4 3 2 1))
;-> nil
(visita-possibile? '(10))
;-> Partenza valida da indice: 0
;-> Percorso: (0)
;-> true
(visita-possibile? '(1 2))
;-> Partenza valida da indice: 0
;-> Percorso: (1 0)
;-> true
(visita-possibile? '(2 3))
;-> nil

(setq test1 '(98 21 27 18 20 62 29 24 37 84 3 5 13 53 55 84 34 75 39 53 17
   59 10 37 22 33 21 51 23 66 9 31 32 9 98 13 38 31 16 31 21 8 23 22 40 1
   50 15 28 7 13 33 39 48 17 26 39 19 24 52 59 26 58 35 9 32 64 44 28 19
   24 6 13 30 16 48 21 62 1 70 3 48 30 8 48 1 60 2 78 48 22 32 81 59 41 19
   84 26 68 85))
(visita-possibile? test1)
;-> Partenza valida da indice: 0
;-> Percorso: (44 45 8 93 9 79 78 27 75 83 80 21 3 20 48 35 6 37 54 38 57 18
;->            51 28 63 50 16 11 92 17 56 49 41 89 87 61 40 68 90 74 36 84
;->            34 31 7 59 91 52 82 58 25 12 96 46 24 4 62 47 26 86 85 72 19
;->            42 33 81 55 76 95 29 2 66 13 10 88 69 14 99 15 77 71 97 65
;->            43 73 64 32 22 1 60 23 67 5 53 94 70 39 30 98 0)
;-> true

(setq test2 '(37 6 77 74 48 75 11 14 11 1 71 78 23 65 44 67 10 65 2 6
   14 19 8 26 4 14 71 35 38 16 66 5 12 10 29 55 5 61 14 11 43 35 17 34
   44 22 23 1 16 47 4 43 39 49 22 19 1 56 10 41 48 58 37 5 58 10 20 40
   25 6 28 13 23 14 8 44 9 26 6 65 12 34 65 30 28 85 64 26 50 81 25 21
   81 24 34 81 25 52 25 94))
(visita-possibile? test2)
;-> nil

La funzione è efficiente quanto basta per liste di dimensioni contenute.
Può essere estesa per restituire tutti i percorsi validi.

Riscriviamo ora la DFS in modo iterativo usando uno stack esplicito, come alternativa alla ricorsione.

(define (vis-pos? lst)
  (local (trovato visitati stack)
    ; Funzione DFS iterativa che esplora partendo dall'indice 'start'
    (define (dfs-iterativa start)
      (setq visitati '())          ; Lista degli indici visitati
      (setq stack (list start))    ; Stack iniziale con il punto di partenza
      ; Finché ci sono elementi nello stack...
      (while stack
        (let ((pos (pop stack)))   ; Prendiamo il prossimo nodo da esplorare
          (unless (or (< pos 0) (>= pos (length lst)) (ref pos visitati))
            (push pos visitati)    ; Aggiungiamo ai visitati
            (let ((val (lst pos)))
              ; Aggiungiamo le due possibili direzioni allo stack
              (push (+ pos val) stack)
              (push (- pos val) stack))))))
    (setq trovato nil)
    ; Provare tutte le possibili partenze
    (for (i 0 (- (length lst) 1) 1 trovato)
      (dfs-iterativa i)
      (when (= (length visitati) (length lst))
        (setq trovato true)
        (println "Partenza valida da indice: " i)
        (println "Percorso: " visitati)))
    trovato))

Proviamo:

(vis-pos? '(1 1 1 1))
;-> true
(vis-pos? '(2 1 2 1))
;-> nil
(vis-pos? '(4 2 6 1 3))
;-> true
(vis-pos? '(6 4 3 2 1))
;-> nil
(vis-pos? '(10))
;-> true
(vis-pos? '(1 2))
;-> true
(vis-pos? '(2 3))
;-> nil
(vis-pos? test1)
;-> true
(vis-pos? test1)
;-> true

Test di velocità:

(time (visita-possibile? test1) 100)
;-> 385.317
(time (vis-pos? test1) 100)
;-> 400.901

(time (visita-possibile? test2) 100)
;-> 1056.837
(time (vis-pos? test2) 100)
;-> 1286.47

La funzione ricorsiva è leggermente più veloce, ma la versione iterativa può gestire liste più grandi perchè non ha il problema dello stack-overflow.

Versione ricorsiva (senza commenti):

(define (visita-possibile? lst)
  (local (trovato visitati)
    (define (dfs pos)
      (if (or (< pos 0) (>= pos (length lst)) (ref pos visitati))
          nil
          (begin
            (push pos visitati)
            (let ((val (lst pos)))
              (dfs (+ pos val))
              (dfs (- pos val))))))
    (setq trovato nil)
    (for (i 0 (- (length lst) 1) 1 trovato)
      (setq visitati '())
      (dfs i)
      (when (= (length visitati) (length lst))
        (setq trovato true)
        (println "Partenza valida da indice: " i)
        (println "Percorso: " visitati)))
    trovato))

Versione iterativa (senza commenti):

(define (vis-pos? lst)
  (local (trovato visitati stack)
    (define (dfs-iterativa start)
      (setq visitati '())
      (setq stack (list start))
      (while stack
        (let ((pos (pop stack)))
          (unless (or (< pos 0) (>= pos (length lst)) (ref pos visitati))
            (push pos visitati)
            (let ((val (lst pos)))
              (push (+ pos val) stack)
              (push (- pos val) stack))))))
    (setq trovato nil)
    (for (i 0 (- (length lst) 1) 1 trovato)
      (dfs-iterativa i)
      (when (= (length visitati) (length lst))
        (setq trovato true)
        (println "Partenza valida da indice: " i)
        (println "Percorso: " visitati)))
    trovato))

Quando la lista da verificare è visitabile le nostre due funzioni (ricorsiva e iterativa) restituiscono il percorso come una lista di tutti gli indici visitati.
Comunque questi indici non sono nell'ordine con cui vengono visitati.
Per ottenere il percorso esatto dobbiamo tenere traccia dell’ordine con cui arriviamo agli indici.

(define (visitabile? lst)
  (local (trovato visitati stack percorso)
    (define (dfs-iterativa start)
      (setq visitati '())
      (setq percorso '())
      (setq stack (list start))
      (while stack
        (let ((pos (pop stack)))
          (unless (or (< pos 0) (>= pos (length lst)) (ref pos visitati))
            (push pos visitati)
            (push pos percorso -1) ; aggiungiamo in ordine di visita
            (let ((val (lst pos)))
              (push (+ pos val) stack)
              (push (- pos val) stack))))))
    (setq trovato nil)
    (for (i 0 (- (length lst) 1) 1 trovato)
      (dfs-iterativa i)
      (when (= (length visitati) (length lst))
        (setq trovato true)
        (println "Partenza valida da indice: " i)
        (println "Percorso ordinato: " percorso)))
    trovato))

(visitabile? '(1 1 1 1))
;-> Partenza valida da indice: 0
;-> Percorso ordinato: (0 1 2 3)
;-> true
(visitabile? '(2 1 2 1))
;-> nil
(visitabile? '(4 2 6 1 3))
;-> Partenza valida da indice: 0
;-> Percorso ordinato: (0 4 1 3 2)
;-> true
(visitabile? '(6 4 3 2 1))
;-> nil
(visitabile? '(10))
;-> Partenza valida da indice: 0
;-> Percorso ordinato: (0)
;-> true
(visitabile? '(1 2))
;-> Partenza valida da indice: 0
;-> Percorso ordinato: (0 1)
;-> true
(visitabile? '(2 3))
;-> nil

(setq test1 '(98 21 27 18 20 62 29 24 37 84 3 5 13 53 55 84 34 75 39 53 17
   59 10 37 22 33 21 51 23 66 9 31 32 9 98 13 38 31 16 31 21 8 23 22 40 1
   50 15 28 7 13 33 39 48 17 26 39 19 24 52 59 26 58 35 9 32 64 44 28 19
   24 6 13 30 16 48 21 62 1 70 3 48 30 8 48 1 60 2 78 48 22 32 81 59 41 19
   84 26 68 85))
(visitabile? test1)
;-> Percorso ordinato: (0 98 30 21 80 77 15 99 14 69 50 37 6 35 22 12 25
;->                     58 34 82 52 13 66 2 29 95 76 55 81 33 24 46 96 42
;->                     19 72 59 7 31 62 4 85 84 36 74 90 68 40 61 87 89
;->                     41 49 56 17 92 11 16 86 26 5 67 23 60 1 47 32 64
;->                     73 43 65 97 71 91 48 20 3 63 28 51 18 57 38 54 88
;->                     10 83 75 27 78 79 9 93 39 8 45 44 70 94 53)
;-> true

(setq test2 '(37 6 77 74 48 75 11 14 11 1 71 78 23 65 44 67 10 65 2 6
   14 19 8 26 4 14 71 35 38 16 66 5 12 10 29 55 5 61 14 11 43 35 17 34
   44 22 23 1 16 47 4 43 39 49 22 19 1 56 10 41 48 58 37 5 58 10 20 40
   25 6 28 13 23 14 8 44 9 26 6 65 12 34 65 30 28 85 64 26 50 81 25 21
   81 24 34 81 25 52 25 94))
(visitabile? test2)
;-> nil


------------------------------------------------
Somma della parte sinistra e destra di una lista
------------------------------------------------

Data una lista di numeri interi scrivere una funzione che restituisce la somma dei numeri della metà sinistra e quella dei numeri della metà destra.
Se la lista ha un numero dispari di elementi, allora l'elemento centrale va sommato con la parte destra.
La funzione deve essere la più corta possibile.

(define (sum-sx-dx lst)
  (let (len (length lst))
    (list (apply + (slice lst 0 (/ len 2)))
          (apply + (slice lst (+ (/ len 2)))))))

Funzione (92 caratteri):
(define(s l)(let(x(length l))(list(apply +(slice l 0(/ x 2)))(apply +(slice l(+(/ x 2)))))))

(setq lst '(5 3 6 2 1))
(s lst)
;-> (8 9)

(setq lst '(1 2 3 4 5 6 7 8 9 10))
(s lst)
;-> (15 40)


-----------------
Periodo di Pisano
-----------------

La successione di Fibonacci modulo 2 è una successione periodica di periodo 3: 0, 1, 1, 0, 1, 1, ...
Per qualsiasi numero intero n, la successione di Fibonacci modulo n è periodica e la lunghezza del ciclo periodico è detta Periodo di Pisano.

Sequenza OEIS A001175:
Pisano periods (or Pisano numbers): period of Fibonacci numbers mod n.
  1, 3, 8, 6, 20, 24, 16, 12, 24, 60, 10, 24, 28, 48, 40, 24, 36, 24, 18,
  60, 16, 30, 48, 24, 100, 84, 72, 48, 14, 120, 30, 48, 40, 36, 80, 24,
  76, 18, 56, 60, 40, 48, 88, 30, 120, 48, 32, 24, 112, 300, 72, 84, 108,
  72, 20, 48, 72, 42, 58, 120, 60, 30, 48, 96, 140, 120, 136, ...

(define (fibo-lutz num)
  (let ( (out '(0L 1L)) (x 1L) )
    (extend out (series x (fn (y) (+ x (swap y x))) (- num 1)))))

(define (pisano limite fibo-max)
  (local (seq fibs idx)
    (setq seq '(1))
    ; calcolo di fibo-max numeri di Fibonacci
    (setq fibs (fibo-lutz fibo-max))
    ; per ogni numero da 2 a limite...
    (for (num 2 limite)
      ; cerca il periodo della successione di Fibonacci modulo num
      (setq idx (find-repeat (map (fn(x) (% x num)) fibs) 2))
      (if (> (length idx) 1)
        (push (idx 1) seq -1)
        (push nil seq -1)))
    seq))

(pisano 67 250)
;-> (1 3 8 6 20 24 16 12 24 60 10 24 28 48 40 24 36 24 18
;->  60 16 30 48 24 100 84 72 48 14 120 30 48 40 36 80 24
;->  76 18 56 60 40 48 88 30 120 48 32 24 112 nil 72 84 108
;->  72 20 48 72 42 58 120 60 30 48 96 140 120 136)
In questo caso il valore 250 non è sufficiente come limite per il numero di numeri di Fibonacci.
Questo significa che il periodo del valore nil è maggiore di 250.
(ref nil (pisano 67 250))
;-> (49)

(pisano 67 350)
;-> (1 3 8 6 20 24 16 12 24 60 10 24 28 48 40 24 36 24 18
;->  60 16 30 48 24 100 84 72 48 14 120 30 48 40 36 80 24
;->  76 18 56 60 40 48 88 30 120 48 32 24 112 300 72 84 108
;->  72 20 48 72 42 58 120 60 30 48 96 140 120 136)

Potremmo scrivere: fibo-max = limite^2 (vedi Nota al termine.)

Vediamo la funzione "find-repeat" che trova il periodo della successione di Fibonacci modulo n.

(define (find-repeat lst distinct)
  (if (apply = lst) ; tutti gli elementi della lista sono uguali
      '(0)
      (let ( (sublist '()) (found nil) )
        ;; Creiamo la sublist finchè non troviamo due numeri distinti
        (dolist (el lst found)
          ;; Aggiungiamo ogni numero alla sublist
          (push el sublist -1)
          ;; Fermiamoci quando abbiamo due distinti
          (if (= (length (unique sublist)) distinct)
              (setq found true))
        )
        ;(println sublist)
        ;; Cerchiamo la sublist nella lista completa
        (filter (fn (i) (= sublist (i (length sublist) lst)))
                (flat (ref-all (sublist 0) lst))))))

Come funziona:
- Costruisce una sublist dinamicamente finché trova distinct numeri diversi.
(nel nostro caso per la successione di Fibonacci modulon sono sufficienti due numeri diversi)
- Cerca la sublist nella lista con filter, identificando tutte le occorrenze.
(vedi funzione "index-seq" su yo.lsp)
- Restituisce gli indici delle ripetizioni.
(nel nostro caso prendiamo il secondo indice (se esiste, altrimenti non esiste periodo))

(setq lst '(1 1 2 3 5 8 4 3 7 1 8 0 8 8 7 6 4 1 5 6 2 8 1 0 1 1 2 3 5 8))
(find-repeat lst 2)
;-> (0 24)
La lista comincia a ripetere gli stessi numeri iniziali alla posizione 24:
... 1 1 2 3 5 8.
(lst 24)
;-> 1

(find-repeat '(1 2 1 2 1 2) 2)
;-> (0 2 4)
(find-repeat '(1 2 1 2 1 2) 3)
;-> (0)
(find-repeat '(1 2 1 2 1 2 1 2 1 2) 3)
;-> (0)
(find-repeat '(2 2 2 2 2) 2)
;-> (0)

Data una lista di numeri (per esempio):
(setq lst '(1 1 2 3 5 8 4 3 7 1 8 0 8 8 7 6 4 1 5 6 2 8 1 0 1 1 2 3 5 8))
(map (fn(x) (% x 2)) (map fibo-i (sequence 1 300)))
(map (fn(x) (% x 3)) (map fibo-i (sequence 1 20)))

(setq u '(1 1 1 1 1))
(apply = u)
;-> true

Soluzione finale:

Come funziona:
- Costruisce una sublist dinamicamente finché trova distinct numeri diversi.
- Cerca la sublist nella lista con filter, identificando tutte le occorrenze.
- Restituisce gli indici delle ripetizioni.

(define (find-repeat lst distinct)
  (if (apply = lst)
      '(0)
      (let ( (sublist '()) (found nil) )
        ;; Creiamo la sublist finchè non troviamo due numeri distinti
        (dolist (el lst found)
          ;; Aggiungiamo ogni numero alla sublist
          (push el sublist -1)
          ;; Fermiamoci quando abbiamo due distinti
          (if (= (length (unique sublist)) distinct)
              (setq found true))
        )
        ;(println sublist)
        ;; Cerchiamo la sublist nella lista completa
        (filter (fn (i) (= sublist (i (length sublist) lst)))
                (flat (ref-all (sublist 0) lst))))))

(setq lst '(1 1 2 3 5 8 4 3 7 1 8 0 8 8 7 6 4 1 5 6 2 8 1 0 1 1 2 3 5 8))
(find-repeat lst 2)
;-> (0 24)
(find-repeat '(1 2 1 2 1 2) 2)
;-> (0 2 4)
(find-repeat '(1 2 1 2 1 2) 3)
;-> (0)
(find-repeat '(2 2 2 2 2) 2)
;-> (0)

Possiamo migliorarla per fare in modo che la sublist si possa costruire a partire dall'indice x invece che l'indice 0. In questo modo potremmo cercare ripetizioni anche che non iniziano dal primo elemento.
Permettere alla sublist di essere costruita da un indice qualsiasi anziché da 0 rende la funzione più flessibile, in modo che possiamo trovare ripetizioni ovunque nella lista.
Possiamo introdurre un parametro start-index per specificare da dove iniziare la costruzione della sublist.
La logica rimarrebbe simile, ma anziché partire sempre dall'inizio, considererebbe un punto arbitrario.
Un possibile schema:
- Inizia la costruzione della sublist dall'indice start-index.
- Aggiungi gli elementi fino a quando trovi distinct numeri diversi.
- Usa index-seq per trovare le ripetizioni della sequenza.

(define (find-repeat lst start-index distinct)
  (if (apply = lst)
      '(0)
      (let ( (sublist '()) (found nil) )
        ;; Creiamo la sublist a partire da start-index
        (dolist (el (slice lst start-index) found)
          ;; Aggiungiamo ogni numero alla sublist
          (push el sublist -1)
          ;; Fermiamoci quando abbiamo il numero desiderato di distinti
          (if (= (length (unique sublist)) distinct)
              (setq found true))
        )
        ;(println sublist)
        ;; Cerchiamo la sublist nella lista completa
        (filter (fn (i) (= sublist (i (length sublist) lst)))
                (flat (ref-all (sublist 0) lst))))))

(setq lst '(1 1 2 3 5 8 4 3 7 1 8 0 8 8 7 6 4 1 5 6 2 8 1 0 1 1 2 3 5 8))
(find-repeat lst 3 2)  ;; Cerca ripetizioni partendo dall'indice 5
;-> (3 27)
(lst 27)
;-> 3

Cosa cambia in questa versione?
 start-index: specifica l’indice da cui iniziare la ricerca.
 Slicing della lista: La sublist viene costruita da start-index, non da 0.
 Stesso metodo di ricerca: Usa index-seq per trovare la ripetizione.
Ora possiamo trovare ripetizioni in qualsiasi posizione della lista.

Nota: le ripetizioni vengono calcolate con 'distinct' numeri, quindi per alcune successioni potrebbero essere necessarie diverse prove prima di individuare il perido corretto.

Nota: Successioni modulo m hanno sempre periodo finito (al massimo m^k per successioni di ordine k).
Per "successioni di ordine k" si intendono le successioni ricorrenti lineari di ordine k, dove ogni termine dipende dai k termini precedenti.
Definizione formale:
Una successione ricorrente lineare di ordine k è definita da una relazione del tipo:

  a(n) = c1·a(n-1) + c2·a(n-2) + ... + ck·a(n-k)
  dove:
  - c1, c2, ..., ck sono coefficienti costanti
  - k è l'ordine della ricorrenza
  - Servono k condizioni iniziali: a(0), a(1), ..., a(k-1)

Esempi:

Ordine 1: a(n) = c·a(n-1)
Esempio: a(n) = 2·a(n-1) con a(0) = 1
Risultato: 1, 2, 4, 8, 16, ...

Ordine 2: a(n) = c1·a(n-1) + c2·a(n-2)
Esempio: Fibonacci -> a(n) = a(n-1) + a(n-2) con a(0)=0, a(1)=1
Risultato: 0, 1, 1, 2, 3, 5, 8, 13, ...

Ordine 3: a(n) = c1·a(n-1) + c2·a(n-2) + c3·a(n-3)
Esempio: Tribonacci -> a(n) = a(n-1) + a(n-2) + a(n-3)

Quando consideriamo una successione ricorrente di ordine k modulo m, lo "stato" della successione in ogni momento è determinato da k valori consecutivi.
Poiché ogni valore può assumere al massimo m valori diversi (da 0 a m-1), ci sono al massimo m^k stati possibili.
Questo significa che il periodo della successione non può superare m^k, perché dopo al massimo m^k passi dovremo necessariamente ripetere uno stato già visto, e da quel momento la successione diventa periodica.
L'ordine k determina quindi un limite superiore teorico per la lunghezza del periodo quando lavoriamo in aritmetica modulare.


----------------------
Stringhe ASCII casuali
----------------------

Scrivere una funzione che genera una stringa casuale che può essere composta dai seguenti caratteri:
1) minuscole (a..z)
2) maiuscole (A..Z)
3) numeri (0..9)
Tutti i caratteri devono avere la stessa probabilità di essere selezionati.
La funzione deve essere la più corta possibile.

(char "a")
;-> 97
(char "z")
;-> 122
(char "A")
;-> 65
(char "Z")
;-> 90
(char "0")
;-> 48
(char "9")
;-> 57

Creazione della lista dei codici ASCII dei caratteri da utilizzare:
(setq codici (flat (map sequence '(48 65 97) '(57 90 122))))
;-> (48 49 50 51 52 53 54 55 56 57 65 66 67 68 69 70 71 72 73 74 75 76
;->  77 78 79 80 81 82 83 84 85 86 87 88 89 90 97 98 99 100 101 102 103
;->  104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
;->  121 122)

Creazione della lista dei caratteri ASCII da utilizzare:
(setq ch (map char codici))
;-> ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F" "G"
;->  "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X"
;->  "Y" "Z" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
;->  "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")

Creazione della stringa finale:
(join (slice (randomize ch) 0 6))
;-> "noXd1u"

Funzione unica:
(define (s n) (join (slice (randomize 
              (map char (flat (map sequence '(48 65 97) '(57 90 122))))) 0 n)))

Funzione finale (94 caratteri):
(define(s n)(join(slice(randomize(map char(flat(map sequence '(48 65 97)'(57 90 122)))))0 n)))

Proviamo:

(s 0)
;-> ""
(s 10)
;-> "EmOsAkfuHb"
(s 6)
;-> "0MeNS4"

Verifichiamo che i caratteri hanno tutti la stessa probabilità.

(define (test max-len iter)
  (let ( (chars '()) (unici '()) )
    (for (i 1 iter)
      (extend chars (explode (s (+ (rand max-len) 1))))
    )
    (setq unici (unique chars))
    (sort (map list (count unici chars) unici))))

(time (println (test 10 1e6)))
;-> ((88157 "8") (88183 "6") (88206 "q") (88271 "r") (88302 "O") (88358 "h")
;->  (88368 "0") (88423 "m") (88428 "u") (88447 "W") (88448 "Z") (88458 "D")
;->  ...
;->  (89048 "Q") (89056 "z") (89091 "x") (89145 "v") (89480 "b"))
;-> 25568.785

Vedi anche "Generatore di stringhe casuali" su "Funzioni varie".


-------------------
Interi Alessandrini
-------------------

Chiameremo un intero positivo A "Intero Alessandrino" se esistono interi p, q, r tali che:

  A = p * q * r
e
  1/A = 1/p + 1/q + 1/r.

Ad esempio, 630 è un intero alessandrino (p = 5, q = −7, r = −18).

Sequenza OEIS A147811:
Alexandrian integers: numbers of the form n=pqr such that 1/n = 1/p - 1/q - 1/r for some integers p,q,r.
  6, 42, 120, 156, 420, 630, 930, 1428, 1806, 2016, 2184, 3192, 4950,
  5256, 8190, 8364, 8970, 10296, 10998, 12210, 17556, 19110, 21114,
  23994, 24492, 28050, 32640, 33306, 34362, 37506, 39270, 44310,
  52326, 57684, 57840, 70686, 74256, 79800, 83076, ...

Algoritmo
I numeri sono nella forma p*(p + d)*(p + (p^2 1)/d), dove d si estende sui divisori di (p^2 + 1) e p si estende su tutti gli interi positivi.

(define (divisors num)
"Generate all the divisors of an integer number"
  (local (f out)
    (cond ((= num 1) '(1))
          (true
           (setq f (factor-group num))
           (setq out '())
           (divisors-aux 0 1)
           (sort out)))))
; auxiliary function
(define (divisors-aux cur-index cur-divisor)
  (cond ((= cur-index (length f))
         (push cur-divisor out -1)
        )
        (true
         (for (i 0 (f cur-index 1))
           (divisors-aux (+ cur-index 1) cur-divisor)
           (setq cur-divisor (* cur-divisor (f cur-index 0)))))))

(define (factor-group num)
"Factorize an integer number"
  (if (= num 1) '((1 1))
    (letn ( (fattori (factor num))
            (unici (unique fattori)) )
      (transpose (list unici (count unici fattori))))))

Funzione che calcola un dato numero di numeri Alessandrini:

(define (alex nums)
  (local (out p2 divisori a)
    (setq out '())
    ; ciclo per ogni numero positivo p
    (for (p 1 nums)
      ; (p^2 + 1)
      (setq p2 (+ (* p p) 1))
      ; divisori di (p^2 + 1)
      (setq divisori (divisors p2))
      ; ciclo per ogni divisore di p corrente
      (dolist (d divisori)
        ; calcolo numero alessandrino
        (setq a (* p (+ p d) (+ p (/ p2 d))))
        ;(print p { } d { } a) (read-line)
        (push a out)))
    ; I numeri alessandrini non vengono calcolati in ordine.
    ; Anzi, alcuni numeri vengono ripetuti più volte.
    ; Il metodo calcola più numeri del necessario,
    ; quindi dobbiamo selezionare solo quelli fino a 'nums'.
    (slice (sort (unique out)) 0 nums)))

(setq oeis '(6 42 120 156 420 630 930 1428 1806 2016 2184 3192 4950
             5256 8190 8364 8970 10296 10998 12210 17556 19110 21114
             23994 24492 28050 32640 33306 34362 37506 39270 44310
             52326 57684 57840 70686 74256 79800 83076))
(length oeis)
;-> 39

(= (alex 39) oeis)
;-> true

(time (alex 1e4))
;-> 164.667

Nota: (alex 1e4) produce tutti risultati corretti (confrontati con OEIS).

Per calcolare grandi numeri occorre utilizzare i big-integer.

Funzione che calcola un dato numero di numeri Alessandrini (big-integer):

(define (alex-i nums)
  (local (ale p p2)
    (setq ale '())
    (setq p 1L)
    (while (< p nums)
      (setq p2 (+ (* p p) 1L))
      (dolist (d (divisors p2))
        (push (* p (+ p d) (+ p (/ p2 d))) ale))
      (++ p))
    (slice (sort (unique ale)) 0 nums)))

Proviamo:

(alex-i 39)
;-> (6L 42L 120L 156L 420L 630L 930L 1428L 1806L 2016L 2184L 3192L 4950L
;->  5256L 8190L 8364L 8970L 10296L 10998L 12210L 17556L 19110L 21114L
;->  23994L 24492L 28050L 32640L 33306L 34362L 37506L 39270L 44310L
;->  52326L 57684L 57840L 70686L 74256L 79800L 83076L)

(= (alex-i 39) oeis)
;-> true

(= (alex-i 1e4) (alex 1e4))
;-> true

(= (setq ai (alex-i 1e5)) (setq a (alex 1e5)))
;-> nil
(ai 0)
;-> 6
(a 0)
;-> -9223028910164634526

(time (alex-i 1e4))
;-> 225.293
(time (alex-i 1e5))
;-> 6563.964

(time (setq t (alex-i 500000)))
;-> 86341.745
(t 0)
;-> 6L
(t -1)
;-> 628833933090L

Possiamo calcolare p, q e r partendo da p e d.
Dalla formula fornita nella sequenza OEIS A147811, abbiamo:
- q = p + d
- r = p + (p^2 + 1)/d
- n = p * q * r
dove:
- p è un intero positivo qualsiasi
- d è un divisore di (p^2 + 1)

Esempio:
N = 630
- Se p = 5, allora p^2 + 1 = 26
- I divisori di 26 sono: 1, 2, 13, 26
- Con d = 2: q = 5 + 2 = 7, r = 5 + 26/2 = 18
- Quindi n = 5 * 7 * 18 = 630

Tuttavia, per ottenere i valori nell'esempio (p = 5, q = -7, r = -18), si deve considerare che la formula può essere adattata con segni appropriati per soddisfare la condizione con alcuni valori negativi.
Esempio:
1. Scegliamo un valore di p
2. Calcoliamo p^2 + 1
3. Troviamo tutti i divisori di (p^2 + 1)
4. Per ogni divisore d, calcoliamo:
   - q = p + d
   - r = p + (p^2 + 1)/d
   - n = p * q * r
Questo genera tutti gli interi alessandrini generati da quel particolare valore di p.


---------
Timer 555
---------

Astable mode
------------
An Astable Circuit has no stable state - hence the name "astable". The output continually switches state between high and low without without any intervention from the user, called a 'square' wave. This type of circuit could be used to give a mechanism intermittent motion by switching a motor on and off at regular intervals. It can also be used to flash lamps and LEDs, and is useful as a 'clock' pulse for other digital ICs and circuits.

Astable 555 Square Wave Calculator
----------------------------------
In an astable circuit, the output continually switches state between high and low without any intervention from the user.
The duration of the high and low states are based on what values you choose for R1, R2 and C1.

The formulas used are:
Time High (secs) = 0.693 * (R1 + R2) * C
Time Low (secs) = 0.693 * R2 * C
Frequency (hertz) = 1.44 / ((R1 + R2 + R2) * C)
Duty Cycle Percentage = (Th / (Th + Tl)) * 100
R1, R2 --> Ohms
C --> Farads

Circuit:

  +Vcc --------+-------------+-----+--------------
               |             |     |
               @             |     |
            R1 @             |     |
               @       +----------------+
               |       |     4     8    |
               +-------| 7              |
               |       |                |
               @       |     (555)      |
            R2 @    +--| 6            3 |----- Output
               @    |  |                |
               |    |  |                |
               +----+--| 2              |
               |       |     1     5    |
               |       +----------------+
               |             |     |
          C  =====           |     |
             =====           |    ===  0.1 nF
               |             |    === 
               |             |     |
               |             |     |
    0V --------+-------------+-----+--------------

Output:

    +----+        +----+        +----+        +----+        +----+
    |    |        |    |        |    |        |    |        |    |
    |    |        |    |        |    |        |    |        |    |
----+    +--------+    +--------+    +--------+    +--------+    +----

Time High (secs) = 0.693 * (R1 + R2) * C
Time Low (secs) = 0.693 * R2 * C
Frequency (hertz) = 1.44 / ((R1 + R2 + R2) * C)
Duty Cycle Percentage = (Th / (Th + Tl)) * 100

(define (t555 r1 r2 c)
  (letn ( (th (mul 0.693 (add r1 r2) c))
          (tl (mul 0.693 r2 c))
          (freq (div 1.44 (mul (add r1 r2 r2) c)))
          (duty (mul (div th (add th tl)) 100)))
     (println "Time High (secs) = " th)
     (println "Time Low (secs) = " tl)
     (println "Frequency (hertz) = " freq)
     (println "Duty Cycle Percentage = " duty) '>))

(t555  10000 5000 0.00005)
;-> Time High (secs) = 0.5197500000000001
;-> Time Low (secs) = 0.17325
;-> Frequency (hertz) = 1.44
;-> Duty Cycle Percentage = 75


---------------------
Timer 555 e Claude AI
---------------------

9 giugno 2025

******
UTENTE
******
In un circuito elettronico l'uscita cambia continuamente stato tra alto e basso senza alcun intervento da parte dell'utente.
La durata degli stati alto e basso si basa sui valori scelti per R1, R2 e C.
Le formule sono le seguenti:
  Time High (secs) = 0.693 * (R1 + R2) * C
  Time Low (secs) = 0.693 * R2 * C
  Frequency (hertz) = 1.44 / ((R1 + R2 + R2) * C)
  Duty Cycle Percentage = (Th / (Th + Tl)) * 100
Come scegliere R1, R2 e C partendo da valori predefiniti di:
Time High, Time Low e Frequency?

******
CLAUDE
******
Il circuito che descrivi è un oscillatore astabile, tipicamente realizzato con un timer 555.
Per calcolare i valori dei componenti R1, R2 e C partendo dai valori desiderati di Time High, Time Low e Frequency, devi risolvere il sistema di equazioni in modo inverso.

Ecco il procedimento:

## Metodo di calcolo
1. Scegli prima il valore del condensatore C
- Inizia scegliendo un valore standard per C (es: 1µF, 10µF, 100nF)
- Valori tipici: da 1nF a 1000µF a seconda della frequenza desiderata
2. Calcola R2 dal Time Low
R2 = Time Low / (0.693 × C)
3. Calcola R1 dal Time High
R1 = (Time High / (0.693 × C)) - R2

## Verifica dei risultati
Una volta calcolati i valori, verifica che:
- R1 > 0 (altrimenti il Time High è troppo piccolo rispetto al Time Low)
- I valori di R1 e R2 siano pratici (tipicamente tra 1kΩ e 1MΩ)
- La frequenza risultante sia quella desiderata usando: f = 1.44 / ((R1 + 2×R2) × C)

## Esempio pratico
Se vuoi:
- Time High = 1ms
- Time Low = 0.5ms  
- C = 100nF

Calcoli:
- R2 = 0.0005 / (0.693 × 100×10⁻⁹) = 7.2kΩ
- R1 = (0.001 / (0.693 × 100×10⁻⁹)) - 7.2kΩ = 7.6kΩ

Nota importante: Se R1 risulta negativo, significa che il Time High desiderato è troppo piccolo rispetto al Time Low. In questo caso devi aumentare il Time High o diminuire il Time Low, oppure usare un condensatore più piccolo.

******
UTENTE
******
Puoi scrivere un programma in newlisp che prende Time High e Time Low e calcola R1, R2 e C rispettando i vincoli che hai elencato?

******
CLAUDE
******
Ho creato un programma completo in NewLISP che calcola i componenti per un oscillatore astabile 555. 
Il programma include:

## Caratteristiche principali:
1. **Calcolo automatico**: Prova diversi valori standard di condensatori e trova la combinazione ottimale
2. **Valori standard**: Usa valori commerciali reali per resistenze e condensatori
3. **Verifica vincoli**: Controlla che R1 > 0 e che i valori siano nel range pratico (1kΩ - 1MΩ)
4. **Ottimizzazione**: Trova la soluzione con il minimo errore rispetto ai valori desiderati
5. **Formattazione intelligente**: Mostra i valori con le unità appropriate (µF, nF, kΩ, MΩ)

## Modalità d'uso:
- **Modalità interattiva**: Inserisci i tuoi valori di Time High e Time Low
- **Esempi predefiniti**: Tre esempi pratici (LED lampeggiante, oscillatore audio, clock digitale)
- **Uso come libreria**: Chiama direttamente '(calculate-555-components t-high t-low)'

Il programma mostra sia i valori calcolati teoricamente che quelli standard più vicini, insieme agli errori percentuali, così puoi valutare se la precisione è accettabile per la tua applicazione.

Per eseguirlo, salva il codice in un file (es. 'timer555.lsp') e lancialo con 'newlisp timer555.lsp'.

#!/usr/bin/env newlisp
; Calcolatore per oscillatore astabile 555 Timer
; Calcola R1, R2 e C partendo da Time High e Time Low

; Valori standard per condensatori (in Farad)
(setq standard-capacitors '(1e-12 10e-12 100e-12 1e-9 10e-9 100e-9 1e-6 10e-6 100e-6 1e-3))

; Valori standard per resistenze (in Ohm)
(setq standard-resistors '(1000 1200 1500 1800 2200 2700 3300 3900 4700 5600 6800 8200
                          10000 12000 15000 18000 22000 27000 33000 39000 47000 56000 68000 82000
                          100000 120000 150000 180000 220000 270000 330000 390000 470000 560000 680000 820000
                          1000000))

; Funzione per trovare il valore standard più vicino
(define (find-closest-standard value standards)
  (let ((min-diff 1e12) (closest 0))
    (dolist (std standards)
      (let ((diff (abs (sub value std))))
        (if (< diff min-diff)
            (begin
              (setq min-diff diff)
              (setq closest std)))))
    closest))

; Funzione per formattare i valori con unità appropriate
(define (format-capacitor c)
  (cond
    ((>= c 1e-3) (format "%.1f mF" (div c 1e-3)))
    ((>= c 1e-6) (format "%.1f µF" (div c 1e-6)))
    ((>= c 1e-9) (format "%.0f nF" (div c 1e-9)))
    (true (format "%.0f pF" (div c 1e-12)))))

(define (format-resistor r)
  (cond
    ((>= r 1000000) (format "%.1f MΩ" (div r 1000000)))
    ((>= r 1000) (format "%.1f kΩ" (div r 1000)))
    (true (format "%.0f Ω" r))))

; Funzione principale di calcolo
(define (calculate-555-components t-high t-low)
  (println "=== CALCOLATORE 555 TIMER ASTABILE ===")
  (println (format "Time High: %.3f ms" (mul t-high 1000)))
  (println (format "Time Low:  %.3f ms" (mul t-low 1000)))
  (println (format "Periodo:   %.3f ms" (mul (add t-high t-low) 1000)))
  (println (format "Frequenza: %.2f Hz" (div 1 (add t-high t-low))))
  (println (format "Duty Cycle: %.1f%%" (mul (div t-high (add t-high t-low)) 100)))
  (println)
  ; Prova diversi valori di condensatore
  (let ((best-solution nil) (best-score 1e12))
    (dolist (c standard-capacitors)
      (let ((r2-calc (div t-low (mul 0.693 c)))
            (r1-calc (sub (div t-high (mul 0.693 c)) (div t-low (mul 0.693 c)))))
        ; Verifica vincoli
        (if (and (> r1-calc 0)
                 (>= r1-calc 1000) (<= r1-calc 1000000)
                 (>= r2-calc 1000) (<= r2-calc 1000000))
            (begin
              ; Trova resistenze standard più vicine
              (let ((r1-std (find-closest-standard r1-calc standard-resistors))
                    (r2-std (find-closest-standard r2-calc standard-resistors)))
                ; Calcola errori con valori standard
                (let ((t-high-actual (mul 0.693 (mul (add r1-std r2-std) c)))
                      (t-low-actual (mul 0.693 (mul r2-std c))))
                  (let ((error-high (abs (div (sub t-high-actual t-high) t-high)))
                        (error-low (abs (div (sub t-low-actual t-low) t-low))))
                    (let ((total-error (add error-high error-low)))
                      (if (< total-error best-score)
                          (begin
                            (setq best-score total-error)
                            (setq best-solution (list c r1-std r2-std r1-calc r2-calc
                                                    t-high-actual t-low-actual error-high error-low))))))))))))
    ; Mostra risultati
    (if best-solution
        (begin
          (let ((c (nth 0 best-solution))
                (r1-std (nth 1 best-solution))
                (r2-std (nth 2 best-solution))
                (r1-calc (nth 3 best-solution))
                (r2-calc (nth 4 best-solution))
                (t-high-actual (nth 5 best-solution))
                (t-low-actual (nth 6 best-solution))
                (error-high (nth 7 best-solution))
                (error-low (nth 8 best-solution)))
            (println "SOLUZIONE OTTIMALE:")
            (println (format "C:  %s" (format-capacitor c)))
            (println (format "R1: %s (calcolato: %s)" (format-resistor r1-std) (format-resistor r1-calc)))
            (println (format "R2: %s (calcolato: %s)" (format-resistor r2-std) (format-resistor r2-calc)))
            (println)
            (println "PRESTAZIONI CON VALORI STANDARD:")
            (let ((freq-actual (div 1.44 (mul (add r1-std (mul 2 r2-std)) c)))
                  (duty-actual (mul (div t-high-actual (add t-high-actual t-low-actual)) 100)))
              (println (format "Time High: %.3f ms (errore: %.1f%%)"
                              (mul t-high-actual 1000) (mul error-high 100)))
              (println (format "Time Low:  %.3f ms (errore: %.1f%%)"
                              (mul t-low-actual 1000) (mul error-low 100)))
              (println (format "Frequenza: %.2f Hz" freq-actual))
              (println (format "Duty Cycle: %.1f%%" duty-actual)))))
        (println "ERRORE: Impossibile trovare una soluzione valida con i vincoli specificati!"))))

; Funzione di test con esempi
(define (test-examples)
  (println "=== ESEMPI DI CALCOLO ===")
  (println)
  ; Esempio 1: LED lampeggiante lento
  (println "ESEMPIO 1: LED lampeggiante (1Hz)")
  (calculate-555-components 0.7 0.3)
  (println)
  ; Esempio 2: Oscillatore audio
  (println "ESEMPIO 2: Oscillatore audio (1kHz)")
  (calculate-555-components 0.0007 0.0003)
  (println)
  ; Esempio 3: Clock digitale
  (println "ESEMPIO 3: Clock digitale (10kHz)")
  (calculate-555-components 0.00007 0.00003)
  (println))
; Funzione interattiva
(define (interactive-mode)
  (println "=== MODALITÀ INTERATTIVA ===")
  (print "Inserisci Time High (secondi): ")
  (let ((t-high (float (read-line))))
    (print "Inserisci Time Low (secondi): ")
    (let ((t-low (float (read-line))))
      (if (and (> t-high 0) (> t-low 0))
          (calculate-555-components t-high t-low)
          (println "ERRORE: I valori devono essere positivi!")))))
; Programma principale
(define (main)
  (println "CALCOLATORE COMPONENTI 555 TIMER")
  (println "================================")
  (println)
  (println "Scegli un'opzione:")
  (println "1) Modalità interattiva")
  (println "2) Esempi predefiniti")
  (print "Scelta (1-2): ")
  (let ((choice (read-line)))
    (cond
      ((= choice "1") (interactive-mode))
      ((= choice "2") (test-examples))
      (true (println "Scelta non valida!")))))

; Avvia il programma
(main)

; Per uso come libreria, puoi chiamare direttamente:
; (calculate-555-components 0.001 0.0005)  ; 1ms high, 0.5ms low
(calculate-555-components 0.00007 0.00003)

******
UTENTE
******
Funziona perfettamente.

******
CLAUDE
******
Perfetto! Sono contento che il programma funzioni bene per te.
Il calcolatore dovrebbe aiutarti a trovare rapidamente i valori dei componenti per il tuo oscillatore 555, tenendo conto sia dei vincoli tecnici che dei valori standard disponibili in commercio.
Se in futuro dovessi aver bisogno di modifiche o estensioni (come aggiungere altri valori standard di componenti, o calcolare parametri aggiuntivi come la corrente assorbita), sarà facile adattare il codice.
Buona progettazione con i tuoi circuiti 555!

Non male!!!


-------------------------
Sequenze Onda di ordine k
-------------------------

La sequenza Onda di ordine k=3 è la seguente:

Valore onda:     1  2    3    2  1  2    3    2  1
                 - --- ----- --- - --- ----- --- -
Numero sequenza: 1 2 2 3 3 3 4 4 5 6 6 7 7 7 8 8 9

Analizziamo la sequenza descritta:
- I valori dell'onda seguono il pattern: 1, 2, 3, 2, 1, 2, 3, 2, 1, ...
- Ogni numero x appare un numero di volte pari al valore dell'onda corrispondente
- La sequenza risultante è: 1, 2, 2, 3, 3, 3, 4, 4, 5, 6, 6, 7, 7, 7, 8, 8, 9, ...

In altre parole, i numeri della sequenza crescono sempre. 
E' il numero di ripetizioni che forma un'onda da 1 a k e da k a 1.

Definizione matematica:

Per una sequenza Onda di ordine k, possiamo definire:

1) Funzione Onda W(i)
   Per la posizione i-esima (i ≥ 1):

   W(i) = k - |((i-1) mod (2k-2)) - (k-1)|

   Per k=3:
   - Il periodo dell'onda è 2k-2 = 4
   - W(i) = 3 - |((i-1) mod 4) - 2|

2) Sequenza S(n) 
   L'n-esimo termine della sequenza è dato da:

   S(n) = min{x : Σ(i=1 to x) W(i) ≥ n}

   In altre parole, S(n) è il più piccolo intero x tale che la somma dei primi x valori dell'onda sia almeno n.

Verifica per k=3:
- W(1) = 1, W(2) = 2, W(3) = 3, W(4) = 2, W(5) = 1, W(6) = 2, ...
- S(1) = 1 (primo 1)
- S(2) = S(3) = 2 (due 2)
- S(4) = S(5) = S(6) = 3 (tre 3)
- S(7) = S(8) = 4 (due 4)
- E così via...

Formula alternativa ricorsiva
-----------------------------
Se definiamo T(x) come il numero totale di elementi fino al numero x:

T(x) = Σ(i=1 to x) W(i)

Allora: S(n) = x se e solo se T(x-1) < n ≤ T(x)

Nota: la sequenza Onda di ordine 1 ha tutti i numeri pari a 1.

; Funzione per calcolare il valore dell'onda alla posizione i
; per una sequenza di ordine k
(define (wave-value i k)
  (- k (abs (- (% (- i 1) (- (* 2 k) 2)) (- k 1)))))

; Funzione per calcolare S(n) - l'n-esimo termine della sequenza Onda
; Metodo iterativo efficiente
(define (wave-sequence n k)
  (let ((x 1) (total 0))
    (while (< total n)
      (setq total (+ total (wave-value x k)))
      (if (< total n) (++ x)))
    x))

; Funzione per calcolare S(n) - l'n-esimo termine della sequenza Onda
; Funzione alternativa più diretta usando somma cumulativa
(define (wave-sequence-alt n k)
  (let ((x 1) (sum 0))
    (while true
      (setq sum (+ sum (wave-value x k)))
      (if (>= sum n) (break))
      (++ x))
    x))

; Funzione per generare i primi n termini della sequenza
(define (generate-wave-sequence n k)
  (if (= k 1)
    (dup 1 n)
    (map (lambda (i) (wave-sequence i k)) (sequence 1 n))))

; Funzione per visualizzare la sequenza con i valori dell'onda
(define (display-wave-pattern n k)
  (println "Sequenza Onda di ordine k=" k)
  (if (= k 1)
    (begin
      (println "Valore onda: " (dup 1 n))
      (println "Sequenza S:  " (dup 1 n)))
    (begin
      (println "Valore onda: " (map (lambda (i) (wave-value i k)) (sequence 1 n)))
      (println "Sequenza S:  " (generate-wave-sequence (apply + (map (lambda (i) (wave-value i k)) (sequence 1 n))) k)))))

; Funzione di test per la verifica dei risultati
(define (test)
  ; Test con k=3 come nell'esempio
  (println "Test con k=3:")
  (println "Primi 20 termini: " (generate-wave-sequence 20 3))
  (println)
  ; Verifica con alcuni valori specifici
  (println "Verifica:")
  (println "S(1) = " (wave-sequence 1 3))
  (println "S(2) = " (wave-sequence 2 3))
  (println "S(3) = " (wave-sequence 3 3))
  (println "S(4) = " (wave-sequence 4 3))
  (println "S(9) = " (wave-sequence 9 3))
  (println "S(17) = " (wave-sequence 17 3))
  (println)
  ; Visualizza il pattern per k=3
  (display-wave-pattern 10 3)
  ; Test con altri valori di k
  (println "\nTest con k=1:")
  (println "Primi 10 termini: " (generate-wave-sequence 10 1))
  (println "\nTest con k=2:")
  (println "Primi 15 termini: " (generate-wave-sequence 15 2))
  (println "\nTest con k=4:")
  (println "Primi 15 termini: " (generate-wave-sequence 15 4))
'>)

; Esegue la funzione di test
(test)

; Test ulteriori
(generate-wave-sequence 20 2)
;-> (1 2 2 3 4 4 5 6 6 7 8 8 9 10 10 11 12 12 13 14)
(generate-wave-sequence 20 5)
;-> (1 2 2 3 3 3 4 4 4 4 5 5 5 5 5 6 6 6 6 7)
(generate-wave-sequence 20 10)
;-> (1 2 2 3 3 3 4 4 4 4 5 5 5 5 5 6 6 6 6 6)


--------------------------
Intersezione di intervalli
--------------------------

Data una lista di coppie di interi (L R) (intervallo di numeri da L a R), contare il numero di coppie univoche che si intersecano.
Una coppia (L1 R1) interseca la coppia (L2 R2) se L1 <= L2 <= R1 o L2 <= L1 <= R2.
Inoltre risulta sempre: L <= R per una singola coppia.

Metodo Brute-Force
------------------

Algoritmo
Controllo di ogni coppia di intervalli

; Funzione che verifica se due coppie si intersecano
(define (cross? c1 c2)
  (or (and (<= (c1 0) (c2 0)) (<= (c2 0) (c1 1)))
      (and (<= (c2 0) (c1 0)) (<= (c1 0) (c2 1)))))

(cross? '(1 2) '(3 4))
;-> nil
(cross? '(1 5) '(5 7))
;-> true

; Funzione che calcola il numero di coppie univoche che si intersecano
(define (num-cross lst)
  (let ( (conta 0) (pairs '()) )
    (for (i 0 (- (length lst) 2))
      (for (j (+ i 1) (- (length lst) 1))
          (if (cross? (lst i) (lst j)) (++ conta))))
    conta))

(num-cross '((1 2) (5 6) (1 5) (4 7)))
;-> 4

; Funzione che calcola le coppie univoche che si intersecano
(define (calc-cross lst)
  (let (pairs '())
    (for (i 0 (- (length lst) 2))
      (for (j (+ i 1) (- (length lst) 1))
          (if (cross? (lst i) (lst j)) (push (list (lst i) (lst j)) pairs -1))))
    pairs))

(calc-cross '((1 2) (5 6) (1 5) (4 7)))
;-> (((1 2) (1 5)) ((5 6) (1 5)) ((5 6) (4 7)) ((1 5) (4 7)))

La complessità temporale della funzione Brute-Force vale O(n^2).

Metodo Sweep-Line
-----------------

Funzionamento dell'algoritmo Sweep-Line:
1. Ordinare gli intervalli per punto di inizio ('sort')
2. Mantenere una lista 'end-points' dei punti finali degli intervalli già processati
3. Per ogni nuovo intervallo:
   - Rimuovere dalla lista i punti finali che non possono più intersecare ('filter')
   - Contare tutti gli intervalli rimanenti (che si intersecano)
   - Aggiungere il nuovo punto finale e mantenere la lista ordinata

La complessità temporale della funzione Sweep-Line vale O(n^2*log(n)) nel caso peggiore a causa del 'sort' ripetuto degli 'end-points'.

; Funzione per contare le coppie di intervalli che si intersecano
; usando l'algoritmo sweep line
(define (count-intersecting-pairs intervals)
  (let ((counts 0)
        (end-points '())
        (sorted-intervals (sort intervals)))
    ; Processa ogni intervallo in ordine
    (dolist (interval sorted-intervals)
      (let ((L (first interval))
            (R (last interval)))
        ; Rimuovi i punti finali che non possono più intersecare
        ; (quelli con R_prev < L)
        (setq end-points
              (filter (lambda (end) (>= end L)) end-points))
        ; Tutti gli intervalli rimanenti si intersecano con quello corrente
        (setq counts (+ counts (length end-points)))
        ; Aggiungi il punto finale corrente e mantieni ordinato
        (push R end-points)
        (setq end-points (sort end-points))))
    counts))

; Funzione ausiliaria per verificare se due intervalli si intersecano
(define (intersect? interval1 interval2)
  (let ((L1 (first interval1))
        (R1 (last interval1))
        (L2 (first interval2))
        (R2 (last interval2)))
    (<= (max L1 L2) (min R1 R2))))

; Implementazione brute force per confronto
(define (count-intersecting-pairs-bruteforce intervals)
  (let ((counts 0)
        (n (length intervals)))
    (for (i 0 (- n 2))
      (for (j (+ i 1) (- n 1))
        (if (intersect? (nth i intervals) (nth j intervals))
            (setq counts (+ counts 1)))))
    counts))

; Funzione di test
(define (test-algorithms)
  (println "Test degli algoritmi")
  (println "====================")
  ; Test case 1: intervalli semplici
  (setq test1 '((1 3) (2 4) (5 7) (6 8)))
  (println "Test 1: " test1)
  (println "Sweep line: " (count-intersecting-pairs test1))
  (println "Brute force: " (count-intersecting-pairs-bruteforce test1))
  (println)
  ; Test case 2: nessuna intersezione
  (setq test2 '((1 2) (3 4) (5 6) (7 8)))
  (println "Test 2: " test2)
  (println "Sweep line: " (count-intersecting-pairs test2))
  (println "Brute force: " (count-intersecting-pairs-bruteforce test2))
  (println)
  ; Test case 3: tutti si intersecano
  (setq test3 '((1 10) (2 8) (3 6) (4 5)))
  (println "Test 3: " test3)
  (println "Sweep line: " (count-intersecting-pairs test3))
  (println "Brute force: " (count-intersecting-pairs-bruteforce test3))
  (println)
  ; Test case 4: caso più complesso
  (setq test4 '((1 4) (2 6) (3 5) (7 9) (8 10) (11 12)))
  (println "Test 4: " test4)
  (println "Sweep line: " (count-intersecting-pairs test4))
  (println "Brute force: " (count-intersecting-pairs-bruteforce test4))
  (println))

; Funzione per generare test casuali
(define (random-intervals n max-val)
  (let ((intervals '()))
    (dotimes (i n)
      (let ((L (rand max-val))
            (R (+ (rand max-val) 1)))
        (if (> L R) (swap L R))
        (push (list L R) intervals)))
    intervals))

; Test di performance
(define (performance-test)
  (println "Test di performance")
  (println "===================")
  ; Dataset piccolo
  (setq small-test (random-intervals 100 10000))
  (println "\nTest con 100 intervalli casuali...")
  (println "Sweep Line: " 
    (time (setq result-sweep-small (count-intersecting-pairs small-test))))
  (println "Brute force: "
    (time (setq result-brute-small (count-intersecting-pairs-bruteforce small-test))))
  (println "Sweep line: " result-sweep-small)
  (println "Brute force: " result-brute-small)
  (println "Risultati corrispondenti: " (= result-sweep-small result-brute-small))
  ; Dataset grande
  (setq large-test (random-intervals 1000 10000))
  (println "Test con 1000 intervalli casuali...")
  (println "Sweep Line: " 
    (time (setq result-sweep (count-intersecting-pairs large-test))))
  (println "Brute force: "
    (time (setq result-brute (count-intersecting-pairs-bruteforce large-test))))
  (println "Sweep line result: " result-sweep)
  (println "Brute force: " result-brute)
  (println "Risultati corrispondenti: " (= result-sweep result-brute)))

; Esempio di utilizzo
(define (main)
  (println "Algoritmi Brute-Force e Sweep-Line per Intervalli Intersecanti")
  (println "==============================================================")
  (println)
  ; Esegui i test
  (test-algorithms)
  (performance-test)
  ; Esempio pratico
  (println "\nEsempio pratico:")
  (setq my-intervals '((1 5) (2 7) (3 4) (6 9) (8 10)))
  (println "Intervalli: " my-intervals)
  (println "Numero di coppie intersecanti: "
           (count-intersecting-pairs my-intervals)) '>)

; Esegue il programma
(main)
;-> Algoritmi Brute-Force e Sweep-Line per Intervalli Intersecanti
;-> ==============================================================
;-> 
;-> Test degli algoritmi
;-> ====================
;-> Test 1: ((1 3) (2 4) (5 7) (6 8))
;-> Sweep line: 2
;-> Brute force: 2
;-> 
;-> Test 2: ((1 2) (3 4) (5 6) (7 8))
;-> Sweep line: 0
;-> Brute force: 0
;-> 
;-> Test 3: ((1 10) (2 8) (3 6) (4 5))
;-> Sweep line: 6
;-> Brute force: 6
;-> 
;-> Test 4: ((1 4) (2 6) (3 5) (7 9) (8 10) (11 12))
;-> Sweep line: 4
;-> Brute force: 4
;-> 
;-> Test di performance
;-> ===================
;-> 
;-> Test con 100 intervalli casuali...
;-> Sweep Line: 0.997
;-> Brute force: 2.992
;-> Sweep line: 3206
;-> Brute force: 3206
;-> Risultati corrispondenti: true
;-> Test con 1000 intervalli casuali...
;-> Sweep Line: 63.33
;-> Brute force: 1621.466
;-> Sweep line result: 326688
;-> Brute force: 326688
;-> Risultati corrispondenti: true
;-> 
;-> Esempio pratico:
;-> Intervalli: ((1 5) (2 7) (3 4) (6 9) (8 10))
;-> Numero di coppie intersecanti: 5


------------
Algoritmo A*
------------

Ecco una descrizione generale dell'algoritmo A*:

Scopo
-----
Trova il percorso ottimale tra due nodi in un grafo con costo minimo.

Componenti Chiave
-----------------
1. Funzione di Costo:
   - g(n) = costo reale dal nodo iniziale al nodo n
   - h(n) = stima euristica dal nodo n al nodo finale (es. distanza di Manhattan)
   - f(n) = g(n) + h(n) = costo totale stimato
2. Liste:
   - open_list: Nodi da esplorare (ordinati per f(n))
   - closed_list: Nodi già esplorati

Pseudo-codice
-------------
function A*(start, goal)
    open_list := {start}  # Nodi da esplorare
    closed_list := {}     # Nodi già esplorati
    start.g := 0
    start.f := h(start, goal)

    while open_list non è vuota
        current := nodo in open_list con f minimo
        
        if current == goal
            return ricostruisci_percorso(current)
        
        rimuovi current da open_list
        aggiungi current a closed_list

        for each neighbor of current
            if neighbor in closed_list
                skip
            
            tentative_g := current.g + costo(current, neighbor)
            
            if neighbor not in open_list OR tentative_g < neighbor.g
                neighbor.parent := current
                neighbor.g := tentative_g
                neighbor.f := neighbor.g + h(neighbor, goal)
                
                if neighbor not in open_list
                    aggiungi neighbor a open_list
    
    return fallimento  # Nessun percorso trovato
end function

function ricostruisci_percorso(node)
    percorso := []
    while node != null
        aggiungi node a percorso
        node := node.parent
    return inverti(percorso)
end function

Spiegazione
-----------
1. Inizializzazione:
   - Aggiungi il nodo di partenza a open_list con g = 0 e f = h(start).
2. Ciclo Principale:
   - Estrai il nodo current con f minimo (più promettente).
   - Se current è il goal, ricostruisci il percorso all'indietro tramite i parent.
   - Espandi i vicini di current:
     - Ignora i nodi già esplorati (closed_list).
     - Calcola un nuovo g provvisorio per ogni vicino.
     - Se il nuovo g è migliore, aggiorna il vicino e aggiungilo a open_list.
3. Terminazione:
   - Se open_list è vuota e il goal non è raggiunto → nessun percorso.

Esempi di Euristiche
--------------------
- Griglia 2D (movimenti ortogonali):  
  h(n) = |x1 - x2| + |y1 - y2| (Distanza di Manhattan)
  
- Griglia 2D (movimenti diagonali):  
  h(n) = max(|x1 - x2|, |y1 - y2|) (Distanza di Chebyshev)

Proprietà
---------
Ottimale: Trova sempre il percorso più breve se:  
   - h(n) è ammissibile (non sovrastima il costo reale).  
   - h(n) è consistente (soddisfa la disuguaglianza triangolare).  

Efficiente
----------
Più veloce di Dijkstra grazie all'euristica (h(n) guida la ricerca).  

Complessità
-----------
- Tempo: O(b^d) (dove b = fattore di diramazione, d = profondità della soluzione).  
- Spazio: O(b^d) (memorizza tutti i nodi espansi).  

Confronto con altri Algoritmi
-----------------------------
| Algoritmo   | Euristica | Ottimalità | Usi Tipici           |
|-------------|-----------|------------|----------------------|
| A*          | Si        | Si         | Pathfinding, AI      |
| Dijkstra    | No        | Si         | Reti, costo uniforme |
| BFS         | No        | Si         | Grafi non pesati     |

Implementazione
---------------
Algoritmo A* per trovare il percorso ottimale in una griglia MxN con 0 (celle libere) e 1 (celle occupate).

; Funzione che stampa la griglia con il percorso
(define (show maze path)
  (local (rows cols solved)
    ; rows of maze
    (setq rows (length maze))
    ; cols of maze
    (setq cols (length (maze 0)))
    ; copy of maze
    (setq solved maze)
    ; Add solution path (".") to solved maze
    (dolist (el path) (setf (solved (el 0) (el 1)) "."))
    ; print solved maze with solution path
    (for (i 0 (- rows 1))
      (for (j 0 (- cols 1))
        (print " " (solved i j)))
      (println)) '>))

; Funzione che cerca un elemento in una lista di coppie
(define (in-list? x y lst)
  (exists (lambda (item) (and (= (item 0) x) (= (item 1) y))) lst))

; Estrae il percorso
(define (extract-path node)
  (if (null? (node 4))
      ; Inverte x e y --> output: riga e colonna
      (list (list (node 1) (node 0)))
      ; Inverte x e y --> output: riga e colonna
      (append (extract-path (node 4)) 
              (list (list (node 1) (node 0))))))
;; Estrae il percorso (con la logica colonna e riga)
;(define (extract-path node)
;  (if (null? (node 4))
;      (list (list (node 0) (node 1)))
;      (append (extract-path (node 4)) 
;              (list (list (node 0) (node 1))))))

; Verifica cella valida
(define (valid-cell? grid x y)
  (and (>= x 0) (>= y 0)
       (< x (length (grid 0)))
       (< y (length grid))
       (= 0 (grid y x))))

; Distanza di Manhattan
(define (manhattan x1 y1 x2 y2)
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

; Algoritmo A* principale
(define (a-star grid start end)
  ; Scambia x e y delle coordinate di 'start' --> colonna, riga
  (setq start (list (start 1) (start 0)))
  ; Scambia x e y delle coordinate di 'end' --> colonna, riga
  (setq end (list (end 1) (end 0)))
  ; Blocco principale con gestione errori
  (catch
    ; Inizializzazione variabili
    (let (
          ; Lista aperta: nodi da esplorare, inizializzata con nodo di partenza
          ; Formato: (x y g h parent)
          ; g = costo percorso da start, h = euristica a end
          (open-list (list (list (start 0) (start 1)
                               0
                               (manhattan (start 0) (start 1) (end 0) (end 1))
                               nil)))
          ; Lista chiusa: nodi già esplorati (solo coordinate x,y)
          (closed-list '())
          ; Direzioni possibili: su, giù, sinistra, destra (4 movimenti)
          (directions '((-1 0) (1 0) (0 -1) (0 1))))
    ; Ciclo principale: continua finché ci sono nodi da esplorare
    (while open-list
      ; 1. Seleziona nodo con f(min) = g + h minimo
      (setq current (first
                    (sort open-list
                          (lambda (a b)
                            (< (+ (a 2) (a 3)) (+ (b 2) (b 3)))))))
      (setq open-list (rest open-list)) ; Rimuovi nodo corrente da open-list
      ; 2. Verifica se siamo arrivati a destinazione
      (when (and (= (current 0) (end 0))
                (= (current 1) (end 1)))
        (throw (extract-path current))) ; Restituisci percorso
      ; 3. Aggiungi nodo corrente alla lista chiusa (solo coordinate)
      (push (list (current 0) (current 1)) closed-list -1)
      ; 4. Espandi i vicini
      (dolist (dir directions)
        (let (
              ; Calcola coordinate del vicino
              (nx (+ (current 0) (dir 0)))
              (ny (+ (current 1) (dir 1))))
          ; Verifica:
          ; - La cella è nella griglia?
          ; - È percorribile (0)?
          ; - Non è già stata esplorata?
          (when (and (valid-cell? grid nx ny)
                    (not (in-list? nx ny closed-list)))
            (let (
                  ; Calcola costo g (costo corrente + 1 passo)
                  (g (+ (current 2) 1))
                  ; Calcola euristica h (distanza da end)
                  (h (manhattan nx ny (end 0) (end 1)))
                  ; Cerca se il nodo è già in open-list
                  (existing-node
                   (find (lambda (n)
                          (and (= (n 0) nx) (= (n 1) ny)))
                         open-list)))
              ; Se il nodo esiste già...
              (if existing-node
                  ; ...e questo percorso ha un g migliore, aggiornalo
                  (when (< g (existing-node 2))
                    (setf (existing-node 2) g) ; Nuovo costo g
                    (setf (existing-node 4) current)) ; Aggiorna parent
                ; Altrimenti aggiungi nuovo nodo a open-list
                (push (list nx ny g h current) open-list -1)))))))
    ; Se open-list è vuota e non ha trovato end: nessun percorso
    nil)))

Proviamo:

Esempio 1:
(setq grid '((0 0 0 0 0)
             (1 1 1 1 0)
             (0 0 0 0 0)
             (0 1 1 0 1)
             (0 0 0 0 0)))
(setq start '(0 0))
(setq end '(4 4))
(a-star grid start end)
;-> ((0 0) (0 1) (0 2) (0 3) (0 4) (1 4) (2 4) (2 3) (3 3) (4 3) (4 4))
(show grid (a-star grid start end))
;-> . . . . .
;-> 1 1 1 1 .
;-> 0 0 0 . .
;-> 0 1 1 . 1
;-> 0 0 0 . .

Esempio 2:
(setq grid '((0 0 0 0 0)
             (1 1 1 1 0)
             (0 0 0 0 0)
             (0 1 1 0 1)
             (0 0 0 0 0)))
(setq start '(0 4))
(setq end '(4 0))
(a-star grid start end)
;-> ((0 4) (1 4) (2 4) (2 3) (3 3) (4 3) (4 2) (4 1) (4 0))
(show grid (a-star grid start end))
;-> 0 0 0 0 .
;-> 1 1 1 1 .
;-> 0 0 0 . .
;-> 0 1 1 . 1
;-> . . . . 0

Esempio 3 (griglia senza percorso possibile):
(setq grid '((0 0 0 0 0)
             (1 1 1 1 1)
             (1 0 0 0 0)
             (1 1 1 0 1)
             (1 1 1 0 0)))
(setq start '(0 0))
(setq end '(4 4))
(a-star grid start end)
;-> nil

Esempio 4:
(setq grid '((0 0 0 0 0)
             (0 1 1 1 0)
             (0 0 0 0 0)
             (1 1 0 1 1)
             (0 0 0 0 0)))
(setq start '(0 0))
(setq end '(4 4))
(a-star grid start end)
;-> ((0 0) (1 0) (2 0) (2 1) (2 2) (3 2) (4 2) (4 3) (4 4))
(show grid (a-star grid start end))
;-> . 0 0 0 0
;-> . 1 1 1 0
;-> . . . 0 0
;-> 1 1 . 1 1
;-> 0 0 . . .

Esempio 5:
(setq grid '((0 0 0 0 0 0 1)
             (1 1 1 1 1 0 1)
             (0 0 0 0 0 0 1)
             (0 1 1 1 1 1 1)
             (0 0 0 0 0 0 1)
             (0 0 0 0 0 0 0)))
(setq start '(0 0))
(setq end '(5 6))
(a-star grid start end)
;-> ((0 0) (0 1) (0 2) (0 3) (0 4) (0 5) (1 5) (2 5) (2 4) (2 3) (2 2)
;->  (2 1) (2 0) (3 0) (4 0) (5 0) (5 1) (5 2) (5 3) (5 4) (5 5) (5 6))
(show grid (a-star grid start end))
;-> . . . . . . 1
;-> 1 1 1 1 1 . 1
;-> . . . . . . 1
;-> . 1 1 1 1 1 1
;-> . 0 0 0 0 0 1
;-> . . . . . . .

Vedi anche "Percorso minimo (BFS) e massimo (DFS) in un labirinto (shortest/longest path)" su "Note libere 30".


--------------------------------------
Funzione autoreplicante con incremento
--------------------------------------

Scrivere una funzione 'fun' che genera la funzione fun1.
La funzione 'fun1' genera la funzione 'fun2'.
La funzione 'fun2' genera la funzione 'fun3'.
...e così via.
Tutte le funzioni devono avere lo stesso codice (a parte il nome).
Le funzioni devono essere eseguite in sequenza: fun, fun1, fun2, fun3, ...

; Inizializzazione della versione (non necessaria la prima volta che si esegue la sequenza delle funzioni)
; (setq version 0)

; Funzione autoreplicante con incremento
(define (fun) (set (sym (string "fun" (inc version))) fun))
;-> (lambda () (set (sym (string "fun" (inc version))) fun))

Proviamo:

(fun)
(fun1)
(fun2)
(fun3)
(= fun fun1 fun2 fun3 fun4)
;-> true


--------------------
Cifrario di Playfair
--------------------

https://en.wikipedia.org/wiki/Playfair_cipher

; Cifrario di Playfair
; Funzione per rimuovere caratteri duplicati da una stringa
(define (remove-duplicates input-str)
  (local (output-str seen-chars current-char)
    (setq output-str "")
    (setq seen-chars '())
    (dostring (idx input-str)
      (setq current-char (upper-case (char idx)))
      (if (and (>= idx 65) (<= idx 90) ; Solo lettere A-Z
               (not (member current-char seen-chars)))
          (begin
            (push current-char seen-chars)
            (extend output-str current-char))))
    output-str))

; Funzione per creare la matrice 5x5 dalla chiave
(define (create-matrix cipher-key)
  (local (alphabet clean-key playfair-matrix row col current-char)
    (setq alphabet "ABCDEFGHIKLMNOPQRSTUVWXYZ") ; Senza J
    (setq clean-key (remove-duplicates cipher-key))
    (setq playfair-matrix (array 5 5))
    ; Aggiungi lettere rimanenti dell'alfabeto
    (dostring (idx alphabet)
      (setq current-char (char idx))
      (if (not (find current-char clean-key))
          (extend clean-key current-char)))
    ; Riempi la matrice 5x5
    (for (row 0 4)
      (for (col 0 4)
        (setf (playfair-matrix row col) (clean-key (+ (* row 5) col)))))
    playfair-matrix))

; Funzione per trovare la posizione di un carattere nella matrice
(define (find-position search-matrix search-char)
  (local (position-result row col)
    (setq position-result '())
    (for (row 0 4)
      (for (col 0 4)
        (if (= (search-matrix row col) search-char)
            (setq position-result (list row col)))))
    position-result))

; Funzione per preparare il testo (rimuove spazi, converte J in I, etc.)
(define (prepare-text input-text)
  (local (clean-text final-text current-char pair1 pair2 temp-list loop-idx)
    (setq clean-text "")
    (dostring (idx (upper-case input-text))
      (setq current-char (char idx))
      (cond
        ((= current-char "J") (extend clean-text "I"))
        ((and (>= idx 65) (<= idx 90)) (extend clean-text current-char))))
    ; Converti in lista per processare le coppie
    (setq temp-list (explode clean-text))
    (setq final-text "")
    ; Processa le coppie
    (setq loop-idx 0)
    (while (< loop-idx (length temp-list))
      (setq pair1 (temp-list loop-idx))
      (if (< (+ loop-idx 1) (length temp-list))
          (begin
            (setq pair2 (temp-list (+ loop-idx 1)))
            (if (= pair1 pair2)
                (begin
                  (extend final-text pair1 "X")
                  (setq loop-idx (+ loop-idx 1)))
                (begin
                  (extend final-text pair1 pair2)
                  (setq loop-idx (+ loop-idx 2)))))
          (begin
            (extend final-text pair1 "X")
            (setq loop-idx (+ loop-idx 1)))))
    final-text))

; Funzione per cifrare una coppia di caratteri
(define (encrypt-pair cipher-matrix char1 char2)
  (local (pos1 pos2 row1 col1 row2 col2)
    (setq pos1 (find-position cipher-matrix char1))
    (setq pos2 (find-position cipher-matrix char2))
    (setq row1 (first pos1))
    (setq col1 (last pos1))
    (setq row2 (first pos2))
    (setq col2 (last pos2))
    (cond
      ; Stessa riga - sposta a destra
      ((= row1 row2)
       (list (cipher-matrix row1 (% (+ col1 1) 5))
             (cipher-matrix row2 (% (+ col2 1) 5))))
      ; Stessa colonna - sposta in basso  
      ((= col1 col2)
       (list (cipher-matrix (% (+ row1 1) 5) col1)
             (cipher-matrix (% (+ row2 1) 5) col2)))
      ; Rettangolo - scambia colonne
      (true
       (list (cipher-matrix row1 col2)
             (cipher-matrix row2 col1))))))

; Funzione per decifrare una coppia di caratteri
(define (decrypt-pair cipher-matrix char1 char2)
  (local (pos1 pos2 row1 col1 row2 col2)
    (setq pos1 (find-position cipher-matrix char1))
    (setq pos2 (find-position cipher-matrix char2))
    (setq row1 (first pos1))
    (setq col1 (last pos1))
    (setq row2 (first pos2))
    (setq col2 (last pos2))
    (cond
      ; Stessa riga - sposta a sinistra
      ((= row1 row2)
       (list (cipher-matrix row1 (% (+ col1 4) 5)) ; +4 equivale a -1 mod 5
             (cipher-matrix row2 (% (+ col2 4) 5))))
      ; Stessa colonna - sposta in alto
      ((= col1 col2)
       (list (cipher-matrix (% (+ row1 4) 5) col1) ; +4 equivale a -1 mod 5
             (cipher-matrix (% (+ row2 4) 5) col2)))
      ; Rettangolo - scambia colonne
      (true
       (list (cipher-matrix row1 col2)
             (cipher-matrix row2 col1))))))

; Funzione principale per cifrare
(define (playfair-encrypt plain-text cipher-key)
  (local (cipher-matrix prepared-text result-text idx encrypted-pair)
    (setq cipher-matrix (create-matrix cipher-key))
    (setq prepared-text (prepare-text plain-text))
    (setq result-text "")
    (for (idx 0 (- (length prepared-text) 1) 2)
      (setq encrypted-pair (encrypt-pair cipher-matrix 
                                        (prepared-text idx) 
                                        (prepared-text (+ idx 1))))
      (extend result-text (first encrypted-pair) (last encrypted-pair)))
    result-text))

; Funzione principale per decifrare
(define (playfair-decrypt cipher-text cipher-key)
  (local (cipher-matrix result-text idx decrypted-pair)
    (setq cipher-matrix (create-matrix cipher-key))
    (setq result-text "")
    (for (idx 0 (- (length cipher-text) 1) 2)
      (setq decrypted-pair (decrypt-pair cipher-matrix 
                                        (cipher-text idx) 
                                        (cipher-text (+ idx 1))))
      (extend result-text (first decrypted-pair) (last decrypted-pair)))
    result-text))

; Funzione per ripulire il testo decifrato (rimuove X aggiunte)
; valida solo quando plain-text (input-str) non contiene "X"
(define (clean-decrypted-text decrypted-text)
  (let (result-text "")
    (dostring (ch decrypted-text)
      (if (!= (char ch) "X")
          (extend result-text (char ch))))
    result-text))

; Funzione completa per decifrare e ripulire
(define (playfair-decrypt-clean cipher-text cipher-key)
  (local (decrypted-text)
    (setq decrypted-text (playfair-decrypt cipher-text cipher-key))
    (clean-decrypted-text decrypted-text)))

; Funzione per stampare la matrice Playfair
(define (print-matrix display-matrix)
  (local (row col)
    (println "Matrice Playfair:")
    (for (row 0 4)
      (for (col 0 4)
        (print (display-matrix row col) " "))
      (println)) '>))

Proviamo:

; Esempi (senza "X" nel testo)
(setq test-key "PLAYFAIR")
(setq test-plaintext "HELLO WORLD")
(setq test-matrix (create-matrix test-key))
(print-matrix test-matrix)
;-> Matrice Playfair:
;-> P L A Y F
;-> I R B C D
;-> E G H K M
;-> N O Q S T
;-> U V W X Z
(setq test-prepared (prepare-text test-plaintext))
;-> "HELXLOWORLDX"
(setq test-encrypted (playfair-encrypt test-plaintext test-key))
;-> "KGYVRVVQGRCZ"
(setq test-decrypted (playfair-decrypt test-encrypted test-key))
;-> "HELXLOWORLDX"
(setq test-cleaned (clean-decrypted-text test-decrypted))
;-> "HELLOWORLD"

(setq test-key "PLAYFAIR")
(setq test-plaintext "NEWLISP IS GREAT NO CROSS")
(setq test-matrix (create-matrix test-key))
(print-matrix test-matrix)
;-> Matrice Playfair:
;-> P L A Y F
;-> I R B C D
;-> E G H K M
;-> N O Q S T
;-> U V W X Z
(setq test-prepared (prepare-text test-plaintext))
;-> "NEWLISPISGREATNOCROSSX"
(setq test-encrypted (playfair-encrypt test-plaintext test-key))
;-> "UNVACNIEOKIGFQOQDBQTXY"
(setq test-decrypted (playfair-decrypt test-encrypted test-key))
;-> "NEWLISPISGREATNOCROSSX"
(setq test-cleaned (clean-decrypted-text test-decrypted))
;-> "NEWLISPISGREATNOCROSS"

(setq test-key2 "MONARCHY")
(setq test-text2 "ATTACKATDAWN")
(setq test-matrix2 (create-matrix test-key2))
(print-matrix test-matrix2)
;-> Matrice Playfair:
;-> M O N A R
;-> C H Y B D
;-> E F G I K
;-> L P Q S T
;-> U V W X Z
(setq test-enc2 (playfair-encrypt test-text2 test-key2))
;-> "RSSRDERSBRNY"
(setq test-dec2 (playfair-decrypt test-enc2 test-key2))
;-> "ATTACKATDAWN"
(setq test-clean2 (clean-decrypted-text test-dec2))
;-> "ATTACKATDAWN"


-----------------------------------------------------------------
Ricerca del k-esimo indice del primo elemento che ha N occorrenze
-----------------------------------------------------------------

Data una lista di interi, scrivere una funzione che, dati due interi K e N, restituisce l'indice della K-esima occorrenza dell'intero che, per primo, ha N occorrenze nella lista.
Risulta sempre 1 <= K <= N.

Esempio:
lista = (2 5 3 7 9 4 2 6 7 7 3 0 1 2 3))
N = 3
K = 2
Il primo elemento che ha 3 (N=3) occorrenze è il 7.
Il secondo (K=2) 7 ha indice pari a 8.

Algoritmo con le primitive di newLISP:

(setq lst '(2 5 3 7 9 4 2 6 7 7 3 0 1 2 3))
(setq n 3)
(setq k 2)
; Crea la lista dei valori unici della lista data
(setq unici (unique lst))
;-> (2 5 3 7 9 4 6 0 1)
; Crea la lista degli interi che hanno almeno n occorrenze
(setq multipli (filter (fn(x) (>= (length (ref-all x lst)) n)) unici))
;-> (2 3 7)
; Crea la lista dei primi n indici di ogni intero che ha almeno n occorrenze
(setq all-index (map (fn(x) (slice (flat (ref-all x lst)) 0 n)) multipli))
;-> ((0 6 13) (2 10 14) (3 8 9))
; Crea la lista dei primi n indici di ogni intero che ha almeno n occorrenze
(setq all-index (map (fn(x) (slice x 0 n)) all-index))
; Seleziona la lista degli indici che ha indice minore all'ultimo posto,
; questo significa che il numero è il promo a comparire k volte
(setq indici (first (sort all-index (fn (a b) (<= (a -1) (b -1))))))
;-> (3 8 9)
; Prendiamo il (k-1)-esimo indice della sottolista selezionata
(indici (- k 1))
;-> 8
(lst 8)
;-> 7

Funzione (primitive):

(define (trova k n lst)
  ; l'indice del primo numero che ha almeno una ripetizione è quello
  ; del primo numero della lista, cioè 0
  (if (and (= k 1) (= n 1))
    0
    (local (unici multipli all-index indici)
      (setq unici (unique lst))
      ; Crea la lista degli interi che hanno almeno n occorrenze
      (setq multipli (filter (fn(x) (>= (length (ref-all x lst)) n)) unici))
      ; Se esistono interi che hanno almeno n occorrenze...
      (if multipli
        (begin
          ; Crea la lista dei primi n indici di ogni intero
          ; che ha almeno n occorrenze
          (setq all-index (map
                (fn(x) (slice (flat (ref-all x lst)) 0 n)) multipli))
          ; Seleziona la sottolista degli indici
          ; che ha indice minore all'ultimo posto.
          ; Questo significa che il numero individuato dalla sottolista
          ; è il primo a comparire k volte
          (setq indici (first
                (sort all-index (fn (a b) (<= (a -1) (b -1))))))
          ; Prendiamo il (k-1)-esimo indice della sottolista selezionata
          (indici (- k 1)))
        nil))))

Proviamo:

(setq lst '(2 5 3 7 9 4 2 6 7 7 3 0 1 2 3))
(trova 2 3 lst)
;-> 8

(setq w '(6 1 2 3 3 4 4 5))
(trova 1 2 w)
;-> 3
(trova 2 2 w)
;-> 4
(trova 1 1 w)
;-> 0
(trova 1 3 w)
;-> nil

Funzione iterativa:

(define (trova-itera k n lst)
  (local (conteggi posizioni trovato finito)
    ; Inizializza la lista associativa per i conteggi
    (setq conteggi '())
    ; Inizializza la lista associativa per le posizioni
    (setq posizioni '())
    ; Flag per tenere traccia se abbiamo trovato il primo elemento con n occorrenze
    (setq trovato nil)
    (setq finito nil)
    ; Scorri la lista con un ciclo for
    (for (i 0 (- (length lst) 1))
      (when (not finito)
        (let ((x (lst i)))
          ; Aggiorna il conteggio dell'elemento x
          (if (not (assoc x conteggi))
              (push (list x 1) conteggi -1) ; prima occorrenza
              (setf (assoc x conteggi) (list x (+ 1 ((assoc x conteggi) 1))))) ; incrementa
          ; Aggiorna la lista delle posizioni di x
          (if (not (assoc x posizioni))
              (push (list x (list i)) posizioni -1) ; prima posizione
              (setf (assoc x posizioni)
                    (list x (append ((assoc x posizioni) 1) (list i))))) ; aggiungi nuova posizione
          ; Se x ha raggiunto n occorrenze ed è il primo a farlo, salvalo
          (if (= ((assoc x conteggi) 1) n)
              (begin
                (setq trovato x)
                (setq finito true)))))) ; ferma il ciclo
    ; Se abbiamo trovato un intero con n occorrenze, restituisci l’indice della k-esima
    (if trovato
        (((assoc trovato posizioni) 1) (- k 1))
        nil))) ; altrimenti restituisci nil

Proviamo:

(setq lst '(2 5 3 7 9 4 2 6 7 7 3 0 1 2 3))
(trova-itera 2 3 lst)
;-> 8

(setq w '(6 1 2 3 3 4 4 5))
(trova-itera 1 2 w)
;-> 3
(trova-itera 2 2 w)
;-> 4
(trova-itera 1 1 w)
;-> 0
(trova-itera 1 3 w)
;-> nil

Test di correttezza:

(for (i 1 1000)
  (setq test (rand 100 10000))
  (setq n (+ 2 (rand 5)))
  (setq k (+ 2 (rand 5)))
  (if (> k n) (swap k n))
  (when (!= (trova k n test) (trova-itera k n test))
    (println k { } n) (setq err test) (read-line)))
;-> nil

Test di velocità:

(setq test1 (rand 10 100))
(time (trova 2 3 test1) 1e4)
;-> 420.092
(time (trova-itera 2 3 test1) 1e4)
;-> 94.32

(silent (setq test2 (rand 100 1e5)))
(time (println (trova 5 10 test2)))
;-> 297
;-> 181.185
(time (println (trova-itera 5 10 test2)))
;-> 297
;-> 9.768

La funzione iterativa è molto più veloce.


-------------------------------------
Espressioni regolari (Basi ed Esempi)
-------------------------------------

Basi fondamentali
-----------------

### 1. "abc"
Matcha esattamente la stringa "abc".
(regex "abc" "xxabcxx")
;-> ("abc" 2 3)

### 2. "a.c"
Il . rappresenta un solo carattere qualsiasi.
Matcha "abc", "a7c", "a_c"
Non matcha "ac" (manca un carattere in mezzo)

### 3. "ab*c"
Il * significa zero o più ripetizioni.
Matcha: "ac", "abc", "abbc", "abbbbbc"

### 4. "ab+c"
Il + significa una o più ripetizioni.
Matcha: "abc", "abbc"
Non matcha: "ac" (manca almeno una b)

### 5. "ab?c"
Il ? significa zero o una ripetizione.
Matcha: "ac", "abc"
Non matcha: "abbc"

Classi di caratteri
-------------------

### 6. "a[xyz]b"
Cerca "a", uno tra x, y, z, poi "b".
Matcha: "axb", "ayb"
Non matcha: "acb"

### 7. "a[^xyz]b"
Il ^ dentro [] nega: matcha qualsiasi tranne x, y, z.
Matcha: "acb", "a9b"
Non matcha: "axb"

### 8. "[0-9]+" oppure "\d+" (in PCRE)
Matcha una o più cifre.
Matcha: "123", "7"

### 9. "[A-Za-z]+"
Matcha una o più lettere ASCII (maiuscole o minuscole).

Ancore e gruppi
---------------

### 10. "^abc"
Il ^ (fuori dalle []) significa inizio stringa.
Matcha "abc" solo se è all'inizio
Non matcha "xabc"

### 11. "abc$"
Il $ significa fine stringa.
Matcha: "xxabc" solo se "abc" è alla fine

### 12. "(abc)+"
Parentesi tonde definiscono un gruppo, che può essere ripetuto.
Matcha: "abcabc" -> il gruppo "abc" appare due volte

Quantificatori avanzati
-----------------------

### 13. "a.{3}b"
Matcha una a, seguita da esattamente 3 caratteri qualsiasi, poi una b.
Matcha: "aXYZb"
Non matcha: "aXXb"

### 14. "a.{2,5}b"
Matcha da 2 a 5 caratteri qualsiasi tra a e b.
Matcha: "a12b", "aABCDEb"
Non matcha: "ab", "a123456b"

Lookahead e lookbehind (in parte disponibili in PCRE)
-----------------------------------------------------
NB: newLISP supporta alcuni lookahead, ma non lookbehind.

### 15. "A(?=[0-9]{2})"
Matcha "A" solo se è seguita da 2 cifre, ma non le include nel match.
Su "A12" --> matcha "A"
Su "AB" --> no match

### 16. "A[0-9A-Za-z]{1,10}B"
Matcha una stringa che inizia con A, segue 1-10 caratteri alfanumerici, termina con B.
"A1B", "A123abcB", "Aabcdef123B"

### 17. "(?<![A-Z])ABC(?![A-Z])"
Matcha solo "ABC" che NON è preceduto o seguito da lettere maiuscole.
NB: Questo richiede full lookbehind/lookahead, non supportato in tutte le versioni.

### 18. "A[0-9]*B"
Matcha da A a B, con solo cifre (anche zero) in mezzo.
"AB", "A12345B"
"AabcB"

### 19. "\\bword\\b"
Matcha "word" come parola isolata (usa \b per word boundary).
"a word in a sentence"
non matcha "password" o "sword"
NB: In newLISP: usa "\\b" perché \ va escapato.

### 20. "(\\d{3})-(\\d{2})-(\\d{4})"
Matcha codici tipo codice fiscale USA: "123-45-6789"
- \d{3} --> 3 cifre
- \d{2} --> 2 cifre
- \d{4} --> 4 cifre

Suggerimenti per newLISP
------------------------
- Usa sempre doppi backslash per escape ("\\d" invece di "\d")
- Per match singoli, usa (regex pattern str 64 start-pos)
- Per match multipli, usa (regex-all pattern str)
- Per match multipli (overlap), usa (regex-all pattern str true)

(define (regex-all regexp str all)
"Find all occurrences of a regex in a string"
  (let ( (out '()) (idx 0) (res nil))
    (setq res (regex regexp str 64 idx))
    (while res
      (push res out -1)
      (if all
          (setq idx (+ (res 1) 1))        ; contiguos pattern (overlap)
          (setq idx (+ (res 1) (res 2)))) ; no contiguos pattern
      (setq res (regex regexp str 64 idx)))
    out))

Esempi di base e intermedi
--------------------------

; Match esatto
(print (regex-all "abc" "xyzabc123abcxyz"))  
;--> (("abc" 3 3) ("abc" 9 3))

; Punto: qualsiasi carattere singolo
(print (regex-all "a.c" "abc a7c acc adc a_c"))  
;--> (("abc" 0 3) ("a7c" 4 3) ("acc" 8 3) ("adc" 12 3) ("a_c" 16 3))

; A seguita da zero o più B, poi C
(print (regex-all "AB*C" "AC ABC ABBBC ABBC"))  
;--> (("AC" 0 2) ("ABC" 3 3) ("ABBBC" 7 5) ("ABBC" 13 4))

; Una o più cifre
(print (regex-all "[0-9]+" "abc123xyz007and42"))  
;--> (("123" 3 3) ("007" 9 3) ("42" 15 2))

; Lettere minuscole/maiuscole
(print (regex-all "[a-z]+" "ABC def GHI jkl"))  
;--> (("def" 4 3) ("jkl" 12 3))

; Una lettera seguita da una cifra
(print (regex-all "[A-Za-z][0-9]" "a1 b2 C3 d4 x9"))  
;--> (("a1" 0 2) ("b2" 3 2) ("C3" 6 2) ("d4" 9 2) ("x9" 12 2))

Quantificatori specifici
------------------------

; Esattamente 3 lettere
(print (regex-all "[A-Za-z]{3}" "abc ab defg XYZ QWERTY"))  
;--> (("abc" 0 3) ("def" 7 3) ("XYZ" 12 3))

; Da 2 a 4 cifre
(print (regex-all "[0-9]{2,4}" "1 12 123 1234 12345"))  
;--> (("12" 2 2) ("123" 5 3) ("1234" 9 4) ("1234" 14 4))

; Almeno una lettera seguita da un numero
(print (regex-all "[A-Za-z]+[0-9]+" "a1 abc123 xyz999 q"))  
;--> (("a1" 0 2) ("abc123" 3 6) ("xyz999" 10 6))

Ancore: inizio e fine stringa
----------------------------=

; Inizio stringa
(print (regex-all "^abc" "abc abcabc abc"))  
;--> (("abc" 0 3))

; Fine stringa
(print (regex-all "abc$" "xyz abc abcabcabc"))  
;--> (("abc" 14 3))

Caratteri negativi, intervalli, gruppi
--------------------------------------

; Tutto tranne le vocali
(print (regex-all "[^aeiou]+" "hello world"))  
;--> (("h" 0 1) ("ll" 2 2) (" w" 4 2) ("rld" 7 3))

; Gruppi ripetuti
(print (regex-all "(ab)+" "abababab ab ab abba"))  
;--> (("abababab" 0 8) ("ab" 9 2) ("ab" 12 2))

; Gruppi misti: lettere e cifre tra A e B (1-5 char)
(print (regex-all "A[0-9a-zA-Z]{1,5}B" "A1B A123B Aab12B A123456B"))  
;--> (("A1B" 0 3) ("A123B" 4 5) ("Aab12B" 10 6))

Combinazioni avanzate
---------------------

; A seguito da lettere o cifre (1-10), poi B
(setq teststr "A1B A123B Aabcdef123B A1234567890B AXB")
(print (regex-all "A[0-9A-Za-z]{1,10}B" teststr))
;--> (("A1B" 0 3) ("A123B" 4 5) ("Aabcdef123B" 10 11) ("A1234567890B" 22 12) ("AXB" 39 3))

; Solo cifre tra A e B (min 2 cifre)
(print (regex-all "A[0-9]{2,}B" "A1B A12B A1234B AXB A0000B"))  
;--> (("A12B" 4 4) ("A1234B" 9 6) ("A0000B" 22 6))

Ulteriori esempi
----------------

Cerca "AAA"
-----------
(setq a "AAAaBAAAABcADccAAAB")
(regex "[A]{3}" a)
;-> ("AAA" 0 3)
(regex-all "[A]{3}" a)
;-> (("AAA" 0 3) ("AAA" 5 3) ("AAA" 15 3))
(regex-all "[A]{3}" a true)
;-> (("AAA" 0 3) ("AAA" 5 3) ("AAA" 6 3) ("AAA" 15 3))

Cerca "AAA" senza "A" prima e senza "A" dopo
--------------------------------------------
(regex "(?<!A)[A]{3}(?!A)" a)
;-> ("AAA" 0 3)

Spiegazione:
"(?<!A)[A]{3}(?!A)" sta usando lookbehind e lookahead negativi, cioè:
- (?<!A) -> assicura che prima della tripla "A" non ci sia una "A",
- [A]{3} -> cattura esattamente tre lettere A,
- (?!A)  -> assicura che dopo la tripla "A" non ci sia una "A".
Quindi la regex seleziona solo le triple "AAA" che non sono precedute né seguite da un'altra A.
Se volessi solo triple "AAA" non precedute da A, ma ti va bene che siano seguite da un'altra A, potresti usare solo:
"(?<!A)[A]{3}"
O viceversa, per escludere solo quelle seguite da un'altra A:
"[A]{3}(?!A)"

Cerca "A...B", cioè "A" qualunque numero di caratteri e "B"
-----------------------------------------------------------
(regex "A.*?B" a)
;-> ("AAAaB" 0 5)

Spiegazione:
- "A" -> match della lettera A iniziale
- .*? -> match di qualunque numero di caratteri (in modo non avido):
- .   -> qualunque carattere (tranne newline)
- *   -> 0 o più volte
- ?   -> quantificatore non-greedy (fermati al primo B possibile)
- "B" -> match della lettera B finale

(regex-all "A.*?B" a)
;-> (("AAAaB" 0 5) ("AAAAB" 5 5) ("ADccAAAB" 11 8))
(regex-all "A.*?B" a true)
;-> (("AAAaB" 0 5) ("AAaB" 1 4) ("AaB" 2 3) ("AAAAB" 5 5) ("AAAB" 6 4)
;->  ("AAB" 7 3) ("AB" 8 2) ("ADccAAAB" 11 8) ("AAAB" 15 4) ("AAB" 16 3)
;->  ("AB" 17 2))

Perché usare .*? invece di .*?
Se usi "A.*B" (senza ?), otterresti un match avido, cioè prende da prima A fino all’ultima B:
(regex "A.*B" a)
;-> ("AAAaBAAAABcADccAAAB" 0 19)
Invece con "A.*?B" prendi il primo match possibile per ogni B, uno alla volta: è quello che normalmente vuoi.

Se vogliamo cercare "A", seguite da almeno un carattere qualunque, e che terminano con "B":
(regex "A.+?B" a)
;-> ("AAAaB" 0 5)

Spiegazione:
"A"   -> match della A iniziale
".+?" -> almeno un carattere (non greedy):
"."   -> qualsiasi carattere
"+"   -> uno o più (a differenza di "*" che include anche zero)
"?"   -> non greedy, quindi si ferma alla prima B possibile
"B"   -> match della B finale

(regex-all "A.+?B" a)
;-> (("AAAaB" 0 5) ("AAAAB" 5 5) ("ADccAAAB" 11 8))
(regex-all "A.+?B" a true)
;-> (("AAAaB" 0 5) ("AAaB" 1 4) ("AaB" 2 3) ("AAAAB" 5 5) ("AAAB" 6 4)
;->  ("AAB" 7 3) ("ABcADccAAAB" 8 11) ("ADccAAAB" 11 8) ("AAAB" 15 4)
;->  ("AAB" 16 3))

Se vogliamo cercare "A", qualunque carattere, "B", qualunque carattere "A":
(setq a "AAAaBAAAABcADccAAAB")
(regex "A.*?B.*?A" a)

Spiegazione:
"A"   -> match della A iniziale
".+?" -> almeno un carattere (non greedy):
"B"   -> match della B in mezzo
".+?" -> almeno un carattere (non greedy):
"A"   -> match della A finale

"."   -> qualsiasi carattere
"+"   -> uno o più (a differenza di "*" che include anche zero)
"?"   -> non greedy, quindi si ferma al primo match possibile

(regex-all "A.*?B.*?A" a)
;-> (("AAAaBA" 0 6) ("AAABcA" 6 6))
(regex-all "A.*?B.*?A" a true)
;-> (("AAAaBA" 0 6) ("AAaBA" 1 5) ("AaBA" 2 4) ("AAAABcA" 5 7)
;->  ("AAABcA" 6 6) ("AABcA" 7 5) ("ABcA" 8 4))

Cerca "A...B", cioè "A" qualunque numero di cifre (anche 0) e "B"
-----------------------------------------------------------------
(setq b "A123B AXB A00B AB A9B A9999B")
(regex "A[0-9]*B")
;-> (("A123B" 0 5) ("A00B" 8 4) ("AB" 12 2) ("A9B" 14 3) ("A9999B" 17 6))
(setq b "A123BAXBA00BABA9BA9999B")
;-> (("A123B" 0 5) ("A00B" 8 4) ("AB" 12 2) ("A9B" 14 3) ("A9999B" 17 6))

Spiegazione:
- "A"      -> match della lettera A
- "[0-9]*" -> zero o più cifre (cioè 0, 1, ..., 9)
- "B"      -> match della lettera B

(regex-all "A[0-9]*B" b)
;-> (("A123B" 0 5) ("A00B" 8 4) ("AB" 12 2) ("A9B" 14 3) ("A9999B" 17 6))
(regex-all "A[0-9]*B" b true)
;-> (("A123B" 0 5) ("A00B" 8 4) ("AB" 12 2) ("A9B" 14 3) ("A9999B" 17 6))

Cerca "A", da almeno 1 a 10 caratteri alfanumerici ([0-9A-Za-z]) e "B"
----------------------------------------------------------------------
(setq c "A1B A123B Aabcdef123B A1234567890B A-B AXB AB")
(regex "A[0-9A-Za-z]{1,10}B" c)
;-> ("A1B" 0 3)
Spiegazione:
- "A" → inizio con A
- "[0-9A-Za-z]{1,10}" → da 1 a 10 caratteri alfanumerici
- "B" → termina con B

(regex-all "A[0-9A-Za-z]{1,10}B" c)
;-> (("A1B" 0 3) ("A123B" 4 5) ("Aabcdef123B" 10 11)
;->  ("A1234567890B" 22 12) ("AXB" 39 3))
(regex-all "A[0-9A-Za-z]{1,10}B" c true)
;-> (("A1B" 0 3) ("A123B" 4 5) ("Aabcdef123B" 10 11)
;->  ("A1234567890B" 22 12) ("AXB" 39 3))

Per includere anche "_"  possiamo usare usare:
(regex-all "A[0-9A-Za-z_]{1,10}B" c)
;-> (("A1B" 0 3) ("A123B" 4 5) ("Aabcdef123B" 10 11)
;->  ("A1234567890B" 22 12) ("AXB" 39 3))

regex-all: versione alternativa fornita da "rrq"
------------------------------------------------
---
rrq
---
I suppose one might use find-all for that. Though, find-all doesn't provide the character index, and it doesn't provide the match overlap variant. It wouldn't be terribly hard to patch the source to make find-all offer the index (eg as $index) similar to the match count.

However, one can use collect and a small helper function to get the following alternative solution:

(define (regex-all EXPR STR OPT ALL)
  (let ((i 0)
        (move-i (fn (X) (setq i (+ (X 1) (if ALL 1 (X -1)))) X)))
  (collect (if (regex EXPR STR OPT i) (move-i $it)))))

(setq a "AAAaBAAAABcADccAAAB")
(regex-all "[A]{3}" a 0)
;-> (("AAA" 0 3) ("AAA" 5 3) ("AAA" 15 3))
(regex-all "[A]{3}" a 0 true)
;-> (("AAA" 0 3) ("AAA" 5 3) ("AAA" 6 3) ("AAA" 15 3))


-------------------------------
Serie di Taylor (Seno e Coseno)
-------------------------------

Espansione in Serie di Taylor per il coseno:

            1     x^2     x^4     x^6            (-1)^k * x^(2*k)
  cos(x) = --- - ----- + ----- - ----- + ... + --------------------
            0!     2!      4!      6!                (2*k)!

Espansione in Serie di Taylor per il seno:

            x     x^3     x^5     x^7             (-1)^k * x^(2k+1)
  sin(x) = --- - ----- + ----- - ----- + ... +  ---------------------
            1!     3!      5!      7!                  (2k+1)!

(define (fact-i num)
"Calculate the factorial of an integer number"
  (if (zero? num)
      1
      (let (out 1L)
        (for (x 1L num)
          (setq out (* out x))))))

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(define (seno x n)
  (let (sum 0)
    (dotimes (k n)
      (setq sum (add sum
        (div (mul (** -1 k) (pow x (+ k k 1))) (fact-i (+ k k 1))))))))

(define (coseno x n)
  (let (sum 0)
    (dotimes (k n)
      (setq sum (add sum
        (div (mul (** -1 k) (pow x (+ k k))) (fact-i (+ k k))))))))

Vediamo per quali angoli si commette l'errore massimo tra le funzioni "seno/coseno" e le primitive "sin/cos":

(define (test step k)
  (local (pi max-err-s max-err-c angle-s angle-c s1 s2 c1 c2 diff-s diff-c)
    (setq pi 3.1415926535897931)
    (setq max-err-s 0)
    (setq max-err-c 0)
    (for (i 0 pi step)
      (setq s1 (cos i))
      (setq s2 (coseno i k))
      (setq diff-s (abs (sub s2 s1)))
      (setq c1 (sin i))
      (setq c2 (seno i k))
      (setq diff-c (abs (sub c2 c1)))
      (when (> diff-c max-err-c)
        (setq max-err-c diff-c)
        (setq angle-c i))
      (when (> diff-s max-err-s)
        (setq max-err-s diff-s)
        (setq angle-s i))
    )
    (list (list diff-s angle-s) (list diff-c angle-c))))

Proviamo:

(test 0.125 5)
;-> ((0.02275837671642078 3.125) (0.006537611038475215 3.125))
(test 0.125 10)
;-> ((3.175125273813251e-009 3.125) (4.733472849793863e-010 3.125))
(test 0.125 12)
;-> ((1.194822019101594e-012 3.125) (1.493145884712277e-013 3.125))
(test 0.125 15)
;-> ((2.220446049250313e-016 2.5) (1.665334536937735e-016 1.75))

(test 0.0001 5)
;-> ((0.02397081882343244 3.1415) (0.006923049402274626 3.1415))
(test 0.0001 10)
;-> ((3.527002911951627e-009 3.1415) (5.285909193127958e-010 3.1415))
(test 0.0001 12)
;-> ((1.354916179252541e-012 3.1415) (1.696848634909556e-013 3.1412))
(test 0.0001 15)
;-> ((3.33066907387547e-016 2.9466) (8.084624549681285e-016 3.0557))

L'errore aumenta quando l'angolo si avvicina al valore di pi greco.

(println pi { } (abs (sub (sin pi) (seno pi 15))))
;-> 3.141592653589793 2.237171498414462e-016
(println pi { } (abs (sub (cos pi) (coseno pi 15))))
;-> 3.141592653589793 2.220446049250313e-016


---------------------------
Interi senza la prima cifra
---------------------------

Trovare un intero che diventa k volte più piccolo quando viene rimossa la sua prima cifra (e diventa x)

  numero = (abc...) = k * (bc...), dove a, b, c, ... sono le cifre del numero

Esempio:
  numero = 7125
  k = 57
  numero senza prima cifra = (bc...) = x = 125
  125*57 = 7125

Algoritmo (forza bruta)
-----------------------
1) Per ogni numero n in un certo intervallo (es. da 10 a 999999):
2) Rimuovere la prima cifra per ottenere x.
3) Calcolare k = n / x (se x ≠ 0 e n mod x = 0).
4) Verificare se k * x == n.

(define (rimuovi-prima-cifra n)
  (int (slice (string n) 1) 0 10))

(define (trova-numeri max-val show)
  (let (soluzioni '())
    (for (n 10 max-val)
      (let ((x (rimuovi-prima-cifra n)))
        (when (and (!= x 0) (= (% n x) 0))
          (let ((k (/ n x)))
            (if show (println n " = " x " * " k))
            (push (list k (list n x k)) soluzioni -1)))))
    soluzioni))

Proviamo:

(trova-numeri 50 true)
;-> 11 = 1 * 11
;-> 12 = 2 * 6
;-> 15 = 5 * 3
;-> 21 = 1 * 21
;-> 22 = 2 * 11
;-> 24 = 4 * 6
;-> 25 = 5 * 5
;-> 31 = 1 * 31
;-> 32 = 2 * 16
;-> 33 = 3 * 11
;-> 35 = 5 * 7
;-> 36 = 6 * 6
;-> 41 = 1 * 41
;-> 42 = 2 * 21
;-> 44 = 4 * 11
;-> 45 = 5 * 9
;-> 48 = 8 * 6
;-> ((11 (11 1 11)) (6 (12 2 6)) (3 (15 5 3)) (21 (21 1 21)) (11 (22 2 11))
;->  (6 (24 4 6)) (5 (25 5 5)) (31 (31 1 31)) (16 (32 2 16)) (11 (33 3 11))
;->  (7 (35 5 7)) (6 (36 6 6)) (41 (41 1 41)) (21 (42 2 21)) (11 (44 4 11))
;->  (9 (45 5 9)) (6 (48 8 6)))

La funzione restituisce una lista con elementi del tipo: (k (n x k)).
Notiamo che esistono soluzioni multiple per alcuni k (es. k=6).

Quali sono i valori unici di k al crescere di n?
------------------------------------------------

Per esempio con n = 100 abbiamo:
(setq k100 (sort (unique (map first (trova-numeri 100)))))
;-> (3 5 6 7 9 11 13 15 16 17 19 21 26 31 36 41 46 51 61 71 81 91)

Per n = 1e6 abbiamo:
(setq k1e6 (sort (unique (map first (trova-numeri 1e6)))))
;-> (3 5 6 7 9 11 13 15 16 17 19 21 25 26 29 31 33 36 37 41 46 49 51 57
;->  61 65 71 73 76 81 91 97 101 113 121 126 129 141 145 151 161 176 181
;->  193 201 225 226 241 251 257 281 289 301 321 351 361 376 401 451 481
;->  501 561 601 626 641 701 721 751 801 876 901 961 1001 1121 1126 1201
;->  1251 1281 1401 1441 1501 1601 1751 1801 1876 2001 2251 2401 2501
;->  2801 3001 3126 3201 3501 3601 3751 4001 4376 4501 4801 5001 5601
;->  5626 6001 6251 6401 7001 7201 7501 8001 8751 9001 9376 10001 11251
;->  12001 12501 14001 15001 15626 16001 17501 18001 18751 20001 21876
;->  22501 24001 25001 28001 28126 30001 31251 32001 35001 36001 37501
;->  40001 43751 45001 50001 56251 60001 62501 70001 75001 80001 87501
;->  90001 100001 112501 120001 125001 140001 150001 160001 175001 180001
;->  200001 225001 250001 300001 350001 400001 450001 500001 600001 700001
;->  800001 900001)

Algoritmo (algebra + forza bruta)
---------------------------------
Dal punto di vista matematico:
sia n un numero intero formato dalle cifre a, b, c, ..., dove a è la prima cifra, e x è il numero che si ottiene rimuovendo la prima cifra di n.
Vogliamo trovare tutti i casi in cui:

  n = k * x

Possiamo esprimere 'n' come:

  n = a * 10^(d-1) + x
  dove 'd' è il numero di cifre di n, e 'a' in [1..9].

Allora:

  a * 10^(d-1) + x = k * x    -->
  a * 10^(d-1) = (k - 1) * x  -->
  x = (a * 10^(d-1))/(k - 1)

Attenzione: non è necessario che x abbia d - 1 cifre.
È sufficiente che rimuovendo la prima cifra di n si ottenga proprio x.

Strategia per trovare la soluzione:
1. Scegliere un numero di cifre d >= 2
2. Scegliere a in [1..9]
3. Per ogni divisore k - 1 di a*10^(d-1), calcolare x = (a * 10^(d-1))/(k - 1)
4. Ricavare n = k * x e controllare se verifica la condizione.

(define (divisors num)
"Generate all the divisors of an integer number"
  (local (f out)
    (cond ((= num 1) '(1))
          (true
           (setq f (factor-group num))
           (setq out '())
           (divisors-aux 0 1)
           (sort out)))))
; auxiliary function
(define (divisors-aux cur-index cur-divisor)
  (cond ((= cur-index (length f))
         (push cur-divisor out -1)
        )
        (true
         (for (i 0 (f cur-index 1))
           (divisors-aux (+ cur-index 1) cur-divisor)
           (setq cur-divisor (* cur-divisor (f cur-index 0)))))))

(define (factor-group num)
"Factorize an integer number"
  (if (= num 1) '((1 1))
    (letn ( (fattori (factor num))
            (unici (unique fattori)) )
      (transpose (list unici (count unici fattori))))))

(define (rimuovi-prima-cifra n)
  (int (slice (string n) 1) 0 10))

Funzione che calcola le soluzioni per i numeri con d cifre:

(define (solve-cifre d)
  (let (risultati '())
    ;; Prova tutte le possibili prime cifre (1..9)
    (for (a 1 9)
      ;; T è la parte "fissa" del numero n: a * 10^(d-1)
      (let ((T (* a (pow 10 (- d 1)))))
        ;; Per ogni divisore di T
        (dolist (m (divisors T))
          (letn (
                 ;; x è il numero che deve essere moltiplicato per k per ottenere n
                 (x (div T m))
                 (k (+ m 1))
                 (n (* k x))
                )
            ;; Se rimuovendo la prima cifra da n si ottiene x, è una soluzione valida
            (when (= x (rimuovi-prima-cifra n))
              ;; Cerca se k è già nella lista associativa dei risultati
              (let ((entry (assoc k risultati)))
                (if entry
                  ;; Aggiunge (n x k) alla lista già presente per k
                  ;; aggiorna valore associato con nuovo elemento (n x k)
                  (setf (lookup k risultati) (append $it (list (list n x k))))
                  ;; Altrimenti crea nuova voce per k nella lista associativa
                  (push (list k (list (list n x k))) risultati -1))))))))
    risultati))

Proviamo:

(solve-cifre 2)
;-> ((3 ((15 5 3))) 
;-> (6 ((12 2 6) (24 4 6) (36 6 6) (48 8 6)))
;-> (11 ((11 1 11) (22 2 11) (33 3 11) (44 4 11) (55 5 11) (66 6 11) (77 7 11)
;->      (88 8 11) (99 9 11)))
;-> (5 ((25 5 5)))
;-> (21 ((21 1 21) (42 2 21) (63 3 21) (84 4 21)))
;-> (7 ((35 5 7)))
;-> (16 ((32 2 16) (64 4 16) (96 6 16)))
;-> (31 ((31 1 31) (62 2 31) (93 3 31)))
;-> (9 ((45 5 9)))
;-> (41 ((41 1 41) (82 2 41)))
;-> (26 ((52 2 26)))
;-> (51 ((51 1 51)))
;-> (13 ((65 5 13)))
;-> (61 ((61 1 61)))
;-> (15 ((75 5 15)))
;-> (36 ((72 2 36)))
;-> (71 ((71 1 71)))
;-> (17 ((85 5 17)))
;-> (81 ((81 1 81)))
;-> (19 ((95 5 19)))
;-> (46 ((92 2 46)))
;-> (91 ((91 1 91))))

Verifichiamo i risultati:

(setq k100 (sort (unique (map first (trova-numeri 100)))))
(setq k100-2 (sort (unique (map first (solve-cifre 2)))))
(= k100 k100-2)
;-> true

Per trovare le stesse soluzioni di "trova-numeri" per d > 2 dobbiamo unire le soluzioni di tutte le cifre da 2 al valore desiderato).

Funzione che calcola le soluzioni per i numeri fino a d cifre:

(define (find-number max-cifre)
  (let (soluzioni (solve-cifre 2))
    (if (> max-cifre 2)
      (for (cifre 3 max-cifre)
        (extend soluzioni (solve-cifre cifre)))
    soluzioni)))

Proviamo:

(find-number 3)
;-> ((3 ((15 5 3))) (6 ((12 2 6) (24 4 6) (36 6 6) (48 8 6)))
;-> ...
;->  (901 ((901 1 901))))

Verifica della correttezza dei risultati delle funzioni:

(setq k1e6 (sort (unique (map first (trova-numeri 1e6)))))
(setq k1e6-2 (sort (unique (map first (find-number 6)))))
(= k1e6 k1e6-2)
;-> true

Test di velocità:

(time (setq k1e7 (sort (unique (map first (trova-numeri 1e7))))))
;-> 7755.561
(time (setq k1e7-2 (sort (unique (map first (find-number 7))))))
;-> 20.239
(= k1e7 k1e7-2)
;-> true

(time (println (length 
      (setq k1e18-2 (sort (unique (map first (find-number 18))))))))
;-> 1382
;-> 1019.944

Nota: la sequenza dei valori di k non esiste in OEIS.


-----------------
Cheryl's Birthday
----------------- 

Vediamo un famoso problema di logica, chiamato "Cheryl's Birthday".

Inglese:
Albert and Bernard just became friends with Cheryl, and they want to know when her birthday is.
Cheryl gave them a list of ten possible dates:
 May 15, May 16, May 19
 June 17, June 18
 July 14, July 16
 August 14, August 15, August 17
Cheryl then tells Albert the month of birth, and Bernard the day (of the month) of birth.
 1) Albert: I don't know when Cheryl's birthday is, but I know that Bernard does not know too.
 2) Bernard: At first I don't know when Cheryl's birthday is, but I know now.
 3) Albert: Then I also know when Cheryl's birthday is.

Italiano:
Albert e Bernard hanno appena fatto amicizia con Cheryl e vogliono sapere quando è il suo compleanno.
Cheryl ha dato loro una lista di dieci possibili date:
15 maggio, 16 maggio, 19 maggio
17 giugno, 18 giugno
14 luglio, 16 luglio
14 agosto, 15 agosto, 17 agosto
Cheryl poi dice ad Albert il mese di nascita e a Bernard il giorno (del mese) di nascita.
1) Albert: Non so quando è il compleanno di Cheryl, ma so che nemmeno Bernard lo sa.
2) Bernard: All'inizio non sapevo quando fosse il compleanno di Cheryl, ma ora lo so.
3) Albert: Allora so anche quando è il compleanno di Cheryl.

Si tratta di una sfida di deduzione logica in cui ogni affermazione fornisce informazioni implicite.

Soluzione
---------

1) Dati iniziali:
Le 10 possibili date sono:
- May 15, 16, 19
- June 17, 18
- July 14, 16
- August 14, 15, 17
Cheryl:
- dice il mese ad Albert,
- dice il giorno a Bernard.

2) Frase 1: Albert dice:
  "I don't know when Cheryl's birthday is, but I know that Bernard does not know too."
- Albert conosce solo il mese, quindi non può conoscere la data precisa.
- Ma sa che anche Bernard, che conosce solo il giorno, non può sapere la data precisa.
Questo è importante. 
Significa che il mese che Albert conosce non può contenere alcun giorno unico — cioè, non può contenere giorni che appaiono solo una volta nella lista.

3) Cerchiamo i giorni unici:
- Giorno 18 -> solo June 18
- Giorno 19 -> solo May 19
Se Albert avesse avuto May, avrebbe potuto pensare che il giorno potesse essere 19, che è unico, e quindi Bernard avrebbe potuto sapere.
Ma Albert è certo che Bernard non sa, quindi il mese non può essere né May né June, perché contengono giorni unici (May 19 e June 18).

4) Eliminiamo:
- May 15, May 16, May 19
- June 17, June 18

5) Rimangono:
- July 14, July 16
- August 14, August 15, August 17

6) Frase 2: Bernard dice:
  "At first I didn't know when Cheryl's birthday is, but now I know."
- Bernard conosce il giorno.
- All'inizio non poteva saperlo (quindi non era 18 o 19), ma ora sì.
- Quindi il giorno che ha deve essere unico tra le date rimaste.

7) Le date rimaste:
- July 14, 16
- August 14, 15, 17

8) Frequenza dei giorni:
- 14 -> appare 2 volte --> (July 14, August 14)
- 16 -> 1 volta        --> (July 16)
- 15 -> 1 volta        --> (August 15)
- 17 -> 1 volta        --> (August 17)

Se Bernard ha sentito 14, non saprebbe ancora.
Ma dice "adesso so", quindi deve avere 16, 15, o 17.

9) Eliminiamo le date con giorno 14:
- July 14
- August 14

10) Rimangono:
- July 16
- August 15
- August 17

11) Frase 3: Albert dice:
  "Then I also know when Cheryl's birthday is."
- Albert conosce solo il mese, ma ora dice di sapere la data esatta.
Quindi, il mese che Albert conosce deve avere una sola data tra le rimanenti.
Le date rimaste:
- July 16
- August 15
- August 17
Vediamo:
- July -> solo July 16
- August -> due opzioni: August 15, August 17
Quindi, se Albert avesse ricevuto August, non saprebbe.
Ma Albert dice di sapere --> Albert ha July --> la data è July 16.

La data di compleanno di Cheryl è: July 16.

Riassunto del ragionamento:
a) Albert capisce che Bernard non può sapere, quindi elimina mesi con giorni unici -> rimangono solo July e August.
b) Bernard ora dice di sapere -> quindi il giorno è unico tra i rimanenti -> rimangono July 16, August 15, August 17.
c) Albert dice di sapere -> quindi il mese dev’essere quello con una sola opzione rimasta -> July.
d) Unica data possibile: July 16.

Simulazione del ragionamento con newLISP:

; Cheryl's Birthday - risoluzione logica in newLISP

; Lista iniziale delle possibili date fornite da Cheryl
(setq tutte-le-date
  '((May 15) (May 16) (May 19)
    (June 17) (June 18)
    (July 14) (July 16)
    (August 14) (August 15) (August 17)))

; Funzione per contare la frequenza dei giorni (parte comune a più step)
(define (giorni-frequenti data)
  (letn ((freq '()))
    (dolist (d data)
      (let ((g (d 1)))
        (if (lookup g freq)
            (setf (lookup g freq) (+ 1 (lookup g freq)))
            (push (list g 1) freq -1))))
    freq))

; ------------------------------------------
; STEP 1 - Albert sa solo il mese,
; ma dice: "So che Bernard non può sapere la data."
; Quindi il mese che Albert ha ricevuto NON deve contenere giorni unici.
; ------------------------------------------

; Calcola le frequenze di tutti i giorni nelle date originali
(setq frequenze (giorni-frequenti tutte-le-date))
;-> ((15 2) (16 2) (19 1) (17 2) (18 1) (14 2))

; Estrai tutti i giorni dalle date
(setq tutti-i-giorni (map (fn (d) (d 1)) tutte-le-date))
;-> (15 16 19 17 18 14 16 14 15 17)

; Trova i giorni unici (che appaiono una sola volta)
(setq giorni-unici
  (filter (fn (g) (= (lookup g frequenze) 1)) tutti-i-giorni))
;-> (19 18)

; Trova i mesi che contengono almeno un giorno unico -> da escludere
(setq mesi-da-escludere
  (unique (map first
               (filter (fn (d) (member (d 1) giorni-unici)) tutte-le-date))))
;-> (May June)

; Filtra tutte le date rimuovendo i mesi da escludere (May, June)
(setq date-step1
  (filter (fn (d) (not (member (d 0) mesi-da-escludere))) tutte-le-date))
;-> ((July 14) (July 16) (August 14) (August 15) (August 17))

; ------------------------------------------
; STEP 2 - Bernard dice: "Ora so la data"
; Quindi il giorno che ha ricevuto deve essere unico tra le date rimaste
; ------------------------------------------

; Calcola frequenze dei giorni nelle date rimaste
(setq frequenze2 (giorni-frequenti date-step1))
;-> ((14 2) (16 1) (15 1) (17 1))

; Filtra le date con giorno a frequenza 1 -> quelle che Bernard può conoscere
(setq date-step2
  (filter (fn (d) (= (lookup (d 1) frequenze2) 1)) date-step1))
;-> ((July 16) (August 15) (August 17))

; ------------------------------------------
; STEP 3 - Albert dice: "Ora so la data"
; Il mese che ha ricevuto deve comparire una sola volta tra le date rimaste
; ------------------------------------------

; Estrai i mesi dalle date dello step 2
(setq mesi (map first date-step2))
;-> (July August August)

; Calcola la frequenza dei mesi rimanenti
(setq frequenze-mesi
  (letn ((f '()))
    (dolist (m mesi)
      (if (lookup m f)
          (setf (lookup m f) (+ 1 (lookup m f)))
          (push (list m 1) f -1)))
    f))
;-> ((July 1) (August 2))

; Filtra la data finale: quella il cui mese ha frequenza 1
(setq data-finale
  (filter (fn (d) (= (lookup (d 0) frequenze-mesi) 1)) date-step2))
;-> ((July 16))


---------------------------------
Interi prodotto di n numeri primi
---------------------------------

Prodotti di esattamente k numeri primi distinti, per k = 1 a 6:
A000040, A006881, A007304, A046386, A046387, A067885.

(define (seq-primi k limite)
  (local (out fattori)
    (setq out '())
    (for (num 2 limite)
      (setq fattori (factor num))
      (if (and (= k (length fattori)) (= fattori (unique fattori)))
          (push num out -1)))
    out))

Sequenza OEIS A000040:
The prime numbers.
  2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61,
  67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137,
  139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211,
  223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, ...

(seq-primi 1 271)
;-> (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61
;->  67 71 73 79 83 89 97 101 103 107 109 113 127 131 137
;->  139 149 151 157 163 167 173 179 181 191 193 197 199 211
;->  223 227 229 233 239 241 251 257 263 269 271)

Sequenza OEIS A006881:
Squarefree semiprimes: Numbers that are the product of two distinct primes.
  6, 10, 14, 15, 21, 22, 26, 33, 34, 35, 38, 39, 46, 51, 55, 57, 58, 62,
  65, 69, 74, 77, 82, 85, 86, 87, 91, 93, 94, 95, 106, 111, 115, 118,
  119, 122, 123, 129, 133, 134, 141, 142, 143, 145, 146, 155, 158, 159
  , 161, 166, 177, 178, 183, 185, 187, 194, 201, 202, 203, 205, ...

(seq-primi 2 205)
;-> (6 10 14 15 21 22 26 33 34 35 38 39 46 51 55 57 58 62
;->  65 69 74 77 82 85 86 87 91 93 94 95 106 111 115 118 
;->  119 122 123 129 133 134 141 142 143 145 146 155 158 159
;->  161 166 177 178 183 185 187 194 201 202 203 205)

Sequenza OEIS A007304:
Sphenic numbers: products of 3 distinct primes.
  30, 42, 66, 70, 78, 102, 105, 110, 114, 130, 138, 154, 165, 170, 174,
  182, 186, 190, 195, 222, 230, 231, 238, 246, 255, 258, 266, 273, 282,
  285, 286, 290, 310, 318, 322, 345, 354, 357, 366, 370, 374, 385, 399,
  402, 406, 410, 418, 426, 429, 430, 434, 435, 438, ...

(seq-primi 3 438)
;-> (30 42 66 70 78 102 105 110 114 130 138 154 165 170 174 182 186 190
;->  195 222 230 231 238 246 255 258 266 273 282 285 286 290 310 318 322
;->  345 354 357 366 370 374 385 399 402 406 410 418 426 429 430 434 435
;->  438)

Sequenza OEIS A046386:
Products of exactly four distinct primes.
  210, 330, 390, 462, 510, 546, 570, 690, 714, 770, 798, 858, 870, 910,
  930, 966, 1110, 1122, 1155, 1190, 1218, 1230, 1254, 1290, 1302, 1326,
  1330, 1365, 1410, 1430, 1482, 1518, 1554, 1590, 1610, 1722, 1770, 1785,
  1794, 1806, 1830, 1870, 1914, 1938, 1974, ...

(seq-primi 4 1974)
;-> (210 330 390 462 510 546 570 690 714 770 798 858 870 910
;->  930 966 1110 1122 1155 1190 1218 1230 1254 1290 1302 1326
;->  1330 1365 1410 1430 1482 1518 1554 1590 1610 1722 1770 1785
;->  1794 1806 1830 1870 1914 1938 1974)

Sequenza OEIS A046387:
Products of exactly 5 distinct primes.
  2310, 2730, 3570, 3990, 4290, 4830, 5610, 6006, 6090, 6270, 6510,
  6630, 7410, 7590, 7770, 7854, 8610, 8778, 8970, 9030, 9282, 9570,
  9690, 9870, 10010, 10230, 10374, 10626, 11130, 11310, 11730, 12090,
  12210, 12390, 12558, 12810, 13090, 13110, ...

(seq-primi 5 13110)
;-> (2310 2730 3570 3990 4290 4830 5610 6006 6090 6270 6510
;->  6630 7410 7590 7770 7854 8610 8778 8970 9030 9282 9570
;->  9690 9870 10010 10230 10374 10626 11130 11310 11730 12090
;->  12210 12390 12558 12810 13090 13110)

Sequenza OEIS A067885:
Products of exactly 6 distinct primes.
  30030, 39270, 43890, 46410, 51870, 53130, 62790, 66990, 67830, 71610,
  72930, 79170, 81510, 82110, 84630, 85470, 91770, 94710, 98670, 99330,
  101010, 102102, 103530, 106590, 108570, 110670, 111930, 114114, 115710,
  117390, 122430, 123690, 124410, 125970, 128310, ...

(seq-primi 6 128310)
;-> (30030 39270 43890 46410 51870 53130 62790 66990 67830 71610
;->  72930 79170 81510 82110 84630 85470 91770 94710 98670 99330
;->  101010 102102 103530 106590 108570 110670 111930 114114 115710
;->  117390 122430 123690 124410 125970 128310)


----------------------------
Breve ripasso sulle hash-map
----------------------------

1) Creazione hash-map hh:
(new Tree 'hh)
;-> hh

2) Contenuto della hash-map:
(hh)
;-> ()

3) Inserimento chiave valore nella hash-map:
(setq key 10)
(setq value "a")
(hh 10 "a")
;-> "a"
(hh)
;-> (("10" "a"))
(hh 20 "b")
;-> "b"
(hh)
;-> (("10" "a") ("20" "b"))
(hh 30 "c")
;-> "c"
(hh)
;-> (("10" "a") ("20" "b") ("30" "c"))

4) Cerca il valore associato ad una chiave:
chiave esistente:
(hh 10)
;-> "a"
chiave non esistente:
(hh 5)
;-> nil

5) Elimina una coppia chiave-valore
(hh 20 nil)
(hh)
;-> (("10" "a") ("30" "c"))

6) Numero di elementi di una hash-map:
(length (hh))
;-> 2

7) Verifica se una hash-map è vuota:
(empty? (hh))
;-> nil

8) Inserisce una lista associativa in una hash-map:
(hh '((1 2) (3 4)))
;-> hh
(hh)
;-> (("1" 2) ("10" "a") ("3" 4) ("30" "c"))

9) Verifica se un simbolo è una hash-map:
  (and (context? (eval hh))
       (not (list? (eval (sym (term hh) hh nil)))))
;-> true

10) Copia i valori di una hash-map in una lista associativa:
(setq lst (hh))
;-> (("1" 2) ("10" "a") ("3" 4) ("30" "c"))
(list? lst)
;-> true

11) Elimina tutti i valori di una hash-map:
(map delete (symbols hh))
;-> (true true true treu nil)
(hh)
;-> ()

12) Elimina una hash-map:
(delete 'hh)
;-> true
(hh)
;-> ERR: invalid function : (hh)


============================================================
-----------------------------------------------
Numeri prodotto di numeri di Fibonacci distinti
-----------------------------------------------

Calcolare la sequenza dei numeri che sono il prodotto di numeri di Fibonacci distinti.

Sequenza OEIS A160009:
Numbers that are the product of distinct Fibonacci numbers.
  0, 1, 2, 3, 5, 6, 8, 10, 13, 15, 16, 21, 24, 26, 30, 34, 39, 40, 42, 48,
  55, 63, 65, 68, 78, 80, 89, 102, 104, 105, 110, 120, 126, 130, 144, 165,
  168, 170, 178, 195, 204, 208, 210, 233, 240, 267, 272, 273, 275, 288,
  312, 315, 330, 336, 340, 377, 390, 432, 440, 442, 445, ...

Esempio di calcolo per calcolare tutti i numeri inferiori a fibo(7) = 13:

1) Calcoliamo i numeri di Fibonacci fino a 13:
Fibonacci = (0 1 1 2 3 5 8 13)

2) Togliamo lo 0 e il 13:
Fibonacci = (1 1 2 3 5 8 13)

3) Il 13 diventa il valore massimo della sequenza (tutti i numeri calcolati devono essere inferiori a 13).
valore-massimo =  13

4) Calcoliamo il power-set (insieme potenza) dei Fibonacci rimasti
powerset(Fibonacci) = powerset(1 1 2 3 5 8 13) = moltiplicazioni =
((1 1 2 3 5 8) (1 1 2 3 5) (1 1 2 3 8) (1 1 2 3) (1 1 2 5 8) (1 1 2 5)
 (1 1 2 8) (1 1 2) (1 1 3 5 8) (1 1 3 5) (1 1 3 8) (1 1 3) (1 1 5 8)
 (1 1 5) (1 1 8) (1 1) (1 2 3 5 8) (1 2 3 5) (1 2 3 8) (1 2 3) (1 2 5 8)
 (1 2 5) (1 2 8) (1 2) (1 3 5 8) (1 3 5) (1 3 8) (1 3) (1 5 8) (1 5) (1 8)
 (1) (1 2 3 5 8) (1 2 3 5) (1 2 3 8) (1 2 3) (1 2 5 8) (1 2 5) (1 2 8)
 (1 2) (1 3 5 8) (1 3 5) (1 3 8) (1 3) (1 5 8) (1 5) (1 8) (1) (2 3 5 8)
 (2 3 5) (2 3 8) (2 3) (2 5 8) (2 5) (2 8) (2) (3 5 8) (3 5) (3 8) (3)
 (5 8) (5) (8) ())

5) Togliamo la lista vuota '() (ultimo elemento):
((1 1 2 3 5 8) (1 1 2 3 5) (1 1 2 3 8) (1 1 2 3) (1 1 2 5 8) (1 1 2 5)
 ...
 (5 8) (5) (8))

6) Adesso calcoliamo la moltiplicazione di ogni elemento della lista e inseriamo il valore nella sequenza solo se è inferiore al 13 (valore massimo):
(6 10 2 3 5 8 1 6 10 2 3 5 8 1 6 10
 2 3 5 8 1 6 10 2 3 5 8)

7) Per creare la sequenza basta prendere i valori unici della lista, ordinarli e inserire uno 0 all'inizio.
(0 1 2 3 5 6 8 10)

Nota: il numero di elementi dell'insieme potenza vale 2^n, dove n è il numero di elementi della lista.
Quindi possiamo calcolare solo pochi numeri di Fibonacci.

Per esempio, il 21-esimo numero di Fibonacci vale 10946.

(define (fibo-lutz num)
  (let ( (out '(0L 1L)) (x 1L) )
    (extend out (series x (fn (y) (+ x (swap y x))) (- num 1)))))

(define (powerset lst)
"Generate all sublists of a list"
  (if (empty? lst)
      (list '())
      (let ((element (first lst))
            (p (powerset (rest lst))))
         (extend (map (fn (subset) (cons element subset)) p) p) )))

Funzione che calcola la sequenza dei numeri che sono il prodotto di numeri di Fibonacci distinti:
(il parametro idx rappresenta l'indice del numero Fibonacci(idx))

(define (seq idx)
  (local (fibo valore-massimo mult out)
    ; calcola la sequenza di Fibonacci fino all'indice idx
    (setq fibo (fibo-lutz idx))
    ; elimina lo 0 iniziale
    (pop fibo)
    ; calcola il valore massimo della sequenza
    ; (ultimo numero della sequenza)
    (setq valore-massimo (pop fibo -1))
    ; calcola l'insieme potenza della lista 'fibo'
    ; lista delle moltiplicazioni da calcolare
    (setq mult (powerset fibo))
    ; elimina la lista vuota '()
    (pop mult -1)
    (setq out '())
    ; ciclo per calcolare ogni moltiplicazione
    (dolist (el mult)
      ; valore della moltiplicazione corrente
      (setq val (apply * el))
      ; inserisce il valore nella soluzione
      ; solo se è inferiore al 'valore-massimo'
      (if (< val valore-massimo) (push val out -1))
    )
    ; prende i valore unici, li ordina e inserisce uno 0 all'inizio
    (push 0L (sort (unique out)))))

Proviamo:

(seq 10)
;-> (0L 1L 2L 3L 5L 6L 8L 10L 13L 15L 16L 21L 24L 26L 30L 34L 39L 40L 42L 48L)

(time (println (seq 21)))
;-> (0L 1L 2L 3L 5L 6L 8L 10L 13L 15L 16L 21L 24L 26L 30L 34L 39L 40L
;->  42L 48L 55L 63L 65L 68L 78L 80L 89L 102L 104L 105L 110L 120L 126L
;->  130L 144L 165L 168L 170L 178L 195L 204L 208L 210L 233L 240L 267L
;->  272L 273L 275L 288L 312L 315L 330L 336L 340L 377L 390L 432L 440L
;->  442L 445L 466L 504L 510L 520L 534L 544L 546L 550L 610L 624L 630L
;->  699L 712L 714L 715L 720L 754L 816L 819L 825L 840L 864L 880L 884L
;->  890L 987L 1008L 1020L 1040L 1131L 1152L 1155L 1157L 1165L 1220L 1320L
;->  1326L 1335L 1360L 1365L 1398L 1424L 1428L 1430L 1440L 1560L 1597L
;->  1632L 1638L 1650L 1680L 1830L 1864L 1869L 1870L 1872L 1885L 1974L
;->  2136L 2142L 2145L 2160L 2184L 2200L 2210L 2262L 2304L 2310L 2314L
;->  2330L 2520L 2584L 2640L 2652L 2670L 2720L 2730L 2961L 3016L 3024L
;->  3026L 3029L 3050L 3120L 3194L 3456L 3465L 3471L 3495L 3536L 3560L
;->  3570L 3575L 3660L 3728L 3738L 3740L 3744L 3770L 4080L 4095L 4181L
;->  4272L 4284L 4290L 4320L 4368L 4400L 4420L 4791L 4880L 4893L 4895L
;->  4896L 4901L 4935L 5040L 5168L 5592L 5607L 5610L 5616L 5655L 5712L
;->  5720L 5760L 5775L 5785L 5922L 6032L 6048L 6052L 6058L 6100L 6552L
;->  6600L 6630L 6765L 6912L 6930L 6942L 6990L 7072L 7120L 7140L 7150L
;->  7752L 7896L 7917L 7920L 7922L 7930L 7985L 8160L 8190L 8362L 9048L
;->  9072L 9078L 9087L 9150L 9240L 9256L 9282L 9320L 9345L 9350L 9360L
;->  9582L 9760L 9786L 9790L 9792L 9802L 9870L 10608L 10680L 10710L 10725L
;->  10920L)
;-> 7062.94

L'algoritmo è corretto, ma è molto lento perchè il calcolo dell'insieme potenza è molto oneroso.

Possiamo utilizzare una "costruzione incrementale":
1) Mantenere una lista dei prodotti generati finora.
2) Per ogni nuovo numero di Fibonacci F, moltiplicare F per ogni prodotto esistente, e aggiungere il nuovo prodotto se è minore del valore massimo.
3) Al termine prendere tutti i valori unici e ordinarli.

(define (seq2 idx)
  (local (fibo valore-massimo out nuovi tmp prodotto)
    ; Calcola la sequenza di Fibonacci fino a idx
    (setq fibo (fibo-lutz idx))
    ; Rimuove lo 0 iniziale
    (pop fibo)
    ; Calcola il valore massimo ammesso
    (setq valore-massimo (last fibo))
    ; Inizializza con 1 la lista dei prodotti
    (setq out '(1L))
    ; Per ogni numero di Fibonacci f
    (dolist (f fibo)
      (setq nuovi '())
      ; Moltiplica ogni valore già presente per f
      (dolist (x out)
        (setq prodotto (* f x))
        ; Solo se minore di valore-massimo
        (if (< prodotto valore-massimo)
            (push prodotto nuovi -1)))
      ; Aggiungi i nuovi prodotti
      (extend out nuovi)
    )
    ; Prende i prodotti unici li ordina e aggiunge 0 all'inizio
    (push 0L (sort (unique out)))))

Proviamo:

(seq2 10)
;-> (0L 1L 2L 3L 5L 6L 8L 10L 13L 15L 16L 21L 24L 26L 30L 34L 39L 40L 42L 48L)

(time (println (seq2 21)))
;-> (0L 1L 2L 3L 5L 6L 8L 10L 13L 15L 16L 21L 24L 26L 30L 34L 39L 40L
;->  42L 48L 55L 63L 65L 68L 78L 80L 89L 102L 104L 105L 110L 120L 126L
;->  130L 144L 165L 168L 170L 178L 195L 204L 208L 210L 233L 240L 267L
;->  272L 273L 275L 288L 312L 315L 330L 336L 340L 377L 390L 432L 440L
;->  442L 445L 466L 504L 510L 520L 534L 544L 546L 550L 610L 624L 630L
;->  699L 712L 714L 715L 720L 754L 816L 819L 825L 840L 864L 880L 884L
;->  890L 987L 1008L 1020L 1040L 1131L 1152L 1155L 1157L 1165L 1220L 1320L
;->  1326L 1335L 1360L 1365L 1398L 1424L 1428L 1430L 1440L 1560L 1597L
;->  1632L 1638L 1650L 1680L 1830L 1864L 1869L 1870L 1872L 1885L 1974L
;->  2136L 2142L 2145L 2160L 2184L 2200L 2210L 2262L 2304L 2310L 2314L
;->  2330L 2520L 2584L 2640L 2652L 2670L 2720L 2730L 2961L 3016L 3024L
;->  3026L 3029L 3050L 3120L 3194L 3456L 3465L 3471L 3495L 3536L 3560L
;->  3570L 3575L 3660L 3728L 3738L 3740L 3744L 3770L 4080L 4095L 4181L
;->  4272L 4284L 4290L 4320L 4368L 4400L 4420L 4791L 4880L 4893L 4895L
;->  4896L 4901L 4935L 5040L 5168L 5592L 5607L 5610L 5616L 5655L 5712L
;->  5720L 5760L 5775L 5785L 5922L 6032L 6048L 6052L 6058L 6100L 6552L
;->  6600L 6630L 6765L 6912L 6930L 6942L 6990L 7072L 7120L 7140L 7150L
;->  7752L 7896L 7917L 7920L 7922L 7930L 7985L 8160L 8190L 8362L 9048L
;->  9072L 9078L 9087L 9150L 9240L 9256L 9282L 9320L 9345L 9350L 9360L
;->  9582L 9760L 9786L 9790L 9792L 9802L 9870L 10608L 10680L 10710L 10725L
;->  10920L)
;-> 15.621

Test di correttezza

(= (seq 21) (seq2 21))
;-> true

Test di velocità

(time (seq 21))
;-> 7844.378
(time (seq2 21))
;-> 0

Calcolare la sequenza dei numeri che sono il prodotto di numeri Fattoriali distinti.

Sequenza OEIS A058295:
Products of distinct factorials.
  1, 2, 6, 12, 24, 48, 120, 144, 240, 288, 720, 1440, 2880, 4320, 5040,
  5760, 8640, 10080, 17280, 30240, 34560, 40320, 60480, 80640, 86400,
  103680, 120960, 172800, 207360, 241920, 362880, 483840, 518400, 604800,
  725760, 967680, 1036800, 1209600, ...

Generalizziamo la funzione "seq2" in modo che possa calcolare la sequenza dei numeri che sono il prodotto dei numeri di una lista data.

(define (single-nums-product lst)
  (local (valore-massimo out nuovi tmp prodotto)
    ; Calcola il valore massimo ammesso
    ; (ultimo valore della lista)
    (setq valore-massimo (last lst))
    ; Inizializza con 1 la lista dei prodotti
    (setq out '(1L))
    ; Per ogni numero della lista
    (dolist (num lst)
      ; Lista dei nuovi valori da aggiungere
      (setq nuovi '())
      ; Moltiplica ogni valore già presente per 'num'
      (dolist (el out)
        (setq prodotto (* num el))
        ; Aggiunge il prodotto a 'nuovi' solo se minore di 'valore-massimo'
        (if (< prodotto valore-massimo)
            (push prodotto nuovi -1)))
      ; Aggiunge la lista 'nuovi' alla lista dei prodotti 'out'
      (extend out nuovi)
    )
    ; Prende i prodotti unici li ordina e aggiunge 0 all'inizio
    (sort (unique out))))

Per i numeri Fattoriali:

(define (fact-i num)
"Calculate the factorial of an integer number"
  (if (zero? num)
      1
      (let (out 1L)
        (for (x 1L num)
          (setq out (* out x))))))

(setq lst (map fact-i (sequence 1 10)))
;-> (1L 2L 6L 24L 120L 720L 5040L 40320L 362880L 3628800L)
(single-nums-product lst)
;-> (1L 2L 6L 12L 24L 48L 120L 144L 240L 288L 720L 1440L 2880L 4320L 5040L
;->  5760L 8640L 10080L 17280L 30240L 34560L 40320L 60480L 80640L 86400L
;->  103680L 120960L 172800L 207360L 241920L 362880L 483840L 518400L 604800L
;->  725760L 967680L 1036800L 1209600L 1451520L 1935360L 2073600L 2177280L)

Per i numeri di Fibonacci:

(define (fibo-lutz num)
  (let ( (out '(0L 1L)) (x 1L) )
    (extend out (series x (fn (y) (+ x (swap y x))) (- num 1)))))

(setq lst (fibo-lutz 20))
; togliamo lo 0 iniziale
(pop lst)

(push 0 (single-nums-product lst))
;-> (0 1L 2L 3L 5L 6L 8L 10L 13L 15L 16L 21L 24L 26L 30L 34L 39L 40L 42L
;->  48L 55L 63L 65L 68L 78L 80L 89L 102L 104L 105L 110L 120L 126L 130L
;->  144L 165L 168L 170L 178L 195L 204L 208L 210L 233L 240L 267L 272L
;->  273L 275L 288L 312L 315L 330L 336L 340L 377L 390L 432L 440L 442L
;->  445L 466L 504L 510L 520L 534L 544L 546L 550L 610L 624L 630L 699L
;->  712L 714L 715L 720L 754L 816L 819L 825L 840L 864L 880L 884L 890L
;->  987L 1008L 1020L 1040L 1131L 1152L 1155L 1157L 1165L 1220L 1320L
;->  1326L 1335L 1360L 1365L 1398L 1424L 1428L 1430L 1440L 1560L 1597L
;->  1632L 1638L 1650L 1680L 1830L 1864L 1869L 1870L 1872L 1885L 1974L
;->  2136L 2142L 2145L 2160L 2184L 2200L 2210L 2262L 2304L 2310L 2314L
;->  2330L 2520L 2584L 2640L 2652L 2670L 2720L 2730L 2961L 3016L 3024L
;->  3026L 3029L 3050L 3120L 3194L 3456L 3465L 3471L 3495L 3536L 3560L
;->  3570L 3575L 3660L 3728L 3738L 3740L 3744L 3770L 4080L 4095L 4181L
;->  4272L 4284L 4290L 4320L 4368L 4400L 4420L 4791L 4880L 4893L 4895L
;->  4896L 4901L 4935L 5040L 5168L 5592L 5607L 5610L 5616L 5655L 5712L
;->  5720L 5760L 5775L 5785L 5922L 6032L 6048L 6052L 6058L 6100L 6552L
;->  6600L 6630L)


----------------------------------------------------------
Numeri prodotto di numeri di Fibonacci/Fattoriali multipli
----------------------------------------------------------

Calcolare la sequenza dei numeri che sono il prodotto di numeri di Fibonacci.
I numeri di Fibonacci possono essere multipli.

Esempio: 8 = 2*2*2

Sequenza OEIS A065108
Positive numbers expressible as a product of Fibonacci numbers.
  1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 18, 20, 21, 24, 25, 26,
  27, 30, 32, 34, 36, 39, 40, 42, 45, 48, 50, 52, 54, 55, 60, 63, 64,
  65, 68, 72, 75, 78, 80, 81, 84, 89, 90, 96, 100, 102, 104, 105, 108,
  110, 117, 120, 125, 126, 128, 130, 135, 136, 144, 150, 156, 160, 162, ...

Calcolare la sequenza dei numeri che sono il prodotto di Fattoriali.
I numeri Fattoriali possono essere multipli.

Sequenza OEIS A001013:
Jordan-Polya numbers: products of factorial numbers A000142.
  1, 2, 4, 6, 8, 12, 16, 24, 32, 36, 48, 64, 72, 96, 120, 128, 144, 192,
  216, 240, 256, 288, 384, 432, 480, 512, 576, 720, 768, 864, 960, 1024,
  1152, 1296, 1440, 1536, 1728, 1920, 2048, 2304, 2592, 2880, 3072, 3456,
  3840, 4096, 4320, 4608, 5040, 5184, 5760, ...

La funzione 'multiple-nums-product' costruisce in maniera incrementale (con BFS) tutti i numeri ottenibili come prodotti di elementi in lst, con ripetizioni ammesse, ma senza superare il valore massimo della lista.
Caratteristiche
- Incrementale: costruisce i prodotti a partire da 1 moltiplicando successivamente con gli elementi di lst.
- Con ripetizioni: un numero può essere usato più volte nei prodotti.
- Senza duplicati: usa una Tree come hash-map per evitare ridondanze.
- BFS: assicura che i prodotti più piccoli vengano generati prima.

Controllo limite: i prodotti che superano valore-massimo vengono scartati.

(define (multiple-nums-product lst)
  (local (valore-massimo queue out prodotto)
    ; Imposta il limite massimo come l'ultimo elemento della lista
    ; Nessun prodotto dovrà superare questo valore
    (setq valore-massimo (last lst))
    ; Crea una hash-map (struttura Tree) per registrare i valori già visitati
    ; Funziona come un "set" per evitare duplicati e ricalcoli
    (new Tree 'hash)
    ; Inizializza la coda per la BFS con il valore iniziale 1
    ; Rappresenta il prodotto neutro di partenza
    (setq queue '(1L))
    ; Lista dei risultati che verranno restituiti
    ; Conterrà tutti i prodotti validi, ordinati e unici
    (setq out '())
    ; Inizia il ciclo principale (Breadth-First Search)
    ; Finché ci sono elementi da esplorare nella coda
    (while queue
      ; Estrae un valore dalla coda
      (setq x (pop queue))
      ; Se non è stato ancora visitato
      (if (not (hash x))
        (begin
          ; Marca il valore come visitato nella hash-map
          (hash x true)
          ; Aggiunge il valore corrente alla lista dei risultati
          (push x out -1)
          ; Genera i nuovi prodotti combinando x con ogni elemento della lista
          (dolist (f lst)
            (setq prodotto (* x f))
            ; Se il prodotto è valido (entro il limite massimo)
            ; lo aggiunge alla coda per ulteriori combinazioni future
            (if (< prodotto valore-massimo)
              (push prodotto queue -1))))))
    ; Dopo la BFS, elimina la hash-map per liberare memoria
    (delete 'hash)
    ; Restituisce la lista dei prodotti validi, ordinati in modo crescente
    (sort out)))

Per i numeri di Fibonacci:

(define (fibo-lutz num)
  (let ( (out '(0L 1L)) (x 1L) )
    (extend out (series x (fn (y) (+ x (swap y x))) (- num 1)))))

(setq lst (fibo-lutz 13))
;-> (0L 1L 1L 2L 3L 5L 8L 13L 21L 34L 55L 89L 144L 233L)
; togliamo lo 0 iniziale
(pop lst)

(multiple-nums-product lst)
;-> (1L 2L 3L 4L 5L 6L 8L 9L 10L 12L 13L 15L 16L 18L 20L 21L 24L 25L 26L
;->  27L 30L 32L 34L 36L 39L 40L 42L 45L 48L 50L 52L 54L 55L 60L 63L 64L
;->  65L 68L 72L 75L 78L 80L 81L 84L 89L 90L 96L 100L 102L 104L 105L 108L
;->  110L 117L 120L 125L 126L 128L 130L 135L 136L 144L 150L 156L 160L 162L
;->  165L 168L 169L 170L 178L 180L 189L 192L 195L 200L 204L 208L 210L 216L
;->  220L 225L)

Per i numeri fattoriali:

(define (fact-i num)
"Calculate the factorial of an integer number"
  (if (zero? num)
      1
      (let (out 1L)
        (for (x 1L num)
          (setq out (* out x))))))

(setq lst (map fact-i (sequence 1 7)))
;-> (1L 2L 6L 24L 120L 720L 5040L)
(multiple-nums-product lst)
;-> (1L 2L 4L 6L 8L 12L 16L 24L 32L 36L 48L 64L 72L 96L 120L 128L 144L 192L
;->  216L 240L 256L 288L 384L 432L 480L 512L 576L 720L 768L 864L 960L 1024L
;->  1152L 1296L 1440L 1536L 1728L 1920L 2048L 2304L 2592L 2880L 3072L 3456L
;->  3840L 4096L 4320L 4608L)


------------------------------------
Targhe alternate con cifre e lettere
------------------------------------

In un paese le targhe delle auto hanno 6 caratteri: 3 cifre e 3 lettere.
Le cifre e le lettere devono essere alternate, quindi le targhe possono avere solo due tipi di configurazioni:
1) LCLCLC
2) CLCLCL
Quante targhe diverse possono essere create?

N = (0..9)  --> 10 simboli
L = (A..Z)  --> 26 simboli

Numero di configurazioni di tipo 1 (LCLCLC) = 10*26*10*26*10*26 = 17576000

Numero di configurazioni di tipo 2 (CLCLCL) = 26*10*26*10*26*10 = 17576000

Totale = 17576000 + 17576000 = 35152000

(* 26 10 26 10 26 10)
;-> 17576000
(+ 17576000 17576000)
;-> 35152000

(char "A")
;-> 65
(char "Z")
;-> 90

(define (calcola)
  (setq out '())
  (setq conta 0)
  (for (a 0 9)
    (for (b 65 90)
      (for (c 0 9)
        (for (d 65 90)
          (for (e 0 9)
            (for (f 65 90)
              ;(push (string a (char b) c (char d) e (char f)) out -1)
              (++ conta)))))))
  conta)

(time (println (calcola)))
;-> 17576000
;-> 528.204

; con push
(time (println (calcola)))
;-> 17576000
;-> 20392.767

(length out)
;-> 17576000

(out 1425000)
;-> "0V0U7S"

============================================================================

