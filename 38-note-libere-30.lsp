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

============================================================================

