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

============================================================================

