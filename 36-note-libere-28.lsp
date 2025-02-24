================

 NOTE LIBERE 28

================

  To build up a library is to create a life.
  It’s never just a random collection of books.

---------
Buon 2025
---------

(string "Buon " (apply * (map char '("-" "-"))))
;-> "Buon 2025"

Come funziona?
(char "-")
;-> 45
(* 45 45)
;-> 2025 ; 2025 è un numero quadrato


-------------------------------------------------
Assegnazione di un valore ad una serie di simboli
-------------------------------------------------

Supponiamo di voler assegnare uno stesso valore (per esempio 0) a più di una variabile/simbolo (per esempio a, b e c).

Dobbiamo scrivere:

(setq a 0)
(setq b 0)
(setq c 0)

Possiamo scrivere una macro per semplificare il processo:

(define-macro (set= val)
  (map set (args) (dup val (length (args)))))

I parametri della macro sono:
1) il valore da assegnare (val)
2) serie di simboli a cui assegnare il valore (args)

Proviamo:

(set= 2 a b c d)
;-> (2 2 2 2)
(println a { } b { } c { } d)
;-> 2 2 2 2

Come si può notare, la macro assegna il valore anche a simboli non ancora definiti (la variabile d).

(set= '(0 1) v1 v2)
;-> ('(0 1) '(0 1))
(println v1 { } v2)
;-> '(0 1) '(0 1)


------------------------------------------
Numeri tetraedici (piramidali triangolari)
------------------------------------------

Un numero tetraedrico, o numero piramidale triangolare, è un numero figurato che rappresenta una piramide con una base triangolare (un tetraedro).
L'n-esimo numero tetraedrico è la somma dei primi n numeri triangolari.

La formula per il calcolo dei numeri tetraedici è la seguente:

  T(n) = binom(n+2, 3) = n*(n + 1)*(n + 2)/6

T(n) è il numero di palline in una piramide triangolare in cui ogni spigolo contiene n palline.

Sequenza OEIS A000292:
Tetrahedral (or triangular pyramidal) numbers: a(n) = C(n+2,3) = n*(n+1)*(n+2)/6.
  0, 1, 4, 10, 20, 35, 56, 84, 120, 165, 220, 286, 364, 455, 560, 680, 816,
  969, 1140, 1330, 1540, 1771, 2024, 2300, 2600, 2925, 3276, 3654, 4060,
  4495, 4960, 5456, 5984, 6545, 7140, 7770, 8436, 9139, 9880, 10660, 11480,
  12341, 13244, 14190, 15180, ...

(define (tetra n)
  (/ (* n (+ n 1) (+ n 2)) 6))

(map tetra (sequence 0 28))
(0 1 4 10 20 35 56 84 120 165 220 286 364 455 560 680 816
 969 1140 1330 1540 1771 2024 2300 2600 2925 3276 3654 4060)


--------------------------
Numeri piramidali quadrati
--------------------------

Un numero piramidale quadrato è un numero figurato che rappresenta una piramide a base quadrata.
L'n-esimo numero di questo tipo è la somma dei quadrati dei primi n numeri naturali.
In formule:

                          n(n + 1)(2n + 1)
 a(n) = Sum[k=1,n] k^2 = ------------------
                                 6
Sequenza OEIS A000330:
Square pyramidal numbers: a(n) = 0^2 + 1^2 + 2^2 + ... + n^2 = n*(n+1)*(2*n+1)/6.
  0, 1, 5, 14, 30, 55, 91, 140, 204, 285, 385, 506, 650, 819, 1015, 1240,
  1496, 1785, 2109, 2470, 2870, 3311, 3795, 4324, 4900, 5525, 6201, 6930,
  7714, 8555, 9455, 10416, 11440, 12529, 13685, 14910, 16206, 17575, 19019,
  20540, 22140, 23821, 25585, 27434, 29370, ...

(define (a n) (/ (* n (+ n 1) (+ (* 2 n) 1)) 6))

(map a (sequence 0 38))
;-> (0 1 5 14 30 55 91 140 204 285 385 506 650 819 1015 1240
;->  1496 1785 2109 2470 2870 3311 3795 4324 4900 5525 6201 6930
;->  7714 8555 9455 10416 11440 12529 13685 14910 16206 17575 19019)


----------------
Numeri dominanti
----------------

La sequenza dei numeri "dominanti" parte da 1 ed ogni numero successivo della sequenza è "i" volte più grande della somma dei numeri da 1 al numero precedente (dove "i" è l'indice del numero corrente).
Dal punto di vista matematico la sequenza è definita nel modo seguente:

  a(1) = 1,
  a(n) = n*Sum[i=1..(n - 1)]a(i), per n > 1

Sequenza OEIS A074143:
a(1) = 1, a(n) = n * Sum_{k=1..n-1} a(k).
  1, 2, 9, 48, 300, 2160, 17640, 161280, 1632960, 18144000, 219542400,
  2874009600, 40475635200, 610248038400, 9807557760000, 167382319104000,
  3023343138816000, 57621363351552000, 1155628453883904000,
  24329020081766400000, 536454892802949120000, ...

(define (seq limite)
  (let ( (arr (array (+ limite 1) '(0L))) (somma 1L) )
    (setf (arr 1) 1L)
    (for (k 2 limite)
      (setf (arr k) (* somma k))
      (setq somma (+ somma (arr k)))
    )
    (slice arr 1)))

Proviamo:

(seq 21)
;-> (1L 2L 9L 48L 300L 2160L 17640L 161280L 1632960L 18144000L 219542400L
;->  2874009600L 40475635200L 610248038400L 9807557760000L 167382319104000L
;->  3023343138816000L 57621363351552000L 1155628453883904000L
;->  24329020081766400000L 536454892802949120000L)


---------
x^6 = 2^6
---------

Data l'espressione x^6 = 2^6, calcolare i valori di x che rendono valida l'uguaglianza.

  x^6 - 2^6 = 0
  (x^3)^2 - (2^3)^2 = 0

  a^2 - b^2 = (a + b)(a - b)
  a = x^3
  b = 2^3

  (x^3 + 2^3)(x^3 - 2^3) = 0
  Caso 1: (x^3 + 2^3) = 0
  Caso 2: (x^3 - 2^3) = 0

  Caso 1: (x^3 + 2^3) = 0
  (a^3 + b^3) = (a + b)(a^2 - ab + b^2)
  (x + 2)(x^2 - 2x + 2^2) = 0
  Caso 1a: (x + 2) = 0  ==> x = -2
  Caso 1b: (x^2 - 2x + 2^2) = 0
  x(1,2) = (- b +- sqrt(b^2 - 4ac))/(2a)
  x(1,2) = 1 +- sqrt(3)i

  Caso 2: (x^3 - 2^3) = 0
  (a^3 - b^3) = (a - b)(a^2 + ab + b^2)
  (x - 2)(x^2 + 2x + 2^2) = 0
  Caso 2a: (x - 2) = 0  ==> x = 2
  Caso 2b: (x^2 + 2x + 2^2) = 0
  x(1,2) = (- b +- sqrt(b^2 - 4ac))/(2a)
  x(1,2) = - 1 +- sqrt(3)i

  Soluzioni: -2, 2, - 1 + sqrt(3)i, - 1 - sqrt(3)i, 1 + sqrt(3)i, 1 - sqrt(3)i


------------------------------------------------
Conteggio di bit nelle sequenze di numeri interi
------------------------------------------------

Dati due numeri interi positivi 'a' e 'b' (con b >= a), calcolare il numero totale di 0 e 1 nei numeri binari che vanno da 'a' a 'b'.

(define (pop-count1 num)
"Calculate the number of 1 in binary value of an integer number"
  (let (counter 0)
    (while (> num 0)
      (setq num (& num (- num 1)))
      (++ counter)
    )
    counter))

(define (pop-count0 num)
"Calculate the number of 0 in binary value of an integer number"
  (- (length (bits num)) (pop-count1 num)))

Funzione che conta i bit 0 e 1 di un intervallo di numeri interi positivi:

(define (bit-count a b)
  (let ( (tot0 0) (tot1 0) )
    (for (num a b)
      (++ tot0 (pop-count0 num))
      (++ tot1 (pop-count1 num)))
    (list tot0 tot1)))

(bit-count 0 10)
;-> (13 17)
(time (println (bit-count 0 1e6)))
;-> (9066447 9884999)
;-> 2659.116

Se poniamo a = 1(binario) e b = 111..1(n+1 bits)(binario) otteniamo la seguente sequenza OEIS:

Sequenza OEIS A000337:
a(n) = (n-1)*2^n + 1.
a(n) gives number of 0's in binary numbers 1 to 111..1 (n+1 bits)
  0, 1, 5, 17, 49, 129, 321, 769, 1793, 4097, 9217, 20481, 45057, 98305,
  212993, 458753, 983041, 2097153, 4456449, 9437185, 19922945, 41943041,
  88080385, 184549377, 385875969, 805306369, 1677721601, 3489660929,
  7247757313, 15032385537, 31138512897, 64424509441, ...

Dove a(n) è il numero di 0 nei numeri binari da 1 a 111..1 (n+1 bits)

(for (i 1 8)
  (setq b (dup "1" i))
  (setq num0 ((bit-count 1 (int b 0 2)) 0))
  (println "n = " (- i 1) " [1," b  "("(int b 0 2)")]: " num0))
;-> n = 0 [1,1(1)]: 0
;-> n = 1 [1,11(3)]: 1
;-> n = 2 [1,111(7)]: 5
;-> n = 3 [1,1111(15)]: 17
;-> n = 4 [1,11111(31)]: 49
;-> n = 5 [1,111111(63)]: 129
;-> n = 6 [1,1111111(127)]: 321
;-> n = 7 [1,11111111(255)]: 769

Se poniamo a = 0 e b = n otteniamo le seguenti sequenze OEIS:

Sequenza OEIS A000788:
Total number of 1's in binary expansions of 0, ..., n.
a(0) = 0, a(2n) = a(n)+a(n-1)+n, a(2n+1) = 2a(n)+n+1
  0, 1, 2, 4, 5, 7, 9, 12, 13, 15, 17, 20, 22, 25, 28, 32, 33, 35, 37, 40,
  42, 45, 48, 52, 54, 57, 60, 64, 67, 71, 75, 80, 81, 83, 85, 88, 90, 93,
  96, 100, 102, 105, 108, 112, 115, 119, 123, 128, 130, 133, 136, 140, 143,
  147, 151, 156, 159, 163, 167, 172, 176, 181, 186, ...

(define (seq1 limite)
  (let (out '())
    (for (i 0 limite) (push ((bit-count 0 i) 1) out -1))))

(seq1 52)
;-> (0 1 2 4 5 7 9 12 13 15 17 20 22 25 28 32 33 35 37 40
;->  42 45 48 52 54 57 60 64 67 71 75 80 81 83 85 88 90 93
;->  96 100 102 105 108 112 115 119 123 128 130 133 136 140 143)

Sequenza OEIS A059015:
Total number of 0's in binary expansions of 0, ..., n.
a(n) = b(n)+1, with b(2n) = b(n)+b(n-1)+n, b(2n+1) = 2b(n)+n
  1, 1, 2, 2, 4, 5, 6, 6, 9, 11, 13, 14, 16, 17, 18, 18, 22, 25, 28, 30, 33,
  35, 37, 38, 41, 43, 45, 46, 48, 49, 50, 50, 55, 59, 63, 66, 70, 73, 76,
  78, 82, 85, 88, 90, 93, 95, 97, 98, 102, 105, 108, 110, 113, 115, 117,
  118, 121, 123, 125, 126, 128, 129, 130, 130, 136, 141, ...

(define (seq0 limite)
  (let (out '())
    (for (i 0 limite) (push ((bit-count 0 i) 0) out -1))))

(seq0 54)
;-> (1 1 2 2 4 5 6 6 9 11 13 14 16 17 18 18 22 25 28 30 33
;->  35 37 38 41 43 45 46 48 49 50 50 55 59 63 66 70 73 76
;->  78 82 85 88 90 93 95 97 98 102 105 108 110 113 115 117)


-----------------
Sequenza fannkuck
-----------------

Questa è una sequenza generata da una semplificazione dell'algoritmo di benchmark "FANNKUCH" definito in "Performing Lisp Analysis of the FANNKUCH Benchmark" di Kenneth R. Anderson e Duane Rettig.
FANNKUCH è l'abbreviazione del termine tedesco Pfannkuchen, ovvero frittelle, in analogia al gesto di girare le frittelle.

https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/fannkuchredux.html

Algoritmo semplificato per calcolare FANNKUCH(n):
-------------------------------------------------

a) Prendere una permutazione di (1,...,n), ad esempio: (4 2 1 5 3).

b) Prendere il primo elemento, qui 4, e invertire l'ordine dei primi 4 elementi: (5 1 2 4 3).

c) Ripetere questo finché il primo elemento non è un 1, quindi l'inversione non cambierà più nulla:
(3 4 2 1 5) (2 4 3 1 5) (4 2 3 1 5) (1 3 2 4 5).

d) Contare il numero di inversioni, qui 5.

e) Fare questo per tutte le n! permutazioni e registrare il numero totale di inversioni.

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

(define (reverse-part obj idx len)
"Reverses part of list or string"
  (extend (slice obj 0 idx)             ; left part
          (reverse (slice obj idx len)) ; invert part
          (slice obj (+ idx len))))     ; right part

Funzione funnkuck:

(define (fannkuck n)
  (local (permute inversioni)
    (setq permute (perm (sequence 1 n)))
    (setq inversioni 0)
    (dolist (p permute)
      (until (= (p 0) 1)
        (setq p (reverse-part p 0 (p 0)))
        (++ inversioni)))
    inversioni))

Proviamo:

(fannkuck 3)
;-> 6
(fannkuck 4)
;-> 38

(time (println (map fannkuck (sequence 1 8))))
;-> (0 1 6 38 265 2115 18508 180260)
;-> 144.943

(time (println (map fannkuck (sequence 1 9))))
;-> (0 1 6 38 265 2115 18508 180260 1911505)
;-> 1478.691

(time (println (map fannkuck (sequence 1 10))))
;-> (0 1 6 38 265 2115 18508 180260 1911505 22169434)
;-> 16967.245

(time (println (map fannkuck (sequence 1 11))))
;-> (0 1 6 38 265 2115 18508 180260 1911505 22169434 277931375)
;-> 266450.518


------------------------------------------------
Verificare se N variabili hanno lo stesso valore
------------------------------------------------

Qaando dobbiamo verificare se alcune varibili, per esempio a, b e c, hanno tutte lo stesso valore, per esempio 1, in genere utilizziamo l'operatore "and" e scriviamo:

(if (and (= a 1) (= b 1) (= c 1)))

Comunque possiamo anche scrivere una espressione più compatta:

(if (= 1 a b c)

opppure

(if (= a b c 1))

Che, oltre ad essere più corta, è anche più elegante.

(setq a 1) (setq b 1) (setq c 1)
(if (and (= a 1) (= b 1) (= c 1)))
;-> true
(if (= 1 a b c))
;-> true

(setq a 1) (setq b 2) (setq c 1)
(if (and (= a 1) (= b 1) (= c 1)))
;-> nil
(if (= 1 a b c))
;-> nil

Vediamo quale espressione è più veloce.
(setq a 1) (setq b 1) (setq c 1)

(define (e1 iterazioni)
  (for (i 1 iterazioni) (if (and (= a 1) (= b 1) (= c 1)) nil)))

(define (e2 iterazioni)
  (for (i 1 iterazioni) (if (= 1 a b c))))

(time (e1 1e7))
;-> 980.244
(time (e2 1e7))
;-> 490.232

La versione compatta è anche più veloce.


---------------------------
Numeri quadrati persistenti
---------------------------

Un numero quadrato persistente di tipo 1 è un numero che rimane quadrato se tutte le sue cifre vengono incrementate di 1.
Nota: 9 + 1 = 0

Un numero quadrato persistente di tipo 2 è un numero che rimane quadrato se tutte le sue cifre vengono decrementate di 1.
Nota: 0 - 1 = 9

Per esempio (tipo 1):
numero = 25 (che è il quadrato di 5)
numero con cifre incrementate di 1 = 36 (che è il quadrato di 6)

Sequenza OEIS A117755:
Squares which remain squares when each digit is replaced by the next digit.
  0, 9, 25, 2025, 13225, 1974025, 4862025, 6943225, 60415182025, 207612366025,
  916408817340025, 9960302475729225, 153668543313582025, 1978088677245614025,
  13876266042653742025, 20761288044852366025, 47285734107144405625,
  406066810454367265225, ...

Per esempio (tipo 2):
numero = 24336 (che è il quadrato di 156)
numero con cifre decrementate di 1 = 13225 (che è il quadrato di 115)
Nota: in questo caso numeri del tipo "1..1abc..." diventano "0..0xyz...".

Funzione che incrementa di 1 le cifre di un numero (9 + 1 = 0):

(define (digit+1 num)
  (if (zero? num) 1
      ;else
      (let (lst '() (new-digit 0))
        ; Crea una lista con le cifre del numero aumentate di 1 (9 + 1 = 0)
        (while (!= num 0)
          (setq new-digit (% (+ (% num 10) 1) 10))
          (push new-digit lst)
          (setq num (/ num 10)))
        ; Crea il nuovo numero con la lista delle cifre
        (dolist (el lst) (setq num (+ el (* num 10))))
        num)))

Proviamo:

(digit+1 0)
;-> 1
(digit+1 9)
;-> 0
(digit+1 25)
;-> 36
(digit+1 1899)
;-> 2900
(digit+1 9999)
;-> 0
(digit+1 25L)
;-> 36L

Funzione che decrementa di 1 le cifre di un numero (0 - 1 = 9):

(define (digit-1 num)
  (if (zero? num) 9
      ;else
      (let (lst '() (new-digit 0))
        ; Crea una lista con le cifre del numero diminuite di 1 (0 - 1 = 9)
        (while (!= num 0)
          (if (zero? (% num 10))
              (setq new-digit 9)
              (setq new-digit (- (% num 10) 1)))
          (push new-digit lst)
          (setq num (/ num 10)))
        ; Crea il nuovo numero con la lista delle cifre
        (dolist (el lst) (setq num (+ el (* num 10))))
        num)))

Proviamo:

(digit-1 0)
;-> 9
(digit-1 1)
;-> 0
(digit-1 36)
;-> 25
(digit-1 2900)
;-> 1899
(digit-1 118336)
;-> 7225 ; perchè 11 diventa 00

(define (square? num)
"Check if an integer is a perfect square"
  (let (isq (int (sqrt num)))
    (= num (* isq isq))))

; Versione alternativa (big-integer, slower)
;(define (square? num)
;"Check if an integer is a perfect square"
;  (local (a)
;    (setq a (bigint num))
;    (while (> (* a a) num)
;      (setq a (/ (+ a (/ num a)) 2))
;    )
;    (= (* a a) num)))
;
;(square? 0)
;-> 0

Funzione che calcola i numeri quadrati persistenti di tipo 1 fino ad un dato limite:

(define (square-plus limite)
  (let ( (out '()) (sq 0) )
    (for (i 0 limite)
      (setq sq (* i i))
      (setq sq+1 (digit+1 sq))
      (when (square? sq+1)
          (push sq out -1)
          (println (list (sqrt sq) sq) { } (list (sqrt sq+1) sq+1))))
    out))

Proviamo:

(square-plus 1e4)
;-> (0 0) (1 1)
;-> (3 9) (0 0)
;-> (5 25) (6 36)
;-> (45 2025) (56 3136)
;-> (115 13225) (156 24336)
;-> (1405 1974025) (1444 2085136)
;-> (2205 4862025) (2444 5973136)
;-> (2635 6943225) (2656 7054336)
;-> (0 9 25 2025 13225 1974025 4862025 6943225)

(time (println (square-plus 1e7)))
;-> (0 0) (1 1)
;-> (3 9) (0 0)
;-> (5 25) (6 36) !!!
;-> (45 2025) (56 3136) !!!
;-> (115 13225) (156 24336)
;-> (1405 1974025) (1444 2085136)
;-> (2205 4862025) (2444 5973136)
;-> (2635 6943225) (2656 7054336)
;-> (245795 60415182025) (267444 71526293136)
;-> (455645 207612366025) (564556 318723477136)
;-> (0 9 25 2025 13225 1974025 4862025 6943225 60415182025 207612366025)
;-> 37686.073

(time (println (squad 1e8)))
;-> (0 0) (1 1)
;-> (3 9) (0 0)
;-> (5 25) (6 36)
;-> (45 2025) (56 3136)
;-> (115 13225) (156 24336)
;-> (1405 1974025) (1444 2085136)
;-> (2205 4862025) (2444 5973136)
;-> (2635 6943225) (2656 7054336)
;-> (245795 60415182025) (267444 71526293136)
;-> (455645 207612366025) (564556 318723477136)
;-> (30272245 916408817340025) (5245944 27519928451136)
;-> (99801315 9960302475729225) (8450656 71413586830336)
;-> (0 9 25 2025 13225 1974025 4862025 6943225 60415182025 207612366025
;->  916408817340025 9960302475729225)
;-> 426837.95

Funzione che calcola i numeri quadrati persistenti di tipo 2 fino ad un dato limite:

(define (square-minus limite)
  (let ( (out '()) (sq 0) (sq-1 0))
    (for (i 0 limite)
      (setq sq (* i i))
      (setq sq-1 (digit-1 sq))
      (when (square? sq-1)
          (push sq out -1)
          (println (list (sqrt sq) sq) { } (list (sqrt sq-1) sq-1))))
    out))

Proviamo:

(square-minus 1e4)
;-> (0 0) (3 9)
;-> (1 1) (0 0)
;-> (6 36) (5 25)
;-> (56 3136) (45 2025)
;-> (156 24336) (115 13225)
;-> (344 118336) (85 7225)
;-> (356 126736) (125 15625)
;-> (1444 2085136) (1405 1974025)
;-> (2444 5973136) (2205 4862025)
;-> (2656 7054336) (2635 6943225)
;-> (0 1 36 3136 24336 118336 126736 2085136 5973136 7054336)

(time (println (square-minus 1e7)))
;-> (0 0) (3 9)
;-> (1 1) (0 0)
;-> (6 36) (5 25)
;-> (56 3136) (45 2025)
;-> (156 24336) (115 13225)
;-> (344 118336) (85 7225)
;-> (356 126736) (125 15625)
;-> (1444 2085136) (1405 1974025)
;-> (2444 5973136) (2205 4862025)
;-> (2656 7054336) (2635 6943225)
;-> (10656 113550336) (1565 2449225)
;-> (267444 71526293136) (245795 60415182025)
;-> (336944 113531259136) (49195 2420148025)
;-> (371156 137756776336) (163235 26645665225)
;-> (564556 318723477136) (455645 207612366025)
;-> (1192856 1422905436736) (558475 311894325625)
;-> (0 1 36 3136 24336 118336 126736 2085136 5973136 7054336 113550336
;->  71526293136 113531259136 137756776336 318723477136 1422905436736)
;-> 67669.743


-----------
Logging I/O
-----------

In any mode, newLISP can write a log when started with the -l or -L option.
Depending on the mode newLISP is running, different output is written to the log file.
Both options always must specify the path of a log-file.
The path may be a relative path and can be either attached or detached to the -l or -L option.
If the file does not exist, it is created when the first logging output is written.

newlisp -l ./logfile.txt -c

newlisp -L /usr/home/www/log.txt -http -w /usr/home/www/htpdocs

The following table shows the items logged in different situations:

Logging mode Command-line and net-eval with -c      HTTP server with -http
------------ ---------------------------------      ----------------------
newlisp -l   log only input and network connections log only network connections
newlisp -L   log also newLISP output (w/o prompts)  log also HTTP requests

All logging output is written to the file specified after the -l or -L option.

Nota: se il file di log esiste, le nuove attività di logging vengono registrate alla fine del file.

Vedi il file "_npp-newlisp-log.ahk" nella cartella "data".


----------------------------------------------------------
Numeri nella forma x^2 + y^2 + z^2 (Somma di tre quadrati)
----------------------------------------------------------

Determinare la lista dei numeri che sono nella forma: x^2 + y^2 + z^2, con x,y,z >= 0.

Sequenza OEIS A000378:
Sums of three squares: numbers of the form x^2 + y^2 + z^2.
  0, 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 19, 20, 21,
  22, 24, 25, 26, 27, 29, 30, 32, 33, 34, 35, 36, 37, 38, 40, 41, 42, 43,
  44, 45, 46, 48, 49, 50, 51, 52, 53, 54, 56, 57, 58, 59, 61, 62, 64, 65,
  66, 67, 68, 69, 70, 72, 73, 74, 75, 76, 77, 78, 80, 81, 82, 83, ...

Algoritmo brute-force
---------------------
Doppio ciclo 'for' per x e y e verifica del valore di z.

(define (square? num)
"Check if an integer is a perfect square"
  (let (isq (int (sqrt num)))
    (= num (* isq isq))))

(define (xyz1? num)
  (catch
    (let ( (x 0) (y 0) )
      (for (x 0 1e7)
        (if (> (* 3 x x) num) (throw nil))
        (setq quit-y nil)
        (for (y x 1e7 1 quit-y)
          (cond ((> (+ (* x x) (* 2 y y)) num) (setq quit-y true))
                ((square? (- num (* x x) (* y y))) (throw true))))))))

Proviamo:

(xyz1? 30)
;-> true
(xyz1? 31)
;-> nil

(filter xyz1? (sequence 0 100))
;-> (0 1 2 3 4 5 6 8 9 10 11 12 13 14 16 17 18 19 20 21
;->  22 24 25 26 27 29 30 32 33 34 35 36 37 38 40 41 42 43
;->  44 45 46 48 49 50 51 52 53 54 56 57 58 59 61 62 64 65
;->  66 67 68 69 70 72 73 74 75 76 77 78 80 81 82 83 84 85
;->  86 88 89 90 91 93 94 96 97 98 99 100)

Algoritmo matematico
--------------------

(define (** num power)
"Calculates the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(define (calc n b)
  (let (v 0)
    (while (and (> n 1) (zero? (% n b)))
      (setq n (/ n b))
      (++ v))
    v))

(define (xyz2? n) (!= (% (/ n (** 4 (calc n 4))) 8) 7))

Proviamo:

(xyz2? 30)
;-> true
(xyz2? 31)
;-> nil

(filter xyz2? (sequence 0 100))
;-> (0 1 2 3 4 5 6 8 9 10 11 12 13 14 16 17 18 19 20 21
;->  22 24 25 26 27 29 30 32 33 34 35 36 37 38 40 41 42 43
;->  44 45 46 48 49 50 51 52 53 54 56 57 58 59 61 62 64 65
;->  66 67 68 69 70 72 73 74 75 76 77 78 80 81 82 83 84 85
;->  86 88 89 90 91 93 94 96 97 98 99 100)

Vediamo se le funzioni producono gli stessi risultati:

(= (filter xyz1? (sequence 0 1000)) (filter xyz2? (sequence 0 1000)))
;-> true

Vediamo la velocità delle due funzioni:

(time (filter xyz1? (sequence 0 1e4)))
;-> 937.21
(time (filter xyz2? (sequence 0 1e4)))
;-> 15.585


--------------------------------
Numero minimo rimuovendo K cifre
--------------------------------

Dato un numero intero non negativo N e un intero k, restituire il più piccolo intero possibile dopo aver rimosso k cifre da N.
Esempi:

Numero = 3611729, k = 3
Minimo = 1129

Numero = 123456789, k = 5
Minimo = 1234

Numero = 1000, k = 1
Minimo = 0

Numero = 10700, k = 1
Minimo = 700

Numero = 1700, k = 1
Minimo = 100

Numero = 77, k = 2
Minimo = 0

Algoritmo greedy
----------------
Per avere un numero il più piccolo possibile, deve risultare che i valori posizionali più alti (come migliaia, centinaia , decine, ecc.) abbiano le cifre più piccole possibili.
Pertanto, durante l'analisi dell numero da sinistra a destra, se incontriamo una cifra più grande di quella successiva, rimuoviamo la cifra più grande (che ha un valore posizionale più alto).
Questa decisione è detta 'greedy' perché fa la scelta migliore a ogni passaggio, per posizionare le cifre più piccole nei valori posizionali più alti.
Utilizziamo una pila per gestire i dati.
Ogni volta che aggiungiamo una nuova cifra alla pila, la confrontiamo con l'elemento in cima alla pila (che rappresenta la cifra precedente nel numero).
Se la nuova cifra è più piccola, significa che possiamo rendere il numero più piccolo estraendo la cifra più grande dalla pila.
Ripetiamo questo processo k volte.
La pila rappresenta le cifre del numero risultante con le cifre più piccole in basso (valori posizionali più alti).
Dopo aver eseguito k rimozioni, oppure il numero è stato completamente analizzato e abbiamo rimosso le ultime n - k cifre dallo stack (dove n è la lunghezza del numero), ricostruiamo il numero con le cifre contenute nello stack.
Gli zeri iniziali non influenzano il valore del numero.
Se vengono rimosse tutte le cifre restituiamo 0 (che è il più piccolo intero non negativo).

(define (minimo num k)
  (cond ((>= k (length num)) 0)
        ((zero? k) num)
        (true
  (local (out stack)
    (setq out 0)
    (setq stack '())
    ; Converte il numero in stringa
    (setq num (string num))
    ; Ciclo sulle cifre del numero...
    (for (i 0 (- (length num) 1))
      ; Finché la cifra corrente è più piccola dell'ultima cifra nello stack e
      ; abbiamo ancora cifre da rimuovere (k > 0), rimuove l'ultima cifra
      (while (and (> k 0) (!= stack '()) (> (stack -1) (num i)))
        (pop stack -1)
        (-- k)
      )
      ; Inserisce la cifra corrente nello stack
      (push (num i) stack -1)
    )
    ;(print stack) (read-line)
    ; Se dopo il ciclo dobbiamo rimuovere altre cifre,
    ; occorre rimuoverle dalla fine.
    (while (> k 0)
      (pop stack -1)
      (-- k)
    )
    ; Converte lo stack (lista) di cifre (stringhe) in numero intero
    (dolist (el stack) (setq out (+ (int el 0 10) (* out 10))))))))

Proviamo:

(minimo 3611729 3)
;-> 1129
(minimo 123456789 5)
;-> 1234
(minimo 1000 1)
;-> 0
(minimo 10700 1)
;-> 700
(minimo 1700 1)
;-> 100
(minimo 77 2)
;-> 0


----------------
Giorni fortunati
----------------

Un giorno viene chiamato "fortunato" se i numeri della data (anno, mese, giorno) sono composti da cifre tutte diverse.
Per esempio il 23 aprile 1965 (1965 4 23) è stato un giorno fortunato.

(define (int-list num)
"Convert an integer to a list of digits"
  (let (out '())
    (while (!= num 0)
      (push (% num 10) out)
      (setq num (/ num 10))) out))

Funzione che verifica se una data è valida:

(define (verify-date anno mese giorno)
  (let ((giorni-mese '(0 31 28 31 30 31 30 31 31 30 31 30 31))
        (bisestile (and (= 0 (mod anno 4)) (or (!= 0 (mod anno 100)) (= 0 (mod anno 400))))))
    (if bisestile (setf (giorni-mese 2) 29))
    (not (or (< mese 1) (> mese 12) (< giorno 1) (> giorno (giorni-mese mese))))))

Funzione che verifica se un giorno (data) è fortunato:

(define (lucky-day? anno mese giorno)
  (let (lst (append (int-list anno) (int-list mese) (int-list giorno)))
    (= (unique lst) lst)))

Proviamo:

(lucky-day? 1965 4 23)
;-> true
(lucky-day? 2025 12 31)
;-> nil

Funzione che trova i giorni fortunati in un intervallo di anni:

(define (luckys anno1 anno2)
  (let (out '())
  (for (anno anno1 anno2)
    (for (mese 1 12)
      (for (giorno 1 31)
        (if (and (lucky-day? anno mese giorno) (verify-date anno mese giorno))
            (push (list anno mese giorno) out -1)))))
  out))

Proviamo:

(luckys 1980 1982)
;-> ((1980 2 3) (1980 2 4) (1980 2 5) (1980 2 6) (1980 2 7) (1980 3 2) 
;->  (1980 3 4) (1980 3 5) (1980 3 6) (1980 3 7) (1980 3 24) (1980 3 25)
;->  ...
;->  (1982 6 30) (1982 7 3) (1982 7 4) (1982 7 5) (1982 7 6) (1982 7 30))

(luckys 1990 2000)
;-> ()


---------------------------------------------------
Numero di ipercubi 4D in un ipercubo n-dimensionale
---------------------------------------------------

Quanti ipercubi 4D si trovano in un ipercubo n-dimensionale?

Sequenza OEIS A003472:
a(n) = 2^(n-4)*C(n,4).
  1, 10, 60, 280, 1120, 4032, 13440, 42240, 126720, 366080, 1025024, 2795520,
  7454720, 19496960, 50135040, 127008768, 317521920, 784465920, 1917583360,
  4642570240, 11142168576, 26528972800, 62704844800, 147220070400, ...

Formula: a(n) = Sum[k=4..n]binom(n k)*binom(k 4)

(define (binom num k)
"Calculates the binomial coefficient (n k) = n!/(k!*(n - k)!) (combinations of k elements without repetition from n elements)"
  (cond ((> k num) 0L)
        ((zero? k) 1L)
        ((< k 0) 0L)
        (true
          (let (r 1L)
            (for (d 1 k)
              (setq r (/ (* r num) d))
              (-- num)
            )
          r))))

(define (a n)
  (let (sum 0)
    (for (k 4 n) (setq sum (+ sum (* (binom n k) (binom k 4)))))
    sum))

(map a (sequence 4 27))
;-> (1 10 60 280 1120 4032 13440 42240 126720 366080 1025024 2795520
;->  7454720 19496960 50135040 127008768 317521920 784465920 1917583360
;->  4642570240 11142168576 26528972800 62704844800 147220070400)


-------------------------------
Intersezione di più liste (set)
-------------------------------

Per effettuare l'intersezione tra due liste possiamo usare la funzione "intersect".

***********************
>>> funzione INTERSECT
***********************
sintassi: (intersect list-A list-B)
sintassi: (intersect list-A list-B bool)

Nella prima sintassi, intersect restituisce un elenco contenente una copia di ogni elemento trovato sia in list-A che in list-B.

(intersect '(3 0 1 3 2 3 4 2 1) '(1 4 2 5))
;-> (2 4 1)

Nella seconda sintassi, intersect restituisce una lista di tutti gli elementi in list-A che sono anche in list-B, senza eliminare i duplicati in list-A. bool è un'espressione che valuta true o qualsiasi altro valore diverso da nil.

(intersect '(3 0 1 3 2 3 4 2 1) '(1 4 2 5) true)
;-> (1 2 4 2 1)

Vedi anche le funzioni per i set "difference", "unique" e "union".
In the first syntax, intersect returns a list containing one copy of each element found both in list-A and list-B.

(intersect '(3 0 1 3 2 3 4 2 1) '(1 4 2 5))  
;-> (2 4 1)
------------

Se vogliamo effettuare l'intersezione di più liste dobbiamo applicare "intersect" ripetutamente.
Per esempio, per effettuare l'intersezione di 3 liste possiamo scrivere:

(setq A '(1 2 3))
(setq B '(1 2))
(setq C '(2 3 4))
(intersect (intersect A B) C)
;-> (2)

Possiamo scrivere una funzione "intersects" che effettua l'intersezione di più liste:

(define (intersects)
  (let (base (args 0))
    (for (k 1 (- (length (args)) 1))
      (setq base (intersect base (args k))))))

Proviamo:

(intersects A B C)
;-> (2)
(apply intersects (list A B C))
;-> (2)


----------------------------
Numeri poligonali (multipli)
----------------------------

Un numero poligonale è un numero figurato che può essere disposto a raffigurare un poligono regolare.
Esempi:
Numero triangolare = 10             Numero quadrato = 16

            *                               * * * *
           * *                              * * * *    
          * * *                             * * * *
         * * * *                            * * * *

Un numero poligonale multiplo è un numero figurato che può essere disposto a raffigurare più di un poligono regolare.
Esempio:
Numero poligonale multiplo = 36 (triangolare e quadrato)

            *               * * * * * *
           * *              * * * * * *    
          * * *             * * * * * *
         * * * *            * * * * * *
        * * * * *           * * * * * *
       * * * * * *          * * * * * *
      * * * * * * *                         
     * * * * * * * * 

Formule per il calcolo dei numeri poligonali:

  Numeri triangolari = (n * (n - 1))/2
  Numeri quadrati    = n * n
  Numeri pentagonali = (n * (3*n - 1))/2
  Numeri esagonali   = n * (2*n - 1)
  Numeri eptagonali  = (5*n*n - 3*n)/2
  Numeri ottagonali  = (3*n*n - 2*n)
  ...

Formula generale
----------------

  Numeri p-gonali = p*n*(n - 1)/2 - n*(n - 2)
  
  Numeri p-gonali = (1/2)*n*[(p - 2)*n - (p - 4)]

oppure:

                     (p - 2)*n^2 - (p - 4)*n
  Numeri p-gonali = -------------------------
                               2

Formula inversa
---------------

Per un dato valore p-gonale X, è possibile trovare n mediante la formula:

       sqrt(8*(p - 2)*X + (p - 4)^2) + p - 4
  n = ---------------------------------------
                     2*(p - 2)

Verifica di un numero X
-----------------------

Un numero intero X è un numero p-gonale se:

  8*(p - 2)*X + (p - 4)^2 è un quadrato perfetto

Funzione che calcola il valore n-esimo dei numeri poligonali con p lati:

(define (polygonal-number p n) (- (/ (* p n (- n 1)) 2) (* n (- n 2))))

Proviamo con p (numero lati del poligono) che va da 3 a 8:

(for (p 3 8)
  (println "numeri " p "-gonali:")
  (println (map (curry polygonal-number p) (sequence 1 20))))
;-> numeri 3-gonali:
;-> (1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210)
;-> numeri 4-gonali:
;-> (1 4 9 16 25 36 49 64 81 100 121 144 169 196 225 256 289 324 361 400)
;-> numeri 5-gonali:
;-> (1 5 12 22 35 51 70 92 117 145 176 210 247 287 330 376 425 477 532 590)
;-> numeri 6-gonali:
;-> (1 6 15 28 45 66 91 120 153 190 231 276 325 378 435 496 561 630 703 780)
;-> numeri 7-gonali:
;-> (1 7 18 34 55 81 112 148 189 235 286 342 403 469 540 616 697 783 874 970)
;-> numeri 8-gonali:
;-> (1 8 21 40 65 96 133 176 225 280 341 408 481 560 645 736 833 936 1045 1160)

Sequenza OEIS A000217:
Triangular numbers: a(n) = binomial(n+1,2) = n*(n+1)/2 = 0 + 1 + 2 + ... + n.
  0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136, 153,
  171, 190, 210, 231, 253, 276, 300, 325, 351, 378, 406, 435, 465, 496,
  528, 561, 595, 630, 666, 703, 741, 780, 820, 861, 903, 946, 990, 1035,
  1081, 1128, 1176, 1225, 1275, 1326, 1378, 1431, ...

Sequenza OEIS A000290:
The squares: a(n) = n^2.
  0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100, 121, 144, 169, 196, 225, 256, 289,
  324, 361, 400, 441, 484, 529, 576, 625, 676, 729, 784, 841, 900, 961, 1024,
  1089, 1156, 1225, 1296, 1369, 1444, 1521, 1600, 1681, 1764, 1849, 1936,
  2025, 2116, 2209, 2304, 2401, 2500, ...

Sequenza OEIS A000326:
Pentagonal numbers: a(n) = n*(3*n-1)/2.
  0, 1, 5, 12, 22, 35, 51, 70, 92, 117, 145, 176, 210, 247, 287, 330, 376,
  425, 477, 532, 590, 651, 715, 782, 852, 925, 1001, 1080, 1162, 1247, 1335,
  1426, 1520, 1617, 1717, 1820, 1926, 2035, 2147, 2262, 2380, 2501, 2625,
  2752, 2882, 3015, 3151, ...

Sequenza OEIS A000384:
Hexagonal numbers: a(n) = n*(2*n-1).
  0, 1, 6, 15, 28, 45, 66, 91, 120, 153, 190, 231, 276, 325, 378, 435, 496,
  561, 630, 703, 780, 861, 946, 1035, 1128, 1225, 1326, 1431, 1540, 1653,
  1770, 1891, 2016, 2145, 2278, 2415, 2556, 2701, 2850, 3003, 3160, 3321,
  3486, 3655, 3828, 4005, 4186, 4371, 4560, ...

Sequenza OEIS A000566:
Heptagonal numbers (or 7-gonal numbers): n*(5*n-3)/2.
  0, 1, 7, 18, 34, 55, 81, 112, 148, 189, 235, 286, 342, 403, 469, 540, 616,
  697, 783, 874, 970, 1071, 1177, 1288, 1404, 1525, 1651, 1782, 1918, 2059,
  2205, 2356, 2512, 2673, 2839, 3010, 3186, 3367, 3553, 3744, 3940, 4141,
  4347, 4558, 4774, 4995, 5221, 5452, 5688, ...

Sequenza OEIS A000567:
Octagonal numbers: n*(3*n-2). Also called star numbers.
  0, 1, 8, 21, 40, 65, 96, 133, 176, 225, 280, 341, 408, 481, 560, 645, 736,
  833, 936, 1045, 1160, 1281, 1408, 1541, 1680, 1825, 1976, 2133, 2296, 2465,
  2640, 2821, 3008, 3201, 3400, 3605, 3816, 4033, 4256, 4485, 4720, 4961,
  5208, 5461, ...

Nota: per i numeri ottagonali 0, 1, 8, 21, 40, 65, ...
Write 1,2,3,4,... in a hexagonal spiral around 0:
Then a(n) is the sequence found by reading the line from 0,1,8,21,...
The spiral:

                 85--84--83--82--81--80
                 /                     \
               86  56--55--54--53--52  79
               /   /                 \   \
             87  57  33--32--31--30  51  78
             /   /   /             \   \   \
           88  58  34  16--15--14  29  50  77
           /   /   /   /         \   \   \   \
         89  59  35  17   5---4  13  28  49  76
         /   /   /   /   /     \   \   \   \   \
       90  60  36  18   6   0   3  12  27  48  75
       /   /   /   /   /   /   /   /   /   /   /
     91  61  37  19   7   1---2  11  26  47  74
       \   \   \   \   \ .       /   /   /   /
       92  62  38  20   8---9--10  25  46  73
         \   \   \   \ .           /   /   /
         93  63  39  21--22--23--24  45  72
           \   \   \ .               /   /
           94  64  40--41--42--43--44  71
             \   \ .                   /
             95  65--66--67--68--69--70
               \ .
               96

Scriviamo una funzione che trova i valori che compaiono in sequenze diverse (cioè i numeri poligonali multipli).
La funzione prende una lista di interi che rappresentano il tipo di numeri poligonali (3 = numero triangolare, 4 = numero quadrato, ecc.) e un intero che rappresenta il numero di elementi da calcolare di ogni sequenza di numeri poligonali.

(define (polygonal-number p n) (- (/ (* p n (- n 1)) 2) (* n (- n 2))))

Funzione che effettua l'intersezione di più liste (set):

(define (intersects)
  (let (base (args 0))
    (for (k 1 (- (length (args)) 1))
      (setq base (intersect base (args k))))))

(define (overlay lst n)
  (let ( (len (length lst)) (all '()) )
    (dolist (p lst)
      (setq nums (map (curry polygonal-number p) (sequence 1 n)))
      ;(println nums)
      (push nums all -1)
    )
    (apply intersects all)))

Proviamo:

Numeri triangolari e quadrati nei primi 1000 termini:
(overlay '(3 4) 1000)
;-> (1 36 1225 41616)

Numeri triangolari e quadrati e esagonali nei primi 1000 termini:
(overlay '(3 4 6) 1000)
;-> (1 1225)

Numeri triangolari e esagonali e ottagonali nei primi 1000 termini:
(overlay '(3 6 8) 1000)
;-> (1 11781)

Numeri triangolari pentagonali e esagonali nei primi 1000 termini:
(overlay '(5 6) 1000)
;-> (1 40755)

Numeri triangolari pentagonali e esagonali nei primi 1e5 (centomila) termini:
(overlay '(3 5) 1e5)
;-> (1 210 40755 7906276 1533776805)


----------------
Giorni fortunati
----------------

Un giorno viene chiamato "fortunato" se i numeri della data (anno, mese, giorno) sono composti da cifre tutte diverse.
Per esempio il 23 aprile 1965 (1965 4 23) è stato un giorno fortunato.

(define (int-list num)
"Convert an integer to a list of digits"
  (let (out '())
    (while (!= num 0)
      (push (% num 10) out)
      (setq num (/ num 10))) out))

Funzione che verifica se una data è valida:

(define (verify-date anno mese giorno)
  (let ((giorni-mese '(0 31 28 31 30 31 30 31 31 30 31 30 31))
        (bisestile (and (= 0 (mod anno 4)) (or (!= 0 (mod anno 100)) (= 0 (mod anno 400))))))
    (if bisestile (setf (giorni-mese 2) 29))
    (not (or (< mese 1) (> mese 12) (< giorno 1) (> giorno (giorni-mese mese))))))

Funzione che verifica se un giorno (data) è fortunato:

(define (lucky-day? anno mese giorno)
  (let (lst (append (int-list anno) (int-list mese) (int-list giorno)))
    (= (unique lst) lst)))

Proviamo:

(lucky-day? 1965 4 23)
;-> true
(lucky-day? 2025 12 31)
;-> nil

Funzione che trova i giorni fortunati in un intervallo di anni:

(define (luckys anno1 anno2)
  (let (out '())
  (for (anno anno1 anno2)
    (for (mese 1 12)
      (for (giorno 1 31)
        (if (and (lucky-day? anno mese giorno) (verify-date anno mese giorno))
            (push (list anno mese giorno) out -1)))))
  out))

Proviamo:

(luckys 1980 1982)
;-> ((1980 2 3) (1980 2 4) (1980 2 5) (1980 2 6) (1980 2 7) (1980 3 2)
;->  (1980 3 4) (1980 3 5) (1980 3 6) (1980 3 7) (1980 3 24) (1980 3 25)
;->  ...
;->  (1982 6 30) (1982 7 3) (1982 7 4) (1982 7 5) (1982 7 6) (1982 7 30))

(luckys 1990 2000)
;-> ()


---------------------------------------------
Numeri primi nelle cifre di e (numeri google)
---------------------------------------------

L'n-esimo numero di Google è il primo numero primo di n cifre trovato nello sviluppo decimale di e.

Sono chiamati numeri di Google a causa dell'insolito annuncio di assunzione pubblicato da Google nel 2004 nella stazione di Harvard Square Red Line [First 10-digit prime found in consecutive digits of e].
https://web.archive.org/web/20041204231318/http://www.boston.com/news/local/articles/2004/09/09/comprehension_test/
Vedi immagine "google-banner.jpg" nella cartella "data".

Sequenza OEIS A095935:
First prime of length n encountered in the decimal representation of e.
  2, 71, 271, 4523, 74713, 904523, 2718281, 72407663, 360287471, 7427466391,
  75724709369, 749669676277, 8284590452353, 99959574966967, 724709369995957,
  2470936999595749, 28459045235360287, 571382178525166427, ...

Per calcolare le cifre decimali di "e" usiamo il metodo spigot.

Vedi "Calcolo di e con il metodo spigot" su "Funzioni varie" .

(define (spigot-e n)
  (local (vec cifra out)
    (setq out '(2))
    ; vettore con n elementi tutti di valore 1
    (setq vec (array n '(1)))
    (for (i 0 (- n 1))
      (setq cifra 0)
      (for (j (- n 1) 0 -1)
        (setf (vec j) (+ (* 10 (vec j)) cifra))
        (setq cifra (/ (vec j) (+ j 2)))
        (setf (vec j) (% (vec j) (+ j 2)))
      )
      (push cifra out -1))
    out))

(spigot-e 1000)
;-> (2 7 1 8 2 8 1 8 2 8 4 5 9 0 ... 5 7 0 3 5 0 3 5 4)

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

Funzione che trova i numeri google fino ad un dato limite:

(define (google cifre limite)
  (local (out found k num)
    (setq out '())
    (setq cifre (join (map string (slice (spigot-e (+ cifre 100)) 0 cifre))))
    (for (n 1 limite 1 stop)
      (setq found nil)
      (setq k 0)
      (until found
        (setq num (int (slice cifre k n) 0 10))
        (when (and (= n (length num)) (prime? num))
          ;(println num)
          (push num out -1)
          (setq found true))
        (++ k)))
    out))

(time (println (google 1000 10)))
;-> (2 71 271 4523 74713 904523 2718281 72407663 360287471 7427466391)
;-> 349.873

(time (println (google 2000 18)))
;-> (2 71 271 4523 74713 904523 2718281 72407663 360287471 7427466391
;->  75724709369 749669676277 8284590452353 99959574966967 724709369995957
;->  2470936999595749 28459045235360287 571382178525166427)
;-> 14488.73

Nota: MAX-INT ha 19 cifre.


------------------------------------
Numeri primi nelle cifre di pi greco
------------------------------------

Scrivere una funzione che cerca il primo numero primo di n cifre nello sviluppo decimale di pi greco.

Sequenza OEIS A104841:
The first n-digit prime occurring in the decimal expansion of Pi, A000796.
  3, 31, 653, 4159, 14159, 314159, 1592653, 28841971, 795028841, 5926535897,
  93238462643, 141592653589, 9265358979323, 23846264338327, 841971693993751,
  8628034825342117, 89793238462643383, 348253421170679821,
  3832795028841971693, 89793238462643383279, ...

Vedi "Calcolo di pi greco con il metodo spigot" su "Note libere 24".

(define (spigot-pi digits)
  (local (d q r t i u y out)
    (setq out '())
    (setq d 0)
    (set 'q 1L 'r 180L 't 60L 'i 2L)
    (while (< d digits)
      (setq u (+ (* 27L i (+ i 1L)) 6L))
      (setq y (/ (+ (* q (- (* 27L i) 12L)) (* 5L r)) (* 5L t)))
      (push (+ 0 y) out -1)
      (setq r (* (* 10L u) (- (+ (* q (- (* 5L i) 2L)) r) (* y t))))
      (setq q (* 10L q i (- (* 2L i) 1L)))
      (setq t (* t u))
      (setq i (+ i 1))
      ;(print q { } r { } t { } i) (read-line)
      (++ d))
    out))

(spigot-pi 1000)
;-> (3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4 3 3 8 3 2 7 9 5
;->  ...
;->  0 6 6 1 3 0 0 1 9 2 7 8 7 6 6 1 1 1 9 5 9 0 9 2 1 6 4 2 0 1 9 8)

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

Funzione che cerca il primo numero primo di n cifre nello sviluppo decimale di pi greco fino a un dato limite:

(define (primi-pi cifre limite)
  (local (out found k num)
    (setq out '())
    (setq cifre (join (map string (slice (spigot-pi (+ cifre 100)) 0 cifre))))
    (for (n 1 limite 1 stop)
      (setq found nil)
      (setq k 0)
      (until found
        (setq num (int (slice cifre k n) 0 10))
        (when (and (= n (length num)) (prime? num))
          ;(println num)
          (push num out -1)
          (setq found true))
        (++ k)))
    out))

Proviamo:

(time (println (primi-pi 1000 10)))
;-> (3 31 653 4159 14159 314159 1592653 28841971 795028841 5926535897)
;-> 39.71

(time (println (primi-pi 2000 18)))
;-> (3 31 653 4159 14159 314159 1592653 28841971 795028841 5926535897
;->  93238462643 141592653589 9265358979323 23846264338327 841971693993751
;->  8628034825342117 89793238462643383 348253421170679821)
;-> 17334.912

Nota: MAX-INT ha 19 cifre.


-------------------------------------------
Numeri primi nella concatenazione dei primi
-------------------------------------------

Scrivere una funzione che cerca il primo numero primo di n cifre nella concatenazione dei numeri primi (235711131719232931374143475359...).

Sequenza OEIS A073062:
Take A000040 (prime numbers), omit commas: 235711131719232931374143,... select first n-digit prime.
  2, 23, 571, 2357, 11131, 711131, 1923293, 35711131, 711131719, 4753596167,
  23571113171, 379838997101, 7535961677173, 29313741434753, 571113171923293,
  7414347535961677, 57111317192329313, 167717379838997101

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (primes num-primes)
"Generates a given number of prime numbers (starting from 2)"
  (let ( (k 3) (tot 1) (out '(2)) )
    (until (= tot num-primes)
      (when (= 1 (length (factor k)))
        (push k out -1)
        (++ tot))
      (++ k 2))
    out))

Funzione che cerca il primo numero primo di n cifre nella concatenazione dei primi:

(define (find-prime num-primi limite)
  (local (out cifre found k num)
    (setq out '())
    (setq cifre (join (map string (primes num-primi))))
    (for (n 1 limite 1 stop)
      (setq found nil)
      (setq k 0)
      (until found
        (setq num (int (slice cifre k n) 0 10))
        (when (and (= n (length num)) (prime? num))
          ;(print num) (read-line)
          (push num out -1)
          (setq found true))
        (++ k)))
    out))

Proviamo:

(time (println (find-prime 100 10)))
;-> (2 23 571 2357 11131 711131 1923293 35711131 711131719 4753596167)
;-> 9.56

(time (println (find-prime 100 18)))
;-> (2 23 571 2357 11131 711131 1923293 35711131 711131719 4753596167
;->  23571113171 379838997101 7535961677173 29313741434753 571113171923293
;->  7414347535961677 57111317192329313 167717379838997101)

Nota: MAX-INT ha 19 cifre.


-------------------------
Numeri somma di due primi
-------------------------

Dobbiamo stabilire quali numeri interi positivi possono essere espressi come somma di due numeri primi (e quelli che non possono essere espressi come somma di due numeri primi).

Sequenza OEIS A014091:
Numbers that are the sum of 2 primes.
  4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 18, 19, 20, 21, 22, 24, 25, 26,
  28, 30, 31, 32, 33, 34, 36, 38, 39, 40, 42, 43, 44, 45, 46, 48, 49, 50,
  52, 54, 55, 56, 58, 60, 61, 62, 63, 64, 66, 68, 69, 70, 72, 73, 74, 75,
  76, 78, 80, 81, 82, 84, 85, 86, 88, 90, 91, 92, 94, 96, 98, ...

Sequenza OEIS A014092:
Numbers that are not the sum of 2 primes.
  1, 2, 3, 11, 17, 23, 27, 29, 35, 37, 41, 47, 51, 53, 57, 59, 65, 67, 71,
  77, 79, 83, 87, 89, 93, 95, 97, 101, 107, 113, 117, 119, 121, 123, 125,
  127, 131, 135, 137, 143, 145, 147, 149, 155, 157, 161, 163, 167, 171, 173,
  177, 179, 185, 187, 189, 191, 197, 203, 205, 207, 209, ...

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

Funzione che prende un intero n e calcola la coppia di numeri primi che sommano a n:
(se i numeri non esistono ritorna '())

(define (sum-of-primes n)
  (if (zero? n) '()
  ;else
  (let ( (out '()) (stop nil) (primi (primes-to n)) )
    (setq stop nil)
    ; Ciclo per ogni numero primo...
    (dolist (p primi stop)
      ;(print p { } (- n p)) (read-line)
      ; se troviamo (- n p) sui primi, allora è la soluzione
      (when (member (- n p) primi)
        (setq out (list p (- n p)))
        ; esce dal ciclo
        (setq stop true))
      ; se (p > n), allora usciamo dal ciclo  
      (if (> p n) (setq stop true))
    )
    out)))

Proviamo:

(sum-of-primes 12)
;-> (7 5)
(sum-of-primes 23)
;-> ()

Per calcolare le due sequenze possiamo modificare la funzione precedente oppure usare il metodo seguente:

1) calcolare le coppie
(setq coppie (map sum-of-primes (sequence 0 20)))
;-> (() () () () (2 2) (2 3) (3 3) (2 5) (3 5) (2 7) (3 7)
;->  () (5 7) (2 11) (3 11) (2 13) (3 13) () (5 13) (2 17) (3 17))

2) calcolare la somma di ogni coppia
(setq somme (map (fn(x) (apply + x)) somme))
;-> (0 0 0 0 4 5 6 7 8 9 10 0 12 13 14 15 16 0 18 19 20)

3) trovare i valori diversi da 0
Numeri che sono somma di due primi
(setq num-ok (clean zero? somme))
;-> (4 5 6 7 8 9 10 12 13 14 15 16 18 19 20)

4) trovare gli indici che hanno 0 come valore associato
Numeri che non sono somma di due primi
(setq num-no (flat (ref-all 0 somme)))
;-> (0 1 2 3 11 17)


---------------------------------------------------------------------
Divisione tra interi con precisione illimitata (algoritmo elementare)
---------------------------------------------------------------------

L'algoritmo per la divisione di due numeri interi viene insegnato alle scuole elementari.
Scriviamo una funzione che implementa questo algoritmo.

Esempio:

(div 34278 24)
;-> 1428.25

  34278 : 24
  34/24 = 1 <---
  34 - 1*24 = 10
  -----------------
  10278
  102/24 = 4 <---
  102 - (4*24) = 6
  -----------------
  678
  67/24 = 2 <---
  67 - (2*24) = 19
  -----------------
  198
  198/24 = 8 <---
  198 - (8*24) = 6
  -----------------
  . <---
  60
  60/24 = 2 <---
  60 - (2*24) = 12
  -----------------
  120
  120/24 = 5 <---
  120 - (5*24) = 0
  -----------------
  1428.25

(define (int-list num)
"Convert an integer to a list of digits"
  (let (out '())
    (while (!= num 0)
      (push (% num 10) out)
      (setq num (/ num 10))) out))

(define (list-int lst)
"Convert a list of digits to integer"
  (let (num 0)
    (dolist (el lst) (setq num (+ el (* num 10))))))

Funzione che trova il dividendo minore tra num1 e num2:

(define (trova-dividendo num1 num2)
  (cond ((<= num1 num2) num1)
    (true
      (let ( (dividendo 0) (k 0) (L1 (int-list num1)))
        (while (< dividendo num2)
          (setq dividendo (+ (L1 k) (* dividendo 10)))
          ;(println k { } dividendo)
          (++ k))
        dividendo))))

Proviamo:

(trova-dividendo 34278 24)
;-> 34

(trova-dividendo 34278 45)
;-> 342

(trova-dividendo 34278 400)
;-> 3427

Funzione che implementa l'algoritmo della divisione:
Parametri:
  num1 = dividendo
  num2 = divisore
  decimali = numero massimo di decimali dopo la virgola (precisione)

(define (divisione num1 num2 decimali)
  (local (out resto virgola dividendo cifra)
    (setq out '())
    (setq resto nil)
    (setq virgola nil)
    (setq decimali (or decimali 16))
    ; Ciclo fino a che non abbiamo resto 0...
    (until (zero? resto)
      ; Calcolo del dividendo corrente
      (setq dividendo (trova-dividendo num1 num2))
      ; Calcola la cifra corrente del risultato
      (setq cifra (/ dividendo num2))
      (push cifra out -1)
      ; Calcolo del resto
      (setq resto (- dividendo (* cifra num2)))
      ; Calcolo nuovo numero
      (cond
        ; Se resto = 0, 
        ; allora fine della divisione (se abbiamo messo la virgola)
        ((and (zero? resto) virgola) nil)
        (true
          ; Creazione nuovo numero
          (setq num1 (int (string resto
                                (slice (string num1) (length dividendo))) 0 10))
          ; Se il nuovo numero < divisore, allora mettiamo la virgola e
          ; aggiungiamo uno zero al nuovo numero
          (when (< num1 num2)
            (setq num1 (* num1 10))
            ; mettiamo la virgola se non l'abbiamo già messa prima
            (if (not virgola) (begin
                (setq virgola true) (push "." out -1)))))
      )
      ;(print (join (map string out))) (read-line)
      ; Se abbiamo calcolato il numero di decimali predefinito,
      ; allora fermiamo la divisione (ponendo resto = 0)
      (when virgola
        (if (> (length (slice out (find "." out))) decimali) (setq resto 0))
      )
    )
    (join (map string out))))

Proviamo:

(divisione 34278 24)
;-> "1428.25"
(divisione 13 7)
;-> "1.8571428571428571"
(divisione 13 7 25)
;-> "1.8571428571428571428571428"
(divisione 12400 300)
;-> "41.3333333333333333"
(divisione 12400 300 6)
;-> "41.333333"
(divisione 1 3)
;-> "0.3333333333333333"
(divisione 1000405 100000)
;-> "1.00405"
(divisione 0 10)
;-> "0."
(divisione 10 0)
;-> ERR: division by zero in function /
;-> called from user function (divisione 10 0)
(div 123 42358)
;-> 0.002903819821521318
(divisione 123 42358 20)
;-> "0.00290381982152131828"

Nota: per trovare il periodo (eventuale) dei decimali deve risultare che durante l'algoritmo un resto sia uguale ad uno dei precedenti (dopo che sia stata messa la virgola).

Scriviamo una funzione che spiega tutti i passaggi dell'algoritmo:

(define (elementare num1 num2 decimali)
  (local (out resto virgola dividendo cifra)
    (setq out '())
    (setq resto nil)
    (setq virgola nil)
    (setq decimali (or decimali 16))
    ; Ciclo fino a che non abbiamo resto 0...
    (println num1 " : " num2 " = ")
    (until (zero? resto)
      ; Calcolo del dividendo corrente
      (setq dividendo (trova-dividendo num1 num2))
      ; Calcola la cifra corrente del risultato
      (setq cifra (/ dividendo num2))
      (push cifra out -1)
      (println "Prendiamo il numero " dividendo ".")
      (println "Il " num2 " sta " cifra " volte nel " dividendo ".")
      (println "Scriviamo " cifra " nel risultato.")
      (println "Risultato: "(join (map string out)))
      (read-line)
      ; Calcolo del resto
      (setq resto (- dividendo (* cifra num2)))
      (println "Calcoliamo il resto: " dividendo " - (" cifra "*" num2 ") = " resto)
      ; Calcolo nuovo numero
      (cond
        ; Se resto = 0, 
        ; allora fine della divisione (se abbiamo messo la virgola)
        ((and (zero? resto) virgola) nil)
        (true
          ; Creazione nuovo numero
          (setq num1 (int (string resto
                                (slice (string num1) (length dividendo))) 0 10))
          (println "Calcolo del nuovo numero: " num1)
          ; Se il nuovo numero < divisore, allora mettiamo la virgola e
          ; aggiungiamo uno zero al nuovo numero
          (when (< num1 num2)
            (println "Nuovo numero: " num1 " è minore del divisore")
            (setq num1 (* num1 10))
            (println "aggiungo uno 0 al nuovo numero " num1)
            ; mettiamo la virgola se non l'abbiamo già messa prima
            (if (not virgola) (begin
                (println "Mettiamo la virgola al risultato")
                (setq virgola true) (push "." out -1)))))
      )
      ;(print (join (map string out))) (read-line)
      ; Se abbiamo calcolato il numero di decimali predefinito,
      ; allora fermiamo la divisione (ponendo resto = 0)
      (when virgola
        (if (> (length (slice out (find "." out))) decimali) (setq resto 0))
      )
    )
  (println "Resto uguale a 0 o precisione raggiunta. La divisione è terminata.")
  (println "Risultato: " (join (map string out))) '>))

Proviamo:

(elementare 34278 24)
;-> 34278 : 24 =
;-> Prendiamo il numero 34.
;-> Il 24 sta 1 volte nel 34.
;-> Scriviamo 1 nel risultato.
;-> Risultato: 1
;-> 
;-> Calcoliamo il resto: 34 - (1*24) = 10
;-> Calcolo del nuovo numero: 10278
;-> Prendiamo il numero 102.
;-> Il 24 sta 4 volte nel 102.
;-> Scriviamo 4 nel risultato.
;-> Risultato: 14
;-> 
;-> Calcoliamo il resto: 102 - (4*24) = 6
;-> Calcolo del nuovo numero: 678
;-> Prendiamo il numero 67.
;-> Il 24 sta 2 volte nel 67.
;-> Scriviamo 2 nel risultato.
;-> Risultato: 142
;-> 
;-> Calcoliamo il resto: 67 - (2*24) = 19
;-> Calcolo del nuovo numero: 198
;-> Prendiamo il numero 198.
;-> Il 24 sta 8 volte nel 198.
;-> Scriviamo 8 nel risultato.
;-> Risultato: 1428
;-> 
;-> Calcoliamo il resto: 198 - (8*24) = 6
;-> Calcolo del nuovo numero: 6
;-> Nuovo numero: 6 è minore del divisore
;-> aggiungo uno 0 al nuovo numero 60
;-> Mettiamo la virgola al risultato
;-> Prendiamo il numero 60.
;-> Il 24 sta 2 volte nel 60.
;-> Scriviamo 2 nel risultato.
;-> Risultato: 1428.2
;-> 
;-> Calcoliamo il resto: 60 - (2*24) = 12
;-> Calcolo del nuovo numero: 12
;-> Nuovo numero: 12 è minore del divisore
;-> aggiungo uno 0 al nuovo numero 120
;-> Prendiamo il numero 120.
;-> Il 24 sta 5 volte nel 120.
;-> Scriviamo 5 nel risultato.
;-> Risultato: 1428.25
;-> 
;-> Calcoliamo il resto: 120 - (5*24) = 0
;-> Resto uguale a 0 o precisione raggiunta. La divisione è terminata.
;-> Risultato: 1428.25

Vedi anche "Periodo e antiperiodo della divisione decimale di due interi M/N" su "Note libere 25".


-------------------
Ricerche su vettori
-------------------

newLISP non ha funzioni di ricerca sui vettori come "find", "ref", "ref-all", ecc. per le liste. 

find per vettori:

(define (find-arr)
  (let (param (args))
    (setf (param 1) (array-list (args 1)))
    (apply find param)))

(setq vet (array 7 '(2 4 5 6 3 1 3)))
(find-arr '3 vet)
;-> 4

ref per vettori:

(define (ref-arr)
  (let (param (args))
    (setf (param 1) (array-list (args 1)))
    (apply ref param)))

(ref-arr '3 vet)
;-> (4)

ref-all per vettori:

(define (ref-all-arr)
  (let (param (args))
    (setf (param 1) (array-list (args 1)))
    (apply ref-all param)))

(ref-all-arr '3 vet)
;-> ((4) (6))

Vediamo la velocità delle funzioni:

(silent (setq lista (rand 1e6 10000))
        (setq vettore (array 10000 lista)))

(setq v1 (lista 0))
(setq v2 (lista -1))

(time (find v1 lista) 1e4)
;-> 0
(time (find-arr v1 vettore) 1e4)
;-> 3770.371
(time (find v2 lista) 1e4)
;-> 883.567
(time (find-arr v2 vettore) 1e4)
;-> 4640.935

(time (ref v1 lista) 1e4)
;-> 0
(time (ref-arr v1 vettore) 1e4)
;-> 3791.012
(time (ref v2 lista) 1e4)
;-> 863.914
(time (ref-arr v2 vettore) 1e4)
;-> 4628.432

(time (ref-all v1 lista) 1e4)
;-> 821.617
(time (ref-all-arr v1 vettore) 1e4)
;-> 4635.193
(time (ref-all v2 lista) 1e4)
;-> 865.061
(time (ref-all-arr v2 vettore) 1e4)
;-> 4627.265

Le funzioni sono molto lente.

Vedi anche "find per vettori" su "Note libere 3".
Vedi anche "Funzioni di ricerca per vettori" su "Note libere 15".


------------------
Orologio analogico
------------------

Scriviamo una funzione che mostra l'ora corrente come un orologio analogico.
I quadranti dell'orologio sono i seguenti:

  Quadrante delle ore               Quadrante dei minuti
                                             00
                                       55          05
          12                                 00
      11      01                         55      05
          12                     50          00          10
  10    11  01    02                 50    55  05    10
      10      02                         50      10
09   09   **   03   03        45   45   45   **   15   15   15
      08      04                         40      20
  08    07  05    04                 40    35  25    20
          06                     40          30          20
      07      05                         35      25
          06                                 30
                                       35          25
                                             30

I minuti vengono arrotondati a 5 oppure a 0.

Esempio:
ora: 8:23
output:

          **
      08
  08        25

              25

                25

ora: 1:12
output:

      01
              10
    01    10
      10
  **

ora: 10:15
output:

  10
      10
          **   15   15   15

Funzione che stampa una matrice di caratteri con cornice:

(define (print-matrix matrix)
  (local (row col)
    (setq row (length matrix))
    (setq col (length (first matrix)))
    (println "+" (dup "-" col) "+")
    (for (r 0 (- row 1))
      (print "|")
      (for (c 0 (- col 1))
        (print (matrix r c)))
      (println "|"))
    (println "+" (dup "-" col) "+") '>))

Funzione che arrotonda un intero a 0 o 5:

(define (round5 numero)
  (let ((resto (mod numero 5)))
    (if (< resto 3)
      (- numero resto)
      (+ numero (- 5 resto)))))

(round5 12)
;-> 10
(round5 18)
;-> 20
(round5 10)
;-> 10

Funzione che mostra l'ora corrente come orologio analogico:

(define (clock)
  (local (w posti-ore posti-minuti hh mm char-hh char-mm)
    (setq posti-ore
    '(("01" ((3 19) (3 20) (5 17) (5 18)))
      ("02" ((5 23) (5 24) (6 19) (6 20)))
      ("03" ((7 20) (7 21) (7 25) (7 26)))
      ("04" ((8 19) (8 20) (9 23) (9 24)))
      ("05" ((9 17) (9 18) (11 19) (11 20)))
      ("06" ((10 15) (10 16) (12 15) (12 16)))
      ("07" ((9 13) (9 14) (11 11) (11 12)))
      ("08" ((8 11) (8 12) (9 7) (9 8)))
      ("09" ((7 5) (7 6) (7 10) (7 11)))
      ("10" ((5 7) (5 8) (6 11) (6 12)))
      ("11" ((3 11) (3 12) (5 13) (5 14)))
      ("12" ((2 15) (2 16) (4 15) (4 16)))))
    (setq posti-minuti
    '(("00" ((0 15) (0 16) (2 15) (2 16) (4 15) (4 16)))
      ("05" ((1 21) (1 22) (3 19) (3 20) (5 17) (5 18)))
      ("10" ((4 27) (4 28) (5 23) (5 24) (6 19) (6 20)))
      ("15" ((7 20) (7 21) (7 25) (7 26) (7 30) (7 31)))
      ("20" ((8 19) (8 20) (9 23) (9 24) (10 27) (10 28)))
      ("25" ((9 17) (9 18) (11 19) (11 20) (13 21) (13 22)))
      ("30" ((10 15) (10 16) (12 15) (12 16) (14 15) (14 16)))
      ("35" ((9 13) (9 14) (11 11) (11 12) (13 9) (13 10)))
      ("40" ((8 11) (8 12) (9 7) (9 8) (10 3) (10 4)))
      ("45" ((7 0) (7 1) (7 5) (7 6) (7 10) (7 11)))
      ("50" ((4 3) (4 4) (5 7) (5 8) (6 11) (6 12)))
      ("55" ((1 9) (1 10) (3 11) (3 12) (5 13) (5 14)))))
      ; matrice di caratteri
      (setq w (array-list (array 15 32 '(" "))))
      (setq (w 7 15) "*")
      (setq (w 7 16) "*")
      ;; clear screen (ANSI sequence)
      (print "\027[H\027[2J")
      ; calcolo ora
      (setq hh (+ ((now) 3) 1))
      (if (> hh 12) (-- hh 12))
      (setq hh (format "%02d" hh))
      ; calcolo minuti
      (setq mm ((now) 4))
      (setq mm (round5 mm))
      (setq mm (format "%02d" mm))
      ; indici delle ore
      (setq char-hh (lookup hh posti-ore))
      ; indici dei minuti
      (setq char-mm (lookup mm posti-minuti))
      ; aggiornamento matrice con indici delle ore
      (dolist (ch char-hh)
        (if (even? $idx)
            (setf (w (ch 0) (ch 1)) (hh 0))
            (setf (w (ch 0) (ch 1)) (hh 1))))
      ; aggiornamento matrice con indici dei minuti
      (dolist (ch char-mm)
        (if (even? $idx)
            (setf (w (ch 0) (ch 1)) (mm 0))
            (setf (w (ch 0) (ch 1)) (mm 1))))
      ; stampa orologio
      (print-matrix w)))

(clock)
;-> +--------------------------------+
;-> |                                |
;-> |                     05         |
;-> |                                |
;-> |                   05           |
;-> |                                |
;-> |                 05             |
;-> |                                |
;-> |               **   03   03     |
;-> |                                |
;-> |                                |
;-> |                                |
;-> |                                |
;-> |                                |
;-> |                                |
;-> |                                |
;-> +--------------------------------+


---------------------
Group-by di una lista
---------------------

Supponiamo di avere una lista con elementi (sottoliste) del tipo: (el0 el1 ... elN).
Vogliamo raggruppare i valori delle sottoliste in base al primo valore di ogni sottolista.

Esempio:
lista = ((a 1 2) (b 6 4 5) (b 1 2) (c 3 5) (a 6 8) (d 1))
group-by (primo valore) = ((a 1 2 6 8) (b 6 4 5 1 2) (c 3 5) (d 1)
Abbiamo raggruppato i valori di a, b, c e d che sono i primi valori di ogni sottolista.

Esempio:
lista = (("A" (1 2)) ("B" (4 3)) ("A" (5 5)) ("C" (7 8)) ("C" (4 3) (5 6)))
group-by (primo valore) (("A" (1 2) (5 5)) ("B" (4 3)) ("C" (7 8) (4 3) (5 6)))
Abbiamo raggruppato i valori di "A", "B" e "C".

Funzione che raggruppa i valori delle sottoliste di una lista in base al primo valore di ogni sottolista:
lst = lista con elementi del tipo (el0 el1 ... elN)
assoc-lst = boolean (true --> crea una lista associativa)
sorted = boolean (true --> crea una lista associativa con sottoliste ordinate)

(define (group-by-first lst assoc-lst sorted)
  (local (out idx valori)
    (setq out '())
    ; Ciclo per ogni elemento della lista
    (dolist (el lst)
      (setq idx (find (list (el 0) '*) out match))
      ; Esiste il primo valore dell'elemento corrente nella lista di output?
      (cond (idx
        ; Esiste: calcola i valori da aggiungere
        (setq valori (rest el))
        ;(setf (out idx) (append (out idx) valori)))
        ; Aggiorna il relativo elemento della lista di output
        ; aggiungendo i nuovi valori
        (extend (out idx) valori))
        (true
          ; Non esiste: aggiunge l'elemento corrente alla lista di output
          (push el out -1))))
    ; Crea la lista di associazione?
    (if assoc-lst 
        ; Sottoliste ordinate?
        (if sorted
            (map (fn(x) (list (x 0) (sort (rest x)))) out)
            (map (fn(x) (list (x 0) (rest x))) out))
        out)))

Proviamo:

(setq lst '((a 1 2) (b 6 4 5) (b 1 2) (c 3 5) (a 6 8) (d 1)))
(group-by-first lst)
;-> ((a 1 2 6 8) (b 6 4 5 1 2) (c 3 5) (d 1))

(setq lst '((a 1 2 3) (b 2 1) (b 4 3 5) (a 4 5) (b 6) (c 1) (a 6)))
(group-by-first lst)
;-> ((a 1 2 3 4 5 6) (b 2 1 4 3 5 6) (c 1))
(group-by-first lst true)
;-> ((a (1 2 3 4 5 6)) (b (2 1 4 3 5 6)) (c (1)))
(group-by-first lst true true)
;-> ((a (1 2 3 4 5 6)) (b (1 2 3 4 5 6)) (c (1)))
(lookup 'a (group-by-first lst true))
;-> (1 2 3 4 5 6)

Vedi anche "Raggruppamento di liste di liste (tabelle) - groupby" su "Note libere 25".


-----------------------------------------------------
Numero più grande/piccolo adiacente ad un dato numero
-----------------------------------------------------

Data una lista di numeri interi e un numero intero K trovare il valore più grande e il valore più piccolo della lista adiacenti a K.

Esempio:
Lista = (1 4 3 9 0 3 7 0)
K = 0
Output = 9 (numero più grande adiacente a 0)
         3 (numero più piccolo adiacente a 0)

Funzione che trova i numeri più grande/piccolo adiacenti ad un dato numero:

(define (adjacent lst k)
  (let ( (maxi -1e99) (mini 1e99) )
    ; Crea le coppie con indici 0-1, 1-2, 2-3, ecc.
    (setq pair (map list (chop lst) (rest lst)))
    ; Ciclo per ogni coppia...
    (dolist (el pair)
      ; se uno dei valori della coppia vale K,
      ; allora aggiorna i valori minimo e massimo
      (cond ((= (el 0) k)
              (setq maxi (max maxi (el 1)))
              (setq mini (min mini (el 1))))
            ((= (el 1) k)
              (setq maxi (max maxi (el 0)))
              (setq mini (min mini (el 0))))))
    ; se k non esiste nella lista, allora restituisce nil
    (if (and (= mini 1e99) (= maxi-1e99)) nil
        (list mini maxi))))

Proviamo:

(setq a '(1 4 3 9 0 3 7 0))
(adjacent a 0)
;-> (3 9)
(adjacent a 2)
;-> nil
(adjacent a 1)
;-> (4 4)

(setq b '(9 4 9 0 9 0 9 15 -2))
(adjacent b 0)
;-> (9 9)
(adjacent b 9)
;-> (0 15)

(setq c '(1 1 1 1 1 1))
(adjacent c 1)
;-> (1 1)

(setq d '(-8 3 -4 1 -6 2 -3 5 9))
(adjacent d 1)
;-> (-6 -4)


------------------------
Giustificare una stringa
------------------------

Funzioni per giustificare/allineare una stringa a sinistra, a destra e al centro.

Left justify
------------
(define (left-string str num-chars fill-char)
"Justify a string to the left"
  (letn ( (len-str (length str)) (space (- num-chars len-str)) )
    (setq fill-char (or fill-char " "))
    (append str (dup fill-char (- num-chars len-str)))))

(left-string "newLISP" 15)
;-> "newLISP        "
(left-string "newLISP" 15 "-")
;-> "newLISP--------"

Right justify
-------------
(define (right-string str num-chars fill-char)
"Justify a string to the right"
  (letn ( (len-str (length str)) (space (- num-chars len-str)) )
    (setq fill-char (or fill-char " "))
    (append (dup fill-char (- num-chars len-str)) str)))

(right-string "newLISP" 15)
;-> "        newLISP"
(right-string "newLISP" 15 "+")
;-> "++++++++newLISP"

Center justify
--------------
(define (center-string str num-chars fill-char)
"Justify a string to the center"
  (letn ( (len-str (length str)) (space (- num-chars len-str)) )
    (setq fill-char (or fill-char " "))
    (if (even? space)
      ; centratura pari
      (append (dup fill-char (/ space 2)) str (dup fill-char (/ space 2)))
      ; centratura dispari
      (append (dup fill-char (/ space 2)) str (dup fill-char (+ (/ space 2) 1))))))

(center-string "newLISP" 15)
;-> "    newLISP    "
(center-string "newLISP" 15 ".")
;-> "....newLISP...."


-------------------------------------------------
Moltiplicazione tra interi (algoritmo elementare)
-------------------------------------------------

Scriviamo una funzione che stampa il processo della moltiplicazione tra interi che abbiamo imparato alle elementari.

(define (moltiplicazione a b)
  (local (result len pad sa sb res)
    (if (> b a) (swap a b))
    (setq result (* a b))
    (setq len (length result))
    (setq pad (+ len 2))
    (setq sa (string a))
    (setq sb (string b))
    (println (right-string sa pad) " x")
    (println (right-string sb pad) " =")
    (println (right-string (dup "-" (length result)) pad) "--")
    (for (i (- (length b) 1) 0 -1)
      (setq res (string (* a (int (sb i))) (dup "0" (- (length b) i 1))))
      ;(if (zero? i)
      ;    (println (right-string (string res) pad) " =")
      ;    (println (right-string (string res) pad) " +"))
      (if (zero? i)
          (print (right-string (string res) pad) " =  ") 
          (print (right-string (string res) pad) " +  "))
      (println "(" a " x " (sb i) (dup "0" (- (length b) i 1)) ")")
    )
    (println (right-string (dup "-" (length result)) pad) "--")
    (println (right-string (string result) pad)) '>))

Proviamo:

(moltiplicazione 342 3566)
;->      3566 x
;->       342 =
;->   ---------
;->      7132 +  (3566 x 2)
;->    142640 +  (3566 x 40)
;->   1069800 =  (3566 x 300)
;->   ---------
;->   1219572

(moltiplicazione 123 123)
;->     123 x
;->     123 =
;->   -------
;->     369 +  (123 x 3)
;->    2460 +  (123 x 20)
;->   12300 =  (123 x 100)
;->   -------
;->   15129

(moltiplicazione 1 1)
;->   1 x
;->   1 =
;->   ---
;->   1 =  (1 x 1)
;->   ---
;->   1


--------------------------------------
Eureka N.3 - The Archimedeans' Journal
--------------------------------------

Archimede dice ad Euclide:
in città il numero di fisici elevato al numero di matematici è uguale al numero di matematici elevato al numero di chimici.
Inoltre, il numero di chimici elevato il numero di matematici è uguale al numero di matematici elevato al numero di fisici.
Infine, il numero di chimici elevato il numero di fisici è uguale al numero di fisici elevato al numero di chimici.

Euclide risponde:
Bene, quindi abbiamo lo stesso numero di matematici, fisici e chimici.

Archimede:
No, stranamente ci sono più matematici che chimici

Euclide (dopo averci pensato un pò):
Allora posso dirti quanti sono per ogni categoria.

Sappiamo risolvere il problema?

Equazioni:

  f^m = m^c
  c^m = m^f
  c^f = f^c
  m > c

Espressione: c^f = f^c

(for (c 1 100)
  (for (f 1 100)
    (if (and (!= c f) (= (pow c f) (pow f c))) (println c { } f))))
;-> 2 4
;-> 4 2

  Caso 1:           Caso 2:           Caso 3: 
  c = 2             c = 4             c = f, con c,f > 0
  f = 4             f = 2             c^m = m^c
  4^m = m^2         2^m = m^4         
  2^m = m^4         4^m = m^2
  m > 2             m > 4

Caso 1:
(for (m 1 100) 
  (if (and (= (pow 4 m) (pow m 2))
           (= (pow 2 m) (pow m 4)))
  (println m)))
;-> nil

Caso 2:
(for (m 1 100) 
  (if (and (= (pow 2 m) (pow m 4))
           (= (pow 4 m) (pow m 2)))
  (println m)))
;-> nil

Caso 3:
(for (c 1 100)
  (for (m 1 100)
    (if (and (!= c m) (= (pow c m) (pow m c))) (println c { } m))))
;-> 2 4
;-> 4 2
Poichè m > c, c = 2 e m = 4.

Soluzione
  c = 2
  f = 2
  m = 4


-------------
Numeri ibridi
-------------

Un numero intero della forma p^q * q^p con numeri primi p != q è detto "intero ibrido" (hybrid-integer).
Ad esempio, 800 = 2^5 5^2 è un numero intero ibrido.

Sequenza OEIS A082949:
Numbers of the form p^q * q^p, with distinct primes p and q.
  72, 800, 6272, 30375, 247808, 750141, 1384448, 37879808, 189267968,
  235782657, 1313046875, 3502727631, 4437573632, 451508436992, 634465620819,
  2063731785728, 7863818359375, 7971951402153, 188153927303168,
  453238525390625, 1145440056788109

(define (primes num-primes)
"Generates a given number of prime numbers (starting from 2)"
  (let ( (k 3) (tot 1) (out '(2)) )
    (until (= tot num-primes)
      (when (= 1 (length (factor k)))
        (push k out -1)
        (++ tot))
      (++ k 2))
    out))

(define (** num power)
"Calculates the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

Funzione che calcola i numeri ibridi:

(define (hybrid num-primes)
  (let ( (out '())
         (lst (array-list (array num-primes (primes num-primes)))) )
    (for (i 0 (- (length lst) 2))
      (for (j (+ i 1) (- (length lst) 1))
        (setq a (lst i)) (setq b (lst j))
        ;(push (list (* (** a b) (** b a)) a b) out -1)))
        (push (* (** a b) (** b a)) out -1)))
    (sort out)))

Proviamo:

(slice (hybrid 100) 0 21)
;-> (72L 800L 6272L 30375L 247808L 750141L 1384448L 37879808L 189267968L
;->  235782657L 1313046875L 3502727631L 4437573632L 451508436992L 634465620819L
;->  2063731785728L 7863818359375L 7971951402153L 188153927303168L
;->  453238525390625L 1145440056788109L)


---------------
La funzione pow
---------------

*****************
>>> funzione POW
*****************
sintassi: (pow num-1 num-2 [num-3 ... ])
sintassi: (pow num-1)

Calcola num-1 alla potenza di num-2 e così via.

(pow 100 2)
;-> 10000
(pow 100 0.5)
;-> 10
(pow 100 0.5 3)
;-> 1000
(pow 3)
;-> 9
Quando num-1 è l'unico argomento, "pow" presuppone 2 come esponente.
------------

Ci sono alcuni casi in cui la funzione "pow" non produce il risultato desiderato.

Consideriamo l'espressione 2^7 = 128
(pow 2 7)
;-> 128
Quindi la radice settima di 128 vale 2:
(pow 128 (div 7))
;-> 2

Consideriamo l'espressione (-2)^7 = -128
(pow -2 7)
;-> -128
In questo caso la radice settima di -128 vale -2:
(pow -128 (div 7))
;-> -1.#IND
Invece la funzione "pow" produce un valore indeterminato (NaN).
Questo perchè (pow x y) è progettata per restituire NaN per x negativo e y non intero.

Matematicamente la radice settima di 128 ha sette soluzioni (sei immaginarie e una reale che vale -2).
Vedi immagine "radice-settima-di-meno-128.png" nella cartella "data".

Per ottenere il valore reale occorre calcolare - pow(- base power):
(sub 0 (pow (sub 0 -128) (div 7)))
;-> -2

Facciamo un altro esempio, vogliamo calcolare la radice sesta di -128:
(pow -128 (div 6))
;-> -1.#IND

(sub 0 (pow (sub 0 -128) (div 6)))
;-> -2.244924096618746
(pow -2.244924096618746 6)
;-> 128

Funzione "pow" estesa:

(define (pow-ext x n)
  (if (< x 0)
      (sub 0 (pow (sub 0 x) n))
      (pow x n)))

Proviamo:

(pow-ext 128 (div 7))
;-> 2

(pow-ext -128 (div 7))
;-> -2

(pow-ext -128 (div 6))
;-> -2.244924096618746

(pow-ext -3 (div -3))
;-> -0.6933612743506348
(pow -0.6933612743506348 -3)
;-> -3

Nota: in Mathematica l'espressione (-128)^(1/7) produce la seguente soluzione approssimata:
  1.8019 + 0.8677i
si tratta del valore principale che viene scelto per convenzione in modo che la funzione diventi monovalore.


-----------------------------------------------
La radice quadrata di 2 è un numero irrazionale
-----------------------------------------------

La radice quadrata di 2 sqrt(2) è un numero irrazionale, ossia un numero che non si può esprimere in forma di frazione m/n con m e n numeri interi).
Dimostriamo questa proposizione utilizziamo il metodo per assurdo: si presuppone vera l'affermazione contraria e si
mostra che questa porta ad una contraddizione.

Per comprendere questa dimostrazione occorre tener presente che:
a  a) un qualsiasi numero naturale moltiplicato per 2 produce un numero pari;
a1 b) un numero naturale pari n può essere rappresentato come n = 2*k, con k numero naturale;
b  c) se il quadrato di un numero n è pari anche n è pari;
c  d) una frazione può essere ridotta ai minimi termini con un numero finito di passaggi;
d  e) nella Logica matematica se un proposizione P è vera la sua negazione è falsa e viceversa.

Iniziamo la dimostrazione per assurdo affermando che sqrt(2) può esere espresse come rapporto di due numeri interi:

  sqrt(2) = m/n     (1)

La frazione m/n è ridotta ai minimi termini e i numeri m e n sono coprimi tra loro (quindi non possono essere entrambi pari).
  
Eleviamo al quadrato l'espressione (1):

  2 = m^2/n^2       (2)

Ricaviamo m^2:

  m^2 = 2*n^2       (3)

Per la proprietà a) risulta che 2*n^2 è un numero pari, quindi anche m^2 è pari.
Per la proprietà c) poichè m^2 è un numero pari, anche m è un numero pari.
Poichè m è pari, per la proprietà b) risulta:

  m = 2*k                   (4)
  m^2 = (2*k)^2 = 4*k^2     (5)

Sosituiamo nella (3) il valore di m^2 dato dalla (5):

  4*k^2 = 2*n^2             (6) 
  
Dividiamo per 2 entrambi i membri della (6):

  2*k^2 = n^2

Per la proprietà a) risulta che 2*k^2 è un numero pari, quindi anche n^2 è pari.
Per la proprietà c) poichè n^2 è un numero pari, anche n è un numero pari.

A questo punto risulta che m e n sono entrambi pari, che è una contraddizione (abbiamo stabilito all'inizio che m e n non possono essere entrambi pari), quindi se una proposizione è falsa, allora la sua negazione è vera.

Questa dimostrazione è stata formulata per la prima volta da Euclide.


---------------------------------------------
Conversione gradi <--> radianti (ottimizzate)
---------------------------------------------

Formula per convertire da gradi a radianti:

  gradi = (radianti * 180)/PI

(setq PI 3.1415926535897931)

(define (deg2rad deg)
"Convert decimal degrees to radiants"
  (div (mul deg PI) 180))

Formula per convertire da radianti a gradi:

  radianti = (gradi * PI)/180

(define (rad2deg rad)
"Convert radiants to decimal degrees"
  (div (mul rad 180) PI))

Possiamo ottimizzare le funzioni e renderle più veloci:

(define (deg-rad deg)
"Convert decimal degrees to radiants"
  (mul deg 1.745329251994329577e-2))

(define (rad-deg rad)
"Convert radiants to decimal degrees"
  (mul rad 57.29577951308232088))

Vediamo la velocità delle funzioni:

(time (deg-rad 60.5) 5e6)
;-> 290.111
(time (deg2rad 60.5) 5e6)
;-> 393.166

(time (rad-deg 12.14) 5e6)
;-> 293.166
(time (rad2deg 12.14) 5e6)
;-> 401.92


--------------------
Problema degli asini
--------------------

Un mercante ha 3 asini in vendita. Un contadino ha intenzione di acquistarli, ma prima vorrebbe sapere la loro età.
Il mercante risponde: "hanno tutti più di un anno, il prodotto delle loro età vale 72 e la loro somma è pari al numero di stelle che ci sono adesso nel cielo".
Il contadino ci pensa un pò e poi risponde: "non è possibile rispondere al problema".
Allora il mercante continua: "il più grande ha la criniera bianca".
A questo punto il contadino dichiara di conoscere le età dei 3 asini.
Quanti anni hanno i 3 asini?

Vediamo quali numeri soddisfano i vincoli dati.

(factor 72)
;-> (2 2 2 3 3)

(setq sol '())

(for (i 2 18)
  (for (j 2 18)
    (for (k 2 18)
      ; se la somma vale 72, allora i,j e k sono una possibile soluzione
      (if (= (* i j k) 72) (push (sort (list i j k)) sol)))))
; elimina le soluzioni uguali e calcola la somma di ogni soluzione
(map (fn(x) (list x (apply + x))) (unique sol))
;-> (((2 2 18) 22) ((2 3 12) 17) ((2 4 9) 15)
;->  ((3 3 8) 14) ((2 6 6) 14) ((3 4 6) 13))

Le soluzioni candidate sono 6:
eta        somma 
(2 2 18)   22
(2 3 12)   17
(2 4 9)    15
(3 3 8)    14
(2 6 6)    14
(3 4 6)    13

Se in numero di stelle nel cielo fosse 22 o 17 o 15 o 13, allora il contadino saprebbe rispondere alla domanda (perchè il numero di stelle sarebbe unico).
Quindi nel cielo ci devono essere 14 stelle (motivo per cui il contadino dichiara di non conoscere la risposta).
A questo punto la soluzione può essere (3 3 8) oppure (2 6 6).
Poichè deve esistere un "unico asino più grande" (che risulta 8), allora la soluzione vale (3 3 8).


---------------------
Minori di una matrice
---------------------

Data una matrice A (M x N), chiamiamo "minori" le matrici, M(i j), ottenute eliminando la riga "i" e la colonna "j" della matrice A (con 0 <= i <= (M-1), e 0 <= j <= (N-1)).

(define (minor matrix r c)
"Removes a row and a column from a matrix"
  (pop matrix r)
  (setq matrix (transpose matrix))
  (pop matrix c)
  (transpose matrix))

(define (print-matrix matrix)
"Print a matrix m x n"
  (local (row col lenmax digit fmtstr)
    ; converto matrice in lista?
    (if (array? matrix) (setq matrix  (array-list matrix)))
    ; righe della matrice
    (setq row (length matrix))
    ; colonne della matrice
    (setq col (length (first matrix)))
    ; valore massimo della lunghezza di un elemento (come stringa)
    (setq lenmax (apply max (map length (map string (flat matrix)))))
    ; calcolo spazio per gli elementi
    (setq digit (+ 1 lenmax))
    ; creo stringa di formattazione
    (setq fmtstr (append "%" (string digit) "s"))
    ; stampa la matrice
    (for (i 0 (- row 1))
      (for (j 0 (- col 1))
        (print (format fmtstr (string (matrix i j))))
      )
      (println))))

Proviamo:

(setq m '((1 2 3) (4 5 6) (7 8 9)))
(print-matrix m)
;-> 1 2 3
;-> 4 5 6
;-> 7 8 9
(print-matrix (minor m 0 0))
;-> 5 6
;-> 8 9
(print-matrix (minor m 1 1))
;-> 1 3
;-> 7 9
(print-matrix (minor m 1 2))
;-> 1 2
;-> 7 8

(setq m1 '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (13 14 15)))
(print-matrix m1)
;->  1  2  3
;->  4  5  6
;->  7  8  9
;-> 10 11 12
;-> 13 14 15
(print-matrix (minor m1 0 0))
;->  5  6
;->  8  9
;-> 11 12
;-> 14 15
(print-matrix (minor m1 1 1))
;->  1  3
;->  7  9
;-> 10 12
;-> 13 15
(print-matrix (minor m1 1 2))
;->  1  2
;->  7  8
;-> 10 11
;-> 13 14
(print-matrix (minor m1 4 0))
;->  2  3
;->  5  6
;->  8  9
;-> 11 12


---------------------------------------------
Determinante di una matrice (Laplace e Gauss)
---------------------------------------------

Il determinante di una matrice quadrata nxn è un valore scalare che fornisce informazioni fondamentali sulla matrice.

Proprietà fondamentali
   - Se una matrice ha una riga o una colonna nulla, il determinante è 0.
   - Se due righe (o colonne) sono uguali, il determinante è 0.
   - Scambiando due righe (o colonne), il determinante cambia segno.
   - Moltiplicando una riga per uno scalare k, il determinante viene moltiplicato per k.
   - Se la matrice è triangolare (superiore o inferiore), il determinante è il prodotto degli elementi diagonali.
   - Una matrice quadrata A è invertibile se e solo se det(A) != 0.
   - Il determinante misura il fattore di scala della trasformazione lineare rappresentata dalla matrice.
   - È fondamentale nella soluzione dei sistemi lineari (Regola di Cramer) e nell'algebra multilineare.

Metodi per il calcolo del determinante

1. Matrici 2x2
   Il determinante di una matrice A = ((a b) (c d)) è dato dalla formula:

   det(A) = ad - bc

2. Espansione di Laplace (Sviluppo per righe o colonne)
   Il determinante di una matrice nxn può essere calcolato sviluppando lungo una riga o una colonna mediante i minori algebrici. Per una matrice A di ordine n:

   det(A) = Sum[j=1..n] ((-1)^(i+j))*a(ij)*M(ij)

   dove M(ij) è il determinante della matrice minore ottenuta eliminando la riga i e la colonna j.
   La variabile i rappresenta la riga lungo la quale si sta sviluppando il determinante.

- Se si sviluppa lungo una riga fissata i:
  - i rimane costante e si somma su tutti gli indici di colonna j da 1 a n.
  - In questo caso, la formula diventa:

  det(A) = Sum[j=1..n] ((-1)^(i+j))*a(ij)*M(ij)

- Se si sviluppa lungo una colonna fissata j:
  - j rimane costante e si somma sugli indici di riga i da 1 a n.
  - La formula diventa:

  det(A) = Sum[i=1..n] ((-1)^(i+j))*a(ij)*M(ij)

Quindi, a seconda della scelta di sviluppo, i o j rimane fisso mentre l'altro varia da 1 a n.

Scriviamo una funzione che calcola il determinate di una matrice con l'espansione di Laplace.

(define (minor matrix r c)
"Removes a row and a column from a matrix"
  (pop matrix r)
  (setq matrix (transpose matrix))
  (pop matrix c)
  (transpose matrix))

Funzione che calcola il determinante con il metodo di Laplace:

(define (laplace matrice)
  (let (len (length matrice))
    (if (= len 1)
        (matrice 0 0)
        (let (deter 0)
          (for (j 0 (- len 1))
            (setq deter (add deter (mul (matrice 0 j) (pow -1 j) (laplace (minor matrice 0 j))))))
          deter))))

Proviamo:

(laplace '((1 2) (2 1)))
;-> -3
(det '((1 2) (2 1)))
;-> -3
(laplace '((1 3) (3 1)))
;-> -8
(det '((1 3) (3 1)))
;-> -8
(laplace '((1 2 3) (4 5 6) (7 8 9)))
;-> 0
(det '((1 2 3) (4 5 6) (7 8 9)))
;-> 6.661338147750939e-016
(laplace '((-1 -2 -3) (4 -5 6) (-7 -8 -9)))
;-> 120
(det '((-1 -2 -3) (4 -5 6) (-7 -8 -9)))
;-> 120
(laplace '((-1.1 -2.2 -3.3) (4.4 -5.5 6.6) (-7.7 -8.8 -9.9)))
;-> 159.72
(det '((-1.1 -2.2 -3.3) (4.4 -5.5 6.6) (-7.7 -8.8 -9.9)))
;-> 159.72
(laplace '((-1.1 -2.2 -3.3) (4.4 -5.5 6.6) (-7.7 -8.8 -9.9)))
;-> 159.72
(det '((-1.1 -2.2 -3.3) (4.4 -5.5 6.6) (-7.7 -8.8 -9.9)))
;-> 159.72
(laplace '((0 1 2) (0 3 4) (5 6 7)))
;-> -10
(det '((0 1 2) (0 3 4) (5 6 7)))
;-> -10

3. Metodo di Gauss
   Utilizzando l'eliminazione di Gauss, è possibile ridurre una matrice a forma triangolare superiore, calcolando il determinante come prodotto degli elementi sulla diagonale, tenendo conto delle operazioni elementari eseguite.

Scriviamo una funzione che calcola il determinate di una matrice con il metodo di Gauss.

(define (gauss matrice)
  (let ((n (length matrice)) (deter 1) (tolleranza 1e-10) (trovato false))
    (for (i 0 (sub n 2))
      (let ((pivot (matrice i i)) (pivot-row i))
        (if (< (abs pivot) tolleranza)
            (for (j (add i 1) (sub n 1))
              (if (and (not trovato) (> (abs (matrice j i)) tolleranza))
                  (begin
                    (swap (matrice i) (matrice j))
                    (setq pivot (matrice i i))
                    (setq deter (mul deter -1))
                    (setq pivot-row j)
                    (setq trovato true)))))
        (if (< (abs pivot) tolleranza)
            (setq deter 0)
            (for (j (add i 1) (sub n 1))
              (let ((fattore (div (matrice j i) pivot)))
                (for (k i (sub n 1))
                  (setf (matrice j k) (sub (matrice j k) (mul fattore (matrice i k))))))))))
    (if (!= deter 0)
        (for (i 0 (sub n 1))
          (setq deter (mul deter (matrice i i)))))
    deter))

Proviamo:

(gauss '((1 2) (2 1)))
;-> -3
(det '((1 2) (2 1)))
;-> -3
(gauss '((1 3) (3 1)))
;-> -8
(det '((1 3) (3 1)))
;-> -8
(gauss '((1 2 3) (4 5 6) (7 8 9)))
;-> 0
(det '((1 2 3) (4 5 6) (7 8 9)))
;-> 6.661338147750939e-016
(gauss '((-1 -2 -3) (4 -5 6) (-7 -8 -9)))
;-> 120
(det '((-1 -2 -3) (4 -5 6) (-7 -8 -9)))
;-> 120
(gauss '((-1.1 -2.2 -3.3) (4.4 -5.5 6.6) (-7.7 -8.8 -9.9)))
;-> 159.72
(det '((-1.1 -2.2 -3.3) (4.4 -5.5 6.6) (-7.7 -8.8 -9.9)))
;-> 159.72
(gauss '((0 1 2) (0 3 4) (5 6 7)))
;-> -10
(det '((0 1 2) (0 3 4) (5 6 7)))
;-> -10

Vediamo la differenza di velocità tra le funzione "laplace", "gauss" e la funzione integrata "det".

Creiamo una matrice 10x10 con numeri compresi tra 0 a 9:

(setq t '((0 5 1 8 5 4 3 8 8 7) 
          (1 8 7 5 3 0 0 3 1 1)
          (9 4 1 0 0 3 5 5 6 6)
          (1 6 4 3 0 6 7 8 5 3)
          (8 7 9 9 5 1 4 2 8 2)
          (7 8 9 9 6 3 2 2 8 0)
          (3 0 6 0 0 9 2 2 5 6)
          (8 7 4 2 7 4 4 9 7 1)
          (5 3 7 6 5 3 1 2 4 8)
          (5 9 7 3 1 6 4 0 6 5)))

Calcoliamo il determinante con le tre funzioni:

(laplace t)
;-> 139223208

(gauss t)
;-> 139223208

(det t)
;-> 139223208

Vediamo la velocità di esecuzione:

(time (laplace t) 5)
;-> 24372.759
La funzione "laplace" è molto lenta perchè è ricorsiva.

Confrontiamo le altre due funzioni:

(time (gauss t) 10000)
;-> 608.275

(time (det test) 10000)
;-> 14.018

Vedi anche "Determinante di una matrice (Gauss)" su "Note libere 10".


-----------------------------------------------------------
Numeri (e parole) lessicograficamente crescenti/decrescenti
-----------------------------------------------------------

Un numero lessicograficamente crescente/decrescente è un intero le cui cifre sono in ordine strettamente crescente/decrescente.
Una parola (stringa) lessicograficamente crescente/decrescente è una stringa in cui i caratteri sono in ordine strettamente crescente/decrescente (ordine del codice ASCII).

Scriviamo una funzione che verifica se una stringa (parola o numero) è lessicograficamente ordinata in un dato tipo.
Il parametro "tipo" può valere:
  1) <  (strettamente crescente)
  2) >  (strettamente decrescente)
  3) >= (decrescente)
  4) <= (crescente)
  5) =  (costante) (cioè tutti i caratteri sono uguali)

(define (lexicographic? str tipo)
  (if (< (length str) 2) true ; 0 o 1 carattere
  ;else
  (apply tipo (explode str))))

Proviamo:

(lexicographic? "" <)
;-> true
(lexicographic? "1" >)
;-> true
(lexicographic? "123" <)
;-> true
(lexicographic? "123" >)
;-> nil
(lexicographic? "122" <=)
;-> true
(lexicographic? "111" =)
;-> true
(lexicographic? "111" >=)
;-> true
(lexicographic? "111" <=)
;-> true
(lexicographic? "111" >)
;-> nil
(lexicographic? "111" <)
;-> nil
(lexicographic? "01a" <)
;-> true
(lexicographic? "abc" <)
;-> true
(lexicographic? "abb" <)
;-> nil
(lexicographic? "Aa" <)
;-> true
(lexicographic? "aA" <)
;-> nil

Scriviamo una funzione che calcola tutti i numeri lessicograficamente crescenti in un intervallo [a, b].

(define (numeri-lexi a b)
  (filter (fn(x) (lexicographic? x <)) (map string (sequence a b))))

(numeri-lexi 1 100)
;-> ("1" "2" "3" "4" "5" "6" "7" "8" "9" "12" "13" "14" "15" "16" "17"
;->  "18" "19" "23" "24" "25" "26" "27" "28" "29" "34" "35" "36" "37" 
;->  "38" "39" "45" "46" "47" "48" "49" "56" "57" "58" "59" "67" "68" 
;->  "69" "78" "79" "89")


----------------------------
Generatore di cartelle BINGO
----------------------------

Le cartelle del BINGO (Italia) hanno 3 righe e 9 colonne.
Un totale di 15 numeri (casuali da 1 a 90 e tutti diversi) vengono inseriti nella cartella in modo che esistano 5 numeri per ogni riga ed almeno un numero per ogni colonna.
Nella colonna 1 devono essere inseriti solo i numeri che vanno da  1 a 10.
Nella colonna 2 devono essere inseriti solo i numeri che vanno da 11 a 20.
Nella colonna 3 devono essere inseriti solo i numeri che vanno da 21 a 30.
Nella colonna 4 devono essere inseriti solo i numeri che vanno da 31 a 40.
Nella colonna 5 devono essere inseriti solo i numeri che vanno da 41 a 50.
Nella colonna 6 devono essere inseriti solo i numeri che vanno da 51 a 60.
Nella colonna 7 devono essere inseriti solo i numeri che vanno da 61 a 70.
Nella colonna 8 devono essere inseriti solo i numeri che vanno da 71 a 80.
Nella colonna 9 devono essere inseriti solo i numeri che vanno da 81 a 90.
I 15 numeri sono ordinati in modo crescente in ogni riga e in ogni colonna.

Esempio di cartella:

  +----+----+----+----+----+----+----+----+----+
  |  5 |    |    |    | 49 |    | 63 | 75 | 80 |
  +----+----+----+----+----+----+----+----+----+
  |    |    | 28 | 34 |    | 52 | 66 | 77 |    |
  +----+----+----+----+----+----+----+----+----+
  |  6 | 11 |    |    |    | 59 | 69 |    | 82 |
  +----+----+----+----+----+----+----+----+----+

Scriviamo una funzione che genera una cartella del BINGO.

Algoritmo
1) Inserimento di 9 numeri nelle colonne con riga casuale.
2) Inserimento di 6 numeri in righe e colonne casuali
3) Ordinamento dei numeri per ogni colonna

(define (rand-range min-val max-val)
"Generate a random integer in a closed range"
  (if (> min-val max-val) (swap min-val max-val))
  (+ min-val (rand (+ (- max-val min-val) 1))))

(apply rand-range '(1 10))
;-> 5

Funzione che stampa una cartella del BINGO:

(define (print-bingo lst)
  (println "+----+----+----+----+----+----+----+----+----+")
  (for (r 0 2)
    (for (c 0 8)
      (if (= (lst r c) nil)
          (print "|    ")
          (print "|" (format "%4d" (out r c))))
    )
    (println "|")
    (println "+----+----+----+----+----+----+----+----+----+")))

Funzione che ordina le colonne di una cartella del BINGO:

(define (sort-bingo lst)
  (local (out valori idx-valori tmp)
    (setq out '())
    ; Traspone righe e colonne
    (setq lst (transpose lst))
    ; Ciclo per ogni riga
    (dolist (riga lst)
      ; elimina i nil e ordina la riga
      (setq valori (sort (clean nil? riga)))
      (setq idx-valori 0)
      (setq tmp '())
      ; Crea la nuova riga
      (dolist (el riga)
        (cond ((nil? el) (push nil tmp -1)) ; nil rimane al proprio posto
              (true
                (push (valori idx-valori) tmp -1) ; inserimento valore corrente
                (++ idx-valori))))
      ; Inserisce la nuova riga nella soluzione
      (push tmp out -1))
    ; Traspone righe e colonne
    (transpose out)))

Funzione che genera una cartella del BINGO:

(define (bingo)
  (setq out (array-list (array 3 9 '())))
  (setq range '((1 10) (11 20) (21 30) (31 40) (41 50)
                 (51 60) (61 70) (71 80) (81 90)))
  ; Inserisce un numero per ogni colonna con riga random (9 numeri)
  (for (col 0 8)
    (setf (out (rand 3) col) (apply rand-range (range col)))
  )
  ; Inserisce i 6 numeri che mancano
  (for (k 0 5)
    (setq inserito nil)
    (until inserito
      (setq riga (rand 3))    ; riga random
      (setq colonna (rand 9)) ; colonna random
      ; Genera un numero in base al valore di "colonna"
      (setq valore (apply rand-range (range colonna)))
      ; Se il numero non compare in "out" e la casella non è occupata...
      (when (and (not (find valore (flat out)))
                 (= (out riga colonna) nil))
            ; ... allora inserisce il numero in "out"
            (setf (out riga colonna) valore)
            (setq inserito true)
      )
    )
  )
  ; Adesso la cartella non ha le colonne ordinate
  (print-bingo out)
  (println)
  ; Ordinamento dei numeri per ogni colonna
  (setq out (sort-bingo out))
  (print-bingo out)
  out)

Proviamo:

(bingo)
;-> +----+----+----+----+----+----+----+----+----+
;-> |   5|  14|  30|  32|    |  53|  64|  76|  83|
;-> +----+----+----+----+----+----+----+----+----+
;-> |    |    |    |    |  42|    |    |  71|  82|
;-> +----+----+----+----+----+----+----+----+----+
;-> |   1|    |  23|    |  48|    |    |    |  89|
;-> +----+----+----+----+----+----+----+----+----+
;-> 
;-> +----+----+----+----+----+----+----+----+----+
;-> |   1|  14|  23|  32|    |  53|  64|  71|  82|
;-> +----+----+----+----+----+----+----+----+----+
;-> |    |    |    |    |  42|    |    |  76|  83|
;-> +----+----+----+----+----+----+----+----+----+
;-> |   5|    |  30|    |  48|    |    |    |  89|
;-> +----+----+----+----+----+----+----+----+----+
;-> ((1 14 23 32 nil 53 64 71 82) (nil nil nil nil 42 nil nil 76 83) (5 nil 30 nil 48
;->   nil nil nil 89))

============================================================================

