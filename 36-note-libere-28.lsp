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

Nota: se il file di log esiste, le attività di logging vengono registrate alla fine del file.

Vedi il file "_npp-newlisp-log.ahk" nella cartella "data".


----------------------------------
Numeri nella forma x^2 + y^2 + z^2
----------------------------------

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

(= (filter xyz1? (sequence 0 100)) (filter xyz2? (sequence 0 100)))
;-> true

============================================================================

