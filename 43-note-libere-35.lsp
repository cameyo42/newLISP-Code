================

 NOTE LIBERE 35

================

  "Prima di parlare, lascia che le tue parole passino attraverso tre porte:
   È vero? È necessario? È gentile?".

---------------------------------------------------
Massima differenza tra elementi successivi ordinati
---------------------------------------------------

Data una lista di interi, restituire la differenza massima tra due elementi successivi nella sua forma ordinata.
Se la lista contiene meno di due elementi, restituire 0.
Possiamo scrivere un algoritmo che viene eseguito in tempo lineare e utilizza uno spazio extra lineare?

Scriviamo prima una funzione con le primitive di newLISP:

(define (max-diff1 lst)
  (sort lst)
  (apply max (map - (rest lst) (chop lst))))

Proviamo:

(max-diff1 '(3 6 9 1))
;-> 3
(max-diff1 '(9 8 7 6 11 1 2 3 4 5))
;-> 2

Questa funzione non viene eseguita in tempo lineare perchè attraversa la lista 3 volte:
1) (sort lst) --> O(n*log(n))
2) (map ...)  --> O(n)
3) (apply max ...) --> O(n)

Per implementare un algoritmo lineare possiamo utilizzare i principi di bucket sort.
Algoritmo:
1) Trovare i valori minimo e massimo nella lista.
2) Creare bucket con dimensione del bucket calcolata in base all'intervallo (max - min) / (n - 1).
3) Distribuire gli elementi nei bucket e tracciare i valori minimo e massimo in ciascun bucket.
4) Il gap massimo deve essere compreso tra l'elemento massimo di un bucket e l'elemento minimo del bucket successivo non vuoto (non all'interno dello stesso bucket a causa del principio del pigeonhole).
5) Iterare i bucket per trovare il gap massimo tra bucket successivi non vuoti.
Questo metodo garantisce che il gap massimo venga trovato in modo efficiente senza ordinare esplicitamente l'intero lista.

(define (max-diff2 lst)
  (local (MAX-INT MIN-INT len min-val max-val bucket-dim bucket-len
          buckets bucket-idx prev-max max-diff)
  ; Inizializza le costanti
  (setq MAX-INT 9223372036854775807)
  (setq MIN-INT -9223372036854775808)
  ; numero di elementi nella lista
  (setq len (length lst))
  ; Trova i valori minimo e massimo della lista
  (setq min-val MAX-INT)
  (setq max-val MIN-INT)
  (setq min-val (apply min lst))
  (setq max-val (apply max lst))
  ; Calcola la dimensione del bucket usando il principio pigeonhole
  ; La distanza minima possibile del gap massimo vale:
  ; (max-val - min-val) / (len - 1)
  (setq bucket-dim (max 1 (/ (- max-val min-val) (- len 1))))
  (setq bucket-len (+ 1 (/ (- max-val min-val) bucket-dim)))
  ; Initializza i buckets: ogni bucket ha i valori (min max)
  ; vettore di liste (min max)
  (setq buckets (array bucket-len (list (list MAX-INT MIN-INT))))
  ; Distribuisce i numeri nei buckets in base ai loro valori
  (dolist (num lst)
    (setq bucket-idx (/ (- num min-val) bucket-dim))
    (setf ((buckets bucket-idx) 0) (min ((buckets bucket-idx) 0) num))
    (setf ((buckets bucket-idx) 1) (max ((buckets bucket-idx) 1) num)))
  ; Trova la differenza massima confrontando i bucket adiacenti non vuoti
  (setq prev-max MAX-INT)
  (setq max-diff 0)
  (dolist (bucket buckets)
    ; Processiamo solo i bucket non vuoti: (bucket 0) <= (bucket 1).
    ; I bucket vuoti sono quelli in cui risulta:
    ; (min-val > max-val) dopo l'inizializzazione.
    (when (<= (bucket 0) (bucket 1))
        ; Calcola la differenza il minimo del bucket corrente e
        ; il massimo del bucket precedente
        (setq max-diff (max max-diff (- (bucket 0) prev-max)))
        (setq prev-max (bucket 1))))
  max-diff))

Proviamo:

(max-diff2 '(3 6 9 1))
;-> 3
(max-diff2 '(9 8 7 6 11 1 2 3 4 5))
;-> 2

Test di correttezza:

(for (i 1 1000)
  (setq a (rand 10000 100))
  (if-not (= (max-diff1 a) (max-diff2 a))
    (println a { } (max-diff1 a) { } (max-diff2 a))))
;-> nil

Test di velocità:

(setq b (rand 100000 10000))
(time (max-diff1 b) 1e3)
;-> 2718.869
(time (max-diff2 b) 1e3)
;-> 7281.574

La funzione con le primitive 'max-diff1' è più veloce di quella con l'algoritmo lineare 'max-diff2' (ed è anche più semplice).


--------------------------------------
Ramanujan's constant: e^(PI*sqrt(163))
--------------------------------------

Nel 1975 Martin Gardner ha pubblicato su Scientific American un articolo con la seguente affermazione:

  L'espressione e^(PI*sqrt(163) genera un numero intero.

Con newLISP non possiamo verificarlo:

(setq pi 3.1415926535897931)
(exp (mul pi (sqrt 163)))
;-> 2.625374126407683e+017

Ma possiamo calcolarlo con WolframScript (Wolfram Engine):
(Vedi "Wolfram Engine e WolframScript" su "Note libere 24")

(exec {wolframscript -code "N[Exp[Pi*Sqrt[163]], 35]"})
;-> ("2.6253741264076874399999999999925007259719818568887933424`35.*^17")

Risultato:
  2.6253741264076874399999999999925007259719818568887933424*10^17
Questo numero è uguale a:
  262537412640768743.99999999999925007259719818568887933424
che è 'quasi' un numero intero 262537412640768743.999999999999...
  262537412640768744


------------------------------
Sequenze di interi concatenati
------------------------------

Sequenza OEIS A007908
Triangle of the gods: to get a(n), concatenate the decimal numbers 1,2,3,...,n.
  1, 12, 123, 1234, 12345, 123456, 1234567, 12345678, 123456789,
  12345678910, 1234567891011, 123456789101112, 12345678910111213,
  1234567891011121314, 123456789101112131415, 12345678910111213141516,
  1234567891011121314151617, 123456789101112131415161718, ...

(define (seq1 num)
  (let ( (out '()) (str "") )
    (for (k 1 num)
      (extend str (string k))
      (push (bigint str) out -1))
    out))

(seq1 18)
;-> (1L 12L 123L 1234L 12345L 123456L 1234567L 12345678L 123456789L
;->  12345678910L 1234567891011L 123456789101112L 12345678910111213L
;->  1234567891011121314L 123456789101112131415L 12345678910111213141516L
;->  1234567891011121314151617L 123456789101112131415161718L)

Vediamo la costruzione di una sequenza simile:
  a(1) = 1
  a(2) = minimo tra 12 e 21 --> 12
  a(3) = minimo tra 123 e 321 --> 123
  ...
  a(9) = 123456789
  a(10) = minimo tra 12345678910 e 10123456789 --> 10123456789
  ...

In formule:
  a(1) = 1
  a(n) = min((n unito a(n-1)) (a(n-1) unito n))

(define (seq2 num)
  (let ( (out '()) (str "") )
    (for (k 1 num)
      (setq a (string str k))
      (setq b (string k str))
      (if (< a b)
        (setq str a)
        ;else
        (setq str b))
      (push (bigint str) out -1))
    out))

(seq2 15)
;-> (1L 12L 123L 1234L 12345L 123456L 1234567L 12345678L 123456789L
;->  10123456789L 1012345678911L 101234567891112L 10123456789111213L
;->  1012345678911121314L 101234567891112131415L)

Se invece prendiamo il valore massimo dell'unione:
  a(1) = 1
  a(n) = max((n unito a(n-1)) (a(n-1) unito n))

(define (seq3 num)
  (let ( (out '()) (str "") )
    (for (k 1 num)
      (setq a (string str k))
      (setq b (string k str))
      (if (> a b)
        (setq str a)
        ;else
        (setq str b))
      (push (bigint str) out -1))
    out))

(seq3 15)
;-> (1L 21L 321L 4321L 54321L 654321L 7654321L 87654321L 987654321L
;->  98765432110L 9876543211011L 987654321101112L 98765432110111213L
;->  9876543211011121314L 987654321101112131415L)

Vediamo una sequenza in cui la concatenazione dei numeri deve essere fatta nell'ordine che minimizza il risultato:

Sequenza OEIS A060555:
String together the first n numbers in an order which minimizes the result.
  1, 12, 123, 1234, 12345, 123456, 1234567, 12345678, 123456789,
  10123456789, 1011123456789, 101111223456789, 10111121323456789,
  1011112131423456789, 101111213141523456789, 10111121314151623456789,
  1011112131415161723456789, 101111213141516171823456789,
  10111121314151617181923456789, ...

Algoritmo:
Trattiamo ogni k come stringa s.
Per due stringhe a e b:
mettiamo a prima di b se ab < ba
cioè confrontiamo in modo lessicografico le concatenazioni.

Complessità
Per ogni n:
 1) sort: O(n log n)
 2) concatenazione: O(n)
 Totale ~ O(n^2*log(n)) per la lista completa.
È inevitabile perché l'ordine cambia a ogni inserimento.

(define (seq4 N)
  (letn (lst '() out '() s "")
    (for (k 1 N)
      (push (string k) lst -1)
      (sort lst (fn (a b) (<= (string a b) (string b a))))
      (setq s "")
      (dolist (x lst)
        (extend s x))
      (push (bigint s) out -1))
    out))

(seq4 20)
;-> (1L 12L 123L 1234L 12345L 123456L 1234567L 12345678L 123456789L
;->  10123456789L 1011123456789L 101111223456789L 10111121323456789L
;->  1011112131423456789L 101111213141523456789L 10111121314151623456789L
;->  1011112131415161723456789L 101111213141516171823456789L
;->  10111121314151617181923456789L 1011112131415161718192023456789L)

Adesso la sequenza in cui la concatenazione dei numeri deve essere fatta nell'ordine che massimizza il risultato:

Sequenza OEIS A060554:
String together the first n numbers in an order which maximizes the result.
  1, 21, 321, 4321, 54321, 654321, 7654321, 87654321, 987654321,
  98765432110, 9876543211110, 987654321211110, 98765432131211110,
  9876543214131211110, 987654321514131211110, 98765432161514131211110, ...

(define (seq5 num)
  (letn (lst '() out '() s "")
    (for (k 1 num)
      (push (string k) lst -1)
      (sort lst (fn (a b) (>= (string a b) (string b a))))
      (setq s "")
      (dolist (x lst)
        (extend s x))
      (push (bigint s) out -1))
    out))

(seq5 20)
;-> (1L 21L 321L 4321L 54321L 654321L 7654321L 87654321L 987654321L
;->  98765432110L 9876543211110L 987654321211110L 98765432131211110L
;->  9876543214131211110L 987654321514131211110L 98765432161514131211110L
;->  9876543217161514131211110L 987654321817161514131211110L
;->  98765432191817161514131211110L 9876543220191817161514131211110L)

Possiamo cambiare anche il verso dell'unione per ogni numero:

  a(1) = 1
  a(2) = 12 (unione a destra)
  a(3) = 312 (unione a sinistra)
  a(4) = 3124 (unione a destra)
  ...
oppure:

  a(1) = 1
  a(2) = 21 (unione a sinistra)
  a(3) = 213 (unione a destra)
  a(4) = 4213 (unione a sinistra)
  ...

(define (seq6 num test)
  (let ( (out '()) (str "") )
    (for (k 1 num)
      (if (test k)
        (setq str (string str k))
        ;else
        (setq str (string k str)))
      (push (bigint str) out -1))
    out))

(seq6 15 odd?)
;-> (1L 21L 213L 4213L 42135L 642135L 6421357L 86421357L 864213579L
;->  10864213579L 1086421357911L 121086421357911L 12108642135791113L
;->  1412108642135791113L 141210864213579111315L)
(seq6 15 even?)
;-> (1L 12L 312L 3124L 53124L 531246L 7531246L 75312468L 975312468L
;->  97531246810L 1197531246810L 119753124681012L 13119753124681012L
;->  1311975312468101214L 151311975312468101214L)


------------------------------------------------------
Numero di zeri nei numeri da 1 a 111...1 (n + 1 cifre)
------------------------------------------------------

Sequenza OEIS A014925:
Number of zeros in numbers 1 to 111...1 (n+1 digits).
  1, 21, 321, 4321, 54321, 654321, 7654321, 87654321, 987654321,
  10987654321, 120987654321, 1320987654321, 14320987654321,
  154320987654321, 1654320987654321, 17654320987654321,
  187654320987654321, 1987654320987654321, 20987654320987654321,
  220987654320987654321

Esempio:
  a(1) --> zeri tra 1 e 11   --> 1
  a(2) --> zeri tra 1 e 111  --> 21
  a(3) --> zeri tra 1 e 1111 --> 4321

Formula:
  a(1) = 1
  a(n) = n*10^(n-1) + a(n-1), per n > 1

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(define (seq num)
  (let (out '(1L))
  (for (n 2 num)
  ;(print n { } (* n (** 10 (- n 1)))) (read-line)
    (push (+ (out -1) (* n (** 10 (- n 1)))) out -1))
  out))

(seq 15)
;-> (1L 21L 321L 4321L 54321L 654321L 7654321L 87654321L 987654321L
;->  10987654321L 120987654321L 1320987654321L 14320987654321L
;->  154320987654321L 1654320987654321L)


--------------------------------
Palprimo1*Palprimo2 = Palindromo
--------------------------------

Trovare, fino ad un dato N, i numeri palindromi che sono il prodotto di due primi palindromi (anche non distinti).

Esempi:
 3*3= 9
 5*11 = 55
 7*11 = 77
 11*3 = 33
 101*3 = 303
 131*3 = 393
 151*11 = 1661

Sequenza OEIS A046328:
Palindromes with exactly 2 prime factors (counted with multiplicity).
  4, 6, 9, 22, 33, 55, 77, 111, 121, 141, 161, 202, 262, 303, 323, 393,
  454, 505, 515, 535, 545, 565, 626, 707, 717, 737, 767, 818, 838, 878,
  898, 939, 949, 959, 979, 989, 1111, 1441, 1661, 1991, 3113, 3223, 3443,
  3883, 7117, 7447, 7997, 9119, 9229, 9449, 10001, ...

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

(define (palindromo? num) (= (string num) (reverse (string num))))

(define (palprimi N)
  (letn ( (out '())
          (lst (primes-to N))
          (len (length lst)) )
    (for (i 0 (- len 1))
      (for (j i (- len 1))
        (setq a (lst i))
        (setq b (lst j))
        (if (palindromo? (* a b))
          (push (list (* a b) a b) out -1))))
    ; generiamo molti palindromi > N,
    ; ma quelli ordinati e corretti sono solo quelli fino a N
    (filter (fn(x) (<= x N)) (sort (map first out)))))

Proviamo:

(palprimi 1000)
;-> (4 6 9 22 33 55 77 111 121 141 161 202 262 303 323 393 454 505 515 535 545 565 626
;->  707 717 737 767 818 838 878 898 939 949 959 979 989)

(time (println (palprimi 10001)))
;-> (4 6 9 22 33 55 77 111 121 141 161 202 262 303 323 393
;->  454 505 515 535 545 565 626 707 717 737 767 818 838 878
;->  898 939 949 959 979 989 1111 1441 1661 1991 3113 3223 3443
;->  3883 7117 7447 7997 9119 9229 9449 10001)
;-> 1741.462


----------------------------
Persistenza dei numeri primi
----------------------------

Definiamo la "persistenza additiva" di un numero come i passaggi prima che il numero collassi a una singola cifra (addizione di cifre).
Definiamo la "persistenza moltiplicativa" di un numero come i passaggi prima che il numero collassi a una singola cifra (moltiplicazione di cifre).

Esempi:
La persistenza additiva di 679 vale 3.
679 -> 6+7+9=21 -> (2+1) = 3
679 -> 21 -> 3 (2 passaggi)

La persistenza moltiplicativa di 679 vale 6.
679 -> (6*7*9)=378 -> (3*7*8)=168 -> (1*6*8)=48 -> (4*2)=32 -> (3*2)=6.
679 -> 378 -> 168 -> 48 -> 32 -> 6 (5 passaggi).

Vedi "Persistenza di un numero" su "Rosetta code".

Sequenza OEIS A107450:
Additive persistence of the prime numbers.
  0, 0, 0, 0, 1, 1, 1, 2, 1, 2, 1, 2, 1, 1, 2, 1, 2, 1, 2, 1, 2, 2, 2, 2,
  2, 1, 1, 1, 2, 1, 2, 1, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 1, 1,
  2, 2, 1, 2, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 2, 1, 2, 2, 2, 2, 2,
  2, 2, 3, 2, 2, 3, 1, 2, 2, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 1,
  2, 1, 2, 2, ...

Sequenza OEIS A129985:
Multiplicative persistence of the prime numbers.
  0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 1, 2, 1, 2, 3, 2, 3, 1, 2, 1, 2, 3, 2, 3,
  3, 1, 1, 1, 1, 1, 2, 1, 2, 3, 3, 1, 3, 2, 2, 2, 3, 1, 1, 3, 3, 2, 1, 2,
  3, 3, 2, 3, 1, 2, 2, 3, 2, 2, 4, 2, 3, 3, 1, 1, 1, 2, 1, 3, 3, 2, 3, 3,
  3, 3, 4, 3, 3, 4, 1, 1, 3, 1, 2, 3, 2, 3, 3, 2, 2, 3, 4, 3, 3, 3, 3, 1,
  1, 2, 2, 2, ...

(define (pers-add n)
"Calculate the additive persistence of an integer"
  (let (out 0)
    (while (> n 9)
      (setq n (digit-sum n))
      (++ out)
    )
    out))

(define (digit-sum num)
"Calculate the sum of the digits of an integer"
  (let (out 0)
    (while (!= num 0)
      (setq out (+ out (% num 10)))
      (setq num (/ num 10)))
    out))

(pers-add 679)
;-> 2
(define (pers-mul n)
"Calculate the multiplicative persistence of an integer"
  (let (out 0)
    (while (> n 9)
      (setq n (digit-prod n))
      (++ out))
    out))

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

(pers-mul 679)
;-> 5

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

(define (seq-add N)
  (map pers-add (primes-to N)))

(seq-add 100)
;-> (0 0 0 0 1 1 1 2 1 2 1 2 1 1 2 1 2 1 2 1 2 2 2 2 2)

(define (seq-mul N)
  (map pers-mul (primes-to N)))

(seq-mul 100)
;-> (0 0 0 0 1 1 1 1 1 2 1 2 1 2 3 2 3 1 2 1 2 3 2 3 3)


-------------
Gosper's Hack
-------------

Il trucco noto come Gosper's hack (o snoob: next higher number with same number of bits) permette, dato un numero x, di trovare il primo numero maggiore di x che ha lo stesso numero di bit a 1.
Il nome viene da Bill Gosper, storico membro del gruppo AI del MIT negli anni '70 (ambiente 'hacker culture', insieme a Knuth, Sussman, ecc.).
Comparve in un laboratorio e poi fu diffuso in 'Hacker's Delight' e negli archivi Usenet.

L'obiettivo matematico è molto semplice:
dato un intero x con k bit a 1:

  k = popcount(x)

trovare il minimo y > x tale che

  popcount(y) = k

cioè il successivo sottoinsieme k-combinazione rappresentato in binario.

Infatti:

  numero binario <-> sottoinsieme
  bit 1 <-> elemento scelto

Esempio con k = 3:

00111 = (0 1 2)
01011 = (0 1 3)
01101 = (0 2 3)
01110 = (1 2 3)
10011 ...

Esempi:

k = 2
  "11"   = 3
  "101"  = 5
  "110"  = 6
  "1001" = 9
  ...

k = 3
  "111"    = 7
  "1011"   = 11
  "1101"   = 13
  "1110"   = 14
  "10011"  = 19
  "10101"  = 21
  "10110"  = 22
  "11001"  = 25
  "11010"  = 26
  "11100"  = 28
  "100011" = 35
  ...

Quindi il problema è puramente combinatorio: stiamo enumerando le combinazioni in ordine lessicografico.

Matematicamente
---------------

Scriviamo x in forma generale:

... A 0 1 1 1 0 0 0
          ^^^ ^^^
        blocco1 zeri finali

Sia:
- c = numero di zeri finali
- r = numero di 1 subito a sinistra

Pattern:

... 0 1^r 0^c


Il prossimo numero con lo stesso numero di 1 si ottiene:
1. spostando a sinistra il primo "01" -> "10"
2. mettendo tutti gli 1 rimasti il più a destra possibile
cioè:

... 1 0 0^(?) 1^(r-1)

Questo garantisce:
- stesso numero di 1
- incremento minimo ⇒ successivo in ordine

Traduzione bitwise
------------------

Step 1 — isola il bit meno significativo

  c = x & (-x)
Proprietà aritmetica in complemento a due:
  -x = ~x + 1
Quindi 'x & (-x)' estrae la potenza di due più bassa (rightmost 1).

Esempio:
  x      = 1011000
  -x     = 0101000 (in two's complement)
  AND    = 0001000
Serve per sapere di quanto "salire".

Step 2 — aggiungi c

  r = x + c
Questo:
- trasforma il primo "01" in "10"
- azzera tutti i bit a destra

Esempio:
  1011000
  +001000
  -------
  1100000
Hai spostato il blocco.

Step 3 — ricostruisci gli 1 persi

  (r ^ x)
mostra i bit cambiati.
Poi:
  ((r ^ x) >> 2) / c
conta quanti 1 devono tornare a destra e li compatta.
Infine:
  | r
li reinserisce.

Risultato finale:
  (((r ^ x) >> 2) / c) | r

Formula completa
----------------
  c = x & -x
  r = x + c
  next = (((r ^ x) >> 2) / c) | r

Costo:
- tempo costante O(1)
- solo operazioni bitwise e aritmetiche

Interpretazione combinatoria
----------------------------

In realtà stiamo facendo:
- enumerazione delle combinazioni
- in ordine co-lessicografico
- senza generare tutte le 2^n stringhe

Quindi:
brute force -> O(valore numerico)
Gosper -> O(numero di combinazioni)


Versione C:

unsigned snoob(unsigned x) {
  unsigned smallest, ripple, ones;
                               // x = xxx0 1111 0000
  smallest = x & -x;           // 0000 0001 0000
  ripple = x + smallest;       // xxx1 0000 0000
  ones = x ^ ripple;           // 0001 1111 0000
  ones = (ones >> 2)/smallest; // 0000 0000 0111
  return ripple | ones;        // xxx1 0000 0111
}

Versione newLISP:

(define (snoob x)
  (letn (c (& x (- x))
         r (+ x c))
    (| r (/ (>> (^ r x) 2) c))))

Proviamo:

(snoob 3)
;-> 5
(snoob 7)
;-> 11

Negli anni '70:
- niente memoria
- niente CPU veloci
Generare combinazioni senza allocazioni era cruciale.
Questo trucco permetteva di scorrere tutte le k-subset usando 'solo registri', senza liste o ricorsione.
Per questo è diventato un classico "hacker trick".


------------------------------------------
N-esimo numero con lo stesso numero di bit
------------------------------------------

Dati due numeri interi positivi n e k, scrivere una funzione che restituisce un numero intero che rappresenta l'n-esimo numero intero positivo più piccolo che ha esattamente k unità (1) nella sua rappresentazione binaria.

Esempi:

n = 3, k = 2 --> soluzione = 6
  "11"   = 3
  "101"  = 5
  "110"  = 6 --> soluione
  "1001" = 9
  ...

n = 5, k = 3 --> soluzione = 19
  "111"    = 7
  "1011"   = 11
  "1101"   = 13
  "1110"   = 14
  "10011"  = 19 --> soluzione
  "10101"  = 21
  "10110"  = 22
  "11001"  = 25
  "11010"  = 26
  "11100"  = 28
  "100011" = 35
  ...

Algoritmo 1: Brute-force
------------------------

Ciclo da 1 all'n-esimo numero che ha k bit a 1 nella sua rappresentazione binaria

(define (pop-count1 num)
"Calculate the number of 1 in binary value of an integer number"
  (let (counter 0)
    (while (> num 0)
      (setq num (& num (- num 1)))
      (++ counter))
    counter))

(define (trova n k)
  (let ( (out '())
         (conta 0)
         (num 1) )
    (while (< conta n)
      (when (= (pop-count1 num) k)
        (push num out -1)
        (++ conta))
      (++ num))
    (println out)
    ;(println (length out))
    (last out)))

Proviamo:

(trova 3 1)
;-> (1 2 4)
;-> 4
(trova 4 2)
;-> (3 5 6 9)
;-> 9
(trova 10 2)
;-> (3 5 6 9 10 12 17 18 20 24)
;-> 24

Comunque questa funzione è molto lenta per grandi valori di n e/o k:

(time (println (trova 250 2)))
;-> (...)
;-> 4456448
;-> 6047.534

Algoritmo 2: Gosper's Hack
--------------------------

Il collo di bottiglia è evidente: stiamo testando tutti i numeri e calcolando ogni volta la 'pop-count'.
Quindi la complessità è circa O(valore-risultato).
Un idea migliore è quella di generare direttamente i numeri con k bit a 1.
Partiamo dal numero più piccolo che ha esattamente 'k' bit a 1:

  111...1   (k volte 1)

cioè,

  (2^k - 1)

Poi possiamo generare il successivo numero con lo stesso numero di bit a 1 usando il trucco bitwise noto come 'Gosper's hack'.

Gosper Hack
-----------
  c = x & -x
  r = x + c
  x = (((r ^ x) >> 2) / c) | r

Questo produce il prossimo intero (x) con lo stesso popcount in tempo costante.

Quindi la complessità diventa O(n) (invece di scandire milioni di numeri).

(define (next-comb x)
  (letn (c (& x (- x))
         r (+ x c))
    (| r (/ (>> (^ r x) 2) c))))

(define (trova-fast n k)
  (letn (x (- (<< 1 k) 1)
         i 1)
    (while (< i n)
      (setq x (next-comb x))
      (++ i))
    x))

Proviamo:

(trova-fast 3 1)
;-> 4
(trova-fast 4 2)
;-> 9
(trova-fast 10 2)
;-> 24

(time (println (trova-fast 250 2)))

ora è praticamente istantaneo.

Se vogliamo la lista completa:

(define (trova-list n k)
  (letn ( (x (- (<< 1 k) 1))
          (out (list x))
          (i 1) )
    (while (< i n)
      (setq x (next-comb x))
      (push x out -1)
      (++ i))
    out))

Proviamo:

(trova-list 3 1)
;-> (1 2 4)
(trova-list 4 2)
;-> (3 5 6 9)
(trova-list 10 2)
;-> (3 5 6 9 10 12 17 18 20 24)


---------------------------
La funzione "doz" o "monus"
---------------------------

La funzione "doz" (Difference Or Zero) è definita nel modo seguente:

           | (x - y), se x >= y
  doz(x) = |
           | 0, se x < y

I matematici la chiamano "monus".
È chiamata anche "sottrazione di prima elementare" perchè il risultato è 0 se si cerca di togliere troppo.

In C:

  doz(x,y) = (x - y) & -(x >= y)

Funzionamento:
- (x >= y) -> 1 oppure 0
- negazione -> -1 (= tutti 1 in complemento a due) oppure 0
- AND -> tiene il risultato oppure lo azzera

In newLISP:
In questo caso non possiamo applicare il bit-trick direttamente perchè in newLISP tutti i valori sono 'true' tranne 'nil', quindi anche '0' vale true.

(define (doz x y)
  (& (- x y) (- (if (> x y) 1 0))))

(doz 10 7)
;-> 3
(doz 7 10)
;-> 0
(doz 7 7)
;-> 0

oppure

(define (doz x y)
  (if (> x y) (- x y) 0))

(doz 10 7)
;-> 3
(doz 7 10)
;-> 0
(doz 7 7)
;-> 0

oppure

(define (doz x y)
  (max 0 (- x y)))

oppure

(define (doz x y)
  (- x (min x y)))

Con la funzione "doz" è possibile implementare le funzioni max e min:

  max(x,y) = y + doz(x,y)
  min(x,y) = x - doz(x,y)


-------------------------------
Alternanza tra due o tre valori
-------------------------------

Abbiamo una variabile 'x' che può avere solo due valori possibili 'a' e 'b'.
Scrivere una funzione che assegna ad 'x' alternativamente 'a' e 'b' (flip value).
In altri termini:
  func(a) --> b
  func(b) --> a

Primo metodo:

In C:

if (x == a) x = b;
else x = a;

oppure

x = x == a ? b : a;

In newLISP:

(define (flip1 x)
  (if (= x a)
      (setq x b)
      ;else
      (setq x a)))

(setq a 1)
(setq b 0)
(setq x a)
;-> 1
(setq x (flip1 x))
;-> 0
(setq x (flip1 x))
;-> 1
(setq x (flip1 x))
;-> 0

Secondo metodo:

(define (flip2 x) (+ a b (- x)))
(setq a 3)
(setq b 5)
(setq x a)
;-> 3
(setq x (flip2 x))
;-> 5
(setq x (flip2 x))
;-> 3
(setq x (flip2 x))
;-> 5

Adesso abbiamo tre costanti arbitrarie ma distinte 'a', 'b' e 'c' e cerchiamo una funzione 'func' che soddisfi:

  func(a) = b
  func(b) = c
  func(c) = a

In C:

if (x == a) x = b;
else if (x == b) x = c;
else x = a;

In newLISP:

(define (flip3)
  (if (= x a) b
      (= x b) c
      a))

(setq a 1 b 2 c 3)
(setq x a)
;-> 1
(setq x (flip3 x))
;-> 2
(setq x (flip3 x))
;-> 3
(setq x (flip3 x))
;-> 1
(setq x (flip3 x))
;-> 2


-------------------------------------------------
Prime potenze di 2 minore e maggiore di un intero
-------------------------------------------------

Dato un numero intero positivo 'x' determinare la prima potenza di 2 minore di 'x' e la prima potenza di 2 maggiore di 'x'.
Se 'x' è una potenza di 2 restituire 'x'.

Esempi:
  x = 5
  Prima potenza di 2 minore = 4
  Prima potenza di 2 maggiore = 8

  x = 31
  Prima potenza di 2 minore = 16
  Prima potenza di 2 maggiore = 31

  x = 64  --> 64 (è una potenza di 2)

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

Prima potenza di 2 minore o uguale a x
--------------------------------------

In C:
unsigned flp(unsigned x) {
  x = x | (x >> 1);
  x = x | (x >> 2);
  x = x | (x >> 4);
  x = x | (x >> 8);
  x = x | (x >> 16);
  x = x | (x >> 32);
  return x - (x >> 1);
}

(define (flp x)
  (setq x (| x (>> x 1)))
  (setq x (| x (>> x 2)))
  (setq x (| x (>> x 4)))
  (setq x (| x (>> x 8)))
  (setq x (| x (>> x 16)))
  (setq x (| x (>> x 32)))
  (- x (>> x 1)))

(flp 16385)
;-> 16384
(flp 16384)
;-> 16384
(flp 16383)
;-> 8192

Valore massimo:
(- (** 2 63) 1)
;-> 9223372036854775807
MAX-INT = 9223372036854775807
(flp 9223372036854775807)
;-> 4611686018427387904
(** 2 62)
;-> 4611686018427387904L

Prima potenza di 2 maggiore o uguale a x
----------------------------------------
In C:
unsigned clp(unsigned x) {
  x = x − 1;
  x = x | (x >> 1);
  x = x | (x >> 2);
  x = x | (x >> 4);
  x = x | (x >> 8);
  x = x | (x >> 16);
  x = x | (x >> 32);
  return x + 1;
}

(define (clp x)
  (setq x (- x 1))
  (setq x (| x (>> x 1)))
  (setq x (| x (>> x 2)))
  (setq x (| x (>> x 4)))
  (setq x (| x (>> x 8)))
  (setq x (| x (>> x 16)))
  (setq x (| x (>> x 32)))
  (+ x 1))

(clp 16385)
;-> 32768
(clp 16384)
;-> 16384
(clp 16383)
;-> 16384

Valore massimo:
(** 2 61)
;-> 2305843009213693952
(clp (+ 2305843009213693952 1))
;-> 4611686018427387904
(clp 2305843009213693952)
;-> 2305843009213693952


-------------
Cocktail sort
-------------

Il Cocktail Sort o (Shaker Sort) è una variante di Bubble Sort.
L'algoritmo Bubble Sort attraversa sempre gli elementi da sinistra e sposta l'elemento più grande nella posizione corretta nella prima iterazione, il secondo più grande nella seconda iterazione e così via.
Cocktail Sort attraversa una data lista in entrambe le direzioni alternativamente.
Cocktail Sort non esegue iterazioni non necessarie, rendendolo efficiente per array di grandi dimensioni.

Algoritmo:
Ogni iterazione dell'algoritmo è suddivisa in 2 fasi:
La prima fase esegue un ciclo nella lista da sinistra a destra, proprio come Bubble Sort.
Durante il ciclo, gli elementi adiacenti vengono confrontati e, se il valore a sinistra è maggiore del valore a destra, i valori vengono scambiati.
Alla fine della prima iterazione, il numero più grande si troverà alla fine della lista.
La seconda fase esegue un ciclo nella lista in direzione opposta, partendo dall'elemento immediatamente precedente a quello ordinato più di recente e tornando all'inizio della lista.
Anche in questo caso, gli elementi adiacenti vengono confrontati e, se necessario, scambiati.

1) Iniziare dall'inizio ddella lista e confrontare ogni coppia adiacente di elementi. Se sono nell'ordine sbagliato, scambiarli.
2) Continuare a iterare nella lista in questo modo fino a raggiungere la fine della lista.
3) Quindi, spostarsi nella direzione opposta dalla fine della lista all'inizio, confrontando ogni coppia adiacente di elementi e scambiandoli se necessario.
4) Continuare a iterare nella lista in questo modo fino a raggiungere l'inizio della lista.
5) Ripetere i passaggi da 1 a 4 fino a quando la lista non è completamente ordinata.

; Funzione che ordina una lista di numeri in modo ascendente (cocktail sort)
(define (cocktail lst)
  (local len arr start end swapped continue)
    (setq len (length lst))
    ; usiamo un vettore che è molto più veloce di una lista
    ; quando indicizziamo gli elementi
    (setq arr (array len lst))
    (setq start 0)
    (setq end (- len 1))
    (setq swapped true)
    (setq continue true)
    (while (and swapped continue)
          ; reimposta a 'nil' il flag 'swapped' all'ingresso del ciclo,
          ; perché potrebbe essere 'true' da un'iterazione precedente.
          (setq swapped false)
          ; ciclo da sinistra a destra come il bubble sort
          (for (i start (- end 1))
            (when (> (arr i) (arr (+ i 1)))
                (swap (arr i) (arr (+ i 1)))
                (setq swapped true)))
          (cond
              ; se non viene spostato nulla, la lista è ordinata
              ; e fermiamo il ciclo ponendo 'continue' a nil
              ((nil? swapped) (setq continue nil))
              (true
                ; altrimenti, reimposta il flag 'swapped' in modo che
                ; possa essere utilizzato nella fase successiva
                (setq swapped nil)
                ; sposta indietro di uno il punto finale
                ; perchè l'elemento alla fine è nella sua posizione corretta
                (-- end)
                ; ciclo da destra a sinistra,
                ; eseguendo lo stesso confronto della fase precedente
                (for (i (- end 1) start -1)
                  (when (> (arr i) (arr (+ i 1)))
                      (swap (arr i) (arr (+ i 1)))
                      (setq swapped true)))
                ; aumentare il punto di partenza,
                ; perchè l'ultima fase ha spostato il successivo
                ; numero più piccolo nella sua posizione corretta.
                (++ start))))
    ; restituisce la lista ordinata (conversione dal vettore 'arr')
    (array-list arr))

Proviamo:

(cocktail '(2 4 1 3 5 4 21 7 8 9))
;-> (1 2 3 4 4 5 7 8 9 21)

(setq a (rand 10 100))
(cocktail a)
;-> (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 3 3 3 3 3 3
;->  4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 6 6 6 6
;->  7 7 7 7 7 7 7 7 8 8 8 8 8 8 8 8 8 8 8 8 9 9 9 9 9 9 9 9 9 9 9 9 9 9)

Test di velocità:

(setq lst (rand 1000 10000))
(time (cocktail lst))
;-> 5111.574
(time (sort lst))
;-> 0

Test di correttezza:

(for (i 1 100)
  (setq lst (rand 100 1000))
  (if (!= (cocktail lst) (sort lst))
      (println "Errore: " lst)))
;-> nil


----------------------------
Verificare le ottimizzazioni
----------------------------

Alle volte, quello che sembra una modifica che ottimizza una funzione, in realtà ne peggiora le prestazioni.
Prendiamo il prodotto di due numeri complessi.

  c1 = (a + bi)
  c2 = (c + di)

Il loro prodotto vale:

  m = c1 * c2 = (a + bi )(c + di) = (ac – bd) + (ad + bc)i

Per eseguire il calcolo del prodotto dobbiamo eseguire 4 moltiplicazioni e 3 somme/sottrazioni:
Moltiplicazioni: a*c, a*d, b*c, e b*d.

Con passaggi algebrici possiamo ridurre il calcolo a 3 moltiplicazioni e 6 somme/sottrazioni:

  p = ac
  q = bd
  r = (a + b)(c + d)
  m = (p - q) + (r - p - q)i

Moltiplicazioni: a*c, b*d, e (a + b)*(c + d)

Quindi risulta:

(ac – bd) + (ad + bc)i = (p - q) + (r - p - q)i

In teoria la moltiplicazione che abbiamo evitato dovrebbe essere più importante delle 3 somme/sottrazioni che abbiamo introdotto.
Cioè, dovrebbe essere più veloce fare 3 somme/sottrazioni che una sola moltiplicazione.
Vediamo se questo è vero:

; Funzione con 4* e 3+-
(define (m1 a b c d)
  (list (sub (mul a c) (mul b d)))
        (add (mul a d) (mul b c)))

; Funzione con 3* e 6+-
(define (m2 a b c d)
  (list (sub (mul a c) (mul b d)))
        (sub (mul (add a b) (add c d)) (mul a c) (mul b d)))

(m1 11 77 21 42)
;-> 2079
(m2 11 77 21 42)
;-> 2079

Test di correttezza:

(for (i 1 1000)
  (setq t (rand 1000 4))
  (if (!= (apply m1 t) (apply m2 t))
      (println t)))
;-> nil

Test di velocità:

Numeri interi (int):
(time (m1 35 6 940 197) 1e6)
;-> 230.181
(time (m2 35 6 940 197) 1e6)
;-> 280.207

Numeri a virgola mobile (float):
(time (m1 284.188360240485 920.92654194769 769.2190313425 839.167455061) 1e6)
;-> 234.254
(time (m2 284.188360240485 920.92654194769 769.2190313425 839.167455061) 1e6)
;-> 288.763

Quindi la nostra 'ottimizzazione' ha peggiorato la velocità della funzione.

Vediamo un altro confronto:

(setq a 11.7312)
(setq b -122198565.39987233612)
(setq c 565827.2760238)
(define (plus) (add a b c))
(define (mult) (mul a b))
(define (mult1) (mul a b c))
(time (plus) 1e7)
;-> 464.078
(time (mult) 1e7)
;-> 426.364
(time (mult1) 1e7)
;-> 471.49


--------------------------
Numeri naturali senza zeri
--------------------------

Dato un numero intero positivo N, generiamo la sequenza dei numeri da 1 a N eliminando tutte le cifre che valgono 0.
Quanti numeri 'diversi' abbiamo generato?
Scrivere una funzione che conta i numeri univoci (tutti diversi tra loro).

Sequenza OEIS A052386:
Number of integers from 1 to 10^n-1 that lack 0 as a digit.
  0, 9, 90, 819, 7380, 66429, 597870, 5380839, 48427560, 435848049,
  3922632450, 35303692059, 317733228540, 2859599056869, 25736391511830,
  231627523606479, 2084647712458320, 18761829412124889,
  168856464709124010, 1519708182382116099, 13677373641439044900, ...

Primo metodo: brute force
-------------------------

(define (diversi1 N)
  (if (= N 1) 1
  ;else
  (let ( (out '()) (seq (sequence 1 N)) )
    (dolist (el seq)
      (push (int (replace "0" (string el) "") 0 10) out -1))
    (length (unique out)))))

(diversi1 200)
;-> 171

Per creare la sequenza:

(map diversi1 '(1 10 1e2 1e3 1e4 1e5 1e6))
;-> (1 9 90 819 7380 66429 597870)

Secondo metodo: formula
-----------------------

  a(n) = 9*a(n-1) + 9

(define (diversi2 num)
  (let (out '(0L))
  (for (i 1 (- num 1))
    (push (+ 9L (* 9L (out -1))) out -1))))

(diversi2 21)
;-> (0L 9L 90L 819L 7380L 66429L 597870L 5380839L 48427560L 435848049L
;->  3922632450L 35303692059L 317733228540L 2859599056869L 25736391511830L
;->  231627523606479L 2084647712458320L 18761829412124889L
;->  168856464709124010L 1519708182382116099L 13677373641439044900L)


-----------------------------------
Radice quadrata intera di un intero
-----------------------------------

La seguente funzione calcola la radice quadrata intera (floor(sqrt(x))) usando la ricerca binaria.

(define (isqrt x)
  ; a = limite inferiore della ricerca
  ; b = limite superiore stimato come 2^(bits(x)/2)
  (letn (a 1
         b (<< 1 (>> (length (bits x)) 1)))
    ; ciclo finché l'intervallo di ricerca è valido
    (do-while (>= b a)
      ; punto medio dell'intervallo (a + b) / 2
      (setq m (>> (+ a b) 1))
      ; se m^2 supera x, la radice è più piccola -> sposta b a sinistra
      (if (> (* m m) x)
          (setq b (- m 1))
          ; altrimenti la radice è >= m -> sposta a a destra
          (setq a (+ m 1))))
    ; quando il ciclo termina, a ha superato la soluzione di 1
    ; quindi la radice intera è a-1
    (- a 1)))

Proviamo:

(isqrt 15)
;-> 3
(isqrt 18)
;-> 4

(isqrt 4294836225)
;-> 65535
(* 65535 65535)
;-> 4294836225

(* 65536 65536)
;-> 4294967296
(isqrt 4294967296)
;-> nil
(isqrt 4294967295)
;-> 65535

Quindi il valore massimo gestibile dalla funzione vale 4294967295.

Algoritmo
---------
1) Stima iniziale del limite superiore
   (bits x) produce la rappresentazione binaria di 'x'.
   (length (bits x)) è il numero di bit ~ log2(x)+1.
   Se 'x' ha k bit, allora sqrt(x) ha circa k/2 bit.
   Quindi:
   2^(k/2) è un ottimo limite superiore.
   Questo è ciò che fa:
   (>> (length (bits x)) 1)   ; k/2
   (<< 1 ...)                 ; 2^(k/2)
   Risultato: 'b' è appena sopra sqrt(x).
   Funziona per 32/64/128 bit senza costanti (ma la primitiva >> funziona solo per i numeri int64).

2) Ricerca binaria
   Manteniamo l'intervallo [a, b].
   Ogni iterazione:
   - calcola il medio 'm'
   - confronta 'm*m' con 'x'
   Se (m*m > x) -> radice più piccola -> riduci 'b'
   Altrimenti -> radice più grande -> aumenta 'a'
   Complessità: O(log x)
3. Terminazione
   Quando (a > b), l’ultimo valore valido è (a-1), che è:
   il massimo 'm' tale che (m*m <= x).

In pratica è una 'binary search' su [1, 2^(bits/2)].


-------------------------------------------------
Ordinamento di interi in base al binario riflesso
-------------------------------------------------

La riflessione binaria di un numero intero positivo è definita come il numero ottenuto invertendo l'ordine delle sue cifre binarie (ignorando gli zeri iniziali) e interpretando il numero binario risultante come decimale.
Data una lista di interi positivi, ordinare la lista in ordine crescente in base alla riflessione binaria di ciascun elemento.
Se due numeri diversi hanno la stessa riflessione binaria, il numero originale più piccolo dovrebbe apparire per primo.

Esempio: lista = (3 6 5 8)
(setq base '(3 6 5 8))
(setq lst base)
; numeri in binario
(setq lst (map bits lst))
("11" "110" "101" "1000")
; numeri in binario riflessi
(setq lst (map reverse lst))
; numeri decimali dei binari riflessi
(setq lst (map (fn(x) (int x 0 2)) lst))
;-> (3 3 5 1)
; associazione tra coppie:
; (numero-della-lista-data numero-decimale-del-binario-riflesso)
(setq coppie (map list lst base))
;-> ((3 3) (3 6) (5 5) (1 8))
; ordinamento crescente delle coppie
(sort coppie)
;-> ((1 8) (3 3) (3 6) (5 5))
; estrazione della lista ordinara (primo elemento di ogni coppia)
(map last coppie)
;-> (8 3 6 5)

; Funzione che ordina una la lista in ordine crescente in base alla riflessione binaria di ciascun elemento
(define (ordina lst)
  (map last
      (sort (map list (map (fn(x) (int (reverse (bits x)) 0 2)) lst) lst))))

Proviamo:

(ordina '(3 6 5 8))
;-> (8 3 6 5)

(ordina (sequence 1 10))
;-> (1 2 4 8 3 6 5 10 7 9)

Vedi anche "Ordinare una lista con un'altra lista" su "Note libere 5".
Vedi anche "Ordinare una lista con un'altra lista (variante)" su "Note libere 19".


---------------------------------------
Puzzle 1256: Another sequence of primes
---------------------------------------

https://www.primepuzzles.net/puzzles/puzz_1256.htm

Consider the following process: take a prime k>9, sum the digits, repeat the sum deleting the first addendum and adding the previous sum and so on.
Sequence lists the minimum prime k that produces a run of exactly n consecutive primes.

The first 12 terms:
  0   [13]
  1   [23, 5]
  2   [101, 2, 3]
  3   [11, 2, 3, 5]
  4   [317, 11, 19, 37, 67]
  5   [331, 7, 11, 19, 37, 67]
  6   [599, 23, 41, 73, 137, 251, 461]
  7   [35311, 13, 23, 41, 79, 157, 313, 613]
  8   [3393311, 23, 43, 83, 157, 311, 619, 1237, 2473]
  9   [3533377, 31, 59, 113, 223, 443, 883, 1759, 3511, 6991]
  10  [1933537, 31, 61, 113, 223, 443, 881, 1759, 3511, 6991, 13921]
  11  [331733953, 37, 71, 139, 277, 547, 1091, 2179, 4349, 8693, 17383, 34729]

Example:
  Seq(5) = 331 because:
  1) 3 + 3 + 1 = 7, prime;
  2) 3 + 1  + 7 = 11, prime;
  3) 1 + 7 + 11 = 19, prime;
  4) 7 + 11 + 19 = 37, prime;
  5) 11 + 19 + 37 = 67, prime, and 19 + 37 + 67 = 123 composite.

Sequenza OEIS A391445:
Take a prime p>9, sum the digits, repeat the sum deleting the first addendum and adding the previous sum and so on.
Sequence lists the minimum prime p that produces a run of exactly n consecutive primes.
  13, 23, 101, 11, 317, 331, 599, 35311, 3393311, 3533377, 1933537,
  331733953, 59393313971, 7117113335317, 171355597959913, 799353955313539,
  395377199771711897, 33793799795555374999, 33793799795555379859, ...

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

(define (int-list num)
"Convert an integer to a list of digits"
  (if (zero? num) '(0)
  (let (out '())
    (while (!= num 0)
      (push (% num 10) out)
      (setq num (/ num 10))) out)))

; Funzione che simula il processo per un numero primo
(define (count-primes num)
    (let ( (digits (int-list num))
           (stop nil)
           (conta 0) )
      (until stop
        ; somma le cifre
        (setq somma (apply + digits))
        ; Se la somma è un numero primo...
        (if (prime? somma)
          (begin
            ; aumenta il conteggio
            (++ conta)
            ; toglie il primo addendo (nella lista delle cifre)
            (pop digits)
            ;(println digits)
            ; aggiunge la somma come ultimo addendo
            (push somma digits -1))
          ;else
          ; altrimenti ferma il ciclo
          (setq stop true)))
      conta))

(count-primes 317)
;-> 4

; Funzione che elimina i nil alla fine di una lista/vettore
(define (trim-trailing-nil lst)
  (letn (ultimo -1)
    (for (i 0 (- (length lst) 1))
      (if (lst i) (setq ultimo i)))
    (if (= ultimo -1) '() (slice lst 0 (+ ultimo 1)))))

; Funzione che genera la sequenza (dato un valore massimo per i numeri primi):
(define (seq N)
  (local (out primi contatore conta)
    (setq out '())
    (setq primi (primes-to N))
    (println "...")
    (setq primi (slice primi 4))
    (setq contatore (array (+ (length primi) 1) '(nil)))
    ;(println primi)
    (dolist (p primi)
      (setq conta (count-primes p))
      ; aggiorna (contatore conta) se (contatore conta) vale 'nil'
      ; (così ogni cella del vettore può essere aggiornata solo una volta)
      (if-not (contatore conta) (setf (contatore conta) p)))
    ; rimuove i valori 'nil' alla fine del contatore
    (trim-trailing-nil contatore)))

Proviamo:

(time (println (seq 1e4)))
;-> (13 23 101 11 317 331 599)
;-> 15.586
(time (println (seq 1e6)))
;-> (13 23 101 11 317 331 599 35311)
;-> 275.717
(time (println (seq 1e7)))
;-> (13 23 101 11 317 331 599 35311 3393311 3533377 1933537)
;-> 2849.178
(time (println (seq 1e8)))
;-> (13 23 101 11 317 331 599 35311 3393311 3533377 1933537)
;-> 29308.478

Non provare '(time (println (seq 1e9))).
La funzione 'primes-to' può calcolare i primi fino 1e8, poi crasha il sistema (32 Gb RAM).


-------------
Trimming list
-------------

Data una lista del tipo:
  (nil nil 1 4 2 nil 4 nil nil 6 7 nil nil nil nil)

Come togliere tutti i 'nil' all'inizio della lista (trim-leading)?
Risultato: (1 4 2 nil 4 nil nil 6 7 nil nil nil nil)

Come togliere tutti i 'nil' in fondo alla lista (trim-trailing)?
Risultato: (nil nil 1 4 2 nil 4 nil nil 6 7)

Algoritmo (trime-leading)
  Scorrere la lista da sinistra
  Al primo valore uguale ad un dato valore salvare l'indice e fermare il ciclo
  Usare slice per restituire la lista partendo dall'indice trovato
  Se non trova nulla -> lista vuota

Logica:
- leading -> dolist diretto + slice
- trailing -> reverse + dolist + reverse
(il reverse costa, ma spesso resta molto veloce in pratica)

(define (trim-trailing value lst)
  ; indice del primo elemento valido trovato (nella lista invertita)
  ; stop serve per terminare dolist appena trovato
  ; rev è la lista invertita per poter tagliare i valori finali
  (let ((primo -1) (stop nil) (rev (reverse lst)))
    ; scorre la lista invertita
    ; $idx è l'indice corrente fornito da dolist
    ; appena trova un elemento diverso da value salva l'indice e ferma il ciclo
    (dolist (el rev stop)
      (if-not (= el value) (set 'primo $idx 'stop true)))
    ; se non è stato trovato nulla restituisce lista vuota
    ; altrimenti taglia e reinverte per ripristinare l'ordine originale
    (if (= primo -1) '() (reverse (slice rev primo)))))

(define (trim-leading value lst)
  ; indice del primo elemento valido
  ; stop per uscita anticipata
  (let ((primo -1) (stop nil))
    ; scorre la lista da sinistra
    ; al primo elemento diverso da value memorizza la posizione e termina
    (dolist (el lst stop)
      (if-not (= el value) (set 'primo $idx 'stop true)))
    ; se tutti gli elementi sono value restituisce lista vuota
    ; altrimenti fa slice dalla prima posizione valida
    (if (= primo -1) '() (slice lst primo))))

Proviamo:

(setq a '(nil nil 1 4 2 nil 4 nil nil 6 7 nil nil nil nil))
(setq b '(0 0 1 4 2 0 4 0 0 6 7 0 0 0 0))

(trim-leading nil a)
;-> (1 4 2 nil 4 nil nil 6 7 nil nil nil nil)
(trim-trailing nil a)
;-> (nil nil 1 4 2 nil 4 nil nil 6 7)

(trim-leading 0 b)
;-> (1 4 2 0 4 0 0 6 7 0 0 0 0)
(trim-trailing 0 b)
;-> (0 0 1 4 2 0 4 0 0 6 7)

Possiamo rendere le funzioni molto più veloci utilizzando 'find' per trovare direttamente il primo elemento diverso:

(find 0 '(0 0 1 3 2 0 2) !=)
;-> 2

(define (trim-leading value lst)
  ; 'find' trova l'indice del primo elemento diverso da value
  ; 'slice' restituisce la lista a partire da quell'indice
  (slice lst (find value lst !=)))

(define (trim-trailing value lst)
  ; inverte la lista per trasformare i trailing in leading
  (let (rev (reverse lst))
    ; 'find' trova il primo elemento diverso da value nella lista invertita
    ; 'slice' rimuove i value iniziali
    ; reverse finale ripristina l'ordine originale
    (reverse (slice rev (find value rev !=)))))

Proviamo:

(trim-leading nil a)
;-> (1 4 2 nil 4 nil nil 6 7 nil nil nil nil)
(trim-trailing nil a)
;-> (nil nil 1 4 2 nil 4 nil nil 6 7)

(trim-leading 0 b)
;-> (1 4 2 0 4 0 0 6 7 0 0 0 0)
(trim-trailing 0 b)
;-> (0 0 1 4 2 0 4 0 0 6 7)


---------------------------------------------------
Numeri composti uguali alla somma dei fattori primi
---------------------------------------------------

Il numero 39 è un numero composto uguale alla somma dei suoi fattori primi:

  39 = 3 * 13 = 3 + 5 + 7 + 11 + 13

Sequenza OEIS A055233:
Composite numbers equal to the sum of the primes from their smallest prime factor to their largest prime factor.
  10, 39, 155, 371, 2935561623745, 454539357304421

Esempio: 10 = 2*5 = 2 + 3 + 5;

(define (primes-range n1 n2)
"Generate all prime numbers in the interval [n1..n2]"
  (if (> n1 n2) (swap n1 n2))
  (cond ((= n2 1) '())
        ((= n2 2) '(2))
        (true
          (let ((lst '(2)) (arr (array (+ n2 1))))
            ; initialize lst
            (if (> n1 2) (setq lst '()))
            (for (x 3 n2 2)
              (when (not (arr x))
                ; push current primes (x) only if > n1
                (if (>= x n1) (push x lst -1))
                (for (y (* x x) n2 (* 2 x) (> y n2))
                  (setf (arr y) true)))) lst))))

(primes-range 3 11)
;-> (3 5 7 11)

(primes-range 3 5)
;-> (3 5)

(define (curio num)
  (let (fat (factor num)) ; lista dei fattori del numero
    ; quando il numero non è primo...
    (when (> (length fat) 1)
      ; somma dei fattori: (apply + (primes-range (fat 0) (fat -1)))
      ; se num == somma dei fattori --> true (altrimenti nil)
      (if (= num (apply + (primes-range (fat 0) (fat -1)))) true nil))))

Proviamo:

(curio 39)
;-> true

(time (println (filter curio (sequence 1 1e4))))
;-> (10 39 155 371)
;-> 420.187

(time (println (filter curio (sequence 1 1e5))))
;-> (10 39 155 371)
;-> 34514.342

Vediamo un altro metodo: calcoliamo i numeri primi tutti in una volta.

Il limite superiore per il calcolo dei numeri primi fino a N vale (N/2 + 1).
Infatti serve:
  max(fattore primo) <= N/2
perché:
  se (p > N/2) --> (2p > N) --> impossibile

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

(define (seq N)
  (letn ( (out '())
          ; limite superiore per il calcolo dei numeri primi fino a N
          (primi (primes-to (+ (/ N 2)) 1))
          (len (length primi))
          (revprimi (reverse (copy primi)))
          (fat nil) (i1 nil) (i2 nil) )
    (for (num 2 N)
      ; lista dei fattori del numero
      (setq fat (factor num))
      ; quando il numero non è primo...
      (when (> (length fat) 1)
        ; ricerca dell'indice del primo fattore nella lista dei primi
        (setq i1 (find (fat 0) primi))
        ; ricerca dell'indice dell'ultimo fattore nella lista dei primi inversa
        (setq i2 (find (fat -1) revprimi))
        ; indice dell'ultimo fattore nella lista dei primi
        (setq i2 (- len i2 1))
        ; somma dei fattori: (apply + (slice primi i1 (- i2 i1 (- 1))))
        ; se num == somma dei fattori,
        ; allora inserisce num nella lista di output
        (if (= num (apply + (slice primi i1 (- i2 i1 (- 1)))))
            (push num out -1))))
    out))

Proviamo:

(time (println (seq 1e4)))
;-> (10 39 155 371)
;-> 69.224

(time (println (seq 1e5)))
;-> (10 39 155 371)
;-> 4824.975

Questa funzione è 7 volte più veloce, ma non possiamo calcolare i termini successivi della sequenza in un tempo ragionevole:
  2935561623745, 454539357304421
Però possiamo verificarli:

(time (println (curio 2935561623745)))
;-> true
;-> 1582.673

(time (println (curio 454539357304421)))
;-> true
;-> 24493.207


-----------------
Costante di Cahen
-----------------

La costante di Cahen è un numero trascendente che vale:

  C = 0.64341054628833802618225430775756...

Un numero trascendente è un numero reale o complesso che non è soluzione di alcuna equazione polinomiale a coefficienti interi (o razionali).

La costante viene definita come la somma di una serie infinita di frazioni unitarie, con segni alterni (partendo da +):

  C = 1 - 1/2 + 1/6 - 1/42 + 1/1806 - 1/3263442 + ...

Come si calcolano i denominatori?
I denominatori si determinano tramite i valori della sequenza di Sylvester.

Sequenza di Sylvester
Ogni termine è il prodotto di tutti i termini precedenti +1.
Definizione:
  s(0) = 2
  s(1) = s(0) + 1 = 3
  s(2) = s(0)*s(1) + 1 = 7
  s(3) = s(0)*s(1)*s(2) + 1 = 43
  ...
  s(n+1) = s(n)^2 - s(n) + 1

(define (sylvester N)
  (let (out '(2L))
    (setq cur 2L)
    (for (i 1 (- N 1))
      (setq cur (+ (* cur cur) (- cur) 1L))
      (push cur out -1))))

(sylvester 9)
;-> (2L 3L 7L 43L 1807L 3263443L 10650056950807L
;->  113423713055421844361000443L
;->  12864938683278671740537145998360961546653259485195807L)

La formula per i denominatori vale:

  denominatore(k) = sylvester(k) - 1

(setq denominatori (map -- (sylvester 9)))
;-> (1L 2L 6L 42L 1806L 3263442L 10650056950806L
;->  113423713055421844361000442L
;->  12864938683278671740537145998360961546653259485195806L)

Sommando a coppie i valori della costante otteniamo un risutato interessante:

  C = (1 - 1/2) + (1/6 - 1/42) + (1/1806 - 1/3263442) + ...

Sommiamo le coppie:

  1 - 1/2 = 1/2
  1/6 - 1/42 = 1/7
  1/1806 - 1/3263442 = 1/1807

Quindi possiamo scrivere:

  C = 1/2 + 1/7 + 1/1807 + ...

In questo caso i denominatori dei termini sono i valori con indice pari della sequenza di Sylvester:

  s(0) = 2, s(2) = 7, s(4) = 1807, ...

Sequenza OEIS A129871:
A variant of Sylvester's sequence: a(0)=1 and for n>0, a(n) = (a(0)*a(1)*...*a(n-1)) + 1.
  1, 2, 3, 7, 43, 1807, 3263443, 10650056950807,
  113423713055421844361000443,
  12864938683278671740537145998360961546653259485195807, ...

Scriviamo una funzione che calcola la costante di Cahen:

(div 10650056950807)
;-> 9.389621150563197e-014
(div 113423713055421844361000443L)
;-> 8.816498535111202e-027
(div 12864938683278671740537145998360961546653259485195807L)
;-> 7.773064641961799e-053

(setq C 0.64341054628833802618225430775756)
;-> 0.6434105462883381

; Funzione che calcola la costante di Cahen
; Restituisce una coppia di valori:
; (C-calcolata  (C-vera - C-calcolata))
(define (cahen N)
  (let ( (C 0.6434105462883381) (sylv '()) (calc 0) )
    (if (odd? N) (++ N)) ; solo valori pari per N
    ; lista di elementi con indice pari della sequenza di sylvester
    (setq sylv (select (sylvester N) (sequence 0 (- N 1) 2)))
    (setq calc (apply add (map div sylv)))
    (list calc (sub C calc))))

Proviamo:

(cahen 3)
;-> (0.6428571428571428 0.000553403431195254)
2 cifre corrette: 0.64...

(cahen 5)
;-> (0.643410546288244 9.403589018575076e-014)
12 cifre corrette: 0.643410546288...

(cahen 7)
;-> (0.6434105462883379 1.110223024625157e-016)
14 cifre corrette: 0.64341054628833...

(cahen 8)
;-> (0.6434105462883379 1.110223024625157e-016)
;-> (0.6434105462883379 1.110223024625157e-016)
14 cifre corrette: 0.64341054628833...
Abbiamo raggiunto il limite di precisione della funzione.


------------------------------
Trasformazione di numeri primi
------------------------------

Trovare i numeri primi P che rimangono primi quando si applica contemporaneamente la seguente serie di trasformazioni:

a) tutte le cifre "1" si trasformano in cifre "3"
b) tutte le cifre "3" si trasformano in cifre "7"
c) tutte le cifre "7" si trasformano in cifre "9"
d) tutte le cifre "9" si trasformano in cifre "1"
e) le altre cifre (0, 2, 4, 5, 6 e 8) rimangono invariate (cifre "inattive")

Dato un numero primo P, determinare tutti i numeri che si mantengono primi applicando continuamente le regole di trasformazione.

Esempi:
  numero primo = 19
  prima trasformazione: 31
  seconda trasformazione: 73
  terza trasformazione: 97
  quarta trasformazione: 19 (ciclo)
  lista di numeri: (19 31 73 97)

  numero primo = 131
  prima trasformazione: 373
  seconda trasformazione: 797
  terza trasformazione: 919
  quarta trasformazione: 131 (ciclo)
  lista di numeri: (131 373 797 919)

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

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

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

; Funzione che calcola le liste di primi per ogni numero primo da 1 a N
(define (carousel N)
  (local (primi out cur-seq new-num stop cifre new-cifre cifra new-num)
    ; lista delle soluzioni
    (setq out '())
    ; lista di primi
    (setq primi (primes-to N))
    ; ciclo per ogni primo...
    (dolist (p primi)
      ; lista di primi per il numero primo corrente
      (setq cur-seq (list p))
      ; nuovo numero
      (setq new-num p)
      ; flag per terminare il ciclo delle trasformazioni
      (setq stop nil)
      ; ciclo delle trasformazioni...
      (until stop
        ; converte il numero corrente in cifre
        (setq cifre (int-list new-num))
        ; Trasformazione del numero
        (setq new-cifre '())
        (dolist (c cifre)
          (if (= c 1) (setq cifra 3)
              (= c 3) (setq cifra 7)
              (= c 7) (setq cifra 9)
              (= c 9) (setq cifra 1)
              (setq cifra c))
          (push cifra new-cifre -1)
        )
        ; nuovo numero (numero corrente trasformato)
        (setq new-num (list-int new-cifre))
        ;(println "new-num: " new-num)
        ; controlla se il nuovo numero è primo e se è stato già calcolato
        (if (and (prime? new-num) (not (find new-num cur-seq)))
            (push new-num cur-seq -1)
            ; else
            (setq stop true))
      )
      ; inserisce il risultato del numero primo corrente
      ; nella lista delle soluzioni
      (push cur-seq out -1))
      out))

Proviamo:

(carousel 50)
;-> ((2) (3 7) (5) (7) (11) (13 37 79) (17) (19 31 73 97) (23) (29)
;->  (31 73 97 19) (37 79) (41 43 47) (43 47) (47))

Vediamo quanto vale la massima lunghezza delle liste di primi:

(apply max (map length (carousel 10000000)))
;-> 4

Vediamo la frequenza delle lunghezze delle liste di primi:

(count '(1 2 3 4) (map length (carousel 100000)))
;-> (7422 1349 485 336)


-----------------------------------------
Numeri con cifre in successione (+1 o -1)
-----------------------------------------

Esistono due tipi di numeri con cifre in successione:

1) Successione ascendente: cifra(i) + 1 = cifra(i+1)

Ordine delle cifre ascendenti:
  0 -> 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 -> 9 -> 0 -> 1 ...

Sequenza OEIS A059043:
Numbers in which each digit is the (immediate) successor of the previous one (if it exists) and 0 is considered the successor of 9.
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 23, 34, 45, 56, 67, 78, 89, 90, 123,
  234, 345, 456, 567, 678, 789, 890, 901, 1234, 2345, 3456, 4567, 5678,
  6789, 7890, 8901, 9012, 12345, 23456, 34567, 45678, 56789, 67890,
  78901, 89012, 90123, 123456, 234567, 345678, 456789, ...

2) Successione discendente: cifra(i) - 1 = cifra(i+1)

Ordine delle cifre discendenti:
  0 -> 9 -> 8 -> 7 -> 6 -> 5 -> 4 -> 3 -> 2 -> 1 -> 0 -> 9 ...

Sequenza OEIS A158699:
Start with 0, then add one to each single digit.
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 21, 32, 43, 54, 65, 76, 87, 98, 109,
  210, 321, 432, 543, 654, 765, 876, 987, 1098, 2109, 3210, 4321, 5432,
  6543, 7654, 8765, 9876, 10987, 21098, 32109, 43210, 54321, 65432, 76543,
  87654, 98765, 109876, 210987, 321098, 432109, 543210, ...

(define (int-list num)
"Convert an integer to a list of digits"
  (if (zero? num) '(0)
  (let (out '())
    (while (!= num 0)
      (push (% num 10) out)
      (setq num (/ num 10))) out)))

; Funzione che verifica se un numero ha tutte le cifre in successione ascendente (+1)
(define (digit-up? num)
  (if (< num 10) num
  ;else
  (let ( (len (length num))
         (cifre (int-list num))
         (stop nil) )
    (for (k 0 (- len 2) 1 stop)
      ; controllo coppie di cifre consecutive
      (cond ((= (cifre k) 9)
              (if-not (= (cifre (+ k 1)) 0) (setq stop true)))
            (true
              (if-not (= (cifre k) (- (cifre (+ k 1)) 1)) (setq stop true)))))
    (not stop))))

; Funzione che verifica se un numero ha tutte le cifre in successione discendente (-1)
(define (digit-down? num)
  (if (< num 10) num
  ;else
  (let ( (len (length num))
         (cifre (int-list num))
         (stop nil) )
    (for (k 0 (- len 2) 1 stop)
      ; controllo coppie di cifre consecutive
      (cond ((= (cifre k) 0)
              (if-not (= (cifre (+ k 1)) 9) (setq stop true)))
            (true
              (if-not (= (cifre k) (+ (cifre (+ k 1)) 1)) (setq stop true)))))
    (not stop))))

Proviamo:

(filter digit-up? (sequence 0 456789))
;-> (0 1 2 3 4 5 6 7 8 9 12 23 34 45 56 67 78 89 90 123
;->  234 345 456 567 678 789 890 901 1234 2345 3456 4567 5678
;->  6789 7890 8901 9012 12345 23456 34567 45678 56789 67890
;->  78901 89012 90123 123456 234567 345678 456789)

(filter digit-down? (sequence 0 543210))
;-> (0 1 2 3 4 5 6 7 8 9 10 21 32 43 54 65 76 87 98 109
;->  210 321 432 543 654 765 876 987 1098 2109 3210 4321 5432
;->  6543 7654 8765 9876 10987 21098 32109 43210 54321 65432 76543
;->  87654 98765 109876 210987 321098 432109 543210)

Vediamo quali sono i numeri primi.

Sequenza OEIS A006055:
Primes with consecutive (ascending) digits.
  2, 3, 5, 7, 23, 67, 89, 4567, 78901, 678901, 23456789, 45678901,
  9012345678901, 789012345678901, 56789012345678901234567890123,
  90123456789012345678901234567, 678901234567890123456789012345678901, ...

Sequenza OEIS A120804:
Primes with consecutive digits descending.
  2, 3, 5, 7, 43, 109, 10987, 76543, 10987654321098765432109876543210987, 4321098765432109876543210987654321098765432109876543210987654321, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(filter prime? (filter digit-up? (sequence 0 456789)))
;-> (2 3 5 7 23 67 89 4567 78901)

(filter prime? (filter digit-down? (sequence 0 543210)))
(2 3 5 7 43 109 10987 76543)

(digit-up? 4321098765432109876543210987654321098765432109876543210987654321)
;-> nil
(digit-down? 4321098765432109876543210987654321098765432109876543210987654321)
;-> true

(digit-up? 10987654321098765432109876543210987)
;-> nil
(digit-down? 10987654321098765432109876543210987)
;-> true


-------------------------
Primo + somma delle cifre
-------------------------

Iniziamo con un numero primo P, quindi aggiungiamo a P la somma delle cifre di P (SOD).
Prima sequenza:
Se il nuovo numero è primo, ripetiamo la procedura fino a ottenere un numero composto.
Seconda sequenza:
Se il nuovo numero è composto, ripetiamo la procedura fino a ottenere un numero primo.

Esempi:
Sequenza 1
  N = 277
  Output = (277 293 307 317 328)

Sequenza 2
  N = 2
  Output = (2 4 8 16 23)

(define (digit-sum num)
"Calculate the sum of the digits of an integer"
  (let (out 0)
    (while (!= num 0)
      (setq out (+ out (% num 10)))
      (setq num (/ num 10)))
    out))

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

; Funzione che calcola il valore della sequenza 1 per un dato numero primo
(define (seq1 primo)
  (let ( (out (list primo))
         (stop nil)
         (new-num primo) )
    (until stop
      ; calcolo del nuovo numero
      (setq new-num (+ new-num (digit-sum new-num)))
      ; se abbiamo già incontrato il nuovo numero,
      ; allora fermiamo il ciclo
      (if (find new-num out) (setq stop true))
      (push new-num out -1)
      ; se nuovo numero non è primo,
      ; allora fermiamo il ciclo
      (if-not (prime? new-num) (setq stop true)))
    out))

(seq1 277)
;-> (277 293 307 317 328)

; Funzione che calcola il valore della sequenza 2 per un dato numero primo
(define (seq2 primo)
  (let ( (out (list primo))
         (stop nil)
         (new-num primo) )
    (until stop
      ; calcolo del nuovo numero
      (setq new-num (+ new-num (digit-sum new-num)))
      ; se abbiamo già incontrato il nuovo numero,
      ; allora fermiamo il ciclo
      (if (find new-num out) (setq stop true))
      (push new-num out -1)
      ; se nuovo numero è primo,
      ; allora fermiamo il ciclo
      (if (prime? new-num) (setq stop true)))
    out))

(seq2 2)
;-> (2 4 8 16 23)

Per il numero primo 3, la sequenza 2 sembra non terminare mai perchè ogni numero generato è divisibile per 3.

(setq new-num 3)
(setq out (list new-num))
(setq stop nil)
(for (i 1 20)
  (setq new-num (+ new-num (digit-sum new-num)))
      (if (find new-num out) (setq stop true))
      (push new-num out -1)
      (if (prime? new-num) (setq stop true)))
out

;-> (3 6 12 15 21 24 30 33 39 51 57 69 84 96 111 114 120 123 129 141 147)
I numeri della lista di output sono tutti divisibili per 3:

(filter (fn(x) (!= (% x 3) 0)) out)
;-> ()

Vediamo quanto vale il numero primo che genera la lista più lunga:

(define (seq1-max N)
  (local (primo max-list max-len cur-list cur-len)
    (setq primo (primes-to N))
    (setq max-list '())
    (setq max-len 0)
    (dolist (p primo)
      (setq cur-list (seq1 p))
      (setq cur-len (length cur-list))
      ;(print cur-list) (read-line)
      (when (> cur-len max-len)
        (setq max-list cur-list)
        (setq max-len cur-len)))
    (list max-len max-list)))

(time (println (seq1-max 1e7)))
;-> (7 (516493 516521 516541 516563 516589 516623 516646))
;-> 4078.722
Il numero 516493 genera la lista con lunghezza massima (7).

(define (seq2-max N)
  (local (primo max-list max-len cur-list cur-len)
    (setq primo (primes-to N))
    (println "...")
    ; rimuove il numero primo 3
    (pop primo 1)
    (setq max-list '())
    (setq max-len 0)
    (dolist (p primo)
      (setq cur-list (seq2 p))
      (setq cur-len (length cur-list))
      ;(print cur-list) (read-line)
      (when (> cur-len max-len)
        (setq max-list cur-list)
        (setq max-len cur-len)))
    (list max-len max-list)))

(time (println (seq2-max 1e7)))
;-> (111 (8047399 8047439 8047474 8047508 8047540 8047568 8047606 8047637
;->       8047672 8047706 8047738 8047775 8047813 8047844 8047879 8047922
;->       8047954 8047991 8048029 8048060 8048086 8048120 8048143 8048171
;->       ...
;->       8050546 8050574 8050603 8050625 8050651 8050676 8050708 8050736
;->       8050765 8050796 8050831))
;-> 21970.61
Il numero 8047399 genera la lista con lunghezza massima (111).


---------------------------------
Corda e arco di una circonferenza
---------------------------------

Abbiamo un cerchio di centro C=(x0,y0) e raggio R.
Dati due punti A e B sul perimetro della circonferenza quali sono le formule per calcolare:
1) la lunghezza dell'arco delimitato dai punti A e B?
2) la superficie dell'area delimitata dall'arco AB e dalla corda AB
3) la superficie dell'area delimitata dall'arco AB e dai segmenti AC e BC
4) la distanza del centro C dalla corda AB

Vedi figura "corda-arco.png" nella cartella "data".

Dati:
 C = (x0, y0) centro
 R = raggio
 A = (x1, y1)
 B = (x2, y2)

Angolo al centro theta (in radianti)
------------------------------------
Metodo del prodotto scalare:
  u = (x1 - x0, y1 - y0)
  v = (x2 - x0, y2 - y0)
  dot = (x1-x0)*(x2-x0) + (y1-y0)*(y2-y0)
  cos(theta) = dot / (R^2)
  theta = arccos(dot / (R^2))
  (con 0 <= theta <= pi per l'arco minore)

Se vogliamo l'angolo dell'arco maggiore:
  theta_mag = 2*pi - theta

Lunghezza dell'arco AB
----------------------
  Arco minore:
  L = R * theta
  Arco maggiore:
  L = R * (2*pi - theta)

Area delimitata da arco AB e corda AB (segmento circolare)
----------------------------------------------------------
Formula standard (theta in radianti):
  Area = (R^2 / 2) * (theta - sin(theta))
equivalentemente:
  Area = (R^2 * theta)/2 - (R^2 * sin(theta))/2

Formule utili
-------------
Lunghezza corda:
  d = sqrt((x2-x1)^2 + (y2-y1)^2)

Angolo dal lato della corda:
  theta = 2 * arcsin(d / (2*R))

che possiamo usare direttamente nelle formule sopra.

Distanza tra il centro C e la corda AB
--------------------------------------
Formula geometrica
Nel triangolo isoscele ACB:
  d = 2 R sin(theta/2)
La distanza del centro dalla corda (chiamiamola h) è l'altezza:
  h = R cos(theta/2)
Quindi:
  h = R cos(theta/2)

Solo in funzione della corda d:
Dal triangolo rettangolo:
  (R)^2 = h^2 + (d/2)^2
quindi:
  h = sqrt(R^2 - (d/2)^2)

Direttamente dalle coordinate (formula punto-retta):
Equazione della retta AB.
Distanza punto-retta:
h = abs((x2-x1)*(y1-y0) - (y2-y1)*(x1-x0)) / sqrt((x2-x1)^2 + (y2-y1)^2)

Riassunto pratico
-----------------
Se conosci R e theta:
  h = R cos(theta/2)
Se conosci R e d:
  h = sqrt(R^2 - d^2/4)
Se conosciamo solo coordinate: usiamo la formula punto-retta sopra.

Area del settore circolare
--------------------------
Il settore circolare è delimitato da:
- arco AB
- raggio AC
- raggio BC
cioè il settore di angolo centrale theta.

Formula principale (theta in radianti):
  Area_settore = (R^2 * theta) / 2

Se theta è in gradi:
  Area_settore = (theta / 360) * pi * R^2

Se conosciamo solo la lunghezza dell'arco L:
Poiché
  L = R * theta
allora
  theta = L / R
e quindi
  Area_settore = (R * L) / 2

Se abbiamo solo le coordinate dei punti
vettori
  u = (x1-x0, y1-y0)
  v = (x2-x0, y2-y0)
angolo
  theta = arccos( (u·v) / (R^2) )
area
  Area_settore = (R^2 * theta) / 2

Nota geometrica:
  Settore = segmento + triangolo
cioè
  Area_settore = (R^2/2)(theta - sin(theta)) + (R^2/2) sin(theta)
che torna a
  Area_settore = (R^2 * theta)/2


--------------
Numeri di Jeff
--------------

https://www.primepuzzles.net/puzzles/puzz_210.htm

Un numero è di Jeff (Jeff Heleen) se le cifre nei suoi fattori primi (k > 1) sono le stesse del numero stesso.

Definiamo la seguente sequenza:
sia H(k) il numero composto più piccolo tale che le cifre nei suoi fattori primi (k > 1) siano le stesse del numero stesso, senza aggiunte.

Ad esempio, il numero più piccolo per k=2 che abbia questa proprietà è 1255 = 5*251.
I valori di H(k) per k = 2,...,9 sono:

  k  H(n)                  Fattorizzazione
  2  1255                  5*251
  3  163797                3*71*769
  4  11937639              3*7*61*9319
  5  1037715385            5*7*7*83*51031
  6  117295838975          5*5*7*89*821*9173
  7  11099654778737        7*7*7*7*89*541*96013
  8  1091778783077899      7*7*7*7*7*809*8191*9803
  9  1023976197718878397   7*7*7*7*61*89*971*983*82301

(define (same-digits? num1 num2)
"Check if two numbers have the same digits"
  (= (sort (explode (string num1))) (sort (explode (string num2)))))

; Funzione che verifica se un dato numero è di Jeff
(define (jeff? num)
  (let (fattori (factor num))
    (if (> (length fattori) 1)
        (same-digits? num (join (map string (factor num))))
        nil)))

(jeff? 11)
;-> nil

(jeff? 1255)
;-> true

(jeff? 11937639)
;-> true

(time (println (filter jeff? (sequence 1 1e6))))
;-> (1255 12955 17482 25105 100255 101299 105295 107329 117067 124483 127417 129595 132565
;->  145273 146137 149782 163797 174082 174298 174793 174982 250105 256315 263155 295105
;->  297463 307183 325615 371893 536539 687919)
;-> 4625.416

; Funzione che determina il numero H(k) (dato k e il limite massimo M)
(define (jeff k N)
  (setq sol nil)
  (setq stop nil)
  (for (num 2 N 1 stop)
    (setq fattori (factor num))
    (when (and (= (length fattori) k)
               (same-digits? num (join (map string fattori))))
      (setq sol (list num fattori))
      (setq stop true)))
  sol)

(time (println (jeff 2 1e8)))
;-> (1255 (5 251))
;-> 0
(time (println (jeff 3 1e8)))
;-> (163797 (3 71 769))
;-> 312.417
(time (println (jeff 4 1e8)))
;-> (11937639 (3 7 61 9319))
;-> 36518.974


------------------------------------
Numeri ppn (picture-perfect numbers)
------------------------------------

I numeri ppn sono i numeri interi positivi n in cui l'inversione di n è uguale alla somma delle inversioni dei divisori propri di n:

  reverse(n) = Sum[i=1..k]reverse(divisor(i))
  dove k sono il numero di divisori propri di n

Sequenza OEIS A069942:
Reversal of n equals the sum of the reversals of the proper divisors of n.
  6, 10311, 21661371, 1460501511, 7980062073, 79862699373, 798006269373

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

; Funzione che verifica se un numero è ppn
(define (ppn? num)
  (let (divisori (divisors num))
    (pop divisori -1)
    (= (int (reverse (string num)) 0 10)
       (apply + (map (fn(x) (int (reverse (string x)) 0 10)) divisori)))))

Proviamo:

(ppn? 8)
;-> nil
(ppn? 10311)
;-> true
(ppn? 7980062073)
;-> true

Limiti della funzione 'divisors':

(time (map divisors (sequence 1 1e6)))
;-> 10298.166
(time (map divisors (sequence 1 1e7)))
;-> 130476.072

(time (println (filter ppn? (sequence 1 1e5))))
;-> (6 10311)
;-> 1468.705


--------------------------------------------------
Somme delle coppie di cifre speculari di un numero
--------------------------------------------------

Determinare i numeri che si ottengono sommando le coppie di cifre speculari di un numero intero N.
Abbiamo due modi di calcolare le coppie:

1) Solo coppie uniche
Esempio:
  N = 38421
  Coppie di cifre speculari:
  (3)842(1) --> 3 + 1 = 4
  3(8)4(2)1 --> 8 + 2 = 10
  38(4)21   --> 4 + 4 = 8
  Output: (4 10 8)

2) Tutte le coppie speculari
  N = 38421
  Coppie di cifre speculari:
  3  8  4  2  1
  +  +  +  +  +
  1  2  4  8  3
  =============
  4 10  8 10  4
  Output: (4 10 8 10 4)

In formule:

  Lista S = (a(0) + a(k-1), a(1) + a(k-2), ...)
  con s(i) = a(i) + a(k-1-i),
  dove k è la lunghezza del numero
  per i = 0,...,(k - 1)/2, metà intervallo -> solo coppie uniche (e centro)
  per i = 0,...,(k - 1), intervallo completo -> tutte le posizioni speculari

(define (int-list num)
"Convert an integer to a list of digits"
  (if (zero? num) '(0)
  (let (out '())
    (while (!= num 0)
      (push (% num 10) out)
      (setq num (/ num 10))) out)))

; Funzione che calcola le somme delle coppie di cifre speculari di un numero
(define (sum-digit-pairs num)
  (let ( (out '()) (k (length num)) (d (int-list num)) )
    (for (i 0 (/ (- k 1) 2))
      (push (+ (d i) (d (- k 1 i))) out -1))
    out))

(sum-digit-pairs 38421)
;-> (4 10 8)

(sum-digit-pairs 12345)
(sum-digit-pairs 1234)
(sum-digit-pairs 2)
(sum-digit-pairs 67382)
(sum-digit-pairs 6738345623463452)
(sum-digit-pairs 111111111143333333333)

(define (sum-digit-pairs-all num)
  (letn ( (out '()) (d (int-list num)) (k (length d)) )
    (for (i 0 (- k 1))
      (push (+ (d i) (d (- k 1 i))) out -1))
    out))

(sum-digit-pairs-all 38421)
;-> (4 10 8 10 4)

Quindi:
- (k-1)/2: metà intervallo -> solo coppie uniche (più l'eventuale centro)
- (k-1): intervallo completo -> tutte le posizioni speculari


------------------------------------
Coppie di numeri primi complementari
------------------------------------

Due numeri (A, B) si dicono "coppia di numeri primi complementari" (Digit Complementary Prime Pair - DCPP) se:

a) A e B sono primi
b) la somma delle cifre nelle posizioni corrispondenti è "10" o "0"
c) A è l'inverso di B (e viceversa)

Esempio: condizioni a) e b)
  A = 4721
  B = 6389

Esempio: condizioni a), b) e c)
  A = 3467
  B = 7643

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

(define (primes-range n1 n2)
"Generate all prime numbers in the interval [n1..n2]"
  (if (> n1 n2) (swap n1 n2))
  (cond ((= n2 1) '())
        ((= n2 2) '(2))
        (true
          (let ((lst '(2)) (arr (array (+ n2 1))))
            ; initialize lst
            (if (> n1 2) (setq lst '()))
            (for (x 3 n2 2)
              (when (not (arr x))
                ; push current primes (x) only if > n1
                (if (>= x n1) (push x lst -1))
                (for (y (* x x) n2 (* 2 x) (> y n2))
                  (setf (arr y) true)))) lst))))

Per rispettare le condizioni i numeri A e B devono avere lo stesso numero di cifre.
Quindi scriviamo una funzione per calcolare le DCCP che prende come parametro il numero di cifre (k).

; Funzione che trova le coppie dcpp con numeri di k cifre che rispettano le condizioni a), b) e c)
(define (dcpp k)
  (local (out start end primi inv digits1 digits2)
    (setq out '())
    (setq start (pow 10 (- k 1)))
    (setq end (- (pow 10 k) 1))
    (println (time (setq primi (primes-range start end))))
    (setq primi '(19))
    (dolist (p primi)
      (setq inv (int (reverse (string p)) 0 10))
      (setq digits1 (int-list p))
      (setq digits2 (int-list inv))
      (if (and (for-all (fn(x) (or (= x 0) (= x 10))) (map + digits1 digits2))
              (prime? inv))
          (push (list p inv) out -1)))
    out))

Proviamo:

(dcpp 2))
;-> 0
;-> ((37 73) (73 37))

(dcpp 3)
;-> 0
;-> ()

(dcpp 4)
;-> 0
;-> ((1009 9001) (1559 9551) (3467 7643) (3917 7193) (7193 3917)
;->  (7643 3467) (9001 1009) (9551 1559))

(dcpp 5)
;-> 15.586
;-> ((10009 90001) (90001 10009))

(dcpp 6)
;-> 140.753
;-> ((107309 903701) (130079 970031) (306407 704603) (309107 701903)
;->  (701903 309107) (704603 306407) (903701 107309) (970031 130079))

(time (println (dcpp 7)))
;-> 1624.95
;-> ((1050509 9050501) (1235789 9875321) (1285289 9825821) (1375379 9735731)
;->  (1395179 9715931) (1445669 9665441) (1735739 9375371) (1915919 9195191)
;->  (1985219 9125891) (3395177 7715933) (3825827 7285283) (7285283 3825827)
;->  (7715933 3395177) (9050501 1050509) (9125891 1985219) (9195191 1915919)
;->  (9375371 1735739) (9665441 1445669) (9715931 1395179) (9735731 1375379)
;->  (9825821 1285289) (9875321 1235789))
;-> 3796.961

(time (println (dcpp 8)))
;-> 18423.549
;-> ((10064009 90046001) (10200809 90800201) (10600409 90400601)
;->  (11246899 99864211) (11328799 99782311) (11346799 99764311)
;->  (12364789 98746321) (12991189 98119921) (13282879 97828231)
;->  ...
;->  (97828231 13282879) (98119921 12991189) (98746321 12364789)
;->  (99764311 11346799) (99782311 11328799) (99864211 11246899))
;-> 38617.42


-----------
Palinpoints
-----------

https://jlpe.tripod.com/ppn/ppnpaper.htm

Nel paragrafo "7. Palinpoints of arithmetical functions" dell'articolo "Picture-Perfect Numbers and Other Digit-Reversal Diversions" Joseph L. Pe definisce un'interessante equazione funzionale:

  f(r(x)) = r(f(x))
  dove: x è un numero intero
        f(x) è una data funzione aritmetica di x, ed è essa stessa un intero
        r(x) indica l'inverso numerico dell'intero x

I "Palinpoints" sono tutti i valori di x che risolvono l'equazione per una data funzione specifica f(x).

Esempio:
Sia f(x)=isqrt(x), dove isqrt(x) è la radice quadrata intera di x.
Quindi l'equazione diventa:

  isqrt(r(x)) = r(isqrt(x))

Il numero 154 è un 'palinpoint':
  x = 154
  r(x) = 451
  isqrt(r(154)) = isqrt(451) = 21
  r(isqrt(154) = r(12) = 21

Palinpoints:
  1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 67 76 77 88 89 98 99 100
  121 131 141 144 154 164 169 179 189 400 441 451 461 484 494
  505 515 525 900 961 971 981 ...

Scriviamo una funzione che calcola i 'palinpoints'.

(define (pe func x)
  (let (rx (int (reverse (string x)) 0 10))
    (= (func rx) (int (reverse (string (func x))) 0 10))))

1) Prendiamo la funzione f(x) = isqrt(x):

(define (isqrt x) (int (sqrt x)))

(filter (fn(x) (pe isqrt x)) (sequence 1 981))
;-> (1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 67 76 77 88 89 98 99 100
;->  121 131 141 144 154 164 169 179 189 400 441 451 461 484 494
;->  505 515 525 900 961 971 981)

2) Prendiamo la funzione f(x) = x^2:

(define (square x) (* x x))

Esempio:
  x = 31
  r(x) = 13
  square(r(31)) = square(13) = 169
  r(square 31) = r(961) = 169

(filter (fn(x) (pe square x)) (sequence 1 981))
;-> (1 2 3 10 11 12 13 20 21 22 30 31 100 101 102 103 110 111 112 113
;->  120 121 122 130 200 201 202 210 211 212 220 221 300 301 310 311)

Sequenza OEIS A061909:
Skinny numbers: numbers n such that there are no carries when n is squared by "long multiplication".
  0, 1, 2, 3, 10, 11, 12, 13, 20, 21, 22, 30, 31, 100, 101, 102, 103, 110,
  111, 112, 113, 120, 121, 122, 130, 200, 201, 202, 210, 211, 212, 220, 221,
  300, 301, 310, 311, 1000, 1001, 1002, 1003, 1010, 1011, 1012, 1013, 1020,
  1021, 1022, 1030, 1031, 1100, 1101, 1102, ...

3) Prendiamo con la funzione f(x) = prime(x):

Esempio:
  x = 12
  r(x) = 21
  primo(r(12)) = primo(21) = 73
  r(primo 12) = r(37) = 73

(define (primo x) ((primes-to 1000) (- x 1)))
(filter (fn(x) (pe primo x)) (sequence 1 100))
;-> (1 2 3 4 5 12 21)

Sequenza OEIS A069469:
Numbers k such that prime(reversal(k)) = reversal(prime(k)). Ignore leading 0's.
  1, 2, 3, 4, 5, 12, 21, 8114118, 535252535, ...


-------------
Primi gemelli
-------------

Due numeri sono primi gemelli se n e (n + 2) sono entrambi primi.
Le coppie di primi gemelli sono infinite, ma la loro frequenza diminuisce con l'aumentare di n.

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

; Funzione che calcola i primi gemelli nell'intervallo [n1,n2]
(define (prime-pairs n1 n2)
"Generate pairs of twin primes in the interval [n1..n2]"
  (let ((out (list)) (x nil) (FX (* 2 3 5 7 11 13)) (M 0))
    (for (y (if (odd? n1) n1 (++ n1)) n2 2)
      (if (if (< y FX) (1 (factor y))
             (or (= (setf M (% y FX))) (if (factor M) (<= ($it 0) 13)) (1 (factor y))))
        (setf y nil)
        x (push (list x y) out -1))
      (setf x y))
    out))

(prime-pairs 2 100)
;-> ((3 5) (5 7) (11 13) (17 19) (29 31) (41 43) (59 61) (71 73))

; Funzione che calcola i primi gemelli fino ad un dato intero N
(define (gemelli N)
  (local (out primi len)
    (setq out '())
    (println (time (setq primi (primes-to N))))
    ; numero di primi
    (setq len (length primi))
    ; lista delle differenze tra coppie di primi adiacenti
    (setq diff (map - (rest primi) (chop primi)))
    ; trasforma la lista in vettore (indicizzazione più veloce)
    (setq primi (array len primi))
    ; ciclo sulla lista delle differenze
    (dolist (d diff)
      ; se la differenza corrente vale 2,
      ; allora recupera dal vettore i relativi gemelli
      ; e li inserisce nella soluzione
      (if (= d 2) (push (list (primi $idx) (primi (+ $idx 1))) out -1)))
    out))

Proviamo:

(gemelli 100)
;-> ((3 5) (5 7) (11 13) (17 19) (29 31) (41 43) (59 61) (71 73))
(time (gemelli2 1e7))

Test di correttezza:

(= (prime-pairs 2 1e6) (gemelli 1e6))
;-> true

Test di velocità:

(time (prime-pairs 2 1e6))
;-> 442.998
(time (gemelli 1e6))
;-> 156.206

(time (prime-pairs 2 1e7))
;-> 7813.043
(time (gemelli 1e7))
;-> 1765.791

Vedi anche "Coppie di primi gemelli" su "Rosetta code".


----------------
Terzine di primi
----------------

Una terzina (terna) di primi è una lista di tre numeri primi della forma (p, p + 2, p + 6) o (p, p + 4, p + 6).
Le prime terzine di primi sono (sequenza A098420 dell'OEIS):

  (5, 7, 11), (7, 11, 13), (11, 13, 17), (13, 17, 19), (17, 19, 23),
  (37, 41, 43), (41, 43, 47), (67, 71, 73), (97, 101, 103), (101, 103, 107),
  (103, 107, 109), (107, 109, 113), (191, 193, 197), (193, 197, 199),
  (223, 227, 229), (227, 229, 233), (277, 281, 283), (307, 311, 313),
  (311, 313, 317), (347, 349, 353), (457, 461, 463), (461, 463, 467),
  (613, 617, 619), (641, 643, 647), (821, 823, 827), (823, 827, 829),
  (853, 857, 859), (857, 859, 863), (877, 881, 883), (881, 883, 887), ...

Sequenza OEIS A098420:
Members of prime triples (p,q,r) with p < q < r = p + 6.
  5, 7, 11, 13, 17, 19, 23, 37, 41, 43, 47, 67, 71, 73, 97, 101, 103, 107,
  109, 113, 191, 193, 197, 199, 223, 227, 229, 233, 277, 281, 283, 307,
  311, 313, 317, 347, 349, 353, 457, 461, 463, 467, 613, 617, 619, 641,
  643, 647, 821, 823, 827, 829, 853, 857, 859, 863, ...

Scrivere una funzione che calcola le terzine di primi fino ad un dato intero N.

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

(define (terne-primi N)
  (local (out primi len pp)
    (setq out '())
    (println (time (setq primi (primes-to N))))
    (setq len (length primi))
    (setq pp (array len primi))
    (for (k 0 (- len 1))
      (setq p (pp k))
      ; (println p { } (+ p 2) { } (+ p 6))
      ; (print p { } (+ p 4) { } (+ p 6)) (read-line)
      (if (and (find (+ p 2) primi) (find (+ p 6) primi))
          (push (list p (+ p 2) (+ p 6)) out -1))
      (if (and (find (+ p 4) primi) (find (+ p 6) primi))
          (push (list p (+ p 4) (+ p 6)) out -1)))
    out))

(terne-primi 1e3)
;-> ((5 7 11) (7 11 13) (11 13 17) (13 17 19) (17 19 23) (37 41 43)
;->  (41 43 47) (67 71 73) (97 101 103) (101 103 107) (103 107 109)
;->  (107 109 113) (191 193 197) (193 197 199) (223 227 229) (227 229 233)
;->  (277 281 283) (307 311 313) (311 313 317) (347 349 353) (457 461 463)
;->  (461 463 467) (613 617 619) (641 643 647) (821 823 827) (823 827 829)
;->  (853 857 859) (857 859 863) (877 881 883) (881 883 887))

Calcoliamo i numeri della sequenza:

(sort (unique (flat (terne-primi 1e3))))
;-> (5 7 11 13 17 19 23 37 41 43 47 67 71 73 97 101 103 107
;->  109 113 191 193 197 199 223 227 229 233 277 281 283 307
;->  311 313 317 347 349 353 457 461 463 467 613 617 619 641
;->  643 647 821 823 827 829 853 857 859 863 877 881 883 887)

Le seguenti sequenze riportano solo il primo numero delle terzine:

(p, p+2, p+6)
Sequenza OEIS A022004:  5, 11, 17, 41, 101, 107, ...

(p, p+2, p+8)
Sequenza OEIS A046134:  3, 5, 11, 29, 59, 71, 101, ...

(p, p+2, p+12)
Sequenza OEIS A046135:  5, 11, 17, 29, 41, 59, 71, ...

(p, p+4, p+6)
Sequenza OEIS A022005:  7, 13, 37, 67, 97, 103, ...

(p, p+4, p+10)
Sequenza OEIS A046136:  3, 7, 13, 19, 37, 43, 79, ...

(p, p+4, p+12)
Sequenza OEIS A046137:  7, 19, 67, 97, 127, 229, ...

(p, p+6, p+8)
Sequenza OEIS A046138:  5, 11, 23, 53, 101, 131, ...

(p, p+6, p+10)
Sequenza OEIS A046139:  7, 13, 31, 37, 61, 73, 97, ...

(p, p+6, p+12)
Sequenza OEIS A023241:  5, 7, 11, 17, 31, 41, 47, ...

(p, p+8, p+12)
Sequenza OEIS A046141:  5, 11, 29, 59, 71, 89, 101, ...

(define (seq-terne a b N)
  (local (out primi len pp)
    (setq out '())
    (println (time (setq primi (primes-to N))))
    (setq len (length primi))
    (setq pp (array len primi))
    (for (k 0 (- len 1))
      (setq p (pp k))
      ; (println p { } (+ p a) { } (+ p b))
      (if (and (find (+ p a) primi) (find (+ p b) primi))
          ;(push (list p (+ p a) (+ p b)) out -1)))
          (push p out -1)))
    (sort (unique (flat out)))))

Proviamo:

(seq-terne 2 6 250)
;-> (5 11 17 41 101 107 191 227)
(seq-terne 2 8 250)
;-> (3 5 11 29 59 71 101 149 191)
(seq-terne 2 12 250)
;-> (5 11 17 29 41 59 71 101 137 179 227)
(seq-terne 4 6 250)
;-> (7 13 37 67 97 103 193 223)
(seq-terne 4 10 250)
;-> (3 7 13 19 37 43 79 97 103 127 163 223 229)
(seq-terne 4 12 250)
;-> (7 19 67 97 127 229)
(seq-terne 6 8 250)
;-> (5 11 23 53 101 131 173 191 233)
(seq-terne 6 10 250)
;-> (7 13 31 37 61 73 97 103 157 223)
(seq-terne 6 12 250)
;-> (5 7 11 17 31 41 47 61 67 97 101 151 167 227)
(seq-terne 8 12 250)
;-> (5 11 29 59 71 89 101)


-----------------------------
Sequenza "Ulisse" James Joice
-----------------------------

Sequenza OEIS A002488:
a(n) = n^(n^n)
  -1, 0, 1, 16, 7625597484987, ...

Sequenza OEIS A054382:
James Joyce's "Ulysses" sequence: number of digits in n^(n^n).
1, 1, 2, 13, 155, 2185, 36306, 695975, 15151336, 369693100, 10000000001, 297121486765, 9622088391635, 337385711567665, 12735782555419983, 515003176870815368, 22212093154093428530, 1017876887958723919835, 49390464231494436119285

(define (pow-i num power)
"Calculate the integer power of an integer"
  (local (pot out)
    (if (zero? power)
        (setq out 1L)
        (begin
          (setq pot (pow-i num (/ power 2)))
          (if (odd? power)
              (setq out (* num pot pot))
              (setq out (* pot pot)))
        )
    )
    out))

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(define (nnn1 num)
  (let (power (pow-i num num))
    (pow-i num power)))

(define (nnn2 num)
  (let (power (** num num))
    (** num power)))

(length (nnn1 5L))
;-> 2185
(length (nnn2 5))
;-> 2185

(time (println (length (nnn1 6L))))
;-> 36306
;-> 9.525
(time (println (length (nnn2 6L))))
;-> 36306
;-> 210.131

La funzione 'pow-i' è molto più veloce della funzione '**' soprattutto per numeri molto grandi

(time (println (length (nnn1 7L))))
;-> 695975
;-> 3505.075

(map nnn1 '(0L 1L 2L 3L))
;-> (0L 1L 16L 7625597484987L)

(time (println (map (fn(x) (length (nnn1 x))) '(0L 1L 2L 3L 4L 5L 6L 7L))))
;-> (1 1 2 13 155 2185 36306 695975)
;-> 3519.984


------------------
Gelfand's question
------------------

Consideriamo la cifra decimale più a sinistra (ovvero la più significativa) dei numeri 2^n, 3^n, ..., 9^n.
Quali sono le sequenze di cifre che compaiono nella tabella per n = 1,2,...

   n  OEIS     2^n 3^n 4^n 5^n 6^n 7^n 8^n 9^n
   1  A000027   2   3   4   5   6   7   8   9
   2  A002993   4   9   1   2   3   4   6   8
   3  A002994   8   2   6   1   2   3   5   7
   4  A097408   1   8   2   6   1   2   4   6
   5  A097409   3   2   1   3   7   1   3   5
   6  A097410   6   7   4   1   4   1   2   5
   7  A097411   1   2   1   7   2   8   2   4
   8  A097412   2   6   6   3   1   5   1   4
   9  A097413   5   1   2   1   1   4   1   3
  10  A097414   1   5   1   9   6   2   1   3

Sequenza OEIS A000027: The positive integers. Also called the natural numbers, the whole numbers or the counting numbers, but these terms are ambiguous.
Sequenza OEIS A002993: Initial digits of squares.
Sequenza OEIS A002994: Initial digit of cubes.
Sequenza OEIS A097408: Initial decimal digit of n^4.
Sequenza OEIS A097409: Initial decimal digit of n^5.
Sequenza OEIS A097410: Initial decimal digit of n^6.
Sequenza OEIS A097411: Initial decimal digit of n^7.
Sequenza OEIS A097412: Initial decimal digit of n^8.
Sequenza OEIS A097413: Initial decimal digit of n^9.
Sequenza OEIS A097414: Initial decimal digit of n^10.

(define (first-digit num)
  (if (zero? num) 0
    ;else
    ;(/ num (pow 10 (- (length num) 1)))) ; slower
    ; (- (length num) 1) = (int (log num 10))
    (/ num (pow 10 (int (log num 10))))))

(first-digit 214)
;-> 2
(first-digit 9223372036854775807)
;-> 9
(first-digit 92233720368547758070000)
;-> 9L

(define (seq n max-base)
  (map first-digit (map (fn(x) (pow x n)) (sequence 2 max-base))))

(map (fn(n) (seq n 9)) (sequence 1 10))
;-> ((2 3 4 5 6 7 8 9)
;->  (4 9 1 2 3 4 6 8)
;->  (8 2 6 1 2 3 5 7)
;->  (1 8 2 6 1 2 4 6)
;->  (3 2 1 3 7 1 3 5)
;->  (6 7 4 1 4 1 2 5)
;->  (1 2 1 7 2 8 2 4)
;->  (2 6 6 3 1 5 1 4)
;->  (5 1 2 1 1 4 1 3)
;->  (1 5 1 9 6 2 1 3))


----------------------------------------
Complessità di un intero (Mahler-Popken)
----------------------------------------

La complessità di un intero n è il minimo numero di 1 necessari per rappresentarlo utilizzando solo addizioni, moltiplicazioni e parentesi. Questo non consente la giustapposizione di 1 per formare interi più grandi, quindi, ad esempio, 2 = 1+1 ha complessità 2, ma 11 no (incollare insieme due 1 non è un'operazione consentita).

Sequenza OEIS A005245:
The (Mahler-Popken) complexity of n: minimal number of 1's required to build n using + and *.
  1, 2, 3, 4, 5, 5, 6, 6, 6, 7, 8, 7, 8, 8, 8, 8, 9, 8, 9, 9, 9, 10, 11,
  9, 10, 10, 9, 10, 11, 10, 11, 10, 11, 11, 11, 10, 11, 11, 11, 11, 12,
  11, 12, 12, 11, 12, 13, 11, 12, 12, 12, 12, 13, 11, 12, 12, 12, 13, 14,
  12, 13, 13, 12, 12, 13, 13, 14, 13, 14, 13, 14, 12, 13, 13, 13, 13, 14,
  13, 14, ...

   n.........minimal expression...........a(n) = number of 1's
   1..................1...................1
   2.................1+1..................2
   3................1+1+1.................3
   4.............(1+1)*(1+1)..............4
   5............(1+1)*(1+1)+1.............5
   6............(1+1)*(1+1+1).............5
   7...........(1+1)*(1+1+1)+1............6
   8..........(1+1)*(1+1)*(1+1)...........6
   9...........(1+1+1)*(1+1+1)............6
  10..........(1+1+1)*(1+1+1)+1...........7
  11.........(1+1+1)*(1+1+1)+1+1..........8
  12.........(1+1)*(1+1)*(1+1+1)..........7
  13........(1+1)*(1+1)*(1+1+1)+1.........8
  14.......[(1+1)*(1+1+1)+1]*(1+1)........8
  15.......[(1+1)*(1+1)+1]*(1+1+1)........8
  16.......(1+1)*(1+1)*(1+1)*(1+1)........8
  17......(1+1)*(1+1)*(1+1)*(1+1)+1.......9
  18........(1+1)*(1+1+1)*(1+1+1).........8
  19.......(1+1)*(1+1+1)*(1+1+1)+1........9
  20......[(1+1+1)*(1+1+1)+1]*(1+1).......9
  21......[(1+1)*(1+1+1)+1]*(1+1+1).......9
  22.....[(1+1)*(1+1+1)+1]*(1+1+1)+1.....10
  23....[(1+1)*(1+1+1)+1]*(1+1+1)+1+1....11
  24......(1+1)*(1+1)*(1+1)*(1+1+1).......9
  25.....(1+1)*(1+1)*(1+1)*(1+1+1)+1.....10
  26....[(1+1)*(1+1)*(1+1+1)+1]*(1+1)....10
  27.......(1+1+1)*(1+1+1)*(1+1+1)........9
  28......(1+1+1)*(1+1+1)*(1+1+1)+1......10
  29.....(1+1+1)*(1+1+1)*(1+1+1)+1+1.....11
  30.....[(1+1+1)*(1+1+1)+1]*(1+1+1).....10
  31....[(1+1+1)*(1+1+1)+1]*(1+1+1)+1....11
  32....(1+1)*(1+1)*(1+1)*(1+1)*(1+1)....10
  33...(1+1)*(1+1)*(1+1)*(1+1)*(1+1)+1...11
  34..[(1+1)*(1+1)*(1+1)*(1+1)+1]*(1+1)..11
...

Usiamo la programmazione dinamica bottom-up.
Relazione della programmazione dinamica bottom-up:

  C(n) = min [C(n-1) + 1, min(C(a) + C(b))]
    (a*b=n, a,b>=2)

cioè:

  Addizione --> n = (n-1) + 1
  Moltiplicazione --> n = a * b

Poiché tutti i valori usati sono minori di n, possiamo riempire la tabella in ordine crescente.

Quindi per ogni 'n', la complessità è il minimo tra:
1. Addizione: c(n) = c(n-1) + 1, si aggiunge un 1
2. Moltiplicazione: c(n) = c(a) + c(b) per ogni fattorizzazione a * b = n con a, b >= 2
Possiamo esplorare solo 'a' fino a 'sqrt(n) perchè se a * b = n e (a <= b), vale (a <= sqrt(n)), evitando duplicati (complessità O(N*sqrt(N)).

; Complessità di un intero (Mahler-Popken)
; complexity(n) = minimo numero di 1 necessari per costruire n
;                 usando solo + e * (e parentesi)
; Algoritmo: programmazione dinamica bottom-up.
; Per ogni n, la complessità è il minimo tra:
; 1) complexity(n-1) + 1          (addizione di 1)
; 2) complexity(a) + complexity(b) per ogni a*b = n, con a,b >= 2

; Funzione che costruisce la tabella delle complessità da 1 a limit.
(define (complexity-table limit)
  (let ((c (array (+ limit 1) (list 999999))))
    (setf (c 1) 1)
    (for (n 2 limit)
      ; Caso 1: n = (n-1) + 1
      (setf (c n) (+ (c (- n 1)) 1))
      ; Caso 2: n = a * b  per ogni fattorizzazione non banale
      (for (a 2 (int (sqrt n)))
        (when (= (% n a) 0)
          (let ((b (/ n a)))
            (let ((costo (+ (c a) (c b))))
              (when (< costo (c n))
                (setf (c n) costo)))))))
    c))

(complexity-table 10)
;-> (999999 1 2 3 4 5 5 6 6 6 7)

; Funzione che restituisce la complessità intera (costo) di un numero positivo intero
(define (complexity num)
  ((complexity-table num) num))

(complexity 10)
;-> 7

Se vogliamo restituire anche l'espressione di 1 della complessità di un numeri dobbiamo fare alcune modifiche:

1) 'complexity-tables: ora mantiene due vettori paralleli:
'cost' per il numero di 1, e 'expr' per l'espressione testuale corrispondente.

2) 'parenthesize': aggiunge parentesi attorno a un'espressione solo se contiene +, così la precedenza della * viene rispettata correttamente (es. (1+1)*... ma 1+1+1 non ne ha bisogno come fattore).

3) 'complexity-expr': restituisce una lista (costo espressione) invece del solo numero.

; Complessità di un intero (Mahler-Popken)
; complexity(n) = minimo numero di 1 necessari per costruire n
;                 usando solo + e * (e parentesi)
; Algoritmo: programmazione dinamica bottom-up.
; Per ogni n, la complessità è il minimo tra:
; 1) complexity(n-1) + 1          (addizione di 1)
; 2) complexity(a) + complexity(b) per ogni a*b = n, con a,b >= 2

; Aggiunge parentesi attorno a expr se contiene '+' (priorità della *)
(define (parenthesize expr)
  (if (find "+" expr)
    (append "(" expr ")")
    expr))

(parenthesize "1+1")
;-> "(1+1)"
(parenthesize "1*1")
;-> "1*1"

; Costruisce le tabelle 'cost' ed 'expr' da 1 fino a limit.
; Restituisce una lista (costo expr) da due vettori.
(define (complexity-tables limit)
  (let ((cost (array (+ limit 1) (list 999999)))
        (expr (array (+ limit 1) (list ""))))
    ; caso base
    (setf (cost 1) 1)
    (setf (expr 1) "1")
    (for (n 2 limit)
      ; --- caso 1: addizione n = (n-1) + 1 ---
      (setf (cost n) (+ (cost (- n 1)) 1))
      (setf (expr n) (append (expr (- n 1)) "+1"))
      ; --- caso 2: moltiplicazione n = a * b ---
      (for (a 2 (int (sqrt n)))
        (when (= (% n a) 0)
          (letn ((b (/ n a))
                 (c (+ (cost a) (cost b))))
            (when (< c (cost n))
              (setf (cost n) c)
              (setf (expr n)
                (append (parenthesize (expr a))
                        "*"
                        (parenthesize (expr b))))))))
    )
    (list cost expr)))

(complexity-tables 10)
;-> ((999999 1 2 3 4 5 5 6 6 6 7)
;->  ("" "1" "1+1" "1+1+1" "1+1+1+1" "1+1+1+1+1" "(1+1)*(1+1+1)"
;->   "(1+1)*(1+1+1)+1" "(1+1)*(1+1+1+1)" "(1+1+1)*(1+1+1)"
;->   "(1+1+1)*(1+1+1)+1"))

; Funzione che restituisce la complessità intera (costo espressione) di un numero positivo intero
(define (complexity-expr num)
  (letn ((tables (complexity-tables num))
         (cost (tables 0))
         (expr (tables 1)))
    (list (cost num) (expr num))))

(complexity-expr 22)
;-> (10 "(1+1+1)*((1+1)*(1+1+1)+1)+1")

(complexity-expr 101)
;-> (15 "(1+1)*((1+1)*((1+1)*((1+1)*((1+1)*(1+1+1)))+1))+1")

Nota:
la parte additiva:
  (setf (cost n) (+ (cost (- n 1)) 1))
  (setf (expr n) (append (expr (- n 1)) "+1"))
è corretta ma non sempre minima.
Infatti la formula teorica completa è:

  C(n) = min (C(k)+C(n-k))
       (1<=k<n)

ma nella pratica si usa solo (n-1) perché quasi sempre è il migliore caso additivo.

; Funzione che stampa tutti risultati fino a un dato intero N
(define (print-table N)
  (let (table (complexity-tables N))
    (for (k 1 N)
      (println k { } (table 0 k) { } (table 1 k))) '>))

(print-table 34)
;-> 1 1 1
;-> 2 2 1+1
;-> 3 3 1+1+1
;-> 4 4 1+1+1+1
;-> 5 5 1+1+1+1+1
;-> 6 5 (1+1)*(1+1+1)
;-> 7 6 (1+1)*(1+1+1)+1
;-> 8 6 (1+1)*(1+1+1+1)
;-> 9 6 (1+1+1)*(1+1+1)
;-> 10 7 (1+1+1)*(1+1+1)+1
;-> 11 8 (1+1+1)*(1+1+1)+1+1
;-> 12 7 (1+1)*((1+1)*(1+1+1))
;-> 13 8 (1+1)*((1+1)*(1+1+1))+1
;-> 14 8 (1+1)*((1+1)*(1+1+1)+1)
;-> 15 8 (1+1+1)*(1+1+1+1+1)
;-> 16 8 (1+1)*((1+1)*(1+1+1+1))
;-> 17 9 (1+1)*((1+1)*(1+1+1+1))+1
;-> 18 8 (1+1)*((1+1+1)*(1+1+1))
;-> 19 9 (1+1)*((1+1+1)*(1+1+1))+1
;-> 20 9 (1+1)*((1+1+1)*(1+1+1)+1)
;-> 21 9 (1+1+1)*((1+1)*(1+1+1)+1)
;-> 22 10 (1+1+1)*((1+1)*(1+1+1)+1)+1
;-> 23 11 (1+1+1)*((1+1)*(1+1+1)+1)+1+1
;-> 24 9 (1+1)*((1+1)*((1+1)*(1+1+1)))
;-> 25 10 (1+1)*((1+1)*((1+1)*(1+1+1)))+1
;-> 26 10 (1+1)*((1+1)*((1+1)*(1+1+1))+1)
;-> 27 9 (1+1+1)*((1+1+1)*(1+1+1))
;-> 28 10 (1+1+1)*((1+1+1)*(1+1+1))+1
;-> 29 11 (1+1+1)*((1+1+1)*(1+1+1))+1+1
;-> 30 10 (1+1)*((1+1+1)*(1+1+1+1+1))
;-> 31 11 (1+1)*((1+1+1)*(1+1+1+1+1))+1
;-> 32 10 (1+1)*((1+1)*((1+1)*(1+1+1+1)))
;-> 33 11 (1+1)*((1+1)*((1+1)*(1+1+1+1)))+1
;-> 34 11 (1+1)*((1+1)*((1+1)*(1+1+1+1))+1)

Sequenza OEIS A005520:
Smallest number of complexity n: smallest number requiring n 1's to build using + and *.
  1, 2, 3, 4, 5, 7, 10, 11, 17, 22, 23, 41, 47, 59, 89, 107, 167, 179,
  263, 347, 467, 683, 719, 1223, 1438, 1439, 2879, 3767, 4283, 6299,
  10079, 11807, 15287, 21599, 33599, 45197, 56039, 81647, 98999, 163259,
  203999, 241883, 371447, 540539, 590399, 907199, ...


------------------
Numeri di Friedman
------------------

Un numero di Friedman è un numero che può essere rappresentato con un'espressione che utilizza solo le cifre del numero.
Inoltre, l'espressione può includere +, -, ×, ÷, esponenti e parentesi, ma nient'altro.
Ad esempio, 25 è un numero di Friedman perché può essere rappresentato come 5^2.
Scrivi un programma in newLISP che verifica se un intero positivo N è un numero di Friedman e restituisce la relativa espressione.

Sequenza OEIS A036057:
Friedman numbers: can be written in a nontrivial way using their digits and the operations + - * / ^ and concatenation of digits (but not of results).
  25, 121, 125, 126, 127, 128, 153, 216, 289, 343, 347, 625, 688, 736,
  1022, 1024, 1206, 1255, 1260, 1285, 1296, 1395, 1435, 1503, 1530, 1792,
  1827, 2048, 2187, 2349, 2500, 2501, 2502, 2503, 2504, 2505, 2506, 2507,
  2508, 2509, 2592, 2737, 2916, 3125, 3159, ...

Un numero di Friedman 'simpatico' è un numero di Friedman in cui le cifre si presentano nell'espressione nello stesso ordine in cui si trovano nel numero originale.
Quindi, 343 è un numero di Friedman simpatico, perché può essere rappresentato da un'espressione con le cifre 3, 4 e 3 nello stesso ordine:

  343 = (3 + 4)^3

I primi sette numeri di Friedman simpatici sono 127, 343, 736, 1285, 2187, 2502, 2592.

Algoritmo (forza bruta intelligente)
------------------------------------
1. digits: estrae le cifre del numero (es. 343 -> (3 4 3))
2. permutations: genera tutte le permutazioni delle cifre
3. multi-digit-splits: per ogni permutazione, genera tutte le possibili tokenizzazioni: le cifre possono essere raggruppate in numeri multi-cifra (es. (3 4 3) -> (3 43), (34 3), ecc.), escludendo zeri iniziali.
4. build-expressions: data una sequenza di token, costruisce ricorsivamente tutte le espressioni binarie possibili (come gli alberi di parsing di una grammatica)
5. combine: applica tutti gli operatori (+, -, *, /, ^) con controlli di sicurezza (divisione intera, potenze non negative, limite per evitare overflow)

;---------------------------------------------------------
; Genera tutte le permutazioni degli elementi di una lista
;---------------------------------------------------------
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
              (swap (lst (indici i)) (lst i)))
            (push lst out -1)
            (++ (indici i))
            (setq i 0))
          (begin
            (setf (indici i) 0)
            (++ i)
          )))
    out))

;----------------------------------------------------------
; Estrae le cifre di un numero intero e le restituisce
; come lista di interi.
; Esempio: 343 -> (3 4 3)
;----------------------------------------------------------
(define (digits n)
  ; converte il numero in stringa, esplode in caratteri
  ; e riconverte ogni carattere in intero
  (map (fn (c) (int (string c))) (explode (string n))))

;----------------------------------------------------------
; Divisione sicura.
; Restituisce nil se:
;   - divisione per zero
;   - divisione non intera
;----------------------------------------------------------
(define (safe-div a b)
  ; controlla validità divisione
  (if (or (= b 0) (!= (mod a b) 0))
      nil
      (/ a b)))

;----------------------------------------------------------
; Potenza intera sicura.
; Limita gli esponenti per evitare overflow.
;----------------------------------------------------------
(define (safe-pow base expn)
  ; controlla casi non validi
  (cond
    ((< expn 0) nil)
    ((and (= base 0) (= expn 0)) nil)
    ((> expn 20) nil)
    ; calcolo della potenza
    (true
     (let ((result 1))
       (dotimes (i expn)
         (setq result (* result base)))
       result))))

;----------------------------------------------------------
; Combina due espressioni applicando tutti gli operatori.
; Ogni espressione è nella forma:
;   (valore stringa-espressione)
;----------------------------------------------------------
(define (combine expr1 expr2)
  ; estrae valore e stringa della prima espressione
  (letn ((v1 (first expr1))
         (s1 (last expr1))
         ; estrae valore e stringa della seconda espressione
         (v2 (first expr2))
         (s2 (last expr2))
         ; lista risultati
         (results '()))
    ; operazione di addizione
    (push (list (+ v1 v2) (string "(" s1 "+" s2 ")")) results)
    ; operazione di sottrazione
    (push (list (- v1 v2) (string "(" s1 "-" s2 ")")) results)
    ; operazione di moltiplicazione
    (push (list (* v1 v2) (string "(" s1 "*" s2 ")")) results)
    ; divisione sicura
    (let ((d (safe-div v1 v2)))
      (if d
          (push (list d (string "(" s1 "/" s2 ")")) results)))
    ; potenza sicura
    (let ((p (safe-pow v1 v2)))
      (if p
          (push (list p (string "(" s1 "^" s2 ")")) results)))
    results))

;----------------------------------------------------------
; Costruisce ricorsivamente tutte le espressioni possibili
; da una lista di numeri.
;----------------------------------------------------------
(define (build-expressions token-list)
  ; se c'è un solo numero l'espressione è il numero stesso
  (if (= (length token-list) 1)
      (list (list (first token-list) (string (first token-list))))
      ; altrimenti divide la lista in due parti
      (let ((results '()))
        (for (i 0 (- (length token-list) 2))
          ; divide lista in parte sinistra e destra
          (letn ((left  (slice token-list 0 (+ i 1)))
                 (right (slice token-list (+ i 1)))
                 ; genera tutte le espressioni per entrambe
                 (left-exprs  (build-expressions left))
                 (right-exprs (build-expressions right)))
            ; combina ogni espressione sinistra con ogni destra
            (dolist (le left-exprs)
              (dolist (re right-exprs)
                (dolist (c (combine le re))
                  (push c results))))))
        results)))

;----------------------------------------------------------
; Genera tutte le possibili suddivisioni delle cifre
; in numeri a più cifre.
; Esempio:
; (3 4 3) -> (3 4 3) (3 43) (34 3) (343)
;----------------------------------------------------------
(define (multi-digit-splits digit-list)
  ; caso base
  (if (null? digit-list)
      '(())
      ; lista risultati
      (let ((results '()))
        ; prova tutte le lunghezze del prefisso
        (for (len 1 (length digit-list))
          (letn ((pfx  (slice digit-list 0 len))
                 (rdl  (slice digit-list len))
                 ; costruzione numero dal prefisso
                 (num-str (apply string pfx))
                 (num     (int num-str 0 10)))
            ; evita numeri con zero iniziale
            (when (or (= len 1) (!= (first pfx) 0))
              ; continua con il resto della lista
              (dolist (sbsplit (multi-digit-splits rdl))
                (push (cons num sbsplit) results)))))
        results)))

;----------------------------------------------------------
; Rimuove eventuali parentesi esterne inutili
;----------------------------------------------------------
(define (clean-expr s)
  (if (and (> (length s) 2)
           (= (first s) "(")
           (= (last s) ")"))
      (slice s 1 (- (length s) 2))
      s))

;----------------------------------------------------------
; Verifica se un numero è un numero di Friedman.
; Restituisce l'espressione se trovata.
;----------------------------------------------------------
(define (is-friedman? n)
  ; estrae le cifre del numero
  (letn ((digs  (digits n))
         ; genera tutte le permutazioni delle cifre (senza duplicati)
         ;(perms (perm-unique digs)))
         ;(perms (unique (permutations digs)))
         (perms (unique (perm digs)))
         (found nil))
    ; ricerca con uscita anticipata
    (catch
      ; prova ogni permutazione
      (dolist (p perms)
        ; prova ogni possibile suddivisione delle cifre
        (dolist (split (multi-digit-splits p))
          ; evita il caso banale con un solo numero
          (when (> (length split) 1)
            ; genera tutte le espressioni
            (dolist (expr (build-expressions split))
              ; verifica se il valore coincide con n
              (when (= (first expr) n)
                (setq found (last expr))
                (throw (clean-expr found))))))))
    found))

Proviamo:

(is-friedman? 25)
;-> "(5^2)"

(is-friedman? 41665)
;-> "(641*65)"

(time (println (filter is-friedman? (sequence 1 2000))))
;-> (25 121 125 126 127 128 153 216 289 343 347 625 688 736
;->  1022 1024 1206 1255 1260 1285 1296 1395 1435 1503 1530 1792 1827)
;-> 20093.839

(time (println (filter is-friedman? (sequence 1 3159))))
;-> (25 121 125 126 127 128 153 216 289 343 347 625 688 736
;->  1022 1024 1206 1255 1260 1285 1296 1395 1435 1503 1530 1792
;->  1827 2048 2187 2349 2500 2501 2502 2503 2504 2505 2506 2507
;->  2508 2509 2592 2737 2916 3125 3159)
;-> 42023.818

Il programma funziona, ma è molto lento perché genera tutte le espressioni possibili.
Le tre parti più critiche sono:
1) permutazioni delle cifre
Se il numero ha cifre ripetute (es. 343, 1285, ecc.) l'algoritmo genera molte permutazioni identiche.
Si potrebbero generare solo le permutazioni univoche.
2) costruzione degli alberi di espressione
La funzione 'build-expressions' viene chiamata molte volte con gli stessi token.
Si potrebbe utilizzare una hash-table o una lista associativa per implementare una memoization dei risultati.
3) valori intermedi enormi
Molte espressioni producono numeri enormi che non potranno mai tornare al valore target.
Si potrebbero scartare subito utilizzando un valore massimo.


--------
x% di y%
--------

Quanto vale il 12% del 42% di un numero R?

Il 12% vale 12/100,
il 42% vale 42/100,
quindi il 12% del 42% vale:
  (12/100)*(42/100) = (12*42) / (100*100) = 504/10000 = 0.0504
Cioè il (0.0504 * 100) = 5.04% del numero R.

; Funzione che calcola la percentuale di un valore
(define (perc p val) (div (mul p val) 100))

Scriviamo una funzione generale che prende una lista con le percentuali ed un eventuale numero R:

  (perc1 perc2 perc3 ... percN R)

; Funzione che calcola le percentuali totali di un eventuale valore
(define (perc-mult lst)
  (let ( (R (pop lst -1))
         (perc-tot (apply mul (map (fn(x) (div x 100)) lst))) )
    (if R
        (list (mul 100 perc-tot) (mul R perc-tot))
        ;else
        (list (mul 100 perc-tot) R))))

Proviamo:

(perc-mult '(12 42 nil))
(perc-mult '(12 42 100))
(perc-mult '(12 42 120))

Quanto vale il 200% del 50% del 100% di 120?
(perc-mult '(200 50 100 120))

Quanto vale il 100% del 100% del 100% di 100?
(perc-mult '(50 50 50 100))

Quanto vale il 60% di 200?
(perc-mult '(60 200))
;-> (60 120)


---------------------------------------------
Numeri con somma delle cifre primo e divisore
---------------------------------------------

Determinare la sequenza dei numeri in cui la somma delle cifre è primo ed è divisore del numero.

Esempio:
  numero = 12
  somma-cifre = 3  -->  numero primo e divisore di 12

Sequenza OEIS A062713:
Numbers k such that the sum of the digits of k is a prime factor of k.
  2, 3, 5, 7, 12, 20, 21, 30, 50, 70, 102, 110, 111, 120, 133, 140, 200,
  201, 209, 210, 230, 247, 300, 308, 320, 322, 364, 407, 410, 476, 481,
  500, 506, 511, 605, 629, 700, 704, 715, 782, 803, 832, 874, 902, 935,
  1002, 1010, 1011, 1015, 1020, 1040, 1066, 1088, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (seq limit)
  (let (out '())
    (for (k 1 limit)
      (setq somma (apply + (map (fn (c) (int (string c))) (explode (string k)))))
      (if (and (zero? (% k somma)) (prime? somma))
          (push k out -1)))
    out))

(seq 1088)
;-> (2 3 5 7 12 20 21 30 50 70 102 110 111 120 133 140 200
;->  201 209 210 230 247 300 308 320 322 364 407 410 476 481
;->  500 506 511 605 629 700 704 715 782 803 832 874 902 935
;->  1002 1010 1011 1015 1020 1040 1066 1088)


-----------------------
Numeri printer's errors
-----------------------

Il numero 2592 è un numero 'printer's errors'.
Perché se una stampante provasse a comporre 2^59^2 e dimenticasse accidentalmente di elevare gli esponenti, non si verificherebbe alcun errore (2^59^2 --> 2^5*9^2 = 2592).

Numeri in cui inserendo in qualche modo le operazioni * e ^ tra le cifre è possibile ottenere un'espressione che valuta al numero stesso.

Sequenza OEIS A096298:
Numbers, not ending with 0, that are "printer's errors".
  2592, 34425, 312325, 492205, 3472875, 10744475, 13745725, 13942125,
  14569245, 14706125, 16746975, 19748225, 60466176, 189637632, 373156875,
  381358125, 514155276, 684204032, 1268929233, 1297080225, 1368408064,
  1527672265, ...

Esempi:
  2^5 9^2 = 2592
  3^4*425 = 34425
  31^2*325 = 312325
  49^2*205 = 492205
  1*7^3*7*72375 = 173772375

Sequenza OEIS A156322:
Integers n such that if you insert between each of their digits either "*" (times), "^" (exponentiation), or "nothing" (so that two or more digits are merged to form an integer), then you can recover n in a nontrivial way (however, two "^" mustn't be adjacent - you must avoid decompositions containing a^b^c).
  2592, 34425, 35721, 312325, 344250, 357210, 492205, 1492992, 1729665,
  1769472, 3123250, 3365793, 3442500, 3472875, 3572100, 3639168, 4922050,
  6718464, 6967296, 7587328, 10744475, 10756480, 13745725, 13942125,
  14569245, 16746975, 17266392, 17296650, 17577728, 17694720, ...

Quindi dobbiamo verificare se un numero può essere espresso come prodotto di termini dove ogni termine è o una sequenza di cifre consecutive o una base elevata a un esponente (entrambi formati da cifre consecutive del numero).

Algoritmo
---------
1) 'partitions': genera tutte le 2^(n-1) partizioni delle cifre in segmenti contigui
2) 'build-tokens': per ogni partizione esplora ricorsivamente due opzioni:
      Opzione A: chiudere il token corrente
      Opzione B: estendere il token corrente con ^
3) 'check-partition': filtra i risultati cercando un prodotto uguale a 'n' con almeno un '^' usato
4) Il punto di ingresso prova tutte le partizioni fermandosi alla prima soluzione trovata

; Verifica se un numero è un "printer's error number":
; le sue cifre possono essere partizionate in segmenti contigui
; dove ogni segmento è un intero oppure base^exp (con base ed exp
; formati da segmenti adiacenti), e il prodotto di tutti i termini
; vale n. Deve esserci almeno un ^.
; Restituisce nil oppure la stringa dell'espressione trovata.

(define (printer-error? n)
  (letn ((s      (string n))
         (digits (map int (explode s)))
         (len    (length digits)))
    ; --- Converte una lista di cifre in intero ---
    (define (digits-to-int ds)
      (let ((result 0))
        (dolist (d ds)
          (setq result (+ (* result 10) d)))
        result))
    ; --- Genera tutte le partizioni di lst in segmenti contigui non vuoti ---
    ; Es: (1 2 3) -> ((1)(2)(3)) ((1)(2 3)) ((1 2)(3)) ((1 2 3))
    (define (partitions lst)
      (if (null? lst)
          '(())
          (let ((result '()))
            (for (i 1 (length lst))
              (letn ((head (slice lst 0 i))
                     (tail (slice lst i)))
                (dolist (resto (partitions tail))
                  (push (cons head resto) result -1))))
            result)))
    ; --- Costruisce ricorsivamente tutti i modi di assegnare ^ tra segmenti ---
    ; parts      = lista di segmenti (es. ((2)(5)(9)(2)))
    ; idx        = indice del segmento corrente in esame
    ; current    = segmento o coppia (base exp) in costruzione
    ; has-exp    = true se current è già una coppia base^exp
    ; acc-vals   = valori numerici dei token chiusi finora
    ; acc-exprs  = stringhe dei token chiusi finora
    ; results    = lista accumulata di (vals exprs) completi
    ; Ogni token può essere:
    ;   - un intero  -> valore = digits-to-int, expr = "123"
    ;   - base^exp   -> valore = pow(base,exp),  expr = "2^5"
    (define (build-tokens parts idx current has-exp acc-vals acc-exprs results)
      (letn ((k        (length parts))
             (tok-val  (if has-exp
                           (pow (digits-to-int (first current))
                                (digits-to-int (last  current)))
                           (digits-to-int current)))
             (tok-expr (if has-exp
                           (string (digits-to-int (first current))
                                   "^"
                                   (digits-to-int (last  current)))
                           (string (digits-to-int current)))))
        (if (= idx k)
            ; Tutti i segmenti consumati: chiudi l'ultimo token e salva
            (when (and (> tok-val 0) (= tok-val (int tok-val)))
              (push (list (append acc-vals  (list (int tok-val)))
                          (append acc-exprs (list tok-expr)))
                    results -1))
            (begin
              ; Opzione A: chiudi il token corrente e inizia un nuovo token
              ;            col segmento idx
              (when (and (> tok-val 0) (= tok-val (int tok-val)))
                (setq results
                      (build-tokens parts
                                    (+ idx 1)
                                    (nth idx parts)
                                    nil
                                    (append acc-vals  (list (int tok-val)))
                                    (append acc-exprs (list tok-expr))
                                    results)))
              ; Opzione B: estendi il token corrente usando ^ con il segmento idx
              ;            (solo se non c'è già un esponente)
              (when (not has-exp)
                (setq results
                      (build-tokens parts
                                    (+ idx 1)
                                    (list current (nth idx parts))
                                    true
                                    acc-vals
                                    acc-exprs
                                    results)))))
        results))
    ; --- Controlla una singola partizione ---
    ; Una soluzione valida richiede:
    ;   - almeno un ^ (length vals < length parts)
    ;   - prodotto dei valori = n
    ; Restituisce la stringa dell'espressione o nil
    (define (check-partition parts)
      (letn ((token-lists (build-tokens parts 1 (nth 0 parts) nil '() '() '()))
             (found nil))
        (dolist (entry token-lists found)
          (letn ((vals  (first entry))
                 (exprs (last  entry)))
            (when (and (< (length vals) (length parts))
                       (= n (apply * vals)))
              (setq found (join exprs " * ")))))
        found))
    ; --- Punto di ingresso: prova tutte le partizioni ---
    (letn ((all-parts (partitions digits))
           (found nil))
      (dolist (p all-parts found)
        (letn ((res (check-partition p)))
          (when res (setq found res)))))))

Proviamo:

(printer-error? 2592)
;-> "2^5 * 9^2"

(printer-error? 35721)
;-> "3^5 * 7 * 21"
3^5 * 7 * 21 = 35721

(* (pow 3 5) 7 21)
(map printer-error? '(2592 34425 312325 492205 3472875 10744475 13745725 60466176))
;-> ("2^5 * 9^2" "3^4 * 425" "31^2 * 325" "49^2 * 205" "3^4 * 7^2 * 875"
;->  "1^0 * 7^4 * 4475" "1^3 * 7^4 * 5725" "6^4 * 6^6 * 1^76")

(map printer-error? '(1234 9999 2593 34426))
;-> (nil nil nil nil)

(filter printer-error? (sequence 1 1e4))
;-> (2592)

La funzione è molto lenta:
(time (println (filter printer-error? (sequence 1 1e5))))
;-> (2592 34425 35721)
;-> 42946.466


--------------------------
Stringhe "Number-Sum-Char"
--------------------------

Una stringa "Number-Sum-Char" di un numero intero N ha le seguenti proprietà:
1) è costituita da numeri in lettere la cui somma vale N.
2) la somma dei caratteri dei numeri in lettere vale N.

Esempi:

  N = 11
  Stringa NSC = "uno più tre più sette"
  1) 1 + 3 + 7 = 11
  2) (uno)3 + (tre)3 + (sette)5 = 11, cioè length("unotresette") = 11

  N = 21
  Stringa NSC = "quattro più quattro più tredici"
  1) 4 + 4 + 13 = 21
  2) length("quattroquattrotredici") = 21

; ---------------------------------------------------------
; Lista dei numeri e loro rappresentazione in lettere
; (0 "zero") ... (100 "cento") espandibile
; Ogni numero ha associata la sua stringa in italiano
; ---------------------------------------------------------
(setq names
'((0 "zero") (1 "uno") (2 "due") (3 "tre") (4 "quattro") (5 "cinque")
  (6 "sei") (7 "sette") (8 "otto") (9 "nove") (10 "dieci") (11 "undici")
  (12 "dodici") (13 "tredici") (14 "quattordici") (15 "quindici")
  (16 "sedici") (17 "diciassette") (18 "diciotto") (19 "diciannove")
  (20 "venti") (21 "ventuno") (22 "ventidue") (23 "ventitre")
  (24 "ventiquattro") (25 "venticinque") (26 "ventisei") (27 "ventisette")
  (28 "ventotto") (29 "ventinove") (30 "trenta") (31 "trentuno")
  (32 "trentadue") (33 "trentatre") (34 "trentaquattro") (35 "trentacinque")
  (36 "trentasei") (37 "trentasette") (38 "trentotto") (39 "trentanove")
  (40 "quaranta") (41 "quarantuno") (42 "quarantadue") (43 "quarantatre")
  (44 "quarantaquattro") (45 "quarantacinque") (46 "quarantasei")
  (47 "quarantasette") (48 "quarantotto") (49 "quarantanove")
  (50 "cinquanta") (51 "cinquantuno") (52 "cinquantadue") (53 "cinquantatre")
  (54 "cinquantaquattro") (55 "cinquantacinque") (56 "cinquantasei")
  (57 "cinquantasette") (58 "cinquantotto") (59 "cinquantanove")
  (60 "sessanta") (61 "sessantuno") (62 "sessantadue") (63 "sessantatre")
  (64 "sessantaquattro") (65 "sessantacinque") (66 "sessantasei")
  (67 "sessantasette") (68 "sessantotto") (69 "sessantanove") (70 "settanta")
  (71 "settantuno") (72 "settantadue") (73 "settantatre")
  (74 "settantaquattro") (75 "settantacinque") (76 "settantasei")
  (77 "settantasette") (78 "settantotto") (79 "settantanove") (80 "ottanta")
  (81 "ottantuno") (82 "ottantadue") (83 "ottantatre") (84 "ottantaquattro")
  (85 "ottantacinque") (86 "ottantasei") (87 "ottantasette") (88 "ottantotto")
  (89 "ottantanove") (90 "novanta") (91 "novantuno") (92 "novantadue")
  (93 "novantatre") (94 "novantaquattro") (95 "novantacinque")
  (96 "novantasei") (97 "novantasette") (98 "novantotto") (99 "novantanove")
  (100 "cento")))

; ---------------------------------------------------------
; Precalcolo della lista dei numeri con lunghezze e delta
; Ogni elemento di 'nums' contiene:
;   (n len delta)
;   - n     = valore numerico
;   - len   = lunghezza del numero in lettere (minuscole)
;   - delta = n - len
; Ordiniamo poi per delta crescente per ottimizzare il backtracking
; ---------------------------------------------------------
(setq nums '())
(dolist (p names)
  (letn ((n (p 0)) (l (length (lower-case (p 1)))))
    (if (> n 0)
      (push (list n l (- n l)) nums -1))))
(setq nums (sort nums (fn (a b) (< (a 2) (b 2)))))

; ---------------------------------------------------------
; Funzione: expr-string
; Dato un elenco di numeri, costruisce la stringa finale in lettere
; concatenando i nomi dei numeri con " più "
; ---------------------------------------------------------
(define (expr-string lst)
  (join (map (fn (x) (lower-case (lookup x names))) lst) " più "))

; ---------------------------------------------------------
; Funzione: expr-len
; Calcola la lunghezza della stringa finale di una lista di numeri
; Conta solo i caratteri dei numeri (esclude spazi e 'più')
; ---------------------------------------------------------
(define (expr-len lst)
  (let ((total 0))
    (dolist (x lst)
      (setq total (+ total (length (lower-case (lookup x names))))))
    total))

; ---------------------------------------------------------
; Funzione: find-NSC
; Ricerca combinazioni di numeri tali che:
;   - la somma dei numeri = target
;   - la lunghezza della stringa (dei soli numeri) = target
; Algoritmo:
;   - Ricorsione con backtracking (ottimizzato)
;   - 'lst' = numeri scelti finora
;   - 'sum' = somma corrente dei numeri
;   - 'len' = lunghezza corrente della stringa
;   - 'start' = indice corrente in 'nums' per evitare duplicati
; ---------------------------------------------------------
(define (find-NSC target)
  ; Lista risultato di tutte le combinazioni valide
  (let ((out '()))
    ; Funzione interna ricorsiva per il backtracking
    ; lst   = lista dei numeri scelti finora
    ; sum   = somma dei numeri scelti
    ; len   = somma delle lettere dei numeri scelti
    ; start = indice corrente in nums per evitare duplicati
    (define (searcha lst sum len start)
      ; Ciclo su tutti i numeri a partire da start
      (for (i start (- (length nums) 1))
        (letn ((n (nums i 0))   ; valore numerico corrente
               (ln (nums i 1))) ; lunghezza in lettere del numero
          ; Nuove variabili aggiornate per questo ramo della ricorsione
          (let ((new-lst (append lst (list n)))  ; aggiunge il numero corrente alla lista
                (new-sum (+ sum n))              ; aggiorna la somma dei numeri
                (new-len (+ len ln)))            ; aggiorna la somma delle lettere
            ; Potatura: se la somma dei numeri supera il target, non esplorare questo ramo
            (when (<= new-sum target)
              ; Potatura: se la somma delle lettere supera il target, non esplorare
              (when (<= new-len target)
                ; Se entrambe le condizioni sono esattamente uguali al target
                (if (and (= new-sum target) (= new-len target))
                    ; Combinazione valida -> aggiungi la stringa finale in out
                    (push (expr-string new-lst) out -1)
                    ; Altrimenti continua la ricorsione dal numero corrente
                    (searcha new-lst new-sum new-len i))))))))
    ; Avvio della ricerca con lista vuota, somma e lunghezza iniziali 0, indice 0
    (searcha '() 0 0 0)
    ; Restituisce la lista di combinazioni valide
    out))

Spiegazione:
1. Backtracking ricorsivo: esplora tutte le combinazioni possibili dei numeri memorizzati in 'nums'.
2. Pruning/potatura:
   - Se (sum > target) -> stop
   - Se (len > target) -> stop
3. Controllo combinazione valida:
   - sum = target
   - len = target (solo lettere dei numeri)
4. Start index: permette di riusare numeri, evitando duplicati e combinazioni già viste in ordine diverso.
5. (append lst (list n)): crea una nuova lista per il ramo ricorsivo, evitando di modificare 'lst' originale.
6. (push ... out -1): aggiunge la combinazione finale all’output in coda alla lista.

; ---------------------------------------------------------
; Funzione: find-NSC (senza commenti)
; Ricerca combinazioni di numeri tali che:
;   - la somma dei numeri = target
;   - la lunghezza della stringa = target
; ---------------------------------------------------------
(define (find-NSC target)
  (let ((out '()))
    (define (searcha lst sum len start)
      (for (i start (- (length nums) 1))
        (letn ((n (nums i 0)) (ln (nums i 1)))
          (let ((new-lst (append lst (list n)))
                (new-sum (+ sum n))
                (new-len (+ len ln)))
            (when (<= new-sum target)
              (when (<= new-len target)
                (if (and (= new-sum target) (= new-len target))
                    (push (expr-string new-lst) out -1)
                    (searcha new-lst new-sum new-len i))))))))
    (searcha '() 0 0 0)
    out))

Proviamo:

(find-NSC 3)
;-> "tre"
(find-NSC 8)
;-> ("uno più sette")
(find-NSC 10)
;-> ("quattro più sei" "uno più uno più otto")
(find-NSC 11)
;-> ("uno più tre più sette" "due più due più sette")
(find-NSC 21)
;-> ("quattro più quattro più tredici"
;->  "quattro più uno più uno più quindici"
;->  "quattro più uno più tre più tre più dieci"
;->  "quattro più uno più tre più sette più sei"
;->  "quattro più cinque più due più dieci"
;->  "quattro più due più due più tre più dieci"
;->  "quattro più due più due più sette più sei"
;->  "quattro più tre più quattordici"
;->  "uno più uno più uno più uno più uno più sedici"
;->  ...
;->  "cinque più due più due più tre più tre più sei"
;->  "due più due più due più due più due più undici"
;->  "due più due più due più tre più tre più tre più sei"
;->  "tre più tre più tre più tre più tre più tre più tre")

Test di velocità:

(time (println (length (find-NSC 35))))
;-> 515
;-> 1328.552
(time (println (length (find-NSC 40))))
;-> 1165
;-> 3780.109
(time (println (length (find-NSC 45))))
;-> 2627
;-> 10241.541

Test di correttezza:
; ---------------------------------------------------------
; Funzione: check-solution
; Verifica la correttezza delle soluzioni generate da 'find-NSC'
; ---------------------------------------------------------
; Lista di elementi: (numero-lettere numero)
(setq numbers (map (fn(x) (list (x 1) (x 0))) names))
(define (check-solution num lst)
  (let ( (str "") (somma 0) )
    ; Primo controllo: lunghezza caratteri dei numeri = num
    (dolist (el lst)
      (setq str (replace " più " el ""))
      (when (!= num (length str)) ; Errore
          (print num { } str { } (length str))))
    ; Secondo controllo: somma dei numeri in lettere = num
    (dolist (el lst)
      (setq numeri (parse (replace "più" el "")))
      (setq somma 0)
      (dolist (el numeri) (++ somma (lookup el numbers)))
      (when (!= num somma) ; Errore
          (print num { } str { } somma)))))

Proviamo:

(check-solution 11 (find-NSC 11))
;-> nil

(check-solution 35
  '("cinque più cinque più cinque più due più due più tre più sette più sei"))
;-> nil

(check-solution 35 (find-NSC 35))
;-> nil


-------------------------------------
Numeri primi e crivello di Eratostene
-------------------------------------

Crivello di Eratostene:
1. Si crea un array booleano di dimensione N+1, inizialmente tutto 'true'
2. Si marcano '0' e '1' come non-primi
3. Per ogni numero 'i' a partire da 2, se 'i' è ancora marcato come primo, si eliminano tutti i suoi multipli a partire da 'i*i'
4. Il ciclo esterno si ferma a 'sqrt(N)' (ottimizzazione chiave: oltre quella soglia i composti sono già stati eliminati)
5. Si raccolgono tutti gli indici ancora marcati 'true'

Versione 1
-----------
Ottimizzazioni applicate:
- Il ciclo esterno arriva solo fino a 'sqrt(N)' invece di 'N'
- I multipli partono da 'i*i' invece che da '2i' (quelli inferiori sono già stati eliminati da fattori precedenti)
- Uso di un array invece di una lista per accesso O(1)
- La raccolta finale scorre al contrario e usa 'push' per evitare 'append' ripetute
La complessità è O(N*log(log N)), che è quasi lineare e praticamente ottimale per questo problema.

(define (primi1 N)
  (if (< N 2)
    '()
    (let (sieve (array (+ N 1) (list true)))
      (setf (sieve 0) nil)
      (setf (sieve 1) nil)
      (let (i 2)
        (while (<= (* i i) N)
          (when (sieve i)
            (let (j (* i i))
              (while (<= j N)
                (setf (sieve j) nil)
                (inc j i))))
          (inc i)))
      (let (result '())
        (for (k N 2 -1)
          (when (sieve k)
            (push k result)))
        result))))

Proviamo:

(primi1 50)
;-> (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)

(time (println (length (primi1 1e6))))
;-> 78498
;-> 325.058

(time (println (length (primi1 1e7))))
;-> 664579
;-> 3712.06

Versione 2
----------
Ottimizzaioni applicate:
- Salto dei pari: (for (x 3 num 2)
Il ciclo esterno parte da 3 e avanza di 2, saltando tutti i numeri pari. La metà dei candidati viene ignorata a priori.
- Salto dei multipli pari: (for (y (* x x) num (* 2 x)
Il ciclo interno avanza di '2x' invece di 'x', saltando i multipli pari di ogni primo (già esclusi dal ciclo esterno). Dimezza le scritture sull'array.
- Array senza inizializzazione: (array (+ num 1))
Nessun 'true' come valore iniziale: l'array parte con 'nil' ovunque, e 'nil' significa "primo" (logica invertita). Si scrive 'true' solo sui composti, evitando di inizializzare N+1 celle.

(define (primi2 num)
  (cond ((= num 1) '())
        ((= num 2) '(2))
        (true
          (let ((lst '(2)) (arr (array (+ num 1))))
            (for (x 3 num 2)
              (when (nil? (arr x))
                (push x lst -1)
                (for (y (* x x) num (* 2 x) (> y num))
                  (setf (arr y) true)))) lst))))

Proviamo:

(primi2 50)
;-> (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)

(time (println (length (primi2 1e6))))
;-> 78498
;-> 140.176

(time (println (length (primi2 1e7))))
;-> 664579
;-> 1629.982

(time (println (length (primi2 1e8))))
;-> 5761455
;-> 18657.478

;;; Non provare !!!
;;; (time (println (length (primi2 1e9))))
La funzione 'primi2' può calcolare i primi fino 1e8, poi crasha il sistema (32 Gb RAM).

Versione 3
----------
Otimizzazioni applicate:
L'idea è comprimere il crivello memorizzando solo i numeri dispari.
Mapping semplice:
  indice i --> numero = 2*i + 3
quindi l'array rappresenta:
  3,5,7,9,11,13,...
La dimensione diventa circa N/2 invece di N, e tutti i multipli pari spariscono automaticamente.
Quando troviamo un primo 'p', il primo multiplo da eliminare è 'p*p'.
L'indice corrispondente è: ((p*p) - 3) / 2 e gli incrementi sono 'p'.

(define (primi3 N)
  (cond ((< N 2) '())
        ((< N 3) '(2))
        (true
          (letn ((m (/ (- N 3) 2))
                 (arr (array (+ m 1)))
                 (lim (/ (- (int (sqrt N)) 3) 2))
                 (lst '(2)))
            (for (i 0 lim)
              (when (nil? (arr i))
                (letn ((p (+ (* 2 i) 3))
                       (j (/ (- (* p p) 3) 2)))
                  (for (k j m p (> k m))
                    (setf (arr k) true)))))
            (for (i 0 m)
              (when (nil? (arr i))
                (push (+ (* 2 i) 3) lst -1)))
            lst))))

Proviamo:

(primi3 50)
;-> (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)

(time (println (length (primi3 1e6))))
;-> 78498
;-> 114.421

(time (println (length (primi3 1e7))))
;-> 664579
;-> 1379.886

(time (println (length (primi3 1e8))))
;-> 5761455
;-> 15500.004

Con questa versione possiamo calcolare i numeri primi fino a 1e9 (32 GB RAM):
(time (println (length (primi3 1e9))))
;-> 50847534
;-> 178960.989


------------------------------------
La funzione "sequence" per caratteri
------------------------------------

La funzione "sequence" genera solo sequenze di interi o float.

*********************
>>>funzione SEQUENCE
*********************
sintassi: (sequence num-start num-end [num-step])

Genera una sequenza di numeri da "num-start" a "num-end" con una dimensione passo facoltativa di "num-step".
Quando "num-step" viene omesso, si assume il valore 1 (uno).
I numeri generati sono di tipo intero (quando non è specificata alcuna dimensione del passo opzionale) o virgola mobile (quando è presente la dimensione del passo opzionale).

Scriviamo una funzione che genera sequnze di caratteri.

; Funzione che genera sequenze di caratteri
(define (sequence-char start end step)
  (let ( (a (char start)) (b (char end)) (k (or step 1)) )
    (map char (sequence a b k))))

Proviamo:

(sequence-char "A" "C")
;-> ("A" "B" "C")
(sequence-char "a" "z" 2)
;-> ("a" "c" "e" "g" "i" "k" "m" "o" "q" "s" "u" "w" "y")
(sequence-char "a" "z" -2)
;-> ("a" "c" "e" "g" "i" "k" "m" "o" "q" "s" "u" "w" "y")
(sequence-char "a" "Z")
;-> ("a" "`" "_" "^" "]" "\\" "[" "Z")
(join (sequence-char "a" "h"))
;-> "abcdefgh"


-----------------------------------------------------
Estrazione di elementi da una lista con un generatore
-----------------------------------------------------

Data una funzione e una lista, scrivere un programma che restituisce il primo elemento della lista per cui la funzione restituisce un valore vero, così come la lista senza quell'elemento.
Questo è utile in situazioni in cui si desidera consumare iterativamente gli elementi da una lista.

Utilizziamo un generatore al'interno di un contesto.

; Funzione che inizializza il generatore
(define (extract:init lst func)
  (setq extract:lst lst)
  (setq extract:func func))

; Funzione che restituisce:
; a) il primo elemento della lista per cui la funzione data restituisce vero,
; b) la lista senza quell'elemento
(define (extract:extract)
  (let ((stop nil) (val nil))
    (dolist (el extract:lst stop)
      (when (extract:func el) ; (func el) == true
        ; toglie dalla lista corrente solo l'elemento trovato
        ;(setq val (pop extract:lst $idx))
        ; toglie dalla lista corrente gli elementi dal primo
        ; fino all'elemento trovato compreso
        (setq val el)
        (setq extract:lst (slice extract:lst (+ $idx 1)))
        ; esce dal ciclo
        (setq stop true)))
    (if val (list val extract:lst) nil)))

; Funzione da passare al generatore
(define (f x) (odd? x))

Inizializziamo il generatore con una lista e la funzione (f x):
(extract:init '(1 2 3 4 5 11 7 2 21 2) f)

Estraiamo gli elementi:

(extract:extract)
;-> (1 (2 3 4 5 11 7 2 21 2))
(extract:extract)
;-> (3 (4 5 11 7 2 21 2))
(extract:extract)
;-> (5 (11 7 2 21 2))
(extract:extract)
;-> (11 (7 2 21 2))
(extract:extract)
;-> (7 (2 21 2))
(extract:extract)
;-> (21 (2))
(extract:extract)
;-> nil

Possiamo anche definire un 'alias' per la funzione (extract:extract):
(define _ext extract:extract)

(extract:init '(1 5 2 3) f)
(_ext)
;-> (1 (5 2 3))
(_ext)
;-> (5 (2 3))
(_ext)
;-> (3 ())
(_ext)
;-> nil

Vediamo quanti simboli esistono nel contesto 'extract':

(define (simboli contesto) (dotree (el contesto) (println el)) '>)
(simboli extract)
;-> extract:extract
;-> extract:func
;-> extract:init
;-> extract:lst


----------------
Prese elettriche
----------------

Un appartamento ha N prese elettriche a muro.
Inoltre abbiano M prese multiple che possono avere da 2 a K uscite.
Quanti apparecchi elettrici possiamo usare utilizzando tutte le prese?

Esempio:
 N = 1, una presa a muro
 M = 2 --> (2 3), 2 prese multiple con 2 e 3 ingressi
 Attacchiamo la presa multipla (2) a muro --> (+ 2)
 Attacchiamo la presa multipla (3) alla presa multipla (2) --> (- 1 + 3).
 Prese totali = 2 - 1 + 3 = 4

La formula è:

  Prese-Totali = N - M + Sum[i=1,M]S(i)

dove N = numero di prese a muro
     M = numero di prese multiple
     S = lista con i valori di ogni presa multipla (M valori)
     S(i) = numero di uscite della i-esima presa multipla

Spiegazione
-----------
All'inizio abbiamo N prese.
Una presa multipla occupa una presa ma ne crea S(i) nuove.
Quindi il guadagno netto della i-esima presa multipla vale:

  guadagno(i) = S(i) - 1

Dopo aver collegato tutte le M prese multiple:

  Totale = N + somma(S(i) - 1) per i = 1..M

cioè:

  Totale = N - M + Sum[i=1,M]S(i)

Una presa multipla puo essere collegata solo se esiste una presa libera.
Dopo aver collegato k prese multiple, perche il processo sia sempre possibile deve esserci sempre almeno una presa libera per collegare la successiva.
Inoltre, poichè risulta (S(i) >= 2), ogni presa multipla aumenta sempre il numero totale di prese.
Quindi se N >= 1 possiamo sempre collegare tutte le prese multiple e l'ordine non ha importanza.

; Funzione che calcola il numero totale di prese elettriche disponibili
(define (prese N S)
  (+ N (- (apply + S) (length S))))

Proviamo:

(prese 1 '(2 3))
;-> 4

(prese 4 '(3 3 3 3 2 2))
;-> 14


---------------------------------
Incontro tra robot in una griglia
---------------------------------

Due robot vengono posizionati su una griglia MxN.
I robot si muovono simultaneamente a caso in una cella adiacente (se possibile).
Ogni robot può spostarsi in una delle 8 celle adiacenti (N,E,S,O,NE,SE,SO,NO).
I robot non possono oltrepassare i bordi della griglia (cioè se lo spostamento casuale non è possibile, allora il robot rimane fermo).
Quando i robot si trovano nella stessa cella, allora il processo finisce.

; Funzione che stampa la griglia
(define (print-grid grid ch0 ch1 coord)
"Print a matrix with only digits (0..9)"
  (local (row col)
    (setq row (length grid))
    (setq col (length (first grid)))
    ; indici di colonna della griglia
    (if coord
        (println "  " (join (map (fn(x) (format "%2d" x)) (sequence 0 (- col 1))))))
    (for (i 0 (- row 1))
      ; indice di riga della griglia
      (if coord (print (format "%2d" i)))
      ; stampa della griglia
      (for (j 0 (- col 1))
        (if (and (!= ch0 "") (!= ch1 ""))
            (begin
              (cond ((= (grid i j) 0) (print ch0))
                    ((= (grid i j) 1) (print ch1))
                    (true
                      (print (format "%2d" (grid i j))))))
            ;else
            (print (format "%2d" (grid i j)))))
      (println))))

; Funzione che simula il processo di movimento dei due robot
(define (robot-meet M N p1 p2)
  (letn ((grid (array M N '(0)))
         (moves '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))
         (x1 (p1 0)) (y1 (p1 1))
         (x2 (p2 0)) (y2 (p2 1))
         (steps 0) (finito nil))
    (setf (grid x1 y1) 1)
    (setf (grid x2 y2) 1)
    (print-grid grid ". " "1 ")
    (read-line)
    (until finito
      (++ steps)
      ; muove robot 1
      (letn ((m1 (moves (rand 8)))
             (nx1 (+ x1 (m1 0)))
             (ny1 (+ y1 (m1 1))))
        (if (and (>= nx1 0) (< nx1 M) (>= ny1 0) (< ny1 N))
            (begin
              (setf (grid x1 y1) 0)
              (setf (grid nx1 ny1) 1)
              (setq x1 nx1) (setq y1 ny1))))
      ; muove robot 2
      (letn ((m2 (moves (rand 8)))
             (nx2 (+ x2 (m2 0)))
             (ny2 (+ y2 (m2 1))))
        (if (and (>= nx2 0) (< nx2 M) (>= ny2 0) (< ny2 N))
            (begin
              (setf (grid x2 y2) 0)
              (setf (grid nx2 ny2) 1)
              (setq x2 nx2) (setq y2 ny2))))
      ; stampa griglia corrente
      (print-grid grid ". " "1 ")
      (read-line)
      ; controllo collisione
      (if (and (= x1 x2) (= y1 y2))
          (setq finito true)))
    (list finito steps (list x1 y1) (list x2 y2))))

Proviamo:

(robot-meet 5 5 '(0 0) '(4 4) 1000)
;-> 1 . . . .
;-> . . . . .
;-> . . . . .
;-> . . . . .
;-> . . . . 1
;-> 
;-> . 1 . . .
;-> . . . . .
;-> . . . . .
;-> . . . . .
;-> . . . 1 .
;-> 
;-> ...
;-> 
;-> . 1 . . .
;-> . . . . .
;-> . . . . .
;-> . . . . .
;-> . . . . .
;-> 
;-> (true 13 (0 1) (0 1))

; Funzione che calcola il numero di passi di un processo
(define (robot-meet-fast M N x1 y1 x2 y2)
  (let ((dx (array 8 '(-1 -1 -1 0 0 1 1 1)))
        (dy (array 8 '(-1 0 1 -1 1 -1 0 1)))
        (steps 0)
        (finito nil)
        r nx ny)
    (until finito
      (++ steps)
      ; muove robot 1
      (setq r (rand 8))
      (setq nx (+ x1 (dx r)))
      (setq ny (+ y1 (dy r)))
      (if (and (>= nx 0) (< nx M) (>= ny 0) (< ny N))
          (begin (setq x1 nx) (setq y1 ny)))
      ; muove robot 2
      (setq r (rand 8))
      (setq nx (+ x2 (dx r)))
      (setq ny (+ y2 (dy r)))
      (if (and (>= nx 0) (< nx M) (>= ny 0) (< ny N))
          (begin (setq x2 nx) (setq y2 ny)))
      ; controllo collisione
      (if (and (= x1 x2) (= y1 y2))
          (setq finito true)))
    steps))

Proviamo:

(collect (robot-meet-fast 5 5 0 0 4 4) 10)
;-> (38 166 33 24 32 69 65 43 94 37)

; Funzione ausiliaria per calcolare la media dei passi di 'iter' processi
(define (robot-stat-aux M N x1 y1 x2 y2 dx dy)
  (local (steps finito r nx ny)
    (setq steps 0)
    (until finito
      (++ steps)
      (setq r (rand 8))
      (setq nx (+ x1 (dx r)))
      (setq ny (+ y1 (dy r)))
      (when (and (>= nx 0) (< nx M) (>= ny 0) (< ny N))
        (setq x1 nx y1 ny))
      (setq r (rand 8))
      (setq nx (+ x2 (dx r)))
      (setq ny (+ y2 (dy r)))
      (when (and (>= nx 0) (< nx M) (>= ny 0) (< ny N))
        (setq x2 nx y2 ny))
      (if (and (= x1 x2) (= y1 y2))
          (setq finito true)))
    steps))

; Funzione che calcola la media dei passi di 'iter' processi
; ogni processo inizia con i robot posizionati in modo casuale
(define (robot-stat M N iter)
  (let ( (dx (array 8 '(-1 -1 -1 0 0 1 1 1)))
         (dy (array 8 '(-1 0 1 -1 1 -1 0 1)))
         (somma 0) x1 y1 x2 y2 )
    (for (i 1 iter)
      (setq x1 (rand M))
      (setq y1 (rand N))
      (setq x2 (rand M))
      (setq y2 (rand N))
      (++ somma (robot-stat-aux M N x1 y1 x2 y2 dx dy)))
    (div somma iter)))

Proviamo:

(time (println (robot-stat 5 5 1e5)))
;-> 35.16844
;-> 2378.43
(time (println (robot-stat 5 5 1e6)))
;-> 35.140599
;-> 23635.555

Griglie di dimensioni diverse con lo stesso numero di celle:
(time (println (robot-stat 6 10 1e5)))
;-> 93.09115
;-> 6164.18
(time (println (robot-stat 5 12 1e5)))
;-> 99.52536000000001
;-> 6565.682
(time (println (robot-stat 4 15 1e5)))
;-> 115.23469
;-> 7567.635
(time (println (robot-stat 2 30 1e5)))
;-> 300.6541
;-> 19137.07
(time (println (robot-stat 1 60 1e5)))

Il numero medio di passi diminuisce se la griglia diventa più 'quadrata'.

Adesso vogliamo calcolare, per ogni coppia di posizioni iniziali, il numero medio di passi prima dell'incontro.
Il risultato è una matrice MxN dove ogni cella rappresenta il tempo medio quando il robot 1 parte da quella cella e il robot 2 è fissato in una posizione.
Questo produce una specie di 'heatmap' del tempo di incontro.

; Funzione che calcola la mappa dei tempi medi
(define (robot-heatmap M N x2 y2 iter)
  (let ((matrix (array M N '(0)))
        (dx (array 8 '(-1 -1 -1 0 0 1 1 1)))
        (dy (array 8 '(-1 0 1 -1 1 -1 0 1)))
        somma)
    (for (x1 0 (- M 1))
      (for (y1 0 (- N 1))
        (setq somma 0)
        (for (k 1 iter)
          (++ somma (robot-stat-aux M N x1 y1 x2 y2 dx dy)))
        (setf (matrix x1 y1) (div somma iter))))
    matrix))

Proviamo:

Robot 2 fissato al centro della griglia:
(setq H (robot-heatmap 5 5 2 2 2000))
;-> ((35.595 33.8725 34.7175 34.081 34.9045)
;->  (34.8365 35.1295 33.568 34.957 35.5575)
;->  (35.298 35.273 32.4855 34.28 33.568)
;->  (35.228 34.672 34.7195 35.5175 35.3215)
;->  (37.784 34.285 35.413 34.8585 35.498))

La struttura è radialmente simmetrica:
- vicino al robot fisso -> incontro più veloce
- lontano -> più lento
Il massimo è agli angoli della griglia.
Questo succede perché la distanza iniziale è maggiore e il moto è diffuso.
Il centro non ha valore 0 perchè controlliamo la collisione 'dopo' i movimenti.

Robot 2 è in un angolo:
(robot-heatmap 5 5 0 0 2000)
;-> ((15.6185 23.526 32.24 37.048 39.779)
;->  (23.8105 27.6335 34.491 37.5375 39.201)
;->  (32.203 32.8165 36.6305 40.0085 38.8305)
;->  (36.4095 37.0525 37.0075 40.446 42.337)
;->  (39.549 39.327 41.682 41.932 43.872))
questa volta la mappa diventa asimmetrica, perché i bordi limitano il movimento.


--------------------------------------------
Applicazione multipla di una funzione (nest)
--------------------------------------------

La funzione Nest accetta tre parametri: f, start e times, dove f è una funzione, start è il suo argomento iniziale e times indica quante volte la funzione viene applicata.
Dovrebbe restituire un'espressione con f applicata times volte partendo da start.
Pertanto, funzionerà in questo modo:
  (nest f x 3)
restituisce:
  f(f(f(x)))
e
  (nest f '(a b) 3)
restituisce:
  f(f(f('(a b))))

Se il valore di 'start' è un numero, allora possiamo usare la funzione 'series':

(define (nest func x0 iteration all)
  (if all
    (series x0 (fn(x) (func x)) (+ iteration 1))
    (last (series x0 (fn(x) (func x)) (+ iteration 1)))))

(nest sin 0.5 10)
;-> 0.3660990370275885
(nest sin 0.5 10 true)
;-> (0.5 0.479425538604203 0.4612695550331807 0.4450853368470909
;->  0.4305349046817725 0.417356952802932 0.4053456944600499
;->  0.3943364654278321 0.3841956640114271 0.3748135580091452
;->  0.3660990370275885)

Quando il parametro 'all' vale 'true', allora vengono restituiti tutti i valori.

Comunque il valore iniziale può essere anche una lista, quindi dobbiamo modificare la funzione.
Inoltre ci sono alcune considerazioni da fare:
1) l'applicazione della funzione 'f' deve continuare se raggiungiamo un valore specifico (es. nil o '() o 0)?
2) l'applicazione della funzione 'f' deve continuare se incontriamo un ciclo (un valore già incontrato in precedenza)?
3) In tali casi, quali valori vogliamo restituire?

Per il caso 1) inseriamo come parametro una funzione 'fn-true' che restituisce 'true' quando dobbiamo fermarci (cioè quando il valore corrente ha un valore specifico).
Per il caso 2) inseriamo un parametro 'cycle', quando vale 'true' controlliamo gli eventuali cicli (cioè controlliamo se un valore è stato calcolato precedentemente).
Quando i casi 1) o 2) sono attivi e vengono verificati, allora restituiamo gli ultimi due valori calcolati.

(define (nested func start times all cycle fn-true)
    (if all ; restituisce tutti i risultati
        (let ( (cur start) (out (list start)) (stop nil) (prev nil) )
          (for (i 1 times 1 stop)
            ; salva il valore precedente
            (setq prev cur)
            ; calcola il valore corrente
            (setq cur (func cur))
            ; controllo ciclo
            (if (and cycle (ref cur out)) (setq stop true))
            ; inserisce il valore corrente nella lista di output
            (push cur out -1)
            ; controllo valori di terminazione
            (if (and fn-true (fn-true cur)) (setq stop true))
            ; controllo punto fisso
            (if (and cycle (= prev cur)) (setq stop true)))
            out)
        ;else 
        (let ( (cur start) (stop nil) (prev nil) )
          (for (i 1 times 1 stop)
            ; salva il valore precedente
            (setq prev cur)
            ; inserisce il valore corrente nella lista di output
            (setq cur (func cur))
            ; controllo valori di terminazione
            (if (and fn-true (fn-true cur)) (setq stop true))
            ; controllo punto fisso
            (if (and cycle (= prev cur)) (setq stop true)))
            ; restituisce il risultato
            (if stop (list prev cur) cur))))
fn-true
Proviamo:
(nested sin 0.5 10)
;-> 0.3660990370275885
(nested sin 0.5 10 true)
;-> (0.5 0.479425538604203 0.4612695550331807 0.4450853368470909
;->  0.4305349046817725 0.417356952802932 0.4053456944600499
;->  0.3943364654278321 0.3841956640114271 0.3748135580091452
;->  0.3660990370275885)

(define (f pair) (list (string (pair 0) (pair 1)) (string (pair 1) (pair 0))))
(f '("a" "b"))
;-> ("ab" "ba")
(nested f '("a" "b") 3)
(nested f '("a" "b") 3 true)
;-> (("a" "b") ("ab" "ba") ("abba" "baab") ("abbabaab" "baababba"))

(define (func-true x) (= x '("abba" "baab")))
(nested f '("a" "b") 10 true nil func-true)
;-> (("a" "b") ("ab" "ba") ("abba" "baab"))

(define (g lst) (pop lst) lst)
(define (func-true x) (= x '()))
(nested g '(1 2 3) 2 true)
;-> ((1 2 3) (2 3) (3))
(nested g '(1 2 3) 3 true)
;-> ((1 2 3) (2 3) (3) ())
(nested g '(1 2 3) 4 true)
;-> ((1 2 3) (2 3) (3) () ())
(nested g '(1 2 3) 4 true nil func-true)
;-> ((1 2 3) (2 3) (3) ())
(nested g '(1 2 3) 4 nil nil func-true)
;-> ((3) ())
(nested g '(1 2 3) 4 nil true nil)
;-> (nested g '(1 2 3) 4 nil true nil)
;-> (() ())

Vedi anche "La funzione "nest"" su "Note libere 23".


-----------------------------
Visitare punti in una griglia
-----------------------------

In una griglia 2D infinita abbiamo N punti: (x1, y1) (x2, y2) ... (xN, yN).
Questi punti devono essere visitati nell'ordine indicato, partendo da (x1, y1) e terminando nel punto (xN, yN).
In una singola mossa, possiamo spostarci di una cella in alto, in basso, a sinistra o a destra.
Determinare il percorso minimo che visita tutti i punti.

L'idea è che tra due punti consecutivi possiamo sempre costruire un percorso usando solo mosse orizzontali e verticali.
La distanza minima tra due punti (cioè il numero minimo di mosse):
(x1,y1) e (x2,y2)) è la distanza Manhattan:
  
  d = |x2 - x1| + |y2 - y1|

Poiché la griglia è infinita, un percorso esiste sempre.

Costruzione del percorso
Per andare da (x(i),y(i)) a (x(i+1),y(i+1)) (coppia di punti consecutivi):
1. muoversi orizzontalmente fino a raggiungere x(i+1)
2. muoversi verticalmente fino a raggiungere y(i+1)
oppure viceversa (l’ordine è indifferente).

Esempio: (2,3) -> (5,1)
  (2,3), muove lungo X
  (3,3), muove lungo X
  (4,3), muove lungo X
  (5,3), muove lungo X 
  (5,2), muove lungo Y
  (5,1), muove lungo Y

(define (path points)
  (let (out '())
    (for (i 0 (- (length points) 2))
      (letn ((p1 (points i))
             (p2 (points (+ i 1)))
             (x1 (p1 0)) (y1 (p1 1))
             (x2 (p2 0)) (y2 (p2 1)))
        ; inserisce il punto iniziale solo per il primo segmento
        (if (= i 0) (push (list x1 y1) out -1))
        ; muove lungo X
        (while (!= x1 x2)
          (if (< x1 x2) (inc x1) (-- x1))
          (push (list x1 y1) out -1))
        ; muove lungo Y
        (while (!= y1 y2)
          (if (< y1 y2) (inc y1) (-- y1))
          (push (list x1 y1) out -1))))
    out))

Proviamo:

(path '((0 0) (3 2) (1 4) (0 0)))
;-> ((0 0) (1 0) (2 0) (3 0) (3 1) (3 2) (2 2) (1 2) (1 3) (1 4))

Quanti sono i percorsi minimi tra due punti?
--------------------------------------------
Supponiamo di andare da (x1,y1) a (x2,y2).
Definiamo:
  dx = |x2 - x1|, dy = |y2 - y1|

Il percorso minimo ha lunghezza: dx + dy e consiste di:
- (dx) mosse orizzontali
- (dy) mosse verticali
Il problema diventa: in quanti modi possiamo ordinare queste mosse?
La risposta è il coefficiente binomiale:

  binom(dx+dy, dx)

(define (binom num k)
"Calculate the binomial coefficient (n k) = n!/(k!*(n - k)!) (combinations of k elements without repetition from n elements)"
  (cond ((> k num) 0L)
        ((zero? k) 1L)
        ((< k 0) 0L)
        (true
          (let (r 1L)
            (for (d 1 k)
              (setq r (/ (* r num) d))
              (-- num))
          r))))

Esempio:
Da (0,0) a (3,2) --> dx = 3, dy = 2
Numero di percorsi minimi:
(binom 5 3)
;-> 10L

Perché dobbiamo scegliere 3 posizioni su 5 dove mettere le mosse orizzontali.
Esempio di sequenze:
  XXXYY
  XXYXY
  XXYYX
  XYXXY
  XYXYX
  XYYXX
  YXXXY
  YXXYX
  YXYXX
  YYXXX

Quanti sono i percorsi minimi tra N punti?
------------------------------------------
Se dobbiamo visitare più punti P1 -> P2 -> ... -> PN e i segmenti sono indipendenti, il numero totale di percorsi minimi è:

  Prod[i=1,N-1]binom(dx(i)+dy(i), dx(i))

dove:

  dx(i) = |x(i+1) - x(i)|
  dy(i) = |y(i+1) - y(i)|

Quali sono tutti i percorsi minimi tra due punti?
-------------------------------------------------
Per generare tutti i percorsi minimi tra due punti basta osservare:
- dobbiamo fare dx = |x2-x1| mosse orizzontali
- dobbiamo fare dy = |y2-y1| mosse verticali
- il percorso ha lunghezza (dx + dy)

Quindi dobbiamo scegliere le posizioni delle mosse orizzontali (o verticali) nella sequenza di (dx + dy) mosse.
Il numero totale dei percorsi è binom(dx+dy,dx), per generarli tutti:
1. generare tutte le combinazioni delle posizioni delle mosse 'X'
2. costruire la sequenza di mosse
3. trasformarla nella sequenza di coordinate.

(define (comb k lst (r '()))
"Generate all combinations of k elements without repetition from a list of items"
  (if (= (length r) k)
    (list r)
    (let (rlst '())
      (dolist (x lst)
        (extend rlst (comb k ((+ 1 $idx) lst) (append r (list x)))))
      rlst)))

; Genera tutti i percorsi minimi tra due punti p1 e p2
; usando solo mosse orizzontali e verticali
(define (all-paths p1 p2)
  (letn ((x1 (p1 0)) (y1 (p1 1)) ; coordinate punto iniziale
         (x2 (p2 0)) (y2 (p2 1)) ; coordinate punto finale
         (dx (abs (- x2 x1))) ; numero mosse orizzontali
         (dy (abs (- y2 y1))) ; numero mosse verticali
         (n (+ dx dy)) ; lunghezza totale del percorso minimo
         ; tutte le combinazioni delle posizioni delle dx mosse orizzontali
         (xs (comb dx (sequence 0 (- n 1))))
         (out '())) ; lista dei percorsi risultanti
    ; per ogni combinazione delle posizioni delle mosse orizzontali
    (dolist (cx xs)
      (letn ((x x1) (y y1) ; posizione corrente
             (path (list (list x y)))) ; il percorso parte dal punto iniziale
        ; costruiamo il percorso passo per passo
        (for (i 0 (- n 1))
          ; se la posizione i appartiene alla combinazione → mossa orizzontale
          (if (ref i cx)
              (if (< x1 x2) (++ x) (-- x))
              ; altrimenti mossa verticale
              (if (< y1 y2) (++ y) (-- y)))
          ; aggiungiamo la nuova cella al percorso
          (push (list x y) path -1))
        ; salviamo il percorso completo
        (push path out -1)))
    ; restituisce tutti i percorsi
    out))

Proviamo:

(all-paths '(0 0) '(2 1))
;-> (((0 0) (1 0) (2 0) (2 1))
;->  ((0 0) (1 0) (1 1) (2 1))
;->  ((0 0) (0 1) (1 1) (2 1)))

(length (all-paths '(0 0) '(3 2)))
;-> 10

Quali sono tutti i percorsi minimi tra N punti?
-----------------------------------------------
Abbiamo i punti P1 -> P2 -> P3 -> ... -> PN
Ogni segmento è indipendente.

Per il segmento P(i) -> P(i+1) definiamo:

  dx(i) = |x(i+1) - x(i)|
  dy(i) = |y(i+1) - y(i)|

Il numero di percorsi minimi del segmento è

  binom(dx(i) + dy(i),dx(i))

Il numero totale di percorsi è quindi il prodotto:

  Prod[i=1,N-1]binom(dx(i)+dy(i), dx(i))

Per generare tutti i percorsi globali occorre:
1. generare tutti i percorsi minimi di ogni segmento
2. fare il prodotto cartesiano dei segmenti
3. concatenare i percorsi eliminando il punto duplicato.

Dividiamo il problema in due parti:

A) Funzione "paths2"
Genera tutti i percorsi minimi tra due punti
1. tra due punti servono dx mosse orizzontali e dy verticali
2. il percorso ha n = dx + dy mosse
3. scegliamo le posizioni delle dx mosse orizzontali tra n posizioni
4. ogni scelta genera un percorso.

; Funzione che genera tutti i percorsi minimi tra due punti
(define (paths2 p1 p2)
  (letn (
        ; coordinate dei punti
        (x1 (p1 0)) (y1 (p1 1))
        (x2 (p2 0)) (y2 (p2 1))
        ; numero di mosse necessarie
        (dx (abs (- x2 x1))) ; mosse orizzontali
        (dy (abs (- y2 y1))) ; mosse verticali
        ; lunghezza totale del percorso
        (n (+ dx dy))
        ; tutte le combinazioni delle posizioni delle mosse orizzontali
        ; esempio: (0 2 4)
        (cx (comb dx (sequence 0 (- n 1))))
        ; lista dei percorsi risultanti
        (out '())
       )
    ; per ogni combinazione
    (dolist (c cx)
      ; coordinate correnti
      (letn (
            (x x1)
            (y y1)
            ; il percorso parte dal punto iniziale
            (path (list (list x y)))
           )
        ; costruiamo il percorso passo per passo
        (for (i 0 (- n 1))
          ; se la posizione i è nella combinazione
          ; allora facciamo una mossa orizzontale
          (if (ref i c)
              (if (< x1 x2) (++ x) (-- x))
              ; altrimenti mossa verticale
              (if (< y1 y2) (++ y) (-- y))
          )
          ; aggiungiamo la nuova cella al percorso
          (push (list x y) path -1)
        )
        ; salviamo il percorso costruito
        (push path out -1)
      )
    )
    ; ritorna tutti i percorsi
    out))

B) Funzione "all-paths"
Genera tutti i percorsi che visitano N punti nell'ordine dato.
1. generiamo tutti i percorsi per ogni segmento
2. combiniamo i segmenti con un **prodotto cartesiano**
3. uniamo i percorsi eliminando il punto duplicato.

; Funzione che genera tutti i percorsi che visitano i punti nell'ordine dato
(define (all-paths points)
  ; lista dei percorsi per ogni segmento
  (let ((paths '()))
    ; per ogni coppia di punti consecutivi
    (for (i 0 (- (length points) 2))
      ; genera tutti i percorsi tra i due punti
      (push (paths2 (points i) (points (+ i 1))) paths -1)
    )
    ; inizialmente i percorsi sono quelli del primo segmento
    (let ((out (first paths)))
      ; combiniamo progressivamente i segmenti successivi
      (dolist (seg (rest paths))
        ; nuovo insieme di percorsi
        (setq out
          (flat
            ; per ogni percorso già costruito
            (map
              (fn (p)
                ; lo concateno con ogni percorso del segmento successivo
                (map
                  (fn (q)
                    ; eliminiamo il primo punto di q
                    ; perché coincide con l'ultimo di p
                    (append p (rest q))
                  )
                  seg
                )
              )
              out
            )
            ; appiattiamo di un livello
            1
          )
        )
      )
      ; tutti i percorsi completi
      out
    )
  )
)

Proviamo:

(all-paths '((0 0) (1 1) (2 0)))
;-> (((0 0) (1 0) (1 1) (2 1) (2 0))
;->  ((0 0) (1 0) (1 1) (1 0) (2 0))
;->  ((0 0) (0 1) (1 1) (2 1) (2 0))
;->  ((0 0) (0 1) (1 1) (1 0) (2 0)))

Segmenti:
  (0,0) -> (1,1)   2 percorsi
  (1,1) -> (2,0)   2 percorsi

Totale:
  2 x 2 = 4 percorsi

Nota: questo metodo esplode combinatoriamente, infatti se ogni segmento ha ~10 percorsi: 10 segmenti -> 10^10 percorsi.


-----------------------------------
Taglio di corde lineari e circolari
-----------------------------------

Abbiamo una corda di lunghezza unitaria.
Se gli estremi della corda sono staccati (corda lineare) con N tagli otteniamo (N+1) segmenti di corda.
Se gli estremi della corda sono uniti (corda circolare) con N tagli otteniamo N segmenti di corda.
Con N tagli, quanto è lungo, in media, il pezzo più corto?
Con N tagli, quanto è lungo, in media, il pezzo più lungo?

Esempio corda lineare:
  N = 2
  t1 = 0.3
  t2 = 0.8
                   t1                            t2
  |-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|
  0    0.1   0.2   0.3   0.4   0.5   0.6   0.7   0.8   0.9   1.0

  d1 = t1 - 0 = t1 = 0.3
  d2 = t2 - t1 = t1 = 0.8 - 0.3 = 0.5
  d3 = 1 - t3 = 1 - 0.8 = 0.2

; Funzione che calcola la lunghezza dei segmenti di una corda lineare unitaria
; sottoposta ai tagli contenuti in una lista (t1,t2,...,tN)
(define (break-linear T)
  (let (T (flat (list 0 (sort T) 1)))
    (map sub (rest T) (chop T))))

Proviamo:

(break-linear '(0.3 0.8))
;-> (0.3 0.5 0.2)
(break-linear '(0.8 0.3))
;-> (0.3 0.5 0.2)
(break-linear '(0.5))
;-> (0.5 0.5)

Esempio corda circolare (immagina che 0 e 1 sono uniti):
  N = 2
  t1 = 0.3
  t2 = 0.8
                   t1                            t2
  |-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|
  0    0.1   0.2   0.3   0.4   0.5   0.6   0.7   0.8   0.9   1.0

  d1 = t2 - t1 = 0.8 - 0.3 = 0.5
  d3 = (1 - t2) + t1 = 0.2 + 0.3 = 0.5 (Perchè i punti 0 e 1 sono uniti)

(define (break-circular T)
  (let (T (flat (list (sort T) 1)))
      (setf (T -1) (add 1 (T 0))) ; coordinata finale
      (map sub (rest T) (chop T))))

Proviamo:

(break-circular '(0.3 0.8))
;-> (0.5 0.5)
(break-circular '(0.8 0.3))
;-> (0.5 0.5)

Con un taglio otteniamo un segmento lineare di lunghezza 1:
(break-circular '(0.5))
;-> (1)
(break-circular '(0.2))
;-> (1)

; Funzione che calcola il valore minimo medio e il valore massimo medio
; di N tagli di una corda lineare
(define (medie-lineari N iter)
  (let ( (emin 0) (emax 0) (dist 0) )
    (for (i 1 iter)
      (setq dist (break-linear (random 0 1 N)))
      (inc emin (apply min dist))
      (inc emax (apply max dist))
    )
    ;(println "valore minimo medio  = " (div emin iter))
    ;(println "valore massimo medio = " (div emax iter))
    (list (div emin iter) (div emax iter))))

Proviamo:

(medie-lineari 3 1e5)
;-> (0.06248319009980664 0.5210660414441305)
(medie-lineari 20 1e5)
;-> (0.002268929105502424 0.1733977004303195)
(medie-lineari 50 1e5)
;-> (0.0003839600817895947 0.0886794573809124)

Matematicamente (caso lineare):
Il valore minimo medio vale: 1/(N+1)^2
Il valore massimo medio vale: (1 + 1/2 + ... + 1/N + 1/(N+1)) / (N+1)

; Funzione che calcola il valore minimo medio e il valore massimo medio
; di N tagli di una corda circolare
(define (medie-circolari N iter)
  (let ( (emin 0) (emax 0) (dist 0) )
    (for (i 1 iter)
      (setq dist (break-circular (random 0 1 N)))
      (inc emin (apply min dist))
      (inc emax (apply max dist))
    )
    ;(println "valore minimo medio  = " (div emin iter))
    ;(println "valore massimo medio = " (div emax iter))
    (list (div emin iter) (div emax iter))))

Proviamo:

(medie-circolari 3 1e5)
;-> (0.1114481221961225 0.610833123264258)
(medie-circolari 20 1e5)
;-> (0.002484960478530159 0.1797936817529937)
(medie-circolari 50 1e5)
;-> (0.0004025788140507105 0.08992011139257101)

Matematicamente (caso circolare):
Il valore minimo medio vale: 1/(N)^2
Il valore massimo medio vale: (1 + 1/2 + ... + 1/N) / N

Verifichiamo i valori calcolati con le simulazioni:

(define (check N)
  (list (div 1 (* N N)) (div (apply add (map div (sequence 1 N))) N)))

Caso lineare:
(check 4)
;-> (0.0625 0.5208333333333333)
(check 21)
;-> (0.002267573696145125 0.1735885097506062)
(check 51)
;-> (0.0003844675124951942 0.08860418002875839)

Caso circolare:
(check 3)
;-> (0.1111111111111111 0.6111111111111111)
(check 20)
;-> (0.0025 0.1798869828571841)
(check 50)
;-> (0.0004 0.08998410676658847)


----------------
X*Y = reverse(Y)
----------------

Determinare le coppie di numeri X e Y per cui risulta::

  X*Y = reverse(Y)

cioè, Y ={abc...z} --> X*{abc...z} = {z...cba}

  X*Y = inizia con reverse(Y)
  X*{abc...z} = {z...cba}...

Proviamo con un metodo brute-force.
Due cicli 'for' per X (da 2 a max-X) e Y (da 2 a max-Y).

Comunque possiamo ottimizzare il ciclo per Y:

Quando X*Y ha lo stesso numero di cifre di Y (X e Y interi)?
Quando risulta X <= 9 floor((10^d - 1)/Y).
Quindi per Y basta arrivare a:

  Y <= max-X / Y

perché X*Y non può superare il massimo numero con lo stesso numero di cifre.

(define (findXY max-X max-Y)
  (for (X 2 max-X)
      (for (Y 2 (/ max-Y X))
        (if (= (* X Y) (int (reverse (string Y)) 0 10))
            (println X { } Y { } (* X Y) { } (reverse (string Y)))))))

Proviamo:

(time (findXY 9 1e6))
;-> 4 2178 8712 8712
;-> 4 21978 87912 87912
;-> 4 219978 879912 879912
;-> 9 1089 9801 9801
;-> 9 10989 98901 98901
;-> 9 109989 989901 989901
;-> 1024.658

(time (findXY 9 1e7))
;-> 4 2178 8712 8712
;-> 4 21978 87912 87912
;-> 4 219978 879912 879912
;-> 4 2199978 8799912 8799912
;-> 9 1089 9801 9801
;-> 9 10989 98901 98901
;-> 9 109989 989901 989901
;-> 9 1099989 9899901 9899901
;-> 10635.099

Dal punto di vista matematico vogliamo risolvere:
  X * Y = reverse(Y)
dove:
 Y = a(d-1) a(d-2) ... a(1) a(0), con a(d-1) != 0.

Il numero invertito è:
  reverse(Y) = a(0) a(1) ... a(d-2) a(d-1)

A) Ultima cifra della moltiplicazione
-------------------------------------
La prima colonna della moltiplicazione è:
  X * a(0)

La cifra finale deve diventare la prima cifra di reverse(Y):
  X * a(0) = a(d-1) (mod 10)

B) Prima cifra del risultato
----------------------------
La cifra piu' significativa di X*Y proviene da
  X * a(d-1) + carry
e deve essere:
  a(0)
Quindi:
  X * a(d-1) + c = a(0) + 10k

C) Vincolo modulo 9
-------------------
Poichè:
  10 == 1 (mod 9)
vale:
  Y == reverse(Y) (mod 9)

Quindi dalla relazione:
  X*Y = reverse(Y)
segue:
  X*Y == Y (mod 9)
  (X-1)Y == 0 (mod 9)

cioè:
  Y == 0 (mod 9) oppure X == 1 (mod 9)

D) Proviamo i valori piccoli di X
---------------------------------
Le equazioni sulle cifre:
  X * a(0) = a(d-1) (mod 10)
  X * a(d-1) + c = a(0) (mod 10)
eliminano quasi tutti i valori.

Gli unici che funzionano sono:
  X = 4
  X = 9

E) Famiglia per X = 4
---------------------
Le soluzioni hanno forma:
  Y = 21 99...99 78

Esempi:
  2178
  21978
  219978
  2199978
  ...
Verifica:
  4 * 2178 = 8712
  4 * 21978 = 87912

F) Famiglia per X = 9
---------------------
Le soluzioni hanno forma:
  Y = 10 99...99 89

Esempi:
  1089
  10989
  109989
  1099989
  ...
Verifica
  9 * 1089 = 9801
  9 * 10989 = 98901

E) Struttura delle soluzioni
Le soluzioni si ottengono inserendo qualsiasi numero di '9' al centro.

(define (solutions max9)
  (for (k 0 max9)
    (let ((mid (dup "9" k)))
      (let ((y1 (string "21" mid "78"))
            (y2 (string "10" mid "89")))
        (println 4 " " y1 " " (* 4 (int y1)))
        (println 9 " " y2 " " (* 9 (int y2)))))) '>)

(solutions 1)
;-> 4 2178 8712
;-> 9 1089 9801
;-> 4 21978 87912
;-> 9 10989 98901

(solutions 5)
;-> 4 2178 8712
;-> 9 1089 9801
;-> 4 21978 87912
;-> 9 10989 98901
;-> 4 219978 879912
;-> 9 109989 989901
;-> 4 2199978 8799912
;-> 9 1099989 9899901
;-> 4 21999978 87999912
;-> 9 10999989 98999901
;-> 4 219999978 879999912
;-> 9 109999989 989999901


--------------------------------
Probabilità di superare un esame
--------------------------------

Per ogni domanda di un esame, è possibile indovinare la risposta corretta con probabilità p(i).
Una risposta corretta vale +1 punto, una risposta errata vale -1 punto e non rispondere vale 0 punti.
Per superare l'esame occorre raggiungere o superare un punteggio P.
Possiamo scegliere K domande a cui vogliamo rispondere.
Che probabilità abbiamo di superare l'esame con K risposte?

Quindi abbiamo:
1) una lista con le N probabilità p(0),...p(N-1)
2) il punteggio da raggiungere P
3) il numero di domande a cui rispondere K (quelle con probabilità maggiore)

Vogliamo la probabilità di ottenere esattamente un punteggio >= P

Se ogni risposta corretta = +1, errata = -1, allora:
  punteggio finale = 2*corrette - K
  corrette = (punteggio + K)/2

Quindi, per ottenere almeno P punti, serve almeno:
  need = ceil((K + P)/2)

Il problema si riduce a calcolare la probabilità di avere ≥ 'need' risposte corrette tra le K domande scelte.
Possiamo usare DP 1D classico:
  dp(c) = probabilità di avere esattamente c risposte corrette

Vediamo come funziona questo DP 1D:
- Abbiamo K domande selezionate, con probabilità di risposta corretta (p(0(, p(1), …, p(K-1)).
- Vogliamo sapere la probabilità di avere esattamente 'c' risposte corrette su queste K domande.
- Ogni risposta sbagliata vale 0 punti o -1, ma trasformando il problema in 'numero di risposte corrette' possiamo lavorare solo con numeri interi da 0 a K.
1. dp(c) = probabilità di avere esattamente 'c' risposte corrette dopo aver considerato alcune domande.
2. Inizialmente, nessuna domanda considerata:
  dp(0) = 1
  dp(c>0) = 0
3. Poi aggiungiamo le domande una a una.
Per ogni nuova domanda (i) con probabilità di successo p(i):
  dp2(c) = p(i)*dp(c-1)       +      (1-p(i))*dp(c)
           (risposta corretta)       (risposta errata)
Se rispondi correttamente, il numero di risposte corrette aumenta di 1 -> moltiplichiamo la probabilità di avere 'c-1' corrette prima per p(i).
Se sbagli, il numero di corrette rimane lo stesso -> moltiplichiamo la probabilità di avere 'c' corrette prima per (1-p(i)).
Invece di calcolare tutte le combinazioni possibili esplicitamente, aggiorniamo una sola lista dp ad ogni domanda.
Dopo aver considerato tutte le K domande, 'dp(c)' contiene la probabilità esatta di avere c corrette.

Come ottenere la probabilità di almeno 'need' punti?
1. Ogni risposta corretta = +1, ogni risposta sbagliata = -1
2. Punteggio finale = 2*corrette - K
3. Numero minimo di corrette per ottenere almeno P punti:
  need = ceil((K + P)/2)
4. Probabilità finale = somma di tutte le dp(c) con (c >= need)

Controllo dei casi impossibili:
1. (P > K() -> impossibile ottenere più punti di quante domande si rispondano.
2. (K > length(probs)) -> impossibile rispondere a più domande di quelle disponibili.
3. (P > length(probs)) -> impossibile ottenere più punti del numero totale di domande.
In tutti questi casi restituiamo '0'.

'sel' prende le K domande migliori
'dp(c)' calcola le probabilità esatte di avere c risposte corrette
'need = ceil((K+P)/2)' -> numero minimo di risposte corrette per ottenere almeno P punti
'Somma dei dp(c) da need a K' -> probabilità finale

(define (prob-punteggio probs K P)
  (if (or (> P K) (> K (length probs)) (> P (length probs))) 0
  ;else
  (letn ((n (length probs))) ; numero totale di domande
    ;; selezioniamo le K domande con probabilità più alte
    (setq sel '())
    (setq ps (sort probs >)) ; ordinamento decrescente
    (for (i 0 (sub K 1))
      (push (ps i) sel -1))
    ;; DP 1D: dp(c) = probabilità di avere esattamente c risposte corrette
    (setq dp (dup 0 (add K 1)))
    (setf (dp 0) 1) ; inizializziamo 0 corrette
    (for (i 0 (sub K 1))
      (setq pi (sel i)) ; probabilità della i-esima domanda
      (setq dp2 (dup 0 (add K 1))) ; buffer temporaneo per il DP
      (for (c 0 K)
        (if (> c 0)
            (setf (dp2 c) (add (mul pi (dp (sub c 1))) ; risposta corretta
                                (mul (sub 1 pi) (dp c)))) ; risposta errata
            (setf (dp2 0) (mul (sub 1 pi) (dp 0))))) ; caso c=0
      (setq dp dp2)) ; aggiorniamo dp con dp2
    ;; calcoliamo il numero minimo di risposte corrette per ottenere almeno P punti
    (setq need (ceil (div (add K P) 2)))
    ;; somma delle probabilità di avere >= need risposte corrette
    (setq prob 0)
    (for (c need K)
      (setq prob (add prob (dp c))))
    prob)))

Proviamo:

(prob-punteggio '(0.9 0.8 0.6 0.55 0.4) 3 2)
;->  0.432
Domande scelte:(0.9 0.8 0.6) -> le 3 domande migliori
DP passo passo:
Risposta0: 0.9 -> dp dopo: (0.0999 0.9 0 0)
Risposta1: 0.8 -> dp dopo: (0.02 0.26 0.72 0)
Risposta2: 0.6 -> dp dopo: (0.008 0.116 0.444 0.432)
Need=3 -> per ottenere almeno 2 punti con 3 domande, serve almeno 3 risposte corrette
Probabilità finale=0.432
La logica di need = ceil((K+P)/2) calcola esattamente il numero minimo di risposte corrette per ottenere almeno P punti:
K=3, P=2 -> need = ceil((3+2)/2)=3
K=3, P=1 -> need = ceil((3+1)/2)=2

(prob-punteggio '(0.9 0.8 0.6 0.55 0.4) 3 1)
;-> 0.876
(prob-punteggio '(1.0 0.8 0.6 0.55 0.4) 1 1)
;-> 1
(prob-punteggio '(1.0 0.8 0.6 0.55 0.4) 2 1)
;-> 0.8
(prob-punteggio '(1.0 0.8 0.6 0.55 0.4) 1 2)
;-> 0

(prob-punteggio '(0.5 0.5 0.5 0.5) 1 1)
;-> 0.5
(prob-punteggio '(0.5 0.5 0.5 0.5) 2 1)
;-> 0.25
(prob-punteggio '(0.5 0.5 0.5 0.5) 3 1)
;-> 0.5
(prob-punteggio '(0.5 0.5 0.5 0.5) 4 1)
;-> 0.3125


--------------------
Duello tra pistoleri
--------------------

Ci sono N pistoleri disposti in modo equidistante lungo una circonferenza.
Ad ogni turno ogni pistolero "spara" ad un'altro pistolero a caso (tranne che a se stesso).
Ogni pistolero ha una data probabilità di colpire il bersaglio (indipendente dalla distanza).
1 colpisce sempre il bersaglio e 0 non lo colpisce mai.
Quindi chi spara potrebbe non colpire il pistolero scelto a caso.
Tutti gli spari di ogni turno avvengono simultaneamente (es. se 1 spara a 3 e 3 spara a 1, allora potrebbero essere colpiti entrambi).
I pistoleri colpiti non partecipano al turno successivo (cioè non è possibile sparare ad un pistolero colpito).
Il duello continua fino a che non rimane soltanto uno o nessun pistolero.

; Funzione che simula un duello
(define (duello P)
  (local (len S shooted idx bersaglio)
    ; numero di pistoleri iniziali
    (setq len (length P))
    ; lista degli indici dei pistoleri
    (setq S (sequence 0 (- len 1)))
    (while (> len 1)
      ; lista dei colpiti
      (setq shooted '())
      ; ciclo per ogni pistolero...
      (dolist (el P)
        ; scelta del bersaglio da colpire
        (setq idx $idx) ; because 'until' update $idx...
        (until (!= idx (setq bersaglio (rand len))))
        ; sparo...
        (setq prob (random 0 1))
        ; se il bersaglio è stato colpito,
        ; allora lo aggiunge la lista dei colpiti
        (if (>= el prob) (push bersaglio shooted -1)))
      ; qualcuno potrebbe essere stato colpito più volte...
      (setq shooted (unique (sort shooted >)))
      ; elimina i colpiti dalla lista dei pistoleri
      (map (fn(x) (pop P x)) shooted)
      ;(println "P: " P)
      ; elimina i colpiti dalla lista degli indici dei pistoleri
      (map (fn(x) (pop S x)) shooted)
      ;(println "S: " S)
      ; calcola il numero dei pistoleri rimasti
      (setq len (length P))
    )
    ; Restituisce:
    ; una coppia (prob indice) oppure '() (cioè nessun vincitore)
    (flat (list P S))))

Proviamo:

(setq pp '(1.0 0.2 0.3 0.5 0.9 0.9 0.7 1.0 0.4))
(duello pp)

; Funzione che simula un determinato numero di duelli
; restituisce una coppia (a b)
; a --> numero di duelli nulli
; b --> lista con le vittorie di ogni pistolero
(define (simula-duelli probs iter)
  (let ( (out (array (length probs) '(0))) (nulli 0) )
    (for (i 1 iter)
      (setq res (duello probs))
      (if (= res '())
          (++ nulli) ; duello senza vincitore
          ;else
          (++ (out (res 1)))))
    (list nulli out)))

Proviamo:

(setq pp '(1.0 0.2 0.3 0.5 0.9 0.9 0.7 1.0 0.4))
(simula-duelli pp 1e5)
;-> (31517 (10549 3897 4587 6215 9588 9725 7796 10575 5551))

I più bravi hanno maggiori probabilità di vittoria.
(setq pp '(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1))
(simula-duelli pp 1e5)
;-> (23670 (1456 2595 3834 4843 5735 6722 7911 9268 10017 11384 12565))

Nota: anche chi ha probabilità 0 può vincere.
Infatti immaginiamo la situazione in cui rimangono 3 pistoleri:
se due pistoleri si sparano a vicenda e vengono entrambi colpiti, allora chi rimane potrebbe essere quello con probabilità 0.

Quando sono tutti bravi allo stesso modo, allora hanno tutti la stessa probabilità di vittoria.
(setq pp '(0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5))
(simula-duelli pp 1e5)
;-> (24442 (6945 6892 6871 7016 6993 6870 6669 6872 6781 6848 6801))

(setq pp '(0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2))
(simula-duelli pp 1e5)
;-> (9881 (8134 8137 8391 8112 8209 8115 8249 8028 8177 8279 8288))

Quando tutte le probabilità valgono 1 non viene sprecato alcun colpo, quindi risulta il maggior numero di 'nulli' (cioè i duelli in cui tutti vengono colpiti e non c'è alcun vincitore)
(setq pp '(1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0))
(simula-duelli pp 1e5)
;-> (44380 (5156 5116 5136 5160 4992 4995 5009 4995 4998 5052 5011))

Vedi anche "Duello continuo" su "Note libere 6".


-----------------------
Il cammello e le banane
-----------------------

Dobbiamo trasportare 3000 banane per 1000 km fino al mercato e abbiamo a disposizione un solo
cammello.
Però questo cammello ha dei limiti:
1) può portare al massimo 1000 banane per volta,
2) deve mangiare una banana per ogni chilometro percorso
Qual è il numero massimo di banane che arriva al mercato?

Disegniamo un grafico:
A = punto di partenza
B = punto di arrivo (mercato)

  A                                                         B
  |---------------------------------------------------------|
                            1000 km

Se partiamo con 1000 banane e arriviamo direttamente in B otteniamo:

  1000 - 1*1000 = 0

cioè il cammello ha mangiato tutte le 1000 banane per arrivare al mercato.

Stabiliamo di partire con 1000 banane ed arrivare ad un punto intermedio: 1 km

  A    1km                                                  B
  |-----|...------------------------------------------------|
                            1000 km

Trasporto 1:
 1000 - 1 = 999
Poi dobbiamo tornare indietro al punto A e consumiamo un'altra banana:
 1000 - 2 = 998
Adesso siamo al punto A con 2000 banane.
Nel punto a 1 km ci sono 998 banane.

Trasporto 2
-----------
 Ripetiamo lo stesso viaggio e adesso al punto 1km abbiamo 998 + 998 banane.

Trasporto 3
-----------
Ripetiamo lo stesso viaggio e adesso al punto 1km abbiamo 998 + 998 + 999 = 2995 banane.
Questo perchè non dobbiamo tornare ad A (in quanto non ci sono più banane).

Comunque ripetendo per 1000 volte (cioè per 1000 km) questo metodo otteniamo:

  5 * 1000 = 5000 banane

perchè per ogni tratta di 1km il cammello consuma 5 banane.
Ma non abbiamo 5000 banane, quindi non possiamo usare questo metodo.

Come ridurre questi trasporti e passare da tre a due trasporti?
Quello che bisogna fare è consumare 1000 banane per arrivare al primo deposito, perché nel momento in cui le banane da 3000 diventano 2000, dovrò fare solo due viaggi perché sposto 1000 banane una volta e poi un'altra volta altre 1000 e quindi dovrò fare solo due trasporti.
E quindi dove conviene fare il primo deposito?
Quando avrò consumato 1000 banane.
E per consumare 1000 banane devo arrivare a 200km, perchè per ogni kilometro consumo 5 banane (infatti 1000/5 = 200).

Stabiliamo di partire con 1000 banane ed arrivare ad un punto intermedio: 200 km

  A        200 km                                              B
  |----------|-------------------------------------------------|
                                  800 km

Trasporto 1
-----------
1000 - 400 = 600 banane

Trasporto 2
-----------
1000 - 400 = 600 banane

Trasporto 3
-----------
1000 - 200 = 800 banane

In questo modo a 200km abbiamo 2000 banane.

Adesso dove piazzare il secondo punto di sosta?
Ripetendo il ragionamento, da 200km devo fare 2 trasporti fino a quando a 200km rimangono 1000 banane.
Con 1000 banane e 3 spostamenti devo fare 1000/3 = 333.333 km.
Arrotondiamo al valore intero: 333.

  A        200 km          200+333 km                          B
  |----------|----------------|--------------------------------|
                             533 km

Trasporto 1
-----------
1000 - 666 = 334

Trasporto 2
-----------
1000 - 333 = 667

Adesso abbiamo 0 banane a 200km e 1001 banane al 533 km.
Adesso possiamo arrivare direttamente al punto B consumando:

1000 - 533 = 467 banane

Quindi al mercato arrivano 1001 - 467 = 534 banane

Generalizzazione del problema
-----------------------------
Questo è un classico problema di ottimizzazione a tratti: il costo (banane consumate per km) dipende da quante volte dobbiamo fare avanti-indietro.

Parametri:
  B = numero iniziale di banane
  D = distanza totale (km)
  C = capacita del cammello (banane per viaggio)
Il cammello:
- può portare al massimo C banane
- consuma 1 banana per km

1) Numero di viaggi
Se abbiamo B banane, il numero di carichi necessari e:
  n = ceil(B / C)

2) Consumo per km
Per avanzare di 1 km con n carichi:
- n viaggi in avanti
- n-1 viaggi di ritorno
Totale percorsi: 2n - 1
Quindi il consumo per km e:
  consumo = 2n - 1

3) Soluzione
Bisogna ridurre il numero di viaggi il prima possibile.
Strategia:
- parti con n = ceil(B / C)
- avanzi finche le banane scendono a (n-1)*C
- poi passi a n-1 viaggi
- e ripeti

4) Distanza di transizione
Per passare da n a n-1 viaggi:
Banane da consumare:
  B - (n-1)*C
Dato che il consumo per km e (2n - 1), la distanza e:
  d(n)= (B - (n-1)*C) / (2n - 1)

5) Algoritmo
Ripeti:
1. n = ceil(B / C)
2. Se n = 1:
   risultato = B - D
   STOP
3. Calcola:
   d(n) = (B - (n-1)*C) / (2n - 1)
4. Se d(n) >= D:
   risultato = B - (2n - 1)*D
   STOP
5. Altrimenti:
   B = (n-1)*C
   D = D - d(n)
   ripeti

6) Esempio: B=3000, C=1000, D=1000

Fase 1: n = 3
  d(3) = (3000 - 2000) / 5 = 1000 / 5 = 200
Dopo 200 km:
  B = 2000
  D = 800

Fase 2: n = 2
  d(2) = (2000 - 1000) / 3 = 1000 / 3 = 333.33...
Dopo 333.33 km:
  B = 1000
  D = 466.67

Fase 3: n = 1
Vai diretto:
risultato = 1000 - 466.67 = 533.33...

Risultato finale
  533.33...
Se si usano km interi: 534

Nota:
Il costo per km cambia a tratti:
n = 3 -> consumo = 5
n = 2 -> consumo = 3
n = 1 -> consumo = 1
La soluzione ottima consiste nel cambiare regime esattamente quando possibile.

Definizione ricorsiva
---------------------
           = B - D,                   se B <= C
f(B, D, C) = f((n-1)*C, D - d(n), C), se d(n) < D
           = B - (2n - 1)*D,          se d(n) >= D
dove:
  n = ceil(B / C)
  d(n) = (B - (n-1)*C) / (2n - 1)

Se il cammello consuma 2 banane per km, allora basta modificare il modello introducendo un parametro:
  k = consumo (banane per km)

1) Modifica fondamentale
Prima:
  consumo per km = 2n - 1
Ora ogni km costa k banane per ogni tratto, quindi:
  consumo per km = k * (2n - 1)
2) Distanza di transizione
Prima:
  d(n) = (B - (n-1)*C) / (2n - 1)
Ora diventa:
 d(n) = (B - (n-1)*C) / (k * (2n - 1))
3) Caso finale (n = 1)
Prima:
  B - D
Ora:
  B - k * D

Algoritmo (modello matematico a tratti)
- Il problema e diviso in fasi
- In ogni fase il numero di viaggi e n = ceil(B/C)
- Il costo per km e k*(2n-1)
- Si avanza finche si puo ridurre n di 1
- Quando n diventa 1, si va direttamente al mercato

(define (banane-finali B D C k)
  ; B = banane iniziali
  ; D = distanza totale
  ; C = capacita cammello
  ; k = consumo (banane per km per ogni tratta)
  (letn ((finito nil) n dn)
    (while (not finito)
      ; numero di carichi necessari
      (setq n (ceil (div B C)))
      ; caso finale: un solo viaggio, si va diretto
      (if (= n 1)
          (begin
            ; consumo lineare: k banane per km
            (setq B (sub B (mul k D)))
            (setq finito true))
          (begin
            ; distanza per ridurre i carichi da n a n-1
            ; consumo per km = k*(2n-1)
            (setq dn (div (sub B (mul (sub n 1) C))
                          (mul k (sub (mul 2 n) 1))))
            ; se non si completa la fase, si termina qui
            (if (>= dn D)
                (begin
                  ; consumo totale sul tratto rimanente
                  (setq B (sub B (mul k (sub (mul 2 n) 1) D)))
                  (setq finito true))
                (begin
                  ; si completa la fase:
                  ; si arriva a (n-1)*C banane
                  (setq B (mul (sub n 1) C))
                  ; si riduce la distanza residua
                  (setq D (sub D dn)))))))
    ; risultato finale (puo essere anche negativo)
    B))

(banane-finali 3000 1000 1000 1)
;-> 533.3333333333333

(banane-finali 3000 1000 1000 2)
;-> -466.6666666666667 (non è possibile arrivare al mercato con le banane)

Aumentare k ha un effetto drastico:
- il primo tratto si accorcia molto
- si consuma troppo velocemente
- non si riesce a coprire tutta la distanza
Per arrivare almeno con 0 banane serve che il consumo totale minimo sia <= B.
Nel caso migliore (n = 1 sempre): k * D <= B
Se non vale nemmeno questa, allora impossibile trasportare anche una sola banana.

Vediamo una funzione che stampa i risultati intermedi.

(define (banane-depositi B D C k)
  ; B = banane iniziali
  ; D = distanza totale
  ; C = capacita
  ; k = consumo per km
  (letn ((finito nil) n dn fase 1 pos 0)
    (println "Start: posizione = 0 banane = " B)
    (while (not finito)
      (setq n (ceil (div B C)))
      (println "Fase " fase ": posizione = " pos " B = " B " D = " D " n = " n)
      (if (= n 1)
          (begin
            (println "  Ultimo tratto fino al mercato")
            (println "  Consumo totale = " (mul k D))
            (setq pos (add pos D))
            (setq B (sub B (mul k D)))
            (println "  Arrivo al mercato: posizione = " pos " banane = " B)
            (setq finito true))
          (begin
            ; distanza per creare nuovo deposito
            (setq dn (div (sub B (mul (sub n 1) C))
                          (mul k (sub (mul 2 n) 1))))
            (println "  Consumo/km = " (mul k (sub (mul 2 n) 1)))
            (println "  Distanza deposito = " dn)
            (if (>= dn D)
                (begin
                  (println "  Non si riesce a creare un nuovo deposito")
                  (setq pos (add pos D))
                  (setq B (sub B (mul k (sub (mul 2 n) 1) D)))
                  (println "  Arrivo finale: posizione = " pos " banane = " B)
                  (setq finito true))
                (begin
                  ; creazione deposito
                  (setq pos (add pos dn))
                  (setq B (mul (sub n 1) C))
                  (println "  Deposito creato a posizione = " pos)
                  (println "  Banane nel deposito = " B)
                  (setq D (sub D dn)))))
      (setq fase (add fase 1))))
    (println "Risultato finale: " B)
    B))

(banane-depositi 3000 1000 1000 1)
;-> Start: posizione = 0 banane = 3000
;-> Fase 1: posizione = 0 B = 3000 D = 1000 n = 3
;->   Consumo/km = 5
;->   Distanza deposito = 200
;->   Deposito creato a posizione = 200
;->   Banane nel deposito = 2000
;-> Fase 2: posizione = 200 B = 2000 D = 800 n = 2
;->   Consumo/km = 3
;->   Distanza deposito = 333.3333333333333
;->   Deposito creato a posizione = 533.3333333333333
;->   Banane nel deposito = 1000
;-> Fase 3: posizione = 533.3333333333333 B = 1000 D = 466.6666666666667 n = 1
;->   Ultimo tratto fino al mercato
;->   Consumo totale = 466.6666666666667
;->   Arrivo al mercato: posizione = 1000 banane = 533.3333333333333
;-> Risultato finale: 533.3333333333333
;-> 533.3333333333333


----------------------
Primi strobogrammatici
----------------------

Un numero strobogrammatico è un numero che è uguale a se stesso se ruotato di 180 gradi (il centro di rotazione si trova a metà del numero).
Ad esempio, i numeri "69", "88" e "818" sono tutti strobogrammatici.
Scrivere una funzione per determinare se un numero è primo strobogrammatico.

Sequenza OEIS A007597:
Strobogrammatic primes.
  11, 101, 181, 619, 16091, 18181, 19861, 61819, 116911, 119611,
  160091, 169691, 191161, 196961, 686989, 688889, 1008001, 1068901,
  1160911, 1180811, 1190611, 1191611, 1681891, 1690691, 1880881,
  1881881, 1898681, 1908061, 1960961, 1990661, 6081809, 6100019,
  6108019, ...

Rappresentiamo la mappa dei numeri (1 -> 1), (8 -> 8), (0 -> 0), (6 -> 9) e (9 -> 6) con una lista associativa.
Usiamo due puntatori (sinistra e destra) che si muovono, rispettivamente verso destra e verso sinistra.
Fino a che non risulta sinistra = destra:
  se il numero corrente di sinistra non compare nella lista di mappatura (link) oppure
  se il carattere che mappa il numero corrente di sinistra è diverso
  dal carattere corrente di destra, allora usciamo dal ciclo e restituiamo nil.
Se terminiamo il ciclo, allora restituiamo true.

(define (strobo? num)
  (local (link s sx dx continua)
    (setq continua true)
    (setq link '(("1" "1") ("0" "0") ("8" "8") ("6" "9") ("9" "6")))
    (setq s (string num))
    (setq sx 0)
    (setq dx (- (length s) 1))
    (while (and continua (<= sx dx))
      (if (or (not (lookup (s sx) link)) (!= (lookup (s sx) link) (s dx)))
          (setq continua nil)
      )
      (++ sx)
      (-- dx)
    )
    ; 'continua': true se il ciclo è terminato senza interruzioni (sx == dx)
    ; 'continua': nil se il ciclo è stato interrotto
    continua))

Proviamo:

(strobo? 69)
;-> true
(strobo? 169)
;-> nil
(strobo? 1691)
;-> true

(filter strobo? (sequence 0 10000)
;-> (0 1 8 11 69 88 96 101 111 181 609 619 689 808 818 888
;->  906 916 986 1001 1111 1691 1881 1961 6009 6119 6699 6889
;->  6969 8008 8118 8698 8888 8968 9006 9116 9696 9886 9966)

(define (primes-to num)
"Generate all prime numbers less than or equal to a given number"
  (cond ((< num 2) '())
        ((< num 3) '(2))
        (true
          (letn ((m (/ (- num 3) 2))
                 (arr (array (+ m 1)))
                 (lim (/ (- (int (sqrt num)) 3) 2))
                 (lst '(2)))
            (for (i 0 lim)
              (when (nil? (arr i))
                (letn ((p (+ (* 2 i) 3))
                       (j (/ (- (* p p) 3) 2)))
                  (for (k j m p (> k m))
                    (setf (arr k) true)))))
            (for (i 0 m)
              (when (nil? (arr i))
                (push (+ (* 2 i) 3) lst -1)))
            lst))))

(time (println (filter strobo? (primes-to 1e7))))
;-> (11 101 181 619 16091 18181 19861 61819 116911 119611
;->  160091 169691 191161 196961 686989 688889 1008001 1068901
;->  1160911 1180811 1190611 1191611 1681891 1690691 1880881
;->  1881881 1898681 1908061 1960961 1990661 6081809 6100019
;->  6108019 6110119 6608099 6610199 6800089 6801089 6860989)
;-> 3135.722


--------------------------------------------
Somma delle cifre delle potenze di un numero
--------------------------------------------

Determinare la sequenza dei numeri N nei seguenti casi:
a) somma-cifre(N) = somma-cifre(N^2)
b) somma-cifre(N) = somma-cifre(N^2) = somma-cifre(N^3)
c) somma-cifre(N) = somma-cifre(N^2) = somma-cifre(N^3) = somma-cifre(N^4)
d) ...

Sequenza OEIS A111434:
Numbers k such that the sums of the digits of k, k^2 and k^3 coincide.
  0, 1, 10, 100, 468, 585, 1000, 4680, 5850, 5851, 5868, 10000, 28845,
  46800, 58500, 58510, 58680, 58968, 100000, 288450, 468000, 585000,
  585100, 586800, 589680, 1000000, 2884500, 4680000, 5850000, 5851000,
  5868000, 5896800, 10000000, ...

(define (digit-sum num)
"Calculate the sum of the digits of an integer"
  (let (out 0)
    (while (!= num 0)
      (setq out (+ out (% num 10)))
      (setq num (/ num 10)))
    out))

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

; Funzione che calcola i numeri N fino ad un dato limite 
; che soddisfano a:
; somma-cifre(N) = somma-cifre(N^p(1)) = ... = somma-cifre(N^p(M))
; dove p(i) sono M potenze da verificare (lista 'powers')
(define (seq powers limite)
  (setq out '())
  (for (num 1 limite)
    (setq values (map (fn(x) (digit-sum (** num x))) powers))
    ;(println num { } values)
    (if (apply = values) (push num out -1)))
  out)

Proviamo:

N e N^2:
(seq '(1 2) 1e2)
;-> (1 9 10 18 19 45 46 55 90 99 100)

N e N^3:
(seq '(1 3) 1e3)
;-> (1 8 10 80 100 171 378 468 487 577 585 586 684 800 1000)

N, N^2 e N^3:
(seq '(1 2 3) 1e3)
;-> (1 10 100 468 585 1000)
(seq '(1 2 3) 1e5)
;-> (1 10 100 468 585 1000 4680 5850 5851 5868 10000 28845
;->  46800 58500 58510 58680 58968 100000)

N, N^2, N^3 e N^4:
(time (println (seq '(1 2 3 4) 1e6)))
;-> (1 10 100 1000 10000 100000 1000000)
;-> 30424.844

N^2 e N^3:
(seq '(2 3) 1e2)
;-> (1 3 6 10 24 28 30 37 60 64 81 87 93 100)

N^2, N^3 e N^4:
(seq '(2 3 4) 1e3)
;-> (1 3 10 30 93 100 219 267 300 387 685 930 1000)

N^2, N^3, N^4 e N^5:
(seq '(2 3 4 5) 1e4)
(1 3 10 30 100 300 1000 3000 10000)

N^2, N^3, N^4, N^5 e N^6:
(seq '(2 3 4 5 6) 1e4)
;-> (1 10 100 1000 10000)

N^3, N^4, N^5, N^6 e N^7:
(seq '(3 4 5 6 7) 1e4)
;-> (1 10 100 1000 10000)

N^4, N^5, N^6 e N^7:
(seq '(4 5 6 7) 1e4)
;-> (1 10 100 414 1000 4140 10000)

N^4, N^5, N^6, N^7 e N^8:
(seq '(4 5 6 7 8) 1e4)
;-> (1 10 100 1000 10000)

N^5, N^6, N^7 e N^8:
(seq '(5 6 7 8) 1e4)
;-> (1 10 100 1000 10000)

N^6, N^7 e N^8:
(seq '(6 7 8) 1e3)
;-> (1 3 10 30 39 91 100 147 300 390 483 910 1000)


---------------------
Conteggio di detenuti
---------------------

Ci sono N persone in N celle isolate tra loro (numerate da 1 a N).
Ogni persona sa che ci sono in tutto N persone.
Una cella vuota con dentro una lavagna.
Ad ogni turno viene chiamata una persona a caso che entra nella cella vuota e:
1) cancella il contenuto la lavagna
2) scrive il proprio numero sulla lavagna
Le persone non sanno chi viene chiamato ogni volta (tranne la persona chiamata).
Le persone non possono comunicare tra loro in alcun modo.

In media quante chiamate sono necessarie affinchè una qualunque delle persone possa dichiarare che sono state chiamate tutte le persone almeno una volta?

Strategia A
-----------

1) Nominiamo un 'contatore' tra le persone (es. il numero 1)
2) Ogni volta che il numero 1 viene chiamato memorizza il valore che trova nella lavagna (prima di cancellarlo e scrivere il proprio numero).
Quando il numero 1 ha contato tutti i numeri, allora può dichiarare che sono state chiamate tutte le persone almeno una volta.

Quindi sono necessarie diverse chiamate prima che il 'contatore' abbia visto tutti i numeri.

(define (simula N)
  (local (lst prev conta stop curr)
    ; lista di persone (tutte con valore 0)
    ; 0: persona non contata
    ; 1: persona contata
    (setq lst (dup 0 N))
    ; il 'contatore' è la persona con indice 0
    ; quindi ha già contato se stessa
    (setf (lst 0) 1)
    ; prima persona scelta
    (setq prev (rand N))
    ; conta il numero di persone scelte
    (setq conta 1)
    ; ciclo fino a che il 'contatore' non ha contato tutte le altre persone...
    (setq stop nil)
    (until stop
      ; persone corrente scelta
      (setq curr (rand N))
      (++ conta)
      ; se la persona corrente è il 'contatore'
      ; e
      ; la persona precedente non era il 'contatore'
      ; e 
      ; la persona precedente non è stata ancora conteggiata
      ; allora marca la persona corrente come conteggiata (lista(prev)=1)
      (if (and (= curr 0) (!= prev 0) (zero? (lst prev)) (setf (lst prev) 1)))
      ;(print prev { } curr { } lst) (read-line)
      ; controllo fine del conteggio:
      ; se tutte le persone hanno valore 1, allora sono state tutte contate e
      ; termina il ciclo
      (if (for-all (fn(x) (= x 1)) lst)
          (setq stop true))
          ; altrimenti la persona corrente diventa la persona precedente
          (setq prev curr)
    )
    ; restituisce il numero di persone scelte
    conta))

(div (apply add (collect (simula 4) 1e5)) 1e5)
;-> 29.336873
(mul 4 4 (log 4))
;-> 22.18

Strategia B
-----------

Tutti sono contatori, quindi usiamo una matrice (una riga per ogni 'contatore')

(define (simula2 N)
  (local (lst prev conta stop curr)
    ; matrice di persone (tutte con valore 0)
    ; ogni riga rappresenta un 'contatore'
    ; 0: persona non contata
    ; 1: persona contata
    (setq lst (dup (dup 0 N) N))
    ; i 'contatore' hanno già contato se stesse
    ; (elementi della diagonale della matrice con valore 1)
    (for (i 0 (- N 1)) (setf (lst i i) 1))
    ; prima persona scelta
    (setq prev (rand N))
    ; conta il numero di persone scelte
    (setq conta 1)
    ; ciclo fino a che un 'contatore' non ha contato tutte le altre persone...
    (setq stop nil)
    (until stop
      ; persone corrente scelta
      (setq curr (rand N))
      (++ conta)
      ; se la persona corrente non è uguale alla precedente
      ; e
      ; la persona precedente non è stata ancora conteggiata
      ; allora marca la persona corrente come conteggiata
      ; per il 'contatore' corrente 'curr'
      (if (and (!= curr prev) (zero? (lst curr prev))) (setf (lst curr prev) 1))
      ;(print prev { } curr { } lst) (read-line)
      ; controllo fine del conteggio per il 'contatore' corrente:
      ; se tutte le persone della riga del 'contatore' corrente hanno valore 1,
      ; allora sono state tutte contate e termina il ciclo
      (if (for-all (fn(x) (= x 1)) (lst curr))
          (setq stop true))
          ; altrimenti la persona corrente diventa la persona precedente
          (setq prev curr)
    )
    ;(println (list curr lst))
    ; restituisce il numero di persone scelte
    conta))

(div (apply add (collect (simula2 4) 1e5)) 1e5)
;-> 15.32489
(mul 4 4)
;-> 16

Spiegazione strategia A
-----------------------
L'unica informazione globale disponibile è la sequenza delle coppie consecutive (prev -> curr)
Ogni persona può usare solo le coppie in cui compare come 'curr'.
Quindi ogni persona vede una sottosequenza casuale di transizioni.
Questi implica che per una persona (i):
- ogni volta che viene chiamata, osserva 'prev'
- quindi sta raccogliendo valori in (1,...,N) tranne i.
Questo è esattamente un "collezione di figurine" su (N-1) elementi, ma con osservazioni 'diluite':
- viene chiamata con probabilità (1/N)
- quindi osserva una figurina ogni ~(N) chiamate

Il tempo per vedere tutti i predecessori:

  (N-1)*log(N-1)

ma ogni osservazione costa (N) chiamate, quindi il tempo vale:

  O(N^2*log(N))

Il contatore registra un numero solo se appare immediatamente prima di lui, cioè raccoglie valori nuovi solo nelle transizioni X -> contatore.
Quindi nel processo spesso si ripetono gli stessi valori.
Questo introduce un fattore di rallentamento r(N) e il tempo diventa:

  O(N^2*log(N) + r(N))

Spiegazione strategia B
-----------------------
La strategia 2 fa in modo che basta fermarsi quando qualunque persona ha completato il suo conteggio.
Quindi stiamo prendendo il minimo su N processi.
Questo riduce il tempo di un fattore circa (N): O(N^2)
Comunque anche in questo caso molti aggiornamenti sono "sprecati".
Quindi anche qui abbiamo un fattore di rallentamento r(N) e il tempo diventa:

  O(N^2 + r(N))

Si può fare meglio?
Non credo.
Infatti per qualunque strategia:
- una persona deve vedere almeno (N-1) persone diverse
- ogni osservazione avviene solo quando viene chiamata
- quindi servono almeno O(N^2) passi

Nota: il problema equivale a trovare un nodo in una catena di Markov casuale che abbia visto tutti gli altri stati come predecessori (e questo richiede inevitabilmente tempo quadratico).


----------------
Primi bilanciati
----------------

Un numero primo bilanciato è un numero primo che è uguale alla media aritmetica dei numeri primi precedente e successivo, cioè se risulta:

          p(n-1) + p(n+1)
  p(n) = -----------------
                 2
  
Sequenza OEIS A006562:
Balanced primes (of order one): primes which are the average of the previous prime and the following prime.
  5, 53, 157, 173, 211, 257, 263, 373, 563, 593, 607, 653, 733, 947, 977,
  1103, 1123, 1187, 1223, 1367, 1511, 1747, 1753, 1907, 2287, 2417, 2677,
  2903, 2963, 3307, 3313, 3637, 3733, 4013, 4409, 4457, 4597, 4657, 4691,
  4993, 5107, 5113, 5303, 5387, 5393, ...

(define (primes-to num)
"Generate all prime numbers less than or equal to a given number"
  (cond ((< num 2) '())
        ((< num 3) '(2))
        (true
          (letn ((m (/ (- num 3) 2))
                 (arr (array (+ m 1)))
                 (lim (/ (- (int (sqrt num)) 3) 2))
                 (lst '(2)))
            (for (i 0 lim)
              (when (nil? (arr i))
                (letn ((p (+ (* 2 i) 3))
                       (j (/ (- (* p p) 3) 2)))
                  (for (k j m p (> k m))
                    (setf (arr k) true)))))
            (for (i 0 m)
              (when (nil? (arr i))
                (push (+ (* 2 i) 3) lst -1)))
            lst))))

(define (balanced limit)
  (letn ( (out '())
          (primi (primes-to limit))
          (len (length primi)) )
  (setq primi (array len primi))
  (for (i 1 (- len 2))
    (if (= (primi i) (div (+ (primi (- i 1)) (primi (+ i 1))) 2))
        (push (primi i) out -1)))
  out))

(balanced 5400)
;-> (5 53 157 173 211 257 263 373 563 593 607 653 733 947 977
;->  1103 1123 1187 1223 1367 1511 1747 1753 1907 2287 2417 2677
;->  2903 2963 3307 3313 3637 3733 4013 4409 4457 4597 4657 4691
;->  4993 5107 5113 5303 5387 5393)


-----------------------
Film a diverse velocità
-----------------------

Abbiamo un film registrato su DVD che ha una durata di 1h 40m 21 s.
Usando il tasto Forward (avanzamento) con un fattore di velocità maggiore di 1, il film viene velocizzato e termina prima.
Con un fattore di velocità minore di 1 il film viene visto al rallentatore e impiega più tempo a terminare.
Scrivere una funzione che prende la durata del film e il fattore di velocità e restituisce il tempo di durata del film a quella velocità.

La funzione è molto semplice, ma bisogna capire che si tratta di una proporzionalità inversa.
Maggiore è il fattore di velocità e minore è la durata del film e viceversa.

(define (film durata fattore)
  (if (zero? fattore) "infinito"
      (div durata fattore)))

Proviamo:

(film 100 1)
;-> 100
(film 100 0.5)
;-> 200
(film 100 1.25)
;-> 80
(film 100 0)
;-> "infinito"
(film 100 100)
;-> 1


----------------
Question on $idx
----------------

Scriviamo una funzione che associa ogni indice di una lista con un indice diverso della stessa lista.

(define (sel-idx lst)
  (let ((out '()) (len (length lst)))
    (dolist (el lst)
      ; select and index of the list different from the current
      (while (= (setq idx (rand len)) $idx))
      ;(println "coppia: " $idx idx)
      (push (list $idx idx) out -1))
    out))

Adesso se lst a un solo valore la funzione dovrebbe entrare in loop (perchè non è possibile selezionare un indice diverso da 0):
(sel-idx '(1))
;-> ((0 0))  --> ERRORE

Se invece lst ha più di un valore allora la funzione dovrebbe funzionare corrrettamente:
(sel-idx '(1 2))
;-> ((0 1) (1 0))  --> OK
(sel-idx '(1 2))
;-> ((0 1) (1 1))  --> ERRORE

Perchè la funzione non produce risultati corretti?

Perchè anche 'until' (e 'while') ha una variabile interna $idx.
Quindi non stiamo confrontando il valore generato 'idx' con $idx di 'dolist', ma con $idx di 'until'.

Per risolvere il problema dobbiamo copiare il valore '$idx' di 'dolist' in una variabile e utilizzarlo all'interno del ciclo 'until'.

(define (sel-idx-ok lst)
  (let ((out '()) (len (length lst)) (tmp 0))
    (dolist (el lst)
      (setq tmp $idx)
      ; select and index of the list different from the current
      (while (= (setq idx (rand len)) tmp))
      ;(println "coppia: " $idx idx)
      (push (list tmp idx) out -1))
    out))

(sel-idx-ok '(1))
;-> ... --> ciclo infinito
(sel-idx-ok '(1 2))
;-> ((0 1) (1 0)) --> OK
(sel-idx '(0 1 2 3 4 5 6 7 8 9))
;-> ((0 4) (1 9) (2 4) (3 8) (4 8) (5 9) (6 1) (7 1) (8 6) (9 6))


-----------------------
Numeri zigzag di Eulero
-----------------------

I numeri a zigzag di Eulero sono sequenze di numeri interi che costituiscono un numero di permutazioni di tali numeri in modo che ogni elemento sia alternativamente maggiore o minore dell'elemento precedente.
c1, c2, c3, c4 è una permutazione alternata in cui:
c1 < c2
c3 < c2
c3 < c4...

Sequenza OEIS A000111:
Euler or up/down numbers: e.g.f. sec(x) + tan(x). Also for n >= 2, half the number of alternating permutations on n letters (A001250).
  1, 1, 1, 2, 5, 16, 61, 272, 1385, 7936, 50521, 353792, 2702765, 22368256,
  199360981, 1903757312, 19391512145, 209865342976, 2404879675441,
  29088885112832, 370371188237525, 4951498053124096, 69348874393137901,
  1015423886506852352, 15514534163557086905, 246921480190207983616,
  4087072509293123892361, ...

Definizione della sequenza ZigZag:

  a(0) = 1
  a(1) = 1
  a(n+1) = (1/2)*Sum[k=0,n](binom n k)*a(k)*a(n-k)

(define (binom num k)
"Calculate the binomial coefficient (n k) = n!/(k!*(n - k)!) (combinations of k elements without repetition from n elements)"
  (cond ((> k num) 0L)
        ((zero? k) 1L)
        ((< k 0) 0L)
        (true
          (let (r 1L)
            (for (d 1 k)
              (setq r (/ (* r num) d))
              (-- num))
          r))))

Versione 1
----------
Applicazione diretta della formula

(define (zz1 N)
  (setq out '(1 1))
  (for (i 2 (- N 1))
    (setq somma 0)
    (for (k 0 (- i 1))
      (++ somma (* (binom (- i 1) k) (out k) (out (- i 1 k)))))
    (push (/ somma 2) out -1))
  out)

(zz1 8)
;-> (1 1 1 2 5 16 61 272)

Versione 1
----------
Creazione del triangolo di Pascal

(define (zz2 N)
  ; out contiene i valori della sequenza ZigZag, inizializzata con a(0)=1 e a(1)=1
  ; coeff contiene la riga corrente del triangolo di Pascal
  (letn ((out '(1 1)) (coeff '(1)))
    ; ciclo principale: calcola a(i) per i da 2 a N-1
    (for (i 2 (- N 1))
      ; costruisce la nuova riga dei coefficienti binomiali (Pascal)
      (letn ((nuovi '(1)))
        ; se la riga precedente ha almeno 2 elementi, somma elementi adiacenti
        (if (> (length coeff) 1)
          (for (k 1 (- (length coeff) 1))
            ; coeff[k-1] + coeff[k]
            (push (+ (coeff (- k 1)) (coeff k)) nuovi -1)))
        ; aggiunge l'ultimo 1 della riga di Pascal
        (push 1 nuovi -1)
        ; aggiorna la riga corrente: coeff = binom(i-1,k)
        (setq coeff nuovi))
      ; inizializza accumulatore per la convoluzione
      (setq somma 0)
      ; calcola la somma: sum binom(i-1,k)*a(k)*a(i-1-k)
      (for (k 0 (- i 1))
        (++ somma (* (coeff k) (out k) (out (- i 1 k)))))
      ; divide per 2 e aggiunge il nuovo termine alla sequenza
      (push (/ somma 2) out -1))
    ; restituisce la lista dei valori
    out))

(zz2 8)
;-> (1 1 1 2 5 16 61 272)

Versione 3
----------
Creazione del triangolo di Pascal (solo la riga corrente)

(define (zz3 N)
  ; out contiene i valori della sequenza ZigZag, inizializzata con a(0)=1 e a(1)=1
  ; coeff contiene la riga corrente del triangolo di Pascal
  (letn ((out '(1 1)) (coeff '(1)))
    ; ciclo principale: calcola a(i) per i da 2 a N-1
    (for (i 2 (- N 1))
      ; costruisce la nuova riga di Pascal: coeff = (1 ... 1)
      ; somma elementi adiacenti della riga precedente usando map su liste allineate
      (setq coeff
        (cons 1
          (append
            ; (slice coeff ...) prende tutti gli elementi tranne l'ultimo
            ; (rest coeff) prende tutti gli elementi tranne il primo
            ; map + somma le coppie corrispondenti → elementi centrali della nuova riga
            (map + (slice coeff 0 (- (length coeff) 1)) (rest coeff))
            '(1))))
      ; inizializza accumulatore per la convoluzione
      (setq somma 0)
      ; calcola la somma: sum binom(i-1,k)*a(k)*a(i-1-k)
      (for (k 0 (- i 1))
        (++ somma (* (coeff k) (out k) (out (- i 1 k)))))
      ; divide per 2 e aggiunge il nuovo termine alla sequenza
      (push (/ somma 2) out -1))
    ; restituisce la lista dei valori della sequenza
    out))

(zz3 8)
;-> (1 1 1 2 5 16 61 272)

Versione 4
----------
Niente Pascal
Niente liste di coefficienti
Niente 'map', 'slice', 'append'
Niente chiamate a 'binom'
Binomiali aggiornati incrementalmente 'c'

(define (zz4 N)
  ; sequenza iniziale
  ;(let ((out '(1 1))) ; con le liste
  (let ( (out (array N '(1))) ) ; con i vettori
    ; calcola a(n) per n>=2
    (for (n 2 (- N 1))
      ; c = binom(n-1,0)
      (let ( (somma 0) (c 1) )
        (for (k 0 (- n 1))
          ; somma += binom(n-1,k) * a(k) * a(n-1-k)
          (++ somma (* c (out k) (out (- n 1 k))))
          ; aggiorna binomiale: binom(n-1,k+1)
          (setq c (/ (* c (- n 1 k)) (+ k 1))))
        ; fattore 1/2 finale
        (setf (out n) (/ somma 2)))) ; con i vettori
        ;(push (/ somma 2) out -1))) ; con le liste
    out))

(zz4 8)
;-> (1 1 1 2 5 16 61 272)

Test di correttezza:
(= (zz1 24) (zz2 24) (zz3 24) (zz4 24))
;-> true

Test di velocità:
(time (zz1 24) 1e4)
;-> 8679.228
(time (zz2 24) 1e4)
;-> 834.119
(time (zz3 24) 1e4)
;-> 622.406
(time (zz4 24) 1e4)
;-> 662.449

Versione 5
----------
Come la versione 4, ma modificata per gestire i big-integer

(define (zz5 N)
  ; sequenza iniziale
  ;(let ((out '(1 1))) ; con le liste
  (let ( (out (array N '(1L))) ) ; con i vettori
    ; calcola a(n) per n>=2
    (for (n 2 (- N 1))
      ; c = binom(n-1,0)
      (let ( (somma 0L) (c 1L) )
        (for (k 0 (- n 1))
          ; somma += binom(n-1,k) * a(k) * a(n-1-k)
          (++ somma (* c (out k) (out (- n 1 k))))
          ; aggiorna binomiale: binom(n-1,k+1)
          (setq c (/ (* c (- n 1 k)) (+ k 1))))
        ; fattore 1/2 finale
        (setf (out n) (/ somma 2)))) ; con i vettori
        ;(push (/ somma 2) out -1))) ; con le liste
    out))

(zz5 8)
;-> (1L 1L 1L 2L 5L 16L 61L 272L)

(zz5 30)
;-> (1L 1L 1L 2L 5L 16L 61L 272L 1385L 7936L 50521L 353792L 2702765L 22368256L
;->  199360981L 1903757312L 19391512145L 209865342976L 2404879675441L
;->  29088885112832L 370371188237525L 4951498053124096L 69348874393137901L
;->  1015423886506852352L 15514534163557086905L 246921480190207983616L
;->  4087072509293123892361L 70251601603943959887872L
;->  1252259641403629865468285L 23119184187809597841473536L)

L'uso dei big-integer rallenta molto la funzione:
 
(time (zz5 24) 1e4)
;-> 2051.94


-----------------------
Primo elemento ripetuto
-----------------------

Trovare il primo elemento ripetuto di una stringa e di una lista.

; stringhe e liste (doppio ciclo)
(define (first-repeat1 obj)
  (let ( (len (length obj)) (out nil) (stop nil) )
    ; Iterate through each element in the list/string
    (for (i 1 (- len 1) 1 stop)
      ; Check if the current element is a repeating element
      (for (j 0 (- i 1) 1 stop)
          (when (= (obj i) (obj j))
            (setq out (obj i))
            ; element found --> stop
            (setq stop true))))
    ; If no repeating element is found, return nil
    ; else return the repeating element
    out))

(first-repeat1 "abcfgdhrtujkda")
;-> "d"
(first-repeat1 "aba")
;-> "a"
(first-repeat1 "abccba")
;-> "c"
(first-repeat1 "-+!@#$%^&*[]!#abccba")
;-> "!"

; stringhe (vettore 256)
(define (first-repeat2 str)
  (let ( (len (length str))
         (arr (array 256 '(nil)))
         (out nil) (stop nil) )
    (dostring (c str stop)
      (if (arr c)
          (set 'out (char c) 'stop true))
          ;else
          (setf (arr c) true))
    out))

(first-repeat2 "abcfgdhrtujkda")
;-> "d"
(first-repeat2 "aba")
;-> "a"
(first-repeat2 "abccba")
;-> "c"
(first-repeat2 "-+!@#$%^&*[]!#abccba")
;-> "!"

; liste (lista visitati + member)
(define (first-repeat3 lst)
  (let ( (visti '()) (out nil) (stop nil) )
    (dolist (el lst stop)
      (if (member el visti)
          (set 'out el 'stop true))
          ;else
          (push el visti))
    out))

(first-repeat3 (explode "abcfgdhrtujkda"))
;-> "d"
(first-repeat3 (explode "aba"))
;-> "a"
(first-repeat3 (explode "abccba"))
;-> "c"
(first-repeat3 (explode "-+!@#$%^&*[]!#abccba"))
;-> "!"

; liste (hash-map)
(define (first-repeat4 lst)
  (new Tree 'visti) ; hash-map degli elementi visitati (globale)
  (let ( (out nil) (stop nil) )
    ; ciclo che cerca il primo elemento ripetuto (se esiste)
    ; altrimenti restituisce nil
    (dolist (el lst stop)
      ; se elemento esiste nella hash-map, allora ferma il ciclo
      (if (visti el)
          (set 'out el 'stop true))
          ;altrimenti inserisce l'elemento nella hash-map
          (visti el el))
    ; elimina la hash-map
    (delete 'visti)
    out))

(first-repeat4 (explode "abcfgdhrtujkda"))
;-> "d"
(first-repeat4 (explode "aba"))
;-> "a"
(first-repeat4 (explode "abccba"))
;-> "c"
(first-repeat4 (explode "-+!@#$%^&*[]!#abccba"))
;-> "!"

Test di velocità:

(time (first-repeat1 "-+!@#$%^&*[]!#abccba") 1e5)
;-> 1713.239
(time (first-repeat2 "-+!@#$%^&*[]!#abccba") 1e5)
;-> 514.908

(setq t (explode "-+!@#$%^&*[]!#abccba"))
(time (first-repeat3 t) 1e5)
;-> 783.902

(setq t (explode "-+!@#$%^&*[]!#abccba"))
(time (first-repeat4 t) 1e5)
;-> 1734.481

(setq t '(1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 1 2))
(time (first-repeat4 t) 1e5)
;-> 1015.599

(setq t (rand 1000 1000))
(time (first-repeat4 t) 1e5)
;-> 4109.701


---------------------------
Primo elemento non ripetuto
---------------------------

Trovare il primo elemento non ripetuto di una stringa e di una lista.

; stringhe (vettore 256)
(define (first-no-repeat1 str)
  (let ( (len (length str))
         (arr (array 256 '(0)))
         (out nil) (stop nil) )
    ; riempimento del vettore
    ; zero volte carattere -> 0
    ; prima volta carattere -> 1
    ; seconda volta carattere -> 2
    ; terza volta carattere -> 2
    ; ...
    (dostring (c str) (if (< (arr c) 2) (++ (arr c))))
    ; per ogni carattere verifica se è comparso una sola volta
    ; (nel vettore vale 1)
    ; il primo carattere è la soluzione
    (dostring (c str stop) (if (= (arr c) 1) (set 'out (char c) 'stop true)))
    out))

(first-no-repeat1 "abcdabcf")
;-> "d"

; liste (lista associativa)
(define (first-no-repeat2 lst)
  (let ( (freq '()) (out nil) (stop nil) )
    ; ciclo per creare la lista delle frequenze (elemento occorrenze)
    (dolist (el lst)
      (if (lookup el freq)
          ; se elemento esiste, allora aumenta la frequenza
          (++ (lookup el freq))
          ;else
          ; altrimenti inserisce l'elemento nella lista 'freq'
          ; con frequenza 1
          (push (list el 1) freq -1)))
    ; cerca e restituisce il primo elemento con frequenza 1 (se esiste)
    ; altrimenti restituisce nil
    (if (find '(? 1) freq match)
        ($0 0)
        nil)))

(setq a '(1 2 3 4 6 5 1 2 3 4))
(first-no-repeat2 a)
;-> 6
(first-no-repeat2 '(1 2 1 2 3 3))
;-> nil

; liste (hash-map)
(define (first-no-repeat3 lst)
  (new Tree 'freq) ; hash-map delle frequenze (globale)
  (let ( (out nil) (stop nil) )
    ; ciclo per creare la hash-map delle frequenze (elemento occorrenze)
    (dolist (el lst)
      (if (freq el)
          ; se elemento esiste, allora aumenta la frequenza
          (freq el (+ $it 1))
          ;else
          ; altrimenti inserisce l'elemento nella hash-map 'freq'
          ; con frequenza 1
          (freq el 1)))
    ; cerca e restituisce il primo elemento con frequenza 1 (se esiste)
    ; altrimenti restituisce nil
    (dolist (el lst stop)
      (if (= (freq el) 1) (set 'out el 'stop true)))
    ; elimina la hash-map
    (delete 'freq)
    out))

(setq a '(1 2 3 4 6 5 1 2 3 4))
(first-no-repeat3 a)
;-> 6
(first-no-repeat3 '(1 2 1 2 3 3))
;-> nil

Test di velocità:
100 elementi:
(setq t (rand 10 100))
(push 111 t (rand 100))
(time (first-no-repeat2 t) 1e4)
;-> 147.164
(time (first-no-repeat3 t) 1e4)
;-> 395.554

1000 elementi:
(setq t (rand 100 1000))
(push 1111 t (rand 1000))
(time (first-no-repeat2 t) 1e4)
;-> 5154.071
(time (first-no-repeat3 t) 1e4)
;-> 4780.08

10000 elementi:
(setq t (rand 1000 10000))
(push 11111 t (rand 10000))
(time (first-no-repeat2 t) 1e4)
;-> 493901.883
(time (first-no-repeat3 t) 1e4)
;-> 48338.125

Le liste associative scalano molto male.


---------------------------------
a^b * c^d * ... = ... * x^y * z^w
---------------------------------

Consideriamo l'equazione a^b * c^d * ... = ... * x^y * z^w, dove a,b,...,w sono numeri interi.
Dato un numero intero pari N < 11, determinare se i numeri da 1 a N generano un'equazione valida.

Esempio
  N = 6
  Equazione: 4^3 = 2^6 * 1^5  --> 64 = 64 * 1 

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
              (swap (lst (indici i)) (lst i)))
            (push lst out -1)
            (++ (indici i))
            (setq i 0))
          (begin
            (setf (indici i) 0)
            (++ i)
          )))
    out))

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

; Funzione che determina le equazioni valide
(define (potenze N dbg)
  (local (out nums len tagli permute sx dx val-sx val-dx)
    (setq out '())
    (setq nums (sequence 1 N))
    (setq len (length nums))
    ; numero di tagli
    (setq tagli (- (/ len 2) 1))
    (setq permute (perm nums))
    ; ciclo per ogni permutazione...
    (dolist (p permute)
      ; creazione delle coppie del tipo: a^b)
      (setq p (explode p 2))
      ; ciclo per provare tutte le combinazioni di '=' nella permutazione corrente
      (for (t 1 tagli)
        ; parte sinistra dell'equazione
        (setq sx (slice p 0 t))
        (setq dx (slice p t))
        ; parte destra dell'equazione
        ; valore parte sinistra
        (setq val-sx (apply * (map (fn(x) (** (x 0) (x 1))) sx)))
        ; valore parte destra
        (setq val-dx (apply * (map (fn(x) (** (x 0) (x 1))) dx)))
        (when dbg
          (println t)
          (println sx { = } dx)
          (println val-sx { } val-dx)
          (read-line))
        ; controllo correttezza dell'equazione
        ; (uguaglianza tra val-sx e val-dx)
        (if (= val-sx val-dx)
          (push (list sx dx val-sx) out -1))))
    ; stampa della soluzione
    (dolist (el out)
      (setq sx (el 0))
      (setq dx (el 1))
      (dolist (c sx) 
        (print (c 0) "^" (c 1))
        (if (!= $idx (- (length sx) 1)) (print " * ")))
      (print " = ")
      (dolist (c dx)
        (print (c 0) "^" (c 1))
        (if (!= $idx (- (length dx) 1))  (print " * ")))
      (println " = " val-sx)) '>))

Proviamo:

(potenze 4)
;->

(potenze 6)
;-> 1^5 * 4^3 = 2^6 = 8192L
;-> 4^3 = 1^5 * 2^6 = 8192L
;-> 4^3 * 1^5 = 2^6 = 8192L
;-> 2^6 = 4^3 * 1^5 = 8192L
;-> 4^3 = 2^6 * 1^5 = 8192L
;-> 2^6 = 1^5 * 4^3 = 8192L
;-> 2^6 * 1^5 = 4^3 = 8192L
;-> 1^5 * 2^6 = 4^3 = 8192L

(potenze 8)
;-> 8^5 = 2^3 * 4^6 * 1^7 = 2293235712L
;-> 4^6 * 2^3 = 8^5 * 1^7 = 2293235712L
;-> ...
;-> 4^7 = 2^5 * 8^3 * 1^6 = 2293235712L
;-> 4^7 = 8^3 * 2^5 * 1^6 = 2293235712L

(time (potenze 10))
;-> 4^3 * 2^7 * 9^5 = 8^1 * 6^10 = 307792887033102336L
;-> 2^7 * 4^3 * 9^5 = 8^1 * 6^10 = 307792887033102336L
;-> ...
;-> 3^5 * 8^7 = 9^2 * 4^10 * 6^1 = 307792887033102336L
;-> 8^7 * 3^5 = 9^2 * 4^10 * 6^1 = 307792887033102336L
;-> 108098.837


---------
Slap sort
---------

"Slap sort" è un termine specialistico utilizzato nella programmazione competitiva per descrivere una strategia di ordinamento che prevede lo spostamento ripetuto del primo elemento maggiore del suo successore alla fine della lista, fino a quando la lista non è ordinata.

Algoritmo
Identificare la prima coppia di elementi A e B tali che (A > B) e spostare A alla fine della lista.
Ripetere questa operazione finché non esistono più coppie di questo tipo (ovvero, la lista è in ordine non-decrescente).

(define (slap-sort lst)
  (let ((ordinata nil))
    (until ordinata
      (setq ordinata true)
      (setq coppia-trovata nil)
      (for (i 0 (- (length lst) 2) 1 coppia-trovata)
        (when (> (lst i) (lst (+ i 1)))
          (setq ordinata nil)
          (setq coppia-trovata true)
          (push (pop lst i) lst -1))
        (println "i: " i { } coppia-trovata { } lst)))
    lst))

(slap-sort '(3 2 1))
;-> i: 0 true (2 1 3)
;-> i: 0 true (1 3 2)
;-> i: 0 nil (1 3 2)
;-> i: 1 true (1 2 3)
;-> i: 0 nil (1 2 3)
;-> i: 1 nil (1 2 3)
;-> (1 2 3)


-------------------------------------------------
Il gioco della catena di parole (Word chain game)
-------------------------------------------------

Le regole del gioco sono le seguenti:
a) un gruppo di giocatori si alternano nel pronunciare le parole di qualche gruppo specifico (es. animali, nomi, verbi, ecc.)
b) al proprio turno ogni giocatore deve pronunciare una parola del gruppo che inizia con la stessa lettera con cui termina il nome dell'ultima parola pronunciata e non deve essere stata già ststa pronunciata nei turni precedenti.
c) Se un giocatore non riesce a trovare una parola valida entro un dato intervallo di tepo, allora viene eliminato.
d) L'ultimo giocatore che rimane è il vincitore.

Scriviamo un programma che simula il gioco tra un utente e il computer.
Per fare questo dobbiamo cambiare alcune regole.
Il gruppo di parole viene passato come parametro (lista di parole).
Il computer può scegliere solo dalla lista di parole data.
L'utente puo scegliere anche parole nuove (che vengono poi aggiunte alla lista iniziale).
Il computer cerca di trovare una parola per cui non esiste altra parola che inizia con il carattere finale della parola scelta. 
Se tale parola non esiste, allora sceglie una parola a caso dalla lista iniziale.

Algoritmo della parola 'quasi' vincente (l'utente può scegliere una parola nuova)
1. Per ogni parola non utilizzata x che inizia con l'ultima lettera della parola precedente, verificate se non esistono parole non utilizzate che iniziano con l'ultima lettera di x (in tal caso, x è vincente).
2. Caso speciale: x inizia e finisce con la stessa lettera.

(setq gruppo '("asino" "ornitorinco" "oca" "ragno" "serpente" "otaria"
               "anatra" "iguana" "opossum"))

; Funzione che cerca parole che iniziano con il carattere 'ch'
(define (words-start ch parole)
  (filter (fn(x) (starts-with x ch)) parole))

(words-start "o" gruppo)
;-> ("ornitorinco" "oca" "otaria" "opossum")

; Funzione che cerca parole che terminano con il carattere 'ch'
(define (words-end ch parole)
  (filter (fn(x) (ends-with x ch)) parole))

(words-end "o" gruppo)
;-> ("asino" "ornitorinco" "ragno")

; Funzione che cerca una parola 'quasi' vincente
(define (winning-move)
  (local (out possible-words stop ch valid-words)
    (setq out "")
    ; crea una lista 'possible-words' di tutte le parole che
    ; iniziano con il primo carattere della parola 'curr-word'
    (setq possible-words (words-start (curr-word -1) parole))
    ;(println possible-words)
    (setq stop nil)
    ; ciclo per ogni parola possibile...
    (dolist (el possible-words stop)
      ; carattere finale della parola possibile corrente
      (setq ch (el -1))
      ; trova tutte le parole valide che terminano con
      ; l'ultimo carattere della parola possibile corrente 'ch'
      (setq valid-words (words-start ch parole))
      ; Caso delle parole che iniziano e terminano con lo stesso carattere:
      ; rimuove, se esiste, la parola possibile corrente dalla lista delle
      ; parole valide.
      ; In questo modo si considera la parola possibile corrente
      ; come candidata a parola vincente
      (replace el valid-words)
      ;(print el { } ch { } valid-words) (read-line)
      ; se non esistono parole valide, (cioè se 'valid-words' è vuota)
      ; allora la parola possibile corrente 'el' è una parola vincente
      (when (= valid-words '())
            (setq out el)
            (setq stop true)))
    out))

(setq curr-word "ragno")
(setq parole gruppo)
(winning-move)
;-> "opossum"

; Funzione che calcola una parola per il computer
(define (computer-move)
  (local (win possible-words)
    (println "Computer: ")
    ;(println parole)
    ; elimina la parola corrente dalla lista delle parole
    (replace curr-word parole)
    ; cerca una parola vincente
    (setq win (winning-move))
    (if (!= win "")
        (begin ; parola vincente
          (setq curr-word win)
          ; elimina la parola dalla lista delle parole
          (replace curr-word parole)
          ; aggiunge la parola alle parole usate
          (push curr-word parole-usate -1)
          ;stampa la parola corrente
          (println "Parola corrente vincente: " curr-word))
        (begin ; parola casuale
          ; crea una lista 'possible-words' di tutte le parole che
          ; iniziano con il primo carattere della parola 'curr-word'
          (setq possible-words (words-start (curr-word -1) parole))
          ; parola trovata nella lista delle parole
          (if possible-words
              (begin ; esistono parole valide --> ne sceglie una a caso
                ; sceglie una parola a caso tra quelle possibili
                (setq curr-word (parole (rand (length parole))))
                ; elimina la parola dalla lista delle parole
                (replace curr-word parole)
                ; aggiunge la parola alle parole usate
                (push curr-word parole-usate -1)
                ;stampa la parola corrente
                (println "Parola corrente: " curr-word))
              (begin ; non esistono parole valide --> fine del gioco
                (setq end-game true)
                (println "Non esistono parole valide: " parole)
                (println "Fine del gioco.")))))))

; Funzione che permette all'utente di scegliere una parola
(define (user-move)
    (local (input possible-words)
    (println "User: ")
    ;(println parole)
    ; elimina la parola corrente dalla lista delle parole
    (replace curr-word parole)
    ; crea una lista 'possible-words' di tutte le parole che
    ; iniziano con il primo carattere della parola 'curr-word'
    (setq possible-words (words-start (curr-word -1) parole))
    ;(println possible-words)
    (print "Parola che inizia con " (curr-word -1) "? ") (read-line)
    (setq input (current-line))
    (cond
        ; Parola che non inizia con la lettera corretta
          ((!= (input 0) (curr-word -1)) ;
            ;(println curr-word)
            (println input " non inizia con " (curr-word -1))
            (user-move))
        ; Parola già usata
          ((find input parole-usate)
            (println input " è stata già usata.")
            (user-move))
        ; Parola trovata nella lista delle parole
            ((find input possible-words)
            ; elimina la parola dalla lista delle parole
            (replace input parole)
            ; aggiunge la parola alle parole usate
            (push input parole-usate -1)
            ; imposta la parola corrente
            (setq curr-word input)
            ;stampa la parola corrente
            (println "Parola corrente: " curr-word))
          ; Parola non trovata nella lista delle parole
          (true
            (println input " non esiste nelle parole.")
            (print "è una parola valida? (y/n) " (read-line))
            (if (= (current-line) "y")
                (begin
                  ; aggiorna la lista globale delle parole (lst)
                  (push input lst -1)
                  ; aggiunge la parola alle parole usate
                  (push input parole-usate -1)
                  ; imposta la parola corrente
                  (setq curr-word input)
                  ;stampa la parola corrente
                  (println "Parola corrente: " curr-word))
                ;else
                (user-move))))))

; Funzione che simula il gioco
(define (words-game gruppo curr-word)
  ; lista iniziale delle parole
  (setq parole gruppo)
  ; lista di tutte le parole (iniziale + quelle nuove inserite dall'utente)
  (setq lst gruppo)
  ; lista delle parole usate
  (setq parole-usate '())
  ; flag per la fine del gioco
  (setq end-game nil)
  ; ciclo del gioco
  (until end-game
    (user-move)
    (computer-move))
  (println "Parole rimaste:" parole)
  (println "Parole totali:" lst) '>)

Proviamo:

(setq curr-word "oca")
(setq gruppo '("asino" "ornitorinco" "oca" "ragno" "serpente" "otaria"
               "anatra" "iguana" "opossum"))
(words-game gruppo "oca")

(words-game gruppo "oca")
;-> User:
;-> Parola che inizia con a? anatra
;-> Parola corrente: anatra
;-> Computer:
;-> Parola corrente: serpente
;-> User:
;-> Parola che inizia con e? epor
;-> epor non esiste nelle parole.
;-> è una parola valida? (y/n) y
;-> yParola corrente: epor
;-> Computer:
;-> Parola corrente: ornitorinco
;-> User:
;-> Parola che inizia con o? otte
;-> otte non esiste nelle parole.
;-> è una parola valida? (y/n) n
;-> nUser:
;-> Parola che inizia con o? oriz
;-> oriz non esiste nelle parole.
;-> è una parola valida? (y/n) y
;-> yParola corrente: oriz
;-> Computer:
;-> Non esistono parole valide: ("asino" "ragno" "otaria" "iguana" "opossum")
;-> Fine del gioco.
;-> Parole rimaste:("asino" "ragno" "otaria" "iguana" "opossum")
;-> Parole totali:("asino" "ornitorinco" "oca" "ragno" "serpente" "otaria" "anatra" "iguana" "opossum"
;->  "epor" "oriz")

============================================================================

