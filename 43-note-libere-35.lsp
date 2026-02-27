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
      (set 's "")
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
      (set 's "")
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

============================================================================

