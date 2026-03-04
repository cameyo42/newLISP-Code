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
      (if (lst i) (set 'ultimo i)))
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

============================================================================

