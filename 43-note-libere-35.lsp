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

============================================================================

