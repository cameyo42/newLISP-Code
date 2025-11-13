================

 NOTE LIBERE 34

================

    "Aiuto..." Anonimi

------------------------------------------------------
Somme e differenze alternate del powerset di una lista
------------------------------------------------------

Per (1, 2, 3, ..., N) e ciascuno dei suoi sottoinsiemi non vuoti, una somma-differenza alternata è definita nel modo seguente:
disporre i numeri del sottoinsieme in ordine decrescente e quindi, iniziando dal più grande, sommare e sottrarre alternativamente i numeri successivi.
Ad esempio, la somma-differenza alternata per (1, 2, 3, 6, 9) è 9 - 6 + 3 - 2 + 1 = 5 e per (5) è semplicemente 5.
Scrivere una funzione che calcola la somma alternata totale per un dato N.

(define (powerset-i lst)
"Generate all sublists of a list (binary mask)"
  (if (= lst '()) lst
  ;else
      (let ((out '(())) (n (length lst)) (group '()))
        (for (mask 1 (- (<< 1 n) 1))
          (setq group '())
          (for (i 0 (- n 1))
            (if (!= (& mask (<< 1 i))) (push (lst i) group -1)))
          (push group out -1))
        out)))

Funzione che calcola la somma-differenza alternata di una lista di numeri interi:

(define (sum-diff lst after)
  (if (null? lst '()) 0
  ;else
    (let ( (s-d 0) )
      ; after = true --> segni alternati (+ - + ...) dopo il primo termine
      (if after (setq s-d (pop lst)))
      (dolist (el lst)
        (if (even? $idx)
            (++ s-d el)
            (-- s-d el)))
      s-d)))

; Funzione che calcola la somma alternata totale per un dato N
(define (solve N ord after)
  (if (zero? N) 0
  ;else
    (let ( ; somma totale delle somme/differenze alternate
           (sum-sd 0)
           ; insieme potenza della sequenza (1..N) (2^N elemeti)
           (powset (powerset-i (sequence N 1))) )
  ; per ogni elemento dell'insieme...
  (dolist (el powset)
    ; aggiorna la somma totale delle somme/differenze alternate
    ; (la lista corrente 'el' viene ordinata)
    (++ sum-sd (sum-diff (sort el >))))
  sum-sd)))

Proviamo:
(solve 12)
;-> 24576

(map solve (sequence 0 15))
;-> (0 1 4 12 32 80 192 448 1024 2304 5120 11264 24576 53248 114688 245760)

Sequenza OEIS A001787:
a(n) = n*2^(n-1).
  0, 1, 4, 12, 32, 80, 192, 448, 1024, 2304, 5120, 11264, 24576, 53248,
  114688, 245760, 524288, 1114112, 2359296, 4980736, 10485760, 22020096,
  46137344, 96468992, 201326592, 419430400, 872415232, 1811939328,
  3758096384, 7784628224, 16106127360, 33285996544, ...

Questo risultato vale per le sequenze ordinate in modo decrescente e con i segni alternati dal primo termine.
Vediamo quali sequenze vengono generate se cambiamo questi due parametri ('order' e 'after').

; Funzione che calcola la somma alternata totale per un dato N
; order --> > or <
; after --> true or nil
(define (solve N order after)
  (if (zero? N) 0
  ;else
    (let ( ; somma totale delle somme/differenze alternate
           (sum-sd 0)
           ; insieme potenza della sequenza (1..N) (2^N elemeti)
           (powset (powerset-i (sequence N 1))) )
      ; per ogni elemento dell'insieme potenza...
      (dolist (el powset)
        ; aggiorna la somma totale delle somme/differenze alternate
        ; (la lista corrente 'el' viene ordinata in modo decrescente)
        (if after
            (++ sum-sd (sum-diff (sort el order) true)))
            (++ sum-sd (sum-diff (sort el order))))
      sum-sd)))

Proviamo:

1) order = > e after = nil (stessa sequenza del problema):
(map (fn(x) (solve x '> nil)) (sequence 0 15))
;-> (0 1 4 12 32 80 192 448 1024 2304 5120 11264 24576 53248 114688 245760)

2) order = > e after = true
(map (fn(x) (solve x '> true)) (sequence 0 15))
;-> (0 2 10 34 98 258 642 1538 3586 8194 18434 40962 90114 196610
;->  425986 917506)

Sequenza OEIS A036799:
a(n) = 2 + 2^(n+1)*(n-1).
  0, 2, 10, 34, 98, 258, 642, 1538, 3586, 8194, 18434, 40962, 90114, 196610,
  425986, 917506, 1966082, 4194306, 8912898, 18874370, 39845890, 83886082,
  176160770, 369098754, 771751938, 1610612738, 3355443202, 6979321858,
  14495514626, 30064771074, 62277025794, ...

3) order = < e after = nil
(map (fn(x) (solve x '< nil)) (sequence 0 15))
;-> (0 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384)

Sequenza OEIS A000079:
Powers of 2: a(n) = 2^n.
  1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384,
  32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, ...

4) order = < e after = true
(map (fn(x) (solve x '< true)) (sequence 0 15))
(0 2 8 22 52 114 240 494 1004 2026 4072 8166 16356 32738 65504 131038)

Sequenza OEIS A005803:
Second-order Eulerian numbers: a(n) = 2^n - 2*n.
  1, 0, 0, 2, 8, 22, 52, 114, 240, 494, 1004, 2026, 4072, 8166, 16356,
  32738, 65504, 131038, 262108, 524250, 1048536, 2097110, 4194260, 8388562,
  16777168, 33554382, 67108812, 134217674, 268435400, 536870854, 1073741764,
  2147483586, ...)


-------------------------------------
Segni alternati nelle liste di numeri
-------------------------------------

Data una lista di numeri interi, scrivere una funzione che restituisce una lista con gli stessi elementi della lista data con i segni alternati.
La funzione deve permettere di generare i segni alternati partendo con "+" --> (+, -, +, -, ...) oppure con "-"  --> (-, +, -, +, ...).

Esempi:

lista = (2 7 3 9 4)
output1 = (2 -7 3 -9 4)
output2 = (-2 7 -3 9 -4)

lista = (-1 4 -3 -2)
output1 = (1 -4 3 -2)
output2 = (-1 4 -3 2)

Metodo 1:
---------
Crea una lista (1 -1 1 -1 ...) e poi la moltiplica con la lista data.

(define (alt-sign1 lst minus)
  (let ( (len (length lst)) (lst (map abs lst)) )
    (if minus
      (setq unos (flat (dup '(-1 1) (+ (/ len 2) 1))))
      (setq unos (flat (dup '(1 -1) (+ (/ len 2) 1)))))
    (map * lst unos)))

(setq L '(2 7 3 9 4))
(alt-sign1 L)
;-> (2 -7 3 -9 4)
(alt-sign1 L true)
;-> (-2 7 -3 9 -4)

(setq L '(-1 4 -3 -2))
(alt-sign1 L)
;-> (1 -4 3 -2)
(alt-sign1 L true)
;-> (-1 4 -3 2)

(setq L '(-1 3 4 5 8 -2 -2 -2 8 6 5 6 9 0 0 1))
(alt-sign1 L)
;-> (1 -3 4 -5 8 -2 2 -2 8 -6 5 -6 9 0 0 -1)
(alt-sign1 L true)
;-> (-1 3 -4 5 -8 2 -2 2 -8 6 -5 6 -9 0 0 1)

Metodo 2:
---------
Ciclo per ogni elemento della lista.

(define (alt-sign2 lst minus)
  (let ( (out '()) (len (length lst)) )
    (if minus (setq x -1) (setq x 1))
    (dolist (el lst)
      (push (* (abs el) x) out -1)
      (setq x (* x -1)))
    out))

(setq L '(2 7 3 9 4))
(alt-sign2 L)
;-> (2 -7 3 -9 4)
(alt-sign2 L true)
;-> (-2 7 -3 9 -4)

(setq L '(-1 4 -3 -2))
(alt-sign2 L)
;-> (1 -4 3 -2)
(alt-sign2 L true)
;-> (-1 4 -3 2)

(setq L '(-1 3 4 5 8 -2 -2 -2 8 6 5 6 9 0 0 1))
(alt-sign1 L)
;-> (1 -3 4 -5 8 -2 2 -2 8 -6 5 -6 9 0 0 -1)
(alt-sign1 L true)
;-> (-1 3 -4 5 -8 2 -2 2 -8 6 -5 6 -9 0 0 1)

Test di correttezza:

(setq t (rand 10 100))
(= (alt-sign1 t) (alt-sign2 t))
;-> true
(= (alt-sign1 t true) (alt-sign2 t true))
;-> true

Test di velocità:

(setq t (rand 10 1000))
(time (alt-sign1 t) 10000)
;-> 906.197
(time (alt-sign2 t) 10000)
;-> 1203.34

Se vogliamo creare una sequenza da 1 a N con segni alternati possiamo usare la seguente funzione:

(define (alt-seq N minus)
  (cond ((true? minus)
          (define (change-sign x) (if (> x 0) (- (+ x 1)) (+ (- x) 1)))
          (series -1 change-sign N))
        (true
          (define (change-sign x) (if (< x 0) (+ (- x) 1) (- (+ x 1))))
          (series 1 change-sign N))))

Proviamo:

(alt-seq 10)
;-> (1 -2 3 -4 5 -6 7 -8 9 -10)

(alt-seq 10 true)
;-> (-1 2 -3 4 -5 6 -7 8 -9 10)


-----------------------
Monete e banconote Euro
-----------------------

I tagli delle monete in Euro sono:

  0.01  0.02  0.05  0.10  0.20  0.50  1.00  2.00

I tagli delle banconote in Euro sono:

  5.00  10.00  20.00  50.00  100.00  200.00  500.00

Scrivere una funzione che stampa i i tagli delle monete e delle banconote utilizzando come separatore decimale la virgola "," (quello in uso nella maggior parte delle nazioni europee).

  Monete = 0,01 0,02 0,05 0,10 0,20 0,50 1,00 2,00
  Banconote = 5,00 10,00 20,00 50,00 100,00 200,00 500,00

(map (fn(x) (replace "." x ",")) (map (fn(y) (format "%.2f" y)) '(.1 .2 .5 .1 .5 1 2)))
;-> ("0,10" "0,20" "0,50" "0,10" "0,50" "1,00" "2,00")

(map (fn(x) (replace "." x ",")) (map (fn(y) (format "%.2f" y)) '(5 10 20 50 100 200 500)))
;-> ("5,00" "10,00" "20,00" "50,00" "100,00" "200,00" "500,00")


-----------------------------------------
Modi di ottenere N moltiplicando K numeri
-----------------------------------------

Dato un numero intero N <= 10000 e un numero intero K, scrivere una funzione che determina i modi in cui è possibile ottenere N moltiplicando K numeri.
Non è possibile utilizzare 1 come moltiplicando.

(define (factorizations num)
"Calculate all the factorizations of an integer number"
  (let (afc '())
    (factorizations-aux num '() num)))
; funzione ausiliaria
(define (factorizations-aux num parfac parval)
  (let ((newval parval) (i (- num 1)))
    (while (>= i 2)
      (cond ((zero? (% num i))
              (if (> newval 1) (setq newval i))
              (if (and (<= (/ num i) parval) (<= i parval) (>= (/ num i) i))
                  (begin
                    (push (append parfac (list i (/ num i))) afc -1)
                    (setq newval (/ num i))))
              (if (<= i parval)
                  (factorizations-aux (/ num i) (append parfac (list i)) newval)
              )))
      (-- i))
    (sort (unique (map sort afc)))))

(factorizations 100)
;-> ((2 2 5 5) (2 2 25) (2 5 10) (2 50) (4 5 5) (4 25) (5 20) (10 10))

Funzione che restituisce i modi in cui è possibile ottenere N moltiplicando K numeri.
(define (multi N K)
  (let ( (out '()) (factorize (factorizations N)) )
    (dolist (f factorize)
      (if (= (length f) K)
          (push f out -1)))
    out))

Proviamo:

(multi 100 2)
;-> ((2 50) (4 25) (5 20) (10 10))

(multi 100 3)
;-> ((2 2 25) (2 5 10) (4 5 5))

(multi 120 4)
;-> ((2 2 2 15) (2 2 3 10) (2 2 5 6) (2 3 4 5))

Se volessimo considerare anche 1 come moltiplicando dobbiamo aggiungere alla soluzione precedente:
1) la lista (1 1 ... N) (con (K-1) numeri 1)
2) le liste di lunghezza (K - 1) aggiungendo (1)
3) le liste di lunghezza (K - 2) aggiungendo (1 1)
...


-----------
Alberi trie
-----------

Un albero trie è una struttura dati ad albero utilizzata per memorizzare e recuperare in modo efficiente le chiavi in un insieme di stringhe.
Le funzioni principali sono:
1) Crea Trie: crea un trie vuoto
2) Insert: inserisce una stringa nel trie
3) Delete: elimina una stringa dal trie
4) Search: verifica se una data stringa esiste nel trie
5) Start-with-prefix: verifica se un esiste nel trie almeno una stringa che inizia con un dato prefisso

Come esempio vediamo una funzione di ricerca.

; trie con le parole: an, ant, all, allot, alloy, aloe, are, ate, be
(set 'aTrie '(
  (a (n () (t)) (l (l () (o (t) (y))) (o (e))) (r (e)) (t (e)))
  (b (e))))

; Funzione di ricerca di una stringa nel trie
(define (search-trie trie word)
  ;(println word { } trie)
  (cond
      ( (and (empty? trie) (not (empty? word))) nil)
      ( (and (empty? trie) (empty? word)) true)
      ( (and (empty? (first trie)) (empty? word)) true)
      ( (and (empty? (first trie)) (empty? word)) true)
      ( (empty? word) nil)
      ( (and
          (set 'tail (assoc (first word) trie))
          (search-trie (rest tail) (rest word))) true)))

Proviamo:

(search-trie aTrie '(a l))
;-> nil
(search-trie aTrie '(a l l))
;-> true
(search-trie aTrie '(a n t))
;-> true
(search-trie aTrie '(a n t s))
;-> nil
(search-trie aTrie '(a l l o))
;-> nil
(search-trie aTrie '(a l l o t))
;-> true
(search-trie aTrie '(a l l o y))
;-> true
(search-trie aTrie '(a l o))
;-> nil
(search-trie aTrie '(a l o e))
;-> true
(search-trie aTrie '(b e))
;-> true
(search-trie aTrie '(b))
;-> nil


--------------------------------------------------
Funzione inversa di una funzione razionale lineare
--------------------------------------------------

Per trovare l'inversa di una funzione razionale lineare f(x) = (a*x + b)/(c*x + d), il procedimento generale è puramente algebrico:

  si scambia il ruolo di x e y e si risolve rispetto a y.

Vediamo un esempio:

  f(x) = (2x - 1)/(x - 3)

1. Sostituire f(x) con y:
   y = (2x - 1) / (x - 3)
2. Scambiare x e y:
   x = (2y - 1) / (y - 3)
3. Moltiplicare entrambi i lati per (y - 3):
   x * (y - 3) = 2y - 1
4. Espandere:
   xy - 3x = 2y - 1
5. Portare i termini con y da una parte:
   xy - 2y = 3x - 1
6. Fattorizzare y:
   y * (x - 2) = 3x - 1
7. Isolare y:
   y = (3x - 1) / (x - 2)
Risultato finale:
   f-inv(x) = (3x - 1) / (x - 2)

In generale l'inversa di una funzione razionale lineare:

          (a*x + b)
  f(x) = -----------
          (c*x + d)

si ottiene risolvendo x = (a*y + b) / (c*y + d) rispetto a y.

Algebricamente:

1. Moltiplicare entrambi i lati per (c*y + d):
   x*(c*y + d) = a*y + b
2. Raggruppare i termini con y:
   x*(c*y - a*y) = b - x*d
3. Fattorizzare y:
   y*(c*x - a) = b - d*x
4. Isolare y:
   y = (b - d*x)/(c*x - a) = (-d*x + b)/(-c*x + a)

Quindi, se la funzione originale ha coefficienti (a b c d), i coefficienti dell'inversa (e f g h) sono:

  (e, f, g, h) = (-d, b, -c, a)

(define (inverse-coeffs coeffs)
  ;; coeffs = (a b c d)
  ;; restituisce (e f g h) per l'inversa
  (list (- (coeffs 3)) (coeffs 1) (- (coeffs 2)) (coeffs 0)))

(inverse-coeffs '(2 -1 1 -3))
; -> (-3 1 -1 2)
che corrisponde all'inversa: y = (-3x + 1)/(-x +2) = (1 - 3x)/(2 - x)

(inverse-coeffs '(2 -3 -5 4))
;-> (-4 -3 5 2)

(inverse-coeffs '(1 4 -1 3))
;-> (-3 4 1 1)

Vediamo una funzione che calcola l'inversa partendo da una formula scritta in stringa (come "f(x) = (2x - 1)/(x - 3)") e la restituisce in modo formattato:

(define (inversa-ascii fx)
  (local (coeffs a b c d fx-inv)
    ; elimina gli spazi
    (replace " " fx "")
    ; rimuove "f(x) ="
    (setq fx (slice fx (+ (find "=" fx) 1)))
    ; inserisce 1 a x
    (replace "(x" fx "(1x")
    ; inserisce -1 a -x
    (replace "(-x" fx "(-1x")
    ; toglie il + a x
    (replace "(+x" fx "(1x")
    (replace "(+1x" fx "(1x")
    ; elimina parentesi
    (replace "(" fx "")
    (replace ")" fx "")
    ; estrae i coefficienti dalla stringa fx (interi e float)
    (setq coeffs (map float (find-all {-?.?\d+(\.\d+)?} fx)))
    ; estrae i coefficienti dalla stringa fx (solo interi)
    ;(setq coeff (map (fn(x) (int x 0 10)) (find-all {-?\d+} fx)))
    ; coefficienti della funzione inversa
    (setq a (- (coeffs 3)))
    (setq b (coeffs 1))
    (setq c (- (coeffs 2)))
    (setq d (coeffs 0))
    ; crea la stringa di output: "f(x) = ..."
    (setq fx-inv "f(x) = (")
    ; crea il numeratore
    (cond ((= a 1)  (extend fx-inv "x"))
          ((= a -1) (extend fx-inv "-x"))
          ((> a 0)  (extend fx-inv (string a "x")))
          ((< a 0)  (extend fx-inv (string "-" (abs a) "x"))))
    (cond ((> b 0)  (extend fx-inv (string " + " b)))
          ((< b 0)  (extend fx-inv (string " - " (abs b)))))
    (extend fx-inv ")/(")
    ; crea il denominatore
    (cond ((= c 1)  (extend fx-inv "x"))
          ((= c -1) (extend fx-inv "-x"))
          ((> c 0)  (extend fx-inv (string c "x")))
          ((< c 0)  (extend fx-inv (string "-" (abs c) "x"))))
    (cond ((> d 0)  (extend fx-inv (string " + " d)))
          ((< d 0)  (extend fx-inv (string " - " (abs d)))))
    (extend fx-inv ")")
    fx-inv))

Proviamo:

(inversa-ascii "f(x) = (x + 4)/(- x + 3)")
;-> "f(x) = (-3x + 4)/(x + 1)"
(inversa-ascii "f(x) =(x +4)/(-x + 3)")
;-> "f(x) = (-3x + 4)/(x + 1)"
(inversa-ascii "f(x) =(+x +4.2)/(-1.1x + 3.2)")
;-> "f(x) = (-3x + 4.2)/(x + 1)"
(inversa-ascii "f(x) = (2x - 3)/ (- 5 x + 3)")
;-> "f(x) = (-3x - 3)/(5x + 2)"
(inversa-ascii "f(x) = (- x +4.2)/(-1.1x + 3.2)")
;-> "f(x) = (-3x + 4.2)/(x - 1)"
(inversa-ascii "f(x) = (+2x - 3)/ (- 5 x + 3)")
;-> "f(x) = (-3x - 3)/(5x + 2)"

Nota: equazioni funzionali
Per un'equazione del tipo f(g(x)) = h(x) la soluzione è f(x) = h(g-inv(x))dove g-inv(x) è l'inversa di g(x) (quando esiste).
Se g(x) è una funzione razionale lineare, allora possiamo usare la funzione 'inversa-ascii' per calcolare la funzione inversa g-inv(x).

============================================================================

