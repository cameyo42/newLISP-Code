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


---
i^i
---

  i^i = e^(-2*pi*n - pi/2), per n elemento di Z

La scelta di n è determinata dal ramo del logaritmo utilizzato per l'esponenziale complesso.

(define (i^i n)
  (let ( (e 2.7182818284590451)
         (pi 3.1415926535897931) )
    (pow e (sub (mul -2 pi n) (div pi 2)))))

(i^i 0)
;-> 0.2078795763507619

Sequenza OEIS A049006:
Decimal expansion of i^i = exp(-pi/2).
  2, 0, 7, 8, 7, 9, 5, 7, 6, 3, 5, 0, 7, 6, 1, 9, 0, 8, 5, 4, 6, 9, 5, 5, 6,
  1, 9, 8, 3, 4, 9, 7, 8, 7, 7, 0, 0, 3, 3, 8, 7, 7, 8, 4, 1, 6, 3, 1, 7, 6,
  9, 6, 0, 8, 0, 7, 5, 1, 3, 5, 8, 8, 3, 0, 5, 5, 4, 1, 9, 8, 7, 7, 2, 8, 5,
  4, 8, 2, 1, 3, 9, 7, 8, 8, 6, 0, 0, 2, 7, 7, 8, 6, 5, 4, 2, 6, 0, 3, 5, ...

Dimostrazione
Per definizione generale, per z complesso ed esponente complesso w si ha:
  z^w = e^(w * Log(z))
  dove Log(z) = ln|z| + i*Arg(z), e Arg(z) e' multivalore:
  Arg(z) = theta + 2*pi*n  con n intero.
Prendiamo z = i.
Il modulo di i e' |i| = 1, quindi ln|i| = 0.
Gli argomenti possibili di i sono:
  Arg(i) = pi/2 + 2*pi*n
  con n intero.
Quindi:
  Log(i) = i * (pi/2 + 2*pi*n)
Ora calcoliamo i^i:
  i^i = e^(i * Log(i))
      = e^(i * i * (pi/2 + 2*pi*n))
      = e^(-(pi/2 + 2*pi*n))
Pertanto, per ogni n intero:
  i^i = e^(-pi/2 - 2*pi*n)
Il valore principale (cioe' per n = 0) e':
  i^i = e^(-pi/2)
che è un numero reale positivo (0.2078795763507619...).

Vedi anche "Espansione digitale di un numero generato da una formula (con BC)" su "Note libere 33".


----------------------------------------------
Spostamenti di 3 punti lungo la retta numerica
----------------------------------------------

Abbiamo tre punti in posizioni diverse su una retta numerica.
Le posizioni sono date da tre numeri interi a, b e c.
Ad ogni mossa dobbiamo:
Prendere un punto dalla posizione più a sinistra o più a destra e metterlo in qualsiasi posizione intera vuota compresa tra i punti estremi.
Ad esempio, se i punti si trovano nelle posizioni x, y, z dove x < y < z:
Possiamo prendere il punto nella posizione x e spostarlo in qualsiasi posizione p dove x < p < z e p != y.
Oppure possiamo prendere il punto nella posizione z e spostarlo in qualsiasi posizione p dove x < p < z e p != y.
Le mosse possibili terminano quando i tre punti occupano posizioni consecutive (es. 3, 4, 5).
Detrminare:
1) Il numero massimo di mosse possibili per terminare il gioco
2) Il numero minimo di mosse necessarie per terminare il gioco

1) Calcolo del numero massimo di passi

Possiamo continuare a spostare i punti finali una posizione alla volta verso il punto centrale, utilizzando tutte le posizioni vuote disponibili finché i tre punti non saranno finalmente consecutive.

Esempio:

  x = 2, y = 4, z = 7

      x   y     z
  - - - - - - - - - - -
  0 1 2 3 4 5 6 7 8 9

a) Sposta x vicino a y (cioè x va in posizione 3)
   mosse = y - x - 1 = 4 - 2 - 1 = 1

        x y     z
  - - - - - - - - - - -
  0 1 2 3 4 5 6 7 8 9

b) Sposta z vicino a y (cioè z va in posizione 5):
   mosse = z - y - 1 = 7 - 4 - 1 = 2 passi

      x y z
  - - - - - - - - - - -
  0 1 2 3 4 5 6 7 8 9

c) Numero massimo mosse = (y - x - 1) + (z - y - 1) = z - x - 2

2) Calcolo del numero minimo di passi

Ordiniamo i punti in modo crescente nelle posizioni x, y, z, dove x < y < z.
Se i punti sono già consecutivi (z - x = 2), non servono mosse.
Se due punti sono già consecutivi o hanno un solo spazio tra loro (es 2,3,5), possiamo terminare in una sola mossa posizionando il terzo punto accanto a loro (es. 5 in 4).
Altrimenti, servono 2 mosse: prima avviciniamo un punto estremo per creare uno spazio di dimensione 2 o inferiore, poi terminiamo con la seconda mossa.
Se y - x <= 2 oppure z - y <= 2, allora, possiamo terminare in una sola mossa.

Esempio:

  x = 2, y = 4, z = 7

      x   y     z
  - - - - - - - - - - -
  0 1 2 3 4 5 6 7 8 9

Poichè (y - x) = 1 < 2, allora basta una sola mossa: spostare z in 3.

     x z y
 - - - - - - - - - - -
 0 1 2 3 4 5 6 7 8 9

(define (mosse a b c)
  (local (x y z massimo minimo)
    ; ordina a, b, c in modo crescente --> x, y, z
    (map set '(x y z) (sort (list a b c)))
    ;(println x { } y { } z)
    (setq massimo 0)
    (setq minimo 0)
    ; verifica se due punti sono consecutivi
    (when (> (- z x) 2)
        ; Calcolo del massimo
        ; Possiamo spostare i punti una posizione alla volta
        ; Numero degli spazi vuoti tra i punti x e z
        (setq massimo (- z x 2))
        ; Calcolo del minimo
        ; Se uno dei due gap (y-x o z-y) è <= 2, possiamo farlo in 1 mossa
        ; Altrimenti, abbiamo bisogno di 2 mosse
        (if (or (<= (- y x) 2) (<= (- z y) 2))
            (setq minimo 1)
        ;else
            (setq minimo 2)))
    (list massimo minimo)))

Proviamo:

(mosse 7 2 4)
;-> (3 1)
(mosse 2 3 1)
;-> (0 0)


-------------------------------------------------------
Rettangoli che coprono esattamente un'area rettangolare
-------------------------------------------------------

Abbiamo una lista di rettangoli in cui ogni rettangolo è rappresentato come:
 rettangolo[i] = (x1 y1 x2 y2).
Ogni rettangolo è allineato agli assi (parallelo agli assi x e y) con:
  (x1, y1) coordinate dell'angolo in basso a sinistra
  (x2, y2) coordinate dell'angolo in alto a destra
Determinare se tutti questi rettangoli insieme formano una copertura esatta di un'area rettangolare.

Affinchè i rettangoli formino una copertura esatta:
a) I rettangoli devono combinarsi per formare un unico rettangolo più grande senza spazi vuoti.
b) Non devono esserci aree sovrapposte tra i rettangoli.
c) Ogni punto all'interno del rettangolo più grande deve essere coperto esattamente una volta.

Per verificare questo usiamo due condizioni:
1) Verifica dell'area
La somma delle aree di tutti i singoli rettangoli deve essere uguale all'area del rettangolo di delimitazione (formato dalle coordinate minima e massima).

2) Conteggio dei vertici:
Ogni vertice di un rettangolo viene conteggiato in base al numero di rettangoli che condividono quel vertice
Deve risultare:
- Per i quattro vertici del rettangolo complessivo --> conteggio = 1.
- Per tutti i vertici interni (dove i rettangoli si incontrano) --> conteggio = 2 oppure 4.
  Questo perchè i vertici interni sono condivisi da rettangoli adiacenti.

Algoritmo
- Calcolare l'area totale e trovare le coordinate del rettangolo di delimitazione (min-x, min-y) e (max-x, max-y).
- Contare quante volte ogni vertice appare in tutti i rettangoli.
- Verificare che l'area totale corrisponda all'area del rettangolo di delimitazione.
- Verificare che i quattro vertici esterni appaiano esattamente una volta.
- Verificare che tutti i vertici interni appaiano 2 o 4 volte (condivisione valida tra rettangoli).
- Se tutte le condizioni sono soddisfatte:
  allora i rettangoli formano una copertura esatta,
  altrimenti, non formano una copertura esatta.

Quindi l'algoritmo verifica due condizioni:
1. L'area totale di tutti i rettangoli è uguale all'area del rettangolo di delimitazione
2. I punti d'angolo compaiono con le frequenze corrette:
- I 4 angoli del rettangolo di delimitazione compaiono esattamente una volta
- Tutti gli altri angoli compaiono 2 o 4 volte (condivisi dai rettangoli adiacenti)

(define (cover rects)
  (local (out vertici area min-x min-y max-x max-y cur-vert)
    (setq out true)
    ; lista associativa con elementi: ((x y) occorrenze)
    (setq vertici '())
    ; area del rettangolo di delimitazione
    (setq area 0)
    ; coordinate vertice basso-sinistra del rettangolo di delimitazione
    (setq min-x (rects 0 0)) (setq min-y (rects 0 1))
    ; coordinate vertice alto-destra del rettangolo di delimitazione
    (setq max-x (rects 0 2)) (setq max-y (rects 0 3))
    ; ciclo per ogni rettangolo...
    (dolist (r rects)
      (map set '(x1 y1 x2 y2) r)
      ;(println x1 { } y1 { } x2 { } y2)
      ; aggiorna area totale
      (++ area (* (- x2 x1) (- y2 y1)))
      ; aggiorna le coordinate del rettangolo di delimitazione
      (setq min-x (min min-x x1))
      (setq min-y (min min-y y1))
      (setq max-x (max max-x x2))
      (setq max-y (max max-y y2))
      ; vertici del rettangolo corrente
      (setq cur-vert (list (list x1 y1) (list x1 y2) (list x2 y2) (list x2 y1)))
      ;(println cur-vert)
      ; aggiornamento delle occorrenze dei vertici
      (dolist (v cur-vert)
        (if (lookup (list v) vertici)
            ; aggiorna le occorrenze del vertice (+ 1)
            (setf (lookup (list v) vertici) (+ $it 1))
            ;altrimenti inserisce il vertice nella lista associativa
            (push (list v 1) vertici -1))))
    ;(println vertici)
    ; Verifica area
    (if (!= area (* (- max-x min-x) (- max-y min-y))) (setq out nil))
    ; Verifica del conteggio dei vertici del rettangolo di delimitazione (= 1)
    (if (or (!= (lookup (list (list min-x min-y)) vertici) 1)
            (!= (lookup (list (list min-x max-y)) vertici) 1)
            (!= (lookup (list (list max-x max-y)) vertici) 1)
            (!= (lookup (list (list max-x min-y)) vertici) 1))
        (setq out nil))
    ; elimina i vertici con occorrenza uguale a 1
    ; (i vertici del rettangolo di delimitazione)
    (pop-assoc (list (list min-x min-y)) vertici)
    (pop-assoc (list (list min-x max-y)) vertici)
    (pop-assoc (list (list max-x max-y)) vertici)
    (pop-assoc (list (list max-x min-y)) vertici)
    (println vertici)
    ; Verifica del conteggio dei vertici interni al rettangolo (= 2 o = 4)
    (if-not (for-all (fn(x) (or (= (x 1) 2) (= (x 1) 4))) vertici) (setq out nil))
    out))

Proviamo:

(setq R1 '((1 1 3 3) (3 1 4 2) (3 2 4 4) (1 3 2 4) (2 3 3 4)))
(cover R1)
;-> true

(setq R2 '((1 1 2 3) (1 3 2 4) (3 1 4 2) (3 2 4 4)))
(cover R2)
;-> nil

(setq R3 '((1 1 2 3) (1 3 2 4) (3 1 4 2) (3 2 4 4)))
(cover R3)
;-> nil

(setq R4 '((0 0 2 2) (2 2 4 4) (0 2 2 4) (2 0 4 2)))
(cover R4)
;-> true

(setq R5 '((0 0 2 2) (2 2 4 4) (0 2 2 4) (2 0 4 2) (2 0 4 2)))
(cover R5)
;-> nil

(setq R6 '((0 0 2 2) (2 2 4 4) (0 2 2 4) (2 0 4 2) (1 1 3 3)))
(cover R6)
;-> nil


-------------------------------------------------------
Palindromi che sommati/moltiplicati generano palindromi
-------------------------------------------------------

; Funzione che verifica se un intero positivo (Int64) è palindromo:
(define (pali? num) (= (string num) (reverse (string num))))

Sequenza OEIS A057135:
Palindromes whose square is a palindrome
Also palindromes whose sum of squares of digits is less than 10.
  0, 1, 2, 3, 11, 22, 101, 111, 121, 202, 212, 1001, 1111, 2002, 10001,
  10101, 10201, 11011, 11111, 11211, 20002, 20102, 100001, 101101,
  110011, 111111, 200002, 1000001, 1001001, 1002001, 1010101, 1011101,
  1012101, 1100011, 1101011, 1102011, 1110111, 1111111, ...

(define (pali-square limite)
  (let ( (out '(0 1)) )
    (for (num 2 limite)
      (if (and (pali? num) (pali? (* num num)))
          (push num out -1)))
    out))

(setq a057135 (pali-square 1111111))
;-> (0 1 2 3 11 22 101 111 121 202 212 1001 1111 2002 10001
;->  10101 10201 11011 11111 11211 20002 20102 100001 101101
;->  110011 111111 200002 1000001 1001001 1002001 1010101 1011101
;->  1012101 1100011 1101011 1102011 1110111 1111111)

Sequenza OEIS A118596:
Palindromes in base 5 (written in base 5).
  0, 1, 2, 3, 4, 11, 22, 33, 44, 101, 111, 121, 131, 141, 202, 212, 222,
  232, 242, 303, 313, 323, 333, 343, 404, 414, 424, 434, 444, 1001, 1111,
  1221, 1331, 1441, 2002, 2112, 2222, 2332, 2442, 3003, 3113, 3223, 3333,
  3443, 4004, 4114, 4224, 4334, 4444, 10001, ...
Equivalently, palindromes k (written in base 10) such that 2*k is a palindrome.
Bruno Berselli, Sep 12 2018

Sequenza OEIS A118595:
Palindromes in base 4 (written in base 4).
  0, 1, 2, 3, 11, 22, 33, 101, 111, 121, 131, 202, 212, 222, 232, 303,
  313, 323, 333, 1001, 1111, 1221, 1331, 2002, 2112, 2222, 2332, 3003,
  3113, 3223, 3333, 10001, 10101, 10201, 10301, 11011, 11111, 11211,
  11311, 12021, 12121, 12221, 12321, 13031, ...
Equivalently, palindromes k (written in base 10) such that 3*k is a palindrome.
Bruno Berselli, Sep 12 2018

Sequenza OEIS A118594:
Palindromes in base 3 (written in base 3).
  0, 1, 2, 11, 22, 101, 111, 121, 202, 212, 222, 1001, 1111, 1221, 2002,
  2112, 2222, 10001, 10101, 10201, 11011, 11111, 11211, 12021, 12121,
  12221, 20002, 20102, 20202, 21012, 21112, 21212, 22022, 22122, 22222,
  100001, 101101, 102201, 110011, 111111, 112211, 120021, ...
Equivalently, palindromes k (written in base 10) such that 4*k is a palindrome.
Bruno Berselli, Sep 12 2018

(define (pali-mul-K limite k)
  (let ( (out '(0 1)) )
    (for (num 2 limite)
      (if (and (pali? num) (pali? (* k num)))
          (push num out -1)))
    out))

(setq a118596 (pali-mul-K 10001 2))
;-> (0 1 2 3 4 11 22 33 44 101 111 121 131 141 202 212 222
;->  232 242 303 313 323 333 343 404 414 424 434 444 1001 1111
;->  1221 1331 1441 2002 2112 2222 2332 2442 3003 3113 3223 3333
;->  3443 4004 4114 4224 4334 4444 10001)

(setq a118595 (pali-mul-K 13031 3))
;-> (0 1 2 3 11 22 33 101 111 121 131 202 212 222 232 303
;->  313 323 333 1001 1111 1221 1331 2002 2112 2222 2332 3003
;->  3113 3223 3333 10001 10101 10201 10301 11011 11111 11211
;->  11311 12021 12121 12221 12321 13031)
 13031)

(setq a118594 (pali-mul-K 120021 4))
;-> (0 1 2 11 22 101 111 121 202 212 222 1001 1111 1221 2002
;->  2112 2222 10001 10101 10201 11011 11111 11211 12021 12121
;->  12221 20002 20102 20202 21012 21112 21212 22022 22122 22222
;->  100001 101101 102201 110011 111111 112211 120021)

Sequenza OEIS A069748:
Numbers k such that k and k^3 are both palindromes.
  0, 1, 2, 7, 11, 101, 111, 1001, 10001, 10101, 11011, 100001, 101101,
  110011, 1000001, 1001001, 1100011, 10000001, 10011001, 10100101,
  11000011, 100000001, 100010001, 100101001, 101000101, 110000011,
  1000000001, 1000110001, 1010000101, 1100000011, 10000000001, ...

Sequenza OEIS A056810:
Numbers whose fourth power is a palindrome.
  0, 1, 11, 101, 1001, 10001, 100001, 1000001, 10000001, 100000001,
  1000000001, 10000000001, 100000000001, ...

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))
          
; Funzione che verifica se un intero positivo (Int64 o Big-Integer) è palindromo:
(define (pali-big? num)
  (let ( (str (string num)) )
    ; remove final "L" (if exists)
    (if (= (str -1) "L") (pop str -1))
    (= str (reverse (copy str)))))

(pali-big? "123411111111111111111111111111111111111114321L")
(pali-big? "1234114321")
;-> true

(define (pali-pow-K limite k)
  (let ( (out '(0 1)) )
    (for (num 2 limite)
      (if (and (pali-big? num) (pali-big? (** num k)))
          (push num out -1)))
    out))

(= (pali-pow-K 1111111 2) a057135)
;-> true

(setq a069748 (pali-pow-K 1e7 3))
;-> (0 1 2 7 11 101 111 1001 10001 10101 11011 100001 101101
;->  110011 1000001 1001001 1100011)

(setq a056810 (pali-pow-K 1e7 4))
;-> (0 1 11 101 1001 10001 100001 1000001)

Sequenza OEIS A057148:
Palindromes only using 0 and 1 (i.e., base-2 palindromes).
  0, 1, 11, 101, 111, 1001, 1111, 10001, 10101, 11011, 11111, 100001,
  101101, 110011, 111111, 1000001, 1001001, 1010101, 1011101, 1100011,
  1101011, 1110111, 1111111, 10000001, 10011001, 10100101, 10111101,
  11000011, 11011011, 11100111, 11111111, 100000001, ...

; Funzione che calcola i palindromi binari fino ad un dato limite
(define (pali-binary limite)
  (let ( (out '(0 1)) )
    (for (num 2 limite)
      (setq bin (bits num))
      (if (= bin (reverse (copy bin)))
          (push (int bin 0 10) out -1)))
    out))

(pali-binary 257)
;-> (0 1 11 101 111 1001 1111 10001 10101 11011 11111 100001
;->  101101 110011 111111 1000001 1001001 1010101 1011101 1100011
;->  1101011 1110111 1111111 10000001 10011001 10100101 10111101
;->  11000011 11011011 11100111 11111111 100000001)

============================================================================

