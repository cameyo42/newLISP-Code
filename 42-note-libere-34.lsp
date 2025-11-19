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
        ; se il vertice corrente è stato già incontrato...
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


----------------------------------
Elementi con determinate frequenze
----------------------------------

Data una lista di N elementi e un intero K, scrivere una funzione che trova tutti gli elementi che:
  1) compaiono K volte          (=)  oppure
  2) compaiono più di K volte   (>)  oppure
  3) compaiono meno di K volte  (<)  oppure
  4) compaiono minimo K volte   (>=) oppure
  5) compaiono massimo K volte  (<=) oppure
  6) non compaiono K volte      (!=)

Esempi:
  lista = (1 2 2 3 3 3 4 4 5)
  K = 2
  1) (=)  2  (2 4)
  2) (>)  2  (3)
  3) (<)  2  (1 5)
  4) (>=) 2  (2 3 4)
  5) (<=) 2  (1 2 4 5)
  6) (!=) 2  (1 3 5)

Versione 1 (lista associativa):
-------------------------------

(define (freq1 lst k op)
  (let (nums '()) ; lista associativa con elementi: (numero occorrenze)
    ; operatore di default: op --> =
    (setq op (or op =))
    ; ciclo per ogni elemento della lista...
    (dolist (el lst)
      ; se l'elemento corrente è stato già incontrato...
      ; (cioè esiste nella lista associativa)
      (if (lookup el nums)
          ; allora aggiorna le occorrenze dell'elemento (+1)
          (setf (lookup el nums) (+ $it 1))
          ; altrimenti inserisce l'elemento '(el 1)'
          ; nella lista associativa 'nums'
          (push (list el 1) nums -1)))
    ; estrae dalla lista associativa i numeri le cui occorrenze
    ; verificano le condizioni richieste
    (map first (filter (fn(x) (op (x 1) k)) nums))))

Proviamo:

(setq L '(1 2 2 3 3 3 4 4 5))
(freq1 L 2 =)
;-> (2 4)
(freq1 L 2 >)
;-> (3)
(freq1 L 2 <)
;-> (1 5)
(freq1 L 2 >=)
;-> (2 3 4)
(freq1 L 2 <=)
;-> (1 2 4 5)
(freq1 L 2 !=)
;-> (1 3 5)

Versione 2 (primitive unique e count):
--------------------------------------

(define (freq2 lst k op)
  (let ( (unici (unique lst)) ; elementi unici della lista
         (op (or op =)) ) ; operatore di default: op --> =
    (map first (filter (fn(x) (op (x 1) k))
                        (map list unici (count unici lst))))))

Test di correttezza:

(define (test lst k)
  (if (!= (freq1 lst k)    (freq2 lst k))    (println "u"))
  (if (!= (freq1 lst k =)  (freq2 lst k =))  (println "="))
  (if (!= (freq1 lst k >=) (freq2 lst k >=)) (println ">="))
  (if (!= (freq1 lst k <=) (freq2 lst k <=)) (println "<="))
  (if (!= (freq1 lst k >)  (freq2 lst k >))  (println ">"))
  (if (!= (freq1 lst k <)  (freq2 lst k <))  (println "<"))
  (if (!= (freq1 lst k !=) (freq2 lst k !=)) (println "!=")))

(setq L (rand 10 100))
(map (curry test L) (sequence 1 5))
;-> (nil nil nil nil nil)

Test di velocità:

100 elementi:
(setq L (rand 10 100))
(time (freq1 L) 1e4)
;-> 164.383
(time (freq2 L) 1e4)
;-> 196.915

1000 elementi:
(setq L (rand 100 1000))
(time (freq1 L) 1e3)
;-> 540.112
(time (freq2 L) 1e3)
;-> 354.558

10000 elementi:
(setq L (rand 1000 10000))
(time (freq1 L) 1e2)
;-> 4730.804
(time (freq2 L) 1e2)
;-> 514.975

Nota: le liste associative vanno bene fino ad un migliaio di elementi.


----------------------------
Divisione solo con addizioni
----------------------------

Scrivere una funzione che effettua la divisione intera tra due numeri interi utilizzando solo l'addizione.

(define (divide a b)
  (let ( (step b) (res 0) )
    (while (<= b a) (++ b step) (++ res))
    res))

(divide 10 3)
;-> 3
(divide 24 6)
;-> 4
(divide 30 7)
;-> 4

Test di correttezza:

(for (i 1 1000)
  (setq a (+ (rand 1000) 1))
  (setq b (+ (rand 1000) 1))
  (if (!= (divide a b) (/ a b))
      (println a { } b)))

Vesione code-golf (61 caratteri):

(define(d a b)(let((s b)(r 0))(until(> b a)(++ b s)(++ r))r))


--------------
Fibonattoriale
--------------

Il Fibonattoriale di un numero N è il prodotto dei prini N numneri di Fibonacci.

Sequenza OEIS A003266:
Product of first n nonzero Fibonacci numbers F(1), ..., F(n).
  1, 1, 1, 2, 6, 30, 240, 3120, 65520, 2227680, 122522400, 10904493600,
  1570247078400, 365867569267200, 137932073613734400, 84138564904377984000,
  83044763560621070208000, 132622487406311849122176000,
  342696507457909818131702784000, ...

Versione 1 (fibonacci + fattoriale):

(define (fibo-i num)
"Calculate the Fibonacci number of an integer number"
  (if (zero? num) 0L
  ;else
  (local (a b c)
    (setq a 0L b 1L c 0L)
    (for (i 0 (- num 1))
      (setq c (+ a b))
      (setq a b)
      (setq b c))
    a)))

(define (f! n)
  (if (zero? n) 0
      ;else
      (apply * (map fibo-i (sequence 1 n)))))

(map f! (sequence 1 18))
;-> (1L 1L 2L 6L 30L 240L 3120L 65520L 2227680L 122522400L 10904493600L
;->  1570247078400L 365867569267200L 137932073613734400L 84138564904377984000L
;->  83044763560621070208000L 132622487406311849122176000L
;->  342696507457909818131702784000L)

Versione 2 (formula):

Formula:
  a(n+3) = a(n+1)*a(n+2)/a(n) + a(n+2)^2/a(n+1)

Ponendo m = n+3 otteniamo:

  a(m) = a(m-2)*a(m-1)/a(m-3) + a(m-1)^2/a(m-2)

(define (a003266 limite)
  (let (a '(1L 1L 1L))
    (for (m 3 limite)
      (push (+ (/ (* (a (- m 2)) (a (- m 1))) (a (- m 3)))
               (/ (* (a (- m 1)) (a (- m 1))) (a (- m 2)))) a -1))
    a))

(a003266 18)
;-> (1L 1L 2L 6L 30L 240L 3120L 65520L 2227680L 122522400L 10904493600L
;->  1570247078400L 365867569267200L 137932073613734400L 84138564904377984000L
;->  83044763560621070208000L 132622487406311849122176000L
;->  342696507457909818131702784000L)


------------------
(a! * b!) / c! = 1
------------------

Trovare tutte le triple (a, b, c) che soddisfano (a! * b!) / c! = 1, con a,b,c distinti.

La condizione: (a! * b!) / c! = 1 è equivalente a:  (a! * b!) = c!

; Funzione che restituisce un vettore con i numeri fattoriali da 0 a limit:
(define (factorial limit)
  (let (f (array (+ limit 1) '(1L)))
    (for (n 2 limit)
      (setf (f n) (* (f (- n 1)) n)))
    f))

(setq f (factorial 10))
;-> (1L 1L 2L 6L 24L 120L 720L 5040L 40320L 362880L 3628800L)

; Funzione che cerca tutte le triple (a,b,c) che soddisfano:
; (a! * b!) / c! = 1 con a,b,c che variano da 1 a limite
; (a! * b!) = c!
(define (triple limite)
  (let ( (out '()) (f (factorial limite)) )
    (for (a 1 (- limite 2))
      (for (b (+ a 1) (- limite 1))
        (for (c (+ b 1) limite)
          (if (= (* (f a) (f b)) (f c))
              (push (list a b c) out -1)))))
    out))

Proviamo:

(triple 100)
;-> ((3 5 6) (4 23 24) (6 7 10))

Test di velocità:

(time (triple 100))
;-> 38.205
(time (triple 150))
;-> 187.257
(time (triple 200))
;-> 660.562
(length (triple 200))
;-> 4

Test di correttezza:

(define (fact-i num)
"Calculate the factorial of an integer number"
  (if (zero? num)
      1
      (let (out 1L)
        (for (x 1L num)
          (setq out (* out x))))))

; Funzione che verifica una soluzione (a b c)
(define (check sol)
  (= (* (fact-i (sol 0)) (fact-i (sol 1))) (fact-i (sol 2))))

(filter nil? (map check (triple 100)))
;-> ()

Versione ottimizzata (esce dal ciclo 'for' 'c' quando (a!*b! < c!):

(define (triples limite)
  (let ( (out '()) (f (factorial limite)) )
    (for (a 1 (- limite 2))
      (for (b (+ a 1) (- limite 1))
        (setq prod (* (f a) (f b)))
        (setq stop nil)
        (for (c (+ b 1) limite 1 stop)
          (if (< prod (f c))
              (setq stop true)
              ;else
              (if (= prod (f c)) (push (list a b c) out -1))))))
    out))

Proviamo:

(triples 100)
;-> ((3 5 6) (4 23 24) (6 7 10))

Test di velocità:

(time (triples 100))
;-> 12.1
(time (triples 150))
;-> 30.015
(time (triples 200))
;-> 71.05
(length (triples 200))
;-> 4

Test di correttezza:

(= (triple 200) (triples 200))
;-> true

(filter nil? (map check (triples 100)))
;-> ()

(time (println (triples 500))))
;-> ((3 5 6) (4 23 24) (5 119 120) (6 7 10))
;-> 1646.005

(time (println (triples 1000))))
;-> ((3 5 6) (4 23 24) (5 119 120) (6 7 10) (6 719 720))
;-> 22608.996


---------------------------------------------
Ricerca delle componenti connesse in un grafo
---------------------------------------------

Data una matrice che rappresenta un grafo (matrice di adiacenza), scrivere una funzione che restituisce il numero delle componenti connesse e la lista di elementi di ogni componente.

; ------------------------------------------------------------------
; Funzione che identifica tutte le componenti connesse di un grafo
; rappresentato tramite matrice di adiacenza.
; ------------------------------------------------------------------
(define (disjoint-graph matrix)
  (local (len visited components current-comp)
    ; ------------------------------------------------------------------
    ; DFS (Depth-First Search)
    ; Questa procedura esplora in profondità un'intera componente
    ; connessa a partire dal nodo "current". Oltre a segnare il nodo come
    ; visitato, inserisce ogni nodo raggiungibile nella lista temporanea
    ; "current-comp", che rappresenta l'isola attualmente in esplorazione.
    ; L'algoritmo ricalca il comportamento tradizionale del DFS:
    ;   - si entra in un nodo
    ;   - si marcano come visitati tutti i nodi connessi
    ;   - si prosegue ricorsivamente fino ad esaurire la componente
    ; ------------------------------------------------------------------
    (define (dfs current)
      (setf (visited current) true)
      (push current current-comp -1)
      (for (next 0 (- (length matrix) 1))
        (when (and (not (visited next))
                   (= ((matrix current) next) 1))
          (dfs next))))
    ; ------------------------------------------------------------------
    ; Scopo: identificare tutte le componenti connesse di un grafo
    ; rappresentato tramite matrice di adiacenza.
    ; Struttura del processo:
    ;   1. Prepara un array "visited" per ricordare quali nodi sono già
    ;      stati esplorati.
    ;   2. Scansiona tutti i nodi. Ogni nodo non visitato è l'inizio di
    ;      una nuova componente connessa.
    ;   3. Prima di chiamare DFS, si azzera "current-comp", la lista che
    ;      accumula i nodi della componente corrente.
    ;   4. Dopo che DFS ha esplorato completamente l'isola, "current-comp"
    ;      contiene tutti i nodi raggiungibili dal nodo iniziale.
    ;   5. Si aggiunge questa lista all'elenco globale "components".
    ;   6. Al termine, il numero di componenti è la lunghezza della lista
    ;      "components".
    ; Risultato finale: (numero-delle-componenti, lista-di-tutte-le-isole)
    ; ------------------------------------------------------------------
    (setq len (length matrix))
    (setq visited (array len '(nil)))
    (setq components '())
    (for (i 0 (- len 1))
      (when (not (visited i))
        ; nuova isola: si prepara la lista dei nodi che la compongono
        (setq current-comp '())
        ; DFS visita tutta la componente a partire da i
        (dfs i)
        ; si aggiunge la componente trovata alla lista totale
        (push current-comp components -1)))
    ; Ritorna sia il numero delle isole che la lista stessa
    (list (length components) components)))

Proviamo:

(setq M '((1 1 0)
          (1 1 0)
          (0 0 1)))
(disjoint-graph M)
;-> (2 ((0 1) (2)))

(setq M '((1 0 0)
          (0 1 0)
          (0 0 1)))
(disjoint-graph M)
;-> (3 ((0) (1) (2)))

(setq M '((1 1 0 0)
          (1 1 0 0)
          (0 0 1 1)
          (0 0 1 1)))
(disjoint-graph M)
;-> (2 ((0 1) (2 3)))

(setq M '((1 1 1 1)
          (1 1 1 1)
          (1 1 1 1)
          (1 1 1 1)))
(disjoint-graph M)
;-> (1 ((0 1 2 3)))


---------------------------
Isole nelle matrici binarie
---------------------------

Abbiamo una matrice binaria M x N.
Un'isola è un gruppo celle con valore 1 collegate in 4 (orizzontali o verticali) o 8 direzioni (anche le diagonali).
L'area di un'isola è il numero di celle con valore 1 nell'isola.
Scrivere una funzione che calcola il numero di 1 per ogni isola e le coordinate di ogni 1.
Se non ci sono isole, restituisce 0.

Le 4 direzioni ortogonali (su, giù, sinistra, destra):
  (-1 0)  --> su        -->  riga sopra, stessa colonna
  (1 0)   --> giù       -->  riga sotto, stessa colonna
  (0 -1)  --> sinistra  -->  stessa riga, colonna a sinistra
  (0 1))  --> destra    -->  stessa riga, colonna a destra

Le 4 direzioni diagonali (senza includere le ortogonali) sono:
  (-1 -1)  --> alto a sinistra   -->  riga sopra, colonna a sinistra
  (-1 1)   --> alto a destra     -->  riga sopra, colonna a destra
  (1 -1)   --> basso a sinistra  -->  riga sotto, colonna a sinistra
  (1 1))   --> basso a destra    -->  riga sotto, colonna a destra

Versione iterativa (ricorsione con stack esplicito)
---------------------------------------------------
Algoritmo:
1) Scansiona tutta la matrice.
2) Quando trova un '1' non visitato avvia una DFS iterativa con uno stack.
3) DFS iterativa:
   - Marca la cella come visitata.
   - Aggiunge le coordinate alla lista dell'isola.
   - Chiama sé stessa per tutte le 4/8 celle vicine che siano ancora '1' e non visitate.
   - Quando la DFS finisce, si è ottenuta un'intera isola.
4) Alla fine della chiamata ricorsiva, aggiunge '(area coords)' alla lista dei risultati.
5) Se non ci sono isole, restituisce '0'.

(define (isole-I matrix diag)
  (letn ( (R (length matrix))
          (C (length (matrix 0)))
          (visited (array R C '(nil)))
          (ris '())
          (i 0) (j 0)
          (stack '())
          (coords '())
          (r 0) (c 0)
          (cell '())
          (nr 0) (nc 0)
          (d '((-1 0) (1 0) (0 -1) (0 1))) )
    ; diag = true --> 8 direzioni
    (if diag (setq d '((-1 0) (1 0) (0 -1) (0 1) (-1 -1) (-1 1) (1 -1) (1 1))))
    (for (i 0 (- R 1))
      (for (j 0 (- C 1))
        (when (and (= ((matrix i) j) 1) (not (visited i j)))
          ; nuova isola -> inizializza coords e stack
          (setq coords '())
          (setq stack (list (list i j)))
          (setf (visited i j) true) ; marca subito la cella iniziale
          (while stack
            (setq cell (pop stack))
            (setq r (cell 0))
            (setq c (cell 1))
            (push (list r c) coords -1)
            ; esplora tutte le 8 direzioni
            (dolist (dir d)
              (setq nr (+ r (dir 0)))
              (setq nc (+ c (dir 1)))
              (when (and (>= nr 0) (< nr R) (>= nc 0) (< nc C)
                         (= ((matrix nr) nc) 1)
                         (not (visited nr nc)))
                (push (list nr nc) stack -1)
                (setf (visited nr nc) true))))
          (push (list (length coords) coords) ris -1))))
    (if (null? ris) 0 ris)))

Proviamo:

(setq M '((0 0 1 0 0 0 0 1 0 0 0 0 0)
          (0 0 0 0 0 0 0 1 1 1 0 0 0)
          (0 1 1 0 1 0 0 0 0 0 0 0 0)
          (0 1 0 0 1 1 0 0 1 0 1 0 0)
          (0 1 0 0 1 1 0 0 1 1 1 0 0)
          (0 0 0 0 0 0 0 0 0 0 1 0 0)
          (0 0 0 0 0 0 0 1 1 1 0 0 0)
          (0 0 0 0 0 0 0 1 1 0 0 0 0)))

(isole-I M)
;-> ((1 ((0 2)))
;->  (4 ((0 7) (1 7) (1 8) (1 9)))
;->  (4 ((2 1) (3 1) (2 2) (4 1)))
;->  (5 ((2 4) (3 4) (4 4) (3 5) (4 5)))
;->  (6 ((3 8) (4 8) (4 9) (4 10) (3 10) (5 10)))
;->  (5 ((6 7) (7 7) (6 8) (7 8) (6 9))))

(isole-I M true)
;-> ((1 ((0 2)))
;->  (4 ((0 7) (1 7) (1 8) (1 9)))
;->  (4 ((2 1) (3 1) (2 2) (4 1)))
;->  (5 ((2 4) (3 4) (3 5) (4 4) (4 5)))
;->  (11 ((3 8) (4 8) (4 9) (4 10) (3 10) (5 10) (6 9) (6 8) (7 8) (6 7) (7 7))))

Ricorsione con uno stack esplicito
----------------------------------
1. Stack di chiamate
- Ogni volta che una funzione chiama sé stessa, il computer salva lo stato attuale (valori delle variabili locali, punto di esecuzione) nello stack delle chiamate.
- Poi passa a eseguire la nuova chiamata con i nuovi parametri.
2. Esecuzione
- La chiamata più recente va in cima allo stack.
- La funzione ricorsiva continua a chiamare sé stessa finché non raggiunge un caso base.
- Il caso base non fa ulteriori chiamate: allora la funzione termina e lo stack "si svuota" iniziando a tornare indietro.
3. Tornare indietro
- Quando una chiamata termina, il computer riprende lo stato salvato nello stack e continua da dove era rimasto.
- Questo processo continua fino a tornare alla prima chiamata originale.

Esempio concettuale: DFS ricorsiva
- Ogni cella visitata chiama 'dfs' sulle celle vicine.
- Ogni chiamata 'dfs' non termina finché tutte le celle vicine non sono state esplorate.
- Lo stack delle chiamate tiene traccia di tutte le celle "in attesa" di essere completate.
- Quando non ci sono più celle vicine da visitare, lo stack inizia a "scaricarsi" restituendo i risultati delle singole chiamate.

In pratica: una ricorsione usa implicitamente uno stack, mentre in una versione iterativa crei tu stesso uno stack esplicito (lista o array) per gestire le celle da visitare.

Versione ricorsiva pura
-----------------------
Algoritmo:
1) Scansiona tutta la matrice.
2) Quando trovs un '1' non visitato chiama la funzione ricorsiva 'dfs'.
3) dfs:
   - Marca la cella come visitata.
   - Aggiunge le coordinate alla lista dell'isola.
   - Chiama sé stessa per tutte le 4/8 celle vicine che siano ancora '1' e non visitate.
4) Alla fine della chiamata ricorsiva, aggiunge '(area coords)' alla lista dei risultati.
5) Se non ci sono isole, restituisce '0'.

(define (isole-R matrix diag)
  (letn ( (R (length matrix))
          (C (length (matrix 0)))
          (visited (array R C '(nil)))
          (ris '())
          (i 0) (j 0)
          (r 0) (c 0)
          (coords '())
          (d '((-1 0) (1 0) (0 -1) (0 1))) )
    (define (dfs r c coords)
      (when (and (>= r 0) (< r R) (>= c 0) (< c C)
                 (not (visited r c)) (= ((matrix r) c) 1))
        (setf (visited r c) true)
        (push (list r c) coords -1)
        (dolist (dir d)
          (setq coords (dfs (+ r (dir 0)) (+ c (dir 1)) coords))))
      coords)
    ; diag = true --> 8 direzioni
    (if diag (setq d '((-1 0) (1 0) (0 -1) (0 1) (-1 -1) (-1 1) (1 -1) (1 1))))
    (for (i 0 (- R 1))
      (for (j 0 (- C 1))
        (when (and (= ((matrix i) j) 1) (not (visited i j)))
          (setq coords (dfs i j '()))
          (push (list (length coords) coords) ris -1))))
    (if (null? ris) 0 ris)))

Proviamo:

(setq M '((0 0 1 0 0 0 0 1 0 0 0 0 0)
          (0 0 0 0 0 0 0 1 1 1 0 0 0)
          (0 1 1 0 1 0 0 0 0 0 0 0 0)
          (0 1 0 0 1 1 0 0 1 0 1 0 0)
          (0 1 0 0 1 1 0 0 1 1 1 0 0)
          (0 0 0 0 0 0 0 0 0 0 1 0 0)
          (0 0 0 0 0 0 0 1 1 1 0 0 0)
          (0 0 0 0 0 0 0 1 1 0 0 0 0)))

(isole-R M)
;-> ((1 ((0 2)))
;->  (4 ((0 7) (1 7) (1 8) (1 9)))
;->  (4 ((2 1) (3 1) (4 1) (2 2)))
;->  (5 ((2 4) (3 4) (4 4) (4 5) (3 5)))
;->  (6 ((3 8) (4 8) (4 9) (4 10) (3 10) (5 10)))
;->  (5 ((6 7) (7 7) (7 8) (6 8) (6 9))))

(isole-R M true)
;-> ((1 ((0 2)))
;->  (4 ((0 7) (1 7) (1 8) (1 9)))
;->  (4 ((2 1) (3 1) (4 1) (2 2)))
;->  (5 ((2 4) (3 4) (4 4) (4 5) (3 5)))
;->  (11 ((3 8) (4 8) (4 9) (4 10) (3 10) (5 10) (6 9) (6 8) (7 8) (7 7) (6 7))))

Test di correttezza:

(= (sort (map first (isole-R M))) (sort (map first (isole-I M))))
;-> true
(= (sort (map first (isole-R M true))) (sort (map first (isole-I M true))))
;-> true

(= (sort (map length (isole-R M))) (sort (map length (isole-I M))))
;-> true
(= (sort (map length (isole-R M true))) (sort (map length (isole-I M true))))
;-> true

(= (sort (flat (sort (isole-R M)))) (sort (flat (sort (isole-I M)))))
;-> true
(= (sort (flat (sort (isole-R M true)))) (sort (flat (sort (isole-I M true)))))
;-> true

Test di velocità:

(setq T (array 20 20 (rand 2 400)))
(time (isole-I T) 1000)
;-> 385.327
(time (isole-R T) 1000)
;-> 784.001

La versione ricorsiva pura è più lenta e con matrici molto grandi può causare stack overflow.


--------------------------------------------
Quadrato di lato uguale a N ripetuto N volte
--------------------------------------------

Dato un numero intero N, scrivere una funzione che stampa un quadrato/rettangolo in cui i lati orizzontali contengono N copie di N e i lati verticali contengono N copie di N.

Esempi:
  N = 0, nessun output
  N = 1, 1
  N = 2: 22
         22
  N = 3: 333
         3 3
         333
  N = 4: 4444
         4  4
         4  4
         4444
  N = 5: 55555
         5   5
         5   5
         5   5
         55555

(define (quad N)
  (letn ( (s (string N)) (h (dup s N))
          (v (string N (dup " " (* (- N 2) (length N))) N)) )
    (println h)
    (dotimes (r (- N 2)) (println v))
    (if (> N 1) (println h))'>))

Proviamo:

(quad 0)
;->
(quad 1)
;-> 1
(quad 2)
;-> 22
;-> 22
(quad 5)
;-> 55555
;-> 5   5
;-> 5   5
;-> 5   5
;-> 55555
(quad 10)
;-> 10101010101010101010
;-> 10                10
;-> 10                10
;-> 10                10
;-> 10                10
;-> 10                10
;-> 10                10
;-> 10                10
;-> 10                10
;-> 10101010101010101010
(quad 12)
;-> 121212121212121212121212
;-> 12                    12
;-> 12                    12
;-> 12                    12
;-> 12                    12
;-> 12                    12
;-> 12                    12
;-> 12                    12
;-> 12                    12
;-> 12                    12
;-> 12                    12
;-> 121212121212121212121212
(quad 36)

Versione code-golf (139 caratteri, one-line):

(define(f n)
(let(h(dup(string n)n))(println h)
(dotimes(r(- n 2))(println(string n(dup" "(*(- n 2)(length n)))n)))
(if(> n 1)(println h))'>))


---------------
49 monete d'oro
---------------

Un padre ha 49 monete d'oro.
La prima moneta pesa un grammo.
La seconda moneta pesa 2 g e così via fino alla 49esima moneta che pesa 49 g.
Vuole dare sette monete a ciascuno dei sette figli in modo che ogni figlio riceva monete dello stesso peso totale.
Scrivere una funzione che trova un modo per distribuire correttamente le monete.

Lista di monete: 1..N
(setq N 49)
(setq M (sequence 1 N))

Numero di figli:
(setq figli 7)

Peso totale delle monete: N*(N+1)/2 = 49*50/2 = 1225 g
(setq peso-totale (/ (* N (+ N 1)) 2))

Peso monete per ogni figlio: Peso-totale/figli = 1125/7 = 175 g
(setq peso (div peso-totale figli))

Quindi dobbiamo trovare sette sequenze di numeri in cui ognuna somma a 175 con il vincolo che i numeri (1..49) devono essere usati tutti per una sola volta.

La soluzione potrebbe essere calcolata con la funzione "trova-gruppi" che si trova in "Sottoinsiemi non sovrapposti che sommano ad un numero" su "Note libere 33".
Purtroppo la funzione è molto lenta e non riesce a calcolare i gruppi per una lista di 49 elementi (da 1 a 49).

Allora usiamo la funzione "qmDispari" che si trova in "Quadrati magici" su "Problemi vari".
Questa funzione genera un quadrato magico di ordine dispari, quindi basta creare un quadrato magico di ordine 7 ed utilizzare i valori delle righe per distribuire le monete correttamente (infatti ogni riga somma a 175 e anche ogni colonna).

(define (qmDispari n)
  (define (f n x y) (% (add x (mul y 2) 1) n))
  (local (val nm row out)
    (setq out '())
    (setq row '())
    ;calcolo quadrato magico
    (for (i 0 (sub n 1))
      (for (j 0 (sub n 1))
        (setq val (add (mul (f n (sub n j 1) i) n)
                       (add (f n j i))
                       1))
        (push val row -1)
      )
      (push row out -1)
      (setq row '())
    )
    ;calcolo numero magico
    (setq nm (div (mul n (add 1 (mul n n))) 2))
    (println nm)
    out
  )
)

(setq sol (qmDispari 7))
;-> 175
;-> ((2 45 39 33 27 21 8)
;->  (18 12 6 49 36 30 24)
;->  (34 28 15 9 3 46 40)
;->  (43 37 31 25 1913 7)
;->  (10 4 47 41 35 22 16)
;->  (26 20 14 1 44 38 32)
;->  (42 29 23 17 11 5 48))

Verifica delle righe:
(dolist (s sol) (print (apply + s) "-"))
;-> 175-175-175-175-175-175-175

Verifica delle colonne:
(dolist (s (transpose sol)) (print (apply + s) "-"))
;-> 175-175-175-175-175-175-175


---------------------------------------
Diagonalizzare una lista in una matrice
---------------------------------------

Dato una lista L con N elementi, scrivere una funzione che costruisce la matrice M di dimensioni NxN dove:

  M(i,i) = V(i), per i = 1,..,N
  M(i,j) = 0, per (i != j)

In altre parole, gli elementi della lista costituiscono la diagonale principale della matrice.

(define (diagonalize lst)
  (letn ( (N (length lst)) (matrix (array N N '(0))) )
    (dolist (el lst) (setf (matrix $idx $idx) el))
    ;(map (fn(x) (setf (matrix $idx $idx) x)) lst)
  matrix))

(diagonalize '(0))
;-> ((0))
(diagonalize '(1))
;-> ((1))
(diagonalize '(1 2 3 4))
;-> ((1 0 0 0) (0 2 0 0) (0 0 3 0) (0 0 0 4))

Versione code-golf (86 caratteri):

(define(f l)(letn((n(length l))(m(array n n'(0))))(map(fn(x)(setf(m $idx $idx)x))l)m))

(f '(0))
;-> ((0))
(f '(1))
;-> ((1))
(f '(1 2 3 4))
;-> ((1 0 0 0) (0 2 0 0) (0 0 3 0) (0 0 0 4))


----------
Numeri BCD
----------

I numeri interi BCD (Binary-Coded Decimal) rappresentano ogni cifra decimale usando 4 bit (nibble).
È un sistema usato in contesti dove è importante mantenere la precisione decimale ed evitare conversioni binario<->decimale che potrebbero introdurre errori (per esempio nelle calcolatrici o nei registratori di cassa).

Rappresentazione
Ogni cifra 0–9 è codificata come un nibble (4 bit):

  | Dec | BCD  |
  | --- | ---- |
  | 0   | 0000 |
  | 1   | 0001 |
  | 2   | 0010 |
  | 3   | 0011 |
  | 4   | 0100 |
  | 5   | 0101 |
  | 6   | 0110 |
  | 7   | 0111 |
  | 8   | 1000 |
  | 9   | 1001 |

I codici da 1010 a 1111 non rappresentano cifre valide in BCD.

Per esempio:
- 37 in BCD -> '0011 0111'
- 509 in BCD -> '0101 0000 1001'

Basta convertire ogni cifra decimale in binario a 4 bit, senza mai trasformare il numero intero in binario "classico".

Pro
- Conversione rapida a decimale.
- Nessun errore di arrotondamento sulle operazioni "per cifre".
- Velocizza le operazioni decimali nei primi microprocessori.
- Facilita la visualizzazione (ogni nibble = una cifra).
- Supporto hardware semplice.
Contro
- Richiede più memoria del binario puro.
- Operazioni aritmetiche più lente (correzioni da fare).
- Non rappresenta bene numeri molto grandi o calcoli complessi.

Varianti
- Packed BCD: ogni byte contiene due cifre (alto e basso nibble).
- Unpacked BCD: una cifra per byte (comune nei vecchi mainframe).
- Signed BCD: l'ultimo nibble (o byte) contiene il segno.

Vediamo due funzioni che convertono Intero decimale <--> Intero BCD.

; Funzione che prende un intero decimale e restituisce l'intero BCD (lista di nibble)
(define (to-BCD n)
  (define (bit d k) (& (>> d k) 1)) ; estrae il k-esimo bit da d
  (let ( (s (string n)) (out '()) (d 0) (nib '()) )
    (dolist (ch (explode s))
      (setq d (- (char ch) 48))
      (setq nib (list (bit d 3) (bit d 2) (bit d 1) (bit d 0)))
      (push nib out -1))
    out))

(to-BCD 0)
;-> ((0 0 0 0)
(to-BCD 37)
;-> ((0 0 1 1) (0 1 1 1))
(to-BCD 509)
;-> ((0 1 0 1) (0 0 0 0) (1 0 0 1))

; Funzione che prende un intero BCD (lista di nibble) e restituisce l'intero decimale.
(define (from-BCD bcd)
  (if (= bcd '(0 0 0 0)) 0
  ;else
  (let ( (out 0) (d 0) )
    (dolist (nib bcd)
      (setq d (+ (* (nib 0) 8) (* (nib 1) 4) (* (nib 2) 2) (* (nib 3) 1)))
      (println d)
      (setq out (+ (* out 10) d)))
    out)))

(from-BCD '(0 0 0 0))
;-> 0
(from-BCD '((0 0 1 1) (0 1 1 1)))
;-> 37
(from-BCD '((0 1 0 1) (0 0 0 0) (1 0 0 1)))
;-> 509

Somma in BCD
------------
La somma non è una semplice somma binaria:
se il nibble risultante è > 9 (1001) bisogna aggiungere 6 (0110) per correggere.

Esempio: 8 + 7
- binario: 1000 + 0111 = 1111 (15) -> invalido per BCD
- aggiungi 0110: 1111 + 0110 = 1 0101 -> risultato: 0001 0101 (cioè 15)

Scriviamo un addizionatore BCD che somma due numeri interi positivi in BCD:

L'algoritmo segue queste idee:
1) Si parte dalla cifra meno significativa (ultima della lista).
2) Si sommano i due nibble e l'eventuale riporto.
3) Se la somma è > 9, si aggiunge 6 per riportarla nel range BCD.
4) Si produce il nibble corretto e il nuovo riporto.
5) Alla fine, se resta un riporto, si aggiunge un nibble 0001.

(define (BCD-add bcd1 bcd2)
  (define (to-dec nib) (+ (* (nib 0) 8) (* (nib 1) 4) (* (nib 2) 2) (* (nib 3) 1)))
  (define (to-nib d) (list (& (>> d 3) 1) (& (>> d 2) 1) (& (>> d 1) 1) (& d 1)))
  (letn ((a (reverse bcd1)) (b (reverse bcd2)) (len (max (length a) (length b)))
        (carry 0) (out '()) (d1 0) (d2 0) (s 0))
    (for (i 0 (- len 1))
      (setq d1 (if (< i (length a)) (to-dec (a i)) 0))
      (setq d2 (if (< i (length b)) (to-dec (b i)) 0))
      (setq s (+ d1 d2 carry))
      (if (> s 9) (setq s (+ s 6) carry 1) (setq carry 0))
      (push (to-nib (& s 15)) out))
    (if (= carry 1) (push '(0 0 0 1) out))
    out))

(BCD-add (to-BCD 37) (to-BCD 85))
;-> ((0 0 0 1) (0 0 1 0) (0 0 1 0))
(from-BCD (BCD-add (to-BCD 37) (to-BCD 85)))
;-> 122

(BCD-add (to-BCD 509) (to-BCD 91))
;-> ((0 1 1 0) (0 0 0 0) (0 0 0 0))
(from-BCD (BCD-add (to-BCD 509) (to-BCD 91)))
;-> 600

(BCD-add (to-BCD 123456789) (to-BCD 123456789))
;-> ((0 0 1 0) (0 1 0 0) (0 1 1 0) (1 0 0 1) (0 0 0 1)
;->  (0 0 1 1) (0 1 0 1) (0 1 1 1) (1 0 0 0))
(from-BCD (BCD-add (to-BCD 123456789) (to-BCD 123456789)))
;-> 246913578
(+ 123456789 123456789)
;-> 246913578

Scriviamo un addizionatore BCD con N addendi:

(define-macro (BCD+)
"Sum two of more BCD positive integers"
  (apply BCD-add (map eval (args)) 2))

(setq bcd1 (to-BCD 101))
;-> ((0 0 0 1) (0 0 0 0) (0 0 0 1))
(setq bcd2 (to-BCD 91))
;-> ((1 0 0 1) (0 0 0 1))
(setq bcd3 (to-BCD 9))
;-> (1 0 0 1)

(BCD+ bcd1 bcd2 bcd3)
;-> ((0 0 1 0) (0 0 0 0) (0 0 0 1))
(from-BCD (BCD+ bcd1 bcd2 bcd3))
;-> 201
(from-BCD (apply BCD+ (list bcd1 bcd2 bcd3)))
;-> 201

Vedi anche "Codifica BCD (Binary-Coded Decimal)" su "Note libere 26".


----------------------
Legge di Weber-Fechner
----------------------

Ernst Heinrich Weber (1795-1878)
Gustav Theodor Fechner (1801-1887)
La legge di Weber-Fechner, è un principio fondamentale della psicofisica che descrive come la nostra percezione di una variazione in uno stimolo dipenda dall'intensità iniziale di quello stimolo.
In sintesi, afferma che la soglia differenziale (la minima variazione percepibile) è una frazione costante del valore dello stimolo stesso, espressa dalla formula k = deltaR / R. 
Questo significa che se lo stimolo è debole, anche una piccola variazione assoluta può essere percepita, mentre con stimoli più intensi, la variazione assoluta deve essere molto maggiore per essere notata.
Quindi abbiamo una relazione proporzionale per cui l'aumento dello stimolo necessario per avvertire una differenza è proporzionale all'intensità dello stimolo iniziale.

Esempio:
Se tieni in mano un peso da 1kg e aggiungi 100g, noterai sicuramente il cambiamento.
Se invece tieni in mano un peso da 10kg e aggiungi sempre 100g, è probabile che la differenza non venga percepita.
La minima variazione percepibile viene chiamata "just-noticeable difference" o JND.

Formula:
La legge può essere espressa come k = deltaR / R,
dove:
  k è la costante di Weber (una costante che varia a seconda del senso, ad esempio vista, udito, tatto).
  deltaR è la variazione dello stimolo necessaria affinché la differenza sia percepita (la soglia differenziale).
  R è l'intensità dello stimolo iniziale.

Applicazioni e implicazioni:
Percezione sensoriale: Spiega perché i nostri sensi non percepiscono le variazioni in modo lineare, ma logaritmico, come formulato nella successiva legge di Weber-Fechner: S = k*log(I).

Psicologia: È un pilastro della psicologia sperimentale e della psicofisica, permettendo di studiare e quantificare la relazione tra stimoli fisici e sensazioni soggettive.

Economia: È stata estesa al concetto di utilità marginale decrescente in economia: l'incremento di utilità derivante da un bene diminuisce all'aumentare della sua quantità già posseduta.

Vediamo un esempio visivo.

Scriviamo una funzione che genera una matrice binaria MxN con K valori a 1.

; Funzione che genera una matrice binaria random con K valori a 1
(define (random-binary-matrix M N K)
  (letn ( (matrix (array-list (array M N '(0))))
          (tot (* M N))
          (posizioni (randomize (sequence 0 (- tot 1))))
          (p 0) (r 0) (c 0) )
    (if (> K tot) nil ; celle < numero di 1 da inserire --> impossibile
    ;else
      (for (i 0 (- K 1))
        (setq p (posizioni i))
        (setq r (/ p N))
        (setq c (% p N))
        (setf (matrix r c) 1))
      matrix)))

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

Poi generiamo 4 matrici nel modo seguente:
0) Definiamo M e N (uguali per tutte le matrici) (es. 18x18)
1) la prima matrice contiene K celle con valore 1 (es. K = 8)
Questa matrice contiene 8 celle con valore 1.
2) la seconda matrice contiene (+ K delta) celle con valore 1 (es. delta = 10)
Questa matrice contiene 18 celle con valore 1.
3) la terza matrice contiene K celle con valore 1 (es. K = 100)
Questa matrice contiene 100 celle con valore 1.
4) la quarta matrice contiene (+ K delta) celle con valore 1 (es. delta = 10)
Questa matrice contiene 110 celle con valore 1.

(setq m1 (random-binary-matrix 18 18 8))
(setq m2 (random-binary-matrix 18 18 18))
(setq m3 (random-binary-matrix 18 18 100))
(setq m4 (random-binary-matrix 18 18 110))

Adesso stampiamo e confrontiamo le matrici 1 e 2:

(print-grid m1 "  " " *" true)
(print-grid m2 "  " " *" true)

   0 1 2 3 4 5 6 7 8 91011121314151617      0 1 2 3 4 5 6 7 8 91011121314151617
 0                                        0             *
 1                                        1   *
 2                                        2
 3                                        3         *
 4           *                   *        4                 *     *
 5                             *          5
 6                                        6       *
 7                       *                7           *                       *
 8                                        8
 9                         *              9       *                     *
10                                       10   *
11     *                                 11           *
12                                       12
13                                       13
14                                       14 *
15           *                           15 *
16                                       16     *             *         *
17     *                                 17 *

Si riesce chiaramente a percepire che gli asterischi "*" della matrice 2 sono in numero maggiore.

Adesso stampiamo confrontiamo le matrici 3 e 4:

(print-grid m3 "  " " *" true)
(print-grid m4 "  " " *" true)

   0 1 2 3 4 5 6 7 8 91011121314151617      0 1 2 3 4 5 6 7 8 91011121314151617
 0 * * *   *     *           *            0     *         * *   *   *
 1               * * *   *   *     *      1           *     *   *   *   * *
 2   *   *           *     * *     * *    2       *     * *   * * *       * *
 3           * *     * * * * *       *    3 *         * *   *   * * * *     *
 4       * *                         *    4   *           *         *   *
 5 * *             *     *                5 *     *   *       *       *
 6   * * *   * *     *                    6 *       *     *   *   * * *
 7       *     *     *           * * *    7 *   *             * *   *   * * *
 8 *             *           *            8 *   *   *         * * *
 9     * * *         *                    9 *     *   *                   *
10     *             *               *   10 *   *       *       *     *     * *
11       *   * *         *     * * *     11       *   *     *   *   * *
12     *       * *         * * *         12 *         *         *
13           * *               *     *   13 * *                     * * *   *
14             * *     * *     * *   *   14 *           * * *         *   *   *
15       *     * * *         * * *   *   15                   * *           * *
16     *     *           * * *           16   *   * *   *     * * * * * *
17       *     *       *   * * *   *     17 *                 * *   *         *

Questa volta è più difficile stabilire quale matrice ha più asterischi "*".


-----------------------------------------------
Matrici binarie random e indicizzazione lineare
-----------------------------------------------

Scriviamo una funzione che può generare due tipi di matrici binarie random:
1) una matrice binaria random
2) una matrice binaria random con k celle a 1

Per inserire gli 1 in modo casuale nella matrice utilizziamo l'indicizzazione lineare degli elementi di una lista di posizioni casuali:
1) generiamo una sequenza casuale con numeri che vanno da 0 a (rows*cols - 1)
(es. (8 2 6 7 3 9 0 1 5 4))
2) prendiamo i primi k valori della sequenza
(es k = 4 --> (8 2 6 7))
3) per ogni valore x della sequenza calcoliamo il relativo indice nella matrice con le seguenti formule:
   Indice-riga    = x / cols
   Indice-colonna = x % cols
4) Inseriamo 1 nella cella (Indice-riga Indice-colonna)

L'indicizzazione lineare -> (riga, colonna) funziona così:
Data una matrice è M x N, con M = numero di righe e N = numero di colonne,
Allora, data una posizione lineare 'p' (0-based):
  Indice di riga    = p / N
  Indice di colonna = p % N
Questo perché ogni riga contiene N elementi e quindi ogni blocco di lunghezza N corrisponde a una riga.

; Funzione che genera una matrice binaria casuale (rows x cols) (opzionale: con k celle a 1)
; Parametri:
; <rows> numero di righe (intero)
; <cols> numero di colonne (intero)
; <k> numero di celle a 1 (intero)
; Output: matrice binaria (list of lists)
(define (random-binary-matrix rows cols k)
  (let ( (matrix '())
         (posizioni '())
         (tot (* rows cols))
         (p 0) (r 0) (c 0) )
    (cond ((nil? k) ; matrice random binaria
            (array-list (array rows cols (rand 2 tot))))
          ((> k tot) nil) ; (celle a 1) > celle --> impossibile
          (true ; matrice random binaria con k celle a 1
            (setq matrix (array-list (array rows cols '(0))))
            ; posizioni lineari random degli 1
            (setq posizioni (slice (randomize (sequence 0 (- tot 1))) 0 k))
            ; Inserimento degli 1 (k)
            (dolist (p posizioni)
              ; indicizzazione lineare
              (setq r (/ p cols)) ; indice riga
              (setq c (% p cols)) ; indice colonna
              (setf (matrix r c) 1))
            matrix))))

Proviamo:

(random-binary-matrix 4 4)     
;-> ((0 1 1 1) (1 0 0 0) (1 1 0 0) (1 0 0 0))
(random-binary-matrix 5 5 10)  
;-> ((0 0 0 0 1) (0 0 1 0 1) (0 0 0 0 1) (0 0 1 1 1) (0 0 1 1 1))
(random-binary-matrix 3 3 0)
;-> ((0 0 0) (0 0 0) (0 0 0))
(random-binary-matrix 3 3 10)
;-> nil
(random-binary-matrix 3 3 9)
;-> ((1 1 1) (1 1 1) (1 1 1))


-------------------------------------------
The first, the last, and everything between
-------------------------------------------

https://codegolf.stackexchange.com/questions/175485/the-first-the-last-and-everything-between

Nota:
Tutto il contenuto dei siti di Stack Exchange è rilasciato sotto la licenza CC BY-SA 4.0 (Creative Commons Attribution-ShareAlike 4.0).

Dati due numeri interi, restituire una lista con prima i due numeri interi e quindi l'intervallo tra di essi (escludendo entrambi).
L'ordine dell'intervallo deve essere lo stesso dell'input.

(define (flev a b)
  (cond ((= a b) (list a b)) ; numeri uguali
        ((= (abs (- a b)) 1) (list a b)) ; numeri con diffrenza assoluta = 1
        (true
          ; genera la sequenza con i due numeri estremi compresi
          (setq seq (sequence a b))
          ; sposta l'ultimo numero della sequenza in seconda posizione 
          ; cioè dopo il primo numero --> indice 1
          (push (pop seq -1) seq 1)
          seq)))

Proviamo:

(flev 0 5)
;-> (0 5 1 2 3 4)
(flev -3 8)
;-> (-3 8 -2 -1 0 1 2 3 4 5 6 7)
(flev 4 4)
;-> (4 4)
(flev 4 5)
;-> (4 5)
(flev 8 2)
;-> (8 2 7 6 5 4 3)
(flev -2 -7)
;-> (-2 -7 -3 -4 -5 -6)

Versione code-golf (87 caratteri):

(define(f a b)(if(<=(abs(- a b))1)(list a b)(let(s(sequence a b))(push(pop s -1)s 1))))

(f 0 5)
;-> (0 5 1 2 3 4)
(f -3 8)
;-> (-3 8 -2 -1 0 1 2 3 4 5 6 7)
(f 4 4)
;-> (4 4)
(f 4 5)
;-> (4 5)
(f 8 2)
;-> (8 2 7 6 5 4 3)
(f -2 -7)
;-> (-2 -7 -3 -4 -5 -6)


--------------------------------------------------------------
Ordinare un set con differenza costante tra i numeri adiacenti
--------------------------------------------------------------

Data una lista di interi tutti diversi (un set o insieme), scrivere una funzione che ordina la lista in modo tale che la differenza assoluta di ogni coppia di numeri adiacenti sia sempre uguale a k: abs(x(i+1) - x(i)) = k, per tutte le coppie adiacenti.
Se questo non è possibile restituire nil.

Affinché una lista di numeri diversi possa essere riordinata in modo che abs(x(i+1) - x(i)) = k per ogni coppia adiacente, i numeri devono formare una catena aritmetica con passo +-k:

  ..., a-k, a, a+k, a+2k, ...

Questo implica che:
1. Tutti gli elementi devono appartenere allo stesso insieme di congruenza modulo k:
   (x % k) deve essere sempre lo stesso per tutti gli elementi
2. Se questo è vero, l'unica sequenza possibile è ordinare la lista in modo crescente oppure in modo decrescente.

In definitiva, dopo aver ordinato la lista basta controllare se la sequenza costruita soddisfa la condizione, altrimenti restituire 'nil'.

; Funzione che ordina una lista in modo che tra gli elementi adiacenti
; ci sia sempre un differenza pari a k
(define (sort-delta lst k)
  (sort lst)
  (for-all (fn(x) (= x k)) 
            (map (fn(x y) (abs (- x y))) (chop lst) (rest lst))))

(setq L1 '(1 3 5 7))
(setq L2 '(1 2 3 4 5 6 7))
(sort-delta L1 1)
;-> nil
(sort-delta L1 2)
;-> true
(sort-delta L2 1)
;-> true
(sort-delta L2 2)
;-> nil

Se la lista può contenere numeri uguali, allora il problema è più difficile.
Un metodo brute-force è quello di calcolare tutte le permutazioni e verificare quale permutazione soddisfa la condizione.
Comunque la lista da ordinare non deve superare i 10 elementi.

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

(define (ordina-delta lst k)
  (let ( (out '())
         (permute (perm lst)) )
       (dolist (p permute)
         (if (for-all (fn(x) (= x k)) 
                      (map (fn(x y) (abs (- x y))) (chop p) (rest p)))
             (push p out)))
       (unique out)))

Proviamo:

(setq L1 '(1 3 5 7))
(ordina-delta L1 2)
;-> ((7 5 3 1) (1 3 5 7))

(setq L2 '(1 2 3 4 5 6 7))
(ordina-delta L2 1)
((7 6 5 4 3 2 1) (1 2 3 4 5 6 7))

(setq L3 '(4 4 5 5 5))
(ordina-delta L3 1)
;-> ((5 4 5 4 5))

(setq L4 '(1 7 5 2 6 4 4 3 3))
(ordina-delta L4 1)
;-> ((7 6 5 4 3 4 3 2 1) (1 2 3 4 3 4 5 6 7))

(setq L5 '(1 2 2 3 3 3 4 4 4))
(ordina-delta L5 1)
;-> ((2 1 2 3 4 3 4 3 4) (4 3 2 1 2 3 4 3 4)
;->  (4 3 4 3 2 1 2 3 4) (4 3 4 3 4 3 2 1 2))

============================================================================

