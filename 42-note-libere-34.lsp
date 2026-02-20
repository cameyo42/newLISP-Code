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


---------------------------------------------------------------
Numeri divisibili/non-divisibili dal numero di fattori distinti
---------------------------------------------------------------

Sequenza OEIS A075592:
Numbers n such that number of distinct prime divisors of n is a divisor of n.
  2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 19, 20, 22, 23,
  24, 25, 26, 27, 28, 29, 30, 31, 32, 34, 36, 37, 38, 40, 41, 42, 43, 44,
  46, 47, 48, 49, 50, 52, 53, 54, 56, 58, 59, 60, 61, 62, 64, 66, 67, 68,
  71, 72, 73, 74, 76, 78, 79, 80, 81, 82, 83, 84, 86, 88, 89, 90, ...

(define (a075592 limite)
  (let (out '())
    (for (num 2 limite)
      (if (zero? (% num (length (unique (factor num)))))
        (push num out -1)))
    out))

(a075592 90)
;-> (2 3 4 5 6 7 8 9 10 11 12 13 14 16 17 18 19 20 22 23
;->  24 25 26 27 28 29 30 31 32 34 36 37 38 40 41 42 43 44
;->  46 47 48 49 50 52 53 54 56 58 59 60 61 62 64 66 67 68
;->  71 72 73 74 76 78 79 80 81 82 83 84 86 88 89 90)

Sequenza OEIS A185307:
Numbers not divisible by the number of their distinct prime factors.
  1, 15, 21, 33, 35, 39, 45, 51, 55, 57, 63, 65, 69, 70, 75, 77, 85, 87,
  91, 93, 95, 99, 110, 111, 115, 117, 119, 123, 129, 130, 133, 135, 140,
  141, 143, 145, 147, 153, 154, 155, 159, 161, 170, 171, 175, 177, 182,
  183, 185, 187, 189, 190, 201, 203, 205, 207, 209, ...

(define (a185307 limite)
  (let (out '(1))
    (for (num 2 limite)
      (if-not (zero? (% num (length (unique (factor num)))))
        (push num out -1)))
    out))

(a185307 209)
;-> (1 15 21 33 35 39 45 51 55 57 63 65 69 70 75 77 85 87
;->  91 93 95 99 110 111 115 117 119 123 129 130 133 135 140
;->  141 143 145 147 153 154 155 159 161 170 171 175 177 182
;->  183 185 187 189 190 201 203 205 207 209)

Sequenza OEIS A074946:
Positive integers n for which the sum of the prime-factorization exponents of n (bigomega(n) = A001222(n)) divides n.
  2, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 16, 17, 18, 19, 22, 23, 24, 26, 27,
  29, 30, 31, 34, 36, 37, 38, 40, 41, 42, 43, 45, 46, 47, 53, 56, 58, 59,
  60, 61, 62, 63, 66, 67, 71, 73, 74, 75, 78, 79, 80, 82, 83, 84, 86, 88,
  89, 94, 96, 97, 99, 100, 101, 102, 103, 104, 105, 106, ...

(define (a074946 limite)
  (let (out '())
    (for (num 2 limite)
      (setq fattori (factor num))
      (setq somma (apply + (flat (count (unique fattori) fattori))))
      (if (zero? (% num somma))
        (push num out -1)))
    out))

(a074946 106)
;-> (2 3 4 5 6 7 10 11 12 13 14 16 17 18 19 22 23 24 26 27
;->  29 30 31 34 36 37 38 40 41 42 43 45 46 47 53 56 58 59
;->  60 61 62 63 66 67 71 73 74 75 78 79 80 82 83 84 86 88
;->  89 94 96 97 99 100 101 102 103 104 105 106)


------------------------------------------------
Massimo numero di elementi per sequenze sum-free
------------------------------------------------

Data una sequenza di interi da 1 a N, calcolare la lista sum-free con il numero massimo di elementi.
Una lista è sum-free se nessuna coppia di numeri (non necessariamente distinti) somma ad un numero contenuto nella lista.

Soluzione brute-force
---------------------

Algoritmo
1) Creazione del powerset della lista (lista di tutte le sottoliste)
2) Ciclo per ogni elemento del powerset
   2.1 verifica sum-free
   2.2 eventuale aggiornamento della soluzione (lista più grande)

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

(define (sum-free? lst)
"Check if a list is sum-free (no pair of numbers (not necessarily distinct) sums to a number in the list)"
  (let (out nil)
    (dolist (el1 lst out)
      (dolist (el2 lst out)
        (if (find (+ el1 el2) lst) (setq out true))))
    (not out)))

(define (list-max N)
  (setq max-len 0)
  (setq seq (sequence 1 N))
  (setq ps (powerset-i seq))
  (dolist (el ps)
    (if (sum-free? el)
      (when (> (length el) max-len)
        (setq max-len (length el))
        (setq out el))))
  out)

Proviamo:

(list-max 5)
;-> (1 3 5)

(list-max 10)
;-> (1 3 5 7 9)

(time (println (list-max 20)))
;-> (1 3 5 7 9 11 13 15 17 19)
;-> 3828.755

Soluzione ragionata
-------------------
Sappiamo che un numero dispari più un numero dispari dà sempre come risultato un numero pari.
Consideriamo il sottoinsieme di tutti i numeri dispari D = (1 3 5 ...).
L'addizione di un qualsiasi numero pari comporterebbe una violazione delle regole (lista non sum-free), quindi il numero massimo di elementi della lista è dato da quanti numeri dispari esistono nella sequenza (1..N).

(define (list-max2 N) (sequence 1 N 2))

(list-max2 5)
;-> (1 3 5)
(list-max2 10)
;-> (1 3 5 7 9)
(list-max2 20)
;-> (1 3 5 7 9 11 13 15 17 19)


----------------------------------------
Sequenza di parole che genera palindromi
----------------------------------------

Una parola (stringa) è palindroma se si legge allo stesso modo sia in avanti che al contrario.
Definiamo una sequenza di parole nel modo seguente:

  w(0) = "0"
  w(1) = "1"
  w(n) = w(n-2) + w(n-1),
  dove il segno '+' rappresenta l'unione delle due stringhe

Per n >= 1 la parola formata dall'unione di w(1), w(2), ... w(n) è palindroma.

Scriviamo la funzione che genera la sequenza di parole:

(letn ((out (array 10 '(0))) ((out 0) -1)))

(define (parole limite)
  (let ((out (array (+ limite 1) '(0))))
    (setf (out 0) "0")
    (setf (out 1) "1")
    (for (i 2 limite) (setf (out i) (append (out (- i 2)) (out (- i 1)))))
    out))

(setq seq (parole 10))
;-> ("0" "1" "01" "101" "01101" "10101101" "0110110101101"
;->  "101011010110110101101" "0110110101101101011010110110101101"
;->  "1010110101101101011010110110101101101011010110110101101"
;->  "01101101011011010110101101101011011010110101101101011010
;->   110110101101101011010110110101101")

Nota:
; (setq seq (parole 50))
;-> ERR: not enough memory

Adesso scriviamo la funzione che genera palindromi dalla sequenza:

(define (pali n)
  (let (out "")
    (for (i 1 n)
      (extend out (seq i)))))

(map pali (sequence 1 6))
;-> ("1" "101" "101101" "10110101101" "1011010110110101101"
;->  "10110101101101011010110110101101")

Verifichiamo:

(define (palindrome? str) (= str (reverse (copy str))))

(map palindrome? (map pali (sequence 1 6)))
;-> (true true true true true true)


-----------------------------------
Prodotti di cifre divisibili per 10
-----------------------------------

Dato un generatore di numeri casuali da 1 a 9 (con la stessa probabilità), determinare la probabilità che dopo aver generato n numeri (con n > 1) il loro prodotto sia divisibile per 10.

1) Soluzione con simulazione
----------------------------

(define (simula prove n)
  (let (conta 0)
    (for (p 1 prove)
      (if (zero? (% (apply * (map (fn(x) (+ x 1)) (rand 9 n))) 10))
          (++ conta)))
    (div conta prove)))

(simula 1e6 2)
;-> 0.098401

(map (curry simula 1e6) (sequence 2 10))
;-> (0.09850299999999999 0.2138 0.318995 0.409833 0.48421
;->  0.548634 0.602352 0.648996 0.689505)

2) Soluzione matematica
-----------------------

Principio di complementarità
----------------------------
Quando vogliamo calcolare la probabilità di un evento A, a volte è molto più facile calcolare la probabilità dell'evento opposto not(A) (cioè l'evento che A non accada).
Il principio di complementarità dice:

  P(A) = 1 - P(not(A))

In questo caso:
- A = "il prodotto è divisibile per 10"
- not(A) = "il prodotto non è divisibile per 10"

Quando il prodotto non è divisibile per 10?
Quando manca almeno uno dei due fattori necessari: il 2 o il 5.
Quindi:

  P(A) = 1 - P("manca almeno un fattore tra 2 e 5")

Principio di inclusione–esclusione
----------------------------------
L'evento: "manca almeno un fattore tra 2 e 5"
è l'unione di due eventi:

  E2 = "manca il 2"
  E5 = "manca il 5"

Quindi vogliamo calcolare:

  P(E2 unito E5)

Il principio di inclusione–esclusione dice:

  P(E2 unito E5) = P(E2) + P(E5) - P(E2 intersezione E5)

Il motivo è semplice:
- Se sommiamo P(E2)) e P(E5), l'evento in cui mancano entrambi (cioè (E2 unito E5)) viene contato due volte.
- Quindi bisogna sottrarlo una volta.

Quindi nel nostro problema:

1) Probabilità che non compaia nessun 5:

  P(E5) = (8/9)^n, (i numeri possibili sono 1,2,3,4,6,7,8,9)

2) Probabilità che non compaia nessun 2:

  P(E2) = (5/9)^n, (i numeri possibili sono 1,3,5,7,9)

3) Probabilità che non compaiano né 2 né 5:

  P(E2 intersezione E5) = (4/9)^2, (i numeri possibili sono 1,3,7,9)

4) Probabilità che manca il 2 o manca il 5 --> P(E2 unito E5)

  P(E2 unito E5) = (8/9)^n + (5/9)^n - (4/9)^n

Infine, per complementarità, la probabilità che il prodotto sia divisibile per 10 vale:

  P("prodotto sia divisibile per 10") = 1 - P(E2 unito E5) =
  = 1 - (8/9)^n - (5/9)^n + (4/9)^n

Spiegazione breve
Affinché il prodotto sia divisibile per 10, deve contenere un fattore 2 e un fattore 5.
La probabilità che non ci sia 5 (1 2 3 4 6 7 8 9) vale (8/9)^n.
La probabilità che non ci sia 2 (1 3 5 7 9) vale (5/9)^n.
La probabilità che non ci sia né 2 né 5 (1 3 7 9) vale (4/9)^n.
L'unica possibilità rimasta è ottenere un 2 e un 5, rendendo il prodotto divisibile per 10.
Per complementarità e principio di inclusione-esclusione, questa la probabilità vale:

  P("prodotto sia divisibile per 10") = 1 - (8/9)^n - (5/9)^n + (4/9)^n

(define (prob n)
  (sub 1 (pow (div 8 9) n) (pow (div 5 9) n) (sub (pow (div 4 9) n))))

(map prob (sequence 2 10))
;-> (0.09876543209876543 0.2139917695473251 0.319463496418229
;->  0.40949042320784450 0.4850359682448288 0.5486291046419076
;->  0.60270365308428490 0.6491958663549156 0.6895538271051249)

Vediamo una funzione che restituisce la probabilità come frazione (num e den):

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(define (prob-rat n)
  (local (a b c den num)
    (setq a (** 8 n))
    (setq b (** 5 n))
    (setq c (** 4 n))
    (setq den (** 9 n))
    (setq num (- den a b (- c)))
    (list num den)))

(map prob-rat (sequence 2 10))
;-> ((8L 81L) (156L 729L) (2096L 6561L) (24180L 59049L) (257768L 531441L)
;->  (2624076L 4782969L) (25944416L 43046721L) (251511780L 387420489L)
;->  (2404325528L 3486784401L))

(map (fn(x) (div (x 0) (x 1))) (map prob-rat (sequence 2 10)))
;-> (0.09876543209876543 0.2139917695473251 0.3194634964182289
;->  0.40949042320784440 0.4850359682448287 0.5486291046419076
;->  0.60270365308428490 0.6491958663549154 0.6895538271051248)

Vediamo le differenze tra i due risultati (simulazione e formula)

(map sub (map (curry simula 1e6) (sequence 2 10))
         (map prob (sequence 2 10)))
;-> (0.0004445679012345799 -0.0003717695473251237 0.0002705035817709978
;->  -0.0002204232078444335 -0.0006499682448288247 0.000309895358092338
;->  -0.0003386530842848234 0.0006691336450844743 0.0007901728948750408)


-------------------------------
Complemento di un numero intero
-------------------------------

Il complemento C di un numero intero N è il numero decimale che ha tutti i bit invertiti nella rappresentazione binaria di N.

Esempio:
  N = 5
  binario(5) = 101
  flip-bit(101) = 010
  C = decimale(010) = 2

Metodo 1 (stringa)
------------------

(define (complement1 num)
  (let ( (out "") (bin-str (bits num)) )
    (dostring (b bin-str)
      (if (= b 48) ; "0"
          (push "1" out -1)
          (push "0" out -1)))
    (int out 0 2)))

(map complement1 (sequence 0 10))
;-> (1 0 1 0 3 2 1 0 7 6 5)

Metodo 2 (bit manipulation)
---------------------------

(define (complement2 num)
  (letn ( (len (length (bits num))) ; numero di bit significativi
          ; mask = (2^len - 1)
          (mask (- (<< 1 len) 1)) ) ; crea una maschera di 1 (lunghezza = len)
    (^ num mask)))                  ; inverte solo i bit signific

(map complement2 (sequence 0 10))
;-> (1 0 1 0 3 2 1 0 7 6 5)

Test di correttezza:

(setq t (rand 1e3 1e3))
(= (map complement1 t) (map complement1 t))
;-> true

Test di velocità:

(time (map complement1 t) 100)
;-> 187.447
(time (map complement2 t) 100)
;-> 31.234


------------
Matrioske 3D
------------

Abbiamo una collezione di oggetti tridimensionali, ciascuno descritto da tre valori positivi:

  (x y z)

che rappresentano altezza, larghezza e profondità.
Un oggetto può essere inserito in un altro se e solo se tutte e tre le dimensioni sono minori dell'oggetto "contenitore".
L'obiettivo è trovare la catena più lunga di oggetti inseriti uno dentro l'altro, cioè costruire una matrioska 3D massima.

Analisi
-------
1. Rotazioni degli oggetti:
Un oggetto può essere ruotato, quindi possiamo ordinare le sue dimensioni internamente in modo crescente.
In questo modo (3 2 7) diventa (2 3 7) e tutte le possibili rotazioni sono considerate.

2. Comparazione tra oggetti:
Per sapere se un oggetto 'A' può entrare in 'B', dobbiamo confrontare tutte e tre le dimensioni:
  A.x < B.x AND A.y < B.y AND A.z < B.z
Non è sufficiente guardare solo una dimensione (come nella versione 2D), perché potrebbero verificarsi conflitti tra le dimensioni.

3. Ordinamento iniziale:
Ordinare gli oggetti può essere utile per rendere più efficiente la ricerca della catena, ma nell'approccio generale 3D non basta ridurre il problema a una singola dimensione come nel caso delle buste 2D.
È comunque utile ordinare le dimensioni interne degli oggetti e, se si vuole, fare un ordinamento lessicografico iniziale per garantire coerenza.

Algoritmo
---------
1. Normalizzazione degli oggetti
- Per ogni oggetto, ordiniamo le tre dimensioni internamente in ordine crescente.
- Questo permette di considerare tutte le rotazioni possibili senza doverle enumerare esplicitamente.
2. Dynamic programming per 3D
- Per risolvere il problema, si utilizza una tecnica simile alla Longest Increasing Subsequence (LIS), ma estesa a 3 dimensioni.
- Per ogni oggetto i nella lista ordinata, si considera la catena più lunga terminante in i:
  1. Si confrontano tutti gli oggetti j precedenti (j < i).
  2. Si verifica se tutte e tre le dimensioni di j sono minori di quelle di i.
  3. Se sì, la catena che termina in j può essere estesa aggiungendo i.
  4. Si mantiene la catena più lunga possibile.
- Questo garantisce che ogni oggetto nella catena può essere inserito nel successivo.
3. Ricostruzione della catena
- Durante la procedura, si mantiene un array dei predecessori per ogni oggetto.
- Alla fine, risalendo dagli oggetti con lunghezza massima della catena, si può ricostruire l'intera sequenza di oggetti.

Note:
a) Non si può ridurre a una LIS su una sola dimensione**.
- Questo funziona in 2D perché basta ordinare larghezza e altezza in modo particolare.
- In 3D, una LIS su una dimensione potrebbe selezionare oggetti incompatibili in altre dimensioni.
b) La normalizzazione è fondamentale.
- Garantisce che ogni oggetto sia rappresentato nella forma ottimale e tutte le rotazioni siano considerate.
c) Complessità temporale O(n^2)
- Con n oggetti, si confrontano tutti i precedenti per ogni oggetto, quindi la complessità è quadratica in numero di oggetti (gestibile per centinaia o poche migliaia di oggetti).

Esempio:
  Oggetti iniziali: (3 2 7) (4 5 1) (6 9 2) (3 8 1)
  Normalizziamo internamente: (2 3 7) (1 4 5) (2 6 9) (1 3 8)
  Confrontiamo tutte le triple secondo le regole:
  - (1 3 8) --> può essere primo della catena
  - (1 4 5) --> non può entrare in (1 3 8)
  - (2 3 7) --> non può entrare in (1 3 8)
  - (2 6 9) --> può entrare dopo (1 3 8)
  Catena massima: ((1 3 8) (2 6 9))
  - Lunghezza = 2
  - Tutti gli oggetti rispettano il vincolo "tutte le dimensioni minori"

; Confronto lessicografico su triple normalizzate (può essere utile per ordinare inizialmente)
; Ordinamento lessicografico:
; prima per dimensione 1 (crescente)
; poi dimensione 2 (crescente)
; poi dimensione 3 (decrescente)
(define (cmp x y)
  (if (= (first x) (first y))
      (if (= (x 1) (y 1))
          (>= (last x) (last y))
          (<= (x 1) (y 1)))
      (<= (first x) (first y))))

; LIS 3D: calcola lunghezza massima e catena di oggetti incastrabili in 3 dimensioni
(define (lis3D objs)
  (local (len vet prev out last-idx)
    ; calcola il numero di oggetti
    (setq len (length objs))
    ; vet[i] = lunghezza massima della catena terminante in i
    (setq vet (array len '(1)))
    ; prev[i] = indice del predecessore nella catena per ricostruzione
    (setq prev (array len '(-1)))
    ; out = lunghezza massima globale trovata finora
    (setq out 1)
    ; last-idx = indice dell'ultimo oggetto nella catena massima
    (setq last-idx 0)
    ; ciclo su ogni oggetto come potenziale termine di catena
    (for (i 0 (- len 1))
      ; ciclo sui precedenti per estendere la catena
      (for (j 0 (- i 1))
        (let ((a (objs j)) (b (objs i)))
          ; verifica che tutte le dimensioni di a siano minori di b
          (if (and (< (a 0) (b 0)) (< (a 1) (b 1)) (< (a 2) (b 2)))
              ; se la catena terminante in j estesa con i è più lunga di quella attuale in i
              (if (> (+ (vet j) 1) (vet i))
                  (begin
                    ; aggiorna lunghezza della catena terminante in i
                    (setf (vet i) (+ (vet j) 1))
                    ; registra il predecessore per ricostruire la catena
                    (setf (prev i) j))))))
      ; aggiorna la lunghezza massima globale se necessario
      (if (> (vet i) out)
          (begin
            (setq out (vet i))
            (setq last-idx i))))
    ; ricostruzione della catena completa partendo dall'ultimo elemento
    (setq chain '())
    (while (>= last-idx 0)
      (push (objs last-idx) chain)
      (setq last-idx (prev last-idx)))
    ; restituisce una lista con lunghezza massima, lista delle terze dimensioni e catena completa
    (list out (map last chain) chain)))

; Funzione MATRIOSKA 3D
(define (matrioska3D objs)
  (local (norm sorted third result)
    ; 1) normalizzazione interna delle triple (gestione rotazioni)
    (setq norm (map sort objs))
    ; 2) ordinamento globale (facoltativo, ma mantiene ordine coerente)
    (setq sorted (sort norm cmp))
    ; 3) applica LIS 3D
    (setq result (lis3D sorted))
    result))

Proviamo:

(matrioska3D '((3 2 7) (4 5 1) (6 9 2) (3 8 1)))
;-> (2 (8 9) ((1 3 8) (2 6 9)))
(matrioska3D '((2 3 7) (1 4 5) (2 6 9) (1 3 8)))
;-> (2 (8 9) ((1 3 8) (2 6 9)))
(matrioska3D '((4 5 6) (1 2 3) (6 7 8) (9 10 11) (9 10 11)))
;-> (4 (3 6 8 11) ((1 2 3) (4 5 6) (6 7 8) (9 10 11)))
(matrioska3D '((4 5 7) (4 5 6) (1 2 3) (6 7 8) (9 10 11) (9 10 11)))
;-> (4 (3 7 8 11) ((1 2 3) (4 5 7) (6 7 8) (9 10 11)))

Ecco la versione corretta di 'verifica':
(- n 1) al posto di n nel 'for'
; Funziona che verifica l'output della funzione 'matrioska3D':
(define (verifica risultato)
  (letn ((objs (last risultato)) (ok true) (i 0) (n (- (length objs) 1)))
    (for (i 0 (- n 1))
      (let ((a (objs i)) (b (objs (+ i 1))))
        (if (or (>= (a 0) (b 0))
                (>= (a 1) (b 1))
                (>= (a 2) (b 2)))
            (setq ok false))))
    ok))

(verifica (matrioska3D '((4 5 7) (4 5 6) (1 2 3) (6 7 8) (9 10 11) (9 10 11))))
;-> true

Vedi anche "Buste Matrioska" su "Note libere 4".


----------------------
Risolutore di rapporti
----------------------

Abbiamo una lista di coppie di stringhe:

(setq coppie '(("a" "b") ("b" "c") ("x" "y") ("x" "a"))

e una lista di valori:

(setq values '(1.5 2.5 5.0 3.0))

Queste due liste rappresentano i rapporti:

  a/b = 1.5
  b/c = 2.5
  x/y = 5.0
  x/a = 3.0

Scrivere una funzione che date due stringhe calcola il loro rapporto.

Esempi:
(solve a c) --> 3.75
  Spiegazione:
  a/b * b/c = a/c = 1.5*2.5 = 3.75

(solve b y) --> 1.11111111
  Spiegazione:
  a/b = 1.5 --> b = a/1.5
  x/a = 3.0 --> x = 3.0*a
  x/y = 5.0 --> y = x/5.0 --> y = a*3.0/5.0
  Quindi:
  b/y = (a/1.5)*(5.0/a*3.0) = 5 / 4.5 = 1.1111111

Algoritmo
---------
Costruire un grafo orientato in cui ogni coppia genera due archi:
  da A a B con peso A/B = v
  da B a A con peso B/A = 1/v
Poi fare una ricerca in profondità (DFS) accumulando il prodotto dei pesi fino ad arrivare al nodo di destinazione.

(define (ratio-solver coppie valori a b)
  ; L'algoritmo costruisce un grafo pesato orientato in memoria.
  ; Ogni coppia (X Y) con valore V rappresenta l'equazione X/Y = V.
  ; Da essa si derivano due archi:
  ;   X -> Y con peso V
  ;   Y -> X con peso 1/V
  ; Questo permette di navigare il sistema anche "al contrario".
  ; Una volta costruito il grafo, si cerca un percorso da 'a' a 'b'.
  ; Il percorso rappresenta una catena di rapporti concatenati.
  ; Il valore finale è il prodotto dei pesi lungo il percorso.
  ; Se non esiste nessun percorso, i due nodi appartengono a componenti
  ; disconnesse del grafo delle equazioni, quindi si restituisce nil.
  ; Se le variabili 'a' o 'b' non esistono, restituisce nil.
  (if (and (find a (flat coppie)) (find b (flat coppie))) ; controllo 'a' 'b'
    (letn ((G '())  ; lista associativa del grafo
          ; funzione locale: aggiunge due archi (diretto e reciproco)
          (add-edge (lambda (x y v)
                      (letn ((lx (lookup x G)) (ly (lookup y G)))
                        (if (not lx) (push (list x '()) G -1))
                        (if (not ly) (push (list y '()) G -1))
                        (push (list y v) (lookup x G) -1)
                        (push (list x (div 1 v)) (lookup y G) -1))))
          )
      ; --- COSTRUZIONE DEL GRAFO ---
      ; Per ogni coppia usa il valore corrispondente con $idx
      (dolist (p coppie)
        (add-edge (p 0) (p 1) (valori $idx)))
      ; --- RISOLUZIONE DEL RAPPORTO ---
      ; Si usa DFS iterativa con uno stack.
      ; Lo stack contiene coppie (nodo corrente, prodotto accumulato).
      (letn ((stack (list (list a 1.0)))
            (visited '())
            result)
        ; Cerca finché ci sono nodi da esplorare
        (until (null? stack)
          (letn ((curr (pop stack))
                (node (curr 0))
                (prod (curr 1)))
            ; Se ho trovato il nodo target -> fine
            (if (= node b)
                (begin (setq result prod) (setq stack '()))
                ; Altrimenti esploro i vicini non ancora visitati
                (begin
                  (push node visited)
                  (dolist (nx (lookup node G))
                    (let (next (nx 0))
                      (if (not (ref next visited))
                          (push (list next (mul prod (nx 1))) stack -1))))))))
        ; Se non trovato -> nilL (grafi disconnessi)
        result))))

Proviamo:

(setq coppie '(("a" "b") ("b" "c") ("x" "y") ("x" "a") ("d" "f")))
(setq valori '(1.5 2.5 5.0 3.0 2.0))

(ratio-solver coppie valori "a" "c")
;-> 3.75
(ratio-solver coppie valori "b" "y")
;-> 1.111111111
(ratio-solver coppie valori "c" "y")
;-> 0.4444444444444444
(ratio-solver coppie valori "f" "d")
;-> 0.5
(ratio-solver coppie valori "f" "a")
;-> nil ; non esiste un percorso
(ratio-solver coppie valori "z" "a")
;-> nil ; non esiste una varibile ("z")

Possiamo usare anche i simboli al posto delle stringhe:

(setq coppie '((a b) (b c) (x y) (x a) (d f)))

(ratio-solver coppie valori 'a 'c)
;-> 3.75
(ratio-solver coppie valori 'c 'y)
;-> 0.4444444444444444
(ratio-solver coppie valori 'f 'a)
;-> nil ; non esiste un percorso
(ratio-solver coppie valori 'f 'z)
;-> nil ; non esiste una varibile (z)

Vediamo una funzione con ricorsione pura (senza stack):

(define (ratio-solver-R coppie valori a b)
  ; L'algoritmo interpreta ogni equazione X/Y = V come un arco orientato
  ; nel grafo: X -> Y con peso V, e il reciproco Y -> X con peso 1/V.
  ; Per trovare il rapporto richiesto si cerca ricorsivamente un percorso
  ; che porta da 'a' a 'b'. Ogni passo ricorsivo moltiplica il peso
  ; dell'arco percorso per il valore accumulato finora.
  ; La ricerca procede in profondità: ogni ramo rappresenta una possibile
  ; concatenazione di rapporti. Se si raggiunge il nodo finale, il valore
  ; accumulato rappresenta il rapporto desiderato.
  ; Se non esiste alcun percorso, significa che le due variabili
  ; appartengono a componenti disconnesse del sistema delle equazioni
  ; -> la funzione restituisce nil.
  ; Se le variabili 'a' o 'b' non esistono, restituisce nil.
  (if (and (find a (flat coppie)) (find b (flat coppie))) ; controllo 'a' 'b'
    (letn ((G '())
          (visited '())
          (finito nil)
          (risultato nil)
          (add-edge
            (lambda (x y v)
              (letn ((lx (lookup x G)) (ly (lookup y G)))
                (if (not lx) (push (list x '()) G -1))
                (if (not ly) (push (list y '()) G -1))
                (push (list y v)           (lookup x G) -1)
                (push (list x (div 1 v))   (lookup y G) -1))))
          (dfs
            (lambda (node target prod)
              ; Se già trovato -> non proseguire oltre
              (if finito nil
                  (if (= node target)
                      ; trovato risultato -> salva e blocca ulteriori ricorsioni
                      (begin
                        (setq risultato prod)
                        (setq finito true))
                      ; altrimenti continua la ricerca
                      (begin
                        (push node visited)
                        (dolist (nx (lookup node G))
                          (if finito (setq nx nx)    ; evita altri rami
                              (letn ((next (nx 0))
                                      (peso (nx 1)))
                                (if (not (ref next visited))
                                    (dfs next target (mul prod peso))))))))))))
      ; Costruzione del grafo
      (dolist (p coppie)
        (add-edge (p 0) (p 1) (valori $idx)))
      ; Avvia la DFS
      (dfs a b 1.0)
      ; Se mai trovato -> risultato, altrimenti NIL
      risultato)))

Proviamo:

(setq coppie '(("a" "b") ("b" "c") ("x" "y") ("x" "a") ("d" "f")))
(setq valori '(1.5 2.5 5.0 3.0 2.0))

(ratio-solver-R coppie valori "a" "c")
;-> 3.75
(ratio-solver-R coppie valori "b" "y")
;-> 1.111111111

(setq coppie '((a b) (b c) (x y) (x a) (d f)))

(ratio-solver-R coppie valori 'a 'c)
;-> 3.75
(ratio-solver-R coppie valori 'f 'a)
;-> nil
(ratio-solver-R coppie valori 'f 'z)
;-> nil


-------------------
Distanza di Hamming
-------------------

La distanza di Hamming tra due stringhe di lunghezza uguale è il numero di posizioni nelle quali i simboli corrispondenti sono diversi.
Quindi la distanza di Hamming misura il numero di sostituzioni necessarie per convertire una stringa nell'altra.
Rappresenta anche il numero minimo di errori che possono aver portato alla trasformazione di una stringa nell'altra.

; Funzione che calcola la distanza di Hamming tra due stringhe
(define (hamming-dist str1 str2)
  (if (= (length str1) (length str2))
      (length (clean true? (map = (explode str1) (explode str2))))))

Proviamo:

(hamming-dist "1234" "1235")
;-> 1
(hamming-dist "1234" "1234")
;-> 0
(hamming-dist "1235" "4321")
;-> 4
(hamming-dist "1235" "43210")
;-> nil

Per le stringhe che rappresentano numeri binari (stringhe binarie) è possibile calcolare la distanza di Hamming anche con un altro metodo.

(define (pop-count1 num)
"Calculate the number of 1 in binary value of an integer number"
  (let (counter 0)
    (while (> num 0)
      (setq num (& num (- num 1)))
      (++ counter))
    counter))

; Funzione che calcola la distanza di Hamming tra due stringhe binarie
; (devono entrambe iniziare con 1)
(define (hamming-bit b1 b2)
  (pop-count1 (^ (int (string b1) 0 2) (int (string b2) 0 2))))

Proviamo:

(hamming-bit "1010" "1111")
;-> 2
(hamming-bit 1010 1111)
;-> 2
(hamming-bit 1000 1111)
;-> 3

(hamming-bit "011" "111")
;-> 3 ; errore
(hamming-dist "011" "111")
;-> 1 ; corretto

Un altro metodo per numeri interi:

Versione C (wikipedia)
int hamming_distance(unsigned x, unsigned y)
{
    int dist = 0;
    // The ^ operators sets to 1 only the bits that are different
    for (unsigned val = x ^ y; val > 0; ++dist)
    {
        // We then count the bit set to 1 using the Peter Wegner way
        val = val & (val - 1); // Set to zero val's lowest-order 1
    }
    // Return the number of differing bits
    return dist;
}

Versione newlisp:

(define (hamming-num x y)
  (letn ((dist 0)
         (val (^ x y)))
    (while (> val 0)
      (++ dist)
      (setq val (& val (- val 1))))
    dist))

(bits 34)
;-> "100010"
(bits 45)
;-> "101101"
(hamming-num 34 45)
;-> 4

La distanza di Hamming può generare uno spazio metrico perché una funzione di distanza in una metrica deve soddisfare i seguenti assiomi per tutte le stringhe x, y e z di uguale lunghezza.

1) Non negatività

   a) DH(x,y) >= 0
   b) DH(x,y) = 0 se e solo se x = y

La distanza di Hamming conta il numero di posizioni diverse tra due stringhe, quindi è sempre un numero non negativo. Inoltre, se e e y solo identiche, allora DH(x,y) = 0.

2) Simmetria

   DH(x,y) = DH(y,x)

La distanza di Hamming tra x e y è uguale alla distanza tra y e x perché conta solo le differenze di posizione, indipendentemente dall'ordine.

3) Disuguaglianza triangolare

   DH(x,z) = DH(x,y) + DH(y,z)

Se consideriamo tre stringhe x, y, e z, la distanza diretta DH(x,z) non può superare la somma delle distanze DH(x,y) + DH(y,z).
Questo accade perché ogni differenza tra x e z deve apparire in almeno una delle distanze parziali tra x e y o tra y e z.

La distanza di Hamming soddisfa tutte queste proprietà, quindi definisce uno spazio metrico (o sistema metrico) sull'insieme di tutte le stringhe di una certa lunghezza.


------------
Hamming Ball
------------

Per ogni stringa x di lunghezza n e raggio r, è possibile definire una "open ball" (intorno) B centrata in x come l'insieme di tutte le stringhe di lunghezza n che hanno una distanza di Hamming inferiore a r da x:

  B(x,r) = {y : DH(x,y) < r}

Problema
Dati una stringa binaria 'x' di lunghezza 'N' e un raggio 'r' vogliamo generare tutte le stringhe binarie a distanza di Hamming inferiore a 'r' da 'x'.
La distanza di Hamming tra due stringhe binarie è il numero di bit in cui differiscono.

Algoritmo generale
1. Distanza zero (k = 0): includiamo la stringa originale 'x'.
2. Distanze da 1 a r-1 (k = 1..r-1): generiamo (in modo iterativo) tutte le combinazioni di 'k' bit da invertire.
3. Per ogni combinazione di posizioni dei bit da flippare:
   - Copiamo la stringa originale in 'tmp'
   - Invertiamo i bit nelle posizioni indicate dalla combinazione
   - Convertiamo 'tmp' in stringa e aggiungiamo al risultato 'res'
4. Ripetiamo finché non abbiamo generato tutte le combinazioni per tutti i valori di 'k'.

Algoritmo specifico
1. Preparazione iniziale
   - 'chars = explode(x)': converte la stringa in lista di caratteri ('"101"' -> '("1" "0" "1")')
   - 'res = ()': lista vuota per memorizzare tutte le stringhe della Hamming ball
   - 'comb = ()': combinazione corrente di posizioni da flippare
   - 'n = length(chars)': lunghezza della stringa
   - 'done': flag booleano per terminare il ciclo di generazione combinazioni
2. Ciclo esterno su 'k' (numero di bit da cambiare)
   (setq comb (sequence 0 (- k 1)))
   - 'k = 0': stringa originale -> aggiunta direttamente a 'res'
   - 'k >= 1': inizializza la prima combinazione (0 1 ... k-1)
3. Ciclo interno per generare tutte le combinazioni lessicografiche
   3.1. Costruzione della stringa corrente:
        - 'tmp = ()'
        - Per ogni indice 'j' da 0 a n-1:
          - Se 'j' è in 'comb' -> flip bit -> 'tmp.push(flip(chars[j]))'
          - Altrimenti -> mantieni bit originale -> 'tmp.push(chars[j])'
        - Converti 'tmp' in stringa con 'join' e aggiungi a 'res'
   3.2. Aggiornamento combinazione (generazione della combinazione successiva in ordine lessicografico):
        - Parti dall'ultima posizione i = k-1
        - Trova la posizione 'i' più a destra che può essere incrementata senza uscire dal range [0..n-1]
        - Se non esiste (i < 0) -> tutte le combinazioni generate -> 'done = true'
        - Altrimenti:
          - Incrementa comb[i]++
          - Aggiorna tutte le posizioni successive j = i+1 .. k-1 così che comb[j] = comb[j-1] + 1
        - Questo garantisce che le combinazioni siano ordinate e non ripetute
4. Uscita
Alla fine:
- 'res' contiene tutte le stringhe binarie a distanza di Hamming < r da 'x'
- La dimensione di 'res' è: |B(x,r)| = Sum[k=0,r-1]binom(N k)

Quanti elementi ha una Hamming ball con x di lunghezza N e raggio r?
Per ogni (k = 0, 1, ..., r-1) bit da cambiare, il numero di combinazioni possibili è il coefficiente binomiale:

  binom(N k)

- k=0 -> la stringa originale
- k=1 -> tutte le stringhe con 1 bit invertito
- ... fino a (k = r-1)
Quindi il numero totale di elementi nella Hamming ball è:

  |B(x,r)| = Sum[k=0,r-1]binom(N k)

; Funzione ausiliaria: inverte un singolo bit
(define (flip ch)
  (if (= ch "0") "1" "0"))

(define (hamming-ball x r)
  ; Calcola tutte le stringhe binarie di lunghezza n
  ; a distanza di Hamming < r dalla stringa x
  (letn ((chars (explode x))       ; converte la stringa in lista di caratteri
         (n (length chars))        ; lunghezza della stringa
         (res '())                 ; lista dei risultati (stringhe nella ball)
         (comb '())                ; lista degli indici dei bit da flippar
         (tmp '())                 ; lista temporanea per costruire ogni stringa modificata
         (i 0) (j 0) (k 0)         ; contatori per i loop
         (done nil))               ; variabile booleana per terminare il ciclo delle combinazioni
    (for (k 0 (- r 1))             ; per ogni numero di bit da cambiare (0 .. r-1)
      (if (= k 0)
          ; caso k=0: nessun bit da flippare, aggiungo la stringa originale
          (push (join chars "") res -1)
          ;else
          (begin
            ; inizializza la prima combinazione di k bit [0..k-1]
            (setq comb '())
            (setq comb (sequence 0 (- k 1)))
            (setf done nil)           ; resettare done per il nuovo k
            (while (not done)         ; ciclo fino a generare tutte le combinazioni
              (setq tmp '())          ; reset della stringa temporanea
              (for (j 0 (- n 1))      ; ciclo su tutti i bit della stringa originale
                (if (ref j comb)      ; se j è nella combinazione corrente
                    (push (flip (chars j)) tmp -1) ; inverto il bit
                    (push (chars j) tmp -1)))      ; altrimenti lascio il bit originale
              (push (join tmp "") res -1)          ; aggiungo la stringa modificata ai risultati
              (setf i (- k 1))                     ; parto dall'ultima posizione della combinazione
              (while (and (>= i 0) (>= (comb i) (- n (- k i)))) (-- i))
                ; decremento i finché non trovo un elemento della combinazione che può essere incrementato
              (if (< i 0)
                  (setf done true) ; tutte le combinazioni generate, termino il while
                  ;else
                  (begin
                    (setf (comb i) (+ (comb i) 1))
                    ; incremento il valore trovato
                    (if (< (+ i 1) (length comb))
                        ; aggiorno tutti gli elementi successivi della combinazione
                        (for (j (+ i 1) (- k 1))
                          (setf (comb j) (+ (comb (- j 1)) 1)))))))))) ; garantisce ordine crescente
    res)) ; restituisce la lista completa di stringhe nella Hamming ball

Proviamo:

(hamming-ball "10" 2)
;-> ("10" "00" "11")
(hamming-ball "101" 2)
;-> ("101" "001" "111" "100")
(hamming-ball "10" 3)
;-> ("10" "00" "11" "01")
(hamming-ball "101" 3)
;-> ("101" "001" "111" "100" "011" "000" "110")
(hamming-ball "1111" 4)
;-> ("1111" "0111" "1011" "1101" "1110" "0011" "0101" "0110"
;->  "1001" "1010" "1100" "0001" "0010" "0100" "1000")
(length (hamming-ball "1111" 4))
;-> 15
(length (hamming-ball "10101010" 3))
;-> 37

; Funzione che calcola la distanza di Hamming tra due stringhe
(define (hamming-dist str1 str2)
  (if (= (length str1) (length str2))
      (length (clean true? (map = (explode str1) (explode str2))))))

(map (curry hamming-dist "10101010") (hamming-ball "10101010" 3))
;-> (0 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)

Adesso vediamo una funzione che calcola il numero di elementi di una Hamming Ball.

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

; Funzione che calcola il numero di elementi di una Hamming Ball con raggio r di una parola binaria x lunga n
(define (ball-length n r)
  (let (sum 0)
    (for (k 0 (- r 1)) (++ sum (binom n k))) sum))

(ball-length 4 4)
;-> 15
(ball-length 8 3)
;-> 37

; Funzione hamming-ball (senza commenti)
(define (hamming-ball x r)
  (letn ((chars (explode x))
         (n (length chars))
         (res '())
         (comb '())
         (tmp '())
         (i 0) (j 0) (k 0)
         (done nil))
    (for (k 0 (- r 1))
      (if (= k 0)
          (push (join chars "") res -1)
          ;else
          (begin
            (setq comb '())
            (setq comb (sequence 0 (- k 1)))
            (setf done nil)
            (while (not done)
              (setq tmp '())
              (for (j 0 (- n 1))
                (if (ref j comb)
                    (push (flip (chars j)) tmp -1)
                    (push (chars j) tmp -1)))
              (push (join tmp "") res -1)
              (setf i (- k 1))
              (while (and (>= i 0) (>= (comb i) (- n (- k i)))) (-- i))
              (if (< i 0)
                  (setf done true)
                  ;else
                  (begin
                    (setf (comb i) (+ (comb i) 1))
                      (if (< (+ i 1) (length comb))
                          (for (j (+ i 1) (- k 1))
                          (setf (comb j) (+ (comb (- j 1)) 1))))))))))
    res))

Esempio dettagliato
-------------------
Vediamo uno schema visivo che mostra come l'algoritmo genera tutte le combinazioni di bit da flippare per costruire la Hamming Ball.
Dati:
  x = "101"
  N = 3
  r = 3
Quindi generiamo distanze k = 0, 1, 2
(la distanza 3 è esclusa perché il raggio è < r)

Contiamo tutte le stringhe con 0, 1 o 2 bit diversi da '"101"'.

k = 0 -> Nessun flip
Combinazione: '()'
  Stringa: 101

k = 1 -> Flip di 1 bit
Tutte le combinazioni di lunghezza 1 prese da (0 1 2)
Combinazioni generate dal codice (lessicografiche):
  (0)
  (1)
  (2)
Costruzione stringhe:
  +------+------------------+-----------+
  | Comb | Flippa posizioni | Risultato |
  +------+------------------+-----------+
  | (0)  | 1->0             | 001       |
  | (1)  | 0->1             | 111       |
  | (2)  | 1->0             | 100       |
  +------+------------------+-----------+

k = 2 -> Flip di 2 bit
Tutte le combinazioni di 2 elementi da (0 1 2)
Generazione lessicografica:
  L'algoritmo parte da:
  (0 1)
  e genera:
  (0 1)
  (0 2)
  (1 2)
  (con l'algoritmo iterativo "incrementa la posizione destra disponibile")
Costruzione stringhe:
  +-------+------------+-----------+
  | Comb  | Flip       | Risultato |
  +-------+------------+-----------+
  | (0 1) | 1->0, 0->1 | 011       |
  | (0 2) | 1->0, 1->0 | 000       |
  | (1 2) | 0->1, 1->0 | 110       |
  +-------+----------+-------------+

Risultato finale: ("101" "001" "111" "100" "011" "000" "110")

Schema generale della generazione delle combinazioni
----------------------------------------------------
Per ogni 'k':
1. Inizializzazione
comb = (0 1 2 ... k-1)
2. Per ogni combinazione:
- Usa 'comb' per determinare quali bit flippar
- Poi genera la successiva combinazione con la regola:
    Parti da i = k-1 (da destra)
    Trova la prima posizione che può essere incrementata
    Incrementala
    Aggiorna tutte le posizioni successive:
      comb[j] = comb[j-1] + 1
3. Se nessuna posizione si può incrementare -> done = true

Vediamo anche la rappresentazione grafica bit-per-bit.
Dati:
  x = "101"
  r = 3   -> k = 0, 1, 2

Stringa di partenza
  pos: 0 1 2
  x =  1 0 1

k = 0  -> nessun flip
  101

k = 1  -> flip di 1 bit
  Combinazioni: '(0) (1) (2)'
  - Flip in posizione 0 -> '(0)'
    x:    1 0 1
    flip: ^
    out:  0 0 1    = "001"
  - Flip in posizione 1 -> '(1)'
    x:    1 0 1
    flip:   ^
    out:  1 1 1    = "111"
  - Flip in posizione 2 -> '(2)'
    x:    1 0 1
    flip:     ^
    out:  1 0 0    = "100"

k = 2  -> flip di 2 bit
  Combinazioni: '(0 1) (0 2) (1 2)'
  - Flip in posizioni 0 e 1 -> '(0 1)'
    x:    1 0 1
    flip: ^ ^
    out:  0 1 1    = "011"
  - Flip in posizioni 0 e 2 -> '(0 2)'
    x:    1 0 1
    flip: ^   ^
    out:  0 0 0    = "000"
  - Flip in posizioni 1 e 2 -> '(1 2)'
    x:    1 0 1
    flip:   ^ ^
    out:  1 1 0    = "110"

Risultato finale: ("101" "001" "111" "100" "011" "000" "110")


-------------------
Panchine e lampioni
-------------------

Lungo un viale rettilineo ci sono panchine P e lampioni L:

      P   L       P       L   P   P   P
  *---*---*---*---*---*---*---*---*---*-
  0   1   2   3   4   5   6   7   8   9

Ogni lampione fa luce fino ad una distanza r (raggio) a destra e a sinistra).
Determinare il valore minimo di r affinchè tutte le panchine siano illuminate almeno da un lampione.

Versione 1
----------
Ricerca brute-force con un ciclo per il raggio che va dalla posizione minima alla posizione massima con passo 1 e verifica se il raggio corrente soddisfa le condizioni.

; Funzione che verifica se una panchina è illuminata da una lista di lampioni com raggio r (lumen)
(define (check? p lmp r)
  (exists (fn(x) (<= (abs (- p x)) r)) lmp))

(setq panc '(1 4 7 8 9))
(setq lamp '(2 6))

(check? 1 lamp 2)
;-> 2
(check? 8 lamp 2)
;-> 6
(check? 9 lamp 2)
;-> nil

; Funzione che calcola il raggio minimo per illuminare una disposizione di panchine e lampioni
(define (lumen1 panchine lampioni)
  (local (min-lumen max-lumen sol stop)
    (setq sol nil)
    (sort panchine)
    (sort lampioni)
    (setq max-lumen (max (panchine -1) (lampioni -1)))
    (setq min-lumen (min (panchine 0) (lampioni 0)))
    ;(println min-lumen { } max-lumen)
    (setq stop nil)
    (for (raggio min-lumen max-lumen 1 stop)
      (when (for-all (fn(x) (check? x lampioni raggio)) panchine)
        (setq sol raggio) (setq stop true)))
    sol))

Proviamo:

(lumen1 panc lamp)
;-> 3

(setq p1 (rand 1000 100))
(setq l1 (rand 1000 10))
(lumen1 p1 l1)
;-> 268

La funzione è lenta per valori grandi di panchine e lampioni.

(setq p2 (rand 50000 5000))
(setq l2 (rand 50000 1000))

(time (println (lumen1 p2 l2)))
;-> 246
;-> 1359.962

Versione 2
----------
Modifichiamo la funzione inserendo una ricerca binaria al posto del ciclo 'for' (che aumenta r solamente di 1 per ogni passo).

; Funzione che calcola il raggio minimo per illuminare una disposizione di panchine e lampioni
(define (lumen2 panchine lampioni)
  (local (low high mid sol)
    (sort panchine)
    (sort lampioni)
    ; raggio massimo possibile = distanza max tra estremi
    (setq high (max (abs (- (panchine -1) (lampioni 0)))
                    (abs (- (lampioni -1) (panchine 0)))))
    (setq low 0)
    (setq sol nil)
    (while (<= low high)
      (setq mid (/ (+ low high) 2))
      (if (for-all (fn(x) (check? x lampioni mid)) panchine)
          (begin
            (setq sol mid)       ; mid funziona: prova a ridurre
            (setq high (- mid 1)))
          (begin
            (setq low (+ mid 1)) ; mid non funziona: aumenta
          ))
    )
    sol))

Proviamo:

(lumen2 panc lamp)
;-> 3

(lumen2 p1 l1)
;-> 268

(time (println (lumen2 p2 l2)))
;-> 246
;-> 5096.708

Questa modifica ha peggiorato il tempo di esecuzione.
Probabilmente la funzione nativa 'exists' in 'check?' è più veloce.

Versione 3
----------
Ottimizziamo anche la funzione 'check?' (usando una ricerca binaria anche tra i lampioni per trovare il più vicino a una panchina).

; Ricerca binaria del lampione più vicino alla panchina p.
(define (closest-lamp p lamp)
  (letn ((low 0) (high (- (length lamp) 1)) (best nil))
    (while (<= low high)
      (let ((mid (div (+ low high) 2)))
        (if (< (lamp mid) p)
            (setq low (+ mid 1))
            (setq high (- mid 1)))
        ; memorizza il candidato più vicino visto finora
        (if (nil? best)
            (setq best (lamp mid))
            (when (< (abs (- p (lamp mid))) (abs (- p best)))
              (setq best (lamp mid)))))
    )
    best))

; Funzione che verifica se una panchina è illuminata da una lista di lampioni com raggio r (lumen)
(define (check p lamp r)
  (let ((near (closest-lamp p lamp)))
    (<= (abs (- p near)) r)))

; Funzione che calcola il raggio minimo per illuminare una disposizione di panchine e lampioni
(define (lumen3 panchine lampioni)
  (local (low high mid sol)
    (sort panchine)
    (sort lampioni)
    (setq high (max (abs (- (panchine -1) (lampioni 0)))
                    (abs (- (lampioni -1) (panchine 0)))))
    (setq low 0)
    (setq sol nil)
    (while (<= low high)
      (setq mid (/ (+ low high) 2))
      (if (for-all (fn(x) (check x lampioni mid)) panchine)
          (begin
            (setq sol mid)
            (setq high (- mid 1)))
          (setq low (+ mid 1))))
    sol))

Proviamo:

(lumen3 panc lamp)
;-> 3
(lumen3 p1 l1)
;-> 268
(time (println (lumen3 p2 l2)))
;-> 246
;-> 1691.435

Questa versione è più veloce di 'lumen2' (come 'lumen1').
Il risultato dipende anche dalla disposizione delle panchine e dei lampioni, non solo dal loro numero.

Versione 4
----------
Possiamo scrivere una funzione ancora più veloce che non usa ricerca binaria: ci muoviamo in parallelo lungo le due liste, avanzando l'indice dei lampioni solo quando serve.

Algoritmo
---------
1. Ordina panchine e lampioni.
2. Usa due indici:
   - 'i' per scorrere le panchine
   - 'j' per scorrere i lampioni
3. Per ogni panchina:
   - prova il lampione attuale lampioni[j]
   - se il successivo lampioni[j+1] è più vicino, incrementa j
   - ripeti finché conviene
4. Il lampione selezionato è sempre il più vicino, perché:
   - p cresce monotonicamente
   - j avanza solo in avanti
5. Aggiorna continuamente la distanza massima (raggio minimo).
6. Alla fine questa distanza è il raggio minimo richiesto.
(Si tratta dello stesso principio del merge-sort)

Complessità
-----------
Ogni panchina incrementa 'i' di 1 -> O(P)
Ogni lampione incrementa 'j' di 1 -> al massimo M volte -> O(L)
Totale: O(P + L) --> complessità lineare

; Funzione che calcola il raggio minimo per illuminare una disposizione di panchine e lampioni
(define (lumen4 panchine lampioni)
  (local (i j n m p l dist-max)
    ; Le due liste vengono ordinate. Questo è fondamentale,
    ; perché l'algoritmo sfrutta l'ordine crescente di panchine e lampioni
    ; per scorrere entrambe le liste una sola volta.
    (sort panchine)
    (sort lampioni)
    ; n = numero panchine, m = numero lampioni
    (setq n (length panchine))
    (setq m (length lampioni))
    ; i scorre le panchine, j scorre i lampioni
    ; entrambi partono dall'inizio delle rispettive liste
    (setq i 0)
    (setq j 0)
    ; dist-max terrà la distanza massima minima trovata
    ; cioè il raggio minimo necessario per coprire tutte le panchine
    (setq dist-max 0)
    ; Ciclo principale: si processa ogni panchina
    (while (< i n)
      ; p = posizione della panchina corrente
      (setq p (panchine i))
      ; Per ogni panchina, ci si sposta tra i lampioni
      ; SEMPRE e SOLO in avanti.
      ; L'idea chiave:
      ; - p cresce nel tempo (perché scorriamo panchine ordinate)
      ; - anche j può solo crescere, mai tornare indietro
      ; Si muove l'indice j finché il lampione successivo è PIÙ vicino
      ; alla panchina corrente rispetto al lampione attuale.
      ; Questa condizione è ciò che permette la linearità:
      ; j viene incrementato al massimo m volte in tutta la funzione.
      (while (and (< (+ j 1) m)
                  (<= (abs (- p (lampioni (+ j 1))))
                      (abs (- p (lampioni j)))))
        (++ j))
      ; A questo punto, lampioni[j] è il lampione più vicino alla panchina p.
      (setq l (lampioni j))
      ; Calcoliamo la distanza attuale.
      (let ((d (abs (- p l))))
        ; Se questa distanza è più grande della distanza massima trovata finora,
        ; aggiorniamo dist-max. La distanza massima tra tutte le panchine
        ; e il loro lampione più vicino è il raggio minimo necessario.
        (when (> d dist-max)
          (setq dist-max d)))
      ; Passiamo alla prossima panchina
      (++ i)
    )
    ; dist-max è il raggio minimo che illumina tutte le panchine
    dist-max))

Proviamo:

(lumen4 panc lamp)
;-> 3
(lumen4 p1 l1)
;-> 268
(time (println (lumen4 p2 l2)))
;-> 246
;-> 65.522

Questa funzione è molto veloce.

Complessità delle quattro versioni
----------------------------------
  P = numero di panchine
  L = numero di lampioni
  R = intervallo dei raggi testati (max-r - min-r)

1) lumen1  (versione iniziale, raggio lineare)
   -------------------------------------------
   check(p) = O(L)
   test su tutti i raggi = R iterazioni
   test su tutte le panchine = P
   => O(P * L * R)

2) lumen2  (ricerca binaria sul raggio)
   ------------------------------------
   check(p) = O(L)
   ricerca binaria sul raggio = log R tentativi
   => O(P * L * log R)

3) lumen3  (ricerca binaria su raggio e su lampione piu' vicino)
   -------------------------------------------------------------
   check(p) = O(log L)
   ricerca binaria sul raggio = log R
   => O(P * log L * log R)

4) lumen4  (algoritmo lineare)
   --------------------------------------
   scansione con due indici (stile merge)
   ogni panchina incrementa i di 1
   ogni lampione incrementa j di 1
   => O(P + L)

Versione 5
----------
Possiamo ottimizzare 'lumen4' in diversi modi:

1) Eliminazione delle 'abs' nel ciclo interno
'lumen4' chiama 'abs' due volte ad ogni confronto interno.
'lumen5' trasforma il confronto usando algebra -> zero abs interne.
Questo riduce enormemente il costo di ogni iterazione.

2) Nessun 'let' interno
'lumen4' usa 'let' per calcolare la distanza -> ogni 'let' crea un frame.
'lumen5' non crea nessun frame dentro i cicli -> zero overhead.

3) Uso di 'dolist' anziché '(while ...)' + accesso '(panchine i)'
'dolist':
- non fa lookup '(panchine idx)' -> evita un'operazione costosa
- non richiede incrementi manuali
- è implementato in modo molto efficiente a livello C
Questo da solo rende 'lumen5' molto più rapida.

4) Solo una chiamata a 'abs' per panchina
In lumen4 le 'abs' possono essere tre per panchina (due interne + una esterna).
In lumen5 solo la chiamata finale è necessaria.

5) Meno accessi a liste
'lumen4' fa diverse chiamate:
  (panchine i)
  (lampioni j)
  (lampioni (+ j 1))
'lumen5' riduce gli accessi e li fa in modo più lineare.

Nota: l'algoritmo è lo stesso di 'lumen4', quindi la complessità è sempre lineare.

; Funzione che calcola il raggio minimo per illuminare una disposizione di panchine e lampioni
(define (lumen5 panchine lampioni)
  (local (i n m out)
    ; Ordiniamo panchine e lampioni come in lumen4.
    ; L'algoritmo resta O(P + L), ma l'implementazione diventa più snella.
    (sort panchine)
    (sort lampioni)
    ; Numero di panchine e lampioni
    ; (setq n (length panchine)) ; Non serve
    (setq m (length lampioni))
    ; i = indice del lampione scelto come "candidato corrente".
    (setq i 0)
    ; out = distanza massima trovata.
    (setq out 0)
    ; Uso di dolist:
    ; - evita di accedere continuamente a (panchine k)
    ; - elimina un contatore manuale
    ; - rimuove chiamate '(list index)' ripetute
    ; Questo riduce il numero di operazioni nel ciclo principale,
    ; rendendolo più veloce della variante lumin4.
    (dolist (p panchine)
      ; Ciclo per avanzare nei lampioni:
      ; Si sfrutta una proprietà matematica:
      ;   abs(p - L[i+1]) < abs(p - L[i])
      ; è equivalente a:
      ;   (p - L[i]) > (L[i+1] - p)
      ; cioè non serve abs, basta confrontare due differenze semplici.
      ; Questo elimina tutte le chiamate a abs() nel ciclo interno.
      ; In lumen4 invece le abs vengono chiamate continuamente.
      (while (and (< (+ i 1) m)
                  (> (- p (lampioni i))
                     (- (lampioni (+ i 1)) p)))
        (++ i))
      ; Aggiorna la distanza massima (1 sola abs a ciclo esterno).
      ; Eliminato il let interno usato in lumen4 che creava un frame
      ; a ogni iterazione: qui l'allocazione è zero.
      (setq out (max out (abs (- (lampioni i) p))))
    )
    out))

Proviamo:

(lumen5 panc lamp)
;-> 3
(lumen5 p1 l1)
;-> 268
(time (println (lumen5 p2 l2)))
;-> 246
;-> 15.587

Questa funzione è la più veloce in assoluto.

Verifica di correttezza di tutte le funzioni:

(for (i 1 100)
  (setq p (rand 1000 100))
  (setq l (rand 1000 10))
  (if-not (= (lumen1 p l) (lumen2 p l) (lumen3 p l (lumen4 p l) (lumen5 p l)))
    (println p { } l)))
;-> nil


---------------------------
Allungamento di una stringa
---------------------------

Data una stringa di lunghezza N (es. "abc"), stamparla nel modo seguente:

  riga 1         --> stringa (0 spazi tra i caratteri)
  riga 2         --> stringa con 1 spazio tra i caratteri
  riga 3         --> stringa con 2 spazi tra i caratteri
  ...
  riga N         --> stringa con (N-1) spazi tra i caratteri
  riga (N+1)     --> stringa con (N-2) spazi tra i caratteri
  riga (N+2)     --> stringa con (N-3) spazi tra i caratteri
  ...
  riga (N*2 - 1) --> stringa (0 spazi tra i caratteri)

Esempio:
stringa = abcd
output =
  abcd
  a b c d
  a  b  c  d
  a   b   c   d
  a  b  c  d
  a b c d
  abcd

Esempio:
stringa = 12345
output =
  12345
  1 2 3 4 5
  1  2  3  4  5
  1   2   3   4   5
  1    2    3    4    5
  1   2   3   4   5
  1  2  3  4  5
  1 2 3 4 5
  12345

Esempio di funzionamento:

(let (ch '("a" "b" "c"))
  (for (i 0 2)
    (println (join (flat (transpose (list ch (dup (dup " " i) 10 true)))))))
  (for (i 1 0)
    (println (join (flat (transpose (list ch (dup (dup " " i) 10 true))))))))
;-> abc
;-> a b c
;-> a  b  c
;-> a b c
;-> abc

(define (elongate str)
  (local (chars k)
    (setq chars (explode str)) ; lista di caratteri
    (setq k (- (length str) 1)) ; spazio massimo tra due numeri
    (for (i 0 k) ; stampa le prime (k+1) righe
      (println (join (flat (transpose (list chars (dup (dup " " i) 10 true)))))))
    (for (i (- k 1) 0) ; stampa le restanti k righe
      (println (join (flat (transpose (list chars (dup (dup " " i) 10 true)))))))))

(elongate "abcd")
;-> abcd
;-> a b c d
;-> a  b  c  d
;-> a   b   c   d
;-> a  b  c  d
;-> a b c d
;-> abcd
(elongate "12345")
;-> 12345
;-> 1 2 3 4 5
;-> 1  2  3  4  5
;-> 1   2   3   4   5
;-> 1    2    3    4    5
;-> 1   2   3   4   5
;-> 1  2  3  4  5
;-> 1 2 3 4 5
;-> 12345

Adesso convertiamo il doppio ciclo 'for' in un ciclo 'for' unico.

Supponiamo di avere il seguente codice:

(let (k 3)
  (for (i 0 k)
    (println (dup "*" i)))
  (for (i (- k 1) 0)
    (println (dup "*" i))))
;-> *
;-> **
;-> ***
;-> **
;-> *

Bisogna costruire una sequenza che faccia:

  '0 1 2 ... k ... 2 1 0'

Con un solo 'for' che va da 0 a (* 2 k).
Per un indice globale 'i' che va da 0 a (* 2 k):
- se i <= k  -> new-i = i
- altrimenti -> new-i = (- (* 2 k) i)

Questo genera esattamente la sequenza: '0 1 2 ... k ... 2 1 0'
E il codice diventa:

(let (k 3)
  (for (i 0 (* 2 k))
    (let (new-i (if (<= i k) i (- (* 2 k) i)))
      (println (dup "*" new-i)))))
;-> *
;-> **
;-> ***
;-> **
;-> *

La funzione 'elongate' diventa:

(define (elongate str)
  (let ( (chars (explode str))    ; lista di caratteri
         (k (- (length str) 1)) ) ; spazio massimo tra due numeri
  (for (i 0 (* 2 k)) ; ciclo unico per tutte le righe
    (let (new-i (if (<= i k) i (- (* 2 k) i)))
      (println (join (flat (transpose
                (list chars (dup (dup " " new-i) 10 true))))))))))

(elongate "abcd")
;-> abcd
;-> a b c d
;-> a  b  c  d
;-> a   b   c   d
;-> a  b  c  d
;-> a b c d
;-> abcd
(elongate "12345")
;-> 12345
;-> 1 2 3 4 5
;-> 1  2  3  4  5
;-> 1   2   3   4   5
;-> 1    2    3    4    5
;-> 1   2   3   4   5
;-> 1  2  3  4  5
;-> 1 2 3 4 5
;-> 12345

Versione code-golf (161 caratteri, one-line):

(define(f s)
(let((c(explode s))(k(-(length s)1)))
(for(i 0(* 2 k))(let(d(if(<= i k)i(-(* 2 k)i)))
(println(join(flat(transpose(list c(dup(dup" "d)10 true))))))))))

(f "12345")
;-> 12345
;-> 1 2 3 4 5
;-> 1  2  3  4  5
;-> 1   2   3   4   5
;-> 1    2    3    4    5
;-> 1   2   3   4   5
;-> 1  2  3  4  5
;-> 1 2 3 4 5
;-> 12345

(f ">>>>>>")
;-> >>>>>>
;-> > > > > > >
;-> >  >  >  >  >  >
;-> >   >   >   >   >   >
;-> >    >    >    >    >    >
;-> >     >     >     >     >     >
;-> >    >    >    >    >    >
;-> >   >   >   >   >   >
;-> >  >  >  >  >  >
;-> > > > > > >
;-> >>>>>>


------------------
Pagine di un libro
------------------

Le pagine di un libro sono numerate da 1 a n.
Quando sono stati sommati i numeri di pagina del libro, uno dei numeri di pagina è stato aggiunto per errore due volte, ottenendo una somma errata di 5100.
Qual è il numero della pagina aggiunta due volte?

La somma dei primi N numeri vale:

; Funzione che calcola la somma dei primi n numeri
(define (somma n) (/ (* n (+ n 1)) 2))

Ponendo x la pagina contata due volte, risulta:

  n*(n+1)/2 + x = somma = 5100

Possiamo stabilire un limite superiore e inferiore per il numero di pagine contenute nel libro, dove il limite superiore massimizza la somma delle pagine e il limite inferiore massimizza la pagina extra (e quindi minimizza la somma totale delle pagine).
In altre parole, ponendo x = 1 troviamo il limite minimo, mentre ponendo x = n troviamo il limite massimo:

Limite superiore di n: n*(n+1)/2 + 1 = somma = 5100
  n^2 + n - 10198 = 0
  n^2 + n - 2*(s - 1) = 0

Limite inferiore di n: n*(n+1)/2 + n = s = 5100
  n^2 + 3*n - 10200 = 0
  n^2 + 3*n - 2*s = 0

Dopo aver calcolato i limiti (inferiore e superiore) di n possiamo calcolare i valori della x e poi verificare quale risultato sia corretto.

Nota: le equazioni n^2 + n - 2*(s - 1) = 0 e n^2 + 3*n - 2*s = 0 hanno sempre radici reali, perchè (b^2 - 4ac) è sempre positivo.

(define (quadratic a b c)
"Find the solutions of A*x^2 + B*x + C = 0"
  (letn ((delta (sub (mul b b) (mul 4 a c)))
         (den (mul 2 a))
         x1 x2 i1 i2)
    (cond
      ((= a 0)  ; equazione lineare
        (if (!= b 0)
            (setq x1 (sub 0 (div c b))  x2 x1  i1 0.0  i2 0.0)))
      ((> delta 0) ; due radici reali
        (letn ((sd (sqrt delta)))
          (setq x1 (div (add (sub 0 b) sd) den))
          (setq x2 (div (sub (sub 0 b) sd) den))
          (setq i1 0.0 i2 0.0)))
      ((< delta 0) ; radici complesse
        (letn ((sd (sqrt (sub 0 delta))))
          (setq x1 (div (sub 0 b) den))
          (setq x2 x1)
          (setq i1 (div sd den) i2 (sub 0 (div sd den)))))
      (true ; delta = 0: radici coincidenti
        (setq x1 (sub 0 (div b den)))
        (setq x2 x1 i1 0.0 i2 0.0))
    )
    (list (list x1 i1) (list x2 i2))))

; Funzione che calcola x per un dato n:
(define (solve-x s n) (- s (/ (* n (+ n 1)) 2)))

; Funzione che calcola la pagina che è stata calcolata due volte
(define (book s)
  (setq upper (quadratic 1 1 (* -2 (- s 1))))
  (println "upper: " upper)
  (cond
    ((> (upper 0 0) 0) (setq massimo (floor (upper 0 0))))
    ((> (upper 1 0) 0) (setq massimo (floor (upper 1 0))))
    (true (println "Errore: nessuna soluzione positiva " upper)))
  (setq lower (quadratic 1 3 (* -2 s)))
  (println "lower: " lower)
  (cond
    ((> (lower 0 0) 0) (setq minimo (ceil (lower 0 0))))
    ((> (lower 1 0) 0) (setq minimo (ceil (lower 1 0))))
    (true (println "Errore: nessuna soluzione positiva " lower)))
  (println "minimo: " minimo)
  (println "massimo: " massimo)
  (setq stop nil)
  (for (num minimo massimo 1 stop)
    (setq x (solve-x s num))
    (setq pagine num)
    (println "num: " num)
    (setq x (solve-x s num))
    (println "x: " x)
    (print "(somma " num "):" (somma num))
    (when (and (!= x 0) (= (- s x) (somma num)))
      (setq pagine num)
      (setq stop true)))
  (if-not stop
      (println "Errore: nessuna soluzione")
      ;else
      (begin
        (println "Somma delle pagine da 1 a " pagine " : " (somma pagine))
        (println "Somma delle pagine (" s ") - pagina doppia (" x ") : " (- s x))
        (println "Pagina doppia (contata due volte): " x))))

Proviamo:

(book 5100)
;-> upper: ((100.4863852209792 0) (-101.4863852209792 0))
;-> lower: ((99.50618792925511 0) (-102.5061879292551 0))
;-> minimo: 100
;-> massimo: 100
;-> num: 100
;-> x: 50
;-> (somma 100):5050Somma delle pagine da 1 a 100 : 5050
;-> Somma delle pagine (5100) - pagina doppia (50) : 5050
;-> Pagina doppia (contata due volte): 50
;-> 50

(book 1031)
;-> upper: ((44.88997686714546 0) (-45.88997686714546 0))
;-> lower: ((43.93401809217406 0) (-46.93401809217406 0))
;-> minimo: 44
;-> massimo: 44
;-> num: 44
;-> x: 41
;-> (somma 44):990Somma delle pagine da 1 a 44 : 990
;-> Somma delle pagine (1031) - pagina doppia (41) : 990
;-> Pagina doppia (contata due volte): 41
;-> 41

(book 500001500000)
;-> upper: ((1000000.999998 0) (-1000001.999998 0))
;-> lower: ((1000000 0) (-1000003 0))
;-> minimo: 1000000
;-> massimo: 1000000
;-> num: 1000000
;-> x: 1000000
;-> (somma 1000000):500000500000Somma delle pagine da 1 a 1000000 : 500000500000
;-> Somma delle pagine (500001500000) - pagina doppia (1000000) : 500000500000
;-> Pagina doppia (contata due volte): 1000000
;-> 1000000


------------------------------------------------------
Massime occorrenze dei numeri nelle tavole pitagoriche
------------------------------------------------------

Dato un numero intero positivo N:
1) calcolare la tavola pitagorica di ordine N, cioè la tabella di moltiplicazione 1-N.
2) calcolare quale numero ha il massimo delle occorrenze (se esistono più numeri con le stesse occorrenze, allora prendere il numero con valore massimo).

Esempio:
M(n) è la tabella in cui m(x,y) = x*y per x = 1 a n e y = 1 a n.
In M(10), i numeri più frequenti sono 6, 8, 10, 12, 18, 20, 24, 30, 40, ciascuno dei quali ricorre 4 volte.
Il più grande di questi numeri è 40, quindi a(10) = 40.

Sequenza OEIS A057143:
Largest of the most frequently occurring numbers in 1-to-n multiplication table.
  1, 2, 6, 4, 4, 12, 12, 24, 24, 40, 40, 24, 24, 24, 60, 60, 60, 36, 36,
  60, 60, 60, 60, 120, 120, 120, 120, 168, 168, 120, 120, 120, 120, 120,
  120, 180, 180, 180, 180, 120, 120, 120, 120, 120, 360, 360, 360, 360,
  360, 360, 360, 360, 360, 360, 360, 360, 360, 360, ...

(define (major N)
  (let ( (pita '()) (unici '()) )
    (for (i 1 N)
      (for (j 1 N)
        (push (* i j) pita -1)))
    (setq unici (unique pita))
    (last (first (sort (map list (count unici pita) unici) >)))))

(map major (sequence 1 70))
;-> (1 2 6 4 4 12 12 24 24 40 40 24 24 24 60 60 60 36 36
;->  60 60 60 60 120 120 120 120 168 168 120 120 120 120 120
;->  120 180 180 180 180 120 120 120 120 120 360 360 360 360
;->  360 360 360 360 360 360 360 360 360 360 360 360 360 360
;->  360 360 360 360 360 360 360 840)


-----------------------------------------
Lista di tutti i colori RGB (esadecimale)
-----------------------------------------

Scrivere una funzione che genera tutti i colori RGB in formato esadecimale.
Il numero di colori RGB è 16777216: 16 milioni 777 mila 216.

Esempi:
  rgb = (125 15 33)
  esadecimale = "7D0F21"

  rgb = (16 24 255)
  esadecimale = "1018FF"

(define (all-rgb)
  (let (rgb '())
    (for (r 0 255)
      (for (g 0 255)
        (for (b 0 255)
          (push (format "%02X%02X%02X" r g b) rgb))))))

(time (println (length (setq res (all-rgb)))))
;-> 16777216
;-> 16700.35

(slice res 100000 10)
;-> ("FE795F" "FE795E" "FE795D" "FE795C" "FE795B" "FE795A" "FE7959"
;->  "FE7958" "FE7957" "FE7956")


-------------------------------------------------------------
Sequenza dei numeri k tali che 2^k non supera l'n-esimo primo
-------------------------------------------------------------

Sequenza OEIS A098388
a(n) = floor(log_2(prime(n))).
  1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
  7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8, 9, 9, 9, 9, 9, 9, 9, 9, ...

a(n) is the greatest k such that 2^k does not exceed prime(n)

(define (primes num-primes)
"Generate a given number of prime numbers (starting from 2)"
  (let ( (k 3) (tot 1) (out '(2)) )
    (until (= tot num-primes)
      (when (= 1 (length (factor k)))
        (push k out -1)
        (++ tot))
      (++ k 2))
    out))

(define (seq limite)
  (map (fn(x) (floor (log x 2))) (primes limite)))

(seq 105)
;-> (1 1 2 2 3 3 4 4 4 4 4 5 5 5 5 5 5 5 6 6 6 6 6 6
;->  6 6 6 6 6 6 6 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
;->  7 7 7 7 7 7 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
;->  8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
;->  8 9 9 9 9 9 9 9 9)


--------------------------------------------------------------------
Numero di 0 nella rappresentazione binaria dell'n-esimo numero primo
--------------------------------------------------------------------

Sequenza OEIS A035103:
Number of 0's in binary representation of n-th prime.
  1, 0, 1, 0, 1, 1, 3, 2, 1, 1, 0, 3, 3, 2, 1, 2, 1, 1, 4, 3, 4, 2, 3, 3, 4,
  3, 2, 2, 2, 3, 0, 5, 5, 4, 4, 3, 3, 4, 3, 3, 3, 3, 1, 5, 4, 3, 3, 1, 3, 3,
  3, 1, 3, 1, 7, 5, 5, 4, 5, 5, 4, 5, 4, 3, 4, 3, 4, 5, 3, 3, 5, 3, 2, 3, 2,
  1, 5, 4, 5, 4, 4, 4, 2, 4, 2, 2, 5, 4, 3, 2, 3, 1, 2, 2, 2, 1, 1, 7, 6, 5,
  6, 5, 5, 5, 4, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (bit1-count num)
"Count the bit 1 of an integer"
  (let (counter 0)
    (while (> num 0)
      (setq num (& num (- num 1)))
      (++ counter))
    counter))

(define (bit0-count num)
"Count the bit 0 of an integer"
  (- (length (bits num)) (bit1-count num)))

(define (seq limite)
  (let (out '())
    (for (num 2 limite)
      (if (prime? num) (push (bit0-count num) out -1)))
    out))

(seq 500)
;-> (1 0 1 0 1 1 3 2 1 1 0 3 3 2 1 2 1 1 4 3 4 2 3 3 4
;->  3 2 2 2 3 0 5 5 4 4 3 3 4 3 3 3 3 1 5 4 3 3 1 3 3
;->  3 1 3 1 7 5 5 4 5 5 4 5 4 3 4 3 4 5 3 3 5 3 2 3 2
;->  1 5 4 5 4 4 4 2 4 2 2 5 4 3 2 3 1 2 2 2)


----------------------
Quanti pesci nel lago?
----------------------

Un biologo va in un lago e cattura e marca k pesci il giorno 1.
Il giorno 2 cattura k pesci e ne trova x già marcati il giorno precedente (quindi ne marca k-x).
Il giorno 3 cattura k pesci e ne trova y già marcati i gioni precedenti (quindi ne marca k-y).
Stimare quanti pesci ci sono nel lago.

Descrizione del problema
- Giorno 1: catturiamo e marchiamo k pesci.
- Giorno 2: catturiamo k pesci e ne troviamo x gia' marcati.
  Quindi marchiamo (k - x) pesci nuovi.
- Giorno 3: catturiamo k pesci e ne troviamo y marcati nei giorni precedenti.
  Quindi marchiamo (k - y) pesci nuovi.
- Obiettivo: stimare il numero totale N di pesci nel lago.

Assunzioni del modello classico: popolazione chiusa, mescolamento casuale, marcatura innocua, stessa probabilita' di cattura per tutti.

1. Metodo elementare
--------------------
A) Stima da giorni 1 e 2 (metodo Lincoln–Petersen)

Stima semplice:

  N12 = (k * k) / x

Stima corretta per piccoli campioni (formula di Chapman):

  N12c = ((k + 1) * (k + 1)) / (x + 1) - 1

B) Stima da giorni 2 e 3

Dopo il giorno 2, il numero totale di pesci marcati è:

  M2 = k + (k - x) = 2*k - x

Usando i dati del giorno 3:

Stima semplice:

  N23 = (M2 * k) / y
      = (k * (2*k - x)) / y

Stima di Chapman:

  N23c = ((M2 + 1) * (k + 1)) / (y + 1) - 1

Nota: Se le ipotesi del modello sono valide, N12 e N23 dovrebbero essere simili.
Se sono molto diversi, le assunzioni potrebbero essere violate.

2. Metodo rigoroso: massimo della verosimiglianza (MLE)
-------------------------------------------------------
Il modello usa la distribuzione ipergeometrica.

Giorno 2 (recapture x):

Probabilita':

  P2 = C(k, x) * C(N - k, k - x) / C(N, k)

Giorno 3 (recapture y):

Dopo giorno 2 i marcati totali sono:

  M2 = 2*k - x

Probabilita':

  P3 = C(M2, y) * C(N - M2, k - y) / C(N, k)

Verosimiglianza totale (in funzione di N):

  L(N) = P2 * P3

La stima MLE è il valore di N (intero) che massimizza L(N).

La ricerca si fa provando N = 2*k, 2*k+1, ..., fino a un massimo (es. 10*k
o finchè L(N) scende molto).

Vediamo una funzione che esegue la ricerca MLE con log-verosimiglianza:

(define (log-choose n k)
  (let ((k (min k (sub n k))))
    (if (< k 0) -1e99
      (let ((s 0) (i 1))
        (for (i 1 k)
          (setf s (add s (log (div (add n (sub k i) 1) i)))))
        s))))

(define (log-hyperprob N M sample success)
  (add
    (log-choose M success)
    (log-choose (sub N M) (sub sample success))
    (sub (log-choose N sample))))

(define (estimate-N-mle k x y Nmax)
  (let ((M2 (sub (add k k) x))
        (bestN 0)
        (bestLogL -1e99))
    (for (N (max (add k k) (add M2 k)) Nmax)
      (letn ((l1 (log-hyperprob N k k x))
            (l2 (log-hyperprob N M2 k y))
            (logL (add l1 l2)))
        (when (> logL bestLogL)
          (setf bestLogL logL)
          (setf bestN N))))
    (list bestN bestLogL)))

(estimate-N-mle 100 20 35 2000)
;-> (509 -4.619505243265053)

3. Considerazioni pratiche
--------------------------
- Confrontare le due stime semplici N12 e N23: devono essere simili.
- Se vogliamo una singola stima robusta usiamo il metodo MLE.
- Per un intervallo di confidenza, possiamo esaminare i valori di N vicino al massimo e vedere dove la log-verosimiglianza scende di una quantità standard (profilo della verosimiglianza).


--------------------------------------------------
Numeri non-square, non-cube, squarefree e cubefree
--------------------------------------------------

Non-Square: numeri che non sono il quadrato di un numero intero positivo
Non-Cube: numeri che non sono il cubo di un numero intero positivo

(define (square? num)
"Check if an integer is a perfect square"
  (let (isq (int (sqrt num)))
    (= num (* isq isq))))

(define (cube? num)
"Check if an integer is a perfect cube"
  (let ((cr (int (pow num (div 3)))))
    (cond ((= (* cr cr cr) num) true)
          ((= (* (+ cr 1) (+ cr 1) (+ cr 1)) num) true)
          ((= (* (- cr 1) (- cr 1) (- cr 1)) num) true)
          (true nil))))

Sequenza OEIS A000037:
Numbers that are not squares (or, the nonsquares).
  2, 3, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24,
  26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 37, 38, 39, 40, 41, 42, 43, 44,
  45, 46, 47, 48, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
  65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 82, 83,
  84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, ...

Formule:
  a(n) = n + floor(1/2 + sqrt(n)).
  a(n) = n + floor(sqrt( n + floor(sqrt n))).

(clean square? (sequence 1 99))
;-> (2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24
;->  26 27 28 29 30 31 32 33 34 35 37 38 39 40 41 42 43 44
;->  45 46 47 48 50 51 52 53 54 55 56 57 58 59 60 61 62 63
;->  65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 82 83
;->  84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99)

Sequenza OEIS A007412:
The noncubes: a(n) = n + floor((n + floor(n^(1/3)))^(1/3)).
  2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
  23, 24, 25, 26, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41,
  42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
  60, 61, 62, 63, 65, 66, 67, 68, 69, 70, 71, ...

(clean cube? (sequence 1 71))
;-> (2 3 4 5 6 7 9 10 11 12 13 14 15 16 17 18 19 20 21 22
;->  23 24 25 26 28 29 30 31 32 33 34 35 36 37 38 39 40 41
;->  42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59
;->  60 61 62 63 65 66 67 68 69 70 71)

Sequenza OEIS A094784:
Numbers that are neither squares nor cubes.
  2, 3, 5, 6, 7, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24,
  26, 28, 29, 30, 31, 32, 33, 34, 35, 37, 38, 39, 40, 41, 42, 43, 44, 45,
  46, 47, 48, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 65,
  66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 82, ...

(clean (fn(x) (or (square? x) (cube? x))) (sequence 1 82))
;-> (2 3 5 6 7 10 11 12 13 14 15 17 18 19 20 21 22 23 24
;->  26 28 29 30 31 32 33 34 35 37 38 39 40 41 42 43 44 45
;->  46 47 48 50 51 52 53 54 55 56 57 58 59 60 61 62 63 65
;->  66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 82)

SquareFree: numeri che non sono divisibili per un quadrato maggiore di 1.
CubeFree: numeri che non sono divisibili per un cubo maggiore di 1.

(define (squarefree? num)
  (if (= num 1) true
      (letn ( (fattori (factor num))
              (unici (unique fattori)) )
        ; non esiste un fattore quadrato?
        (not (find 1 (count unici fattori) <)))))

(define (cubefree? num)
  (if (= num 1) true
      (letn ( (fattori (factor num))
              (unici (unique fattori)) )
        ; non esiste un fattore cubico?
        (not (find 2 (count unici fattori) <)))))

Sequenza OEIS A005117:
Squarefree numbers: numbers that are not divisible by a square greater than 1.
  1, 2, 3, 5, 6, 7, 10, 11, 13, 14, 15, 17, 19, 21, 22, 23, 26, 29, 30, 31,
  33, 34, 35, 37, 38, 39, 41, 42, 43, 46, 47, 51, 53, 55, 57, 58, 59, 61,
  62, 65, 66, 67, 69, 70, 71, 73, 74, 77, 78, 79, 82, 83, 85, 86, 87, 89,
  91, 93, 94, 95, 97, 101, 102, 103, 105, 106, 107, 109, 110, 111, 113, ...

(filter squarefree? (sequence 1 113))
;-> (1 2 3 5 6 7 10 11 13 14 15 17 19 21 22 23 26 29 30 31
;->  33 34 35 37 38 39 41 42 43 46 47 51 53 55 57 58 59 61
;->  62 65 66 67 69 70 71 73 74 77 78 79 82 83 85 86 87 89
;->  91 93 94 95 97 101 102 103 105 106 107 109 110 111 113)

Sequenza OEIS A004709:
Cubefree numbers: numbers that are not divisible by any cube > 1.
  1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23,
  25, 26, 28, 29, 30, 31, 33, 34, 35, 36, 37, 38, 39, 41, 42, 43, 44, 45, 46,
  47, 49, 50, 51, 52, 53, 55, 57, 58, 59, 60, 61, 62, 63, 65, 66, 67, 68, 69,
  70, 71, 73, 74, 75, 76, 77, 78, 79, 82, 83, 84, 85, ...

(filter cubefree? (sequence 1 85))
;-> (1 2 3 4 5 6 7 9 10 11 12 13 14 15 17 18 19 20 21 22 23
;->  25 26 28 29 30 31 33 34 35 36 37 38 39 41 42 43 44 45 46
;->  47 49 50 51 52 53 55 57 58 59 60 61 62 63 65 66 67 68 69
;->  70 71 73 74 75 76 77 78 79 82 83 84 85)


-----------------------------------------
Somma massima di elementi non consecutivi
-----------------------------------------

Abbiamo una lista di numeri interi positivi.
Partendo dal primo numero e attraversando la lista possiamo prendere qualunque numero con il solo vincolo di non poter prendere due numeri adiacenti.
Scrivere una funzione che produce la somma massima con i numeri presi.

Possiamo usare un approccio ricorsivo (con memoizzazione).
Una funzione dfs(i) che calcola la somma massima che può essere presa a partire dall'indice i.
A ogni indice, abbiamo due possibilità:
Prendere il numero corrente e saltare quello successivo: lista[i] + dfs(i + 2)
Saltare il numero corrente e considerare quello successivo: dfs(i + 1)
Questo metodo funziona perchè quando ci troviamo nella posizione i, il massimo che possiamo ottenere dipende solo dalle scelte disponibili dalla posizione i in poi, non importa come siamo arrivati alla posizione i.
Questa è la proprietà della sottostruttura ottimale che rende applicabile la programmazione dinamica.

Algoritmo:
- dfs(i) restituisce la somma massima ottenibile dagli indici i..end
- Se l'indice va oltre la lista, restituisce 0
- Se il valore e gia in memo, viene riusato
- Altrimenti calcola:
  - prendi = lst[i] + dfs(i+2)
  - salta = dfs(i+1)
- Salva il massimo in memo
- Parte da dfs(0)

(define (max-somma-lista lst)
  (letn ((n (length lst)) (memo '()))
    (define (dfs i)
      (if (>= i n) '(0 ())
        (let (m (lookup i memo))
          (if m m
            (letn ((res1 (dfs (+ i 1))) (sum1 (res1 0)) (list1 (res1 1))
                   (res2 (dfs (+ i 2))) (sum2 (res2 0)) (list2 (res2 1))
                   (sum2 (+ (lst i) sum2)) (list2 (cons (lst i) list2))
                   (best (if (> sum2 sum1) (list sum2 list2) (list sum1 list1))))
              (setf (lookup i memo) best)
              best)))))
    (dfs 0)))

Proviamo:

(max-somma-lista '(1 2 3 1))
;-> (4 (1 3))
(max-somma-lista '(3 2 7 10))
;-> (13 (3 10))
(max-somma-lista '(1 3 11 2 1 10))
;-> (22 (1 11 10))

Vediamo una versione iterativa (bottom-up).

Algoritmo:
- dp[i] contiene la somma massima ottenibile dalla posizione i
- scelte[i] contiene la lista degli elementi corrispondenti
- Costruzione bottom-up:
  - Base: dp[n] = 0 e lista vuota
  - Base: dp[n-1] = lst[n-1]
  - Per ogni i da n-2 a 0:
    - confronta "prendi" e "salta"
    - salva in dp[i] il migliore
    - salva in scelte[i] la lista corrispondente

(define (max-somma-lista-iter lst)
  (letn ((n (length lst)) (dp '()) (scelte '()) (i 0))
    (for (k 0 n) (push 0 dp -1) (push '() scelte -1))
    (setf (dp n) 0) (setf (scelte n) '())
    (setf (dp (sub n 1)) (lst (sub n 1))) (setf (scelte (sub n 1)) (list (lst (sub n 1))))
    (for (i (sub n 2) 0 -1)
      (letn ((prendi (add (lst i) (dp (add i 2))))
             (salta (dp (add i 1))))
        (if (> prendi salta)
          (begin (setf (dp i) prendi) (setf (scelte i) (cons (lst i) (scelte (add i 2)))))
          (begin (setf (dp i) salta) (setf (scelte i) (scelte (add i 1)))))))
    (list (dp 0) (scelte 0))))

Proviamo:

(max-somma-lista-iter '(1 2 3 1))
;-> (4 (1 3))
(max-somma-lista-iter '(3 2 7 10))
;-> (13 (3 10))
(max-somma-lista-iter '(1 3 11 2 1 10))
;-> (22 (1 11 10))


------------
Il numero 37
------------

Dato un numero intero positivo N < 10 risulta:

  NNN/(N+N+N) = 37, dove NNN è la concatenazione di N

(define (il37 num)
  (let ( (numer (int (dup (string num) 3) 0 10))
         (denom (+ num num num)) )
    (div numer denom)))

(map il37 (sequence 1 9))
;-> (37 37 37 37 37 37 37 37 37)

In generale risulta che la soluzione sarà sempre 33...36...67 dove al posto dei primi puntini ci saranno tanti 3 e al posto dei secondi puntini ci saranno tanti 6.
Per un numero a 1 cifra sarà 37.
Per un numero a 2 cifre 3367.
Per un numero a 3 cifre 333667.
Per un numero a 4 cifre 33336667.
Per un numero a k cifre: 3(k volte)6(k-1 volte)7
Quindi tra il primo 3 e il 7 ci saranno tanti 3 quanti il numero delle cifre e tanti 6 pari al numero delle cifre meno 1.

(define (generale num)
  (let ( (len (length num))
         (numer (int (dup (string num) 3) 0 10))
         (denom (+ num num num))
         (res1 0) (res2 0) )
    (setq res1 (div numer denom))
    (setq res2 (int (string (dup (string 3) len)
                            (dup (string 6) (- len 1))
                            7) 0 10))
    (list res1 res2)))

Proviamo:

(map generale (sequence 1 20))
;-> ((37 37) (37 37) (37 37) (37 37) (37 37) (37 37) (37 37) (37 37) (37 37)
;->  (3367 3367) (3367 3367) (3367 3367) (3367 3367) (3367 3367) (3367 3367)
;->  (3367 3367) (3367 3367) (3367 3367) (3367 3367) (3367 3367))

(map generale '(1 22 333 4444 55555))
;-> ((37 37) (3367 3367) (333667 333667) (33336667 33336667)
;->  (3333366667 3333366667))


----------------------------
Eliminazione di una sequenza
----------------------------

Data una sequenza di interi da 0 a N applicare il seguente processo:
1) Mischiare gli elementi delle lista
2) Eliminare gli elementi il cui valore è uguale alla loro posizione
3) Se esiste almeno un elemento con valore >= alla lunghezza della lista,
   allora stop (restituire il numero di elementi della lista),
   altrimenti andare al passo 1).

Esempio:
  N = 3
  sequenza = (0 1 2 3)
  mischia --> (0 2 1 3)
  Togliere lo 0 e il 3 --> (1 2)
  mischia --> (2 1)
  Togliere l'1 --> (2)
  Il 2 è maggiore della lunghezza della lista (1) --> Stop
  Valore restituito = 1

Qual'è la probabilità che al termine del processo la lista sia vuota?
(cioè siano stati eliminati tutti gli elementi della lista)

Scriviamo una funzione per simulare il processo.
Per verificare la condizione di stop usiamo la seguente espressione:
  (exists (fn(x) (< x (length lst))) lst)

(define (processo num)
  (local (tmp)
    (setq lst (sequence 0 num))
    (while (and lst (exists (fn(x) (< x (length lst))) lst))
      ; Mischia la lista
      ; il parametro 'true' permette a 'randomize' di restituire
      ; una lista uguale a quella passata come parametro,
      ; altrimenti con alcune liste (es. (0 2)) si avrebbe un loop infinito
      (setq lst (randomize lst true))
      ;(print lst) (read-line)
      ; Elimina gli lementi che hanno valore uaguale alla loro posizione
      (setq tmp '())
      (dolist (el lst)
        (if-not (= el $idx) (push el tmp)))
      (setq lst tmp)
      ;(print lst) (read-line)
    )
    (length lst)))

Proviamo:

N = 1 --> seq = (0 1)
(collect (processo 1) 20)
;-> (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

N = 2 --> seq = (0 1 2)
(collect (processo 2) 20)
;-> (1 0 0 0 0 0 1 0 1 1 1 0 1 0 1 1 0 1 0 0)

N = 3 --> seq = (0 1 2 3)
(collect (processo 3 ) 20)
;-> (1 1 1 2 2 0 2 0 1 1 1 1 1 1 0 0 2 2 1 1)

N = 3 --> seq = (0 1 2 3 4)
(collect (processo 4 ) 20)
;-> (1 2 2 1 2 1 1 1 2 1 2 1 2 2 2 2 0 1 2 1)

Con l'aumentare di N la probabilità che il processo produca una lista vuota diminuisce velocemente.

(count '(0) (collect (processo 9) 1e3))
;-> (0) ; nessuna lista vuota
(count '(0) (collect (processo 9) 1e6))
;-> (24)

(time (println (count '(0) (collect (processo 20) 1e6))))
;-> (0)
;-> 56536.502

(time (println (count (sequence 0 20) (collect (processo 20) 1e6))))
;-> (0 0 0 15 1334 25093 161210 383889 331549 91551 5359 0 0 0 0 0 0 0 0 0 0)
;-> 56599.113

Vediamo il processo da un punto di vista matematico.

Stato iniziale:

  L(N+1) = (0, 1, 2, 3, ..., N)

Ad ogni iterazione:

1. si prende una permutazione casuale uniforme della lista corrente L(k)
2. si eliminano tutti gli elementi tali che:

  valore = posizione

3. il processo continua solo se:

  per ogni x in L(k) vale x < k

altrimenti si ferma e restituisce k.

L'obiettivo è calcolare la probabilità che il processo termini con:

  k = 0 (lista vuota)

Dopo il primo shuffle, la lista è una permutazione uniforme di: (0 1 ... N).

Dopo la rimozione dei punti fissi, rimane un insieme di cardinalità:

  K = (N+1) - F

dove F è il numero di punti fissi della permutazione.

F ha distribuzione asintoticamente simile a:

  Poisson(lambda=1) --> P(X = k) = exp(-1) / k!.

Una variabile casuale X ha distribuzione di Poisson con parametro lambda se:

  P(X = k) = exp(-lambda) * lambda^k / k!

per k = 0, 1, 2, ...

Nel nostro caso lambda = 1, quindi:

  P(X = k) = exp(-1) / k!

La lista residua è un sottoinsieme casuale di (0 1 ... N) condizionato a non contenere i punti fissi.

Da questo punto in poi, la struttura iniziale è dimenticata.

Ad ogni iterazione successiva:

- la lista corrente viene randomizzata in modo uniforme
- in media viene eliminato circa 1 elemento
- la lunghezza cambia lentamente: k -> k - 1

Ma la condizione di stop richiede:

  max(L(k)) < k

Se anche un solo elemento >= k è presente, il processo termina.

Condizionando sulla lunghezza k, la lista è approssimativamente un sottoinsieme casuale di (0 1 ... N) di cardinalità k.

La probabilità che tutti gli elementi siano < k vale:

 P(max(L(k)) < k) = C(k, k) / C(N+1, k) = 1 / C(N+1, k)

dove C è il coeffiente binomiale.

Questa probabilità diventa rapidamente minuscola quando k < N+1.

Per arrivare a k = 0 bisogna superare il test di stop per tutti i valori:

  k = N+1, N, N-1, ..., 1

Una stima superiore è:

  P(lista vuota) <= Prod[k=1,N+1]( 1 / C(N+1, k) )

cioè:

  P(lista vuota) <= 1 / Prod[k=1,N+1] C(N+1, k) )

Usando la stima:

  C(N+1, k) >= ((N+1) / k)^k

si ottiene:

  log P(lista vuota) <= - Sum[k=1,N]( k * log(N / k) )

che cresce come:

  N^2 / 2

Quindi:

  P(lista vuota) ~= exp(-c*N^2),  c > 0

Partendo dalla sequenza completa:

  (0 1 2 3 ... N)

la probabilità che il processo elimini tutti gli elementi prima di fermarsi soddisfa:

   lim P(lista vuota) = 0
  N->Inf

con decadimento super-esponenziale.

In breve, con N sufficientemente grande il processo quasi sempre si arresta molto prima che la lista possa diventare vuota.

I dati ottenuti dalle simulazioni mostrano che:

- per N = 9 la probabilità è circa 2.4e-5
- per N = 20 non si osservano liste vuote nemmeno con 1e6 simulazioni

Questo è coerente con una legge del tipo:

  P(N) ~= exp(-c*N^2)


-------------------
Lavori in parallelo
-------------------

Partiamo da un problema di Erone di Alessandria (I secolo d.c.).

Una vasca si può riempire tramite tre rubinetti.
Il rubinetto A impiega 4 ore per riempirle, il rubinetto B ne impiega 6 e il rubinetto C ne impiega 8.
In quanto tempo si riempirebbe la vasca se i tre rubinetti venissero aperti contemporaneamente?


Calcoliamo il minimo comune multiplo delle ore impiegate dai 3 rubinetti:

  mcm(4 6 8) = 24

Calcoliamo quante vasche riempie ogni rubinetto in 24 ore:

  A = 24/4 = 6 vasche
  B = 24/6 = 4 vasche
  C = 24/8 = 3 vasche

Quindi in 24 ore i tre rubinetti riempiono 6 + 4 + 3 = 13 vasche.

Allora per riempire una vasca i tre rubinetti impiegano 24/13 ore (1 ora 50 minuti 26 secondi).

(div 24 13)
;-> 1.846153846153846

Possiamo ragionare anche in termini di spazio, tempo, velocità:

  4 ore a vasca -> v1 = 1/4 vasche/ora
  6 ore a vasca -> v2 = 1/6 vasche/ora
  8 ore a vasca -> v3 = 1/8 vasche/ora

La velocità di riempimento media è uguale alla quantità di acqua aperta diviso il tempo impiegato.
Indicando con x il numero di vasche (spazio) possiamo scrivere:

  x = v * t --> t = x/v

In questa formula abbiamo:

  v = v1 + v2 + v3 (somma delle tre velocità)
  x = 1 (perchè dobbiamo riempire una vasca)

Quindi il tempo impiegato per riempire una vasca vale:

  t = 1/(v1 + v2 + v3) = 1/(1/4 + 1/6 + 1/8) = 1/(13/24) = 24/13 ore

Scriviamo una funzione che risolve questo tipo di problemi:

(define (parallel workers)
  (div (apply add (map div workers))))

Proviamo:

(parallel '(4 6 8))
;-> 1.846153846153846

(parallel '(1 1 10))
;-> 0.4761904761904762
(parallel '(2 2 8))
;-> 0.8888888888888888
(parallel '(4 4 4))
;-> 1.333333333333333


--------------------------------
Conversione ore, minuti, secondi
--------------------------------

Vediamo alcune funzioni per convertire i tempi da ore, minuti, secondi in ore, minuti, secondi decimali e viceversa.

(define (hh-hhmmss hh)
"Convert hours (decimal) to hours, minutes, seconds"
  (local (hh-int mm mm-int ss)
    (setq hh-int (int hh))
    (setq mm (mul (sub hh hh-int) 60))
    (setq mm-int (int mm))
    (setq ss (int (add (mul (sub mm mm-int) 60) 0.5)))
    (when (= ss 60)
      (setq ss 0)
      (++ mm-int))
    (when (= mm-int 60)
      (setq mm-int 0)
      (++ hh-int))
    (list hh-int mm-int ss)))

(hh-hhmmss 1.846153846153846)
;-> (1 50 46)
(hh-hhmmss 1.621)
;-> (1 37 16)
(hh-hhmmss 1.9997222)
;-> (1 59 59)
(hh-hhmmss 1.9998621)
;-> (2 0 0)
(hh-hhmmss 2.5)
;-> (2 30 0)

(define (mm-hhmmss mm)
"Convert minutes (decimal) to hours, minutes, seconds"
  (local (hh mm-int ss)
    (setq hh 0)
    (setq mm-int (int mm))
    (setq ss (int (add (mul (sub mm mm-int) 60) 0.5)))
    (when (= ss 60)
      (setq ss 0)
      (++ mm-int))
    (while (> mm-int 59)
      (-- mm-int 60)
      (++ hh))
    (list hh mm-int ss)))

(mm-hhmmss 60)
;-> (1 0 0)
(mm-hhmmss 121.6)
;-> (2 1 36)
(mm-hhmmss 59.999)
;-> (1 0 0)

(define (ss-hhmmss ss)
"Convert seconds (decimal) to hours, minutes, seconds"
  (local (hh mm ss-int)
    (setq hh 0)
    (setq mm 0)
    (setq ss-int (int (add ss 0.5)))
    (while (> ss-int 59)
      (-- ss-int 60)
      (++ mm))
    (while (> mm 59)
      (-- mm 60)
      (++ hh))
    (list hh mm ss-int)))

(ss-hhmmss 3599)
;-> (0 59 59)
(ss-hhmmss 7321.6)
;-> (2 2 2)
(ss-hhmmss 3599)
;-> 0 59 59
(ss-hhmmss 7321.6)
;-> (2 2 2)

Funzioni inverse:

(define (hhmmss-hh hh mm ss)
"Convert hours, minutes, seconds (not normalized) to decimal hours"
  (div (add (mul hh 3600)
            (mul mm 60)
            ss)
       3600))

(hhmmss-hh 1 120 0)
;-> 3
(hhmmss-hh 0 59 120)
;-> 1
(hhmmss-hh 1 112 12)
;-> 2.87

(define (hhmmss-mm hh mm ss)
"Convert hours, minutes, seconds (not normalized) to decimal minutes"
  (div (add (mul hh 3600)
            (mul mm 60)
            ss)
       60))

(hhmmss-mm 1 120 0)
;-> 180
(hhmmss-mm 0 1 30)
;-> 1.5

(define (hhmmss-ss hh mm ss)
"Convert hours, minutes, seconds (not normalized) to decimal seconds"
  (add (mul hh 3600)
       (mul mm 60)
       ss))

(hhmmss-ss 0 59 120)
;-> 3660
(hhmmss-ss 1 0 -60)
;-> 3540


----------------------
Vincere con meno punti
----------------------

In alcuni giochi (es. tennis, ping-pong) è possibile vincere facendo meno punti dell'avversario.
Esempio:
Una partita di Tennis in cui vince chi si aggiudica per primo 3 set.
Punteggio per vincere un set: 6
Risultato tra due giocatori A e B:

  A | 6 5 5 6 5
 ---------------
  B | 1 6 6 1 6

Vince il giocatore B per 3 a 2, ma A ha fatto 6+5+5+6+5 = 27 punti, mentre B, pur vincendo il match, ha fatto soltanto 1+6+6+1+6 = 20 punti.

Dato il numero di giochi da aggiudicarsi per vincere un match e il punteggio da raggiungere per vincere un gioco, determinare la differenza massima tra i giochi vinti dal perdente e quelli vinti dal vincitore.

(define (max-diff giochi punti)
  (local (A-win A-lose B-win B-lose)
    (setq A-win (* giochi punti))
    (setq A-lose 0)
    (setq B-win (* (- giochi 1) punti))
    (setq B-lose (* giochi (- punti 1)))
    (println A-win { } (+ B-win B-lose))
    (- (+ B-win B-lose) A-win)))

Proviamo:

(max-diff 3 6)
;-> 18 27
;-> 9
(max-diff 4 11)
;-> 44 73
;-> 29

In alcuni casi per vincere un gioco occorre raggiungere il punteggio stabilito con più di un punto di vantaggio sull'avversario (es. 2 punti di vantaggio: 6-5 non vince, ma 7-5 vince).

(define (max-diff giochi punti gap)
  (local (A-win A-lose B-win B-lose)
    (setq A-win (* giochi punti))
    (setq A-lose 0)
    (setq B-win (* (- giochi 1) punti))
    (setq B-lose (* giochi (- punti gap)))
    (println A-win { } (+ B-win B-lose))
    (- (+ B-win B-lose) A-win)))

(max-diff 3 6 2)
;-> 18 24
;-> 9

(max-diff 4 11 2)
;-> 44 69
;-> 25

(max-diff 3 6 1)
;-> 18 27
;-> 9

(max-diff 4 11 1)
;-> 44 73
;-> 29


-------------
Funzione XAND
-------------

Tavola di verità

 +---+---+-----+      +------+------+------+
 | A | B | Out |      |  A   |  B   | Out  |
 +---+---+-----+      +------+------+------+
 | 0 | 0 |  1  |      | nil  | nil  | true |
 | 0 | 1 |  0  |      | nil  | true | nil  |
 | 1 | 0 |  0  |      | true | nil  | nil  |
 | 1 | 1 |  1  |      | true | true | true |
 +---+---+-----+      +------+------+------+

(define (xand a b)
  (cond ((and (nil? a) (nil? b)) true)
        ((and (nil? a) (true? b)) nil)
        ((and (true? a) (nil? b)) nil)
        ((and (true? a) (true? b)) true)))

(setq A '(nil nil true true))
(setq B '(nil true nil true))
(map xand A B)
;-> (true nil nil true)

Nota: generalizzando per qualunque tipo di parametri 'a' e 'b' possiamo scrivere:
  se (a == b), allora output = true
               altrimenti output = nil

(define (xand a b) (= a b))
(map xand A B)
;-> (true nil nil true)


----------------------------------------
Stampa dell'N-esima riga di una funzione
----------------------------------------

Scrivere una funzione che prende un intero N e una funzione F e stampa la riga N-esima (1-based) della funzione F.

(define (print-line-N N func)
  (let (linee (parse (source 'func) "\r\n"))
    ; toglie gli ultimi due \r\n
    (pop linee -1) (pop linee -1)
    ; stampa la linea N-esima del sorgente di F
    ; (come rappresentato internamente da newlisp)
    ; N deve essere un numero di linea valido
    (if (and (> N 0) (<= N (length linee)))
        (println (linee (- N 1)))
        nil)))

Proviamo:

(define (somma a b)
  (if (> a b)
    (+ a b b)
    ;else
    (+ a a b)))

(print-line-N 2 somma)
;->   (if (> a b)

(print-line-N 2 print-line-N)
;->   (let (linee (parse (source 'func) "\r\n"))


--------------------
Hacker logo (Glider)
--------------------

  +---+---+---+
  |   | * |   |
  +---+---+---+
  |   |   | * |
  +---+---+---+
  | * | * | * |
  +---+---+---+

Il Glider è un simbolo utilizzato per rappresentare la cultura hacker.
Fu proposto nel 2003 da Eric S. Raymond, figura di spicco del movimento open source, per dare agli hacker un'identità condivisa, distinta dai "cracker" (hacker criminali).

Origine e significato
---------------------
Il Gioco della Vita di Conway:
il simbolo è un pattern tratto da LIFE, una simulazione di automa cellulare creata dal matematico John Conway nel 1970.

La "Spaceship" (Navicella Spaziale) più semplice:
il Glider è il pattern più piccolo e riconoscibile che si muove lungo la griglia.

Simbolismo:
rappresenta un comportamento complesso ed emergente che nasce da regole semplici, una metafora di come gli hacker utilizzano le tecnologie di base per creare sistemi sofisticati.

Storia: fu descritto per la prima volta su Scientific American nel 1970, più o meno nello stesso periodo in cui nacquero Unix e Internet.

(define (glider side blank ch)
  (if (< side 5) (setq side 5))
  (if (even? side) (++ side)) ; lenght of cell must be odd
  (setq pos1 (/ side 2))
  (setq pos2 (+ pos1 (- side 1)))
  (setq pos3 (+ pos2 (- side 1)))
  ;(println pos1 { } pos2 { } pos3)
  (setq bordo (string (dup (string "+" (dup "-" (- side 2))) 3) "+"))
  (setq blank-line (string (dup (string "|" (dup " " (- side 2))) 3) "|"))
  ; linea 1
  (setq linea1 (string (dup (string "|" (dup " " (- side 2))) 3) "|"))
  (setf (linea1 pos2) ch)
  (println bordo)
  (if (> blank 0) (for (i 1 blank) (println blank-line)))
  (println linea1)
  (if (> blank 0) (for (i 1 blank) (println blank-line)))
  ; linea 2
  (setq linea2 (string (dup (string "|" (dup " " (- side 2))) 3) "|"))
  (setf (linea2 pos3) ch)
  (println bordo)
  (if (> blank 0) (for (i 1 blank) (println blank-line)))
  (println linea2)
  (if (> blank 0) (for (i 1 blank) (println blank-line)))
  ; linea 3
  (setq linea3 (string (dup (string "|" (dup " " (- side 2))) 3) "|"))
  (setf (linea3 pos1) ch)
  (setf (linea3 pos2) ch)
  (setf (linea3 pos3) ch)
  (println bordo)
  (if (> blank 0) (for (i 1 blank) (println blank-line)))
  (println linea3)
  (if (> blank 0) (for (i 1 blank) (println blank-line)))
  (println bordo)
'> )

Proviamo:

(glider 5 0 "*")
;-> +---+---+---+
;-> |   | * |   |
;-> +---+---+---+
;-> |   |   | * |
;-> +---+---+---+
;-> | * | * | * |
;-> +---+---+---+

(glider 5 1 "*")
;-> +---+---+---+
;-> |   |   |   |
;-> |   | * |   |
;-> |   |   |   |
;-> +---+---+---+
;-> |   |   |   |
;-> |   |   | * |
;-> |   |   |   |
;-> +---+---+---+
;-> |   |   |   |
;-> | * | * | * |
;-> |   |   |   |
;-> +---+---+---+

(glider 9 0 "*")
;-> +-------+-------+-------+
;-> |       |   *   |       |
;-> +-------+-------+-------+
;-> |       |       |   *   |
;-> +-------+-------+-------+
;-> |   *   |   *   |   *   |
;-> +-------+-------+-------+

(glider 9 1 "*")
;-> +-------+-------+-------+
;-> |       |       |       |
;-> |       |   *   |       |
;-> |       |       |       |
;-> +-------+-------+-------+
;-> |       |       |       |
;-> |       |       |   *   |
;-> |       |       |       |
;-> +-------+-------+-------+
;-> |       |       |       |
;-> |   *   |   *   |   *   |
;-> |       |       |       |
;-> +-------+-------+-------+

(glider 12 1 "*")
;-> +-----------+-----------+-----------+
;-> |           |           |           |
;-> |           |     *     |           |
;-> |           |           |           |
;-> +-----------+-----------+-----------+
;-> |           |           |           |
;-> |           |           |     *     |
;-> |           |           |           |
;-> +-----------+-----------+-----------+
;-> |           |           |           |
;-> |     *     |     *     |     *     |
;-> |           |           |           |
;-> +-----------+-----------+-----------+

(glider 12 2 "O")
;-> +-----------+-----------+-----------+
;-> |           |           |           |
;-> |           |           |           |
;-> |           |     O     |           |
;-> |           |           |           |
;-> |           |           |           |
;-> +-----------+-----------+-----------+
;-> |           |           |           |
;-> |           |           |           |
;-> |           |           |     O     |
;-> |           |           |           |
;-> |           |           |           |
;-> +-----------+-----------+-----------+
;-> |           |           |           |
;-> |           |           |           |
;-> |     O     |     O     |     O     |
;-> |           |           |           |
;-> |           |           |           |
;-> +-----------+-----------+-----------+

Nota: i caratteri UTF-8 che sono rappresentati con più di 1 byte non vengono stampati correttamente.

Vedi anche "Game of Life" su "Rosetta Code".


-----------------------------
Liste con la stessa struttura
-----------------------------

Date due liste (anche annidate) determinare se hanno la stessa struttura.
Due liste hanno la stessa struttura se le relative liste di indici degli elementi sono uguali.

La seguente funzione genera la lista degli indici di tutti gli elementi della lista data:

(define (index-list lst)
"Create a list of indexes for all the elements of a list"
  (ref-all nil lst (fn(x) true)))

(setq lst '(1 2 (3 4)))

Lista degli indici:
(setq indici (index-list lst))
;-> ((0) (1) (2) (2 0) (2 1))

Indicizzazione di tutti gli elementi della lista iniziale:
(dolist (idx indici) (print (lst idx) { }))
;-> 1 2 (3 4) 3 4

Un caso particolare si verifica quando una lista di indici ha la stessa struttura della parte iniziale dell'altra lista di indici, cioè quando la lista più corta ha la stessa struttura della parte iniziale della lista più lunga.

Esempi:
  a = (1 (1 (2 3)) (3) (3 4))
  b = (9 (0 (4 2)) (1) (6 7))
  c = (2 (0 (3 4)))
  d = (9 (0 (4 2 3)) (4) (0 9))
  e = (1 2 3 4)
  f = (5 6 7 8 9 10 11)

  a e b hanno la stessa struttura
  c ha la stessa struttura di a (solo per gli elementi di c)
  a e d non hanno la stessa struttura
  e ha la stessa struttura di f (solo per gli elementi di e)

(setq a '(1 (1 (2 3)) (3) (3 4)))
(setq b '(9 (0 (4 2)) (1) (6 7)))
(setq c '(9 (0 (4 2))))
(setq d '(9 (0 (4 2 3)) (4) (0 9)))
(setq e '(1 2 3 4))
(setq f '(5 6 7 8 9 10 11))

(index-list a)
;-> ((0) (1) (1 0) (1 1) (1 1 0) (1 1 1) (2) (2 0) (3) (3 0) (3 1))
(index-list b)
;-> ((0) (1) (1 0) (1 1) (1 1 0) (1 1 1) (2) (2 0) (3) (3 0) (3 1))
(index-list d)
;-> ((0) (1) (1 0) (1 1) (1 1 0) (1 1 1) (1 1 2) (2) (2 0) (3) (3 0) (3 1))
(index-list e)
;-> ((0) (1) (2) (3))
(index-list f)
;-> ((0) (1) (2) (3) (4) (5) (6))

Funzione che verifica se due liste hanno la stessa struttura:
(define (same-structure? lst1 lst2)
  (local (s1 s2 len-s1 len-s2 struttura indici all)
    ; calcola la lista degli indici di tutti gli elementi della lista lst1
    (setq s1 (ref-all nil lst1 (fn(x) true)))
    ; calcola la lista degli indici di tutti gli elementi della lista lst2
    (setq s2 (ref-all nil lst2 (fn(x) true)))
    (setq len-s1 (length s1))
    (setq len-s2 (length s2))
    (cond ((= len-s1 len-s2) ; liste di indici con stessa lunghezza
            (when (= s1 s2)
              (setq struttura true)
              (setq indici s1)
              (setq all true)))
          ((> len-s1 len-s2) ; lunghezza della prima lista di indici maggiore
            (when (= (slice s1 0 len-s2) s2)
              (setq struttura true)
              (setq indici (slice s1 0 len-s2))))
          ((< len-s1 len-s2) ; lunghezza della seconda lista di indici maggiore
            (when (= (slice s2 0 len-s1) s1)
              (setq struttura true)
              (setq indici (slice s2 0 len-s1))))
    )
    ; struttura: true --> liste con la stessa struttura
    ; indici: true --> lista degli indici uguali
    ; all: true --> la lista degli indici uguali comprende tutti gli elementi
    (list struttura indici all)))

Proviamo:

(same-structure? a b)
;-> (true ((0) (1) (1 0) (1 1) (1 1 0) (1 1 1) (2) (2 0) (3) (3 0) (3 1)) true)
(same-structure? a c)
;-> (true ((0) (1) (1 0) (1 1) (1 1 0) (1 1 1)) nil)
(same-structure? a d)
;-> (nil nil nil)
(same-structure? a e)
;-> (nil nil nil)
(same-structure? e f)
;-> (true ((0) (1) (2) (3)) nil)


---------------------------------------------------
Massima differenza tra coppie di elementi adiacenti
---------------------------------------------------

Data una lista L di numeri interi, scrivere una funzione che determina la variazione massima (in valore assoluto) tra tutte le coppie di numeri adiacenti.
Esempio:
  L = (1 3 2 6 5)
  Variazioni tra coppie adiacenti di numeri adiacenti:
  (1 3) --> 2
  (3 2) --> 1
  (2 6) --> 4
  (6 5) --> 1
  Variazione massima: (2 6) --> 4

(define (delta lst)
  (local (diff max-diff indice)
    ; lista delle differenze assolute di ogni coppia di numeri adiacenti
    (setq diff (map (fn(x y) (abs (- x y))) (rest lst) (chop lst)))
    ;(println diff)
    ; calcolo della variazione (differenza) massima
    (setq max-diff (apply max diff))
    ; indice della variazione
    (setq indice (find max-diff diff))
    ; restituisce (variazione-massima (primo-numero secondo-numero))
    (list max-diff (list (lst indice) (lst (+ indice 1))))))

Proviamo:

(setq L '(1 3 2 6 5))
(delta L)
;-> (4 (2 6))

(setq L '(7 9 4 1 6 5))
(delta L)
;-> (5 (9 4))

(setq L '(1 2 3 4 5 7))
(delta L)
;-> (2 (5 7))


-------------------------------
John D. Cook's exponential sums
-------------------------------

https://www.johndcook.com/
https://www.johndcook.com/expsum/
https://www.johndcook.com/expsum/details.html

La pagina web di John D. Cook include una sezione che disegna la "somma esponenziale del giorno".
Questa è definita come segue:

  Sum[n=0,N]exp(2πi(n/m + n^2/d + n^3/y))
  dove i è l'unità immaginaria e N è calcolato come 2*mcm(m,d,y)+1, dove mcm indica il minimo comune multiplo.

Equazione di eulero per il calcolo dell'esponenziale complesso:

  e^(a+ib) = e^a * (cos(b) + i sin(b))

(define (lcm_ a b) (/ (* a b) (gcd a b)))
(define-macro (lcm)
"Calculate the lcm of two or more number"
  (apply lcm_ (map eval (args)) 2))

(define (cook dd mm yy)
  (letn ((out '())
         (pi 3.1415926535897931)
         ; calcola 2*mcm(m,d,y) per avere 2*lcm+1 campioni con (for (n 0 N).
         (N (* 2 (lcm dd mm yy))))
    (for (n 0 N)
      (setq f (add (div n mm) (div (mul n n) dd) (div (mul n n n) yy)))
      ; Riduzione a modulo 1 ((sub f (floor f)) prima di moltiplicare per 2π:
      ; poichè per n grandi l'argomento può diventare enorme
      ; e cos/sin perdono precisione.
      (setq power (mul 2 pi (sub f (floor f))))
      (setq re (cos power))
      (setq im (sin power))
      (push (list re im) out))
    out))

(cook 1 2 3)
;-> ((1 0)
;->  (0.4999999999995876 0.8660254037846767)
;->  (-0.4999999999998968 0.8660254037844982)
;->  (-1 1.224606353822377e-016)
;->  (-0.500000000000052 -0.8660254037844086)
;->  (0.499999999999897 -0.8660254037844981)
;->  (1 0)
;->  (0.5000000000000516 0.8660254037844088)
;->  (-0.499999999999974 0.8660254037844536)
;->  (-1 1.224606353822377e-016)
;->  (-0.5000000000000034 -0.8660254037844366)
;->  (0.4999999999999993 -0.866025403784439)
;->  (1 0))

Nota: la lista di punti può essere esportata e plottata con un foglio elettronico.


------------------
Il Colpo di Venere
------------------

"Siccome mi sembrava che per puro caso alcuni fatti fossero avvenuti così com'erano stati predetti dagl'indovini, tu hai parlato a lungo del caso, e hai detto, per esempio, che si può ottenere il 'colpo di Venere' lanciando a caso quattro dadi [...]".
Cicerone, "De Divinatione",II,21,48

Nel dialogo col fratello Quinto, Cicerone parla del Colpo di Venere che consiste nel lanciare quattro dadi tetraedrici ottenendo quattro risultati diversi.

Un dado tetraedrico è una piramide con base triangolare e quindi ha quattro facce.

Supponendo che le facce di ciascun dato siano equiprobabili, determinare:
A) la probabilità di ottenere il Colpo di Venere nel lancio di quattro dati,
B) la probabilità di ottenere quattro numeri tutti uguali.

Punto A
-------

Simulazione:

(define (simula-A iter)
  (let ( (ok 0) (lancio nil) )
    (for (i 1 iter)
      (setq lancio (rand 4 4))
      ; controlla se tutti i numeri del lancio sono diversi
      (if (= (sort lancio) '(0 1 2 3)) (++ ok)))
    (div ok iter)))

(simula-A 1e4)
;-> 0.094
(simula-A 1e7)
;-> 0.0937672

Calcolo delle probabilità:

Per ottenere quattro numeri diversi (colpo di Venere) deve risultare:
Per il primo dado qualunque numero va bene, cioè abbiamo 4 possibilità su 4 di scegliere un numero corretto (4/4 = 1).
Per il secondo dado abbiamo 3 possibilità su 4 di scegliere un numero corretto (3/4).
Per il terzo dado abbiamo 2 possibilità su 4 di scegliere un numero corretto (2/4 = 1/2).
Per il quarto e ultimo dado abbiamo 1 possibilità su 4 di scegliere il numero corretto (1/4).
Quindi la probabilità di ottenere il Colpo di Venere vale:

  P(venere) = 1 * 3/4 * 1/2 * 1/4 = 3/32 = 0.09375 (circa il 9.4%)

(mul 1 (div 3 4) (div 1 2) (div 1 4))
;-> 0.09375

Nota:
Il settimo degli otto quesiti di matematica della maturità 2025 per i licei scientifici parte proprio dalla citazione di Cicerone e chiede di determinare la probabilità del Colpo di Venere.

Punto B
-------

Simulazione:

(define (simula-B iter)
  (let ( (ok 0) (lancio nil) )
    (for (i 1 iter)
      (setq lancio (rand 4 4))
      ; controlla se tutti i numeri sono uguali
      (if (apply = lancio) (++ ok)))
    (div ok iter)))

(simula-B 1e4)
;-> 0.017

(simula-B 1e7)
;-> 0.0155937

Calcolo delle probabilità:

Per ottenere quattro numeri uguali deve risultare:
Per il primo dado qualunque numero va bene, cioè abbiamo 4 possibilità su 4 di scegliere un numero corretto (4/4 = 1).
Per il secondo dado abbiamo 1 possibilità su 4 di scegliere il numero corretto (1/4).
Per il terzo dado abbiamo 1 possibilità su 4 di scegliere il numero corretto (1/4).
Per il quarto dado abbiamo 1 possibilità su 4 di scegliere il numero corretto (1/4).
Quindi la probabilità di ottenere quattro numeri uguali vale:

  P(tutti uguali) = 1 * 1/4 * 1/4 * 1/4 = 1/64 = 0.015625 (circa 1.6%)

(mul 1 (div 1 4) (div 1 4) (div 1 4))
;-> 0.015625

Nota:
In alcuni casi, probabilmente non in questo che è banale, è utile utilizzare la "funzione generatrice".
E' un metodo assai utile quando si hanno dadi con numero facce diverse e probabilità differenziate da faccia a faccia (in questi casi i conti diventano complicati).
Scrivo la funzione che caratterizza un dado a 4 facce equiprobabili sostituendo idealmente i numeri 1:4 con le lettere a, b, c, d, assegnando ad ogni lettera la singola probabilità di uscita (nel nostro caso equiprobabile è sempre 1/4):

  f(a,b,c,d) = 1/4a+1/4b+1/4c+1/4d

Siccome i dadi lanciati sono 4 (oppure è un dado lanciato 4 volte), elevo il polinomio alla quarta potenza.
Il coefficiente del termine: "abcd" ci dà la probabilità delle 4 facce diverse.
Il coefficiente di: "a^4, b^4, c^4, d^4" ci restituisce la probabilità di 4a, 4b, 4c, 4d.
Se si sviluppa (1/4a+1/4b+1/4c+1/4d)^4 si ha:
3/32abcd (3/32 probabilità di 4 facce diverse)
a^4/256, b^4/256, c^4/256, d^4/256.
Per esempio, nello sviluppo si ha 3/64ab²d: significa che la configurazione del tipo: 1-2-2-4 ha probabilità 3/64.


-----------------------
Sempre lo stesso numero
-----------------------

Scegliere un numero di 3 cifre tale che:
- il valore assoluto della differenza della prima e ultima cifra non deve essere inferiore a 2
- invertire le cifre
- sottrarre i 2 numeri (il più grande meno il più piccolo)
- somma il numero ottenuto dalla sottrazione precedente con lo stesso numero con le cifre invertite
- somma i 2 numeri ottenuti
Il numero che si ottiene vale sempre 1089.

(define (trasforma n)
  (setq s (string n))
  (cond ((!= (length n) 3) nil)
        ((< (abs (- (int (s 0) 0 10) (int (s 2) 0 10))) 2) nil)
        (true
          (setq s1 (reverse (copy s)))
          (setq n1 (abs (- (int s 0 10) (int s1 0 10))))
          (setq n2 (int (reverse (string n1)) 0 10))
          ;(println s { } s1 { } n1 { } n2)
          (+ n1 n2))))

Proviamo:

(trasforma 267)
;-> 1089
(trasforma 162)
;-> nil
(trasforma 16)
;-> nil
(trasforma 341)
;-> 1089

Verifichiamo che 1089 sia l'unico risultato:
(sort (unique (map trasforma (sequence 100 999))))
;-> (nil 1089)

Spiegazione matematica:

Indichiamo il numero di tre cifre come:
  abc = 100a + 10b + c
con a != 0 e |a − c| >= 2.
Invertendo le cifre otteniamo
  cba = 100c + 10b + a

1) Sottrazione
Senza perdita di generalità supponiamo a > c (altrimenti scambiamo i due numeri).
Calcoliamo:
  abc − cba
  = (100a + 10b + c) − (100c + 10b + a)
  = 100a − 100c + c − a
  = 99a − 99c
  = 99(a − c)
Quindi la differenza è sempre un multiplo di 99.
Pongo d = a − c con d >= 2.
Allora:
99d

2) Struttura di 99d
Per d = 2,...,9 si ottiene sempre un numero del tipo:
  198, 297, 396, 495, 594, 693, 792, 891
Osservazione chiave: le cifre esterne sommano sempre 9 e la cifra centrale è 9.
Infatti:
  99d = 100d − d = (d−1) 9 (10−d)
esempio:
  d = 4 -> 396 -> 3 + 6 = 9

3) Somma col numero invertito
Sia il risultato:
  xyz
con x + z = 9 e y = 9.

Il numero invertito è:
  zyx
Sommiamo:
  xyz + zyx
  = (100x + 90 + z) + (100z + 90 + x)
  = 101x + 101z + 180
  = 101(x+z) + 180
  = 101·9 + 180
  = 909 + 180
  = 1089

4) Conclusione
Tutto dipende dal fatto che:
- la sottrazione produce sempre 99(a−c)
- quel numero ha forma (x, 9, 9−x)
- la somma con l'inverso diventa costante
Perciò il risultato finale è sempre 1089 indipendentemente dal numero iniziale (purché |a−c| >= 2).

Adesso togliamo due vincoli:
- il valore assoluto della differenza tra la prima e l'ultima cifra può avere un valore qualsiasi
- il numero può avere qualunque numero di cifre

(define (trasforma2 n)
  (setq s (string n))
  (setq s1 (reverse (copy s)))
  (setq n1 (abs (- (int s 0 10) (int s1 0 10))))
  (setq n2 (int (reverse (string n1)) 0 10))
  ;(println s { } s1 { } n1 { } n2)
  (+ n1 n2))

Proviamo:

(sort (unique (map trasforma2 (sequence 1 9))))
;-> (0)
(sort (unique (map trasforma2 (sequence 10 99))))
;-> (0 18 99)
(sort (unique (map trasforma2 (sequence 100 999))))
;-> (0 198 1089)
(sort (unique (map trasforma2 (sequence 1000 9999))))
;-> (0 99 261 342 423 504 585 666 747 828 1170 1251 1332
;->  1413 1494 1575 1656 1737 1818 1998 9999 10890 10989)
(sort (unique (map trasforma2 (sequence 1 10000))))
;-> (0 18 99 198 261 342 423 504 585 666 747 828 1089 1170 1251 1332
;->  1413 1494 1575 1656 1737 1818 1998 9999 10890 10989 19998)

Il caso in cui n ha esattamente 3 cifre da 100 a 999) genera i seguenti risultati:
a) 0, quando la differenza assoluta tra la prima e la terza cifra di n vale 0 (cioè le due cifre sono uguali)
b) 198, quando la differenza assoluta tra la prima e la terza cifra di n vale 1
c) 1089, quando la differenza assoluta tra la prima e la terza cifra è maggiore o uguale a 2

(sort (unique (map trasforma2 (sequence 100 999))))
;-> (0 198 1089)


---------------------
Piramide di bicchieri
---------------------

Supponiamo di avere alcuni bicchieri impilati in una piramide di N righe:
la prima fila contiene 1 bicchiere, la seconda fila 2 bicchieri e così via fino alla N-esima fila.
Ogni bicchiere è in grado di contenere 1 litro.

Poi, cominciamo a versare acqua nel primo bicchiere in cima.
Quando il bicchiere più in alto è pieno, il liquido in eccesso versato cadrà in parti uguali nel bicchiere immediatamente a sinistra e a destra di esso nella riga sottostante.
Quando questi bicchieri si riempiono, l'acqua in eccesso cadrà in parti uguali a sinistra e a destra di quei bicchieri nella riga sottostante, e così via.
I bicchieri nell'ultima fila inferiore vedono l'acqua in eccesso cadere sul pavimento.

Esempio:
dopo aver versato un  litro di acqua, il bicchiere più in alto è pieno.
Dopo aver versato due litri di acqua, i due bicchieri della seconda fila sono pieni a metà.
Dopo aver versato tre litri di acqua, questi due bicchieri si riempiono completamente: ora ci sono 3 bicchieri pieni in totale.
Dopo aver versato quattro litri di aqua, la terza fila ha il bicchiere centrale mezzo pieno e i due bicchieri esterni pieni per un quarto.

Scrivere una funzione che prende il numero di file di bicchieri e la quantità d'acqua versata e restituisce la situazione (quantità di acqua) di tutti i bicchieri nella piramide

Disposizione dei bicchieri a piramide:

riga 0         U
riga 1        U U
riga 2       U U U
riga 3     U U U U U
riga 4    U U U U U U
riga 5  U U U U U U U U
...

Flusso dell'acqua:
                      ..
                      ..
                   ........
                   .|....|.
riga 0             .|....|.
                   .|....|.
                   .+----+.
              ........  ........
              .|....|.  .|....|.
riga 1        .|....|.  .|....|.
              .|....|.  .|....|.
              .+----+.  .+----+.
              .      .  .      .
           |  . |   |.  .|   | .  |
riga 2     |  . |   |....|   | .  |
           |....|   |....|   |....|
           +----+   +----+   +----+
...

Funzione che stampa la piramide dei bicchieri:

(define (show-piramide lst)
  (dolist (row lst)
    (dolist (el row)
      (if (!= el 0) (print el { })))
    (println)))

Funzione che calcola l'acqua nei bicchieri:

(define (piramide N qt)
  ; N:  numero di righe della piramide
  ; qt: litri di acqua versati nei bicchieri della piramide
  (local (pira eccesso acqua pavimento)
    ; Creazione di una matrice che rappresenta la piramide di bicchieri
    ; pira[i][j] contiene la quantità di acqua
    ; del j-esimo bicchiere della i-esima riga
    ; l'ultima fila della matrice rappresenta l'acqua che cade nel pavimento
    (setq pira (array (+ N 1) (+ N 1) '(0)))
    ; Versiamo tutta l'acqua nel bicchiere in cima
    (setf (pira 0 0) qt)
    ; Processiamo ogni riga dall'alto verso il basso...
    (for (riga 0 (- N 1))
      ; processiamo ogni bicchiere nella riga corrente
      ; (la riga i ha i+1 bicchieri)
      (for (bicchiere 0 riga)
        ; Se il bicchiere attuale contiene più di 1 litro
        (when (> (pira riga bicchiere) 1)
          ; Calcola la quantità di acqua che trabocca nei bicchieri sottostanti
          (setq eccesso (div (sub (pira riga bicchiere) 1) 2))
            ; Il bicchiere attuale può contenere al massimo 1 litro
          (setf (pira riga bicchiere) 1)
          ; Divide equamente l'acqua in eccesso nei due bicchieri sottostanti
          ; bicchiere sotto a sinistra
          (inc (pira (+ riga 1) bicchiere) eccesso)
          ; bicchiere sotto a destra
          (inc (pira (+ riga 1) (+ bicchiere 1)) eccesso))))
    ; Calcola l'acqua totale che si trova nei bicchieri
    (setq acqua (apply add (flat (slice (array-list pira) 0 N))))
    ; Calcola l'acqua caduta nel pavimento
    (setq pavimento (apply add (flat (last (array-list pira) 0 N))))
    ; Deve risultare che l'acqua versata (qt) è uguale alla somma
    ; dell'acqua nei bicchieri e dell'acqua nel pavimento
    (if (!= qt (add pavimento acqua)) (print "ERRORE: "))
    (println acqua { } pavimento)
    ; stampa la matrice che rappresenta la piramide dei bicchieri
    (show-piramide (slice (array-list pira) 0 N)) '>))

Proviamo:

(piramide 2 2)
;-> 2 0
;-> 1
;-> 0.5 0.5

(piramide 2 3)
;-> 3 0
;-> 1
;-> 1 1

(piramide 3 5)
;-> 5 0
;-> 1
;-> 1 1
;-> 0.5 1 0.5

(piramide 3 10)
;-> 6 4
;-> 1
;-> 1 1
;-> 1 1 1

(piramide 4 10)
;-> 8.75 1.25
;-> 1
;-> 1 1
;-> 1 1 1
;-> 0.375 1 1 0.375

(piramide 10 100)
;-> 45.921875 54.078125
;-> 1
;-> 1 1
;-> 1 1 1
;-> 1 1 1 1
;-> 1 1 1 1 1
;-> 1 1 1 1 1 1
;-> 0.578125 1 1 1 1 1 0.578125
;-> 1 1 1 1 1 1
;-> 0.8828125 1 1 1 1 1 0.8828125
;-> 1 1 1 1 1 1


--------------
Velocità media
--------------

Problema 1
----------
Una moto percorre un percorso da A a B a 70 km/h.
Poi ritorna da B ad A alla velocità di 80 km/h.
Calcolare la velocità media complessiva.

Problema 2
----------
Una moto percorre un percorso da A a B a 70 km/h.
Poi compie metà percorso di ritorno a 60 km/h e l'altra metà a 80 km/h.
Calcolare la velocità media complessiva.

Soluzione 1
-----------
Sia s la distanza tra A e B.

v1 = s/t1  --> t1 = s/v1  (1)
v2 = s/t2  --> t2 = s/v2  (2)

Velocità media totale:

  V = (s + s) / (t1 + t2)

Sostituiamo t1 e t2 con le formule (1) e (2):

            2s                 2s                (v1*v2)
  V = ---------------  = --------------- = 2 * -----------
       (s/v1 + s/v2)      (v1*s + v2*s)         (v1 + v2)
                          -------------
                              v1*v2

Quindi risulta che la velocità media vale:

            2s
  V = ---------------
       (s/v1 + s/v2)

(define (vel-media v1 v2) (div (mul 2 v1 v2) (add v1 v2)))
(vel-media 70 80)
;-> 74.66666666666667

Soluzione 2
-----------
Sia s la distanza tra A e B.

Andata:
velocita v1
tempo
  t1 = s / v1

Ritorno:
meta distanza a v2
  t2 = (s/2) / v2 = s/(2*v2)

altra meta a v3
  t3 = (s/2) / v3 = s/(2*v3)

Tempo totale:
  t = t1 + t2 + t3
  = s/v1 + s/(2*v2) + s/(2*v3)

Distanza totale: 2s

Velocita media:
  vm = distanza totale / tempo totale
  vm = 2s / (s/v1 + s/(2*v2) + s/(2*v3))

Semplificando s:
  vm = 2 / (1/v1 + 1/(2*v2) + 1/(2*v3))

oppure moltiplicando numeratore e denominatore per 2:
  vm = 4 / (2/v1 + 1/v2 + 1/v3)
Questa e la formula generale.

Con:
  v1 = 70
  v2 = 60
  v3 = 80

  vm = 4 / (2/70 + 1/60 + 1/80) = 6720/97 ~ 69.28 km/h

Nota:
Nel caso di velocità su più tratte (n) della stessa lunghezza per calcolare la velocità media si utilizza la formula per la media armonica:

                      n
  V = ---------------------------------
       1/v1 + 1/v2 + /1v3 + ... + 1/vn

(define (velocita-media lst)
  (let (sum-rec 0)
    (dolist (x lst)
      (setq sum-rec (add sum-rec (div x))))
    (div (length lst) sum-rec)))

(velocita-media '(70 80))
;-> 74.6666666666666

(velocita-media '(70 60 80))
;-> 69.04109589041096


-------------------
Spinta di Archimede
-------------------

Una zattera di legno quadrata con lato L (4m) e altezza H (0.5 m) galleggia sull'acqua.
Il peso specifico del legno vale PS (0.8 g/cm^3 = 800 kg/m^3)
Qual'è l'altezza della zattera immersa nell'acqua?

Mettiamo sopra la zattera un carico di peso N (400 kg).
Qual'è l'altezza della zattera immersa nell'acqua?

Quale peso possiamo caricare per fare in modo che tutta la zattera sia sommersa?

Principio di Archimede
Immergendo un corpo nell'acqua, questo riceve una spinta verso l'alto con una forza uguale al peso dell'acqua che è stata spostata.

  Peso del corpo = Peso dell'acqua spostata

Calcoliamo la massa (m) partendo dalla formula della densità (d):

  d = m/V , dove V è il volume del corpo
  m = d * V

  Peso del corpo = Volume della zattera * densità della zattera =
  = L * L * H * PS = 4 * 4 * 0.5 * 800 = 6400 kg

  Peso dell'acqua spostata = Volume immerso * densità dell'acqua =
  = Volume immerso * 1 = Volume immerso

Quindi risulta:

  L * L * H * PS = 4 * 4 * 0.5 * 800 = 6400 = Volume immerso

Quanto volume occupa 6400 kg di acqua?

La densità dell'acqua (d1) vale 1000 kg/m^3.

  Volume immerso = m/d1 = 6400/1000 = 6.4 m^3

Adesso possiamo calcolare l'altezza della parte immersa della zattera:

  Volume immerso = Area di base * altezza

  altezza = Volume immerso / Area di base = 6.4 / 16 = 0.4 m = 40 cm

Nel caso in cui aggiungiamo il carico di 400 kg otteniamo:

  altezza = Volume immerso / Area di base = 6.8 / 16 = 0.425 m = 42.5 cm

Per far si che la zattera sia completamente sommersa:

  Volume immerso = L * L * H = 8 m^3
  Acqua spostata = Volume immerso * densità acqua = 8 * 1000 = 8000 m^3

Quindi dobbiamo caricare (8000 - 6400) = 1600 kg sulla zattera per fare in modo che venga completamente sommersa dall'acqua.

In modo più formale:

Una zattera di legno quadrata con lato L e altezza H galleggia sull'acqua.
Il peso specifico del legno vale PS1.
Il peso specifico dell'acqua vale PS2.
Qual'è l'altezza della zattera immersa nell'acqua? Quale peso dobbiamo caricare per fare in modo che tutta la zattera sia sommersa (cioè la parte sommersa della zattera vale H)?

Consideriamo il principio di Archimede: un corpo che galleggia sposta un volume di acqua il cui peso e uguale al peso totale del corpo.

Dati:
  L = lato della base quadrata
  H = altezza della zattera
  PS1 = peso specifico del legno
  PS2 = peso specifico dell acqua

Volume totale zattera:
  V = L^2 * H

1) Altezza immersa senza carico

Sia h l altezza immersa.

Volume immerso:
  Vimm = L^2 * h

  Spinta di Archimede = PS2 * Vimm = PS2 * L^2 * h
  Peso zattera = PS1 * V = PS1 * L^2 * H

  Equilibrio:
  PS2 * L^2 * h = PS1 * L^2 * H

  Semplificando L^2:

  PS2 * h = PS1 * H

  h = (PS1 / PS2) * H

2) Peso da aggiungere per sommergerla tutta

Se è tutta immersa:
  Volume immerso massimo = L^2 * H

Spinta massima:
  Fmax = PS2 * L^2 * H

Peso zattera:
  Plegno = PS1 * L^2 * H

Peso aggiuntivo necessario:
  Pextra = Fmax - Plegno

  Pextra = (PS2 - PS1) * L^2 * H

Quindi le formule finali sono le seguenti:

Altezza immersa:
  h = (PS1/PS2) * H

Peso da caricare per immersione completa:
  Pextra = (PS2 - PS1) * L^2 * H

Adesso scriviamo una funzione che prende L, H, PS1, PS2 e P il peso del carico, e calcola l'altezza della parte immersa.

Basta applicare ancora il principio di Archimede includendo anche il carico.

Peso totale:
Ptot = PS1 * L^2 * H + P

Spinta:
PS2 * L^2 * h

Equilibrio:
PS2 * L^2 * h = Ptot

Quindi:
h = (PS1 * L^2 * H + P) / (PS2 * L^2)

Se h > H la zattera e completamente sommersa, quindi affonda.

(define (altezza-immersa L H PS1 PS2 P)
  (letn (base volume peso-legno peso-tot h)
    (setq base (mul L L))
    (setq volume (mul base H))
    (setq peso-legno (mul PS1 volume))
    (setq peso-tot (add peso-legno P))
    (setq h (div peso-tot (mul PS2 base)))
    (cond ((= h H) (println "sommersa") h)
          ((> h H) (println "affondata") h)
          (true h))))

Proviamo:

(altezza-immersa 4 0.5 800 1000 0)
;-> 0.4

(altezza-immersa 4 0.5 800 1000 400)
;-> 0.425

(altezza-immersa 4 0.5 800 1000 1600)
;-> sommersa
;-> 0.5

(altezza-immersa 4 0.5 800 1000 2000)
;-> affondata
;-> 0.525

Nota: per correttezza, quando h > H dovremmo considerare anche la forma e la densità del carico (per tenere conto del volume della parte del carico sommersa dall'acqua).


---------------------------------
Quadrati unitari in caduta libera
---------------------------------

Ci sono diversi quadrati unitari (di lato 1) che vengono rilasciati dall'alto verso sull'asse X di un piano 2D.
I quadrati sono rappresentati con una lista Q di valori interi che rappresentano le coordinate dell'asse X.
Ogni quadrato viene rilasciato uno alla volta allineato alla sua coordinata X e da un'altezza superiore a qualsiasi quadrato già atterrato.
Quindi cade verso il basso (direzione Y negativa) finché non atterra sul lato superiore di un altro quadrato o sull'asse X.
Una volta atterrato, ogni quadrato si blocca in posizione e non può essere spostato.
Dopo aver rilasciato tutti i quadrati stampare la situazione finale come istogramma.

Esempio:
  lista quadrati (coordinate x) = (1 4 3 3 5 3 5 3 2 1)
    Alla coordinata 0 non ci sono quadrati
    Alla coordinata 1 ci sono 2 quadrat1
    Alla coordinata 2 c'è 1 quadrato
    Alla coordinata 3 ci sono 4 quadrat1
    Alla coordinata 4 c'è 1 quadrato
  Istogramma:
    . . . * . .
    . . . * . .
    . * . * . *
    . * * * * *

Data una lista con numeri interi, stampare il suo istogramma.
Esempio:
  lista = (0 2 1 4 1 2)
  istogramma =
    . . . * . .
    . . . * . .
    . * . * . *
    . * * * * *
    -----------

Per costruire l'istogramma contiamo prima le frequenze e poi stampiamo riga per riga dall'alto verso il basso:
- trova il valore massimo 'max-val'
- per ogni livello da 'max-val' a 1
- stampa "*" se il valore >= livello, altrimenti "."
- infine stampa la linea di base

Funzione che genera un semplice istogramma per una lista di interi:

(define (istogramma lst ch1 ch2)
  (let ( (max-val (apply max lst)) (len (length lst)) )
    (setq ch1 (or ch1 "*"))
    (setq ch2 (or ch2 "."))
    (for (livello max-val 1 -1)
      (for (i 0 (- len 1))
        (if (>= (lst i) livello)
            (print ch1 " ")
            (print ch2 " ")))
      (println))
    (println (dup "-" (- (* 2 len) 1))) '>))

Proviamo:

(istogramma '(0 1 0 3 4 2 5))
;-> . . . . . . *
;-> . . . . * . *
;-> . . . * * . *
;-> . . . * * * *
;-> . * . * * * *
;-> -------------

(istogramma '(0 1 0 3 4 2 5) "■" " ")
;->             ■
;->         ■   ■
;->       ■ ■   ■
;->       ■ ■ ■ ■
;->   ■   ■ ■ ■ ■
;-> -------------

(istogramma '(0 1 0 3 4 2 5) "█")
;-> . . . . . . █
;-> . . . . █ . █
;-> . . . █ █ . █
;-> . . . █ █ █ █
;-> . █ . █ █ █ █
;-> -------------

Funzione che genera la situazione finale della caduta di quadrati unitari:

(define (fall Q ch1 ch2)
  ; massimo valore della coordinata x
  (setq x-max (apply max Q))
  ; vettore che rappresernta l'asse X
  (setq asse-x (array (+ x-max 1) '(0)))
  ; ciclo per ogni quadrato...
  (dolist (quad Q)
    ; aggiunge un quadrato alla relativa coordinata dell'asse X
    ; (aggiorna l'altezza della coordinata x)
    (++ (asse-x quad)))
  ; stampa la situazione finale
  (println "Asse X: " asse-x)
  (istogramma asse-x ch1 ch2))

Proviamo:

(fall '(1 4 3 3 5 3 5 3 2 1))
;-> Asse X: (0 2 1 4 1 2)
;-> . . . * . .
;-> . . . * . .
;-> . * . * . *
;-> . * * * * *
;-> -----------

(fall (rand 20 100) "█")
;-> Asse X: (3 5 5 6 4 5 3 5 3 7 6 5 3 8 7 4 8 3 5 5)
;-> . . . . . . . . . . . . . █ . . █ . . .
;-> . . . . . . . . . █ . . . █ █ . █ . . .
;-> . . . █ . . . . . █ █ . . █ █ . █ . . .
;-> . █ █ █ . █ . █ . █ █ █ . █ █ . █ . █ █
;-> . █ █ █ █ █ . █ . █ █ █ . █ █ █ █ . █ █
;-> █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
;-> █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
;-> █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
;-> ---------------------------------------

(fall (rand 10 100) "█")
;-> Asse X: (11 5 10 10 11 8 12 12 10 11)
;-> . . . . . . █ █ . .
;-> █ . . . █ . █ █ . █
;-> █ . █ █ █ . █ █ █ █
;-> █ . █ █ █ . █ █ █ █
;-> █ . █ █ █ █ █ █ █ █
;-> █ . █ █ █ █ █ █ █ █
;-> █ . █ █ █ █ █ █ █ █
;-> █ █ █ █ █ █ █ █ █ █
;-> █ █ █ █ █ █ █ █ █ █
;-> █ █ █ █ █ █ █ █ █ █
;-> █ █ █ █ █ █ █ █ █ █
;-> █ █ █ █ █ █ █ █ █ █
;-> -------------------


----------------------
Parsing di quaternioni
----------------------

https://codegolf.stackexchange.com/questions/76545/parse-a-quaternion

Un quaternione è fondamentalmente un numero composto da 4 parti (con una componente reale e tre componenti immaginarie.
Le componenti immaginarie sono rappresentate dai suffissi i, j, k.
Ad esempio, 1-2i+3j-4k è un quaternione con 1 come componente reale e -2, 3 e -4 come componenti immaginarie.

Data una stringa di un quaternione (es. "1+2i-3j-4k") trasfornarla in una lista di coefficienti (es. (1 2 -3 -4)). 

Tuttavia, la stringa dei quaternioni può essere formattata in molti modi diversi:
1) Potrebbe essere normale: 1+2i-3j-4k
2) Potrebbe avere termini mancanti: 1-3k, 2i-4k (se mancano termini, restituire 0 per quei termini)
3) Potrebbe avere coefficienti mancanti: i+j-k (in questo caso, ciò equivale a 1i+1j-1k. In altre parole, una i, j o k senza un numero davanti si presume abbia un 1 davanti per impostazione predefinita)
4) Potrebbe non essere nell'ordine corretto: 2i-1+3k-4j
5) I coefficienti possono essere semplicemente numeri interi o decimali: 7-2,4i+3,75j-4,0k
6) Potrebbero esseci degli spazi nella stringa: 1 +2i -3j -4 k
Ci sono alcune cose da notare durante l'analisi:

La stringa di input è sempre ben formata ed ha le seguenti caratteristiche:
a) Ci sarà sempre un + o un - tra i termini
b) contiene almeno 1 termine e senza lettere ripetute (nessuna j-j)
c) i numeri contenuti possono essere interi o float

Algoritmo
---------
1) Eliminazione degli spazi dalla stringa
2) Inserisce gli eventuali +1 o -1 alle parti immaginarie
3) Per ogni componente immaginaria "im":
   3.1) Cerca la posizione di "im"
        3.1.1) se non trovata --> "im" = 0
        3.1.2) se trovata
             3.1.2.1) cerca il numero associato alla "im" (get-num s idx)
                      (partendo dalla posizione corrente a sinistra
                       fino a "+" o "-")
             3.1.2.2) "im" = numero
             3.1.2.3) elimina la stringa numero e "im" dalla stringa
4) La stringa data adesso contiene solo la parte reale (oppure "").

Funzione che prende una stringa e un indice e ritorna la stringa dall'indice precedente a quello dato fino ad un "+" o un "-" a sinistra:

(define (get-num s idx)
  (local (num i found)
    (setq num "")
    (setq i (- idx 1))
    (setq found nil)
    (until found
      (if (or (= (s i) "+") (= (s i) "-")) (setq found true))
      (push (s i) num)
      (-- i))
    num))

Funzione che effetua il parsing di un quaternione in forma di stringa:

(define (quat s)
  ; lista risultato
  (setq res '(0 0 0 0))
  ; elimina gli spazi
  (replace " " s "")
  ; aggiunge un (eventuale) "+" all'inizio
  (if (and (!= (s 0) "-") (!= (s 0) "+")) (push "+" s))
  ; inserisce gli eventuali valori a 1 delle parti immaginarie
  (replace "+i" s "+1i") (replace "-i" s "-1i")
  (replace "+j" s "+1j") (replace "-j" s "-1j")
  (replace "+k" s "+1k") (replace "-k" s "-1k")
  ; ciclo per ogni parte immaginaria...
  (dolist (im '("i" "j" "k"))
    ; cerca l'indice della parte immaginaria corrente nella stringa
    (setq idx (find im s))
    ;(println im)
    (cond
      ; parte immaginaria non trovata --> 0
      ((nil? idx) (setf (res (+ $idx 1)) 0))
      ; parte immaginaria trovata
      (true
        ; trova il numero associato (a sinistra) alla parte immaginaria
        (setq num (get-num s idx))
        ;(println num)
        ; aggiorna la lista dei risultati con il valore
        ; della parte immaginaria
        (setf (res (+ $idx 1)) (eval-string num))
        ; elimina il valore e la relativa parte immaginaria dalla stringa
        (replace (string num im) s "")
        ;(println s)
        )))
  ; adesso la stringa data contiene solo la parte reale
  ; aggiorna la parte reale della lista soluzione
  (if (!= s "")
    (setf (res 0) (eval-string s)))
  res)

Proviamo:

(quat "1+2i+3j+4k")
;-> (1 2 3 4)
(quat "-1+3i-3j+7k")
;-> (-1 3 -3 7)
(quat "-1-4i-9j-2k")
;-> (-1 -4 -9 -2)
(quat "17-16i-15j-14k")
;-> (17 -16 -15 -14)
(quat "7+2i")
;-> (7 2 0 0)
(quat "2i-6k")
;-> (0 2 0 -6)
(quat "1-5j+2k")
;-> (1 0 -5 2)
(quat "3+4i-9k")
;-> (3 4 0 -9)
(quat "42i+j-k")
;-> (0 42 1 -1)
(quat "6-2i+j-3k")
;-> (6 -2 1 -3)
(quat "1+i+j+k")
;-> (1 1 1 1)
(quat "-1-i-j-k")
;-> (-1 -1 -1 -1)
(quat "16k-20j+2i-7")
;-> (-7 2 -20 16)
(quat "i+4k-3j+2")
;-> (2 1 -3 4)
(quat "5k-2i+9+3j")
;-> (9 -2 3 5)
(quat "5k-2j+3")
;-> (3 0 -2 5)
(quat "1.75-1.75i-1.75j-1.75k")
;-> (1.75 -1.75 -1.75 -1.75)
(quat "2.0j-3k+0.47i-13")
;-> (-13 0.47 2.0 -3)
(quat "5.6-3i")
;-> (5.6 -3 0 0)
(quat "k-7.6i")
;-> (0 -7.6 0 1)
(quat "0")
;-> (0 0 0 0)
(quat "0j+0k")
;-> (0 0 0 0)
(quat "-0j")
;-> (0 0 0 0)
(quat "1-0k")
;-> (1 0 0 0)
(quat " 1 - 1 k ")
;-> (1 0 0 -1)

Versione code-golf (142 caratteri, one-line):
(define(g s x)(local(n i f)(setq n "")(setq i(- x 1))(setq f nil)
(until f(if(or(=(s i) "+")(=(s i) "-"))(setq f true))
(push(s i) n)(-- i)) n))

Versione code-golf (335 caratteri, one-line):
(define(q s)(setq o '(0 0 0 0))(replace" "s"")
(if(and(!=(s 0)"-")(!=(s 0)"+"))(push"+"s))
(replace"([+-])([ijk])"s(string $1"1"$2)0)
(dolist(x '("i""j""k"))(setq idx(find x s))
(cond((nil? idx)(setf(o(+ $idx 1)) 0))
(true(setq n(g s idx))(setf(o(+ $idx 1))(eval-string n))
(replace(string n x)s""))))(if(!= s"")(setf(o 0)(eval-string s)))o)


---------------------------------------------
Modo idiomatico di fare replacement con regex
---------------------------------------------

Partiamo dalla stringa:
(setq s "-i+k-2j")

Modifichiamo la stringa:
(replace "+i" s "+1i") (replace "-i" s "-1i")
(replace "+j" s "+1j") (replace "-j" s "-1j")
(replace "+k" s "+1k") (replace "-k" s "-1k")
;-> "-1i+1k-2j"

Abbiamo inserito 1 davanti alle lettere "i", "j" e "k" che non avevano un numero.

Come facciamo a farlo con un replace unico (utilizzando una regex)?

Quando usiamo i "gruppi di cattura", la funzione 'replace' mette i match nelle variabili di sistema:

- '$0' = match completo
- '$1 $2 ...' = gruppi catturati

(setq s "-i+k-2j")
(replace "([+-])([ijk])" s (string $1 "1" $2) 0)
;-> "-1i+1k-2j"

Come funziona:
Regex:
  ([+-])   -> segno
  ([ijk])  -> lettera

Replacement:
  $1 "1" $2

Quindi:
  -i  -> -1i
  +k  -> +1k

'-2j' non matcha, quindi resta invariato.

Regola:
1) Se usiamo "regex con gruppi", usiamo $1 $2 ...
2) Se usiamo una "funzione", riceviamo la lista '("match" start end)'.


--------------------------
Il gioco dei tubi di cifre
--------------------------

Abbiamo N tubi (0 <= N <= 9) con N cifre casuali (0..(N-1)) in ogni tubo.
Le NxN cifre sono composte da N volte ogni cifra (N zeri, N uno, ecc.).
Inoltre abbiamo anche un tubo vuoto.
L'obiettivo è ordinare le cifre in modo che ogni tubo contenga N cifre di un solo tipo.
È possibile spostare solo la cifra più in alto da un tubo all'altro.
Non è possibile scambiare direttamente le cifre più in alto tra 2 tubi, ma solo una cifra alla volta.
Ogni tubo ha una capacità massima di N cifre.

Esempio con N=3 tubi (+ uno vuoto):

Mosse: 0            Mosse: 1            Mosse: 2            Mosse: 3
|0|0|0| |           | |0|0| |           | | |0| |           | | | |0|
|2|2|1| |           |2|2|1| |           |2|2|1|0|           |2|2|1|0|
|2|1|1| |           |2|1|1|0|           |2|1|1|0|           |2|1|1|0|
---------           ---------           ---------           ---------
 A B C D             A B C D             A B C D             A B C D
from --> to: A D    from --> to: B D    from --> to: C D    from --> to: B A

Mosse: 4            Mosse: 5
|2| | |0|           |2| |1|0|
|2| |1|0|           |2| |1|0|
|2|1|1|0|           |2| |1|0|
---------           ---------
 A B C D             A B C D
from --> to: B C    Stop: ogni tubo contiene 3 cifre di un solo tipo.

Nota: questo gioco è molto simile al "Water Sort Puzzle".

Scriviamo una serie di funzioni che permettono di giocare interattivamente a questo puzzle.

; Funzione che crea i tubi con le cifre da ordinare
(define (create-tubes N)
  (let (tubes '())
    (for (c 0 (- N 1))
      (push (dup c N) tubes))
    (setq tubes (randomize (flat tubes)))
    (setq tubes (explode tubes N))
    (for (t 0 (- N 1))
      (push " " (tubes t) -1))
    tubes))

; Funzione che stampa i tubi
(define (print-tubes matrix)
  (local (letters rows cols)
    (setq letters " A B C D E F G H I J")
    (setq rows (length matrix))
    (setq cols (length (matrix 0)))
    (for (r 0 (- rows 1))
      (for (c 0 (- cols 1))
        (print "|" (matrix r c)))
      (println "|"))
      (println (dup "-" (+ (* cols 2) 1)))
      (println (slice letters 0 (* cols 2))) '>))

; Funzione che verifica se in ogni tubo ci sono cifre tutte uguali
; (oppure tutti " ")
(define (solved? tubi)
  (let (sorted true)
  (dolist (riga (transpose tubi))
    (if-not (apply = riga) (setq sorted nil)))
  sorted))

; Funzione che gestisce una partita del gioco
(define (new-game N)
  (local (tubi mosse game-over input-ok input len from to
          cond1 cond2 stop r1 c1 r2 c2)
  (setq tubi (create-tubes N))
  (setq mosse 0)
  (setq game-over nil)
  (until game-over
    (println "\nMosse: " mosse)
    (print-tubes tubi)
    (setq input-ok nil)
    (until input-ok
      (print "from --> to: ")
      (setq input (parse (upper-case (read-line))))
      (setq len (length input))
      (cond ((zero? len) nil)
            ((!= len 2) nil)
            (true
              (setq from (- (char (input 0)) 65))
              (setq to (- (char (input 1)) 65))
              (if (and (>= from 0) (<= from N)
                       (>= to 0) (<= to N))
                  (setq input-ok true))))
    );input-ok
    ; cerca la prima cifra della colonna 'from'
    ; se esiste, allora cond1 --> true
    (setq cond1 nil)
    (setq r1 nil)
    (setq stop nil)
    (for (riga 0 (- N 1) 1 stop)
      (when (!= (tubi riga from) " ")
          (setq r1 riga) (setq stop true)))
    (setq c1 from)
    (if-not stop
          ; cifra non trovata (colonna vuota)
          (println "Il tubo " (char (+ from 65)) " 'from' è vuoto.")
          ; cifra trovata
          (setq cond1 true))
    ; cerca il primo posto vuoto della colonna 'to'
    ; se esiste, allora cond2 --> true
    (setq cond2 nil)
    (setq r2 nil)
    (setq stop nil)
    (for (riga (- N 1) 0 -1 stop)
      (when (= (tubi riga to) " ")
          (setq r2 riga) (setq stop true)))
    (setq c2 to)
    (if-not stop
          ; posto vuoto non trovato (colonna piena)
          (println "Il tubo " (char (+ to 65)) " 'to' è pieno.")
          ; posto vuoto trovato
          (setq cond2 true))
    ;(println cond1 { } cond2)
    ; Se entrambe le condizioni sono soddisfatte,
    ; (cioè se 'from' è 'piena' e 'to' è vuota)
    ; allora scambia la prima cifra della colonna 'from'
    ; con il primo posto vuoto " " della colonna 'to'
    (when (and cond1 cond2)
      (swap (tubi r1 c1) (tubi r2 c2))
      (++ mosse)
      (if (solved? tubi) (setq game-over true)))
  );game-over
  (println "\n*** BRAVO!!! ***")
  (println "Mosse: " mosse)
  (print-tubes tubi) '>))

Proviamo:
(new-game 4)

;-> Mosse: 0
;-> |0|2|0|3| |
;-> |0|3|3|1| |
;-> |2|1|2|1| |
;-> |3|2|1|0| |
;-> -----------
;->  A B C D E
;-> from --> to: D E
;-> 
;-> Mosse: 1
;-> |0|2|0| | |
;-> |0|3|3|1| |
;-> |2|1|2|1| |
;-> |3|2|1|0|3|
;-> -----------
;->  A B C D E
;-> from --> to: D E
;-> ...
;-> ...
;-> *** BRAVO!!! ***
;-> Mosse: 19
;-> | |2|1|0|3|
;-> | |2|1|0|3|
;-> | |2|1|0|3|
;-> | |2|1|0|3|
;-> -----------
;->  A B C D E


-----------------
Il dado a 6 facce
-----------------

Un dado da gioco di tipo occidentale è costruito in base a due regole.

Prima regola
La somma dei numeri su ogni coppia di facce opposte del dado è sempre 7.
Così il numero 1 è opposto al 6, il 2 al 5 e il 3 al 4.

Seconda regola
Se si guarda un dado in modo da vedere le tre facce corrispondenti ai numeri 1, 2, 3, si nota che i tre numeri si succedono in senso antiorario. Stessa cosa vale per le terne (3, 4, 5), (1, 3, 5) e (2, 4, 6).

La figura seguente mostra lo sviluppo sul piano di un dado.

                      +---------+
                      |       O |
                      |         |
                      | O       |
  +---------+---------+---------+---------+
  | O       | O     O | O     O |         |
  |    O    | O     O |         |    O    |
  |       O | O     O | O     O |         |
  +---------+---------+---------+---------+
                      | O     O |
                      |    O    |
                      | O     O |
                      +---------+

Scriviamo una funzione che simula il lancio di un dado (graficamente).

(define (dado)
  (let (d (+ (rand 6) 1))
    (setq linea "+---------+")
    (setq punti '(() ((1 1)) ((0 2) (2 0)) ((0 0) (1 1) (2 2))
                  ((0 0) (0 2) (2 0) (2 2)) ((0 0) (0 2) (1 1) (2 0) (2 2))
                  ((0 0) (0 2) (1 0) (1 2) (2 0) (2 2))))
    (setq num (punti d))
    ;(println d { } num)
    (println linea)
    (for (r 0 2)
      (print "|")
      (for (c 0 2)
        (if (find (list r c) num)
          (print " " "O" " ")
          ;else
          (print " " " " " ")))
      (println "|"))
    (println linea) d))

Proviamo:

(dado)
;-> +---------+
;-> | O     O |
;-> |         |
;-> | O     O |
;-> +---------+
;-> 4

(dado)
;-> +---------+
;-> | O     O |
;-> | O     O |
;-> | O     O |
;-> +---------+
;-> 6


-------------
Dado truccato
-------------

Un dado regolare (non truccato) è un dado in cui le 6 facce hanno tutte la stessa probabilità (1/6).
Un dado truccato è un dado in cui le 6 facce non sono tutte equiprobabili.

Scriviamo una funzione che simula un dado truccato.
La funzione prende due parametri:
1) una lista con i numeri da truccare
2) una lista che contiene le percentuali relative ai numeri da truccare

; Funzione che simula un dado truccato
; Parametri:
; 1) una lista con i numeri da truccare (es. (2 3))
; 2) lista con le percentuali dei numeri da truccare (es. 0.4 0.2)
(define (unfair nums probs)
  (local (somma-trucco trucco non-trucco prob-facce lst somma rnd stop out)
    ; somma delle probabilità delle facce truccate
    (setq somma-trucco (apply add probs))
    (cond
      ((> somma-trucco 1)
        (println "Errore: Somma probabilità truccate > 1: " somma-trucco)
        nil)
      (true
        ; A) Calcola la lista delle probabilità di ogni numero (faccia)
        ; numero di facce truccate
        (setq trucco (length nums))
        ; numero di facce non-truccate
        (setq non-trucco (- 6 trucco))
        ; probabilità delle facce non truccate
        (setq prob-facce (div (sub 1 somma-trucco) non-trucco))
        ; creazione della lista di probabilità dei numeri (facce)
        (setq lst (array 7 (list prob-facce)))
        ; il numero 0 non esiste nel dado, quindi ha probabilità 0
        (setf (lst 0) 0)
        ; aggiornamento della lista di probabilità
        ; con i valori dei dadi truccati
        (dolist (el nums)
          (setf (lst el) (probs $idx)))
        ;(println lst)
        (setq somma (apply add lst))
        ; controllo per verificare la correttezza
        ; della lista di probabilità creata
        (if (> (abs (sub 1 somma)) 1e-6)
            (println "Errore: Somma probabilità diversa da 1: " somma))
        ; B) Genera il numero (faccia) di output
        ; generiamo un numero random diverso da 1 e da 0
        ; (per evitare errori di arrotondamento)
        (while (and (setq rnd (random)) (or (= rnd 0) (= rnd 1))))
        (setq stop nil)
        (dolist (p lst stop)
          ; sottraiamo la probabilità corrente al numero random...
          (setq rnd (sub rnd p))
          ; se il risultato è minore di zero,
          ; allora restituiamo l'indice della probabilità corrente
          (if (< rnd 0) (set 'out $idx 'stop true)))
        out))))

Proviamo:

(unfair '(2) '(0.5))
;-> 4

(unfair '(2 3) '(0.4 0.2))
;-> 2

(unfair '(2 3) '(0.6 0.6))
;-> Errore: Somma probabilità truccate > 1: 1.2
;-> nil

(setq vet (array 7 '(0)))
;-> (0 0 0 0 0 0 0)
(unfair '(2 3) '(0.4 0.2))
;-> 5
(for (i 0 999999) (++ (vet (unfair '(2 3) '(0.4 0.2)))))
vet
;-> (0 99752 400175 200211 100247 99679 99936)
(apply + vet)
;-> 1000000

Dado regolare:
(setq pf (div 1 6))
(setq pd (list pf pf pf pf pf pf))
(unfair '(1 2 3 4 5 6) pd)
;-> 1
(setq vet (array 7 '(0)))
;-> (0 0 0 0 0 0 0)
(for (i 0 999999) (++ (vet (unfair '(1 2 3 4 5 6) pd))))
vet
;-> (0 166027 166590 166952 166501 166546 167384)
(apply + vet)
;-> 1000000

Dado sempre 6:
(setq vet (array 7 '(0)))
;-> (0 0 0 0 0 0 0)
(unfair '(6) '(1))
;-> 6
(for (i 0 999999) (++ (vet (unfair '(6) '(1)))))
vet
;-> (0 0 0 0 0 0 1000000)
(apply + vet)
;-> 1000000

Dado solo 3 o 4:
(setq vet (array 7 '(0)))
;-> (0 0 0 0 0 0 0)
(unfair '(3 4) '(0.5 0.5))
;-> 4
(for (i 0 999999) (++ (vet (unfair '(3 4) '(0.5 0.5)))))
vet
;-> (0 0 0 499924 500076 0 0)
(apply + vet)
;-> 1000000


---------------
Corsa campestre
---------------

Ad una corsa campestre partecipano N persone ognuna con un numero diverso.
Al termine della corsa viene compilato in una lista l'ordine di arrivo dei partecipanti.
Data un'altra lista contenente alcuni numeri dei partecipanti, determinare l'ordine di arrivo di questi.
La lista dei numeri contiene solo alcuni numeri che appartengono alla lista di arrivo.

Esempio:
  partecipanti: (1 3 5 6 8)
  lista di arrivo: (3 5 1 6 8)
  lista di numeri: (1 8 5)
  numeri in ordine di arrivo: (5 1 8)

(define (order arrivo numeri)
  (let (indici '())
    ; cerca gli indici dei numeri nella lista arrivo...
    (dolist (el numeri) (push (find el arrivo) indici))
    ; seleziona gli elementi della lista arrivo utilizzando gli indici ordinati
    (select arrivo (sort indici))))

Proviamo:

(setq arrivo '(3 5 1 6 8))
(setq numeri '(1 8 5))
(order arrivo numeri)
;-> (5 1 8)

(order '(3 1 2 5 4) '(1 3 4))
;-> (3 1 4)

(order '(1 4 5 3 2) '(2 5))
;-> (5 2)

(order '(1 2 3 4 5 6) '(6 5 4 3 2 1))
;-> (1 2 3 4 5 6)

Versione code-golf (65 caratteri):
(define(o a n i)(dolist(x n)(push(find x a)i))(select a(sort i)))

(setq a '(3 5 1 6 8))
(setq n '(1 8 5))
(o a n)
;-> (5 1 8)

(o '(3 1 2 5 4) '(1 3 4))
;-> (3 1 4)

(o '(1 4 5 3 2) '(2 5))
;-> (5 2)

(o '(1 2 3 4 5 6) '(6 5 4 3 2 1))
;-> (1 2 3 4 5 6)

Versione code-golf (60 caratteri):
(define(o a n)(select a(sort(map find n(dup a(length n))))))

(setq a '(3 5 1 6 8))
(setq n '(1 8 5))
(o a n)
;-> (5 1 8)

(o '(3 1 2 5 4) '(1 3 4))
;-> (3 1 4)

(o '(1 4 5 3 2) '(2 5))
;-> (5 2)

(o '(1 2 3 4 5 6) '(6 5 4 3 2 1))
;-> (1 2 3 4 5 6)


--------------------------
Processo di contaminazione
--------------------------

In una griglia MxN sono posizionate cellule con uno di tre valori possibili:

  0: vuoto
  1: cellula sana
  2: cellula malata

Il processo di contaminazione funziona come segue:
ogni minuto, qualsiasi cellula sana adiacente (in alto, in basso, a sinistra o a destra) a una cellula malata viene contaminata e divanta malata..
Ad esempio, dopo il primo minuto, tutte le cellule sane direttamente accanto alle cellule inizialmente malate diventeranno malate.
Dopo il secondo minuto, le cellule appena malate dal minuto 1 faranno ammalare le cellule sane adiacenti, e così via.

Trovare il numero minimo di minuti necessari affinché tutte le cellule sane risultino ammalate.
Trovare anche il numero di cellule sane rimaste al termine del processo (cioè tutte le cellule sane isolate che non possono essere contaminate da nessuna cellula malata.

Utilizziamo un algoritmo BFS (Breadth-First-Search) per simulare questo processo di diffusione minuto per minuto.
Iniziamo trovando tutte le cellule inizialmente malate e contando tutte le cellule sane.
Quindi elaboriamo il processo di contaminazione in cicli, dove ogni ciclo rappresenta un minuto.
In ogni ciclo, tutte le cellule attualmente malate fanno ammalare le cellule sane adiacenti simultaneamente.
Il processo continua finché tutte le cellule sane non sono malate o non è più possibile raggiungere cellule sane.

; Funzione che stampa la griglia di cellule
(define (print-grid grid titolo)
  (println "\n" titolo)
  (dolist (r grid)
    (dolist (v r)
      (print v " "))
    (println)))

; Funzione che simula il processo di contaminazione
(define (contaminazione grid)
  ; Simula il processo di contaminazione e ritorna (minuti sane-rimaste)
  (letn (
         ; numero di righe della griglia
         rows (length grid)
         ; numero di colonne (griglia rettangolare)
         cols (length (grid 0))
         ; contatore delle cellule sane (valore 1)
         numero-sane 0
         ; coda BFS con le posizioni delle cellule malate
         queue '()
         ; minuti trascorsi durante la simulazione
         minuti-totale 0
         ; spostamenti 4-direzionali: su destra giù sinistra
         directions '((-1 0) (0 1) (1 0) (0 -1))
         ; flag per terminare anticipatamente quando non restano sane
         finito nil
         ; valore finale dei minuti
         risultato 0)
    ; scansione completa della griglia
    (for (r 0 (- rows 1))
      (for (c 0 (- cols 1))
        (cond
          ; se la cella è già malata la inserisco nella coda iniziale
          ((= (grid r c) 2)
            (push (list r c) queue -1))
          ; se la cella è sana incremento il contatore
          ((= (grid r c) 1)
            (inc numero-sane)))))
    ; stampa la griglia iniziale
    (print-grid grid "stato iniziale:")
    ; stampa numero di sane iniziali
    (println "sane iniziali = " numero-sane)
    ; stampa numero di malate iniziali
    (println "malate iniziali = " (length queue))
    ; ciclo BFS: ogni iterazione rappresenta 1 minuto
    (while (and (> (length queue) 0) (> numero-sane 0) (not finito))
      ; incremento il tempo
      (++ minuti-totale)
      ; debug minuto corrente
      (println "\nminuto = " minuti-totale)
      ; numero di celle malate da processare in questo minuto (livello BFS)
      (let (current-level-size (length queue))
        ; elaboro solo gli elementi presenti all'inizio del minuto
        (for (i 1 current-level-size)
          ; estraggo la prossima posizione dalla coda
          (letn (pos (pop queue 0)
                 ; riga corrente
                 curr-row (pos 0)
                 ; colonna corrente
                 curr-col (pos 1))
            ; controllo le quattro direzioni adiacenti
            (dolist (d directions)
              ; calcolo coordinate della cella vicina
              (letn (next-row (+ curr-row (d 0))
                     next-col (+ curr-col (d 1)))
                ; verifico limiti e che sia sana
                (when (and (>= next-row 0) (< next-row rows)
                           (>= next-col 0) (< next-col cols)
                           (= (grid next-row next-col) 1))
                  ; la cella sana diventa malata
                  (setf (grid next-row next-col) 2)
                  ; la aggiungo alla coda per il minuto successivo
                  (push (list next-row next-col) queue -1)
                  ; decremento il numero di sane rimaste
                  (-- numero-sane)
                  ; debug infezione
                  (println "  malata: (" next-row { } next-col ")"
                           " sane rimaste = " numero-sane)
                  ; se non restano sane termino anticipatamente
                  (when (= numero-sane 0)
                    (set 'finito true))))))))
      ; stampa la griglia dopo questo minuto
      (print-grid grid (string "stato dopo minuto " minuti-totale)))
    ; salvo i minuti totali trascorsi
    (set 'risultato minuti-totale)
    ; stampa stato finale della griglia
    (print-grid grid "stato finale:")
    ; stampa minuti totali
    (println "\nminuti = " risultato)
    ; stampa quante sane sono rimaste isolate
    (println "sane rimaste = " numero-sane)
    ; ritorna (minuti sane-rimaste)
    (list risultato numero-sane)))

Proviamo:

(set 'g '((2 1 1)
          (1 1 0)
          (0 1 1)))
(contaminazione g)
;-> stato iniziale:
;-> 2 1 1
;-> 1 1 0
;-> 0 1 1
;-> sane iniziali = 6
;-> malate iniziali = 1
;-> 
;-> minuto = 1
;->   malata: (0 1) sane rimaste = 5
;->   malata: (1 0) sane rimaste = 4
;-> 
;-> stato dopo minuto 1
;-> 2 2 1
;-> 2 1 0
;-> 0 1 1
;-> 
;-> minuto = 2
;->   malata: (0 2) sane rimaste = 3
;->   malata: (1 1) sane rimaste = 2
;-> 
;-> stato dopo minuto 2
;-> 2 2 2
;-> 2 2 0
;-> 0 1 1
;-> 
;-> minuto = 3
;->   malata: (2 1) sane rimaste = 1
;-> 
;-> stato dopo minuto 3
;-> 2 2 2
;-> 2 2 0
;-> 0 2 1
;-> 
;-> minuto = 4
;->   malata: (2 2) sane rimaste = 0
;-> 
;-> stato dopo minuto 4
;-> 2 2 2
;-> 2 2 0
;-> 0 2 2
;-> 
;-> stato finale:
;-> 2 2 2
;-> 2 2 0
;-> 0 2 2
;-> 
;-> minuti = 4
;-> sane rimaste = 0
;-> (4 0)

(setq g '((2 1 1)
          (0 1 1)
          (1 0 1)))
(contaminazione g)
;-> ...
;-> (5 1)

(setq g '((0 2)))
(contaminazione g)
;-> ...
;-> (0 0)

(setq g '((0 2) (1 0)))
(contaminazione g)
;-> ...
;-> (1 1)


-------------------------------------------------------
Trasformare una funzione non-distruttiva in distruttiva
-------------------------------------------------------

Una funzione viene detta "distruttiva" quando modifica il proprio argomento (pass-by-reference).
Una funzione viene detta "non-distruttiva" quando modifica una copia del proprio argomento (pass-by-value).

Esempio funzione distruttiva: sort
----------------------------------
(setq a '(2 5 3 1 7))
(sort a)
;-> (1 2 3 5 7)
Il parametro 'a' è stato modificato da 'sort':
a
;-> (1 2 3 5 7)

Per fare in modo che la lista 'a' non venga modificata occorre usare la funzione 'copy':

(setq a '(2 5 3 1 7))
(sort (copy a))
;-> (1 2 3 5 7)
In questo caso il parametro 'a' non viene modificato:
a
;-> (2 5 3 1 7)

Esempio funzione non-distruttiva: randomize
-------------------------------------------
(setq a '(2 5 3 1 7))
(randomize a true)
;-> (7 2 3 1 5)
Il parametro 'a' non è stato modificato da 'randomize':
a
;-> (2 5 3 1 7)

Per fare in modo che la lista 'a' venga modificata occorre scrivere una macro.
Vediamo prima perchè una funzione non è sufficiente:

La seguente funzione si limita a passare gli argomenti di 'randomize1' alla funzione 'randomize', quibdi la lista 'a' non viene modificata:

(define (randomize1)
  (apply randomize (args)))

(setq a '(2 5 3 1 7))
(randomize1 a true)
;-> (3 2 5 7 1)
a
;-> (2 5 3 1 7)

Proviamo con una macro:

(define-macro (randomize2) (eval (cons 'randomize (args))))
(randomize2 '(2 5 3 1 7) true)
;-> (5 2 3 7 1)

(setq a '(2 5 3 1 7))
(randomize2 a true)
;-> (3 5 2 1 7)
a
;-> (2 5 3 1 7)

Anche questa macro non modifica la lista 'a'.

Normalmente per modificare la lista 'a' scriviamo:

(setq a (randomize a true))
;-> (7 1 3 2 5)
a
;-> (7 1 3 2 5)

Quindi possiamo scrivere una macro che prima crea l'espressione '(setq...)' e poi la valuta/esegue:

(define-macro (randomize@)
 ; unione degli argomenti in formato stringa
 (setq arg (join (map string (args)) " "))
 (println "arg: " arg)
 ; creazione dell'espressione (setq...)
 (setq expr (string "(setq " (args 0) " (randomize " arg "))"))
 (println "expr: " expr)
 (eval-string expr))

(setq a '(2 5 3 1 7))
(randomize@ a true)
;-> arg: a true
;-> expr: (setq a (randomize a true))
;-> (1 7 5 3 2)
a
;-> (1 7 5 3 2)
In questo caso la lista 'a' è stata modificata.

Proviamo la macro all'interno di una funzione:

(define (test)
  (let (lst (rand 10 6))
    (println "Originale: " lst)
    (randomize@ lst)
    (println "Ordinata: " lst) '>))

(test)
;-> Originale: (1 4 2 8 2 7)
;-> arg: lst
;-> expr: (setq lst (randomize lst))
;-> Ordinata: (7 1 4 2 8 2)

Nota: la macro è lenta rispetto all'assegnazione diretta.
Inoltre le macro vengono generalmente usate per restituire codice e non per restituire un valore.


----------------
Matrici riflesse
----------------

Problema 1
----------
Date due matrici A e B, determinare se sono una il riflesso dell'altra.

Problema 2
----------
Data una matrice A, determinare tutte le matrici riflesse.

Soluzione 1
-----------
Per verificare se due matrici sono l'una il riflesso dell'altra, dobbiamo prima definire l'asse di simmetria.

Ecco i 4 casi possibili:

1) Riflessione orizzontale (Left <-> Right)
-------------------------------------------
Specchio rispetto ad un asse verticale.
Effetto: inverti le colonne, le righe restano uguali.
Condizione: A[i][j] == B[i][N-1-j]
Dimensioni richieste A e B entrambe M × N.
Esempio:
Matrice A: [1, 2, 3] --> Matrice B: [3, 2, 1]

(define (riflessione-orizzontale? A B)
  (letn (r (length A)
         c (length (A 0))
         ok true)
    (if (or (!= r (length B)) (!= c (length (B 0))))
        nil
        (begin
          (for (i 0 (- r 1))
            (for (j 0 (- c 1))
              (if (!= (A i j) (B i (- c 1 j)))
                  (setq ok nil))))
          ok))))

2) Riflessione verticale (Top <-> Bottom)
-----------------------------------------
Specchio rispetto ad un asse orizzontale.
Effetto: inverti le righe, le colonne restano uguali.
Condizione: A[i][j] == B[M-1-i][j]
Dimensioni richieste A e B entrambe M × N.

(define (riflessione-verticale? A B)
  (letn (r (length A)
         c (length (A 0))
         ok true)
    (if (or (!= r (length B)) (!= c (length (B 0))))
        nil
        (begin
          (for (i 0 (- r 1))
            (for (j 0 (- c 1))
              (if (!= (A i j) (B (- r 1 i) j))
                  (setq ok nil))))
          ok))))

3) Riflessione diagonale (trasposizione)
----------------------------------------
Specchio rispetto alla diagonale principale (alto-sinistra -> basso-destra).
Effetto: scambio righe <-> colonne.
Condizione: A[i][j] == B[j][i]
Dimensioni richieste (devono essere invertite):
A = M × N
B = N × M
Questa NON è un semplice flip: è uno scambio di indici.
Qui non stiamo 'ribaltando', stiamo 'ruotando la griglia degli indici'.
Esempio 2×3:
1 2 3      1 4
4 5 6  ->  2 5
           3 6

(define (riflessione-diagonale? A B)
  (letn (r (length A)
         c (length (A 0))
         ok true)
    (if (or (!= c (length B)) (!= r (length (B 0))))
        nil
        (begin
          (for (i 0 (- r 1))
            (for (j 0 (- c 1))
              (if (!= (A i j) (B j i))
                  (setq ok nil))))
          ok))))

4) Riflessione diagonale secondaria (alto-destra -> basso-sinistra)
-------------------------------------------------------------------
Specchio rispetto alla diagonale secondaria (alto-destra -> basso-sinistra).
Condizione (matrice quadrata N x N): A[i][j] == B[N-1-j][N-1-i]
Dimensioni richieste (solo matrici quadrate): N x N
Questa riflessione equivale a:
trasposizione + flip verticale (oppure orizzontale + trasposizione).
Esempio:
1 2      4 2
3 4  ->  3 1
Questo è davvero uno 'specchio'.

(define (riflessione-diagonale-secondaria? A B)
  (letn (n (length A)
         ok true)
    (if (or (!= n (length B)) (!= n (length (A 0))) (!= n (length (B 0))))
        nil
        (begin
          (for (i 0 (- n 1))
            (for (j 0 (- n 1))
              (if (!= (A i j) (B (- n 1 j) (- n 1 i)))
                  (setq ok nil))))
          ok))))

Nota:
questa trasformazione ha senso solo per matrici quadrate, quindi il controllo dimensioni è necessario.

Intuitivamente:
- orizzontale -> flip righe
- verticale -> flip colonne
- diagonale principale -> scambio indici (trasposizione)
- diagonale secondaria -> scambio + doppio flip

Riepilogo degli indici
----------------------
orizzontale:   (i, j) -> (i, N-1-j)
verticale:     (i, j) -> (M-1-i, j)
trasposizione: (i, j) -> (j, i)
secondaria:    (i, j) -> (N-1-j, N-1-i)

L'idea migliore è non costruire la matrice riflessa, ma confrontare direttamente gli indici 'specchiati' è più veloce e consuma meno memoria.

Algoritmo (pseudo-codice):
Verificare che A e B abbiano le stesse dimensioni (Righe x Colonne).
Scegliere l'asse di riflessione.
Cicla su ogni elemento i, j:
  Se A[i][j] != B[posizione_specchiata], allora NON sono riflesse.
  Se il ciclo termina senza errori, le matrici sono speculari.

; Funzione generale per determinare se due matrici A e B sono una il riflesso dell'altra
(define (tipo-riflessione A B)
  ; numero righe e colonne di A
  (letn (rA (length A)
         cA (length (A 0))
         ; numero righe e colonne di B
         rB (length B)
         cB (length (B 0))
         ok true
         rifles '())
    ; riflessione ORIZZONTALE (Left<->Right)
    ; stesse dimensioni
    ; A[i][j] == B[i][cA-1-j]
    (if (and (= rA rB) (= cA cB))
        (begin
          (setq ok true)
          (for (i 0 (- rA 1))
            (for (j 0 (- cA 1))
              (if (!= (A i j) (B i (- cA 1 j)))
                  (setq ok nil))))
          (if ok (push "orizzontale" rifles))))
    ; riflessione VERTICALE (Top<->Bottom)
    ; stesse dimensioni
    ; A[i][j] == B[rA-1-i][j]
    (if (and (= rA rB) (= cA cB))
        (begin
          (setq ok true)
          (for (i 0 (- rA 1))
            (for (j 0 (- cA 1))
              (if (!= (A i j) (B (- rA 1 i) j))
                  (setq ok nil))))
          (if ok (push "verticale" rifles))))
    ; riflessione DIAGONALE PRINCIPALE (trasposizione)
    ; dimensioni invertite
    ; A[i][j] == B[j][i]
    (if (and (= rA cB) (= cA rB))
        (begin
          (setq ok true)
          (for (i 0 (- rA 1))
            (for (j 0 (- cA 1))
              (if (!= (A i j) (B j i))
                  (setq ok nil))))
          (if ok (push "diagonale-principale" rifles))))
    ; riflessione DIAGONALE SECONDARIA
    ; solo quadrate
    ; A[i][j] == B[n-1-j][n-1-i]
    (if (and (= rA cA) (= rB cB) (= rA rB))
        (begin
          (setq ok true)
          (for (i 0 (- rA 1))
            (for (j 0 (- cA 1))
              (if (!= (A i j) (B (- rA 1 j) (- rA 1 i)))
                  (setq ok nil))))
          (if ok (push "diagonale-secondaria" rifles))))
    ; risultato finale
    (if rifles
      (list true rifles)
      (list nil nil))))

Proviamo:

(setq A '((1 2 3)))
(setq B '((3 2 1)))
(tipo-riflessione A B)
;-> (true "orizzontale")

(setq A '((1 2)
          (3 4)))
(setq B '((4 2)
          (3 1)))
(tipo-riflessione A B)
;-> (true ("diagonale-secondaria"))

(setq A '((1 2 3)))
(setq B '((3 2 1)))
(tipo-riflessione A B)
;-> (true ("orizzontale"))

Nota: due matrici possono avere piu di un tipo di riflessione contemporaneamente.
Succede quando la matrice ha simmetrie interne.
In quel caso più trasformazioni producono la stessa matrice.
Non è un fatto raro: dipende solo dai valori.

Caso 1 — matrice costante
Tutti gli elementi uguali.
Qualunque trasformazione dà la stessa matrice:
- orizzontale
- verticale
- diagonale principale
- diagonale secondaria
(setq a1 '((1 1)
           (1 1)))
(tipo-riflessione a1 a1)
;-> (true ("diagonale-secondaria" "diagonale-principale"
;->        "verticale" "orizzontale"))

Caso 2 — doppia simmetria (orizzontale + verticale)
Simmetrica rispetto:
- asse verticale
- asse orizzontale
Ma non rispetto alle diagonali.
(setq a2 '((1 2 1)
           (3 4 3)
           (1 2 1)))
(tipo-riflessione a2 a2)
;-> (true ("verticale" "orizzontale"))

Caso 3 — simmetria diagonale
In questo caso risulta: A[i][j] = A[j][i]
Quindi simmetrica solo rispetto:
- diagonale principale
(setq a3 '((1 2 3)
           (2 4 5)
           (3 5 6)))
(tipo-riflessione a3 a3)
;-> (true ("diagonale-principale"))

Caso 4 — rettangolari
Se la matrice NON è quadrata:
- orizzontale e verticale -> possibili
- diagonale principale -> cambia dimensioni
- diagonale secondaria -> impossibile
Quindi al massimo una tra (orizzontale, verticale) + eventualmente la trasposizione.

Regola generale:
Più riflessioni sono vere solo se:
  A = riflesso(A)
cioè la matrice è 'invariante' rispetto a quell'asse.

Soluzione 2
-----------
Costruiamo ora una funzione che, data una sola matrice A, genera tutte le matrici ottenute dalle riflessioni possibili.
Restituisce una lista associativa del tipo:

  ((orizzontale <matrice>)
   (verticale <matrice>)
   (diagonale-principale <matrice>)
   (diagonale-secondaria <matrice>))   ; solo se quadrata

(define (riflessioni A)
  ; numero righe e colonne
  (letn (r (length A)
         c (length (A 0))
         out '())
    ; RIFLESSO ORIZZONTALE (Left<->Right)
    ; invertiamo le colonne
    ; B[i][j] = A[i][c-1-j]
    (let (B '())
      (for (i 0 (- r 1))
        (let (row '())
          (for (j 0 (- c 1))
            (push (A i (- c 1 j)) row -1))
          (push row B -1)))
      (push (list "orizzontale" B) out -1))
    ; RIFLESSO VERTICALE (Top<->Bottom)
    ; invertiamo le righe
    ; B[i] = A[r-1-i]
    (let (B '())
      (for (i (- r 1) 0 -1)
        (push (A i) B -1))
      (push (list "verticale" B) out -1))
    ; RIFLESSO DIAGONALE PRINCIPALE (trasposizione)
    ; B[j][i] = A[i][j]
    (let (B '())
      (for (j 0 (- c 1))
        (let (row '())
          (for (i 0 (- r 1))
            (push (A i j) row -1))
          (push row B -1)))
      (push (list "diagonale-principale" B) out -1))
    ; RIFLESSO DIAGONALE SECONDARIA
    ; solo matrici quadrate
    ; B[i][j] = A[n-1-j][n-1-i]
    (if (= r c)
        (let (B '())
          (for (i 0 (- r 1))
            (let (row '())
              (for (j 0 (- c 1))
                (push (A (- r 1 j) (- r 1 i)) row -1))
              (push row B -1)))
          (push (list "diagonale-secondaria" B) out -1)))
    ; lista finale delle trasformazioni
    out))

Proviamo:

(setq A '((1 2)
          (3 4)))
(riflessioni A)
;-> (("orizzontale" ((2 1) (4 3)))
;->  ("verticale" ((3 4) (1 2)))
;->  ("diagonale-principale"  ((1 3) (2 4)))
;->  ("diagonale-secondaria" ((4 2) (3 1))))

(setq B '((1 2 3)
          (3 4 5)))
(riflessioni B)
(("orizzontale" ((3 2 1) (5 4 3)))
 ("verticale" ((3 4 5) (1 2 3)))
 ("diagonale-principale" ((1 3) (2 4) (3 5))))

Nota: la funzione restituisce anche le trasformazioni uguali alla matrice data.


--------------------
Rotazioni di matrici
--------------------

1️) Rotazioni geometriche (90/180/270 gradi)
-------------------------------------------

; Funzione che ruota una matrice in senso orario o antiorario di 90 o 180 o 270 gradi
(define (rotate-matrix A gradi direzione)
  ; numero righe e colonne di A
  (letn (r (length A)
         c (length (A 0))
         B '())
    ; 90° ORARIO
    (if (and (= gradi 90) (= direzione 'orario))
        (begin
          ; dimensioni invertite
          (for (i 0 (- c 1))
            (let (row '())
              (for (j 0 (- r 1))
                (push (A (- r 1 j) i) row -1))
              (push row B -1)))))
    ; 90° ANTIORARIO
    (if (and (= gradi 90) (= direzione 'antiorario))
        (begin
          (for (i 0 (- c 1))
            (let (row '())
              (for (j 0 (- r 1))
                (push (A j (- c 1 i)) row -1))
              (push row B -1)))))
    ; 180° (stesso per entrambe le direzioni)
    (if (= gradi 180)
        (begin
          (for (i (- r 1) 0 -1)
            (let (row '())
              (for (j (- c 1) 0 -1)
                (push (A i j) row -1))
              (push row B -1)))))
    ; 270° ORARIO
    (if (and (= gradi 270) (= direzione 'orario))
        (begin
          (for (i 0 (- c 1))
            (let (row '())
              (for (j 0 (- r 1))
                (push (A j (- c 1 i)) row -1))
              (push row B -1)))))
    ; 270° ANTIORARIO
    (if (and (= gradi 270) (= direzione 'antiorario))
        (begin
          (for (i 0 (- c 1))
            (let (row '())
              (for (j 0 (- r 1))
                (push (A (- r 1 j) i) row -1))
              (push row B -1)))))
    B))

Proviamo:

(setq m '((1 2 3 4)
          (5 6 7 8)
          (9 10 11 12)
          (13 14 15 16)))

(rotate-matrix m 90 'orario)
;-> ((13 9 5 1)
;->  (14 10 6 2)
;->  (15 11 7 3)
;->  (16 12 8 4))

(rotate-matrix m 90 'antiorario)
;-> ((4 8 12 16)
;->  (3 7 11 15)
;->  (2 6 10 14)
;->  (1 5 9 13))

(setq m1 (rotate-matrix m 90 'orario))
(setq m2 (rotate-matrix m1 90 'antiorario))
(= m m2)
;-> true

(setq m1 (rotate-matrix m 180 'orario))
(setq m2 (rotate-matrix m1 180 'antiorario))
(= m m2)
;-> true

(setq m1 (rotate-matrix m 270 'orario))
(setq m2 (rotate-matrix m1 270 'antiorario))
(= m m2)
;-> true

2) Rotazioni di 1..N posizioni ("layered rotation")
---------------------------------------------------

Layered rotation
Spostamento degli elementi lungo i bordi concentrici (anelli/layer) della matrice di X posizioni.
Sia matrici quadrate che rettangolari >= 2X2.
Direzioni oraria e antioraria.
Numero di posizioni X da ruotare.
Ogni anello (layer) ruota indipendentemente dagli altri.
L'elemento centrale di matrici con dimensioni dispari rimane fermo.

; Funzione che ruota una matrice in senso orario di una posizione
; layered rotation
(define (rotate-ring-orario matrix row col idx)
  (local (i value temp)
    (setq value (matrix row col idx))
    ;; caso A: bottom right -> left
    (setq i (- col 1))
    (while (>= i idx)
      (setq temp (matrix row i))
      (setf (matrix row i) value)
      (setq value temp)
      (-- i))
    ;; caso B: bottom left -> top
    (setq i (- row 1))
    (while (>= i idx)
      (setq temp (matrix i idx))
      (setf (matrix i idx) value)
      (setq value temp)
      (-- i))
    ;; caso C: top left -> right
    (setq i (+ idx 1))
    (while (<= i col)
      (setq temp (matrix idx i))
      (setf (matrix idx i) value)
      (setq value temp)
      (++ i))
    ;; caso D: top right -> bottom
    (setq i (+ idx 1))
    (while (<= i row)
      (setq temp (matrix i col))
      (setf (matrix i col) value)
      (setq value temp)
      (++ i))
    matrix))

; Funzione che ruota una matrice in senso orario di una posizione
; layered rotation
(define (rotate-ring-antiorario matrix row col idx)
  (local (i value temp)
    ;; valore iniziale: angolo in alto a sinistra dell'anello
    (setq value (matrix idx idx))
    ;; caso A: colonna sinistra dall'alto verso il basso
    (setq i (+ idx 1))
    (while (<= i row)
      (setq temp (matrix i idx))
      (setf (matrix i idx) value)
      (setq value temp)
      (++ i))
    ;; caso B: riga inferiore da sinistra verso destra
    (setq i (+ idx 1))
    (while (<= i col)
      (setq temp (matrix row i))
      (setf (matrix row i) value)
      (setq value temp)
      (++ i))
    ;; caso C: colonna destra dal basso verso l'alto
    (setq i (- row 1))
    (while (>= i idx)
      (setq temp (matrix i col))
      (setf (matrix i col) value)
      (setq value temp)
      (-- i))
    ;; caso D: riga superiore da destra verso sinistra
    (setq i (- col 1))
    (while (>= i idx)
      (setq temp (matrix idx i))
      (setf (matrix idx i) value)
      (setq value temp)
      (-- i))
    matrix))

; Funzione che ruota una matrice in senso orario o antiorario di una posizione
; layered rotation
(define (rotate-matrix-one matrix direzione)
  (local (row col size ring)
    (setq row (length matrix))
    (setq col (length (matrix 0)))
    (setq size (min row col))
    (setq ring 0)
    (while (< ring (/ size 2))
      (-- row)
      (-- col)
      (if (= direzione 'orario)
          (setq matrix (rotate-ring-orario matrix row col ring))
          (setq matrix (rotate-ring-antiorario matrix row col ring)))
      (++ ring))
    matrix))

Proviamo:

(setq m '((1 2 3 4)
          (5 6 7 8)
          (9 10 11 12)
          (13 14 15 16)))
(rotate-matrix-one  m 'orario)
;-> ((5 1 2 3)
;->  (9 10 6 4)
;->  (13 11 7 8)
;->  (14 15 16 12))
(rotate-matrix-one  m 'antiorario)
;-> ((2 3 4 8) 
;->  (1 7 11 12) 
;->  (5 6 10 16) 
;->  (9 13 14 15))

(setq q '((1 2 3) 
          (4 5 6)))
(rotate-matrix-one  q 'orario)
;-> ((4 1 2)
;->  (5 6 3))
(rotate-matrix-one  q 'antiorario)
;-> ((2 3 6)
;->  (1 4 5))

(setq t '(( 1  2  3  4  5)
          ( 6  7  8  9 10)
          (11 12 13 14 15)
          (16 17 18 19 20)))
(setq t1 (rotate-matrix-one t 'orario))
(setq t2 (rotate-matrix-one t1 'orario))
(setq t3 (rotate-matrix-one t2 'antiorario))
(setq t4 (rotate-matrix-one t3 'antiorario))
(= t t4)
;-> true

Vedi anche "Rotazione di 90 gradi (in senso orario) di una matrice quadrata" su "Note libere 18".
Vedi anche "Rotazione di una matrice" in "Note libere 19".


----------------------
Il giudice della città
----------------------

Una città è popolata da N persone etichettate da 1 a N.
Il 'giudice della città' ha due caratteristiche uniche:
1) Il giudice non si fida di nessun'altra persona in città
2) Tutti gli altri (N-1) si fidano del giudice

Abbiamo una lista F con elementi del tipo: (A B) che significa:
'A' si fida di 'B' (dove 'A' e 'B' sono numeri interi compresi tra 1 e N)
Le relazioni di fiducia sono direzionali:
se la persona A si fida della persona B, non significa che B si fidi di A.

Trovare, se esiste, il giudice della città.

Esempio:
Se N = 3 e F = ((1 3) (2 3)), la persona 3 è il giudice perché:
La persona 3 non si fida di nessuno (nessuna voce con 3 come primo elemento)
Tutti gli altri (persone 1 e 2) si fidano della persona 3.

Le relazioni di fiducia possono essere rappresentate come a un grafo orientato in cui ogni persona è un nodo e ogni relazione di fiducia è un arco orientato dalla persona che si fida alla persona che riceve fiducia.
In questo modo il giudice della città è un nodo con le seguenti proprietà:
  In-degree: N-1 archi in entrata (tutti gli altri si fidano di lui)
  Out-degree: 0 archi in uscita (il giudice non si fida di nessuno)
Invece di costruire un grafo vero e proprio, possiamo semplificare il tutto calcolando In-degree e Out-degree per ogni persona.

(define (giudice F)
  (local (N out-degree in-degree judge out)
    ; calcolo del numero di persone
    (setq N (apply max (flat F)))
    ; vettore archi in uscita
    (setq out-degree (array (+ N 1) '(0)))
    ; vettore archi in ingresso
    (setq in-degree (array (+ N 1) '(0)))
    ; calcola i valori di out-degree e in-degree per ogni persona
    (dolist (el F)
      (++ (out-degree (el 0)))
      (++ (in-degree (el 1))))
    ; cerca il giudice
    (setq judge nil)
    (for (i 1 N 1 judge)
      ;(println i { } (out-degree i) { } (in-degree i))
      (when (and (= (out-degree i) 0) (= (in-degree i) (- N 1)))
        (setq out i) (setq judge true)))
    out))

Proviamo:

(giudice '((1 2)))
;-> 2
(giudice '((1 3) (2 3) (3 1)))
;-> nil
(giudice '((1 3) (2 3)))
;-> 3


---------------------
Sequenza di Brougnard
---------------------

Brougnard sequence
------------------
The gb[n] sequence definition is the following :
1) gb[0] = starter =  any prime > 2
2) computation of gb[n+1]
let p : = gb[n]
if p  = 2 (mod 3)
         then p  := (p-1)/2  ;
         else p  := p * 2 ;
gb[n+1] := next prime > p  ;

Examples:
gb[0]=23  -> (23 13 29 27 11 7)
(length 6)
gb[0]=281 -> (281 149 79 163 331 673 1361 683 347 179 97 197 101 53 29 17 11 7)
(length 18)

Note: The sequence 7 17 11 7 .. is periodic.
A gb-sequence is convergent if gb[n] = 7 for some n. The sequence stops at n.
A sequence which is not convergent is divergent.

; Funzione che verifica se un dato numero è primo
(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

; Funzione che calcola il primo successivo ad un dato numero
(define (next-prime num) (until (prime? (++ num))) num)

(next-prime 11)
;-> 13

; Funzione che genera una sequenza di Brougnard
; Parametri: starter = numero primo > 2
;            limit = lunghezza massima della sequenza
; Output: lista di numeri (sequenza di Brougnard a partire da starter)
(define (gb-seq starter limit)
  (letn ((out (list starter)) (p starter) (stop nil) (k 0))
    (until stop
      (if (= (% p 3) 2)
          (setq p (/ (- p 1) 2))
          (setq p (* p 2)))
      (setq p (next-prime p))
      (push p out -1)
      (++ k)
      (if (= p 7) (setq stop true))
      (when (> k limit) (setq stop true) (println starter ": Limit error!!!")))
    ;(println (length out))
    out))

Proviamo:

Nota: il numero primo 2 produce una sequenza infinita di 2
(gb-seq 2 10)
;-> 2: Limit error!!!
;-> (2 2 2 2 2 2 2 2 2 2 2 2)

(gb-seq 23 1000)
;-> (23 13 29 17 11 7)
(gb-seq 281 1000)
;-> (281 149 79 163 331 673 1361 683 347 179 97 197 101 53 29 17 11 7)

Vediamo quale numero primo ha la sequenza di Brougnard più lunga.
Abbiamo bisogno di una funzione che genera N numeri primi.

(define (primes num-primes)
"Generate a given number of prime numbers (starting from 2)"
  (let ( (k 3) (tot 1) (out '(2)) )
    (until (= tot num-primes)
      (when (= 1 (length (factor k)))
        (push k out -1)
        (++ tot))
      (++ k 2))
    out))

(time (primes 1e5))
;-> 605.382

; Funzione che calcola la sequenza più lunga di N numeri primi (1..N)
; Parametri: N = numero di primi da calcolare
;            limit = lunghezza massima della sequenza
; Output: coppia di elementi (primo lunghezza-sequenza)
(define (find-gb N limit)
  (let ((max-len 0) (max-prime 3) (lst '()) (primi (primes N)))
    (pop primi)
    (dolist (primo primi)
      (setq lst (gb-seq primo limit))
      ;(println primo { } (length lst))
      (when (> (setq len (length lst)) max-len)
        (setq max-len len)
        (setq max-primo primo)))
    (list max-primo max-len)))

Proviamo:

(find-gb 12 5000)
;-> (31 21)
(find-gb 100 5000)
;-> 359 61

(time (println (find-gb 1000 5000)))
;-> (6481 451)
;-> 1780.268
(apply max (gb-seq 6481 5000))
;-> 111800125737497

(time (println (find-gb 1400 5000)))
;-> (9241 469)
;-> 2330.738
(time (println (find-gb 1500 5000)))
;-> (12211 1194)
;-> 131367.445


-----------------
Sequenza multipla
-----------------

Definizione:

a(0) = 0 oppure a(0) = 1

a(n) = n * a(n-1) if n pari
       n + a(n-1) if n dispari

oppure

a(n) = n * a(n-1) if n dispari
       n + a(n-1) if n pari

Scriviamo una funzione che calcola tutte le sequenze della definizione.

(define (seq N a0 f)
  (let (out (list a0))
    (for (i 1 N)
      (if (f i)
        (push (* i (out (- i 1))) out))
        (push (+ i (out (- i 1))) out))
    (unique (sort out))))

Proviamo:

(seq 20 0 odd?)
;-> (0 1 2 4 5 7 8 13 14 16 17 18 19 22 23 25 26 33 34 44 65 75 119 152)
(seq 20 1 odd?)
;-> (1 2 3 5 6 8 9 10 11 14 15 17 18 21 23 24 26 27 40 41 55 78 136 150 171)
(seq 20 0 even?)
;-> (0 1 3 4 5 9 10 12 13 14 15 16 17 19 20 24 27 28 30 40 48 56 80 180)
(seq 20 1 even?)
;-> (1 2 4 5 6 7 8 10 11 12 13 14 18 19 21 22 28 29 31 32 50 84 96 112 200 216)


--------------------------------
Self Numbers (Numeri Colombiani)
--------------------------------

Sequenza OEIS A003052:
Self numbers or Colombian numbers (numbers that are not of the form m + sum of digits of m for any m).
  1, 3, 5, 7, 9, 20, 31, 42, 53, 64, 75, 86, 97, 108, 110, 121, 132, 143,
  154, 165, 176, 187, 198, 209, 211, 222, 233, 244, 255, 266, 277, 288,
  299, 310, 312, 323, 334, 345, 356, 367, 378, 389, 400, 411, 413, 424,
  435, 446, 457, 468, 479, 490, 501, 512, 514, 525, ...

The following recurrence relation generates infinite base 10 self numbers:
C(k)=8.10^(k-1)+C(k-1)+8; C(1)=9
The term "self numbers" was coined by Kaprekar (1959).
The term "Colombian number" was coined by Recamán (1973) of Bogota, Colombia.
The asymptotic density of sequence is approximately 0.0977778 (Guaraldo, 1978).

La relazione: C(k)=8·10^(k-1)+C(k-1)+8 ; C(1)=9
non genera tutti i self numbers, ma solo una sottosequenza infinita garantita (serve come costruzione teorica).

Un numero 'n' è 'self' se non esiste alcun 'm' tale che
  m + somma_cifre(m) = n

Algoritmo
1) per ogni 'm' calcolare il generatore:
   g(m) = m + sumdigits(m)
2) segnare tutti i valori generati
3) i numeri non generati sono i numeri self

(define (digit-sum num)
"Calculate the sum of the digits of an integer"
  (let (out 0)
    (while (!= num 0)
      (setq out (+ out (% num 10)))
      (setq num (/ num 10))
    )
    out))

; Funzione che genera i self numbers fino a N
(define (self-numbers N)
  ; gen = array di segnature (true se il numero è generato)
  ; out = lista risultato dei self numbers
  (letn (gen (array (+ N 1) '(nil)) out '())
    ; per ogni m da 1 a N calcola il generatore g(m)=m+somma_cifre(m)
    (for (m 1 N)
      ; calcolo del valore generato
      (let (g (+ m (digit-sum m)))
        ; se il generato è entro il limite, viene segnato come "non self"
        (if (<= g N) (setf (gen g) true))))
    ; scansiona tutti i numeri 1..N
    (for (i 1 N)
      ; se non è mai stato segnato, è un self number
      (if (nil? (gen i)) (push i out -1)))
    out))

(self-numbers 525)
;-> (1 3 5 7 9 20 31 42 53 64 75 86 97 108 110 121 132 143
;->  154 165 176 187 198 209 211 222 233 244 255 266 277 288
;->  299 310 312 323 334 345 356 367 378 389 400 411 413 424
;->  435 446 457 468 479 490 501 512 514 525)

Densità asintotica:

(define (density N) (div (length (self-numbers N)) N))

(map density '(1e2 1e3 1e4 1e5 1e6))
;-> (0.13 0.102 0.0983 0.09784 0.097786)

(time (println (density 1e7)))
;-> 0.0977787
;-> 12311.072

============================================================================

