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

============================================================================

