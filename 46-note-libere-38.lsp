================

 NOTE LIBERE 38

================

  "Nothing interesting"

--------------------------------------------------
Distanza minima e massima tra due percorsi espansi
--------------------------------------------------

Dati due percorsi espansi (nella stessa posizione temporale), determinare la distanza Manhattan minima e massima tra due percorsi.

Esempio:
  P1: (0,0) (1,0) (2,0)
  P2: (0,3) (1,3) (2,3)
  distanza minima = 3

Esempio:
  P1: (0,0) (1,0) (2,0)
  P2: (0,3) (1,3) (2,3)
  distanza massima = 3

Assumiamo che i percorsi iniziano allo stesso istante e quindi confrontiamo i punti con lo stesso indice temporale:

  t=0 -> path1[0] con path2[0]
  t=1 -> path1[1] con path2[1]

(define (dist-min-paths-manh path1 path2)
  (local (min-dist d len)
    ; numero di posizioni temporali confrontabili
    (setq len (min (length path1) (length path2)))
    (if (= len 0)
        nil
        (begin
          (setq min-dist 1e9)
          ; confronta le posizioni con lo stesso indice temporale
          (for (i 0 (- len 1))
            (setq d (dist-manh (path1 i) (path2 i)))
            (if (< d min-dist)
                (setq min-dist d)))
          min-dist))))

(define (dist-max-paths-manh path1 path2)
  (local (max-dist d len)
    ; numero di posizioni temporali confrontabili
    (setq len (min (length path1) (length path2)))
    (if (= len 0)
        nil
        (begin
          (setq max-dist 0)
          ; confronta le posizioni con lo stesso indice temporale
          (for (i 0 (- len 1))
            (setq d (dist-manh (path1 i) (path2 i)))
            (if (> d max-dist)
                (setq max-dist d)))
          max-dist))))

Proviamo:

(setq p1 '((0 0) (1 0) (2 0)))
(setq p2 '((0 3) (1 3) (2 3)))
(dist-min-paths-manh p1 p2)
;-> 3
(dist-max-paths-manh p1 p2)
;-> 3

(setq p1 '((0 0) (1 0) (2 0) (3 0)))
(setq p2 '((0 5) (1 4) (2 2) (3 1)))

(dist-min-paths-manh p1 p2)
;-> 1
(dist-max-paths-manh p1 p2)
;-> 5

Queste due funzioni sono complementari a 'collision-time-manh':
- distanza 0 al tempo t significa collisione temporale;
- distanza minima > 0 significa distanza minima mantenuta tra i due oggetti durante il moto.


-----------------------------------------------------
Generazione casuale di percorsi espansi tra due punti
-----------------------------------------------------

Dato un punto di partenza e il numero di passi, generare un percorso espanso casuale.
Il percorso deve contenere le coordinate di ogni punto in ordine sequenziale di movimento.

Esempio:
  partenza = (0 0)
  numero punti = 4
  percorso casuale = (0 0) (1 0) (1 1) (2 1) (3 1)

La funzione produce un percorso espanso valido:
- ogni coppia di punti consecutivi ha distanza Manhattan 1;
- non contiene salti;
- però può contenere cicli (ad esempio tornare su una cella già visitata), perché la generazione è casuale.

Per generare percorsi casuali senza ritornare mai su celle già visitate (random walk senza loop), dobbiamo usare una variante con controllo delle celle occupate.
Quindi aggiungiamo il parametro 'noloop'.
Comportamento:
- noloop = nil  -> random walk normale, può ripassare su celle già visitate;
- noloop = true -> ogni nuova posizione deve essere diversa da tutte quelle già presenti nel percorso.
Nel caso 'noloop' sia impossibile continuare (ad esempio una griglia chiusa o troppo pochi movimenti disponibili), la funzione termina restituendo il percorso generato fino a quel momento.

(define (random-path-manh start steps noloop)
  (local (path x y moves valid dir nx ny)
    ; coordinate iniziali
    (setq x (start 0))
    (setq y (start 1))
    ; il percorso contiene sempre il punto di partenza
    (setq path (list (list x y)))
    ; genera un passo alla volta
    (for (i 1 steps)
      ; tutte le direzioni possibili
      (setq moves '((1 0) (-1 0) (0 1) (0 -1)))
      ; se richiesto elimina le mosse che creano un ciclo
      (if noloop
          (begin
            (setq valid '())
            (dolist (m moves)
              (setq nx (+ x (m 0)))
              (setq ny (+ y (m 1)))
              (if (not (ref (list nx ny) path))
                  (push m valid -1)))
            (setq moves valid)))
      ; se non ci sono mosse disponibili termina
      (if (null? moves)
          (begin
            (setq i (+ steps 1)))
          (begin
            ; sceglie casualmente una direzione disponibile
            (setq dir (moves (rand (length moves))))
            ; aggiorna la posizione
            (setq x (+ x (dir 0)))
            (setq y (+ y (dir 1)))
            ; aggiunge il nuovo punto al percorso
            (push (list x y) path -1))))
    path))

Proviamo:

Random walk normale:
(random-path-manh '(0 0) 8 nil)
;-> ((0 0) (0 -1) (0 0) (1 0) (1 -1) (1 0) (1 1) (0 1) (1 1))
con possibili ritorni sulle celle già visitate.
(print-grid-manh '((0 0) (0 -1) (0 0) (1 0) (1 -1) (1 0) (1 1) (0 1) (1 1)) '())
;-> * B
;-> A *
;-> * *

Random walk senza loop:
(random-path-manh '(0 0) 8 true)
;-> ((0 0) (1 0) (1 1) (2 1) (3 1) (3 0) (2 0) (2 -1) (3 -1))
Ogni punto compare una sola volta.
(print-grid-manh '((0 0) (1 0) (1 1) (2 1) (3 1) (3 0) (2 0) (2 -1) (3 -1)) '())
;-> * B
;-> A *
;-> * *
;-> nil


------------------------------------------------------------
Generazione casuale di percorsi minimi espansi tra due punti
------------------------------------------------------------

Dati due punti A(x1,y1) e B(x2,y2) calcolare un percorso minimo casuale tra A e B (senza ostacoli).

Per un percorso minimo senza ostacoli basta generare una sequenza casuale di movimenti orizzontali e verticali, mantenendo il numero corretto di passi:

  numero passi = |x2-x1| + |y2-y1|

Calcolo dei delta x e delta y:

  dx = 8
  dy = 6

Creazione dei passi necessari:

  (R R R R R R R R D D D D D D)

Mischiare i passi (con possibile risultato):

  D R R D D R R R D R D R R D

In questo modo ogni percorso prodotto sarà sempre minimo, diretto da A a B e casuale nell'ordine dei passi.

(define (random-minpath-manh A B)
  (local (out x1 y1 x2 y2 dx dy oriz vert mov)
    ; coordinate iniziali e finali
    (setq x1 (A 0))
    (setq y1 (A 1))
    (setq x2 (B 0))
    (setq y2 (B 1))
    ; differenze assolute sugli assi
    (setq dx (abs (- x2 x1)))
    (setq dy (abs (- y2 y1)))
    ; lista finale dei movimenti
    (setq out '())
    (cond
      ; stesso punto: nessun movimento
      ((and (= x1 x2) (= y1 y2))
        out)
      ; solo movimento verticale
      ((= x1 x2)
        (setq vert (if (> y1 y2) 'D 'U))
        (for (i 1 dy)
          (push vert out -1)))
      ; solo movimento orizzontale
      ((= y1 y2)
        (setq oriz (if (> x1 x2) 'L 'R))
        (for (i 1 dx)
          (push oriz out -1)))
      ; movimento misto
      (true
        ; direzione orizzontale
        (setq oriz (if (> x1 x2) 'L 'R))
        ; direzione verticale
        (setq vert (if (> y1 y2) 'D 'U))
        ; crea la lista completa dei passi necessari
        (for (i 1 dx) (push oriz out -1))
        (for (i 1 dy) (push vert out -1))
        ; mischia casualmente l'ordine dei passi
        (setq out (randomize out))))
    out))

Proviamo:

(random-minpath-manh '(0 0) '(5 0))
;-> (R R R R R)
(random-minpath-manh '(0 0) '(0 3))
;-> (U U U)
(random-minpath-manh '(1 2) '(7 4))
;-> (R R U U U R R R)

(setq mp (random-minpath-manh '(-1 2) '(7 -4)))
;-> (R D R D D D R R R R R D R D)

Creazione percorso con coordinate dei punti (che dovrebbe terminare a (7 -4)):
(setq pp (directions-path-manh '(-1 2) mp))
;-> ((-1 2) (0 2) (0 1) (1 1) (1 0) (1 -1) (1 -2) (2 -2) (3 -2)
;->  (4 -2) (5 -2) (6 -2) (6 -3) (7 -3) (7 -4))


------------------
Even and Odd kinds
------------------

https://codegolf.stackexchange.com/questions/253528/even-and-odd-kinds

Sia n un intero positivo. Diciamo che n è pari se la sua scomposizione in fattori primi (contando i duplicati) ha un numero pari di interi.
Ad esempio, 6 = 2 * 3 è pari.
Allo stesso modo, diciamo che n è dispari se la sua scomposizione in fattori primi ha un numero dispari di interi, come ad esempio 18 = 2 * 3 * 3.
Si noti che, poiché la prima scomposizione di 1 non contiene numeri primi, è pari.
Sia E(n) il numero di interi positivi pari minori o uguali a n, e O(n) il numero di interi positivi dispari minori o uguali a n.
Scrivere una funzione più corta possibile che prende un intero positivo n > 0 e restituisce i due valori E(n) e O(n).

Esempio:
  n = 14
  E(14)=6 (1 4 6 9 10 14)
  O(14)=8 (2 3 5 7 8 11 12 13)

 n    =   1     2     3     4     5     6     7     8     9     10         
(E O) = (1 0) (1 1) (1 2) (2 2) (2 3) (3 3) (3 4) (3 5) (4 5) (5 5)
       
 n    =   11    12    13    14    15    16    17    18     19     20  
(E O) =  (5 6) (5 7) (5 8) (6 8) (7 8) (8 8) (8 9) (8 10) (8 11) (8 12) ...

Polya ("Problema di Polya") ha dimostrato che se E(n) <= O(n) è vero per ogni n >= 2, ciò implicherebbe l'ipotesi di Riemann. Ma il controesempio più piccolo è n = 906150257.

(length (factor 1))
;-> 0

(define (EO num)
  (let ((ee 0) (oo 0))
    (for (k 1 num)
      (if (even? (length (factor k))) (++ ee) (++ oo)))
    (list ee oo)))

Proviamo:

(EO 14)
;-> (6 8)

(map EO (sequence 1 20))
;-> ((1 0) (1 1) (1 2) (2 2) (2 3) (3 3) (3 4) (3 5) (4 5) (5 5)
;->  (5 6) (5 7) (5 8) (6 8) (7 8) (8 8) (8 9) (8 10) (8 11) (8 12))

Versione code-golf (86 caratteri):

(define(f n(e 0)(o 0))(for(k 1 n)(if(even?(length(factor k)))(++ e)(++ o)))(list e o))
(f 14)
;-> (6 8)
(map f (sequence 1 20))
;-> ((1 0) (1 1) (1 2) (2 2) (2 3) (3 3) (3 4) (3 5) (4 5) (5 5)
;->  (5 6) (5 7) (5 8) (6 8) (7 8) (8 8) (8 9) (8 10) (8 11) (8 12))


---------------------------------------------
Distanza dei numeri naturali dai numeri primi
---------------------------------------------

Dato un numero naturale N determinare il numero primo più vicino a N.

Sequenza OEIS A051700:
Distance from n to closest prime that is different from n.
  2, 1, 1, 1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 4,
  1, 2, 3, 2, 1, 2, 1, 2, 1, 2, 3, 2, 1, 4, 1, 2, 1, 2, 1, 2, 1, 2, 1, 4,
  1, 2, 3, 2, 1, 6, 1, 2, 3, 2, 1, 2, 1, 2, 1, 2, 3, 2, 1, 4, 1, 2, 1, 2,
  1, 2, 1, 2, 3, 2, 1, 4, 1, 2, 1, 4, 1, 2, 3, 2, 1, 6, 1, 2, 3, 4, 3, 2,
  1, 4, 1, 2, 1, 2, 1, 2, 1, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (closest N)
  (local (succ prec found dist-sx dist-dx)
    (cond ((= N 0) 2) ; perchè 2 è il primo più vicino 0 --> abs(2 - 0) = 2 
          ((= N 1) 1) ; perchè 2 è il primo più vicino 1 --> abs(2 - 1) = 1
          ((= N 2) 1) ; perchè 3 è il primo più vicino 2 --> abs(3 - 2) = 1
          ((= N 3) 1) ; perchè 2 è il primo più vicino 3 --> abs(2 - 3) = 1
          (true
            ; cerca il primo numero primo minore di N
            (setq prec (- N 1))
            (setq found nil)
            (until found
              (if (prime? prec)
                  (setq found true)
                  (-- prec)))
            ; cerca il primo numero primo maggiore di N
            (setq succ (+ N 1))
            (setq found nil)
            (until found              
              (if (prime? succ)
                  (setq found true)
                  (++ succ)))
            ;(println prec { } N { } succ)
            (setq dist-sx (abs (- N prec)))
            (setq dist-dx (abs (- succ N)))
            (if (> dist-sx dist-dx) dist-dx dist-sx)))))

Proviamo:

(closest 53)
;-> 6

(map closest (sequence 0 72))
;-> (2 1 1 1 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 4
;->  1 2 3 2 1 2 1 2 1 2 3 2 1 4 1 2 1 2 1 2 1 2 1 4
;->  1 2 3 2 1 6 1 2 3 2 1 2 1 2 1 2 3 2 1 4 1 2 1 2
;->  1)

============================================================================

