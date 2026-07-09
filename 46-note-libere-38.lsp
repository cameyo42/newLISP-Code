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
