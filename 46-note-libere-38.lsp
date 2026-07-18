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


---------------------
Output = 2 * function
---------------------

Scrivere una funzione (più corta possibile) che restituisce un numero di caratteri pari al doppio della lunghezza della funzione stessa.

Versione code-golf (21 caratteri):

(define(f)(dup 1 10))
(f)
;-> (1 1 1 1 1 1 1 1 1 1)

(length "(define(f)(dup 1 10))")
;-> 21
(length "(1 1 1 1 1 1 1 1 1 1)")
;-> 21


-------------------------------------
Sequenze di primi sommati (p + p + 1)
-------------------------------------

Dato un numero primo P generare la seguente sequenza:

  P(0) = P
  P(1) = P(0) + P(0) + 1, se P(1) è primo
  ...
  P(n) = P(n-1) + P(n-1) + 1, se P(n) è primo 

La sequenza si ferma al primo P(i) non primo.

Esempio:
  P = 2
  P(0) = 2
  P(1) = 2 + 2 + 1 = 5 (primo)
  P(2) = 5 + 5 + 1 = 11 (primo)
  P(3) = 11 + 11 + 1 = 23 (primo)
  P(4) = 23 + 23 + 1 = 47 (primo)
  P(5) = 47 + 47 + 1 = 95 (composto)
  Sequenza finale: seq(2) = 2 5 11 23 47

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

(define (create-seq num)
  (let ((out (list num)) (stop nil))
    (until stop
      (setq num (+ num num 1))
      (if (prime? num)
          (push num out -1)
          (setq stop true)))
    out))

(create-seq 2)
;-> (2 5 11 23 47)

(create-seq 41)
;-> (41 83 167)

(define (sequenze limite)
  (map create-seq (primes-to limite)))

(define (filtra seq op val)
  (filter (fn(x) (op (length x) val)) seq))

Primi fino a 1000: lunghezza massima sequenza = 6
(silent (setq ss (sequenze 1000)))
(filtra ss = 5)
;-> ((2 5 11 23 47) (179 359 719 1439 2879))
(filtra ss = 6)
;-> ((89 179 359 719 1439 2879))
(filtra ss = 7)
;-> ()

Primi fino a 1e7 (10 milioni): lunghezza massima sequenza = 7
(silent (setq ss (sequenze 1e7)))
(filtra ss = 7)
;-> ((1122659 2245319 4490639 8981279 17962559 35925119 71850239)
;->  (2164229 4328459 8656919 17313839 34627679 69255359 138510719)
;->  (2329469 4658939 9317879 18635759 37271519 74543039 149086079))
(filtra ss = 8)
;-> ()


----------------------------------------------------
Ordinamento topologico di un DAG (algoritmo di Kahn)
----------------------------------------------------

Dato un grafo aciclico diretto (DAG - Directed Acyclic Graph) con V vertici ed E archi, trovare un ordinamento topologico valido del grafo.
Ordinamento topologico: è un ordinamento lineare dei vertici tale che per ogni arco diretto u -> v, il vertice u precede v nell'ordinamento.

Esempio 1:

  +---+       +---+       +---+       +---+
  | 0 |------>| 1 |------>| 2 |------>| 3 |
  +---+       +---+       +---+       +---+
                ^           ^
                |           |
  +---+       +---+         |
  | 4 |------>| 5 |---------+
  +---+       +---+

Input: adj = ((1) (2) (3) () (5) (1 2))
Output: (0 4 5 1 2 3)
Per ogni coppia di vertici (u -> v) del grafo, nell'ordinamento u viene prima di v.

Esempio 2:

              +---+       +---+
              | 0 |------>| 1 |
              +---+       +---+
                            ^
                            |
  +---+       +---+       +---+
  | 4 |<------| 3 |------>| 2 |
  +---+       +---+       +---+

Input: adj = ((1) (2) () (2 4) ())
Output: (0 3 1 4 2)
Per ogni coppia di vertici (u -> v) del grafo, nell'ordinamento u viene prima di v.

La lista delle adiacenze 'adj'
'adj' è una lista di liste che rappresenta il grafo tramite una lista di adiacenza (adjacency list).
L'idea è molto semplice:
- la lista principale ha un elemento per ogni vertice;
- l'elemento in posizione 'i' contiene la lista di tutti i vertici raggiungibili con un arco uscente da 'i'.
Nel primo esempio:
    adj = ((1) (2) (3) () (5) (1 2))

Ci sono 6 vertici (0 ... 5).
Ogni elemento ha il seguente significato:

adj[0] = (1)
Dal vertice '0' parte un arco verso '1': 0 --> 1

adj[1] = (2)
Dal vertice '1' parte un arco verso '2': 1 --> 2

adj[2] = (3)
Dal vertice '2' parte un arco verso '3': 2 --> 3

adj[3] = ()
Il vertice '3' non ha archi uscenti: ()

adj[4] = (5)
Dal vertice '4' parte un arco verso '5': 4 --> 5

adj[5] = (1 2)
Dal vertice '5' partono due archi: 5 --> 1 e 5 --> 2

Mettendo insieme tutte le liste si ottiene il grafo:

  0 --> 1 --> 2 --> 3
        ^      ^
        |      |
  4 --> 5 -----+

La struttura è efficiente perchè per conoscere i vicini del vertice 'u' basta leggere adj[u].
Ad esempio:
adj[5] = (1 2), significa immediatamente che i vicini di '5' sono '1' e '2'.
Non è necessario esaminare tutti gli altri vertici del grafo.

Confronto con una matrice di adiacenza
Lo stesso grafo può essere rappresentato anche con una matrice di adicenza 6*6, dove '1' indica la presenza di un arco e '0' la sua assenza:

      0 1 2 3 4 5
    +------------
  0 | 0 1 0 0 0 0
  1 | 0 0 1 0 0 0
  2 | 0 0 0 1 0 0
  3 | 0 0 0 0 0 0
  4 | 0 0 0 0 0 1
  5 | 0 1 1 0 0 0

La lista di adiacenza è però molto più compatta quando il numero di archi è piccolo rispetto al numero massimo possibile (grafi sparsi), ed è per questo che algoritmi come quello di Kahn la utilizzano normalmente.

Algoritmo di Kahn
-----------------
L'algoritmo di Kahn applica la ricerca in ampiezza (BFS) per generare un ordinamento topologico valido.
Calcolare il grado di entrata di ogni vertice, che rappresenta il numero di archi entranti di ogni vertice.
Tutti i vertici con grado di entrata pari a 0 vengono aggiunti a una coda, poiché possono comparire per primi nell'ordinamento.
Rimuovere ripetutamente un vertice dalla coda, aggiungerlo alla lista dei risultati e ridurre il grado di entrata di tutti i suoi vertici adiacenti.
Se uno qualsiasi di questi vertici ha ora un grado di entrata pari a 0, viene aggiunto alla coda.
Questo processo continua finché la coda non è vuota e l'ordinamento risultante rappresenta un ordinamento topologico valido del grafo.

(define (addEdge adj u v)
  ; Aggiunge un arco orientato dal vertice u al vertice v.
  ; La struttura dati "adj" è una lista di adiacenza:
  ;   adj[i] contiene tutti i vertici raggiungibili con
  ;   un arco uscente dal vertice i.
  ; L'istruzione seguente inserisce il vertice v in fondo
  ; alla lista dei vicini del vertice u.
  ; La funzione restituisce la nuova lista di adiacenza,
  ; poiché in newLISP i parametri vengono passati per valore
  ; e quindi il chiamante deve riassegnare il risultato.
  (push v (adj u) -1)
  adj)

; Sort topologico di un DAG (algoritmo di Kahn)
(define (topo-sort adj)
  (local (n indegree res queue top next-node)
    ; Determina il numero di vertici del grafo.
    ; La lista di adiacenza contiene una lista per ogni vertice,
    ; quindi la sua lunghezza coincide con il numero di nodi.
    (setq n (length adj))
    ; Crea il vettore dei gradi entranti (indegree).
    ; L'elemento indegree[i] rappresenta il numero di archi
    ; che arrivano al vertice i. Inizialmente tutti i valori
    ; sono posti a zero e verranno aggiornati analizzando il grafo.
    (setq indegree (dup 0 n))
    ; Lista che conterrà l'ordinamento topologico finale.
    (setq res '())
    ; Coda dei vertici con grado entrante uguale a zero.
    ; I vertici vengono inseriti in fondo (push ... -1)
    ; ed estratti dalla testa (pop), ottenendo una vera FIFO.
    (setq queue '())
    ; Calcola il grado entrante di ogni vertice.
    ; Per ogni arco i -> next-node incrementa il numero
    ; di archi entranti del nodo di destinazione.
    (for (i 0 (- n 1))
      (dolist (next-node (adj i))
        (++ (indegree next-node))))
    ; Inserisce nella coda tutti i vertici che non hanno
    ; archi entranti. Essi possono comparire per primi
    ; nell'ordinamento topologico.
    (for (i 0 (- n 1))
      (if (= (indegree i) 0)
          (push i queue -1)))
    ; Algoritmo di Kahn.
    ; Finché esistono vertici con grado entrante zero:
    ;   1) estrae il primo vertice dalla coda;
    ;   2) lo aggiunge al risultato;
    ;   3) elimina virtualmente tutti i suoi archi uscenti
    ;      decrementando il grado entrante dei relativi vicini;
    ;   4) quando un vicino raggiunge grado entrante zero,
    ;      viene inserito nella coda.
    (while queue
      (setq top (pop queue))
      (push top res -1)
      (dolist (next-node (adj top))
        (-- (indegree next-node))
        (if (= (indegree next-node) 0)
            (push next-node queue -1))))
    ; Restituisce l'ordinamento topologico.
    ; Se il grafo contiene un ciclo, il numero di elementi
    ; della lista restituita sarà minore del numero di vertici.
    res))

Proviamo:

(setq n 6)
(setq adj (dup '() n))
(push 1 (adj 0) -1)
(push 2 (adj 1) -1)
(push 3 (adj 2) -1)
(push 5 (adj 4) -1)
(push 1 (adj 5) -1)
(push 2 (adj 5) -1)
(topo-sort adj)
;-> (0 4 5 1 2 3)

Vedi anche "Sort topologico" su "Note libere 7".


-----------------------------------
Ricerca cicli in un grafo orientato
-----------------------------------

Dato un grafo rappresentato con una lista di adiacenza:
adj = ((1) (2) (3) () (5) (1 2))
il vertice 'i' ha come vicini tutti i nodi presenti in adj[i].
Per verificare se un grafo orientato contiene cicli si può usare una visita DFS (Depth First Search) con tre stati per ogni vertice.
Gli stati possibili sono:
  0 = nodo non ancora visitato
  1 = nodo attualmente nella pila DFS (in elaborazione)
  2 = nodo completamente visitato

Strategia:
Se durante una DFS troviamo un arco verso un nodo con stato '1', significa che stiamo tornando indietro verso un nodo già presente nel percorso corrente.
Quindi esiste un ciclo.

Esempio:

          +---+
          | 1 |
          v   |
  0 ----> 1 --+
          |
          v
          2

  Lista: adj = ((1) (2) (1))
  Visita da '0':
  DFS(0)
  stato: 0 = 1
  vai a 1
  stato: 0 = 1, 1 = 1
  vai a 2
  stato: 0 = 1, 1 = 1, 2 = 1
  da 2 trovi arco verso 1
  1 ha stato = 1
  ciclo trovato
  Il percorso corrente è:  0 -> 1 -> 2
                                ^    |
                                |____|

Esempio:
  Caso senza ciclo: 0 ---> 1 ---> 2 ---> 3
  Lista: adj = ((1) (2) (3) ())
  DFS:
  visita 0  --> stato[0] = 1
  visita 1  --> stato[1] = 1
  visita 2  --> stato[2] = 1
  visita 3  --> stato[3] = 1
  3 terminato  --> stato[3] = 2
  2 terminato  --> stato[2] = 2
  1 terminato  --> stato[1] = 2
  0 terminato  --> stato[0] = 2
  Durante la visita non è mai stato trovato un arco verso un nodo con stato '1'.
  Quindi non esistono cicli.

Pseudocodice:
ciclo(adj):
  stato = [0,0,...,0]
  per ogni nodo v:
      se stato[v] == 0:
          se DFS(v):
              ritorna true
  ritorna nil
DFS(v):
  stato[v] = 1
  per ogni nodo u in adj[v]:
      se stato[u] == 1:
          ritorna true
      se stato[u] == 0:
          se DFS(u):
              ritorna true
  stato[v] = 2
  ritorna nil

Complessità: Tempo: O(V + E), Spazio: O(V)
dove: V = numero di vertici, E = numero di archi

Per un grafo orientato questa è la tecnica standard per il rilevamento dei cicli.
Un'altra possibilità equivalente è usare l'algoritmo di Kahn: se l'ordinamento topologico non riesce a estrarre tutti i vertici, allora il grafo contiene almeno un ciclo.

(define (has-cycle adj)
  (local (n state cycle)
    ; Numero dei vertici del grafo
    (setq n (length adj))
    ; Stato dei vertici:
    ; 0 = mai visitato
    ; 1 = nodo attualmente nel percorso DFS
    ; 2 = nodo completamente analizzato
    (setq state (dup 0 n))
    ; Variabile che indica se è stato trovato un ciclo
    (setq cycle nil)
    ; Avvia una DFS da ogni componente non ancora visitata
    (for (i 0 (- n 1))
      (if (and (= (state i) 0) (not cycle))
          (setq cycle (dfs-cycle adj state i))))
    cycle))

(define (dfs-cycle adj state v)
  (local (cycle)
    ; Il nodo corrente entra nella pila della DFS
    (setf (state v) 1)
    (setq cycle nil)
    ; Esamina tutti gli archi uscenti da v
    (dolist (u (adj v))
      (if (not cycle)
          (cond
            ; Troviamo un arco verso un nodo già
            ; presente nel percorso corrente:
            ; quindi esiste un ciclo orientato
            ((= (state u) 1)
              (setq cycle true))
            ; Continua la visita se il nodo non è stato visitato
            ((= (state u) 0)
              (if (dfs-cycle adj state u)
                  (setq cycle true))))))
    ; La visita del nodo è terminata
    ; quindi viene marcato come completato
    (setf (state v) 2)
    cycle))

Proviamo:
(setq adj1 '((1) (2) (3) ()))
(has-cycle adj1)
;-> nil
Grafo:
0 ---> 1 ---> 2 ---> 3

(setq adj2 '((1) (2) (3) (1)))
(has-cycle adj2)
;-> true
Grafo:
0 ---> 1 ---> 2 ---> 3
              ^      |
              |______|

(setq adj3 '((1) (2) (1) ()))
(has-cycle adj3)
;-> true
Grafo:
0 ---> 1 ---> 2
       ^      |
       |______|


------------------------------
Ricerca dei percorsi in un DAG
------------------------------

Dato un DAG (Directed Acyclic Graph) determinare tutti i percorsi

Algoritmo: DFS (Depth First Search) con backtracking.

Un percorso valido è una sequenza di vertici tale che ogni coppia consecutiva è collegata da un arco orientato.
L'algoritmo si divide in due fasi.

1) Ricerca delle sorgenti
Per ogni vertice viene calcolato il grado entrante (indegree), cioè il numero di archi che arrivano al vertice.
Tutti i vertici con grado entrante uguale a zero sono sorgenti.
Poiche' un DAG puo' avere una o piu' sorgenti, la ricerca dei percorsi deve iniziare da ognuna di esse.

2) DFS con backtracking
Per ogni sorgente viene eseguita una visita DFS.
Durante la visita viene mantenuta una lista chiamata "path", che contiene il percorso corrente.
Ogni volta che si entra in un nodo:
- il nodo viene aggiunto in fondo a "path";
- se il nodo è un pozzo (non ha archi uscenti), una copia di "path" viene salvata nella lista dei risultati;
- altrimenti si visitano ricorsivamente tutti i figli.
Quando tutti i figli sono stati visitati, il nodo viene eliminato da "path".
Questa operazione prende il nome di backtracking e permette di riutilizzare la stessa lista per esplorare gli altri rami del grafo.

Ad esempio, nel grafo:

          0
         / \
        1   2
         \ /
          3

la visita procede cosi':

  path = ()
  entra in 0
  path = (0)
  
  entra in 1
  path = (0 1)
  
  entra in 3
  path = (0 1 3)
  
  3 è un pozzo
  salva (0 1 3)
  
  backtracking
  path = (0 1)
  
  backtracking
  path = (0)
  
  entra in 2
  path = (0 2)
  
  entra in 3
  path = (0 2 3)
  
  3 è un pozzo
  salva (0 2 3)
  
  backtracking
  path = (0 2)
  
  backtracking
  path = (0)
  
  backtracking
  path = ()

Alla fine sono stati trovati tutti i percorsi:
  (0 1 3)
  (0 2 3)

Poichè il grafo è aciclico, ogni visita termina sempre in un pozzo.
Non è quindi necessario mantenere una lista dei nodi visitati o controllare la presenza di cicli.

Complessità
-----------
  V = numero di vertici
  E = numero di archi
  P = numero di percorsi
Il calcolo dei gradi entranti richiede O(V + E).
La DFS visita ogni arco quando esplora un percorso, ma il tempo totale dipende dal numero dei percorsi presenti nel DAG.
Nel caso peggiore il numero dei percorsi può crescere in modo esponenziale rispetto al numero dei vertici.
Tempo  = O(somma delle lunghezze di tutti i percorsi)
Spazio = O(profondita' massima della DFS)

(define (dag-paths adj)
; Scope dinamico: 'path' e 'out' sono locali a 'dag-paths',
; ma sono visibili a 'dfs-paths' senza dover essere passati come parametri.
  (local (n indegree sources path out)
    ; Numero di vertici del DAG
    (setq n (length adj))
    ; Calcola il grado entrante di ogni vertice
    (setq indegree (dup 0 n))
    (for (i 0 (- n 1))
      (dolist (v (adj i))
        (++ (indegree v))))
    ; Individua tutte le sorgenti (grado entrante = 0)
    (setq sources '())
    (for (i 0 (- n 1))
      (if (= (indegree i) 0)
          (push i sources -1)))
    ; Variabili condivise dalla DFS
    (setq path '())
    (setq out '())
    ; Avvia una DFS da ogni sorgente
    (dolist (s sources)
      (dfs-paths adj s))
    ; Restituisce tutti i percorsi trovati
    out))

(define (dfs-paths adj node)
  ; Aggiunge il nodo corrente al percorso
  (push node path -1)
  ; Se il nodo è un pozzo salva una copia del percorso
  (if (null? (adj node))
      ;(push (copy path) out -1)
      (push path out -1)
      ; Altrimenti visita ricorsivamente tutti i figli
      (dolist (v (adj node))
        (dfs-paths adj v)))
  ; Backtracking: rimuove l'ultimo nodo del percorso
  (pop path -1))

Proviamo:

Grafo:
      0
     / \
    1   2
     \ /
      3
(setq adj '((1 2) (3) (3) ()))
(dag-paths adj)
;-> ((0 1 3) (0 2 3))

Grafo:
0 ---> 2 ---> 4
1 ---> 3 ---> 4
(setq adj '((2) (3) (4) (4) ()))
(dag-paths adj)
;-> ((0 2 4) (1 3 4))

Grafo:
      0
     / \
    1   2
   / \   \
  3   4   |
   \ /    |
    5 <---+
(setq adj '((1 2) (3 4) (4) (5) (5) ()))
(dag-paths adj)
;-> ((0 1 3 5) (0 1 4 5) (0 2 4 5))


----------------------------------------------
Numeri somma di tre cubi (teorema di Legendre)
----------------------------------------------

Dato un numero non negativo N determinare se può essere espresso come somma di 3 quadrati:

  N = x^2 + y^2 + z^2

Dato un numero non negativo N il teorema di Legendre (three-square theorem) afferma che N può essere espresso come somma di 3 quadrati N = x^2 + y^2 + z^2 se e solo se risulta: N = 4^a*(8*b+7).
Per verificare questo bisogna controllare se, dopo aver tolto tutti i fattori '4', il numero rimanente è congruo a '7 mod 8'.

In altre parole la condizione:

 N = 4^a * (8*b + 7)

significa:

1) dividere N per 4 finché è possibile;
2) chiamare il risultato M;
3) controllare se M mod 8 = 7.
Se è vero, N non è rappresentabile come somma di tre quadrati.
Se è falso, N è rappresentabile.

Esempi:

  N = 28
  28 / 4 = 7
  M = 7
  7 mod 8 = 7
  quindi:
  28 = 4^1 * (8*0 + 7)
  NON rappresentabile

  N = 12
  12 / 4 = 3
  M = 3
  3 mod 8 = 3
  quindi non è della forma proibita
  rappresentabile: 12 = 2^2 + 2^2 + 2^2

(define (legendre3? n)
  (while (= (% n 4) 0)
    (setq n (/ n 4)))
  (!= (% n 8) 7))

Proviamo:

(legendre3? 28)
;-> nil
; 28 = 4 * 7 = 4^1 * (8*0+7)
(legendre3? 12)
;-> true
; 12 = 2^2 + 2^2 + 2^2
(legendre3? 7)
;-> nil
(legendre3? 100)
;-> true
; 100 = 10^2 + 0^2 + 0^2
(legendre3? 1145141919810)
;-> true
; 1145141919810 = 1070113^2 + 295^2 + 4^2
(legendre3? 245657627368729)
;-> true
; Rappresentabile come somma di 2 quadrati
; 245657627368729 = 15384573^2 + 2995420^2 (+ 0^2).
(legendre3? 12345678987654321)
;-> True
; numero quadrato
; 12345678987654321 = 111111111^2 (+ 0^2 + 0^2)

; big-integer (works well)
(legendre3? 199070263454016156300L)
;-> true
; 199070263454016156300 = 1234567890^2 + 9876543210^2 + 9999999990^2
(legendre3? 185724285729475816451975L)
;-> nil

La complessità è O(log N), perché il ciclo elimina al massimo un fattore 4 ad ogni iterazione.

Un altro metodo è il seguente:

(define (legendre3 n)
  ; La funzione verifica il teorema dei tre quadrati di Legendre.
  ; Restituisce true se n può essere scritto come:
  ;     n = x^2 + y^2 + z^2
  ; e nil se n è della forma proibita:
  ;     n = 4^a * (8*b + 7)
  ; Il metodo evita di dividere ripetutamente n per 4.
  ; Usa il fatto che:
  ;     c = n & (-n)
  ; estrae il massimo divisore di 2 contenuto in n, cioè
  ; la massima potenza di 2 che divide n.
  ; Esempio:
  ;     n = 40 = 101000 (binario)
  ;     c = 8  = 001000
  ; quindi:
  ;     c = 2^k
  ; Se k è pari, allora c è una potenza di 4:
  ;     c = 4^a
  ; Se k è dispari, allora c contiene un fattore 2 aggiuntivo:
  ;     c = 2 * 4^a
  ; Per distinguere i due casi si usa la proprietà:
  ;     4^a mod 3 = 1
  ; mentre:
  ;     2 * 4^a mod 3 = 2
  ; Quindi:
  ;     c mod 3 = 1
  ; indica che c è esattamente una potenza di 4.
  ; Solo in questo caso bisogna controllare la condizione
  ; proibita di Legendre.
  ; Dopo aver eliminato la potenza di 4 rimane:
  ;     m = n / c
  ; Il teorema dice che il numero non è rappresentabile
  ; se e solo se:
  ;     m = 8*b + 7
  ; cioè:
  ;     m mod 8 = 7
  ; Se c contiene un fattore 2 dispari, il numero è sempre
  ; rappresentabile perché dopo la divisione per 4 rimarrebbe
  ; un numero pari e quindi non può essere congruo a 7 modulo 8.
  ; Caso particolare:
  ; n = 0 è rappresentabile come:
  ;     0 = 0^2 + 0^2 + 0^2
  ; e viene gestito prima per evitare la divisione per zero.
  (if (zero? n)
      true
      ;else
      (let ((c (& n (- n))))
        (not (and (= (% c 3) 1)
                  (= (% (/ n c) 8) 7))))))

Proviamo:

(legendre3 28)
;-> nil
(legendre3 12)
;-> true
(legendre3 7)
;-> nil
(legendre3 100)
;-> true
(legendre3 1145141919810)
;-> true
(legendre3 245657627368729)
;-> true
(legendre3 12345678987654321)
;-> True

; big-integer don't works
(legendre3 199070263454016156300L)
;-> ERR: bigint type not applicable
(legendre3 185724285729475816451975L)
;-> ERR: bigint type not applicable

Test di velocità:
(time (legendre3 12345678987654321) 1e6)
;-> 313.336
(time (legendre3? 12345678987654321) 1e6)
;-> 146.635

Sequenza OEIS A000378:
Sums of three squares: numbers of the form x^2 + y^2 + z^2.
  0, 1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 19, 20, 21, 22,
  24, 25, 26, 27, 29, 30, 32, 33, 34, 35, 36, 37, 38, 40, 41, 42, 43, 44,
  45, 46, 48, 49, 50, 51, 52, 53, 54, 56, 57, 58, 59, 61, 62, 64, 65, 66,
  67, 68, 69, 70, 72, 73, 74, 75, 76, 77, 78, 80, 81, 82, 83, ...

(filter legendre3 (sequence 1 83))
;-> (1 2 3 4 5 6 8 9 10 11 12 13 14 16 17 18 19 20 21 22
;->  24 25 26 27 29 30 32 33 34 35 36 37 38 40 41 42 43 44
;->  45 46 48 49 50 51 52 53 54 56 57 58 59 61 62 64 65 66
;->  67 68 69 70 72 73 74 75 76 77 78 80 81 82 83)

Sequenza OEIS A004215:
Numbers that are the sum of 4 but no fewer nonzero squares.
  7, 15, 23, 28, 31, 39, 47, 55, 60, 63, 71, 79, 87, 92, 95, 103, 111, 112,
  119, 124, 127, 135, 143, 151, 156, 159, 167, 175, 183, 188, 191, 199, 207,
  215, 220, 223, 231, 239, 240, 247, 252, 255, 263, 271, 279, 284, 287, 295,
  303, 311, 316, 319, 327, 335, 343, ...

(clean legendre3 (sequence 1 343))
;-> (7 15 23 28 31 39 47 55 60 63 71 79 87 92 95 103 111 112
;->  119 124 127 135 143 151 156 159 167 175 183 188 191 199 207
;->  215 220 223 231 239 240 247 252 255 263 271 279 284 287 295
;->  303 311 316 319 327 335 343)


------------------------
Gestione tabelle di dati
------------------------

Data una lista di liste che rappresenta una tabella scrivere una funzione che calcola una nuova colonna utilizzando una funzione che agisce sulle colonne predefinite di ogni riga.

Esempio:

          | 1 10 20 "a" |
Tabella = | 2 11 22 "b" |
          | 3 12 77 "c" |  

(setq t '((1 10 20 "a") (2 11 22 "b") (3 12 77 "c")))

Funzione = f(x y) (x + y)

(define (f x y) (+ x y))

Lista degli indici delle colonne = (1 2)

(setq cc '(1 2))

Funzione da definire:

  (calc table func cols)
  dove: 'table' è la tabella da utilizzare
        'func' è la funzione che opera con le colonne
        'cols' è una lista degli indici delle colonne da utilizzare

L'esecuzione della funzione 'calc':

  (calc t f cc)

genera una nuova tabella con una colonna in più:

          | 1 10 20 "a" 30|
Tabella = | 2 11 22 "b" 33|
          | 3 12 77 "c" 89|

Come si è formata la nuova colonna:

  riga 0: (col 1) + (col 2) = 30
  riga 1: (col 1) + (col 2) = 33
  riga 2: (col 1) + (col 2) = 89

(define (calc table func cols)
  (local (col-values new-value)
    ; Ciclo per ogni riga della tabella...
    (dolist (el table)
      ; estrae dalla riga corrente i valori delle colonne
      ; i cui indici sono specificati in 'cols'
      (setq col-values (select el cols))
      ; calcola il nuovo valore applicando la funzione 'func' ai
      ; valori della colonna
      (setq new-value (apply func col-values))
      ; inserisce il nuovo valore in fondo alla riga corrente
      (push new-value (table $idx) -1))
    table))

(setq t (calc t f '(1 2)))
;-> ((1 10 20 "a" 30) (2 11 22 "b" 33) (3 12 77 "c" 89))

; Add a row to a matrix/table
(define (add-row matrix values row)
  (push values matrix row))

; Add a column to a matrix/table
(define (add-col matrix values col)
  (dolist (row matrix)
    (push (values $idx) (matrix $idx) col))
  matrix)

; Remove a row from a matrix/table
(define (remove-row matrix row)
  (pop matrix row)
  matrix)

; Remove a column from a matrix/table
(define (remove-col matrix col)
  (setq matrix (transpose matrix))
  (pop matrix col)
  (transpose matrix))

Proviamo:

(setq t (remove-col t -1))
;-> ((1 10 20 "a") (2 11 22 "b") (3 12 77 "c"))

(setq t (add-row t '(0 5 6 "k") 0))
;-> ((0 5 6 "k") (1 10 20 "a") (2 11 22 "b") (3 12 77 "c"))

(setq t (add-col t '("q" "w" "e" "r") -1))
;-> ((0 5 6 "k" "q") (1 10 20 "a" "w") (2 11 22 "b" "e") (3 12 77 "c" "r"))

(define (g x y) (string x y))
(calc t g '(3 4))
;-> ((0 5 6 "k" "q" "kq")
;->  (1 10 20 "a" "w" "aw")
;->  (2 11 22 "b" "e" "be")
;->  (3 12 77 "c" "r" "cr"))


----------------------------------------
Stringhe conformi a pattern di caratteri
----------------------------------------

Data una stringa e un pattern di caratteri, verificare se i caratteri nella stringa seguono lo stesso ordine determinato dai caratteri del pattern.
Nel pattern non ci sono caratteri duplicati.

Esempi:

  stringa = "newlisp"
  pattern = "ew"
  output  = true (perchè nella stringa il carattere 'e' precede sempre 'w')

  stringa = "newlisp is not new"
  pattern = "ew"
  output  = nil (perchè nella stringa la seconda 'e' precede la prima 'w')
  Ovvero, la 'e' di "new" si trova dopo la 'w' di "newlisp".


Metodo 1
--------
Per ogni coppia (x, y) di caratteri consecutivi del pattern, cerchiamo l'ultima occorrenza di x e la prima occorrenza di y nella stringa di input.
Se l'ultima occorrenza di x è successiva alla prima occorrenza di y per una qualsiasi coppia, restituiamo nil.
Controllare ogni coppia di caratteri consecutivi nella stringa del pattern è sufficiente.
Infatti, presi tre caratteri consecutivi nel pattern (x, y e z) se (x, y) e (y, z) restituiscono true, ciò implica che anche (x, z) è true.

; funzione che cerca una sottostringa in una stringa
; partendo dal fondo della stringa (da destra).
; Restituisce l'indice dell'inizio della sottostringa nella stringa (o nil)
(define (find-rev findThis inThis)
  (- (length inThis) (find (reverse findThis) (reverse inThis)) (length findThis)))

(define (check-pattern1 str pat)
  (local (len-s len-p out x y ultimo primo)
    ; numero di caratteri della stringa
    (setq len-s (length str))
    ; numero di caratteri del pattern
    (setq len-p (length pat))
    ; se il pattern è più lungo della stringa...
    (if (> len-p len-s)
        ; allora il risultato vale nil
        (setq out true)
        ;else
        (begin
          (setq out nil)
          ; Ciclo per ogni coppia di caratteri del pattern...
          (for (i 0 (- len-p 2) 1 out)
            ; x e y sono una coppia di caratteri adiacenti del pattern
            (setq x (pat i))
            (setq y (pat (+ i 1)))
            ; trova l'indice dell'ultima occorrenza del carattere x nella stringa
            (setq ultimo (find-rev x str))
            ; trova l'indice della prima occorrenza del carattere y nella stringa
            (setq primo (find y str))
            ;(print (list x y primo ultimo)) (read-line)
            ; restituisce nil se x o y non sono presenti nella stringa
            ; OPPURE l'ultima occorrenza di x è successiva alla
            ; prima occorrenza di y nella stringa
            (when (or (nil? primo) (nil? ultimo) (> ultimo primo))
                ;(println (list x y primo ultimo))
                (setq out true)))))
    ; restituisce la negazione di 'out' dall'uscita del 'for'
    (not out)))

Proviamo:

(check-pattern1 "newlisp" "ew")
;-> true
(check-pattern1 "newlisp is not new" "ew")
;-> ("e" "w" 2 16)
;-> nil
(check-pattern1 "engineers rock" "er")
;-> true
(check-pattern1 "engineers rock" "egr")
;-> ("e" "g" 2 6)
;-> nil
(check-pattern1 "engineers rock" "gsr")
;-> ("s" "r" 7 8)
;-> nil
(check-pattern1 "bfbaeadeacc" "bac")
;-> true

Metodo 2
--------
Riduzione della stringa al pattern.
Per ogni carattere del pattern, manteniamo nella stringa solo i caratteri corrispondenti.
Nella nuova stringa, eliminiamo i caratteri ripetuti consecutivamente.
Se la stringa finale è uguale al pattern restituiamo true, altrimenti nil.

Esempio
  stringa = "bfbaeadeacc"
  pattern = "bac"
  1) Rimozione dei caratteri extra dalla stringa
     (cio quei caratteri che non sono presenti nel pattern)
     str = "bbaaacc"   ('f', 'e' e 'd' sono stati rimossi)
  2) Rimozione dei caratteri ripetuti consecutivi
     str = "bac"
  3) Se la stringa finale è uguale al pattern restituiamo true, altrimenti nil.

Nella funzione i passi 1) e 2) vengono raggruppati ed effettuati con un solo ciclo.

(define (check-pattern2 str pat)
  (local (new-str ch)
    (setq new-str "")
    ; Ciclo per ogni carattere della stringa...
    (dostring (c str)
      (setq ch (char c))
      ; se il carattere corrente si trova in pat 
      ; ed è diverso dall'ultimo carattere di 'new-str',
      ; allora lo aggiunge a 'new-str'
      (if (and (find ch pat) (!= ch (new-str -1))) (extend new-str ch)))
    ;(println new-str)
    ; verifica se 'new-str' e 'pat' sono uguali
    (= new-str pat)))

Proviamo:

(check-pattern2 "newlisp" "ew")
;-> ew
;-> true
(check-pattern2 "newlisp is not new" "ew")
;-> ewew
;-> nil
(check-pattern2 "engineers rock" "er")
;-> er
;-> true
(check-pattern2 "engineers rock" "egr")
;-> eger
;-> nil
(check-pattern2 "engineers rock" "gsr")
;-> grsr
;-> nil
(check-pattern2 "bfbaeadeacc" "bac")
;-> bac
;-> true

Metodo 3
--------
Assegniamo un'ordine numerico crescente ai caratteri del pattern.
Ad esempio, il pattern "ew" viene ordinato come segue:
  "e" => 1
  "w" => 2
Questo significa che tutte le "e" devono venire prima di tutte le "w".
  (tenendo traccia dell'ordine corrente
Attraversiamo la stringa (tenendo traccia dell'ordine corrente 'cur-ord'):
  Se il carattere corrente si trova nel pattern:
    - calcolare il suo ordine 'ord'
    - se 'ord' > 'cur-ord', allora restituiamo nil
    Altrimenti, aggiorniamo 'cur-ord' con 'ord'.
Se tutti i caratteri rispettano l'ordine, restituiamo true.

(define (check-pattern3 str pat)
  (local (cur-ord out ch ord)
    (setq out nil)
    ; ordine corrente
    (setq cur-ord 0)
    ; Ciclo per ogni carattere...
    (dostring (c str out)
      ; carattere corrente
      (setq ch (char c))
      ; ordine/indice del carattere corrente nel pattern
      (setq ord (find ch pat))
      ; se il carattere corrente esiste nel pattern
      (if ord
        ; se l'ordine del carattere corrente è minore dell'ordine corrente
        (if (< ord cur-ord)
            (begin 
              ; esce dal ciclo con risultato 'nil'
              (setq out true)
              ;(println (list ch ord cur-ord)))
            ; altrimenti aggiorna l'ordine corrente con 
            ; l'ordine del carattere corrente
            (setq cur-ord ord))))
    (not out)))

Proviamo:

(check-pattern3 "newlisp" "ew")
;-> true
(check-pattern "newlisp is not new" "ew")
;-> ("e" "w" 2 16)
;-> nil
(check-pattern3 "engineers rock" "er")
;-> true
(check-pattern3 "engineers rock" "egr")
;-> ("e" 0 1)
;-> nil
(check-pattern3 "engineers rock" "gsr")
;-> ("s" 1 2)
;-> nil
(check-pattern3 "bfbaeadeacc" "bac")
;-> true

Test di velocità

(setq t "bdjjdbsdgfjbdfjghjdfgbdsgfhasdhadskfsdkladskglhsdgadfkghgfcsdkjsdkc")
(= (check-pattern1 t "bac") (check-pattern2 t "bac") (check-pattern3 t "bac"))
;-> true

(time (check-pattern1 t "bac") 1e5)
;-> 196.054
(time (check-pattern2 t "bac") 1e5)
;-> 1452.137
(time (check-pattern3 t "bac") 1e5)
;-> 1382.842


------------------------------------------
Rettangolo massimo in una matrice booleana
------------------------------------------

Data una matrice booleana MxN, determinare il rettangolo di 0/1 più grande.

Esempio:
matrice = 1 0 0 0 1 0 0
          0 1 0 0 1 1 0
          1 1 0 0 0 0 0
          1 0 1 0 0 0 0
          1 0 0 1 0 1 1
rettangolo massimo di 0 = 0 0 0 0 (ultimi 4 elementi della riga 2)
                          0 0 0 0 (ultimi 4 elementi della riga 3)
rettangolo massimo di 1 = 1 1 (primi 2 elementi della riga 2)
                          1 1 (primi 2 elementi della riga 3)
                          1 1 (primi 2 elementi della riga 4)

Il metodo è lo stesso per il problema del 'massimo rettangolo in un istogramma'.

Metodo (con 0)
--------------
Si elaborano le righe una alla volta.
Per ogni colonna si mantiene l'altezza consecutiva di zeri terminante nella riga corrente.
Esempio:
  0 0 1 0
  0 0 0 0
  1 0 0 0
le altezze diventano
  riga 0 -> 1 1 0 1
  riga 1 -> 2 2 1 2
  riga 2 -> 0 3 2 3
Ogni vettore di altezze è un istogramma.
A questo punto basta trovare il rettangolo massimo dell'istogramma mediante una pila monotona in O(N).
Il massimo tra tutti gli istogrammi è il rettangolo cercato.

Esempio:
  1 0 0 0 1 0 0
  0 1 0 0 1 1 0
  1 1 0 0 0 0 0
  1 0 1 0 0 0 0
  1 0 0 1 0 1 1
  Altezze
  0 1 1 1 0 1 1
  1 0 2 2 0 0 2
  0 0 3 3 1 1 3
  0 1 0 4 2 2 4
  0 2 1 0 3 0 0
  Alla terza riga (indice 2) l'istogramma è
  0 0 3 3 1 1 3
  Il massimo rettangolo dell'istogramma è quello formato dalle colonne 3..6 (indici 3..6) con altezza 2:
  0 0 0 0
  0 0 0 0
  area = 2 × 4 = 8

Durante il calcolo si possono ricavare:
- area massima
- larghezza
- altezza
- riga superiore
- riga inferiore
- colonna sinistra
- colonna destra
quindi è possibile ricostruire il rettangolo massimo.

L'algoritmo visita ongi cella della mtrice una sola volta.
Tempo: O(M*N)
Memoria: O(N)

Algoritmo
1) aggiornare il vettore delle altezze degli zeri;
2) per ogni riga risolvere il problema del massimo rettangolo nell'istogramma tramite una pila monotona.
Quando un elemento viene estratto dalla pila si conoscono:
- altezza h
- larghezza w
- area a = h*w
e anche le coordinate del rettangolo.
Se la riga corrente è 'r', allora:
  riga-bassa = r
  riga-alta  = r - h + 1
  colonna-destra  = i - 1
  colonna-sinistra = stack-top + 1
dove 'i' è la colonna corrente (quella che ha provocato l'estrazione dalla pila).
Quindi, quando si trova un rettangolo più grande, basta memorizzare:
  area
  (riga-alta colonna-sinistra)
  (riga-bassa colonna-destra)

La funzione restituisce: (area (x1 y1) (x2 y2))
  x1 = colonna-sinistra
  y1 = riga-bassa
  x2 = colonna-destra
  y2 = riga-alta

(define (max-rect value matrix)
  (local (rows cols h stack best-area best-top best-left best-bottom best-right)
    ; numero di righe della matrice
    (setq rows (length matrix))
    ; numero di colonne della matrice
    (setq cols (length (matrix 0)))
    ; vettore delle altezze degli zeri consecutivi
    (setq h (dup 0 cols))
    ; inizializza il rettangolo massimo
    (setq best-area 0)
    (setq best-top 0)
    (setq best-left 0)
    (setq best-bottom 0)
    (setq best-right 0)
    ; analizza ogni riga della matrice
    (for (r 0 (- rows 1))
      ; aggiorna le altezze degli zeri consecutivi
      ; se l'elemento corrente vale 0 incrementa l'altezza,
      ; altrimenti la azzera
      (for (c 0 (- cols 1))
        (if (= ((matrix r) c) value)
            (++ (h c))
            (setf (h c) 0)))
      ; crea una pila monotona di indici di colonne
      ; le altezze corrispondenti risultano sempre crescenti
      (setq stack '())
      ; visita tutte le colonne più una colonna fittizia finale
      ; di altezza zero per svuotare completamente la pila
      (for (i 0 cols)
        ; altezza corrente dell'istogramma
        (let (curr (if (= i cols) 0 (h i)))
          ; estrae tutti gli elementi più alti dell'altezza corrente
          ; ogni estrazione individua il rettangolo massimo avente
          ; come altezza quella della barra estratta
          (while (and stack (> (h (last stack)) curr))
            (letn ((idx (pop stack -1))
                   (height (h idx))
                   (left (if stack (+ (last stack) 1) 0))
                   (right (- i 1))
                   (width (+ (- right left) 1))
                   (area (* height width)))
              ; se il rettangolo trovato è il più grande,
              ; memorizza area e coordinate
              (if (> area best-area)
                  (begin
                    (setq best-area area)
                    (setq best-top (+ (- r height) 1))
                    (setq best-bottom r)
                    (setq best-left left)
                    (setq best-right right)))))
          ; inserisce la colonna corrente nella pila
          (push i stack -1))))
    ; restituisce:
    ; area massima
    ; coordinata (riga colonna) dell'angolo superiore sinistro
    ; coordinata (riga colonna) dell'angolo inferiore destro
    (list best-area
          (list best-top best-left)
          (list best-bottom best-right))))

Proviamo:

(setq mt '((1 0 0 0 1 0 0)
           (0 1 0 0 1 1 0)
           (1 1 0 0 0 0 0)
           (1 1 1 0 0 0 0)
           (1 1 0 1 0 1 1)))

(max-rect 0 mt)
;-> (8 (2 3) (3 6))
(max-rect 1 mt)
;-> (6 (2 0) (4 1))

(setq mt '((0 0 0 0 0 0 0)
           (0 0 0 0 0 1 0)
           (1 1 1 1 0 0 0)
           (1 1 1 1 0 0 0)
           (1 1 1 1 0 1 1)))

(max-rect 0 mt)
;-> (10 (0 0) (1 4))
(max-rect 1 mt)
;-> (12 (2 0) (4 3))


--------------------------
Somma X nel lancio di dadi
--------------------------

Dati N dadi, ciascuno con M facce, numerate da 1 a M, trovare il numero di modi per ottenere una data somma x.
x è la somma dei valori su ciascuna faccia quando tutti i dadi vengono lanciati.
Abbiamo N dadi con il numero di facce non necessariamente uguale.
Ogni dado ha un numero per ogni faccia.
VOgliamo trovare in quanti e quali modi possiamo ottenere una data somma x.
x è la somma dei valori su ciascuna faccia quando tutti i dadi vengono lanciati.

Un dado viene rappresentato con una lista che contiene i valori delle facce.

(define (cartesian)
"Calculate the cartesian product of lists"
  (let (out '(()) )
    (dolist (lst (args))
      (let (tmp '())
        (dolist (parz out)
          (dolist (el lst)
            (push (append parz (list el)) tmp -1)))
        (setq out tmp)))
    out))

(cartesian '(1 2 3) '(4 5) '(6 7))
;-> ((1 4 6) (1 4 7) (1 5 6) (1 5 7) (2 4 6) (2 4 7) (2 5 6) (2 5 7)
;->  (3 4 6) (3 4 7) (3 5 6) (3 5 7))

(define (modi x lst)
  (local (all somme))
    ; calcola tutti le possibili combinazioni di lanci
    (setq all (apply cartesian lst))
    ; calcola la somma di ogni lancio
    (setq somme (sort (map (fn(x) (apply + x)) all)))
    ;(println somme)
    ; conta quante volte x compare nella lista delle somme
    (first (count (list x) somme)))

Proviamo:

(modi 12 (dup '(1 2 3 4 5 6) 3))
;-> 25
(modi 2 (dup '(1 2 3 4 5 6) 3))
;-> 0
(modi 6 '((1 4 5) (1 2 3) (1 1 2)))
;-> 3
(modi 6 '((1 4 5) (1 2 3) (1 1 2)))


----------------------------------------
Coppie di elementi uguali in una matrice
----------------------------------------

Data una matrice MxN trovare per ogni elemento il numero di coppie di elementi uguali.
Le coppie devono essere univoche, cioè ogni elemento può essere usato solo una volta.

Esempio:

          | 1 3 5 |
matrice = | 3 1 6 |
          | 3 1 1 |

Coppie: (1 2) --> il numero 1 compare 4 volte --> 2 coppie
        (3 1) --> il numero 3 compare 3 volte --> 1 coppia

(setq mt '((1 2 3 4 5 6)
           (1 2 3 4 5 6)
           (1 2 3 4 5 6)
           (7 8 9 1 2 3)))

Metodo 1 (Lista associativa)
----------------------------

(define (find-pair1 matrix)
  (let ((alst '()) (lst (flat matrix)))
    (dolist (el lst)
      (if (lookup el alst)
          ; se elemento esiste, allora aumenta il numero di occorrenze
          (++ (lookup el alst))
          ;else
          ; altrimenti inserisce l'elemento nella lista associativa 'alst'
          ; con numero di occorrenze pari a 1
          (push (list el 1) alst -1)))
    (setq alst (filter (fn(x) (!= (x 1) 1)) alst))
    (map (fn(x) (list (x 0) (/ (x 1) 2))) alst)))

(find-pair1 mt)
;-> ((1 1) (2 1) (3 1) (4 1) (5 1) (6 1))

Metodo 2 (primitive di newLISP)
-------------------------------

(define (find-pair2 matrix)
  (letn ((lst (flat matrix))
         (unici (unique lst))
         (conteggio (map (fn(x y) (list x y)) unici (count unici lst))))
    (setq conteggio (filter (fn(x) (!= (x 1) 1)) conteggio))
    (map (fn(x) (list (x 0) (/ (x 1) 2))) conteggio)))

(find-pair2 mt)
;-> ((1 1) (2 1) (3 1) (4 1) (5 1) (6 1))

Metodo 3 (ciclo for)
--------------------

(define (find-pair3 matrix)
  (local (lst out val conta)
    (setq lst (sort (flat matrix)))
    (setq out '())
    (setq val (lst 0))
    (setq conta 0)
    (dolist (el lst)
      (if (= el val)
          (++ conta)
          (begin
            (if (>= conta 2)
                (push (list val (/ conta 2)) out -1))
            (setq val el)
            (setq conta 1))))
    (if (>= conta 2)
        (push (list val (/ conta 2)) out -1))
    out))

(setq mt '((1 2 3 4 5 6)
           (1 2 3 4 5 6)
           (1 2 3 4 5 6)
           (7 8 9 1 2 3)))

(find-pair3 mt)
;-> ((1 2) (2 2) (3 2) (4 1) (5 1) (6 1))

Test di velocità

(setq t (map list (rand 1000 100) (rand 1000 100)))
(length t)
;-> 100
(= (length (find-pair1 t)) (length (find-pair2 t)) (length (find-pair3 t)))
;-> true
(= (sort (find-pair1 t)) (sort (find-pair2 t)) (sort (find-pair3 t)))
;-> true

(time (find-pair1 t) 1e4)
;-> 1485.159
(time (find-pair2 t) 1e4)
;-> 1344.292
(time (find-pair3 t) 1e4)
;-> 407.933


-------------------------------------
Sottoliste con elementi tutti diversi
-------------------------------------

Data una lista dividerla in sottoliste consecutive in cui ognuma ha tutti elementi diversi.

Esempio:
lista = (1 4 5 5 5 6 2 3 3 8)
sottoliste consecutive con elementi diversi = (1 4 5) (5) (5 6 2 3) (3 8)

(define (break-unique lst)
  (local (len out palo cur)
    (setq len (length lst))
    (cond 
      ((= len 0) '())
      ((= len 1) (list lst))
      (true
        (setq out '()) ; lista soluzione
        (setq palo (lst 0)) ; palo
        (setq cur (list palo)) ; sottolista corrente
        (dolist (el (rest lst))
          (cond ((= palo el) ; elemento uguale al precedente
                  ; inserisce la sottolista corrente nella lista soluzione
                  (push cur out -1)
                   ; aggiorna il valore del palo con elemento corrente
                  (setq palo el)
                  ; initializza una nuova sottolista (sottolista corrente)
                  (setq cur (list palo)))
                (true ; elemento diverso dal precedente
                  ; aggiunge l'elemento corrente alla sottolista corrente
                  (push el cur -1)
                  ; aggiorna il valore del palo con l'elemento corrente                  
                  (setq palo el))))
        (push cur out -1)
        out))))

Proviamo:

(break-unique '())
;-> ()
(break-unique '(2))
;-> ((2))
(break-unique '(1 2))
;-> ((1 2))

(setq L '(1 1 1 1 1))
(break-unique L)
;-> ((1) (1) (1) (1) (1))

(setq L '(1 4 5 5 5 6 2 3 3 8))
(break-unique L)
;-> ((1 4 5) (5) (5 6 2 3) (3 8))

(setq L '(1 4 5 5 5 6 2 3 3 8 8))
(break-unique L)
;-> ((1 4 5) (5) (5 6 2 3) (3 8) (8))


----------------------------------
Aggiungere 1 ad una lista di cifre
----------------------------------

Data una lista di cifre che rappresentano un numero intero, scrivere una funzione che aggiunge 1 al numero rappresentato dalla lista e restituisce una lista con le cifre del nuovo numero.

Esempi:
  lista = (2 5 1)
  output --> 251 + 1 = 252 --> (2 5 2)

  lista = (9 9 9)
  output --> 999 + 1 = 1000 --> (1 0 0 0)

Metodo 1 (somma con riporto - carry)
------------------------------------

(define (add-one1 lst)
  (let ((carry 1) (sum 0) (out '()))
    (for (i (- (length lst) 1) 0 -1)
      (setq sum (+ carry (lst i)))
      (setq carry (/ sum 10))
      (push (% sum 10) out)
    )
    (if (> carry 0) (push carry out))
    out))


(add-one1 '(1 2 3 4))
;-> (1 2 3 5)

(add-one1 '(9 8 8 9))
;-> (9 8 9 0)

(add-one1 '(9 9 9 9))
;-> (1 0 0 0 0)

(add-one1 '(0))
;-> (1)

(add-one1 '(0 1))
;-> (0 2)

Metodo 2 (controllo cifre con valore 9)
---------------------------------------

(define (add-one2 lst)
    ; inizializza l'indice corrente alla fine della lista
    (let (idx (- (length lst) 1))
      ; finchè l'indice corrente è valido e il valore relativo vale 9
      (while (and (>= idx 0) (= (lst idx) 9))
        ; inserisce uno 0 all'indice corrente
        (setf (lst idx) 0)
        (-- idx))
      ; se (indice < 0), allora le cifre della lista erano tutti 9
      (if (< idx 0)
        ; inserisce un 1 all'inizio della lista
        (push 1 lst)
        ; altrimenti
        ; incrementa il valore della lista relativo all'indice corrente
        (++ (lst idx)))
      lst))

(add-one2 '(1 2 3 4))
;-> (1 2 3 5)

(add-one2 '(9 8 8 9))
;-> (9 8 9 0)

(add-one2 '(9 9 9 9))
;-> (1 0 0 0 0)

(add-one2 '(0))
;-> (1)

(add-one2 '(0 1))
;-> (0 2)

Test di velocità

(setq t (rand 10 10))
(= (add-one1 t) (add-one2 t))
;-> true
(time (add-one1 t) 1e5)
;-> 193.823
(time (add-one2 t) 1e5)
;-> 51.09

(setq t (rand 10 100))
(= (add-one1 t) (add-one2 t))
;-> true
(time (add-one1 t) 1e4)
;-> 197.784
(time (add-one2 t) 1e4)
;-> 16.083

(setq t (rand 10 1000))
(= (add-one1 t) (add-one2 t))
;-> true
(time (add-one1 t) 1e3)
;-> 615.413
(time (add-one2 t) 1e3)
;-> 17.047


--------------------------
Abaco giapponese (Soroban)
--------------------------

"The Japanese Abacus: Its Use and Theory" by Takashi Kojima
"Advanced Abacus: Japanese Theory and Practice" by Takashi Kojima

Rappresentazione grafica delle cifre (in colonna) in un soroban:

+---+   +---+   +---+   +---+   +---+   +---+   +---+   +---+   +---+   +---+
| ■ |   | ■ |   | ■ |   | ■ |   | ■ |   |   |   |   |   |   |   |   |   |   |
|   |   |   |   |   |   |   |   |   |   | ■ |   | ■ |   | ■ |   | ■ |   | ■ |
+---+   +---+   +---+   +---+   +---+   +---+   +---+   +---+   +---+   +---+
|   |   | ■ |   | ■ |   | ■ |   | ■ |   |   |   | ■ |   | ■ |   | ■ |   | ■ |
| ■ |   |   |   | ■ |   | ■ |   | ■ |   | ■ |   |   |   | ■ |   | ■ |   | ■ |
| ■ |   | ■ |   |   |   | ■ |   | ■ |   | ■ |   | ■ |   |   |   | ■ |   | ■ |
| ■ |   | ■ |   | ■ |   |   |   | ■ |   | ■ |   | ■ |   | ■ |   |   |   | ■ |
| ■ |   | ■ |   | ■ |   | ■ |   |   |   | ■ |   | ■ |   | ■ |   | ■ |   |   |
+---+   +---+   +---+   +---+   +---+   +---+   +---+   +---+   +---+   +---+
  0       1       2       3       4       5       6       7       8       9

Rappresentazione grafica di un soroban con tutte le cifre a 0:

Colonna:   0   1   2   3   4   5   6   7   8   9   10  11
         +---+---+---+---+---+---+---+---+---+---+---+---+
         | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ |
         |   |   |   |   |   |   |   |   |   |   |   |   |
         +---+---+---+---+---+---+---+---+---+---+---+---+
         |   |   |   |   |   |   |   |   |   |   |   |   |
         | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ |
         | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ |
         | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ |
         | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ |
         +---+---+---+---+---+---+---+---+---+---+---+---+
 Valore:   0   0   0   0   0   0   0   0   0   0   0   0

Rappresentazione di un soroban (matrice 7 x 12) con tutte cifre a 0:

  1 1 1 1 1 1 1 1 1 1 1 1
  0 0 0 0 0 0 0 0 0 0 0 0
  0 0 0 0 0 0 0 0 0 0 0 0
  1 1 1 1 1 1 1 1 1 1 1 1
  1 1 1 1 1 1 1 1 1 1 1 1
  1 1 1 1 1 1 1 1 1 1 1 1
  1 1 1 1 1 1 1 1 1 1 1 1

; Crea un nuovo soroban
(define (new-soroban num-cifre)
  ; lista delle rappresentazioni delle cifre (per righe)
  ; (Variabile globale)
  (setq DIGITS '((1 0 0 1 1 1 1)   ; 0
                 (1 0 1 0 1 1 1)   ; 1
                 (1 0 1 1 0 1 1)   ; 2
                 (1 0 1 1 1 0 1)   ; 3
                 (1 0 1 1 1 1 0)   ; 4
                 (0 1 0 1 1 1 1)   ; 5
                 (0 1 1 0 1 1 1)   ; 6
                 (0 1 1 1 0 1 1)   ; 7
                 (0 1 1 1 1 0 1)   ; 8
                 (0 1 1 1 1 1 0))) ; 9
  ; crea un soroban con tutti i valori a 0
  (transpose (dup '(1 0 0 1 1 1 1) num-cifre)))

; Stampa un soroban
(define (print-soroban matrix ch0 ch1)
  (letn ( (row (length matrix))
          (col (length (first matrix)))
          (line (string (dup "+---" col) "+")) )
    (setq ch0 (or ch0 "0"))
    (setq ch1 (or ch1 "1"))
    ; stampa del soroban
    (println line)
    (for (i 0 (- row 1))
      (if (= i 2) (println line))
      (for (j 0 (- col 1))
        ;(print "| " (matrix i j) " "))
        (if (= (matrix i j) 0)
          (print "| " ch0 " ")
          (print "| " ch1 " ")))
        (println "|"))
    (println line)
    ; stampa delle cifre rappresentate nel soroban
    (setq tr (transpose matrix))
    (for (i 0 (- col 1))
      (print "  " (find (tr i) DIGITS) " "))
    (println) '>))

; Scrive una cifra in un sokoban in una data posizione (colonna)
(define (write-digit digit matrix pos)
    (if (>= pos (length (matrix 0)))
        (println "Error: overflow")
        ;else
        (for (i 0 6) 
          (setf (matrix i pos) (DIGITS digit i))))
  matrix)

; Scrive un numero in un sokoban partendo da una data posizione (colonna)
; (se start-pos == nil, inserisce il numero tutto a destra)
; (se il numero ha troppe cifre, il sokoban non viene modificato)
(define (write-number num matrix start-pos)
  (let ((len (length num)) (str (string num)))
    (setq start-pos (or start-pos (- (length (matrix 0)) len)))
    (if (> (+ len start-pos) (length (matrix 0)))
        (println "Error: overflow")
        ;else
        (for (i 0 (- len 1))
          (setq matrix (write-digit (int (str i)) matrix start-pos))
          (++ start-pos)))
    matrix))

Proviamo:

(setq abaco (new-soroban 10))
(print-soroban abaco)
;-> +---+---+---+---+---+---+---+---+---+---+
;-> | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
;-> | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
;-> +---+---+---+---+---+---+---+---+---+---+
;-> | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
;-> | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
;-> | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
;-> | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
;-> | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
;-> +---+---+---+---+---+---+---+---+---+---+
;->   0   0   0   0   0   0   0   0   0   0
(print-soroban abaco " " "■")
;-> +---+---+---+---+---+---+---+---+---+---+
;-> | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ |
;-> |   |   |   |   |   |   |   |   |   |   |
;-> +---+---+---+---+---+---+---+---+---+---+
;-> |   |   |   |   |   |   |   |   |   |   |
;-> | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ |
;-> | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ |
;-> | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ |
;-> | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ |
;-> +---+---+---+---+---+---+---+---+---+---+
;->   0   0   0   0   0   0   0   0   0   0
(for (k 0 7) (setq abaco (write-digit k abaco k)))
(print-soroban abaco " " "■")
(setq abaco (write-digit 8 abaco 8))
(setq abaco (write-digit 9 abaco 9))
(print-soroban abaco " " "■")
;-> +---+---+---+---+---+---+---+---+---+---+
;-> | ■ | ■ | ■ | ■ | ■ |   |   |   |   |   |
;-> |   |   |   |   |   | ■ | ■ | ■ | ■ | ■ |
;-> +---+---+---+---+---+---+---+---+---+---+
;-> |   | ■ | ■ | ■ | ■ |   | ■ | ■ | ■ | ■ |
;-> | ■ |   | ■ | ■ | ■ | ■ |   | ■ | ■ | ■ |
;-> | ■ | ■ |   | ■ | ■ | ■ | ■ |   | ■ | ■ |
;-> | ■ | ■ | ■ |   | ■ | ■ | ■ | ■ |   | ■ |
;-> | ■ | ■ | ■ | ■ |   | ■ | ■ | ■ | ■ |   |
;-> +---+---+---+---+---+---+---+---+---+---+
;->   0   1   2   3   4   5   6   7   8   9
(setq abaco (write-number 6666 abaco))
;-> +---+---+---+---+---+---+---+---+---+---+
;-> | ■ | ■ | ■ | ■ | ■ |   |   |   |   |   |
;-> |   |   |   |   |   | ■ | ■ | ■ | ■ | ■ |
;-> +---+---+---+---+---+---+---+---+---+---+
;-> |   | ■ | ■ | ■ | ■ |   | ■ | ■ | ■ | ■ |
;-> | ■ |   | ■ | ■ | ■ | ■ |   |   |   |   |
;-> | ■ | ■ |   | ■ | ■ | ■ | ■ | ■ | ■ | ■ |
;-> | ■ | ■ | ■ |   | ■ | ■ | ■ | ■ | ■ | ■ |
;-> | ■ | ■ | ■ | ■ |   | ■ | ■ | ■ | ■ | ■ |
;-> +---+---+---+---+---+---+---+---+---+---+
;->   0   1   2   3   4   5   6   6   6   6
(print-soroban abaco " " "■")
(setq abaco (write-number 6666 abaco 0))
(print-soroban abaco " " "■")
;-> +---+---+---+---+---+---+---+---+---+---+
;-> |   |   |   |   | ■ |   |   |   |   |   |
;-> | ■ | ■ | ■ | ■ |   | ■ | ■ | ■ | ■ | ■ |
;-> +---+---+---+---+---+---+---+---+---+---+
;-> | ■ | ■ | ■ | ■ | ■ |   | ■ | ■ | ■ | ■ |
;-> |   |   |   |   | ■ | ■ |   |   |   |   |
;-> | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ |
;-> | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ |
;-> | ■ | ■ | ■ | ■ |   | ■ | ■ | ■ | ■ | ■ |
;-> +---+---+---+---+---+---+---+---+---+---+
;->   6   6   6   6   4   5   6   6   6   6
(setq abaco (write-number 6666 abaco 8))
;-> Error: overflow
(print-soroban abaco " " "■")
;-> +---+---+---+---+---+---+---+---+---+---+
;-> |   |   |   |   | ■ |   |   |   |   |   |
;-> | ■ | ■ | ■ | ■ |   | ■ | ■ | ■ | ■ | ■ |
;-> +---+---+---+---+---+---+---+---+---+---+
;-> | ■ | ■ | ■ | ■ | ■ |   | ■ | ■ | ■ | ■ |
;-> |   |   |   |   | ■ | ■ |   |   |   |   |
;-> | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ |
;-> | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ |
;-> | ■ | ■ | ■ | ■ |   | ■ | ■ | ■ | ■ | ■ |
;-> +---+---+---+---+---+---+---+---+---+---+
;->   6   6   6   6   4   5   6   6   6   6


----------------
Flash calculator
----------------

'Flash calculator' è un gioco per esercitare il calcolo mentale nella somma di numeri.
Il gioco consiste nel presentare un elenco di numeri visualizzandoli uno alla volta per poco tempo.
Alla fine dell'elenco il giocatore deve calcolare la somma di tutti i numeri visualizzati.

La funzione del gioco utilizza 3 parametri:
1) numeri --> numero di numeri visualizzati
2) cifre  --> numero di cifre dei numeri visualizzati
3) tempo  --> tempo di visualizzazione di ogni numero (msec)

(define (flash numeri cifre tempo)
  (local (min-val max-val nums somma res)
    ; clear screen of REPL (ANSI sequence)
    (print "\027[H\027[2J")
    ; valore minimo dei numeri
    (setq min-val (pow 10 (- cifre 1)))
    ; valore minimo dei numeri
    (setq max-val (- (pow 10 cifre) 1))
    ; crea la lista dei numeri da visualizzare (numeri random)
    (setq nums (collect (+ min-val (rand (+ (- max-val min-val) 1))) numeri))
    ; somma dei numeri
    (setq somma (apply + nums))
    ;(println somma { } nums)
    (println "*************************************")
    (println "******    FLASH CALCULATOR     ******")
    (println "*************************************\n")
    (println "--- Press a key to start the game ---")
    (read-key)
    ; ciclo di visualizzazione dei numeri
    (dolist (el nums)
      (print "\027[H\027[2J")
      (print el)
      (sleep tempo))
    (print "\027[H\027[2J")
    ; input utente della somma dei numeri
    (print "Somma totale = ")
    (setq start (time-of-day))
    (setq res (int (read-line)))
    (setq elapsed (- (time-of-day) start))
    ; controllo del risultato
    (if (= res somma)
        (println "BRAVO !!!")
        (println "Errore: Somma totale = " somma))
    (println "Tempo di risposta = " elapsed " (millisec)") '>))

Proviamo:

; 5 numeri da 1 cifra con 1 secondo di tempo per ogni numero
(flash 5 1 1000)

; 3 numeri da 2 cifre con 1 secondo di tempo per ogni numero
(flash 3 2 1000)


-------------------
Sempre 123 (DENEAT)
-------------------

Iniziare con un numero intero positivo qualsiasi, poi scrivere il numero di cifre pari seguito dal numero di cifre dispari e infine il numero totale di cifre.
Dopo un numero finito di ripetizioni il risultato sarà 123.
Gli eventuali zeri iniziali vengono omessi.

Esempio:
  N = 3454
  numero di cifre pari = 2 (4 4)
  numero di cifre dispari = 2 (3 5)
  lunghezza numero = 4
  N --> 224
  numero di cifre pari = 3 (2 2 4)
  numero di cifre dispari = 0
  lunghezza numero = 3
  N --> 303
  numero di cifre pari = 1 (0)
  numero di cifre dispari = 2 (3 3)
  lunghezza numero = 3
  N --> 123
  
DENEAT(n): concatenate number of even digits in n, number of odd digits and total number of digits.
(Digits: Even, Not Even, And Total). Leading zeros are then omitted.

Sequenza OEIS A073053:
Apply DENEAT operator (or the Sisyphus function) to n.
  101, 11, 101, 11, 101, 11, 101, 11, 101, 11, 112, 22, 112, 22, 112, 22, 112,
  22, 112, 22, 202, 112, 202, 112, 202, 112, 202, 112, 202, 112, 112, 22, 112,
  22, 112, 22, 112, 22, 112, 22, 202, 112, 202, 112, 202, 112, 202, 112, 202,
  112, 112, 22, 112, 22, ...

Sequenza OEIS A073054:
Number of applications of DENEAT operator x -> A073053(x) needed to transform n to 123.
  2, 5, 2, 5, 2, 5, 2, 5, 2, 5, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 3, 2, 3, 2,
  3, 2, 3, 2, 3, 2, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 3, 2, 3, 2, 3, 2, 3, 2,
  3, 2, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 3, 2, 3, 2, 3, 2, 3, 2, 3, 2, 2, 4,
  2, 4, 2, 4, 2, 4, 2, 4, 3, 2, 3, 2, 3, 2, 3, 2, 3, 2, 2, 4, 2, 4, 2, 4,
  2, 4, 2, 4,...

(define (deneat num)
  (let ( (str (string num)) (len (length num)) (pari 0) (dispari 0) )
    (for (i 0 (- len 1))
      (if (even? (int (str i)))
        (++ pari)
        (++ dispari)))
    (if (zero? pari)
        (setq str (string dispari len))
        (setq str (string pari dispari len)))
    (int str)))

(map deneat (sequence 0 53))
;-> (101 11 101 11 101 11 101 11 101 11 112 22 112 22 112 22 112
;->  22 112 22 202 112 202 112 202 112 202 112 202 112 112 22 112
;->  22 112 22 112 22 112 22 202 112 202 112 202 112 202 112 202
;->  112 112 22 112 22)

(define (seq-deneat num show)
  (if (= num 123) 0
      ;else
      (let (conta 0)
        (while (!= num 123)
          (setq num (deneat num))
          (if show (println num))
          (++ conta))
        conta)))

Proviamo:

(seq-deneat 3454 true)
;-> 224
;-> 303
;-> 123
;-> 3

(seq-deneat 123456789 true)
;-> 459
;-> 123
;-> 2

(map seq-deneat (sequence 0 72))
;-> (2 5 2 5 2 5 2 5 2 5 2 4 2 4 2 4 2 4 2 4 3 2 3 2
;->  3 2 3 2 3 2 2 4 2 4 2 4 2 4 2 4 3 2 3 2 3 2 3 2
;->  3 2 2 4 2 4 2 4 2 4 2 4 3 2 3 2 3 2 3 2 3 2 2 4
;->  2)

Generalizziamo la funzionde 'deneat' inserendo un parametro che specifica l'ordine di costruzione del numero.
Usiamo una lista i cui elementi determina l'ordine di inserimento.
Gli elementi della lista sono E (Even), O (Odd) e L (Length).
Esempi:
  (E O L) --> Even + Odd + Length
  (L O E) --> Length + Odd + Even

(define (make num order)
  (let ( (str (string num)) (len (length num)) (pari 0) (dispari 0) )
    (for (i 0 (- len 1))
      (if (even? (int (str i)))
        (++ pari)
        (++ dispari)))
    (setq str "")
    (dolist (el order)
      (cond ((= el 'E) (extend str (string pari)))
            ((= el 'O) (extend str (string dispari)))
            ((= el 'L) (extend str (string len)))))
    (int str 0 10)))

; Even - Odd - Length (deneat)
(map (fn(x) (make x '(E O L))) (sequence 0 25))
;-> (101 11 101 11 101 11 101 11 101 11 112 22 112 22 112 22 112 22
;->  112 22 202 112 202 112 202 112)

; Odd - Even - Length
(map (fn(x) (make x '(O E L))) (sequence 0 25))
;-> (11 101 11 101 11 101 11 101 11 101 112 202 112 202 112 202 112
;->  202 112 202 22 112 22 112 22 112)

; Length - Even - Odd
(map (fn(x) (make x '(L E O))) (sequence 0 25))
;-> (110 101 110 101 110 101 110 101 110 101 211 202 211 202 211 202
;->  211 202 211 202 220 211 220 211 220 211)

; Length - Odd- Even
(map (fn(x) (make x '(L O E))) (sequence 0 25))
;-> (101 110 101 110 101 110 101 110 101 110 211 220 211 220 211 220 211
;->  220 211 220 202 211 202 211 202 211)

Adesso la sequenza che generiamo applicando ripetutamente 'make' dipende dall'ordine predefinito quindi non è detto che finisca sempre a 123 come 'deneat'.
Anzi, la sequenza potrebbe anche entrare in un ciclo.

; Genera i numeri sequenziali della sequenza scelta
; (fino alla prima ripetizione di un numero)
(define (seq-make num order)
  (let (conta 0)
    (setq out '())
    ; ciclo finchè 'make' genera numeri nuovi...
    (until (find num out)
      ;(println num)
      (push num out -1)
      (setq num (make num order))
      (++ conta))
    (push num out -1)
    (list conta out)))

Numero con 2 cifre pari e 2 cifre dispari:
; Termina a 123
(seq-make 3454 '(E O L))
;-> (4 (3454 224 303 123 123))
; Oscilla tra 33 e 202
(seq-make 3454 '(O E L))
;-> (4 (3454 224 33 202 33))
; Termina a 312
(seq-make 3454 '(L E O))
;-> (4 (3454 422 330 312 312))
; Termina a 321
(seq-make 3454 '(L O E))
;-> (4 (3454 422 303 321 321))

Numero con 1 cifra pari e 3 cifre dispari:
; Termina a 123
(seq-make 3451 '(E O L))
;-> (3 (3451 134 123 123))
; Termina a 213
(seq-make 3451 '(O E L))
;-> (3 (3451 314 213 213))
; Termina a 312
(seq-make 3451 '(L E O))
;-> (3 (3451 413 312 312))
; Termina a 321
(seq-make 3451 '(L O E))
;-> (3 (3451 431 321 321))

Numero con 3 cifre pari e 1 cifra dispari:
; Termina a 123
(seq-make 3428 '(E O L))
;-> (3 (3428 314 123 123))
; Termina a 213
(seq-make 3428 '(O E L))
;-> (3 (3428 134 213 213))
; Termina a 312
(seq-make 3428 '(L E O))
;-> (3 (3428 431 312 312))
; Termina a 321
(seq-make 3428 '(L O E))
;-> (3 (3428 413 321 321))

============================================================================

