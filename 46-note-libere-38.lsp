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

============================================================================

