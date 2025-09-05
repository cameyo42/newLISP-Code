================

 NOTE LIBERE 32

================

    "Cuius rei demonstrationem mirabilem sane detexi
     hanc marginis exiguitas non caperet." Pierre de Fermat

-----------------------------------
Somma dei numeri che non dividono N
-----------------------------------

Sequenza OEIS A024816:
Antisigma(n): Sum of the numbers less than n that do not divide n.
  0, 0, 2, 3, 9, 9, 20, 21, 32, 37, 54, 50, 77, 81, 96, 105, 135, 132, 170,
  168, 199, 217, 252, 240, 294, 309, 338, 350, 405, 393, 464, 465, 513, 541,
  582, 575, 665, 681, 724, 730, 819, 807, 902, 906, 957, 1009, 1080, 1052,
  1168, 1182, 1254, 1280, 1377, 1365, ...

(define (factor-group num)
"Factorize an integer number"
  (if (= num 1) '((1 1))
    (letn ( (fattori (factor num))
            (unici (unique fattori)) )
      (transpose (list unici (count unici fattori))))))

(define (divisors num)
"Generate all the divisors of an integer number"
  (local (f out)
    (cond ((= num 1) '(1))
          (true
           (setq f (factor-group num))
           (setq out '())
           (divisors-aux 0 1)
           (sort out)))))
; auxiliary function
(define (divisors-aux cur-index cur-divisor)
  (cond ((= cur-index (length f))
         (push cur-divisor out -1)
        )
        (true
         (for (i 0 (f cur-index 1))
           (divisors-aux (+ cur-index 1) cur-divisor)
           (setq cur-divisor (* cur-divisor (f cur-index 0)))))))

Funzione 1
----------
(define (seq1 num)
  (- (apply + (sequence 1 num)) (apply + (divisors num))))

(map seq1 (sequence 1 54))
;-> (0 0 2 3 9 9 20 21 32 37 54 50 77 81 96 105 135 132 170
;->  168 199 217 252 240 294 309 338 350 405 393 464 465 513 541
;->  582 575 665 681 724 730 819 807 902 906 957 1009 1080 1052
;->  1168 1182 1254 1280 1377 1365)

Funzione 2
----------
(define (seq2 num)
  (apply + (filter (fn(x) (!= (% num x) 0)) (sequence 1 num))))

(map seq2 (sequence 1 54))
;-> (0 0 2 3 9 9 20 21 32 37 54 50 77 81 96 105 135 132 170
;->  168 199 217 252 240 294 309 338 350 405 393 464 465 513 541
;->  582 575 665 681 724 730 819 807 902 906 957 1009 1080 1052
;->  1168 1182 1254 1280 1377 1365)

Test di correttezza:

(= (map seq1 (sequence 1 100)) (map seq2 (sequence 1 100)))
;-> true

Test di velocità:

(time (map seq1 (sequence 1 100)) 1e3)
;-> 469.732

(time (map seq2 (sequence 1 100)) 1e3)
;-> 989.143


-------------------------------------------------------------
Numeri divisibili dalla somma e dal prodotto delle loro cifre
-------------------------------------------------------------

Sequenza OEIS A038186:
Numbers divisible by the sum and product of their digits.
  1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 24, 36, 111, 112, 132, 135, 144, 216,
  224, 312, 315, 432, 612, 624, 735, 1116, 1212, 1296, 1332, 1344, 1416,
  2112, 2232, 2916, 3132, 3168, 3276, 3312, 4112, 4224, 6624, 6912, 8112,
  9612, 11112, 11115, 11133, 11172, 11232, ...

(define (check? num)
  (let ((prod 1) (sum 0) (val num))
    (while (!= val 0)
      ; somma delle cifre
      (setq sum (+ sum (% val 10)))
      ; prodotto delle cifre
      (setq prod (* prod (% val 10)))
      (setq val (/ val 10))
    )
    (and (!= prod 0) (!= sum 0)
         (zero? (% num prod)) (zero? (% num sum)))))

(filter check? (sequence 1 11232))
;-> (1 2 3 4 5 6 7 8 9 12 24 36 111 112 132 135 144 216
;->  224 312 315 432 612 624 735 1116 1212 1296 1332 1344 1416
;->  2112 2232 2916 3132 3168 3276 3312 4112 4224 6624 6912 8112
;->  9612 11112 11115 11133 11172 11232)


---------------------------------------
Somma alternata dei numeri di una lista
---------------------------------------

Data una lista di numeri sommare i numeri a coppie:
- il primo con l'ultimo
- il secondo con il penultimo
- ecc.
Se la lista ha un numero dispari di elementi l'ultima coppia da sommare è l'elemento centrale con se stesso.
La funzione deve essere la più corta possibile.

Esempi:
lista = (1 2 7 3 4 1)
output = (2 6 10) dove 2 = 1 + 1, 6 = 2 + 4, 10 = 7 + 3

lista = (1 2 7 8 3 4 1)
output = (2 6 10 16), dove 16 = 8 + 8

Versione 1:

(define (somma1 lst)
  (letn ( (len (length lst)) (meta (/ len 2)) (out '()) )
    (setq out (map + (slice lst 0 meta) (reverse (slice lst (- meta)))))
    (if (odd? len) (push (+ (lst meta) (lst meta)) out -1))
    out))

(somma1 '(1 2 7 3 4 1))
;-> (2 6 10)
(somma1 '(1 2 7 8 3 4 1))
;-> (2 6 10 16)
(somma1 '(1 2))
;-> (3)
(somma1 '(5))
;-> (10)

Versione 1 code-golf (131 caratteri):

(define(s1 a)
(letn((l(length a))(m(/ l 2)))
(setq o(map +(slice a 0 m)(reverse(slice a(- m)))))
(if(odd? l)(push(+(a m)(a m))o -1))o))

(s1 '(1 2 7 3 4 1))
;-> (2 6 10)
(s1 '(1 2 7 8 3 4 1))
;-> (2 6 10 16)
(s1 '(1 2))
;-> (3)
(s1 '(5))
;-> 10

Versione 2:

(define(somma2 lst)
  (letn ( (len (length lst)) (max-idx (- (ceil (div len 2)) 1)) (out '()) )
    (for (i 0 max-idx)
      (push (+ (lst i) (lst (- len i 1))) out -1))))

(somma2 '(1 2 7 3 4 1))
;-> (2 6 10)
(somma2 '(1 2 7 8 3 4 1))
;-> (2 6 10 16)
(somma2 '(1 2))
;-> (3)
(somma2 '(5))
;-> 10

Versione 2 code-golf (106 caratteri):

(define(s2 a)
(letn((l(length a))(x(-(ceil(div l 2))1))(o '()))
(for(i 0 x)(push(+(a i)(a(- l i 1)))o -1))))

(s2 '(1 2 7 3 4 1))
;-> (2 6 10)
(s2 '(1 2 7 8 3 4 1))
;-> (2 6 10 16)
(s2 '(1 2))
;-> (3)
(s2 '(5))
;-> 10


-----------------------------------------------------------------------
Print the internal format of the source of a function with line numbers
-----------------------------------------------------------------------

Scrivere una funzione che stampa il formato interno del sorgente (con numeri di linea) di una funzione.

(define (print-source func)
  (let (line (parse (source 'func) "\r\n"))
    ; toglie gli ultimi due \r\n
    (pop line -1) (pop line -1)
    ; stampa le linee del sorgente con numeri di linea
    (map (fn(x) (println (format "%3d " (+ $idx 1)) x)) line) '>))

(define (somma a b)
  (if (> a b)
    (+ a b b)
    ;else
    (+ a a b)))

(print-source somma)
;-> 1 (define (func a b)
;-> 2   (if (> a b)
;-> 3    (+ a b b)
;-> 4    (+ a a b)))

(print-source print-source)
;-> 1 (define (func func)
;-> 2   (let (line (parse (source 'func) "\r\n"))
;-> 3    (pop line -1)
;-> 4    (pop line -1)
;-> 5    (map (lambda (x) (println (format "%3d " (+ $idx 1)) x)) line) '>))


----------------------------------------
Quante partite a scacchi sono possibili?
----------------------------------------

Negli scacchi una mossa significa che sia il Bianco che il Nero hanno spostato un pezzo.
In altre parole, una sola mossa del Bianco o del Nero viene chiamata 'ply' (semimossa).
Occorrono un ply del Bianco e un ply del Nero per fare una mossa completa.

Il primo scienziato a fare una stima del numero di partite di scacchi possibili è stato Claude Shannon.
Nel suo articolo del 1950 "Programming a Computer for Playing Chess" (Programmazione di un computer per giocare a scacchi), Shannon ha presentato un calcolo per il limite inferiore della complessità dell'albero di gioco degli scacchi, che ha portato a circa 10^120 partite possibili, per dimostrare l'impraticabilità di risolvere gli scacchi con la forza bruta.

Il calcolo considera che per ogni posizione esistono, in media, 30 ply (semimosse) legali possibili.
e che una partita, in media, dura 40 mosse (80 ply).
In questo modo il numero di partite possibili vale: 30^80

Per ogni posizione esistono, in media, 30 ply (semimosse) legali possibili.
Quindi il numero di partite possibili vale: 30^80

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(** 30 80)
;-> 147808829414345923316083210206383297601000000000000000000000
;-> 00000000000000000000000000000000000000000000000000000000000L

Funzione che converte una potenza a^b in 10^x (calcola x):
(define (power10 base exponent) (div (mul exponent (log base)) (log 10)))

(power10 30 80)
;-> 118.169700377573

Quindi 30^80 ~= 10^118.

Nota: gli atomi Universo osservabile sono circa 10^80.

Shannon ha anche stimato il numero di possibili posizioni:

  64!/(32!*8!^2*2!^6) ~= 10^43

Questo valore include alcune posizioni illegali.

Sequenza OEIS A048987:
Number of possible chess games at the end of the n-th ply.
  1, 20, 400, 8902, 197281, 4865609, 119060324, 3195901860, 84998978956,
  2439530234167, 69352859712417, 2097651003696806, 62854969236701747,
  1981066775000396239, 61885021521585529237, 2015099950053364471960, ...
Does not include games which end in fewer than n plies.
According to the laws of chess, the "50-move rule" and "draw by 3-fold repetition" do not prevent infinite games because they require an appeal by one of the players, but the "75-move rule" introduced on Jul 01 2014 is automatic and makes chess finite.

Sequenza OEIS A079485:
Number of chess games that end in checkmate after exactly n plies.
  0, 0, 0, 0, 8, 347, 10828, 435767, 9852036, 400191963, 8790619155,
  362290010907, 8361091858959, 346742245764219, ...


----------------------------------------------
Struttura dati Disjoint Set Union (Find-Union)
----------------------------------------------

La struttura dati Find-Union (chiamata anche Disjoint Set - DSU) memorizza e gestisce insiemi (set) che non hanno elementi in comune.
La struttura supporta le seguenti operazioni:
1) make-set(v):
   Crea un nuovo insieme costituito dal nuovo elemento v
2) union-sets(a b):
   Unisce i due insiemi specificati (l'insieme in cui si trova l'elemento a e l'insieme in cui si trova l'elemento b)
3) find-set(v):
   Restituisce il rappresentante (detto anche leader) dell'insieme che contiene l'elemento v.
   Questo rappresentante è un elemento del suo insieme corrispondente.
   Viene selezionato in ogni insieme dalla struttura dati stessa (e può cambiare nel tempo, in particolare dopo le chiamate a union-sets).
   Questo rappresentante può essere utilizzato per verificare se due elementi fanno parte dello stesso insieme o meno.
   a e b appartengono esattamente allo stesso insieme, se find_set(a) == find_set(b). Altrimenti appartengono a insiemi diversi.
Come vedremo in seguito, la struttura dati consente di eseguire ciascuna di queste operazioni in un tempo medio di quasi O(1).

Vediamo un esempio:
Abbiamo 10 individui: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9.
Tra gli individui esistono le seguenti relazioni:

  0 <-> 1
  1 <-> 3
  2 <-> 5
  2 <-> 8
  9 <-> 4
  6 <-> 9

Per rispondere a domande del tipo se '0' sia amico di '3' o meno, dobbiamo creare i seguenti 4 gruppi e mantenere una connessione accessibile tra gli elementi del gruppo:

  G1 = (0 1 3)
  G2 = (2 5 8)
  G3 = (4 6 9)
  G4 = (7)

Determinare se x e y appartengono allo stesso gruppo o meno, significa trovare se x e y sono amici diretti/indiretti.

L'idea centrale è quella di suddividere gli individui in insiemi diversi in base ai gruppi a cui appartengono.
Questo metodo è noto come Unione di insiemi disgiunti, che mantiene una collezione di insiemi disgiunti e ogni insieme è rappresentato da uno dei suoi membri.
Inizialmente, tutti gli elementi appartengono a insiemi diversi.
Dopo aver rappresentato le relazioni fornite, selezioniamo un membro come rappresentante.
Quindi, due persone appartengono allo stesso gruppo se i loro rappresentanti sono gli stessi (cioè sono amici diretti/indiretti).

Per implementare l'algoritmo abbiamo bisogno delle seguenti strutture dati:

1) Lista: una lista di interi (es. 'parent').
Con N elementi, l'indice i-esimo della lista rappresenta l'elemento i-esimo.
Più precisamente, l'indice i-esimo della lista 'parent' contiene il genitore dell'elemento i-esimo.
Queste relazioni creano uno o più alberi virtuali.

2) Albero: è un insieme (set) disgiunto (implementato come lista).
Se due elementi appartengono allo stesso albero, allora appartengono allo stesso insieme disgiunto.
Il nodo radice (o il nodo più in alto) di ogni albero è chiamato rappresentante dell'insieme.
Esiste sempre un singolo rappresentante univoco per ogni insieme.
Una semplice regola per identificare un rappresentante è: se 'i' è il rappresentante di un insieme, allora (parent i) = i.
Se 'i' non è il rappresentante del suo insieme, allora può essere trovato risalendo l'albero fino a trovare il rappresentante.

Le due operazioni principali sulle strutture dati sono:

1) find-set(v):
Funzione che cerca il rappresentante dell'insieme di un dato elemento.
Il rappresentante è sempre la radice dell'albero.
Quindi implementiamo 'find-set' attraversando ricorsivamente la lista 'parent' fino a trovare un nodo che è radice (genitore di se stesso).

2) union-sets(a b):
Funzione che combina due insiemi e per crearne uno.
Prende due elementi come input e trova i rappresentanti dei loro insiemi utilizzando l'operazione 'find', e infine inserisce uno degli alberi (quello che rappresenta l'insieme) sotto il nodo radice dell'altro albero.

Implementazione O(N):

Funzione di inizializzazione della struttura dati:

(define (init-dsu size) (setq parent (sequence 0 (- size 1))))

Funzione per inizializzare un elemento della struttura dati:

(define (make-set v) (setf (parent v) v))

Funzione che ricerca il rappresentante di un elemento:

(define (find-set v)
  (cond ((= v (parent v)) v)
        (true (find-set (parent v)))))

Funzione che combina due insiemi in uno (imposta le relazioni):

(define (union-sets a b)
  (setq a (find-set a))
  (setq b (find-set b))
  (if (!= a b) (setf (parent b) a)))

Esempio 1:

Elementi: 1 2 3 4
Relazioni:
  1 <-> 2
  3 <-> 4
  1 <-> 3
Gruppi:
  G = (1 2 3 4)

(init-dsu 5)
(union-sets 1 2)
;-> 1
(union-sets 3 4)
;-> 3
(union-sets 1 3)
;-> 1
(= (find-set 1) (find-set 2))
;-> true
(= (find-set 1) (find-set 3))
;-> true
(= (find-set 1) (find-set 0))
;-> nil

Esempio 2:

Elementi: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9.
Relazioni:
  0 <-> 1
  1 <-> 3
  2 <-> 5
  2 <-> 8
  9 <-> 4
  6 <-> 9
Gruppi:
  G1 = (0 1 3)
  G2 = (2 5 8)
  G3 = (4 6 9)
  G4 = (7)

(init-dsu 10)
(union-sets 0 1)
(union-sets 1 3)
(union-sets 2 5)
(union-sets 2 8)
(union-sets 9 4)
(union-sets 6 9)
parent
;-> (0 0 2 0 9 2 6 7 2 6)
(= (find-set 0) (find-set 3))
;-> true
parent
;-> (0 0 2 0 9 2 6 7 2 6)
(= (find-set 4) (find-set 9))
;-> true
parent
;-> (0 0 2 0 9 2 6 7 2 6)
(= (find-set 6) (find-set 2))
;-> nil
parent
;-> (0 0 2 0 9 2 6 7 2 6)

La funzione find-set(v) ha una complessità temporale di O(N).
Possiamo migliorare il comportamento con alcune modifiche.

Ottimizzazione (Compressione dei percorsi e Unione per rango/dimensione)
------------------------------------------------------------------------
L'idea principale è quella di ridurre l'altezza degli alberi che rappresentano insiemi diversi. Questo obiettivo viene raggiunto con due metodi più comuni:
1) Compressione dei percorsi
2) Unione per rango (o per dimensione)

1) Compressione dei percorsi (utilizzata per migliorare find-set())
-------------------------------------------------------------------
L'idea è quella di appiattire l'albero quando viene chiamata find-set().
Quando find-set() viene chiamata per un elemento x, viene restituita la radice dell'albero.
L'operazione find-set() attraversa da x per trovare la radice.
L'idea della compressione dei percorsi è quella di rendere la radice trovata il padre di x in modo da non dover attraversare nuovamente tutti i nodi intermedi.
Se x è la radice di un sottoalbero, anche il percorso (verso la radice) da tutti i nodi sotto x viene compresso.
Accelera la struttura dati comprimendo l'altezza degli alberi.
Questo può essere ottenuto inserendo un piccolo meccanismo di caching nell'operazione find-set(v).

La nuova implementazione di 'find-set' diventa:

(define (find-set v)
  (cond ((= v (parent v)) v)
        (true (setf (parent v) (find-set (parent v))))))

Questa modifica prima trova il rappresentante dell'insieme (vertice radice), e poi, durante il processo di stack unwinding, i nodi visitati vengono associati direttamente al rappresentante.
In questo modo l'operazione raggiunge una complessità temporale media di O(log n) per chiamata.

Proviamo:

(init-dsu 10)
(union-sets 0 1)
(union-sets 1 3)
(union-sets 2 5)
(union-sets 2 8)
(union-sets 9 4)
(union-sets 6 9)
parent
;-> (0 0 2 0 9 2 6 7 2 6)
(= (find-set 0) (find-set 3))
;-> true
parent
;-> (0 0 2 0 9 2 6 7 2 6)
(= (find-set 4) (find-set 9))
;-> true
parent
;-> (0 0 2 0 6 2 6 7 2 6) ; cambia la radice da 9 a 6 dell'indice 4
(= (find-set 6) (find-set 2))
;-> nil
parent
;-> (0 0 2 0 6 2 6 7 2 6)

La seconda modifica che implementiamo rende il tutto ancora più veloce.
In questa ottimizzazione modificheremo l'operazione union-set().
Per essere precisi, cambieremo quale albero viene collegato all'altro.
Nell'implementazione di base, il secondo albero veniva sempre collegato al primo.
In pratica, questo può portare ad alberi contenenti catene di lunghezza O(N).
Con questa ottimizzazione eviteremo questo problema scegliendo con molta attenzione quale albero viene collegato.

2a) Unione per rango (utilizzata per migliorare union-sets())
-------------------------------------------------------------
Il rango è come l'altezza degli alberi che rappresentano insiemi diversi.
Utilizziamo una lista aggiuntiva di interi chiamata 'rank'.
La dimensione di questa lista è la stessa della lista padre 'parent'.
Se 'i' è rappresentativo di un insieme, rank(i) è il rango dell'elemento i.
Il rango è uguale all'altezza se non si utilizza la compressione dei percorsi.
Con la compressione dei percorsi, il rango può essere maggiore dell'altezza effettiva.
Quindi utilizziamo il limite superiore della profondità dell'albero, perché la profondità diminuisce quando si applica la compressione dei percorsi.
Ora ricordiamo che nell'operazione di Unione, non importa quale dei due alberi venga spostato sotto l'altro.
Ora ciò che vogliamo fare è ridurre al minimo l'altezza dell'albero risultante.
Se stiamo unendo due alberi (o insiemi), chiamiamoli sinistro e destro, quindi tutto dipende dal rango di sinistro e dal rango di destro.
- Se il rango di sinistra è inferiore al rango di destra, allora è meglio spostarsi a sinistra sotto a destra, perché ciò non cambierà il rango di destra (mentre spostarsi a destra sotto a sinistra aumenterebbe l'altezza).
- Allo stesso modo, se il rango di destra è inferiore al rango di sinistra, allora dovremmo spostarci a destra sotto a sinistra.
- Se i ranghi sono uguali, non importa quale albero si trovi sotto l'altro, ma il rango del risultato sarà sempre maggiore di uno rispetto al rango degli alberi.

2b) Unione per dimensione (utilizzata per migliorare union-sets())
Utilizziamo una lista di interi chiamata 'size'.
La dimensione di questa lista è la stessa della lista padre 'parent'.
Se 'i' è il rappresentante di un insieme, size(i) è il numero di elementi nell'albero che rappresenta l'insieme.
Ora stiamo unendo due alberi (o insiemi), chiamiamoli left e right: in questo caso, tutto dipende dalla dimensione dell'albero left e dalla dimensione dell'albero right (o insieme).
- Se la dimensione di left è inferiore a quella di right, allora è meglio spostare left sotto right e aumentare la dimensione di right di quella di left.
- Allo stesso modo, se la dimensione di right è inferiore a quella di left, allora dovremmo spostare right sotto left e aumentare la dimensione di left di quella di right.
- Se le dimensioni sono uguali, non importa quale albero vada sotto l'altro.

In entrambi gli approcci, l'essenza dell'ottimizzazione è la stessa: colleghiamo l'albero con il rango inferiore a quello con il rango maggiore.
Entrambe le ottimizzazioni sono equivalenti in termini di complessità temporale e spaziale. Quindi, in pratica, è possibile utilizzarne una qualsiasi.

Unione per dimensione (implementazione)
---------------------------------------
Funzione di inizializzazione della struttura dati:

(define (init-dsu N)
  (setq parent (sequence 0 (- N 1)))
  (setq size (dup 1 N)))

Funzione per inizializzare un elemento della struttura dati:

(define (make-set v)
  (setf (parent v) v)
  (setf (size v) 1))

Funzione che combina due insiemi in uno (imposta le relazioni):

(define (union-sets a b)
  (setq a (find-set a))
  (setq b (find-set b))
  (if (!= a b)
    (begin
      (if (< (size a) (size b)) (swap a b))
      (setf (parent b) a)
      (setq (size a) (+ (size a) (size b))))))

Proviamo:

(init-dsu 10)
(union-sets 0 1)
(union-sets 1 3)
(union-sets 2 5)
(union-sets 2 8)
(union-sets 9 4)
(union-sets 6 9)
parent
;-> (0 0 2 0 9 2 9 7 2 9) ; diversa dalla lista della versione di base
(= (find-set 0) (find-set 3))
;-> true
parent
;-> (0 0 2 0 9 2 9 7 2 9)
(= (find-set 4) (find-set 9))
;-> true
parent
;-> (0 0 2 0 9 2 9 7 2 9)
(= (find-set 6) (find-set 2))
;-> nil
parent
;-> (0 0 2 0 9 2 9 7 2 9)

Unione per rango (implementazione)
----------------------------------
Funzione di inizializzazione della struttura dati:

(define (init-dsu N)
  (setq parent (sequence 0 (- N 1)))
  (setq rank (dup 0 N)))

Funzione per inizializzare un elemento della struttura dati:

(define (make-set v)
  (setf (parent v) v)
  (setf (rank v) 0))

Funzione che combina due insiemi in uno (imposta le relazioni):

(define (union-sets a b)
  (setq a (find-set a))
  (setq b (find-set b))
  (if (!= a b)
    (begin
      (if (< (rank a) (rank b)) (swap a b))
      (setf (parent b) a)
      (if (= (rank a) (rank b)) (++ (rank a))))))

Proviamo:

(init-dsu 10)
(union-sets 0 1)
(union-sets 1 3)
(union-sets 2 5)
(union-sets 2 8)
(union-sets 9 4)
(union-sets 6 9)
parent
;-> (0 0 2 0 9 2 9 7 2 9) ; diversa dalla lista della versione di base
(= (find-set 0) (find-set 3))
;-> true
parent
;-> (0 0 2 0 9 2 9 7 2 9)
(= (find-set 4) (find-set 9))
;-> true
parent
;-> (0 0 2 0 9 2 9 7 2 9)
(= (find-set 6) (find-set 2))
;-> nil
parent
;-> (0 0 2 0 9 2 9 7 2 9)

Riassunto
---------
La Disjoint Set Union (DSU), detta anche Union-Find, è una struttura dati molto usata in algoritmi e problemi che richiedono di gestire partizioni dinamiche di insiemi disgiunti.
Permette di:

- capire rapidamente se due elementi appartengono allo stesso insieme (find)
- unire due insiemi (union)

Grazie a ottimizzazioni come path compression e union by rank/size, le operazioni hanno costo quasi costante (ammortizzato, O(alpha(N)), dove alpha è la funzione inversa di Ackermann).

Casi d'uso principali di DSU:

1. Grafi e connettività
   - Verificare se due nodi appartengono alla stessa componente connessa.
   - Costruzione di alberi di copertura minimi (algoritmo di Kruskal).
   - Rilevare la presenza di cicli durante l'aggiunta di archi.

2. Problemi di clustering
   - Partizionare dati in insiemi disgiunti.
   - Algoritmi di segmentazione (ad esempio in computer vision).

3. Problemi di equivalenza
   - Gestire classi di equivalenza (es. congruenze, raggruppamenti).
   - Determinare se due elementi sono equivalenti in base a vincoli dati.

4. Percolazione e sistemi fisici
   - Modellare reti di connessione (acqua, elettricità, ecc.) e verificare la percolazione.
   - Verificare la formazione di cluster connessi.

5. Dynamic Connectivity
   - Rispondere a query tipo "aggiungi un arco" e "sono i due nodi collegati?" in tempo efficiente.

6. Problemi su insiemi
   - Gestire insiemi che si fondono nel tempo (ad esempio simulazioni di unioni di gruppi).
   - Giochi o puzzle dove gruppi di celle si uniscono.

Esempi:
- In un social network: determinare se due persone fanno parte dello stesso gruppo di amici.
- In un labirinto random: verificare che l'aggiunta di un corridoio non crei cicli.
- Nei problemi in cui occorre risolvere query di tipo "sono nello stesso insieme?" o "unisci i due insiemi".

Ulteriori approfondimenti:
https://cp-algorithms.com/data_structures/disjoint_set_union.html


---------------------------
Numeri primi su un orologio
---------------------------

Quanti e quali numeri primi ci sono in un orologio (ore, minuti, secondi) in un giorno?
Quanti e quali numeri primi ci sono in un orologio (ore, minuti) in un giorno?

Sequenza OEIS A229106:
Prime time display in hours, minutes, seconds on a six-digit 24-hour digital clock.
  2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 101, 103,
  107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 211, 223, 227, 229, 233,
  239, 241, 251, 257, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 401,
  409, 419, 421, 431, 433, 439, 443, ...
Leading zeros are ignored, so the term a(3) = 5, for example, corresponds to the display 00:00:05. Sequence has 7669 entries.
The first 211 terms are the same as in A050246.

Sequenza OEIS A050246:
Digital clock primes.
  2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 101, 103,
  107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 211, 223, 227, 229, 233,
  239, 241, 251, 257, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 401,
  409, 419, 421, 431, 433, 439, 443, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (time-list t1 t2)
"Generate a list of times (HH MM SS) from t1 to t2"
  (local (out a b)
    (setq out '())
    (for (h (t1 0) (t2 0))
      (for (m 0 59)
        (for (s 0 59)
          (push (list h m s) out -1))))
    (setq a (find t1 out))
    (setq b (find t2 out))
    (slice out a (+ (- b a) 1))))

(define (primi-hms)
  (let (lst (time-list '(0 0 0) '(23 59 59)))
    (setq lst (map (fn(x) (format "%02d%02d%02d" (x 0) (x 1) (x 2))) lst))
    (filter prime? (map (fn(x) (int x 0 10)) lst))))

(primi-hms)
;-> (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 101 103
;->  107 109 113 127 131 137 139 149 151 157 211 223 227 229 233
;->  239 241 251 257 307 311 313 317 331 337 347 349 353 359 401
;->  409 419 421 431 433 439 443 449 457 503 509 521 523 541 547
;-> ...
;->  235349 235439 235441 235447 235513 235519 235523 235537 235541
;->  235553 235559 235601 235607 235621 235723 235747 235751 235811
;->  235813 235849 235901 235919 235927 235951)
(length (primi-hms))
;-> 7669

(define (primi-hm)
  (let (lst (time-list '(0 0 0) '(23 59 59)))
    (setq lst (unique (map (fn(x) (slice x 0 2)) lst)))
    (setq lst (map (fn(x) (format "%02d%02d" (x 0) (x 1))) lst))
    (filter prime? (map (fn(x) (int x 0 10)) lst))))

(primi-hm)
;-> (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 101 103
;->  107 109 113 127 131 137 139 149 151 157 211 223 227 229 233
;->  239 241 251 257 307 311 313 317 331 337 347 349 353 359 401
;->  409 419 421 431 433 439 443 449 457 503 509 521 523 541 547
;-> ...
;->  1823 1831 1847 1901 1907 1913 1931 1933 1949 1951 2003 2011
;->  2017 2027 2029 2039 2053 2111 2113 2129 2131 2137 2141 2143
;->  2153 2203 2207 2213 2221 2237 2239 2243 2251 2309 2311 2333
;->  2339 2341 2347 2351 2357)

(length (primi-hm))
;-> 211


------------------------------------------------------------
Liste di k primi consecutivi che sommano come i loro inversi
------------------------------------------------------------

Cominciamo considerando una coppia di primi consecutivi.

Data la lista dei primi p1, p2, ..., pk,... e dei loro inversi q1, q2, ... qk, ..., determinare le coppie consecutive di primi p(i) e p(i+1) per cui risulta:

  p(i) + p(i+1) = q(i) + q(i+1)

Per inverso di un numero si intende il numero con tutte le cifre invertite.
Esempio:
N = 25437, Inversione = 73452

Inoltre deve risultare p(i) != q(i) e p(i+1) != q(i+1), cioè i numeri primi non devono essere palindromi.

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

Funzione che inverte le cifre di un numero:

(define (invert-digits num) (int (reverse (string num)) 0 10))

Funzione che cerca le coppie di primi consecutivi che soddisfano i requisiti:

(define (search-pair limite show)
  (local (out primi len conta p1 p2 q1 q2)
    (setq out '())
    (setq primi (primes-to limite))
    (setq len (length primi))
    (setq primi (array len primi))
    (setq conta 0)
    (for (i 0 (- len 2))
      (setq p1 (primi i))
      (setq p2 (primi (+ i 1)))
      (setq q1 (invert-digits p1))
      (setq q2 (invert-digits p2))
      ;(println p1 { } p2 { } q1 { } q2)
      (when (and (!= p1 q1) (!= p2 q2)
                (= (+ p1 p2) (+ q1 q2)))
        (push (list p1 p2 (+ p1 p2) q1 q2) out -1)
        (if show (println p1 " + " p2 " = " (+ p1 p2) " = " q1 " + " q2))
        (++ conta)))
    (if show
        (println conta " coppie.")
        ;else
        out)))

Proviamo:

(search-pair 1e3 true)
;-> 211 + 223 = 434 = 112 + 322
;-> 281 + 283 = 564 = 182 + 382
;-> 457 + 461 = 918 = 754 + 164
;-> 487 + 491 = 978 = 784 + 194
;-> 509 + 521 = 1030 = 905 + 125
;-> 557 + 563 = 1120 = 755 + 365
;-> 569 + 571 = 1140 = 965 + 175
;-> 587 + 593 = 1180 = 785 + 395
;-> 653 + 659 = 1312 = 356 + 956
;-> 827 + 829 = 1656 = 728 + 928
;-> 857 + 859 = 1716 = 758 + 958
;-> 11 coppie.

(length (search-pair 1e5))
;-> 56

(length (search-pair 1e7))
;-> 357

Possiamo estendere la funzione per trattare una lista di K primi consecutivi.

Funzione che cerca k primi consecutivi che soddisfano i requisiti:

(define (search-k k limite)
  (local (out primi len p q)
    (setq out '())
    (setq primi (primes-to limite))
    (setq len (length primi))
    (setq primi (array len primi))
    (for (i 0 (- len k))
      (setq p '())
      (setq q '())
      ; costruzione delle liste dei primi (p) e dei suoi inversi (q)
      (for (n 0 (- k 1))
        (push (primi (+ i n)) p -1)
        (push (invert-digits (primi (+ i n))) q -1))
      ;(println p { } q)
                 ; primi non palindromi?
      (when (and (for-all true? (map (fn (x y) (!= x y)) p q))
                 ; somme uguali?
                 (= (apply + p) (apply + q)))
        (push (list p (apply + p) q) out -1))
    out)))

Proviamo:

(search-k 2 1e3)
;-> (((211 223) 434 (112 322)) ((281 283) 564 (182 382))
;->  ((457 461) 918 (754 164)) ((487 491) 978 (784 194))
;->  ((509 521) 1030 (905 125)) ((557 563) 1120 (755 365))
;->  ((569 571) 1140 (965 175)) ((587 593) 1180 (785 395))
;->  ((653 659) 1312 (356 956)) ((827 829) 1656 (728 928))
;->  ((857 859) 1716 (758 958)))

(length (search-K 2 1e5))
;-> 56

(length (search-K 2 1e7))
;-> 357

(search-k 3 1e3)
;-> (((37 41 43) 121 (73 14 34))
;->  ((43 47 53) 143 (34 74 35))
;->  ((59 61 67) 187 (95 16 76))
;->  ((491 499 503) 1493 (194 994 305))
;->  ((541 547 557) 1645 (145 745 755))
;->  ((571 577 587) 1735 (175 775 785))
;->  ((599 601 607) 1807 (995 106 706)))

(time (println (length (setq k37 (search-k 3 1e7)))))
;-> 16
;-> 4641.255
(time (println (length (setq k47 (search-k 4 1e7)))))
;-> 70
;-> 5470.054
(time (println (length (setq k57 (search-k 5 1e7)))))
;-> 13
;-> 6343.569
(time (println (length (setq k67 (search-k 6 1e7)))))
;-> 32
;-> 7188.629
(time (println (length (setq k77 (search-k 7 1e7)))))
;-> 4
;-> 8078.289
(time (println (length (setq k87 (search-k 8 1e7)))))
;-> 7
;-> 8902.718999999999
(time (println (length (setq k97 (search-k 9 1e7)))))
;-> 3
;-> 9795.275
(time (println (length (setq k107 (search-k 10 1e7)))))
;-> 6
;-> 10685.699
(k107 0)
;-> ((569 571 577 587 593 599 601 607 613 617)
;->   5934
;->  (965 175 775 785 395 995 106 706 316 716))


---------------------------------------
Somma quadrata dei primi N numeri primi
---------------------------------------

Determinare i valori di N per cui la somma dei primi N numeri primi è un quadrato perfetto.

Sequenza OEIS A033997:
Numbers n such that sum of first n primes is a square.
  9, 2474, 6694, 7785, 709838, 126789311423, ...

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

(define (square? num)
"Check if an integer is a perfect square"
  (let (a (bigint num))
    (while (> (* a a) num)
      (setq a (/ (+ a (/ num a)) 2))
    )
    (= (* a a) num)))

(define (sequenza limite)
  (local (out primi len num)
    (setq out '())
    (setq primi (primes-to limite))
    (setq len (length primi))
    ; converte in vettore (indicizzazione più veloce della lista)
    (setq primi (array len primi))
    (setq num 0)
    (for (i 0 (- len 1))
      (++ num (primi i))
      (if (square? num) (push (list (+ i 1) num) out -1)))
    out))

(time (println (sequenza 1.1e7)))
;-> ((9 100) (2474 25633969) (6694 212372329)
;->  (7785 292341604) (709838 3672424151449))
;-> 10516.531


---------------------------------
La media dei primi K numeri primi
---------------------------------

Definiamo la media della somma dei primi k numeri primi (Average Prime Number):

  APN(k) = Sum[i=1,k]p(k) / k

Determinare i valori di k per cui APN è un numero intero.

Sequenza OEIS A045345:
Numbers k such that k divides sum of first k primes A007504(k).
  1, 23, 53, 853, 11869, 117267, 339615, 3600489, 96643287, 2664167025,
  43435512311, 501169672991, 745288471601, 12255356398093, 153713440932055,
  6361476515268337, ...

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

(define (sequenza limite)
  (local (out primi len num)
    (setq out '())
    (setq primi (primes-to limite))
    (setq len (length primi))
    ; converte in vettore (indicizzazione più veloce della lista)
    (setq primi (array len primi))
    (setq num 0)
    (for (i 0 (- len 1))
      (++ num (primi i))
      (if (zero? (% num (+ i 1))) (push (list (+ i 1) (primi i)) out -1)))
    out))

Proviamo:

(time (println (sequenza 1e7)))
;-> ((1 2) (23 83) (53 241) (853 6599) (11869 126551)
;->  (117267 1544479) (339615 4864121))
;-> 1765.533

(time (println (sequenza 1e8)))
;-> ((1 2) (23 83) (53 241) (853 6599) (11869 126551)
;->  (117267 1544479) (339615 4864121) (3600489 60686737))
;-> 19017.252


-----------------------
La funzione "takewhile"
-----------------------

La funzione "takewhile" (python, haskell, ecc.) prende come parametri una lista e una funzione/predicato.
Restituisce i primi elementi della lista che soddisfano il predicato.
Quando incontra il primo elemento che non soddisfa il predicato, allora restituisce la lista costruita fino a quel momento.

Esempio:
  lista = (1 3 6 5 2)
  predicato = odd?
  output = (1 3)
Perchè quando incontriamo 6 (che non è dispari) usciamo dalla funzione e i numeri dispari incontrati precedentemente sono 1 e 3.

(define (takewhile lst pred)
  (let ( (out '()) (stop nil) )
    (dolist (el lst stop)
      (if (pred el)
          (push el out -1)
          (setq stop true)))
    out))

Proviamo:

(takewhile '(1 3 6 5 2) odd?)
;-> (1 3)

Con una funzione/predicato definito dall'utente:

(define (pred x) (or (even? x) (and (> x 0) (< x 5))))

(takewhile '(1 3 6 5 2) pred)
;-> (1 3 6)


----------------------
La costante di Honaker
----------------------

Consideriamo la serie della somma degli inversi dei numeri primi palindromi:

  S = Sum[i=1..Inf] 1/p(i) = 1/2 + 1/3 + 1/5 + 1/7 + 1/11 + 1/101 + ...

Questa serie è convergente.

Valore della serie (calcolato fino a tutti i primi palindromi con 11 cifre):

  1.3239820264...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

Funzione che inverte le cifre di un numero intero:

(define (invert-digits num) (int (reverse (string num)) 0 10))

(define (honaker limite)
  (let (val 0)
    (for (num 2 limite)
      ; (invert-digits num) è più veloce di (prime? num)
      (if (and (= num (invert-digits num)) (prime? num))
          (setq val (add val (div num)))))
    val))

Proviamo:

(honaker 10)
;-> 1.176190476190476
(honaker 101)
;-> 1.267099567099567
(honaker 1000)
;-> 1.32072324459029  ; 2 cifre corrette (dopo la virgola)
(time (println (honaker 1e6)))
;-> 1.323748402250648 ; 3 cifre corrette (dopo la virgola)
;-> 638.471
(time (println (honaker 1e7)))
;-> 1.323964105671205 ; 4 cifre corrette (dopo la virgola)
;-> 6406.019
(time (println (honaker 1e8)))
;-> 1.323964105671205 ; 4 cifre corrette (dopo la virgola)
;-> 65912.74000000001

Abbiamo raggiunto il limite di precisione dei numeri float.

Proviamo calcolando la somma come frazione e poi effettuare la divisione:

Calcola i primi palindromi fino ad un dato limite:

(define (primi-pali limite)
  (let (out '(2))
    (for (num 3 limite 2)
      (if (and (= num (invert-digits num)) (prime? num))
          (push num out -1)))
    out))

Somma gli inversi di una lista di numeri:
(restituisce una frazione (numeratore denominatore))

(define (sum-inv lst)
  (local (den num)
    (setf (lst 0) 2L)
    (setq den (apply * lst))
    (setq num (apply + (map (fn(x) (/ den x)) lst)))
    (list num den)))

Proviamo fino a 1e6:

(setq fraz (map string (sum-inv (primi-pali 1e6))))
;-> ("6515180...936554873L"
;->  "4921766...330192690L")

Usiamo WolframScript per calcolare la frazione con 20 cifre di precisione.
Vedi "Wolfram Engine e WolframScript" su "Note libere 24".

Togliamo le "L" finali ai numeri:
(pop (fraz 0) -1)
(pop (fraz 1) -1)

Impostiamo l'expressione:
(setq expr (string "N[" (fraz 0) "/" (fraz 1) ",20]"))

Impostiamo il comando:
(setq cmd (append "{wolframscript -code \"" expr "\"}"))

Eseguiamo il comando:
(exec (eval-string cmd))
;-> ("1.32374840225064855416442574628003596276`20.")

Putroppo non possiamo continuare perchè la stringa di comando 'cmd' diventa troppo lunga per il terminale.

Comunque risulta:
1e7  -->  1.3239641056712024580 ; 4 cifre corrette (dopo la virgola)
1e8  -->  1.3239641056712024580 ; 4 cifre corrette (dopo la virgola)

Con Mathematica (2e9):

AbsoluteTiming[{limit = 2000000000;
  (*Primi palindromi fino al limite*)
  palPrimes = Select[Prime[Range[PrimePi[limit]]], PalindromeQ];
  (*Somma degli inversi*)
  sumReciprocals = Total[1/palPrimes];
  (*Visualizzazione numerica a 20 cifre*)
  N[sumReciprocals, 20]}]
;-> {470.4, {1.3239807180655250609}} ; 5 cifre corrette (dopo la virgola)


---------------------
Cruciverba matematico
---------------------

Risolvere il seguente cruciverba (crucinumero):

  +-----+-----+-----+
  | A   | B   | C   |
  |     |     |     |
  +-----+-----+-----+
  | D   |     |     |
  |     |     |     |
  +-----+-----+-----+
  | E   |     |
  |     |     |
  +-----+-----+

Orizzontali
A: numero con solo cifre pari in ordine strettamente decrescente.
D: un numero non divisibile per 9.
E: un numero divisibile per 11

Verticali
A: numero con cifre consecutive in ordine crescente.
B: numero in cui il prodotto delle due cifre più piccole è uguale alla cifra più grande.
C: un numero primo più grande di 10.

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (int-list num)
"Convert an integer to a list of digits"
  (if (zero? num) '(0)
  (let (out '())
    (while (!= num 0)
      (push (% num 10) out)
      (setq num (/ num 10))) out)))

(define (group-block lst num)
"Create a list with blocks of elements: (0..num) (1..num+1) (n..num+n)"
  (local (out items len)
    (setq out '())
    (setq len (length lst))
    (if (>= len num) (begin
        ; numero di elementi nella lista di output (numero blocchi)
        (setq items (- len num (- 1)))
        (for (k 0 (- items 1))
          (push (slice lst k num) out -1))))
  out))

AO: numero (3 cifre) con solo cifre pari in ordine strettamente decrescente.
(define (AO) (group-block '(8 6 4 2) 3))
(AO)
;-> ((8 6 4) (6 4 2))

DO: numero (3 cifre) non divisibile per 9.
(define (DO) (map int-list (clean (fn(x) (zero? (% x 9))) (sequence 100 999))))
(DO)
;-> ((1 0 0) (1 0 1) (1 0 2) (1 0 3) (1 0 4) (1 0 5) (1 0 6) (1 0 7)
;->  ...
;->  (9 9 1) (9 9 2) (9 9 3) (9 9 4) (9 9 5) (9 9 6) (9 9 7) (9 9 8))

EO: numero (2 cifre) divisibile per 11
(define (EO) (map int-list (filter (fn(x) (zero? (% x 11))) (sequence 10 99))))
(EO)
;-> ((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9))

AV: numero (3 cifre) con cifre consecutive in ordine crescente.
(define (AV) (group-block '(1 2 3 4 5 6 7 8 9) 3))
(AV)
;-> ((1 2 3) (2 3 4) (3 4 5) (4 5 6) (5 6 7) (6 7 8) (7 8 9))

BV: numero (3 cifre) in cui il prodotto delle due cifre più piccole è uguale alla cifra più grande.
(define (BV)
  (let (out '())
    (for (num 100 999)
      (setq lst (sort (int-list num)))
      (if (= (* (lst 0) (lst 1)) (lst 2))
          (push num out -1)))
     (map int-list out)))
(BV)
;-> ((1 1 1) (1 2 2) (1 3 3) (1 4 4) (1 5 5) (1 6 6) (1 7 7) (1 8 8) (1 9 9)
;->  (2 1 2) (2 2 1) (2 2 4) (2 3 6) (2 4 2) (2 4 8) (2 6 3) (2 8 4) (3 1 3)
;->  (3 2 6) (3 3 1) (3 3 9) (3 6 2) (3 9 3) (4 1 4) (4 2 2) (4 2 8) (4 4 1)
;->  (4 8 2) (5 1 5) (5 5 1) (6 1 6) (6 2 3) (6 3 2) (6 6 1) (7 1 7) (7 7 1)
;->  (8 1 8) (8 2 4) (8 4 2) (8 8 1) (9 1 9) (9 3 3) (9 9 1))

CV: numero (2 cifre) primo più grande di 10.
(define (CV) (map int-list (filter prime? (sequence 11 99))))
(CV)
;-> ((1 1) (1 3) (1 7) (1 9) (2 3) (2 9) (3 1) (3 7) (4 1) (4 3) (4 7)
;->  (5 3) (5 9) (6 1) (6 7) (7 1) (7 3) (7 9) (8 3) (8 9) (9 7))

Cominciamo mettendo (8 6 4) in AO.
Cerchiamo in AV se esiste almeno un numero che inizia per 8.
(find '(8 ? ?) (AV) match)
(find-all '(8 ? ?) (AV))
;-> ()
Non c'è nessun numero che inizia per 8 in AV.

Allora in AO mettiamo (6 4 2).
Cerchiamo in AV se esiste almeno un numero che inizia per 8.
(find-all '(6 ? ?) (AV))
;-> ((6 7 8))
Troviamo un numero che inizia per 6 in AV.

  +-----+-----+-----+
  | A   | B   | C   |
  | 6   | 4   | 2   |
  +-----+-----+-----+
  | D   |     |     |
  | 7   |     |     |
  +-----+-----+-----+
  | E   |     |
  | 8   |     |
  +-----+-----+

Adesso cerchiamo i numeri che iniziano per 8 in EO.
(find-all '(8 ?) (EO))
;-> ((8 8))

  +-----+-----+-----+
  | A   | B   | C   |
  | 6   | 4   | 2   |
  +-----+-----+-----+
  | D   |     |     |
  | 7   |     |     |
  +-----+-----+-----+
  | E   |     |
  | 8   | 8   |
  +-----+-----+

Cerchiamo in BV i numeri che iniziano per 4 e terminano con 8.
(find-all '(4 ? 8) (BV))
;-> ((4 2 8))

  +-----+-----+-----+
  | A   | B   | C   |
  | 6   | 4   | 2   |
  +-----+-----+-----+
  | D   |     |     |
  | 7   | 2   |     |
  +-----+-----+-----+
  | E   |     |
  | 8   | 8   |
  +-----+-----+

Adesso cerchiamo i numeri che iniziano con (2) in CV.
(find-all '(2 ?) (CV))
;-> ((2 3) (2 9))

Cerchiamo i numeri che iniziano con (7 2) in DO.
(find-all '(7 2 ?) (DO))
;-> ((7 2 1) (7 2 2) (7 2 3) (7 2 4) (7 2 5) (7 2 6) (7 2 7) (7 2 8))
Per soddisfare CO il numero in DO deve avere l'ultima cifra che vale 3 o 9.
(find-all '(7 2 3) (DO))
;-> ((7 2 3))
(find-all '(7 2 9) (DO))
;-> ()
Quindi il numero cercato per DO vale (7 2 3).

Soluzione cruciverba:

  +-----+-----+-----+
  | A   | B   | C   |
  | 6   | 4   | 2   |
  +-----+-----+-----+
  | D   |     |     |
  | 7   | 2   | 3   |
  +-----+-----+-----+
  | E   |     |
  | 8   | 8   |
  +-----+-----+


------------------------------------------
Alcuni confronti tra elementi di due liste
------------------------------------------

Funzione che verifica se due liste (con la stessa lunghezza) hanno elementi tutti uguali negli stessi indici (cioè se due liste sono uguali):

(define (all-equal-idx? lst1 lst2) (= lst1 lst2))

(all-equal-idx? '(1 2 3) '(1 2 3))
;-> true
(all-equal-idx? '(1 2 3) '(1 2 4))
;-> nil

Funzione che verifica se due liste (con la stessa lunghezza) hanno elementi tutti diversi negli stessi indici:

(define (all-different-idx? lst1 lst2)
  (for-all true? (map (fn (x y) (!= x y)) lst1 lst2)))

(all-different-idx? '(1 2 3) '(4 5 6))
;-> true
(all-different-idx? '(1 2 3) '(4 2 4))
;-> nil

Funzione che verifica se due liste (con la stessa lunghezza) hanno almeno un elemento diverso negli stessi indici:

(define (atleast-one-different-idx? lst1 lst2)
  (exists true? (map (fn (x y) (!= x y)) lst1 lst2)))

(atleast-one-different-idx? '(1 2 4) '(1 2 3))
;-> true
(atleast-one-different-idx? '(1 2 4) '(1 2 4))
;-> nil

Funzione che verifica se due liste (con la stessa lunghezza) hanno almeno un elemento uguale negli
stessi indici:

(define (atleast-one-equal-idx? lst1 lst2)
  (exists true? (map (fn (x y) (= x y)) lst1 lst2)))

(atleast-one-equal-idx? '(1 2 4) '(5 6 4))
;-> true
(atleast-one-equal-idx? '(1 2 4) '(5 6 7))
;-> nil

Funzione che verifica se due liste hanno gli stessi elementi (in qualunque ordine):

(define (all-equal? lst1 lst2) (= (sort lst1) (sort lst2)))

(all-equal? '(1 2 3) '(3 1 2))
;-> true
(all-equal? '(1 2 3) '(1 2 4))
;-> nil

Funzione che verifica se due liste hanno tutti elementi diversi (in qualunque ordine):

(define (all-different? lst1 lst2)
  (local (len1 len2 len-all)
    (setq len1 (length lst1))
    (setq len2 (length lst2))
    (setq len-all (length (unique (extend lst1 lst2))))
    (= len-all (+ len1 len2))))

(all-different? '(1 2 3) '(4 5 6))
;-> true
(all-different? '(1 2 3) '(4 5 1))
;-> nil

Funzione che rimuove gli elementi in comune tra due liste  (1:1):

(define (remove-common lst1 lst2)
"Remove common elements of two lists (remove 1:1)"
  (cond
    ((= lst1 '()) lst2)
    ((= lst2 '()) lst1)
    (true
      (local (out len1 len2 i j el1 el2)
        (setq out '())
        (setq len1 (length lst1))
        (setq len2 (length lst2))
        ; usa i vettori
        (setq lst1 (array len1 lst1))
        (setq lst2 (array len2 lst2))
        ; ordina le liste
        (sort lst1)
        (sort lst2)
        (setq i 0)
        (setq j 0)
        ; confronta gli elementi ed avanza con due puntatori
        (while (and (< i len1) (< j len2))
          (setq el1 (lst1 i))
          (setq el2 (lst2 j))
                ; elementi uguali
          (cond ((= el1 el2) (++ i) (++ j))
                ; primo elemento minore
                ((< el1 el2) (push el1 out -1) (++ i))
                ; secondo elemento minore
                ((> el1 el2) (push el2 out -1) (++ j)))
        )
        ; aggiunge gli elementi della lista più lunga
        (cond ((and (= i len1) (= j len2)) out)
              ((= i len1) (extend out (array-list (slice lst2 j))))
              ((= j len2) (extend out (array-list (slice lst1 i)))))))))

(remove-common '(3 2 2 1 0) '(1 2 3 4 5))
;-> (0 2 4 5)
(remove-common '(1 2 3 4) '(5 6 7 2 2))
;-> (1 2 3 4 5 6 7)

Funzione che prende gli elementi in comune tra due liste (1:1):

(define (take-common lst1 lst2)
"Take common elements of 2 lists (take 1:1)"
  (cond
    ((or (= lst1 '()) (= lst2 '())) '())
    (true
      (local (out len1 len2 i j el1 el2)
        (setq out '())
        (setq len1 (length lst1))
        (setq len2 (length lst2))
        ; usa i vettori
        (setq lst1 (array len1 lst1))
        (setq lst2 (array len2 lst2))
        ; ordina le liste
        (sort lst1)
        (sort lst2)
        (setq i 0)
        (setq j 0)
        ; confronta gli elementi ed avanza con due puntatori
        (while (and (< i len1) (< j len2))
          (setq el1 (lst1 i))
          (setq el2 (lst2 j))
                ; elementi uguali
          (cond ((= el1 el2) (push el1 out -1) (++ i) (++ j))
                ; primo elemento minore
                ((< el1 el2) (++ i))
                ; secondo elemento minore
                ((> el1 el2) (++ j)))
        )
        out))))

(take-common '(3 2 1 1 0) '(1 2 3 4 5 1))
;-> (1 1 2 3)
(take-common '(1 2 3 4) '(5 6 7))
;-> ()


--------------------
Creazione di griglie
--------------------

Scrivere una funzione che prende larghezza e altezza e stampa una griglia in cui una cella ha la seguente raffigurazione:

  ╔════╗
  ║    ║
  ╚════╝

Esempi:
Larghezza = 5
Altezza = 4
  ╔════╦════╦════╦════╦════╗
  ║    ║    ║    ║    ║    ║
  ╠════╬════╬════╬════╬════╣
  ║    ║    ║    ║    ║    ║
  ╠════╬════╬════╬════╬════╣
  ║    ║    ║    ║    ║    ║
  ╠════╬════╬════╬════╬════╣
  ║    ║    ║    ║    ║    ║
  ╚════╩════╩════╩════╩════╝

Larghezza = 4
Altezza = 1
  ╔════╦════╦════╦════╗
  ║    ║    ║    ║    ║
  ╚════╩════╩════╩════╝

La funzione deve essere la più corta possibile.

(define (griglia width height)
  (let ( (top    (string "╔══" (dup "══╦══" (- width 1)) "══╗"))
         (mid    (string (dup "║    " width) "║"))
         (inter  (string "╠══" (dup "══╬══" (- width 1)) "══╣"))
         (bottom (string "╚══" (dup "══╩══" (- width 1)) "══╝")) )
    (println top "\n" mid)
    (dotimes (x (- height 1)) (println inter "\n" mid))
    (println bottom) '>))

Proviamo:

(griglia 1 1)
(griglia 5 4)
(griglia 2 1)
(griglia 8 8)

Versione code-golf (217 caratteri - one line):

(define(g w h)
(letn((z println)(k string)
(t(k"╔══"(dup"══╦══"(- w 1))"══╗"))
(m(k(dup"║    "w)"║"))
(i(k"╠══"(dup"══╬══"(- w 1))"══╣"))
(b(k"╚══"(dup"══╩══"(- w 1))"══╝")))
(z t"\n"m)(dotimes(x(- h 1))(z i"\n"m))(z b)'>))

Proviamo:

(g 1 1)
;-> ╔════╗
;-> ║    ║
;-> ╚════╝

(g 5 4)
;-> ╔════╦════╦════╦════╦════╗
;-> ║    ║    ║    ║    ║    ║
;-> ╠════╬════╬════╬════╬════╣
;-> ║    ║    ║    ║    ║    ║
;-> ╠════╬════╬════╬════╬════╣
;-> ║    ║    ║    ║    ║    ║
;-> ╠════╬════╬════╬════╬════╣
;-> ║    ║    ║    ║    ║    ║
;-> ╚════╩════╩════╩════╩════╝

(g 2 1)
;-> ╔════╦════╗
;-> ║    ║    ║
;-> ╚════╩════╝

(g 8 8)
;-> ╔════╦════╦════╦════╦════╦════╦════╦════╗
;-> ║    ║    ║    ║    ║    ║    ║    ║    ║
;-> ╠════╬════╬════╬════╬════╬════╬════╬════╣
;-> ║    ║    ║    ║    ║    ║    ║    ║    ║
;-> ╠════╬════╬════╬════╬════╬════╬════╬════╣
;-> ║    ║    ║    ║    ║    ║    ║    ║    ║
;-> ╠════╬════╬════╬════╬════╬════╬════╬════╣
;-> ║    ║    ║    ║    ║    ║    ║    ║    ║
;-> ╠════╬════╬════╬════╬════╬════╬════╬════╣
;-> ║    ║    ║    ║    ║    ║    ║    ║    ║
;-> ╠════╬════╬════╬════╬════╬════╬════╬════╣
;-> ║    ║    ║    ║    ║    ║    ║    ║    ║
;-> ╠════╬════╬════╬════╬════╬════╬════╬════╣
;-> ║    ║    ║    ║    ║    ║    ║    ║    ║
;-> ╠════╬════╬════╬════╬════╬════╬════╬════╣
;-> ║    ║    ║    ║    ║    ║    ║    ║    ║
;-> ╚════╩════╩════╩════╩════╩════╩════╩════╝


------------------
Primi e palindromi
------------------

Alcune sequenze su numeri primi  e palindromi.

Sequenza OEIS A038582:
Numbers k such that sum of the first k primes is a palindrome.
  1, 2, 8, 7693, 8510, 12941, 146134, 637571, 27198825, 53205635,
  6283318531, 7167375533, 226095996998, 435966708249, ...
8 si trova nella sequenza perchè la somma dei primi 8 primi 2+3+5+7+11+13+17+19 = 77 è palindromo.

Sequenza OEIS A038583:
Palindromes that are the sum of consecutive initial primes.
  2, 5, 77, 285080582, 352888253, 854848458, 137372273731, 2939156519392,
  6833383883833386, 27155268786255172, 477749724515427947774,
  625179415050514971526, 714014821826628128410417,
  2719564270866680724659172,...
77 si trova nella sequenza perchè 2+3+5+7+11+13+17+19 = 77 è palindromo

Sequenza OEIS A038584:
Primes p such that the sum of the primes from 2 through p is a palindrome.
  2, 3, 19, 78347, 87641, 139241, 1959253, 9564097, 516916921, 1048924213, 155353386241, 178196630873, 6433462703963, 12702232868389
19 si trova nella sequenza perchè 2 + 3 + 5 + 7 + 11 + 13 + 17 + 19 = 77 è palindromo.

(define (reverse-digits num)
"Reverse the digits of an integer"
  (int (reverse (string num)) 0 10))

(define (primes num-primes)
"Generate a given number of prime numbers (starting from 2)"
  (let ( (k 3) (tot 1) (out '(2)) )
    (until (= tot num-primes)
      (when (= 1 (length (factor k)))
        (push k out -1)
        (++ tot))
      (++ k 2))
    out))

Possiamo scrivere una funzione unica per tutte e tre le sequenze:

(define (seq-all limite)
  (local (out primi sum)
    (setq out '())
    (setq primi (primes limite))
    (setq sum 0)
    (dolist (p primi)
      (++ sum p)
      (if (= sum (reverse-digits sum))
          ; A038582 --> (+ $idx 1)
          ; A038583 --> sum
          ; A038584 --> p
          (push (list (+ $idx 1) sum p) out -1)))
    out))

(time (println (setq lst (seq-all 1e6))))
;-> ((1 2 2) (2 5 3) (8 77 19) (7693 285080582 78347) (8510 352888253 87641)
;->  (12941 854848458 139241) (146134 137372273731 1959253)
;->  (637571 2939156519392 9564097))
;-> 18655.959

A038582
(map first lst)
;-> (1 2 8 7693 8510 12941 146134 637571)

A038583
(map (fn(x) (x 1)) lst)
;-> (2 5 77 285080582 352888253 854848458 137372273731 2939156519392)

A038584
(map last lst)
;-> (2 3 19 78347 87641 139241 1959253 9564097)


----------------------------------------
Numeri con cifre crescenti e decrescenti
----------------------------------------

Dati due numeri interi positivi a e b (con b > a), determinare tutti i numeri fra a e b (compresi) che hanno cifre crescenti.
Esempi:
N = 122 (cifre crescenti)
N = 131 (cifre non crescenti)

Sequenza OEIS A009994:
Numbers with digits in nondecreasing order.
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 22,
  23, 24, 25, 26, 27, 28, 29, 33, 34, 35, 36, 37, 38, 39, 44, 45, 46,
  47, 48, 49, 55, 56, 57, 58, 59, 66, 67, 68, 69, 77, 78, 79, 88, 89,
  99, 111, 112, 113, 114, 115, 116, 117, 118, 119, 122, ...

Dati due numeri interi positivi a e b (con b > a), determinare tutti i numeri fra a e b (compresi) che hanno cifre decrescenti.
Esempi:
N = 311 (cifre decrescenti)
N = 131 (cifre non decrescenti)

Sequenza OEIS A009996:
Numbers with digits in nonincreasing order.
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 20, 21, 22, 30, 31, 32, 33, 40,
  41, 42, 43, 44, 50, 51, 52, 53, 54, 55, 60, 61, 62, 63, 64, 65, 66, 70,
  71, 72, 73, 74, 75, 76, 77, 80, 81, 82, 83, 84, 85, 86, 87, 88, 90, 91,
  92, 93, 94, 95, 96, 97, 98, 99, 100, 110, 111, 200, 210, 211, ...

(define (decrescente? num)
  (if (< num 10) true
      ;else
        (let ((prev 0) (stop nil))
          ; controlliamo dalla cifra più a destra
          ; quindi le cifre devono essere crescenti
          ; cifra precedente = 0
          (while (and (!= num 0) (not stop))
            ;(print (% num 10) { } prev) (read-line)
            ; controllo se l'ordinamento è decrescente
            (if (< (% num 10) prev)
                (setq stop true)
                ;else
                (set 'prev (% num 10) 'num (/ num 10))))
          (not stop))))

(decrescente? 211)
;-> true
(decrescente? 2113)
;-> nil
(decrescente? 1234)
;-> nil
(decrescente? 1111)
;-> true

(define (crescente? num)
  (if (< num 10) true
      ;else
      (let ((prev 9) (stop nil))
        ; controlliamo dalla cifra più a destra
        ; quindi le cifre devono essere decrescenti
        ; cifra precedente = 9
        (while (and (!= num 0) (not stop))
          ;(print (% num 10) { } prev) (read-line)
          ; controllo se l'ordinamento è crescente
          (if (> (% num 10) prev)
              (setq stop true)
              ;else
              (set 'prev (% num 10) 'num (/ num 10))))
        (not stop))))

(crescente? 233)
;-> true
(crescente? 2113)
;-> nil
(crescente? 1234)
;-> true
(crescente? 1111)
;-> true

(define (trova-crescenti a b) (filter crescente? (sequence a b)))

(trova-crescenti 10 30)
;-> (11 12 13 14 15 16 17 18 19 22 23 24 25 26 27 28 29)

(trova-crescenti 0 122)
;-> (0 1 2 3 4 5 6 7 8 9 11 12 13 14 15 16 17 18 19 22
;->  23 24 25 26 27 28 29 33 34 35 36 37 38 39 44 45 46
;->  47 48 49 55 56 57 58 59 66 67 68 69 77 78 79 88 89
;->  99 111 112 113 114 115 116 117 118 119 122)

(length (trova-crescenti 0 10000))
;-> 715

(define (trova-decrescenti a b) (filter decrescente? (sequence a b)))

(trova-decrescenti 10 30)
;-> (10 11 20 21 22 30)

(trova-decrescenti 0 211)
;-> (0 1 2 3 4 5 6 7 8 9 10 11 20 21 22 30 31 32 33 40
;->  41 42 43 44 50 51 52 53 54 55 60 61 62 63 64 65 66 70
;->  71 72 73 74 75 76 77 80 81 82 83 84 85 86 87 88 90 91
;->  92 93 94 95 96 97 98 99 100 110 111 200 210 211)

(length (trova-decrescenti 0 10000))
;-> 998

Vedi anche "Numeri metadrome, plaindrome, nialpdrome e katadrome" su "Note libere 15".


--------------------------------------------------------
Deviazione standard della somma dei primi K numeri primi
--------------------------------------------------------

Calcolare la sequenza delle deviazioni standard della somma dei primi K numeri primi.

Funzione per il calcolo della media di una lista di numeri:

(define (media lst)
  (div (apply add lst) (length lst)))

Funzione per il calcolo della deviazione standard di una lista di numeri:

(define (devst lst) ; diviso N
  (let (m (media lst))
    (sqrt (div (apply add (map (fn(x) (mul (sub x m) (sub x m))) lst))
               (length lst)))))

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

Funzione che calcola le deviazioni standard delle somme cumulate dei numeri primi fino ad un dato limite:

(define (primi-devstd limite)
  (local (out primi sum)
    (setq out '())
    (setq primi (primes-to limite))
    (setq cur '())
    (dolist (p primi)
      (push p cur -1)
      (push (devst cur) out -1))
    (println (length primi))
    out))

(primi-devstd 100)
;-> 25
;-> (0 0.5 1.247219128924647 1.920286436967152 3.2 4.017323597731316
;->  5.146823867043378 5.977823600609171 7.030796453136166 8.560957890329798
;->  9.680004098079865 11.15391062462947 12.55991557654513 13.67199580850415
;->  14.81830699581508 16.20655249428453 17.76937998501645 19.05037182489273
;->  20.50133432587436 21.89857301287004 23.08969005526216 24.43633150701351
;->  25.75300742251957 27.19269586365672 28.88044320989551)

Calcoliamo la differenza tra coppie di deviazioni standard consecutive:

(silent
  (setq ds (primi-devstd 1e5))
  (setq diff (map sub (rest ds) (chop ds))))

(apply max diff)
;-> 3.385527391139476
(apply min diff)
;-> 0.5

(slice diff 0 10)
;-> (0.5 0.747219128924647 0.6730673080425049 1.279713563032848
;->  0.817323597731316 1.129500269312062 0.8309997335657933 1.052972852526995
;->  1.530161437193631 1.119046207750067)

(slice diff -10 10)
;-> (3.372961606048193 3.3715936630324 3.373212165715813 3.372217523257859
;->  3.373089144122787 3.372094767983072 3.37595129852707 3.375701852084603
;->  3.376945016683749 3.375202878207347)

Differenza delle differenze:

(setq diff2 (map sub (rest diff) (chop diff)))

(slice diff2 0 5)
;-> (0.247219128924647 -0.07415182088214212 0.6066462549903433
;->  -0.4623899653015322 0.3121766715807457)

(slice diff2 -5 5)
;-> (-0.0009943761397153139 0.003856530543998815 -0.0002494464424671605
;->  0.00124316459914553 -0.001742138476402033)


------------------
Persona più vicina
------------------

Ci sono tre persone che si trovano lungo una linea (retta numerica) alle posizioni intere x, y e z:
- x è la posizione della Persona 1 (che si muove verso la Persona 3).
- y è la posizione della Persona 2 (che si muove verso la Persona 3).
- z è la posizione della Persona 3 (che non si muove).
Sia la Persona 1 che la Persona 2 si muovono verso la Persona 3 alla stessa velocità.
Determinare quale persona raggiunge per prima la Persona 3:
- Restituire 0 se entrambe arrivano contemporaneamente.
- Restituire 1 se la Persona 1 arriva per prima.
- Restituire 2 se la Persona 2 arriva per prima.

(define (vicina a b c)
  (let ( (ac (abs (- a c))) (bc (abs (- b c))) )
    (cond ((< ac bc) 1) ((> ac bc) 2) (true 0))))

(vicina -3 3 -1)
;-> 1
(vicina -3 -3 -3)
;-> 0

Adesso scriviamo una funzione che accetta una lista di N persone/posizioni con numeri reali e restituisce tutte le persone più vicine alla persona ferma.
La persona ferma (target) è l'ultima della lista.

(define (vicinaN lst)
  (local (out target min-dist dist)
    (setq out '())
    (setq target (pop lst -1))
    (setq min-dist (abs (sub target (lst 0))))
    (dolist (el lst)
      (setq dist (abs (sub target el)))
      (cond ((= dist min-dist)
              ; distanza uguale --> aggiungo alla lista
              (push (list el $idx) out -1))
            ((< dist min-dist)
              ; distanza minore --> aggiorno distanza minore e creo nuova lista
              (setq min-dist dist)
              (setq out (list (list el $idx))))))
    (println "Posizione Target: " target)
    (println "Distanza minima: " min-dist)
    (println "Persone a distanza minima (posizione indice):")
    out))

Proviamo:

(setq lst '(1 2 3 4 5 6 7 11 9))
(vicinaN lst)
;-> Posizione Target: 9
;-> Distanza minima: 2
;-> Persone a distanza minima (posizione indice):
;-> ((7 6) (11 7))

(setq lst '(-3 3 3 -3 3 -3 0))
(vicinaN lst)
;-> Posizione Target: 0
;-> Distanza minima: 3
;-> Persone a distanza minima (posizione indice):
;-> ((-3 0) (3 1) (3 2) (-3 3) (3 4) (-3 5))

(setq lst (rand 100 100))
(vicinaN lst)
;-> Posizione Target: 50
;-> Distanza minima: 1
;-> Persone a distanza minima (posizione indice):
;-> ((51 13) (51 38) (51 90) (49 96))


--------------------------------
Cifre uguali dopo trasformazione
--------------------------------

Dato un numero intero N di K cifre eseguire ripetutamente la seguente operazione finché il numero non ha esattamente due cifre:
 1) Per ogni coppia di cifre consecutive in N, a partire dalla prima cifra, calcolare una nuova cifra come somma delle due cifre modulo 10.
 2) Unire le cifre calcolate, mantenendo l'ordine in cui sono state prodotte.
Al termine restituire true se le due cifre che rimangono sono uguali, altrimenti restituire nil.

Esempio:

N = 3911
(3 + 9) % 10 = 2
(9 + 1) % 10 = 0
(1 + 1) % 10 = 2
N = 202
(2 + 0) % 10 = 2
(0 + 2) % 10 = 2
N = 22  --> true

(define (int-list num)
"Convert an integer to a list of digits"
  (if (zero? num) '(0)
  (let (out '())
    (while (!= num 0)
      (push (% num 10) out)
      (setq num (/ num 10))) out)))

(define (go num)
  (let ( (len (length num)) (digits (int-list num)) )
    (while (> len 2)
      (setq digits (map (fn(x y) (% (+ x y) 10)) (rest digits) (chop digits)))
      ;(print digits) (read-line)
      (-- len))
    (if (= (digits 0) (digits 1))
        (list (+ (* 10 (digits 0)) (digits 1)) true)
        (list (+ (* 10 (digits 0)) (digits 1)) nil))))

(go 3911)
;-> (22 true)
(go 3902L)
;-> (11 true)
(go 34789L)
;-> (48 nil)
(go 39023547345737L)
;-> (83 nil)

Proviamo con numeri grandi:

(define (fact-i num)
"Calculate the factorial of an integer number"
  (if (zero? num)
      1
      (let (out 1L)
        (for (x 1L num)
          (setq out (* out x))))))

(length (fact-i 200))
;-> 375
(time (println (go (fact-i 200))))
;-> (82 nil)
;-> 39.615

(length (fact-i 1000))
;-> 2568
(time (println (go (fact-i 1000))))
;-> (66 true)
;-> 1587.675

(length (fact-i 3250))
;-> 10005
(time (println (go (fact-i 3250))))
;-> (14 nil)
;-> 24256.541


-------------------
Numeri k-palindromi
-------------------

Un numero intero è k-palindromo se è palindromo ed è divisibile da k.
Dato k e un numero di cifre N, determinare il palindromo più grande con N cifre divisibile da k.
Vincoli:
  1 <= N <= 10^5
  1 <= K <= 9

(define (reverse-digits num)
"Reverse the digits of an integer"
  (int (reverse (string num)) 0 10))

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(define (k-pal n k)
  (let ( (out nil) (stop nil)
         (max-number (- (** 10 n) 1))
         (min-number (- (** 10 (- n 1)) 1)) )
  (for (i max-number min-number -1 stop)
    (when (and (zero? (% i k)) (= i (reverse-digits i)))
          (setq out i) (setq stop true)))
  out))

Proviamo:

(k-pal 3 5)
;-> 595

(k-pal 1 4)
;-> 8

(k-pal 5 6)
;-> 89898

La funzione è molto lenta per valori di N maggiori di 8.

(time (println (k-pal 6 5)))
;-> 599995
;-> 130.082

(time (println (k-pal 7 5)))
;-> 5999995
;-> 1270.995

(time (println (k-pal 8 5)))
;-> 59999995
;-> 12792.944

Con N = 10^5 la funzione non termina prima di anni...

N=8
(time (for (i 1 1e8) (setq a 1)))
;-> 2534
N=9
(time (for (i 1 1e9) (setq a 1)))
;-> 25219.387

Per ogni incremento di N, il tempo viene moltiplicato per 10.
Considerando che l'aumento sia lineare (ma non lo è):
(mul 25.219387 10 (- 10e5 9))
;-> 252191600.25517 ;secondi
(div 252191600.25517 3600)
;-> 70053.22229310278 ; ore
(div 70053.22229310278 24)
;-> 2918.884262212616 ; giorni
(div 2918.884262212616 365)
;-> 7.996943184144153 ; anni

Analizziamo quali sono i numeri k-pal per N che va da 1 a 9 con numeri da 1 a K cifre:

(define (lista k-max n-max)
  (setq out '())
  (for (k 1 k-max)
    (setq tmp (list k))
    (for (n 1 n-max)
      (push (list n (k-pal n k)) tmp -1))
    (push tmp out -1))
  out)

(lista 9 7)
;-> ((1 (1 9) (2 99) (3 999) (4 9999) (5 99999) (6 999999) (7 9999999))
;->  (2 (1 8) (2 88) (3 898) (4 8998) (5 89998) (6 899998) (7 8999998))
;->  (3 (1 9) (2 99) (3 999) (4 9999) (5 99999) (6 999999) (7 9999999))
;->  (4 (1 8) (2 88) (3 888) (4 8888) (5 88988) (6 889988) (7 8899988))
;->  (5 (1 5) (2 55) (3 595) (4 5995) (5 59995) (6 599995) (7 5999995))
;->  (6 (1 6) (2 66) (3 888) (4 8778) (5 89898) (6 897798) (7 8998998))
;->  (7 (1 7) (2 77) (3 959) (4 9779) (5 99799) (6 999999) (7 9994999))
;->  (8 (1 8) (2 88) (3 888) (4 8888) (5 88888) (6 888888) (7 8889888))
;->  (9 (1 9) (2 99) (3 999) (4 9999) (5 99999) (6 999999) (7 9999999)))

Analizzando i valori possiamo costruire il numero k-palindromo.

(define (k-pal2 n k)
  (local (out l q r)
    (cond ((= k 1) (setq out (dup "9" n)))
          ((= k 2)
            (if (< n 3)
                (setq out (dup "8" n))
                ;else
                (setq out (string "8" (dup "9" (- n 2)) "8"))))
          ((= k 3) (setq out (dup "9" n)))
          ((= k 4)
            (if (< n 5)
                (setq out (dup "8" n))
                ;else
                (setq out (string "88" (dup "9" (- n 4)) "88"))))
          ((= k 5)
            (if (< n 3)
                (setq out (dup "5" n))
                ;else
                (setq out (string "5" (dup "9" (- n 2)) "5"))))
          ((= k 6)
            (cond ((< n 3)
                    (setq out (dup "6" n)))
                  ((= (% n 2) 1)
                    (setq l (- (/ n 2) 1))
                    (setq out (string "8" (dup "9" l) "8" (dup "9" l) "8")))
                  (true
                    (setq l (- (/ n 2) 2))
                    (setq out (string "8" (dup "9" l) "77" (dup "9" l) "8")))))
          ((= k 7)
            (setq mid '("" "7" "77" "959"
                        "9779" "99799" "999999" "9994999"
                        "99944999" "999969999" "9999449999" "99999499999"))
            (setq q (/ n 12))
            (setq r (% n 12))
            (setq out (string (dup "999999" q) (mid r) (dup "999999" q))))
          ((= k 8)
            (if (< n 7)
                (setq out (dup "8" n))
                ;else
                (setq out (string "888" (dup "9" (- n 6)) "888"))))
          ((= k 9) (setq out (dup "9" n)))
    )
    out))

Proviamo:

(k-pal2 3 5)
;-> "595"

(k-pal2 1 4)
;-> "8"

(k-pal2 5 6)
;-> "89898"

(time (println (k-pal2 6 5)))
;-> "599995"
;-> 0

(time (println (k-pal2 7 5)))
;-> "5999995"
;-> 0

(time (println (k-pal2 8 5)))
;-> "59999995"
;-> "1.009"

(k-pal2 100 7)
;-> "99999999999999999999999999999999999999999999999997
;->  79999999999999999999999999999999999999999999999999"

(define (lista2 k-max n-max)
  (setq out '())
  (for (k 1 k-max)
    (setq tmp (list k))
    (for (n 1 n-max)
      (push (list n (k-pal2 n k)) tmp -1))
    (push tmp out -1))
  out)

(lista2 9 7)
;-> ((1 (1 "9") (2 "99") (3 "999") (4 "9999") (5 "99999") (6 "999999") (7 "9999999"))
;->  (2 (1 "8") (2 "88") (3 "898") (4 "8998") (5 "89998") (6 "899998") (7 "8999998"))
;->  (3 (1 "9") (2 "99") (3 "999") (4 "9999") (5 "99999") (6 "999999") (7 "9999999"))
;->  (4 (1 "8") (2 "88") (3 "888") (4 "8888") (5 "88988") (6 "889988") (7 "8899988"))
;->  (5 (1 "5") (2 "55") (3 "595") (4 "5995") (5 "59995") (6 "599995") (7 "5999995"))
;->  (6 (1 "6") (2 "66") (3 "888") (4 "8778") (5 "89898") (6 "897798") (7 "8998998"))
;->  (7 (1 "7") (2 "77") (3 "959") (4 "9779") (5 "99799") (6 "999999") (7 "9994999"))
;->  (8 (1 "8") (2 "88") (3 "888") (4 "8888") (5 "88888") (6 "888888") (7 "8889888"))
;->  (9 (1 "9") (2 "99") (3 "999") (4 "9999") (5 "99999") (6 "999999") (7 "9999999")))


----------------
Nave e container
----------------

Una nave ha NxN celle per container.
Ogni cella può contenere un container con peso massimo pari a W.
La nave può caricare al massimo un peso totale pari a P.
Dati N, P e W, determinare quanti container può caricare la nave.

Quanti container al massimo può contenere la nave (spazio)? A = NxN
Quanti container al massimo può contenere la nave (peso)?   B = W / P
Quindi il numero di container è il valore minimo tra A e B.

(define (ship N P W) (min (* N N) (/ W P)))

Proviamo:

(ship 2 3 15)
;-> 4
(ship 4 6 10)
;-> 1
(ship 3 5 20)
;-> 4
(ship 10 15 200)
;-> 13


-------------------------------------------------------------------
Rimozione continua e decrescente di K carte da un mazzo con N carte
-------------------------------------------------------------------

Il gioco si svolge tra due giocatori A e B con un mazzo di N carte.
Inizialmente il giocatore A rimuove le prime K carte dal mazzo, poi il giocatore B rimuove le prime (K - 1) carte del mazzo.
Il gioco continua in questo modo, ed ogni volta il numero di carte da rimuovere viene diminuito di 1.
Il primo giocatore che non può muovere (perchè non ci sono carte a sufficienza da rimuovere) perde.
Se un giocatore raggiunge K = 0 (cioè deve rimuovere 0 carte) e ci sono ancora carte nel mazzo, allora il gioco finisce in pareggio.
Se un giocatore raggiunge K = 1, e il mazzo ha una sola carta, allora il gioco finisce in pareggio (perchè l'altro giocatore dovrebbe rimuovere 0 carte infatti per lui K = 0).

Dati N e K determinare il vincitore del gioco.

(define (game n k)
  (let (out "AB")
        ; (n > (somma dei primi k numeri)) ?
    (if (>= n (/ (* k (+ k 1)) 2)) ; sempre pareggio
        (setq out "AB")
        ;else
        (let (continua true)
          (++ k)
          (while (and (>= n 0) (>= k 0) continua)
            (-- k)
            (-- n k)
            (when (< n 0)
            ;(println n { } k)
              (setq continua nil)
              (setq out "B"))
            (when continua
              (-- k)
              (-- n k)
              (when (< n 0)
              ;(println n { } k))
                (setq continua nil)
                (setq out "A"))))))
    out))

Proviamo:

(game 12 10)
;-> "A"
(game 55 10)
;-> "AB"
(game 100 10)
;-> "AB"
(game 21 10)
;-> "B"


----------------
Formaggio e topi
----------------

Abbiamo una certa quantità totale di formaggio (kg).
Alcuni topi mangiano il formaggio contemporaneamente e riducono la quantità di formaggio.
Per mangiare X kg di formaggio i topi impiegano:
  (timeTopo(i)*1 + timeTopo(i)*2 + ... + timeTopo(i)*X) secondi.
Esempio:
Per mangiare 1kg di formaggio occorrono timeTopo(i) secondi,
per mangiare 2kg di formaggio occorrono timeTopo(i) + timeTopo(i)*2 secondi,
e così via.
Data la quantità totale di formaggio Q e una lista con i tempi di lavoro di ogni topo determinare il numero di secondi necessario affinchè i topi mangino tutto il formaggio.

Per risolvere il problema eseguiamo una ricerca binaria sul massimo numero di secondi per verificare se è sufficiente mangiare tutto il formaggio con tutti i topi che mangiano contemporaneamente.

I kg 'x' che un topo con tempo di lavoro 't' mangia in 'k' secondi valgono:

  t * (1 + 2 + ... + x) <= k
          (1 + x) * x/2 <= k/t
      x^2 + x - 2 * k/t <= 0
                      x <= (sqrt(1 + 8 * k / t) - 1)/2

(define (secondi totale topo)
  (local (minimo sx dx)
    (setq minimo (apply min topo))
    (setq sx 1)
    (setq dx (/ (* minimo totale (+ totale 1) 2)))
    (while (< sx dx)
      (setq m (/ (+ sx dx) 2))
      (if (< (mangia topo m) totale)
          (setq sx (+ m 1))
          (setq dx m)))
    sx))

(define (mangia topo m)
  (let (qty 0)
    (dolist (t topo)
      (++ qty (int (div (add (sqrt (add 1 (div (mul 8 m) t))) -1) 2))))
    qty))

Proviamo:

(secondi 20 '(1 1 1 1))
;-> 15
(secondi 20 '(1 1 1))
;-> 28
(secondi 4 '(2 1 1))
;-> 3
(seconds 10 '(3 2 2 4))
;-> 12
(secondi 5 '(1))
;-> 15


-------------------------
Tre scelte con una moneta
-------------------------

Abbiamo tre vernici di colore diverso e vogliamo pitturare una stanza usando un solo colore scelto a caso. Con noi abbiamo solo una moneta (Testa/Croce).
Come utilizzare la moneta per scegliere un colore a caso (con probabilità uguali)?

Basta lanciare la moneta due volte per avere 4 eventi distinti:
1) Testa-Croce
2) Testa-Testa
3) Croce-Testa
4) Croce-Croce

Poi abbiniamo i colori (es. Rosso, Giallo, Blu) ai primi 3 eventi:
1) Testa-Croce --> Rosso
2) Testa-Testa --> Giallo
3) Croce-Testa --> Blu
Se esce il quarto evento, allora ripetiamo il processo:
4) Croce-Croce --> Rilanica la moneta 2 volte

Testa = 0
Croce = 1

(define (moneta3)
  (let ( (a (rand 2)) (b (rand 2)) )
    (cond ((and (= a 0) (= b 1)) 'Rosso)  ; TC
          ((and (= a 0) (= b 0)) 'Giallo) ; TT
          ((and (= a 1) (= b 0)) 'Blu)    ; TT
          (true (moneta3)))))

Proviamo:

(collect (moneta3) 10)
;-> (Blu Rosso Blu Rosso Blu Giallo Blu Giallo Blu Giallo)

Verifichiamo le probabilità di ogni colore:

(count (list 'Rosso 'Giallo 'Blu) (collect (moneta3) 1e5))
;-> (33273 33245 33482)


---------------------------------------------
Creazione di intervalli con numeri di k cifre
---------------------------------------------

Dato un numero di cifre k, determinare tutti gli intervalli numerici (valore minimo, valore massimo) che hanno cifre da 1 a k.

Esempio:
  k = 3
  Intervallo numerico con 1 cifra: (1 9)
  Intervallo numerico con 2 cifre: (10 99)
  Intervallo numerico con 3 cifre: (100 999)
Output: (1 9) (10 99) (100 999)

Inoltre la funzione deve prendere un parametro ('odd o 'even) che specifica se le cifre devono essere in numero dispari o in numero pari.

Esempio:
  k = 3, dispari
  Intervallo numerico con 1 cifra: (1 9),     dispari? si
  Intervallo numerico con 2 cifre: (10 99),   dispari? no
  Intervallo numerico con 3 cifre: (100 999), dispari? si
  Output: (1 9) (100 999)

(define (range-k-digits k type)
  ; caso speciale
  (if (= k 1)
      ; gestione dei casi speciali
      (cond ((= type 'odd) (list 1 9)) ; un solo intervallo se dispari
            ((= type 'even) '())       ; nessun intervallo se pari
            (true (list 1 9)))         ; tutti gli intervalli: 1
  ; else: costruiamo la lista completa di intervalli
      (let ( (all '()) (minimo 0) (massimo 0) )
        ; per ogni numero di cifre da 1 a k
        (for (i 1 k) ; i = numero di cifre
          ; calcoliamo minimo e massimo dell’intervallo
          (setq minimo (pow 10 (sub i 1)))
          (setq massimo (sub (pow 10 i) 1))
          ; aggiungiamo l’intervallo (minimo, massimo), tagliato da max-num
          (push (list minimo massimo) all -1)
        )
        ;(println all { } (length all))
        ; selezione finale: dispari, pari, o tutti
        (cond ((= type 'odd)
                (select all (sequence 0 (- (length all) 1) 2))) ; cifre dispari
              ((= type 'even)
                (select all (sequence 1 (- (length all) 1) 2))) ; cifre pari
              (true all))))) ; tutti gli intervalli

Proviamo:

(range-k-digits 1)
;-> (1 9)
(range-k-digits 3)
;-> ((1 9) (10 99) (100 999))
(range-k-digits 10)
;-> ((1 9) (10 99) (100 999) (1000 9999) (10000 99999) (100000 999999)
;->  (1000000 9999999) (10000000 99999999) (100000000 999999999)
;->  (1000000000 9999999999))
(range-k-digits 10 'odd)
;-> ((1 9) (100 999) (10000 99999) (1000000 9999999) (100000000 999999999))
(range-k-digits 10 'even)
;-> ((10 99) (1000 9999) (100000 999999) (10000000 99999999)
;->  (1000000000 9999999999))

Adesso scriviamo una funzione che prende un valore massimo come limite dell'ultimo intervallo (invece del numero di cifre k).
In questo caso dobbiamo calcolare k e considerare quando inserire il valore massimo nell'ultimo intervallo.

(define (range-digits max-num type)
  ; Se il numero massimo è minore di 10
  (if (< max-num 10)
      ; gestione dei casi speciali
      (cond ((= type 'odd) (list 1 max-num)) ; un solo intervallo se dispari
            ((= type 'even) '())             ; nessun intervallo se pari
            (true (list 1 max-num)))         ; tutti gli intervalli: 1
  ; else: costruiamo la lista completa di intervalli
      (let ( (all '()) (minimo 0) (massimo 0) (k 0) )
        ; k = numero di cifre del numero massimo
        (setq k (ceil (log max-num 10)))
        ; aumenta k se max-num è una potenza di 10
        (if (= max-num (pow 10 k)) (++ k))
        ; per ogni numero di cifre da 1 a k
        (for (i 1 k) ; i = numero di cifre
          ; calcoliamo minimo e massimo dell’intervallo
          (setq minimo (pow 10 (sub i 1)))
          (setq massimo (sub (pow 10 i) 1))
          ; aggiungiamo l’intervallo (minimo, massimo), tagliato da max-num
          (push (list minimo (min max-num massimo)) all -1)
        )
        ;(println all { } (length all))
        ; selezione finale: dispari, pari, o tutti
        (cond ((= type 'odd)
                (select all (sequence 0 (- (length all) 1) 2))) ; cifre dispari
              ((= type 'even)
                (select all (sequence 1 (- (length all) 1) 2))) ; cifre pari
              (true all))))) ; tutti gli intervalli

Proviamo:

(range-digits 1000)
;-> ((1 9) (10 99) (100 999) (1000 1000))
(range-digits 1000 'odd)
;-> ((1 9) (100 999))
(range-digits 1000 'even)
;-> ((10 99) (1000 1000))
(range-digits 100)
;-> ((1 9) (10 99) (100 100))
(range-digits 100 'odd)
;-> ((1 9) (100 100))
(range-digits 100 'even)
;-> ((10 99))
(range-digits 99)
;-> ((1 9) (10 99))
(range-digits 99 'odd)
;-> ((1 9))
(range-digits 99 'even)
;-> ((10 99))
(range-digits 123456)
;-> ((1 9) (10 99) (100 999) (1000 9999) (10000 99999) (100000 123456))
(range-digits 123456 'odd)
;-> ((1 9) (100 999) (10000 99999))
(range-digits 123456 'even)
;-> ((10 99) (1000 9999) (100000 123456))
(range-digits 12345)
;-> ((1 9) (10 99) (100 999) (1000 9999) (10000 12345))
(range-digits 12345 'odd)
;-> ((1 9) (100 999) (10000 12345))
(range-digits 12345 'even)
;-> ((10 99) (1000 9999))
(range-digits 1)
;-> (1 1)
(range-digits 4 'odd)
;-> (1 4)
(range-digits 8 'even)
;-> ()


-----------------------------------------------------------------
Numeri con numero di cifre pari uguale al numero di cifre dispari
-----------------------------------------------------------------

Determinare la sequenza dei numeri che hanno un numero di cifre pari uguale al numero di cifre dispari.

Sequenza OEIS A227870:
Numbers with equal number of even and odd digits.
  10, 12, 14, 16, 18, 21, 23, 25, 27, 29, 30, 32, 34, 36, 38, 41, 43, 45,
  47, 49, 50, 52, 54, 56, 58, 61, 63, 65, 67, 69, 70, 72, 74, 76, 78, 81,
  83, 85, 87, 89, 90, 92, 94, 96, 98, 1001, 1003, 1005, 1007, 1009, 1010,
  1012, 1014, 1016, 1018, 1021, 1023, 1025, ...

(define (pari-dispari num)
  (if (odd? (length num)) ; se abbiamo un numero di cifre dispari, allora nil
      nil
      ;else
      (let ( (pari 0) (dispari 0))
        (while (!= num 0)
          (if (even? (% num 10))
              (++ pari)
              (++ dispari))
          (setq num (/ num 10)))
        (= pari dispari))))

(filter pari-dispari (sequence 1  1025))
;-> (10 12 14 16 18 21 23 25 27 29 30 32 34 36 38 41 43 45
;->  47 49 50 52 54 56 58 61 63 65 67 69 70 72 74 76 78 81
;->  83 85 87 89 90 92 94 96 98 1001 1003 1005 1007 1009 1010
;->  1012 1014 1016 1018 1021 1023 1025)


-----------------------------------------------------------------------
Numeri con somma delle cifre pari uguale alla somma delle cifre dispari
-----------------------------------------------------------------------

Determinare la sequenza dei numeri che hanno somma delle cifre pari uguale alla somma delle cifre dispari.

Sequenza OEIS A036301:
Numbers whose sum of even digits and sum of odd digits are equal.
  0, 112, 121, 134, 143, 156, 165, 178, 187, 211, 314, 336, 341, 358, 363,
  385, 413, 431, 516, 538, 561, 583, 615, 633, 651, 718, 781, 817, 835,
  853,  871, 1012, 1021, 1034, 1043, 1056, 1065, 1078, 1087, 1102, 1120,
  1201, 1210, 1223, 1232, 1245, 1254, 1267, 1276, 1289, 1298, ...

(define (sum-pari-dispari num)
  (let ( (pari 0) (dispari 0) (cifra 0) )
    (while (!= num 0)
      (if (even? (setq cifra (% num 10)))
          (++ pari cifra)
          (++ dispari cifra))
      (setq num (/ num 10)))
    (= pari dispari)))

(filter sum-pari-dispari (sequence 0 1298))
;-> (0 112 121 134 143 156 165 178 187 211 314 336 341 358 363
;->  385 413 431 516 538 561 583 615 633 651 718 781 817 835
;->  853 871 1012 1021 1034 1043 1056 1065 1078 1087 1102 1120
;->  1201 1210 1223 1232 1245 1254 1267 1276 1289 1298)


------------------------------------
Numeri primi in una stringa di cifre
------------------------------------

Data una stringa di cifre, determinare tutti i numeri primi contenuti nella stringa come sottostringa.

Esempio
  stringa = "12234"
  primi = 2 3 23 223 1223

Sequenza OEIS A039997:
Number of distinct primes which occur as substrings of the digits of n.
  0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 2, 0, 1, 0, 2, 0, 1, 1, 1, 1, 3,
  1, 2, 1, 2, 1, 2, 1, 2, 2, 1, 1, 2, 1, 3, 1, 1, 0, 1, 1, 2, 0, 1, 0,
  2, 0, 0, 1, 1, 2, 3, 1, 1, 1, 2, 1, 2, 0, 1, 1, 1, 0, 1, 0, 2, 0, 0,
  1, 2, 2, 3, 1, 2, 1, 1, 1, 2, 0, 0, 1, 2, 0, 1, 0, 1, 0, 1, 0, 0, 1,
  1, 0, 1, 0, 2, 0, 0, 0, 1, 1, 2, 0, 1, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (divide str binary)
  (let (out "")
    (for (i 0 (- (length binary) 1))
      (if (= (binary i) "1")
          ; taglio
          (extend out (string (str i) { }))
          ; nessun taglio
          (extend out (str i))))
    ; inserisce caratteri finali
    (extend out (str -1))
    out))

(define (find-primi str)
  (cond ((= (length str) 1)
          (if (prime? (int str 0 10))
              (list str)
              '()))
        (true
          (local (out len max-tagli taglio fmt numeri)
            (setq out '())
            (setq len (length str))
            ; numero massimo di tagli
            (setq max-tagli (- len 1))
            ; formattazione con 0 davanti
            (setq fmt (string "%0" max-tagli "s"))
            (for (i 0 (- (pow 2 max-tagli) 1))
              ; taglio corrente
              (setq taglio (format fmt (bits i)))
              ; taglia la stringa con taglio corrente e
              ; forma una lista di numeri (stringhe)
              (setq numeri (parse (divide str taglio) " "))
              ;(println numeri)
              ; cerca i numeri primi nella lista di numeri
              (dolist (num numeri)
                (if (prime? (int num 0 10)) (push (int num 0 10) out -1)))
            )
            (unique (sort out))))))

Proviamo:

(find-primi "12234")
;-> (2 3 23 223 1223)
(find-primi "123456789")
;-> (2 3 5 7 23 67 89 4567 23456789)
(time (println (find-primi "123456789123456789")))
;-> (2 3 5 7 23 67 89 4567 67891 89123 4567891 23456789
;->  56789123 1234567891 4567891234567 23456789123456789)
;-> 2921.919

Calcoliamo la sequenza:

(map (fn(x) (length (find-primi x))) (map string (sequence 1 105)))
;-> (0 1 1 0 1 0 1 0 0 0 1 1 2 0 1 0 2 0 1 1 1 1 3
;->  1 2 1 2 1 2 1 2 2 1 1 2 1 3 1 1 0 1 1 2 0 1 0
;->  2 0 0 1 1 2 3 1 1 1 2 1 2 0 1 1 1 0 1 0 2 0 0
;->  1 2 2 3 1 2 1 1 1 2 0 0 1 2 0 1 0 1 0 1 0 0 1
;->  1 0 1 0 2 0 0 0 1 1 2 0 1)


---------------------------------------------------------
Numeri con somma delle cifre uguale al numero di divisori
---------------------------------------------------------

Determinare la sequenza dei numeri che hanno la somma delle cifre uguale al numero di divisori.

Sequenza OEIS A057531:
Numbers whose sum of digits and number of divisors are equal.
  1, 2, 11, 22, 36, 84, 101, 152, 156, 170, 202, 208, 225, 228, 288, 301,
  372, 396, 441, 444, 468, 516, 525, 530, 602, 684, 710, 732, 804, 828,
  882, 952, 972, 1003, 1016, 1034, 1070, 1072, 1106, 1111, 1164, 1236,
  1304, 1308, 1425, 1472, 1476, 1521, 1524, ...

(define (factor-group num)
"Factorize an integer number"
  (if (= num 1) '((1 1))
    (letn ( (fattori (factor num))
            (unici (unique fattori)) )
      (transpose (list unici (count unici fattori))))))

(define (divisors-count num)
"Count the divisors of an integer number"
  (if (= num 1)
      1
      (let (lst (factor-group num))
        (apply * (map (fn(x) (+ 1 (last x))) lst)))))

(define (digit-sum num)
"Calculate the sum of the digits of an integer"
  (let (out 0)
    (while (!= num 0)
      (setq out (+ out (% num 10)))
      (setq num (/ num 10))
    )
    out))

(define (check? num) (= (digit-sum num) (divisors-count num)))

Proviamo:

(check? 30)
;-> nil
(check? 22)
;-> true

(filter check? (sequence 1 1524))
;-> (1 2 11 22 36 84 101 152 156 170 202 208 225 228 288 301
;->  372 396 441 444 468 516 525 530 602 684 710 732 804 828
;->  882 952 972 1003 1016 1034 1070 1072 1106 1111 1164 1236
;->  1304 1308 1425 1472 1476 1521 1524)

(time (println (length (setq all (filter check? (sequence 1 1e6))))))
;-> 11405
;-> 3625.516


-----------------------------------------------------------------
Numero minimo di operazioni per rendere tutti i numeri uguali a 1
-----------------------------------------------------------------

Abbiamo una lista di N numeri interi positivi.
L'unica operazione possibile sui numeri della lista è la seguente:
selezionare un indice i tale che 0 <= i < (N - 1) e sostituisci nums(i) o nums(i+1) con il loro MCD (Massimo Comun Divisore).
Scrivere una funzione che restituisce il numero minimo di operazioni per rendere tutti gli elementi della lista uguali a 1.

Possiamo ridurre tutti gli elementi a 1 solo se il MCD di tutti gli elementi è uguale a 1.
Abbiamo due casi:
1) Se ci sono degli 1 nella lista, è possibile utilizzare ogni 1 per convertire i suoi vicini a 1 tramite operazioni MCD (poiché MCD(1, qualsiasi numero) = 1).
In questo caso il numero di operazioni minime necessarie vale:
  lunghezza-della-lista - numero-di-uno-nella-lista
2) Se inizialmente non ci sono 1, è necessario prima crearne almeno uno trovando la sottolista più corta il cui MCD sia uguale a 1, quindi 'distribuire' quell'1 a tutti gli altri elementi.
In questo caso il numero di operazioni minime dipende da quanti elementi devono essere modificati e dal numero minimo di passaggi necessari per creare il primo 1.

Esempio 1:
Input: nums = (2 8 3 4)
Output: 4
Operazioni:
- indice i = 2, sostituire (nums 2) con MCD(3,4) = 1, Risultato = [2,8,1,4].
- indice i = 1, sostituire (nums 1) con MCD(8,1) = 1, Risultato = [2,1,1,4].
- indice i = 0, sostituire (nums 0) con MCD(2,1) = 1, Risultato = [1,1,1,4].
- indice i = 2, sostituire (nums 3) con MCD(1,4) = 1, Risultato = [1,1,1,1].

Esempio 2:
Input: nums = (2 4 8 12)
Output: nil
Operazioni: nessuna operazione può creare un 1.

(define (min-ops nums)
  (local (len ones min-len cur-gcd stop)
    ; numero di elementi (N)
    (setq len (length nums))
    ; conta il numero di 1 nella lista
    (setq ones (first (count '(1) nums)))
    (cond 
      ; Se ci sono x valori a 1 nella lista ,
      ; allora abbiamo bisogno (N - x) operazioni per convertire
      ; tutti gli elementi diversi da 1 in 1
      ((> ones 1) (- len ones))
      (true
        ; Cerca la sottolista di lunghezza minima il cui MCD è uguale a 1
        (setq min-len (+ len 1))
        ;(println "min-len: " min-len)
        ; Prova tutte le possibili sottolista a partire dall'indice i
        (for (i 0 (- len 1))
          ;(println "i: " i)
          (setq cur-gcd 0)
          (setq stop nil)
          ; Estende la sottolista da i a j
          (for (j i (- len 1) 1 stop)
            ; Calcola il MCD di tutti gli elementi dall'indice i a j
            (setq cur-gcd (gcd cur-gcd (nums j)))
            ;(println "i: " i { } "j: " j)
            ;(println "nums(i): " (nums i) { } "numsj): " (nums j))
            ;(print "cur-gcd: " cur-gcd) (read-line)
            ; Se troviamo una sottolista con MCD = 1, allora...
            (when (= cur-gcd 1) 
              ; aggiorna la lunghezza minima
              (setq min-len (min min-len (+ j 1 (- i))))
              ;(println "min-len: " min-len) (read-line)
              ; Ferma il ciclo perchè non abbiamo bisogno di estendere
              ; la lista da questo punto
              (setq stop true))))
        (if (> min-len len)
            ; Se non esiste alcun sottoarray con MCD = 1, restituisce nil
            nil
            ;else
            ; Totale operazioni = 
            ;    (min-len - 1) [per creare il primo 1] +
            ;    (len - 1)     [per propagare 1 a tutti gli altri elementi]
            (+ len min-len (- 2)))))))

Proviamo:

(min-ops '(2 8 4 3))
;-> 4

(min-ops '(2 4 8 12))
;-> nil

(min-ops '(1 2 3 4 5 6 7 8 9))
;-> 8

(min-ops '(2 2 3 4 5 6 7 8 9))
;-> 9

Togliere il commento alle linee "print" per vedere come funziona l'algoritmo.

============================================================================

