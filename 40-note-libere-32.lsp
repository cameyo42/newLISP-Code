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


--------------------
Chess Bar Evaluation
--------------------

In una partita a scacchi il computer valuta la posizione dopo ogni mossa del Bianco e del Nero.
La valutazione è un numero che può essere:
1) maggiore di 0 (posizione favorevole al Bianco)
2) uguale a 0 (posizione pari)
3) minore di 0 (posizione favorevole al Nero)
Graficamente questa valutazione viene rappresentata con un numero e una barra:

Esempio:
valutazione = +1.5
      -5 -4 -3 -2 -1  0 +1 +2 +3 +4 +5
       .  .  .  .  .  .  .  .  .  .  .
(1.5)  ====================...........

Nota: in modo indicativo quando la valutazione vale intorno a 1, un giocatore è in vantaggio di circa un pedone (materialmente o posizionalmente)

Esempio di partita (con valutazione per ogni mossa):
 1. e4   (+0.28)      c5   (+0.40)
 2. d4   (+0.16)      cxd4 (+0.16)
 3. Nf3  (+0.16)      Cc6  (+0.36)
 4. c3   (-0.12)      dxc3 (-0.12)
 5. Nxc3 (-0.16)      e6   (-0.08)
 6. Bc4  (-0.20)      Bb4  (-0.08)
 7. O-O  (-0.12)      a6   (+0.12)
 8. a4   (-0.36)      Bxc3 (+0.80)
 9. bxc3 (+0.80)      h6   (+1.36)
10. Ba3  (+1.00)      Nge7 (+1.24)
11. Nd4  (+1.24)      O-O  (+1.32)
12. Nxc6 (+1.32)      bxc6 (+1.48)
13. Qd6  (+0.68)      Re8  (+0.68)
14. f4   (+0.24)      Bb7  (+2.24)
15. f5   (+0.77)      Nc8  (+3.28)
16. Qg3  (+3.28)      d5   (+8.60)
17. f6   (+8.60)      g6   (+9.32)
18. Qh4  (+9.32)      Kh7  (+99)
19. Bc1  (+99)        Abbandona

Nota: negli scacchi una mossa significa una semi-mossa del Bianco e una semi-mossa del Nero.

La lista delle valutazioni vale:

(setq values '(0.28 0.40 0.16 0.16 0.16 0.36 -0.12 -0.12 -0.16 -0.08
               -0.20 -0.08 -0.12 0.12 -0.36 0.80 0.80 1.36 1.00 1.24
               1.24 1.32 1.32 1.48 0.68 0.68 0.24 2.24 0.77 3.28
               3.28 8.60 8.60 9.32 9.32 99 99))

Vogliamo scrivere una funzione che analizza la lista delle valutazioni e determina chi, dove e quanto hanno sbagliato i giocatori durante la partita.
La funzione prende un parametro 'threshold' che specifica il minimo scarto tra due valutazioni consecutive.

(define (analyze threshold values)
  (setq game (map (fn(x) (list (+ $idx 1) x)) (map sub (rest values) (chop values))))
  (let ( (changes '()) (prev 0.0) )
    ; ciclo per ogni valutazione...
    (dolist (el values)
      ; variazione della valutazione tra
      ; la semi-mossa corrente e la semi-mossa precedente
      (setq delta (abs (sub el prev)))
      ; numero mossa corrente
      (setq mossa (/ (+ $idx 2) 2))
      (when (> delta threshold)
          ;(print prev { } el) (read-line)
          ; Inserisce una lista:
          ; (giocatore valore-precedente valore-corrente delta-valore)
          ; Indice pari: mossa del Bianco
          ; Indice dispari: mossa del Nero
          (if (even? $idx)
              (push (list 'B mossa prev el (format "%2.2f" (sub el prev)))
                    changes -1)
              (push (list 'N mossa prev el (format "%2.2f" (sub el prev)))
                    changes -1)))
      ; aggiorna valutazione precedente
      (setq prev el))
    changes))

Proviamo:

Errori di circa un pedone:
(analyze 1 values)
;-> ((N 8 -0.36 0.8 "1.16")
;->  (N 14 0.24 2.24 "2.00")
;->  (B 15 2.24 0.77 "-1.47")
;->  (N 15 0.77 3.28 "2.51")
;->  (N 16 3.28 8.6 "5.32")
;->  (N 18 9.32 99 "89.68"))

Il valore (N 8 -0.36 0.8 "1.16") significa:
Il Nero, con la mossa 8, è passato da -036 a 0.8, perdendo 1.16 punti.

Errori grossolani:
(analyze 2 values)
;-> ((N 15 0.77 3.28 "2.51")
;->  (N 16 3.28 8.6 "5.32")
;->  (N 18 9.32 99 "89.68"))

Errori impercettibili:
(analyze 0.25 values)
;-> ((B 1 0 0.28 "0.28") 
;->  (B 4 0.36 -0.12 "-0.48") 
;->  (B 8 0.12 -0.36 "-0.48")
;->  (N 8 -0.36 0.8 "1.16")
;->  (N 9 0.8 1.36 "0.56")
;->  (B 10 1.36 1 "-0.36")
;->  (B 13 1.48 0.68 "-0.80")
;->  (B 14 0.68 0.24 "-0.44")
;->  (N 14 0.24 2.24 "2.00")
;->  (B 15 2.24 0.77 "-1.47")
;->  (N 15 0.77 3.28 "2.51")
;->  (N 16 3.28 8.6 "5.32")
;->  (N 17 8.6 9.32 "0.72")
;->  (N 18 9.32 99 "89.68"))

In teoria, se la differenza assoluta tra due numeri consecutivi è maggiore del valore assoluto del doppio del primo numero, allora mossa con 3 punti esclamativi !!! (non l'ha vista neanche il computer).
In pratica, il computer valuta l'ultima posizione con una mossa in più della precedente, quindi potrebbe "vedere" un vantaggio maggiore.


---------------------------------------
La funzione "parse" (Tokenize a string)
---------------------------------------

"Tokenize a string" significa suddividere una stringa in parti più piccole, chiamate token.
Un token è un'unità significativa di testo (una parola, un numero, un simbolo, ecc.) oppure una sequenza definita da una regola (come separatori o spazi).

Esempio:
  stringa = "Questo è un esempio di tokenize."
  tokenizzazione per spazi " " = "Questo" "è" "un" "esempio" "di" "tokenize."

In programmazione si usa per dividere una stringa in parole o simboli su cui lavorare (es. parsing di comandi, lettura CSV).
In linguistica computazionale/NLP è il primo passo per elaborare il linguaggio naturale (es. trasformare una frase in singole parole o sottoparole).
Nei compilatori la tokenizzazione (o lexing) trasforma il codice sorgente in simboli (parole chiave, numeri, operatori, ecc.).

Per fare questo newLISP ha la funzione "parse".

*******************
>>> funzione PARSE
*******************
sintassi: (parse str-data [str-break [regex-option]])

Suddivide la stringa risultante dalla valutazione di "str-data" in token di stringa, che vengono poi restituiti in una lista.
Se non viene specificato "str-break", l'analisi esegue la tokenizzazione in base alle regole di analisi interne di newLISP.
È possibile specificare una stringa in "str-break" per tokenizzare solo al momento dell'occorrenza di una stringa.
Se viene specificato un numero o una stringa "regex-option", è possibile utilizzare un pattern di espressione regolare in "str-break".
Se "str-break" non è specificato, la dimensione massima del token è 2048 per le stringhe tra virgolette e 256 per gli identificatori.
In questo caso, newLISP utilizza lo stesso tokenizzatore più veloce utilizzato per l'analisi del codice sorgente newLISP.
Se viene specificato "str-break", non vi è alcuna limitazione alla lunghezza dei token.
Viene utilizzato un algoritmo diverso che suddivide la stringa sorgente "str-data" in corrispondenza della stringa in "str-break".

; no break string specified
(parse "hello how are you")
;-> ("hello" "how" "are" "you")

; strings break after spaces, parentheses, commas, colons and numbers.
; Spaces and the colon are swollowed
(parse "weight is 10lbs")
;-> ("weight" "is" "10" "lbs")
(parse "one:two:three" ":")
;-> ("one" "two" "three")

;; specifying a break string
(parse "one--two--three" "--")
;-> ("one" "two" "three")

; a regex option causes regex parsing
(parse "one-two--three---four" "-+" 0)
;-> ("one" "two" "three" "four")

(parse "hello regular   expression 1, 2, 3" {,\s*|\s+} 0)
;-> ("hello" "regular" "expression" "1" "2" "3")

Gli ultimi due esempi mostrano un'espressione regolare come stringa di interruzione con l'opzione predefinita 0 (zero).
Invece di { e } (parentesi graffe sinistra e destra), è possibile utilizzare le virgolette doppie per limitare il pattern.
In questo caso, è necessario utilizzare doppie barre rovesciate all'interno del pattern.
L'ultimo pattern potrebbe essere utilizzato per l'analisi di file CSV (Comma Separated Values).
Per i numeri delle opzioni delle espressioni regolari, vedere "regex".

parse restituirà i campi vuoti attorno ai separatori come stringhe vuote:
; empty fields around separators returned as empty strings
(parse "1,2,3," ",")
;-> ("1" "2" "3" "")
(parse "1,,,4" ",")
;-> ("1" "" "" "4")
(parse "," ",")
;-> ("" "")
Questo comportamento è necessario quando si analizzano record con campi vuoti.

L'analisi di una stringa vuota produrrà sempre un elenco vuoto:
(parse "")
;-> ()
(parse "" " ")
;-> ()

Utilizzare la funzione "regex" per suddividere le stringhe e le funzioni "directory", "find", "find-all", "regex", "replace" e "search" per utilizzare le espressioni regolari.
---------------------

Supponiamo di avere la stringa:
(setq str "3,  5 ;;; (define) 7 8, k 9 text")
;-> "3,  5 ;;; (define) 7 8, k 9 text"
e di voler tokenizzarla/suddividerla in base ai caratteri ",", ";", "(", ")" e " ".
Possiamo usare una espressione regolare: "[,;() ]+"
(parse str "[,;() ]+" 0)
;-> ("3" "5" "define" "7" "8" "k" "9" "text")

In pratica con "parse" possiamo suddividere una stringa in (quasi) qualunque modo.


-------------------------------------------
La funzione "history" (Nome della funzione)
-------------------------------------------

Vediamo la definizione della funzione "history" dal manuale:

********************
>>>funzione HISTORY
********************
sintassi: (history [bool-params])

history returns a list of the call history of the enclosing function.
Without the optional bool-params, a list of function symbols is returned.
The first symbol is the name of the enclosing function.
When the optional bool-params evaluates to true, the call arguments are included with the symbol.

(define (foo x y)
    (bar (+ x 1) (* y 2)))

(define (bar a b)
    (history))

; history returns names of calling functions
(foo 1 2)
;-> (bar foo)

; the additional 'true' forces inclusion of callpatterns
(define (bar a b)
    (history true))

(foo 1 2)
;-> ((bar (+ x 1) (* y 2)) (foo 1 2))
---------------------

Con "history" possiamo determinare il nome di una funzione all'interno di se stessa:

(define (get-my-name) (history))

(define (somma a b)
  (let (name (get-my-name))
    (println "I'm " (name 1))
    (+ a b)))

(somma 1 2)
;-> I'm somma
;-> 3

Possiamo anche usare "history" all'interno della funzione stessa:

(define (somma a b)
  (println "Eseguo: " (first (history true)))
  (+ a b))

(somma 1 2)
;-> Eseguo: (somma 1 2)
;-> 3

(somma (+ 1 1) (+ 2 2))
;-> Eseguo: (somma (+ 1 1) (+ 2 2))
;-> 6

Questa funzione può essere utile in fase di debug dei programmi (es. stampare la lista delle funzioni chiamate).

Vedi anche "Nome della funzione" su "Note libere 8".


--------------------
Automa in espansione
--------------------

Abbiamo una griglia bidimensionale infinita di celle vuote.
Un automa viene inserito in una cella.
Ad ogni generazione l'automa si riproduce in tutte le celle adiacenti (alto, basso, destra, sinistra).
Vediamo le prime 4 generazioni:

                                                     ██
                                  ██               ██████  
                ██              ██████           ██████████
  ██          ██████          ██████████       ██████████████
                ██              ██████           ██████████
                                  ██               ██████
                                                     ██  
  gen 1      gen 2            gen 3            gen 4
  celle=1    celle=5          celle=13         celle=25

Dopo N generazioni, quante celle sono piene/occupate?

Calcolo di tutte le celle (per righe)
-------------------------------------
'row-max' è il numero di celle della riga più lunga (ed è anche il numero di righe di ogni configurazione).

(define (celle1 n)
  (local (row-max celle)
    (setq row-max (- (* 2L n) 1))
    (setq celle (- row-max))
    (for (i 1 row-max 2) (++ celle (* i 2)))
    celle))

(map celle1 (sequence 1 10))
(1 5 13 25 41 61 85 113 145 181)
;-> (1L 5L 13L 25L 41L 61L 85L 113L 145L 181L)

Formula matematica
------------------
Se racchiudiamo ogni generazione nel suo quadrato minimo di contenimento possiamo osservare che il numero di celle vuote è uguale al numero di celle piene meno 1.
Quindi il numero di celle occupate vale:

  (row-max * row-max) - (row-max * row-max)/2

(define (celle2 n)
  (let (r (- (* 2L n) 1))
    (- (* r r) (/ (* r r) 2))))

(celle2 3)

(map celle2 (sequence 1 10))
;-> (1L 5L 13L 25L 41L 61L 85L 113L 145L 181L)

Formula matematica
------------------
a(n) = n^2 + (n - 1)^2

(define (celle3 n) (+ (* n n) (* (- n 1) (- n 1))))

(map celle3 (sequence 1 10))
;-> (1 5 13 25 41 61 85 113 145 181)

Formula matematica
------------------
a(n) = 2*(n-1)*n+1

(define (celle4 n) (+ (* 2 n (- n 1)) 1))

(map celle4 (sequence 1 10))
;-> (1 5 13 25 41 61 85 113 145 181)

Sequenza OEIS A001844:
Centered square numbers: a(n) = 2*n*(n+1)+1. Sums of two consecutive squares.
Also, consider all Pythagorean triples (X,Y,Z=Y+1) ordered by increasing Z:
then sequence gives Z values.
  1, 5, 13, 25, 41, 61, 85, 113, 145, 181, 221, 265, 313, 365, 421, 481,
  545, 613, 685, 761, 841, 925, 1013, 1105, 1201, 1301, 1405, 1513, 1625,
  1741, 1861, 1985, 2113, 2245, 2381, 2521, 2665, 2813, 2965, 3121, 3281,
  3445, 3613, 3785, 3961, 4141, 4325, 4513, ...

(map celle4 (sequence 1 50))
;-> (1 5 13 25 41 61 85 113 145 181 221 265 313 365 421 481
;->  545 613 685 761 841 925 1013 1105 1201 1301 1405 1513 1625
;->  1741 1861 1985 2113 2245 2381 2521 2665 2813 2965 3121 3281
;->  3445 3613 3785 3961 4141 4325 4513 4705 4901)

File per Golly (Life Simulation):
A) Regola1.table --> regola dell'esempio
B) Regola2.table --> regola dell'esempio + celle morte con 4 vicini

I file "Regola1.table" e "Regola2.table" si trovano nella cartella "data".

Copiare il seguente file "Regola1.table" sulla cartella \Rules di Golly.
----------------------------
#
# Regola 1
#
n_states:2
neighborhood:vonNeumann
symmetries:none

# Stati:
# 0 = morto
# 1 = vivo

# variabili per i 4 vicini
var n={0,1}
var e={0,1}
var s={0,1}
var w={0,1}

# ordine: C,N,E,S,W,C'

# ---- celle morte ----
0,0,0,0,0,0     # morta senza vicini resta morta
0,n,e,s,w,1     # morta con almeno 1 vicino -> viva

# ---- celle vive ----
1,n,e,s,w,1     # viva resta viva sempre
----------------------------

Copiare il seguente file "Regola2.table" sulla cartella \Rules di Golly.
----------------------------
#
# Regola 2
#
n_states:2
neighborhood:vonNeumann
symmetries:none

# variabili per i 4 vicini (ognuno può valere 0 o 1)
var n={0,1}
var e={0,1}
var s={0,1}
var w={0,1}

# ordine: C,N,E,S,W,C'  (C' è il nuovo stato)
# 1) cella morta con 0 vicini resta morta
0,0,0,0,0,0

# 2) cella morta con almeno 1 vicino -> nasce (la regola generale)
0,n,e,s,w,1

# 3) cella viva con 4 vicini -> muore
1,1,1,1,1,0
----------------------------


--------------------------------------------------------------
Sequenze della somma/differenza dei numeri con il loro inverso
--------------------------------------------------------------

Determinare le sequenze dei numeri n per cui risulta:

1) n + inversione delle cifre di n
2) n - inversione delle cifre di n

Sequenza OEIS A056964:
a(n) = n + reversal of digits of n.
  0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 11, 22, 33, 44, 55, 66, 77, 88, 99,
  110, 22, 33, 44, 55, 66, 77, 88, 99, 110, 121, 33, 44, 55, 66, 77, 88,
  99, 110, 121, 132, 44, 55, 66, 77, 88, 99, 110, 121, 132, 143, 55, 66,
  77, 88, 99, 110, 121, 132, 143, 154, 66, 77, 88, 99, 110, ...

Sequenza OEIS A056965:
a(n) = n - (reversal of digits of n).
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, -9, -18, -27, -36, -45, -54, -63,
  -72, 18, 9, 0, -9, -18, -27, -36, -45, -54, -63, 27, 18, 9, 0, -9, -18,
  -27, -36, -45, -54, 36, 27, 18, 9, 0, -9, -18, -27, -36, -45, 45, 36,
  27, 18, 9, 0, -9, -18, -27, -36, 54, 45, 36, 27, 18, 9, 0, -9, -18, -27,
  63, 54, 45, 36, 27, 18, 9, 0, -9, -18, 72, 63, 54, ...

(define (seq op num)
  (op num (int (reverse (string num)) 0 10)))

(map (curry seq +) (sequence 0 65))
;-> (0 2 4 6 8 10 12 14 16 18 11 22 33 44 55 66 77 88 99
;->  110 22 33 44 55 66 77 88 99 110 121 33 44 55 66 77 88
;->  99 110 121 132 44 55 66 77 88 99 110 121 132 143 55 66
;->  77 88 99 110 121 132 143 154 66 77 88 99 110 121)

(map (curry seq -) (sequence 0 65))
;-> (0 0 0 0 0 0 0 0 0 0 9 0 -9 -18 -27 -36 -45 -54 -63
;->  -72 18 9 0 -9 -18 -27 -36 -45 -54 -63 27 18 9 0 -9 -18
;->  -27 -36 -45 -54 36 27 18 9 0 -9 -18 -27 -36 -45 45 36
;->  27 18 9 0 -9 -18 -27 -36 54 45 36 27 18 9)


-----------------------
Scontornare una matrice
-----------------------

Data una matrice, eliminare le righe e le colonne che contengono tutti zero.

Esempio:

  0 0 0 0 0
  0 1 0 2 0       1 0 2 0
  0 0 4 5 0  -->  0 4 5 0
  0 0 0 3 0       0 0 3 0
  0 0 0 0 1       0 0 0 1

(define (cutout matrix)
  (let ( (rows '()) (out '()) )
    (dolist (row matrix)
      (if-not (for-all zero? row)
          (push row rows -1)))
    (setq rows (transpose rows))
    (dolist (row rows)
      (if-not (for-all zero? row)
          (push row out -1)))
    (transpose out)))

Proviamo:

(setq m '((0 0 0 0 0) (0 1 0 2 0) (0 0 4 5 0) (0 0 0 3 0) (0 0 0 0 1)))

(cutout m)
;-> ((1 0 2 0) (0 4 5 0) (0 0 3 0) (0 0 0 1))


-------------
Algoritmo 196
-------------

Algoritmo 196
-------------
Iniziare con un numero intero positivo n.
Se palindromo, stop.
In caso contrario, somma n con le cifre invertite di n (es 123 + 321 = 444).
a(n) restituisce il palindromo in corrispondenza del quale questo processo si ferma, oppure -1 se non viene mai raggiunto alcun palindromo.

Sequenza OEIS A033865:
Start with n. If palindrome, stop. Otherwise add to itself with digits reversed.
a(n) gives palindrome at which it stops, or -1 if no palindrome is ever reached.
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 11, 33, 44, 55, 66, 77, 88, 99, 121,
  22, 33, 22, 55, 66, 77, 88, 99, 121, 121, 33, 44, 55, 33, 77, 88, 99,
  121, 121, 363, 44, 55, 66, 77, 44, 99, 121, 121, 363, 484, 55, 66, 77,
  88, 99, 55, 121, 363, 484, 1111, 66, 77, 88, 99, 121, 121, 66, 484,
  1111, 4884, 77, 88, 99, 121, 121, 363, 484, 77, 4884, 44044, 88, ...

Funzione ricorsiva:

(define (find-pali num)
  (let (rev (int (reverse (string num)) 0 10))
    (cond ((= num rev) num)
          (true (find-pali (+ num rev))))))

Proviamo:

(map find-pali (sequence 0 80))
;-> (0 1 2 3 4 5 6 7 8 9 11 11 33 44 55 66 77 88 99 121
;->  22 33 22 55 66 77 88 99 121 121 33 44 55 33 77 88 99
;->  121 121 363 44 55 66 77 44 99 121 121 363 484 55 66 77
;->  88 99 55 121 363 484 1111 66 77 88 99 121 121 66 484
;->  1111 4884 77 88 99 121 121 363 484 77 4884 44044 88)

Con numeri grandi la ricorsione genera stack overflow:

(map find-pali (sequence 0 200))
;-> ERR: call or result stack overflow in function reverse : string
;-> called from user function (find-pali (+ num rev))

Scriviamo una funzione iterativa che lavora con i big integer:

(define (find-pali2 limite max-recur)
  (local (out num stop cur rev)
    (setq out '(0L))
    (setq num 1L)
    ; ciclo per calcolare la sequenza da 1 a limite...
    (while (< num limite)
      (setq stop nil)
      (setq cur num)
      ; ripete il processo per 'max-recur' iterazioni...
      (for (r 1 max-recur 1 stop)
        ; inverte il numero corrente
        (setq rev (reverse (string cur)))
        ; sposta la "L" dei big-integer dall'inizio alla fine
        (when (= (rev 0) "L")
          (pop rev) (push "L" rev -1))
        ; rimuove eventuali zeri iniziali
        (while (= (rev 0) "0") (pop rev))
        ; controllo condizione palindromi
        (cond ((= rev (string cur))
                (setq stop true)
                (push cur out -1))
              (true (setq cur (+ cur (eval-string rev)))))
        ;(print cur { } rev) (read-line)
      )
      ; stampa i numeri che non hanno palindromi
      ; dopo 'max-recur' iterazioni del processo
      (if (not stop) (println (list num max-recur)))
      ; prossimo numero
      (setq num (+ num 1L)))
    out))

Proviamo:

(find-pali2 200 30)
;-> (196L 30)
;-> (0L 1L 2L 3L 4L 5L 6L 7L 8L 9L 11L 11L 33
;-> ...
;-> 233332L 2992L 9339L 881188L 79497L 3113L)
Con 30 iterazioni il numero 196 non ha generato alcun palindromo.

(find-pali2 200 1000)
;-> (196L 30)
;-> (0L 1L 2L 3L 4L 5L 6L 7L 8L 9L 11L 11L 33
;-> ...
;-> 233332L 2992L 9339L 881188L 79497L 3113L)
Con 1000 iterazioni il numero 196 non ha generato alcun palindromo.

https://mathworld.wolfram.com/196-Algorithm.html
Partendo da N=196, il 1 maggio 2006, dopo 724756966 iterazioni ancora nessun numero palindromo: il numero raggiunto ha più di 300 milioni di cifre (VanLandingham).

Vedi anche "Numeri palindromi e numeri di Lychrel" su "Note libere 5".


-------------------------------------------------------
Numeri come differenza di un numero e del suo contrario
-------------------------------------------------------

Calcolare la sequenza dei numeri interi positivi che possono essere espressi come differenza assoluta di due interi positivi k e il contrario di k.

La seguente funzione calcola tutte le possibili coppie (k, inverse(k)) che soddisfano:

  abs(k - inverse(k)) = num

(define (mirror num)
  (let ( (out '()) (kappa nil) )
    (for (k 0 num)
      ; calcola il contrario (inverso) del numero
      (setq kappa (int (reverse (string k)) 0 10))
      (if (= (abs (- k kappa)) num)
          (if (> k kappa)
            (push (list k kappa) out -1)
            (push (list kappa k) out -1)
          )
      )
    )
    (unique out)))

Proviamo:

Prima vediamo quali numeri sono nella sequenza fino a 10000:

(setq seq (filter mirror (sequence 1 1000)))
;-> (18 27 36 45 54 63 72 198 297 396 495 594 693 792)

Nota: questa sequenza non esiste su OEIS (7 settembre 2025).

Adesso vediamo quante coppie hanno ciascun numero della sequenza calcolata:

(setq coppie (map (fn(x y) (list x (mirror y))) seq seq))

(slice coppie 0 5)
;-> ((18 ((31 13)))
;->  (27 ((41 14) (52 25)))
;->  (36 ((51 15) (62 26)))
;->  (45 ((61 16) (72 27) (83 38)))
;->  (54 ((71 17) (82 28) (93 39))))

(slice coppie -2 2)
;-> ((693 ((801 108) (811 118) (821 128) (831 138) (841 148) (851 158)
;->        (861 168) (871 178) (881 188) (891 198) (902 209) (912 219)
;->        (922 229) (932 239) (942 249) (952 259) (962 269) (972 279)
;->        (982 289) (992 299)))
;->  (792 ((901 109) (911 119) (921 129) (931 139) (941 149) (951 159)
;->        (961 169) (971 179) (981 189) (991 199))))

Vedi anche "Numeri come somma di un numero e del suo contrario" su "Note libere 22".


-----------------------------------------------------
Invertire le cifre di un numero integer o big-integer
-----------------------------------------------------

Scrivere una funzione che inverte le cifre di un numero intero (anche big-integer).

(define (reverse-digits-big num)
  (if (zero? num) 0
  ;else
    (begin
    ; converte 'num' in stringa e lo inverte
    (setq num (reverse (string num)))
    ; se 'num' è big-integer, allora sposta la "L" dall'inizio alla fine
    (when (= (num 0) "L") (pop num) (push "L" num -1))
    ; rimuove eventuali zeri iniziali
    (while (= (num 0) "0") (pop num))
    ; converte la stringa 'num' in numero intero o big-integer
    (eval-string num))))

Proviamo:

(reverse-digits-big 0)
;-> 0
(reverse-digits-big 2)
;-> 2
(reverse-digits-big 2L)
;-> 2L
(reverse-digits-big 100000000000000000000000000000)
;-> 1L
(reverse-digits-big 100000000000000000000000000000L)
;-> 1L
(reverse-digits-big 1234567890)
;-> 987654321
(reverse-digits-big 1234567890L)
;-> 987654321L


-------------------------------------------
Coppie di interi divisibili per un numero k
-------------------------------------------

Data una lista di numeri interi di lunghezza n e un intero k, restituire il numero di coppie (i, j) tali che:

  1) 0 <= i < j <= n - 1 and
  2) nums[i] * nums[j] is divisible by k.

(define (pairs-ij lst)
"Generate all pairs of elements of a list with (index i < index j)"
  (let ( (out '()) (len (length lst)) )
    (for (i 0 (- len 2))
      (for (j (+ i 1) (- len 1))
        (push (list (lst i) (lst j)) out -1)))))

(define (all-pairs1 lst k)
  (let ( (out '()) (len (length lst)) )
    (for (i 0 (- len 2))
      (for (j (+ i 1) (- len 1))
        (if (zero? (% (* (lst i) (lst j)) k))
            (push (list i j) out -1))))))

(setq lst '(1 2 3 4 5))
(all-pairs1 lst 2)
;-> ((0 1) (0 3) (1 2) (1 3) (1 4) (2 3) (3 4))

Elementi di ogni coppia:
(map (fn(x) (list (lst (x 0)) (lst (x 1)))) (all-pairs1 lst 2))
;-> ((1 2) (1 4) (2 3) (2 4) (2 5) (3 4) (4 5))

Test di velocità:

(silent (setq test (rand (pow 10 2) (pow 10 2))))
(time (println (length (all-pairs1 test 2))))
;-> 3465
;-> 0.996

(silent (setq test (rand (pow 10 3) (pow 10 3))))
(time (println (length (all-pairs1 test 2))))
;-> 372240
;-> 599.557

(silent (setq test (rand (pow 10 4) (pow 10 4))))
(time (println (length (all-pairs1 test 2))))
;-> 37412364
;-> 661047.466 ; 661 secondi (11 minuti)

Proviamo ad usare un vettore al posto di una lista:

(define (all-pairs2 lst k)
  (letn ( (out '()) (len (length lst)) (vec (array len lst)) )
    (for (i 0 (- len 2))
      (for (j (+ i 1) (- len 1))
        (if (zero? (% (* (vec i) (vec j)) k))
            (push (list i j) out -1))))))

(setq lst '(1 2 3 4 5))
(all-pairs2 lst 2)
;-> ((0 1) (0 3) (1 2) (1 3) (1 4) (2 3) (3 4))

Elementi di ogni coppia:
(map (fn(x) (list (lst (x 0)) (lst (x 1)))) (all-pairs2 lst 2))
;-> ((1 2) (1 4) (2 3) (2 4) (2 5) (3 4) (4 5))

Test di velocità:

(silent (setq test (rand (pow 10 2) (pow 10 2))))
(time (println (length (all-pairs2 test 2))))
;-> 3572
;-> 0

(silent (setq test (rand (pow 10 3) (pow 10 3))))
(time (println (length (all-pairs2 test 2))))
;-> 372240
;-> 94.604

(silent (setq test (rand (pow 10 4) (pow 10 4))))
(time (println (length (all-pairs2 test 2))))
;-> 37938495
;-> 8531.493

Con i vettori la funzione è più veloce, ma sempre O(n^2).

Poichè il problema richiede di trovare il numero delle coppie (e non tutti gli indici delle coppie), possiamo ottimizzare la funzione utilizzando il MCD (Massimo Comun Divisore).

Vediamo prima il codice e poi come lavora la funzione:

(define (count-pairs nums k show)
  (letn ((ans 0)
         (gcds '())) ; lista associativa per contare i gcd
    (for (i 0 (- (length nums) 1))
      (let (gcd-i (gcd (nums i) k))
      ; controllo con tutti i gcd già incontrati
      (dolist (pair gcds)
        (let ((gcd-j (first pair)) (conta (last pair)))
          (if (= 0 (% (* gcd-i gcd-j) k))
              (++ ans conta))))
      ; aggiorno contatore per gcd-i
      (if (lookup gcd-i gcds)
          (++ (lookup gcd-i gcds))
          (push (list gcd-i 1) gcds -1))
      ; stampa dopo aver processato nums(i)
      (if show (println "dopo " (nums i) " -> " gcds))))
    (if show (println "finale gcds: " gcds))
    ans))

Proviamo:

(count-pairs '(1 2 3 4 5) 2)
;-> 7

(time (println (count-pairs test 2)))
;-> 110.264

1. Funzione
-----------
La funzione è:
(define (count-pairs nums k) ...)
Riceve:
  nums = lista di numeri,
  k = divisore richiesto.
Restituisce:
  ans = numero di coppie (i,j) tali che (nums i) * (nums j) è divisibile per k.

2. Variabili principali
-----------------------
(letn ((ans 0)
       (gcds '()))
  ans = contatore delle coppie trovate, parte da 0.
  gcds = lista associativa (tipo dizionario), che memorizza quante volte è apparso ogni gcd(nums(i), k).
  La chiave è il valore del gcd, il valore è il numero di occorrenze.
Esempio:
  ((2 3) (3 1)) significa che abbiamo visto
  gcd=2 tre volte,
  gcd=3 una volta.

3. Ciclo principale
-------------------
(for (i 0 (- (length nums) 1)) ...)
Scorriamo tutta la lista nums.
Per ogni numero:
a. Calcoliamo il suo gcd con k:
   (set 'gcd-i (gcd (nums i) k))
   Questo valore rappresenta:
   "la parte di nums(i) che può contribuire a rendere il prodotto divisibile da k".
b. Confrontiamo gcd-i con tutti i gcd già visti:
   (dolist (pair gcds)
     (let ((gcd-j (first pair)) (conta (last pair)))
       (if (= 0 (% (* gcd-i gcd-j) k))
           (++ ans conta))))
   gcd-j = un gcd precedente,
   conta = quante volte è comparso.
   Se gcd-i * gcd-j è multiplo di k, significa che il nuovo numero può formare una "coppia valida" con ciascuno dei numeri che avevano gcd-j.
   --> Incrementiamo ans di conta.
c. Aggiorniamo il dizionario gcds:
   (if (lookup gcd-i gcds)
       (++ (lookup gcd-i gcds))
       (push (list gcd-i 1) gcds -1))
   Se gcd-i è già presente, aumentiamo il suo conteggio,
   Altrimenti lo aggiungiamo con valore 1.

4. Stampa passo passo
---------------------
Dopo ogni numero stampiamo lo stato di gcds:
(println "dopo " (nums i) " -> " gcds)
Alla fine stampiamo il risultato finale:
(println "finale gcds: " gcds)
ans

5. Esempio
----------
(count-pairs '(2 3 4 9 6) 6)

Esecuzione:
1. num=2, gcd=2 --> gcds=((2 1))
2. num=3, gcd=3 --> prodotto con gcd=2: 2*3=6, divisibile --> 1 coppia trovata
   gcds=((2 1)(3 1))
3. num=4, gcd=2 --> prodotto con (2,1) e (3,1):
   - 2*2=4 non divisibile,
   - 2*3=6 divisibile --> 1 coppia.
     gcds=((2 2)(3 1))
4. num=9, gcd=3 --> prodotto con (2,2) e (3,1):
   - 3*2=6 divisibile --> 2 coppie,
   - 3*3=9 non divisibile.
     gcds=((2 2)(3 2))
5. num=6, gcd=6 --> prodotto con (2,2), (3,2):
   - 6*2=12 divisibile --> 2 coppie,
   - 6*3=18 divisibile --> 2 coppie.
     Totale +4.
     gcds=((2 2)(3 2)(6 1))
Risultato finale:
finale gcds: ((2 2) (3 2) (6 1))
8

Test di velocità
(silent (setq test (rand (pow 10 2) (pow 10 2))))
(time (println (count-pairs test 2)))
;-> 3915
;-> 0

(silent (setq test (rand (pow 10 3) (pow 10 3))))
(time (println (count-pairs test 2)))
;-> 375747
;-> 0

(silent (setq test (rand (pow 10 4) (pow 10 4))))
(time (println (count-pairs test 2)))
;-> 37236174
;-> 110.465

(silent (setq test (rand (pow 10 5) (pow 10 5))))
(time (println (count-pairs test 2)))
;-> 3756266999
;-> 9782.138

Nota:
Adesso la funzione restituisce solo il numero totale delle coppie. Per avere anche le coppie di indici, bisogna memorizzarle mentre si fa il ciclo.
L'idea è questa:
- oltre a 'ans' (che conta), si mantiene una lista 'coppie',
- ogni volta che si trova che '(gcd-i * gcd-j) % k = 0', invece di aumentare solo il contatore, si aggiungono a 'coppie' tutte le coppie di indici '(j, i)' corrispondenti,
- per poterlo fare serve che 'gcds' non memorizzi solo quante volte è comparso un gcd, ma anche gli indici dove è comparso.
In pratica:
- adesso 'gcds' è del tipo ((2 2) (3 2) (6 1)) (gcd --> conteggio),
- andrebbe trasformato in qualcosa tipo '((2 (0 2)) (3 (1 3)) (6 (4)))' (gcd --> lista di indici).
Così, quando si arriva a un nuovo 'i' con 'gcd-i', possiamo vedere in 'gcds' tutti gli indici 'j' precedenti con 'gcd-j' compatibile e salvare la coppia '(j, i)'.
Alla fine la funzione può restituire sia il numero delle coppie che la lista completa delle coppie di indici.

Comunque in questo modo (cioe' restituendo anche le coppie di indici) la funzione rallenta molto in caso di liste lunghe.

Il motivo è che l'attuale funzione è stata pensata per essere ottimizzata:
- Con il solo conteggio, non serve più controllare ogni coppia '(i,j)' esplicitamente --> la complessità si riduce notevolmente (dipende dal numero di divisori di 'k', non dalla lunghezza di 'nums').
- Se invece vuoi anche le **coppie di indici**, sei costretto a memorizzare tutti gli indici associati a ciascun 'gcd' e, al momento del confronto, a generare le coppie esplicite.
- Questo porta a un'esplosione combinatoria: per ogni coppia di gcd compatibili, si deve produrre un numero di tuple proporzionale al prodotto delle cardinalità delle loro liste di indici.
- la versione con le coppie esplicite diventa quasi equivalente a un algoritmo quadratico (O(n^2) nel caso peggiore).

Analisi comparativa
-------------------

A) Variante solo conteggio
  Per ogni numero 'num' calcola 'gcd(num, k)' → 'gcd-i'.
  Confronta 'gcd-i' con i gcd già visti ('gcds'), sommando direttamente il **conteggio** dei validi.
  Non deve mai scorrere le liste degli indici, solo aggiornare i contatori.
Complessità
Tempo:
  Ogni 'num' viene confrontato solo con i diversi 'gcd' già memorizzati.
  Il numero di possibili valori di 'gcd(num, k)' è **limitato dai divisori di 'k'** (di solito pochi rispetto a 'n').
  Complessità ≈ 'O(n * d)' dove 'd = numero di divisori di k'.
  Esempio: se 'k = 1000', 'd = 16'.
Spazio:
  Memorizza solo un contatore per ogni 'gcd'.
  Complessità 'O(d)'.

B) Variante con lista di coppie (i,j)
  'gcds' deve contenere **gli indici** dei numeri con quel 'gcd'.
  Esempio: '((2 (0 2)) (3 (1 3)) (6 (4)))'.
  Per ogni nuovo indice 'i', si confronta 'gcd-i' con ogni 'gcd-j' compatibile, e si devono costruire tutte le coppie '(j, i)'.
Complessità
Tempo:
Nel peggiore dei casi, può produrre fino a tutte le coppie possibili --> quadratico O(n^2).
Anche se il controllo resta limitato ai divisori di 'k', la creazione esplicita delle coppie costringe a scorrere liste potenzialmente grandi.
Spazio:
Non solo i 'd' contatori, ma anche tutte le liste di indici.
Nel peggiore dei casi, memorizza praticamente tutta la sequenza di indici --> O(n).
Inoltre, se si restituiscono tutte le coppie, la lista finale può avere dimensione fino a O(n^2).

Riassunto
+----------------+--------------------+----------------+----------------+
| Variante       | Tempo medio        | Tempo peggiore | Spazio         |
+----------------+--------------------+----------------+----------------+
| Solo conteggio | O(n * d)           | O(n * d)       | O(d)           |
| Con coppie     | O(n * d + #coppie) | O(n²)          | O(n + #coppie) |
+----------------+--------------------+----------------+----------------+


-------------------------------------------------
Ordinare una lista in base al MCD con un numero k
-------------------------------------------------

Data una lista L di interi e un numero intero k, restituire la lista degli interi ordinata in base ai valori di MCD[L(i), k].
La funzione deve essere la più corta possibile.

(define (order-gcd lst k show)
  (local (gcds coppie)
    ; calcola gli MCD di tutti gli elementi con k
    (setq gcds (map (curry gcd k) lst))
    (if show (println "gcds: " gcds))
    ; crea le coppie (mcd numero) per ogni numero
    (setq coppie (map (fn(x y) (list x y)) gcds lst))
    (if show (println "coppie: " coppie))
    ; ordina le coppie (tipo ordine: prima per gcd e poi per numero)
    (sort coppie)
    (if show (println "coppie ordinate: " coppie))
    ; prende tutti i numeri dalle coppie ordinate
    (map last coppie)))

Proviamo:

(setq L1 '(1 2 3 4 5 6 7))
(order-gcd L1 4 true)
;-> gcds: (1 2 1 4 1 2 1)
;-> coppie: ((1 1) (2 2) (1 3) (4 4) (1 5) (2 6) (1 7))
;-> coppie ordinate: ((1 1) (1 3) (1 5) (1 7) (2 2) (2 6) (4 4))
;-> (1 3 5 7 2 6 4)
;-> (1 3 5 7 2 6 4)

(setq L2 '(51 28 14 65 52 17 45 23 53 62))
(order-gcd L2 4 true)
;-> gcds: (1 4 2 1 4 1 1 1 1 2)
;-> coppie: ((1 51) (4 28) (2 14) (1 65) (4 52) (1 17) (1 45) (1 23)
;->          (1 53) (2 62))
;-> coppie ordinate: ((1 17) (1 23) (1 45) (1 51) (1 53) (1 65) (2 14)
;->                   (2 62) (4 28) (4 52))
;-> (17 23 45 51 53 65 14 62 28 52)

Versione code-golf (75 caratteri):

(define(o l k)(map last(sort(map(fn(x y)(list x y))(map(curry gcd k)l)l))))

(o L1 4)
;-> (1 3 5 7 2 6 4)
(o L2 4)
;-> (17 23 45 51 53 65 14 62 28 52)


----------------------------------------
Operazioni per rendere due numeri uguali
----------------------------------------

Abbiamo due interi con lo stesso numero di cifre.
Dobbiamo far diventare i numeri uguali usando solo le seguenti operazioni:
1) scambiare di posto due cifre ad uno dei due numeri
2) aggiungere 1 ad una cifra di uno dei due numeri (9+1=0)
Scrivere una funzione in newLISP che calcolare il numero minimo di operazioni per far diventare i due numeri uguali.
I numeri uguali possono anche essere diversi dai due numeri dati.
Esempio:
  a = 13420
  b = 31259
  Scambio 1 e 3 in a  --> a = 31420
  Scambio 4 e 2 in a  --> a = 31240
  Aggiungo 1 a 4 in a --> a = 31250
  Aggiungo 1 a 9 in b --> b = 31250
  a = b = 31250

Algoritmo
1. Trasformare i numeri in liste di cifre.
2. Controllare che abbiano la stessa lunghezza.
3. Generare tutte le permutazioni di 'digits-a'.
4. Per ogni permutazione calcolare il numero minimo di scambi usando la decomposizione in cicli.
5. Per ogni permutazione di 'b' calcolare analogamente gli scambi.
6. Per ogni coppia di permutazioni '(perm-a, perm-b)' calcola la somma delle distanze cicliche tra le cifre corrispondenti.
7. Sommare tutti i costi: scambi di 'a' + scambi di 'b' + trasformazioni +1.
8. Memorizzare il minimo totale tra tutte le combinazioni.

(define (int-list num)
"Convert an integer to a list of digits"
  (if (zero? num) '(0)
  (let (out '())
    (while (!= num 0)
      (push (% num 10) out)
      (setq num (/ num 10))) out)))

(define (list-int lst)
"Convert a list of digits to integer"
  (let (num 0)
    (dolist (el lst) (setq num (+ el (* num 10))))))

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

; Calcola la distanza ciclica minima tra due cifre (mod 10)
(define (cyclic-distance a b)
  ; Minimo tra andare avanti o indietro sulla ruota delle cifre
  (min (abs (- a b)) (- 10 (abs (- a b)))))

; Calcola il numero minimo di operazioni per rendere due numeri uguali
(define (min-operations a b)
  (if (= a b) 0
  ;else
  (let ((digits-a (int-list a))
        (digits-b (int-list b)))
    (if (!= (length digits-a) (length digits-b))
        nil
        ;else
        (let ((n (length digits-a))
              (min-total-cost 999999))
          ; Genera tutte le permutazioni di a
          (let ((all-perms-a (perm digits-a)))
            (dolist (perm-a all-perms-a)
              ; Calcola il costo degli scambi per ottenere questa permutazione
              (let ((swap-cost-a 0))
                ; Calcola il numero minimo di scambi (cicli nella permutazione)
                (let ((visited (array n '(nil))))
                  (for (i 0 (- n 1))
                    (unless (visited i)
                      (let ((cycle-length 0)
                            (current i))
                        (while (not (visited current))
                          (setf (visited current) true)
                          (inc cycle-length)
                          ; Trova dove va l'elemento corrente
                          (for (j 0 (- n 1))
                            (if (= (digits-a current) (perm-a j))
                                (setq current j))))
                        (if (> cycle-length 1)
                            (setq swap-cost-a (+ swap-cost-a (- cycle-length 1))))))))
                ; Ora prova tutte le permutazioni di b
                (let ((all-perms-b (perm digits-b)))
                  (dolist (perm-b all-perms-b)
                    ; Calcola il costo degli scambi per perm-b
                    (let ((swap-cost-b 0))
                      ; Calcolo analogo per b
                      (let ((visited-b (array n '(nil))))
                        (for (i 0 (- n 1))
                          (unless (visited-b i)
                            (let ((cycle-length 0)
                                  (current i))
                              (while (not (visited-b current))
                                (setf (visited-b current) true)
                                (inc cycle-length)
                                (for (j 0 (- n 1))
                                  (if (= (digits-b current) (perm-b j))
                                      (setq current j))))
                              (if (> cycle-length 1)
                                  (setq swap-cost-b (+ swap-cost-b (- cycle-length 1))))))))
                      ; Calcola il costo delle trasformazioni con +1 (mod 10)
                      (letn ((transform-cost (apply + (map cyclic-distance perm-a perm-b)))
                            (total-cost (+ swap-cost-a swap-cost-b transform-cost)))
                        ; Aggiorna il costo minimo se trovato uno più piccolo
                        (if (< total-cost min-total-cost)
                            (setq min-total-cost total-cost)))))))))
          min-total-cost)))))

Proviamo:

(min-operations 123 123)
;-> 0

(setq a 13420)
(setq b 31259)
(min-operations a b)
;-> 4

(setq a 12)
(setq b 31)
(min-operations a b)
;-> 2

(setq a 12)
(setq b 41)
(min-operations a b)
;-> 3

(setq a 12345)
(setq b 54321)
(min-operations 12345 54321)
;-> 2

Nota: il codice funziona correttamente, ma è molto lento per numeri con più di 5-6 cifre, perché il numero di permutazioni cresce esponenzialmente.

(time (println (min-operations 132683 945612)))
;-> 6
;-> 3656.564
(time (println (min-operations 1326837 9456125)))
;-> 8
;-> 225162.333

Per visualizzare le operazioni effettive (scambi e +1) dobbiamo tenere traccia di ogni operazione quando viene calcolata, invece di calcolare solo il costo.

Algoritmo
1) Converte a e b in liste di cifre.
2) Genera tutte le permutazioni di a e b.
3) Per ogni permutazione:
4)  - Calcola gli scambi minimi usando i cicli, e registra gli scambi in ops.
5)  - Calcola la trasformazione ciclica +1 per far corrispondere le cifre, e registra le +1 in ops.
6) Somma scambi e +1 per ottenere il costo totale.
7) Tiene traccia del costo minimo e della lista di operazioni corrispondente (best-ops).
8) Alla fine stampa il costo e la lista completa delle operazioni.

Lista delle operazioni:
+-------------------+-----------------------------+
| Tipo operazione   | Lista / campi               |
+-------------------+-----------------------------+
| Scambio ("swap")  | '("swap" numero pos1 pos2)' |
| Incremento ("+1") | '("+1" numero pos n)'       |
+-------------------+-----------------------------+
Esempi
  ("swap" "A" 0 2) --> scambia la cifra in posizione 0 con quella in posizione 2 del numero 'A'
  ("+1" "B" 3 4) --> aggiunge 1 per 4 volte alla cifra in posizione 3 del numero 'B'

; Calcola il numero minimo di operazioni per rendere due numeri uguali
; Restituisce (costo-minimo lista-operazioni)
(define (min-operations-ops a b)
  (if (= a b) (list 0 '())
  ;else
  (letn ((digits-a (int-list a)) (digits-b (int-list b)))
    (if (!= (length digits-a) (length digits-b))
        nil
        ;else
        (letn ((n (length digits-a)) (min-total-cost 999999) (best-ops '()))
          (let ((all-perms-a (perm digits-a)))
            (dolist (perm-a all-perms-a)
              (letn ((swap-cost-a 0) (ops-a '()) (visited (array n '(nil))))
                (for (i 0 (- n 1))
                  (unless (visited i)
                    (letn ((cycle-length 0) (cycle-pos '()) (current i))
                      (while (not (visited current))
                        (setf (visited current) true)
                        (push current cycle-pos -1)
                        (inc cycle-length)
                        (for (j 0 (- n 1))
                          (if (= (nth current digits-a) (nth j perm-a))
                              (setq current j))))
                      (if (> cycle-length 1)
                          (for (k 1 (- (length cycle-pos) 1))
                            (push (list "swap" "A" (nth 0 cycle-pos) (nth k cycle-pos)) ops-a -1)
                            (setf swap-cost-a (+ swap-cost-a 1)))))))
                (let ((all-perms-b (perm digits-b)))
                  (dolist (perm-b all-perms-b)
                    (letn ((swap-cost-b 0) (ops-b '()) (visited-b (array n '(nil))))
                      (for (i 0 (- n 1))
                        (unless (visited-b i)
                          (letn ((cycle-length 0) (cycle-pos '()) (current i))
                            (while (not (visited-b current))
                              (setf (visited-b current) true)
                              (push current cycle-pos -1)
                              (inc cycle-length)
                              (for (j 0 (- n 1))
                                (if (= (nth current digits-b) (nth j perm-b))
                                    (setq current j))))
                            (if (> cycle-length 1)
                                (for (k 1 (- (length cycle-pos) 1))
                                  (push (list "swap" "B" (nth 0 cycle-pos) (nth k cycle-pos)) ops-b -1)
                                  (setf swap-cost-b (+ swap-cost-b 1)))))))
                      (letn ((ops-transform '()) (transform-cost 0))
                        (for (i 0 (- n 1))
                          (let ((dist (cyclic-distance (nth i perm-a) (nth i perm-b))))
                            (when (> dist 0)
                              (push (list "+1" "A" i dist) ops-transform -1)
                              (setf transform-cost (+ transform-cost dist)))))
                        (let ((total-cost (+ swap-cost-a swap-cost-b transform-cost))
                              (ops-total (append ops-a ops-b ops-transform)))
                          (if (< total-cost min-total-cost)
                              (begin
                                (setf min-total-cost total-cost)
                                (setf best-ops ops-total))))))))))
            (println "Costo minimo:" min-total-cost)
            (println "Operazioni:")
            ;(println best-ops)
            (dolist (op best-ops) (println op))
            (list min-total-cost best-ops)))))))

Proviamo:

(min-operations-ops 123 123)
;-> (0 ())

(min-operations-ops 13420 31259)
;-> Costo minimo:4
;-> Operazioni:
;-> ("swap" "B" 0 1)
;-> ("swap" "B" 2 3)
;-> ("+1" "A" 2 1)
;-> ("+1" "A" 4 1)
;-> (4 (("swap" "B" 0 1) ("swap" "B" 2 3) ("+1" "A" 2 1) ("+1" "A" 4 1)))

(min-operations-ops 12 31)
;-> Costo minimo:2
;-> Operazioni:
;-> ("swap" "B" 0 1)
;-> ("+1" "A" 1 1)
;-> (2 (("swap" "B" 0 1) ("+1" "A" 1 1)))

(min-operations-ops 12 41)
;-> Costo minimo:3
;-> Operazioni:
;-> ("swap" "B" 0 1)
;-> ("+1" "A" 1 2)
;-> (3 (("swap" "B" 0 1) ("+1" "A" 1 2)))

(min-operations-ops 12345 54321)
;-> Costo minimo:2
;-> Operazioni:
;-> ("swap" "B" 0 4)
;-> ("swap" "B" 1 3)
;-> (2 (("swap" "B" 0 4) ("swap" "B" 1 3)))


(time (println (min-operations-ops 132683 945612)))
;-> Costo minimo:6
;-> Operazioni:
;-> ("swap" "A" 0 4)
;-> ("swap" "A" 1 5)
;-> ("+1" "A" 0 1)
;-> ("+1" "A" 1 1)
;-> ("+1" "A" 2 2)
;-> (6 (("swap" "A" 0 4) ("swap" "A" 1 5) ("+1" "A" 0 1) ("+1" "A" 1 1) ("+1" "A" 2 2)))
;-> 6250.516

(time (println (min-operations-ops 1326837 9456125)))
;-> Costo minimo:8
;-> Operazioni:
;-> ("swap" "A" 1 4)
;-> ("swap" "B" 0 6)
;-> ("+1" "A" 1 1)
;-> ("+1" "A" 2 1)
;-> ("+1" "A" 3 1)
;-> ("+1" "A" 5 2)
;-> ("+1" "A" 6 1)
;-> (8 (("swap" "A" 1 4) ("swap" "B" 0 6) ("+1" "A" 1 1) ("+1" "A" 2 1) ("+1" "A" 3 1)
;->   ("+1" "A" 5 2)
;->   ("+1" "A" 6 1)))
;-> 386633.251


----------------------
Sottostringhe omogenee
----------------------

Data una stringa determinare tutte le sottostringhe omogenee.
Una sottostringa omogenea è una sottostringa in cui tutti i caratteri sono identici.

Esempio:
  stringa = "aaa"
  sottostringhe omogenee = "a", "a", "a", "aa", "aa" e "aaa".

Esempio:
  stringa = "abbcccaa", le sottostringhe omogenee sono:
  Da "a":   1 sottostringa  ("a")
  Da "bb":  3 sottostringhe ("b", "b", "bb")
  Da "ccc": 6 sottostringhe ("c", "c", "c", "cc", "cc", "ccc")
  Da "aa":  3 sottostringhe ("a", "a", "aa")

Osservando l'ultimo esempio ("abbcccaa"), possiamo notare che sottostringhe omogenee possono derivare solo da segmenti continui di caratteri identici.
Non è possibile formare una sottostringa omogenea utilizzando segmenti con caratteri diversi.
Inoltre, quando incontriamo n caratteri uguali (es. "aaa") il numero di sottostringhe omogenee che possono essere formate vale:

  n + (n-1) + (n-2) + ... + 1 =  n*(n+1)/2 (che è la somma dei numeri da 1 a n).

Infatti la stringa "aaa" che ha n = 3 genera (3*2)/2 = 6 sottostringhe omogenee:
  3 Sottostringhe omogenee di lunghezza 1: "a", "a", "a"
  2 Sottostringhe omogenee di lunghezza 2: "aa", "aa"
  1 Sottostringa  omogenea di lunghezza 3: "aaa"

Funzione che somma i numeri da 1 a n:

(define (sum-1n n) (/ (* n (+ n 1)) 2))

Funzione che restituisce tutti i segmenti di una stringa con cartteri consecutivi uguali:

(define (segment-string str)
  (if (= str "") '()
  ;else
  (let ( (out '())
         (cur-char "")
         (len (length str)))
    (for (i 0 (- len 1))
      (if (or (= i 0) (= (str i) (str (- i 1))))
          (setf cur-char (extend cur-char (string (str i))))
          (begin
            (push cur-char out -1)
            (setf cur-char (string (str i))))))
    (push cur-char out -1)
    out)))

Proviamo:

(segment-string "")
;-> ()
(segment-string "abbcccaa")
;-> ("a" "bb" "ccc" "aa")
(segment-string "aabbcccbbaaf")
;-> ("aa" "bb" "ccc" "bb" "aa" "f")

Funzione che genera tutte le sottostringhe omogenee di una stringa che ha n caratteri uguali:

(define (omogenee-equal str)
  (if (= str "") '()
  ;else
  (let ( (len (length str)) (ch (str 0)) (out '()) )
    ; ciclo per ogni segmento di lunghezza: n, (n-1), (n-2), ..., 1
    (for (i 1 len)
      ; segmento corrente
      (setq seg (dup ch i))
      ; inserimento del segmento corrente nella soluzione
      (for (k 0 (- len i)) (push seg out -1)))
    out)))

Proviamo:

(omogenee-equal "aaa")
;-> ("a" "a" "a" "aa" "aa" "aaa")
(omogenee-equal "a")
;-> ("a")
(omogenee-equal "")
;-> ()
Se la stringa non contiene caratteri tutti uguali, allora la funzione usa solo il primo carattere:
(omogenee-equal "abc")
;-> ("a" "a" "a" "aa" "aa" "aaa")

Funzione finale che calcola tutte le sottostringhe omogenee di una stringa data:
all = true --> calcola il numero e la lista delle sottostringhe omogenee
all = nil  --> calcola il numero delle sottostringhe omogenee

(define (omogenee str all)
  (let (segs (segment-string str))
    (cond ((nil? all) ; solo il numero delle sottostringhe
            (let (totale 0)
              (dolist (s segs) (++ totale (sum-1n (length s))))
              totale))
          ((true? all) ; il numero e il valore delle sottostringhe
            (let (out '())
              ;(println segs)
              (dolist (s segs)
              ;(print (omogenee-equal s)) (read-line)
              (extend out (omogenee-equal s)))
              (list (length out) out))))))

Proviamo:

(omogenee "")
;-> 0
(omogenee "" true)
;-> (0 ())
(omogenee "abbcccaa")
;-> 13
(omogenee "abbcccaa" true)
;-> (13 ("a" "b" "b" "bb" "c" "c" "c" "cc" "cc" "ccc" "a" "a" "aa"))
(omogenee "aabbcccbbaaf" true)
;-> (19 ("a" "a" "aa" "b" "b" "bb" "c" "c" "c" "cc" "cc" "ccc"
;->      "b" "b" "bb" "a" "a" "aa" "f"))
(omogenee "xxxxx" true)
;-> (15 ("x" "x" "x" "x" "x" "xx" "xx" "xx" "xx"
;->      "xxx" "xxx" "xxx" "xxxx" "xxxx" "xxxxx"))
(omogenee "1234567890" true)
;-> (10 ("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
(omogenee "aabb" true)
;-> (6 ("a" "a" "aa" "b" "b" "bb"))


-------------------------
Punti e angoli di visione
-------------------------

In un piano 2D abbiamo un punto O (Osservatore) in una posizione fissa (x,y).
Inizialmente il punto è rivolto (guarda) verso Est (direzione x positiva) con un angolo di visione (cono visivo) di Alfa (gradi).
Il punto O guarda solo in avanti (non guarda anche dietro di lui).
Un altro punto P si trova in posizione (x1,y1).
Determinare se il punto O vede il punto P.

    Y
  
    |
    |      left-ray
    |        /
  y1|       /              P
    |      /
    |     / alfa/2
   y|    O-------------> direzione visiva
    |     \ alfa/2
    |      \
    |       \
    |        \
    |      right ray
  0 +---------------------------- X
    0    x                 x1

Per generalizzare la situazione, supponiamo che il punto possa ruotare in senso antiorario di un angolo pari a Beta (gradi).
Quando ruota ('facing') di Beta gradi, il suo campo visivo copre tutti gli angoli nell'intervallo (Beta - alfa/2, Beta + alfa/2)
facing = +x --> angoli positivi --> ruota antiorario
(es. da Est verso Nord se x = pi/2).
facing = -x --> angoli negativi --> ruota orario
(es. da Est verso Sud se x = pi/2).

In pratica per "ruotare" il campo visivo di P di x radianti, basta impostare il facing = x.
  facing = 0 --> guarda a Est
  facing = pi/2 --> guarda a Nord
  facing = pi --> guarda a Ovest
  facing = -pi/2 --> guarda a Sud

Il punto P è visibile se l'angolo che forma con la posizione di P (rispetto alla direzione Est) rientra nel campo visivo attuale.
Quindi occorre confrontare l'angolo direzionale del punto O con l'angolo verso P, normalizzando gli angoli per gestire il "wrap" intorno a -pi...pi.
Il calcolo dell'angolo relativo tra O e P deve essere fatto tenendo conto del valore di 'facing'.

Algoritmo:
1) calcolare dx = x1-x, dy = y1-y
2) angolo verso P: atan2(dy,dx)
3) angolo di facing (0 = Est, pi/2 = nord, ecc.)
4) prendere la differenza e normalizzarla in (-pi,pi]
5) confrontore abs(diff) <= alpha/2

- atan2(dy,dx) dà l'angolo assoluto verso P rispetto all'asse X positivo (Est).
- 'facing' è l'angolo che indica la direzione in cui guarda O (in radianti). Se O guarda Est, facing = 0.
- 'alpha' è l'ampiezza dell'angolo di visione in radianti
- La normalizzazione evita problemi quando l'angolo attraversa -pi/pi (es. facing vicino a pi).

Schema logico:
1) Differenze di coordinate
   Calcola
   dx = x1 - x, dy = y1 - y
   cioè lo spostamento dal punto O a P.
2) Angolo assoluto verso P
   Usa 'atan2(dy, dx)' per ottenere l'angolo (in radianti) che il vettore OP forma con l'asse X positivo (Est).
   Questo angolo è compreso tra -pi e pi.
3) Angolo relativo rispetto a facing
   Sottrae l'angolo 'facing' (la direzione di osservazione di O):
   rel = angle - text
   Poi normalizza questo valore nell'intervallo (-pi, pi] per evitare problemi di salto quando si passa da pi a -pi.
4) Ampiezza del campo visivo
   Prende alpha/2, che rappresenta la metà dell'angolo di visione (metà a sinistra e metà a destra rispetto alla direzione di facing).
5) Confronto finale
   Se (rel <= alpha/2), allora il punto P cade dentro il cono visivo di O.
   Altrimenti è fuori.

; valore di pi greco
(setq pi 3.1415926535897931)

(define (deg-rad deg)
"Convert decimal degrees to radiants"
  (mul deg 1.745329251994329577e-2))

(define (normalize-angle a) ; normalizza un angolo nell'intervallo (-pi, pi]
 (letn ((t (mod a (* 2 pi))))
  (if (> t pi) (sub t (* 2 pi)) (if (<= t (- pi)) (add t (* 2 pi)) t))))

Funzione che verifica se il punto O vede il punto P:

(define (sees? ox oy px py alfa facing)
 ; ox, oy = coordinate osservatore O
 ; px, py = coordinate punto P
 ; alfa = ampiezza dell'angolo di visione (in gradi)
 ; facing = direzione in cui guarda O (0=Est, pi/2=Nord, ecc.)
 (letn (
   (alpha (deg-rad alfa)) ; alfa in radianti
   (dx (sub px ox)) ; differenza x tra P e O
   (dy (sub py oy)) ; differenza y tra P e O
   (angle (atan2 dy dx)) ; angolo assoluto verso P rispetto a Est
   (rel (normalize-angle (sub angle facing))) ; angolo relativo tra facing e P
   (half (div alpha 2)) ; metà del campo visivo
  )
  (<= (abs rel) half))) ; vero se P cade entro +-alpha/2 (nel campo visivo)

Il ruolo di 'facing' viene incluso nel passo:
  rel = normalize-angle(angle - facing)
Questa operazione "sposta" il sistema di riferimento in modo che la direzione in cui guarda O diventi 0.
Quindi dopo la normalizzazione il confronto si riduce a chiedere:
    rel <= alpha/2
Ecco perché 'half' non tiene conto di 'facing': l'angolo relativo è già stato calcolato.

Proviamo:

(sees? 0 0 1 1 90 0)
;-> true
(sees? 0 0 -1 1 90 0)
;-> nil
(sees? 5 5 10 6 60 0)
;-> true
(sees? 5 5 5 10 60 0)
;-> nil
(sees? 5 5 10 6 60 (div pi 2))
;-> nil
(sees? 5 5 5 10 60 (div pi 2))
;-> true
(sees? 5 5 10 6 2 0)
;-> nil
(sees? 5 5 10 5 2 0)
;-> true

Nota:
Se vogliamo inserire il parametro della profondita della visione (PV) del punto O, basta verificare che la distanza tra P e O sia minore o uguale a PV (il cono visivo viene delimitato da un arco di cerchio di raggio PV e centro in O).


---------------
Divisori vicini
---------------

Dato un numero intero N, trova i due numeri interi più vicini in differenza assoluta il cui prodotto è uguale a N+1 o N+2.

Esempio
N = 8
Output = (3 3)
N + 1 = 9  --> divisori vicini (3 3) --> 3 - 3 = 0 (differenza minima)
N + 2 = 10 --> divisori vicini (2 5) --> 5 - 2 = 3

I due interi più vicini in differenza assoluta sono inferiori o uguali a sqrt(N).
Questo perchè i fattori da moltiplicare diventano più "bilanciati" man mano che ci avviciniamo alla radice quadrata.

Algoritmo
---------
Per N+1:
1) Iniziare l'iterazione da i = int(sqrt(N)) fino a 1
2) Per ogni i, controllare se N % i == 0 (ovvero, i divide x esattamente)
3) Quando troviamo il primo divisore, restituiamo la coppia (i N/i)
4) Poiché partiamo da sqrt(x) e procediamo verso il basso, il primo divisore che troviamo ci darà la coppia di fattori con differenza minima 'diff1'.
Per N+2:
5) lo stesso procedimento ci porta a calcolare 'diff2'.
6) Restituire la coppia di fattori per cui la differenza assoluta è minore.

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (close-div N)
  ; calcolo con N+1
  (letn ( (num (+ N 1))
          (idx (int (sqrt num)))
          (d1 '()) (d2 '()) (diff1 0) (diff2 0) )
    (if (prime? num) ; se num è primo non occorre effettuare il ciclo
        (set 'd1 (list 1 num) 'diff1 (- num 1))
    ;else
        (while (> idx 0)
          (when (zero? (% num idx))
            (setq d1 (list idx (/ num idx)))
            (setq diff1 (abs (- idx (/ num idx))))
            (setq idx 0))
          (-- idx)))
    (println (apply * d1) { } d1 { } diff1)
    ; calcolo con N+2
    (setq num (+ N 2))
    (if (prime? num) ; se num è primo non occorre effettuare il ciclo
        (set 'd2 (list 1 num) 'diff2 (- num 1))
    ;else        
        (setq idx (int (sqrt num)))
        (while (> idx 0)
          (when (zero? (% num idx))
            (setq d2 (list idx (/ num idx)))
            (setq diff2 (abs (- idx (/ num idx))))
            (setq idx 0))
          (-- idx)))
    (println (apply * d2) { } d2 { } diff2)
    (if (<= diff1 diff2)
        (list (apply * d1) d1 diff1)
        (list (apply * d2) d2 diff2))))

Proviamo:

(close-div 8)
;-> ((3 3) 0)
;-> 9 (3 3) 0
;-> 10 (2 5) 3
;-> (9 (3 3) 0)

(close-div 10)
;-> 11 (1 11) 10
;-> 12 (3 4) 1
;-> (12 (3 4) 1)

(close-div 888)
;-> 889 (7 127) 120
;-> 890 (10 89) 79
;-> (890 (10 89) 79)

(close-div 123456789)
;-> 123456790 (370 333667) 333297
;-> 123456791 (1 123456791) 123456790
;-> (123456790 (370 333667) 333297)


-----------------------------
K-esimo divisore di un numero
-----------------------------

Dato un numero intero positivo N determinare il suo divisore k-esimo.
Se K è maggiore del numero di divisori di N, allora restituire nil.

Esempi:
  N = 6
  K = 3
  Divisori di 6 = (1 2 3)
  K-esimo divisore = Terzo divisore = 3
  
  N = 12
  K = 5
  Divisori di 12 = (1 2 3 4 6 12)
  K-esimo divisore = Quinto divisore = 6

Algoritmo Brute-Force 1
-----------------------
Ciclo per x che va da 1 a num
  Se x è divisore di num, diminuiamo k di 1.
  Se k == 0, allora abbiamo trovato la soluzione e usciamo dal ciclo.
Se usciamo dal ciclo con k > 0, allora restituire nil.

(define (k-divisor num k)
  (let ( (out 0) (stop nil) )
    (for (i 1 num 1 stop)
      (when (zero? (% num i))
        (-- k)
        (if (zero? k) (set 'out i 'stop true))))
    (if stop out nil)))

Proviamo:

(k-divisor 6 3)
;-> 3

(k-divisor 12 5)
;-> 6

(k-divisor 11 2)
;-> 11

(k-divisor 5 5)
;-> nil

(time (println (k-divisor 123456789 10)))
;-> 13717421
;-> 793.625

(time (println (k-divisor 123456789 12)))
;-> 123456789
;-> 6979.883

Algoritmo Brute-Force 2
-----------------------
Calcoliamo tutti i divisori con un algoritmo più veloce del ciclo da 1 a N.
Dalla lista dei divisori troviamo, se esiste, il k-esimo divisore (in realtà il (k-1)-esimo della lista perchè gli indici sono zero-based).

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

(define (k-divisore num k)
  (let (divisori (divisors num))
    (if (> k (length divisori)) nil (divisori (- k 1)))))

Proviamo:

(k-divisore 6 3)
;-> 3

(k-divisore 12 5)
;-> 6

(k-divisore 11 2)
;-> 11

(k-divisore 5 5)
;-> nil

(time (println (k-divisore 123456789 10)))
;-> 13717421
;-> 0

(time (println (k-divisore 123456789 12)))
;-> 123456789
;-> 0

Test di velocità:

(setq nums (rand 10e6 50))
(setq kappa (map + (rand 10 50) (dup 1 50)))

(time (setq t1 (map (fn(x y) (k-divisor x y)) nums kappa)))
;-> 4064.888

(time (setq t2 (map (fn(x y) (k-divisore x y)) nums kappa)))
;-> 0.998


---------------------------------------------------
Quanti numeri primi possiamo calcolare con newLISP?
---------------------------------------------------

La funzione 'primes-to' può calcolare i primi fino al numero 1e8 (con 32 GB RAM).
Il limite è dovuto alla dimensione del vettore che ha dimensione (1e8 + 1).

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

Proviamo:

(time (println (length (primes-to 1e6))))
;-> 78498
;-> 140.621
(time (println (length (primes-to 1e7))))
;-> 664579
;-> 1656.263
(time (println (length (primes-to 1e8))))
;-> 5761455
;-> 18329.923

Per ogni potenza il tempo si moltiplica circa per 10.

La funzione 'primiN' può calcolare i primi anche maggiori di 1e8, ma è molto lenta.
Il limite è dovuto al ciclo 'for' che deve arrivare a 1e8 (con passo 2).

(define (primiN N)
  (let (tot 1)
    (for (num 3 N 2) (if (= 1 (length (factor num))) (++ tot)))
  tot))

Proviamo:

(primiN 100)
;-> 25

(time (println (primiN 1e6)))
;-> 78498
;-> 390.561
(time (println (primiN 1e7)))
;-> 664579
;-> 9438.386
(time (println (primiN 1e8)))
;-> 5761455
;-> 246741.111 ; 247 secondi

Per ogni potenza il tempo si moltiplica circa per 25.


-------------------------------------------------------------------------
Prodotto di cifre uguale al prodotto tra somma di cifre e radice digitale
-------------------------------------------------------------------------

Sequenza di numeri per cui prodotto di cifre è uguale al prodotto tra la somma delle cifre e la radice digitale.

(define (digit-root num)
"Calculate the repeated sum of the digits of an integer"
    (+ 1 (% (- (abs num) 1) 9)))

(define (digit-sum num)
"Calculate the sum of the digits of an integer"
  (let (out 0)
    (while (!= num 0)
      (setq out (+ out (% num 10)))
      (setq num (/ num 10))
    )
    out))

(define (digit-prod num)
"Calculate the product of the digits of an integer"
  (if (zero? num)
      0
      (let (out 1)
        (while (!= num 0)
          (setq out (* out (% num 10)))
          (setq num (/ num 10))
        )
    out)))

(define (check? num) (= (digit-prod num) (* (digit-sum num) (digit-root num))))

(filter check? (sequence 1 1e5))
;-> (1 66 257 275 369 396 527 572 639 693 725 752 936 963 1236 1263 1326
;->  1356 1362 1365 1447 1474 1536 1563 1623 1632 1635 1653 1744 2136
;->  2163 2316 2361 2613 2631 3126 3156 3162 3165 3216 3261 3516 3561
;->  3612 3615 3621 3651 4147 4174 4417 4471 4714 4741 5136 5163 5316
;->  5361 5613 5631 6123 6132 6135 6153 6213 6231 6312 6315 6321 6351
;->  6513 6531 7144 7414 7441 11125 11152 11215 11251 11334 11343 11433
;->  11512 11521 12115 12151 12247 12274 12339 12393 12427 12472 12511
;->  12724 12742 12933 13134 13143 13239 13293 13314 13329 13341 13392
;->  13413 13431 13923 13932 14133 14227 14272 14313 14331 14722 15112
;->  15121 15211 17224 17242 17422 19233 19323 19332 21115 21151 21247
;->  21274 21339 21393 21427 21472 21511 21724 21742 21933 22147 22174
;->  22417 22471 22714 22741 23139 23193 23319 23391 23913 23931 24127
;->  24172 24217 24271 24712 24721 25111 27124 27142 27214 27241 27412
;->  27421 29133 29313 29331 31134 31143 31239 31293 31314 31329 31341
;->  31392 31413 31431 31923 31932 32139 32193 32319 32391 32913 32931
;->  33114 33129 33141 33192 33219 33291 33411 33912 33921 34113 34131
;->  34311 39123 39132 39213 39231 39312 39321 41133 41227 41272 41313
;->  41331 41722 42127 42172 42217 42271 42712 42721 43113 43131 43311
;->  47122 47212 47221 51112 51121 51211 52111 71224 71242 71422 72124
;->  72142 72214 72241 72412 72421 74122 74212 74221 91233 91323 91332
;->  92133 92313 92331 93123 93132 93213 93231 93312 93321)

Nota: questa sequenza non esiste su OEIS (12 settembre 2025).


--------------------------------------
Frequenza delle cifre dei numeri primi
--------------------------------------

Calcolare la frequenza delle cifre dei numeri primi fino a N.

(define (int-list num)
"Convert an integer to a list of digits"
  (if (zero? num) '(0)
  (let (out '())
    (while (!= num 0)
      (push (% num 10) out)
      (setq num (/ num 10))) out)))

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

Metodo 1
--------
Calcola la lista con tutte le cifre dei numeri primi fino a N.
Calcola le frequenze delle cifre dalla lista.

(define (digit-freq1 N)
  (local (all cifre primi conta-cifre num-cifre freq)
    (setq all '())
    ; lista delle cifre
    (setq cifre '(0 1 2 3 4 5 6 7 8 9))
    ; crea una lista con tutti i numeri primi fino a N
    (setq primi (primes-to N))
    ; crea una lista 'all' con tutte le cifre dei primi
    (dolist (p primi) (extend all (int-list p)))
    ; conta le occorrenze delle cifre 0..9 nella lista delle cifre
    (setq conta-cifre (count cifre all))
    ; numero totale di cifre
    (setq num-cifre (apply + conta-cifre))
    ; crea una lista 'freq' con la frequenza di ogni cifra (0..9)
    (setq freq (map (fn(x) (format "%.3f" (div x num-cifre))) conta-cifre))
    ; crea una lista di elementi (cifra frequenza) per ogni cifra (0..9)
    (map (fn(x) (list $idx (float x))) freq)))

(digit-freq1 10)
;-> ((0 0) (1 0) (2 0.25) (3 0.25) (4 0) (5 0.25) (6 0) (7 0.25) (8 0) (9 0))

(digit-freq1 1e4)
;-> ((0 0.049) (1 0.144) (2 0.083) (3 0.143) (4 0.076) (5 0.076) (6 0.078) (7 0.138) (8 0.074) (9 0.137))

(digit-freq1 1e5)
;-> ((0 0.059) (1 0.137) (2 0.084) (3 0.134) (4 0.081) (5 0.082) (6 0.080) (7 0.133) (8 0.079) (9 0.132))

(digit-freq1 1e6)
;-> ((0 0.066) (1 0.130) (2 0.086) (3 0.128) (4 0.085) (5 0.085) (6 0.084) (7 0.127) (8 0.084) (9 0.126))

(time (println (digit-freq1 1e7)))
;-> ((0 0.071) (1 0.125) (2 0.088) (3 0.124) (4 0.087) (5 0.087) (6 0.087) (7 0.123) (8 0.086) (9 0.123))
;-> 4562.941

(time (println (digit-freq1 1e8)))
;-> ((0 0.075) (1 0.122) (2 0.089) (3 0.121) (4 0.089) (5 0.089) (6 0.088) (7 0.120) (8 0.088) (9 0.120))
;-> 50160.665

Metodo 2
--------
Usa un vettore per il conteggio delle cifre.
Per ogni numero primo aggiorna il vettore delle cifre.

(define (digit-freq2 N)
  (local (cifre primi conta-cifre num-cifre freq)
    ; lista delle cifre
    (setq cifre '(0 1 2 3 4 5 6 7 8 9))
    ; vettore delle occorrenze delle cifre
    (setq conta-cifre (array 10 '(0)))
    ; crea una lista con tutti i numeri primi fino a N
    (setq primi (primes-to N))
    ; ciclo che aggiorna il vettore delle occorrenze delle cifre
    ; con tutti i numeri primi
    (dolist (p primi)
      (while (!= p 0)
        (++ (conta-cifre (% p 10)))
        (setq p (/ p 10))))
    ; numero totale di cifre
    (setq num-cifre (apply + conta-cifre))
    ; crea una lista 'freq' con la frequenza di ogni cifra (0..9)
    (setq freq (map (fn(x) (format "%.3f" (div x num-cifre))) conta-cifre))
    ; crea una lista di elementi (cifra frequenza) per ogni cifra (0..9)
    (map (fn(x) (list $idx (float x))) freq)))

(digit-freq2 10)
;-> ((0 0) (1 0) (2 0.25) (3 0.25) (4 0) (5 0.25) (6 0) (7 0.25) (8 0) (9 0))

(digit-freq2 1e5)
;-> ((0 0.059) (1 0.137) (2 0.084) (3 0.134) (4 0.081) (5 0.082) (6 0.08) (7 0.133) (8 0.079) (9 0.132))

(= (digit-freq1 1e6) (digit-freq2 1e6))
;-> true

(time (println (digit-freq2 1e7)))
;-> ((0 0.071) (1 0.125) (2 0.088) (3 0.124) (4 0.087) (5 0.087) (6 0.087) (7 0.123) (8 0.086) (9 0.123))
;-> 2250.191

(time (println (digit-freq2 1e8)))
;-> ((0 0.075) (1 0.122) (2 0.089) (3 0.121) (4 0.089) (5 0.089) (6 0.088) (7 0.120) (8 0.088) (9 0.120))
;-> 24330.508

Con l'aumentare del numero dei primi le cifre si comportano nel modo seguente:

  +------ +---------------+
  | Cifra | Comportamento |
  +------ +---------------+
  |   0   | aumenta       |
  |   1   | diminuisce    |
  |   2   | aumenta       |
  |   3   | diminuisce    |
  |   4   | aumenta       |
  |   5   | aumenta       |
  |   6   | aumenta       |
  |   7   | diminuisce    |
  |   8   | aumenta       |
  |   9   | diminuisce    |
  +------ +---------------+


-----------------------
Una funzione come "map"
-----------------------

Una funzione per modificare i valori di una lista.
Parametri:
  lst = lista di elementi
  func = funzione da applicare
  val = valore da usare nella funzione

(define (touch lst func val)
  (if val
    (map (curry func val) lst)
    (map func lst)))

Esempi:
(setq lst '(1 2 3 4 5))

(touch lst + 1)
;-> (2 3 4 5 6)

(touch lst sin)
;-> (0.8414709848078965 0.9092974268256817 0.1411200080598672
;->  -0.7568024953079282 -0.9589242746631385)

(define (f a b) (* a b))
(touch lst f 2)
;-> (2 4 6 8 10)
(touch lst * 2)
;-> (2 4 6 8 10)


-------------------------------------------------------------------
Statistiche di un campione di interi con le occorrenze degli interi
-------------------------------------------------------------------

Abbiamo una lista L di N numeri.
Ogni numero L(k) rappresenta il numero di volte che l'intero k appare in un campione di interi più grande.
Tutti gli interi nel campione sono compresi nell'intervallo [0, 255].
Calcolare le seguenti cinque misure statistiche del campione:
1) Minimo: il valore più piccolo che appare nel campione (con count > 0)
2) Massimo: il valore più grande che appare nel campione (con count > 0)
3) Media: il valore medio, calcolato come la somma di tutti gli elementi divisa per il numero totale di elementi
4) Mediana:
   se il numero totale di elementi è dispari, la mediana è l'elemento centrale quando tutti gli elementi sono ordinati
   se il numero totale di elementi è pari, la mediana è la media dei due elementi centrali quando ordinati
5) Moda: il valore che appare più frequentemente nel campione (univoco garantito)

Esempio:
lista = (0 1 3 4 0 0 0]
Il campione vale (1 2 2 2 3 3 3 3) perchè:
Il valore 0 appare 0 volte
Il valore 1 appare 1 volta
Il valore 2 appare 3 volte
Il valore 3 appare 4 volte
Il valore 4 appare 0 volte
Il valore 5 appare 0 volte
Il valore 6 appare 0 volte
In questo caso il campione ha 8 elementi.

Una soluzione potrebbe essere quella di ricreare tutti i numeri del campione e poi calcolare le misure statistiche.
Comunque questo non è fattibile nel caso il campione contenga moltissimi numeri.

Un'altra soluzione potrebbe essere quella di attraversare la lista L ed estrapolare le misure statistiche dalle occorrenze dei numeri del campione.
Questo è fattibile, ma, poichè la lista L contiene al massimo 256 numeri, proviamo ad utilizzare le primitive di ricerca.

Elementi (Numero di elementi della lista diversi da 0):
- (length (clean zero? lst)))

Totale (Numero di elementi del campione)
- (apply + (clean zero? lst)))

Minimo: (Valore minimo dei numeri del campione)
- (find 0 lst <) trova l'indice del primo elemento che è maggiore di 0.

Massimo: (Valore massimo dei numeri del campione)
- (reverse (copy lst)) mette l'ultimo elemento in testa.
- (find 0 (reverse (copy lst)) <) trova l'indice del primo elemento che è maggiore di 0.
- (- (length lst) 1 ...) trasforma l'indice dalla lista rovesciata a quello originale.

Somma: (Somma di tutti i numeri del campione)
- (apply + (map (fn(x) (* x $idx)) lst)) moltiplica ogni elemento per il proprio indice e poi somma tutto

Media: (Media dei numeri del campione)
(div somma totale)

Moda: (Moda dei numeri del campione)
- (find (apply max lst) lst) trova l'indice del valore massimo della lista data

Mediana: (Mediana dei numeri del campione)
- per la mediana dobbiamo scrivere una funzione specifica

Funzione che calcola le misure statistiche di un campione di interi partendo dalla lista delle occorrenze degli interi del campione:

(define (stat lst)
  (local (elementi minimo massimo somma media moda)
    ; Numero di elementi diversi da 0
    (setq elementi (length (clean zero? lst)))
    ; Totale numeri del campione
    (setq totale (apply + (clean zero? lst)))
    ; trova il minimo dei numeri
    (setq minimo (find 0 lst <))
    (println "minimo: " minimo)
    ; trova il massimo dei numeri
    (if (!= (lst -1) 0)
      (setq massimo (- (length lst) 1))
      (setq massimo (- (length lst) (find 0 (reverse (copy lst)) <) 1)))
    (println "massimo: " massimo)
    ; calcola la somma dei numeri
    (setq somma (apply + (map (fn(x) (* x $idx)) lst)))
    (println "somma: " somma)
    ; calcola la media dei numeri
    (setq media (div somma totale))
    (println "media: " media)
    ; calcola la mediana dei numeri
    (setq mediana (median lst))
    (println "mediana: " mediana)
    ; calcola la moda dei numeri
    (setq moda (find (apply max lst) lst))
    (println "moda: " moda)
  )
)

Per calcolare la mediana dobbiamo trovare il valore dell'indice centrale del campione.
Per fare questo attraversiamo la lista fino a che non abbiamo raggiunto l'indice centrale.

(define (median lst)
  (local (totale mid mid1 mid2 stop indice mediana)
    ; Totale numeri del campione
    (setq totale (apply + (clean zero? lst)))
    (println "totale:" totale)
    (cond ((odd? totale) ; totale numeri dispari
            (setq mid (/ totale 2))
            (setq stop nil)
            (setq indice 0)
            ; ciclo su lst per arrivare all'indice corretto...
            (dolist (el lst stop)
              ; indice corrente
              (setq indice (+ indice el))
              ;(print $idx { } indice { } mid) (read-line)
              ; quando superiamo 'mid', abbiamo trovato la mediana
              (when (>= indice mid)
                (setq mediana $idx)
                (setq stop true))))
          ((even? totale) ; totale numeri pari
            (setq mid (/ totale 2))
            (setq stop nil)
            (setq indice 0)
            (dolist (el lst stop)
              (setq indice (+ indice el))
              ;(print $idx { } indice { } mid) (read-line)
              (cond ((= indice mid)
                      ; quando 'indice' == 'mid', abbiamo trovato mid1
                      ; mid1 = $idx
                      ; per trovare mid2 occorre andare avanti nella lista
                      ; fino a trovare il primo indice valido (non-zero)
                      ; mid2 = ($idx + 1)
                      ; mediana = ($idx + ($idx + 1))/2
                      (setq mid1 $idx)
                      ; trova l'indice del primo elemento maggiore di 'el'
                      (setq mid2 (find el lst <))
                      ;(println mid1 { } mid2)
                      (setq mediana (div (+ mid1 mid2) 2))
                      (setq stop true))
                    ((> indice mid)
                      ; quando superiamo 'mid', abbiamo trovato la mediana
                      ; mid1 = $idx e mid2 = $idx
                      ; mediana = ($idx + $idx)/2 = $idx
                      (setq mediana $idx)
                      (setq stop true))))))
    mediana))

Proviamo:

lista:
(setq a '(0 1 3 4 0 0 0 0))
campione:
(setq aa (1 2 2 2 3 3 3 3))

(stat a)
;-> minimo: 1
;-> massimo: 3
;-> somma: 19
;-> media: 2.375
;-> mediana: 2.5
;-> moda: 3

lista:
(setq b '(0 4 3 2 2 0 0 0))
campione:
(setq bb '(1 1 1 1 2 2 2 3 3 4 4))

(stat b)
;-> minimo: 1
;-> massimo: 4
;-> somma: 24
;-> media: 2.181818181818182
;-> mediana: 2
;-> moda: 1

lista
(setq c '(0 1 3 0 0 0 4 0 0 0 0))
campione:
(setq cc '(1 2 2 2 6 6 6 6))

(stat c)
;-> minimo: 1
;-> massimo: 6
;-> somma: 31
;-> media: 3.875
;-> totale:8
;-> mediana: 4
;-> moda: 6

lista
(setq d '(1 1 3 0 0 0 3 0 0 0 0))
campione:
(setq dd '(0 1 2 2 2 6 6 6))

(stat d)
;-> minimo: 0
;-> massimo: 6
;-> somma: 25
;-> media: 3.125
;-> totale:8
;-> mediana: 2
;-> moda: 2


--------------
Numeri confusi
--------------

Un numero confuso è un numero che diventa un numero diverso quando viene ruotato di 180 gradi, con tutte le cifre che rimangono valide dopo la rotazione.
Quando ruotiamo le singole cifre di 180 gradi:
  0 rimane 0
  1 rimane 1
  6 diventa 9
  8 rimane 8
  9 diventa 6
  2, 3, 4, 5 e 7 diventano non validi (non possono essere ruotati)
Il processo di rotazione funziona ruotando ogni cifra singolarmente e poi leggendo il risultato da destra a sinistra.

Il termine "confuso" deriva dalla seguente situazione:
Supponiamo di avere uno scatolone in cui è scritto un numero su una faccia.
Se rovesciamo lo scatolone anche il numero viene rovesciato di 180 gradi e genera un altro numero.
Se entrambi i numeri sono validi, allora entrambi i numeri sono "confusi" (cioè non sappiamo qual è il vero numero dello scatolone).

Esempio 1:
  N = 16
  Rotazioni:
  1 diventa 1
  6 diventa 9
  Numero risultante = 91
  Poichè 16 è diverso da 91, allora 16 è un numero confuso.

Esempio 2:
  N = 69
  Rotazioni:
  6 diventa 9
  9 diventa 6
  Numero risultante = 69
  Poichè 16 è uguale a 69, allora 69 non è un numero confuso.

Esempio 3:
  N = 126
  Rotazioni:
  1 diventa 1
  2 non può essere ruotato, quindi 126 non è un numero confuso

Sequenza OEIS A272264:
Numbers that become a different number when flipped upside down.
  6, 9, 16, 18, 19, 61, 66, 68, 81, 86, 89, 91, 98, 99, 106, 108, 109, 116,
  118, 119, 161, 166, 168, 169, 186, 188, 189, 191, 196, 198, 199, 601, 606,
  608, 611, 616, 618, 661, 666, 668, 669, 681, 686, 688, 691, 696, 698, 699,
  801, 806, 809, 811, 816, 819, 861, 866, 868, 869, 881, 886, 889, ...

(define (confuse? num)
  (local (out flip str rev stop cifra)
    ; all'inizio il numero è confuso
    (setq out true)
    ; lista delle rotazioni dei numeri
    (setq flip '(("0" "0") ("1" "1") ("6" "9") ("8" "8") ("9" "6")))
    (setq str (string num))
    ; numero ruotato
    (setq rev "")
    (setq stop nil)
    ; ciclo di rotazione per ogni cifra 
    (dolist (s (explode str) stop)
      ; ricerca la cifra ruotata
      (setq cifra (lookup s flip))
      (if cifra
          (push cifra rev) ; cifra ruotata
          (set 'stop true 'out nil))) ; impossibile ruotare cifra. Stop ciclo.
    ; Adesso per essere confuso deve risultare
    ; il numero ruotato deve avere un valore
    ; e la prima cifra del numero ruotato non può essere 0
    ; e i due numeri non devono essere uguali
    (if (and out
             (or (= rev "") (= (rev 0) "0")
             (= (int rev 0 10) num))) (setq out nil))
    ;(println (list num rev out))
    out))

Proviamo:

(confuse? 16)
;-> true
(confuse? 69)
;-> nil
(confuse? 126)
;-> nil
(confuse? 1)
;-> nil

(filter confuse? (sequence 1 1000))
;-> (6 9 16 18 19 61 66 68 81 86 89 91 98 99 106 108 109 116
;->  118 119 161 166 168 169 186 188 189 191 196 198 199 601 606
;->  608 611 616 618 661 666 668 669 681 686 688 691 696 698 699
;->  801 806 809 811 816 819 861 866 868 869 881 886 889 891 896
;->  898 899 901 908 909 911 918 919 961 966 968 969 981 988 989
;->  991 996 998 999)


--------------------------------------------------------------------------
Trovare valori e indici degli elementi di una lista diversi da un valore k
--------------------------------------------------------------------------

Data una lista di elementi e un valore k, determinare:
1) tutti i valori della lista diversi da k 
2) tutti gli indici dei valori della lista diversi da k 

(define (find-different lst k indexes)
  (if indexes
      ; calcola gli indici degli elementi di lst che sono diversi da k
      (index (fn(x y) (!= x k)) lst)
      ; else
      ; calcola i valori degli elementi di lst che sono diversi da k
      (filter (fn(x) (!= x k)) lst)))

Proviamo:

(setq a '(1 4 3 2 6 7 8 4 3 5 6))
(find-different a 3)
(find-different a 3 true)

(setq b '(1 1 1 4 1 2 4 3 4 4 5))
(find-different b 1)
;-> (4 2 4 3 4 4 5)
(find-different b 1 true)
;-> (3 5 6 7 8 9 10)

(setq c '(2 2 2))
(find-different c 2)
;-> ()
(find-different c 2 true)
;-> ()


----------------------------------------------------------------------------
Permutazioni con primi negli indici primi e non-primi negli indici non-primi
----------------------------------------------------------------------------

Data una lista di interi da 1 a N, trovare quante permutazioni esistono in cui i numeri primi sono posizionati negli indici primi.
Nota: l'indice inizia con 1.
I numeri primi devono essere posizionati in posizioni prime.
I numeri non primi devono essere posizionati in posizioni non prime.

Esempio:
  Lista = (1 2 3 4 5)
  Numeri primi: 2 3 5
  Indici primi: 2 3 5
  Numeri non primi: 1 4
  Indici non primi: 1 4
Le disposizioni valide collocherebbero (2 3 5) nelle posizioni (2 3 5) e (1 4) nelle posizioni (1 4).

Non dobbiamo calcolare tutte le permutazioni.
Questo è un problema di permutazione vincolato che può essere scomposto in due sottoproblemi indipendenti.
1) I numeri primi possono andare SOLO in posizioni prime
2) I numeri non primi possono andare SOLO in posizioni non prime
I due gruppi sono separati e non interagiscono tra loro.
Se abbiamo K numeri primi nell'intervallo [1, N], allora abbiamo anche esattamente K posizioni prime.
In quanti modi possiamo disporre K numeri primi in K posizioni prime?
  modi = K!
Analogamente, abbiamo (N - K) numeri non primi e (N - K) posizioni non prime.
Il numero di modi per disporli è (N - K)!.
Poiché queste due disposizioni sono indipendenti (scegliere come disporre i numeri primi non influisce su come possiamo disporre i numeri non primi), moltiplichiamo le possibilità: K! * (N - K)!.

Algoritmo
---------
1) Contare quanti numeri primi esistono nell'intervallo [1, N]
2) Calcolare il numero di modi per disporre i numeri primi in posizioni prime: fattoriale(numero_primi)
3) Calcolare il numero di modi per disporre i numeri non primi in posizioni non prime: fattoriale(N - numero_primi)
4) Moltiplicare questi due valori per ottenere il numero totale di permutazioni valide

(define (primes-to-count num)
"Calculate the number of primes less than or equal to a given number"
  (cond ((< num 2) 0)
        ((= num 2) 1)
        (true
         (let (np 1)
         (setq arr (array (+ num 1)))
         (for (x 3 num 2)
              (when (not (arr x))
                (++ np)
                (for (y (* x x) num (* 2 x) (> y num))
                     (setf (arr y) true)))) np))))

(define (fact-i num)
"Calculate the factorial of an integer number"
  (if (zero? num)
      1
      (let (out 1L)
        (for (x 1L num)
          (setq out (* out x))))))

(define (perm-prime N)
  (let (K (primes-to-count N))
    (* (fact-i K) (fact-i (- N K)))))

Proviamo:

(perm-prime 5)
;-> 12L

(perm-prime 100)
;-> 38481979412012894561143749751991768407900856981542463736958373544134
;-> 4644079427866367657953100203395372827017216000000000000000000000000L


----------------------
Matrice delle distanze
----------------------

Date le dimensioni NxM di una matrice e le coordinate di una cella (row col) che appartiene alla matrice, calcolare la matrice delle distanze (cioè una matrice NxM in cui ogni cella contiene la distanza dalla cella (row col).
La 'distanza' può essere calcolata in 3 modi diversi:
1) Distanza cartesiana
2) Distanza di Manhattan (4 direzioni)
3) Distanza di Manhattan (8 direzioni)

(define (dist2d x1 y1 x2 y2)
"Calculate 2D Cartesian distance of two points P1 = (x1 y1) and P2 = (x2 y2)"
  (sqrt (add (mul (sub x1 x2) (sub x1 x2))
             (mul (sub y1 y2) (sub y1 y2)))))

(define (dist-manh4 x1 y1 x2 y2)
"Calculate Manhattan distance (4 directions - rook) of two points P1=(x1 y1) e P2=(x2 y2)"
  (add (abs (sub x1 x2)) (abs (sub y1 y2))))

(define (dist-manh8 x1 y1 x2 y2)
"Calculate Manhattan distance (8 directions - queen) of two points P1=(x1 y1) e P2=(x2 y2)"
  (max (abs (sub x1 x2)) (abs (sub y1 y2))))

(define (print-matrix matrix border)
"Print a matrix m x n"
  (local (row col lenmax fmtstr)
    ; converto matrice in lista?
    (if (array? matrix) (setq matrix  (array-list matrix)))
    ; righe della matrice
    (setq row (length matrix))
    ; colonne della matrice
    (setq col (length (first matrix)))
    ; valore massimo della lunghezza tra gli elementi (come stringa)
    (setq lenmax (apply max (map length (map string (flat matrix)))))
    ; creo stringa di formattazione
    (setq fmtstr (append "%" (string (+ lenmax 1) "s")))
    ; stampa la matrice
    (for (i 0 (- row 1))
      (if border (print "|"))
      (for (j 0 (- col 1))
        (print (format fmtstr (string (matrix i j))))
      )
      (if border (println " |") (println))
    ) nil))

Funzione che genera la matrice delle distanze:

(define (distanza-matrice N M row col dist-func)
    (let (matrix (dup (dup 0 N) M))
      (for (r 0 (- N 1))
        (for (c 0 (- M 1))
          (setf (matrix r c) (dist-func r c row col))))
      matrix))

Proviamo:

(print-matrix (distanza-matrice 10 10 0 0 dist-manh4))
;->  0  1  2  3  4  5  6  7  8  9
;->  1  2  3  4  5  6  7  8  9 10
;->  2  3  4  5  6  7  8  9 10 11
;->  3  4  5  6  7  8  9 10 11 12
;->  4  5  6  7  8  9 10 11 12 13
;->  5  6  7  8  9 10 11 12 13 14
;->  6  7  8  9 10 11 12 13 14 15
;->  7  8  9 10 11 12 13 14 15 16
;->  8  9 10 11 12 13 14 15 16 17
;->  9 10 11 12 13 14 15 16 17 18

(print-matrix (distanza-matrice 10 10 0 0 dist-manh8))
;->  0 1 2 3 4 5 6 7 8 9
;->  1 1 2 3 4 5 6 7 8 9
;->  2 2 2 3 4 5 6 7 8 9
;->  3 3 3 3 4 5 6 7 8 9
;->  4 4 4 4 4 5 6 7 8 9
;->  5 5 5 5 5 5 6 7 8 9
;->  6 6 6 6 6 6 6 7 8 9
;->  7 7 7 7 7 7 7 7 8 9
;->  8 8 8 8 8 8 8 8 8 9
;->  9 9 9 9 9 9 9 9 9 9

(print-matrix
  (map (fn(riga) (map (fn(el) (format "%2.2f" el)) riga))
       (distanza-matrice 10 10 0 0 dist2d)))
;->  0.00  1.00  2.00  3.00  4.00  5.00  6.00  7.00  8.00  9.00
;->  1.00  1.41  2.24  3.16  4.12  5.10  6.08  7.07  8.06  9.06
;->  2.00  2.24  2.83  3.61  4.47  5.39  6.32  7.28  8.25  9.22
;->  3.00  3.16  3.61  4.24  5.00  5.83  6.71  7.62  8.54  9.49
;->  4.00  4.12  4.47  5.00  5.66  6.40  7.21  8.06  8.94  9.85
;->  5.00  5.10  5.39  5.83  6.40  7.07  7.81  8.60  9.43 10.30
;->  6.00  6.08  6.32  6.71  7.21  7.81  8.49  9.22 10.00 10.82
;->  7.00  7.07  7.28  7.62  8.06  8.60  9.22  9.90 10.63 11.40
;->  8.00  8.06  8.25  8.54  8.94  9.43 10.00 10.63 11.31 12.04
;->  9.00  9.06  9.22  9.49  9.85 10.30 10.82 11.40 12.04 12.73

(print-matrix (distanza-matrice 7 7 3 3 dist-manh4))
;->  6 5 4 3 4 5 6
;->  5 4 3 2 3 4 5
;->  4 3 2 1 2 3 4
;->  3 2 1 0 1 2 3
;->  4 3 2 1 2 3 4
;->  5 4 3 2 3 4 5
;->  6 5 4 3 4 5 6

Per matrici grandi conviene utilizzare un vettore a due dimensioni.

(define (distanza-matrice-v N M row col dist-func)
    (let (matrix (array N M '(0)))
      (for (r 0 (- N 1))
        (for (c 0 (- N 1))
          (setf (matrix r c) (dist-func r c row col))))
      matrix))

(print-matrix (distanza-matrice-v 7 7 3 3 dist-manh4))
;->  6 5 4 3 4 5 6
;->  5 4 3 2 3 4 5
;->  4 3 2 1 2 3 4
;->  3 2 1 0 1 2 3
;->  4 3 2 1 2 3 4
;->  5 4 3 2 3 4 5
;->  6 5 4 3 4 5 6

Test di correttezza:

(= (distanza-matrice 100 100 0 0 dist-manh4)
   (array-list (distanza-matrice-v 100 100 0 0 dist-manh4)))
;-> true

Test di velocità:

Con matrici piccole le funzioni sono equivalenti
(time (distanza-matrice 10 10 5 5 dist-manh4) 1e5)
;-> 2369.662
(time (distanza-matrice-v 10 10 5 5 dist-manh4) 1e5)
;-> 2739.854

Con matrici grandi la funzione che usa il vettore è molto più veloce:
(time (distanza-matrice 1000 1000 500 500 dist-manh4))
;-> 3249.45
(time (distanza-matrice-v 1000 1000 500 500 dist-manh4))
;-> 235.916

Vediamo quanto guadagniamo in velocità se scriviamo la funzione di distanza direttamente dentro la funzione principale:

(define (distanza-matrice-manh4 N M row col)
    (let (matrix (array N M '(0)))
      (for (r 0 (- N 1))
        (for (c 0 (- N 1))
          (setf (matrix r c) (add (abs (sub r row)) (abs (sub c col))))))
      matrix))

(= (distanza-matrice-v 100 100 0 0 dist-manh4)
   (distanza-matrice-manh4 100 100 0 0))
;-> true

(time (distanza-matrice-manh4 10 10 5 5 dist-manh4) 1e5)
;-> 2082.424 ;(35% più veloce)
(time (distanza-matrice-manh4 1000 1000 500 500 dist-manh4))
;-> 183.803 ;(22% più veloce)


--------------------------------------------
Contare le cifre dei numeri in un intervallo
--------------------------------------------

Scrivere una funzione che conta le cifre dei numeri nell'intervallo [a, b].
La funzione deve restituire il numero totale di cifre oppure una lista con le occorrenze delle cifre da 0 a 9 dei numeri compresi nell'intervallo.

Esempio:
  Intervallo = [10, 20]
  Numeri = 11 12 13 14 15 16 17 18 19 20
  cifre = 2 11 2 1 1 1 1 1 1 1
  totale numero cifre = 22

Metodo Brute-Force
------------------
Per ogni numero dell'intervallo contiamo le sue cifre e le aggiungiamo alla lista finale.

(define (digits-count a b all)
  (local (conta-cifre num-cifre freq)
    ; vettore delle occorrenze delle cifre
    (setq conta-cifre (array 10 '(0)))
    ; ciclo che, per ogni numero da 'a' a 'b' aggiorna
    ; il vettore delle occorrenze delle cifre
    (for (num a b)
      (while (!= num 0)
        (++ (conta-cifre (% num 10)))
        (setq num (/ num 10))))
    (if all
        ; numero totale di cifre
        (apply + conta-cifre)
        ; lista delle cifre
        conta-cifre)))

Proviamo:

(digits-count 10 20)
;-> (2 11 2 1 1 1 1 1 1 1)
(digits-count 10 20 true)
;-> 22

Il codice è veloce per piccoli intervalli, ma ovviamente non per intervalli maggiori di 1e6.

(time (println (digits-count 1 1e6)))
;-> (488895 600001 600000 600000 600000 600000 600000 600000 600000 600000)
;-> 772.418
> (time (println (digits-count 1 1e7)))
;-> (5888896 7000001 7000000 7000000 7000000 7000000
;->  7000000 7000000 7000000 7000000)
;-> 8747.777

Analizzando i risultati di alcuni intervalli possiamo notare che esiste una regolarità nei valori ottenuti:

(digits-count 1 9)
;-> (0 1 1 1 1 1 1 1 1 1)
(digits-count 10 99)
;-> (9 19 19 19 19 19 19 19 19 19)
(digits-count 100 999)
;-> (180 280 280 280 280 280 280 280 280 280)
(digits-count 1000 9999)
;-> (2700 3700 3700 3700 3700 3700 3700 3700 3700 3700)
(digits-count 10000 99999)
;-> (36000 46000 46000 46000 46000 46000 46000 46000 46000 46000)
(digits-count 100000 999999)
;-> (450000 550000 550000 550000 550000 550000 550000 550000 550000 550000)

Quindi deve esistere un metodo più generale per contare quante volte compare ogni cifra nell'intervallo [a, b].

Metodo veloce
-------------
Per un numero N, si conta posizione per posizione (unità, decine, centinaia, ...).
Ad ogni posizione si vede quante volte ciascuna cifra appare 'ciclicamente'.

Sia N il limite, e consideriamo una certa posizione p (unità = 1, decine = 10, centinaia = 100, ...).
Dividiamo N in tre parti:
  1) alto = N // (p*10)      (parte a sinistra di questa posizione)
  2) corr = (N // p) % 10    (cifra nella posizione corrente)
  3) basso = N % p           (parte a destra di questa posizione)

Allora il conteggio della cifra d in questa posizione è:

                  (alto)*p,               se d = 0
  conteggio(d) =  (alto + 1)*p,           se d < corr
                  (alto)*p + (basso + 1), se d = corr
                  (alto)*p,               se d > corr

Nota: per la cifra '0' bisogna aggiustare, perché non può essere la cifra più significativa (non contiamo leading zeros).
Non si possono togliere gli '0' con un'unica sottrazione per posizione dopo il conteggio: lo zero va trattato posizione per posizione (usando 'alto', 'corr', 'basso') perché i leading-zero non si contano nella posizione più significativa.
La complessità di questo metodo vale O(log10(N)).

Per un l'intervallo [a, b], basta calcolare:

  conteggio(b) - conteggio(a-1)

dove 'conteggio(N)' è la funzione che conta le cifre da '1..N'.

;-----------------------------------------------
; Conta le occorrenze delle cifre da 1 a N
; Restituisce un array di 10 elementi:
; indice = cifra (0..9), valore = occorrenze
;-----------------------------------------------
(define (digit-count-up-to N)
  (if (< N 1) (array 10 '(0))     ; se N < 1 non ci sono cifre
    (local (conta p alto curr basso d)
      ; inizializza array delle occorrenze (0 per ogni cifra)
      (setq conta (array 10 '(0)))
      ; p rappresenta la posizione corrente (unità=1, decine=10, centinaia=100, ...)
      (setq p 1)
      ; ciclo sulle potenze di 10 fino a coprire tutte le cifre di N
      (while (<= p N)
        ; parte "a sinistra" della cifra corrente
        (setq alto (/ N (* p 10)))
        ; cifra nella posizione corrente
        (setq curr (% (/ N p) 10))
        ; parte "a destra" della cifra corrente
        (setq basso (% N p))
        ;-----------------------------------------------
        ; Conta cifre 1..9 per la posizione corrente
        ;-----------------------------------------------
        (for (d 1 9)
          (cond
            ; se la cifra corrente di N è > d,
            ; allora la cifra d ha fatto (alto+1) cicli completi in questa posizione
            ((> curr d)
              (setf (conta d) (+ (conta d) (* (+ alto 1) p))))
            ; se la cifra corrente è uguale a d,
            ; allora la cifra d ha fatto "alto cicli completi" + (basso+1) extra
            ((= curr d)
              (setf (conta d) (+ (conta d) (+ (* alto p) (+ basso 1)))))
            ; se la cifra corrente è < d,
            ; allora la cifra d ha fatto solo "alto cicli completi"
            (true
              (setf (conta d) (+ (conta d) (* alto p))))))
        ;-----------------------------------------------
        ; Caso speciale: cifra 0
        ; Non si contano gli zeri "leading" (più significativi)
        ; quindi la formula è diversa
        ;-----------------------------------------------
        (if (= alto 0)
            nil ; non ci sono più cifre significative
          (if (= curr 0)
              ; se la cifra corrente è 0:
              ; gli zeri compaiono (alto-1) volte completi + (basso+1)
              (setf (conta 0) (+ (conta 0) (+ (* (- alto 1) p) (+ basso 1))))
              ; altrimenti compaiono "alto" volte completi
              (setf (conta 0) (+ (conta 0) (* alto p)))))
        ; passa alla posizione successiva (decine -> centinaia -> migliaia ...)
        (setq p (* p 10)))
      conta))) ; ritorna array delle frequenze

;-----------------------------------------------
; Conta le occorrenze delle cifre nell'intervallo [a,b]
; Calcola differenza: conteggio(1..b) - conteggio(1..a-1)
;-----------------------------------------------
(define (digits-count-fast a b)
  (local (cntA cntB res i)
    (setq cntA (digit-count-up-to (- a 1)))
    (setq cntB (digit-count-up-to b))
    (setq res (array 10 '(0)))
    (for (i 0 9)
      (setf (res i) (- (cntB i) (cntA i))))
    res))

Proviamo:

(digits-count-fast 1 9)
;-> (0 1 1 1 1 1 1 1 1 1)
(digits-count-fast 10 99)
;-> (9 19 19 19 19 19 19 19 19 19)
(digits-count-fast 100 999)
;-> (180 280 280 280 280 280 280 280 280 280)
(digits-count-fast 1000 9999)
;-> (2700 3700 3700 3700 3700 3700 3700 3700 3700 3700)
(digits-count-fast 10000 99999)
;-> (36000 46000 46000 46000 46000 46000 46000 46000 46000 46000)
(digits-count-fast 100000 999999)
;-> (450000 550000 550000 550000 550000 550000 550000 550000 550000 550000)

(digits-count 123 12345)
;-> (4642 8065 5095 4699 4649 4643 4642 4642 4642 4642)
(digits-count-fast 123 12345)
;-> (4642 8065 5095 4699 4649 4643 4642 4642 4642 4642)

Test di correttezza:

(= (digits-count 123 1234567) (digits-count-fast 123 1234567))
;-> true

Test di velocità:

(time (println (digits-count 123 1234567)))
;-> (713284 1058929 758959 718963 713963 713363 713293 713285 713284 713284)
;-> 980.047
(time (println (digits-count-fast 123 1234567)))
;-> (713284 1058929 758959 718963 713963 713363 713293 713285 713284 713284)
;-> 3.989

(time (println (digits-count 123 12345678)))
;-> (8367605 11824361 8824391 8424395 8374395 8368395 8367695 8367615 8367606 8367605)
;-> 11243.901
(time (println (digits-count-fast 123 12345678)))
;-> (8367605 11824361 8824391 8424395 8374395 8368395 8367695 8367615 8367606 8367605)
;-> 6.504

La funzione 'digits-count-fast' è praticamente immediata.

Vedi anche "Somma delle cifre dei numeri da 1 a N" su "Note libere 14".


------------------------------------------------
Numeri espressi come somma di numeri consecutivi
------------------------------------------------

Dato un numero intero N, trovare in quanti modi diversi si può esprimere N come somma di numeri interi positivi consecutivi.

Esempio
  N = 9
  Somma1 = 9
  somma2 = 4 + 5
  somma3 = 2 + 3 + 4

Esprimiamo N come somma di k numeri consecutivi (partendo da 'a'):

  N =  a + (a+1) + (a+2) + ... + (a+k-1)

Utilizzando la formula della serie aritmetica, questa diventa:

  somma = (primo-termine + ultimo-termine) * number-of-terms / 2*n
        = (a + (a+k-1))*k/2*n = (2a + k - 1)*k/2

Moltiplicando entrambi i membri per 2:

  2*n = (2*a + k - 1)*k

  2*n = 2*a*k + k*k - k*2n = 2*a*k + k*(k - 1)*2*a = 2*n/k - k + 1

Quindi affinché 'a' sia un numero intero positivo, devono verificarsi due cose:

1) k deve dividere 2*n esattamente (altrimenti 2*n/k non sarebbe un numero intero)
2) 2*a deve essere un numero pari positivo, il che significa che 2*n/k - k + 1 deve essere pari

Inoltre, poiché 'a' deve essere almeno 1 (abbiamo bisogno di numeri interi positivi), abbiamo:

  2a >= 2*2*n/k - k + 1 >= 2*2n/k >= k + 1*2*n >= k(k + 1)

Questo ci fornisce un limite superiore per k:
dobbiamo solo controllare i valori in cui k*(k + 1) <= 2*n.

Funzione che trova tutte le sequenze di numeri consecutivi che sommano ad un dato numero:
(k --> seq-len, a --> start-num)

(define (sequenze-somma n)
  (local (out doppioN seq-len start-num)
    (setq out '())
    ; Raddoppia il valore di n per la formula matematica
    ; Se abbiamo k numeri consecutivi a partire da 'a', la somma è:
    ; k * a + k * (k - 1) / 2 = n
    ; Riordinando: 2*n = k * (2a + k - 1)
    (setq doppioN (* n 2))
    ;(setq conta 0)
    (setq seq-len 1)
    ; Itera sulle possibili lunghezze k della sequenza
    ; k * (k + 1) è la somma minima per k numeri consecutivi a partire da 1
    (while (<= (* seq-len (+ seq-len 1)) doppioN)
      ; Verifica due condizioni:
      ; 1. doppioN è divisibile per seq-len
      ; 2. Il numero iniziale dovrebbe essere un numero intero positivo
      ; (doppioN // seq-len - seq-len + 1) deve essere pari
      ; per garantire che il numero iniziale sia un numero intero
      (when (and (zero? (% doppioN seq-len))
                 (even? (- (/ doppioN seq-len) seq-len -1)))
            ; calcola 'a' della sequenza corrente
            (setq start-num (/ (- (/ doppioN seq-len) seq-len -1) 2))
            (push (list start-num seq-len) out -1))
      (++ seq-len))
    out))

Proviamo:

(sequenze-somma 9)
;-> ((9 1) (4 2) (2 3))

(sequenze-somma 100)
;-> ((100 1) (18 5) (9 8))

Vedi anche "Numeri somma di numeri consecutivi" su "Note libere 14".
Vedi anche "Somma di numeri consecutivi" su "Note libere 27".


-----------------------------------------
Punto in movimento in un piano cartesiano
-----------------------------------------

In un piano cartesiano infinito, un punto P si trova a (0,0) ed è rivolto inizialmente verso Nord.
La direzione Nord è la direzione positiva dell'asse y.
La direzione Sud è la direzione negativa dell'asse y.
La direzione Est è la direzione positiva dell'asse x.
La direzione Ovest è la direzione negativa dell'asse x.
Il punto può effettuare le seguenti tre istruzioni:
1)  "M": si muove nella direzione corrente di 1 unità.
2)  "S": ruota di 90 gradi a sinistra (ovvero, in senso antiorario).
3)  "D": ruota di 90 gradi a destra (ovvero, in senso orario).
Data una sequenza di istruzioni, il punto le esegue in ordine e le ripete all'infinito.

Nota: la prima istruzione deve essere M, altrimenti occorrebbe impostare la direzione iniziale all'ultima direzione prima della M.
Per esempio, con la lista istruzioni (S D D M), abbiamo:
  Direzione iniziale: Nord
  S: ruota di 90 gradi in senso antiorario. Direzione: Ovest.
  D: ruota di 90 gradi in senso orario. Direzione: Nord.
  D: ruota di 90 gradi in senso orario. Direzione: Est.
  Adesso incontriamo M (il primo movimento) e la direzione iniziale è Est.

Scrivere una funzione che prende una lista di istruzioni e determina se il punto entra in un ciclo infinito.

Esempio 1:
Lista istruzioni = (M M S S M M)
Il punto si trova inizialmente a (0, 0) rivolto verso Nord.
M: muove di un passo. Posizione: (0,1). Direzione: Nord.
M: muove di un passo. Posizione: (0,2). Direzione: Nord.
S: ruota di 90 gradi in senso antiorario. Posizione: (0,2). Direzione: Ovest.
S: ruota di 90 gradi in senso antiorario. Posizione: (0,2). Direzione: Sud.
M: muove di un passo. Posizione: (0,1). Direzione: Sud.
M: muove di un passo. Posizione: (0,0). Direzione: Sud.
Ripetendo le istruzioni, il punto entra nel ciclo:
(0,0) --> (0,1) --> (0,2) --> (0,1) --> (0,0).

Esempio 2:
Lista istruzioni = (M M S D)
Il punto si trova inizialmente a (0, 0) rivolto verso Nord.
M: muove di un passo. Posizione: (1, 0). Direzione: Nord.
M: muove di un passo. Posizione: (2, 0). Direzione: Nord.
D: ruota di 90 gradi in senso orario. Posizione: (2,0). Direzione: Est.
S: ruota di 90 gradi in senso anti-orario. Posizione: (2,0). Direzione: Nord.
Ripetendo le istruzioni, il punto continua ad avanzare in direzione Nord e non entra in cicli.

Esempio 3:
Lista istruzioni = (M D)
Il punto si trova inizialmente a (0,0) rivolto verso Nord.
M: muove di un passo. Posizione: (0,1). Direzione: Nord.
D: ruota di 90 gradi in senso orario. Posizione: (0,1). Direzione: Est.
M: muove di un passo. Posizione: (1,1). Direzione: Est.
D: ruota di 90 gradi in senso orario. Posizione: (1,1). Direzione: Sud.
M: muove di un passo. Posizione: (1,0). Direzione: Sud.
D: ruota di 90 gradi in senso orario. Posizione: (1,0). Direzione: Ovest.
M: muove di un passo. Posizione: (0,0). Direzione: Ovest.
D: ruota di 90 gradi in senso orario. Posizione: (0,0). Direzione: Nord.
Ripetendo le istruzioni, il punto entra nel ciclo:
(0,0) --> (0,1) --> (-1,1) --> (-1,0) --> (0,0).

Dopo aver eseguito un ciclo di istruzioni, possiamo avere due casi:

Caso 1: Il punto ritorna nella posizione iniziale dopo un ciclo
Se dopo un'esecuzione il punto torna a (0, 0), allora non importa in quale direzione sia rivolto, ripetere le istruzioni continuerà a riportarlo all'origine. Il punto traccia un percorso chiuso.

Caso 2: Il punto non ritorna nella posizione iniziale dopo un ciclo
Se il punto cambia direzione dopo un ciclo, i diversi orientamenti causano l'annullamento dei vettori di spostamento (dopo al massimo 4 cicli).
Ma se mantiene la direzione originale, continua ad accumulare spostamenti nella stessa direzione per sempre.

Pertanto possiamo risolvere il problema verificando, dopo un ciclo di istruzioni, le seguenti condizioni:
1) Siamo tornati all'origine?
   Allora il punto entra in un ciclo.
2) Abbiamo cambiato direzione?
   Allora il punto entra in un ciclo.
Se una delle due condizioni è vera, allora il punto entra in un ciclo.

(define (go lst)
  ; Indice di direzione: 0=Nord, 1=Ovest, 2=Sud, 3=Est
  (setq dir 0)
  ; Distanze di spostamento in ogni direzione
  (setq dist-dir '(0 0 0 0))
  ; Ciclo delle istruzioni
  (dolist (el lst)
    (cond ((= el 'S)
            # Gira a Sinistra: ruota di 90 gradi anti-orario
            (setq dir (% (+ dir 1) 4)))
          ((= el 'D)
            # Gira a Destra: ruota di 90 gradi orario
            (setq dir (% (+ dir 3) 4)))
          ((= el 'M)
            # Muove di 1 nella direzione corrente
            (++ (dist-dir dir)))
          (true (println el " : istruzione sconosciuta"))))
  (println dir { } dist-dir)
  ; Verifica se il punto entra in un ciclo
  (or
    ; 1) Ritorna all'origine?
    ; (distanze Nord-Sud uguali e distanze Est-Ovest uguali)
    (and (= (dist-dir 0) (dist-dir 2)) (= (dist-dir 1) (dist-dir 3)))
    ; 2) non è rivolto verso Nord dopo un ciclo
    (!= dir 0)))

Proviamo:

(go '(M M S S M M))
;-> true

(go '(M M S D))
;-> nil

(go '(M D))
;-> true


----------------------------------------------------
Numeri con cifre ripetute e numeri con cifre diverse
----------------------------------------------------

A) Dato un numero intero N, determinare la lista dei numeri interi positivi nell'intervallo [a, b] che hanno almeno una cifra ripetuta.

Esempio 1:
N = 20
Lista = (11)

Esempio 2:
N = 100
Lista = (11 22 33 44 55 66 77 88 99 100)

Sequenza OEIS A109303:
Numbers k with at least one duplicate base-10 digit.
  11, 22, 33, 44, 55, 66, 77, 88, 99, 100, 101, 110, 111, 112, 113, 114,
  115, 116, 117, 118, 119, 121, 122, 131, 133, 141, 144, 151, 155, 161,
  166, 171, 177, 181, 188, 191, 199, 200, 202, 211, 212, 220, 221, 222,
  223, 224, 225, 226, 227, 228, 229, 232, 233, 242, ...
All numbers greater than 9876543210 (last term of A010784) are terms.

Funzione che verifica se un numero intero ha almeno una cifra doppia:

(define (cifra-doppia? num)
  (letn ((cifre-viste (array 10 '(nil)))
         (doppio nil))
    (while (and (> num 0) (not doppio))
      (let (cifra (% num 10))
        (if (cifre-viste cifra)
            (setq doppio true)
            (setf (cifre-viste cifra) true)))
      (setq num (/ num 10)))
    doppio))

Proviamo:

(cifra-doppia? 11)
;-> true
(cifra-doppia? 1234)
;-> nil
(cifra-doppia? 1234567891)
;-> true

(filter cifra-doppia? (sequence 1 100))
;-> (11 22 33 44 55 66 77 88 99 100)

(filter cifra-doppia? (sequence 1 242))
;-> (11 22 33 44 55 66 77 88 99 100 101 110 111 112 113 114
;->  115 116 117 118 119 121 122 131 133 141 144 151 155 161
;->  166 171 177 181 188 191 199 200 202 211 212 220 221 222
;->  223 224 225 226 227 228 229 232 233 242)

(length (filter cifra-doppia? (sequence 1 1000)))
;-> 262
(length (filter cifra-doppia? (sequence 1001 2000)))
;-> 496

(time (println (length (filter cifra-doppia? (sequence 1 1e6)))))
;-> 831430
;-> 1665.269
I numeri che non hanno almeno una cifra doppia sono:
(- 1e6 831430)
;-> 168570

(time (println (length (filter cifra-doppia? (sequence 1 1e7)))))
;-> 9287110
;-> 16911.667
I numeri che non hanno almeno una cifra doppia sono:
(- 1e7 9287110)
;-> 712890

B) Dato un numero intero N, determinare la lista dei numeri interi positivi nell'intervallo [a, b] che hanno tutte cifre diverse.

Sequenza OEIS A010784:
Numbers with distinct decimal digits.
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20,
  21, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 37, 38, 39, 40,
  41, 42, 43, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 56, 57, 58, 59, 60,
  61, 62, 63, 64, 65, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 78, 79, 80,
  81, 82, 83, 84, 85, 86, 87, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 102,
  103, 104, 105, 106, 107, 108, 109, 120, ...
This sequence is finite: a(8877691) = 9876543210 is the last term.

Funzione che verifica se un numero intero ha tutte cifre diverse:

(define (cifre-diverse? num)
  (letn ((cifre-viste (array 10 '(nil)))
         (diverse true))
    (while (and (> num 0) diverse)
      (let (cifra (% num 10))
        (if (cifre-viste cifra)
            (setq diverse nil)
            (setf (cifre-viste cifra) true)))
      (setq num (/ num 10)))
    diverse))

Proviamo:

(cifre-diverse? 1234)
;-> true
(cifre-diverse? 12341)
;-> nil

(filter cifre-diverse? (sequence 1 120))
;-> (1 2 3 4 5 6 7 8 9 10 12 13 14 15 16 17 18 19 20
;->  21 23 24 25 26 27 28 29 30 31 32 34 35 36 37 38 39 40
;->  41 42 43 45 46 47 48 49 50 51 52 53 54 56 57 58 59 60
;->  61 62 63 64 65 67 68 69 70 71 72 73 74 75 76 78 79 80
;->  81 82 83 84 85 86 87 89 90 91 92 93 94 95 96 97 98 102
;->  103 104 105 106 107 108 109 120)

(length (filter cifre-diverse? (sequence 1 1000)))
;-> 738
(length (filter cifre-diverse? (sequence 1001 2000)))
;-> 504

(time (println (length (filter cifre-diverse? (sequence 1 1e6)))))
;-> 168570
;-> 1581.958
(time (println (length (filter cifre-diverse? (sequence 1 1e7)))))
;-> 712890
;-> 15976.968

Nell'intervallo [1,N] esistono:
- x numeri con almeno una cifra doppia
- y = (N - x) numeri con tutte cifre diverse (cioè numeri con nessuna cifra doppia)

============================================================================

