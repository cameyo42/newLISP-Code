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

============================================================================

