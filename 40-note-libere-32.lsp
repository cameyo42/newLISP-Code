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
Struttura dati Find-Union (Disjoint Set Union)
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
  (if (!= (parent v) v)
      (setf (parent v) (find-set (parent v))))
  (parent v))

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
;-> (0 0 2 0 6 2 6 7 2 6)
(= (find-set 6) (find-set 2))
;-> nil
parent
(0 0 2 0 6 2 6 7 2 6)

Queste funzioni hanno una complessità temporale di O(N).
Possiamo migliorare il comportamento con alcune modifiche.

...CONTINUA...

============================================================================

