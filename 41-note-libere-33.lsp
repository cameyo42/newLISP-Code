================

 NOTE LIBERE 33

================

    "What are you doing on your free time? ... no, wait, don't answer."

-------------
RNA in codoni
-------------

https://codegolf.stackexchange.com/questions/69513/parse-rna-into-codons

Nota:
Tutto il contenuto dei siti di Stack Exchange è rilasciato sotto la licenza CC BY-SA 4.0 (Creative Commons Attribution-ShareAlike 4.0).

L'RNA è il cugino meno famoso del DNA.
Il suo scopo principale è controllare la produzione di proteine nelle cellule attraverso un processo chiamato "traduzione".
Considereremo l'RNA come una lunga stringa che attraversa l'alfabeto di coppie di basi, AUCG.
Nella traduzione, l'RNA viene suddiviso in frammenti non sovrapposti di tre coppie di basi, chiamati codoni.
Il processo inizia con un codone di inizio, AUG, e termina con un codone di stop, tra UAA, UAG o UGA.
Ogni codone (eccetto i codoni di stop) corrisponde a un amminoacido e la stringa di amminoacidi risultante forma la proteina.
L'output è la lista dei codoni in cui l'RNA è suddiviso.
In questo modello semplificato, il processo inizia con il codone di inizio più a sinistra, AUG, che è incluso nell'output.
Termina quando si incontra un codone di stop o quando si esaurisce l'RNA.
Se l'input non contiene alcun codone di inizio, l'output sarà un elenco vuoto.

Esempio:

Consideriamo la sequenza di input:

  ACAUGGAUGGACUGUAACCCCAUGC

L'analisi inizia dall'occorrenza più a sinistra di AUG, all'indice 2. Prosegue come segue:

  AC AUG GAU GGA CUG UAA CCCCAUGC
     *   ^   ^   ^   +

Il codone contrassegnato con * è il codone di inizio, e quelli contrassegnati con ^ fanno anche parte dell'output.
Il codone di stop è contrassegnato con +.
L'output corretto è:

  AUG,GAU,GGA,CUG

Per l'input più breve:

ACAUGGAUGGACUGU

il processo prosegue così:

AC AUG GAU GGA CUG U
   *   ^   ^   ^

Questa volta, non viene incontrato un codone di stop, quindi il processo si interrompe quando esauriamo le coppie di basi.
L'output è lo stesso del precedente.

Casi di test:

  GGUACGGAUU ->
  GGCGAAAUCGAUGCC -> AUG
  ACAUGGAUGGACUGU -> AUG,GAU,GGA,CUG
  AUGACGUGAUGCUUGA -> AUG,ACG
  UGGUUAGAAUAAUGAGCUAG -> AUG,AGC
  ACAUGGAUGGACUGUAACCCCAUGC -> AUG,GAU,GGA,CUG
  CUAAGAUGGCAUGAGUAAUGAAUGGAG -> AUG,GCA
  AAUGGUUUAAUAAAUGUGAUAUGAUGAUA -> AUG,GUU
  UGUCACCAUGUAAGGCAUGCCCAAAAUCAG -> AUG
  UAUAGAUGGUGAUGAUGCCAUGAGAUGCAUGUUAAU -> AUG,GUG,AUG,AUG,CCA
  AUGCUUAUGAAUGGCAUGUACUAAUAGACUCACUUAAGCGGUGAUGAA -> AUG,CUU,AUG,AAU,GGC,AUG,UAC
  UGAUAGAUGUAUGGAUGGGAUGCUCAUAGCUAUAAAUGUUAAAGUUAGUCUAAUGAUGAGUAGCCGAUGGCCUAUGAUGCUGAC -> AUG,UAU,GGA,UGG,GAU,GCU,CAU,AGC,UAU,AAA,UGU

(define (rna-codoni str)
  (local (start tri stop)
    ;trova il primo "AUG"
    (setq start (find "AUG" str))
    (if start ; se esiste "AUG"...
      (begin
        ; elimina i caratteri precedenti al primo "AUG"
        (setq str (slice str start))
        ; converte la stringa in una lista con elementi da 3 caratteri ognuno
        ; e rimuove l'ultimo elemento se non è lungo 3
        (setq tri (explode str 3 true))
        ; trova l'indice del primo elemento "UAA" o "UAG" o "UGA" nella lista
        (setq stop (find '("UAA" "UAG" "UGA") tri (fn (x y) (member y x))))
        (if stop
            ; prende gli elementi della lista da "AUG" all'indice 'stop'
            (slice tri 0 stop)
            ; altrimenti prende tutta la lista
            tri))
      nil)))

Proviamo:

(rna-codoni "GGUACGGAUU")
;-> nil
(rna-codoni "GGCGAAAUCGAUGCC")
;-> ("AUG")
(rna-codoni "ACAUGGAUGGACUGU")
;-> ("AUG" "GAU" "GGA" "CUG")
(rna-codoni "AUGACGUGAUGCUUGA")
;-> ("AUG" "ACG")
(rna-codoni "UGGUUAGAAUAAUGAGCUAG")
;-> ("AUG" "AGC")
(rna-codoni "ACAUGGAUGGACUGUAACCCCAUGC")
;-> ("AUG" "GAU" "GGA" "CUG")
(rna-codoni "CUAAGAUGGCAUGAGUAAUGAAUGGAG")
;-> ("AUG" "GCA")
(rna-codoni "AAUGGUUUAAUAAAUGUGAUAUGAUGAUA")
;-> ("AUG" "GUU")
(rna-codoni "UGUCACCAUGUAAGGCAUGCCCAAAAUCAG")
;-> ("AUG")
(rna-codoni "UAUAGAUGGUGAUGAUGCCAUGAGAUGCAUGUUAAU")
;-> ("AUG" "GUG" "AUG" "AUG" "CCA")
(rna-codoni "AUGCUUAUGAAUGGCAUGUACUAAUAGACUCACUUAAGCGGUGAUGAA")
;-> ("AUG" "CUU" "AUG" "AAU" "GGC" "AUG" "UAC")
(rna-codoni "UGAUAGAUGUAUGGAUGGGAUGCUCAUAGCUAUAAAUGUUAAAGUUAGUCUAAUGAUGAGUAGCCGAUGGCCUAUGAUGCUGAC")
;-> ("AUG" "UAU" "GGA" "UGG" "GAU" "GCU" "CAU" "AGC" "UAU" "AAA" "UGU")


(define (f s)
  (local (i t e)
    (setq i (find "AUG" s))
    (if i
      (begin
        (setq t (explode (slice s i) 3 true))
        (setq e (find '("UAA" "UAG" "UGA") t (fn (x y) (member y x))))
        (if e
            (slice t 0 e)
            t))
      nil)))

Versione code-golf (160 caratteri, one-line):

(define(f s)
(setq i(find "AUG" s))
(if i(begin(setq t(explode(slice s i)3 true))(setq e(find '("UAA""UAG""UGA")t(fn(x y)(member y x))))
(if e(slice t 0 e)t))nil))

(f "AUGCUUAUGAAUGGCAUGUACUAAUAGACUCACUUAAGCGGUGAUGAA")
;-> ("AUG" "CUU" "AUG" "AAU" "GGC" "AUG" "UAC")


--------------
Griglia di led
--------------

Abbiamo una griglia MxN di led.
All'inizio i led sono tutti spenti (0 = spento, 1 = acceso).
Premendo il led di una cella questo e tutti i led della stessa riga, della stessa colonna e delle diagonali cambiano il suo stato (da 0 a 1 oppure da 1 a 0).
Scegliendo a caso un led per K volte (es. K=100), quanti led, in media, rimangono accesi?

1) Simulazione del processo
---------------------------

Funzione che stampa la griglia:

(define (print-grid matrix)
  (local (row col)
    (setq row (length matrix))
    (setq col (length (matrix 0)))
    (for (i 0 (- row 1))
      (for (j 0 (- col 1))
        (print (format "%2d " (matrix i j))))
      (println))))

Funzione che seleziona casualmente un led e modifica i led relativi:

(define (switch)
  (local (row col tmp c)
    ; generazione casuale della cella di partenza (row col)
    (setq row (rand M))
    (setq col (rand N))
    ;(println row { } col)
    ; memorizza il valore della cella di partenza
    ; (perchè cambia di valore dopo ogni inversione)
    (setq tmp (grid row col))
    ; inverte gli elementi delle colonne
    (for (c 0 (- N 1)) (setf (grid row c) (- 1 (grid row c))))
    ; inverte gli elementi delle righe
    (for (r 0 (- M 1)) (setf (grid r col) (- 1 (grid r col))))
    ; inverte gli elementi della diagonale principale,
    ; cioè gli elementi per cui r - c è costante
    (for (r 0 (- M 1))
      (setq c (+ (- r row) col))
      (when (and (>= c 0) (< c N))
        (setf (grid r c) (- 1 (grid r c)))))
    ; inverte gli elementi della diagonale secondaria
    ; cioè gli elementi per cui r + c è costante
    (for (r 0 (- M 1))
      (setq c (- (+ row col) r))
      (when (and (>= c 0) (< c N))
        (setf (grid r c) (- 1 (grid r c)))))
    ; imposta il valore della cella di partenza (row col)
    (setq (grid row col) (- 1 tmp))))

Funzione che simula il processo:

(define (simula M N K show)
  (let (grid (array M N '(0)))
    ; Selezione casuale di K led ...
    (for (i 1 K) (switch))
    (if show (print-grid grid))
    ; conteggio dei led accesi
    (first (count '(1) (flat (array-list grid))))))

(simula 10 10 100 true)
;->  0  0  0  0  0  0  0  1  1  0
;->  0  0  1  1  1  0  0  1  1  0
;->  1  0  0  1  1  0  0  1  1  1
;->  1  0  0  1  1  0  0  1  1  0
;->  1  1  1  1  0  0  1  1  0  1
;->  0  1  1  0  1  0  1  1  0  1
;->  0  1  0  1  0  1  1  0  1  1
;->  1  0  1  1  1  0  0  1  1  1
;->  1  0  1  0  1  1  0  1  0  0
;->  1  1  1  0  1  1  0  1  0  1
;-> 56

(simula 5 5 1000)
;-> 14
(simula 10 1 1)
;-> 10
(simula 10 1 2)
;-> 0

Funzione che calcola la media attesa (utilizzando un dato numero di iterazioni):

(define (media-led M N K iterazioni)
  (let (led 0)
    (for (i 1 iterazioni) (++ led (simula M N K)))
    (div led iterazioni)))

Proviamo:

(time (println (media-led 10 10 100 1000)))
;-> 49.944
;-> 562.609

(time (println (media-led 6 6 100 10000)))
;-> 17.9974
;-> 3641.22

(time (println (media-led 6 6 10 10000)))
;-> 18.0388
;-> 452.996

(time (println (media-led 7 8 100 10000)))
;-> 28.0186
;-> 4250.533

(time (println (media-led 7 8 10 10000)))
;-> 28.0207
;-> 484.289

Notiamo che la media attesa tende al valore M*N/2.

2) Ragionamento matematico
--------------------------

Ogni cella (i,j) viene "toccata" ogni volta che si preme un led nella stessa riga, colonna o diagonale.
Il numero di tali led è S(i,j).
La probabilità di una cella di essere "toccata" è p(i,j) = S(i,j) / (M·N).
Dopo K pressioni indipendenti e uniformi, la probabilità che la cella sia accesa è:

  P(on) = 1/2 * [1 - (1 - 2*p)^K]

La media attesa complessiva del numero di led accesi è la somma di tutte queste probabilità:

  E = Sum[P(on)(i,j)]

Per K grandi, (1 - 2*p)^K -> 0, quindi P(on) -> 1/2 e il valore medio tende a M*N/2.

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

;------------------------------------------------------------
; led-accesi: calcola il numero medio di LED accesi
; dopo K pressioni casuali in una griglia MxN di LED.
;
; Ogni pressione su una cella (i,j):
; - cambia lo stato (toggle) del LED in (i,j)
; - cambia tutti i LED della stessa riga, della stessa colonna
; - cambia tutti i LED delle due diagonali che passano per (i,j)
;
; Tutti i LED iniziano spenti (0).
; Dopo K pressioni casuali, si vuole sapere quanti LED,
; in media, risultano accesi (valore atteso).
;------------------------------------------------------------
(define (led-accesi M N K show)
  (letn (total (mul M N)  ; numero totale di celle = M*N
         approx 0         ; accumulatore per il valore atteso approssimato
         A 0)             ; somma esatta (usata solo per K piccoli)
    ; scorre tutte le celle (i,j) della matrice
    (for (i 0 (- M 1))
      (for (j 0 (- N 1))
        ;----------------------------------------------------
        ; Calcolo della lunghezza delle due diagonali
        ; passanti per la cella (i,j)
        ; diagonale principale: r - c = costante
        ; diagonale secondaria: r + c = costante
        ; Ogni diagonale contiene 1 + min(...) + min(...) elementi
        ;----------------------------------------------------
        (letn ( (d1 (add 1 (min i j) (min (sub M 1 i) (sub N 1 j))))
                (d2 (add 1 (min i (sub N 1 j)) (min (sub M 1 i) j)))
                 ;------------------------------------------------
                 ; Numero totale di celle che cambiano stato
                 ; quando si preme (i,j):
                 ;   tutte le celle della riga + colonna + diagonali
                 ; meno le sovrapposizioni (la cella stessa contata 3 volte)
                 ;------------------------------------------------
                 (S (sub (add N M d1 d2) 3))
                 ;------------------------------------------------
                 ; Probabilità che una singola pressione tocchi la cella (i,j)
                 ;------------------------------------------------
                 (p (div S total))
                 ;------------------------------------------------
                 ; Dopo K pressioni, la probabilità che la cella sia accesa
                 ; è P(on) = (1 - (1 - 2p)^K)/2
                 ; (formula derivata dal fatto che la cella cambia stato
                 ;  ogni volta che viene "toccata", quindi serve la probabilità
                 ;  che il numero di toggle sia dispari)
                 ;------------------------------------------------
                 (t (sub 1 (mul 2 p)))
                 (prob (div (sub 1 (pow t K)) 2)) )
          ; aggiorna il valore atteso approssimato
          (setq approx (add approx prob)))))
    ; Stampa dei risultati
    (when show
      (println "M = " M " N = " N " K = " K)
      (println "Media led accesi:" approx))
    approx))

Proviamo:

(led-accesi 10 10 100)
;-> 50

(led-accesi 7 8 100 true)
;-> M = 7 N = 8 K = 100
;-> Media led accesi:28
;-> 28

(led-accesi 5 5 100)
;-> 12.5

(led-accesi 10 1 1)
;-> 10

(led-accesi 10 1 2)
;-> 0

3. Probabilità dei led
----------------------

Qual è la probabilità di ogni led?
Cioè, quale probabilità ha ogni led di essere acceso/spento dopo una pressione casuale?

Possiamo calcolare quanti led "vede" ogni led, cioè per una cella (r c) possiamo sommare i led della riga, della colonna e delle diagonali.
Questa somma rappresenta il numero di led che, se selezionati, modificano lo stato del led della cella (r c).
Per calcolare la probabilità del led basta dividere questa somma per il totale dei led della griglia.

Calcolo della somma della riga, della colonna e delle diagonali per un led:

1. Diagonale principale (dall’alto a sinistra verso il basso a destra)
Questa diagonale è composta da tutte le celle (r, c) tali che:

  r - c = i - j

Lunghezza = L1 = 1 + min(i, j) + min(M - 1 - i, N - 1 - j)

- min(i, j) = quanti passi possiamo fare indietro prima di uscire dai limiti.
- min(M-1-i, N-1-j) = quanti passi possiamo fare avanti.
- +1 = include la cella centrale stessa.

2. Diagonale secondaria (dall’alto a destra verso il basso a sinistra)

Questa è formata da celle (r, c) tali che:

  r + c = i + j

Lunghezza = L2 = 1 + min(i, N - 1 - j) + min(M - 1 - i, j)

- min(i, N-1-j) = passi indietro (da basso a alto).
- min(M-1-i, j) = passi avanti (da alto a basso️).
- +1 = include la cella centrale stessa.

(define (diagonali M N i j)
  (letn (L1 (+ 1 (min i j) (min (- M 1 i) (- N 1 j)))
         L2 (+ 1 (min i (- N 1 j)) (min (- M 1 i) j)))
    (+ L1 L2)))

Funzione che calcola la probabilità dei led:

(define (prob-led grid)
  (let ( (rows (length grid))
         (cols (length (grid 0)))
         (somme '())
         (prob '()) )
    ; ciclo per ogni led
    (for (r 0 (- rows 1))
      (for (c 0 (- cols 1))
        ; calcolo della somma per il led corrente
        ; -3 perchè la cella corrente viene conteggiata 4 volte
        (setf (grid r c) (+ (diagonali rows cols r c) rows cols (- 3)))))
    ; lista di tutte le somme uniche
    (setq somme (sort (unique (flat (array-list grid)))))
    ; calcolo della probabilità dei led
    (setq prob (map list somme (map (fn(x) (div x (* rows cols))) somme)))
    (print-grid grid)
    (println prob) '>))

Proviamo:

(setq g (array 2 2 '(0)))
(prob-led g)
;->  4  4
;->  4  4
;-> ((4 1))

(setq g (array 3 3 '(0)))
(prob-led g)
;->  7  7  7
;->  7  9  7
;->  7  7  7
;-> ((7 0.7777777777777778) (9 1))

(setq g (array 5 7 '(0)))
(prob-led g)
;-> 15 16 17 17 17 16 15
;-> 15 17 18 19 18 17 15
;-> 15 17 19 19 19 17 15
;-> 15 17 18 19 18 17 15
;-> 15 16 17 17 17 16 15
;-> ((15 0.4285714285714286) (16 0.4571428571428571) (17 0.4857142857142857)
;->  (18 0.5142857142857142) (19 0.5428571428571428))

(setq g (array 10 10 '(0)))
(prob-led g)
;-> 28 28 28 28 28 28 28 28 28 28
;-> 28 30 30 30 30 30 30 30 30 28
;-> 28 30 32 32 32 32 32 32 30 28
;-> 28 30 32 34 34 34 34 32 30 28
;-> 28 30 32 34 36 36 34 32 30 28
;-> 28 30 32 34 36 36 34 32 30 28
;-> 28 30 32 34 34 34 34 32 30 28
;-> 28 30 32 32 32 32 32 32 30 28
;-> 28 30 30 30 30 30 30 30 30 28
;-> 28 28 28 28 28 28 28 28 28 28

(setq g (array 10 4 '(0)))
(prob-led g)
;-> 16 16 16 16
;-> 17 18 18 17
;-> 18 19 19 18
;-> 19 19 19 19
;-> 19 19 19 19
;-> 19 19 19 19
;-> 19 19 19 19
;-> 18 19 19 18
;-> 17 18 18 17
;-> 16 16 16 16
;-> ((28 0.28) (30 0.3) (32 0.32) (34 0.34) (36 0.36))


------------------------------------
Liste che tornano uguali a se stesse
------------------------------------

Data una lista di elementi, applicare le seguenti operazioni:
1) invertire la lista
2) raggruppare gli elementi della lista in coppie invertite
3) ripetere i passi 1) e 2) fino a che non si ottiene nuovamente la lista di partenza.

Scrivere una funzione che conta i passi necessari per ottenere la lista di partenza.

Esempio:
Lista = (1 2 3 4 5)
Passo 1:
  Lista = (1 2 3 4 5)
  Invertire la lista --> (5 4 3 2 1)
  Raggruppare le coppie invertite: ((4 5) (2 3) (1))
  Nel caso gli elementi siano in numero dispari, allora l'ultimo elemento non è una coppia.
Passo 2:
Lista = (4 5 2 3 1)
  Invertire la lista --> (1 3 2 5 4)
  Raggruppare le coppie invertite: ((3 1) (5 2) (4))
Passo 3:
Lista = (3 1 5 2 4)
  Invertire la lista --> (4 2 5 1 3)
  Raggruppare le coppie invertite: ((2 4) (1 5) (3))
Passo 4:
Lista = (2 4 1 5 3)
  Invertire la lista --> (3 5 1 4 2)
  Raggruppare le coppie invertite: ((5 3) (4 1) (2))
Passo 5:
Lista = (5 3 4 1 2)
  Invertire la lista --> (2 1 4 3 5)
  Raggruppare le coppie invertite: ((1 2) (3 4) (5))
  Adesso la lista è uguale a quella di partenza: (1 2 3 4 5) --> stop.

Funzione che calcola il numero di passi necessari per completare le operazioni:

(define (ciclo lst show)
  (local (passi stop lista coppie tmp)
    ; numero di passi
    (setq passi 0)
    ; variabile booleana per il controllo di fine operazioni
    (setq stop nil)
    ; lista iniziale
    (setq lista lst)
    (when show
      (println "Passo: " passi)
      (println lista))
    (until stop
      ; inverte la lista
      (reverse lista)
      ; genera la lista delle coppie contigue dalla lista corrente
      (setq coppie (explode lista 2))
      ; la nuova lista è inizialmente vuota
      (setq lista '())
      ; controllo sull'ultimo valore della lista coppie (se è una coppia o no)
      (setq tmp nil)
      (if (= (length (e -1)) 1)
        (setq tmp (first (pop coppie -1))))
      ; costruzione nuova lista (inserisce le coppie invertite)
      (dolist (el coppie)
        (extend lista (reverse el)))
      ; aggiunge la non-coppia (se necessario)
      (if tmp (push tmp lista -1))
      ; incrementa numero di passi
      (++ passi)
      (when show
        (println "Passo: " passi)
        (println lista))
      ; controllo fine operazioni
      (if (= lista lst) (setq stop true)))
    passi))

Proviamo:

(ciclo '(1 2 3 4 5) true)
;-> Passo: 0
;-> (1 2 3 4 5)
;-> Passo: 1
;-> (4 5 2 3 1)
;-> Passo: 2
;-> (3 1 5 2 4)
;-> Passo: 3
;-> (2 4 1 5 3)
;-> Passo: 4
;-> (5 3 4 1 2)
;-> Passo: 5
;-> (1 2 3 4 5)
;-> 5

Per le liste con un numero pari di elementi i passi necessari sono sempre 2:

(ciclo '(1 2 3 4) true)
;-> Passo: 0
;-> (1 2 3 4)
;-> Passo: 1
;-> (3 4 1 2)
;-> Passo: 2
;-> (1 2 3 4)
;-> 2

(ciclo '(1 2 3 4 5 6 7 8 9 10) true)
;-> Passo: 0
;-> (1 2 3 4 5 6 7 8 9 10)
;-> Passo: 1
;-> (9 10 7 8 5 6 3 4 1 2)
;-> Passo: 2
;-> (1 2 3 4 5 6 7 8 9 10)
;-> 2

(ciclo (sequence 1 1000))
;-> 2

Per le liste con un numero dispari di elementi i passi necessari sono il numero di elementi:

(ciclo (sequence 1 3))
;-> 3
(ciclo (sequence 1 11))
;-> 11
(ciclo (sequence 1 1001))
;-> 1001

Per dimostrare che il periodo è n quando n è dispari, seguiamo dove va l'elemento 1: segue il percorso 1, n, 2, n-2, 4, n-4, 6, n-6, 8, n-8, ... quindi un elemento in posizione pari x si sposta in n-x dopo un passo, e un elemento in posizione dispari x si sposta in n-x+2.
Quindi se n=2k+1, allora dopo il 2k-esimo passo 1 sarà a 2k, e al passo successivo a n-2k = 1.

Nota:
Se gli elementi della lista sono tutti uguali, allora il numero di operazioni vale sempre 1.
Se la lista contiene meno di tre elementi, allora il numero di operazioni vale sempre 1.

(ciclo '(5 5 5 5 5))
;-> 1
(ciclo '(4 4 4 4))
;-> 1

Test di correttezza:

(define (test prove)
  (for (i 1 prove)
    (letn ( (n (rand 1000))
            (lst (rand 1000 n))
            (res (ciclo lst)) )
    (when (or (and (even? n) (!= res 2))
              (and (odd? n) (!= res n)))
      (if (> (length lst) 2)
        (println n { } res { } lst))))))

(time (println (test 1000)))
;-> nil
;-> 8674.452

Quindi possiamo scrivere la seguente funzione:

(define (cicli lst)
  (let (len (length lst))
    (if (or (< len 3) (apply = lst))
        1
        (if (odd? len) len 2))))

Proviamo:

(cicli '(1 2 3 4 5 6 7 8 9 10 11))
;-> 11
(cicli '(1 2 3 4 5))
;-> 5
(cicli '(1 2 3 4))
;-> 2
(cicli '(1 2))
;-> 1
(cicli '())
;-> 1
(cicli '(3 3 3))
;-> 1
(cicli '(2 2))
;-> 1

Versione code-golf (73 caratteri):

(define(c l)(let(n(length l))(if(or(< n 3)(apply = l))1(if(odd? n)n 2))))


--------------------------------
Ricostruzione di numeri mancanti
--------------------------------

Abbiamo una lista di numeri con N+M numeri.
Conosciamo la media di tutti i numeri e i valori di M numeri.
Calcolare il valore degli N numeri mancanti.

Conoscendo la media di tutti i numeri e quanti sono i numeri possiamo calcolare la somma di tutti i numeri:

  somma = media * (N + M)

Adesso possiamo calcolare la somma degli N numeri:

  sommaN = somma - sommaM

A questo punto dobbiamo generare N numeri casuali che sommano a sommaN.
(Naturalmente esistono infinite soluzioni).

Funzione che genera N numeri float che sommano a K:

(define (N-sommano-K N K)
  (letn (
          ; Genera una lista di N numeri float casuali tra 0 e 1
          (nums (random 0 1 N))
          ; Calcola la somma dei numeri generati
          (somma (apply add nums))
          ; Calcola il fattore di scala in modo che
          ; la somma diventi esattamente K
          (fattore (div K somma))
        )
    ; Moltiplica ogni numero per il fattore di scala,
    ; in modo che la somma finale sia K
    (map (fn (x) (mul x fattore)) nums)))

Funzione che calcola N numeri in modo che sommati con M numeri dati producano una media data:

(define (genera-N N lstM media)
  (local (M numero-elementi somma sommaM sommaN)
    ; numero di elementi M
    (setq M (length lstM))
    ; numero totale di elementi
    (setq numero-elementi (+ M N))
    ; somma di tutti gli elementi
    (setq somma (mul media numero-elementi))
    ; somma degli M elementi conosciuti
    (setq sommaM (apply add lstM))
    ; somma degli N elementi sconosciuti
    (setq sommaN (sub somma sommaM))
    ; calcolo degli N elementi sconosciuti
    ; (esistono infinite soluzioni)
    (setq lstN (N-sommano-K N sommaN))
    (setq media-calcolata (div (apply add (append lstN lstM)) (+ M N)))
    (println media { } media-calcolata)
    lstN))

Proviamo:

(genera-N 5 '(1.3 4.3 5.3 6.445) 3)
;-> 3 3
;-> (1.425365393402948 1.591709766953814 4.498324419923165
;->  0.1271276253659858 2.012472794354088)

(genera-N 5 '(1.3 4.3 5.3 6.445) -2)
;-> -2 -2
;-> (-1.866867527540419 -13.64935434468596 -1.133037886566541
;->  -0.1771525034371139 -18.51858773776997)


--------------------------
Coppie uguali in una lista
--------------------------

Abbiamo una lista i cui elementi sono coppie di cifre (0..9).
Due coppie sono uguali se risulta:
1) (x y) == (x y)
oppure
2) (x y) == (y x), perchè la coppia può essere invertita (tessera del domino)

Determinare le coppie uguali e i relativi numeri di occorrenze.

Esempio:
lista = ((1 3) (2 4) (0 0) (3 1) (2 4) (3 2) (4 2))
coppie uguali: (0 0) 1  --> (0 0)
               (3 2) 1  --> (3 2)
               (1 3) 2  --> (1 3) (3 1)
               (2 4) 3  --> (2 4) (2 4) (4 2)

Codifichiamo le coppie in modo che le codifiche di (x y) e (y x) siano identiche.
Meccanismo di codifica:
1) se x == 0 e y == 0, codice = 0
2) se x == 0 e y != 0, codice = y
3) se x != 0 e y == 0, codice = x
4) se x > y,  codice = (10 * y) + x
5) se x <= y, codice = (10 * x) + y

Esempi:
  x = 2, y = 4  -->  24
  x = 4, y = 2  -->  24
  x = 0, y = 0  -->  0
  x = 0, y = 3  -->  3
  x = 2, y = 0  -->  2
  x = 6, y = 5  -->  56

(define (codifica coppia)
  (let ( (x (coppia 0)) (y (coppia 1)) )
    (cond ((and (zero? x) (zero? y)) 0)
          ((zero? x) y)
          ((zero? y) x)
          (true
            (if (> x y) (+ (* 10 y) x) (+ (* 10 x) y))))))

(codifica '(2 4))
;-> 24
(codifica '(4 2))
;-> 24
(codifica '(0 0))
;-> 0
(codifica '(0 3))
;-> 3
(codifica '(2 0))
;-> 2
(codifica '(6 5))
;-> 56

Dopo la codifica possiamo calcolare le occorrenze di tutte le coppie uniche.

(define (coppie-uniche lst)
  (local (codici unici)
    ; codifica delle coppie
    (dolist (el lst) (push (codifica el) codici))
    ; calcolo dei codici unici
    (setq unici (unique codici))
    ; conteggio delle occorrenze dei codici unici
    (map list unici (count unici codici))))

Proviamo:

(setq L '((1 3) (2 4) (0 0) (3 1) (2 4) (3 2) (4 2)))
(coppie-uniche L)
;-> ((24 3) (23 1) (13 2) (0 1))

Possiamo anche calcolare tutto con un ciclo unico utilizzando una lista associativa per memorizzare le coppie uniche e le relative occorrenze.

(define (couple lst)
  (local (alst codice)
    (setq alst '())
    (dolist (el lst)
      ; codifica la coppia corrente
      (setq codice (codifica el))
      ; se il codice corrente esiste nella lista associativa...
      (if (lookup codice alst)
          ; allora aumenta di 1 le sue occorrenze
          (setf (lookup codice alst) (+ $it 1))
          ; altrimenti inserisce il (codice 1) nella lista associativa
          (push (list codice 1) alst -1)))
    alst))

Proviamo:

(setq L '((1 3) (2 4) (0 0) (3 1) (2 4) (3 2) (4 2)))
(couple L)
;-> ((13 2) (24 3) (0 1) (23 1))

Test di correttezza:

(setq c (map list (rand 10 100) (rand 10 100)))
(= (sort (couple c)) (sort (coppie-uniche c)))
;-> true

Test di velocità:

(setq c (map list (rand 10 100) (rand 10 100)))
(time (couple c) 1e4)
;-> 578.106
(time (coppie-uniche c) 1e4)
;-> 609.223

(silent (setq c (map list (rand 10 10000) (rand 10 10000))))
(time (couple c) 100)
;-> 687.625
(time (coppie-uniche c) 100)
;-> 734.264

(silent (setq c (map list (rand 10 100000) (rand 10 100000))))
(time (couple c) 10)
;-> 703.247
(time (coppie-uniche c) 10)
;-> 812.571


--------------------------------------
Coppie di interi con somma predefinita
--------------------------------------

Data una lista di interi trovare tutte le coppie di numeri che sommano a K.
Ogni elemento della lista può essere usato una sola volta per formare una coppia.

(define (coppie-somma-K nums K)
  ; nums -> lista di numeri
  ; K    -> somma desiderata
  (letn (risultati '()   ; lista dove salveremo le coppie valide
         usati '())      ; lista degli indici già usati in altre coppie
    (for (i 0 (- (length nums) 2))  ; ciclo sul primo elemento della coppia
      (unless (ref i usati)         ; verifica che questo elemento non sia già stato usato
        (let (trovato nil)          ; variabile booleana per interrompere il ciclo interno
          (for (j (+ i 1) (- (length nums) 1))  ; ciclo sul secondo elemento
            (when (and (not trovato) (not (ref j usati))
                       (= (+ (nums i) (nums j)) K)) ; verifica la somma
              (push (list (nums i) (nums j)) risultati -1) ; salva la coppia
              (push i usati -1)   ; segna il primo elemento come usato
              (push j usati -1)   ; segna il secondo elemento come usato
              (setq trovato true)))))) ; interrompe il ciclo interno appena trovato un abbinamento
    risultati)) ; restituisce tutte le coppie valide

Proviamo:

(coppie-somma-K '(1 2 3 4 5 6 6 1 8 6) 7)
;-> ((1 6) (2 5) (3 4) (6 1))

(coppie-somma-K '(1 2 3 4 5 1 2 3 4 5 1 6) 6)
;-> ((1 5) (2 4) (3 3) (1 5) (2 4))

(coppie-somma-K '(5 1 1 1 1 1 5) 7)
;-> ()


------------------------
N numeri che sommano a K
------------------------

Data una lista di interi, un numero N e una somma K, determinare tutte le combinazioni di N numeri che sommano a K.

Caso 1
------
Ogni elemento della lista può essere usato solo una volta per ogni combinazione, ma può essere usato quante volte vogliamo per formare altre combinazioni.

(define (comb k lst (r '()))
"Generate all combinations of k elements without repetition from a list of items"
  (if (= (length r) k)
    (list r)
    (let (rlst '())
      (dolist (x lst)
        (extend rlst (comb k ((+ 1 $idx) lst) (append r (list x)))))
      rlst)))

Calcoliamo tutte le combinazioni di N numeri e filtriamo quelle che sommano a K.

(define (N-somma-K-all lst N K)
  (let (combinazioni (comb N lst))
    (filter (fn(x) (= (apply + x) K)) combinazioni)))

(N-somma-K-all '(1 2 3 4 5 6 6 1 8 6) 2 7)
;-> ((1 6) (1 6) (1 6) (2 5) (3 4) (6 1) (6 1) (1 6))

(N-somma-K-all '(2 2 2 3 3 4) 3 7)
;-> ((2 2 3) (2 2 3) (2 2 3) (2 2 3) (2 2 3) (2 2 3))

(N-somma-K-all '(1 2 3 4 5 6 6 1 8 6) 3 12)
;-> ((1 3 8) (1 5 6) (1 5 6) (1 5 6) (2 4 6) (2 4 6) (2 4 6) (3 4 5)
;->  (3 1 8) (5 6 1) (5 6 1) (5 1 6))

(N-somma-K-all '(1 1 1) 3 3)
;-> ((1 1 1))

(setq t (rand 10 100))
(time (println (length (N-somma-K-all t 3 10))))
;-> 9188
;-> 185.799

Caso 2
------
Ogni elemento della lista può essere usato solo una volta in assoluto.

;---------------------------------------------------------
; Funzione: N-somma-K
; Trova gruppi di N numeri in lst che sommano a K.
; Ogni elemento viene usato solo una volta (nessuna ripetizione).
;---------------------------------------------------------
(define (N-somma-K nums N K)
  ; nums -> lista di numeri (anche con duplicati)
  ; N    -> quanti elementi sommare
  ; K    -> somma desiderata
  (letn (risultati '()        ; lista delle combinazioni valide trovate
         usati '()            ; indici già usati in combinazioni precedenti
         lung (length nums))  ; numero totale di elementi in nums
    ;------------------------------------------------------------
    ; Funzione ausiliaria che genera tutte le combinazioni di N indici
    ; distinti scelti da una lista di indici disponibili
    ;------------------------------------------------------------
    (define (combinazioni-indici indici n)
      (let (ris '())
        (if (= n 1)
            ; caso base: ogni indice è una combinazione singola
            (dolist (x indici)
              (push (list x) ris -1))
            ; caso generale: costruiamo combinazioni prendendo il primo
            ; elemento e combinandolo con le sottocombinazioni del resto
            (for (i 0 (- (length indici) n))
              (letn (resto (slice indici (+ i 1)))
                ; generiamo solo se ci sono abbastanza elementi rimanenti
                (when (>= (length resto) (- n 1))
                  (dolist (c (combinazioni-indici resto (- n 1)))
                    ; aggiunge (indici i) davanti alla sottocombinazione
                    (push (cons (indici i) c) ris -1))))))
        ris)) ; restituisce tutte le combinazioni trovate
    ;------------------------------------------------------------
    ; Ciclo principale: cerca combinazioni valide e segna gli indici usati
    ;------------------------------------------------------------
    (let (trovato true)
      (while trovato
        (setq trovato nil) ; assume che non troverà altre combinazioni
        ; crea la lista degli indici ancora disponibili
        (letn (disponibili (filter (fn (x) (not (ref x usati)))
                                   (sequence 0 (- lung 1))))
          ; se non ci sono abbastanza elementi, interrompi il ciclo
          (when (>= (length disponibili) N)
            ; genera tutte le combinazioni di indici disponibili
            (dolist (c (combinazioni-indici disponibili N))
              (when (not trovato)
                ; ottieni i valori corrispondenti ai rispettivi indici
                (let (valori (map (fn (x) (nums x)) c))
                  ; verifica se la somma dei valori è uguale a K
                  (when (= (apply add valori) K)
                    ; aggiungi la combinazione trovata al risultato
                    (push valori risultati -1)
                    ; segna tutti gli indici come usati globalmente
                    (dolist (idx c)
                      (push idx usati -1))
                    ; indica che è stata trovata almeno una combinazione
                    (setq trovato true)))))))))
    ;------------------------------------------------------------
    risultati)) ; restituisce la lista di tutte le combinazioni valide

Spiegazione:

1) combinazioni-indici
Genera tutte le combinazioni possibili di N indici distinti da una lista di indici disponibili.
Viene usata per evitare di ripetere lo stesso elemento più volte.

2) Lista usati
Tiene traccia di tutti gli indici già coinvolti in combinazioni precedenti, garantendo che ogni elemento della lista originale sia usato una sola volta in tutto il risultato.

3) Ciclo while
Continua a cercare nuove combinazioni finché ne trova almeno una.
Quando non è più possibile formare somme esatte, termina.

Proviamo:

(N-somma-K '(1 2 3 4 5 6 6 1 8 6) 2 7)
;-> ((1 6) (2 5) (3 4) (6 1))

(N-somma-K '(2 2 2 3 3 4) 3 7)
;-> ((2 2 3))

(N-somma-K '(1 2 3 4 5 6 6 1 8 6) 3 12)
;-> ((1 3 8) (2 4 6) (5 6 1))

(N-somma-K '(1 1 1) 3 3)
;-> ((1 1 1))

La funzione è molto lenta:

(setq t (rand 10 100))
(time (println (N-somma-K t 3 10)))
;-> ((8 0 2) (8 2 0) (8 2 0) (2 2 6) (8 1 1) (8 1 1) (8 2 0) (8 1 1) (3 4 3)
;->  (8 0 2) (6 3 1) (5 5 0) (9 0 1) (7 2 1) (9 0 1) (6 2 2) (6 2 2) (4 4 2)
;->  (5 3 2) (5 3 2) (3 3 4))
;-> 47836.434

Proviamo a scrivere una versione iterativa:

;---------------------------------------------------------
; Funzione: N-somma-K-iter
; Trova gruppi di N numeri in lst che sommano a K.
; Ogni elemento viene usato solo una volta (nessuna ripetizione).
;---------------------------------------------------------
(define (N-somma-K-iter lst N K)
  ; lst = lista dei numeri
  ; N   = lunghezza della combinazione
  ; K   = somma desiderata
  (letn (risultati '()
         lung (length lst)
         usati '()) ; lista di indici già utilizzati
    ; generiamo tutte le combinazioni di N indici non ancora usati
    (define (combinazioni indici n)
      (let (ris '())
        (if (= n 1)
            (dolist (x indici) (push (list x) ris -1))
            (for (i 0 (- (length indici) n))
              (letn (resto (slice indici (+ i 1)))
                (when (>= (length resto) (- n 1))
                  (dolist (c (combinazioni resto (- n 1)))
                    (push (cons (indici i) c) ris -1))))))
        ris))
    ; ciclo principale: continua finché ci sono combinazioni disponibili
    (let (trovato true) ; booleano per continuare il ciclo
      (while trovato
        (setq trovato nil)
        (letn (disponibili (filter (fn (x) (not (ref x usati)))
                                   (sequence 0 (- lung 1))))  ; indici liberi
          (when (>= (length disponibili) N)
            (dolist (c (combinazioni disponibili N)) ; tutte le combinazioni di indici liberi
              (when (not trovato)
                (let (valori (map (fn (i) (lst i)) c)) ; valori corrispondenti agli indici
                  (when (= (apply add valori) K)
                    (push valori risultati -1) ; aggiunge combinazione valida
                    ; marca gli indici come usati globalmente
                    (dolist (idx c) (push idx usati -1))
                    (setq trovato true)))))))))
    risultati))

Proviamo:

(N-somma-K-iter '(1 2 3 4 5 6 6 1 8 6) 2 7)
;-> ((1 6) (2 5) (3 4) (6 1))

(N-somma-K-iter '(2 2 2 3 3 4) 3 7)
;-> ((2 2 3))

(N-somma-K-iter '(1 2 3 4 5 6 6 1 8 6) 3 12)
;-> ((1 3 8) (2 4 6) (5 6 1))

(N-somma-K-iter '(1 1 1) 3 3)
;-> ((1 1 1))

(setq t (rand 10 100))
(time (println (N-somma-K-iter t 3 10)))
;-> ((8 0 2) (8 2 0) (8 2 0) (2 2 6) (8 1 1) (8 1 1) (8 2 0) (8 1 1) (3 4 3)
;->  (8 0 2) (6 3 1) (5 5 0) (9 0 1) (7 2 1) (9 0 1) (6 2 2) (6 2 2) (4 4 2)
;->  (5 3 2) (5 3 2) (3 3 4))
;-> 27621.018

La versione iterativa è più veloce.

Test di correttezza:

(= (sort (N-somma-K-iter t 3 10)) (sort (N-somma-K t 3 10)))
;-> true


------------------------
Lo Stack size di newLISP
------------------------

newlisp -s 4000
newlisp -s 100000 aprog bprog
newlisp -s 6000 myprog
newlisp -s 6000 http://asite.com/example.lsp

The above examples show starting newLISP with different stack sizes using the -s option, as well as loading one or more newLISP source files and loading files specified by an URL.
When no stack size is specified, the stack defaults to 2048.
Per stack position about 80 bytes of memory are preallocated.


----------------------------
Conversione IPv4 <--> Intero
----------------------------

Un indirizzo IPv4 è formato da 32 bit, normalmente rappresentati in notazione decimale puntata, cioè divisi in 4 gruppi da 8 bit (ottetti) separati da punti (".").

Ecco la struttura:

  xxxxxxxx.xxxxxxxx.xxxxxxxx.xxxxxxxx

Ogni gruppo (ottetto) viene convertito in un numero decimale tra 0 e 255.

Esempio:

  IPv4 = 192.168.1.10

  | Parte      | Bit | Range | Esempio |
  | ---------- | --- | ----- | ------- |
  | 1° ottetto | 8   | 0–255 | 192     |
  | 2° ottetto | 8   | 0–255 | 168     |
  | 3° ottetto | 8   | 0–255 | 1       |
  | 4° ottetto | 8   | 0–255 | 10      |

L'indirizzo 192.168.1.10 in binario è:

  11000000.10101000.00000001.00001010

Formula per convertire un indirizzo IPv4 nella sua rappresentazione intera:

  (Ottetto1 * 16777216) + (Ottetto2 * 65536) + (Ottetto3 * 256) + (Ottetto4)
  dove: 16777216 = 256^3, 65536 = 256^2

Esempi:

  IPv4 = 192.168.1.1
  Intero = (192 * 16777216) + (168 * 65536) + (1 * 256) + 1 = 3232235777

  IPv4 = 10.10.104.36
  Intero = (10 * 16777216) + (10 * 65536) + (104 * 256) + 36 = 168454180

  IPv4 = 8.8.8.8
  Intero = (8 * 16777216) + (8 * 65536) + (8 * 256) + 8 = 134744072

(define (ip4-int str)
  (apply + (map * (map int (parse str ".")) '(16777216 65536 256 1))))

(ip4-int "192.168.1.1")
;-> 3232235777
(ip4-int "10.10.104.36")
;-> 168454180
(ip4-int "8.8.8.8")
;-> 134744072
(ip4-int "255.255.255.255")
;-> 4294967295

Oppure con gli operatori bitwise:

  num = (a << 24) | (b << 16) | (c << 8) | d

(define (ip4-int-bit str)
  (letn ((lst (map int (parse str "."))))
    (| (<< (lst 0) 24)
       (<< (lst 1) 16)
       (<< (lst 2) 8)
       (lst 3))))

(ip4-int-bit "192.168.1.1")
;-> 3232235777
(ip4-int-bit "10.10.104.36")
;-> 168454180
(ip4-int-bit "8.8.8.8")
;-> 134744072
(ip4-int-bit "255.255.255.255")
;-> 4294967295

Formula per convertire un intero IPv4 nella sua rappresentazione in ottetti:

  a = (num / 16777216)
  b = (num / 65536) % 256
  b = (num / 256) % 256
  c = num % 256

(define (int-ip4 num)
  (join (map string (list (/ num 16777216)
                          (% (/ num 65536) 256)
                          (% (/ num 256) 256)
                          (% num 256))) "."))

(int-ip4 168454180)
;-> "10.10.104.36"
(int-ip4 134744072)
;-> "8.8.8.8"
(int-ip4 4294967295)
;-> "255.255.255.255"

Oppure con gli operatori bitwise:

  a = (ip >> 24) & 255
  b = (ip >> 16) & 255
  c = (ip >> 8)  & 255
  d = ip & 255

(define (int-ip4-bit num)
  (join (map string (list (& (>> num 24) 255)
                          (& (>> num 16) 255)
                          (& (>> num 8)  255)
                          (& num 255))) "."))

(int-ip4-bit 3232235777)
;-> "192.168.1.1"
(int-ip4-bit 168454180)
;-> "10.10.104.36"
(int-ip4-bit 134744072)
;-> "8.8.8.8"
(int-ip4-bit 4294967295)
;-> "255.255.255.255"

Le due funzioni sono perfettamente inverse tra loro e non perdono precisione, poiché usano solo operazioni su interi a 32 bit.


------------------
Ricostruzione IPv4
------------------

Abbiamo degli indirizzi IPv4 senza il punto "." che separa i numeri.
Scrivere una funzione che determina tutti gli IPv4 validi del numero dato.

Esempio:
 numero = "25425411224"
 IPv4 validi = "254.254.11.224", "254.254.112.24")

Algoritmo
---------
1) Divisione della stringa in tutti i modi possibili
2) Selezione delle divisioni che hanno esattamente 4 elementi (stringhe)
   in cui ogni elemento deve valere "0" oppure non iniziare con "0".
3) Selezione degli elementi i cui 4 valori sono minori di 256

(define (split-string str)
"Generate all the splits of a string in order"
(if (= (length str) 1) (list (list str))
  ;else
  (local (out len max-tagli taglio fmt cur-str)
    (setq out '())
    (setq len (length str))
    ; numero massimo di tagli
    (setq max-tagli (- len 1))
    ; formattazione con 0 davanti
    (setq fmt (string "%0" max-tagli "s"))
    (for (i 0 (- (pow 2 max-tagli) 1))
      ; taglio corrente
      (setq taglio (format fmt (bits i)))
      ; taglia la stringa con taglio corrente
      (setq cur-str (split-aux str taglio))
      ; inserisce la stringa tagliata corrente nella soluzione
      (push cur-str out -1)
    )
    out)))
;Funzione ausiliaria
(define (split-aux str binary)
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

Proviamo:

(define (make-ip str)
  (local (out parts)
    (setq out '())
    ; calcola tutte le divisioni della stringa
    (setq parts (map parse (split-string str)))
    ; Prende solo le parti con 4 elementi i quali devono tutti :
    ; valere "0" oppure non iniziare con "0"
    (setq parts (filter
                  (fn(x) (and (= (length x) 4)
                              (for-all (fn(y) (or (= y "0") (!= (y 0) "0"))) x))) parts))
    ;(print parts) (read-line)
    ; ciclo sulle parti per determinare gli ip validi
    (dolist (el parts)
      ; converte in interi le stringhe dell'elemento corrente
      (setq el (map (fn(x) (int x 0 10)) el))
      ; se tutti numeri sono minori di 256,
      ; allora è un ip valido e lo inseriamo nella soluzione
      (if (for-all (fn(x) (< x 256)) el) (begin
          (push (join (map string el) ".") out))))
    out))

Proviamo:

(make-ip "25425411224")
;-> ("254.254.11.224" "254.254.112.24")

(make-ip "192168110")
;-> ("1.92.168.110" "19.2.168.110" "19.21.68.110" "19.216.8.110"
;->  "19.216.81.10" "192.1.68.110" "192.16.8.110" "192.16.81.10"
;->  "192.168.1.10" "192.168.11.0")

(make-ip "0000")
;-> ("0.0.0.0")

(make-ip "202034")
;-> ("2.0.20.34" "2.0.203.4" "20.2.0.34" "20.20.3.4" "202.0.3.4")

Possiamo scrivere una funzione che divide la stringa solo in quattro parti in tutti i modi possibili.

Algoritmo
---------
Ci sono n−1 possibili punti di taglio nella stringa.
Per ottenere K parti, servono K−1 tagli.
Si generano tutte le combinazioni crescenti di tagli.
Ogni combinazione (p1, p2, ..., p(K-1)) produce le sottostringhe:

  [0,p1), [p1,p2), ..., [p(K-1), fine)

(define (comb k lst (r '()))
"Generate all combinations of k elements without repetition from a list of items"
  (if (= (length r) k)
    (list r)
    (let (rlst '())
      (dolist (x lst)
        (extend rlst (comb k ((+ 1 $idx) lst) (append r (list x)))))
      rlst)))

(define (divide-string str K)
  ; divide la stringa str in tutte le possibili K parti non vuote
  (letn (n (length str)
         tagli (comb (- K 1) (sequence 1 (- n 1)))
         risultati '())
    (dolist (pos tagli)
      (let (start 0 parti '())
        (dolist (p pos)
          (push (slice str start (- p start)) parti -1)
          (setq start p))
        (push (slice str start) parti -1)
        (push parti risultati -1)))
    risultati))

(define (make-ip2 str)
  ; divide la stringa str in tutte le possibili 4 parti non vuote
  (letn ( (n (length str))
          (tagli (comb (- 4 1) (sequence 1 (- n 1))))
          (four '())
          (out '()) )
    (dolist (pos tagli)
      (let ( (start 0) (parti '()) )
        (dolist (p pos)
          (push (slice str start (- p start)) parti -1)
          (setq start p))
        (push (slice str start) parti -1)
        ; Prende solo le parti i cui tutti i 4 elementi
        ; valgono "0" oppure non iniziano con "0"
        (if (for-all (fn(x) (or (= x "0") (!= (x 0) "0"))) parti)
            (push parti four -1))))
    ; ciclo sulla lista four parti per determinare gli ip validi
    (dolist (el four)
      ; converte in interi le stringhe dell'elemento corrente
      (setq el (map (fn(x) (int x 0 10)) el))
      ; se tutti numeri sono minori di 256,
      ; allora è un ip valido e lo inseriamo nella soluzione
      (if (for-all (fn(x) (< x 256)) el) (begin
          (push (join (map string el) ".") out))))
    out))

(make-ip2 "25425411224")
;-> ("254.254.11.224" "254.254.112.24")

(make-ip2 "192168110")
;-> ("1.92.168.110" "19.2.168.110" "19.21.68.110" "19.216.8.110"
;->  "19.216.81.10" "192.1.68.110" "192.16.8.110" "192.16.81.10"
;->  "192.168.1.10" "192.168.11.0")

(make-ip2 "0000")
;-> ("0.0.0.0")

(make-ip2 "202034")
;-> ("2.0.20.34" "2.0.203.4" "20.2.0.34" "20.20.3.4" "202.0.3.4")


-----------------------------------------------------------
Dividere in tutti i modi una lista o una stringa in K parti
-----------------------------------------------------------

(define (comb k lst (r '()))
"Generate all combinations of k elements without repetition from a list of items"
  (if (= (length r) k)
    (list r)
    (let (rlst '())
      (dolist (x lst)
        (extend rlst (comb k ((+ 1 $idx) lst) (append r (list x)))))
      rlst)))

(define (divide-parts obj K)
  ;-----------------------------------------------------------
  ; Funzione: divide-parts
  ; Scopo   : dividere una lista/stringa 'obj' in tutte le possibili
  ;           K sottoliste/sottostringhe non vuote, mantenendo l'ordine.
  ;
  ; Argomenti:
  ;   obj -> la lista/stringa di partenza (es. '(a b c d e))
  ;   K   -> numero di parti desiderate (es. 3 -> ((a)(b)(c d e)) ecc.)
  ;
  ; Metodo:
  ;   1. La lista/stringa di lunghezza n può essere tagliata in (n-1) punti.
  ;   2. Per ottenere K parti, servono (K-1) tagli.
  ;   3. Si generano tutte le combinazioni crescenti di (K-1) indici
  ;      da 1 a (n-1).
  ;   4. Ogni combinazione rappresenta dove tagliare la lista/stringa.
  ;   5. Si costruiscono le sottoliste/sottostringhe corrispondenti e
  ;      si aggiungono al risultato.
  ;
  ; Dipendenze: richiede la funzione (comb k lista).
  ;-----------------------------------------------------------
  (letn ( (n (length obj))                             ; lunghezza della lista
          (tagli (comb (- K 1) (sequence 1 (- n 1))))  ; tutte le combinazioni di tagli
          (out '()) )                            ; lista dei out finali
    ; per ogni combinazione di posizioni di taglio
    (dolist (pos tagli)
      (let ( (start 0) (parti '()) )   ; posizione iniziale e lista delle parti
        ; per ogni posizione di taglio nella combinazione
        (dolist (p pos)
          (push (slice obj start (- p start)) parti -1) ; aggiunge sottolista [start,p)
          (setq start p))                 ; aggiorna inizio per la prossima
        (push (slice obj start) parti -1) ; aggiunge ultima parte fino alla fine
        (push parti out -1)))       ; aggiunge la decomposizione trovata
    out))                           ; restituisce tutte le suddivisioni

Proviamo:

(divide-parts '(a b c d e) 4)
;-> (((a) (b) (c) (d e)) ((a) (b) (c d) (e))
;->  ((a) (b c) (d) (e)) ((a b) (c) (d) (e)))

(divide-parts "abcde" 4)
;-> (("a" "b" "c" "de") ("a" "b" "cd" "e")
;->  ("a" "bc" "d" "e") ("ab" "c" "d" "e"))

(divide-parts "abcde" 5)
;-> (("a" "b" "c" "d" "e"))

(divide-parts "abcde" 6)
;-> ()

Nota: numero combinazioni = C(n-1, K-1)

Ecco la versione che non costruisce l'intera lista dei risultati, ma restituisce una partizione alla volta tramite una funzione fornita come call-back.
Questo approccio è utile se vogliamo elaborare molte partizioni senza consumare troppa memoria.

(define (divide-parts-one obj K call-back)
  ;-----------------------------------------------------------
  ; Funzione: divide-parts-one
  ; Scopo   : generare tutte le possibili divisioni di una lista/stringa obj
  ;           in K sottoliste/sottostringhe non vuote, chiamando 'call-back'
  ;           su ciascuna partizione trovata.
  ;
  ; Argomenti:
  ;   obj       -> lista/stringa di partenza (es. '(a b c d e))
  ;   K         -> numero di sottoliste/sottostringhe desiderate
  ;   call-back -> funzione che riceve una singola partizione
  ;                (verrà invocata con una lista di K sottoliste/sottostringhe)
  ;
  ; Esempio:
  ;   (divide-parts-on '(a b c d) 3 println)
  ;
  ; Metodo:
  ;   identico alla versione base, ma invece di accumulare i risultati
  ;   li invia direttamente alla funzione call-back.
  ;
  ; Nota: richiede la funzione (comb k lista).
  ;-----------------------------------------------------------
  (letn ( (n (length obj))
          (tagli (comb (- K 1) (sequence 1 (- n 1)))) )
    (dolist (pos tagli)
      (let ( (start 0) (parti '()) )
        (dolist (p pos)
          (push (slice obj start (- p start)) parti -1)
          (setq start p))
        (push (slice obj start) parti -1)
        (call-back parti)))))

(divide-parts-one '(a b c d e) 4 println)
;-> ((a) (b) (c) (d e))
;-> ((a) (b) (c d) (e))
;-> ((a) (b c) (d) (e))
;-> ((a b) (c) (d) (e))

(divide-parts-one '(1 2 3 4 5) 3 println)
;-> ((1) (2) (3 4 5))
;-> ((1) (2 3) (4 5))
;-> ((1) (2 3 4) (5))
;-> ((1 2) (3) (4 5))
;-> ((1 2) (3 4) (5))
;-> ((1 2 3) (4) (5))
;-> ((1 2 3) (4) (5))

Questo metodo permette di filtrare solo certe partizioni:

(divide-parts-one '(1 2 3 4 5) 3
  (fn(x) (when (> (apply + (first x)) 1) (println x))))
;-> ((1 2) (3) (4 5))
;-> ((1 2) (3 4) (5))
;-> ((1 2 3) (4) (5))
;-> ((1 2 3) (4) (5))

In questo modo la funzione si comporta come un generatore controllato che produce tutte le suddivisioni, ma permette di decidere se usarle o scartarle.

Adesso scriviamo una versione che restituisce un iteratore vero e proprio (cioè una funzione che possiamo chiamare più volte per ottenere la prossima partizione).

(define (comb k lst (r '()))
"Generate all combinations of k elements without repetition from a list of items"
  (if (= (length r) k)
    (list r)
    (let (rlst '())
      (dolist (x lst)
        (extend rlst (comb k ((+ 1 $idx) lst) (append r (list x)))))
      rlst)))

;(context 'divide-parts-gen)
;-----------------------------------------
; Generatore che divide una lista o stringa
; in tutte le possibili partizioni in K parti.
; Usa un contesto per mantenere lo stato.
;-----------------------------------------
(define (divide-parts-gen:init seq K)
  (letn (n (length seq))
    (if (or (< K 2) (< n K))
        (begin (setq divide-parts-gen:partitions nil) false)
        (begin
          ; Tutte le combinazioni di posizioni di taglio
          (setq divide-parts-gen:cuts (comb (- K 1) (sequence 1 (- n 1))))
          (setq divide-parts-gen:seq seq)
          (setq divide-parts-gen:index 0)
          true))))

;-----------------------------------------------------------
; Funtore del contesto: genera la prossima partizione
;-----------------------------------------------------------
(define (divide-parts-gen:divide-parts-gen)
  (if (or (not divide-parts-gen:cuts)
          (>= divide-parts-gen:index (length divide-parts-gen:cuts)))
      nil
      (letn (seq divide-parts-gen:seq
             cuts (divide-parts-gen:cuts divide-parts-gen:index)
             start 0 parts '())
        (dolist (p (append cuts (list (length seq))))
          (push (slice seq start (- p start)) parts -1)
          (setq start p))
        (inc divide-parts-gen:index)
        parts)))

;(context MAIN)

Proviamo:

Lista:
(divide-parts-gen:init "abcde" 4)
;-> true
(divide-parts-gen)
;-> ("a" "b" "c" "de")
(divide-parts-gen)
;-> ("a" "b" "cd" "e")
(divide-parts-gen)
;-> ("a" "bc" "d" "e")
(divide-parts-gen)
;-> ("ab" "c" "d" "e")
(divide-parts-gen)
;-> nil

Stringa:
(divide-parts-gen:init "abcde" 4)
;-> true
(divide-parts-gen)
;-> ("a" "b" "c" "de")
(divide-parts-gen)
;-> ("a" "b" "cd" "e")
(divide-parts-gen)
;-> ("a" "bc" "d" "e")
(divide-parts-gen)
;-> ("ab" "c" "d" "e")
(divide-parts-gen)
;-> nil


----------------
Sequenza a*b + 1
----------------

Determinare la sequenza definita nel modo seguente:
  a(0) = 2
  se a e b appartengono alla sequenza, allora anche (a*b + 1) ne fa parte.

Sequenza OEIS A009293:
If a, b in sequence, so is a*b+1.
  2, 5, 11, 23, 26, 47, 53, 56, 95, 107, 113, 116, 122, 131, 191, 215, 227,
  233, 236, 245, 254, 263, 266, 281, 287, 383, 431, 455, 467, 473, 476, 491,
  509, 518, 527, 530, 533, 536, 563, 566, 575, 581, 584, 599, 611, 617, 656,
  677, 767, 863, 911, 935, 947, 953, 956, 983, ...

Algoritmo
- Si parte dal numero 2,
- Si considerano tutte le coppie ((a,b)) appartenenti all'insieme corrente,
- Si aggiungono i nuovi valori (a*b + 1) che rispettano la condizione,
- E si ripete fino a stabilità.

;------------------------------------------------------------
; Generatore della sequenza OEIS A009293
; Se a e b appartengono alla sequenza, allora anche (a*b + 1) ne fa parte.
; Si parte dal valore iniziale 2 e si aggiungono nuovi termini
; fino a quando non si generano più numeri minori o uguali a 'limite'.
;------------------------------------------------------------
(define (sequenza limite)
  ; 'out' contiene la sequenza generata (inizialmente con solo 2)
  ; 'finito' serve per capire se in un ciclo non vengono trovati nuovi elementi
  (letn ( (out '(2 5)) (finito nil) )
    (setq finito nil)    ; inizializza la variabile di controllo
    ; ciclo principale: continua finché vengono aggiunti nuovi numeri
    (while (not finito)
      (setq finito true) ; assume che non si trovino nuovi numeri
      ; esplora tutte le coppie (a,b) della sequenza corrente
      (for (i 0 (- (length out) 1))
        (for (j i (- (length out) 1))
          (letn (a (out i) b (out j) x (+ 1 (* a b)))  ; calcola x = a*b + 1
            (when (<= x limite)          ; considera solo i valori <= limite
              (when (not (ref x out))   ; se x non è già nella sequenza
                (push x out -1)         ; aggiungilo alla fine
                (setq finito nil))))))) ; è stato trovato un nuovo numero
    ; ordina la sequenza in ordine crescente prima di restituirla
    (sort out (fn (u v) (< u v)))))

(sequenza 1000)
;-> (2 5 11 23 26 47 53 56 95 107 113 116 122 131 191 215 227
;->  233 236 245 254 263 266 281 287 383 431 455 467 473 476 491
;->  509 518 527 530 533 536 563 566 575 581 584 599 611 617 656
;->  677 767 863 911 935 947 953 956 983)

Da notare che la regola di definizione della sequenza:

 "if a,b in sequence, then a*b + 1 is also in the sequence"

richiede almeno una coppia iniziale (a,b) per cominciare a generare nuovi termini.
Comunque in questo caso il motivo per cui basta partire solo da '2' è che la sequenza è definita come la chiusura minima rispetto all'operazione (a,b --> a*b + 1) contenente il numero 2.
Quindi l'intera sequenza si auto-genera partendo dal singolo seme '2', proprio perché l'operazione è chiusa:
il singolo elemento 2 basta perché genera da solo la prima coppia utile (2,2).


-----------------------------
Generatore di intervalli IPv4
-----------------------------

Dati due indirizzi IPv4 a e b, restituire tutti gli indirizzi compresi in quell'intervallo.

Primo metodo
------------
Conversione degli IP in interi a 32 bit

Algoritmo
1) 'ip4-int' converte un indirizzo in formato "a.b.c.d" in un intero a 32 bit:
             a * 256^3 + b * 256^2 + c * 256 + d
2) 'int-ip4' fa l'inverso: da un numero intero ricava i 4 byte corrispondenti.
3) 'intervallo-ip':
   - Converte i due indirizzi 'a' e 'b' in interi.
   - Crea una lista di tutti i numeri da 'start' a 'end'.
   - Li riconverte in stringhe IP e li accumula in 'out'.
   - Restituisce la lista completa.

Nota: l'intervallo tra "124.0.200.0" e "125.0.0.0" contiene più di 16 milioni di indirizzi IP, quindi la funzione genererà una lista enorme.

; Funzione per convertire un indirizzo IP (stringa) in intero
(define (ip4-int str)
  (apply + (map * (map int (parse str ".")) '(16777216 65536 256 1))))

; Funzione per convertire un intero in indirizzo IP (stringa)
(define (int-ip4 num)
  (join (map string (list (/ num 16777216)
                          (% (/ num 65536) 256)
                          (% (/ num 256) 256)
                          (% num 256)))
        "."))

; Funzione principale: restituisce tutti gli IP compresi tra a e b inclusi
(define (intervallo-ip a b)
  (letn ( (start (ip4-int a)) (end (ip4-int b)) (out '()) )
    (for (i start end)
      ;(print (int-ip4 i)) (read-line)
      (push (int-ip4 i) out -1))
    out))

Proviamo:

(intervallo-ip "123.0.0.0" "123.0.0.10")
;-> ("123.0.0.0" "123.0.0.1" "123.0.0.2" "123.0.0.3" "123.0.0.4" "123.0.0.5"
;->  "123.0.0.6" "123.0.0.7" "123.0.0.8" "123.0.0.9" "123.0.0.10")

(length (intervallo-ip "123.0.200.0" "123.0.201.10"))
;-> 267
(length (intervallo-ip "123.0.200.0" "123.1.0.0"))
;-> 14437
(length (intervallo-ip "10.6.0.0" "10.6.255.255"))
;-> 65536

Secondo metodo
--------------
Questa è una versione completamente aritmetica che incrementa i quattro ottetti senza convertire in intero.

Algoritmo
1) 'inc-ip' incrementa manualmente i 4 ottetti, gestendo i "riporti" come in un contatore in base 256.
2) 'intervallo-ip':
    - Converte le stringhe iniziale e finale in liste di interi (a b c d).
    - Usa un ciclo 'while' che aggiunge ogni IP alla lista 'out' fino a raggiungere l'ultimo ('end').
    - Riconverte ogni IP in stringa con 'join'.

; Funzione che incrementa di 1 un indirizzo IP rappresentato come lista di 4 ottetti
(define (inc-ip ip)
  (letn ((a (ip 0)) (b (ip 1)) (c (ip 2)) (d (ip 3)))
    (++ d)
    (when (> d 255) (setq d 0) (++ c))
    (when (> c 255) (setq c 0) (++ b))
    (when (> b 255) (setq b 0) (++ a))
    (list a b c d)))

; Funzione principale: genera tutti gli indirizzi da a a b (inclusi)
(define (intervallo-ip-arit a b)
  (letn ((start (map int (parse a ".")))
         (end (map int (parse b ".")))
         (curr start)
         (out '())
         (finito nil))
    (while (not finito)
      (push (join (map string curr) ".") out -1)
      (if (= curr end)
          (setq finito true)
          (setq curr (inc-ip curr))))
    out))

Proviamo:

(intervallo-ip-arit "123.0.0.0" "123.0.0.10")
;-> ("123.0.0.0" "123.0.0.1" "123.0.0.2" "123.0.0.3" "123.0.0.4" "123.0.0.5"
;->  "123.0.0.6" "123.0.0.7" "123.0.0.8" "123.0.0.9" "123.0.0.10")

(length (intervallo-ip-arit "123.0.200.0" "123.0.201.10"))
;-> 267
(length (intervallo-ip-arit "123.0.200.0" "123.1.0.0"))
;-> 14437
(length (intervallo-ip-arit "10.6.0.0" "10.6.255.255"))
;-> 65536

Test di correttezza:

(= (intervallo-ip "10.6.0.0" "10.6.255.255")
   (intervallo-ip-arit "10.6.0.0" "10.6.255.255"))
;-> true

Test di velocità:
(time (intervallo-ip "10.6.0.0" "10.6.255.255") 10)
;-> 1250.202
(time (intervallo-ip-arit "10.6.0.0" "10.6.255.255") 10)
;-> 1336.475


--------------------------------------------------
Ricerca di una stringa in una matrice di caratteri
--------------------------------------------------

Data una matrice di caratteri e una stringa determinare se la stringa si trova nella matrice.
Una stringa si trova nella matrice se i suoi caratteri formano un percorso contiguo nella matrice (nelle 4 direzioni Nord, Est, Sud, Ovest oppure nelle 8 direzioni N, E, S, O, SE, SO, NE, NO).

Algoritmo
1) Si esplora la matrice partendo da ogni cella che corrisponde alla prima lettera della stringa.
2) Si usa una funzione interna cerca che segue i 4 o 8 movimenti possibili.
3) La lista visitati evita di riutilizzare la stessa cella nel percorso corrente.
4) Se si raggiunge l’ultimo carattere (k = len-1), la stringa è trovata

; Funzione che verifica se una stringa è presente in una matrice di caratteri
; La stringa è considerata presente se i suoi caratteri formano un percorso
; contiguo nelle 4 direzioni cardinali: Nord, Est, Sud, Ovest,
; oppure nelle 8 direzioni cardinali: S, N, E, O, SE, SO, NE, NO (diag -> true)
; Ogni cella può essere usata una sola volta nello stesso percorso.
(define (stringa-in-matrice matrice stringa diag)
  (letn ( (righe (length matrice)) ; numero di righe della matrice
          (colonne (length (matrice 0))) ; numero di colonne della matrice
          (trovato nil) ; variabile booleana per segnalare se la stringa è stata trovata
          (len (length stringa)) ; lunghezza della stringa da cercare
          (direzioni '((1 0) (-1 0) (0 1) (0 -1))) ) ; vettori spostamento (Sud, Nord, Est, Ovest)
    (if diag ; 8 direzioni: S, N, E, O, SE, SO, NE, NO
        (setq direzioni '((1 0) (-1 0) (0 1) (0 -1) (1 1) (1 -1) (-1 1) (-1 -1))))
    ; funzione ricorsiva che esplora la matrice a partire da una cella (i,j)
    ; e verifica se è possibile completare la stringa a partire dal carattere indice k
    ; "visitati" è la lista delle coordinate già utilizzate nel percorso corrente
    (define (cerca i j k visitati)
      ; controllo che la posizione sia dentro la matrice, non ancora visitata
      ; e che il carattere della cella corrisponda al carattere della stringa in posizione k
      (if (and (>= i 0) (< i righe) (>= j 0) (< j colonne)
               (= ((matrice i) j) (stringa k))
               (not (ref (list i j) visitati)))
          ; se l’ultimo carattere corrisponde, la stringa è stata trovata
          (if (= k (- len 1))
              (setq trovato true)
              ; altrimenti continua la ricerca nelle quattro direzioni adiacenti
              (dolist (d direzioni)
                (if (not trovato)
                    (cerca (+ i (d 0)) (+ j (d 1)) (+ k 1)
                           (append visitati (list (list i j)))))))))
    ; avvia la ricerca da ogni cella della matrice che contiene il primo carattere della stringa
    (for (i 0 (- righe 1))
      (for (j 0 (- colonne 1))
        (if (and (not trovato) (= ((matrice i) j) (stringa 0)))
            (cerca i j 0 '()))))
    ; restituisce true se la stringa è stata trovata, altrimenti nil
    trovato))

Proviamo:

(setq M '(("A" "B" "C" "E")
          ("S" "F" "C" "S")
          ("A" "D" "E" "E")))

(stringa-in-matrice M "ABCCED")
;-> true
(stringa-in-matrice M "SEE")
;-> true
(stringa-in-matrice M "SCFS")
;-> true
(stringa-in-matrice M "ABCB")
;-> nil
(stringa-in-matrice M "AFE")
;-> nil
(stringa-in-matrice M "AFE" true)
;-> true
(stringa-in-matrice M "AFECBCESE" true)
;-> true

Adesso scriviamo una funzione che restituisce le celle (i j) dei caratteri della stringa trovata.

; Funzione che verifica se una stringa è presente in una matrice di caratteri
; Restituisce la lista delle celle (coordinate) che formano la stringa trovata
; oppure nil se la stringa non è presente
(define (stringa-in-matrice2 matrice stringa diag)
  (letn ( (righe (length matrice))            ; numero di righe della matrice
          (colonne (length (matrice 0)))      ; numero di colonne della matrice
          (len (length stringa))              ; lunghezza della stringa da cercare
          (direzioni '((1 0) (-1 0) (0 1) (0 -1))) ; direzioni cardinali: S, N, E, O
          (percorso-trovato nil) )            ; memorizza il percorso trovato
    (if diag ; se abilitate le diagonali
        (setq direzioni '((1 0) (-1 0) (0 1) (0 -1) (1 1) (1 -1) (-1 1) (-1 -1))))
    ; funzione ricorsiva che esplora la matrice da (i,j)
    (define (cerca i j k visitati)
      (if (and (>= i 0) (< i righe) (>= j 0) (< j colonne)
               (= ((matrice i) j) (stringa k))
               (not (ref (list i j) visitati)))
          (begin
            (setq visitati (append visitati (list (list i j))))
            ; se siamo all'ultimo carattere -> percorso trovato
            (if (= k (- len 1))
                (setq percorso-trovato visitati)
                ; continua a cercare nelle direzioni
                (dolist (d direzioni)
                  (if (not percorso-trovato)
                      (cerca (+ i (d 0)) (+ j (d 1)) (+ k 1) visitati)))))))
    ; avvia la ricerca da ogni cella che contiene il primo carattere
    (for (i 0 (- righe 1))
      (for (j 0 (- colonne 1))
        (if (and (not percorso-trovato) (= ((matrice i) j) (stringa 0)))
            (cerca i j 0 '()))))
    ; restituisce il percorso trovato oppure nil
    percorso-trovato))

Proviamo:

(setq M '(("A" "B" "C" "E")
          ("S" "F" "C" "S")
          ("A" "D" "E" "E")))

(stringa-in-matrice2 M "ABCCED")
;-> ((0 0) (0 1) (0 2) (1 2) (2 2) (2 1))
(stringa-in-matrice2 M "ABCCED")
;-> ((0 0) (0 1) (0 2) (1 2) (2 2) (2 1))
(stringa-in-matrice2 M "SEE")
;-> ((1 3) (2 3) (2 2))
(stringa-in-matrice2 M "SCFS")
;-> ((1 3) (1 2) (1 1) (1 0))
(stringa-in-matrice2 M "ABCB")
;-> nil
(stringa-in-matrice2 M "AFE")
;-> nil
(stringa-in-matrice2 M "AFE" true)
;-> ((0 0) (1 1) (2 2))
(stringa-in-matrice2 M "AFECBCESE" true)
;-> ((0 0) (1 1) (2 2) (1 2) (0 1) (0 2) (0 3) (1 3) (2 3))


-------------------------------------------------------
Numero di occorrenze delle sottostringhe di una stringa
-------------------------------------------------------

Data una stringa di N caratteri e un intero K <= N, determinare tutte le sottostringhe contigue di lunghezza K e le relative occorrenze nella stringa.

Esempio:
  Stringa = "ABCFGABCFGAH"
  K = 3
  Sottostringhe = ("ABC" "BCF" "CFG" "FGA" "GAB" "ABC" "BCF" "CFG" "FGA" "GAH")
  Occorrenze: ("ABC" 2) ("BCF" 2) ("CFG" 2) ("FGA" 2) ("GAB" 1) ("GAH" 1)

(define (group-block obj num)
"Create a list with blocks of elements: (0..num) (1..num+1) (n..num+n)"
  (local (out items len)
    (setq out '())
    (setq len (length obj))
    (if (>= len num) (begin
        ; numero di elementi nella lista di output (numero blocchi)
        (setq items (- len num (- 1)))
        (for (k 0 (- items 1))
          (push (slice obj k num) out -1)
        )))
    out))

Algoritmo
---------
; Input
(setq str "ABCFGABCFGAH")
(setq K 3)
; genera tutte le sottostringhe (uniche) di lunghezza K
(setq blocks (unique (group-block str K)))
;-> ("ABC" "BCF" "CFG" "FGA" "GAB" "GAH")
; conta le occorrenze di ogni sottostringa
(setq occorrenze (map (fn(x) (length (find-all x str))) blocks))
; Crea le coppie (sottostringa occorrenze)
(setq result (map list blocks occorrenze))

Funzione che trova tutte le sottostringhe di lunghezza K e le relative occorrenze in una stringa data:

(define (conta-substr str K)
  (letn (blocks (unique (group-block str K)))
    (map list blocks (map (fn(x) (length (find-all x str))) blocks))))

Proviamo:

(conta-substr "ABCFGABCFGAH" 3)
;-> (("ABC" 2) ("BCF" 2) ("CFG" 2) ("FGA" 2) ("GAB" 1) ("GAH" 1))

(conta-substr "ABCGHDRKABCHFJIGABCHGJIHDRKABJIGABCKEBGAB" 3)
;-> (("ABC" 4) ("BCG" 1) ("CGH" 1) ("GHD" 1) ("HDR" 2) ("DRK" 2) ("RKA" 2)
;->  ("KAB" 2) ("BCH" 2) ("CHF" 1) ("HFJ" 1) ("FJI" 1) ("JIG" 2) ("IGA" 2)
;->  ("GAB" 3) ("CHG" 1) ("HGJ" 1) ("GJI" 1) ("JIH" 1) ("IHD" 1) ("ABJ" 1)
;->  ("BJI" 1) ("BCK" 1) ("CKE" 1) ("KEB" 1) ("EBG" 1) ("BGA" 1))

(conta-substr "abc" 1)
;-> (("a" 1) ("b" 1) ("c" 1))

(conta-substr "123456123" 1)
;-> (("1" 2) ("2" 2) ("3" 2) ("4" 1) ("5" 1) ("6" 1))

(conta-substr "ABCGHDRKABCHFJIGABCHGJIHDRKABJIGABCKEBGAB" 41)
;-> (("ABCGHDRKABCHFJIGABCHGJIHDRKABJIGABCKEBGAB" 1))

Proviamo ad scrivere una funzione unica:

(define (conta-substr2 str K)
  (local (out len items blocco occorrenze)
    (setq out '())
    (setq len (length str))
    (when (>= len K)
      ; numero di elementi nella lista di output (numero blocchi)
      (setq items (- len K (- 1)))
      (for (i 0 (- items 1))
        (setq blocco (slice str i K))
        (setq occorrenze (length (find-all blocco str)))
        (push (list blocco occorrenze) out -1)))
    (unique out)))

(conta-substr2 "123456123" 1)
;-> (("1" 2) ("2" 2) ("3" 2) ("4" 1) ("5" 1) ("6" 1) ("1" 2) ("2" 2) ("3" 2))

(setq s "ABCGHDRKABCHFJIGABCHGJIHDRKABJIGABCKEBGABAUNCBABCDHGFJKGHUTHGABAHUAB")

Test di correttezza:

(= (conta-substr s 2) (conta-substr2 s 2))
;-> true

Test di velocità:

Con K piccolo è più veloce "conta-str":

(time (conta-substr s 2) 1e4)
;-> 647.24
(time (conta-substr2 s 2) 1e4)
;-> (978.15)

Con K grande è più veloce "conta-str2":

(time (conta-substr s 10) 1e4)
;-> 875.325
(time (conta-substr2 s 10) 1e4)
;-> 763.611


----------------------------
Bitwise AND di un intervallo
----------------------------

Dato un intervallo di numeri interi [a, b], calcolare il bitwise AND di tutti i numeri da 'a' a 'b'.

Metodo brute-force
------------------
Generiamo la sequenza di numeri e applichiamo l'AND a tutti.

(define (and-all1 a b) (apply & (sequence a b)))

Proviamo:

(and-all1 26 31)
;-> 24
(and-all1 4 7)
;-> 4
(and-all1 33 44)
;-> 32

Metodo veloce (shift comune)
----------------------------
Esiste un modo molto più efficiente per calcolare il bitwise AND di tutti i numeri in un intervallo [a, b] senza scorrere tutti i valori.
Il risultato dell'AND di tutti i numeri tra 'a' e 'b' è dato dai bit comuni più alti di 'a' e 'b' (cioè la parte del prefisso binario uguale).
Appena 'a' e 'b' differiscono in qualche bit, tutti i bit meno significativi diventano 0 nel risultato.
Finchè 'a' != 'b', facciamo uno shift a destra di entrambi fino a quando diventano uguali, contando gli shift.
Alla fine rispostiamo a sinistra del numero di bit rimossi.

Esempio:
  a = 26 -> 11010
  b = 30 -> 11110
  Prefisso comune: 11000 -> risultato = 24.

 26 = 11010
 30 = 11110
 differiscono a bit 3 -> shift finché uguali
 a = 110  b = 111  -> shift = 2
 a = 11   b = 11   -> uguali -> risultato = 11 << 2 = 11000 = 24

L'AND diventa 0 non appena tra 'a' e 'b' si attraversa una potenza di 2.
Questo significa che i due numeri hanno il bit più significativo diverso.
Matematicamente, l'AND di tutti i numeri in [a, b] vale 0 se e solo se:

  floor(log2 a) != floor(log2 b)

cioè quando 'a' e 'b' appartengono a intervalli di potenze di 2 diversi.

; Funzione che calcola il bitwise AND di tutti i numeri compresi tra a e b
; Individua il prefisso binario comune tra a e b:
; finché a e b differiscono in qualche bit, vengono shiftati a destra
; ogni shift elimina un bit meno significativo che diventa sicuramente 0 nell'AND finale
; quando a e b diventano uguali, si risposta il valore a sinistra per ricostruire il risultato
(define (and-all2 a b)
  (if (!= (floor (log a 2)) (floor (log b 2)))
      0
      ;else
      (let (shift 0)        ; contatore di bit eliminati
        (while (!= a b)     ; finché a e b sono diversi
          (setq a (>> a 1)) ; elimina un bit da a
          (setq b (>> b 1)) ; elimina un bit da b
          (++ shift))       ; incrementa il numero di shift effettuati
        (<< a shift)))) ; ricostruisce il risultato rispostando a sinistra

Proviamo:

(and-all2 26 31)
;-> 24
(and-all2 4 7)
;-> 4
(and-all2 33 44)
;-> 32

Test di correttezza:

(for (i 1 1000)
  (setq a (rand 100))
  (setq b (+ a (rand 100) 1))
  (if (!= (and-all1 a b) (and-all2 a b)) (println a { } b)))
;-> nil

Test di velocità:

(time (and-all1 16777220 33554000))
;-> 369.808
(time (and-all2 16777220 33554000))
;-> 0
(time (and-all1 16777220 33555555))
;-> 355.411
(time (and-all2 16777220 33555555))
;-> 0


-----------------
Flag of Palestine
-----------------

  +--------+-----------+--------+-------------+----------+
  | Colore |     Rosso | Bianco |        Nero |    Verde |
  +--------+-----------+--------+-------------+----------+
  | DOS    |       160 |      0 |         255 |       28 |
  +--------+-----------+--------+-------------+----------+
  | RGB    | 238 42 53 |  0 0 0 | 255 255 255 | 0 151 54 |
  +--------+-----------+--------+-------------+----------+

  p*****************************
  **p***************************
  ****p*************************
  ******p***********************
  ********p*********************
  ******p***********************
  ****p*************************
  **p***************************
  p*****************************

(define (fore color)
"Change the color of terminal foreground (text) (ANSI sequence)"
  (let (f (string "\027[38;5;" color "m"))
    ;(print f "\r" "")))
    (print f)))

(define (back color)
"Change the color of terminal background (ANSI sequence)"
  (let (b (string "\027[48;5;" color "m"))
    ;(print b "\r" "")))
    (print b)))

(define (palestine ch)
  ; set background color (gray)
  (println (back 145) "\n")
  ; print flag line x line
  (print (fore 160) ch) (println (fore 16) (dup ch 28))
  (print (fore 160) (dup ch 3)) (println (fore 16) (dup ch 26))
  (print (fore 160) (dup ch 5)) (println (fore 16) (dup ch 24))
  (print (fore 160) (dup ch 7)) (println (fore 255) (dup ch 22))
  (print (fore 160) (dup ch 9)) (println (fore 255) (dup ch 20))
  (print (fore 160) (dup ch 7)) (println (fore 255) (dup ch 22))
  (print (fore 160) (dup ch 5)) (println (fore 28) (dup ch 24))
  (print (fore 160) (dup ch 3)) (println (fore 28) (dup ch 26))
  (print (fore 160) ch) (println (fore 28) (dup ch 28))
  ; reset foreground and background colors
  (println "\027[39;49m")'>)

Proviamo:

(palestine "*")
(palestine "■")
(palestine "▀")
(palestine "█")


--------------------------
Numeri ordinali in lettere
--------------------------

Scrivere una funzione che prende un intero positivo e restituisce il relativo numero ordinale in lettere.

Esempio:
16 -> Sedicesimo
44 -> Quarantaquattresimo

Sequenza OEIS A372204:
a(n) is the number of letters in the Italian name of the n-th ordinal number.
  5, 7, 5, 6, 6, 5, 7, 6, 4, 6, 10, 10, 11, 15, 12, 10, 15, 12, 14, 9, 11,
  12, 13, 16, 15, 13, 14, 12, 13, 10, 12, 13, 14, 17, 16, 14, 15, 13, 14,
  12, 14, 15, 16, 19, 18, 16, 17, 15, 16, 13, 15, 16, 17, 20, 19, 17, 18,
  16, 17, 12, 14, 15, 16, 19, 18, 16, 17, 15, 16, 12, ...

Esempi:
  Zeresimo, Primo, secondo, terzo, quarto, quinto,
  sesto, settimo, ottavo, nono, decimo, ...

Ad eccezione dei primi 10 numeri, che hanno una denominazione propria, per trasformare un numero cardinale in numero ordinale basta togliere l'ultima lettera e aggiungere:

           Femminile   Maschile
Singolare  -esima      -esimo
Plurale    -esime      -esimi

La caduta della vocale finale del cardinale non avviene per quei numeri terminanti con -tré, perché l'ultima vocale è accentata:
ventitré + -esimo --> ventitreesimo, quarantatré + -esimo -->quarantatreesimo.
Non avviene, inoltre, per i composti con sei: ventisei + -esimo -> ventiseiesimo.
Queste ultime regole non valgono per 13 -> Tredicesimo e 16 -> Sedicesimo.

;---------------------------------------------------------------------------
; Funzioni per la conversione da numero intero a lettere (italiano)
;---------------------------------------------------------------------------
(define (numero num)
  (local (lst tri val out)
    ; la cifra 1
    (setq un "Un")
    ; le dieci cifre - codeA
    (setq cifre '("Zero" "Uno" "Due" "Tre" "Quattro" "Cinque" "Sei" "Sette"
      "Otto" "Nove"))
    ; i primi venti numeri - code
    (setq venti '("Zero" "Uno" "Due" "Tre" "Quattro" "Cinque" "Sei" "Sette"
      "Otto" "Nove" "Dieci" "Undici" "Dodici" "Tredici" "Quattordici"
      "Quindici" "Sedici" "Diciassette" "Diciotto" "Diciannove"))
    ; le decine - codeB
    (setq decine '("" "" "Venti" "Trenta" "Quaranta" "Cinquanta"
      "Sessanta" "Settanta" "Ottanta" "Novanta"))
    ; le decine senza vocali - codeB1
    (setq dcn    '("" "" "Vent" "Trent" "Quarant" "Cinquant"
      "Sessant" "Settant" "Ottant" "Novant"))
    ; il numero 100
    (setq cento "Cento")
    ; multipli con la cifra 1 - codeC
    (setq multiplo '("" "Mille" "Milione" "Miliardo" "Bilione" "Biliardo"
      "Trilione" "Triliardo" "Quadrilione" "Quadriliardo"))
    ; multipli con la cifra diversa da 1 - codeC1
    (setq multipli '("" "Mila" "Milioni" "Miliardi" "Bilioni" "Biliardi"
      "Trilioni" "Triliardi" "Quadrilioni" "Quadriliardi"))
    (setq out "")
    (if (= (string num) "0")
      (setq out "Zero")
      (begin
        ; calcola il numero di triplette
        (if (zero? (% (length (string num)) 3))
            (setq tri (/ (length (string num)) 3))
            (setq tri (+ (/ (length (string num)) 3) 1))
        )
        ; formatta in stringa il numero (padding)
        ; e crea una lista con tutte le triplette
        (setq lst (explode (pad (string num) (* 3 tri) "0") 3))
        ; ciclo per la creazione della stringa finale
        (dolist (el lst)
          ; creazione del numero rappresentato dalla tripletta
          (setq val (triple el))
          ; controllo se tale numero vale "Uno"
          (if (= val "Uno")
            (cond ((= $idx (- (length lst) 1)) ; primo gruppo a destra ?
                  (setq out (append out val))) ; aggiungo solo "Uno"
                  ((= $idx (- (length lst) 2)) ; secondo gruppo a destra ?
                  (setq out (string out (multiplo (- tri 1))))) ;aggiungo solo "Mille"
                  ;altrimenti aggiungo "Un" e il codice corrispondente
                  (true (setq out (string out "Un" (multiplo (- tri 1)))))
            )
            (if (!= val "") ; se la tripletta vale "000" --> val = ""
              (setq out (string out val (multipli (- tri 1)))))
          )
          (-- tri)
          ;(println (triple el))
        )
        out))))
;----------------------------------------
(define (triple num)
  (local (lst res)
    (setq res "")
    ; lista delle cifre
    (setq lst (map int (explode (string num))))
    (dolist (el lst)
      (cond ((= el 0) nil)
            (true (cond ((= $idx 2) ; cifra unita ?
                          (if (!= 1 (lst 1)) ; ultime 2 cifre > 19 ?
                              (setq res (append res (cifre el)))))
                        ((= $idx 1) ; cifra decine ?
                          (if (= el 1) ; ultime 2 cifre < 20 ?
                            ; prendo il numero da 11 a 19
                            (setq res (append res (venti (+ 9 el (lst 2)))))
                            ; oppure prendo le decine
                            (if (or (= 1 (lst 2)) (= 8 (lst 2))) ; numero finisce con 1 o con 8?
                              ; prendo le decine senza vocale finale
                              (setq res (append res (dcn el)))
                              ; oppure prendo le decine con vocale finale
                              (setq res (append res (decine el))))))
                        ((= $idx 0) ; cifra centinaia ?
                          (if (= el 1) ; cifra centinaia = 1 ?
                              ; prendo solo "cento"
                              (setq res (append res cento))
                              ; prendo il numero e "cento"
                              (setq res (append res (venti el) cento))))))
      )
    )
    res))
;----------------------------------------
(define (pad num len ch)
  (local (out)
    (setq out (string num))
    (while (> len (length out))
      (setq out (string ch out)))
  out))
;---------------------------------------------------------------------------

Funzione che converte un intero positivo nel relativo ordinale in lettere:

(define (ordinale num)
  (cond ((= num 1) "Primo")
        ((= num 2) "Secondo")
        ((= num 3) "Terzo")
        ((= num 4) "Quarto")
        ((= num 5) "Quinto")
        ((= num 6) "Sesto")
        ((= num 7) "Settimo")
        ((= num 8) "Ottavo")
        ((= num 9) "Nono")
        ((= num 10) "Decimo")
        ((= num 13) "Tredicesimo")
        ((= num 16) "Sedicesimo")
        (true
          (let ( (last-digit (% num 10))
                 (lettere (numero num)) )
            (if (and (!= last-digit 3) (!= last-digit 6))
              (pop lettere -1))
            (extend lettere "esimo")))))

Proviamo:

(ordinale 16)
;-> "Sedicesimo"
(ordinale 42)
;-> "QuarantaDuesimo"

(map ordinale (sequence 1 21))
;-> ("Primo" "Secondo" "Terzo" "Quarto" "Quinto" "Sesto" "Settimo" "Ottavo"
;->  "Nono" "Decimo" "Undicesimo" "Dodicesimo" "Tredicesimo" "Quattordicesimo"
;->  "Quindicesimo" "Sedicesimo" "Diciassettesimo" "Diciottesimo"
;->  "Diciannovesimo" "Ventesimo" "VentUnesimo")

Calcoliamo la sequenza OEIS:

(map (fn(x) (length (ordinale x))) (sequence 1 50))
;-> (5 7 5 6 6 5 7 6 4 6 10 10 11 15 12 10 15 12 14 9 11
;->  12 13 16 15 13 14 12 13 10 12 13 14 17 16 14 15 13 14
;->  12 14 15 16 19 18 16 17 15 16 13)


-----------------------------------------
Griglie e sensori (Distanza di Chebyshev)
-----------------------------------------

Abbiamo una griglia N × M e un numero intero K.
Un sensore posizionato sulla cella (r, c) copre tutte le celle la cui distanza di Chebyshev da (r, c) è al massimo K.
La distanza di Chebyshev tra due celle (r1, c1) e (r2, c2) è max(|r1 − r2|,|c1 − c2|).
Il numero minimo di sensori necessari per coprire una griglia N x M con raggio di copertura K si trova calcolando separatamente il numero di sensori necessari per righe e colonne e poi moltiplicandoli.
La formula è: ceil(N/(2*K+1)) * ceil(M/(2*K+1))
Questo perché ogni sensore copre un'area quadrata di lato 2*K + 1 sotto la distanza di Chebyshev.

Funzione che restituisce il numero minimo di sensori necessari per coprire ogni cella della griglia:

(define (sensori N M K)
  (letn ( (copertura (+ (* 2 K) 1))
          ;(sensori-riga (/ (+ N copertura -1) copertura))
          (sensori-riga (ceil (div N copertura)))
          ;(sensori-colonna (/ (+ M copertura -1) copertura))
          (sensori-colonna (ceil (div M copertura))) )
    (int (mul sensori-riga sensori-colonna))))

Proviamo:

(sensori 3 3 1)
;-> 1

(sensori 5 5 1)
;-> 4

(sensori 7 10 2)
;-> 4

(sensori 8 11 2)
;-> 6

Adesso calcoliamo le posizioni (r, c) di ogni sensore.
Dato che ogni sensore copre un quadrato di lato (2*K + 1), possiamo distribuirli in modo uniforme con passo 'copertura' = 2*K + 1 lungo righe e colonne, a partire dalla cella (K, K) (cioè centrando il primo sensore nel primo quadrato coperto).
La posizione dei sensori sarà quindi:

  r = (K, 3*K+1, 5*K+2, ... ) fino a < N
  c = (K, 3*K+1, 5*K+2, ... ) fino a < M

ossia con incremento di 'copertura' in entrambe le direzioni.

Quindi:
- 'copertura' = 2*K + 1 è la dimensione del quadrato coperto da un sensore.
- Ogni sensore successivo si posiziona a distanza 'copertura' per evitare sovrapposizioni inutili.
- I sensori sono posizionati al centro dei blocchi di copertura.

Nel caso in cui N o M non sono multipli esatti di 'copertura', allora piazziamo sensori fino a coprire l'intera griglia.
Per farlo, basta controllare se l'ultimo sensore non copre fino al bordo della griglia, e in tal caso aggiungerne uno finale centrato vicino all'ultimo bordo.

(define (posizioni-sensori N M K)
  (letn ( (copertura (+ (* 2 K) 1))
          (righe '())
          (colonne '())
          (posizioni '())
          (r 0) (c 0) )
    ; calcola gli indici centrali dei sensori lungo righe
    (for (r K (- N 1) copertura)
      (push r righe -1))
    ; se l'ultima copertura non arriva al fondo,
    ; allora aggiunge un sensore vicino al bordo
    (if (< (+ (last righe) K) (- N 1))
        (push (- N K 1) righe -1))
    ; calcola gli indici centrali dei sensori lungo le colonne
    (for (c K (- M 1) copertura)
      (push c colonne -1))
    ; se l'ultima copertura non arriva al fondo,
    ; allora aggiunge un sensore vicino al bordo
    (if (< (+ (last colonne) K) (- M 1))
        (push (- M K 1) colonne -1))
    ; --- combina tutte le coppie (r,c) ---
    (dolist (r righe)
      (dolist (c colonne)
        (push (list r c) posizioni -1)))
    posizioni))

Proviamo:

(posizioni-sensori 3 3 1)
;-> ((1 1))

(posizioni-sensori 5 5 1)
;-> ((1 1) (1 4) (4 1) (4 4))

(posizioni-sensori 7 10 2)
;-> ((2 2) (2 7) (4 2) (4 7))

(posizioni-sensori 8 11 2)
;-> ((2 2) (2 7) (2 8) (7 2) (7 7) (7 8))

Scriviamo una funzione che stampa la griglia e i sensori:

(define (print-grid N M K sensors)
  (letn ( (grid (array N M '("."))) ) ; creazione della griglia
    ; per ogni sensore: marca le celle coperte
    ; cioè marca le celle del quadrato di lato (2*K + 1) centrato in (r c)
    (dolist (el sensors)
      (letn ( (r (el 0)) (c (el 1)) )
        (for (dr (- K) K)
          (for (dc (- K) K)
            (letn ( (nr (+ r dr)) (nc (+ c dc)) )
              (when (and (>= nr 0) (< nr N) (>= nc 0) (< nc M))
                (setf (grid nr nc) "*")))))
        ; centro = 1
        (setf (grid r c) "1")))
    ; stampa la griglia
    (for (r 0 (- N 1))
      (for (c 0 (- M 1))
        (print (format "%2s " (grid r c))))
      (println)))'>)

Proviamo:

(print-grid 6 7 2 '((4 3)))
;->  .  .  .  .  .  .  .
;->  .  .  .  .  .  .  .
;->  .  *  *  *  *  *  .
;->  .  *  *  *  *  *  .
;->  .  *  *  1  *  *  .
;->  .  *  *  *  *  *  .

(print-grid 8 11 2 (posizioni-sensori 8 11 2))
;->  *  *  *  *  *  *  *  *  *  *  *
;->  *  *  *  *  *  *  *  *  *  *  *
;->  *  *  1  *  *  *  *  *  1  *  *
;->  *  *  *  *  *  *  *  *  *  *  *
;->  *  *  *  *  *  *  *  *  *  *  *
;->  *  *  *  *  *  *  *  *  *  *  *
;->  *  *  *  *  *  *  *  *  *  *  *
;->  *  *  1  *  *  *  *  *  1  *  *

Se abbiamo degli ostacoli nella griglia che impediscono ai sensori di 'vedere oltre', il problema cambia completamente.
Nota: un l'ostacolo blocca la visone di un sensore solo in riga, in colonna e nelle diagonali a 45 gradi.

Vediamo semplicemente una funzione che permette di calcolare le celle di copertura di un sensore in una griglia con ostacoli nei casi:
1) K rappresenta il lato del quadrato di copertura del sensore (copertura quadrata)
2) K rappresenta il raggio del cerchio (approssimato) di copertura del sensore (copertura circolare)
3) K non viene fornito e la visione è fino al bordo

; Funzione che restituisce tutte le celle coperte da un sensore in posizione (r c)
; su una griglia NxM, considerando eventuali ostacoli e un parametro K che definisce
; il tipo e la portata della copertura:
; 1) Se K rappresenta il lato del quadrato -> copertura quadrata
; 2) Se K rappresenta il raggio -> copertura circolare (approssimata)
; 3) Se K è nil -> visione fino al bordo
; Gli ostacoli bloccano la visione lungo righe, colonne e diagonali a 45°.
(define (celle-coperte N M ostacoli r c K)
  (letn ((celle '())                                      ; lista delle celle coperte
         (direzioni '((-1 0) (1 0) (0 -1) (0 1) (-1 -1) (-1 1) (1 -1) (1 1))) ) ; 8 direzioni
    (push (list r c) celle -1)                            ; aggiunge la cella del sensore
    (dolist (d direzioni)
      (letn ((dr (d 0)) (dc (d 1)) (rr r) (cc c) (finito nil))
        (while (not finito)
          (setq rr (+ rr dr))
          (setq cc (+ cc dc))
          (cond
            ((or (< rr 0) (>= rr N) (< cc 0) (>= cc M)) (setq finito true)) ; fuori bordo
            ((ref (list rr cc) ostacoli) (setq finito true))                ; ostacolo blocca visione
            (true
              (cond
                ; Caso 1: copertura quadrata
                ((and (integer? K) (>= K 1))
                  (if (and (<= (abs (- rr r)) (/ (- K 1) 2))
                           (<= (abs (- cc c)) (/ (- K 1) 2)))
                      (push (list rr cc) celle -1)
                      (setq finito true)))
                ; Caso 2: copertura circolare (approssimata)
                ((and (float? K) (>= K 1))
                  (if (<= (sqrt (+ (mul (- rr r) (- rr r)) (mul (- cc c) (- cc c)))) K)
                      (push (list rr cc) celle -1)
                      (setq finito true)))
                ; Caso 3: K non fornito -> visione fino al bordo
                ((nil? K) (push (list rr cc) celle -1))
                (true (setq finito true))))))))
    celle))

Proviamo:

(celle-coperte 5 5 '((1 1) (3 2)) 2 2 4)
;-> ((2 2) (1 2) (2 1) (2 3) (1 3) (3 1) (3 3))


--------------------
Codifica di N numeri
--------------------

Abbiamo N numeri interi positivi num1, num2, ..., numN.
La codifica degli N numeri è un numero intero definita nel modo seguente:
1) se i numeri non sono della stessa lunghezza si aggiungono degli zeri iniziali fino a raggiungere la lunghezza massima.
2) La i-esima cifra (1 <= i <= N) del codice è data dalla cifra più piccola tra le i-esime cifre degli N numeri.
Il valore massimo dei numeri vale 1 bilione = 1000 miliardi = 10^12 = 1.000.000.000.000.

Esempio:
  N = 3
  Numeri = (287 34 99)
  1) padding con 0 iniziali: 287 034 099
  2) cifre minime: 2 8 7
                   0 3 4
                   0 9 9
                   -----
                   0 3 4
  Codice: 34 (senza zeri iniziali)

Scrivere una funzione che prende una lista di numeri N interi positivi e restituisce il codice degli N numeri.

(define (codice nums)
  (apply + (map * (map (fn(x) (apply min x))
                       (transpose
                        (map (fn(x) (map int (explode (format "%012s" (string x))))) nums)))
                  (reverse (series 1 10 12)))))

Proviamo:

(codice '(287 34 99))
;-> 34

(codice '(987 879 798 749))
;-> 747

Come funziona:
;converte i numeri in stringa
(string x) --> ("287" "34" "99")
; pad con 12 zeri
(format "%012s" --> ("000000000287" "000000000034" "000000000099")
; converte le stringhe in caratteri
(explode '("000000000287" "000000000034" "000000000099"))
--> (("0" "0" "0" "0" "0" "0" "0" "0" "0" "2" "8" "7")
-->  ("0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "3" "4")
-->  ("0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "9" "9"))
; converte i caratteri in interi
int
--> ((0 0 0 0 0 0 0 0 0 2 8 7)
-->  (0 0 0 0 0 0 0 0 0 0 3 4)
-->  (0 0 0 0 0 0 0 0 0 0 9 9))
; calcola la trasposta
(transpose '((0 0 0 0 0 0 0 0 0 2 8 7)
             (0 0 0 0 0 0 0 0 0 0 3 4)
             (0 0 0 0 0 0 0 0 0 0 9 9)))
--> ((0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0)
--> (0 0 0) (0 0 0) (0 0 0) (2 0 0) (8 3 9) (7 4 9))
; prende il minimo di ogni elemento
(map (fn(x) (apply min x)) '((0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0)
 (0 0 0) (0 0 0) (0 0 0) (2 0 0) (8 3 9) (7 4 9)))
--> (0 0 0 0 0 0 0 0 0 0 3 4)
; moltiplica questa lista con (reverse (series 1 10 12)):
(map * '(0 0 0 0 0 0 0 0 0 0 3 4)
       '(100000000000 10000000000 1000000000 100000000 10000000 1000000
         100000 10000 1000 100 10 1))
--> (0 0 0 0 0 0 0 0 0 0 30 4)
; somma tutti gli elementi
(apply + '(0 0 0 0 0 0 0 0 0 0 30 4))
--> 34

Versione code-golf (one-line): 147 caratteri

(define(f N)
(apply +(map *(map(fn(x)(apply min x))
(transpose(map(fn(x)(map int(explode(format "%012s"(string x)))))N)))
(reverse(series 1 10 12)))))

(f'(287 34 99))
;-> 34

(f '(987 879 798 749))
;-> 747


---------------------------
Tornei con sistema svizzero
---------------------------

Il sistema svizzero è un metodo di gestione di tornei (usato negli scacchi, go, bridge, ecc.) pensato per ridurre il numero di turni necessari per classificare i giocatori, evitando eliminazioni dirette.

Regole fondamentali
-------------------
1. Tutti i giocatori partecipano a ogni turno.
   Nessuno viene eliminato.
2. I giocatori vengono accoppiati con altri di punteggio simile.
   Dopo ogni turno, i partecipanti sono ordinati per punteggio e accoppiati a due a due.
3. Nessun incontro viene ripetuto.
   Ogni coppia di giocatori si affronta al massimo una volta.
4. Colori alternati (negli scacchi):
   Si cerca di bilanciare il numero di partite con il bianco e con il nero.
5. In caso di numero dispari di giocatori, uno riceve un "bye" (un punto automatico, senza giocare).

Punteggi
--------
- Vittoria -> 1 punto
- Patta -> 0.5 punti
- Sconfitta -> 0 punti
- Bye -> 1 punto

Il programma simula un torneo svizzero completo, generando accoppiamenti, risultati casuali e aggiornando la classifica dopo ogni turno.

;;;'ultimo-colore'
(define (ultimo-colore g)
  (if (and (list? g) (>= (length g) 4) (list? (g 3)) (> (length (g 3)) 0))
      (last (g 3))
      nil))
Restituisce l'ultimo colore giocato da un giocatore 'g', se disponibile.
Serve per bilanciare il colore (bianco/nero) nel turno successivo.

;;;'accoppiamento-svizzero'
(define (accoppiamento-svizzero)
  (let ((sorted (sort giocatori (fn (a b) (> (a 1) (b 1))))) (accoppiamenti '()))
    ...
    accoppiamenti))
Crea gli accoppiamenti del turno secondo il sistema svizzero.

Funzionamento:
- I giocatori ('giocatori') sono ordinati per punteggio decrescente.
- Si prende il primo ('g1') e si cerca un avversario ('g2') che:
  - non sia già stato incontrato ('not (ref (cand 0) (g1 2))'),
  - sia disponibile nella lista ordinata.
- Se non ci sono avversari validi -> 'g1' ottiene un bye.
- Quando si trova 'g2', viene rimosso dalla lista dei disponibili.
- Il colore (B/N) viene scelto cercando di:
  1. Bilanciare il numero di partite giocate con bianco e nero,
  2. Alternare il colore rispetto all'ultimo turno.
L'output è una lista del tipo:
  ((G1 G2 Colore1 Colore2) (G3 'bye) ...)

;;;'aggiorna-colori'
(define (aggiorna-colori accoppiamenti)
  (dolist (a accoppiamenti)
    (when (and (list? a) (>= (length a) 4))
      ...
      (setf ((giocatori i) 3) (append (g 3) (list c1))))))
Aggiunge i colori appena giocati (B/N) alla cronologia di ciascun giocatore.
Ignora i bye.

;;;'aggiorna-punteggi'
(define (aggiorna-punteggi risultati)
  (dolist (r risultati)
    (letn ((g1 (r 0)) (g2 (r 1)) (ris (r 2))) ...)))
Aggiorna i punteggi dei giocatori dopo ogni turno.

Funzionamento:
- Ogni elemento di 'risultati' è del tipo '(Giocatore1 Giocatore2 risultato)'.
- Se 'risultato = 1' -> 'Giocatore1' vince.
  Se 'risultato = 0.5' -> patta.
  Se 'risultato = 0' -> perde.
- Aggiorna anche la lista degli avversari affrontati per evitare ripetizioni.

;;;'simula-risultati'
(define (simula-risultati accoppiamenti)
  (let ((ris '()))
    ...
    (push (list (a 0) (a 1) (nth (rand 3) '(1 0.5 0))) ris)))
Genera risultati casuali per gli accoppiamenti.
Dettagli:
- '(rand 3)' sceglie casualmente un indice -> '1', '0.5' o '0'.
- I bye ricevono automaticamente un punto.

;;;'stampa-classifica'
(define (stampa-classifica)
  (println "\nClassifica:")
  (dolist (g (sort giocatori (fn (a b) (>= (a 1) (b 1)))))
    (println (format "%s - %.1f punti" (string (g 0)) (g 1)))))
Ordina e mostra la classifica corrente.

;;;'simula-turno'
(define (simula-turno n)
  (println (format "\n--- Turno %d ---" n))
  (setq acc (accoppiamento-svizzero))
  (setq risultati (simula-risultati acc))
  ...
  (aggiorna-punteggi risultati)
  (aggiorna-colori acc)
  (stampa-classifica))
Simula un turno completo del torneo:
1. Genera accoppiamenti.
2. Simula risultati.
3. Stampa le partite e i risultati.
4. Aggiorna punteggi e colori.
5. Mostra la nuova classifica.

;;;'simula-torneo'
(define (simula-torneo turni)
  (for (t 1 turni) (simula-turno t))
  (println "\nTorneo terminato!")
  (stampa-classifica)
  giocatori)
Gestisce l'intero torneo per 'turni' turni consecutivi, richiamando 'simula-turno' e stampando la classifica finale.
Ogni giocatore è rappresentato come:
  (Nome punteggio avversari colori)
E al termine, il sistema mostra sia la classifica sia la cronologia di ciascun giocatore.

Riassunto
---------
| Funzione                 | Scopo principale                                  |
| ------------------------ | ------------------------------------------------- |
| 'ultimo-colore'          | Controlla l'ultimo colore giocato                 |
| 'accoppiamento-svizzero' | Crea accoppiamenti evitando ripetizioni           |
| 'aggiorna-colori'        | Aggiorna la lista dei colori di ciascun giocatore |
| 'aggiorna-punteggi'      | Somma i punti ottenuti e registra avversari       |
| 'simula-risultati'       | Genera esiti casuali                              |
| 'stampa-classifica'      | Mostra la graduatoria ordinata                    |
| 'simula-turno'           | Gestisce un turno completo                        |
| 'simula-torneo'          | Simula l'intero torneo                            |

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SIMULAZIONE TORNEO CON SISTEMA SVIZZERO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DESCRIZIONE GENERALE
;;
;; Il sistema svizzero è una modalità di torneo utilizzata in scacchi e giochi
;; da tavolo per gestire competizioni con molti giocatori e pochi turni.
;;
;; Regole principali:
;; 1. Tutti i giocatori partecipano a ogni turno (non ci sono eliminazioni).
;; 2. I giocatori vengono accoppiati in base al punteggio attuale: chi ha
;;    punteggi simili gioca contro altri con punteggi simili.
;; 3. Nessun giocatore può affrontare lo stesso avversario più di una volta.
;; 4. Quando il numero di partecipanti è dispari, uno riceve un "bye" (vince
;;    automaticamente il turno con 1 punto).
;; 5. Si cerca di alternare i colori (B/N) nel caso degli scacchi.
;;
;; Struttura dei dati:
;; Ogni giocatore è rappresentato da una lista:
;; (ID punteggio avversari colori)
;;
;; Dove:
;;  - ID = simbolo identificativo (es. A, B, C...)
;;  - punteggio = totale dei punti ottenuti
;;  - avversari = lista dei giocatori già affrontati
;;  - colori = lista dei colori giocati (B o N)
;;
;; Esempio:
;; (A 2.5 (B C D) (B N B))
;; -> Il giocatore A ha 2.5 punti, ha giocato contro B, C, D e ha usato i colori
;;   Bianco, Nero, Bianco nei tre turni.
;;
;; Struttura del programma:
;; - ultimo-colore      -> restituisce l'ultimo colore giocato da un giocatore
;; - accoppiamento-svizzero -> genera gli accoppiamenti del turno
;; - aggiorna-colori    -> aggiorna la cronologia dei colori dei giocatori
;; - aggiorna-punteggi  -> assegna i punti in base ai risultati
;; - simula-risultati   -> genera esiti casuali (vittoria, pareggio, sconfitta)
;; - stampa-classifica  -> mostra la classifica corrente
;; - simula-turno       -> esegue un turno completo (accoppiamento + risultati)
;; - simula-torneo      -> esegue tutti i turni del torneo
;;
;; Esempio di simulazione:
;; (simula-torneo 4)
;; Simula un torneo di 4 turni con abbinamenti, punteggi e colori automatici.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Funzione che restituisce l'ultimo colore usato da un giocatore
(define (ultimo-colore g)
  (if (and (list? g) (>= (length g) 4) (list? (g 3)) (> (length (g 3)) 0))
      (last (g 3))
      nil))

;; Funzione principale per l'accoppiamento dei giocatori secondo il sistema svizzero
(define (accoppiamento-svizzero)
  ;; Ordina i giocatori per punteggio decrescente e prepara la lista degli accoppiamenti
  (let ((sorted (sort giocatori (fn (a b) (> (a 1) (b 1))))) (accoppiamenti '()))
    ;; Finché ci sono giocatori da accoppiare
    (while (> (length sorted) 0)
      ;; Estrae il primo (miglior) giocatore
      (let ((g1 (pop sorted)))
        ;; Se è rimasto solo, ottiene un bye
        (if (= (length sorted) 0)
            (push (list (g1 0) 'bye) accoppiamenti)
            ;; Altrimenti cerca un avversario valido
            (let ((g2 nil) (idx -1) (len (length sorted)))
              ;; Scorre i rimanenti per trovare qualcuno che non ha già incontrato
              (for (j 0 (- len 1))
                (let ((cand (sorted j)))
                  (when (not (ref (cand 0) (g1 2)))
                    (setq g2 cand)
                    (setq idx j)
                    (setq j len))))
              ;; Se non trova nessuno, assegna un bye
              (if (nil? g2)
                  (push (list (g1 0) 'bye) accoppiamenti)
                  ;; Altrimenti crea l'accoppiamento
                  (begin
                    ;; Rimuove l'avversario scelto dalla lista
                    (let ((newsorted '()))
                      (for (k 0 (- (length sorted) 1))
                        (unless (= k idx) (push (sorted k) newsorted)))
                      (setq sorted (reverse newsorted)))
                    ;; Determina i colori da assegnare ai due giocatori
                    (letn ((ultimo1 (ultimo-colore g1))
                           (bcount1 (count '(B) (g1 3)))
                           (ncount1 (count '(N) (g1 3)))
                           (colore1 nil) (colore2 nil))
                      ;; Assegna colore opposto a quello più frequente
                      (cond
                        ((> bcount1 ncount1) (setq colore1 'N colore2 'B))
                        ((> ncount1 bcount1) (setq colore1 'B colore2 'N))
                        ;; Se uguali, alterna rispetto all'ultimo colore
                        (true
                          (if (= ultimo1 'B) (setq colore1 'N colore2 'B)
                                             (setq colore1 'B colore2 'N))))
                      ;; Salva l'accoppiamento con i colori assegnati
                      (push (list (g1 0) (g2 0) colore1 colore2) accoppiamenti))))))))
    ;; Restituisce la lista degli accoppiamenti
    accoppiamenti))

;; Aggiorna i colori dei giocatori dopo il turno (ignora i bye)
(define (aggiorna-colori accoppiamenti)
  (dolist (a accoppiamenti)
    (when (and (list? a) (>= (length a) 4))
      (letn ((g1 (a 0)) (g2 (a 1)) (c1 (a 2)) (c2 (a 3)))
        ;; Aggiorna i colori giocati da ciascun giocatore
        (for (i 0 (- (length giocatori) 1))
          (let ((g (giocatori i)))
            (when (= (g 0) g1)
              (setf ((giocatori i) 3) (append (g 3) (list c1))))
            (when (= (g 0) g2)
              (setf ((giocatori i) 3) (append (g 3) (list c2))))))))))

;; Aggiorna i punteggi e i giocatori già incontrati
(define (aggiorna-punteggi risultati)
  (dolist (r risultati)
    (letn ((g1 (r 0)) (g2 (r 1)) (ris (r 2)))
      ;; Aggiorna punteggi e avversari per ciascun giocatore
      (for (i 0 (- (length giocatori) 1))
        (let ((g (giocatori i)))
          (cond
            ;; Caso del primo giocatore
            ((= (g 0) g1)
              (setf ((giocatori i) 1) (+ (g 1) ris))
              (when (and g2 (not (= g2 'bye)))
                (setf ((giocatori i) 2) (append (g 2) (list g2)))))
            ;; Caso del secondo giocatore
            ((and g2 (not (= g2 'bye)) (= (g 0) g2))
              (setf ((giocatori i) 1) (+ (g 1) (- 1 ris)))
              (setf ((giocatori i) 2) (append (g 2) (list g1))))))))))

;; Genera risultati casuali per ogni accoppiamento
(define (simula-risultati accoppiamenti)
  (let ((ris '()))
    (dolist (a accoppiamenti)
      ;; Gestisce i bye (1 punto automatico)
      (cond
        ((= (length a) 2)
          (push (list (a 0) 'bye 1) ris))
        ;; Altrimenti genera risultato casuale tra 1, 0.5 e 0
        ((= (length a) 4)
          (push (list (a 0) (a 1) (nth (rand 3) '(1 0.5 0))) ris))))
    ris))

;; Stampa la classifica corrente
(define (stampa-classifica)
  (println "\nClassifica:")
  (dolist (g (sort giocatori (fn (a b) (>= (a 1) (b 1)))))
    (println (format "%s - %.1f punti" (string (g 0)) (g 1)))))

;; Simula un turno completo (accoppiamento, risultati, aggiornamenti)
(define (simula-turno n)
  (println (format "\n--- Turno %d ---" n))
  (setq acc (accoppiamento-svizzero))
  (setq risultati (simula-risultati acc))
  ;; Stampa gli incontri del turno
  (dolist (a acc)
    (cond
      ;; Caso del bye
      ((= (length a) 2)
        (println (format "%s ha il bye (+1 punto)" (string (a 0)))))
      ;; Caso partita normale
      ((= (length a) 4)
        (letn ((idx (ref (list (a 0) (a 1))
                         (map (fn (x) (list (x 0) (x 1))) risultati)))
               (ris (if idx ((risultati idx) 2) "?")))
          (println (format "%s (%s) vs %s (%s) -> risultato: %s"
                           (string (a 0)) (string (a 2)) (string (a 1)) (string (a 3)) (string ris)))))))
  ;; Aggiorna i dati dei giocatori
  (aggiorna-punteggi risultati)
  (aggiorna-colori acc)
  (stampa-classifica))

;; Simula un intero torneo per un numero dato di turni
(define (simula-torneo turni)
  (for (t 1 turni) (simula-turno t))
  (println "\nTorneo terminato!")
  ;(stampa-classifica)
  (map println giocatori) '>)

;; ESEMPIO
(setq giocatori '(
  (A 0.0 () () )
  (B 0.0 () () )
  (C 0.0 () () )
  (D 0.0 () () )
  (E 0.0 () () )
  (F 0.0 () () )
  (G 0.0 () () )
))

(simula-torneo 4)
;-> --- Turno 1 ---
;-> D ha il bye (+1 punto)
;-> E (B) vs C (N) -> risultato: 1
;-> F (B) vs B (N) -> risultato: 0.5
;-> G (B) vs A (N) -> risultato: 0.5
;->
;-> Classifica:
;-> E - 1.0 punti
;-> D - 1.0 punti
;-> B - 1.0 punti
;-> A - 1.0 punti
;-> G - 0.0 punti
;-> F - 0.0 punti
;-> C - 0.0 punti
;->
;-> --- Turno 2 ---
;-> E ha il bye (+1 punto)
;-> D (B) vs C (N) -> risultato: 0.5
;-> B (B) vs G (N) -> risultato: 0.5
;-> A (B) vs F (N) -> risultato: 0.5
;->
;-> Classifica:
;-> E - 2.0 punti
;-> A - 1.0 punti
;-> B - 1.0 punti
;-> D - 1.0 punti
;-> C - 1.0 punti
;-> F - 1.0 punti
;-> G - 1.0 punti
;->
;-> --- Turno 3 ---
;-> B ha il bye (+1 punto)
;-> F (B) vs C (N) -> risultato: 1
;-> G (B) vs D (N) -> risultato: 0.5
;-> E (N) vs A (B) -> risultato: 0.5
;->
;-> Classifica:
;-> E - 2.0 punti
;-> F - 2.0 punti
;-> D - 2.0 punti
;-> B - 2.0 punti
;-> A - 2.0 punti
;-> G - 1.0 punti
;-> C - 1.0 punti
;->
;-> --- Turno 4 ---
;-> G ha il bye (+1 punto)
;-> D (B) vs F (N) -> risultato: 0.5
;-> B (N) vs E (B) -> risultato: 1
;-> A (N) vs C (B) -> risultato: 0.5
;->
;-> Classifica:
;-> B - 3.0 punti
;-> F - 3.0 punti
;-> A - 2.0 punti
;-> D - 2.0 punti
;-> E - 2.0 punti
;-> C - 2.0 punti
;-> G - 2.0 punti
;->
;-> Torneo terminato!
;-> (B 3 (F G E) (N B N))
;-> (F 3 (B A C D) (B N B N))
;-> (A 2 (G F E C) (N B B N))
;-> (D 2 (C G F) (B N B))
;-> (E 2 (C A B) (B N B))
;-> (C 2 (E D F A) (N N N B))
;-> (G 2 (A B D) (B N B))

Nota: questo è solo un programma di base per la gestione di un torneo con sistema svizzero.
Per la gestione di un torneo vero e proprio consiglio di utilizzare programmi specifici.


----------------------------------------------
Triangolo con perimetro massimo e area massima
----------------------------------------------

Data una lista di punti 2D con coordinate intere e positive determinare il triangolo con perimetro massimo e il triangolo con area massima.

Algoritmo
---------
1) Generare tutte le combinazioni di 3 punti con la lista dei punti.
   Ogni combinazione rappresenta un triangolo.
2) Ciclo su tutti i triangoli
   Calcolare area e perimetro del triangolo corrente
   Aggiornare area massima e perimetro massimo (e relativi triangoli)
3) Restituire i risultati

Nota: quando aggiorniamo il perimetro massimo dobbiamo verificare che il triangolo corrente sia un triangolo valido, cioè che abbia un'area maggiore di 0 (altrimenti sono tre punti allineati).

(define (comb k lst (r '()))
"Generate all combinations of k elements without repetition from a list of items"
  (if (= (length r) k)
    (list r)
    (let (rlst '())
      (dolist (x lst)
        (extend rlst (comb k ((+ 1 $idx) lst) (append r (list x)))))
      rlst)))

; Funzione che calcola l'area di un triangolo dati i 3 vertici (x1 y1), (x2 y2), (x3 y3)
; Usa la formula determinante:
; area = |x1*(y2 - y3) + x2*(y3 - y1) + x3*(y1 - y2)| / 2
(define (area-triangolo x1 y1 x2 y2 x3 y3)
  (abs (div (add (mul x1 (sub y2 y3))
                 (mul x2 (sub y3 y1))
                 (mul x3 (sub y1 y2))) 2)))

Nota: nel caso di 3 punti allineati l'area vale 0.

; Funzione che calcola il perimetro di un triangolo dati i 3 vertici (x1 y1), (x2 y2), (x3 y3)
; Usa la formula della distanza euclidea tra i vertici: sqrt((x2-x1)^2 + (y2-y1)^2)
; Perimetro = lato1 + lato2 + lato3
(define (perimetro-triangolo x1 y1 x2 y2 x3 y3)
  (letn ( (l1 (sqrt (add (mul (sub x2 x1) (sub x2 x1))
                         (mul (sub y2 y1) (sub y2 y1)))))
          (l2 (sqrt (add (mul (sub x3 x2) (sub x3 x2))
                         (mul (sub y3 y2) (sub y3 y2)))))
          (l3 (sqrt (add (mul (sub x1 x3) (sub x1 x3))
                         (mul (sub y1 y3) (sub y1 y3))))) )
    (add l1 l2 l3)))

Funzione che calcola il triangolo con area massima e il triangolo con area minima di una lista di punti 2D:

(define (triangolo-massimo pts)
  (local (area-max triangolo-area-max perimetro-max triangolo-perimetro-max
          triangoli area perimetro)
  (setq area-max -1) ; valore area massima
  (setq triangolo-area-max '()) ; punti del triangolo con area massima
  (setq perimetro-max -1) ; valore perimetro massimo
  (setq triangolo-perimetro-max '()) ; punti del triangolo con perimetro massimo
  ; crea la lista di tutti i triangoli utilizzando i punti della lista
  (setq triangoli (comb 3 pts))
  ; ciclo per calcolare area e perimetro di ogni triangolo...
  (dolist (tri triangoli)
    ; calcolo area del triangolo corrente
    (setq area (apply area-triangolo (flat tri)))
    ; aggiornamento area massima
    (when (> area area-max)
      (setq area-max area)
      (setq triangolo-area-max tri))
    ; calcolo perimetro del triangolo corrente
    (setq perimetro (apply perimetro-triangolo (flat tri)))
    ; aggiornamento perimetro massimo
    ; controllare che sia un triangolo valido (area > 0)
    (when (and (> area 0) (> perimetro perimetro-max))
      (setq perimetro-max perimetro)
      (setq triangolo-perimetro-max tri))
  )
  (println "Area massima = " area-max)
  (println "Triangolo = " triangolo-area-max)
  (println "Perimetro massimo = " perimetro-max)
  (println "Triangolo = " triangolo-perimetro-max) '>))

Proviamo:

(setq pts '((1 1) (1 2) (3 2) (3 3) (4 5) (3 5)))
(triangolo-massimo pts)
;-> Area massima = 3
;-> Triangolo = ((1 1) (3 2) (3 5))
;-> Perimetro massimo = 10.47213595499958
;-> Triangolo = ((1 1) (4 5) (3 5))

(setq pts (map list (rand 100 100) (rand 100 100)))
(triangolo-massimo pts)
;-> Area massima = 3827
;-> Triangolo = ((99 11) (62 95) (7 13))
;-> Perimetro massimo = 288.8465489274908
;-> Triangolo = ((23 95) (99 11) (7 13))

Vedi anche "Triangoli in una lista" su "Note libere 19".


--------------------
Il barista solitario
--------------------

Un bar ha un solo barista che può servire un cliente alla volta.
Data una lista di clienti in cui ogni elemento cliente(i) = (arrivo, tempo) rappresenta:
  arrivo: tempo di arrivo dell'i-esimo cliente (ordinato non decrescente)
  tempo: quanto tempo ci vuole per servire l'i-esimo cliente
Il processo lavorativo è il seguente:
Quando un cliente arriva, l'ordine viene inserito immadiatamente.
- Il barista inizia a servire solo quando non sta servendo un altro cliente.
- I clienti devono attendere che il loro ordine sia completamente preparato.
- Il barista deve servire i clienti nell'ordine in cui appaiono.
- Il tempo di attesa per ciascun cliente è calcolato come:
  l'ora in cui il suo ordine è completato meno l'ora del suo arrivo.

Calcolare il tempo di attesa di ogni cliente e il tempo di attesa medio.

Per risolvere il problema dobbiamo monitorare il "tempo di disponibilità" del barista durante tutto il processo lavorativo.
Quando arriva un nuovo cliente, possono verificarsi due casi:
1) Il barista è inattivo: cioè ha terminato l'ordine precedente prima dell'arrivo del cliente attuale e quindi può iniziare a cucinare immediatamente.
Il nuovo tempo di disponibilità dello chef diventa (tempo-arrivo + tempo-servizio).
2) Il barista è occupato: cioè sta ancora cucinando quando arriva il cliente e quindi quest'ultimo deve attendere.
Il barista inizia questo ordine subito dopo aver terminato quello precedente, quindi il suo nuovo tempo di disponibilità diventa (tempo-corrente + tempo-servizio).

Possiamo rappresentare entrambi gli scenari con una singola espressione: t = max(t, tempo-arrivo) + tempo-servizio, dove t tiene traccia di quando il barista termina l'ordine attuale.
Infatti, se t > tempo-arrivo (barista occupato), usiamo t, mntre se (t <= tempo-arrivo) (barista inattivo), usiamo tempo-arrivo.
Per calcolare il tempo di attesa, ogni cliente attende dal suo arrivo fino al completamento dell'ordine.
Questo vale (t - tempo-arrivo) dopo l'elaborazione di ciascun cliente.

(define (bar clienti)
  (local (attesa tempo-corrente tempo-attesa)
    ; lista del tempo di attesa di ogni cliente
    (setq attesa '())
    ; tempo diponibile --> quando il barista finirà l'ordine corrente
    (setq tempo-corrente 0)
    ; ciclo per ogni cliente...
    (dolist (cl clienti)
      ; Il barista inizia a servire quando arriva il cliente o quando è libero,
      ; a seconda di quale evento si verifica più tardi.
      (setq tempo-corrente (+ (max tempo-corrente (cl 0)) (cl 1)))
      ; Tempo di attesa = tempo in cui l'ordine è pronto - tempo di arrivo
      (setq tempo-attesa (- tempo-corrente (cl 0)))
      ; Inserisce il tempo di attesa del cliente corrente
      ; nella lista dei tempi di attesa
      (push tempo-attesa attesa -1))
    (list attesa (div (apply add attesa) (length attesa)))))

Proviamo:

(bar '((1 2) (2 1) (2 2) (3 2)))
;-> (2 2 4 5)
;-> 3.25

(bar '((1 2) (2 1) (3 4)))
;-> ((2 2 5) 3)


---------------------
La sequenza Inventory
---------------------

A Number Sequence with Everything - Numberphile
https://www.youtube.com/watch?v=rBU9E-ZOZAI

Sequenza OEIS A342585:
Inventory sequence: record the number of zeros thus far in the sequence, then the number of ones thus far, then the number of twos thus far and so on, until a zero is recorded; the inventory then starts again, recording the number of zeros.
  0, 1, 1, 0, 2, 2, 2, 0, 3, 2, 4, 1, 1, 0, 4, 4, 4, 1, 4, 0, 5, 5, 4, 1, 6,
  2, 1, 0, 6, 7, 5, 1, 6, 3, 3, 1, 0, 7, 9, 5, 3, 6, 4, 4, 2, 0, 8, 9, 6, 4,
  9, 4, 5, 2, 1, 3, 0, 9, 10, 7, 5, 10, 6, 6, 3, 1, 4, 2, 0, 10, 11, 8, 6,
  11, 6, 9, 3, 2, 5, 3, 2, 0, 11, 11, 10, ...

Per iniziare, ci chiediamo: quanti termini zero ci sono?
Poiché non ci sono ancora termini nella sequenza, registriamo uno '0' e, dopo aver registrato uno '0', ricominciamo: Quanti termini zero ci sono?
Ora c'è uno 0, quindi registriamo un '1' e continuiamo.
Quanti 1 ci sono? Attualmente c'è un '1' nella sequenza, quindi registriamo un '1' e continuiamo.
Quanti 2 ci sono? Non ci sono ancora 2, quindi registriamo uno '0' e, dopo aver registrato uno 0, ricominciamo con la domanda "Quanti termini zero ci sono?" E così via.

(define (seq n)
  (let ( (valori '())        ; Lista che conterrà la sequenza
         (numero-corrente 0) ; Numero corrente da cercare
         (valore-corrente 0) ; Il primo numero da aggiungere (inizialmente 0)
       )
    ; Ciclo che genera la sequenza fino al numero richiesto di elementi (n)
    (for (i 0 (- n 1))
      ; Calcola il valore corrente come:
      ; il numero di occorrenze di 'numero-corrente' nella lista 'valori'
      (setq valore-corrente (length (ref-all numero-corrente valori)))
      ; Aggiunge il valore corrente alla fine della lista 'valori'
      (push valore-corrente valori -1)
      ; Se il valore corrente è 0...
      (if (zero? valore-corrente)
          ; Allora azzera il numero-corrente
          (setq numero-corrente 0)
          ; Altrimenti, incrementa il numero-corrente
          (++ numero-corrente)))
    ; Restituisce la lista dei valori della sequenza
    valori))

Proviamo:

(seq 85)
;-> (0 1 1 0 2 2 2 0 3 2 4 1 1 0 4 4 4 1 4 0 5 5 4 1 6
;->  2 1 0 6 7 5 1 6 3 3 1 0 7 9 5 3 6 4 4 2 0 8 9 6 4
;->  9 4 5 2 1 3 0 9 10 7 5 10 6 6 3 1 4 2 0 10 11 8 6
;->  11 6 9 3 2 5 3 2 0 11 11 10)


-----------------------------
Equazione a^3 + b^3 + c^3 = k
-----------------------------

L'equazione

  a^3 + b^3 + c^3 = k

è un classico problema di teoria dei numeri, noto come problema della somma di tre cubi (sum of three cubes problem).
L'obiettivo è trovare interi (a, b, c) tali che la somma dei loro cubi dia un numero prefissato (k).

L'equazione è un caso particolare delle equazioni diofantee, cioè equazioni in cui si cercano soluzioni intere.
La forma generale (a^3 + b^3 + c^3 = k) è particolarmente interessante perché:
- i cubi crescono rapidamente → lo spazio delle soluzioni è molto sparso;
- non esiste una formula generale per trovarle;
- alcune soluzioni sono enormi (centinaia o migliaia di cifre).

Numeri impossibili da scomporre come somma di 3 cubi:
ciascun cubo (x^3) mod 9 può assumere solo tre possibili resti:

  x^3 ≡ 0, 1, 8 (mod 9)

Poiché (8 ≡ -1), allora

  x^3 ≡ 0, -1, 1 (mod 9)

Quindi la somma (a^3 + b^3 + c^3) può dare solo i valori (0, +-1, +-2, +-3, +-4) modulo 9.
Allora, se k ≡ 4 o 5 (mod 9), non esiste alcuna soluzione intera.
Esempi impossibili: (k = 4, 5, 13, 14, 22, 23, ...)

Esempi:

| k   | Soluzione (a,b,c)                                              |
| --- | ---------------------------------------------------------------|
| 1   | (1, 0, 0)                                                      |
| 2   | (1, 1, 0)                                                      |
| 3   | (1, 1, 1)                                                      |
| 6   | (2, -1, -1)                                                    |
| 9   | (4, 4, -5)                                                     |
| 33  | a=8866128975287528,  b=-8778405442862239, c=-2736111468807040  | 2019
| 42  | a=80435758145817515, b=80435758145817515, c=-80538738812075974 | 2019

Queste ultime due soluzioni sono state trovate grazie a calcoli distribuiti su supercomputer.

L'equazione può essere riscritta come:

  a^3 + b^3 = k - c^3

Cioè, si cerca una coppia di cubi la cui somma sia un certo valore.
In pratica:
1. si fissano due variabili e si cerca la terza;
2. si riduce il problema a un cuboide ellittico (curva ellittica di Mordell tipo (x^3 + y^3 = k));
3. si utilizzano algoritmi di ricerca per valori residui modulari e ottimizzazione su GPU.

L'equazione (x^3 + y^3 = k) definisce una curva ellittica.
Molti strumenti moderni (come la discesa di Selmer o la teoria dei gruppi di Mordell–Weil) si applicano anche qui.
In effetti, la ricerca di soluzioni intere o razionali per (a^3 + b^3 + c^3 = k) è profondamente legata alla teoria delle curve ellittiche.
Per k da 1 a 100, tutte le soluzioni sono note tranne per pochi valori (alcuni ancora non risolti fino al 2024).
Gli ultimi numeri risolti sono stati proprio (k = 33) e (k = 42), trovati nel 2019 da Andrew Booker e Andrew Sutherland.

Ecco un semplice algoritmo di ricerca brute-force (solo per piccoli numeri):

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(define (somma-tre-cubi k limite)
  (for (a (- limite) limite)
    (for (b (- limite) limite)
      (for (c (- limite) limite)
        (when (= (+ (** a 3) (** b 3) (** c 3)) k)
            (println a "^3 + " b "^3 + " c "^3 = " k))))))

(time (somma-tre-cubi 6 100))
;-> -58^3 + -43^3 + 65^3 = 6
;-> -58^3 + 65^3 + -43^3 = 6
;-> -43^3 + -58^3 + 65^3 = 6
;-> -43^3 + 65^3 + -58^3 = 6
;-> -1^3 + -1^3 + 2^3 = 6
;-> -1^3 + 2^3 + -1^3 = 6
;-> 2^3 + -1^3 + -1^3 = 6
;-> 65^3 + -58^3 + -43^3 = 6
;-> 65^3 + -43^3 + -58^3 = 6
;-> 19420.574

Naturalmente inefficiente per valori grandi di k, ma utile per sperimentare con numeri piccoli.

Vediamo una versione ottimizzata che utilizza una hash-map e cerca soluzioni con a <= b <= c.

(define (somma-tre-cubi2 k limite passo)
  ; verifica dei numeri impossibili da scomporre come somma di 3 cubi
  (if (or (= (% k 9) 4) (= (% k 9) 5)) nil
      ; else
      (begin
        ; crea hash-map
        (new Tree 'tab)
        (letn ( (out '())
                (cubi (array (+ (* 2 limite) 1))) )
          ; precalcolo cubi
          (for (i (- limite) limite passo)
            (setf (cubi (+ i limite)) (* i i i)))
          ; tabella a^3 + b^3 con a <= b
          (for (a (- limite) limite passo)
            (for (b a limite passo)
              (tab (+ (cubi (+ a limite)) (cubi (+ b limite))) (list a b))))
          ; cerca soluzioni con a <= b <= c
          (for (c (- limite) limite passo)
            (letn ((t (- k (cubi (+ c limite)))) (pair (tab t)))
              (if (and pair (<= (pair 1) c))
                (push (list (pair 0) (pair 1) c) out -1))))
          ; elimina hash-map
          (delete 'tab)
          out))))

Proviamo:

(somma-tre-cubi2 22 100 1)
;-> nil

(time (println (somma-tre-cubi2 6 100 1)))
;-> ((-1 -1 2) (-58 -43 65))
;-> 15.622

(time (println (somma-tre-cubi2 9 100 1)))
;-> ((0 1 2))
;-> 31.231


----------------------
Numeri primi in base B
----------------------

Per verificare se un numero è primo non importa la base in cui è scritto: la primalità dipende dal valore numerico, non dalla rappresentazione.
Quindi, poichè un numero primo in base 10 è primo in qualunque base, per verificare se un numero in base B è primo possiamo convertirlo in base 10 e poi verificare la sua primalità.

Funzione che verifica se un numero in base B è primo

(define (is-prime? num B)
  (let (val (int num 0 B))
    (if (< val 2)
        nil
        (= 1 (length (factor val))))))

Prima convertiamo un numero (stringa) dalla base 'base' alla base 10:
(int num 0 base)

Poi verifichiamo se il numero in base 10 è primo:
(if (< val 2)
    nil
    (= 1 (length (factor val))))))

Proviamo:

(is-prime? "1F" 16)
;-> true
(is-prime? "10" 16)
;-> nil
(is-prime? "B" 16)
;-> true
(is-prime? "20" 10)
;-> nil
(is-prime? "17" 10)
;-> true
(is-prime? "1000" 2)
;-> nil
(is-prime? "1011" 2)
;-> true
(is-prime? "101" 2)
;-> true
(is-prime? "101" 2)
;-> true
(is-prime? "1011" 2)
;-> true
(is-prime? "1000" 2)
;-> nil

Nota: nella funzione 'int' il parametro base ha valore massimo 36.
Se abbiamo bisogno di un valore maggiore per la base possiamo usare la funzione seguente:

(define (baseN-base10 number-string base)
"Convert a number from base N (<=62) to base 10"
  (let ((charset "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
        (result 0)
        (len (length number-string)))
    (dolist (digit (explode number-string))
      (setq result (+ (* result base) (find digit charset)))
    )
    result))


-----------------
Primi di Paterson
-----------------

I primi di Paterson sono quei numeri primi che, se scritti in base 4 e poi reinterpretati in base 10, danno di nuovo numeri primi.

Esempio:
  Numero primo (base10) = 11
  11 (base 4) = 23
  23 è un numero primo in base 10, quindi 11 è un primo di Paterson

Sequenza OEIS A065722:
Primes that when written in base 4, then reinterpreted in base 10, again give primes.
  2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 37, 43, 47, 53, 61, 71, 73, 79, 83,
  97, 103, 107, 109, 113, 131, 149, 151, 157, 163, 167, 181, 191, 193, 197,
  227, 233, 241, 251, 277, 293, 307, 311, 313, 317, 349, 359, 373, 389, 401,
  419, 421, 433, 443, 449, 463, 467, 503, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (base10-baseN number base)
"Convert a number from base 10 to base N (<=62)"
  (let ((charset "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
        (result '())
        (quotient number))
    (while (>= quotient base)
      (push (charset (% quotient base)) result)
      (setq quotient (/ quotient base))
    )
    (push (charset quotient) result)
    (join result)))

Funzione che calcola la sequenza dei primi di Paterson:

(define (sequenza-paterson limite)
  (let (seq '(2))
    (for (num 3 limite 2)
      (if (and (prime? num) (prime? (int (base10-baseN num 4) 0 10)))
          (push num seq -1)))
    seq))

(sequenza-paterson 503)
(2 3 5 7 11 13 17 19 23 29 37 43 47 53 61 71 73 79 83
 97 103 107 109 113 131 149 151 157 163 167 181 191 193 197
 227 233 241 251 277 293 307 311 313 317 349 359 373 389 401
 419 421 433 443 449 463 467 503)

Funzione che calcola il rapporto tra il numero di primi di Paterson e il numero degli altri primi:

(define (paterson limite)
  (let ( (pat '(2)) (other '()) (len-pat 0) (len-other 0) )
    (for (num 3 limite 2)
      (if (and (prime? num) (prime? (int (base10-baseN num 4) 0 10)))
          (push num pat -1)
          ; else
          (if (prime? num) (push num other -1))))
    (setq len-pat (length pat))
    (setq len-other (length other))
    ; (println pat)
    (println "Numero primi di Paterson  = " len-pat)
    (println "Numero primi non Paterson = " len-other)
    (println "Rapporto = " (format "%2.4f" (div len-pat len-other)))))

Proviamo:

(paterson 100)
;-> Numero primi di Paterson  = 20
;-> Numero primi non Paterson = 5
;-> Rapporto = 4.0000

(paterson 1000)
;-> Numero primi di Paterson  = 94
;-> Numero primi non Paterson = 74
;-> Rapporto = 1.2703

(paterson 10000)
;-> Numero primi di Paterson  = 459
;-> Numero primi non Paterson = 770
;-> Rapporto = 0.5961

(paterson 100000)
;-> Numero primi di Paterson  = 2477
;-> Numero primi non Paterson = 7115
;-> Rapporto = 0.3481

(paterson 1000000)
;-> Numero primi di Paterson  = 16259
;-> Numero primi non Paterson = 62239
;-> Rapporto = 0.2612

(time (paterson 10000000))
;-> Numero primi di Paterson  = 116209
;-> Numero primi non Paterson = 548370
;-> Rapporto = 0.2119
;-> 142800.22


--------------------------------
Primi in base 10 e in altra base
--------------------------------

Dato un numero primo in base 10, convertirlo in base 2..9 e verificare se il numero ottenuto è primo in base 10.

Esempio:
  numero primo (base10) = 23
  23 (base2) = 10111 --> primo (base 10)
  23 (base3) = 212   --> composto (base 10)
  23 (base4) = 113   --> primo (base 10)
  23 (base5) = 43    --> primo (base 10)
  23 (base6) = 35    --> composto (base 10)
  23 (base7) = 32    --> composto (base 10)
  23 (base8) = 27    --> composto (base 10)
  23 (base9) = 25    --> composto (base 10)
  Il numero 23 è primo in base 10 e primo in base 2, 4 e 5 quando viene considerato in base 10.

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (base10-baseN number base)
"Convert a number from base 10 to base N (<=62)"
  (let ((charset "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
        (result '())
        (quotient number))
    (while (>= quotient base)
      (push (charset (% quotient base)) result)
      (setq quotient (/ quotient base))
    )
    (push (charset quotient) result)
    (join result)))

Funzione che calcola la sequenza dei numeri che sono primi in base 10 e in base B fino ad un dato limite:

(define (sequenza B limite)
  (let (seq '())
    (if (prime? (int (base10-baseN 2 B) 0 10)) (push 2 seq -1))
    (for (num 3 limite 2)
      (when (odd? num)
        (if (and (prime? num) (prime? (int (base10-baseN num B) 0 10)))
            (push num seq -1))))
    seq))

Sequenza OEIS A065720:
Primes whose binary representation is also the decimal representation of a prime.
  3, 5, 23, 47, 89, 101, 149, 157, 163, 173, 179, 199, 229, 313, 331, 367,
  379, 383, 443, 457, 523, 587, 631, 643, 647, 653, 659, 709, 883, 947,
  997, 1009, 1091, 1097, 1163, 1259, 1277, 1283, 1289, 1321, 1483, 1601,
  1669, 1693, 1709, 1753, 1877, 2063, 2069, 2099, ...

(sequenza 2 2099)
;-> (3 5 23 47 89 101 149 157 163 173 179 199 229 313 331 367
;->  379 383 443 457 523 587 631 643 647 653 659 709 883 947
;->  997 1009 1091 1097 1163 1259 1277 1283 1289 1321 1483 1601
;->  1669 1693 1709 1753 1877 2063 2069 2099)

Sequenza OEIS A065721:
Primes p whose base-3 expansion is also the decimal expansion of a prime.
  2, 67, 79, 103, 139, 157, 181, 193, 199, 211, 229, 277, 283, 307, 313,
  349, 367, 373, 409, 421, 433, 439, 463, 523, 541, 547, 571, 577, 751,
  829, 883, 919, 1021, 1033, 1039, 1087, 1171, 1249, 1303, 1429, 1483,
  1579, 1597, 1621, 1741, 1783, 1789, 1873, ...

(sequenza 3 1873)
;-> (2 67 79 103 139 157 181 193 199 211 229 277 283 307 313
;->  349 367 373 409 421 433 439 463 523 541 547 571 577 751
;->  829 883 919 1021 1033 1039 1087 1171 1249 1303 1429 1483
;->  1579 1597 1621 1741 1783 1789 1873)

Sequenza OEIS A065722:
Primes that when written in base 4, then reinterpreted in base 10, again give primes.
  2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 37, 43, 47, 53, 61, 71, 73, 79, 83,
  97, 103, 107, 109, 113, 131, 149, 151, 157, 163, 167, 181, 191, 193, ...

(sequenza 4 193)
;-> (2 3 5 7 11 13 17 19 23 29 37 43 47 53 61 71 73 79 83
;->  97 103 107 109 113 131 149 151 157 163 167 181 191 193)

Sequenza OEIS A065723:
Primes that when written in base 5, then reinterpreted in base 10, again give primes.
  2, 3, 13, 23, 41, 71, 83, 101, 163, 191, 211, 281, 283, 311, 331, 463,
  503, 571, 613, 653, 701, 743, 823, 863, 881, 983, 1091, 1213, 1231,
  1283, 1301, 1373, 1381, 1423, 1471, 1493, 1531, 1543, 1621, 1741,
  1783, 1861, 1873, 1931, 2063, 2203, 2213, 2221, ...

(sequenza 5 2221)
;-> (2 3 13 23 41 71 83 101 163 191 211 281 283 311 331 463
;->  503 571 613 653 701 743 823 863 881 983 1091 1213 1231
;->  1283 1301 1373 1381 1423 1471 1493 1531 1543 1621 1741
;->  1783 1861 1873 1931 2063 2203 2213 2221)

Sequenza OEIS A065724:
Primes p such that the decimal expansion of its base-6 conversion is also prime.
  2, 3, 5, 7, 19, 37, 67, 79, 97, 103, 127, 157, 163, 193, 229, 283, 307,
  337, 439, 487, 547, 571, 601, 631, 643, 673, 733, 751, 757, 853, 877,
  907, 937, 1021, 1033, 1039, 1087, 1093, 1117, 1171, 1249, 1279, 1423,
  1567, 1627, 1663, 1723, 1753, 1831, 1873, ...

(sequenza 6 1873)
;-> (2 3 5 7 19 37 67 79 97 103 127 157 163 193 229 283 307
;->  337 439 487 547 571 601 631 643 673 733 751 757 853 877
;->  907 937 1021 1033 1039 1087 1093 1117 1171 1249 1279 1423
;->  1567 1627 1663 1723 1753 1831 1873)

Sequenza OEIS A065725:
Primes p such that the decimal expansion of its base-7 conversion is also prime.
  2, 3, 5, 17, 29, 31, 43, 59, 71, 127, 157, 197, 211, 227, 239, 241, 337,
  353, 367, 379, 409, 463, 491, 563, 577, 619, 647, 743, 757, 773, 787,
  857, 911, 953, 967, 1093, 1123, 1163, 1193, 1249, 1303, 1373, 1429,
  1459, 1471, 1499, 1583, 1597, 1613, 1627, 1669, ...

(sequenza 7 1669)
;-> (2 3 5 17 29 31 43 59 71127 157 197 211 227 239 241 337
;->  353 367 379 409 463 491 563 577 619 647 743 757 773 787
;->  857 911 953 967 1093 1123 1163 1193 1249 1303 1373 1429
;->  1459 1471 1499 1583 1597 1613 1627 1669)

Sequenza OEIS A065726:
Primes p whose base-8 expansion is also the decimal expansion of a prime.
  2, 3, 5, 7, 11, 19, 31, 43, 59, 67, 71, 89, 137, 151, 179, 191, 199, 223,
  251, 257, 281, 283, 307, 311, 337, 353, 359, 367, 383, 409, 419, 433,
  443, 449, 523, 563, 617, 619, 641, 659, 727, 787, 809, 811, 857, 887,
  907, 919, 947, 977, 1033, 1039, 1097, 1123, ...

(sequenza 8 1123)
;-> (2 3 5 7 11 19 31 43 59 67 71 89 137 151 179 191 199 223
;->  251 257 281 283 307 311 337 353 359 367 383 409 419 433
;->  443 449 523 563 617 619 641 659 727 787 809 811 857 887
;->  907 919 947 977 1033 1039 1097 1123)

Sequenza OEIS A065727:
Primes p such that the decimal expansion of its base-9 conversion is also prime.
  2, 3, 5, 7, 37, 43, 61, 109, 127, 199, 271, 277, 379, 457, 487, 523, 541,
  613, 619, 673, 727, 757, 883, 907, 919, 991, 997, 1033, 1117, 1249, 1447,
  1483, 1531, 1549, 1567, 1627, 1693, 1699, 1747, 1753, 1987, 2053, 2161,
  2221, 2287, 2341, 2347, 2437, 2473, ...

(sequenza 9 2473)
;-> (2 3 5 7 37 43 61 109 127 199 271 277 379 457 487 523 541
;->  613 619 673 727 757 883 907 919 991 997 1033 1117 1249 1447
;->  1483 1531 1549 1567 1627 1693 1699 1747 1753 1987 2053 2161
;->  2221 2287 2341 2347 2437 2473)

Vediamo se esiste qualche numero che è primo in tutte le basi (se considerato in base 10 dopo la conversione).

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

(define (base10-baseX number base)
  "Convert a number from base 10 to base N (<=10) e restituisce un intero"
  (letn ((result 0) (power 1))
    (while (> number 0)
      (setq result (+ result (* (% number base) power)))
      (setq number (/ number base))
      (setq power (* power 10)))
    result))

(define (primiN limite)
  (local (out primi conta val)
    (setq out '())
    (setq primi (primes-to limite))
    (dolist (p primi)
      (setq conta 1) ; p è primo in base 10
      (for (base 2 9)
        (setq val (base10-baseX p base))
        (if (prime? val) (++ conta)))
      (push (list conta val) out)
    )
    (sort out >)))

Proviamo:

(primiN 100)
;-> ((8 3) (8 2) (7 5) (5 78) (5 47) (5 7)
;->  (4 87) (4 74) (4 41) (4 25) (4 21)
;->  (3 117) (3 108) (3 102) (3 67) (3 65)
;->  (3 52) (3 34) (3 32) (3 18) (3 14) (3 12)
;->  (2 81) (2 58) (2 45))

(time (primiN 10000))
;-> 766.301

(time (setq p100 (primiN 100000)))
;-> 182345.123
(slice p100 0 10)
;-> ((8 3) (8 2)
;->  (7 26357) (7 13607) (7 5)
;->  (6 158777) (6 131581) (6 112627) (6 87187) (6 77001))

Non credo che esista alcun numero primo in base 10 che sia anche primo in tutte le basi da 2 a 9 (quando viene considerato in base 10 dopo la conversione in altra base).


------------------------------------
Numeri primi in una matrice di cifre
------------------------------------

Data una matrice di cifre (0..9) trovare il percorso che genera il primo più grande.
Il percorso considera 4 direzioni (N, S, E, O).
Ogni cifra può essere usata solo una volta.

Esempio:
 1 2 3
 3 2 2
 3 3 2
 Primo maggiore = 33322321 con percorso = (3 3 3 2 2 3 2 1))

Algoritmo
- La funzione 'esplora' genera ricorsivamente tutti i numeri possibili concatenando cifre adiacenti.
- Ogni cella è marcata come "visitata" per il percorso corrente.
- A ogni passo, se il numero formato è primo, viene confrontato con il massimo trovato.
- Al termine, 'max-primo' contiene il numero primo più grande, e 'miglior-percorso' la sequenza di cifre che lo genera.

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

; Funzione che trova il numero primo più grande generabile da una matrice di cifre (0..9)
; Ogni cella può essere usata solo una volta nel percorso.
; Sono considerate le 4 direzioni (N, S, E, O)
(define (primo-maggiore matrice show-all)
  (letn ( (righe (length matrice))
          (colonne (length (matrice 0)))
          ; 8 direzioni (N, S, E, O, NE, NO, SE, SO)
          ;(direzioni '((1 0) (-1 0) (0 1) (0 -1) (1 1) (1 -1) (-1 1) (-1 -1)))
          ; 4 direzioni (N, S, E, O)
          (direzioni '((1 0) (-1 0) (0 1) (0 -1)))
          (primi '())
          (max-primo 0)
          (miglior-percorso '()) )
    ; funzione ricorsiva per esplorare tutti i percorsi
    (define (esplora i j numero percorso visitati)
      (if (and (>= i 0) (< i righe) (>= j 0) (< j colonne)
               (not (ref (list i j) visitati)))
          (letn ((nuovo-numero (+ (* numero 10) ((matrice i) j)))
                 (nuovo-percorso (append percorso (list ((matrice i) j))))
                 (nuovi-visitati (append visitati (list (list i j)))))
            ; se è primo, controlla se è il più grande trovato
            (when (and (> nuovo-numero 1) (prime? nuovo-numero))
                (push nuovo-numero primi)
                (if (> nuovo-numero max-primo)
                    (setq max-primo nuovo-numero miglior-percorso nuovo-percorso)))
            ; continua l’esplorazione nelle 8 direzioni
            (dolist (d direzioni)
              (esplora (+ i (d 0)) (+ j (d 1)) nuovo-numero nuovo-percorso nuovi-visitati)))))
    ; avvia la ricerca da ogni cella della matrice
    (for (i 0 (- righe 1))
      (for (j 0 (- colonne 1))
        (esplora i j 0 '() '())))
    (if show-all (println primi))
    (list max-primo miglior-percorso)))

Proviamo:

(setq matrice '((1 2 3)
                (3 2 2)
                (3 3 2)))

(primo-maggiore matrice true)
;-> (2333 233 23 2232133 223 2 33322321 3331 32232133 3223223 323123 32232133
;->  32213 3221 3 33223 3323 33223 33223 33223 33223 3323 331 3 223 2221333
;->  2221 22333123 223331 223 23223331 23 22333123 223331 223 2 2333 233
;->  23 2232133 223 22133 2213 223 2333 233 23 2 3221 3223223 312233 31223
;->  31 33322321 33322321 3 3213233 321323 32233 32233 32233 322213 32233
;->  32233 3 213223 21323 23223331 23223331 23 223 2222333 223331 223
;->  2 12322333 12322333 12322333 1223 1223 13)
;-> (33322321 (3 3 3 2 2 3 2 1))

Per interi a 64 bit il valore massimo è 9223372036854775807, che ha 19 cifre.
Quindi la matrice di cifre può avere al massimo 18 elementi.
Vediamo una funzione che limita la lunghezza massima dei primi da considerare utilizzando un parametro N.
Inoltre la funzione calcola anche il numero totale di tutti i percorsi esplorati, cioè di tutte le sequenze di cifre generate da 'esplora', con lunghezza da 1 a N, indipendentemente dal fatto che formino primi o no.

; Funzione che trova il numero primo più grande generabile da una matrice di cifre (0..9)
; Ogni cella può essere usata solo una volta nel percorso.
; Sono considerate le 4 direzioni (N, S, E, O)
; Il parametro N limita la lunghezza massima del numero generato (max N = 18).
; Restituisce anche il numero totale di percorsi esplorati (lunghezza 1..N)
(define (primo-maggiore matrice N show-all)
  (letn ( (righe (length matrice))
          (colonne (length (matrice 0)))
          (direzioni '((1 0) (-1 0) (0 1) (0 -1)))
          (primi '())
          (max-primo 0)
          (miglior-percorso '())
          (conteggio 0) )
    (define (esplora i j numero percorso visitati)
      (if (and (>= i 0) (< i righe) (>= j 0) (< j colonne)
               (not (ref (list i j) visitati))
               ; controllo lunghezza del numero corrente
               (< (length percorso) N))
          (letn ((nuovo-numero (+ (* numero 10) ((matrice i) j)))
                 (nuovo-percorso (append percorso (list ((matrice i) j))))
                 (nuovi-visitati (append visitati (list (list i j)))))
            ; ogni percorso valido (di almeno 1 cifra) incrementa il contatore
            (++ conteggio)
            ; se è primo, controlla se è il più grande trovato
            (when (and (> nuovo-numero 1) (prime? nuovo-numero))
                (push nuovo-numero primi)
                (if (> nuovo-numero max-primo)
                    (setq max-primo nuovo-numero miglior-percorso nuovo-percorso)))
            ; continua solo se la lunghezza è ancora < N
            (dolist (d direzioni)
              (esplora (+ i (d 0)) (+ j (d 1)) nuovo-numero nuovo-percorso nuovi-visitati)))))
    (for (i 0 (- righe 1))
      (for (j 0 (- colonne 1))
        (esplora i j 0 '() '())))
    (if show-all (println primi))
    (list max-primo miglior-percorso conteggio)))

Proviamo:

(setq matrice '((1 2 3)
                (3 2 2)
                (3 3 2)))

(primo-maggiore matrice 8)
;-> (33322321 (3 3 3 2 2 3 2 1) 613)

(primo-maggiore matrice 6 true)
;-> (2333 233 23 223 2 3331 323123 32213 3221 3 33223 3323 33223 33223
;->  33223 33223 3323 331 3 223 2221 223331 223 23 223331 223 2 2333 233
;->  23 223 22133 2213 223 2333 233 23 2 3221 312233 31223 31 3 321323
;->  32233 32233 32233 322213 32233 32233 3 213223 21323 23 223 223331
;->  223 2 1223 1223 13)
;-> (323123 (3 2 3 1 2 3) 389)

(setq matrice '((1 2 3 4 5 6)
                (3 2 2 7 9 8)
                (8 3 4 3 3 2)
                (1 7 6 3 9 1)))

(primo-maggiore matrice 6 true)
;-> (19333 193379 1933 193 193379 19333 1933 193 19 123379 123973 128939
;->  ...
;->  2 123479 12347 1223 122279 122273 12227 1223 132371 138371 1381 13)
;-> (986543 (9 8 6 5 4 3) 3408)

Aumentando N aumentano anche i percorsi da considerare e la funzione rallenta (anche perchè 'prime?' deve verificare numeri più grandi).
Nota: N non può essere maggiore di 18 (cioè non possiamo usare numeri più grandi di quelli consentiti da Int64).

(time (println (primo-maggiore matrice 10)))
;-> (9865473437 (9 8 6 5 4 7 3 4 3 7) 45924)
;-> 1060.107

(time (println (primo-maggiore matrice 11)))
;-> (98654724323 (9 8 6 5 4 7 2 4 3 2 3) 78716)
;-> 4323.084

(time (println (primo-maggiore matrice 12)))
;-> (986547342223 (9 8 6 5 4 7 3 4 2 2 2 3) 131492)
;-> 18921.877

(time (println (primo-maggiore matrice 13)))
;-> (9865473422381 (9 8 6 5 4 7 3 4 2 2 3 8 1) 207540)
;-> 78916.385

Anche aumentare le dimensioni della matrice influisce sulla velocità della funzione.


----------------------
Scheduling di processi
----------------------

Abbiamo N interi che rappresentano il tempo di esecuzione di N processi.
Un computer con M core esegue in parallelo un processo per ogni core.
Determinare l'ordine di completamento dei processi e il tempo reale (attesa + esecuzione) di ogni processo.

Esempio:
  N = 4
  M = 2
  tempi = (5 3 4 6)
  Il computer prende i primi 2 processi (5 e 3) e dopo 3 unità di tempo termina il secondo processo.
  Adesso il computer prende il terzo processo (4) e cominica la sua esecuzione.
  Dopo 2 secondi termina il primo processo, mentre il terzo processo ha ancora 2 unità di tempo.
  Il computer prende il quarto processo.
  Dopo 2 secondi termina il terzo processo.
  Dopo 4 secondi termina il quarto processo.
  Ordine di completamento (durate): (3 5 4 6)
  Tempi reali di completamento per processo (ordine originale): (5 3 7 11)
  Spiegazione: i processi terminano ai tempi 3, 5, 7, 11.
  Quindi:
  il processo con durata 5 (primo nell'input) finisce a t = 5,
  quello con durata 3 finisce a t = 3,
  quello con durata 4 finisce a t = 7,
  quello con durata 6 finisce a t = 11.

Algoritmo
---------
Si tratta di simulare un scheduling a più processori (M core), dove i processi vengono gestiti in modo non preemptive, cioè ogni processo viene eseguito fino al termine una volta iniziato.
1. Inizializzazione
   Tutti i processi sono messi nella coda 'queue', contenente gli indici (da 0 a n−1).
   I primi 'M' processi vengono avviati contemporaneamente:
   - 'run-idx' tiene traccia degli indici dei processi in esecuzione
   - 'run-rem' conserva i tempi rimanenti per ciascuno
2. Avanzamento del tempo
   A ogni iterazione si calcola 'minr', il tempo minimo rimanente tra i processi in corso.
   Il tempo globale 'adesso' viene incrementato di 'minr'.
3. Aggiornamento processi attivi
   Tutti i processi attivi riducono il loro tempo rimanente di 'minr'.
   Chi arriva a 0 viene considerato terminato:
   - viene stampato (se 'show' è true)
   - viene registrato in 'comp-order' e 'comp-pairs'
   - rimosso da 'run-idx' e 'run-rem'
4. Gestione nuovi processi
   Ogni volta che un processo termina, se ci sono ancora elementi in 'queue',
   vengono avviati nuovi processi fino a riempire i 'M' core disponibili.
5. Calcolo dei tempi finali
   Al termine, 'real-times' contiene per ogni processo il tempo di completamento.
   L'ordine corrisponde all'ordine originale dei processi.
6. Risultato finale
   La funzione restituisce:
   - 'comp-order': i tempi originali nell'ordine in cui i processi sono finiti
   - 'real-times': i tempi di completamento effettivi, indicizzati per processo

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FUNZIONE: scheduler
; Simula l'esecuzione di più processi su M core paralleli (CPU)
; Ogni processo ha un tempo di esecuzione specificato in ‘tempi'.
; I processi vengono eseguiti in ordine di arrivo (round-robin senza preemption).
; Quando un core termina un processo, ne prende subito uno nuovo dalla coda.
; Parametri:
;   tempi -> lista dei tempi di esecuzione dei processi
;   M -> numero di core disponibili
;   show -> se true, stampa inizio e termine dei processi
; Output:
;   una lista composta da:
;     1. ordine di completamento dei processi (lista di tempi)
;     2. lista dei tempi effettivi di completamento
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scheduler tempi M show)
  (letn (n (length tempi) ; numero di processi
         queue '()        ; coda dei processi in attesa
         run-idx '()      ; indici dei processi attivi
         run-rem '()      ; tempi rimanenti dei processi attivi
         comp-order '()   ; tempi originali nell'ordine di completamento
         comp-pairs '()   ; coppie (indice processo, tempo di completamento)
         adesso 0)        ; tempo globale corrente
    ; inizializza la coda con tutti i processi
    ;(for (i 0 (- n 1) 1) (push i queue -1))
    (setq queue (sequence 0 (- n 1)))
    ; avvia i primi M processi
    (for (k 0 (- M 1) 1)
      (if (> (length queue) 0)
        (begin
          (push (queue 0) run-idx -1)
          (push (tempi (queue 0)) run-rem -1)
          (if show (println "Inizio processo " (+ (queue 0) 1) ": " adesso))
          (pop queue 0))))
    ; ciclo principale: continua finché ci sono processi attivi
    (while (> (length run-idx) 0)
      ; trova il tempo minimo rimanente tra i processi attivi
      (letn (minr (apply min run-rem))
        ; avanza il tempo globale
        (setq adesso (+ adesso minr))
        ; aggiorna i tempi rimanenti
        (for (j 0 (- (length run-rem) 1) 1)
          (setf (run-rem j) (- (run-rem j) minr)))
        ; controlla i processi che sono terminati
        (let ((i 0))
          (while (< i (length run-idx))
            (if (= (run-rem i) 0)
              (begin
                (if show (println "Termine processo " (+ (run-idx i) 1) ": " adesso))
                (push (tempi (run-idx i)) comp-order -1)
                (push (list (run-idx i) adesso) comp-pairs -1)
                (pop run-idx i)
                (pop run-rem i))
              (++ i))))
        ; riempi i core liberi con nuovi processi dalla coda
        (while (and (> (length queue) 0) (< (length run-idx) M))
          (if show (println "Inizio processo " (+ (queue 0) 1) ": " adesso))
          (push (queue 0) run-idx -1)
          (push (tempi (queue 0)) run-rem -1)
          (pop queue 0))))
    ; calcolo dei tempi di completamento effettivi
    (let ((real-times '()))
      (for (i 0 (- n 1) 1)
        (let ((found nil) (k 0))
          (while (and (not found) (< k (length comp-pairs)))
            (let ((p (comp-pairs k)))
              (if (= (p 0) i)
                (begin
                  (push (p 1) real-times -1)
                  (setq found true)))
              (++ k)))
          (if (not found) (push nil real-times -1))))
      (list comp-order real-times))))

Proviamo:

(setq tempi '(5 3 4 6))
(scheduler tempi 2 true)
;-> Inizio processo 1: 0
;-> Inizio processo 2: 0
;-> Termine processo 2: 3
;-> Inizio processo 3: 3
;-> Termine processo 1: 5
;-> Inizio processo 4: 5
;-> Termine processo 3: 7
;-> Termine processo 4: 11
;-> ((3 5 4 6) (5 3 7 11))

(setq tempi '(5 3 4 6))
(scheduler tempi 1 true)
;-> Inizio processo 1: 0
;-> Termine processo 1: 5
;-> Inizio processo 2: 5
;-> Termine processo 2: 8
;-> Inizio processo 3: 8
;-> Termine processo 3: 12
;-> Inizio processo 4: 12
;-> Termine processo 4: 18
;-> ((5 3 4 6) (5 8 12 18))

(setq tempi '(5 3 4 6))
(scheduler tempi 4 true)
;-> Inizio processo 1: 0
;-> Inizio processo 2: 0
;-> Inizio processo 3: 0
;-> Inizio processo 4: 0
;-> Termine processo 2: 3
;-> Termine processo 3: 4
;-> Termine processo 1: 5
;-> Termine processo 4: 6
;-> ((3 4 5 6) (5 3 4 6))

(setq tempi '(5))
(scheduler tempi 2 true)
;-> Inizio processo 1: 0
;-> Termine processo 1: 5
;-> ((5) (5))

(setq tempi (sequence 1 9))
(scheduler tempi 3 true)
;-> Inizio processo 1: 0
;-> Inizio processo 2: 0
;-> Inizio processo 3: 0
;-> Termine processo 1: 1
;-> Inizio processo 4: 1
;-> Termine processo 2: 2
;-> Inizio processo 5: 2
;-> Termine processo 3: 3
;-> Inizio processo 6: 3
;-> Termine processo 4: 5
;-> Inizio processo 7: 5
;-> Termine processo 5: 7
;-> Inizio processo 8: 7
;-> Termine processo 6: 9
;-> Inizio processo 9: 9
;-> Termine processo 7: 12
;-> Termine processo 8: 15
;-> Termine processo 9: 18
;-> ((1 2 3 4 5 6 7 8 9) (1 2 3 5 7 9 12 15 18))


------------------------------------
Cifra maggiore e minore di un numero
------------------------------------

Dato un numero intero determinare la cifra più grande e la più piccola.

Sequenza OEIS A054055:
Largest digit of n.
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 2, 2, 2, 3,
  4, 5, 6, 7, 8, 9, 3, 3, 3, 3, 4, 5, 6, 7, 8, 9, 4, 4, 4, 4, 4, 5, 6, 7,
  8, 9, 5, 5, 5, 5, 5, 5, 6, 7, 8, 9, 6, 6, 6, 6, 6, 6, 6, 7, 8, 9, 7, 7,
  7, 7, 7, 7, 7, 7, 8, 9, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9,
  9, 9, 9, 9, 1, 1, 2, 3, 4, ...

Sequenza OEIS A054054:
Smallest digit of n.
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 2, 2,
  2, 2, 2, 2, 2, 2, 0, 1, 2, 3, 3, 3, 3, 3, 3, 3, 0, 1, 2, 3, 4, 4, 4, 4,
  4, 4, 0, 1, 2, 3, 4, 5, 5, 5, 5, 5, 0, 1, 2, 3, 4, 5, 6, 6, 6, 6, 0, 1,
  2, 3, 4, 5, 6, 7, 7, 7, 0, 1, 2, 3, 4, 5, 6, 7, 8, 8, 0, 1, 2, 3, 4, 5,
  6, 7, 8, 9, 0, 0, 0, 0, 0, ...

Metodo1:

(define (min-max1 num)
  (let (cifre (map int (explode (string (abs num)))))
    (list (apply min cifre) (apply max cifre))))

(min-max1 1234)
;-> (1 4)
(min-max1 55555)
;-> (5 5)
(min-max1 -64718)
;-> (1 8)

(map last (map min-max1 (sequence 0 50)))
;-> (0 1 2 3 4 5 6 7 8 9 1 1 2 3 4 5 6 7 8 9 2 2 2 3
;->  4 5 6 7 8 9 3 3 3 3 4 5 6 7 8 9 4 4 4 4 4 5 6 7
;->  8 9 5)

Metodo2:

(define (min-max2 num)
  (setq num (abs num)) ; per gestire anche i numeri negativi
  (if (< num 10) ; numeri con una cifra
      (list num num)
  ;else
      (let ( (min-val 10) (max-val -1) (cifra 0) )
        (while (!= num 0)
          (setq cifra (% num 10))
          (setq min-val (min cifra min-val))
          (setq max-val (max cifra max-val))
          (setq num (/ num 10)))
        (list min-val max-val))))

(min-max2 1234)
;-> (1 4)
(min-max2 55555)
;-> (5 5)
(min-max2 -64718)
;-> (1 8)

(map first (map min-max2 (sequence 0 50)))
;-> (0 1 2 3 4 5 6 7 8 9 0 1 1 1 1 1 1 1 1 1 0 1 2 2
;->  2 2 2 2 2 2 0 1 2 3 3 3 3 3 3 3 0 1 2 3 4 4 4 4
;->  4 4 0)

Test di velocità:

(silent (setq nums (rand 1e12 10000)))
(time (map min-max1 nums) 100)
;-> 2750.12
(time (map min-max2 nums) 100)
;-> 2743.54

Le due funzioni hanno la stessa velocità.


-----------------
Rebasing a number
-----------------

Definiamo l'operazione di "Rebasing a number" come la scrittura delle sue cifre in forma decimale, quindi la loro interpretazione nella base N più piccola possibile (2 <= N <= 10) e la sua conversione in base 10.
Esempio:
  numero = 1234
  La base più piccola vale 5.
  La conversione di 1234 da base 5 a base 10 vale 194

Sequenza OEIS A068505:
Decimal representation of n interpreted in base b+1, where b=A054055(n) is the largest digit in decimal representation of n.
  1, 2, 3, 4, 5, 6, 7, 8, 9, 2, 3, 5, 7, 9, 11, 13, 15, 17, 19, 6, 7, 8,
  11, 14, 17, 20, 23, 26, 29, 12, 13, 14, 15, 19, 23, 27, 31, 35, 39, 20,
  21, 22, 23, 24, 29, 34, 39, 44, 49, 30, 31, 32, 33, 34, 35, 41, 47, 53,
  59, 42, 43, 44, 45, 46, 47, 48, 55, 62, 69, 56, 57, 58, 59, 60, 61, ...

(define (base10-baseX number base)
  "Convert a number from base 10 to base N<=10 (return integer)"
  (letn ((result 0) (power 1))
    (while (> number 0)
      (setq result (+ result (* (% number base) power)))
      (setq number (/ number base))
      (setq power (* power 10)))
    result))

(define (baseX-base10 number base)
  "Convert a number from base N<=10 to base 10 (return integer)"
  (letn ((result 0) (power 1) (n number))
    (while (> n 0)
      (setq result (+ result (* (% n 10) power)))
      (setq n (/ n 10))
      (setq power (* power base)))
    result))

Funzione che restituisce la cifra più grande di un numero intero:

(define (cifra-maggiore num)
  (let (cifre (map int (explode (string (abs num)))))
    (apply max cifre)))

Funzione che effettua il processo 'rebasing a number':

(define (rebase num)
  (let (base-minore (+ (cifra-maggiore num) 1))
    (baseX-base10 num base-minore)))

Proviamo:

(rebase 1234)
;-> 194

(map rebase (sequence 1 75))
;-> (1 2 3 4 5 6 7 8 9 2 3 5 7 9 11 13 15 17 19 6 7 8
;->  11 14 17 20 23 26 29 12 13 14 15 19 23 27 31 35 39 20
;->  21 22 23 24 29 34 39 44 49 30 31 32 33 34 35 41 47 53
;->  59 42 43 44 45 46 47 48 55 62 69 56 57 58 59 60 61)

============================================================================

