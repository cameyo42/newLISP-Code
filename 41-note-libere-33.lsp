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

============================================================================

