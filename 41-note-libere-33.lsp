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

============================================================================

