================

 NOTE LIBERE 40

================

  "Da vicino nessuno è normale"

----------------
Cassette di mele
----------------

Ci sono nove cassette contenenti mele da 100 g e una cassetta contenente mele da 150 g.
Ogni cassetta contiene fra le 10 e le 20 mele (ma non sappiamo il numero esatto di mele in ogni cassetta).
Abbiamo a disposizione una bilancia elettronica e la possiamo usare una sola volta per trovare l'unica cassetta che contiene le mele più pesanti, cioè la cassetta contenente mele da 150 g.

Soluzione
---------
Prendiamo:
  1 mela dalla prima cassetta
  2 mele dalla seconda cassetta
  3 mele dalla terza cassetta
  ...
  10 mele dalla decima cassetta.

Se le mele prese pesassero tutte 100 g, allora il peso totale sarebbe:

  Peso_totale = Numero_mele_prese * 100 g

Il numero delle mele prese vale:

  1 + 2 + 3 + ... + 10 = n*(n + 1)/2 = 55

Quindi il peso totale sarebbe 5500 g.
Comunque le mele non pesano tutte 100 g, quindi il peso totale delle mele prese deve essere maggiore di 5500 g.
Pesando le mele prese otteniamo un peso pari a X g.
Se la mela da 150 g era nella cassetta 1, allora X vale 5500 + 50 (1 sola mela da 150 g)
Se la mela da 150 g era nella cassetta 2, allora X vale 5500 + 100 (2 sole mele da 150 g)
...
Se la mela da 150 g era nella cassetta 10, allora X vale 5500 + 500 (10 sole mele da 150 g)
Possiamo concludere che la differenza tra X e 5500 è un multiplo di 50.
Quindi il valore del multiplo rappresenta la cassetta con le mele da 150 g.

(define (mele)
  (local (cassette peso100 peso-totale cassetta-150)
    (setq cassette (dup 100 10))
    (setf (cassette (rand 10)) 150)
    (println "Cassette = " cassette)
    (println "Cassetta con mele da 150g = " (+ (find 150 cassette) 1))
    (setq peso100 (mul 55 100))
    (setq peso-totale (apply + (map (fn(x) (* x (+ $idx 1))) cassette)))
    (setq cassetta150 (/ (- peso-totale peso100) 50))
    (println "Cassetta calcolata con mele da 150g = " cassetta150) '>))

(mele)
;-> Cassette = (100 100 100 150 100 100 100 100 100 100)
;-> Cassetta con mele da 150g = 4
;-> Cassetta calcolata con mele da 150g = 4


---------------------------------
Il rompicapo logico più difficile
---------------------------------

Questo rompicapo è stato ideato da Raymond Smullyan e John McCarthy.
Secondo il logico matematico George Boolos, è il più difficile di tutti.

Tre oracoli divini A, B, e C sono chiamati, in un qualche ordine, Verace, Mendace e Imprevedibile.
Verace dice sempre il vero, Mendace dice sempre il falso, mentre Imprevedibile decide se essere sincero o meno in modo completamente casuale.
Il problema consiste nel determinare le identità di A, B, e C ponendo loro tre domande a cui è possibile rispondere con un "sì" o con un "no".
Ogni domanda deve essere posta a uno solo degli oracoli, che, pur comprendendo l'italiano, risponderà sempre nella propria lingua con le parole "da" o "ja".
Non si sa quale di questi termini corrisponda a "sì" e quale a "no".

Le tre domande che risolvono il problema sono:

1) Chiedo ad A: Se ti chiedessi "B è Imprevedibile?", risponderesti "ja"?
Con questa domanda si individua l'oracolo che non è Imprevedibile.

2) Chiedo all'oracolo non Imprevedibile: Se ti chiedessi "Sei Verace?", risponderesti "ja"?
Con questa domanda si individua se loracolo che non è imprevedibile è Verace oppure Mendace.

3) Chiedo all'oracolo non Imprevedibile: Se ti chiedessi "A è Imprevedibile?", risponderesti "ja"?
Con questa domanda si individua l'oracolo Imprevedibile: A oppure l'oracolo con cui non abbiamo parlato.

Vediamo il ragionamento logico che conduce alla soluzione.

Il punto fondamentale del rompicapo è la particolare forma delle domande:
  > "Se ti chiedessi P, risponderesti 'ja'?"

Questa costruzione elimina contemporaneamente il problema della lingua (ja = sì oppure no) e, quando l'oracolo è deterministico, il problema della verità o della menzogna.
Vediamo esattamente perché.

1) Il principio fondamentale
----------------------------
Consideriamo un oracolo che sappiamo essere non Imprevedibile, quindi necessariamente Verace oppure Mendace.
Supponiamo di chiedergli:

  > "Se ti chiedessi P, risponderesti ja?"

Chiamiamo P una qualsiasi proposizione.

a) Se l'oracolo è Verace
Se P è vera, alla domanda diretta P risponderebbe ja se ja significa sì.
Quindi alla domanda:
  > "Risponderesti ja?"
risponde ja.
Se invece P è falsa, alla domanda diretta risponderebbe l'altra parola, quindi alla domanda meta risponde l'altra parola.
Pertanto:
Verace:
P vera  -> ja
P falsa -> non-ja

b) Se l'oracolo è Mendace
Qui avviene qualcosa di interessante.
Se P è vera, alla domanda diretta P il Mendace mentirebbe.
Quindi direbbe ja se ja significa no, e direbbe l'altra parola se ja significa sì.
La domanda meta:
  > "Risponderesti ja?"
viene poi anch'essa falsificata dal Mendace.
Il risultato finale è ancora:
Mendace:
  P vera  -> ja
  P falsa -> non-ja
Quindi, per entrambi gli oracoli deterministici:
  "Risponderesti ja se ti chiedessi P?"
               |
               v
         ja <=> P vera
Questo è il meccanismo fondamentale del rompicapo.
In altre parole, se l'oracolo è Verace o Mendace, possiamo interpretare la sua risposta ja come "P è vera", anche se non sappiamo cosa significhi realmente ja.

2) Prima domanda
----------------
La domanda è:
  > "Se ti chiedessi 'B è Imprevedibile?', risponderesti ja?"
Indichiamo con:
  P = "B è Imprevedibile"
Se A è Verace o Mendace, abbiamo appena dimostrato che:
  A risponde ja <=> B è Imprevedibile
Ma c'è un problema: A potrebbe essere proprio Imprevedibile.
In quel caso la risposta di A non ci dà alcuna informazione.
Ed è proprio qui che entra il trucco.

Caso 1: A è Imprevedibile
Allora B e C sono necessariamente:
  B = Verace/Mendace
  C = Mendace/Verace
Quindi sia B sia C sono non Imprevedibili.
Qualunque sia la risposta di A, possiamo quindi scegliere indifferentemente, per esempio, B come oracolo non Imprevedibile.

Caso 2: A non è Imprevedibile
Allora A è Verace oppure Mendace, quindi la risposta è affidabile nel senso appena dimostrato.
Abbiamo:
ja    -> B è Imprevedibile
non-ja -> B non è Imprevedibile
Quindi:
risposta ja:
    B = Imprevedibile
    C = non Imprevedibile
    -> scegli C
risposta non-ja:
    B = non Imprevedibile
    -> scegli B
Possiamo quindi costruire la regola:
1a risposta   oracolo da scegliere
----------------------------------
ja            C
non-ja        B
E questa scelta è sempre un oracolo non Imprevedibile.
Questo è il vero risultato della prima domanda.
Non abbiamo ancora stabilito chi sia Verace e chi Mendace, ma abbiamo trovato con certezza un oracolo deterministico.

3) Seconda domanda
------------------
Ora abbiamo un oracolo X che sappiamo essere non Imprevedibile.
Gli chiediamo:
  > "Se ti chiedessi 'Sei Verace?', risponderesti ja?"
Qui:
  P = "Sei Verace?"
Poiché X è certamente Verace oppure Mendace, possiamo applicare il principio fondamentale.
Otteniamo:
  risposta ja     -> P vera  -> X è Verace
  risposta non-ja -> P falsa -> X è Mendace
Quindi la seconda domanda determina esattamente l'identità di X.
Per esempio:
ja     -> X = Verace
non-ja -> X = Mendace
A questo punto conosciamo sia X sia l'altro oracolo non Imprevedibile.
Di conseguenza conosciamo anche chi è Imprevedibile: è il terzo oracolo.
Ma la terza domanda serve a determinarlo direttamente rispetto ad A.

4) Terza domanda
----------------
Poniamo nuovamente la domanda allo stesso oracolo X, che sappiamo essere deterministico:
  > "Se ti chiedessi 'A è Imprevedibile?', risponderesti ja?"
Poniamo:
  P = "A è Imprevedibile"
Poiché X è Verace oppure Mendace, il principio fondamentale ci garantisce:
  ja     <=> A è Imprevedibile
  non-ja <=> A non è Imprevedibile
Quindi:
a) Se risponde ja: A = Imprevedibile
b) Se risponde l'altra parola: A != Imprevedibile
Ma sappiamo già che X non è Imprevedibile.
Quindi, se A non è Imprevedibile, l'Imprevedibile deve necessariamente essere il terzo oracolo, quello con cui non abbiamo parlato.

5) Riassunto dell'intera strategia
----------------------------------
DOMANDA 1
A: "B e' Imprevedibile?"

        |
        v

Se A e' Imprevedibile:
    B e C sono entrambi non Imprevedibili.
    Scegli B.

Se A non e' Imprevedibile:
    ja     -> B e' Imprevedibile -> scegli C
    non-ja -> B non e' Imprevedibile -> scegli B

        |
        v

Abbiamo sicuramente un oracolo X
che non e' Imprevedibile.

DOMANDA 2
X: "Sei Verace?"

    ja     -> X = Verace
    non-ja -> X = Mendace

DOMANDA 3
X: "A e' Imprevedibile?"

    ja     -> A = Imprevedibile
    non-ja -> A non e' Imprevedibile
              -> l'Imprevedibile e' il terzo oracolo

A questo punto le identità di tutti e tre sono determinate.

6) Considerazioni finali
------------------------
Le difficoltà del problema sono tre:
1) Non sappiamo chi mente.
2) Non sappiamo quale parola significhi "si'".
3) Uno degli oracoli risponde casualmente.
La domanda
  > "Se ti chiedessi P, risponderesti ja?"
risolve contemporaneamente i primi due problemi, purché l'oracolo interrogato non sia Imprevedibile.
Infatti per Verace e Mendace vale sempre:
                 P
                 |
                 v
"Risponderesti ja?" 
                 |
                 v
             ja <=> P

La prima domanda serve quindi soprattutto a garantire che dalla seconda domanda in poi interroghiamo un oracolo affidabile.
Ed è questo il passaggio più elegante del rompicapo: non cerchiamo subito di identificare Verace e Mendace, prima costruiamo una procedura che ci garantisce di aver trovato uno dei due.
Solo dopo utilizziamo quell'oracolo per risolvere il resto del rompicapo.


------------
Chuck-a-luck
------------

Nel gioco di fortuna Chuck-a-luck si fa una puntata su un numero da uno a sei e poi lanciano tre dadi.
Se il punteggio scelto esce in k dadi, si vince k volte la puntata.
Altrimenti si perdela puntata (k = 0).
Nota: con k = 1 significa che riprendiamo i soldi della puntata, non si vince niente.

(define (chuck1 iter)
  ; la puntata vale 1
  (let ((totale 0) (k 0) (num 0))
    (for (i 1 iter)
      (setq k 0)
      (setq num (rand 6))
      (if (= num (rand 6)) (++ k))
      (if (= num (rand 6)) (++ k))
      (if (= num (rand 6)) (++ k))
      (if (zero? k)
        (-- totale) ; perde la puntata (1)
        (++ totale k))) ; vince k volte la puntata (k*1)
    ; Restituisce una lista con due valori:
    ; soldi finali
    ; soldi vinti(+) o persi(-) per ogni giocata
    (list totale (div totale iter))))

Proviamo:

(seed (time-of-day) true)
(chuck1 1e7)
;-> (-786343 -0.0786343)

Poichè si perde 0.0786343 per ogni puntata da 1, il gioco è favorevole al banco.


----------------
Poker con i dadi
----------------

Lanciamo 5 dadi equi con facce da 1 a 6.
Vogliamo determinare le seguenti combinazioni:

1) Pokerissimo (cinque numeri uguali)
2) Poker (quattro numeri uguali)
3) Full (tre numeri uguali + due numeri uguali)
4) Scala massima (2 3 4 5 6)
5) Scala minima (1 2 3 4 5)
6) Tris (tre numeri uguali)
7) Doppia coppia (due numeri ugugali + due numeri uguali)
8) coppia (due numeri uguali)
9) numero più alto

Scala dei valori:
  Pokerissimo > Poker > Full > Scala massima > Scala minima > Tris > Doppia coppia > Coppia > Numero più alto

Scriviamo una funzione che prende una lista con 5 valori (ognuno compreso tra 1 a 6) e restituisce la combinazione più alta ottenibile con i valori dati.
Conviene far restituire una lista di due elementi:
  (nome-combinazione valori)
Per esempio:
  ("Tris" (2 2 2))
  ("Doppia coppia" (5 5 3 3))
  ("Poker" (4 4 4 4))
In questo modo il secondo elemento contiene esattamente i dadi che determinano la combinazione.
La scala dei valori viene codificata nei vari rami della funzione 'cond'.

; Determina la combinazione migliore ottenibile con 5 dadi.
; I valori devono essere interi compresi tra 1 e 6.
; Le combinazioni sono ordinate dalla piu' alta alla piu' bassa:
; Pokerissimo, Poker, Full, Scala massima, Scala minima, Tris,
; Doppia coppia, Coppia, Numero piu' alto.
(define (combinazione dadi)
  ; Calcola quante volte compare ciascun valore da 1 a 6.
  (let (freq ordinati max-freq)
    (setq freq (flat (map (fn (x) (count (list x) dadi)) (sequence 1 6))))
    ; Ordina i dadi per verificare le scale.
    (setq ordinati (sort dadi))
    ; Trova la frequenza massima.
    (setq max-freq (apply max freq))
    ; Pokerissimo: cinque valori uguali.
    (cond
      ((= max-freq 5)
        (list "Pokerissimo" ordinati))
      ; Poker: quattro valori uguali.
      ((= max-freq 4)
        (list "Poker" (filter (fn (x) (= ((count (list x) dadi) 0) 4)) dadi)))
      ; Full: tre valori uguali e due valori uguali.
      ((and (= max-freq 3) (find 2 freq))
        (list "Full" ordinati))
      ; Scala massima: 2, 3, 4, 5, 6.
      ((= ordinati '(2 3 4 5 6))
        (list "Scala massima" ordinati))
      ; Scala minima: 1, 2, 3, 4, 5.
      ((= ordinati '(1 2 3 4 5))
        (list "Scala minima" ordinati))
      ; Tris: tre valori uguali.
      ((= max-freq 3)
        (list "Tris" (filter (fn (x) (= ((count (list x) dadi) 0) 3)) dadi)))
      ; Doppia coppia: due valori compaiono due volte.
      ((= (length (filter (fn (x) (= x 2)) freq)) 2)
        (let (coppie '())
          (dolist (x (sequence 1 6))
            (if (= ((count (list x) dadi) 0) 2)
                (extend coppie (list x x))))
          (list "Doppia coppia" coppie)))
      ; Coppia: un valore compare due volte.
      ((find 2 freq)
        (list "Coppia"
              (filter (fn (x) (= ((count (list x) dadi) 0) 2)) dadi)))
      ; Nessuna combinazione: restituisce il dado piu' alto.
      (true
        (list "Numero piu' alto" (list (apply max dadi)))))))

Proviamo:

(combinazione '(2 2 2 5 6))
;-> ("Tris" (2 2 2))
(combinazione '(5 5 3 3 1))
;-> ("Doppia coppia" (3 3 5 5))
(combinazione '(4 4 4 4 2))
;-> ("Poker" (4 4 4 4))
(combinazione '(3 3 3 6 6))
;-> ("Full" (3 3 3 6 6))
(combinazione '(2 3 4 5 6))
;-> ("Scala massima" (2 3 4 5 6))
(combinazione '(1 2 3 4 5))
;-> ("Scala minima" (1 2 3 4 5))
(combinazione '(2 2 3 4 5))
;-> ("Coppia" (2 2))
(combinazione '(1 3 4 5 6))
;-> ("Numero piu' alto" (6))
(combinazione '(2 2 4 5 6))
;-> ("Coppia" (2 2))
(combinazione '(4 4 6 2 2))
;-> ("Doppia coppia" (2 2 4 4))
(combinazione '(2 2 2 4 6))
;-> ("Tris" (2 2 2))
(combinazione '(2 2 2 4 4))
;-> ("Full" (2 2 2 4 4))
(combinazione '(3 3 3 3 5))
;-> ("Poker" (3 3 3 3))
(combinazione '(4 4 4 4 4))
;-> ("Pokerissimo" (4 4 4 4 4))

Calcoliamo la probabilità di ogni combinazione.

(define (probabilita iter)
  (local (freq punteggi dadi c val)
    (setq freq (array 9 '(0)))
    (setq punteggi '("Numero piu' alto" "Coppia" "Doppia coppia"
          "Tris" "Scala minima" "Scala massima" "Full" "Poker" "Pokerissimo"))
    (for (i 1 iter)
      (setq dadi (map (curry + 1) (rand 6 5)))
      (setq c (combinazione dadi))
      (setq val (find (c 0) punteggi))
      (++ (freq val)))
    (map (fn(x y) (list x y
                  (float (format "%3.4f" (div y iter)))))
                  punteggi freq)))

(time (println (probabilita 1e6)))
(("Numero piu' alto" 61851 0.0619)
 ("Coppia" 462446 0.4624)
 ("Doppia coppia" 232150 0.2322)
 ("Tris" 153870 0.1539)
 ("Scala minima" 15494 0.0155)
 ("Scala massima" 15573 0.0156)
 ("Full" 38658 0.0387)
 ("Poker" 19213 0.0192)
 ("Pokerissimo" 745 0.0007))
;-> 9841.797000000001

Nota: 'Full' è più frequente delle Scale. 
Se vogliamo che le Scale 'vincano' su 'Full', allora basta cambiare l'ordine dei test nella 'cond'.
      ; Scala massima: 2, 3, 4, 5, 6.
      ((= ordinati '(2 3 4 5 6))
        (list "Scala massima" ordinati))
      ; Scala minima: 1, 2, 3, 4, 5.
      ((= ordinati '(1 2 3 4 5))
        (list "Scala minima" ordinati))
      ; Full: tre valori uguali e due valori uguali.
      ((and (= max-freq 3) (find 2 freq))
        (list "Full" ordinati))
Inoltre bisogna anche cambiare l'ordine della lista punteggi usata da best-roll, altrimenti combinazione e best-roll attribuiscono ranghi diversi alla stessa combinazione:
      '("Numero piu' alto" "Coppia" "Doppia coppia"
        "Tris" " Full" "Scala minima" "Scala massima"
        "Poker" "Pokerissimo")))

Adesso una funzione che prende due lanci e restituisce quello maggiore.
L'algoritmo usato si basa su una semplice idea: trasformare ciascun lancio in una sequenza ordinata che rappresenta completamente il suo punteggio, e poi confrontare le due sequenze.

1) Calcolo delle combinazioni
Per entrambi i lanci viene chiamata 'combinazione'.
Si ottengono quindi:
- il nome della combinazione (Coppia, Tris, Full, ecc.);
- i valori che costituiscono la combinazione.
Per esempio:
  (2 2 4 4 5) -> Doppia coppia -> (2 2 4 4)
  (3 3 4 4 1) -> Doppia coppia -> (3 3 4 4)

2) Confronto del tipo di combinazione
Alle nove combinazioni viene associato implicitamente un ordine:
  Numero piu' alto   0
  Coppia             1
  Doppia coppia      2
  Tris               3
  Scala minima       4
  Scala massima      5
  Full               6
  Poker              7
  Pokerissimo        8
Se i due lanci hanno combinazioni diverse, non serve fare altro: vince quello con il rango maggiore.

3) Caso di combinazioni dello stesso tipo
Se le combinazioni sono dello stesso tipo, bisogna confrontare i valori.
Si parte dai valori restituiti da combinazione e li si ordina in senso decrescente.
Per esempio, per:
  (2 2 4 4 5)
la combinazione è:
  (4 4 2 2)
che viene già trasformata nella sequenza:
(4 4 2 2)

4) Recupero dei valori mancanti
A questo punto si copia il lancio originale.
Per ogni valore presente nella combinazione si elimina una sola occorrenza dalla copia.
Nel caso:
  lancio:       (2 2 4 4 5)
  combinazione: (4 4 2 2)
vengono eliminati:
  4
  4
  2
  2
e rimane:
  (5)
Il valore rimasto è il kicker.

5) Costruzione della sequenza completa
Il kicker viene ordinato in senso decrescente e aggiunto alla sequenza della combinazione.
Quindi:
  (4 4 2 2) + (5)
diventa:
  (4 4 2 2 5)
Per un tris, ad esempio:
  (6 6 6 4 2)
si ottiene:
  (6 6 6 4 2)
Per una coppia:
  (5 5 3 6 3)
si ottiene:
  (5 5 6 3 1)
In pratica, la sequenza contiene prima la combinazione principale, poi tutti i valori secondari, dal maggiore al minore.

6) Confronto finale
Quando entrambe le sequenze sono state costruite, vengono trasformate in stringhe e confrontate.
Per esempio:
  (4 4 2 2 5)
  (4 4 3 3 1)
Il confronto procede implicitamente da sinistra verso destra:
4 = 4
4 = 4
2 < 3
quindi la seconda sequenza è maggiore.

7) Perche' funziona per tutte le combinazioni
Il punto forte dell'algoritmo è che non bisogna scrivere una regola particolare per confrontare i kicker di ogni combinazione.
La stessa procedura funziona automaticamente:
  Coppia: coppia + 3 kicker
  Doppia coppia: coppia maggiore + coppia minore + kicker
  Tris: tris + 2 kicker
  Full: tris + coppia
  Poker: poker + kicker
  Pokerissimo: cinque valori uguali
Anche il Numero piu' alto funziona: la combinazione contiene inizialmente solo il valore massimo, mentre gli altri quattro dadi vengono aggiunti come valori mancanti.
In sostanza, l'algoritmo trasforma ogni lancio in una chiave di confronto ordinata, del tipo:
  tipo di combinazione
          +
  valori significativi in ordine decrescente
          +
  kicker in ordine decrescente
e quindi il confronto finale diventa semplicemente un confronto tra due sequenze.
Il metodo ha il vantaggio che non serve conoscere separatamente le regole di confronto di Coppia e Doppia coppia la struttura di c1 e c2 contiene già i valori prioritari, mentre i valori rimasti sono automaticamente i kicker.
Quando la combinazione è Full, la sequenza s deve essere costruita mettendo prima il tris e poi la coppia, invece di ordinare semplicemente tutti e cinque i dadi in ordine decrescente.

8) Riassunto
Per ciascun lancio:
  1. si prende (c1 1) e lo si ordina decrescente;
  2. si copia il lancio;
  3. per ogni valore presente nella combinazione si elimina una sola occorrenza dal lancio;
  4. ciò che rimane sono i kicker;
  5. i kicker vengono ordinati decrescentemente e aggiunti;
  6. si confrontano le due stringhe.

(define (best-roll dadi1 dadi2)
  ; Calcola le combinazioni dei due lanci.
  (let (c1 c2 r1 r2 s1 s2 tmp tris1 coppia1 tris2 coppia2)
    (setq c1 (combinazione dadi1))
    (setq c2 (combinazione dadi2))
    ; Determina il rango delle combinazioni.
    (setq r1 (find (c1 0)
      '("Numero piu' alto" "Coppia" "Doppia coppia"
        "Tris" "Scala minima" "Scala massima"
        "Full" "Poker" "Pokerissimo")))
    (setq r2 (find (c2 0)
      '("Numero piu' alto" "Coppia" "Doppia coppia"
        "Tris" "Scala minima" "Scala massima"
        "Full" "Poker" "Pokerissimo")))
    ; Se le combinazioni hanno rango diverso, restituisce quella maggiore.
    (cond
      ((> r1 r2) (reverse (sort (copy dadi1))))
      ((< r1 r2) (reverse (sort (copy dadi2))))
      ; A parita' di combinazione costruisce le sequenze di confronto.
      (true
        ; Il Full viene rappresentato prima dal tris e poi dalla coppia.
        (if (= r1 6)
          (begin
            ; Cerca il valore del tris e quello della coppia nel primo lancio.
            (setq tris1 nil)
            (setq coppia1 nil)
            (dolist (x (sequence 1 6))
              (if (= ((count (list x) dadi1) 0) 3)
                  (setq tris1 x))
              (if (= ((count (list x) dadi1) 0) 2)
                  (setq coppia1 x)))
            ; Cerca il valore del tris e quello della coppia nel secondo lancio.
            (setq tris2 nil)
            (setq coppia2 nil)
            (dolist (x (sequence 1 6))
              (if (= ((count (list x) dadi2) 0) 3)
                  (setq tris2 x))
              (if (= ((count (list x) dadi2) 0) 2)
                  (setq coppia2 x)))
            ; Costruisce la sequenza tris + coppia.
            (setq s1 (list tris1 tris1 tris1 coppia1 coppia1))
            (setq s2 (list tris2 tris2 tris2 coppia2 coppia2)))
          (begin
            ; Parte dai valori della combinazione in ordine decrescente.
            (setq s1 (reverse (sort (copy (c1 1)))))
            (setq s2 (reverse (sort (copy (c2 1)))))
            ; Copia il primo lancio.
            (setq tmp (copy dadi1))
            ; Elimina una occorrenza per ogni valore della combinazione.
            (dolist (x (c1 1))
              (pop tmp (find x tmp)))
            ; Aggiunge i valori mancanti in ordine decrescente.
            (extend s1 (reverse (sort tmp)))
            ; Copia il secondo lancio.
            (setq tmp (copy dadi2))
            ; Elimina una occorrenza per ogni valore della combinazione.
            (dolist (x (c2 1))
              (pop tmp (find x tmp)))
            ; Aggiunge i valori mancanti in ordine decrescente.
            (extend s2 (reverse (sort tmp)))))
        ; Se le sequenze sono identiche, i due punteggi sono pari.
        (if (= s1 s2)
            nil
            ; Altrimenti restituisce la sequenza maggiore.
            (if (> (string s1) (string s2))
              s1
              s2))))))

Proviamo:

(best-roll '(2 2 4 4 5) '(3 3 4 4 1))
;-> (4 4 3 3 1)
(best-roll '(4 4 4 4 3) '(4 4 4 4 2))
;-> (4 4 4 4 3)
(best-roll '(2 2 4 4 5) '(3 3 4 4 1))
;-> (4 4 3 3 1)
(best-roll '(2 5 2 4 4) '(2 2 4 4 1))
;-> (4 4 2 2 5)
(best-roll '(2 2 1 6 3) '(4 2 3 4 5))
;-> (4 4 5 3 2)
(best-roll '(1 1 1 1 1) '(2 2 2 2 2))
;-> (2 2 2 2 2)
(best-roll '(1 1 2 1 3) '(3 2 1 2 2))
;-> (2 2 2 3 1)
(best-roll '(4 4 4 1 2) '(4 4 4 1 5))
;-> 4 4 4 5 1
(best-roll '(1 1 3 1 3) '(1 2 1 1 2))
;-> (1 1 1 3 3)
(best-roll '(6 6 2 6 2) '(1 5 1 1 5))
;-> (6 6 6 2 2)
(best-roll '(3 3 3 4 4) '(1 5 1 1 5))
;-> (3 3 3 4 4)
(best-roll '(1 2 3 4 6) '(6 4 3 2 1))
;-> nil
(best-roll '(2 2 4 4 5) '(4 5 2 4 2))
;-> nil
(best-roll '(4 4 2 2 6) '(4 4 3 3 1))
;-> (4 4 3 3 1)
(best-roll '(4 4 2 2 6) '(2 2 4 4 5))
;-> (4 4 2 2 6)
(best-roll '(3 3 3 6 1) '(3 3 3 5 4))
;-> (3 3 3 6 1)
(best-roll '(3 3 3 4 4) '(3 3 3 2 2))
;-> (3 3 3 4 4)
(best-roll '(2 2 2 6 6) '(2 2 2 3 3))
;-> (2 2 2 6 6)
(best-roll '(4 4 4 4 2) '(4 4 4 4 6))
;-> (4 4 4 4 6)
(best-roll '(6 5 4 2 1) '(6 5 3 2 1))
;-> (6 5 4 2 1)
(best-roll '(6 4 3 2 1) '(6 5 4 2 1))
;-> (6 5 4 2 1)
(best-roll '(1 2 3 4 5) '(2 3 4 5 6))
;-> (6 5 4 3 2)
(best-roll '(5 1 4 2 3) '(6 3 5 2 4))
;-> (6 3 5 2 4)


--------------------
Messaggio di Arecibo
--------------------

https://it.wikipedia.org/wiki/Messaggio_di_Arecibo

Il messaggio di Arecibo è un messaggio radio trasmesso nello spazio dal radiotelescopio di Arecibo, in Porto Rico, il 16 novembre 1974.
È stato indirizzato verso l'Ammasso Globulare di Ercole (M13), a 25 000 anni luce di distanza.
La scelta di M13 è legata al fatto che si tratta di ampia costellazione, relativamente stabile e visibile nel cielo al tempo della cerimonia.

Il messaggio è composto da 1679 cifre binarie, numero appositamente scelto in quanto prodotto di due numeri primi (23 e 73).
In questo modo, presupponendo che chiunque lo riceva decida di ordinarlo in un quadrilatero, potrà farlo soltanto ordinandolo in 23 righe e 73 colonne o 73 righe e 23 colonne.
L'informazione così sistemata nella prima disposizione (23 righe, 73 colonne) produce un disegno senza senso, ma nel secondo modo (73 righe, 23 colonne), se correttamente disposto in caratteri e spaziature, forma un'immagine nella quale si possono riconoscere delle informazioni.

Il file dei dati "arecibo.lsp" si trova nella cartella "data".
; Carica una lista di nome 'data'
; con i valori binari del messaggio di Arecibo
(load "arecibo.lsp")

(define (print-grid grid ch0 ch1 coord)
"Print a matrix with only digits (0..9)"
  (local (row col)
    (setq row (length grid))
    (setq col (length (first grid)))
    ; indici di colonna della griglia
    (if coord
        (println "  " (join (map (fn(x) (format "%2d" x)) (sequence 0 (- col 1))))))
    (for (i 0 (- row 1))
      ; indice di riga della griglia
      (if coord (print (format "%2d" i)))
      ; stampa della griglia
      (for (j 0 (- col 1))
        (if (and (!= ch0 "") (!= ch1 ""))
            (begin
              (cond ((= (grid i j) 0) (print ch0))
                    ((= (grid i j) 1) (print ch1))
                    (true
                      (print (format "%2d" (grid i j))))))
            ;else
            (print (format "%2d" (grid i j)))))
      (println))))

(print-grid data " " "█")
(print-grid data " " "■")
(print-grid data " " "*")
;->       * * * *
;->   * *     * *       *
;-> *   *   *   *  * **  *
;-> * * * * * * * *  *  *
;-> 
;->             **
;->           ** *
;->           ** *
;->          * * *
;->          *****
;-> 
;-> **    ***   **    **
;-> *             **  *
;-> ** *   **   **    ** *
;-> ***** ***** ***** *****
;-> 
;->    *                 *
;-> 
;->     *                 *
;-> *****             *****
;-> 
;-> **    **    ***   **
;-> *       *         *
;-> ** *    **   ***  ** *
;-> ***** ***** ***** *****
;-> 
;->    *      **         *
;->           **
;->     *     **          *
;-> *****     **      *****
;->           **
;->   *        *        *
;->    *      **       *
;->     **    **      *
;->       **   *    **
;->           **  **
;->       **   *    **
;->     **    **      *
;->    *      *        *
;->   *       **        *
;->  *        **        *
;->  *         *       *
;->   *       *       *
;->    *            **
;->     **        **
;->   *   *** * **
;->   *       *
;->   *     *****
;->   *    * *** *  * ** **
;->       *  ***  *  ******
;-> * ***    ***     ** ***
;->          * *     *** **
;->   *      * *     ******
;->   *      * *     **
;->   *     ** **
;-> 
;->   ***     *
;->   *** * *   * * * * * *
;->   ***         * * * *
;->               * *
;->         *****
;->       *********
;->     ***       ***
;->    **           **
;->   ** *         * **
;->  **  **       **  **
;->  *   * *     * *   *
;->  *   *  *   *  *   *
;->      *   * *   *
;->      *    *    *
;->      *         *
;->        *  * *
;->  ****  ***** *  ****

============================================================================

