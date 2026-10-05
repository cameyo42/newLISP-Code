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


---------------------
Sul bordo del burrone
---------------------

Un uomo ubriaco si trova sul bordo di un burrone.
Un solo passo in avanti lo farebbe precipitare.
L'uomo ha una probabilità 'p' di fare un passo indietro e una probabilità '(1 - p)' di fare un passo in avanti.
Se l'uomo fa N passi, qual è la sua probabilità di sopravvivenza (cioè la probabilità di non cadere nel burrone)?

Poniamo l'uomo nella posizione 0 della linea dei numeri naturali.
Finchè la sua posizione è maggiore di 0, l'uomo è sopra il burrone.
Se la sua posizione diventa minore di 0, allora è caduto nel burrone.

     -1    0    1    2    3    4
      +----+----+----+----+----+--...

(define (uomo p N iter)
  (local (morto vivo pos-vivo passi-burrone pos passi burrone)
    (setq morto 0)
    (setq vivo 0)
    (setq pos-vivo 0)
    (setq passi-burrone 0)
    (for (i 1 iter)
      (setq pos 0)
      (setq passi 0)
      (setq burrone nil)
      (while (and (< passi N) (not burrone))
        (if (> (random) p)
            (-- pos)
            (++ pos))
        (++ passi)
        (if (= pos -1) (setq burrone true)))
      (if burrone
        (begin
          (++ passi-burrone passi)
          (++ morto))
        ;else
        (begin
          (++ pos-vivo pos)
          (++ vivo))))
    ;(println "passi-burrone: " passi-burrone)
    (println "media passi-burrone: " (div passi-burrone morto))
    ;(println "pos-vivo: " pos-vivo)
    (println "media pos-vivo: " (div pos-vivo vivo))
    (list (div vivo iter) (div morto iter))))

Proviamo:

(seed (time-of-day) true)

(uomo 0.45 1 1e5)
;-> media passi-burrone: 1
;-> media pos-vivo: 1
;-> (0.45017 0.54983)
(uomo 0.45 10 1e5)
;-> media passi-burrone: 2.251338456495611
;-> media pos-vivo: 2.626240853139713
;-> (0.17629 0.8237100000000001)
(uomo 0.45 100 1e5)
;-> media passi-burrone: 6.674820261105114
;-> media pos-vivo: 7.982241953385127
;-> (0.01802 0.98198)
(uomo 0.45 1000 1e5)
;-> media passi-burrone: 9.968619372387447
;-> media pos-vivo: 16
;-> (2e-005 0.99998)
(uomo 0.45 10000 1e5)
;-> media passi-burrone: 9.87182
;-> media pos-vivo: -1.#IND
;-> (0 1)
(uomo 0.45 100000 1e5)
;-> media passi-burrone: 10.0058
;-> media pos-vivo: -1.#IND
;-> (0 1)

(uomo 0.5 1 1e5)
;-> media passi-burrone: 1
;-> media pos-vivo: 1
;-> (0.50196 0.49804)
(uomo 0.5 100 1e5)
;-> media passi-burrone: 7.641195183653085
;-> media pos-vivo: 11.55096872229961
;-> (0.07897 0.92103)
(uomo 0.5 1000 1e5)
;-> media passi-burrone: 25.29348998182248
;-> media pos-vivo: 38.76284735439665
;-> (0.02627 0.97373)
(uomo 0.5 10000 1e5)
;-> media passi-burrone: 82.65733229879382
;-> media pos-vivo: 124.2128777923785
;-> (0.00761 0.99239)
(uomo 0.5 100000 1e5)
;-> media passi-burrone: 247.3373933616032
;-> media pos-vivo: 390.2672064777328
;-> (0.00247 0.99753)

(uomo 0.666667 1 1e5)
;-> media passi-burrone: 1
;-> media pos-vivo: 1
;-> (0.66467 0.33533)
(uomo 0.666667 100 1e5)
;-> media passi-burrone: 3.019112627986348
;-> media pos-vivo: 35.29758916118749
;-> (0.5019 0.4981)
(uomo 0.666667 1000 1e5)
;-> media passi-burrone: 3.010033444816054
;-> media pos-vivo: 335.1264668059798
;-> (0.49768 0.50232)
(uomo 0.666667 10000 1e5)
;-> media passi-burrone: 3.015092061575611
;-> media pos-vivo: 3335.307862041547
;-> (0.50305 0.49695)

(uomo 0.75 1 1e5)
;-> media passi-burrone: 1
;-> media pos-vivo: 1
;-> (0.74973 0.25027)
(uomo 0.75 100 1e5)
;-> media passi-burrone: 2.000656363744854
;-> media pos-vivo: 51.02144941487921
;-> (0.66482 0.33518)
(uomo 0.75 1000 1e5)
;-> media passi-burrone: 2.005761267478845
;-> media pos-vivo: 500.8978012418634
;-> (0.66674 0.33326)
(time (println (uomo 0.75 10000 1e5)))
;-> passi-burrone: 66518
;-> media passi-burrone: 1.992869554796573
;-> pos-vivo: 333188126
;-> media pos-vivo: 5001.172675692714
;-> (66622 0.66622 33378 0.33378)
;-> 113021.749

(uomo 0.8 1 1e5)
;-> media passi-burrone: 1
;-> media pos-vivo: 1
;-> (0.79836 0.20164)
(uomo 0.8 100 1e5)
;-> media passi-burrone: 1.683722417652702
;-> media pos-vivo: 60.69812226608343
;-> (0.74984 0.25016)
(uomo 0.8 1000 1e5)
;-> media passi-burrone: 1.672666639881072
;-> media pos-vivo: 600.590512707859
;-> (0.7511100000000001 0.24889)


--------------------------
Random-walk in una matrice
--------------------------

Abbiamo una matrice MxN con tutti 0.
Dalla cella (r c) parte un punto che si muove casualmente in una delle quattro direzione Nord, Sud, Est o Ovest.
Il punto si ferma quando è passato almeno una volta su tutte le celle.

Scriviamo una funzione che prende M, N, r, c e restituisce una matrice MxN in cui ogni cella contiene il numero di volte che il punto l'ha occupata (hot-map).

(define (random-walk M N r c)
  ; Crea la matrice MxN inizializzata a zero.
  (letn ((matrice (array M N '(0)))
         (visitati 0)
         (totale (* M N))
         (nr r)
         (nc c)
         (dir '())
         (passi 0))
    ; Conta la cella di partenza.
    (setf (matrice nr nc) 1)
    (setq visitati 1)
    ; Continua finche' tutte le celle sono state visitate almeno una volta.
    (while (< visitati totale)
      ; Costruisce l'elenco delle direzioni che restano nella matrice.
      (setq dir '())
      (if (> nr 0)
          (push 'N dir))
      (if (< nr (- M 1))
          (push 'S dir))
      (if (> nc 0)
          (push 'O dir))
      (if (< nc (- N 1))
          (push 'E dir))
      ; Sceglie casualmente una delle direzioni valide.
      (setq dir (dir (rand (length dir))))
      ; Esegue il movimento.
      (cond
        ((= dir 'N)
          (-- nr))
        ((= dir 'S)
          (++ nr))
        ((= dir 'O)
          (-- nc))
        ((= dir 'E)
          (++ nc)))
      ; Incrementa il numero di passi
      (++ passi)
      ; Incrementa il numero di occupazioni della nuova cella.
      (++ (matrice nr nc))
      ; Se questa e' la prima visita, incrementa il numero di celle visitate.
      (if (= (matrice nr nc) 1)
          (++ visitati)))
    (list passi matrice)))

Proviamo:

(seed (time-of-day) true)

(random-walk 3 4 0 0)
;-> (45 ((5 6 6 2) (6 5 4 2) (2 4 3 1)))
45 sono i passi, mentre le celle occupate sono 46.
Esempio:
A->B->C
Parto da A, per arrivare a C occorrono 2 passi, ma occupo 3 caselle (A, B e C).

(random-walk 10 10 4 4)
;-> (781 ((2 2 4 4 4 9 11 12 12 12)
;->       (3 3 1 2 7 13 17 16 17 15)
;->       (5 5 2 7 10 8 15 20 17 11)
;->       (2 2 4 5 8 11 10 14 9 10)
;->       (9 13 8 8 8 7 4 9 6 5)
;->       (11 11 10 4 4 6 5 6 6 3)
;->       (14 14 15 6 4 4 4 5 5 3)
;->       (19 21 13 8 5 5 3 6 5 4)
;->       (13 14 10 9 8 10 7 6 12 10)
;->       (3 4 4 3 4 4 4 2 4 4)))

; Calcola la media dei passi necessari per visitare casualmente
; una matrice MxN partendo dalla cella (r c).
(define (media-passi M N r c iter)
  (let (totale-passi 0)
    (for (i 1 iter)
      (++ totale-passi ((random-walk M N r c) 0)))
    (div totale-passi iter)))

; 3x3 partendo dal centro
(media-passi 3 3 1 1 1e6)
;-> 33.354145
; 3x3 partendo da uno spigolo
(media-passi 3 3 0 0 1e6)
;-> 31.173233

; 10x10 partendo dal centro
(media-passi 10 10 4 4 1e5)
;-> 1296.96354
; 10x10 partendo da uno spigolo
(media-passi 10 10 0 0 1e5)
;-> 1249.36952


------------------------------
Ritorno a casa in N dimensioni
------------------------------

Abbiamo un punto posizionato a 0 sulla linea dei numeri interi.
Il punto si muove casualmente a destra (+1) o a sinistra (-1) per N passi.
Comunque se il punto ritorna nella posizione 0, si ferma.

Scriviamo una funzione che simula questo random-walk e ritorna il valore dei 'passi' necessari per tornare al punto di partenza (0).
Se il punto non torna a zero entro N passi, allora restituiamo N.
Il valore dei 'passi' vale:
  Numero di passi per tornare a 0
oppure
  N (perchè il punto non è ritornato a 0 in N passi)

; random-walk return (1D)
(define (walk1D N)
  ; Le due possibili mosse del punto.
  (letn ((mosse '(-1 1))
         (pos 0)
         (passi 0)
         (fine nil))
    ; Esegue al massimo N passi.
    (for (i 1 N 1 fine)
      ; Aggiorna la posizione con una mossa casuale.
      (++ pos (mosse (rand 2)))
      ; Memorizza il numero di passi eseguiti.
      (setq passi i)
      ; Interrompe il ciclo quando si torna a 0.
      (if (= pos 0)
        (setq fine true)))
    passi))

Il ritorno a 0 può avvenire solo dopo un numero pari di passi.
Quindi non potrà mai restituire 3, 5, 7, ecc.
Sfruttiamo questo fatto per evitare il caso in cui la funzione restituisce N:
in questo caso non sappiamo se il punto non è ritornato a 0 in N mosse oppure è ritornato a 0 proprio con N mosse.
Passando un numero N dispari siamo sicuri che se la funzione restituisce N, allora il punto non è ritornato a 0.

(collect (walk1D 1001) 10)
;-> (2 212 6 6 218 64 4 2 2 2)

Per la versione 2D usiamo esattamente la stessa logica, ma ora la posizione è una coppia (riga colonna).
Ad ogni passo scegliamo casualmente una delle quattro direzioni: alto, basso, sinistra, destra.

; random-walk return (2D)
(define (walk2D N)
  ; Le quattro possibili mosse: su, giu', sinistra, destra.
  (letn ((mosse '((-1 0) (1 0) (0 -1) (0 1)))
         (x 0)
         (y 0)
         (passi 0)
         (fine nil))
    ; Esegue al massimo N passi.
    (for (i 1 N 1 fine)
      ; Sceglie casualmente una delle quattro direzioni.
      (let ((mossa (mosse (rand 4))))
        ; Aggiorna la posizione nella direzione scelta.
        (++ x (mossa 0))
        (++ y (mossa 1)))
      ; Memorizza il numero di passi eseguiti.
      (setq passi i)
      ; Se il punto e' tornato all'origine, termina.
      (if (and (= x 0) (= y 0))
        (setq fine true)))
    passi))

(collect (walk2D 1001) 10)
;-> (4 2 2 54 1001 2 1001 1001 40 1001)

Anche in 2D un ritorno all'origine può avvenire soltanto dopo un numero pari di passi.
Quindi possiamo continuare a usare N dispari per eliminare l'ambiguità del risultato N.

La versione 3D segue la stessa struttura: la posizione è (x y z) e ci sono 6 mosse possibili, una per ciascun asse e verso.

(define (walk3D N)
  ; Le sei possibili mosse: destra, sinistra, avanti, indietro, su, giu'.
  (letn ((mosse '((1 0 0) (-1 0 0) (0 1 0) (0 -1 0) (0 0 1) (0 0 -1)))
         (x 0)
         (y 0)
         (z 0)
         (passi 0)
         (fine nil))
    ; Esegue al massimo N passi.
    (for (i 1 N 1 fine)
      ; Sceglie casualmente una delle sei direzioni.
      (let ((mossa (mosse (rand 6))))
        ; Aggiorna la posizione nella direzione scelta.
        (++ x (mossa 0))
        (++ y (mossa 1))
        (++ z (mossa 2)))
      ; Memorizza il numero di passi eseguiti.
      (setq passi i)
      ; Se il punto e' tornato all'origine, termina.
      (if (and (= x 0) (= y 0) (= z 0))
        (setq fine true)))
    passi))

Anche qui, come in 1D e 2D, il ritorno all'origine può avvenire solo dopo un numero pari di passi.
Quindi usando N dispari, N identifica senza ambiguità il caso in cui il punto non è mai tornato all'origine.

(collect (walk3D 1001) 10)
;-> (1001 1001 1001 1001 1001 1001 2 8 1001 1001)

Vediamo quante volte torniamo a 0 con N passi.

(define (ritorno func N prove)
  (let (sim (collect (func N) prove))
    (- prove (length (find-all N sim)))))

Caso 1D
-------
N = 11, prove = 100
(ritorno walk1D 11 100)
;-> 73
N = 101, prove = 100
(ritorno walk1D 101 100)
;-> 88
N = 1001, prove = 100
(ritorno walk1D 1001 100)
;-> 99
N = 10001, prove = 100
(ritorno walk1D 10001 100)
;-> 100

In 1D all'aumentare del numero dei passi N si ritorna a 0 il 100% delle volte. 

Caso 2D
-------
N = 11, prove = 100
(ritorno walk2D 11 100)
;-> 46
N = 101, prove = 100
(ritorno walk2D 101 100)
;-> 56
N = 1001, prove = 100
(ritorno walk2D 1001 100)
;-> 72
N = 10001, prove = 100
(ritorno walk2D 10001 100)
;-> 78
(ritorno walk2D 100001 100)
;-> 79
(ritorno walk2D 1000001 100)
;-> 80
(ritorno walk2D 10000001 100)
;-> 86
(ritorno walk2D 100000001 100)

In 2D all'aumentare del numero dei passi N si ritorna a 0 intorno all'85% delle volte. 

Caso 3D
-------
N = 11, prove = 100
(ritorno walk3D 11 100)
;-> 24
N = 101, prove = 100
(ritorno walk3D 101 100)
;-> 30
N = 1001, prove = 100
(ritorno walk3D 1001 100)
;-> 34
N = 10001, prove = 100
(ritorno walk3D 10001 100)
;-> 31
(ritorno walk3D 100001 100)
;-> 34
(ritorno walk3D 1000001 100)
;-> 33
(ritorno walk3D 10000001 100)
;-> 35

In 3D all'aumentare del numero dei passi N si ritorna a 0 intorno al 30-35% delle volte. 

Caso: dimensione arbitraria 
---------------------------
Possiamo generalizzare direttamente a una dimensione arbitraria D.

(define (walk D N)
  ; Crea le 2*D possibili mosse.
  ; Per ogni dimensione esistono una mossa +1 e una mossa -1.
  (letn ((mosse '())
         (pos (dup 0 D))
         (mossa '())
         (passi 0)
         (fine nil))
    ; Costruisce le mosse per ogni dimensione.
    (for (d 0 (- D 1))
      ; Costruisce la mossa positiva.
      (setq mossa (dup 0 D))
      (setf (mossa d) 1)
      (push mossa mosse -1)
      ; Costruisce la mossa negativa.
      (setq mossa (dup 0 D))
      (setf (mossa d) -1)
      (push mossa mosse -1))
    ; Esegue al massimo N passi.
    (for (i 1 N 1 fine)
      ; Sceglie casualmente una delle 2*D direzioni.
      (setq mossa (mosse (rand (* 2 D))))
      ; Aggiorna tutte le coordinate.
      (for (d 0 (- D 1))
        (++ (pos d) (mossa d)))
      ; Memorizza il numero di passi eseguiti.
      (setq passi i)
      ; Controlla se il punto e' tornato all'origine.
      (if (= pos (dup 0 D))
        (setq fine true)))
    passi))

Proviamo:

(collect (walk 1 11) 10)
;-> (8 6 2 11 2 2 11 11 11 11)

(define (return D N prove)
  (let (sim (collect (walk D N) prove))
    (- prove (length (find-all N sim)))))

Proviamo:

(return 1 100001 100)
;-> 100
(return 2 100001 100)
;-> 85
(return 3 100001 100)
;-> 35


--------------------
Attesa per una carta
--------------------

Abbiamo un mazzo di N carte.
Una certa carta X compare K volte nel mazzo.
Mischiamo il mazzo e poi prendiamo una carta dalla cima.
Quante carte, in media, dobbiamo prendere prima di ottenere una carta X?

A) Soluzione con simulazione

(define (simula N K iter)
  (let ((totale 0)
        (carte (append (dup 0 (- N K)) (dup 1 K))))
    (for (i 1 iter)
      (setq carte (randomize carte true))
      (++ totale (find 1 carte)))
    (div totale iter)))

(seed (time-of-day) true)

(simula 52 4 1e6)
;-> 9.597951

B) Soluzione matematica

Le K carte uguali dividono il mazzo in (K+1) blocchi di carte.
La lunghezza di questi blocchi varia da 0 a (N-K).
Il principio di simmetria dice che (K+1) blocchi hanno una media pari a:

  media = (N - K)/(K + 1)

(define (media N K)
  (div (- N K) (+ K 1)))

(media 52 4)
;-> 9.6

(simula 100 11 1e6)
;-> 7.407334
(media 100 11)
;-> 7.416666666666667


---------------------------------------------
Il gioco del Wari (Awari-Oware-Awele-Mancala)
---------------------------------------------

"How to play Warri" David Chamberlin

Il Wari (noto anche come Awari o Oware o Awélé) è un antico e diffuso gioco da tavolo astratto della famiglia dei mancala, basato sulla logica e sul calcolo senza alcuna componente di fortuna.

Struttura del Gioco
-------------------
a) Il tabellone: È composto da due file di sei piccole cavità chiamate case (o buche) e due grandi cavità alle estremità chiamate granai (o depositi).
Ciascun giocatore controlla la fila di sei case dal proprio lato e il granaio alla propria destra.

b) I pezzi: Si usano 48 semi (o sassolini), disposti inizialmente in numero di 4 in ciascuna delle 12 case.

c) Obiettivo: Catturare più semi dell'avversario.
Poiché i semi in totalità sono 48, vince chi per primo ne raccoglie 25 o più nel proprio granaio.

Come si Gioca
-------------
1. Il turno: I giocatori si alternano muovendo uno alla volta.
Nel proprio turno, un giocatore sceglie una qualsiasi delle proprie sei case e prende tutti i semi contenuti all'interno.

2. La semina: Il giocatore distribuisce i semi uno alla volta nelle case successive procedendo in senso antiorario.
Se il giro è lungo e supera il numero di case, la casa di partenza (da cui sono stati presi i semi) viene saltata e lasciata vuota.

3. La cattura (raccolta): La cattura avviene se l'ultimo seme seminato cade in una casa dell'avversario che, dopo l'inserimento, contiene un totale di 2 o 3 semi. In questo caso, il giocatore raccoglie quei semi e li mette nel proprio granaio.

4. Catena di cattura: Se la casa immediatamente precedente (sempre andando a ritroso in senso orario) contiene anch'essa 2 o 3 semi, anche questi vengono catturati e così via, finché si incontrano case con un numero diverso di semi o case del proprio lato.

Regole Speciali
---------------
a) La carestia: Se un giocatore rimane senza semi nelle proprie case, l'avversario ha l'obbligo (se possibile) di effettuare una mossa che gli restituisca almeno un seme.
Se non è possibile, la partita finisce e i semi rimasti sul tabellone vengono presi dal giocatore che li possiede.

b) Fine della partita:
Il gioco termina quando:
a) un giocatore raggiunge i 25 semi.
b) la posizione si ripete (ciclo). Ciascun giocatore prende i semi rimasti sul proprio lato.
c) Quando un giocatore non ha più semi. L'altro giocatore prende i semi del proprio lato.

Notazione del Wari
------------------
    a   b   c   d   e   f
  +---+---+---+---+---+---+
  |   |   |   |   |   |   |
  +---+---+---+---+---+---+
  |   |   |   |   |   |   |
  +---+---+---+---+---+---+
    F   E   D   C   B   A

Rappresentazione della tavola del Wari
--------------------------------------
       a        b        c        d        e        f      --> lettere
      11       10        9        8        7        6      --> indici
  +--------+--------+--------+--------+--------+--------+
  |        |        |        |        |        |        | ---> Case Nord (Y)
  |        |        |        |        |        |        |   0 --> Granaio Nord
  |        |        |        |        |        |        |
  +--------+--------+--------+--------+--------+--------+

  +--------+--------+--------+--------+--------+--------+
  |        |        |        |        |        |        | ---> Case Sud (X)
  |        |        |        |        |        |        |   0 --> Granaio Sud
  |        |        |        |        |        |        |         (X)
  +--------+--------+--------+--------+--------+--------+
       0        1        2        3        4        5      --> indici
       F        E        D        C        B        A      --> lettere

Vediamo alcune funzioni che ci permettono di giocare a Wari in modo interattivo.

; Inizia una nuova partita
(define (setup lst g1 g2)
  ; Inizializza la tavola del gioco
  (setq board (if lst lst (dup 4 12)))
  ; Memorizza la tavola corrente (for undo)
  (setq old-board board)
  ; Inizializza i granai
  (if lst 
    (begin
      ; Granaio del giocatore Sud (0 1 2 3 4 5)
      (setq granaio1 (or g1 0))
      ; Granaio del giocatore Nord (6 7 8 9 7 6)
      (setq granaio2 (or g2 0)))
    ;else
    (begin
      (setq granaio1 0)
      (setq granaio2 0)))
  ; Memorizza il granaio 1 (for undo)
  (setq old-granaio1 granaio1)
  ; Memorizza il granaio 2 (for undo)
  (setq old-granaio2 granaio2)
  ; Giocatore corrente
  (setq player nil)
  ; Stampa della tavola iniziale
  (print-board board granaio1 granaio2))

; Converte la lettera della casa in indice della casa
(define (lettera-indice lettera)
  ; 11 10 9 8 7 6
  ;  a  b c d e f
  ; 0 1 2 3 4 5
  ; F E D C B A
  (lookup lettera '(("A" 5) ("B" 4) ("C" 3) ("D" 2) ("E" 1) ("F" 0)
                    ("a" 11) ("b" 10) ("c" 9) ("d" 8) ("e" 7) ("f" 6)
                    (A 5) (B 4) (C 3) (D 2) (E 1) (F 0)
                    (a 11) (b 10) (c 9) (d 8) (e 7) (f 6))))

; Converte l'indice della casa in lettera della casa
(define (indice-lettera indice)
  ; 11 10 9 8 7 6
  ;  a  b c d e f
  ; 0 1 2 3 4 5
  ; F E D C B A
  (lookup indice '((5 "A") (4 "B") (3 "C") (2 "D") (1 "E") (0 "F")
                  (11 "a") (10 "b") (9 "c") (8 "d") (7 "e") (6 "f")
                  (5 A) (4 B) (3 C) (2 D) (1 E) (0 F)
                  (11 a) (10 b) (9 c) (8 d) (7 e) (6 f))))

; Annulla l'ultima mossa (solo una)
; (utilizzabile dopo la 'semina' o dopo la 'raccolta')
(define (undo)
  (setq granaio1 old-granaio1)
  (setq granaio2 old-granaio2)
  (setq board old-board)
  (print-board board granaio1 granaio2))

; Stampa la posizione corrente della tavola
(define (print-board board g1 g2)
  (let ((border "  +---+---+---+---+---+---+")
        (top    "    a   b   c   d   e   f  ")
        ;(top    "   11  10   9   8   7   6")
        ;(bottom "    0   1   2   3   4   5  "))
        (bottom "    F   E   D   C   B   A  "))
  (println top) (println border)
  (println "  "
           (join (reverse (slice (map (fn(x) (format "|%2d " x)) board) 6)))
           (format "|%4d" g2))
  (println border)
  (println "  "
           (join (slice (map (fn(x) (format "|%2d " x)) board) 0 6))
           (format "|%4d" g1))
  (println border) (println bottom) '>))

; Effettua l'operazione di semina
(define (semina lettera)
  (setq idx-casa (lettera-indice lettera))
  (if (zero? (board idx-casa))
    (begin
      (println "Semina impossibile: la casa " lettera " non ha semi.")
      (print-board board granaio1 granaio2)'>)
  ;else
    (let ((semi (board idx-casa)) ; numeri di semi nella casa di partenza
          (k 1) ; contatore
          (indice 0)) ; indice corrente
      ; giocatore corrente (1 o 2)
      (setq player (if (< idx-casa 6) 1 2))
      (println "Semina del giocatore: " (if (= player 1) "Sud" "Nord"))
      (println "Casa: " lettera ", Semi: " semi)
      ; Memorizza la tavola corrente (for undo)
      (setq old-board board)
      ; Azzera il numero di semi della casa di partenza
      (setf (board idx-casa) 0)
      ; Spostamento dei semi:
      ; posiziona i semi nelle case successive (1 per ogni casa)
      ; in senso antiorario (saltando sempre la casa di partenza)
      (setq k 1)
      (until (zero? semi)
        (setq indice (% (+ idx-casa k) 12))
        (when (!= indice idx-casa)
            (++ (board (% (+ idx-casa k) 12)))
            (-- semi))
        (++ k))
      (print-board board granaio1 granaio2)
      ; 'indice' è l'ultima casa visitata
      ; (dove è stato posto l'ultimo seme della semina)
      (check-raccolta (indice-lettera indice)))))

; Controlla se esiste un giocatore che può effettuare la raccolta
(define (check-raccolta lettera)
  (let (idx-casa (lettera-indice lettera))
    (cond ((and (= player 2) (< idx-casa 6)
                (or (= (board idx-casa) 2) (= (board idx-casa) 3)))
            (println "Il giocatore Nord può effettuare la raccolta dalla casa: " lettera))
          ((and (= player 1) (> idx-casa 5)
                (or (= (board idx-casa) 2) (= (board idx-casa) 3)))
            (println "Il giocatore Sud può effettuare la raccolta dalla casa: " lettera))
          (true (println "Nessuna raccolta possibile dalla casa: " lettera "."))) '>))

; Controlla se esiste un giocatore in carestia
(define (check-carestia)
  (if (zero? (apply + (slice board 0 6)))
        (println "Il giocatore Sud è in carestia."))
  (if (zero? (apply + (slice board 6)))
        (println "Il giocatore Nord è in carestia.")) '>)

; Controlla se uno dei giocatori ha vinto
(define (game-over?)
  (cond ((> granaio1 24)
          (println "Il giocatore Sud ha vinto: " granaio1 " - "granaio2))
        ((> granaio2 24)
          (println "Il giocatore Nord ha vinto: " granaio2 " - "granaio1))) '>)

; Effettua l'operazione di raccolta
(define (raccolta lettera)
  (setq idx-casa (lettera-indice lettera))
  (println "Raccolta del giocatore: " (if (= player 1) "Sud" "Nord"))
  (setq old-granaio1 granaio1)
  (setq old-granaio2 granaio2)
  (cond ((and (= player 2) (< idx-casa 6)
              (or (= (board idx-casa) 2) (= (board idx-casa) 3)))
              ; operazione di cattura del giocatore Nord
              (let (stop nil)
                (for (idx idx-casa 0 -1 stop)
                  (if (or (= (board idx) 2) (= (board idx) 3))
                    (begin
                      (++ granaio2 (board idx))
                      (setq (board idx) 0))
                    ;else
                    (setq stop true)))))
        ((and (= player 1) (> idx-casa 5)
              (or (= (board idx-casa) 2) (= (board idx-casa) 3)))
              ; operazione di cattura del giocatore Sud
              (let (stop nil)
                (for (idx idx-casa 6 -1 stop)
                  (if (or (= (board idx) 2) (= (board idx) 3))
                    (begin
                      (++ granaio1 (board idx))
                      (setq (board idx) 0))
                    ;else
                    (setq stop true)))))
        (true (println "Nessuna raccolta possibile.")))
  (print-board board granaio1 granaio2)
  (check-carestia)
  (game-over?))

Proviamo:

(setup)
;->     a   b   c   d   e   f
;->   +---+---+---+---+---+---+
;->   | 4 | 4 | 4 | 4 | 4 | 4 |   0
;->   +---+---+---+---+---+---+
;->   | 4 | 4 | 4 | 4 | 4 | 4 |   0
;->   +---+---+---+---+---+---+
;->     F   E   D   C   B   A

(semina 'F)
;-> Semina del giocatore: Sud
;-> Casa: F, Semi: 4
;->     a   b   c   d   e   f
;->   +---+---+---+---+---+---+
;->   | 4 | 4 | 4 | 4 | 4 | 4 |   0
;->   +---+---+---+---+---+---+
;->   | 0 | 5 | 5 | 5 | 5 | 4 |   0
;->   +---+---+---+---+---+---+
;->     F   E   D   C   B   A
;-> Nessuna raccolta possibile dalla casa: B.

(semina 'a)
;-> Semina del giocatore: Nord
;-> Casa: a, Semi: 4
;->     a   b   c   d   e   f
;->   +---+---+---+---+---+---+
;->   | 0 | 4 | 4 | 4 | 4 | 4 |   0
;->   +---+---+---+---+---+---+
;->   | 1 | 6 | 6 | 6 | 5 | 4 |   0
;->   +---+---+---+---+---+---+
;->     F   E   D   C   B   A
;-> Nessuna raccolta possibile dalla casa: C.

(undo)
;->     a   b   c   d   e   f
;->   +---+---+---+---+---+---+
;->   | 4 | 4 | 4 | 4 | 4 | 4 |   0
;->   +---+---+---+---+---+---+
;->   | 0 | 5 | 5 | 5 | 5 | 4 |   0
;->   +---+---+---+---+---+---+
;->     F   E   D   C   B   A

(setup '(1 2 3 4 5 6 2 2 2 2 2 2))
;->     a   b   c   d   e   f
;->   +---+---+---+---+---+---+
;->   | 2 | 2 | 2 | 2 | 2 | 2 |   0
;->   +---+---+---+---+---+---+
;->   | 1 | 2 | 3 | 4 | 5 | 6 |   0
;->   +---+---+---+---+---+---+
;->     F   E   D   C   B   A

(semina 'A)
;-> Semina del giocatore: Sud
;-> Casa: A, Semi: 6
;->     a   b   c   d   e   f
;->   +---+---+---+---+---+---+
;->   | 3 | 3 | 3 | 3 | 3 | 3 |   0
;->   +---+---+---+---+---+---+
;->   | 1 | 2 | 3 | 4 | 5 | 0 |   0
;->   +---+---+---+---+---+---+
;->     F   E   D   C   B   A
;-> Il giocatore Sud può effettuare la raccolta dalla casa: a

(raccolta 'a)
;-> Raccolta del giocatore: Sud
;->     a   b   c   d   e   f
;->   +---+---+---+---+---+---+
;->   | 0 | 0 | 0 | 0 | 0 | 0 |   0
;->   +---+---+---+---+---+---+
;->   | 1 | 2 | 3 | 4 | 5 | 0 |  18
;->   +---+---+---+---+---+---+
;->     F   E   D   C   B   A
;-> Il giocatore Nord è in carestia.

(setup '(1 1 1 1 1 1 1 1 1 1 1 22))
;->     a   b   c   d   e   f
;->   +---+---+---+---+---+---+
;->   |22 | 1 | 1 | 1 | 1 | 1 |   0
;->   +---+---+---+---+---+---+
;->   | 1 | 1 | 1 | 1 | 1 | 1 |   0
;->   +---+---+---+---+---+---+
;->     F   E   D   C   B   A

(semina 'a)
;-> Semina del giocatore: Nord
;-> Casa: a, Semi: 22
;->     a   b   c   d   e   f
;->   +---+---+---+---+---+---+
;->   | 0 | 3 | 3 | 3 | 3 | 3 |   0
;->   +---+---+---+---+---+---+
;->   | 3 | 3 | 3 | 3 | 3 | 3 |   0
;->   +---+---+---+---+---+---+
;->     F   E   D   C   B   A
;-> Nessuna raccolta possibile dalla casa: b.

(setup '(1 2 3 4 5 6 2 2 2 2 0 2))
(semina 'A)
(raccolta 'a)

; Stampa la posizione corrente della tavola (più grande)
(define (print-board board g1 g2)
  (let ((border "  +--------+--------+--------+--------+--------+--------+")
        (inside "  |        |        |        |        |        |        |")
        ;(top    "      11       10        9        8        7        6")
        (top    "       a        b        c        d        e        f")
        ;(bottom "       0        1        2        3        4        5"))
        (bottom "       F        E        D        C        B        A")
        (center "  +=====================================================+"))
  (println top) (println border) (println inside)
  (println "  "
           (join (reverse (slice (map (fn(x) (format "|%5d   " x)) board) 6)))
           (format "|%4d" g2))
  (println inside) (println center) (println inside)
  (println "  "
           (join (slice (map (fn(x) (format "|%5d   " x)) board) 0 6))
           (format "|%4d" g1))
  (println inside) (println border) (println bottom) '>))

============================================================================

