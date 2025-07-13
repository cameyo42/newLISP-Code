================

 NOTE LIBERE 31

================

    "L'ora del coglione passa per tutti"
    "No escaping from dumbass moment"

----------------------------------------
Inversioni crescenti dei numeri naturali
----------------------------------------

Sequenza OEIS A061579:
Reverse one number (0), then two numbers (2,1), then three (5,4,3), then four (9,8,7,6), etc.
  0, 2, 1, 5, 4, 3, 9, 8, 7, 6, 14, 13, 12, 11, 10, 20, 19, 18, 17, 16, 15,
  27, 26, 25, 24, 23, 22, 21, 35, 34, 33, 32, 31, 30, 29, 28, 44, 43, 42,
  41, 40, 39, 38, 37, 36, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45, 65, 64,
  63, 62, 61, 60, 59, 58, 57, 56, 55, 77, 76, 75, 74, 73, 72, 71, 70, 69,
  68, 67, 66, ...

a(n) = floor(sqrt(2n+1)-1/2) * floor(sqrt(2n+1)+3/2) - n

(define (seq0 n)
  (sub (mul (floor (sub (sqrt (add (mul 2 n) 1)) 0.5))
            (floor (add (sqrt (add (mul 2 n) 1)) 1.5)))
         n))

(map seq0 (sequence 0 60))
;-> (0 2 1 5 4 3 9 8 7 6 14 13 12 11 10 20 19 18 17 16 15
;->  27 26 25 24 23 22 21 35 34 33 32 31 30 29 28 44 43 42
;->  41 40 39 38 37 36 54 53 52 51 50 49 48 47 46 45 65 64
;->  63 62 61 60)

Sequenza OEIS A038722:
Take the sequence of natural numbers (A000027) and reverse successive subsequences of lengths 1,2,3,4,...
  1, 3, 2, 6, 5, 4, 10, 9, 8, 7, 15, 14, 13, 12, 11, 21, 20, 19, 18, 17,
  16, 28, 27, 26, 25, 24, 23, 22, 36, 35, 34, 33, 32, 31, 30, 29, 45, 44,
  43, 42, 41, 40, 39, 38, 37, 55, 54, 53, 52, 51, 50, 49, 48, 47, 46, 66,
  65, 64, 63, 62, 61, 60, 59, 58, 57, 56, 78, 77, 76, ...

Formula:
  a(n) = floor(sqrt(2n-1) - 1/2)*floor(sqrt(2n-1) + 3/2) - n + 2
       = A061579(n-1) + 1

(define (seq1 n) (+ (seq0 (- n 1)) 1))

(map seq1 (sequence 1 60))
;-> (1 3 2 6 5 4 10 9 8 7 15 14 13 12 11 21 20 19 18 17
;->  16 28 27 26 25 24 23 22 36 35 34 33 32 31 30 29 45 44
;->  43 42 41 40 39 38 37 55 54 53 52 51 50 49 48 47 46 66
;->  65 64 63 62)


------------------------------------
Generatore di schemi per Cercaparole
------------------------------------

Il "Cercaparole" è un gioco che consiste nel cercare alcune parole inglobate in una matrice di caratteri.
Vedi "Cercaparole" su "Note libere 3".

Scriviamo una funzione che prende una lista di parole, le dimensioni larghezza e altezza e genera una matrice di caratteri con le parole date.

Nota: nella lista delle parole nessuna parola deve essere sottostringa di un'altra parola.

(define (genera-cercaparole parole larghezza altezza)
  (local (matrice direzioni max-len)
    (setq max-len (apply max (map length parole)))
    (if (< larghezza max-len)
      (println "Attenzione: larghezza(" larghezza ") > lunghezza massima parole(" max-len ")."))
    (if (< altezza max-len)
      (println "Attenzione: altezza(" altezza ") > lunghezza massima parole(" max-len ")."))
    ; ordina le parole 
    ; (diminuisce la probabilità di mancati inserimenti di parole)
    (sort parole (fn(x y) (>= (length x) (length y))))
    ; inizializza la matrice vuota
    (setq matrice (array-list (array altezza larghezza '(nil))))
    ; definisce le 8 direzioni possibili: orizzontale, verticale, diagonale
    (setq direzioni '((1 0) (-1 0) (0 1) (0 -1) (1 1) (-1 -1) (-1 1) (1 -1)))
    ; verifica se una parola può essere inserita
    ; partendo da (x, y) in direzione (dx, dy)
    (define (puo-inserire? parola x y dx dy)
      (let ((len (length parola)) (ok true))
        (for (i 0 (- len 1))
          (let ((cx (+ x (* i dx)))
                (cy (+ y (* i dy))))
            (if (or (< cx 0) (>= cx larghezza) (< cy 0) (>= cy altezza))
                (setq ok nil))
            (let ((cella (and (< cy altezza) (< cx larghezza) (matrice cy cx))))
              (if (and cella (!= cella (parola i)))
                  (setq ok nil)))))
        ok))
    ; inserisce una parola nella matrice, con massimo 10000 tentativi
    (define (inserisci parola)
      (let ((inserita nil) (tentativi 0) (max-tentativi 10000))
        (while (and (not inserita) (< tentativi max-tentativi))
          (++ tentativi)
          (letn ((dir (direzioni (rand (length direzioni))))
                (dx (dir 0)) (dy (dir 1))
                (x (rand larghezza)) (y (rand altezza)))
            (if (puo-inserire? parola x y dx dy)
                (begin
                  (for (i 0 (- (length parola) 1))
                    (let ((cx (+ x (* i dx)))
                          (cy (+ y (* i dy))))
                      (setf (matrice cy cx) (parola i))))
                  (setq inserita true)))))
        (if (not inserita)
            (println "Errore: non riesco a inserire la parola: " parola))))
    ; inserisce tutte le parole nella matrice
    (dolist (p parole) (inserisci p))
    ; rimpiazza le celle non usate con spazi
    (for (y 0 (- altezza 1))
      (for (x 0 (- larghezza 1))
        (if (= (matrice y x) nil)
            (setf (matrice y x) " "))))
    ; stampa la matrice soluzione (parole + spazi)
    (println "Matrice soluzione:")
    (dolist (riga matrice)
      (println (join (map string riga) " ")))
    ; riempie gli spazi vuoti con lettere A-Z casuali
    (for (y 0 (- altezza 1))
      (for (x 0 (- larghezza 1))
        (if (= (matrice y x) " ")
            (setf (matrice y x) (char (+ 65 (rand 26)))))))
    ; stampa la matrice problema (da cercare)
    (println "Matrice problema:")
    (dolist (riga matrice)
      (println (join (map string riga) " "))) matrice))

(genera-cercaparole '("CANE" "GATTO" "UVA" "LIMONE" "MELA") 8 8)
;-> E
;->   N           C
;->     O         A
;->     G M       N
;->     A E I   U E
;->     T L   L V
;->     T A     A
;->     O
;-> Matrice problema:
;-> E I I O S J P A
;-> U N S Q Z X U C
;-> I N O T L Q F A
;-> C K G M L K S N
;-> F P A E I Q U E
;-> U H T L G L V J
;-> K U T A L Q A Y
;-> K Z O C W B W Q

(setq parole '("ASSEMBLY" "FORTRAN" "LISP" "ALGOL" "BASIC" "COBOL"
               "ADA" "DELPHI" "SMALLTALK" "JAVA" "PHP" "LOGO" "PASCAL"
               "PERL" "PYTHON"))

(setq mtx (genera-cercaparole parole 12 12))
;-> Matrice soluzione:
;->   D F O R T R A N
;-> A E       A
;->   L       S       A O
;->   P G P A S C A L D G
;-> N H   O   E       A O
;-> O I     L M P       L
;-> H P       B E P S I L C
;-> T   H     L R         I
;-> Y     P   Y L         S
;-> P K L A T L L A M S   A
;->         C O B O L     B
;->         A V A J
;-> Matrice problema:
;-> Z D F O R T R A N V C Q
;-> A E G F T A S N T Z Q E
;-> N L I C Q S D Q Y A O B
;-> Y P G P A S C A L D G G
;-> N H K O T E M F G A O H
;-> O I W R L M P Q S R L W
;-> H P P Y G B E P S I L C
;-> T Z H O E L R S N V N I
;-> Y P C P A Y L Q T N O S
;-> P K L A T L L A M S C A
;-> A H N A C O B O L J O B
;-> K L J Q A V A J O A M W

(cercaparole mtx parole)
;-> ((("ASSEMBLY" 2 6 "S") ("FORTRAN" 1 3 "E") ("LISP" 7 11 "O")
;->   ("ALGOL" 2 1 "SE") ("BASIC" 11 12 "N") ("COBOL" 11 5 "E")
;->   ("ADA" 2 6 "SE") ("DELPHI" 1 2 "S") ("SMALLTALK" 10 10 "O")
;->   ("JAVA" 12 8 "O") ("PHP" 7 2 "SE") ("LOGO" 6 11 "N")
;->   ("PASCAL" 4 4 "E") ("PERL" 6 7 "S") ("PYTHON" 10 1 "N")) nil)

La funzione "cercaparole" si trova in "Cercaparole" su "Note libere 3".

Caso delle dimensioni inferiori alla lunghezza delle parole:

(genera-cercaparole parole 8 8)
;-> Attenzione: larghezza(8) > lunghezza massima parole(9).
;-> Attenzione: altezza(8) > lunghezza massima parole(9).
;-> Errore: non riesco a inserire la parola: SMALLTALK
;-> Errore: non riesco a inserire la parola: PASCAL
;-> Errore: non riesco a inserire la parola: PERL
;-> Errore: non riesco a inserire la parola: PYTHON
;-> Matrice soluzione:
;-> L   A   N     O
;->   I S J A V A G
;->   L S   R I   O
;-> L O E P T H B L
;-> O B M P R P A A
;-> G O B H O L S D
;-> L C L P F E I A
;-> A   Y     D C
;-> Matrice problema:
;-> L O A N N I D O
;-> R I S J A V A G
;-> L L S I R I P O
;-> L O E P T H B L
;-> O B M P R P A A
;-> G O B H O L S D
;-> L C L P F E I A
;-> A B Y D E D C S

Con la funzione "matrice2html" (vedi "Matrice --> HTML" su "Note libere 31" possiamo salvare lo schema generato come pagina html:

; crea la pagina html (stringa)
(setq html (matrice2html "Cerca Parole" mtx parole))
; salva su file
(write-file "matrice.html" html)
; visualizza il file html sul browser predefinito
(exec "matrice.html")


----------------
Matrice --> HTML
----------------

Conversione di una matrice in una pagina HTML.

La funzione prende tre parametri:
  1) Titolo (posizionato in al centro)
  2) Matrice (al centro, sotto al Titolo)
  3) Lista parole (al centro sotto alla matrice)
e genera una variabile stringa che contiene la matrice come pagina HTML.

(define (matrice2html titolo matrice parole)
  (let ((html '()))
    ; inizio documento HTML
    (push "<!DOCTYPE html>" html -1)
    (push "<html><head><meta charset='UTF-8'>" html -1)
    (push (string "<title>" titolo "</title>") html -1)
    (push "</head><body>" html -1)
    ; titolo centrato
    (push (string "<h1 style='text-align:center;'>" titolo "</h1>") html -1)
    ; inizio tabella centrata con bordo
    (push "<table style='margin:auto; border-collapse:collapse;'>" html -1)
    ; per ogni riga della matrice
    (dolist (riga matrice)
      (push "<tr>" html -1)
      (dolist (cella riga)
        (push (string "<td style='border:1px solid black; padding:5px; text-align:center;'>"
                      cella
                      "</td>") html -1))
      (push "</tr>" html -1))
    ; fine tabella
    (push "</table>" html -1)
    (when parole
      ; inserisce la lista parole dopo la tabella
      (push "<div style='text-align:center; margin-top:20px;'>" html -1)
      ;(push (join (map string parole) ", ") html -1)
      ; grassetto
      (push (string "<b>" (join (map string parole) ", ") "</b>") html -1)
      (push "</div>" html -1))
    ; chiusura documento
    (push "</body></html>" html -1)
    ; restituisce l'HTML come stringa
    (join html "\n")))

Proviamo:

(setq matrix '(
  ("A" "B" "C")
  ("D" "E" "F")
  ("G" "H" "I")))

; crea la pagina html (stringa)
(setq html (matrice2html "Matrice di Test" matrix '("uno" "due" "tre"))
; salva su file
(write-file "matrice.html" html)
; visualizza il file html sul browser predefinito
(exec "matrice.html")


-------------
Brazilian CPF
-------------

https://codegolf.stackexchange.com/questions/269151/validate-a-cpf-number

Nota:
Tutto il contenuto dei siti di Stack Exchange è rilasciato sotto la licenza CC BY-SA 4.0 (Creative Commons Attribution-ShareAlike 4.0).

Ogni cittadino brasiliano ha un numero di identificazione nazionale associato, denominato CPF.
Questo numero ha 11 cifre (formattate come XXX.XXX.XXX-XX) ed è convalidato dal seguente algoritmo (reale):
  1) Prendi le 9 cifre più significative del numero. Chiamalo c.
  2) Moltiplica l'n-esima cifra meno significativa di c per n+1, dove 1<=n<=9.
  3) Somma tutti i prodotti e calcola il resto della divisione tra questo e 11.
  4) Se il resto r è minore di 2, considera 0. 
     Altrimenti, considera 11 - r. 
     Aggiorna 'c' per includerlo come cifra meno significativa aggiunta.
  5) Prima volta al passaggio 4?
     Torna al passaggio 1, ma ora 1<=n<=10.
     Altrimenti, vai al passaggio 5.
  6) 'c' è uguale al tuo numero iniziale?
     In caso affermativo, il tuo CPF è valido.

Esempio:
Prendiamo 111.444.777-35.

Prendiamo le 9 cifre più significative del numero. Chiamiamolo 'c':
c = 111444777

Moltiplica l'n-esima cifra meno significativa di 'c' per n+1, dove 1 ≤ n ≤ 9:
     c   1 1 1  4  4  4  7  7  7
 (n+1)  10 9 8  7  6  5  4  3  2
result  10 9 8 28 24 20 28 21 14

Somma tutti i prodotti e calcola il resto della divisione tra questo e 11:
10+9+8+28+24+20+28+21+14 = 162 mod 11 = 8

Se il resto r è minore di 2, considera 0.
Altrimenti, considera 11 - r.
Aggiorna 'c' aggiungendo la cifra meno significativa:
Considera 11 - 8 = 3, c = 1114447773.

Torna al punto 1, ma ora 1 ≤ n ≤ 10.
     c   1  1 1  4  4  4  7  7  7 3
 (n+1)  11 10 9  8  7  6  5  4  3 2
result  11 10 9 32 28 24 35 28 21 6

11+10+9+32+28+24+35+28+21+6 = 204 mod 11 = 6

Quindi 11 - 6 = 5, c = 11144477735.

'c' è uguale al numero iniziale? Sì. Pertanto, il CPF è valido.

(define (cpf? code)
  (local (str cifre somma-prodotti val)
    ; creazione numero/stringa 'c' a 9 cifre
    (setq str (select code '(0 1 2 4 5 6 8 9 10)))
    ;(setq num (int str 0 10))
    ; Primo giro
    ; creazione lista delle cifre
    (setq cifre (map (fn(x) (int x 0 10)) (explode str)))
    ; somma dei prodotti cifra*(n+1)
    (setq somma-prodotti (apply + (map * cifre '(10 9 8 7 6 5 4 3 2))))
    ; calcolo del modulo
    ; e aggiunta del valore al numero/stringa --> nuovo numero/stringa
    (if (< (% somma-prodotti 11) 2)
        (push "0" str -1)
        (push (string (- 11 (% somma-prodotti 11))) str -1))
    ; Secondo giro
    ; creazione lista delle cifre del nuovo numero
    (setq cifre (map (fn(x) (int x 0 10)) (explode str)))
    ; somma dei prodotti cifra*(n+1)
    (setq somma-prodotti (apply + (map * cifre '(11 10 9 8 7 6 5 4 3 2))))
    ; calcolo del modulo
    ; e aggiunta del valore al numero/stringa --> nuovo numero/stringa
    (if (< (% somma-prodotti 11) 2)
        (push "0" str -1)
        (push (string (- 11 (% somma-prodotti 11))) str -1))
    ; aggiunge "." "." e "-" al numero-stringa
    (push "." str 3)
    (push "." str 7)
    (push "-" str 11)
    (= code str)))

(cpf? "111.444.777-35") ;-> true
(cpf? "523.986.800-02") ;-> true
(cpf? "058.300.335-42") ;-> true
(cpf? "068.650.863-76") ;-> true
(cpf? "393.852.580-01") ;-> true
(cpf? "392.906.007-80") ;-> true
(cpf? "018.718.637-59") ;-> true
(cpf? "033.373.634-66") ;-> true
(cpf? "663.778.052-92") ;-> true
(cpf? "714.628.314-35") ;-> true
(cpf? "546.692.758-95") ;-> true
(cpf? "240.299.015-57") ;-> true
(cpf? "831.901.302-05") ;-> true
(cpf? "977.278.105-09") ;-> true
(cpf? "701.595.919-45") ;-> true
(cpf? "687.304.519-57") ;-> true
(cpf? "558.298.493-30") ;-> true
(cpf? "301.623.419-03") ;-> true
(cpf? "566.847.323-65") ;-> true
(cpf? "605.053.766-60") ;-> true
(cpf? "295.350.637-31") ;-> true

(cpf? "001.338.908-38") ;-> nil
(cpf? "637.940.265-42") ;-> nil
(cpf? "351.161.559-40") ;-> nil
(cpf? "781.618.495-93") ;-> nil
(cpf? "103.413.164-75") ;-> nil
(cpf? "255.341.928-32") ;-> nil
(cpf? "764.835.030-56") ;-> nil
(cpf? "413.953.767-24") ;-> nil
(cpf? "238.849.696-53") ;-> nil
(cpf? "287.101.226-91") ;-> nil
(cpf? "669.784.801-84") ;-> nil
(cpf? "514.627.048-28") ;-> nil
(cpf? "148.932.528-80") ;-> nil
(cpf? "957.015.430-39") ;-> nil
(cpf? "117.182.278-24") ;-> nil
(cpf? "896.383.465-78") ;-> nil
(cpf? "713.315.098-39") ;-> nil
(cpf? "301.031.051-83") ;-> nil
(cpf? "473.829.973-76") ;-> nil

Possiamo semplificare la funzione nel modo seguente:
1) Usiamo filter per ottenere solo cifre, evitando l'uso manuale di select.
2) Usiamo una funzione 'digit-check' che calcola le cifre finali in modo parametrico.
3) Funziona anche se il CPF è dato in formato stringa senza punti/lineette.

(define (cpf? code)
  (local (digits c sum r)
    ;----------------------
    (define (digit-check n)
      (setq c (slice digits 0 n))
      (setq sum (apply + (map * c (sequence (+ 1 n) 2 -1))))
      (setq r (% sum 11))
      (if (< r 2) 0 (- 11 r)))
    ;----------------------
    ; crea una lista di cifre rimuovendo 
    ; tutti i caratteri non numerici
    (setq digits (map int (filter (fn(x) (<= "0" x "9")) (explode code))))
    (and (= (length digits) 11) ; numero-cifre = 11 ?
         ; prima cifra calcolata = prima cifra ?
         (= (digits 9) (digit-check 9)) 
         ; seconda cifra calcolata = seconda cifra ?
         (= (digits 10) (digit-check 10)))))

Versione code-golf (214 caratteri):

(define(z e)(define(k n)(setq r(%(apply +(map *(slice d 0 n)(sequence(+ 1 n)2 -1)))11))(if(< r 2)0(- 11 r)))
(setq d(map int(filter(fn(x)(<= "0" x "9"))(explode e))))
(and(=(length d)11)(=(d 9)(k 9))(=(d 10)(k 10))))


-------------------------
Messaggio binario segreto
-------------------------

Scriviamo due funzioni "encode" e "decode" per spedire un messaggio segreto.
Le funzioni prendono due parametri:
a) la stringa da codificare/decodificare
b) la stringa dei comandi ("lrfu")

La stringa dei comandi ha il seguente significato:
  "l" --> ruota la stringa di 1 bit a sinistra
  "r" --> ruota la stringa di 1 bit a destra
  "f" --> inverte ogni bit nella stringa di bit
  "u" --> inverte la stringa di bit
I comandi possono essere in qualsiasi ordine, qualche comando può mancare e alcuni possono essere multipli.

La codifica viene effettuata con il seguente algoritmo:
1) Convertire ogni carattere della stringa nel relativo numero ASCII a 7bit.
2) Convertire ogni numero in binario (con 7 caratteri)
3) Unire tutti i numeri binari in un unica stringa
4) Per ogni comando della stringa comandi:
   Se "l" --> ruota la stringa di 1 bit a sinistra
   Se "r" --> ruota la stringa di 1 bit a destra
   Se "f" --> inverte ogni bit nella stringa di bit
   Se "u" --> inverte la stringa di bit

(define (flip-bits bin-str)
  (let (out "")
    (dostring (b bin-str)
      (if (= b 48) ; "0"
          (push "1" out -1)
          (push "0" out -1)))))

(define (encode str cmd)
  (let (binary (join (map (fn(x) (format "%07s" (bits (char x)))) (explode str))))
    (dostring (ch cmd)
      (cond ((= ch 108) (rotate binary -1)) ;l (rotate left)
            ((= ch 114) (rotate binary +1)) ;r (rotate right)
            ((= ch 102) (setq binary (flip-bits binary))) ;f (flip)
            ((= ch 117) (reverse binary)))) ;u (reverse)
    binary))

(setq crypt (encode "newLISP" "lufr"))
;-> "0011110100011010011011011001100001000010110010001"

La decodifica viene effettuata con il seguente algoritmo:
1) Per ogni comando della stringa comandi:
   Se "l" --> ruota la stringa di 1 bit a sinistra
   Se "r" --> ruota la stringa di 1 bit a destra
   Se "f" --> inverte ogni bit nella stringa di bit
   Se "u" --> inverte la stringa di bit
2) Unire tutti i numeri binari in un unica stringa
3) Dividere la stringa in numeri binari da 7 bit.
4) Convertire i numeri binari in caratteri.

Per la decodifica dobbiamo conoscere la stringa dei comandi con cui abbiamo codificato la stringa.
Per costruire la stringa dei comandi della decodifica partendo dalla stringa dei comandi della codifica bisogna effetture i seguenti passaggi:
1) convertire ogni "l" in "r" e ogni "r" in "l"
2) invertire la stringa

Esempio:
codifica comandi = "lufr"
decodifica comandi (scambia l/r) --> "rufl" (inverte stringa) --> "lfur"

(define (decode-cmd cmd)
  (let (out "")
    (for (i 0 (- (length cmd) 1))
      (cond ((= (cmd i) "l") (push "r" out -1))
            ((= (cmd i) "r") (push "l" out -1))
            ((= (cmd i) "u") (push "u" out -1))
            ((= (cmd i) "f") (push "f" out -1))))
    (reverse out)))

(decode-cmd "lufr")
;-> "lfur"

(define (decode str dmc)
  (dostring (ch dmc)
    (cond ((= ch 108) (rotate str -1)) ;l (rotate left)
          ((= ch 114) (rotate str +1)) ;r (rotate right)
          ((= ch 102) (setq str (flip-bits str))) ;f (flip)
          ((= ch 117) (reverse str)))) ;u (reverse)
    (join (map (fn(x) (char (int x 0 2))) (explode str 7))))

(decode crypt (decode-cmd "lufr"))
;-> "newLISP"

(decode (encode "sanpierdarena" "llufr") (decode-cmd "llufr"))
;-> "sanpierdarena"

Vedi anche "Codifica e decodifica di una stringa" su "Note libere 24".


-------------------------
Esplosione di una stringa
-------------------------

Data una stringa composta da caratteri ASCII (33..126) (no spazi).
1) trasformare la stringa in palindroma invertendola e aggiungendola prima di se stessa (escludendo il carattere centrale)
Esempio: la stringa "012345", diventerà "54321012345".
2) stampare il seguente testo:

  5    4    3    2    1    0    1    2    3    4    5
       5   4   3   2   1   0   1   2   3   4   5
            5  4  3  2  1  0  1  2  3  4  5
                 5 4 3 2 1 0 1 2 3 4 5
                      54321012345
                 5 4 3 2 1 0 1 2 3 4 5
            5  4  3  2  1  0  1  2  3  4  5
       5   4   3   2   1   0   1   2   3   4   5
  5    4    3    2    1    0    1    2    3    4    5

Dal centro verso l'esterno in entrambe le direzioni, ogni carattere è separato da uno spazio in più rispetto alla riga precedente.

Procedimento:

; stringa di esempio
(setq str "012345")
; rende palindroma la stringa
(setq str (append (reverse (slice str 1)) str))
;-> "54321012345"
; calcola quante righe devono essere stampate
(setq rows (- (length str) 2))
;-> 9
; metà intera delle righe
(setq meta (/ rows 2))
;-> 4
; calcola, per ogni riga, il valore degli spazi interni
; tra due caratteri per ogni riga (lista)
(setq inside (sequence 0 meta))
;-> (0 1 2 3 4)
(setq inside (append (reverse (slice inside 1)) inside))
;-> (4 3 2 1 0 1 2 3 4)
; calcola, per ogni riga, il valore degli spazi iniziali 
; a sinistra della riga
(setq left (series 0 (fn(x) (+ x (div (length str) 2))) (+ meta 1)))
;-> (0 5 10 15 20)
(setq left (append left (slice (reverse left) 1)))
;-> (0 5 10 15 20 15 10 5 0)
; ciclo di stampa (per ogni riga)
(for (r 0 (- rows 1))
  ; stampa gli spazi iniziali a sinistra
  (print (dup " " (left r)))
  ; spazi tra i caratteri
  (setq spaces (inside r))
  ; ciclo di stampa di tutti i caratteri della riga corrente
  (dolist (ch (explode str))
    ; stampa carattere e spazi
    (print ch (dup " " spaces))
  )
  (println)
)
;-> 5    4    3    2    1    0    1    2    3    4    5
;->      5   4   3   2   1   0   1   2   3   4   5
;->           5  4  3  2  1  0  1  2  3  4  5
;->                5 4 3 2 1 0 1 2 3 4 5
;->                     54321012345
;->                5 4 3 2 1 0 1 2 3 4 5
;->           5  4  3  2  1  0  1  2  3  4  5
;->      5   4   3   2   1   0   1   2   3   4   5
;-> 5    4    3    2    1    0    1    2    3    4    5

(define (boom str)
  (local (rows meta inside left)
    ; rende palindroma la stringa
    (setq str (append (reverse (slice str 1)) str))
    ; calcola quante righe devono essere stampate
    (setq rows (- (length str) 2))
    ; metà intera delle righe
    (setq meta (/ rows 2))
    ; calcola, per ogni riga, il valore degli spazi interni
    ; tra due caratteri per ogni riga (lista)
    (setq inside (sequence 0 meta))
    (setq inside (append (reverse (slice inside 1)) inside))
    ; calcola, per ogni riga, il valore degli spazi iniziali 
    ; a sinistra della riga
    (setq left (series 0 (fn(x) (+ x (div (length str) 2))) (+ meta 1)))
    (setq left (append left (slice (reverse left) 1)))
    ; ciclo di stampa (per ogni riga)
    (for (r 0 (- rows 1))
      ; stampa gli spazi iniziali a sinistra
      (print (dup " " (left r)))
      ; spazi tra i caratteri
      (setq spaces (inside r))
      ; ciclo di stampa di tutti i caratteri della riga corrente
      (dolist (ch (explode str))
        ; stampa carattere e spazi
        (print ch (dup " " spaces))
      )
      (println)) '>))

(boom ".......")
;-> .     .     .     .     .     .     .     .     .     .     .     .     .
;->       .    .    .    .    .    .    .    .    .    .    .    .    .
;->             .   .   .   .   .   .   .   .   .   .   .   .   .
;->                   .  .  .  .  .  .  .  .  .  .  .  .  .
;->                         . . . . . . . . . . . . .
;->                               .............
;->                         . . . . . . . . . . . . .
;->                   .  .  .  .  .  .  .  .  .  .  .  .  .
;->             .   .   .   .   .   .   .   .   .   .   .   .   .
;->       .    .    .    .    .    .    .    .    .    .    .    .    .
;-> .     .     .     .     .     .     .     .     .     .     .     .     .

(boom "1234567890")

Vedi immagine "boom-string.png" nella cartella "data".


--------------------------------------------
Numeri casuali, pi greco, logaritmi naturali
--------------------------------------------

Generiamo due numeri casuali tra 0 e 1 con distribuzione uniforme: n1 e n2.
Dividiamo i numeri random: res = n1/n2
Qual'è la probabilità che round(res) sia un numero pari?
Qual'è la probabilità che floor(res) sia un numero pari?

(define (test-round prove)
  (setq pari 0)
  (for (i 1 prove)
    (setq n1 (random))
    (setq n2 (random))
    (setq res (round (div n1 n2) 0))
    (if (even? res) (++ pari)))
  (div pari prove))

(time (test-round 1e7))
;-> 0.4646439

Matematicamente la probabilità cercata vale:

Prob(x/y even) = (5 - pi)/4

(setq pi 3.1415926535897931)
(div (sub 5 pi) 4)
;-> 0.4646018366025517

(define (test-floor prove)
  (setq pari 0)
  (for (i 1 prove)
    (setq n1 (random))
    (setq n2 (random))
    (setq res (floor (div n1 n2)))
    (if (even? res) (++ pari)))
  (div pari prove))

(test-floor 1e7)
;-> 0.6535496

Matematicamente la probabilità cercata vale:

Prob(x/y even) = (1/2)*(2 - ln(2))

(mul 0.5 (sub 2 (log 2)))
;-> 0.6534264097200273


---------------------------------
Sequenze, dismutazioni, rotazioni
---------------------------------

Abbiamo una sequenza 'seq' di N numeri da 1 a N.
Abbiamo una lista 'lst' di N numeri da 1 a N.
Risulta che:

  lst(i) != seq(i), per i=0..(N-1)

Quindi la lista è una dismutazione (derangement) della sequenza.
In altre parole è una permutazione della sequenza tale che nessun elemento appare nella sua posizione originale.

Vedi "Dismutazioni (Derangements)" su "Funzioni varie".

Se facciamo ruotare la lista di una posizione alla volta, qual è il valore minimo e massimo di numeri nella stessa posizione tra la lista e la sequenza?
Esempio:
Lista = (1 2 3 4)
Dismutazioni:  (2 4 1 3) (4 1 2 3) (2 1 4 3) (3 1 4 2) (3 4 1 2)
               (4 3 1 2) (4 3 2 1) (3 4 2 1) (2 3 4 1)
Output:
Dismutazione   Rotazione    Elementi uguali allo stesso indice di (1 2 3 4)
(2 4 1 3)      (1 3 2 4)    2
(4 1 2 3)      (1 2 3 4)    4
(2 1 4 3)      (3 2 1 4)    2
(3 1 4 2)      (4 2 3 1)    2
(3 4 1 2)      (1 2 3 4)    4
(4 3 1 2)      (1 2 4 3)    2
(4 3 2 1)      (1 4 3 2)    2
(3 4 2 1)      (2 1 3 4)    2
(2 3 4 1)      (1 2 3 4)    4
Valore minimo di elementi uguali = 2
Valore massimo di elementi uguali = 4

(define (dism lst)
"Generates all dismutations without repeating from a list of items"
(define (dis?)
(catch
  (let (ok true)
    (dolist (el lst)
      (if (= el (base $idx))
        (throw nil)))
    ok)))
  (local (i indici out base)
    ; lista originale
    (setq base lst)
    (setq indici (dup 0 (length lst)))
    (setq i 0)
    ; non aggiungiamo la lista iniziale alla soluzione
    ; perchè non è una dismutazione
    ; (setq out (list lst))
    (setq out '())
    (while (< i (length lst))
      (if (< (indici i) i)
          (begin
            (if (zero? (% i 2))
              (swap (lst 0) (lst i))
              (swap (lst (indici i)) (lst i))
            )
            ;(println lst)
            ; inseriamo la permutazione corrente solo se ogni elemento
            ; non appare nella sua posizione originale
            (if (dis?)
              (push lst out -1)
            )
            (++ (indici i))
            (setq i 0)
          )
          (begin
            (setf (indici i) 0)
            (++ i)
          )
       )
    )
    out))

(dism '(1 2 3 4))
;-> ((2 4 1 3) (4 1 2 3) (2 1 4 3) (3 1 4 2) (3 4 1 2)
;->  (4 3 1 2) (4 3 2 1) (3 4 2 1) (2 3 4 1))

Funzione che calcola il numero di elementi che hanno lo stesso valore nelle stesse posizioni di due liste:

(define (uguali lst1 lst2)
  (first (count '(0) (map - lst1 lst2))))

(uguali '(1 2 3 4) '(0 2 3 4))
;-> 3

(define (min-max items show)
  (local (max-equal lst dismute current tmp num-equal)
    (setq out '())
    (setq max-equal 0)
    (setq lst (sequence 1 items))
    (setq dismute (dism lst))
    (when show
      (println "Sequenza: " lst)
      (println "Dismutazioni: " (length dismute)))
    (dolist (el dismute)
      (setq max-equal 0)
      (setq current (list el 0))
      (setq tmp el)
      (for (r 1 (length tmp))
        (setq num-equal (uguali (rotate tmp) lst))
        (when (> num-equal max-equal)
          (setq max-equal num-equal)
          (setq current (list el tmp max-equal)))
      )
      (push current out -1)
      ;(if (zero? (% $idx 10000)) (println $idx))
    )
    (setq nums (map (fn(x) (x 2)) out))
    (list (apply min nums) (apply max nums))))

Proviamo:

(min-max 6 true)
;-> Sequenza: (1 2 3 4 5 6)
;-> Dismutazioni: 265
;-> (2 6)

(map min-max (sequence 2 9))
;-> ((2 2) (3 3) (2 4) (2 5) (2 6) (2 7) (2 8) (2 9))

(map output (sequence 2 9))
;-> ((2 2) (3 3) (2 4) (2 5) (2 6) (2 7) (2 8) (2 9))

Il risultato è intuitivo:
1) con N diverso 3 il minimo è sempre 2, perchè qualunque dismutazione riporta almeno due elementi nella posizione originale.
2) il massimo vale sempre N, perchè esiste sempre una dismutazione che è uguale alla rotazione di un posto della sequenza.


-------------------------------
x^2 + y^2 + z^2 + w^2 = x*y*z*w
-------------------------------

A simple equation that behaves weirdly - Numberphile
https://www.youtube.com/watch?v=a7BVL1MOCl4

Risolvere la seguente equazione:

  x^2 + y^2 + z^2 + w^2 = x*y*z*w

per gli interi positivi x,y,z,w interi fino ad un dato N.

La prima soluzione vale: x = y = z = w = 2

Elenco di tutte le soluzioni (23) con valori fino a 1 milione:

  2,2,2,2
  2,2,2,6
  2,2,6,22
  2,2,22,82
  2,2,82,306
  2,2,306,1142
  2,2,1142,4262
  2,2,4262,15906
  2,2,15906,59362
  2,2,59362,221542
  2,2,221542,826806
  2,6,22,262
  2,6,262,3122
  2,6,3122,37202
  2,6,37202,443302
  2,22,82,3606
  2,22,262,11522
  2,22,3606,158582
  2,22,11522,506706
  2,82,306,50182
  2,82,3606,591362
  2,306,1142,698902
  6,22,262,34582

(setq sol-milione '((2 2 2 2) (2 2 2 6) (2 2 6 22) (2 2 22 82)
                   (2 2 82 306) (2 2 306 1142) (2 2 1142 4262)
                   (2 2 4262 15906) (2 2 15906 59362) (2 2 59362 221542)
                   (2 2 221542 826806) (2 6 22 262) (2 6 262 3122)
                   (2 6 3122 37202) (2 6 37202 443302) (2 22 82 3606)
                   (2 22 262 11522) (2 22 3606 158582) (2 22 11522 506706)
                   (2 82 306 50182) (2 82 3606 591362) (2 306 1142 698902)
                   (6 22 262 34582)))

(length sol-milione)
;-> 23

Funzione dell'equazione:

(define (f x y z w) (= (+ (* x x) (* y y) (* z z) (* w w)) (* x y z w)))

Metodo brute-force:

(define (genera1 limite)
  ;(for (x 2 limite 2)
    (setq x 2) ; trucco
    (for (y x 22 2) ; trucco
      (for (z y limite 2)
        (for (w z limite 2)
          (if (f x y z w) (println x { } y { } z { } w))))))

(time (genera1 10000))
;-> 2 2 2 2
;-> 2 2 2 6
;-> 2 2 6 22
;-> 2 2 22 82
;-> 2 2 82 306
;-> 2 2 306 1142
;-> 2 2 1142 4262
;-> 2 6 22 262
;-> 2 6 262 3122
;-> 2 22 82 3606
;-> 37447.687

Non è possibile risolvere il problema usando la forza bruta.

Dal punto di vista matematico è stato dimostrato che è possibile generare nuove soluzioni partendo da una soluzione nota.
Esempio:
Partiamo dalla soluzione con x = y = z = w = 2:
(2 2 2 2)
Per generare una nuova soluzione:
1) scegliamo uno dei 4 valori e lo chiamiamo 'x' (per esempio il primo 2):
2) calcoliamo il nuovo valore di 'x': new-x = y*z*w - x
   new-x = 2*2*2 - 2 = 6
3) Sostituiamo 'x' con 'new-x' nella soluzione di partenza
   (6 2 2 2)

Verifichiamo che la soluzione sia corretta:
(f 6 2 2 2)
;-> true
(apply f '(6 2 2 2))
;-> true

Per ogni soluzione di partenza possiamo generare 4 soluzioni diverse.

Algoritmo
fatte --> lista delle soluzioni terminate (espanse)
da-fare --> lista delle soluzioni da espandere
1) Partiamo dalla soluzione base (2 2 2 2).
2) Generiamo 4 soluzioni diverse e le inseriamo in fondo alla lista 'da-fare'
   (se le soluzioni contengono numeri inferiori ad un limite dato
    e se non sono già presenti nella lista 'da-fare' e nella lista 'fatte').
3) Inseriamo la soluzione base nella lista 'fatte'
   (se non era già presente nella lista 'fatte').
4) Se la lista 'da-fare' è vuota --> Stop
   Altrimenti:
      Estraiamo la prima soluzione dalla lista 'da-fare'.
      Questa lista diventa la soluzione base.
      Ripetere dal passo 2).

Ad ogni passo generiamo 1 soluzione terminata.

Funzione che trova tutte le soluzioni intere dell'equazione fino ad un dato limite:

(define (genera2 limite)
  (define (f x y z w) (= (+ (* x x) (* y y) (* z z) (* w w)) (* x y z w)))
  (local (fatte da-fare lst prod tmp x new-x)
    ; lista delle soluzioni calcolate
    (setq fatte '())
    ; lista delle soluzioni da espandere
    ; inizializzata con la soluzione base (biginteger)
    (setq da-fare '((2L 2L 2L 2L)))
    (setq stop nil)
    (until stop
      (if da-fare ; se la lista 'da-fare' non è vuota
          (begin
            ; prende la prima lista dalle soluzioni da espandere
            (setq lst (pop da-fare))
            (setq prod (apply * lst))
            ;(print lst) (read-line)
            ; modifica ogni elemento/numero della lista corrente
            ; per creare una nuova soluzione
            (for (k 0 3)
              (setq tmp lst)
              (setq x (lst k))
              (setq new-x (- (/ prod x) x))
              (setf (tmp k) new-x)
              ; ordina la lista corrente per evitare duplicati
              ; es. (2 2 2 6) deve essere uguale a (2 6 2 2))
              (sort tmp)
              ;(println "tmp: " tmp)
              ; inserisce la lista tmp (nuova soluzione)
              ; nella lista delle soluzioni da espandere:
              ; se new-x è inferiore a 'limite' e
              ; se non esiste già nelle soluzioni da espandere e
              ; se non esiste già nelle soluzioni calcolate
              (if (and (<= new-x limite)
                      (not (ref tmp da-fare))
                      (not (ref tmp fatte)))
                  (push tmp da-fare -1))
            )
            ; ordina la lista corrente per evitare duplicati
            ; es. (2 2 2 6) deve essere uguale a (2 6 2 2))
            (sort lst)
            ; inserisce la lista corrente nelle soluzioni calcolate
            ; (se non esiste nelle soluzioni calcolate)
            (if (not (ref lst fatte)) (push lst fatte -1)))
          ;else (la lista da-fare è vuota --> stop)
            (setq stop true)))
    ;(println (length fatte))
    ;(println (length da-fare))
    (sort fatte)))

Proviamo:

(genera2 10000)
;-> ((2L 2L 2L 2L) (2L 2L 2L 6L) (2L 2L 6L 22L) (2L 2L 22L 82L)
;->  (2L 2L 82L 306L) (2L 2L 306L 1142L) (2L 2L 1142L 4262L)
;->  (2L 6L 22L 262L) (2L 6L 262L 3122L) (2L 22L 82L 3606L))

(genera2 1e6)
;-> ((2L 2L 2L 2L) (2L 2L 2L 6L) (2L 2L 6L 22L) (2L 2L 22L 82L)
;->  (2L 2L 82L 306L) (2L 2L 306L 1142L) (2L 2L 1142L 4262L)
;->  (2L 2L 4262L 15906L) (2L 2L 15906L 59362L) (2L 2L 59362L 221542L)
;->  (2L 2L 221542L 826806L) (2L 6L 22L 262L) (2L 6L 262L 3122L)
;->  (2L 6L 3122L 37202L) (2L 6L 37202L 443302L) (2L 22L 82L 3606L)
;->  (2L 22L 262L 11522L) (2L 22L 3606L 158582L) (2L 22L 11522L 506706L)
;->  (2L 82L 306L 50182L) (2L 82L 3606L 591362L) (2L 306L 1142L 698902L)
;->  (6L 22L 262L 34582L))
 
(time (genera2 1e6))
;-> 0 ms

Verifichiamo la correttezza della soluzione:

(= (sort (genera2 1e6)) (sort sol-milione))
;-> true

La funzione è molto veloce:

(time (println (length (genera2 1e20))))
;-> 325
;-> 15.567
(time (println (length (sort (genera2 1e50)))))
;-> 2903
;-> 698.961 ms


------------------------------------------------------------------
Ordinamento di lettere senza distinzioni tra maiuscole e minuscole
------------------------------------------------------------------

Data una stringa con soli caratteri alfabetici (a..z,A..Z), ordinare i caratteri senza considerare le differenze tra maiuscole e minuscole (ordinamento: A,a,B,b,C,c,...Z,z,).
Per esempio:
stringa = "BbAaBcC"
output = "AaBBbCc"

La primitiva 'sort' ordina i caratteri in modo ASCII:

(join (sort (explode "AaBBbCc")))
;-> "ABBCabc

Però possiamo usare lo stesso 'sort' con un comparatore specifico:

(define (ordina-stringa str)
  (join
    (sort (explode str)
          (fn (a b)
            (letn ((la (lower-case a))
                   (lb (lower-case b)))
              (if (= la lb)
                  (<= (char a) (char b)) ; maiuscole prima
                  (<= la lb))))))) ; ordinamento standard

Spiegazione del comparatore:
1) lower-case rende il confronto insensibile al case.
2) Se le lettere sono uguali (ignora case), usa (char a) per dare precedenza alla maiuscola (perché ha valore ASCII minore).

Proviamo:

(ordina-stringa "BbAaBcC")
;-> "AaBBbCc"

(ordina-stringa "SuperCaliFragilistiCheSpiralidoso")
;-> "aaaCCdeeFghiiiiiillloopprrrSSsstu"


------------------------------
Numero medio di corrispondenze
------------------------------

Abbiamo due liste composte ognuna da N numeri interi.
I numeri delle liste sono scelti indipendentemente e uniformemente a caso tra M possibili valori interi.
Qual è il numero medio di elementi (media attesa) che hanno lo stesso valore e la stessa posizione nelle due liste?

Risulta che:
  Se M = N, allora la media attesa vale 1.
  Se M > N, allora la media attesa è minore di 1.
  Se M < N, allora la media attesa è maggiore di 1.

Funzione che verifica quanti elementi tra due liste hanno lo stesso valore nella stessa posizione:

(define (same lst1 lst2)
  (first (count '(true) (map = lst1 lst2))))

(same '(1 2 3 4 5) '(1 2 3 4 6))
;-> 4
(same '(0 1 2 3 4 5) '(1 2 3 4 5 0))
;-> 0

Funzione che simula il processo:

(define (simula N M itera)
  (let ( (totale 0) (l1 '()) (l2 '()) )
    (for (i 1 itera)
      (setq l1 (rand M N))
      (setq l2 (rand M N))
      (++ totale (same l1 l2)))
    (div totale itera)))

Proviamo:

(simula 100 100 1e5)
;-> 1.00155
(simula 100 1000 1e5)
;-> 0.10003
(simula 100 10 1e5)
;-> 10.00065
(simula 1000 10 1e5)
;-> 100.00466

La media attesa del numero di elementi nella stessa posizione con lo stesso valore tra due liste di lunghezza N, i cui elementi sono scelti indipendentemente e uniformemente a caso tra M possibili valori interi, vale:

  Media = N/M

La probabilità che due numeri casuali indipendenti tra 0 e M−1 coincidano:

  P(uguali) = 1/M
  
Questo vale per ogni posizione 'i' (da 0 a N-1).
Poichè ci sono N posizioni, la media attesa vale:

  Media = N * 1/M = N/M


----------------------------------
Numeri con somma dei fattori prima
----------------------------------

Sequenza OEIS A100118:
Numbers whose sum of prime factors is prime (counted with multiplicity).
  2, 3, 5, 6, 7, 10, 11, 12, 13, 17, 19, 22, 23, 28, 29, 31, 34, 37, 40,
  41, 43, 45, 47, 48, 52, 53, 54, 56, 58, 59, 61, 63, 67, 71, 73, 75, 76,
  79, 80, 82, 83, 88, 89, 90, 96, 97, 99, 101, 103, 104, 107, 108, 109,
  113, 117, 118, 127, 131, 136, 137, 139, 142, 147, 148, 149, ...

(define (seq? num)
  (prime? (apply + (factor num))))

(seq? 11)
;-> true

(filter seq? (sequence 2 149))
;-> (2 3 5 6 7 10 11 12 13 17 19 22 23 28 29 31 34 37 40
;->  41 43 45 47 48 52 53 54 56 58 59 61 63 67 71 73 75 76
;->  79 80 82 83 88 89 90 96 97 99 101 103 104 107 108 109
;->  113 117 118 127 131 136 137 139 142 147 148 149)


-------------------------------------------
Numeri composti con somma dei fattori prima
-------------------------------------------

Sequenza OEIS A046363:
Composite numbers whose sum of prime factors (with multiplicity) is prime.
  6, 10, 12, 22, 28, 34, 40, 45, 48, 52, 54, 56, 58, 63, 75, 76, 80, 82,
  88, 90, 96, 99, 104, 108, 117, 118, 136, 142, 147, 148, 153, 165, 172,
  175, 176, 184, 198, 202, 207, 210, 214, 224, 245, 248, 250, 252, 268,
  273, 274, 279, 294, 296, 298, 300, 316, 320, 325, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (seq? num)
  (let (f (factor num))
    (and (> (length f) 1) (prime? (apply + f)))))

(seq? 12)

(filter seq? (sequence 2 325))
;-> (6 10 12 22 28 34 40 45 48 52 54 56 58 63 75 76 80 82
;->  88 90 96 99 104 108 117 118 136 142 147 148 153 165 172
;->  175 176 184 198 202 207 210 214 224 245 248 250 252 268
;->  273 274 279 294 296 298 300 316 320 325)


-------
Repunit
-------

Una repunit R(n) è un numero come 11, 111 o 1111 che contiene n volte solo la cifra 1.
Il termine sta per "repeated unit" ed è stato coniato nel 1966 da Albert Beiler nel suo libro "Recreations in the Theory of Numbers".
Definizione:

  R(1) = 1
  R(n) = (10^n - 1)/9

Sequenza OEIS A002275:
Repunits: (10^n - 1)/9. Often denoted by R_n.
  0, 1, 11, 111, 1111, 11111, 111111, 1111111, 11111111, 111111111,
  1111111111, 11111111111, 111111111111, 1111111111111, ...

Proprietà
- Solo le repunit con un numero primo di cifre possono essere prime (condizione necessaria, ma non sufficiente).
- Tutte le repunit consecutive R(n−1) e R(n) sono prime tra loro per ogni n.

Sequenza OEIS A004022:
Primes of the form (10^k - 1)/9. Also called repunit primes or repdigit primes.
  11, 1111111111111111111, 11111111111111111111111, ...

(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

(define (primi-rep)
  (filter (fn(x) (prime? (/ (- (** 10 x) 1) 9))) (sequence 2 19 2)))

(define (primi-rep)
  (let ( (out '()) (rep 1) )
    (for (n 2 19) ; fino al numero di cifre del valore massimo di Int64 (19)
      (setq rep (/ (- (** 10 n) 1) 9))
      (if (prime? rep) (push rep out -1)))))

(primi-rep)
;-> (11L 1111111111111111111L)


---------------
Numero di Erdos
---------------

Il numero di Erdos è un modo per descrivere la "distanza" tra una persona e il matematico ungherese Paul Erdos in termini di collaborazione in pubblicazioni matematiche.
La definizione del numero di Erdos è ricorsiva: per possedere un numero di Erdos, un autore deve aver collaborato a una ricerca con un autore che possiede un numero di Erdos.
Erdos ha un numero di Erdos pari a 0.
Per ogni altro autore, se k è il minimo numero di Erdos dei suoi collaboratori, il suo numero di Erdos è k+1.
Di conseguenza, i coautori di Erdos hanno numero di Erdos 1, i loro coautori (se non hanno collaborato a loro volta con Erdos) hanno numero di Erdos 2, e così via.
Chi non ha nessuna catena di collaborazioni che porta a Paul Erdos non ha un numero di Erdos, oppure lo ha infinito.

Quindi per ogni pubblicazione abbiamo una lista di autori.
Per esempio  (E = Erdos):
(E A B) (E C) (E Z) (E D L) (K C) (L K)
Come calcolare i numeri di Erdos per A, B, C, D, E, L, K, e Z?

Per calcolare i numeri di Erdos a partire da una lista di pubblicazioni (ognuna con una lista di autori), possiamo modellare il problema come una visita in ampiezza (BFS) su un grafo in cui i nodi sono gli autori e gli archi rappresentano un collegamento tra due autori se hanno scritto un articolo insieme.

Algoritmo
1) Costruire un grafo in cui ogni autore è un nodo, e per ogni pubblicazione collegsre tutti gli autori tra loro.
2) Iniziare da "E" (Erdos) con numero 0.
3) Usare una coda per eseguire una BFS:
   - per ogni autore nella coda, assegna 'numero + 1' ai suoi vicini non ancora visitati.

Esempio:
pubblicazioni = ((E A B) (E C) (E Z) (E D L) (K C) (L K))

1) Trasformiamo le pubblicazioni in un grafo non orientato:
  E: A B C Z D L  
  A: E B  
  B: E A  
  C: E K  
  Z: E  
  D: E L  
  L: E D K  
  K: C L

2) Il numero di Erdos di E vale 0
  E = 0

3) BFS

Partiamo da E:
- Visitiamo i vicini di E: A B C Z D L --> numero Erdos 1
- Visitiamo i vicini di A B C Z D L:
  A --> già assegnato
  B --> già assegnato
  C --> collega K, ancora non assegnato --> K ha Erdos 2
  Z --> nessun altro
  D --> già fatto
  L --> già fatto
- Visitiamo K --> i vicini sono C, L --> già assegnati
Fine.

Output:
  (E 0) (A 1) (B 1) (C 1) (Z 1) (D 1) (L 1) (K 2)

; Funzione di utilità per cercare un valore in una lista associativa
(define (lookup-assoc chiave lista)
  (let ((ris (assoc chiave lista)))
    (if ris (last ris) nil)))

; Funzione di utilità per aggiornare o aggiungere una coppia chiave-valore in una lista associativa
(define (setf-assoc chiave valore lista)
  (let ((pos (find chiave (map first lista))))
    (if pos
        (begin
          (setf (last (lista pos)) valore)
          lista)
        (append lista (list (list chiave valore))))))

; Funzione per eliminare duplicati da una lista eventualmente annidata
(define (unique-flat lst)
  (unique (flat lst)))

; Funzione principale per calcolare i numeri di Erdos
(define (erdos pubblicazioni)
  (local (grafo erdos-num visited queue)
    ; Costruzione del grafo:
    ; lista associativa autore --> lista dei suoi coautori
    ; Per ogni pubblicazione, 
    ; colleghiamo ogni autore a tutti gli altri della stessa pubblicazione
    (setq grafo '())
    (dolist (pubblicazione pubblicazioni)
      (dolist (a pubblicazione)
        (dolist (b pubblicazione)
          (unless (= a b)
            (let ((vicini (or (lookup-assoc a grafo) '())))
              (setq grafo
                    (setf-assoc a
                                (unique-flat (append vicini (list b)))
                                grafo)))))))
    ;(println grafo)
    ; Inizializzazione della lista associativa dei numeri di Erdos
    (setq erdos-num '())
    ; Lista degli autori già visitati durante la BFS
    (setq visited '())
    ; Coda per la BFS: inizialmente contiene solo Erdos con numero 0
    (setq queue '(("E" 0)))
    ; Esecuzione della visita in ampiezza (BFS)
    (while queue
      (letn ((corrente (pop queue))
             (nome (corrente 0))
             (numero (corrente 1)))
        ; Se il nodo non è stato ancora visitato
        (unless (ref nome visited)
          ; Aggiungiamo il nodo alla lista dei visitati
          (push nome visited)
          ; Registriamo il numero di Erdos per questo autore
          (setq erdos-num (setf-assoc nome numero erdos-num))
          ; Aggiungiamo alla coda tutti i coautori con numero incrementato
          (dolist (vicino (or (lookup-assoc nome grafo) '()))
            (push (list vicino (+ numero 1)) queue -1)))))
    ; Ritorna la lista autore --> numero di Erdos
    erdos-num))

Proviamo:

(setq pubblicazioni '(
  ("E" "A" "B")
  ("E" "C")
  ("E" "Z")
  ("E" "D" "L")
  ("K" "C")
  ("L" "K")))
(erdos pubblicazioni)
;-> (("E" 0) ("A" 1) ("B" 1) ("C" 1) ("Z" 1) ("D" 1) ("L" 1) ("K" 2))

(setq pubblicazioni '(
  ("E" "A" "B")
  ("E" "C")
  ("E" "Z")
  ("E" "D" "C")
  ("K" "C")
  ("L" "K")
  ("K" "Y")))
(erdos pubblicazioni)
;-> (("E" 0) ("A" 1) ("B" 1) ("C" 1) ("Z" 1) ("D" 1) ("K" 2) ("L" 3) ("Y" 3))


---------------------
Curiosità aritmetiche
---------------------

Sottrazione incantevole
-----------------------

 987654321 -  -->  digit sum 45
 123456789 =  -->  digit sum 45
 ---------
 864197532    -->  digit sum 45

Schema 1
--------

  12345679 * 9 * 1 = 111111111
  12345679 * 9 * 2 = 222222222
  12345679 * 9 * 3 = 333333333
  12345679 * 9 * 4 = 444444444
  12345679 * 9 * 5 = 555555555
  12345679 * 9 * 6 = 666666666
  12345679 * 9 * 7 = 777777777
  12345679 * 9 * 8 = 888888888
  12345679 * 9 * 9 = 999999999

(define (schema1)
  (local (a b c)
    (setq a 12345679)
    (setq b 9)
    (for (k 1 9)
      (setq c k)
      (println a " * " b " * " c " = "  (* a b c))) '>))

(schema1)
;-> 12345679 * 9 * 1 = 111111111
;-> 12345679 * 9 * 2 = 222222222
;-> 12345679 * 9 * 3 = 333333333
;-> 12345679 * 9 * 4 = 444444444
;-> 12345679 * 9 * 5 = 555555555
;-> 12345679 * 9 * 6 = 666666666
;-> 12345679 * 9 * 7 = 777777777
;-> 12345679 * 9 * 8 = 888888888
;-> 12345679 * 9 * 9 = 999999999

Come viene creata?

(define (tabelle cifre)
  (local (base fattori cost molt)
    (setq base (/ (- (pow 10 cifre) 1) 9))
    (setq fattori (factor base))
    (println base " - " fattori)
    (dolist (f fattori)
      (println "f = " f)
      (setq cost f)
      (setq molt (/ (apply * fattori) f))
      (for (k 1 9)
        (println (string molt " * " cost " * " k " = " (* molt cost k)))
      )
      (for (k 1 9)
        (println (string molt " * " (* cost k) " = " (* molt cost k)))
      )
      (for (k 1 9)
        (println (string (* molt cost) " * " k " = " (* molt cost k)))
      )
      (print "----------------------------------------") (read-line))))

(tabelle 9)

Schema 2
--------

          1*9 +  2 = 11
         12*9 +  3 = 111
        123*9 +  4 = 1111
       1234*9 +  5 = 11111
      12345*9 +  6 = 111111
     123456*9 +  7 = 1111111
    1234567*9 +  8 = 11111111
   12345678*9 +  9 = 111111111
  123456789*9 + 10 = 1111111111

(define (schema2)
  (local (spaces a b c)
    (setq spaces 9)
    (for (k 1 9)
      (setq a "")
      (for (i 1 k) (extend a (string i)))
      (setq a (int a))
      (setq b 9)
      (setq c (+ k 1))
      (println (string (dup " " (- spaces k))
                a "*" b " +" (format "%3d" c) " = "
                (+ (* a b) c)))) '>))

(schema2)
;->         1*9 +  2 = 11
;->        12*9 +  3 = 111
;->       123*9 +  4 = 1111
;->      1234*9 +  5 = 11111
;->     12345*9 +  6 = 111111
;->    123456*9 +  7 = 1111111
;->   1234567*9 +  8 = 11111111
;->  12345678*9 +  9 = 111111111
;-> 123456789*9 + 10 = 1111111111

Schema 3
--------

          1*8 + 1 = 9
         12*8 + 2 = 98
        123*8 + 3 = 987
       1234*8 + 4 = 9876
      12345*8 + 5 = 98765
     123456*8 + 6 = 987654
    1234567*8 + 7 = 9876543
   12345678*8 + 8 = 98765432
  123456789*8 + 9 = 987654321

(define (schema3)
  (local (spaces a b c)
    (setq spaces 9)
    (for (k 1 9)
      (setq a "")
      (for (i 1 k) (extend a (string i)))
      (setq a (int a))
      (setq b 8)
      (setq c k)
      (println (string (dup " " (- spaces k))
                a "*" b " +" (format "%2d" c) " = "
                (+ (* a b) c)))) '>))

(schema3)
;->         1*8 + 1 = 9
;->        12*8 + 2 = 98
;->       123*8 + 3 = 987
;->      1234*8 + 4 = 9876
;->     12345*8 + 5 = 98765
;->    123456*8 + 6 = 987654
;->   1234567*8 + 7 = 9876543
;->  12345678*8 + 8 = 98765432
;-> 123456789*8 + 9 = 987654321

Schema4
-------

         9*9 + 7 = 88
        98*9 + 6 = 888
       987*9 + 5 = 8888
      9876*9 + 4 = 88888
     98765*9 + 3 = 888888
    987654*9 + 2 = 8888888
   9876543*9 + 1 = 88888888
  98765432*9 + 0 = 888888888

(define (schema4)
  (local (spaces a b c)
    (setq spaces 8)
    (for (k 1 8)
      (setq a "")
      (for (i 1 k) (extend a (string (- 10 i))))
      (setq a (int a))
      (setq b 9)
      (setq c (- 8 k))
      (println (string (dup " " (- spaces k))
                a "*" b " +" (format "%3d" c) " = "
                (+ (* a b) c)))) '>))

(schema4)
;->        9*9 +  7 = 88
;->       98*9 +  6 = 888
;->      987*9 +  5 = 8888
;->     9876*9 +  4 = 88888
;->    98765*9 +  3 = 888888
;->   987654*9 +  2 = 8888888
;->  9876543*9 +  1 = 88888888
;-> 98765432*9 +  0 = 888888888

Schema 5
--------

          1 * 1         =         1
         11 * 11        =        121
        111 * 111       =       12321
       1111 * 1111      =      1234321
      11111 * 11111     =     123454321
     111111 * 111111    =    12345654321
    1111111 * 1111111   =   1234567654321
   11111111 * 11111111  =  123456787654321
  111111111 * 111111111 = 12345678987654321

(define (schema5)
  (local (spaces a b c)
    (setq spaces 8)
    (for (k 1 9)
      (setq a (/ (- (pow 10 k) 1) 9))
      (setq sp (dup " " spaces))
      (println (string sp a " * " a sp " = " sp (* a a)))
      (-- spaces 1)) '>))
      ;(println (string (dup " " spaces) a " * " a " = " (* a a)))
      ;(-- spaces 2)) '>))

(schema5)
;->         1 * 1         =         1
;->        11 * 11        =        121
;->       111 * 111       =       12321
;->      1111 * 1111      =      1234321
;->     11111 * 11111     =     123454321
;->    111111 * 111111    =    12345654321
;->   1111111 * 1111111   =   1234567654321
;->  11111111 * 11111111  =  123456787654321
;-> 111111111 * 111111111 = 12345678987654321

Schema 6
--------

                        1 + 2 = 3
                    4 + 5 + 6 = 7 + 8
             9 + 10 + 11 + 12 = 13 + 14 + 15
       16 + 17 + 18 + 19 + 20 = 21 + 22 + 23 + 24
  25 + 26 + 27 + 28 + 29 + 30 = 31 + 32 + 33 + 34 + 35
  ecc.

(define (center-string str num-chars fill-char)
"Justify a string to the center"
  (letn ( (len-str (length str)) (space (- num-chars len-str)) )
    (setq fill-char (or fill-char " "))
    (if (even? space)
      ; centratura pari
      (append (dup fill-char (/ space 2)) str (dup fill-char (/ space 2))))
      ; centratura dispari
      (append (dup fill-char (/ space 2)) str (dup fill-char (+ (/ space 2) 1)))))

(define (schema6 rows)
  (local (table sx dx curr riga max-len)
    (setq table '())
    (setq sx 2)
    (setq dx 1)
    (setq curr 0)
    ; ciclo per ogni riga...
    (for (r 1 rows)
      (setq riga "")
      ; parte sinistra
      (for (s 1 sx)
        (++ curr)
        (push (string curr) riga -1)
        (if (!= s sx) (push " + " riga -1)))
      (++ sx)
      (push " = " riga -1)
      ; parte destra
      (for (d 1 dx)
        (++ curr)
        (push (string curr) riga -1)
        (if (!= d dx) (push " + " riga -1)))
      (++ dx)
      (push riga table -1)
      ;(println riga)
    )
    ; lunghezza della riga più lunga
    (setq max-len (length (table -1)))
    ; stampa centrata
    (dolist (el table)
      (if (odd? (length el)) (push " " el))
      (println (center-string el max-len " "))) '>))

(schema6 7)
;->                                 1 + 2 = 3
;->                             4 + 5 + 6 = 7 + 8
;->                      9 + 10 + 11 + 12 = 13 + 14 + 15
;->                16 + 17 + 18 + 19 + 20 = 21 + 22 + 23 + 24
;->           25 + 26 + 27 + 28 + 29 + 30 = 31 + 32 + 33 + 34 + 35
;->      36 + 37 + 38 + 39 + 40 + 41 + 42 = 43 + 44 + 45 + 46 + 47 + 48
;-> 49 + 50 + 51 + 52 + 53 + 54 + 55 + 56 = 57 + 58 + 59 + 60 + 61 + 62 + 63

============================================================================

