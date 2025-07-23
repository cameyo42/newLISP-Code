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


-----------------
Bandiera Italiana
-----------------

Dimensioni standard: 2/3
Dimensioni di guerra: 1/1

Colori (wikipedia):
  Pantone
  Verde:  (17-6153 TCX)
  Bianco: (11-0601 TCX)
  Rosso:  (18-1662 TCX)

  CMYK
  Verde:  (C:100 M:0 Y:100 K:0)
  Bianco: (C:0 M:0 Y:0 K:5)
  Rosso:  (C:0 M:100 Y:100 K:0)

  RGB
  Verde:  (R:0 G:140 B:69)
  Bianco: (R:244 G:249 B:255)
  Rosso:  (R:205 G:33 B:42)

Colori usati:
  Verde:  (R:0 G:53 B:26)     --> (ANSI 29)
  Bianco: (R:244 G:249 B:255) --> (ANSI 231)
  Rosso:  (R:73 G:12 B:13)    --> (ANSI 160)

(define (foreground color)
  (let (f (string "\027[38;5;" color "m"))
    (print f)))

(define (it h w)
  (local (colori banda)
    (setq colori '(29 231 160))
    (setq banda (/ w 3))
    (for (i 1 h)
      (dolist (c colori) (print (foreground c) (dup "█" banda)))
      (println))
      (print (foreground 255)) '>))

Proviamo:

(it 3 9)
(it 6 18)
(it 5 18)
(it 4 9)
(it 14 9)

Nota: la dimensione più comune di una cella nel terminale DOS è 9x14 pixel.
Comunque il carattere "█" ha un rapporto di 1 a 2 (circa).

(define (quadrato l w)
  (for (i 1 l) (println (foreground 231) (dup "█" (* l 2))))
    (print (foreground 255)) '>)

(quadrato 2)
(quadrato 5)
(quadrato 10)


-----------------
Talmudic division
-----------------

Nel Talmud babilonese (Bava Metzia 2a) viene riportato il seguente problema:
  Un uomo ha debiti con tre creditori rispettivamente per 100, 200 e 300 euro.
  Se l'uomo ha un totale di 100 euro, come ripartirli tra i debitori?
  Se l'uomo ha un totale di 200 euro, come ripartirli tra i debitori?
  Se l'uomo ha un totale di 300 euro, come ripartirli tra i debitori?

Il Talmud riporta la seguente soluzione:
  +--------+---------+---------+---------+----------------------------------+
  | Totale |  C1     |  C2     |  C3     | Metodo                           |
  +--------+---------+---------+---------+----------------------------------+
  |  100   |  33+1/3 |  33+1/3 |  33+1/3 | divisione in parti uguali        |
  +--------+---------+---------+---------+----------------------------------+
  |  200   |  50     |  75     |  75     | divisione ???                    |
  +--------+---------+---------+---------+----------------------------------+
  |  300   |  50     |  100    |  150    | divisione in parti proporzionali |
  +--------+---------+---------+---------+----------------------------------+

Nel Talmud troviamo anche un esempio con 2 creditori in cui uno vuole il 50% di una somma e l'altro vuole il 100%.
La soluzione consiste nel determinare la differenza tra le due richieste (parte disputata), dividere questa differenza per due e assegnare ognuna di queste parti ai due creditori.
La parte restante (parte non disputata) viene assegnata a chi ha chiesto la parte più alta.
In questo caso la parte disputata vale (1 - 1/2)/2 = 1/4.
Ognuno riceve 1/4.
Rimane 1/2 che viene assegnato a chi ha chiesto il 100%.
Quindi il primo creditore riceve 1/4 della somma e il secondo creditore riceve 3/4 della somma (qualunque sia la somma iniziale).

Questo metodo viene chiamato "equal divison of the contested sum" e nel 1980 Robert Aumann e Michael Maschler pubblicarono un algoritmo per estendere questo metodo al caso di N creditori.

Algoritmo per la divisione del patrimonio secondo il Talmud
-----------------------------------------------------------
1) Ordina i creditori in base all'entità del credito, dal più piccolo al più grande.
2) Dividi equamente il patrimonio tra tutti i creditori fino a quando il creditore con la richiesta più bassa ha ricevuto la metà del proprio credito.
3) Continua la divisione equa del patrimonio, escludendo il creditore che ha già ricevuto la metà, fino a quando il secondo creditore più basso raggiunge la metà del proprio credito.
4) Ripeti il processo, escludendo progressivamente i creditori che hanno già ricevuto la metà del loro credito, fino a quando ogni creditore ha ricevuto almeno la metà di quanto richiesto.
5) A questo punto, inverti la logica: comincia a distribuire il patrimonio restante partendo dal creditore con il credito più alto, fino a quando la perdita (cioè la differenza tra quanto richiesto e quanto ricevuto) di quel creditore è uguale alla perdita del creditore con il credito immediatamente inferiore.
6) Poi dividi equamente il patrimonio rimanente tra i creditori con la perdita maggiore, fino a quando le loro perdite diventano uguali a quella del gruppo successivo.
7) Prosegui in questo modo fino a quando tutto il patrimonio è stato assegnato.

Si tratta di un algoritmo di divisione proporzionale con soglia, noto come "Talmudic division", quando le risorse sono insufficienti a coprire tutti i debiti.

Esempio:
--------
Patrimonio totale: 200
Creditori:
  C1: 100
  C2: 200
  C3: 300

Fase 1: Prima metà (fino a metà delle richieste)
------------------------------------------------
Ordinati: C1 (100), C2 (200), C3 (300)
Obiettivo: dare fino a metà di ogni richiesta.
Quindi:
  C1 --> massimo 50
  C2 --> massimo 100
  C3 --> massimo 150
Totale massimo distribuito in questa fase: 50 + 100 + 150 = 300, ma abbiamo solo 200, quindi ci fermiamo prima.

Distribuzione in ordine

1. Distribuiamo equamente tra i 3 creditori finché C1 raggiunge la metà della sua richiesta (50).
   Ogni giro dà x euro a ciascuno. Dopo x euro:
     C1: riceve x --> vogliamo x = 50 --> 50
     C2: riceve x = 50
     C3: riceve x = 50
   Spesa totale: 3 * 50 = 150
   Rimangono: 200 - 150 = 50

2. Ora C1 è fuori (ha metà del suo credito).
   Restano C2 e C3, che hanno ricevuto 50 ciascuno (su un massimo di 100 e 150 rispettivamente).

3. Distribuiamo equamente tra C2 e C3 finché uno dei due raggiunge metà del proprio credito:
   Mancano ancora 50 --> dividiamo 25 a testa
   C2 arriva a 75
   C3 arriva a 75
   (C2 doveva arrivare a 100, quindi non ancora a metà: fermiamo qui perché i 200 sono finiti)

Risultato dopo la Fase 1:
  C1: 50
  C2: 75
  C3: 75

Ora passiamo alla seconda parte dell'algoritmo: lavorare all'indietro sui "loss" (le perdite).

Fase 2: Bilanciamento delle perdite
-----------------------------------
Calcoliamo quanto ha perso ogni creditore:

  +-----------+-----------+----------+---------+
  | Creditore | Richiesta | Ricevuto | Perdita |
  +-----------+-----------+----------+---------+
  | C1        | 100       | 50       | 50      |
  +-----------+-----------+----------+---------+
  | C2        | 200       | 75       | 125     |
  +-----------+-----------+----------+---------+
  | C3        | 300       | 75       | 225     |
  +-----------+-----------+----------+---------+

L'algoritmo ora mira a equa perdita tra i creditori partendo dai più grandi.
Cerchiamo di bilanciare la perdita del creditore più alto (C3) con quella del secondo (C2), e poi con C1 se possibile.

Ma nel nostro caso, i soldi sono già finiti nella Fase 1.
Quindi non possiamo fare alcun aggiustamento nella Fase 2: tutto è già stato distribuito.

Conclusione
-----------
La distribuzione finale secondo l'algoritmo descritto è:
  C1: 50
  C2: 75
  C3: 75

La somma è 200, e rispetta la logica:
a) Prima, garantire fino alla metà delle richieste (tutti ottengono fino a quel punto).
b) Poi, nessuna risorsa residua da riequilibrare le perdite, quindi non si prosegue.

(define (talmud-division totale crediti)
  (local (creditori n assegnati meta fine attivi k max-quote quota
          perdite max-perdita massimi prossime delta)
  ; Ordina i crediti in ordine crescente
  (setq creditori (sort crediti <))
  ; Numero di creditori
  (setq n (length creditori))
  ; Inizializza l'elenco degli importi assegnati a zero
  (setq assegnati (dup 0 n))
  ; Calcola la metà di ciascun credito
  (setq meta (map (fn (x) (div x 2)) creditori))
  ; FASE 1: Assegna fino a metà di ogni credito
  ; -------------------------------------------
  (setq fine nil) ; variabile di controllo ciclo
  (while (not fine)
    ; Trova gli indici dei creditori che non hanno ancora raggiunto la metà
    (setq attivi (filter (fn (i) (< (assegnati i) (meta i))) (sequence 0 (- n 1))))
    (setq k (length attivi))
    ; Se non ci sono più attivi o non ci sono fondi, esci dal ciclo
    (if (or (= k 0) (<= totale 0))
        (setq fine true)
        (begin
          ; Calcola quanto può ricevere ciascun attivo:
          ; il minimo tra ciò che gli manca e il totale diviso per k
          (setq max-quote (apply min (map (fn (i) (sub (meta i) (assegnati i))) attivi)))
          (setq quota (min max-quote (div totale k)))
          ; Se la quota è nulla o negativa, termina il ciclo
          (if (<= quota 0)
              (setq fine true)
              ; Altrimenti assegna la quota a ciascun attivo
              (dolist (i attivi)
                (inc (assegnati i) quota)
                (dec totale quota))))))
  ; FASE 2: Bilancia le perdite tra i creditori maggiori
  ; ----------------------------------------------------
  ; Calcola la perdita per ciascun creditore = credito - ricevuto
  (setq perdite (map sub creditori assegnati))
  (setq fine nil)
  (while (not fine)
    (if (<= totale 0)
        (setq fine true)
        (begin
          ; Trova la perdita massima
          (setq max-perdita (apply max perdite))
          ; Trova gli indici dei creditori con perdita massima
          (setq massimi (filter (fn (i) (= (perdite i) max-perdita)) (sequence 0 (- n 1))))
          (setq k (length massimi))
          ; Trova le perdite minori tra i restanti
          (setq prossime (filter (fn (i) (< (perdite i) max-perdita)) (sequence 0 (- n 1))))
          ; Calcola quanto si può ridurre la perdita dei massimi
          (if (null? prossime)
              (setq delta max-perdita)
              (begin
                (setq prox-max (apply max (map (fn (i) (perdite i)) prossime)))
                (setq delta (sub max-perdita prox-max))))
          ; La quota è il minimo tra 'delta' e 'totale / k'
          (setq quota (min delta (div totale k)))
          ; Se la quota è nulla, termina
          (if (<= quota 0)
              (setq fine true)
              ; Altrimenti assegna la quota a ciascun "massimo"
              (begin
                (dolist (i massimi)
                  (inc (assegnati i) quota)
                  (dec totale quota))
                ; Ricalcola le perdite aggiornate
                (setq perdite (map sub creditori assegnati)))))))
  ; Ritorna la lista degli importi assegnati a ciascun creditore
  assegnati))

Proviamo:

(talmud-division 100 '(100 200 300))
;-> (33.33333333333334 33.33333333333334 33.33333333333334)
(talmud-division 200 '(100 200 300))
;-> (50 75 75)
(talmud-division 300 '(100 200 300))
;-> (50 100 150)
Abbiamo trovato le stesse soluzioni del Talmud.

(talmud-division 700 '(100 200 300))
;-> (100 200 300)
(talmud-division 500 '(100 200 300))
;-> (66.66666666666667 166.6666666666667 266.6666666666667)
(talmud-division 103 '(100 200 300))
;-> (34.33333333333334 34.33333333333334 34.33333333333334)
(talmud-division 99 '(99 200 300))
;-> (33 33 33)
(talmud-division 50 '(100 200 300))
;-> (16.66666666666667 16.66666666666667 16.66666666666667)
(talmud-division 0 '(100 200 300))
;-> (0 0 0)

(talmud-division 1000 '(100 200 500 800))
;-> (50 100 275 575)
(talmud-division 10 '(1 2 3 4 5 6))
;-> (0.5 1 1.5 2 2.5 2.5)
(talmud-division 100 '(10 20 30 40 50 60))
;-> (5 10 15 20 25 25)


--------------------------------------------------
Punti collineari nei segmenti con coordinate intere
--------------------------------------------------

Abbiamo due punti 2D p1 = (x1,y1) e p2 = (x2,y2) con coordinate intere (anche negative).
Calcolare i punti di coordinate intere che appartengono al segmento che unisce i due punti (considerando anche p1 e p2).

Per calcolare 'quanti' sono i punti collineari con coordinate intere tra p1 e p2 possiamo usare la seguente formula:

  numero punti interi = gcd((abs(x2 - x1)), (abs(y2 - y1)))) + 1

(define (conta-punti-interi x1 y1 x2 y2)
  (+ 1 (gcd (abs (- x2 x1)) (abs (- y2 y1)))))

Proviamo:

(conta-punti-interi 5 10 10 5)
;-> 6
(conta-punti-interi -8 5 0 5)
;-> 9
(conta-punti-interi -3 -3 2 2)
;-> 6
(conta-punti-interi 0 0 0 10)
;-> 11
(conta-punti-interi -5 -1 1 8)
;-> 4
(conta-punti-interi -2 -5 1 10)
;-> 4

Per calcolare 'quali' sono i punti collineari con coordinate intere tra p1 e p2 usiamo il seguente algoritmo:
https://stackoverflow.com/questions/23729244/algorithm-to-calculate-the-number-of-lattice-points-in-a-polygon

1) Calcolare:

   a) delta-x = x2 - x1, delta-y = y2 - y1
   b) k = gcd((abs(delta-x)), (abs(delta-y))).

2) Definire il passo minimo in cui i punti rimangono interi:
 
   dx, dy = ((delta-x/k), (delta-y/k)))
  
3) I punti interi sul segmento sono:

   (x1 + i*delta-x), (y1 + i*delta-y), con i = 0, 1, ..., k. 

In totale (k + 1) punti (compresi gli estremi).

(define (punti-collineari x1 y1 x2 y2)
  (letn ((dx (- x2 x1))                   ; differenza orizzontale
         (dy (- y2 y1))                   ; differenza verticale
         (k (gcd (abs dx) (abs dy)))      ; quanti passi interi (inclusivi)
         (sx (/ dx k))                    ; passo orizzontale
         (sy (/ dy k)))                   ; passo verticale
    (map (fn(i) (list (+ x1 (* sx i))     ; genera i punti (x, y)
                      (+ y1 (* sy i))))
         (sequence 0 k))))                ; i da 0 a k inclusi

Proviamo:

(punti-collineari 5 10 10 5)
;-> ((5 10) (6 9) (7 8) (8 7) (9 6) (10 5))
(punti-collineari -8 5 0 5)
;-> ((-8 5) (-7 5) (-6 5) (-5 5) (-4 5) (-3 5) (-2 5) (-1 5) (0 5))
(punti-collineari -3 -3 2 2)
;-> ((-3 -3) (-2 -2) (-1 -1) (0 0) (1 1) (2 2))
(punti-collineari 0 0 0 10)
;-> ((0 0) (0 1) (0 2) (0 3) (0 4) (0 5) (0 6) (0 7) (0 8) (0 9) (0 10))
(punti-collineari -5 -1 1 8)
;-> ((-5 -1) (-3 2) (-1 5) (1 8))
(punti-collineari -2 -5 1 10)
;-> ((-2 -5) (-1 0) (0 5) (1 10))

Vedi anche "Punti collineari" su "Note libere 18".


---------------------------------------------------------
Multimagic Square - Quadrati magici di potenze dei numeri
---------------------------------------------------------

Un quadrato magico è bimagico (o 2-multimagico) se rimane magico dopo che ogni suo numero è stato elevato al quadrato.
Per estensione, un quadrato è P-multimagico se rimane magico dopo che ogni suo numero è stato sostituito dalla sua potenza k-esima (per k=1, 2, ..., a P).

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

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

Funzione che verifica la correttezza di un quadrato magico:
(restituisce il numero magico se è un quadrato magico, altrimenti nil)

(define (check matrix show)
  (local (result rows cols sum)
    (setq result '())
    ; somme righe
    (setq rows (length matrix))
    (setq cols (length (matrix 0)))
    (dolist (row matrix)
      (setq sum (apply + row))
      (push sum result -1)
      (if show (println "Row " (+ $idx 1) ": " sum))
    )
    ; somme colonne
    (setq trasposta (transpose matrix))
    (dolist (row trasposta)
      (setq sum (apply + row))
      (push sum result -1)
      (if show (println "Col " (+ $idx 1) ": " sum))
    )
    ; somma diagonale (alto sx --> basso dx)
    (setq sum 0L)
    (for (i 0 (- rows 1)) (++ sum (matrix i i)))
    (push sum result -1)
    (if show (println "D1:  " sum))
    ; somma diagonale (basso sx --> alto dx)
    (setq sum 0L)
    (for (i 0 (- rows 1)) (++ sum (matrix (- rows i 1) i)))
    (push sum result -1)
    (if show (println "D2:  " sum))
    ; le somme sono tutte uguali?
    (if (apply = result) sum nil)))

Funzione che calcola una data potenza di tutti i numeri di una matrice:

(define (pow-matrix matrix power)
  (let (out '())
    (dolist (row matrix)
      (push (map (fn(x) (** x power)) row) out -1))))

The Matt Parker Square (3x3) (2016)
-----------------------------------
Quasi-magico solo il quadrato

(setq parker '((29 1 47)
               (41 37 1)
               (23 41 29)))

(print-matrix (setq parker2 (pow-matrix parker 2)))
;->  841L    1L 2209L
;-> 1681L 1369L    1L
;->  529L 1681L  841L

(check parker2 true)
;-> Row 1: 3051L
;-> Row 2: 3051L
;-> Row 3: 3051L
;-> Col 1: 3051L
;-> Col 2: 3051L
;-> Col 3: 3051L
;-> D1:  3051L
;-> D2:  4107L
;-> nil

The Cheah Xu Heng Square (3x3) (2024)
-------------------------------------
Quasi-magico solo il quadrato

(setq heng '((222 381 6)
             (291 174 282)
             (246 138 339)))

(setq heng2 (pow-matrix heng 2))
;-> ((49284 145161 36) (84681 30276 79524) (60516 19044 114921))

(print-matrix (setq heng2 (pow-matrix heng 2)))
;-> 49284L 145161L     36L
;-> 84681L  30276L  79524L
;-> 60516L  19044L 114921L

(check heng2 true)
;-> Row 1: 194481L
;-> Row 2: 194481L
;-> Row 3: 194481L
;-> Col 1: 194481L
;-> Col 2: 194481L
;-> Col 3: 194481L
;-> D1:  194481L
;-> D2:  90828L
;-> nil

The Jaren Wakley Square (3x3) (2021)
------------------------------------
Quasi-magico solo il quadrato

(setq wakley '((1756 292 1234)
               (1006 1604 1052)
               (772 1426 1436)))

(print-matrix (setq wakley2 (pow-matrix wakley 2)))
;-> 3083536L   85264L 1522756L
;-> 1012036L 2572816L 1106704L
;->  595984L 2033476L 2062096L

(check wakley2 true)
;-> Row 1: 4691556L
;-> Row 2: 4691556L
;-> Row 3: 4691556L
;-> Col 1: 4691556L
;-> Col 2: 4691556L
;-> Col 3: 4691556L
;-> D1:  7718448L
;-> D2:  4691556L
;-> nil

The Georges Pfeffermann Square (8x8) (1890)
-------------------------------------------
BiMagico

(setq pfeffer '((56 34 8 57 18 47 9 31)
                (33 20 54 48 7 29 59 10)
                (26 43 13 23 64 38 4 49)
                (19 5 35 30 53 12 46 60)
                (15 25 63 2 41 24 50 40)
                (6 55 17 11 36 58 32 45)
                (61 16 42 52 27 1 39 22)
                (44 62 28 37 14 51 21 3)))

(check pfeffer)
;-> 260L

(setq pfeffer2 (pow-matrix pfeffer 2))
;-> ((3136 1156 64 3249 324 2209 81 961)
;->  (1089 400 2916 2304 49 841 3481 100)
;->  (676 1849 169 529 4096 1444 16 2401)
;->  (361 25 1225 900 2809 144 2116 3600)
;->  (225 625 3969 4 1681 576 2500 1600)
;->  (36 3025 289 121 1296 3364 1024 2025)
;->  (3721 256 1764 2704 729 1 1521 484)
;->  (1936 3844 784 1369 196 2601 441 9))

(print-matrix (setq pfeffer2 (pow-matrix pfeffer 2)))
;-> 3136L 1156L   64L 3249L  324L 2209L   81L  961L
;-> 1089L  400L 2916L 2304L   49L  841L 3481L  100L
;->  676L 1849L  169L  529L 4096L 1444L   16L 2401L
;->  361L   25L 1225L  900L 2809L  144L 2116L 3600L
;->  225L  625L 3969L    4L 1681L  576L 2500L 1600L
;->   36L 3025L  289L  121L 1296L 3364L 1024L 2025L
;-> 3721L  256L 1764L 2704L  729L    1L 1521L  484L
;-> 1936L 3844L  784L 1369L  196L 2601L  441L    9L

(check pfeffer2)
;-> 11180L

The Walter Trump square (12x12) (2002)
--------------------------------------
TriMagico

(setq trump '((1 22 33 41 62 66 79 83 104 112 123 144)
              (9 119 45 115 107 93 52 38 30 100 26 136)
              (75 141 35 48 57 14 131 88 97 110 4 70)
              (74 8 106 49 12 43 102 133 96 39 137 71)
              (140 101 124 42 60 37 108 85 103 21 44 5)
              (122 76 142 86 67 126 19 78 59 3 69 23)
              (55 27 95 135 130 89 56 15 10 50 118 90)
              (132 117 68 91 11 99 46 134 54 77 28 13)
              (73 64 2 121 109 32 113 36 24 143 81 72)
              (58 98 84 116 138 16 129 7 29 61 47 87)
              (80 34 105 6 92 127 18 53 139 40 111 65)
              (51 63 31 20 25 128 17 120 125 114 82 94)))

(setq trump2 (pow-matrix trump 2))
(check trump2)
;-> 83810L

(setq trump3 (pow-matrix trump 3))
(check trump3)
;-> 9082800L

http://multimagie.com/

TriMagic:
- Gaston Tarry (128x128) (1902)
- Eutrope Cazalas (62x62) (1933)
- William H. Benson (32x32) (1976)
- Walter Trump (12x12) (2002)

TetraMagic
- Charles Devimeux (256x256) (1983)
- Andre Vericel (512x512) (2001)
- Pang Fengchu (243x243) (2004)

PentaMagic
- Andre Vericel (1024x1024) (2001)
- Li Wen (729x729)
ecc.

Proviamo a verificare il quadrato TetraMagico 256x256 di Charles Devimeux.
Il file "devimeux.lsp" si trova nella cartella "data".

(load "devimeux.lsp")
(length devimeux)
;-> 256
(map length devimeux)

(check devimeux)
;-> 8388480L
(check (setq devimeux2 (pow-matrix devimeux 2)))
;-> 366495487360L
 (check (setq devimeux3 (pow-matrix devimeux 3)))
;-> 18013848757862400L
(check (setq devimeux4 (pow-matrix devimeux 4)))
;-> 944437268143413954688L

Nota: la difficolta di costruzione dei quadrati multimagici aumenta con il diminuire delle dimensioni del quadrato.
Questo perchè per ogni somma abbiamo N numeri da modificare per ottenere il risultato voluto.
Nel 2025 ancora non è stato scoperto nessun quadrato multimagico 3x3 (e non si sa neanche se esiste).
Invece è stato dimostrato che esistono quadrati multimagici per ogni dimensione maggiore di 3.


------------
Gara di moto
------------

Durante una gara di moto, vengono registrate le posizioni di ogni pilota alla partenza e al termine di ogni giro fino alla fine della corsa.
Nella registrazione ogni pilota appare (giri + 1) volte (consideriamo anche la partenza).
Determinare, per ogni pilota, le sue posizioni dal primo all'ultimo giro.
Le posizioni devono essere elencate in ordine di arrivo dei piloti.

Esempio: gara con 3 piloti e 4 giri
  piloti: a b c
               Posizioni
  partenza --> a b c
  giro 1   --> a c b
  giro 2   --> c a b
  giro 3   --> c a b
  giro 4   --> a c b
  Input: ((a b c) (a c b) (c a b) (c a b) (a c b))
  Output: (non consideriamo la posizione di partenza)
  pilota a --> (1 2 2 1)
  pilota c --> (2 1 1 2)
  pilota b --> (3 3 3 3)

Durante un giro gara possono verificarsi i seguenti casi:
1) sorpasso tra due o più piloti (cambiano le posizioni tra i piloti)
2) ritiro di uno o più piloti (mancano alcuni piloti)
3) i doppiati sono trattati normalmente (cioè, quando arrivano al giro i-esimo vengono inseriti nella lista i-esima in base alla loro posizione in quel giro) e terminano comunque la corsa.

Esempio: gara con 3 piloti e 4 giri (con ritiro)
  piloti: a b c
               Posizioni
  partenza --> a b c
  giro 1   --> a c b
  giro 2   --> c a b
  giro 3   --> c b (a ritirato)
  giro 4   --> b c
  Input: ((a b c) (a c b) (c a b) (c b) (b c))
  Output: (non consideriamo la posizione di partenza)
  pilota b --> (3 3 2 1)
  pilota c --> (2 1 1 2)
  pilota a --> (1 2 r r)

(define (andamento posizioni)
  (let ( (out '()) (piloti (posizioni 0)) (tmp '()) )
    ;(println (posizioni 0))
    ; ciclo per ogni pilota
    (dolist (p piloti)
        (setq tmp '())
        ; ciclo per ogni giro
        (dolist (g (slice posizioni 1)) ; togliamo la posizione di partenza
          (setq pos (find p g))
          ; se esiste il pilota in questo giro,
          ; allora aggiungiamo la posizione corrente
          (if (setq pos (find p g)) 
              (push (+ pos 1) tmp -1)
              (push 'r tmp -1)))
      ; inseriamo la lista corrente 'tmp' (pilota (posizioni)) alla soluzione
      (push (list p tmp) out -1))
    ;(println out)
    ; ordina la lista in base alla posizione di arrivo dei piloti
    (sort out (fn(x y) (<= (x 1 -1) (y 1 -1))))))

Proviamo:
(setq lst '((a b c) (a c b) (c a b) (c a b) (a c b)))
(andamento lst)
;-> ((a (1 2 2 1)) (c (2 1 1 2)) (b (3 3 3 3)))

(setq lst '((a b c) (a c b) (c a b) (c b) (b c)))
(andamento lst)
;-> ((b (3 3 2 1)) (c (2 1 1 2)) (a (1 2 r r)))

(setq piloti '(a b c d e f))
(setq giri (list piloti))
(extend giri (collect (randomize piloti true) 5))
; al sesto giro si ritira il pilota f
(setq piloti '(a b c d e))
(extend giri (collect (randomize piloti true) 11))
; al dodicesimo giro si ritira il pilota b
(setq piloti '(a c d e))
(extend giri (collect (randomize piloti true) 4))

(andamento giri)
;-> ((c (3 3 2 4 6 1 1 4 2 5 4 1 3 3 3 3 1 2 2 1))
;->  (a (6 6 4 5 1 2 3 3 3 1 3 3 5 2 5 4 4 1 3 2))
;->  (e (5 5 6 3 4 3 2 2 5 3 1 4 2 4 1 1 2 3 1 3))
;->  (d (4 4 3 1 2 5 5 1 4 2 2 2 1 1 4 2 3 4 4 4))
;->  (b (1 2 5 6 5 4 4 5 1 4 5 5 4 5 2 5 r r r r))
;->  (f (2 1 1 2 3 r r r r r r r r r r r r r r r)))


------------------
Undo/Redo classico
------------------

Vediamo alcune funzioni per spiegare il meccanismo di base delle operazioni di Undo e Redo.

Modello 1: Cronologia lineare storica
-------------------------------------
- Ogni operazione (anche dopo un 'undo') aggiunge un nuovo stato e non cancella nulla.
- 'redo' è sempre possibile, anche dopo nuove modifiche.
- La 'redo-list' funziona come uno stack parallelo, che si arricchisce indipendentemente.

Pro:
- Mantiene una cronologia completa di tutte le operazioni
- Permette di "navigare" avanti e indietro tra tutti gli stati, come una 'timeline'.
- Più flessibile per scenari in cui si vuole esplorare la cronologia senza perdere nulla.

Contro
- Si discosta dal comportamento classico degli editor (Ctrl+Z -> modifica -> niente più Ctrl+Y).
- Può confondere l'utente se si aspetta che un nuovo 'insert' annulli il percorso di redo.

Modello 2: Timeline a ramo unico (comportamento classico degli editor)
----------------------------------------------------------------------
- Se facciamo 'undo', poi eseguiamo una nuova operazione ('insert', 'remove'), la 'redo-list' viene azzerata.
- Il nuovo stato "taglia" la cronologia futura.

Pro:
- Si comporta come gli utenti si aspettano da Notepad, Gimp, ecc.
- Più semplice per l'utente finale.

Contro:
- Perdiamo la possibilità di tornare su percorsi alternativi nella cronologia.

Implementazione Modello 1
-------------------------
- 'redo' è sempre possibile, anche dopo modifiche.
- Le due pile 'undo-list' e 'redo-list' sono indipendenti, gestite simmetricamente.
- La 'redo-list' non viene mai cancellata.

; Stato globale
(setq lista '())     ; La lista principale modificata dall'utente
(setq undo-list '()) ; Stack degli stati precedenti (per undo)
(setq redo-list '()) ; Stack degli stati successivi (per redo)

; Inserisce un elemento alla fine della lista
(define (insert el)
  ; Salva lo stato corrente per permettere undo
  (push lista undo-list)
  ; NON si azzera la redo-list: manteniamo la cronologia completa
  (push el lista -1)
  lista)

; Rimuove l'ultimo elemento della lista
(define (remove)
  (when lista
    ; Salva lo stato corrente per permettere undo
    (push lista undo-list)
    ; NON si azzera la redo-list
    (pop lista -1))
  lista)

; Ripristina lo stato precedente (undo)
(define (undo)
  (when undo-list
    ; Salva lo stato attuale per permettere redo
    (push lista redo-list)
    ; Ripristina l'ultimo stato precedente
    (setq lista (pop undo-list)))
  lista)

; Riapplica uno stato annullato (redo)
(define (redo)
  (when redo-list
    ; Salva lo stato attuale per permettere undo
    (push lista undo-list)
    ; Ripristina lo stato successivo
    (setq lista (pop redo-list)))
  lista)

Proviamo:

(insert 'a)  --> (a)
(insert 'b)  --> (a b)
(undo)       --> (a)
(insert 'c)  --> (a c)
(redo)       --> (a b)

Implementazione Modello 2
-------------------------
- Dopo ogni 'undo', se si fa 'insert' o 'remove', si azzera la 'redo-list', ma solo in quel caso.
- Bisogna sapere se l'ultimo comando è stato 'undo', prima di decidere se azzerare la 'redo-list'.
- Questo richiede una variabile di stato che tenga traccia dell'ultima azione.
- last-op tiene traccia dell'ultima operazione, (quindi possiamo sapere se siamo nel "ramo alternativo").
- Il comportamento finale è esattamente quello di un editor tradizionale con Ctrl+Z e Ctrl+Y.

; Stato globale
(setq lista '())     ; La lista principale che l'utente modifica
(setq undo-list '()) ; Stack degli stati precedenti (per undo)
(setq redo-list '()) ; Stack degli stati futuri (per redo)
(setq last-op 'none) ; Ultima operazione ('insert, 'remove, 'undo, 'redo)

; Inserisce un elemento alla fine della lista
(define (insert el)
  ; Salva lo stato corrente per poter fare undo
  (push lista undo-list)
  ; Se l'ultima operazione è stata un undo, la redo-list non è più valida
  (if (= last-op 'undo) (setq redo-list '()))
  ; Aggiunge l'elemento alla lista
  (push el lista -1)
  ; Aggiorna il tipo dell'ultima operazione
  (setq last-op 'insert)
  lista)

; Rimuove l'ultimo elemento della lista
(define (remove)
  (when lista
    ; Salva lo stato per undo
    (push lista undo-list)
    ; Invalida redo se l'ultimo comando era un undo
    (if (= last-op 'undo) (setq redo-list '()))
    ; Rimuove l'ultimo elemento
    (pop lista -1))
  ; Aggiorna il tipo dell'ultima operazione
  (setq last-op 'remove)
  lista)

; Ripristina lo stato precedente
(define (undo)
  (when undo-list
    ; Salva lo stato attuale per permettere il redo
    (push lista redo-list)
    ; Ripristina l'ultimo stato precedente
    (setq lista (pop undo-list))
    ; Registra che è stato fatto un undo
    (setq last-op 'undo))
  lista)

; Riapplica uno stato annullato (solo se non invalidato da insert/remove dopo un undo)
(define (redo)
  (when redo-list
    ; Salva lo stato attuale per permettere eventuale undo
    (push lista undo-list)
    ; Ripristina lo stato successivo
    (setq lista (pop redo-list))
    ; Registra che è stato fatto un redo
    (setq last-op 'redo))
  lista)

Proviamo:

(insert 'a)  --> (a)
(insert 'b)  --> (a b)
(undo)       --> (a)
(insert 'c)  --> (a c)
(redo)       --> (a c)

Nota: quando il passaggio tra due stati avviene con una funzione invertibile, allora nelle liste 'undo' e 'redo' possiamo memorizzare solo la funzione inversa (risparmio di memoria).
Questo approccio corrisponde a una macchina a stack di comandi invertibili, un pattern chiamato anche Command Pattern con undo/redo.
In ambito funzionale e logico si parla anche di 'reversible computation'.


--------------------
Lettura al contrario
--------------------

I correttori di bozze a volte leggono le righe al contrario.
Questa tecnica, detta anche "lettura al contrario" o "lettura a ritroso", è un metodo utilizzato per individuare errori che potrebbero sfuggire durante una lettura tradizionale.
Leggendo il testo al contrario, il correttore si concentra sulla singola parola o frase, senza farsi trascinare dal significato complessivo del testo, riducendo così la probabilità di ignorare errori di battitura, refusi o piccoli errori grammaticali. 

Data una stringa che contiene un testo su più righe, scrivere una funzione che inverte le parole di ogni riga.
La funzione deve essere la più breve possibile.

(setq str 
"Questa settimana vado al mare.
Stasera mangio la pizza con la mozzarella.
Senti, corriamo più piano.
Attenzione: pericolo valanghe")

(define (bozze str)
  (let (righe (map parse (parse str "\r\n")))
    (dolist (r righe) (println (join (reverse r) " ")))))

(bozze str)

Versione code-golf (90 caratteri):
(define(b s)(let(a(map parse(parse s "\r\n")))(dolist(r a)(println(join(reverse r)" ")))))

(b str)
;-> mare. al vado settimana Questa
;-> mozzarella. la con pizza la mangio Stasera
;-> piano. più corriamo , Senti
;-> valanghe pericolo Attenzione


-------------------------------------------
Estrazione casuale di elementi da una lista
-------------------------------------------

Data una lista di elementi (non necessariamente univoci) dobbiamo estrarre alcuni elementi in modo casuale.

(setq lst '(a b c d a e f b g h g))

1) Estrazione casuale di N elementi
-----------------------------------

(define (sample-list num lst)
  (if (> num (length lst)) '()
      (slice (randomize lst) 0 num)))

Proviamo:

(sample-list 5 lst)
;-> (b b f e a)
(sample-list 5 lst)
;-> (e c a b a)
(sample-list 5 lst)
;-> (a h b g f)

2) Estrazione casuale di N elementi univoci
-------------------------------------------

(define (sample-list-unique num lst)
  (letn ( (unici (unique lst)) (len (length unici)) )
    (if (> num len) '()
        (slice (randomize unici) 0 num))))

(sample-list-unique 5 lst)
;-> (h g a c b)
(sample-list-unique 5 lst)
;-> (b h d g a)

Questa funzione estrae correttamente N elementi univoci, ma gli elementi con più occorrenze hanno la stessa probabilità di essere estratti di quelli che compaiono solo una volta (perchè estraiamo dalla lista dei numeri unici).
Se vogliamo che i numeri con più occorrenze abbiamo una maggiore probabilità di essere estratti dobbiamo usare un altro metodo.

(define (sample-list-unique num lst)
  (letn ( (len (length lst)) 
          (unici (unique lst)) (len-u (length unici))
          (out '()) )
    (if (> num len-u) '()
        (begin
          ; creazione di una hashmap
          (new Tree 'myHash)
          (until (= (length (myHash)) num)
            ; estrae un elemento a caso
            (setq value (lst (rand len)))
            ; inserisce valore casuale nell'hash (se non esiste)
            (myHash (string value) value))
          ; assegnazione dei valori dell'hashmap ad una lista
          (setq out (myHash))
          (setq out (map last out))
          ; eliminazione dell'hashmap
          (delete 'myHash)
          ; out è ordinata, quindi dobbiamo mischiare gli elementi
          (randomize out)))))

Proviamo:

(sample-list-unique 5 lst)
;-> (h e c b g)
(sample-list-unique 5 lst)
;-> (a d b e g)

Questa funzione rallenta con l'aumentare delle occorrenze degli elementi (perchè abbiamo una maggiore probabilità di selezionare un numero già scelto prima).

Proviamo con una lista di 20003 elementi (20000 a, 1 b, 1 c, 1 d):

(silent (setq t (dup 'a 20000))
        (push 'b t 5000)
        (push 'c t 10000)
        (push 'd t 15000))

(time (println (sample-list-unique 1 t)))
;-> (a)
;-> 9.519

(time (println (sample-list-unique 2 t)))
;-> (b a)
;-> 339.99

(time (println (sample-list-unique 3 t)))
;-> (b a d)
;-> 474.957

(time (println (sample-list-unique 4 t)))
;-> (b c a d)
;-> 644.405


-------------------------
Distanza tra numeri primi
-------------------------

Sequenza OEIS A001223:
Prime gaps: differences between consecutive primes.
  1, 2, 2, 4, 2, 4, 2, 4, 6, 2, 6, 4, 2, 4, 6, 6, 2, 6, 4, 2, 6, 4, 6,
  8, 4, 2, 4, 2, 4, 14, 4, 6, 2, 10, 2, 6, 6, 4, 6, 6, 2, 10, 2, 4, 2,
  12, 12, 4, 2, 4, 6, 2, 10, 6, 6, 6, 2, 6, 4, 2, 10, 14, 4, 2, 4, 14,
  6, 10, 2, 4, 6, 8, 6, 6, 4, 6, 8, 4, 8, 10, 2, 10, 2, 6, 4, 6, 8, 4,
  2, 4, 12, 8, 4, 8, 4, 6, 12, ...

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

(define (pair-func lst func rev)
"Produces a list applying a function to each pair of elements of a list"
      (if rev
          (map func (chop lst) (rest lst))
          (map func (rest lst) (chop lst))))

(define (dist-primi limite)
  (pair-func (primes-to limite) -))

(dist-primi 500)
;-> (1 2 2 4 2 4 2 4 6 2 6 4 2 4 6 6 2 6 4 2 6 4 6
;->  8 4 2 4 2 4 14 4 6 2 10 2 6 6 4 6 6 2 10 2 4 2
;->  12 12 4 2 4 6 2 10 6 6 6 2 6 4 2 10 14 4 2 4 14
;->  6 10 2 4 6 8 6 6 4 6 8 4 8 10 2 10 2 6 4 6 8 4
;->  2 4 12 8 4 8)


------------------
GraphWiz e newLISP
------------------

https://graphviz.org/

What is Graphviz?
-----------------
Graphviz is open source graph visualization software. Graph visualization is a way of representing structural information as diagrams of abstract graphs and networks.
It has important applications in networking, bioinformatics, software engineering, database and web design, machine learning, and in visual interfaces for other technical domains.

Features
--------
The Graphviz layout programs take descriptions of graphs in a simple text language, and make diagrams in useful formats, such as images and SVG for web pages, PDF or Postscript for inclusion in other documents, or display in an interactive graph browser.
Graphviz has many useful features for concrete diagrams, such as options for colors, fonts, tabular node layouts, line styles, hyperlinks, and custom shapes.

Command Line of DOT rendering program:
All Graphviz programs have a similar invocation:

cmd [ flags ] [ input files ]

Esempio:

Un esempio di file in formato .dot per i grafi orientati è il seguente:

file: grafo.dot

digraph G {
  A -> B [label="4"];
  B -> C [label="5"];
  B -> D [label="2"];
  C -> D [label="3"];
  D -> C [label="3"];
  D -> A [label="6"];
  D -> E [label="6"];
  E -> B [label="3"];
  E -> C [label="4"];
}

Adesso con il programma GraphWiz possiamo costruire il grafo con il seguente comando da terminale:

c:\util\Graphviz-13.1.0-win64\bin>dot -Tpng grafo.dot -o graphwiz.png

Il file "graphwiz.png" si trova nella cartella "data".

Formati di output:
PNG:
dot -Tpng grafo.dot -o grafo.png

PDF:
dot -Tpdf grafo.dot -o grafo.pdf

SVG:
dot -Tsvg grafo.dot -o grafo.svg

Comandi alternativi:

fdp: visualizzatore mediamente veloce (layout meno preciso):
fdp -Tpng grafo.dot -o grafo.png

neato: visualizzatore più veloce (layout peggiore):
neato -Tpdf grafo.dot -o grafo.pdf

Per maggiori informazioni riferirsi alla documentazione di GraphWiz:
https://graphviz.org/documentation/


-------------------------------
Programming with Bits and Bytes
-------------------------------

https://codegolf.stackexchange.com/questions/57204/programming-with-bits-and-bytes

Nota:
Tutto il contenuto dei siti di Stack Exchange è rilasciato sotto la licenza CC BY-SA 4.0 (Creative Commons Attribution-ShareAlike 4.0).

In questa sfida scriverai un interprete per un linguaggio semplice che ho inventato.
Il linguaggio si basa su un singolo accumulatore A, che ha una lunghezza di un byte esatto (8 bit).
All'inizio di un programma, A = 0.
Queste sono le istruzioni del linguaggio:

!: Inversione (Inversion)
Questa istruzione inverte semplicemente ogni bit dell'accumulatore.
Ogni zero diventa uno e ogni uno diventa zero. Semplice!

>: Sposta a destra (shift-right)
Questa istruzione sposta ogni bit di A di una posizione a destra.
Il bit più a sinistra diventa uno zero e il bit più a destra viene scartato.

<: Sposta a sinistra (shift-left)
Questa istruzione sposta ogni bit di A di una posizione a sinistra.
Il bit più a destra diventa uno zero e il bit più a sinistra viene scartato.

@: Scambia Nybbles (swap-nybbles)
Questa istruzione scambia i quattro bit superiori di A con i quattro bit inferiori.
Ad esempio, se A è 01101010 e si esegue @, A sarà 10100110:

 ____________________
 |                  |
0110 1010    1010 0110
      |_______|

$: Stampa (print-acc)
Stampa il contenuto dell'accumulatore A (binario e intero)

Funzioni per le istruzioni del linguaggio:

(define (inversion n)  (& (~ n) 255))
(define (shift-right n) (>> n 1))
; shift-left mascherato per rimanere entro 8 bit
(define (shift-left n) (& (<< n 1) 255))
(define (swap-nybbles n)  (| (>> n 4) (& (<< n 4) 240)))
(define (print-acc n) (println "A = " (format "%3d - %08s" n (bits n))))

Funzione che esegue un programma nel nostro linguaggio:

(define (run prg verbose)
  (let (A 0)
    (if verbose (println "Init: " A))
    (dolist (cmd (explode prg))
      ;(println cmd)
      (cond ((= cmd "!") (setq A (inversion A)))
            ((= cmd ">") (setq A (shift-right A)))
            ((= cmd "<") (setq A (shift-left A)))
            ((= cmd "@") (setq A (swap-nybbles A)))
            ((= cmd "$") (print-acc A))
            ((= cmd " ") nil)  ; spazio escluso
            ((= cmd "\r") nil) ; return escluso
            ((= cmd "\n") nil) ; newline escluso
            (true (println "Error: '" cmd "' unknown.")))
      (if verbose (println cmd " --> A = " (format "%3d - %08s" A (bits A)))))
    A))

Proviamo:

(run "!")
;-> 255
(run "!>>")
;-> 63
(run "!<@")
;-> 239

(run "!$<$@$")
;-> A = 255 - 11111111
;-> A = 254 - 11111110
;-> A = 239 - 11101111
;-> 239

(run "!$<$@$" true)
Init: 0
;-> ! --> A = 255 - 11111111
;-> A = 255 - 11111111
;-> $ --> A = 255 - 11111111
;-> < --> A = 254 - 11111110
;-> A = 254 - 11111110
;-> $ --> A = 254 - 11111110
;-> @ --> A = 239 - 11101111
;-> A = 239 - 11101111
;-> $ --> A = 239 - 11101111
;-> 239

(run "!nop!&6*!")
;-> Error: 'n' unknown.
;-> Error: 'o' unknown.
;-> Error: 'p' unknown.
;-> Error: '&' unknown.
;-> Error: '6' unknown.
;-> Error: '*' unknown.
;-> 255

(setq s "!
        <
        k
        @")
(run s)
;-> Error: 'k' unknown.
;-> 239

Problema: scrivere un programma che stampa i numeri da 1 a 5.

Soluzione: "!<!$ <$ >!<!$ ><<$ >!<!$"

(run "!<!$ <$ >!<!$ ><<$ >!<!$")
;-> A =   1 - 00000001
;-> A =   2 - 00000010
;-> A =   3 - 00000011
;-> A =   4 - 00000100
;-> A =   5 - 00000101

Come abbiamo fatto?

Il linguaggio permette solamente di partire da 255 e arrivare (dopo un certo numero di operazioni) ad un altro numero.
Quanti numeri possiamo raggiungere partendo da 255?

Possiamo modellare lo spazio degli stati dell'accumulatore A partendo dal valore iniziale 11111111 (255) e applicando ricorsivamente le operazioni !, >, <, @ fintanto che il valore non raggiunge 0 (che è uno stato terminale).
Per ogni numero n != 0, costruiamo una lista con elementi del tipo:

  (n (! n) (> n) (< n) (@ n))

e arrestiamo la costruzione se uno dei risultati è 0.
Usiamo una funzione ricorsiva che genera elementi/nodi di 5 elementi.
Per evitare cicli usiamo una lista di numeri visitati.
La funzione genera tutte le transizioni raggiungibili partendo da un dato numero e ci permette di capire quali numeri sono raggiungibili da un certo stato.


Funzione che genera tutte le possibili transizioni partendo da uno stato iniziale:

(define (genera-transizioni start)
(define (step n)
  (list n (inversion n) (shift-right n) (shift-left n) (swap-nybbles n)))
  (letn ((visitate '())
         (risultato '()))
    (define (esplora n)
      (unless (or (= n 0) (ref n visitate))
        (push n visitate)
        (let ((riga (step n)))
          (push riga risultato -1)
          (dolist (x (rest riga))
            (esplora x)))))
    (esplora start)
    risultato))

Proviamo:

(setq transizioni (genera-transizioni 255))
;-> ((255 0 127 254 255) (127 128 63 254 247) (128 127 64 0 8)
;->  (64 191 32 128 4) (191 64 95 126 251) (95 160 47 190 245)
;->  (160 95 80 64 10) (80 175 40 160 5) (175 80 87 94 250)
;->  ...
;->  (59 196 29 118 179) (28 227 14 56 193) (227 28 113 198 62)
;->  (198 57 99 140 108) (57 198 28 114 147) (4 251 2 8 64)
;->  (251 4 125 246 191) (53 202 26 106 83) (202 53 101 148 172)) 

Vediamo quanti numeri possiamo raggiungere partendo da 255:
(length transizioni)
;-> 255

Vediamo quali sono questi 255 numeri:
(setq raggiungibili (unique (sort (map first transizioni))))
;-> (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
;->  26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
;->  48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69
;->  70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 86 88 89 90 91 92 94
;->  96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113
;->  114 115 116 118 119 120 121 122 123 124 126 127 128 129 130 131 132
;->  133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149
;->  150 151 152 154 156 158 160 161 162 163 164 165 166 167 168 169 172
;->  173 176 177 178 179 180 181 182 183 184 188 192 193 194 195 196 197
;->  198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 214 216
;->  218 220 222 224 225 226 227 228 229 230 231 232 233 236 237 238 239
;->  240 241 242 243 244 246 247 248 252 254 255)
(length raggiungibili)
;-> 255

(difference (sequence 1 255) (map first transizioni))
;-> ()

Quindi partendo da 255 possiamo raggiungere tutti i numeri da 1 a 255.
(Il numero 0 lo possiamo raggiungere partendo da 1 e applicando '!').

Queste transizioni rappresentano lo spazio degli stati raggiungibili e può essere visto come un grafo orientato fortemente connesso (da ogni nodo è possibile raggiungere qualunque altro nodo).

Possiamo scrivere una funzione 'trova-path' che cerca nel grafo il percorso da un numero ad un altro.
Il risultato è una lista di coppie (numero operazione).

(define (trova-path start target)
  ; Inizializza la coda con un percorso contenente 
  ; solo il nodo iniziale e operazione vuota
  (letn ((queue (list (list (list start ""))))
         ; Array per segnare i nodi già visitati
         (visitati (array 256 '(nil)))
         ; Flag per indicare se il percorso è stato trovato
         (trovato nil)
         ; Variabile per salvare il percorso trovato
         (risultato nil))
    ; Continua finché la coda non è vuota e il percorso non è stato trovato
    (while (and queue (not trovato))
      ; Estrai il primo percorso dalla coda
      (let ((path (pop queue)))
        ; Prendi il valore dell'ultimo nodo del percorso
        (let (n (first (last path)))
          ; Se il nodo non è stato ancora visitato
          (unless (visitati n)
            ; Segna il nodo come visitato
            (setf (visitati n) true)
            ; Se è il nodo target, salva il percorso e ferma il ciclo
            (if (= n target)
                (begin
                  (setq trovato true)
                  (setq risultato path))
                ; Altrimenti, genera le transizioni possibili
                (dolist (t (list (list (>> n 1) ">")
                                 (list (& (<< n 1) 255) "<")
                                 (list (| (>> n 4) (& (<< n 4) 240)) "@")
                                 (list (& (~ n) 255) "!")))
                  (letn ((m (t 0))     ; nuovo numero raggiunto
                         (op (t 1)))   ; operazione usata
                    ; Aggiungi alla coda se il nodo è valido e non visitato
                    (unless (or (= m 0) (visitati m))
                      ; Costruisci un nuovo path aggiungendo la coppia (m op)
                      (push (append path (list (list m op))) queue -1)))))))))
    ; Restituisci il percorso trovato (lista di coppie (numero operazione))
    risultato))

Proviamo:

(trova-path 255 1)
;-> ((255 "") (254 "<") (1 "!"))

(trova-path 255 142)
;-> ((255 "") (254 "<") (252 "<") (248 "<") (124 ">") (199 "@") (142 "<"))

Con questa funzione possiamo costruire il programma che stampa i numeri da 1 a 5:

(trova-path 255 1)
;-> ((255 "") (254 "<") (1 "!"))
(trova-path 1 2)
;-> ((1 "") (2 "<"))
(trova-path 2 3)
;-> ((2 "") (1 ">") (254 "!") (252 "<") (3 "!"))
(trova-path 3 4)
;-> ((3 "") (1 ">") (2 "<") (4 "<"))
(trova-path 3 4)
;-> ((3 "") (1 ">") (2 "<") (4 "<"))
(trova-path 4 5)
;-> ((4 "") (2 ">") (253 "!") (250 "<") (5 "!"))

(setq program "!") ; partiamo da 255
(extend program (join (map last (trova-path 255 1)))) ; arriviamo a 1
(push "$" program -1) ; stampiamo 1
(extend program (join (map last (trova-path 1 2))))   ; arriviamo a 2
(push "$" program -1) ; stampiamo 2
(extend program (join (map last (trova-path 2 3))))   ; arriviamo a 3
(push "$" program -1) ; stampiamo 3
(extend program (join (map last (trova-path 3 4))))   ; arriviamo a 4
(push "$" program -1) ; stampiamo 4
(extend program (join (map last (trova-path 4 5))))   ; arriviamo a 5
(push "$" program -1) ; stampiamo 5
;-> "!<!$<$>!<!$><<$>!<!$"

(run program)
;-> A =   1 - 00000001
;-> A =   2 - 00000010
;-> A =   3 - 00000011
;-> A =   4 - 00000100
;-> A =   5 - 00000101

Per finire rappresentiamo il grafo delle transizioni in un file SVG.
(Vedi "GraphWiz e newLISP" su "Note libere 31".)

Partendo dall'output di 'genera-transizioni':
(es. ((255 0 127 254 255) (127 128 63 254 247) (128 127 64 0 8) (64 191 32 128 4) ...)
Generiamo un file .dot per GraphWiz del tipo:

digraph G {
  255 -> 0 [label="!"];
  255 -> 127 [label=">"];
  255 -> 254 [label="<"];
  255 -> 255 [label="@"];
  ...
}

(define (genera-dot transizioni)
  (let ((operazioni '("!" ">" "<" "@"))
        (risultato '()))
    (push "digraph G {" risultato)
    (dolist (riga transizioni)
      (let ((nodo (riga 0)))
        (for (i 1 4)
          (let ((dest (riga i)))
            ; Evita archi ridondanti (nodo -> nodo) se non vuoi loop
            (push (string "  " nodo " -> " dest " [label=\"" (operazioni (- i 1)) "\"];") risultato -1)))))
    (push "}" risultato -1)
    (join risultato "\n")))

(setq graph (genera-dot transizioni))

Scriviamo il risultato in un file:

(setq f (open "grafo.dot" "w"))
(write f graph)
(close f)

Creiamo il grafo in formato SVG dal terminale con GraphWiz:
(ci vuole molto tempo...)

dot -Tsvg grafo.dot -o grafo.svg

Il file "grafo.svg" si trova nella cartella "data".

Oppure (layout più conciso):

fdp -Tsvg grafo.dot -o grafo-fdp.svg

Il file "grafo-fdp.svg" si trova nella cartella "data".


----------------
Balanced Ternary
----------------

Il "balanced ternary" (ternario bilanciato) è un sistema numerico posizionale non standard.
È un sistema in base 3, che, a differenza del sistema ternario standard, usa come cifre -1, 0 e 1 anziché 0, 1 e 2.
Le potenze di 3 usate per rappresentare il numero possono avere quindi coefficiente positivo, nullo o negativo.
In questo sistema ogni cifra rappresenta -1, 0 o +1, con potenze di 3:

  valore = cifra0*3^0 + cifra1*3^1 + cifra2*3^2 + ...

Esempio: 4 = 1*3^2 + (-1)*3^0 -> balance ternary: (1 0 -1)

Funzione che prende un intero decimale e lo converte nella notazione "balanced ternary" (-1,0,1).

(define (balanced-ternary num)
  (let (ris '())
    (while (!= num 0)
      (letn ((r (% num 3)))
        (cond
          ((= r 0) (push 0 ris) (setq num (/ num 3)))
          ((= r 1) (push 1 ris) (setq num (/ num 3)))
          ((= r 2) (push -1 ris) (setq num (+ (/ num 3) 1))))))
    ris))

Proviamo:

(balanced-ternary 42)
;-> (1 -1 -1 -1 0)

(map (fn(x) (list x (balanced-ternary x))) (sequence 1 12))
;-> ((1 (1)) (2 (1 -1)) (3 (1 0)) (4 (1 1)) (5 (1 -1 -1)) (6 (1 -1 0))
;->  (7 (1 -1 1)) (8 (1 0 -1)) (9 (1 0 0)) (10 (1 0 1)) (11 (1 1 -1))
;->  (12 (1 1 0)))

La lista risultante è ordinata dalla cifra meno significativa alla più significativa.

Funzione che un numero intero nella notazione "balanced ternary" (-1,0,1) e lo converte in intero decimale:

(define (from-balanced-ternary lst)
  (letn ((ris 0)
         (len (length lst)))
    (dotimes (i len)
      (setq ris (+ ris (* (lst i) (pow 3 (- len i 1))))))
    ris))

Proviamo:

(from-balanced-ternary '(1 -1 -1 -1 0))
;-> 42

(map (fn(x) (list x (from-balanced-ternary (balanced-ternary x))))
     (sequence 1 20))
;-> ((1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9) (10 10) (11 11)
;->  (12 12) (13 13) (14 14) (15 15) (16 16) (17 17) (18 18) (19 19) (20 20))

Sequenza OEIS A065363:
Sum of balanced ternary digits in n. Replace 3^k with 1 in balanced ternary expansion of n.
  0, 1, 0, 1, 2, -1, 0, 1, 0, 1, 2, 1, 2, 3, -2, -1, 0, -1, 0, 1, 0, 1, 2,
  -1, 0, 1, 0, 1, 2, 1, 2, 3, 0, 1, 2, 1, 2, 3, 2, 3, 4, -3, -2, -1, -2,
  -1, 0, -1, 0, 1, -2, -1, 0, -1, 0, 1, 0, 1, 2, -1, 0, 1, 0, 1, 2, 1, 2,
  3, -2, -1, 0, -1, 0, 1, 0, 1, 2, -1, 0, 1, 0, 1, 2, 1, 2, 3, 0, 1, 2, 1,
  2, 3, 2, 3, 4, -1, 0, 1, 0, 1, 2, 1, 2, 3, 0, 1, 2, 1, 2, ...

(define (seq num) (apply + (balanced-ternary num)))
(map seq (sequence 0 50))
;-> (0 1 0 1 2 -1 0 1 0 1 2 1 2 3 -2 -1 0 -1 0 1 0 1 2
;->  -1 0 1 0 1 2 1 2 3 0 1 2 1 2 3 2 3 4 -3 -2 -1 -2 
;->  -1 0 -1 0 1 -2)

Vediamo come vengono composti i numeri con questo sistema:

(define (show-balanced-ternary num)
  (letn ( (ris 0) (lst (balanced-ternary num)) (len (length lst)) )
  (println num " -> " lst)
    (dotimes (i len)
      (println (format "%+2d%s%d%s%d"
               (lst i) "*3^" (- len i 1) " = " (* (lst i) (pow 3 (- len i 1)))))
      (setq ris (+ ris (* (lst i) (pow 3 (- len i 1))))))
    ris))

Proviamo:

(show-balanced-ternary 49)
;-> 49 -> (1 -1 -1 1 1)
;-> +1*3^4 = 81
;-> -1*3^3 = -27
;-> -1*3^2 = -9
;-> +1*3^1 = 3
;-> +1*3^0 = 1
;-> 49

(show-balanced-ternary 11)
;-> 11 -> (1 1 -1)
;-> +1*3^2 = 9
;-> +1*3^1 = 3
;-> -1*3^0 = -1
;-> 11

-----------------------------------------
Liste originali di un prodotto cartesiano
-----------------------------------------

Il prodotto cartesiano di due liste A e B è la lista di tutte le coppie ordinate composte da un elemento di A e un elemento di B (l'ordine della lista non è rilevante).

Esempio, il prodotto cartesiano di {1,2,7,1} e {4,6} vale:

(define (cp lst1 lst2)
  (let (out '())
    (if (or (null? lst1) (null? lst2))
        '()
        (dolist (el1 lst1)
          (dolist (el2 lst2)
            (push (list el1 el2) out -1))))))

(cp '(1 2 7) '(4 6))
;-> ((1 4) (1 6) (2 4) (2 6) (7 4) (7 6))

Scrivere una funzione che prende una lista e determina se è un prodotto cartesiano.
In caso positivo deve restituire le liste originali, altrimenti nil.

In ogni lista L che rappresenta un prodotto cartesiano risulta:
- per ogni coppia di coppie (a,b),(c,d) in L i prodotti dei conteggi count(a,b) * count(c,d) e count(a,d) * count(c,b) sono uguali.
Chiaramente, questo è necessario affinché L sia un prodotto cartesiano.
Per la sufficienza, osserviamo che risulta count(a,b)/count(c,b) = count(a,d)/count(c,d) per ogni b,d, quindi la riga a è un multiplo della riga c. Poiché questo vale per ogni a,c, la matrice dei conteggi ha rango <= 1 e può essere scritta come prodotto esterno.

Quindi per verificare se una lista L è un prodotto cartesiano deve risultare:

  count(a,b) * count(c,d) = count(a,d) * count(c,b)

(define (check l1 l2)
  (let ( (a (l1 0)) (b (l1 1)) (c (l2 0)) (d (l2 1)) )
    (= (* (first (count (list l1) lst))             ;(a b)*
          (first (count (list l2) lst)))            ;(c d)
       (* (first (count (list (list a d)) lst))     ;(a d)*
          (first (count (list (list c b)) lst)))))) ;(c b)

Quando la lista data L rappresenta un prodotto cartesiano vogliamo restituire le due liste di partenza.
Le due liste di partenza sono costituite da:
a) prima lista = elementi presenti nelle prime posizioni di ogni coppia di L
a) seconda lista = elementi presenti nelle seconde posizioni di ogni coppia di L

Nota: questo è vero solo quando non eistono molteplicità, cioè le liste originali sono due 'set' (ognuna lista è costituita da elementi diversi)

(define (cartesian? lst)
  ;; Se la lista ha una sola coppia, è comunque un prodotto cartesiano:
  (if (= (length lst) 1)
      ;; Estrae i due elementi della coppia in due liste distinte
      (list (list (lst 0 0)) (list (lst 0 1)))
      ;else
      (let (stop nil)
        ;; Confronta ogni coppia di coppie (i, j)
        (for (i 0 (- (length lst) 2) 1 stop)
          (for (j (+ i 1) (- (length lst) 1) 1 stop)
            ;; Se una violazione della regola viene trovata, stop = true
            (if-not (check (lst i) (lst j)) (setq stop true))))
        ;; Se nessuna violazione, restituisce le due liste originali
        (if stop nil
            ;(list (map first lst) (map last lst))))))
            (list (unique (map first lst)) (unique (map last lst)))))))

Proviamo:

(cartesian? '((1 1)))
;-> ((1) (1))
(cartesian? '((3 1)))
;-> ((3) (1))

(cartesian? (cp '(1 2 7) '(4 6)))
;-> ((1 2 7) (4 6))
(cartesian? '((1 4) (1 6) (2 4) (7 4) (7 6) (2 6)))
;-> ((1 2 7) (4 6))
(cartesian? (cp '(1 2 3 4) '(1 2)))
;-> ((1 2 3 4) (1 2))

(cartesian? '((1 2) (2 2) (3 2) (1 3) (2 3)))
;-> nil

Quando esistono molteplicità, allora la funzione non genera correttamente le liste:

(cartesian? (cp '(1 2 3 4) '(1 2 1)))
;-> ((1 2 3 4) (1 2)) ; errore
(cartesian? (cp '(1 2 3 4) '(1 2 1 2)))
;-> ((1 2 3 4) (1 2)) ; errore

Questo perchè quando usiamo 'unique' per selezionare gli elementi delle liste risultanti eliminiamo le molteplicità.

Quindi la funzione 'cartesian?' verifica che la lista data sia esattamente un prodotto cartesiano con molteplicità coerenti (matrice rango 1).
In altre parole, se le liste di partenza sono due 'set' (cioè ognuna ha elementi tutti distinti), allora la funzione produce risultati corretti.
In definitiva, la funzione verifica sempre correttamente se la lista data è un prodotto cartesiano, ma non produce liste corrette nel caso in cui le liste originali hanno elementi multipli.

La distinzione tra "set" e "liste" è cruciale.
Se L1 e L2 possono contenere duplicati e il loro ordine è rilevante, la ricostruzione diventa più complessa e in molti casi impossibile da fare in modo univoco da un prodotto cartesiano.
Generalmente, il prodotto cartesiano è definito su insiemi (dove gli elementi sono unici e non ordinati).
Se si tratta di liste con duplicati, il comportamento è più simile a un prodotto vettoriale o tensoriale e richiede più informazioni.

Vedi anche "Prodotto cartesiano" su "Funzioni varie".


--------------------------------------
Palindromo più vicino divisibile per N
--------------------------------------

Dato un numero intero N diverso da 0, determinare il numero palindromo più vicino ad N che è divisibile per N.

(define (palindromo? num)
  (let ( (rev 0) (val num) )
    ; crea il numero invertito
    (until (zero? val)
      (setq rev (+ (* rev 10)
      (% val 10))) (setq val (/ val 10)))
    (= rev num)))

(define (find-pali num)
  (local (up down found out)
    (setq up num)
    (setq down num)
    (setq found nil)
    ; ricerca bidirezionale, partendo da 'num',
    ; del palindromo che è divisibile per 'num'
    (until found
      ;(print down { } up) (read-line)
            ; caso palindromo maggiore
      (cond ((and (!= up 0) (palindromo? up) (zero? (% up num)))
              (set 'out up 'found true))
            ; caso palindromo minore
            ((and (!= down 0) (palindromo? down) (zero? (% down num)))
              (set 'out down 'found true))
            ; incr e decr up e down
            (true (++ up) (-- down))))
    out))

Proviamo:

(find-pali 16)
;-> 272
(/ 272 16)
;-> 17

(find-pali 1)
;-> 1
(find-pali 42)
;-> 252
(find-pali 111)
;-> 111
(time (println (find-pali 1234)))
;-> 28382
;-> 53.36
(time (println (find-pali -1234)))
;-> -28382
;-> 53.391

Sembra veloce, ma:

(time (println (find-pali 12342)))
;-> 456654
;-> 942.439

(time (println (find-pali 12345)))
(CTRL-C per fermare)

Il metodo è molto inefficiente.
Molto meglio moltiplicare il numero dato per k=1,2,... fino a che k*n non risulta palindromo.

(define (find-pali2 num)
  (let ( (out 0) (num (* 1L num)) (found nil) (k 1) )
    (until found
      (when (palindromo? (* num k))
        (setq found true)
        (setq out (* num k)))
      (++ k))
    out))

Proviamo:

(find-pali2 16)
;-> 272L
(find-pali2 1)
;-> 1L
(find-pali2 42)
;-> 252L
(find-pali2 111)
;-> 111L
(time (println (find-pali2 1234)))
;-> 23382L
;-> 0
(time (println (find-pali2 -1234)))
;-> -23382L
;-> 0
(time (println (find-pali2 12342)))
;-> 456654L
;-> 0
(time (println (find-pali2 12345)))
;-> 536797635L
;-> 184.987
(time (println (find-pali2 123456)))
;-> 6180330816L
;-> 227.779
(time (println (find-pali2 1234567)))
;-> 85770307758L
;-> 343.711
(time (println (find-pali2 1234568)))
;-> 2957554557592L
;-> 13498.625
(time (println (find-pali2 123456789)))
;-> 535841353148535L
;-> 29525.506


-----------------------------
Andata e ritorno di una barca
-----------------------------

Una barca impiega t1 minuti per andare da A a B con la corrente a favore.
La stessa barca impiega t2 minuti per andare da B a A controcorrente.
Quanto impiegherebbe la barca ad andare e tornare senza corrente?

v = velocità barca
t1 = tempo di andata

c = velocità corrente
t2 = tempo di ritorno

v1 = velocità di andata = (v + c)
v2 = velocità di ritorno = (v - c)
S = distanza andata (AB) = distanza ritorno (BA)

S = v2*t1 = v1*t2
S = (v + c)*t1 = (v - c)*t2

v*t1 + c*t1 = v*t2 - c*t2
c*(t1 + t2) = v*(t2 - t1)
c = v*(t2 - t1)/(t1 + t2)     (1)

deltaT = tempo andata/ritorno senza corrente

v*deltaT = (v + c)*t1 = (v - c)*t2   (2)

Sostituendo la (1) nella (2):

t1*(v + v*(t2 - t1)/(ta + t2)) + t2*(v - v*(t2 - t1)/(t1 + t2)) = v*deltaT
v*t1 + (v*t1)*(t2-t1)/(t1+t2)) + v*t2 - (v*t2)*(t2-t1)/(t1+t2)) = v*deltaT
v*t1*(1 + (t2 - t1)/(t1 + t2)) + v*t2*(1 - (t2 - t1)/(t1 + t2)) = v*deltaT

deltaT = t1*(1 + (t2 - t1)/(t1 + t2)) + t2*(1 - (t2 - t1)/(t1 + t2))

(define (tempo t1 t2)
  (add (mul t1 (add 1 (div (sub t2 t1) (add t1 t2))))
       (mul t2 (sub 1 (div (sub t2 t1) (add t1 t2))))))

Esempio:
t1 = 3 minuti
t2 = 5 minuti

Tempo necessario di andata e ritorno senza corrente:

(tempo 3 5)
;-> 7.5 ; minuti

Nota: la barca impiega più tempo ad andare e tornare quando c'è la corrente.
Questo accade perchè t2 > t1, cioè la barca viaggia per più tempo controcorrente di quanto non faccia a favore di corrente. In altre parole, il tratto controcorrente dura di più e quindi il tempo per cui siamo sfavoriti è maggiore del tempo per cui siamo favoriti.


---------------------
Creazione di acronimi
---------------------

Data una stringa di parole separate da spazio " ", scrivere una funzione che genera uno o più acronimi per la stringa.
La funzione prende due parametri:
1) una stringa di parole
2) una stringa di parole che non devono essere considerate per l'acronimo 

(define (powerset lst)
"Generate all sublists of a list"
  (if (empty? lst)
      (list '())
      (let ((element (first lst))
            (p (powerset (rest lst))))
         (extend (map (fn (subset) (cons element subset)) p) p) )))

(define (acronimo str nowords)
  (local (words all out lower diff stringa res)
    (setq words (parse str))
    ; powerset di tutte le parole da escludere
    (setq all (powerset nowords))
    (setq out '())
    ; per ogni lista in powerset...
    (dolist (el all)
      ; genera l'acronimo
      (setq lower (map lower-case words))   ; lowercase 
      (setq diff (difference lower el))     ; remove nowords 
      (setq stringa (map title-case diff))  ; title-case
      (setq res (join (map first stringa))) ; current string
      ; inserisce la stringa e il relativo acronimo nella soluzione
      (push (list (join stringa " ") res) out -1)
    )
    ; elimina risultati uguali
    (unique out)))

Proviamo:

(setq nowords '("the" "and" "or" "by" "of"))

(acronimo "newLISP Programming language" nowords)
;-> (("Newlisp Programming Language" "NPL"))

(acronimo "Light Amplification by Stimulation of Emitted Radiation" nowords)
;-> (("Light Amplification Stimulation Emitted Radiation" "LASER")
;->  ("Light Amplification Stimulation Of Emitted Radiation" "LASOER")
;->  ("Light Amplification By Stimulation Emitted Radiation" "LABSER")
;->  ("Light Amplification By Stimulation Of Emitted Radiation" "LABSOER"))

(acronimo "The united states of america" nowords)
;-> (("United States America" "USA")
;->  ("United States Of America" "USOA")
;->  ("The United States America" "TUSA")
;->  ("The United States Of America" "TUSOA"))

(acronimo "Jordan Of the World" nowords)
;-> (("Jordan World" "JW")
;->  ("Jordan Of World" "JOW")
;->  ("Jordan The World" "JTW")
;->  ("Jordan Of The World" "JOTW"))


---------------------
La costante dei primi
---------------------

Esiste un numero decimale con infinite cifre che rappresenta tutti i numeri primi.
Questo numero vale:

  0.4146825098511116602...

Come viene calcolato?

1) Prendiamo la lista dei primi fino a un dato N
2) Formare un numero binario (come stringa) composto da "0" e "1" in cui:
   a) se l'indice della stringa è primo scrivere "1"
   b) se l'indice della stringa non è primo scrivere "0"
   (l'indice della stringa parte da 1)
3) Aggiungere "0." al numero binario/stringa
4) Convertire il numero binario/stringa in decimale

Esempio:
N = 20
primi = (2 3 5 7 11 13 17 19)
stringa binaria = 0 1 1 0 1 0 1 0 0 0 1 0 1 0 0 0 1 0 1
                    2 3   5   7   0  11  13      17  19
stringa binaria = "0110101000101000101"
Aggiungo "0."   = "0.0110101000101000101"
Conversione stringa binaria in decimale = 0.4146823883056641

Funzione che converte da stringa binaria a numero decimale:

(define (bin-frac binary)
  (setq len (length binary))
  (setq punto (or (find "." binary) len))
  (setq int-dec 0)
  (setq frac-dec 0)
  (setq due 1)
  ; conversione parte intera del binario in decimale
  (for (i (- punto 1) 0 -1)
    (setq int-dec (+ int-dec (* (int (binary i)) due)))
    (setq due (* due 2))
  )
  ; conversione parte frazionaria del binario in decimale
  (setq due 2)
  (for (i (+ punto 1) (- len 1))
    (setq frac-dec (add frac-dec (div (int (binary i)) due)))
    (setq due (* due 2))
  )
  (add int-dec frac-dec))

(bin-frac "0.0110")
;-> 0.375
(bin-frac "0.01101010101")
;-> 0.41650390625

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

Funzione che costruisce la stringa binaria dalla lista dei numeri primi:

(define (build limite)
  (local (primi s)
    (setq primi (primes-to limite))
    (setq s (dup "0" (+ (apply max primi) 1)))
    (dolist (el primi) (setf (s el) "1"))
    (push "." s 1)))

Proviamo:

(build 20)
;-> "0.0110101000101000101"
(bin-frac (build 20) 25)
;-> 0.4146823883056641
(bin-frac (build 25) 25)
;-> 0.4146825075149536
(bin-frac (build 50) 50)
;-> 0.4146825098511116
(bin-frac (build 60) 60)
;-> 0.4146825098511117
(bin-frac (build 65) 65)
;-> 0.4146825098511117  ; stesso risultato
(bin-frac (build 70) 70)
;-> -1.#IND

Otteniamo lo stesso risultato perchè i float IEEE 754 hanno al massimo 16 cifre.

Vediamo un metodo alternativo.
Calcoliamo l'espansione in potenze di 2 di un numero binario.

Funzione che converte da stringa binaria a espansione binaria:

(define (bin-frac-expansion binary)
  (local (len punto int-dec frazioni due)
  (setq len (length binary))
  (setq punto (or (find "." binary) len))
  (setq int-dec 0)
  (setq frazioni '())
  (setq due 1)
  ; parte intera
  (for (i (- punto 1) 0 -1)
    (setq int-dec (+ int-dec (* (int (binary i)) due)))
    (setq due (* due 2)))
  ; parte frazionaria (corretto j -> j+1)
  (for (j 0 (- len punto 2))
    (if (= (binary (+ punto 1 j)) "1")
        (push (string "1/2^" (+ j 1)) frazioni -1)))
  ; risultato
  (if (= (length frazioni) 0)
      (string int-dec)
      (string int-dec " + " (join frazioni " + ")))))

(bin-frac-expansion "0.011101")
;-> "0 + 1/2^2 + 1/2^3 + 1/2^4 + 1/2^6"
(bin-frac-expansion "0.1010101")
;-> "0 + 1/2^1 + 1/2^3 + 1/2^5 + 1/2^7"
(bin-frac-expansion "0.0110101000101000101")
;-> "0 + 1/2^2 + 1/2^3 + 1/2^5 + 1/2^7 + 1/2^11 + 1/2^13 + 1/2^17 + 1/2^19"

Calcoliamo la frazione generata dall'espansione binaria e poi calcoliamo la frazione con una funzione che divide due numeri con precisione predefinita.

(define (** num power)
"Calculate the integer power of an integer"
  (if (zero? power) 1L
      (let (out 1L)
        (dotimes (i power)
          (setq out (* out num))))))

Funzione che valuta una espansione binaria:

(define (eval-binary-expansion s)
  (local (termini num den k n d g)
    (setq termini (parse s "+"))
    (setq num 0L)
    (setq den 1L)
    (dolist (trm termini)
      (setq trm (trim trm))
      (if (find "^" trm)
          ; termine frazionario "1/2^k"
          (begin
            (setq k (int (last (parse trm "^"))))
            (setq n 1L)
            (setq d (** 2L k)))
          ; termine intero
          (begin
            (setq n (bigint (int trm)))
            (setq d 1L)))
      ; somma frazioni: num/den + n/d
      (setq num (+ (* num d) (* n den)))
      (setq den (* d den)))
    ; semplifica
    (setq g (gcd num den))
    (list (/ num g) (/ den g))))

Proviamo:

(bin-frac-expansion "0.011101")
;-> "0 + 1/2^2 + 1/2^3 + 1/2^4 + 1/2^6"
(eval-binary-expansion "0 + 1/2^2 + 1/2^3 + 1/2^4 + 1/2^6")
;-> (29L 64L)

(bin-frac-expansion "0.1010101")
;-> "0 + 1/2^1 + 1/2^3 + 1/2^5 + 1/2^7"
(eval-binary-expansion "0 + 1/2^1 + 1/2^3 + 1/2^5 + 1/2^7")
;-> (85L 128L)

(bin-frac-expansion "0.0110101000101000101")
;-> "0 + 1/2^2 + 1/2^3 + 1/2^5 + 1/2^7 + 1/2^11 + 1/2^13 + 1/2^17 + 1/2^19"
(eval-binary-expansion "0 + 1/2^2 + 1/2^3 + 1/2^5 + 1/2^7 + 1/2^11 + 1/2^13 + 1/2^17 + 1/2^19")
;-> (217413L 524288L)
(div 217413L 524288L)
;-> 0.4146823883056641

(define (int-list num)
"Convert an integer to a list of digits"
  (let (out '())
    (while (!= num 0)
      (push (% num 10) out)
      (setq num (/ num 10))) out))

(define (bigint-int num)
"Convert a big-integer to int (if possible)"
  (let ( (MAX-INT 9223372036854775807) (MIN-INT -9223372036854775808) )
    (if (or (< num MIN-INT) (> num MAX-INT))
        num
        (+ 0 num))))

(map bigint-int (int-list 1234))

(define (list-int lst)
"Convert a list of digits to integer"
  (let (num 0)
    (dolist (el lst) (setq num (+ el (* num 10))))))

Funzione che trova il dividendo minore tra num1 e num2:

(define (trova-dividendo num1 num2)
  (cond ((<= num1 num2) num1)
    (true
      (let ( (dividendo 0L) (k 0) (L1 (int-list num1)) )
        (while (< dividendo num2)
          (setq dividendo (+ (L1 k) (* dividendo 10)))
          (++ k))
        dividendo))))

Funzione che effettua la divisione tra due numeri interi con decimali predefiniti:

(define (divisione num1 num2 decimali)
  (local (intero out resto virgola dividendo cifra)
    ; calcolo parte intera
    (setq intero "")
    (cond ((= num1 num2) "1")
          ((zero? (% num1 num2)) (string (/ num1 num2)))
          (true
          (when (> num1 num2)
                (setq intero (string (/ num1 num2)))
                (setq num1 (% num1 num2)))
          ; calcolo parte decimale
          (setq out '())
          (setq resto nil)
          (setq virgola nil)
          (setq decimali (or decimali 16))
          ; Ciclo fino a che non abbiamo resto 0...
          (until (zero? resto)
            ; Calcolo del dividendo corrente
            (setq dividendo (trova-dividendo num1 num2))
            ; Calcola la cifra corrente del risultato
            (setq cifra (/ dividendo num2))
            (push cifra out -1)
            ; Calcolo del resto
            (setq resto (- dividendo (* cifra num2)))
            ; Calcolo nuovo numero
            (cond
              ; Se resto = 0,
              ; allora fine della divisione (se abbiamo messo la virgola)
              ((and (zero? resto) virgola) nil)
              (true
                ; Creazione nuovo numero
                (setq num1 (int (string resto
                                (slice (string num1) (length dividendo))) 0 10))
                ; Se il nuovo numero < divisore, allora mettiamo la virgola e
                ; aggiungiamo uno zero al nuovo numero
                (when (< num1 num2)
                  (setq num1 (* num1 10))
                  ; mettiamo la virgola se non l'abbiamo già messa prima
                  (if (not virgola) (begin
                      (setq virgola true) (push "." out -1)))))
            )
            ;(print (join (map string out))) (read-line)
            ; Se abbiamo calcolato il numero di decimali predefinito,
            ; allora fermiamo la divisione (ponendo resto = 0)
            (when virgola
              (if (> (length (slice out (find "." out))) decimali)
                  (setq resto 0)))
          )
          (when (!= intero "")
            (pop out) (push intero out))
          (join (map string out))))))

Proviamo:

(setq s (build 60))
;-> "0.01101010001010001010001000001010000010001010001000001000001"

(setq e (bin-frac-expansion s))
;-> "0 + 1/2^2 + 1/2^3 + 1/2^5 + 1/2^7 + 1/2^11 + 1/2^13 + 1/2^17 + 1/2^19 +
;->  1/2^23 + 1/2^29 + 1/2^31 + 1/2^37 + 1/2^41 + 1/2^43 + 1/2^47 + 1/2^53 +
;->  1/2^59"

(setq f (eval-binary-expansion e))
;-> (239048191595843649L 576460752303423488L)

(div (f 0) (f 1))
;-> 0.4146825098511117

(divisione (f 0) (f 1))
;-> "0L.4146825098511116"

(divisione (f 0) (f 1) 25)
;-> "0L.4146825098511116598071213"

Purtroppo la funzione 'divisione' non accetta numeri superiori a Int64.

(setq s (build 100))
(setq e (bin-frac-expansion s))
(setq f (eval-binary-expansion e))
(div (f 0) (f 1))
;-> 0.4146823883056641

(divisione (f 0) (f 1) 50)
;-> ERR: number out of range in function <
;-> called from user function (divisione (f 0) (f 1))


------------------------
Numeri con pi greco ed e
------------------------

Utilizzando la costante 'pi greco' e la costante 'e' scrive 12 espressioni che restituiscono i nuimeri da 0 a 10 e il numero 42.
Non è possibile usare cifre (0..9) o stringhe ("...") nelle espressioni.

(define (expr12)
  (let ( (e 2.7182818284590451) (pi 3.1415926535897931) )
    (print (sub e e) { })
    (print (div e e) { })
    (print (floor e) { })
    (print (ceil e) { })
    (print (ceil pi) { })
    (print (ceil (sub (mul e e) e)) { })
    (print (ceil (add e e)) { })
    (print (floor (mul e e)) { })
    (print (ceil (mul e e)) { })
    (print (ceil (mul pi e)) { })
    (print (ceil (mul pi pi)) { })
    (print (ceil (mul (pow e e) e)) "\n") '>))

(expr12)
;-> 0 1 2 3 4 5 6 7 8 9 10 42

Verrsione code-golf (244 caratteri, senza \r\n):

(define (f)
(let((e 2.718)(pi 3.14))
(println(sub e e){ }(div e e){ }(floor e){ }
(ceil e){ }(ceil pi){ }(ceil(sub(mul e e)e)){ }(ceil(add e e)){ }
(floor(mul e e)){ }(ceil(mul e e)){ }(ceil(mul pi e)){ }
(ceil(mul pi pi)){ }(ceil(mul(pow e e)e)))))

(f)
;-> 0 1 2 3 4 5 6 7 8 9 10 42

============================================================================

