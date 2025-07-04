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
(setq html (matrice2html "Matrice di Test" matrix))
; salva su file
(write-file "matrice.html" html)
; visualizza il file html sul browser predefinito
(exec "matrice.html")

============================================================================

