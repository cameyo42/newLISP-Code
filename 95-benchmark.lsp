;===================================================================
; newLISP benchmark
; ver 0.01
; 22 giugno 2025
;
; some code (biginteger, float, ...) from "qa-specific-tests"
; (newLISP source distribution)
;===================================================================
; Configuration
; -------------
; CPU: Intel Core i7-9700
; Motherboard: ASUS TUF Z390-PLUS GAMING (WI-FI)
; Chipset: Intel Z390 (Cannon Lake-H)
; RAM: 32 GB DDR4 (1333 MHz)
; Graphic Card: NVIDIA GeForce GTX 750 (2 GB GDDR5 SDRAM 128 bit)
; Hard Disk: HDD WDC-WD10EZEX (2 x 1 TB)
; S.O.: Microsoft Windows 10 Professional (x64) Build 19045.5854 (22H2)
;
; Result
; ------
; newLISP benchmark
; (now) --> (2025 6 23 15 24 32 676266 174 1 120 2)
; Permutazioni di una lista con 10 elementi: 3031 ms
; Partizioni di una lista con di 20 elementi: 7719 ms
; Collatz (da 1 a 300000): 5141 ms
; Numeri primi (prime?) fino a 1e6: 922 ms
; Numeri primi (primes-to) fino a 1e7: 1890 ms
; Divisori dei numeri (1..1e6): 10235 ms
; Hash-map con 1 milione di elementi: 1718 ms
; Lista associativa con 20000 elementi: 7594 ms
; Biginteger test: 2140 ms
; Biginteger bench (10 milioni di operazioni): 5656 ms
; Float test:
; Subnormals (0 4.940656458e-324): (0 4.940656458412465e-324)
; Machine epsilon (1.110223025e-16): 1.110223024625157e-016
;   Bit patterns OK
;   Floating point tests SUCCESSFUL
;   time: 0 ms
; Float bench (5000000 operazioni): 2562 ms
; End of benchmark.
;===================================================================
(println "newLISP benchmark")
(println "(now) --> " (now))
(set-locale "C")
;=====================
; PERMUTATIONS
;=====================
(define (perm lst)
"Generate all permutations without repeating from a list of items"
  (local (i indici out)
    (setq indici (dup 0 (length lst)))
    (setq i 0)
    ; aggiungiamo la lista iniziale alla soluzione
    (setq out (list lst))
    (while (< i (length lst))
      (if (< (indici i) i)
          (begin
            (if (zero? (% i 2))
              (swap (lst 0) (lst i))
              (swap (lst (indici i)) (lst i)))
            ;(println lst);
            (push lst out -1)
            (++ (indici i))
            (setq i 0))
          (begin
            (setf (indici i) 0)
            (++ i)
          )))
    out))
; Test
(println "Permutazioni di una lista con 10 elementi: "
         (int (time (perm (sequence 0 9)))) " ms")
;=====================
; PARTITIONS
;=====================
(define (partition lst)
"Generate all the partitions of a list"
  (if (= (length lst) 1) (list lst)
    (local (out len max-tagli taglio fmt)
      (setq out '())
      (setq len (length lst))
      ; numero massimo di tagli
      (setq max-tagli (- len 1))
      ; formattazione con 0 davanti
      (setq fmt (string "%0" max-tagli "s"))
      (for (i 0 (- (pow 2 max-tagli) 1))
        ; taglio corrente
        (setq taglio (format fmt (bits i)))
        ; taglia la lista con taglio corrente
        ; e la inserisce nella lista soluzione
        (push (partition-aux lst taglio) out -1))
    out)))
; Auxiliary function for (partition lst)
(define (partition-aux lst binary)
  (local (out tmp)
    (setq out '())
    (setq tmp '())
    (for (i 0 (- (length binary) 1))
      (cond ((= (binary i) "1") ; taglio
              (push (lst i) tmp -1)
              (push tmp out -1)
              (setq tmp '()))
            (true  ; nessun taglio
              (push (lst i) tmp -1))))
    ; inserisce lista finale
    (push (lst -1) tmp -1)
    (push tmp out -1)
    out))
; Test
(println "Partizioni di una lista con di 20 elementi: "
         (int (time (partition (sequence 0 20)))) " ms")
;=====================
; COLLATZ
;=====================
(define (collatz n)
"Generate the collatz sequence of a positive integer number"
  (let (out (list n))
    (until (= n 1)
      (if (even? n)
          (setq n (/ n 2))
          (setq n (+ (* 3 n) 1))
      )
      (push n out -1)
    )
    out))
; Test
(println "Collatz (da 1 a 300000): "
         (int (time (map collatz (sequence 1 3e5)))) " ms")
;=====================
; PRIME NUMBERS (prime?)
;=====================
(define (prime? num)
"Check if a number is prime"
   (if (< num 2) nil
       (= 1 (length (factor num)))))
(println "Numeri primi (prime?) fino a 1e6: "
         (int (time (filter prime? (sequence 2 1e6)))) " ms")
;=====================
; PRIME NUMBERS (primes-to)
;=====================
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
; Test
(println "Numeri primi (primes-to) fino a 1e7: "
         (int (time (primes-to 1e7))) " ms")
;=====================
; DIVISORS
;=====================
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
(define (factor-group num)
"Factorize an integer number"
  (if (= num 1) '((1 1))
    (letn ( (fattori (factor num))
            (unici (unique fattori)) )
      (transpose (list unici (count unici fattori))))))
; Test
(println "Divisori dei numeri (1..1e6): "
         (int (time (map divisors (sequence 1 1e6)))) " ms")
;=====================
; HASH-MAP
;=====================
(define (hash-map items)
  (new Tree 'hash)
  (let ( (t 0) (as '()) (start 0) (elapsed 0) )
    ; Insert all key-value
    (for (i 1 items) (hash i (rand items)))
    ; Query all keys
    (for (q 1 items) (if (hash q) (++ t)))
    ; Copy hash-map elements on association list
    (setq as (hash))
    ; Start timer for calculate 'delete' time:
    ;(setq start (time-of-day))
    ; Delete hash-map from memory
    (delete 'hash)))
    ;(setq elapsed (- (time-of-day) start))
    ;(println "Delete hash: " elapsed)))
; Test
(println "Hash-map con 1 milione di elementi: "
         (int (time (hash-map 1e6))) " ms")
;=====================
; ASSOCIATION LIST
;=====================
(define (as-list items)
  (let ( (as '()) (t1 0) (t2 0) )
    ; Insert pairs
    (setq as (map list (randomize (sequence 1 items))
                       (randomize (sequence 1 items))))
    ; Query all keys
    (for (q 1 items)
      (if (lookup q as) (++ t1))
      (if (assoc q as) (++ t2))
      (if (!= t1 t2) (println "Error:" (list t1 t2)) true))
    ; Modify all values
    (for (m 1 items) (setf (lookup m as) (+ $it 10)))))
; Test
(println "Lista associativa con 20000 elementi: "
         (int (time (as-list 2e4))) " ms")
;=====================
; BIG INTEGER TEST
;=====================
; test biginteger operators
(define (biginteger)
  (unless bigint
      (println "big integers not enabled in this version")
      (exit))
  ;(println ">>>>> Testing big integer arithmetic ... ")
  (set-locale "C")
  ;; check embedded bigint 0's
  (set 'nums '(
  ; aligned
  12345678901000000000L
  123456789010000000001234567890L
  123456789010000000000000000001234567890L
  123456789010000000000000000000L
  123000000000000000000000000000L
  123000000000000000001L
  ; not aligned
  123456789010000000000L
  1234567890100000000001234567890L
  1234567890100000000000000000000L
  1230000000000000000000000000000L
  1230000000000000000001L
  1234578901000000000L
  12345789010000000001234567890L
  12345789010000000000000000000L
  123000000000000000000000000L
  123000000000000001L
  ))
  (dolist (num nums)
      (unless (= (/ num 1) num)
          (println num)
          (println ">>>>> ERROR in big integer zeros")
          (exit))
  )
  ; some special cases
  (unless (and
      (= (/ 1234567890123456789012345678901234567890 12345678901234567890) 100000000000000000001L)
      (= (/ 1234567890L 12345L) 100005L)
      (= (/ 1234567891L 1234567890L) 1L)
      (= (/ 1234567890L 1234567890L) 1L)
      (= (/ 888888888888888888888888888888888888888888888888888888888888888888888888
            888888888888888888888888888888888888888888888888888888888888889) 999999999L)
      (= (/ 888888888888888888888888888888888888888888888888888888888888888888888888
            888888888888888888888888888888888888888888888888888888888888888) 1000000000L)
      (= (/ 888888888888888888888888888888888888888888888888888888888888888888888888
            888888888888888888888888888888888888888888888888888888888888887) 1000000000L)
      (= (/ 11111111111111111L 11111111111111111L) 1L) ; problems with gcc optimizations on Linux
      (= (/ 22222222222222222L 22222222222222222L) 1L)
      (= (/ 44444444444444444L 44444444444444444L) 1L)
      (= (/ 88888888888888888L 88888888888888888L) 1L)
      (= (/ 99999999999999999L 99999999999999999L) 1L)  )
          (println ">>>>> ERROR in special cases")
              (exit)
  )
  (seed 5212011)
  (if (> (length (main-args)) 2)
      (set 'N (int (main-args -1)))
      (set 'N 100000))
  (if eval-string-js (set 'N 1000)) ; for JavaScript compiled newLISP
  (dotimes (i N)
      (set 'f (pow (random 10 100) (+ 15 (rand 50))))
      (set 'f1 (float (bigint f)))
      ;(unless eval-string-js
      ;    (when (= i (* (/ i 1000) 1000)) (print ".")))
      ;(println "=>" (sub (abs (div f f1)) 1.0))
      (unless (<= (abs (sub (abs (div f f1)) 1.0)) 0.000000001)
        (println f "   " f1 " " (abs (sub (abs (div f f1)) 1)))
        (println ">>>>> ERROR in big integer/float conversion")
        (exit))
  )
  ;(println)
  (define (get-bignum n , num)
      (set 'num (amb "-" ""))
      (if (zero? n) (++ n))
      (dotimes (i n)
          (extend num (string (+ (rand 1000) 1))))
      (extend num (dup (string (rand 10)) (rand 10)))
      (extend num "L")
      (bigint num))
  (dotimes (i N)
      (setq x (get-bignum (rand 30)))
      (setq y (get-bignum (rand 30)))
      ;(println "x=" x " y=" y)
      ;(unless eval-string-js
      ;    (when (= i (* (/ i 1000) 1000)) (print ".")))
      (unless (= (zero? x) (= x 0))
          (println ">>>>> ERROR in zero? for x = " x)
          (exit))
      (unless (and
          (= (/ x x) 1L)
          (= (/ y y) 1L)
          )
              (println ">>>>> ERROR in x/x y/y " x " " y)
              (exit))
      (setq x+y (+ x y))
      (setq x-y (- x y))
      (setq x*y (* x y))
      (setq x/y (/ x y))
      (set 'xx x)
      (unless (= (++ xx y) x+y)
          (println ">>>>> ERROR in ++ with " x " " y)
          (exit))
      (set 'xx x)
      (unless (= (-- xx y) x-y)
          (println ">>>>> ERROR in -- with " x " " y)
          (exit))
      (unless (and (= (- x+y y) x) (= (- x+y x) y) (= (+ x-y y) x) )
              (println ">>>>> ERROR in +, - with " x " " y)
              (exit))
      (unless (and (= (/ x*y x) y) (= (/ x*y y) x))
          (println ">>>>> ERROR in * / with:\n" x "\n" y "\nat: " i)
          (println "x*y / x ->" (/ x*y x) )
          (println "x*y / y ->" (/ x*y y) )
          (exit))
      (unless (= (% x y) (- x (* x/y y)))
              (println ">>>>> ERROR in %, * , / operation with " x " " y)
              (exit))
      (when (> (abs x/y) 0)
          ;(println x " " y " remainder " (- (abs x) (* (abs x/y) (abs y))))
          (unless (< (- (abs x) (* (abs x/y) (abs y))) (abs y))
              (println ">>>>> ERROR in abs, -, *, - with " x " " y)
              (exit))
      )
  )
  ;(println)
  ; gcd for bigint
  ; from http://bit-player.org/2013/the-keys-to-the-keydom
  ; and: http://en.wikipedia.org/wiki/Euclidean_algorithm
  (set 'x  123784517654557044204572030015647643260197571566202790882488143432336664289530131607571273603815008562006802500078945576463726847087638846268210782306492856139635102768022084368721012277184508607370805021154629821395702654988748083875440199841915223400907734192655713097895866822706517949507993107455319103401)
  (set 'y  139752806258570179719657334941265463008205415047073349942370461270597321020717639292879992151626413610247750429267916230424010955054750502833517070395986289724237112410816000558148623785411568845517146303421384063525091824898318226175234193815950597041627518140906384889218054867887058429444934835873139133193)
  (define (gcd-big a b)
    (if (= b 0) a (gcd-big b (% a b))))
  (define (gcd-big a b , t)
    (until (= b 0)
      (set 't b)
      (set 'b (% a b))
      (set 'a t)
    ))
  (set 'f 10704679319376067064256301459487150226969621912489596482628509800922080318199635726117009340189103336170841315900354200725312700639146605265442630619090531)
  (unless (= (gcd x y) f)
      (println ">>>>> ERROR in big integer gcd")
      (exit))
  (unless (= (length 1234567890123456789012345) 25)
      (println ">>>>> ERROR in big integer length")
      (exit))
  (dotimes (i 1000)
      (unless (= (gcd i (- 1000 i)) (gcd (bigint i) (- 1000 i)))
          (println ">>>>> ERROR in gcd to bigint gcd comparison")))
  ;(println ">>> big int gcd benchmark " (time (gcd x y) 1000) " micro secs")
  ;(println ">>>>> abs bigint float gcd length zero? + - * / % ++ -- big ints tested SUCCESSFUL")
)
; Test
(let (res (int (time (biginteger))))
  (println "Biginteger test: " res " ms"))
;=====================
; BIG INTEGER BENCH
;=====================
; test biginteger velocity
(define (biginteger-bench iter)
  (setq x1 12345678901234567890L)
  (setq y1 1234567890L)
  (setq x2 1234567890123456789012345678901234567890L)
  (setq y2 12345678901234567890L)
  (setq x3 123456789012345678901234567890123456789012345678901234567890L)
  (setq y3 123456789012345678901234567890L)
  (setq x4 12345678901234567890123456789012345678901234567890123456789012345678901234567890L)
  (setq y4 1234567890123456789012345678901234567890L)
  (setq x5 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890L)
  (setq y5 12345678901234567890123456789012345678901234567890L)
  (setq x6 123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890L)
  (setq x7 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890L)
  (setq x8 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890L)
  (setq x9 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890L)
  (for (i 1 iter)
    (setq res (+ x1 y1))
    (setq res (+ x2 y2))
    (setq res (+ x3 y3))
    (setq res (+ x4 y4))
    (setq res (+ x5 y5))
    (setq res (- x1 y1))
    (setq res (- x2 y2))
    (setq res (- x3 y3))
    (setq res (- x4 y4))
    (setq res (- x5 y5))
    (setq res (* x1 y1))
    (setq res (* x2 y2))
    (setq res (* x3 y3))
    (setq res (* x4 y4))
    (setq res (* x5 y5))
    (setq res (/ x1 y1))
    (setq res (/ x2 y2))
    (setq res (/ x3 y3))
    (setq res (/ x4 y4))
    (setq res (/ x5 y5))
    (setq res (+ x5 x5))
    (setq res (- x5 x5))
    (setq res (* x5 x5))
    (setq res (/ x5 x5))
    (setq res (* x1 x1))
    (setq res (* x2 x2))
    (setq res (* x3 x3))
    (setq res (* x4 x4))
    (setq res (* x5 x5))
    (setq res (* x6 x6))
    (setq res (* x7 x7))
    (setq res (* x8 x8))
    (setq res (* x9 x9))))
; Test (33 x 303030 = 9999990)
(println "Biginteger bench (10 milioni di operazioni): "
          (int (time (biginteger-bench 303030))) " ms")
;=====================
; FLOAT TEST
;=====================
; test float operators
(define (floating)
  ; Test IEE compliance of some FP operations and handling of 'inf' and 'NaN'
  ; numbers. In all versions of newLISP (32Bit and 64Bit) floating point
  ; numbers are represented as IEEE 754 64-bit: Double (binary64) numbers.
  ;(println)
  ;(println "Testing floating point performance")
  (set-locale "C")
  (set 'aNan (sqrt -1))
  (set 'aInf (div 1.0 0))
  (set 'aNegInf (div -1 0))
  ; operation on NaN result in NaN
  (set 'tests '(
    "operation on NaN result in NaN"
    (NaN? (mul 1.0 aNan))
    (NaN? (div 1.0 aNan))
    (NaN? (add 1.0 aNan))
    (NaN? (sub 1.0 aNan))
    (NaN? (sin aNan))
    (NaN? (cos aNan))
    (NaN? (tan aNan))
    (NaN? (atan aNan))
    "comparison with NaN is always nil"
    (not (< 1.0 aNan))
    (not (> 1.0 aNan))
    (not (>= 1.0 aNan))
    (not (<= 1.0 aNan))
    (not (= aNan aNan))
    "NaN is not equal to itself"
    (not (= aNan aNan))
    "integer operations assume NaN as 0"
    (= (- 1 aNan) 1)
    (= (+ 1 aNan) 1)
    (= (* 1 aNan) 0)
    (not (catch (/ 1 aNan) 'error))
    (= (>> aNan) 0)
    (= (<< aNan) 0)
    "integer operations assume inf as max-int"
    (= (* 1 aInf) 9223372036854775807)
    (= (- aInf 1) 9223372036854775806)
    (= (+ aInf 1) -9223372036854775808) ; wrap around
    "FP division by inf results in 0"
    (= (/ 1 aInf) 0)
    (= (div 1 aInf) 0)
    "inf specials"
    (= aInf aInf)
    (NaN? (sub aInf aInf))
    "retain sign of -0.0"
    (= (set 'tiny (div -1 aInf)) -0.0)
    (= (sqrt tiny) -0.0)
      (= (div -1 (div 1.0 0)) -0.0)
    "inf is signed too"
    (= aNegInf (div -1 0))
    (!= aNegInf (div 1 0))
      (= (int aNegInf) -9223372036854775808)
    "mod with 0 divisor is NaN"
    (NaN? (mod 10 0))
    "% with 0 divisor throws error"
    (not (catch (% 10 0) 'error))
    )
  )
  (dolist (t tests)
    (if (string? t)
      ;(println (format "\n%-47s\n%s" t (dup "-" 47)))
      (let (result (eval t))
        ;(println (format "%40s => %s" (string t) (string result)))
        (push result result-list))
    )
  )
  ;(println)
  (set 'result '())
  (set 'u 1.0)
  (while (> u 0.0) (set 'u (mul u 0.5)) (push u result))
  ;(println "Support of subnormals: " (0 2 result) " => (0 4.940656458e-324)")
  (println "Subnormals (0 4.940656458e-324): " (0 2 result))
  (set 'epsilon 1.0)
  (while (!= 1.0 (add 1.0 epsilon))
      (set 'epsilon (mul epsilon 0.5))
  )
  (println "Machine epsilon (1.110223025e-16): " epsilon)
  ;(println)
  (if
      (and
          ;(= 2.2250738585072007e-308 2.2250738585072011e-308) ; true on OSX nil on FreeBSD and Win232
          ;(= 2.2250738585072011e-308 2.2250738585072012e-308) ; true on FreeBSD and Win32 nil on OSX

          ;(= (bits (first (unpack "Lu" (pack "lf" 2.2250738585072007e-308)))) ; true on FreeBSD and Win32 nil OSX
          ;"1111111111111111111111111111111111111111111111111110")
          ;(= (bits (first (unpack "Lu" (pack "lf" 2.2250738585072007e-308)))) ; true on OSX nil on FreeBSD and Win32
          ;"1111111111111111111111111111111111111111111111111111")

          ;(= (bits (first (unpack "Lu" (pack "lf" 2.2250738585072011e-308)))) ; true on OSX and Win32 nil on FreeBSD
          ;"1111111111111111111111111111111111111111111111111111") ; 52 bits
          ;(= (bits (first (unpack "Lu" (pack "lf" 2.2250738585072011e-308)))) ; true on FreeBS nil on OSX and Win32
          ;"10000000000000000000000000000000000000000000000000000") ; 53 bits

          ; work on FreeBSD and OSX but not on Win32 XP
          ;(= (bits 2.2250738585072007e-308) "0")
          ;(= (bits (first (unpack "Lu" (pack "lf" 2.2250738585072012e-308))))
          ;"10000000000000000000000000000000000000000000000000000")  ; 53 bits

          ; works on FreeBSD, OSX and Win32
          (= (bits (first (unpack "Lu" (pack "lf" (sqrt -1)))))
          "1111111111111000000000000000000000000000000000000000000000000000") ; 64 bits
      )
      (println "  Bit patterns OK")
      (println "  Problems in bit patterns")
  )
  ;(println)
  (if-not (apply and result-list)
    (println "  PROBLEM in floating point tests")
    (println "  Floating point tests SUCCESSFUL")
  )
)
; Test
(println "Float test: ")
(let (res (int (time (floating))))
  (println "  time: " res " ms"))
;=====================
; FLOAT BENCH
;=====================
; test float velocity
(define (floating-bench iter)
  (local (rnd a b c)
    (setq rnd (randomize (append (random 0 1e6 100) (random 0 -1e6 100))))
    (setq a (slice rnd 0 50))
    (setq b (slice rnd 50))
    (for (i 1 iter)
      (dolist (x a)
        (dolist (y b)
          (setq c (add x y))
          (setq c (sub x y))
          (setq c (mul x y))
          (setq c (div x y)))))))
; Test (2500 x 2000 = 5000000)
(println "Float bench (5000000 operazioni): "
         (int (time (floating-bench 2000))) " ms")
;=====================
; END
;=====================
(define (end) (println "End of benchmark.") '>)
(end)
