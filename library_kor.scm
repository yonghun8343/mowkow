(정의 (머리 x) (머 x))
;(정의 머 머리)
(정의 (꼬리 x) (꼬 x))
;(정의 꼬 꼬리)
(정의 (머머 x) (머 (머 x))) 
(정의 (꼬머 x) (머 (꼬 x)))

(정의 (그대로 x) 
  x)
(정의 (아톰? x) 
  (그리고 (부정 (짝? x)) 
          (부정 (공? x))))
(정의 (리스트? x)
  (또는 (짝? x)
        (공? x)))

(정의 (절댓값 x) 
  (만약 (< x 0) 
    (- 0 x) 
    x))

(정의 (머리돌기 proc init list) 
  (만약 list 
    (머리돌기 proc (proc init (머 list)) (꼬 list)) 
    init))

(정의 (꼬리돌기 proc init list) 
  (만약 list 
    (proc (머 list) (꼬리돌기 proc init (꼬 list))) 
    init))

;향후 ASM으로 이동할 함수

(정의 (나머지 가 나)
      (- 가 (* (/ 가 나) 나)))

(정의 (이의승 n)
  (만약 (= n 0) 
    1
    (* 2 (이의승 (- n 1)))
  )
)
;ash 구현
(정의 (비트이동 val count)
  (조건
    ((= count 0) val)
    ((> count 0)
      (* val (이의승 count))
    )
    ((< count 0)
      (/ val (이의승 (- 0 count)))
    )
  )
)

;ldb 구현
(정의 (비트추출 size pos n)
  (비트곱
    (비트이동 n (- 0 pos))
    (- (이의승 size) 1)
  )
)

(정의 (비트곱 a b)
  (만약 (또는 (= a 0) (= b 0))
    0
    (
      +
      (* (나머지 a 2) (나머지 b 2))
      (* 2 (비트곱 (/ a 2) (/ b 2)))
    )
  )
)

(정의 (비트합 a b)
  (만약 (그리고 (= a 0) (= b 0))
    0
    (
      +
      (만약 (그리고 (= (나머지 a 2) 0) (= (나머지 b 2) 0))
        0 1)
      (* 2 (비트합 (/ a 2) (/ b 2)))
    )
  )
)

(정의 (베타합 a b)
  (조건
    ((= a 0) b)
    ((= b 0) a)
    (#참
      (
        +
        (만약 (= (나머지 a 2) (나머지 b 2))
          0 1)
        (* 2 (베타합 (/ a 2) (/ b 2)))
      )
    )
  )
)

;(정의 foldr 꼬리돌기)
;(정의 foldl 머리돌기)

(정의 (리스트 . items) 
  (꼬리돌기 짝 공 items))

(정의 (거꾸로 list) 
  (머리돌기 (람다 (a x) (짝 x a)) 공 list))

(정의 (한맵 proc list) 
  (꼬리돌기 (람다 (x rest) (짝 (proc x) rest)) 공 list))

(정의 (맵 proc . arg-lists) 
  (만약 (머 arg-lists) 
    (짝 (적용 proc (한맵 머 arg-lists)) 
    (적용 맵 (짝 proc (한맵 꼬 arg-lists)))) 공))

(정의 (접합 a b) 
  (꼬리돌기 짝 b a))

(매크로 (특이인용 x) 
  (만약 (짝? x) 
    (만약 (같다? (머 x) '비인용) 
      (꼬머 x) 
      (만약 (짝? (머 x)) 
        (만약 (같다? (머머 x) '비인용연결) 
          (리스트 '접합 (꼬머 (머 x)) 
          (리스트 '특이인용 (꼬 x))) 
        (리스트 '짝 (리스트 '특이인용 (머 x)) (리스트 '특이인용 (꼬 x)))) 
    (리스트 '짝 (리스트 '특이인용 (머 x)) (리스트 '특이인용 (꼬 x))))) 
  (리스트 '인용 x)))

; (매크로 (임시 defs . body) 
;   `((람다 ,(맵 머 defs) ,@body) ,@(맵 꼬머 defs)))

;(매크로 (새함수 body)
;  (임시 ((함수명 (_모))
;         (변수명 (_모)))
;    `(정의 ,함수명 (람다 (,변수명) (,@body)))))

; (정의 let 임시)
; (정의 mapcar 한맵)
; (정의 gensym _모)
; (정의 listp 리스트?)

; (매크로 (임시 defs . body) 
;   `((람다 ,(맵 머 defs) ,@body) ,@(맵 꼬머 defs)))

; (defmacro (letrec bindings . body)
;   (let ((temp (gensym)))
;     `(let (,@(mapcar (lambda (binding)
;                        (if (listp binding)
;                            `(,temp (labels ((,(car binding) ,@(cdr binding))))
;                               (setf ,(car binding) (funcall ,temp)))
;                            `(,binding)))
;                      bindings))
;        ,@body)))

; (매크로 (잠시 defs . body)
;   ()
;   (임시 ((gensym (람다 () (접합 'g (머리돌기 (람다 (x) (+ x 1)) 0 (꼬리돌기 (람다 (x y) (접합 x y)) '() (맵 (람다 (x) (리스트 x)) (거꾸로 defs)))))))
;     `(임시rec (,gensym ,defs)
;        ,@body)))

;(매크로 (letrec (&rest bindings) &body body)
;  (let ((temp (gensym)))
;    `(let (,@(mapcar (lambda (binding)
;                       (if (listp binding)
;                           `(,temp (labels ((,(머 binding) ,@(꼬 binding))))
;                              (setf ,(머 binding) (funcall ,temp)))
;                           `(,binding)))
;                     bindings))
;       ,@body)))

; '임시rec'를 이용하여 다시 써야 함
;(매크로 (조건 . 절)
;  (정의 (cond-clauses->if lst)
;    (만약 (아톰? lst)
;          공
;          (임시 ((첫절 (머 lst)))
;            (만약 (또는 (같다? (머 첫절) '그외)
;                        (같다? (머 첫절) #참))
;                  (만약 (공? (꼬 첫절))
;                        (머 첫절)
;                        (짝 '머 (꼬 첫절)))
;                  (만약 (공? (꼬 첫절))
;                        ; test by itself
;                        (리스트 '또는
;                                (머 첫절)
;                                (cond-clauses->if (꼬 lst)))
;                        (리스트 '만약
;                                (머 첫절)
;                                (짝 '머 (꼬 첫절))
;                                (cond-clauses->if (꼬 lst))))))))
;  (cond-clauses->if 절))

; (정의 (cond-clauses->if lst)
;   (만약 (아톰? lst)
;         공
;         (임시 ((첫절 (머 lst)))
;           (만약 (또는 (같다? (머 첫절) '그외)
;                       (같다? (머 첫절) #참))
;                 (만약 (공? (꼬 첫절))
;                       (머 첫절)
;                       (머 (꼬 첫절)))
;                 (만약 (공? (꼬 첫절))
;                       ; test by itself
;                       (또는 (머 첫절)
;                             (cond-clauses->if (꼬 lst)))
;                       (만약
;                               (머 첫절)
;                               (머 (꼬 첫절))
;                               (cond-clauses->if (꼬 lst))))))))

; (정의 (조건->만약 절)
;   (만약 (공? 절)
;         공
;         (임시 ((첫절 (머 절)))
;           (만약 (또는 (같다? (머 첫절) '그외)
;                       (같다? (머 첫절) #참))
;                 (머 (꼬 첫절))
;                 (만약 (공? (꼬 첫절))
;                       (또는 (머 첫절)
;                             (조건->만약 (꼬 절)))
;                       (만약 (머 첫절)
;                             (머 (꼬 첫절))
;                             (조건->만약 (꼬 절))))))))

; ; (조건 ((#참 2) (그외 3))) => 공  ;; ???
; (정의 (조건->만약 절)
;   (만약 (공? 절)
;         공
;         (임시 ((첫절 (머 절)))
;           (만약 (또는 (같다? (머 첫절) '그외)
;                       (같다? (머 첫절) #참))
;                 (머 (꼬 첫절))
;                 (조건->만약 (꼬 절))))))

; (매크로 (조건 . 절)
;   (조건->만약 절))

;(매크로 (조건 . 절)
;  (정의 (cond-clauses->if lst)
;    (만약 (아톰? lst)
;          공
;          (임시 ((첫절 (머 lst)))
;            (만약 (또는 (같다? (머 첫절) '그외)
;                        (같다? (머 첫절) #참))
;                  (만약 (공? (꼬 첫절))
;                        (머 첫절)
;                        (짝 '머 (꼬 첫절)))
;                  (만약 (공? (꼬 첫절))
;                        ; test by itself
;                        (리스트 '또는
;                                (머 첫절)
;                                (cond-clauses->if (꼬 lst)))
;                        (리스트 '만약
;                                (머 첫절)
;                                (짝 '머 (꼬 첫절))
;                                (cond-clauses->if (꼬 lst))))))))
;  (cond-clauses->if 절))


;                          ; test => expression
;                          (만약 (같다? (꼬머 첫절) '=>)
;                                (만약 (1arg-lambda? (꼬꼬머 첫절))
;                                      ; test => (lambda (x) ...)
;                                      (임시 ((var (꼬머머 (꼬꼬머 첫절))))
;                                        `(임시 ((,var ,(머 첫절)))
;                                           (만약 ,var ,(머 'begin (꼬꼬 (꼬꼬머 첫절)))
;                                                 ,(cond-clauses->if (꼬 lst)))))
;                                      ; test => proc
;                                      (임시 ((b (gensym)))
;                                        `(임시 ((,b ,(머 첫절)))
;                                           (만약 ,b
;                                                 (,(꼬꼬머 첫절) ,b)
;                                                 ,(cond-clauses->if (꼬 lst))))))
;                                (리스트 '만약
;                                        (머 첫절)
;                                        (짝 'begin (꼬 첫절))
;                                        (cond-clauses->if (꼬 lst)))))))))
;   (cond-clauses->if clauses))

; (매크로 (cond . clauses)
;   (정의 (cond-clauses->if lst)
;     (만약 (아톰? lst)
;           공
;           (임시 ((첫절 (머 lst)))                       ; (임시 ((clause (머 lst)))
;                 (만약 (또는 (같다? (머 첫절) '그외)
;                             (같다? (머 첫절) #참))
;                       (만약 (공? (꼬 첫절))
;                             (머 첫절)
;                             (짝 '머 (꼬 첫절)))
;                       (만약 (공? (꼬 첫절))
;                             ; test by itself
;                             (리스트 '또는
;                                     (머 첫절)
;                                     (cond-clauses->if (꼬 lst)))
;                             ; test => expression
;                             (만약 (같다? (cadr clause) '=>)
;                                   (if (1arg-lambda? (caddr clause))
;                           ; test => (lambda (x) ...)
;                           (let ((var (caadr (caddr clause))))
;                             `(let ((,var ,(car clause)))
;                                (if ,var ,(cons 'begin (cddr (caddr clause)))
;                                    ,(cond-clauses->if (cdr lst)))))
;                           ; test => proc
;                           (let ((b (gensym)))
;                             `(let ((,b ,(car clause)))
;                                (if ,b
;                                    (,(caddr clause) ,b)
;                                    ,(cond-clauses->if (cdr lst))))))
;                       (list 'if
;                             (car clause)
;                             (cons 'begin (cdr clause))
;                             (cond-clauses->if (cdr lst)))))))))
;   (cond-clauses->if clauses))
