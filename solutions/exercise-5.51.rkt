#lang eopl

(define program
  "let buffer = 0
   in let mut = mutex()
      in let producer = proc (n)
                          letrec wait1(k) = if zero?(k)
                                            then begin set buffer = n;
                                                       signal(mut)
                                                 end
                                            else begin print(-(k, -200));
                                                       (wait1 -(k, 1))
                                                 end
                          in (wait1 5)
         in let consumer = proc (d)
                             begin wait(mut);
                                   buffer
                             end
            in begin wait(mut);
                     spawn(proc (d)
                             (producer 44));
                     print(300);
                     (consumer 86)
               end")

(provide program)
