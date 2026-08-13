      // @MBRTEXT: array ds example
         ctl-opt dftactgrp(*no) ;

         dcl-c sflpag 26;

         dcl-pi *N;
           rrn int(10);
           sflArray likeDS(sflArray_t) dim(SFLPAG);
         end-pi;

         dcl-ds sflArray_t qualified template;
           id_prod int(10);
           id_cat int(10);
           precio packed(13:2);
           descr varchar(50);
         end-ds;

         LoadSubfile();
         *inlr = *on;

         dcl-proc LoadSubfile;
           dcl-s x int(5);
           for x = 1 to SFLPAG;
             sflArray(x).id_prod = x;
             sflArray(x).id_cat = x;
             sflArray(x).precio = 1.99;
             sflArray(x).descr = 'Item Description ' + %char(x) +
                                                ' *more text*';
             rrn = x;
           endfor;
         end-proc;
