     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) ACTGRP('CONNECT')
     H BNDDIR('WTBNDDIR')
     H*====================================================================
     H/TITLE FIXSY424 - Populate Inv# File for Portal
     H*====================================================================
      /Free
       Dcl-F WMP3P Usage(*Input) Rename(pp301:pp30p);
       Dcl-F WDSEQP Usage(*Output) Rename(pseq01:pseq0p);
       Dcl-S maxInvoice# Packed(10:0);
       Dcl-S saveInvoice# Packed(10:0);
       Dou %Eof(WMP3P);
          Read WMP3P;
          If %Eof;
             Leave;
          EndIf;
          Clear maxInvoice#;
          Clear saveInvoice#;
          Exec Sql
           Select Max(EAninv)
           Into :maxInvoice#
           From WMEAP
           Where EAco#  = :P3co#
            And  EAfnd  = :P3fnd
            And  EAemp# = :P3emp#
            And  EAdiv  = :P3div;
          Exec Sql
           Select Max(EAinv#)
           Into :saveInvoice#
           From WDEAP
           Where EAco#  = :P3co#
            And  EAfnd  = :P3fnd
            And  EAemp# = :P3emp#
            And  EAdiv  = :P3div;
          If saveInvoice# > maxInvoice#;
             maxInvoice# = saveInvoice#;
          EndIf;
          Exec Sql
           Select Max(EAinv#)
           Into :saveInvoice#
           From WDEAP_HLD
           Where EAco#  = :P3co#
            And  EAfnd  = :P3fnd
            And  EAemp# = :P3emp#
            And  EAdiv  = :P3div;
          If saveInvoice# > maxInvoice#;
             maxInvoice# = saveInvoice#;
          EndIf;
          Exec Sql
           Select Max(XAninv)
           Into :saveInvoice#
           From WMXAP
           Where XAco#  = :P3co#
            And  XAfnd  = :P3fnd
            And  XAemp# = :P3emp#
            And  XAdiv  = :P3div;
          If saveInvoice# > maxInvoice#;
             maxInvoice# = saveInvoice#;
          EndIf;
          If maxInvoice# <> 0;
             SEkey = %Editc(P3co#:'X') + %Editc(P3fnd:'X') +
              %Editc(P3emp#:'X') + %Editc(P3div:'X');
             SELastSeq# = maxInvoice#;
             Write pSeq0p;
          EndIf;
       Enddo;
       *InLR = *On;
      /End-Free
