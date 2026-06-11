     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller)
     H*=====================================================================
     H/TITLE FIXWDSEQP - Store last invoice number used by Pay As You Go
     H*=====================================================================
       Dcl-F Wmeml Usage(*Input)
        Keyed Rename(pem01:pem00);
       Dcl-F Wdseqp Usage(*Update:*Output)
        Keyed Rename(pseq01:pseq0p);
       Dcl-S maxInvoice# Packed(10:0);
       Dcl-S saveInvoice# Packed(10:0);
       // Loop through Emp Master
       // Find max invoice #.
       //  Iter if zero,
       //  Else update or write to WDSEQP.
       Dou %Eof(Wmeml);
          Read pem00;
          If %Eof;
             Leave;
          Endif;
          Clear saveInvoice#;
          Clear maxInvoice#;
          Exec Sql
           Select Max(EAninv)
           Into :maxInvoice#
           From WMEAP
           Where EAco#  = :EMco#
            And  EAfnd  = :EMfnd
            And  EAemp# = :EMemp#
            And  EAdiv  = :EMdiv;
          Exec Sql
           Select Max(EAinv#)
           Into :saveInvoice#
           From WDEAP
           Where EAco#  = :EMco#
            And  EAfnd  = :EMfnd
            And  EAemp# = :EMemp#
            And  EAdiv  = :EMdiv;
          If saveInvoice# > maxInvoice#;
             maxInvoice# = saveInvoice#;
          EndIf;
          Exec Sql
           Select Max(EAinv#)
           Into :saveInvoice#
           From WDEAP_HLD
           Where EAco#  = :EMco#
            And  EAfnd  = :EMfnd
            And  EAemp# = :EMemp#
            And  EAdiv  = :EMdiv;
          If saveInvoice# > maxInvoice#;
             maxInvoice# = saveInvoice#;
          EndIf;
          If maxInvoice# = 0;
             Iter;
          Endif;
          SEkey =
           %Editc(EMco#:'X') +
           %Editc(EMfnd:'X') +
           %Editc(EMemp#:'X') +
           %Editc(EMdiv:'X');
          Chain SEkey pseq0p;
          If %Found;
             If SElastSeq# < maxInvoice#;
                SELastSeq# = maxInvoice#;
             Endif;
             Update pseq0p;
          ELse;
             SELastSeq# = maxInvoice#;
             Write pseq0p;
          Endif;
       Enddo;
       *Inlr = *On;
