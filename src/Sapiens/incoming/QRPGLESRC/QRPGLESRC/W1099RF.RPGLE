     h/title W1099RT - create "F" Record for IRSTAPE file
     H dftactgrp(*no) actgrp(*CALLER)
      ** =========================================================== **
      *  (C) Copyright Progressive Data Solutions, Inc., 1997, 2002
      ** =========================================================== **
      * REVISIONS:                                                    *
      *                                                               *
      *  01/21/13  C:287 SEQ#: SY266_OVR                              *
      *                  Add default activation group compiler options*
      *                                                               *
      *  10/11/02  C:132 SEQ#: WC1097                                 *
      *                  Add code for sequence number in F record &   *
      *                  use input parm for number of A records       *
      *                  Add two input parameters, first for number   *
      *                  of companies in file, second for Seq nbr     *
      ** =========================================================== **
     Fwatnp     o    e             disk    usropn
     Fwxtnp     if   e             disk    rename(ptn01 : ptn02)
     F
     Dcmdstr           s             30
     Dlength           s             15  5
     Dcount            s              8  0
     D
     Dcmd1             c                   const('CLRPFM FILE(WATNP) +
     D                                     MBR(WATNP)')
     Dcmd2             c                   const('OVRDBF FILE(WATNP) +
     D                                     MBR(WATNP)')
     D fData           ds           749
     D  aCount                        8S 0
     D  zeros                        21
      ***** BEG ADD **** 10/11/02 **************************************
     D  seqnbr               499    506                                         IRS Seq Nbr
      ***** END ADD **** 10/11/02 **************************************
     c*
      ***** BEG ADD **** 10/11/02 **************************************
     c     *entry        plist
     c                   parm                    qqccnt            8            Company Count
     c                   parm                    qqseq#            8            Seq # for F record
      ***** END ADD **** 10/11/02 **************************************
      *
     C
     C                   call      'QCMDEXC'
     C                   parm      cmd1          cmdstr
     C                   parm      30            length
     C
     C                   call      'QCMDEXC'
     C                   parm      cmd2          cmdstr
     C                   parm      30            length
     C
     C                   open      watnp
     C
     C                   read      wxtnp                                  99
     C                   dow       *in99 = *off
     C                   add       1             count
     C                   read      wxtnp                                  99
     C                   enddo
     C
     C                   move      count         aCount
      ***** BEG ADD **** 10/11/02 **************************************
     c                   if        qqccnt > *zeros
     c                   move      qqccnt        aCount
     c                   endif
     c                   move      qqseq#        seqnbr
      ***** END ADD **** 10/11/02 **************************************
      *
     C                   move      *all'0'       zeros
     C                   movel     'F'           tnrtyp
     C                   movel     fData         tndata
     C                   write     ptn01
     C
     C                   eval      *inlr = *on
