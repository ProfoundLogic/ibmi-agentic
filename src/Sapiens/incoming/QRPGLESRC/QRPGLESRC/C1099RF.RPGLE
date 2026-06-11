     h/title C1099RT - create "F" Record for IRSTAPE file
     H DFTACTGRP(*NO) ACTGRP(*caller)
      ** =========================================================== **
      *  (C) Copyright Progressive Data Solutions, Inc., 2002, 2003
      ** =========================================================== **
      *                                                               *
      *  11/11/02  C:132 PROJECT: PC008                               *
      *                  Add code for sequence number in F record &   *
      *                  use input parm for sequence number to be     *
      *                  output in position 500-507 of the F record   *
      *                                                               *
      ** =========================================================== **
      * REVISIONS:                                                    *
     H*                                                               *
     H*  02/25/03  C:132 SEQ#: PC051  36176                           *
     H*                  Change FRCD in Constant CMD1 to CATNP mbr    *
     H*                  name. Delete the code to clear member        *
      *                                                               *
      ** =========================================================== **
     Fcatnp     o    e             disk    usropn
     Fcxtnp     if   e             disk    rename(ptn01 : ptn02)
     F
     Dcmdstr           s             30
     Dlength           s             15  5
     Dcount            s              8  0
     D
      ***** BEG CHG ***** 02/25/03 **************************************
     D******cmd1             c                   const('CLRPFM FILE(CATNP) +
     D******                                     MBR(FRCD)')
     Dcmd1             c                   const('CLRPFM FILE(CATNP) +
     D                                     MBR(CATNP)')
      ***** END CHG ***** 02/25/03 **************************************
     Dcmd2             c                   const('OVRDBF FILE(CATNP) +
     D                                     MBR(CATNP)')
     D fData           ds           749
     D  aCount                        8S 0
     D  zeros                        21
     D  seqnbr               499    506                                         IRS Seq Nbr
     c*
     c     *entry        plist
     c                   parm                    qqccnt            8            Company Count
     c                   parm                    qqseq#            8            Seq # for F record
      *
     C
      ***** BEG DEL ***** 02/25/03 **************************************
      * Dont clear P&C file member, has live records
     C********           call      'QCMDEXC'
     C********           parm      cmd1          cmdstr
     C********           parm      30            length
      ***** END DEL ***** 02/25/03 **************************************
     C
     C                   call      'QCMDEXC'
     C                   parm      cmd2          cmdstr
     C                   parm      30            length
     C
     C                   open      catnp
     C
     C                   read      cxtnp                                  99
     C                   dow       *in99 = *off
     C                   add       1             count
     C                   read      cxtnp                                  99
     C                   enddo
     C*
      ***** BEG ADD ***** 02/25/03 **************************************
      * If no records in CXTNP, make sure count is at least one
     C                   if        count = *zeros
     c                   z-add     1             count
     c                   endif
      ***** END ADD ***** 02/25/03 **************************************
     C*
     C                   move      count         aCount
     C*
     c                   if        qqccnt > *zeros
     c                   move      qqccnt        aCount
     c                   endif
     c                   move      qqseq#        seqnbr
      *
     C                   move      *all'0'       zeros
     C                   movel     'F'           tnrtyp
     C                   movel     fData         tndata
     C                   write     ptn01
     C
     C                   eval      *inlr = *on
