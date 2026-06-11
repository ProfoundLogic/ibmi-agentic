     hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')

     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     h*****************************************************************
     h/TITLE W8006R5 - Fee invoice deletion program
     h*****************************************************************
     h* PROGRAM DESCRIPTION:                                          *
     h*   This program delete fee invoices                            *
     h*                                                               *
     h* CALLED FROM:                                                  *
     h*   W8006C5                                                     *
     h* CALLS TO:                                                     *
     h*   NONE                                                        *
     h* PROGRAMMER: C:65                                              *
     h* CREATED:    08/29/11            PROJECT: WC1851               *
     h*****************************************************************
     h* INDICATORS:                                                   *
     h*   *IN60 - GENERAL CHAIN, DOWEQ                                *
     h*****************************************************************
     h* Revisions:                                                    *
     h*                                                               *
     h*                                                               *
     h*****************************************************************
     fwmexl     if   e           k disk
     fwmfnl     if   e           k disk
     fwmfnp     if   e             disk    rename(pfn01 : pfn0p)
     fwmemp     if   e             disk    rename(pem01 : pem0p)
     fwdfcl4    uf   e           k disk
     fwdeal28   uf   e           k disk
     Fwdeal4    uf a e           k disk    rename(pea01:pea04)

      * Accounts Receivable
     Fwmeap     o    e           k disk    rename(pea01:pea02)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PPA01)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     DsysDate7         S              7  0
     DsysTime6         s              6  0
     d s2fnd           s                   like(eafnd)
     d s2emp#          s                   like(eaemp#)
     d s2div           s                   like(eadiv)
     d s2fyr           s                   like(eafyr)
     d s2bat#          s                   like(eabat#)
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d comapi          c                   const('WA001R')
     d @@co            s              3
     d @@fnd           s              3
     d @@emp#          s              9
     d @@div           s              5
     d @@podt          s              8
     d ##co#           s              3  0
     d ##fnam          s             40
     d ##fnd           s              3  0
     d found           s               n   inz(*off)
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d svfnd           s                   like(exfnd)
     d kkfnd           s                   like(exfnd)
     d kkemp#          s                   like(ememp#)
     d kkpodt          s              7  0
     d kkdiv           s              5  0
     d rrnGrp          s              9  0
     d rrnEmp          s              9  0
     d error           s              1
     d inkc            s              1
     d inkl            s              1
     d NewBAT#         s                   like(eaBAT#)
     d svBATA          s                   like(eaBATA)
     d xxpodt8         s              8  0
     d k1co#           s                   like(eaco#)
     d k1bat#          s                   like(eabat#)
     d k1fnd           s                   like(eafnd)
     d k1emp#          s                   like(eaemp#)
     d k1div           s                   like(eadiv)
     d k1fyr           s                   like(eafyr)
      *
     *---------------------------------------------------------------------
     * Data Structures and Arrays
     *---------------------------------------------------------------------
     *---------------------------------------------------------------------
     * Prototypes
     *---------------------------------------------------------------------
     D/copy sPrDate
     d/copy wpremplr
    c*================================================================
    c*  PLIST definitions
    c*================================================================
     c     datchk        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr

     c     *entry        plist
     c                   parm                    @@co
     c                   parm                    @@fnd
     c                   parm                    @@emp#
     c                   parm                    @@div
     c                   parm                    @@podt
     *
     *================================================================
     *  KLIST definitions
     *================================================================

     C     eakey1        KLIST
     C                   KFLD                    k1CO#
     C                   KFLD                    k1BAT#
     C                   KFLD                    k1FND
     C                   KFLD                    k1EMP#
     C                   KFLD                    k1DIV
     C                   KFLD                    k1FYR
      *
     C     eakey2        KLIST
     C                   KFLD                    k1CO#
     C                   KFLD                    k1BAT#
      *
     C     eakey3        KLIST
     C                   KFLD                    k1CO#
     C                   KFLD                    NewBAT#
      *
     c     exky          klist
     c                   kfld                    ##co#
     c                   kfld                    kkfnd

     c     fcky          klist
     c                   kfld                    ##co#
     c                   kfld                    kkfnd
     c                   kfld                    kkpodt

     c     fcky1         klist
     c                   kfld                    ##co#
     c                   kfld                    kkfnd
     c                   kfld                    kkpodt
     c                   kfld                    kkemp#

     c     fcky2         klist
     c                   kfld                    ##co#
     c                   kfld                    kkfnd
     c                   kfld                    kkpodt
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv

     c     fcky3         klist
     c                   kfld                    ##co#
     c                   kfld                    fcfnd
     c                   kfld                    fcpodt
     c                   kfld                    fcemp#
     c                   kfld                    fcdiv
     C*
     C     TBKEY         klist
     C                   kfld                    ##CO#
     C                   kfld                    XXKEY            10
     C*
     c                   move      @@co          ##co#
     c                   move      @@fnd         kkfnd
     c                   move      @@emp#        scemp#            9 0
     c                   move      @@div         scdiv             5
     c                   move      @@podt        xxpodt            8 0
     c                   eval      KKpodt = dtCYMD(xxpodt)
     C*
     C* Process records
     C*
     c                   eval      NewBAT# = 0
     C*
     c                   exsr      process
     c
     c*  Process Fee Transfer Batch if needed
     c
     c                   if        NewBat# <> 0
     c                   exsr      TranBat
     c                   endif
     c*
     c                   move      *on           *inlr
     C*
    c*====================================================================
    c* Process - Process records indicated by parms
    c*====================================================================
     c     process       begsr

     c*    If Employer field is blank then delete all the records of group with provided
     c*    Post date
     c  n90              if        scemp# = *zeros

     c     fcky          setll     pfc01
     c     fcky          reade     pfc01                                  60
     c                   z-add     0             s2fnd
     c                   z-add     0             s2emp#
     c                   z-add     0             s2div
     c                   z-add     0             s2fyr
     c                   z-add     0             s2bat#

     c* Check for existence of data in WDFCP
     c                   if        *in60 = *on
     c                   eval      *in77 = *on
     c                   eval      *in90 = *on
     c                   leavesr

     c                   else
     c                   dow       not(%eof)
     c                   delete    pfc01
     c
     c     fcfnd         ifne      s2fnd
     c     fcemp#        orne      s2emp#
     c     fcdiv         orne      s2div
     c     fcfyr         orne      s2fyr
     c     fcbat#        orne      s2bat#
     c                   exsr      DltWdeapTyp4
     c                   z-add     fcfnd         s2fnd
     c                   z-add     fcemp#        s2emp#
     c                   z-add     fcdiv         s2div
     c                   z-add     fcfyr         s2fyr
     c                   z-add     fcbat#        s2bat#
     c                   endif
     c     fcky          reade     pfc01
     c                   enddo

     c                   endif                                                  *in60 = *on
     c                   endif                                                  scemp# = 0

     c*    If Employer field is not blank and Div is blank, then delete all records of
     c*    this employer.
     c  n90              if        scemp# <> *zeros and scdiv = *blanks
     c                   eval      kkemp# = scemp#
     c     fcky1         setll     pfc01
     c     fcky1         reade     pfc01                                  60
     c                   z-add     0             s2fnd
     c                   z-add     0             s2emp#
     c                   z-add     0             s2div
     c                   z-add     0             s2fyr
     c                   z-add     0             s2bat#

     c* Check for existence of data in WDFCP
     c                   if        *in60 = *on
     c                   eval      *in77 = *on
     c                   eval      *in90 = *on
     c                   leavesr

     c                   else
     c                   dow       not(%eof)
     c                   delete    pfc01
     c
     c     fcfnd         ifne      s2fnd
     c     fcemp#        orne      s2emp#
     c     fcdiv         orne      s2div
     c     fcfyr         orne      s2fyr
     c     fcbat#        orne      s2bat#
     c                   exsr      DltWdeapTyp4
     c                   z-add     fcfnd         s2fnd
     c                   z-add     fcemp#        s2emp#
     c                   z-add     fcdiv         s2div
     c                   z-add     fcfyr         s2fyr
     c                   z-add     fcbat#        s2bat#
     c                   endif
     c     fcky1         reade     pfc01
     c                   enddo

     c                   endif                                                  *in60 = *on
     c                   endif                                                  scemp# <> 0,scdiv=bl

     c*    If Employer and Div field is not blank, then delete specific record.
     c  n90              if        scemp# <> *zeros and scdiv <> *blanks
     c                   eval      kkemp# = scemp#
     c                   movel     scdiv         kkdiv
     c     fcky2         setll     pfc01
     c     fcky2         reade     pfc01                                  60
     c                   z-add     0             s2fnd
     c                   z-add     0             s2emp#
     c                   z-add     0             s2div
     c                   z-add     0             s2fyr
     c                   z-add     0             s2bat#

     c* Check for existence of data in WDFCP
     c                   if        *in60 = *on
     c                   eval      *in77 = *on
     c                   eval      *in90 = *on
     c                   leavesr

     c                   else
     c                   dow       not(%eof)
     c                   delete    pfc01
     c
     c     fcfnd         ifne      s2fnd
     c     fcemp#        orne      s2emp#
     c     fcdiv         orne      s2div
     c     fcfyr         orne      s2fyr
     c     fcbat#        orne      s2bat#
     c                   exsr      DltWdeapTyp4
     c                   z-add     fcfnd         s2fnd
     c                   z-add     fcemp#        s2emp#
     c                   z-add     fcdiv         s2div
     c                   z-add     fcfyr         s2fyr
     c                   z-add     fcbat#        s2bat#
     c                   endif
     c
     c     fcky2         reade     pfc01
     c                   enddo

     c                   endif                                                  *in60 = *on
     c                   endif
      *
     c                   endsr                                                  process

    c*====================================================================
    c* DltWdeapTyp4 - Delete specific records that match to fee detail.
    c*====================================================================
     c     DltWdeapTyp4  begsr

     c     fcky3         setll     pea01
     c     fcky3         reade     pea01                                  60
     c                   dow       not(%eof)
     c                   if        eabat# = fcbat#
     c                             and eafyr = fcfyr
     c                   exsr      DeleteARBtch
     c                   endif
     c     fcky3         reade     pea01
     c                   enddo

     c                   endsr
    c*====================================================================

      *====================================================================
      * DeleteARBtch - Delete A/R Batch                                     pecific record.
      *====================================================================
     c     DeleteARBtch  begsr
     c                   eval      k1co# = eaco#

     c                   if        eaachf <> 'F'
     c                   leavesr
     c                   endif

     c                   select

      * not approved and not tranferred -- delete record update batch amt

     c                   when      (eatfap <> 'Y') and (eatffl <> 'Y')
     c                   eval      k1bat# = eabat#
     c                   eval      svbata = eabata - eaamt
     c                   delete    pea01
     c                   exsr      UpdBat

      * approved and not tranferred
      *        - Create offetting entry
      *        - Update batch with new totals

     c                   when      (eatfap = 'Y') and (eatffl <> 'Y')
     C                   move      EACO#         #1CO#             3
     C                   move      EAFND         #1FND             3
     C                   move      EAEMP#        #1EMP#            9
     C                   move      EADIV         #1DIV             5
     C                   move      EAFYR         #1FYR             3
     c                   eval      xxpodt8 = dtMDYY(eapodt)
     C                   move      xxpodt8       #1PODT            8
     C                   move      eaarcd        #1ARCD            2
     C                   z-add     eaamt         xxAMT            11 2
     c                   eval      xxamt = xxamt * -1
     C                   move      xxamt         #1AMT            11
     C                   move      eabat#        #1BAT#            9
     C                   move      *blanks       #1MODE            1

     c                   call      'SRBAT'
     C                   PARM                    #1CO#             3
     C                   PARM                    #1FND             3
     C                   PARM                    #1EMP#            9
     C                   PARM                    #1DIV             5
     C                   PARM                    #1FYR             3
     C                   PARM                    #1PODT            8
     C                   PARM                    #1ARCD            2
     C                   PARM                    #1AMT            11
     C                   PARM                    #1BAT#            9
     C                   PARM                    #1MODE            1

      *  Tranferred
      *     - create new batch with offsetting entry

     c                   when      (eatffl = 'Y')
     C                   move      EACO#         #1CO#             3
     C                   move      EAFND         #1FND             3
     C                   move      EAEMP#        #1EMP#            9
     C                   move      EADIV         #1DIV             5
     C                   move      EAFYR         #1FYR             3
     c                   eval      xxpodt8 = dtMDYY(eapodt)
     C                   move      xxpodt8       #1PODT            8
     C                   move      eaarcd        #1ARCD            2
     C                   z-add     eaamt         xxAMT            11 2
     c                   eval      xxamt = xxamt * -1
     C                   move      xxamt         #1AMT            11
     C                   move      NewBat#       #1BAT#            9
     C                   move      *blanks       #1MODE            1

     c                   call      'SRBAT'
     C                   PARM                    #1CO#             3
     C                   PARM                    #1FND             3
     C                   PARM                    #1EMP#            9
     C                   PARM                    #1DIV             5
     C                   PARM                    #1FYR             3
     C                   PARM                    #1PODT            8
     C                   PARM                    #1ARCD            2
     C                   PARM                    #1AMT            11
     C                   PARM                    #1BAT#            9
     C                   PARM                    #1MODE            1

     c                   move      #1bat#        NewBat#

     c                   endsl

     c                   endsr                                                  DeleteARBtch
      *====================================================================


      *====================================================================
      * UpdBat - UPDATE BATCH TOTAL AND REMOVE DUMMY PLACEHOLDER BATCH.
      *====================================================================
     c     UpdBat        begsr

      *  UPDATE ALL BATCH RECORDS WITH THE TOTAL BATCH AMOUNT

     c     eakey2        chain     pea04                              60
      *
     c     *IN60         DOWEQ     '0'
     c                   eval      eabata = svbata
     c                   UPDATE    pea04
     c     EAKEY2        READE     pea04                                  60
     c                   ENDDO                                                  *IN60 DOWEQ

      *  Delete the A/R batch dummy record.

     c                   MOVE      *HIVAL        k1FND
     c                   MOVE      *HIVAL        k1EMP#
     c                   MOVE      *HIVAL        k1DIV
     c                   MOVE      *HIVAL        k1FYR
     c     EAKEY1        CHAIN     pea04                              60
     c  N60              DELETE    pea04

     c                   ENDSR                                                  UpdBat
      *====================================================================


      *====================================================================
      * TranBat - Transfer New Batch
      *====================================================================
     c     TranBat       begsr
     c                   eval      sysDate7 = DateTo7(SystemDate)
     c                   eval      sysTime6 = TimeTo6(SystemTime)
     c                   z-add     0             s2fnd
     c                   z-add     0             s2emp#
     c                   z-add     0             s2div
     c                   z-add     0             s2fyr

      *  Transfer new batch records

     c     eakey3        setll     pea04
     c     eakey3        reade     pea04

     c                   dow       not(%eof)
     c                   eval      eatfap = 'Y'
     c                   eval      eatffl = 'Y'
     c                   eval      eaachf = 'F'
      *
     c                   eval      eaedit = *blank
     c                   eval      eastat = *blank
     c                   eval      eauser = q1user
     c                   eval      eapgm  = q1pgm
     c                   eval      eabul1 = eabat#
     c                   eval      eatfam = eaamt
     c                   eval      eaninv = eainv#
     C                   MOVEL     EAARCD        XXKEY
     C*
     C     TBKEY         CHAIN     PTT01                              60
     c
     c                   move      tttype        eaarty
     c                   move      ttctyp        eactyp
     c
     c     eafnd         ifne      s2fnd
     c     eaemp#        orne      s2emp#
     c     eadiv         orne      s2div
     c     eafyr         orne      s2fyr
     c                   eval      sysTime6 = TimeTo6(SystemTime)
     c                   z-add     eafnd         s2fnd
     c                   z-add     eaemp#        s2emp#
     c                   z-add     eadiv         s2div
     c                   z-add     eafyr         s2fyr
     c                   endif
     c
     c                   eval      EATRDT = sysDate7
     c
     C                   eval      sysTime6 = sysTime6 + 1
     C                   eval      EATIME = sysTime6
     c                   eval      EADT   = sysDate7
     C                   write     pea02

     c                   eval      eastat = 'T'
     c                   eval      eapgm  = q1pgm
     c                   update    pea04
     c     eakey3        reade     pea04
     c                   enddo

     c                   endsr                                                  TranBat
      *====================================================================


