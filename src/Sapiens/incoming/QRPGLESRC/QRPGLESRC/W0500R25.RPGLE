     H Copyright('(c) 2022 Sapiens. All Rights Reserved.')
     H/title  W0500R25 - GENERATE FUND CHECK REQUESTS
      **********************************************************************************************
      *
      *  Called from: W0500C25 (W0500R - Opt25)
      *
      *  Created: 03/12/2003  John Stone/Matt Dawson                  *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      *  10/13/22 C:291 Project: 129WXD                               *
      *                 Print Federal Name on Refund checks if it     *
      *                 exist and the checkbox is checked at company  *
      *                 level.                                        *
      *                                                               *
      *  12/26/07 C:183 Project: 81WZB                                *
      *                 Added code to populate R4CUST and R4SDFL.     *
      *                                                               *
      *  07/18/07 C:136 Project: WC1491                               *
      *                 Implement Ind Cash and Return Deposit         *
      *                                                               *
      *  03/07/05  C:65  Project: WC1202                              *
      *                  Store extended employer name field for checks.
      *                                                               *
      *  11/05/04  C:33  Project: WC1033                              *                            *
      *                  Include calculations for Agency Billing.     *                            *
      *===============================================================*
     Fwxrtl     uf   e           k disk
     Fwxrnl     uf   e           k disk
     Fwmfnl     if   e           k disk
     Fwmeml     if   e           k disk
     Fwmaal1    if   e           k disk
     Fwmagl     if   e           k disk
     Fwmpfl     if   e           k disk
     Fwar4p     o    e             disk
      *** Beg Add ***** 10/13/22 **************************************
     Fsmcol     if   e           k disk    Rename(pco01:pco00)
      *** End Add ***** 10/13/22 **************************************
      *
     D first           S              1n
     D datusa          S               D   datfmt(*usa)
      *
     D                 ds
     D  dsdttm                 1     12  0
     D  dstime                 1      6  0
     D  dsdate                 7     12  0
      *
     D psds          esds                  extname(szq1p)
      *===============================================================*
      * klist
      *===============================================================*
     C     emky          klist
     C                   kfld                    RTCO#
     C                   kfld                    RTFND
     C                   kfld                    RTEMP#
     C                   kfld                    RTDIV
      *
     C     aaky          klist
     C                   kfld                    RTCO#
     C                   kfld                    RTFND
     C                   kfld                    RTAGT#
      *
     C     agky          klist
     C                   kfld                    AACO#
     C                   kfld                    AAFEIN
     C                   kfld                    AAMOD
     C*
     C     keyWMAGL      klist
     C                   kfld                    RTCO#
     C                   kfld                    RTFEIN
     C                   kfld                    RTMOD
      *
     C     fnky          klist
     C                   kfld                    RTCO#
     C                   kfld                    RTFND
      *
     C     pfky          klist
     C                   kfld                    RTCO#
     C                   kfld                    RTFND
     C                   kfld                    RTPFIN
      *
     C     rtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkbat#
      *
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqbat#            9
      *===============================================================*
      * Main
      *===============================================================*
     C                   move      qqco#         kkco#             3 0
     C                   move      qqbat#        kkbat#            9 0
      /Free
      //*** Beg Add ***** 10/13/22 ********************************
       Chain (kkCo#) pco00;
      //*** End Add ***** 10/13/22 ********************************
      /End-Free

     C     rtky          setll     prt01
     C                   dou       %eof
     C     rtky          reade     prt01
      *
     C                   if        %eof
     C                   leave
     C                   endif
      *
     C                   if        RTSELC <> 'Y'
     C                   iter
     C                   endif
      *
     C                   exsr      move_out
     C                   enddo
      *
      * Delete batch control record
      *
     C                   dou       not %found
     C     rtky          delete    wxrnl
     C                   enddo
      *
      * Delete batch detail records
      *
     C                   dou       not %found
     C     rtky          delete    wxrtl
     C                   enddo
      *
     C                   eval      *inlr = *on
      *===================================================================
      * Move detail fields to file WAR4P
      *===================================================================
     C     Move_Out      Begsr
      *
     C                   clear                   pr401
     C                   MOVE      RTCO#         R4CO#
     C                   MOVE      RTFND         R4FND
     C                   MOVE      RTEMP#        R4EMP#
     C                   MOVE      RTDIV         R4DIV
     C                   MOVE      RTFYR         R4FYR
     C                   MOVE      RTAGT#        R4AGT#
     C                   MOVE      RTFEIN        R4FEIN
     C                   MOVE      RTMOD         R4MOD
     C                   MOVE      RTAMT         R4AMT
     C                   MOVE      RTPFIN        R4PFIN
     C                   MOVE      RTPTFL        R4PTFL
     C                   MOVE      'Y'           R4SELC
     C                   MOVE      RTARCD        R4ARCD
     C                   MOVE      RTBTYP        R4BTYP
     C                   EVAL      R4CUST = RTCUST
     C                   EVAL      R4SDFL = RTSDFL
      *
     C                   select
     C*
     C                   when      RTACFL = 'Y'
     c                   if        RTBTYP = 'A'
     C                   move      'N'           R4SELC
     c                   else
     c                   move      'Y'           R4SELC
     c                   endif
     C     keyWMAGL      chain     pag01
     C                   if        %found
     c                   movel(p)  agname        r4ptnm
     c                   move      *blanks       r4ptn2
     C                   eval      R4PAD1 = AGMAD1
     C                   eval      R4PAD2 = *blanks
     C                   eval      R4PCTY = AGMCTY
     C                   eval      R4PST  = AGMST
     C                   move      AGMZIP        R4PZIP
     C                   endif
      *
     C                   when      R4PTFL = 'E'
     C     emky          chain     wmeml
     C                   if        %found
      //*** Beg Add ***** 10/13/22 ***********************************
      /Free
        If COfedrc = 'Y' and EMfdnm <> *Blank;
           R4PtNm = EMfdnm;
           R4PtN2 = EMfdn2;
        Else;
      /End-Free
      //*** End Add ***** 10/13/22 ***********************************
     C                   movel(p)  emname        R4ptnm
     c                   movel(p)  emnam2        r4ptn2
      //*** Beg Add ***** 10/13/22 ***********************************
      /Free
        Endif;
      /End-Free
      //*** End Add ***** 10/13/22 ***********************************
      *
     C                   If        Emmpfl = 'M'
     C                   Movel     Emmad1        R4pad1
     C                   Movel     Emmad2        R4pad2
     C                   Movel     Emmcty        R4pcty
     C                   Movel     Emmst         R4pst
     C                   Movel     Emmzip        R4pzip
     C                   Else
     C                   Movel     Empad1        R4pad1
     C                   Movel     Empad2        R4pad2
     C                   Movel     Empcty        R4pcty
     C                   Movel     Empst         R4pst
     C                   Movel     Empzip        R4pzip
     C                   Endif
      *
     C                   Endif
      *
     C                   When      R4PTFL = 'A'
     C     aaky          chain     paa01
     C     agky          chain     pag01
     C                   If        %Found
     C                   Movel(p)  Agname        R4ptnm
     c                   move      *blanks       r4ptn2
     C                   Movel     Agmad1        R4PAD1
     C                   Movel     *Blanks       R4PAD2
     C                   Movel     Agmcty        R4PCTY
     C                   Movel     Agmst         R4PST
     C                   Movel     Agmzip        R4PZIP
     C                   endif
      *
     C                   When      R4PTFL = 'P'
     C     pfky          Chain     Wmpfl
     C                   If        %Found
     C                   Movel(p)  Pfname        R4ptnm
     c                   move      *blanks       r4ptn2
     C                   Movel     Pfadr1        R4pad1
     C                   Movel     Pfadr2        R4pad2
     C                   Movel     Pfcty         R4pcty
     C                   Movel     Pfst          R4pst
     C                   Movel     Pfzip         R4pzip
     C                   Endif
      *
     C                   when      R4PTFL = 'G'
     C     fnky          chain     pfn01
     C                   if        %found
     C                   movel(p)  FNNAME        R4PTNM
     c                   move      *blanks       r4ptn2
     C                   movel     FNMAD1        R4PAD1
     C                   movel     FNMAD2        R4PAD2
     C                   movel     FNMCTY        R4PCTY
     C                   movel     FNMST         R4PST
     C                   movel     FNMZIP        R4PZIP
     C                   endif
      *
     C                   endsl
      *
     C                   move      RTACFL        R4ACFL
     C                   move      RTCOM1        R4COM1
     C                   move      RTCOM2        R4COM2
     C                   move      RTCOM3        R4COM3
     C                   eval      R4IPGM = Q1PGM
     C                   eval      R4PGM  = Q1PGM
     C                   eval      R4USER = Q1USER
     C                   eval      R4IUSR = Q1USER
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
      *
     C                   call      'SRCVTDAT'
     C                   parm                    srdti             8 0
     C                   parm      *blanks       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
      *
     C                   movel     srdto         R4IDT
     C                   movel     srdto         R4DATE
     C                   eval      R4ITIM = dstime
     C                   eval      R4TIME = dstime
     C                   write     pr401
      *
     C                   Endsr
