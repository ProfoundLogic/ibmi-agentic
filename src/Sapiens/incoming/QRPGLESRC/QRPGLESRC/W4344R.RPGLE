     H*****************************************************************
     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H*****************************************************************
     H actgrp(*caller)
     H/TITLE W4344R - Lifetime Income Benefit Maintenance
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*  Description:
     H*
     H*  Lifetime Income Benefits Maintenance
     H* *****************************************************************
     H*  Called From:                                                   *
     H*    W0070R09                                                     *
    H*
     H* *****************************************************************
     H*  Created:   2/22/10  C:247  Project:  25WQC
     H*****************************************************************
     H* Revisions:                                                    *
    H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 02/01/14 C:190 RedMine#3834                                   *
    H*                Screen blowing up when Delete was hit twice.   *
    H*                                                               *
     H* 10/21/10 C:183 Project: 25WQC(Priss#36)                       *
     H*                Changed edit comparison for LIB < DOI to       *
     H*                LIB <= DOI.                                    *
    H*                                                               *
     H* 08/10/10 C:190 Project: 25WQC                                 *
    H*                Additional changes.                            *
    H*                                                               *
     H* 08/05/10 C:190 Project: 25WQC(Priss#41)                       *
    H*                Corrected issue with Total Calc. Benefit       *
    H*                                                               *
    H*                                                               *
     H* 07/02/10 C:190 Project: 25WQC(Priss#35, 36, 38)               *
     H*                Changed edit message for LIB calculate.        *
     H*                New edit for LIBS start date.                  *
     H*                F7 just to calculate, not to save values.      *
     H*                Enter will save values to database.            *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw4344f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F*                                    infds(fids01)
    F*
     Fwmlbl     uf a e           k disk
     Fwmcml1    uf   e           k disk
     Fwmupl     if   e           k disk
     Fwmfnl     if   e           k disk
     Fsmcol     if   e           k disk
     Fwmcdl     if   e           k disk
     Fwmeml     if   e           k disk
     Fwdrbl     if   e           k disk
    D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Lifetime Income Benefits'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W4344R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@call          s              1
     D xxco#           s                   like(cmco#)
     D xxfnd           s                   like(cmfnd)
     D xxemp#          s                   like(cmemp#)
     D xxdiv           s                   like(cmdiv)
     D xxfyr           s                   like(cmfyr)
     D xxcase          s                   like(cmcase)
     D xxjrst          s                   like(cmjrst)
     D xxefdt          s                   like(cmacdt)

     D ##lstd          s                   like(sclstd)
     D ##lper          s                   like(sclper)
     D ##lyrs          s                   like(sclyrs)
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D RRNM            S                   Like(SCRRN1)
     D SFRRNH          S                   Like(SCRRN1)
     D SFRRN1          S                   Like(SCRRN1)
     D xxlefdt         S                   Like(lbordt)
     D xxlyers         S                   Like(lbfyr)
      *
     D                 Ds
     Ddsdttm                   1     14  0
     Ddstime                   1      6  0
     Ddsdate                   7     14  0

      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      /copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    qqcall            1
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqfyr             3
     C                   parm                    qqcase            7
     C                   parm                    qqrtcd            1
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
      *
     C     upkey         klist
     C                   kfld                    q1user
      *
     C     fnkey         klist
     C                   kfld                    upco#
     C                   kfld                    xxfnd
      *
     C     cmkey         klist
     C                   kfld                    upco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxcase
      *
     C     lbkey         klist
     C                   kfld                    upco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxcase
     C                   kfld                    xxlefdt
     C                   kfld                    xxlyers
      *
     C     lbkey1        klist
     C                   kfld                    upco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxcase
      *
     C     rbky          klist
     C                   kfld                    upco#
     C                   kfld                    xxfnd
     C                   kfld                    xxjrst
     C                   kfld                    xxefdt
      *
     C     rbky1         klist
     C                   kfld                    upco#
     C                   kfld                    xxfnd
     C                   kfld                    xxjrst
      *
     C     emkey         klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C                   Movel     qqco#         xxco#
     C                   Movel     qqfnd         xxfnd
     C                   Movel     qqfyr         xxfyr
     C                   Movel     qqcase        xxcase
      * Get company name
     C     upkey         chain     pup01                              60
     c                   if        *in60 = *on
     c                   move      *zeros        upco#
     c                   endif
      *
     c     upco#         chain     pco01                              60
     c                   movel     coname        srdfld
     c                   call      'SRCENTER'
     C                   parm                    srdfld           80
     C                   parm      40            srdlen            3 0
     C                   Movel     srdfld        scconm
      *
     C     fnkey         chain     pfn01                              60
     c                   movel     xxfnd         scfnd
     c                   movel     fnname        scname
      *
     C                   Movel     optnm         ##data
     C                   Call      'SRCENTER'
     C                   Parm                    ##data           80
     C                   Parm      40            ##leng            3 0
     C                   Movel     ##data        scprds
      *
     C     cmkey         chain     pcm01                              60
     C     CMSS#         IFNE      *BLANK
      *****  Begin Chg  ***  06/09/20  **********************
     C*****SS1           CAT(P)    '-':0         SCSS#
     C*****SCSS#         CAT       SS2:0         SCSS#
     C*****SCSS#         CAT       '-':0         SCSS#
     C*****SCSS#         CAT       SS3:0         SCSS#
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-Free
      ******  End   Chg  ***  06/09/20  **********************
     C                   END
      *  DOI
     C                   Z-ADD     CMACDT        srdti
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
      *  FYR-CASE
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCL
     C     SCYRCL        CAT       XACASE:0      SCYRCL
      *
     C                   Movel     cmacl#        scscl#
      *
     C                   Movel     cmjrst        xxjrst
     C                   z-add     cmacdt        xxefdt
      *
     C     emkey         chain     pem01                              60
     C                   If        *in60 = *off
     C                   Call      'WA005R'
     C                   Parm                    xxemp#
     C                   Parm                    xxdiv
     C                   Parm                    emname
     C     scedn         Parm                    xxedn            80
     C                   Endif
      *
     C     cmss#         chain     pcd01                              60
     C                   If        *in60 = *off
     C                   Eval      sccnam = %trim(cdlsn) + ',' +
     C                                      %trim(cdfsn) + ' ' +
     C                                      %trim(cdmid)
     C                   Endif

      *
     C                   MOVE      CMAWW         SCAWW
      *
     c*** beg   dlt *** 08/10/10 **************************************
      *  calculated comp rate.
     C*****              Move      *zeros        SCCRT
     C*****rbky          setgt     prb01
     C*****rbky1         readpe    prb01                                  60
     C*****              if        rbcmpr > *zeros
     C*****rbcmpr        div(H)    100           xxcmpr            7 6
     C*****cmaww         mult(H)   xxcmpr        sccrt
     C*****              Endif
     c*** end   dlt *** 08/10/10 **************************************
      *
      *  COMP RATE CAN NOT BE LESS THAN THE MINIMUM
     C                   If        sccrt < rbmnrt
     C                   z-add     rbmnrt        sccrt
     C                   Endif
      *
      *  COMP RATE CAN NOT BE GREATER THAN THE MAXIMUM
     C                   If        sccrt > rbpacp
     C                   z-add     rbpacp        sccrt
     C                   Endif
      *
      *  COMP RATE CAN NOT BE LESS THAN THE MINIMUM
     C                   If        sccrt < rblhmn
     C                   z-add     rblhmn        sccrt
     C                   Endif
      *
      *  COMP RATE CAN NOT BE GREATER THAN THE MAXIMUM
     C                   If        sccrt > rblhmx
     C                             and rblhmx <> *zeros
     C                   z-add     rblhmx        sccrt
     C                   Endif
      *
     C                   TIME                    DSDTTM
     C                   If        cmlisd <> *zeros
     c                   z-add     cmlisd        srdti
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   Movel     SRDTO         sclstd
     C                   Endif
      *
     c*** beg   add *** 08/10/10 **************************************
     C     lbkey1        chain     plb01                              60
     c                   if        *in60 = *off
     c                   z-add     lbcrt         sccrt
     c                   endif
     c*** end   add *** 08/10/10 **************************************
     c                   Movel     cmlpct        sclper
     c                   z-add     cmtlba        sctlba
     c                   z-add     cmlyrs        sclyrs
     C                   z-add     cmtlba        sctlba
      *
     c                   Movel     cmlpct        ##lper
     c                   z-add     cmlyrs        ##lyrs
     C                   move      sclstd        ##lstd
      *
     C                   If        sclyrs = 0
     c                   Eval      sclyrs = 65
     C                   Endif
      *
     C                   Move      *blanks       Firstld           1
      *
     C                   EXSR      Clear_SF
     C     lbkey1        setll     wmlbl
     C                   Exsr      Load_Dbfile
     C                   Eval      Firstld = 'Y'
      *
     C                   Dow       Not *inKC and Not *inKL
      *
     C                   Write     SCR01
      *
     C                   If        sclyrs = sfrrn1
     C                   move      *on           *in60
     C                   Endif
      *
     C                   Exfmt     SCR01C
      *
     C                   Move      *blanks       validfl           1
     C                   Exsr      Error_chk
      *
     c*** beg   add *** 07/02/10 **************************************
     c*** beg   chg *** 02/01/14 **************************************
     C*************      If        *inKH = *on
     C                   If        *inKH = *on and scYear<>0
     c*** end   chg *** 02/01/14 **************************************
     C                   Exsr      Delete_rec
     C                   EXSR      Display_SR
     c                   eval      *in60 = *on
     C                   Exsr      Updwmcmp
     c                   endif
     c*** end   add *** 07/02/10 **************************************
     C                   If        *inKG = *on
     c*** beg   add *** 07/02/10 **************************************
     C                   Move      *blanks       validfl           1
     C                   Exsr      Error_chk
     C                   If        validfl <> 'N'
     c*** end   add *** 07/02/10 **************************************
     C                   Eval      ##lstd = sclstd
     C                   Eval      ##lper = sclper
     C                   Eval      ##lyrs = sclyrs
     C                   If        sclstd <> *zeros and
     C                             sclper <> *zeros and
     C                             sclyrs <> *zeros
     c*** beg   dlt *** 07/02/10 **************************************
     C**********         Exsr      Delete_rec
     c*** end   dlt *** 07/02/10 **************************************
     C                   EXSR      Display_SR
     c*** beg   dlt *** 07/02/10 **************************************
     C***********        Exsr      updfile
     c*** end   dlt *** 07/02/10 **************************************
     c*** beg   add *** 08/05/10 **************************************
     C                   z-add     *zeros        reccnt            3 0
     C                   Move      sclstd        dbyear            4 0
     C                   Movel     sclstd        dbmmdd            4 0
     C                   z-add     *zeros        ratincr           9 2
     C                   z-add     sccrt         dbwamt            9 2
     C                   z-add     *zeros        sctlba
     C                   Dow       reccnt < sclyrs
     C                   Movel     dbmmdd        dbeffdt           8 0
     C                   Move      dbyear        dbeffdt
     C                   Call      'SRCVTDAT'    dtcymd
     C                   Movel     srdto         xxeffdt           7 0
     C                   Add       1             reccnt
     C                   Eval      lbwrat=  dbwamt + ratincr
     C                   Eval      lbyrat=  lbwrat * 52
     C                   Eval      sctlba = sctlba + lbyrat
     C                   add       1             dbyear
     C                   Eval      dbwamt = lbwrat
     C                   Eval      ratincr = lbwrat * (sclper /100)
     C                   Enddo
     c*** end   add *** 08/05/10 **************************************
     C                   Eval      Firstld = 'N'
     C                   Endif
     C                   Endif
     c*** beg   add *** 07/02/10 **************************************
     C                   Endif
     c*** end   add *** 07/02/10 **************************************
      *
     c*** beg   chg *** 07/02/10 **************************************
     C                   If        *in70 = *on and @@flg = 'Y'
     c                   eval      *in70 = *off
     c                   eval      *in60 = *on
     c                   else
     C                   If        *in70 = *on
     C***********        If        *in70 = *on
     c*** end   chg *** 07/02/10 **************************************
     C                   If        Firstld = 'Y'
     C                   Exsr      Load_dbfile
     C                   Else
     C                   Exsr      Load_SF
     C                   Endif
     C                   Endif
     c*** beg   add *** 07/02/10 **************************************
     C                   Endif
     c*** end   add *** 07/02/10 **************************************
      *
     c                   Enddo

     C     PgmEnd        TAG
     C                   MOVE      '1'           *INLR
     c                   return

    C*================================================================
      * Validate screen input fields
    C*================================================================
     C     Error_chk     Begsr
     C                   Movea     '0000'        *in(27)
     c*** beg   add *** 07/02/10 **************************************
     C                   Movea     '0000'        *in(31)
     c*** end   add *** 07/02/10 **************************************
     C                   Move      *off          *in61
      *
     c*** beg   add *** 07/02/10 **************************************
     c*  LIB Start Date cannot be blank.
     C                   If        sclstd = *zeros
     C                   Eval      *in32 = *on
     C                   Eval      validfl = 'N'
     c                   Endif

     c*  LIB Percent cannot be blank.
     c                   If        sclper = *zeros
     C                   Eval      *in33 = *on
     C                   Eval      validfl = 'N'
     c                   Endif

     c*  LIB Number of Years cannot be blank.
     c                   If        sclyrs = *zeros
     C                   Eval      *in33 = *on
     C                   Eval      validfl = 'N'
     c                   Endif
     c*** end   add *** 07/02/10 **************************************
     C                   If        *inkg = *on
     C                   If        sclstd = *zeros
     C                   Eval      *in27 = *on
     C                   Eval      validfl = 'N'
     c                   Endif
      *
     C                   If        sclper = *zeros
     C                   Eval      *in30 = *on
     C                   Eval      validfl = 'N'
     C                   Endif
      *
     C                   If        sclyrs = *zeros
     C                   Eval      sclyrs = 65
     C                   Endif
      *
     C                   Endif
      *
     C                   If        sclstd <> *zeros
     C                   z-add     sclstd        srdti
     C                   Call      'SRCVTDAT'    DATCHK
     C                   Movel     srdto         sclstd
      *
     C                   If        srerr = *on
     C                   Eval      *in28 = *on
     C                   Eval      validfl = 'N'
     C                   Endif
     c*** beg   add *** 07/02/10 **************************************
     c* LIB Start Date cannot be less than the DOI.
     c                   z-add     sclstd        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxfrom            7 0

     c                   z-add     scacdt        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxto              7 0

     c*** Beg chg ***** 10/21/10 **************************************
      * Change the condition for comparison from LIB < DOI to
      * LIB <= DOI.
     c*****              If        xxfrom < xxto
     c                   If        xxfrom <= xxto
     c*** End chg ***** 10/21/10 **************************************
     c                   Eval      *in31 = *on
     c                   Eval      validfl = 'N'
     c                   Endif
     c*** end   add *** 07/02/10 **************************************
      *
     C                   Endif
      *
     C                   If        sclyrs <> *zeros and
     C                             sclyrs > 65
     C                   Eval      *in29 = *on
     C                   Eval      validfl = 'N'
     C                   Endif
      *
     C                   If        *inkg = *off and
     C                             *in70 = *off
     c*** beg   dlt *** 07/02/10 **************************************
     C********           If        sfrrn1 = 0
     C******             If        sclstd <> *zeros and
     C********                     sclper <> *zeros and
     C******                       sclyrs <> *zeros
     C********           eval      *in61 = *on
     C******             Endif
      ********
     C******             Else
     C********           If        sclstd <> ##lstd or
     C********                     sclper <> ##lper or
     C********                     sclyrs <> ##lyrs
     C********           eval      *in61 = *on
     C******             endif
     c*** end   dlt *** 07/02/10 **************************************
     c*** beg   add *** 07/02/10 **************************************
     c                   if         validfl <> 'N' and *inkl = *off
     C                              and  *inkh = *off
     C                   Exsr      Delete_rec
     C                   EXSR      Display_SR
     C                   Exsr      updfile
     C                   Exsr      Updwmcmp
     C                   goto      pgmend
     C                   endif
     c*** end   add *** 07/02/10 **************************************
     C                   endif
     C
     C                   Endsr
    C*================================================================
      * Delete records if any exists for the claim, before loading new
      * set
    C*================================================================
     C     Delete_rec    Begsr
     C     lbkey1        setll     wmlbl
     C     lbkey1        reade     wmlbl                                  60
     C                   Dow       *in60 = *off
     C                   Delete    plb01
     C     lbkey1        reade     wmlbl                                  60
     C                   Enddo
     C                   Endsr
    C*================================================================
      * Load and display subfile
    C*================================================================
     c     Display_SR    Begsr
     C                   EXSR      Clear_SF
      *
     C                   z-add     *zeros        lyrcnt            3 0
     C                   z-add     sclyrs        lyrcnt
     C                   Exsr      Load_SF
     C
     C                   If        sfrrn1 > 0
     C                   Move      '1'           *in04
     C                   Endif
      *
     c                   Endsr
    C*================================================================
     C     Clear_SF      Begsr
     C                   Move      '1'           *IN03
     C                   Move      '0'           *IN04
     C                   Move      *zeros        scyear
     C                   Move      *zeros        scefdt
     C
     C                   Write     SCR01C
     C                   Move      '0'           *IN03
     C                   Move      '0'           *IN04
     C                   Move      '0'           *IN26
     C                   Z-Add     0             sfrrn1
     C                   Z-Add     0             SFRRNH
     C                   Move      sclstd        ##efyr            4 0
     C                   Movel     sclstd        ##mmdd            4 0
     C                   z-add     *zeros        perincr           7 2
     C                   z-add     sccrt         yramt             7 2
     c                   Endsr
    C*================================================================
     C     Load_SF       Begsr
     C                   Eval      *in60 = *off
     C                   z-add     *zeros        sfrcnt            2 0
     c*** beg   add *** 07/02/10 **************************************
     c                   move      scyear        @@year            3 0
     c*** beg   dlt *** 08/05/10 **************************************
     C******             z-add     *zeros        sctlba
     c*** end   dlt *** 08/05/10 **************************************
     c*** end   add *** 07/02/10 **************************************
     c*** beg   chg *** 07/02/10 **************************************
     C                   Dow       @@year < lyrcnt
     C***********        Dow       scyear < lyrcnt
     c*** end   chg *** 07/02/10 **************************************
     C                   If        sfrcnt >= 10
     C                   Leave
     C                   Endif
      *
     C                   Dow       sfrcnt < 10
     c*** beg   chg *** 07/02/10 **************************************
     C                   If        @@year >= lyrcnt
     C**********         If        scyear >= lyrcnt
     c*** end   chg *** 07/02/10 **************************************
     C                   Eval      *in60 = *on
     C                   Leave
     C                   Endif
      *
     C                   Add       1             sfrrn1
     C                   add       1             sfrcnt
     c*** beg   add *** 07/02/10 **************************************
     c                   move      *blanks       @@flg             1
     c                   if        *inkh = *on
     c                   eval      ##mmdd = *zeros
     c                   eval      ##efyr = *zeros
     c                   eval      ##effdt = *zeros
     c                   eval      yramt = *zeros
     c                   eval      perincr = *zeros
     c                   MOVE      *blanks       scyear
     c                   eval      sclstd = *zeros
     c                   eval      sclper = *zeros
     c                   eval      sclyrs = 65
     c                   eval      @@flg  = 'Y'
     c                   else
     c*** end   add *** 07/02/10 **************************************
     C                   Movel     ##mmdd        ##effdt           8 0
     C                   Move      ##efyr        ##effdt
     C                   Move      ##effdt       scefdt
     c*** beg   add *** 07/02/10 **************************************
     C                   Endif
     c*** end   add *** 07/02/10 **************************************
     c*** beg   chg *** 07/02/10 **************************************
     C                   Add       1             @@year
     C********           Add       1             scyear
     c                   if        *inkh = *off
     C                   move      @@year        scyear
     c                   endif
     c*** end   chg *** 07/02/10 **************************************
     C                   Eval      scwrat = yramt + perincr
     C                   Eval      scyrat = scwrat * 52
     c*** beg   add *** 07/02/10 **************************************
     c*** beg   dlt *** 08/05/10 **************************************
     C*************      Eval      sctlba = sctlba + scyrat
     c*** end   dlt *** 08/05/10 **************************************
     c*** end   add *** 07/02/10 **************************************
     C
     C                   Write     SCR01S
     C                   Add       1             ##efyr
     C                   Eval      yramt = scwrat
     C                   Eval      perincr = scwrat * (sclper /100)
     C                   Enddo
     C                   Enddo
     C                   z-add     sfrrn1        scrrn1
      *
     C                   endsr
    C*================================================================
      * Load subfile from database file, if record exists
    C*================================================================
     C     Load_dbfile   Begsr
     C                   z-add     *zeros        sfrcnt            2 0
     C     lbkey1        reade     wmlbl                                  60
     C                   Dow       *in60 = *off
      *
     C                   If        sfrcnt <= 10
      *
     C                   Add       1             sfrrn1
     C                   add       1             sfrcnt
     c                   z-add     lbordt        srdti
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   Movel     SRDTO         scefdt
      *
     C                   z-add     lblyrs        scyear
     C                   z-add     lbwrat        scwrat
     C                   z-add     lbyrat        scyrat
     C                   Write     SCR01S
     C                   Endif
      *
     C                   If        sfrcnt >= 10
     C                   Leave
     C                   Endif
      *
     C     lbkey1        reade     wmlbl                                  60
     C                   Enddo
     C                   z-add     sfrrn1        scrrn1
      *
     C                   If        sfrrn1 > 0
     C                   Move      '1'           *in04
     C                   Endif
     C                   endsr
    C*================================================================
      * Update the database file
    C*================================================================
     C     Updfile       Begsr
     C                   z-add     scefdt        srdti
     C                   Call      'SRCVTDAT'    dtcymd
     C                   Movel     srdto         xxeffdt           7 0
     C                   z-add     xxeffdt       xxlefdt
     C                   z-add     scyear        ##years           3 0
     C                   z-add     *zeros        sctlba
      *
     C                   z-add     *zeros        reccnt            3 0
     C                   Move      sclstd        dbyear            4 0
     C                   Movel     sclstd        dbmmdd            4 0
     C                   z-add     *zeros        ratincr           9 2
     C                   z-add     sccrt         dbwamt            9 2
     c*** beg   add *** 08/10/10 **************************************
     C                   z-add     sccrt         lbcrt
     c*** end   add *** 08/10/10 **************************************
     C                   Dow       reccnt < sclyrs
     C                   Movel     dbmmdd        dbeffdt           8 0
     C                   Move      dbyear        dbeffdt
     C                   z-add     dbeffdt       srdti
     C                   Call      'SRCVTDAT'    dtcymd
     C                   Movel     srdto         xxeffdt           7 0
     C                   Add       1             reccnt
     C                   Eval      lbco# =  xxco#
     C                   Eval      lbfnd =  xxfnd
     C                   Eval      lbfyr =  xxfyr
     C                   Eval      lbcase=  xxcase
     C                   Eval      lblyrs=  reccnt
     C                   Eval      lbordt=  xxeffdt
     C                   Eval      lbwrat=  dbwamt + ratincr
     C                   Eval      lbyrat=  lbwrat * 52
     C                   Eval      lbiusr=  q1user
     C                   Eval      lbuser=  q1user
     C                   Eval      lbipgm=  q1pgm
     C                   Eval      lbpgm =  q1pgm
     C                   z-add     dsdate        srdti
     C                   Call      'SRCVTDAT'    dtcymd
     C                   Movel     srdto         lbidt
     C                   Movel     srdto         lbdt
     C                   Time                    lbitm
     C                   Time                    lbtm
     C                   Eval      sctlba = sctlba + lbyrat
     C                   Write     plb01
     C                   add       1             dbyear
     C                   Eval      dbwamt = lbwrat
     C                   Eval      ratincr = lbwrat * (sclper /100)
     C                   Enddo
     C                   endsr
     c*** beg   add *** 07/02/10 **************************************
      * Update claims master file with lifetime benefits start date,
      * percent and years
     C     Updwmcmp      Begsr
     C     cmkey         setll     pcm01
     C     cmkey         reade     pcm01                                  65
     C                   If        *in65 = *off
     C                   z-add     sclstd        srdti
     C                   Call      'SRCVTDAT'    dtcymd
     C                   Movel     srdto         cmlisd
      *
     C                   z-add     sclper        cmlpct
     C                   z-add     sclyrs        cmlyrs
     C                   z-add     sctlba        cmtlba
     C                   update    pcm01
     C                   Endif
     C                   endsr
     c*** end   add *** 07/02/10 **************************************
