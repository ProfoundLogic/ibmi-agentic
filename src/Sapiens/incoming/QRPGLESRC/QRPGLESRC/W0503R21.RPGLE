      *  (C) Copyright Progressive Data Solutions, Inc., 2003-2008
      *****************************************************************
      *  DESCRIPTION:  Checks to be Written Report Prompt.            *
      *                                                               *
      *  CREATED: 04/01/2003  JOHN STONE                              *
      *****************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      *  04/24/24 C:300 Project: WC2676                               *
      *                 This program is no longer in use.             *
      *                 Pass-through this program to new screen.      *
      *                 Add extra entry parm to avoid Pass-through of old screen
      *                                                               *
      *  07/18/07 C:136 Project: WC1491                               *
      *                 Implement Ind Cash, Return Deposit            *
      *                                                               *
      *  04/07/05 C:136 Support: WA093 46179                          *
      *                 Fix pgm to pass user parm outq.               *
      *****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0503F21  CF   E             Workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     *
     *  Company file used to get company name
     FSmcol     If   E           K Disk
     *
     *  User file used to get company number.
     FWmupl     If   E           K Disk
     *
      *  Worker's comp flags
     Fswcpf     if   e           k disk
     D Datusa          S               d   Datfmt(*Usa)
      *
     D                 DS
     D  Dsdttm                 1     12  0
     D  Dstime                 1      6  0
     D  Dsdate                 7     12  0
      *
     D PSDS          ESDS                  Extname(Szq1p)
     *================================================================
     *  KLIST definitions
     *================================================================
     C     upky          klist
     C                   kfld                    q1user
      *
      **** Begin Add **** 04/24/24 ******************************************
     *================================================================
     *  PLIST definitions
     *================================================================
    C     *entry        plist
    C                   parm                    @@PassThru        1
      **** End   Add **** 04/24/24 ******************************************
     *================================================================
     * Mainline
     *================================================================
      **** Begin Add **** 04/24/24 ******************************************
      * Do not Pass-Through to new screen, if parameter (@@PassThru) is passed
     c                   If        Q1PRMS < 1
     c                   Call      'WTRFCHKRPT'
      *
      /free
       *InLr = *On;
       Return;
      /end-free
     C                   Endif
      **** End Add ****** 04/24/24 ******************************************
     C                   Dou       *Inkc or
     C                             *Inkk
      *
     C                   Exfmt     Scr01
      *
     C                   If        *Inkc or
     C                             *Inkk
     C                   Leave
     C                   Endif
      *
     C                   Eval      *In30 = *Off
     C                   Exsr      Validate
      *
      *** beg chg ***** 07/18/07 **********************************************
     C*****************  If        *In30
     C                   If        *in90
      *** end chg ***** 07/18/07 **********************************************
     C                   Iter
     C                   Else
     C                   Exsr      Chk_Prompt
     C                   Leave
     C                   Endif
      *
     C                   Enddo
      *
     C                   Eval      *Inlr = *On
     *================================================================
     * Validate screen
     *================================================================
     C     Validate      Begsr
      *
      *** beg add ***** 07/18/07 ***************************************
     c                   setoff                                       717290
     c   36              if        scbtyp <> *blank and
     c                             scbtyp <> 'A' and
     c                             scbtyp <> 'D' and
     c                             scbtyp <> 'I' and
     c                             scbtyp <> 'P'
     c                   seton                                        71  90
     c                   endif

     c  n36              if        scbtyp <> *blank and
     c                             scbtyp <> 'A' and
     c                             scbtyp <> 'P'
     c                   seton                                        71  90
     c                   endif
      *
      *** end add ***** 07/18/07 ***************************************

      *** beg chg ***** 07/18/07 ***************************************
     C*****************  If        Schrb <> 'H' and
     C  n90              If        Schrb <> 'H' and
      *** end chg ***** 07/18/07 ***************************************
     C                             Schrb <> 'R' and
     C                             Schrb <> 'B'
      *** beg add ***** 07/18/07 ***************************************
     C*****************  Eval      *In30 = *On
     C                   seton                                        72  90
      *** end add ***** 07/18/07 ***************************************
     C                   Endif
      *
     C                   Endsr
     *================================================================
     * Inzsr - Initialize
     *================================================================
     C     *Inzsr        Begsr
      *
      * Display window if return premium module not installed
      *
     C                   read      swcpf
     C                   if        %eof or WCFL25 <> 'Y'
     C                   exfmt     rpinstwdw
     C                   seton                                        lr
     C                   return
     C                   endif

      *** beg add ***** 07/18/07 *******************************************************
     c                   if        WCFL36 = 'Y'
     c                   eval      *in36 = *on
     c                   endif
      *** end add ***** 07/18/07 *******************************************************

     C     upky          chain     pup01
     C     upco#         Chain     Pco01
     C                   If        %Found
     C                   Movel(p)  Coname        ##data           80
     C                   Call      'SRCENTER'
     C                   Parm                    ##data
     C                   Parm      40            ##leng            3 0
     C                   Movel     ##data        Scconm
     C                   Endif
      *
     C                   Endsr
     *================================================================
     * Return premium check print prompt
     *================================================================
     C     Chk_Prompt    Begsr
      *
     C                   move      upco#         @@co#
     C                   Call      'W0503C21'
     C                   Parm                    @@co#             3
     C                   Parm      Schrb         @@hrb             1
      *** beg chg ***** 04/7/05 ***************************************
     C*****************  Parm      upusnm        @@outq           10
     C                   Parm      UPOUTQ        @@outq           10
      *** end chg ***** 04/7/05 ***************************************
      *** beg add ***** 04/7/05 ***************************************
     C                   parm      scbtyp        @@btyp            1
      *** end add ***** 04/7/05 ***************************************
      *
     C                   Endsr
