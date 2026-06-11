     A*  (C) Copyright Fiserv, Inc., 2003 - 2008
     H/Title - Work With Return Fund Checks
      *****************************************************************
      *  DESCRIPTION:  Work with Return Fund Checks.                  *
      *                                                               *
      *  Called from: Menu                                            *
      *                                                               *
      *  Calls to: W0505R02 - Return Fund Check Informatin            *
      *                                                               *
      *  CREATED: 03/12/2003  C:136/C:142                             *
      *****************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      *  04/24/24 C:300 Project: WC2676                               *
      *                 This program is no longer in use.             *
      *                 Pass-through this program to new screen.      *
      *                 Add extra entry parm to avoid Pass-through of old screen
      *                                                               *
      *  09/25/08 C:136 Support: WA109 65336                          *
      *                 Fix display issues in screen file.            *
      *                                                               *
      *  12/26/07 C:183 Project: 81WZB                                *
      *                -Added subset for Surplus Dividend, Employer   *
      *                 # range, customer ID range.                   *
      *                                                               *
      *  07/18/07 C:136 PROJECT: WC1491                               *
      *                -IMPLEMENT IND CASH DIST, RETURN DEPOSIT       *
      *                -ADD LOGIC SO THAT THIS PROGRAM CAN BE CALLED  *
      *                 FROM W/W EMPLOYERS, W/W GROUPS, W/W AGENTS    *
      *                                                               *
      *  10/03/06  C:53  Project: 543293                              *
      *                  Add edit for voids/stops                     *
      *                                                               *
      *  05/24/05  C:33  Project: WC1033                              *
      *                  Include calculations for Agency Billing.     *
      *****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0505F    CF   E             Workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     Sfile(scr01s:sfrrn1)
     F                                     Sfile(scr02s:sfrrn2)
     F                                     Infds(fids01)
     *  Physical file definition (for refreshing subfile record)
     FWar4p     IF   E             Disk
     F                                     Rename(Pr401:Pfrcdf)
     *  Primary logical file definition (for displaying items)
     FWar4l6    IF A E           K Disk
     F                                     Infds(Dbfds)
     F                                     Rename(Pr401:Dbrcdf)
     *  Secondary logical files definition (for displaying items)
     FWar4l5    IF A E           K Disk
     F                                     Infds(Dbfds2)
     F                                     Rename(Pr401:Dbrcdf2)
      * Employer Detail
     Fwdell     IF   E           K Disk
     F                                     Infds(elinfds)
      * Employer Master
     Fwmeml     IF   E           K Disk
     F                                     Infds(eminfds)
     F***  Begin Add  ***  05/24/05  **************************************
     Fwsasl     if   e           k disk    rename(pas01:pas00)
     F                                     infds(asinfds)
     F***  End   Add  ***  05/24/05  **************************************
     Fswcpf     if   e           k Disk
     F*** beg add ***** 07/18/07 ******************************************
     Fwmfnp     if   e             disk
     Fwmemp     if   e             disk    rename(pem01 : pem01p)
     Fwmaap     if   e             disk
     F*** end add ***** 07/18/07 ******************************************
     *
     D eminfds         ds
     D  emrrn                397    400b 0
     D elinfds         ds
     D  elrrn                397    400b 0
     D***  Begin Add  ***  05/24/05  **************************************
     D asinfds         ds
     D  asrrn                397    400b 0
     D***  End   Add  ***  05/24/05  **************************************
     D hd              S             78    dim(4) ctdata perrcd(1)
     D***  Begin Chg  ***  05/24/05  **************************************
     D*of*******       S              1    dim(11) ctdata perrcd(1)
     D*ofd******       S             35    dim(11) alt(of)
     D #ofOpts         C                   15
     D of              S              1    dim(#ofOpts) ctdata perrcd(1)
     D ofd             S             35    dim(#ofOpts) alt(of)
     D***  End   Chg  ***  05/24/05  **************************************
     *================================================================
     *  /COPY required constants
     *================================================================
     D optnm           C                   'Return Fund Checks'
     D slmsgi          C                   'WC39998'
     D slmsgf          C                   'WMESGG'
     D acdpgm          C                   'W0505R02'
     D lo              C                   'abcdefghijklmnopqrs-
     D                                     tuvwxyz'
     D up              C                   'ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ'
     D comapi          C                   'WA001R'
     D uspapi          C                   'W0006R'
     D infomx          C                   1
     D viewmx          C                   2
     D sf1pag          C                   10
     D Datusa          S               d   Datfmt(*Usa)
     D Datmdy          S               d   Datfmt(*Mdy)
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##mode2         s              1
     D ##rrn           s              9  0
     D @@call          s              1
      *
     D Kkfnd           S                   Like(R4fnd)
     D Kkemp#          S                   Like(R4emp#)
     D Kkdiv           S                   Like(R4div)
     D Kkfyr           S                   Like(R4fyr)
     D Kkchk#          S                   Like(R4chk#)
     D Kkckdt          S                   Like(R4ckdt)
     D Kkacct          S                   Like(R4acct)
     D Kkptnm          S                   Like(R4ptnm)
     D Kkamt           S                   Like(R4amt)
     D Kkptfl          S                   Like(R4ptfl)
     D Kkvdsp          S                   Like(R4vdsp)
     D ssfdat7         S                   Like(R4ckdt)
     D sstdat7         S                   Like(R4ckdt)
     D xxckdt          S              8  0
      *** beg add ***** 07/18/07 *******************************************************
     D xxrrn           s              9  0
      *** end add ***** 07/18/07 *******************************************************
     D*** BEG ADD **** 12/26/07 ********************************************************
     D @R4CUST         S                   LIKE(R4CUST)
     D @SSCUS1         S                   LIKE(R4CUST)
     D @SSCUS2         S                   LIKE(R4CUST)
     D*** END ADD **** 12/26/07 ********************************************************
      *
     D                 DS
     D  Dsdttm                 1     12  0
     D  Dstime                 1      6  0
     D  Dsdate                 7     12  0
      *
     D                 DS
     D  View1                  1     74
     D  V1ptnm                 1     15
     D  V1ckdt                17     24
     D  V1chk#                26     32
      *** beg chg ***** 07/18/07 **************************************
     D**V1acct*********       34     53
     D**V1amt**********       55     69
     D**V1vdsp*********       73     73
     D  V1acct                34     46
     D  V1btyp                49     49
     D  V1amt                 52     66
     D  V1vdsp                69     69
      *** end chg ***** 07/18/07 **************************************
      *
     D                 DS
     D  View2                  1     74
     D  V2chk#                 1      7
      *** beg chg ***** 07/18/07 **************************************
     D**V2acct*********        9     28
     D**V2ckdt*********       30     37
     D**V2ptnm*********       39     53
     D**V2amt**********       55     69
     D**V2vdsp*********       73     73
     D  V2acct                 9     21
     D  V2ckdt                23     30
     D  V2ptnm                32     46
     D  V2btyp                49     49
     D  V2amt                 52     66
     D  V2vdsp                69     69
      *** end chg ***** 07/18/07 **************************************
      *
     C/COPY SCOPYWW01C
     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        plist
     C                   parm                    @@call
      *** beg add ***** 07/18/07 **************************************
    C                   parm                    @@rrn             9 0
      *** end add ***** 07/18/07 **************************************
      **** Begin Add **** 04/24/24 ******************************************
     C                   PARM                    @@PassThru        1
      **** End   Add **** 04/24/24 ******************************************
     *
     *  Add/change/display program parameter list
     *
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm                    @@outq           10
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
      *
     C     DTCYMD        Plist
     C                   Parm                    SRDTI
     C                   Parm      *Blanks       SRFFMT
     C                   Parm      '*CYMD'       SRTFMT
     C                   Parm                    SRDTO
     C                   Parm                    SRERR
      *
     C     DTMDY         Plist
     C                   Parm                    SRDTI
     C                   Parm      '*CYMD'       SRFFMT
     C                   Parm      '*MDY'        SRTFMT
     C                   Parm                    SRDTO
     C                   Parm                    SRERR
      *
     C     DTMDY1        Plist
     C                   Parm                    SRDTI
     C                   Parm      *Blanks       SRFFMT
     C                   Parm      '*MDY'        SRTFMT
     C                   Parm                    SRDTO
     C                   Parm                    SRERR
     *================================================================
     *  KLIST definitions
     *================================================================
     C     key1          klist
     C                   kfld                    ##co#
     C                   Kfld                    Kkptnm
     C                   Kfld                    Kkckdt
     C                   Kfld                    Kkchk#
     *
     C     key2          klist
     C                   kfld                    ##co#
     C                   Kfld                    Kkchk#
     C                   Kfld                    Kkacct
     C                   Kfld                    Kkckdt
     *
     C     War4l_Key     Klist
     C                   Kfld                    ##co#
     C                   Kfld                    Kkfnd
     C                   Kfld                    Kkptnm
     *
     C     War4l1_Key    Klist
     C                   Kfld                    ##co#
     C                   Kfld                    Kkfnd
     C                   Kfld                    Kkacct
     C                   Kfld                    Kkptnm
     *
     C     War4l2_Key    Klist
     C                   Kfld                    ##co#
     C                   Kfld                    Kkfnd
     C                   Kfld                    Kkacct
     *
     C     War4l4_Key    Klist
     C                   Kfld                    ##co#
     C                   Kfld                    Kkfnd
     C                   Kfld                    Kkemp#
     C                   Kfld                    Kkdiv
     C                   Kfld                    Kkfyr
     C                   Kfld                    Kkchk#
     *
     C     Wmealc_Key    Klist
     C                   Kfld                    ##co#
     C                   Kfld                    Kkfnd
     C                   Kfld                    Sffyr
     C                   Kfld                    Sfemp#
     C                   Kfld                    Sfdiv

     C     emky          klist
     C                   kfld                    R4CO#
     C                   kfld                    R4FND
     C                   kfld                    R4EMP#
     C                   kfld                    R4DIV
      *
     C     elky          klist
     C                   kfld                    R4CO#
     C                   kfld                    R4FND
     C                   kfld                    R4EMP#
     C                   kfld                    R4DIV
     C                   kfld                    R4FYR
      *
     *================================================================
     * CHKSSM - Set indicator 11 for subset message
     *================================================================
     C     chkssm        Begsr

     C                   if        ssacct <> *blanks or
     C                             ssfnd  <> *zeros  or
     C                             ssptfl <> *blanks or
     C                             ssemp# <> *zeros  or
     C                             ssdiv  <> *zeros  or
     C                             ssptnm <> *blanks or
     C                             ssvdsp <> *blank  or
     C                             ssfamt <> *zeros  or
     C                             sstamt <> *zeros  or
     C                             ssfdat <> *zeros  or
     C                             sstdat <> *zeros
     C                   seton                                        11
     C                   endif

     C                   Endsr
     *================================================================
     * ERRCK1 - Additional errors on subfile selection
     *================================================================
     C     errck1        Begsr
     C*** beg add *** 10/03/06 ******************************
     C                   if        scsel = '8' or
     C                             scsel = '9'
     C                   if        r4vdsp = 'V' or
     C                             r4vdsp = 'S'
     C                   move      *on           *in96
     C                   move      *on           *in90
     C                   endif
     C                   endif
     C*** end add *** 10/03/06 ******************************
     C                   Endsr
     *================================================================
     * ERRCK2 - Error checking for the subset screen
     *================================================================
     C     errck2        Begsr

     C                   if        ssfdat <> *zeros
     C                   eval      srdti = ssfdat
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         ssfdat
     C                   endif

     C                   if        sstdat <> *zeros
     C                   eval      srdti = sstdat
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         sstdat
     C                   endif
     C***  BEG ADD ******  12/26/07  **************************************
      *
      * Convert SSCUS1 and SSCUS2 to Upper case
      *
     C                   EVAL      @SSCUS1 = %XLATE(LO:UP:SSCUS1)
     C                   EVAL      @SSCUS2 = %XLATE(LO:UP:SSCUS2)
      *
     C                   SELECT
      *
     C                   WHEN      SSCUS1 <> *BLANKS AND SSCUS2 = *BLANKS
     C                   SETON                                        33  90
     C                   EVAL      *IN33 = *ON
      *
     C                   WHEN      SSCUS2 <> *BLANKS AND SSCUS1 = *BLANKS
     C                   SETON                                        32  90
      *
     C                   WHEN      @SSCUS2 < @SSCUS1
     C                   SETON                                        39  90
      *
     C                   WHEN      SSEMP1 <> *ZERO AND SSEMP2 = *ZERO
     C                   SETON                                        35  90
      *
     C                   WHEN      SSEMP2 <> *ZERO AND SSEMP1 = *ZERO
     C                   SETON                                        34  90
      *
     C                   WHEN      SSEMP2 < SSEMP1
     C                   SETON                                        40  90
      *
     C                   WHEN      SSAGT1 <> *ZERO AND SSAGT2 = *ZERO
     C                   SETON                                        37  90
      *
     C                   WHEN      SSAGT2 <> *ZERO AND SSAGT1 = *ZERO
     C                   SETON                                        36  90
      *
     C                   WHEN      SSAGT2 < SSAGT1
     C                   SETON                                        41  90
      *
     C                   WHEN      SSSDFL <> 'Y' AND SSSDFL <> 'N' AND
     C                             SSSDFL <> ' ' AND CLTID = '081'
     C                   SETON                                        38  90
      *
     C                   ENDSL
     C***  END ADD ******  12/26/07  **************************************

     C                   Endsr
     *================================================================
     * INZSST - Initialize the subset fields
     *================================================================
     C     inzsst        Begsr
     C                   Endsr
     *================================================================
     * KEYINZ - Initialize key values for program start
     *================================================================
     C     Keyinz        Begsr
     C                   Movel     *Loval        Kkptnm
     C                   Movel     *hival        Kkckdt
     C                   Movel     *hival        Kkchk#
     C                   Endsr
     *================================================================
     * KEYSET - Set key values when doing "position to"
     *================================================================
     C     Keyset        Begsr
      *
     C                   Select
      *
     C                   When      *in21
     C                   Movel(p)  Scpst1        KKptnm
     C                   eval      srdti = scpst1a
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         kkckdt
     c                   eval      kkchk# = scpst1b
      *
     C                   When      *in22
     C                   Move(p)   Scpst2        kkchk#
     C                   movel(p)  scpst2a       kkacct
     C                   eval      srdti = scpst2b
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         kkckdt
      *
     C                   Endsl
      *
     C                   Endsr
     *================================================================
     * KEYSFL - Set key values from subfile records for paging
     *================================================================
     C     Keysfl        Begsr
     C                   eval      kkacct = sfacct
     C                   eval      kkchk# = sfchk#
     C                   eval      kkckdt = sfckdt
     C                   eval      kkptnm = sfptnm
     C                   Endsr
     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     C     mvdbsc        begsr
      *
     C                   eval      Sfacct = R4acct
     C                   eval      Sfchk# = R4chk#
     C                   eval      sfptnm = r4ptnm
     c                   eval      sfckdt = r4ckdt
     C***  Begin Add  ***  05/24/05  **************************************
     C                   eval      sfagt# = R4AGT#
     C                   eval      sfstdt = R4STDT
     C                   eval      sfacfl = R4ACFL
     C***  End   Add  ***  05/24/05  **************************************
      *
     c                   select
     c                   when      *in21
     C                   eval      v1ptnm = r4ptnm
     C                   eval      srdti  = r4ckdt
     C                   call      'SRCVTDAT'    dtmdy
     C                   movel     srdto         xxckdt
     c                   eval      v1ckdt = %editc(xxckdt:'Y')
     C                   eval      V1chk# = %editc(R4chk#:'Z')
     C                   eval      v1acct = r4acct
     C                   eval      V1amt  = %editc(R4amt:'2')
     C                   eval      v1vdsp = r4vdsp
      *** beg add ***** 07/18/07 ***********************************************
     C                   eval      v1btyp = R4BTYP
      *** end add ***** 07/18/07 ***********************************************
     C                   eval      scdata = view1
      *
     c                   when      *in22
     C                   eval      V2chk# = %editc(R4chk#:'Z')
     c                   eval      v2acct = r4acct
     C                   eval      srdti  = r4ckdt
     C                   call      'SRCVTDAT'    dtmdy
     C                   movel     srdto         xxckdt
     c                   eval      v2ckdt = %editc(xxckdt:'Y')
     C                   eval      v2ptnm = r4ptnm
     C                   eval      V2amt  = %editc(R4amt:'2')
     C                   eval      v2vdsp = r4vdsp
      *** beg add ***** 07/18/07 ***********************************************
     C                   eval      v2btyp = R4BTYP
      *** end add ***** 07/18/07 ***********************************************
     C                   eval      scdata = view2
     C                   endsl
      *
     C                   endsr
     *================================================================
     * Option 6 - Call W/W Employers
     *================================================================
     C     opt6          begsr

     C     scdbrr        chain     pfrcdf
     C     emky          chain     pem01
     C                   if        %found
     C                   call      'W0050R'
     C                   parm                    ##kc              1
     C                   parm                    ##kl              1
     C                   parm      '1'           ##call            1
     C                   parm      *zero         ##rrn             9 0
     C                   parm      *zero         ##rrn2            9 0
     C                   parm                    ##qte             1
     C                   parm                    ##view            1 0
     C                   parm      emrrn         ##rrn3            9 0
     C                   endif

     C                   endsr
     *================================================================
     * Option 60 processing
     *================================================================
     C     Opt60         begsr

     C     scdbrr        chain     pfrcdf
     C     elky          chain     pel01
     C                   if        %found
     C                   move(p)   R4CO#         #1co#
     C                   move(p)   R4FND         #1fnd
     C                   Call      'W0612R'
     C                   parm                    ##kc
     C                   parm                    ##kl
     C                   parm      '1'           ##call
     C                   parm                    #1co#             3
     C                   parm                    #1fnd             3
     C                   parm                    ##emp#            9
     C                   parm                    ##div             5
     C                   parm                    ##fyr             3
     C                   parm                    ##date            8
     C                   parm                    ##amount          9
     C                   parm                    ##chk#            7
     C                   parm                    ##rtcd            1
     C                   parm      elrrn         ##rrn
     C                   endif

     C                   endsr
     C*
     C***  Begin Add  ***  05/24/05  **************************************
    C*====================================================================
    C* Option 61 processing
    C*====================================================================
     C     opt61         begsr
     C*
     C     asky          klist
     C                   kfld                    ##co#
     C                   kfld                    sfstdt
     C                   kfld                    sfagt#
     C*
     C     asky          chain     pas00
     C*
     C                   if        %found
     C*
     C                   call      'W3103R'
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      '1'           ##call
     C                   parm      asrrn         ##rrn
     C*
     C                   endif
     C*
     C                   endsr
     C***  End   Add  ***  05/24/05  **************************************
     C*
     *================================================================
     * OTHFKS - Process other function keys
     *================================================================
     C     Othfks        Begsr
     C                   Endsr
     *================================================================
     * OTHOPT - Process other options
     *================================================================
     C     othopt        begsr

     C     scsel         caseq     '6'           opt6
     c     scsel         caseq     '8'           void
     c     scsel         caseq     '9'           stop
     C     scsel         caseq     '60'          opt60
     C***  Begin Add  ***  05/24/05  **************************************
     C     scsel         caseq     '61'          opt61
     C***  End   Add  ***  05/24/05  **************************************
     c                   endcs

     C                   endsr
     *================================================================
     * Void Payment Subroutine
     *================================================================
     C     void          begsr
      *
     C                   call      'W0505R02'
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      'V'           ##mode
     C                   parm      Scdbrr        ##rrn
     C                   parm                    @@outq           10
      *
     C                   endsr
     *================================================================
     * Stop Payment Subroutine
     *================================================================
     C     stop          begsr
      *
     C                   call      'W0505R02'
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      'S'           ##mode
     C                   parm      Scdbrr        ##rrn
     C                   parm                    @@outq           10
      *
     C                   endsr
     *================================================================
     * PGMEND - End of program processing
     *================================================================
     C     pgmend        begsr
     C                   seton                                        lr
     C                   endsr
     *================================================================
     * PGMSTR - Start of program processing
     *================================================================
     C     pgmstr        begsr
      **** Begin Add **** 04/24/24 ******************************************
      * Do not Pass-Through to new screen, if 3rd parameter (@@PassThru) is passed
     c                   If        Q1PRMS < 3
     c                   Call      'WTRFCHK'
     c                   parm                    @@rrn2            9            tab index
     c                   parm                    @@mode            1            tab index
     c                   parm                    @@next           10            tab index
     c                   parm                    @@TabInd          5            tab index
      *
      /free
       *InLr = *On;
       Return;
      /end-free
     C                   Endif
      **** End Add ****** 04/24/24 ******************************************
      *
      * Display window if return premium module not installed
      *
     C                   read      swcpf
     C                   if        %eof or WCFL25 <> 'Y'
     C                   exfmt     rpinstwdw
     C                   seton                                        lr
     C                   return
     C                   endif

      *** beg add ***** 07/18/07 **************************************
     c                   if        WCFL36 = 'Y'
     c                   eval      *in36  = *on
     c                   endif

     c                   if        Q1PRMS > 1
     c                   eval      xxrrn  = @@rrn
     c                   select
     c                   when      @@call = 'G'
     c     xxrrn         chain     wmfnp
     c                   eval      ssfnd  = fnfnd
     c                   eval      ssptfl = 'G'
     c                   when      @@call = 'E'
     c     xxrrn         chain     wmemp
     c                   eval      ssfnd  = emfnd
     c                   eval      ssemp# = ememp#
     c                   eval      ssdiv  = emdiv
     c                   eval      ssptfl = 'E'
     c                   when      @@call = 'A'
     c     xxrrn         chain     wmaap
     c                   eval      ssfein = aafein
     c                   eval      ssmod  = aamod
     c                   eval      ssptfl = 'A'
     c                   endsl
     c                   if        @@call = 'G' or
     c                             @@call = 'A' or
     c                             @@call = 'E'
     C                   move      '1'           xxopfn
     C                   move      '06'          xxnmbr
     C                   move      'NNN'         xxmis
     C                   exsr      updofa
     C                   move      '1'           xxopfn
     C                   move      '08'          xxnmbr
     C                   move      'NNN'         xxmis
     C                   exsr      updofa
     C                   move      '1'           xxopfn
     C                   move      '09'          xxnmbr
     C                   move      'NNN'         xxmis
     C                   exsr      updofa
     c                   eval      @@call = ' '
     c                   endif
     c                   endif
      *** end add ***** 07/18/07 **************************************
     C*** BEG ADD ***** 12/26/07 ******************************************
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C                   IF        CLTID = '081'
     C                   EVAL      *IN31 = *ON
     C                   ENDIF                                                  CLTID = '081'
      *
     C*** END ADD ***** 12/26/07 ******************************************

     C                   endsr
     *================================================================
     * READ   - Read next record from file
     *================================================================
     C     Read          Begsr
      *
     C                   If        *In21 = *On
     C     ##co#         Reade     Dbrcdf                                 95
     C                   Endif
      *
     C                   If        *In22 = *On
     C     ##co#         Reade     Dbrcdf2                                95
     C                   Endif
      *
     C                   Endsr
     *================================================================
     * READP  - Read previous record from file
     *================================================================
     C     Readp         Begsr
      *
     C                   If        *In21 = *On
     C     ##co#         Readpe    Dbrcdf                                 95
     C                   Endif
      *
     C                   If        *In22 = *On
     C     ##co#         Readpe    Dbrcdf2                                95
     C                   Endif
      *
     C                   Endsr
     *================================================================
     * SETGT  - Set greater than on file
     *================================================================
     C     Setgt         Begsr
      *
     C                   If        *In21 = *on
     C     Key1          Setgt     Dbrcdf                             96
     C                   Endif
      *
     C                   If        *In22 = *on
     C     Key2          Setgt     Dbrcdf2                            96
     C                   Endif
      *
     C                   Endsr
     *================================================================
     * SETLL  - Set lower limits on file
     *================================================================
     C     Setll         Begsr
      *
     C                   If        *In21 = *On
     C     Key1          Setll     Dbrcdf                             96
     C                   Endif
      *
      *
     C                   If        *In22 = *On
     C     Key2          Setll     Dbrcdf2                            96
     C                   Endif
      *
      *
     C                   Endsr
     *================================================================
     * SF1UPD - Update subfile record when item is deleted
     *================================================================
     C     sf1upd        Begsr
      *                  eval      scname = '***Deleted***'
     C                   Endsr
     *================================================================
     * SLCTRD - Move fields for selected record to parameters
     *================================================================
     C     slctrd        Begsr
     C                   Endsr
     *================================================================
     * Add 'RP' as a valid application type.e put in subfile
     *================================================================
     C     Subset        Begsr
      *
     c                   if        r4chk# = 0
     c                   eval      meetss = *off
     c                   endif
      *
     C                   If        SSacct <> *Blanks and
     C                             SSacct <> R4acct
     C                   Eval      Meetss = *Off
     C                   Endif                                              ROS
      *
     C                   If        SSfnd <> *Zeros and
     C                             SSfnd <> R4fnd
     C                   Eval      Meetss = *Off
     C                   Endif                                              ROS
      *
      *** beg add ***** 07/18/07 *************************************
     C                   if        ssbtyp <> *blanks and
     c                             ssbtyp <> R4BTYP
     c                   eval      meetss = *off
     c                   endif
      *** end add ***** 07/18/07 *************************************

     C                   If        SSptfl <> *Blanks and
     C                             SSptfl <> R4ptfl
     C                   Eval      Meetss = *Off
     C                   Endif                                              ROS
      *
     C                   If        SSemp# <> *Zeros and
     C                             SSemp# <> R4emp#
     C                   Eval      Meetss = *Off
     C                   Endif                                              ROS
      *
     C                   If        SSdiv <> *Zeros and
     C                             SSdiv <> R4div
     C                   Eval      Meetss = *Off
     C                   Endif                                              ROS
      *
     C                   If        SSfein <> *Zeros and
     C                             SSfein <> R4fein
     C                   Eval      Meetss = *Off
     C                   Endif                                              ROS
      *
     C                   If        SSmod <> *Zeros and
     C                             SSmod <> R4mod
     C                   Eval      Meetss = *Off
     C                   Endif                                              ROS
      *
      *  CHECK PAYEE AGAINST SEARCH STRING
      *
     C                   If        SSptnm <> *Blanks
     C     LO:UP         Xlate(p)  R4ptnm        U1pdsc           40
     C     LO:UP         Xlate(p)  SSptnm        U2pdsc           40
     C                   Exsr      String
     C                   Endif
      *
     C                   If        (SSvdsp = 'V' and R4vdsp <> 'V')
     C                   Eval      Meetss = *Off
     C                   Endif                                              ROS
      *
     C                   If        (SSvdsp = 'S' and R4vdsp <> 'S')
     C                   Eval      Meetss = *Off
     C                   Endif                                              ROS
      *
      *
     C                   If        ((SSvdsp = 'B' and R4vdsp <> 'V') and
     C                             (SSvdsp = 'B' and R4vdsp <> 'S'))
     C                   Eval      Meetss = *Off
     C                   Endif                                              ROS
      *
     C                   If        SSfamt <> *Zeros and
     C                             R4amt  <  SSfamt
     C                   Eval      Meetss = *Off
     C                   Endif                                              ROS
      *
     C                   If        SStamt <> *Zeros and
     C                             R4amt   > SStamt
     C                   Eval      Meetss = *Off
     C                   Endif                                              ROS
      *
     C                   if        ssfdat <> *zeros
     C                   eval      srdti = ssfdat
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ssfdat7

     C                   if        r4ckdt < ssfdat7
     C                   eval      meetss = *off
     C                   endif

     c                   endif

     C                   if        sstdat <> *zeros
     C                   eval      srdti = sstdat
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         sstdat7

     C                   if        r4ckdt > sstdat7
     C                   eval      meetss = *off
     C                   endif

     c                   endif
     C***  BEG ADD  *****  12/26/07  **************************************
      *
     C                   IF        SSAGT1 <> *ZERO AND SSAGT2 <> *ZERO
     C                                           AND
     C                             (R4AGT# < SSAGT1 OR R4AGT# > SSAGT2)
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF                                                  SSAGT1 <> *ZERO
      *
     C                   IF        SSEMP1 <> *ZERO AND SSEMP2 <> *ZERO
     C                                           AND
     C                             (R4EMP# < SSEMP1 OR R4EMP# > SSEMP2)
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF                                                  SSEMP1 <> *ZERO
      *
     C                   IF        SSCUS1 <> *BLANKS AND SSCUS2 <> *BLANKS
     C                   EVAL      @R4CUST = %XLATE(LO:UP:R4CUST)
     C
     C                   IF        @R4CUST < @SSCUS1 OR @R4CUST > @SSCUS2
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF                                                  @R4CUST < SSCUS1
      *
     C                   ENDIF                                                  SSCUS1 <> *BLANKS
      *
     C                   IF        CLTID = '081'
      *
     C                   IF        R4SDFL = *BLANK
     C                   MOVE      'N'           @R4SDFL           1
     C                   ELSE
     C                   MOVE      R4SDFL        @R4SDFL           1
     C                   ENDIF                                                  R4SDFL = *BLANK
      *
     C                   IF        (SSSDFL <> *BLANK AND
     C                             SSSDFL <> @R4SDFL)
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF                                                  (SSSDFL <> *BLANK
     C                   ENDIF                                                  CLTID = '081'
     C***  END ADD  *****  12/26/07  **************************************

     C                   Endsr
      *===============================================================
      * STRING - SUBROUTINE TO SEARCH STRINGS
      *===============================================================
     C     String        Begsr
      *
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      40            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
      *
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   Endsr
      *===============================================================
** Column headings
Opt Payee Name      Chk Date  Check# Bank Account  Typ      Check Amt  V/S
Opt  Check# Bank Account  Chk Date Payee Name      Typ      Check Amt  V/S

** Description--------------MISN
1055=Display                YYY
1066=Employer Info          YYN
1088=Void                   YNN
1099=Stop pay               YNN
16060=A\R Inquiry           YYN
16161=Acct Curr Stmt        YYN                            05/24/05
203F3=Exit                  YYN
205F5=Refresh               YYY
211F11=Change View          YYN
212F12=Cancel               NNN
217F17=Subset               YYN
3
* * * Future Use
* * * Future Use
* * * Future Use
