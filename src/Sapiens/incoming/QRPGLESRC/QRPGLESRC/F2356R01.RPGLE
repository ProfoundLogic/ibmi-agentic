      *=========================================================================
     H* Copyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H* StoneRiver is a trademark of StoneRiver, Inc.')
      *=========================================================================
     H/TITLE - Work With IPPS Provider Specific File display screen            *
      *                                                                        *
      *  DESCRIPTION: This program Shows the W/W detail screen for FTPFP       *
      *                                                                        *
      *   CALLED BY: MENU                                                      *
      *                                                                        *
      *                                                                         *
      *                                                                        *
      *  CREATED: 06/12/2008  C:189 PROJECT TX039                              *
      *=========================================================================
      **************************************************************************
      * REVISIONS:                                                             *
     H*                                                                        *
     H*  10/30/15 C:309  Project: FS468                                        *
     H*                  Added 3 new fields on screen 2                        *
     H*                  1) Wage index by Provider ID                          *
     H*                  2) Wage index by CBSA                                 *
     H*                  3) Wage index by state code CBSA                      *
     H*                                                                        *
     H*  07/17/15 C:079  Project: FS466                                        *
     H*                  Map new field Uncompensated Care                      *
     H*                                                                        *
     H*  01/02/14 C:296  Project: FS426                                        *
     H*                  Map new fields                                        *
     H*                                                                        *
     H*  05/21/09 C:198  Project: FS343                                        *
     H*                   Expanded SCNPI# and added SCSCOD                     *
     H*                                                                        *
     H* 08/05/08 C:189 Project: TX039                                          *
     H*                Change PFEFDT To PFFFDT( Eff. Date Of First Period)     *
     H*                                                                        *
     H*                                                                        *
     H*                                                                        *
      **************************************************************************
    H*
     H DFTACTGRP(*NO)
     Ff2356f01  cf   e             workstn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
    F*
    F*  Physical file definition (for maintaining record)
     Fftpfp     uf a e             disk
     F                                     rename(ptp01:dbrcdf)
     F**** Beg Add *** 10/30/15 ***************************************
     FFTI2L1    IF   E           K Disk
     FFTA4L1    IF   E           K Disk
     FFTB4L1    IF   E           K Disk
     F**** End Add *** 10/30/15 ***************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'IPPS PROVIDER SPECIFIC FILE  '
     D comapi          c                   'FA007R'
     D #ofscr          c                   2
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@dtcvt         ds             8
     D @@yr                           4
     D @@mnth                         2
     D @@dt                           2
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     dtcymd1       plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
     C                   parm      '*MDYY  '     srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*** Beg Add ***** 10/30/15 ******************************************
     C     l2ky          klist
     C                   kfld                    kkprov#          10
     C                   kfld                    kkefdt            7 0
      *
     C     l2ky1         klist
     C                   kfld                    kkprov#
      *
     C     l4ky          klist
     C                   kfld                    kkCBSA            5
     C                   kfld                    kkefdt
      *
     C     l4ky1         klist
     C                   kfld                    kkCBSA
     C*** End Add ***** 10/30/15 ******************************************
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   movel     pfnpi#        scnpi#
     C                   movel     pfprov        scprov
     C                   movel     pfname        scname
     C*
     C                   z-add     pfefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         scefdt
     C*
     C                   Movel     pfbgdt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         scbgdt
     C*
     C                   z-add     pfrpdt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         scrpdt
     C*
     C                   z-add     pftrdt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         sctrdt
     C*
     C                   Movel     pfwaiv        scwaiv
     C                   Movel     pfint         scint
     C                   Movel     pftype        sctype
     C                   Movel     pfcens        sccens
     C                   Movel     pfcdwi        sccdwi
     C                   Movel     pfamsa        scamsa
     C                   Movel     pfwmsa        scwmsa
     C                   Movel     pfsmsa        scsmsa
     C                   Movel     pfsole        scsole
     C                   Movel     pflug         sclug
     C                   Movel     pfrel         screl
     C                   Movel     pffble        scfble
     C                   Movel     pfmix         scmix
     C                   Movel     pfmix1        scmix1
     C                   Movel     pfcola        sccola
     C                   Movel     pfibed        scibed
     C                   Movel     pfbeds        scbeds
     C                   Movel     pfoccr        scoccr
     C                   Movel     pfssi         scssi
     C                   Movel     pfmrat        scmrat
     C                   Movel     pfppsp        scppsp
     C                   Movel     pfspfa        scspfa
     C                   Movel     pfodsh        scodsh
     C                   Movel     pfsppy        scsppy
     C                   z-add     pfendt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         scendt
     c                   Movel     pfhoqu        schoqu
     c                   Movel     pfacbs        scacbs
     c                   Movel     pfwcbs        scwcbs
     c                   Movel     pfsalo        scsalo
     c                   Movel     pfspwi        scspwi
     c                   Movel     pfgaf         scgaf
     c                   Movel     pfwind        scwind
     c                   Movel     pfcpas        sccpas
     c                   Movel     pfdpas        scdpas
     c                   Movel     pfopas        scopas
     c                   Movel     pftpas        sctpas
     c                   Movel     pfcpps        sccpps
     c                   Movel     pfhsca        schsca
     c                   Movel     pfohhr        scohhr
     c                   Movel     pfchhr        scchhr
     c                   Movel     pfcccr        sccccr
     c                   Movel     pfnew         scnew
     c                   Movel     pfcime        sccime
      *** BEG CHG ***** 05/21/09 ******************************************
     c                   Movel     pfscod        scscod
      *** END CHG ***** 05/21/09 ******************************************
     c                   Movel     pfcexc        sccexc
     c                   Movel     pfstat        scstat
      *** BEG CHG ***** 08/05/08 ******************************************
     C                   z-add     pfffdt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         scffdt
      *** END CHG ***** 08/05/08 ******************************************
     C*** Beg Chg ***** 01/02/14 ******************************************
     C                   movel     pfvbpp        scvbpp
     C                   movel     pfvbpa        scvbpa
     C                   movel     pfhrri        schrri
     C                   movel     pfhrra        schrra
     C                   movel     pfm1bd        scm1bd
     C*** End Chg ***** 01/02/14 ******************************************
     C*** Beg Add ***** 07/17/15 ******************************************
     C                   Eval      scUncm = PFuncm
     C*** End Add ***** 07/17/15 ******************************************
     C*** Beg Add ***** 10/30/15 ******************************************
      /Free
          kkPROV# = PFPROV ;
          kkCBSA  = PFACBS ;
          kkefdt = %dec(%char(%date(PFefdt:*iso):*cymd0):7:0);

          //Populate Wage index by Provider ID
          Setgt l2ky PI201;
          Readpe l2ky1 PI201;
          If Not %Eof(FTI2L1);
            SCPVWI=I2CRWI;
          Endif;

          //Populate Wage index by CBSA
          Setgt l4ky PA401;
          Readpe l4ky1 PA401;
          If Not %Eof(FTA4L1);
            SCCBWI = A4CRWI;
          Endif;

          //Populate Wage index by state code CBSA
          Setgt l4ky PB401;
          Readpe l4ky1 PB401;
          If Not %Eof(FTB4L1);
            SCSTWI = B4CRWI;
          Endif;
      /End-Free
     C*** End Add ***** 10/30/15 ******************************************
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   movel     scnpi#        pfnpi#
     C                   movel     scprov        pfprov
     C                   movel     scname        pfname
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         pfefdt
     C                   z-add     scbgdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         pfbgdt
     C                   z-add     scrpdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         pfrpdt
     C                   z-add     sctrdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         pftrdt
     C                   Movel     scwaiv        pfwaiv
     C                   Movel     scint         pfint
     C                   Movel     sctype        pftype
     C                   Movel     sccens        pfcens
     C                   Movel     sccdwi        pfcdwi
     C                   Movel     scamsa        pfamsa
     C                   Movel     scwmsa        pfwmsa
     C                   Movel     scsmsa        pfsmsa
     C                   Movel     scsole        pfsole
     C                   Movel     sclug         pflug
     C                   Movel     screl         pfrel
     C                   Movel     scfble        pffble
     C                   Movel     scmix         pfmix
     C                   Movel     scmix1        pfmix1
     C                   Movel     sccola        pfcola
     C                   Movel     scibed        pfibed
     C                   Movel     scbeds        pfbeds
     C                   Movel     scoccr        pfoccr
     C                   Movel     scssi         pfssi
     C                   Movel     scmrat        pfmrat
     C                   Movel     scppsp        pfppsp
     C                   Movel     scspfa        pfspfa
     C                   Movel     scodsh        pfodsh
     C                   Movel     scsppy        pfsppy
     C                   z-add     scendt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         pfendt
     c                   Movel     schoqu        pfhoqu
     c                   Movel     scacbs        pfacbs
     c                   Movel     scwcbs        pfwcbs
     c                   Movel     scsalo        pfsalo
     c                   Movel     scspwi        pfspwi
     c                   Movel     scgaf         pfgaf
     c                   Movel     scwind        pfwind
     c                   Movel     sccpas        pfcpas
     c                   Movel     scdpas        pfdpas
     c                   Movel     scopas        pfopas
     c                   Movel     sctpas        pftpas
     c                   Movel     sccpps        pfcpps
     c                   Movel     schsca        pfhsca
     c                   Movel     scohhr        pfohhr
     c                   Movel     scchhr        pfchhr
     c                   Movel     sccccr        pfcccr
     c                   Movel     scnew         pfnew
     c                   Movel     sccime        pfcime
      *** BEG CHG ***** 05/21/09 ******************************************
     c                   Movel     scscod        pfscod
      *** END CHG ***** 05/21/09 ******************************************
     c                   Movel     sccexc        pfcexc
     C*** Beg Chg ***** 01/02/14 ******************************************
     C                   movel     scvbpp        pfvbpp
     C                   movel     scvbpa        pfvbpa
     C                   movel     schrri        pfhrri
     C                   movel     schrra        pfhrra
     C                   movel     scm1bd        pfm1bd
     C*** End Chg ***** 01/02/14 ******************************************
     C*** Beg Add ***** 07/17/15 ******************************************
     C                   Eval      PFUncm = scUncm
     C*** End Add ***** 07/17/15 ******************************************
     C*
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *inlr = *on
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      ftpfp
     C                   endsr
