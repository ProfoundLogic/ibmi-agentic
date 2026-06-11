     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H**************************************************************************************
     H* H/TITLE - DRG calculation display program                                          *
     H*                                                                                    *
     H* DESCRIPTION: This pgm shows the detailed DRG calculation derived from FTPFP.       *
     H*                                                                                    *
     H*                                                                                    *
     H* CALLED BY: F0363R                                                                  *
     H*                                                                                    *
     H* CALLS TO :                                                                         *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created: 06/12/08   Programmer C:189   Project:TX039                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H* 10/31/18 C:274 Project: FS535
     H*                Add HAC Adjustment                                                  *
     H*                                                                                    *
     H* 06/15/15 C:079 Project: FS466
     H*                Half adjust the total                                               *
     H*                                                                                    *
     H* 11/13/14 C:212 Project: FS448
     H*                Added new field - SCUNCOMP on screen (SCR01) and                    *
     H*                related processing                                                  *
     H*                                                                                    *
     H* 06/04/14 C:296 Support: 85375                                                      *
     H*                1) For date greater than 01012012, do not divide state              *
     H*                   multiplier by 100.                                               *
     H*                2) Remove hardcoding for state multiplier and check in file         *
     H*                   FTRTP for state multiplier.                                      *
     H*                                                                                    *
     H* 01/30/14 C:079 Project: FS426
     H*                Adding four src new fields and polulating value.                    *
     H*                                                                                    *
     H* 01/02/14 C:296 Project: FS426
     H*                Adding four src new fields and polulating value.                    *
     H*                1) New tech amt (SCNTEC)                                            *
     H*                2) Value Based Purchase Adjustment (SCVBPA)                         *
     H*                3) Readmit Adjustment (SCHRRA)                                      *
     H*                4) Low volume Adjustment (SCLVOL)                                   *
     H*                                                                                    *
     H* 03/02/11 C:227 Support: 75991                                                      *
     H*                Enable the functionality of enter key on screen                     *
     H*                                                                                    *
     H* 12/07/10 C:251 Project: SC018                                                      *
     H*                Changed Logical to pick corrcet Pass Through Amount (same as        *
     H*                in FA756R while calculations)                                       *
     H*                                                                                    *
     H* 10/01/10 C:198 Project: WV009                                                      *
     H*                Displayed state multiplier from ftrtp where medicare state          *
     H*                processing is on                                                    *
     H*                                                                                    *
     H* 09/06/10 C:220 Project: FS365                                                      *
     H*                Load SCWIND from EOWIND on and after 10/01/2010                     *
     H*                                                                                    *
     H* 08/12/10 C:198 Project: 114MWS                                                     *
     H*                Hid State multiplier and otal adjusted pmnt for medicare PPO        *
     H*                processing.                                                         *
     H*                                                                                    *
    H* 11/09/09 C:220 Project: FS345                                                      *
     H*                Medicare 2009 Q2 Update                                             *
     H*                                                                                    *
     H* 10/22/08 C:189 Support: 65387                                                      *
     H*                Replace 'DRG' with '100' as its causing an EDI error.Since CACPT4   *
     H*                is now blank so use CACODE to access the DRG line item              *
     H*                                                                                    *
     H* 10/06/08 C:189 PRTSK : 28 Project TX039                                            *
     H*                Total is being incorrectly displayed                                *
     H*                                                                                    *
     H* 09/23/08 C:189 Support: 65151                                                      *
     H*                Implants 'I' will be multiplied by 143%                             *
     H*                                                                                    *
     H*                                                                                    *
     H**************************************************************************************
     Ff0364f    cf   e             workstn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     Ffaeul     if   e           K DISK
     *** BEG CHG *** 12/07/10 **********************************************
     F*******ftpfl3    if   e           K DISK
     Fftpfl     if   e           K DISK
     *** END CHG *** 12/07/10 **********************************************
     Fftdgl     if   e           k DISK
     *** BEG CHG *** 10/22/08 **********************************************
     F***************fdcal18   if   e           k DISK
     Ffdcalw    if   e           k DISK
     *** END CHG *** 10/22/08 **********************************************
     Ffaeolg    if   e           k DISK
     Fstabl     if   e           k DISK
     F*** Beg Add *** 10/01/10 *********************************************
     Fftrtl2    if   e           k disk
     F*** End Add *** 10/01/10 *********************************************
      *
     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     *** BEG CHG *** 10/22/08 **********************************************
     D***************** ##cpt4          s              5    INZ('DRG')
     D ##CODE          s             11    INZ('100   ')
     *** END CHG *** 10/22/08 **********************************************
     D OPTNM           C                   CONST('IPPS Payment Information')
     D*** Beg Add *** 10/01/10 *********************************************
     D XXMFS           s                   like(stmfs)
     D*** End Add *** 10/01/10 *********************************************
     D
      *
     DPSDS           ESDS                  Extname(SZQ1P)
      *
      *                                            I
     C*
     C*
     C     *entry        plist
     C                   parm                    ##CO#             3 0
     C                   parm                    ##SUB#            5 0
     C                   parm                    ##FND             3 0
     C                   parm                    ##FYR             3 0
     C                   parm                    ##CASE            7 0
     C                   parm                    ##EOB#            9 0
     C                   parm                    ##conm           40
     C*
     C     euky          klist
     C                   kfld                    ##CO#
     C                   kfld                    ##SUB#
     C                   kfld                    ##FND
     C                   kfld                    ##FYR
     C                   kfld                    ##CASE
     C                   kfld                    ##EOB#
     C*
     C     ipky          klist
     C                   kfld                    @@prov#
     C                   kfld                    @@eudt#
      *
     C     ipky#         klist
     C                   kfld                    @@prov#
     C*
     C     dgky          klist
     C                   kfld                    @@drg#
     C*
     C     fdky          klist
     C                   kfld                    ##CO#
     C                   kfld                    ##SUB#
     C                   kfld                    ##FND
     C                   kfld                    ##FYR
     C                   kfld                    ##CASE
     C                   kfld                    ##EOB#
     *** BEG CHG *** 10/22/08 **********************************************
     C****************   kfld                    ##CPT4
     C                   kfld                    ##CODE
     *** END CHG *** 10/22/08 **********************************************
     C*
     C     stky          klist
     C                   kfld                    @@ast
     C*
     C*** Beg Add *** 10/01/10 ***************************************
     C     RTKY          KLIST
     C                   KFLD                    @@st              2
     C                   KFLD                    @@type            1
     C                   KFLD                    @@efdt            7 0
     C*
     C     RTKY1         KLIST
     C                   KFLD                    @@st              2
     C                   KFLD                    @@type            1
     C*
     C*** End Add *** 10/01/10 ***************************************
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *Blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtcymd1       PLIST
     C                   parm                    srdti             8 0
     C                   parm      '*YYMD'       srffmt            8
     C                   parm      *Blanks       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     *
     C     dtcymd2       plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C
     C*----------------------------------------------------------------------
     C*  MAINLINE CODE
     C*----------------------------------------------------------------------
     C*
     C     euky          chain     peu01                              99
     C                   if        *in99 = *off
     C                   Z-add     eudrg#        @@drg#            5 0
     C                   movel     eumcar        @@prov#           6
     C*
     C                   Z-Add     euaddt        srdti
     C                   call      'SRCVTDAT'    dtcymd2
     C                   Movel     srdto         @@eudt#           8 0
     C*
     C     ipky          setgt     ptp01                              99
     C     ipky#         readpe    ptp01
     C*
     C     dgky          chain     pdg01                              99
     C*
     C                   exsr      pgmstr
     C                   exsr      mvdbsc
     C                   exfmt     Scr01
     C                   endif
     C*
     C*** BEG DEL *** 03/02/11 ****************************************
     C***********        if        *inkc = *on
     C***********        move      *on           *inlr
     C***********        endif
     C***********        if        *inkl = *on
     C***********        move      *on           *inlr
     C***********        endif
     C*** END DEL *** 03/02/11 ****************************************
     C*** BEG ADD *** 03/02/11 ****************************************
     C                   move      *on           *inlr
     C*** END ADD *** 03/02/11 ****************************************
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*          Get FAEOP
     C*          ------------------------------------------------------
     C     EUKY          CHAIN     PEO01                              99
     C                   if        *in99 = *off
     C                   EVAL      SCAST = EOAST
     C                   MOVEL     EOAST         @@AST            10
     C*** Beg Add *** 08/12/10 ****************************************
     C                   MOVEL     EODSTY        @@DSTY            2
     C                   if        eodsty = 'DS'
     C                   eval      *in71 = *off
     C                   else
     C                   eval      *in71 = *on
     C                   endif
     C*** End Add *** 08/12/10 *******************************************
     C     STKY          CHAIN     PST01                              99
     C                   if        *in99 = *off
     C                   MOVEL(p)  STNAME        SCSTDS
     C*** Beg Add *** 10/01/10 ******************************************
     C                   movel     stmfs         xxmfs
     C*** End Add *** 10/01/10 ******************************************
     C                   endif
     C                   endif
     C*
     C*          Get from FAEUP
     C*          ------------------------------------------------------
     C                   EVAL      SCDRG   =     EUDRG#
     C                   EVAL      SCDGDS  =     EUDGDS
     C                   EVAL      SCPROV  =     EUMCAR
     C                   Z-Add     EUADDT        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   MOVEL     SRDTO         SCADDT
     C*
     C*          Get FTPFP
     C*          ------------------------------------------------------
     C                   Z-Add     PFEFDT        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   EVAL      SCWIND  =     PFWIND
     *** BEG CHG *** 09/06/10 **********************************************
     C     EUDCDT        IFGE      1091001
     C                   EVAL      SCWIND  =     EOWIND
     C                   ENDIF
     *** END CHG *** 09/06/10 **********************************************
     C     PFSPPY        IFEQ      'Y'
     C     PFSPPY        OREQ      '2'
     C                   EVAL      SCCBSA  =     PFWCBS
     C                   ELSE
     C                   EVAL      SCCBSA  =     PFACBS
     C                   ENDIF
     C*          Get FDCAP
     C*          ------------------------------------------------------
     *** BEG CHG *** 10/22/08 **********************************************
     C**************     fdky          Chain     FDCAl18                            60
     C     fdky          Chain     FDCAlW                             60
     *** END CHG *** 10/22/08 **********************************************
     C     *in60         IFEQ      *off
     C                   Z-add     CABOPR        SCBOPR
     C*** BEG ADD ********** 11/09/09 ************************************
     C                   Z-add     CAHSPB        SCHSPB
     C*** END ADD ********** 11/09/09 ************************************
     C                   Z-add     CABCAP        SCBCAP
     C                   Z-add     CAOIME        SCOIME
     C                   Z-add     CACIME        SCCIME
     C                   Z-add     CAODSH        SCODSH
     C                   Z-add     CACDSH        SCCDSH
     C                   Z-add     CAOUTL        SCOUTL
     C                   MOVE      *ZEROS        XXTDRG            9 2
     C***** Beg add ***** 01/02/14 ******************************
     C                   eval      SCNTEC = CANTEC
     C                   eval      SCVBPA = CAVBPA
     C                   eval      SCHRRA = CAHRRA
     C                   eval      SCLVOL = CALVOL
     C***** End add ***** 01/02/14 ******************************
     C***** Beg add ***** 11/13/14 ******************************
     C                   eval      SCUNCOMP = CAUNCOMP
     C***** End add ***** 11/13/14 ******************************
     C***** Beg add ***** 10/31/18 ******************************
     C                   eval      SCHAC = CAHAC
     C***** End add ***** 10/31/18 ******************************
     C*** BEG CHG ********** 10/06/08 ************************************
     C**************     EVAL      XXTDRG=CABOPR+CABCAP+CAOIME+CAODSH+CACDSH+
     C***************              +CAOUTL + PFTPAS
     C*** BEG CHG ********** 11/09/09 ************************************
     C***************    EVAL      XXTDRG=CABOPR+CABCAP+CAOIME+CAODSH+CACDSH+
     C***************              +CAOUTL + PFTPAS + CACIME
     C*** BEG CHG *** 01/30/14 *******************************************
     C***************    EVAL      XXTDRG=CABOPR+CABCAP+CAOIME+CAODSH+CACDSH+
     C***************              +CAOUTL + PFTPAS + CACIME + CAHSPB
     C***** Beg chg ***** 11/13/14 *****************************************
     C***************    EVAL      XXTDRG=CABOPR+CABCAP+CAOIME+CAODSH+CACDSH+
     C***************              +CAOUTL + PFTPAS + CACIME + CAHSPB +
     C***************              CANTEC + CAVBPA + CAHRRA + CALVOL

     C***** Beg chg ***** 10/31/18 *****************************************
     C***************    EVAL      XXTDRG=CABOPR+CABCAP+CAOIME+CAODSH+CACDSH+
     C***************              +CAOUTL + PFTPAS + CACIME + CAHSPB +
     C***************              CANTEC + CAVBPA + CAHRRA + CALVOL
     C***************              + CAUNCOMP

     C                   EVAL      XXTDRG=CABOPR+CABCAP+CAOIME+CAODSH+CACDSH+
     C                             +CAOUTL + PFTPAS + CACIME + CAHSPB +
     C                             CANTEC + CAVBPA + CAHRRA + CALVOL
     C                             + CAUNCOMP + CAHAC
     C***** End chg ***** 10/31/18 *****************************************
     C***** End chg ***** 11/13/14 *****************************************
     C*** End CHG *** 01/30/14 *******************************************
     C*** END CHG ********** 11/09/09 ************************************
     C*** END CHG ********** 10/06/08 ************************************
     C                   Z-ADD     XXTDRG        SCTDRG
     C*** Beg Del ********** 06/04/14 ************************************
     C*****     EUIMST        IFEQ      'N'
     C*****     EUIMST        OREQ      'P'
     C*** BEG CHG ********** 09/23/08 ************************************
     C*****    EUIMST        OREQ      'I'
     C*** END CHG ********** 09/23/08 ************************************
     C*****                   Z-ADD     143           SCMULT
     C*****                   ELSE
     C*** BEG CHG ********** 09/23/08 ************************************
     C********* EUIMST        IFEQ      'I'
     C********     EUIMST        OREQ      'S'
     C*****     EUIMST        IFEQ      'S'
     C*** END CHG ********** 09/23/08 ************************************
     C*****                   Z-ADD     108           SCMULT
     C*****                   ENDIF
     C*****                   ENDIF
     C*** End Del ********** 06/04/14 ************************************
     C*** Beg Add *** 10/01/10 **************************************
     C*** Beg Chg ********** 06/04/14 ************************************
     C*****              if        xxmfs = 'Y'
     C*** End Chg ********** 06/04/14 ************************************
     C                   Movel     eoast         @@st
     C                   Movel     'I'           @@type
     C                   Movel     euaddt        @@efdt
     C*
     C     rtky          setgt     prt01                              99
     C     rtky1         readpe    prt01                                  99
     C*
     C                   dow       *in99 = *off
     C                   if        rtstat = 'I'
     C     rtky1         readpe    prt01                                  99
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C*** Beg Chg ********** 06/04/14 ************************************
     C*****              if        *in99 = *off
     C*****              Eval      scmult  = RTMUL1
     C*****              endif
      /Free
        If *in99 = *off ;
          Select ;
          When EUIMST = 'N' or EUIMST = 'P' or EUIMST = 'I' ;
            scmult = RTMUL1 ;
          When EUIMST = 'S' ;
            scmult = RTMUL2 ;
          Endsl ;
        Endif ;
      /End-Free
     C*****              endif                                                  xxmfs = 'Y'
     C*** End Chg ********** 06/04/14 ************************************
     C*** End Add *** 10/01/10 **************************************
     C                   MOVE      CATRAN        SCTRAN
     C                   MOVEL     SCMULT        SCMULT#
     C     CATRAN        IFGT      0
     C*** Beg Chg ********** 06/04/14 ************************************
      /Free
        If @@efdt < 1120101 ;
      /End-Free
     C*** End Chg ********** 06/04/14 ************************************
     C                   EVAL      SCTADJ# = (CATRAN * SCMULT) /100
     C*** Beg Chg ********** 06/04/14 ************************************
      /Free
        Else ;
       //** Beg Chg ********** 06/15/15 ************************************
       //*** SCTADJ# = (CATRAN * SCMULT) ;
          eval(H) SCTADJ# = (CATRAN * SCMULT) ;
       //** End Chg ********** 06/15/15 ************************************
        Endif ;
      /End-Free
     C*** End Chg ********** 06/04/14 ************************************
     C                   ELSE
     C*** Beg Chg ********** 06/04/14 ************************************
      /Free
        If @@efdt < 1120101 ;
      /End-Free
     C*** End Chg ********** 06/04/14 ************************************
     C                   EVAL      SCTADJ = (SCTDRG * SCMULT) /100
     C*** Beg Chg ********** 06/04/14 ************************************
      /Free
        Else ;
       //** Beg Chg ********** 06/15/15 ************************************
       //***   SCTADJ = (SCTDRG * SCMULT) ;
          Eval(H) SCTADJ = (SCTDRG * SCMULT) ;
       //** End Chg ********** 06/15/15 ************************************
        Endif ;
      /End-Free
     C*** End Chg ********** 06/04/14 ************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     PFTPAS        SCTPAS
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVEL(P)  ##conm        scconm
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C*                  MOVE      *ON           DSPTYP
     C*** Beg Add *** 08/12/10 ****************************************
     C                   eval      *in71 = *off
     C*** End Add *** 08/12/10 ****************************************
     C*
     C                   ENDSR                                                  PGMSTR
