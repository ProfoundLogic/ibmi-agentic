     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H actgrp(*caller)
     H/TITLE F0365R                                                                        *
     H**************************************************************************************
     H*  Description:  ASC Payment Information                                             *
     H*                                                                                    *
     H*  Created: 11/14/08   Programmer C:212   Project:TX043                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  07/20/15 C:079 Project: NC039                                                     *
    H*                 Add logic for hospital as well as physician                        *
     H*                                                                                    *
     H*  04/14/09 C:079 Project: TX043 P# 36                                               *
    H*                 Load the ASC payment amount correctly                              *
     H*                                                                                    *
     H*  03/17/09 C:227 Project: TX057 P# 11                                               *
    H*                 Corrected APC payment info screen wage index on                    *
    H*                 taking F9 option.                                                  *
     H*                                                                                    *
     H*  03/16/09 C:198 Project: TX043 P# 33                                               *
    H*                 Include processing for no match in FTWGP                           *
     H*                                                                                    *
     H**************************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FF0365f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FFDDHL     if   e           K DISK
     FFMASL1    if   e           K DISK
     FFAEHL     if   e           k DISK
     FFAEOLG    if   e           k DISK
     FFTWGL1    if   e           k DISK
     FFDCAL24   if   e           k DISK
     F*** BEG ADD *** 03/16/09 *********************************************
     Fftwgl2    if   e           k disk    rename(pwg01:pwg02)
     Fststl1    if   e           k disk
     F*** END ADD *** 03/16/09 *********************************************
     F*** BEG ADD *** 04/14/09 *********************************************
     Fftapl     if   e           k disk
     F*** END ADD *** 04/14/09 *********************************************
     F*** Beg Add *** 07/20/15 *********************************************
     FFDDUL     if   e           K DISK
     FFAEUL     if   e           k DISK
     F*** End Add *** 07/20/15 *********************************************
      *
     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D ##cpt4          s              5    INZ('DRG')
     D OPTNM           C                   CONST('ASC Payment Information')
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
     C                   parm                    ##CONM           40
     C                   parm                    #1DSEQ            5 0
     C*
     C*** Beg Add *** 07/20/15 ****************************************
     C     eoky          klist
     C                   kfld                    ##CO#
     C                   kfld                    ##SUB#
     C                   kfld                    ##FND
     C                   kfld                    ##FYR
     C                   kfld                    ##CASE
     C                   kfld                    ##EOB#
     C*
     C     duky          klist
     C                   kfld                    ##CO#
     C                   kfld                    ##SUB#
     C                   kfld                    ##FND
     C                   kfld                    ##FYR
     C                   kfld                    ##CASE
     C                   kfld                    ##EOB#
     C                   kfld                    #1DSEQ
     C*
     C     euky          klist
     C                   kfld                    ##CO#
     C                   kfld                    ##SUB#
     C                   kfld                    ##FND
     C                   kfld                    ##FYR
     C                   kfld                    ##CASE
     C                   kfld                    ##EOB#
     C*** End Add *** 07/20/15 ****************************************
     C*
     C     dhky          klist
     C                   kfld                    ##CO#
     C                   kfld                    ##SUB#
     C                   kfld                    ##FND
     C                   kfld                    ##FYR
     C                   kfld                    ##CASE
     C                   kfld                    ##EOB#
     C                   kfld                    #1DSEQ
     C*
     C     caky          klist
     C                   kfld                    ##CO#
     C                   kfld                    ##SUB#
     C                   kfld                    ##FND
     C                   kfld                    ##FYR
     C                   kfld                    ##CASE
     C                   kfld                    ##EOB#
     C                   kfld                    #1DSEQ
     C*
     C     asky          klist
     C                   kfld                    ##PRCD            5
     C                   kfld                    ##EFDT            7 0
     C*
     C     asky1         klist
     C                   kfld                    ##PRCD
     C*
     C     ehky          klist
     C                   kfld                    ##CO#
     C                   kfld                    ##SUB#
     C                   kfld                    ##FND
     C                   kfld                    ##FYR
     C                   kfld                    ##CASE
     C                   kfld                    ##EOB#
     C*
     C     wgky          klist
     C                   kfld                    ##CBSA            5
     C*
     C*** BEG ADD *** 03/17/09 *********************************************
     C     wgky3         klist
     C                   kfld                    ##CBSA
     C                   kfld                    ##efdt
     C*
     C*** END ADD *** 03/17/09 *********************************************
     C*** BEG ADD *** 03/16/09 *********************************************
     C     wgky2         klist
     C                   kfld                    ##nam            40
     C                   kfld                    ##efdt
     C*
     C     wgky1         klist
     C                   kfld                    ##nam
     C*
     C     stky          klist
     C                   kfld                    ##key            10
     C*
     C*** END ADD *** 03/16/09 *********************************************
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
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C
     C*----------------------------------------------------------------------
     C*  MAINLINE CODE
     C*----------------------------------------------------------------------
     C*
     C*** Beg Add *** 07/20/15 *********************************************
     C     eoky          Chain     pEO01                              99
     C*
     C                   if        *in99 = *Off
     C*
     C                   Select
     C*
     C     EObTyp        WhenEQ    'N'
     C*** End Add *** 07/20/15 *********************************************
     C*
     C     dhky          chain     pdh01                              99
     C                   if        *in99 = *off
     C*
     C                   movel     dhprcd        ##prcd
     C                   movel     ##prcd        scprcd
     C*** Beg Add *** 07/20/15 *********************************************
     C*
     C                   Endif
     C*** End Add *** 07/20/15 *********************************************
     C*** Beg Add *** 07/20/15 *********************************************
     C     EObTyp        WhenEQ    'H'
     C*
     C     duky          chain     pDU01                              99
     C                   if        *in99 = *off
     C*
     C                   movel     DUrate        ##prcd
     C                   movel     ##prcd        scprcd
     C*
     C                   Endif
     C*
     C                   EndSl
     C*** End Add *** 07/20/15 *********************************************
     C*
     C     caky          chain     pca01                              99
     C                   if        *in99 = *off
     C                   movel     cafdat        ##efdt
     C                   endif
     C*
     C     asky          setgt     pas01
     C     asky1         readpe    pas01                                  99
     C                   if        *in99 = *off
     C                   z-add     asalow        scalow
     C                   z-add     asalow        NatReimAmt        9 2
     C                   z-add     asefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd2
     C                   movel     srdto         scefdt
     C                   movel     asdesc        scdesc
     C                   movel     aspi          scpi
     C                   movel     asdev         scdev
     C                   z-add     asdper        scdper
     C                   endif
     C*
     C*** Beg Add *** 07/20/15 *********************************************
     C                   Select
     C*
     C     EObTyp        WhenEQ    'N'
     C*** End Add *** 07/20/15 *********************************************
     C*
     C     ehky          chain     peh01                              99
     C                   if        *in99 = *off
     C                   movel     ehcbsa        ##cbsa
     C                   movel     ehcbsa        sccbsa
     C                   movel     ehimst        scimst
     C                   endif
     C*** Beg Add *** 07/20/15 *********************************************
     C*
     C     EObTyp        WhenEQ    'H'
     C*
     C     euky          chain     peu01                              99
     C                   if        *in99 = *off
     C                   movel     eucbsa        ##cbsa
     C                   movel     eucbsa        sccbsa
     C                   movel     euimst        scimst
     C                   endif
     C*
     C                   endsl
     C*** End Add *** 07/20/15 *********************************************
     C*
     C*** BEG CHG *** 03/17/09 **************************************************
     C**** wgky          chain     pwg01                              99
     C     wgky3         setgt     pwg01                              99
     C     wgky          readpe    pwg01                                  99
     C*** END CHG *** 03/17/09 **************************************************
     C                   if        *in99 = *off
     C                   z-add     wgwind        scwind
     C                   z-add     wgwind        xxwind            5 4
     C*** BEG ADD *** 03/16/09 **************************************************
     C                   else
     C                   movel     ehrst         ##key
     C     stky          chain     pst01                              99
     C                   if        *in99 = *off
     C                   movel     stname        ##nam
     C     wgky2         setgt     pwg02                              99
     C     wgky1         readpe    pwg02                                  99
     C  N99              z-add     wgwind        scwind
     C  N99              z-add     wgwind        xxwind
     C                   endif                                                  *in99 = *off
     C*** END ADD *** 03/16/09 **************************************************
     C                   endif
     C*
     C     caky          chain     pca01                              99
     C                   if        *in99 = *off
     C                   z-add     capaid        sctpmt
     C                   z-add     camult        scmult
     C*** BEG CHG *** 04/14/09 **************************************************
     C****************** z-add     camra         scapmt
     C*
     C     asky          setgt     pap01                              99
     C     asky1         readpe    pap01                                  99
     C                   if        *in99 = *off
     C                   eval(H)   scapmt = apalow
     C                   else
     C                   eval(H)   scapmt = 0
     C                   endif
     C*** END CHG *** 04/14/09 **************************************************
     C                   endif
     C*
     C*** Determining the geographic adjustment
     C*
     C                   move      *zeros        temp1             9 2
     C                   move      *zeros        temp2             6 4
     C                   move      *zeros        GeoAdjReim        9 2
     C*
     C                   eval      temp1 = NatReimAmt * 0.5
     C                   eval      temp2 = xxwind + 1
     C                   eval      GeoAdjReim = temp1 * temp2
     C*
     C                   z-add     GeoAdjReim    scgpmt
     C*
     C                   exsr      pgmstr
     C                   exfmt     Scr01
     C                   endif
     C*
     C                   move      *on           *inlr
     C*
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
     C*
     C                   ENDSR                                                  PGMSTR
