    H**************************************************************************************
     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**************************************************************************************
     H* H/TITLE - Work with OPPS Provider Specific Detail program.                         *
     H*                                                                                    *
     H* DESCRIPTION: This program shows option 5(Detail) for Work with OPPS Provider       *
     H*              Specific Detail Screen.                                               *
     H*                                                                                    *
     H* CALLED BY: F2357R                                                                  *
     H*                                                                                    *
     H* CALLS TO :                                                                         *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created: 06/12/08   Programmer C:198   Project:TX039                              *
     H**************************************************************************************
     H*  Revisions:
     H*
     H*  04/18/19 C:212 Project: FS558                                                     *
     H*                 Add parm for admission date                                        *
     H*
     H*  10/30/15 C:309  Project: FS468                                        *
     H*                  Added 3 new fields on screen 2                        *
     H*                  1) Wage index by Provider ID                          *
     H*                  2) Wage index by CBSA                                 *
     H*                  3) Wage index by state code CBSA                      *
     H*
     H*  09/06/10 C:220 Project: FS365                                                     *
     H*                 Change Wage Index heading to Wage Ind. pre 10/01/09.file FTOPP.    *
     H*                                                                                    *
     H*  05/21/09 C:198  Project: FS343
     H*                   Expanded OPNPI# and added OPHOQU
     H*
     H**************************************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2357f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fftopp     uf a e             disk
     F                                     rename(pop01:dbrcdf)
     F**** Beg Add *** 10/30/15 ***************************************
     FFTO2L1    If   E           K Disk
     FFTOAL1    If   E           K Disk
     FFTOBL1    If   E           K Disk
     F**** End Add *** 10/30/15 ***************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'OPPS Provider Specific File'
     D comapi          c                   'FA007R'
     D #ofscr          c                   1
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
     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
      *
      *
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
     C***** Beg Add *** 04/18/19 **************************************
     C                   parm                    qqAdDt            7 0
     C***** End Add *** 04/18/19 **************************************
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C*
     C     dtmmdy        plist
     C                   parm                    srdti
     C                   parm      '*YYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C**** Beg Add *** 10/30/15 ***************************************
     C     l2ky          klist
     C                   kfld                    kkprov           10
     C                   kfld                    kkefdt            7 0
     C*
     C     l2ky1         klist
     C                   kfld                    kkprov
     C*
     C     l4ky          klist
     C                   kfld                    kkCBSA            5
     C                   kfld                    kkefdt
     C*
     C     l4ky1         klist
     C                   kfld                    kkCBSA
     C**** End Add *** 10/30/15 ***************************************
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
     C                   movel     opnpi#        scnpi#
     C                   movel     opprov        scprov
     C                   z-add     opdpt1        srdti
     C                   call      'SRCVTDAT'    dtmmdy
     C                   Movel     srdto         scefdt
     C                   z-add     opdpt1        srdti
     C                   call      'SRCVTDAT'    dtmmdy
     C                   Movel     srdto         scdtp1
     C                   z-add     opbgdt        srdti
     C                   call      'SRCVTDAT'    dtmmdy
     C                   Movel     srdto         scbgdt
     C                   z-add     oprpdt        srdti
     C                   call      'SRCVTDAT'    dtmmdy
     C                   Movel     srdto         scrpdt
     C                   z-add     optrdt        srdti
     C                   call      'SRCVTDAT'    dtmmdy
     C                   Movel     srdto         sctrdt
     C                   eval      scint  = opint
     C                   eval      scpcr  = oppcr
     C                   movel     opwaiv        scwaiv
     C                   movel     optype        sctype
     C                   movel     opsloc        scsloc
     C                   movel     opamsa        scamsa
     C                   movel     opacbs        scacbs
     C                   movel     optops        sctops
     C*** BEG ADD *** 05/21/09 *************************************
     c                   movel     opstat        scstat
     C                   movel     ophoqu        schoqu
     C*** END ADD *** 05/21/09 *************************************
     C                   eval      scscod = opscod
     C                   eval      scoccr = opoccr
     C                   eval      scsppy = opsppy
     C                   eval      scrctr = oprctr
     C                   movel     opdesc        scdesc
     C                   eval      sccdwi = opcdwi
     C                   eval      scwmsa = opwmsa
     C                   eval      scwcbs = opwcbs
     C                   eval      scwind = opwind
     C                   eval      scspwi = opspwi
     C*
     C*** Beg Add ***** 10/30/15 ******************************************
      /Free
          kkPROV  = OPPROV;
          kkCBSA = OPacbs;
          kkefdt = %dec(%char(%date(OPefdt:*iso):*cymd0):7:0);

      //***** Beg Add ***** 04/18/19 **************************************
        // Use Admission date as key if that is passed in

          If Q1prms > 4;
            If qqAdDt <> *zeros;
              kkefdt = qqAdDt;
            EndIf;
          EndIf;
      //***** End Add ***** 04/18/19 **************************************

          //Populate Wage index by Provider ID
          Setgt l2ky PO201;
          Readpe l2ky1 PO201;
          If Not %Eof(FTO2L1);
            SCPVWI = O2CRWI;
          Endif;

          //Populate Wage index by CBSA
          Setgt l4ky POA01;
          Readpe l4ky1 POA01;
          If Not %Eof(FTOAL1);
            SCCBWI = OACRWI;
          Endif;

          //Populate Wage index by state code CBSA
          Setgt l4ky POB01;
          Readpe l4ky1 POB01;
          If Not %Eof(FTOBL1);
            SCSTWI = OBCRWI;
          Endif;
      /End-Free
     C*** End Add ***** 10/30/15 ******************************************
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   movel     scnpi#        opnpi#
     C                   movel     scprov        opprov
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   Movel     srdto         opdpt1
     C                   z-add     scdtp1        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   Movel     srdto         opdpt1
     C                   z-add     scbgdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   Movel     srdto         opbgdt
     C                   z-add     scrpdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   Movel     srdto         oprpdt
     C                   z-add     sctrdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   Movel     srdto         optrdt
     C                   eval      opint  = scint
     C                   eval      oppcr  = scpcr
     C                   movel     scwaiv        opwaiv
     C                   movel     sctype        optype
     C                   movel     scsloc        opsloc
     C                   movel     scamsa        opamsa
     C                   movel     scacbs        opacbs
     C                   movel     sctops        optops
     C*** BEG ADD *** 05/21/09 *************************************
     C                   movel     schoqu        ophoqu
     C*** END ADD *** 05/21/09 *************************************
     C                   eval      opscod = scscod
     C                   eval      opoccr = scoccr
     C                   eval      opsppy = scsppy
     C                   eval      oprctr = scrctr
     C                   movel     scdesc        opdesc
     C                   eval      opcdwi = sccdwi
     C                   eval      opwmsa = scwmsa
     C                   eval      opwcbs = scwcbs
     C                   eval      opwind = scwind
     C                   eval      opspwi = scspwi
     C*PUIMOD
     C*                  move      *date         sys_date
     C                   eval       sys_date = %date()
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      OPdt
     C                   eval      OPpgm=q1pgm
     C                   eval      OPuser=q1user
     C                   move      sys_time      OPtime
     C*
     C     *cymd         move      sys_date      OPidt
     C                   eval      OPipgm=q1pgm
     C                   eval      OPiusr=q1user
     C                   move      sys_time      OPitim
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
     C                   feod      ftopp
     C                   endsr
