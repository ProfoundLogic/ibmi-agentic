     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*====================================================================
     H*  Created: 05/20/09   C:33   Project: WC1770
     H*====================================================================
     H*  Revisions:
     H*
     H*  08/05/13 C:277 PROJECT: MC329 Phase II
     H*                 Add search functionality and Validate ICD10
     H*
     H*  05/23/13 C:277 PROJECT: MC329
     H*                 Activate the ICD10 fields if ICD10 is active
     H*
     H*  01/21/12   C:282  Project: SY266 Redmine Item 2223
     H*                    Fix the issue by replacing the Company API value of
     H*                    SA002R by WA001R.
     H*
     H*  03/28/11   C:198  Project: WC1879 Priss 142
     H*                    -subscriber# need not to be added in case ICD9 codes
     H*                     are added from WC side.
     H*                    -do not display all ICD9 codes from WC side when taken
     H*                     from Managed care side.
     H*
     H*  01/21/11   C:198  Project: WC1879 Priss 142
     H*                    Fixed the issue to load subscriber# for ICD9 codes
     H*                    entered for WC.
     H*                    CMS flag should be checked as I when claims are
     H*                    added for WC.
     H*
     H*  11/19/10   C:198  Project: WC1819 Priss 122
     H*                    Corrected ICD9 edits w.r.t CMS status flag.
     H*
     H*  05/18/10   C:198  Project: WC1879
     H*                    Help on ICD-9 code modified.
     H*
     H*====================================================================
     Fs0275f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     Fsadap     uf a e             disk    rename(pda01:dbrcdf)
     F                                     infds(dbfds)
     Fsadal     if   e           k disk    rename(pda01:pda00)
     Fsadal1    if   e           k disk
     Fsadal2    if   e           k disk    rename(pda01:pda02) prefix(l2)
     Fsmdil     if   e           k disk    rename(pdi01:pdi00)
     Fsidip     if   e           k disk    rename(pdi01:pdi0p)
     Fstgtl     if   e           k disk    rename(pgt01:pgt00)
     Fstgtp     if   e             disk    rename(pgt01:pgt0p)
     Fstgil     if   e           k disk    rename(pgi01:pgi00)
     Ffmcmp     if   e             disk    rename(pcm01:mcm0p) usropn
     Fwmcmp     if   e             disk    rename(pcm01:wcm0p) usropn
     F*** Beg Add *** 08/05/13 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/05/13 ***************************************
     D*
     D OPTNM           c                   const('Diagnosis Information')
     C*** Beg Chg *** 01/21/12 ***********************************
     D*****COMAPI          c                   const('SA002R')
     D COMAPI          c                   const('WA001R')
     C*** End Chg *** 01/21/12 ***********************************
     D #ofScr          c                   const(1)
     D clmFiledOpened  s              1n
     D*
     Ddbfds            DS
     D dbrrn                 397    400B 0
     D*
     DUpdateMC         pr                  extPgm('FA275R')
     D rrn                            9  0 const
     D mode                           1    const
     D fromFile                       2    const
     D toFile                         2    const
     D*
     D @@Area          s              2
     D xxArea          s                   like(@@Area)
     D addMode         s              1n
     D chgMode         s              1n
     D dspMode         s              1n
     D ILOC            s              4
     D ISIT            s              4
     D*
     D/copy sCopyMT43d
     D*
     D/copy sPrDate
     D/copy sPrString
     D*
     D***Beg Add *** 05/23/13 ***************************************
     D SA015R          pr                  extpgm('SA015R')
     D @@rtcd                         1
     D*
     D xxicd10         s              1
     D @@rtcd          s              1
     D***End Add *** 05/23/13 ***************************************
     D*** Beg Add *** 08/05/13 ***************************************
     D kkicd10         s                   like(iccode)
     D*** End Add *** 08/05/13 ***************************************
     Imcm0p
     I              CMOSCL                      C2OSCL
     C*
     C     *ENTRY        plist
     C                   parm      *INKC         @@KC              1
     C                   parm      *INKL         @@KL              1
     C                   parm                    @@MODE            1
     C                   parm                    @@RRN             9 0
     C                   parm                    @@RRN2            9 0
     C                   parm                    @@Area
     C*
     C     COMPL         plist
     C                   parm                    ##CO#             3 0
     C                   parm                    ##FND             3 0
     C                   parm                    ##CONM           40
     C                   parm                    ##FNAM           40
     C                   parm                    ##SUB#            5 0
     C                   parm                    ##FYR             3 0
     C*
     C* DIAGNOSIS CODE INFO FILR
     C*
     C     DAKY          klist
     C                   kfld                    KKCO#
     C                   kfld                    KKSUB#
     C                   kfld                    KKFND
     C                   kfld                    KKFYR
     C                   kfld                    KKCASE
     C                   kfld                    KKSEQ#
     C                   kfld                    KKICD9
     C*
     C* KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C*
     C     DIKEY         klist
     C                   kfld                    KKICD9
     C                   kfld                    KKLOST
     C*
     C*** Beg Add *** 05/18/10 ***********************************
     C     DIKEY2        klist
     C                   kfld                    KKICD9
     C*
     C*** End Add *** 05/18/10 ***********************************
     C* KEY LIST FOR GENERIC TABLE FILE
     C*
     C     GTKY          klist
     C                   kfld                    KKCO#
     C                   kfld                    KKTID             4
     C                   kfld                    KKTKEY           10
     C*
     C     GIKY          klist
     C                   kfld                    ##CO#
     C                   kfld                    KKTID
     C*
     C* KEY LIST FOR DIAGNOSIS IMFORMATION FILE
     C*
     C     DAKY1         klist
     C                   kfld                    KKCO#
     C                   kfld                    KKSUB#
     C                   kfld                    KKFND
     C                   kfld                    KKFYR
     C                   kfld                    KKCASE
     C                   kfld                    KKPRIM
     C*
     C* KEY LIST FOR DIAGNOSIS IMFORMATION FILE
     C*
     C     DAKY2         klist
     C                   kfld                    KKCO#
     C                   kfld                    KKSUB#
     C                   kfld                    KKFND
     C                   kfld                    KKFYR
     C                   kfld                    KKCASE
     C                   kfld                    KKICD9
     C                   kfld                    KKLOST
     C*
     C* DIAGNOSIS CODE INFO FILR
     C*
     C     DAKY3         klist
     C                   kfld                    KKCO#
     C                   kfld                    KKSUB#
     C                   kfld                    KKFND
     C                   kfld                    KKFYR
     C                   kfld                    KKCASE
     C                   kfld                    KKSEQ#
     C*
     C* DIAGNOSIS CODE INFO FILR
     C*
     C     DAKY4         klist
     C                   kfld                    KKCO#
     C                   kfld                    KKSUB#
     C                   kfld                    KKFND
     C                   kfld                    KKFYR
     C                   kfld                    KKCASE
     C*** Beg Add *** 08/05/13 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C*** End Add *** 08/05/13 ***************************************
     C*
     C     *LIKE         DEFINE    DAICD9        KKICD9
     C     *LIKE         DEFINE    DALOST        KKLOST
     C     *LIKE         DEFINE    DACO#         KKCO#
     C     *LIKE         DEFINE    DASUB#        KKSUB#
     C     *LIKE         DEFINE    DAFND         KKFND
     C     *LIKE         DEFINE    DAFYR         KKFYR
     C     *LIKE         DEFINE    DACASE        KKCASE
     C     *LIKE         DEFINE    DASEQ#        KKSEQ#
     C     *LIKE         DEFINE    DAPRIM        KKPRIM
     C     *LIKE         DEFINE    DAICD9        HDICD9
     C     *LIKE         DEFINE    DALOST        HDLOST
     C     *LIKE         DEFINE    DAPRIM        HDPRIM
     C     *LIKE         DEFINE    DAISDT        XXISDT
     C     *LIKE         DEFINE    DAEXDT        XXEXDT
     C     *LIKE         DEFINE    DASEQ#        XXSEQ#
     C*
     C/copy sCopyMT43c
     C*
     C*====================================================================
     C* DATCVT - RUNS A DATCHK TO GO FROM 6 TO 8.
     C*====================================================================
     C     DATCVT        BEGSR
     C*
     C                   eval      scIsDt = DtCheck(scIsDt)
     C                   eval      scExDt = DtCheck(scExDt)
     C*
     C                   ENDSR
     C*====================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*====================================================================
     C     DLTPRE        BEGSR
     C*
     C* Ensure WCMC is kept in sync
     C                   callp(e)  UpdateMC(dbrrn:'D':'SY':'MC')
     C*
     C                   ENDSR
     C*====================================================================
     C* DSPLYR - Processing after displaying information
     C*====================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*====================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*====================================================================
     C     ERRCK1        BEGSR
     C*
     C** SET THESE INDICATORS OFF - USED AS POSITION CURSORS
     C*
     C                   SETOFF                                       303132
     C                   SETOFF                                       3334
     C*
     C     *IN40         ifEQ      *ON
     C*
     C* VALIDATE DIAGNOSIS CODE AND TYPE
     C*
     C     SCICD9        ifEQ      *BLANKS
     C                   seton                                        9071
     C                   leavesr
     C                   endif
     C*
     C     SCLOST        ifEQ      *BLANKS
     C                   seton                                        9072
     C                   leavesr
     C                   endif
     C*
     C     SCICD9        ifNE      *BLANKS
     C     SCLOST        ANDNE     *BLANKS
     C*
     C                   MOVEL     SCICD9        KKICD9
     C                   MOVEL     SCLOST        KKLOST
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkicd10 = scICd9;
          Chain fmicd10l1key pic01;
          If not %found(fmicd10l1);
             *in90 = *on;
             *in73= *on;
             leavesr;
          Else;
             If ICstat = 'I';
                *in90 = *on;
                *in79 = *on;
                leavesr;
             Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     DIKEY         chain     pdi00                              99
     C     *IN99         ifEQ      '1'
     C                   seton                                        9073
     C                   leavesr
     C                   endif
     C     DISTAT        ifEQ      'I'
     C                   seton                                        9079
     C                   leavesr
     C                   endif                                                  DISTAT ifEQ
     C*** Beg Add *** 01/21/11 *********************************************
     C                   if        xxarea <> 'MC'
     C                             and cmmbs = 'Y'
     C     DICMS         ifne      'I'
     C                   seton                                        9085
     C                   leavesr
     C                   endif                                                  DICMS  ifEQ
     C                   endif                                                  xxarea <> 'MC'
     C*** End Add *** 01/21/11 *********************************************
     C*** Beg Del *** 01/21/11 *********************************************
     C*** Beg Add *** 05/18/10 *********************************************
     C***  DICMS         ifne      'I'
     C*** Beg Add *** 11/19/10 *********************************************
     C***  DICMS         andne     ' '
     C*** End Add *** 11/19/10 *********************************************
     C***                seton                                        9085
     C***                leavesr
     C***                endif                                                  DICMS  ifEQ
     C*** End Del *** 01/21/11 *********************************************
     C*** Beg Add *** 01/21/11 *********************************************
     C                   if        xxarea <> 'MC'
     C                             and cmmbs = 'Y'
     C*** End Add *** 01/21/11 *********************************************
     C*** Beg Chg *** 11/19/10 *********************************************
     C***                if        %subst(scicd9:1:1) = 'E' or
     C*** Beg Chg *** 01/21/11 *********************************************
     C***                if        (%subst(scicd9:1:1) = 'E' and
     C***                          dicms <>  ' ') or
     C                   if        %subst(scicd9:1:1) = 'E' or
     C*** End Chg *** 01/21/11 *********************************************
     C*** End Chg *** 11/19/10 *********************************************
     C                             %subst(scicd9:1:1) = 'V'
     C*** Beg Add *** 11/19/10 *********************************************
     C*** Beg Del *** 01/21/11 *********************************************
     C***                          and dicms <> ' '
     C***                          and dicms <>  'I'
     C*** End Del *** 01/21/11 *********************************************
     C*** End Add *** 11/19/10 *********************************************
     C                   MOVE      '1'           *IN90
     C                   MOVE      '1'           *IN85
     C*** Beg Add *** 01/21/11 *********************************************
     C                   leavesr
     C*** End Add *** 01/21/11 *********************************************
     C                   endif
     C*** End Add *** 05/18/10 *********************************************
     C                   endif                                                  SCICD9 ifNE
     C*** Beg Add *** 01/21/11 *********************************************
     C                   endif                                                  xxarea <> 'MC'
     C*** End Add *** 01/21/11 *********************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C*
     C                   endif                                                  *IN40  ifEQ
     C*
     C* CHECK TO SEE if RECORD ALREADY EXISTS
     C*
     C     *IN40         ifEQ      *ON
     C     DAKY2         chain     pda01                              99
     C     *IN99         ifEQ      *OFF
     C                   seton                                        9089
     C                   leavesr
     C                   endif
     C*
     C                   ELSE
     C*
     C     HDICD9        ifNE      SCICD9
     C     HDLOST        ORNE      SCLOST
     C     DAKY2         chain     pda01                              99
     C     *IN99         ifEQ      *OFF
     C                   seton                                        9089
     C                   leavesr
     C                   endif                                                  *IN99 ifEQ *OFF
     C                   endif                                                  HDICD9 ifNE SCICD9
     C*
     C                   endif                                                  @@MODE ifEQ
     C*
     C* VALIDATE PRIMARY DIAGNOSIS CODE
     C*
     C     SCPRIM        ifNE      'Y'
     C     SCPRIM        ANDNE     'N'
     C                   seton                                        9074
     C                   leavesr
     C                   endif
     C*
     C* CHECK TO SEE if A PRIMARY CODE ALREADY EXISTS
     C*
     C     *IN40         ifEQ      *ON
     C*
     C     SCPRIM        ifEQ      'Y'
     C                   MOVE      SCPRIM        KKPRIM
     C     DAKY1         chain     pda02                              99
     C     *IN99         ifEQ      *OFF
     C                   seton                                        9080
     C                   leavesr
     C                   endif                                                  *IN99 ifEQ OFF
     C                   endif                                                  SCPRIM ifEQ Y
     C*
     C                   ELSE
     C*
     C     SCPRIM        ifEQ      'Y'
     C     HDPRIM        ANDNE     SCPRIM
     C                   MOVE      SCPRIM        KKPRIM
     C     DAKY1         chain     pda02                              99
     C     *IN99         ifEQ      *OFF
     C                   seton                                        9080
     C                   leavesr
     C                   endif                                                  *IN99 ifEQ OFF
     C                   endif                                                  SCPRIM ifEQ Y
     C*
     C                   endif                                                  @@MODE ifEQ C
     C* CALL ICD9 EDIT PROGRAM
     C                   MOVEL     SCICD9        ##ICD9
     C     SCPRIM        ifEQ      'Y'
     C                   MOVE      'P'           ##PRSC
     C                   ELSE
     C                   MOVE      'S'           ##PRSC
     C                   endif
     C                   MOVE      'C'           ##CALL
     C                   MOVE      'P'           ##SYS
     C                   MOVE      *BLANKS       ##STYP
     C                   MOVE      *BLANKS       ##VLD
     C                   MOVE      *BLANKS       ##GRP
     C                   MOVE      *BLANKS       ##ERR
     C                   MOVE      *BLANKS       ##STS
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C                   CALL      'F4200ROH'                           99
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****              parm                    ##ICD9            5            IDC9 CODE
     C                   parm                    ##ICD9                         IDC9 CODE
     C*** End Chg *** 08/05/13 ***************************************
     C                   parm                    ##PRSC            1            PRIMARY/SECOND
     C                   parm                    ##CALL            1            CALLED PGM CODE
     C                   parm                    ##SYS             1            SYSTEM (P/H)
     C                   parm                    ##STYP            1            INPAT/OUTPAT
     C                   parm                    ##VLD             1            VALID/INVALID
     C                   parm                    ##GRP             1            GROUP COVERAGE
     C                   parm                    ##ERR             2            Ennn/Vnnn ICD9
     C                   parm                    ##STS             2            CLAIM STATUS
     C                   parm                    #1CO#             3            CO #
     C                   parm                    #1SUB             5            SUB #
     C                   parm                    #1FND             3            FUND
     C                   parm                    #1FYR             3            FUND YEAR
     C                   parm                    #1CASE            7            CASE#
     C*
     C* When claim is CMS Submission, Primary Code can not be insufficient
     C*** Beg Del *** 05/18/10 ***********************************
     C*******            if        CMMBS = 'Y' and SCPRIM = 'Y'
     C*******            clear                   DIINSSTS
     C**** SCICD9        chain     SIDIP
     C*******            if        DIINSSTS = 'I'
     C*** End Del *** 05/18/10 ***********************************
     C*** Beg Add *** 01/21/11 *********************************************
     C                   if        xxarea <> 'MC'
     C                             and cmmbs = 'Y'
     C*** End Add *** 01/21/11 *********************************************
     C*** Beg Add *** 05/18/10 ***********************************
     C                   MOVEL     SCICD9        KKICD9
     C     DIKEY2        chain     PDI00
     C                   if        DICMS = 'E'
     c*** End Add *** 05/18/10 ***********************************
     C                   seton                                        35  90
     C*** Beg Add *** 01/21/11 *********************************************
     C                   leavesr
     C*** End Add *** 01/21/11 *********************************************
     C                   endif
     c*** Beg Del *** 05/18/10 ***********************************
     C******             endif
     c*** End Del *** 05/18/10 ***********************************
     C*
     C* CHECK ERROR FLAG FOR 'E' OR 'V' CODE
     C     ##ERR         ifEQ      'EV'
     C                   MOVE      '1'           *IN90
     C                   MOVE      '1'           *IN85
     C                   leavesr
     C                   endif
     C*** Beg Add *** 01/21/11 *********************************************
     C                   endif                                                        xxarea <> 'MC'
     C*** End Add *** 01/21/11 *********************************************
     C*
     C* CHECK WHETHER ICD9 CODE HAS BEEN FLAGGED AS BWC INVALID
     C     ##VLD         ifEQ      'I'
     C                   MOVE      '1'           *IN90
     C                   MOVE      '1'           *IN86
     C                   leavesr
     C                   endif
     C*
     C* VALIDATE LOCATION
     C*
     C     SCILOC        ifNE      *BLANKS
     C                   MOVEL(P)  ILOC          KKTID
     C                   MOVEL(P)  SCILOC        KKTKEY
     C     GTKY          chain     pgt00                              99
     C     *IN99         ifEQ      '1'
     C                   seton                                        9076
     C                   leavesr
     C                   endif                                                  *IN99 ifEQ
     C                   endif
     C*
     C* VALIDATE SITE
     C*
     C     SCISIT        ifNE      *BLANKS
     C                   MOVEL(P)  ISIT          KKTID
     C                   MOVEL(P)  SCISIT        KKTKEY
     C     GTKY          chain     pgt00                              99
     C     *IN99         ifEQ      '1'
     C                   seton                                        9077
     C                   leavesr
     C                   endif                                                  *IN99 ifEQ
     C                   endif
     C*
     C* VALIDATE STATUS
     C*
     C     SCISTS        ifNE      *BLANKS
     C                   MOVEL(P)  'ISTS'        KKTID
     C                   MOVEL(P)  SCISTS        KKTKEY
     C     GTKY          chain     pgt00                              99
     C     *IN99         ifEQ      '1'
     C                   seton                                        9078
     C                   leavesr
     C                   endif                                                  *IN99 ifEQ
     C                   endif
     C*
     C* DATE if ENTERED HAS TO BE VALID
     C*
     C     SCISDT        ifNE      *ZEROS                                       ADD 12/15/99
     C                   eval      scIsDt = DtCheck(scIsDt)
     C*
     C                   if        scIsDt = *zero
     C                   seton                                        8790
     C                   leavesr
     C                   endif                                                  SRERR
     C*
     C                   endif                                                  SCISTS
     C*
     C* DATE if ENTERED HAS TO BE VALID
     C*
     C     SCEXDT        ifNE      *ZEROS
     C                   eval      scExDt = DtCheck(scExDt)
     C*
     C                   if        scExDt = *zero
     C  N90              seton                                        8190
     C                   leavesr
     C                   endif                                                  SRERR
     C                   endif
     C*
     C* SCEXDT HAS TO BE GREATER THAN OR EQUAL TO SCISDT.
     C*
     C                   if        CvtToDate(scIsDt) > CvtToDate(scExDt)
     C                              and scExDt <> *zeros
     C  N90              seton                                        8290
     C                   leavesr
     C                   endif
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*====================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*====================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*====================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*
     C*====================================================================
     C* GENTBL - GENERIC TABLE SEARCH
     C*====================================================================
     C     GENTBL        BEGSR
     C*
     C                   CALL      'S2380R'                             99
     C                   parm      '2'           ##CALL            1
     C     *INKC         parm      '0'           ##KC              1
     C     *INKL         parm      '0'           ##KL              1
     C                   parm      *ZEROS        ##RRN             9 0
     C                   parm                    ##TBID            4
     C*
     C                   ENDSR
     C*====================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*====================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      *BLANKS       SCICD9
     C                   MOVE      *BLANKS       SCLOST
     C                   MOVE      *BLANKS       SCDSC1
     C                   MOVE      *BLANKS       SCPRIM
     C                   MOVE      *BLANKS       SCILOC
     C                   MOVE      *BLANKS       SCLDSC
     C                   MOVE      *BLANKS       SCISIT
     C                   MOVE      *BLANKS       SCSDSC
     C                   MOVE      *BLANKS       SCISTS
     C                   MOVE      *BLANKS       SCSTSD
     C                   Z-ADD     0             SCISDT
     C                   Z-ADD     0             SCEXDT
     C*
     C     *IN40         ifEQ      *OFF
     C*
     C                   MOVEL     DAICD9        SCICD9
     C                   MOVE      DALOST        SCLOST
     C*
     C                   MOVE      *BLANKS       KKICD9
     C                   MOVE      *BLANKS       KKLOST
     C                   MOVEL     SCICD9        KKICD9
     C                   MOVE      SCLOST        KKLOST
     C*
     C     DIKEY         chain     pdi00                              99
     C     *IN99         ifEQ      '0'
     C                   MOVEL     DIDSC1        SCDSC1
     C                   endif
     C*
     C                   MOVE      DAPRIM        SCPRIM
     C                   MOVEL     DAILOC        SCILOC
     C*
     C* GET LOCATION  DESCRIPTION
     C*
     C                   MOVEL(P)  ILOC          KKTID
     C                   MOVEL     SCILOC        KKTKEY
     C     GTKY          chain     pgt00                              99
     C     *IN99         ifEQ      '0'
     C                   MOVEL     GTDESC        SCLDSC
     C                   endif
     C*
     C                   MOVEL     DAISIT        SCISIT
     C*
     C* GET SITE DESCRIPTION
     C*
     C                   MOVEL(P)  ISIT          KKTID
     C                   MOVEL     SCISIT        KKTKEY
     C     GTKY          chain     pgt00                              99
     C     *IN99         ifEQ      '0'
     C                   MOVEL     GTDESC        SCSDSC
     C                   endif
     C*
     C                   MOVEL     DAISTS        SCISTS
     C*
     C* GET STATUS DESCRIPTION
     C*
     C                   MOVEL(P)  'ISTS'        KKTID
     C                   MOVEL     SCISTS        KKTKEY
     C     GTKY          chain     pgt00                              99
     C     *IN99         ifEQ      '0'
     C                   MOVEL     GTDESC        SCSTSD
     C                   endif
     C*
     C                   eval      scIsDt = DtMDYY(DAISDT)
     C                   eval      scExDt = DtMDYY(DAEXDT)
     C*
     C                   endif                                                  @@MODE ifEQ
     C     @@MODE        ifEQ      'C'
     C                   MOVEL(P)  DAICD9        HDICD9
     C                   MOVE(P)   DALOST        HDLOST
     C                   MOVE(P)   DAPRIM        HDPRIM
     C                   endif
     C*
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
         If xxicd10 = *on and DAicdind ='Y';
            scICd9   = DAicd1001;
            scICdind = DAicdind;
            kkICd10  = DAicd1001;
            scDSc1 = *blanks;
            Chain fmicd10l1key pic01;
            If %found(fmicd10l1) and ICstat ='A';
               scDSc1 = ICdsc1;
            Endif;
         Elseif DAicdind ='N';
            scICdind = DAicdind;
         Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C                   ENDSR
     C*====================================================================
     C* KJDO - F10=SEARCH.
     C*====================================================================
     C     KJDO          BEGSR
     C*
     C                   EXSR      DATCVT
     C*
     C* DIAGNOSIS CODE AND TYPE SEARCH
     C*
     C     HFLD          ifEQ      'SCICD9'
     C*** Beg Add *** 08/05/13 ****************************************
     C                   If        scICDind <> 'Y'
     C                   eval      xxicd10 = '0'
     C                   eval      ##LOST = *BLANKS
     C                   else
     C                   eval      xxicd10 = '1'
     C                   eval      ##LOST = 'D'
     C                   Endif
     C                   CALL      'F2012R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0          FADAP FILE
     C                   PARM      *BLANKS       ##ICD9            7            ICD9 FILE
     C                   PARM                    ##LOST            1            ICD9 FILE
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      @@RRN2        ##RRN2            9 0          FMCMP FILE
     C                   PARM                    xxicd10
     C*** End Add *** 08/05/13 ****************************************
     C*****
     C*** Beg Del *** 08/05/13 ****************************************
     C*** Beg Add *** 05/18/10 ********************************************
     C*****              if        cmmbs = 'Y'
     C*****
     C*****              call      'S2010R'
     C*****              parm      '2'           ##CALL            1
     C*****              parm      *BLANKS       ##KC              1
     C*****              parm      *BLANKS       ##KL              1
     C*****              parm      *BLANKS       ##ICD9            5
     C*****              parm      *BLANKS       ##LOST            1
     C*****              parm      'DI'          ##AREA            2
     C*****
     C*****              else
     C*****
     C*** End Add *** 05/18/10 ********************************************
     C*****              call      'S2010R'
     C*****              parm      '2'           ##CALL            1
     C*****              parm      *BLANKS       ##KC              1
     C*****              parm      *BLANKS       ##KL              1
     C*****              parm      *BLANKS       ##ICD9            5
     C*****              parm      *BLANKS       ##LOST            1
     C*****
     C*** Beg Add *** 05/18/10 ********************************************
     C*****              endif
     C*****
     C*** End Add *** 05/18/10 ********************************************
     C*** End Del *** 08/05/13 ****************************************
     C     ##KC          ifEQ      '1'
     C                   MOVE      '1'           OUTFLG            1
     C                   endif
     C*
     C     ##KL          ifEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C                   endif
     C*
     C     ##ICD9        ifNE      *BLANKS
     C                   MOVEL(P)  ##ICD9        SCICD9
     C                   MOVEL(P)  ##LOST        SCLOST
     C                   MOVEL     SCICD9        KKICD9
     C                   MOVEL     SCLOST        KKLOST
     C*** Beg Chg *** 08/05/13 ****************************************
      /Free
       If xxicd10 ='1' and scICdInd = 'Y';
          scDSc1 = *blanks;
          kkicd10= ##icd9;
          Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
               scDSc1 = ICdsc1;
            Endif;
       else;
      /End-Free
     C*** End Chg *** 08/05/13 ****************************************
     C     DIKEY         chain     pdi00                              99
     C     *IN99         ifEQ      '0'
     C                   MOVEL     DIDSC1        SCDSC1
     C                   endif
     C*** Beg Chg *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Chg *** 08/05/13 ****************************************
     C                   seton                                        9030      IN30, POSITION
     C                   endif
     C*
     C                   endif                                                  HFLD ifEQ
     C*
     C* LOCATION SEARCH
     C*
     C     HFLD          ifEQ      'SCILOC'
     C                   MOVEL(P)  ILOC          ##TBID
     C                   EXSR      GENTBL
     C*
     C     *INKL         ifEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   seton                                        9031      31, POS CURSOR
     C                   ELSE
     C*
     C     ##RRN         chain     pgt0p                              99
     C                   seton                                        9032      IN32, POSITION
     C                   MOVEL     GTKEY         SCILOC
     C                   MOVEL     GTDESC        SCLDSC
     C*
     C                   endif
     C*
     C                   endif                                                  HFLD ifEQ
     C*
     C* SITE SEARCH
     C*
     C     HFLD          ifEQ      'SCISIT'
     C                   MOVEL(P)  ISIT          ##TBID
     C                   EXSR      GENTBL
     C*
     C     *INKL         ifEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   seton                                        9032      32, POS CURSOR
     C                   ELSE
     C*
     C     ##RRN         chain     pgt0p                              99
     C*
     C                   MOVEL     GTKEY         SCISIT
     C                   MOVEL(P)  GTDESC        SCSDSC
     C                   seton                                        9033      IN33, POSITION
     C                   endif
     C*
     C                   endif                                                  HFLD ifEQ
     C*
     C* BWC STATUS SEARCH
     C*
     C     HFLD          ifEQ      'SCISTS'
     C                   MOVEL     'ISTS'        ##TBID
     C                   EXSR      GENTBL
     C*
     C     *INKL         ifEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   seton                                        9033      33, POS CURSOR
     C                   ELSE
     C*
     C                   seton                                        9034      IN34, POSITION
     C*
     C     ##RRN         chain     pgt0p                              99
     C*
     C                   MOVEL     GTKEY         SCISTS
     C                   MOVEL     GTDESC        SCSTSD
     C*
     C                   endif
     C*
     C                   endif                                                  HFLD ifEQ
     C*
     C                   seton                                        90
     C*
     C                   ENDSR
     C*====================================================================
     C* KNDO - F14=DARTS.
     C*====================================================================
     C     KNDO          BEGSR
     C*
     C                   EXSR      DATCVT
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C*
     C                   CALL      'FA703R'
     C                   parm      *BLANK        ##RTCD            1
     C                   parm                    ##WCIN            1
     C                   parm                    ##WCDK            1
     C                   parm                    #1CO#             3
     C                   parm                    #1SUB#            5
     C                   parm                    #1FND             3
     C                   parm                    #1FYR             3
     C                   parm                    #1CASE            7
     C                   parm      *BLANK        ##KEY            40
     C*
     C                   MOVE      '1'           *IN90
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* Update maintenance fields
     C*====================================================================
     C     MaintFlds     BEGSR
     C*
     C                   eval      DAUSER = q1User
     C                   eval      DAPGM  = q1Pgm
     C                   eval      DATIME = TimeTo6(SystemTime)
     C                   eval      DADT   = DateTo7(SystemDate)
     C*
     C                   if        addMode
     C                   eval      DASTAT = 'A'
     C                   eval      DAIUSR = DAUSER
     C                   eval      DAIPGM = DAPGM
     C                   eval      DAITIM = DATIME
     C                   eval      DAIDT  = DADT
     C                   endif
     C*
     C                   ENDSR
     C*====================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*====================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     CMCO#         DACO#
     C                   Z-ADD     CMSUB#        DASUB#
     C                   Z-ADD     CMFND         DAFND
     C                   MOVE      CMFYR         DAFYR
     C                   Z-ADD     CMCASE        DACASE
     C                   MOVEL     SCICD9        DAICD9
     C                   MOVE      SCLOST        DALOST
     C                   MOVE      SCPRIM        DAPRIM
     C                   MOVEL     SCILOC        DAILOC
     C                   MOVEL     SCISIT        DAISIT
     C                   MOVEL     SCISTS        DAISTS
     C                   eval      DAISDT = DtCYMD(scIsDt)
     C                   eval      DAEXDT = DtCYMD(scExDt)
     C*
     C                   if        addMode
     C                   Z-ADD     XXSEQ#        DASEQ#
     C                   endif
     C*
     C                   exsr      MaintFlds
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
          //Beg Chg *** 08/05/13 ***************************************
          //If xxicd10 = *on;
          DAicdind  = scICdind;
          If xxicd10 = *on and scICdInd ='Y';
          //End Chg *** 08/05/13 ***************************************
             DAicd1001 = scICd9;
             DAicdind  = scICdind;

             // If ICD10 is Active then no need of ICD9 fields

             DAIcd9 = *blanks;
          //Beg Chg *** 08/05/13 ***************************************
          else;
             DAicd1001 = *Blanks;
          //End Chg *** 08/05/13 ***************************************
          Endif;
      /End-Free
     C*** End Add *** 05/23/13 ****************************************
     C*
     C                   ENDSR
     C*====================================================================
     C* OTHERR - Processing for other modes
     C*====================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*====================================================================
     C* OTHFKS - Processing other fucntion keys
     C*====================================================================
     C     OTHFKS        BEGSR
     C*
     C* SET OF INDICATORS FOR POSISTION CURSORS
     C*
     C                   SETOFF                                       303132
     C                   SETOFF                                       3334
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C     *INKN         CASEQ     *ON           KNDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*====================================================================
     C* PGMEND - End of program processing
     C*====================================================================
     C     PGMEND        BEGSR
     C*
     C                   seton                                        LR
     C*
     C                   ENDSR
     C*====================================================================
     C* PGMSTR - Start of program processing
     C*====================================================================
     C     PGMSTR        BEGSR
     C*
     C                   if        %parms >= 6
     C                   eval      xxArea = @@Area
     C                   endif
     C*
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'D'                                    09
     C*
     C                   eval      addMode = @@Mode = 'A'
     C                   eval      chgMode = @@Mode = 'C'
     C                   eval      dspMode = @@Mode = 'D'
     C*
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
     C                   CALL      'SA001R'
     C                   parm                    ##WCif            1            WC INST
     C                   parm                    ##MCif            1            MC INST
     C                   parm                    ##WCIN            1            WC INTERFACE
     C                   parm                    ##AR              1            MC AR INST
     C                   parm                    ##NTWK            1            NETWORK INST
     C                   parm                    ##UR              1            PRECERT INST
     C                   parm                    ##MED             1            MEDMGMT INST
     C                   parm                    ##EDI             1            EDI INST
     C                   parm                    ##WCDK            1            WC DARTS KEY
     C*
     C                   eval      scPrDs = CenterFld(optNm:%size(scPrDs))
     C*
     C                   MOVE      *ON           DSPTYP
     C*
     C* Get claim master record
     C                   if        not clmFiledOpened
     C                   select
     C                   when      xxArea = 'MC'
     C                   open      fmcmp
     C     @@rrn2        chain     mcm0p                              99
     C                   clear                   CMMBS
     C                   other
     C                   open      wmcmp
     C     @@rrn2        chain     wcm0p                              99
     C                   clear                   CMSUB#
     C*** Beg Add *** 01/21/11 ************************************************
     C*** Beg Del *** 03/28/11 ************************************************
     C*
     C*********          if        ##mcif = 'Y'
     C*********          call      'FA007R'      compl
     C*
     C*********          if        cmsub# = 0
     C*********          move      ##sub#        cmsub#
     C*********          endif                                                  cmsub# = 0
     C*
     C*********          endif                                                  ##mcif = 'Y'
     C*** End Del *** 03/28/11 ************************************************
     C*** End Add *** 01/21/11 ************************************************
     C                   endsl
     C                   eval      clmFiledOpened = *on
     C                   endif
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      CMCO#         #1CO#             3
     C                   MOVE      CMSUB#        #1SUB#            5
     C                   MOVE      CMFND         #1FND             3
     C                   MOVE      CMFYR         #1FYR             3
     C                   MOVE      CMCASE        #1CASE            7
     C*
     C                   select
     C                   when      xxArea = 'MC'
     C                   call      'FA702R'
     C                   parm      *BLANK        ##RTCD            1
     C                   parm                    #1CO#
     C                   parm                    #1SUB#
     C                   parm                    #1FND
     C                   parm                    #1FYR
     C                   parm                    #1CASE
     C     SCL1HD        parm      *BLANK        ##L1             78
     C     SCL2HD        parm      *BLANK        ##L2             78
     C                   other
     C                   call      'WA006R'
     C                   parm      CMCO#         #2CO#             3 0
     C                   parm      CMFND         #2Fnd             3 0
     C                   parm      CMFYR         #2Fyr             3 0
     C                   parm      CMCASE        #2Case            7 0
     C                   parm                    #2Cs01           79
     C                   parm                    #2Cs02           79
     C                   eval      scL1Hd = #2Cs01
     C                   eval      scL2Hd = #2Cs02
     C                   endsl
     C*
     C* GET KEY LIST VALUES.
     C*
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
     C*
     C* Determine the table ID's
     C*  Location
     C                   eval      ILOC = CMAST + 'IL'
     C                   eval      kkTId = ILOC
     C     GIKY          chain     pgi00                              99
     C                   if        not %found
     C                   eval      ILOC = 'ZZIL'
     C                   endif
     C*
     C*  Site
     C                   eval      ISIT = CMAST + 'IS'
     C                   eval      kkTId = ISIT
     C     GIKY          chain     pgi00                              99
     C                   if        not %found
     C                   eval      ILOC = 'ZZIS'
     C                   endif
     C*** Beg Add *** 05/23/13 ****************************************
      /Free

         // Call System API to check wheather the ICD10 is active or not

         @@rtcd = *blanks;
         callp SA015R(@@rtcd);

         Select;
         // ICD10 is inactive
         When @@rtcd = '1';
         xxicd10 = '0';
         DspIcdInd = '1';

         // ICD10 is active
         When @@rtcd = '2';
         xxicd10 = '1';
         DspIcdInd = '0';
         // Beg Add *** 08/05/13 ****************************************
         scICdInd ='Y';
         // End Add *** 08/05/13 ****************************************
         Endsl;

         If @@MODE = 'D';
            DspicdInd = '1';
         Endif;
      /End-Free
     C*** End Add *** 05/23/13 ****************************************
     C*
     C                   ENDSR
     C*====================================================================
     C* PREOTH - Pre-processing for other modes
     C*====================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*====================================================================
     C* UPDATR - Update data base record after processing
     C*====================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         chain     DBRCDF                             99
     C*
     C                   if        %found
     C                   exsr      MvScDb
     C                   UPDATE    DBRCDF
     C                   endif
     C*
     C* Ensure WCMC is kept in sync
     C                   callp(e)  UpdateMC(dbrrn:'U':'SY':'MC')
     C*
     C                   ENDSR
     C*====================================================================
     C* WRITER - Write data base record after processing
     C*====================================================================
     C     WRITER        BEGSR
     C*
     C* GET THE SEQUENCE NUMBER
     C*
     C                   clear                   DASEQ#
     C                   eval      kkSeq# = *hival
     C     DAKY3         SETGT     pda00
     C     DAKY4         READPE    pda00                                  99
     C                   eval      xxSeq# = DASEQ# + 1
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   WRITE     DBRCDF
     C*
     C                   MOVE      '1'           OUTFLG
     C*
     C* Ensure WCMC is kept in sync
     C                   callp(e)  UpdateMC(dbrrn:'U':'SY':'MC')
     C*
     C                   ENDSR
