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
     H*  03/28/11   C:198  Project: WC1879 Priss 142
     H*                    -display all ICD9 codes from WC side when taken
     H*                     from WW claims.
     H*                    -display all ICD9 codes from MC side when taken
     H*                     from Managed care side.
     H*
     H*  01/21/11   C:198  Project: WC1879 Priss 142
     H*                    -display CMS flag I codes from WC side.
     H*
     H*  08/12/09  C:227   Project : SY184
     H*                              Priss 16 Diagnosis code not being
     H*                              displayed after entering.
     H*====================================================================
     Fs0275f    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     Fsadap     IF   E             DISK
     F                                     RENAME(PDA01:PFRCDF)
     Fsadal     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PDA01:DBRCDF)
     Fsadal1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PDA01:DBRCDF2)
     Ffmcmp     IF   E             DISK    rename(pcm01:mcm0p) usropn
     Fwmcmp     IF   E             DISK    rename(pcm01:wcm0p) usropn
     Fsmdil     IF   E           K DISK
     Fstgtl     IF   E           K DISK
     F*** Beg Add *** 08/05/13 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/05/13 ***************************************
     D*
     D HD              s             78    dim(2) ctdata perrcd(1)              Column Headings
     D#ofOpt           c                   12
     D of              s              1    dim(#ofOpt) ctdata perrcd(1)         Options, Function
     D ofd             s             35    dim(#ofOpt) alt(of)
     D DATA            s             74    dim(2)
     D DTA1            s              1    dim(74)
     D DTA2            s              1    dim(74)
     D*
     D/copy sCopyWW43d
     D*
     D* /COPY required constants
     D*
     D OPTNM           c                   CONST('Diagnosis Information')
     D ACDPGM          c                   CONST('S0275R02')
     D SLMSGI          c                   CONST('SYS0001')
     D SLMSGF          c                   CONST('SYMESG')
     D COMAPI          c                   CONST('WA001R')
     D USPAPI          c                   CONST('F0006R')
     D SF1PAG          c                   CONST(7)
     D INFOMX          c                   CONST(1)
     D VIEWMX          c                   CONST(2)
     D deleted         c                   CONST('* * * Deleted')
     D @@Area          s              2
     D xxArea          s                   like(@@Area)
     D clmFiledOpened  s              1n
     D*** Beg Add *** 08/05/13 ***************************************
     D RtvICD10Code    pr                  extpgm('F2554R')
     D incall                         1
     D inKC                           1
     D inKL                           1
     D outICD10                       7
     D inLOST                         1
     D*
     D incall          s              1
     D inKC            s              1
     D inKL            s              1
     D outICD10        s              7
     D inLOST          s              1
     D kkicd10         s                   like(iccode)
     D*** End Add *** 08/05/13 ***************************************
     D*
     D* Program specific constants
     D*
     D                 DS
     D  HLISDT                 1      8
     D  H1ISDT                 1      2
     D  H2ISDT                 3      4
     D  H3ISDT                 5      8
     D*
     DRtvMCRRN         pr                  extPgm('FA275R')
     D rrn                            9  0
     D mode                           1    const
     D fromFile                       2    const
     D toFile                         2    const
     D*
     DGetMCRRN         pr             9  0
     D*
     D/copy sPrDate
     D/copy sPrMessage
     D/copy sPrRunCmd
     D/copy sPrString
     D/copy sPrWrkWth
     D*
     Imcm0p
     I              CMOSCL                      C2OSCL
     I*
     C     *LIKE         DEFINE    DACO#         KKCO#
     C     *LIKE         DEFINE    DASUB#        KKSUB#
     C     *LIKE         DEFINE    DAFND         KKFND
     C     *LIKE         DEFINE    DACASE        KKCASE
     C     *LIKE         DEFINE    DASEQ#        KKSEQ#
     C     *LIKE         DEFINE    CMACDT        KKACDT
     C     *LIKE         DEFINE    CMEMP#        KKEMP#
     C     *LIKE         DEFINE    CMDIV         KKDIV
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    DAICD9        KKICD9
     C     *LIKE         DEFINE    DALOST        KKLOST
     C     *LIKE         DEFINE    DAFYR         KKFYR
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKTKEY
     C*
     C     *ENTRY        plist
     C                   parm      *INKC         @@kc              1
     C                   parm      *INKL         @@kl              1
     C                   parm                    @@Call            1
     C                   parm                    @@rrn             9 0          RRN FADAP
     C                   parm                    @@rrn2            9 0          RRN FMCMP
     C                   parm                    @@Area
     C*
     C     ACDPL         plist
     C     *INKC         parm      '0'           ##KC              1
     C     *INKL         parm      '0'           ##KL              1
     C                   parm                    ##MODE            1
     C                   parm      SCDBRR        ##RRN             9 0
     C                   parm      @@rrn2        ##RRN2            9 0
     C*** BEG ADD *** 08/12/09 *********************************************
     C                   parm                    @@Area
     C*** END ADD *** 08/12/09 *********************************************
     C*
     C     ACDPL2        plist
     C     *INKC         parm      '0'           ##KC              1
     C     *INKL         parm      '0'           ##KL              1
     C                   parm      @@Call        ##CALL            1
     C                   parm      *ZEROS        ##RRN             9 0
     C                   parm      @@rrn2        ##RRN2            9 0
     C*
     C     COMPL         plist
     C                   parm                    ##CO#             3 0
     C                   parm                    ##FND             3 0
     C                   parm                    ##CONM           40
     C                   parm                    ##FNAM           40
     C                   parm                    ##SUB#            5 0
     C                   parm                    ##FYR             3 0
     C*
     C     KEY1          klist
     C                   kfld                    KKCO#
     C                   kfld                    KKSUB#
     C                   kfld                    KKFND
     C                   kfld                    KKFYR
     C                   kfld                    KKCASE
     C*
     C     KEY2          klist
     C                   kfld                    KKCO#
     C                   kfld                    KKSUB#
     C                   kfld                    KKFND
     C                   kfld                    KKFYR
     C                   kfld                    KKCASE
     C                   kfld                    KKSEQ#
     C                   kfld                    KKICD9
     C*
     C     KEY3          klist
     C                   kfld                    KKCO#
     C                   kfld                    KKSUB#
     C                   kfld                    KKFND
     C                   kfld                    KKFYR
     C                   kfld                    KKCASE
     C                   kfld                    KKICD9
     C*
     C     DIKEY         klist
     C                   kfld                    KKICD9
     C                   kfld                    KKLOST
     C*
     C*** Beg Add *** 01/21/11 **********************************
     C*** Beg Del *** 03/28/11 **********************************
     C**** DIKY2         klist
     C*********          kfld                    KKICD9
     C*
     C*** End Del *** 03/28/11 **********************************
     C*** End Add *** 01/21/11 **********************************
     C*  KEY LIST FOR GENERIC TABLE FILE
     C*
     C     XTKY          klist
     C                   kfld                    KKCO#
     C                   kfld                    KKTBID
     C                   kfld                    KKTKEY
     C*** Beg Add *** 08/05/13 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C*** End Add *** 08/05/13 ***************************************
     C*
     C/copy sCopyWW43c
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   if        scSel = ' 4' or scSel = '4 '
     C*          ICD9 can only be deleted if BWC status is blank
     C                   if        scISts <> *blanks
     C                   seton                                        71  90
     C                   leavesr
     C                   endif
     C                   endif                                                  SEL = 4
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   if        %parms >= 6
     C                   eval      xxArea = @@Area
     C                   endif
     C*
     C* Get claim master record
     C*
     C                   if        not clmFiledOpened
     C                   select
     C                   when      xxArea = 'MC'
     C                   open      fmcmp
     C     @@rrn2        chain     mcm0p                              99
     C                   other
     C                   open      wmcmp
     C     @@rrn2        chain     wcm0p                              99
     C                   clear                   CMSUB#
     C*** Beg Add *** 01/21/11 ************************************************
     C*** Beg Del *** 01/21/11 ************************************************
     C*********          CALL      'SA001R'
     C*********          parm                    ##WCIF            1            WC INST
     C*********          parm                    ##MCIF            1            MC INST
     C*********          parm                    ##WCIN            1            WC INTERFACE
     C*********          parm                    ##AR              1            MC AR INST
     C*********          parm                    ##NTWK            1            NETWORK INST
     C*********          parm                    ##UR              1            PRECERT INST
     C*********          parm                    ##MED             1            MEDMGMT INST
     C*********          parm                    ##EDI             1            EDI INST
     C*********          parm                    ##WCDK            1            WC DARTS KEY
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C*********          if        ##mcif = 'Y'
     C*********          call      'FA007R'      compl
     C*
     C*********          if        cmsub# = 0
     C*********          move      ##sub#        cmsub#
     C*********          endif                                                  cmsub# = 0
     C*
     C*********          endif                                                  ##mcif = 'Y'
     C*** End Del *** 01/21/11 ************************************************
     C*** End Add *** 01/21/11 ************************************************
     C                   endsl
     C                   eval      clmFiledOpened = *on
     C                   endif
     C*
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
     C*
     C                   Z-ADD     0             KKSEQ#
     C                   MOVEL     *LOVAL        KKICD9
     C                   MOVEL     *LOVAL        KKSEQ#
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   Z-ADD     0             KKSEQ#
     C                   MOVE      *BLANKS       KKICD9
     C*
     C** FOR VIEW#1 SET KEYS FOR SEQ# AND ICD9 CODE
     C*
     C     *IN21         IFEQ      *ON
     C*
     C                   MOVEL(P)  SCPST2        KKICD9
     C*
     C** TRANSLATE BWC STATUS INTO SEQ#
     C*
     C     SCPST1        IFEQ      *BLANKS
     C                   Z-ADD     99000         KKSEQ#
     C                   ELSE
     C                   MOVEL     'ISTS'        KKTBID
     C                   MOVEL(P)  SCPST1        KKTKEY
     C*
     C     XTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     GTN501        KKSEQ#
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C                   ENDIF                                                  SCPST1 IFEQ BLANKS
     C                   ENDIF                                                  *IN21 IFEQ *ON
     C*
     C** FOR VIEW#2 SET KEYS FOR ICD9 CODE
     C*
     C   22              MOVEL     SCPST3        KKICD9
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL     SCICD9        KKICD9
     C   21              Z-ADD     SCSEQ#        KKSEQ#
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS.
     C*================================================================
     C     KNDO          BEGSR
     C*
     C                   select
     C*___________
     C*
     C                   when      xxArea = 'MC'
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE                   90
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
     C*___________
     C*
     C                   other
     C*
     C                   eval      ##Key = %editc(CMCO# :'X') +
     C                                     %editc(CMFND :'X') +
     C                                     %editc(CMFYR :'X') +
     C                                     %editc(CMCASE:'X')
     C*
     C                   call      'SC010R'
     C                   parm                    ##Key            40
     C                   parm      'W0070R'      ##Pgm            10
     C*___________
     C*
     C                   endsl
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      *BLANKS       SCICD9
     C                   MOVE      *BLANKS       SCPRIM
     C                   MOVE      *BLANKS       SCLOST
     C                   MOVE      *BLANKS       SCISTS
     C                   MOVE      *ZEROS        SCISDT
     C                   MOVE      *BLANKS       SCDSC1
     C*
     C* GET ONLY ACTIVE RECORDS
     C*
     C     DASTAT        IFNE      'I'
     C                   MOVEL     DAICD9        SCICD9
     C                   MOVE      DAPRIM        SCPRIM
     C                   MOVE      DALOST        SCLOST
     C                   MOVE      DAISTS        SCISTS
     C*
     C                   eval      scIsDt = DtMDYY(DAISDT)
     C     SCISDT        IFNE      *ZEROS
     C*
     C                   MOVE      SCISDT        HLISDT
     C     H1ISDT        CAT       '/'           T1ISDT            3
     C     T1ISDT        CAT       H2ISDT        T2ISDT            5
     C     T2ISDT        CAT       '/'           T3ISDT            6
     C     T3ISDT        CAT       H3ISDT        ALISDT           10
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       ALISDT
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     DASEQ#        SCSEQ#
     C*
     C                   MOVE      SCICD9        KKICD9
     C                   MOVE      SCLOST        KKLOST
     C*
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If Daicdind ='Y';
          KKICD10 = DAICD1001;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               SCDSC1 = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     DIKEY         CHAIN     PDI01                              99
     C*
     C     *IN99         IFEQ      '0'
     C                   MOVEL     DIDSC1        SCDSC1
     C                   ENDIF
     C*** End Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   SELECT
     C     HD#           WHENEQ    1
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     DAISTS        DTA1(Y)
     C                   ADD       4             Y
     C***** End Chg ***** 08/05/13 ***********************************
     C*****              MOVEA     SCICD9        DTA1(Y)
     C                   if        DAICDIND = 'Y'
     C                   movea     DAICD1001     DTA1(Y)
     C                   else
     C                   movea     SCICD9        DTA1(Y)
     C                   endif
     C***** End Chg ***** 08/05/13 ***********************************
     C                   ADD       7             Y
     C                   MOVEA     SCPRIM        DTA1(Y)
     C                   ADD       4             Y
     C                   MOVEA     SCLOST        DTA1(Y)
     C                   ADD       4             Y
     C                   MOVEA     SCDSC1        DTA1(Y)
     C                   ADD       41            Y
     C                   MOVEA     ALISDT        DTA1(Y)
     C                   MOVEA(P)  DTA1          SCDATA
     C*
     C     HD#           WHENEQ    2
     C                   Z-ADD     1             Y
     C***** Beg Chg ***** 08/05/13 ***********************************
     C*****              MOVEA     SCICD9        DTA2(Y)
     C                   if        DAICDIND = 'Y'
     C                   movea     DAICD1001     DTA2(Y)
     C                   else
     C                   movea     SCICD9        DTA2(Y)
     C                   endif
     C***** End Chg ***** 08/05/13 ***********************************
     C                   ADD       8             Y
     C                   MOVEA     SCPRIM        DTA2(Y)
     C                   ADD       4             Y
     C                   MOVEA     SCLOST        DTA2(Y)
     C                   ADD       4             Y
     C                   MOVEA     SCDSC1        DTA2(Y)
     C                   ADD       40            Y
     C                   MOVEA     SCISTS        DTA2(Y)
     C                   ADD       5             Y
     C                   MOVEA     ALISDT        DTA2(Y)
     C                   MOVEA(P)  DTA2          SCDATA
     C                   ENDSL
     C*
     C                   ENDIF                                                  DASTAT IFNE
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKN         CASEQ     *ON           KNDO                     90
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   if        clmFiledOpened
     C                   select
     C                   when      xxArea = 'MC'
     C                   close     fmcmp
     C                   other
     C                   close     wmcmp
     C                   endsl
     C                   eval      clmFiledOpened = *off
     C                   endif
     C*
     C                   eval      *inlr = *on
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C* CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
     C*
     C                   CALL      'SA001R'
     C                   parm                    ##WCIF            1            WC INST
     C                   parm                    ##MCIF            1            MC INST
     C                   parm                    ##WCIN            1            WC INTERFACE
     C                   parm                    ##AR              1            MC AR INST
     C                   parm                    ##NTWK            1            NETWORK INST
     C                   parm                    ##UR              1            PRECERT INST
     C                   parm                    ##MED             1            MEDMGMT INST
     C                   parm                    ##EDI             1            EDI INST
     C                   parm                    ##WCDK            1            WC DARTS KEY
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C*
     C                   MOVE      CMCO#         #1CO#             3
     C                   MOVE      CMSUB#        #1SUB#            5
     C                   MOVE      CMFND         #1FND             3
     C                   MOVE      CMFYR         #1FYR             3
     C                   MOVE      CMCASE        #1CASE            7
     C*
     C                   select
     C                   when      xxArea = 'MC'
     C                   CALL      'FA702R'
     C                   parm      *BLANK        ##RTCD            1
     C                   parm                    #1CO#
     C                   parm                    #1SUB#
     C                   parm                    #1FND
     C                   parm                    #1FYR
     C                   parm                    #1CASE
     C     SCL1HD        parm      *BLANK        #1L1             78
     C     SCL2HD        parm      *BLANK        #1L2             78
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
      *
     C*
     C                   Z-ADD     2             VIEW#
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   eval      scData = deleted
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   eval      @@rrn = scDbRR
     C*
     C                   select
     C                   when      @@Area = 'MC'
     C                   eval      @@rrn = GetMCRRN
     C                   endsl
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*** Beg Add *** 01/21/11 ********************************************
     C*** Beg Del *** 03/28/11 ********************************************
     C*** search daicd9 in SMDIP to see whether it has CMS fag as I or not
     C** if called from WC side.
     C*********          MOVE      DAICD9        KKICD9
     C*
     C*********          if        xxarea <> 'MC'
     C*********                    and cmmbs = 'Y'
     C**** DIKY2         chain     pdi01                              99
     C*********          if        *in99 = *off and dicms <> 'I'
     C*********          eval      meetSS = *off
     C*********          endif                                                  dicms <> 'I'
     C*********          endif                                                  xxarea <> 'MC'
     C*** End Del *** 03/28/11 ********************************************
     C*** End Add *** 01/21/11 ********************************************
     C                   ENDSR





     P*====================================================================
     P* Get the MC RRN
     P*====================================================================
     PGetMCRRN         b
     DGetMCRRN         pi             9  0
     D*
     D inRRN           s              9  0

     C                   eval      inRRN = scDbRr
     C                   callp     RtvMCRRN(inRRN:'R':'SY':'MC')
     C                   return    inRRN

     PGetMCRRN         e





** Column Headings
Opt Sts Diag Prim Typ  Description                                 Date
Opt Diag  Prim Typ  Description                             Sts    Date
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               YYY
214F14=DARTS                YYY
3
