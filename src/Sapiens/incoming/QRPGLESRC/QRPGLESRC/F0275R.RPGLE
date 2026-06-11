     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H****************************************************************
     H* PROGRAM: F0275R                                              *
     H* DESCRIPTION: DAIGNOSIS INFORMATION FILE PROGRAM              *
     H****************************************************************
     H* INDICATORS:                                                  *
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                               *
     H*  06/09/20  C:212 Project : SY420                              *
     H*                  Mask SSN                                     *
     H*                                                              *
     H*  06/09/15 C:79 Support: MC144 35218                          *
     H*                Pass back parms from S0275R                   *
     H*                                                              *
     H*  05/20/09 C:33 Project: WC1770                               *
     H*                This program will now only call the system    *
     H*                 version.                                     *
     H*                                                              *
     H* 11/19/98 C:62  SUPPORT 15160                                 *
     H*                CHANGED SF1PAG TO 7 TO MATCH SFLPAG.          *
     H*                                                              *
     H* 06/09/98 C:94  MC145                                         *
     H*                1) SWITCHED VIEW 1 TO VIEW 2 AND VIEW 2 TO    *
     H*                   VIEW 1.                                    *
     H*                2) MOVED DIAG. COLUMN NEXT TO STS COLUMN      *
     H*                                                              *
     H* 03/30/98 C:47  87MCB                                         *
     H*                ADDED ADDITIONAL PARM VALUE FOR @@CALL:       *
     H*                'S' TO USE ONLY THIS PGM FOR A SEARCH.        *
     H*                                                              *
     H* 02/24/98 C:62  MC145 #9115                                   *
     H*             1. Correct duplication of records when paging    *
     H*                down.                                         *
     H*             2. Add BWC Sts as a "positon to field"           *
     H*             3. Add second view with display by ICD9 code     *
     H*                                                              *
     H*  02/03/98 C:62  Project: MC145                               *
     H*                 1) SCEL ON SCR02S (DELETE) IS NOW OUTPUT.    *
     H*                 2) F5=REFRESH HAS BEEN ADDED.                *
     H*                                                              *
     H*  01/08/98 C:86 PROJECT: MC145                                *
     H*               DISPLAY FILE F0275F HAS BEEN CHANGED TO PRINT  *
     H*               A 2 LINE SUB-HEADING. THIS PROGRAM CALLS FA702R*
     H*               TO PRINT IT & USES LIKE DEFINES RO REDEFINE THE*
     H*               OMMITTED SCREEN FIELDS.                        *
     H*                                                              *
     H*  08/25/97 C:31  Project: 87MAZ  CO# 48                       *
     H*                 Remove case management fields                *
     H*                 (Compile only)                               *
     H*                                                              *
     H*  08/21/97 C:31  Project: 87MAZ  CO# 32                       *
     H*                 1. Add option 4=Delete                       *
     H*                                                              *
     H*  06/24/97 C:56  87MAZ                                        *
     H*                 ADD @@KC AND @@KL TO ENTRY PARM LIST         *
     H*                                                              *
     H*  06/16/97 C:56  87MAZ  CO# 22                                *
     H*                 CLEAR FIELDS IN SUBFILE IF NO INFORMATION    *
     H*                 IS AVALIABLE                                 *
     H*                                                              *
     H*  02/18/97 C:56  87MAZ                                        *
     H*                 CREATED                                      *
     H*                                                              *
     H****************************************************************
     FF0275F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FFADAP     IF   E             DISK
     F                                     RENAME(PDA01:PFRCDF)
     FFADAL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PDA01:DBRCDF)
     F*** BEGIN ADD *** 02/24/98 **************************************
     FFADAL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PDA01:DBRCDF2)
     F*** END   ADD *** 02/24/98 **************************************
     FFMCMP     IF   E             DISK
     F********   FMCDL   IF  E           K        DISK
     FFMDIL     IF   E           K DISK
     F*** BEGIN ADD *** 02/24/98 **************************************
     FFTGTL     IF   E           K DISK
     F*** END   ADD *** O2/24/98 **************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column Headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D*** BEGIN ADD *** 02/24/98 **************************************
     D DATA            S             74    DIM(2)
     D DTA1            S              1    DIM(74)
     D DTA2            S              1    DIM(74)
     D*** END   ADD *** 02/24/98 **************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D* /COPY required constants
     D*
     D OPTNM           C                   CONST('Diagnosis Informati-
     D                                     on')
     D ACDPGM          C                   CONST('F0275R01')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D*** BEGIN CHG *** 11/19/98 **************************************
     D**********    9                     C         SF1PAG
     D SF1PAG          C                   CONST(7)
     D*** END   CHG *** 11/19/98 **************************************
     D INFOMX          C                   CONST(1)
     D*** BEGIN CHG *** 02/24/98 **************************************
     D**********    1                     C         VIEWMX
     D VIEWMX          C                   CONST(2)
     D*** END   CHG *** 02/24/98 **************************************
     D*
     D* Program specific constants
     D*
     D*** BEGIN ADD *** 02/24/98 **************************************
     D                 DS
     D  HLISDT                 1      8
     D  H1ISDT                 1      2
     D  H2ISDT                 3      4
     D  H3ISDT                 5      8
     D*** END   ADD *** 02/24/98 **************************************
     D DL              C                   CONST('* * * DELETED * * *')

     I*

     C*
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
     C************ BEGIN ADD ******************** 01/08/98 ************
     C     *LIKE         DEFINE    DAFYR         KKFYR
     C************ END   ADD ******************** 01/08/98 ************
     C*** BEGIN ADD *** 02/24/98 **************************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKTKEY
     C*** END   ADD *** 02/24/98 **************************************
     C*
     C     *ENTRY        PLIST
     C*** BEGIN ADD *** 06/24/97 **************************************
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C*** END ADD *** 06/24/97 ****************************************
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          RRN FADAP
     C                   PARM                    @@RRN2            9 0          RRN FMCMP
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C*
     C     ACDPL2        PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKSEQ#
     C                   KFLD                    KKICD9
     C*** BEGIN ADD *** 02/24/98 **************************************
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKICD9
     C*** END   ADD *** 02/24/98 **************************************
     C********* BEGIN DELETE *******************8 01/08/97 ************
     C*********  CDKEY     KLIST
     C*********            KFLD           XXSS#
     C********* END   DELETE *******************8 01/08/97 ************
     C*
     C     DIKEY         KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*** BEGIN ADD *** 02/24/98 **************************************
     C*
     C*  KEY LIST FOR GENERIC TABLE FILE
     C*
     C     XTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKTKEY
     C*** END   ADD *** 02/24/98 **************************************
     C*
     C* IF CALLED SEPERATED FROM PROGRAM CHAIN.
     C*
     C                   MOVEL     *OFF          STOP              1
     C*
     C                   SELECT
     C*
     C     @@CALL        WHENEQ    'L'
     C                   MOVEL     '1'           @@CALL
     C                   MOVEL     *ON           STOP
     C*
     C     @@CALL        WHENEQ    'M'
     C                   MOVEL     ' '           @@CALL
     C                   MOVEL     *ON           STOP
     C*
      *** BEGIN ADD ** 03/30/98 ************************************************
     C*
     C     @@CALL        WHENEQ    'S'
     C                   MOVEL(P)  '2'           @@CALL
     C                   MOVEL     *ON           STOP
     C*
      *** END   ADD ** 03/30/98 ************************************************
     C                   ENDSL
     C*
     C/COPY SCOPYWW01C
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
     C*** BEGIN ADD ** 08/21/97 ************************************************
     C*
     C*          ICD9 can only be deleted if BWC status is blank
     C*          ------------------------------------------------------
     C     SCSEL         IFEQ      ' 4'
     C     SCISTS        ANDNE     *BLANKS
     C     SCSEL         OREQ      '4 '
     C     SCISTS        ANDNE     *BLANKS
     C                   SETON                                        7190
     C                   ENDIF                                                  SEL = 4
     C*** END ADD ** 08/21/97 **************************************************
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
     C* GET CLAIM MASTER RECORD.
     C*
     C     @@RRN2        CHAIN     PCM01                              99
     C*
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
     C*
     C**********           Z-ADD1         KKSEQ#
     C                   Z-ADD     0             KKSEQ#
     C                   MOVEL     *LOVAL        KKICD9
     C*** BEGIN ADD *** 02/24/98 **************************************
     C                   MOVEL     *LOVAL        KKSEQ#
     C*** END   ADD *** 02/24/98 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C*** BEGIN DEL *** 02/24/98 **************************************
     C**********           MOVELSCPST1    KKICD9
     C*** END   DEL *** 02/24/98 **************************************
     C*** BEGIN ADD *** 02/24/98 **************************************
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
     C*** END   ADD *** 02/24/98 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL     SCICD9        KKICD9
     C*** BEGIN ADD *** 02/24/98 **************************************
     C   21              Z-ADD     SCSEQ#        KKSEQ#
     C*** END   ADD *** 02/24/98 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS.
     C*================================================================
     C     KNDO          BEGSR
     C*
     C************** BEGIN ADD ***************** 01/08/98 *************
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE                   90
     C*
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##WCIN            1
     C                   PARM                    ##WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY            40
     C*
     C************** END   ADD ***************** 01/08/98 *************
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*** BEGIN ADD *** 06/16/97 **************************************
     C                   MOVE      *BLANKS       SCICD9
     C                   MOVE      *BLANKS       SCPRIM
     C                   MOVE      *BLANKS       SCLOST
     C                   MOVE      *BLANKS       SCISTS
     C                   MOVE      *ZEROS        SCISDT
     C                   MOVE      *BLANKS       SCDSC1
     C*** END ADD *** 06/16/97 ****************************************
     C*
     C* GET ONLY ACTIVE RECORDS
     C*
     C     DASTAT        IFNE      'I'
     C                   MOVEL     DAICD9        SCICD9
     C                   MOVE      DAPRIM        SCPRIM
     C                   MOVE      DALOST        SCLOST
     C                   MOVE      DAISTS        SCISTS
     C*
     C                   Z-ADD     DAISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCISDT
     C*** BEGIN ADD *** 02/24/98 **************************************
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
     C*** END   ADD *** 02/24/98 **************************************
     C*
     C                   MOVE      SCICD9        KKICD9
     C                   MOVE      SCLOST        KKLOST
     C*
     C     DIKEY         CHAIN     PDI01                              99
     C*
     C     *IN99         IFEQ      '0'
     C                   MOVEL     DIDSC1        SCDSC1
     C                   ENDIF
     C*** BEGIN ADD *** 02/24/98 **************************************
     C                   SELECT
     C     HD#           WHENEQ    1
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     DAISTS        DTA1(Y)
     C*** BEG CHG *** 06/09/98 ******************************
     C                   ADD       4             Y
     C                   MOVEA     SCICD9        DTA1(Y)
     C***                  ADD  6         Y
     C                   ADD       7             Y
     C                   MOVEA     SCPRIM        DTA1(Y)
     C                   ADD       4             Y
     C                   MOVEA     SCLOST        DTA1(Y)
     C                   ADD       4             Y
     C                   MOVEA     SCDSC1        DTA1(Y)
     C***                  ADD  41        Y
     C***                  MOVEASCICD9    DTA1,Y
     C***                  ADD  6         Y
     C                   ADD       41            Y
     C                   MOVEA     ALISDT        DTA1(Y)
     C                   MOVEA(P)  DTA1          SCDATA
     C*** END CHG *** 06/09/98 ******************************
     C*
     C     HD#           WHENEQ    2
     C                   Z-ADD     1             Y
     C                   MOVEA     SCICD9        DTA2(Y)
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
     C*** END   DD *** 02/24/98 **************************************
     C*
     C                   ENDIF                                                  DASTAT IFNE
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKN         CASEQ     *ON           KNDO                           F14=DARTS
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
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     STOP          ANDEQ     *OFF
     C*
     C     ##MED         IFEQ      'Y'
     C*
     C                   CALL      'F0276R'      ACDPL2
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   MOVEL     *ON           RSTPGM
     C                   MOVEL     *OFF          *INKL
     C*
     C                   ELSE
     C*
     C                   MOVEL     '1'           *INLR
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   CALL      'F0277R'      ACDPL2
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   MOVEL     *ON           RSTPGM
     C                   MOVEL     *OFF          *INKL
     C*
     C                   ELSE
     C*
     C                   MOVEL     '1'           *INLR
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      '1'           *INLR
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C***  Begin Add  ***  05/20/09  **************************************
     C***  Beg Chg *** 06/09/15 *******************************************
     C**************
     C**************     CALL      'S0275R'
     C**************     PARM      @@KC          ##KC
     C**************     PARM      @@KL          ##KL
     C**************     PARM      @@CALL        ##CALL
     C**************     PARM      @@RRN         ##RRN
     C**************     PARM      @@RRN2        ##RRN2
     C**************     PARM      'MC'          ##AREA            2
     C                   CALL      'S0275R'
     C     @@KC          PARM      @@KC          ##KC
     C     @@KL          PARM      @@KL          ##KL
     C     @@CALL        PARM      @@CALL        ##CALL
     C     @@RRN         PARM      @@RRN         ##RRN
     C     @@RRN2        PARM      @@RRN2        ##RRN2
     C                   PARM      'MC'          ##AREA            2
     C***  End Chg *** 06/09/15 *******************************************
     C*
     C                   EXSR      PGMEND
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C***  End   Add  ***  05/20/09  **************************************
     C*
     C************* BEGIN ADD ****************** 01/08/98 *************
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C* CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
     C*
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C************* END   ADD ****************** 01/08/98 *************
     C*********** BEGIN DELETE ******************* 01/08/98 ***********
     C*********            MOVELCMSS#     XXSS#
     C*********  CDKEY     CHAINPCD01                98
     C*********  *IN98     IFEQ '0'
     C*********            MOVELCDLSN     ##LAST 56
     C*********            MOVELCDFSN     ##FRST 15
     C*********            MOVE '1'       ##TERM  1
     C*********            CALL 'SRNAME'
     C*********            PARM           ##LAST
     C*********            PARM           ##FRST
     C*********            PARM           ##TERM
     C*********            MOVEL##LAST    SCCLNM
     C*********            ELSE
     C*********            MOVEL*BLANKS   SCCLNM
     C*********            ENDIF
     C*********** END   DELETE ******************* 01/08/98 ***********
     C*********** BEGIN ADD ********************** 01/08/98 ***********
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
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCL1HD        PARM      *BLANK        #1L1             78
     C     SCL2HD        PARM      *BLANK        #1L2             78
      *
     C*********** END   ADD ********************** 01/08/98 ***********
     C*
     C*** BEG ADD *** 06/09/98 ****************************************
     C                   Z-ADD     2             VIEW#
     C*** END ADD *** 06/09/98 ****************************************
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C*** BEGIN ADD *** 02/24/98 **************************************
     C   22KEY1          READE     DBRCDF2                                95
     C*** END   ADD *** 02/24/98 **************************************
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C*** BEGIN ADD *** 02/24/98 **************************************
     C   22KEY1          READPE    DBRCDF2                                95
     C*** END   ADD *** 02/24/98 **************************************
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C*** BEGIN ADD *** 02/24/98 **************************************
     C   22KEY3          SETGT     DBRCDF2                            96
     C*** END   ADD *** 02/24/98 **************************************
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
     C*** BEGIN ADD *** 02/24/98 **************************************
     C   22KEY3          SETLL     DBRCDF2                            96
     C*** END   ADD *** 02/24/98 **************************************
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C**                   MOVE DL        SCNAME
     C*** BEGIN ADD ** 08/21/97 ************************************************
     C                   MOVEL(P)  DL            SCDSC1
     C*** END ADD ** 08/21/97 **************************************************
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   MOVE      SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR
     C*** BEGIN ADD *** 02/24/98 **************************************
     C*
     C*** ADD CHANGE VIEW FUNCTION KEY
     C*** ADD COLUMN HEADINGS
     C*
     C*** END   ADD *** 02/24/98 **************************************
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
