     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**********************************************************
     H* PROGRAM: F0275R01                                      *
     H* DESCRIPTION: WORK WITH DIAGNOSIS INFORMATION           *
     H**********************************************************
     H* INDICATORS:                                            *
     H**********************************************************
     H* REVISIONS:                                             *
     H*                                                        *
     H* 02/09/01 C:62 PROJECT: MCMNT   BASE ONLY               *
     H*               COMPILE ONLY                             *
     H*                                                        *
     H* 12/15/99 C:53 PROJECT: Y2K                             *
     H*               CORRECT MISCODED ERROR MSG EDIT.         *
     H*                                                        *
     H* 07/01/99 C:62 PROJECT:MC207                            *
     H*               -CHANGE TBID'S FOR LOCATION AND SITE.    *
     H*               -IMPLEMENT SEQUENCE NUMBERS              *
     H*                                                        *
     H* 02/24/98 C:62 PROJECT: MC145 #9115                     *
     H*            1. On the add of a new diagnosis code,      *
     H*               default the seq# to 99000.  This will    *
     H*               allow view #1 to sort recores in SEQ#    *
     H*               order with those records with a BWC      *
     H*               Status appearing at the top.  A new      *
     H*               sequence nember is assigned based on the *
     H*               the BWC Status given by the State --     *
     H*               Done in W8074R2                          *
     H*                                                        *
     H* 01/29/98 C:56  87MDQ                                   *
     H*                1. Prevent addition of more than one    *
     H*                   primary ICD9 code per case.          *
     H*                                                        *
     H* 01/27/98 C:62 PROJECT: MC145                           *
     H*                 1) MAINT FIELDS ADDED                  *
     H*                 2) SCISTS AND SCISDT ARE INPUT/OUTPUT  *
     H*                 3) SEARCHES AND EDITS ADDED            *
     H*                 4) FIXED ENTER ON DISPLAY              *
     H*                                                        *
     H* 01/08/98 C:86 PROJECT: MC145                           *
     H*          1.   DISPLAY FILE F0278F01 HAS BEEN CHANGED TO*
     H*               TO PRINT A 2 LINE SUB-HEADING. THIS PRO- *
     H*               GRAM CALLS FA702R TO PRINT IT & USES LIKE*
     H*               DEFINES TO REDEFINE THE OMMITTED SCREEN  *
     H*               FIELDS.                                  *
     H*          2.   DARTS CALL WITH OPTION 14 TO SC010R HAS  *
     H*               BEEN REPLACED WITH A CALL TO FA703R AND A*
     H*               CALL TO SA001R FOR CORRECT FLAGS & INFO  *
     H*                                                        *
     H* 12/30/97 C:86  PROJECT: MC145                          *
     H*          CODE To allow DARTS from display screen       *
     H*                                                        *
     H* 12/19/97 C:56  Project: 87MAZ                          *
     H*                Seton indicator 90 when returning from  *
     H*                search with F3 key                      *
     H*                                                        *
     H* 12/04/97 C:86  PROJECT: MC145                          *
     H*          CHANGE MOVE FROM F0075R TO F0270R.            *
     H*                                                        *
     H* 05/29/97 C:32  87MBY                                   *
     H*                REMOVE EDITS FOR ICD9 CODE AND ADD CALL *
     H*                TO PGM F4200ROH                         *
     H*                                                        *
     H* 05/06/97 C:56  87MAZ                                   *
     H*                REMOVE ICD9 SEARCH F0510R AND REPLACE   *
     H*                WITH F2010R                             *
     H*                                                        *
     H* 02/18/97 C:56  87MAZ                                   *
     H*                CREATED                                 *
     H**********************************************************
     FF0275F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FFADAP     UF A E             DISK
     F                                     RENAME(PDA01:DBRCDF)
     FFADAL     IF   E           K DISK
     FFMDIL     IF   E           K DISK
     F***********     FMF1L   IF  E           K        DISK
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     F*** BEGIN ADD *** 07/01/99 **************************************
     FFTGIL     IF   E           K DISK
     F*** END   ADD *** 07/01/99 **************************************
     F*** BEGIN ADD *** 02/24/98 **************************************
     FFADAL1    IF   E           K DISK
     F                                     RENAME(PDA01:PDA02)
     F*** END   ADD *** 02/24/98 **************************************
     F*** BEGIN ADD *** 01/29/98 **************************************
     FFADAL2    IF   E           K DISK
     F                                     RENAME(PDA01:PDA03)
     F*** END ADD *** 01/29/98 ****************************************
     F*** BEGIN ADD *** 10/28/97 *************************************
     FFMCMP     IF   E             DISK
      /COPY SCOPYMT01D
     D*
     D OPTNM           C                   CONST('Diagnosis Informatio-
     D                                     n')
     D COMAPI          C                   CONST('FA007R')
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0
     F*** END ADD *** 10/28/97 ***************************************

     I*** BEGIN ADD *** 01/29/98 ************************************
     IPDA03
     I              DACO#                       D3CO#
     I              DASUB#                      D3SUB#
     I              DAFND                       D3FND
     I              DAFYR                       D3FYR
     I              DACASE                      D3CASE
     I              DASEQ#                      D3SEQ#
     I              DAICD9                      D3ICD9
     I              DALOST                      D3LOST
     I              DAPRIM                      D3PRIM
     I              DASTDT                      D3STDT
     I              DAILOC                      D3ILOC
     I              DAISIT                      D3ISIT
     I              DAISTS                      D3ISTS
     I              DAISDT                      D3ISDT
     I              DASTAT                      D3STAT
     I              DAUSER                      D3USER
     I              DAPGM                       D3PGM
     I              DATIME                      D3TIME
     I              DADT                        D3DT
     I              DAIUSR                      D3IUSR
     I              DAIPGM                      D3IPGM
     I              DAITIM                      D3ITIM
     I              DAIDT                       D3IDT
     I*** END ADD *** 01/29/98 ***************************************
     I*
     I***PY SCOPYMT01I

     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C*** DIAGNOSIS CODE INFO FILR
     C*
     C     DAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKSEQ#
     C                   KFLD                    KKICD9
     C*
     C*** KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C*
     C     DIKEY         KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*
     C** KEY LIST FOR GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTID             4
     C                   KFLD                    KKTKEY           10
     C*** BEGIN ADD *** 07/01/99 **************************************
     C*
     C     GIKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTID
     C*** END   ADD *** 07/01/99 **************************************
     C*** BEGIN ADD *** 01/29/98 **************************************
     C*
     C** KEY LIST FOR DIAGNOSIS IMFORMATION FILE
     C*
     C     DAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKPRIM
     C*
     C*** END ADD *** 01/29/98 ****************************************
     C*** BEGIN ADD *** 10/28/97 *************************************
     C*
     C** KEY LIST FOR DIAGNOSIS IMFORMATION FILE
     C*
     C     DAKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKICD9
     C*** BEGIN ADD *** 01/29/98 **************************************
     C                   KFLD                    KKLOST
     C*** END ADD *** 01/29/98 ****************************************
     C*** BEGIN ADD *** 07/01/99 **************************************
     C*
     C*** DIAGNOSIS CODE INFO FILR
     C*
     C     DAKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKSEQ#
     C*
     C*** DIAGNOSIS CODE INFO FILR
     C*
     C     DAKY4         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** END   ADD *** 07/01/99 **************************************
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
     C*** BEGIN ADD *** 07/01/99 **************************************
     C     *LIKE         DEFINE    DASEQ#        XXSEQ#
     C*** END   ADD *** 07/01/99 **************************************
     C*
     C                   Z-ADD     1             #OFSCR            1 0
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DATCVT - RUNS A DATCHK TO GO FROM 6 TO 8.
     C*================================================================
     C     DATCVT        BEGSR
     C*
     C                   Z-ADD     SCISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCISDT
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C                   ENDSR
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C** SET THESE INDICATORS OFF - USED AS POSITION CURSORS
     C*
     C                   SETOFF                                       303132
     C                   SETOFF                                       3334
     C*
     C     *IN40         IFEQ      *ON
     C*
     C*** VALIDATE DIAGNOSIS CODE AND TYPE
     C*
     C     SCICD9        IFEQ      *BLANKS
     C                   SETON                                        9071
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C     SCLOST        IFEQ      *BLANKS
     C                   SETON                                        9072
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C     SCICD9        IFNE      *BLANKS
     C     SCLOST        ANDNE     *BLANKS
     C*
     C                   MOVEL     SCICD9        KKICD9
     C                   MOVEL     SCLOST        KKLOST
     C     DIKEY         CHAIN     PDI01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9073
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C     DISTAT        IFEQ      'I'
     C                   SETON                                        9079
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  DISTAT IFEQ
     C                   ENDIF                                                  SCICD9 IFNE
     C*
     C                   ENDIF                                                  *IN40  IFEQ
     C*
     C*** BEGIN ADD *** 10/28/97 **************************************
     C*
     C** CHECK TO SEE IF RECORD ALREADY EXISTS
     C*
     C     *IN40         IFEQ      *ON
     C     DAKY2         CHAIN     PDA02                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        9089
     C                   GOTO      ERRSTP
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 01/29/98 **************************************
     C                   ELSE
     C*
     C     HDICD9        IFNE      SCICD9
     C     HDLOST        ORNE      SCLOST
     C     DAKY2         CHAIN     PDA02                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        9089
     C                   GOTO      ERRSTP
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C                   ENDIF                                                  HDICD9 IFNE SCICD9
     C*
     C                   ENDIF                                                  @@MODE IFEQ
     C*** END ADD *** 01/29/98 ***************************************
     C*
     C*** END ADD *** 10/28/97 ****************************************
     C*
     C*** VALIDATE PRIMARY DIAGNOSIS CODE
     C*
     C     SCPRIM        IFNE      'Y'
     C     SCPRIM        ANDNE     'N'
     C                   SETON                                        9074
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 01/29/98 **************************************
     C*
     C** CHECK TO SEE IF A PRIMARY CODE ALREADY EXISTS
     C*
     C     *IN40         IFEQ      *ON
     C*
     C     SCPRIM        IFEQ      'Y'
     C                   MOVE      SCPRIM        KKPRIM
     C     DAKY1         CHAIN     PDA03                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        9080
     C                   GOTO      ERRSTP
     C                   ENDIF                                                  *IN99 IFEQ OFF
     C                   ENDIF                                                  SCPRIM IFEQ Y
     C*
     C                   ELSE
     C*
     C     SCPRIM        IFEQ      'Y'
     C     HDPRIM        ANDNE     SCPRIM
     C                   MOVE      SCPRIM        KKPRIM
     C     DAKY1         CHAIN     PDA03                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        9080
     C                   GOTO      ERRSTP
     C                   ENDIF                                                  *IN99 IFEQ OFF
     C                   ENDIF                                                  SCPRIM IFEQ Y
     C*
     C                   ENDIF                                                  @@MODE IFEQ C
     C*** END ADD *** 01/29/98 ****************************************
     C*****************************************************************
     C*   Per OH BWC regulations, E and V codes cannot be Primary     *
     C*****************************************************************
     C*** BEGIN DEL *** 05/29/97 **************************************
     C*********  SCPRIM    IFEQ 'Y'
     C*********            MOVELSCICD9    FRSTLT  1
     C*********  FRSTLT    IFEQ 'E'
     C*********  FRSTLT    OREQ 'V'
     C*********            SETON                     9085
     C*********  *IN90     CABEQ'1'       ERRSTP
     C*********            ENDIF                           FRSTLT IFEQ
     C*********            ENDIF                           SCPRIM IFEQ
     C*** END   DEL *** 05/29/97 **************************************
     C*** BEGIN ADD *** 05/29/97 **************************************
     C*** CALL ICD9 EDIT PROGRAM
     C                   MOVEL     SCICD9        ##ICD9
     C     SCPRIM        IFEQ      'Y'
     C                   MOVE      'P'           ##PRSC
     C                   ELSE
     C                   MOVE      'S'           ##PRSC
     C                   ENDIF
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
     C                   PARM                    ##ICD9            5            IDC9 CODE
     C                   PARM                    ##PRSC            1            PRIMARY/SECOND
     C                   PARM                    ##CALL            1            CALLED PGM CODE
     C                   PARM                    ##SYS             1            SYSTEM (P/H)
     C                   PARM                    ##STYP            1            INPAT/OUTPAT
     C                   PARM                    ##VLD             1            VALID/INVALID
     C                   PARM                    ##GRP             1            GROUP COVERAGE
     C                   PARM                    ##ERR             2            Ennn/Vnnn ICD9
     C                   PARM                    ##STS             2            CLAIM STATUS
     C                   PARM                    #1CO#             3            CO #
     C                   PARM                    #1SUB             5            SUB #
     C                   PARM                    #1FND             3            FUND
     C                   PARM                    #1FYR             3            FUND YEAR
     C                   PARM                    #1CASE            7            CASE#
     C*
     C* CHECK ERROR FLAG FOR 'E' OR 'V' CODE
     C     ##ERR         IFEQ      'EV'
     C                   MOVE      '1'           *IN90
     C                   MOVE      '1'           *IN85
     C                   ENDIF
     C*
     C* CHECK WHETHER ICD9 CODE HAS BEEN FLAGGED AS BWC INVALID
     C     ##VLD         IFEQ      'I'
     C                   MOVE      '1'           *IN90
     C                   MOVE      '1'           *IN86
     C                   ENDIF
     C*
     C*** END   ADD *** 05/29/97 **************************************
     C*
     C*** VALIDATE LOCATION
     C*
     C     SCILOC        IFNE      *BLANKS
     C*** BEGIN CHG *** 07/01/99 **************************************
     C**********           MOVEL'ILOC'    KKTID     P
     C                   MOVEL(P)  ILOC          KKTID
     C*** END   CHG *** 07/01/99 **************************************
     C                   MOVEL(P)  SCILOC        KKTKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9076
     C     *IN99         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  *IN99 IFEQ
     C                   ENDIF
     C*
     C*** VALIDATE SITE
     C*
     C     SCISIT        IFNE      *BLANKS
     C*** BEGIN CHG *** 07/01/99 **************************************
     C**********           MOVEL'ISIT'    KKTID     P
     C                   MOVEL(P)  ISIT          KKTID
     C*** END   CHG *** 07/01/99 **************************************
     C                   MOVEL(P)  SCISIT        KKTKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9077
     C     *IN99         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  *IN99 IFEQ
     C                   ENDIF
     C*
     C*** VALIDATE STATUS
     C*
     C     SCISTS        IFNE      *BLANKS
     C                   MOVEL(P)  'ISTS'        KKTID
     C                   MOVEL(P)  SCISTS        KKTKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9078
     C     *IN99         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  *IN99 IFEQ
     C                   ENDIF
     C*
     C* DATE IF ENTERED HAS TO BE VALID
     C*
     C********** SCISTS    IFNE *BLANKS                    ADD 12/15/99
     C     SCISDT        IFNE      *ZEROS                                       ADD 12/15/99
     C                   Z-ADD     SCISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCISDT
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        8790
     C                   ENDIF                                                  SRERR
     C*
     C*** BEG DEL *** 12/15/99 **************************
     C**********           ELSE
     C********** SCISDT    IFNE *ZEROS
     C**********           SETON                     8890
     C**********           ENDIF                           SCISDT
     C*** BEG DEL *** 12/15/99 **************************
     C                   ENDIF                                                  SCISTS
     C*
     C* DATE IF ENTERED HAS TO BE VALID
     C*
     C     SCEXDT        IFNE      *ZEROS
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C     SRERR         IFEQ      *ON
     C  N90              SETON                                        8190
     C                   ENDIF                                                  SRERR
     C                   ENDIF
     C*
     C* SCEXDT HAS TO BE GREATER THAN OR EQUAL TO SCISDT.
     C*
     C                   Z-ADD     SCISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXISDT
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
     C*
     C     XXISDT        IFGT      XXEXDT
     C     SCEXDT        ANDNE     *ZEROS
     C  N90              SETON                                        8290
     C                   ENDIF
     C*
     C     ERRSTP        ENDSR
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
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
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     DAICD9        SCICD9
     C                   MOVE      DALOST        SCLOST
     C*
     C                   MOVE      *BLANKS       KKICD9
     C                   MOVE      *BLANKS       KKLOST
     C                   MOVEL     SCICD9        KKICD9
     C                   MOVE      SCLOST        KKLOST
     C*
     C     DIKEY         CHAIN     PDI01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     DIDSC1        SCDSC1
     C                   ENDIF
     C*
     C                   MOVE      DAPRIM        SCPRIM
     C                   MOVEL     DAILOC        SCILOC
     C*
     C*** GET LOCATION  DESCRIPTION
     C*
     C*** BEGIN CHG *** 07/01/99 **************************************
     C**********           MOVEL'ILOC'    KKTID     P
     C                   MOVEL(P)  ILOC          KKTID
     C*** END   CHG *** 07/01/99 **************************************
     C                   MOVEL     SCILOC        KKTKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     GTDESC        SCLDSC
     C                   ENDIF
     C*
     C                   MOVEL     DAISIT        SCISIT
     C*
     C*** GET SITE DESCRIPTION
     C*
     C*** BEGIN CHG *** 07/01/99 **************************************
     C**********           MOVEL'ISIT'    KKTID     P
     C                   MOVEL(P)  ISIT          KKTID
     C*** END   CHG *** 07/01/99 **************************************
     C                   MOVEL     SCISIT        KKTKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     GTDESC        SCSDSC
     C                   ENDIF
     C*
     C                   MOVEL     DAISTS        SCISTS
     C*
     C* GET STATUS DESCRIPTION
     C*
     C                   MOVEL(P)  'ISTS'        KKTID
     C                   MOVEL     SCISTS        KKTKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     GTDESC        SCSTSD
     C                   ENDIF
     C*
     C                   Z-ADD     DAISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCISDT
     C*
     C                   Z-ADD     DAEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C                   ENDIF                                                  @@MODE IFEQ
     C*** BEGIN ADD *** 01/29/98 *************************************
     C     @@MODE        IFEQ      'C'
     C                   MOVEL(P)  DAICD9        HDICD9
     C                   MOVE(P)   DALOST        HDLOST
     C                   MOVE(P)   DAPRIM        HDPRIM
     C                   ENDIF
     C*** END ADD *** 01/29/98 ***************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*** BEGIN DEL *** 07/01/99 **************************************
     C*
     C** SET OF INDICATORS FOR POSISTION CURSORS
     C*
     C**********           SETOF                     303132
     C**********           SETOF                     3334
     C*** END   DEL *** 07/01/99 **************************************
     C*
     C                   EXSR      DATCVT
     C*
     C*** DIAGNOSIS CODE AND TYPE SEARCH
     C*
     C     HFLD          IFEQ      'SCICD9'
     C*** BEGIN DEL *** 05/06/97 **************************************
     C**********           MOVELOPTNM     ##PRDS
     C**********           MOVE '2'       ##CALL
     C**********           MOVE *BLANKS   ##ICD9
     C**********           MOVE *BLANKS   ##RTCD
     C**********
     C**********           CALL 'F0510R'
     C**********           PARM           ##PRDS 40
     C**********           PARM           ##CALL  1
     C**********           PARM           ##ICD9  5
     C**********           PARM           ##RTCD  1
     C**********           MOVEL##ICD9    SCICD9
     C**********           MOVE ##CALL    SCLOST
     C*** END DEL *** 05/06/97 ****************************************
     C*
     C*** BEGIN ADD *** 05/06/97 **************************************
     C                   MOVE      '2'           ##CALL
     C                   MOVEL     *BLANKS       ##KC
     C                   MOVEL     *BLANKS       ##KL
     C                   MOVEL     *BLANKS       ##ICD9
     C                   MOVEL     *BLANKS       ##LOST
     C*
     C                   CALL      'F2010R'
     C                   PARM                    ##CALL            1
     C                   PARM                    ##KC              1
     C                   PARM                    ##KL              1
     C                   PARM                    ##ICD9            5
     C                   PARM                    ##LOST            1
     C*** END ADD *** 05/06/97 ****************************************
     C*
     C*** BEGIN CHG *** 05/06/97 **************************************
     C********** ##RTCD    IFEQ 'A'
     C     ##KC          IFEQ      '1'
     C*** END CHG *** 05/06/97 ****************************************
     C                   MOVE      '1'           OUTFLG            1
     C                   ENDIF
     C*
     C*** BEGIN CHG *** 05/06/97 **************************************
     C********** ##RTCD    IFEQ 'B'
     C********** SCICD9    OREQ *BLANKS
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C*** END CHG *** 05/06/97 ****************************************
     C                   ENDIF
     C*
     C*** BEGIN CHG *** 05/06/97 **************************************
     C********** ##RTCD    IFEQ '*'
     C     ##ICD9        IFNE      *BLANKS
     C*** END CHG *** 05/06/97 ****************************************
     C                   MOVEL(P)  ##ICD9        SCICD9
     C                   MOVEL(P)  ##LOST        SCLOST
     C                   MOVEL     SCICD9        KKICD9
     C                   MOVEL     SCLOST        KKLOST
     C     DIKEY         CHAIN     PDI01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     DIDSC1        SCDSC1
     C                   ENDIF
     C                   SETON                                        9030      IN30, POSITION
     C                   ENDIF
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*** LOCATION SEARCH
     C*
     C     HFLD          IFEQ      'SCILOC'
     C*** BEGIN CHG *** 07/01/99 **************************************
     C**********           MOVEL'ILOC'    ##TBID
     C                   MOVEL(P)  ILOC          ##TBID
     C*** END   CHG *** 07/01/99 **************************************
     C                   EXSR      GENTBL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9031      31, POS CURSOR
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C                   SETON                                        9032      IN32, POSITION
     C                   MOVEL     GTKEY         SCILOC
     C                   MOVEL     GTDESC        SCLDSC
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*** SITE SEARCH
     C*
     C     HFLD          IFEQ      'SCISIT'
     C*** BEGIN CHG *** 07/01/99 **************************************
     C**********           MOVEL'ISIT'    ##TBID
     C                   MOVEL(P)  ISIT          ##TBID
     C*** END   CHG *** 07/01/99 **************************************
     C                   EXSR      GENTBL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9032      32, POS CURSOR
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C                   MOVEL     GTKEY         SCISIT
     C                   MOVEL(P)  GTDESC        SCSDSC
     C                   SETON                                        9033      IN33, POSITION
     C                   ENDIF
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*
     C*** BWC STATUS SEARCH
     C*
     C     HFLD          IFEQ      'SCISTS'
     C                   MOVEL     'ISTS'        ##TBID
     C                   EXSR      GENTBL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9033      33, POS CURSOR
     C                   ELSE
     C*
     C                   SETON                                        9034      IN34, POSITION
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C                   MOVEL     GTKEY         SCISTS
     C                   MOVEL     GTDESC        SCSTSD
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS.
     C*================================================================
     C     KNDO          BEGSR
     C*
     C                   EXSR      DATCVT
     C*
     C************** BEGIN ADD ***************** 01/08/98 *************
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
     C                   MOVE      '1'           *IN90
     C*
     C************** END   ADD ***************** 01/08/98 *************
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
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
     C                   Z-ADD     SCISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DAISDT
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DAEXDT
     C*** BEGIN ADD *** 07/01/99 **************************************
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   Z-ADD     XXSEQ#        DASEQ#
     C*
     C                   ENDIF
     C*** END   ADD *** 07/01/99 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing other fucntion keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*** BEGIN ADD *** 07/01/99 **************************************
     C*
     C** SET OF INDICATORS FOR POSISTION CURSORS
     C*
     C                   SETOFF                                       303132
     C                   SETOFF                                       3334
     C*** END   ADD *** 07/01/99 **************************************
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C     *INKN         CASEQ     *ON           KNDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*** BEGIN ADD *** 07/01/99 **************************************
     C*
     C** SET ON LR
     C*
     C                   SETON                                        LR
     C*** END   ADD *** 07/01/99 **************************************
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'D'                                    09
     C************* BEGIN ADD ****************** 01/08/98 *************
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
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
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C* GET CLAIM MASTER RECORD.
     C*
     C     @@RRN2        CHAIN     PCM01                              99
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
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
     C     SCL1HD        PARM      *BLANK        ##L1             78
     C     SCL2HD        PARM      *BLANK        ##L2             78
     C*
     C* GET KEY LIST VALUES.
     C*
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
     C*
     C*********** END   CHANGE ******************* 01/08/98 ***********
     C*
     C*** BEGIN ADD *** 07/01/99 **************************************
     C*
     C* DETERMINE THE TBID'S FOR LOCATION AND SITE.
     C*
     C* LOCATION
     C*
     C     CMAST         CAT(P)    'IL':0        ILOC              4
     C*
     C                   MOVEL(P)  ILOC          KKTID
     C*
     C     GIKY          CHAIN     PGI01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  'ZZIL'        ILOC
     C*
     C                   ENDIF
     C*
     C* SITE
     C*
     C     CMAST         CAT(P)    'IS':0        ISIT              4
     C*
     C                   MOVEL(P)  ISIT          KKTID
     C*
     C     GIKY          CHAIN     PGI01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  'ZZIS'        ISIT
     C*
     C                   ENDIF
     C*** END   ADD *** 07/01/99 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C*
     C                   TIME                    DFDTTM
     C*
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        DAUSER
     C                   MOVEL     Q1PGM         DAPGM
     C                   MOVEL     DFTIME        DATIME
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DADT
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C*** BEGIN ADD *** 07/01/99 **************************************
     C*
     C* GET THE SEQUENCE NUMBER
     C*
     C                   Z-ADD     *HIVAL        KKSEQ#
     C*
     C     DAKY3         SETGT     PDA01
     C     DAKY4         READPE    PDA01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   ADD       1             XXSEQ#
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             XXSEQ#
     C*
     C                   ENDIF
     C*** END   ADD *** 07/01/99 **************************************
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   TIME                    DFDTTM
     C*
     C                   MOVEL     Q1USER        DAIUSR
     C                   MOVEL     Q1PGM         DAIPGM
     C                   MOVEL     DFTIME        DAITIM
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DAIDT
     C*** BEGIN ADD *** 01/27/97 **************************************
     C                   MOVEL     Q1USER        DAUSER
     C                   MOVEL     Q1PGM         DAPGM
     C                   MOVEL     SRDTO         DADT
     C                   MOVEL     DFTIME        DATIME
     C*** END   ADD *** 01/27/97 **************************************
     C*
     C                   WRITE     DBRCDF
     C*
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
     C*================================================================
     C* GENTBL - GENERIC TABLE SEARCH
     C*================================================================
     C     GENTBL        BEGSR
     C*
     C                   CALL      'F2380R'                             99
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID            4
     C                   ENDSR
