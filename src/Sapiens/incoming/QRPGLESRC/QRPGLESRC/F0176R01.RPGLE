     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H**********************************************************
     H* PROGRAM: F0176R01
     H* DESCRIPTION: WORK WITH WORK HISTORY INFORMATION
     H**********************************************************
     H* INDICATORS:
     H**********************************************************
     H*REVISIONS:                                             *
     H*                                                        *
     H*  07/07/11 C:251 Project: 114WZN                        *
     H*                 Update Loss Time Days in WMCMP         *
     H*                                                        *
     H* 04/19/06 C:163 PROJECT 114WMF                         *
     H*                Back to Base - Medical Case Infor-     *
     H*                               mation(114MCQ)          *
     H*                                                        *
     H*04/19/06 C:163 Project: 114MCQ                         *
     H*               ADD EDITS FOR LDW,RTW,LWP,LWR CODES.    *
     H*                                                        *
     H*  07/01/99 C:62  Project: MC207                         *
     H*                 ASSIGNING A SEQ# TO ADDED RECORDS.     *
     H*                                                        *
     H* 01/27/98 C:62 PROJECT: MC145                           *
     H*               1) DATCHK BEFORE SEARCHES                *
     H*                                                        *
     H* 01/08/98 C:86 PROJECT: MC145                           *
     H*          1.   DISPLAY FILE F0176F01 HAS BEEN CHANGED TO*
     H*               TO PRINT A 2 LINE SUB-HEADING. THIS PRO- *
     H*               GRAM CALLS FA702R TO PRINT IT & USES LIKE*
     H*               DEFINES TO REDEFINE THE OMMITTED SCREEN  *
     H*               FIELDS.                                  *
     H*          2.   DARTS CALL WITH FUNCTION 14 TO SC010R HAS*
     H*               BEEN REPLACED WITH A CALL TO FA703R AND A*
     H*               CALL TO SA001R FOR CORRECT FLAGS & INFO  *
     H*                                                        *
     H***************************************************
     H*   NEW COMMENTS START HERE
     H***************************************************
     H* 12/19/97 C:32  PROJECT: 87MAZ
     H*        Add check for duplicates in add/chg mode by date
     H*        Add SCCODE Chgs made on the B70
     H*
     H* 12/02/97 C:86  PROJECT: MC145
     H*        REMOVE EDIT CHECK ON SCCODE
     H*                                                        *
     H* 09/23/97 C:59 PROJECT: SUPPORT MC087/#8828             *
     H*               Calling incorrect program on darts       *
     H*               correct is F0270R, Change Both keys to   *
     H*               be blank instead of zeros.  Causing      *
     H*               problem in the file.                     *
     H*                                                        *
     H* 04/01/97 C:32  87MAZ
     H*                CREATED
     H*
     H* 05/23/97 C:56  PROJECT: 87MAZ
     H*             1. REPLACE HARD-CODED DISPLAY OF WORK CODE
     H*                DESCRIPTION WITH DESCRIPTION FOR FTGTP.
     H*
     H*             2. ADD F10=SEARCH ON WORK CODE.
     H*
     H* 06/12/97 C:56  PROJECT: 87MAZ  CO# 23
     H*                REMOVE LEVEL OF DUTY PERCENTAGE
     H**********************************************************
     FF0176F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FFAWHP     UF A E             DISK
     F                                     RENAME(PWH01:DBRCDF)
     F*** BEGIN ADD *** 12/19/97 **************************************
     FFAWHL     IF   E           K DISK
     F*** END   ADD *** 12/19/97 **************************************
     F*** BEGIN ADD *** 07/01/99 **************************************
     FFAWHL5    IF   E           K DISK
     F                                     RENAME(PWH01:PWH02)
     F*** END   ADD *** 07/01/99 **************************************
     F***********          FMF1L   IF  E           K        DISK
     FFTDLL     IF   E           K DISK
     F*** BEGIN ADD *** 05/23/97 **************************************
     FFTGTL     IF   E           K DISK
     F*** END ADD *** 05/23/97 ****************************************
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     FFMCMP     IF   E             DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D OPTNM           C                   CONST('Work History Inform-
     D                                     ation')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*** BEGIN DEL *** 05/23/97 **************************************
     D**********    'Estimated Return to- C         XXDES1
     D**********    ' Work'
     D**********    'Date Last Worked'    C         XXDES2
     D**********    'Actual Return to wo- C         XXDES3
     D**********    'rk'
     D*** END DEL *** 05/23/97 ****************************************
     D*
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0

     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          RRN FAWHP
     C                   PARM                    @@RRN2            9 0          RRN FMCMP
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C     DLKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*** BEGIN ADD *** 05/23/97 **************************************
     C*
     C*** KEY LIST FOR GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKY
     C*** END ADD *** 05/23/97 ****************************************
     C*** BEGIN ADD *** 12/19/97 **************************************
     C*
     C*** KEY LIST FOR WORK HISTORY FILE
     C*
     C     WHKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKWKDT
     C*** END   ADD *** 12/19/97 **************************************
     C*** BEGIN ADD *** 07/01/99 **************************************
     C*
     C     WHKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     WHKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKSEQ#
     C*** END   ADD *** 07/01/99 **************************************
     C*
     C     *LIKE         DEFINE    WHCO#         KKCO#
     C     *LIKE         DEFINE    WHSUB#        KKSUB#
     C     *LIKE         DEFINE    WHFND         KKFND
     C     *LIKE         DEFINE    WHFYR         KKFYR
     C     *LIKE         DEFINE    WHCASE        KKCASE
     C     *LIKE         DEFINE    DLKEY         KKKEY
     C     *LIKE         DEFINE    GTDT          HLTST1
     C*** BEGIN ADD *** 05/23/97 **************************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKY
     C*** END ADD *** 05/23/97 ****************************************
     C*** BEGIN ADD *** 12/19/97 **************************************
     C     *LIKE         DEFINE    GTDT          KKWKDT
     C     *LIKE         DEFINE    WHWKDT        SVWKDT
     C*** END ADD *** 12/19/97 ****************************************
     C*** BEGIN ADD *** 07/01/99 **************************************
     C     *LIKE         DEFINE    WHSEQ#        KKSEQ#
     C     *LIKE         DEFINE    WHSEQ#        XXSEQ#
     C*** END   ADD *** 07/01/99 **************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DATCVT - RUNS A DATCHK TO GO FROM 6 TO 8.
     C*================================================================
     C     DATCVT        BEGSR
     C*
     C                   Z-ADD     SCWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCWKDT
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
     C* ERRCK1 - Error checking for screen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C** CHECK FOR VALID WORK CODE
     C*
     C******* BEGIN DELETE *************** 12/02/97 *******************
     C*********  SCCODE    IFNE '296'
     C*********  SCCODE    ANDNE'297'
     C*********  SCCODE    ANDNE'552'
     C*********            SETON                     9071
     C*********  *IN90     CABEQ'1'       ERRSTP
     C*********            ENDIF
     C******* END   DELETE *************** 12/02/97 *******************
     C*
     C*** BEGIN DEL *** 05/23/97 **************************************
     C**********           SELEC
     C********** SCCODE    WHEQ '296'
     C**********           MOVELXXDES1    SCDESC    P
     C********** SCCODE    WHEQ '297'
     C**********           MOVELXXDES2    SCDESC    P
     C********** SCCODE    WHEQ '552'
     C**********           MOVELXXDES3    SCDESC    P
     C**********           OTHER
     C**********           MOVE *BLANKS   SCDESC
     C**********           ENDSL
     C*** END DEL *** 05/23/97 ****************************************
     C*
     C*** BEGIN ADD *** 05/23/97 **************************************
     C                   MOVEL(P)  'WKCD'        KKTBID
     C                   MOVEL(P)  SCCODE        KKKY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*** BEGIN CHG *** 12/19/97 **************************************
     C********** *IN99     IFEQ '1'
     C     *IN99         IFEQ      '0'
     C     GTSTAT        ANDNE     'I'
     C*** END   CHG *** 12/19/97 **************************************
     C                   MOVEL     GTDESC        SCDESC
     C                   ELSE
     C                   MOVE      *BLANKS       SCDESC
     C                   SETON                                        9071      ADD 12/19/97
     C                   ENDIF
     C*** END ADD *** 05/23/97 ****************************************
     C*
     C*
     C*  VALIDATE THE WORK DATE ENTERED
     C     SCWKDT        IFEQ      *ZEROS
     C                   SETON                                        9072
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C                   Z-ADD     SCWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCWKDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9073
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   END
     C*
     C                   Z-ADD     SCWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLTST1
     C     HLTST1        IFLT      CMACDT
     C                   SETON                                        9074
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 12/19/97 ********************************************
     C     @@MODE        IFEQ      'A'
     C                   MOVE      HLTST1        KKWKDT
     C     WHKEY         CHAIN     PWH01                              99
     C     *IN99         IFEQ      '0'
     C                   SETON                                        9078
     C                   ENDIF
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  END @@MODE
     C     @@MODE        IFEQ      'C'
     C     HLTST1        IFNE      SVWKDT
     C                   MOVE      HLTST1        KKWKDT
     C     WHKEY         CHAIN     PWH01                              99
     C     *IN99         IFEQ      '0'
     C                   SETON                                        9078
     C                   ENDIF
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  END HLTST1
     C                   ENDIF                                                  END @@MODE
     C*** END   ADD *** 12/19/97 ********************************************
     C*** BEGIN ADD *** 04/19/06 ********************************************
     C*
     C* MAKE SURE THAT LDW, LWP AND RTW, LWR ARE ENTERED IN CORRECT
     C* ORDER.
     C*          LDW - NO DUTY STARTS
     C*          RTW - NO DUTY ENDS
     C*          LWP - LIGHT DUTY STARTS
     C*          LWR - LIGHT DUTY ENDS
     C*
B0   C     XXCLT#        IFEQ      '114'
B-1  C     *IN90         IFEQ      *OFF
B01  C     SCCODE        IFEQ      'LDW'
     C     SCCODE        OREQ      'LWP'
     C     SCCODE        OREQ      'RTW'
     C     SCCODE        OREQ      'LWR'
     C*
     C                   Z-ADD     HLTST1        KKWKDT
     C*                    MOVEL*OFF      PASS    1
     C*
     C* CHECK PREVIOUS CODE IF THERE IS ONE
     C*
     C     WHKEY         SETLL     PWH01
     C     WHKY2         READE     PWH01                                  99
B02  C     *IN99         DOWEQ     *OFF
B03  C     WHCODE        IFEQ      'LDW'
     C     WHCODE        OREQ      'LWP'
     C     WHCODE        OREQ      'RTW'
     C     WHCODE        OREQ      'LWR'
B04  C     *IN40         IFEQ      *OFF
     C     WHWKDT        ANDNE     HLTST1
     C                   LEAVE
E04  C                   ENDIF
B04  C     *IN40         IFEQ      *ON
     C                   LEAVE
E04  C                   ENDIF
E03  C                   ENDIF
     C     WHKY2         READE     PWH01                                  99
E02  C                   ENDDO
     C*
B06  C     *IN99         IFEQ      *ON
     C*
     C* IF THERE ARE NO PREVIOUS ENTRIES THEN MUST USE A START CODE.
     C*
B07  C     SCCODE        IFEQ      'RTW'
     C     SCCODE        OREQ      'LWR'
     C                   SETON                                        8990
E07  C                   ENDIF
     C*
S06  C                   ELSE
     C*
     C* IF THE PREVIOUS CODE IS THE START NO DUTY CODE THEN
     C* THE STOP NO DUTY CODE IS REQUIRED
     C*
B08  C     WHCODE        IFEQ      'LDW'
     C     SCCODE        ANDNE     'RTW'
     C                   SETON                                        8890
E08  C                   ENDIF
     C*
     C* IF THE PREVIOUS CODE IS THE START LIGHT DUTY CODE THEN
     C* THE STOP LIGHT DUTY CODE IS REQUIRED
     C*
B09  C     WHCODE        IFEQ      'LWP'
     C     SCCODE        ANDNE     'LWR'
     C                   SETON                                        8790
E09  C                   ENDIF
     C*
     C* IF THE PREVIOUS CODE IS A STOP CODE THEN
     C* ONLY A START CODE CAN BE ENTERED.
     C*
B10  C     WHCODE        IFEQ      'RTW'
     C     WHCODE        OREQ      'LWR'
B11  C     SCCODE        IFEQ      'RTW'
     C     SCCODE        OREQ      'LWR'
     C                   SETON                                        8690
E11  C                   ENDIF
E10  C                   ENDIF
     C*
E06  C                   ENDIF
     C*
     C* CHECK NEXT CODE IF THERE IS ONE
     C*
B12  C     *IN90         IFEQ      *OFF
     C*
     C     WHKEY         SETGT     PWH01
     C     WHKY2         READPE    PWH01                                  99
B13  C     *IN99         DOWEQ     *OFF
B14  C     SCCODE        IFEQ      'LDW'
     C     SCCODE        OREQ      'LWP'
     C     SCCODE        OREQ      'RTW'
     C     SCCODE        OREQ      'LWR'
B15  C     *IN40         IFEQ      *OFF
     C     WHWKDT        ANDNE     HLTST1
     C                   LEAVE
E15  C                   ENDIF
B16  C     *IN40         IFEQ      *ON
     C                   LEAVE
E16  C                   ENDIF
E14  C                   ENDIF
     C     WHKY2         READPE    PWH01                                  99
E13  C                   ENDDO
     C*
B17  C     *IN99         IFEQ      *OFF
     C*
     C* IF THE NEXT CODE IS A STOP NO DUTY CODE, THEN THE
     C* START NO DUTY CODE IS REQUIRED.
     C*
B18  C     WHCODE        IFEQ      'RTW'
     C     SCCODE        ANDNE     'LDW'
     C                   SETON                                        8590
E18  C                   ENDIF
     C*
     C* IF THE NEXT CODE IS A STOP LIGHT DUTY CODE, THEN THE
     C* START LIGHT DUTY CODE IS REQUIRED.
     C*
B19  C     WHCODE        IFEQ      'LWR'
     C     SCCODE        ANDNE     'LWP'
     C                   SETON                                        8490
E19  C                   ENDIF
     C*
     C* IF THE NEXT CODE IS A START CODE THEN
     C* ONLY A STOP CODE CAN BE ENTERED.
     C*
B20  C     WHCODE        IFEQ      'LDW'
     C     WHCODE        OREQ      'LWP'
B21  C     SCCODE        IFEQ      'LDW'
     C     SCCODE        OREQ      'LWP'
     C                   SETON                                        8390
E21  C                   ENDIF
E20  C                   ENDIF
     C*
E17  C                   ENDIF
     C*
E12  C                   ENDIF
     C*
E01  C                   ENDIF
E-1  C                   ENDIF
E0   C                   ENDIF                                                  XXCLT#=114
     C*
     C* LEVEL OF DUTY MUST BE VALID IF ENTERED
     C*
     C     XXCLT#        IFEQ      '114'
     C     *IN90         ANDEQ     *OFF
     C     XXCLT#        ORNE      '114'
     C*** END   ADD *** 04/19/06 ********************************************
     C                   MOVE      *BLANKS       SCDESW
     C     SCLVL         IFNE      *BLANKS
     C                   MOVEL(P)  SCLVL         KKKEY
     C     DLKY          CHAIN     PDL01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     DLDESC        SCDESW
     C                   ELSE
     C*
     C                   SETON                                        9075
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C                   ENDIF
     C*** BEGIN ADD *** 04/19/06 ********************************************
     C                   ENDIF                                                  XXCLT#=114 AND...
     C*** END   ADD *** 04/19/06 ********************************************
     C*
     C     ERRSTP        ENDSR
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
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** BEGIN ADD *** 05/23/97 **************************************
     C*
     C                   EXSR      DATCVT
     C*
     C*** WORK CODE SEARCH
     C*
     C     HFLD          IFEQ      'SCCODE'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'WKCD'        ##TBID            4
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C                   SETON                                          7690
     C                   MOVEL     GTKEY         SCCODE
     C                   MOVEL     GTDESC        SCDESC
     C*
     C                   ENDIF                                                  *INKL EQ '1'
     C*                                                                *
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*** LEVEL OF DUTY SEARCH
     C*
     C     HFLD          IFEQ      'SCLVL'
     C*
     C                   CALL      'F2172R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##LVL             3
     C                   PARM      *BLANKS       ##DESW           40
     C*
     C     *INKL         IFEQ      '1'
     C     ##LVL         OREQ      *BLANKS
     C*
     C*
     C                   ELSE
     C*
     C                   MOVEL     ##LVL         SCLVL
     C                   MOVEL     ##DESW        SCDESW
     C*
     C                   SETON                                        9077
     C                   ENDIF                                                  ##LVL IFNE
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** END ADD *** 05/23/97 ****************************************
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
     C************** END   ADD ***************** 01/08/98 *************
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      *BLANKS       SCCODE
     C                   MOVE      *BLANKS       SCDESC
     C                   Z-ADD     0             SCWKDT
     C                   MOVE      *BLANKS       SCLVL
     C                   MOVE      *BLANKS       SCDESW
     C*** BEGIN DEL *** 06/12/97 **************************************
     C**********           Z-ADD0         SCPCT
     C*** END ADD *** 06/12/97 ****************************************
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     WHCODE        SCCODE
     C*** BEGIN DEL *** 05/23/97 **************************************
     C**********           SELEC
     C********** WHCODE    WHEQ '296'
     C**********           MOVELXXDES1    SCDESC    P
     C********** WHCODE    WHEQ '297'
     C**********           MOVELXXDES2    SCDESC    P
     C********** WHCODE    WHEQ '552'
     C**********           MOVELXXDES3    SCDESC    P
     C**********           OTHER
     C**********           MOVE *BLANKS   SCDESC
     C**********           ENDSL
     C*** END DEL *** 05/23/97 ****************************************
     C*
     C*** BEGIN ADD *** 05/23/97 **************************************
     C                   MOVEL     SCCODE        KKKY
     C                   MOVEL     'WKCD'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     GTDESC        SCDESC
     C                   ELSE
     C                   MOVE      *BLANKS       SCDESC
     C                   ENDIF
     C*** END ADD *** 05/23/97 ****************************************
     C*
     C*** BEGIN ADD *** 12/19/97 **************************************
     C                   Z-ADD     WHWKDT        SVWKDT
     C*** END   ADD *** 12/19/97 **************************************
     C                   Z-ADD     WHWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWKDT
     C*
     C                   MOVEL(P)  WHLVL         SCLVL
     C*** BEGIN DEL *** 06/12/97 **************************************
     C**********           Z-ADDWHPCT     SCPCT
     C*** END DEL *** 06/12/97 ****************************************
     C*
     C                   MOVE      *BLANKS       SCDESW
     C     WHLVL         IFNE      *BLANKS
     C                   MOVEL(P)  WHLVL         KKKEY
     C     DLKY          CHAIN     PDL01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     DLDESC        SCDESW
     C                   ENDIF                                                  END *IN99
     C                   ENDIF                                                  END WHLVL
     C                   ENDIF                                                  *IN40 EQ '0'
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   Z-ADD     CMCO#         WHCO#
     C                   Z-ADD     CMSUB#        WHSUB#
     C                   Z-ADD     CMFND         WHFND
     C                   MOVE      CMFYR         WHFYR
     C                   Z-ADD     CMCASE        WHCASE
     C                   MOVEL     SCCODE        WHCODE
     C                   Z-ADD     SCWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WHWKDT
     C                   MOVEL     SCLVL         WHLVL
     C*** BEGIN DEL *** 06/12/97 **************************************
     C**********           Z-ADDSCPCT     WHPCT
     C*** END DEL *** 06/12/97 ****************************************
     C*** ADD SEQ NUMBER
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other function keys
     C*================================================================
     C     OTHFKS        BEGSR
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
     C*** Beg Add *** 07/07/11 ****************************************
     C*
     C     XXCLT#        IFEQ      '114'
     C     ##WCIF        ANDEQ     'Y'
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       #RTCD             1
     C                   PARM                    #CO#
     C                   PARM                    #SUB#
     C                   PARM                    #FND
     C                   PARM      *BLANKS       #OUTS             1
     C*
     C*** VALID RECORD ' ' = YES
     C*
     C     #RTCD         IFEQ      *BLANKS
     C     #OUTS         ANDNE     'Y'                                          NOT AN OUT ACCT
     C                   CALL      'FA771R'
     C                   PARM      *BLANKS       ##MODE            1
     C                   PARM                    #2CO#
     C                   PARM                    #2SUB#
     C                   PARM                    #2FND
     C                   PARM                    #2FYR
     C                   PARM                    #2CASE
     C                   PARM                    ##LTDY            5 0
     C                   PARM                    ##WGRP            3
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C                   ENDIF
     C                   ENDIF
     C*** End Add *** 07/07/11 ****************************************
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'D'                                    09
     C*
     C************* BEGIN ADD ****************** 01/08/98 *************
     C*
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
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
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
      *
     C*********** END   CHANGE ******************* 01/08/98 ***********
     C*** BEGIN ADD *** 04/19/06 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
     C     XXCLT#        IFEQ      '114'
     C                   SETON                                        20
     C                   ELSE
     C                   SETOFF                                       20
     C                   ENDIF                                                  XXCLT#=114
     C*
     C*** END   ADD *** 04/19/06 **************************************
     C*** Beg Add *** 07/07/11 ****************************************
     C                   MOVE      CMCO#         #CO#              3
     C                   MOVE      CMSUB#        #SUB#             5
     C                   MOVE      CMFND         #FND              3
     C*
     C                   MOVE      CMCO#         #2CO#             3 0
     C                   MOVE      CMSUB#        #2SUB#            5 0
     C                   MOVE      CMFND         #2FND             3 0
     C                   MOVE      CMFYR         #2FYR             3 0
     C                   MOVE      CMCASE        #2CASE            7 0
     C*** End Add *** 07/07/11 ****************************************
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
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C                   TIME                    DFDTTM
     C                   MOVEL     Q1USER        WHUSER
     C                   MOVEL     Q1PGM         WHPGM
     C                   MOVEL     DFTIME        WHTIME
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WHDT
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record after processing
     C*================================================================
     C     WRITER        BEGSR
     C*** BEGIN ADD *** 07/01/99 **************************************
     C*
     C* GET NEXT AVAILABLE SEQUENCE NUMBER
     C*
     C                   Z-ADD     *HIVAL        KKSEQ#
     C*
     C     WHKY3         SETGT     PWH02
     C     WHKY2         READPE    PWH02                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     WHSEQ#        ADD       1             XXSEQ#
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             XXSEQ#
     C*
     C                   ENDIF
     C*
     C*** END   ADD *** 07/01/99 **************************************
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*** BEGIN ADD *** 07/01/99 **************************************
     C                   Z-ADD     XXSEQ#        WHSEQ#
     C*** END   ADD *** 07/01/99 **************************************
     C                   TIME                    DFDTTM
     C                   MOVEL     Q1USER        WHIUSR
     C                   MOVEL     Q1PGM         WHIPGM
     C                   MOVEL     DFTIME        WHITIM
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WHIDT
     C*
     C                   MOVEL     Q1USER        WHUSER
     C                   MOVEL     Q1PGM         WHPGM
     C                   MOVEL     DFTIME        WHTIME
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WHDT
     C*
     C                   WRITE     DBRCDF
     C**********           FEOD FAWHP
     C*
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
