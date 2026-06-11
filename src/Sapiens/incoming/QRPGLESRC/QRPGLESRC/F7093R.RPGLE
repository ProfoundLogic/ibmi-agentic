     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F7093R - CASE TRANSFER/DELETION REASON UPDATE/DISPLAY
     H*****************************************************************
     H*****************************************************************
     H* DESCRIPTION: CASE TRANSFER/DELETION REASON PROGRAM
     H*
     H* 09/21/98  C:94  PROJECT: MC151
     H*
     H*****************************************************************
     H** REVISIONS:
     H*  11/19/99 C:31  Project: MC151
     H*                 Claim Transfer/Deletion
     H*                 1. Assume comment from code search
     H*
     H*****************************************************************
     H*
     FF7093F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     FFTGTP     IF   E             DISK
     F*
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*
     FSMUPL2    IF   E           K DISK
     F*  Physical file definition (for maintaining record)
     FFMCMP     UF A E             DISK
     F                                     RENAME(PCM01:DBRCDF)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Case Transfer/Deleti-
     D                                     on Reason')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN1            9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@RRN3            9 0
     C                   PARM                    @@TD              1
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@SS#             9
     C*** BEG CHG *** 11/30/98 ***************************************
     C                   PARM                    @@ACDT            8 0
     C**********           PARM           @@ACDT  70
     C*** END CHG *** 11/30/98 ***************************************
     C                   PARM                    @@LSN            30
     C                   PARM                    @@FSN            15
     C                   PARM                    @@MID             1
     C                   PARM                    @@ADR1           40
     C                   PARM                    @@ADR2           40
     C                   PARM                    @@CTY            25
     C                   PARM                    @@ST              2
     C                   PARM                    @@ZIP             9 0
     C                   PARM                    @@DOB             7 0
     C                   PARM                    @@SEX             1
     C                   PARM                    @@FLAG            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     MUKY          KLIST
     C                   KFLD                    KKUSER
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKTKEY
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    Q1USER        KKUSER
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C*
     C/COPY SCOPYMT01C
     C*
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
     C     SCUSER        IFEQ      *BLANKS
     C                   SETON                                        9071
     C                   GOTO      ENDCK1
     C                   ELSE
     C                   MOVEL(P)  SCUSER        KKUSER
     C     MUKY          CHAIN     PUP01                              99
     C                   MOVEL(P)  UPUSNM        SCUNAM
     C                   SETON                                        9072
     C                   ENDIF
     C*
     C     SCCODE        IFEQ      *BLANKS
     C                   SETON                                        9073
     C                   GOTO      ENDCK1
     C                   ELSE
     C*
     C                   MOVEL(P)  CMCO#         KKCO#
     C                   MOVEL(P)  'DLTR'        KKTBID            4
     C                   MOVEL(P)  SCCODE        KKTKEY           10
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        906272
     C                   ELSE
     C                   MOVEL(P)  GTKEY         SCCODE
     C                   MOVEL(P)  GTDESC        SCDESC
     C*** BEGIN ADD *** 11/19/99 **************************************
     C     SCCOM1        IFEQ      *BLANKS
     C                   MOVEL(P)  GTDESC        SCCOM1
     C                   ENDIF                                                  COM1 = blanks
     C*** END ADD *** 11/19/99 ****************************************
     C                   SETON                                        9074
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCCOM1        IFEQ      *BLANKS
     C                   SETON                                        907475
     C                   ELSE
     C                   SETOFF                                       90
     C                   ENDIF
     C*
     C     ENDCK1        ENDSR
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
     C*** TRANSFER/DELETE CODE SEARCH ***
     C*
     C     HFLD          IFEQ      'SCCODE'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'DLTR'        ##TBID            4
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      '1'           OUTFLG
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        7290
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL(P)  GTKEY         SCCODE
     C                   MOVEL(P)  GTDESC        SCDESC
     C*** BEGIN ADD *** 11/19/99 **************************************
     C     SCCOM1        IFEQ      *BLANKS
     C                   MOVEL(P)  GTDESC        SCCOM1
     C                   ENDIF                                                  COM1 = blanks
     C*** END ADD *** 11/19/99 ****************************************
     C*
     C                   SETON                                        7490
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL(P)  Q1USER        KKUSER
     C     MUKY          CHAIN     PUP01                              99
     C                   MOVEL(P)  UPUSNM        SCUNAM
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   SETOFF                                       1110
     C     @@TD          IFEQ      'D'
     C                   SETON                                        11
     C                   ELSE
     C                   SETON                                        10
     C                   ENDIF
     C*
     C** Get standard screen headings
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          SCHD1
     C                   MOVEL     #1L2          SCHD2
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C*
     C*                    MOVELSCUSER    ##USER    P
     C                   MOVE(P)   GTKEY         XXCODE           10 0
     C*                    MOVELSCCOM1    ##COM1    P
     C*                    MOVELSCCOM2    ##COM2    P
     C*                    MOVELSCCOM3    ##COM3    P
     C*
     C                   CALL      'F7095R'
     C                   PARM      @@RRN         ##RRN
     C                   PARM      @@CO#         ##CO#             3 0
     C                   PARM      @@SUB#        ##SUB#            5 0
     C                   PARM      @@FND         ##FND             3 0
     C                   PARM      @@FYR         ##FYR             3 0
     C                   PARM      @@CASE        ##CASE            7 0
     C                   PARM      @@EMP#        ##EMP#            9 0
     C                   PARM      @@DIV         ##DIV             5 0
     C                   PARM      @@SS#         ##SS#             9
     C*** BEG CHG *** 11/30/98 ****************************************
     C                   PARM      @@ACDT        ##ACDT            8 0
     C**********           PARM @@ACDT    ##ACDT  70
     C*** END CHG *** 11/30/98 ****************************************
     C                   PARM      @@LSN         ##LSN            30
     C                   PARM      @@FSN         ##FSN            15
     C                   PARM      @@MID         ##MID             1
     C                   PARM      @@ADR1        ##ADR1           40
     C                   PARM      @@ADR2        ##ADR2           40
     C                   PARM      @@CTY         ##CTY            25
     C                   PARM      @@ST          ##ST              2
     C                   PARM      @@ZIP         ##ZIP             9 0
     C                   PARM      @@DOB         ##DOB             7 0
     C                   PARM      @@SEX         ##SEX             1
     C                   PARM      SCUSER        ##USER           10
     C                   PARM      XXCODE        ##CODE           10 0
     C                   PARM      SCCOM1        ##COM1           70
     C                   PARM      SCCOM2        ##COM2           70
     C                   PARM      SCCOM3        ##COM3           70
     C                   PARM      @@FLAG        ##FLAG            1
     C                   PARM      @@TD          ##TD              1
     C                   PARM      @@RRN3        ##RRN3            9 0
     C                   PARM      'N'           ##WCMS            1
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FMCMP
     C                   ENDSR
