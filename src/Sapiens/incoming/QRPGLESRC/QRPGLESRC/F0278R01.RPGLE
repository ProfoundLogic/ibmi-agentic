     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H**********************************************************
     H* PROGRAM: F0278R01                                      *
     H* DESCRIPTION: WORK WITH TREATMENT                       *
     H**********************************************************
     H* INDICATORS:                                            *
     H**********************************************************
     H* REVISIONS:                                             *
     H*                                                        *
     H* 11/13/13 C:296 PROJECT: MC372                          *
     H*                Change Treatment Plan to Text Area.     *
     H*                                                        *
     H* 01/26/00 C:62 PROJECT: MCMNT                           *
     H*               KEEP FROM FALLING BACK TO WORK WITH.     *
     H*                                                        *
     H* 02/01/99 C:62 PROJECT: 87MHL                           *
     H*               ADDED PARM TO FA703R TO RETRIEVE PGM-ID. *
     H*                                                        *
     H* 10/07/98 C:94 PROJECT: 87MFX                           *
     H*               CHANGE F12=PREVIOUS TO F12=CANCEL        *
     H*                                                        *
     H* 02/08/98 C:62 PROJECT: MC145                           *
     H*               1) REDUCED PARM LIST BY INCLUDING RRN    *
     H*                  OF FATRP.                             *
     H*               2) PUT IN PLACE WW/MAINT STANDARDS.      *
     H*                                                        *
     H* 02/04/98 C:62 PROJECT: MC145                           *
     H*               1) MAKE SURE TIPRIM CONTAINS VALID VALUES*
     H*               2) USE DAISDT INSTEAD OF DASTDT.         *
     H*               3) WHEN COMPARING BGDT AGAINST ENDT,     *
     H*                  THE DATES MUST BE IN CYMD FORMAT.     *
     H*                                                        *
     H* 01/30/98 C:62 PROJECT: MC145                           *
     H*               1) F0070R6 COND TO POS 6 OF FBAL04 = Y   *
     H*               2) ADDED PGM/USER/TIME/DT TO ADD         *
     H*               3) CALL F0279R NOT F0279R01              *
     H*               4) FIXED DATE COMP OF BGDT<LTDT<ENDT     *
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
     H* 09/25/97 C:59 PROJECT: 87MBW CO#3                      *
     H*               Only write records from FADAP that are   *
     H*               in an 'AL' Allowed Status.               *
     H*                                                        *
     H* 09/23/97 C:59 PROJECT: SUPPORT MC087/#8828             *
     H*               Calling incorrect program on darts       *
     H*               correct is F0270R, Change Both keys to   *
     H*               be blank instead of zeros.  Causing      *
     H*               problem in the file.                     *
     H*                                                        *
     H* 08/27/97 C:15 PROJECT: 87MCN                           *
     H*               Added code to call the Log API pgm to    *
     H*               create a 278 log record when a treatment *
     H*               is created.                              *
     H*                                                        *
     H**********************************************************
     H*
     FF0278F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFATRP     UF A E             DISK
     F                                     RENAME(PTR01:DBRCDF)
     F                                     INFDS(DBFDS)
     FFATRL     IF   E           K DISK
     F                                     RENAME(PTR01:PTR02)
     FFATDL1    UF A E           K DISK
     FFADAL     IF   E           K DISK
     FFATIP     O    E             DISK
     FFMCMP     IF   E             DISK
      /COPY SCOPYMT01D
     D*
     D DSSC            S            240    DIM(6)                               Descriptions
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Treatment Plan Infor-
     D                                     mation')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(3)
     D*
     D** NOTES DESCRIPTION FIELDS
     D*
     D                 DS
     D  DSSC1                  1    216
     D  SCDS1                  1     72
     D  SCDS2                 73    144
     D  SCDS3                145    216
     D*
     D                 DS
     D  DSSC2                  1    240
     D  XXDSA                  1     24
     D  SCDS5                 25     96
     D  SCDS6                 97    168
     D  SCDS7                169    240
     D*
     D                 DS
     D  DSSC3                  1    192
     D  XXDSB                  1     48
     D  SCDS9                 49    120
     D  SCDS10               121    192
     D*
     D** ADDITIONAL NOTES DESCRIPTION FIELDS
     D*
     D*
     D                 DS
     D  DSSC4                  1    216
     D  SCDS12                 1     72
     D  SCDS13                73    144
     D  SCDS14               145    216
     D*
     D                 DS
     D  DSSC5                  1    240
     D  XXDSC                  1     24
     D  SCDS16                25     96
     D  SCDS17                97    168
     D  SCDS18               169    240
     D*
     D                 DS
     D  DSSC6                  1    192
     D  XXDSD                  1     48
     D  SCDS20                49    120
     D  SCDS21               121    192
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0

     I*
     I*COPY SCOPYWW01E
     I*
     I*
     I*  USED TO FIND LAST TREATMENT PLAN #
     I*
     IPTR02
     I              TRCO#                       X2CO#
     I              TRSUB#                      X2SUB#
     I              TRFND                       X2FND
     I              TRFYR                       X2FYR
     I              TRCASE                      X2CASE
     I              TRTX#                       X2TX#
     I              TRBGDT                      X2BGDT
     I              TRENDT                      X2ENDT
     I              TRMMI                       X2MMI
     I              TRLTDT                      X2LTDT
     I              TRCONT                      X2CONT
     I              TRAREA                      X2AREA
     I              TRPHN#                      X2PHN#
     I              TRSTAT                      X2STAT
     I              TRUSER                      X2USER
     I              TRPGM                       X2PGM
     I              TRDT                        X2DT
     I              TRTIME                      X2TIME
     I              TRIUSR                      X2IUSR
     I              TRIPGM                      X2IPGM
     I              TRITIM                      X2ITIM
     I              TRIDT                       X2IDT
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          RRN FATRP
     C                   PARM                    @@RRN2            9 0          RRN FMCMP
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#
     C                   PARM                    ##FYR
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*** KEY LIST TO FIND NEXT TX PLAN NUMBER
     C*
     C     TXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKTX#
     C*
     C     TXKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*** KEY LIST TREATMENT PLAN NOTES
     C*
     C     TDKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKTX#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKSEQ#
     C*
     C*** KEY LIST TO FIND ALL ICD9 CODES ASSOCIATED ON INTIAL CREATE
     C*
     C     FADKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*
     C     *LIKE         DEFINE    TRTX#         KKTX#
     C     *LIKE         DEFINE    TDSEQ#        KKSEQ#
     C     *LIKE         DEFINE    TDTYPE        KKTYPE
     C     *LIKE         DEFINE    TRCO#         ##CO#
     C     *LIKE         DEFINE    TRFYR         ##FYR
     C     *LIKE         DEFINE    TRFND         ##FND
     C     *LIKE         DEFINE    TRSUB#        ##SUB#
     C     *LIKE         DEFINE    TRCASE        ##CASE
     C************ BEGIN ADD ******************** 01/08/98 ************
     C     *LIKE         DEFINE    TRCASE        KKCASE
     C     *LIKE         DEFINE    TRFYR         KKFYR
     C     *LIKE         DEFINE    TRFND         KKFND
     C     *LIKE         DEFINE    TRSUB#        KKSUB#
     C     *LIKE         DEFINE    TRCO#         KKCO#
     C************ END   ADD ******************** 01/08/98 ************
     C*** BEGIN ADD *** 01/30/98 **************************************
     C     *LIKE         DEFINE    TRDT          HLBGDT
     C     *LIKE         DEFINE    TRDT          HLENDT
     C     *LIKE         DEFINE    TRDT          HLLTDT
     C*** END   ADD *** 01/30/98 **************************************
     C*================================================================
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
     C* CHECK BEGIN DATE
     C*
     C  N90SCBGDT        COMP      *ZEROS                                 80
     C  N90SCBGDT        COMP      *ZEROS                                 71
     C  N90SCBGDT        COMP      *ZEROS                                 90
     C*
     C  N90SCBGDT        IFNE      *ZEROS
     C                   Z-ADD     SCBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCBGDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9070
     C                   SETON                                        9080      80, POS CURSOR
     C                   ENDIF                                                  END SRERR
     C                   ENDIF                                                  END NE*BLANK
     C*
     C* BEGIN DATE MUST BE >= ACCIDENT DATE.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLBGDT
     C*
     C     HLBGDT        IFLT      CMACDT
     C*
     C                   SETON                                        8590
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* CHECK END DATE
     C*
     C* N90      SCENDT    COMP *ZEROS                   81
     C* N90      SCENDT    COMP *ZEROS                   72
     C* N90      SCENDT    COMP *ZEROS                   90
     C*
     C  N90SCENDT        IFNE      *ZEROS
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCENDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9070
     C                   SETON                                        9081      81, POS CURSOR
     C                   ENDIF                                                  END SRERR
     C                   ENDIF                                                  END NE*BLANK
     C*
     C* END DATE MUST BE >= ACCIDENT DATE.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCENDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLENDT
     C*
     C     HLENDT        IFLT      CMACDT
     C*
     C                   SETON                                        8690
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEGIN CHG *** 02/04/98 **************************************
     C*
     C**N90***** SCENDT    IFEQ *ZEROS
     C********** SCENDT    ORLT SCBGDT
     C  N90SCENDT        IFNE      *ZEROS
     C     HLENDT        ANDLT     HLBGDT
     C*** END   CHG *** 02/04/98 **************************************
     C                   SETON                                        9074
     C                   SETON                                        9081      81, POS CURSOR
     C                   ELSE
     C                   SETON                                        82        IN82, POSITION
     C                   ENDIF
     C*
     C* CHECK MAX MED IMPROVEMENT
     C*
     C  N90SCMMI         IFNE      'Y'
     C     SCMMI         ANDNE     'N'
     C                   SETON                                        9075
     C                   SETON                                        9082      82, POS CURSOR
     C                   ELSE
     C*
     C                   SETON                                        83        IN83, POSITION
     C                   ENDIF
     C*
     C* CHECK Est. Date of Last TX
     C*
     C* N90      SCLTDT    COMP *ZEROS                   83
     C* N90      SCLTDT    COMP *ZEROS                   73
     C* N90      SCLTDT    COMP *ZEROS                   90
     C*
     C  N90SCLTDT        IFNE      *ZEROS
     C                   Z-ADD     SCLTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCLTDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9070
     C                   SETON                                        9083      83, POS CURSOR
     C                   ENDIF                                                  END SRERR
     C                   ENDIF                                                  END NE*BLANK
     C*** BEGIN CHG *** 01/30/98 **************************************
     C*
     C                   Z-ADD     SCBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLBGDT
     C*
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLENDT
     C*
     C                   Z-ADD     SCLTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLLTDT
     C*
     C  N90SCENDT        IFNE      *ZEROS
     C*
     C     SCLTDT        IFNE      *ZEROS
     C*
     C  N90HLLTDT        IFLT      HLBGDT
     C     HLLTDT        ORGT      HLENDT
     C*** END   CHG *** 01/30/98 **************************************
     C                   SETON                                        9076
     C                   SETON                                        9083      83, POS CURSOR
     C                   ELSE
     C                   SETON                                        84        IN84, POSITION
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     SCLTDT        IFNE      *ZEROS
     C*
     C  N90HLLTDT        IFLT      HLBGDT
     C*** END   CHG *** 01/30/98 **************************************
     C                   SETON                                        9076
     C                   SETON                                        9083      83, POS CURSOR
     C                   ELSE
     C                   SETON                                        84        IN84, POSITION
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* CHECK MCO CONTACT NAME, VERIFY NOT BLANK
     C*
     C**N90      SCCONT    IFEQ *BLANKS
     C****                 SETON                     9077
     C****                 SETON                     9084  84, POS CURSOR
     C****                 ELSE
     C****                 SETON                     85    IN85, POSITION
     C****                 ENDIF
     C****
     C**CHECK MCO CONTACT AREA CODE, VERIFY NOT BLANK
     C****
     C**N90      SCAREA    IFEQ *ZEROS
     C****                 SETON                     9078
     C****                 SETON                     9085  85, POS CURSOR
     C****                 ELSE
     C****                 SETON                     86    IN86, POSITION
     C****                 ENDIF
     C****
     C**CHECK MCO CONTACT PHONE#, VERIFY NOT BLANK
     C****
     C**N90      SCPHN#    IFEQ *ZEROS
     C****                 SETON                     9079
     C****                 SETON                     9086  86, POS CURSOR
     C****                 ELSE
     C****                 SETON                     80    IN80, POSITION
     C****                 ENDIF
     C*
     C                   ENDSR
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
     C* KFDO - F6=
     C*================================================================
     C     KFDO          BEGSR
     C*
     C                   CALL      'F0279R01'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      'A'           ##MODE            1
     C                   PARM      *ZEROS        ##RRN             9 0          RRN FATIP
     C                   PARM      @@RRN         ##RRN2            9 0          RRN FATRP
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KGDO - F7=
     C*================================================================
     C     KGDO          BEGSR
     C*
     C                   CALL      'F0279R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0          RRN FATIP
     C                   PARM      @@RRN         ##RRN2            9 0          RRN FATRP
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO - F14-DARTS.
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
     C*** BEGIN ADD *** 02/01/99 **************************************
     C                   PARM      *BLANK        ##PGM            10
     C*** END   ADD *** 02/01/99 **************************************
     C*
     C************** END   ADD ***************** 01/08/98 *************
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     1             Z
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   Z-ADD     *ZEROS        SCBGDT
     C                   Z-ADD     *ZEROS        SCENDT
     C                   MOVE      *BLANK        SCMMI
     C                   Z-ADD     *ZEROS        SCLTDT
     C****                 MOVE *BLANKS   SCCONT
     C****                 Z-ADD*ZEROS    SCAREA
     C****                 Z-ADD*ZEROS    SCPHN#
     C*
     C                   CLEAR                   DSSC1
     C                   CLEAR                   DSSC2
     C                   CLEAR                   DSSC3
     C                   CLEAR                   DSSC4
     C                   CLEAR                   DSSC5
     C                   CLEAR                   DSSC6
     C*
     C                   MOVEL     *BLANKS       SCDS4
     C                   MOVEL     *BLANKS       SCDS8
     C                   MOVEL     *BLANKS       SCDS11
     C                   MOVEL     *BLANKS       SCDS15
     C                   MOVEL     *BLANKS       SCDS19
     C                   MOVEL     *BLANKS       SCDS22
     C*
     C*   SEARCH FOR NEXT TREATMENT NUMBER
     C                   Z-ADD     99999         KKTX#
     C     TXKY          SETGT     PTR02                              99
     C     TXKY1         READPE    PTR02                                  99
     C*
B01  C     *IN99         IFEQ      *OFF
     C     X2TX#         ADD       1             SCTX#
     C                   ELSE
     C                   Z-ADD     1             SCTX#
     C                   ENDIF                                                    *IN99
     C*
     C                   ELSE
     C*
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           Z-ADDTRBGDT    SCBGDT
     C**********           Z-ADDSCBGDT    SRDTI
     C*** END   DEL *** 12/02/97 **************************************
     C*
     C*** BEGIN CHG *** 12/02/97 **************************************
     C                   Z-ADD     TRBGDT        SRDTI
     C*** END   CHG *** 12/02/97 **************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCBGDT
     C*
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           Z-ADDTRENDT    SCENDT
     C*** END   DEL *** 12/02/97 **************************************
     C*
     C                   Z-ADD     TRENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCENDT
     C                   MOVE      TRMMI         SCMMI
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           Z-ADDTRLTDT    SCLTDT
     C*** END   DEL *** 12/02/97 **************************************
     C                   Z-ADD     TRLTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCLTDT
     C****                 MOVELTRCONT    SCCONT
     C****                 MOVE TRAREA    SCAREA
     C****                 MOVE TRPHN#    SCPHN#
     C                   MOVE      TRTX#         SCTX#
     C                   MOVE      TRTX#         KKTX#
     C*
     C                   DO        2             X
     C*
     C     X             IFEQ      1
     C                   MOVEL     'D'           KKTYPE
     C                   ELSE
     C                   MOVEL     'N'           KKTYPE
     C                   ENDIF
     C*
     C                   DO        3             CNT
     C*
     C                   Z-ADD     CNT           KKSEQ#
     C     TDKY          CHAIN     PTD01                              99
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     TDDESC        DSSC(Z)
     C*
     C                   SELECT
     C     Z             WHENEQ    1
     C                   MOVE      TDDESC        XXDS1
     C     Z             WHENEQ    2
     C                   MOVE      TDDESC        XXDS2
     C     Z             WHENEQ    3
     C                   MOVE      TDDESC        XXDS3
     C     Z             WHENEQ    4
     C                   MOVE      TDDESC        XXDS4
     C     Z             WHENEQ    5
     C                   MOVE      TDDESC        XXDS5
     C     Z             WHENEQ    6
     C                   MOVE      TDDESC        XXDS6
     C                   ENDSL
     C*
     C                   ADD       1             Z
     C                   ENDIF                                                    *IN99
     C*
     C                   ENDDO                                                    END CNT
     C                   ENDDO                                                    END X
     C*
     C                   MOVEL     DSSC(1)       DSSC1
     C                   MOVEL     DSSC(2)       DSSC2
     C                   MOVEL     DSSC(3)       DSSC3
     C                   MOVEL     DSSC(4)       DSSC4
     C                   MOVEL     DSSC(5)       DSSC5
     C                   MOVEL     DSSC(6)       DSSC6
     C*
     C                   MOVEL     XXDS1         SCDS4
     C                   MOVEL     XXDS2         SCDS8
     C                   MOVEL     XXDS3         SCDS11
     C                   MOVEL     XXDS4         SCDS15
     C                   MOVEL     XXDS5         SCDS19
     C                   MOVEL     XXDS6         SCDS22
     C*
     C                   MOVE      XXDSA         SCDS4
     C                   MOVE      XXDSB         SCDS8
     C                   MOVE      XXDSC         SCDS15
     C                   MOVE      XXDSD         SCDS19
      /Free
         // Beg Add *** 11/13/13 ************************************
         // Populating Text Area SCDSC1 on screen SCR02 and
         // SCDSC2 on screen SRC03

        scDSC1 = DSSC1 + %SUBST(SCDS4:1:48) + DSSC2 +
                 %SUBST(SCDS8:1:24) + DSSC3 + SCDS11;

        scDSC2 = DSSC4 + %SUBST(SCDS15:1:48) + DSSC5 +
                 %SUBST(SCDS19:1:24) + DSSC6 + SCDS22;

         // End Add *** 11/13/13 ************************************
      /End-Free
     C                   ENDIF                                                    *IN40
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C*  FATRP FILE
     C                   MOVE      CMCO#         TRCO#
     C                   MOVE      CMSUB#        TRSUB#
     C                   MOVE      CMFND         TRFND
     C                   MOVE      CMFYR         TRFYR
     C                   MOVE      CMCASE        TRCASE
     C                   MOVE      SCTX#         TRTX#
     C*
     C                   Z-ADD     SCBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TRBGDT
     C*
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TRENDT
     C*
     C                   Z-ADD     SCLTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TRLTDT
     C*
     C                   MOVE      SCMMI         TRMMI
     C*  FATDP FILE
     C                   MOVE      CMCO#         TDCO#
     C                   MOVE      CMSUB#        TDSUB#
     C                   MOVE      CMFND         TDFND
     C                   MOVE      CMFYR         TDFYR
     C                   MOVE      CMCASE        TDCASE
     C                   MOVE      SCTX#         TDTX#
     C                   MOVE      'A'           TDSTAT
     C*
     C*  SETUP FOR NOTES AND ADDITIONAL NOTES  -- SETS UP HALF LINES
     C*
     C                   MOVEL     SCDS4         XXDS1            48
     C                   MOVE      SCDS4         XXDSA
     C                   MOVEL     SCDS8         XXDS2            24
     C                   MOVE      SCDS8         XXDSB
     C                   MOVE      SCDS11        XXDS3            72
     C*
     C                   MOVEL     SCDS15        XXDS4            48
     C                   MOVE      SCDS15        XXDSC
     C                   MOVEL     SCDS19        XXDS5            24
     C                   MOVE      SCDS19        XXDSD
     C                   MOVE      SCDS22        XXDS6            72
     C*
     C                   MOVEL     DSSC1         DSSC(1)
     C                   MOVEL     DSSC2         DSSC(2)
     C                   MOVEL     DSSC3         DSSC(3)
     C                   MOVEL     DSSC4         DSSC(4)
     C                   MOVEL     DSSC5         DSSC(5)
     C                   MOVEL     DSSC6         DSSC(6)
     C*
      /Free
         // Beg Add *** 11/13/13 ************************************
         // Load from text area insted of text box.
            Exsr srLdText;
         // Beg Add *** 11/13/13 ************************************
      /End-Free
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
     C*
     C     *INKF         CASEQ     *ON           KFDO                           F6=
     C     *INKG         CASEQ     *ON           KGDO                           F7=
     C     *INKN         CASEQ     *ON           KNDO                           F14=DARTS
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09
     C     @@MODE        COMP      '2'                                    09
     C     @@MODE        COMP      '3'                                    09
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40
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
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C                   MOVE      CMFND         ##FND
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
      *
     C                   Z-ADD     *ZEROS        X                 2 0
     C                   Z-ADD     *ZEROS        Y                 2 0
     C                   Z-ADD     *ZEROS        Z                 2 0
     C                   Z-ADD     *ZEROS        CNT               2 0
     C*
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
     C*
     C                   Z-ADD     1             Z
     C                   MOVEL     'D'           KKTYPE
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        TRUSER
     C                   MOVEL     Q1PGM         TRPGM
     C                   TIME                    DFDTTM
     C                   MOVEL     DFTIME        TRTIME
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TRDT
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C*
     C*
     C*
     C                   MOVEL     Q1USER        TDUSER
     C                   MOVEL     Q1PGM         TDPGM
     C                   MOVEL     DFTIME        TDTIME
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TDDT
     C*
     C*  REPEAT TWICE -- FOR NOTES AND ADDITIONAL NOTES
     C*  FIRST PASS WRITES NOTES, SECOND PASS WRITES ADDITIONAL NOTES
     C*
     C                   DO        2             X
     C*
     C*  REPEAT THREE TIMES -- THE FIRST WILL UPDATE THE FIRST 264
     C*  CHARACTERS OF THE NOTES OR ADDITIONAL NOTES THEN IT WILL
     C*  UPDATE THE SECOND AND THIRD SET OF 264 CHARACTERS
     C*
     C                   DO        3             CNT
     C*
     C                   Z-ADD     CNT           KKSEQ#
     C                   Z-ADD     CNT           TDSEQ#
     C*
     C     TDKY          CHAIN     PTD01                              99
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     DSSC(Z)       TDDESC
     C*
     C                   SELECT
     C     Z             WHENEQ    1
     C                   MOVE      XXDS1         TDDESC
     C     Z             WHENEQ    2
     C                   MOVE      XXDS2         TDDESC
     C     Z             WHENEQ    3
     C                   MOVE      XXDS3         TDDESC
     C     Z             WHENEQ    4
     C                   MOVE      XXDS4         TDDESC
     C     Z             WHENEQ    5
     C                   MOVE      XXDS5         TDDESC
     C     Z             WHENEQ    6
     C                   MOVE      XXDS6         TDDESC
     C                   ENDSL
     C*
     C                   ADD       1             Z
     C*
     C                   UPDATE    PTD01
     C                   ENDIF
     C                   ENDDO                                                       END Z
     C                   MOVEL     'N'           KKTYPE
     C                   ENDDO                                                       END X
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   Z-ADD     1             Z
     C*
     C                   CLEAR                   DBRCDF
     C                   CLEAR                   PTD01
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        TRIUSR
     C                   MOVEL     Q1PGM         TRIPGM
     C                   TIME                    DFDTTM
     C                   MOVEL     DFTIME        TRITIM
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           MOVELSRDTO     XXDATE
     C**********           Z-ADDXXDATE    TRIDT
     C*** END   DEL *** 12/02/97 **************************************
     C*
     C*** BEGIN CHG *** 12/02/97 **************************************
     C                   MOVEL     SRDTO         TRIDT
     C*** END   CHG *** 12/02/97 **************************************
     C*** BEGIN ADD *** 01/30/98 **************************************
     C                   MOVEL     Q1USER        TRUSER
     C                   MOVEL     Q1PGM         TRPGM
     C                   MOVEL     DFTIME        TRTIME
     C                   MOVEL     SRDTO         TRDT
     C*** END   ADD *** 01/30/98 **************************************
     C                   WRITE     DBRCDF
     C                   FEOD      FATRP
     C*
     C*****************************************************************
     C*
     C                   MOVEL     Q1USER        TDIUSR
     C                   MOVEL     Q1PGM         TDIPGM
     C                   MOVEL     DFTIME        TDITIM
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           MOVELSRDTO     XXDATE
     C**********           Z-ADDXXDATE    TDIDT
     C*** END   DEL *** 12/02/97 **************************************
     C*
     C*** BEGIN CHG *** 12/02/97 **************************************
     C                   MOVEL     SRDTO         TDIDT
     C*** END   CHG *** 12/02/97 **************************************
     C*** BEGIN ADD *** 01/30/98 **************************************
     C                   MOVEL     Q1USER        TDUSER
     C                   MOVEL     Q1PGM         TDPGM
     C                   MOVEL     DFTIME        TDTIME
     C                   MOVEL     SRDTO         TDDT
     C*** END   ADD *** 01/30/98 **************************************
     C*
     C*  FIRST PASS WRITES NOTES, SECOND PASS WRITES ADDITIONAL NOTES
     C*
     C*  REPEAT THREE TIMES -- THE FIRST WILL WRITE THE FIRST 264
     C*  CHARACTERS OF THE NOTES OR ADDITIONAL NOTES THEN IT WILL
     C*  WRITE THE SECOND AND THIRD SET OF 264 CHARACTERS
     C*
     C                   DO        2             X
     C                   DO        3             CNT
     C*
     C     X             IFEQ      1
     C                   MOVEL     'D'           TDTYPE
     C                   ELSE
     C                   MOVEL     'N'           TDTYPE
     C                   ENDIF
     C*
     C                   Z-ADD     CNT           TDSEQ#
     C                   MOVEL     DSSC(Z)       TDDESC
     C*
     C                   SELECT
     C     Z             WHENEQ    1
     C                   MOVE      XXDS1         TDDESC
     C     Z             WHENEQ    2
     C                   MOVE      XXDS2         TDDESC
     C     Z             WHENEQ    3
     C                   MOVE      XXDS3         TDDESC
     C     Z             WHENEQ    4
     C                   MOVE      XXDS4         TDDESC
     C     Z             WHENEQ    5
     C                   MOVE      XXDS5         TDDESC
     C     Z             WHENEQ    6
     C                   MOVE      XXDS6         TDDESC
     C                   ENDSL
     C*
     C                   ADD       1             Z
     C*
     C                   WRITE     PTD01
     C*
     C                   FEOD      FATDL1
     C                   MOVEL     *BLANKS       TDDESC
     C*
     C                   ENDDO                                                       END Z
     C                   ENDDO                                                       END X
     C                   MOVE      '1'           OUTFLG
     C*****************************************************************
     C* WRITE ALL FATIP ICD9 RECORDS ASSOCIATED WITH CASE#
     C*
     C     FADKY         CHAIN     PDA01                              60
     C     *IN60         DOWEQ     '0'
     C*** BEGIN ADD *** 09/25/97 **************************************
      *  Only add records from fadap that are in an 'AL' Allowed status
     C     DAISTS        IFEQ      'AL '
     C*** BEGIN ADD *** 09/25/97 **************************************
     C                   MOVEL     DACO#         TICO#
     C                   MOVEL     DASUB#        TISUB#
     C                   MOVEL     DAFND         TIFND
     C                   MOVEL     DAFYR         TIFYR
     C                   MOVEL     DACASE        TICASE
     C*** BEGIN ADD *** 02/04/98 **************************************
     C*
     C* IF DAPRIM IS NOT EQ 'Y' OR 'N' THEN MOVE 'N' TO TIPRIM
     C*
     C     DAPRIM        IFEQ      'Y'
     C     DAPRIM        OREQ      'N'
     C*
     C*** END   ADD *** 02/04/98 **************************************
     C                   MOVEL     DAPRIM        TIPRIM
     C*** BEGIN ADD *** 02/04/98 **************************************
     C                   ELSE
     C*
     C                   MOVEL     'N'           TIPRIM
     C*
     C                   ENDIF
     C*** END   ADD *** 02/04/98 **************************************
     C*** BEGIN CHG *** 02/04/98 **************************************
     C*
     C* IF DAPRIM EQ 'Y' THEN MOVE DAISDT ELSE *ZEROS.
     C*
     C     DAPRIM        IFEQ      'Y'
     C*
     C**********           MOVELDASTDT    TIBGDT
     C                   MOVE      DAISDT        TIBGDT
     C*
     C                   ELSE
     C*
     C                   MOVE      *ZEROS        TIBGDT
     C*
     C                   ENDIF
     C*** END   CHG *** 02/04/98 **************************************
     C                   MOVE      SCTX#         TITX#
     C                   MOVE      'A'           TISTAT
     C                   MOVEL     Q1USER        TIIUSR
     C                   MOVEL     Q1PGM         TIIPGM
     C                   MOVEL     DAICD9        TIICD9
     C                   MOVEL     DALOST        TILOST
     C                   MOVEL     DFTIME        TIITIM
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C*** BEGIN DEL *** 12/02/97 **************************************
     C**********           MOVELSRDTO     XXDATE
     C**********           Z-ADDXXDATE    TIIDT
     C*** END   DEL *** 12/02/97 **************************************
     C*
     C*** BEGIN CHG *** 12/02/97 **************************************
     C                   MOVEL     SRDTO         TIIDT
     C*** END   CHG *** 12/02/97 **************************************
     C*
     C*** BEGIN ADD *** 01/30/98 **************************************
     C                   MOVEL     Q1USER        TIUSER
     C                   MOVEL     Q1PGM         TIPGM
     C                   MOVEL     DFTIME        TITIME
     C                   MOVEL     SRDTO         TIDT
     C*** END   ADD *** 01/30/98 **************************************
     C*
     C                   WRITE     PTI01
     C*** BEGIN ADD *** 09/25/97 **************************************
      *  Only add records from fadap that are in an 'AL' Allowed status
     C                   ENDIF                                                  EQ 'AL' STAT
     C*** BEGIN ADD *** 09/25/97 **************************************
     C*
     C     FADKY         READE     PDA01                                  60
     C                   ENDDO
     C*** BEGIN ADD *** 01/30/98 **************************************
     C*
     C* CONDITIONED CALL OF F0070R6 BASED ON 'Y' IN POS 6 OF FBAL04.
     C*
     C     ##EDI         IFEQ      'Y'
     C*** END   ADD *** 01/30/98 **************************************
     C*
     C*** BEGIN ADD *** 08/27/97 ***********************************************
     C*  Call the log API to create a log record for this Treatment Plan.
     C*
     C                   MOVE      CMCO#         QQCO#
     C                   MOVE      CMSUB#        QQSUB#
     C                   MOVE      CMFND         QQFND
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   MOVE      *ZEROS        QQTX#
     C                   MOVE      SCTX#         QQTX#
     C                   CALL      'F0070R6'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQTX#             9
     C                   PARM      'A'           QQACTF            1
     C                   PARM      *BLANKS       QQMGID            7
     C                   PARM      '1'           QQRTCD            1
     C                   PARM      '278'         QQTSET            3
     C*** END ADD ***** 08/27/97 ***********************************************
     C*
     C*** BEGIN ADD *** 01/30/98 **************************************
     C*
     C                   ENDIF
     C*
     C*   CALL TO F0279F TO VIEW AND UPDATE ICD9 CODES
     C*
     C                   CALL      'F0279R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##CALL            1
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      DBRRN         ##RRN2
     C*** BEGIN ADD *** 01/26/00 **************************************
     C     *INKL         IFEQ      *ON
     C*
     C                   SETOFF                                       KL
     C*
     C                   ENDIF
     C*** END   ADD *** 01/26/00 **************************************
     C*
     C*** END   ADD *** 01/30/98 **************************************
     C*
     C                   ENDSR
      /Free
         // Beg Add *** 11/13/13 ************************************
          BegSr srLdText;
            xxds1 = %subst(SCDSC1:217:48);
            xxdsa = %subst(SCDSC1:265:24);
            xxds2 = %subst(SCDSC1:505:24);
            xxdsb = %subst(SCDSC1:529:48);
            xxds3 = %subst(SCDSC1:721:72);

            xxds4 = %subst(SCDSC2:217:48);
            xxdsc = %subst(SCDSC2:265:24);
            xxds5 = %subst(SCDSC2:505:24);
            xxdsd = %subst(SCDSC2:529:48);
            xxds6 = %subst(SCDSC2:721:72);

            DSSC(1) = %subst(SCDSC1:1:216);
            DSSC(2) = %subst(SCDSC1:265:240);
            DSSC(3) = %subst(SCDSC1:529:192);
            DSSC(4) = %subst(SCDSC2:1:216);
            DSSC(5) = %subst(SCDSC2:265:240);
            DSSC(6) = %subst(SCDSC2:529:192);
          EndSr;
         // End Add *** 11/13/13 ************************************
      /End-Free
