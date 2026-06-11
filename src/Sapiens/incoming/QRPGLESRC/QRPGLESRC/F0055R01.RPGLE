     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0055R01 - SUBSCRIBER INFORMATION  C:62 12/16/97             *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*               F0050R                                          *
     H*  CALLS TO:                                                    *
     H*               F0055R02                                        *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  08/15/00 C:62  PROJECT: MC215                                *
     H*                 REPLACE SFBPF REFERENCES WITH APIS.           *
     H*                                                               *
     H*  06/19/00 C:62 SUPPORT: MC065 19768                           *
     H*                SWAP OUT USAGE OF STABL TO STSTL3, SO THAT UC  *
     H*                BECOMES INVALID                                *
     H*                                                               *
     H*  06/30/99 C:62 PROJECT: MC206                                 *
     H*                ADD PARM FOR CONTINUE WORKFLOW FOR F0055R02.   *
     H*                                                               *
     H*  12/01/98 C:94 PROJECT: MC145                                 *
     H*                ADDED F14=DARTS TO PROGRAM                     *
     H*                                                               *
     H*  10/07/98 C:94 PROJECT: 87MFX                                 *
     H*                CHANGE F12=PREVIOUS TO F12=CANCEL              *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF0055F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMSMP     UF A E             DISK
     F                                     RENAME(PSM01:DBRCDF)
     FFMSML     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
     F*** BEGIN CHG *** 06/19/00 **************************************
     F**********STABL   IF  E           K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END   CHG *** 06/19/00 **************************************
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
      /COPY SCOPYMT01D
     D SYN             S             10    DIM(10) CTDATA PERRCD(1)             SC DESC
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  SYD                    1     10
     D                                     DIM(10)                              DB SYSTEM
     D  SMSY01                 1      1
     D  SMSY02                 2      2
     D  SMSY03                 3      3
     D  SMSY04                 4      4
     D  SMSY05                 5      5
     D  SMSY06                 6      6
     D  SMSY07                 7      7
     D  SMSY08                 8      8
     D  SMSY09                 9      9
     D  SMSY10                10     10
     D                 DS
     D  SYS                    1     10
     D                                     DIM(10)                              SC SYSTEM
     D  SCSY01                 1      1
     D  SCSY02                 2      2
     D  SCSY03                 3      3
     D  SCSY04                 4      4
     D  SCSY05                 5      5
     D  SCSY06                 6      6
     D  SCSY07                 7      7
     D  SCSY08                 8      8
     D  SCSY09                 9      9
     D  SCSY10                10     10
     D                 DS
     D  DSY                    1    400
     D                                     DIM(10)                              SC DESC
     D  SCSD01                 1     40
     D  SCSD02                41     80
     D  SCSD03                81    120
     D  SCSD04               121    160
     D  SCSD05               161    200
     D  SCSD06               201    240
     D  SCSD07               241    280
     D  SCSD08               281    320
     D  SCSD09               321    360
     D  SCSD10               361    400
     D*** BEGIN DEL *** 08/15/00 **************************************
     D**********  DS
     D**********                              1  10 FBAL04
     D**********                              5   5 XXARIN
     D*** END   DEL *** 08/15/00 **************************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Subscriber Inform-
     D                                     ation')
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
     C*
     C* SEARCH PARM LIST
     C*
     C     ACDPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*
     C* SEARCH PARM LIST
     C*
     C     ACDPL1        PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C*** BEGIN ADD *** 06/30/90 **************************************
     C                   PARM      'Y'           ##CONT            1
     C*** END   ADD *** 06/30/90 **************************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  State code search parm list
     C*
     C     STCPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           KKFKEY  2
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C     SMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID
     C                   KFLD                    KKGKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    SMCO#         ##CO#
     C     *LIKE         DEFINE    SMSUB#        KKSUB#
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKGKEY
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     SMSTAT        IFEQ      'I'
     C                   MOVE      'A'           SMSTAT
     C                   ELSE
     C                   MOVE      'I'           SMSTAT
     C                   ENDIF                                                  SMSTAT
     C*
     C                   MOVEL     Q1USER        SMUSER
     C                   MOVEL     Q1PGM         SMPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SMDT
     C*
     C                   MOVEL     DSTIME        SMTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
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
     C*
     C                   EXSR      NEXTSC
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* SUBSCRIBER CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCSUB#        IFEQ      *ZEROS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE SUBSCRIBER CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVE      SCSUB#        KKSUB#
     C*
     C   40
     CANN90SMKY          CHAIN     PSM01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* SUBSCRIBER NAME CANNOT BE BLANK
     C*
     C  N90SCNAME        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C* ADDRESS 1 MUST BE ENTERED.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCADR1        ANDEQ     *BLANKS
     C*
     C                   SETON                                        87  90
     C*
     C                   ENDIF
     C*
     C* CITY MUST BE ENTERTED.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCTY         ANDEQ     *BLANKS
     C*
     C                   SETON                                        88  90
     C*
     C                   ENDIF
     C*
     C* STATE CODE IF NOT BLANK MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL     SCST          KKKEY
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C   99              SETON                                        85  90
     C*
     C                   ENDIF
     C*
     C* ZIP MUST BE ENTERED.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCZIP1        ANDEQ     *ZEROS
     C*
     C                   SETON                                        89  90
     C*
     C                   ENDIF
     C*
     C* THE FIRST SYSTEM CODE MUST BE ENTERED AND VALID
     C*
     C*
     C  N90SYS(1)        IFEQ      *BLANKS
     C*
     C  N90              SETON                                        7490
     C*
     C                   ENDIF
     C*
     C* IF SYSTEM CODE IS NOT BLANK IT MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     1             DO        10            X
     C*
     C     SYS(X)        IFNE      *BLANKS
     C*
     C                   MOVEL     'SYS'         ##TBID
     C                   MOVEL     SYS(X)        KKGKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C     *IN90         ANDEQ     *OFF
     C*
     C     X             ADD       74            W
     C                   MOVE      *ON           *IN(W)
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C* SYSTEM CODE CAN ONLY BE USED ONCE
     C*
     C                   Z-ADD     10            X
     C*
     C  N90X             DOWGE     1
     C*
     C     SYS(X)        IFNE      *BLANKS
     C*
     C                   Z-ADD     1             Y                 2 0
     C*
     C     SYS(X)        LOOKUP    SYS(Y)                                 99
     C*
     C     X             IFNE      Y
     C     *IN99         ANDEQ     *ON
     C*
     C     X             ADD       49            W
     C                   MOVE      *ON           *IN(W)
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   SUB       1             X
     C*
     C                   ENDDO
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Searches.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C     HFLD          IFEQ      'SCST'
     C*
     C                   CALL      'SY601R'      STCPL
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        8690
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PST02                              99
     C*
     C                   MOVEL     STKEY         SCST
     C*
     C                   SETON                                        8790
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     1             X
     C                   MOVEL     'SYS'         ##TBID
     C*
     C     HFLD          LOOKUP    SYN(X)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C     29            ADD       X             W                 2 0
     C                   MOVE      *ON           *IN(W)
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C                   MOVEL     GTKEY         SYS(X)
     C                   MOVEL     GTDESC        DSY(X)
     C*
     C     X             IFLT      10
     C*
     C     30            ADD       X             W
     C                   MOVE      *ON           *IN(W)
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   SETON                                            90
     C*
     C                   ENDSR
     C*** BEG ADD *** 12/01/98 ****************************************
     C*================================================================
     C* KNDO  - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      SCSUB#        #1SUB#
     C*
     C                   CALL      'FA717R'
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** END ADD *** 12/01/98 ****************************************
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVEL     SMNAME        SCNAME
     C                   MOVEL     SMADR1        SCADR1
     C                   MOVEL     SMADR2        SCADR2
     C                   MOVEL     SMCTY         SCCTY
     C                   MOVEL     SMST          SCST
     C                   MOVEL     SMZIP         SCZIP1
     C                   MOVE      SMZIP         SCZIP2
     C                   MOVE      SMAREA        SCAREA
     C                   MOVEL     SMPHN#        SCPHNA
     C                   MOVE      SMPHN#        SCPHNB
     C*
     C                   MOVEL     'SYS'         ##TBID
     C*
     C     1             DO        10            X                 2 0
     C*
     C                   MOVEL     SYD(X)        SYS(X)
     C*
     C                   MOVEL     SYD(X)        KKGKEY
     C*
     C     SYD(X)        IFNE      *BLANKS
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTDESC        DSY(X)
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       DSY(X)
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   MOVEL     SMSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVE      *ZEROS        SCSUB#
     C                   MOVEL     *BLANKS       SCNAME
     C                   MOVEL     *BLANKS       SCADR1
     C                   MOVEL     *BLANKS       SCADR2
     C                   MOVEL     *BLANKS       SCCTY
     C                   MOVEL     *BLANKS       SCST
     C                   MOVE      *ZEROS        SCZIP1
     C                   MOVE      *ZEROS        SCZIP2
     C                   MOVE      *ZEROS        SCAREA
     C                   MOVE      *ZEROS        SCPHNA
     C                   MOVE      *ZEROS        SCPHNB
     C                   MOVEA     *BLANKS       SYS
     C                   MOVEA     *BLANKS       DSY
     C                   MOVEL     'A'           SCSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         SMCO#
     C                   MOVE      SCSUB#        SMSUB#
     C                   MOVEL     SCNAME        SMNAME
     C                   MOVEL     SCADR1        SMADR1
     C                   MOVEL     SCADR2        SMADR2
     C                   MOVEL     SCCTY         SMCTY
     C                   MOVEL     SCST          SMST
     C                   MOVEL     SCZIP1        SMZIP
     C                   MOVE      SCZIP2        SMZIP
     C                   MOVE      SCAREA        SMAREA
     C                   MOVEL     SCPHNA        SMPHN#
     C                   MOVE      SCPHNB        SMPHN#
     C*
     C                   Z-ADD     1             Y                 2 0
     C*
     C     1             DO        10            X
     C*
     C     SYS(X)        IFNE      *BLANKS
     C*
     C                   MOVEL     SYS(X)        SYD(Y)
     C                   ADD       1             Y
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C     Y             IFLE      10
     C*
     C                   MOVEA     *BLANKS       SYD(Y)
     C*
     C                   ENDIF
     C*
     C                   MOVEL     SCSTAT        SMSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* NEXTSC - PROCESS NEXT SCREEN - A/R INFORMATION
     C*================================================================
     C     NEXTSC        BEGSR
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVEL'FB'      KKFKEY
     C*
     C********** FBKY      CHAINSFB01                99
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C*** BEGIN CHG *** 08/15/00 **************************************
     C********** XXARIN    IFEQ 'Y'
     C     ##AR          IFEQ      'Y'
     C*** END   CHG *** 08/15/00 **************************************
     C*
     C                   MOVE      SCSUB#        KKSUB#
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C                   MOVE      DBRRN         ##RRN
     C*
     C     @@MODE        IFEQ      'D'
     C*
     C                   MOVE      'D'           ##MODE
     C*
     C                   ELSE
     C*
     C                   MOVE      'C'           ##MODE
     C*
     C                   ENDIF
     C*
     C                   CALL      'F0055R02'    ACDPL1
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SETON                                        90
     C                   EXSR      GETDTM
     C*
     C     @@MODE        IFEQ      'A'
     C*
     C                   MOVE      *OFF          *IN90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 06/30/99 **************************************
     C*
     C                   ELSE
     C*
     C     ##MED         IFEQ      'Y'
     C*
     C     @@MODE        IFEQ      'D'
     C*
     C                   MOVEL     '1'           ##MODE
     C*
     C                   ELSE
     C*
     C                   MOVEL     ' '           ##MODE
     C*
     C                   ENDIF
     C*
     C                   CALL      'F0046R'
     C                   PARM                    ##MODE            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SETON                                        90
     C                   EXSR      GETDTM
     C*
     C     @@MODE        IFEQ      'A'
     C*
     C                   SETOFF                                       90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 06/30/99 **************************************
     C*
     C                   ENDIF
     C*
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
     C                   MOVEA     '00000'       *IN(30)
     C                   MOVEA     '00000'       *IN(35)
     C                   MOVEA     '00000'       *IN(50)
     C                   MOVEA     '00000'       *IN(55)
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C*** BEG ADD *** 12/01/98 ***************************************
     C     *INKN         CASEQ     *ON           KNDO
     C*** END ADD *** 12/01/98 ***************************************
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*** BEGIN ADD *** 06/30/99 **************************************
     C*
     C**  GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*** END   ADD *** 06/30/99 **************************************
     C*
     C** CHECK FOR DIARY FUNCTION
     C*
     C*                    MOVEL'FB'      KKKEY     P
     C*
     C*          FBKY      CHAINSFB01                99
     C*
     C*          *IN99     IFEQ '0'
     C*
     C*          FBFL02    IFEQ 'Y'
     C*
     C*                    MOVE '1'       *IN16
     C*
     C*                    ELSE                            ELSE FBFL02 = 'Y'
     C*
     C*                    MOVE '0'       *IN16
     C*
     C*                    ENDIF                           END FBFL02 = 'Y'
     C*
     C*                    ENDIF                           END *IN60 SFB01
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        SMUSER
     C                   MOVEL     Q1PGM         SMPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SMDT
     C*
     C                   MOVEL     DSTIME        SMTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   EXSR      NEXTSC
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        SMUSER
     C                   MOVEL     Q1PGM         SMPGM
     C                   MOVEL     Q1PGM         SMIPGM
     C                   MOVEL     Q1USER        SMIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SMDT
     C*
     C                   MOVEL     SRDTO         SMIDT
     C*
     C                   MOVEL     DSTIME        SMTIME
     C                   MOVEL     DSTIME        SMITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FMSMP
     C*
     C                   EXSR      NEXTSC
     C*
     C                   ENDSR
** SCN - SCREEN NAME
SCSY01
SCSY02
SCSY03
SCSY04
SCSY05
SCSY06
SCSY07
SCSY08
SCSY09
SCSY10
