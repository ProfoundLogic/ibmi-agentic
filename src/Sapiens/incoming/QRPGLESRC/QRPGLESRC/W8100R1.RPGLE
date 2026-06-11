     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8100R1 - CARRIER MAINTENANCE
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/12/01 C:90  PROJECT: WC1054                                *
     H*                Add edits to require entry of carrier policy # *
     H*                and not allow dupicate entry if the proper     *
     H*                flags were set in the groups carrier info.     *
     H*                                                               *
     H* 05/14/01 C:61  SUPPORT WA112 27469                            *
     H*                Cancel date should only appear in header if    *
     H*                employer is cancelled.                         *
     H*                                                               *
     H* 12/06/00 C:61  PROJECT WC577                                  *
     H*                Do not allow entry of inactive carrier.        *
     H*                                                               *
     H* 06/10/99  C:33  SEQ#: WA090/18391                             *
     H*                 USE ANOTHER SUBROUTINE DSPLY2 INSTEAD OF      *
     H*                 DSPLYR FOR PROCESSING AFTER DISPLAYING INFO   *
     H*                 TO AVOID RECURSSIVE CALLS TO DSPLYR.          *
     H*                                                               *
     H* 12/14/98  C:33   PROJECT: 45WBO                               *
     H*                  ADDED MORE FIELDS TO SCRAIG                  *
     H*                                                               *
     H* 09/29/98  C:33   PROJECT: 45WBO                               *
     H*                  ADDED SCRAIG FOR AIG CARRIER INFORMATION     *
     H*                                                               *
     H* 08/29/98  C:35   PROJECT: WC862                               *
     H*                  CHANGED SCREEN FIELD FOR CARRIER POLICY      *
     H*                  NUMBER TO 18 CHARACTERS, SINCE THIS IS ALL   *
     H*                  NCCI WILL ACCEPT.                            *
     H*****************************************************************
     FW8100F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMRPP     UF A E             DISK
     F                                     RENAME(PRP01:DBRCDF)
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELP     IF   E             DISK
     FWMRPL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWTGTL     IF   E           K DISK
      **** BEGIN ADD **** 10/12/01 ************************************
     FWMRPL3    IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     FWMGCL     IF   E           K DISK
      **** END   ADD **** 10/12/01 ************************************
      /COPY SCOPYMT01D
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*
     D***PY SCOPYMT01I
     D*
     D OPTNM           C                   CONST('Carrier Information')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  CARRIER FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      **** BEGIN ADD **** 10/12/01 ************************************
     C     RPKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXCAP#           25
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      **** END   ADD **** 10/12/01 ************************************
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C*
     C*  SYSTEM TABLE VALUE FILE - CARRIER ID
     C*
     C     GTKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      **** BEGIN ADD **** 10/12/01 ************************************
     C*
     C*  GROUP CARRIER INFORMATION
     C*
     C     GCKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      **** END   ADD **** 10/12/01 ************************************
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN2            9 0
     C*
     C     *LIKE         DEFINE    FNEXDT        SYSDT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    RPCARR        KKCARR
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    GTKEY         ##KEY
     C     *LIKE         DEFINE    RPEFDT        XXEFDT
     C     *LIKE         DEFINE    RPEXDT        XXEXDT
     C*
     C/COPY SCOPYMT01C
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C*
     C*
     C                   ENDSR                                                  DLTPRE
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C***  BEGIN ADD  ***  06/10/99  ********************************
     C*
     C*  ANY PROCESSING THAT WOULD NORMALLY BE DONE IN DSPLYR MUST NOW
     C*  BE DONE IN DSPLY2
     C*
     C                   EXSR      DSPLY2
     C***  END   ADD  ***  06/10/99  ********************************
     C*
     C***  BEGIN CHG  ***  09/29/98  *********************************
     C**********           EXSR NXTSCR
     C                   SELECT
     C     Q2RECF        WHENEQ    'SCR01'
     C                   Z-ADD     1             XXSCR#            1 0
     C     Q2RECF        WHENEQ    'SCR02'
     C                   Z-ADD     2             XXSCR#
     C     Q2RECF        WHENEQ    'SCR03'
     C                   Z-ADD     3             XXSCR#
     C                   OTHER
     C                   Z-ADD     *ZERO         XXSCR#
     C                   ENDSL
     C*
     C     #OFSCR        CASEQ     XXSCR#        NXTSCR
     C                   ENDCS
     C***  END   CHG  ***  09/29/98  *********************************
     C*
     C                   ENDSR                                                  DSPLYR
     C*
     C***  BEGIN ADD  ***  06/10/99  ********************************
     C*===============================================================
     C* DSPLY2 - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLY2        BEGSR
     C                   ENDSR                                                  DSPLY2
     C***  END   ADD  ***  06/10/99  ********************************
     C*
     C***  BEGIN ADD  ***  09/29/98  *********************************
     C*===============================================================
     C* ERRAIG - SUBROUTINE FOR ERROR CHECKING SCRAIG
     C*===============================================================
     C*
     C     ERRAIG        BEGSR
     C*
     C*  INDEPENDENT SUBCONTRACTOR MUST BE ENTERED
     C*
     C  N90SCIND         IFEQ      *ZEROS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCIND=*ZEROS
     C*
     C*  STATE TAX ID BE ENTERED
     C*
     C  N90SCTAX#        IFEQ      *BLANKS
     C                   SETON                                        72  90
     C                   ENDIF                                                  SCTAX#=*BLANKS
     C*
     C*  AIGRM ZERO BALANCE MUST BE 'Y' OR 'N'
     C*
     C  N90SCZERO        IFNE      'Y'
     C     SCZERO        ANDNE     'N'
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCZERO<>Y/N
     C***  BEGIN ADD  ***  12/14/98  *********************************
     C*
     C*  POLICY PREFIX MUST BE ENTERED
     C*
     C  N90SCPRFX        IFEQ      *BLANKS
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCPRFX=*BLANKS
     C*
     C*  INDENTIFIER MUST BE ENTERED
     C*
     C  N90SCIDNT        IFEQ      *ZEROS
     C                   SETON                                        75  90
     C                   ENDIF                                                  SCIND=*ZEROS
     C***  END   ADD  ***  12/14/98  *********************************
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRAIG
     C***  END   ADD  ***  09/29/98  *********************************
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C* CHECK CARRIER CODE AGAINST SYSTEM TABLE VALUE FILE
     C*
     C                   MOVEL     'CRR'         KKTBID
     C                   MOVEL     SCCARR        KKKEY
     C*
     C  N90GTKY          CHAIN     PGT01                              99
     C*
     C  N90
     CAN 99              SETON                                        7190
     C  N90
     CANN99              MOVEL     GTDESC        SCDESC
      ***** BEG ADD ******* 12/06/00 **********************************
      *
      * Do not allow entry of inactive carrier code
      *
     C  N90GTSTAT        IFNE      'A'
     C                   SETON                                        90  76
     C                   ENDIF
      *
      ***** END ADD ******* 12/06/00 **********************************
     C*
     C* VALIDATE ISSUANCE DATE IF ENTERED
     C*
     C  N90SCISDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCISDT
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF                                                  SRERR EQ *ON
     C*
     C                   ENDIF                                                  SCISDT NE 0
     C*
     C* VALIDATE START DATE IF ENTERED
     C*
     C  N90SCEFDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        7390
     C*
     C                   ENDIF                                                  SRERR EQ *ON
     C*
     C                   ENDIF                                                  SCISDT NE 0
     C*
     C* VALIDATE END DATE IF ENTERED
     C*
     C  N90SCEXDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        7490
     C*
     C                   ENDIF                                                  SRERR EQ *ON
     C*
     C                   ENDIF                                                  SCISDT NE 0
     C*
     C* START DATE CANNOT BE GREATER THAN END DATE
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
     C*
     C  N90XXEFDT        IFGT      XXEXDT
     C*
     C                   SETON                                        7590
     C*
     C                   ENDIF                                                  EFDT > EXDT
     C*
      **** BEGIN ADD **** 10/12/01 ************************************
     C*
     C* CARRIER POLICY REQUIRED, IF SELECTED IN GROUP CARRIER INFO
     C*
     C  N90*IN33         IFEQ      *ON
     C     SCCAP#        IFEQ      *BLANKS
     C                   SETON                                        3490
     C                   ENDIF                                                  SCCAP#
     C                   ENDIF                                                  *IN33
     C*
     C* CARRIER POLICY CANNOT BE DUPLICATE IF FLAG IS SET
     C*
     C  N90*IN35         IFEQ      *ON
     C                   MOVE      SCCAP#        XXCAP#
     C     RPKY2         CHAIN     PRP02                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        3690
     C                   ENDIF                                                  *IN60
     C                   ENDIF                                                  *IN33
      **** END   ADD **** 10/12/01 ************************************
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C***  BEGIN ADD  ***  09/29/98  *********************************
     C     *IN90         IFEQ      *OFF
     C     #OFSCR        ANDEQ     1
     C                   EXSR      NXTSCR
     C                   ENDIF                                                  *IN90=*OFF
     C***  END   ADD  ***  09/29/98  *********************************
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C***  BEGIN ADD  ***  09/29/98  *********************************
     C     *IN90         IFEQ      *OFF
     C     #OFSCR        ANDEQ     2
     C                   EXSR      NXTSCR
     C                   ENDIF                                                  *IN90=*OFF
     C***  END   ADD  ***  09/29/98  *********************************
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C***  BEGIN ADD  ***  09/29/98  *********************************
     C     *IN90         IFEQ      *OFF
     C     #OFSCR        ANDEQ     3
     C                   EXSR      NXTSCR
     C                   ENDIF                                                  *IN90=*OFF
     C***  END   ADD  ***  09/29/98  *********************************
     C*
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C     HFLD          IFEQ      'SCCARR'
     C*
     C                   CALL      'W2493R'
     C                   PARM                    ##DATA
     C                   PARM      '2'           ##MODE            1
     C                   PARM      'CRR  '       ##TBID
     C                   PARM      *BLANKS       ##KEY
     C                   PARM                    ##RTC             1
     C*
     C     ##KEY         IFNE      *BLANKS
     C*
     C                   MOVE      *ON           *IN32
     C                   MOVEL(P)  ##KEY         SCCARR
     C                   MOVEL     ##TBID        KKTBID
     C                   MOVEL(P)  ##KEY         KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              60
     C*
     C                   MOVEL(P)  GTDESC        SCDESC
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN31
     C*
     C                   ENDIF                                                  ##KEY NE ' 'KS
     C*
     C                   ENDIF                                                  HFLD SCCARR KS
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   MOVE      *BLANKS       SCCARR
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCCAP#
     C                   Z-ADD     0             SCISDT
     C                   MOVE      *BLANKS       SCCLT#
     C                   MOVE      *BLANKS       SCTPA#
     C                   MOVE      *BLANKS       SCBRCH
     C                   MOVE      *BLANKS       SCCNTR
     C                   Z-ADD     0             SCEFDT
     C                   Z-ADD     0             SCEXDT
     C*
     C                   ELSE
     C*
     C                   MOVEL     RPCARR        SCCARR
     C                   MOVEL     RPDESC        SCDESC
     C                   MOVEL     RPCAP#        SCCAP#
     C*
     C                   Z-ADD     RPISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCISDT
     C*
     C                   MOVEL     RPCLT#        SCCLT#
     C                   MOVEL     RPTPA#        SCTPA#
     C                   MOVEL     RPBRCH        SCBRCH
     C                   MOVEL     RPCNTR        SCCNTR
     C*
     C                   Z-ADD     RPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     RPEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  09/29/98  *********************************
     C                   CLEAR                   SCIND
     C                   CLEAR                   SCTAX#
     C                   CLEAR                   SCZERO
     C*
     C                   SELECT
     C     SCCARR        WHENEQ    'AIG'
     C                   Z-ADD     RP60S1        SCIND
     C                   MOVEL     RP30A2        SCTAX#
     C                   MOVEL     RP30A3        SCZERO
     C***  BEGIN ADD  ***  12/14/98  *********************************
     C                   MOVEL     RP30A1        SCPRFX
     C                   Z-ADD     RP60S2        SCIDNT
     C***  END   ADD  ***  12/14/98  *********************************
     C                   ENDSL
     C***  END   ADD  ***  09/29/98  *********************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      ELCO#         RPCO#
     C                   MOVE      ELFND         RPFND
     C                   MOVE      ELEMP#        RPEMP#
     C                   MOVE      ELDIV         RPDIV
     C                   MOVE      ELFYR         RPFYR
     C                   MOVEL     SCCARR        RPCARR
     C                   MOVEL     SCDESC        RPDESC
     C                   MOVEL     SCCAP#        RPCAP#
     C*
     C                   Z-ADD     SCISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPISDT
     C*
     C                   MOVEL     SCCLT#        RPCLT#
     C                   MOVEL     SCTPA#        RPTPA#
     C                   MOVEL     SCBRCH        RPBRCH
     C                   MOVEL     SCCNTR        RPCNTR
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPEFDT
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPEXDT
     C*
     C***  END   ADD  ***  09/29/98  *********************************
     C                   SELECT
     C     SCCARR        WHENEQ    'AIG'
     C                   Z-ADD     SCIND         RP60S1
     C                   MOVEL     SCTAX#        RP30A2
     C                   MOVEL     SCZERO        RP30A3
     C***  BEGIN ADD  ***  12/14/98  *********************************
     C                   MOVEL     SCPRFX        RP30A1
     C                   Z-ADD     SCIDNT        RP60S2
     C***  END   ADD  ***  12/14/98  *********************************
     C                   ENDSL
     C***  END   ADD  ***  09/29/98  *********************************
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*===============================================================
     C* NXTSCR - SUBROUTINE THAT SELECTS PGM BY CARRIER CODE TO
     C*          GO TO UNIQUE CARRIER SCREEN.
     C*===============================================================
     C*
     C     NXTSCR        BEGSR
     C*
     C***  BEGIN ADD  ***  09/29/98  *********************************
     C     @@MODE        CASEQ     'C'           UPDATR
     C***  BEGIN CHG  ***  06/10/99  *********************************
     C********** @@MODE    CASEQ'D'       DSPLYR
     C     @@MODE        CASEQ     'D'           DSPLY2
     C***  END   CHG  ***  06/10/99  *********************************
     C                   CAS                     OTHERR
     C                   ENDCS
     C*
     C                   EXSR      REFRS2
     C*
B01  C     *IN90         DOUEQ     *OFF
     C***  END   ADD  ***  09/29/98  *********************************
     C*
     C                   SELECT
     C*
     C     SCCARR        WHENEQ    'AIG'
     C***  BEGIN ADD  ***  09/29/98  *********************************
     C                   EXFMT     SCRAIG
     C***  END   ADD  ***  09/29/98  *********************************
     C     SCCARR        WHENEQ    'CLR'
     C*
     C*
     C     SCCARR        WHENEQ    'EVR'
     C*
     C*
     C     SCCARR        WHENEQ    'LGN'
     C*
     C*
     C     SCCARR        WHENEQ    'REL'
     C*
     C*
     C     SCCARR        WHENEQ    'ZUR'
     C*
     C*
     C                   ENDSL
     C*
     C***  BEGIN ADD  ***  09/29/98  *********************************
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
     C*
B02  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C     *INKE         CASEQ     *ON           KEDO
     C                   CAS                     OTHFKS
     C                   ENDCS
     C*
B03  C     *IN90         IFEQ      *OFF
     C     @@MODE        ANDNE     'D'
     C*
     C     SCCARR        CASEQ     'AIG'         ERRAIG
     C                   ENDCS
     C*
E03  C                   ENDIF                                                  *IN90=*OFF
     C*
B03  C     *IN90         IFEQ      *OFF
     C***  BEGIN CHG  ***  06/10/99  *********************************
     C**********           EXSR FINISH
     C*
     C     @@MODE        CASEQ     'A'           WRITER
     C     @@MODE        CASEQ     'C'           UPDATR
     C     @@MODE        CASEQ     'D'           DSPLY2
     C     @@MODE        CASEQ     'P'           WRITER
     C                   CAS                     OTHERR
     C                   ENDCS
     C*
B01  C     @@MODE        IFNE      'A'
     C                   MOVE      *ON           OUTFLG
E01  C                   ENDIF                                                  @@MODE<>A
     C***  END   CHG  ***  06/10/99  *********************************
     C*
E03  C                   ENDIF                                                  *IN90=*OFF
     C*
E02  C                   ENDIF                                                  *INKC=*OFF/*INKL=*OF
     C*
E01  C                   ENDDO                                                  *IN90=*OFF
     C*
B01  C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   EXSR      REFRS2
E01  C                   ENDIF
     C***  END   ADD  ***  09/29/98  *********************************
     C*
     C                   ENDSR                                                   NXTSCR
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C*
     C     *INKC         CASEQ     *ON           FINISH
     C     @@MODE        CASEQ     'N'           WRITER
     C                   ENDCS
     C*
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    INQUIRY
     C*
     C     @@RRN2        CHAIN     WDELP                              99
     C*
     C                   MOVE      ELFYR         SCYR
     C*
     C     EMKY          CHAIN     PEM01                              99
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
      ***** BEG ADD ******* 05/14/01 **********************************
     C                   Z-ADD     *ZERO         SCCXDT
      ***** END ADD ******* 05/14/01 **********************************
     C*
     C                   DO        5             I1                3 0
     C*
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   ELSE
     C                   LEAVE
     C                   ENDIF                                                  CX,I1<>*ZERO
     C*
     C                   ENDDO                                                  I1->5
      *
      ***** BEG ADD ******* 05/14/01 **********************************
     C     SCCXDT        COMP      *ZERO                                  49
      ***** END ADD ******* 05/14/01 **********************************
     C*
     C*  GET GROUP INFORMATION
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   MOVE      FNFND         SCFND
     C*
     C                   MOVEL     FNNAME        SCNAME
     C*
     C     RPKY          CHAIN     PRP01                              99
     C*
     C  N99              Z-ADD     DBRRN         @@RRN             9 0
     C   99              Z-ADD     0             @@RRN             9 0
     C   99              CLEAR                   DBRCDF
     C*
     C* SET CORRECT DISPLAY MODE.
     C*
     C  N99
     CANN09              MOVE      'C'           @@MODE            1            CHG RECORD
     C  N99
     CANN09              SETOFF                                       40
     C***  BEGIN CHG  ***  09/29/98  *********************************
     C***99N09**           MOVE 'N'       @@MODE           NEW RECORD
     C   99
     CANN09              MOVE      'A'           @@MODE                         NEW RECORD
     C***  END   CHG  ***  09/29/98  *********************************
     C   99
     CANN09              SETON                                        40
     C*
     C     @@CALL        IFEQ      '1'
     C*
     C                   MOVE      'D'           @@MODE                         DISPLAY
     C*
     C                   ENDIF                                                  @@CALL EQ '1'
      **** BEGIN ADD **** 10/12/01 ************************************
     C     GCKY          CHAIN     PGC01                              60
     C     *IN60         IFEQ      *OFF
      *
     C     GCCRFL        IFEQ      'Y'
     C                   SETON                                        33
     C                   ENDIF                                                  GCCRFL = Y
      *
     C     GCDCFL        IFEQ      'Y'
     C                   SETON                                        35
     C                   ENDIF                                                  GCCRFL = Y
      *
     C                   ENDIF                                                  *IN60
      **** END   ADD **** 10/12/01 ************************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C*
     C     @@MODE        CASEQ     'N'           PREADD
     C                   ENDCS
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C***  BEGIN ADD  ***  09/29/98  *********************************
     C     *IN99         IFEQ      *OFF
     C***  END   ADD  ***  09/29/98  *********************************
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   MOVEL     DSTIME        RPTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C***  BEGIN ADD  ***  09/29/98  *********************************
     C                   ENDIF                                                  *IN99=*OFF
     C***  END   ADD  ***  09/29/98  *********************************
     C***  BEGIN DEL  ***  09/29/98  *********************************
     C**********           EXSR NXTSCR
     C***  END   DEL  ***  09/29/98  *********************************
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        RPIUSR
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPIPGM
     C                   MOVEL     Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPIDT
     C                   MOVEL     SRDTO         RPDT
     C                   MOVEL     DSTIME        RPITM
     C                   MOVEL     DSTIME        RPTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMRPP
     C*
     C***  BEGIN DEL  ***  09/29/98  *********************************
     C**********           EXSR NXTSCR
     C***  END   DEL  ***  09/29/98  *********************************
     C***  BEGIN ADD  ***  09/29/98  *********************************
     C                   MOVE      'C'           @@MODE
     C***  END   ADD  ***  09/29/98  *********************************
     C*
     C                   ENDSR                                                  WRITER
