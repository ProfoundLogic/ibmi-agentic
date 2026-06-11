     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**
     H/TITLE E8005R - EMP EXP MOD - CVT BSI-17 to EXCESS/NON-EXCESS
     H*               MASTER FILE
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**  THIS PROGRAM CONVERTS THE  BSI - 17   LOSS WORK FILE  TO   **
     H**  EXPERIENCE MOD MASTER FILES                                **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E8001C                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: Lori J. Quinn                                 **
     H**   CREATED:    03/04/91     PROJECT: EXP MOD                 **
     H**                                                             **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/21/01  C:136  PROJECT: WC577                               *
     H*                  Delete only the records in the workfile that *
     H*                  were written by the program.                 *
     H*                                                               *
     H* 10/10/98  C:58   PROJECT: WC853
     H*                  Set XXMULT and EEMULT if NCCI = '06'
     H*
     H* 08/31/98  C:03   PROJECT: WC853                               *
     H*                  SET XXMULT BEFOR CHAIN TO EMEEP              *
     H*                                                               *
     H* 08/25/98  C:35   PROJECT: WC853                               *
     H*                  UNIQUELY TOTAL CLAIMS WITH NCCI CODE = '06', *
     H*                  '6 ' OR ' 6'.                                *
     H*                                                               *
     H* 08/12/98  C:90   PROJECT: WC853                               *
     H*                  MODIFIED TO VERSION 3 STANDARDS              *
     H*                                                               *
     H*****************************************************************
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    EMEEL1(PEE01): FUND/BATCH                                **
     H**    EMEEL (PEE02): FUND/EMP#/BATCH/ACC DT/CASE #             **
     H**    EXB7P (PB701)                                            **
     H*****************************************************************
     H*
     F* PC BSI-17 Loss File
     F*
     FEMEEL1    UF   E           K DISK
     FEMEEL     UF A E           K DISK
     F                                     RENAME(PEE01:PEE02)
     F*
     F* BSI-17 Loss Work File
     F*
     FEXB7P     IF   E           K DISK
     F*

     D*
     D* Table for Batch Years to be Processed
     D BT              S              3  0 DIM(9)
     D*
     D* System Date and Time Data Structure
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* External Program Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     I*

     C*
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    EECO#         XXCO#
     C     *LIKE         DEFINE    EEFND         XXFND
     C     *LIKE         DEFINE    EEYEAR        XXYEAR
     C     *LIKE         DEFINE    EEEMP#        XXEMP#
     C     *LIKE         DEFINE    EEACDT        XXACDT
     C     *LIKE         DEFINE    EECASE        XXCASE
      **** BEGIN ADD **** 08/25/98 *********************************************
     C     *LIKE         DEFINE    EEMULT        XXMULT
      **** END   ADD **** 08/25/98 *********************************************
     C*
     C* Key List for the Excess/Non-Excess Master File
     C*
     C     EEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXYEAR
     C*
     C     EEKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXYEAR
     C                   KFLD                    XXACDT
     C                   KFLD                    XXCASE
      **** BEGIN ADD **** 08/25/98 *********************************************
     C                   KFLD                    XXMULT
      **** END   ADD **** 08/25/98 *********************************************
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQBAT1            3
     C                   PARM                    QQBAT2            3
     C                   PARM                    QQBAT3            3
     C                   PARM                    QQBAT4            3
     C                   PARM                    QQBAT5            3
     C                   PARM                    QQBAT6            3
     C                   PARM                    QQBAT7            3
     C                   PARM                    QQBAT8            3
     C                   PARM                    QQBAT9            3
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* Initialize and Set-up
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   Z-ADD     0             BT
     C                   MOVE      QQBAT1        BT(1)
     C                   MOVE      QQBAT2        BT(2)
     C                   MOVE      QQBAT3        BT(3)
     C                   MOVE      QQBAT4        BT(4)
     C                   MOVE      QQBAT5        BT(5)
     C                   MOVE      QQBAT6        BT(6)
     C                   MOVE      QQBAT7        BT(7)
     C                   MOVE      QQBAT8        BT(8)
     C                   MOVE      QQBAT9        BT(9)
     C*
     C* First delete all records in Excess/Non-Excess with same
     C* fund/batch year
     C*
     C                   EXSR      DELWK
     C*
     C* Begin Loop to Read All Records in Work File
     C*
     C                   READ      PB701                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C* Clear Excess/Non-Excess File Fields
     C*
     C                   EXSR      CLR
     C*
     C* Add Comp. and Med. Payments Together for Total Loss Amount
     C*
     C     B7MEDA        ADD       B7INDA        X1AMT
     C*
     C* Round Loss Amount into 9/0 Numeric Field
     C*
     C     X1AMT         MULT(H)   1             XXAMT
     C*
     C* Move Fields Across to PC file
     C*
     C* If the To Date Equals 0 then this is an Excess Claim
     C*
     C     B7EOPD        IFEQ      0
     C                   Z-ADD     B7FND         XXFND
     C                   Z-ADD     B7EMP#        XXEMP#
     C                   Z-ADD     B7FYR         XXYEAR
     C                   Z-ADD     B7ACCD        XXACDT
     C                   MOVE      *BLANKS       XXCASE
     C                   MOVEL     B7SS#         XXCASE
     C                   Z-ADD     XXCO#         EECO#
     C                   Z-ADD     B7FND         EEFND
     C                   Z-ADD     B7EMP#        EEEMP#
     C                   Z-ADD     B7FYR         EEYEAR
     C                   Z-ADD     B7ACCD        EEACDT
     C                   MOVEL     B7SS#         EECASE
     C     B7STAT        IFEQ      1
     C                   MOVE      'F'           EESTAT
     C                   ELSE
     C                   MOVE      'O'           EESTAT
     C                   END
     C                   MOVE      'N'           EEMULT
      **** BEGIN ADD **** 08/31/98 *********************************************
     C                   MOVE      'N'           XXMULT
      ****  END  ADD **** 08/31/98 *********************************************
     C**** BEGIN ADD **** 10/10/98 *********************************************
     C     B7NCCI        IFEQ      '06'
     C     B7NCCI        OREQ      ' 6'
     C     B7NCCI        OREQ      '6 '
     C                   MOVE      '6'           XXMULT
     C                   MOVE      '6'           EEMULT
     C                   ENDIF                                                          . NCCI = ...
     C****  END  ADD **** 10/10/98 *********************************************
     C                   Z-ADD     0             EETLCA
     C                   Z-ADD     XXAMT         EEAMT
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EEDT
     C*
     C                   Z-ADD     DSTIME        EETIME
     C                   MOVE      Q1USER        EEUSER
     C                   MOVE      Q1PGM         EEPGM
     C     EEKY1         CHAIN     PEE02                              50
     C     *IN50         IFEQ      '0'
     C                   UPDATE    PEE02
     C                   ELSE
     C*
     C                   MOVE      Q1USER        EEIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C                   MOVEL     SRDTO         EEIDT
     C                   WRITE     PEE02
     C                   END
     C*
     C* ELSE this is a Non-Excess Claim
     C*
     C                   ELSE
     C*
     C                   Z-ADD     B7FND         XXFND
     C                   Z-ADD     B7EMP#        XXEMP#
     C                   Z-ADD     B7FYR         XXYEAR
     C                   Z-ADD     0             XXACDT
     C                   MOVE      *BLANKS       XXCASE
     C                   Z-ADD     XXCO#         EECO#
     C                   Z-ADD     B7FND         EEFND
     C                   Z-ADD     B7EMP#        EEEMP#
     C                   Z-ADD     B7FYR         EEYEAR
     C                   Z-ADD     0             EEACDT
     C                   MOVE      *BLANKS       EECASE
     C                   MOVE      *BLANKS       EESTAT
     C                   MOVE      *BLANKS       EEMULT
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EEDT
     C*
     C                   Z-ADD     DSTIME        EETIME
     C                   MOVE      Q1USER        EEUSER
     C                   MOVE      Q1PGM         EEPGM
      **** BEGIN ADD **** 08/25/98 *********************************************
     C     B7NCCI        IFEQ      '06'
     C     B7NCCI        OREQ      '6 '
     C     B7NCCI        OREQ      ' 6'
     C                   MOVE      '6'           EEMULT
      **** BEGIN ADD **** 08/31/98 *********************************************
     C                   MOVE      '6'           XXMULT
      ****  END  ADD **** 08/31/98 *********************************************
     C                   ELSE
     C                   MOVE      *BLANK        EEMULT
      **** BEGIN ADD **** 08/31/98 *********************************************
     C                   MOVE      *BLANK        XXMULT
      ****  END  ADD **** 08/31/98 *********************************************
     C                   ENDIF                                                  B7NCCI IFEQ '06'
      **** END   ADD **** 08/25/98 *********************************************
     C     EEKY1         CHAIN     PEE02                              50
     C     *IN50         IFEQ      '0'
     C     EETLCA        ADD       B7#CLM        EETLCA
     C     EEAMT         ADD       XXAMT         EEAMT
     C                   UPDATE    PEE02
     C                   ELSE
     C                   Z-ADD     B7#CLM        EETLCA
     C                   Z-ADD     XXAMT         EEAMT
     C                   MOVE      Q1USER        EEIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EEIDT
     C*
     C                   WRITE     PEE02
     C                   END
     C*
     C                   END
     C*
     C* Get the Next Record
     C*
     C                   READ      PB701                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*****************************************************************
     C*          Subroutine to Clear Fields in PC Payroll File        *
     C*****************************************************************
     C*
     C     CLR           BEGSR
     C*
     C                   Z-ADD     0             EECO#
     C                   Z-ADD     0             EEFND
     C                   Z-ADD     0             EEEMP#
     C                   Z-ADD     0             EEYEAR
     C                   Z-ADD     0             EEACDT
     C                   MOVE      *BLANKS       EECASE
     C                   MOVE      *BLANKS       EESTAT
     C                   MOVE      *BLANKS       EEMULT
     C                   Z-ADD     0             EETLCA
     C                   Z-ADD     0             EEAMT
     C                   MOVE      *BLANKS       EEUSER
     C                   MOVE      *BLANKS       EEPGM
     C                   MOVE      *BLANKS       EEIUSR
     C                   Z-ADD     0             EETIME
     C                   Z-ADD     0             EEDT
     C                   Z-ADD     0             EEIDT
     C*
     C                   Z-ADD     0             XXAMT             9 0
     C                   Z-ADD     0             X1AMT            11 2
     C                   TIME                    DSDTTM
     C*
     C                   ENDSR
     C****************************************************************
     C*  DELWK - subroutine to delete loss file records
     C****************************************************************
     C*
     C* This subroutine will delete records from the work file for
     C* fund, batch year
     C*
     C     DELWK         BEGSR
     C*
     C                   Z-ADD     0             XXFND
     C                   MOVE      QQFND         XXFND
     C                   Z-ADD     1             X                 3 0
     C*
     C*  Loop thru and process all batch year <> 0
     C*
     C                   DO        9             X
     C                   Z-ADD     BT(X)         XXYEAR
     C     XXYEAR        IFNE      0
     C*
     C     EEKEY         CHAIN     PEE01                              60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD ******** 08/21/01 ****************************************
     C     EEPGM         IFEQ      'E8005R'
      *** END ADD ******** 08/21/01 ****************************************
     C                   DELETE    PEE01
      *** BEG ADD ******** 08/21/01 ****************************************
     C                   ENDIF                                                  EPPGM
      *** END ADD ******** 08/21/01 ****************************************
     C     EEKEY         READE     PEE01                                  60
     C                   END
     C*
     C                   END                                                    YR = 0
     C*
     C                   END                                                    END DO 9
     C*
     C                   ENDSR
