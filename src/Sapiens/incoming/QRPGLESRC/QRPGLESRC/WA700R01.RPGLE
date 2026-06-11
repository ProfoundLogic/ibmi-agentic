     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE WA700R01 - BATCH MONITOR FOR AUTO-DARTS REMARKS
     H*****************************************************************
     H*  Description:
     H*         This program runs daily during batch processing.
     H*         It writes Auto DARTS remarks for payments.
     H*
     H* *****************************************************************
     H*  Called From:                                                   *
     H*    BATCH                                                        *
     H*  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     H*  Calls To:                                                      *
     H*    DA700R - Write DARTS API                                     *
     H*                                                                 *
     H* *****************************************************************
     H*  Created:   3/03/05  C:58  Project:  WC1128                     *
     H* *****************************************************************
     H** REVISIONS:                                                     *
     H*
     H*  12/19/18  C:062 Ticket: 75632(114)
     H*                 Expand the index size again.
     H*                                                                 *
     H* 04/04/17 C:062 Support: 46005(136)                              *
     H*                Process multiple groups                          *
     H*                                                                 *
     H* 02/12/15 C:062 Support: 33194 (clt129)                          *
     H*                Prevent infinite loops.                          *
     H*                                                                 *
     H* 12/08/10 C:136 SUPPORT: WA139 75136                             *
     H*                EXPAND SIZE OF ARRAY COUNTER                     *
     H*                                                                 *
     H*                                                                 *
     H* 05/27/10  C:220  Project: 25WQO- Additional changes           *
     H*                  New Auto diary features                      *
     H*                  - Hard delete the auto Darts for CLMRETEN    *
     H*                                                               *
     F* 02/01/10  C:247  Project: 25WQO:                              *
     F*                  New Auto diary features                      *
     H*                                                               *
     H* ****************************************************************
     H*  Indicators:
     H*
     H*    45 - LOKUP
     H*    46 - LOKUP
     H*
     H*    50 - Read Payments                           WDPALH
     H*    51 - CHAIN/Read Auto-DARTS Control File      WMADL
     H*    52 - CHAIN User file                         WMUPL1
     H*    54 - CHAIN Claim Master                      WMCML1
     H*    55 - CHAIN/Read DARTS remarks                SDCMLU
     H*    56 - CHAIN/Read payments-count               WDPAL5
     H*
     H*    60 - General File Access
     H*
     H*    80 - This fund selected: has auto-DARTS control.
     H*   N80 - This fund not selected, no auto-DARTS control record.
     H*    81 -   Fund has auto-DARTS LOWRESV control.
     H*    82 -   Fund has auto-DARTS PAYMENT control.
     H* ****************************************************************
     F*
     F* ****************************************************************
     F*      F I L E S
     F* ****************************************************************
     F*   Payments    WDPALH
     FWDPALH    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F*
     F*   Payments - count # since remark          WDPAL5
     FWDPAL5    IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     F*
     F*   Payments - to refresh for DARTS creation
     FWDPAP     IF   E             DISK
     F                                     RENAME(PPA01:PPA03)
     F*
     F*   Auto DARTS Control File - WMADL3
     FWMADL3    IF   E           K DISK
     F*
     F*   Claim Master File  - WMCML1
     FWMCML1    IF   E           K DISK
     F*
     F*   DARTS remarks
     FSDCMLU    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F*
     F*   Remark table
     FSTCTL1    IF   E           K DISK
     F*
     F*   Users by staf code.
     F***WMUPL1  IF  E           K        DISK
     F*
     F******** BEG ADD ********  02/01/10 *************************
     FWDF1L     IF   E           K DISK
     F******** END ADD ********  02/01/10 *************************

     D* -----------------------------------------------------------
     D*    A r r a y s
     D* -----------------------------------------------------------
     D*    Codes and Data from LOWRSV controls, this group.
     D L@              S              7    DIM(999)                             Rsv + RMK     S/AU
     D LA              S              9  0 DIM(999)                             Amounts       S/AU
     D LC              S              3  0 DIM(999)                             Counts        S/AU
     D LF              S              1    DIM(999)                             Low done flagsS/AU
     D*
     D*    Codes and Data from PAYMENT controls, this group.
     D P@              S              9    DIM(999)                             Rsv+RMK+BR2   S/AU
     D PA              S              9  0 DIM(999)                             Amounts       S/AU
     D PC              S              3  0 DIM(999)                             Counts        S/AU
     D PF              S              1    DIM(999)                             Lrg done flagsS/AU
     D*
     D*   Claim master reserve data
     D*
     D*****               ST          8  3               Staf codes DIUS/AU
     D CI              S              1    DIM(8)                               Claim indic. codes
     D******* BEG ADD ************ 02/01/10 ***********************
     D LP              S              2    DIM(999)                             BR2 CODES
     D LW              S              3  0 DIM(999)                             BR2 CODES
     D LR              S              5  2 DIM(999)                             BR2 CODES
     D LT              S             11  2 DIM(999)                             BR2 CODES
     D******* BEG CHG ************ 05/27/10 ***********************
     D*******             CD2       100  2               BR2 CODES
     D*******             WK#       100  3 0             BR2 # OF WKS
     D*******             DA#       100  1 0             BR2 # OF DYS
     D*** Begin Chg *** 12/19/18 **********************************
     D*CD2******       S              2    DIM(9999)                            BR2 CODES
     D*WK#******       S              3  0 DIM(9999)                            BR2 # OF WKS
     D*DA#******       S              1  0 DIM(9999)                            BR2 # OF DYS
     D CD2             S              2    DIM(99999)                           BR2 CODES
     D WK#             S              3  0 DIM(99999)                           BR2 # OF WKS
     D DA#             S              1  0 DIM(99999)                           BR2 # OF DYS
     D AM2             S             11  2 DIM(99999)                           BR2 CODE AMT
     D*** End   Chg *** 12/19/18 **********************************
     D******* END CHG ************ 05/27/10 ***********************
     D WKC             S              3    DIM(100)                             BR2 # OF WKS
     D DAC             S              1    DIM(100)                             BR2 # OF DYS
     D******* BEG CHG ************ 05/27/10 ***********************
     D*******             AM2       100 11 2             BR2 CODE AMT
     D*** Begin Del *** 12/19/18 **********************************
     D*AM2******       S             11  2 DIM(9999)                            BR2 CODE AMT
     D*** End   Del *** 12/19/18 **********************************
     D******* END CHG ************ 05/27/10 ***********************
     D******* END ADD ************ 02/01/10 ***********************
     D*============================================================
     D*    D A T A
     D*============================================================
     D*    DARTS key for claim.
     D                 DS                  INZ
     D  DSKEY                  1     40
     D  KKCO#                  1      3  0
     D  PAFND                  4      6  0
     D  PAFYR                  7      9  0
     D  PACASE                10     16  0
     D*
     D*    Combine Reserve code and Remark code.
     D                 DS                  INZ
     D  RSVRMK                 1      7
     D  ADRECD                 1      2
     D  ADRMCD                 3      7
     D*
     D*    Claim master reserve codes.
     D                 DS                  INZ
     D  RT                     1     20
     D                                     DIM(10)                              Reserve codes
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D*
     D*    Claim master remaining reserves.
     D                 DS                  INZ
     D  RR                     1     90  2
     D                                     DIM(10)                              Remain reserve
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*    System date and time.
     D                 DS                  INZ
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*============================================================
     C*    M  A  I  N        L  O  G  I  C
     C*============================================================
     C*    Read payments.
     C     PAKEY0        CHAIN     PPA01                              50
     C     *IN50         DOWEQ     *OFF
     C*
     C******* BEG ADD ********* 02/01/10 ******************************
     C                   EXSR      SUMCLP
     C                   EXSR      FUNRET
     C                   EXSR      SETNDR
     C******* END ADD ********* 02/01/10 ******************************
     C*    Check for different fund.
     C     SAVFND        IFNE      PAFND
     C                   EXSR      SETFND                                       Fnd setup
     C                   Z-ADD     PAFND         SAVFND
     C                   EXSR      GETCLM                                       Clm setup
     C                   ENDIF                                                  eSAVFND <>
     C*
     C*    Check for new claim.
     C     SVFYR         IFNE      PAFYR
     C     SVCASE        ORNE      PACASE
     C                   EXSR      GETCLM                                       Clm setup
     C                   ENDIF                                                  eSVFYR <>
     C*
     C*    Process only if control(s) exist and claim master found.
     C     *IN80         IFEQ      *ON                                          some control
     C     *IN54         ANDEQ     *OFF                                         have claim
     C*
     C                   Z-ADD     0             R
     C     R             DOUGT     999
     C                   ADD       1             R
     C*
     C     R             IFEQ      0
     C                   LEAVE
     C                   ENDIF
     C*
     C*    1.  End of low array entries, quit process.
     C                   MOVEL     L@(R)         WKRECD
     C     WKRECD        CABEQ     '00'          ENDLO
     C*
     C*    2.  Check for low reserve.
     C     CMSTAT        IFEQ      'O'
     C     WKRECD        ANDEQ     *BLANK
     C     CMSTAT        OREQ      'O'
     C     WKRECD        ANDEQ     PARECD
     C                   EXSR      LOWRSV
     C                   ENDIF                                                  e STAT=A...
     C     ENDLO         TAG
     C*
     C*    3.  End of large payment array data, quit process.
     C                   MOVEL     P@(R)         WKRECD
     C     WKRECD        CABEQ     '00'          ENDLGP
     C                   MOVE      P@(R)         WKBR2
     C*
     C*    4.  Check for large payment.
     C     WKRECD        IFEQ      *BLANK
     C     WKBR2         ANDEQ     *BLANK
     C     WKRECD        OREQ      PARECD
     C     WKBR2         ANDEQ     *BLANK
     C     WKRECD        OREQ      PARECD
     C     WKBR2         ANDEQ     PABR2
     C     PAAMT         IFGE      PA(R)
     C                   EXSR      LRGPMT
     C                   ENDIF                                                  e AMT>=PA,R
     C                   ENDIF                                                  e RECD=' '..
     C     ENDLGP        TAG
     C********* BEG ADD ******* 02/01/10 ******************************
      * 5. Check value for '#WEEKS '
     C*    End of array data, quit process.
     C                   MOVEL     LW(R)         WK#WKS            3 0
     C     WK#WKS        CABEQ     *ZEROS        END#WK
     C                   MOVEL(P)  '#WEEKS  '    XXKEY
     C                   EXSR      DRAPI
      *
     C     END#WK        TAG
      *
      * 7. Check value for 'PAYMENT2'
     C*    End of array data, quit process.
     C                   MOVEL     LT(R)         TOTA#            11 2
     C     TOTA#         CABEQ     *ZEROS        ENDAMT
     C                   Z-ADD     LT(R)         TOTA#
     C                   MOVEL(P)  'PAYMENT2'    XXKEY
     C                   EXSR      DRAPI
      *
     C     ENDAMT        TAG
     C********* END ADD ******* 02/01/10 ******************************
     C                   ENDDO                                                  e DO 999
     C*
     C                   ENDIF                                                  e IN80 ON...
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C*    Next payment today.
     C*** Begin Chg *** 04/04/17 ********************************************
     C*****PAKEY         READE     PPA01                                  50
     C     PAKEY0        READE     PPA01                                  50
     C*** End   Chg *** 04/04/17 ********************************************
     C                   ENDDO                                                  e DOW 50 ON
     C*
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*    End.
     C     ENDAP         TAG
     C*******              DIV  S         Q       30
     C                   SETON                                        LR
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C*================================================================
     C*   S  U  B  R  O  U  T  I  N  E  S
     C*================================================================
     C*
     C*================================================================
     C*    LOWRSV  Low reserve processing.
     C*================================================================
     C     LOWRSV        BEGSR
     C*
     C*     Get reserve array location
     C                   Z-ADD     1             S                 3 0
     C                   MOVE      *OFF          *IN46
     C     WKRECD        LOOKUP    RT(S)                                  46
     C     *IN46         IFEQ      *ON                                          Found
     C*
     C*     Skip if this reserve was already done.
     C     LF(R)         CABEQ     'Y'           ENDLR
     C*
     C*    Check remaining reserve.
     C     RR(S)         IFLE      LA(R)
     C*
     C*    Get Remark Table record.
     C                   MOVE(P)   L@(R)         KKRMCD
     C     RMKEY         CHAIN     PCT01                              60
     C     *IN60         CABEQ     *ON           ENDLR
     C     CTSTAT        CABEQ     'I'           ENDLR
     C*
     C*    See if this remark is there already.
     C                   Z-ADD     *HIVAL        KKSEQ#
     C     DARKY         SETGT     PCM02
     C     DARKY2        READPE    PCM02                                  55
     C*
     C*    It is: count payments since last DARTS was created.
     C     *IN55         IFEQ      *OFF
     C                   Z-ADD     0             Z                 3 0
     C                   Z-ADD     CMTRDT        XXTRDT
     C     PAKY2         CHAIN     PPA02                              56
     C     *IN56         DOWEQ     *OFF
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C                   ADD       1             Z
     C                   ENDIF                                                   TYPE = P...
     C     PAKY3         READE     PPA02                                  56
     C                   ENDDO                                                      DOW IN56 OFF
     C                   ENDIF                                                      e 55 OFF
     C*
     C     Z             IFGE      LC(R)                                         Enough
     C     LC(R)         OREQ      *ZERO                                         Don't count
     C     *IN55         OREQ      *ON                                           None yet
     C*
     C*      Now write the remark
     C                   MOVEL(P)  'LOWRSV'      XXKEY
     C                   EXSR      DRAPI
     C                   MOVE      'Y'           LF(R)
     C                   ENDIF                                                      Z > LC,R
     C*
     C                   ENDIF                                                      e RR,S<=
     C                   ENDIF                                                      e 46 ON
     C*
     C     ENDLR         TAG
     C                   ENDSR
     C*
     C*================================================================
     C*    LRGPMT - Large payment processing.
     C*================================================================
     C     LRGPMT        BEGSR
     C*
     C*     Save payment.
     C                   Z-ADD     DBRRN         XXRRN             9 0
     C*
     C*     Skip if this remark was already done.
     C     PF(R)         CABEQ     'Y'           ENDLP
     C*
     C*    Get Remark Table record.
     C                   MOVE(P)   P@(R)         WK7               7
     C                   MOVEL(P)  WK7           KKRMCD
     C     RMKEY         CHAIN     PCT01                              60
     C     *IN60         CABEQ     *ON           ENDLP
     C     CTSTAT        CABEQ     'I'           ENDLP
     C*
     C*    See if this remark is there already.
     C                   Z-ADD     *HIVAL        KKSEQ#
     C     DARKY         SETGT     PCM02
     C     DARKY2        READPE    PCM02                                  55
     C*
     C*    It is: count payments since last DARTS was created.
     C     *IN55         IFEQ      *OFF
     C                   Z-ADD     1             Z                 3 0
     C                   Z-ADD     CMTRDT        XXTRDT
     C     PAKY2         CHAIN     PPA02                              56
     C     *IN56         DOWEQ     *OFF
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C                   ADD       1             Z
     C                   ENDIF                                                   TYPE = P...
     C     PAKY3         READE     PPA02                                  56
     C                   ENDDO                                                      DOW IN56 OFF
     C                   ENDIF                                                   e IN55 OFF
     C*
     C*      Now write the remark
     C     *IN55         IFEQ      *ON                                           None yet
     C     Z             ORGE      PC(R)                                         Enough
     C     PC(R)         OREQ      *ZERO                                         Don't count
     C*
     C     XXRRN         CHAIN     PPA03                              49        Refresh data
     C                   MOVEL(P)  'PAYMENT'     XXKEY
     C                   EXSR      DRAPI
     C                   MOVE      'Y'           PF(R)
     C                   ENDIF                                                      Z > PC,R
     C*
     C     ENDLP         TAG
     C                   ENDSR
     C*
     C*================================================================
     C*    SETFND  Fund setup subroutine.
     C*================================================================
     C     SETFND        BEGSR
     C                   Z-ADD     PAFND         KKFND                            Set key
     C                   MOVE      *ALL'0'       L@                               Clear
     C                   Z-ADD     0             LA                                Zero
     C                   Z-ADD     0             LC                                  out
     C                   MOVE      *ALL'0'       P@                               Clear
     C                   Z-ADD     0             PA                                Zero
     C                   Z-ADD     0             PC                                  out
     C                   MOVEA     '000'         *IN(80)                          Unset
     C                   Z-ADD     1             R                 3 0            Start IX
     C*
     C*    Get 'LOWRSV' control record(s).
     C                   MOVEL(P)  'LOWRSV'      KKKEY
     C     ACKEY         CHAIN     PAD01                              51
     C     *IN51         CABEQ     *ON           LARGP
     C                   MOVE      *ON           *IN(81)
     C*
     C*    Process all 'LOWRSV' key for this fund.
     C     *IN51         DOWEQ     *OFF
     C*** Begin Chg *** 02/12/15 ************************************
     C*****ADSTAT        IFEQ      'I'
     C**********         ITER
     C**********         ENDIF                                                  e ADSTAT=I
     C     ADSTAT        IFNE      'I'
     C*** End   Chg *** 02/12/15 ************************************
     C                   MOVEL(P)  RSVRMK        L@(R)                             -RSV+RMK
     C                   Z-ADD     ADAMT         LA(R)                             -amount
     C                   Z-ADD     ADCNT         LC(R)                             -number
     C                   ADD       1             R                                Bump
     C*** Begin Add *** 02/12/15 ************************************
     C                   ENDIF
     C*** End   Add *** 02/12/15 ************************************
     C     ACKEY         READE     PAD01                                  51     Next record
     C                   ENDDO                                                  e 51 DOW=OFF
     C*
     C* -----------------------------------------------------------------
     C*    Get 'LARGEPMT' control record(s).
     C     LARGP         TAG
     C                   MOVEL(P)  'PAYMENT '    KKKEY
     C     ACKEY         CHAIN     PAD01                              51
     C     *IN51         CABEQ     *ON           ENDFND
     C                   MOVE      *ON           *IN(82)
     C*
     C*    Process all 'LARGEMPMT key for this fund.
MF   C                   Z-ADD     1             R
     C*
     C     *IN51         DOWEQ     *OFF
     C*** Begin Chg *** 02/12/15 ************************************
     C*****ADSTAT        IFEQ      'I'
     C**********         ITER
     C**********         ENDIF                                                  e ADSTAT=I
     C     ADSTAT        IFNE      'I'
     C*** End   Chg *** 02/12/15 ************************************
     C                   MOVEL(P)  RSVRMK        P@(R)                             -RSV+RMK
     C                   MOVE      ADBR2         P@(R)                             -RSV+RMK
     C                   Z-ADD     ADAMT         PA(R)                             -amount
     C                   Z-ADD     ADCNT         PC(R)                             -number
     C                   ADD       1             R                                Bump
     C*** Begin Add *** 02/12/15 ************************************
     C                   ENDIF
     C*** End   Add *** 02/12/15 ************************************
     C     ACKEY         READE     PAD01                                  51     Next record
     C                   ENDDO                                                  e 51 DOW=OFF
     C*
     C*** Begin Add *** 04/04/17 ***************************
     C     ENDFND        TAG
     C*** End   Add *** 04/04/17 ***************************
     C*
     C     *IN81         IFEQ      *ON
     C     *IN82         OREQ      *ON
     C                   MOVE      *ON           *IN80
     C                   ENDIF                                                  e 81+82 ON
     C*
     C*** Begin Del *** 04/04/17 ***************************
     C*****ENDFND        TAG
     C*** End   Del *** 04/04/17 ***************************
     C                   ENDSR
     C***** BEG ADD ********** 02/01/10 *******************************
     C*================================================================
     C* CHECK NEW DIARY CONDITIONS
     C*================================================================
     C     SETNDR        BEGSR
     C                   Z-ADD     PAFND         KKFND                            Set key
     C*    Get '#WEEKS  ' control record(s).
     C     #WEEKS        TAG
     C                   MOVEL(P)  '#WEEKS  '    KKKEY
      *** BEG CHG ***** 12/08/10 **********************************
     C***********          Z-ADD1         X       30
     C                   Z-ADD     1             X                 5 0
      *** END CHG ***** 12/08/10 **********************************
     C     ACKEY         CHAIN     PAD01                              51
     C****** BEG CHG ***** 05/27/10 ******************************
     C******     *IN51     CABEQ*ON       GRPRET
     C     *IN51         CABEQ     *ON           TOTPAD
     C****** END CHG ***** 05/27/10 ******************************
      *
     C                   Z-ADD     1             R
     C     *IN51         DOWEQ     *OFF
     C     ADSTAT        IFNE      'I'
     C     ADWKPD        IFNE      *ZEROS
     C                   Z-ADD     *ZEROS        #TWKPD            3 0
     C                   Z-ADD     1             X
     C     CD2(X)        DOWNE     *BLANKS
     C     CD2(X)        IFEQ      ADBR2A
     C     CD2(X)        OREQ      ADBR2B
     C     CD2(X)        OREQ      ADBR2C
     C     CD2(X)        OREQ      ADBR2D
     C                   MOVE      *ZEROS        #NWK2
     C                   MOVE      WK#(X)        #NWK2             3 0
     C                   ADD       #NWK2         #TWKPD
     C                   ENDIF
     C                   ADD       1             X
     C                   ENDDO
      *
     C     #TWKPD        IFGE      ADWKPD
     C                   MOVE      #TWKPD        LW(R)                             -RSV+RMK
     C                   ADD       1             R                                Bump
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
     C     ACKEY         READE     PAD01                                  51     Next record
     C                   ENDDO                                                  e 51 DOW=OFF
      *
     C*    Get 'PAYMENT2' control record(s).
     C     TOTPAD        TAG
     C                   MOVEL(P)  'PAYMENT2'    KKKEY
     C     ACKEY         CHAIN     PAD01                              51
     C     *IN51         CABEQ     *ON           ENDNDR
      *
     C                   Z-ADD     1             R
     C     *IN51         DOWEQ     *OFF
     C     ADSTAT        IFNE      'I'
     C                   Z-ADD     1             X
     C                   Z-ADD     *ZEROS        #TAMT2
     C     CD2(X)        DOWNE     *BLANKS
     C     CD2(X)        IFEQ      ADBR2
     C     CD2(X)        OREQ      ADBR2A
     C     CD2(X)        OREQ      ADBR2B
     C     CD2(X)        OREQ      ADBR2C
     C     CD2(X)        OREQ      ADBR2D
     C                   MOVE      *ZEROS        #AMT2            11 2
     C                   MOVE      AM2(X)        #AMT2
     C                   ADD       #AMT2         #TAMT2           11 2
     C                   ENDIF
     C                   ADD       1             X
     C                   ENDDO
      *
     C     #TAMT2        IFGE      ADTOTP
     C                   MOVE      #TAMT2        LT(R)
     C                   ENDIF
      *
     C                   ADD       1             R                                Bump
     C                   ENDIF
     C     ACKEY         READE     PAD01                                  51     Next record
     C                   ENDDO                                                  e 51 DOW=OFF
     C     ENDNDR        TAG
     C                   ENDSR
     C***** END ADD ********** 02/01/10 *******************************
     C*================================================================
     C*    GETCLM  Claim process.
     C*================================================================
     C     GETCLM        BEGSR
     C     CLMKEY        CHAIN     PCM01                              54
     C     *IN54         CABEQ     *ON           ENDCLM
     C                   Z-ADD     PACO#         SVCO#
     C                   Z-ADD     PAFND         SVFND
     C                   Z-ADD     PAFYR         SVFYR
     C                   Z-ADD     PACASE        SVFND
     C                   MOVE      *BLANKS       LF                              Clear flags
     C                   MOVE      *BLANKS       PF                              Clear flags
     C*
     C     ENDCLM        TAG
     C                   ENDSR
     C*================================================================
     C*    DRAPI - CALL DARTS API DA700R for Low Reserve
     C*================================================================
     C     DRAPI         BEGSR
     C*******
     C*******    Call API to write a DARTS Remark
     C*******    ------------------------------------------------------
     C*******              MOVELDSKEY     @@KEY  40 P        1
     C*******              MOVELCTDESC    @@COMM256 P
     C*******
     C***~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
     C******Diary data for diary type remarks
     C*******    CTFL01    IFEQ 'Y'
     C*******              EXSR GETDAT
     C*******    @@CDAT    CABEQ*ZERO     ENDAPI
     C*******    SRERR     CABEQ'1'       ENDAPI
     C*          ------------------------------------------------------
     C********
     C********             MOVEL*BLANKS   @@USER 10 P        7
     C********   ADUSR     IFNE *BLANKS
     C********             MOVELADUSR     @@USER    P        7
     C********             ELSE                            x ADUSR<> ' '
     C********             EXSR GETUSR
     C********   @@USER    CABEQ*BLANK    ENDAPI
     C********             ENDIF                           e ADUSR<> ' '
     C********             ENDIF                           e FL01=Y
     C*  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
     C*******
     C*******              MOVELCTDEPG    @@DCID 12 P        4
     C*******              MOVEL'W0070R'  @@PNAM 10 P        5
     C*******              MOVE ADSLVL    @@SECL  3          6
     C*******              MOVELQ1PGM     @@PGM  10 P        8
     C*******    CTDEPG    IFNE *BLANKS
     C*******              MOVELDSKEY     @@FOLD 80 P        9
     C*******              MOVE 'Y'       @@FL01  1 P       12
     C*******              ELSE                            x DEPG NE ' '
     C*******              MOVE 'N'       @@FL01            12
     C*******              MOVEL*BLANKS   @@FOLD    P
     C*******              ENDIF                           e DEPG NE ' '
     C*******              MOVELCTFL01    @@DIFL  1 P       10
     C*******              MOVELQ1PGM     @@IPGM 10 P       11
     C*******    CTDPRG    IFGT 0
     C*******              MOVE 'Y'       @@FL02  1 P       13
     C*******              ELSE                            DPRG > 0
     C*******              MOVE 'N'       @@FL02            13
     C*******              ENDIF                           DPRG > 0
     C*******              MOVELADRMCD    @@RMCD  5 P       14
     C*******              MOVE '00000'   @@SEQ#  5         15
     C*******    ADSLVL    IFGT 0
     C*******              MOVEL'Y'       @@SBYL  1 P       16
     C*******              ELSE                            X-SLVL > 0
     C*******              MOVE *BLANKS   @@SBYL    P       16
     C*******              ENDIF                           SLVL <= 0
     C*          ------------------------------------------------------
     C                   CALL      'WA700R'
     C                   PARM      CMCO#         @@CO#             3 0
     C                   PARM      CMFND         @@FND             3 0
     C                   PARM      CMFYR         @@FYR             3 0
     C                   PARM      CMCASE        @@CASE            7 0
     C                   PARM      XXKEY         @@DKEY           10
     C                   PARM      *BLANKS       @@COMM          256
     C*          ------------------------------------------------------
     C     ENDAPI        TAG
     C                   ENDSR
     C*
     C***** BEG  ADD ********* 02/01/10 *******************************
     C*================================================================
     C     SUMCLP        BEGSR
     C                   MOVEL     KKCO#         QQCO#
     C                   MOVEL     PAFND         QQFND
     C                   MOVEL     PAFYR         QQFYR
     C                   MOVEL     PACASE        QQCASE
     C                   MOVEA     *BLANKS       CD2
     C                   MOVEA     *ZEROS        WK#
     C                   MOVEA     *ZEROS        DA#
     C                   CALL      'WA576R5'
     C                   PARM      *BLANKS       QQRTCD            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM      *BLANKS       QQTRCD            2
     C                   PARM      *BLANKS       QQTYPE            1
     C                   PARM      *BLANKS       QQRECD            2
     C                   PARM      *BLANKS       QQRETP            1
     C                   PARM                    CD2
     C                   PARM                    WK#
     C                   PARM                    DA#
     C                   PARM      *ZEROS        QQTAMT           11 2
     C                   PARM      *ZEROS        QQTRSV           11 2
     C                   PARM                    AM2
     C                   ENDSR
     C*================================================================
     C* TO CALCULATE THE FUND RETENTION PERCENTAGE FROM WDF1P
     C     FUNRET        BEGSR
     C     FNKY          CHAIN     PF101                              75
     C     *IN75         IFEQ      *OFF
     C                   Z-ADD     F1EMRT        FNRET#           11 2
     C     FNRET#        IFNE      *ZEROS
     C     QQTRSV        DIV       FNRET#        RETPR#            5 2
     C     RETPR#        MULT      100           RETPR#
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C***** END  ADD ********* 02/01/10 *******************************
     C*================================================================
     C*    GETDAT - Calculate CDAT for remark/diary
     C*================================================================
     C****
     C****       GETDAT    BEGSR
     C****                 SELEC
     C****       ADDIDT    WHEQ '1'
     C****                 MOVE DSDATE    SRDATE  80
     C****       ADDIDT    WHEQ '2'
     C****                 Z-ADDCMRCVD    SRDTI
     C****                 CALL 'SRCVTDAT'DTMDYY
     C****                 MOVELSRDTO     SRDATE
     C****       ADDIDT    WHEQ '3'
     C****                 Z-ADDCMACDT    SRDTI
     C****                 CALL 'SRCVTDAT'DTMDYY
     C****                 MOVELSRDTO     SRDATE
     C****       ADDIDT    WHEQ '4'
     C****                 Z-ADDCMDTRP    SRDTI
     C****                 CALL 'SRCVTDAT'DTMDYY
     C****                 MOVELSRDTO     SRDATE
     C****                 ENDSL
     C****
     C****
     C****                 Z-ADDADDIDY    SRDAYS  50
     C****                 MOVE *BLANKS   SRERR   1
     C****                 CALL 'SRFUTR8'
     C****                 PARM           SRDATE
     C****                 PARM           SRDAYS
     C****                 PARM           SRERR
     C****
     C****                 MOVELSRDATE    @@CDAT  8 P
     C****
     C****                 ENDSR
     C*================================================================
     C*    GETUSR - Determine user for remark/diary
     C*================================================================
     C****       GETUSR    BEGSR
     C****                 MOVE *OFF      *IN52
     C****
     C**** First try DIUS
     C****       ADDIUS    IFGE '01'                        numeric ?
     C****       ADDIUS    ANDLE'08'                        numeric ?
     C****                 MOVE ADDIUS    I       20         index
     C****                 MOVE ST,I      KKSTAF    P
     C****       KKSTAF    CHAINPUP01                52
     C****       *IN52     IFEQ *OFF
     C****                 MOVELUPUSR     @@USER    P
     C****                 ENDIF                            e 52 OFF
     C****                 ENDIF                            e DIUS>01
     C****
     C**** OK now try DIAU
     C****       @@USER    IFEQ *BLANK
     C****                 MOVE *OFF      *IN52
     C****       ADDIAU    IFGE '01'                        numeric ?
     C****       ADDIAU    ANDLE'08'                        numeric ?
     C****                 MOVE ADDIAU    I       20         index
     C****                 MOVE ST,I      KKSTAF    P
     C****       KKSTAF    CHAINPUP01                52
     C****       *IN52     IFEQ *OFF
     C****                 MOVELUPUSR     @@USER    P
     C****                 ENDIF                            e 52 OFF
     C****                 ENDIF                            e DIAU>01 ...
     C****                 ENDIF                            e USER=' '
     C****                 ENDSR
     C*================================================================
     C*    *INZSR - INITIALIZATION SUBROUTINE
     C*================================================================
     C     *INZSR        BEGSR
     C*
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C*       PARAMETER LISTS
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C     *ENTRY        PLIST
     C                   PARM                    ##CO#             3            01
     C                   PARM                    ##DATE            8            02
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C*       K  E  Y      L  I  S  T  S
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C*   KEY LISTS FOR PAYMENTS
     C     PAKEY0        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTRDT
     C*
     C     PAKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTRDT
     C                   KFLD                    KKFND
     C*
     C     PAKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    XXTRDT
     C*
     C     PAKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*
     C*   KEY LIST FOR THE REMARK TYPE TABLE FILE
     C     RMKEY         KLIST
     C                   KFLD                    KKRMCD
     C*
     C*   KEY LIST FOR AUTO DARTS CONTROL    WMADL
     C     ACKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKKEY
     C*
     C*   KEY LIST FOR DARTS MASTER - SDCMLU
     C     DARKY         KLIST
     C                   KFLD                    DSKEY
     C                   KFLD                    KKRMCD
     C                   KFLD                    KKSEQ#
     C*
     C     DARKY2        KLIST
     C                   KFLD                    DSKEY
     C                   KFLD                    KKRMCD
     C*
     C*   KEY LIST FOR CLAIM MASTER - WMCML1
     C     CLMKEY        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*
     C****** BEG ADD ************ 02/01/10 ****************************
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C****** END ADD ************ 02/01/10 ****************************
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C*       DATA DEFINITIONS
     C* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C     *LIKE         DEFINE    PADT          XXDT
     C     *LIKE         DEFINE    CMSEQ#        KKSEQ#
     C     *LIKE         DEFINE    PAFND         KKFND
     C     *LIKE         DEFINE    PAFND         SAVFND
     C     *LIKE         DEFINE    PATRDT        KKTRDT
     C     *LIKE         DEFINE    PATRDT        XXTRDT
     C     *LIKE         DEFINE    ADKEY         XXKEY
     C     *LIKE         DEFINE    PARECD        WKRECD
     C     *LIKE         DEFINE    PABR2         WKBR2
     C     *LIKE         DEFINE    PACO#         SVCO#
     C     *LIKE         DEFINE    PAFND         SVFND
     C     *LIKE         DEFINE    PAFYR         SVFYR
     C     *LIKE         DEFINE    PACASE        SVCASE
     C     *LIKE         DEFINE    ADRMCD        WKRMCD
     C     *LIKE         DEFINE    ADKEY         KKKEY
     C     *LIKE         DEFINE    ADRMCD        KKRMCD
     C****       *LIKE     DEFN UPSTAF    KKSTAF
     C*
     C* GET TODAY'S DATE
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   MOVE      ##DATE        XXDATE            8 0
     C*
     C* GET JULIAN DATE TO READ PAYMENTS
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKTRDT
     C*
     C*    Set up key to read payments.
     C                   MOVE      ##CO#         KKCO#
     C                   Z-ADD     0             KKFND
     C*
     C* --------------------------------------------------------
     C                   ENDSR
     C*================================================================
