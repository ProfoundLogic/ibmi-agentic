     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H actgrp(*caller)
     H/TITLE W0070R32 - EDI R3 Additional Info
     H* ***************************************************************
     H* DESCRIPTION:  THIS PROGRAM ALLOWS THE USER TO ENTER           *
     H*               EDI Weekly Amounts Info                         *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H* CALLS TO:     NONE                                            *
     H* CREATED BY:   191                                             *
     H* PROJECT:      WC1584               10/19/07                   *
     H* ***************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  06/09/20 C:318 Project: SY420
     H*                 Mask SSN
     H*
     H* 12/13/11  C:189 SUPPORT 78563                                 *
     H*                 Display the option irrespective of the        *
     H*                 EDI flag                                      *
     H*                                                               *
     H* 09/01/10  C:246 SUPPORT 74036                                 *
     H*                 Remove editable option in Inq Mode            *
     H*                                                               *
     H* 04/29/09  C:180     SUPPORT 68059 - EP claims
     h*
     h* 07/30/08  C:180     Update date/timestamp fields.
     h*
     H*                                                               *
     H* ***************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  *IN09  -  INDICATES MODE (MAINT/INQ)                         *
     H*  *IN61-69  Position Cursor
     H*  *IN71-89  ERROR INDICATORS                                   *
     H* ***************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0070F3   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWDA3L     UF A E           K DISK
     FWTGTL     IF   E           K DISK
     F*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D PRDS            S             40    DIM(1) CTDATA
      *
     D XXDONE          S               N
     D XXERROR         S               N
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                        6  0 OVERLAY(DSDTTM)
     D  DSDATE                        8  0 OVERLAY(DSDTTM:7)
      *
     D                 DS
     D  XXSS#                  1      9
     D  XXSS1                  1      3
     D  XXSS2                  4      5
     D  XXSS3                  6      9
     D  XXRRN          S                   LIKE(HRRN)
     C*Clear Subfile
     C                   EXSR      SRCLRSF
     C*Load Subfile
     C                   EXSR      SRLODSF
     C*Main Program Loop
     C                   DOW       NOT *INKC AND NOT *INKL
     C*Display Subfile
     C                   EXSR      SRDSPSF
     C                   SELECT
     C                   WHEN      *INKC
     C                   EVAL      @@RTCD = 'A'
     C                   WHEN      *INKL
     C                   EVAL      @@RTCD = 'B'
     C                   WHEN      *INKJ
     C                   EXSR      SRKJDO
     C                   OTHER
     C*Error subfile
     C                   EXSR      SRERRCK
     C                   IF        not  xxerror
     C*If not error then
     C*Update database
     C                   EXSR      SRUPDAT
     C                   ENDIF
     C*If process done then exit program
     C                   IF        xxdone
     C                   EVAL      @@RTCD = 'B'
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDSL
     C                   ENDDO
     C*
     C                   EVAL      *INLR=*ON
     C**************************************************************************
     C*  SRLODSF - Load Subfile using any data available in WDA3P
     C**************************************************************************
     C     SRLODSF       BEGSR
      ***        Write the subfile records, using any information
      ***        available in file
      *
     C     A3KY          CHAIN     PA301                              60
     C                   DOW       NOT *IN60
     C                   CLEAR                   SCR01S
      *
      *
     C                   EVAL      SCPMCH = A3BTC
     C                   EVAL      SCPMCD = A3BTC
     C*** BEGIN CHANGE ****** 04/29/09 *************************************
     C***                EVAL      SCGWA  = A3GWA
     C***                Z-ADD     A3GWD         SRDTI
     C                   EVAL      SCPAID = A3PAID
     C                   Z-ADD     A3STDT        SRDTI
     C*** END CHANGE ******** 04/29/09 *************************************
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C*** BEGIN CHANGE ****** 04/29/09 *************************************
     C***                MOVEL(P)  SRDTO         SCGWD
     C***                Z-ADD     A3PID         SRDTI
     C                   MOVEL(P)  SRDTO         SCSTDT
     C                   Z-ADD     A3ENDT        SRDTI
     C*** END CHANGE ******** 04/29/09 *************************************
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C*** BEGIN CHANGE ****** 04/29/09 *************************************
     C****               MOVEL(P)  SRDTO         SCISDT
     C                   MOVEL(P)  SRDTO         SCENDT
     C*** END CHANGE ******** 04/29/09 *************************************
      *
     C                   EVAL      RRN2 = RRN2 + 1
     C                   WRITE     SCR01S                                 95
     C                   IF        *IN95 = *ON OR RRN2 = 10
     C                   LEAVE
     C                   ENDIF
      *
     C     A3KY          READE     PA301                                  60
     C                   ENDDO
     C*** Initialize remaining records with blank values
     C                   DOW       RRN2 < 10
     C                   EVAL      SCPMCD = *BLANKS
     C                   EVAL      SCPMCH = *BLANKS
     C*** BEGIN CHANGE ****** 04/29/09 *************************************
     C***                EVAL      SCGWA  = *ZEROS
     C***                EVAL      SCGWD  = *ZEROS
     C***                EVAL      SCISDT = *ZEROS
     C                   EVAL      SCPAID = *ZEROS
     C                   EVAL      SCSTDT = *ZEROS
     C                   EVAL      SCENDT = *ZEROS
     C*** END CHANGE ******** 04/29/09 *************************************
     C                   EVAL      RRN2 = RRN2 + 1
     C                   WRITE     SCR01S                                 95
     C                   ENDDO
     C*
     C                   ENDSR
     C**************************************************************************
     C*  SRUPDAT - Update Database file
     C**************************************************************************
     C     SRUPDAT       BEGSR
     C                   READC     SCR01S                                 62
     C     *IN62         DOWEQ     *OFF
      *
     C                   IF        SCPMCD <> *BLANKS
     C                   EVAL      XXBTC = SCPMCH
     C     A3KY1         CHAIN     PA301                              60
     C                   EVAL      A3CO# = XXCO#
     C                   EVAL      A3FND = XXFND
     C                   EVAL      A3FYR = XXFYR
     C                   EVAL      A3CASE = XXCASE
     C                   EVAL      A3BTC = SCPMCD
     C*** BEGIN CHANGE ****** 04/29/09 *************************************
     C***                EVAL      A3GWA = SCGWA
     C***                Z-ADD     SCGWD         SRDTI
     C                   EVAL      A3PAID= SCPAID
     C                   Z-ADD     SCSTDT        SRDTI
     C*** END CHANGE ******** 04/29/09 *************************************
     C                   CALL      'SRCVTDAT'    DTYYMD
     C*** BEGIN CHANGE ****** 04/29/09 *************************************
     C****               MOVEL     SRDTO         A3GWD
     C****               Z-ADD     SCISDT        SRDTI
     C                   MOVEL     SRDTO         A3STDT
     C                   Z-ADD     SCENDT        SRDTI
     C*** END CHANGE ******** 04/29/09 *************************************
     C                   CALL      'SRCVTDAT'    DTYYMD
     C*** BEGIN CHANGE ****** 04/29/09 *************************************
     C*****              MOVEL     SRDTO         A3PID
     C                   MOVEL     SRDTO         A3ENDT
     C*** END CHANGE ******** 04/29/09 *************************************
      ***** BEGIN ADD *****  07/30/08 *********************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         A3DT
      *
     C                   MOVEL     DSTIME        A3TIME
     C                   MOVEL     Q1USER        A3USER
     C                   MOVEL     Q1PGM         A3PGM
      ***** END ADD *******  07/30/08 *********************************
     C                   IF        *IN60 = *OFF
     C                   UPDATE    PA301
     C                   ELSE
      ***** BEGIN ADD *****  07/30/08 *********************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         A3IDT
      *
     C                   MOVEL     DSTIME        A3ITIM
     C                   MOVEL     Q1USER        A3IUSR
     C                   MOVEL     Q1PGM         A3IPGM
      ***** END ADD *******  07/30/08 *********************************
     C                   WRITE     PA301
     C                   ENDIF
     C                   ENDIF                                                  SCPMTC<>*BLANKS
      *
      ******** BEGIN ADD ******* 04/29/09 *****************************
      *
      * Check for record to be deleted
      *
     C                   IF        SCPMCD = *BLANKS and SCPMCH <> *BLANKS and
     C                             SCSTDT = *ZEROS and SCENDT = *ZEROS and
     C                             SCPAID = *ZEROS
     C                   EVAL      XXBTC = SCPMCH
     C     A3KY1         CHAIN     PA301                              60
     C                   IF        *IN60 = *OFF
     C                   DELETE    PA301
     C                   MOVE      *BLANKS       SCPMCH
     C                   UPDATE    SCR01S
     C                   ENDIF
     C                   ENDIF
      ******** END ADD ********* 04/29/09 *****************************
      *
     C                   READC     SCR01S                                 62
     C                   ENDDO
     C                   EVAL      xxdone = *on
      *
     C                   ENDSR
     C*****************************************************************
     C*  SRERRCK - Error checking routine
     C*****************************************************************
     C     SRERRCK       BEGSR
     C                   EVAL      xxdone = *off
     C                   EVAL      xxerror = *off
     C                   READC     SCR01S                                 62
      *
     C     *IN62         DOWEQ     *OFF
     C* Check for duplicate Payment Code
     C                   IF        SCPMCD<>*BLANKS
     C                   IF        SCPMCD <> SCPMCH
     C                   EVAL      XXBTC = SCPMCD
     C     A3KY1         CHAIN     PA301                              60
     C                   IF        *IN60 = *OFF
     C                   SETON                                        21
     C                   EVAL      xxerror = *on
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCPMCD<>SCPMCH
     C*
     C*Pmt Code must be valid
     C                   CLEAR                   XXTKEY
     C                   MOVEL     'PADJ'        XXTBID
     C                   MOVEL     SCPMCD        XXTKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                          22
     C                   EVAL      xxerror = *on
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCPMCD<>*BLANKS
     C*
     C* If Payment code is blank then other values should be blanks
     C                   IF        SCPMCD = *BLANKS AND
     C*** BEGIN CHANGE ****** 04/29/09 *************************************
     C                             (SCPAID<> *ZERO OR SCSTDT<> *ZERO OR
     C                              SCENDT <> *ZERO)
     C****                         (SCGWA <> *ZERO OR SCGWD <> *ZERO OR
     C****                          SCISDT <> *ZERO)
     C*** END CHANGE ******** 04/29/09 *************************************
     C                   SETON                                          25
     C                   EVAL      xxerror = *on
     C                   ENDIF                                                  SCPMCD = ' '
     C*
     C*** BEGIN CHANGE ****** 04/29/09 *************************************
     C**** Check for Gross weekly Amount Effective Date
     C****                   IF        SCGWD <> *ZERO
     C****                   Z-ADD     SCGWD         SRDTI
     C* Check for Start date
     C                   IF        SCSTDT<> *ZERO
     C                   Z-ADD     SCSTDT        SRDTI
     C*** END CHANGE ******** 04/29/09 *************************************
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   IF        SRERR = '1'
     C                   SETON                                          23
     C                   EVAL      xxerror = *on
     C                   ENDIF
     C                   ENDIF
     C*
     C*** BEGIN CHANGE ****** 04/29/09 *************************************
     C*****ck for Payment Issue date
     C****               IF        SCISDT <> *ZERO
     C****               Z-ADD     SCISDT        SRDTI
     C* Check for END DATE
     C                   IF        SCENDT <> *ZERO
     C                   Z-ADD     SCENDT        SRDTI
     C*** END CHANGE ******** 04/29/09 *************************************
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   IF        SRERR = '1'
     C                   SETON                                          24
     C                   EVAL      xxerror = *on
     C                   ENDIF
     C                   ENDIF
     C*
     C* Seton Next change indicator to mark record as changed for next
     C                   SETON                                        05
     C                   UPDATE    SCR01S
     C                   READC     SCR01S                                 62
     C                   ENDDO
     C*
     C                   ENDSR
     C**************************************************************************
     C*  SRDSPSF - Display Subfile
     C**************************************************************************
     C     SRDSPSF       BEGSR
     C     SCRRN3        IFEQ      0
     C                   Z-ADD     1             SCRRN3
     C                   ENDIF
     C     HRRN          IFEQ      0
     C                   Z-ADD     1             HRRN
     C                   ENDIF
     C                   WRITE     SCR01
     C                   MOVE      *ON           *IN95
     C                   EXFMT     SCR01C
     C                   ENDSR
     C**************************************************************************
     C*  SRKJDO - Table Search
     C**************************************************************************
     C     SRKJDO        BEGSR
      *
     C     XXRRN         IFNE      *ZEROS
     C     XXRRN         CHAIN     SCR01S
     C                   EVAL      *IN72 = *OFF
     C                   EVAL      *IN71 = *OFF
     C                   UPDATE    SCR01S
     C                   ENDIF
      *
     C     HRRN          CHAIN     SCR01S                             60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *BLANKS       ##TKEY
     C                   MOVE      *BLANKS       ##TBID
     C                   MOVEL     'PADJ'        ##TBID
     C                   CALL      'W2493R'
     C                   PARM      SCPRDS        ##PRDS           40
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##TBID            4
     C                   PARM                    ##TKEY           10
     C                   PARM      *BLANKS       ##RTCD            1
      *
     C     ##RTCD        IFEQ      '*'
     C                   MOVEL     ##TKEY        SCPMCD
     C                   EVAL      *IN72 = *ON
     C                   ELSE
     C                   EVAL      *IN71 = *ON
     C                   ENDIF                                                  *IN60=*OFF
     C                   UPDATE    SCR01S
     C                   EVAL      XXRRN = HRRN
     C                   Z-ADD     HRRN          SCRRN3
     C                   ENDIF                                                  *IN60=*OFF
      *
     C                   ENDSR
     C**************************************************************************
     C*  SRCLRSF - Clear Subfile
     C**************************************************************************
     C     SRCLRSF       BEGSR
     C                   MOVE      *OFF          *IN01
     C                   MOVE      *ON           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      *ON           *IN01
     C                   MOVE      *OFF          *IN04
     C                   ENDSR
     C**************************************************************************
     C*  *INZSR - Initialization
     C**************************************************************************
     C     *INZSR        BEGSR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@CALL            1
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@RTCD            1
     C********BEG ADD ******** 09/01/10*******************************
     C                   PARM                    @@MODE            1
     C********END ADD ******** 09/01/10*******************************
     C*
     C* Parameter Lists
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *** Define Key Lists
      *
      ***        SMCOL - Company Master
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      ***        WMFNL - Fund Master
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      ***        WMUPL - User Parameter File
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      ***        WTABL - System Table File
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTKEY
      *
      ***        WMCML - Claim Master File
      *
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
     C     A3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     A3KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXBTC
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID            4
     C                   KFLD                    XXTKEY           10
      *
      ***** BEGIN ADD *****  07/30/08 *********************************
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      ***** END ADD *******  07/30/08 *********************************
      *
      *** Define work fields
      *
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    A3BTC         XXBTC
     C***
     C                   MOVE      *ZEROS        XXDATE            8 0
     C                   MOVE      *ZEROS        RRN2              5 0
     C********BEG ADD ******** 09/01/10*******************************
     C                   if        @@mode <> 'D'
     C                   Eval      *In35 = *ON
     C                   Eval      *In36 = *ON
     C                   Endif
     C********END ADD ******** 09/01/10*******************************
      *** Retrieve the User Parameter File
      *
     C     UPKY          CHAIN     PUP01                              60
     C     XXCO#         IFEQ      0
     C                   MOVE      UPCO#         XXCO#
     C                   ENDIF
      *
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@FYR         XXFYR
     C                   MOVE      @@CASE        XXCASE
      *
      *** Retrieve Company Record
      *
     C     COKY          CHAIN     PCO01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       CONAME
     C                   ENDIF
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
      *** Retrieve Fund Record
      *
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *ALL'*'       FNNAME
     C                   ENDIF
      *
     C*** BEG DEL ****** 12/13/11 *************************************
     C************       IF        FNEDI = 'Y'
     C************       EVAL      *INLR = *ON
     C************       RETURN
     C************       ENDIF
     C*** END DEL ****** 12/13/11 *************************************
     C                   MOVE      XXFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C***
      *** Initialize Screen Fields
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVE      Q1USER        SCUSER
     C                   IF        @@PRDS = *BLANKS
     C                   MOVE      PRDS(1)       SCPRDS
     C                   ELSE
     C                   MOVE      @@PRDS        SCPRDS
     C                   ENDIF
      *
     C                   MOVEL     XXFYR         SCYRCL
     C                   MOVE      XXCASE        SCYRCL
     C* Get Claim master record
     C     CMKY          CHAIN     PCM01                              60
     C     CMSS#         IFNE      *BLANKS
     C                   MOVE      CMSS#         XXSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   MOVEL(P)  XXSS1         SCSS#
     C                   CAT       '-':0         SCSS#
     C                   CAT       XXSS2:0       SCSS#
     C                   CAT       '-':0         SCSS#
     C                   CAT       XXSS3:0       SCSS#
     C                   ENDIF
     C*
     C     CMLSN         IFNE      *BLANKS
     C                   MOVEL     CMLSN         SCCLNM
     C                   ENDIF
     C*
     C     CMFSN         IFNE      *BLANKS
     C                   CAT       ',':0         SCCLNM
     C                   CAT       CMFSN:1       SCCLNM
     C                   ENDIF
     C*
     C     CMMID         IFNE      *BLANKS
     C                   CAT       CMMID:1       SCCLNM
     C                   ENDIF
     C     CMACDT        IFNE      *ZEROS
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   ELSE
     C                   MOVE      *BLANK        SCACDT
     C                   ENDIF
     C                   ENDSR
** PGM DESC
    Employer-Paid Information
