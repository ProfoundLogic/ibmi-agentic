     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller)
     H/TITLE W0070R7 - WITNESS MAINT/INQ
     H Bnddir('WTBNDDIR')
     H* ***************************************************************
     H* DESCRIPTION:  THIS PROGRAM ALLOWS THE USER TO INSERT/\PDATE   *
     H*               WITNESS INFORMATION.                            *
     H*                                                               *
     H* CALLED FROM:  W0070R07 via F8                                 *
     H* CALLS TO:     NONE                                            *
     H* CREATED BY:   185                                             *
     H* PROJECT:      WC1177               05/01/07                   *
     H* ***************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 05/25/12 C:282 Project: WC2109                                *
     H*                Added Claim Contacts email field             . *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  *IN09  -  INDICATES MODE (MAINT/INQ)                         *
     H*  *IN23  -  AMOUNT CANNOT BE LESS THAN ZERO                    *
     H*  *IN28  -  POSITION CURSOR ON BODY PART CODE                  *
     H*  *IN29  -  POSITION CURSOR ON PERCENTAGE                      *
     H*  *IN40  -  INDICATES WHETHER TO WRITE OR UPDATE RECORD        *
     H*  *IN60  -  GENERAL USAGE (CHAIN, READ, ETC..)                 *
     H*  *IN69  -  EXCESS REINSURANCE
     H*  *IN71-6   POSITION CURSOR
     H* ***************************************************************
     FW0070FA   CF   E             WORKSTN
     F                                     INFDS(FIDS01)
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMWIL     UF A E           K DISK
     D ONM             S             40    DIM(5)
     D OPH             S             15  0 DIM(5)
     D PRDS            S             40    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      * WITNESS NAME
     D                 DS
     D  NM                     1    200
     D                                     DIM(5)
     D  SCNM01                 1     40
     D  SCNM02                41     80
     D  SCNM03                81    120
     D  SCNM04               121    160
     D  SCNM05               161    200
      * BUSINESS PHONE NUMBER
     D                 DS
     D  PH                     1     75  0
     D                                     DIM(5)
     D  SCPH11                 1      3  0
     D  SCPH21                 4      6  0
     D  SCPH31                 7     10  0
     D  SCPH41                11     15  0
     D  SCPH12                16     18  0
     D  SCPH22                19     21  0
     D  SCPH32                22     25  0
     D  SCPH42                26     30  0
     D  SCPH13                31     33  0
     D  SCPH23                34     36  0
     D  SCPH33                37     40  0
     D  SCPH43                41     45  0
     D  SCPH14                46     48  0
     D  SCPH24                49     51  0
     D  SCPH34                52     55  0
     D  SCPH44                56     60  0
     D  SCPH15                61     63  0
     D  SCPH25                64     66  0
     D  SCPH35                67     70  0
     D  SCPH45                71     75  0
      *
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
      *
     D XCEXRE          C                   CONST('EXCS REIN ')
     D MODEC           C                   CONST('- Change -')
     D MODED           C                   CONST('-Display- ')
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS           DS
     D  DBRRN                397    400B 0
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     C*
     C*  KEY LIST FOR THE WITNESS FILE
     C*
     C     WIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     WIKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXNAME
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    WINAME        XXNAME
     C*
     C* *****************************************************************
     C*  *ENTRY PARAMETERS FOR PROGRAM
     C* *****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQRTCD            1
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
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVEL(P)  MODEC         SCMODE
     C     QQCALL        COMP      '1'                                    09
     C  N09              MOVE      PRDS(1)       SCPRDS
     C   09              MOVE      PRDS(2)       SCPRDS
     C   09              MOVEL(P)  MODED         SCMODE
     C*
     C*  RETRIEVE THE USER PARAMETER, COMPANY AND FUND RECORDS
     C*
     C                   Z-ADD     0             XXCO#
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     UPCO#         XXCO#
     C                   ENDIF
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     XXFND         SCFND
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C*  RETRIEVE THE CLAIMANT INFORMATION
     C*
      *  FYR-CASE
     C                   MOVE(P)   XXFYR         XAFYR             3
     C                   MOVE(P)   XXCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCL
     C     SCYRCL        CAT       XACASE:0      SCYRCL
      *
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     DBRRN         XXRRN             9 0
     C                   ENDIF
     C*
      *  CLAIMANT SS#
     C     CMSS#         IFNE      *BLANK
      *****  Begin Chg  ***  06/09/20  **********************
     C*****SS1           CAT(P)    '-':0         SCSS#
     C*****SCSS#         CAT       SS2:0         SCSS#
     C*****SCSS#         CAT       '-':0         SCSS#
     C*****SCSS#         CAT       SS3:0         SCSS#
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-Free
      *****  End Chg  ***  06/09/20  **********************
     C                   ENDIF
      *
      *  GET EXCESS REINSURANCE INFO
      *
     C     CMEXCS        IFNE      'Y'
     C     XXRRN         IFNE      *ZERO
     C                   CALL      'WA010R'
     C                   PARM                    XXRRN                          FOR WMCMP
     C                   PARM                    XXFNDR           13 2
     C                   PARM                    XXEXRE            1
     C                   ENDIF
     C                   ENDIF
      *
     C     XXEXRE        IFEQ      'Y'
     C     CMEXCS        OREQ      'Y'
     C                   MOVEL(P)  XCEXRE        SCMSG1
     C                   MOVE      *ON           *IN69
     C                   ENDIF
      *
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    CMEMP#
     C                   PARM                    CMDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
     C                   ENDIF
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVEL     CMACL#        SCSCL#
      *
      *  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
      *
     C     CMCVT#        IFNE      *BLANK
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C                   ENDIF
     C*
     C*  RETRIEVE THE CLAIMANT NAME
     C*
     C     CDKY          CHAIN     PCD01                              60
     C                   MOVE      *BLANKS       SCCNAM
     C                   MOVEL     CDLSN         SCCNAM
     C     CDLSN         IFNE      *BLANKS
     C     SCCNAM        CAT       ',':0         SCCNAM
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C                   ELSE
     C                   MOVEL     CDFSN         SCCNAM
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C                   ENDIF
     C*
     C                   EXSR      CLR
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     WIKY          CHAIN(N)  PWI01                              60
     C*
     C                   EXSR      LOAD
     C*
     C*  POSITION CURSOR ON FIRST BLANK CODE SPACE
     C*
     C                   DO        5             Y                 1 0
     C     NM(Y)         IFEQ      *BLANK
     C     55            ADD       Y             Y1                3 0
     C                   MOVE      '1'           *IN(Y1)
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     C*
     C*   PROCESS ANY CHANGES
     C*
     C     *INKL         IFEQ      *OFF
     C                   EXSR      WRITER
     C                   ENDIF
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F12 = CANCEL
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      'B'           QQRTCD
     C                   ENDIF
     C                   ENDIF                                                  *IN11 PROCESSING
     C*
     C*  END THE PROGRAM
     C*
     C                   MOVE      'B'           QQRTCD
     C*
     C                   SETON                                        LR
     C**************************************************************************
     C*  CLEAR THE SCREEN ARRAYS
     C*****************************************************************
     C     CLR           BEGSR
     C*
     C                   MOVE      *BLANKS       NM
     C                   MOVE      *ZEROS        PH
     C                   MOVE      *BLANKS       ONM
     C                   MOVE      *ZEROS        OPH
     C                   Z-ADD     *ZERO         I1                3 0          ARRAY INDEX
     C*
     C                   ENDSR
     C**************************************************************************
     C*  FILL SCREEN ARRAYS
     C*****************************************************************
     C     FILL          BEGSR
     C*
     C  N60              MOVEL     WINAME        NM(I1)
     C  N60              Z-ADD     WIBPH#        PH(I1)
     C                   MOVE      NM(I1)        ONM(I1)
     C                   MOVE      PH(I1)        OPH(I1)
     C     I1            ADD       40            X1
     C                   MOVE      '1'           *IN(X1)
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       scEmad = WIemad;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C*
     C                   ENDSR
     C**************************************************************************
     C*  LOAD FORWARD SUBROUTINE
     C*****************************************************************
     C     LOAD          BEGSR
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVER          TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     WIKY          READE(N)  PWI01                                  60
     C     I1            COMP      5                                    61
     C   61              GOTO      OVER
     C     DONE          TAG
     C*
     C                   ENDSR
     C**************************************************************
     C*  WRITER  SUBROUTINE - UPDATE RECORDS                       *
     C**************************************************************
     C     WRITER        BEGSR
     C*
     C                   DO        5             X                 3 0
     C     X             ADD       30            X1                2 0          ERROR
     C*
     C*   COMPARE HOLD FIELDS FROM ORIGINAL SCREEN TO THE CURRENT
     C*   FIELDS. IF THE FIELDS CHANGED, EDIT & UPDATE OR WRITE.
     C*
     C     NM(X)         IFNE      ONM(X)
     C     PH(X)         ORNE      OPH(X)
     C*
     C*  WRITE A NEW RECORD OR UPDATE AN EXISTING RECORD.
     C*
     C     ONM(X)        IFEQ      *BLANKS
     C*
     C                   Z-ADD     XXCO#         WICO#
     C                   Z-ADD     XXFND         WIFND
     C                   Z-ADD     XXFYR         WIFYR
     C                   Z-ADD     XXCASE        WICASE
     C                   MOVEL     NM(X)         WINAME
     C                   Z-ADD     PH(X)         WIBPH#
     C*
     C                   MOVE      Q1USER        WIUSER
     C                   MOVE      Q1PGM         WIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WIDT
     C*
     C                   Z-ADD     DSTIME        WITIME
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       WIemad = scEmad;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C*
     C                   WRITE     PWI01
     C*
     C                   ELSE
     C*
     C     WIKY          CHAIN     PWI01                              40
     C                   MOVEL     NM(X)         WINAME
     C                   Z-ADD     PH(X)         WIBPH#
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       WIemad = scEmad;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C*
     C                   UPDATE    PWI01
     C                   ENDIF
     C*
     C*  MOVE NEW FIELDS TO OLD FIELDS
     C*
     C                   MOVE      NM(X)         ONM(X)
     C                   Z-ADD     PH(X)         OPH(X)
     C*
     C                   END
     C                   END
     C*
     C                   ENDSR
     C**************************************************************************
** PGM DESC
        WITNESS MAINTENANCE
          WITNESS INQUIRY
