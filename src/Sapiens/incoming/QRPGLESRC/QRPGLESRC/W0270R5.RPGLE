     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*    TITLE W0270R5 - EDI LOG HISTORY
     H Bnddir('WTBNDDIR')
     H*
     H******************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM DISPLAYS EDI LOG HISTORY
     H*
     H* CALLED FROM: W0070R                                           *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: WC468
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H*  1/04/00 C:15 Project: Y2K                                    *
     H*               Change fields in pgm and on screen to show yr   *
     H*               2000 dates correctly.                           *
     H*                                                               *
     H*  9/16/99 C:58 Support MC087 sequence 19631 and 19662.
     H*               Change key save fields to numeric DS for subfile
     H*               proper loading and roll.
     H*
     H* 10/29/97 C:58 PROJECT: WC468
     H*    o  Add CO# and Fund to parameters and use to get record.
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     FW0270F5   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F*
     FWMLGL     UF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWTGTL     IF   E           K DISK

      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
      ***  End   Add  ***  06/09/20  ****************************
     D*
     D PRDS            S             40    DIM(2) CTDATA PERRCD(1)
      *
      *** Constants
      *
     D XCKDSC          C                   CONST('Table ID')
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*** BEGIN ADD *** 09/16/99 **************************************
     D*
     D*      Next and prior keys -- save fields
     D                 DS
     D  KEYN                   1     13  0
     D  KEYNDT                 1      7  0
     D  KEYNTM                 8     13  0
     D                 DS
     D  KEYP                   1     13  0
     D  KEYPDT                 1      7  0
     D  KEYPTM                 8     13  0
     D*** END ADD ***** 09/16/99 **************************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    LGFYR         XXFYR
     C     *LIKE         DEFINE    LGCASE        XXCASE
     C     *LIKE         DEFINE    LGIDT         XXIDT
     C     *LIKE         DEFINE    LGITM         XXITM
     C     *LIKE         DEFINE    UPFL2         SVFL2
     C     *LIKE         DEFINE    *IN60         RCD0
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXTBKY
     C*
     C*** BEGIN CHG *** 01/04/00 **************************************
     C******************** MOVE *ZEROS    XXDATE  60
     C                   MOVE      *ZEROS        XXDATE            7 0
     C*** END CHG ***** 01/04/00 **************************************
     C*
     C*   KEY LIST FOR EDI LOG FILE
     C*
     C     LGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXIDT
     C                   KFLD                    XXITM
     C     LGKEYP        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE CLAIM MASTER
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXTBKY
     C*
     C*   KEY LIST FOR THE CLAIMANT DEWMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              KFLD                    SCSS#
     C                   KFLD                    CMSS#
      *****  End Chg  ***  06/09/20  **********************
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFYR             3
     C*** BEGIN ADD *** 10/30/97 **************************************
     C                   PARM                    QQFND             3
     C                   PARM                    QQCO#             3
     C*** END ADD ***** 10/30/97 **************************************
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
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   Z-ADD     *ZEROS        XXIDT
     C                   Z-ADD     *ZEROS        XXITM
     C*
     C*  FORMAT THE YEAR AND CLAIM NUMBER INTO 1 FIELD (9.0)
     C*
     C*** BEGIN DLT *** 01/04/00 **************************************
     C******************** MOVE QQFYR     XXYR2   20
     C******************** MOVE QQCASE    SCYRCL
     C******************** MOVELXXYR2     SCYRCL
     C*** END DLT***** 01/04/00 **************************************
     C*** BEGIN ADD *** 01/04/00 **************************************
     C     QQFYR         CAT(P)    '-':0         SCYRCL
     C                   CAT       QQCASE:0      SCYRCL
     C*** END ADD ***** 01/04/00 **************************************
     C*
     C     TOP           TAG
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C     QQCALL        COMP      '1'                                    09
     C  N09              MOVE      PRDS(1)       SCPRDS
     C   09              MOVE      PRDS(2)       SCPRDS
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPFND         XXFND
     C                   MOVEL     UPUSNM        SCUSER
     C                   MOVE      *BLANKS       SVFL2
     C                   MOVEL     UPFL2         SVFL2
     C*** BEGIN ADD *** 10/30/97 **************************************
     C*
     C*  Use Co and Group passed from caller
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQCO#         UPCO#
     C                   MOVE      QQFND         UPFND
     C*** END ADD ***** 10/30/97 **************************************
     C*
     C*  GET THE COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE FUND NUMBER AND NAME
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      UPFND         SCFND
     C                   MOVE      FNNAME        SCFNME
     C*
     C*  GET THE CLAIMS MASTER (SOC SEC NUMBER)
     C*
     C     CMKEY         CHAIN     PCM01                              60
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVE      CMSS#         SCSS#
      /Free
       scss# = MaskData('SSN':'cmss#':cmss#);
      /End-free
      *****  End Chg  ***  06/09/20  **********************
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   MOVEL     CMACL#        SCACL#
     C*
     C*  GET THE CLAIMANT DEMOGRAPHICS FILE (NAME)
     C*
     C                   MOVE      ','           COMMA             1
     C                   MOVE      *BLANKS       WRK              50
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ALL'*'       SCCLNM
     C                   ELSE
     C     CDLSN         CAT       COMMA:0       WRK
     C     WRK           CAT       CDFSN:1       WRK
     C     WRK           CAT       CDMID:1       WRK
     C                   MOVEL     WRK           SCCLNM
     C                   END
     C*
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C*  INITIALIZE ROUTINE
     C*
     C                   EXSR      INIT
     C*
     C                   MOVE      *OFF          RCD0
     C     LGKEYP        SETLL     PLG01                                  91
     C     *IN91         IFEQ      *OFF
     C                   MOVE      *ON           RCD0
     C                   ELSE
     C                   Z-ADD     LGIDT         XXIDT
     C                   Z-ADD     LGITM         XXITM
     C                   END
     C*
     C* SET PAGE TO FORWARD
     C*
     C                   MOVE      'F'           @PGD
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
     C     ' '           CHECKR    Q2RECF        @P                5 0
     C     10            SUB       @P            @P
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C*
     C****************************************
     C*   F12 = Cancel                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F6 = RELEASE/HOLD EDI LOG RECORD   *
     C****************************************
     C     *INKF         IFEQ      '1'
     C*
     C*  DETERMINE WHICH RECORD THE CURSOR IS POSITIONED ON.
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       13            I1                3 0
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     8
     C*
     C*  GET THE SUBFILE RECORD FOR THE DATE AND TIME ENTRIES.
     C*
     C     I1            CHAIN     SCR01S                             60
     C     *IN60         IFEQ      '0'
     C*
     C*  GET THE LOG HISTORY RECORD AND UPDATE.
     C*
     C                   Z-ADD     SCIDT         XXIDT
     C                   Z-ADD     SCITM         XXITM
     C     LGKEY         CHAIN     PLG01                              50
     C     *IN50         IFEQ      '0'
     C*
     C     LGSTS         IFEQ      'EI'
     C     LGSTS         OREQ      'EC'
     C     LGSTS         OREQ      'EX'
     C*
     C     LGSTAT        IFEQ      'H'
     C                   MOVE      ' '           LGSTAT
     C                   ELSE
     C                   MOVE      'H'           LGSTAT
     C                   END
     C*
     C                   MOVE      Q1USER        LGUSER
     C                   MOVE      Q1PGM         LGPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDT
     C                   Z-ADD     DSTIME        LGTIME
     C                   UPDATE    PLG01
     C                   END                                                    LGSTS = EI OR EC
     C*
     C*  RE-DISPLAY THE SCREEN TO SHOW THE CHANGE
     C*
     C*** BEGIN CHG *** 09/16/99 **************************************
     C**********           MOVELKEYP      XXIDT
     C**********           MOVE KEYP      XXITM
     C                   Z-ADD     KEYPDT        XXIDT
     C                   Z-ADD     KEYPTM        XXITM
     C*** END CHANGE ** 09/16/99 **************************************
     C     LGKEY         SETLL     PLG01
     C                   MOVE      'F'           @PGD
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C*
     C                   END                                                    *IN50 = '0'
     C*
     C                   END                                                    *IN60 = '0'
     C*
     C                   END                                                    1 < I1 > 8
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKF
     C****************************************
     C*   *IN10 -  PROCESS HELP REQUESTS     *
     C****************************************
     C  N10
     CAN 10*IN10         IFEQ      '1'
     C  N10
     CAN 10              MOVE      '1'           *IN10
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C*
     C****************************************
     C*   *IN17 -  ROLLDOWN REQUEST          *
     C****************************************
     C     *IN17         IFEQ      '1'                                          ROLLDOWN
     C     KEYPF         IFEQ      *OFF                                         NO ROLL IF EOF
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF
     C*** BEGIN CHG *** 09/16/99 **************************************
     C**********           MOVELKEYP      XXIDT
     C**********           MOVE KEYP      XXITM
     C                   Z-ADD     KEYPDT        XXIDT
     C                   Z-ADD     KEYPTM        XXITM
     C*** END CHANGE ** 09/16/99 **************************************
     C     LGKEY         SETLL     PLG01
     C                   MOVE      'B'           @PGD
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN17 ROLLDOWN
     C*
     C*
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     KEYNF         IFEQ      *OFF                                         NO ROLL IF EOF
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF
     C*** BEGIN CHG *** 09/16/99 **************************************
     C**********           MOVELKEYN      XXIDT
     C**********           MOVE KEYN      XXITM
     C                   Z-ADD     KEYNDT        XXIDT
     C                   Z-ADD     KEYNTM        XXITM
     C*** END CHANGE ** 09/16/99 **************************************
     C     LGKEY         SETGT     PLG01
     C                   MOVE      'F'           @PGD
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C**************************************************************************
     C*
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C*
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*****************************************************************
     C* INIT SUBROUTINE                ** Subroutine **   I N I T     *
     C*****************************************************************
     C     INIT          BEGSR
     C                   Z-ADD     0             RRN2              5 0
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     *LIKE         DEFINE    *IN60         @LDF
     C                   MOVE      *OFF          @LDF
     C                   MOVE      *BLANK        @PGD              1
     C*
     C     *LIKE         DEFINE    *IN60         KEYPF
     C     *LIKE         DEFINE    *IN60         KEYNF
     C                   MOVE      *OFF          KEYPF
     C                   MOVE      *OFF          KEYNF
     C*
     C*
     C                   ENDSR
     C*****************************************************************
     C* LOAD SUBROUTINE                ** Subroutine **   L O A D     *
     C*****************************************************************
     C     LOAD          BEGSR
     C*
     C* IF NO LOG RECORDS, WRITE 1 BLANK SUBFILE RECORD
     C*
     C     RCD0          IFEQ      *ON
     C                   MOVE      *OFF          RCD0
     C                   MOVE      *ON           *IN60
     C                   Z-ADD     *ZEROS        RRN2
     C                   GOTO      NORCD
     C                   END
     C*
     C                   MOVE      *ON           @LDF
DO1  C     @LDF          DOWEQ     *ON
     C                   MOVE      *OFF          @LDF
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   Z-ADD     *ZEROS        KEYP
     C                   Z-ADD     *ZEROS        KEYN
     C                   MOVE      *OFF          KEYPF
     C                   MOVE      *OFF          KEYNF
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C     @PGD          IFEQ      'F'
     C                   MOVE      *ZEROS        RRN2
     C                   ELSE
     C                   Z-ADD     9             RRN2
     C                   ENDIF
     C*
     C*  LOAD THE SUBFILE SCREEN WITH UPTO 8 LINES
     C*
DO2  C                   DO        8
     C*
     C     @PGD          IFEQ      'F'
     C     LGKEYP        READE     PLG01                                  60
     C                   ELSE
     C     LGKEYP        READPE    PLG01                                  60
     C                   ENDIF
     C*
     C     *IN60         IFEQ      '1'
     C                   LEAVE
     C                   END
     C*
     C     @PGD          IFEQ      'F'
     C                   ADD       1             RRN2
     C                   ELSE
     C                   SUB       1             RRN2
     C                   ENDIF
     C* LOAD SUBFILE FIELDS FROM LOG FILE
     C                   MOVEL     LGTSET        SCTSET
     C                   MOVEL     LGMTC         SCMTC
     C*
     C* GET MAINT TYPE CODE DESCRIPTION - IF 148 TRANS
     C*
     C                   SELECT
     C     SCTSET        WHENEQ    '148'
     C                   MOVEL     'FMTC'        XXTBID
     C     SCTSET        WHENEQ    'A49'
     C                   MOVEL     'SMTC'        XXTBID
     C                   ENDSL
     C*
     C                   MOVE      *BLANKS       SCMTCD
     C                   MOVEL(P)  SCMTC         XXTBKY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        SCMTCD
     C*
     C                   MOVEL     LGSTS         SCSTS
     C                   MOVE      LGDUE         XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDUE
     C                   MOVE      LGEXDT        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C                   MOVE      LGSENT        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSENT
     C                   MOVE      LGARCV        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCARCV
     C                   MOVEL     LGSTAT        SCSTAT
     C                   Z-ADD     LGIDT         SCIDT
     C                   Z-ADD     LGITM         SCITM
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
     C*
     C     RRN2          IFEQ      1
     C*** BEGIN CHG *** 09/16/99 **************************************
     C**********           MOVELLGIDT     KEYP
     C**********           MOVE LGITM     KEYP
     C                   Z-ADD     LGIDT         KEYPDT
     C                   Z-ADD     LGITM         KEYPTM
     C*** END CHANGE ** 09/16/99 **************************************
     C                   Z-ADD     RRN2          #@$2
     C                   ENDIF
     C*
     C     RRN2          IFEQ      8
     C*** BEGIN CHG *** 09/16/99 **************************************
     C**********           MOVELLGIDT     KEYN
     C**********           MOVE LGITM     KEYN
     C                   Z-ADD     SCITM         KEYNTM
     C                   Z-ADD     SCIDT         KEYNDT
     C*** END CHANGE ** 09/16/99 **************************************
     C                   ENDIF
     C*
EDO2 C                   ENDDO
     C*
     C     @PGD          IFNE      'F'
     C     RRN2          ANDGT     1
     C                   Z-ADD     *ZEROS        XXIDT
     C                   Z-ADD     *ZEROS        XXITM
     C     LGKEYP        SETLL     PLG01
     C                   MOVE      *ON           @LDF
     C                   MOVE      'F'           @PGD
     C                   ENDIF
     C*
EDO1 C                   ENDDO
     C*
     C*  Set Page Indicators
     C*
     C     KEYP          IFGT      *ZEROS
     C*** BEGIN CHG *** 09/16/99 **************************************
     C**********           MOVELKEYP      XXIDT
     C**********           MOVE KEYP      XXITM
     C                   Z-ADD     KEYPDT        XXIDT
     C                   Z-ADD     KEYPTM        XXITM
     C*** END CHANGE ** 09/16/99 **************************************
     C     LGKEY         CHAIN     PLG01                              60
     C     LGKEYP        READPE    PLG01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           KEYPF
     C                   ENDIF
     C                   ENDIF
     C*
     C     KEYN          IFGT      *ZEROS
     C*** BEGIN CHG *** 09/16/99 **************************************
     C**********           MOVELKEYN      XXIDT
     C**********           MOVE KEYN      XXITM
     C                   Z-ADD     KEYNDT        XXIDT
     C                   Z-ADD     KEYNTM        XXITM
     C*** END CHANGE ** 09/16/99 **************************************
     C     LGKEY         CHAIN     PLG01                              60
     C     LGKEYP        READE     PLG01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           KEYNF
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *ON           *IN05
     C                   ENDIF
     C*
     C*  INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C*
     C     NORCD         TAG
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVEL     *BLANK        SCMTC
     C                   MOVEL     *BLANK        SCMTCD
     C                   MOVEL     *BLANK        SCSTS
     C                   Z-ADD     *ZEROS        SCDUE
     C                   Z-ADD     *ZEROS        SCEXDT
     C                   Z-ADD     *ZEROS        SCSENT
     C                   Z-ADD     *ZEROS        SCARCV
     C                   MOVEL     *BLANK        SCSTAT
     C                   Z-ADD     *ZEROS        SCIDT
     C                   Z-ADD     *ZEROS        SCITM
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     RRN2          #@$2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C*** BEGIN ADD *** 09/16/99 **************************************
     C                   Z-ADD     *ZEROS        SCIDT
     C                   Z-ADD     *ZEROS        SCITM
     C*** END ADD ***** 09/16/99 **************************************
     C*
     C                   ENDSR
** PGM DESC
      EDI LOG HISTORY SEARCH
        EDI LOG HISTORY SEARCH
