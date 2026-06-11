     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      *
      /TITLE W0190R - DIARY DATE MAINTENANCE
      * ***************************************************************
      *  DESCRIPTION:  THIS PROGRAM WILL DISPLAY DIARY INFORMATION.   *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    W0070R - CLAIM MASTER SEARCH                               *
      *  CALLS TO:                                                    *
      *    NONE                                                       *
      *                                                               *
      * ***************************************************************
      *  INDICATOR USAGE:                                             *
      *                                                               *
      *  64 - CLAIM WAS CONVERTED FROM ANOTHER SYSTEM                 *
      *  69 - EXCESS REINSURANCE                                      *
      *  70 - BAR CODE                                                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  06/09/20  C:318 Project: SY420
     H*                  Mask SSN
     H*
     H* 08/26/05 C:61  PROJECT WC1315                                 *
     H*                Change to pull default DARTS code description  *
     H*                from table file (not hard coded).              *
     H*                                                               *
     H* 10/14/03 C: 53 SUPPORT: WC129 #39135                          *
     H*                Do not use defaults for dates.                 *
     H*                                                               *
     H* 12/19/00 C:15 Project: WC577                                  *
     H*               Added code to authority subroutine to correct   *
     H*               the authority edit.                             *
     H*                                                               *
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
      * 11/16/98 C:15 Project: WC577                                  *
      *               Changed the bar code process.  A bar code API   *
      *               is now called to return certain parameters.     *
      *                                                               *
      * 02/23/98 C:85 Project: WC577                                  *
      *               CHANGED THE FIELD MOVED TO CMSMCD               *
      *               FROM *BLANKS TO CTSMCD.                         *
      *
      *  01/16/98  47  60AND                                          *
      *         1. IF CALLED FROM W0070R AND IN DISPLAY MODE, GO TO   *
      *            SCR02 IN DISPLAY MODE.                             *
      *         2. EVEN IF OPT 2 OR 5 WAS SELECTED, END THE PROGRAM   *
      *            SEQUENCE IF F3=EXIT WAS HIT.                       *
      *                                                               *
      *    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
      *    -------------------------------------------------------    *
      * ***************************************************************
     FW0190F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMDDL     UF A E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM03)
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     F*** BEGIN CHG *** 04/26/00 **************************************
     F*****SDCMLA  UF  E           K        DISK                      A
     F********    PCM01                             KRENAMEPCM02
     FSDCMLA    UF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F********SDCMLI  IF  E           K        DISK
     F********    PCM01                             KRENAMEPCM0I
     F*** END CHG *** 04/26/00 ****************************************
     FSTCTL1    IF   E           K DISK
     FMMOPL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP03)
     FWMCXL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
      *
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      *

     D UA              S             10    DIM(3) CTDATA PERRCD(1)
     D SQ              S              5  0 DIM(5)
      ***** BEG DEL ******* 08/26/05 **********************************
     D*********           DSC     1   5 20
      ***** END DEL ******* 08/26/05 **********************************
     D STF             S              3    DIM(5)
     D USR             S             10    DIM(5)
     D DCID            S             12    DIM(5)
     D FL2             S              1    DIM(5)
     D FL3             S              1    DIM(5)
     D MNAM            S             10    DIM(5)
     D OPTN            S              4    DIM(5)
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  SQV                    1     25  0
     D                                     DIM(5)
     D  DDSDSQ                 1      5  0
     D  DDCDSQ                 6     10  0
     D  DDMDSQ                11     15  0
     D  DDBDSQ                16     20  0
     D  DDUDSQ                21     25  0
     D                 DS
     D  COM                    1    163
     D  XXCOM1                 1     43
     D  XXCOM2                44    103
     D  XXCOM3               104    163
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS
     D  QDT                    1     40  0
     D                                     DIM(5)
     D  QCSDAT                 1      8  0
     D  QCCDAT                 9     16  0
     D  QCMDAT                17     24  0
     D  QCBDAT                25     32  0
     D  QCUDAT                33     40  0
     D                 DS
     D  DT                     1     40  0
     D                                     DIM(5)
     D  SCSDAT                 1      8  0
     D  SCCDAT                 9     16  0
     D  SCMDAT                17     24  0
     D  SCBDAT                25     32  0
     D  SCUDAT                33     40  0
     D                 DS
     D  RC                     1     25
     D                                     DIM(5)
     D  SCRC01                 1      5
     D  SCRC02                 6     10
     D  SCRC03                11     15
     D  SCRC04                16     20
     D  SCRC05                21     25
     D                 DS
     D  CM1                    1    215
     D                                     DIM(5)
     D  SCCM11                 1     43
     D  SCCM21                44     86
     D  SCCM31                87    129
     D  SCCM41               130    172
     D  SCCM51               173    215
     D                 DS
     D  CM2                    1    300
     D                                     DIM(5)
     D  SCCM12                 1     60
     D  SCCM22                61    120
     D  SCCM32               121    180
     D  SCCM42               181    240
     D  SCCM52               241    300
     D                 DS
     D  CM3                    1    300
     D                                     DIM(5)
     D  SCCM13                 1     60
     D  SCCM23                61    120
     D  SCCM33               121    180
     D  SCCM43               181    240
     D  SCCM53               241    300
     D                 DS
     D  OT                     1      6
     D                                     DIM(6)
     D  OPTY01                 1      1
     D  OPTY02                 2      2
     D  OPTY03                 3      3
     D  OPTY04                 4      4
     D  OPTY05                 5      5
     D  OPTY06                 6      6
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
      *
     D OPTNM           C                   CONST('DARTS Benefit Diary -
     D                                     Screen')
     D XCEXRE          C                   CONST('EXCS REIN ')
     D*** BEGIN DLT *** 11/16/98 **************************************
     D************* 'FILE STATUS IS OUT, -C         XCBC01
     D************* 'USER - XXXXXXXXXX'
     D************* 'FILE STATUS IS IN'   C         XCBC02
     D*** END DLT ***** 11/16/98 **************************************
     D*** BEGIN ADD *** 11/16/98 **************************************
     D XCBC01          C                   CONST('CLAIM FILE STATUS IS-
     D                                      OUT, USER -')
     D XCBC02          C                   CONST('          CLAIM FILE-
     D                                      STATUS IS IN')
     D*** END ADD ***** 11/16/98 **************************************
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     I*** BEGIN DEL *** 04/26/00 **************************************
     I********PCM02
     I********      CMUSER                          C2USER
     I********      CMSTAT                          C2STAT
     I********PCM0I
     I********      CMKEY                           PIKEY
     I********      CMSEQ#                          PISEQ#
     I*** END DEL *** 04/26/00 ****************************************
     IPUP02
     I              UPCO#                       U2CO#
     I              UPFND                       U2FND
     I              UPUSR                       U2USR
     IPUP03
     I              UPCO#                       U3CO#
     I              UPFND                       U3FND
     I              UPUSR                       U3USR
     I              UPTYPE                      U3TYPE
     I              UPLVL                       U3LVL

     C*
     C*  CLAIM DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
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
     C*  REMARK CODE TABLE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXRMCD
     C*
     C*   KEY LIST FOR THE CLAIM/DARTS CONTROL FILE
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  DARTS FILE
     C*
     C     C2KY          KLIST
     C                   KFLD                    QCKEY
     C                   KFLD                    XXSEQ#            5 0
     C*
     C*  DIARY DATE FILE
     C*
     C     DDKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*  OPTION FILE
     C*
     C     OPKEY         KLIST
     C                   KFLD                    XXMNAM
     C                   KFLD                    XXOPTN
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKY1         KLIST
     C                   KFLD                    XXSTAF
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQACDT            8
     C                   PARM                    QQTRCD            1
     C*
     C     ACDPL3        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQPGM            10
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
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMCASE        SVCASE
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMEMP#        SVEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMDIV         SVDIV
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMACDT        XXACDT                         ACCIDENT DATE
     C     *LIKE         DEFINE    CMACDT        XXDT                           SYSTEM DT
     C     *LIKE         DEFINE    CMACDT        XXDATE
     C     *LIKE         DEFINE    OPMNAM        XXMNAM
     C     *LIKE         DEFINE    OPOPTN        XXOPTN
     C     *LIKE         DEFINE    CTRMCD        XXRMCD
     C     *LIKE         DEFINE    CMACDT        XXMDAT
     C     *LIKE         DEFINE    CMACDT        XXCDAT
     C     *LIKE         DEFINE    CMACDT        XXBDAT
     C     *LIKE         DEFINE    CMACDT        XXSDAT
     C     *LIKE         DEFINE    CMACDT        XXUDAT
     C     *LIKE         DEFINE    UPSTAF        XXSTAF
     C*
     C*  INITIALIZE FIELDS
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQACDT        XAACDT            8 0          MMDDYY
     C                   MOVE      XXFYR         SCFYR
     C                   MOVE      XXCASE        SCCASE
     C                   MOVE      XXCASE        SVCASE
     C     XAACDT        COMP      0                                      30
      *
     C                   Z-ADD     0             DT
     C                   Z-ADD     0             QDT
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C     XXCO#         IFEQ      *ZERO
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPFND         XXFND
     C                   END
     C*
     C* BEGIN PROCESSING
     C*
     C     STRT01        TAG
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVE      CONAME        SCCONM
     C                   END
     C*
     C*  GET THE CLAIMS CONTROL INFORMATION
     C*
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       CXRADS
     C                   MOVE      *BLANKS       CXRSSD
     C                   MOVE      *BLANKS       CXRUMD
     C                   MOVE      *BLANKS       CXRCAD
     C                   MOVE      *BLANKS       CXRSRD
     C                   Z-ADD     *ZERO         CXBADJ
     C                   Z-ADD     *ZERO         CXBSPV
     C                   Z-ADD     *ZERO         CXBMGR
     C                   END
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVEL     OPTNM         SCPRDS
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C     QQCALL        IFEQ      '1'
     C     QQCALL        OREQ      'Z'
     C                   MOVE      *ON           *IN09
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C* COMING FROM CLAIM PROGRAM DO NOT ALLOW FOR CHANGE IN USER PARMS
     C*
     C     QQTRCD        IFNE      *BLANK
     C                   MOVE      '1'           *IN15
     C                   END
     C*
     C*  GET THE FUND INFORMATION
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCNAME
     C                   GOTO      STRT
     C                   END
     C                   MOVE      FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C                   MOVEL     FNNAME        SCFNAM
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     *IN15         IFEQ      *ON
     C                   GOTO      SCR01T
     C                   END
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*=========================================================================
     C*  Process Function keys
     C*
     C     *IN11         IFEQ      '1'                                          VALID CMD KEYS
     C**********
     C*   F3 = Exit
     C**********
     C     *INKC         IFEQ      *ON
     C                   MOVE      'A'           QQTRCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C**********
     C*   F12 = Cancel
     C**********
     C     *INKL         IFEQ      *ON
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C     *IN15         IFEQ      '1'
     C                   MOVE      'B'           QQTRCD
     C                   GOTO      TERM
     C                   ELSE
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF                                                  Q2RECF = SCR02
     C*
     C                   ENDIF                                                  *INKL = *ON
     C**********
     C*   F10 = Search
     C**********
     C     *INKJ         IFEQ      *ON
     C***
     C*   Screen one
     C***
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*   Claim search
     C*
     C                   CALL      'W0070R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM      *ZEROS        @@RRN             9 0
     C                   PARM      'CN'          @@SORT            2
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM      SCFYR         @@FYR             3 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZERO
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C*
     C     @@RRN         CHAIN     WMCMP                              60
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMFYR         SCFYR
     C                   MOVE      CMCASE        SCCASE
     C                   MOVE      CMSS#         XXSS#
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
     C                   END
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    *INKL
     C*
     C                   END                                                    SCR01
     C***
     C*   Screen two
     C***
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00'          *IN(42)
     C*
     C*   Diary remark search
     C*
     C                   MOVE      *BLANKS       XXCODE
     C                   CALL      'SC581R'
     C                   PARM                    XXCODE            5
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCRC01'
     C                   MOVE      '1'           *IN71
     C     QQRTC         IFEQ      'B'
     C     XXCODE        OREQ      *BLANKS
     C                   MOVE      '1'           *IN42
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   Z-ADD     1             X                 3 0
     C                   MOVEL(P)  XXCODE        RC(X)
     C                   MOVE      '1'           *IN43
      ***** BEG ADD ******* 08/26/05 **********************************
     C                   MOVEL(P)  XXCODE        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C  N60              MOVEL(P)  CTDESC        SCCM11
      ***** END ADD ******* 08/26/05 **********************************
     C                   END
     C*
     C     HFLD          WHENEQ    'SCRC02'
     C                   MOVE      '1'           *IN72
     C     QQRTC         IFEQ      'B'
     C     XXCODE        OREQ      *BLANKS
     C                   MOVE      '1'           *IN42
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   Z-ADD     2             X
     C                   MOVEL(P)  XXCODE        RC(X)
     C                   MOVE      '1'           *IN43
      ***** BEG ADD ******* 08/26/05 **********************************
     C                   MOVEL(P)  XXCODE        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C  N60              MOVEL(P)  CTDESC        SCCM21
      ***** END ADD ******* 08/26/05 **********************************
     C                   END
     C*
     C     HFLD          WHENEQ    'SCRC03'
     C                   MOVE      '1'           *IN73
     C     QQRTC         IFEQ      'B'
     C     XXCODE        OREQ      *BLANKS
     C                   MOVE      '1'           *IN42
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   Z-ADD     3             X
     C                   MOVEL(P)  XXCODE        RC(X)
     C                   MOVE      '1'           *IN43
      ***** BEG ADD ******* 08/26/05 **********************************
     C                   MOVEL(P)  XXCODE        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C  N60              MOVEL(P)  CTDESC        SCCM31
      ***** END ADD ******* 08/26/05 **********************************
     C                   END
     C*
     C     HFLD          WHENEQ    'SCRC04'
     C                   MOVE      '1'           *IN74
     C     QQRTC         IFEQ      'B'
     C     XXCODE        OREQ      *BLANKS
     C                   MOVE      '1'           *IN42
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   Z-ADD     4             X
     C                   MOVEL(P)  XXCODE        RC(X)
     C                   MOVE      '1'           *IN43
      ***** BEG ADD ******* 08/26/05 **********************************
     C                   MOVEL(P)  XXCODE        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C  N60              MOVEL(P)  CTDESC        SCCM41
      ***** END ADD ******* 08/26/05 **********************************
     C                   END
     C*
     C     HFLD          WHENEQ    'SCRC05'
     C                   MOVE      '1'           *IN75
     C     QQRTC         IFEQ      'B'
     C     XXCODE        OREQ      *BLANKS
     C                   MOVE      '1'           *IN42
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   Z-ADD     5             X
     C                   MOVEL(P)  XXCODE        RC(X)
     C                   MOVE      '1'           *IN43
      ***** BEG ADD ******* 08/26/05 **********************************
     C                   MOVEL(P)  XXCODE        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C  N60              MOVEL(P)  CTDESC        SCCM51
      ***** END ADD ******* 08/26/05 **********************************
     C                   END
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C                   END                                                    SCR02
     C*
     C                   END                                                    F10
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C                   GOTO      STRT
     C                   END
     C**********
     C                   END                                                    End function keys
     C*=========================================================================
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*=========================================================================
     C*   PROCESS REQUEST SCREEN  1
     C*
     C     SCR01T        TAG
     C*
     C                   Z-ADD     SCFYR         XXFYR
      *
     C                   EXSR      FIRST
     C*
     C* VALIDATE FUND YEAR
     C*
     C     F1KEY         CHAIN     PF101                              23
     C     *IN23         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C* VALIDATE CLAIM NUMBER
     C*
     C     CMKY          CHAIN     PCM01                              20
     C     *IN20         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   Z-ADD     0             SQV
     C   30              Z-ADD     CMACDT        SRDTI
     C   30              CALL      'SRCVTDAT'    DTMDYY
     C   30              MOVEL     SRDTO         XAACDT            8 0
     C     DDKEY         CHAIN(N)  PDD01                              40
     C     *IN40         IFEQ      '0'
     C*
     C                   MOVE      UA(2)         SCMODE
      *
     C                   Z-ADD     DDSDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSDAT
      *
     C                   Z-ADD     DDCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCDAT
      *
     C                   Z-ADD     DDMDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMDAT
      *
     C                   Z-ADD     DDBDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCBDAT
      *
     C                   Z-ADD     DDUDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCUDAT
     C*
      *** BEG ADD *** 10/14/03 *************************************
     C                   ENDIF
      *** BEG ADD *** 10/14/03 *************************************
      *** BEG DEL *** 10/14/03 *************************************
     C**********           ELSE                            ADD MODE
     C**********
     C****BEG*DEL *** 01/16/98 *************************************
     C********** *IN09     IFEQ '1'
     C**********           MOVE '1'       *IN20
     C**********           GOTO STRT
     C**********           END
     C****END*DEL *** 01/16/98 *************************************
     C**********
     C**********           MOVE UA,1      SCMODE    P
     C****BEG*ADD *** 01/16/98 *************************************
     C********** *IN09     IFEQ '1'
     C**********           MOVE UA,3      SCMODE    P
     C**********           END
     C****END*ADD *** 01/16/98 *************************************
     C**********           MOVE *BLANKS   SCSDAT
     C**********           MOVE *BLANKS   SCCDAT
     C**********           MOVE *BLANKS   SCMDAT
     C**********           MOVE *BLANKS   SCBDAT
     C**********           MOVE *BLANKS   SCUDAT
     C**********
     C**IF*LOSS*TIME FLAG = Y CALCULATE SUPERVISOR AND UNIT MANAGER DATES
     C**********
     C********** CMLTFL    IFEQ 'I'
     C**********
     C**********           Z-ADDCMRCVD    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDYY
     C**********           MOVELSRDTO     XXRCVD  80
     C**********
     C********** XXRCVD    IFEQ *ZERO
     C**********           MOVE DSDATE    XXRCVD
     C**********           END
     C**********
     C***CALCULATE SUPERVISOR DATE
     C**********
     C**********           MOVE XXRCVD    SRDATE  80
     C**********           Z-ADDCXBSPV    SRDAYS  50
     C**********           MOVE *BLANKS   SRERR   1
     C**********           CALL 'SRFUTR8'
     C**********           PARM           SRDATE
     C**********           PARM           SRDAYS
     C**********           PARM           SRERR
     C**********           MOVE SRDATE    SCSDAT
     C**********
     C***CALCULATE MANAGER DATE
     C**********
     C**********           MOVE XXRCVD    SRDATE
     C**********           Z-ADDCXBMGR    SRDAYS
     C**********           MOVE *BLANKS   SRERR
     C**********           CALL 'SRFUTR8'
     C**********           PARM           SRDATE
     C**********           PARM           SRDAYS
     C**********           PARM           SRERR
     C**********           MOVE SRDATE    SCSDAT
     C**********
     C**********           END
     C**********
     C***IF*CLAIM LOST TIME FLAG = 'N' AND ADDING A RECORD, CALCULATE
     C***MEDICAL DATE.
     C**********
     C********** CMLTFL    IFNE 'I'
     C**********           MOVE DSDATE    SRDATE
     C**********           Z-ADDCXBADJ    SRDAYS
     C**********           MOVE *BLANKS   SRERR
     C**********           CALL 'SRFUTR8'
     C**********           PARM           SRDATE
     C**********           PARM           SRDAYS
     C**********           PARM           SRERR
     C**********           MOVE SRDATE    SCMDAT
     C**********           END
     C**********
     C**********           END
      *** END DEL *** 10/14/03 *************************************
     C*
     C*  INITIALIZE DARTS FIELDS
     C*
     C                   MOVE      *BLANKS       RC
     C                   MOVE      *BLANKS       CM1
     C                   MOVE      *BLANKS       CM2
     C                   MOVE      *BLANKS       CM3
     C                   MOVE      CXRSSD        SCRC01
      *
      ***** BEG DEL ******* 08/26/05 **********************************
     C*********            MOVELDSC,1     SCCM11
      ***** END DEL ******* 08/26/05 **********************************
      ***** BEG ADD ******* 08/26/05 **********************************
      *
     C                   MOVE(P)   *BLANKS       SCCM11
     C                   MOVEL(P)  SCRC01        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C  N60              MOVEL(P)  CTDESC        SCCM11
      *
      ***** END ADD ******* 08/26/05 **********************************
     C                   MOVE      *BLANKS       SCCM12
     C                   MOVE      *BLANKS       SCCM13
     C                   MOVE      CXRCAD        SCRC02
      *
      ***** BEG DEL ******* 08/26/05 **********************************
     C*********            MOVELDSC,2     SCCM21
      ***** END DEL ******* 08/26/05 **********************************
      ***** BEG ADD ******* 08/26/05 **********************************
      *
     C                   MOVE(P)   *BLANKS       SCCM21
     C                   MOVEL(P)  SCRC02        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C  N60              MOVEL(P)  CTDESC        SCCM21
      *
      ***** END ADD ******* 08/26/05 **********************************
     C                   MOVE      *BLANKS       SCCM22
     C                   MOVE      *BLANKS       SCCM23
     C                   MOVE      CXRADS        SCRC03
      *
      ***** BEG DEL ******* 08/26/05 **********************************
     C*********            MOVELDSC,3     SCCM31
      ***** END DEL ******* 08/26/05 **********************************
      ***** BEG ADD ******* 08/26/05 **********************************
      *
     C                   MOVE(P)   *BLANKS       SCCM31
     C                   MOVEL(P)  SCRC03        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C  N60              MOVEL(P)  CTDESC        SCCM31
      *
      ***** END ADD ******* 08/26/05 **********************************
     C                   MOVE      *BLANKS       SCCM32
     C                   MOVE      *BLANKS       SCCM33
     C                   MOVE      CXRSRD        SCRC04
      *
      ***** BEG DEL ******* 08/26/05 **********************************
     C*********            MOVELDSC,4     SCCM41
      ***** END DEL ******* 08/26/05 **********************************
      ***** BEG ADD ******* 08/26/05 **********************************
      *
     C                   MOVE(P)   *BLANKS       SCCM41
     C                   MOVEL(P)  SCRC04        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C  N60              MOVEL(P)  CTDESC        SCCM41
      *
      ***** END ADD ******* 08/26/05 **********************************
     C                   MOVE      *BLANKS       SCCM42
     C                   MOVE      *BLANKS       SCCM43
     C                   MOVE      CXRUMD        SCRC05
      *
      ***** BEG DEL ******* 08/26/05 **********************************
     C*********            MOVELDSC,5     SCCM51
      ***** END DEL ******* 08/26/05 **********************************
      ***** BEG ADD ******* 08/26/05 **********************************
      *
     C                   MOVE(P)   *BLANKS       SCCM51
     C                   MOVEL(P)  SCRC05        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C  N60              MOVEL(P)  CTDESC        SCCM51
      *
      ***** END ADD ******* 08/26/05 **********************************
     C                   MOVE      *BLANKS       SCCM52
     C                   MOVE      *BLANKS       SCCM53
     C*
     C*   LOAD DARTS DIARY REMARK FOR EACH DATE IF ONE EXISTS.
     C*
     C     *IN40         IFEQ      '0'
     C                   MOVE      *BLANKS       QCKEY
     C                   Z-ADD     XXCO#         QCCO#
     C                   Z-ADD     SCFND         QCFND
     C                   Z-ADD     XXFYR         QCFYR
     C                   Z-ADD     SCCASE        QCCASE
     C*
     C                   DO        5             X
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   Z-ADD     SQV(X)        XXSEQ#
     C     C2KY          CHAIN(N)  PCM02                              65
     C     *IN65         IFEQ      '0'
     C     XXSEQ#        ANDNE     0
     C     CMDCDT        ANDEQ     0
     C     XXDATE        ANDEQ     CMCDAT
     C                   MOVE      *BLANKS       RC(X)
     C                   MOVEL     CMRMCD        RC(X)
     C                   MOVE      *BLANKS       COM
     C                   MOVE      *BLANKS       CM1(X)
     C                   MOVE      *BLANKS       CM2(X)
     C                   MOVE      *BLANKS       CM3(X)
     C                   MOVEL     CMCOMM        COM
     C                   MOVEL     XXCOM1        CM1(X)
     C                   MOVEL     XXCOM2        CM2(X)
     C                   MOVEL     XXCOM3        CM3(X)
     C                   ELSE
     C                   Z-ADD     0             DT(X)
     C                   Z-ADD     0             SQV(X)
     C                   END
     C                   END
     C                   END
     C*
     C*  LOAD DATES INTO SAVE FIELDS
     C*
     C                   Z-ADD     SCSDAT        QCSDAT
     C                   Z-ADD     SCCDAT        QCCDAT
     C                   Z-ADD     SCMDAT        QCMDAT
     C                   Z-ADD     SCBDAT        QCBDAT
     C                   Z-ADD     SCUDAT        QCUDAT
     C                   MOVEA     SQV           SQ
     C                   MOVE      '0'           *IN80
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*  =============================================================
     C*   PROCESS REQUEST SCREEN  2
     C*
     C     SCR02T        TAG
     C*
     C     *IN09         CABEQ     '1'           NOEDT
     C                   Z-ADD     XXDT          XXDATE
     C                   MOVE      *BLANKS       STF
     C                   MOVE      *BLANKS       USR
     C                   MOVE      *BLANKS       DCID
     C                   MOVE      *BLANKS       FL2
     C                   MOVE      *BLANKS       FL3
     C                   MOVE      *BLANKS       MNAM
     C                   MOVE      *BLANKS       OPTN
     C                   MOVE      *BLANKS       OT
     C*
     C*  VALIDATE THE ENTERED MEDICAL DATE
     C*
     C     SCMDAT        IFNE      *ZEROS
     C     SCMDAT        IFNE      QCMDAT
     C     *IN40         OREQ      '1'
     C*
     C                   Z-ADD     SCMDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCMDAT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
      *
     C                   Z-ADD     SCMDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXMDAT
     C     XXMDAT        IFLT      XXDATE
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C     CMCLEX        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN47
     C                   GOTO      STRT
     C                   END
     C                   MOVE      CMCLEX        STF(3)
     C*
     C     SCRC03        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN54
     C                   GOTO      STRT
     C                   END
     C     SCCM31        IFEQ      *BLANKS
     C     SCCM32        ANDEQ     *BLANKS
     C     SCCM33        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN55
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANKS       XXRMCD
     C                   MOVEL     SCRC03        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C     CTFL02        IFNE      'M'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEL     CTDEPG        DCID(3)
     C                   MOVEL     CTMNAM        MNAM(3)
     C                   MOVEL     CTOPTN        OPTN(3)
     C     CTDPRG        IFGT      *ZERO
     C                   MOVE      'Y'           FL2(3)
     C                   ELSE
     C                   MOVE      *BLANKS       FL2(3)
     C                   END
     C                   MOVE      CTFL02        FL3(3)
     C*
     C                   END
     C                   END
     C*
     C*  VALIDATE THE ENTERED COMPENSATION DATE
     C*
     C     SCCDAT        IFNE      *ZEROS
     C     SCCDAT        IFNE      QCCDAT
     C     *IN40         OREQ      '1'
     C*
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCDAT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
      *
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C     XXCDAT        IFLT      XXDATE
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C     CMCLEX        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN46
     C                   GOTO      STRT
     C                   END
     C                   MOVE      CMCLEX        STF(2)
     C*
     C     SCRC02        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN52
     C                   GOTO      STRT
     C                   END
     C     SCCM21        IFEQ      *BLANKS
     C     SCCM22        ANDEQ     *BLANKS
     C     SCCM23        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN53
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANKS       XXRMCD
     C                   MOVEL     SCRC02        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C     CTFL02        IFNE      'C'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEL     CTDEPG        DCID(2)
     C                   MOVEL     CTMNAM        MNAM(2)
     C                   MOVEL     CTOPTN        OPTN(2)
     C     CTDPRG        IFGT      *ZERO
     C                   MOVE      'Y'           FL2(2)
     C                   ELSE
     C                   MOVE      *BLANKS       FL2(2)
     C                   END
     C                   MOVE      CTFL02        FL3(2)
     C*
     C                   END
     C                   END
     C*
     C*  VALIDATE THE ENTERED BCL-13 DATE
     C*
     C     SCBDAT        IFNE      *ZEROS
     C     SCBDAT        IFNE      QCBDAT
     C     *IN40         OREQ      '1'
     C*
     C                   Z-ADD     SCBDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCBDAT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
      *
     C                   Z-ADD     SCBDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBDAT
     C     XXBDAT        IFLT      XXDATE
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C     CMCLEX        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN48
     C                   GOTO      STRT
     C                   END
     C                   MOVE      CMCLEX        STF(4)
     C*
     C     SCRC04        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN56
     C                   GOTO      STRT
     C                   END
     C     SCCM41        IFEQ      *BLANKS
     C     SCCM42        ANDEQ     *BLANKS
     C     SCCM43        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN57
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANKS       XXRMCD
     C                   MOVEL     SCRC04        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C     CTFL02        IFNE      'D'
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEL     CTDEPG        DCID(4)
     C                   MOVEL     CTMNAM        MNAM(4)
     C                   MOVEL     CTOPTN        OPTN(4)
     C     CTDPRG        IFGT      *ZERO
     C                   MOVE      'Y'           FL2(4)
     C                   ELSE
     C                   MOVE      *BLANKS       FL2(4)
     C                   END
     C                   MOVE      CTFL02        FL3(4)
     C*
     C                   END
     C                   END
     C*
     C*  VALIDATE THE ENTERED SUPERVISOR DATE
     C*
     C     SCSDAT        IFNE      *ZEROS
     C     SCSDAT        IFNE      QCSDAT
     C     *IN40         OREQ      '1'
     C*
     C                   Z-ADD     SCSDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSDAT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
      *
     C                   Z-ADD     SCSDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSDAT
     C     XXSDAT        IFLT      XXDATE
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C     CMSUPR        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN45
     C                   GOTO      STRT
     C                   END
     C                   MOVE      CMSUPR        STF(1)
     C*
     C     SCRC01        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN50
     C                   GOTO      STRT
     C                   END
     C     SCCM11        IFEQ      *BLANKS
     C     SCCM12        ANDEQ     *BLANKS
     C     SCCM13        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN51
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANKS       XXRMCD
     C                   MOVEL     SCRC01        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C     CTFL02        IFNE      'S'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEL     CTDEPG        DCID(1)
     C                   MOVEL     CTMNAM        MNAM(1)
     C                   MOVEL     CTOPTN        OPTN(1)
     C     CTDPRG        IFGT      *ZERO
     C                   MOVE      'Y'           FL2(1)
     C                   ELSE
     C                   MOVE      *BLANKS       FL2(1)
     C                   END
     C                   MOVE      CTFL02        FL3(1)
     C*
     C                   END
     C                   END
     C*
     C*  VALIDATE THE ENTERED CLAIM MANAGER DATE
     C*
     C     SCUDAT        IFNE      *ZEROS
     C     SCUDAT        IFNE      QCUDAT
     C     *IN40         OREQ      '1'
     C*
     C                   Z-ADD     SCUDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCUDAT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
      *
     C                   Z-ADD     SCUDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXUDAT
     C     XXUDAT        IFLT      XXDATE
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C     CMUMAN        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN49
     C                   GOTO      STRT
     C                   END
     C                   MOVE      CMUMAN        STF(5)
     C*
     C     SCRC05        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN58
     C                   GOTO      STRT
     C                   END
     C     SCCM51        IFEQ      *BLANKS
     C     SCCM52        ANDEQ     *BLANKS
     C     SCCM53        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN59
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANKS       XXRMCD
     C                   MOVEL     SCRC05        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C     CTFL02        IFNE      'G'
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEL     CTDEPG        DCID(5)
     C                   MOVEL     CTMNAM        MNAM(5)
     C                   MOVEL     CTOPTN        OPTN(5)
     C     CTDPRG        IFGT      *ZERO
     C                   MOVE      'Y'           FL2(5)
     C                   ELSE
     C                   MOVE      *BLANKS       FL2(5)
     C                   END
     C                   MOVE      CTFL02        FL3(5)
     C*
     C                   END
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED  - - - - - - - - - - - - - - - - - -
     C*
     C*  A DARTS RECORD WILL BE WRITTEN IF IN ADD MODE OR IF A DATE WAS
     C*  CHANGED AND THE CORRESPONDING STAFF CODE EXISTS IN WMCMP.
     C*
     C                   DO        5             X
     C     DT(X)         IFNE      *ZEROS
     C     *IN40         IFEQ      '1'
     C     DT(X)         ORNE      QDT(X)
     C*
     C*  GET DIARY USER AND CHECK AUTHORITY. IF NOT AUTHORIZED SEND
     C*  DIARY TO SUPERVISOR.
     C*
     C                   MOVE      *BLANKS       XXSTAF
     C                   MOVE      STF(X)        XXSTAF
     C                   EXSR      SRAUT
     C     *IN75         IFEQ      '1'
     C                   MOVE      *BLANKS       XXSTAF
     C                   MOVE      CMSUPR        XXSTAF
     C                   END
     C     UPKY1         CHAIN     PUP02                              61
     C  N61              MOVEL     U2USR         USR(X)
     C*
     C                   END
     C                   END
     C                   END                                                    END DO LOOP
     C*
     C*  GET NEXT SEQUENCE NUMBER FOR DARTS RECORD
     C*
     C                   Z-ADD     XXCO#         QCCO#
     C                   Z-ADD     SCFND         QCFND
     C                   Z-ADD     XXFYR         QCFYR
     C                   Z-ADD     SCCASE        QCCASE
     C                   MOVE      *HIVAL        XXSEQ#
     C*
     C*  DO LOOP TO WRITE DARTS RECORD
     C*
     C                   DO        5             X
     C     USR(X)        IFNE      *BLANKS
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********
     C***INITIALIZE FIELDS
     C********
     C********             MOVE *BLANKS   CMCOMM
     C********             MOVE *BLANKS   CMKEY
     C********             MOVE *BLANKS   CMDCID
     C********             MOVE *BLANKS   CMPNAM
     C********             MOVE *BLANKS   C2USER
     C********             MOVE *BLANKS   CMPGM
     C********             MOVE *BLANKS   CMIUSR
     C********             MOVE *BLANKS   CMFOLD
     C********             MOVE *BLANKS   CMIPGM
     C********             MOVE *BLANKS   CMRMCD
     C********
     C********             MOVELQCKEY     CMKEY
     C********             MOVE *BLANKS   CMCSID
     C********             MOVE *BLANKS   COM
     C********             MOVELCM1,X     XXCOM1
     C********             MOVELCM2,X     XXCOM2
     C********             MOVELCM3,X     XXCOM3
     C********             MOVELCOM       CMCOMM
     C********
     C********             Z-ADDDT,X      SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMCDAT
     C********             MOVE *BLANKS   CMCODE
     C********             MOVELDCID,X    CMDCID
     C********             MOVEL'W0070R'  CMPNAM
     C********
     C********             Z-ADDXXDT      CMTRDT
     C********             Z-ADDDSTIME    CMTRTM
     C********             Z-ADD*ZEROS    CMSECL
     C********             MOVE 'N'       CMSBYL
     C********             MOVE 'A'       C2STAT
     C********             MOVELUSR,X     C2USER
     C********             MOVELQ1PGM     CMPGM
     C********             MOVE '0000001' CMTIME
     C********
     C********             Z-ADDXXDT      CMDT
     C********             MOVELQ1USER    CMIUSR
     C********   CMDCID    IFNE *BLANKS
     C********             MOVELQCKEY     CMFOLD
     C********             ELSE
     C********             MOVE *BLANKS   CMFOLD
     C********             END
     C********             Z-ADD0         CMDCDT
     C********             MOVE 'Y'       CMDIFL
     C********             MOVE *BLANKS   CMAPID
     C********             MOVE *BLANKS   CMIDOC
     C********             MOVE *BLANKS   CMIQUE
     C********             MOVE *BLANKS   CMIMGF
     C********             MOVE *BLANKS   CMIPTH
     C********             MOVE *BLANKS   CMPIDX
     C********             MOVE *BLANKS   CMQPTY
     C********             MOVE *BLANKS   CMDCTP
     C********             MOVELQ1PGM     CMIPGM
     C********                                                         *********
     C****BEGIN DEL *** 02/23/98 ***********************************************
     C**********           MOVE *BLANKS   CMSMCD
     C****END*  DEL *** 02/23/98 ***********************************************
     C********                                                         *********
     C********   CMDCID    IFNE *BLANKS
     C********             MOVE 'Y'       CMFL01
     C********             ELSE
     C********             MOVE *BLANKS   CMFL01
     C********             END
     C********             MOVE FL2,X     CMFL02
     C********             MOVE FL3,X     CMFL03
     C********             MOVE *BLANKS   CMFL04
     C********             MOVE *BLANKS   CMFL05
     C********             MOVELRC,X      CMRMCD
     C********                                                         *********
     C****BEGIN ADD *** 02/23/98 ***********************************************
     C********                                                         *********
     C********   CMRMCD    CHAINPCT01                60
     C********                                                         *********
     C********   *IN60     IFEQ *OFF
     C********             MOVE CTSMCD    CMSMCD
     C********             ENDIF                           *IN60=OFF
     C********                                                         *********
     C****END*  ADD *** 02/23/98 ***********************************************
     C********
     C********             Z-ADDXXDT      CMIDT
     C********
     C***GET*LAST SEQ #
     C********
     C********   C2KY      SETGTPCM0I
     C********             READPPCM0I                    60
     C********   *IN60     IFEQ '0'
     C********   QCKEY     ANDEQPIKEY
     C********   PISEQ#    ADD  1         CMSEQ#
     C********             ELSE
     C********             Z-ADD1         CMSEQ#
     C********             END
     C********             WRITEPCM02
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCMLA
     C******END   ADD **** 07/01/99 ***********************************
     C********             Z-ADDCMSEQ#    SQ,X
     C*** END DEL *** 04/26/00 ****************************************
     C*** BEGIN ADD *** 04/26/00 **************************************
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVE      *BLANKS       COM
     C                   MOVEL     CM1(X)        XXCOM1
     C                   MOVEL     CM2(X)        XXCOM2
     C                   MOVEL     CM3(X)        XXCOM3
     C                   MOVEL(P)  COM           Q#COMM          256
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DT(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  DCID(X)       Q#DCID           12
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  USR(X)        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  QCKEY         Q#FOLD           80
     C                   ELSE                                                   DCID <> blanks
     C                   MOVE      *BLANKS       Q#FOLD
     C                   ENDIF                                                  DCID <> blanks
     C                   MOVEL(P)  'Y'           Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVE(P)   'Y'           Q#FL01            1
     C                   ELSE                                                   DCID <> blanks
     C                   MOVE(P)   *BLANKS       Q#FL01
     C                   ENDIF                                                  DCID <> blanks
     C                   MOVEL(P)  FL2(X)        Q#FL02            1
     C                   MOVEL(P)  RC(X)         Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANKS       Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANKS       Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   MOVE      Q#SEQ#        SQ(X)
     C*** END ADD *** 04/26/00 ****************************************
     C*
     C                   END
     C                   END                                                    END DO LOOP
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C     DDKEY         CHAIN     PDD01                              40
     C                   MOVE      XXCO#         DDCO#
     C                   MOVE      SCFND         DDFND
     C                   MOVE      XXFYR         DDFYR
     C                   MOVE      SCCASE        DDCASE
      *
     C                   Z-ADD     SCCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDCDAT
      *
     C                   Z-ADD     SCMDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDMDAT
      *
     C                   Z-ADD     SCSDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDSDAT
      *
     C                   Z-ADD     SCBDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDBDAT
      *
     C                   Z-ADD     SCUDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDUDAT
     C                   MOVE      *BLANKS       DDSTAT
     C                   MOVE      Q1USER        DDUSER
     C                   MOVE      Q1PGM         DDPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     XXDT          DDDT
     C                   Z-ADD     DSTIME        DDTIME
     C                   MOVE      CMCLEX        DDCLEX                         JJJ 2-1-89
     C*
     C*  COMPLETE OLD DARTS DIARY RECORD IF A DIARY DATE IS CHANGED.
     C*  LOAD NEW DIARY DATE SEQ# TO WMDDP.
     C*
     C                   DO        5             X
     C     QDT(X)        IFNE      DT(X)
     C     *IN40         OREQ      '1'
     C*
     C     SQV(X)        IFNE      0
     C                   Z-ADD     SQV(X)        XXSEQ#
     C     C2KY          CHAIN     PCM02                              65
     C     *IN65         IFEQ      '0'
      *
     C                   Z-ADD     XXDT          CMDCDT
     C                   MOVEL     Q1PGM         CMPGM
      *
     C                   Z-ADD     XXDT          CMDT
     C                   MOVE      *BLANKS       CMFL04
     C                   UPDATE    PCM02
     C                   END
     C                   END
     C*
     C                   END
     C                   END
     C                   MOVEA     SQ            SQV
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PDD01
     C                   ELSE
     C                   WRITE     PDD01
     C                   END
     C*
     C     NOEDT         TAG
     C*
     C     *IN15         IFEQ      '1'
     C                   MOVE      '*'           QQTRCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCMODE
     C                   GOTO      STRT
      * ------------------------------------------------------------------
      *  END THE PROGRAM
      *
     C     TERM          TAG
      *
      *  IF QQCALL = Y, GO ON TO THE NEXT PROGRAM.
      *
     C     *INKL         IFEQ      *OFF
      *** BEG ADD *** 01/16/98 *************************************
     C     *INKC         ANDEQ     *OFF
      *** END ADD *** 01/16/98 *************************************
     C     QQCALL        IFEQ      'Y'
     C     QQCALL        OREQ      'Z'
     C     QQCALL        OREQ      'A'
     C     CXDART        IFEQ      'Y'
     C                   Z-ADD     XXCO#         QCCO#
     C                   Z-ADD     SCFND         QCFND
     C                   Z-ADD     XXFYR         QCFYR
     C                   Z-ADD     SCCASE        QCCASE
     C                   MOVEL     QCKEY         QQKEY
     C                   MOVEL     'W0070R'      QQPGM
     C                   CALL      'SC010R'      ACDPL3                 99
     C                   END
     C                   END
     C                   END
      *
     C                   MOVE      '1'           *INLR
     C*
     C*====================================================================*
     C* * *   SUBROUTINE FOR CHECKING USER TYPES, AUTHORITY              ***
     C*====================================================================*
     C     SRAUT         BEGSR
     C*
     C                   MOVE      *BLANKS       XXMNAM
     C                   MOVE      *BLANKS       XXOPTN
     C*** BEGIN ADD *** 12/19/00 **************************************
     C                   MOVE      '0'           *IN75
     C*** END ADD ***** 12/19/00 **************************************
     C*
     C     DCID(X)       IFNE      *BLANKS
     C*** BEGIN ADD *** 12/19/00 **************************************
     C     MNAM(X)       ANDNE     *BLANKS
     C     OPTN(X)       ANDNE     *BLANKS
     C*** END ADD ***** 12/19/00 **************************************
     C                   MOVEL     MNAM(X)       XXMNAM
     C                   MOVEL     OPTN(X)       XXOPTN
     C*
     C     UPKY1         CHAIN     PUP03                              60
     C     *IN60         IFEQ      '0'
     C     OPKEY         CHAIN     POP01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      '0'           *IN75
     C*
     C*  IF THE SECURITY TYPE FROM THE USER FILE IS NOT
     C*  "*" THEN CHECK THE TYPE, AND AUTHORITY
     C*
     C     U3TYPE        IFNE      '*'
     C     U3TYPE        ANDNE     '#'
     C     U3TYPE        ANDNE     '$'
     C     U3TYPE        ANDNE     '&'
     C     U3TYPE        ANDNE     '%'
     C*
     C*  LOOK FOR THE SECURITY TYPE, FROM THE OPTION
     C*
     C     U3TYPE        IFNE      *BLANK
     C     U3TYPE        LOOKUP    OT                                     62
     C  N62              MOVE      '1'           *IN75
     C*
     C   62              DO
     C*
     C*  THE VALUE FROM THE USER FILE MUST BE GREATER OR EQUAL
     C*
     C     U3LVL         IFLT      OPMINS
     C                   MOVE      '1'           *IN75
     C                   END
     C     U3LVL         IFLT      OPMMSC
     C                   MOVE      '1'           *IN75
     C                   END
     C*
     C                   END                                                    END DO
     C*
     C                   ELSE
     C                   MOVE      '1'           *IN75
     C                   END                                                    UPTYPE NE' '
     C*
     C                   ELSE
     C*
     C*  CHECK THE SPECIAL TYPE CODES
     C*
     C                   MOVE      *BLANK        XXSPEC            1
     C     U3TYPE        IFNE      '*'
     C*
     C     1             DO        6             I2                5 0
     C     OT(I2)        IFNE      *BLANK
     C*
     C     U3TYPE        IFEQ      '#'
     C     OT(I2)        IFGE      'A'
     C     OT(I2)        ANDLE     'M'
     C     OT(I2)        OREQ      '#'
     C                   MOVE      '#'           XXSPEC
     C                   END
     C                   END
     C*
     C     U3TYPE        IFEQ      '$'
     C     OT(I2)        IFGE      'N'
     C     OT(I2)        ANDLE     'Z'
     C     OT(I2)        OREQ      '$'
     C                   MOVE      '$'           XXSPEC
     C                   END
     C                   END
     C*
     C     U3TYPE        IFEQ      '&'
     C     OT(I2)        IFGE      'A'
     C     OT(I2)        ANDLE     'Z'
     C     OT(I2)        OREQ      '&'
     C     OT(I2)        OREQ      '#'
     C     OT(I2)        OREQ      '$'
     C                   MOVE      '&'           XXSPEC
     C                   END
     C                   END
     C*
     C     U3TYPE        IFEQ      '%'
     C     OT(I2)        IFGE      '0'
     C     OT(I2)        ANDLE     '9'
     C                   MOVE      '%'           XXSPEC
     C                   END
     C                   END
     C*
     C                   END
     C                   END
     C*
     C     XXSPEC        IFEQ      ' '
     C                   MOVE      '1'           *IN75
     C                   END
     C*
     C     U3LVL         IFLT      OPMINS
     C                   MOVE      '1'           *IN75
     C                   END
     C*
     C     U3LVL         IFLT      OPMMSC
     C                   MOVE      '1'           *IN75
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C                   END
     C                   END
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* FIRST  - SUBROUTINE FOR LOADING SCREEN HEADER FIELDS
     C*===============================================================
     C*
     C     FIRST         BEGSR
      *
      *
     C     CMKY          CHAIN     PCM01                              60
      *
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMEMP#        SVEMP#
     C                   MOVE      CMDIV         XXDIV
     C                   MOVE      CMDIV         SVDIV
      *  ACCIDENT DATE
      *
     C     CMACDT        IFNE      *ZERO
     C                   Z-ADD     CMACDT        XXACDT
     C                   Z-ADD     XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL(P)  SRDTO         SCACDT
     C                   ELSE
     C                   MOVE      *BLANK        SCACDT
     C                   END
      *
      *  GET EXCESS REINSURANCE INFO
      *
     C                   MOVE      *BLANK        SCMSG1
     C                   MOVE      *OFF          *IN69
     C     CMEXCS        IFNE      'Y'
     C     @@RRN         IFNE      *ZERO
     C                   CALL      'WA010R'
     C                   PARM                    @@RRN                          FOR WMCMP
     C                   PARM                    XXFNDR           13 2
     C                   PARM                    XXEXRE            1
     C                   END
     C                   END
      *
     C     XXEXRE        IFEQ      'Y'
     C     CMEXCS        OREQ      'Y'
     C                   MOVEL(P)  XCEXRE        SCMSG1
     C                   MOVE      *ON           *IN69
     C                   END
      *
      * GET SYSTEM CONTROL FILE
      *
     C                   MOVEL(P)  'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *OFF
      *
      * BAR CODE INSTALLED
      *
     C     SYBCIF        IFEQ      'Y'
     C*** BEGIN DLT *** 11/16/98 **************************************
     C******************** CALL 'WA027R'
     C******************** PARM           @@RRN   90
     C*** END DLT ***** 11/16/98 **************************************
     C*** BEGIN ADD *** 11/16/98 **************************************
     C                   CALL      'BA001R'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C*** END ADD ***** 11/16/98 **************************************
     C                   PARM                    QQBCFL            1
     C                   PARM                    QQTRTP            1
     C                   PARM                    QQTUSR           10
     C                   PARM                    QQRTCD            1
      *
     C                   MOVE      *OFF          *IN70
     C                   MOVE      *BLANK        SCMSG2
     C     QQBCFL        IFEQ      'Y'
     C                   MOVE      *ON           *IN70
     C     QQTRTP        IFEQ      'I'
     C                   MOVEL(P)  XCBC02        SCMSG2
     C                   ELSE
     C                   MOVEL(P)  XCBC01        SCMSG2
     C*** BEGIN CHG *** 11/16/98 **************************************
     C******************** MOVE QQTUSR    SCMSG2
     C                   CAT       QQTUSR:1      SCMSG2
     C*** END CHG ***** 11/16/98 **************************************
     C                   END
     C                   END
     C                   END                                                    SYBCIF = Y
      *
     C                   END                                                    *IN60 SSY01
      *
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    XXEMP#
     C                   PARM                    XXDIV
     C                   PARM                    EMNAME
     C                   PARM                    SCEDN
     C                   END                                                    WMEMP
      *
      *  CLAIMANT SS#
      *
     C     CMSS#         IFNE      *BLANK
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C     SS1           CAT(P)    '-':0         SCSS#
     C     SCSS#         CAT       SS2:0         SCSS#
     C     SCSS#         CAT       '-':0         SCSS#
     C     SCSS#         CAT       SS3:0         SCSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
      *
      *  CLAIMANT NAME
      *
     C                   MOVEL     CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      *OFF
     C     CDLSN         CAT(P)    ',':0         SCCNAM                         LAST, FIRST M.
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     CDMID         IFNE      *BLANK
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C     SCCNAM        CAT       '.':0         SCCNAM
     C                   END
     C                   END
      *
     C                   END                                                    CMSS# <> BLANK
      *
      *  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
      *
     C     CMCVT#        IFNE      *BLANK
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C                   END
      *  FYR-CASE
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCS
     C     SCYRCS        CAT       XACASE:0      SCYRCS
      *
     C     CMFYR         IFEQ      *ZERO
     C     CMCASE        OREQ      *ZERO
     C                   MOVE      *BLANK        SCYRCS
     C                   END
      *
      *  AGENCY CLAIM# (OR, STATE CLAIM#)
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVEL(P)  CMACL#        SCSCL#
      *
     C                   ENDSR
      *
      ***** **  TABLE DSC - COMMENTS
      ***** SUPERVISOR DIARY
      ***** COMPENSATION DIARY
      ***** MEDICAL DIARY
      ***** STATE FORM DIARY
      ***** MANAGER DIARY
      *
      *
**  TABLE UA
-- Add --
- Change -
- Display-
