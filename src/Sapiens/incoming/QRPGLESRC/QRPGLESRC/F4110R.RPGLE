     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F4110R - REQUEST WCMC CLAIM DEPT LETTER        C:62 05/14/98 *
     H*                                                               *
     H*  CALLED FROM: F4110C                                          *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/17/11 C:279 PROJECT SY195                                 *
     H*                 QDLS to IFS migration.                        *
     H*                                                               *
     H*  07/01/04 C:62 PROJECT: SY100                                 *
     H*                DATA CENTER LOGIC                              *
     H*                                                               *
     H*  10/18/01 C:62 PROJECT MC048 29357                            *
     H*                COMPILE ONLY.                                  *
     H*                                                               *
     H*  09/30/98 C:62 PROJECT:MC177
     H*                DEFAULT OUTQ TO UPOUTQ IF DOOUTQ IS BLANK.     *
     H*                                                               *
     H*  09/28/98 C:62 PROJECT:MC098 14022                            *
     H*                ADDED PARM TO SDSPDOC AND SA001R               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF4110F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSMDOP     IF   E             DISK
     F                                     RENAME(PDO01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FSMDOL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PDO01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FSMDOL2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PDO01:DBRCDF2)
     FSMDEP     IF   E             DISK
     F*** BEGIN ADD *** 09/30/98 **************************************
     FSMUPL     IF   E           K DISK
     F*** END   ADD *** 09/30/98 **************************************
     FFMCMP     IF   E             DISK
     FFDD1L     UF   E           K DISK
     FFDD1L1    UF   E           K DISK
     F                                     RENAME(PD101:PD102)
     FFDD1L2    UF   E           K DISK
     F                                     RENAME(PD101:PD103)
     FFDD1P     O    E             DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PD101:PD104)
     FFDD1P1    O    E             DISK
     F                                     RENAME(PD101:PD105)
     FFDD1P2    O    E             DISK
     F                                     RENAME(PD101:PD106)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Department Documents')
     D SLMSGI          C                   CONST('MC49998')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('S0555R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(7)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          SMDEP
     C                   PARM                    @@RRN2            9 0          FMCMP
     C                   PARM                    @@REQ#            5 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKDPCD
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKDPCD
     C                   KFLD                    KKDCID
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKDPCD
     C                   KFLD                    KKDESC
     C*
     C     D1KY          KLIST
     C                   KFLD                    K1CO#
     C                   KFLD                    K1SUB#
     C                   KFLD                    K1FND
     C*
     C     D1KY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKREQ#
     C                   KFLD                    K2DPCD
     C                   KFLD                    KKFLDR
     C                   KFLD                    K2DCID
     C*
     C     D1KY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKREQ#
     C*
     C*** BEGIN ADD *** 09/30/98 **************************************
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*** END   ADD *** 09/30/98 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DOCO#         KKCO#
     C     *LIKE         DEFINE    DODPCD        KKDPCD
     C     *LIKE         DEFINE    DODCID        KKDCID
     C     *LIKE         DEFINE    DODESC        KKDESC
     C     *LIKE         DEFINE    D1SUB#        KKSUB#
     C     *LIKE         DEFINE    D1FND         KKFND
     C     *LIKE         DEFINE    D1FYR         KKFYR
     C     *LIKE         DEFINE    D1CASE        KKCASE
     C     *LIKE         DEFINE    D1REQ#        KKREQ#
     C     *LIKE         DEFINE    D1FLDR        KKFLDR
     C     *LIKE         DEFINE    DODPCD        K2DPCD
     C     *LIKE         DEFINE    DODCID        K2DCID
     C     *LIKE         DEFINE    D1CO#         K1CO#
     C     *LIKE         DEFINE    D1SUB#        K1SUB#
     C     *LIKE         DEFINE    D1FND         K1FND
     C     *LIKE         DEFINE    D1REQ#        XXREQ#
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C*** BEGIN DEL *** 12/14/98 **************************************
     C********** SSSTAT    IFNE *BLANKS
     C*
     C**********           MOVE *ON       *IN11
     C*
     C**********           ENDIF
     C*** END   DEL *** 12/14/98 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* CONTRL - Update/Write control record.
     C*================================================================
     C     CONTRL        BEGSR
     C*
     C                   Z-ADD     999           K1CO#
     C                   Z-ADD     99999         K1SUB#
     C                   Z-ADD     999           K1FND
     C*
     C     D1KY          CHAIN     PD101                              99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C     D1REQ#        IFLT      99999
     C*
     C     D1REQ#        ADD       1             D1REQ#
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             D1REQ#
     C*
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        D1USER
     C                   MOVEL     Q1PGM         D1PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         D1DT
     C*
     C                   MOVEL     DSTIME        D1TIME
     C*
     C                   UPDATE    PD101
     C*
     C                   Z-ADD     D1REQ#        XXREQ#
     C*
     C                   ELSE
     C*
     C*  WRITE THE CONTROL RECORD FOR DEPARTMENT REQUESTS
     C*
     C                   Z-ADD     1             D1REQ#
     C                   Z-ADD     *HIVAL        D1CO#
     C                   Z-ADD     *HIVAL        D1SUB#
     C                   Z-ADD     *HIVAL        D1FND
     C                   Z-ADD     *HIVAL        D1FYR
     C                   Z-ADD     *HIVAL        D1CASE
     C                   MOVE      *BLANKS       D1DPCD
     C                   MOVE      *BLANKS       D1FLDR
     C                   MOVE      *BLANKS       D1DCID
     C                   Z-ADD     *HIVAL        D1#CPY
     C                   MOVE      'A'           D1STAT
     C*
     C                   MOVE      Q1USER        D1IUSR
     C                   MOVE      Q1PGM         D1PGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         D1DT
     C*
     C                   Z-ADD     DSTIME        D1TIME
     C                   MOVE      Q1USER        D1IUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         D1IDT
     C*
     C                   WRITE     PD104
     C                   FEOD      FDD1P
     C*
     C                   Z-ADD     D1REQ#        XXREQ#
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* DELREC - Delete UXLCP and UXLCP1 records if no selection.
     C*================================================================
     C     DELREC        BEGSR
     C*
     C     D1KY3         CHAIN     PD101                              99
     C     D1KY3         CHAIN     PD102                              99
     C     D1KY3         CHAIN     PD103                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   DELETE    PD101
     C                   DELETE    PD102
     C                   DELETE    PD103
     C*
     C     D1KY3         READE     PD101                                  99
     C     D1KY3         READE     PD102                                  99
     C     D1KY3         READE     PD103                                  99
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C* IF SCSEL='5' THEN EXECUTE OPT7; IF SCSEL='7' THEN EXECUTE OPT5.
     C*
     C                   SELECT
     C*
     C     SCSEL         WHENEQ    '5'
     C*
     C                   MOVEL(P)  '7'           SCSEL
     C                   MOVEL     *ON           NORFSH
     C*
     C     SCSEL         WHENEQ    '7'
     C*
     C                   MOVEL(P)  '5'           SCSEL
     C                   MOVEL     *ON           NORFSH
     C*
     C                   ENDSL
     C*
     C* # OF COPIES CAN'T BE *ZEROS.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSEL         ANDNE     '5'
     C     SCSEL         ANDNE     '7'
     C     SCCOPY        ANDEQ     *ZEROS
     C*
     C                   Z-ADD     SCHCPY        SCCOPY
     C                   SETON                                        8190
     C*
     C                   ENDIF
     C*
     C* OUTQ MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSEL         ANDNE     '5'
     C     SCSEL         ANDNE     '7'
     C*
     C                   MOVEL(P)  SCOUTQ        ##OUTQ
     C*
     C                   CALL      'S0003C'
     C                   PARM                    ##OUTQ           10
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        IFEQ      '1'
     C*
     C                   MOVEL(P)  SCHOUT        SCOUTQ
     C                   SETON                                        8290
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* IF THE OUTQ IS RESTRICTED, DON'T ALLOW USER TO CHANGE OUTQ.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSEL         ANDNE     '5'
     C     SCSEL         ANDNE     '7'
     C     SCOUTR        ANDEQ     'Y'
     C     SCHOUT        ANDNE     SCOUTQ
     C*
     C                   MOVEL(P)  SCHOUT        SCOUTQ
     C                   SETON                                        8390
     C*
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSEL         ANDEQ     '7'
     C*
     C                   EXSR      OPT7
     C*
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSEL         ANDEQ     '1'
     C*
     C                   ADD       1             COUNT
     C                   ADD       1             COUNT3
     C*
     C     COUNT         IFEQ      1
     C*
     C                   EXSR      CONTRL
     C*
     C                   ENDIF
     C*
     C     STOP          IFEQ      *OFF
     C*
     C                   EXSR      WRUPDR
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        COUNT3
     C*
     C                   ENDIF
     C*
     C                   SUB       COUNT1        COUNT1
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C*
     C                   SETON                                        8990
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C     @@RRN         CHAIN     PDE01                              99
     C*
     C                   MOVE      DECO#         KKCO#
     C                   MOVEL     DEDPCD        KKDPCD
     C                   MOVEL     *BLANKS       KKDCID
     C                   MOVEL     *BLANKS       KKDESC
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVEL(P)  SCPST1        KKDCID
     C                   MOVEL(P)  SCPST2        KKDESC
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCDCID        KKDCID
     C                   MOVEL(P)  SCDESC        KKDESC
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     *OFF          STOP
     C*
     C                   MOVEL(P)  DOOVRL        SCDOVR
     C                   MOVEL(P)  DODCID        SCDCID
     C                   MOVEL(P)  DOFOLD        SCFLDR
     C                   MOVEL(P)  DODESC        SCDESC
     C                   MOVEL(P)  DODESC        XXDESC           35
     C*
     C   21              MOVEL(P)  DODCID        SCDATA
     C   21              MOVE      XXDESC        SCDATA
     C*
     C   22              MOVEL(P)  XXDESC        SCDATA
     C   22              MOVE      DODCID        SCDATA
     C*
     C                   MOVEL(P)  DOAIFL        SCAIFL
     C                   MOVEL(P)  DOPGMN        SCPGMN
     C                   MOVEL(P)  DOOUTR        SCOUTR
     C*
     C     NORFSH        IFEQ      *OFF
     C     *INKK         OREQ      *ON
     C*
     C     DOCOPY        IFNE      *ZEROS
     C*
     C                   Z-ADD     DOCOPY        SCCOPY
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             SCCOPY
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     SCCOPY        SCHCPY
     C*** BEGIN ADD *** 09/30/98 **************************************
     C     DOOUTQ        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  DOOUTQ        SCOUTQ
     C                   MOVEL(P)  DOOUTQ        SCHOUT
     C*
     C                   ELSE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C  N99              MOVEL(P)  UPOUTQ        SCOUTQ
     C  N99              MOVEL(P)  UPOUTQ        SCHOUT
     C   99              MOVEL(P)  *BLANKS       SCOUTQ
     C   99              MOVEL(P)  *BLANKS       SCHOUT
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     *OFF          NORFSH
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDU - Move screen fields to data base fields for update.
     C*================================================================
     C     MVSCDU        BEGSR
     C*
     C                   Z-ADD     SCCOPY        D1#CPY
     C                   MOVE      SCOUTQ        D1OUTQ
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDW - Move screen fields to data base fields for write.
     C*================================================================
     C     MVSCDW        BEGSR
     C*
     C                   Z-ADD     CMCO#         D1CO#
     C                   Z-ADD     CMSUB#        D1SUB#
     C                   Z-ADD     CMFND         D1FND
     C                   Z-ADD     CMFYR         D1FYR
     C                   Z-ADD     CMCASE        D1CASE
     C*
     C                   Z-ADD     XXREQ#        D1REQ#
     C                   MOVE      SCDPCD        D1DPCD
     C*** BEGIN ADD *** 07/01/04 **************************************
     C     ##PFLR        IFNE      *BLANKS
     C     ##PFLR        CAT(P)    '/':0         D1FLDR
     C                   CAT       SCFLDR:0      D1FLDR
     C                   ELSE
     C*** END   ADD *** 07/01/04 **************************************
     C                   MOVE      SCFLDR        D1FLDR
     C*** BEGIN ADD *** 07/01/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/01/04 **************************************
     C                   MOVE      SCDCID        D1DCID
     C                   MOVEL(P)  SCDOVR        D1DOVR
     C*
     C                   Z-ADD     SCCOPY        D1#CPY
     C                   MOVE      SCOUTQ        D1OUTQ
     C                   MOVE      'A'           D1STAT
     C*
     C                   ENDSR
     C*================================================================
     C* OPT99 - Option 99 - DUMMY OPTION
     C*================================================================
     C     OPT99         BEGSR
     C                   ENDSR
     C*================================================================
     C* OPT7 - Option 7 - Display Document.
     C*================================================================
     C     OPT7          BEGSR
     C*
     C                   CALL      'SDSPDOC'
     C                   PARM      SCDCID        ##DCID           12
     C                   PARM      SCFLDR        ##FOLD           80
     C                   PARM      *BLANKS       ##KY             40
     C                   PARM      *ZEROS        ##SQ#             5
     C                   PARM      *BLANKS       ##FL03            1
     C                   PARM      *BLANKS       ##RTCD            1
     C*** BEGIN ADD *** 09/28/98 **************************************
     C                   PARM                    ##MSS
     C*** END   ADD *** 09/28/98 **************************************
     C*** BEGIN ADD *** 10/17/11 **************************************
     C                   PARM      *BLANKS       ##UNCN          255
     C                   PARM      *BLANKS       ##FSF             1
     C*** END   ADD *** 10/17/11 **************************************
     C*
     C                   SELECT
     C*
     C     ##RTCD        WHENEQ    '1'
     C*
     C                   SETON                                        7190
     C*
     C     ##RTCD        WHENEQ    '2'
     C*
     C                   SETON                                        7290
     C*
     C     ##RTCD        WHENEQ    '3'
     C*
     C                   SETON                                        7390
     C*
     C     ##RTCD        WHENEQ    '4'
     C*
     C                   SETON                                        7490
     C*
     C                   ENDSL
     C*
     C     ##RTCD        IFNE      ' '
     C*
     C                   SETON                                        050708
     C                   Z-ADD     SFRRN1        SCRRN1
     C                   MOVEL     '5'           SCSEL
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '99'          OPT99
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C     COUNT3        IFNE      *ZEROS
     C*
     C                   MOVEL     *OFF          *INKC
     C                   MOVEL     *OFF          *INKL
     C                   EXSR      SLCTRD
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        @@REQ#
     C*
     C                   ENDIF
     C*
     C                   MOVEL     *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVEL     *OFF          STOP
     C*** BEGIN ADD *** 09/28/98 **************************************
     C*
     C*** GET SYSTEM INFO
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
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C*** END   ADD *** 09/28/98 **************************************
     C*
     C                   MOVEL     *OFF          NORFSH            1
     C                   Z-ADD     *ZEROS        COUNT             2 0
     C                   Z-ADD     *ZEROS        COUNT3            2 0
     C                   Z-ADD     *ZEROS        XXREQ#
     C*
     C                   MOVEL(P)  DEDPCD        SCDPCD
     C                   MOVEL(P)  DENAME        SCDPTN
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C     @@RRN2        CHAIN     PCM01                              99
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
     C     SCL1HD        PARM      *BLANK        #1L1             78
     C     SCL2HD        PARM      *BLANK        #1L2             78
     C*** BEGIN ADD *** 07/01/04 **************************************
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##PRFX            3
     C                   PARM      *BLANKS       ##PFLR           80
     C*** END   ADD *** 07/01/04 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   Z-ADD     XXREQ#        @@REQ#
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     DOSTAT
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRUPDR - Write/Upadte data base record and after processing
     C*================================================================
     C     WRUPDR        BEGSR
     C*
     C                   Z-ADD     CMCO#         KKCO#
     C                   Z-ADD     CMSUB#        KKSUB#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C                   Z-ADD     XXREQ#        KKREQ#
     C                   MOVEL(P)  SCDPCD        K2DPCD
     C                   MOVEL(P)  SCFLDR        KKFLDR
     C                   MOVEL(P)  SCDCID        K2DCID
     C*
     C     D1KY2         CHAIN     PD101                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     D1KY2         CHAIN     PD102                              99
     C     D1KY2         CHAIN     PD103                              99
     C*
     C                   EXSR      MVSCDU
     C*
     C                   MOVEL     Q1USER        D1USER
     C                   MOVEL     Q1PGM         D1PGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         D1DT
     C*
     C                   MOVEL     DSTIME        D1TIME
     C*
     C                   UPDATE    PD101
     C                   UPDATE    PD102
     C                   UPDATE    PD103
     C*** BEGIN ADD *** 11/23/98 **************************************
     C*
     C                   CALL      'F4110R01'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      DBRRN3        ##RRN             9 0          FDD1P
     C                   PARM      @@RRN2        ##RRN2            9 0          FMCMP
     C                   PARM      SCDBRR        ##RRN3            9 0          SMDOP
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   MOVEL     '99'          SCSEL
     C                   MOVEL     *OFF          SL1FLG
     C                   MOVEL     *OFF          *INKL
     C                   EXSR      DELREC
     C                   MOVEL     *ON           STOP              1
     C                   Z-ADD     *ZEROS        COUNT3
     C*
     C                   ELSE
     C*
     C                   MOVEL     *OFF          STOP
     C*
     C                   ENDIF
     C*** END   ADD *** 11/23/98 **************************************
     C*
     C                   ELSE
     C*
     C                   CLEAR                   PD104
     C                   CLEAR                   PD105
     C                   CLEAR                   PD106
     C*
     C                   EXSR      MVSCDW
     C*
     C                   MOVEL     Q1USER        D1USER
     C                   MOVEL     Q1PGM         D1PGM
     C                   MOVEL     Q1USER        D1IUSR
     C                   MOVEL     Q1PGM         D1IPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         D1DT
     C*
     C                   MOVEL     SRDTO         D1IDT
     C*
     C                   MOVEL     DSTIME        D1TIME
     C                   MOVEL     DSTIME        D1ITIM
     C*
     C                   WRITE     PD104
     C                   WRITE     PD105
     C                   WRITE     PD106
     C*
     C                   FEOD      FDD1P
     C                   FEOD      FDD1P1
     C                   FEOD      FDD1P2
     C*
     C*** BEGIN ADD *** 11/23/98 **************************************
     C                   CALL      'F4110R01'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      DBRRN3        ##RRN             9 0          FDD1P
     C                   PARM      @@RRN2        ##RRN2            9 0          FMCMP
     C                   PARM      SCDBRR        ##RRN3            9 0          SMDOP
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   MOVEL     '99'          SCSEL
     C                   MOVEL     *OFF          SL1FLG
     C                   MOVEL     *OFF          *INKL
     C                   EXSR      DELREC
     C                   MOVEL     *ON           STOP
     C                   Z-ADD     *ZEROS        COUNT3
     C*
     C                   ELSE
     C*
     C                   MOVEL     *OFF          STOP
     C*
     C                   ENDIF
     C*** END   ADD *** 11/23/98 **************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt  Document ID    Document description               Input Copies Queue
Opt  Document description                  Document ID Input Copies Queue
** Description--------------MISN
1011=Select                 YYY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display document       YYY
1077=Document info          YYY
203F3=Exit                  NNN
205F5=Refresh               NNN
206F6=Add                   NNN
211F11=Change View          YYY
212F12=Cancel               YYY
217F17=Subset               NNN
3
