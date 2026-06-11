     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0083R - Medical Bill Authorization
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H* Description: This program will prompt a user to accept,       *
     H*              deny, or question a bill.                        *
     H*                                                               *
     H* Called from: IS215R   - Work with User Queues Index Entries   *
     H* Calls to   : W4110R27 - Additional Info Screen                *
     H*              W4110C   - Department Letter Print Job           *
     H*                                                               *
     H*****************************************************************
     H* Created on 11/21/2002 by c:62 for Project 114MDY              *
     H*****************************************************************
     H* REVISIONS:
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 08/13/14 C:296    Project: 114MNV                             *
     H*                   Move to procura completed queue.            *
     H*                                                               *
     H* 10/25/13 c:290    Support ID114 83715                         *
     H*                   Correctly populate DNIDT(Initial Date).     *
     H*                                                               *
     H*                                                               *
     H* 08/28/12 c:282    Support WA114 80639                         *
     H*                   Swap out DARTS key info with the image      *
     H*                   number.                                     *
     H*                                                               *
     H* 08/06/12 c:282    Project 114IZR                              *
     H*                   Add an image note whenever a claim is       *
     H*                   denied or approved from "Medical bill       *
     H*                   authorization screen.                       *
     H*
     F* 01/25/12 c:190    Project 114IZS                              *
     F*                   -Remark comment change on Medical Bill      *
     F*                    authorization screen.                      *
     F*                                                               *
     H* 10/04/11 C:251  PROJECT 114IAH
     H*                -Update new fields 'Dart Key' and 'Dart Sequence'
     H*
     F* 06/16/11 c:061  PROJECT 114MZP
     F*                -Add entry fields for "Code changes" and
     F*                 "Reasons".
     F*                -Write/Update "Med Bill Auth Aux" file.
     F*                                                               *
     F* 01/27/03 c:62 for Project 114MDY                              *
     F*                   -Add adequate medical reserves logic.       *
     F*                   -Add F8 and F14.                            *
     H*                                                               *
     H*****************************************************************
     FW0083F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     FWDCLL     UF A E           K DISK
     FWDCLL3    O    E           K DISK
     F                                     RENAME(PCL01:PCL02)
     F                                     INFDS(FIDS01)
     FWMDOL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F*** BEGIN ADD *** 01/27/03 **************************************
     F                                     INFDS(DBFDS)
     F*** END   ADD *** 01/27/03 **************************************
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FFADAL2    IF   E           K DISK
     FFMDIL     IF   E           K DISK
     FIMDOL1    IF   E           K DISK
     F                                     RENAME(PDO01:PDO02)
     FIMIEP     IF   E             DISK
      ***** BEG ADD ******* 06/16/11 **********************************
      *
      * Medical Bill Authorization Auxiliary
     FFAAUL1    UF A E           K DISK
      ***** END ADD ******* 06/16/11 **********************************
     F*
     F*** BEG ADD *** 01/25/12 ****************************************
     FSDCMLE    IF   E           K DISK
     F                                     RENAME(PCM01:PCM0X)
     F                                     INFDS(DBFDS1)
     F*** END   ADD *** 01/25/12 **************************************
     F*** BEG ADD *** 08/06/12 ****************************************
     FIMDNL     IF A E           K DISK
     F*** END ADD *** 08/06/12 ****************************************
     F*** Beg Add *** 08/13/14 ****************************************
     FFAEOL68   UF   E           K DISK
     F*** End Add *** 08/13/14 ****************************************

     D*
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     D*** BEGIN ADD *** 01/27/03 **************************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** BEG ADD *** 08/06/12 **************************************
     D                 DS
     D  D2DTTM                 1     14  0
     D  D2TIME                 1      6  0
     D  D2DATE                 7     14  0
      *
     D                 DS
     D  XCURDT                 1      8  0
     D  XCURYR                 1      4  0
     D  XCURMD                 5      8  0

     D                 DS
     D  XXDATE                 1     10
     D  XXMM                   1      2
     D  XXSLH1                 3      3
     D  XXDD                   4      5
     D  XXSLH2                 6      6
     D  XXYY                   7     10

     D                 DS
     D  XXTIME                 1      8
     D  XXHR                   1      2
     D  XXCOL1                 3      3
     D  XXMIN                  4      5
     D  XXCOL2                 6      6
     D  XXSEC                  7      8
     D*** END ADD *** 08/06/12 **************************************
     D*
     D                 DS
     D  $$PARM                 1    136
     D  $$CO#                  1      3
     D  $$FND                  4      6
     D  $$REQ#                 7     11
     D  $$DPCD                12     16
     D  $$DCID                17     28
     D  $$DESC                29     56
     D  $$FLDR                57    136
     D*
     D                 DS
     D  SCCLM#                 1     11
     D  SCFYR                  1      3  0
     D  XXHYPH                 4      4    INZ('-')
     D  SCCASE                 5     11  0
     D*
     D                 DS
     D  XXSS#                  1      9
     D  XXSSA                  1      3
     D  XXSSB                  4      5
     D  XXSSC                  6      9
     D*
     D                 DS
     D  SCSS#                  1     11
     D  SCSSA                  1      3
     D  SCHY1                  4      4    INZ('-')
     D  SCSSB                  5      6
     D  SCHY2                  7      7    INZ('-')
     D  SCSSC                  8     11
     D*** BEGIN ADD *** 01/27/03 **************************************
     D*  DARTS
     D                 DS                  INZ
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
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
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
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
     D*** END   ADD *** 01/27/03 **************************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*** BEGIN ADD *** 01/25/12 **************************************
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D*** End  ADD *** 01/25/12 ***************************************
     D*
     D OPTNM           C                   CONST('Medical Bill Author-
     D                                     ization')
     D OPTNM2          C                   CONST('Medical Bill Denial-
     D                                      Letter')
     D PDSNAM          C                   CONST('Progressive Data So-
     D                                     lutions, Inc.')
     D #OFSCR          C                   CONST(1)
     D COMAPI          C                   CONST('WA001R')
     D CIDX            C                   CONST('**Indexed**')
     D CRJ             C                   CONST('**Rejected**')
     D CRA             C                   CONST('**Reassigned**')
     D*** BEG ADD *** 08/06/12 ***************************************
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D*** END ADD *** 08/06/12 ***************************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D*** Beg Add *** 08/13/14 ****************************************
     D ReAssignQue     pr                  ExtPgm('IA001R')
     D IEdcId                        12A
     D PROAC1                        10A   Const
     D*** End Add *** 08/13/14 ****************************************
     D*** Beg Add *** 08/13/14 ****************************************
     D IndAudNot       s               n   inz(*off)
     D*** End Add *** 08/13/14 ****************************************
     D*

     I*** END   ADD *** 01/27/03 **************************************
     IPDO01
     I              DOIDT                       DXIDT
     IPCL01
     I              CLREQ#                      HIREQ#
     I              CLCO#                       HICO#
     I              CLFND                       HIFND
     I              CLFYR                       HIFYR
     I              CLFCLM                      HIFCLM
     I              CLEMP#                      HIEMP#
     I              CLDIV                       HIDIV
     I              CLDPCD                      HIDPCD
     I              CLFLDR                      HIFLDR
     I              CLDCID                      HIDCID
     I              CL#CPY                      HI#CPY
     I              CLSTAT                      HISTAT
     I              CLIUSR                      HIIUSR
     I              CLUSER                      HIUSER
     I              CLPGM                       HIPGM
     I              CLIPGM                      HIIPGM
     I              CLDT                        HIDT
     I              CLTIME                      HITIME
     I              CLITIM                      HIITIM
     I              CLIDT                       HIIDT

     C*===============================================================
     C* Parameter Lists
     C*===============================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@FND             3 0          Group #
     C                   PARM                    @@FYR             3 0          Year
     C                   PARM                    @@CASE            7 0          Case
     C                   PARM                    @@RRN             9 0          RRN IMIEP
     C                   PARM                    @@DESC           30            Description
     C                   PARM                    @@TITL           15            Title
     C                   PARM                    @@OUTQ           10            Out Queue
     C                   PARM                    @@RTRN            6            Return Codes
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** BEG ADD ****** 01/25/12 ****************************************
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      DBRRN         ##RRN
     C                   PARM      Q1PGM         ##PGM
     C*** END ADD ****** 01/25/12 ****************************************
     C*** BEG ADD ****** 10/25/13 ****************************************
     C     DTYMD         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDYY'       SRFFMT            8
     C                   PARM      '*YMD'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*** END ADD ****** 10/25/13 ****************************************
     C*===============================================================
     C* Key Lists
     C*===============================================================
      ***** BEG ADD ******* 06/16/11 **********************************
      *
      * Medical Bill Authorization Auxiliary
      *
     C     AUKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKMCF#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    DODCID
      *
      ***** END ADD ******* 06/16/11 **********************************
      ***** BEG ADD ******* 08/06/12 **********************************
      * DOC Notes (History) File
      *
     C     DNKY1         KLIST
     C                   KFLD                    DODCID
     C                   KFLD                    NOTSEQ            5 0
      *
     C     DNKY2         KLIST
     C                   KFLD                    DODCID
      ***** END ADD ******* 08/06/12 **********************************
     C*
     C     CLKY          KLIST
     C                   KFLD                    HICO#
     C                   KFLD                    HIFND
     C*
     C     CMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     DAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKMCF#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKPRIM
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*
     C     DOKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKDEPT
     C                   KFLD                    KKDPCD
     C                   KFLD                    KKFLDR
     C                   KFLD                    KKDCID
     C*
     C     DOKY2         KLIST
     C                   KFLD                    IEDCID
     C*
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C***BEG ADD ***** 01/25/12 **************************************
     C     DCMKY         KLIST
     C                   KFLD                    KKDKEY
     C                   KFLD                    KKSEQ#
     C***END ADD ***** 01/25/12 **************************************
     C*===============================================================
     C* Like Defines
     C*===============================================================
     C*
     C     *LIKE         DEFINE    CLREQ#        XXREQ#
     C     *LIKE         DEFINE    DODEPT        KKDEPT
     C     *LIKE         DEFINE    DODPCD        KKDPCD
     C     *LIKE         DEFINE    DOFLDR        KKFLDR
     C     *LIKE         DEFINE    DODCID        KKDCID
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C     *LIKE         DEFINE    DASUB#        KKSUB#
     C     *LIKE         DEFINE    DAFND         KKMCF#
     C     *LIKE         DEFINE    DAPRIM        KKPRIM
     C     *LIKE         DEFINE    NAKEY         KKKEY
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C***BEG ADD ***** 01/25/12 **************************************
     C     *LIKE         DEFINE    CMKEY         KKDKEY
     C     *LIKE         DEFINE    CMSEQ#        KKSEQ#
     C***END ADD ***** 01/25/12 **************************************
     C***BEG ADD ***** 08/28/12 **************************************
     C     *LIKE         DEFINE    DODCID        XXDCID
     C***END ADD ***** 08/28/12 **************************************
     C*
     C                   EXSR      MAIN
     C*
     C*****************************************************************
     C* ADDPGM - Calls Additional Information Program
     C*****************************************************************
     C     ADDPGM        BEGSR
     C*
     C* Assign fields to parms, then call additional program
     C*
     C                   MOVE      CLCO#         $$CO#
     C                   MOVE      CLFND         $$FND
     C                   MOVE      CLREQ#        $$REQ#
     C                   MOVE      CLDPCD        $$DPCD
     C                   MOVE      CLDCID        $$DCID
     C                   MOVEL(P)  'CS'          KKDEPT
     C                   MOVEL(P)  CLDPCD        KKDPCD
     C                   MOVEL(P)  CLDCID        KKDCID
     C                   MOVEL(P)  CLFLDR        KKFLDR
     C*
     C     DOKY          CHAIN     PDO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE(P)   DODESC        $$DESC
     C                   ELSE
     C                   MOVEL     *BLANKS       $$DESC
      ***** BEG ADD ******* 06/16/11 **********************************
     C                   MOVEL(P)  *BLANKS       DODCID
      ***** END ADD ******* 06/16/11 **********************************
     C                   ENDIF
     C*
     C                   MOVE      CLFLDR        $$FLDR
     C*
     C                   CALL      'W4110R27'
     C                   PARM      OPTNM2        $$PRDS           40
     C                   PARM                    $$PARM
     C                   PARM      KKDEPT        $$DEPT            5
     C                   PARM      *BLANKS       $$RTCD            1
     C*
     C     $$RTCD        IFEQ      'A'
     C     $$RTCD        OREQ      'B'
     C                   SETON                                        90
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C* ASSQUE - Assign Queue
     C*****************************************************************
     C     ASSQUE        BEGSR
     C*
     C                   Z-ADD     IESDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         $$SDT
     C*
     C                   MOVE      IESTM         $$STM
     C*
     C     DOKY2         CHAIN     PDO02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DOFLR         XXFLR             8
     C                   MOVEL     DOSFLR        XXSFLR           12
     C     XXFLR         CAT       '/'           XX9A              9
     C     XX9A          CAT(P)    XXSFLR        $$PTH
     C                   ELSE
     C                   MOVEL     *BLANKS       $$PTH
      ***** BEG ADD ******* 06/16/11 **********************************
     C                   MOVEL(P)  *BLANKS       DODCID
      ***** END ADD ******* 06/16/11 **********************************
     C                   ENDIF
     C*
     C                   CALL      'IS115R'
     C                   PARM                    $$SDT             8
     C                   PARM                    $$STM             6
     C                   PARM      IEDOCT        $$DOCT            8
     C                   PARM      IEDCID        $$DCID           12
     C                   PARM      IEFL01        $$FL01            1
     C                   PARM                    $$FLR             8
     C                   PARM                    $$SFLR           12
     C                   PARM                    $$PTH            63
     C                   PARM      IEQID         $$QID            10
     C                   PARM      @@DESC        $$DSC            30
     C                   PARM      *BLANKS       $$RTC1            1
     C*
     C     $$RTC1        IFEQ      'A'
     C     $$RTC1        OREQ      'B'
     C                   SETON                                        90
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       @@RTRN
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C* CNTROL - Comes up with a unique request number
     C*****************************************************************
     C     CNTROL        BEGSR
     C*
     C                   Z-ADD     *HIVAL        HICO#
     C                   Z-ADD     *HIVAL        HIFND
     C*
     C     CLKY          CHAIN     PCL01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     HIREQ#        IFLT      99999
     C     HIREQ#        ADD       1             HIREQ#
     C                   ELSE
     C                   Z-ADD     1             HIREQ#
     C                   ENDIF
     C*
     C*  Update control record
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HIDT
     C*
     C                   Z-ADD     DSTIME        HITIME
     C                   Z-ADD     DSTIME        HIITIM
     C                   MOVEL     Q1USER        HIIUSR
     C                   MOVEL     Q1USER        HIUSER
     C                   MOVEL     Q1PGM         HIIPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HIIDT
     C*
     C                   UPDATE    PCL01
     C*
     C                   ELSE
     C*
     C*  Write the control record for department requests
     C*
     C                   Z-ADD     1             HIREQ#
     C                   Z-ADD     *HIVAL        HICO#
     C                   Z-ADD     *HIVAL        HIFND
     C                   Z-ADD     *HIVAL        HIFYR
     C                   Z-ADD     *HIVAL        HIFCLM
     C                   Z-ADD     *HIVAL        HIEMP#
     C                   Z-ADD     *HIVAL        HIDIV
     C                   MOVE      *BLANKS       HIDPCD
     C                   MOVE      *BLANKS       HIFLDR
     C                   MOVE      *BLANKS       HIDCID
     C                   Z-ADD     *HIVAL        HI#CPY
     C                   MOVE      'A'           HISTAT
     C                   MOVE      Q1USER        HIIUSR
     C                   MOVE      Q1PGM         HIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HIDT
     C*
     C                   Z-ADD     DSTIME        HITIME
     C                   MOVE      Q1USER        HIIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HIIDT
     C*
     C                   WRITE     PCL01
     C                   ENDIF
     C*
     C                   Z-ADD     HIREQ#        XXREQ#
     C*
     C                   ENDSR
     C*===============================================================
     C* ERRCK1 - Screen edits
     C*===============================================================
     C     ERRCK1        BEGSR
     C*** BEGIN ADD *** 01/27/03 **************************************
     C*
     C***** Beg Add ***** 08/13/14 ************************************
     C** check only when bill is not being Audited. When Audited, these
     C** option are not shown to user so do not validate
     C                   IF        IndAud = *off
     C***** End Add ***** 08/13/14 ************************************
     C*
     C* VALID VALUES ARE Y OR N
     C*

     C     *IN90         IFEQ      *OFF
     C     SCRFLG        ANDNE     'Y'
     C     SCRFLG        ANDNE     'N'
     C                   SETON                                        7490
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRFLG        ANDEQ     'N'
     C*
     C                   CALL      'W0070R12'
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      ' '           ##CONT            1
     C*
     C     CMKY          CHAIN     PCM01                              99
     C                   EXSR      GETRSV
     C                   SETON                                        90
     C                   ENDIF
     C*** END   ADD *** 01/27/03 **************************************
     C*
     C* MUST APPROVED, DENIED OR QUESTIONED
     C*
     C     *IN90         IFEQ      *OFF
     C     SCADQ         IFNE      'A'
     C     SCADQ         ANDNE     'D'
     C     SCADQ         ANDNE     'Q'
     C                   SETON                                        7190
     C                   ENDIF
     C                   ENDIF
     C***** Beg Add ***** 08/13/14 ************************************
     C                   ENDIF
     C***** End Add ***** 08/13/14 ************************************
     C*
     C* ONLY ADJUSTERS CAN DELETE
     C*
     C     *IN90         IFEQ      *OFF
     C     SCADQ         IFEQ      'D'
     C     @@TITL        ANDNE     'ADJUSTER'
     C                   SETON                                        7290
     C                   ENDIF
     C                   ENDIF
      ***** BEG ADD ******* 06/16/11 **********************************
      *
      * SCCCOD must be 'Y' if code OR reason entered.
      *
     C  N90SCCCOD        IFEQ      ' '
      *
     C     SCFRC1        IFNE      *BLANK
     C     SCFRC2        ORNE      *BLANK
     C     SCREAS        ORNE      *BLANK
     C                   SETON                                        7590
     C                   ENDIF
      *
     C                   ENDIF
      *
      * CODE or REASON must be entered if SCCCOD = Y
      *
     C  N90SCCCOD        IFEQ      'Y'
     C     SCFRC1        ANDEQ     *BLANK
     C     SCFRC2        ANDEQ     *BLANK
     C     SCREAS        ANDEQ     *BLANK
     C                   SETON                                        7690
     C                   ENDIF
      *
      * SCTOC1 must be entered if SCFRC1 entered
      *
     C  N90SCFRC1        IFNE      *BLANK
     C     SCTOC1        ANDEQ     *BLANK
     C                   SETON                                        7790
     C                   ENDIF
      *
      * SCTOC2 must be entered if SCFRC2 entered
      *
     C  N90SCFRC2        IFNE      *BLANK
     C     SCTOC2        ANDEQ     *BLANK
     C                   SETON                                        7890
     C                   ENDIF
      *
      * SCFRC1 must be entered if SCTOC1 entered
      *
     C  N90SCTOC1        IFNE      *BLANK
     C     SCFRC1        ANDEQ     *BLANK
     C                   SETON                                        7990
     C                   ENDIF
      *
      * SCFRC2 must be entered if SCTOC2 entered
      *
     C  N90SCTOC2        IFNE      *BLANK
     C     SCFRC2        ANDEQ     *BLANK
     C                   SETON                                        8090
     C                   ENDIF
      *
      ***** END ADD ******* 06/16/11 **********************************
     C*
     C*  We have passed all edits.
     C*
     C                   ENDSR                                                  ERRCK1
      *
      *
     C*================================================================
     C*  FINISH - Final processing for file update/display
     C*================================================================
     C     FINISH        BEGSR
     C*
      ***** BEG ADD ******* 08/06/12 **********************************
     C                   MOVE      *BLANKS       XXSTAT            8
     C                   SELECT
     C     SCADQ         WHENEQ    'A'
     C                   MOVEL     'Approved'    XXSTAT
     C     SCADQ         WHENEQ    'D'
     C                   MOVEL     'Denied'      XXSTAT
     C                   OTHER
     C                   MOVE      *BLANKS       XXSTAT
     C                   ENDSL
      *
     C     XXSTAT        IFNE      ' '
     C                   EXSR      WRTNOT
     C                   ENDIF
      ***** END ADD ******* 08/06/12 **********************************
     C     SCADQ         IFEQ      'D'
     C                   EXSR      WRTUPD
     C                   EXSR      ADDPGM
     C                   EXSR      SUBMIT
     C                   ELSE
     C                   EXSR      ASSQUE
     C                   ENDIF
      ***** BEG ADD ******* 06/16/11 **********************************
      *
     C     AUKY          CHAIN     PAU01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      DODCID        AUIDOC
     C                   MOVE      SCCCOD        AUCCOD
     C                   MOVEL(P)  SCFRC1        AUFRC1
     C                   MOVEL(P)  SCTOC1        AUTOC1
     C                   MOVEL(P)  SCFRC2        AUFRC2
     C                   MOVEL(P)  SCTOC2        AUTOC2
     C                   MOVEL(P)  SCREAS        AUREAS
      *
     C                   MOVEL     Q1USER        AUUSER
     C                   MOVEL     Q1PGM         AUPGM
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        AUTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUDT
      *
     C                   UPDATE    PAU01
      *
     C                   ELSE
      *
     C                   CLEAR                   PAU01
      *
     C                   MOVE      ##CO#         AUCO#
     C                   MOVE      KKSUB#        AUSUB#
     C                   MOVE      KKMCF#        AUFND
     C                   MOVE      KKFYR         AUFYR
     C                   MOVE      KKCASE        AUCASE
     C                   MOVE      DODCID        AUIDOC
     C                   MOVE      SCCCOD        AUCCOD
     C                   MOVEL(P)  SCFRC1        AUFRC1
     C                   MOVEL(P)  SCTOC1        AUTOC1
     C                   MOVEL(P)  SCFRC2        AUFRC2
     C                   MOVEL(P)  SCTOC2        AUTOC2
     C                   MOVEL(P)  SCREAS        AUREAS
      *
     C                   MOVEL     Q1USER        AUUSER
     C                   MOVEL     Q1USER        AUIUSR
     C                   MOVEL     Q1PGM         AUPGM
     C                   MOVEL     Q1PGM         AUIPGM
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSTIME        AUTIME
     C                   Z-ADD     DSTIME        AUITIM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUDT
     C                   MOVEL     SRDTO         AUIDT
      *
     C                   WRITE     PAU01
      *
     C                   ENDIF
      *
      ***** END ADD ******* 06/16/11 **********************************
     C*
     C***** BEG ADD ******* 01/25/12 **********************************
     C     SCCOMM        IFNE      *BLANKS
     C                   MOVE      DBRRN1        ##RRN1            9 0
     C                   MOVE      'W0083R'      ##PGM1            1 0
     C                   CALL      'DA704R'
     C                   PARM                    ##RRN1
     C                   PARM                    SCCOMM
     C                   PARM                    ##PGM1
     C                   ENDIF
     C*** BEGIN CHG *** 01/25/12 **************************************
     C***** Beg Add ***** 08/13/14 ************************************
      /Free

        //Update audit flag to 'P' indicating audit is completed.
        If IEdcId <> *blanks;
        Chain IEdcId Peo01;
          If %found(Faeol68) and EOpauf = 'A';

             EOpauf = 'P';
             EOuser = Q1user;
             EOpgm  = Q1pgm;
             EOtime = %Dec(%Time():*hms);
             EOdt   = %Dec(%Date():*cymd);
             Update Peo01;

             //Move bill to Procura audit completed queue.
             ReAssignQue(IEdcId : 'PROAC');
          EndIf;
        EndIf;
      /End-Free
     C***** End Add ***** 08/13/14 ************************************
     C                   ENDSR                                                  FINISH
      *
      *
     C*================================================================
     C*  GETCOF - Gets company and fund names if possible; otherwise
     C*           get PDS name for company name
     C*================================================================
     C     GETCOF        BEGSR
     C*
     C                   CALL      COMAPI        COMPL                  99
B01  C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  PDSNAM        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        ##CONM
E01  C                   ENDIF
     C                   MOVEL     ##CONM        SCCONM
     C*
     C                   ENDSR                                                  GETCOF
      *
      *
     C*================================================================
     C*  GETPGT - Get program title for display heading
     C*================================================================
     C     GETPGT        BEGSR
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 01/27/03 **************************************
     C*****************************************************************
     C* GETRSV - Get the remaining medical reserves amount.
     C*****************************************************************
     C     GETRSV        BEGSR
     C*
     C                   Z-ADD     *ZEROS        SCRRXX
     C                   DO        10            I                 3 0
     C                   MOVEL(P)  RT(I)         KKKEY
     C     TBKY          CHAIN     PRT01                              99
     C     *IN99         IFEQ      *OFF
     C     RTTYPE        ANDEQ     'M'
     C                   Z-ADD     RR(I)         SCRRXX
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDSR
     C*** END   ADD *** 01/27/03 **************************************
     C*================================================================
     C*  KGDO - F7=Image Notes
     C*================================================================
     C     KGDO          BEGSR
     C*
     C                   CALL      'IS016R'
     C                   PARM      IEDCID        $$DCID           12
     C                   PARM      'IS215R'      $$PNAM           10
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 01/27/03 **************************************
     C*===============================================================
     C* KHDO - SUBROUTINE FOR F8=CLAIMS
     C*===============================================================
     C     KHDO          BEGSR
     C*
     C*****              CALL      'W0070R'                             99
     C*****              PARM      '1'           ##CALL            1
     C*****              PARM      '0'           ##KC              1
     C*****              PARM      '0'           ##KL              1
     C*****              PARM      'C'           ##MODE            1
     C*****              PARM      DBRRN         ##RRN             9 0
     C*****              PARM      *BLANKS       ##SORT            2
     C                   Z-ADD     DBRRN         ##RRN             9 0
     C                   MOVEL     ##RRN         ##RRN2            9
     C                   CALL      'STDRVPGM'
     C                   PARM                    ##RRN2
     C                   PARM      'C'           ##MODE            1
     C                   PARM      'WTCLMSUM'    ##Next           10
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR                                                  KNDO
     C*** BEGIN ADD *** 01/25/12 **************************************
     C*================================================================
     C*  KIDO - F9
     C*================================================================
     C     KIDO          BEGSR
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C*****              CALL      'W0070RX1'    ACDPL1                 99
     C                   Z-ADD     DBRRN         ##RRN
     C                   MOVEL     ##RRN         ##RRN2
     C                   CALL      'STDRVPGM'
     C                   PARM                    ##RRN2
     C                   PARM                    ##MODE            1
     C                   PARM      'WTCLMSUM'    ##Next           10
     C*
     C                   ENDSR                                                  KIDO
     C*** END ADD ****** 01/25/12 **********************************************
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14 = DARTS
     C*===============================================================
     C     KNDO          BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      CMCO#         QCCO#
     C                   MOVE      CMFND         QCFND
     C                   MOVE      CMFYR         QCFYR
     C                   MOVE      CMCASE        QCCASE
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0070R'      ##PGM            10
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR                                                  KNDO
     C*** END   ADD *** 01/27/03 **************************************
     C*===============================================================
     C* MAIN - Main Processing
     C*===============================================================
     C     MAIN          BEGSR
     C*
     C                   MOVEL     Q1USER        SCUSER           10
     C*
     C                   EXSR      GETCOF
     C                   EXSR      GETPGT
     C                   EXSR      PGMSTR
     C*
B01  C     *IN90         DOUEQ     *OFF
     C     *INKC         OREQ      *ON
     C                   EXFMT     SCR01
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   SETOFF                                       90
B02  C     *INKC         IFEQ      *OFF
     C     *INKG         CASEQ     *ON           KGDO
     C*** BEGIN ADD *** 01/27/03 **************************************
     C     *INKH         CASEQ     *ON           KHDO
     C*** BEGIN ADD *** 01/25/12 **************************************
     C     *INKI         CASEQ     *ON           KIDO
     C*** END  ADD *** 01/25/12 ***************************************
     C     *INKN         CASEQ     *ON           KNDO
     C*** END   ADD *** 01/27/03 **************************************
     C                   ENDCS
B03  C     *IN90         IFEQ      *OFF
     C                   EXSR      ERRCK1
     C*
     C***** Beg Add ***** 08/13/14 ************************************
     C** If this is a bill being Audited, request for note
     C                   If        *in90 = *off
     C                             and IndAud = *on
     C                             and IndAudNot = *on
     C                   Exsr      KGDO
     C                   eval      IndAudNot = *off
     C                   ENDIF
     C***** End Add ***** 08/13/14 ************************************
B04  C     *IN90         IFEQ      *OFF
     C                   EXSR      FINISH
E04  C                   ENDIF
E03  C                   ENDIF
E02  C                   ENDIF
E01  C                   ENDDO
     C*
     C                   EXSR      PGMEND
     C*
     C                   ENDSR                                                  MAIN
     C*===============================================================
     C* PGMEND - Ends Program
     C*===============================================================
     C     PGMEND        BEGSR
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVEL     *BLANKS       @@RTRN
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C                   RETURN
     C*
     C                   ENDSR
     C*===============================================================
     C* PGMSTR - Starts Program
     C*===============================================================
     C     PGMSTR        BEGSR
     C*
     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C*
     C* Let's get the WCMC Subscriber/Group
     C*
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      KKFND         #1FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       #1RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1FND             3
     C                   PARM      *BLANKS       #1SUB#            5
     C                   PARM      *BLANKS       #1MCF#            3
     C*
     C                   MOVE      #1SUB#        KKSUB#
     C                   MOVE      #1MCF#        KKMCF#
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
     C                   Z-ADD     CMEMP#        SCEMP#
     C                   Z-ADD     CMDIV         SCDIV
     C                   Z-ADD     CMEMP#        KKEMP#
     C                   Z-ADD     CMDIV         KKDIV
     C                   MOVEL(P)  CMNATR        SCNATR
     C                   MOVEL(P)  CMPART        SCPART
     C                   MOVEL(P)  CMCAUS        SCCAUS
     C                   MOVEL(P)  CMACL#        SCACL#
     C                   MOVEL(P)  CMSS#         XXSS#
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVEL(P)  XXSSA         SCSSA
     C*****              MOVEL(P)  XXSSB         SCSSB
     C*****              MOVEL(P)  XXSSC         SCSSC
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-Free
      *****  End   Chg  ***  06/09/20  **********************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C     CMLSN         CAT(P)    ',':0         SCCNAM
     C                   CAT       CMFSN:1       SCCNAM
     C                   CAT       CMMID:1       SCCNAM
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        SCGNAM
     C                   ELSE
     C                   MOVEL     *BLANKS       SCGNAM
     C                   ENDIF
     C*
     C     EMKY          CHAIN     PEM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EMNAME        SCENAM
     C                   ELSE
     C                   MOVEL     *BLANKS       SCENAM
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMNATR        KKKEY
     C     TBKY          CHAIN     PNA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  NADESC        SCNATD
     C                   ELSE
     C                   MOVEL     *BLANKS       SCNATD
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMPART        KKKEY
     C     TBKY          CHAIN     PPA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PADESC        SCPARD
     C                   ELSE
     C                   MOVEL     *BLANKS       SCPARD
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMCAUS        KKKEY
     C     TBKY          CHAIN     PCA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CADESC        SCCAUD
     C                   ELSE
     C                   MOVEL     *BLANKS       SCCAUD
     C                   ENDIF
     C*
     C                   MOVEL(P)  'Y'           KKPRIM
     C     DAKY          CHAIN     PDA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DAICD9        SCICD9
     C                   MOVEL(P)  DAICD9        KKICD9
     C                   MOVEL(P)  DALOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCICDD
     C                   ELSE
     C                   MOVEL     *BLANKS       SCICDD
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       SCICD9
     C                   MOVEL     *BLANKS       SCICDD
     C                   ENDIF
     C*
     C     @@RRN         CHAIN     PIE01                              99
     C*
     C*** BEGIN ADD *** 01/27/03 **************************************
     C*
     C* GET THE MEDICAL REMAINING RESERVE AMOUNT
     C*
     C                   EXSR      GETRSV
     C                   MOVEL     *BLANKS       SCRFLG
     C*** END   ADD *** 01/27/03 **************************************
     C                   MOVEL     *BLANKS       SCADQ
      ***** BEG ADD ******* 06/16/11 **********************************
     C                   MOVEL     *BLANKS       SCCCOD
     C                   MOVEL     *BLANKS       SCFRC1
     C                   MOVEL     *BLANKS       SCTOC1
     C                   MOVEL     *BLANKS       SCFRC2
     C                   MOVEL     *BLANKS       SCTOC2
     C                   MOVEL     *BLANKS       SCREAS
      ***** END ADD ******* 06/16/11 **********************************
     C*
     C***** BEG ADD ******* 01/25/12 **********************************
     C                   MOVEL     *BLANKS       SCCOMM
     C*
     C     DOKY2         CHAIN     PDO02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      DODSQ#        KKSEQ#
     C                   MOVE      DODKEY        KKDKEY
     C                   ENDIF
     C*
     C     DCMKY         CHAIN     PCM0X                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     CMCOMM        SCCOMM
     C                   MOVE      *ON           *IN21
     C                   ELSE
     C                   MOVE      *OFF          *IN21
     C                   ENDIF
     C*
     C***** END ADD ******* 01/25/12 **********************************
     C***** Beg Add ******* 08/13/14 **********************************
      /Free
        indAud = *Off;
        IndAudNot = *off;
        If IEdcId <> *blanks;
           Chain IEdcId Peo01;
              If %found(Faeol68) and EOpauf = 'A';
                 indAud = *On;
                 IndAudNot = *on;
             EndIf;
        EndIf;
      /End-Free
     C***** End Add ******* 08/13/14 **********************************
     C                   ENDSR                                                  PGMSTR
      *
      *
     C*****************************************************************
     C* SUBMIT - Submits the letter process
     C*****************************************************************
     C     SUBMIT        BEGSR
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C*   Assign parm fields, then call submitter CLP
     C*
     C                   MOVE      ##CO#         $$CO#
     C                   MOVE      SCFND         $$FND
     C                   MOVE      XXREQ#        $$REQ#
     C*
     C                   CALL      'W4110C'
     C                   PARM                    $$CO#
     C                   PARM                    $$FND
     C                   PARM                    $$REQ#
     C                   PARM      'CLAIM'       $$DPCD
     C                   PARM      'CLAIMDL'     $$FLDR
     C                   PARM      'CLDENY'      $$DCID
     C*
     C                   MOVEL     'REMOVE'      @@RTRN
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C* WRTUPD - Writes WCMS Department Letter Record
     C*****************************************************************
     C     WRTUPD        BEGSR
     C*
     C* Call CNTROL to obtain a 'Request Number'.
     C*
     C                   EXSR      CNTROL
     C*
     C* If this is a new request, Initialize workfile fields.
     C*
     C                   CLEAR                   PCL02
     C*
     C                   Z-ADD     ##CO#         CLCO#
     C                   Z-ADD     SCFND         CLFND
     C                   Z-ADD     SCFYR         CLFYR
     C                   Z-ADD     SCCASE        CLFCLM
     C                   Z-ADD     XXREQ#        CLREQ#
     C                   Z-ADD     SCEMP#        CLEMP#
     C                   Z-ADD     SCDIV         CLDIV
     C*
     C                   MOVEL(P)  'CLAIM'       CLDPCD
     C                   MOVEL(P)  'CLAIMDL'     CLFLDR
     C                   MOVEL(P)  'CLDENY'      CLDCID
     C                   Z-ADD     1             CL#CPY
     C                   MOVEL(P)  @@OUTQ        CLOUTQ
     C*
     C                   MOVE      'A'           CLSTAT
     C                   MOVEL     Q1USER        CLIUSR
     C                   MOVEL     Q1PGM         CLIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CLIDT
     C*
     C                   Z-ADD     DSTIME        CLITIM
     C*
     C*** BEG ADD *** 10/04/11 *************************************
     C     DOKY2         CHAIN     PDO02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE(P)   DODKEY        CLDKEY
     C                   Z-ADD     DODSQ#        CLDSQ#
     C                   ENDIF
     C*** END ADD *** 10/04/11 *************************************
     C                   WRITE     PCL02
     C*
     C                   ENDSR
     C*** BEG ADD *** 08/06/12 *************************************
     C*===============================================================
     C* WRTNOT - CREATE IMAGE NOTE
     C*===============================================================
     C     WRTNOT        BEGSR
      *
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        XCURYR
     C                   MOVEL     DSDATE        XCURMD
      *
     C                   Z-ADD     *HIVAL        NOTSEQ
     C     DNKY1         SETGT     PDN01
     C     DNKY2         READPE    PDN01                                  60
     C     *IN60         IFEQ      *OFF
     C     1             ADD       DNASQ#        NOTSEQ            5 0
     C                   ELSE
     C                   Z-ADD     1             NOTSEQ
     C                   ENDIF
      *
     C                   MOVEL     DODCID        DNDCID
     C                   MOVE      NOTSEQ        DNASQ#
     C                   MOVE      XCURDT        DNCDAT
      *
     C                   MOVEL     Q1PGM         DNPNAM
     C                   MOVEL     XCURDT        DNTRDT
     C                   MOVE      DSTIME        DNTRTM
      *
      * SECURITY VALUES NOT CURRENTLY USED
     C                   MOVE      *ZEROS        DNSECL
     C                   MOVEL     *BLANKS       DNSBYL
      *
      * PROTECT THIS RCD, AS IT IS A HISTORY RECORD - "H" - HISTORY
     C                   MOVEL     'H'           DNPRFL
      *
     C                   MOVEL     *BLANKS       DNSTAT
     C                   MOVEL     Q1USER        DNIUSR
     C                   MOVEL     Q1PGM         DNIPGM
     C                   TIME                    D2DTTM
     C                   MOVE      D2TIME        DNITIM
     C*** BEG CHG ****** 10/25/13 ****************************************
     C**** D2DATE        MULT      10000.01      DNIDT
     C                   Z-ADD     D2DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYMD
     C                   MOVEL     SRDTO         DNIDT
     C*** END CHG ****** 10/25/13 ****************************************
      *
     C                   MOVEL     *BLANKS       DNLUSR
     C                   MOVEL     *BLANKS       DNLPGM
     C                   MOVE      *ZEROS        DNLTIM
     C                   MOVE      *ZEROS        DNLDT
      *
      * FORMAT THE COMMENT
      *
     C                   MOVEL     DSDATE        XXMM
     C                   MOVE      DSDATE        XXYY
     C                   MOVE      DSDATE        XXCHR8            8
     C     2             SUBST     XXCHR8:3      XXDD
      *
     C                   MOVEL     DSTIME        XXHR
     C                   MOVE      DSTIME        XXSEC
     C                   MOVE      DSTIME        XXCHR6            6
     C     2             SUBST     XXCHR6:3      XXMIN
      *
     C                   MOVEL     '/'           XXSLH1
     C                   MOVEL     '/'           XXSLH2
     C                   MOVEL     ':'           XXCOL1
     C                   MOVEL     ':'           XXCOL2
      *
     C     LO:UP         XLATE     XXSTAT        ##STAT            8
     C                   MOVEL     *BLANKS       XXCM75           75
     C                   MOVEL     *BLANKS       DNCM75           75
      *
     C                   MOVEL     'Date:'       XXCM75
     C     XXCM75        CAT       XXDATE:1      XXCM75
     C     XXCM75        CAT       'Time:':1     XXCM75
     C     XXCM75        CAT       XXTIME:1      XXCM75
     C     XXCM75        CAT       '**':5        XXCM75
     C     XXCM75        CAT       ##STAT:1      XXCM75
     C     XXCM75        CAT       '**':1        XXCM75
      *
     C                   MOVEL     XXCM75        DNCM75
     C                   WRITE     PDN01
      *
     C                   ADD       1             NOTSEQ
      *
     C                   MOVEL     *BLANKS       XXCM75
     C                   MOVEL     *BLANKS       DNCM75
     C                   MOVE      NOTSEQ        DNASQ#
      *
     C*** BEG CHG *** 08/28/12 *************************************
     C*****                MOVEL'Claim:'  XXCM75
     C*****      XXCM75    CAT  KKDKEY:1  XXCM75
     C                   MOVEL     *BLANKS       XXDCID
     C                   MOVEL     DODCID        XXDCID
     C                   MOVEL     'Image:'      XXCM75
     C     XXCM75        CAT       XXDCID:1      XXCM75
     C*** END CHG *** 08/28/12 *************************************
     C     XXCM75        CAT       'has':1       XXCM75
     C     XXCM75        CAT       'been':1      XXCM75
     C     XXCM75        CAT       XXSTAT:1      XXCM75
     C     XXCM75        CAT       'by':1        XXCM75
     C     XXCM75        CAT       'User:':1     XXCM75
     C     XXCM75        CAT       Q1USER:1      XXCM75
      *
     C                   MOVEL     XXCM75        DNCM75
     C                   WRITE     PDN01
      *
     C                   ENDSR
     C*** END ADD *** 08/06/12 *************************************
