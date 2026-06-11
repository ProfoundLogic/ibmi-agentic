     hcopyright('(c) 2020 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*                                                               *
     H*  F8040R - Upload Bills - Selection                            *
     H*****************************************************************
     H*                                                               *
     H*  Called from: F8040C1 - Upload Bills - Controller             *
     H*  Calls to   : F8040C  - Upload Bills - Submitter              *
     H*                                                               *
     H*****************************************************************
     H*  Created on 04/20/2004 by c:62 for project 51MFN              *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H* 05/11/20 C:212   Project: MC437                               *
     H*                  Redesign per CS standards                    *
     H*                  - Call to new CS standard program and return *
     H*                                                               *
     H* 12/23/13 C:296   PROJECT  129MSD                              *
     H*                  BACK TO BASE INITIATIVE-MANAGED CARE -       *
     H*                  Add a parm to indicate whether               *
     H*                  diaries are created (@@CMPY).                *
     H*                                                               *
     H* 08/22/13 C:279  Project: 114WJC                               *
     H*                 Modified to do not list the documents         *
     H*                 starting with 'O'.                            *
     H*                                                               *
     H*****************************************************************
     H*  Indicators used:                                             *
     H*                                                               *
     H*****************************************************************
     FF8040F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     FFAU9L1    IF A E           K DISK
     F                                     INFDS(DBFDS2)
     FFAU9L2    IF   E           K DISK
     F                                     RENAME(PU901:PU902)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFXU0P     IF   E             DISK
     F                                     RENAME(QWHFDML:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFXU0L     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(QWHFDML:DBRCDF)
     F*
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D DATA            S             70    DIM(1)
     D DTA             S              1    DIM(70)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     d***** Beg Add *** 05/11/20 **************************************
     d PBUpldProc2     pr                  extpgm('MTPBULP2')
     d ppCall                         1
     d ppRtcd                         1
     d ppCmpy                         1
     d***** End Add *** 05/11/20 **************************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Bill Upload -
     D                                     Process : Part 2')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('XXXXXXXX')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('XXXXXX')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(14)
     D PROC            C                   CONST('*Submitted*')
     D MBRNAM          C                   CONST('FPRVP     ')
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  KKMBR#                 1     10
     D  YYJUL                  2      8  0
     D  B                      1      1
     D  YYYEAR                 2      5  0
     D  YYDAYS                 6      8  0
     D  SEQ#                   9     10  0
     D*
     D                 DS
     D  U9MBR#                 1     10
     D  YYJUL2                 2      8  0
     D  XXSEQ#                 9     10  0

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RTCD            1
     C***** Beg Add ***** 12/23/13 ************************************
     C                   PARM                    @@CMPY            1
     C***** End Add ***** 12/23/13 ************************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTJUL         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDYY'       SRFFMT            8
     C                   PARM      '*JUL'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKNAME
     C*
     C     U9KY          KLIST
     C                   KFLD                    KKNAME
     C*
     C     U9KY2         KLIST
     C                   KFLD                    KKMBR#
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    MLNAME        KKNAME
     C     *LIKE         DEFINE    SCDATA        HDNAME
     C*
     c***** Beg Add *** 05/11/20 **************************************
     c/free
        Callp PBUpldProc2(@@call:@@rtcd:@@cmpy);
        *inlr = *on;
        Return;
      /end-free
     c***** End Add *** 05/11/20 **************************************
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* BLDMBR - Build Member Number
     C*================================================================
     C     BLDMBR        BEGSR
     C*
     C                   Z-ADD     *ZEROS        YYJUL2
     C                   Z-ADD     *ZEROS        XXSEQ#
     C                   MOVEL(P)  'B'           B
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTJUL
     C                   MOVEL     SRDTO         YYJULI            5 0
     C*
     C                   Z-ADD     DSDATE        YYYEAR
     C                   Z-ADD     YYJULI        YYDAYS
     C                   Z-ADD     99            SEQ#
     C*
     C     U9KY2         SETGT     PU902
     C                   READP     PU902                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     YYJUL         ANDEQ     YYJUL2
     C     XXSEQ#        IFEQ      99
     C                   Z-ADD     0             SEQ#
     C                   ELSE
     C     XXSEQ#        ADD       1             SEQ#
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     0             SEQ#
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   SELECT
     C*
     C     SCSEL         WHENEQ    '1'
     C                   MOVEL     '99'          SCSEL
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVEL(P)  *BLANKS       KKNAME
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCNAME        KKNAME
     C*
     C                   ENDSR
     C*================================================================
     C* OPT99 - Option 99 (SELECT FILE)
     C*================================================================
     C     OPT99         BEGSR
     C*
     C                   MOVE      SCDBR2        #1RRN
     C*
     C                   CALL      'F8040C'
     C                   PARM                    #1RRN             9
     C                   PARM      SCNAME        ##NAME           10
     C                   PARM      SCMBR#        ##MBR#           10
     C***** Beg Add ***** 12/23/13 ************************************
     C                   PARM                    @@CMPY
     C***** End Add ***** 12/23/13 ************************************
     C*
     C                   Z-ADD     SFRRN1        SCSF1R
     C                   MOVEL(P)  '4'           SCSEL
     C                   MOVEL(P)  SCNAME        HDNAME
     C*
     C     SCSF1R        CHAIN     SCR01S                             99
     C     *IN99         IFEQ      *OFF
     C                   CLEAR                   SCR01S
     C                   EXSR      SF1UPD
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   SCDATA
     C*
     C                   TIME                    DSDTTM
     C*
     C     U9KY          CHAIN     PU901                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PU901
     C*
     C                   EXSR      BLDMBR
     C*
     C                   MOVEL(P)  MLNAME        U9NAME
     C                   MOVEL(P)  KKMBR#        U9MBR#
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         U9ULDT
     C*
     C                   Z-ADD     DSTIME        U9ULTM
     C                   Z-ADD     *ZEROS        U9CVDT
     C                   Z-ADD     *ZEROS        U9CVTM
     C                   Z-ADD     *ZEROS        U9PRDT
     C                   Z-ADD     *ZEROS        U9PRTM
     C                   MOVE      'A'           U9STAT
     C                   Z-ADD     *ZEROS        U9TCNT
     C                   Z-ADD     *ZEROS        U9DCNT
     C                   Z-ADD     *ZEROS        U9CCNT
     C                   Z-ADD     *ZEROS        U9RCNT
     C                   Z-ADD     *ZEROS        U9UCNT
     C*
     C                   MOVEL     Q1PGM         U9PGM
     C                   MOVEL     Q1USER        U9USER
     C                   Z-ADD     U9ULDT        U9DT
     C                   Z-ADD     DSTIME        U9TIME
     C*
     C                   MOVEL     Q1PGM         U9IPGM
     C                   MOVEL     Q1USER        U9IUSR
     C                   Z-ADD     U9ULDT        U9IDT
     C                   Z-ADD     DSTIME        U9ITIM
     C*
     C                   WRITE     PU901
     C                   ENDIF
     C*
     C                   Z-ADD     DBRRN2        SCDBR2
     C*
     C                   MOVEL(P)  *BLANKS       SCPROC
     C                   MOVEL(P)  U9NAME        SCNAME
     C                   MOVEL(P)  U9MBR#        SCMBR#
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     SCNAME        DTA(Y)
     C                   MOVEA     DTA           SCDATA
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '99'          OPT99
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@RTCD        IFNE      *BLANK
     C                   SETON                                        8090
     C                   ENDIF
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPROD
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21              READ      DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21              READP     DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY1          SETGT     DBRCDF                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21KEY1          SETLL     DBRCDF                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  HDNAME        SCDATA
     C                   MOVEL(P)  PROC          SCPROC
     C*
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     MLNAME        IFEQ      MBRNAM
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C****** Beg Add ****** 08/22/13 *************************************
     C     1             SUBST     MLNAME:1      @@INTCHAR         1
     C     @@INTCHAR     IFEQ      'O'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C****** End Add ****** 08/22/13 *************************************
     C                   MOVEL     MLNAME        KKNAME
     C*
     C     U9KY          CHAIN     PU901                              99
     C     *IN99         IFEQ      *OFF
     C     U9CVDT        IFNE      *ZEROS
     C     U9PRDT        ANDNE     *ZEROS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt  Member name
** Description--------------MISN
1011=Select to process      YYY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
203F3=Exit                  YYY
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               NNN
217F17=Subset               NNN
3
