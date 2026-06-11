     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* Program: SC711R - Prompt for Remarks Listing
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  20    SCM0508-Invalid User entered                           *
     H*  21    SCM0500-From Date Invalid                              *
     H*  22    SCM0500-To Date Invalid                                *
     H*  23    SCM0551-Dates in ascending order                       *
     H*  24    SCM0512-Invalid Remark Type                            *
     H*  25    SCM0552-Type of Remark must be A or D                  *
     H*  26    SCM0553-Type of Diary required, must be N, C, or B     *
     H*  27    SCM0554-Type of Diary must be blank if TYPE = A        *
     H*  28    SCM0555-Invalid Sort Sequence entered                  *
     H*  29    SCM0601-INVALID SUMMARY CODE
     H*  30    Position Cursor at KEY                                 *
     H*  31    Position Cursor at USER                                *
     H*  32    Position Cursor at (From) DATE                         *
     H*  33    Position Cursor at REMARK TYPE                         *
     H*  34    Position Cursor at DOCUMENT ID                         *
     H*  35    POSITION CURSOR AT SUMMARY CODE
     H*  60    General Chain                                          *
     H*****************************************************************
     H* REVISIONS:
     H*                                                               *
     H* 09/10/25  C:333 - Project: WC2950                             *
     H*                   Redesign of Remarks Listing Prompt.         *
     H*                   - Pass-through this program to new screen   *
     H*                     SCRMARKLST.                               *
     H*                   - A new parameter(12th) is needed to access *
     H*                     old screen.                               *
     H*
     H*  11/08/04 C:136 SUPPORT: DR065 43386                          *
     H*                 Restrict user access if SMSLP/SLRACC = 'Y'    *
     H*                                                               *
     H* 03/23/92 WLD    - Proj: AJP
     H*                   1.  Changed 2A Remark Code to 5A.             ARPNAM
     H*
     H* 06/22/92 KRR    - PROJ:
     H*                   1. CHANGE TO USE OUTPUT ROUTING RECORD AND
     H*                      USER PARMS OUTQUE
     H*
     H* 12/07/92  02    - PROJ:
     H*                   1. CHANGE TO USE YMD DATES ON EDIT FOR
     H*                      ASCENDING DATES ***
     H* 01/24/94  C:25  - PROJ: DR002
     H*                   1. ADDED PROMPT FOR SUMMARY CODE
     H*                   2. ADDED F9 SUMMARY CODE SEARCH
     H* 02/12/98  C:86  - PROJ: DR091   SEQ#10235
     H*           CHANGED START AND END DATES TO BE IN CCYYMMDD MODE
     H*****************************************************************
     FSC711F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMUPL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FSTSML     IF   E           K DISK
      *** BEG ADD ***** 11/08/04 **************************************
     FSMSLL     IF   E           K DISK
      *** END ADD ***** 11/08/04 **************************************

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  REMARK SUMMARY CODE FILE
     C     SMKY          KLIST
     C                   KFLD                    SCSMCD
     C*
     C*  Remark Type Table File
     C     CTKEY         KLIST
     C                   KFLD                    SCRMTY
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    CTDT          @@TODT
     C     *LIKE         DEFINE    CTDT          @@FRDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQUSER           10
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQSMCD            4
     C                   PARM                    QQRMTY            5
     C                   PARM                    QQDCID           12
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQDITY            1
     C                   PARM                    QQSORT            2
     C                   PARM                    QQCALL            1
      *** Beg Add ***** 09/10/25 ***************************************
     C                   PARM                    @@PassThru        1
      *** End Add ***** 09/10/25 ***************************************
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
      *** Beg Add ***** 09/10/25 **********************************
      * Do not Pass-Through to new screen, if 12th parameter (@@PassThru) is passed
     C                   If        Q1PRMS < 12
     C                   Call      'SCRMARKLST'
      *
      /free
       *InLr = *On;
       Return;
      /end-free
     C                   Endif
      *** End Add ***** 09/10/25 **********************************
      /Free
       scPrDs = 'Remarks Listing';
      /End-Free
      *** BEG ADD ***** 11/08/04 *************************************
     C     UPKEY         CHAIN     PSL01                              99
     C     SLRACC        IFEQ      'Y'
     C                   GOTO      TERM
     C                   ENDIF
     C*** END ADD ***** 11/08/04 **************************************
     C*
     C                   MOVEL     QQKEY         SCKEY
     C                   MOVEL     QQUSER        SCUSER
     C                   MOVE      QQFRDT        SCFRDT
     C                   MOVE      QQTODT        SCTODT
     C                   MOVE      QQSMCD        SCSMCD
     C                   MOVE      QQRMTY        SCRMTY
     C                   MOVE      QQDCID        SCDCID
     C                   MOVE      QQTYPE        SCTYPE
     C                   MOVE      QQDITY        SCDITY
     C                   MOVE      QQSORT        SCSORT
     C*
     C                   TIME                    DSDTTM
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*   Load the Record Format Info for First Time
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   Display All Formats
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*************************************************************************
     C*   Process All Command Keys
     C*
     C     *IN11         IFEQ      '1'
     C*
     C***********************************
     C*   F3 = Exit                     *
     C***********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C***********************************
     C*   F6 = Key Search               *
     C***********************************
     C     *INKF         IFEQ      '1'
     C*
     C                   MOVE      *BLANKS       QQRTCD            1
     C                   CALL      'SC595R'
     C                   PARM                    SCKEY
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTCD        IFEQ      'B'
     C     SCKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END *INKF
     C***********************************
     C*   F7 = User Search              *
     C***********************************
     C     *INKG         IFEQ      '1'
     C*
     C                   MOVE      'Y'           QQREGT            1            RE-GET PROFILES
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'SC650C'
     C                   PARM                    SCUSER                             |
     C                   PARM                    QQREGT            1                |
     C                   PARM                    QQRTCD
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTCD        IFEQ      'B'
     C     SCUSER        OREQ      *BLANK
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END *INKG
     C***********************************
     C*   F8 = Remark Type Search       *
     C***********************************
     C     *INKH         IFEQ      '1'
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'SC580R'
     C                   PARM                    SCRMTY
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTCD        IFEQ      'B'
     C     SCRMTY        OREQ      *BLANK
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END *INKH
     C***********************************
     C* F9 = SUMMARY CODE SEARCH        *
     C***********************************
     C     *INKI         IFEQ      '1'
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'SC585R'
     C                   PARM                    SCSMCD
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTCD        IFEQ      'B'
     C     SCSMCD        OREQ      *BLANK
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END *INKI
     C***********************************
     C*   *IN10 = Process Help Requests *
     C***********************************
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
     C*
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C*
     C                   END                                                    END CMDKEYS
     C*************************************************************************
     C*
     C*  Process Request Screen
     C*
     C     SCR01T        TAG
     C*
     C*   Validate User Name - if entered
     C     SCUSER        IFNE      *BLANKS
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'SVALUSR'
     C                   PARM                    SCUSER
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     '1'           STRT                     20
     C                   END                                                    END SCUSER NE *BLANK
     C*
     C*  Validate From Date - if entered
     C     SCFRDT        IFNE      *ZERO
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END                                                    END SRERR EQ '1'
     C                   END                                                    END SCFRDT NE *ZERO
     C*
     C*  Validate To Date - if entered
     C     SCTODT        IFNE      *ZERO
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END                                                    END SRERR EQ '1'
     C                   END                                                    END SCTODT NE *ZERO
     C*
     C*   Dates must be in Ascending Order - if entered
     C     SCFRDT        IFNE      *ZERO
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @@TODT
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @@FRDT
     C*
     C     @@TODT        CABLT     @@FRDT        STRT                   23
     C                   END                                                    END SCFRDT NE *ZERO
     C*
     C*   VALIDATE SUMMARY CODE - IF ENTERED
     C     SCSMCD        IFNE      *BLANKS
     C     SMKY          CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C                   END
     C*   Validate Remark Type - if entered
     C     SCRMTY        IFNE      *BLANKS
     C     SCRMTY        CHAIN     PCT01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END                                                     END *IN60 1 - PCT01
     C                   END                                                     END SCRMTY NE *BLKS
     C*
     C*   Type of Remark must be A or D
     C     SCTYPE        IFNE      'A'
     C     SCTYPE        ANDNE     'D'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END                                                     END SCTYPE NE A/D
     C*
     C*   Type of Diary Remark must be N, C or B - if SCTYPE = D
     C     SCTYPE        IFEQ      'D'
     C     SCDITY        IFNE      'N'
     C     SCDITY        ANDNE     'C'
     C     SCDITY        ANDNE     'B'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END                                                     END SCDITY EQ N/C/B
     C                   END                                                     END SCTYPE EQ D
     C*
     C*   Type of Diary Remark must be blank - if SCTYPE = A
     C     SCTYPE        IFEQ      'A'
     C     SCDITY        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END                                                     END SCTYPE EQ A
     C*
     C*   Validate Sort Sequence
     C     SCSORT        IFNE      'KY'
     C     SCSORT        ANDNE     'KD'
     C     SCSORT        ANDNE     'KR'
     C     SCSORT        ANDNE     'KO'
     C     SCSORT        ANDNE     'UK'
     C     SCSORT        ANDNE     'UD'
     C     SCSORT        ANDNE     'UR'
     C     SCSORT        ANDNE     'UO'
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END                                                     END SCSORT NE...
     C*
     C*   All Edits Passed - Call the CLP to submit the report
     C                   MOVE      SCKEY         QQKEY            40
     C                   MOVE      SCUSER        QQUSER           10
     C     SCFRDT        IFEQ      *ZEROS
     C*********** BEGIN CHANGE ************************ 02/12/98 ******
     C**********           MOVE 01011901  QQFRDT  8
     C                   MOVEL     0000101       QQFRD2            7
     C                   ELSE                                                   ELS SCFRDT EQ *ZEROS
     C**********           MOVE SCFRDT    QQFRDT
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QQFRD2
     C                   END                                                    END SCFRDT EQ *ZEROS
     C     SCTODT        IFEQ      *ZEROS
     C**********           MOVE 12311999  QQTODT  8
     C                   MOVEL     1991231       QQTOD2            7
     C                   ELSE                                                   ELS SCTODT EQ *ZEROS
     C**********           MOVE SCTODT    QQTODT
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QQTOD2
     C*********** END   CHANGE ************************ 02/12/98 ******
     C                   END                                                    END SCTODT EQ *ZEROS
     C                   MOVE      SCSMCD        QQSMCD
     C                   MOVEL     SCRMTY        QQRMTY            5
     C                   MOVE      SCDCID        QQDCID           12
     C                   MOVE      SCTYPE        QQTYPE            1
     C                   MOVE      SCDITY        QQDITY            1
     C                   MOVE      SCSORT        QQSORT            2
     C*
     C* Submit "Regular" DARTS Report - SCTYPE = A
     C     SCTYPE        IFEQ      'A'
     C                   CALL      'SC711C'
     C                   PARM                    QQKEY
     C                   PARM                    QQUSER
     C                   PARM                    QQFRD2                         CYYMMDD
     C                   PARM                    QQTOD2                         CYYMMDD
     C                   PARM                    QQSMCD
     C                   PARM                    QQRMTY
     C                   PARM                    QQDCID
     C                   PARM                    QQSORT
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C*
     C                   ELSE                                                   ELS EQ 'A'
     C*
     C* Submit "Diary" DARTS Report - SCTYPE = D
     C                   CALL      'SC716C'
     C                   PARM                    QQKEY
     C                   PARM                    QQUSER
     C                   PARM                    QQFRD2                         CYYMMDD
     C                   PARM                    QQTOD2                         CYYMMDD
     C                   PARM                    QQSMCD
     C                   PARM                    QQRMTY
     C                   PARM                    QQDCID
     C                   PARM                    QQDITY
     C                   PARM                    QQSORT
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   END                                                    END EQ 'A'
     C*
     C*  End of the Program
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
