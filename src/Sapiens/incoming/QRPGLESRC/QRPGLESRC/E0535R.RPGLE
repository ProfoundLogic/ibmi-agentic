     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**
     H/TITLE E0535R - MANUAL RATE SEARCH
     H**
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PROVIDES SEARCH CABABILITIES FOR THE       **
     H**     EXPECTED LOSS & MANUAL RATE FILE                        **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E0035R                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: ALAN M. MOSS                                  **
     H**   CREATED:    02/13/91     PROJECT: EXP MOD                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** MODIFICATIONS:                                              **
     H**                                                             **
     H**  06/19/24  C:291 Project: WC2952                            **
     H**            Exapand Manual Rate to 8,3.                      **
     H**                                                             **
     H**  08/12/98  C:90  PROJECT: WC853                             **
     H**            MODIFIED TO VERSION 3 STANDARDS.                 **
     H**                                                             **
     H**  11/07/94  C:30  PROJECT:  WC356                            **
     H**            Add field Default Rate Set Code.                 **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      KC      Command 03 - EOJ                               **
     H**      KL      Command 12 - Prior Screen (Screen 2)           **
     H**      10      HELP                                           **
     H**  SCR01:                                                     **
     H**      20      End of File reached on roll (ECR0600)          **
     H**              ECR0600 = End of file reached on roll          **
     H**      18      ROLL UP                                        **
     H**      19      ROLL DOWN                                      **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    E0535F:  Screen Display                                  **
     H**    SMCOL (PCO01): CO. FILE                                  **
     H**    EMUPL (PUP01): USER FILE                                 **
     H**    EMMRL (PWB01): CO/STATE/CLASS/EFF DT                     **
     H**    EMMRL1(PWB01): CO/RATE CODE/CLASS/EFF DT                 **
     H*****************************************************************
     H*
     F*
     F* Display Screen File
     FE0535F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter Master File
     FEMUPL     IF   E           K DISK
     F*
     F* Manual Rate File
     FEMMRL1    IF   E           K DISK

     D*
     D* Cursor Position Field
     D FL              S              1    DIM(14)
     D*
     D* Rate Set Code Field
     D*
     D* Expected Losses Field
     D*
     D* Ballast Value (DATE) Field
     D*
     D* RATE Field
     D*
     D* RATE FIELD  (XXRATE)
     D XXRETE          S              6    DIM(1)
     D*
     D* Status Field
     D*
     D* Roll Indicators For Screen
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D*
     D* Key Value for Class
     D KEYC            S              5  0 DIM(2)
     D* Key Value for EFDT
     D KEYD            S              8  0 DIM(2)
     D* Key Value for Rate Set Code
     D KEYR            S              3    DIM(2)
     D*
     D* Data Structure for System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* Data Structure for Screen Reference
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D* Data Structure for Rate Set Code Array
     D                 DS
     D  RC                     1     42
     D                                     DIM(14)
     D  SCRC01                 1      3
     D  SCRC02                 4      6
     D  SCRC03                 7      9
     D  SCRC04                10     12
     D  SCRC05                13     15
     D  SCRC06                16     18
     D  SCRC07                19     21
     D  SCRC08                22     24
     D  SCRC09                25     27
     D  SCRC10                28     30
     D  SCRC11                31     33
     D  SCRC12                34     36
     D  SCRC13                37     39
     D  SCRC14                40     42
     D*
     D* Data Structure for Expected losses Array
     D                 DS
     D  CL                     1     70  0
     D                                     DIM(14)
     D  SCCL01                 1      5  0
     D  SCCL02                 6     10  0
     D  SCCL03                11     15  0
     D  SCCL04                16     20  0
     D  SCCL05                21     25  0
     D  SCCL06                26     30  0
     D  SCCL07                31     35  0
     D  SCCL08                36     40  0
     D  SCCL09                41     45  0
     D  SCCL10                46     50  0
     D  SCCL11                51     55  0
     D  SCCL12                56     60  0
     D  SCCL13                61     65  0
     D  SCCL14                66     70  0
     D*
     D* Data Structure for Ballast value (date) Array
     D                 DS
     D  DT                     1    112  0
     D                                     DIM(14)
     D  SCDT01               001    008  0
     D  SCDT02               009    016  0
     D  SCDT03               017    024  0
     D  SCDT04               025    032  0
     D  SCDT05               033    040  0
     D  SCDT06               041    048  0
     D  SCDT07               049    056  0
     D  SCDT08               057    064  0
     D  SCDT09               065    072  0
     D  SCDT10               073    080  0
     D  SCDT11               081    088  0
     D  SCDT12               089    096  0
     D  SCDT13               097    104  0
     D  SCDT14               105    112  0
     D*
     D* Data Structure for Rate Array
      *** Beg Chg ***** 06/19/24 ********************************************
     D***               DS
     D***RT                     1     84
     D***                                   DIM(14)
     D***SCRT01                 1      6
     D***SCRT02                 7     12
     D***SCRT03                13     18
     D***SCRT04                19     24
     D***SCRT05                25     30
     D***SCRT06                31     36
     D***SCRT07                37     42
     D***SCRT08                43     48
     D***SCRT09                49     54
     D***SCRT10                55     60
     D***SCRT11                61     66
     D***SCRT12                67     72
     D***SCRT13                73     78
     D***SCRT14                79     84
     D                 DS
     D  RT                     1    112  5
     D                                     DIM(14)
     D  SCRT01                 1      8  5
     D  SCRT02                 9     16  5
     D  SCRT03                17     24  5
     D  SCRT04                25     32  5
     D  SCRT05                33     40  5
     D  SCRT06                41     48  5
     D  SCRT07                49     56  5
     D  SCRT08                57     64  5
     D  SCRT09                65     72  5
     D  SCRT10                73     80  5
     D  SCRT11                81     88  5
     D  SCRT12                89     96  5
     D  SCRT13                97    104  5
     D  SCRT14               105    112  5
      *** End Chg ***** 06/19/24 ********************************************
     D*
     D* Data Structure for XXRATE ARRAY
     D                 DS
     D  XXRATE                 1      6
     D  XXRATL                 1      1
     D  XXDEC                  2      2
     D  XXRATR                 3      6
     D*
     D* Data Structure for Status Array
     D                 DS
     D  ST                     1     14
     D                                     DIM(14)
     D  SCST01                 1      1
     D  SCST02                 2      2
     D  SCST03                 3      3
     D  SCST04                 4      4
     D  SCST05                 5      5
     D  SCST06                 6      6
     D  SCST07                 7      7
     D  SCST08                 8      8
     D  SCST09                 9      9
     D  SCST10                10     10
     D  SCST11                11     11
     D  SCST12                12     12
     D  SCST13                13     13
     D  SCST14                14     14
     D*
     D* external Program Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    MRCLAS        XXCLAS
     C     *LIKE         DEFINE    MREFDT        XXEFDT
     C     *LIKE         DEFINE    MRDRSC        XXDRSC
     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Key List for the Expected Loss & Discount Ratio File
     C*
     C     MRKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXDRSC
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXEFDT                         YY/MM/DD
     C*
     C     MRKEY1        KLIST
     C                   KFLD                    UPCO#
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQDRSC            3
     C                   PARM                    QQCLAS            5 0
     C                   PARM                    QQEFDT            8 0          MM/DD/YY
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
     C* Initialize and Set-Up
     C*
     C                   TIME                    DSDTTM
     C******************   MOVE Q1USER    SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQCLAS        SCCLAS
     C                   MOVE      QQDRSC        SCDRSC
     C*
     C                   Z-ADD     QQEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SCDATE
     C*
     C* Retrieve the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C* Retrieve the Company Record
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       COST
     C                   END
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C* Load Screen Record Format for the First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Load the Screen from Selected Position
     C*
     C     QQCLAS        COMP      *ZERO                                  21
     C                   GOTO      REPOS
     C*
     C* Display All Screen Formats
     C*
     C     STRT          TAG
     C*
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C                   EXFMT     SCR01
     C*
     C* PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* COMMAND 03 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   Z-ADD     *ZERO         QQCLAS
     C                   Z-ADD     *ZERO         QQEFDT
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* COMMAND 12 = CANCEL
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   Z-ADD     *ZERO         QQCLAS
     C                   Z-ADD     *ZERO         QQEFDT
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR01T
     C*
     C* ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR01T
     C*
     C* PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
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
     C                   END
     C*
     C                   END
     C*
     C* Goto the Appropriate Screen
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C* Process Requested Screen
     C*
     C     SCR01T        TAG
     C*
     C* INVALID ROLL REQUEST
     C*
     C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C* ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVE      KEYR(2)       XXDRSC
     C                   Z-ADD     KEYC(2)       XXCLAS
     C*
     C                   Z-ADD     KEYD(2)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     MRKEY         CHAIN     PMR01                              60
     C     MRKEY1        READE     PMR01                                  60
     C                   EXSR      LOAD
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      KEYR(1)       XXDRSC
     C                   Z-ADD     KEYC(1)       XXCLAS
     C*
     C                   Z-ADD     KEYD(1)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     MRKEY         CHAIN     PMR01                              60
     C                   READP     PMR01                                  60
     C                   EXSR      LOADP
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCDRSC        IFNE      *BLANK
     C     SCCLAS        ORNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANKS       KEYR
     C                   MOVE      *BLANKS       KEYC
     C                   MOVE      *BLANKS       KEYD
     C                   MOVE      *BLANK        XXUP
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C                   MOVE      *BLANKS       XXCLAS
     C                   MOVE      *BLANKS       XXEFDT
     C                   MOVE      SCDRSC        XXDRSC
     C                   MOVE      *BLANKS       SCDRSC
     C                   MOVE      SCCLAS        XXCLAS
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      *BLANKS       SCCLAS
     C                   MOVE      *BLANKS       SCDATE
     C     MRKEY         SETLL     PMR01
     C     MRKEY1        READE     PMR01                                  60
     C                   EXSR      LOAD
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       7             I1                3 0
     C*
     C*  VALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     CL(I1)        QQCLAS
     C                   Z-ADD     DT(I1)        QQEFDT
     C                   MOVE      RC(I1)        QQDRSC
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     *ZERO         QQCLAS
     C                   Z-ADD     *ZERO         QQEFDT
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       CL
     C                   MOVE      *BLANKS       DT
     C                   MOVE      *BLANKS       RT
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       RC
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     MRCLAS        CL(I1)
     C*
     C                   Z-ADD     MREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C*
      *** Beg Chg ***** 06/19/24 *********************************************
     C***                MOVEL     MRRATE        XXRATL
     C***                MOVE      '.'           XXDEC
     C***                MOVE      MRRATE        XXRATR
     C***                MOVE      XXRATE        RT(I1)
       RT(I1) = MRRate/100;
      *** End Chg ***** 06/19/24 *********************************************
     C                   MOVEL     MRSTAT        ST(I1)
     C                   MOVE      MRDRSC        RC(I1)
     C                   ENDSR
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
     C*
     C*  END OF FILE
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     MRKEY1        READE     PMR01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVE      RC(1)         KEYR(1)
     C                   MOVE      RC(14)        KEYR(2)
     C                   Z-ADD     CL(1)         KEYC(1)
     C                   Z-ADD     DT(1)         KEYD(1)
     C                   Z-ADD     CL(14)        KEYC(2)
     C                   Z-ADD     DT(14)        KEYD(2)
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     15            I1
     C     OVERP         TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   READP     PMR01                                  60
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVE      RC(1)         KEYR(1)
     C                   MOVE      RC(14)        KEYR(2)
     C                   Z-ADD     CL(1)         KEYC(1)
     C                   Z-ADD     DT(1)         KEYD(1)
     C                   Z-ADD     CL(14)        KEYC(2)
     C                   Z-ADD     DT(14)        KEYD(2)
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     *ZERO         XXCLAS
     C                   Z-ADD     *ZERO         XXEFDT
     C     MRKEY         SETLL     PMR01
     C     MRKEY1        READE     PMR01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     MRKEY1        READE     PMR01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
**
ROLL UP
ROLL DOWN
