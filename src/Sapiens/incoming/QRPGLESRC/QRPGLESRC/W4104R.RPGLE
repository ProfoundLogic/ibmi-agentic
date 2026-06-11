     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4104R - CLAIMS FORM SEARCH
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H*  03/09/99 C:61 PROJECT WC577                                  *
     H*                Minor Screen Changes                           *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
      *
     FW4104F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWALTL     UF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
      *
      *

      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d xxSS#           s              9
      ***  End   Add  ***  06/09/20  ****************************
     D FL              S              1    DIM(12)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY             S              5  0 DIM(2)
      *
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  RQ                     1     60  0
     D                                     DIM(12)
     D  SCRQ01                 1      5  0
     D  SCRQ02                 6     10  0
     D  SCRQ03                11     15  0
     D  SCRQ04                16     20  0
     D  SCRQ05                21     25  0
     D  SCRQ06                26     30  0
     D  SCRQ07                31     35  0
     D  SCRQ08                36     40  0
     D  SCRQ09                41     45  0
     D  SCRQ10                46     50  0
     D  SCRQ11                51     55  0
     D  SCRQ12                56     60  0
     D                 DS
     D  D4                     1     12
     D                                     DIM(12)
     D  SCD401                 1      1
     D  SCD402                 2      2
     D  SCD403                 3      3
     D  SCD404                 4      4
     D  SCD405                 5      5
     D  SCD406                 6      6
     D  SCD407                 7      7
     D  SCD408                 8      8
     D  SCD409                 9      9
     D  SCD410                10     10
     D  SCD411                11     11
     D  SCD412                12     12
     D                 DS
     D  D12                    1     12
     D                                     DIM(12)
     D  SC1201                 1      1
     D  SC1202                 2      2
     D  SC1203                 3      3
     D  SC1204                 4      4
     D  SC1205                 5      5
     D  SC1206                 6      6
     D  SC1207                 7      7
     D  SC1208                 8      8
     D  SC1209                 9      9
     D  SC1210                10     10
     D  SC1211                11     11
     D  SC1212                12     12
     D                 DS
     D  D3                     1     12
     D                                     DIM(12)
     D  SCD301                 1      1
     D  SCD302                 2      2
     D  SCD303                 3      3
     D  SCD304                 4      4
     D  SCD305                 5      5
     D  SCD306                 6      6
     D  SCD307                 7      7
     D  SCD308                 8      8
     D  SCD309                 9      9
     D  SCD310                10     10
     D  SCD311                11     11
     D  SCD312                12     12
     D                 DS
     D  D13                    1     12
     D                                     DIM(12)
     D  SC1301                 1      1
     D  SC1302                 2      2
     D  SC1303                 3      3
     D  SC1304                 4      4
     D  SC1305                 5      5
     D  SC1306                 6      6
     D  SC1307                 7      7
     D  SC1308                 8      8
     D  SC1309                 9      9
     D  SC1310                10     10
     D  SC1311                11     11
     D  SC1312                12     12
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  CLAIMS FORMS PARTIAL KEY
     C*
     C     LTKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  CLAIMS FORMS KEY
     C*
     C     LTKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXREQ#
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              KFLD                    SCSS#
     C                   KFLD                    XXSS#
      *****  End   Chg  ***  06/09/20  **********************
     C*
     C     *LIKE         DEFINE    LTFYR         XXFYR
     C     *LIKE         DEFINE    LTCASE        XXCASE
     C     *LIKE         DEFINE    LTREQ#        XXREQ#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQREQ#            5
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
     C*
     C                   TIME                    DSDTTM
      ***** BEG DEL ******* 03/09/99 **********************************
     C*************        MOVE Q1USER    SCUSER
      ***** END DEL ******* 03/09/99 **********************************
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQREQ#        XXREQ#
     C                   MOVE      QQFYR         SCYR
     C                   MOVE      QQCASE        SCCASE
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   MOVE      UPFND         SCFND
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      FNNAME        SCNAME
     C*
     C*  THE CLAIM MASTER RECORD MUST BE ON FILE
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C*
     C*   LOAD THE CLAIMANT NAME
     C*
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVE      CMSS#         SCSS#
     C                   MOVE      CMSS#         XXSS#
      /Free
       scSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      *****  End   Chg  ***  06/09/20  **********************
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCNAM
     C                   ELSE
     C                   MOVE      *BLANK        SCCNAM
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C                   MOVE      QQREQ#        SCREQ#
     C                   GOTO      REPOS
     C*
     C*   DISPLAY ALL FORMATS
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
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 24 = DELETE REQUEST RECORD
     C*
     C     *INKY         IFEQ      '1'
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN
     C     LIN           SUB       9             I1
     C*
     C*  VALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     12
     C     RQ(I1)        ANDNE     *ZERO
     C                   MOVE      RQ(I1)        XXREQ#
     C     LTKEY1        DELETE    PLT01                              6060
     C                   MOVE      RQ(1)         SCREQ#
     C                   GOTO      REPOS
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR01T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR01T
     C*
     C*   PROCESS HELP REQUESTS
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
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  INVALID ROLL REQUEST
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
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVEL     KEY(2)        XXREQ#
     C     LTKEY1        CHAIN     PLT01                              60
     C     LTKEY         READE     PLT01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     KEY(1)        XXREQ#
     C     LTKEY1        CHAIN     PLT01                              60
     C                   READP     PLT01                                  60
     C  N60LTCO#         COMP      UPCO#                              6060
     C  N60LTFND         COMP      UPFND                              6060
     C  N60LTFYR         COMP      XXFYR                              6060
     C  N60LTCASE        COMP      XXCASE                             6060
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCREQ#        IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEY
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVEL     SCREQ#        XXREQ#
     C                   MOVE      *ZERO         SCREQ#
     C     LTKEY1        SETLL     PLT01
     C     LTKEY         READE     PLT01                                  60
     C                   WRITE     SCR99
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       9             I1                3 0
     C*
     C*  VALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     12
     C                   MOVE      '*'           QQRTCD
     C                   MOVEL     RQ(I1)        QQREQ#
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *ZEROS        QQREQ#
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
     C                   MOVE      *BLANKS       RQ
     C                   MOVE      *BLANKS       D4
     C                   MOVE      *BLANKS       D12
     C                   MOVE      *BLANKS       D3
     C                   MOVE      *BLANKS       D13
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVEL     LTREQ#        RQ(I1)
     C                   MOVEL     LTDWC4        D4(I1)
     C                   MOVEL     LTDW12        D12(I1)
     C                   MOVEL     LTDWC3        D3(I1)
     C                   MOVEL     LTDW13        D13(I1)
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
     C     LTKEY         READE     PLT01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      12                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C  N60              EXCEPT    PLT01E
     C                   MOVEL     RQ(1)         KEY(1)
     C                   MOVEL     RQ(12)        KEY(2)
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     13            I1
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
     C                   READP     PLT01                                  60
     C  N60LTCO#         COMP      UPCO#                              6060
     C  N60LTFND         COMP      UPFND                              6060
     C  N60LTFYR         COMP      XXFYR                              6060
     C  N60LTCASE        COMP      XXCASE                             6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C  N60              EXCEPT    PLT01E
     C                   MOVEL     RQ(1)         KEY(1)
     C                   MOVEL     RQ(12)        KEY(2)
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
     C                   Z-ADD     *ZEROS        XXREQ#
     C     LTKEY1        SETLL     PLT01
     C     LTKEY         READE     PLT01                                  60
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
     C     LTKEY         READE     PLT01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      12                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C*
     OPLT01     E            PLT01E
**
ROLL UP
ROLL DOWN
