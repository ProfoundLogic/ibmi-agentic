     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F0100R                                            *
     H*    DESCRIPTION: DELETE FEE SCHEDULE LOCK OUT RECORD           *
     H*************************************************************** *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/09/20  C:212 Project : SY420                              *
     H*                  Mask SSN                                     *
     H*                                                               *
     H*  07/27/16 C:079 Support: 42603/99                             *
     H*                 Add check box processing                      *
     H*                                                               *
     H*  07/02/14 C:280 SUPPORT 85832                                 *
     H*                 FIX LOCKING RECORDS ON READ                   *
     H*                                                               *
     H*  04/10/02 C:62 PROJECT: AR036    BASE ONLY                    *
     H*                CHANGE F24 TO F2.                              *
     H*                                                               *
     H*  07/21/92 KRR     PROJ: 99AAC                                 *
     H*                   1. ADDED SUB# TO ALL PROCESSING             *
     H*                                                               *
     H*****************************************************************
     FF0100F    CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     INFDS(FIDS01)
     FSMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFXLOL1    UF   E           K DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D FL              S              1    DIM(14)
     D** KEY FIELD VALUES FOR FXLOL1 ...
     D CO              S              3  0 DIM(14)
     D SB              S              5  0 DIM(14)
     D FN              S              3  0 DIM(14)
     D YR              S              3  0 DIM(14)
     D**
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D*** Beg Add *** 07/27/16 *******************************
     D is01            S               n   inz(*off)
     D is02            S               n   inz(*off)
     D is03            S               n   inz(*off)
     D is04            S               n   inz(*off)
     D is05            S               n   inz(*off)
     D is06            S               n   inz(*off)
     D is07            S               n   inz(*off)
     D is08            S               n   inz(*off)
     D is09            S               n   inz(*off)
     D is10            S               n   inz(*off)
     D is11            S               n   inz(*off)
     D is12            S               n   inz(*off)
     D is13            S               n   inz(*off)
     D is14            S               n   inz(*off)
     D n               S              2  0
     D i1              S              3  0
     D*** End Add *** 07/27/16 *******************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  US                     1    140
     D                                     DIM(14)
     D  SCUS01                 1     10
     D  SCUS02                11     20
     D  SCUS03                21     30
     D  SCUS04                31     40
     D  SCUS05                41     50
     D  SCUS06                51     60
     D  SCUS07                61     70
     D  SCUS08                71     80
     D  SCUS09                81     90
     D  SCUS10                91    100
     D  SCUS11               101    110
     D  SCUS12               111    120
     D  SCUS13               121    130
     D  SCUS14               131    140
     D                 DS
     D  AD                     1    112  0
     D                                     DIM(14)
     D  SCAD01               001    008  0
     D  SCAD02               009    016  0
     D  SCAD03               017    024  0
     D  SCAD04               025    032  0
     D  SCAD05               033    040  0
     D  SCAD06               041    048  0
     D  SCAD07               049    056  0
     D  SCAD08               057    064  0
     D  SCAD09               065    072  0
     D  SCAD10               073    080  0
     D  SCAD11               081    088  0
     D  SCAD12               089    096  0
     D  SCAD13               097    104  0
     D  SCAD14               105    112  0
     D                 DS
     D  CA                     1     98  0
     D                                     DIM(14)
     D  SCCA01                 1      7  0
     D  SCCA02                 8     14  0
     D  SCCA03                15     21  0
     D  SCCA04                22     28  0
     D  SCCA05                29     35  0
     D  SCCA06                36     42  0
     D  SCCA07                43     49  0
     D  SCCA08                50     56  0
     D  SCCA09                57     63  0
     D  SCCA10                64     70  0
     D  SCCA11                71     77  0
     D  SCCA12                78     84  0
     D  SCCA13                85     91  0
     D  SCCA14                92     98  0
     D                 DS
     D  SS                     1    126
     D                                     DIM(14)
     D  SCSS01                 1      9
     D  SCSS02                10     18
     D  SCSS03                19     27
     D  SCSS04                28     36
     D  SCSS05                37     45
     D  SCSS06                46     54
     D  SCSS07                55     63
     D  SCSS08                64     72
     D  SCSS09                73     81
     D  SCSS10                82     90
     D  SCSS11                91     99
     D  SCSS12               100    108
     D  SCSS13               109    117
     D  SCSS14               118    126
     D                 DS
     D  NM                     1    224
     D                                     DIM(14)
     D  SCNM01                 1     16
     D  SCNM02                17     32
     D  SCNM03                33     48
     D  SCNM04                49     64
     D  SCNM05                65     80
     D  SCNM06                81     96
     D  SCNM07                97    112
     D  SCNM08               113    128
     D  SCNM09               129    144
     D  SCNM10               145    160
     D  SCNM11               161    176
     D  SCNM12               177    192
     D  SCNM13               193    208
     D  SCNM14               209    224
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
     D                 DS
     D  TM                     1     84  0
     D                                     DIM(14)
     D  SCTM01                 1      6  0
     D  SCTM02                 7     12  0
     D  SCTM03                13     18  0
     D  SCTM04                19     24  0
     D  SCTM05                25     30  0
     D  SCTM06                31     36  0
     D  SCTM07                37     42  0
     D  SCTM08                43     48  0
     D  SCTM09                49     54  0
     D  SCTM10                55     60  0
     D  SCTM11                61     66  0
     D  SCTM12                67     72  0
     D  SCTM13                73     78  0
     D  SCTM14                79     84  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*  CLAIM DETAIL FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    LOCO#
     C                   KFLD                    LOSUB#
     C                   KFLD                    LOFND
     C                   KFLD                    LOFYR
     C                   KFLD                    LOCASE
     C*
     C*  FEE SCHEDULE LOCKOUT FILE
     C*
     C     LOKEY         KLIST
     C                   KFLD                    XXUSER
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C*  INITIALIZE WORK FIELDS
     C     *LIKE         DEFINE    LOCO#         XXCO#
     C     *LIKE         DEFINE    LOSUB#        XXSUB#
     C     *LIKE         DEFINE    LOFND         XXFND
     C     *LIKE         DEFINE    LOFYR         XXFYR
     C     *LIKE         DEFINE    LOCASE        XXCASE
     C     *LIKE         DEFINE    LOUSER        XXUSER
     C     *LIKE         DEFINE    LOCO#         XXCO01
     C     *LIKE         DEFINE    LOSUB#        XXSU01
     C     *LIKE         DEFINE    LOFND         XXFN01
     C     *LIKE         DEFINE    LOFYR         XXYR01
     C     *LIKE         DEFINE    LOCASE        XXCA01
     C     *LIKE         DEFINE    LOUSER        XXUS01
     C     *LIKE         DEFINE    LOCO#         XXCO14
     C     *LIKE         DEFINE    LOSUB#        XXSU14
     C     *LIKE         DEFINE    LOFND         XXFN14
     C     *LIKE         DEFINE    LOFYR         XXYR14
     C     *LIKE         DEFINE    LOCASE        XXCA14
     C     *LIKE         DEFINE    LOUSER        XXUS14
     C     *LIKE         DEFINE    LOSS#         XXSS#
     C*
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   END
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   CLEAR SCREEN FIELDS FOR FIRST TIME TO PREVENT INVALID
     C*       DIGIT OR SIGN IF NO RECORDS ARE FOUND IN FXLOP...
     C*
     C                   EXSR      CLR
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C     SCUSR         COMP      *BLANKS                                21
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
     C*
     C                   EXFMT     SCR01
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 2 = DELETE SELECTED RECORD
     C*
     C*** BEGIN CHG *** 04/10/02 **************************************
     C********** *INKX     IFEQ '1'
     C     *INKB         IFEQ      *ON
     C*** END   CHG *** 04/10/02 **************************************
     C*
     C*  CHECK CURSOR POSTION
     C*
     C*** Beg Del *** 07/27/16 *************************************
     C**** Q2CPOS        DIV       256           LIN               2 0
     C**** LIN           SUB       7             I1                3 0
     C*** End Del *** 07/27/16 *************************************
     C*
     C*  Valid selection - Return with checkbox value
     C*
     C*** Beg Add *** 07/27/16 *************************************
     C*
     C                   eval      N = 1
     C*
     C                   dow       N <= 14
     C*
     C                   eval      I1 = 0
     C*
     C     scFL01        ifeq      '1'
     C                   eval      I1 = 1
     C                   Clear                   scFl01
     C                   goto      tagDel
     C                   endif
     C     scFL02        ifeq      '1'
     C                   eval      I1 = 2
     C                   Clear                   scFl02
     C                   goto      tagDel
     C                   endif
     C     scFL03        ifeq      '1'
     C                   eval      I1 = 3
     C                   Clear                   scFl03
     C                   goto      tagDel
     C                   endif
     C     scFL04        ifeq      '1'
     C                   eval      I1 = 4
     C                   Clear                   scFl04
     C                   goto      tagDel
     C                   endif
     C     scFL05        ifeq      '1'
     C                   eval      I1 = 5
     C                   Clear                   scFl05
     C                   goto      tagDel
     C                   endif
     C     scFL06        ifeq      '1'
     C                   eval      I1 = 6
     C                   Clear                   scFl06
     C                   goto      tagDel
     C                   endif
     C     scFL07        ifeq      '1'
     C                   eval      I1 = 7
     C                   Clear                   scFl07
     C                   goto      tagDel
     C                   endif
     C     scFL08        ifeq      '1'
     C                   eval      I1 = 8
     C                   Clear                   scFl08
     C                   goto      tagDel
     C                   endif
     C     scFL09        ifeq      '1'
     C                   eval      I1 = 9
     C                   Clear                   scFl09
     C                   goto      tagDel
     C                   endif
     C     scFL10        ifeq      '1'
     C                   eval      I1 = 10
     C                   Clear                   scFl10
     C                   goto      tagDel
     C                   endif
     C     scFL11        ifeq      '1'
     C                   eval      I1 = 11
     C                   Clear                   scFl11
     C                   goto      tagDel
     C                   endif
     C     scFL12        ifeq      '1'
     C                   eval      I1 = 12
     C                   Clear                   scFl12
     C                   goto      tagDel
     C                   endif
     C     scFL13        ifeq      '1'
     C                   eval      I1 = 13
     C                   Clear                   scFl13
     C                   goto      tagDel
     C                   endif
     C     scFL14        ifeq      '1'
     C                   eval      I1 = 14
     C                   Clear                   scFl14
     C                   goto      tagDel
     C                   endif
     C     tagDel        tag
     C*** End Add *** 07/27/16 *************************************
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C                   Z-ADD     CO(I1)        XXCO#
     C                   Z-ADD     SB(I1)        XXSUB#
     C                   Z-ADD     FN(I1)        XXFND
     C                   Z-ADD     YR(I1)        XXFYR
     C                   Z-ADD     CA(I1)        XXCASE
     C                   MOVE      *BLANKS       XXUSER
     C                   MOVE      US(I1)        XXUSER
     C**
     C     LOKEY         CHAIN     PLO01                              60
     C     *IN60         IFEQ      '0'
     C                   DELETE    PLO01
     C                   END
     C*** Beg Add *** 07/27/16 ***************************************
     C*
     C                   endif
     C*
     C                   eval      n = n+1
     C*
     C                   enddo
     C*** End Add *** 07/27/16 ***************************************
     C*
     C*  REDISPLAY SCREEN AFTER RECORD IS DELETED
     C*
     C                   EXSR      LOADB
     C                   GOTO      STRT
     C                   END
     C*
     C*  INVALID CURSOR POSITION - REDISPLAY SCREEN WITH ERROR MSG
     C*
     C                   SETON                                        99
     C                   GOTO      STRT
     C*** Beg Del *** 07/27/16 ***************************************
     C***************    END
     C*** End Del *** 07/27/16 ***************************************
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
     C*
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
     C                   Z-ADD     XXCO14        XXCO#
     C                   Z-ADD     XXSU14        XXSUB#
     C                   Z-ADD     XXFN14        XXFND
     C                   Z-ADD     XXYR14        XXFYR
     C                   Z-ADD     XXCA14        XXCASE
     C                   MOVEL     XXUS14        XXUSER
     C     LOKEY         SETLL     PLO01
     C*
     C*** BEG CHG *** 04/10/02 ****************************************
     C*
     C**********         READ      PLO01                                  60
     C                   READ(N)   PLO01                                  60
     C*
     C*** END CHG *** 04/10/02 ****************************************
     C*
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     XXCO01        XXCO#
     C                   Z-ADD     XXSU01        XXSUB#
     C                   Z-ADD     XXFN01        XXFND
     C                   Z-ADD     XXYR01        XXFYR
     C                   Z-ADD     XXCA01        XXCASE
     C                   MOVEL     XXUS01        XXUSER
     C     LOKEY         SETLL     PLO01
     C*
     C*** BEG CHG *** 04/10/02 ****************************************
     C*
     C**********         READ      PLO01                                  60
     C                   READ(N)   PLO01                                  60
     C*
     C*** END CHG *** 04/10/02 ****************************************
     C*
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCUSR         IFNE      *BLANK
     C     REPOS         TAG
     C                   MOVE      *BLANK        XXUP
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C**
     C                   Z-ADD     *ZEROS        XXCO#
     C                   Z-ADD     *ZEROS        XXSUB#
     C                   Z-ADD     *ZEROS        XXFND
     C                   Z-ADD     *ZEROS        XXFYR
     C                   Z-ADD     *ZEROS        XXCASE
     C                   MOVE      *BLANKS       XXUSER
     C                   MOVE      SCUSR         XXUSER
     C**
     C     LOKEY         SETLL     PLO01
     C*
     C*** BEG CHG *** 04/10/02 ****************************************
     C*
     C**********         READ      PLO01                                  60
     C                   READ(N)   PLO01                                  60
     C*
     C*** END CHG *** 04/10/02 ****************************************
     C*
     C*
     C     *IN60         IFEQ      '1'
     C                   SETON                                            98
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   EXSR      LOAD
     C                   MOVE      *BLANKS       SCUSR
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C********************************************************************
     C*  CLEAR SCREEN ARRAYS       ** SUBROUTINE **              C L R   *
     C********************************************************************
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       US
     C                   MOVE      *BLANKS       NM
     C                   Z-ADD     *ZEROS        CA
     C                   MOVE      *BLANKS       SS
     C                   Z-ADD     *ZEROS        AD
     C                   Z-ADD     *ZEROS        DT
     C                   Z-ADD     *ZEROS        TM
     C                   Z-ADD     *ZERO         I1
     C*** Beg Add *** 07/27/16 **********************
     C                   eval      is01 = *off
     C                   eval      is02 = *off
     C                   eval      is03 = *off
     C                   eval      is04 = *off
     C                   eval      is05 = *off
     C                   eval      is06 = *off
     C                   eval      is07 = *off
     C                   eval      is08 = *off
     C                   eval      is09 = *off
     C                   eval      is10 = *off
     C                   eval      is11 = *off
     C                   eval      is12 = *off
     C                   eval      is13 = *off
     C                   eval      is14 = *off
     C*** End Add *** 07/27/16 **********************
     C                   ENDSR
     C********************************************************************
     C*  FILL SCREEN ARRAYS      ** SUBROUTINE **                F I L L *
     C********************************************************************
     C     FILL          BEGSR
     C**
     C     I1            IFEQ      1
     C                   Z-ADD     LOCO#         XXCO01
     C                   Z-ADD     LOSUB#        XXSU01
     C                   Z-ADD     LOFND         XXFN01
     C                   Z-ADD     LOFYR         XXYR01
     C                   Z-ADD     LOCASE        XXCA01
     C                   MOVEL     LOUSER        XXUS01
     C                   END
     C**
     C*** SAVE NAME FOR LAST RECORD
     C**
     C     I1            IFEQ      14
     C                   Z-ADD     LOCO#         XXCO14
     C                   Z-ADD     LOSUB#        XXSU14
     C                   Z-ADD     LOFND         XXFN14
     C                   Z-ADD     LOFYR         XXYR14
     C                   Z-ADD     LOCASE        XXCA14
     C                   MOVEL     LOUSER        XXUS14
     C                   END
     C*** Beg Add *** 07/27/16 **********************
     C**
     C*** Set indicators for check boxes
     C**
     C                   if        i1 = 1
     C                   eval      is01 = *on
     C                   endif
     C                   if        i1 = 2
     C                   eval      is02 = *on
     C                   endif
     C                   if        i1 = 3
     C                   eval      is03 = *on
     C                   endif
     C                   if        i1 = 4
     C                   eval      is04 = *on
     C                   endif
     C                   if        i1 = 5
     C                   eval      is05 = *on
     C                   endif
     C                   if        i1 = 6
     C                   eval      is06 = *on
     C                   endif
     C                   if        i1 = 7
     C                   eval      is07 = *on
     C                   endif
     C                   if        i1 = 8
     C                   eval      is08 = *on
     C                   endif
     C                   if        i1 = 9
     C                   eval      is09 = *on
     C                   endif
     C                   if        i1 = 10
     C                   eval      is10 = *on
     C                   endif
     C                   if        i1 = 11
     C                   eval      is11 = *on
     C                   endif
     C                   if        i1 = 12
     C                   eval      is12 = *on
     C                   endif
     C                   if        i1 = 13
     C                   eval      is13 = *on
     C                   endif
     C                   if        i1 = 14
     C                   eval      is14 = *on
     C                   endif
     C*** End Add *** 07/27/16 **********************
     C**
     C** FILL FXLOP FIELDS
     C**
     C                   MOVEL     LOUSER        US(I1)
     C                   Z-ADD     LOCASE        CA(I1)
     C*
     C                   Z-ADD     LODT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C*
     C                   Z-ADD     LOTIME        TM(I1)
      ***** End Chg *** 06/09/20 *******************************************
     C***                MOVE      LOSS#         SS(I1)
      /Free
       SS(I1) = MaskData('SSN':'LOSS#':LOSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     LOACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         AD(I1)
     C**
     C** SAVE KEY FIELD VALUES FOR FXLOP RECORDS
     C**
     C                   Z-ADD     LOCO#         CO(I1)
     C                   Z-ADD     LOSUB#        SB(I1)
     C                   Z-ADD     LOFND         FN(I1)
     C                   Z-ADD     LOFYR         YR(I1)
     C**
     C** RETRIEVE CLAIM DETAIL RECORD
     C**
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      CMSS#         SS(I1)
      /Free
       SS(I1) = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         AD(I1)
     C**
     C** RETRIEVE CLAIM DEMO RECORD
     C**
     C                   Z-ADD     LOCO#         XXCO#
     C                   MOVE      CMSS#         XXSS#
     C     CDKEY         CHAIN     PCD01                              61
     C     *IN61         IFEQ      '0'
     C**
     C*** COMBINE LAST NAME, FIRST NAME
     C**
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C**
     C                   MOVEL     QQLAST        NM(I1)
     C**
     C                   END
     C**
     C                   END
     C     ENDFIL        ENDSR
     C********************************************************************
     C*  LOAD FORWARD           ** SUBROUTINE **                 L O A D *
     C********************************************************************
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
     C*
     C*** BEG CHG *** 04/10/02 ****************************************
     C*
     C**********         READ      PLO01                                  60
     C                   READ(N)   PLO01                                  60
     C*
     C*** END CHG *** 04/10/02 ****************************************
     C*
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   ENDSR
     C********************************************************************
     C*  LOAD BACKWARD          ** SUBROUTINE **               L O A D P *
     C********************************************************************
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
     C*
     C*** BEG CHG *** 04/10/02 ****************************************
     C*
     C**********         READ      PLO01                                  60
     C                   READ(N)   PLO01                                  60
     C*
     C*** END CHG *** 04/10/02 ****************************************
     C*
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   ENDSR
     C********************************************************************
     C*  LOAD BEGINNING         ** SUBROUTINE **               L O A D B *
     C********************************************************************
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     *ZEROS        XXCO#
     C                   Z-ADD     *ZEROS        XXSUB#
     C                   Z-ADD     *ZEROS        XXFND
     C                   Z-ADD     *ZEROS        XXFYR
     C                   Z-ADD     *ZEROS        XXCASE
     C                   MOVE      *BLANKS       XXUSER
     C     LOKEY         SETLL     PLO01
     C*
     C*** BEG CHG *** 04/10/02 ****************************************
     C*
     C**********         READ      PLO01                                  60
     C                   READ(N)   PLO01                                  60
     C*
     C*** END CHG *** 04/10/02 ****************************************
     C*
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
     C*
     C*** BEG CHG *** 04/10/02 ****************************************
     C*
     C**********         READ      PLO01                                  60
     C                   READ(N)   PLO01                                  60
     C*
     C*** END CHG *** 04/10/02 ****************************************
     C*
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
