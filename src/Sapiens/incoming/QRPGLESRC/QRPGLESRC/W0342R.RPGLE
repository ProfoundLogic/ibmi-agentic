     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0342R - REINSURANCE REQUEST RECOVERY SEARCH - AGGREGATE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  07/07/92 JEI PROJECT: WC157                                  *
     H*               CHANGED CASE NUMBER BACK TO NUMERIC FIELD FOR   *
     H*               'BASE' FILES (XXCASE) - CASE NUMBER REMAINS     *
     H*               14 ALPHA FOR REINSURANCE FILES (X@CASE)         *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     F*****************************************************************
     FW0342F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FWMRQL1    IF   E           K DISK
     FWMRCL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK

     D FL              S              1    DIM(14)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEYR            S              5  0 DIM(2)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  EM                     1    126  0
     D                                     DIM(14)
     D  SCEM01                 1      9  0
     D  SCEM02                10     18  0
     D  SCEM03                19     27  0
     D  SCEM04                28     36  0
     D  SCEM05                37     45  0
     D  SCEM06                46     54  0
     D  SCEM07                55     63  0
     D  SCEM08                64     72  0
     D  SCEM09                73     81  0
     D  SCEM10                82     90  0
     D  SCEM11                91     99  0
     D  SCEM12               100    108  0
     D  SCEM13               109    117  0
     D  SCEM14               118    126  0
     D                 DS
     D  DV                     1     70  0
     D                                     DIM(14)
     D  SCDV01                 1      5  0
     D  SCDV02                 6     10  0
     D  SCDV03                11     15  0
     D  SCDV04                16     20  0
     D  SCDV05                21     25  0
     D  SCDV06                26     30  0
     D  SCDV07                31     35  0
     D  SCDV08                36     40  0
     D  SCDV09                41     45  0
     D  SCDV10                46     50  0
     D  SCDV11                51     55  0
     D  SCDV12                56     60  0
     D  SCDV13                61     65  0
     D  SCDV14                66     70  0
     D                 DS
     D  EF                     1    112  0
     D                                     DIM(14)
     D  SCEF01               001    008  0
     D  SCEF02               009    016  0
     D  SCEF03               017    024  0
     D  SCEF04               025    032  0
     D  SCEF05               033    040  0
     D  SCEF06               041    048  0
     D  SCEF07               049    056  0
     D  SCEF08               057    064  0
     D  SCEF09               065    072  0
     D  SCEF10               073    080  0
     D  SCEF11               081    088  0
     D  SCEF12               089    096  0
     D  SCEF13               097    104  0
     D  SCEF14               105    112  0
     D                 DS
     D  RS                     1     70  0
     D                                     DIM(14)
     D  SCRS01                 1      5  0
     D  SCRS02                 6     10  0
     D  SCRS03                11     15  0
     D  SCRS04                16     20  0
     D  SCRS05                21     25  0
     D  SCRS06                26     30  0
     D  SCRS07                31     35  0
     D  SCRS08                36     40  0
     D  SCRS09                41     45  0
     D  SCRS10                46     50  0
     D  SCRS11                51     55  0
     D  SCRS12                56     60  0
     D  SCRS13                61     65  0
     D  SCRS14                66     70  0
     D                 DS
     D  CO                     1     42  0
     D                                     DIM(14)
     D  SCCO01                 1      3  0
     D  SCCO02                 4      6  0
     D  SCCO03                 7      9  0
     D  SCCO04                10     12  0
     D  SCCO05                13     15  0
     D  SCCO06                16     18  0
     D  SCCO07                19     21  0
     D  SCCO08                22     24  0
     D  SCCO09                25     27  0
     D  SCCO10                28     30  0
     D  SCCO11                31     33  0
     D  SCCO12                34     36  0
     D  SCCO13                37     39  0
     D  SCCO14                40     42  0
     D                 DS
     D  NM                     1    182
     D                                     DIM(14)
     D  SCNM01                 1     13
     D  SCNM02                14     26
     D  SCNM03                27     39
     D  SCNM04                40     52
     D  SCNM05                53     65
     D  SCNM06                66     78
     D  SCNM07                79     91
     D  SCNM08                92    104
     D  SCNM09               105    117
     D  SCNM10               118    130
     D  SCNM11               131    143
     D  SCNM12               144    156
     D  SCNM13               157    169
     D  SCNM14               170    182
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
     D  AR                     1    126  2
     D                                     DIM(14)
     D  SCAR01                 1      9  2
     D  SCAR02                10     18  2
     D  SCAR03                19     27  2
     D  SCAR04                28     36  2
     D  SCAR05                37     45  2
     D  SCAR06                46     54  2
     D  SCAR07                55     63  2
     D  SCAR08                64     72  2
     D  SCAR09                73     81  2
     D  SCAR10                82     90  2
     D  SCAR11                91     99  2
     D  SCAR12               100    108  2
     D  SCAR13               109    117  2
     D  SCAR14               118    126  2
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*                                                               *
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D OPTNM           C                   CONST('Reinsurance Recovery-
     D                                      Received Maint.')

     I*** END   ADD *** 01/28/98 ***********************************************
     I*

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
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*   KEY LIST FOR THE REINSURANCE COMPANY FILE
     C*
     C     RCKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    RQRCO
     C*
     C*  REINSURANCE REQUEST RECOVERY FILE
     C*
     C     RQKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXEFDT
     C                   KFLD                    QQASFL
     C*
     C     RQKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXEFDT
     C                   KFLD                    QQASFL
     C                   KFLD                    X@CASE
     C                   KFLD                    XXSEQ#            5 0
     C*
     C     *LIKE         DEFINE    RQCASE        X@CASE
     C     *LIKE         DEFINE    RQEMP#        XXEMP#
     C     *LIKE         DEFINE    RQDIV         XXDIV
     C     *LIKE         DEFINE    RQDT          XXEFDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQASFL            1
     C                   PARM                    SCEMP#            9 0
     C                   PARM                    SCDIV             5 0
     C                   PARM                    SCEFDT            8 0
     C                   PARM                    QQREQ#            5 0
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
     C*  INITIALIZE FIELDS
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C*
     C**********           MOVE QQPRDS    SCPRDS
     C*
     C*  CENTER PROGRAM NAME
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        SCPRDS
     C                   MOVEL(P)  ##DATA        QQPRDS
     C*
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      *ZEROS        X@CASE
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     UPFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*  POSITION FILE AT REQUESTED RECORD
     C*
     C                   MOVE      *BLANK        KEYR
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C     RQKEY         CHAIN     PRQ01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
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
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   Z-ADD     0             SCEMP#
     C                   Z-ADD     0             SCDIV
     C                   Z-ADD     0             SCEFDT
     C                   Z-ADD     *ZERO         QQREQ#
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   Z-ADD     *ZERO         QQREQ#
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
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
     C                   Z-ADD     KEYR(2)       XXSEQ#
     C     RQKEY1        CHAIN     PRQ01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYR(1)       XXSEQ#
     C     RQKEY         CHAIN     PRQ01                              60
     C                   EXSR      LOADP
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
     C                   Z-ADD     RS(I1)        QQREQ#
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     *ZERO         QQREQ#
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       EM
     C                   MOVE      *BLANKS       DV
     C                   MOVE      *BLANKS       EF
     C                   MOVE      *BLANKS       RS
     C                   MOVE      *BLANKS       CO
     C                   MOVE      *BLANKS       NM
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       DT
     C                   MOVE      *BLANKS       AR
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     RQEMP#        EM(I1)
     C                   Z-ADD     RQDIV         DV(I1)
     C*
     C                   Z-ADD     RQEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EF(I1)
     C*
     C                   MOVE      RQREQ#        RS(I1)
     C                   MOVE      RQRCO         CO(I1)
     C*
     C                   Z-ADD     RQDTRQ        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C*
     C                   Z-ADD     RQRAMT        AR(I1)
     C                   MOVE      RQSTAT        ST(I1)
     C*
     C     RCKEY         CHAIN     PRC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     RCSHNM        NM(I1)
     C                   END
     C*
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
     C     RQKEY         READE     PRQ01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     RS(1)         KEYR(1)
     C                   Z-ADD     RS(14)        KEYR(2)
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
     C                   READP     PRQ01                                  60
     C  N60RQCO#         COMP      UPCO#                              6060
     C  N60RQFND         COMP      UPFND                              6060
     C  N60RQEMP#        COMP      XXEMP#                             6060
     C  N60RQDIV         COMP      XXDIV                              6060
     C  N60RQEFDT        COMP      XXEFDT                             6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     RS(1)         KEYR(1)
     C                   Z-ADD     RS(14)        KEYR(2)
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
     C     RQKEY         CHAIN     PRQ01                              60
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
     C     RQKEY         READE     PRQ01                                  60
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
