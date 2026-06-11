     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0200R - WORKERS COMP CONTRACTORS CLASS MAINTENANCE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/20/10 C:33  Project: SY190 Priss #90  (WC1473)             *
     H*                Ensure new fields are used for updating data.  *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                Add two new columns to display EPSPEC          *
     H*                and EPEXT.                                     *
     H*                                                               *
     H* 04/01/08 C:183 PROJECT: WC1619                                *
     H*                Call SREFFD to set the effective date to be    *
     H*                used on SETGT.                                 *
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
     H*                                                               *
     H* 06/08/04 C:33  Seq#: WA129/42576                              *
     H*                Only edit for hours on Contractor Classes.     *
     H*                                                               *
     H* 11/02/99 C:118 PROJECT: WC577                                 *
     H*                Add F10=Search on Employer and policy years.   *
     H*                                                               *
     H* 10/06/99 C:90 PROJECT: WC945                                  *
     H*                FIND VALID EXTENSION WITH STATE EQUAL TO BLANK *
     H*                IF NOT VALID, THEN FIND VALID EXT WITH STATE   *
     H*                EQUAL TO THE RATING STATE.                     *
     H*                                                               *
     H* 08/11/98 C:65 Project: WC843                                  *
     H*               USE EXTENSION DESCRIPTION IF SPECIFIED.         *
     H*                                                               *
     H* 02/12/98  C:35  PROJECT: SUPPORT                              *
     H*                 CHANGE CODE SO THAT THE CANCELLATION DATE     *
     H*                 DISPLAYED ON THE SCREEN IS THE CX DATE        *
     H*                 ASSOCIATED WITH THE MOST RECENT EFFECTIVE     *
     H*                 DATE.  ALSO, THIS FIELD IS NOW BEING CLEARED  *
     H*                 SO THAT IT WON'T CARRY DATA FROM A PREVIOUSLY *
     H*                 DISPLAYED EMPLOYER IF THE CURRENT EMPLOYER    *
     H*                 IS NOT CANCELLED.  ALSO, CONTROL DISPLAY OF   *
     H*                 THIS FIELD WITH AN INDICATOR SO THAT IF THE   *
     H*                 EMPLOYER IS NOT CANCELLED, THE FIELD AND      *
     H*                 HEADING WILL NOT SHOW ON THE SCREEN.          *
     H*                                                               *
     H* 10/18/96 C:62 PROJECT: WC587                                  *
     H*          ADD RATE CODE TO PRKEY TO PROPERLY CHAIN PRDESC      *
     H*                                                               *
     H*****************************************************************
     FW0200F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMCCL     UF A E           K DISK
     FWMFNL     IF   E           K DISK
     F***** BEGIN ADD **** 11/02/99 ***********************************
     F                                     INFDS(DBFDS)
     F***** END   ADD **** 11/02/99 ***********************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     F***** BEGIN ADD **** 11/02/99 ***********************************
     F                                     INFDS(DBFDS2)
     F***** END   ADD **** 11/02/99 ***********************************
     F***** BEG   CHG **** 04/20/09 ***********************************
     F*********WMEPL1  IF  E           K        DISK
     FWMEPL1E   IF   E           K DISK
     F***** END   CHG **** 04/20/09 ***********************************
     FWDF1L     IF   E           K DISK
     FWMPRL1    IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL02)
     F*** BEGIN ADD ***  08/11/98  ************************************
      **** BEGIN CHG **** 10/06/99 ************************************
     F******WMPXL   IF  E           K        DISK
     FWMPXL1    IF   E           K DISK
      **** END   CHG **** 10/06/99 ************************************
     F*** END ADD *****  08/11/98  ************************************
      **** BEGIN ADD **** 10/06/99 ************************************
     FWMRSL     IF   E           K DISK
      **** END   ADD **** 10/06/99 ************************************
     F***** BEGIN ADD **** 11/02/99 ***********************************
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     F***** END   ADD **** 11/02/99 ***********************************

     D IA              S              1    DIM(999)
     D CLS             S              5  0 DIM(999)
     D***** BEG   ADD **** 04/20/09 ***********************************
     D***** END   ADD **** 04/20/09 ***********************************
     D***** END   ADD **** 04/20/09 ***********************************
     D***************     DS         12 30
     D***** END   CHG **** 04/20/09 ***********************************
     D FL              S              1    DIM(12)
     D RR              S              9    DIM(3) CTDATA PERRCD(1)
     D KEYC            S              5  0 DIM(2)
     D***** BEG   ADD **** 04/20/09 ***********************************
     D KEYS            S              1    DIM(2)
     D KEYE            S              5    DIM(2)
     D***** END   ADD **** 04/20/09 ***********************************
      **** BEGIN ADD **** 02/12/98 *********************************************
      **** END   ADD **** 02/12/98 *********************************************
     D*
     D OPTNM           C                   CONST('Work with Contractor-
     D                                      Classes')
     D*
     D***** BEGIN ADD **** 11/02/99 ***********************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D***** END   ADD **** 11/02/99 ***********************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      **** BEGIN ADD **** 02/12/98 *********************************************
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
      **** END   ADD **** 02/12/98 *********************************************
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D                 DS
     D  CL                     1     60  0
     D                                     DIM(12)
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
     D*********** DS
     D***********                             1 360 DS
     D***********                             1  30 SCDS01
     D***********                            31  60 SCDS02
     D***********                            61  90 SCDS03
     D***********                            91 120 SCDS04
     D***********                           121 150 SCDS05
     D***********                           151 180 SCDS06
     D***********                           181 210 SCDS07
     D***********                           211 240 SCDS08
     D***********                           241 270 SCDS09
     D***********                           271 300 SCDS10
     D***********                           301 330 SCDS11
     D***********                           331 360 SCDS12
     D                 DS
     D  DS                     1    240
     D                                     DIM(12)
     D  SCDS01                 1     20
     D  SCDS02                21     40
     D  SCDS03                41     60
     D  SCDS04                61     80
     D  SCDS05                81    100
     D  SCDS06               101    120
     D  SCDS07               121    140
     D  SCDS08               141    160
     D  SCDS09               161    180
     D  SCDS10               181    200
     D  SCDS11               201    220
     D  SCDS12               221    240
     D                 DS
     D  AM                     1    132  2
     D                                     DIM(12)
     D  SCAM01                 1     11  2
     D  SCAM02                12     22  2
     D  SCAM03                23     33  2
     D  SCAM04                34     44  2
     D  SCAM05                45     55  2
     D  SCAM06                56     66  2
     D  SCAM07                67     77  2
     D  SCAM08                78     88  2
     D  SCAM09                89     99  2
     D  SCAM10               100    110  2
     D  SCAM11               111    121  2
     D  SCAM12               122    132  2
     D                 DS
     D  HR                     1    108  2
     D                                     DIM(12)
     D  SCHR01                 1      9  2
     D  SCHR02                10     18  2
     D  SCHR03                19     27  2
     D  SCHR04                28     36  2
     D  SCHR05                37     45  2
     D  SCHR06                46     54  2
     D  SCHR07                55     63  2
     D  SCHR08                64     72  2
     D  SCHR09                73     81  2
     D  SCHR10                82     90  2
     D  SCHR11                91     99  2
     D  SCHR12               100    108  2
     D***** BEG   ADD **** 04/20/09 ***********************************
     D                 DS
     D  SP                     1     12
     D                                     DIM(12)
     D  SCSP01                 1      1
     D  SCSP02                 2      2
     D  SCSP03                 3      3
     D  SCSP04                 4      4
     D  SCSP05                 5      5
     D  SCSP06                 6      6
     D  SCSP07                 7      7
     D  SCSP08                 8      8
     D  SCSP09                 9      9
     D  SCSP10                10     10
     D  SCSP11                11     11
     D  SCSP12                12     12
     D                 DS
     D  EX                     1     60
     D                                     DIM(12)
     D  SCEX01                 1      5
     D  SCEX02                 6     10
     D  SCEX03                11     15
     D  SCEX04                16     20
     D  SCEX05                21     25
     D  SCEX06                26     30
     D  SCEX07                31     35
     D  SCEX08                36     40
     D  SCEX09                41     45
     D  SCEX10                46     50
     D  SCEX11                51     55
     D  SCEX12                56     60
     D***** END   ADD **** 04/20/09 ***********************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*** BEGIN ADD ***  08/11/98  ************************************
     C*
     C*  PAYROLL DESCRIPTION EXTENSION FILE
     C*
     C     PXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C*** END ADD *****  08/11/98  ************************************
      **** BEGIN ADD **** 10/06/99 ************************************
     C                   KFLD                    XXST
     C*** BEGIN ADD ***  06/01/06  ************************************
     C*** BEG CHG ***** 04/01/08 ***********************************************
     C**************       KFLD           EPEFDT
     C                   KFLD                    X1EFDT            7 0
     C*** END CHG ***** 04/01/08 ***********************************************
     C     PXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
     C*** END ADD *****  06/01/06  ************************************
      *
      * RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      **** END   ADD **** 10/06/99 ************************************
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*   KEY LIST FOR EMPLOYER PAYROLL FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C     EPKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXCLAS
     C***** BEG   ADD **** 04/20/09 ***********************************
     C                   KFLD                    XXSPEC            1
     C                   KFLD                    XXEXT             5
     C***** END   ADD **** 04/20/09 ***********************************
     C*
     C     EPKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    ZZCLAS
     C***** BEG   ADD **** 04/20/09 ***********************************
     C                   KFLD                    XXSPEC            1
     C                   KFLD                    XXEXT             5
     C***** END   ADD **** 04/20/09 ***********************************
     C*
     C*   KEY LIST FOR CONTRACTORS CLASS FILE
     C*
     C     CCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFLAG            1
     C                   KFLD                    XXCLAS
     C***  Begin Add  ***  08/20/10  **************************************
     C                   KFLD                    XXSPEC
     C                   KFLD                    XXEXT
     C***  End   Add  ***  08/20/10  **************************************
     C*
     C     CCKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*   KEY LIST FOR PAYROLL CLASSIFICATION FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    XXCLAS
     C*** BEG CHG ***** 04/01/08 ***********************************************
     C*************        KFLD           ELPFDT
     C                   KFLD                    XXEFDT            7 0
     C*** END CHG ***** 04/01/08 ***********************************************
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    @@RRN             9 0
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
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM      XXCO#         #@CO#             3 0
     C                   PARM      SCFND         #@FND             3 0
     C                   PARM      SCEMP#        #@EMP#            9 0
     C                   PARM      SCDIV         #@DIV             5 0
     C                   PARM      SCFYR         ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    EPCLAS        ZZCLAS
      **** BEGIN ADD **** 10/06/99 ************************************
     C     *LIKE         DEFINE    PXST          XXST
      **** END   ADD **** 10/06/99 ************************************
     C*
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C     Q1PRMS        IFLE      2
     C*
     C                   MOVE      *OFF          *IN99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              55
     C     *IN55         IFEQ      '1'
     C                   Z-ADD     *ZERO         XXCO#
     C                   Z-ADD     *ZERO         XXFND
     C                   ELSE
     C                   Z-ADD     UPCO#         XXCO#
     C                   Z-ADD     UPFND         XXFND
     C                   END
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN99
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C     @@RRN         CHAIN     WDELP                              60
     C                   Z-ADD     ELCO#         XXCO#
     C                   Z-ADD     ELFND         XXFND
     C*
     C                   ENDIF                                                  Q1PRMS<=2
     C*
     C     TOP           TAG
     C*
     C     COKY          CHAIN     PCO01                              55
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C     FNKY          CHAIN     PFN01                              55
     C                   MOVEL     XXFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C     Q1PRMS        IFLE      2
     C                   MOVE      UPFYR         SCFYR
     C                   Z-ADD     SCFYR         XXFYR
     C                   MOVE      UPEMP#        SCEMP#
     C                   MOVE      UPDIV         SCDIV
     C                   ELSE
     C                   Z-ADD     ELFYR         SCFYR
     C                   Z-ADD     ELFYR         XXFYR
     C                   Z-ADD     ELEMP#        SCEMP#
     C                   Z-ADD     ELDIV         SCDIV
     C                   ENDIF                                                  Q1PRMS<=2
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C   99              GOTO      SCR01T
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     XXUP          IFEQ      '1'
     C                   MOVE      RR(3)         SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
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
     C***** BEGIN ADD **** 11/02/99 ***********************************
     C*
     C*  COMMAND F10 = SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCFYR'
     C*
     C*  If cursor position on search is SCFYR, select Emp/div first
     C*
     C     SCEMP#        IFEQ      *ZEROS
     C*
     C                   CALL      'W0050R'
     C                   PARM                    QQKC              1
     C                   PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      DBRRN         QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C                   PARM      1             QQVIEW            1 0
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=ON
     C*
     C     *INKL         IFEQ      *ON
     C     QQRRN         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=ON
     C*                                                    QQRRN=O
     C     QQRRN         IFNE      *ZEROS
     C*
     C     QQRRN         CHAIN     PEM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         XXFND
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   ENDIF                                                  QQRRN<>0
     C*
     C                   ENDIF                                                  SCEMP#=0
     C*
     C*  Call Emp policy years
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   CALL      'W0051R'
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM      '2'           QQCALL
     C                   PARM      0             QQRRN
     C                   PARM      DBRRN2        QQRRN2
     C                   PARM                    QQQTE
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=ON
     C*
     C     *INKL         IFEQ      *ON
     C     QQRRN         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=ON
     C*                                                    QQRRN=0
     C     QQRRN         IFNE      *ZERO
     C*
     C     QQRRN         CHAIN     PEL02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELFYR         SCFYR
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  QQRRN<>0
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C*  If cursor position is Emp/div on search call WW Emp.
     C*
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   CALL      'W0050R'
     C                   PARM                    QQKC              1
     C                   PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      DBRRN         QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C                   PARM      1             QQVIEW            1 0
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=ON
     C*
     C     *INKL         IFEQ      *ON
     C     QQRRN         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=ON
     C*                                                    QQRRN=0
     C     QQRRN         IFNE      *ZEROS
     C*
     C     QQRRN         CHAIN     PEM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   ENDIF                                                  *IN60=OFF
     C                   ENDIF                                                  QQRRN<>0
     C*
     C*  If policy year is blank call Emp policy years
     C*
     C     SCFYR         IFEQ      *ZEROS
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   CALL      'W0051R'
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM      '2'           QQCALL
     C                   PARM      0             QQRRN
     C                   PARM      DBRRN2        QQRRN2
     C                   PARM                    QQQTE
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=ON
     C*
     C     *INKL         IFEQ      *ON
     C     QQRRN         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=ON
     C*                                                    QQRRN=0
     C     QQRRN         IFNE      *ZERO
     C*
     C     QQRRN         CHAIN     PEL02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELFYR         SCFYR
     C                   GOTO      STRT
     C                   ELSE
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  QQRRN<>0
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCFYR=0
     C*
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *INKJ=OFF
     C*
     C***** END   ADD **** 11/02/99 ***********************************
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C  N99              MOVEL     'SCR01'       Q2RECF
     C   99              GOTO      TERM
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*   COMMAND 15 = CALCULATIONN
     C*
     C     *INKP         IFEQ      '1'
     C*
     C                   MOVE      XXCO#         ##CO#
     C                   MOVE      SCFND         ##FND
     C                   MOVE      SCFYR         ##YR
     C                   MOVE      SCEMP#        ##EMP#
     C                   MOVE      SCDIV         ##DIV
     C                   MOVEL     UPOUTQ        ##QUE
     C*
     C                   CALL      'W8500C'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##YR              3
     C                   PARM                    ##EMP#            9
     C                   PARM                    ##DIV             5
     C                   PARM                    ##QUE            10
     C*
     C                   SETON                                        93
     C                   GOTO      STRT
     C                   END
     C*
     C*   COMMAND 21 = PRINT
     C*
     C     *INKV         IFEQ      '1'
     C*
     C                   MOVE      XXCO#         ##CO#
     C                   MOVE      SCFND         ##FND
     C                   MOVE      SCFYR         ##YR
     C                   MOVE      SCEMP#        ##EMP#
     C                   MOVE      SCDIV         ##DIV
     C                   MOVEL     UPOUTQ        ##QUE
     C*
     C                   CALL      'W8510C'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##YR
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##QUE
     C*
     C                   SETON                                        94
     C                   GOTO      STRT
     C                   END
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR02T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR02T
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
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         XXCO#
     C                   Z-ADD     *ZERO         XXFND
     C                   ELSE
     C                   Z-ADD     UPCO#         XXCO#
     C                   Z-ADD     UPFND         XXFND
     C***** BEGIN ADD **** 11/02/99 ***********************************
     C                   MOVE      *BLANKS       FNNAME
     C***** END   ADD **** 11/02/99 ***********************************
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE FUND YEAR ENTERED.
     C*
     C                   Z-ADD     SCFYR         XXFYR             3 0
     C     F1KY          CHAIN     PF101                              20
     C     *IN20         CABEQ     '1'           STRT
     C*
     C*  THE EMPLOYER AND DIV MUST BE ON FILE
     C*
     C     EMKY          CHAIN     PEM01                              21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C***** BEGIN ADD **** 11/02/99 ***********************************
     C*
     C*  THE POLICY YEAR MUST BE VALID FOR EMPLOYER/DIVISION
     C*
     C     ELKY          CHAIN     PEL01                              23
     C     *IN23         CABEQ     '1'           STRT
     C*
     C***** END   ADD **** 11/02/99 ***********************************
     C*
     C*  MOVE EMPLOYER NAME FOR SCR02 DISPLAY
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
      **** BEGIN DEL **** 02/12/98 *********************************************
     C******************** DO   5         I1      30
     C********************
     C********** CX,I1     IFNE *ZERO
     C******************** Z-ADDCX,I1     SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     SCCXDT
     C******************** ELSE
     C******************** LEAVE
     C******************** ENDIF                           CX,I1<>*ZERO
     C********************
     C******************** ENDDO                           I1->5
      **** END   DEL **** 02/12/98 *********************************************
      **** BEGIN ADD **** 02/12/98 *********************************************
     C                   MOVE      *OFF          *IN58
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1
      *
     C     *ZERO         LOOKUP    EF(I1)                                 61
      *
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
      *
     C     I1            IFNE      0
      *
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN58
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
      *
     C                   ENDIF                                                  I1 IFNE 0
      **** END   ADD **** 02/12/98 *********************************************
     C*
     C*  GET AGENT/PRODUCER INFORMATION
     C*
     C                   CALL      'WA002R'      AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C                   MOVE      *BLANKS       SCCLAS
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         CABEQ     '1'           STRT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C*   READ CONTRACTORS CLASS FILE & LOAD CLASS ARRAY USED TO
     C*   VALIDATE CHANGING FROM ACTIVE TO INACTIVE STATUS.
     C*
     C     CCKY2         CHAIN     PCC01                              60
     C                   Z-ADD     1             Y                 2 0
     C     *IN60         DOWEQ     '0'
     C     CCPAMT        IFNE      0
     C                   MOVE      CCCLAS        CLS(Y)
     C                   ADD       1             Y
     C                   END
     C     CCKY2         READE     PCC01                                  60
     C                   END
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C     SCCLAS        COMP      *ZERO                                  56
     C                   GOTO      REPOS
     C*
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR02T        TAG
     C*
     C*  INVALID ROLL REQUEST
     C*
     C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C*
     C                   EXSR      RITE
     C*
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     KEYC(2)       XXCLAS
     C***** BEG   ADD **** 04/20/09 ***********************************
     C                   MOVE      KEYS(2)       XXSPEC
     C                   MOVE      KEYE(2)       XXEXT
     C***** END   ADD **** 04/20/09 ***********************************
     C     EPKY2         CHAIN     PEP01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C*
     C                   EXSR      RITE
     C*
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYC(1)       XXCLAS
     C***** BEG   ADD **** 04/20/09 ***********************************
     C                   MOVE      KEYS(1)       XXSPEC
     C                   MOVE      KEYE(1)       XXEXT
     C***** END   ADD **** 04/20/09 ***********************************
     C     EPKY2         CHAIN     PEP01                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCCLAS        IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        KEYC
     C***** BEG   ADD **** 04/20/09 ***********************************
     C                   MOVE      *BLANK        XXSPEC
     C                   MOVE      *BLANK        XXEXT
     C***** END   ADD **** 04/20/09 ***********************************
     C   56              MOVE      '1'           XXDOWN
     C  N56              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       56
     C                   Z-ADD     SCCLAS        XXCLAS
     C                   MOVE      *BLANKS       SCCLAS
     C     EPKY2         SETLL     PEP01
     C     EPKY          READE     PEP01                                  60
     C*
     C                   Z-ADD     EPEFDT        EPEFDT
     C*
     C*   GET A RECORD WITH AN ACTIVE STATUS, OR A RECORD IN THE
     C*   CONTRACTORS CLASS FILE WHICH HAS BEEN CHANGED FROM ACTIVE TO
     C*   INACTIVE STATUS
     C*
     C                   MOVE      *OFF          *IN57
     C*
     C     *IN60         DOWEQ     *OFF
     C     EPSTAT        ANDEQ     'I'
     C     *IN57         ANDEQ     *OFF
     C*
     C                   Z-ADD     1             Y
     C     EPCLAS        LOOKUP    CLS(Y)                                 57
     C*
     C     *IN57         IFEQ      *OFF                                         NOT FOUND
     C     EPKY          READE     PEP01                                  60
     C                   ELSE
     C                   Z-ADD     EPCLAS        ZZCLAS
     C***** BEG   ADD **** 04/20/09 ***********************************
     C                   MOVE      EPSPEC        XXSPEC
     C                   MOVE      EPEXT         XXEXT
     C***** END   ADD **** 04/20/09 ***********************************
     C     EPKY3         READE     PEP01                                  60
     C   60              MOVE      'I'           IA(Y)
     C  N60              MOVE      *OFF          *IN57
     C                   ENDIF                                                  *IN57=*OFF
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C* IF ENTER WAS PRESSED, EXECUTE THE RITE SUBROUTINE
     C*
     C                   EXSR      RITE
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C  N99              GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *ZEROS        CL
     C                   MOVE      *BLANKS       DS
     C                   MOVE      *ZEROS        AM
     C                   MOVE      *ZEROS        HR
     C                   Z-ADD     *ZERO         I1                2 0
     C                   MOVEA     '0000000'     *IN(31)
     C                   MOVEA     '00000'       *IN(38)
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVE      EPCLAS        CL(I1)
     C***** BEG   ADD **** 04/20/09 ***********************************
     C                   MOVE      EPSPEC        SP(I1)
     C                   MOVE      EPEXT         EX(I1)
     C***** END   ADD **** 04/20/09 ***********************************
     C     I1            ADD       30            X                 2 0
     C                   MOVE      '1'           *IN(X)
     C                   MOVE      EPCLAS        XXCLAS
     C***** BEG   ADD **** 04/20/09 ***********************************
     C                   MOVE      EPSPEC        XXSPEC
     C                   MOVE      EPEXT         XXEXT
     C***** END   ADD **** 04/20/09 ***********************************
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             XXEFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   MOVE(P)   EPEFDT        #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   XXCLAS        #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        XXEFDT
     C*** END ADD *****  04/01/08  *******************************************
     C     PRKY          SETGT     PPR01
     C                   READP     PPR01                                  55
     C     *IN55         IFEQ      '0'
     C     PRCO#         ANDEQ     XXCO#
     C     PRCLAS        ANDEQ     XXCLAS
     C***** BEG   CHG **** 04/20/09 ***********************************
     C**************       MOVE PRDESC    DS,I1
     C                   MOVEL     PRDESC        DS(I1)
     C***** END   CHG **** 04/20/09 ***********************************
     C                   MOVE      PRCCFL        FL(I1)
     C                   MOVE      PRCCFL        XXFLAG
     C                   ELSE
     C                   MOVE      *BLANK        XXFLAG
     C                   END
     C*** BEGIN ADD ***  08/11/98  ************************************
     C     EPEXT         IFNE      *BLANKS
      **** BEGIN ADD **** 10/06/99 ************************************
     C                   MOVE      *BLANKS       XXST
      **** END   ADD **** 10/06/99 ************************************
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X1EFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   MOVE(P)   EPEFDT        #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   XXCLAS        #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        X1EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C*** BEGIN CHG ***  06/01/06  ************************************
     C********** PXKY      CHAINPPX01                60
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C     *IN60         IFEQ      *OFF
     C     PXSTAT        ANDEQ     'A'
     C                   MOVEL(P)  PXDESC        DS(I1)
      **** BEGIN ADD **** 10/06/99 ************************************
     C                   ELSE
     C   60RSKY          CHAIN     PRS01                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C**N61*60** PXKY      CHAINPPX01                60
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             X1EFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   MOVE(P)   EPEFDT        #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   XXCLAS        #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        X1EFDT
     C*** END ADD *****  04/01/08  *******************************************
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C  N61
     CANN60              MOVEL(P)  PXDESC        DS(I1)
      **** END   ADD **** 10/06/99 ************************************
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  EPEXT IFNE
     C*** END ADD *****  08/11/98  ************************************
     C                   MOVE      EPCLAS        XXCLAS
     C***  Begin Add  ***  08/20/10  **************************************
     C                   MOVE      EPSPEC        XXSPEC
     C                   MOVE      EPEXT         XXEXT
     C***  End   Add  ***  08/20/10  **************************************
     C     CCKY          CHAIN     PCC01                              55
     C     *IN55         IFEQ      '0'
     C                   EXCEPT    PCC01E
     C                   Z-ADD     CCPAMT        AM(I1)
     C                   Z-ADD     CCHRS         HR(I1)
     C                   END
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
     C     EPKY2         SETGT     PEP01
     C     EPKY          READE     PEP01                                  60
     C*
     C*   GET A RECORD WITH AN ACTIVE STATUS, OR A RECORD IN THE
     C*   CONTRACTORS CLASS FILE WHICH HAS BEEN CHANGED FROM ACTIVE TO
     C*   INACTIVE STATUS
     C*
     C                   MOVE      *OFF          *IN57
     C*
     C     *IN60         DOWEQ     *OFF
     C     EPSTAT        ANDEQ     'I'
     C     *IN57         ANDEQ     *OFF
     C*
     C                   Z-ADD     1             Y
     C     EPCLAS        LOOKUP    CLS(Y)                                 57
     C*
     C     *IN57         IFEQ      *OFF                                         NOT FOUND
     C*
     C     EPKY          READE     PEP01                                  60
     C                   ELSE
     C                   Z-ADD     EPCLAS        ZZCLAS
     C***** BEG   ADD **** 04/20/09 ***********************************
     C                   MOVE      EPSPEC        XXSPEC
     C                   MOVE      EPEXT         XXEXT
     C***** END   ADD **** 04/20/09 ***********************************
     C     EPKY3         READE     PEP01                                  60
     C   60              MOVE      'I'           IA(Y)
     C  N60              MOVE      *OFF          *IN57
     C                   ENDIF                                                  *IN57=*OFF
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      12                                   57
     C   57              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     CL(1)         KEYC(1)
     C                   Z-ADD     CL(12)        KEYC(2)
     C***** BEG   ADD **** 04/20/09 ***********************************
     C                   MOVE      SP(1)         KEYS(1)
     C                   MOVE      SP(12)        KEYS(2)
     C                   MOVE      EX(1)         KEYE(1)
     C                   MOVE      EX(12)        KEYE(2)
     C***** END   ADD **** 04/20/09 ***********************************
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
     C     EPKY2         SETLL     PEP01
     C                   READP     PEP01                                  60
     C*
     C*   GET A PREV. RECORD WITH AN ACTIVE STATUS, OR A RECORD IN THE
     C*   CONTRACTORS CLASS FILE WHICH HAS BEEN CHANGED FROM ACTIVE TO
     C*   INACTIVE STATUS
     C*
     C                   MOVE      '0'           *IN57
     C     *IN60         DOWEQ     '0'
     C     EPSTAT        ANDEQ     'I'
     C     *IN57         ANDEQ     '0'
     C                   Z-ADD     1             Y
     C     EPCLAS        LOOKUP    CLS(Y)                                 57
     C     *IN57         IFEQ      '0'                                          NOT FOUND
     C                   READP     PEP01                                  60
     C                   END
     C                   END
     C*
     C  N60EPCO#         COMP      XXCO#                              6060
     C  N60EPFND         COMP      XXFND                              6060
     C  N60EPEMP#        COMP      SCEMP#                             6060
     C  N60EPDIV         COMP      SCDIV                              6060
     C  N60EPFYR         COMP      XXFYR                              6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  57
     C   57              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     CL(1)         KEYC(1)
     C                   Z-ADD     CL(12)        KEYC(2)
     C***** BEG   ADD **** 04/20/09 ***********************************
     C                   MOVE      SP(1)         KEYS(1)
     C                   MOVE      SP(12)        KEYS(2)
     C                   MOVE      EX(1)         KEYE(1)
     C                   MOVE      EX(12)        KEYE(2)
     C***** END   ADD **** 04/20/09 ***********************************
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
     C     EPKY          SETLL     PEP01
     C     EPKY          READE     PEP01                                  60
     C*
     C*   GET A RECORD WITH AN ACTIVE STATUS, OR A RECORD IN THE
     C*   CONTRACTORS CLASS FILE WHICH HAS BEEN CHANGED FROM ACTIVE TO
     C*   INACTIVE STATUS
     C*
     C                   MOVE      '0'           *IN57
     C     *IN60         DOWEQ     '0'
     C     EPSTAT        ANDEQ     'I'
     C     *IN57         ANDEQ     '0'
     C                   Z-ADD     1             Y
     C     EPCLAS        LOOKUP    CLS(Y)                                 57
     C     *IN57         IFEQ      '0'                                          NOT FOUND
     C     EPKY          READE     PEP01                                  60
     C                   END
     C                   END
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
     C     EPKY2         SETGT     PEP01
     C     EPKY          READE     PEP01                                  60
     C*
     C*   GET A RECORD WITH AN ACTIVE STATUS, OR A RECORD IN THE
     C*   CONTRACTORS CLASS FILE WHICH HAS BEEN CHANGED FROM ACTIVE TO
     C*   INACTIVE STATUS
     C*
     C                   MOVE      '0'           *IN57
     C     *IN60         DOWEQ     '0'
     C     EPSTAT        ANDEQ     'I'
     C     *IN57         ANDEQ     '0'
     C                   Z-ADD     1             Y
     C     EPCLAS        LOOKUP    CLS(Y)                                 57
     C     *IN57         IFEQ      '0'                                          NOT FOUND
     C     EPKY          READE     PEP01                                  60
     C                   END
     C                   END
     C*
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      12                                   57
     C   57              GOTO      OVERB
     C     DONEB         ENDSR
     C*
     C*  UPDATE RECORDS TO FILE ROUTINE.
     C*
     C     RITE          BEGSR
     C*
     C     *IN75         IFEQ      '0'                                          RCD LVL CHG
     C                   DO        12            X
     C     X             ADD       60            X2
     C     X             ADD       30            X3
     C     X             ADD       42            X4
     C     *IN(X3)       IFEQ      '1'                                          VALIDATE INACTIVE
     C                   Z-ADD     1             Y                                  STATUS
     C     CL(X)         LOOKUP    CLS(Y)                                 57
     C     *IN57         IFEQ      '1'
     C     IA(Y)         ANDEQ     'I'
     C     AM(X)         ANDNE     0
     C     HR(X)         ANDNE     0
     C                   MOVE      '1'           *IN(X2)                        ERROR ON AMT
     C                   MOVE      '1'           *IN(X4)                        ERROR ON HRS
     C                   MOVE      '1'           *IN79                          ERR. ON INACTIVE
     C     *IN79         CABEQ     '1'           STRT
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C     *IN75         IFEQ      '1'                                          RCD LVL CHG
     C                   DO        12            X
     C     X             ADD       80            X1                2 0
     C     X             ADD       60            X2                2 0
     C     X             ADD       30            X3                2 0
     C     X             ADD       42            X4                2 0
     C*
     C     *IN(X1)       IFEQ      '1'                                          FLD LVL CHG
     C     *IN(X3)       ANDEQ     '1'                                          VALID FIELD
     C*
     C     AM(X)         IFLT      *ZERO                                        VALIDATE AMT
     C                   MOVE      '1'           *IN(X2)                        ERROR ON AMT
     C                   MOVE      '1'           ERSL              1
     C                   END
     C*
     C     HR(X)         IFLT      *ZERO                                        VALIDATE HRS
     C                   MOVE      '1'           *IN(X4)                        ERROR ON HRS
     C                   MOVE      '1'           ERSL
     C                   END
     C*
     C     AM(X)         IFEQ      0                                            VALIDATE AMT
     C     HR(X)         ANDNE     0                                             & HRS TO BE
     C     AM(X)         ORNE      0                                             MUTUALLY
     C     HR(X)         ANDEQ     0                                             INCLUSIVE
     C***  Begin Add  ***  06/08/04  **************************************
     C     FL(X)         ANDEQ     'Y'                                           Contractor class
     C***  End   Add  ***  06/08/04  **************************************
     C                   MOVE      '1'           *IN(X2)                        ERROR ON AMT
     C                   MOVE      '1'           *IN(X4)                        ERROR ON HRS
     C                   MOVE      '1'           *IN78                          ERR. ON MUT. INC.
     C                   MOVE      '1'           ERSL
     C                   END
     C*
     C                   END
     C                   END
     C*
     C*
     C     ERSL          IFEQ      '1'                                          ERROR OCCURED
     C                   MOVE      '0'           ERSL
     C                   GOTO      STRT
     C                   END
     C*
     C                   DO        12            X
     C*
     C     X             ADD       80            X1
     C     *IN(X1)       IFEQ      '1'
     C                   MOVE      CL(X)         XXCLAS            5 0
     C***** BEG   ADD **** 04/20/09 ***********************************
     C                   MOVE      SP(X)         XXSPEC            1
     C                   MOVE      EX(X)         XXEXT             5
     C***** END   ADD **** 04/20/09 ***********************************
     C                   MOVE      FL(X)         XXFLAG
     C     CCKY          CHAIN     PCC01                              55
     C   55              MOVE      XXCO#         CCCO#
     C   55              MOVE      XXFND         CCFND
     C   55              MOVE      XXFYR         CCFYR
     C   55              MOVE      SCEMP#        CCEMP#
     C   55              MOVE      SCDIV         CCDIV
     C   55              MOVE      CL(X)         CCCLAS
     C***** BEG   ADD **** 04/20/09 ***********************************
     C                   MOVE      SP(X)         CCSPEC
     C                   MOVE      EX(X)         CCEXT
     C***** END   ADD **** 04/20/09 ***********************************
     C                   MOVE      AM(X)         CCPAMT
     C                   MOVE      HR(X)         CCHRS
     C   55              MOVE      *ZERO         CCRT
     C   55              MOVE      *ZERO         CCMNPR
     C   55              MOVE      *ZERO         CCAHW
     C   55              MOVE      *ZERO         CCCRPC
     C   55              MOVE      *ZERO         CCCAMT
     C   55              MOVE      *BLANK        CCSTAT
     C                   MOVE      Q1USER        CCUSER
     C                   MOVE      Q1PGM         CCPGM
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        CCTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CCDT
     C*
     C   55              MOVE      FL(X)         CCCCFL
     C*
     C     *IN55         IFEQ      '1'
     C     AM(X)         ANDNE     0
     C***  Begin Del  ***  06/08/04  **************************************
     C********** HR,X      ANDNE0
     C***  End   Del  ***  06/08/04  **************************************
     C                   WRITE     PCC01
     C                   END
     C  N55              UPDATE    PCC01
     C                   END
     C                   END
     C*
     C                   END
     C*
     C     RITEN         ENDSR
     OPCC01     E            PCC01E
**
  More...
ROLL DOWN
   Bottom
