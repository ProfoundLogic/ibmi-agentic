     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR  dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7302R - Agent Loss Ratio Report
     H*****************************************************************
     H* REVISIONS:                                                    ***
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/22/13  C:287 Seq#: SY266_OVR                               *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     H*                                                               *
     F* 06/02/10  C:246  Project: 139WNI PRISS 20                     *
     F*                  Fix the Title issue on this report.          *
     F*                                                               *
     F* 01/08/10  C:238  Project: 139WNI
     F*                  Agent Loss Ratio Report for all Groups
     F*                                                               *
     F* 01/28/10  C:249  Project: SY215                               *
     F*           C:189  Retrieve brand Name                          *
     F*                                                               *
     F* 10/20/09 C:189 PROJECT WC1726                                 *
     F*                CSB-AGT LOSS RATIO/PPR RPT SELECTION CHG       *
     H*                                                               *
     H* 07/30/08 C:183 Project: 46WGA                                 *
     H*               -Added changes for new PARM                     *
     H*               -Added the Summarized Report changes            *
     H*                                                               *
     H* 07/29/08 C:183 Support WA046/64684                            *
     H*               -Corrected changes for incorrect Page#          *
     H*                                                               *
     H* 07/16/07 C:183 Project: 45WFO                                 *
     H*               -Integrated changes from the original project   *
     H*                45WDJ.                                         *
     H*                                                               *
     H*  03/31/05 C:132 PROJECT: WA099  46283                         *
     H*               - Change Company name on file output to be left *
     H*                 justified when file output is requested.      *
     H*                                                               *
     H* 05/12/04 C:62 PROJECT: WC1215                                 *
     H*               CHANGE FROM FYR TO EFFECTIVE DATE RANGE         *
     H*                                                               *
     H* 04/16/02 C:90 PROJECT: SEQ#31829                              *
     H*               WRITE RECORDS TO AGENT LOSS RATIO REPORT        *
     H*               WORKFILE - WFALP.                               *
     H*                                                               *
     H* 12/16/99 C:33 Y2K#: 070                                       *
     H*               EXPAND SIZE OF LOSS RATIO FIELDS.               *
     H*                                                               ***
     H* 04/29/99 C:118 PROJECT: WC577
     H*                MODIFIED ROUTING PAGE TO LEFT ALIGN TITLE AND
     H*                CHANGED ':' POSITIONS.
     H*
     H* 06/26/97 C:62 PROJECT WC710                                   *
     H*               COUNTY CODE NOW IS 4A, STATE + COUNTY.          *
     H*               ALTERNATE COMPANY NAME IF NOT BLANK.            *
     H*                                                               *
     H* 06/27/91 TAA  MODIFIED PRODUCER PERFORMANCE REPORT TO PRINT   *
     H* PROJ: AIG     OUT TRUE CANCEL DATES, I.E. CANCEL DATES > THAN *
     H*               LAST EFFECTIVE DATES AND < RUN DATE.
     H*******************************************************************
     FWXALL     IF   E           K DISK
     F* * * Work file built from W7301R
     F*
     FWMAAL1    IF   E           K DISK
     F* * * Agent Assignment File
     F*
     FWMAGL     IF   E           K DISK
     F* * * Agent Master File
     F*
     FWMAPL     IF   E           K DISK
     F* * * Agent Producer File
     F*
     FWMCML     IF   E           K DISK
     F* * * Claims File
     F*
     FWMEML     IF   E           K DISK
     F* * * Employer File
     F*
     FWMFNL     IF   E           K DISK
     F* * * Fund File
     F*
     FSMORL     IF   E           K DISK
     F* * * Batch Routing File
     F*
     FSMCOL     IF   E           K DISK
     F* * * Company File
     F*
     FWMUPL     IF   E           K DISK
     F* * * User Parameter File
     F*
     FWDELL     IF   E           K DISK
     F* * * EMPLOYER DETAIL FILE
     F*
      *** BEG CHG **** 07/16/07 ************************************
     F*****QSYSPRT O   F     196     OF     PRINTER                        UC
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F                                     USROPN
      *** END CHG **** 07/16/07 ************************************
     F*** BEG CHG **** 07/30/08 ************************************
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOB)
     F                                     USROPN
     F*** END CHG **** 07/30/08 ************************************
     F* * * Output File
      **** BEGIN ADD **** 04/16/02 ************************************
     FWMCPL     IF   E           K DISK
     FWFALP     O    E             DISK
     F                                     RENAME(PAL01:PAL02)
     F* * * AGENT LOSS RATIO REPORT WORKFILE
      **** END   ADD **** 04/16/02 ************************************
     F*** BEG   ADD *** 01/08/10 **************************************
     FWXGSL     IF   E           K DISK
     F                                     RENAME(PGS01:PGS00)

     D*** END   ADD *** 01/08/10 **************************************
     F*
     D*
     D*** BEG  ADD *** 01/08/10 **************************************
     D GRP             S              3    DIM(999)
     D GRPX            S              4    DIM(32)
     D*** END  ADD *** 01/08/10 **************************************
     D                 DS
     D  CMT                    1     20
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
     D  CMA                    1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  CMR                    1     90  2
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
     D                 DS
     D  EMC                    1    035  0
     D                                     DIM(5)
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  EME                    1    035  0
     D                                     DIM(5) ASCEND
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#            5 0
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C*  AGENT PRODUCER FILE
     C*
     C     APKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C                   KFLD                    XXPROD            5 0
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXYR              3 0
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXYR
     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      **** BEGIN ADD **** 04/16/02 ************************************
     C*
     C*  CHAPTER FILE
     C*
     C     CPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCHAP
      **** END   ADD **** 04/16/02 ************************************
     C*
     C     *LIKE         DEFINE    EMDT          XXYMD
     C     *LIKE         DEFINE    EMDT          CANDTE
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C*** BEGIN ADD *** 06/26/97 **************************************
     C     *LIKE         DEFINE    FNANAM        XXCONM
     C*** END   ADD *** 06/26/97 **************************************
     C*
      ***** BEG ADD ***** 03/31/05 ************************************
     C     *LIKE         DEFINE    ALCNAM        ZZCONM
      ***** END ADD ***** 03/31/05 ************************************
     C*
      **** BEGIN ADD **** 04/16/02 ************************************
     C     *LIKE         DEFINE    ALCHAP        XXCHAP
      **** END   ADD **** 04/16/02 ************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*** BEGIN CHG *** 05/12/04 **************************************
     C**********           PARM           QQFYR   3
     C                   PARM                    QQFRDT            7
     C                   PARM                    QQTODT            7
     C*** END   CHG *** 05/12/04 **************************************
     C                   PARM                    QQDTE             8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   PARM                    QQOUTP            1
      **** END   ADD **** 04/16/02 ************************************
     C**** BEG ADD ****** 07/30/08 ************************************
     C                   PARM                    QQRTYP            1
     C**** END ADD ****** 07/30/08 ************************************
      *** BEG ADD **** 10/20/09 ************************************
     C                   PARM                    QQAGT#            5
     C                   PARM                    QQPROD            5
      *** END ADD **** 10/20/09 ************************************
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C*** BEG ADD *** 06/02/10 **************************************
     C                   MOVE      *ZERO         SVFND             3 0
     C*** END ADD ***** 06/02/10 **************************************
     C*** BEG ADD ***** 01/08/10 **************************************
     C                   MOVE      '000'         QQFND
     C*** END ADD ***** 01/08/10 **************************************
     C                   MOVE      QQFND         XXFND             3 0
     C*** BEGIN CHG *** 05/12/04 **************************************
     C**********           MOVE QQFYR     XXFYR   30
     C                   MOVE      QQFRDT        XXFRDT            7 0
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FRDT            8 0
     C                   MOVE      QQTODT        XXTODT            7 0
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TODT            8 0
     C*** END   CHG *** 05/12/04 **************************************
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      QQDTE         X1DATE            8 0
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXDATE            6 0
     C*
      *** BEG ADD **** 10/20/09 ************************************
     C                   Z-ADD     0             X@AGT#            5 0
     C                   Z-ADD     0             X@PROD            5 0
     C*
     C     Q1PRMS        IFGT      9
     C*
     C                   MOVE      QQAGT#        X@AGT#
     C                   MOVE      QQPROD        X@PROD
     C*
     C                   ENDIF
     C*
      *** END ADD **** 10/20/09 ************************************
     C*
     C* MOVE ENTRY PARAMETERS INTO REPORT SELECTION FIELDS
     C*
     C     QQCO#         IFEQ      *ZEROS
     C                   MOVEL     'ALL'         RPCO#             3
     C                   ELSE
     C                   MOVEL     QQCO#         RPCO#
     C                   END
     C     QQFND         IFEQ      *ZEROS
      *** BEG CHG **** 01/08/10 ************************************
     C*                    MOVEL'ALL'     RPFND   3
     C                   EXSR      LDRTFD
      *** END CHG **** 01/08/10 ************************************
     C                   ELSE
     C                   MOVEL     QQFND         RPFND
     C                   END
     C*** BEGIN DEL *** 05/12/04 **************************************
     C********** QQFYR     IFEQ *ZEROS
     C**********           MOVEL'ALL'     RPFYR   3
     C**********           ELSE
     C**********           MOVELQQFYR     RPFYR
     C**********           END
     C*** END   DEL *** 05/12/04 **************************************
     C*** BEGIN ADD *** 05/12/04 **************************************
      *** BEG CHG **** 10/20/09 ************************************
     C**************       MOVE QQFRDT    SRDTI
     C                   MOVE      QQFRDT        XXFRDT            7 0
     C                   Z-ADD     XXFRDT        SRDTI
      *** END CHG **** 10/20/09 ************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         RPFRDT            8 0
      *** BEG CHG **** 10/20/09 ************************************
     C***********          MOVE QQTODT    SRDTI
     C                   MOVE      QQTODT        XXTODT            7 0
     C                   Z-ADD     XXTODT        SRDTI
      *** END CHG **** 10/20/09 ************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         RPTODT            8 0
     C*** END   ADD *** 05/12/04 **************************************
     C*** BEG ADD ***** 07/30/08 **************************************
     C                   SELECT
     C     QQRTYP        WHENEQ    'D'
     C                   MOVEL(P)  'DETAIL'      X1RTYP           10
     C     QQRTYP        WHENEQ    'S'
     C                   MOVEL(P)  'SUMMARY'     X1RTYP
     C     QQRTYP        WHENEQ    'B'
     C                   MOVEL(P)  'BOTH'        X1RTYP
     C                   ENDSL
     C*** END ADD ***** 07/30/08 **************************************
     C*
     C                   Z-ADD     XXDATE        RPDATE            6 0
     C*
      *** BEG ADD **** 10/20/09 ************************************
     C     X@AGT#        IFNE      0
     C*
     C                   MOVEL(P)  X@AGT#        XXNUM
     C                   MOVEL(P)  *BLANKS       EDTMSK
     C*
     C                   CALL      'WA003R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C                   MOVEL     XXNUM         @@AGT#            8
     C                   MOVEL     @@AGT#        RPAGT#            5
      *
     C                   ELSE
     C                   MOVEL     '*ALL   '     RPAGT#
      *
     C                   END
      *
     C     X@PROD        IFNE      0
     C*
     C                   MOVEL(P)  X@PROD        XXNUM
     C                   MOVEL(P)  *BLANKS       EDTMSK
     C*
     C                   CALL      'WA003R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C                   MOVEL     XXNUM         @@PROD            8
     C                   MOVEL     @@PROD        RPPROD            5
      *
     C                   ELSE
     C                   MOVEL     '*ALL   '     RPPROD
      *
     C                   END
      *** END ADD **** 10/20/09 ************************************
      *
     C*** BEG ADD *** 01/08/10 **************************************
     C                   EXSR      SR1ST
     C     FNKEY         SETLL     PGS00
     C     COKEY         READE     PGS00                                  50
     C*
     C     *IN50         DOWEQ     *OFF
     C     GSIEFL        IFEQ      'I'
     C                   MOVE      GSCO#         XXCO#
     C                   MOVE      GSFND         XXFND
      *** BEG ADD **** 06/02/10 ************************************
     C     XXFND         IFNE      SVFND
     C     SVFND         ANDNE     0
     C                   MOVE      *ON           *INOF
     C                   ENDIF
     C                   MOVE      XXFND         SVFND
      *** END ADD **** 06/02/10 ************************************
     C                   EXSR      PROC
     C                   ENDIF
     C     COKEY         READE     PGS00                                  50
     C                   ENDDO
     C                   MOVE      *ON           *INOB
     C                   MOVE      *ON           *INOF
     C                   EXSR      SUBL4
     C*
      /Free
       //***** Beg Add ***** 08/26/14 *******************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          If qqOutp <> 'F' and qqRtyp <> 'S';
             Except Route;
          Endif;
       Endif;
       xxPrt = '1';
       Chain orKey POR01;
       If %found;
          If qqOutp <> 'F' and qqRtyp <> 'D';
             Except Route1;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 *******************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     CSR   PROC          BEGSR
     C*** END ADD *** 01/08/10 **************************************
     C*  GET FUND NAME
     C     FNKEY         CHAIN     PFN01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C                   ELSE
     C                   MOVE      *BLANK        XFNAME
     C                   END
     C*** BEGIN ADD *** 06/26/97 **************************************
     C     FNANAM        IFNE      *BLANKS
     C*
     C                   MOVEL     FNANAM        XXCONM
     C*
     C                   ELSE
     C*
     C                   MOVEL     CONAME        XXCONM
     C*
     C                   ENDIF
     C*
      ***** BEG ADD ***** 03/31/05 ************************************
     C                   MOVEL(P)  XXCONM        ZZCONM
      ***** END ADD ***** 03/31/05 ************************************
     C*
     C                   MOVEL(P)  XXCONM        ##DATA
     C*
      ***** BEG ADD ***** 01/08/10 ************************************
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL(P)  FNANAM        ##DATA
     C                   ENDIF
      ***** END ADD ***** 01/08/10 ************************************
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      50            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXCONM
     C*
     C*** END   ADD *** 06/26/97 **************************************
     C*
     C*
     C*  READ AGENT LOSS RATIO WORK FILE
     C*
     F*** BEG CHG **** 01/08/10 ************************************
     C     FNKEY         SETLL     PAL01
     C*                    READ PAL01                    50
     C     FNKEY         READE     PAL01                                  50
     F*** END CHG **** 01/08/10 ************************************
     C*
     C     *IN50         DOWEQ     '0'
     C     SVAGNT        IFNE      ALAGT#
     C     SVAGNT        ANDNE     *ZERO
     C                   EXSR      SUBL1
     C                   EXSR      SUBL2
     C*
     C                   Z-ADD     ALAGT#        SVAGNT
     C*
     C                   EXSR      SUBHD
     C*
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C**** BEG ADD ****** 07/30/08 ************************************
     C     QQRTYP        IFNE      'S'
     C**** END ADD ****** 07/30/08 ************************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
     C**** BEG ADD ****** 07/30/08 ************************************
     C                   ENDIF
     C**** END ADD ****** 07/30/08 ************************************
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C                   ELSE
     C     SVPROD        IFNE      ALPROD
     C     SVPROD        ANDNE     *ZERO
     C                   EXSR      SUBL1
     C                   EXSR      SUBHD
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C**** BEG ADD ****** 07/30/08 ************************************
     C     QQRTYP        ANDNE     'S'
     C**** END ADD ****** 07/30/08 ************************************
     C                   EXCEPT    HEAD2
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C                   END
     C                   END
     C**
     C                   Z-ADD     ALAGT#        SVAGNT            5 0
     C                   Z-ADD     ALPROD        SVPROD            5 0
     C*
     C*  READ EMPLOYER FILE
     C                   Z-ADD     ALEMP#        XXEMP#
     C                   Z-ADD     ALDIV         XXDIV
     C                   Z-ADD     ALFYR         XXYR
     C*** BEGIN ADD *** 05/12/04 **************************************
     C                   Z-ADD     ALFYR         XXFYR             3 0
     C*** END   ADD *** 05/12/04 **************************************
     C*
     C*  CHAIN TO THE EMPLOYER DETAIL FILE, IF NO HIT DO NOT
     C*  INCLUDE ON REPORT
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     EMKEY         CHAIN     PEM01                              51
     C     *IN51         IFEQ      '0'
     C*
     C                   Z-ADD     *ZERO         CANDTE
     C                   ADD       1             EMPTOT            5 0
     C                   MOVEL     EMNAME        ENAME            24
     C                   Z-ADD     5             X                 2 0
     C     CANDTE        DOWEQ     *ZERO
     C     X             ANDGT     *ZERO
     C     EMC(X)        IFEQ      *ZERO
     C                   SUB       1             X
     C                   ELSE
     C                   Z-ADD     EMC(X)        CANDTE
     C                   END
     C                   END
     C*  IF THERE IS AN EFFECTIVE DATE AFTER THE LAST CANCEL DATE DO
     C*  NOT PRINT CANCEL DATE. IF X=5 PRINT THIS CANCEL DATE.
     C                   Z-ADD     0             Z
     C     X             IFLT      5
     C     X             ADD       1             Z                 2 0
     C*
     C     EME(Z)        IFNE      *ZEROS
     C                   Z-ADD     0             CANDTE
     C                   END
     C*
     C                   END
     C*
     C*  IF  CANCEL DATE IS  > OR = RUN DATE DO NOT PRINT CANCEL DATE.
     C*
     C     CANDTE        IFGE      XXYMD
     C                   Z-ADD     0             CANDTE
     C                   END
     C                   Z-ADD     CANDTE        SRDTI
     C*** BEGIN CHG *** 05/12/04 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     CANMDY  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CANMDY            8 0
     C*** END   CHG *** 05/12/04 **************************************
     C*** BEGIN CHG *** 05/12/04 **************************************
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFFMDY            8 0
     C*** END   CHG *** 05/12/04 **************************************
     C*
     C                   END
     C*
     C*****************************************************************
     C*
     C                   ADD       1             L1TOT             5 0
     C*
     C*****************************************************************
     C*
     C*  READ CLAIM FILE
     C*
     C     CMKEY         SETLL     PCM01
     C     CMKEY         READE     PCM01                                  52
     C     *IN52         DOWEQ     '0'
     C*
     C     CMCO#         IFNE      EMCO#
     C     CMFND         ORNE      EMFND
     C     CMEMP#        ORNE      EMEMP#
     C     CMDIV         ORNE      EMDIV
     C                   MOVE      '1'           *IN52
     C                   END
     C                   ADD       1             EMPCLM            5 0
     C*
     C****************************************************************
     C**
     C                   Z-ADD     *ZERO         TOTPMT
     C                   Z-ADD     *ZERO         TOTRES
     C**
     C                   Z-ADD     1             R
     C                   DO        10            R                 2 0
     C**
     C**  ACCUMULATE PAYMENT AMOUNTS
     C                   ADD       CMA(R)        TOTPMT            9 2
     C**
     C**  ACCUMULATE RESERVE AMOUNTS
     C                   ADD       CMR(R)        TOTRES            9 2
     C**
     C                   END
     C                   ADD       TOTPMT        EMPINC           11 2
     C                   ADD       TOTRES        EMPINC
     C                   ADD       TOTPMT        EMPPMT           11 2
     C                   ADD       TOTRES        EMPRES           11 2
     C*
     C     CMKEY         READE     PCM01                                  52
     C                   END
     C*
     C                   ADD       EMPCLM        L1TOTC            5 0
     C                   ADD       EMPPMT        L1ITOT           11 2
     C                   ADD       EMPRES        L1ITOT
     C                   ADD       EMPPMT        L1PTOT           11 2
     C                   ADD       EMPRES        L1RTOT           11 2
      *** BEG ADD **** 07/16/07 **********************************************
     C                   ADD       ALPMTS        L1PPMT
      *** END ADD **** 07/16/07 **********************************************
     C**
     C*  CALCULATE PREMIUM TO DATE
     C                   EXSR      SUBPR
     C                   ADD       PRMYTD        L1PREM           11 2
     C                   Z-ADD     FREQ          L1FREQ            4 2
     C***  BEIGN CHG  ***  12/16/99  *********************************
     C**********           Z-ADDLOSS      L1LOSS  62
     C                   Z-ADD     LOSS          L1LOSS            7 2
     C***  END   CHG  ***  12/16/99  *********************************
     C**
     C     *INOF         IFEQ      '1'
     C                   EXSR      SUBHD
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C**** BEG ADD ****** 07/30/08 ************************************
     C     QQRTYP        IFNE      'S'
     C**** END ADD ****** 07/30/08 ************************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
     C**** BEG ADD ****** 07/30/08 ************************************
     C                   ENDIF
     C**** END ADD ****** 07/30/08 ************************************
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C                   MOVE      '0'           *INOF
     C                   END
     C**** BEG ADD ****** 07/30/08 ************************************
     C     *INOB         IFEQ      '1'
     C                   EXSR      SUBHD
     C     QQOUTP        IFNE      'F'
     C     QQRTYP        IFNE      'D'
     C                   MOVE      *INOB         *IN65
     C                   EXCEPT    HEAD1
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      '0'           *IN65
     C                   MOVE      '0'           *INOB
     C                   ENDIF
     C**** END ADD ****** 07/30/08 ************************************
     C**
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'P'
     C                   EXSR      FILE
     C                   ENDIF
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C**** BEG ADD ****** 07/30/08 ************************************
     C     QQRTYP        ANDNE     'S'
     C**** END ADD ****** 07/30/08 ************************************
     C                   EXCEPT    DETAIL
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C                   END
     C**** BEG ADD ****** 07/30/08 ************************************
     C                   Z-ADD     0             PRMYTD
     C                   Z-ADD     0             EMPCLM
     C                   Z-ADD     0             EMPPMT
     C                   Z-ADD     0             EMPINC
     C                   Z-ADD     0             EMPRES
     C**** END ADD ****** 07/30/08 ************************************
     C**
     C**** BEG CHG ****** 01/08/10 ************************************
     C     FNKEY         READE     PAL01                                  50
     C*                    READ PAL01                    50
     C**** END CHG ****** 01/08/10 ************************************
     C                   END
     C**
     C**** BEG DEL ****** 01/08/10 ************************************
     C**                   MOVE '1'       *INLR
     C**** END DEL ****** 01/08/10 ************************************
     C**
     C**** BEG CHG ****** 01/08/10 **TAKE OUT LR CONDITION*************
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C**** BEG ADD ****** 07/30/08 ************************************
     C     QQRTYP        IFNE      'S'
     C**** END ADD ****** 07/30/08 ************************************
     C   OF              EXCEPT    HEAD
     C   OF              EXCEPT    HEAD2
     C**** BEG ADD ****** 07/30/08 ************************************
     C                   ENDIF
     C     QQRTYP        IFNE      'D'
     C                   MOVE      *INOB         *IN65
     C   OB              EXCEPT    HEAD1
     C                   MOVE      '0'           *IN65
     C                   ENDIF
     C**** END ADD ****** 07/30/08 ************************************
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
      **** BEG ADD **** 01/08/10 ************************************
     C*                    SETOF                     OF
     C                   EXSR      SUBL1
     C                   EXSR      SUBL2
      **** END ADD **** 01/08/10 ************************************
     C*
     C*  EXECUTE FUND TOTAL SUBROUTINES
     C*
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C**** BEG ADD ****** 07/30/08 ************************************
     C     QQRTYP        ANDNE     'S'
     C**** END ADD ****** 07/30/08 ************************************
     C                   EXCEPT    HEAD3
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C                   EXSR      SUBL3
      **** END CHG **** 01/08/10 *******LR CONDITION*****************
      **** BEG ADD **** 01/08/10 ************************************
     C                   ENDSR
      **** END   ADD **** 01/08/10 ************************************
     C*==================================================================*
     C*   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
     C*==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08
     C                   END
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C* * * INITIALIZE FIELDS
     C                   TIME                    DSDTTM
     C*
     C***** BEGIN ADD **** 04/29/99 ***********************************
     C                   MOVE      *BLANKS       XXTITL           60
     C***** END ADD ****** 04/29/99 ***********************************
     C***** BEG ADD ****** 07/30/08 ***********************************
     C                   MOVE      *BLANKS       XXTIT1           60
     C***** END ADD ****** 07/30/08 ***********************************
     C*
     C     COKEY         CHAIN     SMCOL                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C* * * OPEN PRINT FILE
     C**** BEG ADD ****** 07/30/08 ************************************
     C     QQRTYP        IFNE      'S'
     C**** END ADD ****** 07/30/08 ************************************
     C                   OPEN      QSYSPRT
     C**** BEG ADD ****** 07/30/08 ************************************
     C                   ENDIF
     C**** END ADD ****** 07/30/08 ************************************
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C***** BEGIN ADD **** 04/29/99 ***********************************
     C*  LEFT JUSTIFY TITLE
     C                   MOVE      *BLANKS       XXTITL
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      XXTITL
     C                   ENDIF
     C                   ENDIF
     C***** END ADD ****** 04/29/99 ***********************************
     C*   PRINT THE DISTRIBUTION INFORMATION
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C**** BEG ADD ****** 07/30/08 ************************************
     C     QQRTYP        ANDNE     'S'
     C**** END ADD ****** 07/30/08 ************************************
      ***** Beg Del ***** 08/26/14 ************************************
     C*********          EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C**** BEG ADD ****** 01/08/10 ************************************
     C                   EXSR      LDGRP1
     C                   EXCEPT    ROUT12
     C**** END ADD ****** 01/08/10 ************************************
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C                   END
     C**** BEG ADD ****** 07/29/08 ************************************
     C                   Z-ADD     0             PAGE1             4 0
     C**** END ADD ****** 07/29/08 ************************************
     C**** BEG ADD ****** 07/30/08 ************************************
     C*
     C* * * OPEN PRINT FILE
     C     QQRTYP        IFNE      'D'
      *
     C                   OPEN      QSYSPRT1
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  LEFT JUSTIFY TITLE
     C                   MOVE      *BLANKS       XXTIT1
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      XXTIT1
     C                   ENDIF
     C                   ENDIF
     C*   PRINT THE DISTRIBUTION INFORMATION
     C     QQOUTP        IFNE      'F'
      ***** Beg Del ***** 08/26/14 ************************************
     C***********        EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 ************************************
     C**** BEG ADD ****** 01/08/10 ************************************
     C                   EXSR      LDGRP2
     C                   EXCEPT    ROU112
     C**** END ADD ****** 01/08/10 ************************************
     C                   ENDIF
     C                   END
      *
     C                   Z-ADD     0             PAGE2             4 0
      *
     C                   ENDIF
     C**** END ADD ****** 07/30/08 ************************************
     C*
     C                   MOVE      '1'           *IN11
      *** BEG ADD **** 07/16/07 ************************************
     C                   Z-ADD     0             L1PPMT           11 2
     C                   Z-ADD     0             L2PPMT           11 2
     C                   Z-ADD     0             L3PPMT           11 2
      *** END ADD **** 07/16/07 ************************************
     C*
     C                   ENDSR
     C*
     C**
     C*==================================================================*
     C*   Premium Calculations             ** Subroutine **   S U B P R  *
     C*==================================================================*
     C*
     CSR   SUBPR         BEGSR
     C**
     C**    CALL PGM TO CALCULATE EARNED PREMIUM YEAR-TO-DATE
     C**                      AND EARNED PREMIUM THIS PERIOD
     C**
     C                   MOVEL     XXCO#         SRCO#             3
     C                   MOVEL     XXFND         SRFND             3
     C                   MOVEL     XXFYR         SRFYR             3
     C                   MOVEL     XXEMP#        SREMP#            9
     C                   MOVEL     XXDIV         SRDIV             5
     C                   MOVE      *BLANK        SRAMT            11
     C                   MOVE      *BLANK        SRAMT2           11
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRPRT1'
     C                   PARM                    SRCO#
     C                   PARM                    SRFND
     C                   PARM                    SRFYR
     C                   PARM                    SREMP#
     C                   PARM                    SRDIV
     C                   PARM                    QQDTE
     C                   PARM                    SRAMT
     C                   PARM                    SRAMT2
     C                   PARM                    QQTERM
     C                   MOVE      SRAMT         PRMYTD           11 2
     C                   MOVE      SRAMT2        PRMCUR           11 2
     C*
     C**   CALCULATE LOSS RATIO
     C     PRMYTD        IFNE      *ZERO
     C     EMPINC        MULT      100           XXITOT           13 2
     C***  BEIGN CHG  ***  12/16/99  *********************************
     C********** XXITOT    DIV  PRMYTD    LOSS    62H
     C     XXITOT        DIV(H)    PRMYTD        LOSS              7 2
     C***  END   CHG  ***  12/16/99  *********************************
     C                   ELSE
     C                   Z-ADD     *ZERO         LOSS
     C                   END
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     PRMYTD        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     EMPCLM        DIV(H)    THPREM        FREQ              4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         FREQ
     C                   END
     C*
     C                   ENDSR
     C**
     C*==================================================================*
     C*   Level 1 Calculations (PRODUCER)  ** Subroutine **   S U B L 1  *
     C*==================================================================*
     C*
     CSR   SUBL1         BEGSR
     C**
     C                   ADD       L1TOT         ATOT              5 0
     C                   ADD       L1PTOT        L2PTOT           11 2
     C                   ADD       L1RTOT        L2RTOT           11 2
     C                   ADD       L1PREM        L2PREM           11 2
     C                   ADD       L1TOTC        L2TOTC            5 0
     C**
     C                   ADD       L1ITOT        L2ITOT           11 2
      *** BEG ADD **** 07/16/07 ************************************
     C                   ADD       L1PPMT        L2PPMT
      *** END ADD **** 07/16/07 ************************************
     C**
     C**   CALCULATE LOSS RATIO
     C     L1PREM        IFNE      *ZERO
     C     L1ITOT        MULT      100           XXITOT
     C***  BEIGN CHG  ***  12/16/99  *********************************
     C********** XXITOT    DIV  L1PREM    L1LOSS  62H
     C     XXITOT        DIV(H)    L1PREM        L1LOSS
     C***  END   CHG  ***  12/16/99  *********************************
     C                   ELSE
     C                   Z-ADD     *ZERO         L1LOSS
     C                   END
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     L1PREM        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     L1TOTC        DIV(H)    THPREM        L1FREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L1FREQ
     C                   END
     C**
     C     *INOF         IFEQ      '1'
     C*  GET FUND NAME
     C     FNKEY         CHAIN     PFN01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C                   ELSE
     C                   MOVE      *BLANK        XFNAME
     C                   END
     C*
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C**** BEG ADD ****** 07/30/08 ************************************
     C     QQRTYP        ANDNE     'S'
     C**** END ADD ****** 07/30/08 ************************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C                   END
     C*
     C                   SETOFF                                       OF
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C**** BEG ADD ****** 07/30/08 ************************************
     C     QQRTYP        ANDNE     'S'
     C**** END ADD ****** 07/30/08 ************************************
     C     L1TOT         IFGT      *ZERO
     C                   EXCEPT    TOTL1
     C                   ENDIF
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C**** BEG ADD ****** 07/30/08 ************************************
     C     QQOUTP        IFNE      'F'
     C     QQRTYP        ANDNE     'D'
     C                   Z-ADD     0             L1TOT
     C                   Z-ADD     0             L1PREM
     C                   Z-ADD     0             L1TOTC
     C                   Z-ADD     0             L1RTOT
     C                   Z-ADD     0             L1PTOT
     C                   Z-ADD     0             L1ITOT
     C                   Z-ADD     0             L1PPMT
     C                   Z-ADD     0             L1LOSS
     C                   Z-ADD     0             L1FREQ
     C                   ENDIF
     C**** END ADD ****** 07/30/08 ************************************
     C                   ENDSR
     C**
     C*==================================================================*
     C*   Level 2 Calculations (AGENT)     ** Subroutine **   S U B L 2  *
     C*==================================================================*
     C*
     CSR   SUBL2         BEGSR
     C**
     C                   ADD       ATOT          FTOT              5 0
     C                   ADD       L2PTOT        L3PTOT           11 2
     C                   ADD       L2RTOT        L3RTOT           11 2
     C                   ADD       L2PREM        L3PREM           11 2
     C                   ADD       L2TOTC        L3TOTC            5 0
     C                   ADD       L2ITOT        L3ITOT           11 2
      *** BEG ADD **** 07/16/07 ************************************
     C                   ADD       L2PPMT        L3PPMT
      *** END ADD **** 07/16/07 ************************************
     C*
     C**   CALCULATE LOSS RATIO
     C     L2PREM        IFNE      *ZERO
     C     L2ITOT        MULT      100           XXITOT
     C***  BEIGN CHG  ***  12/16/99  *********************************
     C********** XXITOT    DIV  L2PREM    L2LOSS  62H
     C     XXITOT        DIV(H)    L2PREM        L2LOSS            7 2
     C***  END   CHG  ***  12/16/99  *********************************
     C                   ELSE
     C                   Z-ADD     *ZERO         L2LOSS
     C                   END
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     L2PREM        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     L2TOTC        DIV(H)    THPREM        L2FREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L2FREQ
     C                   END
     C**** BEG ADD ****** 07/30/08 ************************************
     C                   Z-ADD     ATOT          ATOT1             5 0
     C                   Z-ADD     L2PREM        L2PRE1           11 2
     C                   Z-ADD     L2PPMT        L2PPM1           11 2
     C                   Z-ADD     L2TOTC        L2TOC1            5 0
     C                   Z-ADD     L2RTOT        L2RTO1           11 2
     C                   Z-ADD     L2PTOT        L2PTO1           11 2
     C                   Z-ADD     L2ITOT        L2ITO1           11 2
     C                   Z-ADD     L2LOSS        L2LOS1            7 2
     C                   Z-ADD     L2FREQ        L2FRE1            4 2
     C**** END ADD ****** 07/30/08 ************************************
     C*
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C**** BEG ADD ****** 07/30/08 ************************************
     C     QQRTYP        IFNE      'S'
     C**** END ADD ****** 07/30/08 ************************************
     C   OF              EXCEPT    HEAD
     C   OF              EXCEPT    HEAD2
     C                   SETOFF                                       OF
     C     ATOT          IFGT      *ZERO
     C                   EXCEPT    TOTL2
     C                   ENDIF
     C**** BEG ADD ****** 07/30/08 ************************************
     C                   ENDIF
     C     QQRTYP        IFNE      'D'
     C                   Z-ADD     0             ATOT
     C                   Z-ADD     0             L2PPMT
     C                   Z-ADD     0             L2PREM
     C                   Z-ADD     0             L2PPMT
     C                   Z-ADD     0             L2TOTC
     C                   Z-ADD     0             L2RTOT
     C                   Z-ADD     0             L2PTOT
     C                   Z-ADD     0             L2ITOT
     C                   Z-ADD     0             L2LOSS
     C                   Z-ADD     0             L2FREQ
     C                   MOVE      *INOB         *IN65
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *IN65
     C                   SETOFF                                       OB
     C                   EXCEPT    TOTL21
     C                   ENDIF
     C**** END ADD ****** 07/30/08 ************************************
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C*** BEG   CHG *** 01/08/10 **************************************
     C*                    Z-ADD0         PAGE
     C                   Z-ADD     0             PAGE1
     C*** END   CHG *** 01/08/10 **************************************
     C*                                                    *LJQ 01-20-88
     C*  CHANGED TO CORRECT THE PRINTING OF HEADINGS AT OVERFLOW WHEN
     C*  PRINTING TOTALS                                   *
     C*                                                    *
     C                   EXSR      SUBHD
     C*
     C                   ENDSR
     C**
     C*==================================================================*
     C*   Level 3 Calculations (FUND)      ** Subroutine **   S U B L 3  *
     C*==================================================================*
     C*
     CSR   SUBL3         BEGSR
     C**
      *** BEG ADD **** 01/08/10 ************************************
     C                   ADD       FTOT          RTOT              5 0
     C                   ADD       L3PTOT        L4PTOT           11 2
     C                   ADD       L3RTOT        L4RTOT           11 2
     C                   ADD       L3PREM        L4PREM           11 2
     C                   ADD       L3TOTC        L4TOTC            5 0
     C                   ADD       L3ITOT        L4ITOT           11 2
     C                   ADD       L3PPMT        L4PPMT           11 2
      *** END ADD **** 01/08/10 ************************************
     C*
     C**   CALCULATE LOSS RATIO
     C     L3PREM        IFNE      *ZERO
     C     L3ITOT        MULT      100           XXITOT
     C***  BEIGN CHG  ***  12/16/99  *********************************
     C********** XXITOT    DIV  L3PREM    L3LOSS  62H
     C     XXITOT        DIV(H)    L3PREM        L3LOSS            7 2
     C***  END   CHG  ***  12/16/99  *********************************
     C                   ELSE
     C                   Z-ADD     *ZERO         L3LOSS
     C                   END
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     L3PREM        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     L3TOTC        DIV(H)    THPREM        L3FREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L3FREQ
     C                   END
     C**** BEG ADD ****** 01/08/10 ************************************
     C                   Z-ADD     FTOT          FTOT1             5 0
     C                   Z-ADD     L3PREM        L3PRE1           11 2
     C                   Z-ADD     L3PPMT        L3PPM1           11 2
     C                   Z-ADD     L3TOTC        L3TOC1            5 0
     C                   Z-ADD     L3RTOT        L3RTO1           11 2
     C                   Z-ADD     L3PTOT        L3PTO1           11 2
     C                   Z-ADD     L3ITOT        L3ITO1           11 2
     C                   Z-ADD     L3LOSS        L3LOS1            7 2
     C                   Z-ADD     L3FREQ        L3FRE1            4 2
     C**** END ADD ****** 01/08/10 ************************************
     C*
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C**** BEG ADD ****** 07/30/08 ************************************
     C     QQRTYP        ANDNE     'S'
     C**** END ADD ****** 07/30/08 ************************************
     C**** BEG ADD ****** 01/08/10 ************************************
     C   OF              EXCEPT    HEAD
     C   OF              EXCEPT    HEAD2
     C                   SETOFF                                       OF
     C**** END ADD ****** 01/08/10 ************************************
     C                   EXCEPT    TOTL3
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C**** BEG ADD ****** 01/08/10 ************************************
     C     QQRTYP        IFNE      'D'
     C                   MOVE      *INOB         *IN65
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *IN65
     C                   SETOFF                                       OB
     C                   EXCEPT    TOTL31
     C                   ENDIF
     C                   Z-ADD     0             FTOT
     C                   Z-ADD     0             L3PPMT
     C                   Z-ADD     0             L3PREM
     C                   Z-ADD     0             L3PPMT
     C                   Z-ADD     0             L3TOTC
     C                   Z-ADD     0             L3RTOT
     C                   Z-ADD     0             L3PTOT
     C                   Z-ADD     0             L3ITOT
     C                   Z-ADD     0             L3LOSS
     C                   Z-ADD     0             L3FREQ
     C                   Z-ADD     0             PAGE
     C                   MOVE      '1'           *INOB
     C**** END ADD ****** 01/08/10 ************************************
     C                   ENDSR
     C**
      **** BEG ADD **** 01/08/10 ************************************
     C*==================================================================*
     C*   Level 4 Calculations             ** Subroutine **   S U B L 4  *
     C*==================================================================*
     C*
     CSR   SUBL4         BEGSR
     C**
     C*
     C**   CALCULATE LOSS RATIO
     C     L4PREM        IFNE      *ZERO
     C     L4ITOT        MULT      100           XXITOT
     C     XXITOT        DIV(H)    L4PREM        L4LOSS            7 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L4LOSS
     C                   END
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     L4PREM        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     L4TOTC        DIV(H)    THPREM        L4FREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L4FREQ
     C                   END
     C*
     C     QQOUTP        IFNE      'F'
     C     QQRTYP        ANDNE     'S'
     C                   EXCEPT    TOTL4
     C                   ENDIF
     C     QQOUTP        IFNE      'F'
     C     QQRTYP        ANDNE     'D'
     C                   EXCEPT    TOTL41
     C                   ENDIF
     C                   Z-ADD     0             PAGE
     C                   ENDSR
      **** END ADD **** 01/08/10 ************************************
     C*==================================================================*
     C*   Calculations for Headings        ** Subroutine **   S U B H D  *
     C*==================================================================*
     C*
     CSR   SUBHD         BEGSR
     C**
     C**  GET AGENT NAME VIA AGENT MASTER FILES
     C                   Z-ADD     ALAGT#        XXAGT#
     C**
     C     AAKEY         CHAIN     PAA01                              80
     C                   Z-ADD     AAFEIN        XXFEIN
     C                   Z-ADD     AAMOD         XXMOD
     C**
     C     AGKEY         CHAIN     PAG01                              80
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
     C                   MOVEL     AGMCTY        XXLAST           56
     C                   MOVEL     AGMST         XXSTZP           13
     C                   MOVE      AGMZIP        XXSTZP
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C                   MOVEL     AGMZIP        XXST10           10
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   END
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        AGCS             40
     C*
     C                   Z-ADD     ALPROD        XXPROD
     C*
     C     APKEY         CHAIN     PAP01                              80
     C     *IN80         IFEQ      '0'
     C*
     C**    CALL PGM TO CONCATENATE PRODUCER NAME
     C**
     C                   MOVEL     APLSN         SRLNAM           56
     C                   MOVEL     APFSN         SRFNAM           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     SRLNAM        PNAME            40
     C                   END
     C                   ENDSR
      **** BEGIN ADD *** 04/16/02 *************************************
     C*==================================================================*
     C*   Write records to workfile        ** Subroutine **   F I L E  *
     C*==================================================================*
     C*
     CSR   FILE          BEGSR
     C*
     C                   CLEAR                   PAL02
     C*
     C                   Z-ADD     XXCO#         ALCO#
     C*
      ***** BEG CHG ***** 03/31/05 ************************************
     C***************      MOVELXXCONM    ALCNAM
     C                   MOVEL(P)  ZZCONM        ALCNAM
      ***** END CHG ***** 03/31/05 ************************************
      *
     C                   Z-ADD     XXFND         ALFND
     C                   MOVEL     XFNAME        ALFNAM
     C                   Z-ADD     SVAGNT        ALAGT#
     C                   MOVEL     AGNAME        ALANAM
     C                   Z-ADD     ELFYR         ALFYR
      *
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ALPFDT
      *
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ALPTDT
      *
     C                   Z-ADD     AACHAP        ALACH#
     C                   Z-ADD     AACHAP        XXCHAP
     C*
     C     CPKEY         CHAIN     PCP01                              60
     C*
     C  N60              MOVEL(P)  CPDESC        ALACHD
     C   60              MOVE      *BLANKS       ALACHD
     C                   MOVEL     AGMAD1        ALAAD1
     C                   MOVEL     AGCS          ALACSZ
      *** BEG CHG **** 10/20/09 ************************************
     C*****************    Z-ADDALEMP#    ALEMP#
     C*****************    Z-ADDALDIV     ALDIV
     C                   Z-ADD     EMEMP#        ALEMP#
     C                   Z-ADD     EMDIV         ALDIV
      *** END CHG **** 10/20/09 ************************************
     C                   MOVEL     ENAME         ALENAM
     C                   Z-ADD     EMCHAP        ALECH#
     C                   Z-ADD     EMCHAP        XXCHAP
     C*
     C     CPKEY         CHAIN     PCP01                              60
     C*
     C  N60              MOVEL(P)  CPDESC        ALECHD
     C   60              MOVE      *BLANKS       ALECHD
      *** BEG CHG **** 10/20/09 ************************************
     C***********          Z-ADDALPROD    ALPROD
     C                   Z-ADD     XXPROD        ALPROD
      *** END CHG **** 10/20/09 ************************************
     C                   MOVEL     PNAME         ALPNAM
     C                   Z-ADD     CANDTE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ALCXDT
     C                   Z-ADD     PRMYTD        ALPRTD
     C                   Z-ADD     EMPRES        ALRESV
     C                   Z-ADD     EMPCLM        ALCLMS
     C                   Z-ADD     EMPPMT        ALPAID
     C                   Z-ADD     EMPINC        ALINCR
     C                   Z-ADD     LOSS          ALLOSR
     C                   Z-ADD     FREQ          ALFRQR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         ALDATE
     C                   Z-ADD     DSTIME        ALTIME
     C                   MOVEL     Q1PGM         ALPGM
     C                   MOVEL     Q1USER        ALUSER
      *
     C                   WRITE     PAL02
     C*
     C                   ENDSR
      **** END   ADD *** 04/16/02 *************************************
      **** BEG   ADD *** 01/08/10 *************************************
     CSR   LDRTFD        BEGSR
     C*
     C     FNKEY         SETLL     PGS00
     C     XXCO#         READE     PGS00                                  50
     C*
     C                   Z-ADD     1             I                 3 0
     C     *IN50         DOWEQ     *OFF
     C     GSIEFL        IFEQ      'I'
     C                   MOVEL     GSFND         GRP(I)
     C                   ADD       1             I
     C                   ENDIF
     C     XXCO#         READE     PGS00                                  50
     C                   ENDDO
     C                   ENDSR
     C*
     CSR   LDGRP1        BEGSR
     C                   Z-ADD     1             I
     C                   Z-ADD     1             J                 2 0
     C                   SETON                                        01
     C     I             DOWLT     1000
     C     GRP(I)        ANDGT     *BLANK
     C                   MOVEL     GRP(I)        GRPX(J)
     C                   MOVE      ','           GRPX(J)
     C     J             IFEQ      32
     C                   MOVE      ' '           GRPX(J)
     C                   MOVEA     GRPX          RPFND           128
     C                   EXCEPT    ROUT11
     C                   SETOFF                                       01
     C                   MOVE      *BLANKS       GRPX
     C                   Z-ADD     0             J
     C                   ENDIF
     C                   ADD       1             I
     C                   ADD       1             J
     C                   ENDDO
     C     J             IFLT      32
     C                   SUB       1             J
     C                   MOVE      ' '           GRPX(J)
     C                   MOVEA     GRPX          RPFND
     C                   EXCEPT    ROUT11
     C                   MOVE      *BLANKS       GRPX
     C                   ENDIF
     C                   ENDSR
     CSR   LDGRP2        BEGSR
     C                   Z-ADD     1             I
     C                   Z-ADD     1             J                 2 0
     C                   SETON                                        01
     C     I             DOWLT     1000
     C     GRP(I)        ANDGT     *BLANK
     C                   MOVEL     GRP(I)        GRPX(J)
     C                   MOVE      ','           GRPX(J)
     C     J             IFEQ      32
     C                   MOVE      ' '           GRPX(J)
     C                   MOVEA     GRPX          RPFND           128
     C                   EXCEPT    ROU111
     C                   MOVE      *BLANKS       GRPX
     C                   SETOFF                                       01
     C                   Z-ADD     0             J
     C                   ENDIF
     C                   ADD       1             I
     C                   ADD       1             J
     C                   ENDDO
     C     J             IFLT      32
     C                   SUB       1             J
     C                   MOVE      ' '           GRPX(J)
     C                   MOVEA     GRPX          RPFND
     C                   EXCEPT    ROU111
     C                   MOVE      *BLANKS       GRPX
     C                   ENDIF
     C                   ENDSR
      **** END   ADD *** 01/08/10 *************************************
     C**
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ***********************************
     O************                                8 'XXXXXXXX'
     O************                               20 'XXXXXX'
     O************                               34 'XX      XX'
     O************                               45 'XXXXXXXXXX'
     O************                               56 'XXXXXXXXXX'
     O************                               67 'XXX     XX'
     O************                               77 'XXXXXX'
     O************           ROUTE          1
     O************                                9 'XXXXXXXXX'
     O************                               21 'XXXXXXXX'
     O************                               34 'XX      XX'
     O************                               45 'XXXXXXXXXX'
     O************                               56 'XXXXXXXXXX'
     O************                               67 'XXXX    XX'
     O************                               78 'XXXXXXXX'
     O************           ROUTE          1
     O************                               10 'XX      XX'
     O************                               22 'XX      XX'
     O************                               34 'XX      XX'
     O************                               41 'XX'
     O************                               52 'XX'
     O************                               67 'XXXX    XX'
     O************                               79 'XX      XX'
     O************           ROUTE          1
     O************                               10 'XX      XX'
     O************                               22 'XX      XX'
     O************                               34 'XX      XX'
     O************                               41 'XX'
     O************                               52 'XX'
     O************                               67 'XX XX   XX'
     O************                               71 'XX'
     O************           ROUTE          1
     O************                                9 'XXXXXXXXX'
     O************                               22 'XX      XX'
     O************                               34 'XX      XX'
     O************                               41 'XX'
     O************                               52 'XX'
     O************                               67 'XX XX   XX'
     O************                               71 'XX'
     O************           ROUTE          1
     O************                                8 'XXXXXXXX'
     O************                               22 'XX      XX'
     O************                               34 'XX      XX'
     O************                               41 'XX'
     O************                               52 'XX'
     O************                               67 'XX  XX  XX'
     O************                               71 'XX'
     O************           ROUTE          1
     O************                                4 'XXXX'
     O************                               22 'XX      XX'
     O************                               34 'XX      XX'
     O************                               41 'XX'
     O************                               52 'XX'
     O************                               67 'XX  XX  XX'
     O************                               79 'XX    XXXX'
     O************           ROUTE          1
     O************                                5 'XXXXX'
     O************                               22 'XX      XX'
     O************                               34 'XX      XX'
     O************                               41 'XX'
     O************                               52 'XX'
     O************                               67 'XX   XX XX'
     O************                               79 'XX    XXXX'
     O************           ROUTE          1
     O************                                6 'XX XXX'
     O************                               22 'XX      XX'
     O************                               34 'XX      XX'
     O************                               41 'XX'
     O************                               52 'XX'
     O************                               67 'XX   XX XX'
     O************                               79 'XX      XX'
     O************           ROUTE          1
     O************                                7 'XX  XXX'
     O************                               22 'XX      XX'
     O************                               34 'XX      XX'
     O************                               41 'XX'
     O************                               52 'XX'
     O************                               67 'XX    XXXX'
     O************                               79 'XX      XX'
     O************           ROUTE          1
     O************                                8 'XX   XXX'
     O************                               21 'XXXXXXXX'
     O************                               33 'XXXXXXXX'
     O************                               41 'XX'
     O************                               56 'XXXXXXXXXX'
     O************                               67 'XX    XXXX'
     O************                               78 'XXXXXXXX'
     O************           ROUTE          3
     O************                                9 'XX    XXX'
     O************                               20 'XXXXXX'
     O************                               32 'XXXXXX'
     O************                               41 'XX'
     O************                               56 'XXXXXXXXXX'
     O************                               67 'XX     XXX'
     O************                               77 'XXXXXX'
     O************ E            ROUTE          1
     O************                               15 'ROUTE TO:'
     O************   08      ORDIS1              60
     O************      09   UPUSNM              67
     O************ E            ROUTE          1
     O************   08      ORDIS2              60
     O************ E            ROUTE          1
     O************   08      ORDIS3              60
     O************ E            ROUTE          2
     O************   08      ORDIS4              60
      ***** End Del ***** 08/26/14 ***********************************
      ***** Beg Add ***** 08/05/15 ***********************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ***********************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O***** BEGIN CHG ******** 04/29/99 ******************************
     O**********               ORTITL    80
     O                       XXTITL              80
     O***** END CHG ********** 04/29/99 *******************************
      ***** Beg Del ***** 08/26/14 ***********************************
     O************  E            ROUTE          2
     O************                               19 'PROGRAM NAME:'
     O************           ORPNAM              30
     O************                               36 'FILE:'
     O************           ORFIL#              38
     O************                               46 'COPIES:'
     O************           ORCOPY        Z     50
     O************                               56 'HOLD:'
     O************           ORHOLD              61
     O************  E            ROUTE          2
     O************                               19 'OUTPUT QUEUE:'
     O************   08      OROUTQ              30
      ***** End Del ***** 08/26/14 ***********************************
     O*  MLS 1/18/88 ADD INDICATORS AND OUT QUEUE
     O                  09   QQQUE               30
     O                                           37 'FORMS:'
     O                       ORFORM              48
     O                                           56 'SAVE:'
     O                       ORSAVE              61
     O          E            ROUTE          2 33
     O                                           18 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O***** BEGIN CHG ******** 04/29/99 *******************************
     O**********                         26 'COMPANY         :'
     O**********               RPCO#     38
     O                                           26 'COMPANY    :'
     O***  BEGIN CHG  ***  12/16/99  ********************************
     O**********               RPCO#     37
     O                       RPCO#               37
     O***  END   CHG  ***  12/16/99  ********************************
     O***  BEG CHG  ***  01/08/10  ********************************
     O          E            ROUT11         2
     O***  END CHG  ***  01/08/10  ********************************
     O**********                         26 'GROUP           :'
     O**********               RPFND     38
     O               01                          26 'GROUP      :'
     O***  BEGIN CHG  ***  12/16/99  ********************************
     O**********               RPFND     36
     O                       RPFND             +001
     O***  END   CHG  ***  12/16/99  ********************************
     O*** BEGIN CHG *** 05/12/04 **************************************
     O********E* 2             ROUTE
     O**********                         26 'POLICY YEAR     :'
     O**********               RPFYR     38
     O**********                         26 'POLICY YEAR:'
     O***  BEGIN CHG  ***  12/16/99  ********************************
     O**********               RPFYR     36
     O**********               RPFYR     37
     O***  BEG CHG  ***  01/08/10  ********************************
     O          E            ROUT12         2
     O***  END CHG  ***  01/08/10  ********************************
     O**********                         26 'POLICY YEAR     :'
     O**********               RPFYR     38
     O                                           26 'DATE RANGE :'
     O                       RPFRDT            +001 '  /  /    '
     O                                         +001 '-'
     O                       RPTODT            +001 '  /  /    '
     O*** END   CHG *** 05/12/04 **************************************
     O***  END   CHG  ***  12/16/99  ********************************
     O          E            ROUT12         2
     O**********                         26 'RUN DATE        :'
     O**********               RPDATE    38 '  /  /  '
     O                                           26 'RUN DATE   :'
     O***  BEGIN CHG  ***  12/16/99  ********************************
     O**********               X1DATEY   38
     O                       X1DATE        Y     37
     O***  END   CHG  ***  12/16/99  ********************************
     O**** BEG ADD ******* 10/20/09  ********************************
     O          E            ROUT12         2
     O                                           26 'AGENT      :'
     O                       RPAGT#              32
     O          E            ROUT12         2
     O                                           26 'PRODUCER   :'
     O                       RPPROD              32
     O**** END ADD ******* 10/20/09  ********************************
     O**** BEG ADD ******* 07/30/08  ********************************
     O          E            ROUT12         2
     O                                           26 'REPORT TYPE:'
     O                       X1RTYP              37
     O**** END ADD ******* 07/30/08  ********************************
     O***** END CHG ********** 04/29/99 *******************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUT12
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O          E            ROUT12         1 59
     O                       #BRND1
     O          E            ROUT12         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*** BEGIN CHG *** 05/12/04 **************************************
     O********E* 106           HEAD
     O**********               DSTIME    08 '  :  :  '
     O*** BEGIN CHG *** 06/26/97 **************************************
     O**********               CONAME    87
     O**********               XXCONM    91
     O*** END   CHG *** 06/26/97 **************************************
     O**********               DSDATEY  132
     O********E* 1             HEAD
     O**********               Q1PGM     10
     O***** BEGIN CHG ******** 04/29/99 *******************************
     O**********               ORTITL    96
     O**********               XXTITL   115
     O***** END CHG ********** 04/29/99 *******************************
     O**********                        126 'PAGE:'
     O**********               PAGE  Z  132
     O********E* 2             HEAD
     O**********                         72 'POLICY YEAR'
     O**********               XXFYR Z +  1
     O***** BEGIN CHG ******** 04/29/99 *******************************
     O**********                        123 'RUN DATE:'
     O**********               XXDATEY  132
     O**********                        120 'RUN DATE:'
     O**********               X1DATEY  132
     O***** END CHG ********** 04/29/99 *******************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             108
     O                       DSDATE        Y    166
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       XXTITL             132
     O                                          160 'PAGE:'
     O**** BEG CHG ****** 07/29/08 ************************************
     O************             PAGE  Z  166
     O                       PAGE1         Z    166
     O**** END CHG ****** 07/29/08 ************************************
     O          E            HEAD           2
     O                                           76 'DATE RANGE'
     O                       X1FRDT            +  1 '  /  /    '
     O                                         +  1 '-'
     O                       X1TODT            +  1 '  /  /    '
     O                                          154 'RUN DATE:'
     O                       X1DATE        Y    166
     O*** END   CHG *** 05/12/04 **************************************
     O          E            HEAD           2
     O                                              'GROUP:'
     O***** BEGIN CHG ******** 04/29/99 *******************************
     O**********               XXFND Z +  1
     O**********               XFNAME  +  1
     O                       XXFND         Z   +  2
     O                       XFNAME            +  2
     O***** END CHG ********** 04/29/99 *******************************
     O          E            HEAD           1
     O                                            6 'AGENT:'
     O                       SVAGNT        Z     15
     O                       AGNAME              56
     O          E            HEAD           1
     O                       AGMAD1              56
     O          E            HEAD           1
     O                       AGCS                56
     O*** BEGIN CHG *** 05/12/04 **************************************
     O***** BEGIN ADD ******** 04/29/99 *******************************
     O********E* 1             HEAD
     O***** END ADD ********** 04/29/99 *******************************
     O**********                        125 'LOSS'
     O**********                        131 'FREQ'
     O          E            HEAD           1
     O***** BEG CHG ********** 07/16/07 *******************************
     O*                                +149 '     LOSS'
     O                                         +166 '     LOSS'
     O***** END CHG ********** 07/16/07 *******************************
     O                                         +  2 '  FREQ'
     O*** END   CHG *** 05/12/04 **************************************
     O*** BEGIN CHG *** 05/12/04 **************************************
     O********E* 1             HEAD
     O**********                         09 'EMPLOYER'
     O**********                         15 'DIV'
     O**********                       +  1 'NAME'
     O**********                         49 'CANC DAT'
     O**********                         64 'PREM TO DATE'
     O**********                         72 'CLAIMS'
     O**********                         84 'RESERVES'
     O**********                        100 'PAID'
     O***  BEGIN CHG  ***  12/16/99  ********************************
     O**********                        116 'INCURRED'
     O**********                        115 'INCURRED'
     O*****END** CHG  ***  12/16/99  ********************************
     O**********                        125 'RATIO'
     O**********                        131 'RATIO'
     O          E            HEAD           1
     O                                            9 ' EMPLOYER'
     O                                         +  2 '  DIV'
     O                                         +  2 'NAME'
     O                                         + 22 ' YR'
     O                                         +  2 '  EFF DATE'
     O                                         +  2 '   CX DATE'
     O                                         +  2 '  PREM TO DATE'
      *** BEG ADD **** 07/16/07 ************************************
     O                                         +  2 'PRM PTS TO DATE'
      *** END ADD **** 07/16/07 ************************************
     O                                         +  2 'CLAIMS'
     O                                         +  2 '       RESERVES'
     O                                         +  2 '           PAID'
     O                                         +  2 '       INCURRED'
     O                                         +  2 '     RATIO'
     O                                         +  2 ' RATIO'
     O*** END   CHG *** 05/12/04 **************************************
     O          E            HEAD2       1  2
     O                                            9 'PRODUCER:'
     O                       ALPROD        Z     15
     O                       PNAME          B    56
     O*
     O*** BEGIN CHG *** 05/12/04 **************************************
     O********E* 106           HEAD3
     O**********               DSTIME    08 '  :  :  '
     O*** BEGIN CHG *** 06/26/97 **************************************
     O**********               CONAME    87
     O**********               XXCONM    91
     O*** END   CHG *** 06/26/97 **************************************
     O**********               DSDATEY  132
     O********E* 1             HEAD3
     O**********               Q1PGM     10
     O***** BEGIN CHG ******** 04/29/99 *******************************
     O**********               ORTITL    96
     O**********               XXTITL   115
     O***** END CHG ********** 04/29/99 *******************************
     O**********                        126 'PAGE:'
     O**********               PAGE  Z  132
     O********E* 2             HEAD3
     O**********                         72 'POLICY YEAR'
     O**********               XXFYR Z +  1
     O**********                         59 'DATE RANGE'
     O***** BEGIN CHG ******** 04/29/99 *******************************
     O**********                        123 'RUN DATE:'
     O**********               XXDATEY  132
     O**********                        120 'RUN DATE:'
     O**********               X1DATEY  132
     O***** END CHG ********** 04/29/99 *******************************
     O          E            HEAD3          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             108
     O                       DSDATE        Y    166
     O          E            HEAD3          1
     O                       Q1PGM               10
     O                       XXTITL             132
     O                                          160 'PAGE:'
     O**** BEG CHG ****** 07/29/08 ************************************
     O**************           PAGE  Z  166
     O                       PAGE1         Z    166
     O**** END CHG ****** 07/29/08 ************************************
     O          E            HEAD3          2
     O                                           76 'DATE RANGE'
     O                       X1FRDT            +  1 '  /  /    '
     O                                         +  1 '-'
     O                       X1TODT            +  1 '  /  /    '
     O                                          154 'RUN DATE:'
     O                       X1DATE        Y    166
     O*** END   CHG *** 05/12/04 **************************************
     O          E            HEAD3          2
     O                                              'GROUP:'
     O***** BEGIN CHG ******** 04/29/99 *******************************
     O**********               XXFND Z +  1
     O**********               XFNAME  +  1
     O                       XXFND         Z   +  2
     O                       XFNAME            +  2
     O***** END CHG ********** 04/29/99 *******************************
     O*** BEGIN CHG *** 05/12/04 **************************************
     O********E* 1             HEAD3
     O**********                        125 'LOSS'
     O**********                        131 'FREQ'
     O          E            HEAD3          1
     O                                         +149 '     LOSS'
     O                                         +  2 '  FREQ'
     O*** END   CHG *** 05/12/04 **************************************
     O*** BEGIN CHG *** 05/12/04 **************************************
     O********E* 2             HEAD3
     O**********                         64 'PREM TO DATE'
     O**********                         72 'CLAIMS'
     O**********                         84 'RESERVES'
     O**********                        100 'PAID'
     O*****BEGIN CHG  ***  12/16/99  ********************************
     O**********                        116 'INCURRED'
     O**********                        115 'INCURRED'
     O*****END** CHG  ***  12/16/99  ********************************
     O**********                        125 'RATIO'
     O**********                        131 'RATIO'
     O          E            HEAD3          1
     O                                         + 73 '  PREM TO DATE'
      *** BEG ADD **** 07/16/07 ************************************
     O                                         +  3 'PRM PTS TO DATE'
      *** END ADD **** 07/16/07 ************************************
     O                                         +  3 'CLAIMS'
     O                                         +  2 '      RESERVES'
     O                                         +  2 '           PAID'
     O                                         +  2 '       INCURRED'
     O                                         +  2 '     RATIO'
     O                                         +  2 ' RATIO'
     O*** END   CHG *** 05/12/04 **************************************
     O*
     O*** BEGIN CHG *** 05/12/04 **************************************
     O********E* 1             DETAIL
     O**********               ALEMP#Z    9
     O**********               ALDIV Z   15
     O**********               ENAME     40
     O**********               CANMDY B  49 '  /  /  '
     O**********               PRMYTDJB  65
     O**********               EMPCLMZB  71
     O**********               EMPRESJB  85
     O**********               EMPPMTJB 101
     O***  BEGIN CHG  ***  12/16/99  ********************************
     O**********               EMPINCJB 117
     O**********               EMPINCJB 116
     O***  END   CHG  ***  12/16/99  ********************************
     O**********               LOSS  JB 126
     O**********               FREQ  JB 132
     O          E            DETAIL         1
     O                       ALEMP#        Z      9
     O                       ALDIV         Z   +  2
     O                       ENAME             +  2
     O                       ALFYR         Z   +  2
     O                       EFFMDY         B  +  2 '  /  /    '
     O                       CANMDY         B  +  2 '  /  /    '
     O                       PRMYTD        JB  +  2
     O*** BEG ADD ***** 07/16/07 **************************************
     O                       ALPMTS        JB  +  2
     O*** END ADD ***** 07/16/07 **************************************
     O                       EMPCLM        ZB  +  3
     O                       EMPRES        JB  +  2
     O                       EMPPMT        JB  +  2
     O                       EMPINC        JB  +  2
     O                       LOSS          JB  +  2
     O                       FREQ          JB  +  2
     O*** END   CHG *** 05/12/04 **************************************
     O*** BEGIN CHG *** 05/12/04 **************************************
     O********E*11             TOTL1
     O**********                         30 'PRODUCER TOTALS:'
     O**********                         43 'COUNT:'
     O**********               L1TOT ZB  49
     O**********               L1PREMJB  65
     O**********               L1RTOTJB  85
     O**********               L1TOTC3B  71
     O**********               L1PTOTJB 101
     O*****BEGIN CHG  ***  12/16/99  ********************************
     O**********               L1ITOTJB 117
     O**********               L1ITOTJB 116
     O***  END   CHG  ***  12/16/99  ********************************
     O**********               L1LOSSJB 126
     O**********               L1FREQJB 132
     O********E*13             TOTL2
     O**********                         30 'AGENT TOTALS:'
     O**********                         43 'COUNT:'
     O**********               ATOT  ZB  49
     O**********               L2PREMJB  65
     O**********               L2RTOTJB  85
     O**********               L2TOTC3B  71
     O**********               L2PTOTJB 101
     O*****BEGIN CHG  ***  12/16/99  ********************************
     O**********               L2ITOTJB 117
     O**********               L2ITOTJB 116
     O*****END** CHG  ***  12/16/99  ********************************
     O**********               L2LOSSJB 126
     O**********               L2FREQJB 132
     O********E*11             TOTL3
     O**********                         30 'GROUP TOTALS:'
     O**********                         43 'COUNT:'
     O**********               FTOT  ZB  49
     O**********               L3PREMJB  65
     O**********               L3RTOTJB  85
     O**********               L3TOTC3B  71
     O**********               L3PTOTJB 101
     O*****BEGIN CHG  ***  12/16/99  ********************************
     O**********               L3ITOTJB 117
     O**********               L3ITOTJB 116
     O*****END** CHG  ***  12/16/99  ********************************
     O**********               L3LOSSJB 126
     O**********               L3FREQJB 132
     O          E            TOTL1       1  1
     O                                           30 'PRODUCER TOTALS:'
     O                                           43 'COUNT:'
     O                       L1TOT         ZB    49
     O                       L1PREM        JB  + 24
     O*** BEG ADD ******  07/16/07  ********************************
     O                       L1PPMT        JB  +  2
     O*** END ADD ******  07/16/07  ********************************
     O                       L1TOTC        3B  +  3
     O                       L1RTOT        JB  +  2
     O                       L1PTOT        JB  +  2
     O                       L1ITOT        JB  +  2
     O                       L1LOSS        JB  +  2
     O                       L1FREQ        JB  +  2
     O          E            TOTL2       1  3
     O                                           30 'AGENT TOTALS:'
     O                                           43 'COUNT:'
     O                       ATOT          ZB    49
     O                       L2PREM        JB  + 24
     O*** BEG ADD ******  07/16/07  ********************************
     O                       L2PPMT        JB  +  2
     O*** END ADD ******  07/16/07  ********************************
     O                       L2TOTC        3B  +  3
     O                       L2RTOT        JB  +  2
     O                       L2PTOT        JB  +  2
     O                       L2ITOT        JB  +  2
     O                       L2LOSS        JB  +  2
     O                       L2FREQ        JB  +  2
     O          E            TOTL3       1  1
     O                                           30 'GROUP TOTALS:'
     O                                           43 'COUNT:'
     O                       FTOT          ZB    49
     O                       L3PREM        JB  + 24
     O*** BEG ADD ******  07/16/07  ********************************
     O                       L3PPMT        JB  +  2
     O*** END ADD ******  07/16/07  ********************************
     O                       L3TOTC        3B  +  3
     O                       L3RTOT        JB  +  2
     O                       L3PTOT        JB  +  2
     O                       L3ITOT        JB  +  2
     O                       L3LOSS        JB  +  2
     O                       L3FREQ        JB  +  2
     O*** END   CHG *** 05/12/04 **************************************
     O          E            TOTL4       1  1
     O                                           30 'REPORT TOTALS:'
     O                                           44 'COUNT:'
     O                       RTOT          Z     50
     O                       L4PREM        J   + 23
     O*** BEG ADD ******  07/16/07  ********************************
     O                       L4PPMT        J   +  2
     O*** END ADD ******  07/16/07  ********************************
     O                       L4TOTC        3   +  3
     O                       L4RTOT        J   +  2
     O                       L4PTOT        J   +  2
     O                       L4ITOT        J   +  2
     O                       L4LOSS        J   +  2
     O                       L4FREQ        J   +  2
     O*** END   CHG *** 05/12/04 **************************************
     O*** BEG ADD  **** 07/30/08 **************************************
     OQSYSPRT1  E            ROUTE1         1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O*********                                   8 'XXXXXXXX'
     O*********                                  20 'XXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXX     XX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE1         1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXXX    XX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE1         1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XXXX    XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE1         1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE1         1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE1         1
     O*********                                   8 'XXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  71 'XX'
     O********* E            ROUTE1         1
     O*********                                   4 'XXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE1         1
     O*********                                   5 'XXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE1         1
     O*********                                   6 'XX XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE1         1
     O*********                                   7 'XX  XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX    XXXX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE1         1
     O*********                                   8 'XX   XXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  33 'XXXXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX    XXXX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE1         3
     O*********                                   9 'XX    XXX'
     O*********                                  20 'XXXXXX'
     O*********                                  32 'XXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX     XXX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE1         1
     O*********                                  15 'ROUTE TO:'
     O*********      08      ORDIS1              60
     O*********         09   UPUSNM              67
     O********* E            ROUTE1         1
     O*********      08      ORDIS2              60
     O********* E            ROUTE1         1
     O*********      08      ORDIS3              60
     O********* E            ROUTE1         2
     O*********      08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE1         1
     O               08      ORDIS2              60
     O          E            ROUTE1         1
     O               08      ORDIS3              60
     O          E            ROUTE1         2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       XXTIT1              80
      ***** Beg Del ***** 08/26/14 ************************************
     O********* E            ROUTE1         2
     O*********                                  19 'PROGRAM NAME:'
     O*********              ORPNAM              30
     O*********                                  36 'FILE:'
     O*********              ORFIL#              38
     O*********                                  46 'COPIES:'
     O*********              ORCOPY        Z     50
     O*********                                  56 'HOLD:'
     O*********              ORHOLD              61
     O********* E            ROUTE1         2
     O*********                                  19 'OUTPUT QUEUE:'
     O*********      08      OROUTQ              30
     O*  MLS 1/18/88 ADD INDICATORS AND OUT QUEUE
     O*********         09   QQQUE               30
     O*********                                  37 'FORMS:'
     O*********              ORFORM              48
     O*********                                  56 'SAVE:'
     O*********              ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE1         2 33
     O                                           18 'REPORT SELECTIONS:'
     O          E            ROUTE1         2
     O                                           26 'COMPANY    :'
     O                       RPCO#               37
     O          E            ROU111         2
     O               01                          26 'GROUP      :'
     O                       RPFND             +001
     O          E            ROU112         2
     O                                           26 'DATE RANGE :'
     O                       RPFRDT            +001 '  /  /    '
     O                                         +001 '-'
     O                       RPTODT            +001 '  /  /    '
     O          E            ROU112         2
     O                                           26 'RUN DATE   :'
     O                       X1DATE        Y     37
     O          E            ROU112         2
     O                                           26 'REPORT TYPE:'
     O                       X1RTYP              37
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROU112
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O          E            ROU112         1 59
     O                       #BRND1
     O          E            ROU112         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E    65      HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             108
     O                       DSDATE        Y    166
     O          E    65      HEAD1          1
     O                       Q1PGM               10
     O                       XXTIT1             132
     O                                          160 'PAGE:'
     O                       PAGE2         Z    166
     O          E    65      HEAD1          2
     O                                           76 'DATE RANGE'
     O                       X1FRDT            +  1 '  /  /    '
     O                                         +  1 '-'
     O                       X1TODT            +  1 '  /  /    '
     O                                          154 'RUN DATE:'
     O                       X1DATE        Y    166
     O          E    65      HEAD1          2
     O                                              'GROUP:'
     O                       XXFND         Z   +  2
     O                       XFNAME            +  2
     O          E   N65      HEAD1          1
     O                                            6 'AGENT:'
     O                       SVAGNT        Z     15
     O                       AGNAME              56
     O          E   N65      HEAD1          1
     O                                          135 'LOSS'
     O                                         +  2 '  FREQ'
     O          E   N65      HEAD1          1
     O                                           48 ' EMPLOYER CT'
     O                                         +  2 '  PREM TO DATE'
     O                                         +  2 'CLAIMS'
     O                                         +  2 '       RESERVES'
     O                                         +  2 '           PAID'
     O                                         +  2 '       INCURRED'
     O                                         +  2 '     RATIO'
     O                                         +  2 ' RATIO'
     O*
     O          E            TOTL21      1  3
     O                                           30 'AGENT TOTALS:'
     O                       ATOT1         ZB    48
     O                       L2PRE1        JB  +  2
     O                       L2TOC1        3B  +  2
     O                       L2RTO1        JB  +  2
     O                       L2PTO1        JB  +  2
     O                       L2ITO1        JB  +  2
     O                       L2LOS1        JB  +  2
     O                       L2FRE1        JB  +  2
     O*** END ADD  **** 07/30/08 **************************************
     O*** BEG ADD  **** 01/08/10 **************************************
     O          E            TOTL31      1  1
     O                                           30 'GROUP TOTALS:'
     O                       FTOT1         ZB    49
     O                       L3PRE1        JB  + 24
     O                       L3PPM1        JB  +  2
     O                       L3TOC1        3B  +  3
     O                       L3RTO1        JB  +  2
     O                       L3PTO1        JB  +  2
     O                       L3ITO1        JB  +  2
     O                       L3LOS1        JB  +  2
     O                       L3FRE1        JB  +  2
     O          E            TOTL41      1  1
     O                                           30 'REPORT TOTALS:'
     O                                           44 'COUNT:'
     O                       RTOT          ZB    50
     O                       L4PREM        JB  + 23
     O                       L4PPMT        JB  +  2
     O                       L4TOTC        3B  +  3
     O                       L4RTOT        JB  +  2
     O                       L4PTOT        JB  +  2
     O                       L4ITOT        JB  +  2
     O                       L4LOSS        JB  +  2
     O                       L4FREQ        JB  +  2
     O*** END ADD  **** 01/08/10 **************************************
