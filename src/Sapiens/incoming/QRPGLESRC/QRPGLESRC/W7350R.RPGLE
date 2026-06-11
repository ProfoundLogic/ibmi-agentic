     H Copyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7350R - WORK WITH STATISTICAL SUBMISSION
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM: MENU                                            *
     H*                                                               *
     H*  CALLS TO: W7350R02                                           *
     H*            SA020R(CHGNUM) - FORMAT SUBFILE FIELDS             *
     H*                                                               *
     H*  CREATED: 05/19/98  PROGRAMMER: C:87  PROJECT: WC591          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/20/21  C:291 Project: 148WCJ                              *
     H*                  Call new Statistical Submissions program     *
     H*                  WTRPTSTAT. This program will only be pass-   *
     H*                  through to the new program WTRPTSTAT.        *
     H*                                                               *
     H*  12/03/15  C:65  Project: 45WKP                               *
     H*                  If processing for LCTA then use old carrier  *
     H*                  # for extracts prior to 07/2017.             *
     H*                                                               *
     H*  04/23/09  C:65  Project: 139WKB                              *
     H*                  ADD A NEW VIEW TO THE SCREEN TO ORDER THE    *
     H*                  DATA IN RPT MONTH/YEAR AND THEN GROUP, TO    *
     H*                  ALLOW EASIER SELECTIONS FOR A MONTH'S WORTH  *
     H*                  OF DATA OVER MULTIPLE GROUPS.                *
     H*                                                               *
     H*  04/07/09  C:65  Project: 139WKB                              *
     H*                  ADD NEW COLUMNS TO SCREEN FOR NCCI CARRIER # *
     H*                  AND SUBMIT (Y/ ).  THE SUBMIT OPTION WILL    *
     H*                  INSTEAD BE USED TO FLAG ENTRIES TO SUBMIT    *
     H*                  AND F02 WILL BE USED TO ISSUE JOB TO CREATE  *
     H*                  A DATA FILE OF ALL THE RECORDS THAT WERE     *
     H*                  FLAGGED TO SUBMIT.  THE EXTRACT FUNCTION     *
     H*                  WILL BE CHANGED TO BRING UP A GROUP SELECT   *
     H*                  SCREEN TO REPLACE THE GROUP RANGE ON PROMPT. *
     H*                                                               *
     H*  08/19/05  C:15  Project: WC591                               *
     H*                  Added the following function keys:           *
     H*                    F7=Submit corrections                      *
     H*                    F8=Send resubmission                       *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*
     FW7350F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMSSP     IF   E             DISK
     F                                     RENAME(PSS01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMSSL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PSS01:DBRCDF)
     F*** BEGIN ADD ***  04/23/09  ***********************************
     F*  SECONDARY logical file definition (for displaying items)
     FWMSSL2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PSS01:DBRCDFB)
     F*** END ADD *****  04/23/09  ***********************************
     F*
     F*  Admin/darts control definition file
     FWMEXL     IF   E           K DISK
     F*** BEGIN ADD ***  04/07/09  ***********************************
     FWDF2L     IF   E           K DISK
     FWXWSL     UF A E           K DISK    USROPN
     FWXWSL1    UF A E           K DISK
     F                                     RENAME(PWS01:PWS02)
      /COPY SCOPYWW01D
     D*** END ADD *****  04/07/09  ***********************************
      *** Beg Add ***** 04/20/21 *************************************
     d StatSubmission  pr                  extpgm('WTRPTSTAT')
     d outMode                        1
      *** End Add ***** 04/20/21 *************************************
     D*
     D*** Begin chg *** 04/23/09 **************************************
     D*********           HDA     1   1 78               Column headings
     D*********           HD      1   1 78               Column headings
     D HDA             S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D*** End chg ***** 04/23/09 **************************************
     D*** Begin chg *** 08/19/05 **************************************
     D***********         OF      1  12  1   OFD    35   Options, Function
     D*** Begin chg *** 04/23/09 **************************************
     D*******             OF      1  15  1   OFD    35   Options, Function
     D OF              S              1    DIM(16) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(16) ALT(OF)
     D*** End chg ***** 04/23/09 **************************************
     D*** End chg ***** 08/19/05 **************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Statistical Submissi-
     D                                     on')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W7350R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(1)
     D*** BEGIN CHG ***  04/23/09  ***********************************
     D*********     1                     C         VIEWMX
     D VIEWMX          C                   CONST(2)
     D*** END CHG *****  04/23/09  ***********************************
     D SF1PAG          C                   CONST(12)
     D DELETD          C                   CONST('*Deleted')
     D*
     D*  Data structures for SCDATA
     D                 DS
     D  XXDATA                 1     78
     D*** BEGIN CHG ***  04/23/09  ***********************************
     D*********                               2   4 XXFND
     D*********                              10  16 XXRPMY
     D  XXDAT                  2     16
     D*** END CHG *****  04/23/09  ***********************************
     D  XXSBDT                21     31
     D  XXRPTL                38     39
     D*** BEGIN ADD ***  04/07/09  ***********************************
     D  XXCAR#                49     53
     D  XXSTAT                60     60
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     I*** END ADD *****  04/07/09  ***********************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Transfer to tape program parameter list
     C*
     C     TTTPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C*** BEGIN CHG ***  04/07/09  *********************************
     C*********            PARM SCDBRR    ##RRN   90
     C                   PARM                    ##RRN             9 0
     C*** END CHG *****  04/07/09  *********************************
     C                   PARM                    ##QUE            10
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY
     C*** BEGIN ADD ***  04/23/09  ***********************************
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKRPMY
     C                   KFLD                    KKFND
     C*** END ADD *****  04/23/09  ***********************************
     C*
     C*  WCSTAT Extract data file
     C     SSKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  Darts/admin control defn key
     C     EXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*** BEGIN ADD ***  04/07/09  ***********************************
     C*
     C*  GROUP AUXILIARY FILE
     C     F2KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    X1FND
     C*
     C*  EXTRACTION WORK FILE
     C     WSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    X1FND
     C                   KFLD                    X1RPMY
     C*
     C     WSKY1         KLIST
     C                   KFLD                    KKCO#
     C*
     C     WSKY2         KLIST
     C                   KFLD                    WSCO#
     C                   KFLD                    XXUSER
     C                   KFLD                    XXTIME
     C                   KFLD                    WSFND
     C                   KFLD                    WSRPMY
     C*
     C     WSKY3         KLIST
     C                   KFLD                    WSCO#
     C                   KFLD                    XXUSER
     C                   KFLD                    XXTIME
     C*
     C*** END ADD *****  04/07/09  ***********************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    SSCO#         KKCO#
     C     *LIKE         DEFINE    SSFND         KKFND
     C     *LIKE         DEFINE    SSRPMY        KKRPMY
     C*** BEGIN ADD ***  04/07/09  ***********************************
     C     *LIKE         DEFINE    WSUSER        XXUSER
     C     *LIKE         DEFINE    WSTIME        XXTIME
     C*** END ADD *****  04/07/09  ***********************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR                                                  CHKSSM
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  Check if extraction is in process
     C     SCDBRR        CHAIN     PFRCDF                             60
     C     SSXTRC        IFEQ      'Y'
     C                   SETON                                        71  90
     C                   ENDIF                                                  SSXTRC IFNE 'Y'
     C*
     C                   Z-ADD     SCFND         KKFND
     C*
     C  N90              SELECT
     C*
     C     SCSEL         WHENEQ    '4'
     C* CHAIN TO WMEXP ( ADMIN DARTS FILE ) TO CHK SECURITY LEVEL
     C     EXKY          CHAIN     PEX01                              60
     C*
     C***  BEGIN DEL  ***  05/26/99  **********************************
     C********** ##LVL     IFLT EXDLTL                     NOT AUTHORIZED
     C********** *IN60     OREQ *ON                        NOT FOUND
     C**********           SETON                     77  90
     C**********           ENDIF                           ##LVL LT EXDLTL
     C***  END   DEL  ***  05/26/99  **********************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  ERRCK1
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVE      ##CO#         KKCO#
     C                   MOVE      *LOVAL        KKFND
     C*** BEGIN CHG ***  04/23/09  ************************************
     C*********            MOVE *LOVAL    KKRPMY
     C   21              MOVE      *LOVAL        KKRPMY
     C   22              MOVE      *HIVAL        KKRPMY
     C*** END CHG *****  04/23/09  ************************************
     C*
     C                   ENDSR                                                  KEYINZ
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C*** BEGIN CHG ***  04/23/09  ************************************
     C*********            MOVE SCPST1    KKFND
     C*********            Z-ADDSCPS1A    SRDTI
     C*********            CALL 'SRCVTMY' DTCYMD
     C*********            MOVELSRDTO     KKRPMY
     C   21              MOVE      SCPST1        KKFND
     C   21              Z-ADD     SCPS1A        SRDTI
     C   21              CALL      'SRCVTMY'     DTCYMD
     C   21              MOVEL     SRDTO         KKRPMY
     C*
     C   22              MOVE      SCPST2        SRDTI
     C   22              CALL      'SRCVTMY'     DTCYMD
     C   22              MOVEL     SRDTO         KKRPMY
     C   22              Z-ADD     SCPS2A        KKFND
     C*
     C     KKRPMY        IFEQ      0
     C                   MOVE      *HIVAL        KKRPMY
     C                   ENDIF
     C*** END CHG *****  04/23/09  ************************************
     C*
     C                   ENDSR                                                  KEYSET
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVE      SCFND         KKFND
     C                   MOVE      SCRPMY        KKRPMY
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*** Begin add *** 08/19/05 **************************************
     C*================================================================
     C*  KGDO - Process F7; Submit Correction process
     C*================================================================
     C     KGDO          BEGSR
     C*
     C                   MOVE      'C'           ##MODE
     C                   CALL      'W7352R2'     ACDPL                  99
     C*
     C                   ENDSR
     C*================================================================
     C*  KHDO - Process F8; Send Resubmission
     C*================================================================
     C     KHDO          BEGSR
     C*
     C                   MOVE      'C'           ##MODE
     C                   CALL      'W7352R3'     ACDPL                  99
     C*
     C                   ENDSR
     C*** End add ***** 08/19/05 **************************************
     C*** BEGIN ADD ***  04/07/09  ***********************************
     C*================================================================
     C*  KBDO - Process F2; SEND SUBMISSION FILE
     C*================================================================
     C     KBDO          BEGSR
     C*
     C* READ SUBMIT SELECTION FILE TO CHECK FOR ERRORS.
     C*
     C     WSKY1         CHAIN     PWS01                              60
     C*
     C* DO NOT PROCESS SUBMIT IF NO RECORDS HAVE BEEN SELECTED.
     C*
     C     *IN60         IFEQ      *ON
     C     *INKL         DOUEQ     *ON
     C                   EXFMT     WDWERR
     C                   ENDDO
     C                   MOVE      *OFF          *INKL
     C                   MOVE      *ON           RSTPGM
     C                   ELSE
     C*
     C* VERIFY ALL SELECTED RECORDS HAVE SAME CARRIER #.
     C*
     C                   MOVE      'Y'           XXOVR1            1
     C                   MOVE      WSFND         X1FND
     C     F2KY          CHAIN     PF201                              50
     C                   Z-ADD     F2CAR#        SVCAR#            5 0
      *** begin add *** 12/03/15 **************************************
     C                   If        cltid = '045' and WSRpmy < 11707
     C                   Z-add     33057         svCar#
     C                   Endif
      *** end add ***** 12/03/15 **************************************
     C*
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      WSFND         X1FND
     C     F2KY          CHAIN     PF201                              50
      *** begin add *** 12/03/15 **************************************
     C                   If        cltid = '045' and WSRpmy < 11707
     C                   Z-add     33057         F2Car#
     C                   Endif
      *** end add ***** 12/03/15 **************************************
     C*
     C     F2CAR#        IFNE      SVCAR#
     C*
     C                   MOVE      *BLANK        XXOVR1
     C                   EXFMT     WDWMSG
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      *OFF          *INKL
     C                   MOVE      *ON           RSTPGM
     C                   ELSE
     C                   MOVE      'Y'           XXOVR1
     C                   ENDIF                                                  *INKL IFEQ
     C*
     C                   LEAVE
     C                   ENDIF                                                  F2CAR@ IFNE
     C*
     C     WSKY1         READE     PWS01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  SUBMIT DATA FILE IF NO ERRORS OR IF OVERRIDE IS CHOSEN.
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        XXTIME
     C                   MOVEL(P)  Q1USER        XXUSER
     C*
     C     XXOVR1        IFEQ      'Y'
     C*
     C     WSKY1         CHAIN(N)  PWS01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     WSKY2         CHAIN     PWS02                              50
     C     *IN50         IFEQ      *ON
     C                   Z-ADD     XXTIME        WSTIME
     C                   MOVEL(P)  XXUSER        WSUSER
     C                   WRITE     PWS02
     C                   ENDIF
     C*
     C     WSKY1         READE(N)  PWS01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      'D'           ##MODE
     C                   MOVE      ##OUTQ        ##QUE
     C                   Z-ADD     XXTIME        ##RRN
     C*
     C                   CALL      'W7352R'      TTTPL
     C*
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C*
     C     WSKY3         CHAIN     PWS02                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   DELETE    PWS02
     C*
     C     WSKY3         READE     PWS02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  *INKC IFEQ
     C*
     C* CLEAR WORKFILE OF RECORDS.
     C*
     C     WSKY1         CHAIN     PWS01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   DELETE    PWS01
     C*
     C     WSKY1         READE     PWS01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   EXSR      KEDO
     C*
     C                   ENDIF                                                  XXOVR1 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDSR                                                  KBDO SUBR
     C*** END ADD *****  04/07/09  ***********************************
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*  Move record values into hidden subfile screen fields
     C                   MOVE      SSFND         SCFND
     C                   MOVE      SSRPMY        SCRPMY
     C*
     C*  Edit stuff for output
     C                   MOVE      *BLANKS       EDTMSK
     C                   MOVEL(P)  SSFND         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C*** BEGIN CHG ***  04/23/09  ***********************************
     C*********            MOVELXXNUM     XXFND
     C                   MOVEL     XXNUM         XXFND             3
     C*** END CHG *****  04/23/09  ***********************************
     C*
     C                   MOVEL(P)  SSRPTL        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXRPTL
     C*
     C                   MOVEL     'DATE'        EDTMSK
     C                   Z-ADD     SSSBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXSBDT
     C*
     C                   MOVEL     'RPMY'        EDTMSK
     C                   Z-ADD     SSRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C*** BEGIN CHG ***  04/23/09  ***********************************
     C*********            MOVELXXNUM     XXRPMY
     C                   MOVEL     XXNUM         XXRPMY            7
     C*** END CHG *****  04/23/09  ***********************************
     C*
     C*** BEGIN ADD ***  04/07/09  ***********************************
     C                   MOVE      SSFND         X1FND             3 0
     C                   MOVE      SSRPMY        X1RPMY            5 0
     C     F2KY          CHAIN     PF201                              60
     C  N60              MOVE      F2CAR#        XXCAR#
      *** begin add *** 12/03/15 **************************************
     C                   If        cltid = '045' and SSRpmy < 11707
     C                   Move      '33057'       xxCar#
     C                   Endif
      *** end add ***** 12/03/15 **************************************
     C*
     C                   MOVE      *BLANK        XXSTAT
     C     WSKY          CHAIN(N)  PWS01                              60
     C  N60              MOVE      'Y'           XXSTAT
     C*** END ADD *****  04/07/09  ***********************************
     C*
     C*** BEGIN ADD ***  04/23/09  ***********************************
     C                   CLEAR                   XXDAT
     C   21              MOVEL     XXFND         XXDAT
     C   21              MOVE      XXRPMY        XXDAT
     C   22              MOVEL     XXRPMY        XXDAT
     C   22              MOVE      XXFND         XXDAT
     C*** END ADD *****  04/23/09  ***********************************
     C                   MOVEL(P)  XXDATA        SCDATA
     C*** BEGIN ADD ***  04/23/09  ***********************************
     C*********            MOVELHDA,1     SCHDLA
     C   21              MOVEL     HDA(1)        SCHDLA
     C   22              MOVEL     HDA(2)        SCHDLA
     C*** END ADD *****  04/23/09  ***********************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* OPT8 - Process option 8 (work with units)
     C*================================================================
     C     OPT8          BEGSR
     C*
     C                   MOVE      @@CALL        ##MODE
     C*
     C                   CALL      'W7354R'      ACDPL
     C*
     C                   ENDSR                                                  OPT8
     C*================================================================
     C* OPT25 - Process option 25 (submit)
     C*================================================================
     C     OPT25         BEGSR
     C*
     C*
     C*** BEGIN CHG ***  04/07/09  ************************************
     C*********            MOVE 'D'       ##MODE
     C*********            MOVE ##OUTQ    ##QUE
     C*
     C*********            CALL 'W7352R'  TTTPL
     C*
     C                   MOVE      SCFND         X1FND
     C                   Z-ADD     SCRPMY        X1RPMY
     C*
     C     WSKY          CHAIN     PWS01                              60
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PWS01
     C                   ELSE
     C                   Z-ADD     KKCO#         WSCO#
     C                   Z-ADD     X1FND         WSFND
     C                   Z-ADD     X1RPMY        WSRPMY
     C                   MOVEL     Q1PGM         WSPGM
     C                   MOVEL     Q1USER        WSUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        WSTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WSDT
     C                   WRITE     PWS01
     C                   ENDIF                                                  *IN60
     C*** END CHG *****  04/07/09  ************************************
     C*
     C                   ENDSR                                                  OPT25
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*** Begin add *** 08/19/05 **************************************
     C*** BEGIN ADD ***  04/07/09  ***********************************
     C     *INKB         CASEQ     *ON           KBDO
     C*** END ADD *****  04/07/09  ***********************************
     C     *INKG         CASEQ     *ON           KGDO
     C     *INKH         CASEQ     *ON           KHDO
     C                   ENDCS
     C*** End add ***** 08/19/05 **************************************
     C*
     C                   ENDSR                                                  OTHFKS
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '25'          OPT25
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*** BEGIN ADD ***  04/07/09  ***********************************
     C                   CLOSE     WXWSL
     C*
     C                   MOVE      'X'           QQFLAG
     C                   EXSR      GRPF
     C                   MOVE      'D'           QQFLAG
     C                   EXSR      GRPF
     C*** END ADD *****  04/07/09  ***********************************
     C                   ENDSR                                                  PGMEND
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR

      //*** Beg Add ***** 04/20/21 ***********************************
      /Free
        StatSubmission(@@Call);
        *Inlr = *on;
        Return;
      /End-Free
      //*** End Add ***** 04/20/21 ***********************************

      *** begin add *** 12/03/15 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *** end add ***** 12/03/15 **************************************
     C*** BEGIN ADD ***  04/07/09  ***********************************
     C                   MOVE      'W'           QQFLAG
     C                   EXSR      GRPF
     C                   MOVE      'C'           QQFLAG
     C                   EXSR      GRPF
     C*
     C                   OPEN      WXWSL
     C*** END ADD *****  04/07/09  ***********************************
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21SSKY          READE     DBRCDF                                 95
     C*** BEGIN ADD ***  04/23/09  ***********************************
     C   22SSKY          READE     DBRCDFB                                95
     C*** END ADD *****  04/23/09  ***********************************
     C                   ENDSR                                                  READ
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21SSKY          READPE    DBRCDF                                 95
     C*** BEGIN ADD ***  04/23/09  ***********************************
     C   22SSKY          READPE    DBRCDFB                                95
     C*** END ADD *****  04/23/09  ***********************************
     C                   ENDSR                                                  READP
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C*** BEGIN ADD ***  04/23/09  ***********************************
     C   22KEY2          SETGT     DBRCDFB                            96
     C*** END ADD *****  04/23/09  ***********************************
     C                   ENDSR                                                  SETGT
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C*** BEGIN ADD ***  04/23/09  ***********************************
     C   22KEY2          SETLL     DBRCDFB                            96
     C*** END ADD *****  04/23/09  ***********************************
     C                   ENDSR                                                  SETLL
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  DELETD        SCDATA
     C*
     C                   ENDSR                                                  SF1UPD
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR                                                  SLCTRD
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR                                                  SUBSET
      *** BEGIN ADD ***  04/07/09  ************************************
     C*================================================================
      * CALL CL TO CREATE TEMPORARY VERSIONS OF FILES.
     C*================================================================
     C     GRPF          BEGSR
      *
     C                   CALL      'W7417C4'
     C                   PARM                    QQFLAG            1
      *
     C                   ENDSR                                                  GRPF ENDSR
     C*================================================================
      *** END ADD *****  04/07/09  ************************************
** Column headings 1st line
            Valuation     Submitted    Reporting    Carrier   Submit
    Valuation             Submitted    Reporting    Carrier   Submit
** Column headings 2nd line
Opt  Group      Month       Date        Levels        Num
Opt     Month    Group      Date        Levels        Num
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                NNN
1088=Work with Units        YNN
12525=Submit Data           YNN
202F2=Submit data file      YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Extract data          YNN
207F7=Submit corrections    YNN
208F8=Send resubmission     YNN
211F11=Change View          YYY
212F12=Cancel               NNY
217F17=Subset               NNN
3
