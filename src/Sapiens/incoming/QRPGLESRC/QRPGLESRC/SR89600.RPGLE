     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/define CONVERT
     H*
     H/TITLE SR89600 - LOAD ENDORSEMENT FORM WORKFILE FOR FORM WC890600
     H*****************************************************************
     H*    PROGRAM DESCRIPTION:                                       *
     H*                                                               *
     H*       THIS PROGRAM IS CALLED TO:                              *
     H*         1) CREATE DATA INTO THE ENDORSEMENT FORM WORKFILE     *
     H*            (WXFMP) PRIOR TO A FORM MERGE.                     *
     H*                                                               *
     H*                                                               *
     H* PROGRAMMER: C:57                                              *
     H* CREATED:    02/17/97            PROJECT: CM027                *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
      *                                                               *
      * 11/10/16 C:136 Project: SY371                                 *
      *               -Add field for extended description             *
      *               -Renamed fields in WTENP because ENdsc2 is      *
      *                used in WMENP and WTENP                        *
      *
      * 10/17/06 C:61  SUPPORT WA081 54973
      *                Include Terrorism and Domestic Terrorism
      *                charges in premium amount.
     H*                                                               *
     H* 08/17/05 C:61  PROJECT WC1266                                 *
     H*                Check for "Rescission Division Deletion" flag. *
     H*                                                               *
     H* 06/15/04 C:61  PROJECT WC1224  WA091 40129                    *
     H*                Only print WC890605 for mailing address change *
     H*                of state reporting division.  If not state     *
     H*                reporting division, print WC890608.            *
     H*                                                               *
     H* 01/22/04 C:09  PROJECT: CM027                                 *
     H*                Correct issue whereby multiple WC890600A forms *
     H*                with same exact data were printing on entity/  *
     H*                location changes.                              *
     H*                                                               *
     H* 01/20/04 C:09  PROJECT: CM027                                 *
     H*                We will no longer generate a separate NCCI     *
     H*                midterm endorsement form (WC890600A) if        *
     H*                payroll changes are made.  One form will       *
     H*                be created with Item 4.*Class, Rate, Other     *
     H*                (WC890615) marked or Interim Adjustment of     *
     H*                Premium (WC890616) marked or both, as appr.    *
     H*                (see W0060R, W0090R  for other changes).       *
     H*                                                               *
     H* 06/04/01 C:90  PROJECT: CM027                                 *
     H*                Load 3 line mid-term endorsement description   *
     H*                from employer billing maintenance.             *
     H*                                                               *
     H* 03/27/01 C:90  PROJECT: CM027                                 *
     H*                ADD CODE TO RECOGNIZE MIDTERM PRODUCER CHANGE. *
     H*                                                               *
     H* 06/29/99 C:113 PROJECT: CM027                                 *
     H*                Add/Delete Division & Add/Delete/Change        *
     H*                Entity/Location now uses constant line under   *
     H*                'is changed to read:'.                         *
     H*                                                               *
     H* 05/27/99 C:113 PROJECT CM027                                  *
     H*                For Deletion of Division and Deletion of       *
     H*                Ent/Loc print constant line under "is changed  *
     H*                to read:"                                      *
     H*                                                               *
     H* 05/20/99 C:113 PROJECT CM027                                  *
     H*                Changed to load mailing address if physical    *
     H*                address is not found on Addition or Deletion   *
     H*                of Division.                                   *
     H*                                                               *
     H* 05/17/99 C:09 PROJECT CM044                                   *
     H*               ADD CODE TO RECOGNIZE MIDTERM ADDITION/DELETION *
     H*               OF DIVISION.                                    *
     H*                                                               *
     H* 05/06/99 C:09 PROJECT CM044                                   *
     H*               ADD CODE TO RECOGNIZE MIDTERM ENTITY/LOCATION   *
     H*               ADDITION, DELETION AND CHANGES.                 *
     H*                                                               *
     H* 10/09/98 C:09 PROJECT CM027                                   *
     H*               ONLY ACCUMULATE PREMIUM FOR A GIVEN FUND YEAR.  *
     H*                                                               *
     H* 08/18/98 C:78 PROJECT CM027                                   *
     H*               LOADING PREMIUM FOR ALL DIVISIONS.              *
     H*                                                               *
     H* 07/07/98 C:57 PROJECT SUPPORT 12250                           *
     H*               REMOVE SECTION FOR BOTH ADDRESS CHG PROCESSING  *
     H*               AS IT WAS CAUSEING TWO SETS OF OUTPUT.          *
     H*                                                               *
     H*****************************************************************
     F*
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWTENL     IF   E           K DISK
     F                                     RENAME(PEN01:PTEN01)
      *** beg add ***** 11/10/16 **************************************
     f                                     prefix(t_)
      *** end add ***** 11/10/16 **************************************
     FWMENL3    IF   E           K DISK
     F                                     RENAME(PEN01:PMEN01)
     FWTGTL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMRPL     IF   E           K DISK
     FWMMUL4    IF A E           K DISK
     FWXICP     O    E             DISK
     FWXFMP     O    E             DISK
     F******* BEGIN ADD ***** 10/09/98 ********************************
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     F******* END ADD ******* 10/09/98 ********************************

     D*
     D XXFORM          C                   CONST('WC890600A ')
     D*** BEGIN ADD *** 05/27/99 **************************************
     D XXEXT1          C                   CONST('See Extension of Inf-
     D                                     ormation Page for')
     D*
     D XXEXT2          C                   CONST('Other Workplaces Inf-
     D                                     ormation')
     D*** END   ADD *** 05/27/99 **************************************
     D*
     D                 DS
     D  XXZIP                  1      9
     D  XXZIP1                 1      5
     D  XXZIP2                 6      9
     D*
     D                 DS
     D  DSDAT7                 1      7  0
     D  DSCENT                 1      1
     D  DSYY                   2      3
     D  DSMM                   4      5
     D  DSDD                   6      7
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D*
     D                 DS
     D  XXFLGS                 1     25
     D*  WC890601
     D  FMX01                  1      1
     D*  WC890602
     D  FMX02                  2      2
     D*  WC890603
     D  FMX03                  3      3
     D*  WC890604
     D  FMX04                  4      4
     D*  WC890605
     D  FMX05                  5      5
     D*  WC890406
     D  FMX06                  6      6
     D*  WC890607
     D  FMX07                  7      7
     D*  WC890608
     D  FMX08                  8      8
     D*  WC890610
     D  FMX09                  9      9
     D*  WC890611
     D  FMX10                 10     10
     D*  WC890612
     D  FMX11                 11     11
     D*  WC890613
     D  FMX12                 12     12
     D*  WC890614
     D  FMX13                 13     13
     D*  WC890615
     D  FMX14                 14     14
     D*  WC890616
     D  FMX15                 15     15
     D*  WC890617
     D  FMX16                 16     16
     D*  WC890618
     D  FMX17                 17     17
     D*  WC890619
     D  FMX18                 18     18
     D  FMFECG                19     19
     D  FMINCG                20     20
     D  FMLSCG                21     21
     D  FMMACG                22     22
     D  FMPACG                23     23
     D  FMPBCG                24     24
     D  FMPRCG                25     25
     D*
     D                 DS
     D  XXDSCS                 1    200
     D  FMC401                 1     40
     D  FMC402                41     80
     D  FMC403                81    120
     D  FMC404               121    160
     D  FMC405               161    200
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C****************************************************************
     C*
     C*  FUND MASTER KLIST
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  EMPLOYER MASTER KLIST
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EMPLOYER DETAIL KLIST
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
      ****** BEGIN ADD ****** 08/18/98 ********************************
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C******* BEGIN ADD ***** 10/09/98 ********************************
     C                   KFLD                    XXFYR
     C******* END ADD ******* 10/09/98 ********************************
     C                   KFLD                    XXEMP#
      ****** END   ADD ****** 08/18/98 ********************************
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*  ENDORSEMENT FORM TABLE FILE
     C*
     C     TENKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XKFORM
     C*
     C*  FUND AUXILIARY KLIST
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  CARRIER FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  EMPLOYER ATTACHED ENDORSEMENTS FILE  (CO#/FND/EMP#/DIV/FYR/END#/SEQ#)
     C*
     C     MUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEND#
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*  ENDORSEMENT REQUEST WORKFILE
     C*
     C     MENKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXJOB
     C                   KFLD                    LDEND#
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
      *** beg chg ***** 11/10/16 ***************************
     C******LIKE         DEFINE    ENFORM        XKFORM
     C     *LIKE         DEFINE    t_ENform      XKFORM
      *** end chg ***** 11/10/16 ***************************
     C     *LIKE         DEFINE    FMEMST        XXEMST
     C     *LIKE         DEFINE    FMEAD1        XXEAD1
     C     *LIKE         DEFINE    FMEAD2        XXEAD2
     C     *LIKE         DEFINE    FMEAD3        XXEAD3
     C     *LIKE         DEFINE    FNPST         XXFNST
     C     *LIKE         DEFINE    FMCARR        XXCARR
     C     *LIKE         DEFINE    FMCPL#        XXCPL#
     C     *LIKE         DEFINE    MUEND#        XXEND#
     C     *LIKE         DEFINE    MUSEQ#        XXSEQ#
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    ENJOB         XXJOB
      ********* BEGIN ADD ******* 08/18/98 ****************************
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C     *LIKE         DEFINE    ELENMP        XXENMP
      ********* END   ADD ******* 08/18/98 ****************************
     C*
     C*  DATE CHECK/CONVERSION PARM LISTS
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
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
     C*  ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQMODE            1
     C                   PARM                    QQJOB             6
     C                   PARM                    QWRT89            1
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQJOB         XXJOB
     C*
     C                   MOVEL     XXFORM        XKFORM
     C                   MOVE      *OFF          XWRT89            1
     C**** BEGIN ADD **** 01/22/04 ************************************
     C                   Z-ADD     *ZEROS        XXCNT             3 0
     C**** END ADD ****** 01/22/04 ************************************
     C*
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*
     C********************************************************************
     C*  MAINLINE
     C********************************************************************
     C*
     C                   SELECT
     C*
     C*  GATHER INFO TO MERGE TO THE FORM
     C*
     C     QQMODE        WHENEQ    '2'
     C                   EXSR      BULDIT
     C     XWRT89        IFEQ      *ON
     C                   EXSR      INCREC
     C                   ENDIF
     C                   ENDSL
     C*
     C                   MOVE      XWRT89        QWRT89
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C********************************************************************
     C*  GATHER INFO TO MERGE TO THE FORM
     C********************************************************************
     C     BULDIT        BEGSR
     C*
     C*  GET THE BASIC INFO
     C*
     C                   EXSR      GETFND
     C                   EXSR      GETEMP
     C                   EXSR      GETAGT
     C                   EXSR      GETFRM
     C                   EXSR      GETCAR
     C*
     C*  CLEAR OUT THE RECORD
     C*
     C                   CLEAR                   PFM01
     C*
     C*  LOAD BASIC WXFMP FIELDS
     C*
     C                   EXSR      BASINF
     C*
     C*  LOAD FORM SPECIFIC INFO
     C*
     C                   EXSR      OTHINF
     C*
     C*  WRITE A RECORD TO THE EMPLOYER ATTACHED ENDORSEMENT FORMS FILE
     C*
     C     XWRT89        IFEQ      *ON
     C                   EXSR      WRTPMU
     C                   ENDIF
     C*
     C                   ENDSR
     C********************************************************************
     C*  GETFND - GET FUND INFORMATION
     C********************************************************************
     C     GETFND        BEGSR
     C*
     C*  GET THE FUND MASTER RECORD
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     FNPST         IFNE      *BLANK
     C                   MOVEL     FNPST         XXFNST
     C                   ELSE
     C                   MOVEL     FNMST         XXFNST
     C                   ENDIF
     C*
     C*  GET THE FUND AUXILIARY RECORD
     C*
     C     F2KY          CHAIN     PF201                              60
     C*
     C                   MOVE      F2CAR#        XXCARR
     C*
     C                   ENDSR
     C********************************************************************
     C*  GETEMP - GET EMPLOYER INFORMATION
     C********************************************************************
     C     GETEMP        BEGSR
     C*
     C                   MOVE      *ZERO         XXPFDT
     C*
     C*  GET THE EMPLOYER MASTER RECORD
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C*  BUILD UP ADDRESS LINES USING MAILING ADDRESS DATA
     C*
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL(P)  EMMAD1        XXEAD1
     C                   MOVEL     EMMST         XXEMST
     C                   MOVE      EMMZIP        XXZIP
     C     EMMCTY        CAT(P)    ',':0         XXCSZ            40
     C                   CAT       EMMST:1       XXCSZ
     C                   CAT       XXZIP1:1      XXCSZ
     C*
     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         XXCSZ
     C                   CAT       XXZIP2:0      XXCSZ
     C                   ENDIF                                                  XXZIP2 <> ' '
     C*
     C     EMMAD2        IFNE      *BLANK
     C                   MOVEL(P)  EMMAD2        XXEAD2
     C                   MOVEL(P)  XXCSZ         XXEAD3
     C                   ELSE
     C                   MOVEL(P)  XXCSZ         XXEAD2
     C                   MOVEL(P)  *BLANKS       XXEAD3
     C                   ENDIF                                                  EMMAD2 <> ' '
     C*
     C                   ELSE
     C*
     C*  BUILD UP ADDRESS LINES USING PHYSICAL ADDRESS DATA
     C*
     C                   MOVEL(P)  EMPAD1        XXEAD1
     C                   MOVEL     EMPST         XXEMST
     C                   MOVE      EMPZIP        XXZIP
     C     EMPCTY        CAT(P)    ',':0         XXCSZ
     C                   CAT       EMPST:1       XXCSZ
     C                   CAT       XXZIP1:1      XXCSZ
     C*
     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         XXCSZ
     C                   CAT       XXZIP2:0      XXCSZ
     C                   ENDIF                                                  XXZIP2 <> ' '
     C*
     C     EMPAD2        IFNE      *BLANK
     C                   MOVEL(P)  EMPAD2        XXEAD2
     C                   MOVEL(P)  XXCSZ         XXEAD3
     C                   ELSE
     C                   MOVEL(P)  XXCSZ         XXEAD2
     C                   MOVEL(P)  *BLANKS       XXEAD3
     C                   ENDIF                                                  EMPAD2 <> ' '
     C*
     C                   ENDIF                                                  EMMPFL = 'M'
     C*
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C*  GET THE EMPLOYER DETAIL RECORD
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C                   ENDIF
     C*
     C                   ENDSR
     C********************************************************************
     C*  GETAGT - GET AGENT INFORMATION
     C********************************************************************
     C     GETAGT        BEGSR
     C*
     C                   Z-ADD     *ZERO         XXAGT#
     C*
     C*  GET THE CURRENT AGENT HISTORY RECORD
     C*
     C     AHKY          SETGT     PAH01
     C     AHKY          READPE    PAH01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     AHKY          READPE    PAH01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     AHAGT#        XXAGT#
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C  N60AAKY          CHAIN     PAA01                              60
     C  N60AGKY          CHAIN     PAG01                              60
     C*
     C                   ENDSR
     C********************************************************************
     C*  GETFRM - GET ENDORSMENT FORM TABLE RECORD
     C********************************************************************
     C     GETFRM        BEGSR
     C*
     C*  GET THE ENDORSMENT FORM TABLE RECORD
     C*
     C     TENKY         CHAIN     PTEN01                             60
     C*
     C     *IN60         DOWEQ     *OFF
      *** beg chg ***** 11/10/16 ********************
     C*****ENSTAT        ANDEQ     'I'
     C     t_ENstat      ANDEQ     'I'
      *** end chg ***** 11/10/16 ********************
     C     TENKY         READE     PTEN01                                 60
     C                   ENDDO                                                  *IN60 = *OFF
     C*
     C                   ENDSR
     C********************************************************************
     C*  GETCAR - GET CARRIER INFORMATION
     C********************************************************************
     C     GETCAR        BEGSR
     C*
     C*  GET THE CARRIER INFO RECORD
     C*
     C     RPKY          CHAIN     PRP01                              60
     C*
     C                   MOVEL     RPCAP#        XXCPL#
     C*
     C                   ENDSR
     C********************************************************************
     C*  BASINF - LOAD BASIC WXFMP FIELDS
     C********************************************************************
     C     BASINF        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      XXCO#         FMCO#
     C                   MOVE      XXFND         FMFND
     C                   MOVE      XXFYR         FMFYR
     C                   MOVE      XXEMP#        FMEMP#
     C                   MOVE      XXDIV         FMDIV
     C                   MOVEL     XXFORM        FMFORM
     C*
      *** beg chg ***** 11/10/16 **************************
     C***********        MOVE      ENEDDT        DSDAT7
     C                   MOVE      t_ENEDDT      DSDAT7
      *** end chg ***** 11/10/16 **************************
     C                   MOVEL     DSMM          FMEDDT
     C                   MOVE      DSYY          FMEDDT
     C*
     C                   MOVE      LDEEDT        FMEEDT
     C*
     C                   Z-ADD     LDEND#        FMEND#
     C*
     C     QQFND         CAT(P)    QQEMP#:0      FMPOL#
     C                   CAT       QQFYR:0       FMPOL#
     C*
     C                   MOVEL     EMTPOB        FMTOBC
     C                   MOVEL     EMNAME        FMEMNM
     C                   MOVEL     EMFDNM        FMFENM
     C                   MOVEL     XXFNST        FMFNST
     C                   MOVEL     FNNAME        FMFNNM
      ******* BEGIN CHG ******** 10/09/98 *****************************
      ******* BEGIN ADD ******** 08/18/98 *****************************
     C                   Z-ADD     XXDIV         SVDIV
     C                   Z-ADD     0             XXENMP
     C     ELKY2         CHAIN     PEL02                              61
     C     *IN61         DOWEQ     '0'
     C                   ADD       ELENMP        XXENMP
      ****** beg add ****** 10/17/06 **********************************
     C                   ADD       ELETER        XXENMP
     C                   ADD       ELEDTR        XXENMP
      ****** end add ****** 10/17/06 **********************************
     C     ELKY2         READE     PEL02                                  61
     C                   ENDDO
     C                   Z-ADD     SVDIV         XXDIV
     C     ELKY          CHAIN     PEL01                              60
      ******* END   ADD ******** 08/18/98 *****************************
      ******* END   CHG ******** 10/09/98 *****************************
      ******* BEGIN CHG ******** 08/18/98 *****************************
     C******************   Z-ADDELENMP    FMPREM
     C                   Z-ADD     XXENMP        FMPREM
      ******* END   CHG ******** 08/18/98 *****************************
     C                   MOVE      XXAGT#        FMAGT#
     C                   MOVEL     AGNAME        FMAGNM
     C                   MOVEL     AGPST         FMAGST
     C                   MOVEL     XXCPL#        FMCPL#
     C                   MOVE      XXCARR        FMCARR
     C                   MOVEL     XXEAD1        FMEAD1
     C                   MOVEL     XXEAD2        FMEAD2
     C                   MOVEL     XXEAD3        FMEAD3
     C                   MOVEL     XXEMST        FMEMST
     C*
     C                   MOVE      ELANDT        DSDAT7
     C                   MOVE      DSDD          FMANDA
     C                   MOVE      DSMM          FMANMO
     C*
     C                   MOVEL     Q1USER        FMUSER
     C                   MOVEL     Q1PGM         FMPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FMDT
     C*
     C                   MOVEL     DSTIME        FMTIME
     C                   MOVEL     'A'           FMSTAT
     C                   MOVEL     Q1USER        FMIUSR
     C                   MOVEL     Q1PGM         FMIPGM
     C                   MOVE      FMDT          FMIDT
     C*
     C                   ENDSR
     C********************************************************************
     C*  OTHINF - LOAD FORM SPECIFIC INFORMATION
     C********************************************************************
     C     OTHINF        BEGSR
     C*
     C*  GET THE MID-TERM ENDORSEMENT WORKFILE RECORD
     C*
     C     MENKY         SETLL     PMEN01
     C     MENKY         READE     PMEN01                                 60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     ENSEQ#        IFNE      *ZERO
     C                   GOTO      NXTMEN
     C                   ENDIF
     C*
     C*  CLEAR ANY FIELDS MANIPULATED IN THIS SUB-ROUTINE TO AVOID CARRY
     C*  OVER DATA
     C*
     C                   MOVEL     *BLANKS       XXFLGS
     C                   MOVEL     *BLANKS       XXDSCS
     C                   MOVE      *ON           XXPRNT            1
     C*
     C*  SET THE APPROPRIATE MID-TERM ENDORSEMENT PRINT FLAG
     C*
     C                   SELECT
     C*-------------------------------------------------------------------------
     C     ENNCFL        WHENEQ    'Y'                                          NAME CHANGE
     C                   MOVE      'X'           FMX01                            WC890601
     C                   MOVEL     EMNAME        FMC401                           DESCRIPTION
     C                   MOVE      'Y'           FMINCG                           INS NAME CHG
     C*-------------------------------------------------------------------------
     C     ENFIFL        WHENEQ    'Y'                                          FED ID # CHANGE
     C                   MOVEL     ENFEN2        XXA2              2
     C                   MOVE      ENFEN2        XXA7              7
     C     'Federal '    CAT       'ID Num':1    FMC401                           DESCRIPTION
     C                   CAT       'ber Ch':0    FMC401
     C                   CAT       'ange':0      FMC401
     C                   CAT       XXA2:1        FMC401
     C                   CAT       '-':0         FMC401
     C                   CAT       XXA7:0        FMC401
     C                   MOVE      'Y'           FMFECG                           FED ID CHG
     C*-------------------------------------------------------------------------
     C     ENTBFL        WHENEQ    'Y'                                          TYP OF BUSINESS CHG
     C                   MOVE      'X'           FMX09                            WC890610
     C                   MOVEL     'BUSI'        XXTBID
     C                   MOVEL(P)  ENTOB2        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        FMC401
     C                   ENDIF
     C                   MOVE      'Y'           FMLSCG                           LEGAL STAT CHG
     C*-------------------------------------------------------------------------
     C*** BEG DEL ***** 07/07/98 ***********************************************
     C***********ENCBAF**  WHEQ 'Y'                        CHG BOTH ADDR FLG
     C*******************  MOVE 'X'       FMX05              WC890605
     C*******************  MOVELENMA12    FMC401             DESCRIPTION 1
     C*******************  MOVELENMA22    FMC402             DESCRIPTION 2
     C*******************
     C*******************  MOVELENMZP2    XXZIP5  5
     C*******************  MOVE ENMZP2    XXZIP4  4
     C***********ENMCT2**  CAT  ',':0     FMC403    P        DESCRIPTION 3
     C*******************  CAT  ENMST2:1  FMC403
     C*******************  CAT  XXZIP5:1  FMC403
     C***********XXZIP4**  IFNE *BLANKS
     C***********XXZIP4**  ANDNE*ZEROS
     C*******************  CAT  '-':0     FMC403
     C*******************  CAT  XXZIP4:0  FMC403
     C*******************  ENDIF
     C***FLOAT*CSZ*UP*IF*NECESSARY
     C***********FMC402**  IFEQ *BLANKS
     C*******************  MOVELFMC403    FMC402
     C*******************  MOVEL*BLANKS   FMC403
     C*******************  ENDIF
     C*******************  MOVE 'Y'       FMMACG             MAIL ADDR CHG
     C*******************
     C*******************  ADD  1         FMTIME
     C*******************  WRITEPFM01
     C*******************
     C*******************  MOVEL*BLANKS   XXFLGS
     C*******************  MOVEL*BLANKS   XXDSCS
     C*******************
     C*******************  MOVE 'X'       FMX08              WC890608
     C*******************  MOVELENPA12    FMC401             DESCRIPTION 1
     C*******************  MOVELENPA22    FMC402             DESCRIPTION 2
     C*******************
     C*******************  MOVELENPZP2    XXZIP5  5
     C*******************  MOVE ENPZP2    XXZIP4  4
     C***********ENPCT2**  CAT  ',':0     FMC403    P        DESCRIPTION 3
     C*******************  CAT  ENPST2:1  FMC403
     C*******************  CAT  XXZIP5:1  FMC403
     C***********XXZIP4**  IFNE *BLANKS
     C***********XXZIP4**  ANDNE*ZEROS
     C*******************  CAT  '-':0     FMC403
     C*******************  CAT  XXZIP4:0  FMC403
     C*******************  ENDIF
     C***FLOAT*CSZ*UP*IF*NECESSARY
     C***********FMC402**  IFEQ *BLANKS
     C*******************  MOVELFMC403    FMC402
     C*******************  MOVEL*BLANKS   FMC403
     C*******************  ENDIF
     C*******************  MOVE 'Y'       FMPACG             PHYS ADDR CHG
     C*** END DEL ***** 07/07/98 ***********************************************
     C*-------------------------------------------------------------------------
     C     ENMAFL        WHENEQ    'Y'                                          MAILING ADDR CHG
      *
      ***** BEG ADD ******* 06/15/04 **********************************
     C     EMSTRP        IFEQ      'Y'
      ***** END ADD ******* 06/15/04 **********************************
     C                   MOVE      'X'           FMX05                            WC890605
      ***** BEG ADD ******* 06/15/04 **********************************
     C                   ELSE
     C                   MOVE      'X'           FMX08                            WC890608
     C                   ENDIF                                                    EMSTRP
      ***** END ADD ******* 06/15/04 **********************************
      *
     C                   MOVEL     ENMA12        FMC401                           DESCRIPTION 1
     C                   MOVEL     ENMA22        FMC402                           DESCRIPTION 2
     C*
     C                   MOVEL     ENMZP2        XXZIP5            5
     C                   MOVE      ENMZP2        XXZIP4            4
     C     ENMCT2        CAT(P)    ',':0         FMC403                           DESCRIPTION 3
     C                   CAT       ENMST2:1      FMC403
     C                   CAT       XXZIP5:1      FMC403
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         FMC403
     C                   CAT       XXZIP4:0      FMC403
     C                   ENDIF
     C*  FLOAT CSZ UP IF NECESSARY
     C     FMC402        IFEQ      *BLANKS
     C                   MOVEL     FMC403        FMC402
     C                   MOVEL     *BLANKS       FMC403
     C                   ENDIF
     C                   MOVE      'Y'           FMMACG                           MAIL ADDR CHG
     C*-------------------------------------------------------------------------
     C     ENPAFL        WHENEQ    'Y'                                          PHYSICAL ADDR CHG
     C                   MOVE      'X'           FMX08                            WC890608
     C                   MOVEL     ENPA12        FMC401                           DESCRIPTION 1
     C                   MOVEL     ENPA22        FMC402                           DESCRIPTION 2
     C*
     C                   MOVEL     ENPZP2        XXZIP5            5
     C                   MOVE      ENPZP2        XXZIP4            4
     C     ENPCT2        CAT(P)    ',':0         FMC403                           DESCRIPTION 3
     C                   CAT       ENPST2:1      FMC403
     C                   CAT       XXZIP5:1      FMC403
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         FMC403
     C                   CAT       XXZIP4:0      FMC403
     C                   ENDIF
     C*  FLOAT CSZ UP IF NECESSARY
     C     FMC402        IFEQ      *BLANKS
     C                   MOVEL     FMC403        FMC402
     C                   MOVEL     *BLANKS       FMC403
     C                   ENDIF
     C                   MOVE      'Y'           FMPACG                           PHYS ADDR CHG
     C*-------------------------------------------------------------------------
     C     ENILFL        WHENEQ    'Y'                                          INCR LMT CHANGE
     C                   MOVE      'X'           FMX11                            WC890612
     C                   MOVEL     ENID12        FMC401                           DESCRIPTION 1
     C                   MOVEL     ENID22        FMC402                           DESCRIPTION 2
     C                   MOVE      'Y'           FMPBCG                           PREM BEARING CHG
     C*-------------------------------------------------------------------------
     C     ENERFL        WHENEQ    'Y'                                          EXP MOD CHG
     C                   MOVE      'X'           FMX06                            WC890406
     C     'See Revi'    CAT       'sed WC':0    FMC401                           DESCRIPTION 1
     C                   CAT       '00 00 ':1    FMC401
     C                   CAT       '01 A':1      FMC401
     C                   MOVE      'Y'           FMPBCG                           PREM BEARING CHG
     C*-------------------------------------------------------------------------
     C**** BEGIN DEL **** 01/20/04 ************************************
     C***********ENCRFL****WHEQ 'Y'                        PAY CLS/RATE CHG
     C*********************MOVE 'X'       FMX14              WC890615
     C***********'See Revi'CAT  'sed WC':0FMC405             DESCRIPTION 5
     C*********************CAT  '00 00 ':1FMC405
     C*********************CAT  '01 A':1  FMC405
     C*********************MOVE 'Y'       FMPBCG             PREM BEARING CHG
     C**** BEGIN ADD **** 06/04/01 ************************************
     C*********************MOVELENDSC1    FMC401
     C*********************MOVELENDSC2    FMC402
     C*********************MOVELENDSC3    FMC403
     C**** END   ADD **** 06/04/01 ************************************
     C**** END DEL ****** 01/20/04 ************************************
     C*-------------------------------------------------------------------------
     C     ENIAPF        WHENEQ    'Y'                                          INT ADJ PREM CHG
     C**** BEGIN ADD **** 01/20/04 ************************************
     C     ENCRFL        IFEQ      'Y'
     C                   MOVE      'X'           FMX14                            WC890615
     C                   ELSE
     C                   MOVE      *BLANK        FMX14
     C                   ENDIF
     C**** END   ADD **** 01/20/04 ************************************
     C                   MOVE      'X'           FMX15                            WC890616
     C**** BEGIN CHG **** 06/04/01 ************************************
     C********** 'See Revi'CAT  'sed WC':0FMC401             DESCRIPTION 1
     C**********           CAT  '00 00 ':1FMC401
     C**********           CAT  '01 A':1  FMC401
     C                   MOVEL     ENDSC1        FMC401
     C                   MOVEL     ENDSC2        FMC402
     C                   MOVEL     ENDSC3        FMC403
     C     'See Revi'    CAT       'sed WC':0    FMC405                           DESCRIPTION 1
     C                   CAT       '00 00 ':1    FMC405
     C                   CAT       '01 A':1      FMC405
     C**** END   CHG **** 06/04/01 ************************************
     C                   MOVE      'Y'           FMPBCG                           PREM BEARING CHG
     C*** BEGIN ADD *** 05/06/99 ***********************************************
     C*-------------------------------------------------------------------------
      *
      *** BEGIN DEL *** 06/29/99 ***********************************************
     C********** ENAELF    WHEQ 'Y'                        ADD OF ENT/LOC
     C****BEING*DEL *** 05/27/99 **************************************
     C********** ENDELF    OREQ 'Y'                        DEL OF ENT/LOC
     C****END***DEL *** 05/27/99 **************************************
     C********** ENCELF    OREQ 'Y'                        CHG OF ENT/LOC
     C**********           MOVE 'X'       FMX08              WC890608
     C**********           MOVELENOEN2    FMC401
     C**********           MOVELENOP12    FMC402
     C**********           MOVELENOP22    FMC403
     C**********
     C**********           MOVELENOPZ2    XXZIP5  5
     C**********           MOVE ENOPZ2    XXZIP4  4
     C********** ENOPC2    CAT  ',':0     FMC404    P
     C**********           CAT  ENOPS2:1  FMC404
     C**********           CAT  XXZIP5:1  FMC404
     C********** XXZIP4    IFNE *BLANKS
     C********** XXZIP4    ANDNE*ZEROS
     C**********           CAT  '-':0     FMC404
     C**********           CAT  XXZIP4:0  FMC404
     C**********           ENDIF
     C***FLOAT*CSZ UP IF NECESSARY
     C********** FMC403    IFEQ *BLANKS
     C**********           MOVELFMC404    FMC403
     C**********           MOVEL*BLANKS   FMC404
     C**********           ENDIF
     C**********           MOVE 'Y'       FMPACG           PHYS ADDR CHG
      *** END   DEL *** 06/29/99 ***********************************************
      *
     C*** END ADD ***** 05/06/99 ***********************************************
     C*
     C*** BEGIN ADD *** 05/27/99 ***********************************************
     C*-------------------------------------------------------------------------
     C     ENDELF        WHENEQ    'Y'                                          DEL OF ENT/LOC
      *** BEGIN ADD *** 06/29/99 ***********************************************
     C     ENAELF        OREQ      'Y'                                          ADD OF ENT/LOC
     C     ENCELF        OREQ      'Y'                                          CHG OF ENT/LOC
      *** END   ADD *** 06/29/99 ***********************************************
     C**** BEGIN ADD **** 01/22/04 ************************************
     C                   ADD       1             XXCNT
     C**** END ADD ****** 01/22/04 ************************************
     C                   MOVE      'X'           FMX08                            WC890608
     C                   MOVEL     XXEXT1        FMC401
     C                   MOVEL     XXEXT2        FMC402
     C                   MOVE      *BLANKS       FMC403
     C                   MOVE      *BLANKS       FMC404
     C                   MOVE      'Y'           FMPACG                         PHYS ADDR CHG
     C*** END   ADD *** 05/27/99 ***********************************************
     C**** BEGIN ADD **** 01/22/04 ************************************
     C     XXCNT         IFGT      1
     C                   MOVE      *OFF          XXPRNT
     C                   ENDIF
     C**** END ADD ****** 01/22/04 ************************************
     C*
     C*** BEGIN ADD *** 05/17/99 ***********************************************
     C*-------------------------------------------------------------------------
      *
      *** BEGIN DEL *** 06/29/99 ***********************************************
     C********** ENADDF    WHEQ 'Y'                        ADD OF DIVISION
     C****BEGIN*DEL *** 05/27/99 **************************************
     C********** ENDLDF    OREQ 'Y'                        DEL OF DIVISION
     C****END***DEL *** 05/27/99 **************************************
     C**********           MOVE 'X'       FMX08              WC890608
     C**********           MOVELENNAM2    FMC401
     C**********
     *****BEGIN*CHG *** 05/20/99 **************************************
     C**********
     C***Use*physical address if present, otherwise use mailing address
     C**********
     C********** ENPA12    IFNE *BLANKS
     C**********
     C**********           MOVELENPA12    FMC402
     C**********           MOVELENPA22    FMC403
     C**********
     C**********           MOVELENPZP2    XXZIP5  5
     C**********           MOVE ENPZP2    XXZIP4  4
     C********** ENPCT2    CAT  ',':0     FMC404    P
     C**********           CAT  ENPST2:1  FMC404
     C**********           CAT  XXZIP5:1  FMC404
     C********** XXZIP4    IFNE *BLANKS
     C********** XXZIP4    ANDNE*ZEROS
     C**********           CAT  '-':0     FMC404
     C**********           CAT  XXZIP4:0  FMC404
     C**********           ENDIF
     C**********
     C**********           ELSE
     C**********
     C**********           MOVELENMA12    FMC402
     C**********           MOVELENMA22    FMC403
     C**********
     C**********           MOVELENMZP2    XXZIP5  5
     C**********           MOVE ENMZP2    XXZIP4  4
     C********** ENMCT2    CAT  ',':0     FMC404    P
     C**********           CAT  ENMST2:1  FMC404
     C**********           CAT  XXZIP5:1  FMC404
     C********** XXZIP4    IFNE *BLANKS
     C********** XXZIP4    ANDNE*ZEROS
     C**********           CAT  '-':0     FMC404
     C**********           CAT  XXZIP4:0  FMC404
     C**********           ENDIF
     C**********
     C**********           ENDIF
     **** END   CHG *** 05/20/99 **************************************
     C**********
     C***FLOAT*CSZ UP IF NECESSARY
     C**********
     C********** FMC403    IFEQ *BLANKS
     C**********           MOVELFMC404    FMC403
     C**********           MOVEL*BLANKS   FMC404
     C**********           ENDIF
     C**********           MOVE 'Y'       FMPACG             PHYS ADDR CHG
      *** END   DEL *** 06/29/99 ***********************************************
      *
     C*** END ADD ***** 05/06/99 ***********************************************
     C*
     C*** BEGIN ADD *** 05/27/99 ***********************************************
     C*-------------------------------------------------------------------------
     C     ENDLDF        WHENEQ    'Y'                                          DEL OF DIV
      *** BEGIN ADD *** 06/29/99 ***********************************************
     C     ENADDF        OREQ      'Y'                                          ADD OF DIV
      *** END   ADD *** 06/29/99 ***********************************************
      ***** BEG ADD ******* 08/17/05 **********************************
     C     ENRDDF        OREQ      'Y'                                          RESCISSION
      ***** END ADD ******* 08/17/05 **********************************
     C                   MOVE      'X'           FMX08                            WC890608
     C                   MOVEL     XXEXT1        FMC401
     C                   MOVEL     XXEXT2        FMC402
     C                   MOVE      *BLANKS       FMC403
     C                   MOVE      *BLANKS       FMC404
     C                   MOVE      'Y'           FMPACG                           PHYS ADDR CHG
     C*** END   ADD *** 05/27/99 ***********************************************
     C*---------------------------------------------                   ---------
     C**** BEGIN ADD **** 03/27/01 ************************************
     C*-------------------------------------------------------------------------
     C     ENCIFL        WHENEQ    'Y'                                          CERT OF INS CHG
     C     ENAGT1        ANDNE     ENAGT2
     C                   MOVE      'X'           FMX07                            WC890607
     C                   MOVEL     AGNAME        FMC401
     C                   MOVE      'Y'           FMPRCG                         PRODUCER CHG
     C*-------------------------------------------------------------------------
     C**** END   ADD **** 03/27/01 ************************************
     C                   OTHER
     C                   MOVE      *OFF          XXPRNT
     C                   ENDSL
     C*
     C*  WRITE THE OUTPUT RECORD
     C*
     C     XXPRNT        IFEQ      *ON
     C                   MOVE      *ON           XWRT89
     C                   ADD       1             FMTIME
     C                   WRITE     PFM01
     C                   ENDIF
     C*
     C     NXTMEN        TAG
     C     MENKY         READE     PMEN01                                 60
     C                   ENDDO                                                    *IN60  DOWEQ *OFF
     C*
     C                   ENDSR
     C****************************************************************
     C*  WRITE A RECORD TO THE EMPLOYER ATTACHED ENDORSEMENT FORMS FILE
     C****************************************************************
     C     WRTPMU        BEGSR
     C*
     C*  DETERMINE THE CURRENT MID-YEAR ENDORSEMENT NUMBER
     C*
     C                   Z-ADD     LDEND#        XXEND#
     C*
     C     MUKY          SETGT     PMU01
     C     MUKY          READPE    PMU01                                  60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     1             XXSEQ#
     C                   ELSE
     C     MUSEQ#        ADD       1             XXSEQ#
     C                   ENDIF
     C*
     C*  CLEAR OUT THE RECORD
     C*
     C                   CLEAR                   PMU01
     C*
     C*  LOAD THE RECORD
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      XXCO#         MUCO#
     C                   MOVE      XXFND         MUFND
     C                   MOVE      XXEMP#        MUEMP#
     C                   MOVE      XXDIV         MUDIV
     C                   MOVE      XXFYR         MUFYR
     C                   MOVE      XXEND#        MUEND#
     C                   MOVE      XXSEQ#        MUSEQ#
     C                   MOVEL     XXFORM        MUFORM
      *** beg chg ***** 11/10/16 **************************
     C***********        MOVEL     ENST          MUENST
     C***********        MOVEL     ENDESC        MUDESC
     C                   MOVEL     t_ENdesc      MUdesc
     C                   MOVEL     t_ENst        MUenst
     C                   MOVEL     t_ENdsc2      MUdsc2
      *** end chg ***** 11/10/16 **************************
     C                   MOVE      ELPFDT        MUEEDT
     C                   MOVEL     'E'           MUEXFL
      *** beg chg ***** 11/10/16 **************************
     C***********        MOVEL     ENMNAU        MUMNAU
     C***********        MOVEL     ENPRNT        MUPRNT
     C                   MOVEL     t_ENMNAU      MUMNAU
     C                   MOVEL     t_ENPRNT      MUPRNT
      *** end chg ***** 11/10/16 **************************
     C                   MOVEL     'A'           MUSTAT
     C                   MOVEL     Q1USER        MUUSER
     C                   MOVEL     Q1PGM         MUPGM
     C                   MOVEL     DSTIME        MUTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUDT
     C*
     C                   MOVEL     Q1USER        MUIUSR
     C                   MOVEL     Q1PGM         MUIPGM
     C                   MOVE      MUDT          MUIDT
     C*
     C*  WRITE A RECORD TO THE EMPLOYER ATTECHED ENDORSEMENT FORMS FILE
     C*
     C                   WRITE     PMU01
     C*
     C                   ENDSR
     C*** BEG ADD ***** 02/18/98 ***********************************************
     C********************************************************************
     C*  INCREC - WRITE A MERGED FORMS DATA RECORD
     C********************************************************************
     C     INCREC        BEGSR
     C*
     C     XXINCF        IFNE      *ON
     C                   MOVE      *ON           XXINCF            1
     C*
     C     '.inc(WC8'    CAT(P)    '90600,':0    ICSTMT
     C                   CAT       'AMTEMP':0    ICSTMT
     C                   CAT       ',)':0        ICSTMT
     C*
     C                   WRITE     PIC01
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C****************************************************************
     C*** END ADD ***** 02/18/98 ***********************************************
