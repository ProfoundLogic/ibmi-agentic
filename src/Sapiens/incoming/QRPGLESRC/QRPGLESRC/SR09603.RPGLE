     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/TITLE SR09603 - LOAD ENDORSEMENT FORM WORKFILE FOR FORM WC090603
      **************************************************************************
      *    PROGRAM DESCRIPTION:                                                *
      *                                                                        *
      *       THIS PROGRAM IS CALLED TO:                                       *
      *         1) DETERMINE IF THIS ENDORSEMENT FORM SHOULD BE                *
      *            ATTACHED TO THE POLICY.                                     *
      *         2) CREATE DATA INTO THE ENDORSEMENT FORM WORKFILE              *
      *            (WXFMP) PRIOR TO A FORM MERGE.                              *
      *                                                                        *
      *                                                                        *
      * PROGRAMMER: C:57                                                       *
      * CREATED:    11/10/97            PROJECT: CM027                         *
      *                                                                        *
      * INDICATOR USAGE:                                                       *
      *                                                                        *
      * 60  GENERAL CHAIN (READ) INDICATOR                                     *
      *                                                                        *
      **************************************************************************
      * REVISIONS:                                                             *
      *                                                               *
      * 05/30/18 C:65  Project: 150WAD/WC2607                         *
      *                Implement logic to store data for type 43 rcds.*
      *                                                                        *
     * 05/22/13 C:287 Support: WA099 80909                                    *
     *                - Add Large Deductible logic                            *
      *                                                                        *
      * 07/14/11 C:061 Support 77336                                           *
      *                Do not attach for flat cancelled divisions.             *
      *                                                                        *
      * 12/16/10 C:61  PROJECT SY190 PRISS 207                                 *
      *                Populate new "Insured Name" and "Insured Federal        *
      *                Name" fields.                                           *
      *                                                                        *
      * 10/11/07 C:190 SUPPORT WA099 54504                                     *
      *                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE               *
      *                DISCOUNT CODE HAVING INACTIVE RECORDS.                  *
      *                                                                        *
      * 09/11/06 C:61  SUPPORT: WA132 54290 132WEW                             *
      *                - Call SRPAMT to get premium amount.                    *
      *                                                                        *
      *  04/10/06 C:65 PROJECT SUPP#51293                                      *
      *                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON             *
      *                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.           *
      *                                                                        *
      *  10/09/98 C:09 PROJECT CM027                                           *
      *                ONLY ACCUMULATE PREMIUM FOR A GIVEN FUND YEAR.          *
      *                                                                        *
      *  08/18/98 C:78 PROJECT CM027                                           *
      *                LOADING PREMIUM FOR ALL DIVISIONS.                      *
      *                                                                        *
      **************************************************************************
      *
     FWMFNL     IF   E           K DISK    USROPN
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMAHL     IF   E           K DISK    USROPN
     FWMAAL1    IF   E           K DISK    USROPN
     FWMAGL     IF   E           K DISK    USROPN
     FWTENL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWTDDL2    IF   E           K DISK
     F*** Begin Add ***** 05/22/13 *******************************************
     Fwtabl     if   e           k disk    include(pbs01)
     Fwmldl1    if   e           k disk
     F*** End Add ******* 05/22/13 *******************************************
     FWDF2L     IF   E           K DISK    USROPN
     FWMRPL     IF   E           K DISK    USROPN
     FWMMUP     O    E             DISK    USROPN
     FWXFMP     O    E             DISK    USROPN
     F******* BEGIN ADD ***** 10/09/98 ********************************
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     F******* END ADD ******* 10/09/98 ********************************
     F*** begin add ***  05/30/18  ***************************************
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     Fststl3    if   e           k disk
     F*** end add *****  05/30/18  ***************************************
     D*
     D XXFORM          C                   CONST('WC090603  ')
     D*
     *** Begin Add ***** 05/22/13 *******************************************
     D xxampo          s                   like(DDAMPO)
     D xxamag          s                   like(DDAMAG)
     D xxclas          s                   like(DDCLAS)
     D xxDedSize       s              1
     d*** begin add ***  05/30/18  ***************************************
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)
     d xxpremPct       s             12  9 inz(0)
     d xxlossSub       s              2  0 inz(0)
     d xxbasisDed      s              2  0 inz(0)
     d xxdedPct        s              2  0 inz(0)
     d xxdedAmt        s              9  0 inz(0)
     d xxdedAgg        s              9  0 inz(0)
     d xxdedClas       s              5  0 inz(0)
     d xxdedNet        s              1    inz(' ')
     d xxdedVerb       s              1    inz(' ')
     d xxPlan          s              1    inz(' ')
     d*** end add *****  05/30/18  ***************************************
     *=========================================================================
     *  Prototypes
     *=========================================================================
    D rtvDedSize      pr             1
     D  co#                                like(ELCO#)
     D  bscd                               like(ELBSCD)
    D rtvSmDedInfo    pr
     D  ampo                               like(DDAMPO)
     D  amag                               like(DDAMAG)
     D  clas                               like(DDCLAS)
    D rtvLgDedInfo    pr
     D  ampo                               like(DDAMPO)
     D  amag                               like(DDAMAG)
     D  clas                               like(DDCLAS)
      *** End Add ****** 05/22/13 *******************************************
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
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     d*** begin add ***  05/30/18  ***************************************
     D/COPY WPREMPLR
      /COPY WWCPOLDAT4

     C     stky          KList
     C                   KFld                    xxkey            10
     C     pekey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxdt              7 0
     C     pekey1        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C     pekey2        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxunid
     C                   KFld                    x1end#
     d*** end add *****  05/30/18  ***************************************
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
     C     ENKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XKFORM
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C*
     C*  DEDUCTIBLE DISCOUNT CODE FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELDCDE
     C*** BEGIN CHG ***  04/10/06  ************************************
     C*************        KFLD           ELPFDT
     C                   KFLD                    XXDT              7 0
     C*** END CHG *****  04/10/06  ************************************
     C*
     C     DDKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELDCDE
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
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C     *LIKE         DEFINE    ENFORM        XKFORM
     C     *LIKE         DEFINE    FMEMST        XXEMST
     C     *LIKE         DEFINE    FMEAD1        XXEAD1
     C     *LIKE         DEFINE    FMEAD2        XXEAD2
     C     *LIKE         DEFINE    FMEAD3        XXEAD3
     C     *LIKE         DEFINE    FNPST         XXFNST
     C     *LIKE         DEFINE    FMCARR        XXCARR
     C     *LIKE         DEFINE    FMCPL#        XXCPL#
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
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C*
     C                   MOVEL     XXFORM        XKFORM
     C*
     C********************************************************************
     C*  MAINLINE
     C********************************************************************
     C*
     C                   SELECT
     C*
     C*  DETERMINE WHETHER OR NOT TO ATTACH THIS FORM TO THE POLICY
     C*
     C     QQMODE        WHENEQ    '1'
     C                   EXSR      CHEKIT
     C*
     C*  GATHER INFO TO MERGE TO THE FORM
     C*
     C     QQMODE        WHENEQ    '2'
     C                   EXSR      BULDIT
     C                   ENDSL
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C********************************************************************
     C*  DETERMINE WHETHER OR NOT TO ATTACH THIS FORM TO THE POLICY
     C********************************************************************
     C     CHEKIT        BEGSR
     C*
     C                   MOVE      *OFF          CRTFRM            1
     C*
     C*  GET THE EMPLOYER AND FORM INFO
     C*
     C                   EXSR      GETEMP
     C                   EXSR      GETFRM
     C*
     C********************************************************************
     C*
     C*    VALIDATE ATTACHMENT OF FORM HERE
     C*
     C********************************************************************
     **** BEGIN CHG *** 02/17/99 **************************************
     C*
     C*  Check all divisions for a matching jurisdiction state
     C*
     C     ELKY2         CHAIN     PEL02                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C*  GET THE JURISDICTION STATE
     C*
     C     RSKY          CHAIN     PRS01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C********** RSJRST    ANDEQ'FL'
     C     RSJRST        ANDEQ     ENST
      ******* beg add ****** 07/14/11 *********************************
     C     elpfdt        andne     elptdt
      ******* end add ****** 07/14/11 *********************************
     C*
     C*  GET THE CURRENT DEDUCTIBLE DISCOUNT RECORD
     C*
     C*** BEGIN CHG ***  04/10/06  ************************************
     C*** Begin Delete ***** 05/22/13 *********************************
     C***                MOVE      ELDCEF        QQDATE

     C***                CALL      'SREFFD'
     C***                PARM                    QQCO#             3
     C***                PARM                    QQFND             3
     C***                PARM                    QQFYR             3
     C***                PARM                    QQEMP#            9
     C***                PARM                    QQDIV             5
     C***                PARM                    QQDATE            7

     C***                MOVE      QQDATE        XXDT
     C*** END CHG *****  04/10/06  ************************************
     C***  DDKY          SETGT     PDD01
     C***  DDKY1         READPE    PDD01                                  60
     C*
     C*  IF FOUND, AND ITS STAT CODE IS WITHIN ONE OF THE RANGES AS
     C*  ASSIGNED ON THE FORM TABLE RECORD, CRT THE ENDORSEMENT FORM
     C*
     C***  *IN60         IFEQ      *OFF
     C*** BEG ADD *****  10/11/07  ************************************
     C***  DDSTAT        ANDEQ     'A'
     C*** END ADD *****  10/11/07  ************************************
     C***  DDCLAS        ANDNE     *ZERO

     C***  DDCLAS        IFGE      ENSCF1                                       WITHIN RANGE 1
     C***  DDCLAS        ANDLE     ENSCT1                                        OR
     C***  DDCLAS        ORGE      ENSCF2                                       WITHIN RANGE 2
     C***  DDCLAS        ANDLE     ENSCT2                                        OR
     C***  DDCLAS        ORGE      ENSCF3                                       WITHIN RANGE 3
     C***  DDCLAS        ANDLE     ENSCT3
     C***                MOVE      *ON           CRTFRM
     C***                ENDIF                                                  DDCLAS >= ENSCF1
     C*
     C***                ENDIF                                                  *IN60 = *OFF
     C*** End Delete   ***** 05/22/13 *********************************
     C*
     C*** Begin Add ****** 05/22/13 ***********************************
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo : xxAmag : xxclas)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo : xxAmag : xxclas)
     C                   EndSl

     C     xxclas        IfGE      ENSCF1                                       WITHIN RANGE 1
     C     xxclas        AndLE     ENSCT1                                        OR
     C     xxclas        OrGE      ENSCF2                                       WITHIN RANGE 2
     C     xxclas        AndLE     ENSCT2                                        OR
     C     xxclas        OrGE      ENSCF3                                       WITHIN RANGE 3
     C     xxclas        AndLE     ENSCT3
     C                   Move      *On           CRTFRM
     C                   Endif                                                  DDCLAS >= ENSCF1

     C*** End Add     ***** 05/22/13 *********************************
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C     ELKY2         READE     PEL02                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C********************************************************************
     C*
     C     CRTFRM        IFEQ      *ON
     C*
     C*  Reset the employer detail and rate set code files
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     RSKY          CHAIN     PRS01                              60
     **** END   CHG *** 02/17/99 **************************************
     C*
     C*  OPEN THE EMPLOYER ATTACHED ENDORSEMENT FORMS FILE
     C*
     C                   OPEN      WMMUP
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
     C                   MOVE      *ZEROS        MUEND#
     C                   MOVE      *ZEROS        MUSEQ#
     C                   MOVEL     XXFORM        MUFORM
     C                   MOVEL     ENST          MUENST
     C                   MOVEL     ENDESC        MUDESC
     C                   MOVE      ELPFDT        MUEEDT
     C                   MOVEL     'I'           MUEXFL
     C                   MOVEL     ENMNAU        MUMNAU
     C                   MOVEL     ENPRNT        MUPRNT
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
     C*  CLOSE THE EMPLOYER ATTECHED ENDORSEMENT FORMS FILE
     C*
     C                   CLOSE     WMMUP
     C*
     C                   ENDIF                                                  CRTFRM = *ON
     C*
     C                   ENDSR
     C********************************************************************
     C*  GATHER INFO TO MERGE TO THE FORM
     C********************************************************************
     C     BULDIT        BEGSR
     C*
     C*  OPEN THE ENDORSEMENT FORM WORKFILE
     C*
     C                   OPEN      WXFMP
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
     C*  CLOSE THE ENDORSEMENT FORM WORKFILE
     C*
     C                   CLOSE     WXFMP
     C*
     C                   ENDSR
     C********************************************************************
     C*  GETFND - GET FUND INFORMATION
     C********************************************************************
     C     GETFND        BEGSR
     C*
     C                   OPEN      WMFNL
     C                   OPEN      WDF2L
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
     C                   CLOSE     WMFNL
     C                   CLOSE     WDF2L
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
     C                   OPEN      WMAHL
     C                   OPEN      WMAAL1
     C                   OPEN      WMAGL
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
     C                   CLOSE     WMAHL
     C                   CLOSE     WMAAL1
     C                   CLOSE     WMAGL
     C*
     C                   ENDSR
     C********************************************************************
     C*  GETFRM - GET ENDORSMENT FORM TABLE RECORD
     C********************************************************************
     C     GETFRM        BEGSR
     C*
     C*  GET THE ENDORSMENT FORM TABLE RECORD
     C*
     C     ENKY          CHAIN     PEN01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ENSTAT        ANDEQ     'I'
     C     ENKY          READE     PEN01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C*
     C                   ENDSR
     C********************************************************************
     C*  GETCAR - GET CARRIER INFORMATION
     C********************************************************************
     C     GETCAR        BEGSR
     C*
     C                   OPEN      WMRPL
     C*
     C*  GET THE CARRIER INFO RECORD
     C*
     C     RPKY          CHAIN     PRP01                              60
     C*
     C                   MOVEL     RPCAP#        XXCPL#
     C*
     C                   CLOSE     WMRPL
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
     C                   MOVE      ENEDDT        DSDAT7
     C                   MOVEL     DSMM          FMEDDT
     C                   MOVE      DSYY          FMEDDT
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FMEEDT
     C*
     C                   Z-ADD     *ZERO         FMEND#
     C*
     C     QQFND         CAT(P)    QQEMP#:0      FMPOL#
     C                   CAT       QQFYR:0       FMPOL#
     C*
     C                   MOVEL     EMTPOB        FMTOBC
     C                   MOVEL     EMNAME        FMEMNM
     C                   MOVEL     EMFDNM        FMFENM
      ****** beg add ****** 12/16/10 **********************************
     C                   eval      fminnm = emname + emnam2
     C                   eval      fmifnm = emfdnm + emfdn2
      ****** end add ****** 12/16/10 **********************************
     C                   MOVEL     XXFNST        FMFNST
     C                   MOVEL     FNNAME        FMFNNM
      ******* BEGIN CHG ******** 10/09/98 *****************************
      ******* BEGIN ADD ******** 08/18/98 *****************************
     C                   Z-ADD     XXDIV         SVDIV
     C                   Z-ADD     0             XXENMP
      ***** BEG DEL ******* 09/11/06 **********************************
     C********   ELKY2     CHAINPEL02                61
     C********   *IN61     DOWEQ'0'
     C********             ADD  ELENMP    XXENMP
     C********   ELKY2     READEPEL02                    61
     C********             ENDDO
      ********
      ***** END DEL ******* 09/11/06 **********************************
      ***** BEG ADD ******* 09/11/06 **********************************
      *
     C                   CALL      'SRPAMT'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     C                   PARM                    XXEMP#
     C                   PARM                    XXENMP
     C                   PARM      *BLANK        XXCLR             1
      *
      ***** END ADD ******* 09/11/06 **********************************
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
     **** BEGIN CHG *** 02/17/99 **************************************
     C*
     C*  Check each division for a matching jurisdiction state
     C*
     C     ELKY2         CHAIN     PEL02                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C*  GET THE JURISDICTION STATE
     C*
     C     RSKY          CHAIN     PRS01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C********** RSJRST    ANDEQ'FL'
     C     RSJRST        ANDEQ     ENST
     C*
     C*  GET THE CURRENT DEDUCTIBLE DISCOUNT RECORD
     C*
     C*** BEGIN CHG ***  04/10/06  ************************************
     C*** Begin Delete *** 05/22/13 ***********************************
     C***                MOVE      ELDCEF        QQDATE
     C*
     C***                CALL      'SREFFD'
     C***                PARM                    QQCO#             3
     C***                PARM                    QQFND             3
     C***                PARM                    QQFYR             3
     C***                PARM                    QQEMP#            9
     C***                PARM                    QQDIV             5
     C***                PARM                    QQDATE            7

     C***                MOVE      QQDATE        XXDT
     C*** END CHG *****  04/10/06  ************************************
     C***  DDKY          SETGT     PDD01
     C***  DDKY1         READPE    PDD01                                  60
     C*
     C*  IF FOUND, AND ITS STAT CODE IS WITHIN ONE OF THE RANGES AS
     C*  ASSIGNED ON THE FORM TABLE RECORD, CRT THE ENDORSEMENT FORM
     C*
     C***  *IN60         IFEQ      *OFF
     C*** BEG ADD *****  10/11/07  ************************************
     C***  DDSTAT        ANDEQ     'A'
     C*** END ADD *****  10/11/07  ************************************
     C***  DDCLAS        ANDNE     *ZERO
     C*
     C***  DDCLAS        IFGE      ENSCF1                                       WITHIN RANGE 1
     C***  DDCLAS        ANDLE     ENSCT1                                        OR
     C***  DDCLAS        ORGE      ENSCF2                                       WITHIN RANGE 2
     C***  DDCLAS        ANDLE     ENSCT2                                        OR
     C***  DDCLAS        ORGE      ENSCF3                                       WITHIN RANGE 3
     C***  DDCLAS        ANDLE     ENSCT3
     C***                Z-ADD     DDAMPO        FMA701
     C***                Z-ADD     DDAMAG        FMA702
     C***                LEAVE
     C***                ENDIF                                                  DDCLAS >= ENSCF1
     C*
     C***                ENDIF                                                  *IN60 = *OFF
     C*** End Delete   ***** 05/22/13 *********************************
     C*
     C*** Begin Add ****** 05/22/13 ***********************************
     C                   Eval      xxDedSize = rtvDedSize (ELCO#:ELBSCD)
     C                   Select
     C                   When      xxDedSize = 'L'
     C                   Callp     rtvLgDedInfo (xxAmpo : xxAmag : xxclas)
     C                   When      xxDedSize = 'S'
     C                   Callp     rtvSmDedInfo (xxAmpo : xxAmag : xxclas)
     C                   EndSl

     C     xxclas        IfGE      ENSCF1                                       WITHIN RANGE 1
     C     xxclas        AndLE     ENSCT1                                        OR
     C     xxclas        OrGE      ENSCF2                                       WITHIN RANGE 2
     C     xxclas        AndLE     ENSCT2                                        OR
     C     xxclas        OrGE      ENSCF3                                       WITHIN RANGE 3
     C     xxclas        AndLE     ENSCT3
     C                   Z-Add     xxampo        FMA701
     C                   Z-Add     xxamag        FMA702
     C*** Begin Add *** 05/30/18 **********************************
     C                   Exsr      WriteWCPOLS
     C*** End Add ***** 05/30/18 **********************************
     C                   Leave
     C                   Endif                                                  DDCLAS >= ENSCF1

     C*** End Add     ***** 05/22/13 *********************************

     C                   ENDIF                                                  *IN60 = *ON

     C     ELKY2         READE     PEL02                                  60
     C                   ENDDO
     **** END   CHG *** 02/17/99 **************************************

     C*  WRITE THE OUTPUT RECORD

     C                   WRITE     PFM01

     C                   ENDSR
     c*** begin add ***  05/30/18  ******************************************
      *=========================================================================
      *  WriteWCPOLS - record information for electronic data record
      *=========================================================================
     C     WriteWCPOLS   Begsr
      *  If printing a policy page.  (Not called from W1054R)

     c                   call      'SRCHKPGM'
     c                   parm      'W1054R   '   #1pgm            10
     c                   parm      *BLANK        #1flag            1

     C                   if        #1flag = '0'

     C                   Clear                   xxpremPct
     C                   Clear                   xxlossSub
     C                   Clear                   xxbasisDed
     C                   Clear                   xxdedPct
     C                   Clear                   xxdedAmt
     C                   Clear                   xxdedAgg
     C                   Clear                   xxdedClas
     C                   Clear                   xxdedNet
     C                   Clear                   xxdedVerb
     C                   Clear                   xxPlan
      /free
       xxPlan = RtvDeductible(elco#:elfnd:elemp#:eldiv:elfyr:xxPremPct:
        xxDedAmt:xxDedAgg:xxLossSub:xxBasisDed:xxDedNet:xxDedVerb:
        xxDedClas:xxDedPct);
      /end-free
      *
      * Find the unique ID number.
      *
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             xxunid
     C*
     c                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      *
      * Copy data into the record.
      *
     C                   If        *in60 = *off
      *
     c                   movel(p)  rsjrst        xxkey
     c     stky          chain     pst01                              60
     c                   if        *in60 = *off
     c                   move      stnst         PEST
     c                   endif
      *
     c                   clear                   E00403
     C                   eval      xxdac = xxDedAmt
     C                   eval      xxprp = xxPremPct
     C                   eval      xxlsd = xxLossSub
     C                   eval      xxbdc = xxBasisDed
     C                   eval      xxdpc = xxDedPct
     C                   eval      xxdag = xxDedAgg
     c                   move      *blanks       pedata
     c                   movel     E00403        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
      *
     C                   Update    ppe01
      *
     C                   Endif                                                  *in60 ifeq off
      *
     C                   Endif
      *
     C                   Endsr
      *=========================================================================
     c*** end add *****  05/30/18  ******************************************

     C*** Begin Add ***** 05/22/13 **************************************

     *=========================================================================
     * Query whether policy is small or large deductible
     *=========================================================================
    P rtvDedSize      b

    D rtvDedSize      pi             1                                         return S/L
     D  co#                                like(ELCO#)
     D  bscd                               like(ELBSCD)

     D kkco#           s                   like(BSCO#)
     D kkkey           s                   like(BSKEY)

     C     bsky          Klist
     C                   Kfld                    kkco#
     C                   Kfld                    kkkey

     C     rsky          Chain     prs01
     C                   If        not(%found)
     C                   Eval      RSJRST = *blank
     C                   Return    ' '
     C                   Endif

     C                   Eval      kkco# = co#
     C                   Movel(p)  bscd          kkkey
     C     bsky          Chain     pbs01
     C                   Select
     C                   When      BSLPGM = 'L'
     C                   Return    'L'
     C                   Other
     C                   Return    'S'
     C                   Endsl

    P rtvDedSize      e
     *=========================================================================
     * rtvSmDedInfo  - retrieves DDAMPO and DDAMAG
     *=========================================================================
    P rtvSmDedInfo    b
    D rtvSmDedInfo    pi
     D  ampo                               like(DDAMPO)
     D  amag                               like(DDAMAG)
     D  clas                               like(DDCLAS)
     D  kkefdt         s                   like(DDEFDT)

     C     ddky1         Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    RSJRST
     C                   Kfld                    ELDCDE
     C                   Kfld                    kkefdt

     C     ddky          Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    RSJRST
     C                   Kfld                    ELDCDE

     C                   Move      ELCO#         qqco#
     C                   Move      ELFND         qqfnd
     C                   Move      ELEMP#        q1emp#
     C                   Move      ELDIV         q1div
     C                   Move      ELFYR         q1fyr
     C                   Move      ELDCEF        q2date

     C                   Call      'SREFFD'
     C                   Parm                    qqco#             3
     C                   Parm                    qqfnd             3
     C                   Parm                    q1fyr             3
     C                   Parm                    q1emp#            9
     C                   Parm                    q1div             5
     C                   Parm                    q2date            7

     C                   Move      q2date        kkefdt
     C     ddky1         Setgt     pdd01
     C     ddky          Readpe    pdd01
     C                   Dow       not(%eof) and DDSTAT <> 'A'
     C     ddky          Readpe    pdd01
     C                   Enddo

     C                   If        not(%eof)
     C                   Eval      ampo = DDAMPO
     C                   Eval      amag = DDAMAG
     C                   Eval      clas = DDCLAS
     C                   Endif

    P rtvSmDedInfo    e
     *=========================================================================
     * rtvLgDedInfo - retrieves LDAMPO and LDAMAG
     *=========================================================================
    P rtvLgDedInfo    b
    D rtvLgDedInfo    pi
     D  ampo                               like(DDAMPO)
     D  amag                               like(DDAMAG)
     D  clas                               like(DDCLAS)

     C     elky          Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELEMP#
     C                   Kfld                    ELDIV
     C                   Kfld                    ELFYR

     C     elky          Chain     pld01
     C                   If        %found
     C                   Eval      ampo = LDAMP1
     C                   Eval      amag = LDAMG1
     C                   Eval      clas = LDCLS1
     C                   Endif

    P rtvLgDedInfo    e

      *** End Add ***** 05/22/13 ***********************************************
