     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')
     H/TITLE SR99313 - Waiver of Subrogation Endorsement
      ****************************************************************
      *                                                               *
      *          WC990313 - Waiver of subrogation - CA                *
      *                                                               *
      * PROGRAM DESCRIPTION:                                          *
      *                                                               *
      * This program is called to:                                    *
      *  1) Determine if this endorsement form should be attached     *
      *     to the policy.                                            *
      *  2) Create data in the Endorsement Form workfile (WXFMP)      *
      *     prior to a form merge.                                    *
      *                                                               *
      * PROGRAMMER: C:267                                             *
      * PROJECT:    C150WBP                                           *
      * CREATED:    10/16/17                                          *
      *             Cloned from SR00313                               *
      *             Note: this is for SUNZ client and CA state only   *
      *                                                               *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
      * ------------------------------------------------------------- *
      *****************************************************************
      * FILE USAGE:                                                   *
      *                                                               *
      *  WDELL  (PEL01):  CO#/FND/EMP#/DIV/FYR                        *
      *  WDELL1 (PEL02):  CO#/FND/FYR/EMP#/DIV                        *
      *  WDF2L  (PF201):  CO#/FND                                     *
      *  WMAAL1 (PAA01):  CO#/FND/AGT#                                *
      *  WMAGL  (PAG01):  CO#/FEIN/MOD                                *
      *  WMAHL  (PAH01):  CO#/FND/EMP#/DIV/FYR/EFDT/AGT#              *
      *  WMEML  (PEM01):  CO#/FND/EMP#/DIV                            *
      *  WMFNL  (PFN01):  CO#/FND                                     *
      *  WMRPL  (PRP01):  CO#/FND/EMP#/DIV/FYR/CARR                   *
      *  WMWVL  (PWV01):  CO#/FND/EMP#/DIV/FYR                        *
      *  WTENL  (PEN01):  CO#/FND/FORM                                *
      *  WMMUL4 (PMU01):  EMPLOYER ATTACHED ENDORSEMENT FORM FILE     *
      *  WXFMP  (PFM01):  ENDORSEMENT FORM WORKFILE                   *
      *  WXICP  (PXI01):  MIDTERM-ENDORSEMENT FILE                    *
      *                                                               *
      *****************************************************************
      * INDICATOR USAGE:                                              *
      *                                                               *
      *  60  GENERAL CHAIN (READ) INDICATOR                           *
      *                                                               *
      *****************************************************************
      * SUBROUTINE SUMMARY:                                           *
      *                                                               *
      *  BASINF - Collect the basic info for WXFMP.                   *
      *  BULDIT - Gather information to merge to the form.            *
      *  CHEKIT - Determine whether or not to attach the form.        *
      *  GETAGT - Get the agent information.                          *
      *  GETCAR - Get the carrier information.                        *
      *  GETEMP - Get the employer information.                       *
      *  GETFND - Get the fund information.                           *
      *  GETFRM - Get the endorsement form information.               *
      *  OTHINF - Get the form-specific information.                  *
      *  INCREC - Write a merged forms data record.                   *
      *                                                               *
      *****************************************************************
      * NOTES:                                                        *
      *                                                               *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *                                                               *
      *****************************************************************
     FWDELL     IF   E           K DISK    infds(elfds)
      *  Employer Policy Year
     FWDELL1    IF   E           k disk    usropn
     F                                     rename(PEL01:PELL1)
     FWDF2L     IF   E           K DISK    USROPN
     FWMAAL1    IF   E           K DISK    USROPN
     FWMAGL     IF   E           K DISK    USROPN
     FWMAHL     IF   E           K DISK    USROPN
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK    USROPN
     FWMMUL4    UF A E           K DISK    USROPN
     FWMRPL     IF   E           K DISK    USROPN
      * Waiver
     FWMWVL6    IF   E           K DISK
     F                                     RENAME(PWV01:PWV06)
     FWMWVL7    IF   E           K DISK
     F                                     RENAME(PWV01:PWV07)
     FWTENL     IF   E           K DISK
     FWXFMP     O    E             DISK    USROPN
     FWXFMP3    O    E             DISK    USROPN
     FWXICP     O    E             DISK    USROPN
     FWXPEL     UF A E           K DISK
     FWMSHL1    IF   E           K DISK
     FWMMUL     IF   E           K DISK
     F                                     RENAME(PMU01:PMU02)
     FXMLPREL1  UF   E           K DISK
     FXMLTEML1  UF   E           K DISK
     fWXWXFMP   o    e             disk    UsrOpn

     D formName        s             15A
     D xxproc          s              1
      *================================================================
      * Data structures & constants
      *================================================================
     D/COPY WWCPOLDAT4

     d elfds           ds
     d   elrrn               397    400B 0

     D qqFORM          C                   CONST('WC990313       ')

     D                 DS
     D  XXZIP                  1      9
     D  XXZIP1                 1      5
     D  XXZIP2                 6      9

     D                 DS
     D  DSDAT7                 1      7  0
     D  DSCENT                 1      1
     D  DSYY                   2      3
     D  DSMM                   4      5
     D  DSDD                   6      7

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     D                 DS
     D  XXNAME                 1    100
     D  FMEMNM                 1     40
     D  FMC215                41     60
     D  FMC441                61    100

     D                 DS
     D  XXFNAM                 1    100
     D  FMFENM                 1     40
     D  FMC216                41     60
     D  FMC442                61    100

     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     d ldaXml                191    250

     D                 DS
     D  NAME                   1    100
     D  WVNAME                 1     40
     D  WVNAM2                41    100
     D                 DS
     D  SNAME                  1    100
     D  SNAM1                  1     50
     D  SNAM2                 51    100
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D isScriptUsed    s              1A   inz('N')

     D PrintingPolicy  S               N
     D RunningSRText   S               N   Inz(*Off)
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     D outCall         s             11
     D InXML           s             60
     D InXML@          s             61
     D InReturn        s            250
     D OutMode         s              1
     D OutRrn          s              9
     D OutPrefix       s              2
     D OutString       s             10
     D OutCo#          s              3
     D OutFnd          s              3
     D pgmInStack      s              1
     D processType     s            250
     D UseScript       s              1n
     D UsePrintOut     s              1n
     D UseXML          S              1n

     D Date8           s               d
     D sysDt7          s              7  0
     D cxDate7         s              7  0
     D endEffDt7       s              7  0
     D midTermEnd      s              1n
     D useDiv          s              1n
     D xxstate         s              2
     D xxwaive         s             11  2

     D* Returns '1' in parm 2 if pgm in stack
     D ChkPgmInStack   pr                  ExtPgm('SRCHKPGM')
     D                               10    const
     D                                1
     D*
     D* Scripture Processing Program
     D StLetter        pr                  ExtPgm('STLETTER')
     D   outCall_                    11
     D   InXML_                      60
     D   InReturn_                  250    options(*nopass)
     D   OutMode_                     1    const options(*nopass)
     D   OutRrn_                      9    const options(*nopass)
     D   OutPrefix_                   2    const options(*nopass)
     D   OutString_                  10    const options(*nopass)
     D   OutCo#_                      3    const options(*nopass)
     D   OutFnd_                      3    const options(*nopass)
     D/copy sPrModule
     D/copy sPrXML

      ****/copy sPrDate
     D/copy wPrEmplr
     D/copy wPrRateSet

     d SendToScriptura...
     d                 pr                  ExtPgm('S4401C')
     d  outXMLString                 61

      *================================================================
      * KLIST definitions
      *================================================================
      * Agent Assignment file
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
      * Agent Master file
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
      * Agent History file
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      * Employer Detail file
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR

     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
      * Employer Master file
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      * Endorsement Form Table file
     C     ENKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XKFORM
      * Fund Master file
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      * Fund Auxiliary file
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      * Carrier file
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      * Employer Attached Endorsement Forms file
     C     MUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEND#
      * Waiver Detail file
     C     WVKY6         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR

     C     WVKY7         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    LDEND#

     C     PEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C     PEKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXUNID
     C                   KFLD                    X1END#
     C     STKY          KLIST
     C                   KFLD                    XXKEY            10
      * Employer Endorsement Forms File Key
     C     MUKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XKFORM
      * SUBMISSION HISTORY FILE
     C     SHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    X1TYPE
      * WAIVERS TO PRINT FILE
     C     W8KY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *================================================================
      * *LIKE DEFN Definitions
      *================================================================
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C     *LIKE         DEFINE    ENFORM        XXFORM
     C     *LIKE         DEFINE    ENFORM        XKFORM
     C     *LIKE         DEFINE    FMEMST        XXEMST
     C     *LIKE         DEFINE    FMEAD1        XXEAD1
     C     *LIKE         DEFINE    FMEAD2        XXEAD2
     C     *LIKE         DEFINE    FMEAD3        XXEAD3
     C     *LIKE         DEFINE    FNPST         XXFNST
     C     *LIKE         DEFINE    FMCARR        XXCARR
     C     *LIKE         DEFINE    FMCPL#        XXCPL#
     C     *LIKE         DEFINE    MUEND#        XXEND#
     C     *LIKE         DEFINE    MUSEQ#        XXSEQ#
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C     *LIKE         DEFINE    ELENMP        XXENMP
     C     *LIKE         DEFINE    FMEEDT        XXEFDT
     C     *LIKE         DEFINE    PEUNID        XXUNID
     C     *LIKE         DEFINE    PEEND#        X1END#
     C     *LIKE         DEFINE    DSTIME        SVTIME
      *================================================================
      * PLIST Definitions
      *================================================================
      *  Date check/conversion parameters
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *  Entry parameters
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQMODE            1
     C                   PARM                    QQRRN             9 0
     C                   PARM                    QQMOD2            1
     C                   PARM                    QQ313             1
      *================================================================
      * Main
      *================================================================
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR

     c                   call      'RTVCLTID'
     c                   parm                    cltid             3

     C                   MOVEL(P)  QQFORM        XXFORM
     C                   MOVEL(P)  XXFORM        XKFORM

     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA

      /Free
          formName = %subst(xxForm :1 :2) + ' ' + %subst(xxForm :3 :2) + ' ' +
           %subst(xxForm :5 :2) + ' ' + %subst(xxForm :7 :2) + ' ' +
           %subst(xxForm :9 :2);
          Exsr Scriptura;
      /End-Free

     C                   SELECT
      * Gather information to merge to the form
     C     QQMODE        WHENEQ    '2'
     C                   EXSR      BULDIT
      * Call additional information entry program
     C     QQMODE        WHENEQ    '3'

     C                   CALL      'W0815R'
     C                   PARM      *OFF          ##KC              1
     C                   PARM      *OFF          ##KL              1
     C                   PARM      QQMOD2        ##CALL            1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      QQRRN         ##RRN2            9 0
     C                   PARM      'SR99313'     ##CPGM           10
      * Gather info to merge to the form for mid-term endors processing
     C     QQMODE        WHENEQ    '4'

     C                   EXSR      CHEKIT

     C     CRTFRM        IFEQ      *ON
     C                   EXSR      BULDIT
     C                   EXSR      INCREC
     C                   ENDIF

     C                   ENDSL

     C                   MOVE      *ON           *INLR
     C                   RETURN
      *================================================================***
      * SRPROC - Determine if Waiver record to be processed
      *================================================================***
     C     SRPROC        BEGSR

     c                   select
      * clt150
     c                   when      (cltid = '150')
     c                   if        wvemst =  'CA'
     c                   eval      xxproc = 'Y'
     c                   else
     c                   eval      xxproc = 'N'
     c                   endif
      * not clt150
     c                   other
     c                   eval      xxproc = 'N'

     c                   endsl

     C                   endsr
      *================================================================***
      * BASINF - Subroutine to load basic WXFMP fields
      *================================================================***
     C     BASINF        BEGSR

     C                   TIME                    DSDTTM

     C                   MOVE      XXCO#         FMCO#
     C                   MOVE      XXFND         FMFND
     C                   MOVE      XXFYR         FMFYR
     C                   MOVE      XXEMP#        FMEMP#
     C                   MOVE      XXDIV         FMDIV
     C                   MOVEL(P)  XXFORM        FMFORM

     C                   MOVE      ENEDDT        DSDAT7
     C                   MOVEL     DSMM          FMEDDT
     C                   MOVE      DSYY          FMEDDT

     C     LDENDA        IFNE      *BLANKS
     C     LDENDA        ANDNE     *ZEROS
     C                   MOVE      LDEEDT        FMEEDT
     C                   Z-ADD     LDEND#        FMEND#
     C                   ELSE
     C     MUKY1         CHAIN     PMU02                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     MUEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FMEEDT
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FMEEDT
     C                   ENDIF

     C                   Z-ADD     *ZERO         FMEND#

     C                   ENDIF

     C     QQFND         CAT(P)    QQEMP#:0      FMPOL#
     C                   CAT       QQFYR:0       FMPOL#

     C                   MOVEL     EMTPOB        FMTOBC
     C                   MOVEL     EMNAME        FMEMNM
     C                   MOVEL     EMFDNM        FMFENM
     C                   eval      fminnm = emname + emnam2
     C                   eval      fmifnm = emfdnm + emfdn2
     C                   MOVEL     XXFNST        FMFNST
     C                   MOVEL     FNNAME        FMFNNM
     C                   Z-ADD     XXDIV         SVDIV
     C                   Z-ADD     0             XXENMP

     C                   CALL      'SRPAMT'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     C                   PARM                    XXEMP#
     C                   PARM                    XXENMP
     C                   PARM      *BLANK        XXCLR             1

     C                   Z-ADD     SVDIV         XXDIV
     C     ELKY          CHAIN     PEL01                              60
     C                   Z-ADD     XXENMP        FMPREM
     C                   MOVE      XXAGT#        FMAGT#
     C                   MOVEL     AGNAME        FMAGNM
     C                   MOVEL     AGPST         FMAGST
     C                   MOVEL     XXCPL#        FMCPL#
     C                   MOVE      XXCARR        FMCARR
     C                   MOVEL     XXEAD1        FMEAD1
     C                   MOVEL     XXEAD2        FMEAD2
     C                   MOVEL     XXEAD3        FMEAD3
     C                   MOVEL     XXEMST        FMEMST

     C                   MOVE      ELANDT        DSDAT7
     C                   MOVE      DSDD          FMANDA
     C                   MOVE      DSMM          FMANMO

     C                   MOVEL     Q1USER        FMUSER
     C                   MOVEL     Q1PGM         FMPGM

     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FMDT

     C                   MOVEL     DSTIME        FMTIME
     C                   MOVEL     'A'           FMSTAT
     C                   MOVEL     Q1USER        FMIUSR
     C                   MOVEL     Q1PGM         FMIPGM
     C                   MOVE      FMDT          FMIDT

     C                   ENDSR
      *================================================================***
      * BULDIT - Subroutine to gather info to merge to the form
      *================================================================***
     C     BULDIT        BEGSR
      * Open the endorsement form workfile
     C                   OPEN      WXFMP
     C                   OPEN      WXFMP3
      * Get the basic information
     C                   EXSR      GETFND
     C                   EXSR      GETEMP
     C                   EXSR      GETAGT
     C                   EXSR      GETFRM
     C                   EXSR      GETCAR
      * Clear out the record
     C                   CLEAR                   PFM01
     C                   CLEAR                   PFM03
      * Load basic WXFMP fields
     C                   EXSR      BASINF
      * Load form specific fields
     C                   EXSR      OTHINF
      * Close the endorsement form workfile
     C                   CLOSE     WXFMP
     C                   CLOSE     WXFMP3

     C                   ENDSR
      *================================================================***
      * CHEKIT -Subroutine to determine if this form should be attached
      *================================================================***
     C     CHEKIT        BEGSR

     C                   MOVE      *OFF          CRTFRM            1
      * Get the employer and form information
     C                   EXSR      GETEMP
     C                   EXSR      GETFRM
      * Validate attachment of form here
     C     Q1PRMS        IFGE      9
     C                   MOVE      *OFF          QQ313             1
     C                   ENDIF
      * Look for any waivers.
      *  mid-term process
     C     WVKY7         CHAIN     PWV07                              60

     C     *IN60         DOWEQ     *OFF

     C     WVSTAT        IFNE      'I'
     c                   exsr      srproc
     c                   if        xxproc = 'Y'
     C     Q1PRMS        IFGE      9
     C                   MOVE      *ON           QQ313             1
     C                   ENDIF
     C                   MOVE      *ON           CRTFRM
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF

     C     WVKY7         READE     PWV07                                  60
     C                   ENDDO
      *  End of validation
     C     CRTFRM        IFEQ      *ON
      * Open the Employer Attached Endorsement Forms file
     C                   OPEN      WMMUL4
     C                   Z-ADD     *ZEROS        XXEND#
     C                   Z-ADD     *ZEROS        XXSEQ#
      * If processing for Mid-Year Endorsements, retrieve the previous
      *  endorsement and sequence numbers.
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C                   Z-ADD     LDEND#        XXEND#

     C     MUKY          SETGT     PMU01
     C     MUKY          READPE    PMU01                                  60

     C     *IN60         IFEQ      *ON
     C                   Z-ADD     1             XXSEQ#
     C                   ELSE
     C     MUSEQ#        ADD       1             XXSEQ#
     C                   ENDIF

     C                   ENDIF
      * Clear out the record
     C                   CLEAR                   PMU01
      * Load the record
     C                   TIME                    DSDTTM

     C                   MOVE      XXCO#         MUCO#
     C                   MOVE      XXFND         MUFND
     C                   MOVE      XXEMP#        MUEMP#
     C                   MOVE      XXDIV         MUDIV
     C                   MOVE      XXFYR         MUFYR
     C                   MOVE      XXEND#        MUEND#
     C                   MOVE      XXSEQ#        MUSEQ#
     C                   MOVEL(P)  XXFORM        MUFORM
     C                   MOVEL     ENST          MUENST
     C                   MOVEL     ENDESC        MUDESC
     C                   MOVEL     ENdsc2        MUdsc2
     C                   MOVE      ELPFDT        MUEEDT

     C     xxEnd#        IFGT      *Zeros
     C                   Z-ADD     LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUEEDT
     C                   ENDIF

     C                   MOVEL     'I'           MUEXFL
     C                   MOVEL     ENMNAU        MUMNAU
     C                   MOVEL     ENPRNT        MUPRNT
     C                   MOVEL     'A'           MUSTAT
     C                   MOVEL     Q1USER        MUUSER
     C                   MOVEL     Q1PGM         MUPGM
     C                   MOVEL     DSTIME        MUTIME

     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUDT

     C                   MOVEL     Q1USER        MUIUSR
     C                   MOVEL     Q1PGM         MUIPGM
     C                   MOVE      MUDT          MUIDT
      * Write the record.
     C                   WRITE     PMU01
      * Close the Employer Attached Endorsement Forms file.
     C                   CLOSE     WMMUL4

     C                   ENDIF

     C                   ENDSR
      *================================================================***
      * GETAGT - Subroutine to get agent information
      *================================================================***
     C     GETAGT        BEGSR

     C                   OPEN      WMAHL
     C                   OPEN      WMAAL1
     C                   OPEN      WMAGL

     C                   Z-ADD     *ZERO         XXAGT#
      * Get the current agent history record
     C     AHKY          SETGT     PAH01
     C     AHKY          READPE    PAH01                                  60

     C     *IN60         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     AHKY          READPE    PAH01                                  60
     C                   ENDDO

     C     *IN60         IFEQ      *OFF
     C                   MOVEL     AHAGT#        XXAGT#
     C                   ENDIF

     C  N60AAKY          CHAIN     PAA01                              60
     C  N60AGKY          CHAIN     PAG01                              60

     C                   CLOSE     WMAHL
     C                   CLOSE     WMAAL1
     C                   CLOSE     WMAGL

     C                   ENDSR
      *================================================================***
      * GETCAR - Subroutine to get carrier information
      *================================================================***
     C     GETCAR        BEGSR

     C                   OPEN      WMRPL
      * Get the Carrier information record
     C     RPKY          CHAIN     PRP01                              60

     C                   MOVEL     RPCAP#        XXCPL#

     C                   CLOSE     WMRPL

     C                   ENDSR
      *================================================================***
      * GETEMP - Subroutine to get employer information
      *================================================================***
     C     GETEMP        BEGSR

     C                   MOVE      *ZERO         XXPFDT
      * Get the Employer Master record
     C     EMKY          CHAIN     PEM01                              60

     C     *IN60         IFEQ      *OFF
      * Build up the address lines using mailing address data . . .
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL(P)  EMMAD1        XXEAD1
     C                   MOVEL     EMMST         XXEMST
     C                   MOVE      EMMZIP        XXZIP
     C     EMMCTY        CAT(P)    ',':0         XXCSZ            40
     C                   CAT       EMMST:1       XXCSZ
     C                   CAT       XXZIP1:1      XXCSZ

     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         XXCSZ
     C                   CAT       XXZIP2:0      XXCSZ
     C                   ENDIF

     C     EMMAD2        IFNE      *BLANK
     C                   MOVEL(P)  EMMAD2        XXEAD2
     C                   MOVEL(P)  XXCSZ         XXEAD3
     C                   ELSE
     C                   MOVEL(P)  XXCSZ         XXEAD2
     C                   MOVEL(P)  *BLANKS       XXEAD3
     C                   ENDIF

     C                   ELSE
      * . . .or build up the address lines using physical address data
     C                   MOVEL(P)  EMPAD1        XXEAD1
     C                   MOVEL     EMPST         XXEMST
     C                   MOVE      EMPZIP        XXZIP
     C     EMPCTY        CAT(P)    ',':0         XXCSZ
     C                   CAT       EMPST:1       XXCSZ
     C                   CAT       XXZIP1:1      XXCSZ

     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         XXCSZ
     C                   CAT       XXZIP2:0      XXCSZ
     C                   ENDIF

     C     EMPAD2        IFNE      *BLANK
     C                   MOVEL(P)  EMPAD2        XXEAD2
     C                   MOVEL(P)  XXCSZ         XXEAD3
     C                   ELSE
     C                   MOVEL(P)  XXCSZ         XXEAD2
     C                   MOVEL(P)  *BLANKS       XXEAD3
     C                   ENDIF

     C                   ENDIF

     C                   ENDIF
      * Get the Employer Detail record
     C     ELKY          CHAIN     PEL01                              60

     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C                   ENDIF

     C                   ENDSR
      *================================================================***
      * GETFND - Subroutine to get fund information
      *================================================================***
     C     GETFND        BEGSR

     C                   OPEN      WMFNL
     C                   OPEN      WDF2L
      * Get the Fund Master record
     C     FNKY          CHAIN     PFN01                              60

     C     FNPST         IFNE      *BLANK
     C                   MOVEL     FNPST         XXFNST
     C                   ELSE
     C                   MOVEL     FNMST         XXFNST
     C                   ENDIF
      * Get the Fund Auxiliary record
     C     F2KY          CHAIN     PF201                              60

     C                   MOVE      F2CAR#        XXCARR

     C                   CLOSE     WMFNL
     C                   CLOSE     WDF2L

     C                   ENDSR
      *================================================================***
      * GETFRM - Subroutine to get endorsement form table record
      *================================================================***
     C     GETFRM        BEGSR

      * Get the Endorsement Form Table record
     C     ENKY          CHAIN     PEN01                              60

     C     *IN60         DOWEQ     *OFF
     C     ENSTAT        ANDEQ     'I'
     C     ENKY          READE     PEN01                                  60
     C                   ENDDO

     C                   ENDSR
      *================================================================***
      * OTHINF - Subroutine to load form specific information
      *================================================================***
     C     OTHINF        BEGSR

      * Get PEO Information if PEO Group
     C     FNPEOF        IFNE      *BLANK
     C     LDENDA        ANDNE     *BLANKS
     C     LDEND#        ANDGT     0
     C                   MOVE      QQCO#         #5CO#
     C                   MOVE      QQFND         #5FND
     C                   MOVE      QQEMP#        #5EMP#
     C                   MOVE      QQDIV         #5DIV
     C                   MOVE      QQFYR         #5FYR

     C                   CALL      'SRADD1'
     C                   PARM                    #5CO#             3 0
     C                   PARM                    #5FND             3 0
     C                   PARM                    #5EMP#            9 0
     C                   PARM                    #5DIV             5 0
     C                   PARM                    #5FYR             3 0
     C     XXNAME        PARM                    #5NAME          100
     C     XXFNAM        PARM                    #5FNAM          100
     C                   PARM                    #5EAD1           40
     C                   PARM                    #5EAD2           40
     C                   PARM                    #5ECSZ           40
     C                   PARM                    #5CTY            25
     C                   PARM                    #5ST              2
     C                   PARM                    #5ZIP             9 0
     C                   PARM                    #5FEIN            9 0
     C                   PARM                    #5EADR           90

     C                   ENDIF
      /Free
          If ldEnda <> *Blanks and ldenda <> *Zeros;
      /End-Free
     C                   EXSR      INFO1

     C                   ELSE
      *  ELSE - called from W1054R
     C                   EXSR      INFO2
     C                   ENDIF


     C                   ENDSR

      *================================================================***
      * INFO1 - Get OTHINF MID-TERM Endorsement records
      *================================================================***
     C     INFO1         BEGSR
     C                   MOVE      'N'           XXWRT             1
     C                   MOVE      *BLANK        XXFRST            1

      * Look for waivers.
     C     WVKY7         CHAIN     PWV07                              60

     C     *IN60         DOWEQ     *OFF

     C     WVSTAT        IFNE      'I'
     c                   exsr      srproc
     c                   if        xxproc = 'Y'
     C                   MOVEL     NAME          SNAME
     C                   MOVEL     SNAM1         FM1F01
     C                   MOVEL(p)  SNAM2         FM1F02
     C                   MOVEL(p)  WVNAME        FMNM01
     c                   eval      fm2001 = fm1f01
     c                   eval      fm2002 = fm1f02
     C                   MOVEL     WVADR1        FMC401

     C     WVADR2        IFNE      *BLANK
     C                   MOVEL     WVADR2        FMC402
     C                   ENDIF

     c                   if        wvzip <> *zeros
     C                   MOVE      WVZIP         XXZIP
     C     WVCTY         CAT       ',':0         FMC403
     C                   CAT       WVST:1        FMC403
     C                   CAT       XXZIP1:1      FMC403

     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         FMC403
     C                   CAT       XXZIP2:0      FMC403
     C                   ENDIF
     C                   endif

     C     FMC402        IFEQ      *BLANKS
     C                   MOVEL(P)  FMC403        FMC402
     C                   MOVEL(P)  *BLANKS       FMC403
     C                   ENDIF
      * Load the project lines.
     C     WVPROJ        IFNE      *BLANKS
     C                   MOVEL     WVPROJ        FMC601
     C                   ENDIF

     C     WVPRJ2        IFNE      *BLANKS
     C                   MOVEL     WVPRJ2        FMC602
     C                   ENDIF

      * Load Flat Charge per waiver amount
      **   WVFCHG        IFNE      *ZEROS
      **                 Z-Add     WVFCHG        FMA921
      **                 ENDIF

      * Load waiver amount for ALL divisions for CA
     C                   EXSR      GETWAV

     C     xxwaive       IFNE      *ZEROS
     C                   Z-Add     xxwaive       FMAM11
     C                   ENDIF

      *  If printing a policy page.  (Not called from W1054R)
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W1054R'      #1PGM            10
     C                   PARM      *BLANK        #1FLAG            1
      * Write the record.
      /Free
          If #1Flag = '0';
             PrintingPolicy = *On;
          Else;
             PrintingPolicy = *Off;
          Endif;
          Exsr WriteWorkFile;
      /End-Free
     C                   MOVE      'Y'           XXWRT             1

     C                   MOVE      *BLANKS       FM1F01
     C                   MOVE      *blanks       FM1F02
     C                   MOVE      *BLANKS       FMNM01
     c                   move      *blanks       fm2001
     c                   move      *blanks       fm2002
     C                   MOVE      *BLANKS       FMC401
     C                   MOVE      *BLANKS       FMC402
     C                   MOVE      *BLANKS       FMC403
     C                   MOVE      *BLANKS       FMC601
     C                   MOVE      *BLANKS       FMC602
     C                   MOVE      *BLANKS       FMAM21


     C                   ENDIF
     C                   ENDIF

     C     WVKY7         READE     PWV07                                  60
     C                   ENDDO

     C     XXWRT         IFEQ      'Y'
     C                   WRITE     PFM03
     C                   ENDIF

     C                   ENDSR
      *================================================================***
      * INFO2 - Get OTHINF Endorsement records if NOT Mid-Term processing
      *================================================================***
     C     INFO2         BEGSR

     C                   MOVE      'N'           XXWRT             1
     C                   MOVE      *BLANK        XXFRST            1
      * Look for waivers.
     C     WVKY6         CHAIN     PWV06                              60

     C     *IN60         DOWEQ     *OFF

     C     WVSTAT        IFNE      'I'
     c                   exsr      srproc
     c                   if        xxproc = 'Y'
     C                   MOVEL     NAME          SNAME
     C                   MOVEL     SNAM1         FM1F01
     C                   MOVEL(p)  SNAM2         FM1F02
     C                   MOVEL(p)  WVNAME        FMNM01
     c                   eval      fm2001 = fm1f01
     c                   eval      fm2002 = fm1f02
     C                   MOVEL     WVADR1        FMC401

     C     WVADR2        IFNE      *BLANK
     C                   MOVEL     WVADR2        FMC402
     C                   ENDIF

     c                   if        wvzip <> *zeros
     C                   MOVE      WVZIP         XXZIP
     C     WVCTY         CAT       ',':0         FMC403
     C                   CAT       WVST:1        FMC403
     C                   CAT       XXZIP1:1      FMC403

     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         FMC403
     C                   CAT       XXZIP2:0      FMC403
     C                   ENDIF
     c                   endif

     C     FMC402        IFEQ      *BLANKS
     C                   MOVEL(P)  FMC403        FMC402
     C                   MOVEL(P)  *BLANKS       FMC403
     C                   ENDIF
      * Load the project lines.
     C     WVPROJ        IFNE      *BLANKS
     C                   MOVEL     WVPROJ        FMC601
     C                   ENDIF

     C     WVPRJ2        IFNE      *BLANKS
     C                   MOVEL     WVPRJ2        FMC602
     C                   ENDIF

      * Load Flat Charge per waiver amount
      **   WVFCHG        IFNE      *ZEROS
      **                 Z-Add     WVFCHG        FMA921
      **                 ENDIF

      * Load waiver amount for ALL divisions for CA
     C                   EXSR      GETWAV

     C     xxwaive       IFNE      *ZEROS
     C                   Z-Add     xxwaive       FMAM11                         field is 11.2
     C                   ENDIF


      *  If printing a policy page.  (Not called from W1054R)
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W1054R'      #1PGM            10
     C                   PARM      *BLANK        #1FLAG            1
      * Write the record.
      /Free
          If #1Flag = '0';
             PrintingPolicy = *On;
          Else;
             PrintingPolicy = *Off;
          Endif;
          Exsr WriteWorkFile;
      /End-Free
     C                   MOVE      'Y'           XXWRT             1
      * Update the endorsement reporting file with the proper data.
     C                   EXSR      ENDRPT

     C                   MOVE      *BLANKS       FM1F01
     C                   MOVE      *BLANKS       FM1F02
     C                   MOVE      *BLANKS       FMNM01
     C                   MOVE      *BLANKS       FM2001
     C                   MOVE      *BLANKS       FM2002
     C                   MOVE      *BLANKS       FMC401
     C                   MOVE      *BLANKS       FMC402
     C                   MOVE      *BLANKS       FMC403
     C                   MOVE      *BLANKS       FMC601
     C                   MOVE      *BLANKS       FMC602
     C                   MOVE      *BLANKS       FMAM21
     C                   ENDIF
     C                   ENDIF

     C     WVKY6         READE     PWV06                                  60
     C                   ENDDO

     C     XXWRT         IFEQ      'Y'
     C                   WRITE     PFM03
     C                   ENDIF

     C                   ENDSR
      *================================================================***
      * GETWAV - Subroutine to get waive amount from all divisions for CA
      *================================================================***
     C     GETWAV        BEGSR
      * Get coverage state of state reporting division
      *
     C                   eval      xxState = RatingSt(XXCO#:
     C                                       (GetRateSet(
     C                                        GetEmpYrRR(XXFYR:XXEMP#:
     C                                        GetStRptDv(XXCO#:XXFND:
     C                                        XXEMP#):XXFND:XXCO#))))
      *
      ******             eval      sysDt7 = DateTo7(UDATE)
     c                   eval      Date8  = %date()
     c     *cymd         move      Date8         sysDt7

     C                   eval      midTermEnd = #1Flag

     c                   z-add     0             xxwaive
     c                   open      wdell1
     c     elky2         setll     pell1
     c     elky2         reade     pell1

     c                   dow       not %eof

     C                   eval      cxDate7 =
     C                              GetCxDate(ELCO#:ELFND:ELEMP#:ELDIV)

     C                   eval      useDiv = cxDate7 = *zero
     C                             or cxDate7 > sysDt7
     C                             or midTermEnd and cxDate7 > endEffDt7

     C                   if        useDiv  and
     c                             xxstate = 'CA'
     c                   eval      xxwaive = xxwaive + elewav
     c                   endif

     c     elky2         reade     pell1
     c                   enddo

     c                   close     wdell1

     C                   ENDSR
      *================================================================***
      * ENDRPT - Update Endorsement reporting file
      *================================================================***
     C     ENDRPT        BEGSR
      *  If printing a policy page.  (Not called from W1054R)
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W1054R'      #1PGM            10
     C                   PARM      *BLANK        #1FLAG            1
     C     #1FLAG        IFEQ      '0'
      * Find the unique ID number.
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             XXUNID
     C     SHKEY         CHAIN     PSH01                              60

     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     SHUNID        XXUNID
     C                   ENDIF

     C                   MOVEL(P)  XXFORM        X1END#
     C     PEKEY2        CHAIN     PPE01                              60
      * Copy data into the record.
     C     *IN60         IFEQ      *OFF
     C                   CLEAR                   E00313
     C                   MOVEL     WVNAME        XXNOP
     C                   MOVEL     WVPROJ        XXNOO
     C                   MOVEL(P)  E00313        PEDATA

     C     XXFRST        IFEQ      *BLANK
     C                   UPDATE    PPE01
     C                   MOVE      'Y'           XXFRST
     C                   ELSE
     C                   WRITE     PPE01
     C                   ENDIF

     C                   ENDIF
     C                   ENDIF

     C                   ENDSR
      *================================================================***
      * INCREC - Subroutine to write a merged forms data record
      *================================================================***
     C     INCREC        BEGSR

     C                   OPEN      WXICP

     C     '.inc(WC0'    CAT(P)    '00313,':0    ICSTMT
     C                   CAT       'AMTEMP':0    ICSTMT
     C                   CAT       ',)':0        ICSTMT

     C                   WRITE     PIC01

     C                   CLOSE     WXICP

     C                   ENDSR

      /Copy SCOPYMT02C
