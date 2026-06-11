     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('WTBNDDIR')
     H/TITLE SR35404 -LOAD ENDORSEMENT FORM WORKFILE FOR FORM WC350404
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H*   THIS PROGRAM IS CALLED TO:                                  *
     H*     1) DETERMINE IF THIS ENDORSEMENT FORM SHOULD BE ATTACHED  *
     H*        TO THE POLICY.                                         *
     H*                                                               *
     H*     2) CREATE DATA IN THE ENDORSEMENT FORM WORKFILE (WXFMP)   *
     H*        PRIOR TO A FORM MERGE.                                 *
     H*                                                               *
     H* PROGRAMMER: C:113                                             *
     H* CREATED:    03/12/99            PROJECT: 48WPB                *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
      *
      * 11/09/17 C:267 Project: SY371                                 *
      *                Recompile for copybook for client path.        *
      *                                                               *
      * 01/03/17 C:180 Project: SY371 Scriptura.                      *
      *                Add scriptura processing.                      *
      *
      * 07/14/11 C:061 Support 77336
      *                Do not attach for flat cancelled divisions.
      *
      * 12/16/10 C:61  PROJECT SY190 PRISS 207
      *                Populate new "Insured Name" and "Insured Federal
      *                Name" fields.
      *                                                               *
      * 09/11/06 C:61  SUPPORT: WA132 54290 132WEW
      *                - Call SRPAMT to get premium amount.
      *                                                               *
      * 02/25/05 C:61  PROJECT: wc1250                                *
      *                Include Domestic Terrorism in premium amount.  *
     H*                                                               *
     H* 05/22/03 C:61  PROJECT WC1120                                 *
     H*                Add Terrorism Risk Act Surcharge to premium.   *
     H*                                                               *
     H*  10/16/01 C:62 PROJECT: CM027                                 *
     H*                Load the endorsement date instead of the       *
     H*                policy effective date in the endorsement       *
     H*                effective date.                                *
     H*                                                               *
     H* XX/XX/XX  C:XXX  PROJECT: XXXXX                               *
     H*                                                               *
     H* ------------------------------------------------------------- *
     H*  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     H* ------------------------------------------------------------- *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  General CHAIN (READ)                                      *
     H*                                                               *
     H*****************************************************************
     H* SUBROUTINE SUMMARY:                                           *
     H*                                                               *
     H* BASINF - Collect the basic info for WXFMP.                    *
     H* BULDIT - Gather information to merge to the form.             *
     H* CHEKIT - Determine whether or not to attach the form.         *
     H* GETAGT - Get the agent information.                           *
     H* GETCAR - Get the carrier information.                         *
     H* GETEMP - Get the employer information.                        *
     H* GETFND - Get the fund information.                            *
     H* GETFRM - Get the endorsement form information.                *
     H* OTHINF - Get the form-specific information.                   *
     H*                                                               *
     H*****************************************************************
     H* NOTES:                                                        *
     H*                                                               *
     H* QQMODE = 1 - Called to determine attacment of endorsement.    *
     H* QQMODE = 2 - Called to print endorsement.                     *
     H* QQMODE = 3 - Called for additional input.                     *
     H* QQMODE = 4 - Called from mid-term processing.                 *
     H*                                                               *
     H*****************************************************************
     FWDELL     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWDF2L     IF   E           K DISK    USROPN
     FWMAAL1    IF   E           K DISK    USROPN
     FWMAGL     IF   E           K DISK    USROPN
     FWMAHL     IF   E           K DISK    USROPN
     FWMEML     IF   E           K DISK
     FWMEPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK    USROPN
     FWMMUP     O    E             DISK    USROPN
     F*** BEGIN ADD *** 10/16/01 **************************************
     FWMMUL     IF   E           K DISK
     F                                     RENAME(PMU01:PMU02)
     F*** END   ADD *** 10/16/01 **************************************
     FWMPRL1    IF   E           K DISK
     FWMRPL     IF   E           K DISK    USROPN
     FWMRSL     IF   E           K DISK
     FWTENL     IF   E           K DISK
     FWXFMP     O    E             DISK    USROPN
     fXMLPREL1  UF   E           K DISK
     fXMLTEML1  UF   E           K DISK
     fWXWXFMP   o    e             disk    UsrOpn

     d svLda           ds          1024    dtaara(*lda)
     d ldaXml                191    250

     d #1PGM           s             10A   INZ('W1054R')
     d #1FLAG          s              1A   INZ
     d formName        s             15A

     D isUseSpoolOut   s              1n   inz
     D isScriptUsed    s              1A   inz('N')

     D isUseXML        S              1n   inz
     D InXML           s             60
     D InXML@          s             61
     D PrintingPolicy  S               N
     D outCall         s             11
     D OutString       s             10
     D InReturn        s            250
     D OutMode         s              1
     D OutRrn          s              9
     D OutPrefix       s              2
     D RunningSRText   S               N   Inz(*Off)

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

     dSendToScriptura  pr                  ExtPgm('S4401C')
     d  outXMLString                 61

     D* Check Pgm
     D SRCHKPGM        pr                  ExtPgm('SRCHKPGM')
     D                               10
     D                                1

     D/copy sPrModule
     D/copy sPrXML
     D*
     D XXFORM          C                   CONST('WC350404  ')
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
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  Fund Master file
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  Employer Master file
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  Employer Detail file
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C*  Agent History File
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  Agent Assignment file
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C*
     C*  Agent Master file
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*  Endorsement Form Table file
     C*
     C     ENKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XKFORM
     C*
     C*  Fund Auxiliary file
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  Carrier file
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  Rate Set Code File
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C*
     C*  Employer Payroll Master file
     C*
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  Payroll Master file
     C*
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
     C                   KFLD                    ELPFDT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
     C*** BEGIN ADD *** 10/16/01 **************************************
     C*
     C* Employer Endorsement Forms File Key
     C*
     C     MUKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XKFORM
     C*** END   ADD *** 10/16/01 **************************************
     C*================================================================
     C*  *LIKE DEFN definitions
     C*================================================================
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
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C     *LIKE         DEFINE    ELENMP        XXENMP
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C*  Date check/conversion PLISTs
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
     C*  *ENTRY parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQMODE            1
     C*================================================================***
     C*  Main
     C*================================================================***
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C*
     C                   MOVEL     XXFORM        XKFORM
      /Free
          formName = %subst(xxForm :1 :2) + ' ' + %subst(xxForm :3 :2) + ' ' +
           %subst(xxForm :5 :2) + ' ' + %subst(xxForm :7 :2) + ' ' +
           %subst(xxForm :9 :2);
          Exsr Scriptura;
      /End-Free
     C*
     C                   SELECT
     C*
     C*  Determine whether or not to attach this form to the policy
     C*
     C     QQMODE        WHENEQ    '1'
     C                   EXSR      CHEKIT
     C*
     C*  Gather info to merge to the form
     C*
     C     QQMODE        WHENEQ    '2'
     C                   EXSR      BULDIT
     C*
     C                   ENDSL
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*================================================================***
     C*  BASINF - Subroutine to load basic WXFMP fields
     C*================================================================***
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
     C*** BEGIN CHG *** 10/16/01 **************************************
     C**********           Z-ADDELPFDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDYY
     C**********           MOVELSRDTO     FMEEDT
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
     C*** END   CHG *** 10/16/01 **************************************
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
     C                   Z-ADD     XXDIV         SVDIV
     C                   Z-ADD     0             XXENMP
     C*
      ***** BEG DEL ******* 09/11/06 **********************************
     C********   ELKY2     CHAINPEL02                61
     C********
     C********   *IN61     DOWEQ*OFF
     C********             ADD  ELENMP    XXENMP
      ******BEG ADD ******* 05/22/03 **********************************
     C********             ADD  ELETER    XXENMP
      ******END ADD ******* 05/22/03 **********************************
      ******beg ADD ****  02/25/05 ************************************
     C********             ADD  ELEDTR    XXENMP
      ******end ADD ****  02/25/05 ************************************
     C********   ELKY2     READEPEL02                    61
     C********             ENDDO                           *IN61=*OFF
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
     C*
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
     C                   ENDSR                                                  BASINF
     C*================================================================***
     C*  BULDIT - Subroutine to gather info to merge to the form
     C*================================================================***
     C     BULDIT        BEGSR
     C*
     C*  Open the Endorsement Form workfile
     C*
     C                   OPEN      WXFMP
     C*
     C*  Get the basic info
     C*
     C                   EXSR      GETFND
     C                   EXSR      GETEMP
     C                   EXSR      GETAGT
     C                   EXSR      GETFRM
     C                   EXSR      GETCAR
     C*
     C*  Clear out the record
     C*
     C                   CLEAR                   PFM01
     C*
     C*  Load basic WXFMP fields
     C*
     C                   EXSR      BASINF
     C*
     C*  Load form specific info
     C*
     C                   EXSR      OTHINF
     C*
     C*  Close the Endorsement Form workfile
     C*
     C                   CLOSE     WXFMP
     C*
     C                   ENDSR                                                  BULDIT
     C*================================================================***
     C*  CHEKIT - Subroutine to determine whether or not to attach form
     C*================================================================***
     C     CHEKIT        BEGSR
     C*
     C                   MOVE      *OFF          CRTFRM            1
     C*
     C*  Get the employer and form info
     C*
     C                   EXSR      GETEMP
     C                   EXSR      GETFRM
     C*****************************************************************
     C*
     C*  Validate attachment of form
     C*
     C*****************************************************************
     C*
     C     ELKY2         CHAIN     PEL02                              60
     C*
     C*  Check each division for a matching jurisdiction state
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     RSKY          CHAIN     PRS01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     RSJRST        ANDEQ     ENST
     C*
     C*  Contracting class credit must be zero
     C*
     C     ELASP1        IFEQ      *ZERO
      ******* beg add ****** 07/14/11 *********************************
     C     elpfdt        andne     elptdt
      ******* end add ****** 07/14/11 *********************************
     C*
     C*  Look for payroll records that have a contracting classification
     C*
     C     EPKY          SETLL     PEP01
     C     EPKY          READE     PEP01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EPSTAT        IFNE      'I'
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     PRCCFL        ANDEQ     'Y'
     C                   MOVE      *ON           CRTFRM
     C                   LEAVE
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDIF                                                  EPSTAT <> 'I'
     C*
     C     EPKY          READE     PEP01                                  60
     C                   ENDDO
     C*
     C     CRTFRM        IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELASP1 = *ZE
     C*
     C                   ENDIF                                                  *IN60 = *OFFRO
     C*
     C     ELKY2         READE     PEL02                                  60
     C*
     C                   ENDDO                                                  *IN60 = *OFF
     C*****************************************************************
     C*  End validation
     C*****************************************************************
     C     CRTFRM        IFEQ      *ON
     C*
     C*  Reset the Employer Detail and Rate Set Code Desc files
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     RSKY          CHAIN     PRS01                              60
     C*
     C*
     C*  Open the Employer Attached Endorsement Forms file
     C*
     C                   OPEN      WMMUP
     C*
     C*  Clear out the record
     C*
     C                   CLEAR                   PMU01
     C*
     C*  Load the record
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
     C                   MOVEL     ENDSC2        MUDSC2
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
     C*  Write a record to the Employer Attached Endorsement Forms file
     C*
     C                   WRITE     PMU01
     C*
     C*  Close the Employer Attached Endorsement Forms file
     C*
     C                   CLOSE     WMMUP
     C*
     C                   ENDIF                                                  CRTFRM = *ON
     C*
     C                   ENDSR                                                  CHEKIT
     C*================================================================***
     C*  GETAGT - Subroutine to get agent information
     C*================================================================***
     C     GETAGT        BEGSR
     C*
     C                   OPEN      WMAHL
     C                   OPEN      WMAAL1
     C                   OPEN      WMAGL
     C*
     C                   Z-ADD     *ZERO         XXAGT#
     C*
     C*  Get the Current Agent History record
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
     C                   ENDSR                                                  GETAGT
     C*================================================================***
     C*  GETCAR - Subroutine to get carrier information
     C*================================================================***
     C     GETCAR        BEGSR
     C*
     C                   OPEN      WMRPL
     C*
     C*  Get the Carrier Info record
     C*
     C     RPKY          CHAIN     PRP01                              60
     C*
     C                   MOVEL     RPCAP#        XXCPL#
     C*
     C                   CLOSE     WMRPL
     C*
     C                   ENDSR                                                  GETCAR
     C*================================================================***
     C*  GETEMP - Subroutine to get employer information
     C*================================================================***
     C     GETEMP        BEGSR
     C*
     C                   MOVE      *ZERO         XXPFDT
     C*
     C*  Get the Employer Master record
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C*  Build up address lines using mailing address data
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
     C*  Build up address lines using physical address data
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
     C*  Get the Employer Detail record
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C                   ENDIF
     C*
     C                   ENDSR                                                  GETEMP
     C*================================================================***
     C*  GETFND - Subroutine to get fund information
     C*================================================================***
     C     GETFND        BEGSR
     C*
     C                   OPEN      WMFNL
     C                   OPEN      WDF2L
     C*
     C*  Get the Fund Master record
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     FNPST         IFNE      *BLANK
     C                   MOVEL     FNPST         XXFNST
     C                   ELSE
     C                   MOVEL     FNMST         XXFNST
     C                   ENDIF
     C*
     C*  Get the Fund Auxiliary record
     C*
     C     F2KY          CHAIN     PF201                              60
     C*
     C                   MOVE      F2CAR#        XXCARR
     C*
     C                   CLOSE     WMFNL
     C                   CLOSE     WDF2L
     C*
     C                   ENDSR                                                  GETFND
     C*================================================================***
     C*  GETFRM - Subroutine to get endorsement form table record
     C*================================================================***
     C     GETFRM        BEGSR
     C*
     C*  Get the Endorsement Form Table record
     C*
     C     ENKY          CHAIN     PEN01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ENSTAT        ANDEQ     'I'
     C     ENKY          READE     PEN01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C*
     C                   ENDSR                                                  GETFRM
     C*================================================================***
     C*  OTHINF - Subroutine to load form specific information
     C*================================================================***
     C     OTHINF        BEGSR
     C*
     C*  WRITE THE OUTPUT RECORD
     C*
      /Free
       SRCHKPGM(#1PGM:#1FLAG);
       If #1Flag = '0';
          PrintingPolicy = *On;
       Else;
          PrintingPolicy = *Off   ;
       Endif;

       If isScriptUsed = 'Y';
           RunningSRtext = *ON;
       Endif;
       Exsr WriteWorkfile;
      /End-Free
     C*
     C                   ENDSR                                                  OTHINF
      /copy SCOPYEN02C
