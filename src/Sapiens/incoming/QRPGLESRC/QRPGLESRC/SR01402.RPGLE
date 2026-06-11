     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     *===============================================================*
     H/TITLE SR01402 - AL - Anniversary Rating Date Endorsement
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H*   THIS PROGRAM IS CALLED TO:                                  *
     H*     1) DETERMINE IF THIS ENDORSEMENT FORM SHOULD BE ATTACHED  *
     H*        TO THE POLICY.                                         *
     H*     2) CREATE DATA IN THE ENDORSEMENT FORM WORKFILE (WXFMP)   *
     H*        PRIOR TO A FORM MERGE.                                 *
     H*                                                               *
     H* PROGRAMMER: C:113                                             *
     H* CREATED:    02/03/99            PROJECT: CM027                *
     H*                                                               *
     H*****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 11/07/17 C:267 Project: SY371                                 *
      *                Recompile for copybook for client path.        *
      *                                                               *
      * 11/10/16 C:136 Project: SY371                                 *
      *                Add field for extended description             *
      *                                                               *
     * 09/29/16 C:33  Project: SY371                                 *
      *               -Implement Scriptura.
      *               -No comments.
      *
      * 07/14/11 C:061 Support 77336
      *              - Convert source to RPGIV.
      *              - Do not attach for flat cancelled divisions.
      *                                                               *
      * 09/11/06 C:61  SUPPORT: WA132 54290 132WEW
      *                - Call SRPAMT to get premium amount.
     H*                                                               *
     H* ------------------------------------------------------------- *
     H*  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     H* ------------------------------------------------------------- *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
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
     FWMFNL     IF   E           K DISK    USROPN
     FWMMUP     O    E             DISK    USROPN
     FWMRPL     IF   E           K DISK    USROPN
     FWMRSL     IF   E           K DISK
     FWTENL     IF   E           K DISK
     FWXFMP     O    E             DISK    USROPN
     FXMLPREL1  UF   E           K DISK
     FXMLTEML1  UF   E           K DISK
     fWXWXFMP   o    e             disk    UsrOpn

     D formName        s             15A
     D*
     D XXFORM          C                   CONST('WC010402  ')
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

     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     d ldaXml                191    250
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D isPrtOnly       s               n   inz(*off)
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
     D*

     D UseScript       s              1n
     D UsePrintOut     s              1n
     D UseXML          S              1n
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
     D/copy sPrJobInfo

     d SendToScriptura...
     d                 pr                  ExtPgm('S4401C')
     d  outXMLString                 61

      *entry parameters
     D SR01402         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR01402         PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
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

     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA

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
     C                   MOVEL     XXFNST        FMFNST
     C                   MOVEL     FNNAME        FMFNNM
     C                   Z-ADD     XXDIV         SVDIV
     C                   Z-ADD     0             XXENMP
      *
     C                   CALL      'SRPAMT'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     C                   PARM                    XXEMP#
     C                   PARM                    XXENMP
     C                   PARM      *BLANK        XXCLR             1
      *
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
     *
     C                   Z-ADD     ELANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FMDT01
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
     C*
     C*  Validate attachment of form
     C*
     C     ELKY2         CHAIN     PEL02                              60
     C*
     C*  Check each division for a rating state of 'AL'
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C     RSJRST        ANDEQ     ENST
     C*
     C*  Anniversary date must be entered
     C*
     C     ELANDT        IFNE      *ZERO
     C     elpfdt        andne     elptdt
     C*
     C                   MOVE      *ON           CRTFRM                         Found one
     C                   LEAVE                                                  Leave the loop
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDIF                                                  ELANDT <> 0
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
      *** beg add ***** 11/10/16 **************************
     C                   MOVEL     ENdsc2        MUdsc2
      *** end add ***** 11/10/16 **************************
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
     C*  Write the output record
     C*
      /Free
       isPrtOnly = PgmInStack('W1054R');  //conditions elec rpt
       printingPolicy = not(isPrtOnly);   //conditions Scriptura
       Exsr WriteWorkFile;
      /End-Free
     C*
     C                   ENDSR                                                  OTHINF
      /Copy SCOPYEN02C
