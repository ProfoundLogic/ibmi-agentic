     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H bnddir('WTBNDDIR')
     H/TITLE SR00000B - LOAD ENDORSEMENT FORM WORKFILE FOR FORM WC000000B
     H*****************************************************************
     H*    PROGRAM DESCRIPTION:                                       *
     H*                                                               *
     H*       THIS PROGRAM IS CALLED TO:                              *
     H*         1) DETERMINE IF THIS ENDORSEMENT FORM SHOULD BE       *
     H*            ATTACHED TO THE POLICY.                            *
     H*         2) CREATE DATA INTO THE ENDORSEMENT FORM WORKFILE     *
     H*            (WXFMP) PRIOR TO A FORM MERGE.                     *
     H*                                                               *
     H* PROGRAMMER: C:220                                             *
     H* CREATED:    08/05/10            PROJECT: WC1920               *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
      * REVISIONS:                                                    *
      *
      * 12/09/16 C:136 Project: SY371                                 *
      *                Add field for extended description             *
      *                                                               *
      *****************************************************************
     F*
     FWMFNL     IF   E           K DISK    USROPN
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMAHL     IF   E           K DISK    USROPN
     FWMAAL1    IF   E           K DISK    USROPN
     FWMAGL     IF   E           K DISK    USROPN
     FWTENL     IF   E           K DISK
     FWMMUP     O    E             DISK    USROPN
     FWMMUL     IF   E           K DISK
     F                                     RENAME(PMU01:PMU02)
     FWDF2L     IF   E           K DISK    USROPN
     FWMRPL     IF   E           K DISK    USROPN
     FWXFMP     O    E             DISK    USROPN
      *** beg add ***** 12/09/16 ******************
     fXMLPREL1  UF   E           K DISK
     fXMLTEML1  UF   E           K DISK
     fWXWXFMP   o    e             disk    UsrOpn

     d formName        s             15A

     d svLda           ds          1024    dtaara(*lda)
     d ldaXml                191    250

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
     D UseScript       s              1n
     D UsePrintOut     s              1n
     D UseXML          s              1n
     D #1pgm           s             10A   INZ('W1054R')
     D #1flag          s              1A   INZ

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

     d SendToScriptura...
     d                 pr                  ExtPgm('S4401C')
     d  outXMLString                 61

     d PrtPgm          pr                  extpgm('WC000000B')
     d  qqco#                         3
     d  qqfnd                         3
     d  qqemp#                        9
     d  qqdiv                         5
     d  qqfyr                         3
     d  qqmode                        1

     d* Check Pgm
     d SrChkPgm        pr                  ExtPgm('SRCHKPGM')
     d                               10
     d                                1
      *** end add ***** 12/09/16 ******************
     D*
     D XXFORM          C                   CONST('WC000000B ')
      *** beg del ***** 12/09/16 ******************
     D*PRTPGM****      C                   CONST('WC000000B ')
      *** end del ***** 12/09/16 ******************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C****************************************************************
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
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
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
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C*  ENDORSEMENT FORM TABLE FILE
     C*
     C     ENKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XKFORM
     C*
     C*  FUND MASTER KLIST
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
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
     C* Employer Endorsement Forms File Key
     C*
     C     MUKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XKFORM
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ENFORM        XKFORM
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C     *LIKE         DEFINE    FMEMST        XXEMST
     C     *LIKE         DEFINE    FMEAD1        XXEAD1
     C     *LIKE         DEFINE    FMEAD2        XXEAD2
     C     *LIKE         DEFINE    FMEAD3        XXEAD3
     C     *LIKE         DEFINE    FNPST         XXFNST
     C     *LIKE         DEFINE    FMCARR        XXCARR
     C     *LIKE         DEFINE    FMCPL#        XXCPL#
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C     *LIKE         DEFINE    ELENMP        XXENMP
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
     C     PRTPL         PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQMODE            1
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

      *** beg add ***** 12/09/16 **********************
      /Free
       formName = %subst(xxForm :1 :2) + ' ' + %subst(xxForm :3 :2) + ' ' +
        %subst(xxForm :5 :2) + ' ' + %subst(xxForm :7 :2) + ' ' +
        %subst(xxForm :9 :2);
       Exsr Scriptura;
      /End-Free
      *** end add ***** 12/09/16 **********************
     C*
     C     *DTAARA       DEFINE    W7413PDF      XXPDF             1
     C*
     C                   IN        XXPDF
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
     C                   MOVE      *ON           CRTFRM            1
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
     C*
     C     CRTFRM        IFEQ      *ON
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
      *** beg add ***** 12/09/16 **************************
     C                   MOVEL     ENDSC2        MUDSC2
      *** end add ***** 12/09/16 **************************
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
     C*
     C     QQFND         CAT       QQEMP#:0      FMPOL#
     C                   CAT       QQFYR:0       FMPOL#
     C*
     C                   MOVEL     EMTPOB        FMTOBC
     C                   MOVEL     EMNAME        FMEMNM
     C                   MOVEL     EMFDNM        FMFENM
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
     C*  WRITE THE OUTPUT RECORD
     C*
      *** beg chg ***** 12/09/16 ***************************
     C***********        SELECT
     C*****XXPDF*        WHENEQ    'Y'
     C***********        CALL      PRTPGM        PRTPL
     C***********        MOVE      'P'           QQMODE
     C***********
     C***********        OTHER
     C***********        WRITE     PFM01
     C***********        ENDSL
     C***********        ENDSR

      *  If printing a policy page.  (Not called from W1054R)
      /Free
       SrChkPgm(#1pgm:#1flag);
       If #1Flag = '0';
          PrintingPolicy = *On;
       Else;
          PrintingPolicy = *Off;
       Endif;

       Exsr WriteWorkFile;

       Endsr;

       Begsr Scriptura;
       // Determine if this module get spool file and/or XML output

          isUseSpoolOut = UseSpoolOut('Policy Page');
          isUseXML = UseScriptura('Policy Page') Or
             Use3rdXml('Policy Page');
          If isUseXML;
             isScriptUsed = 'Y';
             RunningSrText = *on;
          Else;
             isScriptUsed = 'N';
          Endif;

       Endsr;


       Begsr WriteWorkFile;

          Monitor;

             If isScriptUsed = 'N' or isUseSpoolOut = *On;
                If xxPdf = 'Y';
                   PrtPgm (qqco#:qqfnd:qqemp#:qqdiv:qqfyr:qqmode);
                   qqMode = 'P';
                Else;
                   Write pfm01;
                Endif;
             EndIf;

             If isScriptUsed = 'Y';
                Exsr GetXMLFileNm;

                Open WXWXFMP;
                FMxml = inXML;
                FMxmlT = XPxmlT;
                FMFORMF = formName;
                write FMRecord;
                Close WXWXFMP;

                If Not PrintingPolicy;
                   inXML@ = inXML + '$';
                   SendToScriptura(inXML@);
                EndIf;

             Endif;

          On-error;
          Endmon;

       Endsr;

       Begsr GetXMLFileNm;

          If PrintingPolicy;
             in *dtaara;
             outCall = 'TEMPLRQST';
             inXML = ldaXML;
             OutPrefix = '  ';
             OutString = xxForm;
             InReturn ='Policy Page';
             outMode = ' ';
             StLetter(OutCall:InXML:InReturn:outMode:OutRrn:
             OutPrefix:OutString:'000':'000');

             Chain inXML XMLTEML1;
             If %found(XMLTEML1);
                XTdcid = %Trim(xxForm) + '.SCD';
                XPXMLT = XTXMLT;
                If RunningSRText;
                   XTtype = 'Text Only Endorsement';
                Else;
                   XTtype = %SubSt(xxForm:1:8);
                Endif;
                Update XT100;
             EndIf;

          Else;
             outCall = 'XMLREQUEST';
             OutPrefix = '  ';
             OutString = xxForm;
             InReturn ='Policy Page';
             outMode = ' ';
             StLetter(OutCall:InXML:InReturn:outMode:OutRrn:
             OutPrefix:OutString:'000':'000');

             Chain inXML XMLPREL1;
             If %found(XMLPREL1);
                XPdata = *blanks;
                XPdcid = %Trim(xxForm) + '.SCD';
                If RunningSRText;
                   XPtype = 'Text Only Endorsement';
                Else;
                   XPtype = %SubSt(xxForm:1:8);
                Endif;
                Update XP100;
             Endif;
          EndIf;

       Endsr;

      /End-free
      *** end chg ***** 12/09/16 ***************************
