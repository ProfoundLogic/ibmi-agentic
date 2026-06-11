     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR00402 - Anniversary Rating Date Endorsement
      //****************************************************************
      //    PROGRAM DESCRIPTION:                                       *
      //                                                               *
      //       THIS PROGRAM IS CALLED TO:                              *
      //         1) DETERMINE IF THIS ENDORSEMENT FORM SHOULD BE       *
      //            ATTACHED TO THE POLICY.                            *
      //         2) CREATE DATA INTO THE ENDORSEMENT FORM WORKFILE     *
      //            (WXFMP) PRIOR TO A FORM MERGE.                     *
      //                                                               *
      //****************************************************************
      // REVISIONS:                                                    *
      //                                                               *
      // 11/07/17 C:267 Project: SY371                                 *
      //                Recompile for copybook for client path.        *
      //                                                               *
      // 11/10/16 C:136 Project: SY371                                 *
      //                Add field for extended description             *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
      //****************************************************************

     FWMFNL     IF   E           K DISK    USROPN
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMAHL     IF   E           K DISK    USROPN
     FWMAAL1    IF   E           K DISK    USROPN
     FWMAGL     IF   E           K DISK    USROPN
     FWTENL     IF   E           K DISK
     FWDF2L     IF   E           K DISK    USROPN
     FWMRPL     IF   E           K DISK    USROPN
     FWMMUP     O    E             DISK    USROPN
     FWMMUL     IF   E           K DISK
     F                                     RENAME(PMU01:PMU02)
     FWXFMP     O    E             DISK    USROPN
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWMRSL     IF   E           K DISK
     FWXPEL     UF   E           K DISK
     FWXPEL1    IF   E           K DISK
     F                                     RENAME(PPE01:PPE02)
      *** Beg add * 07/17/16
     FXMLPREL1  UF   E           K DISK
     FXMLTEML1  UF   E           K DISK
     fWXWXFMP   o    e             disk    UsrOpn
      *** End add * 07/17/16

      /COPY WWCPOLDAT4

     D XXFORM          C                   CONST('WC000402  ')
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
     D PSDS          ESDS                  EXTNAME(SZQ1P)

      *** Beg Add * 07/17/16
     d svlda          Uds
     d ldaXml                191    250

     D RunningSRText   S               N   Inz(*Off)
     D formName        s             15A
     D PrintingPolicy  S               N
     D isScriptUsed    s              1A   inz('N')
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

     d SendToScriptura...
     d                 pr                  ExtPgm('S4401C')
     d  outXMLString                 61
      *** End Add * 07/17/16

      *  FUND MASTER KLIST
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *  EMPLOYER MASTER KLIST
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *  EMPLOYER DETAIL KLIST
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
      *  AGENT HISTORY FILE
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *  AGENT ASSIGNMENT FILE
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
      *  AGENT MASTER FILE
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
      *  ENDORSEMENT FORM TABLE FILE
     C     ENKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XKFORM
      *  FUND AUXILIARY KLIST
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *  CARRIER FILE
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C     PEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXDT              7 0
     C     PEKEY1        KLIST
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
     C     *LIKE         DEFINE    PEUNID        XXUNID
     C     *LIKE         DEFINE    PEEND#        X1END#
      *  DATE CHECK/CONVERSION PARM LISTS
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
      *  ENTRY PARAMETERS
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQMODE            1
      *================================================================***
      *  Main
      *================================================================***
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   MOVEL     XXFORM        XKFORM

      //  Beg Add *** 07/17/16
     C     *dtaara       Define    *lda          svlda
      /Free
          formName = %subst(xxForm :1 :2) + ' ' + %subst(xxForm :3 :2) + ' ' +
           %subst(xxForm :5 :2) + ' ' + %subst(xxForm :7 :2) + ' ' +
           %subst(xxForm :9 :2);
          Exsr Scriptura;
      /End-Free
      //  Beg Add *** 07/17/16

     C                   SELECT
      *  DETERMINE WHETHER OR NOT TO ATTACH THIS FORM TO THE POLICY
     C     QQMODE        WHENEQ    '1'
     C                   EXSR      CHEKIT
      *  GATHER INFO TO MERGE TO THE FORM
     C     QQMODE        WHENEQ    '2'
     C                   EXSR      BULDIT
     C                   ENDSL
     C                   MOVE      *ON           *INLR
     C                   RETURN
      *================================================================***
      *  CHEKIT - Subroutine to determine whether or not to attach form
      *================================================================***
     C     CHEKIT        BEGSR
     C                   MOVE      *OFF          CRTFRM            1
      *  GET THE EMPLOYER AND FORM INFO
     C                   EXSR      GETEMP
     C                   EXSR      GETFRM
      *    VALIDATE ATTACHMENT OF FORM HERE
      *  ANNIVERSARY DATE MUST BE ENTERED
     C     ELKY2         SETLL     PEL02
     C     ELKY2         READE     PEL02                                  60
      *  At least one division must not have rating state of 'AL'
     C     *IN60         DOWEQ     *OFF
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C     RSJRST        ANDNE     'AL'
     C     ELANDT        ANDNE     *ZERO
     C     ELANDT        ANDNE     elptdt
     C                   MOVE      *ON           CRTFRM
     C                   LEAVE
     C                   ENDIF
     C     ELKY2         READE     PEL02                                  60
     C                   ENDDO
      *  Reset the Employer Detail and Rate Set Code Desc files
     C     ELKY          CHAIN     PEL01                              60
     C     RSKY          CHAIN     PRS01                              60
     C     CRTFRM        IFEQ      *ON
      *  OPEN THE EMPLOYER ATTACHED ENDORSEMENT FORMS FILE
     C                   OPEN      WMMUP
      *  CLEAR OUT THE RECORD
     C                   CLEAR                   PMU01
      *  LOAD THE RECORD
     C                   TIME                    DSDTTM
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
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUDT
     C                   MOVEL     Q1USER        MUIUSR
     C                   MOVEL     Q1PGM         MUIPGM
     C                   MOVE      MUDT          MUIDT
      *  WRITE A RECORD TO THE EMPLOYER ATTECHED ENDORSEMENT FORMS FILE
     C                   WRITE     PMU01
      *  CLOSE THE EMPLOYER ATTECHED ENDORSEMENT FORMS FILE
     C                   CLOSE     WMMUP
     C                   ENDIF
     C                   ENDSR
      *================================================================***
      *  BULDIT - Subroutine to gather info to merge to the form
      *================================================================***
     C     BULDIT        BEGSR
      *  OPEN THE ENDORSEMENT FORM WORKFILE
     C                   OPEN      WXFMP
      *  GET THE BASIC INFO
     C                   EXSR      GETFND
     C                   EXSR      GETEMP
     C                   EXSR      GETAGT
     C                   EXSR      GETFRM
     C                   EXSR      GETCAR
      *  CLEAR OUT THE RECORD
     C                   CLEAR                   PFM01
      *  LOAD BASIC WXFMP FIELDS
     C                   EXSR      BASINF
      *  LOAD FORM SPECIFIC INFO
     C                   EXSR      OTHINF
      *  CLOSE THE ENDORSEMENT FORM WORKFILE
     C                   CLOSE     WXFMP
     C                   ENDSR
      *================================================================***
      *  GETFND - Subroutine to get fund information
      *================================================================***
     C     GETFND        BEGSR
     C                   OPEN      WMFNL
     C                   OPEN      WDF2L
      *  GET THE FUND MASTER RECORD
     C     FNKY          CHAIN     PFN01                              60
     C     FNPST         IFNE      *BLANK
     C                   MOVEL     FNPST         XXFNST
     C                   ELSE
     C                   MOVEL     FNMST         XXFNST
     C                   ENDIF
      *  GET THE FUND AUXILIARY RECORD
     C     F2KY          CHAIN     PF201                              60
     C                   MOVE      F2CAR#        XXCARR
     C                   CLOSE     WMFNL
     C                   CLOSE     WDF2L
     C                   ENDSR
      *================================================================***
      *  GETEMP - Subroutine to get employer information
      *================================================================***
     C     GETEMP        BEGSR
     C                   MOVE      *ZERO         XXPFDT
      *  GET THE EMPLOYER MASTER RECORD
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
      *  BUILD UP ADDRESS LINES USING MAILING ADDRESS DATA
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
      *  BUILD UP ADDRESS LINES USING PHYSICAL ADDRESS DATA
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
      *  GET THE EMPLOYER DETAIL RECORD
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C                   ENDIF
     C                   ENDSR
      *================================================================***
      *  GETAGT - Subroutine to get agent information
      *================================================================***
     C     GETAGT        BEGSR
     C                   OPEN      WMAHL
     C                   OPEN      WMAAL1
     C                   OPEN      WMAGL
     C                   Z-ADD     *ZERO         XXAGT#
      *  GET THE CURRENT AGENT HISTORY RECORD
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
      *  GETFRM - Subroutine to get endorsement form table record
      *================================================================***
     C     GETFRM        BEGSR
      *  GET THE ENDORSMENT FORM TABLE RECORD
     C     ENKY          CHAIN     PEN01                              60
     C     *IN60         DOWEQ     *OFF
     C     ENSTAT        ANDEQ     'I'
     C     ENKY          READE     PEN01                                  60
     C                   ENDDO
     C                   ENDSR
      *================================================================***
      *  GETCAR - Subroutine to get carrier information
      *================================================================***
     C     GETCAR        BEGSR
     C                   OPEN      WMRPL
      *  GET THE CARRIER INFO RECORD
     C     RPKY          CHAIN     PRP01                              60
     C                   MOVEL     RPCAP#        XXCPL#
     C                   CLOSE     WMRPL
     C                   ENDSR
      *================================================================***
      *  BASINF - Subroutine to load basic WXFMP fields
      *================================================================***
     C     BASINF        BEGSR
     C                   TIME                    DSDTTM
     C                   MOVE      XXCO#         FMCO#
     C                   MOVE      XXFND         FMFND
     C                   MOVE      XXFYR         FMFYR
     C                   MOVE      XXEMP#        FMEMP#
     C                   MOVE      XXDIV         FMDIV
     C                   MOVEL     XXFORM        FMFORM
     C                   MOVE      ENEDDT        DSDAT7
     C                   MOVEL     DSMM          FMEDDT
     C                   MOVE      DSYY          FMEDDT
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
     C     QQFND         CAT(P)    QQEMP#:0      FMPOL#
     C                   CAT       QQFYR:0       FMPOL#
     C                   MOVEL     EMTPOB        FMTOBC
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
     *
     C                   Z-ADD     ELANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FMDT01
     *
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
      *========================================================================
      *  OTHINF - Subroutine to load form specific information
      *========================================================================
     C     OTHINF        BEGSR
     C     ELKY2         SETLL     PEL02
     C     ELKY2         READE     PEL02                                  60
      ** At least one division must not have rating state of 'AL'
     C     *IN60         DOWEQ     *OFF
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C     RSJRST        ANDNE     'AL'
     C     ELANDT        ANDNE     *ZEROS
     C                   MOVE      *ZEROS        DSDAT7
     C                   MOVE      *ZEROS        FMANDA
     C                   MOVE      *ZEROS        FMANMO
     C                   MOVE      ELANDT        DSDAT7
     C                   MOVE      DSDD          FMANDA
     C                   MOVE      DSMM          FMANMO
     *
     C                   Z-ADD     ELANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FMDT01
     *
     C                   LEAVE
     C                   ENDIF
     C     ELKY2         READE     PEL02                                  60
     C                   ENDDO
      *  If printing a policy page.  (Not called from W1054R)
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W1054R'      #1PGM            10
     C                   PARM      *BLANK        #1FLAG            1
      * Write the record.

      //  Beg Add *** 07/17/16
      /Free
          If #1Flag = '0';
             PrintingPolicy = *On;
          Else;
             PrintingPolicy = *Off;
          Endif;
          Exsr WriteWorkFile;
      /End-Free
      //  Beg Add *** 07/17/16

     C     #1FLAG        IFEQ      '0'
      * Update the endorsement reporting file with the proper data.
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             XXUNID
      * Find the unique ID number.
     C                   MOVE      *HIVAL        XXDT
     C     PEKEY         SETGT     PPE02
     C     PEKEY1        READPE    PPE02                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     PEUNID        XXUNID
     C                   ENDIF
     C                   MOVEL     XXFORM        X1END#
     C     PEKEY2        CHAIN     PPE01                              60
      * Copy data into the record.
     C     *IN60         IFEQ      *OFF
     C                   CLEAR                   E00402
     C                   MOVE      DSDAT7        X9ANDT
     C                   ADD       10000         X9ANDT
     C                   MOVEL(P)  E00402        PEDATA
     C                   UPDATE    PPE01
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
      // *** Beg Add * 07/17/16
      /Copy SCOPYEN02C
      // *** End Add * 07/17/16
