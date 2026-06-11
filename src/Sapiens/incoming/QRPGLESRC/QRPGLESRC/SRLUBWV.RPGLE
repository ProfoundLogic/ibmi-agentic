     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE LUB0020199 - CLT133 - WAIVER OF SUBROGATION
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* This program is called to:                                    *
     H*  1) Determine if this endorsement form should be attached     *
     H*     to the policy.                                            *
     H*  2) Create data in the Endorsement Form workfile (WXFMP)      *
     H*     prior to a form merge.                                    *
     H*                                                               *
     H* PROGRAMMER: C:61                                              *
     H* CREATED:    06/21/05           PROJECT: 133WBS                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/21/05 C:61  PROJECT 133WBS                                 *
     H*                Copied from SR00313 and modified for clt133.   *
     H*                                                               *
     H*****************************************************************
     H* FILE USAGE:                                                   *
     H*                                                               *
     H*  WDELL  (PEL01):  CO#/FND/EMP#/DIV/FYR                        *
     H*  WDELL1 (PEL02):  CO#/FND/FYR/EMP#/DIV                        *
     H*  WDF2L  (PF201):  CO#/FND                                     *
     H*  WMAAL1 (PAA01):  CO#/FND/AGT#                                *
     H*  WMAGL  (PAG01):  CO#/FEIN/MOD                                *
     H*  WMAHL  (PAH01):  CO#/FND/EMP#/DIV/FYR/EFDT/AGT#              *
     H*  WMEML  (PEM01):  CO#/FND/EMP#/DIV                            *
     H*  WMFNL  (PFN01):  CO#/FND                                     *
     H*  WMRPL  (PRP01):  CO#/FND/EMP#/DIV/FYR/CARR                   *
     H*  WMWVL  (PWV01):  CO#/FND/EMP#/DIV/FYR                        *
     H*  WTENL  (PEN01):  CO#/FND/FORM                                *
     H*  WMMUL4 (PMU01):  EMPLOYER ATTACHED ENDORSEMENT FORM FILE     *
     H*  WXFMP  (PFM01):  ENDORSEMENT FORM WORKFILE                   *
     H*  WXICP  (PXI01):  MIDTERM-ENDORSEMENT FILE                    *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  60  GENERAL CHAIN (READ) INDICATOR                           *
     H*                                                               *
     H*****************************************************************
     H* SUBROUTINE SUMMARY:                                           *
     H*                                                               *
     H*  BASINF - Collect the basic info for WXFMP.                   *
     H*  BULDIT - Gather information to merge to the form.            *
     H*  CHEKIT - Determine whether or not to attach the form.        *
     H*  GETAGT - Get the agent information.                          *
     H*  GETCAR - Get the carrier information.                        *
     H*  GETEMP - Get the employer information.                       *
     H*  GETFND - Get the fund information.                           *
     H*  GETFRM - Get the endorsement form information.               *
     H*  OTHINF - Get the form-specific information.                  *
     H*  INCREC - Write a merged forms data record.                   *
     H*                                                               *
     H*****************************************************************
     H* NOTES:                                                        *
     H*                                                               *
     H*  QQMODE = 2 - Called to print endorsement.                    *
     H*  QQMODE = 3 - Called for additional input.                    *
     H*  QQMODE = 4 - Called for mid-term processing.                 *
     H*                                                               *
     H*****************************************************************
      *
     FWDELL     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWDF2L     IF   E           K DISK    USROPN
     FWMAAL1    IF   E           K DISK    USROPN
     FWMAGL     IF   E           K DISK    USROPN
     FWMAHL     IF   E           K DISK    USROPN
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK    USROPN
     FWMMUL4    IF A E           K DISK    USROPN
     FWMRPL     IF   E           K DISK    USROPN
      *
      * Waiver
     FWMWVL5    UF   E           K DISK
     F                                     RENAME(PWV01:PWV05)
     FWMWVL6    IF   E           K DISK
     F                                     RENAME(PWV01:PWV06)
      *
     FWTENL     IF   E           K DISK
     FWXFMP     O    E             DISK    USROPN
     FWXICP     O    E             DISK    USROPN
     FWXPEL     UF A E           K DISK
     FWMSHL1    IF   E           K DISK
     FWMMUL     IF   E           K DISK
     F                                     RENAME(PMU01:PMU02)
      *
      *
      *================================================================
      * Data structures & constants
      *================================================================

     D/COPY WWCPOLDAT3
      *
     D XXFORM          C                   CONST('LUB0020199')
      *
     D                 DS
     D  XXZIP                  1      9
     D  XXZIP1                 1      5
     D  XXZIP2                 6      9
      *
     D                 DS
     D  XXPROJ                 1     60
     D  FMC301                 1     30
     D  FMC302                31     60
      *
     D                 DS
     D  XXPRJ2                 1     60
     D  FMC303                 1     30
     D  FMC304                31     60
      *
     D                 DS
     D  DSDAT7                 1      7  0
     D  DSCENT                 1      1
     D  DSYY                   2      3
     D  DSMM                   4      5
     D  DSDD                   6      7
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *================================================================
      * KLIST definitions
      *================================================================
      *
      * Agent Assignment file
      *

     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
      *
      * Agent Master file
      *
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
      *
      * Agent History file
      *
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
      * Employer Detail file
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
      *
      * Employer Master file
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      * Endorsement Form Table file
      *
     C     ENKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XKFORM
      *
      * Fund Master file
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      * Fund Auxiliary file
      *
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      * Carrier file
      *
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
      * Employer Endorsement Forms File Key
      *
     C     MUKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XKFORM
      *
     C     MUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEND#
      *
      * Waiver Detail file
      *
     C     WVKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    LDEND#
      *
     C     WVKY6         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
      *
      *
      *
     C     PEKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXUNID
     C                   KFLD                    X1END#
      *
      * SUBMISSION HISTORY FILE
      *
     C     SHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    X1TYPE
      *
      *
      *================================================================
      * *LIKE DEFN Definitions
      *================================================================
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
     C     *LIKE         DEFINE    MUEND#        XXEND#
     C     *LIKE         DEFINE    MUSEQ#        XXSEQ#
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C     *LIKE         DEFINE    ELENMP        XXENMP
     C     *LIKE         DEFINE    FMEEDT        XXEFDT
     C     *LIKE         DEFINE    PEUNID        XXUNID
     C     *LIKE         DEFINE    PEEND#        X1END#
     C     *LIKE         DEFINE    DSTIME        SVTIME
      *
      *
      *================================================================
      * PLIST Definitions
      *================================================================
      *
      *  Date check/conversion parameters
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *  Entry parameters
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQMODE            1
     C                   PARM                    QQRRN             9 0
     C                   PARM                    QQMOD2            1            ONLY FOR QQMODE='3'
      *
      *
      *================================================================
      * Main
      *================================================================
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
      *
     C                   MOVEL     XXFORM        XKFORM
      *
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
      *
     C                   SELECT
      *
      * Gather information to merge to the form
      *
     C     QQMODE        WHENEQ    '2'
     C                   EXSR      BULDIT
      *
      * Call additional information entry program
      *
     C     QQMODE        WHENEQ    '3'
      *
     C                   CALL      'W0815R'
     C                   PARM      *OFF          ##KC              1
     C                   PARM      *OFF          ##KL              1
     C                   PARM      QQMOD2        ##CALL            1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      QQRRN         ##RRN2            9 0
      *
      * Gather info to merge to the form for mid-term endors processing
      *
     C     QQMODE        WHENEQ    '4'
     C                   EXSR      CHEKIT
      *
     C     CRTFRM        IFEQ      *ON
     C                   EXSR      BULDIT
     C                   EXSR      INCREC
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
      *
      *
      *================================================================***
      * BASINF - Subroutine to load basic WXFMP fields
      *================================================================***
     C     BASINF        BEGSR
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVE      XXCO#         FMCO#
     C                   MOVE      XXFND         FMFND
     C                   MOVE      XXFYR         FMFYR
     C                   MOVE      XXEMP#        FMEMP#
     C                   MOVE      XXDIV         FMDIV
     C                   MOVEL     XXFORM        FMFORM
      *
     C                   MOVE      ENEDDT        DSDAT7
     C                   MOVEL     DSMM          FMEDDT
     C                   MOVE      DSYY          FMEDDT
      *
     C     LDENDA        IFNE      *BLANKS
     C     LDENDA        ANDNE     *ZEROS
     C                   MOVE      LDEEDT        FMEEDT
     C                   Z-ADD     LDEND#        FMEND#
      *
     C                   ELSE
      *
     C     MUKY1         CHAIN     PMU02                              99
      *
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     MUEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FMEEDT
      *
     C                   ELSE
      *
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FMEEDT
     C                   ENDIF
      *
     C                   Z-ADD     *ZERO         FMEND#
     C                   ENDIF
      *
     C     QQFND         CAT(P)    QQEMP#:0      FMPOL#
     C                   CAT       QQFYR:0       FMPOL#
      *
     C                   MOVEL     EMTPOB        FMTOBC
     C                   MOVEL     EMNAME        FMEMNM
     C                   MOVEL     EMFDNM        FMFENM
     C                   MOVEL     XXFNST        FMFNST
     C                   MOVEL     FNNAME        FMFNNM
     C                   Z-ADD     XXDIV         SVDIV
     C                   Z-ADD     0             XXENMP
     C     ELKY2         CHAIN     PEL02                              61
      *
     C     *IN61         DOWEQ     *OFF
     C                   ADD       ELENMP        XXENMP
     C                   ADD       ELETER        XXENMP
     C                   ADD       ELEDTR        XXENMP
     C     ELKY2         READE     PEL02                                  61
     C                   ENDDO                                                  *IN61 = *OFF
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
      *
     C                   MOVE      ELANDT        DSDAT7
     C                   MOVE      DSDD          FMANDA
     C                   MOVE      DSMM          FMANMO
      *
     C                   MOVEL     Q1USER        FMUSER
     C                   MOVEL     Q1PGM         FMPGM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FMDT
      *
     C                   MOVEL     DSTIME        FMTIME
     C                   MOVEL     'A'           FMSTAT
     C                   MOVEL     Q1USER        FMIUSR
     C                   MOVEL     Q1PGM         FMIPGM
     C                   MOVE      FMDT          FMIDT
     C                   ENDSR                                                  BASINF
      *
      *
      *================================================================***
      * BULDIT - Subroutine to gather info to merge to the form
      *================================================================***
     C     BULDIT        BEGSR
      *
      * Open the endorsement form workfile
      *
     C                   OPEN      WXFMP
      *
      * Get the basic information
      *
     C                   EXSR      GETFND
     C                   EXSR      GETEMP
     C                   EXSR      GETAGT
     C                   EXSR      GETFRM
     C                   EXSR      GETCAR
      *
      * Clear out the record
      *
     C                   CLEAR                   PFM01
      *
      * Load basic WXFMP fields
      *
     C                   EXSR      BASINF
      *
      * Load form specific fields
      *
     C                   EXSR      OTHINF
      *
      * Close the endorsement form workfile
      *
     C                   CLOSE     WXFMP
     C                   ENDSR                                                  BULDIT
      *
      *
      *================================================================***
      * CHEKIT -Subroutine to determine if this form should be attached
      *================================================================***
     C     CHEKIT        BEGSR
     C                   MOVE      *OFF          CRTFRM            1
      *
      * Get the employer and form information
      *
     C                   EXSR      GETEMP
     C                   EXSR      GETFRM
      *
      *
      ********************************************************************
      * VALIDATE ATTACHMENT OF FORM HERE
      ********************************************************************
      *
      * Look for any waivers.
      *
      *  mid-term process
      *
     C     WVKY5         CHAIN(N)  PWV05                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     WVSTAT        IFNE      'I'                                          NOT INACTIVE
     C                   MOVE      *ON           CRTFRM
     C                   LEAVE
     C                   ENDIF                                                  WVSTAT = 'A'
      *
     C     WVKY5         READE(N)  PWV05                                  60
     C                   ENDDO
      *
      *
      ********************************************************************
      *  END OF VALIDATION
      ********************************************************************
      *
     C     CRTFRM        IFEQ      *ON
      *
      * Open the Employer Attached Endorsement Forms file
      *
     C                   OPEN      WMMUL4
     C                   Z-ADD     *ZEROS        XXEND#
     C                   Z-ADD     *ZEROS        XXSEQ#
      *
      * If processing for Mid-Year Endorsements, retrieve the previous
      *  endorsement and sequence numbers.
      *
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C                   Z-ADD     LDEND#        XXEND#
      *
     C     MUKY          SETGT     PMU01
     C     MUKY          READPE    PMU01                                  60
      *
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     1             XXSEQ#
      *
     C                   ELSE
      *
     C     MUSEQ#        ADD       1             XXSEQ#
     C                   ENDIF
      *
     C                   ENDIF
      *
      * Clear out the record
      *
     C                   CLEAR                   PMU01
      *
      * Load the record
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVE      XXCO#         MUCO#
     C                   MOVE      XXFND         MUFND
     C                   MOVE      XXEMP#        MUEMP#
     C                   MOVE      XXDIV         MUDIV
     C                   MOVE      XXFYR         MUFYR
     C                   MOVE      XXEND#        MUEND#
     C                   MOVE      XXSEQ#        MUSEQ#
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
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MUDT
      *
     C                   MOVEL     Q1USER        MUIUSR
     C                   MOVEL     Q1PGM         MUIPGM
     C                   MOVE      MUDT          MUIDT
      *
      * Write the record.
      *
     C                   WRITE     PMU01
      *
      * Close the Employer Attached Endorsement Forms file.
      *
     C                   CLOSE     WMMUL4
     C                   ENDIF                                                  CRTFRM = *ON
      *
     C                   ENDSR
      *
      *
      *================================================================***
      * GETAGT - Subroutine to get agent information
      *================================================================***
     C     GETAGT        BEGSR
     C                   OPEN      WMAHL
     C                   OPEN      WMAAL1
     C                   OPEN      WMAGL
     C                   Z-ADD     *ZERO         XXAGT#
      *
      * Get the current agent history record
      *
     C     AHKY          SETGT     PAH01
     C     AHKY          READPE    PAH01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     AHKY          READPE    PAH01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     AHAGT#        XXAGT#
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C  N60AAKY          CHAIN     PAA01                              60
     C  N60AGKY          CHAIN     PAG01                              60
      *
     C                   CLOSE     WMAHL
     C                   CLOSE     WMAAL1
     C                   CLOSE     WMAGL
     C                   ENDSR                                                  GETAGT
      *
      *
      *================================================================***
      * GETCAR - Subroutine to get carrier information
      *================================================================***
     C     GETCAR        BEGSR
     C                   OPEN      WMRPL
      *
      * Get the Carrier information record
      *
     C     RPKY          CHAIN     PRP01                              60
     C                   MOVEL     RPCAP#        XXCPL#
     C                   CLOSE     WMRPL
     C                   ENDSR                                                  GETCAR
      *
      *
      *================================================================***
      * GETEMP - Subroutine to get employer information
      *================================================================***
     C     GETEMP        BEGSR
     C                   MOVE      *ZERO         XXPFDT
      *
      * Get the Employer Master record
      *
     C     EMKY          CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
      * Build up the address lines using mailing address data . . .
      *
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL(P)  EMMAD1        XXEAD1
     C                   MOVEL     EMMST         XXEMST
     C                   MOVE      EMMZIP        XXZIP
     C     EMMCTY        CAT(P)    ',':0         XXCSZ            40
     C                   CAT       EMMST:1       XXCSZ
     C                   CAT       XXZIP1:1      XXCSZ
      *
     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         XXCSZ
     C                   CAT       XXZIP2:0      XXCSZ
     C                   ENDIF                                                  XXZIP2 <> ' '
      *
     C     EMMAD2        IFNE      *BLANK
     C                   MOVEL(P)  EMMAD2        XXEAD2
     C                   MOVEL(P)  XXCSZ         XXEAD3
      *
     C                   ELSE
      *
     C                   MOVEL(P)  XXCSZ         XXEAD2
     C                   MOVEL(P)  *BLANKS       XXEAD3
     C                   ENDIF                                                  EMMAD2 <> ' '
      *
     C                   ELSE
      *
      * . . .or build up the address lines using physical address data
      *
     C                   MOVEL(P)  EMPAD1        XXEAD1
     C                   MOVEL     EMPST         XXEMST
     C                   MOVE      EMPZIP        XXZIP
     C     EMPCTY        CAT(P)    ',':0         XXCSZ
     C                   CAT       EMPST:1       XXCSZ
     C                   CAT       XXZIP1:1      XXCSZ
      *
     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         XXCSZ
     C                   CAT       XXZIP2:0      XXCSZ
     C                   ENDIF                                                  XXZIP2 <> ' '
      *
     C     EMPAD2        IFNE      *BLANK
     C                   MOVEL(P)  EMPAD2        XXEAD2
     C                   MOVEL(P)  XXCSZ         XXEAD3
      *
     C                   ELSE
      *
     C                   MOVEL(P)  XXCSZ         XXEAD2
     C                   MOVEL(P)  *BLANKS       XXEAD3
     C                   ENDIF                                                  EMPAD2 <> ' '
      *
     C                   ENDIF                                                  EMMPFL = 'M'
      *
     C                   ENDIF                                                  *IN60 = *ON
      *
      * Get the Employer Detail record
      *
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C                   ENDSR                                                  GETEMP
      *
      *
      *================================================================***
      * GETFND - Subroutine to get fund information
      *================================================================***
     C     GETFND        BEGSR
     C                   OPEN      WMFNL
     C                   OPEN      WDF2L
      *
      * Get the Fund Master record
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C     FNPST         IFNE      *BLANK
     C                   MOVEL     FNPST         XXFNST
     C                   ELSE
     C                   MOVEL     FNMST         XXFNST
     C                   ENDIF                                                  FNPST <> ' '
      *
      * Get the Fund Auxiliary record
      *
     C     F2KY          CHAIN     PF201                              60
      *
     C                   MOVE      F2CAR#        XXCARR
      *
     C                   CLOSE     WMFNL
     C                   CLOSE     WDF2L
      *
     C                   ENDSR                                                  GETFND
      *
      *
      *================================================================***
      * GETFRM - Subroutine to get endorsement form table record
      *================================================================***
     C     GETFRM        BEGSR
      *
      * Get the Endorsement Form Table record
      *
     C     ENKY          CHAIN     PEN01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     ENSTAT        ANDEQ     'I'
     C     ENKY          READE     PEN01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
      *
     C                   ENDSR                                                  GETFRM
      *
      *
      *================================================================***
      * OTHINF - Subroutine to load form specific information
      *================================================================***
     C     OTHINF        BEGSR
      *
      *  If called from mid-term process
      *
     C     QQMODE        IFEQ      '4'
     C     FMEND#        ORNE      *ZERO
     C                   EXSR      INFO1
      *
     C                   ELSE
      *
      *  ELSE - called from W1054R
      *
     C                   EXSR      INFO2
     C                   ENDIF                                                  QQMODE = 4
      *
     C                   ENDSR                                                  OTHINF
      *
      *
      *================================================================***
      * INFO1 - Get OTHINF MID-TERM Endorsement records
      *================================================================***
     C     INFO1         BEGSR
     C                   MOVE      *BLANK        XXFRST            1
      *
      * Look for waivers.
      *
     C     WVKY5         CHAIN     PWV05                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     WVSTAT        IFNE      'I'                                          NOT INACTIVE
     C                   MOVEL     WVNAME        FMNM01
     C                   MOVEL     WVADR1        FMC401
      *
     C     WVADR2        IFNE      *BLANK
     C                   MOVEL     WVADR2        FMC402
     C                   ENDIF                                                  WVADR2 <> ' '
      *
     C                   MOVE      WVZIP         XXZIP
     C     WVCTY         CAT       ',':0         FMC403
     C                   CAT       WVST:1        FMC403
     C                   CAT       XXZIP1:1      FMC403
      *
     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         FMC403
     C                   CAT       XXZIP2:0      FMC403
     C                   ENDIF                                                  XXZIP2 <> ' '
      *
     C     FMC402        IFEQ      *BLANKS
     C                   MOVEL(P)  FMC403        FMC402
     C                   MOVEL(P)  *BLANKS       FMC403
     C                   ENDIF
      *
      * Load the project lines.
      *
     C     WVPROJ        IFNE      *BLANKS
     C                   MOVEL     WVPROJ        XXPROJ
     C                   ENDIF                                                  WVPROJ<>' '
      *
     C     WVPRJ2        IFNE      *BLANKS
     C                   MOVEL     WVPRJ2        XXPRJ2
     C                   ENDIF                                                  WVPRJ2<>' '
      *
     C                   Z-ADD     WVWAV#        FMA701
      *
      * Write the record.
      *
     C                   WRITE     PFM01
      *
     C                   MOVE      'Y'           WVEPRT
     C                   UPDATE    PWV05
      *
     C                   MOVE      *BLANKS       FMNM01
     C                   MOVE      *BLANKS       FMC401
     C                   MOVE      *BLANKS       FMC402
     C                   MOVE      *BLANKS       FMC403
     C                   MOVE      *BLANKS       XXPROJ
     C                   MOVE      *BLANKS       XXPRJ2
     C                   CLEAR                   FMA701
     C                   ENDIF                                                  WVSTAT <>'I'
      *
     C     WVKY5         READE     PWV05                                  60
     C                   ENDDO
      *
     C                   ENDSR                                                  INFO1
      *
      *
      *================================================================***
      * INFO2 - Get OTHINF Endorsement records if NOT Mid-Term processing
      *================================================================***
     C     INFO2         BEGSR
      *
     C                   MOVE      *BLANK        XXFRST            1
      *
      * Look for waivers.
      *
     C     WVKY6         CHAIN     PWV06                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     WVSTAT        IFNE      'I'                                          NOT INACTIVE
     C     WVEND#        ANDEQ     0
     C                   MOVEL     WVNAME        FMNM01
     C                   MOVEL     WVADR1        FMC401
      *
     C     WVADR2        IFNE      *BLANK
     C                   MOVEL     WVADR2        FMC402
     C                   ENDIF                                                  WVADR2 <> ' '
      *
     C                   MOVE      WVZIP         XXZIP
     C     WVCTY         CAT       ',':0         FMC403
     C                   CAT       WVST:1        FMC403
     C                   CAT       XXZIP1:1      FMC403
      *
     C     XXZIP2        IFNE      *BLANK
     C     XXZIP2        ANDNE     *ZERO
     C                   CAT       '-':0         FMC403
     C                   CAT       XXZIP2:0      FMC403
     C                   ENDIF                                                  XXZIP2 <> ' '
      *
     C     FMC402        IFEQ      *BLANKS
     C                   MOVEL(P)  FMC403        FMC402
     C                   MOVEL(P)  *BLANKS       FMC403
     C                   ENDIF
      *
      * Load the project lines.
      *
     C     WVPROJ        IFNE      *BLANKS
     C                   MOVEL     WVPROJ        XXPROJ
     C                   ENDIF                                                  WVPROJ<>' '
      *
     C     WVPRJ2        IFNE      *BLANKS
     C                   MOVEL     WVPRJ2        XXPRJ2
     C                   ENDIF                                                  WVPRJ2<>' '
      *
     C                   Z-ADD     WVWAV#        FMA701
      *
      * Write the record.
      *
     C                   WRITE     PFM01
      *
      * Update the endorsement reporting file with the proper data.
      *
     C                   EXSR      ENDRPT
      *
     C                   MOVE      *BLANKS       FMNM01
     C                   MOVE      *BLANKS       FMC401
     C                   MOVE      *BLANKS       FMC402
     C                   MOVE      *BLANKS       FMC403
     C                   MOVE      *BLANKS       XXPROJ
     C                   MOVE      *BLANKS       XXPRJ2
     C                   CLEAR                   FMA701
     C                   ENDIF                                                  WVSTAT <>'I'
      *
     C     WVKY6         READE     PWV06                                  60
     C                   ENDDO
      *
     C                   ENDSR                                                  INFO2
      *
      *
      *================================================================***
      * ENDRPT - Update Endorsement reporting file
      *================================================================***
     C     ENDRPT        BEGSR
      *
      * Find the unique ID number.
      *
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             XXUNID
     C     SHKEY         CHAIN     PSH01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     SHUNID        XXUNID
     C                   ENDIF
      *
     C                   MOVEL     XXFORM        X1END#
     C     PEKEY2        CHAIN     PPE01                              60
      *
      * Copy data into the record.
      *
     C     *IN60         IFEQ      *OFF
     C                   CLEAR                   E00313
     C                   MOVEL     WVNAME        XXNOP
     C                   MOVEL     WVPROJ        XXNOO
     C                   MOVEL(P)  E00313        PEDATA
      *
     C     XXFRST        IFEQ      *BLANK
     C                   UPDATE    PPE01
     C                   MOVE      'Y'           XXFRST
     C                   ELSE
     C                   WRITE     PPE01
     C                   ENDIF                                                  XXFRST IFEQ
      *
     C                   ENDIF
      *
     C                   ENDSR                                                  ENDRPT
      *
      *
      *================================================================***
      * INCREC - Subroutine to write a merged forms data record
      *================================================================***
     C     INCREC        BEGSR
      *
     C                   OPEN      WXICP
      *
     C     '.inc(LUB'    CAT(P)    '00201.':0    ICSTMT
     C                   CAT       '99,':0       ICSTMT
     C                   CAT       'AMTEMP':0    ICSTMT
     C                   CAT       ',)':0        ICSTMT
      *
     C                   WRITE     PIC01
      *
     C                   CLOSE     WXICP
     C                   ENDSR                                                  INCREC
      *
