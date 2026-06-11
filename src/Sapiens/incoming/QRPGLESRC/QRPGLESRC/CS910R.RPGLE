     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CS901R - PRINT FORMS FOR CLAIM SCAN
      *****************************************************************
      * INDICATOR USAGE:
     H*    01       USED AS FIRST TIME INDICATOR (OFF=1ST TIME)
     H*    05       USE MEMBER TYPE FORM FOR CLIENT PRINT
     H*   N05       USE NON-MEMBER TYPE FORM FOR CLIENT PRINT
     H*    08       CLIENT EXTENDED ZIP CODE EXISTS
     H*    11       FUND 830 MEMBER
     H*    12       FUND 520 MEMBER
     H*    13       FRF NON-SIF MEMBER??
     H*    14       IF MEMBER NUMBER AND FUND, PRINT ON ENVELOPE
     H*    15       MED ONLY PAID
     H*    16       WORKERS' COMP PAID
     H*    60       USED AS GENERAL CHAIN INDICATOR
     H*    LR       LAST RECORD HAS BEEN READ (PERFORM FINAL ROUTINES)
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 06/09/20 C:295 Project: SY420
     H*                Mask SSN.
     H*
     H* 02/10/98 C:62  PROJECT#: MC145
     H*                ** USED CONVERSION FROM MDYY YO CYMD
     H*
     H* 11/11/97 C:86  PROJECT CS010
     H*          1. EXPAND TO 8.0N ON SCREEN AND 7.0N DATABASE DATE
     H*             LENGTH
     H*          2. REMOVE THE USE OF CSITYP AND ITS DESCRIPTION
     H*          3. ADD USE OF CSPART AND ITS DESCRIPTION
     H*          4. REMOVE THE USE OF CSDLST
     H* 09/22/94 C:26  PROJECT
     H*                ADJUST OUTPUT FOR FORMS OVERLAY
     H* 12/18/90 LJD   PER TLB:
     H*                1.  BREAK OUT EMPLOYEE NAME INTO LAST,FIRST,MI
     H*                2.  CHANGED CLIENT FILE TO CMCTP/CMCTL1
     H* 01/08/90 MDG   PER TLB:
     H*                ADD THE DISABILITY TYPE TABLE FOR DESCRIPTIONS
     H*
     H*****************************************************************
      *****************************************************************
     F*** CLIENT SUMMARY FILE (1 PER SCAN REQUEST)
     FCWCLP     IP   E           K DISK
      *
     F*** CLIENT SUMMARY FILE (1 PER SCAN REQUEST)
     FCWCLL1    IF   E           K DISK
     F                                     RENAME(PCL01:PCL02)
      *
     F*** CLIENT MASTER FILE
     FCMCTL1    IF   E           K DISK
      *
     F*** EMPLOYEE DETAIL FILE (1 PER SELECTION)
     FCWCSL     IF   E           K DISK
      *
     F*** CARRIER MASTER FILE
     FCMCRL1    IF   E           K DISK
      *
      ****** BEGIN CHANGE ******* 11/11/97 ***************************
     F*** INJURY DESCRIPTION TABLE FILE                     ** DELETE
      *****  FCTITL1  IF  E           K        DISK
      *
     F*** PART OF BODY TABLE FILE                           ** ADD **
     FCTPBL     IF   E           K DISK
      ****** END   CHANGE ******* 11/11/97 ***************************
      *
     F*** DISABILITY DESCRIPTION TABEL FILE
     FCTDTL1    IF   E           K DISK
      *
     F*** BILLING TRANS FILE
     FCDBLL1    IF A E           K DISK
      *
     F*
      *** BLUE (MEMBER) CLAIM SCAN FORM (CLIENT)
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOA)
      *** YELLOW (NON-MEMBER) CLAIM SCAN FORM (CLIENT)
     FQSYSPRT1  O    F   80        PRINTER OFLIND(*INOB)
      *** ENVELOPE FOR CLIENT
     FQSYSPRT2  O    F   80        PRINTER OFLIND(*INOC)
      *** GREY (EMPLOYEE/ACCIDENT) FORM (ONE PER EMPLOYEE PER ACCIDENT)
     FQSYSPRT3  O    F   80        PRINTER OFLIND(*INOF)

     D*
     D                 DS
     D  CLNAME                 1     20
     D  CLFRST                 1      7
     D  CLBLK1                 8      8
     D  CLMI                   9      9
     D  CLBLK2                10     10
     D  CLLAST                11     20
     D*
     D                 DS
     D  CSNAME                 1     20
     D  CSFRST                 1      7
     D  CSBLK1                 8      8
     D  CSMI                   9      9
     D  CSBLK2                10     10
     D  CSLAST                11     20
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPCL02
     I              CLCLT#                      C2CLT#
     I              CLXREF                      C2XREF
     I              CLIDT                       C2IDT
     I              CLITMS                      C2ITMS
     I              CLFRST                      C2FRST
     I              CLMI                        C2MI
     I              CLLAST                      C2LAST
     I              CLSS#                       C2SS#

     C*
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
     C     CTKEY         KLIST
     C                   KFLD                    CLCLT#
     C*
     C     BLKEY         KLIST
     C                   KFLD                    CLCLT#
     C                   KFLD                    ##SEQ#            4 0
     C*
     C     CSKEY         KLIST
     C                   KFLD                    CLXREF
     C*
     C     CRKEY         KLIST
     C                   KFLD                    CSCARR
     C*
     C******* BEGIN CHANGE ******* 11/11/97 *********** PART 2 & 3 ***
     C*********  ITKEY     KLIST
     C*********            KFLD           CSITYP
     C     PBKY          KLIST
     C                   KFLD                    CSPART
     C******* END   CHANGE ******* 11/11/97 *********** PART 2 & 3 ***
     C*
     C     DTKEY         KLIST
     C                   KFLD                    CSDTYP
     C*
     C******** INITIALIZATION
     C                   MOVE      *BLANKS       CLBLK1
     C                   MOVE      *BLANKS       CLBLK2
     C                   MOVE      *BLANKS       CSBLK1
     C                   MOVE      *BLANKS       CSBLK2
     C*
     C                   TIME                    DSDTTM
     C*
     C*
      * * * * * *  F I R S T   R E C O R D  * * * * * * * *
     C*** GET CLIENT INFORMATION FROM EXISTING DATA BASE (1ST TIME)
     C*** GET ANY ADDITIONAL INFORMATION NEEDED TO PRINT FORM (1ST TIME)
     C*** DETERMINE WHICH SUMMARY FORM TO PRINT(MBR/NON-MBR) (1ST TIME)
     C*
     C  N01              EXSR      FIRST
     C*
     C*
      * * * * * *  D E T A I L   C Y C L E  * * * * * * * *
     C*** PRINT SINGLE DETAIL LINE ON SUMMARY FORM FOR EACH CL RCD
      *   READ
     C*** GET ANY NEEDED INFO TO WRITE DETAIL FORM(GREY)
     C*** PRINT DETAIL FORM(ONE PER SELECTION(CS RCD)) (PER CL RCD)
     C*
     C                   EXSR      DETAIL
     C*
     C*
      * * * * * *  L A S T   R E C O R D    * * * * * * * *
     C*** COMPLETE PRINT OF SUMMARY FORM (LAST RECORD)
     C*** PRINT ENVELOPE (LAST RECORD)
     C*** UPDATE BILLING RECORD (LAST RECORD)
     C*
     CLR                 EXSR      LSTRCD
     C/EJECT
      *=====================================================================*
     C     FIRST         BEGSR
      *=====================================================================*
     C*
     C                   MOVE      '1'           *IN01
     C                   Z-ADD     0             #MAIL
     C                   Z-ADD     0             #TELE
     C                   Z-ADD     0             #FAX
     C                   Z-ADD     9999          ##SEQ#
     C*
     C     *LOVAL        SETLL     PCL02
     C                   READ      PCL02                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     CLMTF         IFEQ      'F'
     C                   ADD       1             #FAX              5 0
     C                   ELSE
     C     CLMTF         IFEQ      'T'
     C                   ADD       1             #TELE             5 0
     C                   ELSE
     C                   ADD       1             #MAIL             5 0
     C                   END
     C                   END
     C*
     C                   READ      PCL02                                  60
     C                   END                                                     DOWEQ'0'
     C*
     C*** GET CLIENT INFORMATION FROM EXISTING DATA BASE
     C     CTKEY         CHAIN     PCT01                              60
     C*
     C*** DETERMINE IF "MEMBER TYPE",  THIS DETERMINES WHICH PRTF TO USE
     C     CTMBR         IFEQ      'M'
     C                   MOVE      '1'           *IN05                               MEMBER
     C                   ELSE
     C                   MOVE      '0'           *IN05                               NON-MEMBER
     C                   END
     C*
     C     CTFND         IFEQ      830
     C                   MOVE      '1'           *IN11
     C                   END
     C*
     C     CTFND         IFEQ      520
     C                   MOVE      '1'           *IN12
     C                   END
     C*
     C     CTFND         IFEQ      520
     C   13
     CANN13              MOVE      '1'           *IN13                              ???SET ON WHEN
     C                   END
     C*
     C     CTFND         IFGT      *ZEROS
     C     CTMBR#        ORNE      *ZERO
     C                   MOVE      '1'           *IN14
     C                   END
     C*
     C*
     C                   Z-ADD     CLIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         #IDT              8 0
     C*
     C     CTZIP2        IFNE      *ZEROS
     C                   MOVE      '1'           *IN08                               EXTENDED ZIP
     C                   END
     C*
     C***  PRINT HEADING STUFF-SUMMARY(CLIENT FORM)
     C                   EXSR      SUMHED
     C*** PRINT ENVELOPE
     C                   EXSR      ENVEL
     C***
     C                   ENDSR
     C***
     C/EJECT
      *=====================================================================*
     C     DETAIL        BEGSR
      *=====================================================================*
     C*
     C***
     C***  CHECK OVERFLOW
     C     *INOA         IFEQ      '1'
     C     *INOB         OREQ      '1'
     C                   EXSR      SUMHED
     C                   MOVE      '0'           *INOA
     C                   MOVE      '0'           *INOB
     C                   END
     C*
     C*** 06/09/20 ************ Beg Add ******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CLSS#'       QQNAME           25
     C     CLSS#         PARM      CLSS#         QQVAL           100
     C*** 06/09/20 ************ End Add ******************************
     C***  EXCPT SINGLE DETAIL LINE
     C                   EXCEPT    SUMDTL
     C*
     C***  IF THERE ARE ANY RCDS FOUND IN SEARCH FOR THIS EMPLOYEE, THEN
     C*** WE MUST PRINT A GREY FORM FOR EACH ACCIDENT SELECTED ON SEARCH.
     C     CLITMS        IFGT      0
     C                   EXSR      EMPDTL
     C                   END
     C***
     C                   ENDSR
     C***
     C/EJECT
      *=====================================================================*
     C     LSTRCD        BEGSR
      *=====================================================================*
     C*
     C*** PRINT FOOTER FOR CLIENT FORMS
     C                   EXCEPT    SUMFOT
     C*
     C*** MOVE FIELDS INTO SRW1TRAN FIELDS ***
     C*
     C* GET LAST SEQ IF A RCD FOR THIS CLIENT ALREADY EXISTS
     C     CTKEY         SETGT     PBL01
     C                   READP     PBL01                                  60
     C     *IN60         IFEQ      '0'
     C     BLCLT#        ANDEQ     CLCLT#
     C                   ADD       1             BLSEQ#
     C                   ELSE
     C                   Z-ADD     1             BLSEQ#
     C                   END
     C*
     C                   Z-ADD     CLCLT#        BLCLT#
     C                   Z-ADD     #MAIL         BL#M
     C                   Z-ADD     #TELE         BL#T
     C                   Z-ADD     #FAX          BL#F
     C*
     C                   Z-ADD     CLIDT         BLSRDT
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BLDT
     C                   Z-ADD     DSTIME        BLTIME
     C                   MOVEL     Q1PGM         BLPGM
     C                   MOVEL     Q1USER        BLUSER
     C                   MOVEL     Q1USER        BLIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BLIDT
     C                   MOVEL     'A'           BLSTAT
     C*
     C*
     C*** WRITE RECORD
     C                   WRITE     PBL01
     C*
     C                   ENDSR
     C/EJECT
      *=====================================================================*
     C     SUMHED        BEGSR
      *=====================================================================*
     C*
     C*** PRINT SUMMARY FORM'S HEADINGS
     C                   EXCEPT    TOPSUM
     C*
     C                   ENDSR
     C/EJECT
      *=====================================================================*
     C     ENVEL         BEGSR
      *=====================================================================*
     C*
     C*** PRINT AN ENVELOPE FOR THIS CLIENT
     C*
     C                   EXCEPT    ENVELO
     C*
     C                   ENDSR
     C/EJECT
      *=====================================================================*
     C     EMPDTL        BEGSR
      *=====================================================================*
     C*
     C*** LOOP THROUGH CS FILE GETTING INFO TO PRINT GREY FORM 1 PER CS RCD
     C     CSKEY         CHAIN     PCS01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*** GET ALL NEEDED INFO
     C*
     C                   MOVEL     *BLANK        #CRNAM           40
     C******* BEGIN CHANGE ******* 11/11/97 ************ PART 2 & 3 **
     C**********           MOVEL*BLANK    #ITDSC 20
     C                   MOVEL     *BLANK        #CPART            2
     C                   MOVEL     *BLANK        #CDESC           40
     C*** 06/09/20 ************ Beg Add ******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CSSS#'       QQNAME           25
     C     CSSS#         PARM      CSSS#         QQVAL           100
     C*** 06/09/20 ************ End Add ******************************
     C                   MOVEL     CSSS#         XCSS#             9 0
     C******* END CHANGE ********* 11/11/97 ************ PART 2 & 3 **
     C                   MOVEL     *BLANK        #DTDSC           20
     C*
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CRNAME        #CRNAM           40
     C                   ELSE
     C                   MOVEL     CSCARR        #CRNAM
     C                   END
     C*
     C******* BEGIN DELETE ******* 11/11/97 *********** PART 2 *******
     C*********  ITKEY     CHAINPIT01                60
     C*********  *IN60     IFEQ '0'
     C*********            MOVELITDESC    #ITDSC
     C*********            ELSE
     C*********            MOVELCSITYP    #ITDSC
     C*********            END
     C******* END   DELETE ******* 11/11/97 *********** PART 2 *******
     C*
     C******* BEGIN ADD ********** 11/11/97 *********** PART 3 *******
     C     PBKY          CHAIN     PPB01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PBDESC        #CDESC
     C                   MOVEL     PBPART        #CPART
     C                   ELSE
     C                   MOVEL     *BLANKS       #CDESC
     C                   MOVEL     CSPART        #CPART
     C                   END
     C******* END ADD ************ 11/11/97 *********** PART 3 *******
     C*
     C*
     C     DTKEY         CHAIN     PDT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     DTDESC        #DTDSC
     C                   ELSE
     C                   MOVEL     CSDTYP        #DTDSC
     C                   END
     C*** DETERMINE IF MED ONLY, OR WORKERS' COMP PAID???
     C***  MED ONLY SET ON 15
     C*** WORKERS' COMP SET ON 16
     C***
     C                   MOVEA     '00'          *IN(15)
     C******* BEGIN DELETE ******* 11/11/97 *********** PART 4 *******
     C*********  CSDLST    IFNE 0
     C*********            MOVE '1'       *IN16
     C*********            ELSE
     C*********            MOVE '1'       *IN15
     C*********            END
     C******* END   DELETE ******* 11/11/97 *********** PART 4 *******
     C*
     C                   Z-ADD     CSBTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         #BTDT             8 0
     C*
     C*** BEGIN CHG *** 02/10/98 **************************************
     C                   Z-ADD     CSACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*** END   CHG *** 02/10/98 **************************************
     C*
     C*** PRINT THE PAGE
     C                   EXCEPT    GREY
     C*
     C*** GET NEXT ONE IF ONE EXISTS
     C*
     C     CSKEY         READE     PCS01                                  60
     C                   END                                                    DOWEQ'0'
     C*
     C                   ENDSR
     C/EJECT
     O*** BLUE FORM (CLIENT) ***************************************************
     O*** BEGIN CHG *** 09/22/94 **************************************
     OQSYSPRT   E    05      TOPSUM           17
     O                       CTCONT              66
     O          E    05      TOPSUM           19
     O                       CTNAME              66
     O                       #IDT          Y     75
     O          E    05      TOPSUM           20
     O                       #MAIL         Z     75
     O          E    05      TOPSUM           21
     O                       CTADR1              66
     O                       #TELE         Z     75
     O          E    05      TOPSUM           22
     O                       #FAX          Z     75
     O          E    05      TOPSUM           23
     O                       CTCTY               52
     O                       CTST                55
     O                       CTZIP1              61
     O               08                          62 '-'
     O               08      CTZIP2              66
     O          E    05      TOPSUM           32
     O          E    05      SUMDTL         2
     O                       CLNAME              31
     O                       CLSS#               56
     O                       CLITMS        3     77
     O          E    11 05   SUMFOT           54
     O                                           18 'X'
     O                       CTMBR#              59
     O          E    12 05   SUMFOT           55
     O                                           18 'X'
     O                       CTMBR#              69
     O          E    13 05   SUMFOT           56
     O                                           18 'X'
     O/SPACE 3
     O*** YELLOW FORM (NON-MEMBER) *********************************************
     OQSYSPRT1  E   N05      TOPSUM           17
     O                       CTCONT              66
     O          E   N05      TOPSUM           19
     O                       CTNAME              66
     O                       #IDT          Y     75
     O          E   N05      TOPSUM           20
     O                       #MAIL         Z     75
     O          E   N05      TOPSUM           21
     O                       CTADR1              66
     O                       #TELE         Z     75
     O          E   N05      TOPSUM           22
     O                       #FAX          Z     75
     O          E   N05      TOPSUM           23
     O                       CTCTY               51
     O                       CTST                54
     O                       CTZIP1              60
     O               08                          61 '-'
     O               08      CTZIP2              65
     O          E   N05      TOPSUM           32
     O          E   N05      SUMDTL         2
     O                       CLNAME              31
     O                       CLSS#               56
     O                       CLITMS        3     77
     O/SPACE 3
     O*** ENVELOPE *************************************************************
     OQSYSPRT2  E            ENVELO           12
     O                       CTCONT              76
     O          E            ENVELO           13
     O                       CTNAME              76
     O          E            ENVELO           14
     O                       CTADR1              76
     O          E            ENVELO           15
     O                       CTCTY               66
     O                       CTST                69
     O                       CTZIP1              75
     O               08                          76 '-'
     O               08      CTZIP2              80
     O          E            ENVELO           19
     O                       CLCLT#              06
     O          E    14      ENVELO           20
     O                       CTFND               06
     O                       CTMBR#              16
     O/SPACE 3
     O*** GREY FORM (EMPLOYEE/ACCIDENT) ****************************************
     OQSYSPRT3  E            GREY             11
     O                       CSNAME              35
     O          E            GREY             13
     O                       XCSS#               39 '   -  -    '
     O          E            GREY             15
     O*** BEGIN CHG *** 02/10/98 **************************************
     O**********               CSACDT    39 '  /  /    '
     O                       XXACDT              39 '  /  /    '
     O*** END   CHG *** 02/10/98 **************************************
     O          E            GREY             18
     O                       CSEMNM              56
     O          E            GREY             20
     O                       CSEMP#        Z     39
     O          E            GREY             22
     O                       #CRNAM              75
     O          E            GREY             25
     C******* BEGIN CHANGE ******* 11/11/97 *********** PART 2 & 3 ***
     O**********               #ITDSC    32
     O                       #CPART              15
     O*                        #CDESC    45
     C******* END   CHANGE ******* 11/11/97 *********** PART 2 & 3 ***
     O          E            GREY             27
     O                       #BTDT         Y     39
     O          E            GREY             29
     O                       CSAREA              39
     O          E            GREY             32
     O                       #IDT          Y     39
     O          E    15      GREY             35
     O                                           28 'X'
     O          E    16      GREY             37
     O                                           28 'X'
     O          E            GREY             39
     C******* BEGIN DELETE ******* 11/11/97 *********** PART 4 *******
     O*                        CSDLST    39
     C******* END   DELETE ******* 11/11/97 *********** PART 4 *******
     O          E            GREY             42
     O                       #DTDSC              41
     O*
