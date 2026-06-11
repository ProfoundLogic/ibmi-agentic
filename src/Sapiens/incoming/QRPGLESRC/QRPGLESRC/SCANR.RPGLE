     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
      *                                                                 *
      *============ SCANR ======== GET NEXT IMAGE ID TO USE ============*
      *                                                                 *
      *****************************************************************
      *
      *
     H/TITLE SCANR - GET NEXT IMAGE ID TO USE ***
      *
      *
      *****************************************************************
      * REVISIONS:
      *
      * 01/04/00 C:113 PROJECT: SUPPORT #21218
      *                Corrected date format conversion.
      *
      *****************************************************************
      * INDICATOR USAGE
      * ALL/DIFFERENT SCREENS AND FUNCTIONS:
      *  60      GENERAL USE INDICATOR                                      PGM
      *
      *****************************************************************
      *
     H/EJECT
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
     FNXTDOC    IF A E           K DISK
      *
     F/EJECT
      *****************************************************************
      * EXTENTION SPECIFICATIONS * * *
      *****************************************************************
      *

     D TVL             S             12    DIM(10)
     D/EJECT
      *****************************************************************
      * INPUT SPECIFICATIONS  * * *
      *****************************************************************
      *
     D*
     D****************  SYSTEM PARAMETERS ***************
     D*
     D** SIZE OF THE DOCUMENT IN BYTES.
     D DOCSZ           DS
     D  DOCSFC                 1      4
     D  DOCSLN                 5      8P 0
     D  DOCSVL                 9     15  0
     D*
     D** MAXIMUM NUMBER OF OBJECTS TO RETURN, OR ACTUAL NUMBER RETURNED
     D NUMOB           DS
     D  NUMOFC                 1      4
     D  NUMOLN                 5      8P 0
     D  NUMOVL                 9     10  0
     D*
     D** NUMBER OF PAGES IN THE DOCUMENT BEING SCANNED.
     D NUMPG           DS
     D  NUMPFC                 1      4
     D  NUMPLN                 5      8P 0
     D  NUMPVL                 9     11  0
     D*
     D** RETURN AREA FOR ALL ERROR CODES.
     D RETAR           DS
     D  RETAFC                 1      4
     D  RETALN                 5      8P 0
     D  RETACD                 9     14  0
     D  RETARC                15     20
     D  RETALF                21     21
     D*
     D** TERMINAL ID TO PRINT ON THE SEPARATOR PAGE.
     D TRMID           DS
     D  TERMFC                 1      4
     D  TERMLN                 5      8P 0
     D  TERMVL                 9     16
     D*
     D** USERID FOR VARIOUS COMMANDS.
     D USRID           DS
     D  USIDFC                 1      4
     D  USIDLN                 5      8P 0
     D  USIDVL                 9     16
     D*
     D****************  DOCUMENT-RELATED PARAMETERS ***************
     D*
     D** ALPHABETIC CHARACTER INDEX OPTION (SPECIFIES WHETHER
     D** ALPHABETIC CHARACTERS ARE ALLOWED IN THE INDEX VALUE).
     D ALPHA           DS
     D  ALPHFC                 1      4
     D  ALPHLN                 5      8P 0
     D  ALPHVL                 9      9
     D*
     D** CHECK DIGIT ROUTINE OPTION (SPECIFIES WHETHER A CHECK DIGIT
     D** ROUTINE IS INSTALLED IN THE WRKSTN TO VALIDATE THE INDEX).
     D CHKDG           DS
     D  CHKDFC                 1      4
     D  CHKDLN                 5      8P 0
     D  CHKDVL                 9      9
     D*
     D** SPECIFIES THE DELETE OPTION AFTER STORING A DOCUMENT.
     D DELFG           DS
     D  DELFFC                 1      4
     D  DELFLN                 5      8P 0
     D  DELFVL                 9      9
     D*
     D** DESCRIPTION FIELD FROM THE DOCUMENT RECORD.
     D DOCDS           DS
     D  DOCDFC                 1      4
     D  DOCDLN                 5      8P 0
     D  DOCDVL                 9     48
     D*
     D** UNIQUE IDENTIFIER OF A DOCUMENT.
     D DOCID           DS
     D  DOCIFC                 1      4
     D  DOCILN                 5      8P 0
     D  DOCIVL                 9     20
     D*
     D** TABLE OF DOCUMENTS USED IN VARIOUS COMMANDS.
     D DOCTB           DS
     D  DOCTFC                 1      4
     D  DOCTLN                 5      8P 0
     D  DOCTVL                 9    128
     D*
     D** WAF DOCUMENT TYPE VALUE.
     D DOCTY           DS
     D  DOCYFC                 1      4
     D  DOCYLN                 5      8P 0
     D  DOCYVL                 9     16
     D*
     D** END OF PRINT PARAMETER.
     D ENDPT           DS
     D  ENDPFC                 1      4
     D  ENDPLN                 5      8P 0
     D  ENDPVL                 9      9
     D*
     D** CASE DESCRIPTION.
     D FLDDS           DS
     D  FLDDFC                 1      4
     D  FLDDLN                 5      8P 0
     D  FLDDVL                 9     34
     D*
     D** ID OF THE CASE BEING USED.
     D FLDID           DS
     D  FLDIFC                 1      4
     D  FLDILN                 5      8P 0
     D  FLDIVL                 9     17
     D*
     D** FOLDER PATH FROM WHICH FORMS ARE RETRIEVED.
     D FORMP           DS
     D  FORMFC                 1      4
     D  FORMLN                 5      8P 0
     D  FORMVL                 9     52
     D*
     D** EXTERNAL CASE IDENTIFIER.
     D IDENT           DS
     D  IDENFC                 1      4
     D  IDENLN                 5      8P 0
     D  IDENVL                 9     48
     D*
     D** INDEX VALUE KEYING OPTION (SPECIFIES WHETHER AN INDEX IS KEYED
     D** WITH THE IMAGE BEING SCANNED).
     D INDEX           DS
     D  INDXFC                 1      4
     D  INDXLN                 5      8P 0
     D  INDXVL                 9      9
     D*
     D** MAXIMUM LENGTH OF THE INDEX FIELD.
     D IDMAX           DS
     D  IDMXFC                 1      4
     D  IDMXLN                 5      8P 0
     D  IDMXVL                 9     10  0
     D*
     D** MINIMUM LENGTH OF THE INDEX FIELD.
     D IDMIN           DS
     D  IDMNFC                 1      4
     D  IDMNLN                 5      8P 0
     D  IDMNVL                 9     10  0
     D*
     D** INDEX VALUE RETURNED WITH THE SCANNED IMAGE.
     D IDXIG           DS
     D  IDXIFC                 1      4
     D  IDXILN                 5      8P 0
     D  IDXIVL                 9     58
     D*
     D** TABLE OF KEYSTROKE STRINGS TO ENTER ON SESSION 2.
     D KEYTB           DS
     D  KEYTFC                 1      4
     D  KEYTLN                 5      8P 0
     D  KEYR01                 9     10  0
     D  KEYC01                11     12  0
     D  KEYL01                13     14  0
     D  KEYS01                15     94
     D  KEYR02                95     96  0
     D  KEYC02                97     98  0
     D  KEYL02                99    100  0
     D  KEYS02               101    180
     D  KEYR03               181    182  0
     D  KEYC03               183    184  0
     D  KEYL03               185    186  0
     D  KEYS03               187    266
     D  KEYR04               267    268  0
     D  KEYC04               269    270  0
     D  KEYL04               271    272  0
     D  KEYS04               273    352
     D  KEYR05               353    354  0
     D  KEYC05               355    356  0
     D  KEYL05               357    358  0
     D  KEYS05               359    438
     D  KEYR06               439    440  0
     D  KEYC06               441    442  0
     D  KEYL06               443    444  0
     D  KEYS06               445    524
     D  KEYR07               525    526  0
     D  KEYC07               527    528  0
     D  KEYL07               529    530  0
     D  KEYS07               531    610
     D  KEYR08               611    612  0
     D  KEYC08               613    614  0
     D  KEYL08               615    616  0
     D  KEYS08               617    696
     D  KEYR09               697    698  0
     D  KEYC09               699    700  0
     D  KEYL09               701    702  0
     D  KEYS09               703    782
     D  KEYR10               783    784  0
     D  KEYC10               785    786  0
     D  KEYL10               787    788  0
     D  KEYS10               789    868
     D  KTV                    9    868
     D                                     DIM(10)
     D*
     D** PRIORITY FIELD FOR STORE AND RETREIVE REQUESTS
     D PRIOR           DS
     D  PRIRFC                 1      4
     D  PRIRLN                 5      8P 0
     D  PRIRVL                 9      9
     D*
     D** DEMAND PRINT QUEUE ID NAME
     D PRTQU           DS
     D  PRTQFC                 1      4
     D  PRTQLN                 5      8P 0
     D  PRTQVL                 9     18
     D*
     D** QUEUE IN WHICH NEW CASE IS PLACED
     D QUEID           DS
     D  QUIDFC                 1      4
     D  QUIDLN                 5      8P 0
     D  QUIDVL                 9     18
     D*
     D** PRIORITY OF CASE WHEN QUEUED
     D QUEPR           DS
     D  QUPRFC                 1      4
     D  QUPRLN                 5      8P 0
     D  QUPRVL                 9     21
     D*
     D** REJECT QUEUE INDICATOR (SPECIFIES IF THE DOCUMENT IS TO BE QUEUED IN
     D** REJECT QUEUE ONCE IT IS REMOVED FROM THE CASE)
     D REJCT           DS
     D  RJCTFC                 1      4
     D  RJCTLN                 5      8P 0
     D  RJCTVL                 9      9
     D*
     D** RESET TERMINAL INDICATOR (SPECIFIES IF THE TERMINAL SHOULD BE
     D** RESET BEFORE THE COMMAND IS EXECUTED)
     D RESET           DS
     D  RSETFC                 1      4
     D  RSETLN                 5      8P 0
     D  RSETVL                 9      9
     D*
     D** RETURN SESSION INDICATOR (SPECIFIES THE SESSION IN WHICH THE
     D** WORKSTATION IS LEFT AFTER THE COMMAND IS EXECUTED)
     D RETRN           DS
     D  RTRNFC                 1      4
     D  RTRNLN                 5      8P 0
     D  RTRNVL                 9      9
     D*
     D** SCAN OVERLAP SELECTOR (SPECIFIES WHETHER TO USE SCAN OVERLAP)
     D SCANO           DS
     D  SCANFC                 1      4
     D  SCANLN                 5      8P 0
     D  SCANVL                 9      9  0
     D*
     D** SCAN OVERLAP INDICATOR
     D SCANT           DS
     D  SCNTFC                 1      4
     D  SCNTLN                 5      8P 0
     D  SCNTVL                 9      9
     D*
     D** NAME OF THE AS/400 CASE IN WHICH THE OBJECT MUST BE PLACED
     D SFOLD           DS
     D  SFLDFC                 1      4
     D  SFLDLN                 5      8P 0
     D  SFLDVL                 9     16
     D*
     D** SPECIAL CHARACTER INDEX OPTION (SPECIFIES WHETHER SPECIAL
     D** CHARACTERS ARE ALLOWED IN THE INDEX VALUE)
     D SPECP           DS
     D  SPECFC                 1      4
     D  SPECLN                 5      8P 0
     D  SPECVL                 9      9
     D*
     D** NAME OF THE AS/400 SUBDIRECTORY IN WHICH THE OBJECT MUST BE PLACED
     D SUBDR           DS
     D  SUBDFC                 1      4
     D  SUBDLN                 5      8P 0
     D  SUBDVL                 9     20
     D*
     D** ONE CHARACTER SYSTEM ID CORRESPONDING TO THE OPTICAL DRIVE AS
     D** DEFINED IN THE OPTICAL CONTROL FILE
     D SYSID           DS
     D  SYSIFC                 1      4
     D  SYSILN                 5      8P 0
     D  SYSIVL                 9      9
     D*
     D** COMPATIBILITY MODE INDICATOR (SPECIFIES IF WORKFOLDER APPLICATION
     D** FACILITY COMPATIBILITY MODE SHOULD BR USED)
     D WAFUS           DS
     D  WAFUFC                 1      4
     D  WAFULN                 5      8P 0
     D  WAFUVL                 9      9
     D*
     D** DOCUMENT ID PARAMETER
     D QQDCID          DS
     D  PREFIX                 1      2
     D  SCNDAT                 3      8  0
     D  DOT                    9      9
     D  IMG#                  10     12  0
     D*
     D** DATE AND TIME FROM SYSTEM
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I/EJECT
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *

     C*** DEFINE FEILDS
     C*          *LIKE     DEFN AAAAAA    BBBBBB
     C*
     C/EJECT
     C*** KEY LISTS
     C     NXKY          KLIST
     C                   KFLD                    PREFIX
     C                   KFLD                    SCNDAT
     C                   KFLD                    XXIMG#            3 0
     C*
     C     NXKY1         KLIST
     C                   KFLD                    PREFIX
     C                   KFLD                    SCNDAT
     C*
     C*** PLIST(S)
     C*
     C     SCNIMG        PLIST
     C                   PARM                    DOCDS
     C                   PARM                    IDXIG
     C                   PARM                    NUMPG
     C                   PARM                    DOCSZ
     C                   PARM                    DOCID
     C                   PARM                    SCANT
     C                   PARM                    RETAR
     C*
     C*
     C     SETIMG        PLIST
     C                   PARM                    FORMP
     C                   PARM                    SCANO
     C                   PARM                    CHKDG
     C                   PARM                    ALPHA
     C                   PARM                    SPECP
     C                   PARM                    INDEX
     C                   PARM                    RESET
     C                   PARM                    IDMAX
     C                   PARM                    IDMIN
     C                   PARM                    RETAR
     C*
     C/EJECT
      *****************************************************************
      *  MAINLINE CODE  *
      *****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12
     C                   PARM                    QQDESC           40
     C                   PARM                    QQRTCD            6
     C                   PARM                    QQ#PGS            3 0
     C                   PARM                    QQDCSZ            7 0
     C*
      *** BEGIN ADD *** 01/04/00 ***********************************************
     C     DTYMD         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*YMD'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *** END   ADD *** 01/04/00 ***********************************************
     C                   TIME                    DSDTTM
     C**
     C                   MOVE      'PC'          PREFIX
     C                   MOVE      '.'           DOT
      *** BEGIN CHG *** 01/04/00 ***********************************************
     C********** DSDATE    MULT 10000.01  SCNDAT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'ISCVTDT'     DTYMD
     C                   MOVEL     SRDTO         SCNDAT
      *** END   CHG *** 01/04/00 ***********************************************
     C                   Z-ADD     *HIVAL        XXIMG#
     C*
     C*** THIS IS NOT EXECUTED IN THE PC VERSION OF CODE ****
     C  N60
     CAN 60              EXSR      WAFINT
     C*
     C***** GET THE NEXT DOC ID HERE AND PASS BACK TO THE CALLING PGM
     C*
     C     NXKY          SETGT     PNX01
     C     NXKY1         READPE    PNX01                                  60
     C*
     C     *IN60         IFEQ      '0'
     C     NXIMG#        ADD       1             IMG#
     C                   ELSE
     C                   Z-ADD     1             IMG#
     C                   END
     C*
     C                   Z-ADD     IMG#          NXIMG#
     C                   Z-ADD     SCNDAT        NXDATE
     C                   MOVEL     PREFIX        NXPREF
     C                   MOVEL     Q1USER        NXUSER
     C                   Z-ADD     DSTIME        NXTIME
     C                   Z-ADD     NXDATE        NXDT
     C                   MOVEL     Q1PGM         NXPGM
     C                   MOVEL     Q1USER        NXUSER
     C*
     C                   WRITE     PNX01
     C*
     C**** THE PC VERSION WILL GOTO TERM HERE ***
     C                   GOTO      TERM
     C********************************************************************
     C                   MOVEL     QQDESC        DOCDVL
     C                   CALL      'SCAN'        SCNIMG
     C*
     C* * * PASS THE RETURN CODE BACK TO CALLING PROGRAM
     C* * * *ZEROS IS A VALID DISPLAY OF IMAGE HAS OCCURED
     C*
     C                   MOVE      RETACD        QQRTCD
     C*
     C                   MOVE      NUMPVL        QQ#PGS
     C                   MOVE      DOCSVL        QQDCSZ
     C                   MOVEL     DOCIVL        QQDCID
     C*
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*
     C/EJECT
      *================================================================*
      *    WAFINT - INITIALIZE THE WAF PARM VALUES -       W A F I N T *
      *================================================================*
     C*
     C* THIS SUBROUTINE SETS ON THE BIT CONFIGURATION
     C*  FOR THE RESET AND LAST FLAG OPTION.
     CSR   WAFINT        BEGSR
     C*
     C**************************
     C*  INITIALIZE WAF PARMS  *
     C**************************
     C*
     C                   BITOFF    '01234567'    HEX00             1
     C                   BITOFF    '01234567'    HEX01             1
     C                   BITON     '7'           HEX01
     C*
     C**  SYSTEM PARAMETERS
     C*
     C                   MOVE      '0001'        USIDFC
     C                   Z-ADD     8             USIDLN
     C                   MOVE      *BLANKS       USIDVL
     C*
     C                   MOVE      '0002'        RETAFC
     C                   Z-ADD     13            RETALN
     C                   MOVE      *ZEROS        RETACD
     C                   MOVE      *ZEROS        RETARC
     C                   MOVE      HEX00         RETALF
     C*
     C                   MOVE      '0003'        DOCSFC
     C                   Z-ADD     7             DOCSLN
     C                   Z-ADD     0             DOCSVL
     C*
     C                   MOVE      '0004'        NUMPFC
     C                   Z-ADD     3             NUMPLN
     C                   Z-ADD     0             NUMPVL
     C*
     C                   MOVE      '0005'        TERMFC
     C                   Z-ADD     8             TERMLN
     C                   MOVE      *BLANKS       TERMVL
     C*
     C                   MOVE      '0006'        NUMOFC
     C                   Z-ADD     2             NUMOLN
     C                   MOVE      *BLANKS       NUMOVL
     C*
     C** DOCUMENT PARAMETERS
     C*
     C                   MOVE      '2001'        DOCIFC
     C                   Z-ADD     12            DOCILN
     C                   MOVE      *BLANKS       DOCIVL
     C*
     C                   MOVE      '2002'        DOCDFC
     C                   Z-ADD     40            DOCDLN
     C                   MOVE      *BLANKS       DOCDVL
     C*
     C                   MOVE      '2003'        DOCTFC
     C                   Z-ADD     120           DOCTLN
     C                   MOVEA     *ALL' '       TVL
     C*
     C*** ??? ****         MOVE '2004'    DOCYFC
     C                   MOVE      '2003'        DOCYFC
     C                   Z-ADD     8             DOCYLN
     C                   MOVE      *BLANKS       DOCYVL
     C*
     C                   MOVE      '2009'        SYSIFC
     C                   Z-ADD     1             SYSILN
     C                   MOVE      *BLANKS       SYSIVL
     C*
     C                   MOVE      '2010'        SFLDFC
     C                   Z-ADD     8             SFLDLN
     C                   MOVE      *BLANKS       SFLDVL
     C*
     C                   MOVE      '2011'        SUBDFC
     C                   Z-ADD     12            SUBDLN
     C                   MOVE      *BLANKS       SUBDVL
     C*
     C*  AS/400 AND WAF PARAMETERS
     C*
     C                   MOVE      '0701'        RSETFC
     C                   Z-ADD     1             RSETLN
     C                   MOVE      HEX01         RSETVL
     C*
     C                   MOVE      '0702'        RTRNFC
     C                   Z-ADD     1             RTRNLN
     C                   MOVE      HEX00         RTRNVL
     C*
     C                   MOVE      '0703'        RJCTFC
     C                   Z-ADD     1             RJCTLN
     C                   MOVE      HEX00         RJCTVL
     C*
     C                   MOVE      '0704'        WAFUFC
     C                   Z-ADD     1             WAFULN
     C                   MOVE      HEX00         WAFUVL
     C*
     C                   MOVE      '0705'        SCANFC
     C                   Z-ADD     1             SCANLN
     C                   MOVE      HEX00         SCANVL
     C*
     C                   MOVE      '0706'        CHKDFC
     C                   Z-ADD     1             CHKDLN
     C                   MOVE      HEX00         CHKDVL
     C*
     C                   MOVE      '0707'        ALPHFC
     C                   Z-ADD     1             ALPHLN
     C                   MOVE      HEX00         ALPHVL
     C*
     C                   MOVE      '0708'        SPECFC
     C                   Z-ADD     1             SPECLN
     C                   MOVE      HEX00         SPECVL
     C*
     C                   MOVE      '0709'        INDXFC
     C                   Z-ADD     1             INDXLN
     C                   MOVE      HEX00         INDXVL
     C*
     C                   MOVE      '0710'        IDMXFC
     C                   Z-ADD     2             IDMXLN
     C                   Z-ADD     0             IDMXVL
     C*
     C                   MOVE      '0711'        IDMNFC
     C                   Z-ADD     2             IDMNLN
     C                   Z-ADD     0             IDMNVL
     C*
     C                   MOVE      '0712'        IDXIFC
     C                   Z-ADD     1             IDXILN
     C                   MOVE      *BLANKS       IDXIVL
     C*
     C                   MOVE      '0713'        KEYTFC
     C                   Z-ADD     860           KEYTLN
     C                   MOVE      *ALL' '       KTV
     C*
     C                   MOVE      '0714'        PRTQFC
     C                   Z-ADD     10            PRTQLN
     C                   MOVE      *BLANKS       PRTQVL
     C*
     C                   MOVE      '0715'        PRIRFC
     C                   Z-ADD     1             PRIRLN
     C                   MOVE      *BLANK        PRIRVL
     C*
     C                   MOVE      '0716'        DELFFC
     C                   Z-ADD     1             DELFLN
     C                   MOVE      HEX00         DELFVL
     C*
     C                   MOVE      '0717'        FORMFC
     C                   Z-ADD     44            FORMLN
     C                   MOVE      *BLANKS       FORMVL
     C*
     C                   MOVE      '0718'        SCNTFC
     C                   Z-ADD     1             SCNTLN
     C                   MOVE      HEX00         SCNTVL
     C*
     C                   MOVE      '0719'        ENDPFC
     C                   Z-ADD     1             ENDPLN
     C                   MOVE      HEX00         ENDPVL
     C*
     C**  FOLDER PARAMETERS
     C*
     C                   MOVE      '1001'        FLDIFC
     C                   Z-ADD     9             FLDILN
     C                   MOVE      *BLANKS       FLDIVL
     C*
     C                   MOVE      '1002'        FLDDFC
     C                   Z-ADD     26            FLDDLN
     C                   MOVE      *BLANKS       FLDDVL
     C*
     C                   MOVE      '1003'        IDENFC
     C                   Z-ADD     40            IDENLN
     C                   MOVE      *BLANKS       IDENVL
     C*
     C                   MOVE      '3001'        QUIDFC
     C                   Z-ADD     10            QUIDLN
     C                   MOVE      *BLANKS       QUIDVL
     C*
     C                   MOVE      '3002'        QUPRFC
     C                   Z-ADD     7             QUPRLN
     C                   MOVE      *BLANKS       QUPRVL
     C*
     C                   ENDSR
     C*****************************************************************
