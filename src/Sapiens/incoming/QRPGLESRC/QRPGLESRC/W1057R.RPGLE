     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
     H/TITLE W1057R - Work With Owners/Officers
     H Bnddir('WTBNDDIR')
      *===============================================================*
      * DESCRIPTION:  This program will allow a user to change, view  *
      *               or add records to the Owner/Officer file.       *
      *                                                                 *
      * CALLED FROM:                                                  *
      *   W0050R - Work With Employers                                *
      *   W0076R - Work With Entity/Locations                         *
      *                                                               *
      * CALLS TO:                                                     *
      *   W1057R02 - Owner/Officer Information                        *
      *   W1057R01 - % Errors                                         *
      *                                                               *
      * CREATED: 04/19/97  PROGRAMMER: C:33  PROJECT: 60AND           *
      * REWRITE: 10/21/99  PROGRAMMER: C:113 PROJECT: CM027           *
      *===============================================================*
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                ADD PROCESSING TO POPULATE EXTENDED EMPLR NAMES*
     H*                                                               *
     H* 02/14/02 C:09  PROJECT: CM027  (WA091/SEQ # 30555)            *
     H*                Changes made by C:90 on 12/15/00 should have   *
     H*                only been applied for ADDITION OF OWNER/OFFICER*
     H*                not for DELETION OF OWNER/OFFICER too.         *
     H*                Correct DELETION OF OWNER/OFFICER processing.  *
     H*                                                               *
     H* 01/22/02 C:09  PROJECT: CM027  (WA107/SEQ # 30152)            *
     H*                Need to be sure that we ignore owner/officer   *
     H*                records that were added under ENTITIES (i.e.   *
     H*                "E" types,Opt 10 on "Work with Employers").    *
     H*                The midterm endorsement process should be      *
     H*                ignoring these.                                *
     H*                                                               *
     H* 01/19/01 C:61  PROJECT WC577                                  *
     H*                Opt "10=Change Inc/Exc" from QUOTE not allowed.*
     H*                                                               *
     H* 12/15/00 C:90  PROJECT: #25353                                *
     H*                REMOVED CONDITIONING TO CHECK IF A NAME ALREADY*
     H*                EXISTS BEFORE PRINTING THE OWNER/OFFICER FORM. *
     H*                                                               *
     H* 11/23/99 C:113 PROJECT: SUPPORT #20804                        *
     H*                Corrected not picking up changing only the     *
     H*                owner/officers effective date.                 *
     H*                                                               *
     H* 08/19/99  C:35  SEQ#: WA071/19338                             *
     H*           C:33  CHANGE CODE SO THAT THE CANCELLATION DATE     *
     H*                 DISPLAYED ON THE SCREEN IS THE CX DATE        *
     H*                 ASSOCIATED WITH THE MOST RECENT EFFECTIVE     *
     H*                 DATE.  ALSO, THIS FIELD IS NOW BEING CLEARED  *
     H*                 SO THAT IT WON'T CARRY DATA FROM A PREVIOUSLY *
     H*                 DISPLAYED EMPLOYER IF THE CURRENT EMPLOYER    *
     H*                 IS NOT CANCELLED.  ALSO, CONTROL DISPLAY OF   *
     H*                 THIS FIELD WITH AN INDICATOR SO THAT IF THE   *
     H*                 EMPLOYER IS NOT CANCELLED, THE FIELD AND      *
     H*                 HEADING WILL NOT SHOW ON THE SCREEN.          *
     H*                                                               *
     H*  06/29/99 C:09 PROJECT: CM027                                 *
     H*                ALLOW "OPT 4=DELETE" AND "F6=ADD" ON A MIDTERM *
     H*                OWNER/OFFICER CHANGE.                          *
     H*                                                               *
     H*  06/29/99 C:33 PROJECT: WC577  PER: C:09                      *
     H*                EDIT OWNER/OFFICER INFOMATION IN PGMEND.       *
     H*                                                               *
     H*  06/18/99 C:09 PROJECT: CM044    (WA091/SEQ# 18522)           *
     H*                DO NOT PROCESS ENTITY OWNER/OFFICER CHANGES    *
     H*                FOR ENDORSEMENTS.                              *
     H*                                                               *
     H*  05/13/99 C:33 SEQ#: WA091/18083                              *
     H*                CLEAR THE ELID KEY FIELD UPON ENTRY INTO PGM   *
     H*                                                               *
     H*  04/13/99 C:09 PROJECT: CM027                                 *
     H*                MIDTERM OWNER/OFFICER ENDORSEMENT REWRITE      *
     H*                                                               *
     H*  04/02/99 C:33 SEQ#: WA999/15331                              *
     H*                ONLY BUILD THE # OF RECORDS THAT WILL FIT ON   *
     H*                ONE SCREEN.                                    *
     H*                                                               *
     H*  10/19/98 C:33 SEQ#: WA101/13875                              *
     H*                DO NOT CLEAR THE ELID KEY FIELD ON REFRESH     *
     H*                                                               *
     H*  08/07/98 C:78 SEQ#: DR095/12876                              *
     H*                ADD PARAMETER ##QTE WHEN CALLING W1057R02.     *
     H*                ALLOW ACCES TO DARTS. RECIEVED @@QTE IF BEING  *
     H*                PASSED ONTO THE PROGRAM.                       *
     H*                                                               *
     H*  01/22/98  C:33  PROJECT: WC577                               *
     H*                  USE DIFFERENT LOGICAL TO DEFINE A UNIQUE KEY *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
      * ------------------------------------------------------------- *
      *===============================================================*
      *
      ** Display File
     FW1057F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      ** Entity/Location Master File
     FWMALP     IF   E             DISK
      *
      ** DARTS Information File
     FWMDXL     IF   E           K DISK
      *
      ** Employer Detail File
     FWDELL     IF   E           K DISK
      *
      ** Employer Master File
     FWMEML     IF   E           K DISK
      *
      ** Employer Master File
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
      *
      ** Endorsement Master File
     FWMENL3    IF A E           K DISK
      *
      ** Group Detail File
     FWDF2L     IF   E           K DISK
      *
      ** Group Master File
     FWMFNL     IF   E           K DISK
      *
      ** Owner/Officer Master File
     FWMOOP     IF   E             DISK
     F                                     RENAME(POO01:PFRCDF)
      *
      ** Owner/Officer Master File
     FWMOOL     IF   E           K DISK
     F                                     RENAME(POO01:POO02)
      *
      ** Owner/Officer Master File
     FWMOOL6    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(POO01:DBRCDF)
      *
      ** Midterm Endorsement Table File
     FWTMYP     IF   E             DISK
      *================================================================
      *  Array & Table Definitions
      *================================================================
      *
      ** Work With arrays
      /COPY SCOPYWW01D
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
      ***  End   Add  ***  06/09/20  ****************************
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
      *
      ** BEFORE arrays
     D LN              S             30    DIM(500)                             Last name
     D FN              S             15    DIM(500)                             First name
     D NM              S             45    DIM(500)                             Both names
     D PC              S              5  2 DIM(500)                             %
     D IE              S              1    DIM(500)                             Include/Exclude
     D SQ              S              5  0 DIM(500)                             Sequence#
     D ST              S              1    DIM(500)                             Status
     D TL              S             30    DIM(500)                             Title
     D ED              S              7  0 DIM(500)                             Effective date
     D EX              S              7  0 DIM(500)                             Expiration date
     D XXBL            S             50    DIM(5)                               Before line
      *
      ** AFTER arrays
     D LN2             S             30    DIM(500)                             Last name
     D FN2             S             15    DIM(500)                             First name
     D NM2             S             45    DIM(500)                             Both names
     D PC2             S              5  2 DIM(500)                             %
     D IE2             S              1    DIM(500)                             Include/Exclude
     D SQ2             S              5  0 DIM(500)                             Sequence#
     D ST2             S              1    DIM(500)                             Status
     D TL2             S             30    DIM(500)                             Title
     D ED2             S              7  0 DIM(500)                             Effective date
     D EX2             S              7  0 DIM(500)                             Expiration date
     D XXAL            S             50    DIM(5)                               After line
      *
     D***PY SCOPYWW01E
      *================================================================
      *  Data Structures and Constants definitions
      *================================================================
     D***PY SCOPYWW01I
      *
      ** Effective Dates
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5)                               Effective dates
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
      *
      ** Cancellation Dates
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               Cancel dates
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *
      ** Midterm Information data structure
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
      *
      ** Info to display
     D                 DS
     D  XXLINE                 1     35
     D  XXDSPL                28     28
      *
      ** Date and time
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
      ** DARTS key
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
      *
      ** MM/DD/YYYY breakdown
     D                 DS
     D  XXDT8                  1      8
     D  XXMN                   1      2
     D  XXDY                   3      4
     D  XXYR                   5      8
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Owners/Officers')
     D DELETD          C                   CONST('*Deleted')
     D ACDPGM          C                   CONST('W1057R02')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D VIEWMX          C                   CONST(1)
     D INFOMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)
     D AGTAPI          C                   CONST('WA002R')
     D**** BEGIN ADD **** 02/14/02 ************************************
     D NEW             C                   CONST('** NEW **')

     I**** END   ADD **** 02/14/02 ************************************
      *================================================================
      *  PLIST definitions
      *================================================================
      *
      ** Entry Parameters

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ELFL            1
     C                   PARM                    @@QTE             1
      *
      ** Add/change/display program parameter list
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C                   PARM      @@ELFL        ##ELFL            1
      *
      ** Company file/fund (group) retrieve program parameter list
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      ** Agent/Producer retrieve program parameter list
     C     AGTPL         PLIST
     C                   PARM                    EMCO#
     C                   PARM                    EMFND
     C                   PARM                    EMEMP#
     C                   PARM                    EMDIV
     C                   PARM      LDFYR         ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
      *================================================================
      *  KLIST definitions
      *================================================================
      *
     C     KEY1          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKELID
      *
     C     KEY1A         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKELID
     C                   KFLD                    KKEXDT
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKOLSN
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITIM
      *
      ** Employer Master file
     C     EMKY          KLIST
     C                   KFLD                    ALCO#
     C                   KFLD                    ALFND
     C                   KFLD                    ALEMP#
     C                   KFLD                    ALDIV
      *
      ** Group Master file
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      *
      ** Owner/Officer file
     C     OOKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      *
      ** Endors/DARTS Control Definition file
     C     DXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      *
      ** Endorsement Request workfile
     C     ENKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
      *
      ** Group Auxiliary file
     C     F2KY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      *
      ** Employer Detail file
     C     ELKY          KLIST
     C                   KFLD                    ENCO#
     C                   KFLD                    ENFND
     C                   KFLD                    ENEMP#
     C                   KFLD                    ENDIV
     C                   KFLD                    ENFYR
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    SCFND         XXFND
     C     *LIKE         DEFINE    SCPRCT        TTLPCT
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    ALELID        KKELID
     C     *LIKE         DEFINE    OOEXDT        KKEXDT
     C     *LIKE         DEFINE    OOEFDT        KKEFDT
     C     *LIKE         DEFINE    OOOSS#        KKSS#
     C     *LIKE         DEFINE    OOOLSN        KKOLSN
     C     *LIKE         DEFINE    OOIDT         KKIDT
     C     *LIKE         DEFINE    OOITIM        KKITIM
     C     *LIKE         DEFINE    RSTPGM        ##ERR
     C                   MOVE      '/'           XXS               1
      *================================================================
      *  Main
      *================================================================
      *
     C/COPY SCOPYWW01C
      *
      *===============================================================
      * CHKSSM - SUBROUTINE FOR SUBSET MESSAGE
      *===============================================================
     C     CHKSSM        BEGSR
      *
      *
     C                   ENDSR                                                  CHKSSM
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     C     ERRCK1        BEGSR
      *
      *
     C                   ENDSR                                                  ERRCK1
      *===============================================================
      * ERRCK2 - SUBROUTINE TO CHECK SUBFILE SELECTIONS
      *===============================================================
     C     ERRCK2        BEGSR
      *
      *
     C                   ENDSR                                                  ERRCK2
      *===============================================================
      * KNDO - SUBROUTINE FOR F14
      *===============================================================
      *
      ** F14=DARTS
     C     KNDO          BEGSR
      *
      ** Establish key
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
     C                   MOVE      XXQTE         QCQTE
      *
      ** Call DARTS entry program
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
      *
     C                   ENDSR                                                  KNDO
      *===============================================================
      * INZSST - SUBROUTINE TO INITIALIZE SUBSET SELECTIONS
      *===============================================================
     C     INZSST        BEGSR
      *
      *
     C                   ENDSR                                                  INZSST
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     C     KEYINZ        BEGSR
      *
     C                   CLEAR                   KKEXDT
     C                   CLEAR                   KKEFDT
     C                   CLEAR                   KKSS#
     C                   CLEAR                   KKOLSN
     C                   CLEAR                   KKIDT
     C                   CLEAR                   KKITIM
      *
     C                   ENDSR                                                  KEYINZ
      *===============================================================
      * KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
      *===============================================================
     C     KEYSET        BEGSR
      *
      *
     C                   ENDSR                                                  KEYSET
      *===============================================================
      * KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
      *===============================================================
     C     KEYSFL        BEGSR
      *
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEXDT
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
      *
      *****  Beg Add  ***  06/09/20  ***********************
      /Free
       SCSS# = UnMaskData('SSN':'OOOSS#':OOOSS#);
      /End-Free
      *****  End Add  ***  06/09/20  ***********************
     C                   MOVE      SCSS#         KKSS#
     C                   MOVEL(P)  SCLNAM        KKOLSN
     C                   Z-ADD     SCIDT         KKIDT
     C                   Z-ADD     SCITIM        KKITIM
      *
     C                   ENDSR                                                  KEYSFL
      *===============================================================
      * KQDO - SUBROUTINE FOR F16
      *===============================================================
      *
      ** F16=% ERRORS
     C     KQDO          BEGSR
      *
     C     XXMID         IFEQ      'M'
     C                   MOVE      'D'           ##ERR
     C                   ELSE
     C                   MOVE      'B'           ##ERR
     C                   ENDIF
      *
     C                   CALL      'W1057R01'
     C                   PARM      @@RRN         ##RRN
     C                   PARM      @@ELFL        ##ELFL
     C                   PARM                    ##ERR
      *
     C                   WRITE     SCR01
      *
     C                   ENDSR                                                  KQDO
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVEL     OOOLSN        SCLNAM
     C                   MOVEL     OOOFSN        SCFNAM
     C                   Z-ADD     OOOFPC        SCPRCT
      *****  Begin Chg  ***  06/09/20  ***********************
     C*****              MOVE      OOOSS#        SCSS#
      /Free
       SCSS# = MaskData('SSN':'OOOSS#':OOOSS#);
      /End-Free
      *****  End Chg  ***  06/09/20  ***********************
     C                   MOVEL     OOEXFL        SCINEX
      *
     C                   Z-ADD     OOEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
      *
     C                   Z-ADD     OOEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
      *
     C                   MOVEL(P)  OOTITL        SCTITL
      *
     C                   Z-ADD     OODOB         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDOB
      *
     C                   Z-ADD     OOIDT         SCIDT
     C                   Z-ADD     OOITIM        SCITIM
      *
     C                   ENDSR                                                  MVDBSC
      *===============================================================
      * OPT10 - SUBROUTINE FOR OPTION 10
      *===============================================================
      *
      ** Option 10=CHANGE INC/EXC
     C     OPT10         BEGSR
      *
     C                   MOVE      'I'           ##MODE
      *
     C                   CALL      ACDPGM        ACDPL                  99
      *
     C  NKC              MOVE      *ON           AUTORF
      *
     C                   ENDSR                                                  OPT10
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C     *INKN         CASEQ     *ON           KNDO                     90
      *
     C     *INKQ         CASEQ     *ON           KQDO                     90
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
      *================================================================
      * OTHOPT - Process other options
      *================================================================
     C     OTHOPT        BEGSR
      *
     C     SCSEL         CASEQ     '10'          OPT10
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHOPT
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
      *
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
      *
      ** Check to see if this is called as part of an Addition of Division
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W0051R19'    ##PGM            10
     C                   PARM      *BLANKS       ##EXST            1
      *
      ** Only write a WMENP record if not from Addition of Division
      ** Also, do not write when an entity is added
     C     ##EXST        IFEQ      *OFF
     C     @@ELFL        ANDEQ     *BLANKS
      *
     C                   Z-ADD     1             X                 3 0
     C                   MOVEA     *BLANKS       LN2
     C                   MOVEA     *BLANKS       FN2
     C                   MOVEA     *ZEROS        PC2
     C                   MOVEA     *BLANKS       IE2
     C                   MOVEA     *ZEROS        SQ2
     C                   MOVEA     *BLANKS       ST2
     C                   MOVEA     *BLANKS       TL2
     C                   Z-ADD     *ZEROS        ED2
     C                   Z-ADD     *ZEROS        EX2
      *
      ** Load the after snapshot
     C     OOKY          CHAIN     POO02                              60
     C     *IN60         DOWEQ     *OFF
      *
      ***** BEGIN ADD ***** 01/22/02 **********************************
     C     OOELID        IFEQ      *BLANKS
      ***** END ADD ******* 01/22/02 **********************************
     C                   MOVEL     OOOLSN        LN2(X)
     C                   MOVEL     OOOFSN        FN2(X)
     C     FN2(X)        CAT       LN2(X):0      NM2(X)
     C                   Z-ADD     OOOFPC        PC2(X)
     C                   MOVEL     OOEXFL        IE2(X)
     C                   Z-ADD     OOSEQ#        SQ2(X)
     C                   MOVEL     OOTITL        TL2(X)
     C                   Z-ADD     OOEFDT        ED2(X)
     C                   Z-ADD     OOEXDT        EX2(X)
     C                   ADD       1             X
      *
      ***** BEGIN ADD ***** 01/22/02 **********************************
     C                   ENDIF                                                  OOELID=' '
      ***** END ADD ******* 01/22/02 **********************************
     C     OOKY          READE     POO02                                  60
     C                   ENDDO
      *
      ** Get the Endorsement/DARTS information
     C     DXKY          CHAIN     PDX01                              60
      *
     C                   CALL      AGTAPI        AGTPL
      *
      ** Setup for next request number
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
      *
      ** Load WMENP record
     C                   MOVE      EMCO#         ENCO#
     C                   MOVE      EMFND         ENFND
     C                   MOVE      EMEMP#        ENEMP#
     C                   MOVE      EMDIV         ENDIV
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
      *
     C                   MOVE      LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
      *
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
     C*** BEGIN ADD ***  03/07/05  ************************************
     C*
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      EMNAM2        ENENM2
     C*
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMFDN2        ENEFN2
     C*
     C*** END ADD *****  03/07/05  ************************************
     C                   MOVEL     EMCONT        ENCON1
     C                   MOVEL     EMATTN        ENATT1
     C                   MOVEL     EMCNT3        ENCN31
     C                   MOVE      EMFEIN        ENFEN1
     C                   MOVEL     EMTPOB        ENTOB1
     C                   MOVEL     EMMAD1        ENMA11
     C                   MOVEL     EMMAD2        ENMA21
     C                   MOVEL     EMMCTY        ENMCT1
     C                   MOVEL     EMMST         ENMST1
     C                   MOVEL     EMMZIP        ENMZP1
     C                   MOVEL     EMPAD1        ENPA11
     C                   MOVEL     EMPAD2        ENPA21
     C                   MOVEL     EMPCTY        ENPCT1
     C                   MOVEL     EMPST         ENPST1
     C                   MOVEL     EMPZIP        ENPZP1
      *
     C                   MOVE      *BLANKS       ENNCFL
     C                   MOVE      *BLANKS       ENADDF
     C                   MOVE      *BLANKS       ENDLDF
     C                   MOVE      *BLANKS       ENFIFL
     C                   MOVE      *BLANKS       ENCNFL
     C                   MOVE      *BLANKS       ENATFL
     C                   MOVE      *BLANKS       ENC3FL
     C                   MOVE      *BLANKS       ENTBFL
     C                   MOVE      *BLANKS       ENRIDF
     C                   MOVE      *BLANKS       ENNOBF
      *
     C                   MOVEL     DXOONC        ENRMCD
      *
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
      *
     C     LDMYRR        CHAIN     PMY01                              60
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
      *
     C                   MOVEL     DSTIME        ENTIME
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
     C     F2KY          CHAIN     PF201                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      F2CAR#        ENCAR1
     C                   END                                                    F2KY *IN60
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   ENDIF                                                  ELKY *IN60  OFF
      *
     C                   Z-ADD     1             X
     C                   MOVE      *OFF          XXDONE            1
     C     X             DOWLT     500
     C     XXDONE        ANDEQ     *OFF
      *
     C                   MOVE      *OFF          XXWRT
      *
     C                   SELECT
      ** Addition of Owner/Officer
     C     @@QTE         WHENEQ    'A'
      *
     C                   Z-ADD     1             Y                 3 0
     C**** BEGIN DEL **** 12/15/00 ************************************
     C********** NM2,X     LOKUPNM,Y                     61
     C********** *IN61     IFEQ *OFF
     C**** END   DEL **** 12/15/00 ************************************
      *
     C                   MOVE      *ON           XXWRT             1
     C                   MOVE      'A'           ENNOFL
      *
      ** Build BEFORE line
     C                   MOVEL     *BLANKS       XXBL
      *
      ** Build AFTER line
     C                   MOVEL     *BLANKS       XXAL
     C                   Z-ADD     1             Z
     C                   MOVEL(P)  LN2(X)        XXWK25           25
     C     FN2(X)        IFNE      *BLANK
     C                   CAT(P)    ',':0         XXWK25           25
     C                   CAT(P)    FN2(X):1      XXWK25
     C                   ENDIF
     C     XXWK25        IFNE      *BLANK
     C     XXWK25        CAT(P)    ',':0         XXAL(Z)
     C                   ENDIF
     C                   MOVEL     PC2(X)        XXWK03            3
     C                   MOVE      PC2(X)        XXWK02            2
     C     PC2(X)        IFNE      *ZERO
     C     XXAL(Z)       ORNE      *BLANK
     C                   CAT(P)    XXWK03:1      XXAL(Z)
     C                   CAT       '.':0         XXAL(Z)
     C                   CAT       XXWK02:0      XXAL(Z)
     C                   ENDIF
     C     IE2(X)        IFNE      *BLANK
     C                   CAT(P)    ',':0         XXAL(Z)
     C                   CAT       IE2(X):1      XXAL(Z)
     C                   ENDIF
     C     TL2(X)        IFNE      *BLANK
     C                   CAT(P)    ',':0         XXAL(Z)
     C                   CAT       TL2(X):1      XXAL(Z)
     C                   ENDIF
      *
     C                   ADD       1             Z
     C     ED2(X)        IFNE      *ZEROS
     C                   CLEAR                   XXDT8
     C                   Z-ADD     ED2(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT8
     C                   CAT       XXMN:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXDY:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXYR:0        XXAL(Z)
     C                   ENDIF
     C     EX2(X)        IFNE      *ZEROS
     C                   CLEAR                   XXDT8
     C                   CAT       ' - ':0       XXAL(Z)
     C                   Z-ADD     EX2(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT8
     C                   CAT       XXMN:1        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXDY:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXYR:0        XXAL(Z)
     C                   ENDIF
     C**** BEGIN ADD **** 02/14/02 ************************************
     C                   Z-ADD     1             Y                 3 0
     C     NM2(X)        LOOKUP    NM(Y)                                  61
     C     *IN61         IFEQ      *OFF
     C                   CAT       NEW:5         XXAL(Z)
     C                   ENDIF
     C**** END   ADD **** 02/14/02 ************************************
      *
     C**** BEGIN DEL **** 12/15/00 ************************************
     C******************** ENDIF                           *IN61 = *OFF
     C**** END   DEL **** 12/15/00 ************************************
      *
      ** Deletion of Owner/Officer
     C     @@QTE         WHENEQ    'X'
     C                   Z-ADD     1             Y
     C**** BEGIN ADD **** 02/14/02 ************************************
     C**** BEGIN DEL **** 12/15/00 ************************************
     C     NM(X)         LOOKUP    NM2(Y)                                 61
     C     *IN61         IFEQ      *OFF
     C**** END   DEL **** 12/15/00 ************************************
     C**** END   ADD **** 02/14/02 ************************************
     C                   MOVE      *ON           XXWRT             1
     C                   MOVE      'D'           ENNOFL
      *
      ** Build BEFORE line
     C                   MOVEL     *BLANKS       XXBL
      *
      ** Build AFTER line
     C                   MOVEL     *BLANKS       XXAL
     C                   Z-ADD     1             Z
     C                   MOVEL(P)  LN(X)         XXWK25           25
     C     FN(X)         IFNE      *BLANK
     C                   CAT(P)    ',':0         XXWK25           25
     C                   CAT(P)    FN(X):1       XXWK25
     C                   ENDIF
     C     XXWK25        IFNE      *BLANK
     C     XXWK25        CAT(P)    ',':0         XXAL(Z)
     C                   ENDIF
     C                   MOVEL     PC(X)         XXWK03            3
     C                   MOVE      PC(X)         XXWK02            2
     C     PC(X)         IFNE      *ZERO
     C     XXAL(Z)       ORNE      *BLANK
     C                   CAT(P)    XXWK03:1      XXAL(Z)
     C                   CAT       '.':0         XXAL(Z)
     C                   CAT       XXWK02:0      XXAL(Z)
     C                   ENDIF
     C     IE(X)         IFNE      *BLANK
     C                   CAT(P)    ',':0         XXAL(Z)
     C                   CAT       IE(X):1       XXAL(Z)
     C                   ENDIF
     C     TL(X)         IFNE      *BLANK
     C                   CAT(P)    ',':0         XXAL(Z)
     C                   CAT       TL(X):1       XXAL(Z)
     C                   ENDIF
     C                   ADD       1             Z
     C     ED(X)         IFNE      *ZEROS
     C                   CLEAR                   XXDT8
     C                   Z-ADD     ED(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT8
     C                   CAT       XXMN:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXDY:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXYR:0        XXAL(Z)
     C                   ENDIF
     C     EX(X)         IFNE      *ZEROS
     C                   CLEAR                   XXDT8
     C                   CAT       ' - ':0       XXAL(Z)
     C                   Z-ADD     EX(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT8
     C                   CAT       XXMN:1        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXDY:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXYR:0        XXAL(Z)
     C                   ENDIF
      *
     C**** BEGIN ADD **** 02/14/02 ************************************
     C**** BEGIN DEL **** 12/15/00 ************************************
     C                   ENDIF                                                  *IN61 = *OFF
     C**** END   DEL **** 12/15/00 ************************************
     C**** END   ADD **** 02/14/02 ************************************
      *
      ** Change of Owner/Officer
     C     @@QTE         WHENEQ    'C'
     C     FN(X)         IFNE      FN2(X)
     C     LN(X)         ORNE      LN2(X)
     C     PC(X)         ORNE      PC2(X)
     C     IE(X)         ORNE      IE2(X)
     C     TL(X)         ORNE      TL2(X)
      *** BEGIN ADD *** 11/23/99 ***********************************************
     C     ED(X)         ORNE      ED2(X)
      *** END   ADD *** 11/23/99 ***********************************************
     C     EX(X)         ORNE      EX2(X)
      *
     C                   MOVE      *ON           XXWRT             1
     C                   MOVE      'C'           ENNOFL
      *
      ** Build BEFORE line
     C                   MOVEL     *BLANKS       XXBL
     C                   Z-ADD     1             Z                 3 0
     C                   MOVEL(P)  LN(X)         XXWK25           25
     C     FN(X)         IFNE      *BLANK
     C                   CAT(P)    ',':0         XXWK25           25
     C                   CAT(P)    FN(X):1       XXWK25
     C                   ENDIF
     C     XXWK25        IFNE      *BLANK
     C     XXWK25        CAT       ',':0         XXBL(Z)
     C                   ENDIF
     C                   MOVEL     PC(X)         XXWK03            3
     C                   MOVE      PC(X)         XXWK02            2
     C     PC(X)         IFNE      *ZERO
     C     XXBL(Z)       ORNE      *BLANK
     C                   CAT       XXWK03:1      XXBL(Z)
     C                   CAT       '.':0         XXBL(Z)
     C                   CAT       XXWK02:0      XXBL(Z)
     C                   ENDIF
     C     IE(X)         IFNE      *BLANK
     C                   CAT       ',':0         XXBL(Z)
     C                   CAT       IE(X):1       XXBL(Z)
     C                   ENDIF
     C     TL(X)         IFNE      *BLANK
     C                   CAT       ',':0         XXBL(Z)
     C                   CAT       TL(X):1       XXBL(Z)
     C                   ENDIF
      *
     C                   ADD       1             Z
     C     ED(X)         IFNE      *ZEROS
     C                   CLEAR                   XXDT8
     C                   Z-ADD     ED(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT8
     C                   CAT       XXMN:0        XXBL(Z)
     C                   CAT       XXS:0         XXBL(Z)
     C                   CAT       XXDY:0        XXBL(Z)
     C                   CAT       XXS:0         XXBL(Z)
     C                   CAT       XXYR:0        XXBL(Z)
     C                   ENDIF
     C     EX(X)         IFNE      *ZEROS
     C                   CLEAR                   XXDT8
     C                   CAT       ' - ':0       XXBL(Z)
     C                   Z-ADD     EX(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT8
     C                   CAT       XXMN:1        XXBL(Z)
     C                   CAT       XXS:0         XXBL(Z)
     C                   CAT       XXDY:0        XXBL(Z)
     C                   CAT       XXS:0         XXBL(Z)
     C                   CAT       XXYR:0        XXBL(Z)
     C                   ENDIF
      *
      ** Build AFTER line
     C                   MOVEL     *BLANKS       XXAL
     C                   Z-ADD     1             Z
     C                   MOVEL(P)  LN2(X)        XXWK25           25
     C     FN2(X)        IFNE      *BLANK
     C                   CAT(P)    ',':0         XXWK25           25
     C                   CAT(P)    FN2(X):1      XXWK25
     C                   ENDIF
     C     XXWK25        IFNE      *BLANK
     C     XXWK25        CAT(P)    ',':0         XXAL(Z)
     C                   ENDIF
     C                   MOVEL     PC2(X)        XXWK03            3
     C                   MOVE      PC2(X)        XXWK02            2
     C     PC2(X)        IFNE      *ZERO
     C     XXAL(Z)       ORNE      *BLANK
     C                   CAT(P)    XXWK03:1      XXAL(Z)
     C                   CAT       '.':0         XXAL(Z)
     C                   CAT       XXWK02:0      XXAL(Z)
     C                   ENDIF
     C     IE2(X)        IFNE      *BLANK
     C                   CAT(P)    ',':0         XXAL(Z)
     C                   CAT       IE2(X):1      XXAL(Z)
     C                   ENDIF
     C     TL2(X)        IFNE      *BLANK
     C                   CAT(P)    ',':0         XXAL(Z)
     C                   CAT       TL2(X):1      XXAL(Z)
     C                   ENDIF
      *
     C                   ADD       1             Z
     C     ED2(X)        IFNE      *ZEROS
     C                   CLEAR                   XXDT8
     C                   Z-ADD     ED2(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT8
     C                   CAT       XXMN:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXDY:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXYR:0        XXAL(Z)
     C                   ENDIF
     C     EX2(X)        IFNE      *ZEROS
     C                   CLEAR                   XXDT8
     C                   CAT       ' - ':0       XXAL(Z)
     C                   Z-ADD     EX2(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT8
     C                   CAT       XXMN:1        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXDY:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXYR:0        XXAL(Z)
     C                   ENDIF
      *
     C                   ENDIF                                                  FN,X != FN2,X
      *
     C                   ENDSL
      *
     C     XXWRT         IFEQ      *ON
     C                   Z-ADD     1             Z
     C     Z             DOWLT     3
     C     XXBL(Z)       IFNE      *BLANKS
     C     XXAL(Z)       ORNE      *BLANKS
     C                   MOVEL     XXBL(Z)       ENBLIN
     C                   MOVEL     XXAL(Z)       ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENTIME
     C                   ADD       1             ENSEQ#
     C                   ADD       1             Z
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      *
     C                   ELSE
      *
     C     NM(X)         IFEQ      *BLANKS
     C     NM2(X)        ANDEQ     *BLANKS
     C                   MOVE      *ON           XXDONE
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ADD       1             X
     C                   ENDDO                                                  DOW<=500
      *
     C                   ENDIF                                                  ##EXST *OFF
     C                   ENDIF                                                  LDENDA<>' '
      *
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
      *
     C                   CALL      'W1057R01'                           99
     C                   PARM      @@RRN         ##RRN
     C                   PARM      @@ELFL        ##ELFL
     C                   PARM      XXMID         ##ERR
      *
     C                   SELECT
     C     ##ERR         WHENEQ    *ON
     C                   MOVE      *ON           RSTPGM
     C     ##ERR         WHENEQ    'C'
     C                   MOVE      *ON           *INKC
     C     ##ERR         WHENEQ    'L'
     C                   MOVE      *ON           *INKL
     C                   ENDSL
      *
     C                   ENDIF                                                  *INKC=*OFF
      *
     C                   MOVE      *ON           *INLR
      *
     C                   ENDSR                                                  PGMEND
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
     C                   CLEAR                   KKELID
      *
     C                   MOVE      *BLANK        XXMID             1
      *
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    09    INQUIRY
     C     @@CALL        COMP      '3'                                    09    INQUIRY
     C     @@CALL        COMP      'D'                                    09    INQUIRY
      *
     C     Q1PRMS        IFGT      5
     C     @@QTE         IFEQ      'A'
     C     @@QTE         OREQ      'C'
     C     @@QTE         OREQ      'X'
     C                   MOVE      *BLANKS       XXQTE
     C                   ELSE
     C                   MOVE      @@QTE         XXQTE             1
     C                   END
     C                   ELSE
     C                   MOVE      *BLANKS       XXQTE             1
     C                   END
      *
     C                   MOVE      *ON           *IN12
      *
     C     @@ELFL        IFEQ      *BLANKS
      *
     C     @@RRN         CHAIN     WMEMP                              99
      *
     C                   MOVE      *OFF          *IN40
     C                   MOVEL     *BLANKS       SCELID
      *
     C                   ELSE
      *
     C     @@RRN         CHAIN     WMALP                              99
      *
     C                   MOVE      *ON           *IN40
     C                   MOVEL(P)  ALELID        SCELID
     C                   MOVEL(P)  ALNAME        SCELNM
      *
     C     @@ELFL        IFEQ      'E'
     C                   MOVEL(P)  ALELID        KKELID
     C                   ENDIF                                                  @@ELFL='E'
      *
      ** Get the Employer Master record
     C     EMKY          CHAIN     PEM01                              99
      *
     C                   ENDIF                                                  @@ELFL=*BLANKS
      *
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
      *
      ** Compress EMP#/DIV/NAME
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
      *
     C                   MOVEL     XXEDN         SCEDN
      *
      ** Get most recent cancellation date
     C                   MOVE      *OFF          *IN41
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1                3 0
      *
     C     *ZERO         LOOKUP    EF(I1)                                 61
      *
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61=*ON
      *
     C     I1            IFNE      0
      *
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN41
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
      *
     C                   ENDIF                                                  I1 IFNE 0
      *
      ** Get the Group Information
     C     FNKY          CHAIN     PFN01                              60
      *
     C                   MOVEL     FNNAME        SCNAME
      *
     C                   Z-ADD     *ZEROS        LDFYR
     C                   Z-ADD     *ZEROS        LDEND#
     C                   Z-ADD     *ZEROS        LDEEDT
     C                   Z-ADD     *ZEROS        LDMYRR
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
      *
      ** Check to see if called as part of an Addition of Division
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W0051R19'    ##PGM            10
     C                   PARM      *BLANKS       ##EXST            1
      *
      ** Only load BEFORE snapshot if not from Addition of Division
     C     ##EXST        IFEQ      *OFF
      *
     C                   MOVE      'M'           XXMID             1
      *
     C                   Z-ADD     1             X
     C                   MOVEA     *BLANKS       LN
     C                   MOVEA     *BLANKS       FN
     C                   Z-ADD     *ZEROS        PC
     C                   MOVEA     *BLANKS       IE
     C                   Z-ADD     *ZEROS        SQ
     C                   MOVEA     *BLANKS       ST
     C                   MOVEA     *BLANKS       TL
     C                   Z-ADD     *ZEROS        ED
     C                   Z-ADD     *ZEROS        EX
      *
     C     OOKY          CHAIN     POO02                              60
     C     *IN60         DOWEQ     *OFF
      *
      ***** BEGIN ADD ***** 01/22/02 **********************************
     C     OOELID        IFEQ      *BLANKS
      ***** END ADD ******* 01/22/02 **********************************
     C                   MOVEL     OOOLSN        LN(X)
     C                   MOVEL     OOOFSN        FN(X)
     C     FN(X)         CAT       LN(X):0       NM(X)
     C                   Z-ADD     OOOFPC        PC(X)
     C                   MOVEL     OOEXFL        IE(X)
     C                   Z-ADD     OOSEQ#        SQ(X)
     C                   MOVEL     OOTITL        TL(X)
     C                   Z-ADD     OOEFDT        ED(X)
     C                   Z-ADD     OOEXDT        EX(X)
     C                   ADD       1             X
      *
      ***** BEGIN ADD ***** 01/22/02 **********************************
     C                   ENDIF                                                  OOELID=' '
      ***** END ADD ******* 01/22/02 **********************************
     C     OOKY          READE     POO02                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  ##EXST *OFF
     C                   ENDIF                                                  LDENDA<>' '
      *
      ** Inactivate appropriate Options/Functions
     C     @@QTE         IFEQ      'A'
     C                   MOVEL     OFD(1)        XXLINE
     C                   MOVE      'N'           XXDSPL
     C                   MOVEL     XXLINE        OFD(1)
      *
     C                   MOVEL     OFD(2)        XXLINE
     C                   MOVE      'N'           XXDSPL
     C                   MOVEL     XXLINE        OFD(2)
      *
     C                   MOVEL     OFD(4)        XXLINE
     C                   MOVE      'N'           XXDSPL
     C                   MOVEL     XXLINE        OFD(4)
      *
     C                   MOVEL     OFD(7)        XXLINE
     C                   MOVE      'Y'           XXDSPL
     C                   MOVEL     XXLINE        OFD(7)
      *
     C                   ELSE
      *
     C     @@QTE         IFEQ      'C'
     C                   MOVEL     OFD(1)        XXLINE
     C                   MOVE      'Y'           XXDSPL
     C                   MOVEL     XXLINE        OFD(1)
      *
     C                   MOVEL     OFD(2)        XXLINE
     C                   MOVE      'N'           XXDSPL
     C                   MOVEL     XXLINE        OFD(2)
      *
     C                   MOVEL     OFD(4)        XXLINE
     C                   MOVE      'Y'           XXDSPL
     C                   MOVEL     XXLINE        OFD(4)
      *
     C                   MOVEL     OFD(7)        XXLINE
     C                   MOVE      'N'           XXDSPL
     C                   MOVEL     XXLINE        OFD(7)
      *
     C                   ELSE
      *
     C     @@QTE         IFEQ      'X'
     C                   MOVEL     OFD(1)        XXLINE
     C                   MOVE      'N'           XXDSPL
     C                   MOVEL     XXLINE        OFD(1)
      *
     C                   MOVEL     OFD(2)        XXLINE
     C                   MOVE      'Y'           XXDSPL
     C                   MOVEL     XXLINE        OFD(2)
      *
     C                   MOVEL     OFD(4)        XXLINE
     C                   MOVE      'N'           XXDSPL
     C                   MOVEL     XXLINE        OFD(4)
      *
     C                   MOVEL     OFD(7)        XXLINE
     C                   MOVE      'N'           XXDSPL
     C                   MOVEL     XXLINE        OFD(7)
      *
     C                   ELSE
      *
     C                   MOVEL     OFD(1)        XXLINE
     C                   MOVE      'Y'           XXDSPL
     C                   MOVEL     XXLINE        OFD(1)
      *
     C                   MOVEL     OFD(2)        XXLINE
     C                   MOVE      'Y'           XXDSPL
     C                   MOVEL     XXLINE        OFD(2)
      *
     C                   MOVEL     OFD(4)        XXLINE
     C                   MOVE      'Y'           XXDSPL
     C                   MOVEL     XXLINE        OFD(4)
      *
     C                   MOVEL     OFD(7)        XXLINE
     C                   MOVE      'Y'           XXDSPL
     C                   MOVEL     XXLINE        OFD(7)
      *
     C                   ENDIF                                                  @@QTE = 'X'
      *
     C                   ENDIF                                                  @@QTE = 'C'
      *
     C                   ENDIF                                                  @@QTE = 'A'
      *
      ***** BEG ADD ******* 01/19/01 **********************************
      *
     C     @@QTE         IFEQ      'Q'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '10'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  @@QTE='Q'
      *
      ***** END ADD ******* 01/19/01 **********************************
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * READ   - Read next record from file
      *================================================================
     C     READ          BEGSR
      *
     C     KEY1          READE     DBRCDF                                 95
      *
     C                   ENDSR                                                  READ
      *================================================================
      * READP  - Read prior record from file
      *================================================================
     C     READP         BEGSR
      *
     C     KEY1          READPE    DBRCDF                                 95
      *
     C                   ENDSR                                                  READP
      *===============================================================
      * SETGT - SUBROUTINE TO SET THE FILE > THE SELECTED FIELD
      *===============================================================
     C     SETGT         BEGSR
      *
     C     KEY1A         SETGT     DBRCDF                             96
      *
     C                   ENDSR                                                  SETGT
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     C     SETLL         BEGSR
      *
     C     KEY1A         SETLL     DBRCDF                             96
      *
     C                   ENDSR                                                  SETLL
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     C     SF1UPD        BEGSR
      *
     C                   MOVEL     DELETD        SCLNAM
      *
     C                   ENDSR                                                  SF1UPD
      *===============================================================
      * SLCTRD - SUBROUTINE TO MOVE FILEDS FROM SELECTED RECORDS
      *===============================================================
     C     SLCTRD        BEGSR
      *
     C                   MOVE      SCDBRR        @@RRN
      *
     C                   ENDSR                                                  SLCTRD
      *===============================================================
      * SUBSET - SUBROUTINE TO SUBSET THE LIST OF EMPLOYERS
      *===============================================================
     C     SUBSET        BEGSR
      *
     C                   ENDSR                                                  SUBSET
      *===============================================================
** Column headings
Last Name                 First Name      Percent  Inc/Exc & Dates
** Description--------------MISN
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
11010=Change Inc/Exc        YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
214F14=Darts                YYN
215F15=Fold/Truncate        YYY
216F16=% Errors             YYN
3
