     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H*    PROGRAM: F9511R1                                           *
     H*    DESCRIPTION: PROGRAM TO VALIDATE THE WORKFILE CREATED IN   *
     H*                 F9511R - FL TAPE SUBMISSION PART II.  THIS    *
     H*                 PROGRAM WILL PRODUCE A REPORT LISTING ANY     *
     H*                 CLAIMS THAT WERE FOUNF TO CONTAIN ERRORS.     *
     H*                 THIS PROGRAM WILL RUN INTERACTIVELY.          *
     H*                                                               *
     H*    CALLED FROM:                                               *
     H*      F9511C2                                                  *
     H*                                                               *
     H*    CALLS TO:                                                  *
     H*      NONE                                                     *
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 06/09/20 C:295 Project: SY420                                 *
     H*                -Mask SSN                                      *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  03/27/98 C:31  Project: MC145
     H*                 Yr 2000, RISC Developemnt
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     FFATPPP    IF   E           K DISK
     F                                     RENAME(FATPPP:PATPPP)
     FFMCMLA    IF   E           K DISK
     FFDF2L     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFTPSLFL   IF   E           K DISK
     FFMPRLFL   IF   E           K DISK
     FFDRUL1    IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
     D*
     D* ARRAYS AND TABLE DEFINITIONS
     D*
     D HER             S             70    DIM(17) CTDATA PERRCD(1)             HEADER ERROR MSG
     D DER             S             70    DIM(7) CTDATA PERRCD(1)              DETAIL ERROR MSG
     D PPL             S              5    DIM(10) CTDATA PERRCD(1)             PAYMENT PLANS
     D*
     D* DATE AND TIME DATA STRUCTURE
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* FL TAPE SUBMISSION LAYOUT HEADER DATA STRUCTURE
     D*
     D FLHEAD          DS
     D  HCNTL                  1     13
     D  HRECFL                14     14
     D  HFRMID                15     16
     D  HCAR#                 17     21
     D  HCFEIN                22     30
     D  HCZIP                 31     39
     D  HSVCA#                40     44
     D  HSFEIN                45     53
     D  HSZIP                 54     62
     D  HCSS#                 63     71
     D  HACDT                 72     79
     D  HAYMD                 74     79
     D  HPLIC#                80     88
     D  HPALPH                80     81
     D  HPFEIN                89     97
     D  HLNAME                98    127
     D  HFNAME               128    142
     D  HMI                  143    143
     D  HDTREC               144    151
     D  HRYMD                146    151
     D  HDTPAD               152    159
     D  HPYMD                154    159
     D  HTOTP                160    170
     D*
     D* FL TAPE SUBMISSION LAYOUT DETAIL DATA STRUCTURE
     D*
     D FLDET           DS
     D  DCNTL                  1     13
     D  DRECFL                14     14
     D  DFRMID                15     16
     D  DPOS                  17     18
     D  DDIA1                 19     24
     D  DDIA2                 25     30
     D  DDIA3                 31     36
     D  DDIA4                 37     42
     D  DPBILL                43     49
     D  DPROC                 43     47
     D  DPPAID                50     56
     D  DCHRG                 57     67
     D  DSRVS                 68     70
     D  DPAID                 71     81
     D  DFDOS                 82     89
     D  DFYMD                 84     89
     D  DTDOS                 90     97
     D  DTYMD                 92     97
     D  DRECON                98     98
     D  DPLAN                 99    103
     D  DFIL                 104    170
     D*
     D* NAMED CONSTANTS
     D*
     D ALPHA           C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D*
     D ALPHAS          C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ ')
     D*
     D*
     D*
     D* PROGRAM STATUS INFORMATION
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     I* RENAME FIELD IN FATPPP - FIELD NAME IS SAME AS FILE NAME
     I*
     IPATPPP
     I              FATPPP                      FLTPPP

     C*
     C* INITIALIZATION SECTION
     C*
     C     *LIKE         DEFINE    F2CO#         XXCO#
     C     *LIKE         DEFINE    F2SUB#        XXSUB#
     C     *LIKE         DEFINE    F2FND         XXFND
     C     *LIKE         DEFINE    F2ST          XXST
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C     *LIKE         DEFINE    CMACDT        XXCRCD
     C     *LIKE         DEFINE    CMACDT        XXCPAD
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C     *LIKE         DEFINE    RUATYP        XXATYP
     C                   Z-ADD     *ZEROS        XXFYR             2 0
     C                   Z-ADD     *ZEROS        X                 5 0
     C                   Z-ADD     *ZEROS        Z                 5 0
     C                   Z-ADD     *ZEROS        XXTO              6 0
     C                   Z-ADD     *ZEROS        XXFROM            6 0
     C                   Z-ADD     *ZEROS        XXFRDT            7 0
     C                   Z-ADD     *ZEROS        XXTODT            7 0
     C                   Z-ADD     *ZEROS        X1FRDT            6 0
     C                   Z-ADD     *ZEROS        X1TODT            6 0
     C                   Z-ADD     *ZEROS        XXDATE            7 0
     C                   Z-ADD     *ZEROS        XXTOTP           11 2
     C                   Z-ADD     *ZEROS        XXPOS             3 0
     C                   MOVE      *BLANKS       XXRCDF            1
     C                   MOVE      *BLANKS       XX5N01            5
     C                   MOVE      *BLANKS       XX5N02            5
     C                   MOVE      *BLANKS       XX9N01            9
     C                   MOVE      *BLANKS       XX9N02            9
     C                   MOVE      *BLANKS       XX9N03            9
     C                   MOVE      *BLANKS       XX9N04            9
     C                   MOVE      *BLANKS       XXFNNM           40
     C                   MOVE      *BLANKS       XXCONM           40
     C                   MOVE      *BLANKS       XXSBNM           40
     C                   MOVE      *BLANKS       XXPROC            9
     C                   MOVE      *BLANKS       CLNAME           30
     C                   MOVE      *BLANKS       FIRSTD            1
     C                   MOVE      'FL'          XXST
     C                   MOVE      '0'           XXPRT
     C                   MOVE      'P'           XXATYP
     C                   TIME                    DSDTTM
     C*
     C* KEY LIST FOR THE GROUP STATE INFORMATION FILE
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C* KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACDT
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C* KEY LIST FOR THE OUTPUT ROUTING FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C* KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C* KEY LIST FOR THE COMPANY MASTER FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C* KEY LIST FOR THE SUBSCRIBER MASTER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C* KEY LIST FOR PLACE OF SERVICE TABLE FILE
     C*
     C     PSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXPOS
     C*
     C* KEY LIST FOR PROCEDURE CODE FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    DPROC
     C                   KFLD                    XXDATE
     C*
     C* STATE FS RULES/PGM CROSS-REFERENCE FILE
     C*
     C     RUKY          KLIST
     C                   KFLD                    XXST
     C                   KFLD                    XXATYP
     C                   KFLD                    XXDATE
     C*
     C* ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQPROC            1
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
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* MOVE THE ENTRY PARMS TO PROGRAM WORK FIELDS
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFRDT        XXFROM
     C                   MOVE      QQTODT        XXTO
     C*
     C* DETERMINED THE SELECTED PROCEDURE
     C*
     C     QQPROC        IFEQ      'P'
     C     'PHYSICIA'    CAT       'N'           XXPROC
     C                   ELSE                                                   ELSE QQPROC EQ 'P'
     C     QQPROC        IFEQ      'H'
     C                   MOVEL     'HOSPITAL'    XXPROC
     C                   END                                                    END QQPROC EQ 'H'
     C                   END                                                    END QQPROC EQ 'P'
     C*
     C* GET COMPANY NAME
     C*
     C     COKY          CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXCONM
     C                   ELSE                                                   ELSE *IN60 EQ '1'
     C                   MOVEL     CONAME        XXCONM
     C                   END                                                    END *IN60 EQ '1'
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* GET SUBCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXSBNM
     C                   ELSE                                                   ELSE *IN60 EQ '1'
     C                   MOVEL     SMNAME        XXSBNM
     C                   END                                                    END *IN60 EQ '1'
     C*
     C* GET THE FUND MASTER RECORD
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXFNNM
     C                   ELSE                                                   ELSE *IN60 EQ '1'
     C                   MOVEL     FNNAME        XXFNNM
     C                   END                                                    END *IN60 EQ '1'
     C*
     C* GET THE ROUTING INFORMATION AND PRINT THE ROUTING PAGE
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C********     ORKY          CHAIN     POR01                              60
     C********     *IN60         IFEQ      '0'
     C********                   EXCEPT    ROUTE
     C********               END                                                    END *IN60 EQ '0'
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C* PRINT THE PAGE HEADER INFORMATION
     C*
     C                   EXCEPT    PHEAD
     C*
     C* READ THE FL TAPE SUBMISSION FILE - FATPPP
     C* NOTE: THIS FILE WILL BE OVERRIDDEN TO THE CORRECT MEMBER IN F9511C
     C*
     C                   READ      PATPPP                                 50
     C     *IN50         DOWEQ     '0'
     C*
     C* INITIALIZE THE HEADER VERIFICATION INDICATORS
     C*
     C                   MOVEA     '00000000'    *IN(20)
     C                   MOVEA     '00'          *IN(28)
     C                   MOVEA     '0000000'     *IN(30)
     C                   MOVEA     '0000000'     *IN(70)
     C*
     C* CLEAR THE HEADER AND DETAIL DATA STRUCTURES
     C*
     C                   CLEAR                   FLHEAD
     C                   CLEAR                   FLDET
     C*
     C* IF WE LOCATED RECORDS IN THE FL TAPE SUBMISSION FILE(FATPPP) CHECK THE
     C* RECORD TYPE FLAG TO DETERMINE WHICH DATA STRUCTURE TO LOAD THE RECORD TO
     C*    1 = HEADER RECORD
     C*    2 = DETAIL RECORD
     C*
     C                   Z-ADD     14            X
     C     1             SUBST     FLTPPP:X      XXRCDF
     C*
     C* IF THE RECORD FLAG IS A '1', THE RECORD IS A HEADER RECORD
     C*
     C     XXRCDF        IFEQ      '1'
     C*
     C* EVEYTIME WE READ A HEADER RECORD RESET THE FALG SO THE DETAIL HEADING
     C* WILL PRINT PROPERLY
     C*
     C                   MOVE      'Y'           FIRSTD
     C*
     C* WE HAVE LOCATED A HEADER RECORD AND SHOULD MOVE THIS INTO THE HEADER
     C* DATA STRUCTURE
     C*
     C                   MOVE      FLTPPP        FLHEAD
     C*
     C* CHAIN TO THE CLAIM MASTER FILE TO GET THE CLAIM INFORMATION
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
         HCSS# = UnMaskData('SSN':'HCSS#':HCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   MOVE      HCSS#         XXSS#
     C                   MOVE      HAYMD         XXACDT
     C                   Z-ADD     XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1ACDT            6 0
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   Z-ADD     *ZEROS        XXFYR
     C                   Z-ADD     *ZEROS        XXCASE
     C                   ELSE                                                   ELSE *IN60 EQ '1'
     C                   Z-ADD     CMFYR         XXFYR
     C                   Z-ADD     CMCASE        XXCASE
     C                   END                                                    END *IN60 EQ '1'
     C*
     C* VERIFY THAT THE FORM ID IS CORRECT
     C* VALID VALUES ARE : '09'
     C*
     C     HFRMID        IFNE      '09'
     C                   MOVE      '1'           *IN21
     C                   END                                                    END HFRMID NE '09'
     C*
     C* CHAIN TO THE GROUP STATE INFORMATION FILE TO GET THE CARRIER, SERVICING
     C* AGENT AND CARRIER/SERVICING AGENT FEDERAL ID#S AND ZIP CODES
     C*
     C                   MOVE      *BLANKS       XX5N01
     C                   MOVE      *BLANKS       XX5N02
     C                   MOVE      *BLANKS       XX9N01
     C                   MOVE      *BLANKS       XX9N02
     C                   MOVE      *BLANKS       XX9N03
     C                   MOVE      *BLANKS       XX9N04
     C     F2KY          CHAIN     PF201                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      F25N01        XX5N01
     C                   MOVE      F25N02        XX5N02
     C                   MOVE      F29N01        XX9N01
     C                   MOVE      F29N02        XX9N02
     C                   MOVE      F29N03        XX9N03
     C                   MOVE      F29N04        XX9N04
     C*
     C                   END                                                    END *IN60 EQ '0' F2K
     C*
     C* COMPARE THE CARRIER# IN THE TAPE SUBMISSION FILE TO THE CARRIER # WE
     C* JUST RETRIEVED - THEY SHOULD MATCH
     C*
     C     HCAR#         IFNE      XX5N01
     C                   MOVE      '1'           *IN22
     C                   END                                                    END HCAR# NE XX5N01
     C*
     C* COMPARE THE CARRIER FEDERAL ID# IN THE TAPE SUBMISSION FILE TO THE
     C* CARRIER FEDERAL ID# WE JUST RETRIEVED - THAY SHOULD MATCH
     C*
     C     HCFEIN        IFNE      XX9N01
     C                   MOVE      '1'           *IN23
     C                   END                                                    END HCFEIN NE XX9N01
     C*
     C* COMPARE THE CARRIER ZIP CODE IN THE TAPE SUBMISSION FILE TO THE CARRIER
     C* ZIP CODE WE JUST RETRIVED - THEY SHOULD MATCH
     C*
     C     HCZIP         IFNE      XX9N02
     C                   MOVE      '1'           *IN24
     C                   END                                                    END HCZIP NE XX9N02
     C*
     C* COMPARE THE SRV AGENT# IN THE TAPE SUBMISSION FILE TO THE SVC AGENT#
     C* WE JUST RETRIVED - THAY SHOULD MATCH
     C*
     C     HSVCA#        IFNE      XX5N02
     C                   MOVE      '1'           *IN25
     C                   END                                                    END HSVCA# NE XX5N02
     C*
     C* COMPARE THE SVC AGENT FEDERAL ID# IN THE TAPE SUBMISSION FILE TO THE
     C* SVC AGENT FEDERAL ID# WE JUST RETRIVED - THEY SHOULD MATCH
     C*
     C     HSFEIN        IFNE      XX9N03
     C                   MOVE      '1'           *IN26
     C                   END                                                    END HSFEIN NE XX9N03
     C*
     C* COMPARE THE SVC AGENT ZIP CODE IN THE TAPE SUBMISSION FILE TO THE SVC
     C* AGENT ZIP CODE WE JUST RETRIEVED - THEY SHOULD MATCH
     C*
     C     HSZIP         IFNE      XX9N04
     C                   MOVE      '1'           *IN27
     C                   END                                                    END HSZIP NE XX9N04
     C*
     C* VALIDATE THE PROVIDER LICENSE NUMBER IF GREATER THAN BLANKS
     C*
     C     HPALPH        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN28
     C                   END                                                    END HPLIC# EQ BLANKS
     C*
     C* VALIDATE THE 1ST TWO POSITIONS OF THE LICENSE # ARE ALPHABETIC
     C* 29 WILL BE '1', IF 1ST TWO POSITIONS ARE NOT ALPHA, ELSE WILL BE '0'
     C*
     C     ALPHA         CHECK     HPALPH                                 29
     C*
     C* THE PROVIDER TAX ID NUMBER MUST BE GREATER THAN ZEROS
     C*
     C     HPFEIN        IFEQ      *BLANKS
     C     HPFEIN        OREQ      *ZEROS
     C                   MOVE      '1'           *IN30
     C                   END                                                    END HPFEIN EQ BLANKS
     C*
     C* THE CLAIMANT LAST NAME CANNOT BE BLANKS
     C*
     C     HLNAME        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN31
     C                   END                                                    END HPLNAME EQ BLANK
     C*
     C* THE CLAIMANT FIRST NAME CANNOT BE BLANKS
     C*
     C     HFNAME        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN32
     C                   END                                                    END HPFNAME EQ BLANK
     C*
     C* VALIDATE THE CLAIMANT'S MIDDLE INITIAL CAN BE BLANK OR ALPHABETIC ONLY
     C* 33 WILL BE '1', IF NOT ALPHA OR BLANK, ELSE WILL BE '0'
     C*
     C     ALPHAS        CHECK     HMI                                    33
     C*
     C* COMBINE THE CLAIMANT FIRST, MIDDLE, AND LAST NAMES IN THE FOLLOWING
     C* FORMAT:  FIRST MIDDLE LAST
     C*
     C                   CLEAR                   CLNAME
     C     HFNAME        CAT       HMI:1         CLNAME
     C     CLNAME        CAT       HLNAME:1      CLNAME
     C*
     C* THE CARRIER RECEIVE DATE MUST BE GREATER OR EQUAL TO THE ACCIDENT DATE
     C*
     C                   MOVE      HRYMD         XXCRCD
     C                   Z-ADD     XXCRCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1CRCD            6 0
     C     XXCRCD        IFLT      XXACDT
     C                   MOVE      '1'           *IN34
     C                   END                                                    END XXCRCD LT XXACDT
     C*
     C* THE CARRIER PAID DATE MUST BE GREATER OR EQUAL TO THE ACCIDENT DATE
     C*
     C                   MOVE      HPYMD         XXCPAD
     C                   Z-ADD     XXCPAD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1CPAD            6 0
     C     XXCPAD        IFLT      XXACDT
     C                   MOVE      '1'           *IN35
     C                   END                                                    END XXCPAD LT XXACDT
     C*
     C* THE CARRIER PAID DATE MUST BE GREATER OR EQUAL TO CARRIER RECEIVE DATE
     C*
     C     XXCPAD        IFLT      XXCRCD
     C                   MOVE      '1'           *IN36
     C                   END                                                    END XXCPAD LT XXCRCD
     C*
     C* PRINT THE HEADER INFORMATION AND ANY ERRORS ASSOCIATED WITH THE HEADER
     C* RECORD
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    PHEAD
     C                   END                                                    END *INOA EQ '1'
     C*
     C                   MOVE      HTOTP         XXTOTP
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
         HCSS# = MaskData('SSN':'HCSS#':HCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   EXCEPT    HDET
     C                   EXCEPT    HERROR
     C*
     C                   ELSE                                                   ELSE XXRCDF EQ '1'
     C*
     C* IF THE RECORD FLAG IS A '2', THE RECORD IS A DETAIL RECORD
     C*
     C     XXRCDF        IFEQ      '2'
     C*
     C* WE HAVE LOCATED A DETAIL RECORD AND SHOULD MOVE THIS INTO THE DETAIL
     C* DATA STRUCTURE
     C*
     C                   MOVE      FLTPPP        FLDET
     C*
     C* VERIFY THAT THE FORM ID IS CORRECT
     C* VALID VALUES ARE : '09'
     C*
     C     DFRMID        IFNE      '09'
     C                   MOVE      '1'           *IN70
     C                   END                                                    END DFRMID NE '09'
     C*
     C* VALIDATE THE POS OF SERVICE CODE
     C*
     C                   MOVE      DPOS          XXPOS
     C     PSKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN71
     C                   END                                                    END *IN60 EQ '1'
     C*
     C* VALIDATE THE PROCEDURE CODE IS VALID FOR DATE OF SERVICE
     C*
     C                   MOVE      DFYMD         XXDATE
     C     RUKY          SETGT     PRU01
     C                   READP     PRU01                                  60
     C     *IN60         IFEQ      '0'
     C     RUST          ANDEQ     XXST
     C     RUATYP        ANDEQ     XXATYP
     C                   Z-ADD     RUEFDT        XXDATE
     C                   ELSE                                                   ELSE *IN60 EQ '0'
     C                   Z-ADD     *ZEROS        XXDATE
     C                   END                                                    END *IN60 EQ '0'
     C*
     C     PRKY          SETGT     PPR01
     C     NXTPR         TAG
     C                   READP     PPR01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     PRCO#         ORNE      XXCO#
     C     PRCPT4        ORNE      DPROC
     C     PREFDT        ORLT      XXDATE
     C                   MOVE      '1'           *IN72
     C                   ELSE                                                   ELSE *IN60 EQ '1'
     C     PRSTAT        IFEQ      'I'
     C                   GOTO      NXTPR
     C                   END                                                    END PRSTAT EQ 'I'
     C                   END                                                    END *IN60 EQ '1'
     C*
     C* THE FROM SERVICE DATE MUST BE GREATER OR EQUAL TO THE ACCIDENT DATE
     C*
     C                   MOVE      DFYMD         XXFRDT            7 0
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1FRDT            6 0
     C     XXFRDT        IFLT      XXACDT
     C                   MOVE      '1'           *IN73
     C                   END                                                    END XXFRDT LT XXACDT
     C*
     C* THE TO SERVICE DATE MUST BE GREATER OR EQUAL TO THE FROM SERVICE DATE
     C*
     C                   MOVE      DTYMD         XXTODT            7 0
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1TODT            6 0
     C     XXTODT        IFLT      XXFRDT
     C                   MOVE      '1'           *IN74
     C                   END                                                    END XXTODT LT XXFRDT
     C*
     C* VALIDATE THE RECONSIDERATION FLAG
     C* VALID VALUES ARE: R & F
     C*       R = RECONSIDERATION
     C*       F = FIRST SUBMISSION OF CODE
     C*
     C     DRECON        IFNE      'R'
     C     DRECON        ANDNE     'F'
     C                   MOVE      '1'           *IN75
     C                   END                                                    END DRECON NE 'R'
     C*
     C* VALIDATE THE PAYMENT PLAN CODE - SEE TABLE BELOW FOR VALID VALUES
     C*
     C                   Z-ADD     1             Z
     C     DPLAN         LOOKUP    PPL(Z)                                 60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN76
     C                   END                                                    END *IN60 EQ '0'
     C*
     C* PRINT THE DETAIL INFORMATION AND ANY ERRORS ASSOCIATED WITH THE DETAIL
     C* RECORD
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    PHEAD
     C                   EXCEPT    DHEAD
     C                   END                                                    END *INOA EQ '1'
     C*
     C* IF THIS IS THE FIRST DETAIL LINE ASSOCIATED WITH A HEADER RECORD PRINT
     C* THE DETAIL LINE HEADINGS, ELSE DO NOT PRINT HEADINGS
     C*
     C     FIRSTD        IFEQ      'Y'
     C                   EXCEPT    DHEAD
     C                   MOVE      'N'           FIRSTD
     C                   END                                                    END FIRSTD EQ 'Y'
     C*
     C                   EXCEPT    DDET
     C                   EXCEPT    DERROR
     C*
     C                   END                                                    END XXRCDF EQ '2'
     C*
     C                   END                                                    END XXRCDF EQ '1'
     C*
     C* READ THE NEXT FLORIDA TAPE SUBMISSION RECORD
     C*
     C                   READ      PATPPP                                 50
     C                   END                                                    END *IN50 DOWEQ '0'
     C*
     C* END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orky Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     O*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             XXFNNM              60
     O**********E            ROUTE          1
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       XXFNNM              60
     O          E            ROUTE          1
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O*
     O          E            ROUTE          1 34
     O                                           18 'COMPANY  :'
     O                       XXCO#         Z     25
     O          E            ROUTE          1
     O                                           18 'SUBSCRIBER:'
     O                       XXSUB#        Z     27
     O          E            ROUTE          1
     O                                           18 'GROUP    :'
     O                       XXFND         Z     25
     O          E            ROUTE          1
     O                                           18 'FROM DATE:'
     O                       XXFROM              30 '  /  /  '
     O          E            ROUTE          1
     O                                           18 'TO DATE  :'
     O                       XXTO                30 '  /  /  '
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                   '(C) COPYRIGHT PROGRESSIV'
     O***                              +  0 'E DATA SOLUTIONS, INC.'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            PHEAD          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             120
     O                       DSDATE        Y    198
     O          E            PHEAD          1
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            PHEAD          1
     O                                           88 'FROM DATE:'
     O                       XXFROM              97 '  /  /  '
     O                                          108 'TO DATE:'
     O                       XXTO               117 '  /  /  '
     O          E            PHEAD          1
     O                       XXPROC             102
     O          E            PHEAD          1
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#        Z     18
     O                       XXSBNM              60
     O          E            PHEAD          2
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                                           12 '-'
     O                       XXFNNM              51
     O*
     O* HEADER INFORMATION
     O*
     O          E            PHEAD          1
     O                                           50 'SVC'
     O                                           59 'SVC'
     O                                           70 'SVC'
     O          E            PHEAD          1
     O                                           22 'CARRIER'
     O                                           32 'CARRIER'
     O                                           43 'CARRIER'
     O                                           51 'AGENT'
     O                                           60 'AGENT'
     O                                           71 'AGENT'
     O          E            PHEAD          1
     O                                           13 'CONTROL #'
     O                                           19 '#'
     O                                           30 'FEIN'
     O                                           41 'ZIP'
     O                                           60 'FEIN'
     O                                           70 'ZIP'
     O                                           84 'SOC SEC #'
     O                                           94 'ACC DATE'
     O                                          106 'YR    CASE'
     O                                          117 'PHYS LIC#'
     O                                          128 'PROV FEIN'
     O                                          143 'CLAIMANT NAME'
     O                                          170 'DATE REC'
     O                                          181 'DATE PAID'
     O                                          197 'TOTAL PAID'
     O          E            HDET        1  2
     O                       HCNTL               13
     O                       HCAR#               22
     O                       HCFEIN              33
     O                       HCZIP               44
     O                       HSVCA#              51
     O                       HSFEIN              62
     O                       HSZIP               73
     O                       HCSS#               84
     O                       X1ACDT              94 '  /  /  '
     O                       XXFYR               98
     O                                           99 '-'
     O                       XXCASE             106
     O                       HPLIC#             117
     O                       HPFEIN             128
     O                       CLNAME             160
     O                       X1CRCD             170 '  /  /  '
     O                       X1CPAD             180 '  /  /  '
     O                       XXTOTP             197
     O          E            HERROR         1
     O                                           28 'HEADER ERRORS:'
     O          E    20      HERROR         1
     O               20      HER(1)              91
     O          E    21      HERROR         1
     O               21      HER(2)              91
     O          E    22      HERROR         1
     O               22      HER(3)              91
     O          E    23      HERROR         1
     O               23      HER(4)              91
     O          E    24      HERROR         1
     O               24      HER(5)              91
     O          E    25      HERROR         1
     O               25      HER(6)              91
     O          E    26      HERROR         1
     O               26      HER(7)              91
     O          E    27      HERROR         1
     O               27      HER(8)              91
     O          E    28      HERROR         1
     O               28      HER(9)              91
     O          E    29      HERROR         1
     O               29      HER(10)             91
     O          E    30      HERROR         1
     O               30      HER(11)             91
     O          E    31      HERROR         1
     O               31      HER(12)             91
     O          E    32      HERROR         1
     O               32      HER(13)             91
     O          E    33      HERROR         1
     O               33      HER(14)             91
     O          E    34      HERROR         1
     O               34      HER(15)             91
     O          E    35      HERROR         1
     O               35      HER(16)             91
     O          E    36      HERROR         1
     O               36      HER(17)             91
     O*
     O* DETAIL INFORMATION
     O*
     O          E            DHEAD          1
     O                                           72 'RECON'
     O                                           77 'PAY'
     O          E            DHEAD          1
     O                                           11 'POS'
     O                                           26 'MED PROC-BILL'
     O                                           41 'MED PROC-PAID'
     O                                           53 'FR DT SERV'
     O                                           65 'TO DT SERV'
     O                                           72 'FLAG'
     O                                           78 'PLAN'
     O          E            DDET           2
     O                       DPOS                11
     O                       DPBILL              26
     O                       DPPAID              41
     O                       X1FRDT              52 '  /  /  '
     O                       X1TODT              64 '  /  /  '
     O                       DRECON              70
     O                       DPLAN               80
     O          E            DERROR         1
     O                                           28 'DETAIL ERRORS:'
     O          E    70      DERROR         1
     O               70      DER(1)              91
     O          E    71      DERROR         1
     O               71      DER(2)              91
     O          E    72      DERROR         1
     O               72      DER(3)              91
     O          E    73      DERROR         1
     O               73      DER(4)              91
     O          E    74      DERROR         1
     O               74      DER(5)              91
     O          E    75      DERROR         1
     O               75      DER(6)              91
     O          E    76      DERROR         1
     O               76      DER(7)              91
**  TABLE HEADER ERROR MESSAGES
*SOCIAL SECURITY#/ACCIDENT DATE ARE INVALID
*THE FORM ID FOR THE HEADER RECORD IS INVALID
*THE CARR # IN TAPE FILE DOES NOT MATCH THE CARR # IN GRP ST FILE
*THE CARR FEIN IN TAPE FILE DOES NOT MATCH THE CARR FEIN IN GRP ST FLE
*THE CARR ZIP IN TAPE FILE DOES NOT MATCH THE CARR ZIP IN GRP ST FILE
*THE SVC AGNT# IN TAPE FILE DOES NOT MATCH THE SVC AGNT# IN GRP ST FLE
*THE SVC FEIN IN TAPE FILE DOES NOT MATCH THE SVC FEIN IN GRP ST FILE
*THE SVC ZIP IN THE TAPE FILE DOES NOT MATCH THE SVC ZIP IN GRP ST FLE
*THE PROVIDER LICENSE # IS EQUAL TO BLANKS
*THE FIRST TWO CHARACTERS OF THE PROV LICENSE# ARE NOT ALPHABETIC
*THE PROVIDER FEDERAL ID# IS EQUAL TO ZEROS
*THE CLAIMANT LAST NAME IS BLANKS
*THE CLAIMANT FIRST NAME IS BLANKS
*THE CLAIMANT MIDDLE INITIAL IS NOT ALPHABETIC OR BLANK
*THE CARRIER RECEIVE DATE IS NOT GREATER THAN OR EQUAL TO THE ACC DT
*THE CARRIER PAID DATE IS NOT GREATER THAN OR EQUAL TO THE ACC DATE
*THE CARRIER PAID DATE IS NOT GREATER OR EQUAL TO THE CARRIER REC DT
**  TABLE DETAIL ERROR MESSAGES
*THE FORM ID FOR THE DETAIL RECORD IS INVALID
*THE PLACE OF SERVICE CODE IS INVALID
*THE CPT-4 PROCEDURE CODE IS INVALID
*THE FROM SERVICE DATE IS NOT GREATER THAN OR EQUAL TO THE ACC DATE
*THE TO SERVICE IS NOT GREATER THAN OR EQUAL TO THE FROM SERVICE DT
*THE RECONSIDERATION FLAG IS INVALID
*THE PAYMENT PLAN CODE IS INVALID
**  TABLE PAYMENT PLANS
CP
CH
CI
MP
MH
MI
FS
NP
NH
NC
