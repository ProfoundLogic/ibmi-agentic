     Hcopyright('(c) 2020 Sapiens, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*
     H/TITLE W8074R - TRANSMISSION FORMAT PROCESSOR
     H*
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 06/18/97  for Version 3 BASE                        *
     H*                    C:58  PROJECT: WC468                       *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/14/22 C:079 Project WC2797                                 *
     H*                IAIABC 3.1.4 field(s)                          *
     H*                                                               *
     H* 04/03/20 C:062 Project WC2730                                 *
     H*                Add new field about collective bargaining      *
     H*                                                               *
     H* 11/21/18 C:180 Support: 74885/WC2515                          *
     H*                Include Carriage Return for Release 3.0        *
     H*                                                               *
     H* 11/12/18 C:053 Project: WC2515 priss                          *
     H*                Change for Mitchell submissions                *
     H*                                                               *
     H* 05/30/18 C:274 Project: WC2515                                *
     H*                Add code for EDI 3.1                           *
     H*                                                               *
     H* 06/26/18 C:180 Support - 71017                                *
     H*                Send blanks instead of zeros for AWW.          *
     H*            DEL-This change applicable to the state of LA.     *
     H*            CHG-This change applicable for all states.         *
     H*                                                               *
     H* 01/31/17 C:306 Project - WC2410                               *
     H*                Add new EDI fields: D9INJS,D9SEMP,D9CLSB,      *
     H*                                    D9DRES,D9ACCD.             *
     H*                                                               *
     H* 05/29/15 C:180 SUPPORT: 34842                                 *
     H*                Add LF for clt 129 and states of MA, NH        *
     H*                                                               *
     H* 04/11/14 C:180 SUPPORT: WC126 85038                           *
     H*                Add LF for clt 126 and state of NY.            *
     H*                Replace 02/03/14 code.                         *
     H*                                                               *
     H* 02/08/14 C:180 SUPPORT PRISS 027 - WC2234                     *
     H*                Add new EDI fields:                            *
     H*                - Work Week Type Code                          *
     H*                - Work Week Schedule Code                      *
     H*                - Employee Security Code                       *
     H*                                                               *
     H* 02/03/14 C:180 SUPPORT: WC136 84609                           *
     H*                Client specific change: Add CRLF to Header and *
     H*                trailer records for CA reporting.              *
     H*                                                               *
     H* 07/02/13 C:190 PROJECT 147WBF                                 *
     H*                Access WXFRP with the new logical.             *
     H*                                                               *
     H* 01/25/12 C:062 PROJECT WC114 79004                            *
     H*                If AWW is zero send blanks if jurisdiction st=WV.
     H*
     H* 11/10/10 C:220 PROJECT WC1933 / 16
     H*                         Blankout the D8LSN for release 3.
     H*
     H* 08/19/10 C:220 PROJECT WC1584 / 139WNN - PRISS 61
     H*                         Add EDIF subkey in IXKY Klist for correct
     H*                         header information
     H*
     H* 07/29/10 C:053 PROJECT 139WNN - PRISS 44
     H*
     H* 07/28/10 C:220 PROJECT 139WNN - Add key EDIF for WXFRL file
     H*
     H* 05/03/10 C:190 PROJECT WC1859 - FROI Late Reason code.
     H*
     H* 07/17/09 C:180 PROJECT WC1836
     H*                Add Employer physical country.
     H*
     H* 04/15/09 C:053 SUPPORT 67734
     H*                For GIC only, make MTC date = original sent date.
     H*
     H* 11/17/08 C:53  SUPPORT 63706
     H*                Changes for insured fein #
     H*                                                               *
     H* 10/20/08 C:180 SUPPORT 65603
     H*                Populate date of knowledge of disability.
     H*
     H* 10/06/08 C:180 SUPPORT 65434                                  *
     H*                Populate claim admin FEIN.
     H*
     H* 10/04/08 C:53  Project:WC1177                                 *
     H*                Remove carriage return coding                  *
     H*                                                               *
     H* 08/06/08 C:53  Project:64700                                  *
     H*                NOT needed in this release.                    *
     H*                                                               *
     H* 02/19/08 C:15  Project:WC1177                                 *
     H*                Added a carriage return to the end of each     *
     H*                record to create variable length records.      *
     H*                This is needed for Roes to process the file.   *
     H*                                                               *
     H* 10/19/07 C:191 Project:WC1177                                 *
     H*                EDI Release 3 - Priss #4                       *
     H*                                                               *
     H* 05/01/07 C:185 Project:WC1177                                 *
     H*                EDI Release 3                                  *
     H*                                                               *
     H* 10/28/05 C:53 Support: 49459                                  *
     H*               CA change to load blanks to phone#              *
     H*                                                               *
     H* 07/03/02 C:53 Support: WC081 # 32456                          *
     H*               Update maintenance fields                       *
     H*                                                               *
     H* 06/29/99 C:15 Project:                                        *
     H*               Load blanks to all dates which equal zero.      *
     H*****************************************************************
     FWMM2L     UF   E           K DISK
     FWMLGL5    UF   E           K DISK
     FWMIXL     IF   E           K DISK
     FWXFRL1    IF   E           K DISK
     FWXX8P     O    E           K DISK
     FSSYLF     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     D*
     D xxpos           s              5  0
     D XXRLSA          S              1A
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXACLC                 1     25
     D  XXCO#                  1      3  0
     D  XXFND                  4      6  0
     D  XXFYR                  7      9  0
     D  XXCASE                10     16  0
     D*
     D                 DS
     D  WKVS21                 1   5923
     D  WKR3A                  1      2
     D  WKR3B                  3      4
     D  WKR3C                  5      6
     D  WKR3D                  7     12
     D  WKR3                  13   5923
     D X8DATA          DS          7535
     D HED148        E DS                  EXTNAME(WXH8P)
     D INP148        E DS                  EXTNAME(WXFRP)
     D DET148        E DS                  EXTNAME(WXD8P)
     D DETR21        E DS                  EXTNAME(WXD9P)
     D DT2231        E DS                  EXTNAME(WXD3P)
     D TRL148        E DS                  EXTNAME(WXT8P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C*
     C*   INITIALIZATION SUBROUTINE
     C*
     C                   EXSR      SRINIT
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C*  VERIFY EDI CONTROL FLAG IS 'Y'
     C*
     C                   MOVEL     'WC'          WCKEY
     C     WCKEY         CHAIN     SWC01                              60
     C     *IN60         IFEQ      *ON
     C     WCFL09        ORNE      'Y'
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF
     C*
     C*  RETRIEVE THE EDI CONTROL RECORD
     C*
     C                   MOVE      *BLANKS       XXTRID
     C                   MOVE      QQEDIF        XXEDIF
     C     IXKY          CHAIN     PIX01                              60
     C     *IN60         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C
     C***     EDI 3.1 --> IXRLS = 'A'
     C                   IF        IXRLS = 'A'
     C                   EVAL      XXRLSA = *ON
     C                   EVAL      IXRLS  = '3'
     C                   EVAL      QQRLSA = '1'
     C                   ENDIF
     C*
     C*  RETRIEVE THE TRANSMISSION MANAGEMENT RECORD.
     C*
     C     M2KY          CHAIN     PM201                              60
     C     *IN60         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C     *IN60         DOWEQ     *OFF
     C     QQEDIF        IFEQ      M2EDIF
     C     M2STAT        IFEQ      'R'
     C                   Z-ADD     M2OTDT        XXSENT
     C                   ELSE
     C                   Z-ADD     0             XXSENT
     C                   ENDIF
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C     M2KY          READE     PM201                                  60
     C     *IN60         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C*
     C*  PROCESS ALL 148 LOG RECORDS
     C*
     C     LGKY          CHAIN     PLG01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C*
     C                   Z-ADD     LGFND         XXFND
     C                   MOVE      *OFF          *IN65
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C     QQEDIF        ANDEQ     FNEDIF
     C                   MOVE      *ON           *IN65
     C                   ENDIF
     C*
     C     LGSTAT        IFNE      'H'
     C     *IN65         ANDEQ     *ON
     C*
     C*  MAINLINE OF PROGRAM - LOOP THRU EXTRACT MANAGEMENT RECORDS
     C*  1) IF THIS IS A RE-TRANSMISSION, PROCESS ALL EXTRACTED RECORDS
     C*      EQUAL TO THE ORIGINAL TRANSMISSION DATE.
     C*  2) IF THIS IS AN ORIGINAL TRANSMISSION, PROCESS ALL EXTRACTED
     C*      RECORDS WHICH HAVEN'T ALREADY BEEN FORMATED AND THE EXTRACT
     C*      DATE IS LESS THAN OR EQUAL TO THE TRANSMISSION DATE.
     C*
     C     M2STAT        IFEQ      'R'
     C     M2STAT        OREQ      'A'
     C     LGSTS         ANDEQ     'EX'
     C*
     C*  FIRST TIME THROUGH, WRITE THE TRANSMISSION HEADER.
     C*
     C     XXTRFG        IFEQ      *OFF
     C                   EXSR      SRXHDR
     C                   MOVE      *ON           XXTRFG
     C                   ENDIF
     C*
     C*  PROCESS ALL 148 DETAIL ASSOCIATED WITH EACH EXTRACT (148)
     C*
     C                   Z-ADD     LGFND         XXFND
     C                   Z-ADD     LGFYR         XXFYR
     C                   Z-ADD     LGCASE        XXCASE
     C     keyWXFRL1     SETGT     PFR01
     C     keyWXFRL1     READPE    PFR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   EXSR      SRXDTL
     C                   ADD       1             XXRCNT
     C     IXRLS         IFEQ      '3'
     C                   EXSR      SRXR21
     C                   ADD       1             XXTCNT
     C                   ADD       1             XXRCNT
     C                   ENDIF
     C*
     C     M2STAT        IFNE      'R'
     C                   MOVE      M2OTDT        LGSENT
     C                   MOVE      'SN'          LGSTS
     C                   UPDATE    PLG01
     C                   END
     C*
     C                   END
     C*
     C*  PROCESS ONLY THE NUMBER SPECIFIED IN EDI CONTROL
     C*
     C     IXRLS         IFEQ      '3'
     C     XXTCNT        IFGE      IXMAXT
     C     IXMAXT        ANDGT     0
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ELSE
     C     XXRCNT        IFGE      IXMAXT
     C     IXMAXT        ANDGT     0
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C     LGKY          READE     PLG01                                  60
     C                   ENDDO
     C*
     C     XXTRFG        IFEQ      *ON
     C                   EXSR      SRXTRL
     C                   ENDIF
     C*
     C                   Z-ADD     XXRCNT        M2148T
     C                   Z-ADD     XXRCNT        M2148S
     C*
     C* IF R3 OVERRIDE W/TRANSMISSIONS SENT
     C*
     C     IXRLS         IFEQ      '3'
     C                   Z-ADD     XXTCNT        M2148T
     C                   Z-ADD     XXTCNT        M2148S
     C                   ENDIF
     C*
     C                   MOVE      'P'           M2STAT
     C                   UPDATE    PM201
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*
     C* DEFINE ALL KLISTS, FIELD DEFINITIONS, ETC...
     C*
     C     SRINIT        BEGSR
     C*
     C*  WMUPL - USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  WXFRL - 148 FILE
     C*

     c     keyWXFRL1     KLIST
     c                   KFLD                    LGco#
     c                   KFLD                    xxFND
     c                   KFLD                    xxFYR
     c                   KFLD                    xxCASE
     c                   KFLD                    LGjst
     c                   KFLD                    LGexdt
     c                   KFLD                    LGtset
     C*
     C*  WMLGL5 - EDI LOG FILE
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXSENT
     C                   KFLD                    XXTSET
     C*
     C*  WMIXL  - EDI CONTROL FILE
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXTRID
     C                   KFLD                    XXEDIF            3
     C*
     C*  WMM2L - SYSTEM MANAGEMENT RECORDS
     C*
     C     M2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXTSET
     C                   KFLD                    XXOTDT            7 0
     C                   KFLD                    XXOTTM            6 0
     C*
     C*  WMFNL - FUND MASTER FILE KEY
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*
     C     *LIKE         DEFINE    LGJST         XXJST
     C     *LIKE         DEFINE    *INLR         XXTRFG
     C     *LIKE         DEFINE    LGSENT        XXSENT
     C     *LIKE         DEFINE    IXTRID        XXTRID
     C*
     C                   MOVE      *ZEROS        XXDATE            8 0
     C                   MOVE      *OFF          XXTRFG
     C                   MOVE      *ZEROS        XXRCNT            9 0
     C                   MOVE      *ZEROS        XXTCNT            9 0
     C*
     C*   ENTRY PARAMETERS (ALL ALPHANUMERIC)
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQJST             2
     C                   PARM                    QQTSET            3
     C                   PARM                    QQOTDT            8
     C                   PARM                    QQOTTM            6
     C                   PARM                    QQEDIF            3
     C                   PARM                    QQRLSA            1
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQJST         XXJST
     C                   MOVE      QQOTDT        XXDATE            8 0
     C                   MOVE      QQOTTM        XXOTTM
     C                   MOVE      QQTSET        XXTSET            3
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXOTDT
     C*
     CSR                 TIME                    DSDTTM
     C*
-ESR CSR   ENINIT        ENDSR
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*
     C* FORMAT 148 HEADER AND WRITE TO FORMATTED TRANSMISSION FILE.
     C*
-BSR C     SRXHDR        BEGSR
     C*
     C                   CLEAR                   X8DATA
     C                   MOVE      'HD1'         H8THDR
     C                   MOVEL     IXSEIN        H8SEIN
     C                   MOVEL     IXSZP         H8SZP
     C                   MOVEL     IXREIN        H8REIN
     C                   MOVEL     IXRZP         H8RZP
     C                   MOVE(P)   M2LTDT        H8LTDT
     C                   MOVEL     M2LTDT        XXCENT            1 0
     C     XXCENT        IFEQ      0
     C                   MOVEL     '19'          H8LTDT
     C                   ELSE
     C                   MOVEL     '20'          H8LTDT
     C                   END
     C                   MOVE      M2LTTM        H8LTTM
     C                   MOVE(P)   M2OTDT        H8OTDT
     C                   MOVEL     M2OTDT        XXCENT
     C     XXCENT        IFEQ      0
     C                   MOVEL     '19'          H8OTDT
     C                   ELSE
     C                   MOVEL     '20'          H8OTDT
     C                   END
     C                   MOVE      M2OTTM        H8OTTM
     C     M2TSTF        IFEQ      'Y'
     C                   MOVE      'T'           H8TPFG
     C                   ELSE
     C                   MOVE      'P'           H8TPFG
     C                   END
     C                   MOVE      QQTSET        H8TSET
     C     QQTSET        IFEQ      '148'
     C                   MOVEL     '01'          H8TRN#
     C                   ELSE
     C                   MOVEL     '1A'          H8TRN#
     C                   ENDIF
     C*
     C     IXRLS         IFEQ      '3'
     C                   MOVEL     '30'          H8TRN#
     C                   ENDIF
     C     XXRLSA        IFEQ      *ON
     C                   MOVEL     '31'          H8TRN#
     C                   ENDIF
     C*
     C                   MOVEL     HED148        X8DATA
      /Free
       If (XXJST = 'CA' and CLTID = '136') or (XXJST = 'NY' and CLTID = '126');
         eval  %subst(X8DATA:88) = x'0D';
       Endif;
       If (XXJST = 'MA' and CLTID = '129') or (XXJST = 'NH' and CLTID = '129');
         eval  %subst(X8DATA:88) = x'0D';
       Endif;

       %subst(X8DATA:88) = x'0D';
      /End-Free
     C                   WRITE     PX801
     C*
-ESR C     ENXHDR        ENDSR
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*
     C* FORMAT 148 DETAIL AND WRITE TO FLAT FILE.
     C*
-BSR C     SRXDTL        BEGSR
     C*
     C                   CLEAR                   X8DATA
     C                   MOVEL     FRTSET        D8TSET
     C                   MOVEL     FRMTC         D8MTC
     C                   MOVE      FRTDT         D8TDT
     C*
     C** OVERRIDE FOR GIC R3 ONLY
     C*
     C     CLTID         IFEQ      '139'
     C     IXRLS         ANDEQ     '3'
     C                   MOVE(P)   M2OTDT        D8TDT
     C                   MOVEL     M2OTDT        XXCENT
     C     XXCENT        IFEQ      0
     C                   MOVEL     '19'          D8TDT
     C                   ELSE
     C                   MOVEL     '20'          D8TDT
     C                   ENDIF
     C                   ENDIF
     C                   MOVEL     FRJRST        D8JRST
     C                   MOVEL     FRACL#        D8ACL#
     C                   MOVEL     FRCLCD        D8CLCD
     C                   MOVEL     FRCLSN        D8CLSN
     C                   MOVEL     FRPADC        D8PADC
     C                   MOVEL     FRPLSN        D8PLSN
     C                   MOVEL     FRAAD1        D8AAD1
     C                   MOVEL     FRAAD2        D8AAD2
     C                   MOVEL     FRACIT        D8ACIT
     C                   MOVEL     FRAST         D8AST
     C                   MOVEL     FRAZP         D8AZP
     C                   MOVEL     FRACLC        D8ACLC
     C                   MOVEL     FREMP#        D8EMP#
     C                   MOVEL     FRILSN        D8ILSN
     C                   MOVEL     FRELSN        D8ELSN
     C                   MOVEL     FREAD1        D8EAD1
     C                   MOVEL     FREAD2        D8EAD2
     C                   MOVEL     FRECIT        D8ECIT
     C                   MOVEL     FREST         D8EST
     C                   MOVEL     FREZP         D8EZP
     C                   MOVEL     FRSFIN        D8SFIN
     C                   MOVEL     FRSIC         D8SIC
     C                   MOVEL     FRRPT#        D8RPT#
     C                   MOVEL     FRLOC#        D8LOC#
     C                   MOVEL     FRPOL#        D8POL#
     C                   MOVE      FRPEFD        D8PEFD
     C                   MOVE      FRPEXD        D8PEXD
     C                   MOVE      FRACDT        D8ACDT
     C                   MOVE      FRACTM        D8ACTM
     C                   MOVEL     FRAZP1        D8AZP1
     C                   MOVEL     FREADR        D8EADR
     C                   MOVEL     FRNATR        D8NATR
     C                   MOVEL     FRPART        D8PART
     C                   MOVEL     FRCAUS        D8CAUS
     C                   MOVEL     FRACDS        D8ACDS
     C                   MOVEL     FRITRT        D8ITRT
     C                   MOVE      FRDTRP        D8DTRP
     C                   MOVE      FRRCVD        D8RCVD
     C                   MOVEL     FRSS#         D8SS#
     C                   MOVEL     FRLSN         D8LSN
     C                   MOVEL     FRFSN         D8FSN
     C                   MOVEL     FRMID         D8MID
     C                   MOVEL     FRADR1        D8ADR1
     C                   MOVEL     FRADR2        D8ADR2
     C                   MOVEL     FRCTY         D8CTY
     C                   MOVEL     FRST          D8ST
     C                   MOVEL     FRZIP         D8ZIP
     C                   MOVEL     FRPHN#        D8PHN#
     C                   MOVE      FRDTOB        D8DTOB
     C                   MOVEL     FRSEX         D8SEX
     C                   MOVEL     FRMSTS        D8MSTS
     C                   MOVE      FRDEP         D8DEP
     C                   MOVE      FRDBDT        D8DBDT
     C                   MOVE      FRDOD         D8DOD
     C                   MOVEL     FRESTS        D8ESTS
     C                   MOVEL     FRCLAS        D8CLAS
     C                   MOVEL     FROCC         D8OCC
     C                   MOVE      FRDOH         D8DOH
      /Free
       D8aww = %Editc(FRaww:'X');
       If FRaww = *zeros;
             D8aww=*blanks;
       Endif;
      /End-Free
     C                   MOVEL     FRWGPD        D8WGPD
     C                   MOVE      FRDAYS        D8DAYS
     C                   MOVE      FRDTLW        D8DTLW
     C                   MOVEL     FRFSAL        D8FSAL
     C                   MOVEL     FRCSAL        D8CSAL
     C                   MOVE      FRRTW         D8RTW
     C     FRPEFD        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D8PEFD
     C                   END
     C     FRPEXD        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D8PEXD
     C                   END
     C     FRACDT        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D8ACDT
     C                   END
     C     FRDTRP        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D8DTRP
     C                   END
     C     FRRCVD        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D8RCVD
     C                   END
     C     FRDTOB        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D8DTOB
     C                   END
     C     FRDBDT        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D8DBDT
     C                   END
     C     FRDOD         IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D8DOD
     C                   END
     C     FRDOH         IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D8DOH
     C                   END
     C     FRDTLW        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D8DTLW
     C                   END
     C     FRRTW         IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D8RTW
     C                   END
     C     FRPHN#        IFEQ      *ZEROS
     C     FRJRST        ANDEQ     'CA'
     C                   MOVE      *BLANKS       D8PHN#
     C                   END
     C*
     C* Clear fields if IXRLS = '3'
     C     IXRLS         IFEQ      '3'
     C                   MOVE      *BLANKS       D8CLSN
     C                   MOVE      *BLANKS       D8PLSN
     C                   MOVE      *BLANKS       D8AAD1
     C                   MOVE      *BLANKS       D8AAD2
     C                   MOVE      *BLANKS       D8PADC
     C                   MOVE      *BLANKS       D8ILSN
     C                   MOVE      *BLANKS       D8ELSN
     C                   MOVE      *BLANKS       D8EAD1
     C                   MOVE      *BLANKS       D8EAD2
     C                   MOVE      *BLANKS       D8SFIN
     C                   MOVE      *BLANKS       D8RPT#
     C                   MOVE      *BLANKS       D8EADR
     C                   MOVE      *BLANKS       D8ACDS
     C                   MOVE      *BLANKS       D8SS#
     C                   MOVE      *BLANKS       D8MID
     C                   MOVE      *BLANKS       D8ADR1
     C                   MOVE      *BLANKS       D8ADR2
     C                   MOVE      *BLANKS       D8PHN#
     C                   MOVE      *BLANKS       D8OCC
     C                   MOVE      *BLANKS       D8CSAL
     C                   MOVE      *BLANKS       D8LSN
     C                   ENDIF                                                  IXRLS='3'
     C                   IF        XXRLSA = *ON
     C                   MOVE      *BLANKS       D8PART
     C                   ENDIF
     C                   MOVEL     DET148        X8DATA
      /Free
       If (XXJST = 'CA' and CLTID = '136') or (XXJST = 'NY' and CLTID = '126');
         eval  %subst(X8DATA:914) = x'0D';
       Endif;
       If (XXJST = 'MA' and CLTID = '129') or (XXJST = 'NH' and CLTID = '129');
         eval  %subst(X8DATA:914) = x'0D';
       Endif;

       %subst(X8DATA:914) = x'0D';
      /End-Free
     C                   WRITE     PX801
     C*
     C     ENXDTL        ENDSR
     C*
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C     SRXR21        BEGSR
     C*
     C                   CLEAR                   X8DATA
     C*
     C                   MOVEL     'R21'         D9TSET
     C                   MOVEL     FRMTCC        D9MTCC
     C                   MOVE      FRMTC2        D9MTC2
     C                   MOVEL     FRJRBC        D9JRBC
     C                   MOVEL     FRACLC        D9ACL#
     C                   MOVEL     FRCAFE        D9CLCD
     C                   MOVEL     FRCLNM        D9CLNM
     C                   MOVEL     FRCLAL        D9CLAL
     C                   MOVEL     FRAAD1        D9AAD1
     C                   MOVEL     FRAAD2        D9AAD2
     C                   MOVEL     FRCLCC        D9CLCC
     C                   MOVEL     FREITQ        D9EITQ
     C                   MOVEL     FREID         D9EID
     C                   MOVEL     FRLSNS        D9LSNS
     C                   MOVEL     FRAURL        D9AURL
     C                   MOVEL     FRSSRL        D9SSRL
     C                   MOVEL     FRLSN         D9LSN
     C                   MOVEL     FRMID         D9MID
     C                   MOVEL     FRADR1        D9ADR1
     C                   MOVEL     FRADR2        D9ADR2
     C                   MOVEL     FRACC         D9ACC
     C                   MOVEL     FRPHN#        D9PHN#
     C                   MOVEL     FRDRIC        D9DRIC
     C                   MOVEL     FRTLC         D9TLC
     C                   MOVEL     FRRWEI        D9RWEI
     C                   MOVEL     FRRTWT        D9RTWT
     C                   MOVEL     FRPRI         D9PRI
     C                   MOVEL     FRIDFE        D9EMP#
     C                   MOVEL     FRILSN        D9ILSN
     C                   MOVEL     FRETC         D9ETC
     C                   MOVEL     FRRPT#        D9RPT#
     C                   MOVEL     FRCLSN        D9CLSN
     C                   MOVEL     FRITCD        D9ITCD
     C                   MOVEL     FRICSN        D9ICSN
     C                   MOVEL     FRAAZP        D9AAZP
     C                   MOVEL     FRACPC        D9ACPC
     C                   MOVEL     FRACSC        D9ACSC
     C                   MOVEL     FRACLN        D9ACLN
     C                   MOVEL     FRACNM        D9ACNM
     C                   MOVEL     FRACTY        D9ACTY
     C                   MOVEL     FRASTR        D9ASTR
     C                   MOVEL     FRACST        D9ACST
     C                   MOVEL     FRACCC        D9ACCC
     C                   MOVEL     FRDODD        D9DBDT
     C                   IF        FRDODD = *ZEROS
     C                   MOVE      *BLANKS       D9DBDT
     C                   ENDIF
     C                   MOVEL     FRELSN        D9ELSN
     C                   MOVEL     FREUI#        D9EUI#
     C                   MOVEL     FREAD1        D9EAD1
     C                   MOVEL     FREAD2        D9EAD2
     C                   MOVEL     FREMCC        D9EMCC
     C                   MOVEL     FREPH#        D9EPH#
     C                   MOVEL     FRECSN        D9ECSN
     C                   MOVEL     FREAL         D9EAL
     C                   MOVEL     FREMCT        D9EMCT
     C                   MOVEL     FREMPC        D9EPCC
     C                   MOVEL     FREMZP        D9EMZP
     C                   MOVEL     FREMA1        D9EMA1
     C                   MOVEL     FREMA2        D9EMA2
     C                   MOVEL     FREMST        D9EMST
     C                   MOVEL     FROCCD        D9OCCD
     C                   MOVE      FRFDDT        D9FDDT
     C                   MOVEL     FRCLSC        D9CLSC
     C                   MOVEL     FRCLTC        D9CLTC
     C                   MOVEL     FRCLRC        D9CLRC
     C                   MOVEL     FRSILC        D9SILC
     C                   MOVEL     FRCOLB        D9COLB
     C                   MOVE      FR#ADN        D9#ADN
     C                   MOVE      FR#DRC        D9#DRC
     C                   MOVE      FR#DRN        D9#DRN
     C                   MOVE      FR#MCO        D9#MCO
     C                   MOVE      FR#WN         D9#WN
     C                   IF        XXRLSA = *ON
     C                   MOVE      FR#PBI        D9#PBI
     C                   MOVE      FR#DE         D9#DE
     C                   MOVE      FR#CE         D9#CE
     C                   MOVEL     FRVS21        D9VS21
     C                   ELSE
     C                   MOVE      FRVS21        WKVS21
     C                   MOVEL     WKR3A         D9#PBI
     C                   MOVEL     WKR3B         D9#DE
     C                   MOVEL     WKR3C         D9#CE
     C                   MOVEL     WKR3D         D9FIL9
     C                   MOVEL     WKR3          D9VS21
     C                   ENDIF
     C*
     C     FRFDDT        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D9FDDT
     C                   END
     C     FRMTC2        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D9MTC2
     C                   END
     C*
      /Free
       D9WWTC = FRWWTC;
       D9WDSC = FRWDSC;
       D9ESID = FRESID;
       D9INJS = FRINJS;
       D9SEMP = FRSEMP;
       D9CLSB = FRCLSB;
       D9DRES = FRDRES;
       D9ACCD = FRACCD;
       //     EDI 3.1 --> XXRLSA = '*ON'
       IF  (XXRLSA = *ON);
      /End-Free
     C                   MOVE      FRIDLT        D9IDLT
     C     FRIDLT        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D9IDLT
     C                   END
     C                   MOVE      FRRTW         D9RTW
     C     FRRTW         IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D9RTW
     C                   END
     C                   MOVE      FRCDDB        D9CDDB
     C     FRCDDB        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D9CDDB
     C                   END
     C                   MOVE      FRCDLW        D9CDLW
     C     FRCDLW        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D9CDLW
     C                   END
     C                   MOVE      FRCDEK        D9CDEK
     C     FRCDEK        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D9CDEK
     C                   END
     C                   MOVE      FRCDAK        D9CDAK
     C     FRCDAK        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       D9CDAK
     C                   END
      /Free
        D9IRTC = FRIRTC;
        D9IRPR = FRIRPR;
        D9IRSE = FRIRSE;
        D9LRTC = FRLRTC;
        D9LRPR = FRLRPR;
        D9LRSE = FRLRSE;
        D9CAEM = FRCAEM;
        D9CARP = FRCARP;
        D9CAPH = FRCAPH;
        D9RWEI = *BLANKS;
        //*** Beg Add ***  02/14/22 ********************************************
        Monitor;
        D9arDt = %Char(%date(FRarDt:*cymd):*iso0);
        On-error;
           D9arDt = *blanks;
        Endmon;
        //*** End Add ***  02/14/22 ********************************************
       EndIf;
      /End-Free
     C                   MOVEL     DETR21        X8DATA
      /Free
       //     EDI 3.1 --> XXRLSA = *ON
       IF  (XXRLSA = *ON);
       IF  (XXJST = 'CA' and CLTID = '136') or (XXJST = 'NY' and CLTID = '126');
             xxpos = (fr#adn * 50) + (fr#drc * 2) +
                     (fr#drn * 50) + (fr#mco * 71) +
                     (fr#wn * 75)  + (fr#pbi * 24) +
                     (fr#de * 40)  + (fr#ce * 196) +
                      1612 + 1;

            %subst(X8DATA:xxpos) = x'0D';
       Endif;

       IF (XXJST = 'MA' and CLTID = '129') or (XXJST = 'NH' and CLTID = '129');
           xxpos = (fr#adn * 50) + (fr#drc * 2) +
                   (fr#drn * 50) + (fr#mco * 71) +
                   (fr#wn * 75)  + (fr#pbi * 24) +
                   (fr#de * 40)  + (fr#ce * 196) +
                    1612 + 1;

         %subst(X8DATA:xxpos) = x'0D';
       Endif;

           xxpos = (fr#adn * 50) + (fr#drc * 2) +
                   (fr#drn * 50) + (fr#mco * 71) +
                   (fr#wn * 75)  + (fr#pbi * 24) +
                   (fr#de * 40)  + (fr#ce * 196) +
                    1612 + 1;
         %subst(X8DATA:xxpos) = x'0D';

       Else;
       // NOT EDI 3.1 --> XXRLSA = '*OFF'
       Select;
       When (XXJST = 'CA' and CLTID = '136') or
            (XXJST = 'NY' and CLTID = '126');
           xxpos = (fr#adn * 50) + (fr#drc * 2) +
                   (fr#drn * 50) + (fr#mco * 71) +
                   (fr#wn * 75)  + 1600 + 1;

         %subst(X8DATA:xxpos) = x'0D';



       When (XXJST = 'MA' and CLTID = '129') or
            (XXJST = 'NH' and CLTID = '129');

           xxpos = (fr#adn * 50) + (fr#drc * 2) +
                   (fr#drn * 50) + (fr#mco * 71) +
                   (fr#wn * 75)  + 1600 + 1;

         %subst(X8DATA:xxpos) = x'0D';


       Other;
           xxpos = (fr#adn * 50) + (fr#drc * 2) +
                   (fr#drn * 50) + (fr#mco * 71) +
                   (fr#wn * 75)  + 1600 + 1;
         %subst(X8DATA:xxpos) = x'0D';
       Endsl;

       Endif;
      /End-Free
     C                   WRITE     PX801
     C*
     C                   ENDSR
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*
     C* FORMAT 148 TRAILER AND WRITE TO FLAT FILE.
     C*
     C     SRXTRL        BEGSR
     C*
     C                   CLEAR                   X8DATA
     C*
     C     IXRLS         IFEQ      '3'
     C                   MOVEL     'TR2'         T8TTRL
     C                   MOVE      XXTCNT        T8TCNT
     C                   ELSE
     C                   MOVEL     'TR1'         T8TTRL
     C                   ENDIF
     C                   MOVE      XXRCNT        T8RCNT
     C*
     C                   MOVEL     TRL148        X8DATA
      /Free
       If (XXJST = 'CA' and CLTID = '136' and IXRLS = '1');
         eval  %subst(X8DATA:13) = x'0D';
       Endif;

       If    (XXJST = 'NY' and CLTID = '126' and IXRLS = '3');
              eval  %subst(X8DATA:22)= x'0D';
       Endif;

       If (XXJST = 'MA' and CLTID = '129' and IXRLS = '3');
         eval  %subst(X8DATA:22) = x'0D';
       Endif;

       If    (XXJST = 'NH' and CLTID = '129' and IXRLS = '3');
              eval  %subst(X8DATA:22)= x'0D';
       Endif;

      /End-Free
     C                   WRITE     PX801
     C*
     C     ENXTRL        ENDSR
