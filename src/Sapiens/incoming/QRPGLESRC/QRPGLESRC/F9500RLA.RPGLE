     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F9500RLA                                          *
     H*    DESCRIPTION: Create LA Data Submission Workfile and        *
     H* Read each EOMB:                                               *
     H*      Separate out by "type" of code: HOSP vs. OTHR vs. MEDI   *
     H*      Write to different file for each "type"                  *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  10/11/94  C:20    REQ/PROJ: 90075/LA009                      *
     H*                    1. REVISED TAPE SABMISSION PROCESS PER NEW *
     H*                       REQUIREMENTS.                           *
     H*                         HOSPITAL:                             *
     H*                           PARISH CODE                         *
     H*                           PROVIDER NAME                       *
     H*                           CHARGE AMOUNT                       *
     H*                           TYPE OF SERVICE                     *
     H*                           LENGTH OF STAY                      *
     H*                           IP/OP INDICATOR                     *
     H*                         MEDICAL                               *
     H*                           CPT4 CODE                           *
     H*                           MODIFIER                            *
     H*                           UNITS                               *
     H*                           AMOUNT CHARGED                      *
     H*                           AMOUNT PAID                         *
     H*                         DENT                                  *
     H*                           CDT-1 CODE                          *
     H*                           PROVIDER NAME                       *
     H*                           PROVIDER SRTEET ADDRESS             *
     H*                           CHARGE AMOUNT                       *
     H*                           PLACE OF TREATMENT                  *
     H*                           DATE OF INJURY (YY/MM/DD)           *
     H*                           CLAIMANT NAME                       *
     H*                           CLAIMANT SS#                        *
     H*                           EMPLOYER NAME                       *
     H*                           DATE OF PAYMENT (YY/MM/DD/)         *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*****************************************************************
     H*  FILES:                                                       *
     H*  FDDDPLA  - DENTAL DATA SUBMISSION                            *
     H*  FDHDPLA  - HOSPITAL DATA SUBMISSION                          *
     H*  FDMDPLA  - MEDICAL DATA SUBMISSION                           *
     H*  FDODPLA  - OTHER DATA SUBMISSION                             *
     H*****************************************************************
     FFDDDPLA   O    E             DISK
     FFDHDPLA   O    E             DISK
     FFDMDPLA   O    E             DISK
     FFDODPLA   O    E             DISK
     FFAEOL4    IF   E           K DISK
     FFDCAL5    IF   E           K DISK
     FFMP1L     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMEML     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFAE1L     IF   E           K DISK
     FFDHCL     IF   E           K DISK
     FFMSVL1LA  IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK    USROPN
     F                                     RENAME(PEM01:WEM01)
     FWMCML1    IF   E           K DISK    USROPN
     F                                     RENAME(PCM01:WCM01)
     FWDPAL4    IF   E           K DISK    USROPN

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXCMSV                 1     11
     D  XXRVCD                 1      3  0
     D  XXHCPC                 4      8
     D  XXUNIT                 9     11  0
     D                 DS
     D  XXCPT4                 1     11
     D  XFIRST                 1      1
     D  XEND                   2      5
     D*
     D* External Data Structure
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IWCM01
     I              CMCO#                       W1CO#
     I              CMFND                       W1FND
     I              CMFYR                       W1FYR
     I              CMEMP#                      W1EMP#
     I              CMDIV                       W1DIV
     I              CMCASE                      W1CASE
     I              CMSS#                       W1SS#
     I              CMACDT                      W1ACDT
     I              CMACTM                      W1ACTM
     I              CMDTRP                      W1DTRP
     I              CMDTLW                      W1DTLW
     I              CMACDS                      W1ACDS
     I              CMMCON                      W1MCON
     I              CMRT01                      W1RT01
     I              CMAR01                      W1AR01
     I              CMDTRE                      W1DTRE
     I              CMSTAT                      W1STAT
     I              CMUSER                      W1USER
     I              CMPGM                       W1PGM
     I              CMTIME                      W1TIME
     I              CMDT                        W1DT
     I              CMENTD                      W1ENTD
     I              CMRCVD                      W1RCVD
     I*** BEGIN ADD *** 03/09/98 **************************************
     I              CMOSCL                      WCOSCL
     I*** END   ADD *** 03/09/98 **************************************
     IWEM01
     I              EMCO#                       WMCO#
     I              EMFND                       WMFND
     I              EMEMP#                      WMEMP#
     I              EMDIV                       WMDIV
     I              EMNAME                      WMNAME
     I              EMMAD1                      WMMAD1
     I              EMMAD2                      WMMAD2
     I              EMMCTY                      WMMCTY
     I              EMMST                       WMMST
     I              EMMZIP                      WMMZIP
     I              EMPAD1                      WMPAD1
     I              EMPAD2                      WMPAD2
     I              EMPCTY                      WMPCTY
     I              EMPST                       WMPST
     I              EMPZIP                      WMPZIP
     I              EMAREA                      WMAREA
     I              EMPHN#                      WMPHN#
     I              EMRGN#                      WMRGN#
     I              EMFEIN                      WMFEIN
     I              EMVIP                       WMVIP
     I              EMMPFL                      WMMPFL
     I              EMSTRP                      WMSTRP
     I              EMDBA                       WMDBA
     I              EMSTAT                      WMSTAT
     I              EMUSER                      WMUSER
     I              EMPGM                       WMPGM
     I              EMTIME                      WMTIME
     I              EMDT                        WMDT

     C*
     C* KEY LIST FOR DETAIL CHARGE FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C* KEY LIST FOR PROVIDER AUXILIARY FILE
     C*
     C     P1KEY         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*
     C* KEY LIST FOR CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*
     C* KEY LIST FOR EMPLOYER MASTER FILE (WCMC)
     C*
     C     EMKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    XXEMP#
     C*
     C* KEY LIST FOR GROUP/FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C*
     C* KEY LIST FOR EOMB AUXILIARY FILE
     C*
     C     E1KEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C* KEY LIST FOR DETAIL CHARGE AUXILIARY FILE
     C*
     C     HCKEY         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CADSEQ
     C*
     C* KEY LIST FOR THE COMMON SERVICE CODE FILE
     C*
     C     SVKEY         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    XXCMSV
     C                   KFLD                    CAFDAT
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* KEY LIST FOR WCMC/FSMS SYSTEM CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY
     C*
     C* KEY LIST FOR THE CLAIM MASTER FILE (WCMS)
     C*
     C     CMKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    WCRFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*
     C* KEY LIST FOR EMPLOYER MASTER FILE (WCMC)
     C*
     C     EMKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    WCRFND
     C                   KFLD                    W1EMP#
     C                   KFLD                    W1DIV
     C*
     C* KEY LIST FOR THE PAYMENT DETAIL FILE (WCMS)
     C*
     C     PAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    WCRFND
     C                   KFLD                    EOWCCK
     C*
     C* INITIALIXE WORK FIELDS
     C*
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    CAICD9        XXICD9
     C     *LIKE         DEFINE    CAPMOD        XXPMOD
     C*
     C                   MOVE      *BLANKS       X1ADR1           15            FOR COMBINE A/C/S
     C                   MOVE      *BLANKS       X1CTY            10            FOR COMBINE A/C/S
     C                   MOVE      *BLANKS       X1FIPS            3            PROV CNTY WORK FIELD
     C                   MOVE      *BLANKS       X1FST             1            1ST POSITION CACPT4
     C                   MOVE      *BLANKS       X1ST              2            FOR COMBINE A/C/S
     C                   MOVE      *BLANKS       X1TYPE            6            PROV FACI WORK FIELD
     C*
     C                   MOVE      *BLANKS       XSKEY             2            FOR SYS CNTRL FILE
     C*
     C                   Z-ADD     *ZEROS        XXACDT            7 0          ACDT WORK FIELD
     C                   MOVE      *BLANKS       XXACS            30            ADR/CITY/STATE
     C                   Z-ADD     *ZEROS        XXAMTC           10 0          TOTAL AMT CHG
     C                   Z-ADD     *ZEROS        XXAMTP           10 0          TOTAL AMT PAID
     C                   Z-ADD     *ZEROS        XXCKDT            7 0          CHK DT PATRDT/EOTRDT
     C                   Z-ADD     *ZEROS        XXUNTS            3 0          UNIT CAANMN OR CAUNT
     C                   MOVE      *BLANKS       XXCNAM           30            CLAIM NAME WRK FIELD
     C                   Z-ADD     *ZEROS        XXCRDT            7 0          E1CRDT
     C                   MOVE      *BLANKS       XXCS             15            CITY/STATE
     C                   Z-ADD     *ZEROS        XXDAYS            5 0          PARM FOR SRDDIF
     C                   MOVE      *BLANKS       XXENAM           30            EMPL NAME WORK FIELD
     C                   MOVE      *BLANKS       XXERR             1            PARM FOR SRDDIF
     C                   Z-ADD     *ZEROS        XXFDAT            7 0          EOFRDT IN MMDDYY
     C                   Z-ADD     *ZEROS        XXFIPS            3 0          PROV CNTY WORK FIELD
     C                   MOVE      *BLANKS       XXFRDC            8 0          PARM FOR SRDDIF
     C                   MOVE      *BLANKS       XXFRST           15            USED IN SRADDR
     C                   MOVE      *BLANKS       XXLAST           56            USED IN SRADDR
     C                   Z-ADD     *ZEROS        XXLOS             4 0          LOS
     C                   MOVE      *BLANKS       XXPADR           30            PROV ADDR WORK FIELD
     C                   MOVE      *BLANKS       XXPNAM           30            PROV NAME WORK FIELD
     C                   MOVE      *BLANKS       XXPOS             2            PLACE OF SERVICE
     C                   Z-ADD     *ZEROS        XXPZIP            9 0          PROV ZIP WORK FIELD
     C                   MOVE      *BLANKS       XXSS#             9            SS# WORK FIELD
     C                   Z-ADD     *ZEROS        XXTDAT            7 0          EOTODT IN MMDDYY
     C                   MOVE      *BLANKS       XXTERM            1            USED IN SRADDR
     C                   MOVE      *BLANKS       XXTODC            8 0          PARM FOR SRDDIF
     C                   MOVE      *BLANKS       XXTSRV           30            TYPE OF SRV WRK FLD
     C                   Z-ADD     *ZEROS        XXTYPE            6 0          PROV FACI WORK FIELD
     C                   Z-ADD     *ZEROS        Q                 2 0
     C                   Z-ADD     *ZEROS        WCRFND            3 0          WCMS XREF FUND #
     C                   Z-ADD     0             HLADDT            7 7          To hold MVR
     C                   Z-ADD     0             HLANMN           10 7          CAANMN for 15 minute
     C                   Z-ADD     0             HLANTV            3 0          Anes Time Value
     C                   MOVE      *BLANKS       XXIOP             1            IN/OUT PAT FLAG
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C* CHAIN TO THE USER PARAMETER FILE TO GET COMPANY NUMBER
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C* CHAIN TO THE WCMC/FSMS SYSTEM CONTROL FILE TO SEE IF WCMS INTERFACE
     C* IS INSTALLED
     C*
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C     FBFL01        IFEQ      'Y'
     C                   MOVE      '1'           *IN71
     C                   OPEN      WMCML1
     C                   OPEN      WMEML
     C                   OPEN      WDPAL4
     C                   ELSE                                                   ELSE FBFL01 = 'Y'
     C                   MOVE      '0'           *IN71
     C                   END                                                    END FBFL01 = 'Y'
     C                   ELSE                                                   ELSE 8IN60 = '0'
     C                   MOVE      '0'           *IN71
     C                   END                                                    END *IN60 = '0'
     C*
     C* READ THE EOMB FILE
     C*
     C                   READ      PEO01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C* ONLY PROCESS IF EOAST IS LA
     C*
     C     EOAST         IFEQ      'LA'
     C*
     C* EXECUTE SUBROUTINE TO GET HEADER IFORMATION FROM EOMB RECORD
     C*
     C                   EXSR      SREOMB
     C*
     C* CHAIN TO THE DETAIL CHARGE FILE AND READ ALL DETAIL CHARGES FOR
     C* THIS EOMB RECORD
     C*
     C     CAKEY         CHAIN     PCA01                              55
     C     *IN55         DOWEQ     '0'
     C*
     C* DEPENDING ON TYPE OF CODE JUST READ EXECUTE THE PROPER SUBROUTINE
     C*
     C     CAPRTY        IFEQ      '4'
     C*
     C                   EXSR      SRHOSP
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             Q
     C     1             SUBST     CACPT4:Q      X1FST                  65
     C     CAPRTY        IFEQ      '1'
     C     X1FST         ANDEQ     'D'
     C                   EXSR      SRDENT
     C*
     C                   ELSE                                                   ELSE CAPTY = '4'
     C*
     C     CAPRTY        IFEQ      '1'
     C     CACPT4        ANDGE     '00100'
     C     CACPT4        ANDLE     '99999'
     C     CAPRTY        OREQ      '1'
     C     CACPT4        ANDGE     'OT010'
     C     CACPT4        ANDLE     'OT680'
     C     CAPRTY        OREQ      '1'
     C     CACPT4        ANDGE     'PT010'
     C     CACPT4        ANDLE     'PT680'
     C*
     C                   EXSR      SRMEDI
     C*
     C                   ELSE                                                   ELSE CAPTY ='1'...
     C*
     C     CAPRTY        IFEQ      '1'
     C*
     C                   EXSR      SROTHR
     C*
     C                   END                                                    END CAPRTY = '1'
     C                   END                                                    END CAPRTY = '1'...
     C                   END                                                    END CAPRTY = '1'...
     C                   END                                                    END CAPRTY = '4'
     C*
     C* READ FOR THE NECT DETAIL CHARGE RECORD FOR THIS EOMB
     C*
     C     CAKEY         READE     PCA01                                  55
     C                   END                                                    END DOWEQ *IN55
     C*
     C                   END                                                    END EOAST = 'LA'
     C*
     C* READ THE NEXT EOMB RECORD
     C*
     C                   READ      PEO01                                  50
     C                   END                                                    END DOWEQ *IN50
     C*
     C* END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*******************************************************************
     C* SUBROUTINE TO LOAD THE ADR1, CTY, STATE INTO 30 BYTE FIELD      *
     C*******************************************************************
     C     SRADDR        BEGSR
     C*
     C* COMBINE THE CITY AND THE STATE
     C*
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXCS
     C                   MOVEL     X1CTY         XXLAST
     C                   MOVEL     X1ST          XXFRST
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCS
     C*
     C* COMBINE THE ADDRESS WITH THE COMBINED CITY/STATE FIELD
     C*
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXACS
     C                   MOVEL     X1ADR1        XXLAST
     C                   MOVEL     XXCS          XXFRST
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXACS
     C*
     C                   ENDSR
     C*******************************************************************
     C* SRDENT - SUBROUTINE TO PROCESS DENTAL INFORMATION               *
     C*******************************************************************
     C* SRDENT: DENTAL INFORMATION:
     C*
     C     SRDENT        BEGSR
     C*
     C* IF CACPT4 IS A DENTAL CODE REPLACE THE FIRST CHARACTER(D) WITH A 0
     C*
     C                   MOVE      *BLANKS       XXCPT4
     C                   MOVE      '0'           XFIRST
     C                   Z-ADD     2             Q
     C     4             SUBST     CACPT4:Q      XEND                   65
     C*
     C                   MOVEL     XXCPT4        XXCPT4
     C*
     C                   Z-ADD     *ZEROS        XXAMTC
     C                   MOVE      CATOTC        XXAMTC
     C*
     C* CHAIN TO THE DETAIL CHARGE AUXILIARY FILE TO GET THE PLACE OF SERVICE
     C*
     C                   MOVE      *BLANKS       XXPOS
     C     HCKEY         CHAIN     PHC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      HCPOS         XXPOS
     C                   ELSE                                                   ELSE *IN60 = '0'
     C                   MOVE      *BLANKS       XXPOS
     C                   END                                                    END *IN60 = '0'
     C*
     C* WRITE RECORD TO THE OTHER DATA SUBMISSION FILE
     C*
     C                   MOVEL     XXCPT4        DDCPT4
     C                   MOVEL     XXPNAM        DDPNAM
     C                   MOVEL     XXPADR        DDPADR
     C                   Z-ADD     XXAMTC        DDAMTC
     C                   MOVE      XXPOS         DDPOS
     C                   Z-ADD     XXACDT        DDACDT
     C                   MOVEL     XXCNAM        DDCNAM
     C                   MOVEL     XXSS#         DDSS#
     C                   MOVEL     XXENAM        DDENAM
     C                   Z-ADD     XXCKDT        DDCKDT
     C                   WRITE     PDD01
     C*
     C                   ENDSR
     C*******************************************************************
     C* SREOMB - SUBROUTINE TO PROCESS THE HEADER/EOMB INFORMATION      *
     C*******************************************************************
     C* SREOMB: header fields
     C*  Used for codes with:
     C*        all types
     C* 30A XXPNAM = Use FAEOP/EOPNAM
     C* 30A XXPADR = Use FAEOP/EOADR1, EOCTY, EOST
     C*  3N XXFIPS = Use FAEOP/EOFEIN & EOMOD to get P1CNTY (middle 4 bytes)
     C*  6N XXTYPE = Use FAEOP/EOFEIN & EOMOD to get P1CNTY (last 6 bytes)
     C*  4N XXLOS  = Use FAEOP/EOFRDT to EOTODT to determine number of days
     C*  6N XXCRDT = Use FAE1P/E1CRDT  (YYMMDD)
     C*  6N XXCKDT = Use FAEOP/EOWCCK to get WMCMP/WDPAP (YYMMDD)
     C*                  If no hit, use EOTRDT
     C*
     C     SREOMB        BEGSR
     C*
     C                   MOVEL     EOPNAM        XXPNAM
     C*
     C* COMBINE EOADR1, EOCTY, EOST TO FORM A 30 LONG ADDRESS FIELD
     C*
     C                   MOVE      *BLANKS       X1ADR1
     C                   MOVE      *BLANKS       X1ST
     C                   MOVE      *BLANKS       X1CTY
     C                   MOVE      *BLANKS       XXPADR
     C*
     C                   MOVEL     EOADR1        X1ADR1
     C                   MOVEL     EOST          X1ST
     C                   MOVEL     EOCTY         X1CTY
     C                   EXSR      SRADDR
     C                   MOVEL     XXACS         XXPADR
     C*
     C                   Z-ADD     EOZIP         XXPZIP
     C*
     C* CHAIN TO THE PROVIDER AUXILIARY FILE TO GET THE FIPS CODE FROM
     C* THE COUNTY CODE
     C*
     C     P1KEY         CHAIN     PP101                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     6             Q
     C     4             SUBST     P1CNTY:Q      X1FIPS                 65
     C                   MOVE      X1FIPS        XXFIPS
     C                   Z-ADD     10            Q
     C     6             SUBST     P1CNTY:Q      X1TYPE                 65
     C                   MOVE      X1TYPE        XXTYPE
     C                   ELSE                                                   ELSE *IN60 = '0'
     C                   Z-ADD     *ZEROS        XXFIPS
     C                   Z-ADD     *ZEROS        XXTYPE
     C                   END                                                    END *IN60 = '0'
     C*
     C* CHAIN TO THE CLAIM MASTER FILE TO GET ACDT/SS#
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     CMACDT        XXACDT
     C                   MOVEL     CMSS#         XXSS#
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   ELSE                                                   ELSE *IN60 = '0'
     C                   Z-ADD     *ZEROS        XXACDT
     C                   MOVE      *BLANKS       XXSS#
     C                   Z-ADD     *ZEROS        XXEMP#
     C                   END                                                    END *IN60 = '0'
     C*
     C* CHAIN TO THE CLAIM DEMOGRAPHICS FILE TO GET THE CLAIMANT NAME
     C*
     C     XXSS#         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVEL     CDLSN         XXLAST
     C                   MOVEL     CDFSN         XXFRST
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCNAM
     C                   ELSE
     C                   MOVE      *BLANKS       XXCNAM
     C                   END
     C*
     C* CHAIN TO THE EMPLOYER FILE TO GET THE EMPLOYER NAME
     C*
     C* IF WCMS INTERFACE INSTALLED (I.E. *IN71 = '1'):
     C*  1.CHAIN TO THE WCMC GROUP/FUND FILE TO GET THE WCMS CROSS REFERECNE FND
     C*  2.CHAIN TO WCMS CLAIM FILE WITH CROSS REFERENCE FUND NUMBER
     C*    TO GET THE EMPLOYER NUMBER AND DIVISION
     C*  3.CHAIN TO WCMS EMPLOYER MASTER FILE TO GET EMPLOYER NAME
     C* IF WCMS INTERFACE NOT INSTALLED (I.E. *IN71 = 0)
     C*  1.CHAIN TO WCMC EMPLOYER FILE TO GET EMPLOYER NAME
     C*
     C     *IN71         IFEQ      '1'
     C*
     C* CHAIN TO WCMC GROUP/FUND FILE TO GET THE WCMS XREF FUND NUMBER
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     FNWCF#        WCRFND
     C                   ELSE                                                   ELSE *IN60 = '0'
     C                   Z-ADD     *ZEROS        WCRFND
     C                   END                                                    END *IN60 = '0'
     C*
     C* CHAIN TO THE WCMS CLAIM MASTER FILE TO GET EMPLOYER NUMBER AND DIV
     C*
     C     CMKY1         CHAIN     WCM01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZEROS        W1EMP#
     C                   Z-ADD     *ZEROS        W1DIV
     C                   END                                                    END *IN60 = '1'
     C*
     C* CHAIN TO THE WCMS EMPLOYER MASTER FILE TO GET EMPLOYER NAME
     C*
     C                   MOVE      *BLANKS       XXENAM
     C     EMKY1         CHAIN     WEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     WMNAME        XXENAM
     C                   ELSE                                                   ELSE *IN60 = '0'
     C                   MOVE      *BLANKS       XXENAM
     C                   END                                                    END *IN60 = '0'
     C*
     C* CHAIN TO THE WCMS PAYMENT DETAIL FILE TO GET THE CHECK DATE
     C*
     C     EOWCCK        IFNE      *ZEROS
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     PATRDT        XXCKDT
     C                   ELSE                                                   ELSE *IN60 = '0'
     C                   Z-ADD     EOTRDT        XXCKDT
     C                   END                                                    END *IN60 = '0'
     C                   ELSE
     C                   Z-ADD     EOTRDT        XXCKDT
     C                   END
     C*
     C                   ELSE                                                   ELSE *IN71 = '1'
     C*
     C* CHAIN TO THE WCMC EMPLOYER FILE TO GET THE EMPLOYER NAME
     C*
     C                   MOVE      *BLANKS       XXENAM
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXENAM
     C                   ELSE                                                   ELSE *IN60 = '0'
     C                   MOVE      *BLANKS       XXENAM
     C                   END                                                    END *IN60 = '0'
     C*
     C* IF WCMS INTERFACE NOT INSTALLED LOAD EOTRDT IN THE CHECK DATE FIELD
     C*
     C                   Z-ADD     EOTRDT        XXCKDT
     C*
     C                   END                                                    END *IN71 = '1'
     C*
     C* DETERMINE THE LENGTH OF STAY EOFRDT VS. EOTODT
     C*
     C                   Z-ADD     EOTODT        XXTDAT
     C                   Z-ADD     EOFRDT        XXFDAT
     C                   Z-ADD     XXFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDC
     C*
     C                   Z-ADD     XXTDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODC
     C*
     C                   Z-ADD     *ZEROS        XXDAYS
     C                   MOVE      *BLANKS       XXERR
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXFRDC
     C                   PARM                    XXTODC
     C                   PARM                    XXDAYS
     C                   PARM                    XXERR
     C                   PARM                    XXTERM
     C*
     C* IF ERROR CONDITION OCCURED ADD ZEROS TO LOS ELSE ADD DIFFERENCE
     C*
     C     XXERR         IFEQ      '1'
     C                   Z-ADD     *ZEROS        XXLOS
     C                   ELSE                                                   ELSE XXERR = '1'
     C                   Z-ADD     XXDAYS        XXLOS
     C     XXLOS         ADD       1             XXLOS
     C                   END                                                    END XXERR = '1'
     C*
     C* CHAIN TO THE EOMB AUXILIARY FILE TO GET E1CRDT
     C*
     C     E1KEY         CHAIN     PE101                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     E1CRDT        XXCRDT
     C                   ELSE                                                   ELSE *IN60 = '0'
     C                   Z-ADD     *ZEROS        XXCRDT
     C                   END                                                    END *IN60 = '0'
     C*
     C                   ENDSR
     C*******************************************************************
     C* SRHOSP - SUBROUTINE TO PROCESS SPECIFIC HOSPITAL INFORMATION    *
     C*******************************************************************
     C* SRHOSP: Hospital
     C*  Used for codes with:
     C*        CAPRTY = 4
     C*
     C* 10N XXAMTC = Use FDCAP/CATOTC
     C* 30A XXTSRV = Use FDCAP/Common Service Code to get FMSVPLA code's desc
     C*
     C     SRHOSP        BEGSR
     C*
     C* DECIDE WHICH EODIA CODE TO USE FOR THIS RECORD
     C*
     C                   MOVE      *BLANKS       XXICD9
     C     CADXR         IFEQ      1
     C                   MOVEL     EODIA1        XXICD9
     C                   ELSE                                                   ELSE CADXR = 1
     C     CADXR         IFEQ      2
     C                   MOVEL     EODIA2        XXICD9
     C                   ELSE                                                   ELSE CADXR = 2
     C     CADXR         IFEQ      3
     C                   MOVEL     EODIA3        XXICD9
     C                   ELSE                                                   ELSE CADXR = 3
     C     CADXR         IFEQ      4
     C                   MOVEL     EODIA4        XXICD9
     C                   END                                                    END CADXR = 4
     C                   END                                                    END CADXR = 3
     C                   END                                                    END CADXR = 4
     C                   END                                                    END CADXR = 5
     C*
     C                   Z-ADD     *ZEROS        XXAMTC
     C                   MOVE      CATOTC        XXAMTC
     C*
     C* CHAIN TO THE DETAIL CHARGE AUXILIARY FILE TO GET THE PLACE OF SERVICE
     C*
     C                   MOVE      *BLANKS       XXPOS
     C     HCKEY         CHAIN     PHC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      HCPOS         XXPOS
     C                   ELSE                                                   ELSE *IN60 = '0'
     C                   MOVE      *BLANKS       XXPOS
     C                   END                                                    END *IN60 = '0'
     C*
     C* CHAIN TO THE COMMON SERVICE CODE FILE TO GET THE CODE DESCRIPTION
     C*
     C                   Z-ADD     CASVCD        XXRVCD
     C                   MOVE      CACPT4        XXHCPC
     C                   Z-ADD     *ZEROS        XXUNIT
     C                   MOVE      *BLANKS       XXTSRV
     C*
     C     SVKEY         SETGT     PSV01
     C     NXTSV1        TAG
     C                   READP     PSV01                                  60    RECORD
     C     *IN60         IFEQ      '0'
     C     SVCO#         ANDEQ     CACO#
     C     SVCMSV        ANDEQ     XXCMSV
     C     SVSTAT        CABEQ     'I'           NXTSV1
     C                   MOVEL     SVDESC        XXTSRV
     C                   ELSE                                                   ELSE *IN60 = '0'
     C                   MOVE      *BLANKS       XXTSRV
     C                   END                                                    END *IN60 = '0'
     C*
     C* MOVE FLAG FOR IN/OUT PATIENT
     C*
     C                   MOVE      *BLANKS       XXIOP
     C     CAIPOP        IFEQ      '1'
     C                   MOVE      'I'           XXIOP
     C                   ELSE
     C                   MOVE      'O'           XXIOP
     C                   END
     C*
     C*** BEGIN CHG *** 10/11/94 ***********************************************
     C*
     C* WRITE RECORD TO THE HOSPITAL DATA SUBMISSION FILE
     C*
     C**********           MOVELXXICD9    HDICD9
     C**********           MOVELXXPADR    HDPADR
     C**********           MOVE XXPOS     HDPOS
     C**********           Z-ADDXXTYPE    HDTYPE
     C**********           Z-ADDXXACDT    HDACDT
     C**********           MOVELXXCNAM    HDCNAM
     C**********           MOVELXXSS#     HDSS#
     C**********           MOVELXXENAM    HDENAM
     C**********           Z-ADDXXCRDT    HDCRDT
     C**********           Z-ADDXXCKDT    HDCKDT
     C                   Z-ADD     XXFIPS        HDFIPS
     C                   MOVEL     XXPNAM        HDPNAM
     C                   MOVEL     XXTSRV        HDTSRV
     C                   MOVE      XXAMTC        HDAMTC
     C                   Z-ADD     XXLOS         HDLOS
     C                   MOVE      XXIOP         HDIOP
     C                   WRITE     PHD01
     C*** END CHG *** 10/11/94 *************************************************
     C*
     C                   ENDSR
     C*******************************************************************
     C* SRMEDI - SUBROUTINE TO PROCESS SPECIFIC MEDICAL INFORMATION     *
     C* FOR CODES IN RANGES: 00100-99999, OT010-OT680, PT010-PT680      *
     C*******************************************************************
     C* SRMEDI: Medical
     C*  Used for codes with:
     C*        CAPRTY = 1
     C*  AND   CACPT4 = 00100-99999, OT010-OT680, PT010-PT680
     C*
     C*  5A XXCODE = Use FDCAP/CACPT4
     C*  2A XXPMOD = Use FDCAP/CAPMOD
     C* 10N XXAMTC = Use FDCAP/CATOTC
     C*
     C     SRMEDI        BEGSR
     C*
     C                   MOVE      *BLANKS       XXCPT4
     C                   MOVEL     CACPT4        XXCPT4
     C*
     C* IF CACPT4 IS A DENTAL CODE REPLACE THE FIRST CHARACTER(D) WITH A 0
     C*
     C                   Z-ADD     1             Q
     C     1             SUBST     CACPT4:Q      X1FST                  65
     C     X1ST          IFEQ      'D'
     C                   MOVE      '0'           XFIRST
     C                   END                                                    END X1ST = 'D'
     C*
     C                   Z-ADD     *ZEROS        XXAMTC
     C                   MOVE      CATOTC        XXAMTC
     C                   Z-ADD     *ZEROS        XXAMTP                         ADD 10/11/94
     C                   MOVE      CATOTP        XXAMTP                         ADD 10/11/94
     C*
     C* IF CAPMOD IS 'RT' MOVE *BLANKS TO XXPMOD ELSE MOVE CAPMOD
     C*
     C                   MOVE      *BLANKS       XXPMOD
     C     CAPMOD        IFEQ      'RT'
     C                   MOVE      *BLANKS       XXPMOD
     C                   ELSE                                                   ELSE CAPMOD = 'RT'
     C                   MOVEL     CAPMOD        XXPMOD
     C                   END                                                    END CAPMOD = 'RT'
     C*
     C* DECIDE WHICH EODIA CODE TO USE FOR THIS RECORD
     C*
     C                   MOVE      *BLANKS       XXICD9
     C     CADXR         IFEQ      1
     C                   MOVEL     EODIA1        XXICD9
     C                   ELSE                                                   ELSE CADXR = 1
     C     CADXR         IFEQ      2
     C                   MOVEL     EODIA2        XXICD9
     C                   ELSE                                                   ELSE CADXR = 2
     C     CADXR         IFEQ      3
     C                   MOVEL     EODIA3        XXICD9
     C                   ELSE                                                   ELSE CADXR = 3
     C     CADXR         IFEQ      4
     C                   MOVEL     EODIA4        XXICD9
     C                   END                                                    END CADXR = 4
     C                   END                                                    END CADXR = 3
     C                   END                                                    END CADXR = 4
     C                   END                                                    END CADXR = 5
     C*
     C* CHAIN TO THE DETAIL CHARGE AUXILIARY FILE TO GET THE PLACE OF SERVICE
     C*
     C                   MOVE      *BLANKS       XXPOS
     C     HCKEY         CHAIN     PHC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      HCPOS         XXPOS
     C                   ELSE                                                   ELSE *IN60 = '0'
     C                   MOVE      *BLANKS       XXPOS
     C                   END                                                    END *IN60 = '0'
     C*
     C* LOAD THE UNIT FIELD
     C*
     C     CACPT4        IFGE      '00100'
     C     CACPT4        ANDLE     '09999'
     C     CAANMN        DIV       15            HLANMN
     C                   MVR                     HLADDT
     C                   Z-ADD     HLANMN        HLANTV
     C     HLADDT        IFGE      .3
     C                   ADD       1             HLANTV
     C                   END
     C                   Z-ADD     HLANTV        XXUNTS
     C*
     C* IF SPECIAL ANES MODIFIER ADD ADDITIONAL TIME
     C*
     C     CAPMOD        IFEQ      'P3'
     C                   ADD       1             XXUNTS
     C                   ELSE
     C     CAPMOD        IFEQ      'P4'
     C                   ADD       2             XXUNTS
     C                   ELSE
     C     CAPMOD        IFEQ      'P5'
     C                   ADD       3             XXUNTS
     C                   END                                                    END CAPMOD EQ 'P5'
     C                   END                                                    END CAPMOD EQ 'P4'
     C                   END                                                    END CAPMOD EQ 'P3'
     C*
     C                   ELSE
     C*
     C     CAANMN        IFGT      *ZEROS
     C                   Z-ADD     CAANMN        XXUNTS
     C                   ELSE
     C                   Z-ADD     CAUNTS        XXUNTS
     C                   END
     C*
     C                   END
     C*
     C* WRITE RECORD TO THE MEDICAL DATA SUBMISSION FILE
     C*
     C*** BEGIN CHG *** 10/11/94 ***********************************************
     C**********           MOVELXXICD9    MDICD9
     C**********           MOVELXXPNAM    MDPNAM
     C**********           Z-ADDXXPZIP    MDPZIP
     C**********           MOVE XXPOS     MDPOS
     C**********           Z-ADDXXACDT    MDACDT
     C**********           MOVELXXCNAM    MDCNAM
     C**********           MOVELXXSS#     MDSS#
     C**********           MOVELXXENAM    MDENAM
     C**********           Z-ADDXXCRDT    MDCRDT
     C**********           Z-ADDXXCKDT    MDCKDT
     C                   MOVEL     XXCPT4        MDCPT4
     C                   MOVEL     XXPMOD        MDPMOD
     C                   Z-ADD     XXUNTS        MDUNIT
     C                   Z-ADD     XXAMTC        MDAMTC
     C                   Z-ADD     XXAMTP        MDAMTP
     C                   WRITE     PMD01
     C*** END CHG *** 10/11/94 *************************************************
     C*
     C                   ENDSR
     C*******************************************************************
     C* SROTHR - SUBROUTINE TO PROCESS SPECIFIC OTHER INFORMATION       *
     C*******************************************************************
     C* SROTHR: All other schedules:
     C*              Vision Care (V2020-V2780)
     C*              Hearing Aid Equipment (V5000-V5299)
     C*              Nursing/Attendant & Home Health Care (W0050-W0325)
     C*              Medical Transportation (A0010-A0999)
     C*              Durable Medical Equipment (A4124-A5131, E0100-E1699)
     C*                                        (Q0036-Q0046)
     C*              Prosthetic & Orthopedic Equipment (L0100-L8501)
     C*              Respiratory Services (E0400-E1406)
     C*              Vocational Rehab (X0710-X0740)
     C*  Used for codes with:
     C*        CAPRTY = 1
     C*  AND   not processed thru SRMEDI
     C* NO DATA SUBMISSION AT THIS TIME:
     C*              Pharmacy
     C*              Home Vehicle
     C*              Miscellaneous Claimant Expenses
     C*
     C*  5A XXCODE = Use FDCAP/CACPT4
     C* 10N XXAMTC = Use FDCAP/CATOTC
     C*  2A XXPOS  = Use FDHCP/HCPOS
     C*
     C     SROTHR        BEGSR
     C*
     C*
     C                   MOVE      *BLANKS       XXCPT4
     C                   MOVEL     CACPT4        XXCPT4
     C*
     C                   Z-ADD     *ZEROS        XXAMTC
     C                   MOVE      CATOTC        XXAMTC
     C*
     C* CHAIN TO THE DETAIL CHARGE AUXILIARY FILE TO GET THE PLACE OF SERVICE
     C*
     C                   MOVE      *BLANKS       XXPOS
     C     HCKEY         CHAIN     PHC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      HCPOS         XXPOS
     C                   ELSE                                                   ELSE *IN60 = '0'
     C                   MOVE      *BLANKS       XXPOS
     C                   END                                                    END *IN60 = '0'
     C*
     C* WRITE RECORD TO THE OTHER DATA SUBMISSION FILE
     C*
     C                   MOVEL     XXCPT4        ODCPT4
     C                   MOVEL     XXPNAM        ODPNAM
     C                   MOVEL     XXPADR        ODPADR
     C                   Z-ADD     XXPZIP        ODPZIP
     C                   Z-ADD     XXAMTC        ODAMTC
     C                   MOVE      XXPOS         ODPOS
     C                   Z-ADD     XXACDT        ODACDT
     C                   MOVEL     XXCNAM        ODCNAM
     C                   MOVEL     XXSS#         ODSS#
     C                   MOVEL     XXENAM        ODENAM
     C                   Z-ADD     XXCRDT        ODCRDT
     C                   Z-ADD     XXCKDT        ODCKDT
     C                   WRITE     POD01
     C*
     C                   ENDSR
