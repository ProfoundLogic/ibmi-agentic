     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE CVLMC151 - LOAD DATA FOR CASE X-FER/DELETE FAT3P
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program loads FAT3P Transfer/Deletion File default **
     H**   file information                                          **
     H**   CALLED FROM:                                              **
     H**     Command line                                            **
     H**   CALLS TO:                                                 **
     H**     None                                                    **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      99      General office                                 **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FAT3L (PT301): Transfer/Deletion File default file     **
     H**                     (FILE)                                  **
     H*****************************************************************
     H*
     FFAT3L     UF A E           K DISK

     D AF1             S              8    DIM(50) CTDATA PERRCD(1)             FILE, DESC
     D AFD             S             30    DIM(50) ALT(AF1)
     D AF2             S              8    DIM(50) CTDATA PERRCD(1)             PROD
     D AFP             S              1    DIM(50) ALT(AF2)
     D*
     D*          Date work area
     D*          ------------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*          PLIST for date conversion
     C*          ------------------------------------------------------
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*          Get date
     C*          ------------------------------------------------------
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C*          Loop through table
     C*          ------------------------------------------------------
     C                   DO        50            X                 2 0
     C     AF1(X)        IFNE      *BLANKS
     C     AF1(X)        CHAIN     PT301                              99
     C*
     C*             Delete discontinued files
     C*             ---------------------------------------------------
     C     *IN99         IFEQ      *OFF
     C     AF1(X)        ANDEQ     'FAPYP'
     C                   DELETE    PT301
     C                   ITER
     C                   ENDIF                                                  *IN99 = Off
     C*
     C*             Move data to output fields
     C*             ---------------------------------------------------
     C                   MOVEL     AFD(X)        T3DESC
     C     AF1(X)        COMP      AF2(X)                                 60
     C   60              MOVEL     AFP(X)        T3PROD
     C  N60              MOVE      *BLANK        T3PROD
     C                   MOVE(P)   Q1USER        T3USER
     C                   MOVE(P)   Q1PGM         T3PGM
     C                   Z-ADD     DSTIME        T3TIME
     C                   MOVEL     SRDTO         T3DT
     C*
     C*             Record not in file: add it
     C*             ---------------------------------------------------
     C     *IN99         IFEQ      *ON
     C                   MOVEL     AF1(X)        T3FILE
     C                   MOVEL(P)  T3USER        T3IUSR
     C                   MOVEL(P)  T3PGM         T3IPGM
     C                   Z-ADD     T3TIME        T3ITIM
     C                   Z-ADD     T3DT          T3IDT
     C                   WRITE     PT301
     C*
     C*             Record in file: update it
     C*             ---------------------------------------------------
     C                   ELSE                                                   *IN99 = On
     C                   UPDATE    PT301
     C                   ENDIF                                                  *IN99 = On
     C                   ENDIF                                                  AF1 <> blanks
     C                   ENDDO                                                  Do 50 loop
     C*
     C*          End of program
     C*          ------------------------------------------------------
     C                   MOVE      *ON           *INLR
     C                   RETURN
** FILES DESCRIPTIONS
FMCDP   Claimant demographics                     1
FMCMP   Claim File                                2
FMCHP   Claim Status History                      3
NMARP   Accounts Receivable                       4
FADAP   Diagnosis Code Info                       5
FAMSP   Claim Med Svc Trans File                  6
FDOSP   Outcome/Savings Transaction File          7
FAPPP   Claim Phys/Prv/Spec Tran                  8
FAPYP   Phys. of Record Trans                     9
FATXP   Treatment Report Trans                   10
FAWHP   Work History File                        11
FATRP   All: BWC TX Plan File                    12
FATDP   All: BWC TX Plan Notes                   13
FATIP   All: BWC TX Plan ICD9                    14
FAERP   Recon/Refund EOMB Trans                  15
FAEOP   EOMB Transaction File                    16
NAMBP   Medical Mgmt Bill Trans                  17
NASBP   Set-up Bill Trans File                   18
NDTMP   T/M Detail Charge File                   19
FAEAP   EOMB Header Amount Trans                 20
FAEDP   Dental Bill Header info                  21
FAEEP   EOMB Header EOB/Notes Trans              22
FAEHP   EOMB Phys/HCFA Header                    23
FAEPP   Pharm. Bill Header info                  24
FAESP   EOMB Status Change Trans                 25
FAEUP   EOMB Hosp/UB Header File                 26
FAE1P   EOMB Auxiliary File                      27
FAHBP   HBA Transaction File                     28
FDCAP   Detail Charge Trans File                 29
FADUP   Dup Claims Wrk File                      30
FDDAP   EOMB Detail Amount Trans                 31
FDDDP   EOMB Dental Detail Trans                 32
FDDEP   EOMB Det EOB/Notes Trans                 33
FDDHP   EOMB Phys/HCFA Det Trans                 34
FDDPP   EOMB Pharmacy Det Trans                  35
FDDUP   EOMB Hosp/UB Det Trans                   36
FDHCP   Detail Charge Auxiliary                  37
FDMGP   Prov Bills spec. errors                  38
NATRP   Network Transaction File                 39
NAPBP   Pre-Cert Billing Trans                   40
FDEOP   EOMB Delete Transaction                  41
FDE1P   EOMB Delete Auxiliary                    42
FADCP   Diag Code/Det Chrg Temp                  43
FDCAP   Detail Charge Trans File                 44
FALTP   Request Letter Trans                     45
UMPCP   Pre-Certification Master                 46
UMCHP   Claim Pre-Cert History                   47
UDPOP   Pre-Cert Procedure Code                  48
SDCMP   DARTS Major Transaction                  49
SMARP   DARTS Addtl Transaction                  50
** PRODUCTS
FMCDP   A
FMCMP   W
FMCHP   W
NMARP   A
FADAP   C
FAMSP   C
FDOSP   C
FAPPP   C
FAPYP   C
FATXP   C
FAWHP   C
FATRP   C
FATDP   C
FATIP   C
FAERP   F
FAEOP   A
NAMBP   C
NASBP   C
NDTMP   C
FAEAP   F
FAEDP   F
FAEEP   F
FAEHP   F
FAEPP   F
FAESP   F
FAEUP   F
FAE1P   F
FAHBP   F
FDCAP   F
FADUP   F
FDDAP   F
FDDDP   F
FDDEP   F
FDDHP   F
FDDPP   F
FDDUP   F
FDHCP   F
FDMGP   F
NATRP   N
NAPBP   U
FDEOP   F
FDE1P   F
FADCP   F
FDCAP   F
FALTP   F
UMPCP   U
UMCHP   U
UDPOP   U
SDCMP   D
SMARP   D
