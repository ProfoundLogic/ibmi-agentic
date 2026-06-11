     Hcopyright('(c) 2022 Sapiens, Inc. All rights reserved. ')
     H*
     H/TITLE W8074R1 - TRANSMISSION FORMAT PROCESSOR - A49
     H*
     H*****************************************************************
     H*  REVISIONS:
     H*
     H* 03/31/25 C:053 Support: 102/118738
     H*                Correct the segment size calc for 3.1
     H*                Correct the segment size calc for 3.0
     H*
     H* 01/30/25 C:079 Support: 102/118422
     H*                For Release 3.1 correct the end of line calc so
     H*                data will not be truncated
     H*
     H* 01/22/24 C:053 Support 114717
     H*                Remove end of line characters from narrative
     H*
     H* 01/20/22 C:053 Support: 93491
     H*                Correct Other Benefit Segment issues.
     H*
     H* 02/14/22 C:079 Project: WC2797
     H*                IAIABC 3.1.4 Fields
     H*
     H* 02/04/19 C:053 Support: 86035
     H*                Qualify fields for blanks or 0's
     H*
     H* 12/11/19 C:053 Support: 85435
     H*                Send fields as blanks instead of 0's
     H*
     H* 09/07/19 C:062 Project 152WAU
     H*                Populate CMPDS#.
     H*
     H* 07/08/19 C:053 Support: 79328
     H*                Add SA back to processing for Ben Seg
     H*
     H* 03/08/19 C:079 Support: 90/77111
     H*                Blank fill SUAWW and SUCAWW if zero for all states
     H*
     H* 11/21/18 C:053 Project: WC2515 priss
     H*                Change for Mitchell submissions
     H*
     H* 06/26/18 C:180 Support - 71017
     H*                Send blanks instead of zeros for AWW.
     H*                This change applicable to the state of LA.
     H*
     H* 05/30/18 C:274 Project: WC2515
     H*            079 Add code for EDI 3.1
     H*            180 Add release 3.1 processing.
     H*            180 Add addtl 3.1 flds (D3 counters, D3JRC#).
     H*            053 Add clears for EDI 3.1
     H*
     H* 01/31/17 C:306 Project - WC2410
     H*                Add new EDI fields:
     H*                D1INJS - Injury Severity Type Code
     H*
     H* 02/08/14 C:180 SUPPORT PRISS 027 - WC2234
     H*                Add new EDI fields:
     H*                - Employer Paid Salary Prior to Acquisition Code
     H*                - Work Week Type Code
     H*                - Work Week Schedule Code
     H*                - Employee Security Code
     H*
     H* 11/06/13     C:180      Support 84069
     H*                         Project 147WBF - Access to other files
     H*                         using OPTACOMP's admin claim number.
     H*
     H* 07/02/13     C:190      PROJECT 147WBF
     H*                         Access WXSUP with the new logical.
     H*
     H* 01/19/11     C:180      Sweep non-linked BTCs when
     H*                         processing concurrent benefits.
     H*
     H* 08/19/10     C:220      PROJECT WC1584 / 139WNN - PRISS 61
     H*                         Add EDIF subkey in IXKY Klist for correct
     H*                         header information
     H*
     H* 08/18/10     C:220      PROJECT WC1584 / 139WNN - PRISS 50
     H*                         - Concurrent Employers information was not
     H*                         uploaded to the ROES files
     H*
     H* 07/29/10     C:053      PROJECT 139WNN - PRISS 44
     H*
     H* 07/28/10     C:220      PROJECT 139WNN - Add key EDIF for WXFRL file
     H*
     H* 09/14/09     C:180      SUPPORT 69295
     h*
     H* 05/22/09     C:18       SUPPORT 68569 - IP error
     H*
     H* 04/30/09     C:180      SUPPORT 68059 EP-Claims
     h*
     H* 04/27/09     C:053      SUPPORT 68211
     H*                         Allow link for 02's
     H*
     H* 04/15/09     C:053      SUPPORT 67734
     H*                         For GIC only, make MTC date = original sent
     H*                         date.
     H*
     H* 03/14/09     C:053      SUPPORT 67522
     H*                         Process new flag for MTC link for ben seg.
     H*
     H* 03/11/09     C:053      SUPPORT 67521
     H*                         Add edit on MTC's
     H*
     H* 11/17/08     C:053      SUPPORT 63706
     H*                         Correct insured FEIN
     H*
     H* 11/11/08     C:180      SUPPORT 65892
     H*                         Donot load MTCs
     H*                         02, 04,  PY, PD, UR, AN, BM, BW, MN, & QT
     H*                         for variable segment.
     H*
     H* 11/10/08     C:180      SUPPORT 65774
     H*                         Format DN298 - Date claim Admin had knowleg
     H*                         of LT
     H*
     H* 11/03/08     C:180      SUPPORT 65627
     H*                         Donot load MTC FN, SA for variable segment.
     H*
     H* 10/30/08     C:180      SUPPORT 65671, 65757  Add EDI R3 fields:
     H*                         Employee ID other than SSN
     H*                         Reduced Benef Amt code
     H*
     H* 10/15/08 C:180 SUPPORT 65530
     H*                Load Current Date disability began
     H*                                                               *
     H* 10/19/07 C:191 Project:WC1584                                 *
     H*                EDI Release 3: Add processing for R22 records  *
     H*                                                               *
     H* 05/01/07 C:185 Project:WC1177                                 *
     H*                EDI Release 3                                  *
     H*                                                               *
     H* 07/03/02 C:53 Support: WC081 # 32456                          *
     H*               Update maintenance fields                       *
     H*                                                               *
     H* 06/29/99 C:15 Project:                                        *
     H*               Load blanks to all dates which equal zero.      *
     H*                                                               *
     H*  03/23/99 C:15 Project: WC468                                 *
     H*                Added code to load the PTD/Reduced Earning     *
     H*                segment and Death Dependent segment.           *
     H*****************************************************************
     FWMM2L     UF   E           K DISK
     FWMLGL5    UF   E           K DISK
     FWXSUL1    IF   E           K DISK
     FWXPIL     IF   E           K DISK
     FWXAJL     IF   E           K DISK
     FWXRDL     IF   E           K DISK
     FWMIXL     IF   E           K DISK
     FWXPTL     IF   E           K DISK
     FWXE3L     IF   E           K DISK
     FWXR3L     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWXX9P     O    E           K DISK
     FSSYLF     IF   E           K DISK
     FWXT2L     IF   E           K DISK
     FWXT3L     IF   E           K DISK
     FWXWIL     IF   E           K DISK
     FWXFRL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWXV3L     IF   E           K DISK
     FWXH3L     IF   E           K DISK
     D DTA             S              1    DIM(19830)
     D PYAJ            S              3    DIM(99)
     D PYT2            S              3    DIM(99)
     D** Rel 1 pmt adj code prevent dups
     D PA              S              3    DIM(10)
     D LGACLC          S             25    INZ
     D XXRLSA          S              1A
     D xxpos           s              5  0
     D*
     D*** begin add ***  01/22/24  *******************************************
     D/copy sprString
     D*** end add *****  01/22/24  *******************************************
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
     D                 DS
     D  DET9PI                 1     20
     D  XXIBPC                 1      3
     D  XXIMPP                 4      8
     D  XXIMLC                 9      9
     D                 DS
     D  DET9AJ                 1     46
     D  XXPYAJ                 1      3
     D  XXPADT                 4     14
     D  XXAWAM                15     25
     D  XXPSDT                26     33
     D  XXPEDT                34     41
     D  XXAWPD                42     45
     D  XXADPD                46     46
     D                 DS
     D  DET9RD                 1     23
     D  XXBACD                 1      4
     D  XXWAAM                 5     15
     D  XXASDT                16     23
     D* Benefit Segment
     D                 DS
     D  DET9BF                 1    103
     D  XXPYAJB                1      3
     D  XXMTC                  4      5
     D  XXGWAM                 6     16
     D  XXGWEF                17     24
     D  XXAWAMB               25     35
     D  XXNWEF                36     43
     D  XXPSDTB               44     51
     D  XXPEDTB               52     59
     D  XXAWPDB               60     63
     D  XXADPDB               64     64
     D  XXPADTB               65     75
     D  XXPYDT                76     83
     D  XXFLBF                84    103
     D* Payment
     D                 DS
     D  DET9PM                 1     98
     D  XXPYRC                 1      3
     D  XXPNAM                 4     43
     D  XXAMT                 44     54
     D  XXFRDT                55     62
     D  XXTODT                63     70
     D  XXISDT                71     78
     D* Other Benefits, Recoveries
     D                 DS
     D  DET9OB                 1     34
     D  XXPYAJO                1      3
     D  XXPADTO                4     14
     D* Benefit ACR Codes
     D                 DS
     D  DET9AC                 1     60
     D  XXACOD                 1      4
     D  XXSTDT                 5     12
     D  XXENDT                13     20
     D  XXAMTC                21     31
     D  XXWAIO                32     42
     D*
     D* Reduced Earnings
     D                 DS
     D  DET9RE                 1     60
     D  XXREWN                 1      2
     D  XXARE                  3     13
     D  XXDRE                 14     24
     D  XXRESD                25     32
     D  XXREED                33     40
     D  XXREWA                41     51
     D* Concurrent Employers
     D                 DS
     D  DET9CE                 1     86
     D  XXCENM                 1     40
     D  XXCEBP                41     55
     D  XXCEWG                56     66
     D* Denial Reason Codes
     D                 DS
     D  DET9DR                 1      2
     D  XXDNYC                 1      2
     D* Denial Reason Narrative
     D                 DS
     D  DET9DN                 1     50
     D  XXDNYN                 1     50
     D* Suspension Narrative
     D                 DS
     D  DET9SN                 1     50
     D  XXSUSN                 1     50
     D* Denial Reason Narrative
     D                 DS
     D  XXDNY                  1    150
     D  XXDNY1                 1     50
     D  XXDNY2                51    100
     D  XXDNY3               101    150
     D                 DS
     D  DET9T2                 1     14
     D  XXPDCD                 1      3
     D  XXPDAM                 4     14
     D                 DS
     D  DET9T3                 1     90
     D  XXDPCD                 1      2
     D  XXDFSN                 3     17
     D  XXDLSN                18     57
     D  XXDDOB                58     65
     D  XXDSEX                66     66
     D  XXEXTD                67     67
     D*
     D* Change Data Elements
     D*
     D  detCDE                 1     40
     D   xxSeg#                1      4
     D   xxReasCd              5      5
     D   xxfillCDE             6     40    Inz(*blanks)
     D*
     D  detNarr                1     70
     D   xxNarr                1     50
     D   xxfillNarr           51     70    Inz(*blanks)
     D*
     D                 DS
     D  DET9W1                 1     55
     D  XXNAME                 1     40
     D  XXBPH#                41     55
     D*
     D X9DATA          DS         19830
     D HEDA49        E DS                  EXTNAME(WXH8P)
     D DETA49        E DS                  EXTNAME(WXDSP)
     D DETR22        E DS                  EXTNAME(WXD1P)
     D DT2231        E DS                  EXTNAME(WXD3P)
     D TRLA49        E DS                  EXTNAME(WXT8P)
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
     C                   IF        IXRLS = 'A'
     C                   EVAL      XXRLSA = *ON
     C                   EVAL      IXRLS  = '3'
     C                   ENDIF
     C*
     C*  RETRIEVE THE TRANSMISSION MANAGEMENT RECORD.
     C*
     C     M2KY          CHAIN     PM201                              60
     C     *IN60         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
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
     C**********
     C*  SECOND, WRITE THE A49 DETAIL.  MORE THAN ONE EXTRACT OF A49
     C*  DETAIL MAY NEED TO BE PROCESSED.  PROCESS ALL EXTRACTS (M1)
     C*  ASSOCIATED WITH THIS ORIGINAL TRANSMISSION OR RE-TRANSMISSION.
     C**********
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
     C*  PROCESS ALL A49 DETAIL ASSOCIATED WITH EACH EXTRACT (A49)
     C*
     C                   Z-ADD     LGFND         XXFND
     C                   Z-ADD     LGFYR         XXFYR
     C                   Z-ADD     LGCASE        XXCASE
      /Free
        LGACLC = XXACLC;
      /End-Free
     c     keyWXSUL1     CHAIN     PSU01                              60
     C     *IN60         IFEQ      *OFF
      /Free
       If (FNgrpasc = 'OPTACOMP' Or cltID = '152') and SUACLC <> *blanks;
          LGACLC = SUACLC;
       Endif;
      /End-Free
     C                   EXSR      SRXDTL
     C     SU#PI         IFGT      *ZERO
     C     xxRlSa        Andne     *on
     C                   EXSR      SRXPI
     C                   ENDIF
     C     IXRLS         IFEQ      '1'
     C     SU#PA         IFGT      *ZERO
     C                   EXSR      SRXAJ
     C                   ENDIF
     C     SU#BA         IFGT      *ZERO
     C                   EXSR      SRXRD
     C                   ENDIF
     C     SU#RER        IFGT      *ZERO
     C                   EXSR      SRXRER
     C                   ENDIF
     C                   ENDIF
     C     SU#DDR        IFGT      *ZERO
     C     xxRlSa        Andne     *on
     C                   EXSR      SRXDDR
     C                   ENDIF
     C     FRKY          CHAIN     PFR01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     FR#WN         IFGT      *ZERO
     C                   EXSR      SRXWN
     C                   ENDIF
     C*
     C                   ENDIF
     C                   MOVEA     DTA           X9DATA
      /Free
       Select;
      // A49 - R1
       When IXRLS = '1';
           xxpos = (su#pi * 8) + (su#ddr * 2) +
                   (su#pa * 46) + (su#ba * 23) +
                   (su#rer * 14) + 208 + 1;
      // A49 - R3
       When IXRLS = '3' and XXRLSA <> *ON;
           xxpos = (su#pi * 8) + (su#ddr * 2) +
                    208 + 1;
      // A49 - R3.1
       When XXRLSA = *ON;
           xxpos =  208 + 1;
       Endsl;
       %subst(X9DATA:xxpos) = x'0D';
      /End-Free
     C                   WRITE     PX901
     C                   ADD       1             XXRCNT
     C* Load R22 information
     C                   IF        IXRLS = '3' and XXRLSA <> *ON
     C                   EXSR      SRXR22
     C* Process variable segments
     C* No. of Benefits
     C     SU#BFT        IFGT      *ZEROS
     C                   EXSR      SR#BFT
     C                   ENDIF
     C* No. of Payments
     C* Only sent on select MTC transmissions IP, PY, AP, RB
     C                   IF        SU#PMT > 0 AND (SUMTC = 'IP' OR
     C                             SUMTC = 'PY' OR SUMTC = 'AP' OR
     C                             SUMTC = 'RB')
     C* Log Other Benefits to the verification array before SR#PMT
     C     SU#OBF        IFGT      *ZEROS
     C                   EXSR      SVFOBF
     C                   ENDIF
     C                   EXSR      SR#PMT
     C                   ENDIF
     C* No. of Other Benefits
     C     SU#OBF        IFGT      *ZEROS
     C                   EXSR      SR#OBF
     C                   ENDIF
     C* No. of Benefits ACR
     C     SU#ACR        IFGT      *ZEROS
     C                   EXSR      SR#ACR
     C                   ENDIF
     C* No. of Recoveries
     C     SU#REC        IFGT      *ZEROS
     C                   EXSR      SR#REC
     C                   ENDIF
     C* No. of Reduced earnings
     C     SU#RDE        IFGT      *ZEROS
     C                   EXSR      SR#RDE
     C                   ENDIF
     C* No. of Concurrent employers
     C     SU#CEM        IFGT      *ZEROS
     C                   EXSR      SR#CEM
     C                   ENDIF
     C* No. of full denial reason codes, denial reason narrative, suspension nar
     C     SU#DRC        IFGT      *ZEROS
     C     SU#DRN        ORGT      *ZEROS
     C     SU#SNN        ORGT      *ZEROS
     C                   EXSR      SR#DRC
     C                   ENDIF
     C* Write R3.0 R22 record to WXX9P
     C                   MOVEA     DTA           X9DATA
      /Free
      // R22 - R3
       If IXRLS = '3' and XXRLSA <> *ON;
           //*** Beg Chg *** 01/30/25 **************************************
           //***xxpos = (su#bft * 103) + (su#pmt * 98) + (su#obf * 34) +
           //***        (su#acr * 51) + (su#rec * 34) + (su#rde * 44) +
           //***        (su#cem * 86) + (su#drc * 2) + (su#drn * 50) +
           //***        (su#snn * 50) +
           //***         650 + 1;
           //*** Beg Chg *** 03/31/25 **************************************
           //*** xxpos = (su#bft * 103) + (su#pmt * 98) + (su#obf * 34) +
           //***         (su#acr * 51) + (su#rec * 34) + (su#rde * 60) +
           //***         (su#cem * 86) + (su#drc * 2) + (su#drn * 50) +
           //***         (su#snn * 50) +
           //***         650 + 1;
                 xxpos = (su#bft * 103) + (su#pmt * 98) + (su#obf * 34) +
                         (su#acr * 51) + (su#rec * 34) + (su#rde * 44) +
                         (su#cem * 86) + (su#drc * 2) + (su#drn * 50) +
                         (su#snn * 50) +
                         650 + 1;
           //*** End Chg *** 03/31/25 **************************************
           //*** End Chg *** 01/30/25 **************************************
           %subst(X9DATA:xxpos) = x'0D';
       Endif;
      /End-Free
     C                   WRITE     PX901
     C                   ADD       1             XXRCNT
     C                   ADD       1             XXTCNT
     C                   ENDIF
     C* Load R22 information   --> EDI 3.1
     C                   IF        XXRLSA = *ON
     C                   EXSR      SR2231
     C* Process variable segments
     C* No. of Benefits
     C     SU#BFT        IFGT      *ZEROS
     C                   EXSR      SR#BFT
     C                   ENDIF
     C* No. of Payments
     C* Only sent on select MTC transmissions IP, PY, AP, RB
     C                   IF        SU#PMT > 0 AND (SUMTC = 'IP' OR
     C                             SUMTC = 'PY' OR SUMTC = 'AP' OR
     C                             SUMTC = 'RB')
     C* Log Other Benefits to the verification array before SR#PMT
     C     SU#OBF        IFGT      *ZEROS
     C                   EXSR      SVFOBF
     C                   ENDIF
     C                   EXSR      SR#PMT
     C                   ENDIF
     C* No. of Other Benefits
     C     SU#OBF        IFGT      *ZEROS
     C                   EXSR      SR#OBF
     C                   ENDIF
     C* No. of Benefits ACR
     C     SU#ACR        IFGT      *ZEROS
     C                   EXSR      SR#ACR
     C                   ENDIF
     C* No. of Recoveries
     C     SU#REC        IFGT      *ZEROS
     C                   EXSR      SR#REC
     C                   ENDIF
     C* No. of Reduced earnings
     C     SU#RDE        IFGT      *ZEROS
     C                   EXSR      SR#RDE
     C                   ENDIF
     C* No. of Concurrent employers
     C     SU#CEM        IFGT      *ZEROS
     C                   EXSR      SR#CEM
     C                   ENDIF
     C* No. of full denial reason codes, denial reason narrative, suspension nar
     C     SU#DRC        IFGT      *ZEROS
     C     SU#DRN        ORGT      *ZEROS
     C     SU#SNN        ORGT      *ZEROS
     C                   EXSR      SR#DRC
     C                   ENDIF
     C     SU#PI         IFGT      *ZERO
     C                   EXSR      SRXPI
     C                   ENDIF
     C     SU#DDR        IFGT      *ZERO
     C                   EXSR      SRXDDR
     C                   ENDIF
      /Free
       // Change Data Elements subroutine
           If su#De > 0;
              Exsr srXdE;
           Endif;
       // Claim Narrative subroutine
           If su#Na > 0;
              Exsr srXnA;
           Endif;
      /End-Free
     C* Write R3.1 R22 record to WXX9P
     C                   MOVEA     DTA           X9DATA
      /Free
      // R22 - R3.1
       If XXRLSA = *ON;
           //*** Beg Chg *** 01/30/25 **************************************
           //***xxpos = (su#bft * 103) + (su#pmt * 98) + (su#obf * 34) +
           //***        (su#acr * 51) + (su#rec * 34) + (su#rde * 44) +
           //***        (su#cem * 86) + (su#drc * 2) + (su#drn * 50) +
           //***        (su#snn * 50) + (su#pi * 20) + (su#ddr * 90) +
           //***        (su#de * 40) + (su#na * 70) +
           //***         758 + 1;
           //*** Beg Chg *** 03/31/25 **************************************
           //***
           //*** xxpos = (su#bft * 103) + (su#pmt * 98) + (su#obf * 34) +
           //***         (su#acr * 51) + (su#rec * 34) + (su#rde * 60) +
           //***         (su#cem * 86) + (su#drc * 2) + (su#drn * 50) +
           //***         (su#snn * 50) + (su#pi * 20) + (su#ddr * 90) +
           //***         (su#de * 40) + (su#na * 70) +
           //***         758 + 1;
           xxpos = (su#bft * 103) + (su#pmt * 98) + (su#obf * 34) +
                   (su#acr * 60) + (su#rec * 34) + (su#rde * 60) +
                   (su#cem * 86) + (su#drc * 2) + (su#drn * 50) +
                   (su#snn * 50) + (su#pi * 20) + (su#ddr * 90) +
                   (su#de * 40) + (su#na * 70) +
                    758 + 1;
           //*** End Chg *** 03/31/25 **************************************
           //*** End Chg *** 01/30/25 **************************************
           %subst(X9DATA:xxpos) = x'0D';
       Endif;
      /End-Free
     C                   WRITE     PX901
     C                   ADD       1             XXRCNT
     C                   ADD       1             XXTCNT
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
     C     XXRCNT        IFGE      IXMAXT
     C     IXMAXT        ANDGT     0
     C                   LEAVE
     C                   ENDIF
     C*
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
     C                   Z-ADD     XXRCNT        M2A49T
     C                   Z-ADD     XXRCNT        M2A49S
     C*
     C                   MOVE      'P'           M2STAT
     C                   UPDATE    PM201
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C**************************************************************************
     C*
     C* DEFINE ALL KLISTS, FIELD DEFINITIONS, ETC...
     C*
     C     SRINIT        BEGSR
     C*                                                                   SRINIT
     C*  WMUPL - USER PARAMETER FILE                                      SRINIT
     C*                                                                   SRINIT
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*                                                                   SRINIT
     C*  WMIXL  - EDI CONTROL FILE                                        SRINIT
     C*                                                                   SRINIT
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXTRID
     C                   KFLD                    XXEDIF            3
     C*                                                                   SRINIT
     C*  WXSUL - A49 FILE                                                 SRINIT
     C*                                                                   SRINIT

     C     SUKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGJST
     C                   KFLD                    LGEXDT
     C                   KFLD                    LGTSET
     C                   KFLD                    LGACLC
     C                   KFLD                    LGMTC

     c     keyWXSUL1     KLIST
     c                   KFLD                    LGco#
     c                   KFLD                    xxfnd
     c                   KFLD                    xxfyr
     c                   KFLD                    xxcase
     c                   KFLD                    LGJST
     c                   KFLD                    LGEXDT
     c                   KFLD                    LGTSET
     c                   KFLD                    LGMTC
     C*                                                                   SRINIT
     C*  WMLGL3 - EDI LOG FILE                                            SRINIT
     C*                                                                   SRINIT
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXSENT
     C                   KFLD                    XXTSET
     C*                                                                   SRINIT
     C*  WMM2L - SYSTEM MANAGEMENT RECORDS                                SRINIT
     C*                                                                   SRINIT
     C     M2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXTSET
     C                   KFLD                    XXOTDT            7 0
     C                   KFLD                    XXOTTM            6 0
     C*
     C*
     C*  WMFNL - FUND MASTER FILE KEY
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*                                                                   SRINIT
     C     *LIKE         DEFINE    LGJST         XXJST
     C     *LIKE         DEFINE    LGSENT        XXSENT
     C     *LIKE         DEFINE    *INLR         XXTRFG
     C     *LIKE         DEFINE    *INLR         XXFRFG
     C     *LIKE         DEFINE    IXTRID        XXTRID
     C     *LIKE         DEFINE    SU#PA         SV#PA
     C     *LIKE         DEFINE    SU#BA         SV#BA
     C     *LIKE         DEFINE    SU#RER        SV#RER
     C*
     C                   MOVE      *OFF          XXTRFG
     C                   MOVE      *OFF          XXFRFG
     C                   MOVE      *ZEROS        XXRCNT            9 0
     C                   MOVE      *ZEROS        XXYEAR            2 0
     C                   MOVE      *BLANKS       XXCC              2
     C                   Z-ADD     0             XP                5 0
     C                   MOVE      *ZEROS        XXTCNT            9 0
     C*
     C*  WXFRL - 148 FILE                                                 SRINIT
     C*                                                                   SRINIT
     C     FRKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGJST
     C                   KFLD                    LGEXDT
     C                   KFLD                    LGTSET
     C                   KFLD                    LGACLC
     C     PTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
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
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
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
     C                   TIME                    DSDTTM
     C*
     C     ENINIT        ENDSR
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*
     C* FORMAT A49 HEADER AND WRITE TO FORMATTED TRANSMISSION FILE.
     C*
     C     SRXHDR        BEGSR
     C*
     C                   CLEAR                   X9DATA
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
     C                   ENDIF
     C                   MOVE      M2LTTM        H8LTTM
     C                   MOVE(P)   M2OTDT        H8OTDT
     C                   MOVEL     M2OTDT        XXCENT
     C     XXCENT        IFEQ      0
     C                   MOVEL     '19'          H8OTDT
     C                   ELSE
     C                   MOVEL     '20'          H8OTDT
     C                   ENDIF
     C                   MOVE      M2OTTM        H8OTTM
     C     M2TSTF        IFEQ      'Y'
     C                   MOVE      'T'           H8TPFG
     C                   ELSE
     C                   MOVE      'P'           H8TPFG
     C                   END
     C                   MOVE      QQTSET        H8TSET
     C     QQTSET        IFEQ      'A49'
     C                   MOVEL     '01'          H8TRN#
     C                   ELSE
     C                   MOVEL     '1A'          H8TRN#
     C                   ENDIF
     C*
     C* If IXRLS = '3' load '30' to H8TRN#
     C     IXRLS         IFEQ      '3'
     C                   MOVEL     '30'          H8TRN#
     C                   ENDIF                                                 I
     C     XXRLSA        IFEQ      *ON
     C                   MOVEL     '31'          H8TRN#
     C                   ENDIF                                                 I
     C***********  COMMENTED FOR PHASE 2.
     C*
     C                   MOVEL     HEDA49        X9DATA
      /Free
         eval  %subst(X9DATA:88) = x'0D';
      /End-Free
     C                   WRITE     PX901
     C*
     C     ENXHDR        ENDSR
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*
     C* FORMAT A49 DETAIL AND WRITE TO FLAT FILE.
     C*
     C     SRXDTL        BEGSR
     C*
     C                   CLEAR                   X9DATA
     C                   CLEAR                   DTA
     C                   Z-ADD     1             XP
     C                   MOVEL     SUTSET        DSTSET
     C                   MOVEL     SUMTC         DSMTC
     C                   MOVE      SUTDT         DSTDT
     C*
     C** OVERRIDE FOR GIC ONLY
     C*
     C     CLTID         IFEQ      '139'
     C     IXRLS         ANDEQ     '3'
     C                   MOVE(P)   M2OTDT        DSTDT
     C                   MOVEL     M2OTDT        XXCENT
     C     XXCENT        IFEQ      0
     C                   MOVEL     '19'          DSTDT
     C                   ELSE
     C                   MOVEL     '20'          DSTDT
     C                   ENDIF
     C                   ENDIF
     C                   MOVEL     SUJRST        DSJRST
     C                   MOVEL     SUCLCD        DSCLCD
     C                   MOVEL     SUFEIN        DSFEIN
     C                   MOVEL     SUAZIP        DSAZIP
     C                   MOVEL     SUSS#         DSSS#
     C                   IF        IXRLS = '3'
     C                   MOVEL     *BLANKS       DSSS#
     C                   ENDIF
     C                   MOVEL     SUDEP         DSDEP
     C                   MOVEL     SUPRED        DSPRED
     C                   MOVEL     SUDBDT        DSDBDT
     C                   MOVEL     SUMAXI        DSMAXI
     C                   MOVEL     SURETW        DSRETW
     C                   MOVEL     SURTW         DSRTW
     C                   MOVEL     SUEDTD        DSEDTD
      /Free
       DSwage = %Editc(SUwage:'X');

       If SUwage = *Zeros;
          If SUjrst = 'LA';
             DSwage = *blanks;
          Endif;
       Endif;
      /End-Free
     C                   IF        IXRLS = '3'
     C                   MOVEL     *BLANKS       DSWAGE
     C                   ENDIF
     C                   MOVEL     SUWGPD        DSWGPD
     C                   MOVEL     SUDAYS        DSDAYS
     C                   MOVEL     SUCSAL        DSCSAL
     C                   MOVEL     SUACDT        DSACDT
     C                   MOVEL     SURPT#        DSRPT#
     C                   MOVEL     SUACLC        DSACLC
     C                   MOVEL     SUACL#        DSACL#
     C                   MOVEL     SUCSTS        DSCSTS
     C                   MOVEL     SUCTYP        DSCTYP
     C                   MOVEL     SUACCD        DSACCD
     C                   MOVE      SURPDT        DSRPDT
     C                   MOVE      SULATC        DSLATC
     C                   MOVE      SU#PI         DS#PI
     C                   MOVE      SU#PA         DS#PA
     C                   MOVE      SU#BA         DS#BA
     C                   MOVE      SU#RER        DS#RER
     C                   IF        IXRLS = '3'
     C                   MOVEL     *BLANKS       DS#PA
     C                   MOVEL     *BLANKS       DS#BA
     C                   MOVEL     *BLANKS       DS#RER
     C                   ENDIF
     C                   MOVE      SU#DDR        DS#DDR
     C     SUDBDT        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       DSDBDT
     C                   END
     C     SUMAXI        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       DSMAXI
     C                   END
     C     SURTW         IFEQ      *ZEROS
     C                   MOVE      *BLANKS       DSRTW
     C                   END
     C     SUEDTD        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       DSEDTD
     C                   END
     C     SURPDT        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       DSRPDT
     C                   END
     C*
     C                   MOVEA     DETA49        DTA(XP)
     C                   ADD       208           XP
     C*
     C     ENXDTL        ENDSR
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C     SRXR22        BEGSR
     C* Load R22 fields
     C                   CLEAR                   X9DATA
     C                   CLEAR                   DTA
     C                   Z-ADD     1             XP
     C***
     C                   MOVEL     'R22'         D1TSET
     C                   MOVEL     SUMTCC        D1MTCC
     C                   IF        SUMTC2 = *ZEROS
     C                   MOVE      *BLANKS       D1MTC2
     C                   ELSE
     C                   MOVE      SUMTC2        D1MTC2
     C                   ENDIF
      *
     C                   IF        SUCNLT > *ZEROS
     C                   MOVE      SUCNLT        D1CNLT
     C                   ELSE
     C                   MOVE      *BLANKS       D1CNLT
     C                   ENDIF
     C                   MOVEL     SUJURO        D1JURO
     C                   MOVEL     SUACLC        D1ACLC
     C                   MOVEL     SUCAFE        D1CAFE
     C                   MOVEL     SUCNAM        D1CANM
     C                   MOVEL     SUCARP        D1CARP
     C                   MOVEL     SUCAPH        D1CAPH
     C                   MOVEL     SUCAEM        D1CAEM
     C                   MOVEL     SUCAFX        D1CAFX
     C                   MOVEL     SUEITQ        D1EITQ
     C                   MOVEL     SUEID         D1SS#
     C                   MOVEL     SURBAC        D1RBAC
     C                   MOVEL     SULSN         D1LSN
     C                   MOVEL     SUFSN         D1FSN
     C                   MOVEL     SUMID         D1MID
     C                   MOVEL     *BLANKS       D1LSNS
     C                   MOVE      SUDTOB        D1DTOB
     C                   IF        SUDTOB = *ZEROS
     C                   MOVE      *BLANKS       D1DTOB
     C                   ENDIF
     C                   MOVEL     SUMSTS        D1MSTS
       D1eEl = %Editc(SUedul:'X');
     C                   MOVE      SUENEX        D1ENEX
     C                   MOVE      *BLANKS       D1FILL
     C                   MOVEL     SUETFS        D1ETFS
     C                   MOVEL     SUIDTH        D1IDTH
     C                   MOVEL     SUIDFE        D1INS#
     C                   MOVEL     SUIIF         D1IIF
     C                   MOVEL     SUEMP#        D1EMP#
     C                   MOVEL     SUEZP         D1EZP
     C                   MOVEL     SUSAME        D1SAME
     C                   MOVE      SUDODD        D1DODD
     C                   IF        SUDODD = *ZEROS
     C                   MOVE      *BLANKS       D1DODD
     C                   ENDIF
     C                   MOVEL     SUNCPC        D1NCPC
     C                   MOVEL     SUWMTH        D1WMTH
     C                   MOVE      SUCDLW        D1CDLW
     C                   IF        SUCDLW = *ZEROS
     C                   MOVE      *BLANKS       D1CDLW
     C                   ENDIF
     C                   MOVE      SUCDOB        D1DBDT
     C                   IF        SUCDOB = *ZEROS
     C                   MOVE      *BLANKS       D1DBDT
     C                   ENDIF
     C                   MOVE      SUIDLD        D1IDLD
     C                   IF        SUIDLD = *ZEROS
     C                   MOVE      *BLANKS       D1IDLD
     C                   ENDIF
     C                   MOVEL     SURTWT        D1RTWT
     C                   MOVEL     SUPRIN        D1PRIN
     C                   MOVE      SUSUEF        D1SUEF
     C                   IF        SUSUEF = *ZEROS
     C                   MOVE      *BLANKS       D1SUEF
     C                   ENDIF
     C                   MOVE      SUDYEF        D1DYEF
     C                   IF        SUDYEF = *ZEROS
     C                   MOVE      *BLANKS       D1DYEF
     C                   ENDIF
     C                   MOVE      SUDRES        D1DRES
     C                   IF        SUDRES = *ZEROS
     C                   MOVE      *BLANKS       D1DRES
     C                   ENDIF
     C                   MOVE      SUPDNY        D1PDNY
      /Free
       D1cwca = %Editc(SUcaww:'X');

       If SUcaww = *ZEROS;
             D1cwca = *blanks;
       Endif;
      /End-Free
     C                   MOVE      SUWGEF        D1WGEF
     C                   IF        SUWGEF = *ZEROS
     C                   MOVE      *BLANKS       D1WGEF
     C                   ENDIF
     C                   MOVE      SUDFBE        D1DFBE
     C                   MOVEL     SUTOLC        D1TOLC
     C                   MOVEL     SUESTS        D1ESTS
     C                   MOVEL     SUPIMP        D1PIMP
     C                   MOVE      SURTWD        D1RTWD
     C                   IF        SURTWD = *ZEROS
     C                   MOVE      *BLANKS       D1RTWD
     C                   ENDIF
     C                   MOVEL     SUFWPI        D1FWPI
     C                   MOVEL     SULUMC        D1LUMC
     C                   MOVEL     SUFSAL        D1FSAL
      /Free
       D1aww = %Editc(SUaww:'X');

       If SUaww = *ZEROS;
             D1aww = *blanks;
       Endif;
      /End-Free
     C                   MOVE      SUIDLT        D1IDLT
     C                   IF        SUIDLT = *ZEROS
     C                   MOVE      *BLANKS       D1IDLT
     C                   ENDIF
     C                   MOVE      SUSTDT        D1STDT
     C                   IF        SUSTDT = *ZEROS
     C                   MOVE      *BLANKS       D1STDT
     C                   ENDIF
     C                   MOVEL     SUCAZP        D1CAZP
     C                   MOVE      *BLANKS       D1FIL2
     C                   MOVE      SU#BFT        D1#BFT
     C                   MOVE      SU#PMT        D1#PMT
     C                   MOVE      SU#OBF        D1#OBF
     C                   MOVE      SU#ACR        D1#ACR
     C                   MOVE      SU#REC        D1#REC
     C                   MOVE      SU#RDE        D1#RDE
     C                   MOVE      SU#CEM        D1#CEM
     C                   MOVE      SU#DRC        D1#DRC
     C                   MOVE      SU#DRN        D1#DRN
     C                   MOVE      SU#SNN        D1#SNN
      /Free
       D1PSPA = SUPSPA;
       D1WWTC = SUWWTC;
       D1WDSC = SUWDSC;
       D1ESID = SUESID;
       D1INJS = SUINJS;
      /End-Free
     C***
     C                   MOVEA     DETR22        DTA(XP)
     C                   ADD       650           XP
     C***
     C                   ENDSR
     C***
     C****************************************************************
     C*** EDI 3.1       SR2231
     C****************************************************************
     C     SR2231        BEGSR
      /Free
      //Load R22 fields
           CLEAR X9DATA;
           CLEAR DTA;
           XP = 1;

           D3TSET = 'R22';
           D3MTCC =  SUMTCC;
           IF        SUMTC2 = *ZEROS;
             D3MTC2 =  *BLANKS;
           ELSE;
             D3MTC2 =   %EditC(SUMTC2:'X');
           ENDIF;  //                 SUMTC2 = *ZEROS

           IF        SUCNLT > *ZEROS;
             D3CNLT =  %EditC(SUCNLT:'X');
           ELSE;
             D3CNLT = *BLANKS;
           ENDIF;

           D3JURO  =  SUJURO;
           D3ACLC  =  SUACLC;
           D3CAFE  =  SUCAFE;
           D3CANM  =  SUCNAM;
           D3CARP  =  SUCARP;
           D3CAPH  =  SUCAPH;
           D3CAEM  =  SUCAEM;
           D3EITQ  =  SUEITQ;
           D3SS#   =  SUEID;
           D3RBAC  =  SURBAC;
           D3LSN   =  SULSN;
           D3FSN   =  SUFSN;
           D3MID   =  SUMID;
           D3LSNS  =  *BLANKS;
           D3DTOB  =  %EditC(SUDTOB:'X');

           IF        SUDTOB = *ZEROS;
             D3DTOB =  *BLANKS;
           ENDIF;    //  SUMTC2 = *ZEROS

           D3MSTS  =   SUMSTS;
           D3EEL   =   *BLANKS;
           D3ENEX  =   %EditC(SUENEX:'X');
           D3ETFS  =   SUETFS;
           D3IDTH  =   SUIDTH;
           D3INS#  =   SUIDFE;
           D3IIF   =   SUIIF;
           D3EMP#  =   SUEMP#;
           D3EZP   =   SUEZP;
           D3DODD  =   %EditC(SUDODD:'X');

           IF        SUDODD = *ZEROS;
              D3DODD  =   *BLANKS;
           ENDIF;  //SUDODD = *ZEROS

           D3NCPC  =  SUNCPC;
           D3WMTH  =  SUWMTH;

           D3CDLW  =  %EditC(SUCDLW:'X');
           IF        SUCDLW = *ZEROS;
             D3CDLW =  *BLANKS;
           ENDIF; // SUCDLW = *ZEROS

           D3DBDT = %EditC(SUCDOB:'X');
           IF        SUCDOB = *ZEROS;
             D3DBDT =  *BLANKS;
           ENDIF;  // SUDBDT = *ZEROS

           D3IDLD = %EditC(SUIDLD:'X');
           IF       SUIDLD = *ZEROS;
              D3IDLD =  *BLANKS;
           ENDIF;  // SUIDLD = *ZEROS

           D3SUEF =  %EditC(SUSUEF:'X');
           IF        SUSUEF = *ZEROS;
             D3SUEF =  *BLANKS;
           ENDIF;  //  SUSUEF = *ZEROS

           D3DYEF = %EditC(SUDYEF:'X');
           IF        SUDYEF = *ZEROS;
              D3DYEF =  *BLANKS;
           ENDIF; // SUDYEF = *ZEROS

           D3DRES  =  %EditC(SUDRES:'X');
           IF        SUDRES = *ZEROS;
             D3DRES  =  *BLANKS;
           ENDIF; // SUDRES = *ZEROS
           D3PDNY  =  SUPDNY;

           D3cwca = %Editc(SUcaww:'X');

           If SUcaww = *ZEROS;
               D3cwca = *blanks;
           Endif;

           D3WGEF =  %EditC(SUWGEF:'X');
           IF        SUWGEF = *ZEROS;
              D3WGEF =  *BLANKS;
           ENDIF; // SUWGEF = *ZEROS;
           D3DFBE  =  %EditC(SUDFBE:'X');
           D3TOLC  =  SUTOLC;
           D3ESTS  =  SUESTS;
           D3PIMP  =  SUPIMP;

           D3RTWD  =  %EditC(SURTWD:'X');
           IF        SURTWD = *ZEROS;
             D3RTWD =  *BLANKS;
           ENDIF; // SURTWD = *ZEROS
           D3FWPI  =     SUFWPI;
           D3LUMC  =     SULUMC;
           D3FSAL  =     SUFSAL;

           D3aww = %Editc(SUaww:'X');

           If SUaww = *ZEROS;
               D3aww = *blanks;
           Endif;

           D3IDLT =  %EditC(SUIDLT:'X');
           IF   SUIDLT = *ZEROS;
            D3IDLT =  *BLANKS;
           ENDIF;  // SUIDLT = *ZEROS

           D3STDT =  %EditC(SUSTDT:'X');
           IF        SUSTDT = *ZEROS;
             D3STDT  =  *BLANKS;
           ENDIF;  //  SUSTDT = *ZEROS

           D3CAZP = SUCAZP;
           D3FIL2 = *BLANKS;
           D3#BFT = %EditC(SU#BFT:'X');
           D3#PMT = %EditC(SU#PMT:'X');
           D3#OBF = %EditC(SU#OBF:'X');
           D3#ACR = %EditC(SU#ACR:'X');
           D3#REC = %EditC(SU#REC:'X');
           D3#RDE = %EditC(SU#RDE:'X');
           D3#CEM = %EditC(SU#CEM:'X');
           D3#DRC = %EditC(SU#DRC:'X');
           D3#DRN = %EditC(SU#DRN:'X');
           D3#SNN = %EditC(SU#SNN:'X');
           D3PSPA = SUPSPA;
           D3WWTC = SUWWTC;
           D3WDSC = SUWDSC;
           D3ESID = SUESID;

           D3INJS = SUINJS;

           D3IRTC = SUIRTC;
           D3IRPR = SUIRPR;
           D3IRSE = SUIRSE;
           D3LRTC = SULRTC;
           D3LRPR = SULRPR;
           D3LRSE = SULRSE;
           D3CDEK = %EditC(SUCDEK:'X');
           If D3CDEK = *zeros;
              D3CDEK = *blanks;
           Endif;
           D3CDAK = %EditC(SUCDAK:'X');
           If D3CDAK = *zeros;
              D3CDAK = *blanks;
           Endif;
           D3ALKD = %EditC(SUALKD:'X');
           If D3ALKD = *zeros;
              D3ALKD = *blanks;
           Endif;
           D3OPYA = '00000000000';
           D3SRCF = SUSRCF;
           D3SRCP = SUSRCP;
           D3PDED = %EditC(SUPDED:'X');
           If D3PDED = *zeros;
              D3PDED = *blanks;
           Endif;
           D3JRC# = SUJRC#;
           D3AWLI = SUAWLI;

           D3#PI  = %Editc(SU#PI:'X');
           D3#DDR = %Editc(SU#DDR:'X');
           D3#DE  = %Editc(SU#DE:'X');
           D3#NA  = %Editc(SU#NA:'X');

        // CLEARS
           D3CAFX = *BLANKS;
           D3SAME = *BLANKS;
           D3RTWT = *BLANKS;
           D3PRIN = *BLANKS;
           D3OPYA = *BLANKS;

        D3eEl = %Editc(SUedul:'X');
        D3bCrC = SUbcRc;

        If SUwhAf > 0;
           D3whAf = '00' + %Editc(SUwhAf:'X');
        Else;
           D3whAf = *Zeros;
        Endif;

        If SuoPya > 0;
           D3opYa = '00' + %Editc(SUoPyA:'X');
        Else;
           D3oPya = *Zeros;
        Endif;

        D3nTz  = SUnTz;
     C                   MOVEA     DT2231        DTA(XP)
      /Free
          XP = XP + 758;

          ENDSR;
      /End-Free

     C**************************************************************************
     C* SR#BFT - Number of benefits
     C**************************************************************************
     C     SR#BFT        BEGSR
     C*
     C                   Z-ADD     0             AJ                2 0
     C                   CLEAR                   PYAJ
     C                   Z-ADD     0             T2                2 0
     C                   CLEAR                   PYT2
     C     SUKY          CHAIN     PAJ01                              61
     C     *IN61         DOWEQ     *OFF
     C*
     C                   MOVEL     AJPYAJ        XXPYN             3 0
     C* Benefits
     C                   IF        (XXPYN >= 10 AND XXPYN <= 299)
     C                             OR XXPYN = 410 OR
     C                             (XXPYN >= 500 AND XXPYN <= 599)
     C                   EVAL      AJ = AJ+1
     C                   MOVEL     AJPYAJ        PYAJ(AJ)
     C                   MOVEL     AJPYAJ        XXPYAJB
     C                   IF        AJMTC = '04' or AJMTC = 'SA' or
     C                             AJMTC = 'FN' or
     C                             AJMTC = 'PY' or AJMTC = 'PD' or
     C                             AJMTC = 'UR' or AJMTC = 'AN' or
     C                             AJMTC = 'BM' or AJMTC = 'BW' or
     C                             AJMTC = 'MN' or AJMTC = 'QT'
     C** include these fields to be blanked out
     C*
     C                   MOVE      *BLANKS       XXGWEF
     C                   MOVE      *BLANKS       XXGWAM
     C                   MOVE      *BLANKS       XXNWEF
     C                   MOVE      *BLANKS       XXAWAMB
     C                   MOVE      *BLANKS       XXPYDT
     C                   MOVE      *BLANKS       XXMTC
     C                   ELSE
     C                   MOVE      AJMTC         XXMTC
     C** for specific MTC's only load mtc in seg's associate w/mtc
     C                   IF        AJMTC = 'RB' or AJMTC = 'S1' or
     C                             AJMTC = 'S2' or AJMTC = 'S3' or
     C                             AJMTC = 'S4' or AJMTC = 'S5' or
     C                             AJMTC = 'S6' or AJMTC = 'S7' or
     C                             AJMTC = 'S8' or AJMTC = 'S9' or
     C                             AJMTC = 'SX' or
     C                             AJMTC = 'CB' or AJMTC = '02' or AJMTC = 'AB'
     C                   IF        AJLNKM <> 'X'
     C                   MOVE      *BLANKS       XXMTC
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      AJGWAM        XXGWAM
     C                   MOVE      AJGWEF        XXGWEF
     C                   IF        AJGWEF = *ZEROS
     C                   MOVE      *BLANKS       XXGWEF
     C                   ENDIF
     C                   MOVE      AJAWAM        XXAWAMB
     C                   MOVE      AJNWEF        XXNWEF
     C                   IF        AJNWEF = *ZEROS
     C                   MOVE      *BLANKS       XXNWEF
     C                   ENDIF
     C                   MOVE      AJPYDT        XXPYDT
     C                   IF        AJPYDT = *ZEROS
     C                   MOVE      *BLANKS       XXPYDT
     C                   ENDIF
     C                   ENDIF
     C                   IF        AJMTC <> 'EP'
     C                   IF        AJPYAJ = '210' or AJPYAJ = '230' or
     C                             AJPYAJ = '240' or AJPYAJ = '242' or
     C                             AJPYAJ = '250' or AJPYAJ = '251' or
     C                             AJPYAJ = '270'
     C                   MOVE      *BLANKS       XXMTC
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      AJPSDT        XXPSDTB
     C                   IF        AJPSDT = *ZEROS
     C                   MOVE      *BLANKS       XXPSDTB
     C                   ENDIF
     C                   MOVE      AJPEDT        XXPEDTB
     C                   IF        AJPEDT = *ZEROS
     C                   MOVE      *BLANKS       XXPEDTB
     C                   ENDIF
     C                   IF        AJADPD = *ZEROS and AJAWPD = *ZEROS
     C                   MOVE      *BLANKS       XXAWPDB
     C                   MOVE      *BLANKS       XXADPDB
     C                   ELSE
     C                   MOVE      AJAWPD        XXAWPDB
     C                   MOVE      AJADPD        XXADPDB
     C                   ENDIF
     C                   IF        AJPADT = *ZEROS
     C                   MOVE      *BLANKS       XXPADTB
     C                   ELSE
     C                   MOVE      AJPADT        XXPADTB
     C                   ENDIF
     C*
     C                   MOVEA     DET9BF        DTA(XP)
     C                   ADD       103           XP
     C*** Save the code for Payment Segment if not a BTC (IE.. OBTC)
     C                   ELSE
     C                   EVAL      T2 = T2+1
     C                   MOVEL     AJPYAJ        PYT2(T2)
     C                   ENDIF
     C*
     C     SUKY          READE     PAJ01                                  61
     C                   ENDDO
     C
     C                   ENDSR
     C**************************************************************************
     C* SR#PMT - Number of Payments
     C**************************************************************************
     C     SR#PMT        BEGSR
     C*
     C                   Z-ADD     0             XXCOUNT           4 0
     C     SUKY          CHAIN     PPT01                              61
     C     *IN61         DOWEQ     *OFF
     C     XXCOUNT       ANDLT     SU#PMT
     C                   CLEAR                   DET9PM
     C                   Z-ADD     1             AJ
     C                   IF        PTPRCD <> *BLANKS
     C** Must match a benefit type code on this transaction
     C     PTPRCD        LOOKUP    PYAJ(AJ)                               62
     C** Or match an other benefit type code on this transaction
     C                   IF        *IN62 = *OFF
     C                   Z-ADD     1             T2
     C     PTPRCD        LOOKUP    PYT2(T2)                               62
     C                   ENDIF
     C                   IF        *IN62 = *ON
     C                   MOVEL     PTPRCD        XXPYRC
     C                   MOVEL     PTPAYE        XXPNAM
     C                   MOVE      PTAMT         XXAMT11          11 2
     C                   MOVE      XXAMT11       XXAMT
     C                   Z-ADD     PTSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         XXFRDT
     C                   IF        PTSTDT = *ZEROS
     C                   MOVE      *BLANKS       XXFRDT
     C                   ENDIF
     C                   Z-ADD     PTENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         XXTODT
     C                   IF        PTENDT = *ZEROS
     C                   MOVE      *BLANKS       XXTODT
     C                   ENDIF
     C                   Z-ADD     PTISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         XXISDT
     C                   IF        PTISDT = *ZEROS
     C                   MOVE      *BLANKS       XXISDT
     C                   ENDIF
     C                   MOVEA     DET9PM        DTA(XP)
     C                   ADD       98            XP
     C                   ADD       1             XXCOUNT
     C                   ENDIF
     C                   ENDIF
     C*
     C     SUKY          READE     PPT01                                  61
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C**************************************************************************
     C* SVFOBF - Add BTC to verification array
     C**************************************************************************
     C     SVFOBF        BEGSR
     C*
     C                   Z-ADD     0             XXCOUNT
     C     SUKY          CHAIN     PT201                              61
     C     *IN61         DOWEQ     *OFF
     C     XXCOUNT       ANDLT     SU#OBF
     C*
     C                   MOVEL     T2CODE        XXPYN
     C                   IF        XXPYN >= 300 AND XXPYN <= 499
     C                             AND XXPYN <> 410
     C                   EVAL      T2 = T2+1
     C                   MOVEL     T2CODE        PYT2(T2)
     C                   ADD       1             XXCOUNT
     C                   ENDIF
     C*
     C     SUKY          READE     PT201                                  61
     C                   ENDDO
     C*
     C                   ENDSR
     C**************************************************************************
     C* SR#OBF - Number of Other benefits
     C**************************************************************************
     C     SR#OBF        BEGSR
     C*
     C                   Z-ADD     0             XXCOUNT
     C     SUKY          CHAIN     PT201                              61
     C     *IN61         DOWEQ     *OFF
     C     XXCOUNT       ANDLT     SU#OBF
     C*
     C                   CLEAR                   DET9OB
     C                   MOVEL     T2CODE        XXPYN             3 0
     C* Other Benefits
     C                   IF        XXPYN >= 300 AND XXPYN <= 499
     C                             AND XXPYN <> 410
     C                   MOVEL     T2CODE        XXPYAJO
     C                   MOVE      T2AMT         XXPADTO
     C                   IF        T2AMT  = *ZEROS
     C                   MOVE      *BLANKS       XXPADTO
     C                   ENDIF
     C                   MOVEA     DET9OB        DTA(XP)
     C                   ADD       34            XP
     C                   ADD       1             XXCOUNT
     C                   ENDIF
     C*
     C     SUKY          READE     PT201                                  61
     C                   ENDDO
     C*
     C                   ENDSR
     C**************************************************************************
     C* SR#ACR - Number of benefits ACR
     C**************************************************************************
     C     SR#ACR        BEGSR
     C*
     C                   Z-ADD     0             XXCOUNT
     C     SUKY          CHAIN     PRD01                              61
     C     *IN61         DOWEQ     *OFF
     C     XXCOUNT       ANDLT     SU#ACR
     C*
     C                   CLEAR                   DET9AC
     C                   MOVE      RDBACD        XXACOD
     C                   MOVE      RDASDT        XXSTDT
     C                   IF        RDASDT = *ZEROS
     C                   MOVE      *BLANKS       XXSTDT
     C                   ENDIF
     C                   MOVE      RDENDT        XXENDT
     C                   IF        RDENDT = *ZEROS
     C                   MOVE      *BLANKS       XXENDT
     C                   ENDIF
     C                   MOVE      RDWAAM        XXAMT11
     C                   MOVE      XXAMT11       XXAMTC
     C                   IF        XXRLSA = *ON
     C                   MOVE      RDWIAO        XXAMT11
     C                   MOVE      XXAMT11       XXWAIO
     C                   ENDIF
     C                   MOVEA     DET9AC        DTA(XP)
     C                   IF        XXRLSA = *ON
     C                   ADD       60            XP
     C                   ELSE
     C                   ADD       51            XP
     C                   ENDIF
     C                   ADD       1             XXCOUNT
     C     SUKY          READE     PRD01                                  61
     C                   ENDDO
     C                   ENDSR
     C**************************************************************************
     C* SR#REC - Number of Recoveries
     C**************************************************************************
     C     SR#REC        BEGSR
     C*
     C                   Z-ADD     0             XXCOUNT
     C     SUKY          CHAIN     PAJ01                              61
     C     *IN61         DOWEQ     *OFF
     C     XXCOUNT       ANDLT     SU#REC
     C*
     C                   MOVEL     AJPYAJ        XXPYN             3 0
     C* Recoveries
     C                   IF        XXPYN >= 800 AND XXPYN <= 899
     C                   CLEAR                   DET9OB
     C                   MOVEL     AJPYAJ        XXPYAJO
     C                   MOVE      AJPADT        XXPADTO
     C                   IF        AJPADT = *ZEROS
     C                   MOVE      *BLANKS       XXPADTO
     C                   ENDIF
     C                   MOVEA     DET9OB        DTA(XP)
     C                   ADD       34            XP
     C                   ADD       1             XXCOUNT
     C                   ENDIF
     C*
     C     SUKY          READE     PAJ01                                  61
     C                   ENDDO
     C*
     C                   ENDSR
     C**************************************************************************
     C* SR#RDE - Number of Reduced Earnings  (Max 52)
     C**************************************************************************
     C     SR#RDE        BEGSR
     C*
     C                   Z-ADD     0             XXCOUNT
     C     PTKY          CHAIN     PR301                              61
     C     *IN61         DOWEQ     *OFF
     C     XXCOUNT       ANDLT     SU#RDE
     C                   CLEAR                   DET9RE
     C                   MOVEL     R0REWK        XXREWN
     C                   MOVE      R0ARE         XXAMT11
     C                   MOVE      XXAMT11       XXARE
     C                   MOVE      R0DRE         XXAMT11
     C                   MOVE      XXAMT11       XXDRE
      *
     C                   IF        XXRLSA = *ON
     C                   MOVE      R0RESD        XXRESD
     C                   IF        XXRESD = *zeros
     C                   MOVE      *BLANKS       XXRESD
     C                   ENDIF
     C                   MOVE      R0REED        XXREED
     C                   IF        XXREED = *zeros
     C                   MOVE      *BLANKS       XXREED
     C                   ENDIF
     C                   MOVE      R0REWA        XXAMT11
     C                   MOVE      XXAMT11       XXREWA
     C                   ENDIF
     C*
     C                   MOVEA     DET9RE        DTA(XP)
     C*
     C                   IF        XXRLSA = *ON
     C                   ADD       60            XP
     C                   ELSE
     C                   ADD       44            XP
     C                   ENDIF
      */Free
     C                   ADD       1             XXCOUNT
     C*
     C     PTKY          READE     PR301                                  61
     C                   ENDDO
     C*
     C                   ENDSR
     C**************************************************************************
     C* SR#CEM - Number of Concurrent Employers (Max 2)
     C**************************************************************************
     C     SR#CEM        BEGSR
     C*
     C                   Z-ADD     0             XXCOUNT
     C     PTKY          CHAIN     PE301                              61
     C     *IN61         DOWEQ     *OFF
     C     XXCOUNT       ANDLT     SU#CEM
     C                   CLEAR                   DET9RE
     C                   MOVEL     E0ENAM        XXCENM
     C                   MOVE      E0PHON        XXCEBP
     C                   MOVE      E0WAGE        XXCEWG
     C                   MOVEA     DET9CE        DTA(XP)
     C                   ADD       86            XP
     C                   ADD       1             XXCOUNT
     C*
     C     PTKY          READE     PR301                                  61
     C                   ENDDO
     C*
     C                   ENDSR
     C**************************************************************************
     C* SR#DRC - Number of Denial Reason Codes/ narratives
     C**************************************************************************
     C     SR#DRC        BEGSR
     C*
     C     PTKY          CHAIN     PCM01                              61
     C                   IF        *IN61 = *OFF
     C                   IF        CMDNY1 <> *BLANKS
     C* Denial Reason Code 1
     C                   CLEAR                   DET9DR
     C                   MOVE      CMDNY1        XXDNYC
     C                   MOVEA     DET9DR        DTA(XP)
     C                   ADD       2             XP
     C                   ENDIF
     C*
     C                   IF        CMDNY2 <> *BLANKS
     C* Denial Reason Code 2
     C                   CLEAR                   DET9DR
     C                   MOVE      CMDNY2        XXDNYC
     C                   MOVEA     DET9DR        DTA(XP)
     C                   ADD       2             XP
     C                   ENDIF
     C*
     C                   IF        CMDNY3 <> *BLANKS
     C* Denial Reason Code 3
     C                   CLEAR                   DET9DR
     C                   MOVE      CMDNY3        XXDNYC
     C                   MOVEA     DET9DR        DTA(XP)
     C                   ADD       2             XP
     C                   ENDIF
     C*
     C                   IF        CMDNY4 <> *BLANKS
     C* Denial Reason Code 4
     C                   CLEAR                   DET9DR
     C                   MOVE      CMDNY4        XXDNYC
     C                   MOVEA     DET9DR        DTA(XP)
     C                   ADD       2             XP
     C                   ENDIF
     C*
     C                   IF        CMDNY5 <> *BLANKS
     C* Denial Reason Code 5
     C                   CLEAR                   DET9DR
     C                   MOVE      CMDNY5        XXDNYC
     C                   MOVEA     DET9DR        DTA(XP)
     C                   ADD       2             XP
     C                   ENDIF
     C*
     C                   IF        CMDNYN <> *BLANKS
     C* Denial Reason Narrative
     C                   MOVE      CMDNYN        XXDNY
     C* Part 1
     C                   IF        XXDNY1 <> *BLANKS
     C                   CLEAR                   DET9DN
     C                   MOVE      XXDNY1        XXDNYN
     C                   MOVEA     DET9DN        DTA(XP)
     C                   ADD       50            XP
     C                   ENDIF
     C* Part 2
     C                   IF        XXDNY2 <> *BLANKS
     C                   CLEAR                   DET9DN
     C                   MOVE      XXDNY2        XXDNYN
     C                   MOVEA     DET9DN        DTA(XP)
     C                   ADD       50            XP
     C                   ENDIF
     C* Part 3
     C                   IF        XXDNY3 <> *BLANKS
     C                   CLEAR                   DET9DN
     C                   MOVE      XXDNY3        XXDNYN
     C                   MOVEA     DET9DN        DTA(XP)
     C                   ADD       50            XP
     C                   ENDIF
     C                   ENDIF
     C*
     C                   IF        CMSUSN <> *BLANKS
     C* Suspension Narrative
     c*** begin add ***  01/22/24  *******************************************
     C                   Clear                   xxTemp          150
      /free
      //* Remove unwanted characters and blank out the \n carrage returns.
           xxTemp = %trim(RmvExceptJSON(%trim(CMsusn)));
           xxTemp = %scanrpl('\n':' ':xxTemp);
           XXDNY  = %subst(xxTemp:1:150);
      /end-free
     c*** end add *****  01/22/24  *******************************************
     c*** beg del *****  01/22/24  *******************************************
     C*****************  MOVE      CMSUSN        XXDNY
     c*** end del *****  01/22/24  *******************************************
     C* Part 1
     C                   IF        XXDNY1 <> *BLANKS
     C                   CLEAR                   DET9SN
     C                   MOVE      XXDNY1        XXSUSN
     C                   MOVEA     DET9SN        DTA(XP)
     C                   ADD       50            XP
     C                   ENDIF
     C* Part 2
     C                   IF        XXDNY1 <> *BLANKS
     C                   CLEAR                   DET9SN
     C                   MOVE      XXDNY2        XXSUSN
     C                   MOVEA     DET9SN        DTA(XP)
     C                   ADD       50            XP
     C                   ENDIF
     C* Part 3
     C                   IF        XXDNY1 <> *BLANKS
     C                   CLEAR                   DET9SN
     C                   MOVE      XXDNY3        XXSUSN
     C                   MOVEA     DET9SN        DTA(XP)
     C                   ADD       50            XP
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C* FORMAT A49 PERMANENT IMPAIRMENT DETAIL.
     C*
     C     SRXPI         BEGSR
     C*
     C     SUKY          CHAIN     PPI01                              61
     C     *IN61         DOWEQ     *OFF
     C*
     C                   CLEAR                   X9DATA
     C                   MOVEL     PIIBPC        XXIBPC
     C                   MOVE      PIIMPP        XXIMPP
     C*
     C                   IF        XXRLSA = *ON
     C                   MOVE      PIIMLC        XXIMLC
     C                   ENDIF
     C                   MOVEA     DET9PI        DTA(XP)
     C                   IF        XXRLSA = *ON
     C                   ADD       20            XP
     C                   ELSE
     C                   ADD       8             XP
     C                   ENDIF
     C*
     C     SUKY          READE     PPI01                                  61
     C                   ENDDO
     C*
     C                   ENDSR
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*
     C* FORMAT A49 PAYMENT/ADJUSMTENT DETAIL.
     C*
     C     SRXAJ         BEGSR
     C*
     C                   CLEAR                   PA
     C*
     C     SUKY          CHAIN     PAJ01                              61
     C     *IN61         DOWEQ     *OFF
     C*
     C** Prevent pmt code from being reported more than once if extracted
     C** on the same day.
     C                   Z-ADD     1             Z                 2 0
     C     AJPYAJ        LOOKUP    PA(Z)                                  62
     C                   IF        *IN62 = *OFF
     C                   Z-ADD     1             Z
     C     *BLANKS       LOOKUP    PA(Z)                                  62
     C                   IF        *IN62 = *ON
     C                   MOVEL     AJPYAJ        PA(Z)
     C*
     C                   CLEAR                   X9DATA
     C                   MOVEL     AJPYAJ        XXPYAJ
     C                   MOVE      AJPADT        XXPADT
     C                   MOVE      AJAWAM        XXAWAM
     C                   MOVE      AJPSDT        XXPSDT
     C                   MOVE      AJPEDT        XXPEDT
     C                   MOVE      AJAWPD        XXAWPD
     C                   MOVE      AJADPD        XXADPD
     C*
     C                   MOVEA     DET9AJ        DTA(XP)
     C                   ADD       46            XP
     C                   ENDIF
     C                   ENDIF
     C*
     C     SUKY          READE     PAJ01                                  61
     C                   ENDDO
     C*
     C     ENXAJ         ENDSR
       //
       // Load the Change Data Elements Segments
       //
      /Free
       Begsr srXde;
         Clear detCDE;

         Chain (LGco#:LGjSt:LGexDt:LGtSet:LGaClc:LGmTc) ph301;

         Dow %found(wxh3l);
            xxSeg# = H3cDn#;
            xxReasCd = H3cRcd;
            xxFillCDE = *blanks;
      /Free
     C                   MOVEA     DETCDE        DTA(XP)
      /End-Free
            xp = xp + 40;
         Reade (LGco#:LGjSt:LGexDt:LGtSet:LGaClc:LGmTc) ph301;

         Enddo;
       Endsr;
       //
       // Load the Narratives for Claims Segments
       //
       Begsr srXna;
         Clear detNarr;

         Chain (LGco#:LGjSt:LGexDt:LGtSet:LGaClc:LGmTc:LGmTcd) pv301;

         Dow %found(wxv3l);
            xxNarr = V3cRna;
            xxFillNarr = *blanks;
      /Free
     C                   MOVEA     DETNARR       DTA(XP)
      /End-Free
            xp = xp + 70;
         Reade (LGco#:LGjSt:LGexDt:LGtSet:LGaClc:LGmTc:LGmTcd) pv301;

         Enddo;
       Endsr;
      /End-Free
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*
     C* FORMAT A49 BENEFIT ADJUSTMENT DETAIL
     C*
     C     SRXRD         BEGSR
     C*
     C     SUKY          CHAIN     PRD01                              61
     C     *IN61         DOWEQ     *OFF
     C*
     C                   CLEAR                   X9DATA
     C                   MOVEL     RDBACD        XXBACD
     C                   MOVE      RDWAAM        XXAMT11
     C                   MOVE      XXAMT11       XXWAAM
     C                   MOVE      RDASDT        XXASDT
     C*
     C                   MOVEA     DET9RD        DTA(XP)
     C                   ADD       23            XP
     C*
     C     SUKY          READE     PRD01                                  61
     C                   ENDDO
     C*
     C                   ENDSR
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*
     C* FORMAT A49 PAID TO DATE/REDUCED EARNINGS/RECOVERIES DETAIL.
     C*
     C     SRXRER        BEGSR
     C*
     C                   CLEAR                   X9DATA
     C     SUKY          CHAIN     PT201                              61
     C     *IN61         DOWEQ     *OFF
     C*
     C                   MOVEL     T2CODE        XXPDCD
     C                   MOVE      T2AMT         XXPDAM
     C*
     C                   MOVEA     DET9T2        DTA(XP)
     C                   ADD       14            XP
     C*
     C     SUKY          READE     PT201                                  61
     C                   ENDDO
     C*
     C                   ENDSR
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*
     C* FORMAT A49 DEATH DEPENDENT/PAYEE RELATIONSHIP DETAIL.
     C*
     C     SRXDDR        BEGSR
     C*
     C                   CLEAR                   X9DATA
     C     SUKY          CHAIN     PT301                              61
     C     *IN61         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  T3REL         XXDPCD
     C                   MOVE      T3BODR        XXDPCD
     C*
     C                   IF        XXRLSA = *ON
     C                   MOVE      T3DFSN        XXDFSN
     C                   MOVE      T3DLSN        XXDLSN
     C                   MOVE      T3DDOB        XXDDOB
     C                   IF        XXDDOB = *zeros
     C                   MOVE      *BLANKS       XXDDOB
     C                   ENDIF
     C                   MOVE      T3DSEX        XXDSEX
     C                   MOVE      T3EXTD        XXEXTD
     C                   ENDIF
     C                   MOVEA     DET9T3        DTA(XP)
     C                   IF        XXRLSA = *ON
     C                   ADD       90            XP
     C                   ELSE
     C                   ADD       2             XP
     C                   ENDIF
     C*
     C     SUKY          READE     PT301                                  61
     C                   ENDDO
     C*
     C                   ENDSR
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C* FORMAT 148 WITNESS DETAIL.
     C*
     C     SRXWN         BEGSR
     C*
     C     SUKY          CHAIN     PW101                              61
     C     *IN61         DOWEQ     *OFF
     C*
     C                   CLEAR                   X9DATA
     C                   MOVEL     W1NAME        XXNAME
     C                   MOVE      W1BPH#        XXBPH#
     C*
     C                   MOVEA     DET9W1        DTA(XP)
     C                   ADD       55            XP
     C*
     C     SUKY          READE     PW101                                  61
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C* FORMAT A49 TRAILER AND WRITE TO FLAT FILE.
     C*
     C     SRXTRL        BEGSR
     C*
     C                   CLEAR                   X9DATA
     C*
     C     IXRLS         IFEQ      '3'
     C                   MOVEL     'TR2'         T8TTRL
     C                   MOVE      XXTCNT        T8TCNT
     C                   ELSE
     C                   MOVEL     'TR1'         T8TTRL
     C                   ENDIF
     C                   MOVE      XXRCNT        T8RCNT
     C*
     C                   MOVEL     TRLA49        X9DATA
      /Free
         eval  %subst(X9DATA:22) = x'0D';
      /End-Free
     C                   WRITE     PX901
     C*
     C     ENXTRL        ENDSR
