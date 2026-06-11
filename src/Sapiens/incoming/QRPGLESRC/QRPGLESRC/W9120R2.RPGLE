     H*copyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H*StoneRiver is a trademark of StoneRiver, Inc.')
     H*
     H/TITLE W8005R - UDS Claim Import File Convert
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*  This program will write the data from the formatted files    *
     H*  to the CLIMPORT file so the data can be imported into WCMS   *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   NONE                                                        *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: C:189                                             *
     H* CREATED: 04/20/10               PROJECT: 114WWA               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*   *IN60 - GENERAL CHAIN, DOWEQ                                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/08/25 C:342  Project C150WGB                               *
     H*                 Recompiled only.                              *
     H*                                                               *
     H* 06/18/13 c:62 Support: WC114 82648                            *
     H*               Expand seq# to 5.0                              *
     H*                                                               *
     H* 05/17/13 c:62 Support: WC114 82381                            *
     H*               Initialize the compare seq# field.              *
     H*               Enhance the additioanl remark loop              *
     H*                                                               *
     H*****************************************************************
     FWXUDP     IF   E           K DISK
     F*** Begin Add *** 05/28/13 *******************************************
     FWXUDL     IF   E           K DISK    RENAME(PUD01:PUD00) PREFIX(R)
     F*** End   Add *** 05/28/13 *******************************************
     FWXUFL     IF   E           K DISK
     FWXULL     IF A E           K DISK
     FCLIMPORT  O    E           K DISK
      *
     D                 DS
     DDSDTTM                   1     12  0
     DDSTIME                   1      6  0
     DDSDATE                   7     12  0
     D*
      *
     DHEADER           DS
     D*
     DH1ID                     1      2
     DH1FND                    3      5  0
     DH1EMP#                   6     14  0
     DH1DIV                   15     19  0 inz(0)
     DH1ID#                   20     28
     DH1ACDT                  29     36  0
     DH1CLM#                  37     76
     DH1CAR#                  77     96
     DH1UPD                   97     97
     DH1FYR                   98    100  0
     DH1CASE                 101    107  0
     D
     DDETAIL           DS
     D*
     DD1ID                     1      2
     DD1FND                    3      5  0
     DD1CLM#                   6     45
     DD1FSN                   46     60
     DD1LSN                   61     90
     DD1MSN                   91     91
     DD1ADD1                  92    131
     DD1ADD2                 132    171
     DD1CTY                  172    196
     DD1STAT                 197    198
     DD1ZIP                  199    207  0
     DD1CC                   208    210
     DD1OC                   211    250
     DD1GDR                  251    251
     DD1MSTS                 252    252
     DD1DOB                  253    260  0
     DD1LSF                  261    261
     DD1CLMS                 262    262
     DD1ACD                  263    382
     DD1NC                   383    384
     DD1PC                   385    386
     DD1CA                   387    388
     DD1LCC                  389    390
     DD1TOC                  391    392
     DD1JST                  393    394
     DD1DIA                  395    402  0
     DD1DIE                  403    410  0
     DD1DOD                  411    418  0
     DD1DC                   419    428
     DD1MC                   429    435
     DD1PCC                  436    440  0 INZ(0)
     DD1OD                   441    480
     DD1AWW                  481    489  0 INZ(0)
     DD1HD                   490    497  0 INZ(0)
     DD1DLW                  498    505  0 INZ(0)
     DD1LSI                  506    506
     DD1MOS                  507    508
     DD1FI                   509    509
     DD1LI                   510    510
     DD1CON                  511    511
     DD1SUB                  512    512
     DD1DEN                  513    513
     DD1MCO                  514    515
     D*** Begin Add *** 05/24/13 *******************************************
     DD1MCT                  516    516
     DD1rt01                 517    518       INZ('ME')
     D*** End   Add *** 05/24/13 *******************************************
     D*** Begin Chg *** 05/24/13 *******************************************
     D***D1OMR                  519    527  0 INZ(0)
     D***D1TMP                  528    536  0 INZ(0)
     D***D1ERC                  537    538
     D***D1OER                  539    547  0 INZ(0)
     D***D1TEP                  548    556  0 INZ(0)
     D***D1IRC                  557    558
     D***D1OIR                  559    567  0 INZ(0)
     D***D1TIP                  568    576  0 INZ(0)
     D***D1LSC                  577    578
     D***D1OLR                  579    587  0 INZ(0)
     D***D1TLP                  588    596  0 INZ(0)
     D***D1RRC                  597    598
     D***D1ORR                  599    607  0 INZ(0)
     D***D1TRP                  608    616  0 INZ(0)
     D***D1RC6                  617    618
     D***D1OR6                  619    627  0 INZ(0)
     D***D1TP6                  628    636  0 INZ(0)
     D***D1RC7                  637    638
     D***D1OR7                  639    647  0 INZ(0)
     D***D1TP7                  648    656  0 INZ(0)
     D***D1RC8                  657    658
     D***D1OR8                  659    667  0 INZ(0)
     D***D1TP8                  668    676  0 INZ(0)
     D***D1RC9                  677    678
     D***D1OR9                  679    687  0 INZ(0)
     D***D1TP9                  688    696  0 INZ(0)
     D***D1RC10                 697    698
     D***D1OR10                 699    707  0 INZ(0)
     D***D1TP10                 708    716  0 INZ(0)
     DD1rr01                 519    527  0 INZ(0)
     DD1ar01                 528    536  0 INZ(0)
     DD1rt02                 537    538    INZ('EX')
     DD1rr02                 539    547  0 INZ(0)
     DD1ar02                 548    556  0 INZ(0)
     DD1rt03                 557    558    INZ('CO')
     DD1rr03                 559    567  0 INZ(0)
     DD1ar03                 568    576  0 INZ(0)
     DD1rt04                 577    578    INZ('LE')
     DD1rr04                 579    587  0 INZ(0)
     DD1ar04                 588    596  0 INZ(0)
     DD1rt05                 597    598    INZ('RE')
     DD1rr05                 599    607  0 INZ(0)
     DD1ar05                 608    616  0 INZ(0)
     DD1rt06                 617    618    INZ('  ')
     DD1rr06                 619    627  0 INZ(0)
     DD1ar06                 628    636  0 INZ(0)
     DD1rt07                 637    638    INZ('  ')
     DD1rr07                 639    647  0 INZ(0)
     DD1ar07                 648    656  0 INZ(0)
     DD1rt08                 657    658    INZ('  ')
     DD1rr08                 659    667  0 INZ(0)
     DD1ar08                 668    676  0 INZ(0)
     DD1rt09                 677    678    INZ('  ')
     DD1rr09                 679    687  0 INZ(0)
     DD1ar09                 688    696  0 INZ(0)
     DD1rt10                 697    698    INZ('  ')
     DD1rr10                 699    707  0 INZ(0)
     DD1ar10                 708    716  0 INZ(0)
     D*** End   Chg *** 05/24/13 *******************************************
     DD1DRA                  717    725  0 INZ(0)
     DD1DCE                  726    733  0 INZ(0)
     DD1CAT                  734    735  0 INZ(0)
     DD1RT                   736    737
     DD1IT                   738    739
     DD1AC                   740    742
     DD1SC                   743    745
     DD1LC                   746    755
     DD1PI                   756    760  0 INZ(0)
     D*
     DHISTRY           DS
     D*
     DHIID                     1      2
     DHIFND                    3      5  0
     DHICLM#                   6     45
     DHICLMS                  46     46
     DHISTD                   47     54
     DNOTE1            DS
     D*
     DN1ID                     1      2
     DN1FND                    3      5  0
     DN1CLM#                   6     45
     DN1RS                    46     50  0
     DN1RD                    51     58  0
     DN1TD                    59     66  0
     DN1TT                    67     72  0 INZ(0)
     DN1DC                    73     80  0 INZ(0)
     DN1DF                    81     81
     DN1RT                    82     86
     DN1COM                   87    361
     DN1RMK                  362    371
     D*
     DNOTE2            DS
     D*
     DN2ID                     1      2
     DN2FND                    3      5  0
     DN2CLM#                   6     45
     DN2RS                    46     50  0
     DN2ARS                   51     55  0
     DN2RT                    56     60
     DN2COM                   61    135
     DXXSEG            S              9  0
     DFSTLEN           S              9  0
     DXXTXTL           S              9  0
     DXXENTX           S                   like(UFENTX)
     D*** Begin Chg *** 05/28/13 *******************************************
     D**********XXNLS#           S                   like(UFNLS#)
     DXXNLS#           S              5  0
     D*** End   Chg *** 05/28/13 *******************************************
     D*** Begin Add *** 05/28/13 *******************************************
     DsvRcl#           S                   like(UDrcl#)
     D*** End   Add *** 05/28/13 *******************************************
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
      *=============================================================
      *  KLIST
      *=============================================================
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD1       PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*** Begin Add *** 05/28/13 *******************************************
     C*
     C     keyWXUDL      KLIST
     C                   KFLD                    UDRCL#
     C*** End   Add *** 05/28/13 *******************************************
      *================================================================
      *  *LIKE DEFN
      *================================================================
     C*
     C*Move parameter values to numeric variables
     C
     C                   TIME                    DSDTTM
     C*
      *================================================================
      *  MAINLINE CODE START HERE
      *================================================================
     C*
     C*"----------------------
     C*" Read WXUDP records
     C*"----------------------
     C*
     C*** Begin Add *** 05/28/13 *******************************************
     C                   eval      svRcl#=' '
     C*** End   Add *** 05/28/13 *******************************************
     C                   READ      PUD01
     C                   DOW       NOT %eof(WXUDP)
     C*** Begin Add *** 05/28/13 *******************************************
     C                   If        UDrcl#<>svRcl#
     C                   eval      svRcl#=UDrcl#
     C*** End   Add *** 05/28/13 *******************************************
     C*
     C* Write the Header record for the claim
     C                   EXSR      WRTHDR
     C*
     C* Write the Detail record for the claim
     C                   EXSR      WRTDTL
     C*
     C* Write the History record for the claim
     C                   EXSR      WRTHST
     C*
     C*If exists in WXUFP write the NOTE record for claim
     C
     C                   EXSR      WRTNOT
     C*
     C*Write the Location records for the claim in WXULP
     C*
     C     UDICL#        CHAIN     PUL01                              60
     C*
     C                   IF        *IN60 = *ON
     C                   EXSR      WRTLOC
     C                   ENDIF
     C*** Begin Add *** 05/28/13 *******************************************
     C                   EndIf
     C*** End   Add *** 05/28/13 *******************************************
     C*
     C                   READ      PUD01
     C                   ENDDO
     C*
     C                   SETON                                        LR
      *===============================================================
      * WRTHDR- Write the Header record in CLIMPORT File
      *===============================================================
     C     WRTHDR        BEGSR
     C
     C                   CLEAR                   VFDATA
     C                   CLEAR                   HEADER
     C                   MOVEL     '01'          H1ID
     C                   MOVEL     QQFND         H1FND
     C                   MOVEL     QQEMP#        H1EMP#
     C                   MOVEL     UDCID#        H1ID#
     C                   Z-ADD     UDLSDT        H1ACDT
     C                   MOVEL     UDICL#        H1CLM#
     C                   MOVEL     UDPOL#        H1CAR#
     C                   MOVEL     'A'           H1UPD
     C
     C                   Z-ADD     UDLSDT        SRDTI
     C
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         H1FYR
     C
     C                   Z-ADD     0             H1CASE
     C                   MOVEL     HEADER        VFDATA
     C
     C                   WRITE     PVF01
     C
     C                   ENDSR
      *===============================================================
      * WRTDTL- Write the detail record in CLIMPORT File
      *===============================================================
     C     WRTDTL        BEGSR
     C
     C                   CLEAR                   DETAIL
     C                   CLEAR                   VFDATA
     C                   MOVEL     '02'          D1ID
     C                   MOVEL     QQFND         D1FND
     C                   MOVEL     UDRCL#        D1CLM#
     C                   MOVEL     UDRCL#        D1CLM#
     C                   MOVEL     UDCNL2        D1FSN
     C                   MOVEL     UDCNL1        D1LSN
     C                   MOVEL     UDCAD1        D1ADD1
     C                   MOVEL     UDCAD2        D1ADD2
     C                   MOVEL     UDCCIT        D1CTY
     C                   MOVEL     UDCSTA        D1STAT
     C                   MOVEL     UDCZCO        D1ZIP
     C                   Z-ADD     UDCBDT        D1DOB
     C
     C                   IF        UDRETY  = 'A'
     C                   MOVEL     'O'           D1CLMS
     C                   ENDIF
     C
     C                   IF        UDRETY  = 'E'
     C                   MOVEL     'C'           D1CLMS
     C                   ENDIF
     C                   MOVEL     UDDOIN        D1ACD
     C                   MOVEL     UDWNOI        D1NC
     C                   MOVEL     UDWPOB        D1PC
     C                   MOVEL     UDWCAU        D1CA
     C                   MOVEL     UDWTLS        D1LCC
     C                   MOVE      UDWTCD        D1TOC
     C                   MOVEL     UDFLST        D1JST
     C                   MOVEL     UDCRDT        D1DIE
     C*** Begin Add *** 05/28/13 *******************************************
     C*
     C* Get Reserve Information
     C*
     C                   eval      D1rt01='ME'
     C                   eval      D1rt02='EX'
     C                   eval      D1rt03='CO'
     C                   eval      D1rt04='LE'
     C                   eval      D1rt05='RE'
     C*
     C     keyWXUDL      Chain     pud00
     C                   If        %Found(WXUDL)
     C                   DoU       %Eof(WXUDL)
     C*
     C                   Select
     C                   When      RUDcvCD=965005
     C                   eval      D1rt03='CO'
     C                   eval      D1rr03=RUDtAmt
     C                   When      RUDcvCD=965010
     C                   eval      D1rr01=RUDtAmt
     C                   When      RUDcvCD=965025
     C                   eval      D1rr05=RUDtAmt
     C                   EndSl
     C*
     C     keyWXUDL      ReadE     pud00
     C                   EndDo
     C                   EndIf
     C*** End   Add *** 05/28/13 *******************************************
     C
     C                   MOVEL     DETAIL        VFDATA
     C                   WRITE     PVF01
     C
     C
     C                   ENDSR
      *===============================================================
      * WRTNOT- Write the notes record in CLIMPORT File
      *===============================================================
     C     WRTNOT        BEGSR
     C
     C                   MOVEL     UDICL#        XXICL#           30
     C*** Begin Add *** 05/17/13 *******************************************
     C                   eval      xxnid#=-1
     C*** End   Add *** 05/17/13 *******************************************
     C     XXICL#        CHAIN     PUF01                              60
     C
     C                   DOW       *IN60 = *OFF
     C
     C                   IF        UFNID#  <> XXNID#
     C                   EXSR      WRT07
     C                   MOVEL     UFNID#        XXNID#            4 0
     C                   ELSE
     C
     C* Write all '08' sequence number records for each '07' Note id records
     C                   EXSR      WRT08
     C                   ENDIF
     C
     C
     C     XXICL#        READE     PUF01                                  60
     C
     C                   ENDDO
     C
     C                   ENDSR
      *===============================================================
      * WRTLOC- Write the Location record
      *===============================================================
     C     WRTLOC        BEGSR
     C
     C                   CLEAR                   HEADER
     C                   MOVEL     UDRCL#        ULUNQ#
     C                   MOVEL     UDINCO        ULICMP
     C                   MOVEL     UDPOL#        ULPOL#
     C                   MOVEL     UDICL#        ULICC#
     C                   MOVEL     UDRCL#        ULRC#
     C                   MOVEL     UDINL1        ULINL1
     C                   WRITE     PUL01
     C
     C                   ENDSR
      *===============================================================
      * WRT07 - Write the '07' note record
      *===============================================================
     C     WRT07         BEGSR
     C
     C                   CLEAR                   VFDATA
     C                   CLEAR                   NOTE1
     C                   MOVEL     '07'          N1ID
     C                   MOVEL     QQFND         N1FND
     C                   MOVEL     UFICL#        N1CLM#
     C                   Z-ADD     UFNID#        N1RS
     C                   MOVEL     UFENDT        N1RD
     C                   MOVEL     UFENDT        N1TD
     C                   MOVEL     'CLREV'       N1RT
     C                   MOVEL     UFENTX        N1COM
     C
     C                   MOVEL     NOTE1         VFDATA
     C                   WRITE     PVF01
     C
     C                   EVAL      XXSEG = 75
     C                   EVAL      FSTLEN = 250
     C
     C                   EVAL      XXNLS# = 0
     C                   EVAL      XXTXTL = %LEN(%TRIM(UFENTX))
     C                   IF        XXTXTL > 250
     C
     C*** Begin Chg *** 05/17/13 *******************************************
     C**********         DOW       XXTXTL >= FSTLEN
     C                   DOW       XXTXTL >= FSTLEN And FSTLEN < 999
     C*** End   Chg *** 05/17/13 *******************************************
     C                   CLEAR                   XXENTX
     C                   EVAL      XXENTX = %SUBST(UFENTX:FSTLEN:75)
     C                   EVAL      FSTLEN = FSTLEN + 75
     C                   EVAL      XXNLS# = XXNLS# + 1
     C                   EXSR      WRT081
     C                   ENDDO
     C
     C                   ENDIF
     C                   ENDSR
      *===============================================================
      * WRT08 - Write the '08' note record
      *===============================================================
     C     WRT08         BEGSR
     C                   CLEAR                   VFDATA
     C                   CLEAR                   NOTE2
     C                   MOVEL     '08'          N2ID
     C                   MOVEL     QQFND         N2FND
     C                   MOVEL     UFICL#        N2CLM#
     C                   Z-ADD     UFNID#        N2RS
     C
     C                   EVAL      XXNLS# = XXNLS# + 1
     C                   Z-ADD     XXNLS#        N2ARS
     C
     C                   MOVEL     'CLREV'       N2RT
     C                   MOVEL     UFENTX        N2COM
     C
     C                   MOVEL     NOTE2         VFDATA
     C                   WRITE     PVF01
     C
     C                   EVAL      XXSEG = 75
     C                   EVAL      FSTLEN = 75
     C
     C                   EVAL      XXTXTL = %LEN(%TRIM(UFENTX))
     C                   IF        XXTXTL > 75
     C                   IF        XXNLS# = 0
     C                   EVAL      XXNLS# = UFNLS#
     C                   ENDIF
     C
     C                   DOW       XXTXTL >= FSTLEN  AND FSTLEN <  975
     C                   CLEAR                   XXENTX
     C                   EVAL      XXENTX = %SUBST(UFENTX:FSTLEN:75)
     C                   EVAL      FSTLEN = FSTLEN + 75
     C
     C                   EVAL      XXNLS# = XXNLS# + 1
     C                   EXSR      WRT081
     C                   ENDDO
     C
     C                   IF        FSTLEN >= 975
     C                   CLEAR                   XXENTX
     C                   EVAL      XXENTX = %SUBST(UFENTX:FSTLEN:26)
     C
     C                   EVAL      XXNLS# = XXNLS# + 1
     C                   EXSR      WRT081
     C
     C                   ENDIF
     C                   ENDIF
     C
     C                   ENDSR
      *===============================================================
      * WRT081- Write the '08' note record
      *===============================================================
     C     WRT081        BEGSR
     C                   CLEAR                   NOTE2
     C                   CLEAR                   VFDATA
     C                   MOVEL     '08'          N2ID
     C                   MOVEL     QQFND         N2FND
     C                   MOVEL     UFICL#        N2CLM#
     C                   Z-ADD     UFNID#        N2RS
     C                   Z-ADD     XXNLS#        N2ARS
     C                   MOVEL     'CLREV'       N2RT
     C                   MOVEL     XXENTX        N2COM
     C
     C                   MOVEL     NOTE2         VFDATA
     C                   WRITE     PVF01
     C
     C                   ENDSR
      *===============================================================
      * WRTHST- Write the History record in CLIMPORT File
      *===============================================================
     C     WRTHST        BEGSR
     C
     C                   CLEAR                   HISTRY
     C                   CLEAR                   VFDATA
     C                   CLEAR                   SRDTI
     C
     C                   MOVEL     '05'          HIID
     C                   MOVEL     QQFND         HICLM#
     C
     C                   IF        UDRETY  = 'A'
     C                   MOVEL     'O'           HICLMS
     C                   ENDIF
     C
     C                   IF        UDRETY  = 'E'
     C                   MOVEL     'C'           HICLMS
     C                   ENDIF
     C
     C                   MOVEL     UDRCL#        HICLM#
     C
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD1
     C                   MOVEL     SRDTO         HISTD
     C
     C                   MOVEL     HISTRY        VFDATA
     C                   WRITE     PVF01
     C                   ENDSR
