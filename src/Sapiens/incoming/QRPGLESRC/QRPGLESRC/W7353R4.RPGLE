     Hcopyright('(c) 2025 StoneRiver, Inc. All rights reserved. -                     *
     H StoneRiver is a trademark of StoneRiver, Inc.')                                *
     H/TITLE W7353R4 - WCSTAT Transfer Data                                           *
     H dftactgrp(*no) actgrp(*caller)                                                 *
     H option(*nodebugio:*srcstmt)                                                    *
     H*********************************************************************************
     H*  Description:  Transfer extraction data to WCSTAT file.                       *
     H*                                                                               *
     H*  Called from: W7353C3                                                         *
     H*  Calls to:                                                                    *
     H*                                                                               *
     H*                                                                               *
     H*  Created: 01/06/06    Programmer C:15    Project: 114Wxx                      *
     H*********************************************************************************
     H*  Revisions:                                                                   *
     H*                                                                               *
     H* 08/22/25 C:333 Support: 119618 (AmWINS)                                       *
     H*                WCSTAT CDX dropping USRs due to 9740 mod errorr                *
     H*                - Changes for RT4 (47-50), reserved for future use.            *
     H*                                                                               *
     H* 08/18/25 C:65  Project: Phw#119560    (Sunz/WC2952)                           *
     H*                Decrease rate to 7.3 size as the exposure record layout for    *
     H*                WCSTAT did not change - it should have stayed 7.3 size.        *
     H*                                                                               *
     H* 02/20/25 C:342 Support: 118529                                                *
     H*                Populate WCSTATX and WCSTATXG with record type 5               *
     H*                These files are used by W7353R7 to populate WCSTAT05           *
     H*                Changed SLRVCD to SLSS# in Loss DS                             *
     H*                                                                               *
     H* 09/20/24 C:344 Project: WC2952                                                *
     H*                Increase rate to 8.3 zoned.                                    *
     H*                                                                               *
     H* 01/02/18 C:267 Project: C150WCI                                               *
     H*                Added Business Segment ID# field in the header DS              *
     H*                                                                               *
     H* 09/01/16 C:65  Project: Phw# 42692 (LUBA)                                     *
     H*                Populate a 'N' into the certified health care network field.   *
     H*                Program variable to be replaced if we add a database field     *
     H*                for the network flag.                                          *
     H*                                                                               *
     H*  01/22/13  C:287 Seq#: SY266_OVR                                              *
     H*                  Added DFTACTGRP and ACTGRP compile options                   *
     H*                                                                               *
     H*  11/23/10  C:190 Project: 139WPQ                                              *
     H*                  Add new mode to transfer the data from the work files to     *
     H*                  WCSTATXG in addition to WCSTATX.                             *
     H*                                                                               *
     H*  06/10/09  C:65  Project: Supp# 66722                                         *
     H*                  If reporting to NJ then update all exposure with a split     *
     H*                  code of 00 to use the ARD date for the rate and mod dates.   *
     H*                                                                               *
     H*  01/12/09  C:65  Project: 132WAZ                                              *
     H*                  Use carrier # 12985 for any NCCI submissions for Compwest    *
     H*                  for group 5.                                                 *
     H*                                                                               *
     H*********************************************************************************
     H*  Indicators:                                                                  *
     H*                                                                               *
     H*  20 - Indicates ASWG submission                                               *
     H*                                                                               *
     H*********************************************************************************
     F* Link, header, name, address file
     FWMSRP     UF   E           K DISK
     F*
     F* Exposure file
     FWDSEL     IF   E           K DISK
     F*
     F* Unit record file
     FWMSQL     IF   E           K DISK
     F*
     FWMEML     IF   E           K DISK
     F*
     FWMRPL     IF   E           K DISK
     F*
     FSSYLF     IF   E           K DISK    include(swc01)
     f*** begin add ***  06/10/09  **********************************************
     Fwdell     UF   E           K DISK
     f*** begin add ***  11/23/10  **********************************************
     f*
     fwcstatxg  if a e             disk
     f*
     f*** end   add ***  11/23/10  **********************************************
     f*** begin add ***  06/10/09  **********************************************
     F*
     FWCSTATX   IF A E             DISK
     F*
      ********* Beg Add *** 02/20/25 *****************************************
     F* Loss file
     FWDSLL     IF   E           K DISK
      ********* End Add *** 02/20/25 *****************************************

     D*  Key field definitions
     D kkdiv           S                   Like(EMDIV)
     D kkfyr           S                   Like(SRFYR)
     D kkey            S                   Like(WCKEY)
     D kkstcd          S                   Like(sestcd)
     D*xxstcd          S                   Like(srstcd)
     c*** begin add ***  06/10/09  **********************************************
     D xxandt          S                   Like(semddt)
     D xxmddt          S                   Like(semddt)
     D xxrtdt          S                   Like(semddt)
     c*** end add *****  06/10/09  **********************************************
     D recFound        s              1
     D x               s              3  0

     D*  Date/time data structure
     D                 DS
     D dsdttm                        12  0
     D  dstime                        6  0 Overlay(dsdttm)
     D  dsdate                        6  0 Overlay(dsdttm:7)

     D xxjuldt         s              5  0
     D xxisodt         s              8  0
     D JulDt           s               d   datfmt(*JUL)
     D IsoDt           s               d   datfmt(*ISO)

     D*** Begin add *** 02/20/25 *******************************************
     DClm              s              1    dim(12)
     DClm2             s              1    dim(12)
     D*** End add ***** 02/20/25 *******************************************
     D*----------------------------
     D*  Transfer data structures
     D*----------------------------
     D ETR             DS
     D  Label                  1     14    INZ('$!+WORKCOMP+!$')
     D  Email                 15     45
     D  Rec_typ               46     47    INZ('  ')
     D  Data_typ              48     50    INZ('WCS')
     D  Data_rcv              51     55
     D  Version               56     63
     D  Subm_typ              64     64    INZ('S')
     D  Subm_rpl_ind          65     72
     D  Data_prov             73     77
     D  Contact               78    102
     D  Rcpt_cd              103    103    INZ('E')
     D  Phone#               104    113
     D  Phone_ext            114    119
     D  Fax#                 120    129
     D  Proc_dt              130    137
     D  Prov_adr             138    238
     D  Prov_ind             239    239    INZ('T')
     D  TPA_FEIN             240    248    INZ('251686685')
     D Link            DS
     D  SRCARR                 1      5  0
     D  Policy#                6     23
     D     FND                 6      8  0
     D     EMP                 9     17  0
     D     FYR                18     20  0
     D  SRCRT#                24     30
     D  SRSTCD                31     32  0
     D  Pol_Efdt              33     38  0
     D  SRRPT#                39     39
     D  SRCORR                40     40
     D*
     D Header          DS
     D  Hdr_type               1      1  0 INZ(1)
     D  SRPCON                 2     13  0
     D*  reserved
     D  Pol_Exp               15     20  0
     D  SRRSK#                21     29
     D*  reserved
     D  SRPND#                31     40
     D  SRTERM                41     41  0
     D  SRURRI                42     42
     D  SRPTYP                43     44  0
     D*  reserved
     D  SRRRPT                65     65
     D*** begin add ***  01/02/18  *********************************************
     D  SRBSI#                66     72  0
     D*** end   add ***  01/02/18  *********************************************
     D*  reserved
     D  SRCTYP                82     82
     D  State_eff             83     88  0
     D  SRFEIN                89     97  0
     D*  reserved
     D  SRPOLC               106    112
     D*** begin add ***  09/01/16  *********************************************
     D  xxCHCN               113    113
     D*** end add *****  09/01/16  *********************************************
     D*  reserved
     D  SRPID#               117    122  0
     D  SRDTYP               125    128  0
     D  SRDPCT               129    130  0
     D  SRDAMP               131    139  0
     D  SRDAGG               140    148  0
     D  SRPRP#               149    150  0
     D*  reserved
     D  SRPCOR               152    152
     D  SRPCAR               153    157  0
     D  Prev_pol#            158    175
     D  Prev_effdt           176    181  0
     D  SRPEXP               182    183  0
     D  SRPCER               184    190
     D*  reserved
     D  SRASWG               210    210
     D*
     D Name            DS
     D  Nme_type               1      1  0 INZ(2)
     D  SRNAME                 2     80
     D*  reserved
     D* note: NJ only uses pos 2 - 51 for insured's name
     D*
     D Address         DS
     D  Add_type               1      1  0 INZ(3)
     D  SRADDR                 2     80
     D*  reserved
     D* note: NJ only uses pos 2 - 51 for insured's address
     D*
     D Exposure        DS
     D  Exp_type               1      1  0 INZ(4)
     D*  reserved
     D  Class_code             3      6  0
     D*  reserved
      *** Beg Del ***** 08/22/25 *******************************************
     D***SEPRVR                 8      8  0
     D***SEEXP#                 9     10  0
      *** End Del ***** 08/22/25 *******************************************
     D  Exp_mod               11     14  3
     D  Mod_eff_dt            15     20  0
     D  Rat_eff_dt            21     26  0
     D  SEEXPA                27     36  0
     D  SEPRMA                37     45  0
     D*** begin chg *** 09/20/24 *******************************************
     D***SEMNRT                46     52  3
     D*** begin chg *** 08/18/25 *******************************************
     D**SEMNRT                46     53  3
     D***SESPLT                53     53  0
     D**SESPLT                54     54  0
     D  exp_Rate              46     52  3
     D  SESPLT                53     53  0
     D*** end chg ***** 08/18/25 *******************************************
     D*** end   chg *** 09/20/24 *******************************************
     D*  reserved
     D  SETIER                71     72  0
     D*  reserved
     D  SEUPDT                81     81
     D*  reserved
     D  SEEXPO                83     84  0
     D*  reserved
     D*
     D*  reserved
     D*
      ********* Beg Add *** 02/20/25 *****************************************
     D Loss            DS
     D  Los_type               1      1  0 INZ(5)
     D*  reserved
     D  ClassCode              3      6  0
     D*  reserved
     D  SLPRVR                 8      8  0
     D  SLLOS#                 9     10  0
     D  SL#CLM                11     14  0
     D  Acc_date              15     20  0
     D  SLCLM#                21     32
     D  SLCLST                33     33  0
     D  SLAWW                 34     38  0
     D  SLINJ#                39     40  0
     D  SLCAT#                41     42  0
     D  SLINDM                43     51  0
     D  SLMEDI                52     60  0
     D  SLSS#                 61     69  0
     D*  reserved
     D  SLCAS#                72     80
     D  SLUPDT                81     81
     D*  reserved
     D  SLLC1                 83     84  0
     D  SLLC2                 85     86  0
     D  SLLC3                 87     88  0
     D  SLLC4                 89     90  0
     D  SLLC5                 91     92  0
     D  SLVRHB                93     99  0
     D*  reserved
     D  SLJRST               100    101  0
     D  SLMCTP               102    103  0
     D  SLPART               104    105  0
     D  SLNATR               106    107  0
     D  SLCAUS               108    109  0
     D  SLOCCU               110    127
     D  SLVOCR               128    128
     D  SLLUMP               129    129
     D  SLFRAU               130    131  0
     D*  reserved
     D  SLPDIN               134    142  0
     D  SLPDMD               143    151  0
     D  SLCATF               152    160  0
     D  SLEATF               161    169  0
     D  SLDEDR               170    178  0
     D  SLTGI                179    187  0
     D*  reserved
     D  SLALAP               190    198  0
     D  SLALAI               199    207  0
     D  SLIPCT               208    210  0
      ********* End Add *** 02/20/25 *****************************************
     D Unit            DS
     D  Unt_type               1      1  0 INZ(6)
     D  SQEPAY                 2     12  0
     D  SQEOTH                13     22  1
     D  SQSUBP                23     32  0
     D  SQSTDP                33     43  0
     D  SQ#CLT                44     48  0
     D  SQTIND                49     58  0
     D  SQTMED                59     68  0
     D  SQ#RCD                69     73  0
     D  SQPRPT                74     74  0
     D*  reserved
     D  SQPDIN                83     92  0
     D  SQPDMD                93    102  0
     D  SQCLAF               103    112  0
     D  SQEMAF               113    122  0
     D  SQTALP               123    132  0
     D  SQTALI               133    142  0
     D*  reserved
     D*
     D Submission      DS
     D  Sub_type               1      1  0 INZ(9)
     D  Dtl_rec#               2      9  0
     D  Ttl_units             10     16  0
     D*  not required
     D  #ICRs                 23     30  0 INZ(0)
     D*  reserved
     D  Sub_ASWG             210    210

     d*** begin add ***  11/23/10  **********************************************
     d data            ds           265
     d  eregdata               1    250
     d  egroup               251    253
     d  epolyr               254    256
     d  eemp#                257    265
      *
     d a1data        e ds                  extname(wcstatxg)
     d*** end   add ***  11/23/10  **********************************************

     D*  Filler constant for submission record
     D Filler          C                   Const('99999999999999999999999999999+
     D                                            99999999999')
     D*

     DPSDS           ESDS                  Extname(SZQ1P)

     C*---------------
     C*  Key list
     C*---------------
     C*    Keys for extracted data files
     C*
     C     key1          klist
     C                   kfld                    srco#
     C                   kfld                    srfnd
     C                   kfld                    srrpmy
     C                   kfld                    sremp#
     C                   kfld                    srfyr
     C                   kfld                    srstcd
     C*
     C*  Key for employer master file
     C     emky          klist
     C                   kfld                    srco#
     C                   kfld                    srfnd
     C                   kfld                    sremp#
     C*
     C*  Key for carrier/state reporting master file
     C     rpky          klist
     C                   kfld                    srco#
     C                   kfld                    srfnd
     C                   kfld                    sremp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr

     c*** begin add ***  11/23/10  **********************************************
     c     *entry        plist
     c                   parm                    @@mode            1
     c*** end   add ***  11/23/10  **********************************************

     C     dtmdyy        plist
     C                   parm                    srdat_in          8 0
     C                   parm      '*CYMD'       srfr_fmt          8
     C                   parm      *blanks       srto_fmt          8
     C                   parm                    srdat_out        10
     C                   parm                    srerror           1

     C*** begin add ***  01/12/09  *******************************************************
     C                   call      'RTVCLTID'
     C                   parm                    cltid             3
     C*** end add *****  01/12/09  *******************************************************

     C                   time                    dsdttm
     C*
     C                   z-add     0             Dtl_rec#
     C                   z-add     0             Ttl_units
     C*

     C*--------------------
     C*  Main processing
     C*--------------------
     C
     C                   eval      recFound = *off
     C                   read      PSR01                                  99
     C*
     C                   dow       *in99 = *off
     c*** begin add ***  06/10/09  **********************************************
     c                   clear                   xxandt
     c*** end add *****  06/10/09  **********************************************
     c*** begin add ***  01/12/09  *****************************************
     c                   if        cltid = '132' and @@stcd = *blanks
     c                             and srfnd = 5
     c                   z-add     12985         srcarr
     c                   endif
     c*** end add *****  01/12/09  *****************************************
     C*
     C                   exsr      LnkHdrNmAd
     C*
     C                   read      PSR01                                  99
     C                   enddo
     C*
     C                   exsr      Sr_Submit
     C*
     C*  Call CL to transfer WCSTATX to save member.
     C*
     C                   call      'W7353CA3'
     C                   parm      *blanks       @@stcd            2
     C*
     C                   eval      *inlr = *on
     C                   return
     C*

     C*==========================================================================
     C*  LnkHdrNmAd  - subroutine to transfer WMSRP files to WCSTAT file
     C*==========================================================================
     C     LnkHdrNmAd    begsr
     C*
     C*  Verify that the policies rating state matches the state being
     C*  reported to.
     C*
     C**                 if        @@stcd = *blanks
     C**                           and srstcd <> 04 and srstcd <> 07
     C**                           and srstcd <> 21 and srstcd <> 29
     C**                           and srstcd <> 37
     C**                           or xxstcd = srstcd
     C**                           or xxstcd = 07 and srstcd = 37               07=DE & 37=PA
     C**                           or xxstcd = 37 and srstcd = 07               07=DE & 37=PA

     C*    set flag that at least one record is found on
     C                   eval      recFound = *on
     C*
     C*    Assign data structure values
     C                   z-add     SREFDT        Pol_Efdt
     C*
     C                   z-add     SRCNDT        Pol_Exp
     C                   z-add     SRSTED        State_Eff
     C                   movel(p)  SRPPOL        Prev_pol#
     C                   z-add     SRPEFD        Prev_effdt
     C*
     C*
     C*    Determine state rptg div and check for carrier policy#
     C     emky          chain     PEM01                              60
     C                   dow       (*in60 = *off) and (EMSTRP <> 'Y')
     C     emky          reade     PEM01                                  60
     C                   enddo                                                  *in60=*off
     C*
     C                   if        *in60=*off
     C*
     C                   eval      kkdiv = EMDIV
     C                   eval      kkfyr = SRFYR
     c*** begin add ***  06/10/09  **********************************************
     c*
     c* Check for ARD for reporting exposures.
     c*
     c     rpky          chain     pel01                              60
     c                   if        *in60 = *off and elandt <> 0
     c                   z-add     elandt        xxandt
     c                   endif
     c*** end add *****  06/10/09  **********************************************
     C     rpky          chain     PRP01                              60
     C                   if        (*in60=*off) and (RPCAP# <> *blank)
     C                   movel(p)  RPCAP#        Policy#
     C                   else
     C                   eval      fnd = srfnd
     C                   eval      emp = sremp#
     C                   eval      fyr = srfyr
     C                   endif                                                  *in60=*off
     C*
     C                   endif                                                  *in60=*off
     C*** begin add ***  09/01/16  *********************************************
     C                   Eval      xxCHCN = 'N'
     C*** end add *****  09/01/16  *********************************************

     C*
     C*  Load RDATA and call subroutine to write to WCSTATX
     C*                Header
     C                   eval      RDATA = Link + Header
     C                   exsr      Writer
     C                   add       1             Dtl_rec#
     C*
     C*                Name
     C                   eval      RDATA = Link + Name
     C                   exsr      Writer
     C                   add       1             Dtl_rec#
     C*
     C*                Address
     C                   eval      RDATA = Link + Address
     C                   exsr      Writer
     C                   add       1             Dtl_rec#

     C*
     C*    Call subroutine to write all exposure recs associated with this emp#/fyr
     C                   exsr      Sr_Expo

      ********* Beg Add *** 02/20/25 *****************************************
     C*    Call subroutine to write all loss recs associated with this emp#/fyr
     C                   exsr      Sr_Loss
      ********* End Add *** 02/20/25 *****************************************
     C*
     C*    Call subroutine to write unit record associated with this emp#/fyr
     C                   exsr      Sr_Unit

     C*
     C*    Indicate submission as ASWG if unit ASWG flag is 'A'
     C                   if        SRASWG = 'A'
     C                   seton                                        20
     C                   endif
     C
     C**                 endif
     C*
     C                   endsr                                                  LnkHdrNmAd

     C*==========================================================================
     C*  Sr_Expo  - subroutine to transfer WDSEP files to WCSTAT file
     C*==========================================================================
     C     Sr_Expo       begsr
     C*
     c*** begin add ***  06/10/09  **********************************************
     c*
     c* if reporting to NJ then find the earliest rate/mod dates for the 1st split
     c* period, which will be the ARD date.
     c*
     c                   move      *hival        xxmddt
     c                   move      *hival        xxrtdt
     c
     c                   if        @@stcd = '29'
     c
     C     key1          chain     PSE01                              60
     C                   dow       *in60 = *off
     c
     c                   if        sesplt = 0
     c
     c                   if        sertdt < xxrtdt
     c                   z-add     semddt        xxmddt
     c                   z-add     sertdt        xxrtdt
     c                   endif
     c
     c                   endif                                                  sesplt = 0
     c
     C     key1          reade     PSE01                                  60
     C                   enddo
     c
     c
     c                   endif                                                  @@stcd = '29'
     c
     c                   if        xxrtdt = *hival or xxandt = 0
     c                   z-add     0             xxmddt
     c                   z-add     0             xxrtdt
     c                   endif
     c*** end add *****  06/10/09  **********************************************
     C     key1          chain     PSE01                              60
     C                   dow       *in60 = *off
     C*
     C*  Assign data structure values
     C                   move      SECLAS        Class_code
     C                   z-add     SEEXMD        Exp_mod
     C                   z-add     SEMDDT        Mod_eff_dt
     C                   z-add     SERTDT        Rat_eff_dt
     c*** begin add ***  06/10/09  **********************************************
     c                   if        sesplt = 0 and xxrtdt <> 0
     c                   z-add     xxmddt        Mod_eff_dt
     c                   z-add     xxrtdt        Rat_eff_dt
     c                   endif
     c*** end add *****  06/10/09  **********************************************
     C*** begin add *** 08/18/25 *******************************************
     C                   z-add     SEMNRT        exp_Rate
     C*** end add ***** 08/18/25 *******************************************

     C*
     C*  Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Link + Exposure
     C                   exsr      Writer
     C                   add       1             Dtl_rec#

     C*
     C     key1          reade     PSE01                                  60
     C                   enddo
     C*
     C                   endsr                                                  Sr_Expo
     C*
      ********* Beg Add *** 02/20/25 *****************************************
     C*==========================================================================
     C*  Sr_Loss - subroutine to transfer WDSLP files to WCSTAT file
     C*==========================================================================
     C     Sr_Loss       begsr
     C*
     C     key1          chain     PSL01                              60
     C                   dow       *in60 = *off
     C*
     C*  Remove hyphens from the claim #.
     C*
     C                   clear                   clm
     C                   clear                   clm2
     C                   movea     slclm#        clm
     C                   z-add     12            t                 2 0
     C                   z-add     12            u                 2 0
     C*
     C                   do        12
     C     clm(t)        ifne      '-'
     C                   move      clm(t)        clm2(u)
     C                   sub       1             u
     C                   endif
     C                   sub       1             t
     C                   enddo
     C*
     C                   movea     clm2          slclm#
     C*  Assign data structure values
     C                   move      SLCLAS        ClassCode
     C                   z-add     SLACDT        Acc_date
     C                   move      *blanks       slcas#

     C*
     C*  Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Link + Loss
     C                   exsr      Writer
     C                   add       1             Dtl_rec#

     C*
     C     key1          reade     PSL01                                  60
     C                   enddo
     C*
     C                   endsr                                                  Sr_Loss
     C*
      ********* End Add *** 02/20/25 *****************************************
     C*==========================================================================
     C*  Sr_Unit - subroutine to transfer WMSQP files to WCSTAT file
     C*==========================================================================
     C     Sr_Unit       begsr
     C*
     C     key1          chain     PSQ01                              60
     C                   if        *in60 = *off
     C*
     C                   if        sqrpt# <> '1'
     C                   eval      sqepay = 0
     C                   eval      sqeoth = 0
     C                   eval      sqsubp = 0
     C                   eval      sqstdp = 0
     C                   endif

     C*
     C*  Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Link + Unit
     C                   exsr      Writer
     C                   add       1             Dtl_rec#
     C                   add       1             Ttl_units
     C*
     C                   endif
     C*
     C                   endsr                                                  Sr_Unit
     C*
     C*==========================================================================
     C*  Sr_Submit  - subroutine to transfer submission record to WCSTAT file
     C*==========================================================================
     C     Sr_Submit     begsr
     C*
     C                   if        *in20 = *on
     C                   movel(p)  'WC'          kkey
     C     kkey          chain     SWC01                              60
     C*
     C                   if        WCFL13 = 'Y'
     C                   eval      Sub_ASWG = 'T'
     C                   else
     C                   eval      Sub_ASWG = 'P'
     C                   endif
     C*
     C                   endif                                                  *in20=*on

     C
     C                   if        recFound = *on

     C*  Load RDATA and call subroutine to write to WCSTATX
     C                   eval      RDATA = Filler + Submission
     C                   exsr      Writer
     C
     C                   endif                                                  recFound=*on

     C*
     C                   endsr                                                  Sr_Submit
     C*
     C*==========================================================================
     C*  Writer - subroutine to write a record to the WCSTAT file
     C*==========================================================================
     C     Writer        begsr
     C*
     C                   write     RSTAT
     c*** begin add ***  11/23/10  **********************************************
     c                   if        @@mode = 'G'
     c                   movel(p)  rdata         eregdata
     c                   movel     srfnd         egroup
     c                   movel     srfyr         epolyr
     c                   movel     sremp#        eemp#
     c                   movel(p)  data          a1data
     c                   write     rstatg
     c                   endif
     c*** end   add ***  11/23/10  **********************************************
     C*
     C                   endsr                                                  Writer
     C*
