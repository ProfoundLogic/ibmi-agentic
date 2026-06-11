     hCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/title WTCLMRWK - Reserve Worksheet
     h dftname(WTCLMRWK)
     h option(*nodebugio: *srcstmt)
     h/define   PROFOUNDUI
     h bnddir('WTBNDDIR')
     h**********************************************************************
      *====================================================================*
      *  Description:  This Program Will Display Reserve Worksheet History *
      *                                                                    *
      *  Called from: Claims Toolbar                                       *
      *                                                                    *
      *  Created on 12/01/2014 by c:279 for Project: 114WLF                *
      *====================================================================*
      *  Revisions:                                                        *
      *                                                                    *
      *  06/23/21 C:318 Project WC2302 - Auto tab closure for
      *                deletion.
      *
      *  06/09/20  C:298 Project: SY420
      *                  Mask SSN
      *
      *  11/21/19 C:079 Phaseware: 129/85284                               *
      *                 Fixed JavaScript refresh_summers routine on the    *
      *                 root directory and the repository                  *
      *                                                                    *
      *  10/29/19 C:062 Ticket: 84644(045)                                 *
      *                 Fix comment parsing.                               *
      *
      *  09/10/19 C:079 Phaseware: 152/81909                               *
      *                 Javascript changes due to Summers changing fields  *
      *                 from numbers to alphas.  Getting rid of commas     *
      *                 before converting back to number                   *
      *
      *  04/29/19 C:298 Phaseware: 78974                                   *
      *                 Edit Word is defined for all Decimal fields        *
      *                 in Display File.                                   *
      *                                                                    *
     H* 08/15/17 C:33  Project: SY394                                 *
     H*                Update initial reserve flag                    *
      *                                                                    *
      *  08/12/16 C:308 C114MWK: Recompile                                 *
      *                                                                    *
      *  06/22/16 C:136 Phaseware: 41700                                   *
      *                 Do not pop up edit window if user can approve      *
      *                 even if they are not the next level user           *
      *                                                                    *
      *  04/18/16 C:310 Support: 40391                                     *
      *                -Modified code in order to fetch valid reserve code *
      *                 that are set up on particular group.               *
      *                                                                    *
      *  03/23/16 C:308 Project: ELEVATE                                   *
      *                -Recompile Only                                     *
      *                                                                    *
      * 01/07/16 C:65  Project: WC2401                                     *
      *                Change the coding done under WC2254 to call an      *
    *                API instead of directly calling the various         *
      *                programs for windows here.                          *
      *                                                                    *
      * 12/09/15 C:62 PROJECT SY371                                        *
      *               Add Scriptura processing.                            *
      *                                                                    *
      *  12/07/15 C:307 Support: 38586                                     *
      *                -Next level user pop up not coming on Save/Approve  *
      *                                                                    *
     h*  11/25/15 C:33  38610                                              *
     h*                 Ensure path includes AO                            *
      *                                                                    *
      *  10/09/15 C:306 Project: 114WVN                                    *
      *                 Additional Changes for Reserve Worksheet           *
      *                                                                    *
      *  10/08/15 C:136 Support: 37105                                     *
      *                 Always populate reserve type fields so that        *
      *                 approve button will populate reserve history       *
      *                 changes for those categories with multiple         *
      *                 reserve types                                      *
      *                                                                    *
      *  09/17/15 C:302 Project: wc2254                                    *
      *                 Recompile only                                     *
      *                                                                    *
      *  06/10/15 C:136 Project: 114WLF                                    *
      *                 Refine claim edits so that they are index          *
      *                 specific when changing reserves                    *
      *                                                                    *
      *  04/16/15 C:65  Project: SY251                                     *
      *                 Add API call to launch new Department letters.     *
      *                                                                    *
      * ------------------------------------------------------------------ *
      *  NOTE: Please add comments to the top of Revisions.                *
      * ------------------------------------------------------------------ *
      *====================================================================*
     fwtclmrwk  cf   e             workstn extfile('WTCLMRWK')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(sflWkHst:rrnWkHst)
     fwtactpnl  cf   e             workstn
     f                                     Handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdClmPnl)
     fststl1    if   e           k disk
     fwmeml     if   e           k disk    rename(pem01:pem00)
     fwmcmp     uf   e             disk    rename(pcm01:pcm0p)
     fwmrhl2    o    e           k disk    rename(prh01:prh02)
     fwmawl1    if   e           k disk
     fwmcwl     if   e           k disk    rename(pcw01:pcw00)
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     fwdell     uf   e           k disk    ReName(pel01:pel00)
     fwdcwl     if   e           k disk
     fwdrwp     uf a e             disk    rename(prw01:prw0p)
     f                                     infds(dbfds1)
     fwdrwl     if   e           k disk    rename(prw01:prw00)
     f                                     infds(dbfds)
     fwtabl     if   e           k disk
     f                                     rename(prt01:prt00)
     f                                     ignore(prs01)
     f                                     ignore(psy01)
     f                                     ignore(ptr01)
     f                                     ignore(ppa01)
     f                                     ignore(pat01)
     f                                     ignore(psp01)
     f                                     ignore(pnc01)
     f                                     ignore(pna01)
     f                                     ignore(pca01)
     f                                     ignore(prc01)
     f                                     ignore(ppc01)
     f                                     ignore(pb201)
     f                                     ignore(pb701)
     f                                     ignore(pbs01)
     f                                     ignore(ptt01)
     f                                     ignore(pcc01)
     f                                     ignore(pbc01)
     f                                     ignore(pcs01)
     f                                     ignore(pcn01)
     f                                     ignore(plr01)
     f                                     ignore(pvs01)
     f                                     ignore(phy01)
     f                                     ignore(pba01)
     f                                     ignore(pid01)
     f                                     ignore(pis01)
     fwtrptwso  o    e             printer usropn
     f                                     oflind(*in99)
     f*** Begin Add *** 12/09/15 ***************************************
     FSXRWLRPT  uf a e           k disk    Rename(prwr01:prwr00)
     FSXRWPSEC  uf a e             disk    Rename(prws01:prws0p)
     FSXRWPBEN  uf a e             disk    Rename(prwb01:prwb0p)
     FSXRWPAMT  uf a e             disk    Rename(prwa01:prwa0p)
     FSXRWPRSV  uf a e             disk    Rename(prwv01:prwv0p)
     FSMCOL     if   e           k disk    Rename(pco01:pco00)
     FXMLPREL1  if   e           k disk    Rename(xp100:pxp01)
     f*** End   Add *** 12/09/15 ***************************************

      /copy sCopyClmPr
       //*** begin add ***  04/16/15  ****************************************
     d/copy sPrLetter
       //*** begin add ***  04/16/15  ****************************************
      //***  Begin Add  ***  08/15/17  ************************************
      /copy wPrClmRsv
      //***  End   Add  ***  08/15/17  ************************************
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************

     d psds          esds                  extname(szq1p)

     d dbfDS           ds
     d  dbRrn                397    400b 0
     d dbfDS1          ds
     d  dbRrn1               397    400b 0

      * Work variables
     d ##key           s             40
     d ##comm          s            256
     d ##cDat          s              8
     d ##dcId          s             12    inz(*blanks)
     d ##pNam          s             10    inz('W0070R')
     d ##secl          s              3    inz('000')
     d ##user          s             10
     d ##pgm           s             10
     d ##fold          s             80    inz(*blanks)
     d ##diFl          s              1    inz(*blanks)
     d ##iPgm          s             10
     d ##fl01          s              1    inz(*blanks)
     d ##fl02          s              1    inz(*blanks)
     d ##rmCd          s              5
     d ##seq#          s              5    inz('00000')
     d ##sbyl          s              1    inz('N')
     d ##com2          s             19    inz(*blanks)
     d ##apId          s              2    inz(*blanks)
     d ##iDoc          s             12    inz(*blanks)
     d ##iQue          s             10    inz(*blanks)
     d ##imgF          s              1    inz(*blanks)
     d ##iPth          s             10    inz(*blanks)
     d ##pIdx          s              5    inz('00000')
     d ##qPty          s              1    inz(*blanks)
     d ##dcTp          s              5    inz(*blanks)
     d ##csId          s              2    inz(*blanks)
     d ##code          s              2    inz(*blanks)
     d ##root          s             10
     d ##co            s              3
     d ##fnd           s              3
     d ##pre           s              2
     d ##rtcd          s              1
     d ##splf          s             10
      *** beg chg ***** 06/10/15 *********************
     d****** arrErrInd       s              1    dim(25)
     d arrErrInd       s              1    dim(45)
      *** end chg ***** 06/10/15 *********************
     d charRsvType     s              1
     d charRsvCode     s              2
     d isPrcMode       s              1
     d isFirst         s               n
     d isLoad          s               n
     d isError         s               n
     d isSkip          s               n
     d isDiscard       s               n
     d isMedExcl       s               n
     d isMenuAvbl      s               n   inz(*on)
     d intRrn#         s              9  0
     d inMode          s              1
     d isNotValid      s               n   inz(*off)
     d isChkFlg        s              1
     d intCo#          s                   like(CMco#)
     d intFnd          s                   like(CMfnd)
     d intCase         s                   like(CMcase)
     d intFyr          s                   like(CMfyr)
     d intCount        s              5  0
     d intCount1       s              3  0
     d intCmpRt        s                   like(AWcRt)
     d intIndex        s              5  0
     d intTotal        s             13  2
     d intLmt          s             11  2
     d intAmt          s              9  2
     d intLen          s              5  0
     d intpos          s              5  0
     d intRetval       s              5  0
     d intLine         s              5  0
     d intLine1        s              5  0
     d retval          s              5  0
     d intCnt          s              5  0
     d intCnt1         s              5  0
     d intPrvVal       s              5  0
     d intArrSiz       c                   22
     d intArrSiz1      c                    6
     d intArrSiz2      c                   37
     d outClientID     s              3    inz('000')
     d strMsg          s            200
     d strDate         s              8
     d strKey          s             10
     d strKey#         s             40
     d strSeq#         s              5
     d strStrng        s             40
     d strBlanks       s              1
     d strString       s            200
     d strString1      s           5000
     d scco#           s                   like(CMco#)
     d scfnd           s                   like(CMfnd)
     d scfyr           s                   like(CMfyr)
     d scemp#          s                   like(CMemp#)
     d scdiv           s                   like(CMdiv)
     d*** Begin Add *** 12/09/15 *********************************************
     D isScriptUsed    s              1A   inz('N')
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     D outCall         s             11
     D InXML           s             60
     D InReturn        s            250
     D OutMode         s              1
     D OutRrn          s              9
     D OutPrefix       s              2
     D OutString       s             10
     D OutCo#          s              3
     D OutFnd          s              3
     D procType        s            100
     d OutMassFlag     s              1    Inz('N')
     d OutXmlLink      s             60    Inz(' ')
     d inCode          s              2
     d*** End   Add *** 12/09/15 *********************************************
     d***  Begin Add  ***  06/23/21  ****************************************
     d OutCmRrnTm      s              9  0
     d***  End   Add  ***  06/23/21  ****************************************

     d  groupRsvDs     ds
     d  FNrs01
     d  FNrs02
     d  FNrs03
     d  FNrs04
     d  FNrs05
     d  FNrs06
     d  FNrs07
     d  FNrs08
     d  FNrs09
     d  FNrs10
     d  groupRsv                      2    overlay(groupRsvDs) dim(10)

     d                 ds
     d  rsvPaidDs              1     90  2
     d                                     Dim(10)
     d  CMar01                 1      9  2
     d  CMar02                10     18  2
     d  CMar03                19     27  2
     d  CMar04                28     36  2
     d  CMar05                37     45  2
     d  CMar06                46     54  2
     d  CMar07                55     63  2
     d  CMar08                64     72  2
     d  CMar09                73     81  2
     d  CMar10                82     90  2

     d                 ds
     d  rsvRemainDs            1     90  2
     d                                     Dim(10)
     d  CMrr01                 1      9  2
     d  CMrr02                10     18  2
     d  CMrr03                19     27  2
     d  CMrr04                28     36  2
     d  CMrr05                37     45  2
     d  CMrr06                46     54  2
     d  CMrr07                55     63  2
     d  CMrr08                64     72  2
     d  CMrr09                73     81  2
     d  CMrr10                82     90  2

     d                 ds
     d  RT                     1     20
     d                                     Dim(10)
     d  CMrt01                 1      2
     d  CMrt02                 3      4
     d  CMrt03                 5      6
     d  CMrt04                 7      8
     d  CMrt05                 9     10
     d  CMrt06                11     12
     d  CMrt07                13     14
     d  CMrt08                15     16
     d  CMrt09                17     18
     d  CMrt10                19     20

     d                 ds
     d  hdg                    1    880
     d                                     Dim(intArrSiz)
     d  CWBfH1                 1     40
     d  CWBfH2                41     80
     d  CWBfH3                81    120
     d  CWBfH4               121    160
     d  CWBfH5               161    200
     d  CWBfHP               201    240
     d  CWIfH1               241    280
     d  CWIfH2               281    320
     d  CWIfH3               321    360
     d  CWIfH4               361    400
     d  CWMfH1               401    440
     d  CWMfH2               441    480
     d  CWMfH3               481    520
     d  CWMfH4               521    560
     d  CWEfH1               561    600
     d  CWEfH2               601    640
     d  CWEfH3               641    680
     d  CWEfH4               681    720
     d  CWLfH1               721    760
     d  CWLfH2               761    800
     d  CWLfH3               801    840
     d  CWLfH4               841    880

     d                 ds
     d  wks                    1     18  0
     d                                     Dim(intArrSiz1)
     d  WksA                   1     18
     d  RWBWk1                 1      3  0
     d  RWBWk2                 4      6  0
     d  RWBWk3                 7      9  0
     d  RWBWk4                10     12  0
     d  RWBWk5                13     15  0
     d  RWBpWk                16     18  0

     d                 ds
     d  cmpRt                  1     42  2
     d                                     Dim(intArrSiz1)
     d  CmpRta                 1     42
     d  RWBCR1                 1      7  2
     d  RWBCR2                 8     14  2
     d  RWBCR3                15     21  2
     d  RWBCR4                22     28  2
     d  RWBCR5                29     35  2
     d  RWBPRT                36     42  2

     d                 ds
     d  Amt                    1    198  2
     d                                     Dim(intArrSiz)
     d  Amta                   1    198
     d  RWBAm1                 1      9  2
     d  RWBAm2                10     18  2
     d  RWBAm3                19     27  2
     d  RWBAm4                28     36  2
     d  RWBAm5                37     45  2
     d  RWBpAm                46     54  2
     d  RWFAm1                55     63  2
     d  RWFAm2                64     72  2
     d  RWFAm3                73     81  2
     d  RWFAm4                82     90  2
     d  RWMAm1                91     99  2
     d  RWMAm2               100    108  2
     d  RWMAm3               109    117  2
     d  RWMAm4               118    126  2
     d  RWEAm1               127    135  2
     d  RWEAm2               136    144  2
     d  RWEAm3               145    153  2
     d  RWEAm4               154    162  2
     d  RWLAm1               163    171  2
     d  RWLAm2               172    180  2
     d  RWLAm3               181    189  2
     d  RWLAm4               190    198  2

     d                 ds
     d  scHeadings             1    880
     d                                     Dim(intArrSiz)
     d  scIndHdg1              1     40
     d  scIndHdg2             41     80
     d  scIndHdg3             81    120
     d  scIndHdg4            121    160
     d  scIndHdg5            161    200
     d  scIndHdg6            201    240
     d  scIndFHdg1           241    280
     d  scIndFHdg2           281    320
     d  scIndFHdg3           321    360
     d  scIndFHdg4           361    400
     d  scMedFHdg1           401    440
     d  scMedFHdg2           441    480
     d  scMedFHdg3           481    520
     d  scMedFHdg4           521    560
     d  scExpFHdg1           561    600
     d  scExpFHdg2           601    640
     d  scExpFHdg3           641    680
     d  scExpFHdg4           681    720
     d  scLglFHdg1           721    760
     d  scLglFHdg2           761    800
     d  scLglFHdg3           801    840
     d  scLglFHdg4           841    880

     d                 ds
     d  scWks                  1     18  0
     d                                     Dim(intArrSiz1)
     d  scWksa                 1     18
     d  scIndFWks1             1      3  0
     d  scIndFWks2             4      6  0
     d  scIndFWks3             7      9  0
     d  scIndFWks4            10     12  0
     d  scIndFWks5            13     15  0
     d  scIndFWks6            16     18  0

     d                 ds
     d  scCmpRt                1     42  2
     d                                     Dim(intArrSiz1)
     d  scIndCmRt1             1      7  2
     d  scIndCmRt2             8     14  2
     d  scIndCmRt3            15     21  2
     d  scIndCmRt4            22     28  2
     d  scIndCmRt5            29     35  2
     d  scIndCmRt6            36     42  2

     d                 ds
     d  scAmt                  1    198  2
     d                                     Dim(intArrSiz)
     d  scIndAmt1              1      9  2
     d  scIndAmt2             10     18  2
     d  scIndAmt3             19     27  2
     d  scIndAmt4             28     36  2
     d  scIndAmt5             37     45  2
     d  scIndAmt6             46     54  2
     d  scIndFlxA1            55     63  2
     d  scIndFlxA2            64     72  2
     d  scIndFlxA3            73     81  2
     d  scIndFlxA4            82     90  2
     d  scMedAmt1             91     99  2
     d  scMedAmt2            100    108  2
     d  scMedAmt3            109    117  2
     d  scMedAmt4            118    126  2
     d  scExpAmt1            127    135  2
     d  scExpAmt2            136    144  2
     d  scExpAmt3            145    153  2
     d  scExpAmt4            154    162  2
     d  scLglAmt1            163    171  2
     d  scLglAmt2            172    180  2
     d  scLglAmt3            181    189  2
     d  scLglAmt4            190    198  2

     d                 ds
     d  scAmtAll               1    135  2
     d                                     Dim(15)
     d  scIndDthAt             1      9  2
     d  scIndFunBn            10     18  2
     d  scIndDsFgm            19     27  2
     d  scIndStlmt            28     36  2
     d  scIndMnAdj            37     45  2
     d  scMedPhys             46     54  2
     d  scMedHsptl            55     63  2
     d  scMedSurgy            64     72  2
     d  scMedRxSup            73     81  2
     d  scMedDgnst            82     90  2
     d  scMedPhyTh            91     99  2
     d  scExpSrv             100    108  2
     d  scExpMdRec           109    117  2
     d  scLglDepst           118    126  2
     d  scLglDefAt           127    135  2

     d                 ds
     d dsDspVis                1     22
     d                                     dim(intArrSiz)
     d  dsbInd0                1      1n
     d  dsbInd1                2      2n
     d  dsbInd2                3      3n
     d  dsbInd3                4      4n
     d  dsbInd4                5      5n
     d  dsbInd5                6      6n
     d  dsbInd6                7      7n
     d  dsbInd7                8      8n
     d  dsbInd8                9      9n
     d  dsbInd9               10     10n
     d  dsbMed1               11     11n
     d  dsbMed2               12     12n
     d  dsbMed3               13     13n
     d  dsbMed4               14     14n
     d  dsbExp1               15     15n
     d  dsbExp2               16     16n
     d  dsbExp3               17     17n
     d  dsbExp4               18     18n
     d  dsbLgl1               19     19n
     d  dsbLgl2               20     20n
     d  dsbLgl3               21     21n
     d  dsbLgl4               22     22n

     d                 ds
     d dsVsbAmt                1     22
     d                                     dim(intArrSiz)
     d  dsbInd00               1      1n
     d  dsbInd01               2      2n
     d  dsbInd02               3      3n
     d  dsbInd03               4      4n
     d  dsbInd04               5      5n
     d  dsbInd05               6      6n
     d  dsbInd06               7      7n
     d  dsbInd07               8      8n
     d  dsbInd08               9      9n
     d  dsbInd09              10     10n
     d  dsbMed11              11     11n
     d  dsbMed12              12     12n
     d  dsbMed13              13     13n
     d  dsbMed14              14     14n
     d  dsbExp01              15     15n
     d  dsbExp02              16     16n
     d  dsbExp03              17     17n
     d  dsbExp04              18     18n
     d  dsbLgl01              19     19n
     d  dsbLgl02              20     20n
     d  dsbLgl03              21     21n
     d  dsbLgl04              22     22n

     d                 ds
     d dsVsbArr                1     22
     d                                     dim(intArrSiz)
     d  vsIndRsv0              1      1n
     d  vsIndRsv1              2      2n
     d  vsIndRsv2              3      3n
     d  vsIndRsv3              4      4n
     d  vsIndRsv4              5      5n
     d  vsIndRsv5              6      6n
     d  vsIndRsv6              7      7n
     d  vsIndRsv7              8      8n
     d  vsIndRsv8              9      9n
     d  vsIndRsv9             10     10n
     d  vsMedRsv1             11     11n
     d  vsMedRsv2             12     12n
     d  vsMedRsv3             13     13n
     d  vsMedRsv4             14     14n
     d  vsExpRsv1             15     15n
     d  vsExpRsv2             16     16n
     d  vsExpRsv3             17     17n
     d  vsExpRsv4             18     18n
     d  vsLglRsv1             19     19n
     d  vsLglRsv2             20     20n
     d  vsLglRsv3             21     21n
     d  vsLglRsv4             22     22n

     d                 ds
     d  dsRsvTyp               1     74
     d                                     Dim(intArrSiz2)
     d  scRsvTyp1              1      2
     d  scRsvTyp2              3      4
     d  scRsvTyp3              5      6
     d  scRsvTyp4              7      8
     d  scRsvTyp5              9     10
     d  scRsvTyp7             11     12
     d  scRsvTyp8             13     14
     d  scRsvTyp9             15     16
     d  scRsvTyp10            17     18
     d  scRsvTyp11            19     20
     d  scRsvTyp22            21     22
     d  scRsvTyp23            23     24
     d  scRsvTyp24            25     26
     d  scRsvTyp25            27     28
     d  scRsvTyp28            29     30
     d  scRsvTyp29            31     32
     d  scRsvTyp30            33     34
     d  scRsvTyp31            35     36
     d  scRsvTyp34            37     38
     d  scRsvTyp35            39     40
     d  scRsvTyp36            41     42
     d  scRsvTyp37            43     44
     d  scRsvTyp6             45     46
     d  scRsvTyp12            47     48
     d  scRsvTyp13            49     50
     d  scRsvTyp14            51     52
     d  scRsvTyp15            53     54
     d  scRsvTyp16            55     56
     d  scRsvTyp17            57     58
     d  scRsvTyp18            59     60
     d  scRsvTyp19            61     62
     d  scRsvTyp20            63     64
     d  scRsvTyp21            65     66
     d  scRsvTyp26            67     68
     d  scRsvTyp27            69     70
     d  scRsvTyp32            71     72
     d  scRsvTyp33            73     74

     d                 ds
     d  dbRsvTyp               1     74
     d                                     Dim(intArrSiz2)
     d  RWICt01                1      2
     d  RWICt02                3      4
     d  RWICt03                5      6
     d  RWICt04                7      8
     d  RWICt05                9     10
     d  RWICt07               11     12
     d  RWICt08               13     14
     d  RWICt09               15     16
     d  RWICt10               17     18
     d  RWICt11               19     20
     d  RWMCt07               21     22
     d  RWMCt08               23     24
     d  RWMCt09               25     26
     d  RWMCt10               27     28
     d  RWECt03               29     30
     d  RWECt04               31     32
     d  RWECt05               33     34
     d  RWECt06               35     36
     d  RWLCt03               37     38
     d  RWLCt04               39     40
     d  RWLCt05               41     42
     d  RWLCt06               43     44
     d  RWICt06               45     46
     d  RWICt12               47     48
     d  RWICt13               49     50
     d  RWICt14               51     52
     d  RWICt15               53     54
     d  RWMCt01               55     56
     d  RWMCt02               57     58
     d  RWMCt03               59     60
     d  RWMCt04               61     62
     d  RWMCt05               63     64
     d  RWMCt06               65     66
     d  RWECt01               67     68
     d  RWECt02               69     70
     d  RWLCt01               71     72
     d  RWLCt02               73     74

     d  scRsvCodeDs    ds            20
     d  scReserve1             1      2
     d  scReserve2             3      4
     d  scReserve3             5      6
     d  scReserve4             7      8
     d  scReserve5             9     10
     d  scReserve6            11     12
     d  scReserve7            13     14
     d  scReserve8            15     16
     d  scReserve9            17     18
     d  scReserv10            19     20
     d  scRsvCode                     2    Overlay(scRsvCodeDs) Dim(10)

     d                 ds
     d  scRsvChg               1     90  2
     d                                     Dim(10)
     d  scReservC1             1      9  2
     d  scReservC2            10     18  2
     d  scReservC3            19     27  2
     d  scReservC4            28     36  2
     d  scReservC5            37     45  2
     d  scReservC6            46     54  2
     d  scReservC7            55     63  2
     d  scReservC8            64     72  2
     d  scReservC9            73     81  2
     d  scReserC10            82     90  2

     d  scRsvChgDs     ds
     d  scReservAmt1           1      9  2
     d  scReservAmt2          10     18  2
     d  scReservAmt3          19     27  2
     d  scReservAmt4          28     36  2
     d  scReservAmt5          37     45  2
     d  scReservAmt6          46     54  2
     d  scReservAmt7          55     63  2
     d  scReservAmt8          64     72  2
     d  scReservAmt9          73     81  2
     d  scReservAmt0          82     90  2
     d  scRsvAmt                      9s 2 Overlay(scRsvChgDs) Dim(10)

     d  rsvHstCdDs     ds
     d  RHrt01
     d  RHrt02
     d  RHrt03
     d  RHrt04
     d  RHrt05
     d  RHrt06
     d  RHrt07
     d  RHrt08
     d  RHrt09
     d  RHrt10
     d  rsvHstCd                      2    Overlay(rsvHstCdDs) Dim(10)

     d  rsvHstChgDs    ds
     d  RHsu01
     d  RHsu02
     d  RHsu03
     d  RHsu04
     d  RHsu05
     d  RHsu06
     d  RHsu07
     d  RHsu08
     d  RHsu09
     d  RHsu10
     d  rsvHstChg                     9  2 Overlay(rsvHstChgDs) Dim(10)

     d                 ds
     d dsErr                   1     37
     d                                     dim(intArrSiz2)
     d  erIndEr1               1      1n
     d  erIndEr2               2      2n
     d  erIndEr3               3      3n
     d  erIndEr4               4      4n
     d  erIndEr5               5      5n
     d  erIndEr6               6      6n
     d  erIndEr7               7      7n
     d  erIndEr8               8      8n
     d  erIndEr9               9      9n
     d  erIndEr10             10     10n
     d  erMedEr1              11     11n
     d  erMedEr2              12     12n
     d  erMedEr3              13     13n
     d  erMedEr4              14     14n
     d  erExpEr1              15     15n
     d  erExpEr2              16     16n
     d  erExpEr3              17     17n
     d  erExpEr4              18     18n
     d  erLglEr1              19     19n
     d  erLglEr2              20     20n
     d  erLglEr3              21     21n
     d  erLglEr4              22     22n
     d  erIndEr11             23     23n
     d  erIndEr12             24     24n
     d  erIndEr13             25     25n
     d  erIndEr14             26     26n
     d  erIndEr15             27     27n
     d  erMedEr5              28     28n
     d  erMedEr6              29     29n
     d  erMedEr7              30     30n
     d  erMedEr8              31     31n
     d  erMedEr9              32     32n
     d  erMedEr10             33     33n
     d  erExpEr5              34     34n
     d  erExpEr6              35     35n
     d  erLglEr5              36     36n
     d  erLglEr6              37     37n

     d                 ds
     d msErr                   1    370
     d                                     dim(intArrSiz2)
     d  msRsvErD1              1     10
     d  msRsvErD2             11     20
     d  msRsvErD3             21     30
     d  msRsvErD4             31     40
     d  msRsvErD5             41     50
     d  msRsvErD6             51     60
     d  msRsvErD7             61     70
     d  msRsvErD8             71     80
     d  msRsvErD9             81     90
     d  msRsvErD10            91    100
     d  msRsvErD22           101    110
     d  msRsvErD23           111    120
     d  msRsvErD24           121    130
     d  msRsvErD25           131    140
     d  msRsvErD28           141    150
     d  msRsvErD29           151    160
     d  msRsvErD30           161    170
     d  msRsvErD31           171    180
     d  msRsvErD34           181    190
     d  msRsvErD35           191    200
     d  msRsvErD36           201    210
     d  msRsvErD37           211    220
     d  msRsvErD11           221    230
     d  msRsvErD12           231    240
     d  msRsvErD13           241    250
     d  msRsvErD14           251    260
     d  msRsvErD15           261    270
     d  msRsvErD16           271    280
     d  msRsvErD17           281    290
     d  msRsvErD18           291    300
     d  msRsvErD19           301    310
     d  msRsvErD20           311    320
     d  msRsvErD21           321    330
     d  msRsvErD26           331    340
     d  msRsvErD27           341    350
     d  msRsvErD33           351    360
     d  msRsvErD32           361    370

     d                 ds
     d  dsRsvTp                1     10
     d                                     Dim(10)
     d  scRsvTp1               1      1
     d  scRsvTp2               2      2
     d  scRsvTp3               3      3
     d  scRsvTp4               4      4
     d  scRsvTp5               5      5
     d  scRsvTp6               6      6
     d  scRsvTp7               7      7
     d  scRsvTp8               8      8
     d  scRsvTp9               9      9
     d  scRsvTp10             10     10

     d                 ds
     d  intDate                1      8  0
     d  strmm                  1      2
     d  strdd                  3      4
     d  stryy                  5      8
      *--------------------------------------------------------------------
      * Protype Declaration
      *--------------------------------------------------------------------
      * build PDF url
     d***  Begin Chg  ***  11/25/15  **************************************
     d*BuildPDFUrl     pr           720
     d#BuildPDFUrl     pr           720
     d***  End   Chg  ***  11/25/15  **************************************
     d inDocId                       12
     d inFold                        80

      * Clear Subfiles
     d ClrSubFiles     pr

     d* Calculate Remaining Reserve Limit for User
     D ClcRemRsvLim    pr                  ExtPgm('SRRSVL')
     d  intCo#                             Like(CMco#)
     d  inFnd                              Like(CMfnd)
     d  inFyr                              Like(CMfyr)
     d  inCase                             Like(CMcase)
     d  inRsvCd                            Like(CMrt01)
     d  intLmt                             like(intLmt)

      * Get Next available document/folder id
     d GetNxtDoc       pr                  extpgm('SNXTDCID')
     d ##folder                      80
     d ##DocId                       12
     d ##Pre                          2
     d ##co                           3
     d ##fnd                          3
     d ##rtcd                         1

      * Get Next available document/folder id
     d GenrtPDF        pr                  extpgm('CRTRSLV')
     d ##folder                      80
     d ##DocId                       12
     d ##splf                        10
     d ##rtcd                         1

      * Get Remaining Reserves by Reserve Type
     d GetRemaining    pr            11  2
     d reserveType                    1    Const

      * Get Paid by Reserve Type
     d GetPaid         pr            11  2
     d reserveType                    1    Const

      * Get Reserve type count
     d GetRsvCnt       pr             3  0
     d reserveType                    1    Const

      * Get Reserve Desc
     d GetRsvDesc      pr            15
     d strRsvCd                      10    Const

      * Load Reserve Worksheet History
     d LoadWrkHst      pr

      * Load Detail
     d LodDetails      pr

      * Main Processing
     d Main            pr

     d* Move database record to screen
     d MvDbSc          pr

      * Process Approval
     d ProcAprv        pr

      * Process Darts
     d ProcDARTS       pr

      * Process Add/Edit Worksheet History
     d ProcAddEdt      pr

      * Process other options
     d ProcOther       pr

     d ResvWkSht       pr                  extpgm('WTCLMRWK')
     d  @@rrn                         9
     d  @@mode                        1
     d  @@next                       10
     d  @@TabInd                      5    Options(*nopass)

     d ResvWkSht       pi
     d  @@rrn                         9
     d  @@mode                        1
     d  @@next                       10
     d  @@TabInd                      5    Options(*nopass)

      * Get Root
     d RtvRoot         pr                  extpgm('RTVDOCROOT')
     d ##root                        10

     d SaveWkHst       pr

      * Validate Employer
     d Validate        pr

      * Write Darts
     d WriteDarts      pr                  extpgm('DA700R')
     d ##key                         40
     d ##comm                       256
     d ##cDat                         8
     d ##dcId                        12
     d ##pNam                        10
     d ##secl                         3
     d ##user                        10
     d ##pgm                         10
     d ##fold                        80
     d ##diFl                         1
     d ##iPgm                        10
     d ##fl01                         1
     d ##fl02                         1
     d ##rmCd                         5
     d ##seq#                         5
     d ##sbyl                         1
     d ##com2                        19
     d ##apId                         2
     d ##iDoc                        12
     d ##iQue                        10
     d ##imgF                         1
     d ##iPth                        10
     d ##pIdx                         5
     d ##qPty                         1
     d ##dcTp                         5
     d ##csId                         2
     d ##code                         2
     d*** Begin Add *** 12/09/15 *********************************************
     D*
     D* Scripture Processing Program
     D StLetter        pr                  ExtPgm('STLETTER')
     D   outCall_                    11
     D   InXML_                      60
     D   InReturn_                  250
     D   OutMode_                     1
     D   OutRrn_                      9
     D   OutPrefix_                   2
     D   OutString_                  10
     D   OutCo#_                      3
     D   OutFnd_                      3
     D   OutMassFlag_                 1
     D   OutXmlLink_                 60
     d*** End   Add *** 12/09/15 *********************************************

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrString
     d/copy sPrGenErr
     d/copy wPrGroup
     d/copy wPrEmpLr
     d/copy wPrUsrPrms
     d/copy wPrClaims
     d/copy wPrAgent
     d/copy wPrLoc
     d/copy wPrContact
     d/copy wPrClmPop
     d/copy wPrClmErr
     d*** Begin Add *** 12/09/15 *********************************************
     D/copy sPrXML
     d*** End   Add *** 12/09/15 *********************************************

     c     keyWMEML      Klist
     c                   Kfld                    CMco#
     c                   Kfld                    CMfnd
     c                   Kfld                    CMemp#
     c                   Kfld                    CMdiv

      /free
       //*** Begin Add *** 12/09/15 *********************************************
       // Determine if this module get spool file and/or XML output
       procType = 'Reserve Worksheet Report';
       isUseSpoolOut = UseSpoolOut(procType);
       isUseXML = UseScriptura(procType) Or
                  Use3rdXml(procType);
       if isUseXML;
         isScriptUsed = 'Y';
       else;
         isScriptUsed = 'N';
       Endif;
       //*** End   Add *** 12/09/15 ******************************************************

       // Main program processing
       If %parms >= 1;
          intRrn# = %int(@@rrn);
       Else;
          intRrn# = 3386;
       Endif;

       // Program Mode.
       If %parms >= 2;
          inMode = @@mode;
       Endif;

       clear rsvPaidDS;
       clear rsvRemainDS;

       // Chain to Employer Detail for Program.
       Chain(n) intRrn# pcm0p;
       If %found(wmcmp);
          intCo#  = CMco#;
          intFnd  = CMfnd;
          intCase = CMcase;
          intFyr  = CMfyr;
       Else;
          Return;
       Endif;
      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrn = intRrn#;
      //**** End Add  ***  06/23/21  ****************************************

       Select;
       When inMode = 'D';
          dspMode = *on;
          dspMode1= *on;
       When inMode = 'C';
          dspMode = *off;
          dspMode1= *off;
       Endsl;

       // Get the client id
       GetClientID(outClientID);

       scActMode  = inMode;
       scActCltId = outClientID;
       scActRrn   = intRrn#;

       TabIndex = 0;
       APnlHeight = 1100;
       isLoad     = *off;

       Main();

       *InLr = *On;

      /end-free
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi
      /free

       rrnWkHst = 0;
       clrWkHst = *On;
       dspWkHst = *Off;
       Write rcdWrkHst;

      /end-free
     p ClrSubfiles     e
       //===================================================================
       // Worksheet History
       //===================================================================
     p LoadWrkHst      B
     d LoadWrkHst      PI
      /free

       // clear the subfile.
       clrWkHst = *Off;
       dspWkHst = *On;
       clear scAddTlTp;

       Select;
       When inMode = 'D';
          dspMode = *on;
       When inMode = 'C';
          dspMode = *off;
       Endsl;


       // Read the file WDRWP

       Setll (intCo#:intFnd:intFyr:intCase) prw00;
       Reade (intCo#:intFnd:intFyr:intCase) prw00;
       Dow Not %eof(wdrwl);
          clear SflWkHst;

          scRsEntDt  = CvtToDate(RWEnDt);
          scRsEntBy  = GetUsrName(RWEnBy);
          scRsAprBy  = GetUsrName(RWApBy);
          scRsAprDt  = CvtToDate(RWApDt);

          scRsTtlInc = RWttic;

          dspApprove = *on;
          dspPending = *on;
          If inMode = 'C';
             If RWApDt > 0 ;
                dspApprove = *off;

                strKey# = RWdkey;
                strSeq# = %Editc(RWdseq#:'X');
       //***  Begin Chg  ***  11/25/15  ***********************************
       //*******scVwURL = BuildPDFUrl(RWDcId:RWFold);
                scVwUrl = BuildPDFUrl(RWdcId:RWfold:'Claim Reserve Worksheet':
                 RWdKey:RWdSeq#);
       //***  End   Chg  ***  11/25/15  ***********************************
             Else;
                dspPending = *off;
             Endif;
          Endif;
          scWkStRrn = dbRrn;

          rrnWkHst = rrnWkHst + 1;

          If RWstat = 'A';
             scRsSts  = 'Approved';
          Else;
             scRsSts  = 'Pending';
             If rrnWkHst = 1;
                dspMode  = *on;
                If CMstat <> 'C';
                   scAddTlTp = 'A new worksheet cannot be added while a +
                    pending worksheet exists.';
                Else;
                   scAddTlTp = 'Reserve changes are not allowed on a closed +
                    claim.';
                Endif;
             Endif;
          Endif;

          clear scRsvRow;
          If rrnWkHst = 1;
             scRsvRow = 'A';
          Endif;

          Write SflWkHst;

          Reade (intCo#:intFnd:intFyr:intCase) prw00;

       EndDo;

       rrnWkHst = 1;

       If scAddTlTp = ' ' and CMstat = 'C';
          dspMode  = *on;
          scAddTlTp = 'Reserve changes are not allowed on a closed claim.';
       Endif;

      /end-free
     p LoadWrkHst      e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /free


       // Load action panel
       LoadActPan(isMenuAvbl);

       // Process until exit button is pressed.
       Dou ExitNow = *on;

          Chain(n) intRrn# pcm0p;
          // Clear Sub-files
          ClrSubfiles();

          // Load Reserve Worksheet Histroy
          LoadWrkHst();

          // Process policy summary screen.
          Exsr ExfmtScreens;

          Select;
       //*** begin chg ***  01/07/16  ****************************************
       //*****   When btnDocWndw = *on;
       //*****      btnDocWndw = *off;
       //*****      DocWindow(scActRRN:'CS');
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'CS':txtWinData);
             btnShowWin = *off;
       //*** end chg *****  01/07/16  ****************************************

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *Off;

          When btnRefresh = *On;
             btnRefresh =*Off;

          When isAdd = *On;
             isAdd = *Off;
      //**** Begin Add  ***  06/23/21  ****************************************
             OutCmRrnTm = OutCmRrn;
      //**** End Add  ***  06/23/21  ****************************************
             clear RcdWkHsEdt;
      //**** Begin Add  ***  06/23/21  ****************************************
             OutCmRrn = OutCmRrnTm;
      //**** End Add  ***  06/23/21  ****************************************
             BtnCancel  = *off;
             BtnSave    = *off;
             BtnSavAprv = *off;
             isPrcMode  = 'A';
             isLoad     = *on;
             ProcAddEdt();

          // Menu Choices
          When scMenuRsp <> *blanks;
             @@next = scMenuRsp;
             Leave;

          Other;
             ProcOther();
          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 2150;
          APnlHeight = TabHeight + 50;
          Write rcdClmPnl;
          Write rcdTabPan;
          Exfmt rcdWrkHst;
          Read rcdClmPnl;
          Read rcdTabPan;
       EndSl;

       Endsr;

      /end-free
     p Main            e
      *=====================================================================
      * ProcAddEdt - Process Add/Edit worksheet history
      *=====================================================================
     p ProcAddEdt      b
     d ProcAddEdt      pi

      /free

       isNotValid = *off;
       isDiscard  = *off;
       isSkip     = *off;
       clear scHeadings;
       clear scWks;
       clear scCmpRt;
       clear scAmt;
       clear Hdg;
       clear Wks;
       clear CmpRt;
       clear Amt;
       clear prw0p;

       // processing for Edit mode
       If isPrcMode = 'C';
          Chain(n) scWkStRrn prw0p;
          MvDbSc();
       Else;
          // load from WMCMP file based on reserve code
          scIndCrOst = GetRemaining('C');
          scIndPdTDt = GetPaid('C');
          scMedCurOt = GetRemaining('M');
          scMedPdTDt = GetPaid('M');
          scExpCurOs = GetRemaining('E');
          scExpPdTDt = GetPaid('E');
          scLglCurOs = GetRemaining('L');
          scLglPdTDt = GetPaid('L');
       Endif;

       Chain (intCo#:CMJrSt) pcw01;
       If not %found(WDCWL) or CWstat = 'I';
          Chain (CMco#:' ') pcw01;
          If not %found(WDCWL) or CWstat = 'I';
             PopOneBtn('State definition does not exist in Claims +
              Configuration Reserve Worksheet set up.':'Error');
             Return;
          Endif;
       Endif;

       LodDetails();
       //*** begin add ***  10/09/15  ****************************************
       If CWbf1Edit = 'Y';
          scDsbIn01 = *Off;
       Else;
          scDsbIn01 = *On;
       EndIf;

       If CWbf2Edit = 'Y';
          scDsbIn02 = *Off;
       Else;
          scDsbIn02 = *On;
       EndIf;

       If CWbf3Edit = 'Y';
          scDsbIn03 = *Off;
       Else;
          scDsbIn03 = *On;
       EndIf;

       If CWbf4Edit = 'Y';
          scDsbIn04 = *Off;
       Else;
          scDsbIn04 = *On;
       EndIf;

       If CWbf5Edit = 'Y';
          scDsbIn05 = *Off;
       Else;
          scDsbIn05 = *On;
       EndIf;

       If CWbfpEdit = 'Y';
          scDsbIn06 = *Off;
       Else;
          scDsbIn06 = *On;
       EndIf;

       //*** End   add ***  10/09/15  ****************************************

       Dou BtnCancel = *on or ((BtnSave = *on or BtnSavAprv = *on) and
        isNotValid = *off);
       //*** Beg Add *** 04/18/16 ******************************************
          Scfil ='WTRTP, WMFNP';
          Scfld = 'TRIM(RTDESC)';
          Scfldv ='RTKEY';
          ScselcrtI=' RTtype =''C'' and RTstat=''A'' ' +
                    ' and FNfnd=' + %EditC(intFnd:'X')  +
                    ' and (RTkey = FNrs01  or RTkey = FNrs02 or RTkey' +
                    ' = FNrs03 or RTkey = FNrs04 or RTkey = FNrs05' +
                    ' or RTkey = FNrs06  or RTkey = FNrs07  or' +
                    ' RTkey = FNrs08 or RTkey = FNrs09 ' +
                    ' or RTkey = FNrs10)';
          ScselcrtM=' RTtype =''M'' and RTstat=''A'' ' +
                    ' and FNfnd=' + %EditC(intFnd:'X')  +
                    ' and (RTkey = FNrs01  or RTkey = FNrs02 or RTkey' +
                    ' = FNrs03 or RTkey = FNrs04 or RTkey = FNrs05' +
                    ' or RTkey = FNrs06  or RTkey = FNrs07  or' +
                    ' RTkey = FNrs08 or RTkey = FNrs09 ' +
                    ' or RTkey = FNrs10)';
          ScselcrtE=' RTtype =''E'' and RTstat=''A'' ' +
                    ' and FNfnd=' + %EditC(intFnd:'X')  +
                    ' and (RTkey = FNrs01  or RTkey = FNrs02 or RTkey' +
                    ' = FNrs03 or RTkey = FNrs04 or RTkey = FNrs05' +
                    ' or RTkey = FNrs06  or RTkey = FNrs07  or' +
                    ' RTkey = FNrs08 or RTkey = FNrs09 ' +
                    ' or RTkey = FNrs10)';
          ScselcrtL=' RTtype =''L'' and RTstat=''A'' ' +
                    ' and FNfnd=' + %EditC(intFnd:'X')  +
                    ' and (RTkey = FNrs01  or RTkey = FNrs02 or RTkey' +
                    ' = FNrs03 or RTkey = FNrs04 or RTkey = FNrs05' +
                    ' or RTkey = FNrs06  or RTkey = FNrs07  or' +
                    ' RTkey = FNrs08 or RTkey = FNrs09 ' +
                    ' or RTkey = FNrs10)';
       //*** End Add *** 04/18/16 ******************************************
          Exfmt RcdWkHsEdt;
          isNotValid = *off;
          erIndMnAdj = *off;
          erIndPerWk = *off;
          erRtCmt    = *off;
          erIndErr   = *off;
          erMedErr   = *off;
          erLglErr   = *off;
          erExpErr   = *off;
          errMedRc   = *off;
          errIndRc   = *off;
          clear dsErr;
          clear msErr;

          Select;
          When BtnSave = *on or BtnSavAprv = *on;
             Exsr VldWkHst;
             If isNotValid = *off;
                SaveWkHst();

                // Process approval
                If BtnSavAprv = *on;
                   Validate();
                   If isSkip = *on;
                      Leave;
                   Endif;
                   If isNotValid = *off;
                      ProcAprv();
                   Endif;
                Endif;
             Endif;

          When BtnCancel = *on;
             BtnCancel = *off;
             If isDiscard = *on;
                strMsg = 'Do you want to discard the saved changes?';
                If PopTwoBtn(strmsg:'Discard':'Discard':'Keep');
                   delete scWkStRrn prw0p;
                Endif;
             Endif;
             Leave;
          Endsl;
       Enddo;
       unlock WDRWP;

       //===================================================================
       // VldWkHst
       //===================================================================
       Begsr VldWkHst;

       If scIndPerWk > 100 or scIndPerWk < 0;
          isNotValid = *on;
          msIndPerWk='% Week value cannot be greater than a 100 or less than 0';
          erIndPerWk=*on;
       Endif;

       If scRtCmt = ' ';
          isNotValid = *on;
          msRtCmt = 'WCR0503';
          erRtCmt = *on;
       Endif;

       If scTtlRecmd = 0 and scIndMnAdj = 0 and scIndRcChg = 0 and
        scMedRcChg = 0 and scExpRcChg = 0 and scLglRcChg = 0;
          isNotValid = *on;
          PopOneBtn('Entries are required to Save the Worksheet.':'Error');
          LeaveSr;
       Endif;

       If (scIndAmt1 > 0 or scIndAmt2 > 0 or scIndAmt3 > 0 or scIndAmt4 > 0 or
        scIndAmt5 > 0 or scIndAmt6 > 0 or scIndDthAt > 0 or scIndFlxA1 > 0 or
        scIndFlxA2 > 0 or scIndFlxA3 > 0 or scIndFlxA4 > 0 or scIndFunBn > 0 or
        scIndDsFgm > 0 or scIndStlmt > 0) and scIndMnAdj > 0;
          isNotValid = *on;
          PopOneBtn('Manual indemnity change and other Indemnity fields are +
           mutually exclusive.':'Error');
          erIndMnAdj = *on;
       Endif;

       // perform edit for reserve changes
       Exsr GetDetails;
       EdtRsvChgs(intRrn#:scRsvCode:scRsvAmt:arrErrInd);

       Clear groupRsv;
       Chain (CMco#:CMfnd) pfn00;

       For intCount = 1 to intArrSiz2;
          If intCount <= intArrSiz;
             intAmt = scAmt(intCount);
          Else;
             intAmt = scAmtAll(intCount-intArrSiz);
          Endif;

          If intAmt <> 0 and dsRsvTyp(intCount) = *blank;
             msErr(intCount) = 'WCR0503';
             dsErr(intCount) = *on;
             isNotValid = *on;
          Else;
             intCnt1 = %lookup(dsRsvTyp(intCount):scRsvCode);
             If intCnt1 > 0 and arrErrInd(4 + intCnt1) = '1' and
              intAmt <> 0;
                msErr(intCount) = 'WCR0552';
                dsErr(intCount) = *on;
                isNotValid = *on;
             Endif;
          Endif;
          Exsr chkGrpVld;
       Endfor;

       // check for valid reserv changes
       Exsr ChkVldRes;

       Endsr;
       //===================================================================
       // chkGrpVld
       //===================================================================
       BegSr chkGrpVld;

       If dsErr(intCount)<>*on and %LookUp(dsRsvTyp(intCount):groupRsv) = 0 and
        dsRsvTyp(intCount) <> *blank and intAmt <> 0;
          msErr(intCount) = 'WC30126';
          dsErr(intCount) = *on;
          isNotValid = *on;
       Endif;

       Endsr;
       //===============================================================
        // ChkVldRes - Check if reserve changes are valid
       //===============================================================
       Begsr ChkVldRes;

       If arrErrInd(1) = '1';
          msIndRc = 'Comp reserves are entered on a non-Indemnity claim.';
          errIndRc = *on;
          isNotValid = *on;
       Endif;

       If arrErrInd(2) = '1';
          msMedRc = 'Med reserves entered on a Policy flagged to exclude +
           Medical.';
          errMedRc = *on;
          isNotValid = *on;
       Endif;

       If arrErrInd(4) = '1';
          PopOneBtn('ELOUTS will be increased beyond 9999999.99':'Error');
          isNotValid = *on;
       Endif;

       If arrErrInd(16) = '1';
          PopOneBtn('Reserve change is negative and the remaining reserve will +
           not cover existing check requests.':'Error');
          isNotValid = *on;
       Endif;

       Endsr;
       //===================================================================
       // GetDetails
       //===================================================================
       BegSr GetDetails;

       clear scRsvCode;
       clear scRsvChg;
       clear scRsvAmt;
       intCnt = 1;
       isError  = *off;

       For intCount = 1 to intArrSiz2;
          If dsRsvTyp(intCount) <> ' ';
             intIndex = %LookUp(dsRsvTyp(intCount):scRsvCode);
             If intIndex = 0;
                scRsvCode(intCnt) = dsRsvTyp(intCount);
                If intCount <= intArrSiz;
                   scRsvChg(intCnt)  = scAmt(intCount);
                Else;
                   scRsvChg(intCnt)  = scAmtAll(intCount-intArrSiz);
                Endif;
                intCnt1 = %LookUp(scRsvCode(intCnt):RT);
                scRsvAmt(intCnt) = scRsvChg(intCnt);

                If intCnt1 > 0;
                   scRsvAmt(intCnt) =rsvRemainDs(intCnt1) + rsvPaidDs(intCnt1) +
                    scRsvAmt(intCnt);
                Endif;

                intCnt = intCnt + 1;
             Else;
                If intCount <= intArrSiz;
                   scRsvChg(intIndex) = scRsvChg(intIndex) + scAmt(intCount);
                   scRsvAmt(intIndex) = scRsvAmt(intIndex) + scAmt(intCount);
                Else;
                   scRsvChg(intIndex) = scRsvChg(intIndex) +
                    scAmtAll(intCount-intArrSiz);
                   scRsvAmt(intIndex) = scRsvAmt(intIndex) +
                    scAmtAll(intCount-intArrSiz);
                Endif;
             Endif;
          Endif;
       Endfor;

       Endsr;
      /end-free
     p ProcAddEdt      e
      *=====================================================================
      * ProcOther - Process Other Options
      *=====================================================================
     p ProcOther       b
     d ProcOther       pi
      /free

       ReadC SflWkHst;

       Dow not %eof;
          Select;
          // process for Editing
          When isEdit = *on;
             isEdit = *off;
      //**** Begin Add  ***  06/23/21  ****************************************
             OutCmRrnTm = OutCmRrn;
      //**** End Add  ***  06/23/21  ****************************************
             clear RcdWkHsEdt;
      //**** Begin Add  ***  06/23/21  ****************************************
             OutCmRrn = OutCmRrnTm;
      //**** End Add  ***  06/23/21  ****************************************
             BtnCancel  = *off;
             BtnSave    = *off;
             BtnSavAprv = *off;
             isPrcMode  = 'C';
             isLoad     = *on;
             ProcAddEdt();

          // process for deleting
          When isDelete = *on;
             Chain scWkStRrn prw0p;
             isDelete = *off;
             strMsg = 'Do you want to continue with the deletion?';
             If PopTwoBtn(strmsg:'Confirm':'Continue':'Cancel');
                delete prw0p;
             Endif;
             unlock WDRWP;

          // process for Audit Company Assignment History
          When isApprove = *on;
             isApprove = *off;
             Validate();
             If isNotValid = *off;
                If isLoad = *off;
                   clear scHeadings;
                   clear scWks;
                   clear scCmpRt;
                   clear scAmt;
                   clear Hdg;
                   clear Wks;
                   clear CmpRt;
                   clear Amt;
                   clear prw0p;
                   isLoad = *on;
                   Chain(n) scWkStRrn prw0p;
                   MvDbSc();

                   Chain (intCo#:CMJrSt) pcw01;
                   If not %found(WDCWL) or CWstat = 'I';
                      Chain (CMco#:' ') pcw01;
                   Endif;

                   LodDetails();
                Endif;
                ProcAprv();
             Endif;
          Endsl;
          ReadC SflWkHst;
       Enddo;

      /end-free
     p ProcOther       e
      *=====================================================================
      * ProcAprv - Process Approval
      *=====================================================================
     p ProcAprv        b
     d ProcAprv        pi

     d strDciD         s             12
     d strFold         s             80

      /free

       open wtrptwso;

       Exsr CreatePDF;

       // Update worksheet history detail file
       Exsr UpdWrkHst;

       // Update Claim reserve details
       Exsr UpdClmRsv;

       //*** Beg Add *** 12/09/15 *****************************************
       //-------------------------------------------------------------
       // WrtSXRWPAMT - Write to Scriptura Reserve Worksheet - Amount
       //--------------------------------------------------------------
           Begsr WrtSXRWPAMT;

              Clear prwa0p;

              RWAxml     = RWRxml;
              RWAsecHdr  = RWSsecHdr;
              RWAamtHdr  = dAmtHdg;
              RWAamtDtl  = dAmtDtl;
              RWArsvCd   = inCode;
              RWArsvCdDs = dRcde;
              RWAiDt     = RWRiDt;

              Write prwa0p;

           Endsr;
       //-------------------------------------------------------------
       // WrtSXRWPBEN - Write to Scriptura Reserve Worksheet - Benefit
       //--------------------------------------------------------------
           Begsr WrtSXRWPBEN;

              Clear prwb0p;

              RWBxml     = RWRxml;
              RWBsecHdr  = RWSsecHdr;
              RWBbenHdr  = dBenHdg;
              RWBbenPer  = dBenPer;
              RWBbenTo   = dBenTo;
              RWBweek    = dWeek;
              RWBcmpRt   = dCmpRt;
              RWBcmpAmt  = dAmt;
              RWBiDt     = RWSiDt;
              RWBrsvCd   = inCode;
              RWBrsvCdDs = dRcode;

              Write prwb0p;

           Endsr;
       //-------------------------------------------------------------
       // WrtSXRWPRPT - Write to Scriptura Reserve Worksheet - Report
       //--------------------------------------------------------------
           Begsr WrtSXRWPRPT;

           Chain inXML prwr00;
           If %Found(sxrwlrpt);
              RWRtotChg= dTotChg;
              RWRtotInc= dTotInc;
              RWRapBy  = RWapBy;
              RWRapByN = GetUsrName(RWApBy);
              RWRapDt  = dtMDYY(RWApDt);
              Update prwr00;
           Else;
              Clear prwr00;

              RWRxml   = inXML;
              RWRco#   = CMco#;

              Chain CMco# pco00;
              RWRcoNm  = %Trim(COname);

              RWRfnd   = CMfnd;

              Chain (CMco#:CMfnd) pfn00;
              RWRfNam  = %Trim(FNname);

              RWRfyr   = CMfyr;
              RWRcase  = CMcase;

              If CMmid <> ' ';
                 RWRcNam  = %Trim(CMfsn) + ' ' + %Trim(CMmid) +
                  '. ' + %Trim(CMlsn);
              Else;
                 RWRcNam  = %Trim(CMfsn) + ' ' + %Trim(CMlsn);
              EndIf;

              RWRacDt  = dtMDYY(CMacDt);

              strKey = CMJrSt;
              Chain strKey pst01;
              If %found(STSTL1);
                 RWRjrSt = %trim(STname);
              Endif;

              RWRenBy  = RWenBy;
              RWRenByN = GetUsrName(RWenBy);
              RWRenDt  = dtMDYY(RWenDt);
              RWRenTm  = RWenTm;
              RWRstat  = RWstat;

              RWRnlUs  = RWnlUs;
              RWRnlUsN = GetUsrName(RWnlUs);
              RWRtinc  = RWTTic;
              RWRtotChg= 0;
              RWRtotInc= 0;
              RWRnotes = RWcomm;
              RWRiDt = %Dec(%Date():*cymd);

              Write prwr00;
           EndIf;

           Endsr;
       //-------------------------------------------------------------
       // WrtSXRWPRSV - Write to Scriptura Reserve Worksheet - Reserves
       //--------------------------------------------------------------
           Begsr WrtSXRWPRSV;

              Clear prwv0p;

              RWVxml    = RWRxml;
              RWVsecHdr = RWSsecHdr;
              RWVrsvHdr = dRsvHdg;
              RWVrsvAmt = dRsvAmt;
              RWViDt    = RWRiDt;

              Write prwv0p;

           Endsr;
       //-------------------------------------------------------------
       // WrtSXRWPSEC - Write to Scriptura Reserve Worksheet - Section
       //--------------------------------------------------------------
           Begsr WrtSXRWPSEC;

              Clear prws0p;

              RWSxml    = RWRxml;
              RWSsecHdr = dSecHdr;
              RWSdtlHdr = dSubRsv;
              RWSidt    = RWRidt;

              Write prws0p;

           Endsr;
       //*** End Add *** 12/09/15 *****************************************
       //===================================================================
       // UpdWrkHst - Update worksheet history detail file
       //===================================================================
       Begsr UpdWrkHst;

       Exsr processDarts;

       Chain scWkStRrn prw0p;

       RWapby = Q1user;
       RWapdt = Dateto7(SystemDate());
       RWstat = 'A';
       RWdKey = ##key;
       RWdseq#= %int(##seq#);
       RWfold = %trim(strFold);
       RWdcid = %trim(strDcId);
       RWuser = q1user;
       RWpgm  = q1pgm;
       RWdt   = Dateto7(SystemDate());
       RWtime = Timeto6(SystemTime());
       Update prw0p;
       //*** Begin Add *** 12/09/15 *****************************************
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
         Exsr wrtSXRWPRPT;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************

       Unlock WDRWP;

       Endsr;
       //===================================================================
       // UpdClmRsv - Update Claim reserves
       //===================================================================
       Begsr UpdClmRsv;

       Chain intRrn# pcm0p;

       For intCnt = 1 to 10;
          If scRsvCode(intCnt) <> *blanks and scRsvChg(intCnt) <> 0;
             intIndex = %LookUp(scRsvCode(intCnt):RT);
             If intIndex = 0;
                intIndex = %LookUp(*Blank:RT);
                If intIndex <> 0;
                   RT(intIndex) = scRsvCode(intCnt);
                   rsvRemainDS(intIndex) = rsvRemainDS(intIndex) +
                    + scRsvChg(intCnt);
                Endif;
             Else;
                rsvRemainDS(intIndex) = rsvRemainDS(intIndex) +
                 + scRsvChg(intCnt);
             Endif;
          Endif;
       Endfor;

       CMuser = q1User;
       CMpgm  = q1Pgm;
       CMtime = TimeTo6(SystemTime());
       CMdt   = DateTo7(SystemDate());
       Update pcm0p;

       Unlock WMCMP;

       clear prh02;
       RHco#  = CMco#;
       RHfnd  = CMfnd;
       RHfyr  = CMfyr;
       RHcase = CMcase;
       RHefDt = DateTo7(SystemDate());
       strDate = %editc(DateTo8(SystemDate()):'X');
       RHexpl = 'Reserve Worksheet approved by ' + %trim(GetUsrName(q1User)) +
        ' on ' + %subst(strDate:1:2) + '/' + %subst(strDate:3:2) + '/' +
        %subst(strDate:5:4) + '.';
       RHuser = q1User;
       RHiusr = q1User;
       RHpgm  = q1Pgm;
       RHiPgm = q1Pgm;
       RHtime = TimeTo6(SystemTime());
       RHiTim = TimeTo6(SystemTime());
       RHdt   = DateTo7(SystemDate());
       RHiDt  = DateTo7(SystemDate());

       For intCnt = 1 to 10;
          If scRsvChg(intCnt) <> 0;
             intIndex = %LookUp(scRsvCode(intCnt):RT);
             If intIndex <> 0;
                rsvHstCd(intIndex)  = scRsvCode(intCnt);
                rsvHstChg(intIndex) = scRsvChg(intCnt);
             Endif;
          Endif;
       Endfor;
       //***  Begin Add  ***  08/15/17  ************************
       RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
       //***  End   Add  ***  08/15/17  *************************

       Write prh02;

       // Update Employer Detail
       Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel00;
       If %found(WDELL);
          ELoutS = ELoutS + %Xfoot(scRsvChg);
          Update pel00;
       EndIf;

       Endsr;
       //===================================================================
       // CreatePDF
       //===================================================================
       Begsr CreatePDF;

       Exsr ProcPrint;

       Exsr ProcPDF;

       Endsr;
       //===================================================================
       // Process PDF
       //===================================================================
       Begsr ProcPDF;

       ##pre   = 'RW';
       ##co    = %Editc(CMco#:'X');
       ##fnd   = %Editc(CMfnd:'X');
       ##rtcd  = 'P';
       strFold = ' ';
       strDcId = ' ';

       GetNxtDoc(strFold:strDcId:##pre:##co:##fnd:##rtcd);

       ##splf = 'WTRPTWSO';
       GenrtPDF(strFold:strDcId:##splf:##rtcd);

       Endsr;
       //===================================================================
       // ChkOvrFlw
       //===================================================================
       Begsr ChkOvrFlw;

       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          If intLine >= intLine1;
             *in99 = *on;
             Write DPGBRK;
             *in99 = *off;
             intLine = 3;
          Endif;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** End   Add *** 12/09/15 *****************************************

       Endsr;
       //*** Beg Add *** 12/09/15 *****************************************
       //-------------------------------------------------------------
       // GetXMLFileNm - Get the XML File Name
       //--------------------------------------------------------------
           Begsr GetXMLFileNm;
            outCall     = 'XMLREQUEST';
            OutPrefix   = '  ';
            OutString   = 'RsvWrkSht';
            InReturn    = procType;
            OutMassFlag = 'N';

            StLetter(OutCall:InXML:InReturn:outMode:OutRrn:
             OutPrefix:OutString:OutCo#:OutFnd:OutMassFlag:OutXmlLink);

            Chain inXML pxp01;
            If %Found(xmlprel1);
               inXML = XPxmlT;
            EndIf;

           Endsr;
       //*** End Add *** 12/09/15 *****************************************
       //===================================================================
       // Scan for Last space
       //===================================================================
       Begsr scanSpace;
         strblanks = ' ' ;
         clear intpos;
         Dou intpos = 0;
             intpos = %scan(strblanks:strstring:intpos+1);
             If intpos > intlen;
                Leave;
             Endif;
             If intpos <> 0;
                retVal = intpos;
             Endif;
         Enddo;

       Endsr;
       //===================================================================
       // Process Spool file
       //===================================================================
       Begsr ProcPrint;

       isFirst = *off;
       intLine = 0;
       DNtCmt = ' ';

       // Load Header
       DClmNam = %trim(CMfsn) + ' ' + %trim(CMlsn);
       DClmNu  = %Editc(CMfnd:'X') + '-' + %Editc(CMfyr:'X') + '-' +
        %Editc(CMcase:'X');
       DAprBy  = GetUsrName(q1User);
       intDate = dtMDYY(CMAcDt);
       DDtInj  = strmm + '/' + strdd + '/' + stryy;
       intDate = dtMDYY(Dateto7(SystemDate()));
       DAprDt  = strmm + '/' + strdd + '/' + stryy;

       DJrsSt = ' ';
       strKey = CMJrSt;
       Chain strKey pst01;
       If %found(STSTL1);
          DJrsSt = %trim(STname);
       Endif;

       // DTtInc = %xFoot(rsvPaidDS) + %xFoot(rsvRemainDS);
       DTtInc = RWTTic;

       // Write Header
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write HdrWrk;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** End   Add *** 12/09/15 *****************************************
       //*** Begin Add *** 12/09/15 *****************************************
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
         ExSr GetXmlFileNm;
         Exsr wrtSXRWPRPT;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************

       // Write Line
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
       Write RLine;
       intLine = intLine + 12;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** End   Add *** 12/09/15 *****************************************

       // Write Section header for Indemnity
       DSecHdr = 'Indemnity';
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write HSecHdr;
          intLine = intLine + 2;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** End   Add *** 12/09/15 *****************************************

       // Write Flexible Benefit details
       DBenPer = ' ';
       DBenTit = ' ';
       DBenTo  = ' ';
       DBnPr   = ' ';
       DSubRsv = ' ';
       DAmsRsv = ' ';
       DRCode  = ' ';

       intCount1 = GetRsvCnt('C');
       If intCount1 > 1;
          DSubRsv = 'Reserve Code';
          DAmsRsv = 'Reserve Code';
       Endif;
       //*** Begin Add *** 12/09/15 *****************************************
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
         Exsr wrtSXRWPSEC;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************

       For intCount = 1 to intArrSiz1;
          If Hdg(intCount) <> *blanks and Amt(intCount) <> 0;
             If isFirst = *off;
                //*** Begin Add *** 12/09/15 *****************************************
                If isUseSpoolout;
                //*** End   Add *** 12/09/15 *****************************************
                   Write DtlSub;
                   intLine = intLine + 1;
                //*** Begin Add *** 12/09/15 *****************************************
                   DSubRsv = ' ';
                EndIf;
                //*** End   Add *** 12/09/15 *****************************************
                isFirst = *on;
             Endif;
             DBenHdg = Hdg(intCount);
             DWeek   = Wks(intCount);
             DCmpRt  = CmpRt(intCount);
             DAmt    = Amt(intCount);
             If intCount1 > 1;
                DRCode  = GetRsvDesc(dbRsvTyp(intCount));
                //*** Begin Add *** 12/09/15 *************************
                inCode = dbRsvTyp(intCount);
                //*** End   Add *** 12/09/15 *************************
             Endif;
             If intCount = intArrSiz1;
                DBenPer = %char(RWBpRc);
                DBenTit = 'to';
                DBenTo  = %trim(RWBpTo);
                DBnPr   = '%';
             Endif;
             //*** Begin Add *** 12/09/15 *****************************************
             If isUseSpoolout;
             //*** End   Add *** 12/09/15 *****************************************
                Write DtlBen;
                intLine = intLine + 1;
             //*** Begin Add *** 12/09/15 *****************************************
             EndIf;
             //*** Write to Scriptura Reserve Worksheet Workfile
             If isScriptUsed = 'Y';
                Exsr wrtSXRWPBEN;
             Endif;
             //*** End   Add *** 12/09/15 *****************************************

             If intCount = intArrSiz1;
                //*** Begin Add *** 12/09/15 *****************************************
                If isUseSpoolout;
                //*** End   Add *** 12/09/15 *****************************************
                   Write DtlPer;
                   intLine = intLine + 1;
                //*** Begin Add *** 12/09/15 *****************************************
                EndIf;
                //*** End   Add *** 12/09/15 *****************************************
             Endif;
          Endif;
       Endfor;

       DBenPer = ' ';
       DBenTit = ' ';
       DBenTo  = ' ';
       DBnPr   = ' ';
       DRCde   = ' ';

       If isFirst = *off;
          //*** Begin Add *** 12/09/15 *****************************************
          If isUseSpoolout;
          //*** End   Add *** 12/09/15 *****************************************
             Write DtlAmS;
             intLine = intLine + 1;
          //*** Begin Add *** 12/09/15 *****************************************
          EndIf;
          //*** End   Add *** 12/09/15 *****************************************
          isFirst = *on;
       Endif;

       // Write Death Benefit details
       If RWDbAm <> 0;
          DBenHdg = 'Death Benefits';
          DWeek   = RWDbWk;
          DCmpRt  = RWDbCr;
          DAmt    = RWDbAm;
          If intCount1 > 1;
             DRCode  = GetRsvDesc(RWICt06);
             //*** Begin Add *** 12/09/15 *************************
             inCode = RWICt06;
             //*** End   Add *** 12/09/15 *************************
          Endif;
          //*** Begin Add *** 12/09/15 *****************************************
          If isUseSpoolout;
          //*** End   Add *** 12/09/15 *****************************************
             Write DtlBen;
             intLine = intLine + 1;
          //*** Begin Add *** 12/09/15 *****************************************
          EndIf;
          //*** Write to Scriptura Reserve Worksheet Workfile
          If isScriptUsed = 'Y';
             Exsr wrtSXRWPBEN;
          Endif;
          //*** End   Add *** 12/09/15 *****************************************
       Endif;

       // Funeral
       If RWFbAm <> 0;
          DAmtHdg = 'Funeral Benefit';
          DAmtDtl = RWFbAm;
          If intCount1 > 1;
             DRCde = GetRsvDesc(RWICt12);
             //*** Begin Add *** 12/09/15 *************************
             inCode = RWICt12;
             //*** End   Add *** 12/09/15 *************************
          Endif;
          //*** Begin Add *** 12/09/15 *****************************************
          If isUseSpoolout;
          //*** End   Add *** 12/09/15 *****************************************
             Write DtlAmt;
             intLine = intLine + 1;
          //*** Begin Add *** 12/09/15 *****************************************
          EndIf;
          //*** Write to Scriptura Reserve Worksheet Workfile
          If isScriptUsed = 'Y';
             Exsr wrtSXRWPAMT;
          Endif;
          //*** End   Add *** 12/09/15 *****************************************
       Endif;

       // Disfigurement
       If RWDsAm <> 0;
          DAmtHdg = 'Disfigurement';
          DAmtDtl = RWDsAm;
          If intCount1 > 1;
             DRCde = GetRsvDesc(RWICt13);
             //*** Begin Add *** 12/09/15 *************************
             inCode = RWICt13;
             //*** End   Add *** 12/09/15 *************************
          Endif;
          //*** Begin Add *** 12/09/15 *****************************************
          If isUseSpoolout;
          //*** End   Add *** 12/09/15 *****************************************
             Write DtlAmt;
             intLine = intLine + 1;
          //*** Begin Add *** 12/09/15 *****************************************
          EndIf;
          //*** Write to Scriptura Reserve Worksheet Workfile
          If isScriptUsed = 'Y';
             Exsr wrtSXRWPAMT;
          Endif;
          //*** End   Add *** 12/09/15 *****************************************
       Endif;

       // Settlement
       If RWStAm <> 0;
          DAmtHdg = 'Settlement';
          DAmtDtl = RWStAm;
          If intCount1 > 1;
             DRCde = GetRsvDesc(RWICt14);
             //*** Begin Add *** 12/09/15 *************************
             inCode = RWICt14;
             //*** End   Add *** 12/09/15 *************************
          Endif;
          //*** Begin Add *** 12/09/15 *****************************************
          If isUseSpoolout;
          //*** End   Add *** 12/09/15 *****************************************
             Write DtlAmt;
             intLine = intLine + 1;
          //*** Begin Add *** 12/09/15 *****************************************
          EndIf;
          //*** Write to Scriptura Reserve Worksheet Workfile
          If isScriptUsed = 'Y';
             Exsr wrtSXRWPAMT;
          Endif;
          //*** End   Add *** 12/09/15 *****************************************
       Endif;

       // Indemnity Flexible Headings
       For intCount = (intArrSiz1 + 1) to (intArrSiz1 + 4);
          If Hdg(intCount) <> *blanks and Amt(intCount) <> 0;
             DAmtHdg = Hdg(intCount);
             DAmtDtl = Amt(intCount);
             If intCount1 > 1;
                DRCde = GetRsvDesc(dbRsvTyp(intCount));
                //*** Begin Add *** 12/09/15 *************************
                inCode = dbRsvTyp(intCount);
                //*** End   Add *** 12/09/15 *************************
             Endif;
             //*** Begin Add *** 12/09/15 *****************************************
             If isUseSpoolout;
             //*** End   Add *** 12/09/15 *****************************************
                Write DtlAmt;
                intLine = intLine + 1;
             //*** Begin Add *** 12/09/15 *****************************************
             EndIf;
             //*** Write to Scriptura Reserve Worksheet Workfile
             If isScriptUsed = 'Y';
                Exsr wrtSXRWPAMT;
             Endif;
             //*** End   Add *** 12/09/15 *****************************************
          Endif;
       Endfor;

       // Manual Adjustment
       If RWiMAj <> 0;
          DAmtHdg = 'Manual Adjustment';
          DAmtDtl = RWiMAj;
          If intCount1 > 1;
             DRCde = GetRsvDesc(RWICt15);
             //*** Begin Add *** 12/09/15 *************************
             inCode = RWICt15;
             //*** End   Add *** 12/09/15 *************************
          Endif;
          //*** Begin Add *** 12/09/15 *****************************************
          If isUseSpoolout;
          //*** End   Add *** 12/09/15 *****************************************
             Write DtlAmt;
             intLine = intLine + 1;
          //*** Begin Add *** 12/09/15 *****************************************
          EndIf;
          //*** Write to Scriptura Reserve Worksheet Workfile
          If isScriptUsed = 'Y';
             Exsr wrtSXRWPAMT;
          Endif;
          //*** End   Add *** 12/09/15 *****************************************
       Endif;

       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write Dnote;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** End   Add *** 12/09/15 *****************************************

       // Current Outstanding/Remaining Indemnity reserve
       DRsvHdg = 'Current Outstanding/Remaining Indemnity reserve';
       DRsvAmt = RWCoir;
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlRsv;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
          Exsr wrtSXRWPRSV;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************

       // Paid to Date
       DRsvHdg = 'Paid to Date';
       DRsvAmt = RWiPtD;
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlRsv;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
          Exsr wrtSXRWPRSV;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************

       // Recommended change
       DRsvHdg = 'Recommended Change';
       DRsvAmt = RWiRCh;
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlRsv;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
          Exsr wrtSXRWPRSV;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************

       // Total Indemnity including change
       DRsvHdg = 'Total Indemnity Including Change';
       DRsvAmt = RWTiic;
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlRsv;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
          Exsr wrtSXRWPRSV;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************

       // Write Line
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write RLine;
          intLine = intLine + 2;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** End   Add *** 12/09/15 *****************************************

       // Write Section header for Medical
       DSecHdr = 'Medical';
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write HSecHdr;
          intLine = intLine + 2;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
         Exsr wrtSXRWPSEC;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************

       DAmsRsv = ' ';
       DRCde   = ' ';
       intCount1 = GetRsvCnt('M');
       isMedExcl = RtvMedExcl(CMco#:CMfnd:CMemp#:CMdiv:CMfYr);
       If intCount1 > 1 and isMedExcl <> 'Y';
          DAmsRsv = 'Reserve Code';
       Endif;

       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlAmS;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** End   Add *** 12/09/15 *****************************************

       // Physician
       If RWPhAm <> 0;
          DAmtHdg = 'Physician';
          DAmtDtl = RWPhAm;
          If intCount1 > 1 and isMedExcl <> 'Y';
             DRCde = GetRsvDesc(RWMCt01);
             //*** Begin Add *** 12/09/15 *************************
             inCode = RWMCt01;
             //*** End   Add *** 12/09/15 *************************
          Endif;
          //*** Begin Add *** 12/09/15 *****************************************
          If isUseSpoolout;
          //*** End   Add *** 12/09/15 *****************************************
             Write DtlAmt;
             intLine = intLine + 1;
          //*** Begin Add *** 12/09/15 *****************************************
          EndIf;
          //*** Write to Scriptura Reserve Worksheet Workfile
          If isScriptUsed = 'Y';
             Exsr wrtSXRWPAMT;
          Endif;
          //*** End   Add *** 12/09/15 *****************************************
       Endif;

       // Hospital
       If RWHoAm <> 0;
          DAmtHdg = 'Hospital';
          DAmtDtl = RWHoAm;
          If intCount1 > 1 and isMedExcl <> 'Y';
             DRCde = GetRsvDesc(RWMCt02);
             //*** Begin Add *** 12/09/15 *************************
             inCode = RWMCt02;
             //*** End   Add *** 12/09/15 *************************
          Endif;
          //*** Begin Add *** 12/09/15 *****************************************
          If isUseSpoolout;
          //*** End   Add *** 12/09/15 *****************************************
             Write DtlAmt;
             intLine = intLine + 1;
          //*** Begin Add *** 12/09/15 *****************************************
          EndIf;
          //*** Write to Scriptura Reserve Worksheet Workfile
          If isScriptUsed = 'Y';
             Exsr wrtSXRWPAMT;
          Endif;
          //*** End   Add *** 12/09/15 *****************************************
       Endif;

       // Surgery
       If RWSuAm <> 0;
          DAmtHdg = 'Surgery';
          DAmtDtl = RWSuAm;
          If intCount1 > 1 and isMedExcl <> 'Y';
             DRCde = GetRsvDesc(RWMCt03);
             //*** Begin Add *** 12/09/15 *************************
             inCode = RWMCt03;
             //*** End   Add *** 12/09/15 *************************
          Endif;
          //*** Begin Add *** 12/09/15 *****************************************
          If isUseSpoolout;
          //*** End   Add *** 12/09/15 *****************************************
             Write DtlAmt;
             intLine = intLine + 1;
          //*** Begin Add *** 12/09/15 *****************************************
          EndIf;
          //*** Write to Scriptura Reserve Worksheet Workfile
          If isScriptUsed = 'Y';
             Exsr wrtSXRWPAMT;
          Endif;
          //*** End   Add *** 12/09/15 *****************************************
       Endif;

       // RX/Med supplies
       If RWRxAm <> 0;
          DAmtHdg = 'RX/Med supplies';
          DAmtDtl = RWRxAm;
          If intCount1 > 1 and isMedExcl <> 'Y';
             DRCde = GetRsvDesc(RWMCt04);
             //*** Begin Add *** 12/09/15 *************************
             inCode = RWMCt04;
             //*** End   Add *** 12/09/15 *************************
          Endif;
          //*** Begin Add *** 12/09/15 *****************************************
          If isUseSpoolout;
          //*** End   Add *** 12/09/15 *****************************************
             Write DtlAmt;
             intLine = intLine + 1;
          //*** Begin Add *** 12/09/15 *****************************************
          EndIf;
          //*** Write to Scriptura Reserve Worksheet Workfile
          If isScriptUsed = 'Y';
             Exsr wrtSXRWPAMT;
          Endif;
          //*** End   Add *** 12/09/15 *****************************************
       Endif;

       // Diagnostics
       If RWDiAm <> 0;
          DAmtHdg = 'Diagnostics';
          DAmtDtl = RWDiAm;
          If intCount1 > 1 and isMedExcl <> 'Y';
             DRCde = GetRsvDesc(RWMCt05);
             //*** Begin Add *** 12/09/15 *************************
             inCode = RWMCt05;
             //*** End   Add *** 12/09/15 *************************
          Endif;
          //*** Begin Add *** 12/09/15 *****************************************
          If isUseSpoolout;
          //*** End   Add *** 12/09/15 *****************************************
             Write DtlAmt;
             intLine = intLine + 1;
          //*** Begin Add *** 12/09/15 *****************************************
          EndIf;
          //*** Write to Scriptura Reserve Worksheet Workfile
          If isScriptUsed = 'Y';
             Exsr wrtSXRWPAMT;
          Endif;
          //*** End   Add *** 12/09/15 *****************************************
       Endif;

       // Physical Therapy
       If RWPtAm <> 0;
          DAmtHdg = 'Physical Therapy';
          DAmtDtl = RWPtAm;
          If intCount1 > 1 and isMedExcl <> 'Y';
             DRCde = GetRsvDesc(RWMCt06);
             //*** Begin Add *** 12/09/15 *************************
             inCode = RWMCt06;
             //*** End   Add *** 12/09/15 *************************
          Endif;
          //*** Begin Add *** 12/09/15 *****************************************
          If isUseSpoolout;
          //*** End   Add *** 12/09/15 *****************************************
             Write DtlAmt;
             intLine = intLine + 1;
          //*** Begin Add *** 12/09/15 *****************************************
          EndIf;
          //*** Write to Scriptura Reserve Worksheet Workfile
          If isScriptUsed = 'Y';
             Exsr wrtSXRWPAMT;
          Endif;
          //*** End   Add *** 12/09/15 *****************************************
       Endif;

       // Medical Flexible Headings
       For intCount = (intArrSiz1 + 5) to (intArrSiz1 + 8);
          If Hdg(intCount) <> *blanks and Amt(intCount) <> 0;
             DAmtHdg = Hdg(intCount);
             DAmtDtl = Amt(intCount);
             If intCount1 > 1 and isMedExcl <> 'Y';
                DRCde = GetRsvDesc(dbRsvTyp(intCount));
                //*** Begin Add *** 12/09/15 *************************
                inCode = dbRsvTyp(intCount);
                //*** End   Add *** 12/09/15 *************************
             Endif;
             //*** Begin Add *** 12/09/15 *****************************************
             If isUseSpoolout;
             //*** End   Add *** 12/09/15 *****************************************
                Write DtlAmt;
                intLine = intLine + 1;
             //*** Begin Add *** 12/09/15 *****************************************
             EndIf;
             //*** Write to Scriptura Reserve Worksheet Workfile
             If isScriptUsed = 'Y';
                Exsr wrtSXRWPAMT;
             Endif;
             //*** End   Add *** 12/09/15 *****************************************
          Endif;
       Endfor;

       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write Dnote;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** End   Add *** 12/09/15 *****************************************

       // Current Outstanding/Remaining Medical Reserve
       DRsvHdg = 'Current Outstanding/Remaining Medical Reserve';
       DRsvAmt = RWComr;
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlRsv;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
          Exsr wrtSXRWPRSV;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************

       // Paid to Date
       DRsvHdg = 'Paid to Date';
       DRsvAmt = RWmPtD;
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlRsv;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
          Exsr wrtSXRWPRSV;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************

       // Recommended change
       DRsvHdg = 'Recommended Change';
       DRsvAmt = RWmRCh;
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlRsv;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
          Exsr wrtSXRWPRSV;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************

       // Total Medical including change
       DRsvHdg = 'Total Medical Including Change';
       DRsvAmt = RWTMic;
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlRsv;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
          Exsr wrtSXRWPRSV;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************

       // Write Line
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write RLine;
          intLine = intLine + 2;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** End   Add *** 12/09/15 *****************************************

       // Write Section header for Expense
       DSecHdr = 'Expense';
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write HSecHdr;
          intLine = intLine + 2;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
         Exsr wrtSXRWPSEC;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************

       DAmsRsv = ' ';
       DRCde   = ' ';
       intCount1 = GetRsvCnt('E');
       If intCount1 > 1;
          DAmsRsv = 'Reserve Code';
       Endif;

       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlAmS;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** End   Add *** 12/09/15 *****************************************

       // Surveillance
       If RWSvAm <> 0;
          DAmtHdg = 'Surveillance';
          DAmtDtl = RWSvAm;
          If intCount1 > 1;
             DRCde = GetRsvDesc(RWECt01);
             //*** Begin Add *** 12/09/15 *************************
             inCode = RWECt01;
             //*** End   Add *** 12/09/15 *************************
          Endif;
          //*** Begin Add *** 12/09/15 *****************************************
          If isUseSpoolout;
          //*** End   Add *** 12/09/15 *****************************************
             Write DtlAmt;
             intLine = intLine + 1;
          //*** Begin Add *** 12/09/15 *****************************************
          EndIf;
          //*** Write to Scriptura Reserve Worksheet Workfile
          If isScriptUsed = 'Y';
             Exsr wrtSXRWPAMT;
          Endif;
          //*** End   Add *** 12/09/15 *****************************************
       Endif;

       // Medical Records
       If RWMrAm <> 0;
          DAmtHdg = 'Medical Records';
          DAmtDtl = RWMrAm;
          If intCount1 > 1;
             DRCde = GetRsvDesc(RWECt02);
             //*** Begin Add *** 12/09/15 *************************
             inCode = RWECt02;
             //*** End   Add *** 12/09/15 *************************
          Endif;
          //*** Begin Add *** 12/09/15 *****************************************
          If isUseSpoolout;
          //*** End   Add *** 12/09/15 *****************************************
             Write DtlAmt;
             intLine = intLine + 1;
          //*** Begin Add *** 12/09/15 *****************************************
          EndIf;
          //*** Write to Scriptura Reserve Worksheet Workfile
          If isScriptUsed = 'Y';
             Exsr wrtSXRWPAMT;
          Endif;
          //*** End   Add *** 12/09/15 *****************************************
       Endif;

       intLine1 = 64;
       // Expense Flexible Headings
       For intCount = (intArrSiz1 + 9) to (intArrSiz1 + 12);
          If Hdg(intCount) <> *blanks and Amt(intCount) <> 0;
             DAmtHdg = Hdg(intCount);
             DAmtDtl = Amt(intCount);
             If intCount1 > 1;
                DRCde = GetRsvDesc(dbRsvTyp(intCount));
                //*** Begin Add *** 12/09/15 *************************
                inCode = dbRsvTyp(intCount);
                //*** End   Add *** 12/09/15 *************************
             Endif;
             //*** Begin Add *** 12/09/15 *****************************************
             If isUseSpoolout;
             //*** End   Add *** 12/09/15 *****************************************
                Write DtlAmt;
                intLine = intLine + 1;
             //*** Begin Add *** 12/09/15 *****************************************
             EndIf;
             //*** Write to Scriptura Reserve Worksheet Workfile
             If isScriptUsed = 'Y';
                Exsr wrtSXRWPAMT;
             Endif;
             //*** End   Add *** 12/09/15 *****************************************
             Exsr chkOvrFlw;
          Endif;
       Endfor;

       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write Dnote;
          intLine = intLine + 1;
          Exsr chkOvrFlw;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** End   Add *** 12/09/15 *****************************************

       // Current Outstanding/Remaining Expense Reserve
       DRsvHdg = 'Current Outstanding/Remaining Expense Reserve';
       DRsvAmt = RWCoer;
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlRsv;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
          Exsr wrtSXRWPRSV;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************
       Exsr chkOvrFlw;

       // Paid to Date
       DRsvHdg = 'Paid to Date';
       DRsvAmt = RWePtD;
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlRsv;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
          Exsr wrtSXRWPRSV;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************
       Exsr chkOvrFlw;

       // Recommended change
       DRsvHdg = 'Recommended Change';
       DRsvAmt = RWeRCh;
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlRsv;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
          Exsr wrtSXRWPRSV;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************
       Exsr chkOvrFlw;

       // Total Expense including change
       DRsvHdg = 'Total Expense Including Change';
       DRsvAmt = RWTEic;
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlRsv;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
          Exsr wrtSXRWPRSV;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************
       intLine1 = 63;
       Exsr chkOvrFlw;

       // Write Line
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          If intLine > 2;
             Write RLine;
             intLine = intLine + 2;
             intLine1 = 62;
             Exsr chkOvrFlw;
          Endif;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** End   Add *** 12/09/15 *****************************************

       // Write Section header for Legal
       intLine1 = 64;
       DSecHdr = 'Legal';
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write HSecHdr;
          intLine = intLine + 2;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
         Exsr wrtSXRWPSEC;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************
       Exsr chkOvrFlw;

       DAmsRsv = ' ';
       DRCde   = ' ';
       intCount1 = GetRsvCnt('L');
       If intCount1 > 1;
          DAmsRsv = 'Reserve Code';
       Endif;

       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlAmS;
          intLine = intLine + 1;
          Exsr chkOvrFlw;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** End   Add *** 12/09/15 *****************************************

       // Defense Attorney
       If RWDfAm <> 0;
          DAmtHdg = 'Defense Attorney';
          DAmtDtl = RWDfAm;
          If intCount1 > 1;
             DRCde = GetRsvDesc(RWLCt02);
             //*** Begin Add *** 12/09/15 *************************
             inCode = RWLCt02;
             //*** End   Add *** 12/09/15 *************************
          Endif;
          //*** Begin Add *** 12/09/15 *****************************************
          If isUseSpoolout;
          //*** End   Add *** 12/09/15 *****************************************
             Write DtlAmt;
             intLine = intLine + 1;
          //*** Begin Add *** 12/09/15 *****************************************
          EndIf;
          //*** Write to Scriptura Reserve Worksheet Workfile
          If isScriptUsed = 'Y';
             Exsr wrtSXRWPAMT;
          Endif;
          //*** End   Add *** 12/09/15 *****************************************
          Exsr chkOvrFlw;
       Endif;

       // Depositions
       If RWDpAm <> 0;
          DAmtHdg = 'Depositions';
          DAmtDtl = RWDpAm;
          If intCount1 > 1;
             DRCde = GetRsvDesc(RWLCt01);
             //*** Begin Add *** 12/09/15 *************************
             inCode = RWLCt01;
             //*** End   Add *** 12/09/15 *************************
          Endif;
          //*** Begin Add *** 12/09/15 *****************************************
          If isUseSpoolout;
          //*** End   Add *** 12/09/15 *****************************************
             Write DtlAmt;
             intLine = intLine + 1;
          //*** Begin Add *** 12/09/15 *****************************************
          EndIf;
          //*** Write to Scriptura Reserve Worksheet Workfile
          If isScriptUsed = 'Y';
             Exsr wrtSXRWPAMT;
          Endif;
          //*** End   Add *** 12/09/15 *****************************************
          Exsr chkOvrFlw;
       Endif;

       // Legal Flexible Headings
       For intCount = (intArrSiz1 + 13) to (intArrSiz1 + 16);
          If Hdg(intCount) <> *blanks and Amt(intCount) <> 0;
             DAmtHdg = Hdg(intCount);
             DAmtDtl = Amt(intCount);
             If intCount1 > 1;
                DRCde = GetRsvDesc(dbRsvTyp(intCount));
                //*** Begin Add *** 12/09/15 *************************
                inCode = dbRsvTyp(intCount);
                //*** End   Add *** 12/09/15 *************************
             Endif;
             //*** Begin Add *** 12/09/15 *****************************************
             If isUseSpoolout;
             //*** End   Add *** 12/09/15 *****************************************
                Write DtlAmt;
                intLine = intLine + 1;
             //*** Begin Add *** 12/09/15 *****************************************
             EndIf;
             //*** Write to Scriptura Reserve Worksheet Workfile
             If isScriptUsed = 'Y';
                Exsr wrtSXRWPAMT;
             Endif;
             //*** End   Add *** 12/09/15 *****************************************
             Exsr chkOvrFlw;
          Endif;
       Endfor;

       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write Dnote;
          intLine = intLine + 1;
          Exsr chkOvrFlw;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** End   Add *** 12/09/15 *****************************************

       // Current Outstanding/Remaining Legal Reserve
       DRsvHdg = 'Current Outstanding/Remaining Legal Reserve';
       DRsvAmt = RWColr;
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlRsv;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
          Exsr wrtSXRWPRSV;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************
       Exsr chkOvrFlw;

       // Paid to Date
       DRsvHdg = 'Paid to Date';
       DRsvAmt = RWlPtD;
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlRsv;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
          Exsr wrtSXRWPRSV;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************
       Exsr chkOvrFlw;

       // Recommended change
       DRsvHdg = 'Recommended Change';
       DRsvAmt = RWlRCh;
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlRsv;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
          Exsr wrtSXRWPRSV;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************
       Exsr chkOvrFlw;

       // Total Legal including change
       DRsvHdg = 'Total Legal Including Change';
       DRsvAmt = RWTLic;
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlRsv;
          intLine = intLine + 1;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
          Exsr wrtSXRWPRSV;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************
       intLine1 = 63;
       Exsr chkOvrFlw;

       // Write Line
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          If intLine > 2;
             Write RLine;
             intLine = intLine + 1;
             intLine1 = 61;
             Exsr chkOvrFlw;
          Endif;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** End   Add *** 12/09/15 *****************************************

       // Write Total Line
       DTotChg = RWtrCh;
       DTotInc = RWTTic;
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write DtlTot;
          intLine = intLine + 4;
          intLine1 = 62;
          Exsr chkOvrFlw;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** End   Add *** 12/09/15 *****************************************

       // Write Section header for Rationale & Comments
       DSecHdr = 'Rationale & Comments';
       //*** Begin Add *** 12/09/15 *****************************************
       If isUseSpoolout;
       //*** End   Add *** 12/09/15 *****************************************
          Write HSecHdr;
          intLine = intLine + 2;
       //*** Begin Add *** 12/09/15 *****************************************
       EndIf;
       //*** Write to Scriptura Reserve Worksheet Workfile
       If isScriptUsed = 'Y';
         Exsr wrtSXRWPSEC;
       Endif;
       //*** End   Add *** 12/09/15 *****************************************
       intLine1 = 63;
       Exsr chkOvrFlw;

       //Print Reserve Rationale
       intlen = %len(DNtCmt);
       intPrvVal = 1;
       intRetVal = 1;

       strString1 = RWcomm;

       Dow strString1 <> ' ';
          strString = %subst(strString1:intPrvVal:intLen);
          Exsr scanSpace;

          If RetVal = intLen or %subst(strString1:intLen+1:1) = ' ';
             DNtCmt = %subst(strString1:intPrvVal:intLen);
             clear strString1;
             intRetVal = intRetVal + intLen;
          Else;
             DNtCmt = %subst(StrString1:intPrvVal:retVal);
             clear strString1;
             intRetVal = intRetVal + retVal;
          Endif;
          //*** Begin Chg *** 10/29/19 *************************************
          //*** strString1 = %subst(RWcomm:intRetVal);
          If intRetVal <= 5000;
             strString1 = %subst(RWcomm:intRetVal);
          Else;
             strString1 = ' ';
          EndIf;
          //*** End   Chg *** 10/29/19 *************************************
          //*** Begin Add *** 12/09/15 *****************************************
          If isUseSpoolout;
          //*** End   Add *** 12/09/15 *****************************************
             Write DNote;
             intLine = intLine + 1;
             Exsr chkOvrFlw;
          //*** Begin Add *** 12/09/15 *****************************************
          EndIf;
          //*** End   Add *** 12/09/15 *****************************************
       Enddo;

       close wtrptwso;

       Endsr;
       //===================================================================
       // Write DARTS Remark
       //===================================================================
       Begsr processDarts;

       // Write Diary
       ##comm = %trim(GetUsrName(Q1user)) + ' approved worksheet.';
       ##user = Q1user;
       ##rmcd = CWrmCd;
       ##dcid = strDcId;
       ##fold = strFold;
       ##imgF = 'O';

       ProcDARTS();

       Endsr;

      /end-free
     p ProcAprv        e
       //===================================================================
       // View/Print PDF
       //===================================================================
     d***  Begin Chg  ***  11/25/15  **************************************
     p*BuildPDFUrl     b
     d*BuildPDFUrl     pi           720
     p#BuildPDFUrl     b
     d#BuildPDFUrl     pi           720
     d***  End   Chg  ***  11/25/15  **************************************
     d inDocId                       12
     d inFold                        80

     d strIgFlr        s             50
     d strImgTitle     s             50
     d strImgLnk       s            720

      /free

       RtvRoot(##root);

       strIgFlr = '/' + %trim(##root) + '/' + %trim(inFold);
       strImgTitle = 'Claim Reserve Worksheet';

       strImgLnk = strImgTitle + '/iDartsWeb/remarks/attachments.do?method=get-
       AttachmentFile&key='+%trim(strKey#)+'&sequence='+%trim(strSeq#)+'&docId-
       ='+%trim(inDocId)+'&folder='+%trim(strIgFlr)+'^P&user='+%trim(RWapby);

       return strImgLnk;

      /end-free
     d***  Begin Chg  ***  11/25/15  **************************************
     p*BuildPDFUrl     e
     p#BuildPDFUrl     e
     d***  End   Chg  ***  11/25/15  **************************************
      *====================================================================
      * Validate
      *====================================================================
     p Validate        b
     d Validate        pi
     d isVldFlg        s               n

      /free

       isNotValid  = *off;
       isVldFlg    = *off;

       Chain(n) scWkStRrn prw0p;

       // check user reserve authority
       Exsr ChkAuthority;

       Chain intCo# pcw00;
       If %found(WMCWL) and CWdisA = 'Y';
          If RWiusr = q1User or RWiusr = ' ';
             isNotValid = *on;
          Endif;
       Endif;

       //*** beg chg ***** 06/22/16 *********
       //*** If RWnlus <> ' ' and RWnlus <> q1User;
       If RWnlus <> ' ' and RWnlus <> q1User and isNotValid = *on;
       //*** end chg ***** 06/22/16 *********
          PopOneBtn(GetUsrName(RWnlus) + ' is the next level user (if Reserve +
           Limit Authority allows), that can approve this worksheet.':
           'Information');
          isSkip = *on;
       Else;
          If isNotValid = *on;
             scNxtAprvl = ' ';
             Dou BtnCanDry = *on or (BtnSbm = *on and isVldFlg = *off);
                Exfmt RcdNxtAprv;
                isVldFlg = *off;
                erNxtAprvl = *off;
                erNxtAprv  = *off;

                Select;
                When BtnSbm = *on;
                   Exsr VldDiary;
                   If isVldFlg = *off;
                      Exsr ProcSave;
                      isSkip = *on;
                   Endif;

                When BtnCanDry = *on;
                   BtnCanDry = *off;
                   Leave;
                Endsl;
             Enddo;

          Endif;
       Endif;

       //===============================================================
        // ChkAuthority - Check authority to approve the reserve payment
        //             worksheet.
       //===============================================================
       Begsr ChkAuthority;

       // Do not allow the user to exceed their reserve authority limit.

       // For indemnity reserves
       isChkFlg = 'Y';
       If isNotValid = *off and (RWirch <> 0 or RWimaj <> 0);
          charRsvType = 'C';
          Exsr GetRsvCdLmt;
          If (intLmt < RWirch and RWirch<>0) or (intLmt < RWimaj and RWimaj<>0);
             isChkFlg = 'N';
             isNotValid = *on;
          Endif;
       Endif;

       // For Medical reserves
       If isNotValid = *off and RWmrch <> 0;
          charRsvType = 'M';
          Exsr GetRsvCdLmt;
          If intLmt < RWmrch;
             isChkFlg = 'N';
             isNotValid = *on;
          Endif;
       Endif;

       // For expense reserves
       If isNotValid = *off and RWerch <> 0;
          charRsvType = 'E';
          Exsr GetRsvCdLmt;
          If intLmt < RWerch;
             isChkFlg = 'N';
             isNotValid = *on;
          Endif;
       Endif;

       // For Legal reserves
       If isNotValid = *off and RWlrch <> 0;
          charRsvType = 'L';
          Exsr GetRsvCdLmt;
          If intLmt < RWlrch;
             isChkFlg = 'N';
             isNotValid = *on;
          Endif;
       Endif;

       // Total reserve change may not exceed the user's reserve authority
       // limit.
       intTotal = RWirch + RWmrch + RWerch + RWlrch;

       // if commulative authority limit is non-zero then check for total
       // reserve limit as well. intLmt will be same for all type codes.
       If isNotValid = *off and getRsvAutLim(q1User) > 0;
          charRsvType = 'M';
          Exsr GetRsvCdLmt;
          If intLmt < intTotal;
             isChkFlg = 'N';
             isNotValid = *on;
          Endif;
       Endif;

       Endsr;
       //===============================================================
        // GetRsvCdLmt
       //===============================================================
       Begsr GetRsvCdLmt;

       Setll intCo# prt00;
       Reade intCo# prt00;
       Dow not %eof;
          If RTstat = 'A' and RTtype = charRsvType;
             charRsvCode = %trim(RTkey);
             Leave;
          Endif;
          Reade intCo# prt00;
       Enddo;

       intLmt = 0;
       ClcRemRsvLim(intCo#:intFnd:intFyr:intCase:charRsvCode:intLmt);

       Endsr;
       //===============================================================
        // VldDiary
       //===============================================================
       Begsr VldDiary;

       erNxtAprvl = *off;
       erNxtAprv  = *off;

       If scNxtAprvl = ' ';
          msNxtAprvl = 'WCR0503';
          erNxtAprvl = *on;
          isVldFlg   = *on;
       Endif;

       If isVldFlg = *off and scNxtAprvl = q1User;
          msgNxtAprv = 'The user cannot be the current user.';
          erNxtAprv  = *on;
          isVldFlg   = *on;
       Endif;

       If isVldFlg = *off and scNxtAprvl = RWiusr;
          msgNxtAprv = 'The user cannot be the one who created the worksheet.';
          erNxtAprv  = *on;
          isVldFlg   = *on;
       Endif;

       Endsr;
       //===============================================================
        // ProcSave
       //===============================================================
       Begsr ProcSave;

       Chain scWkStRrn prw0p;

       RWnlus = scNxtAprvl;

       RWuser = q1user;
       RWpgm  = q1pgm;
       RWdt   = Dateto7(SystemDate());
       RWtime = Timeto6(SystemTime());
       RWapby = ' ';
       RWapdt = 0;
       RWdKey = ' ';
       RWdseq#= 0;
       RWfold = ' ';
       RWdcid = ' ';
       Update prw0p;

       Unlock WDRWP;

       Exsr ProcessDarts;

       Endsr;
       //===================================================================
       // Write DARTS Remark
       //===================================================================
       Begsr processDarts;

       // Write Diary
       ##comm = %trim(GetUsrName(q1User)) + ' submitted worksheet to ' +
        %trim(GetUsrName(RWnlus)) + ' for approval.';
       ##user = RWnlus;
       ##rmcd = CWracd;
       ##dcid = ' ';
       ##fold = ' ';
       ##imgF = ' ';

       ProcDARTS();

       Endsr;

      /end-free

     p Validate        e
       //===================================================================
       // MvDbSc - Move database record to screen
       //===================================================================
     p MvDbSc          b
     d MvDbSc          pi
     c
      /free

       clear scAmtAll;
       scIndPerWk = RWBPRC;
       scIndPerTo = RWBPTO;
       scIndDthWk = RWDBWK;
       scIndDthRt = RWDBCR;
       scIndDthAt = RWDBAM;
       scIndFunBn = RWFBAM;
       scIndDsFgm = RWDSAM;
       scIndStlmt = RWSTAM;
       scIndCrOst = RWCOIR;
       scIndPdTDt = RWIPTD;
       scIndRcChg = RWIRCH;
       scIndMnAdj = RWIMAJ;
       scIndTtInc = RWTIIC;
       scMedPhys  = RWPHAM;
       scMedHsptl = RWHOAM;
       scMedSurgy = RWSUAM;
       scMedRxSup = RWRXAM;
       scMedDgnst = RWDIAM;
       scMedPhyTh = RWPTAM;
       scMedCurOt = RWCOMR;
       scMedPdTDt = RWMPTD;
       scMedRcChg = RWMRCH;
       scMedTtInc = RWTMIC;
       scExpSrv   = RWSVAM;
       scExpMdRec = RWMRAM;
       scExpCurOs = RWCOER;
       scExpPdTDt = RWEPTD;
       scExpRcChg = RWERCH;
       scExpTtlEx = RWTEIC;
       scLglDefAt = RWDFAM;
       scLglDepst = RWDPAM;
       scLglCurOs = RWCOLR;
       scLglPdTDt = RWLPTD;
       scLglRcChg = RWLRCH;
       scLglTtInc = RWTLIC;
       scTtlRecmd = RWTRCH;
       scTtlIncr  = RWTTIC;
       scRtCmt    = RWCOMM;

      /end-free
     p MvDbSc          e
       //===================================================================
       // SaveWkHst - Save Worksheet History changes
       //===================================================================
     p SaveWkHst       b
     d SaveWkHst       pi
     c
      /free

       Chain scWkStRrn prw0p;
       If not %found;
          clear prw0p;
       Endif;
       Wksa     = scWksa;
       For intCount = 1 to intArrSiz;
          Amt(intCount)   = scAmt(intCount);
       Endfor;
       For intCount = 1 to intArrSiz1;
          CmpRt(intCount) = scCmpRt(intCount);
       Endfor;

       dbRsvTyp = dsRsvTyp;

       RWBPRC = scIndPerWk;
       RWBPTO = scIndPerTo;
       RWDBWK = scIndDthWk;
       RWDBCR = scIndDthRt;
       RWDBAM = scIndDthAt;
       RWFBAM = scIndFunBn;
       RWDSAM = scIndDsFgm;
       RWSTAM = scIndStlmt;
       RWCOIR = scIndCrOst;
       RWIPTD = scIndPdTDt;
       RWIRCH = scIndRcChg;
       RWIMAJ = scIndMnAdj;
       RWTIIC = scIndTtInc;
       RWPHAM = scMedPhys;
       RWHOAM = scMedHsptl;
       RWSUAM = scMedSurgy;
       RWRXAM = scMedRxSup;
       RWDIAM = scMedDgnst;
       RWPTAM = scMedPhyTh;
       RWCOMR = scMedCurOt;
       RWMPTD = scMedPdTDt;
       RWMRCH = scMedRcChg;
       RWTMIC = scMedTtInc;
       RWSVAM = scExpSrv;
       RWMRAM = scExpMdRec;
       RWCOER = scExpCurOs;
       RWEPTD = scExpPdTDt;
       RWERCH = scExpRcChg;
       RWTEIC = scExpTtlEx;
       RWDFAM = scLglDefAt;
       RWDPAM = scLglDepst;
       RWCOLR = scLglCurOs;
       RWLPTD = scLglPdTDt;
       RWLRCH = scLglRcChg;
       RWTLIC = scLglTtInc;
       RWTRCH = scTtlRecmd;
       RWTTIC = scTtlIncr;
       RWCOMM = scRtCmt;
       RWapby = ' ';
       RWapdt = 0;
       RWdKey = ' ';
       RWdseq#= 0;
       RWfold = ' ';
       RWdcid = ' ';

       If isPrcMode = 'C';
          RWuser = q1user;
          RWpgm  = q1pgm;
          RWdt   = Dateto7(SystemDate());
          RWtime = Timeto6(SystemTime());
          Update prw0p;
       Else;
          RWco#  = intCo#;
          RWfnd  = intFnd;
          RWfyr  = intFyr;
          RWcase = intCase;
          RWendt = Dateto7(SystemDate());
          RWentm = Timeto6(SystemTime());
          RWenby = q1user;
          RWstat = 'P';
          RWiusr = q1user;
          RWipgm = q1pgm;
          RWidt  = Dateto7(SystemDate());
          RWitim = Timeto6(SystemTime());
       //** Beg Add ***** 12/07/15 ************************************
          RWuser = ' ';
          RWnlus = ' ';
       //** End Add ***** 12/07/15 ************************************
          Write prw0p;
          scWkStRrn = dbRrn1;
          isPrcMode = 'C';
          isDiscard = *on;
       Endif;

      /end-free
     p SaveWkHst       e
       //===================================================================
       // ProcDARTS
       //===================================================================
     p ProcDARTS       b
     d ProcDARTS       pi
     c
      /free

       // Write Diary
       ##key = %EditC(CMco#:'X') + %EditC(CMfnd:'X') +
        %EditC(CMfYr:'X') + %EditC(CMcase:'X');
       ##cDat = %EditC(DateTo8(systemDate()):'X');
       ##pgm  = Q1pgm;
       ##iPgm = Q1pgm;
       ##secl = '000';
       ##diFl = ' ';
       ##fl01 = ' ';
       ##fl02 = ' ';
       ##seq# = '00000';
       ##com2 = ' ';
       ##apId = ' ';
       ##iDoc = ' ';
       ##iQue = ' ';
       ##iPth = ' ';
       ##pIdx = ' ';
       ##qPty = ' ';
       ##dcTp = ' ';
       ##csId = ' ';
       ##code = ' ';

       WriteDarts(##key:##comm:##cDat:##dcId:##pNam:##secl:##user:
        ##pgm:##fold:##diFl:##iPgm:##fl01:##fl02:##rmCd:##seq#:
        ##sbyl:##com2:##apId:##iDoc:##iQue:##imgF:##iPth:##pIdx:
        ##qPty:##dcTp:##csId:##code);

      /end-free
     p ProcDARTS       e
       //===================================================================
       // GetPaid - Get paid  by reserve type.
       //===================================================================
     p GetPaid         b
     d GetPaid         pi            11  2
     d reserveType                    1    const

     d paidAmt         s             11  2 inz(0)
     d i               s              5  0
      /free

       For i=1 to 10;
          strKey = RT(i);
          Chain (CMco#:strKey) prt00;
          If %Found(wtabl) and RTtype = reserveType;
             paidAmt += rsvPaidDs(i);
          EndIf;
       EndFor;

       Return paidAmt;

      /end-free
     p GetPaid         e
       //===================================================================
       // GetRemaining - Get the remaining reserves by reserve type.
       //===================================================================
     p GetRemaining    b
     d GetRemaining    pi            11  2
     d reserveType                    1    const

     d remainingAmt    s             11  2 inz(0)
     d i               s              5  0
      /free

       for i=1 to 10;
          strKey = RT(i);
          Chain (CMco#:strKey) prt00;
          If %Found(wtabl) and RTtype = reserveType;
             remainingAmt += rsvRemainDs(i);
          EndIf;
       EndFor;

       Return remainingAmt;

      /end-free
     p GetRemaining    e
       //===================================================================
       // GetRsvCnt - Get the reserve type count
       //===================================================================
     p GetRsvCnt       b
     d GetRsvCnt       pi             3  0
     d reserveType                    1    const

     d intRsvCnt       s              3  0 inz(0)
      /free

       intRsvCnt = 0 ;
       Chain (CMco#:CMfnd) pfn00;

       Setll intCo# prt00;
       Reade intCo# prt00 ;
       Dow not %eof;
          If RTtype = reserveType and RTstat = 'A' and
           %LookUp(%trim(RTkey):groupRsv) <> 0;
             intRsvCnt = intRsvCnt + 1;
             charRsvCode = %trim(RTkey);
          EndIf;
          Reade intCo# prt00;
       Enddo;

       Return intRsvCnt;

      /end-free
     p GetRsvCnt       e
       //===================================================================
       // Get Reserve Code desc
       //===================================================================
     p GetRsvDesc      b
     d GetRsvDesc      pi            15
     d strRsvCd                      10    const

     d strDesc         s             15    inz

      /free

       Clear strDesc;
       Chain (intCo#:strRsvCd) prt00;
       If %found(wtabl);
          strDesc = %trim(RTdesc);
       Endif;
       Return strDesc;

      /end-free
     p GetRsvDesc      e
       //===================================================================
       // LodDetails
       //===================================================================
     p LodDetails      b
     d LodDetails      pi

      /free

       For intCount = 1 to intArrSiz;
          If Hdg(intCount) = *blanks;
             dsDspVis(intCount) = *on;
             dsVsbArr(intCount) = *on;
             dsVsbAmt(intCount) = *on;
          Else;
             dsVsbArr(intCount) = *off;
             dsVsbAmt(intCount) = *off;
          Endif;
          scAmt(intCount) = Amt(intCount);
       Endfor;

       dsbIndm = *off;
       dsbmedm = *off;
       dsbExpm = *off;
       dsbLglm = *off;
       scHeadings = Hdg;
       scWksa     = Wksa;

       intCmpRt = 0;
       SetGt (CMco#:CMfnd:CMfyr:CMcase:*hival:*hival) paw01;
       ReadPe (CMco#:CMfnd:CMfyr:CMcase) paw01;
       If not %eof(WMAWL1);
          intCmpRt = AWcRt;
       Endif;

       For intCount = 1 to intArrSiz1;
          If isPrcMode = 'C';
             scCmpRt(intCount) = CmpRt(intCount);
          Else;
             scCmpRt(intCount) = intCmpRt;
          Endif;
       Endfor;
       scIndDthRt = intCmpRt;

       // Set Reserve Type
       dsRsvTyp1 = *off;
       dsRsvTyp2 = *off;
       dsRsvTyp3 = *off;
       dsRsvTyp4 = *off;
       clear dsRsvTyp;

       //*** beg chg ***** 10/08/15 *******************//
       //*** If isPrcMode = 'C';
       //***   dsRsvTyp = dbRsvTyp;
       //*** Endif;
       dsRsvTyp = dbRsvTyp;
       //*** end chg ***** 10/08/15 *******************//

       If GetRsvCnt('C') = 1;
          dsRsvTyp1 = *on;
          For intCount = 1 to 10;
             dsRsvTyp(intCount) = charRsvCode;
          Endfor;
          dsRsvTyp(23) = charRsvCode;
          dsRsvTyp(24) = charRsvCode;
          dsRsvTyp(25) = charRsvCode;
          dsRsvTyp(26) = charRsvCode;
          dsRsvTyp(27) = charRsvCode;
       ElseIf GetRsvCnt('C') = 0;
          dsRsvTyp1 = *on;
          For intCount = 1 to 10;
             dsDspVis(intCount) = *on;
          Endfor;
          dsbIndm = *on;
       Endif;

       If GetRsvCnt('M')= 1 and RtvMedExcl(CMco#:CMfnd:CMemp#:CMdiv:CMfYr)<>'Y';
          dsRsvTyp2 = *on;
          dsRsvTyp(11) = charRsvCode;
          dsRsvTyp(12) = charRsvCode;
          dsRsvTyp(13) = charRsvCode;
          dsRsvTyp(14) = charRsvCode;
          dsRsvTyp(28) = charRsvCode;
          dsRsvTyp(29) = charRsvCode;
          dsRsvTyp(30) = charRsvCode;
          dsRsvTyp(31) = charRsvCode;
          dsRsvTyp(32) = charRsvCode;
          dsRsvTyp(33) = charRsvCode;
       ElseIf GetRsvCnt('M') = 0 or RtvMedExcl(CMco#:CMfnd:CMemp#:CMdiv:CMfYr) =
        'Y';
          dsRsvTyp2 = *on;
          For intCount = 11 to 14;
             dsDspVis(intCount) = *on;
          Endfor;
          dsbMedm = *on;
       Endif;

       If GetRsvCnt('E') = 1;
          dsRsvTyp3 = *on;
          dsRsvTyp(15) = charRsvCode;
          dsRsvTyp(16) = charRsvCode;
          dsRsvTyp(17) = charRsvCode;
          dsRsvTyp(18) = charRsvCode;
          dsRsvTyp(34) = charRsvCode;
          dsRsvTyp(35) = charRsvCode;
       ElseIf GetRsvCnt('E') = 0;
          dsRsvTyp3 = *on;
          For intCount = 15 to 18;
             dsDspVis(intCount) = *on;
          Endfor;
          dsbExpm = *on;
       Endif;

       If GetRsvCnt('L') = 1;
          dsRsvTyp4 = *on;
          dsRsvTyp(19) = charRsvCode;
          dsRsvTyp(20) = charRsvCode;
          dsRsvTyp(21) = charRsvCode;
          dsRsvTyp(22) = charRsvCode;
          dsRsvTyp(36) = charRsvCode;
          dsRsvTyp(37) = charRsvCode;
       ElseIf GetRsvCnt('L') = 0;
          dsRsvTyp4 = *on;
          For intCount = 19 to 22;
             dsDspVis(intCount) = *on;
          Endfor;
          dsbLglm = *on;
       Endif;

       For intCount = 1 to intArrSiz;
          If (intCount < 11 and dsRsvTyp1 = *on) or (intCount > 10 and
           intCount < 15 and dsRsvTyp2 = *on) or
           (intCount > 14 and intCount < 19 and dsRsvTyp3 = *on) or
           (intCount > 18 and intCount < 23 and dsRsvTyp4 = *on);
             dsVsbArr(intCount) = *on;
          Endif;
       Endfor;

       clear scRsvCode;
       clear scRsvChg;
       clear dsRsvTp;
       intCnt = 1;
       For intCount = 1 to intArrSiz2;
          If dsRsvTyp(intCount) <> ' ';
             intIndex = %LookUp(dsRsvTyp(intCount):scRsvCode);
             If intIndex = 0;
                scRsvCode(intCnt) = dsRsvTyp(intCount);

                strKey = scRsvCode(intCnt);
                Chain (CMco#:strKey) prt00;
                If %found(wtabl);
                   dsRsvTp(intCnt) = RTtype;
                Endif;

                If intCount <= intArrSiz;
                   scRsvChg(intCnt)  = scAmt(intCount);
                Else;
                   scRsvChg(intCnt)  = scAmtAll(intCount-intArrSiz);
                Endif;
                intCnt = intCnt + 1;
             Else;
                If intCount <= intArrSiz;
                   scRsvChg(intIndex) = scRsvChg(intIndex) + scAmt(intCount);
                Else;
                   scRsvChg(intIndex) = scRsvChg(intIndex) +
                    scAmtAll(intCount-intArrSiz);
                Endif;
             Endif;
          Endif;
       Endfor;

      /end-free
     p LodDetails      e
      *====================================================================
      * utility procedure definitions
      /Copy sCopyClmPi
