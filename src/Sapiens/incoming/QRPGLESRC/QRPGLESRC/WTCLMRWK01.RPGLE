     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**************************************************************************
     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**************************************************************************
     h*                                                                        *
     h*  Description: Response program to Points                               *
     h*               - Activation group should be *CALLER                     *
     h*               - Do not seton LR, just do a return                      *
     h*               - This process should not lock any file                  *
     h*               - This process should only read files, no updates/writes *
     h*                 should be made                                         *
     h*                                                                        *
     h*  Called from: WTCLMRWK                                                 *
     h*  Calls to:                                                             *
     h*                                                                        *
     h*  Created on 12/01/2014 by C:279 for Project: 114WLF                    *
     h**************************************************************************
     h* Revisions:                                                             *
     h*                                                                        *
     h*  10/29/19 C:062 Ticket: 84644(045)
     h*                 Fix comment parsing.
     h*                                                                        *
     h**************************************************************************

     fwdrwp     if   e             disk    rename(prw01:prw0p)
     fwmcml1    if   e           k disk
     fwdcwl     if   e           k disk    rename(pcw01:pcw00)
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
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

      /copy sPrAtrium
      /copy sPrDate
      /copy wPrUsrPrms
      /copy wPrTable
      /copy wPrEmplr

      * Entry
     d ClaimWkHst      pr                  ExtPgm('WTCLMRWK01')
     d                                9  0
     d                            32767
     d                                1

     d ClaimWkHst      pi
     d inRrn                          9  0
     d outJSON                    32767
     d RtnCode                        1

     d intArrSiz       c                   22
     d intArrSiz1      c                    6
     d numEdtCde       c                   const('N')

     d                 ds
     d  hdg                    1    880
     d                                     Dim(intArrSiz)
     d  CWBfH1                 1     40
     d  CWBfH2                41     80
     d  CWBfH3                81    120
     d  CWBfH4               121    160
     d  CWBfH5               161    200
     d  CWBfHp               201    240
     d  CWIFH1               241    280
     d  CWIFH2               281    320
     d  CWIFH3               321    360
     d  CWIFH4               361    400
     d  CWMFH1               401    440
     d  CWMFH2               441    480
     d  CWMFH3               481    520
     d  CWMFH4               521    560
     d  CWEFH1               561    600
     d  CWEFH2               601    640
     d  CWEFH3               641    680
     d  CWEFH4               681    720
     d  CWLFH1               721    760
     d  CWLFH2               761    800
     d  CWLFH3               801    840
     d  CWLFH4               841    880

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
     d  RWBPRt                36     42  2

     d                 ds
     d  Amt                    1    198  2
     d                                     Dim(intArrSiz)
     d  Amta                   1    198
     d  RWBAm1                 1      9  2
     d  RWBAm2                10     18  2
     d  RWBAm3                19     27  2
     d  RWBAm4                28     36  2
     d  RWBAm5                37     45  2
     d  RWBPAm                46     54  2
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
     d  cmt                    1   6200
     d                                     Dim(40)
     d  strCmt1                1    155
     d  strCmt2              156    310
     d  strCmt3              311    465
     d  strCmt4              466    620
     d  strCmt5              621    775
     d  strCmt6              776    930
     d  strCmt7              931   1085
     d  strCmt8             1086   1240
     d  strCmt9             1241   1395
     d  strCmt10            1396   1550
     d  strCmt11            1551   1705
     d  strCmt12            1706   1860
     d  strCmt13            1861   2015
     d  strCmt14            2016   2170
     d  strCmt15            2171   2325
     d  strCmt16            2326   2480
     d  strCmt17            2481   2635
     d  strCmt18            2636   2790
     d  strCmt19            2791   2945
     d  strCmt20            2946   3100
     d  strCmt21            3101   3255
     d  strCmt22            3256   3410
     d  strCmt23            3411   3565
     d  strCmt24            3566   3720
     d  strCmt25            3721   3875
     d  strCmt26            3876   4030
     d  strCmt27            4031   4185
     d  strCmt28            4186   4340
     d  strCmt29            4341   4495
     d  strCmt30            4496   4650
     d  strCmt31            4651   4805
     d  strCmt32            4806   4960
     d  strCmt33            4961   5115
     d  strCmt34            5116   5270
     d  strCmt35            5271   5425
     d  strCmt36            5426   5580
     d  strCmt37            5581   5735
     d  strCmt38            5736   5890
     d  strCmt39            5891   6045
     d  strCmt40            6046   6200

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

      * Get Reserve type count
     d GetRsvCnt       pr             3  0
     d reserveType                    1    Const

     d intCnt          s              3  0
     d intLen          s              5  0
     d intPos          s              5  0
     d intRetval       s              5  0
     d retval          s              5  0
     d intPrvVal       s              5  0
     d intCount        s              5  0
     d strString       s            200
     d strString1      s           5000
     d strBlanks       s              1
     d strKey          s             10
     d strDesc         s             30
     d strMedExcl      s              1

      /free

       Chain(n) inRRN prw0p;
       Chain (RWco#:RWFnd:RWFyr:RWCase) pcm01;

       Chain (RWCo#:CMJrSt) pcw00;
       If not %found(WDCWL) or CWstat = 'I';
          Chain (CMco#:' ') pcw00;
       Endif;

       outJSON = ' ';
       Exsr BuildData;

       RtnCode = ' ';
       Return;

       //**********************************************************
       // BuildData for detail box
       //**********************************************************
       Begsr BuildData;

       intCnt = GetRsvCnt('C');

       If intCnt > 1;
          BuildJSON(outJSON :'OutDtlIndCode' :'Reserve Code');
       Endif;

       BuildJSON(outJSON :'OutBnftHdg1'   :%trimr(Hdg(1)));
       If Hdg(1) <> *blanks;
          BuildJSON(outJSON :'OutBnftWk1'    :%char(Wks(1)));
          BuildJSON(outJSON :'OutBnftRt1'    :%EditC(CmpRt(1):numEdtcde));
          BuildJSON(outJSON :'OutBnftAmt1'   :%EditC(Amt(1):numEdtcde));

          If intCnt > 1;
             strKey = RWICT01;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp1'   :%trim(strDesc));
          Endif;
       Endif;

       BuildJSON(outJSON :'OutBnftHdg2'   :%trimr(Hdg(2)));
       If Hdg(2) <> *blanks;
          BuildJSON(outJSON :'OutBnftWk2'    :%char(Wks(2)));
          BuildJSON(outJSON :'OutBnftRt2'    :%EditC(CmpRt(2):numEdtcde));
          BuildJSON(outJSON :'OutBnftAmt2'   :%EditC(Amt(2):numEdtcde));

          If intCnt > 1;
             strKey = RWICT02;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp2'   :%trim(strDesc));
          Endif;
       Endif;

       BuildJSON(outJSON :'OutBnftHdg3'   :%trimr(Hdg(3)));
       If Hdg(3) <> *blanks;
          BuildJSON(outJSON :'OutBnftWk3'    :%char(Wks(3)));
          BuildJSON(outJSON :'OutBnftRt3'    :%EditC(CmpRt(3):numEdtcde));
          BuildJSON(outJSON :'OutBnftAmt3'   :%EditC(Amt(3):numEdtcde));

          If intCnt > 1;
             strKey = RWICT03;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp3'   :%trim(strDesc));
          Endif;
       Endif;

       BuildJSON(outJSON :'OutBnftHdg4'   :%trimr(Hdg(4)));
       If Hdg(4) <> *blanks;
          BuildJSON(outJSON :'OutBnftWk4'    :%char(Wks(4)));
          BuildJSON(outJSON :'OutBnftRt4'    :%EditC(CmpRt(4):numEdtcde));
          BuildJSON(outJSON :'OutBnftAmt4'   :%EditC(Amt(4):numEdtcde));

          If intCnt > 1;
             strKey = RWICT04;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp4'   :%trim(strDesc));
          Endif;
       Endif;

       BuildJSON(outJSON :'OutBnftHdg5'   :%trimr(Hdg(5)));
       If Hdg(5) <> *blanks;
          BuildJSON(outJSON :'OutBnftWk5'    :%char(Wks(5)));
          BuildJSON(outJSON :'OutBnftRt5'    :%EditC(CmpRt(5):numEdtcde));
          BuildJSON(outJSON :'OutBnftAmt5'   :%EditC(Amt(5):numEdtcde));

          If intCnt > 1;
             strKey = RWICT05;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp5'   :%trim(strDesc));
          Endif;
       Endif;

       BuildJSON(outJSON :'OutBnftHdg7'   :%trimr(Hdg(6)));
       If Hdg(6) <> *blanks;
          BuildJSON(outJSON :'OutBnftPercentTo' :%char(RWBpRc));
          BuildJSON(outJSON :'OutDtlPerTo'      :'to');
          BuildJSON(outJSON :'OutDtlPer'        :'%');
          BuildJSON(outJSON :'OutBnftTo'        :%trim(RWBpTo));
          BuildJSON(outJSON :'OutBnftWk7'    :%char(Wks(6)));
          BuildJSON(outJSON :'OutBnftRt7'    :%EditC(CmpRt(6):numEdtcde));
          BuildJSON(outJSON :'OutBnftAmt7'   :%EditC(Amt(6):numEdtcde));

          If intCnt > 1;
             strKey = RWICT07;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp7'   :%trim(strDesc));
          Endif;
       Endif;

       // Death Benefit details
       BuildJSON(outJSON :'OutBnftHdg6'   :'Death Benefits');
       BuildJSON(outJSON :'OutBnftWk6'    :%char(RWDbWk));
       BuildJSON(outJSON :'OutBnftRt6'    :%EditC(RWDbCr:numEdtcde));
       BuildJSON(outJSON :'OutBnftAmt6'   :%EditC(RWDbAm:numEdtcde));

       // Funeral
       BuildJSON(outJSON :'OutFunBebAmt'  :%EditC(RWFbAm:numEdtcde));

       // Disfigurement
       BuildJSON(outJSON :'OutDsFgmntAmt' :%EditC(RWDsAm:numEdtcde));

       // Settlement
       BuildJSON(outJSON :'OutStlmntAmt'  :%EditC(RWStAm:numEdtcde));

       If intCnt > 1;
          strKey = RWICT06;
          Exsr GetDesc;
          BuildJSON(outJSON :'OutResvTyp6'   :%trim(strDesc));

          strKey = RWICT12;
          Exsr GetDesc;
          BuildJSON(outJSON :'OutResvTyp12'  :%trim(strDesc));
          If strDesc <> ' ';
             BuildJSON(outJSON :'OutResvLbl12'  :'Reserve Code');
          Else;
             BuildJSON(outJSON :'OutResvLbl12'  :' ');
          Endif;

          strKey = RWICT13;
          Exsr GetDesc;
          BuildJSON(outJSON :'OutResvTyp13'  :%trim(strDesc));
          If strDesc <> ' ';
             BuildJSON(outJSON :'OutResvLbl13'  :'Reserve Code');
          Else;
             BuildJSON(outJSON :'OutResvLbl13'  :' ');
          Endif;

          strKey = RWICT14;
          Exsr GetDesc;
          BuildJSON(outJSON :'OutResvTyp14'  :%trim(strDesc));
          If strDesc <> ' ';
             BuildJSON(outJSON :'OutResvLbl14'  :'Reserve Code');
          Else;
             BuildJSON(outJSON :'OutResvLbl14'  :' ');
          Endif;

          strKey = RWICT15;
          Exsr GetDesc;
          BuildJSON(outJSON :'OutResvTyp15'  :%trim(strDesc));
          If strDesc <> ' ';
             BuildJSON(outJSON :'OutResvLbl15'  :'Reserve Code');
          Else;
             BuildJSON(outJSON :'OutResvLbl15'  :' ');
          Endif;
       Endif;

       // Indemnity Flexible
       If Hdg(7) <> *blanks;
          BuildJSON(outJSON :'OutFlxHdg1'    :%trimr(Hdg(7)));
          BuildJSON(outJSON :'OutFlxAmt1'    :%EditC(Amt(7):numEdtcde));
          If intCnt > 1;
             strKey = RWICT08;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp8'   :%trim(strDesc));
             If strDesc <> ' ';
                BuildJSON(outJSON :'OutBnftTypHdg1':'Reserve Code');
             Else;
                BuildJSON(outJSON :'OutBnftTypHdg1':' ');
             Endif;
          Endif;
       Endif;
       If Hdg(8) <> *blanks;
          BuildJSON(outJSON :'OutFlxHdg2'    :%trimr(Hdg(8)));
          BuildJSON(outJSON :'OutFlxAmt2'    :%EditC(Amt(8):numEdtcde));
          If intCnt > 1;
             strKey = RWICT09;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp9'   :%trim(strDesc));
             If strDesc <> ' ';
                BuildJSON(outJSON :'OutBnftTypHdg2':'Reserve Code');
             Else;
                BuildJSON(outJSON :'OutBnftTypHdg2':' ');
             Endif;
          Endif;
       Endif;
       If Hdg(9) <> *blanks;
          BuildJSON(outJSON :'OutFlxHdg3'    :%trimr(Hdg(9)));
          BuildJSON(outJSON :'OutFlxAmt3'    :%EditC(Amt(9):numEdtcde));
          If intCnt > 1;
             strKey = RWICT10;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp10'  :%trim(strDesc));
             If strDesc <> ' ';
                BuildJSON(outJSON :'OutBnftTypHdg3':'Reserve Code');
             Else;
                BuildJSON(outJSON :'OutBnftTypHdg3':' ');
             Endif;
          Endif;
       Endif;
       If Hdg(10) <> *blanks;
          BuildJSON(outJSON :'OutFlxHdg4'    :%trimr(Hdg(10)));
          BuildJSON(outJSON :'OutFlxAmt4'    :%EditC(Amt(10):numEdtcde));
          If intCnt > 1;
             strKey = RWICT11;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp11'  :%trim(strDesc));
             If strDesc <> ' ';
                BuildJSON(outJSON :'OutBnftTypHdg4':'Reserve Code');
             Else;
                BuildJSON(outJSON :'OutBnftTypHdg4':' ');
             Endif;
          Endif;
       Endif;

       // Manual Adjustment
       BuildJSON(outJSON :'OutMnlAdjAmt'  :%EditC(RWiMAj:numEdtcde));

       // Current Outstanding/Remaining Indemnity reserve
       BuildJSON(outJSON :'OutCurOstRsvAmt' :%EditC(RWCoir:numEdtcde));

       // Paid to Date
       BuildJSON(outJSON :'OutPdToDtAmt'    :%EditC(RWiPtD:numEdtcde));

       // Recommended change
       BuildJSON(outJSON :'OutRcmndChgAmt'  :%EditC(RWiRCh:numEdtcde));

       // Total Indemnity including change
       BuildJSON(outJSON :'OutTtlIndIncAmt' :%EditC(RWTiic:numEdtcde));

       intCnt = GetRsvCnt('M');
       strMedExcl = RtvMedExcl(CMco#:CMfnd:CMemp#:CMdiv:CMfYr);

       // Physician
       BuildJSON(outJSON :'OutMedPhyAmt'    :%EditC(RWPhAm:numEdtcde));

       // Hospital
       BuildJSON(outJSON :'OutMedHspAmt'    :%EditC(RWHoAm:numEdtcde));

       // Surgery
       BuildJSON(outJSON :'OutMedSurAmt'    :%EditC(RWSuAm:numEdtcde));

       // RX/Med supplies
       BuildJSON(outJSON :'OutMedRxSpAmt'   :%EditC(RWRxAm:numEdtcde));

       // Diagnostics
       BuildJSON(outJSON :'OutMedDigAmt'    :%EditC(RWDiAm:numEdtcde));

       // Physical Therapy
       BuildJSON(outJSON :'OutMedPhyThrpAmt' :%EditC(RWPtAm:numEdtcde));

       If intCnt > 1 and strMedExcl <> 'Y';
          strKey = RWMCT01;
          Exsr GetDesc;
          BuildJSON(outJSON :'OutResvTyp16'    :%trim(strDesc));
          If strDesc <> ' ';
             BuildJSON(outJSON :'OutResvLbl16'  :'Reserve Code');
          Else;
             BuildJSON(outJSON :'OutResvLbl16'  :' ');
          Endif;

          strKey = RWMCT02;
          Exsr GetDesc;
          BuildJSON(outJSON :'OutResvTyp17'    :%trim(strDesc));
          If strDesc <> ' ';
             BuildJSON(outJSON :'OutResvLbl17'  :'Reserve Code');
          Else;
             BuildJSON(outJSON :'OutResvLbl17'  :' ');
          Endif;

          strKey = RWMCT03;
          Exsr GetDesc;
          BuildJSON(outJSON :'OutResvTyp18'    :%trim(strDesc));
          If strDesc <> ' ';
             BuildJSON(outJSON :'OutResvLbl18'  :'Reserve Code');
          Else;
             BuildJSON(outJSON :'OutResvLbl18'  :' ');
          Endif;

          strKey = RWMCT04;
          Exsr GetDesc;
          BuildJSON(outJSON :'OutResvTyp19'    :%trim(strDesc));
          If strDesc <> ' ';
             BuildJSON(outJSON :'OutResvLbl19'  :'Reserve Code');
          Else;
             BuildJSON(outJSON :'OutResvLbl19'  :' ');
          Endif;

          strKey = RWMCT05;
          Exsr GetDesc;
          BuildJSON(outJSON :'OutResvTyp20'    :%trim(strDesc));
          If strDesc <> ' ';
             BuildJSON(outJSON :'OutResvLbl20'  :'Reserve Code');
          Else;
             BuildJSON(outJSON :'OutResvLbl20'  :' ');
          Endif;

          strKey = RWMCT06;
          Exsr GetDesc;
          BuildJSON(outJSON :'OutResvTyp21'    :%trim(strDesc));
          If strDesc <> ' ';
             BuildJSON(outJSON :'OutResvLbl21'  :'Reserve Code');
          Else;
             BuildJSON(outJSON :'OutResvLbl21'  :' ');
          Endif;
       Endif;

       // Medical Flexible Headings
       If Hdg(11) <> *blanks;
          BuildJSON(outJSON :'OutMedFlxHdg1' :%trimr(Hdg(11)));
          BuildJSON(outJSON :'OutMedFlxAmt1' :%EditC(Amt(11):numEdtcde));
          If intCnt > 1 and strMedExcl <> 'Y';
             strKey = RWMCT07;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp22'  :%trim(strDesc));
             If strDesc <> ' ';
                BuildJSON(outJSON :'OutBnftTypHdg5':'Reserve Code');
             Else;
                BuildJSON(outJSON :'OutBnftTypHdg5':' ');
             Endif;
          Endif;
       Endif;
       If Hdg(12) <> *blanks;
          BuildJSON(outJSON :'OutMedFlxHdg2' :%trimr(Hdg(12)));
          BuildJSON(outJSON :'OutMedFlxAmt2' :%EditC(Amt(12):numEdtcde));
          If intCnt > 1 and strMedExcl <> 'Y';
             strKey = RWMCT08;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp23'  :%trim(strDesc));
             If strDesc <> ' ';
                BuildJSON(outJSON :'OutBnftTypHdg6':'Reserve Code');
             Else;
                BuildJSON(outJSON :'OutBnftTypHdg6':' ');
             Endif;
          Endif;
       Endif;
       If Hdg(13) <> *blanks;
          BuildJSON(outJSON :'OutMedFlxHdg3' :%trimr(Hdg(13)));
          BuildJSON(outJSON :'OutMedFlxAmt3' :%EditC(Amt(13):numEdtcde));
          If intCnt > 1 and strMedExcl <> 'Y';
             strKey = RWMCT09;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp24'  :%trim(strDesc));
             If strDesc <> ' ';
                BuildJSON(outJSON :'OutBnftTypHdg7':'Reserve Code');
             Else;
                BuildJSON(outJSON :'OutBnftTypHdg7':' ');
             Endif;
          Endif;
       Endif;
       If Hdg(14) <> *blanks;
          BuildJSON(outJSON :'OutMedFlxHdg4' :%trimr(Hdg(14)));
          BuildJSON(outJSON :'OutMedFlxAmt4' :%EditC(Amt(14):numEdtcde));
          If intCnt > 1 and strMedExcl <> 'Y';
             strKey = RWMCT10;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp25'  :%trim(strDesc));
             If strDesc <> ' ';
                BuildJSON(outJSON :'OutBnftTypHdg8':'Reserve Code');
             Else;
                BuildJSON(outJSON :'OutBnftTypHdg8':' ');
             Endif;
          Endif;
       Endif;

       // Current Outstanding/Remaining Medical Reserve
       BuildJSON(outJSON :'OutMedCurOstRsvAmt' :%EditC(RWComr:numEdtcde));

       // Paid to Date
       BuildJSON(outJSON :'OutMedPdToDtAmt'    :%EditC(RWmPtD:numEdtcde));

       // Recommended change
       BuildJSON(outJSON :'OutMedRcmndChgAmt'  :%EditC(RWmRCh:numEdtcde));

       // Total Medical including change
       BuildJSON(outJSON :'OutTtlMedIncAmt'    :%EditC(RWTMic:numEdtcde));

       intCnt = GetRsvCnt('E');

       // Surveillance
       BuildJSON(outJSON :'OutExpSrvlncAmt'    :%EditC(RWSvAm:numEdtcde));

       // Medical Records
       BuildJSON(outJSON :'OutExpMedRcdAmt'    :%EditC(RWMrAm:numEdtcde));

       If intCnt > 1;
          strKey = RWECT01;
          Exsr GetDesc;
          BuildJSON(outJSON :'OutResvTyp26'       :%trim(strDesc));
          If strDesc <> ' ';
             BuildJSON(outJSON :'OutResvLbl26'  :'Reserve Code');
          Else;
             BuildJSON(outJSON :'OutResvLbl26'  :' ');
          Endif;

          strKey = RWECT02;
          Exsr GetDesc;
          BuildJSON(outJSON :'OutResvTyp27'       :%trim(strDesc));
          If strDesc <> ' ';
             BuildJSON(outJSON :'OutResvLbl27'  :'Reserve Code');
          Else;
             BuildJSON(outJSON :'OutResvLbl27'  :' ');
          Endif;
       Endif;

       // Expense Flexible Headings
       If Hdg(15) <> *blanks;
          BuildJSON(outJSON :'OutExpFlxHdg1' :%trimr(Hdg(15)));
          BuildJSON(outJSON :'OutExpFlxAmt1' :%EditC(Amt(15):numEdtcde));
          If intCnt > 1;
             strKey = RWECT03;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp28'  :%trim(strDesc));
             If strDesc <> ' ';
                BuildJSON(outJSON :'OutBnftTypHdg9':'Reserve Code');
             Else;
                BuildJSON(outJSON :'OutBnftTypHdg9':' ');
             Endif;
          Endif;
       Endif;
       If Hdg(16) <> *blanks;
          BuildJSON(outJSON :'OutExpFlxHdg2' :%trimr(Hdg(16)));
          BuildJSON(outJSON :'OutExpFlxAmt2' :%EditC(Amt(16):numEdtcde));
          If intCnt > 1;
             strKey = RWECT04;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp29'  :%trim(strDesc));
             If strDesc <> ' ';
                BuildJSON(outJSON :'OutBnftTypHdg10':'Reserve Code');
             Else;
                BuildJSON(outJSON :'OutBnftTypHdg10':' ');
             Endif;
          Endif;
       Endif;
       If Hdg(17) <> *blanks;
          BuildJSON(outJSON :'OutExpFlxHdg3' :%trimr(Hdg(17)));
          BuildJSON(outJSON :'OutExpFlxAmt3' :%EditC(Amt(17):numEdtcde));
          If intCnt > 1;
             strKey = RWECT05;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp30'  :%trim(strDesc));
             If strDesc <> ' ';
                BuildJSON(outJSON :'OutBnftTypHdg11':'Reserve Code');
             Else;
                BuildJSON(outJSON :'OutBnftTypHdg11':' ');
             Endif;
          Endif;
       Endif;
       If Hdg(18) <> *blanks;
          BuildJSON(outJSON :'OutExpFlxHdg4' :%trimr(Hdg(18)));
          BuildJSON(outJSON :'OutExpFlxAmt4' :%EditC(Amt(18):numEdtcde));
          If intCnt > 1;
             strKey = RWECT06;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp31'  :%trim(strDesc));
             If strDesc <> ' ';
                BuildJSON(outJSON :'OutBnftTypHdg12':'Reserve Code');
             Else;
                BuildJSON(outJSON :'OutBnftTypHdg12':' ');
             Endif;
          Endif;
       Endif;

       // Current Outstanding/Remaining Expense Reserve
       BuildJSON(outJSON :'OutExpCurOstRsvAmt' :%EditC(RWCoer:numEdtcde));

       // Paid to Date
       BuildJSON(outJSON :'OutExpPdToDtAmt'    :%EditC(RWePtD:numEdtcde));

       // Recommended change
       BuildJSON(outJSON :'OutExpRcmndChgAmt'  :%EditC(RWeRCh:numEdtcde));

       // Total Expense including change
       BuildJSON(outJSON :'OutTtlExpIncAmt'    :%EditC(RWTEic:numEdtcde));

       intCnt = GetRsvCnt('L');

       // Defense Attorney
       BuildJSON(outJSON :'OutLglDefAtnAmt'    :%EditC(RWDfAm:numEdtcde));

       // Depositions
       BuildJSON(outJSON :'OutLglDepAmt'       :%EditC(RWDpAm:numEdtcde));

       If intCnt > 1;
          strKey = RWLCT02;
          Exsr GetDesc;
          BuildJSON(outJSON :'OutResvTyp32'       :%trim(strDesc));
          If strDesc <> ' ';
             BuildJSON(outJSON :'OutResvLbl32'  :'Reserve Code');
          Else;
             BuildJSON(outJSON :'OutResvLbl32'  :' ');
          Endif;

          strKey = RWLCT01;
          Exsr GetDesc;
          BuildJSON(outJSON :'OutResvTyp33'       :%trim(strDesc));
          If strDesc <> ' ';
             BuildJSON(outJSON :'OutResvLbl33'  :'Reserve Code');
          Else;
             BuildJSON(outJSON :'OutResvLbl33'  :' ');
          Endif;
       Endif;

       // Legal Flexible Headings
       If Hdg(19) <> *blanks;
          BuildJSON(outJSON :'OutLglFlxHdg1' :%trimr(Hdg(19)));
          BuildJSON(outJSON :'OutLglFlxAmt1' :%EditC(Amt(19):numEdtcde));
          If intCnt > 1;
             strKey = RWLCT03;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp34'  :%trim(strDesc));
             If strDesc <> ' ';
                BuildJSON(outJSON :'OutBnftTypHdg13':'Reserve Code');
             Else;
                BuildJSON(outJSON :'OutBnftTypHdg13':' ');
             Endif;
          Endif;
       Endif;
       If Hdg(20) <> *blanks;
          BuildJSON(outJSON :'OutLglFlxHdg2' :%trimr(Hdg(20)));
          BuildJSON(outJSON :'OutLglFlxAmt2' :%EditC(Amt(20):numEdtcde));
          If intCnt > 1;
             strKey = RWLCT04;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp35'  :%trim(strDesc));
             If strDesc <> ' ';
                BuildJSON(outJSON :'OutBnftTypHdg14':'Reserve Code');
             Else;
                BuildJSON(outJSON :'OutBnftTypHdg14':' ');
             Endif;
          Endif;
       Endif;
       If Hdg(21) <> *blanks;
          BuildJSON(outJSON :'OutLglFlxHdg3' :%trimr(Hdg(21)));
          BuildJSON(outJSON :'OutLglFlxAmt3' :%EditC(Amt(21):numEdtcde));
          If intCnt > 1;
             strKey = RWLCT05;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp36'  :%trim(strDesc));
             If strDesc <> ' ';
                BuildJSON(outJSON :'OutBnftTypHdg15':'Reserve Code');
             Else;
                BuildJSON(outJSON :'OutBnftTypHdg15':' ');
             Endif;
          Endif;
       Endif;
       If Hdg(22) <> *blanks;
          BuildJSON(outJSON :'OutLglFlxHdg4' :%trimr(Hdg(22)));
          BuildJSON(outJSON :'OutLglFlxAmt4' :%EditC(Amt(22):numEdtcde));
          If intCnt > 1;
             strKey = RWLCT06;
             Exsr GetDesc;
             BuildJSON(outJSON :'OutResvTyp37'  :%trim(strDesc));
             If strDesc <> ' ';
                BuildJSON(outJSON :'OutBnftTypHdg16':'Reserve Code');
             Else;
                BuildJSON(outJSON :'OutBnftTypHdg16':' ');
             Endif;
          Endif;
       Endif;

       // Current Outstanding/Remaining Legal Reserve
       BuildJSON(outJSON :'OutLglCurOstRsvAmt' :%EditC(RWColr:numEdtcde));

       // Paid to Date
       BuildJSON(outJSON :'OutLglPdToDtAmt'    :%EditC(RWlPtD:numEdtcde));

       // Recommended change
       BuildJSON(outJSON :'OutLglRcmndChgAmt'  :%EditC(RWlRCh:numEdtcde));

       // Total Legal including change
       BuildJSON(outJSON :'OutTtlLglIncAmt'    :%EditC(RWTLic:numEdtcde));

       // Total Line
       BuildJSON(outJSON :'OutTtlRcmndChgAmt'  :%EditC(RWTRCh:numEdtcde));
       BuildJSON(outJSON :'OutTtlIncAftChgAmt' :%EditC(RWTTic:numEdtcde));

       // Reserve Rationale
       intlen    = 155;
       intPrvVal = 1;
       intRetVal = 1;
       strString1 = RWcomm;
       intCount = 1;
       clear Cmt;

       Dow strString1 <> ' ' and intCount <= 40;
          strString = %subst(strString1:intPrvVal:intLen);
          Exsr scanSpace;

          If RetVal = intLen or %subst(strString1:intLen+1:1) = ' ';
             Cmt(intCount) = %subst(strString1:intPrvVal:intLen);
             intCount = intCount + 1;
             clear strString1;
             intRetVal = intRetVal + intLen;
          Else;
             Cmt(intCount) = %subst(strString1:intPrvVal:retVal);
             intCount = intCount + 1;
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
       Enddo;
       BuildJSON(outJSON :'TxaRtnCmt1'  :strCmt1);
       BuildJSON(outJSON :'TxaRtnCmt2'  :strCmt2);
       BuildJSON(outJSON :'TxaRtnCmt3'  :strCmt3);
       BuildJSON(outJSON :'TxaRtnCmt4'  :strCmt4);
       BuildJSON(outJSON :'TxaRtnCmt5'  :strCmt5);
       BuildJSON(outJSON :'TxaRtnCmt6'  :strCmt6);
       BuildJSON(outJSON :'TxaRtnCmt7'  :strCmt7);
       BuildJSON(outJSON :'TxaRtnCmt8'  :strCmt8);
       BuildJSON(outJSON :'TxaRtnCmt9'  :strCmt9);
       BuildJSON(outJSON :'TxaRtnCmt10' :strCmt10);
       BuildJSON(outJSON :'TxaRtnCmt11' :strCmt11);
       BuildJSON(outJSON :'TxaRtnCmt12' :strCmt12);
       BuildJSON(outJSON :'TxaRtnCmt13' :strCmt13);
       BuildJSON(outJSON :'TxaRtnCmt14' :strCmt14);
       BuildJSON(outJSON :'TxaRtnCmt15' :strCmt15);
       BuildJSON(outJSON :'TxaRtnCmt16' :strCmt16);
       BuildJSON(outJSON :'TxaRtnCmt17' :strCmt17);
       BuildJSON(outJSON :'TxaRtnCmt18' :strCmt18);
       BuildJSON(outJSON :'TxaRtnCmt19' :strCmt19);
       BuildJSON(outJSON :'TxaRtnCmt20' :strCmt20);
       BuildJSON(outJSON :'TxaRtnCmt21' :strCmt21);
       BuildJSON(outJSON :'TxaRtnCmt22' :strCmt22);
       BuildJSON(outJSON :'TxaRtnCmt23' :strCmt23);
       BuildJSON(outJSON :'TxaRtnCmt24' :strCmt24);
       BuildJSON(outJSON :'TxaRtnCmt25' :strCmt25);
       BuildJSON(outJSON :'TxaRtnCmt26' :strCmt26);
       BuildJSON(outJSON :'TxaRtnCmt27' :strCmt27);
       BuildJSON(outJSON :'TxaRtnCmt28' :strCmt28);
       BuildJSON(outJSON :'TxaRtnCmt29' :strCmt29);
       BuildJSON(outJSON :'TxaRtnCmt30' :strCmt30);
       BuildJSON(outJSON :'TxaRtnCmt31' :strCmt31);
       BuildJSON(outJSON :'TxaRtnCmt32' :strCmt32);
       BuildJSON(outJSON :'TxaRtnCmt33' :strCmt33);
       BuildJSON(outJSON :'TxaRtnCmt34' :strCmt34);
       BuildJSON(outJSON :'TxaRtnCmt35' :strCmt35);
       BuildJSON(outJSON :'TxaRtnCmt36' :strCmt36);
       BuildJSON(outJSON :'TxaRtnCmt37' :strCmt37);
       BuildJSON(outJSON :'TxaRtnCmt38' :strCmt38);
       BuildJSON(outJSON :'TxaRtnCmt39' :strCmt39);
       BuildJSON(outJSON :'TxaRtnCmt40' :strCmt40);

       Endsr;
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
       // Get Reserve Code desc
       //===================================================================
       Begsr GetDesc;

       clear strDesc;

       Chain (CMco#:strKey) prt00;
       If %found(wtabl);
          strDesc = %trim(RTdesc);
       Endif;

       Endsr;

      /end-free
       //===================================================================
       // GetRsvCnt - Get Count for Reserve type code
       //===================================================================
     p GetRsvCnt       b
     d GetRsvCnt       pi             3  0
     d reserveType                    1    const

     d intRsvCnt       s              3  0 inz(0)
      /free

       intRsvCnt = 0 ;
       Chain (CMco#:CMfnd) pfn00;

       Setll CMco# prt00;
       Reade CMco# prt00 ;
       Dow not %eof;
          If RTtype = reserveType and RTstat = 'A' and
           %LookUp(%trim(RTkey):groupRsv) <> 0;
             intRsvCnt = intRsvCnt + 1;
          EndIf;
          Reade CMco# prt00;
       Enddo;

       Return intRsvCnt;

      /end-free
     p GetRsvCnt       e
