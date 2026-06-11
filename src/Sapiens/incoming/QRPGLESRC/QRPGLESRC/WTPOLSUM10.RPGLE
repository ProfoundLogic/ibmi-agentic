     h Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     h*====================================================================*
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h*====================================================================*
     h*  Description: Response program to serve Premium Information
     h*               detail box - TAB 3.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files,
     h*                  no updates/writes should be made
     h*
     h*  Called from: WTPOLSUM
     h*  Calls to:
     h*
     h*  Created on 01/30/13 by C:65 for Project Transform
     h*====================================================================*
     h* Revisions:
     h*
     h* 07/17/13 C:136 Project: WC2193
     h*                Move calc's to WPIPOLPOP.PremiumInfo subprocedure
     h*                Changes not commented.
     h*====================================================================*

      *entry
     dPolicySumm08     pr                  ExtPgm('WTPOLSUM08')
     d                                9  0
     d                            32767
     d                                1
     dPolicySumm08     pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

      /copy wPrPolPop
      /copy sPrAtrium

     d inFlag          s              1    inz('D')
     d outHeadings     s           1100
     d outFactors      s           1100
     d outAmounts      s            550
     d outInfPrem      s            550

     d                 ds
     d  hdg                    1   1100
     d                                     Dim(50)
     d  hdga                   1   1100
     d  head01                 1     22
     d  head02                23     44
     d  head03                45     66
     d  head04                67     88
     d  head05                89    110
     d  head06               111    132
     d  head07               133    154
     d  head08               155    176
     d  head09               177    198
     d  head10               199    220
     d  head11               221    242
     d  head12               243    264
     d  head13               265    286
     d  head14               287    308
     d  head15               309    330
     d  head16               331    352
     d  head17               353    374
     d  head18               375    396
     d  head19               397    418
     d  head20               419    440
     d  head21               441    462
     d  head22               463    484
     d  head23               485    506
     d  head24               507    528
     d  head25               529    550
     d  head26               551    572
     d  head27               573    594
     d  head28               595    616
     d  head29               617    638
     d  head30               639    660
     d  head31               661    682
     d  head32               683    704
     d  head33               705    726
     d  head34               727    748
     d  head35               749    770
     d  head36               771    792
     d  head37               793    814
     d  head38               815    836
     d  head39               837    858
     d  head40               859    880
     d  head41               881    902
     d  head42               903    924
     d  head43               925    946
     d  head44               947    968
     d  head45               969    990
     d  head46               991   1012
     d  head47              1013   1034
     d  head48              1035   1056
     d  head49              1057   1078
     d  head50              1079   1100

     d                 ds
     d  fct                    1   1100
     d                                     Dim(50)
     d  fcta                   1   1100
     d  fact01                 1     22
     d  fact02                23     44
     d  fact03                45     66
     d  fact04                67     88
     d  fact05                89    110
     d  fact06               111    132
     d  fact07               133    154
     d  fact08               155    176
     d  fact09               177    198
     d  fact10               199    220
     d  fact11               221    242
     d  fact12               243    264
     d  fact13               265    286
     d  fact14               287    308
     d  fact15               309    330
     d  fact16               331    352
     d  fact17               353    374
     d  fact18               375    396
     d  fact19               397    418
     d  fact20               419    440
     d  fact21               441    462
     d  fact22               463    484
     d  fact23               485    506
     d  fact24               507    528
     d  fact25               529    550
     d  fact26               551    572
     d  fact27               573    594
     d  fact28               595    616
     d  fact29               617    638
     d  fact30               639    660
     d  fact31               661    682
     d  fact32               683    704
     d  fact33               705    726
     d  fact34               727    748
     d  fact35               749    770
     d  fact36               771    792
     d  fact37               793    814
     d  fact38               815    836
     d  fact39               837    858
     d  fact40               859    880
     d  fact41               881    902
     d  fact42               903    924
     d  fact43               925    946
     d  fact44               947    968
     d  fact45               969    990
     d  fact46               991   1012
     d  fact47              1013   1034
     d  fact48              1035   1056
     d  fact49              1057   1078
     d  fact50              1079   1100
     d                 ds
     d  tam                    1    550  2
     d                                     Dim(50)
     d  tama                   1    550
     d  decTA01                1     11  2
     d  decTA02               12     22  2
     d  decTA03               23     33  2
     d  decTA04               34     44  2
     d  decTA05               45     55  2
     d  decTA06               56     66  2
     d  decTA07               67     77  2
     d  decTA08               78     88  2
     d  decTA09               89     99  2
     d  decTA10              100    110  2
     d  decTA11              111    121  2
     d  decTA12              122    132  2
     d  decTA13              133    143  2
     d  decTA14              144    154  2
     d  decTA15              155    165  2
     d  decTA16              166    176  2
     d  decTA17              177    187  2
     d  decTA18              188    198  2
     d  decTA19              199    209  2
     d  decTA20              210    220  2
     d  decTA21              221    231  2
     d  decTA22              232    242  2
     d  decTA23              243    253  2
     d  decTA24              254    264  2
     d  decTA25              265    275  2
     d  decTA26              276    286  2
     d  decTA27              287    297  2
     d  decTA28              298    308  2
     d  decTA29              309    319  2
     d  decTA30              320    330  2
     d  decTA31              331    341  2
     d  decTA32              342    352  2
     d  decTA33              353    363  2
     d  decTA34              364    374  2
     d  decTA35              375    385  2
     d  decTA36              386    396  2
     d  decTA37              397    407  2
     d  decTA38              408    418  2
     d  decTA39              419    429  2
     d  decTA40              430    440  2
     d  decTA41              441    451  2
     d  decTA42              452    462  2
     d  decTA43              463    473  2
     d  decTA44              474    484  2
     d  decTA45              485    495  2
     d  decTA46              496    506  2
     d  decTA47              507    517  2
     d  decTA48              518    528  2
     d  decTA49              529    539  2
     d  decTA50              540    550  2

      /Free
       outJSON = ' ';
       PremiumInfo(inRRN:inFLAG:outHeadings:outFactors:outAmounts:outInfPrem);
       hdga = outHeadings;
       fcta = outFactors;
       tama = outAmounts;

       //* Load Heading
       BuildJSON(outJSON :'Outhead01':%TrimR(head01));
       BuildJSON(outJSON :'Outhead02':%TrimR(head02));
       BuildJSON(outJSON :'Outhead03':%TrimR(head03));
       BuildJSON(outJSON :'Outhead04':%TrimR(head04));
       BuildJSON(outJSON :'Outhead05':%TrimR(head05));
       BuildJSON(outJSON :'Outhead06':%TrimR(head06));
       BuildJSON(outJSON :'Outhead07':%TrimR(head07));
       BuildJSON(outJSON :'Outhead08':%TrimR(head08));
       BuildJSON(outJSON :'Outhead09':%TrimR(head09));
       BuildJSON(outJSON :'Outhead10':%TrimR(head10));
       BuildJSON(outJSON :'Outhead11':%TrimR(head11));
       BuildJSON(outJSON :'Outhead12':%TrimR(head12));
       BuildJSON(outJSON :'Outhead13':%TrimR(head13));
       BuildJSON(outJSON :'Outhead14':%TrimR(head14));
       BuildJSON(outJSON :'Outhead15':%TrimR(head15));
       BuildJSON(outJSON :'Outhead16':%TrimR(head16));
       BuildJSON(outJSON :'Outhead17':%TrimR(head17));
       BuildJSON(outJSON :'Outhead18':%TrimR(head18));
       BuildJSON(outJSON :'Outhead19':%TrimR(head19));
       BuildJSON(outJSON :'Outhead20':%TrimR(head20));
       BuildJSON(outJSON :'Outhead21':%TrimR(head21));
       BuildJSON(outJSON :'Outhead22':%TrimR(head22));
       BuildJSON(outJSON :'Outhead23':%TrimR(head23));
       BuildJSON(outJSON :'Outhead24':%TrimR(head24));
       BuildJSON(outJSON :'Outhead25':%TrimR(head25));
       BuildJSON(outJSON :'Outhead26':%TrimR(head26));
       BuildJSON(outJSON :'Outhead27':%TrimR(head27));
       BuildJSON(outJSON :'Outhead28':%TrimR(head28));
       BuildJSON(outJSON :'Outhead29':%TrimR(head29));
       BuildJSON(outJSON :'Outhead30':%TrimR(head30));
       BuildJSON(outJSON :'Outhead31':%TrimR(head31));
       BuildJSON(outJSON :'Outhead32':%TrimR(head32));
       BuildJSON(outJSON :'Outhead33':%TrimR(head33));
       BuildJSON(outJSON :'Outhead34':%TrimR(head34));
       BuildJSON(outJSON :'Outhead35':%TrimR(head35));
       BuildJSON(outJSON :'Outhead36':%TrimR(head36));
       BuildJSON(outJSON :'Outhead37':%TrimR(head37));
       BuildJSON(outJSON :'Outhead38':%TrimR(head38));
       BuildJSON(outJSON :'Outhead39':%TrimR(head39));
       BuildJSON(outJSON :'Outhead40':%TrimR(head40));
       BuildJSON(outJSON :'Outhead41':%TrimR(head41));
       BuildJSON(outJSON :'Outhead42':%TrimR(head42));
       BuildJSON(outJSON :'Outhead43':%TrimR(head43));
       BuildJSON(outJSON :'Outhead44':%TrimR(head44));
       BuildJSON(outJSON :'Outhead45':%TrimR(head45));
       BuildJSON(outJSON :'Outhead46':%TrimR(head46));
       BuildJSON(outJSON :'Outhead47':%TrimR(head47));
       BuildJSON(outJSON :'Outhead48':%TrimR(head48));
       BuildJSON(outJSON :'Outhead49':%TrimR(head49));
       BuildJSON(outJSON :'Outhead50':%TrimR(head50));

       //* Load Factor
       BuildJSON(outJSON :'Outfact01':%TrimR(fact01));
       BuildJSON(outJSON :'Outfact02':%TrimR(fact02));
       BuildJSON(outJSON :'Outfact03':%TrimR(fact03));
       BuildJSON(outJSON :'Outfact04':%TrimR(fact04));
       BuildJSON(outJSON :'Outfact05':%TrimR(fact05));
       BuildJSON(outJSON :'Outfact06':%TrimR(fact06));
       BuildJSON(outJSON :'Outfact07':%TrimR(fact07));
       BuildJSON(outJSON :'Outfact08':%TrimR(fact08));
       BuildJSON(outJSON :'Outfact09':%TrimR(fact09));
       BuildJSON(outJSON :'Outfact10':%TrimR(fact10));
       BuildJSON(outJSON :'Outfact11':%TrimR(fact11));
       BuildJSON(outJSON :'Outfact12':%TrimR(fact12));
       BuildJSON(outJSON :'Outfact13':%TrimR(fact13));
       BuildJSON(outJSON :'Outfact14':%TrimR(fact14));
       BuildJSON(outJSON :'Outfact15':%TrimR(fact15));
       BuildJSON(outJSON :'Outfact16':%TrimR(fact16));
       BuildJSON(outJSON :'Outfact17':%TrimR(fact17));
       BuildJSON(outJSON :'Outfact18':%TrimR(fact18));
       BuildJSON(outJSON :'Outfact19':%TrimR(fact19));
       BuildJSON(outJSON :'Outfact20':%TrimR(fact20));
       BuildJSON(outJSON :'Outfact21':%TrimR(fact21));
       BuildJSON(outJSON :'Outfact22':%TrimR(fact22));
       BuildJSON(outJSON :'Outfact23':%TrimR(fact23));
       BuildJSON(outJSON :'Outfact24':%TrimR(fact24));
       BuildJSON(outJSON :'Outfact25':%TrimR(fact25));
       BuildJSON(outJSON :'Outfact26':%TrimR(fact26));
       BuildJSON(outJSON :'Outfact27':%TrimR(fact27));
       BuildJSON(outJSON :'Outfact28':%TrimR(fact28));
       BuildJSON(outJSON :'Outfact29':%TrimR(fact29));
       BuildJSON(outJSON :'Outfact30':%TrimR(fact30));
       BuildJSON(outJSON :'Outfact31':%TrimR(fact31));
       BuildJSON(outJSON :'Outfact32':%TrimR(fact32));
       BuildJSON(outJSON :'Outfact33':%TrimR(fact33));
       BuildJSON(outJSON :'Outfact34':%TrimR(fact34));
       BuildJSON(outJSON :'Outfact35':%TrimR(fact35));
       BuildJSON(outJSON :'Outfact36':%TrimR(fact36));
       BuildJSON(outJSON :'Outfact37':%TrimR(fact37));
       BuildJSON(outJSON :'Outfact38':%TrimR(fact38));
       BuildJSON(outJSON :'Outfact39':%TrimR(fact39));
       BuildJSON(outJSON :'Outfact40':%TrimR(fact40));
       BuildJSON(outJSON :'Outfact41':%TrimR(fact41));
       BuildJSON(outJSON :'Outfact42':%TrimR(fact42));
       BuildJSON(outJSON :'Outfact43':%TrimR(fact43));
       BuildJSON(outJSON :'Outfact44':%TrimR(fact44));
       BuildJSON(outJSON :'Outfact45':%TrimR(fact45));
       BuildJSON(outJSON :'Outfact46':%TrimR(fact46));
       BuildJSON(outJSON :'Outfact47':%TrimR(fact47));
       BuildJSON(outJSON :'Outfact48':%TrimR(fact48));
       BuildJSON(outJSON :'Outfact49':%TrimR(fact49));
       BuildJSON(outJSON :'Outfact50':%TrimR(fact50));

       //* Load Written Premium.
       BuildJSON(outJSON :'OutdecTA01':%EditC(decTA01:'K'));
       BuildJSON(outJSON :'OutdecTA02':%EditC(decTA02:'K'));
       BuildJSON(outJSON :'OutdecTA03':%EditC(decTA03:'K'));
       BuildJSON(outJSON :'OutdecTA04':%EditC(decTA04:'K'));
       BuildJSON(outJSON :'OutdecTA05':%EditC(decTA05:'K'));
       BuildJSON(outJSON :'OutdecTA06':%EditC(decTA06:'K'));
       BuildJSON(outJSON :'OutdecTA07':%EditC(decTA07:'K'));
       BuildJSON(outJSON :'OutdecTA08':%EditC(decTA08:'K'));
       BuildJSON(outJSON :'OutdecTA09':%EditC(decTA09:'K'));
       BuildJSON(outJSON :'OutdecTA10':%EditC(decTA10:'K'));
       BuildJSON(outJSON :'OutdecTA11':%EditC(decTA11:'K'));
       BuildJSON(outJSON :'OutdecTA12':%EditC(decTA12:'K'));
       BuildJSON(outJSON :'OutdecTA13':%EditC(decTA13:'K'));
       BuildJSON(outJSON :'OutdecTA14':%EditC(decTA14:'K'));
       BuildJSON(outJSON :'OutdecTA15':%EditC(decTA15:'K'));
       BuildJSON(outJSON :'OutdecTA16':%EditC(decTA16:'K'));
       BuildJSON(outJSON :'OutdecTA17':%EditC(decTA17:'K'));
       BuildJSON(outJSON :'OutdecTA18':%EditC(decTA18:'K'));
       BuildJSON(outJSON :'OutdecTA19':%EditC(decTA19:'K'));
       BuildJSON(outJSON :'OutdecTA20':%EditC(decTA20:'K'));
       BuildJSON(outJSON :'OutdecTA21':%EditC(decTA21:'K'));
       BuildJSON(outJSON :'OutdecTA22':%EditC(decTA22:'K'));
       BuildJSON(outJSON :'OutdecTA23':%EditC(decTA23:'K'));
       BuildJSON(outJSON :'OutdecTA24':%EditC(decTA24:'K'));
       BuildJSON(outJSON :'OutdecTA25':%EditC(decTA25:'K'));
       BuildJSON(outJSON :'OutdecTA26':%EditC(decTA26:'K'));
       BuildJSON(outJSON :'OutdecTA27':%EditC(decTA27:'K'));
       BuildJSON(outJSON :'OutdecTA28':%EditC(decTA28:'K'));
       BuildJSON(outJSON :'OutdecTA29':%EditC(decTA29:'K'));
       BuildJSON(outJSON :'OutdecTA30':%EditC(decTA30:'K'));
       BuildJSON(outJSON :'OutdecTA31':%EditC(decTA31:'K'));
       BuildJSON(outJSON :'OutdecTA32':%EditC(decTA32:'K'));
       BuildJSON(outJSON :'OutdecTA33':%EditC(decTA33:'K'));
       BuildJSON(outJSON :'OutdecTA34':%EditC(decTA34:'K'));
       BuildJSON(outJSON :'OutdecTA35':%EditC(decTA35:'K'));
       BuildJSON(outJSON :'OutdecTA36':%EditC(decTA36:'K'));
       BuildJSON(outJSON :'OutdecTA37':%EditC(decTA37:'K'));
       BuildJSON(outJSON :'OutdecTA38':%EditC(decTA38:'K'));
       BuildJSON(outJSON :'OutdecTA39':%EditC(decTA39:'K'));
       BuildJSON(outJSON :'OutdecTA40':%EditC(decTA40:'K'));
       BuildJSON(outJSON :'OutdecTA41':%EditC(decTA41:'K'));
       BuildJSON(outJSON :'OutdecTA42':%EditC(decTA42:'K'));
       BuildJSON(outJSON :'OutdecTA43':%EditC(decTA43:'K'));
       BuildJSON(outJSON :'OutdecTA44':%EditC(decTA44:'K'));
       BuildJSON(outJSON :'OutdecTA45':%EditC(decTA45:'K'));
       BuildJSON(outJSON :'OutdecTA46':%EditC(decTA46:'K'));
       BuildJSON(outJSON :'OutdecTA47':%EditC(decTA47:'K'));
       BuildJSON(outJSON :'OutdecTA48':%EditC(decTA48:'K'));
       BuildJSON(outJSON :'OutdecTA49':%EditC(decTA49:'K'));
       BuildJSON(outJSON :'OutdecTA50':%EditC(decTA50:'K'));

       rtnCode = ' ';
       Return;
      /End-Free

