     D*****************************************************************
     D* REVISIONS:
     D*
     D* 05/08/17 C:180 PROJECT: 93WLH/WC2491
     D*                ADD DATA STRUCTURE FOR WC000424 and WC370401
     D*                ENDORSEMENTS.
     D*
     D* 05/02/17 C:180 PROJECT: WC2493
     D*                ADD DATA STRUCTURE FOR WC040316 ENDORSEMENT.
     D*
     D* 04/20/17 C:180 PROJECT: WC2488
     D*                ADD DATA STRUCTURE FOR WC480315 ENDORSEMENT.
     D*
     D* 04/12/17 C:65  PROJECT: WC2483
     D*                ADD DATA STRUCTURE FOR WC200304 ENDORSEMENT.
     D*
     D* 07/22/16 C:136 Project: WC2450
     D*                Add data structure for CA Waiver
     D*                Charge (WC40306)
     D*
     D* 07/15/16 C:136 Project: WC2449
     D*                Add data structure for CA Voluntary
     D*                Charge (WC40305)
     D*
     D* 05/25/15 C:295 PROJECT: WC2332
     D*                Modify data structure E37310 to eliminate
     D*                the social security numbers fields (XXS#1 and XXS#2).
     D*
     D* 08/06/11 C:190 PROJECT: WC2062
     D*                Add new data structure E22304.
     D*
     I* 08/12/08 C:65  PROJECT: 146WAP
     I*                ADD DATA STRUCTURE FOR WC000412 ENDORSEMENT.
     I*
     I* 09/20/07 C:65  PROJECT: 114WGH
     I*                CHANGE SPECIAL STATE FIELD IN E00301 TO BE ALPHA.
     I*
     D* 10/13/06 C:65  PROJECT: 114WGH
     D*                ADD NEW FIELD TO WC000503 ENDORSEMENT STRUCTURE.
     D*
     D* 08/25/06 C:65  PROJECT: 114WGH
     D*                ADD IN A DATA STRUCTURE FOR THE WC290303 ENDR.
     D*
     D* 09/21/05 C:65  PROJECT: 114WGH
     D*                ADD IN A DATA STRUCTURE FOR THE WC290601A ENDR.
     D*
     D* 05/23/05 C:65  PROJECT: 114WAK
     D*                ADD IN A DATA STRUCTURE FOR THE WC370310B ENDR.
     D*
     D* 05/12/05 C:65  PROJECT: 132WAY
     D*                ADD IN A DATA STRUCTURE FOR THE WC040302A ENDR.
     D*                ADD IN A DATA STRUCTURE FOR THE WC040303 ENDR.
     D*
     D* 07/07/04 C:65  PROJECT: 93WDV
     D*                ADD IN A DATA STRUCTURE FOR THE WC000402 ENDR.
     D*
     D* 06/10/04 C:65  PROJECT: 114WGH/114WBK
     D*                ADD IN A DATA STRUCTURE FOR THE WC290409A ENDR.
     D*
     D*****************************************************************
     D                 DS
     D  E00407                 1    116
     D  XXEFD                  1      6  0
     D  XXSCP                  7     10  2
     D  XXLHP                 11     14  2
     D  XXSID                 15     15  0
     D  XXLID                 16     16  0
     D  XXNLH                 17     20  2
     D  XXCC1                 21     24  0
     D  XXEC1                 25     26  0
     D  XXRT1                 27     36  4
     D  XXCC2                 37     40  0
     D  XXEC2                 41     42  0
     D  XXRT2                 43     52  4
     D  XXCC3                 53     56  0
     D  XXEC3                 57     58  0
     D  XXRT3                 59     68  4
     D  XXCC4                 69     72  0
     D  XXEC4                 73     74  0
     D  XXRT4                 75     84  4
     D  XXCC5                 85     88  0
     D  XXEC5                 89     90  0
     D  XXRT5                 91    100  4
     D  XXCC6                101    104  0
     D  XXEC6                105    106  0
     D  XXRT6                107    116  4
     D                 DS
     D  E00106                 1    149
     D  XXS01                  1      2  0
     D  XXP01                  3      6  1
     D  XXS02                  7      8  0
     D  XXP02                  8     11  1
     D  XXS03                 12     13  0
     D  XXP03                 14     17  1
     D  XXS04                 18     19  0
     D  XXP04                 20     23  1
     D  XXS05                 24     25  0
     D  XXP05                 26     29  1
     D  XXS06                 30     31  0
     D  XXP06                 32     35  1
     D  XXS07                 36     37  0
     D  XXP07                 38     41  1
     D  XXS08                 42     43  0
     D  XXP08                 44     47  1
     D  XXS09                 48     49  0
     D  XXP09                 50     53  1
     D  XXS10                 54     55  0
     D  XXP10                 56     59  1
     D  XXS11                 60     61  0
     D  XXP11                 62     65  1
     D  XXS12                 66     67  0
     D  XXP12                 68     71  1
     D  XXS13                 72     73  0
     D  XXP13                 74     77  1
     D  XXS14                 78     79  0
     D  XXP14                 80     83  1
     D  XXS15                 84     85  0
     D  XXP15                 86     89  1
     D  XXS16                 90     91  0
     D  XXP16                 92     95  1
     D  XXS17                 96     97  0
     D  XXP17                 98    101  1
     D  XXS18                102    103  0
     D  XXP18                104    107  1
     D  XXS19                108    109  0
     D  XXP19                110    113  1
     D  XXS20                114    115  0
     D  XXP20                116    119  1
     D  XXS21                120    121  0
     D  XXP21                122    125  1
     D  XXS22                126    127  0
     D  XXP22                128    131  1
     D  XXS23                132    133  0
     D  XXP23                134    137  1
     D  XXS24                138    139  0
     D  XXP24                140    143  1
     D  XXS25                144    145  0
     D  XXP25                146    149  1
     D                 DS
     D  E00201                 1    152
     D  XPACC                  1     10  0
     D  XPDIS                 11     20  0
     D  XXTWM                 21     30  0
     D  XXDOW                 31    150
     D  X1SEQ                151    152  0
     D                 DS
     D  E00301                 1    166
     D  XXAEN                  1     60
     D  XXAEA                 61    112
     d*** BEGIN CHG ***  09/20/07  ******************************
     D**XXSSE***             113    114  0
     D  XXSSE                113    114
     d*** END CHG *****  09/20/07  ******************************
     D  XXCOP                115    164
     D  X2SEQ                165    166  0
     D                 DS
     D  E00302                 1    122
     D  XXLNC                  1    120
     D  X3SEQ                121    122  0
     D                 DS
     D  E00102                 1    102
     D  XXSCH                  1    100
     D  X4SEQ                101    102  0
     D                 DS
     D  E00311                 1    182
     D  XXIE                   1    100
     D  XXIST                101    140
     D  XXDWC                141    180
     D  X5SEQ                181    182  0
     D                 DS
     D  E00406                 1    185
     D  XXST1                  1      2  0
     D  XXDP1                  3     30  0
     D  X1DP1                  3      6  0
     D  X2DP1                  7      9  1
     D  X3DP1                 10     13  0
     D  X4DP1                 14     16  1
     D  X5DP1                 17     20  0
     D  X6DP1                 21     23  1
     D  X7DP1                 24     27  0
     D  X8DP1                 28     30  1
     D  XXAPD                 31     33  1
     D  XXST2                 34     35  0
     D  XXDP2                 36     63  0
     D  X1DP2                 36     39  0
     D  X2DP2                 40     42  1
     D  X3DP2                 43     46  0
     D  X4DP2                 47     49  1
     D  X5DP2                 50     53  0
     D  X6DP2                 54     56  1
     D  X7DP2                 57     60  0
     D  X8DP2                 61     63  1
     D  XXST3                 64     65  0
     D  XXDP3                 66     93  0
     D  X1DP3                 66     69  0
     D  X2DP3                 70     72  1
     D  X3DP3                 73     76  0
     D  X4DP3                 77     79  1
     D  X5DP3                 80     83  0
     D  X6DP3                 84     86  1
     D  X7DP3                 87     90  0
     D  X8DP3                 91     93  1
     D  XXST4                 94     95  0
     D  XXDP4                 96    123  0
     D  X1DP4                 96     99  0
     D  X2DP4                100    102  1
     D  X3DP4                103    106  0
     D  X4DP4                107    109  1
     D  X5DP4                110    113  0
     D  X6DP4                114    116  1
     D  X7DP4                117    120  0
     D  X8DP4                121    123  1
     D  XXST5                124    125  0
     D  XXDP5                126    153  0
     D  X1DP5                126    129  0
     D  X2DP5                130    132  1
     D  X3DP5                133    136  0
     D  X4DP5                137    139  1
     D  X5DP5                140    143  0
     D  X6DP5                144    146  1
     D  X7DP5                147    150  0
     D  X8DP5                151    153  1
     D  XXST6                154    155  0
     D  XXDP6                156    183  0
     D  X1DP6                156    159  0
     D  X2DP6                160    162  1
     D  X3DP6                163    166  0
     D  X4DP6                167    169  1
     D  X5DP6                170    173  0
     D  X6DP6                174    176  1
     D  X7DP6                177    180  0
     D  X8DP6                181    183  1
     D  X6SEQ                184    185  0
     D                 DS
     D  E00313                 1    120
     D  XXNOP                  1     60
     D  XXNOO                 61    120
     D                 DS
     D  E00310                 1    159
     D  XXDS1                  1      1
     D  XXNM1                  2     51
     D  X1ST1                 52     53  0
     D  XXDS2                 54     54
     D  XXNM2                 55    104
     D  X1ST2                105    106  0
     D  XXDS3                107    107
     D  XXNM3                108    157
     D  X1ST3                158    159  0
     D                 DS
     D  E00308                 1    184
     D  X1DS1                  1      1
     D  X1NM1                  2     61
     D  X1DS2                 62     62
     D  X1NM2                 63    123
     D  X1DS3                124    124
     D  X1NM3                125    184
     D                 DS
     D  E00401                 1    179
     D  X2ST1                  1      2  0
     D  XXAN1                  3     22
     D  XXSC1                 23     26  0
     D  XXMX1                 27     31  0
     D  XXEP1                 32     36  0
     D  X2ST2                 37     38  0
     D  XXAN2                 39     58
     D  XXSC2                 59     62  0
     D  XXMX2                 63     67  0
     D  XXEP2                 68     72  0
     D  X2ST3                 73     74  0
     D  XXAN3                 75     94
     D  XXSC3                 95     98  0
     D  XXMX3                 99    103  0
     D  XXEP3                104    107  0
     D  X2ST4                108    109  0
     D  XXAN4                110    129
     D  XXSC4                130    133  0
     D  XXMX4                134    138  0
     D  XXEP4                139    143  0
     D  X2ST5                144    145  0
     D  XXAN5                146    165
     D  XXSC5                166    169  0
     D  XXMX5                170    174  0
     D  XXEP5                175    179  0
     D                 DS
     D  E00315                 1    150
     D  XXFAW                  1     75
     D  XXDHW                 76    150
     D                 DS
     D  E00403                 1    150
     D  XXDAC                  1      9  0
     D  XXPRP                 10     14  3
     D                 DS
     D*** BEGIN CHG ***  10/13/06  ****************************************
     D*************                           1 119 E00503
     D  E00503                 1    120
     D*** END CHG *****  10/13/06  ****************************************
     D  XXLLIM                 1     11  2
     D  XXLCON                12     16  3
     D  XXTXST                17     21  3
     D  XXTXFE                22     26  3
     D  XXRDF1                27     28  2
     D  XXRDF2                29     30  2
     D  XXRDF3                31     32  2
     D  XXM050                33     37  3
     D  XXM100                38     42  3
     D  XXM150                43     47  3
     D  XXX050                48     52  3
     D  XXX100                53     57  3
     D  XXX150                58     62  3
     D  XXB050                63     67  3
     D  XXB100                68     72  3
     D  XXB150                73     77  3
     D  XXE050                78     87  0
     D  XXE100                88     97  0
     D  XXE150                98    107  0
     D  XXLFST               108    110  3
     D  XXLFFE               111    113  3
     D  XXREFD               114    119  0
     D*** BEGIN ADD ***  10/13/06  ****************************************
     D  XXHG                 120    120
     D*** END ADD *****  10/13/06  ****************************************
     D*** BEGIN ADD ***  06/10/04  ************************************
     D                 DS
     D  E00409                 1     71
     D  XXINOP                 1     60
     D  XXIEFP                61     66  0
     D  XXIPRP                67     71  2
     D*** END ADD *****  06/10/04  ************************************
     D*** BEGIN ADD ***  07/07/04  ************************************
     D                 DS
     D  E00402                 1      6  0
     D  X9ANDT                 1      6  0
     D*** END ADD *****  07/07/04  ************************************
     D*** BEGIN ADD ***  05/12/05  ************************************
     D                 DS
     D  E04302                 1    180
     D  X2NM1                  1     60
     D  X2NM2                 61    120
     D  X2NM3                121    180
     D                 DS
     D  E04303                 1    180
     D  X3NM1                  1     60
     D  X3NM2                 61    120
     D  X3NM3                121    180
     D*** END ADD *****  05/12/05  ************************************
      *** BEG ADD ***** 07/15/16 **************************************
     d                 ds
     d e04305                       180
     d  x5nm1                        60    overlay(e04305)
     d  x5nm2                        60    overlay(e04305:61)
     d  x5nm3                        60    overlay(e04305:121)
      *** END ADD ***** 07/15/16 **************************************
      *** BEG ADD ***** 07/22/16 **************************************
     d                 ds
     d e04306                       124
     d  x6nm1                        60    overlay(e04306)
     d  x6nm2                        60    overlay(e04306:61)
     d  x6pct                         4    overlay(e04306:121)
      *** END ADD ***** 07/22/16 **************************************
     D*** BEGIN ADD ***  05/23/05  ************************************
     D*** Beg Chg *** 05/25/15 ****************************************
     D***              DS
     D*** E37310                 1    127
     D*** XXCOR                  1      1
     D*** X4NM1                  2     41
     D*** XXS#1                 42     50  0
     D*** XXOF1                 51     59
     D*** XXPO1                 60     64  2
     D*** X4NM2                 65    104
     D*** XXS#2                105    113  0
     D*** XXOF2                114    122
     D*** XXPO2                123    127  2

     D                 DS                  INZ
     D  e37310
     D  xxCor                         1    overlay(e37310)
     D  x4Nm1                        40    overlay(e37310:*next)
     D  xxOf1                         9    overlay(e37310:*next)
     D  xxPo1                         5  2 overlay(e37310:*next)
     D  x4Nm2                        40    overlay(e37310:*next)
     D  xxOf2                         9    overlay(e37310:*next)
     D  xxPo2                         5  2 overlay(e37310:*next)
     D  x4Nm3                        40    overlay(e37310:*next)
     D  xxOf3                         9    overlay(e37310:*next)
     D  xxPo3                         5  2 overlay(e37310:*next)
     D*** End Chg *** 05/25/15 ****************************************
     D*** END ADD *****  05/23/05  ************************************
     D*** BEGIN ADD ***  09/21/05  ************************************
     D                 DS
     D  E29601                 1     30
     D  XXBIA                  1     10  0
     D  XXBID                 11     20  0
     D  XXABI                 21     30  0
     D*** END ADD *****  09/21/05  ************************************
     D*** BEGIN ADD ***  08/25/06  ************************************
     D                 DS
     D  E29303                 1    150
     D  XGNM1                  1     90
     D  XGAD1                 91    150
     I*** END ADD *****  08/25/06  ************************************
     I*** BEGIN ADD ***  08/12/08  ************************************
     d                 ds
     d e00412                        10
     d  x4edt                         6  0 overlay(e00412)
     d  x4mod                         4  3 overlay(e00412:7)
     I*** END ADD *****  08/12/08  ************************************
     d*** beg   add ***  08/06/11  ************************************
     d                 ds
     d e22304                  1    178
     d  xhnm1                  1     50
     d  xhadT                 51     51
     d  xhAD1                 52    111
     d  xhcty                112    141
     d  xhst                 142    143
     d  xhzip                144    152
     d  xhfut                153    154
     d  xhfei                155    163
     d  xhuin                164    178
     d*** end   add ***  08/06/11  ************************************
      *** begin add *** 04/12/17 **************************************
     d                 ds
     d e20304                       190
     d  x7cname                      80    overlay(e20304)
     d  x7cfein                       9    overlay(e20304:81)
     d  x7cadr                       60    overlay(e20304:90)
     d  x7ccity                      30    overlay(e20304:150)
     d  x7cst                         2    overlay(e20304:180)
     d  x7czip                        9    overlay(e20304:182)
      *** END ADD ***** 04/12/17 **************************************
      *** begin add *** 04/20/17 **************************************
     d                 ds
     d e48315                       170
     d  x8cname                      60    overlay(e48315)
     d  x8cadr                       60    overlay(e48315:61)
     d  x8ccity                      30    overlay(e48315:121)
     d  x8cst                         2    overlay(e48315:151)
     d  x8czip                        9    overlay(e48315:153)
     d  x8cfein                       9    overlay(e48315:162)
      *** END ADD ***** 04/20/17 **************************************
      *** Begin Add *** 05/02/17 **************************************
     d                 ds
     d e04316                       180
     d  x9cname                      79    overlay(e04316)
     d  x9cadr                       60    overlay(e04316:80)
     d  x9ccity                      30    overlay(e04316:140)
     d  x9cst                         2    overlay(e04316:170)
     d  x9czip                        9    overlay(e04316:172)
      *** End Add ***** 05/02/17 **************************************
      *** begin add *** 05/08/17 **************************************
     d                 ds
     d e00424                       168
     d  x0State1                      2    overlay(e00424)
     d  x0ANCbasis1                  50    overlay(e00424:3)
     d  x0ANCMult1                    4S 0 overlay(e00424:53)
     d  x0State2                      2    overlay(e00424:57)
     d  x0ANCbasis2                  50    overlay(e00424:59)
     d  x0ANCMult2                    4S 0 overlay(e00424:109)
     d  x0State3                      2    overlay(e00424:113)
     d  x0ANCbasis3                  50    overlay(e00424:115)
     d  x0ANCMult3                    4S 0 overlay(e00424:165)
     d                 ds
     d e37401                       168
     d  x37State1                     2    overlay(e37401)
     d  x37ANCbasis1                 50    overlay(e37401:3)
     d  x37ANCmult1                   4S 0 overlay(e37401:53)
     d  x37State2                     2    overlay(e37401:57)
     d  x37ANCbasis2                 50    overlay(e37401:59)
     d  x37ANCmult2                   4S 0 overlay(e37401:109)
     d  x37State3                     2    overlay(e37401:113)
     d  x37ANCbasis3                 50    overlay(e37401:115)
     d  x37ANCmult3                   4S 0 overlay(e37401:165)
      *** END ADD ***** 05/08/17 **************************************
