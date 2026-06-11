      *****************************************************************
      * REVISIONS:
     D*
     D* 01/02/24 C:342 Project: WC2938                                *
     D*                WC2938 - WCPOLS ED Record Type                 *
     D*                                                               *
     D* 09/21/23 C:344 PROJECT: WC2923
     D*                Add new data structure for the EJ record type
     D*                data.
     D*
     D* 11/15/22 C:291 PROJECT: WC2891
     D*                Add data structure E00405 for form WC000405.
     D*
     D* 10/26/22 C:344 PROJECT: 103957
     D*                Add new data structure for the EI record type
     D*                data.
     D*
     D* 10/23/20 C:180 PROJECT: WC2752
     D*                Add field x8cprem to data structure E48315 for
     D*                WC480314 endorsement.
     D*
     D* 11/11/19 C:267 PROJECT: WC2695
     D*                ADD DATA STRUCTURE FOR WC370604 endorsement
     D*
     D* 02/27/19 C:291 PROJECT: WC2636
     D*                Change Class code wording field length to 30.
     D*
     D* 10/09/18 C:291 PROJECT: WC2636
     D*                Add data structure E04342DM and E04342DO for
     D*                form WC040342B.
     D*
     D* 08/20/18 C:267 PROJECT: WC2634
     D*                ADD DATA STRUCTURE FOR WC040315B endorsement
     D*
     D* 05/30/18 C:65  PROJECT: WC2607
     D*                Expand the E00403 data structure to have more
     D*                fields for deductible endorsements.  Expand
     D*                E29601 as well.
     D*
     D* 05/08/17 C:180 PROJECT: 93WLH/WC2491
     D*                ADD DATA STRUCTURE FOR WC000424 and WC370401
     D*                ENDORSEMENTS.
     D*
     D* 05/02/17 C:180 PROJECT: WC2493
     D*                ADD DATA STRUCTURE FOR WC040316 ENDORSEMENT.
     D*
     D* 04/21/17 C:180 PROJECT: WC2488
     D*                ADD DATA STRUCTURE FOR WC480315 ENDORSEMENT.
     D*
     D* 04/12/17 C:65  PROJECT: WC2483
     D*                ADD DATA STRUCTURE FOR WC200304 ENDORSEMENT.
     D*
     D* 07/22/16 C:136 Project: WC2450
     D*                Add data structure for CA Waiver
     D*                Charge (WC40306)
     D*
      * 07/15/16 C:136 Project: WC2449
      *                Add data structure for CA Vol Labor Chg
      *                (WC040305)
      *
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
     I* 10/13/06 C:65  PROJECT: 114WGH
     I*                ADD NEW FIELD TO WC000503 ENDORSEMENT STRUCTURE.
     I*
     I*
     I* 08/25/06 C:65  PROJECT: 114WGH
     I*                ADD IN A DATA STRUCTURE FOR THE WC290303 ENDR.
     I*
     I* 09/21/05 C:65  PROJECT: 114WGH
     I*                ADD IN A DATA STRUCTURE FOR THE WC290601A ENDR.
     I*
     I* 05/23/05 C:65  PROJECT: 114WAK
     I*                ADD IN A DATA STRUCTURE FOR THE WC370310B ENDR.
     I*
     I* 05/12/05 C:65  PROJECT: 132WAY
     I*                ADD IN A DATA STRUCTURE FOR THE WC040302A ENDR.
     I*                ADD IN A DATA STRUCTURE FOR THE WC040303 ENDR.
     I*
     I* 07/07/04 C:65  PROJECT: 93WDV (SY190 PRISS 207)
     I*          C:61  ADD IN A DATA STRUCTURE FOR THE WC000402 ENDR.
     I*
     I* 06/10/04 C:65  PROJECT: 114WGH/114WBK
     I*                ADD IN A DATA STRUCTURE FOR THE WC290409A ENDR.
     I*
     I*****************************************************************
     D                 ds
     D e00407                       116
     D  xxefd                         6  0 overlay(e00407)
     D  xxscp                         4  2 overlay(e00407:7)
     D  xxlhp                         4  2 overlay(e00407:11)
     D  xxsid                         1  0 overlay(e00407:15)
     D  xxlid                         1  0 overlay(e00407:16)
     D  xxnlh                         4  2 overlay(e00407:17)
     D  xxcc1                         4  0 overlay(e00407:21)
     D  xxec1                         2  0 overlay(e00407:25)
     D  xxrt1                        10  4 overlay(e00407:27)
     D  xxcc2                         4  0 overlay(e00407:37)
     D  xxec2                         2  0 overlay(e00407:41)
     D  xxrt2                        10  4 overlay(e00407:43)
     D  xxcc3                         4  0 overlay(e00407:53)
     D  xxec3                         2  0 overlay(e00407:57)
     D  xxrt3                        10  4 overlay(e00407:59)
     D  xxcc4                         4  0 overlay(e00407:69)
     D  xxec4                         2  0 overlay(e00407:73)
     D  xxrt4                        10  4 overlay(e00407:75)
     D  xxcc5                         4  0 overlay(e00407:85)
     D  xxec5                         2  0 overlay(e00407:89)
     D  xxrt5                        10  4 overlay(e00407:91)
     D  xxcc6                         4  0 overlay(e00407:101)
     D  xxec6                         2  0 overlay(e00407:105)
     D  xxrt6                        10  4 overlay(e00407:107)
     D                 ds
     D e00106                       149
     D  xxs01                         2  0 overlay(e00106)
     D  xxp01                         4  1 overlay(e00106:3)
     D  xxs02                         2  0 overlay(e00106:7)
     D  xxp02                         4  1 overlay(e00106:8)
     D  xxs03                         2  0 overlay(e00106:12)
     D  xxp03                         4  1 overlay(e00106:14)
     D  xxs04                         2  0 overlay(e00106:18)
     D  xxp04                         4  1 overlay(e00106:20)
     D  xxs05                         2  0 overlay(e00106:24)
     D  xxp05                         4  1 overlay(e00106:26)
     D  xxs06                         2  0 overlay(e00106:30)
     D  xxp06                         4  1 overlay(e00106:32)
     D  xxs07                         2  0 overlay(e00106:36)
     D  xxp07                         4  1 overlay(e00106:38)
     D  xxs08                         2  0 overlay(e00106:42)
     D  xxp08                         4  1 overlay(e00106:44)
     D  xxs09                         2  0 overlay(e00106:48)
     D  xxp09                         4  1 overlay(e00106:50)
     D  xxs10                         2  0 overlay(e00106:54)
     D  xxp10                         4  1 overlay(e00106:56)
     D  xxs11                         2  0 overlay(e00106:60)
     D  xxp11                         4  1 overlay(e00106:62)
     D  xxs12                         2  0 overlay(e00106:66)
     D  xxp12                         4  1 overlay(e00106:68)
     D  xxs13                         2  0 overlay(e00106:72)
     D  xxp13                         4  1 overlay(e00106:74)
     D  xxs14                         2  0 overlay(e00106:78)
     D  xxp14                         4  1 overlay(e00106:80)
     D  xxs15                         2  0 overlay(e00106:84)
     D  xxp15                         4  1 overlay(e00106:86)
     D  xxs16                         2  0 overlay(e00106:90)
     D  xxp16                         4  1 overlay(e00106:92)
     D  xxs17                         2  0 overlay(e00106:96)
     D  xxp17                         4  1 overlay(e00106:98)
     D  xxs18                         2  0 overlay(e00106:102)
     D  xxp18                         4  1 overlay(e00106:104)
     D  xxs19                         2  0 overlay(e00106:108)
     D  xxp19                         4  1 overlay(e00106:110)
     D  xxs20                         2  0 overlay(e00106:114)
     D  xxp20                         4  1 overlay(e00106:116)
     D  xxs21                         2  0 overlay(e00106:120)
     D  xxp21                         4  1 overlay(e00106:122)
     D  xxs22                         2  0 overlay(e00106:126)
     D  xxp22                         4  1 overlay(e00106:128)
     D  xxs23                         2  0 overlay(e00106:132)
     D  xxp23                         4  1 overlay(e00106:134)
     D  xxs24                         2  0 overlay(e00106:138)
     D  xxp24                         4  1 overlay(e00106:140)
     D  xxs25                         2  0 overlay(e00106:144)
     D  xxp25                         4  1 overlay(e00106:146)
     D                 ds
     D e00201                       152
     D  xpacc                        10  0 overlay(e00201)
     D  xpdis                        10  0 overlay(e00201:11)
     D  xxtwm                        10  0 overlay(e00201:21)
     D  xxdow                       120    overlay(e00201:31)
     D  x1seq                         2  0 overlay(e00201:151)
     D                 ds
     D e00301                       166
     D  xxaen                        60    overlay(e00301)
     D  xxaea                        52    overlay(e00301:61)
     d*** BEGIN CHG ***  09/20/07  ******************************
     D**xxsse******                   2  0 overlay(e00301:113)
     D  xxsse                         2    overlay(e00301:113)
     d*** END CHG *****  09/20/07  ******************************
     D  xxcop                        50    overlay(e00301:115)
     D  x2seq                         2  0 overlay(e00301:165)
     D                 ds
     D e00302                       122
     D  xxlnc                       120    overlay(e00302)
     D  x3seq                         2  0 overlay(e00302:121)
     D                 ds
     D e00102                       102
     D  xxsch                       100    overlay(e00102)
     D  x4seq                         2  0 overlay(e00102:101)
     D                 ds
     D e00311                       182
     D  xxie                        100    overlay(e00311)
     D  xxist                        40    overlay(e00311:101)
     D  xxdwc                        40    overlay(e00311:141)
     D  x5seq                         2  0 overlay(e00311:181)
     D                 ds
     D e00406                       185
     D  xxst1                         2  0 overlay(e00406)
     D  xxdp1                        28  0 overlay(e00406:3)
     D   x1dp1                        4  0 overlay(xxdp1)
     D   x2dp1                        3  1 overlay(xxdp1:5)
     D   x3dp1                        4  0 overlay(xxdp1:8)
     D   x4dp1                        3  1 overlay(xxdp1:12)
     D   x5dp1                        4  0 overlay(xxdp1:15)
     D   x6dp1                        3  1 overlay(xxdp1:19)
     D   x7dp1                        4  0 overlay(xxdp1:22)
     D   x8dp1                        3  1 overlay(xxdp1:26)
     D  xxapd                         3  1 overlay(e00406:31)
     D  xxst2                         2  0 overlay(e00406:34)
     D  xxdp2                        28  0 overlay(e00406:36)
     D   x1dp2                        4  0 overlay(xxdp2)
     D   x2dp2                        3  1 overlay(xxdp2:5)
     D   x3dp2                        4  0 overlay(xxdp2:8)
     D   x4dp2                        3  1 overlay(xxdp2:12)
     D   x5dp2                        4  0 overlay(xxdp2:15)
     D   x6dp2                        3  1 overlay(xxdp2:19)
     D   x7dp2                        4  0 overlay(xxdp2:22)
     D   x8dp2                        3  1 overlay(xxdp2:26)
     D  xxst3                         2  0 overlay(e00406:64)
     D  xxdp3                        28  0 overlay(e00406:66)
     D   x1dp3                        4  0 overlay(xxdp3)
     D   x2dp3                        3  1 overlay(xxdp3:5)
     D   x3dp3                        4  0 overlay(xxdp3:8)
     D   x4dp3                        3  1 overlay(xxdp3:12)
     D   x5dp3                        4  0 overlay(xxdp3:15)
     D   x6dp3                        3  1 overlay(xxdp3:19)
     D   x7dp3                        4  0 overlay(xxdp3:22)
     D   x8dp3                        3  1 overlay(xxdp3:26)
     D  xxst4                         2  0 overlay(e00406:94)
     D  xxdp4                        28  0 overlay(e00406:96)
     D   x1dp4                        4  0 overlay(xxdp4)
     D   x2dp4                        3  1 overlay(xxdp4:5)
     D   x3dp4                        4  0 overlay(xxdp4:8)
     D   x4dp4                        3  1 overlay(xxdp4:12)
     D   x5dp4                        4  0 overlay(xxdp4:15)
     D   x6dp4                        3  1 overlay(xxdp4:19)
     D   x7dp4                        4  0 overlay(xxdp4:22)
     D   x8dp4                        3  1 overlay(xxdp4:26)
     D  xxst5                         2  0 overlay(e00406:124)
     D  xxdp5                        28  0 overlay(e00406:126)
     D   x1dp5                        4  0 overlay(xxdp5)
     D   x2dp5                        3  1 overlay(xxdp5:5)
     D   x3dp5                        4  0 overlay(xxdp5:8)
     D   x4dp5                        3  1 overlay(xxdp5:12)
     D   x5dp5                        4  0 overlay(xxdp5:15)
     D   x6dp5                        3  1 overlay(xxdp5:19)
     D   x7dp5                        4  0 overlay(xxdp5:22)
     D   x8dp5                        3  1 overlay(xxdp5:26)
     D  xxst6                         2  0 overlay(e00406:154)
     D  xxdp6                        28  0 overlay(e00406:156)
     D   x1dp6                        4  0 overlay(xxdp6)
     D   x2dp6                        3  1 overlay(xxdp6:5)
     D   x3dp6                        4  0 overlay(xxdp6:8)
     D   x4dp6                        3  1 overlay(xxdp6:12)
     D   x5dp6                        4  0 overlay(xxdp6:15)
     D   x6dp6                        3  1 overlay(xxdp6:19)
     D   x7dp6                        4  0 overlay(xxdp6:22)
     D   x8dp6                        3  1 overlay(xxdp6:26)
     D  x6seq                         2  0 overlay(e00406:184)
     D                 ds
     D e00313                       120
     D  xxnop                        60    overlay(e00313)
     D  xxnoo                        60    overlay(e00313:61)
     D                 ds
     D e00310                       159
     D  xxds1                         1    overlay(e00310)
     D  xxnm1                        50    overlay(e00310:2)
     D  x1st1                         2  0 overlay(e00310:52)
     D  xxds2                         1    overlay(e00310:54)
     D  xxnm2                        50    overlay(e00310:55)
     D  x1st2                         2  0 overlay(e00310:105)
     D  xxds3                         1    overlay(e00310:107)
     D  xxnm3                        50    overlay(e00310:108)
     D  x1st3                         2  0 overlay(e00310:158)
     D                 ds
     D e00308                       184
     D  x1ds1                         1    overlay(e00308)
     D  x1nm1                        60    overlay(e00308:2)
     D  x1ds2                         1    overlay(e00308:62)
     D  x1nm2                        61    overlay(e00308:63)
     D  x1ds3                         1    overlay(e00308:124)
     D  x1nm3                        60    overlay(e00308:125)
     D                 ds
     D e00401                       179
     D  x2st1                         2  0 overlay(e00401)
     D  xxan1                        20    overlay(e00401:3)
     D  xxsc1                         4  0 overlay(e00401:23)
     D  xxmx1                         5  0 overlay(e00401:27)
     D  xxep1                         5  0 overlay(e00401:32)
     D  x2st2                         2  0 overlay(e00401:37)
     D  xxan2                        20    overlay(e00401:39)
     D  xxsc2                         4  0 overlay(e00401:59)
     D  xxmx2                         5  0 overlay(e00401:63)
     D  xxep2                         5  0 overlay(e00401:68)
     D  x2st3                         2  0 overlay(e00401:73)
     D  xxan3                        20    overlay(e00401:75)
     D  xxsc3                         4  0 overlay(e00401:95)
     D  xxmx3                         5  0 overlay(e00401:99)
     D  xxep3                         4  0 overlay(e00401:104)
     D  x2st4                         2  0 overlay(e00401:108)
     D  xxan4                        20    overlay(e00401:110)
     D  xxsc4                         4  0 overlay(e00401:130)
     D  xxmx4                         5  0 overlay(e00401:134)
     D  xxep4                         5  0 overlay(e00401:139)
     D  x2st5                         2  0 overlay(e00401:144)
     D  xxan5                        20    overlay(e00401:146)
     D  xxsc5                         4  0 overlay(e00401:166)
     D  xxmx5                         5  0 overlay(e00401:170)
     D  xxep5                         5  0 overlay(e00401:175)
     D                 ds
     D e00315                       150
     D  xxfaw                        75    overlay(e00315)
     D  xxdhw                        75    overlay(e00315:76)
     D                 ds
     D e00403                       150
     D  xxdac                         9  0 overlay(e00403)
     D  xxprp                         5  3 overlay(e00403:10)
     D*** begin add ***  05/30/18  **************************************
     D  xxdtype                       4  0 overlay(e00403:15)
     D  xxlsd                         2  0 overlay(e00403:15)
     D  xxbdc                         2  0 overlay(e00403:17)
     D  xxdpc                         2  0 overlay(e00403:19)
     D  xxdag                         9  0 overlay(e00403:21)
     D  xxalei                        1    overlay(e00403:30)
     D  xxfeci                        1    overlay(e00403:31)
     D*** end add *****  05/30/18  **************************************
     D                 ds
     d*** begin chg ***  10/13/06  ******************************************************
     D********** e00503                       119
     D e00503                       120
     d*** end chg *****  10/13/06  ******************************************************
     D  xxllim                       11  2 overlay(e00503)
     D  xxlcon                        5  3 overlay(e00503:12)
     D  xxtxst                        5  3 overlay(e00503:17)
     D  xxtxfe                        5  3 overlay(e00503:22)
     D  xxrdf1                        2  2 overlay(e00503:27)
     D  xxrdf2                        2  2 overlay(e00503:29)
     D  xxrdf3                        2  2 overlay(e00503:31)
     D  xxm050                        5  3 overlay(e00503:33)
     D  xxm100                        5  3 overlay(e00503:38)
     D  xxm150                        5  3 overlay(e00503:43)
     D  xxx050                        5  3 overlay(e00503:48)
     D  xxx100                        5  3 overlay(e00503:53)
     D  xxx150                        5  3 overlay(e00503:58)
     D  xxb050                        5  3 overlay(e00503:63)
     D  xxb100                        5  3 overlay(e00503:68)
     D  xxb150                        5  3 overlay(e00503:73)
     D  xxe050                       10  0 overlay(e00503:78)
     D  xxe100                       10  0 overlay(e00503:88)
     D  xxe150                       10  0 overlay(e00503:98)
     D  xxlfst                        3  3 overlay(e00503:108)
     D  xxlffe                        3  3 overlay(e00503:111)
     D  xxrefd                        6  0 overlay(e00503:114)
     d*** begin add ***  10/13/06  ******************************************************
     D  xxhg                          1    overlay(e00503:120)
     d*** end add *****  10/13/06  ******************************************************
     I*** BEGIN ADD ***  06/10/04  ************************************
     D                 ds
     D e00409                        71
     D  xxinop                       60    overlay(e00409)
     D  xxiefp                        6  0 overlay(e00409:61)
     D  xxiprp                        5  2 overlay(e00409:67)
     I*** END ADD *****  06/10/04  ************************************
     I*** BEGIN ADD ***  07/07/04  ************************************
     D                 ds
     D  E00402                 1      6  0
     D  X9ANDT                 1      6  0
     I*** END ADD *****  07/07/04  ************************************
     I*** BEGIN ADD ***  05/12/05  ************************************
     D                 ds
     D e04302                       180
     D  x2nm1                        60    overlay(e04302)
     D  x2nm2                        60    overlay(e04302:61)
     D  x2nm3                        60    overlay(e04302:121)
     D                 ds
     D e04303                       180
     D  x3nm1                        60    overlay(e04303)
     D  x3nm2                        60    overlay(e04303:61)
     D  x3nm3                        60    overlay(e04303:121)
     I*** END ADD *****  05/12/05  ************************************
     I*** BEGIN ADD ***  05/23/05  ************************************
     d*** Beg Chg *****  05/25/15  ************************************
     d***                ds
     d***e37310                       127
     d*** xxcor                         1    overlay(e37310)
     d*** x4nm1                        40    overlay(e37310:2)
     d*** xxs#1                         9  0 overlay(e37310:42)
     d*** xxof1                         9    overlay(e37310:51)
     d*** xxpo1                         5  2 overlay(e37310:60)
     d*** x4nm2                        40    overlay(e37310:65)
     d*** xxs#2                         9  0 overlay(e37310:105)
     d*** xxof2                         9    overlay(e37310:114)
     d*** xxpo2                         5  2 overlay(e37310:123)

     d                 ds
     d e37310
     d  xxCor                         1    overlay(e37310)
     d  x4Nm1                        40    overlay(e37310:*next)
     d  xxOf1                         9    overlay(e37310:*next)
     d  xxPo1                         5  2 overlay(e37310:*next)
     d  x4Nm2                        40    overlay(e37310:*next)
     d  xxOf2                         9    overlay(e37310:*next)
     d  xxPo2                         5  2 overlay(e37310:*next)
     d  x4Nm3                        40    overlay(e37310:*next)
     d  xxOf3                         9    overlay(e37310:*next)
     d  xxPo3                         5  2 overlay(e37310:*next)
     d*** End Chg *****  05/25/15  ************************************
     d*** END ADD *****  05/23/05  ************************************
     I*** BEGIN ADD ***  09/21/05  ************************************
     d                 ds
     d*** begin chg ***  05/30/18  ************************************
     d***** e29601                        30
     d e29601                        70
     d*** end chg *****  05/30/18  ************************************
     d  xxbia                        10  0 overlay(e29601)
     d  xxbid                        10  0 overlay(e29601:11)
     d  xxabi                        10  0 overlay(e29601:21)
     d*** begin add ***  05/30/18  ************************************
     d  xxbiap                       10  0 overlay(e29601:31)
     d  xxbidp                       10  0 overlay(e29601:41)
     d  xxagpo                       10  0 overlay(e29601:51)
     d  xxagpp                       10  0 overlay(e29601:61)
     d*** end add *****  05/30/18  ************************************
     I*** END ADD *****  09/21/05  ************************************
     I*** BEGIN ADD ***  08/25/06  ************************************
     d                 ds
     d e29303                       150
     d  xgnm1                        90    overlay(e29303)
     d  xgad1                        60    overlay(e29303:91)
     I*** END ADD *****  08/25/06  ************************************
     I*** BEGIN ADD ***  08/12/08  ************************************
     d                 ds
     d e00412                        10
     d  x4edt                         6  0 overlay(e00412)
     d  x4mod                         4  3 overlay(e00412:7)
     I*** END ADD *****  08/12/08  ************************************
     D*** BEGIN ADD ***  08/06/11  ************************************
     D                 DS
     D  E22304                 1    178
     D  XHNM1                  1     50
     D  XHADT                 51     51  0
     D  XHAD1                 52    111
     D  XHCTY                112    141
     D  XHST                 142    143
     D  XHZIP                144    152
     D  XHFUT                153    154
     D  XHFEI                155    163  0
     D  XHUIN                164    178
     D*** END   ADD ***  08/06/11  ************************************
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
      *** Begin Add *** 04/21/17 **************************************
     d                 ds
      *** Beg Chg ***** 10/23/20 **************************************
     d*e48315                       170
     d e48315                       180
      *** End Chg ***** 10/23/20 **************************************
     d  x8cname                      60    overlay(e48315)
     d  x8cadr                       60    overlay(e48315:61)
     d  x8ccity                      30    overlay(e48315:121)
     d  x8cst                         2    overlay(e48315:151)
     d  x8czip                        9    overlay(e48315:153)
     d  x8cfein                       9    overlay(e48315:162)
      *** Beg Add ***** 10/23/20 **************************************
     d  x8cprem                      10S 0 overlay(e48315:171)
      *** End Add ***** 10/23/20 **************************************
      *** End Add ***** 04/21/17 **************************************
      *** Begin Add *** 01/02/24 **************************************
     d                 ds
     d e48316                       184
     d  x10EmpLname                  60    overlay(e48316)
     d  x10Cltname                   60    overlay(e48316:61)
     d  x10TEffDt                     6S 0 overlay(e48316:121)
     d  x10EntRcvF                   52    overlay(e48316:127)
     d  x10DtSent                     6S 0 overlay(e48316:179)
      *** End Add ***** 01/02/24 **************************************
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
     d*** Beg Add *****  08/20/18  ************************************
     D                 ds
     D e04315                       182
     D  xkNmCl                       79    overlay(e04315)
     D  xkadr1                       60    overlay(e04315:*next)
     D  xkadrcty                     30    overlay(e04315:*next)
     D  xkadrstat                     2    overlay(e04315:*next)
     D  xkadrzip                      9    overlay(e04315:*next)
     D  xkseq#                        2    overlay(e04315:*next)
     d*** End Add *****  08/20/18  ************************************
     d*** Beg Add *****  10/09/18  ************************************
     D                 ds
     D*** Beg Chg ***** 02/27/19 ************************************
     D*** e04342DM                      70
     D e04342DM                      80
     D*** End Chg ***** 02/27/19 ************************************
     D  xmNamOp                      40    overlay(e04342DM)
     D  xmClsCd                       4    overlay(e04342DM:*next)
     D  xmClsWSf                      2    overlay(e04342DM:*next)
     D*** Beg Chg ***** 02/27/19 ************************************
     D**xmClsWrd                     20    overlay(e04342DM:*next)
     D  xmClsWrd                     30    overlay(e04342DM:*next)
     D*** End Chg ***** 02/27/19 ************************************
     D  xmEndSeq#                     2    overlay(e04342DM:*next)
     D  xmEndSer#                     2    overlay(e04342DM:*next)
     D                 ds
     D e04342DO                     131
     D  xoINam                       60    overlay(e04342DO)
     D  xoIRCod                       5    overlay(e04342DO:*next)
     D  xoIRNam                      30    overlay(e04342DO:*next)
     D  xoAltPol#                    18    overlay(e04342DO:*next)
     D  xoAltInDt                     6S 0 overlay(e04342DO:*next)
     D  xoAltExdt                     6S 0 overlay(e04342DO:*next)
     D  xoLawInsI                     1    overlay(e04342DO:*next)
     D  xoWrtAfoI                     1    overlay(e04342DO:*next)
     D  xoEndSeq#                     2    overlay(e04342DO:*next)
     D  xoEndSer#                     2    overlay(e04342DO:*next)
     d*** End Add *****  10/09/18  ************************************
      *
     d*** Beg Add *****  11/11/19  ************************************
     D                 ds
     D e37604BJ                      15
     D  X37604EAF                     5S 4 overlay(e37604BJ)
     D  X37604EAA                    10S 2 overlay(e37604BJ:*next)
     d*** End Add *****  11/11/19  ************************************
      *
     d*** Begin Add *****  10/26/22  103957 *****************************
     D                 ds
     D e48322                       184
     d  xiCName                      60    overlay(e48322)
     d  xiCPol#                      18    overlay(e48322:61)
     d  xiCFein                       9    overlay(e48322:79)
     d  xiCAdr                       56    overlay(e48322:88)
     d  xiCCity                      30    overlay(e48322:144)
     d  xiCSt                         2    overlay(e48322:174)
     d  xiCZip                        9    overlay(e48322:176)
     d*** End   Add *****  10/26/22  103957 *****************************
     d*** Beg Add ***** 11/15/22 ********************************
     D                 ds
     D e00405                        36
     D  x5EfDt1                       6    overlay(e00405)
     D  x5ExDt1                       6    overlay(e00405:*next)
     D  x5EfDt2                       6    overlay(e00405:*next)
     D  x5ExDt2                       6    overlay(e00405:*next)
     D  x5EfDt3                       6    overlay(e00405:*next)
     D  x5ExDt3                       6    overlay(e00405:*next)
     D*** End Add ***** 11/15/22 ********************************
     d*** Begin Add ***** 09/21/23 *** 103957 *****************************
     d                 ds
     d e48323                       180
     d  xjCName                      90    overlay(e48323)
     d  xjCAdr                       80    overlay(e48323:91)
     d  xjCCnlc                       2s 0 overlay(e48323:171)
     d  xjCCnld                       8S 0 overlay(e48323:173)
     d*** End   Add *****  09/21/23 *** 103957 **************************
