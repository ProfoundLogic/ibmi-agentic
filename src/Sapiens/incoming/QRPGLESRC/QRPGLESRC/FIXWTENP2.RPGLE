      ** (C) Copyright Progressive Data Solutions, Inc., 1999
     h/title FIXWTENP2: Load the correct edition dates
      **============================================================**
      ** Program Description:                                       **
      **   This program check endorsements for incorrect edition    **
      **   dates and corrects them.                                 **
      **                                                            **
      ** Called from:                                               **
      **                                                            **
      ** Calls to:    None                                          **
      **                                                            **
      ** Programmer: C:113                                          **
      ** Created   : 10/06/99                                       **
      ** Project   : CM027                                          **
      **============================================================**
     hdatedit(*ymd) datfmt(*ymd)

     fwtenl     uf   e           k disk

     fqsysprt   o    f   80        printer oflind(*inoa)                        Output

      *=============================================================================================
      *  Field Definitions
      *=============================================================================================
      ** External data structure
     d psds          esds                  extname(szq1p)                       External prog info

     dxxWorkStor       ds
     d xxDate                         8s 0 inz
     d  xxYear                        4s 0 overlay(xxDATE:1)                    Year
     d  xxMonth                       2s 0 overlay(xxDATE:5)                    Month
     d  xxDay                         2s 0 overlay(xxDATE:7)                    Day

     d xxNew                          7  0 inz
     d  xxNewYear                     2  0 overlay(xxNew:2)
     d  xxNewMonth                    2  0 overlay(xxNew:4)
     d  xxNewDay                      2  0 overlay(xxNew:6)

     d xxOld                          7  0 inz
     d  xxOldYear                     2  0 overlay(xxOld:2)
     d  xxOldMonth                    2  0 overlay(xxOld:4)
     d  xxOldDay                      2  0 overlay(xxOld:6)

     d Forms           s             15a   Dim(138) CTDATA PerRcd(2)            Endorsement Forms

     d Idx             s              3  0 inz(1)
     d Idz             s              3  0 inz(0)
     d xxGreater       s              1
     d xxChanged       s              1    inz(' ')

      *=============================================================================================
      *  Main
      *=============================================================================================
     c                   Eval      xxDATE=*date

     c*******************Except    ##Header

     c                   Read      wtenl                                  60
     c                   DoW       *in60 = *off

     c                   Eval      Idx = 1
     c     enform        LookUp    Forms(Idx)                             20
     c                   If        *in20

     c                   Eval      Idz = Idx + 1
     c                   MoveL     Forms(Idz)    xxNew
     c                   If        eneddt <> xxNew

     c                   If        eneddt > xxNew
     c                   Eval      xxGreater = '*'
     c                   Else
     c                   Eval      xxGreater = ' '
     c                   EndIf

     c                   Eval      xxOld = eneddt
     c*******************Exsr      $PrtDet
     c                   Eval      eneddt = xxNew
     c                   Update    PEN01

     c                   If        xxChanged <> 'Y'
     c                   Eval      xxChanged = 'Y'
     c                   EndIf

     c                   Eval      xxNew = 0

     c                   EndIf                                                  *in20

     c                   EndIf                                                  eneddt <> Dates(Idx)

     c                   Read      wtenl                                  60
     c                   EndDo                                                  *in60 = *off

     c                   If        xxChanged <> 'Y'
     c*******************Except    ##None
     c                   EndIf

     c                   SetOn                                        lr
     c                   Return
      *=============================================================================================
      * $PrtDet - Print output
      *=============================================================================================
     c     $PrtDet       BEGSR
     c                   If        *inoa
     c                   Except    ##Header
     c                   Eval      *inoa = *off
     c                   EndIf

     c                   Except    ##Detail

     c                   ENDSR                                                  $PrtDet
      *=============================================================================================
      *  Printer Output
      *=============================================================================================
      ** Page header
     oqsysprt   e            ##Header       1  2
     o                       q1pgm               10
     o                                           50 'Correct Endorsement Editio'
     o                                           57 'n Dates'
     o                       q1user              80

     o          e            ##Header       2
     o                       xxMonth              2
     o                                            3 '/'
     o                       xxDay                5
     o                                            6 '/'
     o                       xxYear              10
     o                                           74 'Page'
     o                       PAGE          4     80

     o          e            ##Header       2
     o                                           27 '* = Old Date > Correct Dat+
     o                                              e'
     o          e            ##Header       2
     o                                            5 'Group'
     o                                           18 'Endorsement'
     o                                           32 'Old Date'
     o                                           46 'Correct Date'

     o          e            ##Detail       2
     o                       enfnd                3
     o                       enform              22
     o                       xxOldMonth          26
     o                                           27 '/'
     o                       xxOldDay            29
     o                                           30 '/'
     o                       xxOldYear           32

     o                       xxNewMonth          36
     o                                           37 '/'
     o                       xxNewDay            39
     o                                           40 '/'
     o                       xxNewYear           42
     o                       xxGreater           44

     o          e            ##None         2
     o                                           22 '*** No Records Changed'

**CTDATA Forms
WC000000A      0920401
WC000102       0840401
WC000106A      0920401
WC000301A      0890215
WC000302       0840401
WC000305       0840401
WC000308       0840401
WC000310       0840401
WC000311A      0910801
WC000313       0840401
WC000315       0850901
WC000319       0910401
WC000320A      0920225
WC000324       0950801
WC000401A      0860520
WC000402       0840401
WC000403       0840401
WC000404       0840401
WC000406       0840810
WC000407       0840401
WC000412       0900101
WC000414       0900701
WC000503A      0911201
WC010402       0961101
WC060301       0840401
WC060303B      0960401
WC060401       0960701
WC090402       0881001
WC090605       0900701
WC100601A      0930427
WC100602       0900701
WC120301       0840620
WC120302A      0980701
WC120402       0920901
WC120601B      0890601
WC130604       0920101
WC160601       0971215
WC160301       0970501
WC170301       0840701
WC170601A      0980101
WC210604       0840401
WC240301       0840401
WC240401       0900101
WC240403       0930603
WC240404       0930603
WC240406B      0980828
WC240501A      0911201
WC240601B      0960101
WC320301A      0931001
WC350601A      0901001
WC350602A      0941104
WC350404       0960701
WC350603       0931201
WC420301E      0970301
WC420305       0840701
WC420306       0921003
WC420308       0970101
WC420310       0970101
WC420402A      0970301
WC420403A      0970301
WC420501       0850101
WC420503       0850101
WC420515A      0910607
WC420601       0840701
WC420602       0920107
WC450602       0930701
WC480601B      0940401
WC480603A      0970409
