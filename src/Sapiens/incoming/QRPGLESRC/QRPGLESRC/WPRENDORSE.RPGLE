      *=====================================================
      * Revisions:
      * 10/28/20 C:079 Project: WC2671
      *                Add Parm to PrtPolEnd
      *                Add GetPolEndForm
      *=====================================================
     dPrtPolEnd        pr              n
     d inForm                        15
     d inCo#                          3
     d inFnd#                         3
     d inFyr                          3
     d inEmp#                         9
     d inDiv                          5
     D*** Beg Add *** 10/28/20 *********************************************
     d inPfDt                         7
     D*** End Add *** 10/28/20 *********************************************
     d inOutq                        10
     D*** Beg Add *** 10/28/20 *********************************************
     d outPrt                         1
     D*** End Add *** 10/28/20 *********************************************
       //***  Begin Add  ***  07/11/22  ************************
       //===================================================
     dRenewWaivers...
     d                 pr             1n
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inYearNew                      3  0 const
     d inYearOld                      3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5    const options(*nopass)

       //===================================================
     dRtvCurrentForm...
     d                 pr            15
     d inCo#                          3  0 const
     d inForm                        15    const
     d inSt                           2    const
     d inEffDt                        7  0 const
       //***  End   Add  ***  07/11/22  ************************

     dGetDivForWaiverType...
     d                 pr              n
     d inCo#                          3  0
     d inFnd                          3  0
     d inFyr                          3  0
     d inEmp                          9  0
     d inForm                        15
     d inRateState                    2
     d inWvrType                      1
     d outDiv                         5  0
     dGetWaiverInfo    pr              n
     d inCo#                          3  0
     d inFnd                          3  0
     d inState                        2
     d inEfDt                         7  0
     d outForm                       15    Options(*Nopass)
     d outPrtPgm                     10    Options(*Nopass)
     d outtAddInfPgm                 10    Options(*Nopass)
     d outBlkWvrDesc                500    Options(*Nopass)
     d inFyr                          3  0 Options(*Nopass)
     d inEmp                          9  0 Options(*Nopass)
     d inDiv                          5  0 Options(*Nopass)
     dIsIncludedState  pr              n
     d inCo#                          3  0
     d inForm                        10    Const
     d inState                        2    Const
     d inEfDt                         7  0 Const
     dUpdateBlkWvrDft  pr              n
     d inCo#                          3  0
     d inFnd                          3  0
     d inEmp                          9  0
     d inDiv                          5  0
     d inFyr                          3  0
     d inForm                        15
     d inDesc                       500
     D*** Beg Add *** 10/28/20 *********************************************
     dGetPolEnd        pr
     d inCo#                          3  0
     d inFnd                          3  0
     d inState                        2
     d inEfDt                         7  0
     d outForm                       15    Options(*Nopass)
     d outPrtPgm                     10    Options(*Nopass)
     d outtAddInfPgm                 10    Options(*Nopass)
     d outBlkWvrDesc                500    Options(*Nopass)
     d inFyr                          3  0 Options(*Nopass)
     d inEmp                          9  0 Options(*Nopass)
     d inDiv                          5  0 Options(*Nopass)
     D*** Beg Add *** 10/28/20 *********************************************
