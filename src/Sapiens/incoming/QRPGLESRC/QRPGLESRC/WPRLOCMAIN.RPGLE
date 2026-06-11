      *********************************************************************
      * REVISIONS:
      *
      *  01/09/15 C:283 Redmine: 4742 - V14R8
      *                 Added a new procedure setDspInd to the screen
      *                 to set the field indicators.
     *
      *********************************************************************
     p*=====================================================================
     P*=====================================================================
     d LocMaster       pr
     d  inRrn                         9
     d  inMode                        1
     d  inNext                       10
     d  inTabInd                      5    Options(*nopass)
     d  inQte                         1    Options(*nopass)

      *** beg add ******** 01/09/15 *****************************
     d setDspInd       pr
     d inQte                          1A
     d inCall                         3A
     d inMode                         1A
     d inDiv                          5  0

      *** end add ******** 01/09/15 *****************************
