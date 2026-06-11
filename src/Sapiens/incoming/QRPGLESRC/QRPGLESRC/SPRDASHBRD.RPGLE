      *  07/16/24 C:340  Project: WC2972                              *
      *                  Change conBlockHgt to 195.                   *
      *                  Change conDiffHgt  to -50                    *
      *                  Change conDiffTop  to 40                     *
      ******************************************************************
      *** Beg Chg ***** 07/16/24 *** WC2972 ****************************
     d*** conBlockHgt      c                   185
     d*** conDiffHgt       c                    -40
     d*** conDiffTop       c                    30
     dconBlockHgt      c                   195
     dconDiffHgt       c                    -50
     dconDiffTop       c                    40
      *** End Chg ***** 07/16/24 *** WC2972 ****************************
     dconBlockWth      c                   290
     dconDiffLft       c                    10
     dconDiffWth       c                    -20
     dconFirstLft      c                    20
     dconFirstTop      c                   455
     dconTabDiffHgt    c                    60
     dconAddTopTab     c                    45
     dconSpacing       c                    10

     p*====================================================================
     p* Get the Widget Height
     p*====================================================================
     dGetWidgetHeight  pr             5  0
     d inType                        15    const
     d inSize                         5  0 const

     p*====================================================================
     p* Get the Widget Left posistion
     p*====================================================================
     dGetWidgetLeft    pr             5  0
     d inType                        15    const
     d inPos                          5  0 const

     p*====================================================================
     p* Get the Widget Top posistion
     p*====================================================================
     dGetWidgetTop     pr             5  0
     d inType                        15    const
     d inPos                          5  0 const

     p*====================================================================
     p* Get the Widget Width
     p*====================================================================
     dGetWidgetWidth   pr             5  0
     d inType                        15    const
     d inSize                         5  0 const
