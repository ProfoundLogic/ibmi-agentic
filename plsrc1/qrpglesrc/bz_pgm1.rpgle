     FBZ_PGM1   CF   E             WORKSTN

     C                   Eval      *in60 = *on
     C                   Eval      *in61 = *on
     C                   Eval      *in62 = *on
     c                   Eval      message1 = 'Screen1 Error'
     C                   ExFmt     SCREEN1

     C                   Eval      *in80 = *on
     C                   Eval      *in81 = *on
     C                   Eval      *in82 = *on
     C                   Write     WINDOW
     c                   Eval      message3 = 'Popup Error'
     C                   ExFmt     POPUP

     C                   Call      'BZ_PGM2'

     C                   Eval      *in60 = *on
     C                   Eval      *in61 = *on
     C                   Eval      *in62 = *on
     c                   Eval      message1 = 'Screen1 Error'
     C                   ExFmt     SCREEN1

     C                   Eval      *in70 = *on
     C                   Eval      *in71 = *on
     C                   Eval      *in72 = *on
     c                   Eval      message2 = 'Screen2 Error'
     C                   ExFmt     SCREEN2

     C                   Call      'BZ_PGM2'

     C                   Eval      *in70 = *on
     C                   Eval      *in71 = *on
     C                   Eval      *in72 = *on
     c                   Eval      message2 = 'Screen2 Error'
     C                   ExFmt     SCREEN2

     C                   Eval      *inlr = *on
     C                   Return
