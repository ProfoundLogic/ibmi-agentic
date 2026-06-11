      *****************************************************************
      * (C) Copyright Progressive Data Solutions, Inc., 2004          *
      *                                                               *
      *****************************************************************
      *   ADD ONE MONTH TO MONTHEND DATE FOR CREATING QFEUP RECORDS   *
      *   QFEUP records are used for Calendar Earned/Unearned Report  *
      *                                                               *
      * CREATED 09/05/2003 C:132 PROJECT: 129WCU                      *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      *  04/01/04  C: 132   Project: WC1100                           *
      *              - Add to Base to calculate one month from input  *
      *                parm date and then load it in second Date Parm *
      *                                                               *
      *****************************************************************
      *                                                               *
     D Datusa          S               D   DATFMT(*USA)
     D@@date           s              8
     D@@dat2           s              8
     Dnewdat           s              8  0
     D*
     Ddate1            s               d   datfmt(*iso)
     Ddate2            s               d   datfmt(*usa)
      *
     D                 DS
     D  Dsdttm                 1     12  0
     D  Dstime                 1      6  0
     D  Dsdate                 7     12  0
      *
     D PSDS          ESDS                  Extname(Szq1p)
      *
     C                   Eval      *Inlr = *Off
      *
     C     *Entry        Plist
     C                   Parm                    QQdate            8
     C                   Parm                    QQdat2            8
      *
     C                   Move      QQdate        TODATE            8 0
     C                   if        todate  < 1000000
     C     *mdy          test(d)                 TODATE                 99
     C  N99*mdy          move      TODATE        date1
     C                   else
     C     *usa          test(d)                 todate                 99
     C  N99*usa          move      todate        date1
     C                   endif
     C                   if        *in99 = *off
     C     date1         adddur    1:*m          date2                  99
     C                   if        *in99 = *off
     C                   move      date2         newdat
     C                   endif
     C                   endif
     C                   move      newdat        qqdat2
      *
     C                   Eval      *Inlr = *On
      *
      *****************************************************************
