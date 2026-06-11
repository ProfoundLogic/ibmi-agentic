     H copyright('Progressive Data Solutions, Inc 2000')
     Fssypf     uf   e             disk

     Dreset            s              2
     D##serialNo       s              8
     DserialNo7        s              7
     DexpDate          s              8  0
     DscPasWrd         s             28
     DtestRerun        s              2

     DPgmStsDS       esds                  extname(szq1p)
     Dpassword         ds
     D  syal01                       10
     D  syal02                       10
     D  syal03                       10
     D  syal04                       10
     D*=====================================================================
     D* Procedure prototypes
     D*=====================================================================
     DCvtToDate        PR              d
     D numDate                        8  0 Const
     D
     DGetPasswrd       PR            28
     D SerNumber                      7    Const
     D ExpDate                         d   Const
     D CurDate                         d   Const
     D
     DgetSerNum        PR                  extproc('SS011C')
     D serialNum                      8    Const
     D

     C     *entry        plist
     C                   parm                    reset
      *
      *  Check if parameter passed. If parameter is equal to '99',
      *  then allow program to rerun, regardless of what is in the file.
      *
     C                   move      *blanks       testRerun
     C                   if        q1prms > 0
     C                   if        reset = '99'
     C                   eval      testRerun = '00'
     C                   endif
     C                   endif
      *
      *  If no override was passed into program, check the file to see
      *  this program has already been run.
      *
     C                   if        testRerun = *blanks
     C     1             chain(n)  ssypf                              99
     C                   if        *in99 = *off
     C                   move      syal03        testRerun
     C                   if        testRerun <> '99'
     C                   move      syal04        testRerun
     C                   endif
     C                   endif
     C                   endif
      *
      *  Only run the program if it has not been run before.
      *
     C                   if        testRerun <> '99'
     C                   callp     getSerNum(##serialNo)
     C                   move      ##serialNo    serialNo7
     C                   move      12312099      expDate
     C                   eval      scPasWrd = GetPassWrd(serialNo7 :
     C                             CvtToDate(expDate) :
     C                             CvtToDate(*DATE))
     C*
     C     1             chain     ssypf                              99
     C                   if        *in99 = *off
     C                   movel(p)  scPasWrd      passWord
     C                   move      '99'          syal04
     C                   update    ssy01
     C                   endif
     C                   endif
     C
     C                   seton                                        lr

