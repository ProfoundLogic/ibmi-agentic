     H copyright('Progressive Data Solutions, Inc 1999')
     DPgmStsDS       esds                  extname(szq1p)

     D@@pasWrd         s             28
     D@@serNum         s              7
     D@@expDate        s               d
     D@@entDate        s               d
     D@@days           s              5  0
     D@@error          s              1

     D##serialNo       s              8
     DserialNo7        s              7
     DdaysDiff         s              5  0
     D
     D orig            c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ+
     D                                     1234567890')
     D scramble        c                   const('DGJMPSVYBEHKNQTWZCFILORUXA+
     D                                     4703692581')
     D
     Dpassword         ds            28    inz
     D numExpDat                      8s 0
     D numEntDat                      8s 0
     D charDays                       5
     D serNumber                      7
     D
     D*=====================================================================
     D* Procedure prototypes
     D*=====================================================================
     DgetSerNum        PR                  extproc('SS011C')
     D serialNum                      8    Const
     D
     DCvtToDate        PR              d
     D numDate                        8  0 Const
     D
     DunJumble         PR            28
     D string                        28    Const

     C     *entry        plist
     C                   parm                    @@paswrd
     C                   parm                    @@serNum
     C                   parm                    @@expDate
     C                   parm                    @@entDate
     C                   parm                    @@days
     C                   parm                    @@error
     C*
     C                   movel     *blanks       @@serNum
     C                   move      *zeros        @@days
     C                   move      d'1900-01-01' @@expDate
     C                   move      d'1900-01-01' @@entDate
     C                   move      *off          @@error
     C*
     C****               eval      passWord = unjumble(@@pasWrd)
     C     scramble:orig xlate     passWord      passWord
     C
     C     *iso          test(d)                 numExpDat              99
     C  n99*iso          move      numExpDat     @@expDate
     C     *iso          test(d)                 numEntDat              99
     C  n99*iso          move      numEntDat     @@entDate
     C                   move      charDays      @@days
     C                   move      serNumber     @@serNum
     C                   if        *in99 = *off
     C                   exsr      errorCheck
     C                   endif
     C                   move      *in99         @@error
     C
     C                   seton                                        lr
     C
     C*=====================================================================
     C* errorCheck - check for errors in password
     C*=====================================================================
     C     errorCheck    begsr
     C                   callp     getSerNum(##serialNo)
     C                   move      ##serialNo    serialNo7
     C                   if        @@serNum <> serialNo7
     C                   seton                                        99
     C                   endif
     C
     C                   if        *in99 = *off
     C     @@expDate     subdur    @@entDate     daysDiff:*d
     C                   if        daysDiff <> @@days
     C                   seton                                        99
     C                   endif
     C                   endif
     C
     C                   if        *in99 = *off
     C                   if        cvtToDate(*date) > @@expDate
     C                   seton                                        99
     C                   endif
     C                   endif
     C
     C                   endsr
