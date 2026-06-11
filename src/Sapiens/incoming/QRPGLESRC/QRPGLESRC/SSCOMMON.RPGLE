     H copyright('Progressive Data Solutions, Inc 2000')
     H nomain
     h option(*nodebugio: *srcstmt)
     h debug(*yes)
     D*=====================================================================
     D* Procedure prototypes
     D*=====================================================================
      * Revisions
      *
      * 04/03/09  C:61  Project SY180
      *                 - Procedure 'chkPasswrd'
      *                     Add new optional 1A parm for current machine.
      *                 - subroutine 'errorCheck'
      *                     Skip the serial # check if new curMachine = 'N'
      *
      *=====================================================================
     DcenterStr        PR            80
     D string                        80    Const
     D length                         3  0 Const
     D
     DCvtToDate        PR              d
     D numDate                        8  0 Const
     D
     DGetPasswrd       PR            32
     D SerNumber                      7    Const
     D ExpDate                         d   Const
     D CurDate                         d   Const
     D Model                          4    Const options(*nopass)
     D
     DJumbleStr        PR           128    opdesc
     D string                       128    Const options(*varsize)
      ****** beg add ****** 04/03/09 ************************************
     D MaxLength                      3  0 Const options(*nopass)
      ****** end add ****** 04/03/09 ************************************
     D
     DunJumble         PR           128    opdesc
     D string                       128    Const options(*varsize)
      ****** beg add ****** 04/03/09 ************************************
     D MaxLength                      3  0 Const options(*nopass)
      ****** end add ****** 04/03/09 ************************************
     D
     DchkPasswrd       PR             1
     D inPasWrd                      32    Const
     D outSerNbr                      7
     D outExpDate                      d
     D outEntDate                      d
     D outDays                        5  0
     D outModel                       4
      ****** beg add ****** 04/03/09 ************************************
     D curMachine                     1    Const options(*nopass)
      ****** end add ****** 04/03/09 ************************************

      ****** beg del ****** 04/03/09 ************************************
     D*****getSerNum        PR                  extproc('SS011C')
     D***** serialNum                      8    Const
      ****** end del ****** 04/03/09 ************************************
     D
     DgetModel         PR                  extproc('SS012C')
     D model                          4    Const
     D
     D orig            c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ+
     D                                     1234567890')
     D scramble        c                   const('DGJMPSVYBEHKNQTWZCFILORUXA+
     D                                     4703692581')
     P*=====================================================================
     P* centerStr - centers a string within the specified number of
     P*             characters
     P*=====================================================================
     PcenterStr        B                   Export
     DcenterStr        PI            80
     D string                        80    Const
     D length                         3  0 Const
     D
     D trimString      s                   like(string)
     D newString       s                   like(string)
     D x               s                   like(length)
     C
     C                   eval      newString = *blanks
B001 C                   if        string <> *blanks
     C                   eval      trimString = %trim(string)
 001 C     ' '           checkr    trimString    x
B002 C                   if        length - x < *zero
 002 C                   eval      x = x - x
E002 C                   endif
 001 C                   eval      x = (length - x) / 2
 001 C                   cat(p)    trimString:x  newString
E001 C                   endif
     C
     C                   return    newString
     PcenterStr        E
     P
     P*=====================================================================
     P* cvtToDate - convert number to date data type
     P*=====================================================================
     PCvtToDate        B                   Export
     DCvtToDate        PI              d
     D numDate                        8  0 Const
     D
     D date1           s               d
     C*
     C                   if        numDate < 1000000
     C     *mdy          move      numDate       date1
     C                   else
     C     *usa          move      numDate       date1
     C                   endif
     C*
     C                   return    date1
     PCvtToDate        E
     P
     P*=====================================================================
     P* getPasswrd - determine password for a serial number, expiration
     P*              date, and current date
     P*=====================================================================
     PGetPasswrd       B                   Export
     DGetPasswrd       PI            32
     D serNumber                      7    Const
     D expDate                         d   Const
     D curDate                         d   Const
     D model                          4    Const options(*nopass)
     D
     D result32        s             32
     D numDate8        s              8  0
     D numOfDays       s              5  0
     D charExpDat      s              8
     D charCurDat      s              8
     D charNumDay      s              5
     C                   move      expDate       numDate8
     C                   move      numDate8      charExpDat
     C                   move      curDate       numDate8
     C                   move      numDate8      charCurDat
     C     expDate       subdur    curDate       numOfDays:*d
     C                   move      numOfDays     charNumDay
     C
     C                   if        %parms = 3
     C                   eval      result32 = charExpDat + charCurDat
     C                             + charNumDay + serNumber
     C                   else
     C                   eval      result32 = charExpDat + charCurDat
     C                             + charNumDay + serNumber + model
     C                   endif
     C     orig:scramble xlate     result32      result32
      ****** beg chg ****** 04/03/09 ************************************
     C******             return    jumbleStr(result32)
     C                   return    jumbleStr(result32 : 32)
      ****** end chg ****** 04/03/09 ************************************
     PGetPasswrd       E
     P
     P*=====================================================================
     P* jumbleStr - rearranges the characters in a string, returns a new
     P*             string
     P*=====================================================================
     PJumbleStr        B
     DJumbleStr        PI           128    opdesc
     D str                          128    Const options(*varsize)
      ****** beg add ****** 04/03/09 ************************************
     D MaxLength                      3  0 Const options(*nopass)
      ****** end add ****** 04/03/09 ************************************
     D
     D ix              s              3  0 inz(5)
     D ix2             s              3  0
     D length          s              3  0
      ****** beg add ****** 04/03/09 ************************************
     D maxLen          s              3  0
      ****** end add ****** 04/03/09 ************************************
     D                 ds
     D string                              like(str)
     D stringArr                      1    dim(128)
     D                                     overlay(string)
     D string2                             like(string)
     D jumbleArr                           like(stringArr)
     D                                     dim(128)
     D                                     overlay(string2)

      ****** beg add ****** 04/03/09 ************************************
     c                   if        %parms >= 2
     c                   eval      maxLen = MaxLength
     C     ' '           checkr    str:maxLen    length
     c                   else
     c                   eval      maxLen = 128
     C     ' '           checkr    str           length
     c                   endif
      *
      ****** end add ****** 04/03/09 ************************************
      ****** beg del ****** 04/03/09 ************************************
     C*****' '           checkr    str           length
      ****** end del ****** 04/03/09 ************************************
     C                   move      str           string
     C                   move      *blanks       jumbleArr
     C                   do        length        ix2
     C                   if        stringArr(ix) <> ' '
     C                   move      stringArr(ix) jumbleArr(ix2)
     C                   endif
     C                   add       3             ix
     C                   if        ix > length
     C                   sub       length        ix
     C                   endif
     C                   enddo
     C
     C                   return    string2
     PJumbleStr        E
     P
     P*=====================================================================
     P* unJumble - rearrange string to proper sequence
     P*=====================================================================
     PUnJumble         B
     DUnJumble         PI           128    opdesc
     D str                          128    Const options(*varsize)
      ****** beg add ****** 04/03/09 ************************************
     D MaxLength                      3  0 Const options(*nopass)
      ****** end add ****** 04/03/09 ************************************
     D
     D ix              s              3  0
     D ix2             s              3  0 inz(5)
     D length          s              3  0
      ****** beg add ****** 04/03/09 ************************************
     D maxLen          s              3  0
      ****** end add ****** 04/03/09 ************************************
     D                 ds
     D string                              like(str)
     D stringArr                      1    dim(128)
     D                                     overlay(string)
     D string2                             like(string)
     D jumbleArr                           like(stringArr)
     D                                     dim(128)
     D                                     overlay(string2)

      ****** beg add ****** 04/03/09 ************************************
     c                   if        %parms >= 2
     c                   eval      maxLen = MaxLength
     C     ' '           checkr    str:maxLen    length
     c                   else
     c                   eval      maxLen = 128
     C     ' '           checkr    str           length
     c                   endif
      *
      ****** end add ****** 04/03/09 ************************************
      ****** beg del ****** 04/03/09 ************************************
     C*****' '           checkr    str           length
      ****** end del ****** 04/03/09 ************************************
     C                   move      str           string
     C                   move      *blanks       jumbleArr
     C                   do        length        ix
     C                   move      stringArr(ix) jumbleArr(ix2)
     C                   add       3             ix2
     C                   if        ix2 > length
     C                   sub       length        ix2
     C                   endif
     C                   enddo
     C
     C                   return    string2
     PUnJumble         E
     P*=====================================================================
     P* chkPasswrd - checks password for validity; returns information
     P*=====================================================================
     PchkPasswrd       B                   Export
     DchkPasswrd       PI             1
     D inPasWrd                      32    Const
     D outSerNbr                      7
     D outExpDate                      d
     D outEntDate                      d
     D outDays                        5  0
     D outModel                       4
      ****** beg add ****** 04/03/09 ************************************
     D curMachine                     1    Const options(*nopass)
      ****** end add ****** 04/03/09 ************************************

     D##serialNo       s              8
     D##model          s              4
     DserialNo7        s              7
     DdaysDiff         s              5  0
     D
     Dpassword         ds            32    inz
     D numExpDat                      8s 0
     D numEntDat                      8s 0
     D charDays                       5
     D serNumber                      7
     D model                          4
     D
     C
     C                   movel     *blanks       outSerNbr
     C                   move      *zeros        outDays
     C                   move      d'1900-01-01' outExpDate
     C                   move      d'1900-01-01' outEntDate
     C*
     C                   if        %subst(inPasWrd:29:4) = '99  '
     C                   eval      password = %subst(inPasWrd:1:28)
     C                   else
     C                   eval      password = inPasWrd
     C                   endif
     C
      ****** beg chg ****** 04/03/09 ************************************
     C******             eval      passWord = unjumble(passWord)
     C                   eval      passWord = unjumble(passWord : 32)
      ****** end chg ****** 04/03/09 ************************************
     C     scramble:orig xlate     passWord      passWord
     C
     C     *iso          test(d)                 numExpDat              99
     C  n99*iso          move      numExpDat     outExpDate
     C     *iso          test(d)                 numEntDat              99
     C  n99*iso          move      numEntDat     outEntDate
     C                   move      charDays      outDays
     C                   move      serNumber     outSerNbr
     C                   move      model         outModel
     C                   if        *in99 = *off
     C                   exsr      errorCheck
     C                   endif
     C                   return    *in99
     C
     C*=====================================================================
     C* errorCheck - check for errors in password
     C*=====================================================================
     C     errorCheck    begsr
      ****** beg add ****** 04/03/09 ************************************
     C                   if        (%parms < 7) or (curMachine <> 'N')
     c                   call      'SS011C1'
     c                   parm                    ##serialNo
      ****** end add ****** 04/03/09 ************************************
      ****** beg del ****** 04/03/09 ************************************
     C**********         callp     getSerNum(##serialNo)
      ****** end del ****** 04/03/09 ************************************
     C                   move      ##serialNo    serialNo7
     C                   if        outSerNbr <> serialNo7
     C                   seton                                        99
     C                   endif
      ****** beg add ****** 04/03/09 ************************************
     C                   endif
      ****** end add ****** 04/03/09 ************************************
     C
     C                   if        *in99 = *off
     C     outExpDate    subdur    outEntDate    daysDiff:*d
     C                   if        daysDiff <> outDays
     C                   seton                                        99
     C                   endif
     C                   endif
     C
     C                   if        *in99 = *off
     C                   if        cvtToDate(*date) > outExpDate
     C                   seton                                        99
     C                   endif
     C                   endif
     C
     C                   if        *in99 = *off
     C                   if        outModel <> *blanks
     C                   callp     getModel(##model)
     C*********          if        outModel <> %triml(##model)
     C                   if        %trim(outModel) <> %trim(##model)
     C                   seton                                        99
     C                   endif
     C                   endif
     C                   endif
     C
     C                   endsr
     PchkPasswrd       E
