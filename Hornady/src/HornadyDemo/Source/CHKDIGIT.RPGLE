     H COPYRIGHT('(C) Copyright Hornady Manufacturing Company 2004')
      **************************************************************************
      * APPLICATION: Miscellaneous                                             *
      *       TITLE: Check Digit Retrieval and Validation                      *
      *          BY: William Earl (MCA)                                        *
      *        DATE: June 2004                                                 *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------- *
      *  XX/XX/XXXX  XXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                                        *
      **************************************************************************

     Hnomain

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
      /COPY *LIBL/QPRPSRC,CHKDIGIT

     P/EJECT
      **************************************************************************
      * Validate modulus 10 check digit.                                       *
      *   Return values: 0=no error, 1=Error                                   *
      **************************************************************************
     P VldM10ChkDigit  B                   EXPORT

     D                 PI            10I 0
     D Number                        31P 0 CONST
     D SigDigits                      3P 0 CONST

      **************************************************************************
      * Define data structures.                                                *
      **************************************************************************
     D                 DS
     D ds_Number               1     31  0
     D ds_BaseNbr              1     30  0
     D ds_CheckDgt            31     31

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Error        S             10I 0
     D wk_SigDigits    S              3P 0

     C                   EVAL      wk_Error = *Zero

     C                   DO

     C                   EVAL      ds_Number = Number
     C                   EVAL      wk_SigDigits = SigDigits

     C                   IF        wk_SigDigits < 1  or  wk_SigDigits > 31
     C                   EVAL      wk_Error = 1
     C                   LEAVE
     C                   ENDIF                                                  wk_SigDigits<1 or

     C                   IF        RtvM10ChkDigit(ds_BaseNbr:wk_SigDigits-1) <>
     C                               ds_CheckDgt
     C                   EVAL      wk_Error = 1
     C                   LEAVE
     C                   ENDIF                                                  RtvM10ChkDgt(BaseNum

     C                   ENDDO

      *
      * Return the error code.
      *
     C                   RETURN    wk_Error

     P VldM10ChkDigit  E

     P/EJECT
      **************************************************************************
      * Retrieve modulus 10 check digit.                                       *
      *   Return values: check digit, "E"=Error                                *
      **************************************************************************
     P RtvM10ChkDigit  B                   EXPORT

     D                 PI             1A
     D BaseNbr                       31P 0 CONST
     D SigDigits                      3P 0 CONST

      **************************************************************************
      * Define data structures.                                                *
      **************************************************************************
     D                 DS
     D ds_BaseNbr              1     31  0
     D ds_BaseNbrX             1     31

     D                 DS
     D ds_Digit                1      1  0
     D ds_DigitX               1      1

     D                 DS
     D ds_AllDigits            1      3  0
     D ds_AllDigits3           3      3  0

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_ChkDigit     S              1A
     D wk_ChkDigitN    S              1P 0
     D wk_SumDigits    S              3P 0
     D wk_OddDigits    S              3P 0
     D wk_EvenDigits   S              3P 0
     D wk_AllDigits    S              3P 0
     D wk_SigDigits    S              3P 0
     D ptr             S              3P 0

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D Error           C                   'E'

     C                   EVAL      wk_ChkDigit = *Blank

     C                   DO

     C                   EVAL      ds_BaseNbr = BaseNbr
     C                   EVAL      wk_SigDigits = SigDigits

     C                   IF        wk_SigDigits < 1  or  wk_SigDigits > 31
     C                   EVAL      wk_ChkDigit = Error
     C                   LEAVE
     C                   ENDIF                                                  wk_SigDigits<1 or

     C                   EVAL      wk_SumDigits = *Zero
     C                   EVAL      ptr = 32 - wk_SigDigits
     C                   DOW       ptr < 32
     C                   EVAL      ds_DigitX = %subst(ds_BaseNbrX:ptr:1)
     C                   EVAL      wk_SumDigits = wk_SumDigits + ds_Digit
     C                   EVAL      ptr = ptr + 2
     C                   ENDDO                                                  ptr<=32
     C                   EVAL      wk_OddDigits = wk_SumDigits * 3

     C                   EVAL      wk_SumDigits = *Zero
     C                   EVAL      ptr = 33 - wk_SigDigits
     C                   DOW       ptr < 32
     C                   EVAL      ds_DigitX = %subst(ds_BaseNbrX:ptr:1)
     C                   EVAL      wk_SumDigits = wk_SumDigits + ds_Digit
     C                   EVAL      ptr = ptr + 2
     C                   ENDDO                                                  ptr<=32
     C                   EVAL      wk_EvenDigits = wk_SumDigits

     C                   EVAL      wk_AllDigits = wk_OddDigits + wk_EvenDigits

     C                   EVAL      ds_AllDigits = wk_AllDigits
     C                   IF        ds_AllDigits3 = *Zero
     C                   EVAL      wk_ChkDigitN = *Zero
     C                   ELSE
     C                   EVAL      wk_ChkDigitN = 10 - ds_AllDigits3
     C                   ENDIF                                                   ds_AllDigits3=*Zero
     C                   EVAL      ds_Digit = wk_ChkDigitN
     C                   EVAL      wk_ChkDigit = ds_DigitX

     C                   ENDDO

      *
      * Return the check digit.
      *
     C                   RETURN    wk_ChkDigit

     P RtvM10ChkDigit  E

     P/EJECT
