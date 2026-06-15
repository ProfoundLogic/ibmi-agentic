     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2006')
      **************************************************************************
      * APPLICATION: Miscellaneous                                             *
      *       TITLE: Subprocedures - Check Digit                               *
      *          BY: William Earl (MCA)                                        *
      *        DATE: November 2013                                             *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------- *
      *  02/04/2019  MCA  Add RtvM7ChkDgt for FedEx PRO number check digits.   *
      *                                                                        *
      **************************************************************************

     Hnomain

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
      /COPY *LIBL/QPRPSRC,HYR9937

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************

     P/EJECT
      **************************************************************************
      * Calculate a Mod 10 check digit.                                        *
      **************************************************************************
     P RtvM10ChkDgt    B                   EXPORT

     D                 PI             1A
     D  BaseNbr                      30A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_BaseNbr      S             30A
     D wk_ChkDgt       S              1A
     D wk_FldLen       S              3P 0
     D wk_Pos          S              3P 0
     D wk_DblPos       S              3P 0
     D wk_Digit        S              3P 0
     D wk_Sum          S              3P 0
     D wk_Minuend      S              3P 0
     D wk_Difference   S              1P 0

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************

      *
      * Calculate the check digit.
      *
     C                   EVAL      wk_BaseNbr = BaseNbr
     C                   EVAL      wk_ChkDgt = *Blank
     C                   EVAL      wk_FldLen = %len(%trimr(wk_BaseNbr))
     C                   EVAL      wk_Sum = *Zero

     C                   EVAL      wk_DblPos = wk_FldLen

     C                   FOR       wk_Pos = wk_FldLen downto 1
     C                   EVAL      wk_Digit = %dec(%subst(wk_BaseNbr:wk_Pos:1):
     C                                          3:0)
     C                   IF        wk_Pos = wk_DblPos
     C                   EVAL      wk_Digit = wk_Digit * 2
     C                   EVAL      wk_DblPos = wk_DblPos - 2
     C                   ENDIF                                                  wk_Pos=wk_DblPos
     C                   IF        wk_Digit < 10
     C                   EVAL      wk_Sum = wk_Sum + wk_Digit
     C                   ELSE
     C                   EVAL      wk_Sum = wk_Sum + (wk_Digit - 10) + 1
     C                   ENDIF                                                  wk_Digit<10
     C                   ENDFOR                                                 wk_Pos=wk_FldLen

     C                   EVAL      wk_Minuend = %div(wk_Sum:10) * 10
     C                   IF        %rem(wk_Sum:10) > *Zero
     C                   EVAL      wk_Minuend = wk_Minuend + 10
     C                   ENDIF                                                  %rem(wk_Sum:10)>
     C                   EVAL      wk_Difference = wk_Minuend - wk_Sum
     C                   EVAL      wk_ChkDgt = %editc(wk_Difference:'X')

      *
      * Return the calculated check digit.
      *
     C                   RETURN    wk_ChkDgt

     P RtvM10ChkDgt    E

     P/EJECT
      **************************************************************************
      * Calculate a Mod 7 check digit.                                         *
      *  NOTE: Written per FedEx Mod 7 check digit formula.                    *
      **************************************************************************
     P RtvM7ChkDgt     B                   EXPORT

     D                 PI             1A
     D  BaseNbr                      30A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_BaseNbr      S             30A
     D wk_ChkDgt       S              1A
     D wk_BaseNbrX     S             29P 0
     D wk_Remainder    S              1P 0

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D digits          C                   '0123456789'

     C                   DO

      *
      * Calculate the check digit.
      *
     C                   EVAL      wk_BaseNbr = BaseNbr
     C                   EVAL      wk_ChkDgt = *Blank

     C                   IF        %subst(wk_BaseNbr:30:1) <> *Blanks
     C                   LEAVE
     C                   ENDIF

     C                   IF        %check(digits:%trimr(wk_BaseNbr)) > *Zero
     C                   LEAVE
     C                   ENDIF                                                  %check(digits:%trimr

     C                   EVAL      wk_BaseNbrX = %dec(%trimr(wk_BaseNbr):29:0)
     C                   EVAL      wk_Remainder = %rem(wk_BaseNbrX:7)
     C                   EVAL      wk_ChkDgt = %char(wk_Remainder)

     C                   ENDDO

      *
      * Return the calculated check digit.
      *
     C                   RETURN    wk_ChkDgt

     P RtvM7ChkDgt     E

     P/EJECT
