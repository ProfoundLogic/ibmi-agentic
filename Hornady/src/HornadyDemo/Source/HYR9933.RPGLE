     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2025')
      **************************************************************************
      * APPLICATION: Miscellaneous                                             *
      *       TITLE: Subprocedures - Phone number                              *
      *          BY: William Earl (MCA)                                        *
      *        DATE: April 2025                                                *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------- *
      *  XX/XX/XXXX  XXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                                        *
      **************************************************************************

     Hnomain

     FHYPPHON   IF   E           K DISK    USROPN

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
      /COPY *LIBL/QPRPSRC,HYR9933

     D EditW           PR            26A
     D  Number                       11S 0 CONST
     D  Mask                         15A   CONST
     D  MaskLen                       3P 0 CONST
     D  Country                       3A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************

     P/EJECT
      **************************************************************************
      * Format phone number.                                                   *
      *   Return values: Formatted phone number                                *
      **************************************************************************
     P FmtPhone        B                   EXPORT

     D                 PI            15A
     D Phone#                        11S 0 CONST
     D Country                        3A   CONST
     D FmtType                        1A   CONST                                1=Std, 2=E.164

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Phone#       S             11S 0
     D wk_Country      S              3A
     D wk_FmtType      S              1A
     D wk_FmtPhone     S             15A
     D wk_PCCLen       S              1P 0
     D wk_Phone#X      S             11A
     D wk_Phone##      S             11S 0
     D wk_CCChk1       S              1S 0
     D wk_CCChk2       S              2S 0
     D wk_CCChk3       S              3S 0
     D wk_FmtPhoneX    S             26A

     C                   EVAL      wk_Phone# = Phone#
     C                   EVAL      wk_Country = Country
     C                   EVAL      wk_FmtType = FmtType
     C                   EVAL      wk_FmtPhone = *Blanks

     C                   DO

     C                   IF        not %open(HYPPHON)
     C                   OPEN      HYPPHON
     C                   ENDIF                                                  not %open(HYPPHON)

     C                   IF        wk_Phone# = *Zero
     C                   LEAVE
     C                   ENDIF                                                  wk_Phone#=*Zero

     C                   IF        wk_Phone# < 100
     C                   EVAL      wk_FmtPhone = %triml(%editc(wk_Phone#:'Z'))
     C                   LEAVE
     C                   ENDIF                                                  wk_Phone#<100

     C                   IF        wk_Country = *Blanks
     C                   EVAL      wk_Country = 'DFT'
     C                   ENDIF                                                  wk_Country=*Blanks

     C     wk_Country    CHAIN     HYPPHON
     C                   IF        not %found(HYPPHON)
     C                   EVAL      wk_Country = 'DFT'
     C     wk_Country    CHAIN     HYPPHON
     C                   IF        not %found(HYPPHON)
     C                   EVAL      wk_FmtPhone = %triml(%editc(wk_Phone#:'Z'))
     C                   IF        wk_FmtType = '2'
     C                   EVAL      wk_FmtPhone = '+' + wk_FmtPhone
     C                   ENDIF                                                  wk_FmtType='2'
     C                   LEAVE
     C                   ENDIF                                                  not %found(HYPPHON)
     C                   ENDIF                                                  not %found(HYPPHON)

     C                   SELECT
     C                   WHEN      PFPCOD = *Zero
     C                   EVAL      wk_PCCLen = *Zero
     C                   WHEN      PFPCOD < 10
     C                   EVAL      wk_PCCLen = 1
     C                   WHEN      PFPCOD < 100
     C                   EVAL      wk_PCCLen = 2
     C                   OTHER
     C                   EVAL      wk_PCCLen = 3
     C                   ENDSL

     C                   EVAL      wk_Phone#X = %char(wk_Phone#)
     C                   EVAL      wk_CCChk1 = %dec(%subst(wk_Phone#X:1:1):1:0)
     C                   EVAL      wk_CCChk2 = %dec(%subst(wk_Phone#X:1:2):2:0)
     C                   EVAL      wk_CCChk3 = %dec(%subst(wk_Phone#X:1:3):3:0)

     C                   SELECT
     C                   WHEN      wk_PCCLen = 3  and  wk_CCChk3 = PFPCOD
     C                   EVAL      wk_Phone#X = %subst(wk_Phone#X:4:8)
     C                   WHEN      wk_PCCLen = 2  and  wk_CCChk2 = PFPCOD
     C                   EVAL      wk_Phone#X = %subst(wk_Phone#X:3:9)
     C                   WHEN      wk_PCCLen = 1  and  wk_CCChk1 = PFPCOD and
     C                              ((PFPCOD = 1  and                           US & Canada must
     C                                %len(%trimr(wk_Phone#X)) = 11) or          have 10 more digits
     C                                PFPCOD <> 1)
     C                   EVAL      wk_Phone#X = %subst(wk_Phone#X:2:10)
     C                   ENDSL

     C                   EVAL      wk_Phone## = %dec(%trimr(wk_Phone#X):11:0)

     C                   IF        PFPFMT = *Blanks  or
     C                               wk_FmtType = '2'
     C                   EVAL      wk_FmtPhone = %triml(%editc(wk_Phone##:'Z'))
     C                   SELECT
     C                   WHEN      wk_Country = 'IT'
     C                   IF        %len(%trimr(wk_FmtPhone)) = 9
     C                   EVAL      wk_FmtPhone = '0' + wk_FmtPhone
     C                   ENDIF                                                  %len(%trimr(wk_FmtPh
     C                   ENDSL
     C                   IF        PFPCOD <> *Zero
     C                   EVAL      wk_FmtPhone = '+' +
     C                               %triml(%editc(PFPCOD:'Z')) + wk_FmtPhone
     C                   ELSE
     C                   IF        wk_FmtType ='2'
     C                   EVAL      wk_FmtPhone = '+' + wk_FmtPhone
     C                   ENDIF                                                  wk_FmtType='2'
     C                   ENDIF                                                  PFPCOD<>*Zero
     C                   LEAVE
     C                   ENDIF                                                  PFPFMT=*Blanks or

     C                   EVAL      wk_FmtPhoneX = %triml(
     C                               editw(wk_Phone##:PFPFMT:PFPFLN:wk_Country))

     C                   IF        PFPCOD > *Zero
     C                   EVAL      wk_FmtPhoneX = '+' +
     C                               %triml(%editc(PFPCOD:'Z')) + ' ' +
     C                               wk_FmtPhoneX
     C                   ENDIF                                                  PFPCOD>*Zero

     C                   IF        %len(%trimr(wk_FmtPhoneX)) <= 15
     C                   EVAL      wk_FmtPhone = wk_FmtPhoneX
     C                   ELSE
     C                   EVAL      wk_FmtPhone = %triml(%editc(wk_Phone#:'Z'))
     C                   IF        PFPCOD > *Zero  and
     C                               %len(%trimr(wk_FmtPhone)) +
     C                                wk_PCCLen + 2 <= 15
     C                   EVAL      wk_FmtPhone = '+' +
     C                               %triml(%editc(PFPCOD:'Z')) + ' ' +
     C                               wk_FmtPhone
     C                   ENDIF                                                  PFPCOD>*Zero and
     C                   ENDIF                                                  %len(%trimr(wk_FmtPh

     C                   ENDDO

      *
      * Return the formatted phone number.
      *
     C                   RETURN    wk_FmtPhone

     P FmtPhone        E

     P/EJECT
      **************************************************************************
      * Edit number from a mask.                                               *
      *   Return values: Edited number                                         *
      **************************************************************************
     P EditW           B

     D                 PI            26A
     D Number                        11S 0 CONST
     D Mask                          15A   CONST
     D MaskLen                        3P 0 CONST
     D Country                        3A   CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Number       S             11S 0
     D wk_Mask         S             15A
     D wk_MaskLen      S              3P 0
     D wk_Country      S              3A
     D wk_EditedNbr    S             26A
     D wk_NumberX1     S             11A
     D wk_NumberX2     S             26A
     D wk_MaskChar     S              1A
     D ptr1a           S              3P 0
     D ptr1b           S              3P 0
     D ptr2            S              3P 0

     C                   EVAL      wk_Number = Number
     C                   EVAL      wk_Mask = Mask
     C                   EVAL      wk_MaskLen = MaskLen
     C                   EVAL      wk_Country = Country
     C                   EVAL      wk_EditedNbr = *Blanks

     C                   DO

     C                   EVAL      wk_NumberX1 = %char(wk_Number)
     C                   EVAL      wk_NumberX2 = *Blanks

     C                   SELECT
     C                   WHEN      wk_Country = 'IT'
     C                   IF        %len(%trimr(wk_NumberX1)) = 9
     C                   EVAL      wk_NumberX1 = '0' + wk_NumberX1
     C                   ENDIF                                                  %len(%trimr(wk_FmtPh
     C                   ENDSL

     C                   IF        wk_MaskLen = *Zero  or  wk_MaskLen > 15
     C                   EVAL      wk_MaskLen = 15
     C                   ENDIF                                                  wk_MaskLen=*Zero or

     C                   EVAL      ptr1b = 1
     C                   EVAL      ptr2 = *Zero
     C                   FOR       ptr1a = 1 to 11
     C                   IF        ptr2 > wk_MaskLen
     C                   LEAVE
     C                   ENDIF
     C                   EVAL      wk_MaskChar = %subst(wk_Mask:ptr1b:1)
     C                   IF        wk_MaskChar = *Blank
     C                   EVAL      ptr2 = ptr2 + 1
     C                   EVAL      %subst(wk_NumberX2:ptr2:1) =
     C                                %subst(wk_NumberX1:ptr1a:1)
     C                   EVAL      ptr1b = ptr1b + 1
     C                   ELSE
     C                   IF        wk_MaskChar = '&'
     C                   EVAL      wk_MaskChar = *Blank
     C                   ENDIF                                                  wk_MaskChar='&'
     C                   EVAL      ptr2 = ptr2 + 1
     C                   EVAL      %subst(wk_NumberX2:ptr2:1) = wk_MaskChar
     C                   EVAL      ptr2 = ptr2 + 1
     C                   EVAL      %subst(wk_NumberX2:ptr2:1) =
     C                                %subst(wk_NumberX1:ptr1a:1)
     C                   EVAL      ptr1b = ptr1b + 2
     C                   ENDIF                                                  wk_MaskChar=*Blank
     C                   ENDFOR                                                 ptr1a = 1 to 11

     C                   EVAL      wk_EditedNbr = wk_NumberX2

     C                   ENDDO

      *
      * Return the edited number.
      *
     C                   RETURN    wk_EditedNbr

     P EditW           E

     P/EJECT
      **************************************************************************
      * Shut down service program                                              *
      **************************************************************************
     P sd_HYR9933      B                   EXPORT

     D                 PI            10I 0

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_Error        S             10I 0

     C                   EVAL      wk_Error = *Zero

     C                   DO

     C                   IF        %open(HYPPHON)
     C                   CLOSE     HYPPHON
     C                   ENDIF                                                  %open(HYPPHON)

     C                   ENDDO

      *
      * Return the error code.
      *
     C                   RETURN    wk_Error

     P sd_HYR9933      E

     P/EJECT
