     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2006')
      **************************************************************************
      * APPLICATION: Miscellaneous                                             *
      *       TITLE: Subprocedures - Text Manipulation                         *
      *          BY: William Earl (MCA)                                        *
      *        DATE: December 2006                                             *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------- *
      *  10/03/2007  MCA  Add CvtToLower subprocedure.                         *
      *                                                                        *
      **************************************************************************

     Hnomain

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
      /COPY *LIBL/QPRPSRC,HYR9930

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D out_ParmPos     S             10I 0
     D in_DescType     S             10I 0
     D in_DataType     S             10I 0
     D in_DescInf1     S             10I 0
     D in_DescInf2     S             10I 0
     D in_DataLen      S             10I 0
     D in_Feedback     S             12A

     P/EJECT
      **************************************************************************
      * Center a text string.                                                  *
      **************************************************************************
     P Center          B                   EXPORT

     D                 PI           256A   OPDESC
     D  Input                       256A   OPTIONS(*VARSIZE) CONST
     D  OutFldLen                    10I 0 OPTIONS(*NOPASS : *OMIT) CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_PadLen       S              3P 0
     D wk_Field        S            256A
     D Output          S            256A

      *
      * Retrieve operational descriptor information.
      *
     C                   CALLB     'CEEDOD'
     C                   PARM      1             out_ParmPos
     C                   PARM                    in_DescType
     C                   PARM                    in_DataType
     C                   PARM                    in_DescInf1
     C                   PARM                    in_DescInf2
     C                   PARM                    in_DataLen
     C                   PARM      '*OMIT'       in_Feedback

      *
      * Center the text string.
      *
     C                   EVAL      wk_Field =
     C                               %triml(%subst(Input : 1 : in_DataLen))
     C                   IF        %parms = 1  or  %addr(OutFldLen) = *Null
     C                   EVAL      wk_PadLen =
     C                               (in_DataLen - %checkr(' ' : wk_Field)) / 2
     C                   ELSE
     C                   EVAL      wk_PadLen =
     C                               (OutFldLen - %checkr(' ' : wk_Field)) / 2
     C                   ENDIF                                                  %parms=1 or
     C                   EVAL      Output = *Blanks
     C                   EVAL      %subst(Output : wk_PadLen+1 : in_DataLen) =
     C                               %subst(wk_Field : 1 : in_DataLen)

      *
      * Return the centered text string.
      *
     C                   RETURN    Output

     P Center          E

     P/EJECT
      **************************************************************************
      * Convert a text string to mixed case.                                   *
      **************************************************************************
     P CvtToMixed      B                   EXPORT

     D                 PI           256A   OPDESC
     D  Input                       256A   OPTIONS(*VARSIZE) CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_FldLen       S              3P 0
     D wk_Field        S            256A
     D wk_Type         S              1A
     D Output          S            256A
     D wk_Pos          S              3P 0
     D wk_CapIt        S              1N

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D Lower           C                   'abcdefghijklmnopqrstuvwxyz'
     D Upper           C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

      *
      * Retrieve operational descriptor information.
      *
     C                   CALLB     'CEEDOD'
     C                   PARM      1             out_ParmPos
     C                   PARM                    in_DescType
     C                   PARM                    in_DataType
     C                   PARM                    in_DescInf1
     C                   PARM                    in_DescInf2
     C                   PARM                    in_DataLen
     C                   PARM      '*OMIT'       in_Feedback

      *
      * Convert the text string to mixed case.
      *
     C                   EVAL      wk_FldLen = in_DataLen
     C                   EVAL      wk_Field =
     C                               %triml(%subst(Input : 1 : wk_FldLen))
     C                   EVAL      Output = *Blanks
     C                   EVAL      wk_Pos = 1
     C                   DOU       wk_Pos > wk_FldLen
     C                   IF        wk_Pos = 1  or
     C                               %subst(wk_Field:wk_Pos-1:1) = *Blank     or
     C                               %subst(wk_Field:wk_Pos-1:1) = '.'        or
     C                               (wk_Pos > 2  and
     C                                (%xlate(Lower:Upper:%subst(
     C                                 wk_Field:wk_Pos-2:2)) = 'MC'  or
     C                                 %xlate(Lower:Upper:%subst(
     C                                 wk_Field:wk_Pos-2:2)) = 'O'''))        or
     C                               (wk_Pos > 3  and
     C                                (%xlate(Lower:Upper:%subst(
     C                                 wk_Field:wk_Pos-3:3)) = 'MAC'))
     C                   EVAL      wk_CapIt = *ON
     C                   ELSE
     C                   EVAL      wk_CapIt = *OFF
     C                   ENDIF                                                  wk_Pos=1 or
     C                   IF        wk_CapIt
     C                   EVAL      %subst(wk_Field:wk_Pos:1) =
     C                               %xlate(Lower:Upper:
     C                                 %subst(wk_Field:wk_Pos:1))
     C                   ELSE
     C                   EVAL      %subst(wk_Field:wk_Pos:1) =
     C                               %xlate(Upper:Lower:
     C                                 %subst(wk_Field:wk_Pos:1))
     C                   ENDIF                                                  wk_CapIt
     C                   EVAL      wk_Pos = wk_Pos + 1
     C                   ENDDO                                                  wk_Pos>wk_FldLen

     C                   EVAL      Output = wk_Field

      *
      * Return the converted text string.
      *
     C                   RETURN    Output

     P CvtToMixed      E

     P/EJECT
      **************************************************************************
      * Convert a text string to lower case.                                   *
      **************************************************************************
     P CvtToLower      B                   EXPORT

     D                 PI           256A   OPDESC
     D  Input                       256A   OPTIONS(*VARSIZE) CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_FldLen       S              3P 0
     D wk_Field        S            256A
     D Output          S            256A

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D Upper           C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D Lower           C                   'abcdefghijklmnopqrstuvwxyz'

      *
      * Retrieve operational descriptor information.
      *
     C                   CALLB     'CEEDOD'
     C                   PARM      1             out_ParmPos
     C                   PARM                    in_DescType
     C                   PARM                    in_DataType
     C                   PARM                    in_DescInf1
     C                   PARM                    in_DescInf2
     C                   PARM                    in_DataLen
     C                   PARM      '*OMIT'       in_Feedback

      *
      * Convert the text string to lower case.
      *
     C                   EVAL      wk_FldLen = in_DataLen
     C                   EVAL      wk_Field =
     C                               %triml(%subst(Input : 1 : wk_FldLen))
     C                   EVAL      Output = %xlate(Upper:Lower:wk_Field)

      *
      * Return the converted text string.
      *
     C                   RETURN    Output

     P CvtToLower      E

     P/EJECT
