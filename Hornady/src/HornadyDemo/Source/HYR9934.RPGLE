     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2008')
      **************************************************************************
      * APPLICATION: Miscellaneous                                             *
      *       TITLE: Subprocedures - Trademark and Copyright                   *
      *          BY: William Earl (MCA)                                        *
      *        DATE: November 2008                                             *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------- *
      *  02/09/2009  MCA  Add ASCII/EBCDIC pre/post conversion subprocedures.  *
      *                                                                        *
      **************************************************************************

     Hnomain

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
      /COPY *LIBL/QPRPSRC,HYR9934

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

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************
     D RegTM           C                   x'AF'
     D TM              C                   x'BC'
     D TM2             C                   x'39'                                Cvts to x'99' ascii
     D Copyright       C                   x'B4'

     P/EJECT
      **************************************************************************
      * Remove an intellectual property symbol from a text string.             *
      **************************************************************************
     P RmvIPSym        B                   EXPORT

     D                 PI           256A   OPDESC
     D  Symbol                        1A   CONST
     D  Input                       256A   OPTIONS(*VARSIZE) CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_FldLen       S              3P 0
     D wk_Field        S            256A
     D wk_Symbol       S              1A
     D Output          S            256A
     D wk_Pos          S              3P 0

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************

      *
      * Retrieve operational descriptor information.
      *
     C                   CALLB     'CEEDOD'
     C                   PARM      2             out_ParmPos
     C                   PARM                    in_DescType
     C                   PARM                    in_DataType
     C                   PARM                    in_DescInf1
     C                   PARM                    in_DescInf2
     C                   PARM                    in_DataLen
     C                   PARM      '*OMIT'       in_Feedback

      *
      * Remove the intellectual property symbols.
      *
     C                   EVAL      wk_FldLen = in_DataLen
     C                   EVAL      wk_Field =
     C                               %triml(%subst(Input : 1 : wk_FldLen))
     C                   EVAL      wk_Symbol = Symbol
     C                   EVAL      Output = *Blanks
     C                   EVAL      wk_Pos = 1
     C                   DOU       wk_Pos > wk_FldLen
     C                   IF        ((wk_Symbol = *Blank  or  wk_Symbol = 'R')
     C                               and  %subst(wk_Field:wk_Pos:1) = RegTM)  or
     C                             ((wk_Symbol = *Blank  or  wk_Symbol = 'T')
     C                               and  %subst(wk_Field:wk_Pos:1) = TM)  or
     C                             ((wk_Symbol = *Blank  or  wk_Symbol = 'C')
     C                               and  %subst(wk_Field:wk_Pos:1) = Copyright)
     C                   EVAL      wk_Field = %subst(wk_Field:1:wk_Pos-1) +
     C                               %subst(wk_Field:wk_Pos+1)
     C                   ELSE
     C                   EVAL      wk_Pos = wk_Pos + 1
     C                   ENDIF                                                  wk_Sysbol=*Blank or
     C                   ENDDO                                                  wk_Pos>wk_FldLen

     C                   EVAL      Output = wk_Field

      *
      * Return the converted text string.
      *
     C                   RETURN    Output

     P RmvIPSym        E

     P/EJECT
      **************************************************************************
      * Pre EBCDIC to Ascii conversion conversion.                             *
      **************************************************************************
     P PreAsciiCvt     B                   EXPORT

     D                 PI           256A   OPDESC
     D  Symbol                        1A   CONST
     D  Input                       256A   OPTIONS(*VARSIZE) CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_FldLen       S              3P 0
     D wk_Field        S            256A
     D wk_Symbol       S              1A
     D Output          S            256A
     D wk_Pos          S              3P 0

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************

      *
      * Retrieve operational descriptor information.
      *
     C                   CALLB     'CEEDOD'
     C                   PARM      2             out_ParmPos
     C                   PARM                    in_DescType
     C                   PARM                    in_DataType
     C                   PARM                    in_DescInf1
     C                   PARM                    in_DescInf2
     C                   PARM                    in_DataLen
     C                   PARM      '*OMIT'       in_Feedback

      *
      * Convert the intellectual property symbols.
      *
     C                   EVAL      wk_FldLen = in_DataLen
     C                   EVAL      wk_Field =
     C                               %triml(%subst(Input : 1 : wk_FldLen))
     C                   EVAL      wk_Symbol = Symbol
     C                   EVAL      Output = *Blanks
     C                   EVAL      wk_Pos = 1
     C                   DOU       wk_Pos > wk_FldLen
     C                   IF        ((wk_Symbol = 'T')
     C                               and  %subst(wk_Field:wk_Pos:1) = TM)
     C                   EVAL      %subst(wk_Field:wk_Pos:1) = TM2
     C                   ELSE
     C                   EVAL      wk_Pos = wk_Pos + 1
     C                   ENDIF                                                  wk_Sysbol=*Blank or
     C                   ENDDO                                                  wk_Pos>wk_FldLen

     C                   EVAL      Output = wk_Field

      *
      * Return the converted text string.
      *
     C                   RETURN    Output

     P PreAsciiCvt     E

     P/EJECT
      **************************************************************************
      * Post Ascii to EBCDIC conversion conversion.                            *
      **************************************************************************
     P PostAsciiCvt    B                   EXPORT

     D                 PI           256A   OPDESC
     D  Symbol                        1A   CONST
     D  Input                       256A   OPTIONS(*VARSIZE) CONST

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_FldLen       S              3P 0
     D wk_Field        S            256A
     D wk_Symbol       S              1A
     D Output          S            256A
     D wk_Pos          S              3P 0

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************

      *
      * Retrieve operational descriptor information.
      *
     C                   CALLB     'CEEDOD'
     C                   PARM      2             out_ParmPos
     C                   PARM                    in_DescType
     C                   PARM                    in_DataType
     C                   PARM                    in_DescInf1
     C                   PARM                    in_DescInf2
     C                   PARM                    in_DataLen
     C                   PARM      '*OMIT'       in_Feedback

      *
      * Convert the intellectual property symbols.
      *
     C                   EVAL      wk_FldLen = in_DataLen
     C                   EVAL      wk_Field =
     C                               %triml(%subst(Input : 1 : wk_FldLen))
     C                   EVAL      wk_Symbol = Symbol
     C                   EVAL      Output = *Blanks
     C                   EVAL      wk_Pos = 1
     C                   DOU       wk_Pos > wk_FldLen
     C                   IF        ((wk_Symbol = 'T')
     C                               and  %subst(wk_Field:wk_Pos:1) = TM2)
     C                   EVAL      %subst(wk_Field:wk_Pos:1) = TM
     C                   ELSE
     C                   EVAL      wk_Pos = wk_Pos + 1
     C                   ENDIF                                                  wk_Sysbol=*Blank or
     C                   ENDDO                                                  wk_Pos>wk_FldLen

     C                   EVAL      Output = wk_Field

      *
      * Return the converted text string.
      *
     C                   RETURN    Output

     P PostAsciiCvt    E

     P/EJECT
