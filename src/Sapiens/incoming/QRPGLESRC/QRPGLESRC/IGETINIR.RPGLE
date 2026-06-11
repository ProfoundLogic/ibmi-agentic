    H*  (C) Copyright Progressive Data Solutions, Inc., 2002
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     *  Description:                                                   *
     *
     *  This API will FIND THE INI VALUE AND RETURN IT TO CALLER
     *  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     *  SEND TO THIS PGM:  SECTION, PARM, VALUE, RTRNCD
     *
     *  SEND BACK       :
     *                   VALUE, RTNCD
     *  - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
     *                                                                 *
     *  Called From:                                                   *
     *    VARIOUS PROGRAMS                                             *
     *                                                                 *
     *  Calls To:                                                      *
     *    NONE                                                         *
     *                                                                 *
     *  Created:  10/05/02  C:02  Project:                             *
     *                                                                 *
     * *****************************************************************
    H** REVISIONS:
     *
     * *****************************************************************
     *  Indicators:                                                    *
     *                                                                 *
     *    60 - General File Access                                     *
     * *****************************************************************
     *
    F*
     Fixini     if   e             disk
     F                                     rename(ixini:ixrec)
     *
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D l               s              3  0
     D len             s              3  0
     D qqparm          s             30
     D qqrtnc          s              1
     D qqsect          s             30
     D qqval           s             60
     D s               s              3  0
     D xxsecf          s              1
     D x1a             s              1
     D xxsect          s             30
     D xxparm          s             30
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D Up              C                   'ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ'
     D Lo              C                   'abcdefghijklmnopqrs-
     D                                     tuvwxyz'
    F*
     Iixrec
     I              ixini                       dta256
     * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    C*
     *
    C***        *LIKE     DEFN XXXX      XXXX
    C*
     * ---------------------------------------------------------------
     C     *entry        plist
     C                   parm                    qqsect
     C                   parm                    qqparm
     C                   parm                    qqval
     C                   parm                    qqrtnc
    C*
     C     Lo:Up         XLATE     qqsect        xxsect
     C     Lo:Up         XLATE     qqparm        xxparm
    C*
     *-------------------------------------------------------------------
    C***        Q1PRMS    IFGT 15
     *-------------------------------------------------------------------
    C*
     * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     *
     C                   movea     '000'         *in(90)
     C                   move      *off          *in60
     C                   clear                   l
     C                   clear                   s
     C                   clear                   len
     C                   movel     ' '           qqrtnc
    C*
    C*
     C     1             setll     ixrec
    C*
     C                   read      ixrec                                  60
     C                   dow       *in60 = *off
     C*
     C     Lo:Up         XLATE     dta256        xxd256          256
     c*
    C*** IF SECTION NOT YET FOUND CHECK FOR SECTION
    C*
     C                   if        xxsecf <> '1'
     C     xxsect        scan      xxd256:1      l                        90
     C                   if        *in90 = *on
     C                   move      '1'           xxsecf
     C                   endif
    C*
     C                   else
    C*** ELSE SECTION ALREADY FOUND, SO CHECK FOR PARM
    C*
     C                   movel     xxd256        x1a
     C                   if        x1a = '['
    C*
    C*** FOUND ANOTHER SECTION, END PROCESS AS PARM CAN'T BE FOUND
     C                   movel     '1'           qqrtnc
     C                   goto      rtn
     C                   endif
    C*
     C                   Eval      l =(%scan(%trimr(xxparm):xxd256:1))
     C*
     C*****xxparm        scan      xxd256:1      l                        91
     C*
     C                   if        l <> 0
    C*
     C     '='           scan      xxd256:1      l                        92
     C                   if        *in92 = *on
     C                   eval      s = l + 1
     C                   eval      len = 256 - l
     C     len           subst(p)  xxd256:s      qqval
     C                   eval      qqval = %triml(qqval)
     C                   movel     ' '           qqrtnc
     C                   goto      rtn
     C                   else
     C                   movel     '1'           qqrtnc
     C                   goto      rtn
     C                   endif
     C                   endif
    C*
     C                   endif
    C*
     C                   read      ixrec                                  60
     C                   enddo
     C*
     C*** if not found, set as error (not found)
     C                   eval      qqrtnc = '1'
     *
     C     rtn           tag
    C****                 RETRN
     * * MUST CLOSE FILE BECAUSE OF ACCESS FROM CALLING CL PROGRAM
    C*
     C     term          tag
     C                   move      *on           *inlr
