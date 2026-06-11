     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H/define CONVERT
     H/TITLE SS010R
    H*****************************************************************
    H* Needs BNDSRVPGM((SSCOMMON *IMMED))                            *
    H*****************************************************************
    H*  REVISIONS:                                                   *
    H*                                                               *
     H* 01/28/10  C:XXX  Project: SY215                               *
     H*           C:189  Retrieve brand Name
     H*                                                               *
    H*****************************************************************
     Fss010f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF

     DPgmStsDS       esds                  extname(szq1p)
     DtwentyZero       s             20    inz('00000000000000000000')
     D*** BEGIN CHG *********** 01/28/10 ************************************
     D***pdsName       c                   'Progressive Data Solutions, Inc.'
     D pdsName         S             40    Inz
     D COMAPI          c                   'WA001R'
     D*** END CHG ************* 01/28/10 ************************************
     DprogTitle        c                   'Create password for PDS apps'
     DusaDate          s               d   datfmt(*usa)
     D##chkpds         s              1
     D                 ds
     DscPasWrd                       32
     D  scPasWrd1                     4    overlay(scPasWrd:1)
     D  scPasWrd2                     4    overlay(scPasWrd:5)
     D  scPasWrd3                     4    overlay(scPasWrd:9)
     D  scPasWrd4                     4    overlay(scPasWrd:13)
     D  scPasWrd5                     4    overlay(scPasWrd:17)
     D  scPasWrd6                     4    overlay(scPasWrd:21)
     D  scPasWrd7                     4    overlay(scPasWrd:25)
     D  scPasWrd8                     4    overlay(scPasWrd:29)

     D*=====================================================================
     D* Procedure prototypes
     D*=====================================================================
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
     D***  Begin Add  ***  01/28/10  **************************************
     DbrandName        s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************

     C                   call      'SACHKPDS'                           99
     C                   parm                    ##chkpds
     C                   if        ##chkpds = *on  AND
     C                             *in99 = *off
     C
     D*** BEGIN CHG *********** 01/28/10 ************************************
     C                   callp     RtvBrnd('I':brandName)
     C*
     C                   eval       pdsname  = brandname
     D*** END CHG ************* 01/28/10 ************************************
     C*****              EVAL      scActDat  = %DATE
     c                   move      *date         scActDat
     C                   move      12312099      scExpDat
     C                   eval      scconm = centerStr(pdsName : 40)
     C                   eval      scprds = centerStr(progTitle : 40)
     C*
     C                   dou       *inkc = *on
     C                   exfmt     scr01
     C                   movea     twentyZero    *in(71)
     C                   if        *inkc = *off
     C                   exsr      errorCheck
     C**
     C* If no errors, display resulting password
     C**
     C                   if        *in90 = *off
     C                   seton                                        5190
     C                   eval      scPasWrd = GetPassWrd(scSerNbr :
     C                             CvtToDate(scExpDat) :
     C                             CvtToDate(scActDat) : scmodl)
     C                   endif
     C                   endif
     C                   enddo
     C                   endif
     C                   seton                                        lr

     C*=====================================================================
     C* errorCheck - check for errors on screen
     C*=====================================================================
     C     errorCheck    begsr
     C
     C                   if        scSerNbr = *blanks
     C                   seton                                        9071
     C                   endif
     C
     C                   if        *in90 = *off
     C*
     C                   if        scActDat  < 1000000
     C     *mdy          test(d)                 scActDat               90
     C                   if        *in90 = *off
     C     *mdy          move      scActDat      usaDate
     C                   move      usaDate       scActDat
     C                   endif
     C                   else
     C     *usa          test(d)                 scActDat               90
     C                   endif
     C   90              seton                                        74
     C
     C                   endif
     C
     C                   if        *in90 = *off
     C*
     C                   if        scExpDat  < 1000000
     C     *mdy          test(d)                 scExpDat               90
     C                   if        *in90 = *off
     C     *mdy          move      scExpDat      usaDate
     C                   move      usaDate       scExpDat
     C                   endif
     C                   else
     C     *usa          test(d)                 scExpDat               90
     C                   endif
     C   90              seton                                        72
     C
     C                   endif
     C
     C                   if        *in90 = *off
     C                   if        cvtToDate(scExpDat) < cvtToDate(scActDat)
     C                   seton                                        9073
     C                   endif
     C                   endif
     C
     C                   endsr
     C

