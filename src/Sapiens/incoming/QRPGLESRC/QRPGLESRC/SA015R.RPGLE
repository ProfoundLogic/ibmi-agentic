     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE API - SA015R - System API to Determine if ICD10 is active*
     H*****************************************************************
     H* Description:  This program will be used to Read Data Ares     *
     H*               ICD10 and retund that is ICD10 is active or     *
     H*               not.                                            *
     H*                                                               *
     H*  Called From: U0060R01, F0351R, F0351RCA, F0351RFL, F0351RTX, *
     H*               F0361R, F0361RCA, F0361RFL, F0361RGA, F0361RLA, *
     H*               F0361RMA, F0361RNY, F0361RPA, F0361TN, F0361RTX *
     H*               F0381R, F0381R, F0381RCA, F0381RFL, F0381RTX    *
     H*                                                               *
     H*  Calls To:    None                                            *
     H*                                                               *
     H*****************************************************************
     H* Created on 05/23/2013 by C:277   for  Project MC329           *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 02/05/14 C:296 PROJECT: SY336                                 *
     H*                Handel program for blank date.                 *
     H*                                                               *
     H* 08/05/13 C:296 PROJECT: MC329 Phase II                        *
     H*                Added procedure to convert date.               *
     H*****************************************************************
     DsystemDate       pr              d
     D*
     D***** Beg Chg ***** 08/05/13 ************************************
     D*****DateTo8          pr             8  0
     D***** date                            d   datfmt(*USA) Const
     D*****
     DdtCYMD           pr             7  0
     D numDate8                       8  0 Const
     D***** End Chg ***** 08/05/13 ************************************
     D num8            s              8  0
     D* Stand alone Variables
     D***** Beg Chg ***** 08/05/13 ************************************
     D***** sysdate         s              8  0
     D sysdate         s              7  0
     D***** End Chg ***** 08/05/13 ************************************
     D ##rtcd          s              1
     D icd10date       s             10
     D***** Beg Chg ***** 08/05/13 ************************************
     D***** x1icddate       s              8
     D x1icddate       s              8  0
     D***** xxicddate       s              8
     D xxicddate       s              7  0
     D***** End Chg ***** 08/05/13 ************************************
     C*****************************************************************
     C* Program Starts here
     C*
     C     *entry        plist
     C                   parm                    ##rtcd
     C*
     C*   Read Date from ICD10 Data Area
     C*
     C     *dtaara       define    icd10dta      icd10date
     C     *lock         in        icd10date
     C***** Beg Chg ***** 02/05/14 ************************************
     C                   if        icd10date <> *blanks
     C***** End Chg ***** 02/05/14 ************************************
     C*
     C***** Beg Chg ***** 08/05/13 ************************************
     C*****              eval      x1icddate = %subst(icd10date:1:8)
     C*****              movel     x1icddate     xxicddate
     C*****              eval      sysDate = DateTo8(SystemDate)
     C                   eval      sysDate = %dec(%date():*cymd)
     C                   eval      x1icddate = %dec(%subst(icd10date:1:8):8:0)
     C                   eval      xxicddate = dtCYMD(x1icddate)
     C***** End Chg ***** 08/05/13 ************************************
     C*
     C* set the return flag
     C*
     C                   If        icd10date <> *blanks
     C                             and sysDate >= xxicddate
     C*
     C* If ICD10 is Active set retur code '2'
     C*
     C                   eval      ##rtcd = '2'
     C                   else
     C*
     C* If ICD10 is inactive set retur code '1'
     C*
     C                   eval      ##rtcd = '1'
     C                   Endif
     C***** Beg Chg ***** 02/05/14 ************************************
     C                   else
     C* If dataarea (icd10dta) has blank value then return icd 9.
     C                   eval      ##rtcd = '1'
     C                   Endif
     C***** End Chg ***** 02/05/14 ************************************
     C*
     C                   eval      *inlr = *on
