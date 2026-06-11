     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H bnddir('WTBNDDIR')
     H*********************************************************************
     H/title SRCABILL - Determine if CA should be billed
     H*********************************************************************
     H*  Created: 11/19/09   C:33  Project: 25WQI                         *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H* 08/17/20  C:65  Project:  Seq#90110 (Amwins)                      *
     H*                 Change the default check for policy submission to *
     H*                 submission detail file - if policy has submitted  *
     H*                 into a file at any time.                          *
     H*                                                                   *
     H* 04/02/20  C:65  Project:  Seq#88398 (Sunz)                        *
     H*                 Disable CA check for Sunz.                        *
     H*                                                                   *
     H* 05/11/11 C:136 Support: WA136 79749                               *
     H*                Implement same change from 72921 for Carewest      *
     H*                                                                   *
     H* 01/14/10 C:136 Support: WA109 75266                               *
     H*                Implement same change from 72921 for KRMCA         *
     H*                                                                   *
     H* 06/09/10  C:65  Project:  Seq#72921                               *
     H*                 Change the check to prevent assessments from      *
     H*                 changing if status is > 2 instead of > 3, flagged *
     H*                 for compwest.  Move the array definitions for the *
     H*                 state fields to be global to prevent the values   *
     H*                 from being wiped out.                             *
     H*                                                                   *
     H*********************************************************************
     FWMSHL13   if   e           k disk    rename(psh01:psh13)
     F*** begin add ***  08/17/20  ****************************************
     Fwdshl1    if   e           k disk    rename(ps101:ps101)
     F*** end add *****  08/17/20  ****************************************

     D*entry
     DSrCABill         pr
     D inCo#                               like(SHCO#)  const
     D inFnd                               like(SHFND)  const
     D inEmp#                              like(SHEMP#) const
     D inDiv                               like(SHDIV)  const
     D inFyr                               like(SHFYR)  const
     D billCA                         1n
     DSrCABill         pi
     D inCo#                               like(SHCO#)  const
     D inFnd                               like(SHFND)  const
     D inEmp#                              like(SHEMP#) const
     D inDiv                               like(SHDIV)  const
     D inFyr                               like(SHFYR)  const
     D inBillCA                       1n
     d*** begin add ***  06/09/10  **************************************************
     D                 DS
     D  arraySHST              1     30    DIM(15)
     D  SHST01                 1      2
     D  SHST02                 3      4
     D  SHST03                 5      6
     D  SHST04                 7      8
     D  SHST05                 9     10
     D  SHST06                11     12
     D  SHST07                13     14
     D  SHST08                15     16
     D  SHST09                17     18
     D  SHST10                19     20
     D  SHST11                21     22
     D  SHST12                23     24
     D  SHST13                25     26
     D  SHST14                27     28
     D  SHST15                29     30
     D                 DS
     D  arraySHSS              1     15    DIM(15)
     D  SHSS01                 1      1
     D  SHSS02                 2      2
     D  SHSS03                 3      3
     D  SHSS04                 4      4
     D  SHSS05                 5      5
     D  SHSS06                 6      6
     D  SHSS07                 7      7
     D  SHSS08                 8      8
     D  SHSS09                 9      9
     D  SHSS10                10     10
     D  SHSS11                11     11
     D  SHSS12                12     12
     D  SHSS13                13     13
     D  SHSS14                14     14
     D  SHSS15                15     15
     d*** end add *****  06/09/10  **************************************************

     D/copy sPrDate

     D/copy srcabillpr

     C                   eval      inBillCA = BillCA(inCo#
     C                                              :inFnd
     C                                              :inEmp#
     C                                              :inDiv
     C                                              :inFyr
     C                                              )

     C                   eval      *inlr = *on
      /eject
     P*=====================================================================
     PBillCA           b                   export
     DBillCA           pi             1n
     D inCo#                               like(SHCO#)  const
     D inFnd                               like(SHFND)  const
     D inEmp#                              like(SHEMP#) const
     D inDiv                               like(SHDIV)  const
     D inFyr                               like(SHFYR)  const

     D x2type          s                   like(SHTYPE)
     D i               s              3  0
     D*
     d*** begin del ***  06/09/10  **************************************************
     D*************    DS
     D**arraySHST**            1     30    DIM(15)
     D**SHST01*****            1      2
     D**SHST02*****            3      4
     D**SHST03*****            5      6
     D**SHST04*****            7      8
     D**SHST05*****            9     10
     D**SHST06*****           11     12
     D**SHST07*****           13     14
     D**SHST08*****           15     16
     D**SHST09*****           17     18
     D**SHST10*****           19     20
     D**SHST11*****           21     22
     D**SHST12*****           23     24
     D**SHST13*****           25     26
     D**SHST14*****           27     28
     D**SHST15*****           29     30
     D*************    DS
     D**arraySHSS**            1     15    DIM(15)
     D**SHSS01*****            1      1
     D**SHSS02*****            2      2
     D**SHSS03*****            3      3
     D**SHSS04*****            4      4
     D**SHSS05*****            5      5
     D**SHSS06*****            6      6
     D**SHSS07*****            7      7
     D**SHSS08*****            8      8
     D**SHSS09*****            9      9
     D**SHSS10*****           10     10
     D**SHSS11*****           11     11
     D**SHSS12*****           12     12
     D**SHSS13*****           13     13
     D**SHSS14*****           14     14
     D**SHSS15*****           15     15
     d*** end del *****  06/09/10  **************************************************

     C     keyWMSHL13    klist
     C                   kfld                    inCo#
     C                   kfld                    inFnd
     C                   kfld                    inEmp#
     C                   kfld                    inDiv
     C                   kfld                    inFyr
     C                   kfld                    x2type
     c*** begin add ***  08/17/20  ****************************************

     C     keyWDSHL1     klist
     C                   kfld                    inCo#
     C                   kfld                    inFnd
     C                   kfld                    inEmp#
     C                   kfld                    inFyr
     C                   kfld                    x2type
     C                   kfld                    x2st              2
     c*** end add *****  08/17/20  ****************************************
     C*
     c*** begin add ***  06/09/10  *************************************************
    C*
     C                   call      'RTVCLTID'
     C                   parm                    cltid             3
     C*
     c*** end add *****  06/09/10  *************************************************
     C                   eval      x2type = 'P'
     C*
     C     keyWMSHL13    chain     psh13
     C*
     C                   if        not %found
     C*** begin add ***  04/02/20  ********************************************
     C                             or cltid = '150'
     C*** end add *****  04/02/20  ********************************************
     C                   return    *on
     C                   endif
     C*
     C* Bill CA, if CA state exists on policy with status > '3'
     C                   eval      i = 1
     C     'CA'          lookup    arraySHST(i)                           60
     c*** beg chg ***** 01/14/10 ***************************************************
     C***********        if        %found
     C***********                   and arraySHSS(i) > '3'
     c****begin*add ***  06/09/10  *************************************************
     c***********                   and cltid <> '132'
     C***********                   or %found
     C***********                   and arraySHSS(i) > '2'
     c***********                   and cltid = '132'
     c****end*add *****  06/09/10  *************************************************
     C***********        return    *off
     C***********        endif                                                  *IN60 IFEQ '0'

     c                   if        %found
     c                   select
     c                   when      cltid = '109'
     c                             and arraySHSS(i) > '2'
     c                   return    *off
     c                   when      cltid = '132'
     c                             and arraySHSS(i) > '2'
     c                   return    *off
     c*** beg add ***** 05/11/11 ***************************************************
     c                   when      cltid = '136'
     c                             and arraySHSS(i) > '2'
     c                   return    *off
     c*** end add ***** 05/11/11 ***************************************************
     c                   other
     c*** begin chg ***  08/17/20  ****************************************
     c*********          if        arraySHSS(i) > '3'
     c                   Eval      x2st = 'CA'
     C     keyWDSHL1     Chain     ps101
     C                   If        %found
     c*** end chg *****  08/17/20  ****************************************
     c                   return    *off
     c                   endif
     c                   endsl
     c                   endif
     c*** end chg ***** 01/14/10 ***************************************************
     C*
     C                   return    *on
     C*
     PBillCA           e
