     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE W3030R1 - Create EFT Transactions
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
     H*
     H* REWRITTEN 06/29/09 BY C:191 PROJECT: WC1806
    H*********************************************************************
    H* REVISIONS:
     H*
     H*  02/28/13 C:283 Support: WA071 81681                          *
     H*                 Moving the functionality that creates the     *
     H*                 WAEFP records to the place where WAR2P records*
     H*                 are deleted i.e W3080R.                       *
     H*                 Therefore removing the calls to WA073R from   *
     H*                 this program and will call WA073R in W3030R1. *
     H*                                                               *
     H*  04/17/12 C:255 Project: WC2044                               *
     H*                 Modify program to use EFT flag.               *
     H*                 Change existing keylist R2KY2 to use the flag.*
     H*
     H*  06/29/09 C:191 Project: WC1806                               *
     H*                 Re-write to consolidate process               *
     H*                 Change to combine with ACH process.           *
     H*                 Now calls WA073R to write EFT to WAEFP        *
     H*
     H* 09/07/07 :  Copied into ALDON by C:136 for Project WC997
    H*********************************************************************
     Fsmcol     uf   e           k disk    rename(pco01:pco00)
     Fwar2l2    uf   e           k disk    rename(pr201:pr202)
     F                                     infds(dbfds)
     Fwmaal1    if   e           k disk
     Fwmefl     if   e           k disk
     F                                     infds(dbfds1)
     Fssylf     if   e           k disk    ignore(ssy01:spc01:src01:sbc01:scs01)
     F*

     D/copy sprdate

     D psds          esds                  extname(szq1p)

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@date          s              8
     D @@co#           s              3
     D @@fnd           s              3
      *
     D dbfds           ds
     D  dbrrn                397    400b 0
      *
     D dbfds1          ds
     D  dbrrn1               397    400b 0
      *
     D xxco#           s                   like(R2CO#)
     D xxfnd           s                   like(R2FND)
      *******Beg Chg********04/17/12**********************************
     D**** xxcktp          s                   like(R2CKTP) inz('EFT')
     D xxeftc          s                   like(R2eftc) inz('E')
      *******End Chg********04/17/12**********************************
     D xxbul2          s                   like(R2BUL2)
     D x7date          s                   like(EFEFDT)
     D xxtype          s                   like(EFTYPE) inz('A')
     D xxCleanup       s               N   inz
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
      * SMCOL Key
     C     coky          klist
     C                   kfld                    xxco#
    C* WAR2L key
      *******Beg Chg********04/17/12**********************************
     C*****r2ky2         klist
     C*****              kfld                    xxco#
     C*****              kfld                    xxfnd
     C*****              kfld                    xxcktp
     C*
     C     r2ky2         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxeftc
      *******End Chg********04/17/12**********************************
    C* WMAAL key
     C     aaky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    R2AGT#
    C* WMEFL key
     C     efky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxtype
     C                   kfld                    xxfnd
     C                   kfld                    AAFEIN
     C                   kfld                    AAMOD
     C*
     C     efky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxtype
     C                   kfld                    xxfnd
     C                   kfld                    AAFEIN
     C                   kfld                    AAMOD
     C                   kfld                    x7date
    C*____________________________________________________________________
    C*
     C     *entry        plist
     C                   parm                    @@date                         mmddccyy
     C                   parm                    @@co#
     C                   parm                    @@fnd
    C*____________________________________________________________________
    C*
     C                   move      @@co#         xxco#
     C                   move      @@fnd         xxfnd
     C                   eval      x7date = dtCYMD(numDate(@@date))
    C*____________________________________________________________________
    C*
    C* Check EFT flag
     C     'WC'          chain     swc01                              60
     C     WCFL17        cabne     'Y'           bypasit
    C*
      * Chain to Company File for Batch Update Log No. 2
     C     coky          chain(n)  smcol                              60
    C*
     C                   if        not *in60
    C*
     C                   eval      xxbul2 = COBUL2
    C*
     C     r2ky2         chain     war2l2                             60
    C*
     C                   dow       not *in60
     C                   exsr      process
     C     r2ky2         reade     war2l2                                 60
     C                   enddo
     C*
     C                   exsr      updtSMCOP
    C*
     C                   endif
    C*
     C     bypasit       tag
     C*
     C*** BEGIN DEL ***  02/28/13  ************************************
     C*********          if        xxcleanup
      * Call API to finally cleanup
     C*********          call      'WA073R'
     C*********          parm      'X'           ##type
     C*********          parm      *zeros        ##rrn
     C*********          parm      *zeros        ##rrn1
     C*********          parm      ' '           ##rtn
     C*********          endif                                                  xxcleanup
     C*** END   DEL ***  02/28/13  ************************************
     C*
     C                   move      *on           *inlr
    C*____________________________________________________________________
    C*
     C*====================================================================
    C* process - Process the EFT records
     C*====================================================================
    C*
    C     process       begsr
     C*
     C     aaky1         chain     wmaal1                             60
     C                   if        not *in60
     C*
     C     efky1         setgt     pef01
     C     efky          readpe    pef01                                  60
     C*
     C                   dow       not *in60
      *
     C* find effective record, i.e. batch date in between effective and expiry dates
     C                   if        x7date>= efefdt and
     C                             (x7date< efexdt or efexdt = 0)
     C*** BEGIN DEL ***  02/28/13  ************************************
      * Call API to write to EFT central holding file API
     C*********          call      'WA073R'
     C*********          parm      'A'           ##type            1
     C*********          parm      dbrrn         ##rrn             9 0
     C*********          parm      dbrrn1        ##rrn1            9 0
     C*********          parm      ' '           ##rtn             1
     C*
     C*********          if        ##rtn = ' '
     C*** END   DEL ***  02/28/13  ************************************
    C*
     C                   eval      xxcleanup = *on
     C                   eval      xxbul2 = xxbul2 + 1
     C                   eval      R2BUL2 = xxbul2
    C*
     C                   update    pr202
    C*
     C*** BEGIN DEL ***  02/28/13  ************************************
     C*********          endif                                                   ##rtn= ' '
     C*** END   DEL ***  02/28/13  ************************************
     C                   leave
     C                   endif                                                   x7date>=efefdt
     C*
     C     efky          readpe    pef01                                  60
     C                   enddo                                                   not *in60 (wmefp)
     C*
     C                   endif                                                   not *in60 (wmaap)
    C*
     C                   endsr
    C*
     C*====================================================================
    C* updtSMCOP - Update records in SMCOP
     C*====================================================================
    C*
    C     updtSMCOP     begsr
    C*
     C     coky          chain     smcol                              60
    C*
     C                   if        not *in60
     C                   eval      COBUL2 = xxbul2
     C                   update    pco00
     C                   endif
    C*
     C                   endsr
