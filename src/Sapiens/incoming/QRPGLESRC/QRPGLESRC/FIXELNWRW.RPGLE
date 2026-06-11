     H* (C) Copyright Fiserv, Inc., 2008
     H**********************************************************************
     H/TITLE FIXELNWRW - Assign renewal flag for all employers
     H**********************************************************************
     H*  Description:  This program will update the new/renewal flag       *
     H*                for a all employers                                 *
     H*                                                                    *
     H*  Called from:                                                      *
     H*    FIXELNWRWC - Assign renewal flag - override                     *
     H*                                                                    *
     H*  CALLS TO:                                                         *
     H*    None                                                            *
     H*                                                                    *
     H**********************************************************************
     H*  Created on 09/04/2008 by c:62 for Project WC1611                  *
     H**********************************************************************
     H* Revisions:                                                         *
     H*                                                                    *
     H* 12/10/09 C:062  Support: 70983                                     *
     H*                 Add Quote Processing                               *
     H*                                                                    *
     H* 03/01/09 C:136  Support: 139WJU                                    *
     H*                 Speed this up by inserting the SETGT opcode to     *
     H*                 call WA070R just once for each employer.           *
     H*                                                                    *
     H**********************************************************************
     FWDELL     IF   E           K DISK
     FWAELL     UF   E           K DISK    rename(pel01:pel02) prefix(A)
     F*
     D psds          esds                  extname(szq1p)
     D ##co#           s                   like(elco#)
     D ##fnd           s                   like(elfnd)
     D ##emp#          s                   like(elemp#)
     D @@call          s              1
      *** beg add ***** 12/10/09 *******************************************************
     D ##call          s              1
      *** end add ***** 12/10/09 *******************************************************
     D xxnwrw          s              1
     C*
     C     *entry        plist
     C                   parm                    @@call
     C*
     C     elky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    elfyr
     C*
      *** beg add ***** 03/01/09 *******************************************************
     C     elky1         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
      *** end add ***** 03/01/09 *******************************************************
     C*
     C* Let's work thru WDELP and update the new/renewal flag
     C*
     c     *loval        setll     pel01
     C                   read      pel01
     C                   dow       not %eof
     c*
     c                   call      'WA070R'
     c                   parm      elco#         ##co#
     c                   parm      elfnd         ##fnd
     c                   parm      elemp#        ##emp#
      *** beg add ***** 12/10/09 *******************************************************
     c                   parm      @@call        ##call
      *** end add ***** 12/10/09 *******************************************************
     c*
      *** beg add ***** 03/01/09 *******************************************************
     c     elky1         setgt     pel01
      *** end add ***** 03/01/09 *******************************************************
     c                   read      pel01
     c                   enddo
     c*
     c                   if        @@call='P' or @@call='T'
      *** beg chg ***** 03/01/09 *******************************************************
     c******loval        setll     pel01
     C***********        read      pel01
     C***********        dow       not %eof
     c***********
     c***********        eval      xxnwrw=elnwrw
     c***********
     c*****elky**        chain     pel02
     c***********        if        %found
     c***********        update    pel02
     c***********        endif
     C***********
     c***********        read      pel01
     c***********        enddo
      *
     c     *loval        setll     pel02
     C                   read      pel02
     C                   dow       not %eof
     c*
     c     elky          chain     pel01
     c                   if        %found
     c                   eval      AELNWRW = ELNWRW
     c                   update    pel02
     c                   endif
     C*
     c                   read      pel02
     c                   enddo
      *** end chg ***** 03/01/09 *******************************************************
     c                   endif
     c*
     c                   seton                                        lr
