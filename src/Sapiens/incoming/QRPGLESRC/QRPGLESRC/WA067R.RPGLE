     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*
     H/TITLE WA067R - Claim User Security API
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:  Claim User Security API                 *
     H*          Return code @@RTCD (1A)                              *
     H*                ' ' - Claim not secured, User Authorised       *
     H*                '1' - Secured claim, User Authorised           *
     H*                '2' - User Not Authorised                      *
     H* CALLED BY:                                                    *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 05/25/07  C:191 PROJECT WC1515                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/28/18 c:62 Ticket 71780(114)                               *
     H*               Add claim exclusion file processing             *
     H*                                                               *
     H*****************************************************************
     Fwmcul     if   e           k disk
     f*** Begin Add *** 06/28/18 **************************************
     Fwmxrl1    if   e           k disk
     f*** End   Add *** 06/28/18 **************************************
     D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@co#           s              3
     D @@fnd           s              3
     D @@fyr           s              3
     D @@case          s              7
     D @@usr           s             10
     D @@rtcd          s              1
     D kkco#           s              3  0
     D kkfnd           s              3  0
     D kkfyr           s              3  0
     D kkcase          s              7  0
     D kkusr           s             10
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     C*
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@fyr
     C                   parm                    @@case
     C                   parm                    @@usr
     C                   parm                    @@rtcd
     C*
     C*** partial key list to check claim record
     C     cuky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C*** keylist to check authority record
     C     cuky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkusr
     C*
     C*** Set key field values
     C                   move      @@co#         kkco#
     C                   move      @@fnd         kkfnd
     C                   move      @@fyr         kkfyr
     C                   move      @@case        kkcase
     C                   eval      kkusr = @@usr
     C*
     C*** Set return code to default
     C                   eval      @@rtcd = *blanks
     C*** Begin Add *** 06/28/18 ****************************************
     C* Check the Claim Exclusion File
      /free
       Chain (kkCo#:kkFnd:kkFyr:kkCase:kkUsr) pxr01;
       If %Found(wmxrl1) And XRstat <> 'I';
          @@RtCd = '2';
          *inLR  = *On;
          Return;
       EndIf;
      /end-free
     C*** End   Add *** 06/28/18 ****************************************
     C*
     C*** Check if active authority is defined for a claim
     C     cuky          setll     pcu01
     C     cuky          reade     pcu01
     C                   dow       not %eof
     C                   if        custat = 'A'
     C                   eval      @@rtcd = '2'
     C                   leave
     C                   endif                                                  custat = 'A'
     C     cuky          reade     pcu01
     C                   enddo                                                  not %eof
     C*
     C                   if        @@rtcd = ' '
     C*** return to caller, no active record found - Claim is not secured
     C                   eval      *inlr = *on
     C                   return
     C                   endif                                                  @@rtcd = ' '
     C*
     C*** Got here so secured claim, check if user has authority
     C     cuky1         chain     pcu01
     C                   if        %found and custat = 'A'
     C*** User authorised to claim
     C                   eval      @@rtcd = '1'
     C                   else
     C*** User NOT authorised to claim
     C                   eval      @@rtcd = '2'
     C                   endif                                                  %equal
     C*** Finally return to caller
     C                   eval      *inlr = *on
     C                   return
