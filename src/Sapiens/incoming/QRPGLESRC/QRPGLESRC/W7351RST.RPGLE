     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H/TITLE W7353RST - WCSTAT State Specific Requirements
     H*********************************************************************************
     H*  Description:  This program manipulates the WCSTAT extract files to accomodate*
     H*                various state specific requirements.                           *
     H*                                                                               *
     H*  Called from: W7353C                                                          *
     H*  Calls to:                                                                    *
     H*                                                                               *
     H*                                                                               *
     H*                                                                               *
     H*  Created: 10/15/04    Programmer C:15    Project:WC591                        *
     H*********************************************************************************
     H*  Requirements:                                                                *
     H*                                                                               *
     H*     ** NC **    Per NC rating bureau, no other stat codes should be sent on a *
     H*                 policy when stat code '1111' is sent.                         *
     H*********************************************************************************
     H*  Revisions:                                                                   *
     H*                                                                               *
     H*                                                                               *
     H*********************************************************************************
     H*  Indicators:                                                                  *
     H*                                                                               *
     H*                                                                               *
     H*********************************************************************************
     F***WMSSP     IF   E             DISK
     F*
     F* Link, header, name, address file
     FWMSRL     IF   E           K DISK
     F*
     F* Exposure file
     FWDSEL     UF   E           K DISK
     F*
     F* Loss file
     F***WDSLL     UF   E           K DISK
     F*
     F* Unit record file
     FWMSQL     UF   E           K DISK
     F*

     D*================================================================
     D*  Key field definitions
     D kkco#           S                   Like(SRCO#)
     D kkfnd           S                   Like(SRFND)
     D kkrpmy          S                   Like(SRRPMY)
     D kkemp#          S                   Like(SREMP#)
     D kkfyr           S                   Like(SRFYR)
     D kkstcd          S                   Like(SRSTCD)
     D recFound        s              1
     D x               s              3  0

     C*================================================================
     C*---------------
     C*  Key list
     C*---------------
     C*    Keys for extracted data files
     C*
     C     srky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkrpmy
     C*
     C     seky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkrpmy
     C                   kfld                    kkemp#
     C                   kfld                    kkfyr
     C                   kfld                    kkstcd
     C*
     C     sqky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkrpmy
     C                   kfld                    kkemp#
     C                   kfld                    kkfyr
     C                   kfld                    kkstcd

     C     *entry        plist
     C                   parm                    @@co#             3
     C                   parm                    @@fnd             3
     C                   parm                    @@rpmy            5

     C*---------------
     C*
     C                   move      @@co#         kkco#
     C                   move      @@fnd         kkfnd
     C                   move      @@rpmy        kkrpmy
     C*

     C*--------------------
     C*  Main processing
     C*--------------------
     C*
     C     srky          chain     PSR01                              99
     C                   dow       *in99 = *off
     C*
     C                   if        srstcd = 32
     C*
     C                   eval      kkemp# = sremp#
     C                   eval      kkfyr  = srfyr
     C                   eval      kkstcd = srstcd
     C*
     C                   exsr      srNC
     C                   endif
     C*
     C     srky          reade     PSR01                                  99
     C                   enddo

     C*
     C                   eval      *inlr = *on
     C                   return
     C*

     C*==========================================================================
     C*  srNC - subroutine to process NC specific requirements
     C*==========================================================================
     C     srNC          begsr
     C*
     C*----------
     C*  Identify any policies with a '1111' exposure transaction and remove all
     C*  other exposure transactions for that policy.
     C*
     C     seky          chain     PSE01                              99
     C                   dow       *in99 = *off
     C*
     C                   if        seclas = 1111
     C*
     C     seky          chain     PSE01                              98
     C                   dow       *in98 = *off
     C*
     C                   if        seclas <> 1111
     C*
     C*  Update the Unit Total record.
     C*
     C     sqky          chain     PSQ01                              99
     C                   if        *in99 = *off
     C                   eval      sqeoth = sqeoth - seexpa
     C************       eval      sqsubp = sqsubp - seprma
     C                   eval      sq#rcd = sq#rcd - 1
     C                   update    PSQ01
     C                   endif
     C*
     C                   delete    PSE01
     C*
     C                   endif
     C*
     C     seky          reade     PSE01                                  98
     C                   enddo
     C*
     C                   seton                                            99
     C                   endif
     C*
     C  n99seky          reade     PSE01                                  99
     C                   enddo
     C*----------
     C*
     C                   endsr                                                  srNC
     C*
