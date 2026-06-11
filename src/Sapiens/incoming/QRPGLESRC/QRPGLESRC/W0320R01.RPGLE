     H* (C) Copyright Progressive Data Solutions, 1998
     H/TITLE W0320R01                                                                       *
     H**************************************************************************************
     H*  Description:  Assigns catastrophe numbers to claim file.  Cat #'s must start at   *
     H*                1 and increase by 1's and are assigned by co#/fnd/fyr/emp#/div.     *
     H*                                                                                    *
     H*  Called from: W0320R                                                               *
     H*  Calls to:                                                                         *
     H*                                                                                    *
     H*  Created: 06/1/98    Programmer C:87    Project:WC591                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FWMCML     UF   E           K DISK
     FWMCMLX    IF   E           K DISK    Rename(PCM01 : PCM02) Prefix(c2_)
     FWAMCL4    IF   E           K DISK

     D kkco#           S                   Like(MCCO#)
     D kkfnd           S                   Like(MCFND)
     D kkfyr           S                   Like(MCFYR)
     D kkemp#          S                   Like(MCEMP#)
     D kkdiv           S                   Like(MCDIV)
     D kkclm#          S                   Like(MCCLM#)
     D kkcat#          S                   Like(CMCATA)
     D kkcase          S                   Like(CMCASE)
     D*
     D found           S              1
     D cat#            S                   Like(CMCATA)

     DPSDS           ESDS                  Extname(SZQ1P)

     C*  Key list for multiple claim file
     C     mcky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkclm#
     C*
     C*  Key list for claim file
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     cmky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkcat#

     C     *entry        plist
     C                   parm                    @@co#             3 0
     C                   parm                    @@fnd             3 0
     C                   parm                    @@fyr             3 0
     C                   parm                    @@emp#            9 0
     C                   parm                    @@div             5 0
     C                   parm                    @@clm#            7 0

     C                   eval      kkco# = @@co#
     C                   eval      kkfnd = @@fnd
     C                   eval      kkfyr = @@fyr
     C                   eval      kkemp# = @@emp#
     C                   eval      kkdiv = @@div
     C                   eval      kkclm# = @@clm#
     C*
     C                   eval      found = '0'

     C*----------------------------------------------------------------------
     C*  Search for pre-existing catastrophe# for co#/fnd/fyr/emp#/div/clm#
     C*----------------------------------------------------------------------
     C     mcky          chain     PMC01                              60
     C                   dow       (*in60 = *off) and (found <> *on)
     C*
     C*    Check claim file to see if cat# is already assigned
     C                   move      MCCASE        kkcase
     C*
     C     cmky          chain     PCM01                              61
     C                   if        (*in61 = *off) and (CMCATA > 0)
     C                   eval       found = *on
     C                   eval       cat# = CMCATA
     C                   endif
     C*
     C                   if        found = *off
     C     mcky          reade     PMC01                                  60
     C                   endif
     C*
     C                   enddo                                                  *in60=*off

     C*----------------------------------------------------------------------
     C*  Assign catastrophe number to claim records
     C*----------------------------------------------------------------------
     C*    If cat# was not found, assign next cat#
     C                   if        found = *off
     C*
     C                   eval       kkcat# = *hival
     C     cmky1         setgt     PCM02
     C                   readp     PCM02                                  60
     C                   if        *in60 = *off
     C                   eval       cat# = c2_CMCATA + 1
     C                   else
     C                   eval       cat# = 1
     C                   endif
     C*
     C                   endif                                                  found=*off
     C*
     C*    Assign cat# to claim file corresponding to multiple claim file
     C     mcky          chain     PMC01                              60
     C                   dow       *in60 = *off
     C*
     C                   move      MCCASE        kkcase
     C*
     C     cmky          chain     PCM01                              61
     C                   if        *in61 = *off
     C                   eval       CMCATA = cat#
     C                   update    PCM01
     C                   endif
     C*
     C     mcky          reade     PMC01                                  60
     C                   enddo                                                  *in60=*off
     C*
     C                   eval       *inlr = *on
     C                   Return
